-module(mcp_bridge_tool_registry).

-include("mcp_bridge.hrl").
-include_lib("emqx_plugin_helper/include/logger.hrl").
-include_lib("emqx_plugin_helper/include/emqx.hrl").

-behaviour(gen_server).

-export([start_link/0, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    create_table/0,
    delete_table/0,
    save_mcp_over_mqtt_tools/2,
    save_custom_tools/4,
    save_tools/5,
    get_tools/1,
    list_tools/0,
    list_tools/1
]).

-record(emqx_mcp_tool_registry, {
    tool_type,
    tools = [],
    protocol :: mcp_over_mqtt | custom,
    module,
    tool_opts = #{}
}).

-define(TAB, emqx_mcp_tool_registry).

%%==============================================================================
%% API
%%==============================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    gen_server:stop(?MODULE).

create_table() ->
    Copies = ram_copies,
    StoreProps = [
        {ets, [
            compressed,
            {read_concurrency, true},
            {write_concurrency, true}
        ]},
        {dets, [{auto_save, 1000}]}
    ],
    ok = mria:create_table(?TAB, [
        {type, set},
        {rlog_shard, ?COMMON_SHARD},
        {storage, Copies},
        {record_name, emqx_mcp_tool_registry},
        {attributes, record_info(fields, emqx_mcp_tool_registry)},
        {storage_properties, StoreProps}
    ]),
    ok = mria_rlog:wait_for_shards([?COMMON_SHARD], infinity),
    ok = mria:wait_for_tables([?TAB]),
    case mnesia:table_info(?TAB, storage_type) of
        Copies ->
            ok;
        _Other ->
            {atomic, ok} = mnesia:change_table_copy_type(?TAB, node(), Copies),
            ok
    end.

delete_table() ->
    mnesia:delete_table(?TAB).

save_mcp_over_mqtt_tools(ToolType, Tools) ->
    save_tools(mcp_over_mqtt, undefined, ToolType, Tools, #{}).

save_custom_tools(Module, ToolType, Tools, ToolOpts) ->
    save_tools(custom, Module, ToolType, Tools, ToolOpts).

save_tools(Protocol, Module, ToolType, Tools, ToolOpts) ->
    TaggedTools = [
        ToolSchema#{
            <<"name">> => <<ToolType/binary, ":", Name/binary>>
        }
     || #{<<"name">> := Name} = ToolSchema <- Tools
    ],
    mria:dirty_write(
        ?TAB,
        #emqx_mcp_tool_registry{
            protocol = Protocol,
            module = Module,
            tool_type = ToolType,
            tools = TaggedTools,
            tool_opts = ToolOpts
        }
    ).

get_tools(ToolType) ->
    case mnesia:dirty_read(?TAB, ToolType) of
        [#emqx_mcp_tool_registry{tools = Tools, protocol = Proto, module = Mod, tool_opts = ToolOpts}] ->
            Tools1 = inject_target_client_params(Proto, Tools),
            {ok, #{tools => Tools1, protocol => Proto, module => Mod, tool_opts => ToolOpts}};
        [] ->
            {error, not_found}
    end.

list_tools() ->
    lists:flatten([
        inject_target_client_params(Proto, Tools)
     || #emqx_mcp_tool_registry{tools = Tools, protocol = Proto} <- ets:tab2list(?TAB)
    ]).

list_tools(ToolTypes) ->
    lists:flatmap(
        fun(ToolType) ->
            case get_tools(ToolType) of
                {ok, #{tools := Tools}} ->
                    Tools;
                {error, not_found} ->
                    []
            end
        end,
        ToolTypes
    ).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================
init([]) ->
    erlang:process_flag(trap_exit, true),
    ok = create_table(),
    load_custom_tools(),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    ?SLOG(warning, #{msg => unexpected_call, tag => ?MODULE, request => _Request}),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    ?SLOG(warning, #{msg => unexpected_cast, tag => ?MODULE, cast => _Msg}),
    {noreply, State}.

handle_info(_Info, State) ->
    ?SLOG(warning, #{msg => unexpected_info, tag => ?MODULE, info => _Info}),
    {noreply, State}.

terminate(_Reason, _State) ->
    delete_table(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================
load_custom_tools() ->
    lists:foreach(fun({Mod, _}) ->
        case atom_to_list(Mod) of
            "mcp_bridge_tools_" ++ _ ->
                case get_custom_tools(Mod) of
                    no_tools ->
                        ok;
                    {ToolType, Tools, ToolOpts} ->
                        save_custom_tools(Mod, ToolType, Tools, ToolOpts)
                end;
            _ ->
                ok
        end
    end, code:all_loaded()).

get_custom_tools(Module) ->
    Attrs = Module:module_info(attributes),
    case lists:keyfind(tool_type, 1, Attrs) of
        {tool_type, [ToolType]} ->
            Tools = [format_tool(Tool, Module) || {tool, [Tool]} <- Attrs],
            ToolOpts = get_tool_opts(Attrs),
            {bin(ToolType), Tools, ToolOpts};
        _ ->
            case lists:keyfind(tool, 1, Attrs) of
                {tool, _} ->
                    throw({missing_tool_type_definition, Module});
                _ ->
                    no_tools
            end
    end.

format_tool(#{name := Name, title := _, description := _, inputSchema := _} = Tool0, Module) ->
    %% remove opts from tool definition
    Tool = maps:without([opts], Tool0),
    validate_tool_name(Name, Module),
    %% 1. ensure the tool definition can be encoded to JSON
    %% 2. convert keys to binaries
    try emqx_utils_json:decode(emqx_utils_json:encode(Tool))
    catch
        _:Reason ->
            throw({invalid_tool_definition, Module, Name, Reason})
    end;
format_tool(Other, Module) ->
    throw({missing_fields_in_tool_definition, Module, Other}).

get_tool_opts(Attrs) ->
    lists:foldl(fun
        ({tool, [#{name := Name, opts := Opts}]}, Acc) ->
            Acc#{Name => Opts};
        (_, Acc) ->
            Acc
    end, #{}, Attrs).

validate_tool_name(Name, Module) when is_atom(Name) ->
    case erlang:function_exported(Module, Name, 2) of
        true -> ok;
        false -> throw({invalid_tool_definition, Module, Name})
    end;
validate_tool_name(Name, Module) ->
    throw({tool_name_must_be_atom, Module, Name}).

inject_target_client_params(mcp_over_mqtt, Tools) ->
    [
        ToolSchema#{
            <<"inputSchema">> => maybe_add_target_client_param(InputSchema)
        }
     || #{<<"inputSchema">> := InputSchema} = ToolSchema <- Tools
    ];
inject_target_client_params(_, Tools) ->
    Tools.

maybe_add_target_client_param(#{<<"properties">> := Properties} = InputSchema) ->
    case mcp_bridge:get_config() of
        #{get_target_clientid_from := <<"tool_params">>} ->
            Required = maps:get(<<"required">>, InputSchema, []),
            InputSchema#{
                <<"properties">> => Properties#{
                    ?TARGET_CLIENTID_KEY => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The target MQTT client ID to send the request to.">>
                    }
                },
                <<"required">> => [?TARGET_CLIENTID_KEY | Required]
            };
        _ ->
            InputSchema
    end.

bin(Bin) when is_binary(Bin) -> Bin;
bin(String) when is_list(String) -> 
    list_to_binary(String);
bin(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom)).

% trans(Fun) ->
%     case mria:transaction(?COMMON_SHARD, Fun) of
%         {atomic, Res} -> {ok, Res};
%         {aborted, Reason} -> {error, Reason}
%     end.
