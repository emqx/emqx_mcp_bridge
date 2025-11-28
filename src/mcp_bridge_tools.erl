-module(mcp_bridge_tools).

-include("mcp_bridge.hrl").
-include_lib("emqx_plugin_helper/include/emqx.hrl").

-export([
    create_table/0,
    save_tools/2,
    get_tools/1,
    list_tools/0
]).

-record(emqx_mcp_tool_registry, {
    tool_type,
    tools = []
}).

-define(TAB, emqx_mcp_tool_registry).

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

save_tools(ToolType, ToolsList) ->
    TaggedTools = [
        ToolSchema#{
            <<"name">> => <<ToolType/binary, ":", Name/binary>>,
            <<"inputSchema">> => maybe_add_target_client_param(InputSchema)
        } || #{<<"name">> := Name, <<"inputSchema">> := InputSchema} = ToolSchema <- ToolsList
    ],
    mria:dirty_write(?TAB,
        #emqx_mcp_tool_registry{
            tool_type = ToolType,
            tools = TaggedTools
        }
    ).

maybe_add_target_client_param(#{<<"properties">> := Properties} = InputSchema) ->
    case mcp_bridge:get_config() of
        #{get_target_clientid_from := <<"tool_params">>} ->
            InputSchema#{
                <<"properties">> => Properties#{
                    ?TARGET_CLIENTID_KEY => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The target MQTT client ID to send the request to.">>
                    }
                }
            };
        _ ->
            InputSchema
    end.

get_tools(ToolType) ->
    case mnesia:dirty_read(?TAB, ToolType) of
        [#emqx_mcp_tool_registry{tools = TaggedTools}] ->
            {ok, #{tools => TaggedTools}};
        [] ->
            {error, not_found}
    end.

list_tools() ->
    [TaggedTools || #emqx_mcp_tool_registry{tools = TaggedTools} <- ets:tab2list(?TAB)].

% trans(Fun) ->
%     case mria:transaction(?COMMON_SHARD, Fun) of
%         {atomic, Res} -> {ok, Res};
%         {aborted, Reason} -> {error, Reason}
%     end.
