-module(mcp_bridge_tools).

-export([
    create_table/0,
    save_tools/3,
    get_tools/1
]).

-record(emqx_mcp_tool_registry, {
    tool_type,
    mqtt_client_id,
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
        {rlog_shard, ?MODULE},
        {storage, Copies},
        {record_name, emqx_mcp_tool_registry},
        {attributes, record_info(fields, emqx_mcp_tool_registry)},
        {storage_properties, StoreProps}
    ]),
    ok = mria_rlog:wait_for_shards([?MODULE], infinity),
    ok = mria:wait_for_tables([?TAB]),
    case mnesia:table_info(?TAB, storage_type) of
        Copies ->
            ok;
        _Other ->
            {atomic, ok} = mnesia:change_table_copy_type(?TAB, node(), Copies),
            ok
    end.

save_tools(MqttClientId, ToolType, ToolsList) ->
    TaggedTools = [
        ToolSchema#{<<"name">> => <<ToolType/binary, ":", Name/binary>>}
        || #{<<"name">> := Name} = ToolSchema <- ToolsList
    ],
    mria:dirty_write(?TAB,
        #emqx_mcp_tool_registry{
            tool_type = ToolType,
            mqtt_client_id = MqttClientId,
            tools = TaggedTools
        }
    ).

get_tools(ToolType) ->
    case mnesia:dirty_read(?TAB, ToolType) of
        [#emqx_mcp_tool_registry{tools = Tools}] ->
            Tools;
        [] ->
            []
    end.
