-define(PLUGIN_NAME, "mcp_bridge").
-define(PLUGIN_VSN, ?plugin_rel_vsn).
-define(MCP_VERSION, <<"2024-11-05">>).
-define(MCP_BRIDGE_INFO, #{
    <<"name">> => <<"emqx_mcp_bridge">>,
    <<"version">> => <<?plugin_rel_vsn>>,
    <<"title">> => <<"EMQX MCP Bridge">>
}).
-define(MCP_MSG_HEADER, emqx_mcp_bridge).
-define(MCP_CLIENTID_S, "emqx_mcp_bridge").
-define(MCP_CLIENTID_B, <<"emqx_mcp_bridge">>).
-define(TARGET_CLIENTID_KEY_S, "target-mqtt-client-id").
-define(TARGET_CLIENTID_KEY, <<?TARGET_CLIENTID_KEY_S>>).
-define(TOOL_TYPES_KEY_S, "tool-types").
-define(TOOL_TYPES_KEY, <<?TOOL_TYPES_KEY_S>>).
-define(INIT_REQ_ID, <<"init_1">>).
-define(LIST_TOOLS_REQ_ID, <<"list_tools_1">>).

-type request_params() :: map().
-type request_meta() :: map().
-type response() :: map() | [map()].
