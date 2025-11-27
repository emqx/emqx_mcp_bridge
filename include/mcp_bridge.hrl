-define(PLUGIN_NAME, "mcp_bridge").
-define(PLUGIN_VSN, "1.0.0").
-define(MCP_VERSION, <<"2024-11-05">>).
-define(MCP_BRIDGE_INFO, #{
    <<"name">> => <<"emqx_mcp_bridge">>,
    <<"version">> => <<"1.0.0">>,
    <<"title">> => <<"EMQX MCP Bridge">>
}).
-define(MCP_MSG_HEADER, emqx_mcp_bridge).
-define(MCP_CLIENTID_S, "emqx_mcp_bridge").
-define(MCP_CLIENTID_B, <<"emqx_mcp_bridge">>).
-define(TARGET_CLIENTID_KEY_S, "target-mqtt-client-id").
-define(TARGET_CLIENTID_KEY, <<?TARGET_CLIENTID_KEY_S>>).
