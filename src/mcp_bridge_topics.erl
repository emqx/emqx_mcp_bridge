-module(mcp_bridge_topics).

-export([
    get_topic/2
]).

get_topic(server_control, #{server_id := ServerId, server_name := ServerName}) ->
    <<"$mcp-server/", ServerId/binary, "/", ServerName/binary>>;
get_topic(server_capability_list_changed, #{server_id := ServerId, server_name := ServerName}) ->
    <<"$mcp-server/capability/", ServerId/binary, "/", ServerName/binary>>;
get_topic(server_resources_updated, #{server_id := ServerId, server_name := ServerName}) ->
    <<"$mcp-server/capability/", ServerId/binary, "/", ServerName/binary>>;
get_topic(server_presence, #{server_id := ServerId, server_name := ServerName}) ->
    <<"$mcp-server/presence/", ServerId/binary, "/", ServerName/binary>>;
get_topic(client_presence, #{mcp_clientid := McpClientId}) ->
    <<"$mcp-client/presence/", McpClientId/binary>>;
get_topic(client_capability_list_changed, #{mcp_clientid := McpClientId}) ->
    <<"$mcp-client/capability/", McpClientId/binary>>;
get_topic(rpc, #{mcp_clientid := McpClientId, server_id := ServerId, server_name := ServerName}) ->
    <<"$mcp-rpc/", McpClientId/binary, "/", ServerId/binary, "/", ServerName/binary>>.
