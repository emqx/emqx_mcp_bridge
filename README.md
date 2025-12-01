# MCP Bridge Plugin

An EMQX plugin that bridges Streamable HTTP or MCP-SSE to MCP-over-MQTT.

## Overview

This plugin allows HTTP MCP clients to communicate with MCP-over-MQTT clients by bridging the two protocols.

## Configuration

The listening_address parameter in the configuration file specifies the address and path the plugin listens on. The plugin supports both the old SSE MCP clients and the new streamable MCP clients. To only support the old SSE MCP clients, change the path to `/sse`.

```hocon
listening_address = "http://0.0.0.0:9998/mcp"
```

## How does it work

### Load tools from MCP Servers

When an MCP Server using the MCP over MQTT protocol connects to EMQX, the plugin loads tools from the MCP Server in two ways:

- If the MCP Server reports a "notifications/server/online" event with a "tools" field in its "meta" data, the plugin directly uses the tool list from this field.

- If the "tools" field is not present in the "notifications/server/online" event, the plugin sends a "tools/list" request to the MCP Server to retrieve the tool list.

### Store tools by tool type

Tools are stored using their tool type as the primary key, where the tool type is the ServerName of the MCP over MQTT server. If multiple MCP Servers report the same ServerName, the plugin only keeps the tool list from the most recent MCP Server.

After loading tools from the MCP Server, the plugin transforms the tool list as follows:

- It adds the tool type prefix to each tool name, resulting in the format "ToolType:ToolName". This avoids tool name conflicts across different MCP Servers and allows MCP-HTTP clients to filter tools by type.

- If `get_target_clientid_from` is set to `tool_params`, MCP bridge injects a parameter named `target-mqtt-client-id` into each tool. MCP-HTTP clients must provide this parameter when calling the tool, and the plugin uses its value to send the tool invocation request to the specified MQTT MCP Server.

- If `get_target_clientid_from` is set to `http_headers` or `jwt_claims`, MCP-HTTP clients do not need to provide the `target-mqtt-client-id` parameter. Instead, the plugin obtains the target MQTT Client ID from the HTTP headers or JWT claims. This method is suitable only when there is a one-to-one mapping between MCP-HTTP clients and MQTT MCP Servers, i.e., each MCP-HTTP client accesses tools on a single MQTT MCP Server.

### List only specific tool types

When an MCP-HTTP client requests the tool list, it can specify which tool types to include in the response. The plugin retrieves the desired tool types from the HTTP headers or JWT claims based on the `get_tool_types_from` configuration. If no tool types are specified, the plugin returns tools from all available tool types.

## Deployment

See [EMQX documentation](https://docs.emqx.com/en/enterprise/v5.0/extensions/plugins.html) for details on how to deploy custom plugins.
