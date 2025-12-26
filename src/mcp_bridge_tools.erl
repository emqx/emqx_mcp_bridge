-module(mcp_bridge_tools).

-export([
    pack_tool_name/3,
    unpack_tool_name/1
]).

-define(DLMT, "|--|").

pack_tool_name(ToolType, ServerVsn, ToolName) ->
    <<ToolType/binary, ?DLMT, ServerVsn/binary, ?DLMT, ToolName/binary>>.

unpack_tool_name(Name) ->
    case string:split(Name, ?DLMT) of
        [ToolType, Rem] ->
            case string:split(Rem, ?DLMT) of
                [ServerVsn, ToolName] ->
                    {ToolType, ServerVsn, ToolName};
                _ ->
                    throw({invalid_tool_name, Name})
            end;
        _ ->
            throw({invalid_tool_name, Name})
    end.
