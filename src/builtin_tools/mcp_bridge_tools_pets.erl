-module(mcp_bridge_tools_pets).

-include("mcp_bridge.hrl").

-export([
    turn_direction/3,
    wag_tail/3,
    nod/3
]).

-mcp_tool_type(<<"布谷元创/电子宠物"/utf8>>).

-mcp_tool_vsn(<<"1.0.0">>).

-mcp_tool(#{
    name => turn_direction,
    title => <<"转动身体方向"/utf8>>,
    description => <<""
        "控制电子宠物往指定方向转动一定的角度。 "
        "输入参数包括方向（左/右）和角度（0-360度）。"
        ""/utf8>>,
    inputSchema => #{
        type => object,
        properties => #{
            ?TARGET_CLIENTID_KEY => #{
                type => string,
                description => <<"目标电子宠物的MQTT客户端ID"/utf8>>
            },
            direction => #{
                type => string,
                enum => [<<"left"/utf8>>, <<"right"/utf8>>],
                description => <<"转动方向"/utf8>>
            },
            angle => #{
                type => integer,
                minimum => 0,
                maximum => 360,
                description => <<"转动角度，取值范围为0-360度"/utf8>>
            }
        }
    },
    opts => #{}
}).

-mcp_tool(#{
    name => wag_tail,
    title => <<"摇尾巴"/utf8>>,
    description => <<""
        "控制电子宠物摇动尾巴。 "
        "输入参数包括摇动次数和每次摇动的幅度（小/中/大）。"
        ""/utf8>>,
    inputSchema => #{
        type => object,
        properties => #{
            ?TARGET_CLIENTID_KEY => #{
                type => string,
                description => <<"目标电子宠物的MQTT客户端ID"/utf8>>
            },
            times => #{
                type => integer,
                minimum => 1,
                maximum => 20,
                description => <<"摇动次数"/utf8>>
            },
            amplitude => #{
                type => string,
                enum => [<<"small"/utf8>>, <<"medium"/utf8>>, <<"large"/utf8>>],
                description => <<"摇动幅度"/utf8>>
            }
        }
    },
    opts => #{}
}).

-mcp_tool(#{
    name => nod,
    title => <<"点头"/utf8>>,
    description => <<""
        "控制电子宠物点头。 "
        "输入参数包括点头次数和每次点头的幅度（小/中/大）。"
        ""/utf8>>,
    inputSchema => #{
        type => object,
        properties => #{
            ?TARGET_CLIENTID_KEY => #{
                type => string,
                description => <<"目标电子宠物的MQTT客户端ID"/utf8>>
            },
            times => #{
                type => integer,
                minimum => 1,
                maximum => 10,
                description => <<"点头次数"/utf8>>
            },
            amplitude => #{
                type => string,
                enum => [<<"small"/utf8>>, <<"medium"/utf8>>, <<"large"/utf8>>],
                description => <<"点头幅度"/utf8>>
            }
        }
    },
    opts => #{}
}).

turn_direction(ReqId, #{<<"direction">> := Dir, <<"angle">> := Angle, ?TARGET_CLIENTID_KEY := TargetClientId}, _Meta) ->
    Dir1 = case Dir of
        <<"left">> -> 1;
        <<"right">> -> -1
    end,
    send_control_motors_request(TargetClientId, 1, ReqId, Angle, Dir1).

wag_tail(ReqId, #{<<"times">> := Times, <<"amplitude">> := Amplitude, ?TARGET_CLIENTID_KEY := TargetClientId}, _Meta) ->
    AmpValue = case Amplitude of
        <<"small">> -> 30;
        <<"medium">> -> 60;
        <<"large">> -> 90
    end,
    shake_motor(TargetClientId, 2, ReqId, AmpValue, Times).

nod(ReqId, #{<<"times">> := Times, <<"amplitude">> := Amplitude, ?TARGET_CLIENTID_KEY := TargetClientId}, _Meta) ->
    AmpValue = case Amplitude of
        <<"small">> -> 15;
        <<"medium">> -> 30;
        <<"large">> -> 45
    end,
    shake_motor(TargetClientId, 0, ReqId, AmpValue, Times).

%%==============================================================================
%% Helpers
%%==============================================================================
shake_motor(TargetClientId, MotorIndex, ReqId, Angle, Times) ->
    lists:foldl(fun
        (I, {ok, _}) ->
            I == 1 orelse timer:sleep(200),
            send_control_motors_request(TargetClientId, MotorIndex, ReqId, Angle, 1),
            timer:sleep(200),
            send_control_motors_request(TargetClientId, MotorIndex, ReqId, Angle, -1);
        (_, Err) -> Err
    end, {ok, no_need_to_wag_tail}, lists:seq(1, Times)).

send_control_motors_request(TargetClientId, MotorIndex, ReqId, Angle, Dir) ->
    ToolName = <<"control_motors">>,
    Request = #{
        id => ReqId,
        method => <<"tools/call">>,
        params => #{
            <<"name">> => ToolName,
            <<"arguments">> => #{
                %% Motor index:
                %% - head: 0
                %% - body: 1
                %% - tail: 2
                <<"motorIndex">> => MotorIndex,
                <<"angle">> => Angle * Dir
            }
        }
    },
    mcp_bridge_message:send_mom_tools_call(
        TargetClientId,
        <<"SpacemiT/RSIC5/DemoPlatform">>,
        ToolName,
        Request
    ).
