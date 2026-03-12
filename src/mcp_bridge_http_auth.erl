-module(mcp_bridge_http_auth).

-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    Headers = cowboy_req:headers(Req),
    JwtToken = get_jwt_token(Headers),
    #{jwt_secret := Secret, enable_auth := EnableAuth} = mcp_bridge:get_config(),
    case maybe_validate_jwt(JwtToken, Secret, EnableAuth) of
        {ok, JwtClaims} ->
            {ok, Req#{jwt_claims => JwtClaims}, Env};
        {error, invalid_token} ->
            Req2 = cowboy_req:reply(401, #{}, <<"Unauthorized">>, Req),
            {stop, Req2}
    end.

get_jwt_token(Headers) ->
    case string:split(maps:get(<<"authorization">>, Headers, <<>>), " ") of
        [<<"Bearer">>, Token] -> Token;
        _ -> undefined
    end.

maybe_validate_jwt(undefined, _Secret, false) ->
    %% Allow requests without JWT when authentication is disabled
    {ok, #{}};
maybe_validate_jwt(undefined, _Secret, true) ->
    %% Require JWT when authentication is enabled
    {error, invalid_token};
maybe_validate_jwt(JwtToken, _Secret, false) ->
    %% When authentication is disabled, we still decode the JWT to extract claims for
    %% getting tool types if the token is present, but we don't verify it
    try
        {_, #{<<"payload">> := Payload}} = jose_jws:expand(JwtToken),
        Claims = emqx_utils_json:decode(Payload),
        {ok, Claims}
    catch
        _:_ -> {ok, #{}}
    end;
maybe_validate_jwt(JwtToken, Secret, true) ->
    try
        {true, Payload, _} = jose_jws:verify(jose_jwk:from_oct(Secret), JwtToken),
        Claims = emqx_utils_json:decode(Payload),
        {ok, Claims}
    catch
        _:_ -> {error, invalid_token}
    end.
