-module(mcp_bridge_http_auth).

-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, State) ->
    Headers = cowboy_req:headers(Req),
    JwtToken = get_jwt_token(Headers),
    case validate_jwt(JwtToken) of
        {ok, JwtClaims} ->
            {ok, Req, State#{jwt_claims => JwtClaims}};
        {error, invalid_token} ->
            Req2 = cowboy_req:reply(401, #{}, <<"Unauthorized">>, Req),
            {stop, Req2, State}
    end.

get_jwt_token(Headers) ->
    case string:split(maps:get(<<"authorization">>, Headers, <<>>), " ") of
        [<<"Bearer">>, Token] -> Token;
        _ -> undefined
    end.

validate_jwt(undefined) ->
    {ok, #{}};  %% Allow requests without JWT for now
validate_jwt(JwtToken) ->
    #{jwt_secret := Secret} = mcp_bridge:get_config(),
    JWK = jose_jwk:from_oct(Secret),
    case jose_jws:verify(JWK, JwtToken) of
        {true, Payload, _} ->
            {ok, emqx_utils_json:decode(Payload)};
        {false, _, _} ->
            {error, invalid_token}
    end.