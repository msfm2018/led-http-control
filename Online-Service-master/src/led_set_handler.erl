-module(led_set_handler).
-export([init/2]).

init(Req=#{method := <<"GET">>}, State) ->
    Qs = cowboy_req:parse_qs(Req),
    A = proplists:get_value(<<"a">>, Qs, <<"0">>),
    Value = case A of <<"1">> -> 1; _ -> 0 end,
    ets:insert(sensor_latest, {led, Value}),
    Response = integer_to_binary(Value),
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, POST, OPTIONS">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req3),
 
    {ok, Req1, State};

init(Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, POST, OPTIONS">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req3),


    {ok, Req, State}.
