-module(led_status_handler).
-export([init/2]).

init(Req=#{method := <<"GET">>}, State) ->
    Value = case ets:lookup(sensor_latest, led) of
        [{led, V}] -> V;
        [] -> 0
    end,
    %  Response = integer_to_binary(Value),
     Json = io_lib:format("{\"led\":\"~s\"}", [integer_to_list(Value)]),
     Response = list_to_binary(Json),
    io:format("request~n"),
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, POST, OPTIONS">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
    % cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, Response, Req3),

 %% 设置 content-type 为 application/json
cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req3),
    {ok, Req1, State};

init(Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, POST, OPTIONS">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req3),
    {ok, Req, State}.



    % curl "http://192.168.3.69:8999/led/set?a=1"
% curl "http://192.168.3.69:8999/led/set?a=0"
% curl "http://192.168.3.69:8999/led/status"
