%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 NetScale, SL.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(nkimage_netscale_pillow).
-export([parse_processor/2, processor_syntax/0, process/3]).
-include("../../include/nkimage.hrl").

parse_processor(Data, ParseOpts) ->
    case nklib_syntax:parse(Data, #{class=> atom}) of
        {ok, #{class:=pillow}, _} ->
            case nklib_syntax:parse(Data, processor_syntax(), ParseOpts) of
                  {ok, Processor, UnknownFields} ->
                      {ok, Processor, UnknownFields};
                  {error, Error} ->
                      {error, Error}
              end;
          _ ->
              continue
      end.

processor_syntax() ->
    Base = nkimage_util:processor_syntax(),
    Base#{
        config := #{
            host => binary,
            port => integer,
            path => binary,
            scheme => atom,
            user => binary,
            password => binary,
            '__mandatory' => [host, port, path, scheme]
        }
    }.


process(_, Processor, Req) -> 
    
    Auth= auth(Processor),
    Url = url(Processor),
    Action = action(Req),
    Mime = mime(Req),
    Body = body(Req),
    Params = params(Req),

    Continue = case Action of
        <<"resize">> ->
            if
                erlang:size(Body) > ?MIN_IMAGE_SIZE ->
                    ok;
                true ->
                    case handle(request(Url, <<"info">>, Params, Body, Mime, Auth)) of
                        {ok, RawJson} ->
                            Meta = nklib_json:decode(RawJson),
                            case needs_thumbnail(Meta, Req) of
                                false ->
                                    {error, no_thumbnail_needed};
                                true ->
                                    ok
                            end;
                        _ ->
                            ok
                    end
            end;
        _ ->
            ok
    end,
    case Continue of
        ok ->
            case handle(request(Url, Action, Params, Body, Mime, Auth)) of
                {ok, Thumb} ->
                    case Action of
                        <<"resize">> ->
                            BodySize = erlang:size(Body),
                            ThumbSize = erlang:size(Thumb),
                            if
                                BodySize < ThumbSize ->
                                    lager:warning("~p thumbnail_bigger, Original size: ~p, Thumbnail size: ~p", [?MODULE, BodySize, ThumbSize]),
                                    {error, thumbnail_bigger};
                                true ->
                                    {ok, Thumb}
                            end;
                        _ ->
                            {ok, Thumb}
                    end;
                Other ->
                    Other
            end;
        Other ->
            Other
    end.


action(#{ action := resize }) -> <<"resize">>;
action(#{ action := convert }) -> <<"convert">>;
action(#{ action := info }) -> <<"info">>.

mime(#{ from := CT}) -> CT.
body(#{ body := Body}) -> Body.

params(#{ width := Width, 
          height := Height,
          to := <<"image/", Fmt/binary>>,
          options := Options }) -> 
    
    [{<<"width">>, nklib_util:to_binary(Width)},
     {<<"height">>, nklib_util:to_binary(Height)},
     {<<"type">>, Fmt}] ++ parse_opts(Options);

params(#{ width := Width, 
          height := Height,
          to := <<"application/", Fmt/binary>>,
          options := Options }) -> 
    
    [{<<"width">>, nklib_util:to_binary(Width)},
     {<<"height">>, nklib_util:to_binary(Height)},
     {<<"type">>, Fmt}] ++ parse_opts(Options);

params(#{ to := <<"image/", Fmt/binary>>, 
          options := Options }) ->
    
    [{<<"type">>, Fmt}] ++ parse_opts(Options);

params(#{ to := <<"application/", Fmt/binary>>,
          options := Options }) ->
    
    [{<<"type">>, Fmt}] ++ parse_opts(Options);

params(#{ options := Options }) ->
    parse_opts(Options).

parse_opts(Options) ->
    maps:fold( fun(K, V, Opts) ->
                [{ nklib_util:to_binary(K), nklib_util:to_binary(V)}|Opts]
               end, [], Options).

auth(#{ config:=#{ user := User,
                   password := Password }}) ->
    base64:encode_to_string(
                 binary_to_list(<<User/binary, 
                                  <<":">>/binary, 
                                  Password/binary>>));
auth(_) -> 
    none.

url(#{ config:=#{ host := Host,
                             path := Path,
                             port := Port,
                             scheme := Scheme}}) ->
    nklib_util:to_binary(string:join([nklib_util:to_list(Scheme),
                 "://",
                 nklib_util:to_list(Host),
                 ":",
                 nklib_util:to_list(Port),
                 nklib_util:to_list(Path)], "")).


request(BaseUrl, Op, Params, Body, Mime, AuthToken) ->
    Headers = case AuthToken of
                  none -> [];
                  _ ->
                      [{"Authorization", "Basic " ++ AuthToken}]
              end,
    
    Url = nklib_util:to_list(endpoint(BaseUrl, Op, Params)),
    ContentType = nklib_util:to_list(Mime),
    lager:debug("~p: request: url=~p, headers=~p, content_type=~p", 
                [?MODULE, Url, Headers, ContentType]),
    httpc:request(post,{ Url, Headers, ContentType, Body},[],[]).

handle({ok, {{_, 200, _}, _, Body}}) when is_binary(Body)->
    lager:debug("~p pillow response: Status=200", [?MODULE]),
    {ok, Body};

handle({ok, {{_, 200, _}, _, Body}}) when is_list(Body)->
    lager:debug("~p pillow response: Status=200", [?MODULE]),
    {ok, nklib_util:to_binary(Body)};

handle({ok, {{_, StatusCode, _}, _, Body}}) ->
    lager:debug("~p pillow response: Status=~p, Body=~p", [?MODULE, StatusCode, Body]),
    {error, Body};

handle(E) ->
    lager:debug("~p pillow error: ~p", [?MODULE, E]),
    E.


%%====================================================================
%% Internal -- Generic HTTP related utility helper functions
%%====================================================================
endpoint(Url, Op, []) ->
    <<Url/binary, <<"/">>/binary, Op/binary>>;

endpoint(Url, Op, [_|_]=Params) ->
    Qs = build_qs(Params),
    <<Url/binary, <<"/">>/binary, Op/binary, <<"?">>/binary, Qs/binary>>.

build_qs(Qs) ->
    build_qs(Qs, first, []).

build_qs([], _, Qs) ->
    nklib_util:bjoin(lists:reverse(Qs), <<>>); 

build_qs([{K,V}|Remaining], first, Qs) ->
    build_qs(Remaining, others, [<<K/binary, <<"=">>/binary, V/binary>>|Qs]);

build_qs([{K,V}|Remaining], others, Qs) ->
    build_qs(Remaining, others, [<< <<"&">>/binary, K/binary, <<"=">>/binary, V/binary>>|Qs]).


%%====================================================================
%% Internal
%%====================================================================

needs_thumbnail(#{width:=OW, height:=OH}, R) ->
    needs_thumbnail(#{<<"width">> => OW, <<"height">> => OH}, R); 

needs_thumbnail(O, #{width:=RW, height:=RH}) ->
    needs_thumbnail(O, #{<<"width">> => RW, <<"height">> => RH}); 

needs_thumbnail(#{<<"width">>:=OW, <<"height">>:=OH}, #{<<"width">> := RW, <<"height">> := RH}) when OW > RW orelse OH > RH ->
    true;

needs_thumbnail(_O, _R) ->
    false.
