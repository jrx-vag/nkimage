%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Carlos Gonzalez Florido.  All Rights Reserved.
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
-module(nkimage_imaginary).
-export([parse_processor/2, processor_syntax/0, resize/4]).
-include("../../include/nkimage.hrl").

parse_processor(Data, ParseOpts) ->
    case nklib_syntax:parse(Data, #{class=> atom}) of
        {ok, #{class:=imaginary}, _} ->
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
            server => binary,
            user => binary,
            password => binary,
            '__mandatory' => [server]
        }
    }.


-spec resize(nkservice:id(), nkimage:processor(), nkimage:size(), nkfile:file()) ->
    {ok, binary()} | {error, term()}.

resize(_SrvId, #{ config:=#{ server := Server, user := User, password := Password }}, 
       #{width:=Width, height:=Height},  #{meta := #{ file_path := FilePath, content_type := Mime}}) ->
    AuthToken = base64:encode_to_string( 
                 binary_to_list(<<User/binary, <<":">>/binary, Password/binary>>)),
    handle(request(Server, <<"resize">>,
            [{<<"width">>, int_to_binary(Width)},
              {<<"height">>, int_to_binary(Height)}], FilePath, Mime, AuthToken));

resize(_SrvId, #{ config:=#{ server := Server }}, 
       #{width:=Width, height:=Height}, #{meta := #{ file_path := FilePath, content_type := Mime}}) ->
    handle(request(Server, <<"resize">>,  
          [{<<"width">>, int_to_binary(Width)},
           {<<"height">>, int_to_binary(Height)}], FilePath, Mime, undefined)).

%%====================================================================
%% Internal functions
%%====================================================================
request(BaseUrl, Op, Params, FilePath, Mime, AuthToken) ->
    Headers = case AuthToken of
                  undefinde -> [];
                  _ ->
                      [{"Authorization", "Basic " ++ AuthToken}]
              end,
    
    {ok, F} = file:read_file(FilePath),
    Url = endpoint(BaseUrl, Op, Params),
    ?DEBUG("request url: ~p", [Url]),
    httpc:request(post,{erlang:binary_to_list(Url), Headers, binary_to_list(Mime), F},[],[]).

handle({ok, {{_, 200, _}, _, Body}}) ->
    {ok, Body};

handle({ok, {{_, _, _}, _, Body}}) ->
    {error, Body};

handle(E) ->
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
    binary_join(lists:reverse(Qs), <<>>); 

build_qs([{K,V}|Remaining], first, Qs) ->
    build_qs(Remaining, others, [<<K/binary, <<"=">>/binary, V/binary>>|Qs]);

build_qs([{K,V}|Remaining], others, Qs) ->
    build_qs(Remaining, others, [<< <<"&">>/binary, K/binary, <<"=">>/binary, V/binary>>|Qs]).


%%====================================================================
%% Internal -- Binary manipulation helper functions 
%%====================================================================
-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
  <<>>;
binary_join([Part], _Sep) ->
  Part;
binary_join(List, Sep) ->
  lists:foldr(fun (A, B) ->
    if
      bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
      true -> A
    end
  end, <<>>, List).

int_to_binary(Int) ->
    list_to_binary(integer_to_list(Int)).
