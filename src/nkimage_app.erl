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
-module(nkimage_app).
-behaviour(application).
-export([start/2, stop/1]).
-export([get_processor/1, put_processor/2, get/2]).
-include("nkimage.hrl").
-define(APP, nkimage).

start(_Type, _Args) ->
    Syntax = #{processors => {list, map}},
    case nklib_config:load_env(?APP, Syntax) of
        {ok, _} ->
            {ok, Vsn} = application:get_key(?APP, vsn),
            lager:info("nkimage v~s is starting", [Vsn]),
            {ok, Pid} = nkimage_sup:start_link(),
            {ok, Pid};
        {error, Error} ->
            error({syntax_error, Error})
    end.

stop(_) ->
    ok.

get_processor(Id) -> 
    get({processor, nklib_util:to_binary(Id)}, not_found).

put_processor(Id, processor) ->
    Ids = get(processor_ids, []),
    put(processor_ids, nklib_util:store_value(nklib_util:to_binary(Id), Ids)),
    put({processor, Id}, processor).

get(Key, Default) ->
    nklib_config:get(?APP, Key, Default).

