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
-module(nkimage).
-export([parse_processor/3]).
-export_type([processor_id/0, processor/0]).
-include("nkimage.hrl").
%% ===================================================================
%% Types
%% ===================================================================
-type processor_id() :: term().
-type processor() :: #{ config => map() }.


%% @doc
-spec parse_processor(nkservice:id(), map(), nklib_syntax:parse_opts()) ->
    {ok, processor(), [binary()]} | {error, term()}.

parse_processor(SrvId, Map, ParseOpts) ->
    SrvId:nkimage_parse_processor(Map, ParseOpts).