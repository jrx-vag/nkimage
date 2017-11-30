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
-module(nkimage).
-export([parse_processor/3, process/3]).
-include("nkimage.hrl").

-type processor() :: #{ class => atom,
                        config => map() }.

-type action() :: convert | resize.
-type content_type() :: binary.
-type image_bytes() :: binary.
-type req() :: #{ action => action(),
                  from => content_type(),
                  to => content_type(),
                  height => integer,
                  width => integer,
                  body => image_bytes() }.

-spec parse_processor(nkservice:id(), map(), map()) ->
    {ok, processor()} | {error, term()}.
parse_processor(SrvId, Map, ParseOpts) ->
    SrvId:nkimage_parse_processor(Map, ParseOpts).

-spec process(nkservice:id(), processor(), req()) ->
    {ok, image_bytes()} | {error, term()}.
process(SrvId, Processor, Req) ->
    SrvId:nkimage_process(SrvId, Processor, Req).
