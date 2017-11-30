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
-module(nkimage_netscale_pillow_callbacks).
-export([nkimage_parse_processor/2, 
         nkimage_processor_syntax/0, 
         nkimage_process/3]).
-include("../../include/nkimage.hrl").

nkimage_parse_processor(Data, ParseOpts) ->
    nkimage_netscale_pillow:parse_processor(Data, ParseOpts).

nkimage_processor_syntax() ->
    nkimage_netscale_pillow:processor_syntax().

nkimage_process(SrvId, #{class:= imaginary}=Processor, Req) ->
    nkimage_netscale_pillow:process(SrvId, Processor, Req);

nkimage_process(_, _, _) ->
    continue.
