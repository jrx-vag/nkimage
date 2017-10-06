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
-module(nkimage_imaginary_callbacks).
-export([plugin_deps/0]).
-export([nkimage_parse_processor/2, nkimage_processor_syntax/0, nkimage_resize/4]).
-include("../../include/nkimage.hrl").

%% ===================================================================
%% Plugin callbacks
%%
%% These are used when nkimage Imaginary is started as a NkSERVICE plugin
%% ===================================================================

plugin_deps() ->
    [nkimage, nkfile].

%% ===================================================================
%% nkimage callbacks
%% ===================================================================
nkimage_parse_processor(Data, ParseOpts) ->
    nkimage_imaginary:parse_processor(Data, ParseOpts).

nkimage_processor_syntax() ->
    nkimage_imaginary:processor_syntax().

nkimage_resize(SrvId, #{class:= imaginary}=Processor, Size, File) ->
    nkimage_imaginary:resize(SrvId, Processor, Size, File);

nkimage_resize(_, _, _, _) ->
    continue.
