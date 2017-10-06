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
-module(nkimage_callbacks).
-export([plugin_deps/0, plugin_syntax/0]).
-export([service_init/2]).
-export([nkimage_get_processor/2, nkimage_parse_processor/2, nkimage_resize/4]).
-include("nkimage.hrl").


%% ===================================================================
%% Plugin callbacks
%%
%% These are used when nkimage is started as a NkSERVICE plugin
%% ===================================================================


plugin_deps() ->
    [].

plugin_syntax() ->
    #{}.

service_init(_Service, #{id:=SrvId}=State) ->
    ?INFO("service init: ~p", [SrvId]),
    lists:foreach(
        fun 
            (#{id := Id}=Data) ->
                case SrvId:nkimage_parse_processor(Data, #{}) of
                    {ok, processor, _} ->
                        ?WARN("loading processor ~p", [Id]),
                        nkimage_app:put_processor(nklib_util:to_binary(Id), processor);
                    {error, Error} ->
                        ?WARN("error with processor ~p: ~p", [Data, Error])
                end;
            (Data) ->
                ?WARN("invalid processor: ~p", [Data])
        end, 
        nkimage_app:get(processors, [])), 
    {ok, State}.

%% ===================================================================
%% Image processing callbacks
%% ===================================================================

%% @doc Gets a processor
-spec nkimage_get_processor(nkservice:id(), nkimage:processor_id()) ->
    {ok, nkimage:processor()} | {error, term()}.

nkimage_get_processor(_SrvId, Id) ->
    case nkimage_app:get_processor(Id) of
        not_found ->
            {error, {processor_not_found, Id}};
        Processor ->
            {ok, Processor}
    end.

%% @doc Parses a processor
-spec nkimage_parse_processor(map(), nklib_syntax:parse_opts()) ->
    {ok, nkimage:processor(), [binary()]} | {error, term()}.

nkimage_parse_processor(_Processor, _Opts) ->
    {error, invalid_processor}.


%% @doc Resizes an image to the given size
-spec nkimage_resize(nkservice:id(), nkimage:processor(), nkimage:size(), nkfile:file()) ->
    {ok, binary()} | {error, term()}.

nkimage_resize(_SrvId, _Processor, _Size, _File) ->
    {error, invalid_processor}.
