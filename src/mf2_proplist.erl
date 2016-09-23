%% Copyright 2016 Darren Maczka

%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at

%%   http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mf2_proplist).

-export([add/2, add/3]).
-import(mf2_tree, [name_of/1]).

%%TODO: Does it make sense to add a list of valid MF2 property names and validate against that?
%%add({Name, Value}, List) when erlang:is_atom(Name) ->
%%    case lists:member(Name, [name, url, 
add({Name, Value}, List) ->
    add(Name, Value, List).
add(Name, Value, List) ->
    Normalized = erlang:list_to_binary(name_of(Name)),
    Value0 = proplists:append_values(Normalized, [ {Normalized, Value} | List ]),
    List1 = proplists:delete(Normalized, List),
    [ {Normalized, Value0} | List1 ].
                                             
