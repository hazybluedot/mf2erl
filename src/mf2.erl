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

-module(mf2).

-include_lib("mf2.hrl").

-export([make_item/2, make_item/3, 
         push_item/2, 
         unfold/1,
        is_property_node/1,
        is_root_node/1,
        propname_split/1]).

-import(lists, [any/2]).

make_item(Type, Props, Children) ->
    %%io:format("making item type=~p, props=~p~n", [Type, Props]),
    #item{type=Type,
          properties=Props,
          children=Children}.
make_item(Type, Props) ->
    make_item(Type, Props, []).

unfold(#item{type=Type, properties=Properties, children=Children, value=SimpleValue} = _Item) ->
    Base = [{"type", Type},
            {"properties", lists:map(fun({Name, Values}) ->
                                             {Name, lists:map(fun(#item{}=Item) ->
                                                                      unfold(Item);
                                                                 (Value) -> Value
                                                              end, Values)}
                                     end, Properties)}],
    Base1 = case Children of
                %% only add children struct if there are children
                [] -> Base;
                _ -> Base ++ [{"children", lists:map(fun unfold/1, Children)}]
            end,
    case SimpleValue of
        "" -> Base1;
        _ -> Base1 ++ [{"value", SimpleValue}]
    end.
             
push_item(#item{} = Item, {[], _Rels, _RelUrls}) ->
    {[Item], _Rels, _RelUrls};
push_item(#item{}=Item, {Items, _Rels, _RelUrls}) ->
    {[Item|Items], _Rels, _RelUrls}.

propname_split(String) when erlang:is_binary(String) ->
    [Prefix|Name] = string:tokens(erlang:binary_to_list(String), "-"),
    { erlang:list_to_atom(Prefix), erlang:list_to_binary(Name) }. %%TODO: might have to join Name                                    
get_prefix(String) ->
    {Prefix, _ } = propname_split(String),
    Prefix.

is_property_node({_,_,_} = El) ->
    any(fun(Name) ->
                lists:member(get_prefix(Name), ?PropertyPrefix)
        end, mf2_tree:class_properties(El)).
is_root_node({_,_,_} = El) ->
    any(fun(Name) ->
                lists:member(get_prefix(Name), ?RootPrefix)
        end, mf2_tree:class_properties(El)).
