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

-module(mf2_parse).

-export([parse/1, parse_file/1]).

-import(mf2, [unfold/1, make_item/2, is_root_node/1]).
-import(mf2_tree, [onlynodes/1]).

-include_lib("mf2.hrl").


to_json({Items, Rels, RelUrls} = _Result) ->
    Result0 = [ {"items", lists:map(fun(Item) -> unfold(Item) end, Items)},
                {"rels", Rels}, 
                {"rel-urls", RelUrls}
              ],
    erlang:iolist_to_binary(mochijson2:encode(Result0)).

parse(Html) ->
    Tree = mochiweb_html:parse(Html),
    %Result = folddf(do_parse/2, {[],{},{}}, Tree),
    Items = case parse_node(Tree) of
                #item{}=Item -> [Item];
                [_|_]=List -> List
            end,
    to_json({Items, {}, {}}).
parse_file(File) ->
    case file:read_file(File) of
        {ok, Contents} -> parse(Contents);
        _ -> { error, "Could not read file" }
    end.

merge_props(#item{}=Item,Items) when erlang:is_list(Items) ->
    [Item|Items];
merge_props([#item{}|_]=Children, Items) when erlang:is_list(Items) ->
    mf2_proplist:add({<<"children">>, Children}, Items);
merge_props(Props1, Props2) when erlang:is_list(Props1) and erlang:is_list(Props2) ->
    %%io:format("merging ~p with ~p~n", [Props1,Props2]),
    lists:foldl(fun(Prop, Acc) ->
                        mf2_proplist:add(Prop, Acc)
                end, Props2, Props1).

item_or_props({_,_,_} = El, ElProps, ChildProps) when erlang:is_list(ElProps) and erlang:is_list(ChildProps) ->
    %%io:format("item or props for ~p~n", [El]),
    MergedProps = merge_props(ElProps, ChildProps),
    %%io:format("merged: ~p~n", [MergedProps]),
    case is_root_node(El) of
        true -> Item = make_item(mf2_property:root_names(El), mf2_property:implied(El, ChildProps)),
                case ElProps of
                    [] -> Item;
                    _ ->
                        %%io:format("mapping over ~p~n", [proplists:get_keys(ElProps)]),
                        Value = erlang:list_to_binary(lists:foldl(fun({_,Val}, Acc) ->
                                                    Acc ++ Val
                                            end, "", ElProps)),
                        lists:map(fun({Key,_Value}) ->
                                          {Key, [Item#item{value=Value}]}
                                  end, ElProps)
                end;
        false -> MergedProps
    end.

%% Node with no children
%parse_node({_Name, _Attrs, []} = El) ->
%    Props = parse_properties(El),
%    item_or_props(El, Props, []);
%% Node with children
parse_node({_,_,Children} = El) -> 
    Props0 = case onlynodes(Children) of
                 [] -> [];
                 [_|_] = ChildNodes -> 
                     %%io:format("folding over childnodes ~p~n", [ChildNodes]),
                     lists:foldl(fun(Node, Acc) ->
                                         merge_props(parse_node(Node), Acc)
                                 end, [], ChildNodes)
             end,
    item_or_props(El, parse_properties(El), Props0).

parse_properties({_Name, _Attrs, _Children} = El) ->
    %%io:format("Folding for type ~p from ~p~n", [text, properties(text, El)]),
    lists:foldl(fun({Name,Value}, Acc) ->
                        mf2_proplist:add(Name, Value, Acc)
                end, [], mf2_property:properties(El)).            
                                                                             
                                                                            
