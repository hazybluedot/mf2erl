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

-module(mf2_property).

-export([implied/2, properties/1, root_names/1]).

-import(mf2, [is_property_node/1, is_root_node/1, propname_split/1]).
-import(mf2_tree, [get_onlychild/2, get_onlychild/1, get_onlychild_oftype/2, elname_isin/2, onlynodes/1]).
-import(mf2_func, [attr_getter/1]).
-import(mochiweb_node, [innerHtml/1, textContent/1]).
-import(lists, [any/2]).

-include_lib("mf2.hrl").

properties(El) ->
    PropNames = mf2_tree:class_properties(El),
    lists:filtermap(fun(PropName) ->
                            {Prefix,Name} = propname_split(PropName),
                            case lists:member(Prefix, ?PropertyPrefix) of
                                true -> { true, {Name, value(Prefix, El)} };
                                false -> false
                            end
                    end, PropNames).

root_names(El) ->
    PropNames = mf2_tree:class_properties(El),
    lists:filter(fun(PropName) ->
                         {Prefix,_} = propname_split(PropName),
                     lists:member(Prefix, ?RootPrefix)
                 end, PropNames).

%%TODO: refactor to merge functionality of circuit/first_defined and supporting functions
%% prefix_filter(Prefix) ->
%%     fun(Elem) when erlang:is_list(Elem) ->
%%             get_prefix(Elem) =:= Prefix;
%%        (_) -> erlang:error("Must be a list (well, a string, but you know, whatever)")
%%     end.

%-spec first_defined().
first_defined([], Args, NoMatch) when erlang:is_function(NoMatch) ->
    NoMatch(Args);
first_defined([], _Args, NoMatch) ->
    NoMatch;
first_defined([Pred|Rest], Args, NoMatch) ->
    case Pred(Args) of
        undefined ->
            first_defined(Rest, Args, NoMatch);
        false ->
            first_defined(Rest, Args, NoMatch);
        Value when erlang:is_function(Value) ->
            Value(Args);
        Value -> Value
    end.

circuit([], Args, NoMatch) when erlang:is_function(NoMatch) ->
    NoMatch(Args);
circuit([], _Args, NoMatch) ->
    NoMatch;
circuit([{Pred,Value}|Rest], Args, NoMatch) ->
    case Pred(Args) of
        undefined ->
            circuit(Rest, Args, NoMatch);
        false ->
            circuit(Rest, Args, NoMatch);
        true when erlang:is_function(Value) ->
            Value(Args);
        true ->
            Value;
        Incr when erlang:is_function(Incr) ->
            Incr(Args);
        Incr -> Incr
    end.

normalize_url(Url) ->
    Url.

has_class_prop(Prop) ->
    fun({_,_,_} = El) ->
            lists:member(Prop, mf2_tree:class_properties(El))
    end.

vcp_value(private, {_, _, _} = El) ->
    circuit([ { has_class_prop(<<"value-title">>), attr_getter(<<"title">>) },
              { has_class_prop(<<"value">>), fun(E) -> textContent(E) end }
            ], El, undefined).
vcp_descendents({_Name, _Attr, Children}=_El) ->
    Descendents = lists:map(fun(C) ->
                                    case is_property_node(C) of
                                        true ->
                                            undefined;
                                        false -> case vcp_value(private, C) of
                                                     undefined -> vcp_descendents(C);
                                                     V -> V
                                                 end
                                    end
                            end, onlynodes(Children)),
    lists:flatten(Descendents).

vcp_value({_, _, _Children} = El) ->
    VCPDescendents = lists:filter(fun(D)->
                                          not(D =:= undefined)
                                  end, vcp_descendents(El)),
    case VCPDescendents of
        [] -> undefined;
        List ->
            erlang:list_to_binary(string:join(lists:map(fun erlang:binary_to_list/1, List), ""))
    end.

name_class_attr(Names, Attribute) ->
    ifmatch_thenuse(name_isin(Names), Attribute).

name_isin(List) ->
    fun({_Name, _Attrs, _Children} = El) ->
            case elname_isin(List, El) of
                true -> El;
                false -> undefined
            end
    end.

ifmatch_thenuse(Pred, Attr) ->
    fun(El) ->
            case Pred(El) of
                false -> undefined;
                undefined -> undefined;
                Value when not erlang:is_atom(Value) -> mf2_tree:get_attr(Attr, Value)
            end
    end.
                    

value(p, {_Name, _Attrs, _Children} = El)->
    first_defined([ fun vcp_value/1,
                    %%{ elname_isin(<<"abbr">>), attr_getter(<<"title">> }
                    name_class_attr(<<"abbr">>, <<"title">>),
                    name_class_attr(<<"data">>, <<"value">>),
                    name_class_attr(<<"img">>, <<"alt">>)
                  ], El, textContent(El));

value(u, {_Name, _Attrs, _Children} = El) ->                                    
    Value = first_defined([
                           name_class_attr([<<"a">>, <<"area">>], <<"href">>),
                           name_class_attr([<<"img">>, <<"audio">>, <<"video">>], <<"src">>),
                           name_class_attr(<<"object">>, <<"data">>),
                           fun vcp_value/1,
                           name_class_attr(<<"abbr">>, <<"title">>),
                           name_class_attr([<<"data">>, <<"input">>],  <<"value">>)
                          ], El, textContent(El)),
    normalize_url(Value);
value(dt, {_, _, _Children} = El) ->
    first_defined([
                   fun vcp_value/1,
                   name_class_attr([<<"time">>, <<"ins">>], <<"datetime">>)
                  ], El, textContent(El));
value(e, {_,_,_} = El) ->
    [ { <<"html">>, innerHtml(El) },
      { <<"value">>, textContent(El) }
    ].

implied(El, CurrentProps) ->    
    case is_root_node(El) of
        true -> do_implied(El, CurrentProps);
        false -> []
    end.

onlychild(ChildName) ->
    fun(El) ->
            case get_onlychild(ChildName, El) of
                undefined -> undefined;
                Child  -> Child
            end
    end.

onlychild_oftype(Types) ->
    fun(El) ->
            get_onlychild_oftype(Types, El)
    end.

onlygrandchild(Names) ->
    %% TODO: do we want to use get_only_nth_child from mf2_tree?
    fun(El) ->
            case get_onlychild(El) of
                undefined -> undefined;
                Child -> get_onlychild(Names, Child)
            end
    end.                             

do_imply(<<"name">>, {_,_,_} = El) ->
    circuit( [
              { name_isin([<<"img">>, <<"area">>]), attr_getter(<<"alt">>) },
              { name_isin(<<"abbr">>), attr_getter(<<"title">>) },
              { onlychild(<<"img">>), attr_getter(<<"alt">>) },
              { onlychild(<<"area">>), attr_getter(<<"alt">>) },  
              { onlychild(<<"abbr">>), attr_getter(<<"title">>) },
              %%TODO: .h-x>:only-child:not[.h-*]>img:only-child[alt]:not([alt=""]):not[.h-*]
              { onlygrandchild([<<"img">>, <<"area">>]), attr_getter(<<"alt">>) },
              { onlygrandchild(<<"abbr">>), attr_getter(<<"title">>) }
             ], El, textContent(El));
    %% first_defined( [
    %%                 ifmatch_thenuse(name_isin([<<"img">>, <<"area">>]), <<"alt">>),
    %%                 ifmatch_thenuse(name_isin(<<"abbr">>), <<"title">>),
    %%                 ifmatch_thenuse(onlychild(<<"img">>), <<"alt">>),
    %%                 ifmatch_thenuse(onlychild(<<"area">>), <<"alt">>),  
    %%                 ifmatch_thenuse(onlychild(<<"abbr">>), <<"title">>),
    %%                 %%TODO: .h-x>:only-child:not[.h-*]>img:only-child[alt]:not([alt=""]):not[.h-*]
    %%                 ifmatch_thenuse(onlygrandchild([<<"img">>, <<"area">>]), <<"alt">>),
    %%                 ifmatch_thenuse(onlygrandchild(<<"abbr">>), <<"title">>)
    %%                ], El, textContent(El));
do_imply(<<"photo">>, {_,_,_} = El) ->
    first_defined([
                   ifmatch_thenuse(name_isin(<<"img">>), <<"src">>),
                   ifmatch_thenuse(name_isin(<<"object">>), <<"data">>),
                   ifmatch_thenuse(onlychild_oftype(<<"img">>), <<"img">>),
                   ifmatch_thenuse(onlychild_oftype(<<"object">>), <<"data">>)
                   ], El, undefined);
do_imply(<<"url">>, {_,_,_} = El) ->
    first_defined([
                   ifmatch_thenuse(name_isin([<<"a">>, <<"area">>]), <<"href">>),
                   ifmatch_thenuse(onlychild_oftype([<<"a">>, <<"area">>]), <<"href">>)
                  ], El, undefined).
    
do_implied({_, _,_} = El, CurrentProps) ->
    lists:foldl(fun(Name, Props) ->
                        case not proplists:is_defined(Name, Props) of
                            true->
                                case do_imply(Name, El) of
                                       undefined -> Props;
                                       Value -> mf2_proplist:add({Name, Value}, Props)
                                   end;
                            false -> Props
                        end
                end, CurrentProps, [<<"name">>, <<"photo">>, <<"url">>]).
            
        
