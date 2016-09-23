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

-export([value/2, implied/2]).
-import(mf2_tree, [get_onlychild/2, get_onlychild/1, get_onlychild_oftype/2, elname_isin/2]).

%-spec first_defined().
first_defined([], Args, NoMatch) when erlang:is_function(NoMatch) ->
    NoMatch(Args);
first_defined([], _Args, NoMatch) ->
    NoMatch;
first_defined([Pred|Rest], Args, NoMatch) ->
    case Pred(Args) of
        undefined ->
            first_defined(Rest, Args, NoMatch);
        Value when erlang:is_function(Value) ->
            Value(Args);
        Value -> Value
    end.

normalize_url(Url) ->
    Url.

collect_text({_,_,Children}=El) ->
    collect_text(El, <<"">>).
collect_text(Text, Acc) when erlang:is_binary(Text) and erlang:is_binary(Acc) -> 
    Concat = string:concat(erlang:binary_to_list(Text), erlang:binary_to_list(Acc)),
    erlang:list_to_binary(Concat);
collect_text({_,_,Children}=El, Acc) ->
    lists:foldl(fun collect_text/2, Acc, Children).

get_clean_text({_Name,_,_} = El) ->
    case mf2_clean:clean_tree(El) of
        none -> undefined;
        Cleaned -> collect_text(Cleaned)
    end.

concat_if_string(Value, Maybe) ->
    case Maybe of
        undefined -> Value;
        S when erlang:is_list(S) -> string:concat(Value, S);
        S when erlang:is_binary(S) -> string:concat(Value, erlang:binary_to_list(S))
    end.
            
vcp_value({_, _, Children} = _El) ->
    V = lists:foldl(fun({_,_,_} = Child, Value) ->
                            ClassAttrs = mf2_tree:get_class_properties(Child),
                            case lists:member("value-title", ClassAttrs) of
                                true -> string:concat(Value, mf2_tree:get_attr(<<"title">>, Child));
                                false -> 
                                    case lists:member("value", ClassAttrs) of
                                        true -> concat_if_string(Value, get_clean_text(Child));
                                        false -> Value
                                    end
                            end;
                       (_, Value) -> Value
                    end, "", Children),
    %io:format("VCP value of ~p~n", [V]),
    case V of 
        [] -> undefined;
        S when erlang:is_binary(S) -> case string:len(erlang:binary_to_list(V)) > 0 of
                                          true -> V;
                                          false -> undefined
                                      end;
        _ -> undefined
    end.

name_class_attr(Names, _Type, Attribute) ->
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
                    

value(text, {_Name, _Attrs, _Children} = El)->
    first_defined([ fun vcp_value/1,
                    name_class_attr(<<"abbr">>, text, <<"title">>),
                    name_class_attr(<<"data">>, text, <<"value">>),
                    name_class_attr(<<"img">>, text, <<"alt">>)
                  ], El, get_clean_text(El));

value(url, {_Name, _Attrs, _Children} = El) ->                                    
    Value = first_defined([
                   name_class_attr([<<"a">>, <<"area">>], url, <<"href">>),
                   name_class_attr([<<"img">>, <<"audio">>, <<"video">>], url, <<"src">>),
                   name_class_attr(<<"object">>, url, <<"data">>),
                   fun vcp_value/1,
                   name_class_attr(<<"abbr">>, url, <<"title">>),
                   name_class_attr([<<"data">>, <<"input">>], url, <<"value">>)
                  ], El, get_clean_text(El)),
    normalize_url(Value);
value(datetime, {_, _, Children} = El) ->
    first_defined([
           fun vcp_value/1,
           name_class_attr([<<"time">>, <<"ins">>], datetime, <<"datetime">>)
          ], El, get_clean_text(El)).

implied(El, CurrentProps) ->    
    case mf2_tree:has_root_names(El) of
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

do_imply(<<"name">>, {_Name,_,Children}=El) ->
    first_defined( [
                    ifmatch_thenuse(name_isin([<<"img">>, <<"area">>]), <<"alt">>),
                    name_class_attr(<<"abbr">>, root, <<"title">>),
                    ifmatch_thenuse(onlychild(<<"img">>), <<"alt">>),
                    ifmatch_thenuse(onlychild(<<"area">>), <<"alt">>),  
                    ifmatch_thenuse(onlychild(<<"abbr">>), <<"title">>),
                    %%TODO: .h-x>:only-child:not[.h-*]>img:only-child[alt]:not([alt=""]):not[.h-*]
                    ifmatch_thenuse(onlygrandchild([<<"img">>, <<"area">>]), <<"alt">>),
                    ifmatch_thenuse(onlygrandchild(<<"abbr">>), <<"title">>)
                   ], El, get_clean_text(El));
do_imply(<<"photo">>, El) ->
    first_defined([
                   ifmatch_thenuse(name_isin(<<"img">>), <<"src">>),
                   ifmatch_thenuse(name_isin(<<"object">>), <<"data">>),
                   ifmatch_thenuse(onlychild_oftype(<<"img">>), <<"img">>),
                   ifmatch_thenuse(onlychild_oftype(<<"object">>), <<"data">>)
                   ], El, undefined);
do_imply(<<"url">>, El) ->
    first_defined([
                   ifmatch_thenuse(name_isin([<<"a">>, <<"area">>]), <<"href">>),
                   ifmatch_thenuse(onlychild_oftype([<<"a">>, <<"area">>]), <<"href">>)
                  ], El, undefined).
    
do_implied({_Name, _Attrs, _Children} = El, CurrentProps) ->
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
            
    

        
