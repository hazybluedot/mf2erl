-module(mf2_tree).

-export([
         onlynodes/1,
         class_properties/1,
         get_attr/2, 
         get_onlychild/1, get_onlychild/2, 
         get_onlychild_oftype/2,
         elname_isin/2]).

-define(PropertyPrefix, [ "p", "u", "dt", "e" ]).
-define(RootPrefix, "h-").

%% Attributes
get_attr(Name, {_, Attrs, _} = _El) when erlang:is_binary(Name) ->
    proplists:get_value(Name, Attrs);
get_attr(Name, Attrs) when erlang:is_list(Name) ->
    proplists:get_value(erlang:list_to_binary(Name), Attrs).

class_properties({_,_,_} = El) ->
    case get_attr(<<"class">>, El) of
        undefined -> [];
        Value -> lists:map(fun erlang:list_to_binary/1, 
                           string:tokens(erlang:binary_to_list(Value), " ")
                          )
    end.

onlynodes(Siblings) ->            
    lists:filter(fun({_,_,_}) ->
                         true;
                    (_) -> false
                 end, Siblings).

elname_isin([First|_] = Names, {Name,_,_} = _El) when erlang:is_list(Names) and erlang:is_binary(First) ->
    lists:member(Name, Names);
elname_isin(Name, El) when erlang:is_binary(Name) ->
    elname_isin([Name], El).

%% Children and grandchildren
get_onlychild({_Name, _Attrs, [Only|[]]} = _El) when not erlang:is_binary(Only) -> Only;
get_onlychild(_) -> undefined.

get_onlychild(Names, El) ->
    case get_onlychild(El) of
        undefined ->
            undefined;
        Child -> case elname_isin(Names, Child) of
                     true  -> Child;
                     false -> undefined
                 end
    end.

get_only_nth_child(0, El) ->
    El;
get_only_nth_child(Depth, El) when erlang:is_integer(Depth) ->
    case get_onlychild(El) of
        undefined -> undefined;
        Child -> get_only_nth_child(Depth-1, Child)
    end.

get_onlychild_oftype(Types, {_Name, _Attrs, Children} = _El) ->    
    OnlyOfType = lists:filter(fun(Child) ->
                                      elname_isin(Types, Child)
                              end, onlynodes(Children)),
    case OnlyOfType of
        [Only|[]] ->
            Only;
        _ -> undefined
    end.
