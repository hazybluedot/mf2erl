-module(mf2_tree).

-export([get_text/1, 
         onlynodes/1,
         properties/2, properties/1, 
         get_class_properties/1,
         get_attr/2, 
         get_onlychild/1, get_onlychild/2, 
         get_onlychild_oftype/2,
         elname_isin/2,
         name_of/1,
         has_property_type/2, has_property_name/2, has_root_names/1]).

-define(PropertyPrefix, [ "p", "u", "dt", "e" ]).
-define(RootPrefix, "h-").

%% Text nodes
get_text({_El, _Attrs, Children}) ->
    get_text(Children);
get_text([]) ->
    <<"">>;
get_text([_|_] = Children) ->
    case lists:filter(fun(Child) ->
                                    erlang:is_binary(Child)
                      end, Children) of
        [Text|_] -> Text;
        [] -> undefined
    end.

%% Attributes
get_attr(Name, {_, Attrs, _} = _El) when erlang:is_binary(Name) ->
    proplists:get_value(Name, Attrs);
get_attr(Name, Attrs) ->
    proplists:get_value(Name, Attrs).


get_class_properties({_,Attrs,_} = _El) ->
    get_class_properties(Attrs);
get_class_properties(Attrs) ->
    case get_attr(<<"class">>, Attrs) of
        undefined -> [];
        Value -> string:tokens(erlang:binary_to_list(Value), " ")
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

get_prefix(String) ->
    [Prefix|_] = string:tokens(String, "-"),
    Prefix.

prefix_filter(Prefix) ->
    fun(Elem) when erlang:is_list(Elem) ->
            get_prefix(Elem) =:= Prefix;
       (_) -> erlang:error("Must be a list (well, a string, but you know, whatever)")
    end.

%% properties/2
properties(T, {_Name, Attrs, _Children}=_El) ->
    properties(T, Attrs);
properties(T, [{_Name,_Value}|_]=Attrs) ->
    properties(T, get_class_properties(Attrs));
properties(root, Attrs) ->
    lists:filter(prefix_filter("h"), Attrs);    
properties(text, Attrs) ->
    lists:filter(prefix_filter("p"), Attrs);
properties(url, Attrs) ->
    lists:filter(prefix_filter("u"), Attrs);
properties(embedded, Attrs) ->
    lists:filter(prefix_filter("e"), Attrs);
properties(datetime, Attrs) ->
    lists:filter(prefix_filter("dt"), Attrs).

%% properties/1
properties([{_Name,_Value}|_]=Attrs) ->
    properties(get_class_properties(Attrs));
properties(Attrs) ->
    lists:filter(fun(Elem) ->
                         lists:member(get_prefix(Elem), ?PropertyPrefix)
                 end, Attrs).

has_property_type(Type, Attrs) ->
    case properties(Type, Attrs) of 
        [] -> false;
        [_|_] -> true
    end.

name_of(Prop) when erlang:is_binary(Prop) ->
    name_of(erlang:binary_to_list(Prop));
name_of(Prop) ->
    [Prefix|Rest] = string:tokens(Prop, "-"),
    case lists:member(Prefix, ?PropertyPrefix) of
        true ->
            Rest;
        false -> Prop
    end.    

has_property_name(Name, {_,Attrs,_} = _El) ->
    has_property_name(Name, properties(Attrs));
has_property_name(Name, Attrs) ->                            
    lists:member(Name, lists:map(fun name_of/1, properties(Attrs))).

has_root_names({_Name, Attrs, _Children}) ->
    has_root_names(Attrs);
has_root_names(Attrs) ->
    case properties(root, Attrs) of 
        [] -> false;
        [_|_] -> true
    end.

