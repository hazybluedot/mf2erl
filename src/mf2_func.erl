-module(mf2_func).

-export([attr_getter/1, attr_getter/2]).

attr_getter(AttrName) ->
    attr_getter(AttrName, undefined).
attr_getter(AttrName, Default) ->
    fun({_, Attrs, _} = _El) ->
            proplists:get_value(AttrName, Attrs, Default)
    end.

