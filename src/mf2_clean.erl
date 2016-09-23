-module(mf2_clean).

-export([clean_tree/1]).

-define(ELEMENT_WHITELIST, ["html", "body", "p", "h1", "h2", "h3", "h4", "blockquote", "span", "div", "ul", "ol", "li", "img", "a", "link", "area", "object", "section", "article", "header", "footer", "aside", "em", "b", "strong"]).

clean_attrs(Attrs) ->
    lists:foldl(fun({Name, Value}=Attr, Cleaned) ->
                        case lists:member(Name, [<<"href">>, <<"src">>]) of
                            true -> case lists:prefix("javascript:", erlang:binary_to_list(Value)) of
                                        true -> Cleaned;
                                        false-> [Attr|Cleaned]
                                    end;
                            false -> [Attr|Cleaned]
                        end
                end, [], Attrs).

clean_node(Text) when erlang:is_binary(Text) ->
    {true, Text};
clean_node({Name, Attrs, _Children}= _El) ->
    case lists:member(erlang:binary_to_list(Name), ?ELEMENT_WHITELIST) of
        true -> {true, {Name, clean_attrs(Attrs), lists:filtermap(fun clean_node/1, _Children)}};
        false -> false
    end.
             
clean_tree(El) ->
    case clean_node(El) of 
        {true, Cleaned} -> Cleaned;
        _ -> none
    end.
                                 

                                                          
