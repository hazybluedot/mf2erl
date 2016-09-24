-module(mochiweb_node).

-export([innerHtml/1, textContent/1]).

-define(HTML_TEXTNODES, ["html", "body", "p", "h1", "h2", "h3", "h4", "blockquote", "span", "div", "ul", "ol", "li", "img", "a", "link", "area", "section", "article", "header", "footer", "aside", "em", "b", "strong"]).

textContent(private, Text) when erlang:is_binary(Text) ->
    erlang:binary_to_list(Text);
textContent(private, []) -> "";
textContent(private, [Node|Siblings]) ->
    string:concat(textContent(private, Node), textContent(private, Siblings));
textContent(private, {Name, _Attrs, Children} = El) ->
    case lists:member(erlang:binary_to_list(Name), ?HTML_TEXTNODES) of
        true -> textContent(private, Children);
        false -> ""
    end.
textContent({Name, _Attrs, Children} = El) ->
    erlang:list_to_binary(string:strip(textContent(private, El))).

innerHtml({Name, Attrs, Children} = El) ->
    erlang:list_to_binary(string:join(lists:map(fun(Child) when erlang:is_binary(Child) ->
                                  erlang:binary_to_list(Child);
                             (Child) -> mochiweb_html:to_html(Child)
              end, Children), "")).
