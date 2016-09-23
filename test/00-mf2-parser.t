#!/usr/bin/env escript

-define(SCRIPTDIR, filename:dirname(escript:script_name())).

init() ->
    %% Is there a better/standard way to get the built lib stuff into the path for testing?
    LibPath = ?SCRIPTDIR ++ "/../_build/default/lib/",
    case file:list_dir(LibPath) of
               { ok, Files0 } ->
                   lists:foldl(fun(Name, Res) -> 
                                       Res andalso code:add_pathz(
                                         filename:join([LibPath, Name, "ebin"]))
                               end, true, Files0);
               { error, Reason0 } -> { error, Reason0 }
    end.

main(Args) ->
    true = init(),    
    TestData = ?SCRIPTDIR ++ "/mf2/tests/microformats-v2/h-entry",
    TestFiles = case Args of
                    [] -> case file:list_dir(TestData) of 
                              { ok, Files } ->
                                  lists:map(fun(F) ->
                                                    filename:join(TestData,F)
                                            end, lists:filter(fun(Name) ->
                                                       filename:extension(Name) =:= ".html"
                                               end, Files));
                              { error, Reason0 } -> erlang:exit(Reason0)
                          end;
                    [_|_] -> Args
                end,
    
    lists:foreach(fun(TestFile) ->
                          %io:format("~s:~n", [TestFile]),
                          try mf2_parse:parse_file(TestFile) of
                              Result -> io:format("~s~n", [Result])
                          %%end
                          catch
                              Exception:Reason -> {caught, Exception, Reason},
                                                  io:format("~s: Exception: ~p~n", [TestFile,Reason])
                                                      , io:format("~p~n", [Exception])
                                                      , erlang:error(Reason)
                                                      
                          end
                  end, TestFiles).
    
    
