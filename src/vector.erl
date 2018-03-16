%%% @author    gordon@vixoc.om
%%% @copyright (C) 2014
%%% @doc
%%%
%%% @end
%%% Created :  5 Oct 2014 by Gordon Guthrie

-module(vector).

-include("vector.hrl").

-export([
         run/0
        ]).

%% export for common tests only
-export([
         compare_TESTING/2,
         check_TESTING/1
        ]).

run() ->
    {ok, Clocks} = file:consult("new-test-clocks.txt"),
    Interim = [compare(A, B) || {A, B} <- Clocks],
    Results = [check(X) || X <- Interim],
    [format(X) || X <- Results],
    ok.

compare(A, B) ->
    A1 = lists:sort(A),
    B1 = lists:sort(B),
    % io:format("A1 is ~p~nB1 is ~p~n", [A1, B1]),
    c2(A1, B1, #acc{}).

%% three terminal clauses
c2([], [], Acc) -> Acc;
c2(_,  [], Acc) -> Acc#acc{'in a not b' = true};
c2([], _,  Acc) -> Acc#acc{'in b not a' = true};
% now the comparisons
c2([{K,  N}  | Ta], [{K,  N}  | Tb], Acc)              -> c2(Ta, Tb, Acc);
c2([{K,  Na} | Ta], [{K,  Nb} | Tb], Acc) when Na > Nb -> c2(Ta, Tb, Acc#acc{'a later than b' = true});
c2([{K,  Na} | Ta], [{K,  Nb} | Tb], Acc) when Na < Nb -> c2(Ta, Tb, Acc#acc{'b later than a' = true});
c2([{Ka, Na} | Ta], [{Kb, Nb} | Tb], Acc) ->
          {La, Lb, NewA} = if
                               Ka < Kb -> {Ta,              [{Kb, Nb} | Tb], Acc#acc{'in a not b' = true}};
                               Ka > Kb -> {[{Ka, Na} | Ta], Tb,              Acc#acc{'in b not a' = true}}
                           end,
          % io:format("La is ~p~nLb is ~p~n", [La, Lb]),
          c2(La, Lb, NewA).

check(Acc) ->
    % print_acc("Acc", Acc),
    Ret = #result{'a descends b'    = does_a_descend_b(Acc),
                  'b descends a'    = does_b_descend_a(Acc),
                  'a equals b'      = does_a_equal_b(Acc),
                  'a, b concurrent' = are_a_b_concurrent(Acc)},
    % io:format("In check Ret is ~p~n", [Ret]),
    Ret.

format(#result{'a descends b'    = R1,
               'b descends a'    = R2,
               'a equals b'      = R3,
               'a, b concurrent' = R4}) ->
    String = io_lib:format("~p | ~p | ~p | ~p", [R1, R2, R3, R4]),
    plain_log(String, "output.txt").

%% print_acc(String, #acc{'in a not b'     = R1,
%%                        'in b not a'     = R2,
%%                        'a later than b' = R3,
%%                        'b later than a' = R4}) ->
%%     io:format("~p:~n" ++
%%                   "* in a not b     ~p~n" ++
%%                   "* in b not a     ~p~n" ++
%%                   "* a later than b ~p~n" ++
%%                   "* b later than a ~p~n",
%%               [String, R1, R2, R3, R4]).

plain_log(String, File) ->
    _Return = filelib:ensure_dir(File),

    case file:open(File, [append]) of
        {ok, Id} ->
            io:fwrite(Id, "~s~n", [String]),
            file:close(Id);
        _ ->
            error
    end.

does_a_descend_b(#acc{'in b not a'     = false,
                      'b later than a' = false}) -> true;
does_a_descend_b(_)                              -> false.

does_b_descend_a(#acc{'in a not b'     = false,
                      'a later than b' = false}) -> true;
does_b_descend_a(_)                              -> false.

does_a_equal_b(#acc{'in a not b'     = false,
                    'in b not a'     = false,
                    'a later than b' = false,
                    'b later than a' = false}) -> true;
does_a_equal_b(_)                              -> false.

are_a_b_concurrent(#acc{'in a not b'     = true,
                        'in b not a'     = true}) -> true;
are_a_b_concurrent(#acc{'a later than b' = true,
                        'b later than a' = true}) -> true;
are_a_b_concurrent(#acc{'in a not b'     = true,
                        'b later than a' = true}) -> true;
are_a_b_concurrent(#acc{'in b not a'     = true,
                        'a later than b' = true}) -> true;
are_a_b_concurrent(_)                             -> false.

%%%
%%% Common Tests Hooks
%%%
compare_TESTING(A, B) -> compare(A, B).

check_TESTING(A) -> check(A).
