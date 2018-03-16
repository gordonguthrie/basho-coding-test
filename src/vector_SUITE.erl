%%% @author    Gordon Guthrie
%%% @copyright (C) 2014
%%% @doc
%%%
%%% @end
%%% Created :  6 Oct 2014 by gordon@vixo.com

-module(vector_SUITE).

%% only use in test suites
-compile(export_all).

-include("test_server.hrl").
-include("vector.hrl").

%%%
%%% Unit tests
%%%
%%% writing unit tests using common tests so I can macroize their
%%% bodies becuz purty
%%%

-define(test(Name, A, B, Expected) ,
        Name(_Config) ->
               Acc = vector:compare_TESTING(A, B),
               Got = vector:check_TESTING(Acc),
               case Got of
                   Expected -> {test, ok};
                   _        -> print("Expected", Expected),
                               print("Got", Got),
                               exit("test failed " ++ atom_to_list(Name))
               end).

print(String, #result{'a descends b'    = R1,
                      'b descends a'    = R2,
                      'a equals b'      = R3,
                      'a, b concurrent' = R4}) ->
    io:format("~p:~n" ++
                  "* a descends b    ~p~n" ++
                  "* b descends a    ~p~n" ++
                  "* a equals b      ~p~n" ++
                  "* a, b concurrent ~p~n",
              [String, R1, R2, R3, R4]).

%% callbacks
init_per_suite(Config) -> Config.

end_per_suite(_Config) -> ok.

init_per_testcase(_TestCase, Config) -> Config.

end_per_testcase(_TestCase, _Config) -> ok.

all() ->
    [
     empty_equal_test,
     simple_equal_test,
     sorted_equal_test,
     unsorted_equal_test,
     first_entry_test,
     complex_a_descends_b_test,
     basic_a_descends_b_test,
     basic_a_descends_b_2_test,
     unsorted_basic_a_descends_b_2_test,
     first_entry_again_test,
     basic_b_descends_a_test,
     basic_b_descends_a_2_test,
     complex_b_descends_a_2_test,
     basic_concurrent_test,
     basic_concurrent_2_test,
     general_concurrent_test,
     general_concurrent_2_test,
     general_concurrent_3_test,
     general_concurrent_4_test,
     worked_example_test
    ].

%% Equality Tests

?test(empty_equal_test,
      [],
      [],
      #result{'a descends b'    = true,
              'b descends a'    = true,
              'a equals b'      = true,
              'a, b concurrent' = false}).

?test(simple_equal_test,
      [
       {a, 1}
      ],
      [
       {a, 1}
      ],
      #result{'a descends b'    = true,
              'b descends a'    = true,
              'a equals b'      = true,
              'a, b concurrent' = false}).

?test(sorted_equal_test,
      [
       {a, 1},
       {b, 1}
      ],
      [
       {a, 1},
       {b, 1}
      ],
      #result{'a descends b'    = true,
              'b descends a'    = true,
              'a equals b'      = true,
              'a, b concurrent' = false}).

?test(unsorted_equal_test,
      [
       {a, 1},
       {b, 1}
      ],
      [
       {b, 1},
       {a, 1}
      ],
      #result{'a descends b'    = true,
              'b descends a'    = true,
              'a equals b'      = true,
              'a, b concurrent' = false}).

%% A is a descendant of B tests

?test(first_entry_test,
      [
       {a, 1}
      ],
      [
      ],
      #result{'a descends b'    = true,
              'b descends a'    = false,
              'a equals b'      = false,
              'a, b concurrent' = false}).

?test(basic_a_descends_b_test,
      [
       {a, 2}
      ],
      [
       {a, 1}
      ],
      #result{'a descends b'    = true,
              'b descends a'    = false,
              'a equals b'      = false,
              'a, b concurrent' = false}).

?test(basic_a_descends_b_2_test,
      [
       {a, 2},
       {b, 2}
      ],
      [
       {a, 1},
       {b, 1}
      ],
      #result{'a descends b'    = true,
              'b descends a'    = false,
              'a equals b'      = false,
              'a, b concurrent' = false}).

?test(complex_a_descends_b_test,
      [
       {a, 2},
       {b, 2}
      ],
      [
       {a, 1}
      ],
      #result{'a descends b'    = true,
              'b descends a'    = false,
              'a equals b'      = false,
              'a, b concurrent' = false}).

?test(unsorted_basic_a_descends_b_2_test,
      [
       {b, 2},
       {a, 2}
      ],
      [
       {a, 1},
       {b, 1}
      ],
      #result{'a descends b'    = true,
              'b descends a'    = false,
              'a equals b'      = false,
              'a, b concurrent' = false}).

%% B is a descendant of A tests

?test(first_entry_again_test,
      [
      ],
      [
       {a, 1}
      ],
      #result{'a descends b'    = false,
              'b descends a'    = true,
              'a equals b'      = false,
              'a, b concurrent' = false}).

?test(basic_b_descends_a_test,
      [
       {a, 1}
      ],
      [
       {a, 2}
      ],
      #result{'a descends b'    = false,
              'b descends a'    = true,
              'a equals b'      = false,
              'a, b concurrent' = false}).

?test(basic_b_descends_a_2_test,
      [
       {a, 1},
       {b, 1}
      ],
      [
       {a, 2},
       {b, 2}
      ],
      #result{'a descends b'    = false,
              'b descends a'    = true,
              'a equals b'      = false,
              'a, b concurrent' = false}).

?test(complex_b_descends_a_2_test,
      [
       {a, 1}
      ],
      [
       {a, 2},
       {b, 2}
      ],
      #result{'a descends b'    = false,
              'b descends a'    = true,
              'a equals b'      = false,
              'a, b concurrent' = false}).

?test(unsorted_basic_b_descends_a_test,
      [
       {b, 1},
       {a, 1}
      ],
      [
       {a, 2},
       {b, 2}
      ],
      #result{'a descends b'    = false,
              'b descends a'    = true,
              'a equals b'      = false,
              'a, b concurrent' = false}).

%% Concurrent update tests

?test(basic_concurrent_test,
      [
       {a, 1}
      ],
      [
       {b, 1}
      ],
      #result{'a descends b'    = false,
              'b descends a'    = false,
              'a equals b'      = false,
              'a, b concurrent' = true}).

?test(basic_concurrent_2_test,
      [
       {a, 1},
       {b, 2}
      ],
      [
       {a, 2},
       {b, 1}
      ],
      #result{'a descends b'    = false,
              'b descends a'    = false,
              'a equals b'      = false,
              'a, b concurrent' = true}).

?test(general_concurrent_test,
      [
       {a, 1},
       {c, 3},
       {b, 2}
      ],
      [
       {b, 1},
       {a, 2},
       {d, 4}
      ],
      #result{'a descends b'    = false,
              'b descends a'    = false,
              'a equals b'      = false,
              'a, b concurrent' = true}).

?test(general_concurrent_2_test,
      [
       {b, 1},
       {c, 1},
       {d, 1}
      ],
      [
       {a, 1},
       {b, 1},
       {d, 1}
      ],
      #result{'a descends b'    = false,
              'b descends a'    = false,
              'a equals b'      = false,
              'a, b concurrent' = true}).

?test(general_concurrent_3_test,
      [
       {a, 1},
       {b, 1}
      ],
      [
       {a, 2}
      ],
      #result{'a descends b'    = false,
              'b descends a'    = false,
              'a equals b'      = false,
              'a, b concurrent' = true}).

?test(general_concurrent_4_test,
      [
       {a, 2}
      ],
      [
       {a, 1},
       {b, 1}
      ],
      #result{'a descends b'    = false,
              'b descends a'    = false,
              'a equals b'      = false,
              'a, b concurrent' = true}).

%% the worked example

?test(worked_example_test,
      [
       {<<199, 214, 110, 13, 247, 118, 16, 223>>, 1}
      ],
      [
       {<<0, 88, 3, 113, 66, 198, 7, 52>>,        1},
       {<<199, 214, 110, 13, 247, 118, 16, 223>>, 1}
      ],
      #result{'a descends b'    = false,
              'b descends a'    = true,
              'a equals b'      = false,
              'a, b concurrent' = false}).


