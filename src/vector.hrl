%%% @author    gordon@vixo.com
%%% @copyright (C) 2014
%%% @doc
%%%
%%% @end
%%% Created :  5 Oct 2014 by Gordon Guthrie

-record(acc,
        {'in a not b'     = false,
         'in b not a'     = false,
         'a later than b' = false,
         'b later than a' = false
        }).

-record(result,
        {'a descends b'    = false,
         'b descends a'    = false,
         'a equals b'      = false,
         'a, b concurrent' = false
        }).
