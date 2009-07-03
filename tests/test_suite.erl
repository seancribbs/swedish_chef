-module(test_suite).
-author("Mark Imbriaco <mark@37signals.com>").
-author("Sean Cribbs <seancribbs@gmail.com>").
-author("Kevin Smith <kevin@hypotheticalabs.com>").

-include_lib("eunit/include/eunit.hrl").

all_test_() -> [{module, test_chef}].