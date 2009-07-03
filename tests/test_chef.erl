-module(test_chef).
-author('Mark Imbriaco <mark@imbriaco.com>').
-author("Sean Cribbs <seancribbs@gmail.com>").
-author("Kevin Smith <kevin@hypotheticalabs.com>").

-include_lib("eunit/include/eunit.hrl").

-define(CONFIG, [{cookbooks,["tests/data/cookbooks", "tests/data/kitchen"]}]).

cookbooks_test() -> [
                     ?assertMatch(["apache2", "openldap"], chef:cookbooks(?CONFIG))
                    ].

attribute_files_test() ->
  [
   ?assert(proplists:is_defined("tests/data/cookbooks/openldap/attributes/default.rb",
                                chef:attribute_files(["openldap"], ?CONFIG))),
   ?assert(proplists:is_defined("tests/data/cookbooks/openldap/attributes/default.rb",
                                chef:attribute_files(["apache2", "openldap"], ?CONFIG))),
   ?assert(proplists:is_defined("tests/data/cookbooks/apache2/attributes/default.rb",
                                chef:attribute_files(["apache2"], ?CONFIG))),
   ?assert(proplists:is_defined("tests/data/cookbooks/apache2/attributes/default.rb",
                                chef:attribute_files(["apache2", "openldap"], ?CONFIG)))
  ].

definition_files_test() ->
  [
   ?assert(proplists:is_defined("tests/data/cookbooks/openldap/definitions/client.rb",
                                chef:definition_files(["apache2", "openldap"], ?CONFIG))),
   ?assert(proplists:is_defined("tests/data/cookbooks/openldap/definitions/server.rb",
                                chef:definition_files(["apache2", "openldap"], ?CONFIG)))
  ].

library_files_test() ->
  [
   ?assert(proplists:is_defined("tests/data/cookbooks/apache2/libraries/foo.rb",
                                chef:library_files(["apache2", "openldap"], ?CONFIG)))
  ].

recipe_files_test() ->
  [
   ?assert(proplists:is_defined("tests/data/cookbooks/apache2/recipes/default.rb",
                                chef:recipe_files(["apache2", "openldap"], ?CONFIG))),
   ?assert(proplists:is_defined("tests/data/cookbooks/openldap/recipes/default.rb",
                                chef:recipe_files(["apache2", "openldap"], ?CONFIG))),
   ?assert(proplists:is_defined("tests/data/cookbooks/openldap/recipes/gigantor.rb",
                                chef:recipe_files(["apache2", "openldap"], ?CONFIG))),
   ?assert(proplists:is_defined("tests/data/cookbooks/openldap/recipes/one.rb",
                                chef:recipe_files(["apache2", "openldap"], ?CONFIG)))
  ].

template_files_test() ->
  [
   ?assert(proplists:is_defined("tests/data/cookbooks/openldap/templates/default/test.erb",
                                chef:template_files(["apache2", "openldap"], ?CONFIG)))
  ].

segment_files_test() ->
  [
   ?assert(proplists:is_defined("tests/data/cookbooks/openldap/attributes/default.rb",
                                chef:segment_files("openldap", "attributes", ?CONFIG))),
   ?assertNot(proplists:is_defined("tests/data/kitchen/openldap/attributes/default.rb",
                                   chef:segment_files("openldap", "attributes", ?CONFIG))),
   ?assert(proplists:is_defined("tests/data/kitchen/openldap/attributes/robinson.rb",
                                chef:segment_files("openldap", "attributes", ?CONFIG))),
   ?assert(proplists:is_defined("tests/data/cookbooks/openldap/definitions/client.rb",
                                chef:segment_files("openldap", "definitions", ?CONFIG))),
   ?assertNot(proplists:is_defined("tests/data/kitchen/openldap/definitions/client.rb",
                                   chef:segment_files("openldap", "definitions", ?CONFIG))),
   ?assert(proplists:is_defined("tests/data/kitchen/openldap/definitions/drewbarrymore.rb",
                                chef:segment_files("openldap", "definitions", ?CONFIG))),
   ?assert(proplists:is_defined("tests/data/cookbooks/openldap/recipes/gigantor.rb",
                                chef:segment_files("openldap", "recipes", ?CONFIG))),
   ?assertNot(proplists:is_defined("tests/data/kitchen/openldap/recipes/gigantor.rb",
                                   chef:segment_files("openldap", "recipes", ?CONFIG))),
   ?assert(proplists:is_defined("tests/data/kitchen/openldap/recipes/woot.rb",
                                chef:segment_files("openldap", "recipes", ?CONFIG))),
   ?assert(proplists:is_defined("tests/data/cookbooks/openldap/files/default/.dotfile",
                                chef:segment_files("openldap", "files", ?CONFIG)))
  ].

checksum_test() ->
  [?assertMatch("ceb1f8ef42fe91c2bc36bc147d16ca436e1d27d4e4d4406a3117a47d4d87c5d9",
                chef:checksum("tests/data/recipes/test.rb")),
   ?assertMatch({error, enoent}, chef:checksum("not_here"))].
