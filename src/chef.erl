-module(chef).
-author("Mark Imbriaco <mark@37signals.com>").
-author("Sean Cribbs <seancribbs@gmail.com>").
-author("Kevin Smith <kevin@hypotheticalabs.com>").

% -export([checksum/1, cookbooks/1, segment_files/3]).
-compile([export_all]).
cookbooks(Config) ->
  CookbookPaths = cookbook_roots(Config),
  lists:usort([filename:basename(F) || F <- CookbookPaths]).

combined_segment_files(Segment, Cookbooks, Config) ->
  Files = [ segment_files(Cookbook, Segment, Config) || Cookbook <- Cookbooks ],
  lists:flatten(Files).

attribute_files(Cookbooks, Config) ->
  combined_segment_files("attributes", Cookbooks, Config).

definition_files(Cookbooks, Config) ->
  combined_segment_files("definitions", Cookbooks, Config).

library_files(Cookbooks, Config) ->
  combined_segment_files("libraries", Cookbooks, Config).

recipe_files(Cookbooks, Config) ->
  combined_segment_files("recipes", Cookbooks, Config).

template_files(Cookbooks, Config) ->
  combined_segment_files("templates", Cookbooks, Config).

segment_files(Cookbook, Segment, Config) ->
  CookbookPaths = paths_for_cookbook(Cookbook, Config),
  CandidatePaths = resolve_cookbook_segments(CookbookPaths, Segment),
  SegmentRegex = segment_regexp(Segment),
  FileMap = lists:foldl(fun(S, Acc) -> gather_cookbook_files(S, SegmentRegex, Acc) end, dict:new(), CandidatePaths),
  lists:map(fun({_, V}) -> V end, dict:to_list(FileMap)).

segment_regexp("templates") -> "\\.erb$";
segment_regexp("files") -> ".*";
                                 segment_regexp(_) -> "\\.rb$".

cookbook_roots(Config) ->
                                                       RootPaths = lists:flatmap(fun(D) -> filelib:wildcard(filename:join(D, "*")) end,
                                                                                 proplists:get_value(cookbooks, Config, [])),
                                                       lists:filter(fun(D) -> filelib:is_dir(D) =:= true end, RootPaths).

paths_for_cookbook(Cookbook, Config) ->
  lists:filter(fun(I) -> filename:basename(I) =:= Cookbook end, cookbook_roots(Config)).

checksum(Path) ->
  case file:read_file(Path) of
    {ok, Content} ->
      Hash = sha2:hexdigest256(Content),
      lists:flatten(lists:map(fun(Byte) -> io_lib:format("~.16b", [Byte]) end, binary_to_list(Hash)));
    Error ->
      Error
  end.

resolve_cookbook_segments(CookbookPaths, Segment) ->
  lists:foldr(fun(D, Acc) ->
                  Candidate = filename:join(D, Segment),
                  case filelib:is_file(Candidate) of
                    true ->
                      [Candidate|Acc];
                    false ->
                      Acc
                  end end, [], CookbookPaths).

gather_cookbook_files(Path, Regex, Accumulator) ->
  filelib:fold_files(Path, Regex, true, fun(Item, Acc) ->
                                            RelativePath = string:substr(Item, length(Path)+2),
                                            case dict:is_key(RelativePath, Acc) of
                                              true ->
                                                Acc;
                                              false ->
                                                dict:store(RelativePath, {Item, checksum(Item)}, Acc)
                                            end end, Accumulator).
