{application, swedish_chef,
 [{description, "swedish_chef"},
  {vsn, "0.1"},
  {modules, [
    swedish_chef,
    swedish_chef_app,
    swedish_chef_sup,
    swedish_chef_deps,
    chef,
    sha2
  ]},
  {registered, []},
  {mod, {swedish_chef_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
