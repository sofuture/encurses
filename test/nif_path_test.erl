-module(nif_path_test).
-include_lib("eunit/include/eunit.hrl").

'use the module priv directory when code priv isnt known_test'() ->
    NifDir = paths:nif_dir({error, bad_name}, "module dir"),
    ?assertEqual("module dir", NifDir).

'use the code priv directory_test'() -> 
    NifDir = paths:nif_dir("./priv", "module dir"),
    ?assertEqual("./priv", NifDir).

'find the lib dir when in a bin escript directory_test'() ->
    NifDir = paths:nif_dir("/dir/_build/default/bin/app/encurses/priv", "module dir"),
    ?assertEqual("/dir/_build/default/lib/encurses/priv", NifDir).

'find the lib dir when in a bin escript directory when code priv was not found_test'() ->
    NifDir = paths:nif_dir({error, bad_name}, "/dir/_build/default/bin/app/encurses/priv"),
    ?assertEqual("/dir/_build/default/lib/encurses/priv", NifDir).
