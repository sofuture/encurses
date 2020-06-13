-module(paths).
-export([nif_dir/0, nif_dir/2]).

nif_dir() -> 
    Cpd = code:priv_dir(encurses),
    MoCpd = filename:dirname(code:which(?MODULE)) ++ "/../priv",
    nif_dir(Cpd, MoCpd).


nif_dir({error, _}, ModulePrivDir) -> handle_escript_format(ModulePrivDir);
nif_dir(Dir, _) -> handle_escript_format(Dir).

handle_escript_format(Dir) -> 
    Handled = re:replace(Dir, 
               "(_build/[^/]+)/bin/([^/]+)/encurses/priv",
               "\\1/lib/encurses/priv"),
    binary_to_list(iolist_to_binary(Handled)).
