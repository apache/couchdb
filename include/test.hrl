-define(TMP_DIR, "../../../tmp/lib").

-define(TMP_FILE, fun(File) ->
                          filename:join(?TMP_DIR, File)
                  end).

%% priv_dir() ->
%%   Dir = filename:join([t:config(priv_dir), "data", atom_to_list(?MODULE), pid_to_list(self())]),
%%   filelib:ensure_dir(filename:join([Dir, atom_to_list(?MODULE)])),
%%   Dir.

%% priv_file(File) ->
%%   filename:join(priv_dir(), File).
