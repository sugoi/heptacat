{-



[nushio@myhost heptacat-example-record]$  git config  merge.tool heptacat
[nushio@myhost heptacat-example-record]$  git config  mergetool.heptacat.cmd 'heptacat merge "$BASE" "$LOCAL" "$REMOTE" "$MERGED"'
[nushio@myhost heptacat-example-record]$ git config mergetool.trustExitCode false


-}