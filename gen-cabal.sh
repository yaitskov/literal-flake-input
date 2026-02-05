#!/usr/bin/env bash

for i in *.cabal.template ; do
    sed  "/^description:/bx ; b ; :x ; n ; e pandoc --to=haddock README.md | sed -E -e 's/^/    /' -e '1,3d' ;" \
         $i > ${i%.template}
done
