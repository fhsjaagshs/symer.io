#!/bin/bash

cabal build
cp dist/build/blog/blog blog
strip blog
tar czfv - blog assets migrations | ssh nathaniel@blog.symer.io <<EOSSH
mkdir -p /deploy/blog.symer.io.new
cd /deploy/blog.symer.io.new
tar zxvf -
chmod +x blog
cd /deploy;
mv blog.symer.io blog.symer.io.old;
mv blog.symer.io.new blog.symer.io;
/etc/init.d/blog try-restart;
EOSSH
rm blog