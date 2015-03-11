#!/bin/bash

cabal build
cp dist/build/blog/blog blog
strip blog
tar czfv blog.deploy blog assets migrations
rm blog
ssh nathaniel@blog.symer.io "mkdir -p /deploy/blog.symer.io.new"
scp blog.deploy nathaniel@blog.symer.io:/deploy/blog.symer.io.new/
ssh nathaniel@blog.symer.io <<EOT
cd /deploy/blog.symer.io.new;
tar zxvf blog.deploy;
rm blog.deploy;
cd /deploy;
mv blog.symer.io blog.symer.io.old;
mv blog.symer.io.new blog.symer.io;
chmod +x /deploy/blog.symer.io/blog
/etc/init.d/blog restart;
EOT