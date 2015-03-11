#!/bin/bash

cabal build
strip dist/build/blog/blog
rm -rf static/tmp
tar czfv blog.keter dist/build/blog/blog config assets migrations