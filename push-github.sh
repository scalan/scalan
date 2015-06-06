#!/bin/sh
set -o errexit

git fetch origin
git push https://github.com/scalan/scalan.git origin/master:master
