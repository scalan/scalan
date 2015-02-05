#!/bin/sh
set -o errexit

git fetch origin
git fetch github
git push https://github.com/scalan/scalan-ce.git origin/master:master
