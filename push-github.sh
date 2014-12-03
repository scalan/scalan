#!/bin/sh
set -o errexit

git fetch origin
git checkout master-github
git pull
git push https://github.com/scalan/scalan-ce.git origin/master-github:master
git checkout master