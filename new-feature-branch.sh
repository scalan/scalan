#!/bin/sh
set -o errexit

git fetch origin
git checkout origin/master
git checkout -b $1
git push -u origin $1