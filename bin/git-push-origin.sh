#! /bin/bash

CURRENT_GIT_BRANCH=`git branch | grep ^\*| cut -f 2 -d " " `
git push origin $CURRENT_GIT_BRANCH