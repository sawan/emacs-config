#! /bin/bash

CURRENT_GIT_BRANCH=`git branch | grep ^\* - | tr -d "*"` && git checkout master && git pull && git checkout $CURRENT_GIT_BRANCH