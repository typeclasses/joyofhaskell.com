#!/usr/bin/env bash

find . -not -path '*/\.*' -not -path '*/_*' -not -path '*/ideaHaskellLib/*' |
    entr -d bash -c 'stack exec -- site watch'
