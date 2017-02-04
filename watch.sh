#!/usr/bin/env bash

find . -not -path '*/\.*' -not -path '*/_*' | entr -c -d bash -c 'stack exec site build'
