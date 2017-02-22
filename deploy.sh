#!/usr/bin/env bash
aws s3 sync _site s3://joh-web-public-1 --delete --profile joyofhaskell
