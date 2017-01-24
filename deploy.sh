#!/usr/bin/env bash
aws s3 sync _site s3://joh-web-1 --delete
