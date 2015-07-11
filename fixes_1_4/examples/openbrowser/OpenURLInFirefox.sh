#!/usr/bin/env bash

firefox -remote "openurl($1)" || firefox $1 &

