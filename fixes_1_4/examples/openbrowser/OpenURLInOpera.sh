#!/usr/bin/env bash

opera -remote "openURL($1)" || opera $1 &

