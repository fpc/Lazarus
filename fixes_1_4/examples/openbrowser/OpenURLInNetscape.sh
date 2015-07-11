#!/usr/bin/env bash

netscape -remote "openurl($1)" || netscape $1 &

