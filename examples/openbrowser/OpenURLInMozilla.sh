#!/usr/bin/env bash

mozilla -remote "openurl($1)" || mozilla $1 &

