#!/bin/sh

set -e

find configs -type f | sed s/.json//g | sed s/configs.//g | xargs -n 1 node dist/cg -v
