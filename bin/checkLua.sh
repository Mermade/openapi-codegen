#!/bin/sh

set -e

echo lua
cd out/lua
luac *.lua
cd ../..

