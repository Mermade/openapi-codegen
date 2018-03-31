#!/bin/sh

set -e

lua -v

echo lua
cd out/lua
luac *.lua
cd ../..

