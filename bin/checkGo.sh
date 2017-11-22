#!/bin/sh

set -e

echo go
cd out/go
go build .
cd ..

echo go-server
cd out/go-server
go build .
cd ..

