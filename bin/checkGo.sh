#!/bin/sh

set -e

go get golang.org/x/net/context
go get golang.org/x/oauth2
go get github.com/gorilla/mux

echo go
cd out/go
go build .
cd ..

echo go-server
cd out/go-server
go build .
cd ..

