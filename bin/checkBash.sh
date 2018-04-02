#!/bin/sh

set -e

echo bash $BASH_VERSION
cd out/bash
bash -n *.sh
cd ../..
