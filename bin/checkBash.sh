#!/bin/sh

set -e

echo bash
cd out/bash
bash -n *.sh
cd ../..
