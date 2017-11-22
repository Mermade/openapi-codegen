#!/bin/sh
find templates -xdev -type f | cut -d "/" -f 2 | sort | uniq -c | sort -n
