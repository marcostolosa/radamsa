#!/bin/sh

set -e

test -f tmp/out-* && rm tmp/out-*

$@ -H sha256 -o tmp/out-%h rad/*

FILE=$(echo tmp/out-*)

# sha256sum command may be missing or called sha256 on soma machines, so use owl
bin/ol -t "(let
   ((sha (sha256 (file->list \"$FILE\"))))
   (equal? sha
      ((string->regex \"s/tmp\\\/out-//\") \"$FILE\")))"

