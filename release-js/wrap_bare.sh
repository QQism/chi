#!/bin/sh
# Script is gratefully adapted from https://github.com/tonsky/datascript/blob/master/release-js/wrap_bare.sh

set -e

(cat release-js/wrapper.prefix; cat release-js/chi.bare.js; cat release-js/wrapper.suffix) > release-js/chi.js

echo "Packed release-js/chi.js"
