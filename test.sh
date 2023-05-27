#! /bin/bash
set -ex

PATH="$PATH:$(npm bin)"

elm-test --fuzz 1000
