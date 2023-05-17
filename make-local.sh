#! /bin/bash
set -ex

elm make src/Demo/Local.elm --output=spellingbee.js --debug
