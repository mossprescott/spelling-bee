#! /bin/bash
set -ex

elm make src/Main.elm --output=spellingbee.js

cp index.html spellingbee.js ../spellingbee-server/spellingbee/templates/
