#!/bin/bash

find . -name '.hg' -prune -o \
       -name '.hgignore' -prune -o \
       -name 'build.sh' -prune -o \
       -name '*~' -prune -o \
       -print | xargs zip zotero-for-restructured-text.xpi
