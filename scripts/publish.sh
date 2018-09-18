#!/bin/sh
PRG="$0"
# Need this for relative symlinks.
while [ -h "$PRG" ] ; do
    ls=`ls -ld "$PRG"`
    link=`expr "$ls" : '.*-> \(.*\)$'`
    if expr "$link" : '/.*' > /dev/null; then
        PRG="$link"
    else
        PRG=`dirname "$PRG"`"/$link"
    fi
done

ROOT=`dirname "$PRG"`
ROOT=`cd "$ROOT/..";pwd`

cd "$ROOT"

if [ -z "$HACKAGE_USER" -o -z "$HACKAGE_PASS" ]; then
  echo "HACKAGE_USER or HACKAGE_PASS not set"
  exit 1
fi

update=`git diff-index --name-status HEAD | wc -l`

if [ "$update" -gt 0 ]; then
  echo "git has uncommitted modifications"
  exit 1
fi


stack haddock

pkg=.

stack sdist $pkg && stack upload $pkg
hup docboth -u $HACKAGE_USER -p $HACKAGE_PASS

