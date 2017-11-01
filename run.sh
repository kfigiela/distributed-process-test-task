#!/bin/sh
# COMMON=--multicast-discovery
COMMON="--connect-for 0 --send-for 60 --wait-for 10"

stack exec -- dptt --host 127.0.0.1 --port 10000 --with-seed 1  $COMMON                          2>&1 | sed "s/^/ 1 | /" &
stack exec -- dptt --host 127.0.0.1 --port 10001 --with-seed 2  $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/ 2 | /"&
stack exec -- dptt --host 127.0.0.1 --port 10002 --with-seed 3  $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/ 3 | /"&
stack exec -- dptt --host 127.0.0.1 --port 10003 --with-seed 4  $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/ 4 | /"&
stack exec -- dptt --host 127.0.0.1 --port 10004 --with-seed 5  $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/ 5 | /"&
stack exec -- dptt --host 127.0.0.1 --port 10005 --with-seed 6  $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/ 6 | /"&
stack exec -- dptt --host 127.0.0.1 --port 10006 --with-seed 7  $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/ 7 | /"&
stack exec -- dptt --host 127.0.0.1 --port 10007 --with-seed 8  $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/ 8 | /"&
stack exec -- dptt --host 127.0.0.1 --port 10008 --with-seed 9  $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/ 9 | /"&
stack exec -- dptt --host 127.0.0.1 --port 10009 --with-seed 10 $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/10 | /"&
# stack exec -- dptt --host 127.0.0.1 --port 10010 --with-seed 11 $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/11 | /"&
# stack exec -- dptt --host 127.0.0.1 --port 10011 --with-seed 12 $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/12 | /"&
# stack exec -- dptt --host 127.0.0.1 --port 10012 --with-seed 13 $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/13 | /"&
# stack exec -- dptt --host 127.0.0.1 --port 10013 --with-seed 14 $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/14 | /"&
# stack exec -- dptt --host 127.0.0.1 --port 10014 --with-seed 15 $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/15 | /"&
# stack exec -- dptt --host 127.0.0.1 --port 10015 --with-seed 16 $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/16 | /"&
# stack exec -- dptt --host 127.0.0.1 --port 10016 --with-seed 17 $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/17 | /"&

wait