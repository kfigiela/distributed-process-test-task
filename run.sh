#!/bin/sh
# COMMON=--multicast-discovery
COMMON="--connect-for 0"

stack exec -- dptt --host 127.0.0.1 --port 10000 --with-seed 1 $COMMON --peers-file peers/master.txt 2>&1 | sed "s/^/1 | /" &
sleep 1
stack exec -- dptt --host 127.0.0.1 --port 10001 --with-seed 2 $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/2 | /"&
sleep 1
stack exec -- dptt --host 127.0.0.1 --port 10002 --with-seed 3 $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/3 | /"&
sleep 1
stack exec -- dptt --host 127.0.0.1 --port 10003 --with-seed 4 $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/4 | /"&
sleep 1
stack exec -- dptt --host 127.0.0.1 --port 10004 --with-seed 5 $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/5 | /"&
sleep 1
stack exec -- dptt --host 127.0.0.1 --port 10005 --with-seed 6 $COMMON --peers-file peers/1.txt 2>&1 | sed "s/^/6 | /"&


wait