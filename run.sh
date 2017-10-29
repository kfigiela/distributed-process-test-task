#!/bin/sh

stack exec -- distributed-process-test-exe --host 127.0.0.1 --port 10000 --with-seed 1 --multicast-discovery --peers-file peers/master.txt 2>&1 | sed "s/^/1 | /" &
stack exec -- distributed-process-test-exe --host 127.0.0.1 --port 10001 --with-seed 2 --multicast-discovery --peers-file peers/1.txt 2>&1 | sed "s/^/2 | /"&
stack exec -- distributed-process-test-exe --host 127.0.0.1 --port 10002 --with-seed 3 --multicast-discovery --peers-file peers/1.txt 2>&1 | sed "s/^/3 | /"&
stack exec -- distributed-process-test-exe --host 127.0.0.1 --port 10003 --with-seed 4 --multicast-discovery --peers-file peers/1.txt 2>&1 | sed "s/^/4 | /"&
stack exec -- distributed-process-test-exe --host 127.0.0.1 --port 10004 --with-seed 5 --multicast-discovery --peers-file peers/1.txt 2>&1 | sed "s/^/5 | /"&
stack exec -- distributed-process-test-exe --host 127.0.0.1 --port 10005 --with-seed 6 --multicast-discovery --peers-file peers/1.txt 2>&1 | sed "s/^/6 | /"&


wait