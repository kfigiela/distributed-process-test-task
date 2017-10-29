#!/bin/sh

stack exec -- distributed-process-test-exe --host 127.0.0.1 --port 10000 --with-seed 1 2>&1 | sed "s/^/1 | /" &
stack exec -- distributed-process-test-exe --host 127.0.0.1 --port 10001 --with-seed 2 2>&1 | sed "s/^/2 | /"&
stack exec -- distributed-process-test-exe --host 127.0.0.1 --port 10002 --with-seed 3 2>&1 | sed "s/^/3 | /"&
stack exec -- distributed-process-test-exe --host 127.0.0.1 --port 10003 --with-seed 4 2>&1 | sed "s/^/4 | /"&
stack exec -- distributed-process-test-exe --host 127.0.0.1 --port 10004 --with-seed 5 2>&1 | sed "s/^/5 | /"&
stack exec -- distributed-process-test-exe --host 127.0.0.1 --port 10005 --with-seed 6 2>&1 | sed "s/^/6 | /"&


wait