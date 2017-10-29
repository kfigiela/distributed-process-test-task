#!/bin/sh

stack exec -- distributed-process-test-exe --host 127.0.0.1 --port 10000 2>&1 | sed "s/^/1 | /" &
stack exec -- distributed-process-test-exe --host 127.0.0.1 --port 10001 2>&1 | sed "s/^/2 | /"&
stack exec -- distributed-process-test-exe --host 127.0.0.1 --port 10002 2>&1 | sed "s/^/3 | /"&
stack exec -- distributed-process-test-exe --host 127.0.0.1 --port 10003 2>&1 | sed "s/^/4 | /"&
stack exec -- distributed-process-test-exe --host 127.0.0.1 --port 10004 2>&1 | sed "s/^/5 | /"&
stack exec -- distributed-process-test-exe --host 127.0.0.1 --port 10005 2>&1 | sed "s/^/6 | /"&


wait