#!/bin/sh

 stack exec distributed-process-test-exe 127.0.0.1 10000 peers/master.txt 2>&1 | sed "s/^/1 | /" &
 stack exec distributed-process-test-exe 127.0.0.1 10001 peers/1.txt 2>&1 | sed "s/^/2 | /"&
 stack exec distributed-process-test-exe 127.0.0.1 10002 peers/1.txt 2>&1 | sed "s/^/3 | /"&
 stack exec distributed-process-test-exe 127.0.0.1 10003 peers/1.txt 2>&1 | sed "s/^/4 | /"&
 stack exec distributed-process-test-exe 127.0.0.1 10004 peers/1.txt 2>&1 | sed "s/^/5 | /"&
 stack exec distributed-process-test-exe 127.0.0.1 10005 peers/1.txt 2>&1 | sed "s/^/6 | /"&