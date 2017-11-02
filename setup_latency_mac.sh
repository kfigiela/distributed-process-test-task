#!/bin/bash

## Enable firewall
sudo pfctl -e

## Add dummynet-anchor
(cat /etc/pf.conf && echo "dummynet-anchor \"mop\"" && echo "anchor \"mop\"") | sudo pfctl -f -

## Define "network failures"
sudo dnctl pipe 1 config bw 10Mbit/s delay 500
sudo dnctl pipe 2 config bw 10Mbit/s delay 30
sudo dnctl pipe 3 config bw 10Mbit/s delay 200

## Map ports to network failures
(
    echo "dummynet in  quick proto tcp from any to any port 10001 pipe 1"
    echo "dummynet out quick proto tcp from any to any port 10001 pipe 1"
    echo "dummynet in  quick proto tcp from any to any port 10002 pipe 2"
    echo "dummynet out quick proto tcp from any to any port 10002 pipe 2"
    echo "dummynet in  quick proto tcp from any to any port 10003 pipe 3" 
    echo "dummynet out quick proto tcp from any to any port 10003 pipe 3" 
    echo "dummynet in  quick proto tcp from any to any port 10004 pipe 1" 
    echo "dummynet out quick proto tcp from any to any port 10004 pipe 1" 
    echo "dummynet in  quick proto tcp from any to any port 10005 pipe 2" 
    echo "dummynet out quick proto tcp from any to any port 10005 pipe 2" 
    echo "dummynet in  quick proto tcp from any to any port 10006 pipe 3"
    echo "dummynet out quick proto tcp from any to any port 10006 pipe 3"
) | sudo pfctl -a mop -f -
