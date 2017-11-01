#!/bin/bash

(cat /etc/pf.conf && echo "dummynet-anchor \"mop\"" && echo "anchor \"mop\"") | sudo pfctl -f -
echo | sudo pfctl -a mop -f -