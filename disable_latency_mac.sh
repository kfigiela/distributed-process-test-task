#!/bin/bash

## Flush firewall
echo | sudo pfctl -a mop -f -
cat /etc/pf.conf | sudo pfctl -f -
sudo dnctl -f flush