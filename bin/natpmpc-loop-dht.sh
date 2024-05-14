#!/usr/bin/env bash
while true ; do
  date
  natpmpc -a 2 1 udp 60 -g 10.2.0.1 || {
    echo "ERROR with natpmpc command"
  }
  sleep 45
done
