#!/usr/bin/env sh

# https://askubuntu.com/questions/811733/how-can-i-restart-my-wi-fi-connection-from-the-command-line

set -e

nmcli radio wifi off
nmcli radio wifi on
