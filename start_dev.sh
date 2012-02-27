#!/bin/sh

export PATH=/mochi/opt/erlang-R14B01/bin:$PATH
export ERL_FLAGS="-smp auto +A 4 +P 131072 -setcookie ZSXGVURNICGBYNJUSNXS"
export ERL_MAX_PORTS="32768"
OS=`uname`
case $OS in
    Linux) IP=`ifconfig eth0 | grep 'inet addr:'| cut -d: -f2 | awk '{ print $1}'`;;
    Darwin) IP=`ifconfig en0 | grep -E 'inet.[0-9]' | awk '{ print $2}'` ;;
    *) IP="Unknown";;
esac
case $IP in
    "") IP=127.0.0.1;;
    "Unknown") IP=127.0.0.1;;
    *) IP=$IP
esac
echo "node host: $IP"
echo "chat_server dev server starting~."
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl \
             -name breath_server_dev@$IP \
             -s reloader \
             -s breath_server