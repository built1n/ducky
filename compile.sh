#!/bin/sh
build/unix.bin -c $1
if [[ $? != 0 ]]
then
    exit
fi
build/unix.bin -t a.out
if [[ $? != 0 ]]
then
    exit
fi
cc a.c -Wall -lm -g -Og
