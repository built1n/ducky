#!/bin/sh
build/unix.bin -c $1
build/unix.bin -t a.out
cc a.c -Wall -lm -g -Og
