#!/bin/bash


zcc +zx81 -O3  -L/usr/local/share/z88dk/lib  -create-app spaceShooter.c -o spaceShooter
## for some reason the P is always uppercase?!
mv spaceShooter.P spaceShooter.p
