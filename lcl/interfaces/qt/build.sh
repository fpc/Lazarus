#!/bin/bash

cd src

g++ -c -I../include lzqt.cpp
g++ -c -I../include qtengine.cpp
g++ -c -I../include lzwidget.cpp
g++ -c -I../include lpushbt.cpp

cd ../include
moc -o lwidget.cpp lzwidget.h
moc -o pushbt.cpp lpushbt.h
g++ -c pushbt.cpp
g++ -c lwidget.cpp

cd ../src
g++ -shared -I../include -fpic -o liblzqt.so lzqt.o qtengine.o lzwidget.o lpushbt.o ../include/pushbt.o ../include/lwidget.o -lqt


cp liblzqt.so ../lib
cp -f liblzqt.so /lib
rm -rf *.o
rm -rf *.so

#cd ../test
#python testbuild.py
