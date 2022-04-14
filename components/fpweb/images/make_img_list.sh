#!/bin/sh

if [ -f ../fpweb_images.res ]; then
  rm ../fpweb_images.res
fi  
../../../tools/lazres ../fpweb_images.res @img_list.txt
