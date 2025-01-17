#!/usr/bin/env bash

sudo rm -rf /Developer/lazarus /Applications/Lazarus.app /etc/lazarus /usr/local/bin/lazbuild
sudo pkgutil --forget org.freepascal.lazarus.www

# end.

