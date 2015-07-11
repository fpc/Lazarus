#!/usr/bin/env bash
mkdir -p ~/rpm/tmp
for fulldir in BUILD RPMS SOURCES SPECS SRPMS; do
    dir=$(basename "$fulldir")
    mkdir -p ~/rpm/"$dir"
done
cat > ~/.rpmmacros << EOT
%_topdir $(echo ${HOME}/rpm)
%_tmppath $(echo ${HOME}/rpm/tmp)
EOT

