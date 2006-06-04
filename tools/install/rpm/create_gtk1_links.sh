#!/usr/bin/env bash
# Usage: create_gtk1_links.sh [buildroot]

buildroot=$1
mainlibpath="/usr/lib"
libpaths="$mainlibpath /opt/gnome/lib /usr/local/lib"
libs="glib-1.2.so.0 gdk-1.2.so.0 gtk-1.2.so.0 gdk_pixbuf.so.2"

#echo "create_gtk1_links.sh running for $buildroot ..."
for lib in $libs; do
    # create libname without version number
    ShortLib=$(echo $lib | sed -e 's/\.[0-9]\+$//g' -e 's/-.*.so/.so/g')
    #echo "searching lib$ShortLib ..."
    # search lib in all paths
    ShortLibFile=""
    for Path in $libpaths; do
	if [ -z "$ShortLibFile" ]; then
	    File="$buildroot$Path/lib$ShortLib"
	    if [ -f $File ]; then
		ShortLibFile=$File
	    fi
	fi
    done
    #echo "Found: $ShortLibFile"
    if [ -z "$ShortLibFile" ]; then
	# not found - search lib with number and create a link
	for Path in $libpaths; do
	    if [ -z "$ShortLibFile" ]; then
		File="$buildroot$Path/lib$lib"
		#echo "trying "$File
		if [ -f $File ]; then
		    ShortLibFile=$mainlibpath/lib$ShortLib
		    #echo ln -s $Path/lib$lib $ShortLibFile
		    ln -s $Path/lib$lib $ShortLibFile
		fi
	    fi
	done
    fi
done

