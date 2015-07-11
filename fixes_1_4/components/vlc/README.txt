
This component is a video player based on the VLC libraries.
You need the VLC library headers in FPC, so this will only compile with FPC
2.7.1 and higher.

The component works on Windows and Linux with GTK2. 
It should work with QT, but this is untested.

The test directory contains a sample project that shows minimal use of the
component.

There are actually 2 components:
TLCLVLCPLayer
and 
TVLCMediaListPlayer
The former plays 1 video file. The second can be used to create a playlist
and uses a TLCLVLCPLayer to play the media.

