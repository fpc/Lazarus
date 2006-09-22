
The lazarus TJPEGImage is in lazjpeg.pas

It uses the pasjpeg and fpimage libs provided by FreePascal. See there for in
more detailed jpeg handling.


JPEG (pronounced "jay-peg") is a standardized familly of algorithms for
compression of continous tone still images. Most JPEG processes are lossy,
the output image is not exactly identical to the input image. However, on
typical photographic images, very good compression levels can be obtained
with no visible change, and remarkably high compression levels are possible
if you can tolerate a low-quality image. The Independent JPEG Group (IJG) has
created a free, portable C library for JPEG compression and decompression of
JPEG images which has been ported to FreePascal.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We used a simple naming schema to help non expert working with this package.
The support type name and the support unit name are :

T$(most_common_extension)Image is in laz$(most_common_extension).pas

The lazarus TPNGImage is in lazpng.pas
The lazarus TPNMImage is in lazpnm.pas
The lazarus TJPGImage is in lazjpg.pas
The lazarus TBMPImage is in lazbmp.pas
The lazarus TTGAImage is in laztga.pas
The lazarus TXPMImage is in lazxpm.pas

This package uses the fpimage libs provided by FreePascal in the FCL/Image.
See there for in more detailed jpeg handling.

Please report bugs to "Mazen NEIFER" <mazen@freepascal.org>
