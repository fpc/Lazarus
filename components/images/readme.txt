WARNING  WARNING  WARNING  WARNING  WARNING

This package is broken. Read further.

The pasjpeg code in the current fpc sources have a bug.
If you want jpeg, then you must use an older fpc and the jpeg from lazarus-ccr
on sourceforge. Not this package.
However, if you only need interlaced JPEG, you can use this package.

The Targa loder is not fully functional.

-------------------------------------------------------------------------------

We used a simple naming schema to help non expert working with this package.
The support type name and the support unit name are :

T$(most_common_extension)Image is in laz$(most_common_extension).pas

The lazarus TPNGImage is in lazpng.pas
The lazarus TPNMImage is in lazpnm.pas
The lazarus TJPGImage is in lazjpg.pas
The lazarus TBMPImage is in lazbmp.pas
The lazarus TTGAImage is in laztga.pas
The lazarus TXPMImage is in lazxpm.pas

This package uses the fpimage libs provided by FreePascal in the FCL/Image. See there for in
more detailed jpeg handling.

Please report bugs to "Mazen NEIFER" <mazen@freepascal.org>