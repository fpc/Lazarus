The package imagesforlazarus.lpk adds LCL support for some extra image formats like tga.

Normal usage is to add it to your project in the project inspector.
You can install it in the IDE. Then the IDE picture open dialogs will be able to load some more file formats.

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
