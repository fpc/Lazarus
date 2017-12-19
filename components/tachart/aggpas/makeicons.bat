@dir /b /on *.png >pnglist.tmp
@..\..\..\tools\lazres.exe taaggpas.res @pnglist.tmp
@del pnglist.tmp >nul
