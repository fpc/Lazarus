@dir /b /on *.png >pnglist.tmp
@..\..\..\tools\lazres.exe ..\tagraph.lrs @pnglist.tmp
@del pnglist.tmp >nul
