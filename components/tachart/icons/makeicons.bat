@dir /b /on *.png >pnglist.tmp
@..\..\..\tools\lazres.exe ..\tagraph.res @pnglist.tmp
@del pnglist.tmp >nul
