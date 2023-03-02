dir /b /l /on *.png > files.txt
..\..\..\tools\lazres.exe ..\exampleprojects_images.res @files.txt
del files.txt
