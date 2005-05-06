Hi for all,

I think TSemaphorGrid is quite original and it seems to work fine on both
Windows and Linux OS except "HideCol" under Linux that sometimes show a bit
of text contained in the hidden cells.

All the Info you need for TSemaphorGrid are in the source code.
I've commented the main methods in the abstract and other important steps in the code.
I've registered it in the 'Additional' page of Lazarus IDE components because I use it
instead of TStringGrid and if you want you can change this.

TSemaphorGrid come in a Lazarus Package "SemaphorGridLPK.lpk", just install it in the
Lazarus IDE by Components-->Open package file(.lpk)
Compile and install it
In the ".\example" direcory there is a Lazarus project (.lpi) that use TSemaphorGrid
with the main properties and methods.

ABSTRACT:
SEMAFORO (Semaphor) in Italian Language means Traffic Lights. If Semaphor is
set to true,when TSemaphorGrid detect in a non Fixed Cells a string like
StringGreen or StringYellow or StringRed, it show a colored sign in the
corrispondent cells (shape choosed in SemaphorShape). It can be Case Sensitive
(SemaphorCaseSensitive). If Semaphor is false, nothing happen.

SemaphorGrid is able to store and restore data by indipendent method
LoadFromFileG and SaveToFileG wich manage also accented chars in data and
similar. Data are separeted by CHSEP. LoadFromFileG has autoadjust wich allow
SemaphorGrid to AutosizeColumns. SemaphorGrid, at the moment, is unable to store
setting grid (only Column Hidden and in general ColWidth). With the method
ExportToExcel, SemaphorGrid is able set CHSEP so that the file generated is
MS Excel compatible.

Thanks for the Lazarus Project

Salvatore
