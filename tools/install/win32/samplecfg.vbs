Option explicit

Dim WSHShell
Dim FS
Dim InFileName 
Dim OutFileName 
Dim PPDir
Dim InFile
Dim OutFile
Dim AllText
Dim NewText

Set WSHShell = WScript.CreateObject("WScript.Shell")

InFileName = "samplefpc.cfg"
OutFileName = WSHShell.ExpandEnvironmentStrings("%BuildDir%")& "\pp\bin\i386-win32\fpc.cfg"
PPDir = WSHShell.ExpandEnvironmentStrings("%BuildDir%") & "\pp"

Set fs = CreateObject("Scripting.FileSystemObject")
Set InFile = fs.OpenTextFile(InFileName)
AllText = InFile.ReadAll
InFile.Close

NewText = Replace(AllText,"$1",PPDIR,1, -1,0)

Set OutFile = fs.CreateTextFile(OutFileName, True)
OutFile.WriteLine(NewText)
OutFile.Close
