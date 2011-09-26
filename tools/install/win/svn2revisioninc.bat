%SVN% info %1 | %windir%\system32\find.exe "Last Changed Rev:" > svninfo.txt

set /P SVNINFO=<svninfo.txt

echo // Created by Svn2RevisionInc> %2
echo const RevisionStr = '%SVNINFO:~18%';>> %2


