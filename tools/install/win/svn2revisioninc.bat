%SVN% info %1 | %windir%\system32\find "Last Changed Rev:" > svninfo.txt

set /P SVNINFO=<svninfo.txt

echo const RevisionStr = '%SVNINFO:~18%'; > %2


