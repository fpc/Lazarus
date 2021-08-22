%GIT% -C %1 describe --always  > gitinfo.txt

set /P SVNINFO=<gitinfo.txt

echo // Created by Svn2RevisionInc> %2
echo const RevisionStr = '%SVNINFO%';>> %2


