The default configuration of JCF is stored in the resource file
jcfsettings.res, so for changing we need to modify that file with 
a resource editor or rebuild with

windres -i jcfsettings.rc -o jcfsettings.res

and force a recompilation deleting the files in folder lib and subfolders of lib.



