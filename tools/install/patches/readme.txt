This directory contains patches for fpc. For more information about the reason
for these patches, see
http://wiki.lazarus.freepascal.org/Useful_changes_not_in_the_fixes_branch


The file fpc-fixes_2_2.patch is a patch for the fixes_2_2 branch
(http://svn.freepascal.org/svn/fpc/branches/fixes_2_2). 

Details about this patch can be found at 
http://wiki.lazarus.freepascal.org/Useful_changes_not_in_the_fixes_branch#Description_of_the_fixes_2_2_patch

The file windres-2.2.2.patch contains a patch for calling windres in the fpc 2.2.2 compiler, so that the compiler can compile projects with .rc files in directories with spaces. Basically this is a backport of r12545.

