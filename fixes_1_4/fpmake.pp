{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
{$define allpackages}
{$define no_parent}
program fpmake;

uses fpmkunit, sysutils, Classes;
{$endif ALLPACKAGES}
(*

The include files are generated with the following commands:

rm fpmake_proc.inc fpmake_add.inc ; /bin/ls -1 */fpmake.pp| while read file; do cleanedname=`dirname $file | sed -e 's+-+_+g'` ; if ! `grep -i "^procedure add_$cleanedname" $file >/dev/null` ; then printf 'procedure add_%s;\nbegin\n  with Installer do\n{$include %s}\nend;\n\n' $cleanedname $file >> fpmake_proc.inc; else printf '{$include %s}\n\n' $file >> fpmake_proc.inc; fi; echo "  add_$cleanedname;" >> fpmake_add.inc; done

*)

{$include fpmake_proc.inc}

procedure add_packages(const ADirectory: string);

begin
{$include fpmake_add.inc}
end;

{$ifdef no_parent}
begin
  add_packages('');
  Installer.Run;
end.
{$endif no_parent}

