{$mode objfpc}{$H+}
{$define allpackages}
program fpmake;

uses fpmkunit, sysutils, Classes;

Var
  TBuild,T : TTarget;
  PBuild,P : TPackage;
  D : TDependency;
  I : Integer;

(*

The include files are generated with the following commands:

rm fpmake_proc.inc fpmake_add.inc ; /bin/ls -1 */fpmake.pp| while read file; do cleanedname=`dirname $file | sed -e 's+-+_+g'` ; if ! `grep -i "^procedure add_$cleanedname" $file >/dev/null` ; then printf 'procedure add_%s;\nbegin\n  with Installer do\n{$include %s}\nend;\n\n' $cleanedname $file >> fpmake_proc.inc; else printf '{$include %s}\n\n' $file >> fpmake_proc.inc; fi; echo "  add_$cleanedname;" >> fpmake_add.inc; done

*)

{$include fpmake_proc.inc}

begin
{$include fpmake_add.inc}

  With Installer do
    begin
      // Create fpc-all package
      PBuild:=AddPackage('lazarus');
      PBuild.Version:='1.1';
      PBuild.Dependencies.Add('lcl');
      PBuild.Dependencies.Add('codetools');
      PBuild.Dependencies.Add('synedit');
      PBuild.Dependencies.Add('ideintf');
      PBuild.Dependencies.Add('debuggerintf');
      PBuild.Dependencies.Add('lazdebuggergdbmi');


      pbuild.SourcePath.Add('ide');


      T := PBuild.Targets.AddProgram('lazarus.pp');
      T.IncludePath.Add('ide/include');
      T.Options.Add('-Fudebugger');
      T.Options.Add('-Fuide/frames');
      T.Options.Add('-Fudebugger/frames');
      T.Options.Add('-Fupackager');
      t.UnitPath.Add('packager/frames');
      T.Options.Add('-Fudesigner');
      T.Options.Add('-Fuconverter');
      T.Options.Add('-Sci');
      PBuild.SupportBuildModes:=[bmOneByOne];
      Run;
    end;
end.
