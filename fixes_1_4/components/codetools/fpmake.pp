{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for CodeTools 1.0.1

   This file was generated on 02-01-2015
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_CodeTools(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPAckage('codetools');
    P.Version:='1.0.1';

    P.Directory:=ADirectory;

    P.Flags.Add('LazarusDsgnPkg');

    P.Dependencies.Add('lazutils');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Sci');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vh-');
    P.Options.Add('-vewnibq');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('codetools.pas');
    t.Dependencies.AddUnit('basiccodetools');
    t.Dependencies.AddUnit('cachecodetools');
    t.Dependencies.AddUnit('ccodeparsertool');
    t.Dependencies.AddUnit('codeatom');
    t.Dependencies.AddUnit('codebeautifier');
    t.Dependencies.AddUnit('codecache');
    t.Dependencies.AddUnit('codecompletiontool');
    t.Dependencies.AddUnit('codegraph');
    t.Dependencies.AddUnit('codeindex');
    t.Dependencies.AddUnit('codetemplatestool');
    t.Dependencies.AddUnit('codetoolmanager');
    t.Dependencies.AddUnit('codetoolmemmanager');
    t.Dependencies.AddUnit('codetoolsconfig');
    t.Dependencies.AddUnit('codetoolsstrconsts');
    t.Dependencies.AddUnit('codetoolsstructs');
    t.Dependencies.AddUnit('codetree');
    t.Dependencies.AddUnit('customcodetool');
    t.Dependencies.AddUnit('definetemplates');
    t.Dependencies.AddUnit('directivestree');
    t.Dependencies.AddUnit('directorycacher');
    t.Dependencies.AddUnit('eventcodetool');
    t.Dependencies.AddUnit('expreval');
    t.Dependencies.AddUnit('extractproctool');
    t.Dependencies.AddUnit('fileprocs');
    t.Dependencies.AddUnit('finddeclarationcache');
    t.Dependencies.AddUnit('finddeclarationtool');
    t.Dependencies.AddUnit('findoverloads');
    t.Dependencies.AddUnit('h2pastool');
    t.Dependencies.AddUnit('identcompletiontool');
    t.Dependencies.AddUnit('keywordfunclists');
    t.Dependencies.AddUnit('lfmtrees');
    t.Dependencies.AddUnit('linkscanner');
    t.Dependencies.AddUnit('methodjumptool');
    t.Dependencies.AddUnit('multikeywordlisttool');
    t.Dependencies.AddUnit('nonpascalcodetools');
    t.Dependencies.AddUnit('pascalparsertool');
    t.Dependencies.AddUnit('pascalreadertool');
    t.Dependencies.AddUnit('ppucodetools');
    t.Dependencies.AddUnit('ppugraph');
    t.Dependencies.AddUnit('ppuparser');
    t.Dependencies.AddUnit('resourcecodetool');
    t.Dependencies.AddUnit('sourcechanger');
    t.Dependencies.AddUnit('sourcelog');
    t.Dependencies.AddUnit('stdcodetools');
    t.Dependencies.AddUnit('otheridentifiertree');
    t.Dependencies.AddUnit('codetoolscfgscript');
    t.Dependencies.AddUnit('ctxmlfixfragment');
    t.Dependencies.AddUnit('ctunitgraph');
    t.Dependencies.AddUnit('changedeclarationtool');
    t.Dependencies.AddUnit('codetoolsfpcmsgs');
    t.Dependencies.AddUnit('unitdictionary');
    t.Dependencies.AddUnit('ctloadlaz');
    t.Dependencies.AddUnit('ctunitgroupgraph');
    t.Dependencies.AddUnit('codecompletiontemplater');
    t.Dependencies.AddUnit('codetoolgdbtracer');

    T:=P.Targets.AddUnit('basiccodetools.pas');
    T:=P.Targets.AddUnit('cachecodetools.pas');
    T:=P.Targets.AddUnit('ccodeparsertool.pas');
    T:=P.Targets.AddUnit('codeatom.pas');
    T:=P.Targets.AddUnit('codebeautifier.pas');
    T:=P.Targets.AddUnit('codecache.pas');
    T:=P.Targets.AddUnit('codecompletiontool.pas');
    T:=P.Targets.AddUnit('codegraph.pas');
    T:=P.Targets.AddUnit('codeindex.pas');
    T:=P.Targets.AddUnit('codetemplatestool.pas');
    T:=P.Targets.AddUnit('codetoolmanager.pas');
    T:=P.Targets.AddUnit('codetoolmemmanager.pas');
    T:=P.Targets.AddUnit('codetoolsconfig.pas');
    T:=P.Targets.AddUnit('codetoolsstrconsts.pas');
    T:=P.Targets.AddUnit('codetoolsstructs.pas');
    T:=P.Targets.AddUnit('codetree.pas');
    T:=P.Targets.AddUnit('customcodetool.pas');
    T:=P.Targets.AddUnit('definetemplates.pas');
    T:=P.Targets.AddUnit('directivestree.pas');
    T:=P.Targets.AddUnit('directorycacher.pas');
    T:=P.Targets.AddUnit('eventcodetool.pas');
    T:=P.Targets.AddUnit('expreval.pas');
    T:=P.Targets.AddUnit('extractproctool.pas');
    T:=P.Targets.AddUnit('fileprocs.pas');
    T:=P.Targets.AddUnit('finddeclarationcache.pas');
    T:=P.Targets.AddUnit('finddeclarationtool.pas');
    T:=P.Targets.AddUnit('findoverloads.pas');
    T:=P.Targets.AddUnit('h2pastool.pas');
    T:=P.Targets.AddUnit('identcompletiontool.pas');
    T:=P.Targets.AddUnit('keywordfunclists.pas');
    T:=P.Targets.AddUnit('lfmtrees.pas');
    T:=P.Targets.AddUnit('linkscanner.pas');
    P.Sources.AddSrc('memcheck.pas');
    T:=P.Targets.AddUnit('methodjumptool.pas');
    T:=P.Targets.AddUnit('multikeywordlisttool.pas');
    T:=P.Targets.AddUnit('nonpascalcodetools.pas');
    T:=P.Targets.AddUnit('pascalparsertool.pas');
    T:=P.Targets.AddUnit('pascalreadertool.pas');
    T:=P.Targets.AddUnit('ppucodetools.pas');
    T:=P.Targets.AddUnit('ppugraph.pas');
    T:=P.Targets.AddUnit('ppuparser.pas');
    T:=P.Targets.AddUnit('resourcecodetool.pas');
    T:=P.Targets.AddUnit('sourcechanger.pas');
    T:=P.Targets.AddUnit('sourcelog.pas');
    T:=P.Targets.AddUnit('stdcodetools.pas');
    T:=P.Targets.AddUnit('otheridentifiertree.pas');
    T:=P.Targets.AddUnit('codetoolscfgscript.pas');
    T:=P.Targets.AddUnit('ctxmlfixfragment.pas');
    T:=P.Targets.AddUnit('ctunitgraph.pas');
    T:=P.Targets.AddUnit('changedeclarationtool.pas');
    T:=P.Targets.AddUnit('codetoolsfpcmsgs.pas');
    T:=P.Targets.AddUnit('unitdictionary.pas');
    T:=P.Targets.AddUnit('ctloadlaz.pas');
    T:=P.Targets.AddUnit('ctunitgroupgraph.pas');
    T:=P.Targets.AddUnit('codecompletiontemplater.pas');
    T:=P.Targets.AddUnit('codetoolgdbtracer.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('CodeTools.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_CodeTools('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
