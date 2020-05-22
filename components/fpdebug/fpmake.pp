{
   File generated automatically by Lazarus Package Manager
   Created with the Fppkgpackagemanager package installed

   fpmake.pp for fpdebug 0.9

   This file was generated on 22-05-20
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_fpdebug(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('fpdebug');
    P.Version:='0.9.0-0';

    P.Directory:=ADirectory;

    P.Author:='Lazarus Team';
    P.License:='All Files except those listed below: GPL'#10''#10'File(s) with other licenses (see also header in file(s): '#10''#10'* macho.pas '#10'  This file contains Original Code and/or Modifications of Original Code  as defined in and that are subject to the Apple Public Source License Version 2.0 (the ''License''). You may not use this file except in compliance with the License. Please obtain a copy of the License at   http://www.opensource.apple.com/apsl/ and read it before using this  file.'#10''#10'  (Any modifications/translations of this file are from duby)';
    P.Description:='A set of helperclasses for implementing a debugger.'#10''#10'Based on:'#10'1) FPDebug by Marc Weustink '#10'2) Duby by Dmitry Boyarintsev'#10''#10'Extended by Martin Friebe';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('debuggerintf');
    D := P.Dependencies.Add('lclbase');
    D := P.Dependencies.Add('fcl');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-venibq');
    P.Options.Add('-vw-h-');
    P.Options.Add('-vm3057');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('fpdebug.pas');
    D := T.Dependencies.AddUnit('FpDbgClasses');
    D := T.Dependencies.AddUnit('FpDbgDisasX86');
    D := T.Dependencies.AddUnit('FpDbgDwarf');
    D := T.Dependencies.AddUnit('FpDbgDwarfConst');
    D := T.Dependencies.AddUnit('FpDbgLoader');
    D := T.Dependencies.AddUnit('FpDbgPETypes');
    D := T.Dependencies.AddUnit('FpDbgSymbols');
    D := T.Dependencies.AddUnit('FpDbgUtil');
    D := T.Dependencies.AddUnit('FpDbgWinExtra');
    D := T.Dependencies.AddUnit('FpImgReaderWinPE');
    D := T.Dependencies.AddUnit('FpImgReaderElf');
    D := T.Dependencies.AddUnit('FpImgReaderElfTypes');
    D := T.Dependencies.AddUnit('FpImgReaderBase');
    D := T.Dependencies.AddUnit('FpPascalParser');
    D := T.Dependencies.AddUnit('macho');
    D := T.Dependencies.AddUnit('FpImgReaderMachoFile');
    D := T.Dependencies.AddUnit('FpImgReaderMacho');
    D := T.Dependencies.AddUnit('FpPascalBuilder');
    D := T.Dependencies.AddUnit('FpDbgInfo');
    D := T.Dependencies.AddUnit('FpDbgWinClasses');
    D := T.Dependencies.AddUnit('FpDbgDarwinClasses');
    D := T.Dependencies.AddUnit('FpdMemoryTools');
    D := T.Dependencies.AddUnit('FpErrorMessages');
    D := T.Dependencies.AddUnit('FPDbgController');
    D := T.Dependencies.AddUnit('FpDbgDwarfVerbosePrinter');
    D := T.Dependencies.AddUnit('FpDbgDwarfDataClasses');
    D := T.Dependencies.AddUnit('FpDbgDwarfFreePascal');
    D := T.Dependencies.AddUnit('fpDbgSymTableContext');
    D := T.Dependencies.AddUnit('fpDbgSymTable');
    D := T.Dependencies.AddUnit('FpDbgLinuxClasses');
    D := T.Dependencies.AddUnit('FpDbgLinuxExtra');
    D := T.Dependencies.AddUnit('FpDbgAvrClasses');
    D := T.Dependencies.AddUnit('FpDbgDisasAvr');
    D := T.Dependencies.AddUnit('FpDbgRsp');
    D := T.Dependencies.AddUnit('FpDbgCommon');
    T := P.Targets.AddImplicitUnit('fpdbgclasses.pp');
    T := P.Targets.AddImplicitUnit('fpdbgdisasx86.pp');
    T := P.Targets.AddImplicitUnit('fpdbgdwarf.pas');
    T := P.Targets.AddImplicitUnit('fpdbgdwarfconst.pas');
    T := P.Targets.AddImplicitUnit('fpdbgloader.pp');
    T := P.Targets.AddImplicitUnit('fpdbgpetypes.pp');
    T := P.Targets.AddImplicitUnit('fpdbgsymbols.pas');
    T := P.Targets.AddImplicitUnit('fpdbgutil.pp');
    T := P.Targets.AddImplicitUnit('fpdbgwinextra.pp');
    T := P.Targets.AddImplicitUnit('fpimgreaderwinpe.pas');
    T := P.Targets.AddImplicitUnit('fpimgreaderelf.pas');
    T := P.Targets.AddImplicitUnit('fpimgreaderelftypes.pas');
    T := P.Targets.AddImplicitUnit('fpimgreaderbase.pas');
    T := P.Targets.AddImplicitUnit('fppascalparser.pas');
    T := P.Targets.AddImplicitUnit('macho.pas');
    T := P.Targets.AddImplicitUnit('fpimgreadermachofile.pas');
    T := P.Targets.AddImplicitUnit('fpimgreadermacho.pas');
    T := P.Targets.AddImplicitUnit('fppascalbuilder.pas');
    T := P.Targets.AddImplicitUnit('fpdbginfo.pas');
    T := P.Targets.AddImplicitUnit('fpdbgwinclasses.pas');
    T := P.Targets.AddImplicitUnit('fpdbgdarwinclasses.pas');
    T := P.Targets.AddImplicitUnit('fpdmemorytools.pas');
    T := P.Targets.AddImplicitUnit('fperrormessages.pas');
    T := P.Targets.AddImplicitUnit('fpdbgcontroller.pas');
    T := P.Targets.AddImplicitUnit('fpdbgdwarfverboseprinter.pas');
    T := P.Targets.AddImplicitUnit('fpdbgdwarfdataclasses.pas');
    T := P.Targets.AddImplicitUnit('fpdbgdwarffreepascal.pas');
    T := P.Targets.AddImplicitUnit('fpdbgsymtablecontext.pas');
    T := P.Targets.AddImplicitUnit('fpdbgsymtable.pas');
    T := P.Targets.AddImplicitUnit('fpdbglinuxclasses.pas');
    T := P.Targets.AddImplicitUnit('fpdbglinuxextra.pas');
    T := P.Targets.AddImplicitUnit('fpdbgavrclasses.pas');
    T := P.Targets.AddImplicitUnit('fpdbgdisasavr.pp');
    T := P.Targets.AddImplicitUnit('fpdbgrsp.pas');
    T := P.Targets.AddImplicitUnit('fpdbgcommon.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('fpdebug.compiled');
    P.InstallFiles.Add('fpdebug.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_fpdebug('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
