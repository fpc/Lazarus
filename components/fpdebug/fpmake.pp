{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for fpdebug 1.0

   This file was generated on 18/03/2024
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
    P.Version:='1.0.0-0';

    P.Directory:=ADirectory;

    P.Author:='Lazarus Team';
    P.License:='All Files except those listed below: GPL'#13#10''#13#10'File(s) with other licenses (see also header in file(s): '#13#10''#13#10'* macho.pas '#13#10'  This file contains Original Code and/or Modifications of Original Code  as defined in and that are subject to the Apple Public Source License Version 2.0 (the ''License''). You may not use this file except in compliance with the License. Please obtain a copy of the License at   http://www.opensource.apple.com/apsl/ and read it before using this  file.'#13#10''#13#10'  (Any modifications/translations of this file are from duby)';
    P.Description:='A set of helperclasses for implementing a debugger.'#13#10''#13#10'Based on:'#13#10'1) FPDebug by Marc Weustink '#13#10'2) Duby by Dmitry Boyarintsev'#13#10''#13#10'Extended by Martin Friebe';

    D := P.Dependencies.Add('lcl');
    D := P.Dependencies.Add('fcl-net');
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
    P.Options.Add('-vm6058,3057');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCLWidgetType)');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('fpdebug.pas');
    t.Dependencies.AddUnit('fpdbgclasses');
    t.Dependencies.AddUnit('fpdbgdisasx86');
    t.Dependencies.AddUnit('fpdbgdwarf');
    t.Dependencies.AddUnit('fpdbgdwarfconst');
    t.Dependencies.AddUnit('fpdbgloader');
    t.Dependencies.AddUnit('fpdbgpetypes');
    t.Dependencies.AddUnit('fpdbgsymbols');
    t.Dependencies.AddUnit('fpdbgutil');
    t.Dependencies.AddUnit('fpdbgwinextra');
    t.Dependencies.AddUnit('fpimgreaderwinpe');
    t.Dependencies.AddUnit('fpimgreaderelf');
    t.Dependencies.AddUnit('fpimgreaderelftypes');
    t.Dependencies.AddUnit('fpimgreaderbase');
    t.Dependencies.AddUnit('fppascalparser');
    t.Dependencies.AddUnit('macho');
    t.Dependencies.AddUnit('fpimgreadermachofile');
    t.Dependencies.AddUnit('fpimgreadermacho');
    t.Dependencies.AddUnit('fppascalbuilder');
    t.Dependencies.AddUnit('fpdbginfo');
    t.Dependencies.AddUnit('fpdbgwinclasses');
    t.Dependencies.AddUnit('fpdbgdarwinclasses');
    t.Dependencies.AddUnit('fpdmemorytools');
    t.Dependencies.AddUnit('fperrormessages');
    t.Dependencies.AddUnit('fpdbgcontroller');
    t.Dependencies.AddUnit('fpdbgdwarfverboseprinter');
    t.Dependencies.AddUnit('fpdbgdwarfdataclasses');
    t.Dependencies.AddUnit('fpdbgdwarffreepascal');
    t.Dependencies.AddUnit('fpdbgsymtablecontext');
    t.Dependencies.AddUnit('fpdbgsymtable');
    t.Dependencies.AddUnit('fpdbglinuxclasses');
    t.Dependencies.AddUnit('fpdbglinuxextra');
    t.Dependencies.AddUnit('fpdbgavrclasses');
    t.Dependencies.AddUnit('fpdbgdisasavr');
    t.Dependencies.AddUnit('fpdbgrsp');
    t.Dependencies.AddUnit('fpdbgcommon');
    t.Dependencies.AddUnit('fpimgreaderwinpetypes');
    t.Dependencies.AddUnit('fpdbghardcodedfreepascalinfo');
    t.Dependencies.AddUnit('fpdbgcallcontextinfo');
    t.Dependencies.AddUnit('fpwatchresultdata');
    t.Dependencies.AddUnit('fpdbgdwarfcfi');
    t.Dependencies.AddUnit('fpdbgcpux86');
    t.Dependencies.AddUnit('fpdbgrspclasses');
    t.Dependencies.AddUnit('fpdbgdisasxtensa');
    t.Dependencies.AddUnit('fpdbgxtensaclasses');

    T:=P.Targets.AddUnit('fpdbgclasses.pp');
    T:=P.Targets.AddUnit('fpdbgdisasx86.pp');
    T:=P.Targets.AddUnit('fpdbgdwarf.pas');
    T:=P.Targets.AddUnit('fpdbgdwarfconst.pas');
    T:=P.Targets.AddUnit('fpdbgloader.pp');
    T:=P.Targets.AddUnit('fpdbgpetypes.pp');
    T:=P.Targets.AddUnit('fpdbgsymbols.pas');
    T:=P.Targets.AddUnit('fpdbgutil.pp');
    P.Targets.AddImplicitUnit('fpdbgwinextra.pp');
    T:=P.Targets.AddUnit('fpimgreaderwinpe.pas');
    T:=P.Targets.AddUnit('fpimgreaderelf.pas');
    T:=P.Targets.AddUnit('fpimgreaderelftypes.pas');
    T:=P.Targets.AddUnit('fpimgreaderbase.pas');
    T:=P.Targets.AddUnit('fppascalparser.pas');
    T:=P.Targets.AddUnit('macho.pas');
    T:=P.Targets.AddUnit('fpimgreadermachofile.pas');
    T:=P.Targets.AddUnit('fpimgreadermacho.pas');
    T:=P.Targets.AddUnit('fppascalbuilder.pas');
    T:=P.Targets.AddUnit('fpdbginfo.pas');
    P.Targets.AddImplicitUnit('fpdbgwinclasses.pas');
    P.Targets.AddImplicitUnit('fpdbgdarwinclasses.pas');
    T:=P.Targets.AddUnit('fpdmemorytools.pas');
    T:=P.Targets.AddUnit('fperrormessages.pas');
    T:=P.Targets.AddUnit('fpdbgcontroller.pas');
    T:=P.Targets.AddUnit('fpdbgdwarfverboseprinter.pas');
    T:=P.Targets.AddUnit('fpdbgdwarfdataclasses.pas');
    T:=P.Targets.AddUnit('fpdbgdwarffreepascal.pas');
    T:=P.Targets.AddUnit('fpdbgsymtablecontext.pas');
    T:=P.Targets.AddUnit('fpdbgsymtable.pas');
    P.Targets.AddImplicitUnit('fpdbglinuxclasses.pas');
    P.Targets.AddImplicitUnit('fpdbglinuxextra.pas');
    T:=P.Targets.AddUnit('fpdbgavrclasses.pas');
    T:=P.Targets.AddUnit('fpdbgdisasavr.pp');
    T:=P.Targets.AddUnit('fpdbgrsp.pas');
    T:=P.Targets.AddUnit('fpdbgcommon.pas');
    T:=P.Targets.AddUnit('fpimgreaderwinpetypes.pas');
    T:=P.Targets.AddUnit('fpdbghardcodedfreepascalinfo.pas');
    T:=P.Targets.AddUnit('fpdbgcallcontextinfo.pas');
    T:=P.Targets.AddUnit('fpwatchresultdata.pas');
    T:=P.Targets.AddUnit('fpdbgdwarfcfi.pas');
    T:=P.Targets.AddUnit('fpdbgcpux86.pas');
    T:=P.Targets.AddUnit('fpdbgrspclasses.pas');
    T:=P.Targets.AddUnit('fpdbgdisasxtensa.pas');
    T:=P.Targets.AddUnit('fpdbgxtensaclasses.pas');

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
