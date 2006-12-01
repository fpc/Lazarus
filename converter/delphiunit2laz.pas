{  $Id$  }
{
 /***************************************************************************
                          delphiunit2laz.pas
                          ------------------

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    Functions to convert delphi units to lcl units.

}
unit DelphiUnit2Laz;

{$mode objfpc}{$H+}

interface

uses
  // FCL+LCL
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics,
  Dialogs, Buttons, StdCtrls, FileUtil, IniFiles,
  // Components
  SynEdit, CodeAtom, CodeCache, CodeToolManager, DefineTemplates,
  // IDEIntf
  LazIDEIntf, IDEMsgIntf,
  // IDE
  CompilerOptions,
  PackageDefs, Project, DialogProcs, IDEProcs, LazarusIDEStrConsts;

type
  TDelphi2LazarusDialog = class(TForm)
  private
  public
  end;

var
  Delphi2LazarusDialog: TDelphi2LazarusDialog;
  
function CheckDelphiFileExt(const Filename: string): TModalResult;
function CheckFilenameForLCLPaths(const Filename: string): TModalResult;
function ConvertDelphiToLazarusFilename(const DelphiFilename: string;
  RenameLowercase: boolean): string;
function RenameDelphiUnitToLazarusUnit(const DelphiFilename: string;
  RenameDFMFile, RenameLowercase: boolean;
  var LazarusFilename, LFMFilename: string): TModalResult;
function FixMissingUnits(const LazarusUnitFilename: string;
                         IsSubProc: boolean): TModalResult;

// dfm/lfm
function ConvertDFMToLFMFilename(const DFMFilename: string;
  KeepCase: boolean): string;
function FindDFMFileForDelphiUnit(const DelphiFilename: string): string;
function ConvertDFMFileToLFMFile(const DFMFilename: string): TModalResult;
function ConvertDelphiSourceToLazarusSource(const LazarusUnitFilename: string;
  AddLRSCode: boolean): TModalResult;
function LoadUnitAndLFMFile(const UnitFileName: string;
  var UnitCode, LFMCode: TCodeBuffer; LFMMustExist: boolean): TModalResult;
function ConvertLFMtoLRSfile(const LFMFilename: string): TModalResult;

// projects
function CheckDelphiProjectExt(const Filename: string): TModalResult;
function CreateLPRFileForDPRFile(const DPRFilename, LPRFilename: string;
  out LPRCode: TCodeBuffer): TModalResult;

// packages
function ExtractOptionsFromDPK(const Filename: string;
                               APackage: TLazPackage): TModalResult;

// projects/packages
function FindDelphiDOF(const DelphiFilename: string): string;
function ExtractOptionsFromDOF(const DOFFilename: string;
                               AProjPkg: TObject): TModalResult;
function FindDelphiCFG(const DelphiFilename: string): string;
function ExtractOptionsFromCFG(const CFGFilename: string;
                               AProjPkg: TObject): TModalResult;
function ExtractOptionsFromDelphiSource(const Filename: string;
                                        AProjPkg: TObject): TModalResult;

// file names / search paths
function ConvertDelphiAbsoluteToRelativeFile(const Filename: string;
                                             AProject: TProject): string;
function ExpandDelphiFilename(const Filename: string; AProject: TProject): string;
function ExpandDelphiSearchPath(const SearchPath: string;
                                AProject: TProject): string;


implementation


function CheckDelphiFileExt(const Filename: string): TModalResult;
begin
  if CompareFileExt(Filename,'.pas',false)<>0 then begin
    Result:=QuestionDlg(lisNotADelphiUnit,
      Format(lisTheFileIsNotADelphiUnit, ['"', Filename, '"']),
      mtError,[mrCancel,'Skip this file',mbAbort,'Abort'],0);
    exit;
  end;
  Result:=mrOk;
end;

function CheckFilenameForLCLPaths(const Filename: string): TModalResult;
// check if the unitpath of the directory of filename contains the path to the
// LCL
var
  Directory: String;
  UnitPath: String;
  LazarusSrcDir: string;
  LCLPath: String;
  NextStartPos: Integer;
begin
  // get directory of filename
  Directory:=ExtractFilePath(Filename);
  // get unitpath definition of directory
  UnitPath:=CodeToolBoss.GetUnitPathForDirectory(Directory);
  // get lazarus source directory
  LazarusSrcDir:=
           CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'LazarusDir'];
  // create base path to LCL compiled units <LazarusSrcDir>/lcl/units/
  LCLPath:=TrimFilename(LazarusSrcDir+SetDirSeparators('/lcl/units/'));
  NextStartPos:=1;
  //writeln('CheckFilenameForLCLPaths UnitPath="',UnitPath,'" LCLPath="',LCLPath,'"');
  if GetNextUsedDirectoryInSearchPath(UnitPath,LCLPath,NextStartPos)='' then
  begin
    LCLPath:=LCLPath+'$(TargetCPU)-$(TargetOS)';
    Result:=QuestionDlg(lisLCLUnitPathMissing,
      Format(lisTheCurrentUnitPathForTheFileIsThePathToTheLCLUnits, [#13, '"',
        Filename, '"', #13, '"', UnitPath, '"', #13, #13, '"', LCLPath, '"',
        #13, #13, #13]),
      mtError,[mrCancel,'Skip this step',mrAbort,'Abort'],0);
    exit;
  end;
  Result:=mrOk;
end;

function ConvertDelphiToLazarusFilename(const DelphiFilename: string;
  RenameLowercase: boolean): string;
begin
  if RenameLowercase then
    Result:=ExtractFilePath(DelphiFilename)
            +lowercase(ExtractFileName(DelphiFilename))
  else
    Result:=DelphiFilename;
end;

function ConvertDFMToLFMFilename(const DFMFilename: string;
  KeepCase: boolean): string;
begin
  if DFMFilename<>'' then begin
    // platform and fpc independent unitnames are lowercase, so are the lfm files
    Result:=lowercase(ExtractFilenameOnly(DFMFilename));
    if KeepCase then
      Result:=ExtractFilenameOnly(DFMFilename);
    Result:=ExtractFilePath(DFMFilename)+Result+'.lfm';
  end else
    Result:='';
end;

function FindDFMFileForDelphiUnit(const DelphiFilename: string): string;
begin
  Result:=ChangeFileExt(DelphiFilename,'.dfm');
  if FileExists(Result) then exit;
  Result:=ChangeFileExt(DelphiFilename,'.DFM');
  if FileExists(Result) then exit;
  Result:=ChangeFileExt(DelphiFilename,'.xfm');
  if FileExists(Result) then exit;
  Result:=ChangeFileExt(DelphiFilename,'.XFM');
  if FileExists(Result) then exit;
  Result:='';
end;

function RenameDelphiUnitToLazarusUnit(const DelphiFilename: string;
  RenameDFMFile, RenameLowercase: boolean;
  var LazarusFilename, LFMFilename: string): TModalResult;
var
  DFMFilename: String;
begin
  LazarusFilename:=ConvertDelphiToLazarusFilename(DelphiFilename,RenameLowercase);
  LFMFilename:='';
  Result:=RenameFileWithErrorDialogs(DelphiFilename,LazarusFilename,[mbAbort]);
  if Result<>mrOK then exit;
  if RenameDFMFile then begin
    DFMFilename:=FindDFMFileForDelphiUnit(DelphiFilename);
    if (DFMFilename<>'') and (CompareFilenames(DFMFilename,LFMFilename)<>0) then
    begin
      LFMFilename:=ConvertDFMToLFMFilename(DFMFilename,not RenameLowercase);
      if FileExists(LFMFilename) then begin
        if (FileAge(LFMFilename)>=FileAge(DFMFilename)) then begin
          // .lfm is not older than .dfm -> keep .lfm
          // beware: it could be the same file
        end else begin
          // .lfm is older than .dfm -> remove .lfm
          DeleteFile(LFMFilename);
        end;
      end;
      if not FileExists(LFMFilename) then begin
        // TODO: update project
        Result:=RenameFileWithErrorDialogs(DFMFilename,LFMFilename,[mbAbort]);
        if Result<>mrOK then exit;
      end;
    end;
  end;
  Result:=mrOk;
end;

function ConvertDFMFileToLFMFile(const DFMFilename: string): TModalResult;
var
  DFMStream, LFMStream: TMemoryStream;
  LFMFilename: string;
begin
  Result:=mrOk;
  DFMStream:=TMemoryStream.Create;
  LFMStream:=TMemoryStream.Create;
  try
    try
      DFMStream.LoadFromFile(DFMFilename);
    except
      on E: Exception do begin
        Result:=QuestionDlg(lisCodeToolsDefsReadError, Format(
          lisUnableToReadFileError, ['"', DFMFilename, '"', #13, E.Message]),
          mtError,[mrIgnore,mrAbort],0);
        exit;
      end;
    end;
    try
      FormDataToText(DFMStream,LFMStream);
    except
      on E: Exception do begin
        Result:=QuestionDlg(lisFormatError,
          Format(lisUnableToConvertFileError, ['"', DFMFilename, '"', #13,
            E.Message]),
          mtError,[mrIgnore,mrAbort],0);
        exit;
      end;
    end;
    // converting dfm file, without renaming unit -> keep case
    LFMFilename:=ConvertDFMToLFMFilename(DFMFilename,true);
    try
      LFMStream.SaveToFile(LFMFilename);
    except
      on E: Exception do begin
        Result:=MessageDlg(lisCodeToolsDefsWriteError,
          Format(lisUnableToWriteFileError, ['"', LFMFilename, '"', #13,
            E.Message]),
          mtError,[mbIgnore,mbAbort],0);
        exit;
      end;
    end;
  finally
    LFMSTream.Free;
    DFMStream.Free;
  end;
end;

function ConvertDelphiSourceToLazarusSource(const LazarusUnitFilename: string;
  AddLRSCode: boolean): TModalResult;
var
  LazUnitCode: TCodeBuffer;
  CTResult: Boolean;
begin
  Result:=LoadCodeBuffer(LazUnitCode,LazarusUnitFilename,
                         [lbfCheckIfText,lbfUpdateFromDisk]);
  if Result<>mrOk then exit;
  CTResult:=CodeToolBoss.ConvertDelphiToLazarusSource(LazUnitCode,AddLRSCode);
  if not CTResult then begin
    DebugLn('ConvertDelphiSourceToLazarusSource Failed');
    Result:=mrCancel;
    exit;
  end;
  Result:=mrOk;
end;

function FixMissingUnits(const LazarusUnitFilename: string;
  IsSubProc: boolean): TModalResult;

  function MissingUnitNameToMessage(CodeBuf: TCodeBuffer;
    const MissingUnit: string): string;
  var
    p: Integer;
    NamePos, InPos: Integer;
    Line, Col: Integer;
    ShortFilename: String;
    AnUnitName: String;
  begin
    ShortFilename:=ExtractFileName(CodeBuf.Filename);
    AnUnitName:=MissingUnit;
    // cut 'in' extension
    p:=System.Pos(' ',AnUnitName);
    if p>0 then
      AnUnitName:=copy(AnUnitName,1,p-1);
    Line:=1;
    Col:=1;
    if CodeToolBoss.FindUnitInAllUsesSections(CodeBuf,AnUnitName,
      NamePos,InPos)
    then begin
      if InPos=0 then ;
      CodeBuf.AbsoluteToLineCol(NamePos,Line,Col);
    end;
    Result:=ShortFilename+'('+IntToStr(Line)+','+IntToStr(Col)+') Error: '
            +'Can''t find unit '+AnUnitName;
  end;

var
  LazUnitCode: TCodeBuffer;
  CTResult: Boolean;
  MissingUnits: TStrings;
  MissingUnitsText: String;
  i: Integer;
  Msg: String;
  CurDir: String;
  CodePos: PCodeXYPosition;
  MissingIncludeFilesCodeXYPos: TFPList;
begin
  Result:=LoadCodeBuffer(LazUnitCode,LazarusUnitFilename,
                         [lbfCheckIfText,lbfUpdateFromDisk]);
  if Result<>mrOk then exit;

  // fix include filenames
  DebugLn('FixMissingUnits fixing include directives ...');
  MissingIncludeFilesCodeXYPos:=nil;
  try
    if not CodeToolBoss.FixIncludeFilenames(LazUnitCode,true,
      MissingIncludeFilesCodeXYPos)
    then begin
      DebugLn('FixMissingUnits Error="',CodeToolBoss.ErrorMessage,'"');
      if MissingIncludeFilesCodeXYPos<>nil then begin
        for i:=0 to MissingIncludeFilesCodeXYPos.Count-1 do begin
          CodePos:=PCodeXYPosition(MissingIncludeFilesCodeXYPos[i]);
          Msg:=CodePos^.Code.Filename
               +'('+IntToStr(CodePos^.y)+','+IntToStr(CodePos^.x)+')'
               +' missing include file';
          DebugLn('FixMissingUnits "',Msg,'"');
          IDEMessagesWindow.AddMsg(Msg,'',-1);
        end;
      end;
      DebugLn('FixMissingUnits 2 Error="',CodeToolBoss.ErrorMessage,'"');
      Result:=JumpToCodetoolErrorAndAskToAbort(IsSubProc);
      exit;
    end;
  finally
    CodeToolBoss.FreeListOfPCodeXYPosition(MissingIncludeFilesCodeXYPos);
  end;

  MissingUnits:=nil;
  try
    // find missing units
    DebugLn('FixMissingUnits FindMissingUnits');
    CTResult:=CodeToolBoss.FindMissingUnits(LazUnitCode,MissingUnits,true);
    if not CTResult then begin
      Result:=JumpToCodetoolErrorAndAskToAbort(IsSubProc);
      exit;
    end;
    if (MissingUnits=nil) or (MissingUnits.Count=0) then begin
      // no missing units -> good
      Result:=mrOk;
      exit;
    end;

    MissingUnitsText:='';
    for i:=0 to MissingUnits.Count-1 do begin
      if MissingUnitsText<>'' then
        MissingUnitsText:=MissingUnitsText+', ';
      MissingUnitsText:=MissingUnitsText+MissingUnits[i];
    end;
    DebugLn('FixMissingUnits FindMissingUnits="',MissingUnitsText,'"');
    
    // ask user if missing units should be commented
    if MissingUnits.Count=1 then
      Msg:=lisUnitNotFound
    else
      Msg:=lisUnitsNotFound2;
    Msg:=Msg+' '+ExtractFileName(LazUnitCode.Filename);
    
    // add error messages, so the user can click on them
    CurDir:=ExtractFilePath(LazUnitCode.Filename);
    for i:=0 to MissingUnits.Count-1 do begin
      IDEMessagesWindow.AddMsg(
               MissingUnitNameToMessage(LazUnitCode,MissingUnits[i]),CurDir,-1);
    end;

    // ask user, what to do
    Result:=QuestionDlg(Msg,
      Format(lisTheFollowingUnitsWereNotFound1EitherTheseUnitsAreN, [#13,
        MissingUnitsText, #13, #13, #13]),
      mtConfirmation,[mrYes,'Comment missing units',mrAbort],0);
    if Result<>mrYes then exit;

    // comment missing units
    DebugLn('FixMissingUnits CommentUnitsInUsesSections ',MissingUnits.Text);
    CTResult:=CodeToolBoss.CommentUnitsInUsesSections(LazUnitCode,MissingUnits);
    if not CTResult then begin
      Result:=JumpToCodetoolErrorAndAskToAbort(IsSubProc);
      exit;
    end;

  finally
    MissingUnits.Free;
  end;
  Result:=mrOk;
end;

function LoadUnitAndLFMFile(const UnitFileName: string;
  var UnitCode, LFMCode: TCodeBuffer; LFMMustExist: boolean): TModalResult;
var
  LFMFilename: string;
begin
  UnitCode:=nil;
  LFMCode:=nil;
  Result:=LoadCodeBuffer(UnitCode,UnitFileName,
                         [lbfCheckIfText,lbfUpdateFromDisk]);
  if Result<>mrOk then exit;
  LFMFilename:=ChangeFileExt(UnitFileName,'.lfm');
  if FileExists(LFMFilename) then begin
    Result:=LoadCodeBuffer(LFMCode,LFMFilename,
                           [lbfCheckIfText,lbfUpdateFromDisk]);
    if Result<>mrOk then exit;
  end else if LFMMustExist then begin
    Result:=QuestionDlg(lisLFMFileNotFound,
                       Format(lisUnitLFMFile, [UnitFileName, #13, LFMFilename]),
                       mtError,[mrCancel,'Skip this step',mrAbort],0);
  end;
end;

function ConvertLFMtoLRSfile(const LFMFilename: string): TModalResult;
begin
  if not LFMtoLRSfile(LFMFilename) then begin
    Result:=MessageDlg(lisErrorCreatingLrs,
      lisUnableToConvertLfmToLrsAndWriteLrsFile,
      mtError,[mbCancel],0);
    exit;
  end;
  Result:=mrOk;
end;

function CheckDelphiProjectExt(const Filename: string): TModalResult;
begin
  if CompareFileExt(Filename,'.dpr',false)<>0 then begin
    Result:=QuestionDlg(lisNotADelphiProject,
      Format(lisTheFileIsNotADelphiProjectDpr, ['"', Filename, '"']),
      mtError,[mrCancel,'Skipt this step',mbAbort],0);
    exit;
  end;
  Result:=mrOk;
end;

function CreateLPRFileForDPRFile(const DPRFilename, LPRFilename: string;
  out LPRCode: TCodeBuffer): TModalResult;
begin
  if not FileExists(LPRFilename) then begin
    Result:=CopyFileWithErrorDialogs(DPRFilename,LPRFilename,[]);
    if Result<>mrOk then exit;
  end;
  Result:=LoadCodeBuffer(LPRCode,LPRFilename,
                         [lbfCheckIfText,lbfUpdateFromDisk]);
end;

function ExtractOptionsFromDelphiSource(const Filename: string;
  AProjPkg: TObject): TModalResult;
begin
  // TODO remove compiler directives and put them into project/package
  Result:=mrOk;
end;

function ExtractOptionsFromDPK(const Filename: string; APackage: TLazPackage
  ): TModalResult;
begin
  // TODO
  Result:=mrOk;
end;

function FindDelphiDOF(const DelphiFilename: string): string;
var
  Filename: String;
begin
  Result:=ChangeFileExt(DelphiFilename,'.dof');
  Filename:=FindDiskFileCaseInsensitive(Result);
  if Filename<>'' then
    Result:=Filename;
end;

function ExtractOptionsFromDOF(const DOFFilename: string; AProjPkg: TObject
  ): TModalResult;
// parse .dof file and put options into AProject
var
  IniFile: TIniFile;
  AProject: TProject;
  APackage: TLazPackage;
  CompOpts: TBaseCompilerOptions;

  function ReadDirectory(const Section, Ident: string): string;
  begin
    Result:=IniFile.ReadString(Section,Ident,'');
    DebugLn('.dof ReadDirectory Section=',Section,' Ident=',Ident,' Result="',Result,'"');
    Result:=ExpandDelphiFilename(Result,AProject);
  end;
  
  function ReadSearchPath(const Section, Ident: string): string;
  var
    SearchPath: String;
  begin
    SearchPath:=IniFile.ReadString(Section,Ident,'');
    DebugLn('.dof ReadSearchPath Section=',Section,' Ident=',Ident,' SearchPath="',SearchPath,'"');
    Result:=ExpandDelphiSearchPath(SearchPath,AProject);
  end;
  
  procedure AddPackageDependency(const LazarusPkgName: string);
  begin
    if AProject<>nil then
      AProject.AddPackageDependency(LazarusPkgName)
    else if APackage<>nil then
      APackage.AddPackageDependency(LazarusPkgName);
  end;
  
  procedure AddPackageDependency(const DelphiPkgName, DelphiPkgNames,
    LazarusPkgName: string);
  begin
    if DelphiPkgName='' then exit;
    if System.Pos(';'+lowercase(DelphiPkgName)+';',
                  ';'+lowercase(DelphiPkgNames)+';')>0 then begin
      DebugLn('AddPackageDependency adding package dependency ',LazarusPkgName);
      AddPackageDependency(LazarusPkgName);
    end;
  end;

  procedure ReadDelphiPackages;
  var
    DelphiPackages: String;
    Pkgs: TStrings;
    i: Integer;
    Pkg: string;
  begin
    DelphiPackages:=IniFile.ReadString('Directories','Packages','');
    //DebugLn('ReadDelphiPackages DelphiPackages=',DelphiPackages);
    Pkgs:=SplitString(DelphiPackages,';');
    if Pkgs=nil then exit;
    try
      for i:=0 to Pkgs.Count-1 do begin
        Pkg:=Pkgs[i];
        DebugLn('ReadDelphiPackages Pkg=',Pkg);
        AddPackageDependency(Pkg,'rtl,dbrtl','FCL');
        AddPackageDependency('LCL');
      end;
    finally
      Pkgs.Free;
    end;
  end;
  
  procedure AddSearchPath(const SearchPath: string);
  begin
    CompOpts.IncludePath:=MergeSearchPaths(CompOpts.IncludePath,SearchPath);
    CompOpts.Libraries:=MergeSearchPaths(CompOpts.Libraries,SearchPath);
    CompOpts.OtherUnitFiles:=MergeSearchPaths(CompOpts.OtherUnitFiles,SearchPath);
    CompOpts.ObjectPath:=MergeSearchPaths(CompOpts.ObjectPath,SearchPath);
    CompOpts.DebugPath:=MergeSearchPaths(CompOpts.DebugPath,SearchPath);
  end;

var
  OutputDir: String;
  SearchPath: String;
  DebugSourceDirs: String;
begin
  if not FileExists(DOFFilename) then exit(mrOk);
  if AProjPkg is TProject then begin
    AProject:=TProject(AProjPkg);
    APackage:=nil;
    CompOpts:=AProject.CompilerOptions;
  end else if AProjPkg is TLazPackage then begin
    AProject:=nil;
    APackage:=TLazPackage(AProjPkg);
    CompOpts:=APackage.CompilerOptions;
  end else
    RaiseGDBException('invalid AProjPkg');
  
  try
    IniFile:=TIniFile.Create(DOFFilename);
    try
      // output directory
      if AProject<>nil then begin
        OutputDir:=ReadDirectory('Directories','OutputDir');
        if (OutputDir<>'') then begin
          DebugLn('ExtractOptionsFromDOF setting unit output directory to "',OutputDir,'"');
          AProject.CompilerOptions.UnitOutputDirectory:=OutputDir;
        end;
      end;
      
      // search path
      SearchPath:=ReadSearchPath('Directories','SearchPath');
      if (SearchPath<>'') then begin
        DebugLn('ExtractOptionsFromDOF Adding to search paths: "',SearchPath,'"');
        AddSearchPath(SearchPath);
      end;

      // debug source dirs
      DebugSourceDirs:=ReadSearchPath('Directories','DebugSourceDirs');
      if DebugSourceDirs<>'' then begin
        DebugLn('ExtractOptionsFromDOF Adding to debug paths: "',DebugSourceDirs,'"');
        CompOpts.DebugPath:=MergeSearchPaths(CompOpts.DebugPath,DebugSourceDirs);
      end;
      
      // packages
      ReadDelphiPackages;

      if AProject<>nil then begin
        if IniFile.ReadString('Linker','ConsoleApp','')='0' then begin
          // does not need a windows console
          DebugLn('ExtractOptionsFromDOF ConsoleApp=0');
          AProject.LazCompilerOptions.Win32GraphicApp:=true;
        end;
      end;
    finally
      IniFile.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('ExtractOptionsFromDOF failed reading "'+DOFFilename+'" '+E.Message);
    end;
  end;
  Result:=mrOk;
end;

function FindDelphiCFG(const DelphiFilename: string): string;
var
  Filename: String;
begin
  Result:=ChangeFileExt(DelphiFilename,'.cfg');
  Filename:=FindDiskFileCaseInsensitive(Result);
  if Filename<>'' then
    Result:=Filename;
end;

function ExtractOptionsFromCFG(const CFGFilename: string; AProjPkg: TObject
  ): TModalResult;
var
  sl: TStringList;
  i: Integer;
  Line: string;
  UnitPath: String;
  IncludePath: String;
  AProject: TProject;
  CompOpts: TBaseCompilerOptions;
  APackage: TLazPackage;
begin
  if not FileExists(CFGFilename) then exit(mrOk);
  if AProjPkg is TProject then begin
    AProject:=TProject(AProjPkg);
    CompOpts:=AProject.CompilerOptions;
  end else if AProjPkg is TLazPackage then begin
    APackage:=TLazPackage(AProjPkg);
    CompOpts:=APackage.CompilerOptions;
  end else
    RaiseGDBException('invalid AProjPkg');
  try
    sl:=TStringList.Create;
    try
      sl.LoadFromFile(CFGFilename);
      for i:=0 to sl.Count-1 do begin
        Line:=sl[i];
        if Line='' then continue;
        if (Line[1]<>'-') or (length(Line)<2) then continue;
        if Line[2]='U' then begin
          UnitPath:=ExpandDelphiSearchPath(copy(Line,4,length(Line)-4),AProject);
          if UnitPath<>'' then begin
            DebugLn('ExtractOptionsFromCFG adding unitpath "',UnitPath,'"');
            CompOpts.OtherUnitFiles:=
                             MergeSearchPaths(CompOpts.OtherUnitFiles,UnitPath);
          end;
        end else if Line[2]='I' then begin
          IncludePath:=ExpandDelphiSearchPath(copy(Line,4,length(Line)-4),AProject);
          if IncludePath<>'' then begin
            DebugLn('ExtractOptionsFromCFG adding IncludePath "',IncludePath,'"');
            CompOpts.IncludePath:=
                             MergeSearchPaths(CompOpts.IncludePath,IncludePath);
          end;
        end;
      end;
    finally
      sl.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('ExtractOptionsFromDOF failed reading "'+CFGFilename+'" '+E.Message);
    end;
  end;
  Result:=mrOk;
end;

function ConvertDelphiAbsoluteToRelativeFile(const Filename: string;
  AProject: TProject): string;
var
  ProjectDir: String;
  ShortProjectDir: String;
  p: LongInt;
begin
  // often projects use paths near to their project directory
  // For example:
  //   A project /somewhere/MyProjects/project1.dpr
  // and a path C:\Delphi\MyProject\folder
  // can mean, that the relative path is 'folder'

  ProjectDir:=AProject.ProjectDirectory;
  ShortProjectDir:=PathDelim+ExtractFileName(ChompPathDelim(ProjectDir))+PathDelim;
  p:=System.Pos(ShortProjectDir,Filename);
  //DebugLn('ConvertDelphiAbsoluteToRelativeFile ShortProjectDir="',ShortProjectDir,'" ',Filename,' ',dbgs(p));
  if (p>0) then begin
    Result:=copy(Filename,p+length(ShortProjectDir),length(Filename));
    exit;
  end;

  // ignore all other absolute paths
  Result:='';
end;

function ExpandDelphiFilename(const Filename: string; AProject: TProject
  ): string;
var
  p: LongInt;
begin
  Result:=Filename;
  if Result='' then exit;
  Result:=TrimFilename(SetDirSeparators(Result));

  // check for $(Delphi) macro
  p:=System.Pos('$(DELPHI)',Result);
  //DebugLn('ExpandDelphiFilename Result="',Result,'" ',dbgs(p));
  if p>0 then begin
    // Delphi features are provided by FPC and Lazarus
    // -> ignore
    Result:='';
  end;

  // check for other macros
  p:=System.Pos('$(',Result);
  if p>0 then begin
    // path macros are not supported
    // -> ignore
    Result:='';
  end;

  if FilenameIsWinAbsolute(Result) then begin
    // absolute filenames are not portable
    Result:=ConvertDelphiAbsoluteToRelativeFile(Result,AProject);
  end;

  // change PathDelim
  Result:=TrimFilename(SetDirSeparators(Result));
end;

function ExpandDelphiSearchPath(const SearchPath: string;
  AProject: TProject): string;
var
  Paths: TStrings;
  i: Integer;
  CurPath: String;
  j: Integer;
begin
  Result:='';
  //DebugLn('ExpandDelphiSearchPath SearchPath="',SearchPath,'"');
  Paths:=SplitString(SearchPath,';');
  if Paths=nil then exit;
  try
    // expand Delphi paths
    for i:=0 to Paths.Count-1 do
      Paths[i]:=ExpandDelphiFilename(Paths[i],AProject);
    DebugLn(Paths.Text);
    // remove doubles
    for i:=Paths.Count-1 downto 0 do begin
      CurPath:=Paths[i];
      if (CurPath='') then
        Paths.Delete(i)
      else begin
        j:=i-1;
        while (j>=0) and (CompareText(CurPath,Paths[i])<>0) do dec(j);
        if j>=0 then
          Paths.Delete(i);
      end;
    end;
    Result:='';
    for i:=0 to Paths.Count-1 do begin
      if i>0 then Result:=Result+';';
      Result:=Result+Paths[i];
    end;
    //DebugLn('ExpandDelphiSearchPath Result="',Result,'"');
  finally
    Paths.Free;
  end;
end;

initialization
  {$I delphiunit2laz.lrs}

end.

