{  $Id: delphiunit2laz.pas 8788 2006-02-20 23:48:13Z mattias $  }
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
    Functions to convert delphi projects to lazarus projects.

    Converting a delphi project/unit to lazarus can be a very difficult task,
    but also contains some monotone and boring work. These functions try to
    help here.
    Because any conversion step can fail and can need manual fix before
    continuing, the functions are written to recognize, what have been done.
    So, you can call the delphi conversion, abort at any step, fix a few things,
    and invoke it again, till you have a working lazarus project.
}
unit DelphiProject2Laz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Dialogs, FileUtil,
  ExprEval, CodeCache, CodeToolManager,
  SrcEditorIntf, MsgIntf, MainIntf, LazIDEIntf, ProjectIntf,
  DelphiUnit2Laz, Project, DialogProcs, CheckLFMDlg,
  EditorOptions, ProjectInspector, CompilerOptions,
  BasePkgManager, PkgManager;

function ConvertDelphiToLazarusProject(const ProjectFilename: string
  ): TModalResult;
function ConvertDelphiToLazarusUnit(const DelphiFilename: string
  ): TModalResult;

function CreateDelphiToLazarusProject(const LPIFilename: string): TModalResult;
function CreateDelphiToLazarusMainSourceFile(AProject: TProject;
  const DPRFilename, MainSourceFilename: string;
  out LPRCode: TCodeBuffer): TModalResult;
function FindDPRFilename(const StartFilename: string): string;
function ReadDelphiProjectConfigFiles(AProject: TProject): TModalResult;


implementation

function ConvertDelphiToLazarusProject(const ProjectFilename: string
  ): TModalResult;
{ Creates or updates a lazarus project (.lpi+.lpr)
  This function can be invoked on a delphi project .dpr file, or a lazarus
  project (.lpi/.lpr) file.
  It will use, whatever it finds and will make it more lazarus-like.
  It can be aborted and called again.
}
var
  FoundInUnits, MissingInUnits, NormalUnits: TStrings;
  NotFoundUnits: String;
  LPRCode: TCodeBuffer;
  NewProjectDesc: TProjectEmptyProgramDescriptor;
  i: Integer;
  CurUnitInfo: TUnitInfo;
  MainUnitInfo: TUnitInfo;
  LPIFilename: String;
  DPRFilename: String;
  MainSourceFilename: String;
begin
  debugln('ConvertDelphiToLazarusProject ProjectFilename="',ProjectFilename,'"');
  IDEMessagesWindow.Clear;

  // create/open lazarus project file
  LPIFilename:=ChangeFileExt(ProjectFilename,'.lpi');
  Result:=CreateDelphiToLazarusProject(LPIFilename);
  if Result<>mrOk then begin
    DebugLn('ConvertDelphiToLazarusProject failed to create/open project LPIFilename="',LPIFilename,'"');
    exit;
  end;
  
  // create main source file (.lpr) (only copy, no conversion)
  DPRFilename:=FindDPRFilename(ProjectFilename);
  DebugLn('ConvertDelphiToLazarusProject DPRFilename="',DPRFilename,'"');
  MainSourceFilename:=ChangeFileExt(LPIFilename,'.lpr');
  Result:=CreateDelphiToLazarusMainSourceFile(Project1,DPRFilename,
                                              MainSourceFilename,LPRCode);
  if Result<>mrOk then exit;

  // read config files (they often contain clues about paths, switches and defines)
  Result:=ReadDelphiProjectConfigFiles(Project1);
  if Result<>mrOk then exit;

  // load required packages
  Project1.AddPackageDependency('LCL');// Nearly all Delphi projects require it
  PkgBoss.AddDefaultDependencies(Project1);

  // we have now enough information to parse the .dpr file,
  // but not enough to parse the units

  // init codetools
  if not LazarusIDE.BeginCodeTools then begin
    Result:=mrCancel;
    exit;
  end;

  // fix include filenames
  if not CodeToolBoss.FixIncludeFilenames(Project1.MainUnitInfo.Source,true)
  then begin
    LazarusIDE.DoJumpToCodeToolBossError;
    exit(mrCancel);
  end;

  // try to find out as much about search paths as possible before parsing code
  // TODO: open lpr
  // TODO: fix include paths
  // TODO: get all compiler options from .dpr
  // TODO: find all project files in .dpr
  // TODO: fix all include filenames

  {$IFDEF NewDelphiProjConverter}
  exit(mrOk);
  {$ENDIF}
  
  // TODO: get all compiler options from .dpr
  Result:=ExtractOptionsFromDPR(LPRCode,Project1);
  if Result<>mrOk then exit;

  // fix
  Result:=ConvertDelphiToLazarusUnit(LPRCode.Filename);
  if Result=mrAbort then exit;

  // find all project files
  FoundInUnits:=nil;
  MissingInUnits:=nil;
  NormalUnits:=nil;
  try
    debugln('ConvertDelphiToLazarusProject gathering all project units ...');
    if not CodeToolBoss.FindDelphiProjectUnits(LPRCode,FoundInUnits,
                                               MissingInUnits, NormalUnits) then
    begin
      LazarusIDE.DoJumpToCodeToolBossError;
      Result:=mrCancel;
      exit;
    end;
    debugln('ConvertDelphiToLazarusProject FoundInUnits=[',FoundInUnits.Text,']',
      ' MissingInUnits=[',MissingInUnits.Text,']',
      ' NormalUnits=[',NormalUnits.Text,']');
    // warn about missing units
    if (MissingInUnits<>nil) and (MissingInUnits.Count>0) then begin
      NotFoundUnits:=MissingInUnits.Text;
      Result:=MessageDlg('Units not found',
        'Some units of the delphi project are missing:'#13
        +NotFoundUnits,mtWarning,[mbIgnore,mbAbort],0);
      if Result<>mrIgnore then exit;
    end;

    // add all units to the project
    debugln('ConvertDelphiToLazarusProject adding all project units to project ...');
    for i:=0 to FoundInUnits.Count-1 do begin
      CurUnitInfo:=TUnitInfo.Create(nil);
      TUnitInfo(CurUnitInfo).Filename:=FoundInUnits[i];
      Project1.AddFile(CurUnitInfo,false);
    end;
    // set search paths to find all project units
    Project1.CompilerOptions.OtherUnitFiles:=
                        Project1.SourceDirectories.CreateSearchPathFromAllFiles;
    DebugLn('ConvertDelphiToLazarusProject UnitPath="',Project1.CompilerOptions.OtherUnitFiles,'"');

    // save project
    debugln('ConvertDelphiToLazarusProject Saving new project ...');
    Result:=LazarusIDE.DoSaveProject([]);
    if Result<>mrOk then exit;

    // convert all units
    i:=0;
    while i<Project1.UnitCount do begin
      CurUnitInfo:=Project1.Units[i];
      if CurUnitInfo.IsPartOfProject and not (CurUnitInfo.IsMainUnit) then begin
        Result:=ConvertDelphiToLazarusUnit(CurUnitInfo.Filename);
        if Result=mrAbort then exit;
      end;
      inc(i);
    end;

  finally
    FoundInUnits.Free;
    MissingInUnits.Free;
    NormalUnits.Free;
  end;

  debugln('ConvertDelphiToLazarusProject Done');
  Result:=mrOk;
end;

function ConvertDelphiToLazarusUnit(const DelphiFilename: string): TModalResult;
var
  DFMFilename: String;
  LazarusUnitFilename: String;
  LRSFilename: String;
  UnitCode, LFMCode: TCodeBuffer;
  HasDFMFile: boolean;
  LFMFilename: String;
begin
  // check file and directory
  DebugLn('ConvertDelphiToLazarusUnit A ',DelphiFilename);
  Result:=CheckDelphiFileExt(DelphiFilename);
  if Result<>mrOk then exit;
  Result:=CheckFileIsWritable(DelphiFilename,[mbAbort]);
  if Result<>mrOk then exit;
  Result:=CheckFilenameForLCLPaths(DelphiFilename);
  if Result<>mrOk then exit;
  // close Delphi files in editor
  DebugLn('ConvertDelphiToLazarusUnit Close files in editor .pas/.dfm');
  Result:=LazarusIDE.DoCloseEditorFile(DelphiFilename,[cfSaveFirst]);
  if Result<>mrOk then exit;
  DFMFilename:=FindDFMFileForDelphiUnit(DelphiFilename);
  DebugLn('ConvertDelphiToLazarusUnit DFM file="',DFMFilename,'"');
  HasDFMFile:=DFMFilename<>'';
  if HasDFMFile then begin
    Result:=LazarusIDE.DoCloseEditorFile(DFMFilename,[cfSaveFirst]);
    if Result<>mrOk then exit;
  end;
  // rename files (.pas,.dfm) lowercase
  // TODO: rename files in project
  DebugLn('ConvertDelphiToLazarusUnit Rename files');
  LazarusUnitFilename:='';
  LFMFilename:='';
  Result:=RenameDelphiUnitToLazarusUnit(DelphiFilename,true,
                       LazarusUnitFilename,LFMFilename);
  if Result<>mrOk then exit;
  if LFMFilename='' then LFMFilename:=ChangeFIleExt(LazarusUnitFilename,'.lfm');
  HasDFMFile:=FileExists(LFMFilename);
  // convert .dfm file to .lfm file
  if HasDFMFile then begin
    DebugLn('ConvertDelphiToLazarusUnit Convert dfm format to lfm "',LFMFilename,'"');
    Result:=ConvertDFMFileToLFMFile(LFMFilename);
    if Result<>mrOk then exit;
  end;
  // create empty .lrs file
  DebugLn('ConvertDelphiToLazarusUnit Create empty lrs');
  if HasDFMFile then begin
    LRSFilename:=ChangeFileExt(LazarusUnitFilename,'.lrs');
    DebugLn('ConvertDelphiToLazarusUnit Create ',LRSFilename);
    Result:=CreateEmptyFile(LRSFilename,[mbAbort,mbRetry]);
    if Result<>mrOk then exit;
  end else
    LRSFilename:='';

  DebugLn('ConvertDelphiToLazarusUnit Convert delphi source');
  if not LazarusIDE.BeginCodeTools then begin
    Result:=mrCancel;
    exit;
  end;

  // add {$mode delphi} directive
  // remove windows unit and add LResources, LCLIntf
  // remove {$R *.dfm} or {$R *.xfm} directive
  // add initialization
  // add {$i unit.lrs} directive
  // TODO: fix delphi ambiguousities like incomplete proc implementation headers
  Result:=ConvertDelphiSourceToLazarusSource(LazarusUnitFilename,
                                             LRSFilename<>'');
  if Result<>mrOk then begin
    LazarusIDE.DoJumpToCodeToolBossError;
    exit;
  end;

  // comment missing units
  DebugLn('ConvertDelphiToLazarusUnit FixMissingUnits');
  Result:=FixMissingUnits(LazarusUnitFilename);
  if Result<>mrOk then begin
    LazarusIDE.DoJumpToCodeToolBossError;
    exit;
  end;

  // check the LFM file and the pascal unit
  DebugLn('ConvertDelphiToLazarusUnit Check new .lfm and .pas file');
  Result:=LoadUnitAndLFMFile(LazarusUnitFilename,UnitCode,LFMCode,HasDFMFile);
  if Result<>mrOk then exit;
  if HasDFMFile and (LFMCode=nil) then
    DebugLn('WARNING: ConvertDelphiToLazarusUnit unable to load LFMCode');
  if (LFMCode<>nil)
  and (CheckLFMBuffer(UnitCode,LFMCode,@IDEMessagesWindow.AddMsg,true,true)<>mrOk)
  then begin
    LazarusIDE.DoJumpToCompilerMessage(-1,true);
    exit;
  end;

  if LFMCode<>nil then begin
    // save LFM file
    DebugLn('ConvertDelphiToLazarusUnit Save LFM');
    Result:=MainIDEInterface.DoSaveCodeBufferToFile(LFMCode,LFMCode.Filename,false);
    if Result<>mrOk then exit;

    // convert lfm to lrs
    DebugLn('ConvertDelphiToLazarusUnit Convert lfm to lrs');
    Result:=ConvertLFMtoLRSfile(LFMCode.Filename);
    if Result<>mrOk then exit;
  end;

  Result:=mrOk;
end;

function CreateDelphiToLazarusProject(const LPIFilename: string): TModalResult;
// If .lpi does not exist, create it
// open new project
begin
  DebugLn('CreateDelphiToLazarusProject LPIFilename="',LPIFilename,'"');
  if FileExists(LPIFilename) then begin
    // there is already a lazarus project -> open it, if not already open
    if CompareFilenames(Project1.ProjectInfoFile,LPIFilename)<>0 then begin
      DebugLn('CreateDelphiToLazarusProject open "',LPIFilename,'"');
      Result:=LazarusIDE.DoOpenProjectFile(LPIFilename,[]);
      if Result<>mrOk then exit;
    end;
  end else begin
    // create a new lazarus project
    Result:=LazarusIDE.DoNewProject(ProjectDescriptorEmptyProject);
    if Result<>mrOk then begin
      DebugLn('CreateDelphiToLazarusProject failed to create a new project');
      exit;
    end;
    Project1.ProjectInfoFile:=LPIFilename;
  end;
  // save to disk (this makes sure, all editor changes are saved too)
  DebugLn('CreateDelphiToLazarusProject saving project ...');
  Result:=LazarusIDE.DoSaveProject([]);
end;

function CreateDelphiToLazarusMainSourceFile(AProject: TProject;
  const DPRFilename, MainSourceFilename: string;
  out LPRCode: TCodeBuffer): TModalResult;
// if .lpr does not exists, copy the .dpr file to the .lpr
// adds the .lpr as main unit to the project, if not already done
var
  MainUnitInfo: TUnitInfo;
begin
  LPRCode:=nil;
  Result:=CreateLPRFileForDPRFile(DPRFilename,MainSourceFilename,LPRCode);
  if Result<>mrOk then begin
    DebugLn('CreateDelphiToLazarusMainSourceFile CreateLPRFileForDPRFile failed DPRFilename="',DPRFilename,'" MainSourceFilename="',MainSourceFilename,'"');
    exit;
  end;
  if AProject.MainUnitInfo=nil then begin
    // add .lpr file to project as main unit
    DebugLn('CreateDelphiToLazarusMainSourceFile adding .lpr file to project as main unit ',LPRCode.Filename);
    MainUnitInfo:=TUnitInfo.Create(LPRCode);
    MainUnitInfo.SyntaxHighlighter:=
              ExtensionToLazSyntaxHighlighter(ExtractFileExt(LPRCode.Filename));
    MainUnitInfo.IsPartOfProject:=true;
    AProject.AddFile(MainUnitInfo,false);
    AProject.MainFileID:=0;
  end else begin
    // replace main unit in project
    AProject.MainUnitInfo.Source:=LPRCode;
  end;
end;

function FindDPRFilename(const StartFilename: string): string;
// searches the corresponding .dpr file
begin
  if CompareFileExt(StartFilename,'.dpr',false)=0 then
    Result:=StartFilename
  else
    Result:=ChangeFileExt(StartFilename,'.dpr');
  if not FileExists(Result) then
    Result:=FindDiskFileCaseInsensitive(StartFilename);
end;

function ReadDelphiProjectConfigFiles(AProject: TProject): TModalResult;
var
  MainSourceFilename: String;
  DOFFilename: String;
  CFGFilename: String;
begin
  if AProject.MainUnitInfo=nil then exit(mrOk);
  MainSourceFilename:=AProject.MainUnitInfo.Filename;

  // read .dof file
  DOFFilename:=FindDelphiDOF(MainSourceFilename);
  if FileExists(DOFFilename) then begin
    Result:=ExtractOptionsFromDOF(DOFFilename,Project1);
    if Result<>mrOk then exit;
  end;

  // read .cfg file
  CFGFilename:=FindDelphiCFG(MainSourceFilename);
  if FileExists(CFGFilename) then begin
    Result:=ExtractOptionsFromCFG(CFGFilename,Project1);
    if Result<>mrOk then exit;
  end;
end;

end.

