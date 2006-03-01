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

}
unit DelphiProject2Laz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Dialogs,
  ExprEval, CodeCache, CodeToolManager,
  SrcEditorIntf, MsgIntf, MainIntf, LazIDEIntf, ProjectIntf,
  DelphiUnit2Laz, Project, DialogProcs, CheckLFMDlg,
  EditorOptions, ProjectInspector, CompilerOptions,
  BasePkgManager, PkgManager;

function ConvertDelphiToLazarusProject(const DelphiFilename: string
  ): TModalResult;
function ConvertDelphiToLazarusUnit(const DelphiFilename: string
  ): TModalResult;


implementation

function ConvertDelphiToLazarusProject(const DelphiFilename: string): TModalResult;
var
  DPRCode: TCodeBuffer;
  FoundInUnits, MissingInUnits, NormalUnits: TStrings;
  NotFoundUnits: String;
  LPRCode: TCodeBuffer;
  NewProjectDesc: TProjectEmptyProgramDescriptor;
  i: Integer;
  CurUnitInfo: TUnitInfo;
  MainUnitInfo: TUnitInfo;
  DOFFilename: String;
  CFGFilename: String;
begin
  debugln('ConvertDelphiToLazarusProject DelphiFilename="',DelphiFilename,'"');
  IDEMessagesWindow.Clear;
  
  // check Delphi project file
  Result:=CheckDelphiProjectExt(DelphiFilename);
  if Result<>mrOk then exit;
  
  // close Delphi file in editor
  debugln('ConvertDelphiToLazarusProject closing in editor dpr ...');
  Result:=LazarusIDE.DoCloseEditorFile(DelphiFilename,[cfSaveFirst]);
  if Result<>mrOk then exit;
  
  // commit source editor changes to codetools
  if not LazarusIDE.BeginCodeTools then begin
    Result:=mrCancel;
    exit;
  end;
  
  // load Delphi project file .dpr
  debugln('ConvertDelphiToLazarusProject loading dpr ...');
  Result:=LoadCodeBuffer(DPRCode,DelphiFilename,
                         [lbfCheckIfText,lbfUpdateFromDisk]);
  if Result<>mrOk then exit;
  
  // create .lpr file
  debugln('ConvertDelphiToLazarusProject creating lpr ...');
  Result:=CreateLPRFileForDPRFile(DelphiFilename,false,LPRCode);
  if Result<>mrOk then begin
    if CodeToolBoss.ErrorMessage<>'' then LazarusIDE.DoJumpToCodeToolBossError;
    exit;
  end;
  // close old project
  debugln('ConvertDelphiToLazarusProject closing current project ...');
  If Project1<>nil then begin
    if LazarusIDE.DoCloseProject=mrAbort then begin
      Result:=mrAbort;
      exit;
    end;
  end;

  // switch codetools to new project directory
  CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'ProjPath']:=
                              ExpandFilename(ExtractFilePath(LPRCode.Filename));

  // create a new project
  debugln('ConvertDelphiToLazarusProject creating new project ...');
  NewProjectDesc:=TProjectEmptyProgramDescriptor.Create;
  Project1:=MainIDEInterface.CreateProjectObject(NewProjectDesc,
                                           ProjectDescriptorApplication);
  Project1.BeginUpdate(true);
  try
    if ProjInspector<>nil then ProjInspector.LazProject:=Project1;
    MainUnitInfo:=TUnitInfo.Create(LPRCode);
    MainUnitInfo.SyntaxHighlighter:=
              ExtensionToLazSyntaxHighlighter(ExtractFileExt(LPRCode.Filename));
    MainUnitInfo.IsPartOfProject:=true;
    Project1.AddFile(MainUnitInfo,false);
    Project1.MainFileID:=0;
    Project1.ProjectInfoFile:=ChangeFileExt(LPRCode.Filename,'.lpi');
    Project1.CompilerOptions.CompilerPath:='$(CompPath)';
    MainIDEInterface.UpdateCaption;
    IncreaseCompilerParseStamp;

    // TODO: get all compiler options from .dpr
    Result:=ExtractOptionsFromDPR(LPRCode,Project1);
    if Result<>mrOk then exit;

    // TODO: read .dof file
    DOFFilename:=FindDelphiDOF(DelphiFilename);
    if FileExists(DOFFilename) then begin
      Result:=ExtractOptionsFromDOF(DOFFilename,Project1);
      if Result<>mrOk then exit;
    end;

    // TODO: read .cfg file
    CFGFilename:=FindDelphiCFG(DelphiFilename);
    if FileExists(CFGFilename) then begin
      Result:=ExtractOptionsFromCFG(CFGFilename,Project1);
      if Result<>mrOk then exit;
    end;

    // TODO: get all needed packages

    // add and load default required packages
    // TODO: add packages
    // WORKAROUND: add LCL
    // add lcl pp/pas dirs to source search path
    Project1.AddSrcPath('$(LazarusDir)/lcl;'
                 +'$(LazarusDir)/lcl/interfaces/$(LCLWidgetType)');
    Project1.AddPackageDependency('LCL');
    Project1.LazCompilerOptions.Win32GraphicApp:=true;
    PkgBoss.AddDefaultDependencies(Project1);
  finally
    Project1.EndUpdate;
    NewProjectDesc.Free;
  end;

  // show program unit
  debugln('ConvertDelphiToLazarusProject open lpr in editor ...');
  Result:=LazarusIDE.DoOpenEditorFile(LPRCode.Filename,-1,
                                      [ofAddToRecent,ofRegularFile]);
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
        'Some units of the delphi project were not found:'#13
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
  ActiveSrcEdit: TSourceEditorInterface;
  ActiveUnitInfo: TUnitInfo;
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

end.

