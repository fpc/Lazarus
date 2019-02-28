{
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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

 Author: Mattias Gaertner

 Abstract:
   IDE dialog showing stats about the used FPC.
}
unit IDEFPCInfo;

{$mode objfpc}{$H+}

interface

uses
  // RTL + LCL
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ButtonPanel,
  // CodeTools
  DefineTemplates, CodeToolManager, FileProcs,
  // LazUtils
  LazUTF8Classes, LazFileUtils, LazUTF8,
  // Other
  IDEWindowIntf, LazIDEIntf, BaseBuildManager,
  Project, EnvironmentOpts, LazarusIDEStrConsts, AboutFrm, TransferMacros;

type

  { TIDEFPCInfoDialog }

  TIDEFPCInfoDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    CmdLineOutputMemo: TMemo;
    ValuesMemo: TMemo;
    PageControl1: TPageControl;
    ValuesTabSheet: TTabSheet;
    OutputTabSheet: TTabSheet;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdateValuesMemo;
    procedure UpdateCmdLinePage;
    procedure GatherIDEVersion(sl: TStrings);
    procedure GatherEnvironmentVars(sl: TStrings);
    procedure GatherGlobalOptions(sl: TStrings);
    procedure GatherProjectOptions(sl: TStrings);
    procedure GatherActiveOptions(sl: TStrings);
    procedure GatherFPCExecutable(UnitSetCache: TFPCUnitSetCache; sl: TStrings);
  public
  end;

function ShowFPCInfo: TModalResult;

implementation

function ShowFPCInfo: TModalResult;
var
  Dlg: TIDEFPCInfoDialog;
begin
  Dlg:=TIDEFPCInfoDialog.Create(nil);
  try
    Result:=Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

{$R *.lfm}

{ TIDEFPCInfoDialog }

procedure TIDEFPCInfoDialog.FormCreate(Sender: TObject);
begin
  Caption:=lisInformationAboutUsedFPC;

  UpdateValuesMemo;
  UpdateCmdLinePage;
  PageControl1.PageIndex:=0;
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TIDEFPCInfoDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TIDEFPCInfoDialog.UpdateValuesMemo;
var
  sl: TStringList;
  TargetOS: String;
  TargetCPU: String;
  CompilerFilename: String;
  FPCSrcDir: String;
  UnitSetCache: TFPCUnitSetCache;
begin
  sl:=TStringList.Create;
  try
    GatherIDEVersion(sl);
    GatherEnvironmentVars(sl);
    GatherGlobalOptions(sl);
    GatherProjectOptions(sl);
    GatherActiveOptions(sl);

    TargetOS:=BuildBoss.GetTargetOS;
    TargetCPU:=BuildBoss.GetTargetCPU;
    CompilerFilename:=LazarusIDE.GetCompilerFilename;
    FPCSrcDir:=EnvironmentOptions.GetParsedFPCSourceDirectory; // needs FPCVer macro
    UnitSetCache:=CodeToolBoss.CompilerDefinesCache.FindUnitSet(
      CompilerFilename,TargetOS,TargetCPU,'',FPCSrcDir,true);
    GatherFPCExecutable(UnitSetCache,sl);

    ValuesMemo.Lines.Assign(sl);
  finally
    sl.Free;
  end;
end;

procedure TIDEFPCInfoDialog.UpdateCmdLinePage;
var
  TargetOS: String;
  TargetCPU: String;
  CompilerFilename: String;
  CompilerOptions: String;
  Cfg: TPCTargetConfigCache;
  Params: String;
  ExtraOptions: String;
  sl, List: TStringList;
  TestFilename: String;
  Filename: String;
  WorkDir: String;
  fs: TFileStreamUTF8;
begin
  sl:=TStringList.Create;
  List:=nil;
  try
    sl.Add('The IDE asks the compiler with the following command for the real OS/CPU:');
    CompilerFilename:=LazarusIDE.GetCompilerFilename;
    CompilerOptions:='';
    if Project1<>nil then
    begin
      CompilerOptions:=ExtractFPCFrontEndParameters(Project1.CompilerOptions.CustomOptions);
      if not GlobalMacroList.SubstituteStr(CompilerOptions) then
      begin
        sl.Add('invalid macros in project''s compiler options: '+Project1.CompilerOptions.CustomOptions);
        CompilerOptions:='';
      end;
    end;
    if not LazarusIDE.CallHandlerGetFPCFrontEndParams(Self,CompilerOptions) then
    begin
      sl.Add('ERROR: design time event (lihtGetFPCFrontEndParams) failed to extend fpc front end parameters: "'+CompilerOptions+'"');
    end;
    Cfg:=CodeToolBoss.CompilerDefinesCache.ConfigCaches.Find(
                        CompilerFilename,CompilerOptions,'','',true);
    // fpc -i
    ExtraOptions:=Cfg.GetFPCInfoCmdLineOptions(CodeToolBoss.CompilerDefinesCache.ExtraOptions);
    Params:=Trim('-iTOTP '+ExtraOptions);
    WorkDir:=GetCurrentDirUTF8;
    sl.Add(CompilerFilename+' '+Params);
    sl.Add('Working directory: '+WorkDir);
    List:=RunTool(CompilerFilename,Params);
    if (List=nil) or (List.Count<1) then begin
      sl.Add('ERROR: unable to run compiler.');
    end else begin
      sl.Add('Output:');
      sl.AddStrings(List);
    end;
    List.Free;
    sl.Add('');

    // fpc -va
    TargetOS:=BuildBoss.GetTargetOS;
    TargetCPU:=BuildBoss.GetTargetCPU;
    Cfg:=CodeToolBoss.CompilerDefinesCache.ConfigCaches.Find(
                        CompilerFilename,CompilerOptions,TargetOS,TargetCPU,true);
    TestFilename:=CodeToolBoss.CompilerDefinesCache.TestFilename;
    Filename:=ExtractFileName(TestFilename);
    WorkDir:=ExtractFilePath(TestFilename);
    sl.Add('The IDE asks the compiler with the following command for paths and macros:');
    ExtraOptions:=Cfg.GetFPCInfoCmdLineOptions(CodeToolBoss.CompilerDefinesCache.ExtraOptions);
    Params:=Trim('-va '+ExtraOptions)+' '+Filename;
    sl.Add(CompilerFilename+' '+Params);
    sl.Add('Working directory: '+WorkDir);
    // create empty file
    try
      fs:=TFileStreamUTF8.Create(TestFilename,fmCreate);
      fs.Free;
    except
      sl.Add('ERROR: unable to create test file '+TestFilename);
      exit;
    end;
    List:=RunTool(CompilerFilename,Params,WorkDir);
    if (List=nil) or (List.Count<1) then begin
      sl.Add('ERROR: unable to run compiler.');
    end else begin
      sl.Add('Output:');
      sl.AddStrings(List);
      sl.Add('');
      sl.Add('NOTE: The '+Filename+' is empty, so compilation fails. This is what we want.');
    end;

  finally
    CmdLineOutputMemo.Lines.Assign(sl);
    List.free;
    sl.Free;
  end;
end;

procedure TIDEFPCInfoDialog.GatherIDEVersion(sl: TStrings);
begin
  sl.Add('Lazarus version: '+GetLazarusVersionString);
  sl.Add('Lazarus svn revision: '+LazarusRevisionStr);
  sl.Add('Lazarus build date: '+{$I %date%});
  sl.Add('Lazarus was compiled for '+GetCompiledTargetCPU+'-'+GetCompiledTargetOS);
  sl.Add('Lazarus was compiled with fpc '+{$I %FPCVERSION%});
  sl.Add('');
end;

procedure TIDEFPCInfoDialog.GatherEnvironmentVars(sl: TStrings);

  procedure Add(EnvName: string);
  begin
    sl.Add(EnvName+'='+GetEnvironmentVariableUTF8(EnvName));
  end;

begin
  sl.Add('Environment variables:');
  Add('PATH');
  Add('PP');
  Add('FPCDIR');
  Add('USESVN2REVISIONINC');
  Add('USER');
  Add('HOME');
  Add('PWD');
  Add('LANG');
  Add('LANGUAGE');
  sl.Add('');
end;

procedure TIDEFPCInfoDialog.GatherGlobalOptions(sl: TStrings);
begin
  sl.add('Global IDE options:');
  sl.Add('LazarusDirectory='+EnvironmentOptions.LazarusDirectory);
  sl.Add('Resolved LazarusDirectory='+EnvironmentOptions.GetParsedLazarusDirectory);
  if Project1<>nil then
    sl.Add('Project''s CompilerFilename='+Project1.CompilerOptions.CompilerPath);
  sl.Add('Resolved Project''s CompilerFilename='+Project1.GetCompilerFilename);
  sl.Add('Default CompilerFilename='+EnvironmentOptions.CompilerFilename);
  sl.Add('Resolved default compilerFilename='+EnvironmentOptions.GetParsedCompilerFilename);
  sl.Add('CompilerMessagesFilename='+EnvironmentOptions.CompilerMessagesFilename);
  sl.Add('Resolved CompilerMessagesFilename='+EnvironmentOptions.GetParsedCompilerMessagesFilename);
  sl.Add('');
end;

procedure TIDEFPCInfoDialog.GatherProjectOptions(sl: TStrings);
begin
  sl.Add('Project:');
  if Project1<>nil then begin
    sl.Add('lpi='+Project1.ProjectInfoFile);
    sl.Add('Directory='+Project1.Directory);
    sl.Add('TargetOS='+Project1.CompilerOptions.TargetOS);
    sl.Add('TargetCPU='+Project1.CompilerOptions.TargetCPU);
    sl.Add('CompilerFilename='+Project1.CompilerOptions.CompilerPath);
    sl.Add('CompilerOptions='+ExtractFPCFrontEndParameters(Project1.CompilerOptions.CompilerPath));
  end else begin
    sl.Add('no project');
  end;
  sl.Add('');
end;

procedure TIDEFPCInfoDialog.GatherActiveOptions(sl: TStrings);
begin
  sl.Add('Active target:');
  sl.Add('TargetOS='+BuildBoss.GetTargetOS);
  sl.Add('TargetCPU='+BuildBoss.GetTargetCPU);
  sl.Add('');
end;

procedure TIDEFPCInfoDialog.GatherFPCExecutable(UnitSetCache: TFPCUnitSetCache;
  sl: TStrings);
var
  CfgCache: TPCTargetConfigCache;
  i: Integer;
  CfgFileItem: TPCConfigFileState;
  HasCfgs: Boolean;
  SrcCache: TFPCSourceCache;
  AFilename: string;
  AnUnitName: string;
begin
  sl.Add('FPC executable:');
  if UnitSetCache<>nil then begin
    CfgCache:=UnitSetCache.GetConfigCache(false);
    if CfgCache<>nil then begin
      sl.Add('Compiler='+CfgCache.Compiler);
      sl.Add('Options='+CfgCache.CompilerOptions);
      sl.Add('CompilerDate='+DateTimeToStr(FileDateToDateTimeDef(CfgCache.CompilerDate)));
      sl.Add('RealCompiler='+CfgCache.RealCompiler);
      sl.Add('RealCompilerDate='+DateTimeToStr(FileDateToDateTimeDef(CfgCache.RealCompilerDate)));
      sl.Add('RealTargetOS='+CfgCache.RealTargetOS);
      sl.Add('RealTargetCPU='+CfgCache.RealTargetCPU);
      sl.Add('RealCompilerInPath='+CfgCache.RealTargetCPUCompiler);
      sl.Add('Version='+CfgCache.FullVersion);
      HasCfgs:=false;
      if CfgCache.ConfigFiles<>nil then begin
        for i:=0 to CfgCache.ConfigFiles.Count-1 do begin
          CfgFileItem:=CfgCache.ConfigFiles[i];
          if CfgFileItem.FileExists then begin
            sl.Add('CfgFilename='+CfgFileItem.Filename);
            HasCfgs:=true;
          end;
        end;
      end;
      if not HasCfgs then
        sl.Add('WARNING: fpc has no config file');
      sl.Add('');
      sl.Add('Defines:');
      if CfgCache.Defines<>nil then begin
        sl.Add(CfgCache.Defines.AsText);
      end;
      sl.Add('Undefines:');
      if CfgCache.Undefines<>nil then begin
        sl.Add(CfgCache.Undefines.AsText);
      end;
      sl.Add('Include Paths:');
      if CfgCache.IncludePaths<>nil then begin
        sl.AddStrings(CfgCache.IncludePaths);
      end;
      sl.Add('Unit Scopes:');
      if CfgCache.UnitScopes<>nil then begin
        sl.AddStrings(CfgCache.UnitScopes);
      end;
      sl.Add('Unit Paths:');
      if CfgCache.UnitPaths<>nil then begin
        sl.AddStrings(CfgCache.UnitPaths);
      end;
      sl.add('Units:');
      if CfgCache.Units<>nil then begin
        sl.Add(CfgCache.Units.AsText);
      end;
    end;
    SrcCache:=UnitSetCache.GetSourceCache(false);
    if SrcCache<>nil then begin
      sl.Add('Sources:');
      sl.Add('Directory='+SrcCache.Directory);
      if SrcCache.Files<>nil then begin
        sl.Add('Files.Count='+dbgs(SrcCache.Files.Count));
        for i:=0 to SrcCache.Files.Count-1 do begin
          AFilename:=SrcCache.Files[i];
          AnUnitName:=ExtractFilenameOnly(AFilename);
          if (AnUnitName='classes')
          or (AnUnitName='sysutils')
          or (AnUnitName='system')
          then
            sl.Add(AFilename);
        end;
      end else
        sl.Add('Files.Count=0');
    end;
  end;
  sl.Add('');
end;

end.

