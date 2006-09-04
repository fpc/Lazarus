{  $Id: helpmanager.pas 9796 2006-09-02 21:10:32Z mattias $  }
{
 /***************************************************************************
                            buildmanager.pas
                            ----------------


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
}
unit BuildManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLProc, Dialogs,
  // codetools
  CodeToolManager, DefineTemplates,
  // IDEIntf
  MacroIntf, IDEDialogs,
  // IDE
  LazarusIDEStrConsts, DialogProcs, IDEProcs, CodeToolsOptions, InputHistory,
  MiscOptions, LazConf, EnvironmentOpts, TransferMacros, CompilerOptions,
  OutputFilter, Compiler, Project, BaseBuildManager;
  
type

  { TBuildManager }

  TBuildManager = class(TBaseBuildManager)
  private
    CurrentParsedCompilerOption: TParsedCompilerOptions;
    function OnSubstituteCompilerOption(Options: TParsedCompilerOptions;
                                        const UnparsedValue: string;
                                        PlatformIndependent: boolean): string;
    function MacroFuncMakeExe(const Filename: string; const Data: PtrInt;
                              var Abort: boolean): string;
    function MacroFuncProject(const Param: string; const Data: PtrInt;
                              var Abort: boolean): string;
    function MacroFuncProjectUnitPath(Data: Pointer): boolean;
    function MacroFuncProjectIncPath(Data: Pointer): boolean;
    function MacroFuncProjectSrcPath(Data: Pointer): boolean;
    procedure OnCmdLineCreate(var CmdLine: string; var Abort: boolean);
  protected
    OverrideTargetOS: string;
    OverrideTargetCPU: string;
    OverrideLCLWidgetType: string;
  public
    CurDefinesCompilerFilename: String;
    CurDefinesCompilerOptions: String;

    constructor Create;
    destructor Destroy; override;
    procedure SetupTransferMacros;
    procedure SetupCompilerInterface;

    function GetTargetOS(UseCache: boolean): string;
    function GetTargetCPU(UseCache: boolean): string;
    function GetLCLWidgetType(UseCache: boolean): string;

    procedure GetFPCCompilerParamsForEnvironmentTest(out Params: string);
    procedure RescanCompilerDefines(OnlyIfCompilerChanged: boolean);

    // methods for building
    procedure SetBuildTarget(const TargetOS, TargetCPU, LCLWidgetType: string);
    procedure SetBuildTargetIDE;
  end;
  
var
  BuildBoss: TBuildManager = nil;
  TheCompiler: TCompiler = nil;
  TheOutputFilter: TOutputFilter = nil;

implementation

{ TBuildManager }

function TBuildManager.OnSubstituteCompilerOption(
  Options: TParsedCompilerOptions; const UnparsedValue: string;
  PlatformIndependent: boolean): string;
begin
  CurrentParsedCompilerOption:=Options;
  Result:=UnparsedValue;
  if PlatformIndependent then
    GlobalMacroList.SubstituteStr(Result,CompilerOptionMacroPlatformIndependent)
  else
    GlobalMacroList.SubstituteStr(Result,CompilerOptionMacroNormal);
end;

constructor TBuildManager.Create;
begin

end;

destructor TBuildManager.Destroy;
begin
  inherited Destroy;
end;

procedure TBuildManager.SetupTransferMacros;
begin
  GlobalMacroList:=TTransferMacroList.Create;
  IDEMacros:=TLazIDEMacros.Create;
  CompilerOptions.OnParseString:=@OnSubstituteCompilerOption;

  // environment
  EnvironmentOptions.InitMacros(GlobalMacroList);

  // project
  GlobalMacroList.Add(TTransferMacro.Create('MakeExe','',
                      lisMakeExe,@MacroFuncMakeExe,[]));
  GlobalMacroList.Add(TTransferMacro.Create('Project','',
                    lisProjectMacroProperties,@MacroFuncProject,[]));

  // codetools macro functions
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECTUNITPATH',nil,@MacroFuncProjectUnitPath);
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECTINCPATH',nil,@MacroFuncProjectIncPath);
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECTSRCPATH',nil,@MacroFuncProjectSrcPath);
end;

procedure TBuildManager.SetupCompilerInterface;
begin
  TheCompiler := TCompiler.Create;
  with TheCompiler do begin
    OnCommandLineCreate:=@OnCmdLineCreate;
    OutputFilter:=TheOutputFilter;
  end;
end;

function TBuildManager.GetTargetOS(UseCache: boolean): string;
begin
  if UseCache then ;
  if OverrideTargetOS<>'' then
    Result:=OverrideTargetOS
  else if Project1<>nil then
    Result:=lowercase(Project1.CompilerOptions.TargetOS)
  else
    Result:='';
  if (Result='') or (Result='default') then
    Result:=GetDefaultTargetOS;
end;

function TBuildManager.GetTargetCPU(UseCache: boolean): string;
begin
  if UseCache then ;
  if OverrideTargetCPU<>'' then
    Result:=OverrideTargetCPU
  else if Project1<>nil then
    Result:=lowercase(Project1.CompilerOptions.TargetCPU)
  else
    Result:='';
  if (Result='') or (Result='default') then
    Result:=GetDefaultTargetCPU;
end;

function TBuildManager.GetLCLWidgetType(UseCache: boolean): string;
begin
  if UseCache and (CodeToolBoss<>nil) then begin
    Result:=CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'LCLWidgetType'];
  end else begin
    if OverrideLCLWidgetType<>'' then
      Result:=OverrideLCLWidgetType
    else if Project1<>nil then
      Result:=lowercase(Project1.CompilerOptions.LCLWidgetType)
    else
      Result:='';
  end;
  if (Result='') or (Result='default') then
    Result:=GetDefaultLCLWidgetType;
end;

procedure TBuildManager.GetFPCCompilerParamsForEnvironmentTest(out
  Params: string);
var
  CurTargetOS: string;
  CurTargetCPU: string;
begin
  Params:='';
  CurTargetOS:=GetTargetOS(false);
  if CurTargetOS<>'' then
    Params:=AddCmdLineParameter(Params,'-T'+CurTargetOS);
  CurTargetCPU:=GetTargetCPU(false);
  if CurTargetCPU<>'' then
    Params:=AddCmdLineParameter(Params,'-P'+CurTargetCPU);
end;

procedure TBuildManager.RescanCompilerDefines(OnlyIfCompilerChanged: boolean);
var
  CompilerTemplate, FPCSrcTemplate: TDefineTemplate;
  CompilerUnitSearchPath, CompilerUnitLinks: string;
  CurOptions: String;
  TargetOS, TargetProcessor: string;
  UnitLinksValid: boolean;
  i: Integer;

  function SearchSystemInUnitLinks: boolean;
  begin
    Result:=System.Pos('system ',CompilerUnitLinks)>0;
  end;

begin
  GetFPCCompilerParamsForEnvironmentTest(CurOptions);
  {$IFDEF VerboseFPCSrcScan}
  writeln('TMainIDE.RescanCompilerDefines A ',CurOptions,
    ' OnlyIfCompilerChanged=',OnlyIfCompilerChanged,
    ' Valid=',InputHistories.FPCConfigCache.Valid(true),
    ' ID=',InputHistories.FPCConfigCache.FindItem(CurOptions),
    ' CurDefinesCompilerFilename=',CurDefinesCompilerFilename,
    ' EnvCompilerFilename=',EnvironmentOptions.CompilerFilename,
    ' CurDefinesCompilerOptions="',CurDefinesCompilerOptions,'"',
    ' CurOptions="',CurOptions,'"',
    '');
  {$ENDIF}
  // rescan compiler defines
  // ask the compiler for its settings
  if OnlyIfCompilerChanged
  and (CurDefinesCompilerFilename=EnvironmentOptions.CompilerFilename)
  and (CurDefinesCompilerOptions=CurOptions) then
    exit;
  {$IFDEF VerboseFPCSrcScan}
  debugln('TMainIDE.RescanCompilerDefines B rebuilding FPC templates CurOptions="',CurOptions,'"');
  {$ENDIF}
  CompilerTemplate:=CodeToolBoss.DefinePool.CreateFPCTemplate(
                    EnvironmentOptions.CompilerFilename,CurOptions,
                    CreateCompilerTestPascalFilename,CompilerUnitSearchPath,
                    TargetOS,TargetProcessor,CodeToolsOpts);
  //DebugLn('TMainIDE.RescanCompilerDefines CompilerUnitSearchPath="',CompilerUnitSearchPath,'"');

  if CompilerTemplate<>nil then begin
    CurDefinesCompilerFilename:=EnvironmentOptions.CompilerFilename;
    CurDefinesCompilerOptions:=CurOptions;
    CodeToolBoss.DefineTree.ReplaceRootSameNameAddFirst(CompilerTemplate);
    UnitLinksValid:=OnlyIfCompilerChanged
                    and InputHistories.FPCConfigCache.Valid(true);
    if UnitLinksValid then begin
      i:=InputHistories.FPCConfigCache.FindItem(CurOptions);
      if i<0 then begin
        UnitLinksValid:=false;
      end
      else if CompareFilenames(InputHistories.FPCConfigCache.Items[i].FPCSrcDir,
          EnvironmentOptions.FPCSourceDirectory)<>0
      then
        UnitLinksValid:=false;
    end;
    {$IFDEF VerboseFPCSrcScan}
    debugln(['TMainIDE.RescanCompilerDefines B rescanning FPC sources  UnitLinksValid=',UnitLinksValid]);
    {$ENDIF}

    // create compiler macros to simulate the Makefiles of the FPC sources
    CompilerUnitLinks:='';
    if UnitLinksValid then
      CompilerUnitLinks:=InputHistories.FPCConfigCache.GetUnitLinks(CurOptions);
    if not SearchSystemInUnitLinks then begin
      UnitLinksValid:=false;
    end;

    FPCSrcTemplate:=CodeToolBoss.DefinePool.CreateFPCSrcTemplate(
      CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'FPCSrcDir'],
      CompilerUnitSearchPath,
      CodeToolBoss.GetCompiledSrcExtForDirectory(''),
      TargetOS,TargetProcessor,
      UnitLinksValid, CompilerUnitLinks, CodeToolsOpts);
    {$IFDEF VerboseFPCSrcScan}
    debugln('TMainIDE.RescanCompilerDefines C UnitLinks=',copy(CompilerUnitLinks,1,100));
    {$ENDIF}
    if not SearchSystemInUnitLinks then begin
      IDEMessageDialog('Error',
        'The system.ppu was not found in the FPC directories. '
        +'Make sure fpc is installed correctly and the fpc.cfg points to the right directory.',
        mtError,[mbOk]);
    end;

    if FPCSrcTemplate<>nil then begin
      CodeToolBoss.DefineTree.RemoveRootDefineTemplateByName(
                                                           FPCSrcTemplate.Name);
      FPCSrcTemplate.InsertBehind(CompilerTemplate);
      CodeToolBoss.DefineTree.ClearCache;
      // save unitlinks
      InputHistories.SetLastFPCUnitLinks(EnvironmentOptions.CompilerFilename,
                                         CurOptions,CompilerUnitSearchPath,
                                         EnvironmentOptions.FPCSourceDirectory,
                                         CompilerUnitLinks);
      InputHistories.Save;
    end else begin
      IDEMessageDialog(lisFPCSourceDirectoryError,
        lisPlzCheckTheFPCSourceDirectory,mtError,[mbOk]);
    end;
  end else begin
    IDEMessageDialog(lisCompilerError,lisPlzCheckTheCompilerName,mtError,
                     [mbOk]);
  end;
end;

function TBuildManager.MacroFuncMakeExe(const Filename: string;
  const Data: PtrInt; var Abort: boolean): string;
var
  OldExt: String;
  ExeExt: String;
begin
  Result:=Filename;
  OldExt:=ExtractFileExt(Filename);
  ExeExt:=LazConf.GetExecutableExt(GetTargetOS(true));
  if OldExt<>ExeExt then
    Result:=copy(Result,1,length(Result)-length(OldExt))+ExeExt;
  //DebugLn('TMainIDE.MacroFuncMakeExe A ',Filename,' ',Result);
end;

function TBuildManager.MacroFuncProject(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  if Project1<>nil then begin
    if CompareText(Param,'SrcPath')=0 then
      Result:=Project1.CompilerOptions.GetSrcPath(false)
    else if CompareText(Param,'IncPath')=0 then
      Result:=Project1.CompilerOptions.GetIncludePath(false)
    else if CompareText(Param,'UnitPath')=0 then
      Result:=Project1.CompilerOptions.GetUnitPath(false)
    else begin
      Result:='<Invalid parameter for macro Project:'+Param+'>';
      debugln('WARNING: TMainIDE.MacroFuncProject: ',Result);
    end;
  end else begin
    Result:='';
  end;
end;

function TBuildManager.MacroFuncProjectUnitPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  Result:=false;
  if Project1<>nil then begin
    FuncData^.Result:=Project1.CompilerOptions.GetUnitPath(false);
    //DebugLn('TMainIDE.MacroFuncProjectSrcPath "',FuncData^.Result,'"');
    Result:=true;
  end;
end;

function TBuildManager.MacroFuncProjectIncPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  Result:=false;
  if Project1<>nil then begin
    FuncData^.Result:=Project1.CompilerOptions.GetIncludePath(false);
    Result:=true;
  end;
end;

function TBuildManager.MacroFuncProjectSrcPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  Result:=false;
  if Project1<>nil then begin
    FuncData^.Result:=Project1.CompilerOptions.GetSrcPath(false);
    //DebugLn('TMainIDE.OnMacroFuncProjectSrcPath "',FuncData^.Result,'"');
    Result:=true;
  end;
end;

procedure TBuildManager.OnCmdLineCreate(var CmdLine: string; var Abort: boolean
  );
// replace all transfer macros in command line
begin
  Abort:=not GlobalMacroList.SubstituteStr(CmdLine);
end;

procedure TBuildManager.SetBuildTarget(const TargetOS, TargetCPU,
  LCLWidgetType: string);
var
  OldTargetOS: String;
  OldTargetCPU: String;
  OldLCLWidgetType: String;
  NewTargetOS: String;
  NewTargetCPU: String;
  NewLCLWidgetType: String;
  FPCTargetChanged: Boolean;
  LCLTargetChanged: Boolean;
begin
  OldTargetOS:=GetTargetOS(true);
  OldTargetCPU:=GetTargetCPU(true);
  OldLCLWidgetType:=GetLCLWidgetType(true);
  OverrideTargetOS:=TargetOS;
  OverrideTargetCPU:=TargetCPU;
  OverrideLCLWidgetType:=LCLWidgetType;
  NewTargetOS:=GetTargetOS(false);
  NewTargetCPU:=GetTargetCPU(false);
  NewLCLWidgetType:=GetLCLWidgetType(false);

  FPCTargetChanged:=(OldTargetOS<>NewTargetOS)
                    or (OldTargetCPU<>NewTargetCPU);
  LCLTargetChanged:=(OldLCLWidgetType<>NewLCLWidgetType);

  //DebugLn('TMainIDE.SetBuildTarget Old=',OldTargetCPU,'-',OldTargetOS,'-',OldLCLWidgetType,
  //  ' New=',NewTargetCPU,'-',NewTargetOS,'-',NewLCLWidgetType,' FPC=',dbgs(FPCTargetChanged),' LCL=',dbgs(LCLTargetChanged));

  if LCLTargetChanged then
    CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'LCLWidgetType']:=
                                                               NewLCLWidgetType;
  if FPCTargetChanged then
    RescanCompilerDefines(true);

  if FPCTargetChanged or LCLTargetChanged then begin
    IncreaseCompilerParseStamp;
  end;
end;

procedure TBuildManager.SetBuildTargetIDE;
var
  NewTargetOS: String;
  NewTargetCPU: String;
  NewLCLWidgetSet: String;
begin
  NewTargetOS:=MiscellaneousOptions.BuildLazOpts.TargetOS;
  NewTargetCPU:=MiscellaneousOptions.BuildLazOpts.TargetCPU;
  NewLCLWidgetSet:=LCLPlatformNames[MiscellaneousOptions.BuildLazOpts.LCLPlatform];
  if (NewTargetOS='') or (NewTargetOS='default') then
    NewTargetOS:=GetDefaultTargetOS;
  if (NewTargetCPU='') or (NewTargetCPU='default') then
    NewTargetCPU:=GetDefaultTargetCPU;
  if (NewLCLWidgetSet='') or (NewLCLWidgetSet='default') then
    NewLCLWidgetSet:=GetDefaultLCLWidgetType;
  SetBuildTarget(NewTargetOS,NewTargetCPU,NewLCLWidgetSet);
end;

end.

