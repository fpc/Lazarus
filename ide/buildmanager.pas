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
  LCLProc, Dialogs, FileUtil,
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
    function MacroFuncLCLWidgetType(const Param: string; const Data: PtrInt;
                                    var Abort: boolean): string;
    function MacroFuncTargetCPU(const Param: string; const Data: PtrInt;
                                var Abort: boolean): string;
    function MacroFuncTargetOS(const Param: string; const Data: PtrInt;
                               var Abort: boolean): string;
    function MacroFuncParams(const Param: string; const Data: PtrInt;
                             var Abort: boolean): string;
    function MacroFuncProjFile(const Param: string; const Data: PtrInt;
                               var Abort: boolean): string;
    function MacroFuncProjPath(const Param: string; const Data: PtrInt;
                               var Abort: boolean): string;
    function MacroFuncTargetFile(const Param: string; const Data: PtrInt;
                                 var Abort: boolean): string;
    function MacroFuncTargetCmdLine(const Param: string; const Data: PtrInt;
                                    var Abort: boolean): string;
    function MacroFuncRunCmdLine(const Param: string; const Data: PtrInt;
                                 var Abort: boolean): string;
    function MacroFuncProjPublishDir(const Param: string; const Data: PtrInt;
                                     var Abort: boolean): string;
    function MacroFuncProjUnitPath(const Param: string; const Data: PtrInt;
                                   var Abort: boolean): string;
    function MacroFuncProjIncPath(const Param: string; const Data: PtrInt;
                                  var Abort: boolean): string;
    function MacroFuncProjSrcPath(const Param: string; const Data: PtrInt;
                                  var Abort: boolean): string;
    function CTMacroFuncProjectUnitPath(Data: Pointer): boolean;
    function CTMacroFuncProjectIncPath(Data: Pointer): boolean;
    function CTMacroFuncProjectSrcPath(Data: Pointer): boolean;
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

    function GetTargetOS(UseCache: boolean): string; override;
    function GetTargetCPU(UseCache: boolean): string; override;
    function GetLCLWidgetType(UseCache: boolean): string; override;
    function GetRunCommandLine: string; override;

    function GetProjectPublishDir: string; override;
    function GetProjectTargetFilename: string; override;
    function GetTestProjectFilename: string; override;
    function GetTestUnitFilename(AnUnitInfo: TUnitInfo): string; override;
    function GetTestBuildDirectory: string; override;
    function IsTestUnitFilename(const AFilename: string): boolean; override;
    function GetTargetUnitFilename(AnUnitInfo: TUnitInfo): string; override;

    procedure GetFPCCompilerParamsForEnvironmentTest(out Params: string);
    procedure RescanCompilerDefines(OnlyIfCompilerChanged: boolean);

    // methods for building
    procedure SetBuildTarget(const TargetOS, TargetCPU, LCLWidgetType: string);
    procedure SetBuildTargetIDE;
  end;
  
var
  MainBuildBoss: TBuildManager = nil;
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
  if System.Pos('CompPath',UnparsedValue)>0 then
    DebugLn(['TBuildManager.OnSubstituteCompilerOption UnparsedValue="',UnparsedValue,'" Result="',Result,'" ',GlobalMacroList.FindByName('CompPath')<>nil]);
end;

constructor TBuildManager.Create;
begin
  MainBuildBoss:=Self;
  inherited Create;
end;

destructor TBuildManager.Destroy;
begin
  inherited Destroy;
  MainBuildBoss:=nil;
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
  GlobalMacroList.Add(TTransferMacro.Create('LCLWidgetType','',
                    lisLCLWidgetType,@MacroFuncLCLWidgetType,[]));
  GlobalMacroList.Add(TTransferMacro.Create('TargetCPU','',
                    lisTargetCPU,@MacroFuncTargetCPU,[]));
  GlobalMacroList.Add(TTransferMacro.Create('TargetOS','',
                    lisTargetOS,@MacroFuncTargetOS,[]));
  GlobalMacroList.Add(TTransferMacro.Create('Params','',
                    lisCommandLineParamsOfProgram,@MacroFuncParams,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjFile','',
                    lisProjectFilename,@MacroFuncProjFile,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjPath','',
                    lisProjectDirectory,@MacroFuncProjPath,[]));
  GlobalMacroList.Add(TTransferMacro.Create('TargetFile','',
                    lisTargetFilenameOfProject,@MacroFuncTargetFile,[]));
  GlobalMacroList.Add(TTransferMacro.Create('TargetCmdLine','',
                    lisTargetFilenamePlusParams,@MacroFuncTargetCmdLine,[]));
  GlobalMacroList.Add(TTransferMacro.Create('RunCmdLine','',
                    lisLaunchingCmdLine,@MacroFuncRunCmdLine,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjPublishDir','',
                    lisPublishProjDir,@MacroFuncProjPublishDir,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjUnitPath','',
                    lisProjectUnitPath,@MacroFuncProjUnitPath,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjIncPath','',
                    lisProjectIncPath,@MacroFuncProjIncPath,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjSrcPath','',
                    lisProjectSrcPath,@MacroFuncProjSrcPath,[]));

  // codetools macro functions
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECTUNITPATH',nil,@CTMacroFuncProjectUnitPath);
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECTINCPATH',nil,@CTMacroFuncProjectIncPath);
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECTSRCPATH',nil,@CTMacroFuncProjectSrcPath);
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

function TBuildManager.GetRunCommandLine: string;
var
  TargetFileName: string;
begin
  if Project1.RunParameterOptions.UseLaunchingApplication then
    Result := Project1.RunParameterOptions.LaunchingApplicationPathPlusParams
  else
    Result := '';

  if Result=''
  then begin
    Result:=Project1.RunParameterOptions.CmdLineParams;
    if GlobalMacroList.SubstituteStr(Result) then begin
      TargetFileName:='"'+GetProjectTargetFilename+'"';
      if Result='' then
        Result:=TargetFileName
      else
        Result:=TargetFilename+' '+Result;
    end else
      Result:='';
  end else begin
    if not GlobalMacroList.SubstituteStr(Result) then Result:='';
  end;
end;

function TBuildManager.GetProjectPublishDir: string;
begin
  if Project1=nil then begin
    Result:='';
    exit;
  end;
  Result:=Project1.PublishOptions.DestinationDirectory;
  if GlobalMacroList.SubstituteStr(Result) then begin
    if FilenameIsAbsolute(Result) then begin
      Result:=AppendPathDelim(TrimFilename(Result));
    end else begin
      Result:='';
    end;
  end else begin
    Result:='';
  end;
end;

function TBuildManager.GetProjectTargetFilename: string;
begin
  Result:='';
  if Project1=nil then exit;
  Result:=Project1.RunParameterOptions.HostApplicationFilename;
  if Result='' then begin
    if Project1.IsVirtual then
      Result:=GetTestProjectFilename
    else begin
      if Project1.MainUnitID>=0 then begin
        Result:=
          Project1.CompilerOptions.CreateTargetFilename(Project1.MainFilename)
      end;
    end;
  end;
end;

function TBuildManager.GetTestProjectFilename: string;
begin
  Result:='';
  if Project1=nil then exit;
  if (Project1.MainUnitID<0) then exit;
  Result:=GetTestUnitFilename(Project1.MainUnitInfo);
  if Result='' then exit;
  Result:=Project1.CompilerOptions.CreateTargetFilename(Result);
end;

function TBuildManager.GetTestUnitFilename(AnUnitInfo: TUnitInfo): string;
var
  TestDir: String;
begin
  Result:='';
  if AnUnitInfo=nil then exit;
  TestDir:=GetTestBuildDirectory;
  if TestDir='' then exit;
  Result:=ExtractFilename(AnUnitInfo.Filename);
  if Result='' then exit;
  Result:=TestDir+Result;
end;

function TBuildManager.GetTestBuildDirectory: string;
begin
  Result:=EnvironmentOptions.GetTestBuildDirectory;
end;

function TBuildManager.IsTestUnitFilename(const AFilename: string): boolean;
var
  TestDir: string;
begin
  Result:=false;
  if Project1.IsVirtual then begin
    TestDir:=GetTestBuildDirectory;
    Result:=CompareFileNames(TestDir,ExtractFilePath(AFilename))=0;
  end;
end;

function TBuildManager.GetTargetUnitFilename(AnUnitInfo: TUnitInfo): string;
begin
  if Project1.IsVirtual then
    Result:=GetTestUnitFilename(AnUnitInfo)
  else
    Result:=AnUnitInfo.Filename;
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

function TBuildManager.MacroFuncLCLWidgetType(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Data=CompilerOptionMacroPlatformIndependent then
    Result:='%(LCL_PLATFORM)'
  else
    Result:=GetLCLWidgetType(true);
end;

function TBuildManager.MacroFuncTargetCPU(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Data=CompilerOptionMacroPlatformIndependent then
    Result:='%(CPU_TARGET)'
  else
    Result:=GetTargetCPU(true);
end;

function TBuildManager.MacroFuncTargetOS(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Data=CompilerOptionMacroPlatformIndependent then
    Result:='%(OS_TARGET)'
  else
    Result:=GetTargetOS(true);
end;

function TBuildManager.MacroFuncParams(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  if Project1<>nil then
    Result:=Project1.RunParameterOptions.CmdLineParams
  else
    Result:='';
end;

function TBuildManager.MacroFuncProjFile(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Project1<>nil then
    Result:=Project1.MainFilename
  else
    Result:='';
end;

function TBuildManager.MacroFuncProjPath(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Project1<>nil then
    Result:=Project1.ProjectDirectory
  else
    Result:='';
end;

function TBuildManager.MacroFuncTargetFile(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Project1<>nil then
    Result:=GetProjectTargetFilename
  else
    Result:='';
end;

function TBuildManager.MacroFuncTargetCmdLine(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Project1<>nil then begin
    Result:=Project1.RunParameterOptions.CmdLineParams;
    if Result='' then
      Result:=GetProjectTargetFilename
    else
      Result:=GetProjectTargetFilename+' '+Result;
  end else
    Result:='';
end;

function TBuildManager.MacroFuncRunCmdLine(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Project1<>nil then
    Result:=GetRunCommandLine
  else
    Result:='';
end;

function TBuildManager.MacroFuncProjPublishDir(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Project1<>nil then
    Result:=GetProjectPublishDir
  else
    Result:='';
end;

function TBuildManager.MacroFuncProjUnitPath(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Project1<>nil then
    Result:=Project1.CompilerOptions.GetUnitPath(false)
  else
    Result:='';
end;

function TBuildManager.MacroFuncProjIncPath(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Project1<>nil then
    Result:=Project1.CompilerOptions.GetIncludePath(false)
  else
    Result:='';
end;

function TBuildManager.MacroFuncProjSrcPath(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Project1<>nil then
    Result:=Project1.CompilerOptions.GetSrcPath(false)
  else
    Result:='';
end;

function TBuildManager.CTMacroFuncProjectUnitPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  Result:=false;
  if Project1<>nil then begin
    FuncData^.Result:=Project1.CompilerOptions.GetUnitPath(false);
    Result:=true;
  end;
end;

function TBuildManager.CTMacroFuncProjectIncPath(Data: Pointer): boolean;
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

function TBuildManager.CTMacroFuncProjectSrcPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  Result:=false;
  if Project1<>nil then begin
    FuncData^.Result:=Project1.CompilerOptions.GetSrcPath(false);
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

