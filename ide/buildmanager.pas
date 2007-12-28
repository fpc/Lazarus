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
  Classes, SysUtils, AVL_Tree,
  // LCL
  LCLProc, Dialogs, FileUtil, Forms, Controls,
  // codetools
  BasicCodeTools, CodeToolManager, DefineTemplates,
  // IDEIntf
  SrcEditorIntf, ProjectIntf, MacroIntf, IDEDialogs, IDEExternToolIntf,
  LazIDEIntf,
  // IDE
  LazarusIDEStrConsts, DialogProcs, IDEProcs, CodeToolsOptions, InputHistory,
  MiscOptions, LazConf, EnvironmentOpts, TransferMacros, CompilerOptions,
  OutputFilter, Compiler, Project, BaseBuildManager;
  
type

  { TBuildManager }

  TBuildManager = class(TBaseBuildManager)
  private
    CurrentParsedCompilerOption: TParsedCompilerOptions;
    FScanningCompilerDisabled: boolean;
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
    function OnRunCompilerWithOptions(ExtTool: TIDEExternalToolOptions;
                           CompOptions: TBaseCompilerOptions): TModalResult;
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
    procedure SetupInputHistories;

    function GetTargetOS(UseCache: boolean): string; override;
    function GetTargetCPU(UseCache: boolean): string; override;
    function GetLCLWidgetType(UseCache: boolean): string; override;
    function GetRunCommandLine: string; override;

    function GetProjectPublishDir: string; override;
    function GetProjectTargetFilename: string; override;
    function GetProjectUsesAppBundle: Boolean; override;
    function GetTestProjectFilename: string; override;
    function GetTestUnitFilename(AnUnitInfo: TUnitInfo): string; override;
    function GetTestBuildDirectory: string; override;
    function IsTestUnitFilename(const AFilename: string): boolean; override;
    function GetTargetUnitFilename(AnUnitInfo: TUnitInfo): string; override;

    procedure GetFPCCompilerParamsForEnvironmentTest(out Params: string);
    procedure RescanCompilerDefines(OnlyIfCompilerChanged: boolean);
    property ScanningCompilerDisabled: boolean read FScanningCompilerDisabled
                                              write FScanningCompilerDisabled;

    function CheckAmbiguousSources(const AFilename: string;
                                   Compiling: boolean): TModalResult; override;
    function DeleteAmbiguousFiles(const Filename:string
                                  ): TModalResult; override;
    function CheckUnitPathForAmbiguousPascalFiles(const BaseDir, TheUnitPath,
                                    CompiledExt, ContextDescription: string
                                    ): TModalResult; override;
    function BackupFile(const Filename: string): TModalResult; override;

    // methods for building
    procedure SetBuildTarget(const TargetOS, TargetCPU, LCLWidgetType: string);
    procedure SetBuildTargetIDE;
  end;
  
var
  MainBuildBoss: TBuildManager = nil;
  TheCompiler: TCompiler = nil;
  TheOutputFilter: TOutputFilter = nil;

implementation

type
  TUnitFile = record
    UnitName: string;
    Filename: string;
  end;
  PUnitFile = ^TUnitFile;

function CompareUnitFiles(UnitFile1, UnitFile2: PUnitFile): integer;
begin
  Result:=CompareIdentifierPtrs(Pointer(UnitFile1^.UnitName),
                                Pointer(UnitFile2^.UnitName));
end;

function CompareUnitNameAndUnitFile(UnitName: PChar;
  UnitFile: PUnitFile): integer;
begin
  Result:=CompareIdentifierPtrs(Pointer(UnitName),Pointer(UnitFile^.UnitName));
end;

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
  MainBuildBoss:=Self;
  inherited Create;

  OnBackupFileInteractive:=@BackupFile;
  RunCompilerWithOptions:=@OnRunCompilerWithOptions;
end;

destructor TBuildManager.Destroy;
begin
  OnBackupFileInteractive:=nil;
  FreeAndNil(InputHistories);
  
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

procedure TBuildManager.SetupInputHistories;
begin
  if InputHistories<>nil then exit;
  InputHistories:=TInputHistories.Create;
  with InputHistories do begin
    SetLazarusDefaultFilename;
    Load;
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
    Result:=LCLPlatformDirNames[GetDefaultLCLWidgetType];
end;

function TBuildManager.GetRunCommandLine: string;
var
  TargetFileName: string;
  
  function GetTargetFilename: String;
  begin
    Result := GetProjectTargetFilename;
    
    if GetProjectUsesAppBundle then
    begin
      // return command line to Application Bundle (darwin only)
      Result := ExtractFileNameWithoutExt(Result) + '.app';
    end;
  end;
  
begin
  if Project1.RunParameterOptions.UseLaunchingApplication then
    Result := Project1.RunParameterOptions.LaunchingApplicationPathPlusParams
  else
    Result := '';

  if Result=''
  then begin
    Result:=Project1.RunParameterOptions.CmdLineParams;
    if GlobalMacroList.SubstituteStr(Result) then begin
      TargetFileName:='"'+GetTargetFilename+'"';
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
        Result :=
          Project1.CompilerOptions.CreateTargetFilename(Project1.MainFilename);
      end;
    end;
  end;
end;

function TBuildManager.GetProjectUsesAppBundle: Boolean;
begin
  Result := (Project1.RunParameterOptions.HostApplicationFilename = '') and
    (GetTargetOS(False) = 'darwin') and Project1.UseAppBundle;
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

  function FoundSystemPPU: boolean;
  begin
    Result:=System.Pos('system ',CompilerUnitLinks)>0;
  end;

begin
  if ScanningCompilerDisabled then exit;
  GetFPCCompilerParamsForEnvironmentTest(CurOptions);
  {$IFDEF VerboseFPCSrcScan}
  debugln(['TMainIDE.RescanCompilerDefines A ',CurOptions,
    ' OnlyIfCompilerChanged=',OnlyIfCompilerChanged,
    ' Valid=',InputHistories.FPCConfigCache.Valid(true),
    ' ID=',InputHistories.FPCConfigCache.FindItem(CurOptions),
    ' CurDefinesCompilerFilename=',CurDefinesCompilerFilename,
    ' EnvCompilerFilename=',EnvironmentOptions.CompilerFilename,
    ' CurDefinesCompilerOptions="',CurDefinesCompilerOptions,'"',
    ' CurOptions="',CurOptions,'"',
    '']);
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
  SetupInputHistories;
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
    if not FoundSystemPPU then begin
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
    if not FoundSystemPPU then begin
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
        lisPleaseCheckTheFPCSourceDirectory,mtError,[mbOk]);
    end;
  end else begin
    IDEMessageDialog(lisCompilerError,lisPleaseCheckTheCompilerName,mtError,
                     [mbOk]);
  end;
end;

function TBuildManager.CheckAmbiguousSources(const AFilename: string;
  Compiling: boolean): TModalResult;

  function DeleteAmbiguousFile(const AmbiguousFilename: string): TModalResult;
  begin
    if not DeleteFile(AmbiguousFilename) then begin
      Result:=IDEMessageDialog(lisErrorDeletingFile,
       Format(lisUnableToDeleteAmbiguousFile, ['"', AmbiguousFilename, '"']),
       mtError,[mbOk,mbAbort]);
    end else
      Result:=mrOk;
  end;

  function RenameAmbiguousFile(const AmbiguousFilename: string): TModalResult;
  var
    NewFilename: string;
  begin
    NewFilename:=AmbiguousFilename+'.ambiguous';
    if not RenameFile(AmbiguousFilename,NewFilename) then
    begin
      Result:=IDEMessageDialog(lisErrorRenamingFile,
       Format(lisUnableToRenameAmbiguousFileTo, ['"', AmbiguousFilename, '"',
         #13, '"', NewFilename, '"']),
       mtError,[mbOk,mbAbort]);
    end else
      Result:=mrOk;
  end;

  function AddCompileWarning(const AmbiguousFilename: string): TModalResult;
  begin
    Result:=mrOk;
    if Compiling then begin
      TheOutputFilter.ReadConstLine(
        Format(lisWarningAmbiguousFileFoundSourceFileIs,
        ['"', AmbiguousFilename, '"', '"', AFilename, '"']), true);
    end;
  end;

  function CheckFile(const AmbiguousFilename: string): TModalResult;
  begin
    Result:=mrOk;
    if not FileExists(AmbiguousFilename) then exit;
    if Compiling then begin
      Result:=AddCompileWarning(AmbiguousFilename);
      exit;
    end;
    case EnvironmentOptions.AmbiguousFileAction of
    afaAsk:
      begin
        Result:=IDEMessageDialog(lisAmbiguousFileFound,
          Format(lisThereIsAFileWithTheSameNameAndASimilarExtension, [#13,
            AFilename, #13, AmbiguousFilename, #13, #13]),
          mtWarning,[mbYes,mbIgnore,mbAbort]);
        case Result of
        mrYes:    Result:=DeleteAmbiguousFile(AmbiguousFilename);
        mrIgnore: Result:=mrOk;
        end;
      end;

    afaAutoDelete:
      Result:=DeleteAmbiguousFile(AmbiguousFilename);

    afaAutoRename:
      Result:=RenameAmbiguousFile(AmbiguousFilename);

    afaWarnOnCompile:
      Result:=AddCompileWarning(AmbiguousFilename);

    else
      Result:=mrOk;
    end;
  end;

var
  Ext, LowExt: string;
  i: integer;
begin
  Result:=mrOk;
  if EnvironmentOptions.AmbiguousFileAction=afaIgnore then exit;
  if (EnvironmentOptions.AmbiguousFileAction=afaWarnOnCompile)
  and not Compiling then exit;

  if FilenameIsPascalUnit(AFilename) then begin
    Ext:=ExtractFileExt(AFilename);
    LowExt:=lowercase(Ext);
    for i:=Low(PascalFileExt) to High(PascalFileExt) do begin
      if LowExt<>PascalFileExt[i] then begin
        Result:=CheckFile(ChangeFileExt(AFilename,PascalFileExt[i]));
        if Result<>mrOk then exit;
      end;
    end;
  end;
end;

function TBuildManager.DeleteAmbiguousFiles(const Filename: string
  ): TModalResult;
var
  ADirectory: String;
  FileInfo: TSearchRec;
  ShortFilename: String;
  CurFilename: String;
  IsPascalUnit: Boolean;
  UnitName: String;
begin
  Result:=mrOk;
  if EnvironmentOptions.AmbiguousFileAction=afaIgnore then exit;
  if EnvironmentOptions.AmbiguousFileAction
    in [afaAsk,afaAutoDelete,afaAutoRename]
  then begin
    ADirectory:=AppendPathDelim(ExtractFilePath(Filename));
    if SysUtils.FindFirst(ADirectory+GetAllFilesMask,faAnyFile,FileInfo)=0 then
    begin
      ShortFilename:=ExtractFileName(Filename);
      IsPascalUnit:=FilenameIsPascalUnit(ShortFilename);
      UnitName:=ExtractFilenameOnly(ShortFilename);
      repeat
        if (FileInfo.Name='.') or (FileInfo.Name='..')
        or (FileInfo.Name='')
        or ((FileInfo.Attr and faDirectory)<>0) then continue;
        if (ShortFilename=FileInfo.Name) then continue;
        if (AnsiCompareText(ShortFilename,FileInfo.Name)<>0)
        and ((not IsPascalUnit) or (not FilenameIsPascalUnit(FileInfo.Name))
           or (AnsiCompareText(UnitName,ExtractFilenameOnly(FileInfo.Name))<>0))
        then
          continue;

        CurFilename:=ADirectory+FileInfo.Name;
        if EnvironmentOptions.AmbiguousFileAction=afaAsk then begin
          if IDEMessageDialog(lisDeleteAmbiguousFile,
            Format(lisAmbiguousFileFoundThisFileCanBeMistakenWithDelete, ['"',
              CurFilename, '"', #13, '"', ShortFilename, '"', #13, #13]),
            mtConfirmation,[mbYes,mbNo])=mrNo
          then continue;
        end;
        if EnvironmentOptions.AmbiguousFileAction in [afaAutoDelete,afaAsk]
        then begin
          if not DeleteFile(CurFilename) then begin
            IDEMessageDialog(lisDeleteFileFailed,
              Format(lisPkgMangUnableToDeleteFile, ['"', CurFilename, '"']),
              mtError,[mbOk]);
          end;
        end else if EnvironmentOptions.AmbiguousFileAction=afaAutoRename then
        begin
          Result:=BackupFile(CurFilename);
          if Result=mrABort then exit;
          Result:=mrOk;
        end;
      until SysUtils.FindNext(FileInfo)<>0;
    end;
    FindClose(FileInfo);
  end;
end;

{-------------------------------------------------------------------------------
  function TBuildManager.CheckUnitPathForAmbiguousPascalFiles(
    const BaseDir, TheUnitPath, CompiledExt, ContextDescription: string
    ): TModalResult;

  Collect all pascal files and all compiled units in the unit path and check
  for ambiguous files. For example: doubles.
-------------------------------------------------------------------------------}
function TBuildManager.CheckUnitPathForAmbiguousPascalFiles(const BaseDir,
  TheUnitPath, CompiledExt, ContextDescription: string): TModalResult;

  procedure FreeUnitTree(var Tree: TAVLTree);
  var
    ANode: TAVLTreeNode;
    AnUnitFile: PUnitFile;
  begin
    if Tree<>nil then begin
      ANode:=Tree.FindLowest;
      while ANode<>nil do begin
        AnUnitFile:=PUnitFile(ANode.Data);
        Dispose(AnUnitFile);
        ANode:=Tree.FindSuccessor(ANode);
      end;
      Tree.Free;
      Tree:=nil;
    end;
  end;

var
  EndPos: Integer;
  StartPos: Integer;
  CurDir: String;
  FileInfo: TSearchRec;
  SourceUnitTree, CompiledUnitTree: TAVLTree;
  ANode: TAVLTreeNode;
  CurUnitName: String;
  CurFilename: String;
  AnUnitFile: PUnitFile;
  CurUnitTree: TAVLTree;
  FileInfoNeedClose: Boolean;
  UnitPath: String;
  IgnoreAll: Boolean;
begin
  Result:=mrOk;
  UnitPath:=TrimSearchPath(TheUnitPath,BaseDir);

  SourceUnitTree:=TAVLTree.Create(TListSortCompare(@CompareUnitFiles));
  CompiledUnitTree:=TAVLTree.Create(TListSortCompare(@CompareUnitFiles));
  FileInfoNeedClose:=false;
  try
    // collect all units (.pas, .pp, compiled units)
    EndPos:=1;
    while EndPos<=length(UnitPath) do begin
      StartPos:=EndPos;
      while (StartPos<=length(UnitPath)) and (UnitPath[StartPos]=';') do
        inc(StartPos);
      EndPos:=StartPos;
      while (EndPos<=length(UnitPath)) and (UnitPath[EndPos]<>';') do
        inc(EndPos);
      if EndPos>StartPos then begin
        CurDir:=AppendPathDelim(TrimFilename(copy(
                                             UnitPath,StartPos,EndPos-StartPos)));
        FileInfoNeedClose:=true;
        if SysUtils.FindFirst(CurDir+GetAllFilesMask,faAnyFile,FileInfo)=0 then begin
          IgnoreAll:=false;
          repeat
            if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
            or ((FileInfo.Attr and faDirectory)<>0) then continue;
            if FilenameIsPascalUnit(FileInfo.Name) then
              CurUnitTree:=SourceUnitTree
            else if (CompareFileExt(FileInfo.Name,CompiledExt,false)=0) then
              CurUnitTree:=CompiledUnitTree
            else
              continue;
            CurUnitName:=ExtractFilenameOnly(FileInfo.Name);
            if (CurUnitName='') or (not IsValidIdent(CurUnitName)) then
              continue;
            CurFilename:=CurDir+FileInfo.Name;
            //DebugLn(['TBuildManager.CheckUnitPathForAmbiguousPascalFiles ',CurUnitName,' ',CurFilename]);
            // check if unit already found
            ANode:=CurUnitTree.FindKey(PChar(CurUnitName),
                                 TListSortCompare(@CompareUnitNameAndUnitFile));
            if (ANode<>nil) and (not IgnoreAll) then begin
              DebugLn(['TBuildManager.CheckUnitPathForAmbiguousPascalFiles CurUnitName="',CurUnitName,'" CurFilename="',CurFilename,'" OtherUnitName="',PUnitFile(ANode.Data)^.UnitName,'" OtherFilename="',PUnitFile(ANode.Data)^.Filename,'"']);
              // pascal unit exists twice
              Result:=QuestionDlg(lisAmbiguousUnitFound2,
                Format(lisTheUnitExistsTwiceInTheUnitPathOfThe, [CurUnitName,
                  ContextDescription])
                +#13
                +#13
                +'1. "'+PUnitFile(ANode.Data)^.Filename+'"'#13
                +'2. "'+CurFilename+'"'#13
                +#13
                +lisHintCheckIfTwoPackagesContainAUnitWithTheSameName,
                mtWarning, [mrIgnore, mrYesToAll, lisIgnoreAll, mrAbort], 0);
              case Result of
              mrIgnore: ;
              mrYesToAll: IgnoreAll:=true;
              else exit;
              end;
            end;
            // add unit to tree
            New(AnUnitFile);
            AnUnitFile^.UnitName:=CurUnitName;
            AnUnitFile^.Filename:=CurFilename;
            CurUnitTree.Add(AnUnitFile);
          until SysUtils.FindNext(FileInfo)<>0;
        end;
        FindClose(FileInfo);
        FileInfoNeedClose:=false;
      end;
    end;
  finally
    // clean up
    if FileInfoNeedClose then FindClose(FileInfo);
    FreeUnitTree(SourceUnitTree);
    FreeUnitTree(CompiledUnitTree);
  end;
  Result:=mrOk;
end;

function TBuildManager.BackupFile(const Filename: string): TModalResult;
var BackupFilename, CounterFilename: string;
  AText,ACaption:string;
  BackupInfo: TBackupInfo;
  FilePath, FileNameOnly, FileExt, SubDir: string;
  i: integer;
  IsPartOfProject: boolean;
begin
  Result:=mrOk;
  if not (FileExists(Filename)) then exit;
  IsPartOfProject:=(Project1<>nil)
                  and (Project1.FindFile(Filename,[pfsfOnlyProjectFiles])<>nil);
  if IsPartOfProject then
    BackupInfo:=EnvironmentOptions.BackupInfoProjectFiles
  else
    BackupInfo:=EnvironmentOptions.BackupInfoOtherFiles;
  if (BackupInfo.BackupType=bakNone)
  or ((BackupInfo.BackupType=bakSameName) and (BackupInfo.SubDirectory='')) then
    exit;
  FilePath:=ExtractFilePath(Filename);
  FileExt:=ExtractFileExt(Filename);
  FileNameOnly:=ExtractFilenameOnly(Filename);
  if BackupInfo.SubDirectory<>'' then begin
    SubDir:=FilePath+BackupInfo.SubDirectory;
    repeat
      if not DirPathExists(SubDir) then begin
        if not CreateDir(SubDir) then begin
          Result:=IDEMessageDialog('Warning',
                   Format(lisUnableToCreateBackupDirectory, ['"',SubDir, '"'])
                   ,mtWarning,[mbAbort,mbRetry,mbIgnore]);
          if Result=mrAbort then exit;
          if Result=mrIgnore then Result:=mrOk;
        end;
      end;
    until Result<>mrRetry;
  end;
  if BackupInfo.BackupType in
     [bakSymbolInFront,bakSymbolBehind,bakUserDefinedAddExt,bakSameName] then
  begin
    case BackupInfo.BackupType of
      bakSymbolInFront:
        BackupFilename:=FileNameOnly+'.~'+copy(FileExt,2,length(FileExt)-1);
      bakSymbolBehind:
        BackupFilename:=FileNameOnly+FileExt+'~';
      bakUserDefinedAddExt:
        BackupFilename:=FileNameOnly+FileExt+'.'+BackupInfo.AdditionalExtension;
      bakSameName:
        BackupFilename:=FileNameOnly+FileExt;
    end;
    if BackupInfo.SubDirectory<>'' then
      BackupFilename:=SubDir+PathDelim+BackupFilename
    else
      BackupFilename:=FilePath+BackupFilename;
    // remove old backup file
    repeat
      if FileExists(BackupFilename) then begin
        if not DeleteFile(BackupFilename) then begin
          ACaption:=lisDeleteFileFailed;
          AText:=Format(lisUnableToRemoveOldBackupFile, ['"', BackupFilename,
            '"']);
          Result:=IDEMessageDialog(ACaption,AText,mtError,
                                   [mbAbort,mbRetry,mbIgnore]);
          if Result=mrAbort then exit;
          if Result=mrIgnore then Result:=mrOk;
        end;
      end;
    until Result<>mrRetry;
  end else begin
    // backup with counter
    if BackupInfo.SubDirectory<>'' then
      BackupFilename:=SubDir+PathDelim+FileNameOnly+FileExt+';'
    else
      BackupFilename:=Filename+';';
    if BackupInfo.MaxCounter<=0 then begin
      // search first non existing backup filename
      i:=1;
      while FileExists(BackupFilename+IntToStr(i)) do inc(i);
      BackupFilename:=BackupFilename+IntToStr(i);
    end else begin
      // rename all backup files (increase number)
      i:=1;
      while FileExists(BackupFilename+IntToStr(i))
      and (i<=BackupInfo.MaxCounter) do inc(i);
      if i>BackupInfo.MaxCounter then begin
        dec(i);
        CounterFilename:=BackupFilename+IntToStr(BackupInfo.MaxCounter);
        // remove old backup file
        repeat
          if FileExists(CounterFilename) then begin
            if not DeleteFile(CounterFilename) then begin
              ACaption:=lisDeleteFileFailed;
              AText:=Format(lisUnableToRemoveOldBackupFile, ['"',
                CounterFilename, '"']);
              Result:=MessageDlg(ACaption,AText,mtError,
                                 [mbAbort,mbRetry,mbIgnore],0);
              if Result=mrAbort then exit;
              if Result=mrIgnore then Result:=mrOk;
            end;
          end;
        until Result<>mrRetry;
      end;
      // rename all old backup files
      dec(i);
      while i>=1 do begin
        repeat
          if not RenameFile(BackupFilename+IntToStr(i),
             BackupFilename+IntToStr(i+1)) then
          begin
            ACaption:=lisRenameFileFailed;
            AText:=Format(lisUnableToRenameFileTo, ['"', BackupFilename+IntToStr
              (i), '"', '"', BackupFilename+IntToStr(i+1), '"']);
            Result:=MessageDlg(ACaption,AText,mtError,
                               [mbAbort,mbRetry,mbIgnore],0);
            if Result=mrAbort then exit;
            if Result=mrIgnore then Result:=mrOk;
          end;
        until Result<>mrRetry;
        dec(i);
      end;
      BackupFilename:=BackupFilename+'1';
    end;
  end;
  // backup file
  repeat
    if not IDEProcs.BackupFile(Filename,BackupFilename) then begin
      ACaption:=lisBackupFileFailed;
      AText:=Format(lisUnableToBackupFileTo, ['"', Filename, '"', '"',
        BackupFilename, '"']);
      Result:=IDEMessageDialog(ACaption,AText,mterror,[mbabort,mbretry,mbignore]);
      if Result=mrAbort then exit;
      if Result=mrIgnore then Result:=mrOk;
    end;
  until Result<>mrRetry;
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
    if SysUtils.CompareText(Param,'SrcPath')=0 then
      Result:=Project1.CompilerOptions.GetSrcPath(false)
    else if SysUtils.CompareText(Param,'IncPath')=0 then
      Result:=Project1.CompilerOptions.GetIncludePath(false)
    else if SysUtils.CompareText(Param,'UnitPath')=0 then
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
    FuncData^.Result:=
                 Project1.CompilerOptions.GetIncludePath(false,coptParsed,true);
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

function TBuildManager.OnRunCompilerWithOptions(
  ExtTool: TIDEExternalToolOptions; CompOptions: TBaseCompilerOptions
  ): TModalResult;
begin
  if SourceEditorWindow<>nil then
    SourceEditorWindow.ClearErrorLines;
  Result:=EnvironmentOptions.ExternalTools.Run(ExtTool,GlobalMacroList,
                                               nil,CompOptions);
  if LazarusIDE<>nil then
    LazarusIDE.DoCheckFilesOnDisk;
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
  NewLCLWidgetSet:=LCLPlatformDirNames[MiscellaneousOptions.BuildLazOpts.LCLPlatform];
  if (NewTargetOS='') or (NewTargetOS='default') then
    NewTargetOS:=GetDefaultTargetOS;
  if (NewTargetCPU='') or (NewTargetCPU='default') then
    NewTargetCPU:=GetDefaultTargetCPU;
  SetBuildTarget(NewTargetOS,NewTargetCPU,NewLCLWidgetSet);
end;

end.

