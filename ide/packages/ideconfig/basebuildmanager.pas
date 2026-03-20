{
 /***************************************************************************
                            basebuildmanager.pas
                            --------------------
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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit BaseBuildManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, StrUtils, System.UITypes,
  // LazUtils
  LazStringUtils, LazFileUtils, LazUtilities, LazUTF8, LazVersion, FPCAdds,
  LazLoggerBase,
  // CodeTools
  CodeToolManager, DefineTemplates, CodeCache, BasicCodeTools, FileProcs, LinkScanner,
  // BuildIntf
  ProjectIntf, CompOptsIntf, MacroIntf, MacroDefIntf, IDEOptionsIntf,
  // IdeConfig
  LazConf, TransferMacros, ParsedCompilerOpts, CompilerOptions, EnvironmentOpts,
  IdeConfStrConsts;

type

  { TMacroConfig }

  TMacroConfig = class(TComponent)
  private
    FProjectNameSpace: string;
    FProjectNameSpaceCode: TCodeBuffer;
    FProjectNameSpaceCodeChgStep: integer;
    // cache
    FFPCompilerFilename: string;
    FFPCompilerFilenameStamp: Integer;
    // Macro FPCVer
    FFPCVer: string;
    FFPC_FULLVERSION: integer;
    FFPCVerChangeStamp: integer;
    // Macro InstantFPCCache
    FMacroInstantFPCCache: string;
    FMacroInstantFPCCacheValid: boolean;
    procedure MacroSubstitution(TheMacro: TTransferMacro;
                                const MacroName: string; var s: string;
                                const {%H-}Data: PtrInt; var Handled, {%H-}Abort: boolean;
                                {%H-}Depth: integer);
    function SubstituteCompilerOption({%H-}Options: TParsedCompilerOptions;
                                      const UnparsedValue: string;
                                      PlatformIndependent: boolean): string;
    function CTMacroFuncProjectNamespaces(Data: Pointer): boolean;
    function CTMacroFuncProjectUnitPath(Data: Pointer): boolean;
    function CTMacroFuncProjectIncPath(Data: Pointer): boolean;
    function CTMacroFuncProjectSrcPath(Data: Pointer): boolean;
    function GetProjectDefaultNamespace: string; // read .lpr file

    function MacroFuncBuildMode(const {%H-}Param: string; const {%H-}Data: PtrInt;
                             var {%H-}Abort: boolean): string;
    function MacroFuncEnv(const Param: string; const {%H-}Data: PtrInt;
                          var {%H-}Abort: boolean): string;
    function MacroFuncCompPath(const {%H-}s:string; const {%H-}Data: PtrInt;
                               var {%H-}Abort: boolean): string;
    function MacroFuncFPCMsgFile(const {%H-}Param: string; const {%H-}Data: PtrInt;
                          var {%H-}Abort: boolean): string;
    function MacroFuncFPCTarget(const {%H-}Param: string; const Data: PtrInt;
                               var {%H-}Abort: boolean): string;
    function MacroFuncFPCVer(const {%H-}Param: string; const {%H-}Data: PtrInt;
                             var {%H-}Abort: boolean): string;
    function MacroFuncFPC_FULLVERSION(const {%H-}Param: string; const {%H-}Data: PtrInt;
                             var {%H-}Abort: boolean): string;
    function MacroFuncLCLWidgetType(const {%H-}Param: string; const Data: PtrInt;
                                    var {%H-}Abort: boolean): string;
    function MacroFuncLazVer(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                    var {%H-}Abort: boolean): string;
    function MacroFuncMake(const {%H-}Param: string; const {%H-}Data: PtrInt;
                           var {%H-}Abort: boolean): string;// make utility
    function MacroFuncMakeExe(const Filename: string; const {%H-}Data: PtrInt;
                              var {%H-}Abort: boolean): string;
    function MacroFuncMakeLib(const Filename: string; const {%H-}Data: PtrInt;
                              var {%H-}Abort: boolean): string;
    function MacroFuncInstantFPCCache(const {%H-}Param: string; const {%H-}Data: PtrInt;
                           var {%H-}Abort: boolean): string;// path of the instantfpc cache
    function MacroFuncParams(const {%H-}Param: string; const {%H-}Data: PtrInt;
                             var {%H-}Abort: boolean): string;
    function MacroFuncProject(const Param: string; const {%H-}Data: PtrInt;
                              var {%H-}Abort: boolean): string;
    function MacroFuncProjFile(const {%H-}Param: string; const {%H-}Data: PtrInt;
                               var {%H-}Abort: boolean): string;
    function MacroFuncProjIncPath(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                  var {%H-}Abort: boolean): string;
    function MacroFuncProjNamespaces(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                   var {%H-}Abort: boolean): string;
    function MacroFuncProjOutDir(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                 var {%H-}Abort: boolean): string;
    function MacroFuncProjPath(const {%H-}Param: string; const {%H-}Data: PtrInt;
                               var {%H-}Abort: boolean): string;
    function MacroFuncProjSrcPath(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                  var {%H-}Abort: boolean): string;
    function MacroFuncProjUnitPath(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                   var {%H-}Abort: boolean): string;
    function MacroFuncRunCmdLine(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                 var {%H-}Abort: boolean): string;
    function MacroFuncSrcOS(const {%H-}Param: string; const Data: PtrInt;
                            var {%H-}Abort: boolean): string;
    function MacroFuncSubtarget(const {%H-}Param: string; const Data: PtrInt;
                                var {%H-}Abort: boolean): string;
    function MacroFuncTargetCmdLine(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                    var {%H-}Abort: boolean): string;
    function MacroFuncTargetCPU(const {%H-}Param: string; const Data: PtrInt;
                                var {%H-}Abort: boolean): string;
    function MacroFuncTargetFile(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                 var {%H-}Abort: boolean): string;
    function MacroFuncTargetOS(const {%H-}Param: string; const Data: PtrInt;
                               var {%H-}Abort: boolean): string;
    function MacroFuncOutputFile(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                 var {%H-}Abort: boolean): string;
    function MacroFuncPrimaryConfigPath(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                        var {%H-}Abort: boolean): string;
    function MacroFuncSecondaryConfigPath(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                          var {%H-}Abort: boolean): string;
    function MacroFuncFallbackOutputRoot(const {%H-}Param: string; const {%H-}Data: PtrInt;
                                         var {%H-}Abort: boolean): string;
  protected
    FBuildTarget: TLazProject;
    // current target
    fLCLWidgetType: string;
    fSubtarget: string;
    fTargetCPU: string;
    fTargetOS: string;
    function GetActiveBuildModeName: string;
    function GetTargetFilename: String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetCompilerFilename: string;
    function GetFPCompilerFilename: string;
    function GetLCLWidgetType: string;
    function GetProjectTargetFilename(aProject: TLazProject): string;
    function GetProjectUsesAppBundle: Boolean;
    function GetRunCommandLine: string;
    function GetSubtarget: string;
    function GetTargetCPU: string;
    function GetTargetOS: string;
    procedure SetupTransferMacros; virtual;
    procedure TranslateMacros;
  end;

  { TBaseBuildManager }

  TBaseBuildManager = class(TMacroConfig)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // methods for building IDE (will be changed when project groups are there)
    procedure SetBuildTargetProject1; virtual; abstract;
    procedure SetBuildTargetIDE(aQuiet: boolean = false); virtual; abstract;
    function BuildTargetIDEIsDefault: boolean; virtual; abstract;

    function GetBuildMacroOverride(const MacroName: string): string; virtual; abstract;
    function GetBuildMacroOverrides: TStrings; virtual; abstract;
    function GetRunWorkingDir: string; virtual; abstract;
    procedure WriteDebug_RunCommandLine; virtual; abstract;

    function GetFPCFrontEndOptions: string; virtual; abstract;
    function GetProjectPublishDir: string; virtual; abstract;
    function GetTestUnitFilename(AnUnitInfo: TLazProjectFile): string; virtual; abstract;
    function GetTestBuildDirectory: string; virtual; abstract;
    function IsTestUnitFilename(const AFilename: string): boolean; virtual; abstract;
    function GetTargetUnitFilename(AnUnitInfo: TLazProjectFile): string; virtual; abstract;

    procedure RescanCompilerDefines(ResetBuildTarget, ClearCaches,
                                    WaitTillDone, Quiet: boolean); virtual; abstract;
    function CompilerOnDiskChanged: boolean; virtual; abstract;

    function CheckAmbiguousSources(const AFilename: string;
                                   Compiling: boolean): TModalResult; virtual; abstract;
    function DeleteAmbiguousFiles(const Filename:string
                                  ): TModalResult; virtual; abstract;
    function CheckUnitPathForAmbiguousPascalFiles(const BaseDir, TheUnitPath,
                                    CompiledExt, ContextDescription: string
                                    ): TModalResult; virtual; abstract;
    function CreateProjectApplicationBundle: Boolean; virtual; abstract;

    function BackupFileForWrite(const Filename: string): TModalResult; virtual; abstract;

    function UpdateProjectAutomaticFiles(TestDir: string): TModalResult; virtual; abstract;
  end;

const
  cInvalidCompiler = 'InvalidCompiler';
var
  BuildBoss: TBaseBuildManager = nil;


implementation

procedure BMLazConfMacroFunction(var s: string);
begin
  if not GlobalMacroList.SubstituteStr(s) then
    debugln(['BMLazConfMacroFunction failed "',s,'"']);
end;

{ TMacroConfig }

constructor TMacroConfig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EnvironmentOptions := TEnvironmentOptions.Create;
  IDEEnvironmentOptions := EnvironmentOptions;
  FFPCVerChangeStamp:=CTInvalidChangeStamp;
  FFPCompilerFilenameStamp:=CTInvalidChangeStamp;
  fTargetOS:=GetCompiledTargetOS;
  fTargetCPU:=GetCompiledTargetCPU;
  fLCLWidgetType:=GetLCLWidgetTypeName;
end;

destructor TMacroConfig.Destroy;
begin
  FreeAndNil(EnvironmentOptions);
  inherited Destroy;
end;

procedure TMacroConfig.MacroSubstitution(TheMacro: TTransferMacro;
  const MacroName: string; var s: string; const Data: PtrInt; var Handled,
  Abort: boolean; Depth: integer);
var
  xEnv: TStringDynArray;   // cache
  EnvStr, UpperMacroName: String;
  VarCnt, i: Integer;
  p: SizeInt;
begin
  SetLength(xEnv, 0);
  if TheMacro=nil then begin
    if s='' then begin
      // default: use uppercase environment variable
      VarCnt:=GetEnvironmentVariableCountUTF8;
      if length(xEnv)<>VarCnt then begin
        SetLength(xEnv,VarCnt);
        for i:=0 to VarCnt-1 do
          xEnv[i]:=GetEnvironmentStringUTF8(i+1);
      end;
      UpperMacroName:=UTF8UpperCase(MacroName);
      for i:=0 to VarCnt-1 do begin
        EnvStr:=xEnv[i];
        p:=Pos('=',EnvStr);
        if p<2 then continue;
        {$IFDEF Windows}
        if UTF8CompareText(UpperMacroName,LeftStr(EnvStr,p-1))=0 then
        {$ELSE}
        if (p-1=length(UpperMacroName)) and CompareMem(@UpperMacroName[1],@EnvStr[1],p-1) then
        {$ENDIF}
        begin
          Handled:=true;
          s:=copy(EnvStr,p+1,length(EnvStr));
          exit;
        end;
      end;
    end;

    if ConsoleVerbosity>=0 then
      DebugLn('Warning: (lazarus) Macro not defined: "'+MacroName+'".');
    {$IFDEF VerboseMacroNotDefined}
    DumpStack;
    {$ENDIF}
    s:='';
    //IDEMessageDialog('Unknown Macro','Macro not defined: "'+s+'".',mtError,[mbAbort],0);
    Handled:=false;
    exit;
  end;
end;

function TMacroConfig.SubstituteCompilerOption(
  Options: TParsedCompilerOptions; const UnparsedValue: string;
  PlatformIndependent: boolean): string;
begin
  Result:=UnparsedValue;
  if PlatformIndependent then
    GlobalMacroList.SubstituteStr(Result,CompilerOptionMacroPlatformIndependent)
  else
    GlobalMacroList.SubstituteStr(Result,CompilerOptionMacroNormal);
end;

function TMacroConfig.CTMacroFuncProjectNamespaces(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  Result:=false;
  if LazProject1<>nil then begin
    FuncData^.Result:=MergeWithDelimiter(GetProjectDefaultNamespace,
                            LazProject1.LazCompilerOptions.GetNamespacesParsed(),';');
    Result:=true;
  end;
end;

function TMacroConfig.CTMacroFuncProjectUnitPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  Result:=false;
  if LazProject1<>nil then begin
    FuncData^.Result:=LazProject1.LazCompilerOptions.GetUnitPath(false);
    Result:=true;
  end;
end;

function TMacroConfig.CTMacroFuncProjectIncPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  Result:=false;
  if LazProject1<>nil then begin
    FuncData^.Result:=LazProject1.LazCompilerOptions.GetIncludePath(false,coptParsed,true);
    Result:=true;
  end;
end;

function TMacroConfig.CTMacroFuncProjectSrcPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  Result:=false;
  if LazProject1<>nil then begin
    FuncData^.Result:=LazProject1.LazCompilerOptions.GetSrcPath(false);
    Result:=true;
  end;
end;

function TMacroConfig.GetProjectDefaultNamespace: string;
// called by codetools *before* parsing
// Important: use only basiccodetools
var
  NameStart, NameEnd: Integer;
  Code: TCodeBuffer;
  ModuleType, ModuleName: string;
  NestedComments: boolean;
begin
  Result:='';
  if LazProject1=nil then exit;
  if not (pfMainUnitIsPascalSource in LazProject1.Flags) then exit;
  Code:=TCodeBuffer(LazProject1.GetMainUnitSource);
  if Code=nil then exit;
  if (Code<>FProjectNameSpaceCode) or (Code.ChangeStep<>FProjectNameSpaceCodeChgStep) then
  begin
    // read namespace
    FProjectNameSpace:='';
    FProjectNameSpaceCode:=Code;
    FProjectNameSpaceCodeChgStep:=Code.ChangeStep;
    NestedComments:=CompareText(LazProject1.LazCompilerOptions.SyntaxMode,'delphi')<>0;
    ModuleName:=FindModuleNameInSource(Code.Source,ModuleType,NameStart,
      NameEnd,NestedComments);
    FProjectNameSpace:=ChompDottedIdentifier(ModuleName);
  end;
  Result:=FProjectNameSpace;
end;

function TMacroConfig.GetActiveBuildModeName: string;
begin
  if FBuildTarget<>nil then
    Result:=FBuildTarget.ActiveBuildModeID
  else
    Result:='default';
end;

function TMacroConfig.GetTargetFilename: String;
begin
  Result := GetProjectTargetFilename(LazProject1);
  if GetProjectUsesAppBundle then
    // return command line to Application Bundle (darwin only)
    Result := ExtractFileNameWithoutExt(Result) + '.app';
end;

function TMacroConfig.MacroFuncBuildMode(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetActiveBuildModeName;
end;

function TMacroConfig.MacroFuncMakeExe(const Filename: string;
  const Data: PtrInt; var Abort: boolean): string;
var
  CommaPos: SizeInt;
  CurTargetOS: String;
  CurFilename: String;
begin
  CurFilename:=Filename;
  CommaPos:=System.Pos(',',CurFilename);
  CurTargetOS:='';
  if CommaPos>1 then begin
    // makeexe(targetos,filename)
    CurTargetOS:=UTF8LowerCase(LeftStr(CurFilename,CommaPos-1));
    if IsValidIdent(CurTargetOS) then begin
      if CurTargetOS='ide' then
        CurTargetOS:=GetCompiledTargetOS;
      System.Delete(CurFilename,1,CommaPos);
    end;
  end;
  if CurTargetOS='' then
    CurTargetOS:=GetTargetOS;
  Result:=MakeStandardExeFilename(CurTargetOS,CurFilename);
  //DebugLn('TMainIDE.MacroFuncMakeExe A ',Filename,' ',Result);
end;

function TMacroConfig.MacroFuncMakeLib(const Filename: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=MakeStandardLibFilename(GetTargetOS,Filename);
end;

function TMacroConfig.MacroFuncInstantFPCCache(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
var
  Prog: String;
  List: TStringList;
begin
  if not FMacroInstantFPCCacheValid then begin
    FMacroInstantFPCCache:='';
    FMacroInstantFPCCacheValid:=true;
    Prog:=FindDefaultExecutablePath('instantfpc'+GetExecutableExt);
    if Prog<>'' then begin
      List:=nil;
      try
        if ConsoleVerbosity>0 then
          debugln(['Hint: (lazarus) [TBaseConfigPath.MacroFuncInstantFPCCache] Exe=',Prog]);
        List:=RunTool(Prog,'--get-cache','',ConsoleVerbosity<1);
        if (List<>nil) and (List.Count>0) then
          FMacroInstantFPCCache:=List[0];
        List.Free;
      except
        on E: Exception do begin
          if ConsoleVerbosity>=0 then
            debugln(['Warning: (lazarus) [TBaseConfigPath.MacroFuncInstantFPCCache] error running '+Prog+': '+E.Message]);
        end;
      end;
    end;
    if ConsoleVerbosity>=1 then
      debugln(['Hint: (lazarus) [TBaseConfigPath.MacroFuncInstantFPCCache] CacheDir=',FMacroInstantFPCCache]);
  end;
  Result:=FMacroInstantFPCCache;
end;

function TMacroConfig.MacroFuncProject(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  if LazProject1<>nil then begin
    if SysUtils.CompareText(Param,'SrcPath')=0 then
      Result:=LazProject1.LazCompilerOptions.GetSrcPath(false)
    else if SysUtils.CompareText(Param,'IncPath')=0 then
      Result:=LazProject1.LazCompilerOptions.GetIncludePath(false)
    else if SysUtils.CompareText(Param,'Namespaces')=0 then
      Result:=LazProject1.LazCompilerOptions.GetNamespacesParsed
    else if SysUtils.CompareText(Param,'UnitPath')=0 then
      Result:=LazProject1.LazCompilerOptions.GetUnitPath(false)
    else if SysUtils.CompareText(Param,'InfoFile')=0 then
      Result:=LazProject1.ProjectInfoFile
    else if SysUtils.CompareText(Param,'InfoDir')=0 then
      Result:=ExtractFileDir(LazProject1.ProjectInfoFile)
    else if SysUtils.CompareText(Param,'Title')=0 then
      Result:=LazProject1.GetTitleOrName
    else if SysUtils.CompareText(Param,'TitleNew')=0 then begin
      Result:=LazProject1.GetTitleOrName;
      if Result = '' then
        Result := lisnewProject;
    end
    else if SysUtils.CompareText(Param,'OutputDir')=0 then
      Result:=LazProject1.LazCompilerOptions.GetUnitOutputDirectory(false)
    else begin
      Result:='<Invalid parameter for macro Project:'+Param+'>';
      if ConsoleVerbosity>=0 then
        debugln('Warning: (lazarus) [TMainIDE.MacroFuncProject]: ',Result);
    end;
  end else begin
    Result:='';
  end;
end;

function TMacroConfig.MacroFuncLCLWidgetType(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Data=CompilerOptionMacroPlatformIndependent then
    Result:='%(LCL_PLATFORM)'
  else
    Result:=GetLCLWidgetType;
end;

function TMacroConfig.MacroFuncLazVer(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  if Param = '' then exit(LazarusVersionStr)
  else if CompareText(Param, 'major') = 0 then result := ExtractDelimited(1, LazarusVersionStr, ['.'])
  else if CompareText(Param, 'minor') = 0 then result := ExtractDelimited(2, LazarusVersionStr, ['.'])
  else if CompareText(Param, 'rev'  ) = 0 then result := ExtractDelimited(3, LazarusVersionStr, ['.'])
  else if CompareText(Param, 'build') = 0 then result := ExtractDelimited(4, LazarusVersionStr, ['.'])
  else exit(''); // invalid parameter

  if result = '' then
    result := '0';
end;

function TMacroConfig.MacroFuncTargetCPU(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Data=CompilerOptionMacroPlatformIndependent then
    Result:='%(CPU_TARGET)'
  else if SysUtils.CompareText(Param,'IDE')=0 then
    Result:=GetCompiledTargetCPU
  else
    Result:=GetTargetCPU;
end;

function TMacroConfig.MacroFuncTargetOS(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Data=CompilerOptionMacroPlatformIndependent then
    Result:='%(OS_TARGET)'
  else if SysUtils.CompareText(Param,'IDE')=0 then
    Result:=GetCompiledTargetOS
  else
    Result:=GetTargetOS;
end;

function TMacroConfig.MacroFuncPrimaryConfigPath(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetPrimaryConfigPath;
end;

function TMacroConfig.MacroFuncSecondaryConfigPath(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetSecondaryConfigPath;
end;

function TMacroConfig.MacroFuncFallbackOutputRoot(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=AppendPathDelim(GetPrimaryConfigPath)+'lib';
end;

function TMacroConfig.MacroFuncSrcOS(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  if Data=CompilerOptionMacroPlatformIndependent then
    Result:='%(OS_TARGET)'
  else if Param<>'' then
    Result:=GetDefaultSrcOSForTargetOS(Param)
  else
    Result:=GetDefaultSrcOSForTargetOS(GetTargetOS);
end;

function TMacroConfig.MacroFuncSubtarget(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if Data=CompilerOptionMacroPlatformIndependent then
    Result:='%(FPC_SUBTARGET)'
  else if SysUtils.CompareText(Param,'IDE')=0 then
    Result:=''
  else
    Result:=GetSubtarget;
end;

function TMacroConfig.MacroFuncFPCVer(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;

  function TryTarget(CompilerFilename, TargetOS, TargetCPU: String): boolean;
  var
    ConfigCache: TPCTargetConfigCache;
  begin
    Result:=false;
    ConfigCache:=CodeToolBoss.CompilerDefinesCache.ConfigCaches.Find(
                                 CompilerFilename,'',TargetOS,TargetCPU,true);
    if ConfigCache=nil then exit;
    if ConfigCache.NeedsUpdate then begin
      // ask compiler
      if not ConfigCache.Update(CodeToolBoss.CompilerDefinesCache.TestFilename,'',nil)
      then
        exit;
    end;
    FFPCVer:=ConfigCache.GetFPCVer;
    FFPC_FULLVERSION:=ConfigCache.GetFPC_FULLVERSION;
    Result:=FFPC_FULLVERSION>0;
  end;

  procedure Compute;
  var
    TargetOS: String;
    TargetCPU: String;
    CompilerFilename, s: String;
  begin
    FFPC_FULLVERSION:=0;
    if OverrideFPCVer<>'' then begin
      FFPCVer:=OverrideFPCVer;
      FFPC_FULLVERSION:=FPCVersionToNumber(FFPCVer);
      exit;
    end;
    FFPCVer:={$I %FPCVERSION%};   // Version.Release.Patch
    if CodeToolBoss<>nil then begin
      // fetch the FPC version from the current compiler
      // Not from the fpc.exe, but from the real compiler
      CompilerFilename:=GetFPCompilerFilename;
      if not IsCTExecutable(CompilerFilename,s) then exit;

      // 1. try with project target OS/CPU
      TargetOS:=GetTargetOS;
      TargetCPU:=GetTargetCPU;
      if IsPas2jsTargetOS(TargetOS) or IsPas2jsTargetCPU(TargetCPU) then
        // skip
      else if TryTarget(CompilerFilename,TargetOS,TargetCPU) then
        exit;

      // 2. try with IDE target OS/CPU
      TargetOS:=GetCompiledTargetOS;
      TargetCPU:=GetCompiledTargetCPU;
      if TryTarget(CompilerFilename,TargetOS,TargetCPU) then exit;

      // 3. try with no target OS/CPU - using whatever the compiler supports
      TargetOS:='';
      TargetCPU:='';
      if TryTarget(CompilerFilename,TargetOS,TargetCPU) then exit;
    end;
    FFPC_FULLVERSION:=FPCVersionToNumber(FFPCVer);
  end;

begin
  if FFPCVerChangeStamp<>CompilerParseStamp then
  begin
    Compute;
    FFPCVerChangeStamp:=CompilerParseStamp;
    {$IFDEF VerboseFPCSrcScan}
    debugln(['TBaseConfigPath.MacroFuncFPCVer FPCVer=',FFPCVer,' FPC_FULLVERSION=',FFPC_FULLVERSION,' Stamp=',FFPCVerChangeStamp]);
    {$ENDIF}
  end;
  Result:=FFPCVer;
end;

function TMacroConfig.MacroFuncFPC_FULLVERSION(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if FFPCVerChangeStamp<>CompilerParseStamp then
    MacroFuncFPCVer(Param,Data,Abort);
  Result:=IntToStr(FFPC_FULLVERSION);
end;

function TMacroConfig.MacroFuncParams(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  if (LazProject1<>nil) and (LazProject1.RunParameters.GetActiveMode<>nil) then
    Result:=LazProject1.RunParameters.GetActiveMode.CmdLineParams
  else
    Result:='';
end;

function TMacroConfig.MacroFuncProjFile(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if LazProject1<>nil then
    Result:=LazProject1.MainFile.Filename
  else
    Result:='';
end;

function TMacroConfig.MacroFuncProjPath(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if LazProject1<>nil then
    Result:=LazProject1.Directory
  else
    Result:='';
end;

function TMacroConfig.MacroFuncTargetFile(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if LazProject1<>nil then
    Result:=GetProjectTargetFilename(LazProject1)
  else
    Result:='';
end;

function TMacroConfig.MacroFuncOutputFile(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if LazProject1<>nil then
    Result:=LazProject1.LazCompilerOptions.CreateTargetFilename
  else
    Result:='';
end;

function TMacroConfig.MacroFuncTargetCmdLine(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:='';
  if (LazProject1<>nil) then begin
    if (LazProject1.RunParameters.GetActiveMode<>nil) then
      Result:=LazProject1.RunParameters.GetActiveMode.CmdLineParams;
    if Result='' then
      Result:='"'+GetProjectTargetFilename(LazProject1)+'"'
    else
      Result:='"'+GetProjectTargetFilename(LazProject1)+'" '+Result;
  end;
end;

function TMacroConfig.MacroFuncRunCmdLine(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if LazProject1<>nil then
    Result:=GetRunCommandLine
  else
    Result:='';
end;

function TMacroConfig.MacroFuncProjUnitPath(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if LazProject1<>nil then
    Result:=LazProject1.LazCompilerOptions.GetUnitPath(false)
  else
    Result:='';
end;

function TMacroConfig.MacroFuncProjIncPath(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if LazProject1<>nil then
    Result:=LazProject1.LazCompilerOptions.GetIncludePath(false)
  else
    Result:='';
end;

function TMacroConfig.MacroFuncProjNamespaces(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if LazProject1<>nil then
  begin
    Result:=MergeWithDelimiter(GetProjectDefaultNamespace,
      LazProject1.LazCompilerOptions.GetNamespacesParsed,';');
  end
  else
    Result:='';
end;

function TMacroConfig.MacroFuncProjSrcPath(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if LazProject1<>nil then
    Result:=LazProject1.LazCompilerOptions.GetSrcPath(false)
  else
    Result:='';
end;

function TMacroConfig.MacroFuncProjOutDir(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if LazProject1<>nil then
    Result:=LazProject1.LazCompilerOptions.GetUnitOutputDirectory(false)
  else
    Result:='';
end;

function TMacroConfig.MacroFuncEnv(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  Result:=GetEnvironmentVariableUTF8(Param);
end;

function TMacroConfig.MacroFuncCompPath(const s: string; const Data: PtrInt;
  var Abort: boolean): string;
// if parameter is 'IDE' return the environment option
// otherwise use active project's compiler
begin
  Result:='';
  if CompareText(s,'IDE')<>0 then
    Result:=GetCompilerFilename;

  if Result='' then
    Result:=EnvironmentOptions.GetParsedCompilerFilename;
end;

function TMacroConfig.MacroFuncFPCMsgFile(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=EnvironmentOptions.GetParsedCompilerMessagesFilename;
end;

function TMacroConfig.MacroFuncFPCTarget(const Param: string;
  const Data: PtrInt; var Abort: boolean): string;
// works similar to FPC's macro $fpctarget:
// if subtarget is set:
//   targetcpu-targetos-subtarget
// else
//   targetcpu-targetos
// Supports same parameters as $TargetOS(param), i.e. $TargetOS(ide) returns
// the IDE's target, otherwise the project's target platform.
var
  TargetCPU, TargetOS, SubTarget: String;
begin
  Result:='';
  TargetCPU:=MacroFuncTargetCPU(Param,Data,Abort);
  if Abort then exit;
  TargetOS:=MacroFuncTargetOS(Param,Data,Abort);
  if Abort then exit;
  Result:=TargetCPU+'-'+TargetOS;
  if Data=CompilerOptionMacroPlatformIndependent then
    exit; // omit subtarget when creating a platform independent value

  SubTarget:=MacroFuncSubtarget(Param,Data,Abort);
  if Abort then exit;
  if SubTarget<>'' then
    Result+='-'+SubTarget;
end;

function TMacroConfig.MacroFuncMake(const Param: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  Result:=EnvironmentOptions.GetParsedMakeFilename;
  if Result='' then
    Result:=FindDefaultMakePath;
end;

function TMacroConfig.GetCompilerFilename: string;
var
  Opts: TBaseCompilerOptions;
begin
  Result:='';
  //debugln(['TBuildManager.GetCompilerFilename START FBuildTarget=',DbgSName(FBuildTarget)]);
  if FBuildTarget<>nil then
  begin
    Opts:=TBaseCompilerOptions(FBuildTarget.LazCompilerOptions);
    //debugln(['TBuildManager.GetCompilerFilename FBuildTarget=',DbgSName(FBuildTarget),' Path=',Opts.CompilerPath,' Build=',[crCompile,crBuild]*Opts.CompileReasons<>[],' Parsing=',Opts.ParsedOpts.Values[pcosCompilerPath].Parsing]);
    // Note: even if Opts.CompileReasons are disabled, the project compiler path is used by codetools
    if (Opts.CompilerPath<>'')
    and (not Opts.ParsedOpts.Values[pcosCompilerPath].Parsing) then
    begin
      Result:=Opts.CompilerPath;
      // the compiler filename is resolved twice, once for getting the default
      // compiler target OS/CPU and once with the real values.
      // For easier macro debugging, avoid this double resolve.
      if Result='' then
        // see below
      else if Result='$(CompPath)' then
        Result:=''
      else if (Pos('$',Result)<1) and (FilenameIsAbsolute(Result)) then
        Result:=TrimFilename(Result)
      else begin
        Result:=Opts.ParsedOpts.GetParsedValue(pcosCompilerPath); // Compiler Filename
        if Result='' then
        begin
          Result:=cInvalidCompiler;
          debugln(['Error: (lazarus) [TBuildManager.GetCompilerFilename] invalid compiler "',Opts.CompilerPath,'"']);
        end;
      end;
      //debugln(['TBuildManager.GetCompilerFilename project compiler="',Result,'"']);
    end;
  end;
  if Result='' then
    Result:=EnvironmentOptions.GetParsedCompilerFilename;
  //debugln(['TBuildManager.GetCompilerFilename END Result="',Result,'"']);
end;

function TMacroConfig.GetFPCompilerFilename: string;
var
  ErrMsg: string;
  Kind: TPascalCompiler;
begin
  if FFPCompilerFilenameStamp<>CompilerParseStamp then begin
    FFPCompilerFilename:=GetCompilerFilename;
    if (not IsCompilerExecutable(FFPCompilerFilename,ErrMsg,Kind,false)) or (ErrMsg<>'')
        or (Kind<>pcFPC) then
      FFPCompilerFilename:=EnvironmentOptions.GetParsedCompilerFilename;
    FFPCompilerFilenameStamp:=CompilerParseStamp;
  end;
  Result:=FFPCompilerFilename;
end;

function TMacroConfig.GetLCLWidgetType: string;
begin
  Result:=fLCLWidgetType;
end;

function TMacroConfig.GetProjectTargetFilename(aProject: TLazProject): string;
var
  AMode: TAbstractRunParamsOptionsMode;
begin
  Result:='';
  if aProject=nil then exit;
  AMode := aProject.RunParameters.GetActiveMode;
  if AMode<>nil then
    Result:=AMode.HostApplicationFilename;
  GlobalMacroList.SubstituteStr(Result);
  if (Result='') and (aProject.MainFileID>=0) then
    Result := aProject.LazCompilerOptions.CreateTargetFilename;
end;

function TMacroConfig.GetProjectUsesAppBundle: Boolean;
begin
  Result := (LazProject1<>nil)
    and ( (LazProject1.RunParameters.GetActiveMode=nil)
       or (LazProject1.RunParameters.GetActiveMode.HostApplicationFilename = ''))
    and (GetTargetOS = 'darwin') and LazProject1.UseAppBundle;
end;

function TMacroConfig.GetRunCommandLine: string;
var
  TargetFilename: string;
  AMode: TAbstractRunParamsOptionsMode; //TRunParamsOptionsMode;
  Opts: TBaseCompilerOptions;
begin
  Result := '';
  if LazProject1=nil then exit;
  AMode := LazProject1.RunParameters.GetActiveMode;
  if (AMode<>nil) and AMode.UseLaunchingApplication then
    Result := AMode.LaunchingApplicationPathPlusParams;

  if Result='' then
  begin
    if AMode<>nil then
      Result := AMode.CmdLineParams;
    if GlobalMacroList.SubstituteStr(Result) then
    begin
      TargetFilename := GetTargetFilename;
      if (TargetFilename <> '')
      and (TargetFilename[Length(TargetFilename)] in AllowDirectorySeparators) then
      begin
        Opts:=TBaseCompilerOptions(LazProject1.LazCompilerOptions);
        TargetFilename += ExtractFileNameOnly(Opts.GetDefaultMainSourceFileName);
      end;
      TargetFilename := '"'+TargetFilename+'"';
      if Result='' then
        Result:=TargetFilename
      else
        Result:=TargetFilename+' '+Result;
    end else
      Result:='';
  end else begin
    if not GlobalMacroList.SubstituteStr(Result) then Result:='';
  end;
end;

function TMacroConfig.GetSubtarget: string;
begin
  Result:=fSubtarget;
end;

function TMacroConfig.GetTargetCPU: string;
begin
  Result:=fTargetCPU;
  //debugln(['TBuildManager.GetTargetCPU ',Result]);
end;

function TMacroConfig.GetTargetOS: string;
begin
  Result:=fTargetOS;
end;

procedure TMacroConfig.SetupTransferMacros;
begin
  LazConfMacroFunc:=@BMLazConfMacroFunction;
  GlobalMacroList:=TTransferMacroList.Create;
  GlobalMacroList.OnSubstitution:=@MacroSubstitution;
  IDEMacros:=TLazIDEMacros.Create;
  OnParseString:=@SubstituteCompilerOption;

  // ToDo: Move IdeTransferMacros to IdeConfig package.
  //TIdeTransferMarcros.InitMacros(GlobalMacroList);

  // project
  GlobalMacroList.Add(TTransferMacro.Create('Project','',
                      lisProjectMacroProperties,@MacroFuncProject,[]));
  GlobalMacroList.Add(TTransferMacro.Create('BuildMode','',
                      lisNameOfActiveBuildMode, @MacroFuncBuildMode, []));
  GlobalMacroList.Add(TTransferMacro.Create('LCLWidgetType','',
                      lisLCLWidgetType,@MacroFuncLCLWidgetType,[]));
  GlobalMacroList.Add(TTransferMacro.Create('TargetCPU','',
                      lisTargetCPU,@MacroFuncTargetCPU,[]));
  GlobalMacroList.Add(TTransferMacro.Create('TargetOS','',
                      lisTargetOS,@MacroFuncTargetOS,[]));
  GlobalMacroList.Add(TTransferMacro.Create('Subtarget','',
                      lisTargetCPU,@MacroFuncSubtarget,[]));
  GlobalMacroList.Add(TTransferMacro.Create('SrcOS','',
                      lisSrcOS,@MacroFuncSrcOS,[]));
  GlobalMacroList.Add(TTransferMacro.Create('CompPath','',
                      lisCompilerFilename,@MacroFuncCompPath,[]));
  GlobalMacroList.Add(TTransferMacro.Create('FPCTarget','',
                      lisShortFormOfTargetCPUParamTargetOSParamSubTargetPar,
                      @MacroFuncFPCTarget, []));
  GlobalMacroList.Add(TTransferMacro.Create('FPCVer','',
                      lisFPCVersionEG222, @MacroFuncFPCVer, []));
  GlobalMacroList.Add(TTransferMacro.Create('FPC_FULLVERSION','',
                      lisFPCFullVersionEG20701, @MacroFuncFPC_FULLVERSION, []));
  GlobalMacroList.Add(TTransferMacro.Create('FPCMsgFile','',
                      dlgFilterFPCMessageFile, @MacroFuncFPCMsgFile, []));
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
  GlobalMacroList.Add(TTransferMacro.Create('OutputFile','',
                      lisOutputFilenameOfProject,@MacroFuncOutputFile,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjNamespaces','',
                      lisProjectNamespaces,@MacroFuncProjNamespaces,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjOutDir','',
                      lisProjectOutDir,@MacroFuncProjOutDir,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjUnitPath','',
                      lisProjectUnitPath,@MacroFuncProjUnitPath,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjIncPath','',
                      lisProjectIncPath,@MacroFuncProjIncPath,[]));
  GlobalMacroList.Add(TTransferMacro.Create('ProjSrcPath','',
                      lisProjectSrcPath,@MacroFuncProjSrcPath,[]));
  GlobalMacroList.Add(TTransferMacro.Create('Env','',
                      lisEnvironmentVariableNameAsParameter, @MacroFuncEnv, []));
  GlobalMacroList.Add(TTransferMacro.Create('MakeExe','',
                      lisMakeExe,@MacroFuncMakeExe,[]));
  GlobalMacroList.Add(TTransferMacro.Create('MakeLib','',
                      lisMakeExe,@MacroFuncMakeLib,[]));
  GlobalMacroList.Add(TTransferMacro.Create('Make','',
                      lisPathOfTheMakeUtility, @MacroFuncMake, []));
  GlobalMacroList.Add(TTransferMacro.Create('InstantFPCCache','',
                      lisPathOfTheInstantfpcCache, @MacroFuncInstantFPCCache, []));
  GlobalMacroList.Add(TTransferMacro.Create('PrimaryConfigPath','',
                      lisPrimaryConfigPath, @MacroFuncPrimaryConfigPath, []));
  GlobalMacroList.Add(TTransferMacro.Create('SecondaryConfigPath','',
                      lisSecondaryConfigPath, @MacroFuncSecondaryConfigPath, []));
  GlobalMacroList.Add(TTransferMacro.Create('FallbackOutputRoot','',
                      lisSecondaryConfigPath, @MacroFuncFallbackOutputRoot, []));
  GlobalMacroList.Add(TTransferMacro.Create('LAZVer','',
                      lisLAZVer, @MacroFuncLazVer, []));

  // codetools macro functions
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECTNAMESPACES',nil,@CTMacroFuncProjectNamespaces);
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECTUNITPATH',nil,@CTMacroFuncProjectUnitPath);
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECTINCPATH',nil,@CTMacroFuncProjectIncPath);
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECTSRCPATH',nil,@CTMacroFuncProjectSrcPath);
end;

procedure TMacroConfig.TranslateMacros;

  procedure tr(const MacroName, Description: string);
  var
    Macro: TTransferMacro;
  begin
    Macro:=GlobalMacroList.FindByName(MacroName);
    if Macro=nil then exit;
    Macro.Description:=RemoveAmpersands(Description);
  end;

begin
  tr('Project',lisProjectMacroProperties);
  tr('BuildMode',lisNameOfActiveBuildMode);
  tr('BuildModeCaption',lisCaptionOfActiveBuildMode);
  tr('LCLWidgetType',lisLCLWidgetType);
  tr('TargetCPU',lisTargetCPU);
  tr('TargetOS',lisTargetOS);
  tr('Subtarget',lisSubtarget);
  tr('SrcOS',lisSrcOS);
  tr('FPCTarget',lisShortFormOfTargetCPUParamTargetOSParamSubTargetPar);
  tr('FPCVer',lisFPCVersionEG222);
  tr('LAZVer',lisLAZVer);
  tr('FPC_FULLVERSION',lisFPCFullVersionEG20701);
  tr('Params',lisCommandLineParamsOfProgram);
  tr('ProjFile',lisProjectFilename);
  tr('ProjPath',lisProjectDirectory);
  tr('TargetFile',lisTargetFilenameOfProject);
  tr('TargetCmdLine',lisTargetFilenamePlusParams);
  tr('RunCmdLine',lisLaunchingCmdLine);
  tr('OutputFile',lisOutputFilenameOfProject);
  tr('ProjPublishDir',lisPublishProjDir);
  tr('ProjNamespaces',lisProjectNamespaces);
  tr('ProjUnitPath',lisProjectUnitPath);
  tr('ProjIncPath',lisProjectIncPath);
  tr('ProjSrcPath',lisProjectSrcPath);
  tr('ProjOutDir',lisProjectOutDir);
  tr('ProjVer',lisProjectVer);
  tr('Env',lisEnvironmentVariableNameAsParameter);
  tr('FPCMsgFile',dlgFilterFPCMessageFile);
  tr('MakeExe',lisMakeExe);
  tr('MakeLib',lisMakeExe);
  tr('Make',lisPathOfTheMakeUtility);
  tr('InstantFPCCache',lisPathOfTheInstantfpcCache);
  tr('IDEBuildOptions',lisIDEBuildOptions);
  tr('PrimaryConfigPath',lisPrimaryConfigPath);
  tr('SecondaryConfigPath',lisSecondaryConfigPath);
  tr('FallbackOutputRoot',lisSecondaryConfigPath);
  tr('CompPath',lisCompilerFilename);
  // These macros are handled elsewhere but translated here.
  tr('FPCSrcDir',lisFreePascalSourceDirectory);
  tr('LazarusDir',lisLazarusDirectory);
  tr('ExeExt',lisFileExtensionOfPrograms);
  tr('LanguageID',lisLazarusLanguageID);
  tr('LanguageName',lisLazarusLanguageName);
  tr('TestDir',lisTestDirectory);
  tr('ConfDir',lisConfigDirectory);
  tr('Home',lisUserSHomeDirectory);
  tr('Ext',lisTMFunctionExtractFileExtension);
  tr('Path',lisTMFunctionExtractFilePath);
  tr('Name',lisTMFunctionExtractFileNameExtension);
  tr('NameOnly',lisTMFunctionExtractFileNameOnly);
  tr('MakeDir',lisTMFunctionAppendPathDelimiter);
  tr('MakeFile',lisTMFunctionChompPathDelimiter);
  tr('EncloseBracket', lisTMFunctionEncloseBrackets);
end;

{ TBaseBuildManager }

constructor TBaseBuildManager.Create(AOwner: TComponent);
begin
  BuildBoss:=Self;
  inherited Create(AOwner);
end;

destructor TBaseBuildManager.Destroy;
begin
  inherited Destroy;
  BuildBoss:=nil;
end;

end.

