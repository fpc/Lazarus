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

 Code for building the Lazarus IDE. Split from unit BuildLazDialog.
}
unit IdeBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, System.UITypes,
  {$IFDEF Windows}
  Windows,
  {$ENDIF}
  // LazUtils
  FPCAdds, FileUtil, LazFileUtils, LazUTF8, LazLoggerBase, LazFileCache, LazVersion,
  // Codetools
  CodeToolManager,
  // BuildIntf
  LazMsgWorker, IDEExternToolIntf,
  // IdeConfig
  EnvironmentOpts, LazConf, TransferMacros, ParsedCompilerOpts,
  DialogProcs, ApplicationBundle, ModeMatrixOpts, MiscOptions, IdeConfStrConsts;

type
  TBuildLazarusFlag = (
    blfDontBuild,           // skip all building, only cleaning
    blfOnlyIDE,             // skip all but IDE (for example build IDE, but not packages, not lazbuild, ...)
    blfDontClean,           // ignore clean up option in profile
    blfUseMakeIDECfg,       // append @idemake.cfg
    blfBackupOldExe         // rename existing lazarus exe to lazarus.old
    );
  TBuildLazarusFlags = set of TBuildLazarusFlag;

  { TLazarusBuilder }

  TLazarusBuilder = class
  private
    fCompilerTargetCPU, fCompilerTargetOS: String;
    fExtraOptions: string;
    fMacros: TTransferMacroList;
    fOutputDirRedirected: boolean;
    fPackageOptions: string;
    fProfile: TBuildLazarusProfile;
    fProfileChanged: boolean;
    fTargetCPU: string;
    fTargetDir: string;
    fTargetFilename: string; // = fTargetDir + 'lazarus'+GetExecutableExt(fTargetOS)
    fTargetOS: string;
    fUnitOutDir: string;
    fUpdateRevInc: boolean;
    fWorkingDir: string;
    // Methods used by MakeLazarus :
    procedure ApplyCleanOnce;
    function CheckDirectoryWritable(Dir: string): boolean;
    procedure CleanDir(Dir: string; Recursive: boolean = true);
    procedure CleanLazarusSrcDir;
    procedure CheckRevisionInc;
    procedure RestoreBackup;
    // Methods used by SaveIDEMakeOptions :
    function BreakExtraOptions: string;
    // Methods used by CalcTargets :
    procedure AppendOption_IdeConfig;
    // This is used by CreateIDEMakeOptions and IsWriteProtected
    function CalcTargets(Flags: TBuildLazarusFlags): TModalResult;
    // Methods used by CreateIDEMakeOptions :
    function BackupExe(var aTargetFilename: string; const aTitle: string; Flags: TBuildLazarusFlags; AllowAltTargetFile: boolean): boolean;
    function CreateAppleBundle: TModalResult;
    procedure AppendExtraOption(const aOption: string; AutoQuote: boolean = True);
    // This is used by MakeLazarus and SaveIDEMakeOptions
    function PrepareTargetDir(Flags: TBuildLazarusFlags): TModalResult;
  public
    constructor Create;
    //function ShowConfigBuildLazDlg(AProfiles: TBuildLazarusProfiles;
    //                               ADisableCompilation: Boolean): TModalResult;
    function MakeLazarus(Profile: TBuildLazarusProfile; Flags: TBuildLazarusFlags): TModalResult;
    function MakeIDEUsingLazbuild(Clean: boolean): TModalResult;
    function IsWriteProtected(Profile: TBuildLazarusProfile): Boolean;
    function SaveIDEMakeOptions(Profile: TBuildLazarusProfile; Flags: TBuildLazarusFlags): TModalResult;
    function SearchMakeExe(Interactive: boolean): string;
  public
    property PackageOptions: string read fPackageOptions write fPackageOptions;
    property ProfileChanged: boolean read fProfileChanged write fProfileChanged;
  end;

//function GetMakeIDEConfigFilename: string;
//function GetBackupExeFilename(Filename: string): string;


implementation

const
  DefaultIDEMakeOptionFilename = 'idemake.cfg';

function GetMakeIDEConfigFilename: string;
begin
  Result:=AppendPathDelim(GetPrimaryConfigPath)+DefaultIDEMakeOptionFilename;
end;

function GetBackupExeFilename(Filename: string): string;
var
  Ext: String;
begin
  Ext:=ExtractFileExt(Filename);
  Result:=LeftStr(Filename,length(Filename)-length(Ext))+'.old'+Ext;
end;

{ TLazarusBuilder }

constructor TLazarusBuilder.Create;
begin
  fMacros:=GlobalMacroList;
end;

procedure TLazarusBuilder.ApplyCleanOnce;
begin
  if not fProfile.CleanOnce then exit;
  if fProfile.IdeBuildMode=bmBuild then exit;
  fProfile.IdeBuildMode:=bmBuild;
  fProfileChanged:=true;
end;

function TLazarusBuilder.CheckDirectoryWritable(Dir: string): boolean;
begin
  if DirectoryIsWritableCached(Dir) then exit(true);
  Result:=false;
  LazMessageWorker(lisBuildingLazarusFailed,
    Format(lisThisSetOfOptionsToBuildLazarusIsNotSupportedByThis,[LineEnding,Dir,LineEnding]),
    mtError,[mbCancel]);
end;

procedure TLazarusBuilder.CleanDir(Dir: string; Recursive: boolean = true);
var
  FileInfo: TSearchRec;
  Filename: TFilename;
begin
  Dir:=AppendPathDelim(TrimFilename(Dir));
  if FindFirstUTF8(Dir+AllFilesMask,faAnyFile,FileInfo)=0 then begin
    try
      repeat                                        //or (FileInfo.Name='.')
        if (FileInfo.Name='') or (FileInfo.Name='..') or (FileInfo.Name[1]='.') then
          continue;
        Filename:=Dir+FileInfo.Name;
        if faDirectory and FileInfo.Attr>0 then
        begin
          if Recursive then
            CleanDir(Filename)
        end
        else begin
          if FilenameExtIn(FileInfo.Name,['.ppu','.o','.rst','.rsj']) then begin
            if not DeleteFileUTF8(Filename) then
              debugln(['Error : (lazarus) Clean directory: failed to delete file "',Filename,'"']);
          end;
        end;
      until FindNextUTF8(FileInfo)<>0;
    finally
      FindCloseUTF8(FileInfo);
    end;
  end;
end;

procedure TLazarusBuilder.CleanLazarusSrcDir;
var
  s: String;
begin
  // clean all lazarus source directories
  // Note: Some installations put the fpc units into the lazarus directory
  //       => clean only the known directories
  CleanDir(fWorkingDir,false);
  CleanDir(fWorkingDir+PathDelim+'examples');
  CleanDir(fWorkingDir+PathDelim+'components');
  CleanDir(fWorkingDir+PathDelim+'units');
  CleanDir(fWorkingDir+PathDelim+'ide');
  CleanDir(fWorkingDir+PathDelim+'debugger');
  CleanDir(fWorkingDir+PathDelim+'designer');
  CleanDir(fWorkingDir+PathDelim+'packager');
  CleanDir(fWorkingDir+PathDelim+'converter');
  CleanDir(fWorkingDir+PathDelim+'lcl');
  CleanDir(fWorkingDir+PathDelim+'ideintf'); // from very old lazarus
  CleanDir(fWorkingDir+PathDelim+'tools');
  CleanDir(fWorkingDir+PathDelim+'test');

  // clean config directory
  CleanDir(AppendPathDelim(GetPrimaryConfigPath)+'units');

  // clean custom target directory
  if fProfile.TargetDirectory<>'' then begin
    s:=fProfile.GetParsedTargetDirectory(fMacros);
    if (s<>'') and DirPathExists(s) then
      CleanDir(s);
  end;
end;

procedure TLazarusBuilder.CheckRevisionInc;
var
  RevisionIncFile: String;
  sl: TStringList;
begin
  RevisionIncFile:=AppendPathDelim(EnvironmentOptions.GetParsedLazarusDirectory)+'ide'+PathDelim+'revision.inc';
  if not FileExistsUTF8(RevisionIncFile) then begin
    debugln(['Note: (lazarus) revision.inc file missing: ',RevisionIncFile]);
    sl:=TStringList.Create;
    sl.Add('// Created by lazbuild');
    sl.Add('const RevisionStr = '''+LazarusVersionStr+''';');
    try
      sl.SaveToFile(RevisionIncFile);
    except
      on E: Exception do begin
        debugln(['Warning: (lazarus) unable to write ',RevisionIncFile,': ',E.Message]);
      end;
    end;
    sl.Free;
  end;
end;

procedure TLazarusBuilder.RestoreBackup;
var
  BackupFilename: String;
begin
  if FileExistsUTF8(fTargetFilename) then begin
    if not DeleteFileUTF8(fTargetFilename) then begin
      debugln(['Error: (lazarus) Building IDE failed. Unable to delete "',fTargetFilename,'"']);
      exit;
    end;
  end;
  BackupFilename:=GetBackupExeFilename(fTargetFilename);
  if FileExistsUTF8(BackupFilename) then begin
    if not RenameFileUTF8(BackupFilename,fTargetFilename) then begin
      debugln(['Error: (lazarus) Building IDE failed. Unable to restore backup file "',BackupFilename,'" to "',fTargetFilename,'"']);
    end;
  end;
end;

function TLazarusBuilder.MakeLazarus(Profile: TBuildLazarusProfile;
  Flags: TBuildLazarusFlags): TModalResult;
var
  Tool: TAbstractExternalTool;
  Executable, Cmd: String;
  CmdLineParams: TStrings;
  EnvironmentOverrides: TStringList;

  procedure AddCmdLineParam(Param: string; ExecMacros: boolean);
  begin
    if Param='' then exit;
    if ExecMacros and (fMacros<>nil) then
      fMacros.SubstituteStr(Param);
    if Param<>'' then
      CmdLineParams.Add(Param);
  end;

  function Run(CurTitle: string): TModalResult;
  var
    Cmds: TStrings;
  begin
    if Pos(' ',Cmd)>0 then
    begin
      Cmds:=TStringList.Create;
      try
        SplitCmdLineParams(Cmd,Cmds);
        CmdLineParams.AddStrings(Cmds);
      finally
        Cmds.Free;
      end;
    end else
      CmdLineParams.Add(Cmd);
    Tool:=ExternalToolList.Add(CurTitle);
    Tool.Reference(Self,ClassName);
    try
      Tool.Data:=TIDEExternalToolData.Create(IDEToolCompileIDE,'lazarus',
        AppendPathDelim(EnvironmentOptions.GetParsedLazarusDirectory)+'lazarus.pp');
      Tool.FreeData:=true;
      Tool.Process.Executable:=Executable;
      Tool.AddParsers(SubToolFPC);
      Tool.AddParsers(SubToolMake);
      Tool.Process.CurrentDirectory:=fWorkingDir;
      Tool.EnvironmentOverrides:=EnvironmentOverrides;
      Tool.CmdLineParams:=MergeCmdLineParams(CmdLineParams);
      Tool.Execute;
      Tool.WaitForExit;
      if Tool.ErrorMessage='' then
        exit(mrOk)
      else
        exit(mrCancel);
    finally
      Tool.Release(Self);
    end;
  end;

var
  IdeBuildMode: TIdeBuildMode;
  s: String;
  DefaultTargetFilename: String;
begin
  // Get target files and directories.
  Result:=mrCancel;
  fProfile:=Profile;
  if CalcTargets(Flags)<>mrOk then exit;

  if Assigned(OnMainTitleChange) then
    OnMainTitleChange(Profile.Name); //LazarusIDE.MainBarSubTitle:=Profile.Name;
  IdeBuildMode:=Profile.IdeBuildMode;

  EnvironmentOverrides:=TStringList.Create;
  CmdLineParams:=TStringList.Create;
  Tool:=nil;
  try
    // setup external tool
    EnvironmentOverrides.Values['LCL_PLATFORM']:=LCLPlatformDirNames[Profile.TargetPlatform];
    EnvironmentOverrides.Values['LANG']:= 'en_US';
    s:=EnvironmentOptions.GetParsedCompilerFilename;
    if s<>'' then
      EnvironmentOverrides.Values['PP']:=s;

    Executable:=SearchMakeExe(true);

    // add -w option to print leaving/entering messages of "make"
    AddCmdLineParam('-w',false);
    // append target OS
    if fTargetOS<>fCompilerTargetOS then begin
      AddCmdLineParam('OS_TARGET='+fTargetOS,true);
      AddCmdLineParam('OS_SOURCE='+fTargetOS,true);
    end;
    // append target CPU
    if fTargetCPU<>fCompilerTargetCPU then begin
      AddCmdLineParam('CPU_TARGET='+fTargetCPU,true);
      AddCmdLineParam('CPU_SOURCE='+fTargetCPU,true);
    end;

    // create target directory and bundle
    Result:=PrepareTargetDir(Flags);
    if Result<>mrOk then exit;

    fWorkingDir:=EnvironmentOptions.GetParsedLazarusDirectory;

    // clean up
    if (IdeBuildMode<>bmBuild) and (not (blfDontClean in Flags)) then begin

      if not fOutputDirRedirected then begin
        // clean up Lazarus sources
        if not CheckDirectoryWritable(fWorkingDir) then exit(mrCancel);

        if (IdeBuildMode=bmCleanAllBuild) and (not (blfOnlyIDE in Flags)) then
          CleanLazarusSrcDir;

        // call make to clean up
        if (IdeBuildMode=bmCleanBuild) or (blfOnlyIDE in Flags) then
          Cmd:='cleanide'
        else
          Cmd:='cleanlaz';
        Result:=Run(lisCleanLazarusSource);
        if Result<>mrOk then exit;
      end;

      // when cleaning, always clean up fallback output directory too
      // Note: fallback = when lazarusdir is readonly
      if (IdeBuildMode=bmCleanAllBuild) and (not (blfOnlyIDE in Flags)) then
      begin
        // clean up fallback package output directories
        CleanDir(AppendPathDelim(GetPrimaryConfigPath)+'lib');
      end;
      // clean up fallback IDE output directory
      CleanDir(AppendPathDelim(GetPrimaryConfigPath)+'units');

      ApplyCleanOnce;
    end;

    // build IDE
    if not (blfDontBuild in Flags) then begin
      if blfDontClean in Flags then
        IdeBuildMode:=bmBuild;
      if IdeBuildMode=bmBuild then
        Cmd:='ide'
      else
        Cmd:='cleanide ide';

      if (not fOutputDirRedirected) and (not CheckDirectoryWritable(fWorkingDir)) then
        exit(mrCancel);

      // fTargetFilename may be lazarus.new.exe, append -o
      // Note: FPC automatically changes the last extension (append or replace)
      // For example under linux, where executables don't need any extension
      // fpc removes the last extension of the -o option.
      DefaultTargetFilename:='lazarus'+GetExecutableExt(fTargetOS);
      if CreateRelativePath(fTargetFilename,fTargetDir) <> DefaultTargetFilename then
        AppendExtraOption('-o'+fTargetFilename);

      if fExtraOptions<>'' then
        EnvironmentOverrides.Values['OPT'] := fExtraOptions;
      if not fUpdateRevInc then begin
        CheckRevisionInc;
        EnvironmentOverrides.Values['USESVN2REVISIONINC'] := '0';
      end;
      // run
      Result:=Run(lisBuildIDE);
      // clean only once. If building failed the user must first fix the error
      // before a clean build is needed.
      ApplyCleanOnce;
      if Result<>mrOk then begin
        // build failed: restore backup of lazarus.exe
        RestoreBackup;
        exit;
      end;
    end;
    Result:=mrOk;
  finally
    CmdLineParams.Free;
    EnvironmentOverrides.Free;
    if Assigned(OnMainTitleChange) then
      OnMainTitleChange(''); //LazarusIDE.MainBarSubTitle:='';
  end;
end;

function TLazarusBuilder.MakeIDEUsingLazbuild(Clean: boolean): TModalResult;
var
  s, MakeExe, LazbuildExe, StarterFilename: String;
  Tool: TAbstractExternalTool;
  EnvironmentOverrides: TStringList;
begin
  Result:=mrCancel;

  EnvironmentOverrides:=TStringList.Create;
  try
    EnvironmentOverrides.Values['LANG']:= 'en_US';
    s:=EnvironmentOptions.GetParsedCompilerFilename;
    if s<>'' then
      EnvironmentOverrides.Values['PP']:=s;

    MakeExe:=SearchMakeExe(true);
    fWorkingDir:=EnvironmentOptions.GetParsedLazarusDirectory;
    if not CheckDirectoryWritable(fWorkingDir) then
      exit;

    // clean up
    if Clean then
    begin
      Tool:=ExternalToolList.Add('make distclean');
      Tool.Reference(Self,ClassName);
      try
        Tool.Data:=TIDEExternalToolData.Create(IDEToolCompileIDE,Tool.Title,
          MakeExe);
        Tool.FreeData:=true;
        Tool.Process.Executable:=MakeExe;
        Tool.Process.Parameters.Add('distclean');
        Tool.Process.CurrentDirectory:=fWorkingDir;
        Tool.AddParsers(SubToolMake);
        Tool.EnvironmentOverrides:=EnvironmentOverrides;
        Tool.Execute;
        Tool.WaitForExit;
        if Tool.ErrorMessage<>'' then
          exit(mrCancel);
      finally
        Tool.Release(Self);
      end;
    end;

    // build lazbuild
    LazbuildExe:=AppendPathDelim(fWorkingDir)+'lazbuild'+GetExeExt;
    if not BackupExe(LazbuildExe,'lazbuild',[blfBackupOldExe],false) then
      exit(mrCancel);
    Tool:=ExternalToolList.Add('make lazbuild');
    Tool.Reference(Self,ClassName);
    try
      Tool.Data:=TIDEExternalToolData.Create(IDEToolCompileIDE,'make lazbuild',
        MakeExe);
      Tool.FreeData:=true;
      Tool.Process.Executable:=MakeExe;
      Tool.Process.Parameters.Add('lazbuild');
      Tool.AddParsers(SubToolFPC);
      Tool.AddParsers(SubToolMake);
      Tool.Process.CurrentDirectory:=fWorkingDir;
      Tool.EnvironmentOverrides:=EnvironmentOverrides;
      Tool.Execute;
      Tool.WaitForExit;
      if Tool.ErrorMessage<>'' then
        exit(mrCancel);
    finally
      Tool.Release(Self);
    end;

    // build the IDE using lazbuild
    LazbuildExe:=AppendPathDelim(fWorkingDir)+'lazbuild'+GetExeExt;
    Tool:=ExternalToolList.Add('lazbuild --useride=');
    Tool.Reference(Self,ClassName);
    try
      Tool.Data:=TIDEExternalToolData.Create(IDEToolCompileIDE,'lazbuild --user-ide=',
        MakeExe);
      Tool.FreeData:=true;
      Tool.Process.Executable:=LazbuildExe;
      Tool.Process.Parameters.Add('--build-ide=');
      Tool.Process.Parameters.Add('--lazarusdir=.');
      Tool.Process.Parameters.Add('--pcp='+GetPrimaryConfigPath);
      Tool.AddParsers(SubToolFPC);
      Tool.AddParsers(SubToolMake);
      Tool.Process.CurrentDirectory:=fWorkingDir;
      Tool.EnvironmentOverrides:=EnvironmentOverrides;
      Tool.Execute;
      Tool.WaitForExit;
      if Tool.ErrorMessage<>'' then
        exit(mrCancel);
    finally
      Tool.Release(Self);
    end;

    if Clean then
    begin
      // build
      StarterFilename:=AppendPathDelim(fWorkingDir)+'startlazarus'+GetExeExt;
      if not BackupExe(StarterFilename,'startlazarus',[blfBackupOldExe],false) then
        exit(mrCancel);
      Tool:=ExternalToolList.Add('make starter');
      Tool.Reference(Self,ClassName);
      try
        Tool.Data:=TIDEExternalToolData.Create(IDEToolCompileIDE,'make starter',
          MakeExe);
        Tool.FreeData:=true;
        Tool.Process.Executable:=MakeExe;
        Tool.Process.Parameters.Add('starter');
        Tool.AddParsers(SubToolFPC);
        Tool.AddParsers(SubToolMake);
        Tool.Process.CurrentDirectory:=fWorkingDir;
        Tool.EnvironmentOverrides:=EnvironmentOverrides;
        Tool.Execute;
        Tool.WaitForExit;
        if Tool.ErrorMessage<>'' then
          exit(mrCancel);
      finally
        Tool.Release(Self);
      end;
    end;

  finally
    EnvironmentOverrides.Free;
  end;
end;

procedure TLazarusBuilder.AppendOption_IdeConfig;
var
  MakeIDECfgFilename: string;
begin
  MakeIDECfgFilename:=GetMakeIDEConfigFilename;
  //DebugLn(['SpecialIdeConfig MAKE MakeIDECfgFilename=',MakeIDECfgFilename,' ',FileExistsUTF8(MakeIDECfgFilename)]);
  if (FileExistsUTF8(MakeIDECfgFilename)) then begin
    // If a file name contains spaces, a file name whould need to be quoted.
    // Using a single quote is not possible, it is used already in the
    // makefile to group all Profile in OPT='bla bla'.
    // using " implicates that make uses a shell to execute the command of
    // that line. But using shells (i.e. command.com, cmd.exe, etc) is so
    // fragile (see bug 11362), that is better to avoid this.
    // Therefore we use a short 8.3 file and path name, so we don't need to
    // use quotes at all.
    // On platforms other than windows, ExtractShortPathName is implemented
    // too and simply returns the passed file name, so there is no need
    // for $IFDEF.
    if pos(' ',MakeIDECfgFilename)>0 then
      MakeIDECfgFilename:=ExtractShortPathNameUTF8(MakeIDECfgFilename);
    AppendExtraOption('@'+MakeIDECfgFilename);
  end;
end;

function TLazarusBuilder.CalcTargets(Flags: TBuildLazarusFlags): TModalResult;

  function IfPairIs(const Var1, Var2, Value1, Value2: string): boolean;
  begin
    Result:=((Var1=Value1) or (Var1=Value2))
        and ((Var2=Value1) or (Var2=Value2));
  end;

var
  LazDir, TargetLCLPlatform: string;
  IsCrossCompiling: Boolean;
  s: String;
begin
  Result:=mrOk;
  fOutputDirRedirected:=False;
  fUpdateRevInc:=fProfile.UpdateRevisionInc;

  fExtraOptions:='';

  // check for special IDE config file
  //DebugLn(['CreateIDEMakeOptions blfUseMakeIDECfg=',blfUseMakeIDECfg in FLags,' ExtraOptions="',fExtraOptions,'" ',fPackageOptions]);
  if (blfUseMakeIDECfg in Flags) then
  begin
    AppendOption_IdeConfig;
  end
  else begin
    AppendExtraOption(fPackageOptions,false);

    // write full file names and message ids
    AppendExtraOption('-vbq');

    {$IFDEF Windows}
    if (fProfile.TargetPlatform=lpWin32)
    and (Win32MajorVersion <=4)
    and (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) then
      AppendExtraOption('-dWIN9XPLATFORM');
    {$ENDIF}

    // append profile and global custom options
    s:=fProfile.ExtraOptions;
    if OnAppendCustomOption<>nil then
      OnAppendCustomOption(Self,s,[bmgtEnvironment]);

    GlobalMacroList.SubstituteStr(s);
    AppendExtraOption(s,false);
  end;

  // set target filename and target directory:
  // 1. the user has set a target directory
  // 2. For crosscompiling the IDE needs a different directory
  // 3. If lazarus is installed as root/administrator, the lazarus directory
  //    is readonly and needs a different name and directory
  //    (e.g. ~/.lazarus/bin/lazarus).
  // 4. Platforms like windows locks executables, so lazarus can not replace
  //    itself. The IDE will try to rename the file or fallback to another name
  //    (e.g. lazarus.new.exe).
  //    The target directory is writable, the lazarus.o file can be created.
  // Otherwise: Don't touch the target filename.

  fTargetFilename:='';
  fUnitOutDir:='';
  CodeToolBoss.CompilerDefinesCache.ConfigCaches.GetDefaultCompilerTarget(
    EnvironmentOptions.GetParsedCompilerFilename,'',fCompilerTargetOS,fCompilerTargetCPU);
  if fCompilerTargetOS='' then
    fCompilerTargetOS:=FPCAdds.GetCompiledTargetOS;
  if fCompilerTargetCPU='' then
    fCompilerTargetCPU:=FPCAdds.GetCompiledTargetCPU;
  fTargetOS:=fProfile.FPCTargetOS;
  fTargetCPU:=fProfile.FPCTargetCPU;
  TargetLCLPlatform:=LCLPlatformDirNames[fProfile.TargetPlatform];
  if fTargetOS='' then fTargetOS:=fCompilerTargetOS;
  if fTargetCPU='' then fTargetCPU:=fCompilerTargetCPU;
  LazDir:=EnvironmentOptions.GetParsedLazarusDirectory;

  //DebugLn(['CalcTargets NewTargetOS=',fTargetOS,' NewTargetCPU=',fTargetCPU]);
  if (fProfile.TargetDirectory<>'') then begin
    // Case 1. the user has set a target directory
    fTargetDir:=fProfile.GetParsedTargetDirectory(fMacros);
    if fTargetDir='' then begin
      debugln('Error: (lazarus) [CalcTargets] error resolving macros in TargetDirectory=',fProfile.TargetDirectory);
      Exit(mrAbort);
    end;
    fUnitOutDir:=AppendPathDelim(fTargetDir)+'units';
    debugln('Hint: (lazarus) [CalcTargets] TargetDirectory=',fTargetDir);
    debugln('Hint: (lazarus) [CalcTargets] UnitsTargetDirectory=',fUnitOutDir);
  end else begin
    // no user defined target directory
    // => find it automatically
    IsCrossCompiling:=false;
    if (CompareText(fTargetOS,FPCAdds.GetCompiledTargetOS)<>0)
    or (CompareText(fTargetCPU,FPCAdds.GetCompiledTargetCPU)<>0) then
    begin
      IsCrossCompiling:=true;
      if IfPairIs(fTargetCPU,FPCAdds.GetCompiledTargetCPU,'i386','x86_64') then
      begin
        if (fTargetOS=FPCAdds.GetCompiledTargetOS)
        or IfPairIs(fTargetOS,FPCAdds.GetCompiledTargetOS,'win32','win64') then
          IsCrossCompiling:=false; // a 32 or 64bit IDE is more a flavor than cross compiling
      end;
    end;

    if IsCrossCompiling then
    begin
      // Case 2. crosscompiling the IDE
      // lazarus.exe to <primary config dir>/bin/<fTargetCPU>-<fTargetOS>
      fTargetDir:=AppendPathDelim(GetPrimaryConfigPath)+'bin'
                          +PathDelim+fTargetCPU+'-'+fTargetOS;
      // ppu files to <primary config dir>/units/<fTargetCPU>-<fTargetOS>/<LCLWidgetType>
      fUnitOutDir:=AppendPathDelim(GetPrimaryConfigPath)+'units'
                  +PathDelim+fTargetCPU+'-'+fTargetOS+PathDelim+TargetLCLPlatform;
      debugln('Hint: (lazarus) [CalcTargets] Cross Compiling TargetOS=',fProfile.FPCTargetOS,' TargetCPU=',
              fProfile.FPCTargetCPU,' CompilerDefaultOS=',fCompilerTargetOS,' CompilerDefaultCPU=',fCompilerTargetCPU);
    end else begin
      // -> normal compile for this platform

      // get lazarus directory
      fTargetDir:=LazDir;
      if (fTargetDir<>'') and DirPathExists(fTargetDir) then
      begin
        if not DirectoryIsWritableCached(fTargetDir) then begin
          // Case 3. the lazarus directory is not writable
          // lazarus.exe to <primary config dir>/bin/
          // ppu files to <primary config dir>/units/<fTargetCPU>-<fTargetOS>/<LCLWidgetType>
          fUpdateRevInc:=false;
          fTargetDir:=AppendPathDelim(GetPrimaryConfigPath)+'bin';
          debugln('Hint: (lazarus) [CalcTargets] Lazarus directory is readonly, using fallback target directory: ',fTargetDir);
          fUnitOutDir:=AppendPathDelim(GetPrimaryConfigPath)+'units'
                  +PathDelim+fTargetCPU+'-'+fTargetOS+PathDelim+TargetLCLPlatform;
        end else begin
          // Case 4. the lazarus directory is writable
          // ppu files to <lazarusdir>/units/<fTargetCPU>-<fTargetOS>/<LCLWidgetType>
          fUnitOutDir:=AppendPathDelim(fTargetDir)+'units'
                  +PathDelim+fTargetCPU+'-'+fTargetOS+PathDelim+TargetLCLPlatform;
        end;
      end else begin
        // lazarus dir is not valid (probably someone is experimenting)
        // -> just compile to current directory
        fTargetDir:='';
      end;
    end;
  end;

  // compute TargetFilename
  if not FilenameIsAbsolute(fTargetDir) then
    fTargetDir:=TrimFilename(AppendPathDelim(LazDir)+fTargetDir);
  if fTargetFilename='' then
    fTargetFilename:='lazarus'+GetExecutableExt(fTargetOS);
  if not FilenameIsAbsolute(fTargetFilename) then
    fTargetFilename:=TrimFilename(AppendPathDelim(fTargetDir)+fTargetFilename);

  // check if target file is default
  fOutputDirRedirected:=CompareFilenames(ChompPathDelim(LazDir),
                                         ChompPathDelim(fTargetDir))<>0;

  // append target options
  if not (blfUseMakeIDECfg in Flags) then
  begin
    if fTargetOS<>fCompilerTargetOS then
      AppendExtraOption('-T'+fTargetOS);
    if fTargetCPU<>fCompilerTargetCPU then
      AppendExtraOption('-P'+fTargetCPU);

    if fUnitOutDir<>'' then
      // FPC interpretes '\ ' as an escape for a space in a path on Windows,
      // so make sure the directory doesn't end with the path delimiter.
      AppendExtraOption('-FU'+ChompPathDelim(fUnitOutDir));

    //debugln(['TLazarusBuilder.CreateIDEMakeOptions fTargetDir=',fTargetDir,' fOutputDirRedirected=',fOutputDirRedirected,' fTargetFilename=',fTargetFilename]);
    if fOutputDirRedirected then
      // FPC interpretes '\ ' as an escape for a space in a path on Windows,
      // so make sure the directory doesn't end with the path delimiter.
      AppendExtraOption('-FE'+ChompPathDelim(fTargetDir));

    // Important: Do not append -o here, because if the old exe cannot be
    // renamed/deleted it needs to be changed.
  end;

  //DebugLn(['CreateIDEMakeOptions ',MMDef.Name,' ',fExtraOptions]);
end;

function TLazarusBuilder.BackupExe(var aTargetFilename: string;
  const aTitle: string; Flags: TBuildLazarusFlags; AllowAltTargetFile: boolean
  ): boolean;
{ Try to delete old backups and try to rename old exe.
  Some OS (Win) locks the exe while running, so it cannot be deleted.
  Some OS (Win XP) forbids renaming while exe is running.
}
var
  Ext: String;
  BackupFilename: String;
  Backup2Filename: String;
  AltFilename: String;
begin
  Result:=false;
  //debugln(['TLazarusBuilder.BackupExe "',aTargetFilename,'" Exists=',FileExistsUTF8(aTargetFilename),' AllowAltTargetFile=',AllowAltTargetFile,' BackupOldExe=',blfBackupOldExe in Flags]);
  if not FileExistsUTF8(aTargetFilename) then
    exit(true);
  // the exe already exists
  Ext:=ExtractFileExt(aTargetFilename);
  AltFilename:=LeftStr(aTargetFilename,length(aTargetFilename)-length(Ext))+'.new'+Ext;
  if blfBackupOldExe in Flags then begin
    // first try to delete the lazarus.new exe, so that users/startlazarus are
    // not confused which one is the newest.
    // This may fail if OS has locked the exe.
    if FileExistsUTF8(AltFilename) then begin
      if DeleteFileUTF8(AltFilename) then
        debugln(['Note: (lazarus) deleted file "',AltFilename,'"'])
      else
        debugln(['Warning: (lazarus) unable to delete file "',AltFilename,'"']);
    end;

    // try to rename the old exe
    BackupFilename:=GetBackupExeFilename(aTargetFilename);
    if FileExistsUTF8(BackupFilename) then begin
      if DeleteFileUTF8(BackupFilename) then begin
        debugln(['Note: (lazarus) deleted backup "',BackupFilename,'"']);
      end else begin
        // unable to delete old backup file, maybe an old IDE is still running
        // => try to backup the backup
        Backup2Filename:=LeftStr(aTargetFilename,length(aTargetFilename)-length(Ext))+'.old2'+Ext;
        if FileExistsUTF8(Backup2Filename) then begin
          if DeleteFileUTF8(Backup2Filename) then
            debugln(['Note: (lazarus) deleted backup "',Backup2Filename,'"'])
          else
            debugln(['Warning: (lazarus) unable to delete old backup file "'+Backup2Filename+'"']);
        end;
        if not FileExistsUTF8(Backup2Filename) then begin
          if RenameFileUTF8(BackupFilename,Backup2Filename) then
            debugln(['Note: (lazarus) renamed old backup file "'+BackupFilename+'" to "',Backup2Filename,'"'])
          else
            debugln(['Warning: (lazarus) unable to rename old backup file "'+BackupFilename+'" to "',Backup2Filename,'"']);
        end;
      end;
    end;
    if not FileExistsUTF8(BackupFilename) then begin
      if RenameFileUTF8(aTargetFilename,BackupFilename) then
        debugln(['Note: (lazarus) renamed file "'+aTargetFilename+'" to "',BackupFilename,'"'])
      else
        debugln(['Warning: (lazarus) unable to rename file "'+aTargetFilename+'" to "',BackupFilename,'"']);
    end;

    if not FileExistsUTF8(aTargetFilename) then
      exit(true);

    if FileExistsUTF8(AltFilename) then begin
      LazMessageWorker('Delete Error','Unable to rename'#13
        +aTargetFilename+#13
        +'and unable to delete'#13
        +AltFilename+#13
        +'One of them must be gone, before building the '+aTitle+'. Maybe there is another '+aTitle+' still running?',mtError,[mbCancel]);
      exit(false);
    end;
  end;
  if not AllowAltTargetFile then
    exit(false);
  // backup didn't work => use another file name
  if FileExistsUTF8(AltFilename) then
    exit(false);
  aTargetFilename:=AltFilename;
  Result:=true;
end;

function TLazarusBuilder.CreateAppleBundle: TModalResult;
var
  BundleDir: String;
begin
  Result:=mrOk;
  BundleDir:=ChangeFileExt(fTargetFilename,'.app');
  //debugln(['CreateAppleBundle checking bundle ',BundleDir]);
  if not FileExistsCached(BundleDir) then begin
    //debugln(['CreateAppleBundle TargetFile=',fTargetFilename]);
    Result:=CreateApplicationBundle(fTargetFilename, 'Lazarus');
    if not (Result in [mrOk,mrIgnore]) then begin
      debugln(['Error: (lazarus) unable to create application bundle']);
      if Assigned(OnShowMessage) then
        OnShowMessage(mluError,'to create application bundle '+BundleDir);
      exit;
    end;
    Result:=CreateAppBundleSymbolicLink(fTargetFilename);
    if not (Result in [mrOk,mrIgnore]) then begin
      debugln(['Error: (lazarus) unable to create symlink in application bundle: ',fTargetFilename]);
      if Assigned(OnShowMessage) then
        OnShowMessage(mluError,'failed to create application bundle symlink to '+fTargetFilename);
      exit;
    end;
  end;
end;

procedure TLazarusBuilder.AppendExtraOption(const aOption: string; AutoQuote: boolean);
begin
  if aOption='' then exit;
  if fExtraOptions<>'' then
    fExtraOptions:=fExtraOptions+' ';
  if AutoQuote and (pos(' ',aOption)>0) then
    fExtraOptions:=fExtraOptions+AnsiQuotedStr(aOption,'"')
  else
    fExtraOptions:=fExtraOptions+aOption;
  //DebugLn(['AppendExtraOption ',fExtraOptions]);
end;

function TLazarusBuilder.PrepareTargetDir(Flags: TBuildLazarusFlags): TModalResult;
begin
  // backup old exe
  BackupExe(fTargetFilename,'IDE',Flags,true);

  // create output directories
  if fOutputDirRedirected then begin
    Result:=ForceDirectoryInteractive(fTargetDir,[]);
    if Result<>mrOk then exit;
  end;
  if fUnitOutDir<>'' then begin
    Result:=ForceDirectoryInteractive(fUnitOutDir,[]);
    if Result<>mrOk then exit;
  end;

  // create apple bundle if needed
  //debugln(['CreateIDEMakeOptions NewTargetDirectory=',fTargetDir]);
  if (CompareText(fTargetOS,'darwin')=0)
  and fOutputDirRedirected and DirectoryIsWritableCached(fTargetDir) then
  begin
    Result:=CreateAppleBundle;
    if not (Result in [mrOk,mrIgnore]) then Exit;
  end;

  Result:=mrOk;
end;

function TLazarusBuilder.IsWriteProtected(Profile: TBuildLazarusProfile): Boolean;
// Returns True if Lazarus installation directory is write protected. Now uses OutputDirRedirected info.
begin
  fProfile:=Profile;
  if CalcTargets([])<>mrOk then exit(false);
  Result:=fOutputDirRedirected;
end;

function TLazarusBuilder.BreakExtraOptions: string;
var
  StartPos: Integer;
  EndPos: Integer;
  c: Char;
  CurLine: String;
begin
  Result:='';
  // write each option into a line of its own
  StartPos:=1;
  repeat
    while (StartPos<=length(fExtraOptions)) and (fExtraOptions[StartPos]=' ') do
      inc(StartPos);
    EndPos:=StartPos;
    while EndPos<=length(fExtraOptions) do begin
      c:=fExtraOptions[EndPos];
      case c of
      ' ': break;

      '''','"','`':
        begin
          repeat
            inc(EndPos);
            if (fExtraOptions[EndPos]=c) then begin
              inc(EndPos);
              break;
            end;
          until (EndPos>length(fExtraOptions));
        end;

      else
        inc(EndPos);
      end;
    end;
    if (EndPos>StartPos) then begin
      CurLine:=Trim(copy(fExtraOptions,StartPos,EndPos-StartPos));
      if (length(CurLine)>2) and (CurLine[1] in ['''','"','`'])
      and (CurLine[1]=CurLine[length(CurLine)]) then begin
        // whole line enclosed in quotation marks
        // in fpc config this is forbidden and gladfully unncessary
        CurLine:=copy(CurLine,2,length(CurLine)-2);
      end;
      Result:=Result+CurLine+LineEnding;
    end;
    StartPos:=EndPos;
  until StartPos>length(fExtraOptions);
end;

function TLazarusBuilder.SaveIDEMakeOptions(Profile: TBuildLazarusProfile;
  Flags: TBuildLazarusFlags): TModalResult;
var
  Filename: String;
  fs: TFileStream;
  OptionsAsText: String;
begin
  Result:=mrCancel;
  fProfile:=Profile;
  if CalcTargets(Flags-[blfUseMakeIDECfg])<>mrOk then exit;

  Result:=PrepareTargetDir(Flags);
  if Result<>mrOk then exit;
  Filename:=GetMakeIDEConfigFilename;
  try
    InvalidateFileStateCache;
    fs:=TFileStream.Create(Filename,fmCreate);
    try
      if fExtraOptions<>'' then begin
        // FPC expects console codepage for command line params
        // and system codepage in config files
        OptionsAsText:=UTF8ToWinCP(BreakExtraOptions);
        fs.Write(OptionsAsText[1],length(OptionsAsText));
      end;
    finally
      fs.Free;
    end;
  except
    on E: Exception do begin
      Result:=LazMessageWorker(lisLazBuildErrorWritingFile,
        Format(lisLazBuildUnableToWriteFile, [Filename,LineEnding]) + E.Message,
        mtError,[mbCancel,mbAbort]);
      exit;
    end;
  end;
  Result:=mrOk;
end;

function TLazarusBuilder.SearchMakeExe(Interactive: boolean): string;
begin
  Result:=EnvironmentOptions.GetParsedMakeFilename;
  if (Result<>'') and not FileExistsUTF8(Result) then
    Result:=FindDefaultExecutablePath(Result);
  if (Result='') or not FileExistsUTF8(Result) then begin
    Result:=FindDefaultMakePath;
    if (Result='') or not FileExistsUTF8(Result) then begin
      Result:='';
      if not Interactive then
        exit;
      LazMessageWorker(lisMakeNotFound,
        Format(lisTheProgramMakeWasNotFound, [LineEnding]),
        mtError, [mbCancel]);
      exit;
    end;
  end;
end;

end.

