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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner
  Converted to lfm by: Matthijs Willemstein
  Quickoptions added by: Giuliano Colla
  Then extensively modified by: Juha Manninen
   - added support for Build Profiles which extend the idea of Quick Options.
   - changed UI to be less weird and comply better with UI design norms.
   - changed object structure to keep it logical and to avoid duplicate data.

  Abstract:
    Defines settings for the "Build Lazarus" function of the IDE.
    TConfigureBuildLazarusDlg is used to edit the build options.

    The BuildLazarus function will build the lazarus parts.

    Building occurs only with options defined in the Detail Page.
    Profiles are just used to set options there. Therefore beginners can
    use default profiles and don't need to touch the Detail Page.
    Advanced users can define their own profiles for example for cross compiling.
}
unit BuildLazDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, LCLType, StdCtrls, ExtCtrls,
  Buttons, FileUtil, LazUTF8, LazLogger, lazutf8classes, LazFileCache, Dialogs,
  InterfaceBase, CheckLst, Menus, ComCtrls, DividerBevel, DefineTemplates,
  CodeToolManager,
  // IDEIntf
  LazIDEIntf, IDEMsgIntf, IDEHelpIntf, IDEImagesIntf, IDEWindowIntf, IDEDialogs,
  PackageIntf,
  {$IFDEF EnableNewExtTools}
  IDEExternToolIntf,
  {$ENDIF}
  // IDE
  LazarusIDEStrConsts, TransferMacros, LazConf, IDEProcs, DialogProcs,
  MainBar, ExtToolEditDlg, EnvironmentOpts,
  ApplicationBundle, CompilerOptions, BuildProfileManager,
  GenericListEditor, GenericCheckList, PackageSystem, PackageDefs;

type

  TBuildLazarusFlag = (
    blfDontBuild,           // skip all building, only cleaning
    blfOnlyIDE,             // skip all but IDE (for example build IDE, but not packages, not lazbuild, ...)
    blfDontClean,           // ignore clean up option in profile
    blfUseMakeIDECfg,       // append @idemake.cfg
    blfReplaceExe,          // ignore OSLocksExecutables and do not create lazarus.new.exe
    blfBackupOldExe         // rename existing lazarus exe to lazarus.old
    );
  TBuildLazarusFlags = set of TBuildLazarusFlag;

  TLazarusBuilder = class;

  { TConfigureBuildLazarusDlg }

  TConfigureBuildLazarusDlg = class(TForm)
    CleanAllRadioButton: TRadioButton;
    CleanAutoRadioButton: TRadioButton;
    CleanCommonRadioButton: TRadioButton;
    CleanOnceCheckBox: TCheckBox;
    CleanCommonCheckBox: TCheckBox;
    CommonsDividerBevel: TDividerBevel;
    ConfirmBuildCheckBox: TCheckBox;
    DefinesButton: TButton;
    DefinesLabel: TLabel;
    DefinesListBox: TCheckListBox;
    CancelButton: TBitBtn;
    CBLDBtnPanel: TPanel;
    BuildProfileComboBox: TComboBox;
    CompileButton: TBitBtn;
    CompileAdvancedButton: TBitBtn;
    InhTreeView: TTreeView;
    LCLWidgetTypeLabel: TLabel;
    LCLWidgetTypeComboBox: TComboBox;
    OptionsLabel: TLabel;
    OptionsMemo: TMemo;
    CleanUpGroupBox: TGroupBox;
    PageControl1: TPageControl;
    RestartAfterBuildCheckBox: TCheckBox;
    ShowOptsMenuItem: TMenuItem;
    DetailsPanel: TPanel;
    HelpButton: TBitBtn;
    BuildProfileLabel: TLabel;
    OptionsPopupMenu: TPopupMenu;
    Panel2: TPanel;
    SaveSettingsButton: TBitBtn;
    BuildProfileButton: TButton;
    BuildTabSheet: TTabSheet;
    InfoTabSheet: TTabSheet;
    TargetCPUComboBox: TComboBox;
    TargetCPULabel: TLabel;
    TargetDirectoryButton: TButton;
    TargetDirectoryComboBox: TComboBox;
    TargetDirectoryLabel: TLabel;
    TargetOSComboBox: TComboBox;
    TargetOSLabel: TLabel;
    UpdateRevisionIncCheckBox: TCheckBox;
    procedure BuildProfileButtonClick(Sender: TObject);
    procedure BuildProfileComboBoxSelect(Sender: TObject);
    procedure CleanRadioButtonClick(Sender: TObject);
    procedure CleanCommonCheckBoxClick(Sender: TObject);
    procedure CompileAdvancedButtonClick(Sender: TObject);
    procedure CompileButtonClick(Sender: TObject);
    procedure DefinesButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ShowOptsMenuItemClick(Sender: TObject);
    procedure SaveSettingsButtonClick(Sender: TObject);
    procedure TargetDirectoryButtonClick(Sender: TObject);
  private
    fBuilder: TLazarusBuilder;
    // Data is copied by caller before and after opening this dialog.
    fProfiles: TBuildLazarusProfiles;
    fUpdatingProfileCombo: Boolean;
    fImageIndexPackage: Integer;
    fImageIndexRequired: Integer;
    fImageIndexInherited: Integer;
    procedure SetupInfoPage;
    procedure UpdateInheritedTree;
    procedure PrepareClose;
    procedure ShowHideCleanup(aShow: Boolean);
  public
    constructor Create(TheOwner: TComponent); overload; reintroduce;
    destructor Destroy; override;
    procedure CopyProfileToUI(AProfile: TBuildLazarusProfile);
    procedure CopyUIToProfile(AProfile: TBuildLazarusProfile);
    procedure UpdateProfileNamesUI;
  end;

  { TLazarusBuilder }

  TLazarusBuilder = class
  private
    fProfile: TBuildLazarusProfile;
    fExtraOptions: string;
    fPackageOptions: string;
    fMacros: TTransferMacroList;
    fUpdateRevInc: boolean;
    fOutputDirRedirected: boolean;
    fTargetOS: string;
    fTargetCPU: string;
    fTargetFilename: string; // = fTargetDir + 'lazarus'+GetExecutableExt(fTargetOS)
    fTargetDir: string;
    fUnitOutDir: string;
    fWorkingDir: string;
    fProfileChanged: boolean;
    // Methods used by MakeLazarus :
    procedure ApplyCleanOnce;
    function CheckDirectoryWritable(Dir: string): boolean;
    procedure CleanLazarusSrcDir(Dir: string; Recursive: boolean = true);
    procedure CleanAll;
    procedure CheckRevisionInc;
    procedure RestoreBackup;
    // Methods used by SaveIDEMakeOptions :
    function BreakExtraOptions: string;
    // Methods used by CalcTargets :
    procedure SpecialIdeConfig;
    // This is used by CreateIDEMakeOptions and IsWriteProtected
    function CalcTargets(Flags: TBuildLazarusFlags): TModalResult;
    // Methods used by CreateIDEMakeOptions :
    procedure BackupExe(Flags: TBuildLazarusFlags);
    function CreateAppleBundle: TModalResult;
    procedure AppendExtraOption(const aOption: string; EncloseIfSpace: boolean = True);
    // This is used by MakeLazarus and SaveIDEMakeOptions
    function CreateIDEMakeOptions(Flags: TBuildLazarusFlags): TModalResult;
  public
    {$IFNDEF EnableNewExtTools}
    ExternalTools: TBaseExternalToolList;
    {$ENDIF}
    constructor Create;
    function ShowConfigureBuildLazarusDlg(AProfiles: TBuildLazarusProfiles): TModalResult;
    function MakeLazarus(Profile: TBuildLazarusProfile; Flags: TBuildLazarusFlags): TModalResult;
    function IsWriteProtected(Profile: TBuildLazarusProfile): Boolean;
    function SaveIDEMakeOptions(Profile: TBuildLazarusProfile; Flags: TBuildLazarusFlags): TModalResult;
  public
    property PackageOptions: string read fPackageOptions write fPackageOptions;
    property ProfileChanged: boolean read fProfileChanged write fProfileChanged;
  end;

function GetMakeIDEConfigFilename: string;
function GetBackupExeFilename(Filename: string): string;

implementation

{$R *.lfm}

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

function TLazarusBuilder.ShowConfigureBuildLazarusDlg(AProfiles: TBuildLazarusProfiles): TModalResult;
// mrOk=save
// mrYes=save and compile
// mrAll=save and compile all selected profiles
var
  ConfigBuildLazDlg: TConfigureBuildLazarusDlg;
begin
  Result := mrCancel;
  ConfigBuildLazDlg := TConfigureBuildLazarusDlg.Create(nil);
  try
    ConfigBuildLazDlg.fBuilder := Self;
    ConfigBuildLazDlg.fProfiles.Assign(AProfiles); // Copy profiles to dialog.
    Result := ConfigBuildLazDlg.ShowModal;
    if Result in [mrOk,mrYes,mrAll] then
      AProfiles.Assign(ConfigBuildLazDlg.fProfiles); // Copy profiles back from dialog.
  finally
    ConfigBuildLazDlg.Free;
  end;
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
  IDEMessageDialog(lisBuildingLazarusFailed,
             Format(lisThisSetOfOptionsToBuildLazarusIsNotSupportedByThis,
                    [LineEnding, '"', Dir, '"', LineEnding]),
             mtError,[mbCancel]);
end;

procedure TLazarusBuilder.CleanLazarusSrcDir(Dir: string; Recursive: boolean = true);
var
  FileInfo: TSearchRec;
  Ext: String;
  Filename: TFilename;
begin
  Dir:=AppendPathDelim(TrimFilename(Dir));
  if FindFirstUTF8(Dir+AllFilesMask,faAnyFile,FileInfo)=0 then begin
    repeat
      if (FileInfo.Name='') or (FileInfo.Name='.') or (FileInfo.Name='..')
      or (FileInfo.Name='.svn') or (FileInfo.Name='.git')
      then
        continue;
      Filename:=Dir+FileInfo.Name;
      if faDirectory and FileInfo.Attr>0 then
      begin
        if Recursive then
          CleanLazarusSrcDir(Filename)
      end
      else begin
        Ext:=LowerCase(ExtractFileExt(FileInfo.Name));
        if (Ext='.ppu') or (Ext='.o') or (Ext='.rst') or (Ext='.rsj') then begin
          if not DeleteFileUTF8(Filename) then
            debugln(['CleanLazarusSrcDir failed to delete file "',Filename,'"']);
        end;
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
end;

procedure TLazarusBuilder.CleanAll;
var
  s: String;
begin
  // clean all lazarus source directories
  // Note: Some installations put the fpc units into the lazarus directory
  //       => clean only the known directories
  CleanLazarusSrcDir(fWorkingDir,false);
  CleanLazarusSrcDir(fWorkingDir+PathDelim+'examples');
  CleanLazarusSrcDir(fWorkingDir+PathDelim+'components');
  CleanLazarusSrcDir(fWorkingDir+PathDelim+'units');
  CleanLazarusSrcDir(fWorkingDir+PathDelim+'ide');
  CleanLazarusSrcDir(fWorkingDir+PathDelim+'packager');
  CleanLazarusSrcDir(fWorkingDir+PathDelim+'lcl');
  CleanLazarusSrcDir(fWorkingDir+PathDelim+'ideintf');
  CleanLazarusSrcDir(fWorkingDir+PathDelim+'tools');
  CleanLazarusSrcDir(fWorkingDir+PathDelim+'test');

  // clean config directory
  CleanLazarusSrcDir(GetPrimaryConfigPath+PathDelim+'units');

  // clean custom target directory
  if fProfile.TargetDirectory<>'' then begin
    s:=fProfile.GetParsedTargetDirectory(fMacros);
    if (s<>'') and DirPathExists(s) then
      CleanLazarusSrcDir(s);
  end;
end;

procedure TLazarusBuilder.CheckRevisionInc;
var
  RevisionIncFile: String;
  sl: TStringList;
begin
  RevisionIncFile:=AppendPathDelim(EnvironmentOptions.GetParsedLazarusDirectory)+'ide'+PathDelim+'revision.inc';
  if not FileExistsUTF8(RevisionIncFile) then begin
    debugln(['Note: revision.inc file missing: ',RevisionIncFile]);
    sl:=TStringList.Create;
    sl.Add('// Created by lazbuild');
    sl.Add('const RevisionStr = '''+LazarusVersionStr+''';');
    try
      sl.SaveToFile(RevisionIncFile);
    except
      on E: Exception do begin
        debugln(['Note: can not write ',RevisionIncFile,': ',E.Message]);
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
      debugln(['Building IDE failed. Can not delete "',fTargetFilename,'"']);
      exit;
    end;
  end;
  BackupFilename:=GetBackupExeFilename(fTargetFilename);
  if FileExistsUTF8(BackupFilename) then begin
    if not RenameFileUTF8(BackupFilename,fTargetFilename) then begin
      debugln(['Building IDE failed. Can not restore backup file "',BackupFilename,'" to "',fTargetFilename,'"']);
    end;
  end;
end;

function TLazarusBuilder.MakeLazarus(Profile: TBuildLazarusProfile;
  Flags: TBuildLazarusFlags): TModalResult;
var
  {$IFDEF EnableNewExtTools}
  Tool: TAbstractExternalTool;
  {$ELSE}
  Tool: TExternalToolOptions;
  {$ENDIF}
  Executable, CmdLineParams, Cmd: String;
  EnvironmentOverrides: TStringList;

  function Run(CurTitle: string): TModalResult;
  var
    Params: String;
  begin
    Params:=UTF8Trim(CmdLineParams,[]);
    if fMacros<>nil then
      fMacros.SubstituteStr(Params);
    if Params<>'' then
      Params:=Cmd+' '+Params
    else
      Params:=Cmd;
    {$IFDEF EnableNewExtTools}
    Tool:=ExternalToolList.Add(CurTitle);
    Tool.Process.Executable:=Executable;
    Tool.AddParsers(SubToolFPC);
    Tool.AddParsers(SubToolMake);
    Tool.Process.CurrentDirectory:=fWorkingDir;
    Tool.EnvironmentOverrides:=EnvironmentOverrides;
    Tool.CmdLineParams:=Params;
    Tool.Execute;
    Tool.WaitForExit;
    if Tool.ErrorMessage='' then
      exit(mrOk)
    else
      exit(mrCancel);
    {$ELSE}
    if Tool=nil then
      Tool:=TExternalToolOptions.Create;
    Tool.Title:=CurTitle;
    Tool.Filename:=Executable;
    Tool.WorkingDirectory:=fWorkingDir;
    Tool.ScanOutputForFPCMessages:=true;
    Tool.ScanOutputForMakeMessages:=true;
    Tool.CmdLineParams:=Params;
    Tool.EnvironmentOverrides.Assign(EnvironmentOverrides);
    Result:=ExternalTools.Run(Tool,fMacros,false);
    {$ENDIF}
  end;

var
  IdeBuildMode: TIdeBuildMode;
  s: String;
begin
  Result:=mrCancel;
  fProfile:=Profile;

  if LazarusIDE<>nil then
    LazarusIDE.MainBarSubTitle:=Profile.Name;
  IdeBuildMode:=Profile.IdeBuildMode;

  EnvironmentOverrides:=TStringList.Create;
  Tool:=nil;
  try
    // setup external tool
    EnvironmentOverrides.Values['LCL_PLATFORM']:=LCLPlatformDirNames[Profile.TargetPlatform];
    EnvironmentOverrides.Values['LANG']:= 'en_US';
    s:=EnvironmentOptions.GetParsedCompilerFilename;
    if s<>'' then
      EnvironmentOverrides.Values['PP']:=s;

    Executable:=EnvironmentOptions.GetParsedMakeFilename;
    if (Executable<>'') and (not FileExistsUTF8(Executable)) then
      Executable:=FindDefaultExecutablePath(Executable);
    if (Executable='') or (not FileExistsUTF8(Executable)) then begin
      Executable:=FindDefaultMakePath;
      if (Executable='') or (not FileExistsUTF8(Executable)) then begin
        IDEMessageDialog(lisMakeNotFound,
                   Format(lisTheProgramMakeWasNotFoundThisToolIsNeededToBuildLa,
                          ['"', '"', LineEnding, LineEnding]),
                   mtError,[mbCancel]);
        exit;
      end;
    end;

    // add -w option to print leaving/entering messages of "make"
    CmdLineParams:=' -w';
    // append target OS
    if Profile.TargetOS<>'' then
      CmdLineParams+=' OS_TARGET='+Profile.FPCTargetOS+' OS_SOURCE='+Profile.FPCTargetOS;
    // append target CPU
    if Profile.TargetCPU<>'' then
      CmdLineParams+=' CPU_TARGET='+Profile.FPCTargetCPU+' CPU_SOURCE='+Profile.FPCTargetCPU;

    fWorkingDir:=EnvironmentOptions.GetParsedLazarusDirectory;
    // clean up
    if (IdeBuildMode<>bmBuild) and (not (blfDontClean in Flags)) then begin
      if not CheckDirectoryWritable(fWorkingDir) then exit(mrCancel);

      if (IdeBuildMode=bmCleanAllBuild) and (not (blfOnlyIDE in Flags)) then
        CleanAll;

      // call make to clean up
      if (IdeBuildMode=bmCleanBuild) or (blfOnlyIDE in Flags) then
        Cmd:='cleanide'
      else
        Cmd:='cleanlaz';
      Result:=Run(lisCleanLazarusSource);
      if Result<>mrOk then exit;

      ApplyCleanOnce;
    end;

    // build IDE
    if not (blfDontBuild in Flags) then begin
      if blfDontClean in Flags then
        IdeBuildMode:=bmBuild;
      if IdeBuildMode=bmBuild then
        Cmd:='idepkg'
      else
        Cmd:='cleanide ide';
      // append extra Profile
      fExtraOptions:='';
      Result:=CreateIDEMakeOptions(Flags);
      if Result<>mrOk then exit;

      if (not fOutputDirRedirected) and (not CheckDirectoryWritable(fWorkingDir)) then
        exit(mrCancel);

      if fExtraOptions<>'' then
        EnvironmentOverrides.Values['OPT'] := fExtraOptions;
      if not fUpdateRevInc then begin
        CheckRevisionInc;
        EnvironmentOverrides.Values['USESVN2REVISIONINC'] := '0';
      end;
      // run
      Result:=Run(lisIDE);
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
    EnvironmentOverrides.Free;
    {$IFNDEF EnableNewExtTools}
    Tool.Free;
    {$ENDIF}
    if LazarusIDE<>nil then
      LazarusIDE.MainBarSubTitle:='';
  end;
end;

procedure TLazarusBuilder.SpecialIdeConfig;
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
var
  DefaultTargetOS, DefaultTargetCPU: string;
  LazDir, TargetLCLPlatform: string;
begin
  Result:=mrOk;
  fOutputDirRedirected:=False;
  fUpdateRevInc:=fProfile.UpdateRevisionInc;

  // create extra options
  fExtraOptions:=fProfile.ExtraOptions;

  // check for special IDE config file
  if (blfUseMakeIDECfg in Flags) then
    SpecialIdeConfig;

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
  CodeToolBoss.FPCDefinesCache.ConfigCaches.GetDefaultCompilerTarget(
    EnvironmentOptions.GetParsedCompilerFilename,'',DefaultTargetOS,DefaultTargetCPU);
  if DefaultTargetOS='' then
    DefaultTargetOS:=GetCompiledTargetOS;
  if DefaultTargetCPU='' then
    DefaultTargetCPU:=GetCompiledTargetCPU;
  fTargetOS:=fProfile.FPCTargetOS;
  fTargetCPU:=fProfile.FPCTargetCPU;
  TargetLCLPlatform:=LCLPlatformDirNames[fProfile.TargetPlatform];
  if fTargetOS='' then fTargetOS:=DefaultTargetOS;
  if fTargetCPU='' then fTargetCPU:=DefaultTargetCPU;
  LazDir:=EnvironmentOptions.GetParsedLazarusDirectory;

  //DebugLn(['CalcTargets NewTargetOS=',fTargetOS,' NewTargetCPU=',fTargetCPU]);
  if (fProfile.TargetDirectory<>'') then begin
    // Case 1. the user has set a target directory
    fTargetDir:=fProfile.GetParsedTargetDirectory(fMacros);
    if fTargetDir='' then begin
      debugln('CalcTargets macro aborted Options.TargetDirectory=',fProfile.TargetDirectory);
      Exit(mrAbort);
    end;
    fUnitOutDir:=AppendPathDelim(fTargetDir)+'units';
    debugln('CalcTargets TargetDirectory=',fTargetDir);
    debugln('CalcTargets UnitsTargetDirectory=',fUnitOutDir);
  end else begin
    // no user defined target directory
    // => find it automatically
    if (CompareText(fTargetOS,DefaultTargetOS)<>0)
    or (CompareText(fTargetCPU,DefaultTargetCPU)<>0) then
    begin
      // Case 2. crosscompiling the IDE
      // lazarus.exe to <primary config dir>/bin/<fTargetCPU>-<fTargetOS>
      fTargetDir:=AppendPathDelim(GetPrimaryConfigPath)+'bin'
                          +PathDelim+fTargetCPU+'-'+fTargetOS;
      // ppu files to <primary config dir>/units/<fTargetCPU>-<fTargetOS>/<LCLWidgetType>
      fUnitOutDir:=AppendPathDelim(GetPrimaryConfigPath)+'units'
                  +PathDelim+fTargetCPU+'-'+fTargetOS+PathDelim+TargetLCLPlatform;
      debugln('CalcTargets Options.TargetOS=',fProfile.FPCTargetOS,' Options.TargetCPU=',
              fProfile.FPCTargetCPU,' DefaultOS=',DefaultTargetOS,' DefaultCPU=',DefaultTargetCPU);
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
          debugln('CalcTargets LazDir readonly NewTargetDirectory=',fTargetDir);
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
end;

procedure TLazarusBuilder.BackupExe(Flags: TBuildLazarusFlags);
var
  Ext: String;
  BackupFilename: String;
  Backup2Filename: String;
  AltFilename: String;
begin
  if not FileExistsUTF8(fTargetFilename) then exit;
  // the exe already exists
  Ext:=ExtractFileExt(fTargetFilename);
  AltFilename:=LeftStr(fTargetFilename,length(fTargetFilename)-length(Ext))+'.new'+Ext;
  if blfBackupOldExe in Flags then begin
    // always delete the lazarus.new exe, so that users/startlazarus are not
    // confused which one is the newest
    if FileExistsUTF8(AltFilename) then begin
      if DeleteFileUTF8(AltFilename) then
        debugln(['Note: deleted file "',AltFilename,'"'])
      else
        debugln(['WARNING: unable to delete file "',AltFilename,'"']);
    end;

    // try to rename the old exe
    BackupFilename:=GetBackupExeFilename(fTargetFilename);
    if FileExistsUTF8(BackupFilename) then
      if DeleteFileUTF8(BackupFilename) then begin
        debugln(['Note: deleted backup "',BackupFilename,'"']);
      end else begin
        // unable to delete old backup file, maybe an old IDE is still running
        // => try to backup the backup
        Backup2Filename:=LeftStr(fTargetFilename,length(fTargetFilename)-length(Ext))+'.old2'+Ext;
        if FileExistsUTF8(Backup2Filename) then begin
          if DeleteFileUTF8(Backup2Filename) then
            debugln(['Note: deleted backup "',Backup2Filename,'"'])
          else
            debugln(['WARNING: unable to delete old backup file "'+Backup2Filename+'"']);
        end;
        if not FileExistsUTF8(Backup2Filename) then begin
          if RenameFileUTF8(BackupFilename,Backup2Filename) then
            debugln(['Note: renamed old backup file "'+BackupFilename+'" to "',Backup2Filename,'"'])
          else
            debugln(['WARNING: unable to rename old backup file "'+BackupFilename+'" to "',Backup2Filename,'"']);
      end;
    end;
    if not FileExistsUTF8(BackupFilename) then begin
      if RenameFileUTF8(fTargetFilename,BackupFilename) then
        debugln(['Note: renamed file "'+fTargetFilename+'" to "',BackupFilename,'"'])
      else
        debugln(['WARNING: unable to rename file "'+fTargetFilename+'" to "',BackupFilename,'"']);
    end;
  end;
  if (not (blfReplaceExe in Flags)) and FileExistsUTF8(fTargetFilename) then
    fTargetFilename:=AltFilename;  // backup didn't work => use another file name
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
      debugln(['CreateAppleBundle CreateApplicationBundle failed']);
      if IDEMessagesWindow<>nil then
        {$IFDEF EnableNewExtTools}
        IDEMessagesWindow.AddCustomMessage(mluError,'to create application bundle '+BundleDir);
        {$ELSE}
        IDEMessagesWindow.AddMsg('Error: failed to create application bundle '+BundleDir,fTargetDir,-1);
        {$ENDIF}
      exit;
    end;
    Result:=CreateAppBundleSymbolicLink(fTargetFilename);
    if not (Result in [mrOk,mrIgnore]) then begin
      debugln(['CreateAppleBundle CreateAppBundleSymbolicLink failed']);
      if IDEMessagesWindow<>nil then
        {$IFDEF EnableNewExtTools}
        IDEMessagesWindow.AddCustomMessage(mluError,'failed to create application bundle symlink to '+fTargetFilename);
        {$ELSE}
        IDEMessagesWindow.AddMsg('Error: failed to create application bundle symlink to '+fTargetFilename,fTargetDir,-1);
        {$ENDIF}
      exit;
    end;
  end;
end;

procedure TLazarusBuilder.AppendExtraOption(const aOption: string; EncloseIfSpace: boolean);
begin
  if aOption='' then exit;
  if fExtraOptions<>'' then
    fExtraOptions:=fExtraOptions+' ';
  if EncloseIfSpace and (Pos(' ',aOption)>0) then
    fExtraOptions:=fExtraOptions+'"'+aOption+'"'
  else
    fExtraOptions:=fExtraOptions+aOption;
  //DebugLn(['AppendExtraOption ',fExtraOptions]);
end;

function TLazarusBuilder.CreateIDEMakeOptions(Flags: TBuildLazarusFlags): TModalResult;
var
  DefaultTargetFilename: string;
begin
  // Get target files and directories.
  Result:=CalcTargets(Flags);
  if Result<>mrOk then exit;

  // backup old exe
  BackupExe(Flags);

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
  if (fProfile.TargetPlatform in [lpCarbon,lpCocoa])
  and fOutputDirRedirected and DirectoryIsWritableCached(fTargetDir) then
  begin
    Result:=CreateAppleBundle;
    if not (Result in [mrOk,mrIgnore]) then Exit;
  end;

  if fUnitOutDir<>'' then
    // FPC interpretes '\ ' as an escape for a space in a path on Windows,
    // so make sure the directory doesn't end with the path delimiter.
    AppendExtraOption('-FU'+ChompPathDelim(fUnitOutDir));

  //debugln(['TLazarusBuilder.CreateIDEMakeOptions fTargetDir=',fTargetDir,' fOutputDirRedirected=',fOutputDirRedirected,' fTargetFilename=',fTargetFilename]);
  if fOutputDirRedirected then
    // FPC interpretes '\ ' as an escape for a space in a path on Windows,
    // so make sure the directory doesn't end with the path delimiter.
    AppendExtraOption('-FE'+ChompPathDelim(fTargetDir));

  // Note: FPC automatically changes the last extension (append or replace)
  // For example under linux, where executables don't need any extension
  // fpc removes the last extension of the -o option.
  DefaultTargetFilename:='lazarus'+GetExecutableExt(fTargetOS);
  if CreateRelativePath(fTargetFilename,fTargetDir) <> DefaultTargetFilename then
    AppendExtraOption('-o'+fTargetFilename);

  // add package options for IDE
  //DebugLn(['CreateIDEMakeOptions blfUseMakeIDECfg=',blfUseMakeIDECfg in FLags,' ExtraOptions="',fExtraOptions,'" ',fPackageOptions]);
  if not (blfUseMakeIDECfg in Flags) then
    AppendExtraOption(fPackageOptions,false);
  //DebugLn(['CreateIDEMakeOptions ',MMDef.Name,' ',fExtraOptions]);
end;

function TLazarusBuilder.IsWriteProtected(Profile: TBuildLazarusProfile): Boolean;
// Returns True if Lazarus installation directory is write protected. Now uses OutputDirRedirected info.
begin
  fProfile:=Profile;
  CalcTargets([]);
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
  fs: TFileStreamUTF8;
  OptionsAsText: String;
begin
  fProfile:=Profile;
  fExtraOptions:='';
  Result:=CreateIDEMakeOptions(Flags);
  if Result<>mrOk then exit;
  Filename:=GetMakeIDEConfigFilename;
  try
    InvalidateFileStateCache;
    fs:=TFileStreamUTF8.Create(Filename,fmCreate);
    try
      if fExtraOptions<>'' then begin
        OptionsAsText:=BreakExtraOptions;
        fs.Write(OptionsAsText[1],length(OptionsAsText));
      end;
    finally
      fs.Free;
    end;
  except
    on E: Exception do begin
      Result:=IDEMessageDialog(lisLazBuildErrorWritingFile,
        Format(lisLazBuildUnableToWriteFile, [Filename, LineEnding])
        +E.Message,
        mtError,[mbCancel,mbAbort]);
      exit;
    end;
  end;
  Result:=mrOk;
end;

{ TConfigureBuildLazarusDlg }

constructor TConfigureBuildLazarusDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fProfiles:=TBuildLazarusProfiles.Create;
  fUpdatingProfileCombo:=False;
end;

destructor TConfigureBuildLazarusDlg.Destroy;
begin
  FreeAndNil(fProfiles);
  inherited Destroy;
end;

procedure TConfigureBuildLazarusDlg.FormCreate(Sender: TObject);
var
  LCLInterface: TLCLPlatform;
begin
  IDEDialogLayoutList.ApplyLayout(Self,700,480);

  Caption := Format(lisConfigureBuildLazarus, ['"', '"']);
  PageControl1.ActivePage:=BuildTabSheet;
  BuildTabSheet.Caption:=lisBuildCaption;

  // Show Build target names in combobox.
  LCLWidgetTypeLabel.Caption := lisLCLWidgetType;
  for LCLInterface:=Low(TLCLPlatform) to High(TLCLPlatform) do
    LCLWidgetTypeComboBox.Items.Add(LCLPlatformDisplayNames[LCLInterface]);

  BuildProfileLabel.Caption:=lisLazBuildProfile;
  BuildProfileButton.Hint := lisLazBuildManageProfiles2;
  BuildProfileComboBox.Hint := lisLazBuildNameOfTheActiveProfile;
  OptionsLabel.Caption := lisLazBuildOptions;
  TargetOSLabel.Caption := lisLazBuildTargetOS;
  TargetCPULabel.Caption := lisLazBuildTargetCPU;
  TargetDirectoryLabel.Caption := lisLazBuildTargetDirectory;

  DefinesListBox.Hint := lisLazBuildDefinesWithoutD;
  DefinesLabel.Caption := lisLazBuildDefines;
  DefinesButton.Caption := lisLazBuildEditDefines;
  DefinesButton.Hint := lisLazBuildEditListOfDefinesWhichCanBeUsedByAnyProfile;

  CleanUpGroupBox.Caption:=lisCleanUp;
  CleanAutoRadioButton.Caption:=lisAutomatically;
  CleanCommonRadioButton.Caption:=lisCleanCommonFiles;
  CleanAllRadioButton.Caption:=lisCleanAll;
  CleanOnceCheckBox.Caption:=lisCleanOnlyOnce;
  CleanOnceCheckBox.Hint:=lisAfterCleaningUpSwitchToAutomaticClean;
  CleanCommonCheckBox.Caption:=lisCleanCommonFiles;

  UpdateRevisionIncCheckBox.Caption := lisLazBuildUpdateRevInc;
  UpdateRevisionIncCheckBox.Hint := lisLazBuildUpdateRevisionInfoInAboutLazarusDialog;

  CommonsDividerBevel.Caption := lisLazBuildCommonSettings;
  RestartAfterBuildCheckBox.Caption := lisLazBuildRestartAfterBuild;
  RestartAfterBuildCheckBox.Hint := lisLazBuildRestartLazarusAutomatically;
  ConfirmBuildCheckBox.Caption := lisLazBuildConfirmBuild;
  ConfirmBuildCheckBox.Hint := lisLazBuildShowConfirmationDialogWhenBuilding;

  CompileButton.Caption := lisBuild;
  CompileButton.LoadGlyphFromResourceName(HInstance, 'menu_build');
  CompileAdvancedButton.Caption := lisLazBuildBuildMany;
  CompileAdvancedButton.LoadGlyphFromResourceName(HInstance, 'menu_build_all');
  SaveSettingsButton.Caption := lisSaveSettings;
  SaveSettingsButton.LoadGlyphFromStock(idButtonSave);
  if SaveSettingsButton.Glyph.Empty then
    SaveSettingsButton.LoadGlyphFromResourceName(HInstance, 'laz_save');
  CancelButton.Caption := lisCancel;
  HelpButton.Caption := lisMenuHelp;

  OptionsMemo.Hint := lisLazBuildOptionsPassedToCompiler;
  ShowOptsMenuItem.Caption := lisLazBuildShowOptionsAndDefinesForCommandLine;

  with TargetOSComboBox do
  begin
    with Items do begin
      Add(''); //('+rsiwpDefault+')');
      Add('Darwin');
      Add('FreeBSD');
      Add('Linux');
      Add('NetBSD');
      Add('OpenBSD');
      Add('Solaris');
      Add('Win32');
      Add('Win64');
      Add('WinCE');
      Add('go32v2');
      Add('os2');
      Add('beos');
      Add('haiku');
      Add('qnx');
      Add('netware');
      Add('wdosx');
      Add('emx');
      Add('watcom');
      Add('netwlibc');
      Add('amiga');
      Add('atari');
      Add('palmos');
      Add('gba');
      Add('nds');
      Add('macos');
      Add('morphos');
      Add('embedded');
      Add('symbian');
      Add('msdos');
    end;
    ItemIndex:=0;
  end;

  with TargetCPUComboBox do begin
    with Items do begin
      Add(''); //('+rsiwpDefault+')');
      Add('arm');
      Add('i386');
      Add('m68k');
      Add('powerpc');
      Add('sparc');
      Add('x86_64');
      Add('i8086');
    end;
    ItemIndex:=0;
  end;

  SetupInfoPage;
end;

procedure TConfigureBuildLazarusDlg.FormResize(Sender: TObject);
begin
  LCLWidgetTypeComboBox.Width:=(OptionsMemo.Width - 12) div 3;
  TargetOSComboBox.Width:=LCLWidgetTypeComboBox.Width;
  DefinesListBox.Width:=(OptionsMemo.Width - 6) div 2;
end;

procedure TConfigureBuildLazarusDlg.FormShow(Sender: TObject);
begin
  UpdateProfileNamesUI;
end;

procedure TConfigureBuildLazarusDlg.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TConfigureBuildLazarusDlg.ShowOptsMenuItemClick(Sender: TObject);
begin
  CopyUIToProfile(fProfiles.Current);
  ShowMessage(fProfiles.Current.ExtraOptions);
end;

procedure TConfigureBuildLazarusDlg.TargetDirectoryButtonClick(Sender: TObject);
var
  AFilename: String;
  DirDialog: TSelectDirectoryDialog;
begin
  DirDialog:=TSelectDirectoryDialog.Create(nil);
  try
    DirDialog.Options:=DirDialog.Options+[ofPathMustExist];
    DirDialog.Title:=lisLazBuildABOChooseOutputDir+'(lazarus'+
                      GetExecutableExt(fProfiles.Current.FPCTargetOS)+')';
    if DirDialog.Execute then begin
      AFilename:=CleanAndExpandDirectory(DirDialog.Filename);
      TargetDirectoryComboBox.AddHistoryItem(AFilename,10,true,true);
    end;
  finally
    DirDialog.Free;
  end;
end;

procedure TConfigureBuildLazarusDlg.CopyProfileToUI(AProfile: TBuildLazarusProfile);
var
  i: Integer;
begin
  CleanAutoRadioButton.OnClick:=Nil;
  CleanCommonRadioButton.OnClick:=Nil;
  CleanAllRadioButton.OnClick:=Nil;
  CleanCommonCheckBox.OnClick:=Nil;
  try
    LCLWidgetTypeComboBox.ItemIndex   :=ord(AProfile.TargetPlatform);
    UpdateRevisionIncCheckBox.Checked :=AProfile.UpdateRevisionInc;
    TargetOSComboBox.Text             :=AProfile.TargetOS;
    TargetDirectoryComboBox.Text      :=AProfile.TargetDirectory;
    TargetCPUComboBox.Text            :=AProfile.TargetCPU;
    case AProfile.IdeBuildMode of
    bmBuild: CleanAutoRadioButton.Checked:=true;
    bmCleanBuild: CleanCommonRadioButton.Checked:=true;
    bmCleanAllBuild: CleanAllRadioButton.Checked:=true;
    end;
    CleanCommonCheckBox.Checked := AProfile.IdeBuildMode=bmCleanBuild;
    CleanOnceCheckBox.Checked:=AProfile.CleanOnce;
    OptionsMemo.Lines.Assign(AProfile.OptionsLines);
    for i:=0 to DefinesListBox.Items.Count-1 do
      DefinesListBox.Checked[i]:=AProfile.Defines.IndexOf(DefinesListBox.Items[i]) > -1;
  finally
    CleanAutoRadioButton.OnClick:=@CleanRadioButtonClick;
    CleanCommonRadioButton.OnClick:=@CleanRadioButtonClick;
    CleanAllRadioButton.OnClick:=@CleanRadioButtonClick;
    CleanCommonCheckBox.OnClick:=@CleanCommonCheckBoxClick;
  end;
end;

procedure TConfigureBuildLazarusDlg.CopyUIToProfile(AProfile: TBuildLazarusProfile);
var
  i: Integer;
begin
  AProfile.TargetPlatform    :=TLCLPlatform(LCLWidgetTypeComboBox.ItemIndex);
  AProfile.UpdateRevisionInc :=UpdateRevisionIncCheckBox.Checked;
  AProfile.TargetOS          :=TargetOSComboBox.Text;
  AProfile.TargetDirectory   :=TargetDirectoryComboBox.Text;
  AProfile.TargetCPU         :=TargetCPUComboBox.Text;
  if CleanAllRadioButton.Checked then
    AProfile.IdeBuildMode := bmCleanAllBuild
  else if CleanCommonRadioButton.Checked then
    AProfile.IdeBuildMode := bmCleanBuild
  else
    AProfile.IdeBuildMode := bmBuild;
  AProfile.CleanOnce:=CleanOnceCheckBox.Checked;
  AProfile.OptionsLines.Assign(OptionsMemo.Lines);
  AProfile.Defines.Clear;
  for i:=0 to DefinesListBox.Items.Count-1 do
    if DefinesListBox.Checked[i] then
      AProfile.Defines.Add(DefinesListBox.Items[i]);
end;

procedure TConfigureBuildLazarusDlg.UpdateProfileNamesUI;
var
  i: Integer;
begin
  // List of defines to checklistbox.
  DefinesListBox.Items.Clear;
  for i:=0 to fProfiles.AllDefines.Count-1 do
    DefinesListBox.Items.Add(fProfiles.AllDefines[i]);
  // Update the Profiles ComboBox.
  fUpdatingProfileCombo:=True;
  BuildProfileComboBox.Items.BeginUpdate;
  BuildProfileComboBox.Items.Clear;
  for i:=0 to fProfiles.Count-1 do
    BuildProfileComboBox.Items.Add(fProfiles[i].Name);
  BuildProfileCombobox.ItemIndex:=fProfiles.CurrentIndex;
  CopyProfileToUI(fProfiles.Current); // Copy current selection to UI.
  ShowHideCleanup(not fBuilder.IsWriteProtected(fProfiles.Current));
  BuildProfileComboBox.Items.EndUpdate;
  fUpdatingProfileCombo:=False;
  RestartAfterBuildCheckBox.Checked:=fProfiles.RestartAfterBuild;
  ConfirmBuildCheckBox.Checked     :=fProfiles.ConfirmBuild;
end;

procedure TConfigureBuildLazarusDlg.SetupInfoPage;
begin
  InfoTabSheet.Caption:=lisInformation;

  fImageIndexPackage := IDEImages.LoadImage(16, 'item_package');
  fImageIndexRequired := IDEImages.LoadImage(16, 'pkg_required');
  fImageIndexInherited := IDEImages.LoadImage(16, 'pkg_inherited');
  InhTreeView.Images := IDEImages.Images_16;

  UpdateInheritedTree;
end;

procedure TConfigureBuildLazarusDlg.UpdateInheritedTree;
var
  AncestorNode: TTreeNode;

  procedure AddChildNode(const NewNodeName, Value: string);
  var
    VisibleValue: string;
    ChildNode: TTreeNode;
  begin
    VisibleValue := UTF8Trim(Value);
    if VisibleValue = '' then
      exit;
    ChildNode := InhTreeView.Items.AddChild(AncestorNode,
      NewNodeName + ' = "' + VisibleValue + '"');
    ChildNode.ImageIndex := fImageIndexRequired;
    ChildNode.SelectedIndex := ChildNode.ImageIndex;
  end;

var
  PkgList: TFPList;
  i: Integer;
  Pkg: TLazPackage;
  AncestorOptions: TPkgAdditionalCompilerOptions;
  LazDir: String;
begin
  PkgList:=nil;
  InhTreeView.BeginUpdate;
  LazDir:=EnvironmentOptions.GetParsedLazarusDirectory;
  try
    PackageGraph.GetAllRequiredPackages(nil,
      PackageGraph.FirstAutoInstallDependency,PkgList,[pirSkipDesignTimeOnly]);

    // add detail nodes
    if PkgList<>nil then
      for i := 0 to PkgList.Count - 1 do
      begin
        Pkg:=TLazPackage(PkgList[i]);
        AncestorOptions := Pkg.UsageOptions;
        AncestorNode := InhTreeView.Items.Add(nil, '');
        AncestorNode.Text := AncestorOptions.GetOwnerName;
        AncestorNode.ImageIndex := fImageIndexPackage;
        AncestorNode.SelectedIndex := AncestorNode.ImageIndex;
        with AncestorOptions.ParsedOpts do
        begin
          AddChildNode(lisunitPath,
            CreateRelativeSearchPath(GetParsedValue(pcosUnitPath),LazDir));
          AddChildNode(lisincludePath,
            CreateRelativeSearchPath(GetParsedValue(pcosIncludePath),LazDir));
          AddChildNode(lisobjectPath,
            CreateRelativeSearchPath(GetParsedValue(pcosObjectPath),LazDir));
          AddChildNode(lislibraryPath,
            CreateRelativeSearchPath(GetParsedValue(pcosLibraryPath),LazDir));
          AddChildNode(lislinkerOptions, GetParsedValue(pcosLinkerOptions));
          AddChildNode(liscustomOptions, GetParsedValue(pcosCustomOptions));
        end;
        AncestorNode.Expanded := True;
      end;
  finally
    InhTreeView.EndUpdate;
    PkgList.Free;
  end;
end;

procedure TConfigureBuildLazarusDlg.PrepareClose;
begin
  CopyUIToProfile(fProfiles.Current);
  fProfiles.RestartAfterBuild :=RestartAfterBuildCheckBox.Checked;
  fProfiles.ConfirmBuild      :=ConfirmBuildCheckBox.Checked;
  MainIDEBar.itmToolBuildLazarus.Caption:=
    Format(lisMenuBuildLazarusProf, [fProfiles.Current.Name]);
end;

procedure TConfigureBuildLazarusDlg.ShowHideCleanup(aShow: Boolean);
// When target directory is read-only, hide Radiobuttons and show a single checkbox.
begin
  CleanAutoRadioButton.Visible:=aShow;
  CleanCommonRadioButton.Visible:=aShow;
  CleanAllRadioButton.Visible:=aShow;
  CleanOnceCheckBox.Visible:=aShow;
  CleanCommonCheckBox.Visible:=not aShow;
end;

procedure TConfigureBuildLazarusDlg.CompileAdvancedButtonClick(Sender: TObject);
// mrOk=change selected profiles. Selected profiles will be saved or discarded
// depending on the calling dialog
// mrYes=save and compile
// mrCancel=do nothing
var
  EditForm: TGenericCheckListForm;
  i, ind: Integer;
begin
  PrepareClose;
  // Add a button for building all.
  EditForm:=TGenericCheckListForm.CreateWithActionButton(lisBuild, 'menu_build');
  try
    EditForm.Caption:=lisLazBuildSelectProfilesToBuild;
    // Copy profile names to checkboxlist and check the previously selected ones.
    for i:=0 to fProfiles.Count-1 do begin
      ind:=EditForm.CheckListBox1.Items.Add(fProfiles[i].Name);
      if fProfiles.Selected.IndexOf(fProfiles[i].Name)>-1 then
        EditForm.CheckListBox1.Checked[ind]:=True;
    end;
    // Show the form.
    EditForm.ShowModal;
    if EditForm.ModalResult in [mrOK, mrYes] then begin
      // Copy checked profile names to Selected.
      fProfiles.Selected.Clear;
      for i:=0 to fProfiles.Count-1 do begin      // fProfiles and CheckListBox1
        if EditForm.CheckListBox1.Checked[i] then // indexes match now.
          fProfiles.Selected.Add(fProfiles[i].Name);
      end;
    end;
    if EditForm.ModalResult=mrYes then
      ModalResult:=mrAll;
  finally
    EditForm.Free;
  end;
end;

procedure TConfigureBuildLazarusDlg.CompileButtonClick(Sender: TObject);
begin
  PrepareClose;
  ModalResult:=mrYes;
end;

procedure TConfigureBuildLazarusDlg.SaveSettingsButtonClick(Sender: TObject);
begin
  PrepareClose;
  ModalResult:=mrOk;
end;

procedure TConfigureBuildLazarusDlg.DefinesButtonClick(Sender: TObject);
var
  EditForm: TGenericListEditForm;
  i: Integer;
begin
  EditForm:=TGenericListEditForm.Create(Nil);
  try
    EditForm.Caption:=lisLazBuildEditDefines;
    EditForm.Memo1.Lines.Assign(fProfiles.AllDefines);
    if EditForm.ShowModal=mrOK then begin
      CopyUIToProfile(fProfiles.Current); // Make sure changed fields don't get lost.
      fProfiles.AllDefines.Assign(EditForm.Memo1.Lines);
      DefinesListBox.Items.Clear;
      for i:=0 to fProfiles.AllDefines.Count-1 do
        DefinesListBox.Items.Add(fProfiles.AllDefines[i]);
      for i:=0 to DefinesListBox.Items.Count-1 do // Check the right boxes again.
        DefinesListBox.Checked[i]:=fProfiles.Current.Defines.IndexOf(DefinesListBox.Items[i]) > -1;
    end;
  finally
    EditForm.Free;
  end;
end;

procedure TConfigureBuildLazarusDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TConfigureBuildLazarusDlg.BuildProfileButtonClick(Sender: TObject);
var
  Frm: TBuildProfileManagerForm;
begin
  Frm:=TBuildProfileManagerForm.Create(nil);
  try
    CopyUIToProfile(fProfiles.Current);    // Make sure changed fields get included.
    Frm.Prepare(fProfiles);                // Copy profiles to dialog.
    if Frm.ShowModal = mrOk then begin
      fProfiles.Assign(Frm.ProfsToManage); // Copy profiles back from dialog.
      UpdateProfileNamesUI;
    end;
  finally
    Frm.Free;
  end;
end;

procedure TConfigureBuildLazarusDlg.BuildProfileComboBoxSelect(Sender: TObject);
begin
  // QT binding calls this also when items are added to list. It shouldn't.
  if (fProfiles.Count=0) or fUpdatingProfileCombo then Exit;
  if (Sender as TComboBox).ItemIndex=-1 then Exit;
  CopyUIToProfile(fProfiles.Current);      // Save old selection from UI.
  fProfiles.CurrentIndex:=(Sender as TComboBox).ItemIndex;
  CopyProfileToUI(fProfiles.Current);      // Copy new selection to UI.
  ShowHideCleanup(not fBuilder.IsWriteProtected(fProfiles.Current));
end;

procedure TConfigureBuildLazarusDlg.CleanRadioButtonClick(Sender: TObject);
begin
  CleanCommonCheckBox.Checked:=CleanCommonRadioButton.Checked;
  DebugLn(['TConfigureBuildLazarusDlg.CleanRadioButtonClick: set CleanCommonCheckBox to ', CleanCommonRadioButton.Checked]);
end;

procedure TConfigureBuildLazarusDlg.CleanCommonCheckBoxClick(Sender: TObject);
begin
  if CleanCommonCheckBox.Checked then
    CleanCommonRadioButton.Checked:=True
  else
    CleanAutoRadioButton.Checked:=True;
  DebugLn(['TConfigureBuildLazarusDlg.CleanCommonCheckBoxClick: set CleanCommonRadioButton to ', CleanCommonCheckBox.Checked]);
end;

end.

