{
 /***************************************************************************
                            initialsetupdlgs.pas
                            --------------------
       Contains the dialogs to help users setup basic settings.


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

  Author: Mattias Gaertner
  
  Abstract:
    Contains the dialogs to help users setup basic settings.
}
unit InitialSetupDlgs;

{$mode objfpc}{$H+}

{off $DEFINE VerboseFPCSrcScanThead}

interface

uses
  // RTL + FCL
  Classes, SysUtils, pkgglobals, fpmkunit,
  // LCL
  Forms, Controls, Buttons, Dialogs, Graphics, ComCtrls, ExtCtrls, StdCtrls,
  // CodeTools
  FileProcs, CodeToolManager, DefineTemplates,
  // LazUtils
  FileUtil, LazUTF8, LazUTF8Classes, LazFileUtils, LazStringUtils, LazFileCache,
  LazLoggerBase,
  // IdeIntf
  MacroDefIntf, IDEDialogs, IDEImagesIntf, IDEUtils,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // LazDebuggerGdbmi
  GDBMIDebugger,
  // IDE
  TransferMacros, LazarusIDEStrConsts, LazConf, EnvironmentOpts,
  AboutFrm, IDETranslations, BaseBuildManager, InitialSetupProc,
  {$IF FPC_FULLVERSION>30100}
  GenerateFppkgConfigurationDlg,
  {$ENDIF}
  IDEProcs;
  
type
  TInitialSetupDialog = class;

  { TSearchFpcSourceThread }

  TSearchFpcSourceThread = class(TThread)
  private
    fSetupDialog: TInitialSetupDialog;
    fFPCVer: string;
    fFoundFPCSrc: TSDFileInfo;
    {$IFDEF VerboseFPCSrcScanThead}
    fPath: string;
    fFileInfo: TSearchRec;
    procedure Debug;
    {$ENDIF}
    function CheckFPCSrcDir(Dir: string): TSDFileInfo;
    procedure DoSearch(const APath: String);
    procedure UpdateFPCSrcDir;
    procedure Finishing;
  protected
    procedure Execute; override;
  public
    constructor Create(aSetupDialog: TInitialSetupDialog);
    destructor Destroy; override;
  end;

  { TInitialSetupDialog }

  TInitialSetupDialog = class(TForm)
    BtnPanel: TPanel;
    CompilerBrowseButton: TButton;
    CompilerComboBox: TComboBox;
    CompilerLabel: TLabel;
    CompilerMemo: TMemo;
    CompilerTabSheet: TTabSheet;
    DebuggerBrowseButton: TButton;
    DebuggerComboBox: TComboBox;
    DebuggerLabel: TLabel;
    DebuggerMemo: TMemo;
    DebuggerTabSheet: TTabSheet;
    FPCSourcesTabSheet: TTabSheet;
    FPCSrcDirBrowseButton: TButton;
    FPCSrcDirComboBox: TComboBox;
    FPCSrcDirLabel: TLabel;
    FPCSrcDirMemo: TMemo;
    ImageList1: TImageList;
    LazarusTabSheet: TTabSheet;
    LazDirBrowseButton: TButton;
    LazDirComboBox: TComboBox;
    LazDirLabel: TLabel;
    LazDirMemo: TMemo;
    MakeExeBrowseButton: TButton;
    MakeExeComboBox: TComboBox;
    MakeExeLabel: TLabel;
    MakeExeMemo: TMemo;
    MakeExeTabSheet: TTabSheet;
    PropertiesPageControl: TPageControl;
    PropertiesTreeView: TTreeView;
    ScanLabel: TLabel;
    ScanProgressBar: TProgressBar;
    Splitter1: TSplitter;
    StartIDEBitBtn: TBitBtn;
    StopScanButton: TBitBtn;
    WelcomePaintBox: TPaintBox;
    FppkgTabSheet: TTabSheet;
    FppkgComboBox: TComboBox;
    FppkgLabel: TLabel;
    FppkgBrowseButton: TButton;
    FppkgMemo: TMemo;
    FppkgWriteConfigButton: TButton;
    procedure CompilerBrowseButtonClick(Sender: TObject);
    procedure CompilerComboBoxChange(Sender: TObject);
    procedure CompilerComboBoxExit(Sender: TObject);
    procedure DebuggerBrowseButtonClick(Sender: TObject);
    procedure DebuggerComboBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FPCSrcDirBrowseButtonClick(Sender: TObject);
    procedure FPCSrcDirComboBoxChange(Sender: TObject);
    procedure LazDirBrowseButtonClick(Sender: TObject);
    procedure LazDirComboBoxChange(Sender: TObject);
    procedure MakeExeBrowseButtonClick(Sender: TObject);
    procedure MakeExeComboBoxChange(Sender: TObject);
    procedure OnAppActivate(Sender: TObject);
    procedure PropertiesPageControlChange(Sender: TObject);
    procedure PropertiesTreeViewSelectionChanged(Sender: TObject);
    procedure StartIDEBitBtnClick(Sender: TObject);
    procedure StopScanButtonClick(Sender: TObject);
    procedure WelcomePaintBoxPaint(Sender: TObject);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure FppkgComboBoxChange(Sender: TObject);
    procedure FppkgBrowseButtonClick(Sender: TObject);
    procedure FppkgWriteConfigButtonClick(Sender: TObject);
  private
    FSkipDebugger: Boolean;
    FFlags: TSDFlags;
    FLastParsedLazDir: string;
    fLastParsedCompiler: string;
    fLastParsedFPCSrcDir: string;
    fLastParsedMakeExe: string;
    fLastParsedDebugger: string;
    fLastParsedFppkgConfigFile: string;
    FIdleConnected: boolean;
    ImgIDError: LongInt;
    ImgIDWarning: LongInt;
    FHeadGraphic: TPortableNetworkGraphic;
    FInitialDebuggerFileName: String;
    FSelectingPage: boolean;
    FCandidates: array[TSDFilenameType] of TSDFileInfoList; // list of TSDFileInfo
    fSearchFpcSourceThread: TSearchFpcSourceThread;
    procedure UpdateCaptions;
    procedure SelectPage(const NodeText: string);
    function SelectDirectory(aTitle: string): string;
    function SelectDirectory(aTitle: string; aPathFileName: string;
                             aEnvOptParseType: TEnvOptParseType): string; overload;
    procedure StartFPCSrcThread;
    procedure UpdateLazarusDirCandidates;
    procedure UpdateCompilerFilenameCandidates;
    procedure UpdateFPCSrcDirCandidates;
    procedure UpdateFPCSrcDirCandidate(aFPCSrcDirInfo: TSDFileInfo);
    procedure UpdateMakeExeCandidates(aStopIfFits: boolean = False);
    procedure UpdateDebuggerCandidates;
    procedure UpdateFppkgCandidates;
    procedure FillComboboxWithFileInfoList(ABox: TComboBox; List: TSDFileInfoList;
       ItemIndex: integer = 0);
    procedure SetIdleConnected(const AValue: boolean);
    procedure UpdateLazDirNote;
    procedure UpdateCompilerNote(aQuiet: boolean = False);
    procedure UpdateFPCSrcDirNote;
    procedure UpdateMakeExeNote;
    procedure UpdateDebuggerNote;
    procedure UpdateFppkgNote;
    function FirstErrorNode: TTreeNode;
    function FirstWarningNode: TTreeNode;
    function GetFirstCandidate(Candidates: TSDFileInfoList;
      MinQuality: TSDFilenameQuality = sddqCompatible): TSDFileInfo;
    function QualityToImgIndex(Quality: TSDFilenameQuality): integer;
    procedure ShowHideScanControls(aShow: Boolean);
    procedure ThreadTerminated(Sender: TObject); // called in main thread by fSearchFpcSourceThread.OnTerminate
    procedure TranslateResourceStrings;
  public
    TVNodeLazarus: TTreeNode;
    TVNodeCompiler: TTreeNode;
    TVNodeFPCSources: TTreeNode;
    TVNodeMakeExe: TTreeNode;
    TVNodeDebugger: TTreeNode;
    TVNodeFppkg: TTreeNode;
    procedure Init; //Check for config errors, find and show alternatives
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;


function ShowInitialSetupDialog: TModalResult;

// Debugger
// Checks a given file to see if it is a valid debugger (only gdb supported for now)
function CheckDebuggerQuality(AFilename: string; out Note: string; ASkip: Boolean = False): TSDFilenameQuality;
// Search debugger candidates and add them to list, including quality level
function SearchDebuggerCandidates(StopIfFits: boolean): TSDFileInfoList;

implementation

const
  DefaultDebuggerClass: TDebuggerClass = TGDBMIDebugger;

type

  { TSetupMacros }

  TSetupMacros = class(TTransferMacroList)
  protected
    procedure DoSubstitution({%H-}TheMacro: TTransferMacro; const MacroName: string;
      var s: string; const {%H-}Data: PtrInt; var Handled, {%H-}Abort: boolean;
      {%H-}Depth: integer); override;
  public
    FPCVer: string;
    LazarusDir: string;
  end;

function CheckDebuggerQuality(AFilename: string; out Note: string;
  ASkip: Boolean): TSDFilenameQuality;
begin
  Note := '';
  Result:=sddqCompatible;
  if ASkip and // assume compatible
     ( (EnvironmentOptions.CurrentDebuggerPropertiesConfig = nil) or
       (EnvironmentOptions.CurrentDebuggerPropertiesConfig.DebuggerFilename = AFilename)   // unless the user edited the filename
     )
  then
    exit;
  Result:=sddqInvalid;
  AFilename:=TrimFilename(AFilename);
  if not FileExistsCached(AFilename) then
  begin
    Note:=lisFileNotFound4;
    exit;
  end;
  if DirPathExistsCached(AFilename) then
  begin
    Note:=lisFileIsDirectory;
    exit;
  end;
  if not FileIsExecutableCached(AFilename) then
  begin
    Note:=lisFileIsNotAnExecutable;
    exit;
  end;

  { We could call gdb and parse the output looking for something like
  GNU gdb, but that may be going too far. }
  Note:=lisOk;
  Result:=sddqCompatible;
end;

function SearchDebuggerCandidates(StopIfFits: boolean): TSDFileInfoList;

  function CheckFile(AFilename: string; var List: TSDFileInfoList): boolean;
  var
    Item: TSDFileInfo;
    RealFilename: String;
  begin
    Result:=false;
    if AFilename='' then exit;
    ForcePathDelims(AFilename);
    // check if already checked
    if Assigned(List) and List.CaptionExists(AFilename) then exit;
    RealFilename:=EnvironmentOptions.GetParsedValue(eopDebuggerFilename, AFilename);
    debugln(['SearchDebuggerCandidates Value=',AFilename,' File=',RealFilename]);
    if RealFilename='' then exit;
    // check if exists
    if not FileExistsCached(RealFilename) then exit;
    // add to list and check quality
    if List=nil then
      List:=TSDFileInfoList.create(true);
    Item:=List.AddNewItem(RealFilename, AFilename);
    Item.Quality:=CheckDebuggerQuality(RealFilename, Item.Note);
    Result:=(Item.Quality=sddqCompatible) and StopIfFits;
  end;

const
  DebuggerFileName='gdb'; //For Windows, .exe will be appended
var
  s, AFilename, XmlClassName, CurDbgClassName: String;
  Files: TStringList;
  i: Integer;
begin
  Result:=nil;

  // check current setting
  if CheckFile(EnvironmentOptions.DebuggerFilename,Result) then exit;

  if EnvironmentOptions.CurrentDebuggerPropertiesConfig <> nil then
    CurDbgClassName := UpperCase(EnvironmentOptions.CurrentDebuggerPropertiesConfig.ConfigClass)
  else
    CurDbgClassName := UpperCase(DefaultDebuggerClass.ClassName);

  // check the primary options
  XmlClassName :=GetValueFromPrimaryConfig(EnvOptsConfFileName,
                                  'EnvironmentOptions/Debugger/Class');
  if UpperCase(XmlClassName) = CurDbgClassName then begin
    AFilename:=GetValueFromPrimaryConfig(EnvOptsConfFileName,
                                    'EnvironmentOptions/DebuggerFilename/Value');
    if CheckFile(AFilename,Result) then exit;
  end;

  // check the secondary options
  XmlClassName :=GetValueFromSecondaryConfig(EnvOptsConfFileName,
                                  'EnvironmentOptions/Debugger/Class');
  if UpperCase(XmlClassName) = CurDbgClassName then begin
    AFilename:=GetValueFromSecondaryConfig(EnvOptsConfFileName,
                                    'EnvironmentOptions/DebuggerFilename/Value');
    if CheckFile(AFilename,Result) then exit;
  end;

  // Check locations proposed by debugger class
  if EnvironmentOptions.CurrentDebuggerClass <> nil then
    s := EnvironmentOptions.CurrentDebuggerClass.ExePaths
  else
    s := DefaultDebuggerClass.ExePaths;
  while s <> '' do begin
    AFilename := GetPart([], [';'], s);
    if CheckFile(AFilename, Result) then exit;
    if s <> '' then delete(s, 1, 1);
  end;

  // Search for gdb
  // only if TGDBMIDebugger
  if CurDbgClassName = UpperCase(DefaultDebuggerClass.ClassName) then begin

    // Windows-only locations:
    if (GetDefaultSrcOSForTargetOS(GetCompiledTargetOS)='win') then begin
      // check for debugger in fpc.exe directory - could be a lucky shot
      if CheckFile(GetForcedPathDelims('$Path($(CompPath))/'+DebuggerFileName+GetExecutableExt),Result)
        then exit;
    end;

    // check history
    Files:=EnvironmentOptions.DebuggerFileHistory[CurDbgClassName];
    if (Files=nil) or (Files.Count=0) then
      Files:=EnvironmentOptions.DebuggerFileHistory[''];
    if Files<>nil then
      for i:=0 to Files.Count-1 do
        if CheckFile(Files[i],Result) then exit;

    // check PATH
    AFilename:=DebuggerFileName+GetExecutableExt;
    if CheckFile(AFilename,Result) then exit;
  end;

  // There are no common directories apart from the PATH
  // where gdb would be installed. Otherwise we could do something similar as
  // in SearchMakeExeCandidates.
end;

function ShowInitialSetupDialog: TModalResult;
var
  InitialSetupDialog: TInitialSetupDialog;
begin
  InitialSetupDialog:=TInitialSetupDialog.Create(nil);
  try
    Application.TaskBarBehavior:=tbMultiButton;
    InitialSetupDialog.Init;
    Result:=InitialSetupDialog.ShowModal;
  finally
    InitialSetupDialog.Free;
    Application.TaskBarBehavior:=tbDefault;
  end;
end;

{ TSearchFpcSourceThread }

constructor TSearchFpcSourceThread.Create(aSetupDialog: TInitialSetupDialog);
begin
  inherited Create(True);
  FreeOnTerminate:=True;
  fSetupDialog:=aSetupDialog;
end;

destructor TSearchFpcSourceThread.Destroy;
begin
  inherited Destroy;
end;

procedure TSearchFpcSourceThread.Execute;
var
  RootDir: String;
begin
  // ToDo: RootDir must be changed for Windows and maybe other systems.
  //       GetUserDir returns the user profile dir on Windows.
  RootDir:=GetUserDir;
  // Scan directories under root directory.
  DoSearch(AppendPathDelim(RootDir));
  if Assigned(fFoundFPCSrc) then
    Synchronize(@UpdateFPCSrcDir); // Update GUI in main thread.
  Synchronize(@Finishing);
end;

function TSearchFpcSourceThread.CheckFPCSrcDir(Dir: string): TSDFileInfo;
var
  RealDir: String;
begin
  Result:=Nil;
  RealDir:=TrimFilename(Dir);
  if RealDir='' then exit;
  if not DirPathExistsCached(RealDir) then exit;   // check if exists
  Result:=TSDFileInfo.Create;
  Result.Filename:=RealDir;
  Result.Caption:=Dir;                             // check quality
  Result.Quality:=CheckFPCSrcDirQuality(RealDir, Result.Note, fFPCVer, False);
  if Result.Quality<>sddqCompatible then           // return only exact matches
    FreeAndNil(Result);
end;

procedure TSearchFpcSourceThread.DoSearch(const APath: String);
var
  PathInfo: TSearchRec;
  FPCSrc: TSDFileInfo;
begin
  if FindFirstUTF8(APath+AllDirectoryEntriesMask, faDirectory, PathInfo) = 0 then
  try
    repeat
      if Terminated then Break;
      if (PathInfo.Name='') or (PathInfo.Name[1]='.')
      or ((PathInfo.Attr and faDirectory) = 0) then Continue;
      {$IFDEF VerboseFPCSrcScanThead}
      fPath := APath;
      fFileInfo := PathInfo;
      Synchronize(@Debug);
      {$ENDIF}
      DoSearch(AppendPathDelim(APath+PathInfo.Name));  // Recursive call
      FPCSrc:=CheckFPCSrcDir(APath+PathInfo.Name);
      if Assigned(FPCSrc) then begin
        fFoundFPCSrc:=FPCSrc;                 // An exact match was found.
        Terminate;
      end;
    until (FindNextUTF8(PathInfo) <> 0);
  finally
    FindCloseUTF8(PathInfo);
  end;
end;

{$IFDEF VerboseFPCSrcScanThead}
procedure TSearchFpcSourceThread.Debug;
begin
  DebugLn(['* TSearchFpcSourceThread.Debug: Path=', fPath, ', Name=', fFileInfo.Name]);
end;
{$ENDIF}

procedure TSearchFpcSourceThread.UpdateFPCSrcDir;
begin
  DebugLn(['TSearchFpcSourceThread.UpdateFPCSrcDir']);
  fSetupDialog.UpdateFPCSrcDirCandidate(fFoundFPCSrc);
  fSetupDialog.UpdateFPCSrcDirNote;
end;

procedure TSearchFpcSourceThread.Finishing;
begin
  DebugLn(['TSearchFpcSourceThread.Finishing']);
  fSetupDialog.ShowHideScanControls(False); // Hide scan controls
end;

{ TSetupMacros }

procedure TSetupMacros.DoSubstitution(TheMacro: TTransferMacro;
  const MacroName: string; var s: string; const Data: PtrInt; var Handled,
  Abort: boolean; Depth: integer);
begin
  Handled:=true;
  if CompareText(MacroName,'ENV')=0 then
    s:=GetEnvironmentVariableUTF8(MacroName)
  else if CompareText(MacroName,'PrimaryConfigPath')=0 then
    s:=GetPrimaryConfigPath
  else if CompareText(MacroName,'SecondaryConfigPath')=0 then
    s:=GetSecondaryConfigPath
  else if CompareText(MacroName,'FPCVer')=0 then begin
    if FPCVer<>'' then
      s:=FPCVer
    else
      s:={$I %FPCVERSION%};
  end else if CompareText(MacroName,'LazarusDir')=0 then begin
    if LazarusDir<>'' then
      s:=LazarusDir
    else
      s:='<LazarusDirNotSet>';
  end else if (CompareText(MacroName,'TargetOS')=0) then
    s:=GetCompiledTargetOS
  else if (CompareText(MacroName,'TargetCPU')=0) then
    s:=GetCompiledTargetCPU
  else if (CompareText(MacroName,'SrcOS')=0) then
    s:=GetDefaultSrcOSForTargetOS(GetCompiledTargetOS)
  else
    Handled:=false;
  //debugln(['TSetupMacros.DoSubstitution MacroName=',MacroName,' Value="',s,'"']);
end;

{$R *.lfm}

{ TInitialSetupDialog }

procedure TInitialSetupDialog.FormCreate(Sender: TObject);
begin
  LazarusTabSheet.Caption:='Lazarus';
  CompilerTabSheet.Caption:=lisCompiler;
  FPCSourcesTabSheet.Caption:=lisFPCSources;
  MakeExeTabSheet.Caption:='Make';
  DebuggerTabSheet.Caption:=lisDebugger;
  FppkgTabSheet.Caption := 'Fppkg';

  FHeadGraphic:=TPortableNetworkGraphic.Create;
  FHeadGraphic.LoadFromResourceName(HInstance, 'ide_icon48x48');

  TVNodeLazarus:=PropertiesTreeView.Items.Add(nil,LazarusTabSheet.Caption);
  TVNodeCompiler:=PropertiesTreeView.Items.Add(nil,CompilerTabSheet.Caption);
  TVNodeFPCSources:=PropertiesTreeView.Items.Add(nil,FPCSourcesTabSheet.Caption);
  TVNodeMakeExe:=PropertiesTreeView.Items.Add(nil,MakeExeTabSheet.Caption);
  TVNodeDebugger:=PropertiesTreeView.Items.Add(nil,DebuggerTabSheet.Caption);
  {$IF FPC_FULLVERSION>30100}
  TVNodeFppkg:=PropertiesTreeView.Items.Add(nil,FppkgTabSheet.Caption);
  FppkgTabSheet.TabVisible := True;
  {$ELSE}
  FppkgTabSheet.TabVisible := False;
  {$ENDIF FPC_FULLVERSION>30100}
  ImgIDError := Imagelist1.AddResourceName(HInstance, 'state_error');
  ImgIDWarning := Imagelist1.AddResourceName(HInstance, 'state_warning');

  IDEImages.AssignImage(StopScanButton, 'menu_stop');

  UpdateCaptions;

  Application.AddOnActivateHandler(@OnAppActivate);
end;

procedure TInitialSetupDialog.CompilerComboBoxChange(Sender: TObject);
begin
  UpdateCompilerNote({Quiet} True);
  UpdateFPCSrcDirNote;
end;

procedure TInitialSetupDialog.CompilerComboBoxExit(Sender: TObject);
begin
  UpdateCompilerNote({Quiet} False);
  UpdateFPCSrcDirNote;
end;

procedure TInitialSetupDialog.DebuggerBrowseButtonClick(Sender: TObject);
var
  lExpandedName: string; // Expanded name before Dialog
  lDirName, lFileName: string;
  lTitle: string;
  lChanged: boolean=False;
  Dlg: TIDEOpenDialog;
  Filter: String;
begin
  Dlg:=IDEOpenDialogClass.Create(nil);
  try
    lTitle := 'gdb'+GetExecutableExt;
    Dlg.Title := SimpleFormat(lisSelectPathTo, [lTitle]);
    lExpandedName := EnvironmentOptions.GetParsedValue(eopDebuggerFilename, DebuggerComboBox.Text);
    lDirName := GetValidDirectory(lExpandedName, {out} lFileName);
    Dlg.Options := Dlg.Options+[ofFileMustExist];
    if lFileName='' then
      lFileName := lTitle;
    Filter := dlgFilterAll+'|'+GetAllFilesMask;
    if ExtractFileExt(lFileName)<>'' then
      Filter := dlgFilterExecutable+'|*'+ExtractFileExt(lFileName)+'|'+Filter;
    Dlg.Filter := Filter;
    Dlg.InitialDir := lDirName;
    Dlg.FileName := lFileName;
    if not Dlg.Execute then
      exit;
    lFileName := CleanAndExpandFilename(Dlg.Filename);
    lChanged := UpperCase(lExpandedName)<>UpperCase(lFileName);
  finally
    Dlg.Free;
  end;
  if lChanged then begin // Avoid loosing $(macros)
    DebuggerComboBox.Text := lFileName;
    UpdateDebuggerNote;
  end;
end;

procedure TInitialSetupDialog.DebuggerComboBoxChange(Sender: TObject);
begin
  UpdateDebuggerNote;
end;

procedure TInitialSetupDialog.CompilerBrowseButtonClick(Sender: TObject);
var
  lExpandedName: string; // Expanded name before Dialog
  lDirName, lFileName: string;
  lTitle: string;
  lChanged: boolean=False;
  Dlg: TIDEOpenDialog;
  Filter: String;
begin
  Dlg := IDEOpenDialogClass.Create(nil);
  try
    lTitle := 'fpc'+GetExecutableExt;
    Dlg.Title := SimpleFormat(lisSelectPathTo, [lTitle]);
    lExpandedName := EnvironmentOptions.GetParsedValue(eopCompilerFilename, CompilerComboBox.Text);
    lDirName := GetValidDirectory(lExpandedName, {out} lFileName);
    Dlg.Options := Dlg.Options+[ofFileMustExist];
    if lFileName='' then
      lFileName := lTitle;
    Filter := dlgFilterAll+'|'+GetAllFilesMask;
    if ExtractFileExt(lFileName)<>'' then
      Filter := dlgFilterExecutable+'|*'+ExtractFileExt(lFileName)+'|'+Filter;
    Dlg.Filter := Filter;
    Dlg.InitialDir := lDirName;
    Dlg.FileName := lFileName;
    if not Dlg.Execute then
      exit;
    lFileName := CleanAndExpandFilename(Dlg.Filename);
    lChanged := UpperCase(lExpandedName)<>UpperCase(lFileName);
  finally
    Dlg.Free;
  end;
  if lChanged then begin // Avoid loosing $(macros)
    CompilerComboBox.Text := lFileName;
    UpdateCompilerNote;
  end;
end;

procedure TInitialSetupDialog.FormDestroy(Sender: TObject);
var
  d: TSDFilenameType;
begin
  IdleConnected:=false;
  if Assigned(fSearchFpcSourceThread) then begin
    fSearchFpcSourceThread.Terminate;
    fSearchFpcSourceThread.WaitFor;
  end;
  for d:=low(FCandidates) to high(FCandidates) do
    FreeAndNil(FCandidates[d]);
  FreeAndNil(FHeadGraphic);
end;

procedure TInitialSetupDialog.FPCSrcDirBrowseButtonClick(Sender: TObject);
var
  Dir: String;
begin
  Dir:=SelectDirectory(lisSelectFPCSourceDirectory, FPCSrcDirComboBox.Text,eopFPCSourceDirectory);
  if Dir='' then
    exit;
  FPCSrcDirComboBox.Text:=Dir;
  UpdateFPCSrcDirNote;
end;

procedure TInitialSetupDialog.FPCSrcDirComboBoxChange(Sender: TObject);
begin
  UpdateFPCSrcDirNote;
end;

procedure TInitialSetupDialog.LazDirBrowseButtonClick(Sender: TObject);
var
  Dir: String;
begin
  Dir:=SelectDirectory(lisSelectLazarusSourceDirectory,LazDirComboBox.Text,eopLazarusDirectory);
  if Dir='' then
    exit;
  LazDirComboBox.Text:=Dir;
  UpdateLazDirNote;
end;

procedure TInitialSetupDialog.LazDirComboBoxChange(Sender: TObject);
begin
  UpdateLazDirNote;
end;

procedure TInitialSetupDialog.MakeExeBrowseButtonClick(Sender: TObject);
var
  lExpandedName: string; // Expanded name before Dialog
  lDirName, lFileName: string;
  lTitle: string;
  lChanged: boolean=False;
  Dlg: TIDEOpenDialog;
  Filter: String;

begin
  Dlg := IDEOpenDialogClass.Create(nil);
  try
    lTitle := 'make'+GetExecutableExt;
    Dlg.Title := SimpleFormat(lisSelectPathTo, [lTitle]);
    lExpandedName := EnvironmentOptions.GetParsedValue(eopMakeFilename, MakeExeComboBox.Text);
    lDirName := GetValidDirectory(lExpandedName, {out} lFileName);
    Dlg.Options := Dlg.Options+[ofFileMustExist];
    if lFileName='' then
      lFileName := lTitle;
    Filter := dlgFilterAll+'|'+GetAllFilesMask;
    if ExtractFileExt(lFileName)<>'' then
      Filter := dlgFilterExecutable+'|*'+ExtractFileExt(lFileName)+'|'+Filter;
    Dlg.Filter := Filter;
    Dlg.InitialDir := lDirName;
    Dlg.FileName := lFileName;
    if not Dlg.Execute then
      exit;
    lFileName := CleanAndExpandFilename(Dlg.Filename);
    lChanged := UpperCase(lExpandedName)<>UpperCase(lFileName);
  finally
    Dlg.Free;
  end;
  if lChanged then begin // Avoid loosing $(macros)
    MakeExeComboBox.Text := lFileName;
    UpdateMakeExeNote;
  end;
end;

procedure TInitialSetupDialog.MakeExeComboBoxChange(Sender: TObject);
begin
  UpdateMakeExeNote;
end;

procedure TInitialSetupDialog.OnAppActivate(Sender: TObject);
begin
  // switched back from another application
  InvalidateFileStateCache;
end;

procedure TInitialSetupDialog.PropertiesPageControlChange(Sender: TObject);
var
  s: String;
  i: Integer;
begin
  if PropertiesPageControl.ActivePage=nil then exit;
  s:=PropertiesPageControl.ActivePage.Caption;
  for i:=0 to PropertiesTreeView.Items.TopLvlCount-1 do
    if PropertiesTreeView.Items.TopLvlItems[i].Text=s then
      PropertiesTreeView.Selected:=PropertiesTreeView.Items.TopLvlItems[i];
end;

procedure TInitialSetupDialog.PropertiesTreeViewSelectionChanged(Sender: TObject);
begin
  if PropertiesTreeView.Selected=nil then
    SelectPage(TVNodeLazarus.Text)
  else
    SelectPage(PropertiesTreeView.Selected.Text);
end;

procedure TInitialSetupDialog.StartIDEBitBtnClick(Sender: TObject);
var
  Node: TTreeNode;
  s: String;
  MsgResult: TModalResult;
begin
  Node:=FirstErrorNode;
  s:='';
  if Node=TVNodeLazarus then
    s:=lisWithoutAProperLazarusDirectoryYouWillGetALotOfWarn
  else if Node=TVNodeCompiler then
    s:=lisWithoutAProperCompilerTheCodeBrowsingAndCompilingW
  else if Node=TVNodeFPCSources then
    s:=lisWithoutTheProperFPCSourcesCodeBrowsingAndCompletio
  else if Node=TVNodeMakeExe then
    s:=lisWithoutAProperMakeExecutableTheCompilingOfTheIDEIs
  else if Node=TVNodeDebugger then
    s:=lisWithoutAProperDebuggerDebuggingWillBeDisappointing;
  if s<>'' then begin
    MsgResult:=MessageDlg(lisCCOWarningCaption, s, mtWarning, [mbIgnore,
      mbCancel], 0);
    if MsgResult<>mrIgnore then exit;
  end;

  s:=LazDirComboBox.Text;
  if s<>'' then
    EnvironmentOptions.LazarusDirectory:=s;
  s:=CompilerComboBox.Text;
  if s<>'' then
    EnvironmentOptions.CompilerFilename:=s;
  s:=FPCSrcDirComboBox.Text;
  if s<>'' then
    EnvironmentOptions.FPCSourceDirectory:=s;
  s:=MakeExeComboBox.Text;
  if s<>'' then
    EnvironmentOptions.MakeFilename:=s;
  if not (FSkipDebugger and (EnvironmentOptions.CurrentDebuggerPropertiesConfig <> nil))
  then begin
    s:=DebuggerComboBox.Text;
    if s<>'' then begin
      EnvironmentOptions.CurrentDebuggerPropertiesConfig.DebuggerFilename:=s;
      EnvironmentOptions.SaveDebuggerPropertiesList; // Update XML
    end;
  end;

  ModalResult:=mrOk;
end;

procedure TInitialSetupDialog.StopScanButtonClick(Sender: TObject);
begin
  if fSearchFpcSourceThread<>nil then
    fSearchFpcSourceThread.Terminate;
end;

procedure TInitialSetupDialog.WelcomePaintBoxPaint(Sender: TObject);
begin
  with WelcomePaintBox.Canvas do begin
    GradientFill(WelcomePaintBox.ClientRect,$854b32,$c88e60,gdHorizontal);
    Draw(0,WelcomePaintBox.ClientHeight-FHeadGraphic.Height,FHeadGraphic);
    Font.Color:=clWhite;
    Font.Height:=30;
    Brush.Style:=bsClear;
    TextOut(FHeadGraphic.Width+15, 5, lisConfigureLazarusIDE);
  end;
end;

procedure TInitialSetupDialog.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if sdfCompilerFilenameNeedsUpdate in FFlags then begin
    UpdateCompilerFilenameCandidates;
    UpdateCompilerNote;
  end else if sdfFPCSrcDirNeedsUpdate in FFlags then begin
    UpdateFPCSrcDirCandidates;
    UpdateFPCSrcDirNote;
  end else if sdfMakeExeFilenameNeedsUpdate in FFlags then begin
    UpdateMakeExeCandidates;
    UpdateMakeExeNote;
  end else if sdfDebuggerFilenameNeedsUpdate in FFlags then begin
    UpdateDebuggerCandidates;
    UpdateDebuggerNote;
  end else if sdfFppkgConfigFileNeedsUpdate in FFlags then begin
    fLastParsedFppkgConfigFile := ' ';
    UpdateFppkgCandidates;
    UpdateFppkgNote;
  end else
    IdleConnected:=false;
end;

procedure TInitialSetupDialog.UpdateCaptions;
var
  s: String;
begin
  Caption:=SimpleFormat(lisWelcomeToLazarusIDE, [GetLazarusVersionString]);

  StartIDEBitBtn.Caption:=lisStartIDE;

  LazarusTabSheet.Caption:='Lazarus';
  CompilerTabSheet.Caption:=lisCompiler;
  FPCSourcesTabSheet.Caption:=lisFPCSources;
  MakeExeTabSheet.Caption:='Make';
  DebuggerTabSheet.Caption:=lisDebugger;
  FppkgTabSheet.Caption:='Fppkg';

  TVNodeLazarus.Text:=LazarusTabSheet.Caption;
  TVNodeCompiler.Text:=CompilerTabSheet.Caption;
  TVNodeFPCSources.Text:=FPCSourcesTabSheet.Caption;
  TVNodeMakeExe.Text:=MakeExeTabSheet.Caption;
  TVNodeDebugger.Text:=DebuggerTabSheet.Caption;
  {$IF FPC_FULLVERSION>30100}
  TVNodeFppkg.Text:=FppkgTabSheet.Caption;
  {$ENDIF FPC_FULLVERSION>30100}

  LazDirBrowseButton.Caption:=lisPathEditBrowse;
  LazDirLabel.Caption:=SimpleFormat(
    lisTheLazarusDirectoryContainsTheSourcesOfTheIDEAndTh, [PathDelim]);

  FppkgLabel.Caption:=lisFppkgConfiguration;
  FppkgBrowseButton.Caption:=lisPathEditBrowse;
  FppkgWriteConfigButton.Caption:=lisCreateFppkgConfig;

  CompilerBrowseButton.Caption:=lisPathEditBrowse;
  CompilerLabel.Caption:=SimpleFormat(lisTheFreePascalCompilerExecutableTypicallyHasTheName,
    [DefineTemplates.GetDefaultCompilerFilename,
     DefineTemplates.GetDefaultCompilerFilename(GetCompiledTargetCPU)]);

  FPCSrcDirBrowseButton.Caption:=lisPathEditBrowse;
  FPCSrcDirLabel.Caption:=SimpleFormat(lisTheSourcesOfTheFreePascalPackagesAreRequiredForBro,
    [GetForcedPathDelims('rtl/linux/system.pp')]);
  ScanLabel.Caption := lisScanning;
  StopScanButton.Caption:=lisStop;

  MakeExeBrowseButton.Caption:=lisPathEditBrowse;
  MakeExeLabel.Caption:=SimpleFormat(
    lisTheMakeExecutableTypicallyHasTheName, ['make'+GetExecutableExt('')]);

  DebuggerBrowseButton.Caption:=lisPathEditBrowse;
  s:=SimpleFormat(lisTheDebuggerExecutableTypicallyHasTheNamePleaseGive, [
    'gdb'+GetExecutableExt]);
  {$IFDEF Windows}
  s+=' '+lisAUsefulSettingOnWindowsSystemsIsLazarusDirMingwBin;
  {$ENDIF}
  DebuggerLabel.Caption:=s;
end;

procedure TInitialSetupDialog.SelectPage(const NodeText: string);
var
  i: Integer;
  Node: TTreeNode;
begin
  if FSelectingPage then exit;
  FSelectingPage:=true;
  try
    for i:=0 to PropertiesTreeView.Items.TopLvlCount-1 do begin
      Node:=PropertiesTreeView.Items.TopLvlItems[i];
      if Node.Text=NodeText then begin
        PropertiesTreeView.Selected:=Node;
        PropertiesPageControl.ActivePageIndex:=i;
        break;
      end;
    end;
  finally
    FSelectingPage:=false;
  end;
end;

function TInitialSetupDialog.SelectDirectory(aTitle: string): string;
var
  DirDlg: TSelectDirectoryDialog;
begin
  Result:='';
  DirDlg:=TSelectDirectoryDialog.Create(nil);
  try
    DirDlg.Title:=aTitle;
    DirDlg.Options:=DirDlg.Options+[ofPathMustExist,ofFileMustExist];
    if not DirDlg.Execute then exit;
    Result:=DirDlg.FileName;
  finally
    DirDlg.Free;
  end;
end;

function TInitialSetupDialog.SelectDirectory(aTitle: string;
  aPathFileName: string; aEnvOptParseType: TEnvOptParseType): string;
var
  DirDlg: TSelectDirectoryDialog;
  lCurDirName: string;
  lDirPath: string;
  lDirName: string;
begin
  Result := '';
  if aPathFileName='' then
    case aEnvOptParseType of
      eopLazarusDirectory: lDirPath := EnvironmentOptions.GetParsedLazarusDirectory;
      eopFPCSourceDirectory: lDirPath := EnvironmentOptions.GetParsedFPCSourceDirectory;
    end
  else
    lDirPath := EnvironmentOptions.GetParsedValue(eopLazarusDirectory, aPathFileName);
  lCurDirName := CleanAndExpandFilename(ExcludeTrailingBackSlash(lDirPath));
  lDirPath := GetValidDirectoryAndFilename(lCurDirName, {out} lDirName);
  { ~bk
  if lDirName = '' then begin
     lDirName := ExtractFileName(lDirPath);
     lDirPath := ExtractFilePath(lDirPath);
  end;
  }
  lDirPath := ExcludeTrailingBackSlash(lDirPath);
  DirDlg := TSelectDirectoryDialog.Create(nil);
  try
    DirDlg.Title := aTitle;
    DirDlg.InitialDir := lDirPath;
    DirDlg.FileName := lDirName;
    DirDlg.Options := DirDlg.Options + [ofPathMustExist]; // ~bk, ofFileMustExist];
    if DirDlg.Execute then begin
      lDirName := CleanAndExpandFilename(DirDlg.FileName);
      if UpperCase(lCurDirName)<>UpperCase(lDirName) then
        Result := lDirName;
    end;
  finally
    DirDlg.Free;
  end;
end;

procedure TInitialSetupDialog.StartFPCSrcThread;
begin
  fSearchFpcSourceThread:=TSearchFpcSourceThread.Create(Self);
  fSearchFpcSourceThread.OnTerminate:=@ThreadTerminated;
  fSearchFpcSourceThread.fFPCVer:=GetFPCVer;
  ShowHideScanControls(True); // Show scan controls while thread is running
  fSearchFpcSourceThread.Start;
end;

procedure TInitialSetupDialog.UpdateLazarusDirCandidates;
var
  Dirs: TSDFileInfoList;
begin
  Dirs:=SearchLazarusDirectoryCandidates(false);
  FreeAndNil(FCandidates[sddtLazarusSrcDir]);
  FCandidates[sddtLazarusSrcDir]:=Dirs;
  FillComboboxWithFileInfoList(LazDirComboBox,Dirs);
end;

procedure TInitialSetupDialog.UpdateCompilerFilenameCandidates;
var
  Files: TSDFileInfoList;
begin
  Exclude(FFlags,sdfCompilerFilenameNeedsUpdate);
  Files:=SearchFPCExeCandidates(false,CodeToolBoss.CompilerDefinesCache.TestFilename);
  FreeAndNil(FCandidates[sddtCompilerFilename]);
  FCandidates[sddtCompilerFilename]:=Files;
  FillComboboxWithFileInfoList(CompilerComboBox,Files);
end;

procedure TInitialSetupDialog.UpdateFPCSrcDirCandidates;
var
  Dirs: TSDFileInfoList;
begin
  Exclude(FFlags,sdfFPCSrcDirNeedsUpdate);
  Dirs:=SearchFPCSrcDirCandidates(false,GetFPCVer);
  FreeAndNil(FCandidates[sddtFPCSrcDir]);
  FCandidates[sddtFPCSrcDir]:=Dirs;
  FillComboboxWithFileInfoList(FPCSrcDirComboBox,Dirs);
end;

procedure TInitialSetupDialog.UpdateFPCSrcDirCandidate(aFPCSrcDirInfo: TSDFileInfo);
var
  Dirs: TSDFileInfoList;
begin
  Exclude(FFlags,sdfFPCSrcDirNeedsUpdate);
  FreeAndNil(FCandidates[sddtFPCSrcDir]);
  Dirs:=TSDFileInfoList.Create;
  Dirs.Add(aFPCSrcDirInfo);
  FCandidates[sddtFPCSrcDir]:=Dirs;
  FillComboboxWithFileInfoList(FPCSrcDirComboBox,Dirs);
end;

procedure TInitialSetupDialog.UpdateMakeExeCandidates(aStopIfFits: boolean);
var
  Files: TSDFileInfoList;
begin
  Exclude(FFlags,sdfMakeExeFilenameNeedsUpdate);
  Files:=SearchMakeExeCandidates(aStopIfFits);
  FreeAndNil(FCandidates[sddtMakeExeFileName]);
  FCandidates[sddtMakeExeFileName]:=Files;
  FillComboboxWithFileInfoList(MakeExeComboBox,Files);
end;

procedure TInitialSetupDialog.UpdateDebuggerCandidates;
var
  Files: TSDFileInfoList;
begin
  Exclude(FFlags,sdfDebuggerFilenameNeedsUpdate);
  Files:=SearchDebuggerCandidates(false);
  FreeAndNil(FCandidates[sddtDebuggerFilename]);
  FCandidates[sddtDebuggerFilename]:=Files;
  FillComboboxWithFileInfoList(DebuggerComboBox,Files);
end;

procedure TInitialSetupDialog.FillComboboxWithFileInfoList(ABox: TComboBox;
  List: TSDFileInfoList; ItemIndex: integer);
var
  sl: TStringList;
  i: Integer;
begin
  sl:=TStringList.Create;
  try
    if List<>nil then
      for i:=0 to List.Count-1 do
        sl.Add(TSDFileInfo(List[i]).Caption);
    ABox.Items.Assign(sl);
    if (ItemIndex>=0) and (ItemIndex<sl.Count) then
      ABox.Text:=sl[ItemIndex]
    else if ABox.Text=ABox.Name then
      ABox.Text:='';
  finally
    sl.Free;
  end;
end;

procedure TInitialSetupDialog.SetIdleConnected(const AValue: boolean);
begin
  if FIdleConnected=AValue then exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TInitialSetupDialog.UpdateLazDirNote;
var
  CurCaption: String;
  Note: string;
  Quality: TSDFilenameQuality;
  s: String;
  ImageIndex: Integer;
begin
  if csDestroying in ComponentState then exit;
  CurCaption:=LazDirComboBox.Text;
  CurCaption:=ChompPathDelim(CurCaption);
  EnvironmentOptions.LazarusDirectory:=CurCaption;
  if FLastParsedLazDir=EnvironmentOptions.GetParsedLazarusDirectory then exit;
  FLastParsedLazDir:=EnvironmentOptions.GetParsedLazarusDirectory;
  //debugln(['TInitialSetupDialog.UpdateLazDirNote ',FLastParsedLazDir]);
  Quality:=CheckLazarusDirectoryQuality(FLastParsedLazDir,Note);
  case Quality of
  sddqInvalid: s:=lisError;
  sddqCompatible: s:='';
  else s:=lisWarning;
  end;
  if EnvironmentOptions.LazarusDirectory<>EnvironmentOptions.GetParsedLazarusDirectory
  then
    s:=lisDirectory+EnvironmentOptions.GetParsedLazarusDirectory+LineEnding+
      LineEnding+s;
  LazDirMemo.Text:=s+Note;

  ImageIndex:=QualityToImgIndex(Quality);
  TVNodeLazarus.ImageIndex:=ImageIndex;
  TVNodeLazarus.SelectedIndex:=ImageIndex;

  FFlags:=FFlags+[sdfCompilerFilenameNeedsUpdate,sdfFPCSrcDirNeedsUpdate,
                  sdfMakeExeFilenameNeedsUpdate,sdfDebuggerFilenameNeedsUpdate];
  IdleConnected:=true;
end;

procedure TInitialSetupDialog.UpdateCompilerNote(aQuiet: boolean);
var
  CurCaption, ParsedC, Note, s: String;
  Quality: TSDFilenameQuality;
  ImageIndex: Integer;
  CfgCache: TPCTargetConfigCache;
begin
  if csDestroying in ComponentState then exit;
  CurCaption:=CompilerComboBox.Text;
  EnvironmentOptions.CompilerFilename:=CurCaption;
  ParsedC:=EnvironmentOptions.GetParsedCompilerFilename;
  if fLastParsedCompiler=ParsedC then exit;
  fLastParsedCompiler:=ParsedC;
  Quality:=CheckFPCExeQuality(fLastParsedCompiler,Note,
                              CodeToolBoss.CompilerDefinesCache.TestFilename);
  if Quality=sddqCompatible then
  begin
    // check compiler again
    CfgCache:=CodeToolBoss.CompilerDefinesCache.ConfigCaches.Find(
                                               fLastParsedCompiler,'','','',true);
    CfgCache.CompilerDate:=0; // force update
    if CfgCache.NeedsUpdate then
      CfgCache.Update(CodeToolBoss.CompilerDefinesCache.TestFilename);
    BuildBoss.SetBuildTargetIDE(aQuiet);
  end;
  case Quality of
  sddqInvalid: s:=lisError;
  sddqCompatible: s:='';
  else s:=lisWarning;
  end;
  ParsedC:=EnvironmentOptions.GetParsedCompilerFilename;
  if EnvironmentOptions.CompilerFilename<>ParsedC then
    s:=lisFile2+ParsedC+LineEnding+LineEnding+s;
  CompilerMemo.Text:=s+Note;

  ImageIndex:=QualityToImgIndex(Quality);
  TVNodeCompiler.ImageIndex:=ImageIndex;
  TVNodeCompiler.SelectedIndex:=ImageIndex;

  FFlags:=FFlags+[sdfFPCSrcDirNeedsUpdate,sdfFppkgConfigFileNeedsUpdate,
                  sdfMakeExeFilenameNeedsUpdate,sdfDebuggerFilenameNeedsUpdate];
  IdleConnected:=true;
end;

procedure TInitialSetupDialog.UpdateFPCSrcDirNote;
var
  CurCaption: String;
  Note: string;
  Quality: TSDFilenameQuality;
  s: String;
  ImageIndex: Integer;
begin
  if csDestroying in ComponentState then exit;
  CurCaption:=FPCSrcDirComboBox.Text;
  CurCaption:=ChompPathDelim(CurCaption);
  EnvironmentOptions.FPCSourceDirectory:=CurCaption;
  if fLastParsedFPCSrcDir=EnvironmentOptions.GetParsedFPCSourceDirectory then exit;
  fLastParsedFPCSrcDir:=EnvironmentOptions.GetParsedFPCSourceDirectory;
  //debugln(['TInitialSetupDialog.UpdateFPCSrcDirNote ',fLastParsedFPCSrcDir]);
  Quality:=CheckFPCSrcDirQuality(fLastParsedFPCSrcDir,Note,GetFPCVer);
  case Quality of
  sddqInvalid: s:=lisError;
  sddqCompatible: s:='';
  else s:=lisWarning;
  end;
  if EnvironmentOptions.FPCSourceDirectory<>EnvironmentOptions.GetParsedFPCSourceDirectory
  then
    s:=lisDirectory+EnvironmentOptions.GetParsedFPCSourceDirectory+LineEnding+
      LineEnding+s;
  s+=Note;
  if Quality<>sddqCompatible then
    s+=#13+lisYouCanDownloadFPCAndTheFPCSourcesFromHttpSourcefor;
  FPCSrcDirMemo.Text:=s;

  ImageIndex:=QualityToImgIndex(Quality);
  TVNodeFPCSources.ImageIndex:=ImageIndex;
  TVNodeFPCSources.SelectedIndex:=ImageIndex;
end;

procedure TInitialSetupDialog.UpdateMakeExeNote;
var
  CurCaption: String;
  Note: string;
  Quality: TSDFilenameQuality;
  s: String;
  ImageIndex: Integer;
begin
  if csDestroying in ComponentState then exit;
  CurCaption:=MakeExeComboBox.Text;
  EnvironmentOptions.MakeFilename:=CurCaption;
  if fLastParsedMakeExe=EnvironmentOptions.GetParsedMakeFilename then exit;
  fLastParsedMakeExe:=EnvironmentOptions.GetParsedMakeFilename;
  //debugln(['TInitialSetupDialog.UpdateMakeExeNote ',fLastParsedMakeExe]);
  Quality:=CheckMakeExeQuality(fLastParsedMakeExe,Note);

  case Quality of
  sddqInvalid: s:=lisError;
  sddqCompatible: s:='';
  else s:=lisWarning;
  end;
  if EnvironmentOptions.MakeFilename<>EnvironmentOptions.GetParsedMakeFilename
  then
    s:=lisFile2+EnvironmentOptions.GetParsedMakeFilename+LineEnding+
      LineEnding+s;
  MakeExeMemo.Text:=s+Note;

  ImageIndex:=QualityToImgIndex(Quality);
  TVNodeMakeExe.ImageIndex:=ImageIndex;
  TVNodeMakeExe.SelectedIndex:=ImageIndex;

  IdleConnected:=true;
end;

procedure TInitialSetupDialog.UpdateDebuggerNote;
var
  CurCaption: String;
  Note: string;
  Quality: TSDFilenameQuality;
  s, ParsedFName: String;
  ImageIndex: Integer;
begin
  if csDestroying in ComponentState then exit;
  CurCaption:=DebuggerComboBox.Text;
  ParsedFName := EnvironmentOptions.GetParsedValue(eopDebuggerFilename, CurCaption);
  if fLastParsedDebugger=ParsedFName then exit;
  fLastParsedDebugger:=ParsedFName;
  //debugln(['TInitialSetupDialog.UpdateDebuggerNote ',fLastParsedDebugger]);
  Quality:=CheckDebuggerQuality(fLastParsedDebugger,Note, FSkipDebugger);

  case Quality of
  sddqInvalid: s:=lisError;
  sddqCompatible: s:='';
  else s:=lisWarning;
  end;
  if CurCaption<>ParsedFName
  then
    s:=lisFile2+ParsedFName+LineEnding+
      LineEnding+s;
  DebuggerMemo.Text:=s+Note;

  ImageIndex:=QualityToImgIndex(Quality);
  TVNodeDebugger.ImageIndex:=ImageIndex;
  TVNodeDebugger.SelectedIndex:=ImageIndex;

  IdleConnected:=true;
end;

function TInitialSetupDialog.FirstErrorNode: TTreeNode;
var
  i: Integer;
begin
  for i:=0 to PropertiesTreeView.Items.TopLvlCount-1 do
  begin
    Result:=PropertiesTreeView.Items.TopLvlItems[i];
    if Result.ImageIndex=ImgIDError then exit;
  end;
  Result:=FirstWarningNode;
end;

function TInitialSetupDialog.FirstWarningNode: TTreeNode;
var
  i: Integer;
begin
  for i:=0 to PropertiesTreeView.Items.TopLvlCount-1 do
  begin
    Result:=PropertiesTreeView.Items.TopLvlItems[i];
    if Result.ImageIndex=ImgIDWarning then
      exit;
  end;
  Result:=nil;
end;

function TInitialSetupDialog.GetFirstCandidate(Candidates: TSDFileInfoList;
  MinQuality: TSDFilenameQuality): TSDFileInfo;
var
  i: Integer;
begin
  if Candidates<>nil then
    for i:=0 to Candidates.Count-1 do begin
      Result:=TSDFileInfo(Candidates[i]);
      if Result.Quality>=MinQuality then
        exit;
    end;
  Result:=nil;
end;

function TInitialSetupDialog.QualityToImgIndex(Quality: TSDFilenameQuality): integer;
begin
  if Quality=sddqCompatible then
    Result:=-1
  else if Quality=sddqWrongMinorVersion then
    Result:=ImgIDWarning
  else if Quality=sddqIncomplete then
    Result:=ImgIDWarning
  else if Quality=sddqMakeNotWithFpc then
    Result:=ImgIDWarning
  else
    Result:=ImgIDError;
end;

procedure TInitialSetupDialog.ShowHideScanControls(aShow: Boolean);
begin
  // Show ProgressBar and Stop button durin scanning
  ScanLabel.Visible:=aShow;
  ScanProgressBar.Visible:=aShow;
  StopScanButton.Visible:=aShow;
  // At the same time disable other GUI controls so a user can not mess with it
  StartIDEBitBtn.Enabled:=not aShow;
  FPCSrcDirBrowseButton.Enabled:=not aShow;
  FPCSrcDirComboBox.Enabled:=not aShow;
//  FPCSrcDirMemo.Enabled:=not aShow;
end;

procedure TInitialSetupDialog.ThreadTerminated(Sender: TObject);
begin
  debugln(['TInitialSetupDialog.ThreadTerminated ']);
  fSearchFpcSourceThread:=nil; // Thread will free itself. Make the variable nil, too.
  ShowHideScanControls(false);
end;

procedure TInitialSetupDialog.TranslateResourceStrings;
begin
  IDETranslations.TranslateResourceStrings(
                           EnvironmentOptions.GetParsedLazarusDirectory,
                           EnvironmentOptions.LanguageID);
  UpdateCaptions;
end;

procedure TInitialSetupDialog.Init;
var
  Node: TTreeNode;
  Candidate: TSDFileInfo;
  IsFirstStart: Boolean;
  PrimaryFilename: String;
  SecondaryFilename: String;
  PrimaryEnvs: TStringListUTF8;
  SecondaryEnvs: TStringListUTF8;
begin
  IsFirstStart:=not FileExistsCached(EnvironmentOptions.Filename);
  if not IsFirstStart then begin
    IsFirstStart:= False;
    PrimaryFilename:=EnvironmentOptions.Filename;
    SecondaryFilename:=AppendPathDelim(GetSecondaryConfigPath)+ExtractFilename(PrimaryFilename);
    if FileExistsUTF8(PrimaryFilename)
    and FileExistsUTF8(SecondaryFilename) then begin
      // compare content of primary and secondary config
      PrimaryEnvs:=TStringListUTF8.Create;
      SecondaryEnvs:=TStringListUTF8.Create;
      try
        PrimaryEnvs.LoadFromFile(PrimaryFilename);
      except
        on E: Exception do
          debugln(['TInitialSetupDialog.Init unable to read "'+PrimaryFilename+'": '+E.Message]);
      end;
      try
        SecondaryEnvs.LoadFromFile(SecondaryFilename);
      except
        on E: Exception do
          debugln(['TInitialSetupDialog.Init unable to read "'+SecondaryFilename+'": '+E.Message]);
      end;
      // IsFirstStart:=PrimaryEnvs.Text=SecondaryEnvs.Text;
      PrimaryEnvs.Free;
      SecondaryEnvs.Free;
    end;
  end
  else
    IsFirstStart := True;
  //debugln(['TInitialSetupDialog.Init IsFirstStart=',IsFirstStart,' ',EnvironmentOptions.Filename]);

  // Lazarus directory
  UpdateLazarusDirCandidates;
  if IsFirstStart
  or (not FileExistsCached(EnvironmentOptions.GetParsedLazarusDirectory))
  then begin
    // first start => choose first best candidate
    Candidate:=GetFirstCandidate(FCandidates[sddtLazarusSrcDir]);
    if Candidate<>nil then
    begin
      EnvironmentOptions.LazarusDirectory:=Candidate.Caption;
      if Candidate.Quality=sddqCompatible then
        TranslateResourceStrings;
    end;
  end;
  LazDirComboBox.Text:=EnvironmentOptions.LazarusDirectory;
  FLastParsedLazDir:='. .';
  UpdateLazDirNote;

  // compiler filename
  UpdateCompilerFilenameCandidates;
  if IsFirstStart
  or (EnvironmentOptions.CompilerFilename='')
  or (not FileExistsCached(EnvironmentOptions.GetParsedCompilerFilename))
  then begin
    // first start => choose first best candidate
    Candidate:=GetFirstCandidate(FCandidates[sddtCompilerFilename]);
    if Candidate<>nil then
      EnvironmentOptions.CompilerFilename:=Candidate.Caption;
  end;
  CompilerComboBox.Text:=EnvironmentOptions.CompilerFilename;
  fLastParsedCompiler:='. .';
  UpdateCompilerNote;

  // FPC source directory
  UpdateFPCSrcDirCandidates;
  {$IFDEF DebugSearchFPCSrcThread}
  IsFirstStart:=true;
  {$ENDIF}
  if IsFirstStart or (EnvironmentOptions.FPCSourceDirectory='')
  or (not FileExistsCached(EnvironmentOptions.GetParsedFPCSourceDirectory))
  then begin
    // first start => choose first best candidate
    {$IFDEF DebugSearchFPCSrcThread}
    Candidate:=nil;
    {$ELSE}
    Candidate:=GetFirstCandidate(FCandidates[sddtFPCSrcDir]);
    {$ENDIF}
    if Candidate<>nil then begin
      EnvironmentOptions.FPCSourceDirectory:=Candidate.Caption;
    end
    else begin
      // No candidates found => start a thread to scan the file system.
      {$IFNDEF LCLCarbon}
      // carbon interface does not support Synchronize outside Application.Run
      StartFPCSrcThread;
      SelectPage(TVNodeFPCSources.Text);
      {$ENDIF}
    end;
  end;
  ShowHideScanControls(fSearchFpcSourceThread<>nil);
  FPCSrcDirComboBox.Text:=EnvironmentOptions.FPCSourceDirectory;
  fLastParsedFPCSrcDir:='. .';
  UpdateFPCSrcDirNote;

  // Make executable
  UpdateMakeExeCandidates({aStopIfFits} True);
  if IsFirstStart
  or (EnvironmentOptions.MakeFilename='')
  or (not FileExistsCached(EnvironmentOptions.GetParsedMakeFilename)) then
  begin
    // first start => choose first best candidate
    Candidate:=GetFirstCandidate(FCandidates[sddtMakeExeFilename]);
    if Candidate<>nil then
      EnvironmentOptions.MakeFilename:=Candidate.Caption
    else begin // second chance => better an incomplete instead of none (especially for windows)
      Candidate:=GetFirstCandidate(FCandidates[sddtMakeExeFilename], sddqIncomplete);
      if Candidate<>nil then
        EnvironmentOptions.MakeFilename:=Candidate.Caption;
    end;
  end;
  MakeExeComboBox.Text:=EnvironmentOptions.MakeFilename;
  fLastParsedMakeExe:='. .';
  UpdateMakeExeNote;

  RegisterDebugger(TGDBMIDebugger); // make sure we can read the config
  FSkipDebugger := EnvironmentOptions.HasActiveDebuggerEntry // There is a configured entry. Assume it is right, unless we can prove it is incorrect
    and not (
      (EnvironmentOptions.CurrentDebuggerPropertiesConfig <> nil) and    // The ACTIVE dbg is a known debugger
      (EnvironmentOptions.CurrentDebuggerClass <> nil)     and           // with existing debugger class
      (EnvironmentOptions.CurrentDebuggerPropertiesConfig.NeedsExePath)  // Does need an exe
    );
  // Debugger
  FInitialDebuggerFileName := EnvironmentOptions.DebuggerFilename;
  UpdateDebuggerCandidates;
  if (not FSkipDebugger) then begin
    if EnvironmentOptions.CurrentDebuggerPropertiesConfig = nil then
      EnvironmentOptions.CurrentDebuggerPropertiesConfig :=
        EnvironmentOptions.DebuggerPropertiesConfigList.EntryByName('', TGDBMIDebugger.ClassName);
    if EnvironmentOptions.CurrentDebuggerPropertiesConfig = nil then
      EnvironmentOptions.CurrentDebuggerPropertiesConfig :=
        TDebuggerPropertiesConfig.CreateForDebuggerClass(TGDBMIDebugger);
    if IsFirstStart or (not FileExistsCached(EnvironmentOptions.GetParsedDebuggerFilename))
    then begin
      // first start => choose first best candidate
      Candidate:=GetFirstCandidate(FCandidates[sddtDebuggerFilename]);
      if Candidate<>nil then begin
        EnvironmentOptions.CurrentDebuggerPropertiesConfig.DebuggerFilename:=Candidate.Caption;
      end;
    end;
    EnvironmentOptions.SaveDebuggerPropertiesList; // Update XML
  end;

  DebuggerComboBox.Text:=EnvironmentOptions.DebuggerFilename;
  fLastParsedDebugger:='. .';
  UpdateDebuggerNote;

  // Fppkg
  FppkgComboBox.Text := EnvironmentOptions.FppkgConfigFile;
  fLastParsedFppkgConfigFile := ' ';
  UpdateFppkgCandidates;
  UpdateFppkgNote;

  // select first error
  Node:=FirstErrorNode;
  if Node=nil then
    Node:=TVNodeLazarus;
  PropertiesTreeView.Selected:=Node;
end;

procedure TInitialSetupDialog.UpdateFppkgNote;
var
  CurCaption: String;
  FppkgMsg, Note: string;
  Quality: TSDFilenameQuality;
  ConfigFile: string;
  {$IF FPC_FULLVERSION>30100}
  ImageIndex: Integer;
  {$ENDIF}
begin
  if csDestroying in ComponentState then exit;
  CurCaption:=FppkgComboBox.Text;
  EnvironmentOptions.FppkgConfigFile:=CurCaption;
  if fLastParsedFppkgConfigFile=EnvironmentOptions.GetParsedFppkgConfig then exit;
  fLastParsedFppkgConfigFile:=EnvironmentOptions.GetParsedFppkgConfig;

  ConfigFile := fLastParsedFppkgConfigFile;
  Quality := CheckFppkgConfigFile(ConfigFile, Note);
  if Quality<>sddqCompatible then
  begin
    Note := lisError + Note + LineEnding;
    FppkgWriteConfigButton.Enabled := (Quality=sddqIncomplete);
  end
  else
  begin
    Quality := CheckFppkgConfiguration(ConfigFile, FppkgMsg);

    if Quality=sddqCompatible then
    begin
      Note := lisOk;
      FppkgWriteConfigButton.Enabled := False;
    end
    else
    begin
      Note := lisError + Format(lisIncorrectFppkgConfiguration, [FppkgMsg]) + LineEnding;
      Note := Note + LineEnding + lisFppkgFixConfiguration;
      FppkgWriteConfigButton.Enabled := True;
    end;
  end;

  FppkgMemo.Text := lisFile2 + ConfigFile + LineEnding + LineEnding + Note;

  {$IF FPC_FULLVERSION>30100}
  ImageIndex:=QualityToImgIndex(Quality);
  TVNodeFppkg.ImageIndex:=ImageIndex;
  TVNodeFppkg.SelectedIndex:=ImageIndex;
  {$ENDIF FPC_FULLVERSION>30100}

  IdleConnected:=true;
end;

procedure TInitialSetupDialog.FppkgComboBoxChange(Sender: TObject);
begin
  UpdateFppkgNote;
end;

procedure TInitialSetupDialog.UpdateFppkgCandidates;

  function SearchFppkgFpcPrefixCandidates: TSDFileInfoList;

    function CheckFile(AFile: string; var List: TSDFileInfoList): boolean;
    var
      Item: TSDFileInfo;
    begin
      Result:=false;
      if AFile='' then exit;
      ForcePathDelims(AFile);
      // check if already checked
      if Assigned(List) and List.CaptionExists(AFile) then exit;

      if FileExistsUTF8(AFile) then
      begin
        if List=nil then
          List:=TSDFileInfoList.create(true);
        Item:=List.AddNewItem(AFile, AFile);
        Item.Quality:=sddqCompatible;
        Result := True;
      end;
    end;
  begin
    Result:=nil;

    {$IF FPC_FULLVERSION>30100}
    CheckFile(GetFppkgConfigFile(False, False), Result);
    CheckFile(GetFppkgConfigFile(False, True), Result);
    CheckFile(GetFppkgConfigFile(True, False), Result);
    CheckFile(GetFppkgConfigFile(True, True), Result);
    {$ENDIF}
  end;

var
  Files: TSDFileInfoList;
begin
  Exclude(FFlags,sdfFppkgConfigFileNeedsUpdate);
  Files:=SearchFppkgFpcPrefixCandidates;
  FreeAndNil(FCandidates[sddtFppkgFpcPrefix]);
  FCandidates[sddtFppkgFpcPrefix]:=Files;
  FillComboboxWithFileInfoList(FppkgComboBox,Files,-1);
end;


procedure TInitialSetupDialog.FppkgBrowseButtonClick(Sender: TObject);
var
  lExpandedName: string; // Expanded name before Dialog
  lDirName, lFileName: string;
  lTitle: string;
  lChanged: boolean=False;
  Dlg: TIDEOpenDialog;
  Filter: String;
begin
  Dlg:=IDEOpenDialogClass.Create(nil);
  try
    lTitle:='fppkg.cfg';
    Dlg.Title:=SimpleFormat(lisSelectPathTo, [lTitle]);
    lExpandedName:=EnvironmentOptions.GetParsedValue(eopFppkgConfigFile, FppkgComboBox.Text);
    lDirName := GetValidDirectory(lExpandedName, {out} lFileName);
    Dlg.Options:=Dlg.Options+[ofFileMustExist];
    if lFileName='' then
      lFileName:=lTitle;
    Filter:=dlgFilterAll+'|'+GetAllFilesMask;
    if ExtractFileExt(lFileName)<>'' then
      Filter:=dlgFilterExecutable+'|*'+ExtractFileExt(lFileName)+'|'+Filter;
    Dlg.Filter:=Filter;
    Dlg.InitialDir:=lDirName;
    Dlg.FileName:=lFileName;
    if not Dlg.Execute then
      exit;
    lFileName:=CleanAndExpandFilename(Dlg.Filename);
    lChanged := UpperCase(lExpandedName)<>UpperCase(lFileName);
  finally
    Dlg.Free;
  end;
  if lChanged then begin // Avoid loosing $(macros)
    FppkgComboBox.Text:=lFileName;
    UpdateFppkgNote;
  end;
end;

procedure TInitialSetupDialog.FppkgWriteConfigButtonClick(Sender: TObject);
{$IF FPC_FULLVERSION>30100}
var
  Dialog: TGenerateFppkgConfigurationDialog;
{$ENDIF}
begin
  {$IF FPC_FULLVERSION>30100}
  Dialog := TGenerateFppkgConfigurationDialog.Create(Self);
  try
    Dialog.Compiler := fLastParsedCompiler;
    if fLastParsedFppkgConfigFile='' then
      Dialog.FppkgCfgFilename := GetFppkgConfigFile(False, False)
    else
      Dialog.FppkgCfgFilename := fLastParsedFppkgConfigFile;
    Dialog.ShowModal;
  finally
    Dialog.Free;
  end;

  fLastParsedFppkgConfigFile := ' ';
  UpdateFppkgNote;
  {$ENDIF}
end;

end.

