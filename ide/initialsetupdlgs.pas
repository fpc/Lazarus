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
  // RTL + FCL + LCL
  Classes, SysUtils,
  Forms, Controls, Buttons, Dialogs, Graphics, ComCtrls, ExtCtrls, StdCtrls, LCLProc,
  pkgglobals, UTF8Process,
  // CodeTools
  FileProcs, CodeToolManager, DefineTemplates,
  // LazUtils
  FileUtil, LazUTF8, LazUTF8Classes, LazFileUtils, LazFileCache, LazLoggerBase,
  // Other
  MacroDefIntf, GDBMIDebugger, DbgIntfDebuggerBase, IDEDialogs,
  TransferMacros, LazarusIDEStrConsts, LazConf, EnvironmentOpts, IDEImagesIntf,
  AboutFrm, IDETranslations, BaseBuildManager, InitialSetupProc, FppkgHelper,
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
    FFlags: TSDFlags;
    FLastParsedLazDir: string;
    fLastParsedCompiler: string;
    fLastParsedFPCSrcDir: string;
    fLastParsedMakeExe: string;
    fLastParsedDebugger: string;
    fLastParsedFppkgPrefix: string;
    fLastParsedFppkgLibPath: string;
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
    procedure StartFPCSrcThread;
    procedure UpdateLazarusDirCandidates;
    procedure UpdateCompilerFilenameCandidates;
    procedure UpdateFPCSrcDirCandidates;
    procedure UpdateFPCSrcDirCandidate(aFPCSrcDirInfo: TSDFileInfo);
    procedure UpdateMakeExeCandidates;
    procedure UpdateDebuggerCandidates;
    procedure UpdateFppkgCandidates;
    procedure FillComboboxWithFileInfoList(ABox: TComboBox; List: TSDFileInfoList;
       ItemIndex: integer = 0);
    procedure SetIdleConnected(const AValue: boolean);
    procedure UpdateLazDirNote;
    procedure UpdateCompilerNote;
    procedure UpdateFPCSrcDirNote;
    procedure UpdateMakeExeNote;
    procedure UpdateDebuggerNote;
    procedure UpdateFppkgNote;
    function FirstErrorNode: TTreeNode;
    function GetFPCVer: string;
    function GetFirstCandidate(Candidates: TSDFileInfoList;
      MinQuality: TSDFilenameQuality = sddqCompatible): TSDFileInfo;
    function QualityToImgIndex(Quality: TSDFilenameQuality): integer;
    procedure ShowHideScanControls(aShow: Boolean);
    procedure ThreadTerminated(Sender: TObject); // called in main thread by fSearchFpcSourceThread.OnTerminate
    procedure TranslateResourceStrings;
    function CheckFppkgQuality(APrefix: string; out LibPath, Note: string): TSDFilenameQuality;
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
function CheckDebuggerQuality(AFilename: string; out Note: string): TSDFilenameQuality;
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

function CheckDebuggerQuality(AFilename: string; out Note: string): TSDFilenameQuality;
begin
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
    EnvironmentOptions.DebuggerFilename:=AFilename;
    RealFilename:=EnvironmentOptions.GetParsedDebuggerFilename;
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
  OldDebuggerFilename: String;
  s, AFilename: String;
  Files: TStringList;
  i: Integer;
begin
  Result:=nil;

  OldDebuggerFilename:=EnvironmentOptions.DebuggerFilename;
  try
    // check current setting
    if CheckFile(EnvironmentOptions.DebuggerFilename,Result) then exit;

    // check the primary options
    AFilename:=GetValueFromPrimaryConfig(EnvOptsConfFileName,
                                    'EnvironmentOptions/DebuggerFilename/Value');
    if CheckFile(AFilename,Result) then exit;

    // check the secondary options
    AFilename:=GetValueFromSecondaryConfig(EnvOptsConfFileName,
                                    'EnvironmentOptions/DebuggerFilename/Value');
    if CheckFile(AFilename,Result) then exit;

    // Check locations proposed by debugger class
    s := DefaultDebuggerClass.ExePaths;
    while s <> '' do begin
      AFilename := GetPart([], [';'], s);
      if CheckFile(AFilename, Result) then exit;
      if s <> '' then delete(s, 1, 1);
    end;


    // Windows-only locations:
    if (GetDefaultSrcOSForTargetOS(GetCompiledTargetOS)='win') then begin
      // check for debugger in fpc.exe directory - could be a lucky shot
      if CheckFile(GetForcedPathDelims('$Path($(CompPath))/'+DebuggerFileName+GetExecutableExt),Result)
        then exit;
    end;

    // check history
    Files:=EnvironmentOptions.DebuggerFileHistory;
    if Files<>nil then
      for i:=0 to Files.Count-1 do
        if CheckFile(Files[i],Result) then exit;

    // check PATH
    AFilename:=DebuggerFileName+GetExecutableExt;
    if CheckFile(AFilename,Result) then exit;

    // There are no common directories apart from the PATH
    // where gdb would be installed. Otherwise we could do something similar as
    // in SearchMakeExeCandidates.
  finally
    EnvironmentOptions.DebuggerFilename:=OldDebuggerFilename;
  end;
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
  UpdateCompilerNote;
  UpdateFPCSrcDirNote;
end;

procedure TInitialSetupDialog.DebuggerBrowseButtonClick(Sender: TObject);
var
  Filename: String;
  Dlg: TOpenDialog;
  Filter: String;
begin
  Dlg:=TOpenDialog.Create(nil);
  try
    Filename:='gdb'+GetExecutableExt;
    Dlg.Title:=SimpleFormat(lisSelectPathTo, [Filename]);
    Dlg.Options:=Dlg.Options+[ofFileMustExist];
    Filter:=dlgFilterAll+'|'+GetAllFilesMask;
    if ExtractFileExt(Filename)<>'' then
      Filter:=dlgFilterExecutable+'|*'+ExtractFileExt(Filename)+'|'+Filter;
    Dlg.Filter:=Filter;
    if not Dlg.Execute then exit;
    Filename:=Dlg.FileName;
  finally
    Dlg.Free;
  end;
  DebuggerComboBox.Text:=Filename;
  UpdateDebuggerNote;
end;

procedure TInitialSetupDialog.DebuggerComboBoxChange(Sender: TObject);
begin
  UpdateDebuggerNote;
end;

procedure TInitialSetupDialog.CompilerBrowseButtonClick(Sender: TObject);
var
  Filename: String;
  Dlg: TOpenDialog;
  Filter: String;
begin
  Dlg:=TOpenDialog.Create(nil);
  try
    Filename:='fpc'+GetExecutableExt;
    Dlg.Title:=SimpleFormat(lisSelectPathTo, [Filename]);
    Dlg.Options:=Dlg.Options+[ofFileMustExist];
    Filter:=dlgFilterAll+'|'+GetAllFilesMask;
    if ExtractFileExt(Filename)<>'' then
      Filter:=dlgFilterExecutable+'|*'+ExtractFileExt(Filename)+'|'+Filter;
    Dlg.Filter:=Filter;
    if not Dlg.Execute then exit;
    Filename:=Dlg.FileName;
  finally
    Dlg.Free;
  end;
  CompilerComboBox.Text:=Filename;
  UpdateCompilerNote;
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
  Dir:=SelectDirectory(lisSelectFPCSourceDirectory);
  if Dir='' then exit;
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
  Dir:=SelectDirectory(lisSelectLazarusSourceDirectory);
  if Dir='' then exit;
  LazDirComboBox.Text:=Dir;
  UpdateLazDirNote;
end;

procedure TInitialSetupDialog.LazDirComboBoxChange(Sender: TObject);
begin
  UpdateLazDirNote;
end;

procedure TInitialSetupDialog.MakeExeBrowseButtonClick(Sender: TObject);
var
  Filename: String;
  Dlg: TOpenDialog;
  Filter: String;
begin
  Dlg:=TOpenDialog.Create(nil);
  try
    Filename:='make'+GetExecutableExt;
    Dlg.Title:=SimpleFormat(lisSelectPathTo, [Filename]);
    Dlg.Options:=Dlg.Options+[ofFileMustExist];
    Filter:=dlgFilterAll+'|'+GetAllFilesMask;
    if ExtractFileExt(Filename)<>'' then
      Filter:=dlgFilterExecutable+'|*'+ExtractFileExt(Filename)+'|'+Filter;
    Dlg.Filter:=Filter;
    if not Dlg.Execute then exit;
    Filename:=Dlg.FileName;
  finally
    Dlg.Free;
  end;
  MakeExeComboBox.Text:=Filename;
  UpdateMakeExeNote;
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
  s:=DebuggerComboBox.Text;
  if s<>'' then begin
    EnvironmentOptions.DebuggerFilename:=s;
    if s <> FInitialDebuggerFileName then
      EnvironmentOptions.DebuggerConfig.DebuggerClass := 'TGDBMIDebugger';
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
  end else if sdfFppkgFpcPrefixNeedsUpdate in FFlags then begin
    fLastParsedFppkgPrefix := '';
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

  FppkgLabel.Caption:=lisFppkgInstallationPath;
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

procedure TInitialSetupDialog.UpdateMakeExeCandidates;
var
  Files: TSDFileInfoList;
begin
  Exclude(FFlags,sdfMakeExeFilenameNeedsUpdate);
  Files:=SearchMakeExeCandidates(false);
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

procedure TInitialSetupDialog.UpdateCompilerNote;
var
  CurCaption: String;
  Note: string;
  Quality: TSDFilenameQuality;
  s: String;
  ImageIndex: Integer;
  CfgCache: TPCTargetConfigCache;
begin
  if csDestroying in ComponentState then exit;
  CurCaption:=CompilerComboBox.Text;
  EnvironmentOptions.CompilerFilename:=CurCaption;
  if fLastParsedCompiler=EnvironmentOptions.GetParsedCompilerFilename then exit;
  fLastParsedCompiler:=EnvironmentOptions.GetParsedCompilerFilename;
  //debugln(['TInitialSetupDialog.UpdateCompilerNote ',fLastParsedCompiler]);

  Quality:=CheckFPCExeQuality(fLastParsedCompiler,Note,
                                CodeToolBoss.CompilerDefinesCache.TestFilename);
  if Quality<>sddqInvalid then begin
    CodeToolBoss.CompilerDefinesCache.ConfigCaches.Find(
      fLastParsedCompiler,'','','',true);
    // check compiler again
    CfgCache:=CodeToolBoss.CompilerDefinesCache.ConfigCaches.Find(
                                               fLastParsedCompiler,'','','',true);
    CfgCache.Update(CodeToolBoss.CompilerDefinesCache.TestFilename);
    BuildBoss.SetBuildTargetIDE;
  end;

  case Quality of
  sddqInvalid: s:=lisError;
  sddqCompatible: s:='';
  else s:=lisWarning;
  end;
  if EnvironmentOptions.CompilerFilename<>EnvironmentOptions.GetParsedCompilerFilename
  then
    s:=lisFile2+EnvironmentOptions.GetParsedCompilerFilename+LineEnding+
      LineEnding+s;
  CompilerMemo.Text:=s+Note;

  ImageIndex:=QualityToImgIndex(Quality);
  TVNodeCompiler.ImageIndex:=ImageIndex;
  TVNodeCompiler.SelectedIndex:=ImageIndex;

  FFlags:=FFlags+[sdfFPCSrcDirNeedsUpdate,sdfFppkgFpcPrefixNeedsUpdate,
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
  s: String;
  ImageIndex: Integer;
begin
  if csDestroying in ComponentState then exit;
  CurCaption:=DebuggerComboBox.Text;
  EnvironmentOptions.DebuggerFilename:=CurCaption;
  if fLastParsedDebugger=EnvironmentOptions.GetParsedDebuggerFilename then exit;
  fLastParsedDebugger:=EnvironmentOptions.GetParsedDebuggerFilename;
  //debugln(['TInitialSetupDialog.UpdateDebuggerNote ',fLastParsedDebugger]);
  Quality:=CheckDebuggerQuality(fLastParsedDebugger,Note);

  case Quality of
  sddqInvalid: s:=lisError;
  sddqCompatible: s:='';
  else s:=lisWarning;
  end;
  if EnvironmentOptions.DebuggerFilename<>EnvironmentOptions.GetParsedDebuggerFilename
  then
    s:=lisFile2+EnvironmentOptions.GetParsedDebuggerFilename+LineEnding+
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
  Result:=nil;
end;

function TInitialSetupDialog.GetFPCVer: string;
begin
  Result:='$(FPCVer)';
  GlobalMacroList.SubstituteStr(Result);
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
      IsFirstStart:=PrimaryEnvs.Text=SecondaryEnvs.Text;
      PrimaryEnvs.Free;
      SecondaryEnvs.Free;
    end;
  end;
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
  UpdateMakeExeCandidates;
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

  // Debugger
  FInitialDebuggerFileName := EnvironmentOptions.DebuggerFilename;
  UpdateDebuggerCandidates;
  if IsFirstStart or (not FileExistsCached(EnvironmentOptions.GetParsedDebuggerFilename))
  then begin
    // first start => choose first best candidate
    Candidate:=GetFirstCandidate(FCandidates[sddtDebuggerFilename]);
    if Candidate<>nil then
      EnvironmentOptions.DebuggerFilename:=Candidate.Caption;
  end;
  DebuggerComboBox.Text:=EnvironmentOptions.DebuggerFilename;
  fLastParsedDebugger:='. .';
  UpdateDebuggerNote;

  // Fppkg
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
  Msg, Note: string;
  Quality: TSDFilenameQuality;
  ImageIndex: Integer;
begin
  if csDestroying in ComponentState then exit;
  CurCaption:=FppkgComboBox.Text;
  if fLastParsedFppkgPrefix=CurCaption then exit;
  fLastParsedFppkgPrefix:=CurCaption;

  Quality := CheckFppkgConfiguration();

  Msg := '';
  if CheckFppkgQuality(CurCaption,fLastParsedFppkgLibPath,Note)<>sddqCompatible then
    Msg := Note;
  if (CheckFPCExeQuality(fLastParsedCompiler,Note, CodeToolBoss.CompilerDefinesCache.TestFilename)<>sddqCompatible) then
    Msg := Msg + lisWarning + lisFppkgCompilerProblem +Note;

  if Quality=sddqCompatible then
    Note := lisOk
  else
    Note := lisError + lisIncorrectFppkgConfiguration + LineEnding;

  if Msg<>'' then
  begin
      Note := Note + LineEnding + Msg;
    FppkgWriteConfigButton.Enabled := False;
  end
  else
  begin
    FppkgWriteConfigButton.Enabled := True;
  end;

  FppkgMemo.Text := Note;

  ImageIndex:=QualityToImgIndex(Quality);

  {$IF FPC_FULLVERSION>30100}
  TVNodeFppkg.ImageIndex:=ImageIndex;
  TVNodeFppkg.SelectedIndex:=ImageIndex;
  {$ENDIF FPC_FULLVERSION>30100}

  IdleConnected:=true;
end;

function TInitialSetupDialog.CheckFppkgQuality(APrefix: string; out LibPath, Note: string): TSDFilenameQuality;
var
  SR: TRawByteSearchRec;
  LibPathValid: Boolean;
begin
  Result := sddqInvalid;
  LibPath := '';
  APrefix:=TrimFilename(APrefix);
  if not FileExistsCached(APrefix) then
  begin
    Note:= lisWarning + lisFreePascalPrefix + ' ' + lisISDDirectoryNotFound + LineEnding;
  end
  else if not DirPathExistsCached(APrefix) then
  begin
    Note:= lisWarning + lisFreePascalPrefix + ' ' + lisPathIsNoDirectory + LineEnding;
  end
  else
  begin
    LibPathValid := True;
    LibPath := IncludeTrailingPathDelimiter(ConcatPaths([APrefix, 'lib', 'fpc']));
    if not DirPathExistsCached(LibPath) then
    begin
      LibPath := IncludeTrailingPathDelimiter(ConcatPaths([APrefix, 'lib64', 'fpc']));
      if not DirPathExistsCached(LibPath) then
      begin
        LibPathValid := False;
      end;
    end;

    if LibPathValid and (FindFirstUTF8(LibPath+AllFilesMask, faDirectory, SR) = 0) then
    begin
      LibPathValid := False;
      repeat
        if (SR.Name<>'.') and (SR.Name<>'..') then
        begin
          if DirPathExistsCached(LibPath+SR.Name+PathDelim+'fpmkinst') and
            DirPathExistsCached(LibPath+SR.Name+PathDelim+'units') then
              begin
                LibPathValid := True;
                Result := sddqCompatible;
                Break;
              end;
        end;
      until FindNext(SR) <> 0;
      FindCloseUTF8(SR);
    end;

    if not LibPathValid then
      Note:= Note + lisWarning + lisNotAValidFppkgPrefix + LineEnding
    else
      Note:='';
  end;
end;

procedure TInitialSetupDialog.FppkgComboBoxChange(Sender: TObject);
begin
  UpdateFppkgNote;
end;

procedure TInitialSetupDialog.UpdateFppkgCandidates;


  function SearchFppkgFpcPrefixCandidates: TSDFileInfoList;

    function CheckPath(APath: string; var List: TSDFileInfoList): boolean;
    var
      Item: TSDFileInfo;
      LibPath, Note: String;
    begin
      Result:=false;
      if APath='' then exit;
      ForcePathDelims(APath);
      // check if already checked
      if Assigned(List) and List.CaptionExists(APath) then exit;

      if CheckFppkgQuality(APath, LibPath, Note) = sddqCompatible then
      begin
        if List=nil then
          List:=TSDFileInfoList.create(true);
        Item:=List.AddNewItem(APath, APath);
        Item.Quality:=sddqCompatible;
        Result := True;
      end;
    end;

  begin
    Result:=nil;

    CheckPath(ExtractFileDir(ExtractFileDir(EnvironmentOptions.GetParsedCompilerFilename)), Result);

    {$ifdef windows}
    CheckPath('c:\pp', Result);
    CheckPath('d:\pp', Result);
    {$else}
    CheckPath('/usr', Result);
    CheckPath('/usr/local', Result);
    {$endif unix}
  end;

var
  Files: TSDFileInfoList;
begin
  Exclude(FFlags,sdfFppkgFpcPrefixNeedsUpdate);
  Files:=SearchFppkgFpcPrefixCandidates;
  FreeAndNil(FCandidates[sddtFppkgFpcPrefix]);
  FCandidates[sddtFppkgFpcPrefix]:=Files;
  FillComboboxWithFileInfoList(FppkgComboBox,Files);
end;

procedure TInitialSetupDialog.FppkgBrowseButtonClick(Sender: TObject);
var
  Dlg: TSelectDirectoryDialog;
begin
  Dlg:=TSelectDirectoryDialog.Create(nil);
  try
    Dlg.Title:=lisSelectFPCPath;
    Dlg.Options:=Dlg.Options+[ofPathMustExist];
    if not Dlg.Execute then exit;
    FppkgComboBox.Text:=Dlg.FileName;
  finally
    Dlg.Free;
  end;
  UpdateFppkgNote;
end;

procedure TInitialSetupDialog.FppkgWriteConfigButtonClick(Sender: TObject);
{$IF FPC_FULLVERSION>30100}
var
  FpcmkcfgExecutable, CompConfigFilename: string;
  Proc: TProcessUTF8;
  Fppkg: TFppkgHelper;
{$ENDIF}
begin
  {$IF FPC_FULLVERSION>30100}
  try
    FpcmkcfgExecutable := FindFPCTool('fpcmkcfg'+GetExecutableExt, EnvironmentOptions.GetParsedCompilerFilename);
    if FpcmkcfgExecutable<>'' then
    begin
      Proc := TProcessUTF8.Create(nil);
      try
        // Write fppkg.cfg
        Proc.Executable := FpcmkcfgExecutable;
        proc.Parameters.Add('-3');
        proc.Parameters.Add('-o');
        proc.Parameters.Add(GetFppkgConfigFile(False, False));
        proc.Parameters.Add('-d');
        proc.Parameters.Add('globalpath='+fLastParsedFppkgLibPath);
        proc.Parameters.Add('-d');
        proc.Parameters.Add('globalprefix='+fLastParsedFppkgPrefix);
        proc.Execute;

        Fppkg:=TFppkgHelper.Instance;
        Fppkg.ReInitialize;

        // Write default compiler configuration file
        CompConfigFilename := Fppkg.GetCompilerConfigurationFileName;
        if CompConfigFilename <> '' then
          begin
          proc.Parameters.Clear;
          proc.Parameters.Add('-4');
          proc.Parameters.Add('-o');
          proc.Parameters.Add(CompConfigFilename);
          proc.Parameters.Add('-d');
          proc.Parameters.Add('fpcbin='+EnvironmentOptions.GetParsedCompilerFilename);
          proc.Execute;
          end;

        Fppkg.ReInitialize;
      finally
        Proc.Free;
      end;
    end;
  except
    on E: Exception do
      IDEMessageDialog(lisFppkgProblem, Format(lisFppkgWriteConfException, [E.Message]), mtWarning, [mbOK]);
  end;

  fLastParsedFppkgPrefix := '';
  UpdateFppkgNote;
  {$ENDIF}

  if CheckFppkgConfiguration<>sddqCompatible then
    IDEMessageDialog(lisFppkgProblem, lisFppkgWriteConfFailed, mtWarning, [mbOK]);
end;

end.

