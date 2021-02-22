{ Copyright (C) 2005-2014  Andrew Haines, Lazarus contributors

  Main form for lhelp. Includes processing/data communication.

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}
{
Icons from Tango theme:
http://tango.freedesktop.org/Tango_Icon_Library
}

unit LHelpCore;

{$IFDEF LNET_VISUAL}
{$DEFINE USE_LNET} // you must manually add the lnetvisual.lpk package to the dependancy list
{$ELSE}
{$NOTE You can add http capability to lhelp by adding the lnetvisual package v0.6.3 or greater requirement to lhelp.}
{$ENDIF}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SimpleIPC, Laz2_XMLCfg,
  // LCL
  Forms, Controls, Graphics, Dialogs, Buttons, ComCtrls, ExtCtrls, Menus, StdCtrls, Types,
  LCLProc, LCLType, LCLIntf, DefaultTranslator,
  // LazUtils
  LazFileUtils, LazUTF8, LazLoggerBase,
  // ChmHelp
  {$IFDEF USE_LNET}HTTPContentProvider,{$ENDIF}
  BaseContentProvider, FileContentProvider, ChmContentProvider, LHelpStrConsts;

type

  { TContentTab }

  TContentTab = class(TTabSheet)
  private
    fContentProvider: TBaseContentProvider;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ContentProvider: TBaseContentProvider read fContentProvider write fContentProvider;
  end;

  { THelpForm }
  
  THelpForm = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    FileMenuCloseItem: TMenuItem;
    MiQuit: TMenuItem;
    FileMenuItem: TMenuItem;
    FileMenuOpenItem: TMenuItem;
    FileSeperater: TMenuItem;
    ImageList1: TImageList;
    ImageListToolbar: TImageList;
    MainMenu1: TMainMenu;
    FileMenuOpenURLItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    AboutItem: TMenuItem;
    FileMenuOpenRecentItem: TMenuItem;
    MiHide: TMenuItem;
    MiActionsGoBack: TMenuItem;
    MiActionsGoForward: TMenuItem;
    MiActionsGoHome: TMenuItem;
    MiActions: TMenuItem;
    MiActionsSearch: TMenuItem;
    MiActionsIndex: TMenuItem;
    MiActionsTOC: TMenuItem;
    ViewShowStatus: TMenuItem;
    ViewShowSepTabs: TMenuItem;
    PageControl: TPageControl;
    OpenDialog1: TOpenDialog;
    ToolBar1: TToolBar;
    HomeBttn: TToolButton;
    BackBttn: TToolButton;
    ForwardBttn: TToolButton;
    FileButton: TToolButton;
    ToolButton1: TToolButton;
    ViewMenuContents: TMenuItem;
    ViewMenuItem: TMenuItem;
    procedure AboutItemClick(Sender: TObject);
    procedure FileMenuCloseItemClick(Sender: TObject);
    procedure FileMenuOpenItemClick(Sender: TObject);
    procedure FileMenuOpenURLItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown ( Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState) ;
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure MiActionsGoForwardClick ( Sender: TObject ) ;
    procedure MiHideClick ( Sender: TObject ) ;
    procedure MiActionsGoBackClick ( Sender: TObject ) ;
    procedure MiActionsGoHomeClick ( Sender: TObject ) ;
    procedure MiActionsIndexClick ( Sender: TObject ) ;
    procedure MiQuitClick ( Sender: TObject ) ;
    procedure MiActionsSearchClick ( Sender: TObject ) ;
    procedure MiActionsTOCClick ( Sender: TObject ) ;
    procedure PageControlChange(Sender: TObject);
    procedure PageControlEnter(Sender: TObject);
    procedure ViewMenuContentsClick(Sender: TObject);
    procedure ViewMenuItemClick(Sender: TObject);
    procedure ViewShowSepTabsClick(Sender: TObject);
    procedure ViewShowStatusClick(Sender: TObject);
  private
    // SimpleIPC server name (including unique part as per help protocol)
    fServerName: String;
    // Receives commands from IDE
    fInputIPC: TSimpleIPCServer;
    // Sends responses back to IDE
    // only used if lhelp started with --ipcname to indicate
    // IPC communication method should be used
    fOutputIPC: TSimpleIPCClient;
    fInputIPCTimer: TTimer;
    fContext: LongInt; // used once when we are started on the command line with --context
    fConfig: TXMLConfig;
    fShowSepTabs: Boolean;
    fShowStatus: Boolean;
    fHasShowed: Boolean;
    fMustClose: boolean;
    fDefWinMax: Boolean;
    fDefWinSize: TRect;
    fHide: boolean; //If yes, start with content hidden. Otherwise start normally
    fUpdateCount: Integer;
    fLastHiddenRequest: String;
    // Keep track of whether size/position preferences were loaded and applied to form
    fLayoutApplied: boolean;
    // Applies layout (size/position/fullscreen) preferences once in lhelp lifetime
    // Needs LoadPreference to be run first to get fConfig object.
    procedure ApplyLayoutPreferencesOnce;
    // Load preferences. Preferences are unique for server-lhelp pairs and plain lhelp
    procedure LoadPreferences(AIPCName: String);
    // Saves preferences. Uses existing config loaded by LoadPreferences
    procedure SavePreferences;
    // Add filename to recent files (MRU) list
    procedure AddRecentFile(AFileName: String);
    procedure DeleteRecentFile(AFileName: String);
    procedure ContentTitleChange({%H-}sender: TObject);
    procedure OpenRecentItemClick(Sender: TObject);
    // Send response back to server (IDE)
    // Used to acknowledge commands from the server
    procedure SendResponse(Response: DWord);
    // Wait for message from server (IDE) and process
    procedure ServerMessage(Sender: TObject);
    // Parse any given command line options
    procedure ReadCommandLineOptions;
    // Start simple IPC server/client
    // ServerName can be the variable passed to --ipcname
    // It is used both for starting up the local server and to ID the remote server
    procedure StartComms(ServerName: String);
    // Stop simple IPC server/client
    procedure StopComms;
    // Open specified URL in viewer window
    function OpenURL(const AURL: String; AContext: THelpContext=-1): DWord;
    // Open specified URL - presumably used to queue URLs for delayed opening
    procedure LateOpenURL(Url: PStringItem);
    function ActivePage: TContentTab;
    // Update UI visibility
    procedure RefreshState;
    procedure ShowError(AError: String);
    // BeginUpdate tells each content provider to possibly stop some events
    procedure BeginUpdate;
    // EndUpdate tells each content provider to resume normal behavior
    procedure EndUpdate;
    // Bring App on Top and show
    procedure ShowApp();
    // Event process
    //procedure DoShowContent(Sender:Tobject);
  public

  end;

var
  HelpForm: THelpForm;
  // Sends messages to the IDE
  IPCClient: TSimpleIPCClient;
  // Receives messages from the IDE
  IPCServer: TSimpleIPCServer;

const
  INVALID_FILE_TYPE = 1;
  VERSION_STAMP = '2021-02-12'; //used in displaying version in about form etc

implementation

{$R *.lfm}

uses 
  LHelpControl;

const
  DigitsInPID=5; // Number of digits in the formatted PID according to the Help Protocol

type
  TRecentMenuItem = class(TMenuItem)
  public
    URL: String;
  end;

{ THelpForm }

procedure THelpForm.AboutItemClick(Sender: TObject);
var
  f: TForm;
  l1, l2: TLabel;
  b: TButton;
  i: TImage;
  bv: TBevel;
  d6: Integer;
begin
  d6 := Scale96ToForm(6);
  f := TForm.Create(Application);
  try
    f.Caption := slhelp_About;
    f.BorderStyle := bsDialog;
    f.Position := poMainFormCenter;

    i := TImage.Create(f);
    i.Parent := f;
    i.Width := Scale96ToForm(128);
    i.Height := Scale96ToForm(128);
    i.Proportional := true;
    i.Picture.Icon.Assign(Application.Icon);
    i.Picture.Icon.Current := i.Picture.Icon.GetBestIndexForSize(Types.Size(256, 256));
    i.BorderSpacing.Around := d6;
    i.Anchors := [akLeft, akTop];
    i.AnchorSideTop.Control := f;
    i.AnchorSideTop.Side := asrTop;
    i.AnchorSideLeft.Control := f;
    i.AnchorSideLeft.Side := asrTop;

    l1 := TLabel.Create(f);
    l1.Parent := f;;
    l1.Alignment:=taCenter;
    l1.BorderSpacing.Around := d6;
    l1.Caption := 'LHelp';
    l1.Font.Size := 20;
    l1.Font.Style := [fsBold];
    l1.Anchors := [akLeft,akTop, akRight];
    l1.AnchorSideTop.Control := f;
    l1.AnchorSideTop.Side := asrTop;
    l1.AnchorSideLeft.Control := i;
    l1.AnchorSideLeft.Side := asrBottom;
    l1.AnchorSideRight.Control := f;
    l1.AnchorSideRight.Side := asrBottom;
    l1.AutoSize := True;

    l2 := TLabel.Create(f);
    l2.Parent := f;
    l2.Alignment := taCenter;
    l2.BorderSpacing.Around := d6;
    l2.Caption := Format(slhelp_LHelpCHMFileViewerVersionCopyrightCAndrewHainesLaz, [LineEnding, VERSION_STAMP, LineEnding +
      LineEnding, LineEnding]);
    l2.Anchors := [akLeft,akTop, akRight];
    l2.AnchorSideTop.Control := l1;
    l2.AnchorSideTop.Side := asrBottom;
    l2.AnchorSideLeft.Control := i;
    l2.AnchorSideLeft.Side := asrBottom;
    l2.AnchorSideRight.Control := f;
    l2.AnchorSideRight.Side := asrBottom;
    l2.AutoSize := True;
    //l.WordWrap := True; {don't wrap author's name}

    bv := TBevel.Create(f);
    bv.Parent := f;
    bv.Height := 2;
    bv.Shape := bsTopLine;
    bv.BorderSpacing.Around := d6;
    bv.Anchors := [akLeft, akRight, akTop];
    bv.AnchorSideLeft.Control := f;
    bv.AnchorSideLeft.Side := asrTop;
    bv.AnchorSideRight.Control := f;
    bv.AnchorSideRight.Side := asrBottom;
    if i.Top + i.Height > l2.Top + l2.Height then
      bv.AnchorSideTop.Control := i
    else
      bv.AnchorSideTop.Control := l2;
    bv.AnchorSideTop.Side := asrBottom;

    b := TButton.Create(f);
    b.Parent := f;
    b.BorderSpacing.Around := d6;
    b.Anchors := [akTop, akLeft];
    b.AnchorSideTop.Control := bv;
    b.AnchorSideTop.Side := asrBottom;
    b.AnchorSideLeft.Control := f;
    b.AnchorSideLeft.Side := asrCenter;
    b.Caption := slhelp_Ok;
    b.ModalResult := mrOk;
    b.Constraints.MinWidth := Scale96ToFont(70);
    b.AutoSize := true;

    f.AutoSize := False;
    f.AutoSize := True;
    f.ShowModal;
  finally
    f.free;
  end;
end;

procedure THelpForm.FileMenuCloseItemClick(Sender: TObject);
begin
  if Assigned(ActivePage) then
  begin
    ViewMenuContentsClick(Self);
    ActivePage.Free;
    RefreshState;
  end;
end;

procedure THelpForm.FileMenuOpenItemClick(Sender: TObject);
var
  TimerWasOn: boolean;
begin
  // Work around bug 25529: Slow dialog boxes for Windows Vista+ with
  // themes enabled
  // Stop listening to incoming server messages while busy showing dialog
  if assigned(fInputIPCTimer) Then
  begin
    TimerWasOn := fInputIPCTimer.Enabled;
    fInputIPCTimer.Enabled := False;
  end;

  try
    if OpenDialog1.Execute then
    begin
      if OpenURL('file://'+OpenDialog1.FileName) = Ord(srSuccess) then
        AddRecentFile('file://'+OpenDialog1.FileName)
      else
        MessageDlg(Format(slhelp_NotFound, [OpenDialog1.FileName]), mtError, [mbOK], 0);
      RefreshState;
    end;
  finally
    if assigned(fInputIPCTimer) Then
    begin
      fInputIPCTimer.Enabled := TimerWasOn;
    end;
  end;
end;

procedure THelpForm.FileMenuOpenURLItemClick(Sender: TObject);
var
  fRes: String;
  URLSAllowed: String;
  FileExt: String;
  Protocol: TStringList;
  FileTypes: TStringList;
  i, ii: Integer;
begin
  Protocol := GetContentProviderList;
  URLSAllowed:='';
  for i := 0 to Protocol.Count-1 do
  begin
    FileExt := '';
    if i < 1 then
      URLSAllowed := URLSAllowed + Protocol[i]
    else
      URLSAllowed := URLSAllowed + ', ' +Protocol[i];
    if TBaseContentProviderClass(Protocol.Objects[i]) = TFileContentProvider then
    begin
      FileTypes := TFileContentProviderClass(Protocol.Objects[i]).GetRegisteredFileTypes();
      for ii := 0 to Pred(FileTypes.Count) do
      begin
        if ii < 1  Then
          FileExt:= FileTypes[ii]
        else
          FileExt:= FileExt + ', '+FileTypes[ii];
      end;
      if FileExt<>'' then
        URLSAllowed := URLSAllowed + '(*'+FileExt+')';
    end;
  end;

  URLSAllowed := Trim(URLSAllowed);

  fRes:='';
  if InputQuery(slhelp_PleaseEnterAURL,
    Format(slhelp_SupportedURLTypeS, [URLSAllowed]), fRes) then
  begin
    if OpenURL(fRes) = ord(srSuccess) then
      AddRecentFile(fRes);
    RefreshState;
  end;
end;

procedure THelpForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if not fMustClose then
    if MessageDlg(slhelp_LHelp, slhelp_CloseConfirm, mtConfirmation, mbYesNo, '') <> mrYes then
  begin
    CloseAction:= caNone;
    Application.Minimize();
    Exit;
  end;
  //close all tabs to avoid AV with many tabs
  BeginUpdate;
  PageControl.ShowTabs:= False;
  while ActivePage <> nil do
    ActivePage.Free;
  EndUpdate;
  //Visible := false;
  Application.ProcessMessages;
  StopComms;
  SavePreferences;
end;

procedure THelpForm.FormCreate(Sender: TObject);
begin
  FileMenuItem.Caption := slhelp_File;
  FileMenuOpenItem.Caption := slhelp_Open;
  FileMenuOpenRecentItem.Caption := slhelp_OpenRecent;
  FileMenuOpenURLItem.Caption := slhelp_OpenURL;
  FileMenuCloseItem.Caption := slhelp_Close;
  MiHide.Caption := slhelp_Hide;
  MiQuit.Caption := slhelp_EXit;
  MiActions.Caption:= slhelp_Actions;
  MiActionsTOC.Caption:= slhelp_ActionsTOC;
  MiActionsIndex.Caption:= slhelp_ActionsIndex;
  MiActionsSearch.Caption:= slhelp_ActionsSearch;
  MiActionsGoHome.Caption:= slhelp_ActionsGoHome;
  MiActionsGoBack.Caption:= slhelp_ActionsGoBack;
  MiActionsGoForward.Caption:= slhelp_ActionsGoForward;
  ViewMenuItem.Caption := slhelp_View;
  ViewMenuContents.Caption := slhelp_ShowContents;
  ViewShowStatus.Caption := slhelp_OpenNewTabWithStatusBar;
  ViewShowSepTabs.Caption := slhelp_OpenNewFileInSeparateTab;
  HelpMenuItem.Caption := slhelp_Help;

  BackBttn.Hint:= MiActionsGoBack.Caption;
  ForwardBttn.Hint:= MiActionsGoForward.Caption;
  HomeBttn.Hint:= MiActionsGoHome.Caption;
  FileButton.Hint:= FileMenuOpenItem.Caption;

  AboutItem.Caption := slhelp_About2;

  OpenDialog1.Title := slhelp_OpenExistingFile;
  OpenDialog1.Filter := slhelp_HelpFilesChmChmAllFiles;

  fMustClose:= false;
  fContext := -1;
  fUpdateCount:= 0;
  // Safe default:
  fHide := false;
  // ReadCommandLineOptions will set fHide if requested
  ReadCommandLineOptions;
  LoadPreferences(fServerName);
  // Only start IPC if server name passed in --ipcname
  if fServerName <> '' then
  begin
    StartComms(fServerName);
  end;
  // If user wants lhelp to hide, hide entire form.
  if fHide then
    WindowState := wsMinimized
  else
    RefreshState;
end;

procedure THelpForm.FormKeyDown ( Sender: TObject; var Key: Word;
  Shift: TShiftState ) ;
begin
  //if (Shift = [ssAlt]) then
  //case Key of
  //  VK_Left: begin
  //    MiActionsGoHomeClick(Sender); key:= 0;
  //  end;
  //  VK_RIGHT: begin
  //    MiActionsGoForwardClick(Sender); key:= 0;
  //  end;
  //  VK_Home: begin
  //     MiActionsGoBackClick(Sender); key:= 0;
  //  end;
  //end;
end;


procedure THelpForm.FormShow(Sender: TObject);
begin
  if FHasShowed then
    Exit;
  FHasShowed := True;
end;

procedure THelpForm.MiActionsGoForwardClick ( Sender: TObject ) ;
begin
  if Assigned(ActivePage) then ActivePage.ContentProvider.GoForward;
end;

procedure THelpForm.MiHideClick ( Sender: TObject ) ;
begin
  if WindowState <> wsMinimized then
    Application.Minimize();
  //Visible := False;
end;

procedure THelpForm.MiActionsGoBackClick ( Sender: TObject ) ;
begin
  if Assigned(ActivePage) then ActivePage.ContentProvider.GoBack;
end;

procedure THelpForm.MiActionsGoHomeClick ( Sender: TObject ) ;
begin
  if Assigned(ActivePage) then ActivePage.ContentProvider.GoHome;
end;

procedure THelpForm.MiActionsIndexClick ( Sender: TObject ) ;
begin
  if Assigned(ActivePage) then ActivePage.ContentProvider.ActivateIndexControl;
end;

procedure THelpForm.MiQuitClick ( Sender: TObject ) ;
begin
  fMustClose:= True;
  Close();
end;

procedure THelpForm.MiActionsSearchClick ( Sender: TObject ) ;
begin
  if Assigned(ActivePage) then ActivePage.ContentProvider.ActivateSearchControl;
end;

procedure THelpForm.MiActionsTOCClick ( Sender: TObject ) ;
begin
  if Assigned(ActivePage) then ActivePage.ContentProvider.ActivateTOCControl;
end;

procedure THelpForm.PageControlChange(Sender: TObject);
begin
  RefreshState;
end;

procedure THelpForm.PageControlEnter(Sender: TObject);
begin
  RefreshState;
end;

procedure THelpForm.FormWindowStateChange(Sender: TObject);
begin
  if Windowstate = wsNormal then
  begin
    Left   := fDefWinSize.Left;
    Top    := fDefWinSize.Top;
    Width  := fDefWinSize.Width;
    Height := fDefWinSize.Height;
  end;
end;

procedure THelpForm.ApplyLayoutPreferencesOnce;
begin
  if not Assigned(fConfig) then exit;
  if (not fHide) and (not fLayoutApplied) then
  begin
    fDefWinSize.Left := fConfig.GetValue('Position/Left/Value', Left);
    fDefWinSize.Top    := fConfig.GetValue('Position/Top/Value', Top);
    fDefWinSize.Width  := fConfig.GetValue('Position/Width/Value', Width);
    fDefWinSize.Height := fConfig.GetValue('Position/Height/Value', Height);
    fDefWinMax:= fConfig.GetValue('Position/Maximized', false);

    if fDefWinMax then
    begin
      Windowstate := wsMaximized;
    end
    else
    begin
      Windowstate := wsNormal;
      Left   := fDefWinSize.Left;
      Top    := fDefWinSize.Top;
      Width  := fDefWinSize.Width;
      Height := fDefWinSize.Height;
    end;
    // Keep track so we do not reapply initial settings as user may have
    // changed size etc in the meantime.
    fLayoutApplied := true;
  end;
end;

procedure THelpForm.ViewMenuContentsClick(Sender: TObject);
begin
  // TabsControl property in TChmContentProvider
  if Assigned(ActivePage) then
  begin
    with TChmContentProvider(ActivePage.ContentProvider) do
    begin
      TabsControl.Visible := not TabsControl.Visible;
      Splitter.Visible := TabsControl.Visible;
      Splitter.Left := TabsControl.Left + 4; //for splitter to move righter
      ViewMenuContents.Checked := TabsControl.Visible;
    end;
  end;
end;

procedure THelpForm.ViewMenuItemClick(Sender: TObject);
begin
  ViewMenuContents.Checked :=
    Assigned(ActivePage) and
    (ActivePage.ContentProvider is TChmContentProvider) and
    (ActivePage.ContentProvider as TChmContentProvider).TabsControl.Visible;
  ViewShowSepTabs.Checked := fShowSepTabs;
  ViewShowStatus.Checked := fShowStatus;
end;

procedure THelpForm.ViewShowSepTabsClick(Sender: TObject);
begin
  fShowSepTabs := not fShowSepTabs;
end;

procedure THelpForm.ViewShowStatusClick(Sender: TObject);
begin
  fShowStatus := not fShowStatus;
end;

procedure THelpForm.LoadPreferences(AIPCName: String);
var
  PrefFile: String;
  RecentCount: Integer;
  ServerPart: String;
  i: Integer;
begin
  PrefFile := GetAppConfigDirUTF8(False);
  ForceDirectoriesUTF8(PrefFile);
  // --ipcname passes a server ID that consists of a
  // server-dependent constant together with a process ID.
  // Strip out the formatted process ID to get fixed config file names for
  // one server
  if (AIPCName <> '') then
    ServerPart := Copy(AIPCName, 1, length(AIPCName)-DigitsInPID)
  else
    ServerPart:= 'standalone';
  PrefFile := Format('%slhelp-%s.conf',[IncludeTrailingPathDelimiter(PrefFile), ServerPart]);

  fConfig := TXMLConfig.Create(Self);
  fConfig.Filename := PrefFile;

  // Restore window but only if currently not being asked to hide
  ApplyLayoutPreferencesOnce;
  OpenDialog1.FileName := fConfig.GetValue('LastFileOpen/Value', OpenDialog1.FileName);

  RecentCount:= fConfig.GetValue('Recent/ItemCount/Value', 0);
  // downto since oldest are knocked off the list:
  for i := RecentCount-1 downto 0 do
    AddRecentFile(fConfig.GetValue('Recent/Item'+IntToStr(i)+'/Value',''));

  fShowSepTabs := fConfig.GetValue('OpenSepTabs/Value', true);
  fShowStatus := fConfig.GetValue('OpenWithStatus/Value', true);
end;

procedure THelpForm.SavePreferences;
var
  i: Integer;
begin
  if not Assigned(fConfig) then
    exit; //silently abort

  if (WindowState <> wsMaximized) then
  begin
    fConfig.SetValue('Position/Maximized', false);
    fConfig.SetValue('Position/Left/Value', Left);
    fConfig.SetValue('Position/Top/Value', Top);
    fConfig.SetValue('Position/Width/Value', Width);
    fConfig.SetValue('Position/Height/Value', Height);
  end
  else
  begin
    fConfig.SetValue('Position/Maximized', true);
  end;

  fConfig.SetValue('LastFileOpen/Value', OpenDialog1.FileName);

  fConfig.SetValue('Recent/ItemCount/Value', FileMenuOpenRecentItem.Count);
  // downto since oldest are knocked off the list:
  for i := 0 to FileMenuOpenRecentItem.Count-1 do
    fConfig.SetValue('Recent/Item'+IntToStr(i)+'/Value', TRecentMenuItem(FileMenuOpenRecentItem.Items[I]).URL);

  fConfig.SetValue('OpenSepTabs/Value', fShowSepTabs);
  fConfig.SetValue('OpenWithStatus/Value', fShowStatus);

  fConfig.Flush;
  fConfig.Free;
end;

procedure THelpForm.AddRecentFile(AFileName: String);
var
  Item : TRecentMenuItem;
  MaxHistory: longint;
  i: Integer;
begin
  for i := FileMenuOpenRecentItem.Count-1 downto 0 do
    if TRecentMenuItem(FileMenuOpenRecentItem.Items[i]).URL = AFileName then
    begin
      FileMenuOpenRecentItem.Delete(i);
    end;
  Item := TRecentMenuItem.Create(FileMenuOpenRecentItem);
  Item.Caption:=ExtractFileNameOnly(AFileName);
  Item.URL:=AFileName;
  Item.OnClick:=@OpenRecentItemClick;
  Item.Hint:=Item.URL;
  FileMenuOpenRecentItem.Insert(0, Item);

  MaxHistory := fConfig.GetValue('Recent/HistoryCount/Value', 10);

  if FileMenuOpenRecentItem.Count > 0 then
    FileMenuOpenRecentItem.Enabled:=True;

  if FileMenuOpenRecentItem.Count > MaxHistory then
    FileMenuOpenRecentItem.Items[MaxHistory-1].Free;
end;

procedure THelpForm.DeleteRecentFile ( AFileName: String ) ;
var
  i: Integer;
begin
  for i := FileMenuOpenRecentItem.Count-1 downto 0 do
    if TRecentMenuItem(FileMenuOpenRecentItem.Items[i]).URL = AFileName then
      FileMenuOpenRecentItem.Delete(i);
end;

procedure THelpForm.ContentTitleChange(sender: TObject);
begin
  if ActivePage = nil then
    Exit;
  Caption := Format(slhelp_LHelp2, [ActivePage.fContentProvider.Title]);
end;

procedure THelpForm.OpenRecentItemClick(Sender: TObject);
var
  Item: TRecentMenuItem absolute Sender;
  res: DWord;
begin
  res := OpenURL(Item.URL);
  if res = Ord(srSuccess) then
    AddRecentFile(Item.URL)
  else
  begin
    MessageDlg(Format(slhelp_NotFound, [Item.URL]), mtError, [mbOK], 0);
    DeleteRecentFile(Item.URL);
  end;
end;

procedure THelpForm.SendResponse(Response: DWord);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.WriteDWord(Response);
    if assigned(fOutputIPC) and fOutputIPC.Active then
      fOutputIPC.SendMessage(mtUnknown, Stream);
  finally
    Stream.Free;
  end;
end;

procedure THelpForm.ServerMessage(Sender: TObject);
var
  UrlReq: TUrlRequest;
  FileReq: TFileRequest;
  ConReq: TContextRequest;
  MiscReq: TMiscRequest;
  Stream: TStream;
  Res: LongWord;
  Url: String='';
begin
  while fInputIPC.PeekMessage(5, True) do
  begin
    Stream := fInputIPC.MsgData;
    Stream.Position := 0;
    FillByte(FileReq{%H-},SizeOf(FileReq),0);
    Stream.Read(FileReq, SizeOf(FileReq));
    Res := Ord(srError); //fail by default
    case FileReq.RequestType of
      rtFile:
      begin
        Url := 'file://'+FileReq.FileName;
        DebugLn('got rtFile, filename '+filereq.filename);
        {$IFDEF SHOW_ON_REQUEST}
        fHide := false;
        RefreshState;
        ShowApp();
        {$ENDIF}
        Res := OpenURL(URL);
      end;
      rtUrl:
      begin
        Stream.Position := 0;
        FillByte(UrlReq{%H-},SizeOf(UrlReq),0);
        Stream.Read(UrlReq, SizeOf(UrlReq));
        {$IFDEF SHOW_ON_REQUEST}
        fHide := false;
        RefreshState;
        ShowApp();
        {$ENDIF}
        if UrlReq.FileRequest.FileName <> '' then
        begin
          Url := 'file://'+UrlReq.FileRequest.FileName;
          DebugLn('got rturl, filename '+urlreq.filerequest.filename+', url '+urlreq.url);
          Res := OpenUrl(URL+'://'+UrlReq.Url);
        end
        else
        begin
          Url := UrlReq.Url;
          DebugLn('got rturl, filename '+urlreq.filerequest.filename+', url '+urlreq.url);
          Res := OpenURL(Url);
        end;
      end;
      rtContext:
      begin
        Stream.Position := 0;
        FillByte(ConReq{%H-},SizeOf(ConReq),0);
        Stream.Read(ConReq, SizeOf(ConReq));
        Url := 'file://'+ConReq.FileRequest.FileName;
        DebugLn('got rtcontext, filename '+ConReq.FileRequest.FileName+', context '+inttostr(ConReq.HelpContext));
        {$IFDEF SHOW_ON_REQUEST}
        fHide := false;
        RefreshState;
        ShowApp();
        {$ENDIF}
        Res := OpenURL(Url, ConReq.HelpContext);
      end;
      rtMisc:
      begin
        Stream.Position := 0;
        FillByte(MiscReq{%H-},SizeOf(MiscReq),0);
        Stream.Read(MiscReq, SizeOf(MiscReq));
        case MiscReq.RequestID of
          mrClose:
          begin
            fMustClose:=true;
            Res:= ord(srSuccess);
            //DebugLn('got rtmisc/mrClose');
          end;
          mrShow:
          begin
            Res := ord(srSuccess);
            DebugLn('got rtmisc/mrShow');
            fHide := false;
            if (fUpdateCount = 0) and (fLastHiddenRequest <> '')then
            begin
              DebugLn('mrShow OpenUrl '+fLastHiddenRequest);
              Res := OpenURL(fLastHiddenRequest);
              fLastHiddenRequest:= '';
            end;
            RefreshState;
            ShowApp();
          end;
          mrVersion:
          begin
            // Protocol version encoded in the filename
            // Verify what we support
            if strtointdef(MiscReq.FileRequest.FileName,0)=strtointdef(PROTOCOL_VERSION,0) then
              Res := ord(srSuccess)
            else
              Res := ord(srError); //version not supported
            DebugLn('got rtmisc/version ( ver=' + MiscReq.FileRequest.FileName + ' )' +
                    ' Success: ', BoolToStr((Res = ord(srSuccess)), true));
          end;
          mrBeginUpdate:
          begin
            DebugLn('got BeginUpdate');
            BeginUpdate;
            Res := ord(srSuccess);
          end;
          mrEndUpdate:
          begin
            DebugLn('got EndUpdate');
            EndUpdate;
            Res := ord(srSuccess);
          end;
        end;
      end; //rtMisc
    end;

    // This may take some time which may allow receiving end to get ready for
    // receiving messages
    if (URL<>'') and (Res = Ord(srSuccess)) then
      AddRecentFile(Url);
    // Receiving end may not yet be ready (observed with an Intel Core i7),
    // so perhaps wait a bit?
    // Unfortunately, the delay time is guesswork=>Sleep(80)?
    SendResponse(Res); //send response again in case first wasn't picked up
    // Keep after SendResponse to avoid timing issues (e.g. writing to log file):
    //debugln('Just sent TLHelpResponse code: '+inttostr(Res));

    // On received any command:
    if (fMustClose = false) and (fHide = false) then
    begin
      // If lhelp was run with hidden parameter, we need to apply
      // layout preferences once:
      ApplyLayoutPreferencesOnce;
    end;

    if fMustClose then
    begin
      Close;
      Application.Terminate;
    end;
  end;
end;

procedure THelpForm.ReadCommandLineOptions;
var
  X, S: Integer;
  IsHandled: array[0..50] of boolean;
  URL: String;
  StrItem: PStringItem;
  Filename: String;
begin
  FillChar(IsHandled{%H-}, 51, 0);
  X := 1;
  while X <= ParamCount do
  begin
    if CompareText(ParamStrUTF8(X), '--ipcname') = 0 then
    begin
      // IPC name; includes unique PID or other identifier
      IsHandled[X] := True;
      inc(X);
      if X <= ParamCount then
      begin
        fServerName := ParamStrUTF8(X);
        IsHandled[X] := True;
        inc(X);
        DebugLn('Start IPCNAME = ', fServerName);
      end;
    end
    else if CompareText(ParamStrUTF8(X), '--context') = 0 then
    begin
      IsHandled[X] := True;
      inc(X);
      if (X <= ParamCount) then
        if TryStrToInt(ParamStrUTF8(X), fContext) then
        begin
          IsHandled[X] := True;
          inc(X);
          DebugLn('Start CONTEXT = ', IntToStr(fContext));
        end;
    end
    else if CompareText(ParamStrUTF8(X), '--hide') = 0 then
    begin
      IsHandled[X] := True;
      inc(X);
      fHide:=true;
      DebugLn('Start HIDE = True');
    end
    else
    begin
      IsHandled[X] := copy(ParamStrUTF8(X),1,1)='-'; // ignore other parameters
      inc(X);
    end;
  end;

  // Loop through a second time for the URL
  for X := 1 to ParamCount do
    if not IsHandled[X] then
    begin
      //DoOpenChm(ParamStrUTF8(X));
      URL := ParamStrUTF8(X);
      if Pos('://', URL) = 0 then
        URL := 'file://'+URL;
      Filename:=URL;
      //      if copy(Filename,1,length('file://'))='file://' then
      if pos('file://', FileName) = 1 then
      begin
        System.Delete(Filename,1,length('file://'));
        S:= System.Pos('?', Filename);
        if S > 0 then System.Delete(Filename, S, Length(FileName));
        Filename := SetDirSeparators(Filename);
        if not FileExistsUTF8(Filename) then
        begin
          DebugLn(['THelpForm.ReadCommandLineOptions file not found "',Filename,'"']);
          continue;
        end;
      end;
      // https://www.freepascal.org/docs-html/rtl/system/initialize.html
      GetMem(StrItem, SizeOf(TStringItem));
      Initialize(StrItem^);
      StrItem^.FString := URL;
      DebugLn('Start URL = ', URL);
      Application.QueueAsyncCall(TDataEvent(@LateOpenURL), {%H-}PtrUInt(StrItem));
      Break;
    end;
end;

procedure THelpForm.StartComms(ServerName: String);
// Starts IPC server and client for two-way communication with
// controlling program (e.g. Lazarus IDE).

// Only useful if IPC serverID is passed through the --ipcname
// command.
begin
  fInputIPC := TSimpleIPCServer.Create(nil);
  fInputIPC.ServerID := ServerName;
  fInputIPC.Global := True;
  fInputIPC.Active := True;
  IPCServer := fInputIPC;

  // Use timer to check for incoming messages from the IDE
  fInputIPCTimer := TTimer.Create(nil);
  fInputIPCTimer.OnTimer := @ServerMessage;
  fInputIPCTimer.Interval := 200; //milliseconds
  fInputIPCTimer.Enabled := True;
  ServerMessage(nil);

  fOutputIPC := TSimpleIPCClient.Create(nil);
  fOutputIPC.ServerID := ServerName+'client';
  try
    if fOutputIPC.ServerRunning then
      fOutputIPC.Active := True;
  except
    fOutputIPC.Active := False;
  end;
  IPCClient := fOutputIPC;
end;

procedure THelpForm.StopComms;
begin
  if fInputIPC <> nil then
  begin
    if fInputIPC.Active then
      fInputIPC.Active := False;

    FreeAndNil(fInputIPC);
    IPCServer := nil;
    FreeAndNil(fInputIPCTimer);
  end;

  if fOutputIPC <> nil then
  begin
    if fOutputIPC.Active then
      fOutputIPC.Active := False;

    FreeAndNil(fOutputIPC);
    IPCClient := nil;
  end;
end;

function THelpForm.OpenURL(const AURL: String; AContext: THelpContext): DWord;

var
  fURLPrefix: String;
  ContentProviderClass: TBaseContentProviderClass;
  fPage: TContentTab = nil;
  fFirstSameTypePage: TContentTab = nil;
  I: Integer;
  fIsNewPage: Boolean = false;
begin
  Result := Ord(srInvalidURL);
  fURLPrefix := GetUriPrefix(AURL);
  ContentProviderClass := GetContentProvider(fURLPrefix);

  if ContentProviderClass = nil then
  begin
    ShowError(Format(slhelp_CannotHandleThisTypeOfContentForUrl, [fURLPrefix, LineEnding, AURL]));
    Result := Ord(srInvalidURL);
    Exit;
  end;

  ContentProviderClass := ContentProviderClass.GetProperContentProvider(AURL);
  if ContentProviderClass = nil then
  begin
    ShowError(Format(slhelp_CannotHandleThisTypeOfSubcontentForUrl, [fURLPrefix, LineEnding, AURL]));
    Result := Ord(srInvalidURL);
    Exit;
  end;

  Result := Ord(srInvalidFile);

  if (fUpdateCount > 0) then
  begin
    fLastHiddenRequest:= AURL;
    //DebugLn('set fLastHiddenRequest: ', AURL);
  end;

  // Searching a page for loading or refreshing data
  for I := 0 to PageControl.PageCount-1 do
  begin
    fPage := TContentTab(PageControl.Pages[I]);
    if ContentProviderClass.ClassName = fPage.ContentProvider.ClassName then
    begin
      if fFirstSameTypePage = nil then fFirstSameTypePage:= fPage;
      if fPage.ContentProvider.HasLoadedData(AURL) then // need to update data
       break;
    end;
    fPage := nil;
  end;

  if (fPage = nil) and (Assigned(fFirstSameTypePage) and (not fShowSepTabs)) then
  begin // Page with data not found but exists the page with same type
    fPage := fFirstSameTypePage;
  end;

  if (fPage <> nil ) then // load or refresh a data within this page
  begin
    if fPage.ContentProvider.LoadURL(AURL, AContext) then
    begin
     PageControl.ActivePage := fPage;
     Result := Ord(srSuccess);
    end
    else
    begin
     fPage := nil;
     Result := Ord(srInvalidFile);
    end;
  end;

  if fPage = nil then
  begin
    // none existing page that can handle this content, so create one
    fIsNewPage := true;
    fPage := TContentTab.Create(PageControl);
    fPage.ContentProvider := ContentProviderClass.Create(fPage, ImageList1, fUpdateCount);
    fPage.ContentProvider.OnTitleChange := @ContentTitleChange;
    //fPage.ContentProvider.OnContentComplete := @DoShowContent;
    fPage.Parent := PageControl;
    //SetKeyUp(fPage);
    fPage.ContentProvider.LoadPreferences(fConfig);
    if fPage.ContentProvider is TChmContentProvider then
     (fPage.ContentProvider as TChmContentProvider).ShowStatusbar := fShowStatus;

    // BeginUpdate doing into Create
    if fPage.ContentProvider.LoadURL(AURL, AContext) then
    begin
      PageControl.ActivePage := fPage;
      RefreshState;
      Result := Ord(srSuccess);
    end
    else
    begin
      Result := Ord(srInvalidURL);
      if fIsNewPage then
        fPage.Free;
    end;
  end;
end;


procedure THelpForm.LateOpenURL ( Url: PStringItem ) ;
begin
  if OpenURL(URL^.FString, fContext) = ord(srSuccess) then
    AddRecentFile(URL^.FString);
  // we reset the context because at this point the file has been loaded and the
  // context shown
  fContext := -1;

  Finalize(Url^);
  Freemem(Url);
  RefreshState;
end;

function THelpForm.ActivePage: TContentTab;
begin
  Result := TContentTab(PageControl.ActivePage);
end;

procedure THelpForm.RefreshState;
var
  en: Boolean;
begin
  if fHide then
  begin
    en := false;
    // Hide content page
    if Assigned(ActivePage) then
    begin
      with TChmContentProvider(ActivePage.ContentProvider) do
      begin
        ActivePage.Visible := false;
        Visible := false;
        TabsControl.Visible := false;
        Splitter.Visible := false;
      end;
    end;
  end
  else
  begin
    en := Assigned(ActivePage);
    // Show content page
    if en then
    begin
      with TChmContentProvider(ActivePage.ContentProvider) do
      begin
        ActivePage.Visible := true;
        Visible := true;
        TabsControl.Visible := true;
        Splitter.Visible := true;
        ActivateProvider;
      end;
    end;
  end;

  MiActionsGoBack.Enabled:= en;
  MiActionsGoForward.Enabled:=en;
  MiActionsGoHome.Enabled:=en;
  MiActionsIndex.Enabled:=en;
  MiActionsTOC.Enabled:=en;
  MiActionsSearch.Enabled:=en;

  HomeBttn.Enabled := en;
  FileMenuCloseItem.Enabled := en;
  ViewMenuContents.Enabled := en;

  if en and not (csDestroying in ActivePage.ComponentState) then
    Caption := Format(slhelp_LHelp2, [ActivePage.fContentProvider.Title])
  else
    Caption := slhelp_LHelp;
end;

procedure THelpForm.ShowError(AError: String);
begin
  ShowMessage(AError);
end;

procedure THelpForm.BeginUpdate;
var
  Tab: TContentTab;
  i: Integer;
begin
  Inc(fUpdateCount);
  //WriteLn(Format('>> fUpdateCount:=%d',[fUpdateCount]));
  begin
    for i := 0 to PageControl.PageCount-1 do
    begin
      Tab := TContentTab(PageControl.Pages[I]);
      Tab.ContentProvider.BeginUpdate;
    end;
  end;
end;

procedure THelpForm.EndUpdate;
var
  Tab: TContentTab;
  i: Integer;
begin
  Dec(fUpdateCount);
  if fUpdateCount < 0 then
   fUpdateCount:=0;
  //WriteLn(Format('<< fUpdateCount:=%d',[fUpdateCount ]));
  begin
    for i := 0 to PageControl.PageCount-1 do
    begin
      Tab := TContentTab(PageControl.Pages[I]);
      Tab.ContentProvider.EndUpdate;
    end;
  end;
end;

procedure THelpForm.ShowApp();
begin
  if (fHide = false) then
  begin
    fMustClose:= false;
    Application.Restore;
    Application.BringToFront;
    MiActionsTOCClick(Nil);   // Go to TOC TreeView.
  end;
end;
{
procedure THelpForm.DoShowContent(Sender: Tobject);
begin
  ShowApp();
end;
}

{ TContentTab }

constructor TContentTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TContentTab.Destroy;
begin
  fContentProvider.Free;
  inherited Destroy;
end;

finalization
  if IPCServer <> nil then
    FreeAndNil(IPCServer);
  if IPCClient <> nil then
    FreeAndNil(IPCClient);
end.

