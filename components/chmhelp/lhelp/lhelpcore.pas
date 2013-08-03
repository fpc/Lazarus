{ Copyright (C) 2005-2013  Andrew Haines, Lazarus contributors

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
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit lhelpcore;

{$IFDEF LNET_VISUAL}
{$DEFINE USE_LNET} // you must manually add the lnetvisual.lpk package to the dependancy list
{$ELSE}
{$NOTE You can add http capability to lhelp by adding the lnetvisual package v0.6.3 or greater requirement to lhelp.}
{$ENDIF}

{$IFDEF UNIX}
  {$if FPC_FULLVERSION <= 20700}
       {$DEFINE STALE_PIPE_WORKAROUND}
  {$ENDIF}
{$ENDIF}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SimpleIPC, Laz2_XMLCfg,
  FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, LCLProc, IpHtml, ComCtrls, ExtCtrls, Menus, LCLType, LCLIntf, StdCtrls,
  BaseContentProvider, FileContentProvider,
  ChmContentProvider
  {$IFDEF USE_LNET}, HTTPContentProvider{$ENDIF};

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
    FileMenuExitItem: TMenuItem;
    FileMenuItem: TMenuItem;
    FileMenuOpenItem: TMenuItem;
    FileSeperater: TMenuItem;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    FileMenuOpenURLItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    AboutItem: TMenuItem;
    FileMenuOpenRecentItem: TMenuItem;
    PageControl: TPageControl;
    Panel1: TPanel;
    ForwardBttn: TSpeedButton;
    BackBttn: TSpeedButton;
    HomeBttn: TSpeedButton;
    OpenDialog1: TOpenDialog;
    ViewMenuContents: TMenuItem;
    ViewMenuItem: TMenuItem;
    procedure AboutItemClick(Sender: TObject);
    procedure BackToolBtnClick(Sender: TObject);
    procedure FileMenuCloseItemClick(Sender: TObject);
    procedure FileMenuExitItemClick(Sender: TObject);
    procedure FileMenuOpenItemClick(Sender: TObject);
    procedure FileMenuOpenURLItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure ForwardToolBtnClick(Sender: TObject);
    procedure HomeToolBtnClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure PageControlEnter(Sender: TObject);
    procedure ViewMenuContentsClick(Sender: TObject);
  private
    { private declarations }
    fServerName: String;
    // Receives commands from IDE
    fInputIPC: TSimpleIPCServer;
    // Sends responses back to IDE
    // only used if lhelp started with --ipcname to indicate
    // IPC communication method should be used
    fOutputIPC: TSimpleIPCClient;
    fServerTimer: TTimer;
    fContext: LongInt; // used once when we are started on the command line with --context
    fConfig: TXMLConfig;
    FHasShowed: Boolean;
    fHide: boolean; //If yes, start with content hidden. Otherwise start normally
    // Load preferences; separate preferences per coupled server/IDE
    procedure LoadPreferences(AIPCName: String);
    // Saves preferences. Uses existing config loaded by LoadPreferences
    procedure SavePreferences;
    procedure AddRecentFile(AFileName: String);
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
    // Set keyup handler for control (and any child controls)
    procedure SetKeyUp(AControl: TControl);
  public
    { public declarations }
  end;
  

var
  HelpForm: THelpForm;
  IPCClient: TSimpleIPCClient;
  IPCServer: TSimpleIPCServer;

const
  INVALID_FILE_TYPE = 1;
  VERSION_STAMP = '2013-07-31'; //used in displaying about form etc

implementation

{$R *.lfm}

uses 
  LHelpControl;

type
  TRecentMenuItem = class(TMenuItem)
  public
    URL: String;
  end;

{ THelpForm }


procedure THelpForm.BackToolBtnClick(Sender: TObject);
begin
  if Assigned(ActivePage) then ActivePage.ContentProvider.GoBack;
end;

procedure THelpForm.AboutItemClick(Sender: TObject);
var
  f: TForm;
  l: TLabel;
  b: TButton;
begin
  f := TForm.Create(Application);
  try
    f.Caption := 'About';
    f.BorderStyle := bsDialog;
    f.Position := poMainFormCenter;
    f.Constraints.MinWidth := 150;
    f.Constraints.MaxWidth := 350;
    l := TLabel.Create(f);
    l.Parent := f;;
    l.Align := alTop;
    l.BorderSpacing.Around := 6;
    l.Caption := 'LHelp (CHM file viewer)' + LineEnding +
      'Version ' + VERSION_STAMP + LineEnding +
      'Copyright (C) Andrew Haines, ' + LineEnding +
      'Lazarus contributors';
    l.AutoSize := True;
    l.WordWrap := True;
    b := TButton.Create(f);
    b.Parent := f;
    b.BorderSpacing.Around := 6;
    b.Anchors := [akTop, akLeft];
    b.AnchorSide[akTop].Control := l;
    b.AnchorSide[akTop].Side := asrBottom;
    b.AnchorSide[akLeft].Control := f;
    b.AnchorSide[akLeft].Side := asrCenter;
    b.Caption := 'Ok';
    b.ModalResult := mrOk;
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

procedure THelpForm.FileMenuExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure THelpForm.FileMenuOpenItemClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    if OpenURL('file://'+OpenDialog1.FileName) = Ord(srSuccess) then
      AddRecentFile('file://'+OpenDialog1.FileName);
    RefreshState;
  end;
end;

procedure THelpForm.FileMenuOpenURLItemClick(Sender: TObject);
var
  fRes: String;
  URLSAllowed: String;
  Protocall: TStrings;
  i: Integer;
begin
  Protocall := GetContentProviderList;

  URLSAllowed:='';
  for i := 0 to Protocall.Count-1 do
  begin
    if i < 1 then
      URLSAllowed := URLSAllowed + Protocall[i]
    else
      URLSAllowed := URLSAllowed + ', ' +Protocall[i]
  end;
  Protocall.Free;

  URLSAllowed := Trim(URLSALLowed);

  fRes:='';
  if InputQuery('Please enter a URL', 'Supported URL type(s): (' +URLSAllowed+ ')', fRes) then
  begin
    if OpenURL(fRes) = ord(srSuccess) then
      AddRecentFile(fRes);
    RefreshState;
  end;
end;

procedure THelpForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Visible:= False;
  Application.ProcessMessages;
  FileMenuCloseItemClick(Sender);
  StopComms;
  SavePreferences;
end;

procedure THelpForm.FormCreate(Sender: TObject);
begin
  fContext := -1;
  fHide := false;
  ReadCommandLineOptions;
  LoadPreferences(fServerName);
  // Only start IPC if server name passed in --ipcname
  if fServerName <> '' then begin
    StartComms(fServerName);
  end;
  RefreshState;
  SetKeyUp(Self);
end;

procedure THelpForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
  // Backspace: go to previous page (as if BackBttn were clicked)
  if Key = VK_BACK then
    if Assigned(ActivePage) then ActivePage.ContentProvider.GoBack;
end;

procedure THelpForm.FormShow(Sender: TObject);
begin
  if FHasShowed then
    Exit;
  FHasShowed := True;
end;

procedure THelpForm.ForwardToolBtnClick(Sender: TObject);
begin
  if Assigned(ActivePage) then ActivePage.ContentProvider.GoForward;
end;

procedure THelpForm.HomeToolBtnClick(Sender: TObject);
begin
  if Assigned(ActivePage) then ActivePage.ContentProvider.GoHome;
end;

procedure THelpForm.PageControlChange(Sender: TObject);
begin
  RefreshState;
end;

procedure THelpForm.PageControlEnter(Sender: TObject);
begin
  RefreshState;
end;

procedure THelpForm.ViewMenuContentsClick(Sender: TObject);
begin
  //TabsControl property in TChmContentProvider
  if Assigned(ActivePage) then
    with TChmContentProvider(ActivePage.ContentProvider) do
    begin
      TabsControl.Visible := not TabsControl.Visible;
      Splitter.Visible := TabsControl.Visible;
      Splitter.Left := TabsControl.Left + 4; //for splitter to move righter
      ViewMenuContents.Checked := TabsControl.Visible;
    end;
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
  // a server-dependent constant together with a process ID.
  // Strip out the process ID to get fixed config file names for one server
  ServerPart := Copy(AIPCName, 1, length(AIPCName)-5); //strip out PID
  PrefFile:=Format('%slhelp-%s.conf',[IncludeTrailingPathDelimiter(PrefFile), ServerPart]);

  fConfig := TXMLConfig.Create(Self);
  fConfig.Filename:=PrefFile;

  Left   := fConfig.GetValue('Position/Left/Value',   Left);
  Top    := fConfig.GetValue('Position/Top/Value',    Top);
  Width  := fConfig.GetValue('Position/Width/Value',  Width);
  Height := fConfig.GetValue('Position/Height/Value', Height);

  if fConfig.GetValue('Position/Maximized',false)=true then
    Windowstate:=wsMaximized;

  OpenDialog1.FileName := fConfig.GetValue('LastFileOpen/Value', OpenDialog1.FileName);

  RecentCount:= fConfig.GetValue('Recent/ItemCount/Value', 0);

  for i := RecentCount-1 downto 0 do // downto since oldest are knocked off the list
    AddRecentFile(fConfig.GetValue('Recent/Item'+IntToStr(i)+'/Value',''));
end;

procedure THelpForm.SavePreferences;
var
  i: Integer;
begin
  if not (WindowState = wsMaximized) then
  begin
    fConfig.SetValue('Position/Left/Value',   Left);
    fConfig.SetValue('Position/Top/Value',    Top);
    fConfig.SetValue('Position/Width/Value',  Width);
    fConfig.SetValue('Position/Height/Value', Height);
  end
  else
  begin
    fConfig.SetValue('Position/Maximized', true);
  end;

  fConfig.SetValue('LastFileOpen/Value', OpenDialog1.FileName);

  fConfig.SetValue('Recent/ItemCount/Value', FileMenuOpenRecentItem.Count);
  for i := 0 to FileMenuOpenRecentItem.Count-1 do // downto since oldest are knocked off the list
    fConfig.SetValue('Recent/Item'+IntToStr(i)+'/Value', TRecentMenuItem(FileMenuOpenRecentItem.Items[I]).URL);

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

  MaxHistory := fConfig.GetValue('Recent/HistoryCount/Value', 5);

  if FileMenuOpenRecentItem.Count > 0 then
    FileMenuOpenRecentItem.Enabled:=True;

  if FileMenuOpenRecentItem.Count > MaxHistory then
    FileMenuOpenRecentItem.Items[MaxHistory-1].Free;


end;

procedure THelpForm.ContentTitleChange(sender: TObject);
begin
  if ActivePage = nil then
    Exit;

  Caption := 'LHelp - ' + ActivePage.fContentProvider.Title;
end;

procedure THelpForm.OpenRecentItemClick(Sender: TObject);
var
  Item: TRecentMenuItem absolute Sender;
begin
  OpenURL(Item.URL);
  AddRecentFile(Item.URL);
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
  MustClose: boolean=false;
  Stream: TStream;
  Res: LongWord;
  Url: String='';
begin
  if fInputIPC.PeekMessage(5, True) then begin
    Stream := fInputIPC.MsgData;
    Stream.Position := 0;
    FillByte(FileReq{%H-},SizeOf(FileReq),0);
    Stream.Read(FileReq, SizeOf(FileReq));
    case FileReq.RequestType of
      rtFile    : begin
                    Url := 'file://'+FileReq.FileName;
                    Res := OpenURL(URL);
                  end;
      rtUrl     : begin
                    Stream.Position := 0;
                    FillByte(UrlReq{%H-},SizeOf(UrlReq),0);
                    Stream.Read(UrlReq, SizeOf(UrlReq));
                    if UrlReq.FileRequest.FileName <> '' then
                    begin
                      Url := 'file://'+UrlReq.FileRequest.FileName;
                      Res := OpenUrl(URL+'://'+UrlReq.Url)
                    end
                    else
                    begin
                      Url := UrlReq.Url;
                      Res := OpenURL(Url);
                    end;
                  end;
      rtContext : begin
                    Stream.Position := 0;
                    FillByte(ConReq{%H-},SizeOf(ConReq),0);
                    Stream.Read(ConReq, SizeOf(ConReq));
                    Url := 'file://'+FileReq.FileName;
                    Res := OpenURL(Url, ConReq.HelpContext);
                  end;
      rtMisc    : begin
                    Stream.Position := 0;
                    FillByte(MiscReq{%H-},SizeOf(MiscReq),0);
                    Stream.Read(MiscReq, SizeOf(MiscReq));
                    case MiscReq.RequestID of
                      mrClose:
                      begin
                        MustClose:=true;
                        Res:= ord(srSuccess);
                      end;
                      mrShow:
                      begin
                        fHide := false;
                        PageControl.Visible:=true;
                        Res := ord(srSuccess);
                      end;
                      mrVersion:
                      begin
                        //Protocol version encoded in the filename
                        // Verify what we support
                        if strtointdef(FileReq.FileName,0)=strtointdef(PROTOCOL_VERSION,0) then
                          Res := ord(srSuccess)
                        else
                          Res := ord(srError); //version not supported
                      end
                      else {Unknown}
                        Res := ord(srUnknown);
                    end;
                  end;
    end;
    if (URL<>'') and (Res = Ord(srSuccess)) then
      AddRecentFile(Url);
    SendResponse(Res);
    if MustClose then
      Application.Terminate;

    if (MustClose=false) and (fHide=false) then
    begin
      Self.SendToBack;
      Self.BringToFront;
      Self.ShowOnTop;
    end;
  end;
end;

procedure THelpForm.ReadCommandLineOptions;
var
  X: Integer;
  IsHandled: array[0..50] of boolean;
  URL: String;
  StrItem: PStringItem;
  Filename: String;
begin
  FillChar(IsHandled{%H-}, 51, 0);
  X:=1;
  while X<=ParamCount do begin
    if LowerCase(ParamStrUTF8(X)) = '--ipcname' then begin
      IsHandled[X] := True;
      inc(X);
      if X <= ParamCount then begin
        fServerName := ParamStrUTF8(X);
        IsHandled[X] := True;
        inc(X);
      end;
    end else if LowerCase(ParamStrUTF8(X)) = '--context' then begin
      IsHandled[X] := True;
      inc(X);
      if (X <= ParamCount) then
        if TryStrToInt(ParamStrUTF8(X), fContext) then begin
          IsHandled[X] := True;
          inc(X);
        end;
    end else if LowerCase(ParamStrUTF8(X)) = '--hide' then begin
      IsHandled[X] := True;
      inc(X);
      fHide:=true;
    end else begin
      IsHandled[X]:=copy(ParamStrUTF8(X),1,1)='-'; // ignore other parameters
      inc(X);
    end;
  end;

  // Loop through a second time for the url
  for X := 1 to ParamCount do
    if not IsHandled[X] then begin
      //DoOpenChm(ParamStrUTF8(X));
      URL:=ParamStrUTF8(X);
      if Pos('://', URL) = 0 then
        URL := 'file://'+URL;
      Filename:=URL;
      if copy(Filename,1,length('file://'))='file://' then begin
        System.Delete(Filename,1,length('file://'));
        Filename:=SetDirSeparators(Filename);
        if not FileExistsUTF8(Filename) then begin
          debugln(['THelpForm.ReadCommandLineOptions file not found "',Filename,'"']);
          continue;
        end;
      end;
      StrItem := New(PStringItem);
      StrItem^.FString := URL;
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

  fServerTimer := TTimer.Create(nil);
  fServerTimer.OnTimer := @ServerMessage;
  fServerTimer.Interval := 200;
  fServerTimer.Enabled := True;
  ServerMessage(nil);

  fOutputIPC := TSimpleIPCClient.Create(nil);
  fOutputIPC.ServerID := ServerName+'client';
  try
    if fOutputIPC.ServerRunning {$IFDEF STALE_PIPE_WORKAROUND} and not IPCPipeIsStale(fOutputIPC){$ENDIF}
    then
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
     FreeAndNil(fServerTimer);
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
  function GetURLPrefix: String;
  var
    fPos: Integer;
  begin
    fPos := Pos('://', AURL);
    Result := Copy(AURL, 1, fPos+2);
  end;
var
 fURLPrefix: String;
 fContentProvider: TBaseContentProviderClass;
 fRealContentProvider: TBaseContentProviderClass;
 fPage: TContentTab = nil;
 I: Integer;
begin
 Result := Ord(srUnknown);
 fURLPrefix := GetURLPrefix;
 fContentProvider := GetContentProvider(fURLPrefix);
 
 if fContentProvider = nil then begin
   ShowError('Cannot handle this type of content. "' + fURLPrefix + '" for url:'+LineEnding+AURL);
   Result := Ord(srInvalidFile);
   Exit;
 end;
 fRealContentProvider := fContentProvider.GetProperContentProvider(AURL);
 
 if fRealContentProvider = nil then begin
   ShowError('Cannot handle this type of subcontent. "' + fURLPrefix + '" for url:'+LineEnding+AURL);
   Result := Ord(srInvalidFile);
   Exit;
 end;

 for I := 0 to PageControl.PageCount-1 do begin
   if fRealContentProvider.ClassName = TContentTab(PageControl.Pages[I]).ContentProvider.ClassName then begin
     fPage := TContentTab(PageControl.Pages[I]);
     if TContentTab(PageControl.Pages[I]).ContentProvider.LoadURL(AURL, AContext) then
     begin
       PageControl.ActivePage := PageControl.Pages[I];
       Result := Ord(srSuccess);
     end
     else
       Result := Ord(srInvalidFile);
     Exit;
   end;
 end;

 if fPage = nil then
 begin
   //no existing page that can handle this content, so create one
   fPage := TContentTab.Create(PageControl);
   fPage.ContentProvider := fRealContentProvider.Create(fPage, ImageList1);
   fPAge.ContentProvider.OnTitleChange:=@ContentTitleChange;
   fPage.Parent := PageControl;
   SetKeyUp(fPage);
   fPage.ContentProvider.LoadPreferences(fConfig);
 end;

 
 if fPage.ContentProvider.LoadURL(AURL, AContext) then
 begin
   PageControl.ActivePage := fPage;
   RefreshState;
   Result := Ord(srSuccess);
 end
 else
   Result := Ord(srInvalidFile);

 if not fHide then
   ShowOnTop;
end;


procedure THelpForm.LateOpenURL ( Url: PStringItem ) ;
begin
  if OpenURL(URL^.FString, fContext) = ord(srSuccess) then
    AddRecentFile(URL^.FString);
  //we reset the context because at this point the file has been loaded and the
  //context shown
  fContext := -1;

  Dispose(Url);
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
    // todo: even though this code perhaps will hide the content window,
    // starting laz+pressing f1 will show content while loading all chms
    if Assigned(ActivePage) then
      with TChmContentProvider(ActivePage.ContentProvider) do
      begin
        ActivePage.Visible:=false;
        Visible:=false;
        //todo: are these necessary
        TabsControl.Visible := false;
        Splitter.Visible := false;
      end;
  end
  else
    en := Assigned(ActivePage);

  BackBttn.Enabled := en;
  ForwardBttn.Enabled := en;
  HomeBttn.Enabled := en;
  FileMenuCloseItem.Enabled := en;
  ViewMenuContents.Enabled := en;

  if en and not (csDestroying in ActivePage.ComponentState) then
    Caption := 'LHelp - ' + ActivePage.fContentProvider.Title
  else
    Caption := 'LHelp';
end;

procedure THelpForm.ShowError(AError: String);
begin
  ShowMessage(AError);
end;

procedure THelpForm.SetKeyUp(AControl: TControl);
var
  WCont: TWinControl absolute AControl;
  i: Integer;
begin
  if (AControl = nil) or not (AControl.InheritsFrom(TWinControl)) then
    Exit;
  for i := 0 to WCont.ControlCount-1 do
    SetKeyUp(WCont.Controls[i]);
  WCont.OnKeyUp:=@FormKeyUp;
end;

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

