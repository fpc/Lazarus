{ Copyright (C) <2005> <Andrew Haines> lhelpcore.pas

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SimpleIPC, XMLCfg,
  FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, LCLProc, StdCtrls, IpHtml, ComCtrls, ExtCtrls, Menus, LCLType,
  BaseContentProvider, FileContentProvider, ChmContentProvider{$IFDEF USE_LNET}, HTTPContentProvider{$ENDIF};

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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure ForwardToolBtnClick(Sender: TObject);
    procedure HomeToolBtnClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure PageControlEnter(Sender: TObject);
    procedure ViewMenuContentsClick(Sender: TObject);

  private
    { private declarations }
    fServerName: String;
    fInputIPC: TSimpleIPCServer;
    fOutputIPC: TSimpleIPCClient;
    fServerTimer: TTimer;
    fContext: LongInt; // used once when we are started on the command line with --context
    fConfig: TXMLConfig;
    FHasShowed: Boolean;
    procedure LoadPreferences(AIPCName: String);
    procedure SavePreferences(AIPCName: String);
    procedure AddRecentFile(AFileName: String);
    procedure ContentTitleChange(sender: TObject);
    procedure OpenRecentItemClick(Sender: TObject);
    procedure SendResponse(Response: DWord);
    procedure ServerMessage(Sender: TObject);
    procedure ReadCommandLineOptions;
    procedure StartServer(ServerName: String);
    procedure StopServer;
    function  OpenURL(const AURL: String; AContext: THelpContext=-1): DWord;
    procedure LateOpenURL(Url: PStringItem);
    function ActivePage: TContentTab;
    procedure RefreshState;
    procedure ShowError(AError: String);
    procedure SetKeyUp(AControl: TControl);
  public
    { public declarations }
  end;
  

var
  HelpForm: THelpForm;
  IPCServer: TSimpleIPCServer;
const INVALID_FILE_TYPE = 1;

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
begin
  Application.MessageBox('LHelp (CHM file viewer)'#13 +
    'Ver. 2009.06.08'#13 +
    'Copyright (C) Andrew Haines',
    'About', 0);
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

  for i := 0 to Protocall.Count-1 do
  begin
    if i < 1 then
      URLSAllowed := URLSAllowed + Protocall[i]
    else
      URLSAllowed := URLSAllowed + ', ' +Protocall[i]
  end;
  Protocall.Free;

  URLSAllowed := Trim(URLSALLowed);


  if InputQuery('Please Enter a URL', 'Supported URL type(s): (' +URLSAllowed+ ')', fRes) then
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
  StopServer;
  SavePreferences(fServerName);
end;

procedure THelpForm.FormCreate(Sender: TObject);
begin
  fContext := -1;
  ReadCommandLineOptions;
  if fServerName <> '' then begin
    StartServer(fServerName);
  end;
  RefreshState;
  SetKeyUp(Self);
end;

procedure THelpForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure THelpForm.FormShow(Sender: TObject);
begin
  if FHasShowed then
    Exit;
  FHasShowed := True;
  LoadPreferences(fServerName);
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
var
  AWidth: Integer;
begin
  //TabsControl property in TChmContentProvider
  if Assigned(ActivePage) then
    with TChmContentProvider(ActivePage.ContentProvider) do
    begin
      TabsControl.Visible := not TabsControl.Visible;
      Splitter.Visible := TabsControl.Visible;
      Splitter.Left := TabsControl.Left + 4; //for splitter to move righter
      ViewMenuContents.Checked := TabsControl.Visible;
      AWidth := TabsControl.Width + Splitter.Width;
    end;
end;

procedure THelpForm.LoadPreferences(AIPCName: String);
var
  PrefFile: String;
  RecentCount: Integer;
  i: Integer;
begin
  PrefFile := GetAppConfigDirUTF8(False);
  ForceDirectoriesUTF8(PrefFile);
  PrefFile:=Format('%slhelp-%s.conf',[IncludeTrailingPathDelimiter(PrefFile), AIPCName]);

  fConfig := TXMLConfig.Create(Self);
  fConfig.Filename:=PrefFile;

  Left   := fConfig.GetValue('Position/Left/Value',   Left);
  Top    := fConfig.GetValue('Position/Top/Value',    Top);
  Width  := fConfig.GetValue('Position/Width/Value',  Width);
  Height := fConfig.GetValue('Position/Height/Value', Height);

  OpenDialog1.FileName := fConfig.GetValue('LastFileOpen/Value', OpenDialog1.FileName);

  RecentCount:= fConfig.GetValue('Recent/ItemCount/Value', 0);

  for i := RecentCount-1 downto 0 do // downto since oldest are knocked off the list
    AddRecentFile(fConfig.GetValue('Recent/Item/'+IntToStr(i)+'/Value',''));
end;

procedure THelpForm.SavePreferences(AIPCName: String);
var
  i: Integer;
begin
  if not (WindowState = wsMaximized) then
  begin
    fConfig.SetValue('Position/Left/Value',   Left);
    fConfig.SetValue('Position/Top/Value',    Top);
    fConfig.SetValue('Position/Width/Value',  Width);
    fConfig.SetValue('Position/Height/Value', Height);
  end;

  fConfig.SetValue('LastFileOpen/Value', OpenDialog1.FileName);

  fConfig.SetValue('Recent/ItemCount/Value', FileMenuOpenRecentItem.Count);
  for i := 0 to FileMenuOpenRecentItem.Count-1 do // downto since oldest are knocked off the list
    fConfig.SetValue('Recent/Item/'+IntToStr(i)+'/Value', TRecentMenuItem(FileMenuOpenRecentItem.Items[I]).URL);


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
  fOutputIPC := TSimpleIPCClient.Create(nil);
  fOutputIPC.ServerID := fServerName+'client';
  fOutputIPC.Active := True;

  Stream := TMemoryStream.Create;
  Stream.WriteDWord(Response);
  fOutputIPC.SendMessage(mtUnknown, Stream);

  if fOutputIPC.Active then
    fOutputIPC.Active := False;
  FreeAndNil(fOutputIPC);
end;



procedure THelpForm.ServerMessage(Sender: TObject);
var
  UrlReq: TUrlRequest;
  FileReq:TFileRequest;
  ConReq: TContextRequest;
  Stream: TStream;
  Res: LongWord;
  Url: String;
begin
  if fInputIPC.PeekMessage(5, True) then begin
    Stream := fInputIPC.MsgData;
    Stream.Position := 0;
    Stream.Read(FileReq, SizeOf(FileReq));
    case FileReq.RequestType of
      rtFile    : begin
                    Url := 'file://'+FileReq.FileName;
                    Res := OpenURL(URL);
                  end;
      rtUrl     : begin
                    Stream.Position := 0;
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
                    Stream.Read(ConReq, SizeOf(ConReq));
                    Url := 'file://'+FileReq.FileName;
                    Res := OpenURL(Url, ConReq.HelpContext);
                  end;
    end;
    if Res = Ord(srSuccess) then
      AddRecentFile(Url);
    SendResponse(Res);
    Self.SendToBack;
    Self.BringToFront;
    Self.ShowOnTop;
  end;
end;



procedure THelpForm.ReadCommandLineOptions;
var
  X: Integer;
  IsHandled: array[0..50] of boolean;
  URL: String;
  StrItem: PStringItem;
begin
  FillChar(IsHandled, 51, 0);
  for  X := 1 to ParamCount do begin
    if LowerCase(ParamStrUTF8(X)) = '--ipcname' then begin
      IsHandled[X] := True;
      if X < ParamCount then begin
        fServerName := ParamStrUTF8(X+1);
        IsHandled[X+1] := True;
      end;
    end;
    if LowerCase(ParamStrUTF8(X)) = '--context' then begin
      IsHandled[X] := True;
      if (X < ParamCount) then
        if TryStrToInt(ParamStrUTF8(X+1), fContext) then
          IsHandled[X+1] := True;
    end;
  end;
  // Loop through a second time for the url
  for X := 1 to ParamCount do
    if not IsHandled[X] then begin
      //DoOpenChm(ParamStrUTF8(X));
      if Pos('://', ParamStrUTF8(X)) = 0 then
        URL := 'file://'+ParamStrUTF8(X)
      else
        URL := ParamStrUTF8(X);
      StrItem := New(PStringItem);
      StrItem^.FString := URL;
      Application.QueueAsyncCall(TDataEvent(@LateOpenURL), PtrUInt(StrItem));
      Break;
    end;
  //we reset the context because at this point the file has been loaded and the
  //context shown
  fContext := -1;
    

end;

procedure THelpForm.StartServer(ServerName: String);
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


end;

procedure THelpForm.StopServer;
begin
   if fInputIPC = nil then
     exit;

   if fInputIPC.Active then
     fInputIPC.Active := False;

   FreeAndNil(fInputIPC);
   IPCServer := nil;
   FreeAndNil(fServerTimer);
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
   //no page was found already to handle this content so create one
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

 ShowOnTop;
end;


procedure THelpForm.LateOpenURL ( Url: PStringItem ) ;
begin
  if OpenURL(URL^.FString, fContext) = ord(srSuccess) then
    AddRecentFile(URL^.FString);

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
    try
    FreeAndNil(IPCServer);
    except
    end;

end.

