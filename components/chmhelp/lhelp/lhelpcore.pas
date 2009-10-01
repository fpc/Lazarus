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
  Classes, SysUtils, SimpleIPC,
  FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, LCLProc, StdCtrls, IpHtml, ComCtrls, ExtCtrls, Menus,
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
  public
    { public declarations }
  end;
  

var
  HelpForm: THelpForm;
const INVALID_FILE_TYPE = 1;

implementation
uses LHelpControl;

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
    OpenURL('file://'+OpenDialog1.FileName);
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
    OpenURL(fRes);
    RefreshState;
  end;
end;

procedure THelpForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FileMenuCloseItemClick(Sender);
  StopServer;
end;

procedure THelpForm.FormCreate(Sender: TObject);
begin
  fContext := -1;
  ReadCommandLineOptions;
  if fServerName <> '' then begin
    StartServer(fServerName);
  end;
  RefreshState;
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
begin
  if fInputIPC.PeekMessage(5, True) then begin
    Stream := fInputIPC.MsgData;
    Stream.Position := 0;
    Stream.Read(FileReq, SizeOf(FileReq));
    case FileReq.RequestType of
      rtFile    : begin
                    Res := OpenURL('file://'+FileReq.FileName);
                  end;
      rtUrl     : begin
                    Stream.Position := 0;
                    Stream.Read(UrlReq, SizeOf(UrlReq));
                    if UrlReq.FileRequest.FileName <> '' then
                      Res := OpenUrl('file://'+UrlReq.FileRequest.FileName+'://'+UrlReq.Url)
                    else
                      Res := OpenURL(UrlReq.Url);
                  end;
      rtContext : begin
                    Stream.Position := 0;
                    Stream.Read(ConReq, SizeOf(ConReq));
                    Res := OpenURL('file://'+FileReq.FileName, ConReq.HelpContext);
                  end;
    end;
    SendResponse(Res);
    Self.SendToBack;
    Self.BringToFront;
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
 fNewPage: TContentTab;
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
 
 //no page was found already to handle this content so create one
 fNewPage := TContentTab.Create(PageControl);
 fNewPage.ContentProvider := fRealContentProvider.Create(fNewPage, ImageList1);
 fNewPage.Parent := PageControl;

 ShowOnTop;
 
 if fNewPage.ContentProvider.LoadURL(AURL, AContext) then
 begin
   PageControl.ActivePage := fNewPage;
   RefreshState;
   Result := Ord(srSuccess);
 end
 else
   Result := Ord(srInvalidFile);
end;

procedure THelpForm.LateOpenURL ( Url: PStringItem ) ;
begin
  OpenURL(URL^.FString, fContext);
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

initialization
  {$I lhelpcore.lrs}

end.

