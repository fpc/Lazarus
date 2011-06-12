{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Installs a HTML control in the IDE using TIpHtmlPanel.
}
unit IPIDEHTMLControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Graphics, Controls, Dialogs, ExtCtrls,
  IpMsg, Ipfilebroker, IpHtml, IDEHelpIntf, LazHelpIntf, LazIDEIntf, TextTools;

type
  TLazIPHtmlControl = class;

  { TLazIpHtmlDataProvider }

  TLazIpHtmlDataProvider = class(TIpHtmlDataProvider)
  private
    FControl: TLazIPHtmlControl;
  protected
    function DoGetStream(const URL: string): TStream; override;
  public
    property Control: TLazIPHtmlControl read FControl;
  end;

  { TLazIPHtmlControl }

  TLazIPHtmlControl = class(TCustomPanel,TIDEHTMLControlIntf)
    function DataProviderCanHandle(Sender: TObject; const URL: string): Boolean;
    procedure DataProviderCheckURL(Sender: TObject; const URL: string;
      var Available: Boolean; var ContentType: string);
    procedure DataProviderGetHtml(Sender: TObject; const URL: string;
      const {%H-}aPostData: TIpFormDataEntity; var Stream: TStream);
    procedure DataProviderGetImage(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
    procedure DataProviderLeave(Sender: TIpHtml);
    procedure DataProviderReportReference(Sender: TObject; const URL: string);
    procedure IPHTMLPanelHotClick(Sender: TObject);
  private
    FIDEProvider: TAbstractIDEHTMLProvider;
    FIPHTMLPanel: TIpHtmlPanel;
    FURL: string;
    procedure SetIDEProvider(const AValue: TAbstractIDEHTMLProvider);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetURL: string;
    procedure SetURL(const AValue: string);
    property IDEProvider: TAbstractIDEHTMLProvider read FIDEProvider write SetIDEProvider;
    procedure SetHTMLContent(Stream: TStream; const NewURL: string);
    procedure GetPreferredControlSize(out AWidth, AHeight: integer);
    property IPHTMLPanel: TIpHtmlPanel read FIPHTMLPanel;
  end;

  { TLazIPHTMLManager }

  TLazIPHTMLManager = class
  private
    fWaitingForAsync: boolean;
    procedure OpenNextURL(Data: PtrInt); // called via Application.QueueAsyncCall
  public
    NextURL: string;
    procedure AsyncOpenURL(URL: string);
    destructor Destroy; override;
  end;

var
  LazIPHTMLManager: TLazIPHTMLManager = nil;

function IPCreateLazIDEHTMLControl(Owner: TComponent;
  var Provider: TAbstractIDEHTMLProvider): TControl;

procedure Register;

implementation

procedure Register;
begin
  CreateIDEHTMLControl:=@IPCreateLazIDEHTMLControl;
end;

function IPCreateLazIDEHTMLControl(Owner: TComponent;
  var Provider: TAbstractIDEHTMLProvider): TControl;
var
  HTMLControl: TLazIPHtmlControl;
begin
  //debugln(['IPCreateLazIDEHTMLControl ']);
  HTMLControl:=TLazIPHtmlControl.Create(Owner);
  Result:=HTMLControl;
  if Provider=nil then
    Provider:=CreateIDEHTMLProvider(HTMLControl);
  //debugln(['IPCreateLazIDEHTMLControl Provider=',DbgSName(Provider)]);
  HTMLControl.IDEProvider:=Provider;
end;

{ TLazIPHTMLManager }

procedure TLazIPHTMLManager.AsyncOpenURL(URL: string);
begin
  NextURL:=URL;
  if not fWaitingForAsync then begin
    Application.QueueAsyncCall(@OpenNextURL,0);
    fWaitingForAsync:=true;
  end;
end;

procedure TLazIPHTMLManager.OpenNextURL(Data: PtrInt);
var
  URLScheme: string;
  URLPath: string;
  URLParams: string;
  AFilename: String;
  p: TPoint;
begin
  SplitURL(NextURL,URLScheme,URLPath,URLParams);
  if URLScheme='source' then begin
    p:=Point(1,1);
    if REMatches(URLPath,'(.*)\((.*),(.*)\)') then begin
      AFilename:=REVar(1);
      p.Y:=StrToIntDef(REVar(2),p.x);
      p.X:=StrToIntDef(REVar(3),p.y);
    end else begin
      AFilename:=URLPath;
    end;
    AFilename:=SetDirSeparators(AFilename);
    LazarusIDE.DoOpenFileAndJumpToPos(AFilename,p,-1,-1,-1,[]);
  end;
end;

destructor TLazIPHTMLManager.Destroy;
begin
  if (Application<>nil) and fWaitingForAsync then
    Application.RemoveAsyncCalls(Self);
  inherited Destroy;
end;

{ TLazIpHtmlDataProvider }

function TLazIpHtmlDataProvider.DoGetStream(const URL: string): TStream;
begin
  debugln(['TLazIpHtmlDataProvider.DoGetStream ',URL,' ',DbgSName(Control.IDEProvider)]);
  Result:=Control.IDEProvider.GetStream(URL,false);
end;

{ TLazIPHtmlControl }

function TLazIPHtmlControl.DataProviderCanHandle(Sender: TObject;
  const URL: string): Boolean;
begin
  debugln(['TLazIPHtmlControl.DataProviderCanHandle URL=',URL]);
  Result:=false;
end;

procedure TLazIPHtmlControl.DataProviderCheckURL(Sender: TObject;
  const URL: string; var Available: Boolean; var ContentType: string);
begin
  debugln(['TLazIPHtmlControl.DataProviderCheckURL URL=',URL]);
  Available:=false;
  ContentType:='';
end;

procedure TLazIPHtmlControl.DataProviderGetHtml(Sender: TObject;
  const URL: string; const aPostData: TIpFormDataEntity; var Stream: TStream);
begin
  debugln(['TLazIPHtmlControl.DataProviderGetHtml URL=',URL]);
  Stream:=nil;
end;

procedure TLazIPHtmlControl.DataProviderGetImage(Sender: TIpHtmlNode;
  const URL: string; var Picture: TPicture);
var
  URLType: string;
  URLPath: string;
  URLParams: string;
  Filename: String;
  Ext: String;
  Stream: TStream;
  NewURL: String;
begin
  //DebugLn(['TIPLazHtmlControl.HTMLGetImageX URL=',URL]);
  if IDEProvider=nil then exit;
  NewURL:=IDEProvider.MakeURLAbsolute(IDEProvider.BaseURL,URL);
  //DebugLn(['TIPLazHtmlControl.HTMLGetImageX NewURL=',NewURL,' Provider.BaseURL=',IDEProvider.BaseURL,' URL=',URL]);

  Picture:=nil;
  Stream:=nil;
  try
    try
      SplitURL(NewURL,URLType,URLPath,URLParams);
      if URLPath='' then
        URLPath:=NewURL;
      Filename:=URLPathToFilename(URLPath);
      Ext:=ExtractFileExt(Filename);
      //DebugLn(['TIPLazHtmlControl.HTMLGetImageX URLPath=',URLPath,' Filename=',Filename,' Ext=',Ext]);
      Picture:=TPicture.Create;
      // quick check if file format is supported (raises an exception)
      Picture.FindGraphicClassWithFileExt(Ext);
      // get stream
      Stream:=IDEProvider.GetStream(NewURL,true);
      // load picture
      Picture.LoadFromStreamWithFileExt(Stream,Ext);
    finally
      if Stream<>nil then
        IDEProvider.ReleaseStream(NewURL);
    end;
  except
    on E: Exception do begin
      FreeAndNil(Picture);
      DebugLn(['TIPLazHtmlControl.HTMLGetImageX ERROR: ',E.Message]);
    end;
  end;
end;

procedure TLazIPHtmlControl.DataProviderLeave(Sender: TIpHtml);
begin
  //debugln(['TLazIPHtmlControl.DataProviderLeave ']);
end;

procedure TLazIPHtmlControl.DataProviderReportReference(Sender: TObject;
  const URL: string);
begin
  debugln(['TLazIPHtmlControl.DataProviderReportReference URL=',URL]);
end;

procedure TLazIPHtmlControl.IPHTMLPanelHotClick(Sender: TObject);
var
  HotNode: TIpHtmlNode;
  HRef: String;
  Target: String;
  URLScheme: string;
  URLPath: string;
  URLParams: string;
begin
  HotNode:=FIPHTMLPanel.HotNode;
  if HotNode is TIpHtmlNodeA then begin
    HRef := TIpHtmlNodeA(HotNode).HRef;
    Target := TIpHtmlNodeA(HotNode).Target;
  end else begin
    HRef := TIpHtmlNodeAREA(HotNode).HRef;
    Target := TIpHtmlNodeAREA(HotNode).Target;
  end;
  debugln(['TLazIPHtmlControl.IPHTMLPanelHotClick HRef="',HRef,'" Target="',Target,'"']);
  SplitURL(HRef,URLScheme,URLPath,URLParams);
  if URLScheme='source' then begin
    // open the source in the IDE
    // This will close the hint, so the open must be done outside the current event
    if LazIPHTMLManager=nil then
      LazIPHTMLManager:=TLazIPHTMLManager.Create;
    LazIPHTMLManager.AsyncOpenURL(HRef);
  end;
end;

procedure TLazIPHtmlControl.SetIDEProvider(
  const AValue: TAbstractIDEHTMLProvider);
begin
  if FIDEProvider=AValue then exit;
  //debugln(['TLazIPHtmlControl.SetIDEProvider Old=',DbgSName(FIDEProvider),' New=',DbgSName(FIDEProvider)]);
  if FIDEProvider<>nil then begin
    IDEProvider.ControlIntf:=nil;
  end;
  FIDEProvider:=AValue;
  if FIDEProvider<>nil then begin
    FreeNotification(FIDEProvider);
    IDEProvider.ControlIntf:=Self;
  end;
end;

procedure TLazIPHtmlControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    if IDEProvider=AComponent then begin
      if IDEProvider.ControlIntf=TIDEHTMLControlIntf(Self) then
        IDEProvider.ControlIntf:=nil;
      IDEProvider:=nil;
    end;
  end;
end;

constructor TLazIPHtmlControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIPHTMLPanel:=TIpHtmlPanel.Create(Self);
  with FIPHTMLPanel do begin
    Name:='TLazIPHtmlControl_IPHTMLPanel';
    Align:=alClient;
    DefaultFontSize:=8;
    MarginHeight:=2;
    MarginWidth:=2;
    Parent:=Self;
    OnHotClick:=@IPHTMLPanelHotClick;
  end;
  FIPHTMLPanel.DataProvider:=TLazIpHtmlDataProvider.Create(FIPHTMLPanel);
  with TLazIpHtmlDataProvider(FIPHTMLPanel.DataProvider) do begin
    FControl:=Self;
    Name:='TLazIPHtmlControlDataProvider';
    OnCanHandle:=@DataProviderCanHandle;
    OnGetHtml:=@DataProviderGetHtml;
    OnGetImage:=@DataProviderGetImage;
    OnLeave:=@DataProviderLeave;
    OnCheckURL:=@DataProviderCheckURL;
    OnReportReference:=@DataProviderReportReference;
  end;
  Caption:='';
  BevelInner:=bvLowered;
end;

destructor TLazIPHtmlControl.Destroy;
begin
  //debugln(['TLazIPHtmlControl.Destroy ',DbgSName(Self),' ',dbgs(Pointer(Self))]);
  FreeAndNil(FIDEProvider);
  inherited Destroy;
end;

function TLazIPHtmlControl.GetURL: string;
begin
  Result:=FURL;
end;

procedure TLazIPHtmlControl.SetURL(const AValue: string);
var
  Stream: TStream;
  NewHTML: TIpHtml;
  NewURL: String;
  ok: Boolean;
begin
  if IDEProvider=nil then raise Exception.Create('TIPLazHtmlControl.SetURL missing Provider');
  if FURL=AValue then exit;
  NewURL:=IDEProvider.MakeURLAbsolute(IDEProvider.BaseURL,AValue);
  if FURL=NewURL then exit;
  FURL:=NewURL;
  try
    Stream:=IDEProvider.GetStream(FURL,true);
    ok:=false;
    NewHTML:=nil;
    try
      NewHTML:=TIpHtml.Create; // Beware: Will be freed automatically TIpHtmlPanel
      NewHTML.LoadFromStream(Stream);
      ok:=true;
    finally
      if not ok then FreeAndNil(NewHTML);
      IDEProvider.ReleaseStream(FURL);
    end;
    if NewHTML<>nil then
      FIPHTMLPanel.SetHtml(NewHTML);
  except
    on E: Exception do begin
      MessageDlg('Unable to open HTML file',
        'URL: '+FURL+#13
        +'Error: '+E.Message,mtError,[mbCancel],0);
    end;
  end;
end;

procedure TLazIPHtmlControl.SetHTMLContent(Stream: TStream; const NewURL: string
  );
var
  NewHTML: TIpHtml;
  ok: Boolean;
begin
  FURL:=NewURL;
  try
    NewHTML:=nil;
    ok:=false;
    try
      NewHTML:=TIpHtml.Create; // Beware: Will be freed automatically TIpHtmlPanel
      NewHTML.LoadFromStream(Stream);
      ok:=true;
    finally
      if not ok then FreeAndNil(NewHTML);
    end;
    if NewHTML<>nil then
      FIPHTMLPanel.SetHtml(NewHTML);
  except
    on E: Exception do begin
      MessageDlg('Unable to load HTML stream',
        'URL: '+FURL+#13
        +'Error: '+E.Message,mtError,[mbCancel],0);
    end;
  end;
end;

procedure TLazIPHtmlControl.GetPreferredControlSize(out AWidth, AHeight: integer);
begin
  AWidth:=0;
  AHeight:=0;
  inherited GetPreferredSize(AWidth, AHeight);
  //debugln(['TLazIPHtmlControl.GetPreferredControlSize Width=',AWidth,' Height=',AHeight]);
end;

finalization
  FreeAndNil(LazIPHTMLManager);

end.

