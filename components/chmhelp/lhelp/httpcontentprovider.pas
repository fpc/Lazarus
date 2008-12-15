unit HTTPContentProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseContentProvider, LNetHTTPDataProvider, IpHtml, ComCtrls,
  Menus, Controls;
  
type

  { THTTPContentProvider }

  THTTPContentProvider = class(TBaseContentProvider)
  private
    fHomeUrl: String;
    fPopup: TPopupMenu;
    fStatusBar: TStatusBar;
    fHtml: TIpHtmlPanel;
    fHttpDataProvider: TIpHTTPDataProvider;
    procedure IpHtmlPanelHotChange(Sender: TObject);
    procedure PopupCopyClick(Sender: TObject);
    procedure SetTitle(ATitle: String);
  public
    function CanGoBack: Boolean; override;
    function CanGoForward: Boolean; override;
    function GetHistory: TStrings; override;
    function LoadURL(const AURL: String; const AContext: THelpContext=-1): Boolean; override;
    procedure GoHome; override;
    procedure GoBack; override;
    procedure GoForward; override;
    class function GetProperContentProvider(const AURL: String): TBaseContentProviderClass; override;
    constructor Create(AParent: TWinControl; AImageList: TImageList); override;

  end;

implementation



{ THTTPContentProvider }

procedure THTTPContentProvider.IpHtmlPanelHotChange(Sender: TObject);
begin
  fStatusBar.SimpleText := fHtml.HotURL;
end;

procedure THTTPContentProvider.PopupCopyClick(Sender: TObject);
begin
  fHtml.CopyToClipboard;
end;

procedure THTTPContentProvider.SetTitle(ATitle: String);
begin
  if Parent = nil then exit;
  TTabSheet(Parent).Caption := ATitle;
end;

function THTTPContentProvider.CanGoBack: Boolean;
begin
  //Result:=inherited CanGoBack;
  fHtml.canGoBack;
end;

function THTTPContentProvider.CanGoForward: Boolean;
begin
  //Result:=inherited CanGoForward;
  fHtml.canGoForward;
end;

function THTTPContentProvider.GetHistory: TStrings;
begin
  //Result:=inherited GetHistory;
end;

function THTTPContentProvider.LoadURL(const AURL: String;
  const AContext: THelpContext): Boolean;
begin
  Result:=True;
  SetTitle('Loading : ' + AURL );
  //WriteLn('Loading URL:', AURL);
  fHtml.OpenURL(AURL);
  SetTitle(AURL);
  if fHomeURL = '' then fHomeURL := AURL;
  
end;

procedure THTTPContentProvider.GoHome;
begin
  LoadURL(fHomeURL);
end;

procedure THTTPContentProvider.GoBack;
begin
  fHtml.GoBack;
end;

procedure THTTPContentProvider.GoForward;
begin
  fHtml.GoForward;
end;

class function THTTPContentProvider.GetProperContentProvider(const AURL: String
  ): TBaseContentProviderClass;
begin
  Result := THTTPContentProvider;
end;

constructor THTTPContentProvider.Create(AParent: TWinControl; AImageList: TImageList);
begin
  inherited Create(AParent, AImageList);
  fPopUp := TPopupMenu.Create(fHtml);
  fPopUp.Items.Add(TMenuItem.Create(fPopup));
  with fPopUp.Items.Items[0] do begin
    Caption := 'Copy';
    OnClick := @PopupCopyClick;
  end;

  fHttpDataProvider := TIpHTTPDataProvider.Create(AParent);
  fHtml := TIpHtmlPanel.Create(Parent);
  with fHtml do begin
    DataProvider := fHttpDataProvider;
    //OnDocumentOpen := @IpHtmlPanelDocumentOpen;
    OnHotChange := @IpHtmlPanelHotChange;
    PopupMenu := fPopUp;
    Parent := AParent;
    Align := alClient;
    Visible := True;
  end;
  
  fStatusBar := TStatusBar.Create(AParent);
  with fStatusBar do begin
    Parent := AParent;
    Align := alBottom;
    SimplePanel := True;
  end;
end;

initialization
  RegisterContentProvider('http://', THTTPContentProvider);
end.

