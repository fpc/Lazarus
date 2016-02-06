unit browserfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Iphttpbroker, IpHtml, Forms, Buttons, ExtCtrls, StdCtrls,
  ActnList;

type

  { TfrMain }

  TfrMain = class(TForm)
    acBack: TAction;
    acForward: TAction;
    acGo: TAction;
    alMain: TActionList;
    btGo: TSpeedButton;
    edUrl: TEdit;
    IpHtmlPanel1: TIpHtmlPanel;
    IpHttpDataProvider1: TIpHttpDataProvider;
    pnTop: TPanel;
    btBack: TSpeedButton;
    btForward: TSpeedButton;
    procedure acBackExecute(Sender: TObject);
    procedure acForwardExecute(Sender: TObject);
    procedure acGoExecute(Sender: TObject);
    procedure alMainUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure IpHtmlPanel1DocumentOpen(Sender: TObject);
  end;

var
  frMain: TfrMain;

implementation

{$R *.lfm}

{ TfrMain }

procedure TfrMain.IpHtmlPanel1DocumentOpen(Sender: TObject);
begin
  edUrl.Text := IpHtmlPanel1.CurURL;
end;

procedure TfrMain.alMainUpdate(AAction: TBasicAction; var Handled: Boolean);
begin
  acBack.Enabled := IpHtmlPanel1.CanGoBack;
  acForward.Enabled := IpHtmlPanel1.CanGoForward;
  acGo.Enabled := Trim(edUrl.Text) <> '';
end;

procedure TfrMain.acBackExecute(Sender: TObject);
begin
  IpHtmlPanel1.GoBack;
end;

procedure TfrMain.acForwardExecute(Sender: TObject);
begin
  IpHtmlPanel1.GoForward;
end;

procedure TfrMain.acGoExecute(Sender: TObject);
begin
  IpHtmlPanel1.OpenURL(edUrl.Text);
end;

end.

