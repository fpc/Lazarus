unit HtmlHelp2Unit2; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, LCLProc,
  IpHtml, Buttons, helpintfs, lazhelpintf, ComCtrls, ipfilebroker, iputils;

type

  { TForm2 }
  
  TForm2 = class(TForm)
    IHP: TIpHtmlPanel;
    DataProvider: TIpFileDataProvider;
    Panel1: TPanel;
    IndexButton: TSpeedButton;
    BackButton: TSpeedButton;
    ForwardButton: TSpeedButton;
    StatusBar1: TStatusBar;
    procedure BackButtonClick(Sender: TObject);
    procedure ForwardButtonClick(Sender: TObject);
    procedure IHPDocumentOpen(Sender: TObject);
    procedure IHPHotChange(Sender: TObject);
    procedure IndexButtonClick(Sender: TObject);
  private
  public
    { public declarations }
    procedure showURL(URL : String);
  end; 

var
  Form2: TForm2; 

procedure RegisterHelpViewer;

implementation

type
THTMLHelpViewer = class(THelpViewer)
  private
  public
    constructor Create(TheOwner: TComponent); override;
    function ShowNode(Node: THelpNode; var ErrMsg: string): TShowHelpResult; override;
    //procedure Assign(Source: TPersistent); override;
    //procedure Load(Storage: TConfigStorage); override;
    //procedure Save(Storage: TConfigStorage); override;
    //function GetLocalizedName: string; override;
  published
    property AutoRegister;
  end;

{ THTMLHelpViewer }

constructor THTMLHelpViewer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  AddSupportedMimeType('text/html');
end;

function THTMLHelpViewer.ShowNode(Node: THelpNode; var ErrMsg: string): TShowHelpResult;
begin
  DebugLn (Format('ShowNode: URL:"%s" ID:"%s" Context:"%d"',[Node.URL,Node.ID,Node.Context]));
  Form2.ShowURL(Node.URL);
  result := shrSuccess;
end;

var Help_Viewer : THTMLHelpViewer = nil;

procedure RegisterHelpViewer;
begin
  if Help_Viewer = nil then
  begin
    Help_Viewer := THTMLHelpViewer.Create(nil);
    Help_Viewer.RegisterSelf;
  end;
end;

{ TForm2 }

procedure TForm2.IndexButtonClick(Sender: TObject);
begin
  ShowURL('html/index.html');
end;

// Show URL of a link in Status Bar
procedure TForm2.IHPHotChange(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := IHP.HotURL;
end;

procedure TForm2.BackButtonClick(Sender: TObject);
begin
  IHP.GoBack;
end;


procedure TForm2.ForwardButtonClick(Sender: TObject);
begin
  IHP.GoForward;
end;

procedure TForm2.IHPDocumentOpen(Sender: TObject);
begin
  BackButton.Enabled := IHP.canGoBack;
  ForwardButton.Enabled := IHP.canGoForward;
end;


procedure TForm2.showURL(URL : String);
begin
  Show;
  //writeln ('ShowUrl ',ansiuppercase(URL));
  URL := expandLocalHtmlFileName (URL);
  //writeln ('showURL: "',URL,'"');
  IHP.OpenURL(URL);
end;


initialization
  {$I htmlhelp2unit2.lrs}

end.

