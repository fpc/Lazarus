unit frmpas2jsbrowserprojectoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  Spin, strpas2jsdesign;

type

  { TWebBrowserProjectOptionsForm }

  TWebBrowserProjectOptionsForm = class(TForm)
    BPHelpOptions: TButtonPanel;
    CBCreateHTML: TCheckBox;
    CBUseBrowserApp: TCheckBox;
    CBUseWASI: TCheckBox;
    CBUseBrowserConsole: TCheckBox;
    CBUseHTTPServer: TCheckBox;
    CBServerURL: TComboBox;
    CBMaintainPage: TCheckBox;
    CBRunOnReady: TCheckBox;
    cbShowUncaughtExceptions: TCheckBox;
    edtWasmProgram: TEdit;
    RBUseURL: TRadioButton;
    RBStartServerAt: TRadioButton;
    SEPort: TSpinEdit;
    procedure CBCreateHTMLChange(Sender: TObject);
    procedure CBUseBrowserAppChange(Sender: TObject);
    procedure CBUseHTTPServerChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure CheckWasi;
    function GetB(AIndex: Integer): Boolean;
    function GetServerPort: Word;
    function GetURL: String;
    function GetWasmProgramURL: String;
    procedure SetB(AIndex: Integer; AValue: Boolean);
    procedure SetServerPort(AValue: Word);
    procedure SetURL(AValue: String);
    procedure SetWasmProgramURL(AValue: String);
  public
    property CreateHTML : Boolean Index 0 read GetB Write SetB;
    property MaintainHTML : Boolean Index 1 read GetB Write SetB;
    property UseBrowserApp : Boolean Index 2 read GetB Write SetB;
    property UseBrowserConsole : Boolean Index 3 read GetB Write SetB;
    property StartHTTPServer : Boolean Index 4 read GetB Write SetB;
    property UseURL : Boolean Index 5 read GetB Write SetB;
    property UseRunOnReady : Boolean Index 6 read GetB Write SetB;
    property ShowUncaughtExceptions : Boolean Index 7 read GetB Write SetB;
    property UseWASI : Boolean Index 8 read GetB Write SetB;
    Property ServerPort : Word Read GetServerPort Write SetServerPort;
    Property URL : String Read GetURL Write SetURL;
    Property WasmProgramURL : String Read GetWasmProgramURL Write SetWasmProgramURL;
  end;

var
  WebBrowserProjectOptionsForm: TWebBrowserProjectOptionsForm;

implementation

{$R *.lfm}

{ TWebBrowserProjectOptionsForm }

procedure TWebBrowserProjectOptionsForm.CBCreateHTMLChange(Sender: TObject);

  Procedure DOCB(CB : TCheckbox);

  begin
    CB.Enabled:=CBCreateHTML.Checked;
    if not CB.Enabled then
      CB.Checked:=False;
  end;

begin
  DoCB(CBRunOnReady);
  DoCB(CBMaintainPage);
end;

procedure TWebBrowserProjectOptionsForm.CBUseBrowserAppChange(Sender: TObject);
begin
  CheckWASI;
end;

procedure TWebBrowserProjectOptionsForm.CheckWasi;

begin
  CBUseWASI.Enabled:=UseBrowserApp;
  edtWasmProgram.Enabled:=UseBrowserApp;
end;

procedure TWebBrowserProjectOptionsForm.CBUseHTTPServerChange(Sender: TObject);

  procedure disen(C : TControl);

  begin
    C.Enabled:=CBUseHTTPServer.Checked;
    if C is TRadioButton then
      if not C.Enabled then
        TRadioButton(C).Checked:=False;
  end;

begin
  disen(RBStartServerAt);
  disen(RBUseURL);
  disen(SEPort);
  disen(CBServerURL);
end;

procedure TWebBrowserProjectOptionsForm.FormCreate(Sender: TObject);
begin
  Caption:=pjsdPas2JSBrowserProjectOptions;
  CBCreateHTML.Caption:=pjsdCreateInitialHTMLPage;
  CBMaintainPage.Caption:=pjsdMaintainHTMLPage;
  CBRunOnReady.Caption:=pjsdRunRTLWhenAllPageResourcesAreFullyLoaded;
  cbShowUncaughtExceptions.Caption:=pjsdLetRTLShowUncaughtExceptions;
  CBUseBrowserApp.Caption:=pjsdUseBrowserApplicationObject;
  CBUseBrowserConsole.Caption:=pjsdUseBrowserConsoleUnitToDisplayWritelnOutput;
  CBUseHTTPServer.Caption:=pjsdProjectNeedsAHTTPServer;
  RBStartServerAt.Caption:=pjsdStartHTTPServerOnPort;
  RBUseURL.Caption:=pjsdUseThisURLToStartApplication;
  CBUseWASI.Caption:=pjsdUseWASIApplicationObject;
  edtWasmProgram.TextHint:=pjsWasiProgramFileTextHint;
  CBCreateHTMLChange(self);
  CBUseHTTPServerChange(Self);
end;

procedure TWebBrowserProjectOptionsForm.FormShow(Sender: TObject);
begin
  // Need to do this again, in case options were set before show
  CBCreateHTMLChange(self);
  CBUseHTTPServerChange(Self);
end;

function TWebBrowserProjectOptionsForm.GetB(AIndex: Integer): Boolean;
begin
  Case Aindex of
    0 : Result:=CBCreateHTML.Checked;
    1 : Result:=CBMaintainPage.Checked;
    2 : Result:=CBUseBrowserApp.Checked;
    3 : Result:=CBUseBrowserConsole.Checked;
    4 : Result:=RBStartServerAt.Checked;
    5 : Result:=RBUseURL.Checked;
    6 : Result:=CBRunOnReady.Checked;
    7 : Result:=cbShowUncaughtExceptions.Checked;
    8 : Result:=cbUseWASI.Checked;
  else
    Result:=False;
  end;
//  Writeln('Reporting ',AIndex,' : ',Result);
end;

function TWebBrowserProjectOptionsForm.GetServerPort: Word;
begin
  Result:=SEPort.Value;
end;

function TWebBrowserProjectOptionsForm.GetURL: String;
begin
  Result:=CBServerURL.Text;
end;

function TWebBrowserProjectOptionsForm.GetWasmProgramURL: String;
begin
  Result:=edtWasmProgram.Text;
end;

procedure TWebBrowserProjectOptionsForm.SetB(AIndex: Integer; AValue: Boolean);
begin
  Case Aindex of
    0 : CBCreateHTML.Checked:=AValue;
    1 : CBMaintainPage.Checked:=AValue;
    2 :
       begin
       CBUseBrowserApp.Checked:=AValue;
       CheckWASI;
       end;
    3 : CBUseBrowserConsole.Checked:=AValue;
    4 :
      begin
      RBStartServerAt.Checked:=AValue;
      if AValue then
        CBUseHTTPServer.Checked:=true
      end;
    5 :
      begin
      RBUseURL.Checked:=AValue;
      if AValue then
        CBUseHTTPServer.Checked:=true
      end;
    6 : CBRunOnReady.Checked:=Avalue;
    7 : cbShowUncaughtExceptions.Checked:=aValue;
    8 : cbUseWASI.Checked:=aValue;
  end;
end;

procedure TWebBrowserProjectOptionsForm.SetServerPort(AValue: Word);
begin
  SEPort.Value:=AValue;
end;

procedure TWebBrowserProjectOptionsForm.SetURL(AValue: String);
begin
  CBServerURL.Text:=AValue;
end;

procedure TWebBrowserProjectOptionsForm.SetWasmProgramURL(AValue: String);
begin
  edtWasmProgram.Text:=aValue;
end;

end.

