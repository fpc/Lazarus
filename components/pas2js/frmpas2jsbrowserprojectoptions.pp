unit frmpas2jsbrowserprojectoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  Spin, strpas2jsdesign;

const
  WBBoolCreateHTML = 0;
  WBBoolMainHTML = 1;
  WBBoolRunOnReady = 2;
  WBBoolShowUncaughtExceptions = 3;
  WBBoolUseBrowserApp = 4;
  WBBoolUseWASI = 5;
  WBBoolUseBrowserConsole = 6;
  WBBoolUseModule = 7;
  WBBoolRunServerAtPort = 8;
  WBBoolRunBrowserWithURL = 9;
  WBBoolRunDefault = 10;
type

  { TWebBrowserProjectOptionsForm }

  TWebBrowserProjectOptionsForm = class(TForm)
    BPHelpOptions: TButtonPanel;
    CBCreateHTML: TCheckBox;
    CBServerURL: TComboBox;
    CBUseBrowserApp: TCheckBox;
    CBUseModule: TCheckBox;
    CBUseWASI: TCheckBox;
    CBUseBrowserConsole: TCheckBox;
    CBMaintainPage: TCheckBox;
    CBRunOnReady: TCheckBox;
    cbShowUncaughtExceptions: TCheckBox;
    edtWasmProgram: TEdit;
    RBRunDefault: TRadioButton;
    RBRunServerAt: TRadioButton;
    RBRunBrowserWithURL: TRadioButton;
    RunGroupBox: TGroupBox;
    SEPort: TSpinEdit;
    procedure CBCreateHTMLChange(Sender: TObject);
    procedure CBUseBrowserAppChange(Sender: TObject);
    procedure CBUseHTTPServerChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RBRunDefaultChange(Sender: TObject);
    procedure RBRunServerAtChange(Sender: TObject);
    procedure RBRunBrowserWithURLChange(Sender: TObject);
  private
    function GetB(AIndex: Integer): Boolean;
    function GetServerPort: Word;
    function GetURL: String;
    function GetWasmProgramURL: String;
    procedure SetB(AIndex: Integer; AValue: Boolean);
    procedure SetServerPort(AValue: Word);
    procedure SetURL(AValue: String);
    procedure SetWasmProgramURL(AValue: String);
    procedure UpdateHTMLControls;
    procedure UpdateBrowserAppControls;
    procedure UpdateRunControls;
  public
    procedure HideWASM; virtual;
    procedure HideModule; virtual;

    property CreateHTML : Boolean Index WBBoolCreateHTML read GetB Write SetB;
    property MaintainHTML : Boolean Index WBBoolMainHTML read GetB Write SetB;
    property UseRunOnReady : Boolean Index WBBoolRunOnReady read GetB Write SetB;
    property ShowUncaughtExceptions : Boolean Index WBBoolShowUncaughtExceptions read GetB Write SetB;

    property UseBrowserApp : Boolean Index WBBoolUseBrowserApp read GetB Write SetB;
    property UseWASI : Boolean Index WBBoolUseWASI read GetB Write SetB;
    property WasmProgramURL : String Read GetWasmProgramURL Write SetWasmProgramURL;

    property UseBrowserConsole : Boolean Index WBBoolUseBrowserConsole read GetB Write SetB;
    property UseModule : Boolean Index WBBoolUseModule read GetB Write SetB;

    property RunServerAtPort : Boolean Index WBBoolRunServerAtPort read GetB Write SetB;
    property ServerPort : Word Read GetServerPort Write SetServerPort;
    property RunBrowserWithURL : Boolean Index WBBoolRunBrowserWithURL read GetB Write SetB;
    property URL : String Read GetURL Write SetURL;
    property RunDefault : Boolean Index WBBoolRunDefault read GetB Write SetB;
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
  UpdateHTMLControls;
  DoCB(CBMaintainPage);
  DoCB(CBRunOnReady);
end;

procedure TWebBrowserProjectOptionsForm.CBUseBrowserAppChange(Sender: TObject);
begin
  UpdateBrowserAppControls;
end;

procedure TWebBrowserProjectOptionsForm.CBUseHTTPServerChange(Sender: TObject);
begin

end;

procedure TWebBrowserProjectOptionsForm.UpdateBrowserAppControls;

begin
  CBUseWASI.Enabled:=UseBrowserApp;
  edtWasmProgram.Enabled:=UseBrowserApp;
end;

procedure TWebBrowserProjectOptionsForm.FormCreate(Sender: TObject);
begin
  // localize
  Caption:=pjsdPas2JSBrowserProjectOptions;
  CBCreateHTML.Caption:=pjsdCreateInitialHTMLPage;
  CBMaintainPage.Caption:=pjsdMaintainHTMLPage;
  CBRunOnReady.Caption:=pjsdRunRTLWhenAllPageResourcesAreFullyLoaded;
  cbShowUncaughtExceptions.Caption:=pjsdLetRTLShowUncaughtExceptions;

  CBUseBrowserApp.Caption:=pjsdUseBrowserApplicationObject;
  CBUseWASI.Caption:=pjsdUseWASIApplicationObject;
  edtWasmProgram.TextHint:=pjsWasiProgramFileTextHint;

  CBUseBrowserConsole.Caption:=pjsdUseBrowserConsoleUnitToDisplayWritelnOutput;
  CBUseModule.Caption:=pjsCreateAJavascriptModuleInsteadOfAScript;

  RunGroupBox.Caption:=pjsdRun;
  RBRunServerAt.Caption:=pjsdStartHTTPServerOnPort;
  RBRunBrowserWithURL.Caption:=pjsdUseThisURLToStartApplication;
  RBRunDefault.Caption:=pjsExecuteRunParameters;

  CBCreateHTMLChange(self);
end;

procedure TWebBrowserProjectOptionsForm.RBRunDefaultChange(Sender: TObject);
begin
  UpdateRunControls;
end;

procedure TWebBrowserProjectOptionsForm.RBRunServerAtChange(Sender: TObject);
begin
  UpdateRunControls;
end;

procedure TWebBrowserProjectOptionsForm.RBRunBrowserWithURLChange(Sender: TObject);
begin
  UpdateRunControls;
end;

function TWebBrowserProjectOptionsForm.GetB(AIndex: Integer): Boolean;
begin
  Case Aindex of
    WBBoolCreateHTML : Result:=CBCreateHTML.Checked;
    WBBoolMainHTML : Result:=CBMaintainPage.Checked;
    WBBoolRunOnReady : Result:=CBRunOnReady.Checked;
    WBBoolShowUncaughtExceptions : Result:=cbShowUncaughtExceptions.Checked;
    WBBoolUseBrowserApp : Result:=CBUseBrowserApp.Checked;
    WBBoolUseWASI : Result:=cbUseWASI.Checked;
    WBBoolUseBrowserConsole : Result:=CBUseBrowserConsole.Checked;
    WBBoolUseModule : Result:=cbUseModule.Checked;
    WBBoolRunServerAtPort : Result:=RBRunServerAt.Checked;
    WBBoolRunBrowserWithURL : Result:=RBRunBrowserWithURL.Checked;
    WBBoolRunDefault : Result:=RBRunDefault.Checked;
  else
    Result:=False;
  end;
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
  WBBoolCreateHTML : begin CBCreateHTML.Checked:=AValue; UpdateHTMLControls; end;
    WBBoolMainHTML : CBMaintainPage.Checked:=AValue;
    WBBoolRunOnReady : CBRunOnReady.Checked:=AValue;
    WBBoolShowUncaughtExceptions : cbShowUncaughtExceptions.Checked:=AValue;
    WBBoolUseBrowserConsole : CBUseBrowserConsole.Checked:=AValue;
  WBBoolUseBrowserApp : begin CBUseBrowserApp.Checked:=AValue; UpdateBrowserAppControls; end;
  WBBoolUseWASI : begin cbUseWASI.Checked:=AValue; UpdateBrowserAppControls; end;
  WBBoolUseModule : cbUseModule.Checked:=AValue;
  WBBoolRunServerAtPort : begin RBRunServerAt.Checked:=AValue; UpdateRunControls; end;
  WBBoolRunBrowserWithURL : begin RBRunBrowserWithURL.Checked:=AValue; UpdateRunControls; end;
  WBBoolRunDefault : begin RBRunDefault.Checked:=AValue; UpdateRunControls; end;
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

procedure TWebBrowserProjectOptionsForm.UpdateHTMLControls;
var
  aEnabled: Boolean;
begin
  aEnabled:=CBCreateHTML.Checked;
  CBMaintainPage.Enabled:=aEnabled;
  CBRunOnReady.Enabled:=aEnabled;
  cbShowUncaughtExceptions.Enabled:=aEnabled;
  CBUseBrowserConsole.Enabled:=aEnabled;
end;

procedure TWebBrowserProjectOptionsForm.UpdateRunControls;
begin
  SEPort.Enabled:=RBRunServerAt.Enabled and RBRunServerAt.Checked;
  CBServerURL.Enabled:=RBRunBrowserWithURL.Enabled and RBRunBrowserWithURL.Checked;
end;

procedure TWebBrowserProjectOptionsForm.HideWASM;
begin
  CBUseWASI.Visible:=false;
  edtWasmProgram.Visible:=false;
end;

procedure TWebBrowserProjectOptionsForm.HideModule;
begin
  CBUseModule.Visible:=false;
end;

end.

