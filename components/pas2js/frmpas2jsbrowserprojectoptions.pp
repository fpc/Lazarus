unit frmpas2jsbrowserprojectoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  Spin;

type

  { TWebBrowserProjectOptionsForm }

  TWebBrowserProjectOptionsForm = class(TForm)
    BPHelpOptions: TButtonPanel;
    CBCreateHTML: TCheckBox;
    CBUseBrowserApp: TCheckBox;
    CBUseBrowserConsole: TCheckBox;
    CBUseHTTPServer: TCheckBox;
    CBServerURL: TComboBox;
    CBMaintainPage: TCheckBox;
    CBRunOnReady: TCheckBox;
    RBUseURL: TRadioButton;
    RBStartServerAt: TRadioButton;
    SEPort: TSpinEdit;
    procedure CBCreateHTMLChange(Sender: TObject);
  private
    function GetB(AIndex: Integer): Boolean;
    function GetServerPort: Word;
    function GetURL: String;
    procedure SetB(AIndex: Integer; AValue: Boolean);
    procedure SetServerPort(AValue: Word);
    procedure SetURL(AValue: String);

  public
    property CreateHTML : Boolean Index 0 read GetB Write SetB;
    property MaintainHTML : Boolean Index 1 read GetB Write SetB;
    property UseBrowserApp : Boolean Index 2 read GetB Write SetB;
    property UseBrowserConsole : Boolean Index 3 read GetB Write SetB;
    property StartHTTPServer : Boolean Index 4 read GetB Write SetB;
    property UseURL : Boolean Index 5 read GetB Write SetB;
    property UseRunOnReady : Boolean Index 6 read GetB Write SetB;
    Property ServerPort : Word Read GetServerPort Write SetServerPort;
    Property URL : String Read GetURL Write SetURL;
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

procedure TWebBrowserProjectOptionsForm.SetB(AIndex: Integer; AValue: Boolean);
begin
  Case Aindex of
    0 : CBCreateHTML.Checked:=AValue;
    1 : CBMaintainPage.Checked:=AValue;
    2 : CBUseBrowserApp.Checked:=AValue;
    3 : CBUseBrowserConsole.Checked:=AValue;
    4 : RBStartServerAt.Checked:=AValue;
    5 : RBUseURL.Checked:=AValue;
    6 : CBRunOnReady.Checked:=Avalue;
  end;
end;

procedure TWebBrowserProjectOptionsForm.SetServerPort(AValue: Word);
begin
  SEPort.Value:=AValue;
end;

procedure TWebBrowserProjectOptionsForm.SetURL(AValue: String);
begin

end;

end.

