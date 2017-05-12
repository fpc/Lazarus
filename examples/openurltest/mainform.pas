unit mainform;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, StdCtrls, EditBtn, LCLIntf;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnOpenURLHTTP: TButton;
    btnOpenURLFILE: TButton;
    btnOpenDocument: TButton;
    btnFindBrowser: TButton;
    editResult: TEdit;
    editFileName: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure btnFindBrowserClick(Sender: TObject);
    procedure btnOpenDocumentClick(Sender: TObject);
    procedure btnOpenURLHTTPClick(Sender: TObject);
    procedure btnOpenURLFILEClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnFindBrowserClick(Sender: TObject);
var
  lStr, lParams: String;
begin
  FindDefaultBrowser(lStr, lParams);
  editResult.Text := lStr + ' ' + lParams;
end;

procedure TForm1.btnOpenURLHTTPClick(Sender: TObject);
begin
  if editFileName.Text = '' then
    editFileName.Text := 'www.google.com';
  editResult.Text := BoolToStr(OpenURL(editFileName.Text), true);
end;

procedure TForm1.btnOpenDocumentClick(Sender: TObject);
begin
  if editFileName.Text = '' then
    editFileName.Text := 'mainform.pas';
  editResult.Text := BoolToStr(OpenDocument(editFilename.Text), true);
end;

procedure TForm1.btnOpenURLFILEClick(Sender: TObject);
begin
  if editFileName.Text = '' then
    editFileName.Text := ExpandFileName('./mainform.pas');
  editResult.Text := BoolToStr(OpenURL('file://'+editFilename.Text), true);
end;

end.

