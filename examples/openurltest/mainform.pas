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

procedure TForm1.btnOpenURLHTTPClick(Sender: TObject);
begin
  editResult.Text := BoolToStr(OpenURL('www.google.com'));
end;

procedure TForm1.btnOpenDocumentClick(Sender: TObject);
begin
  editResult.Text := BoolToStr(OpenDocument(editFilename.Text));
end;

procedure TForm1.btnFindBrowserClick(Sender: TObject);
var
  lStr, lParams: String;
begin
  FindDefaultBrowser(lStr, lParams);
  editResult.Text := lStr + ' ' + lParams;
end;

procedure TForm1.btnOpenURLFILEClick(Sender: TObject);
begin
  editResult.Text := BoolToStr(OpenURL('file://'+editFilename.Text));
end;

end.

