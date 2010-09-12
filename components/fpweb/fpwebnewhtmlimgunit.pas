unit fpwebNewHTMLImgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, EditBtn, ExtCtrls;

type

  { TfpwebNewHTMLImgForm }

  TfpwebNewHTMLImgForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    edtWidth: TEdit;
    edtHeigth: TEdit;
    edtHSpace: TEdit;
    edtVSpace: TEdit;
    edtAlt: TEdit;
    edtFileName: TFileNameEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure edtFileNameAcceptFileName(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    function HtmlText:string;
  end;

var
  fpwebNewHTMLImgForm: TfpwebNewHTMLImgForm;

implementation

{$R *.lfm}

{ TfpwebNewHTMLImgForm }

procedure TfpwebNewHTMLImgForm.FormCreate(Sender: TObject);
begin
  //
end;

procedure TfpwebNewHTMLImgForm.edtFileNameAcceptFileName(Sender: TObject;
  var Value: String);
begin
  if FileExistsUTF8(Value) then
  begin
    Image1.Picture.LoadFromFile(Value);
    edtHeigth.Text:=IntToStr(Image1.Picture.Bitmap.Height);
    edtWidth.Text:=IntToStr(Image1.Picture.Bitmap.Width);
  end;
end;

function TfpwebNewHTMLImgForm.HtmlText: string;
begin
  Result:='<IMG src="'+edtFileName.FileName+'" alt="'+edtAlt.Text+'" ';
  if edtWidth.Text<>'' then
    Result:=Result + 'width="'+edtWidth.Text+'" ';
  if edtHeigth.Text<>'' then
    Result:=Result + 'height="'+edtHeigth.Text+'" ';
  if edtHSpace.Text<>'' then
    Result:=Result + 'hspace="'+edtHSpace.Text+'" ';
  if edtVSpace.Text<>'' then
    Result:=Result + 'vspace="'+edtVSpace.Text+'" ';

  Result:=Result + '>';
end;

end.

