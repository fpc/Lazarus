{
  Author: Mattias Gaertner

  Abstract:
    A dialog for adding and editing code templates

  ToDo:
    -check if token already exists
}
unit CodeTemplateDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLLinux, LResources, Forms, Buttons, Controls,
  SynEditAutoComplete, StdCtrls, SynEditKeyCmds;

type
  TCodeTemplateEditForm = class(TForm)
    TokenLabel:TLabel;
    TokenEdit:TEdit;
    CommentLabel:TLabel;
    CommentEdit:TEdit;
    OkButton:TButton;
    CancelButton:TButton;
    procedure OkButtonClick(Sender:TObject);
  public
    constructor Create(AOwner:TComponent); override;
    SynAutoComplete:TSynEditAutoComplete;
    TemplateIndex:integer;
  end;

function AddCodeTemplate(ASynAutoComplete:TSynEditAutoComplete;
  var Token,Comment:ansistring):TModalResult;
function EditCodeTemplate(ASynAutoComplete:TSynEditAutoComplete;
  Index:integer):TModalResult;

implementation

function AddCodeTemplate(ASynAutoComplete:TSynEditAutoComplete;
  var Token,Comment:ansistring):TModalResult;
var
  CodeTemplateEditForm:TCodeTemplateEditForm;
begin
  Result:=mrCancel;
  CodeTemplateEditForm:=TCodeTemplateEditForm.Create(Application);
  try
    CodeTemplateEditForm.SynAutoComplete:=ASynAutoComplete;
    CodeTemplateEditForm.TemplateIndex:=ASynAutoComplete.Completions.Count;
    CodeTemplateEditForm.Caption:='Add code template';
    CodeTemplateEditForm.OkButton.Caption:='Add';
    CodeTemplateEditForm.TokenEdit.Text:=Token;
    CodeTemplateEditForm.CommentEdit.Text:=Comment;
    Result:=CodeTemplateEditForm.ShowModal;
    if Result=mrOk then begin
      Token:=CodeTemplateEditForm.TokenEdit.Text;
      Comment:=CodeTemplateEditForm.CommentEdit.Text;
    end;
  finally
    CodeTemplateEditForm.Free;
  end;
end;

function EditCodeTemplate(ASynAutoComplete:TSynEditAutoComplete;
  Index:integer):TModalResult;
var
  CodeTemplateEditForm:TCodeTemplateEditForm;
begin
  Result:=mrCancel;
  if (Index<0) or (Index>=ASynAutoComplete.Completions.Count) then exit;
  CodeTemplateEditForm:=TCodeTemplateEditForm.Create(Application);
  try
    CodeTemplateEditForm.SynAutoComplete:=ASynAutoComplete;
    CodeTemplateEditForm.TemplateIndex:=Index;
    CodeTemplateEditForm.Caption:='Edit code template';
    CodeTemplateEditForm.OkButton.Caption:='Change';
    CodeTemplateEditForm.TokenEdit.Text:=ASynAutoComplete.Completions[Index];
    CodeTemplateEditForm.CommentEdit.Text:=
      ASynAutoComplete.CompletionComments[Index];
    Result:=CodeTemplateEditForm.ShowModal;
    if Result=mrOk then begin
      ASynAutoComplete.Completions[Index]:=
        CodeTemplateEditForm.TokenEdit.Text;
      ASynAutoComplete.CompletionComments[Index]:=
        CodeTemplateEditForm.CommentEdit.Text;
    end;
  finally
    CodeTemplateEditForm.Free;
  end;
end;

{ TCodeTemplateEditForm }

constructor TCodeTemplateEditForm.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    Width:=300;
    Height:=140;
    Position:=poScreenCenter;

    TokenLabel:=TLabel.Create(Self);
    with TokenLabel do begin
      Name:='TokenLabel';
      Parent:=Self;
      Caption:='Token:';
      Left:=12;
      Top:=6;
      Width:=Self.ClientWidth-Left-Left;
      Show;
    end;

    TokenEdit:=TEdit.Create(Self);
    with TokenEdit do begin
      Name:='TokenEdit';
      Parent:=Self;
      Left:=10;
      Top:=TokenLabel.Top+TokenLabel.Height+2;
      Width:=Self.ClientWidth-Left-Left-4;
      Text:='';
      Show;
    end;

    CommentLabel:=TLabel.Create(Self);
    with CommentLabel do begin
      Name:='CommentLabel';
      Parent:=Self;
      Caption:='Comment:';
      Left:=12;
      Top:=TokenEdit.Top+TokenEdit.Height+23;
      Width:=Self.ClientWidth-Left-Left;
      Show;
    end;

    CommentEdit:=TEdit.Create(Self);
    with CommentEdit do begin
      Name:='CommentEdit';
      Parent:=Self;
      Left:=10;
      Top:=CommentLabel.Top+CommentLabel.Height+2;
      Width:=Self.ClientWidth-Left-Left-4;
      Text:='';
      Show;
    end;

    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      Parent:=Self;
      Caption:='Ok';
      OnClick:=@OkButtonClick;
      Left:=50;
      Top:=Self.ClientHeight-Height-12;
      Width:=80;
      Show;
    end;

    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      Parent:=Self;
      Caption:='Cancel';
      ModalResult:=mrCancel;
      Width:=80;
      Left:=Self.ClientWidth-50-Width;
      Top:=Self.ClientHeight-Height-12;
      Show;
    end;
  end;
end;

procedure TCodeTemplateEditForm.OkButtonClick(Sender:TObject);
var a:integer;
  AText,ACaption:AnsiString;
begin
  a:=SynAutoComplete.Completions.IndexOf(TokenEdit.Text);
  if (a<0) or (a=TemplateIndex) then
    ModalResult:=mrOk
  else begin
    AText:=' A token '''+TokenEdit.Text+''' already exists! ';
    ACaption:='Error';
    Application.MessageBox(PChar(AText),PChar(ACaption),0);
  end;
end;

end.
