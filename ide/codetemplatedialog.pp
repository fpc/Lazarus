{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
 
  Author: Mattias Gaertner

  Abstract:
    A dialog for adding and editing code templates

}
unit CodeTemplateDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLLinux, LResources, Forms, Buttons, Controls,
  SynEditAutoComplete, LazarusIDEStrConsts, StdCtrls, SynEditKeyCmds, Dialogs;

type
  TCodeTemplateEditForm = class(TForm)
    TokenLabel:TLabel;
    TokenEdit:TEdit;
    CommentLabel:TLabel;
    CommentEdit:TEdit;
    OkButton:TButton;
    CancelButton:TButton;
    procedure CodeTemplateEditFormResize(Sender: TObject);
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
    CodeTemplateEditForm.Caption:=lisCodeTemplAddCodeTemplate;
    CodeTemplateEditForm.OkButton.Caption:=lisCodeTemplAdd;
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
    CodeTemplateEditForm.Caption:=lisCodeTemplEditCodeTemplate;
    CodeTemplateEditForm.OkButton.Caption:=lisCodeTemplChange;
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
    Height:=150;
    Position:=poScreenCenter;
    OnResize:=@CodeTemplateEditFormResize;

    TokenLabel:=TLabel.Create(Self);
    with TokenLabel do begin
      Name:='TokenLabel';
      Parent:=Self;
      Caption:=lisCodeTemplToken;
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
      Caption:=lisCodeTemplComment;
      Left:=12;
      Top:=TokenEdit.Top+TokenEdit.Height+10;
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
      Caption:=lisLazBuildOk;
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
      Caption:=dlgCancel;
      ModalResult:=mrCancel;
      Width:=80;
      Left:=Self.ClientWidth-50-Width;
      Top:=Self.ClientHeight-Height-12;
      Show;
    end;
  end;
  CodeTemplateEditFormResize(nil);
end;

procedure TCodeTemplateEditForm.CodeTemplateEditFormResize(Sender: TObject);
begin
  with TokenLabel do begin
    Left:=12;
    Top:=6;
    Width:=Self.ClientWidth-Left-Left;
  end;

  with TokenEdit do begin
    Left:=10;
    Top:=TokenLabel.Top+TokenLabel.Height+2;
    Width:=Self.ClientWidth-Left-Left-4;
  end;

  with CommentLabel do begin
    Left:=12;
    Top:=TokenEdit.Top+TokenEdit.Height+10;
    Width:=Self.ClientWidth-Left-Left;
  end;

  with CommentEdit do begin
    Left:=10;
    Top:=CommentLabel.Top+CommentLabel.Height+2;
    Width:=Self.ClientWidth-Left-Left-4;
  end;

  with OkButton do begin
    Left:=50;
    Top:=Self.ClientHeight-Height-12;
    Width:=80;
  end;

  with CancelButton do begin
    Width:=80;
    Left:=Self.ClientWidth-50-Width;
    Top:=Self.ClientHeight-Height-12;
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
    AText:=Format(lisCodeTemplATokenAlreadyExists, ['"', TokenEdit.Text, '"']);
    ACaption:=lisCodeTemplError;

//    Application.MessageBox(PChar(AText),PChar(ACaption),0);
    MessageDlg(ACaption,AText,mterror,[mbok],0);

  end;
end;

end.
