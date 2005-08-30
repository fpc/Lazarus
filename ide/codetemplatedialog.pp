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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  LazarusIDEStrConsts, StdCtrls, Buttons, SynEdit, SynHighlighterPas, ExtCtrls,
  EditorOptions, SynCompletion;

type

  { TCodeTemplateDialog }

  TCodeTemplateDialog = class(TForm)
    AddButton: TButton;
    EditButton: TButton;
    DeleteButton: TButton;
    CancelButton: TButton;
    TemplateListBox: TListBox;
    TemplateSplitter: TSplitter;
    TemplateSynEdit: TSynEdit;
    ASynPasSyn: TSynPasSyn;
    TemplateGroupBox: TGroupBox;
    OkButton: TButton;
    FilenameButton: TButton;
    FilenameEdit: TEdit;
    FilenameGroupBox: TGroupBox;
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TemplateListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    SynAutoComplete: TSynEditAutoComplete;
  public
    procedure FillCodeTemplateListBox;
    procedure ShowCurCodeTemplate;
    procedure SaveCurCodeTemplate;
  end;

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

function ShowCodeTemplateDialog: TModalResult;

function AddCodeTemplate(ASynAutoComplete:TSynEditAutoComplete;
  var Token,Comment:ansistring):TModalResult;
function EditCodeTemplate(ASynAutoComplete:TSynEditAutoComplete;
  Index:integer):TModalResult;

implementation

function ShowCodeTemplateDialog: TModalResult;
var
  CodeTemplateDialog: TCodeTemplateDialog;
begin
  CodeTemplateDialog:=TCodeTemplateDialog.Create(nil);
  Result:=CodeTemplateDialog.ShowModal;
  CodeTemplateDialog.Free;
end;

function AddCodeTemplate(ASynAutoComplete:TSynEditAutoComplete;
  var Token,Comment:ansistring):TModalResult;
var
  CodeTemplateEditForm:TCodeTemplateEditForm;
begin
  Result:=mrCancel;
  CodeTemplateEditForm:=TCodeTemplateEditForm.Create(nil);
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
  CodeTemplateEditForm:=TCodeTemplateEditForm.Create(nil);
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

{ TCodeTemplateDialog }

procedure TCodeTemplateDialog.FormCreate(Sender: TObject);
begin
  SynAutoComplete:=TSynEditAutoComplete.Create(Self);

  AddButton.Caption:=lisCodeTemplAdd;
  EditButton.Caption:=lisCodeToolsDefsEdit;
  DeleteButton.Caption:=dlgEdDelete;
  CancelButton.Caption:=dlgCancel;
  TemplateGroupBox.Caption:=lisCTDTemplates;
  OkButton.Caption:=lisLazBuildOk;
  FilenameGroupBox.Caption:=lisToDoLFile;

  FilenameEdit.Text:=EditorOpts.CodeTemplateFileName;

  TemplateSynEdit.Gutter.Visible:=false;
  with SynAutoComplete do begin
    s:=EditorOpts.CodeTemplateFileName;
    if FileExists(s) then
      try
        AutoCompleteList.LoadFromFile(s);
      except
        DebugLn('NOTE: unable to read code template file ''',s,'''');
      end;
  end;
  FillCodeTemplateListBox;
  with CodeTemplateListBox do
    if Items.Count>0 then begin
      Selected[0]:=true;
      ShowCurCodeTemplate;
    end;
end;

procedure TCodeTemplateDialog.AddButtonClick(Sender: TObject);
var
  Token: String;
  Comment: String;
begin
  SaveCurCodeTemplate;
  Token:='new';
  Comment:='(custom)';
  if AddCodeTemplate(SynAutoComplete,Token,Comment)=mrOk then begin
    SynAutoComplete.AddCompletion(Token, '', Comment);
    FillCodeTemplateListBox;
    Index:=SynAutoComplete.Completions.IndexOf(Token);
    if (Index>=0) and (Index<TemplateListBox.Items.Count) then begin
      TemplateListBox.ItemIndex:=Index;
    end;
    ShowCurCodeTemplate;
  end;
end;

procedure TCodeTemplateDialog.DeleteButtonClick(Sender: TObject);
var
  i: LongInt;
begin
  i:=TemplateListBox.ItemIndex;
  if i<0 then exit;
  if MessageDlg(dlgDelTemplate
      +'"'+SynAutoComplete.Completions[i]+' - '
      +SynAutoComplete.CompletionComments[i]+'"'
      +'?',mtConfirmation,[mbOk,mbCancel],0)=mrOK then begin
    SynAutoComplete.DeleteCompletion(i);
    FillCodeTemplateListBox;
    if (i>=0) and (i<CodeTemplateListBox.Items.Count) then begin
      CodeTemplateListBox.ItemIndex:=i;
    end;
    ShowCurCodeTemplate;
  end;
end;

procedure TCodeTemplateDialog.EditButtonClick(Sender: TObject);
begin
  i:=TemplateListBox.ItemIndex;
  if i<0 then exit;
  if EditCodeTemplate(SynAutoComplete,i)=mrOk then begin
    CodeTemplateListBox.Items[i]:=
       SynAutoComplete.Completions[i]
       +' - "'+SynAutoComplete.CompletionComments[i]+'"';
    ShowCurCodeTemplate;
  end;
end;

procedure TCodeTemplateDialog.TemplateListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  SaveCurCodeTemplate;
  ShowCurCodeTemplate;
end;

procedure TCodeTemplateDialog.FillCodeTemplateListBox;
var a:integer;
begin
  with TemplateListBox do begin
    Items.BeginUpdate;
    Items.Clear;
    for a:=0 to SynAutoComplete.Completions.Count-1 do begin
      Items.Add(SynAutoComplete.Completions[a]
          +' - "'+SynAutoComplete.CompletionComments[a]+'"');
    end;
    Items.EndUpdate;
  end;
end;

procedure TCodeTemplateDialog.ShowCurCodeTemplate;
var
  i, sp, ep: integer;
  s: string;
begin
  TemplateSynEdit.Lines.BeginUpdate;
  TemplateSynEdit.Lines.Clear;
  i:=TemplateListBox.ItemIndex;
  if i>=0 then begin
    CurCodeTemplate:=i;
    s:=SynAutoComplete.CompletionValues[i];
    sp:=1;
    ep:=1;
    while ep<=length(s) do begin
      if s[ep] in [#10,#13] then begin
        TemplateSynEdit.Lines.Add(copy(s,sp,ep-sp));
        inc(ep);
        if (ep<=length(s)) and (s[ep] in [#10,#13]) and (s[ep-1]<>s[ep]) then
          inc(ep);
        sp:=ep;
      end else inc(ep);
    end;
    if (ep>sp) or ((s<>'') and (s[length(s)] in [#10,#13])) then
      TemplateSynEdit.Lines.Add(copy(s,sp,ep-sp));
  end;
  TemplateSynEdit.Lines.EndUpdate;
  TemplateSynEdit.Invalidate;
end;

procedure TCodeTemplateDialog.SaveCurCodeTemplate;
var
  NewValue: string;
  l: integer;
  i: LongInt;
begin
  i:=TemplateListBox.ItemIndex;
  if i<0 then exit;
  NewValue:=CodeTemplateCodePreview.Lines.Text;
  // remove last EOL
  if NewValue<>'' then begin
    l:=length(NewValue);
    if NewValue[l] in [#10,#13] then begin
      dec(l);
      if (l>0) and (NewValue[l] in [#10,#13])
      and (NewValue[l]<>NewValue[l+1]) then
        dec(l);
      SetLength(NewValue,l);
    end;
  end;
  SynAutoComplete.CompletionValues[i]:=NewValue;
end;

initialization
  {$I codetemplatesdlg.lrs}

end.
