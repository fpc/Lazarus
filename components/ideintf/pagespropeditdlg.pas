{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Michael W. Vogel

  Abstract:
    Dialog for the TPage property editor.
}
unit PagesPropEditDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, ButtonPanel, StdCtrls, Dialogs, ExtCtrls, Controls,
  // IdeIntf
  ObjInspStrConsts, IDEDialogs;
  
type

  { TPagesPropEditorFrm }

  TPagesPropEditorFrm = class(TForm)
    BtnPanel: TButtonPanel;
    TextGroupBox: TGroupBox;
    ListBox: TListBox;
    AddButton: TButton;
    RenameButton: TButton;
    DeleteButton: TButton;
    MoveUpButton: TButton;
    MoveDownButton: TButton;
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure MoveDownButtonClick(Sender: TObject);
    procedure MoveUpButtonClick(Sender: TObject);
    procedure RenameButtonClick(Sender: TObject);
  private
    function GetNextPageName: String;
    procedure InvalidateButtons;
  end;


implementation

{$R *.lfm}

uses
  PropEdits;

{ TPagesPropEditorFrm }

procedure TPagesPropEditorFrm.AddButtonClick(Sender: TObject);
var
  aName: String;
begin
  try
    aName := GetNextPageName;
    if not InputQuery(oisAddPage, oisInsertPageName, aName) then Exit;
    if not IsValidIdent(aName) then
      raise Exception.Create(Format(oisComponentNameIsNotAValidIdentifier, [aName]));
    if ListBox.Items.IndexOf(aName) >= 0 then
      raise Exception.Create(Format(oisComponentNameIsNotAValidIdentifier, [aName]));
    ListBox.AddItem(aName, nil);
  except
    on e: Exception do
      ShowMessage(e.Message);
  end;
  InvalidateButtons;
end;

procedure TPagesPropEditorFrm.FormCreate(Sender: TObject);
begin
  Caption := oisPagesEditorDialog;
  TextGroupBox.Caption := oisPages;
  AddButton.Caption := oisAdd;
  RenameButton.Caption := oisRename;
  DeleteButton.Caption := oisDelete;
  MoveUpButton.Caption := rscdMoveUp;
  MoveDownButton.Caption := rscdMoveDown;
end;

procedure TPagesPropEditorFrm.DeleteButtonClick(Sender: TObject);
begin
  if IDEQuestionDialog(nbcesDeletePage, oisDeletePageQuestion,
    mtConfirmation, [mrYes, mrNo, mrCancel]) <> mrYes then Exit;
  ListBox.DeleteSelected;
  InvalidateButtons;
end;

procedure TPagesPropEditorFrm.FormShow(Sender: TObject);
begin
  InvalidateButtons;
end;

procedure TPagesPropEditorFrm.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  if not User then Exit;
  InvalidateButtons;
end;

procedure TPagesPropEditorFrm.MoveDownButtonClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := ListBox.ItemIndex;
  ListBox.Items.Move(Index, Index + 1);
  ListBox.ItemIndex := Index + 1;
end;

procedure TPagesPropEditorFrm.MoveUpButtonClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := ListBox.ItemIndex;
  ListBox.Items.Move(Index, Index - 1);
  ListBox.ItemIndex := Index - 1;
end;

procedure TPagesPropEditorFrm.RenameButtonClick(Sender: TObject);
var
  aName: String;
begin
  try
    aName := ListBox.Items[ListBox.ItemIndex];
    if not InputQuery(oisRenamePage, oisInsertPageName, aName) then Exit;
    if aName = ListBox.Items[ListBox.ItemIndex] then Exit;
    if not IsValidIdent(aName) then
      raise Exception.Create(Format(oisComponentNameIsNotAValidIdentifier, [aName]));
    if ListBox.Items.IndexOf(aName) >= 0 then
      raise Exception.Create(Format(oisComponentNameIsNotAValidIdentifier, [aName]));
    ListBox.Items[ListBox.ItemIndex] := aName;
  except
    on e: Exception do
      ShowMessage(e.Message);
  end;
end;

function TPagesPropEditorFrm.GetNextPageName: String;
var
  i, j: integer;
begin
  // same as TCustomFormEditor.CreateUniqueComponentName
  i := 1;
  while True do begin
    j := ListBox.Items.Count - 1;
    Result := ClassNameToComponentName(TPage.ClassName);
    if Result[Length(Result)] in ['0'..'9'] then
      Result := Result + '_';
    Result := Result + IntToStr(i);
    while (j >= 0)
    and (CompareText(Result, ListBox.Items[j]) <> 0) do
      dec(j);
    if j < 0 then Exit;
    inc(i);
  end;
end;

procedure TPagesPropEditorFrm.InvalidateButtons;
begin
  if ListBox.Count = 0 then
  begin
    RenameButton.Enabled := False;
    DeleteButton.Enabled := False;
    MoveUpButton.Enabled := False;
    MoveDownButton.Enabled := False;
    Exit;
  end;

  if ListBox.Count = 1 then
  begin
    RenameButton.Enabled := True;
    DeleteButton.Enabled := True;
    MoveUpButton.Enabled := False;
    MoveDownButton.Enabled := False;
    ListBox.ItemIndex := 0;
    Exit;
  end;

  RenameButton.Enabled := True;
  DeleteButton.Enabled := True;
  if ListBox.ItemIndex < 0 then
    ListBox.ItemIndex := 0;
  if ListBox.ItemIndex = 0 then
    MoveUpButton.Enabled := False
  else
    MoveUpButton.Enabled := True;
  if ListBox.ItemIndex = ListBox.Count - 1 then
    MoveDownButton.Enabled := False
  else
    MoveDownButton.Enabled := True;
end;

end.

