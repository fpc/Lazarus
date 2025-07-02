{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Dialog for the TStrings property editor.
}
unit StringsPropEditDlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LCL
  Forms, Controls, StdCtrls, ButtonPanel, Dialogs, LCLType,
  // LazUtils
  LazUTF8,
  // IdeIntf
  TextTools, ObjInspStrConsts, IDEWindowIntf, Classes;

type

  { TStringsPropEditorFrm }

  TStringsPropEditorFrm = class(TForm)
    BtnPanel: TButtonPanel;
    ClearButton: TButton;
    SaveButton: TButton;
    LoadButton: TButton;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    StatusLabel: TLabel;
    SortButton: TButton;
    TextGroupBox: TGroupBox;
    Memo: TMemo;
    procedure ClearButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MemoChange(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure SortButtonClick(Sender: TObject);
  public
    procedure AddButtons; virtual;
  end;


implementation

{$R *.lfm}

{ TStringsPropEditorFrm }

procedure TStringsPropEditorFrm.FormCreate(Sender: TObject);
begin
  Caption := oisStringsEditorDialog;
  StatusLabel.Caption := ois0Lines0Chars;
  SortButton.Caption := oisSort;
  ClearButton.Caption := oisClear;
  SaveButton.Caption := oisSave;
  LoadButton.Caption := oisLoad;
  AddButtons;
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TStringsPropEditorFrm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // dialog
  if (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    ModalResult := mrOK;
    Key := 0;
  end
  // save and load
  else if (Key = VK_S) and (Shift = [ssCtrl]) then
  begin
    SaveButtonClick(Sender);
    Key := 0;
  end
  else if (Key = VK_O) and (Shift = [ssCtrl]) then
  begin
    LoadButtonClick(Sender);
    Key := 0;
  end;
end;

procedure TStringsPropEditorFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TStringsPropEditorFrm.ClearButtonClick(Sender: TObject);
begin
  Memo.Clear;
end;

procedure TStringsPropEditorFrm.MemoChange(Sender: TObject);
var
  NumChars: Integer;
  I: Integer;
begin
  NumChars := 0;
  for I := 0 to Memo.Lines.Count - 1 do
    Inc(NumChars, Utf8Length(Memo.Lines[I]));

  if Memo.Lines.Count = 1 then
    StatusLabel.Caption := Format(ois1LineDChars, [NumChars])
  else
    StatusLabel.Caption := Format(oisDLinesDChars, [Memo.Lines.Count, NumChars]);
end;

procedure TStringsPropEditorFrm.SaveButtonClick(Sender: TObject);
begin
  SaveDialog1.Title:=sccsSGEdtSaveDialog;
  if SaveDialog1.Execute then
    Memo.Lines.SaveToFile(SaveDialog1.FileName);
end;

procedure TStringsPropEditorFrm.LoadButtonClick(Sender: TObject);
begin
  OpenDialog1.Title:=sccsSGEdtOpenDialog;
  if OpenDialog1.Execute then
    Memo.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TStringsPropEditorFrm.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  if length(FileNames) <> 1 then
  begin
    MessageDlg(oisError, oisOnlyOneFile, mtError, [mbOK], 0);
    exit;
  end;

  Memo.Lines.LoadFromFile(FileNames[0]);
end;

procedure TStringsPropEditorFrm.SortButtonClick(Sender: TObject);
var
  OldText, NewSortedText: String;
  SortOnlySelection: Boolean;
begin
  if not Assigned(ShowSortSelectionDialogFunc) then
  begin
    SortButton.Enabled := False;
    Exit;
  end;

  SortOnlySelection := True;
  OldText := Memo.SelText;
  if OldText = '' then
  begin
    SortOnlySelection := False;
    OldText := Memo.Lines.Text;
  end;

  NewSortedText:='';
  if ShowSortSelectionDialogFunc(OldText, nil, NewSortedText) <> mrOk then Exit;
  if SortOnlySelection then
    Memo.SelText := NewSortedText
  else
    Memo.Lines.Text := NewSortedText;
end;

procedure TStringsPropEditorFrm.AddButtons;
begin
  //
end;

end.

