{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Dialog for the TStrings property editor.
}
unit StringsPropEditDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Buttons, Dialogs, StdCtrls,
  TextTools, ObjInspStrConsts, ExtCtrls, ButtonPanel;
  
type

  { TStringsPropEditorFrm }

  TStringsPropEditorFrm = class(TForm)
    BtnPanel: TButtonPanel;
    StatusLabel: TLabel;
    SortButton: TButton;
    TextGroupBox: TGroupBox;
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure MemoChange(Sender: TObject);
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

  AddButtons;
end;

procedure TStringsPropEditorFrm.MemoChange(Sender: TObject);
var
  NumChars: Integer;
  I: Integer;
begin
  NumChars := 0;
  for I := 0 to Memo.Lines.Count - 1 do Inc(NumChars, Utf8Length(Memo.Lines[I]));

  if Memo.Lines.Count = 1 then
    StatusLabel.Caption := Format(ois1LineDChars, [NumChars])
  else
    StatusLabel.Caption := Format(oisDLinesDChars, [Memo.Lines.Count, NumChars]);
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

