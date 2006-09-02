{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  Classes, SysUtils, LResources, Forms, Controls, Buttons, Dialogs, StdCtrls,
  TextTools, ObjInspStrConsts;
  
type

  { TStringsPropEditorFrm }

  TStringsPropEditorFrm = class(TForm)
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    StatusLabel: TLabel;
    SortButton: TButton;
    GroupBox1: TGroupBox;
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure MemoChange(Sender: TObject);
    procedure SortButtonClick(Sender: TObject);
  public
    procedure AddButtons; virtual;
  end;


implementation

{ TStringsPropEditorFrm }

procedure TStringsPropEditorFrm.FormCreate(Sender: TObject);
begin
  Caption := oisStringsEditorDialog;
  StatusLabel.Caption := ois0Lines0Chars;
  SortButton.Caption := oisSort;
  OKButton.Caption:=oisOk2;
  CancelButton.Caption:=oiStdActDataSetCancel1Hint;

  AddButtons;
end;

procedure TStringsPropEditorFrm.MemoChange(Sender: TObject);
var
  NumChars: Integer;
  I: Integer;
begin
  NumChars := 0;
  for I := 0 to Memo.Lines.Count - 1 do Inc(NumChars, Length(Memo.Lines[I]));

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

initialization
  {$I stringspropeditdlg.lrs}

end.

