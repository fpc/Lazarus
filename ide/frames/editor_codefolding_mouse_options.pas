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
}
unit editor_codefolding_mouse_options;

{$mode objfpc}{$H+}

interface

uses
  LResources, EditorOptions, LazarusIDEStrConsts, IDEOptionsIntf,
  StdCtrls, ExtCtrls, Classes, Controls, LCLProc,
  SynGutterCodeFolding;

type

  { TMouseGroupBox1 }

  TMouseGroupBox1 = class(TAbstractIDEOptionsEditor)
    MouseCtrl1: TCheckBox;
    MouseAlt1: TCheckBox;
    MouseShift2: TCheckBox;
    MouseCtrl2: TCheckBox;
    MouseAlt2: TCheckBox;
    MouseShift1: TCheckBox;
    MouseEnabled1: TCheckBox;
    MouseEnabled2: TCheckBox;
    MouseGroupBox1: TGroupBox;
    MouseConfListBox: TListBox;
    MouseGroupBox2: TGroupBox;
    MouseRadioGroup1: TRadioGroup;
    MouseRadioGroup2: TRadioGroup;
    procedure MouseConfListBoxClick(Sender: TObject);
    procedure MouseConfListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MouseEnabled1Change(Sender: TObject);
  private
    ListBoxUpdating: Boolean;
    FExpandedClickConf,
    FCollapsedClickConf: TSynGutterFoldClickConfList;
  protected
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{ TMouseGroupBox1 }

procedure TMouseGroupBox1.MouseConfListBoxClick(Sender: TObject);
var
  c: TSynGutterFoldClickConf;
  function GetState(s: TShiftStateEnum): TCheckBoxState;
  begin
    if not(s in c.ShiftMask) then exit(cbGrayed);
    if s in c.Shift then exit(cbChecked);
    Result := cbUnchecked;
  end;
  function GetState2(s: TShiftStateEnum): TCheckBoxState;
  begin
    if not(s in c.ShiftMask2) then exit(cbGrayed);
    if s in c.Shift2 then exit(cbChecked);
    Result := cbUnchecked;
  end;
begin
  ListBoxUpdating := True;
  case MouseConfListBox.ItemIndex of
    0: c  := FExpandedClickConf[sgctFoldOne];
    1: c  := FExpandedClickConf[sgctFoldAll];
    2: c  := FCollapsedClickConf[sgctFoldOne];
    3: c  := FCollapsedClickConf[sgctFoldAll];
    4: c  := FCollapsedClickConf[sgctUnFoldOne];
    5: c  := FCollapsedClickConf[sgctUnFoldAll];
  end;
  MouseEnabled1.Checked := c.Enabled;
  MouseRadioGroup1.ItemIndex := ord(c.Button);
  MouseShift1.State := GetState(ssShift);
  MouseCtrl1.State := GetState(ssCtrl);
  MouseAlt1.State := GetState(ssAlt);
  MouseEnabled2.Checked := c.Enabled2;
  MouseRadioGroup2.ItemIndex := ord(c.Button2);
  MouseShift2.State := GetState2(ssShift);
  MouseCtrl2.State := GetState2(ssCtrl);
  MouseAlt2.State := GetState2(ssAlt);
  ListBoxUpdating := False;
end;

procedure TMouseGroupBox1.MouseConfListBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  MouseConfListBoxClick(Sender);
end;

procedure TMouseGroupBox1.MouseEnabled1Change(Sender: TObject);
var
  c: TSynGutterFoldClickConf;
begin
  if ListBoxUpdating then exit;
  c.Enabled := MouseEnabled1.Checked;
  c.Button := TMouseButton(MouseRadioGroup1.ItemIndex);
  c.Shift := [];
  c.ShiftMask := [];
  if MouseShift1.State = cbChecked then Include(c.Shift, ssShift);
  if not(MouseShift1.State = cbGrayed) then Include(c.ShiftMask, ssShift);
  if MouseCtrl1.State = cbChecked then Include(c.Shift, ssCtrl);
  if not(MouseCtrl1.State = cbGrayed) then Include(c.ShiftMask, ssCtrl);
  if MouseAlt1.State = cbChecked then Include(c.Shift, ssAlt);
  if not(MouseAlt1.State = cbGrayed) then Include(c.ShiftMask, ssAlt);

  c.Enabled2 := MouseEnabled2.Checked;
  c.Button2 := TMouseButton(MouseRadioGroup2.ItemIndex);
  c.Shift2 := [];
  c.ShiftMask2 := [];
  if MouseShift2.State = cbChecked then Include(c.Shift2, ssShift);
  if not(MouseShift2.State = cbGrayed) then Include(c.ShiftMask2, ssShift);
  if MouseCtrl2.State = cbChecked then Include(c.Shift2, ssCtrl);
  if not(MouseCtrl2.State = cbGrayed) then Include(c.ShiftMask2, ssCtrl);
  if MouseAlt2.State = cbChecked then Include(c.Shift2, ssAlt);
  if not(MouseAlt2.State = cbGrayed) then Include(c.ShiftMask2, ssAlt);

  case MouseConfListBox.ItemIndex of
    0: FExpandedClickConf[sgctFoldOne] := c;
    1: FExpandedClickConf[sgctFoldAll] := c;
    2: FCollapsedClickConf[sgctFoldOne] := c;
    3: FCollapsedClickConf[sgctFoldAll] := c;
    4: FCollapsedClickConf[sgctUnFoldOne] := c;
    5: FCollapsedClickConf[sgctUnFoldAll] := c;
  end;
end;

function TMouseGroupBox1.GetTitle: String;
begin
  Result := dlgCodeFoldingMouse;
end;

procedure TMouseGroupBox1.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  MouseConfListBox.Items.Add(dlgMouseFoldExpFoldOne);
  MouseConfListBox.Items.Add(dlgMouseFoldExpFoldAll);
  MouseConfListBox.Items.Add(dlgMouseFoldColFoldOne);
  MouseConfListBox.Items.Add(dlgMouseFoldColFoldAll);
  MouseConfListBox.Items.Add(dlgMouseFoldColUnFoldOne);
  MouseConfListBox.Items.Add(dlgMouseFoldColUnFoldAll);

  MouseGroupBox1.Caption := dlgMouseFoldGroup1;
  MouseEnabled1.Caption := dlgMouseFoldEnabled;
  MouseRadioGroup1.Caption := dlgMouseFoldButton;
  MouseRadioGroup1.Items.add(dlgMouseFoldButtonLeft);
  MouseRadioGroup1.Items.add(dlgMouseFoldButtonRight);
  MouseRadioGroup1.Items.add(dlgMouseFoldButtonMiddle);
  MouseShift1.Caption := dlgMouseFoldModifierShift;
  MouseCtrl1.Caption := dlgMouseFoldModifierCtrl;
  MouseAlt1.Caption := dlgMouseFoldModifierAlt;

  MouseGroupBox2.Caption := dlgMouseFoldGroup2;
  MouseEnabled2.Caption := dlgMouseFoldEnabled;
  MouseRadioGroup2.Caption := dlgMouseFoldButton;
  MouseRadioGroup2.Items.add(dlgMouseFoldButtonLeft);
  MouseRadioGroup2.Items.add(dlgMouseFoldButtonRight);
  MouseRadioGroup2.Items.add(dlgMouseFoldButtonMiddle);
  MouseShift2.Caption := dlgMouseFoldModifierShift;
  MouseCtrl2.Caption := dlgMouseFoldModifierCtrl;
  MouseAlt2.Caption := dlgMouseFoldModifierAlt;
end;

procedure TMouseGroupBox1.ReadSettings(
  AOptions: TAbstractIDEOptions);
var
  x: TSynGutterFoldClickType;
begin
  with AOptions as TEditorOptions do
  begin
    for x := low(TSynGutterFoldClickType) to high(TSynGutterFoldClickType) do begin
      FExpandedClickConf[x] := ExpandedClickConf[x];
      FCollapsedClickConf[x] := CollapsedClickConf[x];
    end;
  end;
  MouseConfListBox.ItemIndex := 0;
  MouseConfListBoxClick(nil);
end;

procedure TMouseGroupBox1.WriteSettings(
  AOptions: TAbstractIDEOptions);
var
  x: TSynGutterFoldClickType;
begin
  with AOptions as TEditorOptions do
  begin
    for x := low(TSynGutterFoldClickType) to high(TSynGutterFoldClickType) do begin
      ExpandedClickConf[x] := FExpandedClickConf[x];
      CollapsedClickConf[x] := FCollapsedClickConf[x];
    end;
  end;
end;

class function TMouseGroupBox1.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  {$I editor_codefolding_mouse_options.lrs}
  RegisterIDEOptionsEditor(GroupEditor, TMouseGroupBox1,
                           EdtOptionsCodeFoldingMouse, EdtOptionsCodeFolding);
end.

