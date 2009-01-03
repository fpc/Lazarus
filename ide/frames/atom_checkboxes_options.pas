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
unit atom_checkboxes_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, IDEOptionsIntf, SourceChanger, StdCtrls;

type
  { TCodetoolsAtomCheckboxesOptionsFrame }

  TCodetoolsAtomCheckboxesOptionsFrame = class(TAbstractIDEOptionsEditor)
  protected
    procedure CreateAtomCheckBoxes(
      ParentGroupBox: TGroupBox; AtomTypes: TAtomTypes; Columns: integer;
      AOnClick: TNotifyEvent);
    function ReadAtomCheckBoxes(ParentGroupBox: TGroupBox): TAtomTypes;
    procedure SetAtomCheckBoxes(AtomTypes: TAtomTypes; ParentGroupBox: TGroupBox);
  end;

implementation

uses
  CodeToolsOptions, SysUtils;

{ TCodetoolsAtomCheckboxesOptionsFrame }

procedure TCodetoolsAtomCheckboxesOptionsFrame.CreateAtomCheckBoxes(
  ParentGroupBox: TGroupBox; AtomTypes: TAtomTypes; Columns: integer;
  AOnClick: TNotifyEvent);
var
  Count, i, yi, MaxYCount: integer;
  a: TAtomType;
  X, Y, CurX, CurY, XStep, YStep: integer;
  NewCheckBox: TCheckBox;
begin
  if Columns < 1 then
    Columns := 1;
  Count := 0;
  for a := Low(TAtomTypes) to High(TAtomTypes) do
    if a in AtomTypes then
      inc(Count);
  if Count = 0 then
    Exit;

  MaxYCount := (Count + Columns - 1) div Columns;
  X := 6;
  Y := 1;
  XStep := (ParentGroupBox.ClientWidth - 10) div Columns;
  YStep := (ParentGroupBox.ClientHeight - 20) div MaxYCount;
  CurX := X;
  CurY := Y;
  i := 0;
  yi := 0;

  for a := Low(TAtomTypes) to High(TAtomTypes) do
  begin
    if a in AtomTypes then
    begin
      inc(i);
      inc(yi);
      NewCheckBox := TCheckBox.Create(ParentGroupBox);
      with NewCheckBox do
      begin
        Name := ParentGroupBox.Name + 'CheckBox' + IntToStr(i + 1);
        Parent := ParentGroupBox;
        SetBounds(CurX, CurY, XStep - 10, Height);
        Caption := GetTranslatedAtomTypes(a);
        OnClick := AOnClick;
        Visible := true;
      end;
      if yi >= MaxYCount then
      begin
        inc(X, XStep);
        CurX := X;
        CurY := Y;
        yi := 0;
      end
      else
        inc(CurY,YStep);
    end;
  end;
end;

function TCodetoolsAtomCheckboxesOptionsFrame.ReadAtomCheckBoxes(
  ParentGroupBox: TGroupBox): TAtomTypes;
var
  i: integer;
  ACheckBox: TCheckBox;
  a: TAtomType;
begin
  Result := [];
  for i := 0 to ParentGroupBox.ComponentCount - 1 do
  begin
    if ParentGroupBox.Components[i] is TCheckBox then
    begin
      ACheckBox := TCheckBox(ParentGroupBox.Components[i]);
      a := TranslatedAtomToType(ACheckBox.Caption);
      if (a <> atNone) and ACheckBox.Checked then
        Include(Result, a);
    end;
  end;
end;

procedure TCodetoolsAtomCheckboxesOptionsFrame.SetAtomCheckBoxes(
  AtomTypes: TAtomTypes; ParentGroupBox: TGroupBox);
var
  i: integer;
  ACheckBox: TCheckBox;
  a: TAtomType;
begin
  for i := 0 to ParentGroupBox.ComponentCount - 1 do
  begin
    if ParentGroupBox.Components[i] is TCheckBox then
    begin
      ACheckBox:=TCheckBox(ParentGroupBox.Components[i]);
      a := TranslatedAtomToType(ACheckBox.Caption);
      ACheckBox.Checked := (a <> atNone) and (a in AtomTypes);
    end;
  end;
end;

end.
