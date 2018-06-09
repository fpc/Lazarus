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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit atom_checkboxes_options;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  // LCL
  StdCtrls,
  // CodeTools
  SourceChanger,
  // IdeIntf
  IDEOptEditorIntf;

type
  { TCodetoolsAtomCheckboxesOptionsFrame }

  TCodetoolsAtomCheckboxesOptionsFrame = class(TAbstractIDEOptionsEditor)
  protected
    procedure CreateAtomCheckBoxes(
      ParentGroupBox: TGroupBox; AtomTypes: TAtomTypes; {%H-}Columns: integer;
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
  a: TAtomType;
  NewCheckBox: TCheckBox;
  i: Integer;
begin
  i:=0;
  for a := Low(TAtomTypes) to High(TAtomTypes) do
  begin
    if a in AtomTypes then
    begin
      inc(i);
      NewCheckBox := TCheckBox.Create(ParentGroupBox);
      with NewCheckBox do
      begin
        Name := ParentGroupBox.Name + 'CheckBox' + IntToStr(i + 1);
        Parent := ParentGroupBox;
        Caption := GetTranslatedAtomTypes(a);
        OnClick := AOnClick;
        Tag := ord(a);
      end;
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
      a := TAtomType(ACheckBox.Tag);
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
      a := TAtomType(ACheckBox.Tag);
      ACheckBox.Checked := (a <> atNone) and (a in AtomTypes);
    end;
  end;
end;

end.
