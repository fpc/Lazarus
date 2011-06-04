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


  author: Alexandru Alexandrov
  date: 11.06.2005

}

unit fieldslist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, DB, StdCtrls, ObjInspStrConsts, ComponentEditors, PropEdits;

type

  { TFieldsListFrm }

  TFieldsListFrm = class(TForm)
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    ListBox1: TListBox;
    procedure BitBtnOkClick(Sender: TObject);
  private
    { private declarations }
    FDesigner: TComponentEditorDesigner;
    LinkDataset: TDataset;
    procedure RefreshFieldsList;
  public
    { public declarations }
    constructor Create(AOwner: TComponent; ADataset: TDataset;
      ADesigner: TComponentEditorDesigner); reintroduce;
  end;

var
  FieldsListFrm: TFieldsListFrm;

implementation

{$R *.lfm}

{ TFieldsListFrm }

procedure TFieldsListFrm.BitBtnOkClick(Sender: TObject);
var
  i: integer;
  NewField: TField;
  fModified: boolean;
  PreActive: boolean;
  FieldDef: TFieldDef;

  function FieldNameToPascalIdentifer(const AName: string): string;
  var
    i : integer;
  begin
    Result := '';
    // FieldName is an ansistring
    for i := 1 to Length(AName) do
      if AName[i] in ['0'..'9','a'..'z','A'..'Z','_'] then
        Result := Result + AName[i];
    if (Length(Result) > 0) and (not (Result[1] in ['0'..'9'])) then
        Exit;
    if Assigned(FieldDef.FieldClass) then
    begin
      Result := FieldDef.FieldClass.ClassName + Result;
      if Copy(Result, 1, 1) = 'T' then
        Result := Copy(Result, 2, Length(Result) - 1);
    end
    else
      Result := 'Field' + Result;
  end;

  function CreateFieldName(Owner: TComponent; const AName: string): string;
  var
    j:integer;
  begin
    for j := 0 to Owner.ComponentCount - 1 do
    begin
      if CompareText(Owner.Components[j].Name, AName) = 0 then
      begin
        Result := FDesigner.CreateUniqueComponentName(LinkDataset.Name +
          FieldNameToPascalIdentifer(NewField.FieldName));
        exit;
      end;
    end;
    Result := AName;
  end;

begin
  LinkDataset.DisableControls;
  try
    PreActive := LinkDataset.Active;
    try
      LinkDataSet.Active := False;
      fModified := False;
      for i := 0 to ListBox1.Items.Count - 1 do 
      begin
        if ListBox1.Selected[i] and (LinkDataset.FindField(ListBox1.Items[i]) = nil) then
        begin
          FieldDef := LinkDataset.FieldDefs.Find(ListBox1.Items[i]);
          if FieldDef = nil then
            Continue;
          NewField := FieldDef.CreateField(LinkDataset.Owner);
          NewField.Name := CreateFieldName(LinkDataset.Owner, LinkDataset.Name +
            FieldNameToPascalIdentifer(NewField.FieldName));
          FDesigner.PropertyEditorHook.PersistentAdded(NewField, True);
          fModified := True;
        end;
      end;
      if fModified then FDesigner.Modified;
    finally
      if PreActive then
        LinkDataset.Active:=True;
    end;    
  finally
    LinkDataset.EnableControls;  
  end;  
end;

procedure TFieldsListFrm.RefreshFieldsList;

  function CheckField(f: TFieldDef): boolean;
  begin
    Result := Assigned(f) and (LinkDataSet.FindField(f.Name) = nil);
  end;
  
  function FillList: integer;
  var
    i: integer;
    f: TFieldDef;
  begin
    Result := 0;
    with LinkDataset do
    begin
      for i := 0 to FieldDefs.Count - 1 do
      begin
        f := FieldDefs.Items[i];
        if CheckField(f) then
        begin
          ListBox1.Items.Add(f.Name);
          inc(Result);
        end;
      end;
    end;
  end;
  
var
  i: integer;
  PreActive: boolean;
begin
  i := 0;
  ListBox1.Clear;
  BitBtnOk.Enabled := False;
  if not Assigned(LinkDataset) then Exit;
  // refresh fielddefs
  LinkDataset.FieldDefs.Update;
  PreActive:=LinkDataset.Active;
  LinkDataset.Active := False;
  try
    i := FillList;
    BitBtnOk.Enabled := i > 0;
  finally
    if PreActive then
      LinkDataset.Active:=True;
  end;    
end;

constructor TFieldsListFrm.Create(AOwner: TComponent; ADataset: TDataset;
      ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AOwner);
  LinkDataset := ADataset;
  if not Assigned(LinkDataset) then
    ShowMessage('LinkDataset = nil!')
  else
  begin
    FDesigner := ADesigner;
    Caption := fesFlTitle + ' - ' + LinkDataset.Name;
  end;
  RefreshFieldsList;
end;

end.

