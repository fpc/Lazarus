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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, DB, StdCtrls, ComponentEditors, PropEdits;

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

resourcestring
  rsTitle = 'FieldDefs';
  
{ TFieldsListFrm }

procedure TFieldsListFrm.BitBtnOkClick(Sender: TObject);
var i: integer;
    NewField: TField;
    fModified: boolean;
begin
  LinkDataSet.Active := False;
  fModified := False;
  for i := 0 to ListBox1.Items.Count - 1 do begin
    if ListBox1.Selected[i] And (LinkDataset.FindField(ListBox1.Items[i]) = Nil) then begin
      NewField := TFieldDef(ListBox1.Items.Objects[i]).CreateField(LinkDataset.Owner);
      NewField.Name := FDesigner.CreateUniqueComponentName(LinkDataset.Name + NewField.FieldName);
      FDesigner.PropertyEditorHook.PersistentAdded(NewField, True);
      fModified := True;
    end;
  end;
  if fModified then FDesigner.Modified;
end;

procedure TFieldsListFrm.RefreshFieldsList;

  function CheckField(f: TFieldDef): boolean;
  begin
    Result := Assigned(f) And (LinkDataSet.FindField(f.Name) = Nil);
  end;
  
  function FillList: integer;
  var
    i: integer;
    f: TFieldDef;
  begin
    Result := 0;
    with LinkDataset do begin
      for i := 0 to FieldDefs.Count - 1 do begin
        f := FieldDefs.Items[i];
        if CheckField(f) then begin
          ListBox1.Items.AddObject(f.Name, f);
          inc(Result);
        end;
      end;
    end;
  end;
  
var i: integer;
begin
  i := 0;
  ListBox1.Clear;
  BitBtnOk.Enabled := False;
  if Not Assigned(LinkDataset) then Exit;
  with LinkDataset do begin
    Active := False;
    FieldDefs.Update;
  end;
  i := FillList;
  BitBtnOk.Enabled := i > 0;
end;

constructor TFieldsListFrm.Create(AOwner: TComponent; ADataset: TDataset;
      ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AOwner);
  LinkDataset := ADataset;
  if Not Assigned(LinkDataset) then ShowMessage('LinkDataset = nil!')
  else begin
    FDesigner := ADesigner;
    Caption := rsTitle + ' - ' + LinkDataset.Name;
  end;
  RefreshFieldsList;
end;

initialization
  {$I fieldslist.lrs}

end.

