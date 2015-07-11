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
unit frmimportdd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CheckLst,
  Buttons, ActnList,fpdatadict, StdCtrls, lazdatadeskstr;

type

  { TImportDDform }

  TImportDDform = class(TForm)
    ASelectNone: TAction;
    ASelectAll: TAction;
    ActionList1: TActionList;
    BOK: TButton;
    BCancel: TButton;
    BSelectNone: TButton;
    BSelectAll: TButton;
    CBUpdateExisting: TCheckBox;
    LBItems: TCheckListBox;
    procedure DoSelection(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HaveItems(Sender: TObject);
  private
    { private declarations }
    procedure SelectItems(DoSelect : Boolean);
  public
    { public declarations }
  end; 

var
  ImportDDform: TImportDDform;

Function GetTableList(DDE : TFPDDEngine; L : TStrings; Out UpdateExisting : Boolean) : Boolean;

implementation

{$R *.lfm}

Function GetTableList(DDE : TFPDDEngine; L : TStrings; Out UpdateExisting : Boolean) : Boolean;

Var
  I : Integer;

begin
  With TImportDDForm.Create(Application) do
    try
      DDE.GetTableList(LBItems.Items);
      Result:=(ShowModal=mrOK);
      if Result then
        begin
        For I:=0 to LBItems.Items.Count-1 do
          If LBItems.Checked[i] then
            L.Add(LBItems.Items[i]);
        UpdateExisting:=CBUpdateExisting.Checked;
        end;
    finally
      Free;
    end;
end;


{ TImportDDform }

procedure TImportDDform.HaveItems(Sender: TObject);
begin
  (Sender as TAction).Enabled:=LBItems.Items.Count>0;
end;

procedure TImportDDform.DoSelection(Sender: TObject);

begin
  SelectItems(Sender=ASelectAll);
end;

procedure TImportDDform.FormCreate(Sender: TObject);
begin
  //
  Caption:= sld_Importupdatedatadictionary;
  BSelectAll.Caption:= sld_Selectall;
  BSelectNone.Caption:= sld_Selectnone;
  CBUpdateExisting.Caption:= sld_Updateexistingtables;
  BOK.Caption:= sld_Ok;
  BCancel.Caption:= sld_Cancel;
  //
end;

procedure TImportDDform.FormShow(Sender: TObject);
begin
  SelectItems(True);
end;

procedure TImportDDform.SelectItems(DoSelect : Boolean);

Var
  I : Integer;
  
begin
  For I:=0 to LBItems.Items.Count-1 do
    LBItems.Checked[i]:=DoSelect;
end;

end.

