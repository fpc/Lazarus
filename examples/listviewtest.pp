{
 /***************************************************************************
                               ListViewTest.pp
                             -------------------
                           Test aplication for list views
                   Initial Revision  : Sun Dec 31 17:30:00:00 CET 2000




 ***************************************************************************/

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

program ListViewTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Classes, Buttons, Controls, ComCtrls, Forms, SysUtils, 
  Graphics, StdCtrls;

type
  TMyForm = class(TForm)
  private
    FItemIndex: Cardinal;          
  public
    ListView: TListView;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    constructor Create(AOwner: TComponent); override;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
  end;

var
  MyForm: TMyForm;

constructor TMyForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Caption := 'List View Test';
  Width := 300;
  Height := 200;

  ListView := TListView.Create(Self);
  ListView.Parent := Self;
  ListView.Height := 150;
  ListView.Align := alTop;
  ListView.ViewStyle := vsReport;
  ListView.Show;
  
  ListView.Columns.Add.Caption := 'Column 1';
  ListView.Columns.Add.Caption := 'Column 2';
  ListView.Columns.Add.Caption := 'Column 3';
  
  Button1 := TButton.Create(Self);
  with Button1 do
  begin
    Parent := Self;
    Caption := 'Add Item';
    Top := 160;
    Left := 10;
    Height := 25;
    Width := 65;
    OnClick := @Button1Click;
    Show;
  end;

  Button2 := TButton.Create(Self);
  with Button2 do
  begin
    Parent := Self;
    Caption := 'Del Item';
    Top := 160;
    Left := 80;
    Height := 25;
    Width := 65;
    OnClick := @Button2Click;
    Show;
  end;
  
  Edit1 := TEdit.Create(Self);
  with Edit1 do
  begin
    Parent := Self;
    Top := 160;
    Left := 150;
    Height := 25;
    Width := 65;
    OnChange := @Edit1Change;
    Show;
  end;
  
  Edit2 := TEdit.Create(Self);
  with Edit2 do
  begin
    Parent := Self;
    Top := 160;
    Left := 220;
    Height := 25;
    Width := 65;
    OnChange := @Edit2Change;
    Show;
  end;
  
  Show;
end;

procedure TMyForm.Button1Click(Sender: TObject);
var
  Item: TListItem;
begin                                        
  Inc(FItemIndex);
  Item := ListView.Items.Add;
  Item.Caption := Format('Item %d', [FItemIndex]);
  Item.SubItems.Add(Format('Sub %d.1', [FItemIndex]));
  Item.SubItems.Add(Format('Sub %d.2', [FItemIndex]));
end;

procedure TMyForm.Button2Click(Sender: TObject);
begin
  ListView.Selected.Free;
end;

procedure TMyForm.Edit1Change(Sender: TObject);
begin
  if ListView.Selected = nil then Exit;
  ListView.Selected.Caption := Edit1.Text;
end;

procedure TMyForm.Edit2Change(Sender: TObject);
begin
  if ListView.Selected = nil then Exit;
  ListView.Selected.SubItems[0] := Edit2.Text;
end;

begin
  Application.Initialize;
  Application.CreateForm(TMyForm, MyForm);
  Application.Run;
end.

