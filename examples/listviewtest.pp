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
  Interfaces, Classes, Buttons, ComCtrls, Forms, SysUtils;

type
  TMyForm = class(TForm)
  private
    FItemIndex: Cardinal;
  public
    ListView: TListView;
    Button1: TButton;
    constructor Create(AOwner: TComponent); override;
    procedure Button1Click(Sender: TObject);
  end;

var
  MyForm: TMyForm;

constructor TMyForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Caption := 'List View Test';
  Width := 175;
  Height := 195;

  ListView := TListView.Create(Self);
  ListView.Parent := Self;
  ListView.Height := 120;
  ListView.Width := 150;
  ListView.Show;
  
  Button1 := TButton.Create(Self);
  with Button1 do
  begin
    Parent := Self;
    Caption := 'Add Item';
    Top := 130;
    Left := 10;
    Height := 25;
    Width := 65;
    OnClick := @Button1Click;
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
  Item.Caption := Format('Item %D', [FItemIndex]);
end;

begin
  Application.Initialize;
  Application.CreateForm(TMyForm, MyForm);
  Application.Run;
end.

{
  $Log$
  Revision 1.4  2002/10/29 08:22:32  lazarus
  MG: added interfaces unit

  Revision 1.3  2002/05/10 06:57:50  lazarus
  MG: updated licenses

  Revision 1.2  2002/02/08 00:44:06  lazarus
  Keith: Fixed form size for listviewtest.pp, kind changing for scrollbar.pp

  Revision 1.1  2002/02/04 10:54:33  lazarus
  Keith:
    * Fixes for Win32
    * Added new listviewtest.pp example


}
