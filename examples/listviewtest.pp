{
 /***************************************************************************
                               ListViewTest.pp
                             -------------------
                           Test aplication for list views
                   Initial Revision  : Sun Dec 31 17:30:00:00 CET 2000




 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}

program ListViewTest;

uses
  Classes, Buttons, ComCtrls, Forms, SysUtils;

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
  Width := 200;
  Height := 300;

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
  Revision 1.1  2002/02/04 10:54:33  lazarus
  Keith:
    * Fixes for Win32
    * Added new listviewtest.pp example


}