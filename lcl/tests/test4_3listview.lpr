{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  LCL Test 4_3

  Showing a TListView.
}
program test4_3listview;

{$mode objfpc}{$H+}

uses
  Interfaces, FPCAdds, LCLProc, LCLType, Classes, Controls, Forms, TypInfo,
  LMessages, Buttons, ExtCtrls, ComCtrls, SynEdit, SynHighlighterPas,
  Graphics;

type

  { TForm1 }

  TForm1 = class(TForm)
    ListView1: TListView;
    procedure Form1Create(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure AddListViewItem(const ACaption, SomeSubItems: string);
  end;

{ TForm1 }

procedure TForm1.Form1Create(Sender: TObject);
begin
  debugln('TForm1.Form1Create ',DbgSName(Sender));
  SetBounds(50,50,500,400);

  ListView1:=TListView.Create(Self);
  with ListView1 do begin
    Name:='ListView1';
    Align:=alTop;
    Height:=200;
    Parent:=Self;
    ViewStyle:=vsReport;
  end;
  ListView1.Columns.Add.Caption:='Name';
  ListView1.Columns.Add.Caption:='Column 2';
  ListView1.Columns.Add.Caption:='Column 3';
  AddListViewItem('First','A1'+LineEnding+'A2');
  AddListViewItem('Second','B1'+LineEnding+'B2');
  AddListViewItem('Third','C1'+LineEnding+'C2');
end;

constructor TForm1.Create(TheOwner: TComponent);
begin
  OnCreate:=@Form1Create;
  inherited Create(TheOwner);
end;

procedure TForm1.AddListViewItem(const ACaption, SomeSubItems: string);
var
  NewItem: TListItem;
begin
  NewItem:=ListView1.Items.Add;
  NewItem.Caption:=ACaption;
  NewItem.SubItems.Text:=SomeSubItems;
end;

var
  Form1: TForm1 = nil;
begin
  Application.Initialize;
  Application.CreateForm(TForm1,Form1);
  Application.Run;
end.

