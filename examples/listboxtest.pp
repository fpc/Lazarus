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
program ListBoxTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Buttons, Classes, Forms, StdCtrls, SysUtils, Controls;

type
  TListBoxTestForm = class(TForm)
  public
    Button1, Button2, Button3, Button4:   TButton;
    ListBox:  TListBox;
    constructor Create(AOwner: TComponent); override;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
  end;

var
  ListBoxTestForm: TListBoxTestForm;

{------------------------------------------------------------------------------}
{  TListBoxTestForm                                          }
{------------------------------------------------------------------------------}
constructor TListBoxTestForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 300;
  Height := 200;
  Left := 200;
  Top := 200;

  // create children
  Button1 := TButton.Create(Self);
  Button1.OnClick := @Button1Click;
  Button1.Parent := Self;
  Button1.left := 40;
  Button1.top :=  170;
  Button1.width := 50;
  Button1.height := 25;
  Button1.caption := 'New';
  Button1.Show;

  Button2 := TButton.Create(Self);
  Button2.OnClick := @Button2Click;
  Button2.Parent := Self;
  Button2.left := 95;
  Button2.top := 170;
  Button2.width := 50;
  Button2.height := 25;
  Button2.caption := 'Delete';
  Button2.Show;

  Button3 := TButton.Create(Self);
  Button3.OnClick := @Button3Click;
  Button3.Parent := Self;
  Button3.left := 150;
  Button3.top := 170;
  Button3.width := 50;
  Button3.height := 25;
  Button3.caption := 'Clear';
  Button3.Show;

  Button4 := TButton.Create(Self);
  Button4.OnClick := @button4click;
  Button4.Parent := Self;
  Button4.left := 205;
  Button4.top := 170;
  Button4.width := 50;
  Button4.height := 25;
  Button4.caption := 'Unused';
  Button4.Show;

//  ListBox := TCListBox.Create(Self);
  ListBox := TListBox.Create(Self);
  ListBox.Parent := Self;
  ListBox.Left := 10;
  ListBox.Top := 10;
  ListBox.Width := 280;
  ListBox.Height := 150;
  ListBox.Anchors := [akLeft, akRight, akTop];
{  ListBox.ExtendedSelect := true;
  ListBox.MultiSelect := true;
 } ListBox.Show;
 
  OnResize := @FormResize;
end;

procedure TListBoxTestForm.Button1Click(Sender: TObject);
var
  Index: integer;
begin
  Index := ListBox.ItemIndex;
  if Index = -1 then
    ListBox.Items.Add('Button 1 clicked')
  else 
    ListBox.Items.Insert(Index, 'Button 1 clicked at '+IntToStr(Index));
  for Index := 0 to ListBox.Items.Count - 1 do 
    ListBox.Items.Objects[Index] := TObject(PtrInt(Index));
end;

procedure TListBoxTestForm.Button2Click(Sender: TObject);
var
  Index: integer;
begin
  Index := ListBox.ItemIndex;
  if Index <> -1
  then ListBox.Items.Delete(Index);
end;

procedure TListBoxTestForm.Button3Click(Sender: TObject);
begin
  ListBox.Items.Clear;
end;

procedure TListBoxTestForm.Button4Click(Sender: TObject);
var
  X: PtrInt;
begin
  X := PtrInt(ListBox.Items.Objects[ListBox.ItemIndex]);
  writeln('TListBoxTestForm.Button4Click ',X);
end;

procedure TListBoxTestForm.FormResize(Sender: TObject);
begin                      
  Caption := Format('%dx%d', [ListBox.Width, ListBox.Height]);
end;

begin
   Application.Initialize;
   Application.CreateForm(TListBoxTestForm, ListBoxTestForm);
   Application.Run;
end.
