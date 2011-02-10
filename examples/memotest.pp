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
program MemoTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Buttons, Classes, Forms, StdCtrls, SysUtils;

type
  TMemoTestForm = class(TForm)
  public
    Button1, Button2, Button3, Button4, Button5, Button6:   TButton;
    Memo1, Memo2:  TMemo;
    MyLabel: TLabel;
    Edit1: TEdit;
    constructor Create(AOwner: TComponent); override;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  end;

var
  MemoTestForm: TMemoTestForm;

{------------------------------------------------------------------------------}
{  TMemoTestorm                                          }
{------------------------------------------------------------------------------}
constructor TMemoTestForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 350;
  Height := 245;
  Left := 200;
  Top := 200;

  // create children
  Button5 := TButton.Create(Self);
  Button5.OnClick := @button5click;
  Button5.Parent := Self;
  Button5.left := 10;
  Button5.top := 210;
  Button5.width := 50;
  Button5.height := 25;
  Button5.caption := 'Add';
  Button5.Show;

  Button3 := TButton.Create(Self);
  Button3.OnClick := @Button3Click;
  Button3.Parent := Self;
  Button3.left := 65;
  Button3.top := 210;
  Button3.width := 50;
  Button3.height := 25;
  Button3.caption := 'Clear 1';
  Button3.Show;

  Button1 := TButton.Create(Self);
  Button1.OnClick := @Button1Click;
  Button1.Parent := Self;
  Button1.left := 120;
  Button1.top :=  210;
  Button1.width := 50;
  Button1.height := 25;
  Button1.caption := '->';
  Button1.Show;

  Button2 := TButton.Create(Self);
  Button2.OnClick := @Button2Click;
  Button2.Parent := Self;
  Button2.left := 175;
  Button2.top := 210;
  Button2.width := 50;
  Button2.height := 25;
  Button2.caption := '<-';
  Button2.Show;

  Button4 := TButton.Create(Self);
  Button4.OnClick := @button4click;
  Button4.Parent := Self;
  Button4.left := 230;
  Button4.top := 210;
  Button4.width := 50;
  Button4.height := 25;
  Button4.caption := 'Clear 2';
  Button4.Show;
  
  Button6 := TButton.Create(Self);
  Button6.OnClick := @button6click;
  Button6.Parent := Self;
  Button6.left := 285;
  Button6.top := 210;
  Button6.width := 50;
  Button6.height := 25;
  Button6.caption := 'Add';
  Button6.Show;

  Edit1 := TEdit.Create(Self);
  Edit1.Parent := Self;
  Edit1.Top := 180;
  Edit1.Height := 25;
  Edit1.Left := 10;
  Edit1.Width := 325;
  Edit1.Visible := True;

  MyLabel := TLabel.Create(Self);
  with MyLabel
  do begin
    Parent := Self;
    Top := 1;
    Left := 10;
    Width := 150;
    Height := 16;
    Caption := 'These are 2 TMemo:';
    Show;
  end;

  Memo1 := TMemo.Create(Self);
  with Memo1
  do begin
    WordWrap := True;
    Parent := Self;
    Left := 10;
    Top := 20;
    Width := 160;
    Height := 155;
    Scrollbars := ssVertical;
    Show;
  end;

  Memo2 := TMemo.Create(Self);
  with Memo2
  do begin
    WordWrap := False;
    Parent := Self;
    Left := 175;
    Top := 20;
    Width := 160;
    Height := 155;
    Scrollbars := ssBoth;
    Show;
  end;
end;

procedure TMemoTestForm.Button1Click(Sender: TObject);
begin
  Memo2.Text := Memo1.Text;
end;

procedure TMemoTestForm.Button2Click(Sender: TObject);
begin
  Memo1.Text := Memo2.Text;
end;

procedure TMemoTestForm.Button3Click(Sender: TObject);
begin
  Memo1.Text := '';
end;

procedure TMemoTestForm.Button4Click(Sender: TObject);
begin
  Memo2.Text := '';
end;

procedure TMemoTestForm.Button5Click(Sender: TObject);
begin
  Memo1.Append(Edit1.Text);
end;

procedure TMemoTestForm.Button6Click(Sender: TObject);
begin
  Memo2.Append(Edit1.Text);
end;

begin
   Application.Initialize;
   Application.CreateForm(TMemoTestForm, MemoTestForm);
   Application.Run;
end.
