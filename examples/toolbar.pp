 {
 /***************************************************************************
                          toolbar - example
                          ------------------


                   Initial Revision  : Wed Dec 29 1999

                   by Shane Miller

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
program Toolbar;

{$mode objfpc}{$H+}

uses
  Interfaces, Classes, StdCtrls, Forms, Buttons, Menus, ComCtrls,
  SysUtils, ExtCtrls, Controls;

type
        TForm1 = class(TFORM)
        public
          Toolbar1 : TToolbar;
          Toolbutton1 : TToolbutton;
          Toolbutton2 : TToolbutton;
          Toolbutton3 : TToolbutton;
          ComboBox1 : TComboBox;
          constructor Create(AOwner: TComponent); override;
        protected
        Procedure Button1Click(Sender : TObject);
        Procedure Button2Click(Sender : TObject);
        Procedure Button3Click(Sender : TObject);
        end;

var
Form1 : TForm1;

constructor TForm1.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   Name:='Form1';
   Caption := 'Toolbar Demo v0.1';
   {  set the height and width }
   Height := 350;
   Width := 700;

ToolBar1 := TToolbar.Create(Self);
  with Toolbar1 do
  begin
    Name:='ToolBar1';
    Parent := Self;
  end;

Toolbutton1 := TToolButton.Create(Toolbar1);
with ToolButton1 do
  Begin
    Name:='Toolbutton1';
    Parent := Toolbar1;
    Caption := '1';
    Style := tbsButton;
    OnClick := @Button1Click;
    Show;
  end;
Toolbutton2 := TToolButton.Create(Toolbar1);
with ToolButton2 do
  Begin
    Name:='Toolbutton2';
    Parent := Toolbar1;
    Caption := '2';
    Style := tbsButton;
    OnClick := @Button2Click;
    Show;
  end;
Toolbutton3 := TToolButton.Create(Toolbar1);
with ToolButton3 do
  Begin
    Parent := Toolbar1;
    Caption := '3';
    Style := tbsButton;
    OnClick := @Button3Click;
    Show;
  end;


ComboBox1 := TComboBox.Create(Self);
with ComboBox1 do
  Begin
    Parent := Toolbar1;
    Items.Add('Item1');
    Items.Add('Item2');
    Items.Add('Item3');
    Items.Add('Item4');
    Items.Add('Item5');
    Items.Add('Item6');
    ItemIndex := 0;
    Show;
  end;

Toolbar1.ShowCaptions := True;
Toolbar1.Height := 25;
Toolbar1.Top := 1;
Toolbar1.Width := ClientWidth;
Toolbar1.Show;


end;

Procedure TFORM1.Button1Click(Sender : TObject);
Begin
Writeln('******************');
Writeln('Toolbar button  1  clicked!');
Writeln('******************');
end;

Procedure TFORM1.Button2Click(Sender : TObject);
Begin
Writeln('******************');
Writeln('Toolbar button  2  clicked!');
Writeln('******************');
end;

Procedure TFORM1.Button3Click(Sender : TObject);
Begin
Writeln('******************');
Writeln('Toolbar button  3  clicked!');
Writeln('******************');
end;



begin
   Application.Initialize; { calls InitProcedure which starts up GTK }
   Application.CreateForm(TForm1, Form1);
   Application.Run;
end.

