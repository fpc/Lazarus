{  $Id$  }
{
 /***************************************************************************


                   Initial Revision  : Sun Mar 28 23:15:32 CST 1999


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
program GroupBox;

{$mode objfpc}{$H+}

uses
  Interfaces, Classes, StdCtrls, Forms, Buttons, Menus, ComCtrls, SysUtils;

type
  TForm1 = class(TFORM)
  public
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    grpTst : TGroupBox;
    mnuFile: TMainMenu;
    itmFileQuit: TMenuItem;
    CheckBox1: TCheckBox;
    constructor Create(AOwner: TComponent); override;
    procedure LoadMainMenu;
    procedure mnuQuitClicked(Sender : TObject);
  protected
    procedure Button1CLick(Sender : TObject);
    procedure Button2CLick(Sender : TObject);
    procedure Button3CLick(Sender : TObject);
    procedure Button4CLick(Sender : TObject);
  end;

var Form1 : TForm1;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := 'Groubox Demo v0.1';
  LoadMainMenu;
end;

procedure TForm1.Button1Click(Sender : TObject);
Begin
  if assigned (grpTst) then grpTst.Height := grpTst.Height + 10;
End;

procedure TForm1.Button2Click(Sender : TObject);
Begin
  if assigned (grpTst) then begin
    grpTst.Width := grpTst.Width + 10;
    grpTst.Show;
  end;
End;

procedure TForm1.Button3Click(Sender : TObject);
Begin
  if assigned (grpTst) then begin
    grpTst.Show;
  end;
End;

procedure TForm1.Button4Click(Sender : TObject);
Begin
  if assigned (grpTst) then begin
    grpTst.Hide;
  end;
End;

{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
procedure TForm1.LoadMainMenu;
begin
  { set the height and width }
  Height := 350;
  Width := 350;

  { Create a groupbox }
  grpTst := TGroupBox.Create(Self);
  with grpTst do begin
    Name:='grpTst';
    Parent := self;
    top := 70;
    left := 10;
    Height :=200;
    Width := 300;
    Caption := 'Groupbox with 2 Buttons';
  end;

  { Create 2 buttons inside the groupbox }
  if assigned (grpTst) then
  begin
    Button2 := TButton.Create(grpTst);
    Button2.Parent := grpTst;

    Checkbox1 := TCheckBox.Create(grpTst);
    Checkbox1.Parent := grpTst;
    Checkbox1.Name:='Checkbox1';
    Checkbox1.Left := 200;
    Checkbox1.Top := 100;
    Checkbox1.Width := 80;
    Checkbox1.Height := 30;
    Checkbox1.Caption := 'Checkbox 1';
  end
  else begin
    Button2 := TButton.Create(Self);
    Button2.Parent := Self;
  end;
  Button2.Name:='Button2';
  Button2.Left := 200;
  Button2.Top := 50;
  Button2.Width := 80;
  Button2.Height := 30;
  Button2.Caption := 'Width ++';
  Button2.OnClick := @Button2Click;


  if assigned (grpTst) then
  begin
    Button1 := TButton.Create(grpTst);
    Button1.Parent := grpTst;
  end
  else begin
    Button1 := TButton.Create(Self);
    Button1.Parent := Self;
  end;
  Button1.Name:='Button1';
  Button1.Left := 50;
  Button1.Top := 50;
  Button1.Width := 80;
  Button1.Height := 30;
  Button1.Caption := 'Height++';
  Button1.OnClick := @Button1Click;

  { Create 2 more buttons outside the groupbox }
  Button3 := TButton.Create(Self);
  Button3.Name:='Button3';
  Button3.Parent := Self;
  Button3.Left := 50;
  Button3.Top := 30;
  Button3.Width := 80;
  Button3.Height := 30;
  Button3.Caption := 'Show';
  Button3.OnClick := @Button3Click;

  Button4 := TButton.Create(Self);
  Button4.Name:='Button4';
  Button4.Parent := Self;
  Button4.Left := 200;
  Button4.Top := 30;
  Button4.Width := 80;
  Button4.Height := 30;
  Button4.Caption := 'Hide';
  Button4.OnClick := @Button4Click;

  mnuFile := TMainMenu.Create(Self);

  itmFileQuit := TMenuItem.Create(Self);
  itmFileQuit.Caption := 'Quit';
  itmFileQuit.OnClick := @mnuQuitClicked;
  mnuFile.Items.Add(itmFileQuit);
end;

{------------------------------------------------------------------------------}
procedure TForm1.mnuQuitClicked(Sender : TObject);
begin
  Close;
end;
{------------------------------------------------------------------------------}

begin
  Application.Initialize; { calls InitProcedure which starts up GTK }
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

