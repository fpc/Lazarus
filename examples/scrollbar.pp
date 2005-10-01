{
 /***************************************************************************
                          Scrollbar - example
                          ------------------

                   Initial Revision  : Thursday Feb 01 2001

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
program Scrollbar;

{$mode objfpc}{$H+}

uses
  Interfaces, Classes, StdCtrls, Forms, Buttons, Menus, ComCtrls,
  SysUtils, ExtCtrls, Controls;

type
  TForm1 = class(TFORM)
    Scrollbar1 : TScrollbar;
    Button1 : TButton;
    Button2 : TButton;
    Button3 : TButton;
    Button4 : TButton;
    Procedure Button1Clicked(sender : tobject);
    Procedure Button2Clicked(sender : tobject);
    Procedure Button3Clicked(sender : tobject);
    Procedure Button4Clicked(sender : tobject);
    Procedure scrollbar1Changed(sender : tobject);
    procedure Scrollbar1OnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
  public
    constructor Create(AOwner: TComponent); override;	
    Procedure FormKill(Sender : TObject);
  protected
  end;

var
  Form1 : TForm1;

constructor TForm1.Create(AOwner: TComponent);	
begin
   inherited Create(AOwner);
   Caption := 'Scrollbar Demo v0.1';
   Height := 350;

   ScrollBar1 := TSCrollBar.Create(self);
   with SCrollbar1 do
    Begin
      Parent := self;
      Kind := sbVertical;
      Left := 100;
      Top := 50;
      Width := 15;
      Height := 300;
     // align := alRight;
      visible := True;
      OnChange := @scrollbar1Changed;
      OnScroll := @ScrollBar1OnScroll;
    end;

  Button1 := TButton.create(self);
  with Button1 do
      begin
     Parent := self;
     Visible := True;
     Caption := 'Button1';
     onclick := @button1clicked;
     Width := 65;
      end;

  Button2 := TButton.create(self);
  with Button2 do
      begin
     Parent := self;
     Left := 65;
     Visible := True;
     Caption := 'Button2';
     onclick := @button2clicked;
     Width := 65;
      end;
  Button3 := TButton.create(self);
  with Button3 do
      begin
     Parent := self;
     Left := 130;
     Visible := True;
     Caption := 'Button3';
     onclick := @button3clicked;
     Width := 65;
      end;
  Button4 := TButton.create(self);
  with Button4 do
      begin
     Parent := self;
     Left := 195;
     Visible := True;
     Caption := 'Button4';
     onclick := @button4clicked;
     Width := 65;
      end;

end;


procedure TForm1.FormKill(Sender : TObject);
Begin

End;

Procedure TForm1.Button1Clicked(sender : tobject);
Begin
Writeln('[Button1cvlikced]');
if ScrollBar1.Kind = sbHorizontal then
    Scrollbar1.Kind := sbVertical else
    Scrollbar1.kind := sbHorizontal;
 
end;

Procedure TForm1.Button2Clicked(sender : tobject);
Begin
ScrollBar1.Min := ScrollBar1.Min + 20;
end;

Procedure TForm1.Button3Clicked(sender : tobject);
Begin
ScrollBar1.Max := ScrollBar1.Max + 25;
end;

Procedure TForm1.Button4Clicked(sender : tobject);
Begin
Scrollbar1.Position := ScrollBar1.Position + 5;
end;

Procedure TForm1.scrollbar1Changed(sender : tobject);
Begin
Caption := 'Scrollbar Demo - Position = '+Inttostr(Scrollbar1.Position);
End;



Procedure TForm1.Scrollbar1OnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
Begin
Writeln('.............Scrolled...............');
End;

begin
   Application.Initialize; { calls InitProcedure which starts up GTK }
   Application.CreateForm(TForm1, Form1);
   Application.Run;
end.

