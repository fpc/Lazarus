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
unit bitbtnform;

{$mode objfpc}
{$H+}

interface

uses
  Interfaces, Classes, Forms, Buttons, cListbox, StdCtrls, Controls;

type
   TForm1 = class(TForm)
   private
     { Private Declarations }
   protected
     { Protected Declarations }
   public
      button1 : TBitBtn;
      Label1 : TLabel;
      Radio1 : TRadioButton;
      Radio2 : TRadioButton;
      Radio3 : TRadioButton;
      Radio4 : TRadioButton;
      Radio5 : TRadioButton;
      constructor Create(AOwner: TComponent); override;
      procedure button1MouseDown(Sender: TObject; Button: TMouseButton;
                                 Shift: TShiftState; X, Y: Integer);
      procedure button1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure button1MouseUp(Sender: TObject; Button: TMouseButton;
                                 Shift: TShiftState; X, Y: Integer);
      procedure button1Enter(Sender : TObject);
      procedure FormDestroy(Sender : TObject);
      procedure Radio1Click(Sender : TObject);
      procedure Radio2Click(Sender : TObject);
      procedure Radio3Click(Sender : TObject);
      procedure Radio4Click(Sender : TObject);
   end;

var
   Form1 : TForm1;

implementation

constructor TForm1.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   Caption := 'TBitBtn Verify';
   Width := 200;
   Height := 150;
   Left := 200;
   Top := 200;

   OnDestroy := @FormDestroy;

   button1 := TBitBtn.Create(Self);
   With button1 do
   begin
     OnMouseUp:= @button1MouseUp;
     OnEnter := @button1Enter;
     OnMouseDown := @button1MouseDown;
     OnMouseMove := @button1MouseMove;
     Parent := Self;
     width := 80;
     height := 32;
     left := 25;
     top := 25;
     layout := blGlyphLeft;
     kind := bkClose;
     caption := 'Close';
     Show;
   end;
   
   Label1 := TLabel.Create(Self);
   With Label1 do
   begin
     Parent := Self;
     width := 80;
     left := 25;
     top := 80;
     Caption := 'bkClose';
     Autosize := True;
     Show;
   end;

   Radio1 := TRadioButton.Create(Self);
   with Radio1 do
   begin
     OnClick := @Radio1Click;
     Parent := Self;
     top := 25;
     left := 125;
     caption := 'Close';
     Checked := True;
     Height := 15;
     Width := 60;
     Show;
   end;

   Radio2 := TRadioButton.Create(Self);
   with Radio2 do
   begin
     OnClick := @Radio2Click;
     Parent := Self;
     top := 50;
     left := 125;
     caption := 'Ok';
     Checked := False;
     Height := 15;
     Width := 50;
     Show;
   end;

   Radio3 := TRadioButton.Create(Self);
   with Radio3 do
   begin
     OnClick := @Radio3Click;
     Parent := Self;
     top := 75;
     left := 125;
     caption := 'Cancel';
     Checked := False;
     Height := 15;
     Width := 65;
     Show;
   end;

   Radio4 := TRadioButton.Create(Self);
   with Radio4 do
   begin
     OnClick := @Radio4Click;
     Parent := Self;
     top := 100;
     left := 125;
     caption := 'Help';
     Checked := False;
     Height := 15;
     Width := 55;
     Show;
   end;

end;

procedure TForm1.FormDestroy(Sender : TObject);
begin
//   Application.Terminate;
end;

procedure TForm1.button1MouseDown(Sender: TObject; Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);
begin
  Label1.Caption := 'Button1.Down';
end;

procedure TForm1.button1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Label1.Caption := 'Button1.Move';
end;

procedure TForm1.button1MouseUp(Sender: TObject; Button: TMouseButton;
                                 Shift: TShiftState; X, Y: Integer);
begin
  Label1.Caption := 'Button1.Up';
end;

procedure TForm1.button1Enter(Sender : TObject);
begin
  //
end;

procedure TForm1.Radio1Click(Sender : TObject);
begin
  button1.Kind := bkClose;
  Label1.Caption := 'bkClose';
end;

procedure TForm1.Radio2Click(Sender : TObject);
begin
  button1.Kind := bkOk;
  Label1.Caption := 'bkOk';
end;

procedure TForm1.Radio3Click(Sender : TObject);
begin
  button1.Kind := bkCancel;
  Label1.Caption := 'bkCancel';
end;

procedure TForm1.Radio4Click(Sender : TObject);
begin
  button1.Kind := bkHelp;
  Label1.Caption := 'bkHelp';
end;

end.

