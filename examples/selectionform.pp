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
unit SelectionForm;

interface

{$mode objfpc}{$H+}

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ControlSelection, StdCtrls, Buttons;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    procedure AddSelClick(Sender: TObject);
    procedure Button8MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Button8MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Button8MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FSelection: TControlSelection;
    FDown:  Boolean;
    FStart: TPoint;
    procedure SelChange(Sender: TObject);
  public
    constructor Create(Owner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation


constructor TForm1.Create(Owner: TComponent); 
begin
  inherited Create(Owner);

  Left := 154;
  Top := 117;
  Width := 500;
  Height := 400;
  Caption := 'Selection Test';
  
  Button1 := TButton.Create(Self);
  with Button1 do 
  begin
    Parent := Self;
    Visible := True;
    Left := 44;
    Top := 128;
    Width := 265;
    Height := 25;
    Caption := 'Button1';
    TabOrder := 0;
    OnClick := AddSelClick;
  end;
  
  Button2 := TButton.Create(Self);
  with Button2 do 
  begin
    Parent := Self;
    Visible := True;
    Left := 212;
    Top := 184;
    Width := 221;
    Height := 105;
    Caption := 'Button2 (Allways selected)';
    TabOrder := 1;
  end;
  
  Button3 := TButton.Create(Self);
  with Button3 do 
  begin
    Parent := Self;
    Visible := True;
    Left := 148;
    Top := 92;
    Width := 75;
    Height := 25;
    Caption := 'Button3';
    TabOrder := 2;
    OnClick := AddSelClick;
  end;
  
  Button4 := TButton.Create(Self);
  with Button4 do 
  begin
    Parent := Self;
    Visible := True;
    Left := 264;
    Top := 80;
    Width := 217;
    Height := 25;
    Caption := 'Button4';
    TabOrder := 3;
    OnClick := AddSelClick;
  end;
  
  Button5 := TButton.Create(Self);
  with Button5 do 
  begin
    Parent := Self;
    Visible := True;
    Left := 96;
    Top := 56;
    Width := 75;
    Height := 25;
    Caption := 'Button5';
    TabOrder := 4;
    OnClick := AddSelClick;
  end;
  
  Button6 := TButton.Create(Self);
  with Button6 do 
  begin
    Parent := Self;
    Visible := True;
    Left := 112;
    Top := 212;
    Width := 75;
    Height := 105;
    Caption := 'Button6';
    TabOrder := 5;
    OnClick := AddSelClick;
  end;
  
  Button7 := TButton.Create(Self);
  with Button7 do 
  begin
    Parent := Self;
    Visible := True;
    Left := 324;
    Top := 48;
    Width := 75;
    Height := 165;
    Caption := 'Button7';
    TabOrder := 6;
    OnClick := AddSelClick;
  end;
  
  Button8 := TButton.Create(Self);
  with Button8 do 
  begin
    Parent := Self;
    Visible := True;
    Left := 32;
    Top := 36;
    Width := 105;
    Height := 85;
    Caption := 'Drag test';
    TabOrder := 7;
    OnMouseDown := Button8MouseDown;
    OnMouseMove := Button8MouseMove;
    OnMouseUp := Button8MouseUp;
  end;
  
  FDown := False;
  
  FSelection := TControlSelection.Create(Self);
  FSelection.OnChange := SelChange;
  FSelection.Add(Button2);
end;

procedure TForm1.AddSelClick(Sender: TObject);
begin
  if FSelection.IsSelected(TControl(Sender))
  then FSelection.Remove(TControl(Sender))
  else FSelection.Add(TControl(Sender));
end;

procedure TForm1.SelChange(Sender: TObject);
begin
  beep;
end;

procedure TForm1.Button8MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
  Button8.Caption := Format('X:%d, Y:%d', [X,  Y]);
  if FDown then
  begin
    Button8.Left := Button8.Left + X - FStart.X;
    Button8.Top := Button8.Top + Y - FStart.Y;
  end;
end;

procedure TForm1.Button8MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDown := True;
  FStart := Point(X, Y);
end;

procedure TForm1.Button8MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDown := False;
end;

end.
