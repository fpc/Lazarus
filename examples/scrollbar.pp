{
 /***************************************************************************
                          Scrollbar - Example
                          ------------------

                   Initial Revision  : Thursday Feb 01 2001
                   by Shane Miller

                   Second Revision   : Saturday Jul 12 2014
                   by Jack D Linke

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
  SysUtils, ExtCtrls, Controls, LazLogger;

type
  TForm1 = class(TFORM)
    Scrollbar1 : TScrollbar;
    Button1 : TButton;
    Button2 : TButton;
    Button3 : TButton;
    Button4 : TButton;
    Label1 : TLabel;
    Label2 : TLabel;
    Label3 : TLabel;
    LabelMin : TLabel;
    LabelMax : TLabel;
    LabelPos : TLabel;
    Procedure Button1Clicked(Sender : TObject);
    Procedure Button2Clicked(Sender : TObject);
    Procedure Button3Clicked(Sender : TObject);
    Procedure Button4Clicked(Sender : TObject);
    Procedure Scrollbar1Changed(Sender : TObject);
    procedure Scrollbar1OnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
  public
    constructor Create(AOwner: TComponent); override;
  protected
  end;

var
  Form1 : TForm1;

constructor TForm1.Create(AOwner: TComponent);	
begin
  inherited CreateNew(AOwner, 1);
  Caption := 'Scrollbar Demo v0.2';
  Height := 350;
  Width := 400;

  ScrollBar1 := TSCrollBar.Create(self);
  with Scrollbar1 do
  Begin
    Parent := Self;
    Kind := sbVertical;
    Left := 100;
    Top := 50;
    Width := 15;
    Height := 300;
    Visible := True;
    OnChange := @scrollbar1Changed;
    OnScroll := @ScrollBar1OnScroll;
  end;

  Button1 := TButton.create(self);
  with Button1 do
  begin
    Parent := self;
    Visible := True;
    Caption := 'Swap Orientation';
    Onclick := @button1clicked;
    Width := 100;
  end;

  Button2 := TButton.create(self);
  with Button2 do
  begin
    Parent := self;
    Left := 100;
    Visible := True;
    Caption := 'Min + 20';
    Onclick := @button2clicked;
    Width := 100;
  end;

  Button3 := TButton.create(self);
  with Button3 do
  begin
    Parent := self;
    Left := 200;
    Visible := True;
    Caption := 'Max + 25';
    Onclick := @button3clicked;
    Width := 100;
  end;

  Button4 := TButton.create(self);
  with Button4 do
  begin
    Parent := self;
    Left := 300;
    Visible := True;
    Caption := 'Position + 5';
    Onclick := @button4clicked;
    Width := 100;
  end;

  Label1 := TLabel.create(self);
  with Label1 do
  begin
    Parent := self;
    Left := 125;
    Top := 100;
    Visible := True;
    Caption := 'Scrollbar1.Min = ';
    Width := 100;
  end;

  Label2 := TLabel.create(self);
  with Label2 do
  begin
    Parent := self;
    Left := 125;
    Top := 130;
    Visible := True;
    Caption := 'Scrollbar1.Max = ';
    Width := 100;
  end;

  Label3 := TLabel.create(self);
  with Label3 do
  begin
    Parent := self;
    Left := 125;
    Top := 160;
    Visible := True;
    Caption := 'Scrollbar1.Position = ';
    Width := 100;
  end;

  LabelMin := TLabel.create(self);
  with LabelMin do
  begin
    Parent := self;
    Left := 250;
    Top := 100;
    Visible := True;
    Caption := IntToStr(Scrollbar1.Min);
    Width := 100;
  end;

  LabelMax := TLabel.create(self);
  with LabelMax do
  begin
    Parent := self;
    Left := 250;
    Top := 130;
    Visible := True;
    Caption := IntToStr(Scrollbar1.Max);
    Width := 100;
  end;

  LabelPos := TLabel.create(self);
  with LabelPos do
  begin
    Parent := self;
    Left := 250;
    Top := 160;
    Visible := True;
    Caption := IntToStr(Scrollbar1.Position);
    Width := 100;
  end;

end;

procedure TForm1.Button1Clicked(Sender : TObject);
begin
  debugln('[Button1Clicked]');
  if ScrollBar1.Kind = sbHorizontal then
    Scrollbar1.Kind := sbVertical else
    Scrollbar1.kind := sbHorizontal;
end;

procedure TForm1.Button2Clicked(Sender : TObject);
begin
  ScrollBar1.Min := ScrollBar1.Min + 20;
  LabelMin.Caption := IntToStr(ScrollBar1.Min);
end;

procedure TForm1.Button3Clicked(Sender : TObject);
begin
  ScrollBar1.Max := ScrollBar1.Max + 25;
  LabelMax.Caption := IntToStr(ScrollBar1.Max);
end;

procedure TForm1.Button4Clicked(Sender : TObject);
begin
  Scrollbar1.Position := ScrollBar1.Position + 5;
  LabelPos.Caption := IntToStr(ScrollBar1.Position);
end;

procedure TForm1.Scrollbar1Changed(Sender : TObject);
begin
  LabelMin.Caption := IntToStr(Scrollbar1.Min);
  LabelMax.Caption := IntToStr(Scrollbar1.Max);
  LabelPos.Caption := IntToStr(Scrollbar1.Position);
end;

procedure TForm1.Scrollbar1OnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  DebugLn('.............Scrolled...............');
end;

begin
  Application.Initialize; { calls InitProcedure which starts up GTK }
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

