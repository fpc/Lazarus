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
program SpeedTest;

{$mode objfpc}{$H+}

uses 
  Interfaces, Forms, SysUtils, Buttons, Classes, StdCtrls, LCLType,
  LCLIntf, Graphics;

type
  TForm1 = class(TForm)
    cmdOK: TButton;
    SpeedButton1 : TSpeedButton;
    SpeedButton2 : TSpeedButton;
    SpeedButton3 : TSpeedButton;
    SpeedButton4 : TSpeedButton;
  private
    FPicture: TPixmap;
    procedure ButtonClick(Sender: TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  

constructor TForm1.Create(AOwner: TComponent); 
var
  S: TFileStream;
begin
  inherited Create(AOwner);
  Width := 300;
  Height := 150;

  cmdOK := TButton.Create(Self);
  with cmdOK do
  begin
    top := 0;
    left := 0;
    Height := 20;
    parent := Self;
    visible := true;
    onClick := @ButtonClick;
  end;

  SpeedButton1 := TSpeedButton.Create(Self);
  with Speedbutton1 do
  begin
    Parent := self;
    Enabled := True;
    Top := 25;
    Visible := True;
   end;

  SpeedButton2 := TSpeedButton.Create(Self);
  with Speedbutton2 do
  begin
    Parent := self;
    Enabled := True;
    Top := 25;
    Left := 25;
    Visible := True;
  end;

  SpeedButton3 := TSpeedButton.Create(Self);
  with Speedbutton3 do
  begin
    Parent := self;
    Enabled := True;
    Top := 50;
    Visible := True;
    Flat := True;
   end;

  SpeedButton4 := TSpeedButton.Create(Self);
  with Speedbutton4 do
  begin
    Parent := self;
    Enabled := True;
    Top := 50;
    Left := 25;
    Flat := True;
    Visible := True;
  end;

  S := TFileStream.Create('../images/penguin.xpm', fmOpenRead);
  try
    FPicture := TPixmap.Create;
    FPicture.TransparentColor := clBtnFace;
    FPicture.LoadFromStream(S);
  finally
    S.Free;
  end;
end;

destructor TForm1.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

procedure TForm1.Paint;
var
  r: TRect;
begin
  inherited Paint;
  Canvas.Copyrect(Bounds(100,0,139,160), FPicture.Canvas, Rect(0,0,138,159));
  
  with SpeedButton4 do
  begin
    Self.Canvas.MoveTo(Left + Width + 2, Top);
    Self.Canvas.LineTo(Left + Width + 12, Top);

    Self.Canvas.MoveTo(Left + Width + 2, Top + Height);
    Self.Canvas.LineTo(Left + Width + 12, Top + Height);

    Self.Canvas.MoveTo(Left, Top + Height + 2);
    Self.Canvas.LineTo(Left, Top + Height + 12);
    
    Self.Canvas.MoveTo(Left + Width, Top + Height + 2);
    Self.Canvas.LineTo(Left + Width, Top + Height + 12);

    R := Bounds(Left + Width + 13, Top, Width, Height);
  end;

  DrawEdge(Canvas.Handle, R, BDR_RAISEDOUTER, BF_RECT);

  with R do
  begin
    Canvas.MoveTo(Left, Bottom + 2);
    Canvas.LineTo(Left, Bottom + 12);

    Canvas.MoveTo(Right, Bottom + 2);
    Canvas.LineTo(Right, Bottom + 12);
  end;
end;

procedure TForm1.ButtonClick(Sender: TObject);
begin
end;


var
  Form1: TForm1;
  
  
begin
   WriteLN('------ INIT ------- ');
   Application.Initialize; { calls InitProcedure which starts up GTK }
   WriteLN('------ CREATE ------- ');
   Application.CreateForm(TForm1, Form1);
   WriteLN('------ RUN ------- ');
   Application.Run;
   WriteLN('------ DONE ------- ');
end.

