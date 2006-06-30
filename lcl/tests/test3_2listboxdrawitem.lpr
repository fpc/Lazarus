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

  LCL Test 3_1

  Showing a TListBox with custom drawn items.
}
program test3_2listboxdrawitem;

{$mode objfpc}{$H+}

uses
  Interfaces, SysUtils, FPCAdds, LCLProc, LCLType, Classes, Controls, Forms,
  TypInfo, LCLIntf, LMessages, Buttons, ExtCtrls, StdCtrls, Graphics;

type

  { TForm1 }

  TForm1 = class(TForm)
    ListBox1: TListBox;
    procedure Form1Create(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

{ TForm1 }

procedure TForm1.Form1Create(Sender: TObject);
var
  sl: TStringList;
  i: Integer;
begin
  debugln('TForm1.Form1Create ',DbgSName(Sender));
  SetBounds(50,50,500,400);

  ListBox1:=TListBox.Create(Self);
  with ListBox1 do begin
    Name:='ListBox1';
    Align:=alTop;
    Height:=100;
    Parent:=Self;
    OnDrawItem:=@ListBox1DrawItem;
    Style:=lbOwnerDrawFixed;
  end;
  sl:=TStringList.Create;
  for i:=0 to 9 do
    sl.Add('Item '+IntToStr(i));
  ListBox1.Items.Assign(sl);
  sl.Free;
end;

procedure TForm1.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  CurRect: TRect;
begin
  DebugLn(['TForm1.ListBox1DrawItem Index=',Index,' ARect=',dbgs(ARect),' State=',integer(State)]);
  with ListBox1.Canvas do begin
    CurRect:=Rect(ARect.Left+50,ARect.Top,ARect.Right-50,ARect.Bottom);
    Brush.Color:=clBlue;
    FillRect(CurRect);
    Brush.Color:=clRed;
    InflateRect(CurRect,-1,-1);
    FillRect(CurRect);
    TextOut(CurRect.Left+10,CurRect.Top+2,ListBox1.Items[Index]);
  end;
end;

constructor TForm1.Create(TheOwner: TComponent);
begin
  OnCreate:=@Form1Create;
  inherited Create(TheOwner);
end;

var
  Form1: TForm1 = nil;
begin
  Application.Initialize;
  Application.CreateForm(TForm1,Form1);
  Application.Run;
end.

