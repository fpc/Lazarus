{  $Id$  }
{
 /***************************************************************************
                               TestForm.pp
                             -------------------




 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
{$H+}
//{$DEFINE NEW_EDITOR}
unit TestForm;

{$mode objfpc}

interface

uses
  classes, Controls, forms,buttons,sysutils, stdctrls,
	mwPasSyn,mwcustomedit, Graphics;
//  compiler, stdctrls,forms,buttons,menus,comctrls,
//	Spin, project,sysutils,global,editor, compileroptions,Controls,graphics,extctrls, TabNotBk,

type

  TTestForm = class(TFORM)
    cmdTest: TButton;
    cmdTest2: TButton;
    cmdTest3: TButton;
    cmdTest4: TButton;
    cmdTest5: TButton;
    mwEdit1 : TMWCustomEdit;
    mwPasSyn1 : TmwPasSyn;
    lbTest : TListBox;
    cbTest : TComboBox;
  private
    FB: TBitmap;
  protected
    procedure TestCLick(Sender : TObject);
    procedure Test2CLick(Sender : TObject);
    procedure Test3CLick(Sender : TObject);
    procedure Test4CLick(Sender : TObject);
    procedure Test5CLick(Sender : TObject);
    Procedure TestFormPaint(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  TestForm1 : TTestForm;

implementation
uses
  LCLLinux;

constructor TTestForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := 'Lazarus Testing';
  Left := 0;
  Top := 0;
  Width := 700;
  height := 300;
  Position:= poMainFormCenter;

  lbTest:= TListBox.Create(Self);
  with lbTest do begin
    Top:= 25;
    Left:= 600;
    Width:= 200;
    Height:= 100;
    Parent:= Self;
    Visible:= true;
    Items.Add('First line');
    Items.Add('Second line');
    BorderStyle:= bsNone;
    MultiSelect:= true;
//    Selected[1]:= true;
  end;

  cbTest:= TComboBox.Create(Self);
  with cbTest do begin
    Top:= 200;
    Left:= 600;
    Width:= 100;
    Height:= 100;
    Parent:= Self;
    Visible:= true;
    Items.Add('First line');
    Items.Add('Second line');
    Items.Add('Third line');
    Text:= 'Hello';
  end;

  mwPasSyn1 := TmwPasSyn.Create(Self);
  mwPasSyn1.CommentAttri.Foreground := clNavy;
  mwPasSyn1.KeyAttri.Foreground := clRed;
  mwPasSyn1.NumberAttri.Foreground := clGreen;

  mwEdit1 := TmwCustomEdit.Create(Self);
  mwEdit1.Top := 25;
  mwEdit1.Left := 0;
  mwEdit1.Width := 500;
  mwEdit1.Height := 250;
  mwEdit1.Parent := Self;
  {$IFNDEF NEW_EDITOR}
  mwEdit1.GutterColor := clBtnface;
 {$ELSE}
  mwEdit1.Gutter.Color := clBtnface;
  mwEdit1.Gutter.ShowLineNumbers := True;
  {$ENDIF}
  mwedit1.Color := clWindow;
  mwEdit1.Visible := True;
  mwEdit1.Font.Name := 'courier';
  mwEdit1.Font.Size := 8;
  mwEdit1.HighLighter := mwPasSyn1;
//  mwEdit1.Text := 'Hi! ';
//  mwEdit1.Align := alClient;

  Assert(False, 'Trace:Created mwCustomEdit ...');

  cmdTest := TButton.Create(Self);

  with cmdTest do
  begin
    Top := 0;
    Left := 0;
    Width := 50;
    Height := 25;
    Parent := Self;
    Caption := 'GDI/API';
    Visible := True;
    OnClick := @TestClick;
  end;

  cmdTest2 := TButton.Create(Self);

  with cmdTest2 do
  begin
    Top := 0;
    Left := 50;
    Width := 50;
    Height := 25;
    Parent := Self;
    Caption := 'SetText';
    Visible := True;
    OnClick := @Test2Click;
  end;

  cmdTest3 := TButton.Create(Self);
  with cmdTest3 do
  begin
    Top := 0;
    Left := 100;
    Width := 50;
    Height := 25;
    Parent := Self;
    Caption := 'Load text';
    Visible := True;
    OnClick := @Test3Click;
  end;

  cmdTest4 := TButton.Create(Self);
  with cmdTest4 do
  begin
    Top := 0;
    Left := 150;
    Width := 50;
    Height := 25;
    Parent := Self;
    Caption := 'Test';
    Visible := True;
    OnClick := @Test4Click;
  end;

  cmdTest5 := TButton.Create(Self);
  with cmdTest5 do
  begin
    Top := 0;
    Left := 200;
    Width := 50;
    Height := 25;
    Parent := Self;
    Caption := 'Focus';
    Visible := True;
    OnClick := @Test5Click;
  end;

  Assert(False, 'Trace:[TTestForm.ButtonClick] BMP: Create');
//  FB := TBitmap.Create;

OnPaint := @TestFormPaint;
end;

Procedure TTestForm.TestFormPaint(Sender : TObject);
Begin

end;


procedure TTestForm.TestClick(Sender : TObject);
const
  TEST_TEXT: array[0..80] of Char = 'with TButton.Create(Self) do' + #10 + #13 +'  Label :=  ''Lazarus test code'';' + #0;
var
  F: TFont;
  R: TRect;
Begin
  Assert(False, 'Trace:[TTestForm.ButtonClick] Canvas.Brush');
  Canvas.Brush.Style := bsFDiagonal;
  canvas.Brush.Color := clRed;
  Assert(False, 'Trace:[TTestForm.ButtonClick] Canvas.Pen');
  canvas.Pen.Color := clBlue;
  Assert(False, 'Trace:[TTestForm.ButtonClick] Canvas.Rectangle');
  Canvas.Rectangle(500,250,550,300);

  Canvas.Pen.Style:= psSolid;
  Canvas.Line(600, 95, 650, 95);
  Canvas.Pen.Color:= clBlack;
  Canvas.Pen.Style:= psDash;
  Canvas.Line(600, 145, 650, 145);
  Canvas.Pen.Color:= clRed;
  Canvas.Pen.Style:= psDot;
  Canvas.Line(600, 195, 650, 195);
  Canvas.Pen.Color:= clGreen;
  Canvas.Pen.Style:= psDashDot;
  Canvas.Line(600, 245, 650, 245);

  Assert(False, 'Trace:[TTestForm.ButtonClick] DrawEdge');
  R := Rect(600, 50, 650, 90);
  DrawEdge(Canvas.Handle, R, EDGE_ETCHED, BF_RECT or BF_MIDDLE);
  R := Rect(600, 100, 650, 140);
  DrawEdge(Canvas.Handle, R, EDGE_BUMP, BF_RECT or BF_MIDDLE);
  R := Rect(600, 150, 650, 190);
  DrawEdge(Canvas.Handle, R, EDGE_RAISED, BF_RECT or BF_MIDDLE);
  R := Rect(600, 200, 650, 240);
  DrawEdge(Canvas.Handle, R, EDGE_SUNKEN, BF_RECT or BF_MIDDLE);
End;

procedure TTestForm.Test2Click(Sender : TObject);
Begin
  try
    Assert(False, 'Trace:[TTestForm.ButtonClick] SetEditText');
//    mwEdit1.Lines.Text := mwEdit1.Lines.Text + 'Lazarus test code ' + #10 ;
    mwEdit1.Lines.Add('Lazarus test code');
    Assert(False, 'Trace:[TTestForm.ButtonClick] Text --> ' + mwEdit1.Lines.Text);
//    mwEdit1.RePaint;
  except
    on E: Exception do WriteLN('Exception: ' + E.Message);
    //on Exception do;

  end;

mwEdit1.Cursor := crIBeam;
end;

procedure TTestForm.Test3Click(Sender : TObject);
Begin
  Assert(False, Format('Trace:[TTestForm.ButtonClick] In window Chars: %d, Lines: %d', [mwEdit1.CharsInWindow, mwEdit1.LinesInWindow]));
  try
    Assert(False, 'Trace:[TTestForm.ButtonClick] Load edit text');
    mwEdit1.Lines.LoadFromFile('testform.pp');

//    mwEdit1.Lines.Text := '';
//    mwEdit1.Lines.Add('Lazarus test code');
//    mwEdit1.RePaint;
  except
    on E: Exception do WriteLN('Exception: ' + E.Message);
    //on Exception do;

  end;
mwEdit1.Cursor := crIBeam;
end;

procedure TTestForm.Test4Click(Sender : TObject);
begin
  Assert(False, 'Trace:[TTestForm.Test4Click]');
  //cmdTest4.Width := cmdTest4.Width + 1;
  mwEdit1.Width := mwEdit1.Width + 1;
end;

procedure TTestForm.Test5Click(Sender : TObject);
begin
  Assert(False, 'Trace:[TTestForm.Test5Click]');
  mwEdit1.SetFocus;
end;

end.



{ =============================================================================

  $Log$
  Revision 1.1  2000/07/13 10:27:48  michael
  + Initial import

  Revision 1.26  2000/05/10 02:34:43  lazarus
  Changed writelns to Asserts except for ERROR and WARNING messages.   CAW

  Revision 1.25  2000/05/09 18:37:02  lazarus
  *** empty log message ***

  Revision 1.24  2000/05/09 12:52:02  lazarus
  *** empty log message ***

  Revision 1.23  2000/03/19 23:01:42  lazarus
  MWE:
    = Changed splashscreen loading/colordepth
    = Chenged Save/RestoreDC to platform  dependent, since they are
      relative to a DC

  Revision 1.22  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.21  2000/02/22 23:26:12  lazarus
  MWE: Fixed cursor movement in editor
       Started on focus problem

  Revision 1.20  2000/02/22 21:29:42  lazarus
  Added a few more options in the editor like closeing a unit.  Also am keeping track of what page , if any, they are currently on.
  Shane

  Revision 1.19  2000/02/20 20:13:47  lazarus
  On my way to make alignments and stuff work :-)

  Revision 1.18  2000/02/18 19:38:52  lazarus
  Implemented TCustomForm.Position
  Better implemented border styles. Still needs some tweaks.
  Changed TComboBox and TListBox to work again, at least partially.
  Minor cleanups.

  Revision 1.17  2000/01/26 19:16:24  lazarus
  Implemented TPen.Style properly for GTK. Done SelectObject for pen objects.
  Misc bug fixes.
  Corrected GDK declaration for gdk_gc_set_slashes.

  Revision 1.16  2000/01/18 21:47:00  lazarus
  Added OffSetRec

  Revision 1.15  2000/01/11 20:50:32  lazarus
  Added some code for SETCURSOR.  Doesn't work perfect yet but getting there.
  Shane

  Revision 1.14  1999/12/21 00:07:06  lazarus
  MWE:
    Some fixes
    Completed a bit of DraWEdge

  Revision 1.13  1999/12/18 18:27:31  lazarus
  MWE:
    Rearranged some events to get a LM_SIZE, LM_MOVE and LM_WINDOWPOSCHANGED
    Initialized the TextMetricstruct to zeros to clear unset values
    Get mwEdit to show more than one line
    Fixed some errors in earlier commits

  Revision 1.12  1999/12/07 21:53:13  lazarus
  Modified mwPasSyn.pas to get the fTableProc assugnment to not crash.
  Shane

  Revision 1.11  1999/12/07 01:19:25  lazarus
  MWE:
    Removed some double events
    Changed location of SetCallBack
    Added call to remove signals
    Restructured somethings
    Started to add default handlers in TWinControl
    Made some parts of TControl and TWinControl more delphi compatible
    ... and lots more ...

  Revision 1.10  1999/12/06 21:17:23  lazarus
  Added a mwPasSyn1 component to TESTFORM.  It gets created in Testform.Create and assigned to mwedit1.highlighter.  It crashes in NULLPROC in mwPasSyn.pas
  Shane

  Revision 1.9  1999/12/06 20:41:14  lazarus
  Miinor debugging changes.
  Shane

  Revision 1.8  1999/12/03 00:26:47  lazarus
  MWE:
    fixed control location
    added gdiobject reference counter

  Revision 1.7  1999/12/02 19:00:59  lazarus
  MWE:
    Added (GDI)Pen
    Changed (GDI)Brush
    Changed (GDI)Font (color)
    Changed Canvas to use/create pen/brush/font
    Hacked mwedit to allow setting the number of chars (till it get a WM/LM_SIZE event)
    The editor shows a line !

  Revision 1.6  1999/11/29 00:46:46  lazarus
  MWE:
    Added TBrush as gdiobject
    commented out some more mwedit MWE_FPC ifdefs

  Revision 1.5  1999/11/25 23:45:08  lazarus
  MWE:
    Added font as GDIobject
    Added some API testcode to testform
    Commented out some more IFDEFs in mwCustomEdit

  Revision 1.4  1999/11/23 22:06:27  lazarus
  Minor changes to get it running again with the latest compiler.  There is something wrong with the compiler that is preventing certain things from working.
  Shane

  Revision 1.3  1999/11/19 14:44:37  lazarus
  Changed the FONTSETNAME to try and load a default font if the first one doesn't work.  This is being done for testing and probably will be removed later.
  Shane

  Revision 1.2  1999/11/19 01:09:43  lazarus
  MWE:
    implemented TCanvas.CopyRect
    Added StretchBlt
    Enabled creation of TCustomControl.Canvas
    Added a temp hack in TWinControl.Repaint to get a LM_PAINT

  Revision 1.1  1999/11/17 01:07:49  lazarus
  MWE:
    Added a testform to test mwEdit and DCs

}


























































