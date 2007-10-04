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

  Abstract:
    This example demonstrates how to
    - create an image with an internal format similar to Delphi's pf24bit
    - convert it to current format and create a TBitmap from it
    - use an approach similar to Delphi's TBitmap.ScanLine.
    
  Delphi's TBitmap implementation only supports windows formats. For example
  the TBitmap.ScanLine function gives a direct pointer to the memory. This is
  not possible under all widget sets. And even those who supports it, uses
  different formats than windows. So Delphi code using TBitmap.ScanLine has to
  be changed anyway. How much depends on how much speed is needed.
  
  If the goal is to quickly port some Delphi code using TBitmap.Scanline, then
  the below code gives some hints how to achieve it.
}
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  FPImage, GraphType, IntfGraphics, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
  public
    MyBitmap: TBitmap;
    procedure PaintToRGB32bitScanLine(Row, ImgWidth: integer; LineStart: Pointer);
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  IntfImage: TLazIntfImage;
  ScanLineImage: TLazIntfImage;
  y: Integer;
  ImgFormatDescription: TRawImageDescription;
begin
  MyBitmap:=TBitmap.Create;

  // create an image with a format similar to Delphi's pf32bit
  // keep in mind that you access it in bytes, not words or dwords
  // For example PowerPC uses another byte order (endian big)
  ScanLineImage:=TLazIntfImage.Create(0,0);
  ImgFormatDescription.Init_BPP32_B8G8R8_BIO_TTB(30,20);
  ScanLineImage.DataDescription:=ImgFormatDescription;

  // call the pf24bit specific drawing function
  for y:=0 to ScanLineImage.Height-1 do
    PaintToRGB32bitScanLine(y,ScanLineImage.Width,
                            ScanLineImage.GetDataLineStart(y));

  // create IntfImage with the format of the current LCL interface
  MyBitmap.Width:=ScanLineImage.Width;
  MyBitmap.Height:=ScanLineImage.Height;
  IntfImage:=MyBitmap.CreateIntfImage;
  // convert the content from the very specific to the current format
  IntfImage.CopyPixels(ScanLineImage);
  MyBitmap.LoadFromIntfImage(IntfImage);

  ScanLineImage.Free;
  IntfImage.Free;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  MyBitmap.Free;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  Canvas.Draw(10,10,MyBitmap);
end;

procedure TForm1.PaintToRGB32bitScanLine(Row, ImgWidth: integer;
  LineStart: Pointer);
// LineStart is pointer to the start of a scanline with the following format:
// 4 bytes per pixel. First byte is blue, second green, third is red.
// Black is 0,0,0, white is 255,255,255
var
  i: Integer;
begin
  // fill line with gray
  for i:=0 to (ImgWidth*4)-1 do
    PByte(LineStart)[i]:=0; // set red, green and blue to 0 (i.e. black)
  // set one pixel to red (this creates a red line)
  PByte(LineStart)[(Row mod ImgWidth)*4+2]:=255;
end;

initialization
  {$I unit1.lrs}

end.

