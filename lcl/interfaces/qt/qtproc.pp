{ $Id$ }
{
                  ----------------------------------
                   qtproc.pp  -  qt interface procs
                  ----------------------------------

 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains procedures/functions needed for the qt <-> LCL interface
}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit QTProc;

{$mode objfpc}{$H+}

interface

uses
  GraphType;

procedure FillStandardDescription(var Desc: TRawImageDescription);

implementation

{------------------------------------------------------------------------------
  Function: FillStandardDescription
  Params:
  Returns:
 ------------------------------------------------------------------------------}
procedure FillStandardDescription(var Desc: TRawImageDescription);
begin
  Desc.Init;

  Desc.Format := ricfRGBA;
//  Desc.Width := 0
//  Desc.Height := 0
//  Desc.PaletteColorCount := 0;

  Desc.BitOrder := riboReversedBits;
  Desc.ByteOrder := riboLSBFirst;
  Desc.LineOrder := riloTopToBottom;

  Desc.BitsPerPixel := 32;
  Desc.Depth := 32;
  // Qt wants dword-aligned data
  Desc.LineEnd := rileDWordBoundary;

  // 8-8-8-8 mode, high byte is Alpha
  Desc.AlphaPrec := 8;
  Desc.RedPrec := 8;
  Desc.GreenPrec := 8;
  Desc.BluePrec := 8;

  Desc.AlphaShift := 24;
  Desc.RedShift := 16;
  Desc.GreenShift := 8;
//  Desc.BlueShift := 0;

  // Qt wants dword-aligned data
  Desc.MaskLineEnd := rileDWordBoundary;
  Desc.MaskBitOrder := riboReversedBits;
  Desc.MaskBitsPerPixel := 1;
//  Desc.MaskShift := 0;
end;

end.
