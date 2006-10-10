{%MainUnit qtint.pp}
{ $Id$ }

{******************************************************************************
                  All QT interface support routines
                   Initial Revision  : Sat Jan 17 19:00:00 2004


  !! Keep alphabetical !!

  Support routines go to qtproc.pp

 ******************************************************************************
 Implementation
 ******************************************************************************

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

//##apiwiz##sps##   // Do not remove

procedure TQtWidgetSet.FillStandardDescription(var Desc: TRawImageDescription);
begin
  FillChar(Desc, SizeOf(Desc), 0);

  Desc.Format := ricfRGBA;
  Desc.HasPalette := False;
//  Desc.Depth := 32;
  // Width and Height not relevant
  Desc.PaletteColorCount := 0;
  Desc.BitOrder := riboReversedBits;
  Desc.ByteOrder := riboLSBFirst;
  Desc.LineOrder := riloTopToBottom;
  Desc.ColorCount := Desc.PaletteColorCount;
  Desc.BitsPerPixel := 32;
  Desc.LineEnd := rileDWordBoundary;

  // 0-8-8-8 mode, high byte is not used
  Desc.RedPrec := 8;
  Desc.GreenPrec := 8;
  Desc.BluePrec := 8;
  Desc.RedShift := 16;
  Desc.GreenShift := 8;
  Desc.BlueShift := 0;
  Desc.Depth := 24;

  Desc.AlphaPrec := 0;
  Desc.AlphaSeparate := False;
  // Qt wants dword-aligned data
  Desc.AlphaLineEnd := rileDWordBoundary;
  Desc.AlphaShift := 0;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
