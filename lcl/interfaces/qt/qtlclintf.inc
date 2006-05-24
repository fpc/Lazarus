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
  Desc.Depth := 32;
  // Width and Height not relevant
  Desc.PaletteColorCount := 0;
  Desc.BitOrder := riboReversedBits;
  Desc.ByteOrder := riboLSBFirst;
  Desc.LineOrder := riloTopToBottom;
  Desc.ColorCount := Desc.PaletteColorCount;
  Desc.BitsPerPixel := 32;
  Desc.LineEnd := rileDWordBoundary;
//  FillRawImageDescriptionColors(Desc);
  Desc.AlphaPrec := 1;
  Desc.AlphaSeparate := False;
  // CreateBitmap winapi call wants word-aligned data
  Desc.AlphaShift := 0;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
