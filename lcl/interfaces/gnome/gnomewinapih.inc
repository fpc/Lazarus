(******************************************************************************
  All GNOME Winapi related stuff goes here.

  !! Keep this alphabetical !!
 ******************************************************************************)
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
}

Function LoadStockPixmap(StockID: longint) : HBitmap; override;

Function RequestInput(const InputCaption, InputPrompt : String; MaskInput : Boolean; var Value : String) : Boolean; override;

{
  $Log$
  Revision 1.3  2002/10/11 16:00:39  lazarus
  AJ: made InputQuery Interface Dependant

  Revision 1.2  2002/10/10 13:29:08  lazarus
  AJ: added LoadStockPixmap routine & minor fixes to/for GNOMEInt

  
}
