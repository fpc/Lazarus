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
Procedure NotifyUser(const DialogCaption, DialogMessage : String; DialogType : longint); override;
Procedure NotifyUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; X, Y : Longint); override;

Function PromptUser(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : Array of Longint) : Longint; override;
Function PromptUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : Array of Longint; X, Y : Longint) : Longint; override;

Function LoadStockPixmap(StockID: longint) : HBitmap; override;

Function RequestInput(const InputCaption, InputPrompt : String; MaskInput : Boolean; var Value : String) : Boolean; override;

{
  $Log$
  Revision 1.4  2002/10/12 16:36:40  lazarus
  AJ: added new QueryUser/NotifyUser

  Revision 1.3  2002/10/11 16:00:39  lazarus
  AJ: made InputQuery Interface Dependant

  Revision 1.2  2002/10/10 13:29:08  lazarus
  AJ: added LoadStockPixmap routine & minor fixes to/for GNOMEInt

  
}
