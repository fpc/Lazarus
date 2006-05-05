{%MainUnit gnomeint.pas}
{ $Id$ }
(******************************************************************************
  All GNOME Winapi related stuff goes here.

  !! Keep this alphabetical !!
 ******************************************************************************)
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

//##apiwiz##sps##   // Do not remove

Function LoadStockPixmap(StockID: longint) : HBitmap; override;

Function PromptUser(const DialogCaption, DialogMessage : String; DialogType : longint;
  Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint) : Longint; override;
Function PromptUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint;
  Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint; override;

Function RequestInput(const InputCaption, InputPrompt : String; MaskInput : Boolean; var Value : String) : Boolean; override;

//##apiwiz##eps##   // Do not remove