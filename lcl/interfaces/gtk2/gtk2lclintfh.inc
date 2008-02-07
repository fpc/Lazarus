{%MainUnit gtk2int.pp}
{ $Id$ }

{******************************************************************************
                 All GTK2 interface communication implementations.
                   Initial Revision  : Sat Jan 17 19:00:00 2004


  !! Keep alphabetical !!

  Support routines go to gtk2proc.pp

 ******************************************************************************
 Interface
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
procedure DrawGrid(DC: HDC; const R: TRect; DX, DY: Integer); override;
function ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect;
  Str: PChar; Count: Longint; Dx: PInteger): Boolean; override;
function TextUTF8Out(DC: HDC; X, Y: Integer; Str: PChar; Count: Longint): Boolean; override;
function ComboBoxDropDown(Handle: HWND; DropDown: boolean): boolean; override;
function SetComboMinDropDownSize(Handle: HWND; MinItemsWidth, MinItemsHeight, MinItemCount: integer): boolean; override;


//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
