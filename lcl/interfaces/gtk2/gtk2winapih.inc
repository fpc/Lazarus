{%MainUnit gtk2int.pp}
{ $Id$ }

// included by gtk2int.pp

{******************************************************************************
  All gtk2 Winapi related stuff goes here.
  This are the overrides of the QT Interface for the methods defined in the
  lcl/include/winapih.inc


  !! Keep alphabetical !!

  Support routines go to gtk2proc.pp

 ******************************************************************************
 Interface
 ******************************************************************************

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

//##apiwiz##sps##   // Do not remove, no wizard declaration before this line

function BeginPaint(Handle: hWnd; Var PS : TPaintStruct) : hdc; override;

function CreateFontIndirectEx(const LogFont: TLogFont; const LongFontName: string): HFONT; override;

function EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer; override;
function ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean; override;

function GetCursorPos(var lpPoint: TPoint ): Boolean; override;
function GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean; override;

function TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean; override;
   

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

{ =============================================================================

  $Log$
  Revision 1.1  2005/02/23 01:12:47  marc
  + Added RemoveProp winapi call
  * Some maintenace on winapi/lclintf files

  Revision 1.2  2004/03/19 00:53:34  marc
  * Removed all ComponentCreateHandle routines

}
