{%MainUnit ../lclintf.pp}
{ $Id: lclintf.inc 8032 2005-11-02 12:44:29Z vincents $
******************************************************************************
  All interface communication related stuff goes here.
  This file is used by LCLIntf.pas
  If a procedure is platform dependent then it should call:
    WidgetSet.MyDependentProc

  If a procedure insn't platform dependent, it is no part of InterfaseBase has
  to be implementerd here

  !! Keep this alphabetical !!

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
 ******************************************************************************

******************************************************************************
  These functions redirect to the platform specific interface object.

  Note:
    the section for not referring WidgetSet is at the end
 ******************************************************************************}
//##apiwiz##sps##   // Do not remove

function TWinCEWidgetSet.DrawSplitter(DC: HDC; const ARect: TRect;
  Horizontal: boolean): boolean;
begin
  Result:=inherited DrawSplitter(DC, ARect, Horizontal);
end;

function TWinCEWidgetSet.ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result:=inherited ExtUTF8Out(DC, X, Y, Options, Rect, Str, Count, Dx);
end;

function TWinCEWidgetSet.FontCanUTF8(Font: HFont): boolean;
begin
  Result:=inherited FontCanUTF8(Font);
end;

function TWinCEWidgetSet.GetAcceleratorString(const AVKey: Byte;
  const AShiftState: TShiftState): String;
begin
  Result:=inherited GetAcceleratorString(AVKey, AShiftState);
end;

function TWinCEWidgetSet.GetControlConstraints(Constraints: TObject): boolean;
begin
  Result:=inherited GetControlConstraints(Constraints);
end;

function TWinCEWidgetSet.GetLCLOwnerObject(Handle: HWnd): TObject;
begin
  Result:=inherited GetLCLOwnerObject(Handle);
end;

function TWinCEWidgetSet.GetListBoxIndexAtY(ListBox: TComponent; y: integer
  ): integer;
begin
  Result:=inherited GetListBoxIndexAtY(ListBox, y);
end;

function TWinCEWidgetSet.GetListBoxItemRect(ListBox: TComponent;
  Index: integer; var ARect: TRect): boolean;
begin
  Result:=inherited GetListBoxItemRect(ListBox, Index, ARect);
end;

function TWinCEWidgetSet.IntfSendsUTF8KeyPress: boolean;
begin
  Result:=inherited IntfSendsUTF8KeyPress;
end;

function TWinCEWidgetSet.ReplaceBitmapMask(var Image, Mask: HBitmap;
  NewMask: HBitmap): boolean;
begin
  Result:=inherited ReplaceBitmapMask(Image, Mask, NewMask);
end;

//##apiwiz##epi##   // Do not remove

