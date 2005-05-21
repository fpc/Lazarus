{%MainUnit carbonint.pas}

{ $Id$ }
{******************************************************************************
  All Carbon interface communication implementations.
  This are the implementation of the overrides of the Carbon Interface for the
  methods defined in the
  lcl/include/lclintf.inc


  !! Keep alphabetical !!

 ******************************************************************************
 Implementation
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

//##apiwiz##sps##   // Do not remove

function TCarbonWidgetSet.DrawSplitter(DC: HDC; const ARect: TRect;
  Horizontal: boolean): boolean;
begin
  Result:=inherited DrawSplitter(DC, ARect, Horizontal);
end;

function TCarbonWidgetSet.ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result:=inherited ExtUTF8Out(DC, X, Y, Options, Rect, Str, Count, Dx);
end;

function TCarbonWidgetSet.FontCanUTF8(Font: HFont): boolean;
begin
  Result:=inherited FontCanUTF8(Font);
end;

function TCarbonWidgetSet.GetAcceleratorString(const AVKey: Byte;
  const AShiftState: TShiftState): String;
begin
  Result:=inherited GetAcceleratorString(AVKey, AShiftState);
end;

function TCarbonWidgetSet.GetControlConstraints(Constraints: TObject): boolean;
begin
  Result:=inherited GetControlConstraints(Constraints);
end;

function TCarbonWidgetSet.GetLCLOwnerObject(Handle: HWnd): TObject;
begin
  Result:=inherited GetLCLOwnerObject(Handle);
end;

function TCarbonWidgetSet.GetListBoxIndexAtY(ListBox: TComponent; y: integer
  ): integer;
begin
  Result:=inherited GetListBoxIndexAtY(ListBox, y);
end;

function TCarbonWidgetSet.GetListBoxItemRect(ListBox: TComponent;
  Index: integer; var ARect: TRect): boolean;
begin
  Result:=inherited GetListBoxItemRect(ListBox, Index, ARect);
end;

function TCarbonWidgetSet.IntfSendsUTF8KeyPress: boolean;
begin
  Result:=inherited IntfSendsUTF8KeyPress;
end;

function TCarbonWidgetSet.MenuItemSetCheck(BaseMenuItem: TComponent): Boolean;
begin
  Result:=inherited MenuItemSetCheck(BaseMenuItem);
end;

function TCarbonWidgetSet.MenuItemSetEnable(BaseMenuItem: TComponent): Boolean;
begin
  Result:=inherited MenuItemSetEnable(BaseMenuItem);
end;

function TCarbonWidgetSet.RightJustifyMenuItem(BaseMenuItem: TComponent): Boolean;
begin
  Result:=inherited RightJustifyMenuItem(BaseMenuItem);
end;

function TCarbonWidgetSet.ReplaceBitmapMask(var Image, Mask: HBitmap;
  NewMask: HBitmap): boolean;
begin
  Result:=inherited ReplaceBitmapMask(Image, Mask, NewMask);
end;

procedure TCarbonWidgetSet.StatusBarPanelUpdate(StatusBar: TObject;
  Index: integer);
begin
  inherited StatusBarPanelUpdate(StatusBar, Index);
end;

procedure TCarbonWidgetSet.StatusBarSetText(StatusBar: TObject;
  PanelIndex: integer);
begin
  inherited StatusBarSetText(StatusBar, PanelIndex);
end;

procedure TCarbonWidgetSet.StatusBarUpdate(StatusBar: TObject);
begin
  inherited StatusBarUpdate(StatusBar);
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

// included by carbonint.pas
{ =============================================================================

  $Log$
  Revision 1.3  2005/05/21 15:58:44  mattias
  implemented right justification for menuitems for winapi intf  from Martin Smat

  Revision 1.2  2005/02/23 01:12:46  marc
  + Added RemoveProp winapi call
  * Some maintenace on winapi/lclintf files

  Revision 1.1  2004/09/06 22:24:52  mattias
  started the carbon LCL interface

}

