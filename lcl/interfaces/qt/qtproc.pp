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
{$ifdef USE_QT_4_3}
  qt43,
{$else}
  qt4,
{$endif}
  GraphType, QtWidgets;

procedure FillStandardDescription(var Desc: TRawImageDescription);
function GetUtf8String(S: String): WideString;
function QtObjectFromWidgetH(const WidgetH: QWidgetH): TQtWidget;

implementation

function QtObjectFromWidgetH(const WidgetH: QWidgetH): TQtWidget;
var
  V: QVariantH;
  Ok: Boolean;
  QtWg: TQtWidget;
begin
  Result := nil;
  V := QVariant_Create();
  try
    QObject_property(QObjectH(WidgetH), V, 'lclwidget');
    if not QVariant_IsNull(v) and QVariant_isValid(V) then
    begin
      //Write('Got a valid variant .. ');
{$IFDEF CPU32}
      QtWg := TQtWidget(QVariant_toUint(V, @Ok));
{$ENDIF}
{$IFDEF CPU64}
      QtWg := TQtWidget(QVariant_toULongLong(V, @Ok));
{$ENDIF}
      if OK then
      begin
        //Write('Converted successfully, Control=');
        if QtWg<>nil then
        begin
          Result := QtWg;
          //WriteLn(Result.LCLObject.Name);
        end else
          ;//WriteLn('nil');
      end else
        ;//WriteLn('Can''t convert to UINT');
    end else
      ;//Writeln('GetFocus: Variant is NULL or INVALID');
  finally
    QVariant_Destroy(V);
  end;
end;

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

function GetUtf8String(S: String): WideString;
begin
  Result := Utf8Decode(S);
  if (Result = '') and (S <> '') then
    Result := S;
end;

end.
