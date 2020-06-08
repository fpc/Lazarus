{
                  ----------------------------------
                   qtproc.pp  -  qt5 interface procs
                  ----------------------------------

 This unit contains procedures/functions needed for the qt5 <-> LCL interface
}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit qtproc;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Bindings
  qt5,
  // LazUtils
  GraphType, LazUTF8,
  // LCL
  InterfaceBase;

const
  EVE_IO_READ  = 1;
  EVE_IO_WRITE = 4;
  EVE_IO_ERROR = 8;

type
  PWaitHandleEventHandler = ^TWaitHandleEventHandler;
  TWaitHandleEventHandler = record
    qsn: array[QSocketNotifierRead..QSocketNotifierException] of QSocketNotifierH; // the notifiers for events
    qsn_hook: array[QSocketNotifierRead..QSocketNotifierException] of QSocketNotifier_hookH; // the hooks
    user_callback: TWaitHandleEvent;
    udata: PtrInt; // the userdata
    socket: Integer; // for mapping
  end;


procedure FillStandardDescription(var Desc: TRawImageDescription);
function GetPixelsPerInch: Integer;
function GetUtf8String(const S: String): WideString;

implementation

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

function GetUtf8String(const S: String): WideString;
begin
  Result := UTF8ToUTF16(S);
  if Result = '' then
    Result := S;
end;

{------------------------------------------------------------------------------
  Function: GetPixelsPerInch
  Params: none
  Returns: pixels per inch value eg. 96
 ------------------------------------------------------------------------------}
function GetPixelsPerInch: Integer;
var
  QtDC: QPaintDeviceH;
begin
  QtDC := QWidget_to_QPaintDevice(QApplication_desktop);
  Result := QPaintDevice_logicalDpiY(QtDC);
end;

end.
