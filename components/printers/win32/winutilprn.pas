{
  Author: Olivier Guilbaud

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Abstract:
    This unit provide an access at Printers spool and other functions for manage
    the printers on Win32

  Documentations
    - Wine project
    - Microsoft MSDN Web
}

unit WinUtilPrn;

{$mode objfpc}{$H+}

interface

{$IFNDEF MSWindows}
{$FATAL This unit is reserved to Win32/Win64}
{$ENDIF}

uses
  Windows, Types, Classes, SysUtils, LCLType, Printers;

const
  {$i winutilprnconst.inc}

  LibWinSpool = 'winspool.drv';

const
  Win32Orientations: array [TPrinterOrientation] of SHORT = (
    DMORIENT_PORTRAIT, DMORIENT_LANDSCAPE, DMORIENT_PORTRAIT, DMORIENT_LANDSCAPE);

type
  TFcntHook = function(Wnd: HWND; uiMsg: UINT; wParam: WPARAM;
    lParam: LPARAM): UINT_PTR; stdcall;

{$ifndef win64}
  {$packrecords 1}
{$endif}

  tagPSD = record
    lStructSize: DWORD;
    hWndOwner: HWND;
    hDevMode: HGLOBAL;
    hDevNames: HGLOBAL;
    Flags: DWORD;
    ptPaperSize: TPOINT;
    rtMinMargin: TRECT;
    rtMargin: TRECT;
    hInstance: HINST;
    lCustData: LPARAM;
    lpfnPageSetupHook: TFcntHook;
    lpfnPagePaintHook: TFcntHook;
    lpPageSetupTemplateName: PChar;
    hPageSetupTemplate: HGLOBAL;
  end;
  PtagPSD = ^tagPSD;

  tagPD = record
    lStructSize: DWORD;
    hWndOwner: HWND;
    hDevMode: HGLOBAL;
    hDevNames: HGLOBAL;
    hDC: HDC;
    Flags: DWORD;
    nFromPage: word;
    nToPage: word;
    nMinPage: word;
    nMaxPage: word;
    nCopies: word;
    hInstance: HINST;
    lCustData: LPARAM;
    lpfnPrintHook: TFcntHook;
    lpfnSetupHook: TFcntHook;
    lpPrintTemplateName: PChar;
    lpSetupTemplateName: PChar;
    hPrintTemplate: HGLOBAL;
    hSetupTemplate: HGLOBAL;
  end;
  PtagPD = ^tagPD;

type
  { TPrinterDevice }

  TPrinterDevice = class
  public
    Name: string;
    Driver: string;
    Device: string;
    Port: string;

    DefaultPaper: Short;

    DevMode: PDeviceMode;
    DevModeSize: integer;
    destructor Destroy; override;
  end;

function DeviceCapabilities(pDevice, pPort: PChar; fwCapability: word;
  pOutput: PChar; DevMode: PDeviceMode): integer; stdcall; external LibWinSpool Name 'DeviceCapabilitiesA';

function GetProfileString(lpAppName: PChar; lpKeyName: PChar; lpDefault: PChar;
  lpReturnedString: PChar; nSize: DWORD): DWORD; stdcall; external 'kernel32' Name 'GetProfileStringA';

function PrintDlg(lppd: PtagPD): BOOL; stdcall; external 'comdlg32.dll' Name 'PrintDlgA';
function PageSetupDlg(lppd: PtagPSD): BOOL; stdcall; external 'comdlg32.dll' Name 'PageSetupDlgA';
function CommDlgExtendedError: DWORD; stdcall; external 'comdlg32.dll' Name 'CommDlgExtendedError';

function CreateIC(lpszDriver, lpszDevice, lpszOutput: PChar; lpdvmInit: PDeviceMode): HDC; stdcall; external 'gdi32.dll' Name 'CreateICA';
function CreateDC(lpszDriver, lpszDevice, lpszOutput: PChar; lpdvmInit: PDeviceMode): HDC; stdcall; external 'gdi32.dll' Name 'CreateDCA';
function DeleteDC(DC: HDC): BOOL; stdcall; external 'gdi32.dll' Name 'DeleteDC';
function StartDoc(DC: HDC; Inf: PDocInfo): integer; stdcall; external 'gdi32.dll' Name 'StartDocA';
function EndDoc(DC: HDC): integer; stdcall; external 'gdi32.dll' Name 'EndDoc';
function StartPage(DC: HDC): integer; stdcall; external 'gdi32.dll' Name 'StartPage';
function EndPage(DC: HDC): integer; stdcall; external 'gdi32.dll' Name 'EndPage';
function AbortDoc(DC: HDC): integer; stdcall; external 'gdi32.dll' Name 'AbortDoc';
function GlobalFree(HMem: HGlobal): HGlobal; stdcall; external 'kernel32.dll' Name 'GlobalFree';

function StartDocPrinter(hPrinter: THANDLE; Level: DWORD; DocInfo: PByte): DWORD; stdcall; external LibWinSpool Name 'StartDocPrinterA';
function StartPagePrinter(hPrinter: THANDLE): DWORD; stdcall; external LibWinSpool Name 'StartPagePrinter';
function EndDocPrinter(hprinter: THANDLE): BOOL; stdcall; external LibWinSpool Name 'EndDocPrinter';
function EndPagePrinter(hprinter: THANDLE): BOOL; stdcall; external LibWinSpool Name 'EndPagePrinter';
function AbortPrinter(hPrinter: THANDLE): BOOL; stdcall; external LibWinSpool Name 'AbortPrinter';
function WritePrinter(hPrinter: THANDLE; Buffer: Pointer; Count: DWord; Written: PDWORD): BOOL; stdcall; external LibWinSpool Name 'WritePrinter';

implementation

{ TPrinterDevice }

destructor TPrinterDevice.Destroy;
begin
  ReallocMem(DevMode, 0);
  inherited Destroy;
end;

end.

