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
     DMORIENT_LANDSCAPE = 2;
     DMORIENT_PORTRAIT = 1;
     DM_ORIENTATION = $1;
     DM_PAPERSIZE = $2;
     DM_COPIES = $100;

Const
  Win32Orientations: array [TPrinterOrientation] of SHORT = (
    DMORIENT_PORTRAIT, DMORIENT_LANDSCAPE, DMORIENT_PORTRAIT, DMORIENT_LANDSCAPE);



type
  PDevNames = ^tagDEVNAMES;
  tagDEVNAMES = packed record
    wDriverOffset: Word;
    wDeviceOffset: Word;
    wOutputOffset: Word;
    wDefault: Word;
  end;

  TFcntHook = function(Wnd: HWND; uiMsg: UINT; wParam: WPARAM; lParam: LPARAM): UINT_PTR; stdcall;


  tagPSD= packed record
    lStructSize : DWORD;
    hWndOwner   : HWND;
    hDevMode    : HGLOBAL;
    hDevNames   : HGLOBAL;
    Flags       : DWORD;
    ptPaperSize : TPOINT;
    rtMinMargin : TRECT;
    rtMargin    : TRECT;
    hInstance   : HINST;
    lCustData   : LPARAM;
    lpfnPageSetupHook : TFcntHook;
    lpfnPagePaintHook : TFcntHook;
    lpPageSetupTemplateName : pChar;
    hPageSetupTemplate : HGLOBAL;
  end;




  tagPD=packed Record
    lStructSize  : DWORD;
    hWndOwner    : HWND;
    hDevMode     : HGLOBAL;
    hDevNames    : HGLOBAL;
    hDC          : HDC;
    Flags        : DWORD;
    nFromPage    : Word;
    nToPage      : Word;
    nMinPage     : Word;
    nMaxPage     : Word;
    nCopies      : Word;
    hInstance    : HINST;
    lCustData    : LPARAM;
    lpfnPrintHook: TFcntHook;
    lpfnSetupHook: TFcntHook;
    lpPrintTemplateName: PChar;
    lpSetupTemplateName: PChar;
    hPrintTemplate     : HGLOBAL;
    hSetupTemplate     : HGLOBAL;
  end;

  PDeviceMode = ^TDeviceMode;
  TDeviceMode =  packed Record
    dmDeviceName      : array[0..31] of AnsiChar;
    dmSpecVersion     : Word;
    dmDriverVersion   : Word;
    dmSize            : Word;
    dmDriverExtra     : Word;
    dmFields          : DWORD;
    dmOrientation     : SHORT;
    dmPaperSize       : SHORT;
    dmPaperLength     : SHORT;
    dmPaperWidth      : SHORT;
    dmScale           : SHORT;
    dmCopies          : SHORT;
    dmDefaultSource   : SHORT;
    dmPrintQuality    : SHORT;
    dmColor           : SHORT;
    dmDuplex          : SHORT;
    dmYResolution     : SHORT;
    dmTTOption        : SHORT;
    dmCollate         : SHORT;
    dmFormName        : Array[0..31] of AnsiChar;
    dmLogPixels       : Word;
    dmBitsPerPel      : DWORD;
    dmPelsWidth       : DWORD;
    dmPelsHeight      : DWORD;
    dmDisplayFlags    : DWORD;
    dmDisplayFrequency: DWORD;
    dmICMMethod       : DWORD;
    dmICMIntent       : DWORD;
    dmMediaType       : DWORD;
    dmDitherType      : DWORD;
    dmICCManufacturer : DWORD;
    dmICCModel        : DWORD;
    dmPanningWidth    : DWORD;
    dmPanningHeight   : DWORD;
  end;
  
  
  Type

  { TPrinterDevice }

  TPrinterDevice = class
  public
    Name   : String;
    Driver : String;
    Device : String;
    Port   : String;

    DefaultPaper : Short;
    
    DevMode: PDeviceMode;
    DevModeSize: Integer;
    destructor destroy; override;
  end;


  PPrinterDefaults = ^_PRINTER_DEFAULTS;
  _PRINTER_DEFAULTS = packed record
    pDatatype    : PChar;
    pDevMode     : PDeviceMode;
    DesiredAccess: DWord;
  end;

  //Size and ImageableArea Specifies the width and height,
  //in thousandths of millimeters, of the form
  PFORM_INFO_1   =^_FORM_INFO_1;
  _FORM_INFO_1    = packed Record
     Flags        : DWORD;
     pName        : PChar;
     Size         : TSize;
     ImageableArea: TRect;
  end;

  PDocInfo = ^TDocInfo;
  TDocInfo = packed record
    cbSize      : Integer;
    lpszDocName : PChar;
    lpszOutput  : PChar;
    lpszDatatype: PChar;
    fwType      : DWORD;
  end;
  
  PDOC_INFO_1 = ^DOC_INFO_1;
  DOC_INFO_1 = packed record
    DocName     : PChar;
    OutputFile  : PChar;
    DataType    : PChar;
  end;

  PPRINTER_INFO_1 = ^_PRINTER_INFO_1;
  _PRINTER_INFO_1 = packed Record
     Flags        : DWORD;
     pDescription : PChar;
     pName        : PChar;
     pComment     : PChar;
  end;

  PPRINTER_INFO_2 = ^_PRINTER_INFO_2;
  _PRINTER_INFO_2 = packed Record
     pServerName     : PChar;
     pPrinterName    : PChar;
     pShareName      : PChar;
     pPortName       : PChar;
     pDriverName     : PChar;
     pComment        : PChar;
     pLocation       : PChar;
     pDevMode        : PDeviceMode;
     pSepFile        : PChar;
     pPrintProcessor : PChar;
     pDatatype       : PChar;
     pParameters     : PChar;
     pSecurityDescriptor : Pointer;
     Attributes      : DWORD;
     Priority        : DWORD;
     DefaultPriority : DWORD;
     StartTime       : DWORD;
     UntilTime       : DWORD;
     Status          : DWORD;
     cJobs           : DWORD;
     AveragePPM      : DWORD;
  end;

  PPRINTER_INFO_4 = ^_PRINTER_INFO_4;
  _PRINTER_INFO_4 = packed Record
     pPrinterName : PChar;
     pServerName  : PChar;
     Attributes   : DWORD;
  end;

  PPRINTER_INFO_5 = ^_PRINTER_INFO_5;
  _PRINTER_INFO_5 = packed Record
     pPrinterName : PChar;
     pPortName    : PChar;
     Attributes   : DWORD;
     DeviceNotSelectedTimeout : DWORD;
     TransmissionRetryTimeout : DWORD;
  end;


function OpenPrinter(pPrinterName : PChar;           // printer or server name
                 var phPrinter    : THandle;         // printer or server handle
                     pDefaults    : PPrinterDefaults // printer defaults
                     ) : BOOL; stdCall; external LibWinSpool name 'OpenPrinterA';
                     
function GetPrinter(hPrinter : THandle;
    Level : DWORD;
    pPrinterEnum: Pointer;
    cbBuf : DWORD;
    var pcbNeeded
   ) : BOOL; stdcall; external LibWinSpool name 'GetPrinterA';

function ClosePrinter(hPrinter : THandle  //handle to printer object
                     ) : BOOL;  stdCall; external LibWinSpool name 'ClosePrinter';

function PrinterProperties(hWnd : THandle;
       hPrinter : THandle) : BOOL; stdcall; external LibWinSpool name 'PrinterProperties';


function EnumPrinters(Flags: DWORD;  //Printer objet type
                      Name : PChar;  //Name of printer object
                      Level: DWORD;  //Information level
                      pPrinterEnum: Pointer; //Printer information buffer
                      cbBuf: DWORD; //Size of printer information buffer
                  var pcbNeeded,    //Bytes recieved or required
                      pcReturned: DWORD //Number of printers enumerated
                      ): BOOL; stdcall; external LibWinSpool name 'EnumPrintersA';
{Unsuported on W95/98/Me :o(
Function EnumForms(
      hPrinter  : THandle;  // handle to printer object
      Level     : DWORD;    // data level
      pForm     : Pointer;  // form information buffer
     cbBuf     : DWord;    // size of form information buffer
  var pcbNeeded : DWORD;    // bytes received or required
  var pcReturned: DWORD     // number of forms received

): BOOL; stdcall; external LibWinSpool name 'EnumFormsA';}

{Function not compatible with all versions of Windows
function GetDefaultPrinter(
               pszBuffer   : PChar; // printer name buffer
           var pcchBuffer  : DWord  // size of name buffer
           ) : BOOL stdcall; external LibWinSpool name 'GetDefaultPrinterA';
}

function AdvancedDocumentProperties(
             hWnd           : HWND;        // handle to parent window
             hPrinter       : THandle;     // handle to printer object
             pDeviceName    : PChar;       // driver name
             pDevModeOutput : PDeviceMode; // modified device mode data
             pDevModeInput  : PDeviceMode  // original device mode data
             ): Longint; stdcall; external LibWinSpool name 'AdvancedDocumentPropertiesA';


function DocumentProperties(
      hWnd          :HWND;        // handle to parent window
      hPrinter      :THandle;     // handle to printer object
      pDeviceName   :Pchar;       // device name
      pDevModeOutput:PdeviceMode; // modified device mode
      pDevModeInput :PdeviceMode; // original device mode
      fMode         :DWORD        // mode options
      ): Longint; stdcall; external LibWinSpool name 'DocumentPropertiesA';

function DeviceCapabilities(pDevice, pPort: PChar; fwCapability: Word; pOutput: PChar;
  DevMode: PDeviceMode): Integer; stdcall; external LibWinSpool name 'DeviceCapabilitiesA';

function GetProfileString(lpAppName:PChar; lpKeyName:PChar; lpDefault:PChar; lpReturnedString:PChar; nSize:DWORD):DWORD; stdcall; external 'kernel32' name 'GetProfileStringA';

function PrintDlg(var lppd : tagPD): BOOL; stdcall; external 'comdlg32.dll'  name 'PrintDlgA';
function PageSetupDlg(var lppd : tagPSD): BOOL; stdcall; external 'comdlg32.dll'  name 'PageSetupDlgA';
function CommDlgExtendedError: DWORD; stdcall; external 'comdlg32.dll'  name 'CommDlgExtendedError';

function CreateIC(lpszDriver, lpszDevice, lpszOutput: PChar; lpdvmInit: PDeviceMode): HDC; stdcall; external 'gdi32.dll' name 'CreateICA';
function CreateDC(lpszDriver, lpszDevice, lpszOutput: PChar; lpdvmInit: PDeviceMode): HDC; stdcall; external 'gdi32.dll' name 'CreateDCA';
function DeleteDC(DC: HDC): BOOL; stdcall; external 'gdi32.dll' name 'DeleteDC';
function StartDoc(DC: HDC; Inf : PDocInfo): Integer; stdcall; external 'gdi32.dll' name 'StartDocA';
function EndDoc(DC: HDC): Integer; stdcall;  external 'gdi32.dll' name 'EndDoc';
function StartPage(DC: HDC): Integer; stdcall; external 'gdi32.dll' name 'StartPage';
function EndPage(DC: HDC): Integer; stdcall; external 'gdi32.dll' name 'EndPage';
function AbortDoc(DC: HDC): Integer; stdcall; external 'gdi32.dll' name 'AbortDoc';
function GlobalFree(HMem: HGlobal): HGlobal; stdcall; external 'kernel32.dll' name 'GlobalFree';

function StartDocPrinter(hPrinter:THANDLE; Level:DWORD; DocInfo:PByte):DWORD; stdcall; external LibWinSpool name 'StartDocPrinterA';
function StartPagePrinter(hPrinter:THANDLE):DWORD; stdcall; external LibWinSpool name 'StartPagePrinter';
function EndDocPrinter(hprinter:THANDLE):BOOL; stdcall; external LibWinSpool name 'EndDocPrinter';
function EndPagePrinter(hprinter:THANDLE):BOOL; stdcall; external LibWinSpool name 'EndPagePrinter';
function AbortPrinter(hPrinter:THANDLE):BOOL; stdcall; external LibWinSpool name 'AbortPrinter';
function WritePrinter(hPrinter:THANDLE; Buffer:Pointer; Count:DWord; Written:PDWORD):BOOL; stdcall; external LibWinSpool name 'WritePrinter';

implementation

{ TPrinterDevice }

destructor TPrinterDevice.destroy;
begin
  ReallocMem(DevMode, 0);
  inherited destroy;
end;

end.

