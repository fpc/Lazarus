{%MainUnit ../osprinters.pas}
{
  Author: Olivier Guilbaud

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Abstract:
    This unit provide an access at Printers for Win32

  History
    04/03/2005 OG - Fix build (from Jesus)
                  - Fix select printer dialog.
    09/03/2005 OG - Includes files for osPrinters

}

uses
  Classes, SysUtils, LazUTF8, Printers, LCLType, Windows;


Type
  THandleType = (htNone, htIC, htDC);

  { TWinPrinter }

  TWinPrinter = class(TPrinter)
  private
    //fDefaultPrinter : String;
    fLastHandleType : THandleType;
    fDC             : HDC;
    fPrinterHandle  : THandle;
    procedure SetIC;
    procedure SetDC;
    procedure ClearDC;
    procedure FreeDC;
    function UpdateDevMode(APrinterIndex:Integer): boolean;

  protected
    function GetDefaultPrinter: string;
    procedure DoBeginDoc; override;
    procedure DoNewPage; override;
    procedure DoEndDoc(aAborded : Boolean); override;
    procedure DoAbort; override;

    procedure DoEnumPrinters(Lst : TStrings); override;
    procedure DoResetPrintersList; override;
    
    procedure DoEnumPapers(Lst : TStrings); override;
    function DoGetPaperName: string; override;
    function DoGetDefaultPaperName: string; override;
    procedure DoSetPaperName(aName : string); override;
    function DoGetPaperRect(aName : string; Var aPaperRc : TPaperRect) : Integer; override;

    procedure DoEnumBins(Lst : TStrings); override;
    function DoGetDefaultBinName: string; override;
    function DoGetBinName: string; override;
    procedure DoSetBinName(aName: string); override;
    procedure DoEnumFonts(Lst: TStrings); override;

    function DoSetPrinter(aName : string): Integer; override;
    
    function DoGetCopies : Integer; override;
    procedure DoSetCopies(aValue : Integer); override;
    function DoGetOrientation: TPrinterOrientation; override;
    procedure DoSetOrientation(aValue : TPrinterOrientation); override;

    function GetXDPI: Integer; override;
    function GetYDPI: Integer; override;
    function GetPrinterType: TPrinterType;override;
    function DoGetPrinterState: TPrinterState;override;
    function GetCanPrint: Boolean;override;
    function GetCanRenderCopies : Boolean;override;
    function GetHandlePrinter : HDC;
    procedure SetHandlePrinter(aValue : HDC);
    procedure RawModeChanging; override;
    procedure PrinterSelected; override;
    procedure DoDestroy; override;
  public
    constructor Create; override;
    
    function Write(const Buffer; Count:Integer; out Written: Integer): Boolean; override;

    //Warning not portable functions here
    procedure AdvancedProperties;
    //Warning it is a not potable property
    property Handle : HDC read GetHandlePrinter write SetHandlePrinter;
    
  end;
  
