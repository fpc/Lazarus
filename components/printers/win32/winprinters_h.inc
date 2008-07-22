{%MainUnit ../osprinters.pas}
{
  Author: Olivier Guilbaud

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Abstract:
    This unit provide an access at Printers for Win32

  History
    04/03/2005 OG - Fix build (from Jesus)
                  - Fix select printer dialog.
    09/03/2005 OG - Includes files for osPrinters

}

uses
  Classes, SysUtils,Printers,LCLType,{Forms,}Windows;//,dialogs;

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
  public
    constructor Create; override;
    destructor Destroy; override;
    
    function Write(const Buffer; Count:Integer; var Written: Integer): Boolean; override;

    //Warning not portable functions here
    procedure AdvancedProperties;
    //Warning it is a not potable property
    property Handle : HDC read GetHandlePrinter write SetHandlePrinter;
    
  end;
  
