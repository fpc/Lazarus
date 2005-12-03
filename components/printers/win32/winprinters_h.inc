{
  Author: Olivier Guilbaud

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
  TWinPrinter = class(TPrinter)
  private
    fDefaultPrinter : String;
    fLastHandleType : Byte; //0=None 1=IC 2=DC
    fDC             : HDC;
    fPrinterHandle  : THandle;
    
    procedure SetIC;
    procedure SetDC;
    procedure ClearDC;
    

  protected
    //function GetDefaultPrinter : String;
    procedure DoBeginDoc; override;
    procedure DoNewPage; override;
    procedure DoEndDoc(aAborded : Boolean); override;
    procedure DoAbort; override;

    procedure DoEnumPrinters(Lst : TStrings); override;
    procedure DoResetPrintersList; override;
    
    procedure DoEnumPapers(Lst : TStrings); override;
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
    function GetHandlePrinter : HDC;
    procedure SetHandlePrinter(aValue : HDC);
  public
    constructor Create; override;
    destructor Destroy; override;
    

    //Warning not portable functions here
    procedure Properties;
    //Warning it is a not potable property
    property Handle : HDC read GetHandlePrinter write SetHandlePrinter;
    
  end;
  

