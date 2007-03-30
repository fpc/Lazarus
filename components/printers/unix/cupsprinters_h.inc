{%MainUnit ../osprinters.pp}
{
 ***************************************************************************
                                cupsprinters_h.inc
                                ------------
                               Printer object
                     Initial Revision  : Mon Nov 05 2002

 ***************************************************************************

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
 
  Author: Tony Maro (printers.pas)
          Olivier Guilbaud (cupsprinters.pas for CUPS implementation)

  Abstract :
    CUPS support :
      With this code, you can select an printer, modify its options (copies,
      format paper, orientation, job priority, ...)
      The height and with of the selected paper it's used for initialize the
      height and width of PostScript canvas (idem for orientation).
      
      see samples for uses.

  history
    nov   04 2003 OG - Add CUPS support
    mars  09 2004 OG - Add SetJobState methode for initialize a state of job
    apr   21 2004 OG - Fixe crash with second call of Printer.ExecuteSetup;
    sept  07 2004 OG - Add Media property
    sept  12 2004 OG - Fix EndDoc bug , second bug :o(
                     - Modify cupsPrintFile methode. If aFineName it's not
                       specified. The filename it's OutputName
                     - Delete TestAll methode
    sept  29 2004 OG - Rebuild object with new Printers unit.
    mars  08 2005 OG - Rename 2 methods
                     - Dynlink
    mars  09 2005 OG - Modifications for Printer4Lazarus pakage
    
-----------------------------------------------------------------------------}

{ ---------------------------------------------------------------------
  This code is heavily based on Tony Maro's initial Printers.pas
  implementation in the LCL, but was adapted to work with CUPS.
  ---------------------------------------------------------------------}
uses
  Classes, SysUtils, LCLProc, PostScriptCanvas, Printers, Dialogs, Controls,
  CUPSDyn;
  
type
  TCUPSPrinterState = (
    cpsDefaultPaperNameValid,
    cpsOrientationValid,
    cpsPaperNameValid,
    cpsCopiesValid,
    cpsPaperRectValid
    );
  TCUPSPrinterStates = set of TCUPSPrinterState;
  
  TPaperItem = record
    PaperName: string[40];
    PhysicalRect: TRect;
    WorkRect: TRect;
    WinCode: Integer;
  end;
  
  { TCUPSPrinter }

  TCUPSPrinter = Class(TPrinter)
  private
     fcupsPrinters: Pcups_dest_t;    //Printers avaible
     fcupsPrinter : Pcups_dest_t;    //Selected printer
     fcupsHttp    : Phttp_t;         //Server connection
     fcupsPPD     : Pppd_file_t;     //PPD file of selected printer
     fcupsPPDName : String;          //File name of PPD file (temporary copy)
     fcupsOptions : Pcups_option_t;  //Options selected
     fcupsNumOpts : Integer;         //Number of Options
     fStates      : TCUPSPrinterStates;
     fCachedGetDefaultPaperName: string;
     fCachedOrientation: TPrinterOrientation;
     fCachedPaperName: string;
     fCachedCopies: integer;
     fCachePaperRectName: string;
     fCachePaperRect: TPaperRect;
     fCachePaperRectResult: Integer;
     FBeginDocCount: Integer;
     fRawModeStream: TMemoryStream;

     function GetCupsRequest : Pipp_t;
     procedure DoCupsConnect;

  private
     fPapers: array of TPaperItem;
     fCupsPapersCount: Integer;
     fCurrentPaper: Integer;
     procedure CreatePapers;
     function  IndexOfPaper(aPaperName: string): integer;
     function  InternalGetDefPaperName: string;
     function  InternalGetCurPaperName: string;
     procedure InternalGetPaperRect(aPaperName:string; var PaperRect:TPaperRect);

  protected
     function GetCanvasRef : TPrinterCanvasRef; override;

     procedure DoBeginDoc; override;
     procedure DoEndDoc(aAborded : Boolean); override;

     
     procedure DoResetPrintersList; override;

     procedure DoEnumPrinters(Lst : TStrings); override;
     procedure DoEnumPapers(Lst : TStrings); override;
     function DoSetPrinter(aName : string): Integer; override;
     function DoGetCopies : Integer; override;
     procedure DoSetCopies(aValue : Integer); override;
     function DoGetOrientation: TPrinterOrientation; override;
     procedure DoSetOrientation(aValue : TPrinterOrientation); override;
     function DoGetDefaultPaperName: string; override;
     function DoGetPaperName: string; override;
     procedure DoSetPaperName(aName : string); override;
     function DoGetPaperRect(aName : string;
                             var aPaperRc: TPaperRect): Integer; override;
     function DoGetPrinterState: TPrinterState; override;
     
     function GetPrinterType : TPrinterType; override;
     function GetCanPrint : Boolean; override;
     
     function GetXDPI: Integer; override;
     function GetYDPI: Integer; override;
     function Write(const Buffer; Count:Integer; var Written: Integer): Boolean; override;

     {-------------------------------------------------
         SPECIFIC CUPS METHODS OR PROPERTIES
     --------------------------------------------------}
     procedure FreeOptions;
     procedure SetOptionsOfPrinter;

     procedure GetEnumAttributeString(aName : PChar; Lst : TStrings);
     function GetAttributeInteger(aName : PChar;DefaultValue : Integer) : Integer;
     function GetAttributeString(aName: PChar; const DefaultValue : string): string;
     function GetAttributeBoolean(aName : PChar; DefaultValue : Boolean) : Boolean;
     function EnumPPDChoice(Lst : TStrings; const aKeyWord : string;
        OptNames: TStrings = nil) : Integer;

     procedure cupsAddOption(aName,aValue: string);
     function cupsGetOption(aKeyWord: string): String;
  public
    constructor Create; override;
    destructor Destroy; override;
    
    {-------------------------------------------------
        SPECIFIC CUPS METHODS OR PROPERTIES
    --------------------------------------------------}
    procedure SetJobState(aJobId : LongInt; aOp : ipp_op_t);
    function PrintFile(aFileName: String): longint;
    procedure GetDebugOptions(Lst : TStrings);
  end;
  
