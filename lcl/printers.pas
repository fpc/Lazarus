{
 /***************************************************************************
                                printers.pas
                                ------------
                               Printer object
                     Initial Revision  : Mon Nov 05 2002

 ***************************************************************************/

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
 
  Author: Tony Maro
}
unit Printers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, PostScriptPrinter;
  
type
  TPrinterOutputType = (
    otStdOut,
    otFile,
    otPrinter,
    otPDF
  );

  { TPrinter }
  
  TPrinter = class(TObject)
  private
    FOutputName: String;
    FOutputType: TPrinterOutputType;
    FPostScript: TPostScript;
    FCanvas: TPostScriptCanvas;
    //FFonts: TStrings;
    FPageNumber: Integer;
    //FPrinters: TStrings;
    //FPrinterIndex: Integer;
    FTitle: string;
    //FPrinting: Boolean;
    //FAborted: Boolean;
    //FCapabilities: TPrinterCapabilities;
    //State: TPrinterState;
    //DC: HDC;
    //DevMode: PDeviceMode;
    //DeviceMode: THandle;
    //FPrinterHandle: THandle;
    //procedure SetState(Value: TPrinterState);
    FPageHeight: Integer;
    FPageWidth: Integer;
    function GetCanvas: TPostScriptCanvas;
    //function GetNumCopies: Integer;
    //function GetFonts: TStrings;
    //function GetHandle: HDC;
    //function GetOrientation: TPrinterOrientation;
    function GetPageHeight: Integer;
    function GetPageWidth: Integer;
    procedure SetOutputName(const AValue: String);
    procedure SetOutputType(const AValue: TPrinterOutputType);
    procedure SetTitle(const AValue: string);
    //function GetPrinterIndex: Integer;
    //procedure SetPrinterCapabilities(Value: Integer);
    //procedure SetPrinterIndex(Value: Integer);
    //function GetPrinters: TStrings;
    //procedure SetNumCopies(Value: Integer);
    //procedure SetOrientation(Value: TPrinterOrientation);
    //procedure SetToDefaultPrinter;
    //procedure CheckPrinting(Value: Boolean);
    //procedure FreePrinters;
    //procedure FreeFonts;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Abort;
    procedure BeginDoc;
    procedure EndDoc;
    procedure NewPage;
    //procedure GetPrinter(ADevice, ADriver, APort: PChar; var ADeviceMode: THandle);
    //procedure SetPrinter(ADevice, ADriver, APort: PChar; ADeviceMode: THandle);
    //procedure Refresh;
    //property Aborted: Boolean read FAborted;
    property Canvas: TPostScriptCanvas read GetCanvas;
    //property Capabilities: TPrinterCapabilities read FCapabilities;
    //property Copies: Integer read GetNumCopies write SetNumCopies;
    //property Fonts: TStrings read GetFonts;
    //property Handle: HDC read GetHandle;
    //property Orientation: TPrinterOrientation read GetOrientation write SetOrientation;
    property PageHeight: Integer read GetPageHeight;
    property PageWidth: Integer read GetPageWidth;
    property PageNumber: Integer read FPageNumber;
    //property PrinterIndex: Integer read GetPrinterIndex write SetPrinterIndex;
    //property Printing: Boolean read FPrinting;
    //property Printers: TStrings read GetPrinters;
    property Title: string read FTitle write SetTitle;
    property PostScript: TPostScript read FPostScript;
    property OutputType: TPrinterOutputType read FOutputType write SetOutputType;
    property OutputName: String read FOutputName write SetOutputName;
  end;
  
function Printer: TPrinter;

implementation

var
  FPrinter: TPrinter;

{ TPrinter ------------------------------------------------------------------ }

function TPrinter.GetCanvas: TPostScriptCanvas;
begin

     Result := FCanvas;

end;

{ Later hook to pull from printer capabilities? }
function TPrinter.GetPageHeight: Integer;
begin

     Result := FPageHeight;

end;

function TPrinter.GetPageWidth: Integer;
begin

     Result := FPageWidth;

end;

procedure TPrinter.SetOutputName(const AValue: String);
begin
  if FOutputName=AValue then exit;
  FOutputName:=AValue;
end;

procedure TPrinter.SetOutputType(const AValue: TPrinterOutputType);
begin
  if FOutputType=AValue then exit;
  FOutputType:=AValue;
end;

procedure TPrinter.SetTitle(const AValue: string);
begin
  if FTitle=AValue then exit;
  FTitle:=AValue;
  
  FPostScript.Title := AValue;
end;

constructor TPrinter.Create;
begin

     FPostScript := TPostScript.Create;
     FCanvas := FPostScript.Canvas;
     FOutputType := otPrinter;
     FOutputName := '';

end;

destructor TPrinter.Destroy;
begin
  FPostScript.Free;

  inherited Destroy;
end;

{ Abort current print job and start over }
procedure TPrinter.Abort;
begin

end;

{ Begin a new document }
procedure TPrinter.BeginDoc;
begin
     // also clear the canvas just in case
     FPostScript.BeginDoc;
     
end;

{ Close current page and print document }
procedure TPrinter.EndDoc;
var
   printcmd: String;
begin

    // print it using postscript
    FPostScript.EndDoc;

    if FOutputType = otFile then begin
       try
          FPostScript.Document.SaveToFile(FOutputName);
       except
          raise exception.create('Error printing to file "'+FOutputName+'"');
       end;
    end;
    
    {$ifdef linux}
    if FOutputType = otPDF then begin
       printcmd := 'ps2pdf /tmp/print.ps '+FOutputName; // I'm betting it's in the users path
       FPostScript.Document.SaveToFile('/tmp/print.ps'); // generate random name later
       with TProcess.Create(nil) do
       try
         CommandLine := printcmd;
         Options := [poWaitOnExit];
         Execute;
       finally
         Free;
       end;
       
       With TProcess.Create(nil) do
       try
         CommandLine := 'rm -f /tmp/print.ps';
         Execute;
       finally
         Free;
       end;
    end;
    {$endif}


    {$ifdef linux}
    if FOutputType = otPrinter then begin
       printcmd := '/usr/bin/lpr';
       if FOutputName <> '' then PrintCmd := PrintCmd+' -P'+FOutputName;

        FPostScript.Document.SaveToFile('/tmp/print.ps');   // generate a random name later
        // linux specific printing
        With TProcess.Create(nil) do
        Try
          CommandLine:=printcmd+' /tmp/print.ps'; // add printer selection later
          //options:=[poUsePipes];
          options:=[poWaitOnExit];
          Execute;
          //FPostScript.Document.SaveToStream(Input);
          //Input.Write(longint(FPostScript.Document.Text),length(FPostScript.Document.Text));
          //Output.Write(FPostScript.Document.Text);
        finally
          Free;
        end;

        With TProcess.Create(nil) do
        Try
          CommandLine:='rm -f /tmp/print.ps'; // add printer selection later
          options := [poWaitOnExit];
          Execute;
        finally
          Free;
        end;
    end;
    {$endif}

end;

{ Close current page and start a new one }
procedure TPrinter.NewPage;
begin

     if FCanvas is TPostScriptCanvas then begin
        PostScript.NewPage;
     end;


end;



{ Create the printer object on first access }
function Printer: TPrinter;
begin
  if FPrinter = nil then FPrinter := TPrinter.Create;
  Result := FPrinter;
end;





{ This part MAY not be needed?  Couldn't define it with the variable though... }

initialization
  FPrinter := nil;

{ Blow away the FPrinter if created }
finalization
  if assigned(FPrinter) then FPrinter.Free;

end.

