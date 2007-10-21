{%MainUnit ../osprinters.pas}
{**************************************************************
Implementation for carbonprinter
***************************************************************}
Uses InterfaceBase, LCLIntf;

{ TCarbonPrinter }

constructor TCarbonPrinter.Create;
begin
  inherited Create;
  //IMPLEMENT ME
end;

destructor TCarbonPrinter.Destroy;
begin
  inherited Destroy;
  //IMPLEMENT ME
end;

function TCarbonPrinter.Write(const Buffer; Count: Integer;
  var Written: Integer): Boolean;
begin
  CheckRawMode(True);
  //IMPLEMENT ME
end;

function TCarbonPrinter.GetHandlePrinter : HDC;
begin
  //IMPLEMENT ME
  result := 0;
end;


procedure TCarbonPrinter.SetHandlePrinter(aValue : HDC);
begin
  CheckRawMode(False);
  //IMPLEMENT ME
end;

procedure TCarbonPrinter.RawModeChanging;
begin
  //IMPLEMENT ME
end;

function TCarbonPrinter.GetXDPI: Integer;
begin
//IMPLEMENT ME
end;

function TCarbonPrinter.GetYDPI: Integer;
begin
//IMPLEMENT ME
end;

procedure TCarbonPrinter.DoBeginDoc;
begin
  inherited DoBeginDoc;
//IMPLEMENT ME
end;

procedure TCarbonPrinter.DoNewPage;
begin
  inherited DoNewPage;
//IMPLEMENT ME
end;

procedure TCarbonPrinter.DoEndDoc(aAborded: Boolean);
begin
  inherited DoEndDoc(aAborded);
//IMPLEMENT ME
end;

procedure TCarbonPrinter.DoAbort;
begin
  inherited DoAbort;
//IMPLEMENT ME
end;

//Enum all defined printers. First printer it's default
procedure TCarbonPrinter.DoEnumPrinters(Lst: TStrings);
begin
//IMPLEMENT ME
end;

procedure TCarbonPrinter.DoResetPrintersList;
begin
//IMPLEMENT ME
  inherited DoResetPrintersList;
end;

procedure TCarbonPrinter.DoEnumPapers(Lst: TStrings);
begin
  inherited DoEnumPapers(Lst);
//IMPLEMENT ME
end;

function TCarbonPrinter.DoGetPaperName: string;
begin
  Result:=inherited DoGetPaperName;
//IMPLEMENT ME
end;

function TCarbonPrinter.DoGetDefaultPaperName: string;
begin
  Result:=inherited DoGetDefaultPaperName;
//IMPLEMENT ME
end;

procedure TCarbonPrinter.DoSetPaperName(aName: string);
begin
  inherited DoSetPaperName(aName);
//IMPLEMENT ME
end;

function TCarbonPrinter.DoGetPaperRect(aName: string;
  var aPaperRc: TPaperRect): Integer;
begin
  Result:=Inherited DoGetPaperRect(aName,aPaperRc);
//IMPLEMENT ME
end;

function TCarbonPrinter.DoSetPrinter(aName: string): Integer;
begin
  Result:=inherited DoSetPrinter(aName);
//IMPLEMENT ME
end;

function TCarbonPrinter.DoGetCopies: Integer;
begin
  Result:=inherited DoGetCopies;
//IMPLEMENT ME
end;

procedure TCarbonPrinter.DoSetCopies(aValue: Integer);
begin
  inherited DoSetCopies(aValue);
//IMPLEMENT ME
end;

function TCarbonPrinter.DoGetOrientation: TPrinterOrientation;
begin
  Result:=inherited DoGetOrientation;
//IMPLEMENT ME
end;

procedure TCarbonPrinter.DoSetOrientation(aValue: TPrinterOrientation);
begin
  inherited DoSetOrientation(aValue);
//IMPLEMENT ME
end;

function TCarbonPrinter.GetPrinterType: TPrinterType;
begin
  Result:=ptLocal;
//IMPLEMENT ME
end;


function TCarbonPrinter.DoGetPrinterState: TPrinterState;
begin
  Result:=psNoDefine;
//IMPLEMENT ME
end;

function TCarbonPrinter.GetCanPrint: Boolean;
begin
//IMPLEMENT ME
 Result := (DoGetPrinterState <> psStopped);
end;

function TCarbonPrinter.GetCanRenderCopies: Boolean;
begin
 Result := inherited GetCanRenderCopies;
//IMPLEMENT ME
end;

initialization
  Printer:=TCarbonPrinter.Create;
