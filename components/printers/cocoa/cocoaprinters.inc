{%MainUnit ../osprinters.pas}
{**************************************************************
Implementation for carbonprinter
***************************************************************}
uses InterfaceBase, LCLIntf, LCLProc;

const
  CleanPMRect: PMRect = (top: 0; left: 0; bottom: 0; right: 0);
  CleanPMOrientation: PMOrientation = 0;

{ TCocoaPrinterView }

function TCocoaPrinterView.initWithFrame(frameRect: NSRect): id;
begin
  PageMin := 1;
  PageMax := 1;
  PageFrom := 1;
  PageTo := 1;
  Result:=inherited initWithFrame(frameRect);
  //Image := NSImage.alloc.initWithSize(Size);
end;

procedure TCocoaPrinterView.dealloc;
begin
  //Image.release();
  inherited dealloc();
end;

procedure TCocoaPrinterView.drawRect(dirtyRect: NSRect);
var
  pageHeight: Double;
  lRect: NSRect;
  lCurPage: Integer;
begin
  // image page printing alternative
  //Image.drawInRect_fromRect_operation_fraction(Self.frame, NSZeroRect, NSCompositeSourceAtop, 1.0);

  if Canvas = nil then Exit;

  pageHeight := calculatePrintHeight();
  lRect := updateSize(False);

  // figure out which page this is
  lCurPage := Round((lRect.size.height - dirtyRect.origin.y) / pageHeight);

  Canvas.DrawRecording(Self, dirtyRect, lCurPage - PageFrom);
end;

// Return the number of pages available for printing
function TCocoaPrinterView.knowsPageRange(range: NSRangePointer): Boolean;
begin
  updateSize(True);
  range^.location := PageFrom;
  range^.length := PageTo - PageFrom + 1;
  Result := True;
end;

function TCocoaPrinterView.rectForPage(page: NSInteger): NSRect;
var
  lBounds: NSRect;
  pageHeight, pageWidth: Double;
begin
  pageHeight := calculatePrintHeight();
  pageWidth := calculatePrintWidth();
  // page Y starting pos is Bottom-Left of page 1 - (pagenr-zero-based) * pageHeight, so:
  // pageHeight * ((PageTo - PageFrom) - (page - PageFrom)), which simplifies to:
  Result := NSMakeRect(0, pageHeight * (PageTo - page),
    pageWidth, pageHeight);
end;

// Calculate the vertical size of the view that fits on a single page
function TCocoaPrinterView.calculatePrintHeight: Double;
var
  pi: NSPrintInfo;
  paperSize: NSSize;
  pageHeight, scale: Double;
begin
  // Obtain the print info object for the current operation
  pi := NSPrintOperation.currentOperation.printInfo;

  // Calculate the page height in points
  paperSize := pi.paperSize;
  pageHeight := paperSize.height - pi.topMargin - pi.bottomMargin;

  // Convert height to the scaled view
  scale := pi.dictionary.objectForKey(NSPrintScalingFactor).floatValue;
  Result := pageHeight / scale;
end;

function TCocoaPrinterView.calculatePrintWidth: Double;
var
  pi: NSPrintInfo;
  paperSize: NSSize;
  pageWidth, scale: Double;
begin
  // Obtain the print info object for the current operation
  pi := NSPrintOperation.currentOperation.printInfo;

  // Calculate the page width in points
  paperSize := pi.paperSize;
  pageWidth := paperSize.width - pi.leftMargin - pi.rightMargin;

  // Convert height to the scaled view
  scale := pi.dictionary.objectForKey(NSPrintScalingFactor).floatValue;
  Result := pageWidth / scale;
end;

function TCocoaPrinterView.updateSize(ADoSetFrame: Boolean): NSRect;
var
  Size: NSSize;
begin
  Size.height := calculatePrintHeight();
  Size.height := Size.height * (PageTo - PageFrom + 1);
  Size.width := calculatePrintWidth();
  if ADoSetFrame then
    Self.setFrameSize(Size);
  Result := NSMakeRect(0, 0, Size.width, Size.height);
end;

{ TCocoaPrinter }

function TCocoaPrinter.CreatePageFormat(APaper: String): PMPageFormat;
var
  I: Integer;
  S: TStringList;
const
  SName = 'CreatePageFormat';
begin
{  if APaper = '' then
  begin
    I := -1;
    S := nil;
  end
  else
  begin
    S := TStringList.Create;
    BeginEnumPapers(S);
    I := S.IndexOf(APaper);
  end;
    
  try
    if I < 0 then
    begin
      Result:=nil;
      if OSError(PMCreatePageFormat(Result), Self, SName, 'PMCreatePageFormat') then
        raise EPrinter.Create('Error initializing printing for Carbon: Unable to create page format!');

      OSError(PMSessionDefaultPageFormat(PrintSession, Result), Self, SName, 'PMSessionDefaultPageFormat');
    end
    else
    begin
      OSError(PMCreatePageFormatWithPMPaper(Result,
          PMPaper(CFArrayGetValueAtIndex(FPaperArray, I))),
        Self, SName, 'PMCreatePageFormatWithPMPaper');

    end;
  finally
    if S <> nil then
    begin
      EndEnumPapers;
      S.Free;
    end;
  end;  }
end;

function TCocoaPrinter.ValidatePageFormat: Boolean;
begin
  Result := False;
  if PMSessionValidatePageFormat(GetPrintSession(), GetPageFormat(), @Result) <> noErr then Exit;
end;

function TCocoaPrinter.ValidatePrintSettings: Boolean;
begin
  Result := False;
  if PMSessionValidatePrintSettings(GetPrintSession(), GetPrintSettings(), @Result) <> noErr then Exit;
end;

function TCocoaPrinter.GetCurrentCarbonPrinter: PMPrinter;
begin
  Result := nil;
  if PMSessionGetCurrentPrinter(GetPrintSession(), Result) <> noErr then Exit;
  { Result := CFStringToStr(PMPrinterGetName(P));
  if Trim(Result) = '' then
    Result := '';  }
end;

function TCocoaPrinter.GetPrintSession: PMPrintSession;
begin
  Result := FPrintInfo.PMPrintSession();
end;

function TCocoaPrinter.GetPrintSettings: PMPrintSettings;
begin
  Result := FPrintInfo.PMPrintSettings();
end;

function TCocoaPrinter.GetPageFormat: PMPageFormat;
begin
  Result := FPrintInfo.PMPageFormat();
end;

procedure TCocoaPrinter.BeginPage;
{var
  PaperRect: PMRect;  }
begin
  {if FBeginDocumentStatus = noErr then
  begin
    FNewPageStatus := PMSessionBeginPage(PrintSession, nil, nil);
    OSError(FNewPageStatus, Self, 'BeginPage', 'PMSessionBeginPage', '', kPMCancel);
    
    // update printer context
    if OSError(PMSessionGetCGGraphicsContext(PrintSession, FPrinterContext.CGContext),
      Self, 'BeginPage', 'PMSessionGetCGGraphicsContext') then
        FPrinterContext.Release
      else
        FPrinterContext.Reset;

    // translate the context from his paper (0,0) origin
    // to our working imageable area
    if PMGetAdjustedPaperRect(PageFormat, PaperRect{%H-})=noErr then
      CGContextTranslateCTM(FPrinterContext.CGContext, -PaperRect.left, -PaperRect.top);

    if Assigned(Canvas) then
      Canvas.Handle := HDC(FPrinterContext);
  end;   }
end;

procedure TCocoaPrinter.EndPage;
begin
  {FPrinterContext.Release;
  if Assigned(Canvas) then Canvas.Handle := 0;
  
  if FBeginDocumentStatus = noErr then
  begin
    if FNewPageStatus = noErr then
      OSError(PMSessionEndPage(PrintSession), Self, 'EndPage', 'PMSessionEndPage', '', kPMCancel);
  end;   }
end;

procedure TCocoaPrinter.FindDefaultPrinter;
{var
  P: PMPrinter;
  I, C: CFIndex;
  pa: CFArrayRef;  }
begin
  {pa:=nil;
  if OSError(PMServerCreatePrinterList(kPMServerLocal, pa),
    Self, 'DoEnumPrinters', 'PMServerCreatePrinterList') then Exit;

  if not Assigned(pa) then Exit;

  C := CFArrayGetCount(pa);
  for I := 0 to C - 1 do
  begin
    P := CFArrayGetValueAtIndex(pa, I);

    if PMPrinterIsDefault(P) then
    begin
      FDefaultPrinter := CFStringToStr(PMPrinterGetName(P));
      Break;
    end;
  end;
  CFRelease(pa);    }
end;

constructor TCocoaPrinter.Create;
var
  FPrintViewRect: NSRect;
begin
  inherited Create;

  FPrintViewRect := GetNSRect(0, 0, 1000, 1000);
  FPrintView := TCocoaPrinterView.alloc.initWithFrame(FPrintViewRect);
  FPrintView.Canvas := Canvas as TCocoaPrinterCanvas;

  FPrintOp := NSPrintOperation.printOperationWithView(FPrintView);
  FPrintInfo := FPrintOp.printInfo();

  //CreatePrintSettings;
  //FPageFormat := CreatePageFormat('');
  //FindDefaultPrinter;
  //UpdatePrinter;

  //DebugLn('Current ' + GetCurrentPrinterName);
  //DebugLn('Default ' + FDefaultPrinter);
end;

procedure TCocoaPrinter.DoDestroy;
begin
  FPrintView.release();
  
  inherited DoDestroy;
end;

function TCocoaPrinter.Write(const Buffer; Count: Integer;
  var Written: Integer): Boolean;
begin
  Result := False;
  CheckRawMode(True);

  DebugLn('TCocoaPrinter.Write Error: Raw mode is not supported for Cocoa!');
end;

procedure TCocoaPrinter.RawModeChanging;
begin
  //
end;

procedure TCocoaPrinter.Validate;
var
  P: String;
begin
  ValidatePrintSettings();
  ValidatePageFormat();
  
  // if target paper is not supported, use the default
  P := DoGetPaperName();
  if PaperSize.SupportedPapers.IndexOf(P) = -1 then
    DoSetPaperName(DoGetDefaultPaperName());
end;

procedure TCocoaPrinter.UpdatePrinter;
{var
  s: string;
  Res: PMResolution;}
begin
  {s := GetCurrentPrinterName;
  if trim(s) = '' then // Observed if Default printer set to "Use last printer", and no printing done
    s := '*';     // so select lcl default
  SetPrinter(s);
  // set the page format resolution
  Res := GetOutputResolution;
  PMSetResolution(PageFormat, Res);
  Validate; }
end;

function TCocoaPrinter.GetOutputResolution: PMResolution;
var
  res: OSStatus;
  FPrintSettings: PMPrintSettings;
begin
  FPrintSettings := GetPrintSettings();
  res := PMPrinterGetOutputResolution(GetCurrentCarbonPrinter(), FPrintSettings, Result);
  if res <> noErr then
  begin
    Result.vRes := 72;
    Result.hRes := 72;
  end;
end;

function TCocoaPrinter.DoDoGetPaperName(APageFormat: PMPageFormat): string;
var
  FPaper: PMPaper = nil;
  lCFString: CFStringRef = nil;
begin
  Result := '';
  if APageFormat = nil then APageFormat := GetPageFormat();
  if PMGetPageFormatPaper(APageFormat, FPaper) <> noErr then Exit;
  if PMPaperGetName(FPaper, lCFString) <> noErr then Exit;

  Result := CFStringToStr(lCFString);
end;

function TCocoaPrinter.GetXDPI: Integer;
var
  dpi: PMResolution;
begin
  dpi := GetOutputResolution;
  result := round(dpi.hRes);
end;

function TCocoaPrinter.GetYDPI: Integer;
var
  dpi: PMResolution;
begin
  dpi := GetOutputResolution;
  result := round(dpi.hRes);
end;

procedure TCocoaPrinter.DoBeginDoc;
begin
  inherited DoBeginDoc;
  
  //DebugLn('TCocoaPrinter.DoBeginDoc ' + DbgS(Printing));
  Validate;

  //FBeginDocumentStatus := PMSessionBeginCGDocument(PrintSession, PrintSettings, PageFormat);
  //OSError(FBeginDocumentStatus, Self, 'DoBeginDoc', 'PMSessionBeginCGDocument', '', kPMCancel);

  BeginPage;
end;

procedure TCocoaPrinter.DoNewPage;
begin
  inherited DoNewPage;

  EndPage;
  BeginPage;
end;

procedure TCocoaPrinter.DoEndDoc(aAborded: Boolean);
begin
  inherited DoEndDoc(aAborded);

  EndPage;
  FPrintOp.setShowsPrintPanel(False);
  FPrintOp.runOperation();
end;

procedure TCocoaPrinter.DoAbort;
begin
  inherited DoAbort;

  //OSError(PMSessionSetError(PrintSession, kPMCancel), Self, 'DoAbort', 'PMSessionSetError');
end;

//Enum all defined printers. First printer it's default
procedure TCocoaPrinter.DoEnumPrinters(Lst: TStrings);
var
  I, PrinterCount: CFIndex;
  NewPrinterNSName: NSString;
  NewPrinterName: String;
  FPrinterArray: NSArray;
  //NewPrinter: NSPrinter;
begin
  FPrinterArray := NSPrinter.printerNames();
  FPrinterArray.retain;
  if FPrinterArray = nil then Exit;

  for I := 0 to FPrinterArray.count - 1 do
  begin
    NewPrinterNSName := NSString(FPrinterArray.objectAtIndex(i));
    NewPrinterName := NSStringToString(NewPrinterNSName);

    // Felipe: This could be used for appending the printer to the TStrings, but what for?
    // also it would be hard to release the NSPrinter later
    //NewPrinter := NSPrinter.printerWithName(NewPrinterNSName);
    //NewPrinter.retain;

    //DebugLn(DbgS(I) + ' ' + PrinterName);
    if NewPrinterName = FDefaultPrinter then
      Lst.InsertObject(0, NewPrinterName, nil{TObject(NewPrinter)})
    else
      Lst.AddObject(NewPrinterName, nil{TObject(NewPrinter)});
  end;
end;

procedure TCocoaPrinter.DoResetPrintersList;
begin
  inherited DoResetPrintersList;
end;

// Cocoa doesn't support this =( We need to use Carbon here
// http://lists.apple.com/archives/cocoa-dev/2005/Nov/msg01227.html
// See Also "Using Cocoa and Core Printing Together"
// https://developer.apple.com/library/mac/technotes/tn2248/_index.html
procedure TCocoaPrinter.DoEnumPapers(Lst: TStrings);
var
  P: PMPaper;
  FPaperArray: CFArrayRef;
  I, C: CFIndex;
  CFString: CFStringRef;
  PaperName: String;
  CarbonCurrentPrinter: PMPrinter;
begin
  FPaperArray := nil;

  CarbonCurrentPrinter := GetCurrentCarbonPrinter();

  if PMPrinterGetPaperList(CarbonCurrentPrinter, FPaperArray) <> noErr then Exit;
  FPaperArray := CFRetain(FPaperArray);

  C := CFArrayGetCount(FPaperArray);
  for I := 0 to C - 1 do
  begin
    P := CFArrayGetValueAtIndex(FPaperArray, I);
    CFString:=nil;
    if PMPaperGetName(P, CFString) <> noErr then Continue;
    PaperName := CFStringToStr(CFString);
    Lst.Add(PaperName);
  end;

  if FPaperArray<>nil then
    CFRelease(FPaperArray);
end;

function TCocoaPrinter.DoGetPaperName: string;
begin
  Result := DoDoGetPaperName(FPrintInfo.PMPageFormat());
end;

function TCocoaPrinter.DoGetDefaultPaperName: string;
var
  FPageFormat: PMPageFormat;
begin
  Result := '';
  
  FPageFormat := CreatePageFormat('');
  Result := DoDoGetPaperName(FPageFormat);
end;

procedure TCocoaPrinter.DoSetPaperName(aName: string);
var
  FOrientation: TPrinterOrientation;
begin
  FOrientation := DoGetOrientation();
  {if FPageFormat <> nil then PMRelease(PMObject(FPageFormat));
  
  FPageFormat := CreatePageFormat(AName);
  DoSetOrientation(FOrientation);}
  
  ValidatePageFormat;
end;

function TCocoaPrinter.DoGetPaperRect(aName: string; var aPaperRc: TPaperRect
  ): Integer;
{var
  T: PMPageFormat;
  PaperRect, PageRect: PMRect;
  S: Double;
  O: PMOrientation;
  Res: PMResolution;
const
  SName = 'DoGetPaperRect';    }
begin
  {Result := -1;
  
  T := CreatePageFormat(AName);
  try
    // copy scale
    S:=0.0;
    OSError(PMGetScale(PageFormat, S), Self, SName, 'PMGetScale');
    OSError(PMSetScale(T, S), Self, SName, 'PMSetScale');
    
    // copy orientation
    O:=CleanPMOrientation;
    OSError(PMGetOrientation(PageFormat, O), Self, SName, 'PMGetOrientation');
    OSError(PMSetOrientation(T, O, False), Self, SName, 'PMSetOrientation');

    // copy resolution
    Res := GetOutputResolution;
    OSError(PMSetResolution(T, Res), self, SName, 'PMSetResolution');
    
    // update
    OSError(PMSessionValidatePageFormat(PrintSession, T, nil),
      Self, SName, 'PMSessionValidatePageFormat');

    PaperRect:=CleanPMRect;
    OSError(PMGetAdjustedPaperRect(T, PaperRect), Self, SName, 'PMGetAdjustedPaperRect');
    PageRect:=CleanPMRect;
    OSError(PMGetAdjustedPageRect(T, PageRect),  Self, SName, 'PMGetAdjustedPageRect');
  finally
    PMRelease(PMObject(T));
  end;
  
  ValidatePageFormat;
  
  APaperRc.PhysicalRect.Left := 0;
  APaperRc.PhysicalRect.Top := 0;
  APaperRc.PhysicalRect.Right := Round(PaperRect.right - PaperRect.left);
  APaperRc.PhysicalRect.Bottom := Round(PaperRect.bottom - PaperRect.top);
  
  APaperRc.WorkRect.Left := Round(-PaperRect.left);
  APaperRc.WorkRect.Top := Round(-PaperRect.top);
  APaperRc.WorkRect.Right := Round(PageRect.right - PageRect.left - PaperRect.left);
  APaperRc.WorkRect.Bottom := Round(PageRect.bottom - PageRect.top - PaperRect.top);
  
  Result := 1;  }
end;

function TCocoaPrinter.DoSetPrinter(aName: string): Integer;
{var
  S: TStringList;
  P: PMPrinter; }
begin
  {S := TStringList.Create;
  BeginEnumPrinters(S);
  try
    Result := S.IndexOf(AName);
    if Result >= 0 then
    begin
      //DebugLn('DoSetPrinter ' + DbgS(Result));
      //DebugLn('TCocoaPrinter.DoSetPrinter ' + AName + ' ' + DbgS(PrintSession) + ' ' + DbgS(Printers.Objects[Result]));
      P := PMPrinter(CFArrayGetValueAtIndex(FPrinterArray, Integer(S.Objects[Result])));
      PMRetain(PMObject(P));
      if OSError(PMSessionSetCurrentPMPrinter(PrintSession, P),
        Self, 'DoSetPrinter', 'PMSessionSetCurrentPMPrinter') then
          raise EPrinter.CreateFmt('The system is unable to select printer "%s"!', [AName]);
    end;
  finally
    EndEnumPrinters;
    S.Free;
  end;   }
end;

function TCocoaPrinter.DoGetCopies: Integer;
var
  NumCopies: UInt32;
begin
  Result := inherited DoGetCopies;
  NumCopies := 0;
  if PMGetCopies(GetPrintSettings(), NumCopies) <> noErr then Exit;
  Result := NumCopies;
end;

procedure TCocoaPrinter.DoSetCopies(aValue: Integer);
begin
  inherited DoSetCopies(AValue);
  if PMSetCopies(GetPrintSettings(), AValue, False) <> noErr then Exit;
  FPrintInfo.updateFromPMPrintSettings();
  
  ValidatePrintSettings;
end;

function TCocoaPrinter.DoGetOrientation: TPrinterOrientation;
var
  FOrientation: PMOrientation;
  FPageFormat: PMPageFormat;
begin
  Result := inherited DoGetOrientation;
  FOrientation := CleanPMOrientation;
  FPageFormat := FPrintInfo.PMPageFormat();
  if PMGetOrientation(FPageFormat, FOrientation) <> noErr then Exit;
  
  case FOrientation of
    kPMPortrait: Result := poPortrait;
    kPMLandscape: Result := poLandscape;
    kPMReversePortrait: Result := poReversePortrait;
    kPMReverseLandscape: Result := poReverseLandscape;
  end;
end;

procedure TCocoaPrinter.DoSetOrientation(aValue: TPrinterOrientation);
var
  FOrientation: PMOrientation;
  FPageFormat: PMPageFormat;
begin
  inherited DoSetOrientation(aValue);

  case AValue of
    poPortrait: FOrientation := kPMPortrait;
    poLandscape: FOrientation := kPMLandscape;
    poReversePortrait: FOrientation := kPMReversePortrait;
    poReverseLandscape: FOrientation := kPMReverseLandscape;
  end;
  
  FPageFormat := FPrintInfo.PMPageFormat();
  PMSetOrientation(FPageFormat, FOrientation, kPMUnlocked);
  FPrintInfo.updateFromPMPageFormat();
  ValidatePageFormat;
end;

function TCocoaPrinter.GetPrinterType: TPrinterType;
var
  IsRemote: Boolean;
begin
  Result := ptLocal;
  IsRemote := false;
  if PMPrinterIsRemote(GetCurrentCarbonPrinter(), IsRemote) <> noErr then Exit;
  if IsRemote then Result := ptNetwork;
end;


function TCocoaPrinter.DoGetPrinterState: TPrinterState;
var
  State: PMPrinterState;
begin
  Result := psNoDefine;

  State:=0;
  if PMPrinterGetState(GetCurrentCarbonPrinter(), State) <> noErr then Exit;
  
  case State of
    kPMPrinterIdle: Result := psReady;
    kPMPrinterProcessing: Result := psPrinting;
    kPMPrinterStopped: Result := psStopped;
  end;
end;

function TCocoaPrinter.DoGetDefaultCanvasClass: TPrinterCanvasRef;
begin
  Result := TCocoaPrinterCanvas;
end;

function TCocoaPrinter.GetCanPrint: Boolean;
begin
  Result := (DoGetPrinterState() <> psStopped);
end;

function TCocoaPrinter.GetCanRenderCopies: Boolean;
begin
  Result := True;
end;

initialization

  Printer := TCocoaPrinter.Create;
  
finalization

  FreeAndNil(Printer);
