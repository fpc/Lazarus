{%MainUnit ../osprinters.pas}
{**************************************************************
Implementation for carbonprinter
***************************************************************}
uses InterfaceBase, LCLIntf, LCLProc, CocoaPrnDelegate;

const
  CleanPMRect: PMRect = (top: 0; left: 0; bottom: 0; right: 0);
  //CleanPMOrientation: PMOrientation = 0;

{ TCocoaPrinterCanvas }

procedure ReleaseMemoryStream(info: UnivPtr; data: UnivPtr; size: size_t); mwpascal;
begin
  TMemoryStream(info).free;
end;

function TCocoaPrinterCanvas.GetCGContext: CGContextRef;
begin
  result := TCocoaContext(Handle).CGContext;
end;

procedure TCocoaPrinterCanvas.DoEllipse(const Bounds: TRect);
var
  R: CGRect;
begin
  R := CGRectMake(bounds.left, bounds.top, bounds.Width, bounds.Height);
  CGContextFillEllipseInRect(CGContext, R);
end;

procedure TCocoaPrinterCanvas.DoEllipseFill(const Bounds: TRect);
var
  R: CGRect;
begin
  R := CGRectMake(bounds.left, bounds.top, bounds.Width, bounds.Height);
  CGContextStrokeEllipseInRect(CGContext, R);
end;

procedure TCocoaPrinterCanvas.DoEllipseAndFill(const Bounds: TRect);
var
  R: CGRect;
begin
  R := CGRectMake(bounds.left, bounds.top, bounds.Width, bounds.Height);
  CGContextFillEllipseInRect(CGContext, R);
  CGContextStrokeEllipseInRect(CGContext, R);
end;

{ TPrinterEnumerator }

constructor TPrinterEnumerator.Create(objType: TObjType; aPrinterRef: PMPrinter);
begin
  inherited create;
  fArray := nil;
  fObjType := objType;
  fIndex := -1;
  fPrinterRef := aPrinterRef;
end;

destructor TPrinterEnumerator.Destroy;
begin
  if (fObjType=otPrinters) and (fArray<>nil) then
    CFRelease(fArray);
  inherited destroy;
end;

function TPrinterEnumerator.MoveNext: boolean;
var
  res: OSStatus;
begin

  if fIndex<0 then
  begin

    case fObjType of
      otPrinters:
        res := PMServerCreatePrinterList(kPMServerLocal, fArray);

      otPapers:
        res := PMPrinterGetPaperList(fPrinterRef, fArray);
    end;

    result := (res=noErr) and (fArray<>nil);
    if not result then
      exit;
  end;

  inc(fIndex);
  result := fIndex<CFArrayGetCount(fArray);
  if result then
    fCurrent := CFArrayGetValueAtIndex(fArray, fIndex);
end;

function TPrinterEnumerator.GetEnumerator: TPrinterEnumerator;
begin
  result := self;
end;

{ TCocoaPrinterView }

procedure TCocoaPrinterView.drawRect(dirtyRect: NSRect);
var
  Context: NSGraphicsContext;
begin
  Context := NSGraphicsContext.currentContext;

  CGContextDrawPDFDocument(context.lclCGContext, CGRect(dirtyRect), doc, pageNum);
end;

// Return the number of pages available for printing
function TCocoaPrinterView.knowsPageRange(range: NSRangePointer): LCLObjCBoolean;
begin
  range^.location := 1;
  range^.length := CGPDFDocumentGetNumberOfPages(doc);
  Result := True;
end;

function TCocoaPrinterView.rectForPage(page: NSInteger): NSRect;
var
  aPage: CGPDFPageRef;
begin
  pageNum := page;
  aPage := CGPDFDocumentGetPage(Doc, pageNum);
  result := NSRect(CGPDFPageGetBoxRect(aPage, kCGPDFMediaBox));
  self.setBounds(result);
end;

function TCocoaPrinterView.isFlipped: LCLObjCBoolean;
begin
  Result := true;
end;


{ TCocoaPrinter }


procedure TCocoaPrinter.ResetPapersList(aPrinter: PMPrinter);
var
  aPaper: PMPaper;
  aName: CFStringRef;
  res: OSStatus;
  pageFormat: PMPageFormat;
  paperR, pageR: PMRect;
  i: Integer;
  aInfo: NSPrintInfo;
  validationResult: Boolean;
  pmOr: PMOrientation;
begin
  aInfo := NSPrintInfo.sharedPrintInfo;
  if aInfo.orientation=NSPortraitOrientation then
    pmOr := kPMPortrait
  else
    pmOr := kPMLandscape;

  SetLength(fPapersList, 0);
  for aPaper in self.GetPMPrinterEnumerator(otPapers, aPrinter) do
  begin

    aName := nil;
    pageFormat := nil;
    res := PMPaperCreateLocalizedName(aPaper, aPrinter, aName);
    if res<>noErr then continue;

    if res<>noErr then continue;
    res := PMCreatePageFormatWithPMPaper(pageFormat, aPaper);
    if res<>noErr then continue;
    res := PMSetOrientation(pageFormat, pmOr, false);
    if res<>noErr then continue;

    res := PMSessionValidatePageFormat(aInfo.PMPrintSession, pageFormat, @validationResult);
    if res<>noErr then continue;

    paperR:=CleanPMRect;
    res := PMGetAdjustedPaperRect(pageFormat, paperR);
    if res<>noErr then begin PMRelease(pageFormat); continue; end;

    pageR:=CleanPMRect;
    res := PMGetAdjustedPageRect(pageFormat, pageR);
    PMRelease(pageFormat);
    if res<>noErr then continue;

    i:= Length(fPapersList);
    SetLength(fPapersList, i+1);

    with fPapersList[i] do
    begin
      PaperName := CFStringToString(aName);
      CFRelease(aName);

      PaperRect.PhysicalRect.Left := 0;
      PaperRect.PhysicalRect.Top := 0;
      PaperRect.PhysicalRect.Right := Round(paperR.right - paperR.left);
      PaperRect.PhysicalRect.Bottom := Round(paperR.bottom - paperR.top);

      PaperRect.WorkRect.Left := Round(-paperR.left);
      PaperRect.WorkRect.Top := Round(-paperR.top);
      PaperRect.WorkRect.Right := Round(pageR.right - pageR.left - paperR.left);
      PaperRect.WorkRect.Bottom := Round(pageR.bottom - pageR.top - paperR.top);
    end;
  end;

end;

function TCocoaPrinter.GetDefaultPaperFromPPDFile: string;
var
  pInfo: NSPrintInfo;
  aStatus: NSPrinterTableStatus;
  aValue: NSString;
begin
  result := '';

  pInfo := NSPrintInfo.sharedPrintInfo;
  aStatus := pInfo.printer.statusForTable(NSSTR('PPD'));
  if aStatus=NSPrinterTableOK then
  begin
    // it is a shame that the NSPrinter functions available for querying
    // PPD key and values are deprecated (except by StatusForTable)
    // if using deprecated functions hurt too much we can always parse the
    // ppd file directly as done in the the carbon printer.
    aValue := pInfo.printer.stringForKey_inTable(NSSTR('DefaultPageSize'), NSSTR('PPD'));
    if aValue<>nil then
      result := aValue.UTF8String;
  end;

end;

function TCocoaPrinter.GetCurrentPrinterName: string;
var
  aInfo: NSPrintInfo;
begin
  aInfo := NSPrintInfo.sharedPrintInfo;
  result := aInfo.printer.name.UTF8String;
end;

function TCocoaPrinter.GetCurrentPrinter: PMPrinter;
var
  pInfo: NSPrintInfo;
begin
  Result := nil;
  pInfo := NSPrintInfo.sharedPrintInfo;
  if PMSessionGetCurrentPrinter(pInfo.PMPrintSession, Result) <> noErr then Exit;
end;

function TCocoaPrinter.GetPMPrinterEnumerator(objType: TObjType;
  printerRef: PMPrinter): TPrinterEnumerator;
begin
  result := TPrinterEnumerator.create(objType, printerRef);
end;

procedure TCocoaPrinter.FindDefaultPrinter;
var
  aPrinter: PMPrinter;
begin
  FDefaultPrinter := '';

  for aPrinter in GetPMPrinterEnumerator(otPrinters) do
  begin
    if PMPrinterIsDefault(aPrinter) then
    begin
      FDefaultPrinter := CFStringToStr(PMPrinterGetName(aPrinter));
      break;
    end;
  end;
end;

constructor TCocoaPrinter.Create;
begin
  inherited Create;

  CanvasClass := TCocoaPrinterCanvas;

  FindDefaultPrinter;
  UpdatePrinter;
  Validate;
end;

procedure TCocoaPrinter.DoDestroy;
begin
  inherited DoDestroy;
end;

function TCocoaPrinter.Write(const Buffer; Count: Integer; out Written: Integer): Boolean;
begin
  Result := False;
  CheckRawMode(True);
  Written := FStream.Write(buffer, count);
  result := (Written=Count);
end;

procedure TCocoaPrinter.CheckPrinterList;
var
  orgList,curList: TStringList;
begin
  orgList := TStringList.create;
  curList := TStringList.create;
  try
    orgList.Assign(Printers);
    DoEnumPrinters(curList);
    orgList.Sort;
    curList.Sort;
    if not curList.Equals(orgList) then
      Refresh;
  finally
    curList.Free;
    orgList.Free;
  end;
end;

procedure TCocoaPrinter.RawModeChanging;
begin
  //
end;

procedure TCocoaPrinter.Validate;
var
  P: String;
  aPrinter: PMPrinter;
  aPaper: PMPaper;
  aPaperName: CFStringRef;
  papersList: TStrings;
  found: Boolean;
begin
  aPrinter := GetCurrentPrinter;
  ResetPapersList(aPrinter);
  PaperSize.SupportedPapers.Clear; // refill on next query, ie. next line

  papersList := PaperSize.SupportedPapers;

  // if target paper is not supported, use the default
  P := DoGetPaperName();
  if papersList.IndexOf(P) = -1 then
  begin
    P := DoGetDefaultPaperName();
    if papersList.IndexOf(P) < 0 then
    begin
      // neither the current paper or the default one exists in the supported
      // list of papers, instead of randomly choose one, let's try a bit harder
      // as some printer ppd files have a hint about the default paper.
      found := false;
      P := GetDefaultPaperFromPPDFile;
      if p<>'' then
      begin
        for aPaper in GetPMPrinterEnumerator(otPapers, aPrinter) do
        begin
          aPaperName := nil;
          PMPaperGetPPDPaperName(aPaper, aPaperName);
          if (aPapername<>nil) and (CFStringToString(aPaperName)=P) then
          begin
            aPaperName := nil;
            PMPaperCreateLocalizedName(aPaper, aPrinter, aPapername);
            if aPaperName<>nil then
            begin
              P := CFStringToString(aPaperName);
              CFRelease(aPapername);
              found := (papersList.IndexOf(P)>=0);
              break;
            end;
          end;
        end;
      end;

      if not found then
        p := papersList[0];

    end;
    DoSetPaperName(P);
  end;
end;

procedure TCocoaPrinter.UpdatePrinter;
var
  s: string;
  pInfo: NSPrintInfo;
begin
  pInfo := NSPrintInfo.sharedPrintInfo;
  s := pInfo.printer.name.UTF8String;
  if trim(s) = '' then // Observed if Default printer set to "Use last printer", and no printing done
    s := '*';     // so select lcl default
  SetPrinter(s);
end;

function TCocoaPrinter.GetOutputResolution: PMResolution;
var
  pInfo: NSPrintInfo;
  prn : PMPrinter;
  cnt : UInt32;
  i   : UInt32;
  rs  : PMResolution;
begin
  Result.vRes := 0;
  Result.hRes := 0;
  cnt := 0;
  pInfo := NSPrintInfo.sharedPrintInfo;
  prn := GetCurrentPrinter();
  // todo: the code has been changed to mirror Carbon implementation
  //       for some drivers PMPrinterGetOutputResolution() returns failure at all times
  //       (even if printer dialog has been called).
  //       Enumeration of resolution doesn't return all possible values either.
  //       (i.e. Brother MFC 7860DW only gives 300x300 resolution
  //        while it actually supports 300, 600 and HQ1200 (2400x600) dpis)
  //       CUPS properties do indicate the actual resoltuion, BUT there's no standard
  //       way of reading the resolution.
  //       (i.e. the same Brother printer reports via "lpoptions -l":
  //         BRResolution/Resolution: 300dpi *600dpi 2400x600dpi)
  if (PMPrinterGetOutputResolution(prn, pInfo.PMPrintSettings, rs{%H-}) = noErr) then
    Result := rs
  else if PMPrinterGetPrinterResolutionCount(prn, cnt) = noErr then
  begin
    for i:=1 to cnt do
    begin
      if (PMPrinterGetIndexedPrinterResolution(prn, i, rs) = noErr)
        and ((rs.vRes > Result.vRes) or (rs.hRes > Result.hRes)) then
        Result := rs;
    end;
  end;

  if (Result.vRes = 0) or (Result.hRes = 0) then
  begin
    Result.vRes := 72;
    Result.hRes := 72;
  end;
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

{$define ToPrinter}

procedure TCocoaPrinter.DoBeginDoc;
var
  //pInfo: NSPrintInfo;
  consumer: CGDataConsumerRef;
  gr: NSGraphicsContext;
  //path: CStringPtr;
  //url: CFUrlRef;
  R: TRect;
begin

  fDocStarted := true;
  if RawMode then
  begin

    fStream := TMemoryStream.create;

  end else
  begin

    // TODO: margins ???
    //
    // Initially NSPrintInfo do not carry the printer real margins
    // (from the imageable area) set up in the printer ppd file but some
    // standard defined (undocumented but I guess they corresponds
    // to the "Any Printer" printer). It is even documented somewhere
    // that in order to use the real printer margins one have to
    // run the PageLayout dialog and choose the real printer instead
    // of the "Any Printer" and this way the margins will match the
    // real printer ones. Check later this stuff...
    //
    // * We don't use margins in the calc of the mediabox because
    //   Apple doesn't implement a custom margins dialog and ...
    // * In order to implement custom margins we have to probably
    //   add some accessory panel, but as we currently don't do that
    //   it makes no sense to use them.
    //
    //pInfo := NSPrintInfo.sharedPrintInfo;
    //pInfo.setLeftMargin(0);
    //pInfo.setTopMargin(0);
    //pInfo.setRightMargin(0);
    //pInfo.setBottomMargin(0);

    R := PaperSize.PaperRect.WorkRect;
    pdfMediaBox := CGRectMake(0, 0, r.Width, r.Height);

    {$ifdef ToPrinter}
    if pdfData<>nil then
      CFRelease(pdfData);
    pdfData := CFDataCreateMutable(nil, 0);
    consumer := CGDataConsumerCreateWithCFData(pdfData);
    {$else}
    path := 'salida.pdf';
    url := CFURLCreateFromFileSystemRepresentation(nil, path, length(path), false);
    consumer := CGDataConsumerCreateWithURL(url);
    CFRelease(url);
    {$endif}
    pdfContext := CGPDFContextCreate(consumer, pdfMediabox, nil);
    CGDataConsumerRelease(consumer);

    gr := NSGraphicsContext.graphicsContextWithGraphicsPort_flipped(pdfContext, false);
    FPrinterContext := TCocoaContext.Create(gr);

  end;

end;

procedure TCocoaPrinter.DoBeginPage;
var
  mediaBox: CGRect;
  R: TRect;
  rgn: TCocoaRegion;
begin

  if RawMode then
    exit;

  R := PaperSize.PaperRect.WorkRect;
  mediaBox := CGRectMake(0, 0, r.Width, r.Height);

  //CGPDFContextBeginPage(pdfContext, nil);
  CGContextBeginPage(pdfContext, mediabox);
  CGContextSaveGState(pdfContext);

  if Assigned(Canvas) then
    Canvas.Handle := HDC(FPrinterContext);

  rgn := TCocoaRegion.Create(0, 0, r.Width, r.height);
  FPrinterContext.SetClipRegion(Rgn, cc_Copy);
  rgn.Free;
end;

procedure TCocoaPrinter.DoEndPage;

  procedure ResetCanvasHandle;
  begin
    if Assigned(Canvas) then
      Canvas.Handle := HDC(0);
  end;

begin

  if RawMode then
    exit;

  if not fDocStarted then
  begin
    ResetcanvasHandle;
    exit;
  end;

  if Aborted then
  begin
    ResetcanvasHandle;
    exit;
  end;

  CGContextRestoreGState(pdfContext);
  CGContextEndPage(pdfContext);

  ResetCanvasHandle;

end;

// starts a raw mode print job
procedure TCocoaPrinter.StartRawModePrintJob(Sender: TObject);
var
  pInfo: NSPrintInfo;
  printDestination: PMDestinationType;
  aPrinter: PMPrinter;
  mimeType: CFStringRef;
  mimeTypes: CFArrayRef;
  arrayCount: CFIndex;
  provider: CGDataProviderRef;
  status: OSStatus;

  procedure Error(s:string);
  begin
    //DebugLn(s);
    try
      fStream.free;
    except
      // DebugLn
    end;
    fStream := nil;
    raise Exception.Create(s);
  end;

begin

  pInfo := NSPrintInfo.sharedPrintInfo;
  printDestination := kPMDestinationInvalid;

  if PMSessionGetDestinationType(pInfo.PMPrintSession, pInfo.PMPrintSettings, printDestination)=noErr then
  begin
    if printDestination=kPMDestinationPrinter then
    begin
      aPrinter := GetCurrentPrinter;
      if aPrinter<>nil then
      begin
        status := PMPrinterGetMimeTypes(aPrinter, pInfo.PMPrintSettings, mimeTypes);
        if (status=noErr) and (mimeTypes<>nil) then
        begin
          mimeType := CFSTR('application/vnd.cups-raw');
          arrayCount := CFArrayGetCount(mimeTypes);
          if CFArrayContainsValue(mimeTypes, CFRangeMake(0, arrayCount), mimeType) then
          begin
            provider := CGDataProviderCreateWithData(fStream, fStream.Memory, fStream.Size, @ReleaseMemoryStream);
            status := PMPrinterPrintWithProvider(aPrinter, pInfo.PMPrintSettings, pInfo.PMPageFormat, mimeType, provider);
            CGDataProviderRelease(provider);
            if status<>noErr then
              Error('Error '+IntToStr(status)+'while raw printing')
            else
              fStream := nil;
          end else
            Error('Printer do not support raw printing');
        end else
          Error('Error while getting supported mime types');
      end else
        Error('Error, couldn''t get the current printer');
    end else
      Error('Error, destination is not printer');
  end else
    Error('Error on getting DestinationType');
end;

procedure TCocoaPrinter.DoEndDoc(aAborded: Boolean);
var
  preview: TCocoaPrinterView;
  op: NSPrintOperation;
  provider: CGDataProviderRef;
  pInfo: NSPrintInfo;
begin

  if not fDocStarted then
    exit;

  if aAborded then
  begin
    // TODO: check leaks ...
  end;

  try

    pInfo := NSPrintInfo.SharedPrintInfo;
    if not RawMode then
    begin
      FPrinterContext.Free;
      FPrinterContext := nil;
      CGPDFContextClose(pdfContext);
      //path := 'salida2.pdf';
      //url := CFURLCreateFromFileSystemRepresentation(nil, path, length(path), false);
      //if not CFURLWriteDataAndPropertiesToResource(url, pdfData, nil, err) then
      //  WriteLn('Error al guardar pdfdata: ', err)
      //else
      //  WriteLn('pdf data guardado exitosamente');
      preview  := TCocoaPrinterView.alloc.initWithFrame(NSRect(pdfMediabox));
      provider := CGDataProviderCreateWithCFData(pdfData);
      preview.Doc := CGPDFDocumentCreateWithProvider(provider);
      CGDataProviderRelease(provider);
    end;
    // here for
    //     rawmode: we have a fStream with data ready to print
    // not rawmode: we have a view that can render pdf content
    //
    // check if we want to print directly or through a print dialog.
    if printDelegate<>nil then
    begin

      printDelegate.sender := self;
      printDelegate.OnStartJob := nil;
      printDelegate.renderView := nil;
      if RawMode then
        printDelegate.OnStartJob := @StartRawModePrintJob
      else
        printDelegate.renderView := preview;

      try
        printDelegate.RunPrintJob;
      finally
        printDelegate := nil;
      end;

    end else
    begin

      if RawMode then
        StartRawModePrintJob(self)
      else
      begin
        // run print operation without print dialog
        op := NSPrintOperation.printOperationWithView_printInfo(preview, pInfo);
        op.setShowsPrintPanel(false);
        op.runOperation;
      end;

    end;

  finally
    fDocStarted := false;
  end;

end;

procedure TCocoaPrinter.DoAbort;
begin
  inherited DoAbort;

  //OSError(PMSessionSetError(PrintSession, kPMCancel), Self, 'DoAbort', 'PMSessionSetError');
end;

//Enum all defined printers. First printer it's default
procedure TCocoaPrinter.DoEnumPrinters(Lst: TStrings);
var
  aName: String;
  aPrinter: PMPrinter;
begin
  Lst.Clear;
  for aprinter in GetPMPrinterEnumerator(otPrinters) do
  begin
    aName := CFStringToStr(PMPrinterGetName(aPrinter));
    if aName=FDefaultPrinter then begin
      Lst.Insert(0, aName);
    end
    else
      Lst.Add(aName);
  end;
end;

procedure TCocoaPrinter.DoResetPrintersList;
begin
  inherited DoResetPrintersList;
end;

// We need to use Core Printing here, see:
// http://lists.apple.com/archives/cocoa-dev/2005/Nov/msg01227.html
// See Also "Using Cocoa and Core Printing Together"
// https://developer.apple.com/library/mac/technotes/tn2248/_index.html
procedure TCocoaPrinter.DoEnumPapers(Lst: TStrings);
var
  aPaper: PMPaper;
  CFString: CFStringRef;
  aPrinter: PMPrinter;
  locName: string;
begin
  aPrinter := GetCurrentPrinter;
  for aPaper in GetPMPrinterEnumerator(otPapers, aPrinter) do
  begin
    CFString := nil;
    PMPaperCreateLocalizedName(aPaper, aPrinter, CFString);
    if CFString<>nil then
    begin
      locName := CFStringToStr(CFString);
      CFRelease(CFString);
      CFString := nil;
      Lst.Add(locName);
    end;
  end;
end;

function TCocoaPrinter.DoGetPaperName(): string;
var
  pInfo: NSPrintInfo;
begin
  pInfo := NSPrintInfo.sharedPrintInfo;
  result := NSStringToString(pInfo.localizedPaperName);
end;

function TCocoaPrinter.DoGetDefaultPaperName: string;
var
  pageFormat: PMPageFormat = nil;
  pInfo: NSPrintInfo;
  aPaper: PMPaper = nil;
  aPapername: CFStringRef = nil;
begin
  Result := '';

  pInfo := NSPrintInfo.sharedPrintInfo;

  if PMCreatePageFormat(pageFormat) <> noErr then exit;
  try
    if PMSessionDefaultPageFormat(pInfo.PMPrintSession, pageFormat) <> noErr then exit;
    if PMGetPageFormatPaper(pageFormat, aPaper) <> noErr then exit;
    if PMPaperCreateLocalizedName(aPaper, GetCurrentPrinter, aPapername) <> noErr then exit;
    result := CFStringToString(aPapername);
  finally
    PMRelease(pageFormat);
  end;

end;

procedure TCocoaPrinter.DoSetPaperName(aName: string);
var
  anOrientation: TPrinterOrientation;
  aPrinter: PMPrinter;
  aPaper: PMPaper;
  orgFormat: PMPageFormat;
  pageFormat: PMPageFormat = nil;
  aPapername: CFStringRef;
  pInfo: NSPrintInfo;
  curName: String;
begin
  pInfo := NSPrintInfo.sharedPrintInfo;
  anOrientation := DoGetOrientation();
  aPrinter := GetCurrentPrinter;
  for aPaper in Self.GetPMPrinterEnumerator(otPapers, aPrinter) do
  begin
    aPapername := nil;
    PMPaperCreateLocalizedName(aPaper, aPrinter, aPaperName);
    if aPapername<>nil then
    begin
      curName := CFStringToString(aPapername);
      CFRelease(aPapername);
      if curName=aName then
      begin
        PMCreatePageFormatWithPMPaper(pageFormat, aPaper);
        orgFormat := pInfo.PMPageFormat;
        PMCopyPageFormat(pageFormat, orgFormat);
        pInfo.updateFromPMPageFormat;
        PMRelease(pageFormat);
        DoSetOrientation(anOrientation);
        break;
      end;
    end;
  end;
end;

function TCocoaPrinter.DoGetPaperRect(aName: string; var aPaperRc: TPaperRect
  ): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i:=0 to Length(fPapersList)-1 do
  begin
    if aName=fPapersList[i].PaperName then
    begin
      aPaperRc := fPapersList[i].PaperRect;
      result := i;
      break;
    end;
  end;
end;

function TCocoaPrinter.DoSetPrinter(aName: string): Integer;
var
  aPrinter: PMPrinter;
  curName: String;
  pInfo: NSPrintInfo;
begin

  result := -1;
  for aPrinter in Self.GetPMPrinterEnumerator(otPrinters) do
  begin
    curName := CFStringToStr(PMPrinterGetName(aPrinter));
    if curName=aName then
    begin
      result := Printers.IndexOf(aName);
      //
      pInfo := NSPrintInfo.sharedPrintInfo;
      PMSessionSetCurrentPMPrinter( pInfo.PMPrintSession, aPrinter);

      ResetPapersList(aPrinter);
      break;
    end;
  end;

end;

function TCocoaPrinter.DoGetCopies: Integer;
var
  NumCopies: UInt32;
  pInfo: NSPrintInfo;
begin
  Result := 1;
  NumCopies := 0;
  pInfo := NSPrintInfo.sharedPrintInfo;
  if PMGetCopies(pInfo.PMPrintSettings, NumCopies) <> noErr then Exit;
  Result := NumCopies;
end;

procedure TCocoaPrinter.DoSetCopies(aValue: Integer);
var
  pInfo: NSPrintInfo;
begin
  pInfo := NSPrintInfo.sharedPrintInfo;
  if PMSetCopies(pInfo.PMPrintSettings, AValue, False) <> noErr then
    Exit;
  pInfo.updateFromPMPrintSettings();
end;

function TCocoaPrinter.DoGetOrientation: TPrinterOrientation;
var
  info: NSPrintInfo;
begin
  info := NSPrintInfo.sharedPrintInfo;
  if info.orientation=NSPortraitOrientation then
    result := poPortrait
  else
    result := poLandscape;
end;

procedure TCocoaPrinter.DoSetOrientation(aValue: TPrinterOrientation);
var
  info: NSPrintInfo;
  oldValue: NSPrintingOrientation;
begin

  info := NSPrintInfo.sharedPrintInfo;
  oldValue := info.orientation;
  case aValue of
    poPortrait, poReversePortrait:
      info.setOrientation(NSPortraitOrientation);
    else
      info.setOrientation(NSLandscapeOrientation);
  end;
  if oldValue<>info.orientation then
    ResetPapersList(GetCurrentPrinter);
end;

function TCocoaPrinter.GetPrinterType: TPrinterType;
var
  IsRemote: Boolean;
begin
  Result := ptLocal;
  IsRemote := false;
  if PMPrinterIsRemote(GetCurrentPrinter(), IsRemote) <> noErr then Exit;
  if IsRemote then Result := ptNetwork;
end;


function TCocoaPrinter.DoGetPrinterState: TPrinterState;
var
  State: PMPrinterState;
begin
  Result := psNoDefine;

  State:=0;
  if PMPrinterGetState(GetCurrentPrinter(), State) <> noErr then Exit;
  
  case State of
    kPMPrinterIdle: Result := psReady;
    kPMPrinterProcessing: Result := psPrinting;
    kPMPrinterStopped: Result := psStopped;
  end;
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
