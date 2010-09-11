{%MainUnit ../osprinters.pas}
{**************************************************************
Implementation for winprinter
***************************************************************}
Uses InterfaceBase, LCLIntf, WinVer, WinUtilPrn;

// todo: this ^ is a mess: mixed WinUtilPrn/Windows units clean...

// todo: this should be a method, can not be atm because mixed units ^
function GetCurrentDevMode(out DM:PDeviceMode): boolean;
var
  PDev: TPrinterDevice;
begin
  result := false;
  if (Printer.Printers.Count>0) then
  begin
    PDev:=TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
    DM := PDev.DevMode;
    result := DM<>nil;
  end;
end;

{ TWinPrinter }

constructor TWinPrinter.Create;
begin
  inherited Create;

  fLastHandleType:=htNone;
  fPrinterHandle :=0; //None
end;

destructor TWinPrinter.Destroy;
begin
  ClearDC;

  DoResetPrintersList;

  if fPrinterHandle<>0 then
      ClosePrinter(fPrinterHandle);

  inherited Destroy;
end;

function TWinPrinter.Write(const Buffer; Count: Integer;
  var Written: Integer): Boolean;
begin
  CheckRawMode(True);
  result := WritePrinter(FPrinterHandle, @Buffer, Count, pdword(@Written));
end;

function TWinPrinter.GetHandlePrinter : HDC;
begin
  SetIC;
  Result:=fDC;
end;


procedure TWinPrinter.SetHandlePrinter(aValue : HDC);
begin
  CheckRawMode(False);
  if aValue <> fDC then
  begin
   ClearDC;
   fDC := aValue;
   if Assigned(Canvas) then Canvas.Handle:=fDC;
   fLastHandleType:=htDC;
  end;
end;

procedure TWinPrinter.RawModeChanging;
begin
  // if old mode was standard free DC if it was created
  if not RawMode and (fDC<>0) then
    FreeDC;
end;

procedure TWinPrinter.PrinterSelected;
begin
  if (PrinterIndex>=0) and not RawMode then
    SetDC;
end;

function TWinPrinter.GetXDPI: Integer;
begin
  Result:=72;
  if (Printers.Count>0) and not RawMode then
  begin
    SetDC;
    Result:=windows.GetDeviceCaps(fDC, LOGPIXELSX);
  end;
end;

function TWinPrinter.GetYDPI: Integer;
begin
  Result:=72;
  if (Printers.Count>0) and not RawMode then
  begin
    SetDC;
    Result:=windows.GetDeviceCaps(fDC,LOGPIXELSY);
  end;
end;

procedure TWinPrinter.SetIC;
var PDev : TPrinterDevice;
begin
  if (fLastHandleType=htNone) and (Printers.Count>0) then
  begin
    PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);
    
    fDC:=CreateIC(PChar(PDev.Driver),PChar(PDev.Device),
           PChar(PDev.Port),PDev.DevMode);

    if fDC=0 then
      fDC:=CreateIC(PChar('WINSPOOL'),PChar(PDev.Device),
           PChar(PDev.Port),PDev.DevMode);

    if fDC=0 then
      raise EPrinter.Create(
        Format('Invalid printer (DC=%d Driver=%s Device=%s Port=%s)',
          [fDC,Pdev.Driver,PDev.Device,PDev.Port]));
          
    if Assigned(Canvas) then
      Canvas.Handle:=fDC;
      
    fLastHandleType:=htIC;
  end;
end;

procedure TWinPrinter.SetDC;
var PDev : TPrinterDevice;
begin

  if (fLastHandleType<>htDC) and (Printers.Count>0) then
  begin
    ClearDC;
    PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);
    try

    //Device is only 32 chars long,
    //if the Printername or share is longer than 32 chars, this will return 0
    fDC := CreateDC(nil, PChar(PDev.Name), nil, PDev.DevMode);
    if fDC=0 then
      fDC := CreateDC(PChar('WINSPOOL'),PChar(PDev.Name), nil, PDev.DevMode);

    {Workaround (hack) for Lexmark 1020 JetPrinter (Mono)}
    if fDC=0 then
      fDC:=CreateDC(nil,PChar(PDev.Driver),nil, PDev.DevMode);
    if fDC=0 then
      fDC:=CreateDC(pChar('WINSPOOL'),PChar(PDev.Driver),nil,PDev.DevMode);

    except on E:Exception do
      raise EPrinter.Create(Format('CreateDC Exception:"%s" (Error:"%s", '+
          'DC=%d Driver="%s" Device="%s" Port="%s")', [E.Message,
          SysErrorMessage(GetLastError),fDC, Pdev.Driver,
          Printers[PrinterIndex],PDev.Port]));
    end;
    
    if fDC=0 then
      raise EPrinter.Create(Format('Invalid printer (Error:%s, '+
          'DC=%d Driver="%s" Device="%s" Port="%s")',
          [SysErrorMessage(GetLastError),fDC,Pdev.Driver,Printers[PrinterIndex],
           PDev.Port]));
      
    if Assigned(Canvas) then
      Canvas.Handle:=fDC;
      
    fLastHandleType:=htDC;

  end;
end;

procedure TWinPrinter.ClearDC;
begin
  if not RawMode then
    FreeDC
end;

procedure TWinPrinter.FreeDC;
begin
  if Assigned(Canvas) then
    Canvas.Handle:=0;

  if fDC<>0 then
  begin
    DeleteDC(fDC);
    fDc := 0;
  end;

  fLastHandleType:=htNone;
end;


// Based on MS Article Q167345
function TWinPrinter.UpdateDevMode(APrinterIndex:Integer): boolean;
var
  PDev: TPrinterDevice;
  dwRet: Integer;
begin
  if FPrinterHandle=0 then begin
    result := false;
    exit;
  end;

  // now we have a right FPrinterHandle, get current printer settings
  PDev := TPrinterDevice(Printers.Objects[APrinterIndex]);

  // 1.	Determine the required size of the buffer from the device,
  //    and then allocate enough memory for it.
  PDev.DevModeSize := DocumentProperties(0, FPrinterHandle, pchar(PDev.Name),
                      nil, nil, 0);
  ReallocMem(Pdev.DevMode, PDev.DevModeSize);
  if PDev.DevModeSize=0 then begin
    result := false;
    exit;
  end;
  
  // 2.	Ask the device driver to initialize the DEVMODE buffer with
  //    the default settings.
  dwRet := DocumentProperties(0, FPrinterHandle, pchar(Pdev.Name),
           PDev.DevMode, nil, DM_OUT_BUFFER);
  result := (dwRet=IDOK);
  if not result then begin
    ReallocMem(PDev.Devmode, 0);
    exit;
  end;
  
end;

procedure TWinPrinter.DoBeginDoc;
var
  Inf: TDocInfo;
  Doc1: DOC_INFO_1;
begin
  inherited DoBeginDoc;

  if fPrinterHandle=0 then
    raise EPrinter.Create('Printer handle not defined');

  if RawMode then begin

    Doc1.DocName    := pchar(Title);
    if Filename<>'' then
      Doc1.OutputFile := PChar(Filename)
    else
      Doc1.OutputFile := nil;
    Doc1.DataType   := 'RAW';
    
    if StartDocPrinter(FPrinterHandle, 1, PByte(@Doc1))=0 then begin
      ClosePrinter(FPrinterHandle);
      FPrinterHandle:=0;
    end else
    if StartPagePrinter(FPrinterHandle)=0 then begin
      EndDocPrinter(FPrinterHandle);
      ClosePrinter(FPrinterHandle);
      FPrinterHandle:=0;
    end;
      
  end else begin

    SetDC;
    Canvas.Handle:=fDC;
    Canvas.Refresh;

    FillChar(Inf,SizeOf(Inf),0);
    Inf.cbSize:=SizeOf(Inf);
    Inf.lpszDocName:=PChar(Title);
    if FileName<>'' then
      Inf.lpszOutput:=PChar(Filename);

    StartDoc(fDC,@Inf);
    StartPage(fDC);
    
  end;
end;

procedure TWinPrinter.DoNewPage;
begin
  inherited DoNewPage;
  
  if RawMode then begin

    EndPagePrinter(FPrinterHandle);
    StartPagePrinter(FPrinterHandle);
    
  end else begin
    EndPage(fDC);
    StartPage(fDC);
    Canvas.Refresh;
  end;
end;

procedure TWinPrinter.DoEndDoc(aAborded: Boolean);
begin
  inherited DoEndDoc(aAborded);

  if RawMode then begin

    EndPagePrinter(FPrinterHandle);
    EndDocPrinter(FPrinterHandle);
    {
    ClosePrinter(FPrinterHandle);
    FPrinterHandle:=0;
    }
  end else begin
    EndPage(fDC);
    if not aAborded then
      WinUtilPrn.EndDoc(fDC);
  end;
end;

procedure TWinPrinter.DoAbort;
begin
  inherited DoAbort;
  if RawMode then
    AbortPrinter(FPrinterHandle)
  else
    AbortDoc(fDC);
end;

function TWinPrinter.GetDefaultPrinter: string;

const
  MAXBUFSIZE = 512;

var
  Needed, PrtCount: DWORD;
  BoolRes: BOOL;
  IntRes: Integer;
  PrintInfo2Buf: pchar;
  GetDefPrnFunc: function(buffer: pchar; var bufSize:DWORD):BOOL; stdcall;
  SpoolerHandle: HINST;
begin
  // retrieve default printer using ms blessed method, see
  // see: http://support.microsoft.com/default.aspx?scid=kb;en-us;246772
  result := '';
  if Win32Platform=VER_PLATFORM_WIN32_WINDOWS then
  begin

    // Get PRINT_INFO_2 record size
    SetLastError(0);
    BoolRes := EnumPrinters(PRINTER_ENUM_DEFAULT, nil, 2, nil, 0,
                              Needed, PrtCount);
    if not BoolRes and
      ((GetLastError<>ERROR_INSUFFICIENT_BUFFER) or (Needed=0)) then
      exit;

    // Get PRINT_INFO_2 record
    GetMem(PrintInfo2Buf, Needed);
    BoolRes := EnumPrinters(PRINTER_ENUM_DEFAULT, nil, 2, PrintInfo2Buf,
                              Needed, Needed, PrtCount);
    if not BoolRes then
    begin
      FreeMem(PrintInfo2Buf);
      exit;
    end;

    Result := PPRINTER_INFO_2(PrintInfo2Buf)^.pPrinterName;
    FreeMem(PrintInfo2Buf);

  end else
  if Win32Platform=VER_PLATFORM_WIN32_NT then
  begin

    if Win32MajorVersion >=5 then
    begin

      // for Windows 2000 or later, use api GetDefaultPrinter
      // TODO: needs to check WindowsUnicodeSupport
      SpoolerHandle := LoadLibrary(LibWinSpool);
      if SpoolerHandle = 0 then
        exit;
      Pointer(GetDefPrnFunc) := GetProcAddress(SpoolerHandle,'GetDefaultPrinterA');
      if GetDefPrnFunc=nil then
      begin
        FreeLibrary(SpoolerHandle);
        exit;
      end;

      PrtCount := MAXBUFSIZE;
      SetLength(Result, PrtCount); // make room for printer name
      BoolRes := GetDefPrnFunc(pchar(Result), prtCount);
      FreeLibrary(SpoolerHandle);
      PrtCount := strlen(pchar(result));
      SetLength(Result, PrtCount);

    end else
    begin

      // for NT, use GetProfileString
      SetLength(result, MAXBUFSIZE);
      IntRes := GetProfileString('windows', 'device', ',,,', pchar(result),
                                                                  MAXBUFSIZE);
      if (IntRes>0) and (pos(',',Result)<>0) then
        Result := copy(Result, 1, pos(',', Result)-1)
      else
        Result := '';

    end;
  end;

  Result := AnsiToUTF8(Result);
end;


//Enum all defined printers. First printer it's default
procedure TWinPrinter.DoEnumPrinters(Lst: TStrings);
Var Flags          : DWORD;
    Level          : DWORD;
    PrtCount       : DWORD;
    Needed         : DWORD;
    Buffer         : PChar;
    InfoPrt        : PChar;
    i              : Integer;
    DefaultPrinter : string;
    PDev           : TPrinterDevice;
    TmpDevMode     : PDeviceMode;
    PrtStr         : string;
begin
  {$IFDEF NOPRINTERS}
  Lst.Clear;
  exit;
  {$ENDIF}
  DefaultPrinter := GetDefaultPrinter;

  Flags:=PRINTER_ENUM_CONNECTIONS or PRINTER_ENUM_LOCAL;
  Level:=2;

  //Evaluate buffer size
  Needed:=0;
  EnumPrinters(Flags,nil,Level,nil,0,Needed,PrtCount);
  if Needed<>0 then
  begin
    GetMem(Buffer,Needed);
    Fillchar(Buffer^, Needed, 0);
    try
      //Enumerate Printers
      if EnumPrinters(Flags,nil,Level,Buffer,Needed,Needed,PrtCount) then
      begin
        InfoPrt:=Buffer;
        for i:=0 to PrtCount-1 do
        begin
          if Level=2 then
          begin
            PDev:=TPrinterDevice.Create;
            PDev.Name  :=PPRINTER_INFO_2(InfoPrt)^.pPrinterName;
            PDev.Driver:=PPRINTER_INFO_2(InfoPrt)^.pDriverName;
            PDev.Port  :=PPRINTER_INFO_2(InfoPrt)^.pPortName;
            TmpDevMode :=PPRINTER_INFO_2(InfoPrt)^.pDevMode;
            if TmpDevMode<>nil then begin
              // the devmode structure obtained this way have two problems
              // 1. It's not the full devmode, because it doesn't have
              //    the private info
              // 2. It's not initialized with the current settings and
              //    have not extra settings at all.
              //
              // PDev.DevMode:=PPRINTER_INFO_2(InfoPrt)^.PDevMode^;
              PDev.Device:= TmpDevMode^.dmDeviceName;
              PDev.DefaultPaper:=TmpDevMode^.dmPaperSize;
            end
            else begin
              PDev.Device:='';
              PDev.DefaultPaper:=0;
            end;
            PrtStr := AnsiToUTF8(PDev.Name);
            if AnsiCompareText(PrtStr,DefaultPrinter)<>0 then
              Lst.AddObject(PrtStr,PDev)
            else
            begin
              Lst.Insert(0,PrtStr);
              Lst.Objects[0]:=PDev;
            end;
            Inc(InfoPrt,SizeOf(_PRINTER_INFO_2));
          end;
        end;
      end;
    finally
      FreeMem(Buffer);
    end;
  end;
end;

procedure TWinPrinter.DoResetPrintersList;
var i   : Integer;
    Obj : TObject;
begin
  if Printers.Count>0 then
  begin
    for i:=0 to Printers.Count-1 do
    begin
      Obj:=Printers.Objects[i];
      Printers.Objects[i]:=nil;
      FreeAndNil(Obj);
    end;
  end;
  
  inherited DoResetPrintersList;
end;

procedure TWinPrinter.DoEnumPapers(Lst: TStrings);
var Buffer   : PChar;
    PaperN   : String;
    PaperC,i : Integer;
    Count    : Integer;
    PDev     : TPrinterDevice;
    ArPapers : Array[0..255] of Word;
begin
  inherited DoEnumPapers(Lst);

  if (Printers.Count>0) then
  begin
    PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);

    if fPrinterHandle=0 then
      SetPrinter(Printers.Strings[PrinterIndex]);

    if fPrinterHandle=0 then
      raise EPrinter.Create('Printer handle not defined');

    //Retreive the supported papers
    PaperC:=0;
    Count := DeviceCapabilities(PChar(Pdev.Name),PCHar(PDev.Port),
          DC_PAPERNAMES,nil,nil);
    if Count<=0 then
      raise EPrinter.CreateFmt('DoEnumPapers<DC_PAPERNAMES> error : %d, (%s)',
          [GetLastError,SysErrorMessage(GetLastError)]);
    GetMem(Buffer,64*Count);
    try
      PaperC:=DeviceCapabilities(PChar(Pdev.Name),PCHar(PDev.Port),
          DC_PAPERNAMES,Buffer,nil);
      for i:=0 to PaperC-1 do
      begin
        PaperN:=StrPas(Buffer+i*64);
        Lst.Add(PaperN);
      end;
    finally
      FreeMem(Buffer);
    end;

    //Retreive the code of papers
    FillChar(ArPapers,SizeOf(ArPapers),0);
    PaperC:=DeviceCapabilities(PChar(Pdev.Name),PChar(PDev.Port),
          DC_PAPERS,PChar(@ArPapers[0]),nil);
    if PaperC<=0 then
      raise EPrinter.CreateFmt('DoEnumPapers<DC_PAPERS> error : %d, (%s)',
          [GetLastError,SysErrorMessage(GetLastError)])
    else if PaperC>Lst.Count then
      PaperC := Lst.Count;
      
    for i:=0 to PaperC-1 do
      Lst.Objects[i]:=TObject(ptrint(ArPapers[i]));
  end;
end;

function TWinPrinter.DoGetPaperName: string;
var
  i    : Integer;
  dm   : PDeviceMode;
begin
  Result:=inherited DoGetPaperName;
  if GetCurrentDevMode(dm) then
    with PaperSize.SupportedPapers do begin
      i := IndexOfObject(TObject(ptrInt(dm^.dmPaperSize)));
      if i>=0 then
        result := PaperSize.SupportedPapers[i];
    end;
end;

function TWinPrinter.DoGetDefaultPaperName: string;
var i    : Integer;
    PDev : TPrinterDevice;
begin
  Result:=inherited DoGetDefaultPaperName;

  if (Printers.Count>0) then
  begin
    PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);
    with PaperSize.SupportedPapers do begin
      i:=IndexOfObject(TObject(ptrint(PDev.DefaultPaper)));
      if i<>-1 then
        Result:= Strings[i];
    end;
  end;
end;

procedure TWinPrinter.DoSetPaperName(aName: string);
var i    : Integer;
    dm   : PDeviceMode;
begin
  inherited DoSetPaperName(aName);
  if GetCurrentDevMode(dm) then begin
    i:=PaperSize.SupportedPapers.IndexOf(aName);
    if i<>-1 then begin
      ClearDC;
      dm^.dmPaperSize := SHORT(ptrint(PaperSize.SupportedPapers.Objects[i]));
    end;
  end;
end;

function TWinPrinter.DoGetPaperRect(aName: string;
  var aPaperRc: TPaperRect): Integer;
var NSize, i : Integer;
    PDev     : TPrinterDevice;
    ArSizes  : Array[0..255] of TPoint;
begin
  Result:=Inherited DoGetPaperRect(aName,aPaperRc);

  if (Printers.Count>0) and not RawMode then
  begin
    // Information for physical sizes can be obtained for any paper supported
    // by the printer, the same is not true for printable paper size, this can
    // be obtained only(?) for currently selected paper.
    //
    if DoGetPaperName=AName then begin
      SetDC;
      with aPaperRC.PhysicalRect do begin
        Left  :=0;
        Top   :=0;
        Right :=Windows.GetDeviceCaps(fDC, PHYSICALWIDTH);
        Bottom:=Windows.GetDeviceCaps(fDC, PHYSICALHEIGHT);
      end;
      with aPaperRC.WorkRect do begin
        Left  :=Windows.GetDeviceCaps(fDC, PHYSICALOFFSETX);
        Top   :=Windows.GetDeviceCaps(fDC, PHYSICALOFFSETY);
        Right :=Left   + Windows.GetDeviceCaps(fDC, HORZRES);
        Bottom:=Top    + Windows.GetDeviceCaps(fDC, VERTRES);
      end;
    end else begin
      // for other papers return at least the physical size
      // note: old implementation was using DeviceCapabilities function with
      //       index DC_PAPERSIZE, unfortunately this returns dimensions in
      //       tenths of millimeter which is wrong, we need points (not font
      //       points, but printer "pixels" at current resolution).
      //
      PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);

      //Retreive the Width and Height of aName paper
      FillChar(ArSizes,SizeOf(ArSizes),0);
      NSize:=DeviceCapabilities(PChar(Pdev.Name),PChar(PDev.Port),
            DC_PAPERSIZE,PChar(@ArSizes[0]),nil);
      i:=PaperSize.SupportedPapers.IndexOf(aName);
      if (i>=0) and (i<NSize) and (NSize<>0) then
      begin
        aPaperRc.PhysicalRect:=Classes.Rect(0,0,ArSizes[i].X,ArSizes[i].Y);
        with aPaperRC.PhysicalRect do begin
          // convert from tenths of millimeter to points
          Right  := round(Right * XDPI / 254);
          Bottom := round(Bottom* YDPI / 254);
        end;
        aPaperRc.WorkRect := aPaperRC.PhysicalRect;
      end;
    end;
    Result:=1;
  end;
end;

function TWinPrinter.DoSetPrinter(aName: string): Integer;
var i    : Integer;
    PDev : TPrinterDevice;
begin
  Result:=inherited DoSetPrinter(aName);

  i:=Printers.IndexOf(aName);
  if i<>-1 then
  begin
    ClearDC;

    if FPrinterHandle<>0 then
      ClosePrinter(FPrinterHandle);

    PDev:=TPrinterDevice(Printers.Objects[i]);
    if not OpenPrinter(PChar(PDev.Name),fPrinterHandle, nil) then
    begin
      FprinterHandle := 0;
      raise EPrinter.CreateFmt('OpenPrinter exception : %s',
                                   [SysErrorMessage(GetlastError)]);
    end;
    
    if UpdateDevMode(i) then
      Result:=i
    else
      Result:=-1;
  end;
end;

function TWinPrinter.DoGetCopies: Integer;
var
  dm: PDeviceMode;
begin
  Result:=inherited DoGetCopies;
  if GetCurrentDevMode(dm) then begin
    if dm^.dmCopies<>0 then
      result := dm^.dmCopies;
  end;
end;

procedure TWinPrinter.DoSetCopies(aValue: Integer);
var
  dm: PDeviceMode;
begin
  inherited DoSetCopies(aValue);
  if (AValue>0) and GetCurrentDevMode(dm) then begin
    ClearDC;
    dm^.dmCopies := SHORT(aValue)
  end;
end;

function TWinPrinter.DoGetOrientation: TPrinterOrientation;
var
  dm: PDeviceMode;
begin
  Result:=inherited DoGetOrientation;
  if GetCurrentDevMode(dm) then begin
    case dm^.dmOrientation of
      DMORIENT_PORTRAIT : result:=poPortrait;
      DMORIENT_LANDSCAPE: result:=poLandscape;
    end;
  end;
end;

procedure TWinPrinter.DoSetOrientation(aValue: TPrinterOrientation);
var
  dm: PDeviceMode;
begin
  inherited DoSetOrientation(aValue);
  if GetCurrentDevMode(dm) then begin
    ClearDC;
    dm^.dmOrientation := Win32Orientations[aValue];
  end;
end;

function TWinPrinter.GetPrinterType: TPrinterType;
var
 Size : Dword;
 InfoPrt : Pointer;
begin
  Result := inherited GetPrinterType;
  Result:=ptLocal;
  //On Win9X all printers are local
  if Win32Platform <> VER_PLATFORM_WIN32_NT then Exit;
  
  GetPrinter(fPrinterHandle,4,nil,0,Size);
  GetMem(InfoPrt,Size);
  try
  if not GetPrinter(fPRinterHandle,4,InfoPrt,Size,Size)
  then
    raise EPrinter.CreateFmt('GetPrinterType failed : %s',
        [SysErrorMessage(GetLastError)]);
  if PPRINTER_INFO_4(InfoPrt)^.Attributes = PRINTER_ATTRIBUTE_NETWORK then
     Result := ptNetwork;
  finally
    FreeMem(InfoPrt);
  end;

end;


function TWinPrinter.DoGetPrinterState: TPrinterState;
var
 Size,Status,Jobs : Dword;
 InfoPrt : Pointer;
begin
  Result := inherited DoGetPrinterState;
  Result:=psNoDefine;
  GetPrinter(fPrinterHandle,2,nil,0,Size);
  GetMem(InfoPrt,Size);
  try
  if not GetPrinter(fPrinterHandle,2,InfoPrt,Size,Size)
  then
    raise EPrinter.CreateFmt('GetPrinterState failed : %s',
        [SysErrorMessage(GetLastError)]);

  Jobs := PPRINTER_INFO_2(InfoPrt)^.cJobs;
  Status := PPRINTER_INFO_2(InfoPrt)^.Status;
  case Status of
     0 :  Result := psReady;
     PRINTER_STATUS_PRINTING,
     PRINTER_STATUS_PROCESSING,
     PRINTER_STATUS_WARMING_UP,
     PRINTER_STATUS_WAITING,
     PRINTER_STATUS_IO_ACTIVE,
     PRINTER_STATUS_PENDING_DELETION,
     PRINTER_STATUS_INITIALIZING: Result := psPrinting;
     PRINTER_STATUS_PAPER_JAM,
     PRINTER_STATUS_PAPER_OUT,
     PRINTER_STATUS_PAPER_PROBLEM,
     PRINTER_STATUS_USER_INTERVENTION,
     PRINTER_STATUS_NO_TONER,
     PRINTER_STATUS_ERROR,
     PRINTER_STATUS_DOOR_OPEN,
     PRINTER_STATUS_PAGE_PUNT,
     PRINTER_STATUS_OUT_OF_MEMORY,
     PRINTER_STATUS_PAUSED : Result := psStopped;
   end;
   
  if (Result = psReady) and (Jobs > 0) then Result := psPrinting;
  finally
    FreeMem(InfoPrt);
  end;
end;

function TWinPrinter.GetCanPrint: Boolean;
begin
 Result := inherited GetCanPrint;
 Result := (DoGetPrinterState <> psStopped);
end;

function TWinPrinter.GetCanRenderCopies: Boolean;
var
 pDev : TPrinterDevice;
  Count : Integer;
begin
 Result := inherited GetCanRenderCopies;
  if (Printers.Count>0) then
  begin
    PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);
    Count := DeviceCapabilities(PChar(Pdev.Name),PCHar(PDev.Port),
                                DC_COPIES,nil,PDev.DevMode);
    Result := (Count>1);
  end;
end;

procedure TWinPrinter.AdvancedProperties;
var
  PDev: TPrinterDevice;
begin
  if Printers.Count>0 then
  begin
    PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);
    DocumentProperties(
      Widgetset.AppHandle,
      FPrinterHandle,
      pchar(PDev.Name),
      Pdev.DevMode, Pdev.DevMode,
      DM_OUT_BUFFER or DM_IN_BUFFER or DM_IN_PROMPT);
    //PrinterProperties(Widgetset.AppHandle,fPrinterHandle)
  end;
end;

initialization
  Printer:=TWinPrinter.Create;

{end.}








