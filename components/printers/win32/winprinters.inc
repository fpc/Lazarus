{**************************************************************
Implementation for winprinter
***************************************************************}
Uses WinUtilPrn, InterfaceBase, Win32Int, LCLIntf,WinVer;


{ TWinPrinter }

constructor TWinPrinter.Create;
begin
  inherited Create;

  fLastHandleType:=0; //None
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

function TWinPrinter.GetHandlePrinter : HDC;
begin
  SetIC;
  Result:=fDC;
end;


procedure TWinPrinter.SetHandlePrinter(aValue : HDC);
begin
  if aValue <> fDC then
  begin
   ClearDC;
   fDC := aValue;
   if Assigned(Canvas) then Canvas.Handle:=fDC;
   fLastHandleType:=2;
  end;
end;

function TWinPrinter.GetXDPI: Integer;
begin
  Result:=72;
  if (Printers.Count>0) then
  begin
    SetDC;
    Result:=GetDeviceCaps(fDC, LOGPIXELSX);
  end;
end;

function TWinPrinter.GetYDPI: Integer;
begin
  Result:=72;
  if (Printers.Count>0) then
  begin
    SetDC;
    Result:=GetDeviceCaps(fDC,LOGPIXELSY);
  end;
end;

procedure TWinPrinter.SetIC;
var PDev : TPrinterDevice;
begin
  if (fLastHandleType=0) and (Printers.Count>0) then
  begin
    PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);
    fDC:=CreateIC(PChar(PDev.Driver),PChar(PDev.Device),
           PChar(PDev.Port),@PDev.DevMode);

    if fDC=0 then
      fDC:=CreateIC(PChar('WINSPOOL'),PChar(PDev.Device),
           PChar(PDev.Port),@PDev.DevMode);

    if fDC=0 then
      raise EPrinter.Create(Format('Invalid printer (DC=%d Driver=%s Device=%s Port=%s)',[fDC,Pdev.Driver,PDev.Device,PDev.Port]));
    if Assigned(Canvas) then
      Canvas.Handle:=fDC;
    fLastHandleType:=1;
  end;
end;

procedure TWinPrinter.SetDC;
var PDev : TPrinterDevice;
begin
  if (fLastHandleType<>2) and (Printers.Count>0) then
  begin
    ClearDC;

    PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);
    try
    fDC:=CreateDC(nil,PChar(PDev.Device),nil,@PDev.DevMode);
    if fDC=0 then fDC:=CreateDC(PChar('WISPOOL'),PChar(PDev.Device),nil,@PDev.DevMode);


    {Workaround (hack) for Lexmark 1020 JetPrinter (Mono)}
    if fDC=0 then fDC:=CreateDC(nil,PChar(PDev.Driver),nil,@PDev.DevMode);
    if fDC=0 then fDC:=CreateDC(pChar('WINSPOOL'),PChar(PDev.Driver),nil,@PDev.DevMode);

           
    except on E:Exception do
       raise EPrinter.Create(Format('CreateDC exception: %s (LastError : %s,DC=%d Driver=%s Device=%s Port=%s)',[E.Message,SysErrorMessage(GetLastError),fDC,Pdev.Driver,PDev.Device,PDev.Port]));
    end;
    
    if fDC=0 then
      raise EPrinter.Create(Format('Invalid printer (Error : %s,DC=%d Driver=%s Device=%s Port=%s)',[SysErrorMessage(GetLastError),fDC,Pdev.Driver,PDev.Device,PDev.Port]));
    if Assigned(Canvas) then
      Canvas.Handle:=fDC;
    fLastHandleType:=2;

  end;
end;

procedure TWinPrinter.ClearDC;
begin
  if Assigned(Canvas) then
    Canvas.Handle:=0;
  if fDC<>0 then
  begin
    DeleteDC(fDC);
    fDc := 0;
  end;
  fLastHandleType:=0;
end;

procedure TWinPrinter.DoBeginDoc;
var Inf: TDocInfo;
begin
  inherited DoBeginDoc;

  if fPrinterHandle=0 then
    raise EPrinter.Create('Printer handle not defined');

  SetDC;
  Canvas.Handle:=fDC;
  Canvas.Refresh;

  FillChar(Inf,SizeOf(Inf),0);
  Inf.cbSize:=SizeOf(Inf);
  Inf.lpszDocName:=PChar(Title);

  StartDoc(fDC,Inf);
  StartPage(fDC);
end;

procedure TWinPrinter.DoNewPage;
begin
  inherited DoNewPage;

  EndPage(fDC);
  StartPage(fDC);
  Canvas.Refresh;
end;

procedure TWinPrinter.DoEndDoc(aAborded: Boolean);
begin
  inherited DoEndDoc(aAborded);

  EndPage(fDC);
  if not aAborded then
    WinUtilPrn.EndDoc(fDC);
end;

procedure TWinPrinter.DoAbort;
begin
  inherited DoAbort;
  AbortDoc(fDC);
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
    DefaultPrinter : array[0..79] of Char;
    PDev           : TPrinterDevice;

begin
  Level:=5; //Compatible with all Win32 versions
  DefaultPrinter:='';

  //Retrieve Default printer
  Flags:=PRINTER_ENUM_DEFAULT;
  //Evaluate buffer size
  Needed:=0;
  EnumPrinters(Flags,nil,Level,nil,0,Needed,PrtCount);
  if Needed>0 then
  begin
    GetMem(Buffer,Needed);
    try
      //Get default printer
      if EnumPrinters(Flags,nil,Level,Buffer,Needed,Needed,PrtCount) then
        DefaultPrinter:=PPRINTER_INFO_5(Buffer)^.pPrinterName;
    finally
      FreeMem(Buffer);
    end;
  end
  else
  begin
    GetProfileString(PChar('windows'),PChar('device'),PChar(''),DefaultPrinter,SizeOf(DefaultPrinter));
    if pos(',',DefaultPrinter)<>0 then
       DefaultPrinter:=Copy(DefaultPrinter,1,Pos(',',DefaultPrinter)-1);
  end;


  Flags:=PRINTER_ENUM_CONNECTIONS or PRINTER_ENUM_LOCAL;
  Level:=2;

  //Evaluate buffer size
  Needed:=0;
  EnumPrinters(Flags,nil,Level,nil,0,Needed,PrtCount);
  if Needed<>0 then
  begin
    GetMem(Buffer,Needed);
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
            if PPRINTER_INFO_2(InfoPrt)^.PDevMode <> nil then begin
              PDev.DevMode:=PPRINTER_INFO_2(InfoPrt)^.PDevMode^;
              PDev.Device:=PDev.DevMode.dmDeviceName;
              PDev.DefaultPaper:=PDev.DevMode.dmPaperSize;
            end
            else begin
              PDev.Device:='';
              PDev.DefaultPaper:=0;
            end;
            if AnsiCompareText(PDev.Name,DefaultPrinter)<>0 then
              Lst.AddObject(PDev.Name,PDev)
            else
            begin
              Lst.Insert(0,PDev.Name);
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
  inherited DoResetPrintersList;

  if Printers.Count>0 then
  begin
    for i:=0 to Printers.Count-1 do
    begin
      Obj:=Printers.Objects[i];
      Printers.Objects[i]:=nil;
      FreeAndNil(Obj);
    end;
  end;
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
    if Count<=0 then raise EPrinter.CreateFmt('DoEnumPapers<DC_PAPERNAMES> error : %d, (%s)',[GetLastError,SysErrorMessage(GetLastError)]);
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
    PaperC:=DeviceCapabilities(PChar(Pdev.Name),PCHar(PDev.Port),
          DC_PAPERS,@ArPapers,nil);
    if PaperC<=0 then raise EPrinter.CreateFmt('DoEnumPapers<DC_PAPERS> error : %d, (%s)',[GetLastError,SysErrorMessage(GetLastError)]);
      for i:=0 to PaperC-1 do
       Lst.Objects[i]:=TObject(ptrint(ArPapers[i]));
  end;
end;

function TWinPrinter.DoGetPaperName: string;
var i    : Integer;
    PDev : TPrinterDevice;
begin
  Result:=inherited DoGetPaperName;

  if (Printers.Count>0) then
  begin
    PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);

    i:=PaperSize.SupportedPapers.IndexOfObject(TObject(ptrint(PDev.Devmode.dmPaperSize)));
    if i<>-1 then
      Result:=PaperSize.SupportedPapers.Strings[i];
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

    i:=PaperSize.SupportedPapers.IndexOfObject(TObject(ptrint(PDev.DefaultPaper)));
    if i<>-1 then
      Result:=PaperSize.SupportedPapers.Strings[i];
  end;
end;

procedure TWinPrinter.DoSetPaperName(aName: string);
var i    : Integer;
    PDev : TPrinterDevice;
    j    : SHORT;
begin
  inherited DoSetPaperName(aName);

  if (Printers.Count>0) then
  begin

    PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);

    i:=PaperSize.SupportedPapers.IndexOf(aName);
    if i<>-1 then
    begin
      ClearDC;
      j:=SHORT(ptrint(PaperSize.SupportedPapers.Objects[i]));
      PDev.DevMode.dmPaperSize:=j;
    end;
  end;
end;

function TWinPrinter.DoGetPaperRect(aName: string; var aPaperRc: TPaperRect): Integer;
var NSize, i : Integer;
    PDev     : TPrinterDevice;
    ArSizes  : Array[0..255] of TPoint;
begin
  Result:=Inherited DoGetPaperRect(aName,aPaperRc);

  if (Printers.Count>0) then
  begin
    PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);

    //Retreive the Width and Height of aName paper
    FillChar(ArSizes,SizeOf(ArSizes),0);
    NSize:=DeviceCapabilities(PChar(Pdev.Name),PChar(PDev.Port),
          DC_PAPERSIZE,@ArSizes,nil);
    i:=PaperSize.SupportedPapers.IndexOf(aName);
    if (i>=0) and (i<NSize) and (NSize<>0) then
    begin
      aPaperRc.PhysicalRect:=Classes.Rect(0,0,ArSizes[i].X,ArSizes[i].Y);
      aPaperRc.WorkRect:=Classes.Rect(0,0,ArSizes[i].X,ArSizes[i].Y);

      Result:=1;
    end;
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
    PDev:=TPrinterDevice(Printers.Objects[i]);
    if fPrinterHandle<>0 then
      ClosePrinter(fPrinterHandle);

    if not OpenPrinter(PChar(PDev.Name),fPrinterHandle,nil) then
      raise EPrinter.CreateFmt('OpenPrinter exception : %s',[SysErrorMessage(GetlastError)]);
     Result:=i;
  end;
end;

function TWinPrinter.DoGetCopies: Integer;
var PDev : TPrinterDevice;
begin
  Result:=inherited DoGetCopies;

  if (Printers.Count>0) then
  begin
    PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);

    if PDev.DevMode.dmCopies<>0 then
      Result:=PDev.DevMode.dmCopies;
  end;
end;

procedure TWinPrinter.DoSetCopies(aValue: Integer);
var PDev : TPrinterDevice;
begin
  inherited DoSetCopies(aValue);

  if (Printers.Count>0) and (aValue>0) then
  begin
    PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);

    ClearDC;
    PDev.DevMode.dmCopies:=SHORT(aValue);
  end;
end;

function TWinPrinter.DoGetOrientation: TPrinterOrientation;
var PDev : TPrinterDevice;
begin
  Result:=inherited DoGetOrientation;

  if (Printers.Count>0) then
  begin
    PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);

    Case PDev.DevMode.dmOrientation of
      DMORIENT_PORTRAIT  : Result:=poPortrait;
      DMORIENT_LANDSCAPE : Result:=poLandscape;
    end;
  end;
end;

procedure TWinPrinter.DoSetOrientation(aValue: TPrinterOrientation);
var PDev : TPrinterDevice;
begin
  inherited DoSetOrientation(aValue);

  if (Printers.Count>0) then
  begin
    PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);

    ClearDC;
    PDev.DevMode.dmOrientation:=Win32Orientations[aValue];
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
    raise EPrinter.CreateFmt('GetPrinterType failed : %s',[SysErrorMessage(GetLastError)]);
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
    raise EPrinter.CreateFmt('GetPrinterState failed : %s',[SysErrorMessage(GetLastError)]);

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
    Count := DeviceCapabilities(PChar(Pdev.Name),PCHar(PDev.Port),DC_COPIES,nil,@PDev.DevMode);
    Result := (Count > 1);
 end;
end;

procedure TWinPrinter.AdvancedProperties;
begin
 PrinterProperties(TWin32WidgetSet(WidgetSet).AppHandle,fPrinterHandle);
end;

initialization
  Printer:=TWinPrinter.Create;

{end.}








