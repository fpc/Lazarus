{ force CRLF fix }

{**************************************************************
Implementation for winprinter
***************************************************************}
Uses WinUtilPrn, InterfaceBase, Win32Int, LCLIntf;

Const
  Win32Orientations: array [TPrinterOrientation] of Integer = (
    DMORIENT_PORTRAIT, DMORIENT_LANDSCAPE, DMORIENT_PORTRAIT, DMORIENT_LANDSCAPE);

Type
  TPrinterDevice = class
    Name   : String;
    Driver : String;
    Device : String;
    Port   : String;

    DefaultPaper : Short;

    DevMode: TDeviceMode;
  end;

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

function TWinPrinter.BaseDialogs(aFlag : DWORD):boolean;
var lpp        : tagPD;
    PDev       : TPrinterDevice;
    DevMode    : PDeviceMode;
    DeviceMode : THandle;

    DevNames   : PDevNames;
    St         : PChar;
begin
  Result:=False;
  if Printers.Count>0 then
  begin
    FillChar(lpp,SizeOf(lpp),0);

    with lpp do
    begin
      lStructSize:=SizeOf(lpp);
      hInstance:=LCLType.HInstance;
      Flags:=aFlag;
      hWndOwner:=TWin32WidgetSet(InterfaceObject).AppHandle;

      PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);

      DeviceMode:=GlobalAlloc(GHND,SizeOf(PDev.DevMode)+1);
      DevMode:=PDeviceMode(GlobalLock(DeviceMode));
      try
        DevMode^:=PDev.DevMode;
      finally
        GlobalUnLock(DeviceMode);
      end;

      hDevMode:=DeviceMode;
      nCopies:=1;
      try
        if PrintDlg(lpp) then
        begin
          St:='';
          //Change Selected printer
          if lpp.hDevNames<>0 then
          begin
            DevNames:=PDevNames(GlobalLock(lpp.hDevNames));
            try
              St:=PChar(DevNames)+DevNames^.wDeviceOffset;
              SetPrinter(St);
            finally
              GlobalUnlock(lpp.hDevNames);
              GlobalFree(lpp.hDevNames);
            end;
          end;

          Result:=True;

          if lpp.hDevMode<>0 then
          begin
            DevMode:=PDeviceMode(GlobalLock(lpp.hDevMode));
            try
              //Set the properties for the selected printer
              PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);
              PDev.DevMode:=DevMode^;
            finally
              GlobalUnlock(lpp.hDevMode);
              GlobalFree(lpp.hDevMode);
            end;
          end;
        end;
      finally
         if DeviceMode<>0 then
           GlobalFree(DeviceMode);

         if hDevNames<>0 then
           GlobalFree(hDevNames);
      end;
    end;
  end;
end;

function TWinPrinter.PrinterSetup: Boolean;
begin
  Result:=BaseDialogs(PD_PRINTSETUP);
end;

function TWinPrinter.PrintDialog: Boolean;
begin
  Result:=BaseDialogs(PD_COLLATE or PD_USEDEVMODECOPIES);
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
      raise EPrinter.Create(Format('Invalide printer (DC=%d Driver=%s Device=%s Port=%s)',[fDC,Pdev.Driver,PDev.Device,PDev.Port]));
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
    fDC:=CreateDC(PChar(PDev.Driver),PChar(PDev.Device),
            PChar(PDev.Port),@PDev.DevMode);
    if fDC=0 then
      raise EPrinter.Create(Format('Invalide printer (DC=%d Driver=%s Device=%s Port=%s)',[fDC,Pdev.Driver,PDev.Device,PDev.Port]));
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
      DeleteDC(fDC);
  fLastHandleType:=0;
end;

procedure TWinPrinter.DoBeginDoc;
var Inf: TDocInfo;
begin
  inherited DoBeginDoc;

  if fPrinterHandle=0 then
    raise EPrinter.Create('Printer handle not defined');

  ClearDC;
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
    LstStr         : TStringList;
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
            PDev.Name   :=PPRINTER_INFO_2(InfoPrt)^.pPrinterName;
            PDev.Device :=PPRINTER_INFO_2(InfoPrt)^.PDevMode^.dmDeviceName;
            PDev.Driver :=PPRINTER_INFO_2(InfoPrt)^.pDriverName;
            PDev.Port   :=PPRINTER_INFO_2(InfoPrt)^.pPortName;
            PDev.DevMode:=PPRINTER_INFO_2(InfoPrt)^.PDevMode^;
            PDev.DefaultPaper:=PPRINTER_INFO_2(InfoPrt)^.PDevMode^.dmPaperSize;

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

    //Reteive the supported papers
    PaperC:=0;
    GetMem(Buffer,64*255);
    try
      PaperC:=DeviceCapabilities(PChar(Pdev.Name),PCHar(PDev.Port),
          DC_PAPERNAMES,Buffer,@PDev.DevMode);
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
          DC_PAPERS,@ArPapers,@PDev.DevMode);
    for i:=0 to PaperC-1 do
      Lst.Objects[i]:=TObject(Pointer(Integer(ArPapers[i])));
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

    i:=PaperSize.SupportedPapers.IndexOfObject(TObject(Pointer(Integer(PDev.DefaultPaper))));
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
    ClearDC;

    PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);

    i:=PaperSize.SupportedPapers.IndexOf(aName);
    if i<>-1 then
    begin
      j:=SHORT(Pointer(PaperSize.SupportedPapers.Objects[i]));
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
          DC_PAPERSIZE,@ArSizes,@PDev.DevMode);
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

    PDev:=TPrinterDevice(Printers.Objects[PrinterIndex]);
    if fPrinterHandle<>0 then
      ClosePrinter(fPrinterHandle);

    if OpenPrinter(PChar(PDev.Name),fPrinterHandle,nil) then
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
    PDev.DevMode.dmCopies:=aValue;
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


INITIALIZATION
  Printer:=TWinPrinter.Create;

{end.}








