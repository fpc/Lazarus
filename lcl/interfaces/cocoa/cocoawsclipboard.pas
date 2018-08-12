unit CocoaWSClipboard;

interface

{$modeswitch objectivec2}

uses
  CocoaAll, Classes, SysUtils, contnrs
  // fcl-image
  ,fpreadpng, fpwritepng, fpimage, fpreadbmp, fpwritebmp
  ,LCLType
  ,CocoaUtils;

type
  TCocoaClipboardDataType = (ccdtText,
    ccdtCocoaStandard, // Formats supported natively by Mac OS X
    ccdtBitmap,     // BMPs need conversion to PNG to work with other Mac OS X apps
    ccdtNonStandard { Formats that will only work in LCL apps } );

  TCocoaClipboardData = class(TObject) // TClipboardFormat is a reference to a TClipboardData
  public
    MimeType: string;
    CocoaFormat: NSString;  // utilized for ccdtCocoaStandard and ccdtNonStandard
    DataType: TCocoaClipboardDataType;
    constructor Create(AMimeType: string; ACocoaFormat: NSString; ADataType: TCocoaClipboardDataType);
    destructor Destroy; override;
  end;

  TCocoaPasteboardsRef = record
    pasteboard : NSPasteboard;
    changeCount: NSInteger;
    dataProc   : TClipboardRequestEvent;
    isOwned    : Boolean;
  end;


  TDynClipboardFormatArray = array of TClipboardFormat;

  { TCocoaWSClipboard }

  TCocoaWSClipboard = class(TObject)
  public
    PrimarySelection: NSPasteboard;
    SecondarySelection: NSPasteboard;
    ClipboardFormats: TFPObjectList; // of TCocoaClipboardData
    Pasteboards: array [TClipboardType] of TCocoaPasteboardsRef;

    constructor Create;
    destructor Destroy; override;

    procedure Sync;

    function FormatToMimeType(FormatID: TClipboardFormat): string;
    function GetData(ClipboardType: TClipboardType;
      FormatID: TClipboardFormat; Stream: TStream): boolean;
    function GetFormats(ClipboardType: TClipboardType;
      var Count: integer; var List: TDynClipboardFormatArray): boolean;
    function GetOwnership(ClipboardType: TClipboardType;
      OnRequestProc: TClipboardRequestEvent; FormatCount: integer;
      Formats: PClipboardFormat): boolean;
    function RegisterFormat(const AMimeType: string): TClipboardFormat;

    function GetClipboardDataForFormat(AFormat: TClipboardFormat): TCocoaClipboardData;
    function RegisterCocoaType(AType: NSString): TClipboardFormat;
    function CocoaTypeToMimeType(AType: NSString): string;
  end;


const
  // these constants are available starting MacOSX 10.6
  // thus for earlier systems must be redeclared
  _NSPasteboardTypeString : NSString = nil;
  _NSPasteboardTypePNG : NSString = nil;

implementation

procedure InitConst;
begin
  _NSPasteboardTypeString := NSSTR('public.utf8-plain-text');
  _NSPasteboardTypePNG := NSSTR('public.png');
end;

{ TCocoaWSClipboard }

constructor TCocoaWSClipboard.Create;
var
  t : TClipboardType;
begin
  inherited Create;
  PrimarySelection := NSPasteboard.pasteboardWithUniqueName();
  SecondarySelection := NSPasteboard.pasteboardWithUniqueName();
  ClipboardFormats := TFPObjectList.Create(True);

  Pasteboards[ctPrimarySelection].pasteboard := PrimarySelection;
  Pasteboards[ctSecondarySelection].pasteboard := SecondarySelection;
  Pasteboards[ctClipboard].pasteboard := NSPasteboard.generalPasteboard;

  for t := Low(TClipboardType) to High(TClipboardType) do
    if Assigned(Pasteboards[t].pasteboard) then
      Pasteboards[t].changeCount := Pasteboards[t].pasteboard.changeCount;
end;

destructor TCocoaWSClipboard.Destroy;
begin
  PrimarySelection.releaseGlobally();
  SecondarySelection.releaseGlobally();
  ClipboardFormats.Free;
  inherited;
end;

procedure TCocoaWSClipboard.Sync;
var
  ct : TClipboardType;
  pb : NSPasteboard;
begin
  for ct := low(TClipboardType) to high(TClipboardType) do begin
    if not Pasteboards[ct].isOwned then Continue;

    pb := Pasteboards[ct].pasteboard;
    if not Assigned(pb) then Continue;

    if (pb.changeCount <> Pasteboards[ct].changeCount) then
    begin
      Pasteboards[ct].isOwned:=false;
      if Assigned(Pasteboards[ct].dataProc) then
        // notifying about the loss of ownership
        Pasteboards[ct].dataProc(0, nil);
    end;
  end;
end;

function TCocoaWSClipboard.FormatToMimeType(FormatID: TClipboardFormat
  ): string;
var
  lFormat: TCocoaClipboardData;
begin
  lFormat := GetClipboardDataForFormat(FormatID);
  if lFormat = nil then Exit;
  Result := lFormat.MimeType;
end;

function TCocoaWSClipboard.GetClipboardDataForFormat(AFormat: TClipboardFormat
  ): TCocoaClipboardData;
begin
  Result := TCocoaClipboardData(AFormat);
end;

function TCocoaWSClipboard.GetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean;
var
  pasteboard: NSPasteboard;
  lFormat: TCocoaClipboardData;
  lNSStr: NSString;
  // for text
  lStr: String;
  // for standard
  lNSData: NSData;
  lNSbytes: PByte;
  i: Integer;
  // for bitmap
  image: TFPCustomImage;
  lTmpStream: TMemoryStream;
  reader: TFPCustomImageReader;
  writer: TFPCustomImageWriter;
begin
  Result := False;
  pasteboard := Pasteboards[ClipboardType].pasteboard;

  lFormat := GetClipboardDataForFormat(FormatID);
  if lFormat = nil then Exit;

  case lFormat.DataType of
  ccdtText:
  begin
    lNSStr := pasteboard.stringForType(lFormat.CocoaFormat);
    if lNSStr = nil then Exit;
    lStr := NSStringToString(lNSStr);
    Stream.Write(lStr[1], Length(lStr));
    {$IFDEF VerboseClipboard}
      DebugLn('TCocoaWidgetSet.ClipboardGetData IsText Result=' + lStr);
    {$ENDIF}
  end;
  ccdtCocoaStandard, ccdtNonStandard:
  begin
    lNSData := pasteboard.dataForType(lFormat.CocoaFormat);
    if lNSData = nil then Exit;
    lNSbytes := lNSData.bytes;
    for i := 0 to lNSData.length-1 do
      Stream.WriteByte(lNSbytes[i]);
  end;
  // In Cocoa images are stored as PNG, convert to BMP for LCL app usage
  ccdtBitmap:
  begin
    lNSData := pasteboard.dataForType(lFormat.CocoaFormat);
    if lNSData = nil then Exit;
    lNSbytes := lNSData.bytes;

    Image := TFPMemoryImage.Create(10, 10);
    Reader := TFPReaderPNG.Create;
    Writer := TFPWriterBMP.Create;
    lTmpStream := TMemoryStream.Create;
    try
      for i := 0 to lNSData.length-1 do
        lTmpStream.WriteByte(lNSbytes[i]);
      lTmpStream.Position := 0;

      Image.LoadFromStream(lTmpStream, Reader);
      Image.SaveToStream(Stream, Writer);
    finally
      Image.Free;
      Reader.Free;
      Writer.Free;
      lTmpStream.Free;
    end;
  end;
  end;

  Result := True;
end;

function TCocoaWSClipboard.GetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: TDynClipboardFormatArray): boolean;
var
  i: Integer;
  pb : NSPasteboard;
  tp : NSString;
begin
  pb := Pasteboards[ClipboardType].pasteboard;
  if not Assigned(pb) then begin
    Result := false;
    Exit;
  end;

  i := 0;
  SetLength(List, pb.types.count);
  for tp in pb.types do
  begin
    List[i]:=RegisterCocoaType(tp);
    inc(i);
  end;

  Count := i;
end;

function TCocoaWSClipboard.GetOwnership(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent; FormatCount: integer;
  Formats: PClipboardFormat): boolean;
var
  pasteboard: NSPasteboard;
  i: Integer;
  lCurFormat: TCocoaClipboardData;
  DataStream: TMemoryStream;
  FormatToOwn: NSString;
  FormatToOwnArray: NSArray;
  // text format
  lText: string;
  lNSText: NSString;
  // non-text
  lNSData: NSData;
  // for bitmap
  image: TFPCustomImage;
  lTmpStream: TMemoryStream;
  reader: TFPCustomImageReader;
  writer: TFPCustomImageWriter;
begin
  pasteboard := Pasteboards[ClipboardType].pasteboard;

  DataStream := TMemoryStream.Create;
  try
    FormatToOwnArray := nil;
    if FormatCount>0 then
    begin
      FormatToOwnArray := NSArray(NSMutableArray.array_);
      for i := 0 to FormatCount-1 do
      begin
        lCurFormat := TCocoaClipboardData(Formats[i]);
        if lCurFormat = nil then Continue;
        FormatToOwn := lCurFormat.CocoaFormat;
        NSMutableArray(FormatToOwnArray).addObject(FormatToOwn);
      end;
    end;

    if Assigned(FormatToOwnArray) and (FormatToOwnArray.count>0) then
      pasteboard.declareTypes_owner(FormatToOwnArray, nil);

    for i := 0 to FormatCount-1 do
    begin
      lCurFormat := TCocoaClipboardData(Formats[i]);
      if lCurFormat = nil then Continue;
      DataStream.Position := 0;
      DataStream.Size := 0;
      OnRequestProc(Formats[i], DataStream);

      case lCurFormat.DataType of
      ccdtText:
      begin
        DataStream.Position := 0;
        SetLength(lText, DataStream.Size);
        DataStream.Read(lText[1], DataStream.Size);
        lNSText := NSStringUtf8(lText);

        pasteboard.setString_forType(lNSText, lCurFormat.CocoaFormat);
      end;
      ccdtCocoaStandard, ccdtNonStandard:
      begin
        DataStream.Position := 0;
        lNSData := NSData.dataWithBytes_length(DataStream.Memory, DataStream.Size);

        pasteboard.setData_forType(lNSData, lCurFormat.CocoaFormat);
      end;
      ccdtBitmap:
      begin
        Image := TFPMemoryImage.Create(10, 10);
        Reader := TFPReaderBMP.Create;
        Writer := TFPWriterPNG.Create;
        lTmpStream := TMemoryStream.Create;
        try
          DataStream.Position := 0;
          Image.LoadFromStream(DataStream, Reader);
          Image.SaveToStream(lTmpStream, Writer);
          lTmpStream.Position := 0;
          lNSData := NSData.dataWithBytes_length(lTmpStream.Memory, lTmpStream.Size);
          pasteboard.setData_forType(lNSData, lCurFormat.CocoaFormat);
        finally
          Image.Free;
          Reader.Free;
          Writer.Free;
          lTmpStream.Free;
        end;
      end;
      end;
    end;
  finally
    DataStream.Free;
  end;

  Pasteboards[ClipboardType].pasteboard:=pasteboard;
  Pasteboards[ClipboardType].dataProc:=OnRequestProc;
  if Assigned(pasteboard) then
  begin
    Pasteboards[ClipboardType].changeCount:=pasteboard.changeCount;
    Pasteboards[ClipboardType].isOwned:=true;
  end else
    Pasteboards[ClipboardType].isOwned:=false;


  Result := True;

end;

function TCocoaWSClipboard.RegisterFormat(const AMimeType: string
  ): TClipboardFormat;
var
  i: Integer;
  lCurData: TCocoaClipboardData;
  lNSStr: NSString = nil;
  lDataType: TCocoaClipboardDataType;
begin
  Result := 0;

  // Check first if it was already registered
  for i := 0 to ClipboardFormats.Count-1 do
  begin
    lCurData := TCocoaClipboardData(ClipboardFormats.Items[i]);
    if lCurData.MimeType = AMimeType then
    begin
      Result := TClipboardFormat(lCurData);
      {$IFDEF VerboseClipboard}
        DebugLn('TCocoaWidgetSet.ClipboardRegisterFormat AMimeType=' + AMimeType
          + ' Result='+DbgS(Result));
      {$ENDIF}
      Exit;
    end;
  end;

  // if none was found, we need to register it

  lDataType := ccdtNonStandard;
  // See PredefinedClipboardMimeTypes for the most common mime-types
  case AMimeType of
  'text/plain':
  begin
    //hack: the name of constants is a hack
    //      should be replaced with either weaklinking
    //      or dynamic loading (dlsym)
    lNSStr := NSSTR('public.utf8-plain-text'); // NSPasteboardTypeString; // commented out for OSX < 10.6  see #33672
    lDataType := ccdtText;
  end;
  'image/png':
  begin
    lNSStr := NSSTR('public.png'); // NSPasteboardTypePNG
    lDataType := ccdtCocoaStandard;
  end;
  'image/bmp':
  begin
    lNSStr := NSSTR('public.png'); // NSPasteboardTypePNG
    lDataType := ccdtBitmap;
  end;
  else
    lNSStr := NSStringUtf8(AMimeType);
    lDataType := ccdtNonStandard;
  end;

  if lNSStr <> nil then
  begin
    lCurData := TCocoaClipboardData.Create(AMimeType, lNSStr, lDataType);
    ClipboardFormats.Add(lCurData);
    Result := TClipboardFormat(lCurData);
  end;
end;

function TCocoaWSClipboard.RegisterCocoaType(AType: NSString): TClipboardFormat;
begin
  Result := RegisterFormat( CocoaTypeToMimeType(AType) );
end;

function TCocoaWSClipboard.CocoaTypeToMimeType(AType: NSString): string;
begin
  // "default" types must be mapped to a default LCL mime-type
  if AType.isEqualToString(_NSPasteboardTypeString) then
    Result := 'text/plan'
  else
    Result := NSStringToString(AType);
end;

{ TCocoaClipboardData }

constructor TCocoaClipboardData.Create(AMimeType: string; ACocoaFormat: NSString; ADataType: TCocoaClipboardDataType);
begin
  MimeType := AMimeType;
  CocoaFormat := ACocoaFormat;
  DataType := ADataType;
end;

destructor TCocoaClipboardData.Destroy;
begin
  CocoaFormat.release;
end;


initialization
  InitConst;

end.
