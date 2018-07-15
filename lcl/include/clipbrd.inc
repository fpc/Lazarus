{%MainUnit ../clipbrd.pp}

{******************************************************************************
                                  TClipBoard
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  The clipboard is able to work with the windows and gtk behaviour/features.
}

{$I clipbrd_html.inc}

{ TClipboard }

constructor TClipboard.Create;
begin
  // default: create a normal Clipboard
  Create(ctClipboard);
end;

constructor TClipboard.Create(AClipboardType: TClipboardType);
begin
  //DebugLn('[TClipboard.Create] A ',ClipboardTypeName[AClipboardType],' Self=',DbgS(Self));
  inherited Create;
  FClipboardType:=AClipboardType;
end;

destructor TClipboard.Destroy;
begin
  //DebugLn('[TClipboard.Destroy] A ',ClipboardTypeName[ClipboardType],' Self=',DbgS(Self));
  OnRequest:=nil; // this will notify the owner
  if FAllocated then begin
    ClipboardGetOwnership(ClipboardType,nil,0,nil);
    FAllocated:=false;
  end;
  Clear;
  inherited Destroy;
  //DebugLn('[TClipboard.Destroy] END ',ClipboardTypeName[ClipboardType]);
end;

function TClipboard.IndexOfCachedFormatID(FormatID: TClipboardFormat;
  CreateIfNotExists: boolean): integer;
var
  NewSize: integer;
  FormatAdded: Boolean;
begin
  //DebugLn('[TClipboard.IndexOfCachedFormatID] A ',ClipboardTypeName[ClipboardType]
  //,' Format=',FormatID,' CreateIfNotExists=',CreateIfNotExists);
  if FormatID=0 then begin
    Result:=-1;
    if CreateIfNotExists then
      raise Exception.Create(
        'IndexOfCachedFormatID: Internal Error: invalid FormatID 0 for '+
        ClipboardTypeName[ClipboardType]);
  end;
  Result:=FCount-1;
  while (Result>=0) and (FData[Result].FormatID<>FormatID) do
    dec(Result);
  FormatAdded:=false;
  if (Result<0) and CreateIfNotExists then begin
    // add new format
    inc(FCount);
    NewSize:=SizeOf(TClipboardData)*FCount;
    ReallocMem(FData,NewSize);
    Result:=FCount-1;
    FData[Result].FormatID:=FormatID;
    FData[Result].Stream:=TMemoryStream.Create;
    FSupportedFormatsChanged:=true;
    FormatAdded:=true;
  end;
  if not IsUpdating then begin
    // CreateIfNotExists = true means changing the clipboard
    // => we need OwnerShip for that
    if CreateIfNotExists and (not GetOwnerShip) then begin
      // getting ownership failed
      if FormatAdded then begin
        // undo: remove added format
        // Note: This creates a little overhead in case of an error, but reduces
        // overhead in case of everything works
        FData[Result].Stream.Free;
        NewSize:=SizeOf(TClipboardData)*FCount;
        ReallocMem(FData,NewSize);
      end;
      Result:=-1;
      raise Exception.Create('Unable to get clipboard ownership for '+
        ClipboardTypeName[ClipboardType]);
    end;
  end;
  //DebugLn('[TClipboard.IndexOfCachedFormatID] END ',ClipboardTypeName[ClipboardType]
  //,' Format=',FormatID,' CreateIfNotExists=',CreateIfNotExists,' Result=',Result);
end;

function TClipboard.AddFormat(FormatID: TClipboardFormat;
  Stream: TStream): Boolean;
// copy Stream to a MemoryStream, add it to cache and tell the interface object
var
  OldPosition: TStreamSeekType;
  i: integer;
begin
  //DebugLn('[TClipboard.AddFormat - Stream] A ',ClipboardTypeName[ClipboardType],' Format=',FormatID);
  Result:=false;
  BeginUpdate;
  try
    i:=IndexOfCachedFormatID(FormatID,true);
    if i<0 then exit;
    if FData[i].Stream<>Stream then begin
      if Stream<>nil then begin
        OldPosition:=Stream.Position;
        FData[i].Stream.LoadFromStream(Stream);
        Stream.Position:=OldPosition;
      end else
        FData[i].Stream.Clear;
      FSupportedFormatsChanged:=true;
    end;
  finally
    Result:=EndUpdate;
  end;
end;

function TClipboard.AddFormat(FormatID: TClipboardFormat;
  var Buffer; Size: Integer): Boolean;
var i: integer;
begin
  //DebugLn('[TClipboard.AddFormat - Buffer] A ',ClipboardTypeName[ClipboardType],' Format=',FormatID,' Size=',Size);
  Result:=false;
  BeginUpdate;
  try
    i:=IndexOfCachedFormatID(FormatID,true);
    if i<0 then exit;
    FData[i].Stream.Clear;
    if Size>0 then
      FData[i].Stream.Write(Buffer,Size);
  finally
    Result:=EndUpdate;
  end;
end;

function TClipboard.SetFormat(FormatID: TClipboardFormat;
  Stream: TStream): Boolean;
// copy Stream to a MemoryStream, set the cache and tell the interface object
begin
  BeginUpdate;
  try
    Clear;
    AddFormat(FormatID,Stream);
  finally
    Result:=EndUpdate;
  end;
end;

procedure TClipboard.Clear;
var i: integer;
begin
  //DebugLn('[TClipboard.Clear] A ',ClipboardTypeName[ClipboardType]);
  if FData<>nil then begin
    for i:=0 to FCount-1 do
      FData[i].Stream.Free;
    FreeMem(FData,SizeOf(TClipboardData)*FCount);
    FData:=nil;
  end;
  FCount:=0;
  //DebugLn('[TClipboard.Clear] END ',ClipboardTypeName[ClipboardType]);
end;

procedure TClipboard.Open;
// Open and Closed must be balanced.
// When the Clipboard is Open, it will not read/write from/to the interface.
// Instead it will collect all changes until Close is called.
// It will then try to commit all changes as one block.
begin
  BeginUpdate;
end;


procedure TClipboard.Close;
begin
  EndUpdate;
end;

procedure TClipboard.InternalOnRequest(
  const RequestedFormatID: TClipboardFormat; AStream: TStream);
begin
  //DebugLn('[TClipboard.InternalOnRequest] A ',ClipboardTypeName[ClipboardType]
  //,' RequestedFormatID=',RequestedFormatID,' AStream=',AStream<>nil,' Allocated=',FAllocated);
  if not FAllocated then exit;
  if (RequestedFormatID=0) then begin
    // loosing ownership
    FAllocated:=false;
    if Assigned(FOnRequest) then FOnRequest(RequestedFormatID,AStream);
    FOnRequest:=nil;
  end else begin
    GetFormat(RequestedFormatID,AStream);
  end;    
end;

function TClipboard.GetOwnerShip: boolean;
var
  FormatList: PClipboardFormat;
  i: integer;
begin
  if (not FAllocated) or FSupportedFormatsChanged then begin
    GetMem(FormatList,SizeOf(TClipboardFormat)*FCount);
    for i:=0 to FCount-1 do
      FormatList[i]:=FData[i].FormatID;
    //DebugLn(['[TClipboard.GetOwnerShip] A ',ClipboardTypeName[ClipboardType],' Allocated=',FAllocated]);
    FAllocated:=true;
    if not ClipboardGetOwnerShip(ClipboardType,@InternalOnRequest,FCount,
                                 FormatList)
    then
      FAllocated:=false;
    FreeMem(FormatList);
    FSupportedFormatsChanged:=false;
  end;
  Result:=FAllocated;
  //DebugLn('[TClipboard.GetOwnerShip] END ',ClipboardTypeName[ClipboardType],' Allocated=',FAllocated);
end;

procedure TClipboard.SetOnRequest(AnOnRequest: TClipboardRequestEvent);
begin
  if Assigned(FOnRequest) then
    // tell the old owner, that it lost the ownership
    FOnRequest(0,nil);
  FOnRequest:=AnOnRequest;
end;

procedure TClipboard.BeginUpdate;
begin
  Inc(FOpenRefCount);
end;

function TClipboard.EndUpdate: Boolean;
begin
  if FOpenRefCount = 0 then
    RaiseGDBException('TClipboard.EndUpdate');
  Result:=true;
  Dec(FOpenRefCount);
  if FOpenRefCount = 0 then begin
    if FSupportedFormatsChanged then begin
      Result:=GetOwnerShip;
      if not Result then
        Clear;
    end;
  end;
end;

function TClipboard.IsUpdating: Boolean;
begin
  Result:=FOpenRefCount>0;
end;

function TClipboard.CanReadFromInterface: Boolean;
begin
  Result:=FAllocated and (not IsUpdating);
end;

function TClipboard.CanReadFromCache: Boolean;
begin
  Result:=FAllocated or IsUpdating;
end;

procedure TClipboard.OnDefaultFindClass(Reader: TReader;
  const AClassName: string; var ComponentClass: TComponentClass);
var
  PersistentClass: TPersistentClass;
begin
  if Reader=nil then ;
  PersistentClass:=FindClass(AClassName);
  if (PersistentClass<>nil) and (PersistentClass.InheritsFrom(TComponent)) then
    ComponentClass:=TComponentClass(PersistentClass);
end;

function TClipboard.GetFormat(FormatID: TClipboardFormat;
  Stream: TStream): Boolean;
// request data from interface object or copy cached data to Stream
var i: integer;
begin
  //DebugLn('[TClipboard.GetFormat] A ',ClipboardTypeName[ClipboardType],' FormatID=',FormatID,' ',ClipboardFormatToMimeType(FormatID),' Allocated=',fAllocated);
  Result:=false;
  if Stream=nil then exit;
  if FormatID=0 then exit;
  if CanReadFromCache then begin
    if Assigned(FOnRequest) then begin
      FOnRequest(FormatID,Stream);
      Result:=true;
    end else begin
      i:=IndexOfCachedFormatID(FormatID,false);
      if i<0 then
        Result:=false
      else begin
        FData[i].Stream.Position:=0;
        if Stream is TMemoryStream then
          TMemoryStream(Stream).SetSize(Stream.Position+FData[i].Stream.Size);
        Stream.CopyFrom(FData[i].Stream,FData[i].Stream.Size);
        Result:=true;
      end;
    end;
  end else begin
    // not the clipboard owner -> request data
    Result:=ClipboardGetData(ClipboardType,FormatID,Stream);
  end;
  //DebugLn('[TClipboard.GetFormat] END ',ClipboardTypeName[ClipboardType],' FormatID=',FormatID,' Result=',Result);
end;

function TClipboard.SetComponent(Component: TComponent): Boolean;
var
  i: integer;
  s: TMemoryStream;
begin
  BeginUpdate;
  try
    i:=IndexOfCachedFormatID(PredefinedClipboardFormat(pcfComponent),true);
    s:=FData[i].Stream;
    s.Clear;
    WriteComponentAsBinaryToStream(s,Component);
    s.Position:=0;
    FSupportedFormatsChanged:=true;
  finally
    Result:=EndUpdate;
  end;
end;

function TClipboard.SetComponentAsText(Component: TComponent): Boolean;
var
  MemStream: TMemoryStream;
  s: string;
begin
  BeginUpdate;
  MemStream:=nil;
  try
    MemStream:=TMemoryStream.Create;
    WriteComponentAsTextToStream(MemStream,Component);
    SetLength(s,MemStream.Size);
    MemStream.Position:=0;
    if s<>'' then
      MemStream.Read(s[1],length(s));
    AsText:=s;
  finally
    MemStream.Free;
    Result:=EndUpdate;
  end;
end;

function TClipboard.GetComponent(Owner, Parent: TComponent): TComponent;
begin
  Result:=nil;
  GetComponent(Result,@OnDefaultFindClass,Owner,Parent);
end;

procedure TClipboard.GetComponent(var RootComponent: TComponent;
  OnFindComponentClass: TFindComponentClassEvent; Owner: TComponent;
  Parent: TComponent);
var
  MemStream: TMemoryStream;
begin
  MemStream:=TMemoryStream.Create;
  try
    if GetFormat(PredefinedClipboardFormat(pcfComponent),MemStream) then begin
      MemStream.Position := 0;
      ReadComponentFromBinaryStream(MemStream,RootComponent,
                                    OnFindComponentClass,Owner,Parent);
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TClipboard.GetComponentAsText(var RootComponent: TComponent;
  OnFindComponentClass: TFindComponentClassEvent; Owner: TComponent;
  Parent: TComponent);
var
  s: String;
  MemStream: TMemoryStream;
begin
  MemStream:=nil;
  try
    MemStream:=TMemoryStream.Create;
    s:=AsText;
    if s<>'' then
      MemStream.Write(s[1],length(s));
    MemStream.Position:=0;
    ReadComponentFromTextStream(MemStream,RootComponent,OnFindComponentClass,
                                Owner,Parent);
  finally
    MemStream.Free;
  end;
end;

function TClipboard.SetBuffer(FormatID: TClipboardFormat;
  var Buffer; Size: Integer): Boolean;
var i: integer;
begin
  BeginUpdate;
  try
    i:=IndexOfCachedFormatID(FormatID,true);
    FData[i].Stream.Clear;
    if Size>0 then begin
      FData[i].Stream.Write(Buffer,Size);
      FData[i].Stream.Position:=0;
    end;
    FSupportedFormatsChanged:=true;
  finally
    Result:=EndUpdate;
  end;
end;

procedure TClipboard.SetTextBuf(Buffer: PChar);
begin
  if Buffer=nil then Buffer:=#0;
  SetBuffer(PredefinedClipboardFormat(pcfText),Buffer^,StrLen(Buffer)+1);
end;

function TClipboard.GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
var MemStream: TMemoryStream;
begin
  Result:=0;
  if (Buffer=nil) or (BufSize=0) then exit;
  MemStream:=TMemoryStream.Create;
  try
    if GetFormat(PredefinedClipboardFormat(pcfText),MemStream) then begin
      MemStream.Position:=0;
      Result:=BufSize;
      if Result>MemStream.Size then Result:=integer(MemStream.Size);
      if Result>0 then
        MemStream.Read(Buffer^,Result);
      Buffer[Result]:=#0;
      Result:=StrLen(Buffer);
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TClipboard.SetAsText(const Value: string);
var s: string;
  i: Integer;
begin
  //DebugLn('[TClipboard.SetAsText] A ',ClipboardTypeName[ClipboardType],' "',Value,'"');
  if Assigned(FOnRequest) then exit;
  if Value<>'' then
    s:=Value
  else
    s:=#0;
  Clear;
  i := Length(Value);
  if ClipboardFormatNeedsNullByte(pcfText) then
    i := i + 1;
  SetBuffer(PredefinedClipboardFormat(pcfText),s[1],i);
  //DebugLn('[TClipboard.SetAsText] END ',ClipboardTypeName[ClipboardType],' "',Value,'"');
end;

function TClipboard.GetAsText: string;
var
  MemStream: TMemoryStream;
  ASize: int64;
begin
  //DebugLn('[TClipboard.GetAsText] A ',ClipboardTypeName[ClipboardType]);
  Result:='';
  MemStream:=TMemoryStream.Create;
  try
    if GetFormat(PredefinedClipboardFormat(pcfText),MemStream) then begin
      ASize:=MemStream.Size;
      if (ASize>0) and (pchar(MemStream.Memory)[ASize-1]=#0) then
        Dec(ASize);
      MemStream.Position:=0;
      SetLength(Result,ASize);
      if ASize>0 then
        MemStream.Read(Result[1],ASize);
    end;
  finally
    MemStream.Free;
  end;
  //DebugLn('[TClipboard.GetAsText] END ',ClipboardTypeName[ClipboardType],' "',dbgstr(Result),'"');
end;

procedure TClipboard.SupportedFormats(List: TStrings);
var cnt, i: integer;
  FormatList: PClipboardFormat;
begin
  //DebugLn('[TClipboard.SupportedFormats]');
  List.Clear;
  if CanReadFromCache then begin
    for i:=0 to FCount-1 do
      List.Add(ClipboardFormatToMimeType(FData[i].FormatID));
  end else begin
    FormatList:=nil;
    if ClipboardGetFormats(ClipboardType,cnt,FormatList) then begin
      for i:=0 to cnt-1 do
        List.Add(ClipboardFormatToMimeType(FormatList[i]));
    end;
    if FormatList<>nil then FreeMem(FormatList);
  end;
end;

procedure TClipboard.SupportedFormats(var AFormatCount: integer;
  var FormatList: PClipboardFormat);
var i: integer;
begin
  AFormatCount:=0;
  FormatList:=nil;
  if CanReadFromCache then begin
    if (FCount>0) then begin
      GetMem(FormatList,SizeOf(TClipBoardFormat)*FCount);
      for i:=0 to FCount-1 do
        FormatList[i]:=FData[i].FormatID;
      AFormatCount:=FCount;
    end;
  end else begin
    ClipboardGetFormats(ClipboardType,AFormatCount,FormatList);
  end;
end;

function TClipboard.SetSupportedFormats(AFormatCount: integer;
  FormatList: PClipboardFormat): Boolean;
var i: integer;
begin
  BeginUpdate;
  try
    Clear;
    FCount:=AFormatCount;
    GetMem(FData,SizeOf(TClipboardData)*FCount);
    for i:=0 to FCount-1 do begin
      FData[i].FormatID:=FormatList[i];
      FData[i].Stream:=TMemoryStream.Create;
    end;
    FSupportedFormatsChanged:=true;
  finally
    Result:=EndUpdate;
  end;
end;

function TClipboard.FindPictureFormatID: TClipboardFormat;
var
  List: PClipboardFormat;
  cnt, i: integer;
begin
  //DebugLn('[TClipboard.FindPictureFormatID]');
  List:=nil;
  Result:=0;
  cnt:=0;
  try
    if not CanReadFromCache then begin
      if not ClipboardGetFormats(ClipboardType,cnt,List) then
        exit;
      for i:=0 to cnt-1 do begin
        Result:=List[i];
        if TPicture.SupportsClipboardFormat(Result) then
          exit;
      end;
    end else begin
      for i:=FCount-1 downto 0 do begin
        Result:=FData[i].FormatID;
        if TPicture.SupportsClipboardFormat(Result) then
          exit;
      end;
    end;
  finally
    if List<>nil then FreeMem(List);
  end;
  Result:=0;
end;

function TClipboard.FindFormatID(const FormatName: string): TClipboardFormat;
var
  List: PClipboardFormat;
  cnt, i: integer;
begin
  //DebugLn('[TClipboard.FindPictureFormatID]');
  List:=nil;
  Result:=0;
  cnt:=0;
  try
    if not CanReadFromCache then begin
      if not ClipboardGetFormats(ClipboardType,cnt,List) then
        exit;
      for i:=0 to cnt-1 do begin
        Result:=List[i];
        if CompareText(ClipboardFormatToMimeType(Result),FormatName)=0 then
          exit;
      end;
    end else begin
      for i:=FCount-1 downto 0 do begin
        Result:=FData[i].FormatID;
        if CompareText(ClipboardFormatToMimeType(Result),FormatName)=0 then
          exit;
      end;
    end;
  finally
    if List<>nil then FreeMem(List);
  end;
  Result:=0;
end;

function TClipboard.HasPictureFormat: boolean;
begin
  Result:=FindPictureFormatID<>0;  
end;

function TClipboard.HasFormat(FormatID: TClipboardFormat): Boolean;
// ask widgetset
var List: PClipboardFormat;
  cnt, i: integer;
begin
  //DebugLn('[TClipboard.HasFormat] A ',ClipboardTypeName[ClipboardType],' Allocated=',FAllocated);
  if FormatID<>0 then begin
    if CanReadFromCache then
      Result := (IndexOfCachedFormatID(FormatID,false)>=0)
    else begin
      if not ClipboardGetFormats(ClipboardType,cnt,List) then begin
        Result:=false;
        exit;
      end;
      i:=0;
      //for i:=0 to cnt-1 do
      //DebugLn('[TClipboard.HasFormat] ',FormatID,' ',List[i]);
      while (i<cnt) and (List[i]<>FormatID) do inc(i);
      Result := i<cnt;
      if List<>nil then FreeMem(List);
    end;
    if not Result then begin
      Result := (PredefinedClipboardFormat(pcfPicture)=FormatID) and (HasPictureFormat);
    end;
  end else
    Result:=false;
  //DebugLn('[TClipboard.HasFormat] END ',ClipboardTypeName[ClipboardType],' FormatID=',FormatID,' Result=',Result);
end;

function TClipboard.HasFormatName(const FormatName: string): Boolean;
begin
  Result:=FindFormatID(FormatName)<>0;
end;

procedure TClipboard.AssignToPicture(Dest: TPicture);
var
  FormatID: TClipboardFormat;
begin
  FormatID:=FindPictureFormatID;
  if FormatID=0 then exit;
  Dest.LoadFromClipboardFormatID(ClipboardType,FormatID);
end;

procedure TClipboard.AssignPicture(Source: TPicture);
begin
  AssignGraphic(Source.Graphic);
end;

function TClipboard.AssignToGraphic(Dest: TGraphic): boolean;
var
  MimeTypes: TStringList;
  i: Integer;
  GraphicFormatID: TClipboardFormat;
begin
  Result:=false;
  MimeTypes:=TStringList.Create;
  try
    Dest.GetSupportedSourceMimeTypes(MimeTypes);
    for i:=0 to MimeTypes.Count-1 do begin
      GraphicFormatID:=FindFormatID(MimeTypes[i]);
      if GraphicFormatID<>0 then begin
        AssignToGraphic(Dest,GraphicFormatID);
        Result:=true;
        exit;
      end;
    end;
  finally
    MimeTypes.Free;
  end;
end;

function TClipboard.AssignToGraphic(Dest: TGraphic; FormatID: TClipboardFormat
  ): boolean;
var
  MemStream: TMemoryStream;
begin
  Result:=false;
  if FormatID=0 then exit;
  MemStream:=TMemoryStream.Create;
  try
    if not GetFormat(FormatID,MemStream) then exit;
    MemStream.Position:=0;
    Dest.LoadFromMimeStream(MemStream,ClipboardFormatToMimeType(FormatID));
  finally
    MemStream.Free;
  end;
  Result:=true;
end;

procedure TClipboard.AssignGraphic(Source: TGraphic);
var
  MimeType: String;
  FormatID: TClipboardFormat;
begin
  MimeType := Source.MimeType;
  FormatID:=ClipboardRegisterFormat(MimeType);
  if FormatID<>0 then
    AssignGraphic(Source,FormatID);
end;

procedure TClipboard.AssignGraphic(Source: TGraphic; FormatID: TClipboardFormat);
var
  MemStream: TMemoryStream;
begin
  MemStream:=TMemoryStream.Create;
  try
    Source.SaveToStream(MemStream);
    MemStream.Position:=0;
    SetFormat(FormatID,MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TClipboard.Assign(Source: TPersistent);
begin
  if Source is TPicture then
    AssignPicture(TPicture(Source))
  else if Source is TGraphic then
    AssignGraphic(TGraphic(Source))
  else
    inherited Assign(Source);
end;

procedure TClipboard.AssignTo(Dest: TPersistent);
begin
  if Dest is TPicture then
    AssignToPicture(TPicture(Dest))
  else if Dest is TGraphic then
    AssignToGraphic(TGraphic(Dest))
  else
    inherited AssignTo(Dest);
end;

function TClipboard.GetFormatCount: Integer;
// ask widgetset
var List: PClipboardFormat;
begin
  //DebugLn('[TClipboard.GetFormatCount]');
  if CanReadFromCache then
    Result:=FCount
  else begin
    Result:=0;
    if ClipboardGetFormats(ClipboardType,Result,List) then begin
      if List<>nil then FreeMem(List);
    end else
      Result:=0;
  end;
end;

function TClipboard.GetFormats(Index: Integer): TClipboardFormat;
var
  List: PClipboardFormat;
  cnt: integer;
begin
  //DebugLn('[TClipboard.GetFormats] Index=',Index);
  if CanReadFromCache then begin
    if (Index<0) or (Index>=FCount) then
      raise Exception.Create('TClipboard.GetFormats: Index out of bounds: Index='
        +IntToStr(Index)+' Count='+IntToStr(FCount));
    Result:=FData[Index].FormatID;
  end else begin
    if ClipboardGetFormats(ClipboardType,cnt,List) then begin
      if (Index>=0) and (Index<cnt) then
        Result:=List[Index]
      else
        Result:=0;
      if List<>nil then FreeMem(List);
    end else
      Result:=0;
  end;
end;

{ Retrieves html formatted text from the clipboard. If ExtractFragmentOnly is
  true then only the relevant html fragment is returned, the rest of the html
  string is dropped. The Office applications in Windows and Linux write the
  full html code which can be retrieved with ExtractFragmentOnly = false.
  In case of Windows, the MS header is automatically removed.}
function TClipboard.GetAsHtml(ExtractFragmentOnly: Boolean): String;
var
  Stream: TMemoryStream;
  bom: TBOM;
  US: UnicodeString;
begin
  //debugln(['TClipboard.GetAsHtml: ExtractFragmentOnly = ',ExtractFragmentOnly]);
  Result := '';
  if (CF_HTML = 0) or not HasFormat(CF_HTML) then
  begin
    //debugln(['TClipboard.GetAsHtml: CF_HTML= ',CF_HTML,' HasFormat(CF_HTML) = ',HasFormat(CF_HTML)]);
    exit;
  end;

  Stream := TMemoryStream.Create;
  try
    if not GetFormat(CF_HTML, Stream) then
    begin
      //debugln(['TClipboard.GetAsHtml: GetFormat(CF_HTML, stream) = False']);
      exit;
    end;
    Stream.Write(#0#0, Length(#0#0));

    bom := GetBomFromStream(Stream);
    case Bom of
      bomUtf8:
        begin
          Stream.Position := 3;
          SetLength(Result, Stream.Size - 3);
          Stream.Read(Result, Stream.Size - 3);
          //ClipBoard may return a larger Stream than the size of the string
          //this gets rid of it, since the string will end in a #0 (wide)char
          Result := PAnsiChar(Result);
          //debugln(['TClipboard.GetAsHtml: Found bomUtf8']);
        end;
      bomUTF16LE:
        begin
          Stream.Position := 2;
          SetLength(US, Stream.Size - 2);
          Stream.Read(US[1], Stream.Size - 2);
          //ClipBoard may return a larger Stream than the size of the string
          //this gets rid of it, since the string will end in a #0 (wide)char
          US := PWideChar(US);
          Result := Utf16ToUtf8(US);
          //debugln(['TClipboard.GetAsHtml: FoundbomUtf16LE']);
        end;
      bomUtf16BE:
        begin
          //this may need swapping of WideChars????
          Stream.Position := 2;
          SetLength(US, Stream.Size - 2);
          Stream.Read(US[1], Stream.Size - 2);
          //ClipBoard may return a larger Stream than the size of the string
          //this gets rid of it, since the string will end in a #0 (wide)char
          US := PWideChar(US);
          Result := Utf16ToUtf8(US);
          //debugln(['TClipboard.GetAsHtml: Found bomUtf16BE']);
        end;
      bomUndefined:
        begin
          //assume the first byte is part of the string and it is some AnsiString
          //CF_HTML returns a string encoded as UTF-8 on Windows
          Result := PAnsiChar(Stream.Memory);
          //debugln(['TClipboard.GetAsHtml: Found bomUndefined']);
        end;
    end;

    if (Result <> '') then begin
      if ExtractFragmentOnly then
        Result := ExtractHtmlFragmentFromClipBoardHtml(Result)
     {$IFDEF WINDOWS}
      else
        Result := ExtractHtmlFromClipboardHtml(Result);
     {$ENDIF}
    end;

  finally
    Stream.Free;
  end;
end;

{ Adds html-formatted text to the clipboard. The main Office applications in
  Windows and Linux require a valid and complete html text (i.e. with <html>
  and <body> tags), therefore we insert them if they are not present.
  In case of Windows, a specific header will be added,
  otherwise the format will not be recognized by the clipboard.
  }
procedure TClipboard.SetAsHtml(Html: String; const PlainText: String);
var
  Stream: TStream;
  IsValid: Boolean;
begin
  if CF_HTML = 0 then
    exit;
  //If the HTML does not have correct <html><body> and closing </body></html> insert them
  MaybeInsertHtmlAndBodyTags(HTML, IsValid);
  if not IsValid then
    exit;

  {$IFDEF WINDOWS}
  Stream := TStringStream.Create(InsertClipHeader(Html));
  {$ELSE}
  Stream := TStringStream.Create(Html);
  {$ENDIF}
  try
    Stream.Position := 0;
    Clipboard.AddFormat(CF_HTML, Stream);

    if (PlainText <> '') then
    begin
      Stream.Size := 0;
      Stream.Position := 0;
      Stream.WriteBuffer(Pointer(PlainText)^, Length(PlainText)+1); //Also write terminating zero
      Stream.Position := 0;
      ClipBoard.AddFormat(CF_TEXT, Stream);
    end;

  finally
    Stream.Free;
  end;
end;

procedure TClipboard.SetAsHtml(Html: String);
begin
  SetAsHtml(Html, '');
end;

