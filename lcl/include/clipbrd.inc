{%MainUnit ../clipbrd.pp}

{******************************************************************************
                                  TClipBoard
 ******************************************************************************

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
}
{
  The clipboard is able to emulate the windows and gtk behaviour/features.
  
}


{ TClipboard }

constructor TClipboard.Create;
begin
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
  while (Result>=0) and (FData[Result].FormatID<>FormatID) do dec(Result);
  FormatAdded:=false;
  if (Result<0) and CreateIfNotExists then begin
    // add format
    inc(FCount);
    NewSize:=SizeOf(TClipboardData)*FCount;
    ReallocMem(FData,NewSize);
    Result:=FCount-1;
    FData[Result].FormatID:=FormatID;
    FData[Result].Stream:=TMemoryStream.Create;
    FSupportedFormatsChanged:=true;
    FormatAdded:=true;
  end;
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
  i:=IndexOfCachedFormatID(FormatID,true);
  if i<0 then exit;
  if FData[i].Stream<>Stream then begin
    if Stream<>nil then begin
      OldPosition:=Stream.Position;
      FData[i].Stream.LoadFromStream(Stream);
      Stream.Position:=OldPosition;
    end else
      FData[i].Stream.Clear;
  end;
  Result:=true;
end;

function TClipboard.AddFormat(FormatID: TClipboardFormat;
  var Buffer; Size: Integer): Boolean;
var i: integer;
begin
  //DebugLn('[TClipboard.AddFormat - Buffer] A ',ClipboardTypeName[ClipboardType],' Format=',FormatID,' Size=',Size);
  Result:=false;
  i:=IndexOfCachedFormatID(FormatID,true);
  if i<0 then exit;
  FData[i].Stream.Clear;
  if Size>0 then
    FData[i].Stream.Write(Buffer,Size);
  Result:=true;
end;

procedure TClipboard.SetFormat(FormatID: TClipboardFormat; Stream: TStream);
// copy Stream to a MemoryStream, set the cache and tell the interface object
begin
  Clear;
  AddFormat(FormatID,Stream);
end;

procedure TClipboard.Clear;
var i: integer;
begin
  //DebugLn('[TClipboard.Clear] A ',ClipboardTypeName[ClipboardType]);
  for i:=0 to FCount-1 do
    FData[i].Stream.Free;
  if FData<>nil then begin
    FreeMem(FData,SizeOf(TClipboardData)*FCount);
    FData:=nil;
  end;
  FCount:=0;
  //DebugLn('[TClipboard.Clear] END ',ClipboardTypeName[ClipboardType]);
end;

{procedure TClipboard.Adding;
begin
  if (FOpenRefCount <> 0) and not FEmptied then begin
    Clear;
    FEmptied := True;
  end;
end;}

procedure TClipboard.Close;
begin
  if FOpenRefCount = 0 then Exit;
  Dec(FOpenRefCount);
  if FOpenRefCount = 0 then begin
  end;
end;

procedure TClipboard.Open;
begin
  if FOpenRefCount = 0 then begin
    if not GetOwnerShip then
      raise Exception.Create('unable to open clipboard');
  end;
  Inc(FOpenRefCount);
end;

procedure TClipboard.InternalOnRequest(const RequestedFormatID: TClipboardFormat;
  AStream: TStream);
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
    //DebugLn('[TClipboard.GetOwnerShip] A ',ClipboardTypeName[ClipboardType],' Allocated=',FAllocated);
    FAllocated:=ClipboardGetOwnerShip(ClipboardType,@InternalOnRequest,FCount,
      FormatList);
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

function TClipboard.GetFormat(FormatID: TClipboardFormat;
  Stream: TStream): Boolean;
// request data from interface object or copy cached data to Stream
var i: integer;
begin
  //DebugLn('[TClipboard.GetFormat] A ',ClipboardTypeName[ClipboardType],' FormatID=',FormatID,' ',ClipboardFormatToMimeType(FormatID),' Allocated=',fAllocated);
  Result:=false;
  if Stream=nil then exit;
  if FormatID=0 then exit;
  if FAllocated then begin
    // having ownership
    if Assigned(FOnRequest) then begin
      FOnRequest(FormatID,Stream);
      Result:=true;
    end else begin
      i:=IndexOfCachedFormatID(FormatID,false);
      if i<0 then
        Result:=false
      else begin
        FData[i].Stream.Position:=0;
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

procedure TClipboard.SetComponent(Component: TComponent);
var
  i: integer;
  s: TMemoryStream;
  DestroyDriver: Boolean;
  Writer: TWriter;
begin
  i:=IndexOfCachedFormatID(PredefinedClipboardFormat(pcfCustomData),true);
  s:=FData[i].Stream;
  s.Clear;
  s.WriteComponent(Component);
  DestroyDriver:=false;
  Writer := CreateLRSWriter(s,DestroyDriver);
  Try
    Writer.WriteDescendent(Component, nil);
  Finally
    if DestroyDriver then Writer.Driver.Free;
    Writer.Destroy;
  end;
  s.Position:=0;
end;

function TClipboard.GetComponent(Owner, Parent: TComponent): TComponent;
var
  MemStream: TMemoryStream;
  Reader: TReader;
  ok: boolean;
  DestroyDriver: boolean;
begin
  Result := nil;
  MemStream:=TMemoryStream.Create;
  try
    if GetFormat(PredefinedClipboardFormat(pcfComponent),MemStream) then begin
      MemStream.Position := 0;
      DestroyDriver:=false;
      Reader := CreateLRSReader(MemStream,DestroyDriver);
      try
        Reader.Parent := Parent;
        Result := Reader.ReadRootComponent(nil);
        ok:=false;
        try
          Owner.InsertComponent(Result);
          ok:=true;
        finally
          if not ok then Result.Free;
        end;
      finally
        if DestroyDriver then Reader.Driver.Free;
        Reader.Free;
      end;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TClipboard.SetBuffer(FormatID: TClipboardFormat;
  var Buffer; Size: Integer);
var i: integer;
begin
  i:=IndexOfCachedFormatID(FormatID,true);
  FData[i].Stream.Clear;
  if Size>0 then begin
    FData[i].Stream.Write(Buffer,Size);
    FData[i].Stream.Position:=0;
  end;
end;

procedure TClipboard.SetTextBuf(Buffer: PChar);
begin
  if Buffer=nil then Buffer:=#0;
  SetBuffer(PredefinedClipboardFormat(pcfText),Buffer^,StrLen(Buffer));
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
begin
  //DebugLn('[TClipboard.SetAsText] A ',ClipboardTypeName[ClipboardType],' "',Value,'"');
  if Assigned(FOnRequest) then exit;
  if Value<>'' then
    s:=Value
  else
    s:=#0;
  SetBuffer(PredefinedClipboardFormat(pcfText),s[1],length(Value)+1);
  //DebugLn('[TClipboard.SetAsText] END ',ClipboardTypeName[ClipboardType],' "',Value,'"');
end;

function TClipboard.GetAsText: string;
var MemStream: TMemoryStream;
begin
  //DebugLn('[TClipboard.GetAsText] A ',ClipboardTypeName[ClipboardType]);
  Result:='';
  MemStream:=TMemoryStream.Create;
  try
    if GetFormat(PredefinedClipboardFormat(pcfText),MemStream) then begin
      MemStream.Position:=0;
      SetLength(Result,MemStream.Size);
      if length(Result)>0 then
        MemStream.Read(Result[1],length(Result));
    end;
  finally
    MemStream.Free;
  end;
  //DebugLn('[TClipboard.GetAsText] END ',ClipboardTypeName[ClipboardType],' "',Result,'"');
end;

procedure TClipboard.SupportedFormats(List: TStrings);
var cnt, i: integer;
  FormatList: PClipboardFormat;
begin
  //DebugLn('[TClipboard.SupportedFormats]');
  List.Clear;
  if FAllocated then begin
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
  if FAllocated and (FCount>0) then begin
    GetMem(FormatList,SizeOf(TClipBoardFormat)*FCount);
    for i:=0 to FCount-1 do
      FormatList[i]:=FData[i].FormatID;
    AFormatCount:=FCount;
  end;
end;

procedure TClipboard.SetSupportedFormats(AFormatCount: integer;
  FormatList: PClipboardFormat);
var i: integer;
begin
  Clear;
  FCount:=AFormatCount;
  GetMem(FData,SizeOf(TClipboardData)*FCount);
  for i:=0 to FCount-1 do begin
    FData[i].FormatID:=FormatList[i];
    FData[i].Stream:=TMemoryStream.Create;
  end;
  FSupportedFormatsChanged:=true;
  if not GetOwnerShip then begin
    raise Exception.Create('Unable to get clipboard ownership for '+
      ClipboardTypeName[ClipboardType]);
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
  if not FAllocated then begin
    if not ClipboardGetFormats(ClipboardType,cnt,List) then
      exit;
  end;
  try
    if not FAllocated then begin
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
  if not FAllocated then begin
    if not ClipboardGetFormats(ClipboardType,cnt,List) then
      exit;
  end;
  try
    if not FAllocated then begin
      for i:=0 to cnt-1 do begin
        Result:=List[i];
        if AnsiCompareText(ClipboardFormatToMimeType(Result),FormatName)=0 then
          exit;
      end;
    end else begin
      for i:=FCount-1 downto 0 do begin
        Result:=FData[i].FormatID;
        if AnsiCompareText(ClipboardFormatToMimeType(Result),FormatName)=0 then
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
// ask interfaceobject
var List: PClipboardFormat;
  cnt, i: integer;
begin
  //DebugLn('[TClipboard.HasFormat] A ',ClipboardTypeName[ClipboardType],' Allocated=',FAllocated);
  if FormatID<>0 then begin
    if FAllocated then
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
      Result:=
            ((PredefinedClipboardFormat(pcfPicture)=FormatID)
        or (PredefinedClipboardFormat(pcfDelphiPicture)=FormatID))
        and (HasPictureFormat);
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
  MimeType:=Source.GetDefaultMimeType;
  FormatID:=ClipboardRegisterFormat(MimeType);
  if FormatID<>0 then
    AssignGraphic(Source,FormatID);
end;

procedure TClipboard.AssignGraphic(Source: TGraphic; FormatID: TClipboardFormat
  );
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

{function TClipboard.GetAsHandle(Format: Word): THandle;
begin
  Open;
  try
    Result := GetClipboardData(Format);
  finally
    Close;
  end;
end;}

{procedure TClipboard.SetAsHandle(Format: Word; Value: THandle);
begin
  Open;
  try
    Adding;
    SetClipboardData(Format, Value);
  finally
    Close;
  end;
end;}

function TClipboard.GetFormatCount: Integer;
// ask interfaceobject
var List: PClipboardFormat;
begin
  //DebugLn('[TClipboard.GetFormatCount]');
  if FAllocated then
    Result:=FCount
  else begin
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
  if FAllocated then begin
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


{
  $Log$
  Revision 1.22  2005/06/21 11:42:35  vincents
  copy terminating #0 to clipboard too, windows expects it.

  Revision 1.21  2005/03/07 21:59:44  vincents
  changed hexstr(cardinal()) for pointers to dbgs() and other 64-bits fixes   from Peter Vreman

  Revision 1.20  2005/01/03 22:44:31  mattias
  implemented TControl.AnchorSide

  Revision 1.19  2005/01/03 16:32:13  micha
  always request ownership when about to modify buffer

  Revision 1.18  2004/08/15 14:39:36  mattias
  implemented platform independent binary object streamer

  Revision 1.17  2004/05/11 11:42:27  mattias
  replaced writeln by debugln

  Revision 1.16  2004/04/10 17:58:56  mattias
  implemented mainunit hints for include files

  Revision 1.15  2003/12/25 14:17:07  mattias
  fixed many range check warnings

  Revision 1.14  2003/09/10 19:15:16  mattias
  implemented copying graphics from/to clipboard

  Revision 1.13  2003/09/10 16:33:51  mattias
  added clipboard checks

  Revision 1.12  2003/09/10 16:29:13  mattias
  added Kylix 3 specials

  Revision 1.11  2003/01/06 10:51:41  mattias
  freeing stopped external tools

  Revision 1.10  2002/10/24 10:05:51  lazarus
  MG: broke graphics.pp <-> clipbrd.pp circle

  Revision 1.9  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.8  2002/04/04 12:25:01  lazarus
  MG: changed except statements to more verbosity

  Revision 1.7  2002/03/11 23:22:46  lazarus
  MG: added TPicture clipboard support

  Revision 1.6  2002/03/09 11:55:13  lazarus
  MG: fixed class method completion

  Revision 1.5  2001/11/14 19:10:03  lazarus
  MG: fixes for parser and linkscanner and small cleanups

  Revision 1.4  2001/11/12 16:56:07  lazarus
  MG: CLIPBOARD

  Revision 1.3  2001/09/30 08:34:49  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.2  2001/08/07 11:05:51  lazarus
  MG: small bugfixes

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

}
