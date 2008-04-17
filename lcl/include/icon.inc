{%MainUnit ../graphics.pp}

{******************************************************************************
                                    TIcon
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{ TIcon }

const
  IconSignature: array [0..3] of char = #0#0#1#0;
  CursorSignature: array [0..3] of char = #0#0#2#0;

function TestStreamIsIcon(const AStream: TStream): boolean;
var
  Signature: array[0..3] of char;
  ReadSize: Integer;
  OldPosition: TStreamSeekType;
begin
  OldPosition:=AStream.Position;
  ReadSize:=AStream.Read(Signature, SizeOf(Signature));
  Result:=(ReadSize=SizeOf(Signature)) and CompareMem(@Signature,@IconSignature,4);
  AStream.Position:=OldPosition;
end;

function TestStreamIsCursor(const AStream: TStream): boolean;
var
  Signature: array[0..3] of char;
  ReadSize: Integer;
  OldPosition: TStreamSeekType;
begin
  OldPosition:=AStream.Position;
  ReadSize:=AStream.Read(Signature, SizeOf(Signature));
  Result:=(ReadSize=SizeOf(Signature)) and CompareMem(@Signature,@CursorSignature,4);
  AStream.Position:=OldPosition;
end;

procedure TIcon.ReadData(Stream: TStream);
var
  Size: longint;
  Position: TStreamSeekType;
begin
  Position := Stream.Position;
  Stream.Read(Size, 4); // Beware BigEndian and LowEndian sytems
  if CompareMem(@Size,@IconSignature,4) then begin
    // Assume Icon - stream without explicit size
    Stream.Position := Position;
    ReadStream(Stream, false, Size);
  end else begin
    Size := LEtoN(Size);
    ReadStream(Stream, true, Size);
  end;
end;

procedure TIcon.InitFPImageReader(IntfImg: TLazIntfImage; ImgReader: TFPCustomImageReader);
begin
  inherited InitFPImageReader(IntfImg, ImgReader);
  if ImgReader is TLazReaderIcon then
    TLazReaderIcon(ImgReader).Icon := self;
end;

function TIcon.LazarusResourceTypeValid(const ResourceType: string): boolean;
var
  ResType: String;
begin
  if Length(ResourceType) < 3 then Exit(False);

  ResType := UpperCase(ResourceType);
  case ResType[1] of
    'I': begin
      Result := (ResType = 'ICO') or (ResType = 'ICON');
    end;
  else
    Result := inherited LazarusResourceTypeValid(ResType);
  end;
end;

class function TIcon.GetFileExtensions: string;
begin
  Result:='ico';
end;

destructor TIcon.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FBitmaps);
end;

function TIcon.GetBitmapNativeType: TBitmapNativeType;
begin
  Result := bnIcon;
end;

procedure TIcon.AddBitmap(Bitmap: TBitmap);
begin
  if not Assigned(FBitmaps) then
    FBitmaps := TObjectList.create(True);
  FBitmaps.Add(Bitmap);
end;


