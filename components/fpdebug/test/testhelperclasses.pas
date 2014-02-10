unit TestHelperClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpImgReaderBase, FpDbgDwarfConst, FpDbgLoader, FpDbgInfo;

const
  TestAddrSize = sizeof(Pointer);

type

  { TTestMemReader }

  TTestMemReader = class(TFpDbgMemReaderBase)
  public
    RegisterValues: array[0..30] of TDbgPtr;
    function ReadMemory(AnAddress: FpDbgInfo.TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; override;
    function ReadMemoryEx({%H-}AnAddress, {%H-}AnAddressSpace: FpDbgInfo.TDbgPtr; {%H-}ASize: Cardinal; {%H-}ADest: Pointer): Boolean; override;
    function ReadRegister(ARegNum: Integer; out AValue: FpDbgInfo.TDbgPtr): Boolean; override;
  end;

  TTestDwarfAbbrev = class;
  TTestDwarfInfoEntry = class;

  { TTestDummySection }

  TTestDummySection = class
  public
    Section: TDbgImageSection;
    procedure CreateSectionData; virtual;
  end;

  { TTestDummyFileSource }

  TTestDummyFileSource = class(TDbgImageReader)
  private
    FSections: TStringList;
    function GetTestSection(const AName: String): TTestDummySection;
  protected
    function GetSection(const AName: String): PDbgImageSection; override;
    //procedure LoadSections;
  public
    class function isValid({%H-}ASource: TDbgFileLoader): Boolean; override;
    class function UserName: AnsiString; override;
  public
    constructor Create; overload;
    destructor Destroy; override;
    property TestSection[const AName: String]: TTestDummySection read GetTestSection;
  end;

  { TTestDummyImageLoader }

  TTestDummyImageLoader = class(TDbgImageLoader)
  private
    FImgReader: TTestDummyFileSource;
  protected
  public
    constructor Create; override;
    property TestImgReader: TTestDummyFileSource read FImgReader;
  end;
  TTestDummyImageLoaderClass = class of TTestDummyImageLoader;

  { TTestDummySectionAbbrevs }

  TTestDummySectionAbbrevs = class(TTestDummySection)
  private
    FCurrentID: Cardinal;
    FList: TList;
    function GetNextID: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    function GetNewAbbrevObj: TTestDwarfAbbrev;
    procedure CreateSectionData; override;
  end;

  { TTestDummySectionInfoEntries }

  TTestDummySectionInfoEntries = class(TTestDummySection)
  private
    FAddrSize: Byte;
    FFirstEntry: TTestDwarfInfoEntry;
    FVersion: Word;
  protected
    function CreateInfoEntryObj: TTestDwarfInfoEntry;
  public
    AbbrevSection: TTestDummySectionAbbrevs;
    constructor Create;
    destructor Destroy; override;
    property Version: Word read FVersion write FVersion;
    property AddrSize: Byte read FAddrSize write FAddrSize;
    function GetFirstInfoEntryObj: TTestDwarfInfoEntry;
    procedure CreateSectionData; override;
  end;

  { TTestDwarfAbbrev }

  TTestDwarfAbbrev = class
  private
    FSection: TTestDummySectionAbbrevs;
    FChildren: Byte;
    FId: Cardinal;
    FTag: Cardinal;
    FData: Array of Cardinal;
    FEncoded: Array of Byte;
    procedure Encode;
  public
    property Id: Cardinal read FId write FId;
    property Tag: Cardinal read FTag write FTag;
    property Children: Byte read FChildren write FChildren;
    procedure Add(ATag, AForm: Cardinal);
    function DataLength: Integer;
    function Data: Pointer;
  end;

  { TTestDwarfInfoEntry }

  PTestDwarfInfoEntry = ^TTestDwarfInfoEntry;
  TTestDwarfInfoEntry = class
  private
    FAbbrevObj: TTestDwarfAbbrev;
    FSection: TTestDummySectionInfoEntries;
    FChildren: TList;
    FEncoded: Array of Byte;
    FRefList: array of record
        Index, FSize: Integer;
        AData: TTestDwarfInfoEntry;
        ADataRef: PTestDwarfInfoEntry;
      end;
    function GetChildren: Byte;
    function GetTag: Cardinal;
    procedure InitEncoded;
    procedure SetChildren(AValue: Byte);
    procedure SetTag(AValue: Cardinal);
  protected
    FWrittenAtIndex: Integer;
    function DataLengthIncl: Integer; // with Children
    procedure WriteToSection(ASectionMem: PByte; AIndex: Integer);
    procedure WriteToSectionFIxRef(ASectionMem: PByte);
  public
    constructor Create;
    destructor Destroy; override;
    property Tag: Cardinal read GetTag write SetTag;
    property Children: Byte read GetChildren write SetChildren;

    procedure Add(AnAttrib, AForm: Cardinal; AData: Array of Byte);
    procedure Add(AnAttrib, AForm: Cardinal; AData: String);
    procedure AddSLEB(AnAttrib, AForm: Cardinal; AData: Int64);
    procedure AddULEB(AnAttrib, AForm: Cardinal; AData: QWord);
    procedure AddAddr(AnAttrib, AForm: Cardinal; AData: QWord);
    procedure Add(AnAttrib, AForm: Cardinal; AData: QWord); // ULEB
    function AddRef(AnAttrib, AForm: Cardinal; AData: TTestDwarfInfoEntry): Integer;
    function AddRef(AnAttrib, AForm: Cardinal; AData: PTestDwarfInfoEntry): Integer;

    procedure SetRef(AIndex: Integer; AData: TTestDwarfInfoEntry);

    function GetNewChild: TTestDwarfInfoEntry;
    function DataLength: Integer; // Exclude Children
    function Data: Pointer;
  end;

function ULEB(ANum: QWord): TBytes;
function SLEB(ANum: Int64): TBytes;
function AddrB(ANum: Int64): TBytes;
function AddrB(ANum: Pointer): TBytes;
function NumS(ANum: Int64; ASize: Integer): TBytes;
function NumU(ANum: QWord; ASize: Integer): TBytes;

function Bytes(a: Array of TBytes): TBytes;
function BytesLen1(a: Array of TBytes): TBytes;
function BytesLen2(a: Array of TBytes): TBytes;
function BytesLen4(a: Array of TBytes): TBytes;
function BytesLen8(a: Array of TBytes): TBytes;
function BytesLenU(a: Array of TBytes): TBytes;

operator := (a: Smallint) b: TBytes;

implementation

operator := (a: Smallint)b: TBytes;
begin
  assert( (a>= -128) and (a<=255));
  SetLength(b, 1);
  b[0] := Byte(a and 255);
end;

function Bytes(a: array of TBytes): TBytes;
var
  i, l, p: Integer;
begin
  l := 0;
  for i := low(a) to high(a) do
    l := l + Length(a[i]);
  SetLength(Result, l);
  p := 0;
  for i := low(a) to high(a) do begin
    l := Length(a[i]);
    if l > 0 then
      move(a[i][0], Result[p], l*SizeOf(Result[0]));
    inc(p, l);
  end;
end;

function BytesLen1(a: array of TBytes): TBytes;
var
  l: Integer;
  d: TBytes;
begin
  d := Bytes(a);
  l := Length(d);
  assert(l <= $ff);
  Result := Bytes([Byte(l), d]);
end;

function BytesLen2(a: array of TBytes): TBytes;
var
  l: Integer;
  b: array[0..1] of Byte;
  d: TBytes;
begin
  d := Bytes(a);
  l := Length(d);
  assert(l <= $ffff);
  PWord(@b[0])^ := Word(l);
  Result := Bytes([b[0], b[1], Bytes(d)]);
end;

function BytesLen4(a: array of TBytes): TBytes;
var
  l: Integer;
  b: array[0..3] of Byte;
  d: TBytes;
begin
  d := Bytes(a);
  l := Length(d);
  assert(l <= $ffff);
  PDWord(@b[0])^ := DWord(l);
  Result := Bytes([b[0], b[1], b[2], b[3], Bytes(d)]);
end;

function BytesLen8(a: array of TBytes): TBytes;
var
  l: Integer;
  b: array[0..7] of Byte;
  d: TBytes;
begin
  d := Bytes(a);
  l := Length(d);
  assert(l <= $ffff);
  PQWord(@b[0])^ := QWord(l);
  Result := Bytes([b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7], Bytes(d)]);
end;

function BytesLenU(a: array of TBytes): TBytes;
var
  l: Integer;
begin
  l := Length(a);
  Result := Bytes([ULEB(l), Bytes(a)]);
end;

procedure WriteULEB128(ANum: QWord; var ADest: TBytes; ADestIdx: Integer);
  procedure AddByte(AByte: Byte);
  begin
    if ADestIdx >= Length(ADest) then SetLength(ADest, ADestIdx + 1);
    ADest[ADestIdx] := AByte;
    inc(ADestIdx);
  end;
begin
  if ANum = 0 then begin
    AddByte(0);
    exit;
  end;;

  while ANum <> 0 do begin
    if ANum > $7f then
      AddByte((ANum and $7f) + $80)
    else
      AddByte((ANum and $7f));
    ANum := ANum shr 7;
  end;
end;

procedure WriteSLEB128(ANum: Int64; var ADest: TBytes; ADestIdx: Integer);
  procedure AddByte(AByte: Byte);
  begin
    if ADestIdx >= Length(ADest) then SetLength(ADest, ADestIdx + 1);
    ADest[ADestIdx] := AByte;
    inc(ADestIdx);
  end;
var
  n: Integer;
  c: Boolean;
  UNum: QWord;
begin
  if ANum = 0 then begin
    AddByte(0);
    exit;
  end;

  if ANum < 0 then begin
    UNum := QWord(ANum);
    n := 9*7;
    while n > 0 do begin
      if ( (UNum and (QWord($7f) shl n)) = (high(QWord) and (QWord($7f) shl n)) ) and
         ( (UNum and (QWord(1) shl (n-1))) <> 0 )
      then
        UNum := UNum and not(high(QWord) shl n)
      else
        break;
      dec(n, 7);
    end;

    while UNum <> 0 do begin
      if UNum > $7f then
        AddByte((UNum and $7f) + $80)
      else
        AddByte((UNum and $7f));
      UNum := UNum shr 7;
    end;

  end
  else begin

    c := False;
    while (ANum <> 0) or c do begin
      c := (ANum and $40) <> 0; // write extra 0, to prevent sign extend
      if c or (ANum > $7f) then
        AddByte((ANum and $7f) + $80)
      else
        AddByte((ANum and $7f));
      ANum := ANum shr 7;
    end;

  end;
end;

function ULEB(ANum: QWord): TBytes;
begin
  SetLength(Result, 0);
  WriteULEB128(ANum, Result, 0);
end;

function SLEB(ANum: Int64): TBytes;
begin
  SetLength(Result, 0);
  WriteSLEB128(ANum, Result, 0);
end;

function AddrB(ANum: Int64): TBytes;
begin
  SetLength(Result, TestAddrSize);
  if TestAddrSize = 4
  then PInteger(@Result[0])^ := Integer(ANum)
  else PInt64(@Result[0])^ := Int64(ANum);
end;

function AddrB(ANum: Pointer): TBytes;
begin
  Result := AddrB(Int64(ANum));
end;

function NumS(ANum: Int64; ASize: Integer): TBytes;
begin
  SetLength(Result, ASize);
  case ASize of
    1: PShortInt(@Result[0])^ := ShortInt(ANum);
    2: PSmallInt(@Result[0])^ := SmallInt(ANum);
    4: PInteger(@Result[0])^ := Integer(ANum);
    8: PInt64(@Result[0])^ := Int64(ANum);
  end;
end;

function NumU(ANum: QWord; ASize: Integer): TBytes;
begin
  SetLength(Result, ASize);
  case ASize of
    1: PByte(@Result[0])^ := Byte(ANum);
    2: PWord(@Result[0])^ := Word(ANum);
    4: PDWord(@Result[0])^ := DWord(ANum);
    8: PQWord(@Result[0])^ := QWord(ANum);
  end;
end;

{ TTestMemReader }

function TTestMemReader.ReadMemory(AnAddress: FpDbgInfo.TDbgPtr; ASize: Cardinal;
  ADest: Pointer): Boolean;
begin
  Result := True;
  Move(Pointer(AnAddress)^, ADest^, ASize);
end;

function TTestMemReader.ReadMemoryEx(AnAddress, AnAddressSpace: FpDbgInfo.TDbgPtr;
  ASize: Cardinal; ADest: Pointer): Boolean;
begin
  Result := False;
end;

function TTestMemReader.ReadRegister(ARegNum: Integer; out AValue: FpDbgInfo.TDbgPtr): Boolean;
begin
  Result := True;
  AValue := RegisterValues[ARegNum];
end;

{ TTestDwarfInfoEntry }

procedure TTestDwarfInfoEntry.InitEncoded;
begin
  SetLength(FEncoded, 0);
  WriteULEB128(FAbbrevObj.Id, FEncoded, length(FEncoded));
end;

function TTestDwarfInfoEntry.GetChildren: Byte;
begin
  Result := FAbbrevObj.Children;
end;

function TTestDwarfInfoEntry.GetTag: Cardinal;
begin
  Result := FAbbrevObj.Tag;
end;

procedure TTestDwarfInfoEntry.SetChildren(AValue: Byte);
begin
  FAbbrevObj.Children := AValue;
end;

procedure TTestDwarfInfoEntry.SetTag(AValue: Cardinal);
begin
  FAbbrevObj.Tag := AValue;
end;

function TTestDwarfInfoEntry.DataLengthIncl: Integer;
var
  i: Integer;
begin
  Result := DataLength;
  for i := 0 to FChildren.Count - 1 do
    Result := Result + TTestDwarfInfoEntry(FChildren[i]).DataLengthIncl;
end;

procedure TTestDwarfInfoEntry.WriteToSection(ASectionMem: PByte; AIndex: Integer);
var
  i: Integer;
begin
  FWrittenAtIndex := AIndex;
  Move(FEncoded[0], (ASectionMem+AIndex)^, Length(FEncoded));

  AIndex := AIndex + Length(FEncoded);
  if FAbbrevObj.Children <> 0 then begin
    for i := 0 to FChildren.Count - 1 do begin
      TTestDwarfInfoEntry(FChildren[i]).WriteToSection(ASectionMem, AIndex);
      AIndex := AIndex + TTestDwarfInfoEntry(FChildren[i]).DataLengthIncl;
    end;
    PByte(ASectionMem+AIndex)^ := 0;
    AIndex := AIndex + 1;
  end
  else
    Assert(FChildren.Count = 0);

  WriteToSectionFIxRef(ASectionMem);
end;

procedure TTestDwarfInfoEntry.WriteToSectionFIxRef(ASectionMem: PByte);
var
  i: Integer;
  v: Integer;
  o: TTestDwarfInfoEntry;
begin
  for i := 0 to Length(FRefList) - 1 do begin
    assert((FRefList[i].AData <> nil) xor (FRefList[i].ADataRef <> nil));
    o := FRefList[i].AData;
    if (o = nil) then
      o := FRefList[i].ADataRef^;
    v := o.FWrittenAtIndex;
    case FRefList[i].FSize of
      1:  PByte(ASectionMem + FWrittenAtIndex + FRefList[i].Index)^ := v;
      2:  PWord(ASectionMem + FWrittenAtIndex + FRefList[i].Index)^ := v;
      4:  PCardinal(ASectionMem + FWrittenAtIndex + FRefList[i].Index)^ := v;
      8:  PQWord(ASectionMem + FWrittenAtIndex + FRefList[i].Index)^ := v;
    end;
  end;


  for i := 0 to FChildren.Count - 1 do
    TTestDwarfInfoEntry(FChildren[i]).WriteToSectionFIxRef(ASectionMem);
end;

constructor TTestDwarfInfoEntry.Create;
begin
  FChildren := TList.Create;
end;

destructor TTestDwarfInfoEntry.Destroy;
var
  i: Integer;
begin
  for i := 0 to FChildren.Count - 1 do
    TObject(FChildren[i]).Free;
  FreeAndNil(FChildren);
  inherited Destroy;
end;

procedure TTestDwarfInfoEntry.Add(AnAttrib, AForm: Cardinal; AData: array of Byte);
var
  c: Integer;
begin
  if Length(FEncoded) = 0 then InitEncoded;
  FAbbrevObj.Add(AnAttrib, AForm);
  if Length(AData) = 0 then exit;
  c := Length(FEncoded);
  SetLength(FEncoded, c + Length(AData));
  Move(AData[0], FEncoded[c], Length(AData));
end;

procedure TTestDwarfInfoEntry.Add(AnAttrib, AForm: Cardinal; AData: String);
var
  c: Integer;
begin
  if Length(FEncoded) = 0 then InitEncoded;
  FAbbrevObj.Add(AnAttrib, AForm);
  if Length(AData) = 0 then exit;
  c := Length(FEncoded);
  SetLength(FEncoded, c + Length(AData));
  Move(AData[1], FEncoded[c], Length(AData));
end;

procedure TTestDwarfInfoEntry.AddSLEB(AnAttrib, AForm: Cardinal; AData: Int64);
begin
  if Length(FEncoded) = 0 then InitEncoded;
  FAbbrevObj.Add(AnAttrib, AForm);
  WriteSLEB128(AData, FEncoded, length(FEncoded));
end;

procedure TTestDwarfInfoEntry.AddULEB(AnAttrib, AForm: Cardinal; AData: QWord);
begin
  if Length(FEncoded) = 0 then InitEncoded;
  FAbbrevObj.Add(AnAttrib, AForm);
  WriteULEB128(AData, FEncoded, length(FEncoded));
end;

procedure TTestDwarfInfoEntry.AddAddr(AnAttrib, AForm: Cardinal; AData: QWord);
var
  c: Integer;
begin
  if Length(FEncoded) = 0 then InitEncoded;
  FAbbrevObj.Add(AnAttrib, AForm);
  if FSection.FAddrSize = 4 then begin
    c := Length(FEncoded);
    SetLength(FEncoded, c + 4);
    PCardinal(@FEncoded[c])^ := AData;
  end else begin
    c := Length(FEncoded);
    SetLength(FEncoded, c + 8);
    PQWord(@FEncoded[c])^ := AData;
  end;
end;

procedure TTestDwarfInfoEntry.Add(AnAttrib, AForm: Cardinal; AData: QWord);
begin
  AddULEB(AnAttrib, AForm, AData);
end;

function TTestDwarfInfoEntry.AddRef(AnAttrib, AForm: Cardinal;
  AData: TTestDwarfInfoEntry): Integer;
var
  c: Integer;
  l: Integer;
begin
  if Length(FEncoded) = 0 then InitEncoded;
  FAbbrevObj.Add(AnAttrib, AForm);

  Result := length(FRefList);
  SetLength(FRefList, Result + 1);

  l := TestAddrSize;
  case AForm of
    DW_FORM_ref1: l := 1;
    DW_FORM_ref2: l := 2;
    DW_FORM_ref4: l := 4;
    DW_FORM_ref8: l := 8;
    DW_FORM_ref_addr: l := FSection.AddrSize;
    //DW_FORM_ref_udata: l := 1;
    else Assert(false);
  end;

  FRefList[Result].AData := AData;
  FRefList[Result].FSize := l;
  FRefList[Result].Index := length(FEncoded);

  c := Length(FEncoded);
  SetLength(FEncoded, c + l);
  case l of
    1:  PByte(@FEncoded[c])^ := 0;
    2:  PWord(@FEncoded[c])^ := 0;
    4:  PCardinal(@FEncoded[c])^ := 0;
    8:  PQWord(@FEncoded[c])^ := 0;
  end;
end;

function TTestDwarfInfoEntry.AddRef(AnAttrib, AForm: Cardinal;
  AData: PTestDwarfInfoEntry): Integer;
var
  c: Integer;
  l: Integer;
begin
  if Length(FEncoded) = 0 then InitEncoded;
  FAbbrevObj.Add(AnAttrib, AForm);

  Result := length(FRefList);
  SetLength(FRefList, Result + 1);

  l := TestAddrSize;
  case AForm of
    DW_FORM_ref1: l := 1;
    DW_FORM_ref2: l := 2;
    DW_FORM_ref4: l := 4;
    DW_FORM_ref8: l := 8;
    DW_FORM_ref_addr: l := FSection.AddrSize;
    //DW_FORM_ref_udata: l := 1;
    else Assert(false);
  end;

  FRefList[Result].ADataRef := AData;
  FRefList[Result].FSize := l;
  FRefList[Result].Index := length(FEncoded);

  c := Length(FEncoded);
  SetLength(FEncoded, c + l);
  case l of
    1:  PByte(@FEncoded[c])^ := 0;
    2:  PWord(@FEncoded[c])^ := 0;
    4:  PCardinal(@FEncoded[c])^ := 0;
    8:  PQWord(@FEncoded[c])^ := 0;
  end;
end;

procedure TTestDwarfInfoEntry.SetRef(AIndex: Integer; AData: TTestDwarfInfoEntry);
begin
  FRefList[AIndex].AData := AData;
end;

function TTestDwarfInfoEntry.GetNewChild: TTestDwarfInfoEntry;
begin
  Result := FSection.CreateInfoEntryObj;
  FChildren.Add(Result);
end;

function TTestDwarfInfoEntry.DataLength: Integer;
begin
  if Length(FEncoded) = 0 then InitEncoded;
  Result := Length(FEncoded);
  if Children <> 0 then Result := Result + 1;
end;

function TTestDwarfInfoEntry.Data: Pointer;
begin
  if Length(FEncoded) = 0 then InitEncoded;
  Result := @FEncoded;
end;

{ TTestDummySectionInfoEntries }

function TTestDummySectionInfoEntries.CreateInfoEntryObj: TTestDwarfInfoEntry;
begin
  Result := TTestDwarfInfoEntry.Create;
  Result.FSection := Self;
  assert(AbbrevSection <> nil);
  Result.FAbbrevObj := AbbrevSection.GetNewAbbrevObj;
end;

constructor TTestDummySectionInfoEntries.Create;
begin
  FVersion := 2;
  FAddrSize := TestAddrSize;
end;

destructor TTestDummySectionInfoEntries.Destroy;
begin
  FreeAndNil(FFirstEntry);
  if Section.RawData <> nil then
    Freemem(Section.RawData);
  inherited Destroy;
end;

function TTestDummySectionInfoEntries.GetFirstInfoEntryObj: TTestDwarfInfoEntry;
begin
  if FFirstEntry= nil then
    FFirstEntry := CreateInfoEntryObj;
  Result := FFirstEntry;
end;

procedure TTestDummySectionInfoEntries.CreateSectionData;
var
  l: Integer;
begin
  l := FFirstEntry.DataLengthIncl + 11;  // 32 bit 4,2,4,1

  Section.Size := l;
  Section.RawData := AllocMem(l);

  PCardinal(Section.RawData)^ := l - 4;
  PWord(Section.RawData+4)^ := FVersion;
  PCardinal(Section.RawData+6)^ := 0;
  PByte(Section.RawData+10)^ := FAddrSize;

  FFirstEntry.WriteToSection(Section.RawData, 11);
end;

{ TTestDwarfAbbrev }

procedure TTestDwarfAbbrev.Encode;
var
  i: Integer;
begin
  if length(FEncoded) > 0 then
    exit;
  WriteULEB128(FId, FEncoded, 0);
  WriteULEB128(FTag, FEncoded, length(FEncoded));
  WriteULEB128(FChildren, FEncoded, length(FEncoded)); // 0 or 1 / 1 byte
  for i := 0 to Length(FData)-1 do
    WriteULEB128(FData[i], FEncoded, length(FEncoded));
  WriteULEB128(0, FEncoded, length(FEncoded));
  WriteULEB128(0, FEncoded, length(FEncoded));
end;

procedure TTestDwarfAbbrev.Add(ATag, AForm: Cardinal);
var
  c: Integer;
begin
  c := Length(FData);
  SetLength(FData, c + 2);
  FData[c] := ATag;
  FData[c+1] := AForm;
end;

function TTestDwarfAbbrev.DataLength: Integer;
begin
  Encode;
  Result := Length(FEncoded);
end;

function TTestDwarfAbbrev.Data: Pointer;
begin
  Encode;
  Result := @FEncoded[0];
end;


{ TTestDummySection }

procedure TTestDummySection.CreateSectionData;
begin
  //
end;

{ TTestDummyFileSource }

function TTestDummyFileSource.GetTestSection(const AName: String): TTestDummySection;
var
  i: Integer;
  t: TTestDummySectionInfoEntries;
begin
  Result := nil;
  i := FSections.IndexOf(AName);
  if i < 0 then begin
    if AName = '.debug_abbrev' then
      i := FSections.AddObject(AName, TTestDummySectionAbbrevs.Create)
    else
    if AName = '.debug_info' then begin
      t := TTestDummySectionInfoEntries.Create;
      t.AbbrevSection := GetTestSection('.debug_abbrev') as TTestDummySectionAbbrevs;
      i := FSections.AddObject(AName, t);
    end
    else
      i := FSections.AddObject(AName, TTestDummySection.Create);
  end;
  Result := TTestDummySection(FSections.Objects[i]);
end;

function TTestDummyFileSource.GetSection(const AName: String): PDbgImageSection;
var
  i: Integer;
  tmp: TTestDummySection;
begin
  Result := nil;
  i := FSections.IndexOf(AName);
  if i < 0 then
    exit;
  tmp := TTestDummySection(FSections.Objects[i]);
  Result := @tmp.Section;
end;

class function TTestDummyFileSource.isValid(ASource: TDbgFileLoader): Boolean;
begin
  Result := True;
end;

class function TTestDummyFileSource.UserName: AnsiString;
begin
  Result := 'Test Source';
end;

constructor TTestDummyFileSource.Create;
begin
  inherited Create(nil, False);
  FSections := TStringList.Create;
end;

destructor TTestDummyFileSource.Destroy;
var
  i: Integer;
begin
  for i := 0 to FSections.Count - 1 do
    FSections.Objects[i].Free;
  FreeAndNil(FSections);
  inherited Destroy;
end;

{ TTestDummyImageLoader }

constructor TTestDummyImageLoader.Create;
begin
  FImgReader := TTestDummyFileSource.Create;
  ImgReader := FImgReader; // vill be destroyed by base
  inherited Create;
end;

{ TTestDummySectionAbbrevs }

function TTestDummySectionAbbrevs.GetNextID: Cardinal;
begin
  Result := FCurrentID;
  inc(FCurrentID);
end;

constructor TTestDummySectionAbbrevs.Create;
begin
  FList := TList.Create;
  Section.Size := 0;
  Section.RawData := nil;
  FCurrentID := 1;;
end;

destructor TTestDummySectionAbbrevs.Destroy;
begin
  while FList.Count > 0 do begin
    TObject(FList[0]).Free;
    FList.Delete(0);
  end;
  FreeAndNil(FList);
  if Section.RawData <> nil then
    Freemem(Section.RawData);
  inherited Destroy;
end;

function TTestDummySectionAbbrevs.GetNewAbbrevObj: TTestDwarfAbbrev;
begin
  Result := TTestDwarfAbbrev.Create;
  Result.FSection := Self;
  Result.Id := GetNextID;
  FList.Add(Result);
end;

procedure TTestDummySectionAbbrevs.CreateSectionData;
var
  i, j, l: Integer;
begin
  l := 1;  // one for zero at end
  for i := 0 to FList.Count - 1 do
    l := l + TTestDwarfAbbrev(FList[i]).DataLength;

  Section.Size := l;
  Section.RawData := AllocMem(l);

  j := 0;
  for i := 0 to FList.Count - 1 do begin
    l := TTestDwarfAbbrev(FList[i]).DataLength;
    move(TTestDwarfAbbrev(FList[i]).Data^, (Section.RawData+j)^, l);
    j := j + l;
  end;

  PByte(Section.RawData+j)^ := 0;
  assert(j < Section.Size);
end;

end.

