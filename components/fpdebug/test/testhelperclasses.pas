unit TestHelperClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpImgReaderBase, FpDbgDwarfConst, FpDbgLoader;

type

  TTestDwarfAbbrev = class;
  TTestDwarfInfoEntry = class;

  { TTestDummySection }

  TTestDummySection = class
  public
    Section: TDbgImageSection;
    procedure CreateSectionData; virtual;
  end;

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

  (*
  TDwarfCUHeader32 = record
    Length: LongWord;
    Version: Word;
    AbbrevOffset: LongWord;
    AddressSize: Byte;
  end;

  TDwarfCUHeader64 = record
    Signature: LongWord;
    Length: QWord;
    Version: Word;
    AbbrevOffset: QWord;
    AddressSize: Byte;
  end;
  *)

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
    constructor Create; virtual;
    property TestImgReader: TTestDummyFileSource read FImgReader;
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

  TTestDwarfInfoEntry = class
  private
    FAbbrevObj: TTestDwarfAbbrev;
    FSection: TTestDummySectionInfoEntries;
    FChildren: TList;
    FEncoded: Array of Byte;
    FRefList: array of record
        Index, FSize: Integer;
        AData: TTestDwarfInfoEntry;
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

    procedure SetRef(AIndex: Integer; AData: TTestDwarfInfoEntry);

    function GetNewChild: TTestDwarfInfoEntry;
    function DataLength: Integer; // Exclude Children
    function Data: Pointer;
  end;

implementation

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
begin
  if ANum = 0 then begin
    AddByte(0);
    exit;
  end;

  if ANum < 0 then begin;
    n := 10*7;
    while n > 0 do begin
      if (ANum and ($7f shl n) = (Int64(-1) and ($7f shl n)) ) and
         (ANum and (int64(1) shl (n-1)) <> 0)
      then
        ANum := ANum and not(-1 shl n)
      else
        break;
      dec(n, 7);
    end;

    while ANum <> 0 do begin
      if ANum > $7f then
        AddByte((ANum and $7f) + $80)
      else
        AddByte((ANum and $7f));
      ANum := ANum shr 7;
    end;

  end
  else begin

    c := False;
    while (ANum <> 0) or c do begin
      if ANum > $7f then
        AddByte((ANum and $7f) + $80)
      else
        AddByte((ANum and $7f));
      c := (ANum and $40) <> 0; // write extra 0, to prlevent sign extend
      ANum := ANum shr 7;
    end;

  end;
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
begin
  for i := 0 to Length(FRefList) - 1 do begin
    v := FRefList[i].AData.FWrittenAtIndex;
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

  l := 4;
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
  FAddrSize := 4;
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

