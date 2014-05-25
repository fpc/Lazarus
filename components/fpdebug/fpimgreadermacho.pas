unit FpImgReaderMacho;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, macho, FpImgReaderMachoFile, FpImgReaderBase, LazLoggerBase,
  fpDbgSymTable;

type

  { TDbgMachoDataSource }

  TDbgMachoDataSource = class(TDbgImageReader)
  private
    fSource     : TDbgFileLoader;
    FSections: TStringList;
    fOwnSource  : Boolean;
    fFile       : TMachoFile;
    hasSymTable : Boolean;
    StabsCmd    : symtab_command;
    fileRead    : Boolean;
    function GetSectionInfo(const SectionName: AnsiString; var Size: int64): Boolean;
    function GetSectionData(const SectionName: AnsiString; Offset, {%H-}Size: Int64; var Buf: array of byte): Int64;
  protected
    procedure ReadFile;
    function GetSymTableSectionInfo({%H-}StabStr: Boolean; var {%H-}SectionOffset, {%H-}SectionSize: Int64): Boolean;
    function GetSectionIndex(const SectionName: AnsiString): Integer;

    function GetSection(const AName: String): PDbgImageSection; override;
  public
    class function isValid(ASource: TDbgFileLoader): Boolean; override;
    class function UserName: AnsiString; override;
    procedure ParseSymbolTable(AfpSymbolInfo: TfpSymbolList); override;
  public
    constructor Create(ASource: TDbgFileLoader; OwnSource: Boolean); override;
    destructor Destroy; override;
  end;

implementation

type
  PnlistArray = ^TnlistArray;
  TnlistArray = array[0..maxSmallint] of nlist;

const
  // Symbol-map section name
  _symbol        = '.symbols';
  _symbolstrings = '.symbolsstrings';


function isValidMachoStream(ASource: TDbgFileLoader): Boolean;
var
  header  : mach_header;
begin
  try
    Result := Assigned(ASource);
    if not Result then Exit;
    Result := ASource.Read(0, sizeof(header), @header) = sizeof(header);
    if not Result then Exit;
    Result := (header.magic = MH_CIGAM) or (header.magic = MH_MAGIC) or
              (header.magic = MH_CIGAM_64) or (header.magic = MH_MAGIC_64);
  except
    Result := false;
  end;
end;

function FixMachoName(const macsectionname: String): String;
begin
  if Copy(macsectionName, 1, 2) = '__' then
    Result:= '.'+Copy(macsectionName, 3, length(macsectionName)-2)
  else
    Result := macsectionname;
end;



{ TDbgMachoDataSource }

class function TDbgMachoDataSource.isValid(ASource: TDbgFileLoader): Boolean;
begin
  Result := isValidMachoStream(ASource);
end;

class function TDbgMachoDataSource.UserName: AnsiString;
begin
  Result:='mach-o file';
end;

procedure TDbgMachoDataSource.ParseSymbolTable(AfpSymbolInfo: TfpSymbolList);
var
  p: PDbgImageSection;
  ps: PDbgImageSection;
  SymbolArr: PnlistArray;
  SymbolStr: pointer;
  i: integer;
  SymbolCount: integer;
begin
  p := Section[_symbol];
  ps := Section[_symbolstrings];
  if assigned(p) and assigned(ps) then
  begin
    SymbolArr:=PDbgImageSectionEx(p)^.Sect.RawData;
    SymbolStr:=PDbgImageSectionEx(ps)^.Sect.RawData;
    SymbolCount := PDbgImageSectionEx(p)^.Sect.Size div sizeof(nlist);
    for i := 0 to SymbolCount-1 do
    begin
      if (SymbolArr^[i].n_type and $e0)<>0 then
        // This is a stabs-entry. Ignore.
        Continue;
      if (SymbolArr^[i].n_type and $0e)=$e then
      begin
        // Section-index is ignored for now...
        AfpSymbolInfo.AddObject(pchar(SymbolStr+SymbolArr^[i].n_un.n_strx), TObject(PtrUInt(SymbolArr^[i].n_value)));
      end
    end;
  end;

end;

procedure TDbgMachoDataSource.ReadFile;
var
  i : Integer;
begin
  if Assigned(fFile) then fFile.Free;
  fFile:=TMachOFile.Create;
  fFile.LoadFromFile(fSource);
  for i := 0 to fFile.header.ncmds - 1 do begin
    hasSymTable := fFile.commands[i]^.cmd = LC_SYMTAB;
    if hasSymTable then begin
      StabsCmd := psymtab_command(fFile.commands[i])^;
      Break;
    end;
  end;
  SetImage64Bit((fFile.header.cputype and CPU_ARCH_ABI64)=CPU_ARCH_ABI64);
  fileRead := true;
end;

function TDbgMachoDataSource.GetSymTableSectionInfo(StabStr: Boolean;
  var SectionOffset, SectionSize: Int64): Boolean;
begin
  Result := hasSymTable;
  if not Result then Exit;
  if StabStr then begin
    SectionOffset := StabsCmd.stroff;
    SectionSize := StabsCmd.strsize;
  end else begin
    SectionOffset := StabsCmd.symoff;
    SectionSize := Int64(StabsCmd.nsyms * sizeof(nlist));
  end;
end;

function TDbgMachoDataSource.GetSectionIndex(const SectionName: AnsiString): Integer;
var
  i     : Integer;
  Name  : AnsiString;
begin
  //todo: hash-table
  for i := 0 to fFile.Sections.Count - 1 do begin
    with TMachoSection(fFile.sections[i]) do
      if is32
        then Name := FixMachoName(sec32.sectname)
        else Name := FixMachoName(sec64.sectname);
    if Name = SectionName then begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

function TDbgMachoDataSource.GetSection(const AName: String): PDbgImageSection;
var
  i: Integer;
  ex: PDbgImageSectionEx;
begin
  Result := nil;
  i := FSections.IndexOf(AName);
  if i < 0 then
    exit;
  ex := PDbgImageSectionEx(FSections.Objects[i]);
  Result := @ex^.Sect;
  if ex^.Loaded then
    exit;
  ex^.Loaded  := True;
  fSource.LoadMemory(ex^.Offs, Result^.Size, Result^.RawData);
end;

constructor TDbgMachoDataSource.Create(ASource: TDbgFileLoader; OwnSource: Boolean);
const
  SymbolsSectionName : array [Boolean] of AnsiString = (_symbol, _symbolstrings);
var
  p: PDbgImageSectionEx;
  fs: TMachOsection;
  i: Integer;
  Name: String;
  soffset: int64;
  ssize: int64;
begin
  fSource := ASource;
  fOwnSource := OwnSource;

  ReadFile;

  FSections := TStringList.Create;
  FSections.Sorted := True;
  //FSections.Duplicates := dupError;
  FSections.CaseSensitive := False;

  for i := 0 to fFile.sections.Count - 1 do begin
    fs := TMachoSection(fFile.sections[i]);
    New(p);

    if fs.is32 then begin
      Name := FixMachoName(fs.sec32.sectname);
      P^.Offs := fs.sec32.offset;
      p^.Sect.Size := fs.sec32.size;
    end
    else begin
      Name := FixMachoName(fs.sec64.sectname);
      P^.Offs := fs.sec64.offset;
      p^.Sect.Size := fs.sec64.size;
    end;
DebugLn(Name);

    p^.Sect.VirtualAddress := 0; // Todo?
    p^.Loaded := False;
    FSections.AddObject(Name, TObject(p));
  end;

  if GetSymTableSectionInfo(false, soffset, ssize) then begin
    new(p);
    p^.Offs:=soffset;
    p^.Sect.Size:=ssize;
    p^.Sect.VirtualAddress:=0;
    p^.Loaded:=false;
    FSections.AddObject(SymbolsSectionName[false], TObject(p));
  end;

  if GetSymTableSectionInfo(true, soffset, ssize) then begin
    new(p);
    p^.Offs:=soffset;
    p^.Sect.Size:=ssize;
    p^.Sect.VirtualAddress:=0;
    p^.Loaded:=false;
    FSections.AddObject(SymbolsSectionName[true], TObject(p));
  end;

  inherited Create(ASource, OwnSource);
end;

destructor TDbgMachoDataSource.Destroy;
begin
  if Assigned(fFile) then fFile.Free;
  if fOwnSource then fSource.Free;
  while FSections.Count > 0 do begin
    Freemem(FSections.Objects[0]);
    FSections.Delete(0);
  end;
  FreeAndNil(FSections);
  inherited Destroy;
end;

{function TDbgMachoDataSource.SectionsCount: Integer;
begin
  if not Assigned(fFile) then ReadFile;
  Result := fFile.Sections.Count;
  if isStabs then inc(Result, 2);
end;

function TDbgMachoDataSource.GetSection(Index: Integer; var Name: AnsiString; var Size: Int64): Boolean;
var
  cnt   : Integer;
  sstr  : Boolean;
const
  StabSectionName : array [Boolean] of AnsiString = (_stab, _stabstr);
begin
  if not Assigned(fFile) then ReadFile;
  cnt := fFile.Sections.Count;
  if isStabs then inc(cnt, 2);
  Result := (Index >= 0) and (Index < cnt);
  if not Result then Exit;

  if Index < fFile.Sections.Count then begin
    with TMachoSection(fFile.sections[index]) do
      if is32 then begin
        Name := FixMachoName(sec32.sectname);
        Size := sec32.size;
      end else begin
        Name := FixMachoName(sec64.sectname);
        Size := sec64.size;
      end;
  end else begin
    sstr := Index = cnt - 1;
    Name := StabSectionName[sstr];
    Result := GetStabSectionInfo(sstr, Size);
  end;
end;

function TDbgMachoDataSource.GetSectionData(index: Integer; outStream: TStream): Boolean;
var
  ofs : Int64;
  sz  : Int64;
begin
  //todo: method will be removed
  if not Assigned(outStream) then begin
    Result := false;
    Exit;
  end;
  if not Assigned(fFile) then ReadFile;
  Result := (Index >= 0) and (Index < fFile.Sections.Count);
  if not Result then Exit;

  with TMachOsection(fFile.sections[index]) do begin
    if is32 then begin
      ofs := sec32.offset;
      sz := sec32.size;
    end else begin
      ofs := sec64.offset;
      sz := sec64.size;
    end;
  end;
  if ofs > 0 then begin
    fSource.Position:=ofs;
    outStream.CopyFrom(fSource, sz);
  end;
end;}

function TDbgMachoDataSource.GetSectionInfo(const SectionName: AnsiString; var Size: int64): Boolean;
var
  idx     : integer;
  symtablestr : Boolean;
  dummy   : int64;
begin
  if not fileRead then ReadFile;

  symtablestr := (SectionName = _symbolstrings);
  if symtablestr or (SectionName = _symbol) then
    Result := GetSymTableSectionInfo(symtablestr, dummy, Size)
  else begin
    idx := GetSectionIndex(SectionName);
    Result := idx >= 0;
    if not Result then Exit;

    with TMachOsection(fFile.sections[idx]) do
      if is32
        then Size := sec32.size
        else Size := sec64.size;
  end;
end;

function TDbgMachoDataSource.GetSectionData(const SectionName: AnsiString; Offset, Size: Int64; var Buf: array of byte): Int64;
var
  idx     : Integer;
  sofs    : int64;
  ssize   : int64;
  symstr : Boolean;
  sz      : Int64;
  s       : TMachOsection;
begin
  if not fileRead then ReadFile;

  Result := 0;
  symstr := SectionName = _symbolstrings;
  if symstr or (SectionName = _symbol)  then begin
    if not GetSymTableSectionInfo(symstr, sofs, ssize) then
      Exit;
  end else begin
    idx := GetSectionIndex(SectionName);
    s := TMachOsection(fFile.sections[idx]);
    if s.is32 then begin
      ssize := s.sec32.size;
      sofs := s.sec32.offset;
    end else begin
      sofs := s.sec64.offset;
      ssize := s.sec64.size;
    end;
  end;

  sz := ssize - Offset;
  if sz < 0 then Exit;

  //fSource.Position := sofs + Offset;
  //Result := fSource.Read(Buf[0], sz);
  Result := fSource.Read(sofs + Offset, sz, @Buf[0]);
end;

initialization
  RegisterImageReaderClass( TDbgMachoDataSource );

end.

