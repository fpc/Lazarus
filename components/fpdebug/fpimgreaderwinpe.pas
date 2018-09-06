{
 This unit contains the types needed for reading PE images.

 This file was ported from DUBY. See svn log for details

 ---------------------------------------------------------------------------

 @created(Thu May 4th WET 2006)
 @lastmod($Date: 2009-01-16 03:26:10 +0300 (Пт, 16 янв 2009) $)
 @author(Marc Weustink <marc@@dommelstein.nl>)

 @modified by dmitry boyarintsev (july 2009:
   + removed Windows unit dependancy. added SectionCount and
   + added Sections access by Index

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit FpImgReaderWinPE;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, FpImgReaderBase, FpImgReaderWinPETypes, LazLoggerBase,
  fpDbgSymTable, DbgIntfBaseTypes;
  
type

  { TPEFileSource }

  TPEFileSource = class(TDbgImageReader)
  private
    FSections: TStringList;
    FFileLoader : TDbgFileLoader;
    FOwnLoader  : Boolean;
    FCodeBase   : DWord;
  protected
    function GetSection(const AName: String): PDbgImageSection; override;

    procedure LoadSections;
  public
    class function isValid(ASource: TDbgFileLoader): Boolean; override;
    class function UserName: AnsiString; override;
  public
    constructor Create(ASource: TDbgFileLoader; ADebugMap: TObject; OwnSource: Boolean); override;
    destructor Destroy; override;
    procedure ParseSymbolTable(AfpSymbolInfo: TfpSymbolList); override;
  end;

implementation

const
  // Symbol-map section name
  _symbol        = '.symbols';
  _symbolstrings = '.symbolsstrings';

type
  PImageSymbolArray = ^TImageSymbolArray;
  TImageSymbolArray = array[0..maxSmallint] of TImageSymbol;

function isValidPEStream(ASource: TDbgFileLoader): Boolean;
var
  DosHeader: TImageDosHeader;
begin
  try
    Result := false;
    if ASource.Read(0, sizeof(DosHeader), @DosHeader) <> sizeof(DosHeader) then
      Exit;
    if (DosHeader.e_magic <> IMAGE_DOS_SIGNATURE) or (DosHeader.e_lfanew = 0) then 
      Exit;
    Result := true;
  except
    Result := false;
  end;
end;

constructor TPEFileSource.Create(ASource: TDbgFileLoader; ADebugMap: TObject; OwnSource: Boolean);
begin
  FSections := TStringList.Create;
  FSections.Sorted := True;
  //FSections.Duplicates := dupError;
  FSections.CaseSensitive := False;

  FFileLoader:=ASource;
  FOwnLoader:=OwnSource;
  LoadSections;
  inherited Create(ASource, ADebugMap, OwnSource);
end;

destructor TPEFileSource.Destroy;  
begin
  if FOwnLoader then FFileLoader.Free;
  while FSections.Count > 0 do begin
    Freemem(FSections.Objects[0]);
    FSections.Delete(0);
  end;
  FSections.Free;
  inherited Destroy;  
end;

procedure TPEFileSource.ParseSymbolTable(AfpSymbolInfo: TfpSymbolList);
var
  p: PDbgImageSection;
  ps: PDbgImageSection;
  SymbolArr: PImageSymbolArray;
  SymbolStr: pointer;
  i,j: integer;
  SymbolCount: integer;
  SymbolName: AnsiString;
begin
  p := Section[_symbol];
  ps := Section[_symbolstrings];
  if assigned(p) and assigned(ps) then
  begin
    SymbolArr:=PDbgImageSectionEx(p)^.Sect.RawData;
    SymbolStr:=PDbgImageSectionEx(ps)^.Sect.RawData;
    SymbolCount := PDbgImageSectionEx(p)^.Sect.Size div sizeof(TImageSymbol);
    {$PUSH}{$R-} // SymbolArr may be more than maxSmallInt
    for i := 0 to SymbolCount-1 do
    begin
      begin
        // Section-index is ignored for now...
        if SymbolArr^[i].N.Name.Short=0 then
          SymbolName:=pchar(SymbolStr+SymbolArr^[i].N.Name.Long)
        else
        begin
          SymbolName:='';
          if SymbolArr^[i].N.Name.Long<>0 then
          begin
            for j := 0 to sizeof(SymbolArr^[i].N.ShortName)-1 do
            begin
              if SymbolArr^[i].N.ShortName[j]=#0 then break;
              SymbolName:=SymbolName+SymbolArr^[i].N.ShortName[j];
            end;
          end;
        end;
        AfpSymbolInfo.Add(SymbolName, TDBGPtr(SymbolArr^[i].Value+ImageBase+FCodeBase));
      end
    end;
    {$POP}
  end;
end;

function TPEFileSource.GetSection(const AName: String): PDbgImageSection;
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
  FFileLoader.LoadMemory(ex^.Offs, Result^.Size, Result^.RawData);
end;

procedure TPEFileSource.LoadSections;

  procedure Add(const AName: String; ARawData: QWord; ASize: QWord; AVirtualAdress: QWord);
  var
    p: PDbgImageSectionEx;
    idx: integer;
  begin
    idx := FSections.AddObject(AName, nil);
    New(p);
    P^.Offs := ARawData;
    p^.Sect.Size := ASize;
    p^.Sect.VirtualAddress := AVirtualAdress;
    p^.Loaded := False;
    FSections.Objects[idx] := TObject(p);
  end;

var
  DosHeader: TImageDosHeader;
  NtHeaders: record
    case integer of
      1: (Sys: TImageNtHeaders;);
      2: (W32: TImageNtHeaders32;);
      3: (W64: TImageNtHeaders64;);
    end;
  SectionHeader: PImageSectionHeader;
  n, i: Integer;
  SectionName: array[0..IMAGE_SIZEOF_SHORT_NAME] of Char;
  SectionMax: QWord;
  s: string[255];
  StringTableLen: DWord;
  StringTableStart: QWord;
begin
  FFileLoader.Read(0, sizeof(DosHeader), @DosHeader);
  if (DosHeader.e_magic <> IMAGE_DOS_SIGNATURE)
  or (DosHeader.e_lfanew = 0)
  then begin
    //WriteLn('Invalid DOS header');
    Exit;
  end;

  FFileLoader.Read(DosHeader.e_lfanew, sizeof(NtHeaders), @NtHeaders);
  if NtHeaders.Sys.Signature <> IMAGE_NT_SIGNATURE
  then begin
    //WriteLn('Invalid NT header: ', IntToHex(NtHeaders^.Signature, 8));
    Exit;
  end;

  SetImage64Bit(NtHeaders.Sys.OptionalHeader.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC);

  if Image64Bit
  then SetImageBase(NtHeaders.W64.OptionalHeader.ImageBase)
  else SetImageBase(NtHeaders.W32.OptionalHeader.ImageBase);
  FCodeBase := NtHeaders.W32.OptionalHeader.BaseOfCode;
  SectionMax := FFileLoader.LoadMemory(
    DosHeader.e_lfanew +
    (@NtHeaders.Sys.OptionalHeader - @NtHeaders.Sys) +
    NtHeaders.Sys.FileHeader.SizeOfOptionalHeader,
    SizeOf(TImageSectionHeader) * NtHeaders.Sys.FileHeader.NumberOfSections,
    SectionHeader
    )
    div SizeOf(TImageSectionHeader);
  if SectionMax <> NtHeaders.Sys.FileHeader.NumberOfSections then begin
    DebugLn(['Could not load all headers', NtHeaders.Sys.FileHeader.NumberOfSections, ' ', SectionMax]);
  end;

  for n := 0 to SectionMax - 1 do
  begin
    //SectionHeader := Pointer(@NtHeaders.Sys.OptionalHeader) +
    //                         NtHeaders.Sys.FileHeader.SizeOfOptionalHeader +
    //                         SizeOf(TImageSectionHeader) * n;
    // make a null terminated name
    Move(SectionHeader[n].Name, SectionName, IMAGE_SIZEOF_SHORT_NAME);
    SectionName[IMAGE_SIZEOF_SHORT_NAME] := #0;
    if (SectionName[0] = '/') and (SectionName[1] in ['0'..'9'])
    then begin
      // long name
      i := FFileLoader.Read(
        NTHeaders.Sys.FileHeader.PointerToSymbolTable +
        NTHeaders.Sys.FileHeader.NumberOfSymbols * IMAGE_SIZEOF_SYMBOL +
        StrToIntDef(PChar(@SectionName[1]), 0), 255, @s[1]);
      s[Min(i, 255)] := #0;
      Add(pchar(@s[1]), SectionHeader[n].PointerToRawData, SectionHeader[n].Misc.VirtualSize,  SectionHeader[n].VirtualAddress);
    end
    else begin
      // short name
      Add(SectionName, SectionHeader[n].PointerToRawData, SectionHeader[n].Misc.VirtualSize,  SectionHeader[n].VirtualAddress);
    end
  end;

  // Create a fake-sections for the symbol-table:
  if NtHeaders.Sys.FileHeader.PointerToSymbolTable<>0 then
    begin
    Add(_symbol,NtHeaders.Sys.FileHeader.PointerToSymbolTable, NtHeaders.Sys.FileHeader.NumberOfSymbols*IMAGE_SIZEOF_SYMBOL,0);
    StringTableStart:=NtHeaders.Sys.FileHeader.PointerToSymbolTable+NtHeaders.Sys.FileHeader.NumberOfSymbols*IMAGE_SIZEOF_SYMBOL;
    FFileLoader.Read(StringTableStart, sizeof(DWord), @StringTableLen);
    Add(_symbolstrings,StringTableStart, StringTableLen, 0);
    end;

  FFileLoader.UnloadMemory(SectionHeader);
end;

class function TPEFileSource.isValid(ASource: TDbgFileLoader): Boolean;
begin
  Result := isValidPEStream(ASource);
end;

class function TPEFileSource.UserName: AnsiString;
begin
  Result:='PE file';
end;


initialization
  RegisterImageReaderClass(TPEFileSource);

end.

