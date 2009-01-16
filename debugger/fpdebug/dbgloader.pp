{ $Id$ }
{
 ---------------------------------------------------------------------------
 dbgloader.pp  -  Native Freepascal debugger - Section loader
 ---------------------------------------------------------------------------

 This unit contains helper classes for loading secions form images.

 ---------------------------------------------------------------------------

 @created(Mon Aug 1st WET 2006)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.nl>)

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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit DbgLoader;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}
  Windows,
  {$endif}
  Classes, SysUtils, DbgPETypes;

type
  TDbgImageSection = record
    RawData: Pointer;
    Size: QWord;
    VirtualAdress: QWord;
  end;
  PDbgImageSection = ^TDbgImageSection;


  { TDbgImageLoader }

  TDbgImageLoader = class(TObject)
  private
    FImage64Bit: Boolean;
    FImageBase: QWord;
    FSections: TStringList;
    function GetSection(const AName: String): PDbgImageSection;
  protected
    procedure Add(const AName: String; ARawData: Pointer; ASize: QWord; AVirtualAdress: QWord);
    procedure SetImageBase(ABase: QWord);
    procedure SetImage64Bit(AValue: Boolean);
    procedure LoadSections; virtual; abstract;
    procedure UnloadSections; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    property ImageBase: QWord read FImageBase;
    Property Image64Bit: Boolean read FImage64Bit;
    property Section[const AName: String]: PDbgImageSection read GetSection;
  end;
  
  { TDbgPEImageLoader }

  TDbgPEImageLoader = class(TDbgImageLoader)
  private
  protected
    function  LoadData(out AModuleBase: Pointer; out AHeaders: PImageNtHeaders): Boolean; virtual; abstract;
    procedure LoadSections; override;
    procedure UnloadData; virtual; abstract;
    procedure UnloadSections; override;
  public
  end;
  
  { TDbgWinPEImageLoader }

  TDbgWinPEImageLoader = class(TDbgPEImageLoader)
  private
    FFileHandle: THandle;
    FMapHandle: THandle;
    FModulePtr: Pointer;
    procedure DoCleanup;
  protected
    function  LoadData(out AModuleBase: Pointer; out AHeaders: PImageNtHeaders): Boolean; override;
    procedure UnloadData; override;
  public
    constructor Create(const AFileName: String);
    constructor Create(AFileHandle: THandle);
  end;

implementation

{ TDbgImageLoader }

procedure TDbgImageLoader.Add(const AName: String; ARawData: Pointer; ASize: QWord; AVirtualAdress: QWord);
var
  p: PDbgImageSection;
  idx: integer;
begin
  idx := FSections.AddObject(AName, nil);
  New(p);
  P^.RawData := ARawData;
  p^.Size := ASize;
  p^.VirtualAdress := AVirtualAdress;
  FSections.Objects[idx] := TObject(p);
end;

constructor TDbgImageLoader.Create;
begin
  inherited Create;
  FSections := TStringList.Create;
  FSections.Sorted := True;
  FSections.Duplicates := dupError;
  FSections.CaseSensitive := False;
  LoadSections;
end;

destructor TDbgImageLoader.Destroy;
var
  n: integer;
begin
  UnloadSections;
  for n := 0 to FSections.Count - 1 do
    Dispose(PDbgImageSection(FSections.Objects[n]));
  FSections.Clear;
  inherited Destroy;
  FreeAndNil(FSections);
end;

function TDbgImageLoader.GetSection(const AName: String): PDbgImageSection;
var
  idx: integer;
begin
  idx := FSections.IndexOf(AName);
  if idx = -1
  then Result := nil
  else Result := PDbgImageSection(FSections.Objects[idx]);
end;

procedure TDbgImageLoader.SetImage64Bit(AValue: Boolean);
begin
  FImage64Bit := AValue;
end;

procedure TDbgImageLoader.SetImageBase(ABase: QWord);
begin
  FImageBase := ABase;
end;

{ TDbgPEImageLoader }

procedure TDbgPEImageLoader.LoadSections;
var
  ModulePtr: Pointer;
  NtHeaders: PImageNtHeaders;
  NtHeaders32: PImageNtHeaders32 absolute NtHeaders;
  NtHeaders64: PImageNtHeaders64 absolute NtHeaders;
  SectionHeader: PImageSectionHeader;
  n, idx: Integer;
  p: Pointer;
  SectionName: array[0..IMAGE_SIZEOF_SHORT_NAME] of Char;
begin
  if not LoadData(ModulePtr, NtHeaders) then Exit;
  
  if NtHeaders^.Signature <> IMAGE_NT_SIGNATURE
  then begin
    WriteLn('Invalid NT header: ', IntToHex(NtHeaders^.Signature, 8));
    Exit;
  end;

  SetImage64Bit(NtHeaders^.OptionalHeader.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC);

  if Image64Bit
  then SetImageBase(NtHeaders64^.OptionalHeader.ImageBase)
  else SetImageBase(NtHeaders32^.OptionalHeader.ImageBase);

  for n := 0 to NtHeaders^.FileHeader.NumberOfSections - 1 do
  begin
    SectionHeader := Pointer(@NtHeaders^.OptionalHeader) + NtHeaders^.FileHeader.SizeOfOptionalHeader + SizeOf(SectionHeader^) * n;
    // make a null terminated name
    Move(SectionHeader^.Name, SectionName, IMAGE_SIZEOF_SHORT_NAME);
    SectionName[IMAGE_SIZEOF_SHORT_NAME] := #0;
    if (SectionName[0] = '/') and (SectionName[1] in ['0'..'9'])
    then begin
      // long name
      p := ModulePtr + NTHeaders^.FileHeader.PointerToSymbolTable + NTHeaders^.FileHeader.NumberOfSymbols * IMAGE_SIZEOF_SYMBOL + StrToIntDef(PChar(@SectionName[1]), 0);
      Add(PChar(p), ModulePtr + SectionHeader^.PointerToRawData, SectionHeader^.Misc.VirtualSize,  SectionHeader^.VirtualAddress);
    end
    else begin
      // short name
      Add(SectionName, ModulePtr + SectionHeader^.PointerToRawData, SectionHeader^.Misc.VirtualSize,  SectionHeader^.VirtualAddress);
    end
  end;
end;

procedure TDbgPEImageLoader.UnloadSections;
begin
  UnloadData;
end;

{ TDbgWinPEImageLoader }

constructor TDbgWinPEImageLoader.Create(const AFileName: String);
begin
  FFileHandle := CreateFile(PChar(AFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if FFileHandle = INVALID_HANDLE_VALUE
  then begin
    WriteLN('Cannot open file: ', AFileName);
  end;
  inherited Create;
end;

constructor TDbgWinPEImageLoader.Create(AFileHandle: THandle);
begin
  FFileHandle := AFileHandle;
  if FFileHandle = INVALID_HANDLE_VALUE
  then begin
    WriteLN('Invalid file handle');
  end;
  inherited Create;
end;

procedure TDbgWinPEImageLoader.DoCleanup;
begin
  if FModulePtr <> nil
  then UnmapViewOfFile(FModulePtr);
  if FMapHandle <> 0
  then CloseHandle(FMapHandle);
  if FFileHandle <> INVALID_HANDLE_VALUE
  then CloseHandle(FFileHandle);

  FFileHandle := INVALID_HANDLE_VALUE;
  FMapHandle := 0;
  FModulePtr := nil;
end;

function TDbgWinPEImageLoader.LoadData(out AModuleBase: Pointer; out AHeaders: PImageNtHeaders): Boolean;
var
  DosHeader: PImageDosHeader;
begin
  Result := False;

  try
    FMapHandle := CreateFileMapping(FFileHandle, nil, PAGE_READONLY{ or SEC_IMAGE}, 0, 0, nil);
    if FMapHandle = 0
    then begin
      WriteLn('Could not create module mapping');
      Exit;
    end;

    FModulePtr := MapViewOfFile(FMapHandle, FILE_MAP_READ, 0, 0, 0);
    if FModulePtr = nil
    then begin
      WriteLn('Could not map view');
      Exit;
    end;

    DosHeader := FModulePtr;
    if (DosHeader^.e_magic <> IMAGE_DOS_SIGNATURE)
    or (DosHeader^.e_lfanew = 0)
    then begin
      WriteLn('Invalid DOS header');
      Exit;
    end;
    
    AModuleBase := FModulePtr;
    AHeaders := FModulePtr + DosHeader^.e_lfanew;
    Result := True;
  finally
    if not Result
    then begin
      // something failed, do some cleanup
      DoCleanup;
    end;
  end;
end;

procedure TDbgWinPEImageLoader.UnloadData;
begin
  DoCleanup;
end;

end.

