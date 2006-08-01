{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpdumpdwarf  -  DWARF debug dump
 ---------------------------------------------------------------------------

 Utility to test and dump DWARF dubug info.

 ---------------------------------------------------------------------------

 @created(Sat Jul 1th WET 2006)
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
program FPDumpDwarf;

{$mode objfpc}{$H+}

uses
  Classes, Windows, SysUtils, WinDDwarf, WinDPETypes, WinDDwarfConst,
  WinDSymbols, WinDLoader;

var
  ModulePtr: Pointer;
  Is64: Boolean;
  Sections: TStringList;
  hMap, hFile: THandle;
  DosHeader: PImageDosHeader;
  NtHeaders: PImageNtHeaders;
  SectionHeader, InfoSH, AbbrevSH: PImageSectionHeader;
  n, idx: Integer;
  SectionName: array[0..IMAGE_SIZEOF_SHORT_NAME] of Char;
  AbbrevPtr, AbbrevPtrMax, AbbrevBase: Pointer;
  InfoPtr, InfoPtrMax: Pointer;
  InfoOffset: Int64;
  Abb: Cardinal;
  CUHeader, NextCUHeader: PDwarfCUHeader32;
  p: Pointer;

  Dwarf: TDbgDwarf;
  AbbrevDecoder: TDwarfAbbrevDecoder;

  Loader: TDbgImageLoader;

begin
  if ParamCount < 1
  then begin
    WriteLN('Usage: FPDumpDwarf <filename>');
    Exit;
  end;
  
  Loader := TDbgWinPEImageLoader.Create(ParamStr(1));
  
(*
  hFile := CreateFile(PChar(ParamStr(1)), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if hFile = INVALID_HANDLE_VALUE
  then begin
    WriteLN('Cannot open file: ', ParamStr(1));
    Exit;
  end;
  
  try
    hMap := 0;
    ModulePtr := nil;
    Sections := nil;
    try
      hMap := CreateFileMapping(hFile, nil, PAGE_READONLY{ or SEC_IMAGE}, 0, 0, nil);
      if hMap = 0
      then begin
        WriteLn('Could not create module mapping');
        Exit;
      end;

      ModulePtr := MapViewOfFile(hMap, FILE_MAP_READ, 0, 0, 0);
      if ModulePtr = nil
      then begin
        WriteLn('Could not map view');
        Exit;
      end;

      DosHeader := ModulePtr;
      if (DosHeader^.e_magic <> IMAGE_DOS_SIGNATURE)
      or (DosHeader^.e_lfanew = 0)
      then begin
        WriteLn('Invalid DOS header');
        Exit;
      end;

      NTHeaders := ModulePtr + DosHeader^.e_lfanew;

      if NTHeaders^.Signature <> IMAGE_NT_SIGNATURE
      then begin
        WriteLn('Invalid NT header: ', IntToHex(NTHeaders^.Signature, 8));
        Exit;
      end;

      Is64 := NTHeaders^.OptionalHeader.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC;

      Sections := TStringList.Create;
      Sections.CaseSensitive := False;
      Sections.Duplicates := dupIgnore;
      Sections.Sorted := True;
      for n := 0 to NtHeaders^.FileHeader.NumberOfSections - 1 do
      begin
        SectionHeader := @NTHeaders^.OptionalHeader + NTHeaders^.FileHeader.SizeOfOptionalHeader + SizeOf(SectionHeader^) * n;
        // make a null terminated name
        Move(SectionHeader^.Name, SectionName, IMAGE_SIZEOF_SHORT_NAME);
        SectionName[IMAGE_SIZEOF_SHORT_NAME] := #0;
        if (SectionName[0] = '/') and (SectionName[1] in ['0'..'9'])
        then begin
          // long name
          p := ModulePtr + NTHeaders^.FileHeader.PointerToSymbolTable + NTHeaders^.FileHeader.NumberOfSymbols * IMAGE_SIZEOF_SYMBOL + StrToIntDef(PChar(@SectionName[1]), 0);
          Sections.AddObject(PChar(p), TObject(SectionHeader));
        end
        else begin
          // short name
          Sections.AddObject(SectionName, TObject(SectionHeader));
        end
      end;
      
      WriteLN('Sections:');
      for n := 0 to Sections.Count - 1 do
        WriteLn('  ', Sections[n], #9'@$', Format('%p', [Pointer(Sections.Objects[n])]));

      idx := Sections.IndexOf('.debug_info');
      if idx = -1
      then begin
        WriteLn('.debug_info section not found. Nothing to do.');
        Exit;
      end;

      InfoSH := Pointer(Sections.Objects[idx]);
      InfoPtr := ModulePtr + InfoSH^.PointerToRawData;
      InfoPtrMax := InfoPtr + InfoSH^.Misc.VirtualSize - 1;
      InfoOffset := PtrUInt(InfoPtr) - NTHeaders^.OptionalHeader.ImageBase - InfoSH^.VirtualAddress;

      idx := Sections.IndexOf('.debug_abbrev');
      if idx = -1
      then begin
        WriteLn('.debug_abbrev section not found. Nothing to do.');
        Exit;
      end;
      AbbrevSH := Pointer(Sections.Objects[idx]);
      AbbrevPtr := ModulePtr + AbbrevSH^.PointerToRawData;
      AbbrevPtrMax := AbbrevPtr + AbbrevSH^.Misc.VirtualSize - 1;
      AbbrevBase := AbbrevPtr - NTHeaders^.OptionalHeader.ImageBase - AbbrevSH^.VirtualAddress;
*)
      Dwarf := TDbgDwarf.Create(Loader);
      n := Dwarf.LoadCompilationUnits;
      for idx := 0 to n - 1 do
      begin
        Dwarf.CompilationUnits[idx].LoadAbbrevs;
        AbbrevDecoder := TDwarfAbbrevDecoder.Create(Dwarf.CompilationUnits[idx]);
        AbbrevDecoder.Decode;
      end;

(*
      CUHeader := InfoPtr;
      while (CUHeader <> nil) and (CUHeader^.Length > 0) and (CUHeader <= InfoPtrMax) do
      begin
        WriteLN('Compilation unit:');
        WriteLn(' length: ', CUHeader^.Length);
        WriteLn(' version: ', CUHeader^.Version);
        WriteLn(' abbrev offset: ', IntToHex(CUHeader^.AbbrevOffset, 8));
        WriteLn(' address size: ', CUHeader^.AddressSize);
        
        NextCUHeader := @CUHeader^.Version + CUHeader^.Length;

        p := AbbrevBase + CUHeader^.AbbrevOffset;
        if (p < AbbrevPtr) or (p > AbbrevPtrMax)
        then begin
          WriteLN('Warning: Abbrev offset not in .debug_abbrev section');
        end;

        if NextCUHeader < InfoPtrMax
        then Abbrev := TDbgDwarf.Create(InfoOffset, p, AbbrevBase + NextCUHeader^.AbbrevOffset)
        else Abbrev := TDbgDwarf.Create(InfoOffset, p, AbbrevPtrMax);
        
        p := CUHeader + 1;
        Abbrev.Decode(p, Pointer(NextCUHeader) - 1);

        FreeAndNil(Abbrev);
        
        CUHeader := NextCUHeader;
      end;
*)
      Dwarf.Free;
      Loader.Free;
(*
    finally
      UnmapViewOfFile(ModulePtr);
      CloseHandle(hMap);
      Sections.Free;
    end;
  finally
    CloseHandle(hFile);
  end;
*)
end.

