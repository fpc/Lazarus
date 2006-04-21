{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpwdpeimage.pas  -  FP standalone windows debugger - PE Image
 ---------------------------------------------------------------------------

 This unit contains routines to access or dump the PE header of a executable
 loaded in memory.

 ---------------------------------------------------------------------------

 @created(Mon Apr 10th WET 2006)
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
unit FPWDPEImage;
{$mode objfpc}{$H+}
interface

{$IF DECLARED(TImageNtHeaders)}
{$DEFINE _headers_translated_in_rtl_}
{$ENDIF}

uses
  Windows, SysUtils, FPWDGLobal, WinDebugger;

const
  IMAGE_FILE_MACHINE_IA64 = $0200;   { Intel IPF }
  IMAGE_FILE_MACHINE_AMD64 = $8664;  { x64 }

  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;  { The application can handle addresses larger than 2 GB. }

  IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT = 13;  { Delay import table }
  IMAGE_DIRECTORY_ENTRY_COM_DECRIPTOR = 14;  { COM descriptor table }

  IMAGE_NT_OPTIONAL_HDR32_MAGIC            = $010B;
  IMAGE_NT_OPTIONAL_HDR64_MAGIC            = $020B;

  IMAGE_SUBSYSTEM_WINDOWS_CE_GUI = 8;      { Windows CE system }
  IMAGE_SUBSYSTEM_XBOX = 9;                { Xbox system }


  IMAGE_LIBRARY_PROCESS_INIT                     = $0001;  // Reserved.
  IMAGE_LIBRARY_PROCESS_TERM                     = $0002;  // Reserved.
  IMAGE_LIBRARY_THREAD_INIT                      = $0004;  // Reserved.
  IMAGE_LIBRARY_THREAD_TERM                      = $0008;  // Reserved.
  IMAGE_DLLCHARACTERISTICS_NO_ISOLATION          = $0200;  { Image understands isolation and doesn't want it }
  IMAGE_DLLCHARACTERISTICS_NO_SEH                = $0400;  { Image does not use SEH.  No SE handler may reside in this image }
  IMAGE_DLLCHARACTERISTICS_NO_BIND               = $0800;  { do not bind this image }
//                                                 $1000;  { Reserved. }
  IMAGE_DLLCHARACTERISTICS_WDM_DRIVER            = $2000;  { dll is a WDM driver }
//                                                 $4000;  { Reserved. }
  IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE = $8000;


  // Reserved section characteristics
  IMAGE_SCN_TYPE_REG      = $00000000;  // Reserved.
  IMAGE_SCN_TYPE_DSECT    = $00000001;  // Reserved.
  IMAGE_SCN_TYPE_NOLOAD   = $00000002;  // Reserved.
  IMAGE_SCN_TYPE_GROUP    = $00000004;  // Reserved.
  IMAGE_SCN_TYPE_COPY     = $00000010;  // Reserved.
  IMAGE_SCN_TYPE_OVER     = $00000400;  // Reserved.
  IMAGE_SCN_MEM_PROTECTED = $00004000;  // Obsolete
  IMAGE_SCN_MEM_SYSHEAP   = $00010000;  // Obsolete

{$IFDEF _headers_translated_in_rtl_}

type
(*
  typedef struct _IMAGE_OPTIONAL_HEADER64 {
 WORD        Magic;
 BYTE        MajorLinkerVersion;
 BYTE        MinorLinkerVersion;
 DWORD       SizeOfCode;
 DWORD       SizeOfInitializedData;
 DWORD       SizeOfUninitializedData;
 DWORD       AddressOfEntryPoint;
 DWORD       BaseOfCode;
 ULONGLONG   ImageBase;
 DWORD       SectionAlignment;
 DWORD       FileAlignment;
 WORD        MajorOperatingSystemVersion;
 WORD        MinorOperatingSystemVersion;
 WORD        MajorImageVersion;
 WORD        MinorImageVersion;
 WORD        MajorSubsystemVersion;
 WORD        MinorSubsystemVersion;
 DWORD       Win32VersionValue;
 DWORD       SizeOfImage;
 DWORD       SizeOfHeaders;
 DWORD       CheckSum;
 WORD        Subsystem;
 WORD        DllCharacteristics;
 ULONGLONG   SizeOfStackReserve;
 ULONGLONG   SizeOfStackCommit;
 ULONGLONG   SizeOfHeapReserve;
 ULONGLONG   SizeOfHeapCommit;
 DWORD       LoaderFlags;
 DWORD       NumberOfRvaAndSizes;
 IMAGE_DATA_DIRECTORY DataDirectory[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
} IMAGE_OPTIONAL_HEADER64, *PIMAGE_OPTIONAL_HEADER64;
*)
  PImageOptionalHeader64 = ^TImageOptionalHeader64;
  _IMAGE_OPTIONAL_HEADER64 = packed record
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: DWORD;
    SizeOfInitializedData: DWORD;
    SizeOfUninitializedData: DWORD;
    AddressOfEntryPoint: DWORD;
    BaseOfCode: DWORD;
//    BaseOfData: DWORD;
    ImageBase: Int64;
    SectionAlignment: DWORD;
    FileAlignment: DWORD;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: DWORD;
    SizeOfImage: DWORD;
    SizeOfHeaders: DWORD;
    CheckSum: DWORD;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: Int64;
    SizeOfStackCommit: Int64;
    SizeOfHeapReserve: Int64;
    SizeOfHeapCommit: Int64;
    LoaderFlags: DWORD;
    NumberOfRvaAndSizes: DWORD;
    DataDirectory: packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TImageDataDirectory;
  end;
  TImageOptionalHeader64 = _IMAGE_OPTIONAL_HEADER64;
  IMAGE_OPTIONAL_HEADER64 = _IMAGE_OPTIONAL_HEADER64;



(*
typedef struct _IMAGE_NT_HEADERS64 {
    DWORD Signature;
    IMAGE_FILE_HEADER FileHeader;
    IMAGE_OPTIONAL_HEADER64 OptionalHeader;
} IMAGE_NT_HEADERS64, *PIMAGE_NT_HEADERS64;

*)

  PImageNtHeaders64 = ^TImageNtHeaders64;
  _IMAGE_NT_HEADERS64 = packed record
    Signature: DWORD;
    FileHeader: TImageFileHeader;
    OptionalHeader: TImageOptionalHeader64;
  end;
  TImageNtHeaders64 = _IMAGE_NT_HEADERS64;
  IMAGE_NT_HEADERS64 = _IMAGE_NT_HEADERS64;

{$ENDIF}

procedure DumpPEImage(const AProcessHandle: THandle; const AAdress: TDbgPtr);

implementation

{$IFDEF _headers_translated_in_rtl_}
const
  DIR_NAMES: array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of string = (
    'EXPORT',
    'IMPORT',
    'RESOURCE',
    'EXCEPTION',
    'SECURITY',
    'BASERELOC',
    'DEBUG',
    'COPYRIGHT',
    'GLOBALPTR',
    'TLS',
    'LOAD_CONFIG',
    'BOUND_IMPORT',
    'IAT',
    'DELAY_IMPORT',
    'COM_DECRIPTOR',
    'Unknown(15)'
  );


procedure DumpPEImage(const AProcessHandle: THandle; const AAdress: TDbgPtr);
var
  DosHeader: TImageDosHeader;
  NtHeaders: TImageNtHeaders64; // read it as 64 bit, so there is enough room. The fields will be decoded manually
  SectionHeader: TImageSectionHeader;
  OH: PImageOptionalHeader64;
  BytesRead: Cardinal;
  R: Boolean;
  n: Integer;
  Is64: Boolean;
  SectionName: array[0..IMAGE_SIZEOF_SHORT_NAME] of Char;
begin
  if not ReadProcessMemory(AProcessHandle, Pointer(AAdress), @DosHeader, SizeOf(DosHeader), BytesRead)
  then begin
    WriteLN('Unable to retrieve DOS header');
    Exit;
  end;

  if (DosHeader.e_magic <> IMAGE_DOS_SIGNATURE)
  or (DosHeader._lfanew = 0)
  then begin
    WriteLN('Invalid DOS header');
    Exit;
  end;

  if not ReadProcessMemory(AProcessHandle, Pointer(PChar(AAdress) + DosHeader._lfanew), @NTHeaders, SizeOf(NTHeaders), BytesRead)
  then begin
    WriteLN('Unable to retrieve NT headers');
    Exit;
  end;

  if NTHeaders.Signature <> IMAGE_NT_SIGNATURE
  then begin
    WriteLN('Invalid NT header: ', IntToHex(NTHeaders.Signature, 8));
    Exit;
  end;

  WriteLN('FileHeader: ');

  with NTHeaders.FileHeader do
  begin
    Write('  Machine:              ', IntToHex(Machine, 4));
    case Machine of
      IMAGE_FILE_MACHINE_I386:    WriteLN(' (Intel 386)');
      IMAGE_FILE_MACHINE_R3000:   WriteLN(' (MIPS little-endian, 0x160 big-endian)');
      IMAGE_FILE_MACHINE_R4000:   WriteLN(' (MIPS little-endian)');
      IMAGE_FILE_MACHINE_R10000:  WriteLN(' (MIPS little-endian)');
      IMAGE_FILE_MACHINE_ALPHA:   WriteLN(' (Alpha_AXP)');
      IMAGE_FILE_MACHINE_POWERPC: WriteLN(' (IBM PowerPC Little-Endian)');
      IMAGE_FILE_MACHINE_IA64:    WriteLN(' (Intel IPF)');
      IMAGE_FILE_MACHINE_AMD64:   WriteLN(' (x64)');
    else
      WriteLN;
    end;
    WriteLN('  NumberOfSections:     ', NumberOfSections);
    WriteLN('  TimeDateStamp:        ', TimeDateStamp);
    WriteLN('  PointerToSymbolTable: ', PointerToSymbolTable);
    WriteLN('  NumberOfSymbols:      ', NumberOfSymbols);
    WriteLN('  SizeOfOptionalHeader: ', SizeOfOptionalHeader);
    Write('  Characteristics:      ', IntToHex(Characteristics, 4), ' [');

    if Characteristics and IMAGE_FILE_RELOCS_STRIPPED <> 0 then Write('RELOCS_STRIPPED ');
    if Characteristics and IMAGE_FILE_EXECUTABLE_IMAGE <> 0 then Write('EXECUTABLE_IMAGE ');
    if Characteristics and IMAGE_FILE_LINE_NUMS_STRIPPED <> 0 then Write('LINE_NUMS_STRIPPED ');
    if Characteristics and IMAGE_FILE_LOCAL_SYMS_STRIPPED <> 0 then Write('LOCAL_SYMS_STRIPPED ');
    if Characteristics and IMAGE_FILE_AGGRESIVE_WS_TRIM <> 0 then Write('AGGRESIVE_WS_TRIM ');
    if Characteristics and IMAGE_FILE_LARGE_ADDRESS_AWARE <> 0 then Write('LARGE_ADDRESS_AWARE ');
    if Characteristics and IMAGE_FILE_BYTES_REVERSED_LO <> 0 then Write('BYTES_REVERSED_LO ');
    if Characteristics and IMAGE_FILE_32BIT_MACHINE <> 0 then Write('32BIT_MACHINE ');
    if Characteristics and IMAGE_FILE_DEBUG_STRIPPED <> 0 then Write('DEBUG_STRIPPED ');
    if Characteristics and IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP <> 0 then Write('REMOVABLE_RUN_FROM_SWAP ');
    if Characteristics and IMAGE_FILE_NET_RUN_FROM_SWAP <> 0 then Write('NET_RUN_FROM_SWAP ');
    if Characteristics and IMAGE_FILE_SYSTEM <> 0 then Write('SYSTEM ');
    if Characteristics and IMAGE_FILE_DLL <> 0 then Write('DLL ');
    if Characteristics and IMAGE_FILE_UP_SYSTEM_ONLY <> 0 then Write('UP_SYSTEM_ONLY ');
    if Characteristics and IMAGE_FILE_BYTES_REVERSED_HI <> 0 then Write('BYTES_REVERSED_HI ');
    WriteLN(']');
  end;

  WriteLN('OptionalHeader: ');
  OH := @NTHeaders.OptionalHeader;
  Is64 := OH^.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC;
  Write('  Magic:                       ', IntToHex(OH^.Magic, 4));
  case OH^.Magic of
    IMAGE_NT_OPTIONAL_HDR32_MAGIC : WriteLN(' (HDR32)');
    IMAGE_NT_OPTIONAL_HDR64_MAGIC : WriteLN(' (HDR64)');
    IMAGE_ROM_OPTIONAL_HDR_MAGIC  : WriteLN(' (ROM)');
  else
    WriteLN;
  end;
  WriteLN('  MajorLinkerVersion:          ', OH^.MajorLinkerVersion);
  WriteLN('  MinorLinkerVersion:          ', OH^.MinorLinkerVersion);
  WriteLN('  SizeOfCode:                  ', OH^.SizeOfCode);
  WriteLN('  SizeOfInitializedData:       ', OH^.SizeOfInitializedData);
  WriteLN('  SizeOfUninitializedData:     ', OH^.SizeOfUninitializedData);
  WriteLN('  AddressOfEntryPoint:         ', FormatAdress(OH^.AddressOfEntryPoint));
  WriteLN('  BaseOfCode:                  ', FormatAdress(OH^.BaseOfCode));
  if Is64
  then begin
    WriteLN('  ImageBase:                   $', IntToHex(OH^.ImageBase, 16));
  end
  else begin
    WriteLN('  BaseOfData:                  $', IntToHex(Integer(OH^.ImageBase), 8));
    WriteLN('  ImageBase:                   $', IntToHex(Integer(OH^.ImageBase shr 32), 8));
  end;
  WriteLN('  SectionAlignment:            ', OH^.SectionAlignment);
  WriteLN('  FileAlignment:               ', OH^.FileAlignment);
  WriteLN('  MajorOperatingSystemVersion: ', OH^.MajorOperatingSystemVersion);
  WriteLN('  MinorOperatingSystemVersion: ', OH^.MinorOperatingSystemVersion);
  WriteLN('  MajorImageVersion:           ', OH^.MajorImageVersion);
  WriteLN('  MinorImageVersion:           ', OH^.MinorImageVersion);
  WriteLN('  MajorSubsystemVersion:       ', OH^.MajorSubsystemVersion);
  WriteLN('  MinorSubsystemVersion:       ', OH^.MinorSubsystemVersion);
  WriteLN('  Win32VersionValue:           ', OH^.Win32VersionValue);
  WriteLN('  SizeOfImage:                 ', OH^.SizeOfImage);
  WriteLN('  SizeOfHeaders:               ', OH^.SizeOfHeaders);
  WriteLN('  CheckSum:                    ', OH^.CheckSum);
  Write('  Subsystem:                   ', OH^.Subsystem);
  case OH^.Subsystem of
    IMAGE_SUBSYSTEM_UNKNOWN:         WriteLN(' (Unknown)');
    IMAGE_SUBSYSTEM_NATIVE:          WriteLN(' (Native)');
    IMAGE_SUBSYSTEM_WINDOWS_CUI:     WriteLN(' (Windows CUI)');
    IMAGE_SUBSYSTEM_WINDOWS_GUI:     WriteLN(' (Windows GUI)');
    IMAGE_SUBSYSTEM_OS2_CUI:         WriteLN(' (OS2_CUI)');
    IMAGE_SUBSYSTEM_POSIX_CUI:       WriteLN(' (POSIX CUI)');
    IMAGE_SUBSYSTEM_WINDOWS_CE_GUI:  WriteLN(' (Windows CE GUI)');
    IMAGE_SUBSYSTEM_XBOX:            WriteLN(' (XBOX)');
  else
    WriteLN;
  end;
  Write('  DllCharacteristics:          ', IntToHex(OH^.DllCharacteristics, 4), ' [');
  if OH^.DllCharacteristics and IMAGE_LIBRARY_PROCESS_INIT                     <> 0 then Write('PROCESS_INIT (reserved) ');
  if OH^.DllCharacteristics and IMAGE_LIBRARY_PROCESS_TERM                     <> 0 then Write('PROCESS_TERM (reserved) ');
  if OH^.DllCharacteristics and IMAGE_LIBRARY_THREAD_INIT                      <> 0 then Write('THREAD_INIT (reserved) ');
  if OH^.DllCharacteristics and IMAGE_LIBRARY_THREAD_TERM                      <> 0 then Write('THREAD_TERM (reserved) ');
  if OH^.DllCharacteristics and IMAGE_DLLCHARACTERISTICS_NO_ISOLATION          <> 0 then Write('NO_ISOLATION ');
  if OH^.DllCharacteristics and IMAGE_DLLCHARACTERISTICS_NO_SEH                <> 0 then Write('NO_SEH ');
  if OH^.DllCharacteristics and IMAGE_DLLCHARACTERISTICS_NO_BIND               <> 0 then Write('NO_BIND ');
  if OH^.DllCharacteristics and IMAGE_DLLCHARACTERISTICS_WDM_DRIVER            <> 0 then Write('WDM_DRIVER ');
  if OH^.DllCharacteristics and IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE <> 0 then Write('TERMINAL_SERVER_AWARE ');
  WriteLN(']');

  Write('  SizeOfStackReserve:          $');
  if Is64
  then begin
    WriteLN(IntToHex(OH^.SizeOfStackReserve, 16));
  end
  else begin
    WriteLN(IntToHex(Integer(OH^.SizeOfStackReserve), 8));
    Dec(PChar(OH), 4); // adjust with 4 bytes so the next record matches again
  end;
  Write('  SizeOfStackCommit:           $');
  if Is64
  then begin
    WriteLN(IntToHex(OH^.SizeOfStackCommit, 16));
  end
  else begin
    WriteLN(IntToHex(Integer(OH^.SizeOfStackCommit), 8));
    Dec(PChar(OH), 4); // adjust with 4 bytes so the next record matches again
  end;
  Write('  SizeOfHeapReserve:           $');
  if Is64
  then begin
    WriteLN(IntToHex(OH^.SizeOfHeapReserve, 16));
  end
  else begin
    WriteLN(IntToHex(Integer(OH^.SizeOfHeapReserve), 8));
    Dec(PChar(OH), 4); // adjust with 4 bytes so the next record matches again
  end;
  Write('  SizeOfHeapCommit:            $');
  if Is64
  then begin
    WriteLN(IntToHex(OH^.SizeOfHeapCommit, 16));
  end
  else begin
    WriteLN(IntToHex(Integer(OH^.SizeOfHeapCommit), 8));
    Dec(PChar(OH), 4); // adjust with 4 bytes so the next record matches again
  end;
  WriteLN('  LoaderFlags:                 ', OH^.LoaderFlags);
  WriteLN('  NumberOfRvaAndSizes:         ', OH^.NumberOfRvaAndSizes);
  WriteLN('  DataDirectory:');
  for n := 0 to IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1 do
  begin
    WriteLN('   [', DIR_NAMES[n]+']':14, ' Adress: $', IntToHex(OH^.DataDirectory[n].VirtualAddress, 8), ' Size: ', OH^.DataDirectory[n]. Size);
  end;

  WriteLN('Sections: ');
  for n := 0 to NtHeaders.FileHeader.NumberOfSections  - 1 do
  begin
    if not ReadProcessMemory(AProcessHandle, Pointer(Cardinal(AAdress) + DosHeader._lfanew + SizeOF(NTHeaders) - SizeOF(NTHeaders.OptionalHeader) + NTHeaders.FileHeader.SizeOfOptionalHeader + SizeOf(SectionHeader) * n), @SectionHeader, SizeOf(SectionHeader), BytesRead)
    then begin
      WriteLN('Unable to retrieve section: ', n);
      Continue;
    end;
    with SectionHeader do
    begin
      Move(Name, SectionName, IMAGE_SIZEOF_SHORT_NAME);
      SectionName[IMAGE_SIZEOF_SHORT_NAME] := #0;
      WriteLN('  Name:                 ',SectionName);
      WriteLN('  Misc.PhysicalAddress: ',FormatAdress(Misc.PhysicalAddress));
      WriteLN('  Misc.VirtualSize:     ',Misc.VirtualSize);
      WriteLN('  VirtualAddress:       ',FormatAdress(VirtualAddress));
      WriteLN('  SizeOfRawData:        ',SizeOfRawData);
      WriteLN('  PointerToRawData:     ',FormatAdress(PointerToRawData));
      WriteLN('  PointerToRelocations: ',FormatAdress(PointerToRelocations));
      WriteLN('  PointerToLinenumbers: ',FormatAdress(PointerToLinenumbers));
      WriteLN('  NumberOfRelocations:  ',NumberOfRelocations);
      WriteLN('  NumberOfLinenumbers:  ',NumberOfLinenumbers);
      Write('  Characteristics:      ', IntToHex(Characteristics, 8), ' [');
      if Characteristics and IMAGE_SCN_TYPE_REG <> 0 then Write('IMAGE_SCN_TYPE_REG(r) ');
      if Characteristics and IMAGE_SCN_TYPE_DSECT <> 0 then Write('IMAGE_SCN_TYPE_DSECT(r) ');
      if Characteristics and IMAGE_SCN_TYPE_NOLOAD <> 0 then Write('IMAGE_SCN_TYPE_NOLOAD(r) ');
      if Characteristics and IMAGE_SCN_TYPE_GROUP <> 0 then Write('IMAGE_SCN_TYPE_GROUP(r) ');
      if Characteristics and IMAGE_SCN_TYPE_NO_PAD <> 0 then Write('IMAGE_SCN_TYPE_NO_PAD(r) ');
      if Characteristics and IMAGE_SCN_TYPE_COPY <> 0 then Write('IMAGE_SCN_TYPE_COPY(r) ');
      if Characteristics and IMAGE_SCN_CNT_CODE <> 0 then Write('IMAGE_SCN_CNT_CODE ');
      if Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA <> 0 then Write('IMAGE_SCN_CNT_INITIALIZED_DATA ');
      if Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA <> 0 then Write('IMAGE_SCN_CNT_UNINITIALIZED_DATA ');
      if Characteristics and IMAGE_SCN_LNK_OTHER <> 0 then Write('IMAGE_SCN_LNK_OTHER(r) ');
      if Characteristics and IMAGE_SCN_LNK_INFO <> 0 then Write('IMAGE_SCN_LNK_INFO(r) ');
      if Characteristics and IMAGE_SCN_TYPE_OVER <> 0 then Write('IMAGE_SCN_TYPE_OVER(r) ');
      if Characteristics and IMAGE_SCN_LNK_COMDAT <> 0 then Write('IMAGE_SCN_LNK_COMDAT ');
      if Characteristics and IMAGE_SCN_MEM_PROTECTED <> 0 then Write('IMAGE_SCN_MEM_PROTECTED(o) ');
      if Characteristics and IMAGE_SCN_MEM_FARDATA <> 0 then Write('IMAGE_SCN_MEM_FARDATA(r) ');
      if Characteristics and IMAGE_SCN_MEM_SYSHEAP <> 0 then Write('IMAGE_SCN_MEM_SYSHEAP(o) ');
      if Characteristics and IMAGE_SCN_MEM_PURGEABLE <> 0 then Write('IMAGE_SCN_MEM_PURGEABLE(r) ');
      if Characteristics and IMAGE_SCN_MEM_16BIT <> 0 then Write('IMAGE_SCN_MEM_16BIT(r) ');
      if Characteristics and IMAGE_SCN_MEM_LOCKED <> 0 then Write('IMAGE_SCN_MEM_LOCKED(r) ');
      if Characteristics and IMAGE_SCN_MEM_PRELOAD <> 0 then Write('IMAGE_SCN_MEM_PRELOAD(r) ');
      // Align
      if Characteristics and $00F00000 <> 0
      then Write('IMAGE_SCN_ALIGN_', 1 shl (((Characteristics and $00F00000) shr 20) - 1),'BYTES ');
      if Characteristics and IMAGE_SCN_LNK_NRELOC_OVFL <> 0 then Write('IMAGE_SCN_LNK_NRELOC_OVFL ');
      if Characteristics and IMAGE_SCN_MEM_DISCARDABLE <> 0 then Write('IMAGE_SCN_MEM_DISCARDABLE ');
      if Characteristics and IMAGE_SCN_MEM_NOT_CACHED <> 0 then Write('IMAGE_SCN_MEM_NOT_CACHED ');
      if Characteristics and IMAGE_SCN_MEM_NOT_PAGED <> 0 then Write('IMAGE_SCN_MEM_NOT_PAGED ');
      if Characteristics and IMAGE_SCN_MEM_SHARED <> 0 then Write('IMAGE_SCN_MEM_SHARED ');
      if Characteristics and IMAGE_SCN_MEM_EXECUTE <> 0 then Write('IMAGE_SCN_MEM_EXECUTE ');
      if Characteristics and IMAGE_SCN_MEM_READ <> 0 then Write('IMAGE_SCN_MEM_READ ');
      if Characteristics and IMAGE_SCN_MEM_WRITE <> 0 then Write('IMAGE_SCN_MEM_WRITE ');
      WriteLN(']');
    end;

  end;
end;
{$ELSE}
procedure DumpPEImage(const AProcessHandle: THandle; const AAdress: TDbgPtr);
begin
{$WARNING PEHeaders not yet translated}
end;
{$ENDIF}



end.
