{
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

  Author: Mattias Gaertner

  Note:
    This unit will move to the FCL when it has stabilized.

  Abstract:
    Functions to classes to read ppu streams (Free Pascal compiled units)
    of various versions. For example reading 2.3.1 ppus compiled for 64bit
    with a lazarus compiled with fpc 2.2.2 i386.
}
unit PPUParser;

{$mode objfpc}{$H+}

{$DEFINE VerbosePPUParser}

interface

uses
  Classes, SysUtils, FileProcs;

const
  PPUIsEndianBig = {$IFDEF ENDIAN_BIG}True{$ELSE}False{$ENDIF};
  
const
{ppu entries}
  mainentryid         = 1;
  subentryid          = 2;
  {special}
  iberror             = 0;
  ibstartdefs         = 248;
  ibenddefs           = 249;
  ibstartsyms         = 250;
  ibendsyms           = 251;
  ibendinterface      = 252;
  ibendimplementation = 253;
//  ibendbrowser        = 254;
  ibend               = 255;
  {general}
  ibmodulename           = 1;
  ibsourcefiles          = 2;
  ibloadunit             = 3;
  ibinitunit             = 4;
  iblinkunitofiles       = 5;
  iblinkunitstaticlibs   = 6;
  iblinkunitsharedlibs   = 7;
  iblinkotherofiles      = 8;
  iblinkotherstaticlibs  = 9;
  iblinkothersharedlibs  = 10;
  ibImportSymbols        = 11;
  ibsymref               = 12;
  ibdefref               = 13;
//  ibendsymtablebrowser   = 14;
//  ibbeginsymtablebrowser = 15;
  ibusedmacros           = 16;
  ibderefdata            = 17;
  ibexportedmacros       = 18;
  ibderefmap             = 19;
  {syms}
  ibtypesym        = 20;
  ibprocsym        = 21;
  ibstaticvarsym   = 22;
  ibconstsym       = 23;
  ibenumsym        = 24;
//  ibtypedconstsym  = 25;
  ibabsolutevarsym = 26;
  ibpropertysym    = 27;
  ibfieldvarsym    = 28;
  ibunitsym        = 29;
  iblabelsym       = 30;
  ibsyssym         = 31;
//  ibrttisym        = 32;
  iblocalvarsym    = 33;
  ibparavarsym     = 34;
  ibmacrosym       = 35;
  {definitions}
  iborddef         = 40;
  ibpointerdef     = 41;
  ibarraydef       = 42;
  ibprocdef        = 43;
  ibshortstringdef = 44;
  ibrecorddef      = 45;
  ibfiledef        = 46;
  ibformaldef      = 47;
  ibobjectdef      = 48;
  ibenumdef        = 49;
  ibsetdef         = 50;
  ibprocvardef     = 51;
  ibfloatdef       = 52;
  ibclassrefdef    = 53;
  iblongstringdef  = 54;
  ibansistringdef  = 55;
  ibwidestringdef  = 56;
  ibvariantdef     = 57;
  ibundefineddef   = 58;
  {implementation/ObjData}
  ibnodetree       = 80;
  ibasmsymbols     = 81;
  ibresources      = 82;

  ibmainname       = 90;
  { target-specific things }
  iblinkotherframeworks = 100;

{ unit flags }
  uf_init          = $1;
  uf_finalize      = $2;
  uf_big_endian    = $4;
//  uf_has_browser   = $10;
  uf_in_library    = $20;     { is the file in another file than <ppufile>.* ? }
  uf_smart_linked  = $40;     { the ppu can be smartlinked }
  uf_static_linked = $80;     { the ppu can be linked static }
  uf_shared_linked = $100;    { the ppu can be linked shared }
//  uf_local_browser = $200;
  uf_no_link       = $400;    { unit has no .o generated, but can still have
                                external linking! }
  uf_has_resourcestrings = $800;    { unit has resource string section }
  uf_little_endian = $1000;
  uf_release       = $2000;   { unit was compiled with -Ur option }
  uf_threadvars    = $4000;   { unit has threadvars }
  uf_fpu_emulation = $8000;   { this unit was compiled with fpu emulation on }
  uf_has_debuginfo = $10000;  { this unit has debuginfo generated }
  uf_local_symtable = $20000; { this unit has a local symtable stored }
  uf_uses_variants  = $40000; { this unit uses variants }
  uf_has_resourcefiles = $80000; { this unit has external resources (using $R directive)}
  uf_has_exports = $100000;   { this module or a used unit has exports }

type
  TPPUPart = (
    ppInterfaceHeader,
    ppInterfaceDefinitions,
    ppInterfaceSymbols,
    ppInterfaceMacros,
    ppImplementationHeader,
    ppImplementationDefinitions,
    ppImplementationSymbols
  );
  TPPUParts = set of TPPUPart;
  
const
  PPUPartsAll = [low(TPPUPart)..high(TPPUPart)];

const
  PPU_ID = 'PPU';
  PPU_ID_Size = 3;
  PPU_Ver_Size = 3;

type
  TPPUHeader = packed record
    id       : array[1..PPU_ID_Size] of char; { = 'PPU' }
    ver      : array[1..PPU_Ver_Size] of char;
    compiler : word;
    cpu      : word;
    target   : word;
    flags    : longint;
    size     : longint; { size of the ppufile without header }
    checksum : cardinal; { checksum for this ppufile }
    interface_checksum : cardinal;
    deflistsize,
    symlistsize : longint;
    future   : array[0..0] of longint;
  end;

  TPPUEntry = packed record
    size : longint; // number of bytes following directly behind the entry
    id   : byte;
    nr   : byte;
  end;
  
  { EPPUParserError }

  EPPUParserError = class(Exception)
  end;

  { TPPU }

  TPPU = class
  private
    fChangeEndian: boolean;
    FInputStream: TStream;
    FHeader: TPPUHeader;
    FEntry: TPPUEntry;
    FEntryPos: integer;
    FEntryBuf: Pointer;
    FEntryBufSize: integer;
    FVersion: integer;
    FDerefData: PByte;
    FDerefDataSize: integer;
    procedure ReadHeader;
    procedure ReadInterfaceHeader;
    function ReadEntry: byte;
    function EndOfEntry: boolean;
    procedure SkipUntilEntry(EntryNr: byte);
    procedure InitInput(s: TStream);
    procedure ReadBuf(var Buf; Count: longint);
    function ReadEntryByte: byte;
    function ReadEntryShortstring: shortstring;
    function ReadEntryLongint: longint;
    procedure ReadEntrySmallSet(var s);
    procedure ReadUsedUnits;
    procedure ReadLinkContainer(Nr: byte);
    procedure ReadImportSymbols;
    procedure ReadDerefData;
    procedure ReadDerefMap;
    procedure ReadDereference;
    procedure ReadDefinitions;
    procedure ReadCommonDefinition;
    procedure Skip(Count: integer);
    procedure Error(const Msg: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromStream(s: TStream; const Parts: TPPUParts = PPUPartsAll);
    procedure LoadFromFile(const Filename: string; const Parts: TPPUParts = PPUPartsAll);
    procedure Dump(const Prefix: string = '');
    procedure DumpHeader(const Prefix: string = '');
    property InputStream: TStream read FInputStream;
  end;

function PPUTargetToStr(w: longint): string;
function PPUCpuToStr(w: longint): string;
function PPUFlagsToStr(flags: longint): string;
function PPUTimeToStr(t: longint): string;

implementation

function reverse_byte(b: byte): byte;
const
  reverse_nible: array[0..15] of 0..15 =
    (%0000,%1000,%0100,%1100,%0010,%1010,%0110,%1110,
     %0001,%1001,%0101,%1101,%0011,%1011,%0111,%1111);
begin
  Result:=(reverse_nible[b and $f] shl 4) or reverse_nible[b shr 4];
end;

function PPUTargetToStr(w: longint): string;
type
       { taken from systems.pas }
       ttarget =
       (
             target_none,               { 0 }
             target_i386_GO32V1,        { 1 }
             target_i386_GO32V2,        { 2 }
             target_i386_linux,         { 3 }
             target_i386_OS2,           { 4 }
             target_i386_Win32,         { 5 }
             target_i386_freebsd,       { 6 }
             target_m68k_Amiga,         { 7 }
             target_m68k_Atari,         { 8 }
             target_m68k_Mac,           { 9 }
             target_m68k_linux,         { 10 }
             target_m68k_PalmOS,        { 11 }
             target_alpha_linux,        { 12 }
             target_powerpc_linux,      { 13 }
             target_powerpc_macos,      { 14 }
             target_i386_sunos,         { 15 }
             target_i386_beos,          { 16 }
             target_i386_netbsd,        { 17 }
             target_m68k_netbsd,        { 18 }
             target_i386_Netware,       { 19 }
             target_i386_qnx,           { 20 }
             target_i386_wdosx,         { 21 }
             target_sparc_sunos,        { 22 }
             target_sparc_linux,        { 23 }
             target_i386_openbsd,       { 24 }
             target_m68k_openbsd,       { 25 }
             system_x86_64_linux,       { 26 }
             system_powerpc_macosx,     { 27 }
             target_i386_emx,           { 28 }
             target_powerpc_netbsd,     { 29 }
             target_powerpc_openbsd,    { 30 }
             target_arm_linux,          { 31 }
             target_i386_watcom,        { 32 }
             target_powerpc_MorphOS,    { 33 }
             target_x86_64_freebsd,     { 34 }
             target_i386_netwlibc,      { 35 }
             system_powerpc_Amiga,      { 36 }
             system_x86_64_win64,       { 37 }
             system_arm_wince,          { 38 }
             system_ia64_win64,         { 39 }
             system_i386_wince,         { 40 }
             system_x86_6432_linux,     { 41 }
             system_arm_gba,            { 42 }
             system_arm_nds             { 43 }
       );
const
  Targets : array[ttarget] of string[17]=(
  { 0 }   'none',
  { 1 }   'GO32V1',
  { 2 }   'GO32V2',
  { 3 }   'Linux-i386',
  { 4 }   'OS/2',
  { 5 }   'Win32',
  { 6 }   'FreeBSD-i386',
  { 7 }   'Amiga',
  { 8 }   'Atari',
  { 9 }   'MacOS-m68k',
  { 10 }  'Linux-m68k',
  { 11 }  'PalmOS-m68k',
  { 12 }  'Linux-alpha',
  { 13 }  'Linux-ppc',
  { 14 }  'MacOS-ppc',
  { 15 }  'Solaris-i386',
  { 16 }  'BeOS-i386',
  { 17 }  'NetBSD-i386',
  { 18 }  'NetBSD-m68k',
  { 19 }  'Netware-i386-clib',
  { 20 }  'Qnx-i386',
  { 21 }  'WDOSX-i386',
  { 22 }  'Solaris-sparc',
  { 23 }  'Linux-sparc',
  { 24 }  'OpenBSD-i386',
  { 25 }  'OpenBSD-m68k',
  { 26 }  'Linux-x86-64',
  { 27 }  'MacOSX-ppc',
  { 28 }  'OS/2 via EMX',
  { 29 }  'NetBSD-powerpc',
  { 30 }  'OpenBSD-powerpc',
  { 31 }  'Linux-arm',
  { 32 }  'Watcom-i386',
  { 33 }  'MorphOS-powerpc',
  { 34 }  'FreeBSD-x86-64',
  { 35 }  'Netware-i386-libc',
  { 36 }  'Amiga-PowerPC',
  { 37 }  'Win64-x64',
  { 38 }  'WinCE-ARM',
  { 39 }  'Win64-iA64',
  { 40 }  'WinCE-i386',
  { 41 }  'Linux-x64',
  { 42 }  'GBA-arm',
  { 43 }  'NDS-arm'
  );
begin
  if w<=ord(high(ttarget)) then
    Result:=Targets[ttarget(w)]
  else
    Result:='<!! Unknown target value '+IntToStr(w)+'>';
end;

function PPUCpuToStr(w:longint):string;
type
  { Copied from systems.pas }
       tsystemcpu=
       (
             cpu_no,                       { 0 }
             cpu_i386,                     { 1 }
             cpu_m68k,                     { 2 }
             cpu_alpha,                    { 3 }
             cpu_powerpc,                  { 4 }
             cpu_sparc,                    { 5 }
             cpu_vm,                       { 6 }
             cpu_iA64,                     { 7 }
             cpu_x86_64,                   { 8 }
             cpu_mips,                     { 9 }
             cpu_arm                       { 10 }
       );
const
  CpuTxt : array[tsystemcpu] of string[8]=
    ('none','i386','m68k','alpha','powerpc','sparc','vis','ia64','x86_64','mips','arm');
begin
  if w<=ord(high(tsystemcpu)) then
    Result:=CpuTxt[tsystemcpu(w)]
  else
    Result:='<!! Unknown cpu value '+IntToStr(w)+'>';
end;

function PPUFlagsToStr(flags:longint):string;
type
  tflagopt=record
    mask : longint;
    str  : string[30];
  end;
const
  flagopts=17;
  flagopt : array[1..flagopts] of tflagopt=(
    (mask: $1    ;str:'init'),
    (mask: $2    ;str:'final'),
    (mask: $4    ;str:'big_endian'),
    (mask: $8    ;str:'dbx'),
//    (mask: $10   ;str:'browser'),
    (mask: $20   ;str:'in_library'),
    (mask: $40   ;str:'smart_linked'),
    (mask: $80   ;str:'static_linked'),
    (mask: $100  ;str:'shared_linked'),
//    (mask: $200  ;str:'local_browser'),
    (mask: $400  ;str:'no_link'),
    (mask: $800  ;str:'has_resources'),
    (mask: $1000  ;str:'little_endian'),
    (mask: $2000  ;str:'release'),
    (mask: $4000  ;str:'local_threadvars'),
    (mask: $8000  ;str:'fpu_emulation_on'),
    (mask: $10000  ;str:'has_debug_info'),
    (mask: $20000  ;str:'local_symtable'),
    (mask: $40000  ;str:'uses_variants')
  );
var
  i : longint;
  first  : boolean;
  s : string;
begin
  s:='';
  if flags<>0 then
   begin
     first:=true;
     for i:=1to flagopts do
      if (flags and flagopt[i].mask)<>0 then
       begin
         if first then
           first:=false
         else
           s:=s+', ';
         s:=s+flagopt[i].str;
       end;
   end
  else
   s:='none';
  Result:=s;
end;

function L0(l: longint): shortstring;
{
  return the string of value l, if l<10 then insert a zero, so
  the string is always at least 2 chars '01','02',etc
}
var
  s : shortstring;
begin
  Str(l,s);
  if l<10 then
    s:='0'+s;
  Result:=s;
end;

function PPUTimeToStr(t: longint): string;
{
  convert dos datetime t to a string YY/MM/DD HH:MM:SS
}
var
  DT: TDateTime;
  hsec: word;
  Year, Month, Day: Word;
  hour, min, sec: word;
begin
  if t=-1 then
  begin
    Result := '<invalid time>';
    exit;
  end;
  DT := FileDateToDateTime(t);
  DecodeTime(DT,hour,min,sec,hsec);
  DecodeDate(DT,year,month,day);
  Result := L0(Year)+'/'+L0(Month)+'/'+L0(Day)+' '+L0(Hour)+':'+L0(min)+':'+L0(sec);
end;

{ TPPU }

procedure TPPU.ReadHeader;
begin
  // read ID
  ReadBuf(FHeader.id,PPU_ID_Size);
  if String(FHeader.id)<>PPU_ID then
    Error('This is not a PPU. Wrong ID.');
  // read version
  ReadBuf(FHeader.ver,PPU_Ver_Size);
  FVersion:=StrToIntDef(String(FHeader.ver),0);
  if FVersion<16 then
    Error('Old PPU versions (<16) are not supported.');
  // read rest of header
  ReadBuf(FHeader.compiler,SizeOf(TPPUHeader)-PPU_Ver_Size-PPU_ID_Size);
  if fChangeEndian then begin
    fHeader.compiler := swapendian(fHeader.compiler);
    fHeader.cpu := swapendian(fHeader.cpu);
    fHeader.target := swapendian(fHeader.target);
    fHeader.flags := swapendian(fHeader.flags);
    fHeader.size := swapendian(fHeader.size);
    fHeader.checksum := swapendian(fHeader.checksum);
    fHeader.interface_checksum := swapendian(fHeader.interface_checksum);
    fHeader.deflistsize := swapendian(fHeader.deflistsize);
    fHeader.symlistsize := swapendian(fHeader.symlistsize);
  end;
  fChangeEndian:=((FHeader.flags and uf_big_endian) = uf_big_endian)<>PPUIsEndianBig;
  FEntryPos:=0;
  FillByte(FEntry,SizeOf(FEntry),0);
  
  {$IFDEF VerbosePPUParser}
  DumpHeader('');
  {$ENDIF}
end;

procedure TPPU.ReadInterfaceHeader;
var
  EntryNr: Byte;
  ModuleName: ShortString;
  Filename: ShortString;
  FileTime: LongInt;
  Conditional: ShortString;
  DefinedAtStartUp: Boolean;
  IsUsed: Boolean;
begin
  repeat
    EntryNr:=ReadEntry;
    //DebugLn(['TPPU.ReadInterface EntryNr=',EntryNr]);
    case EntryNr of
    
    ibmodulename:
      begin
        ModuleName:=ReadEntryShortstring;
        {$IFDEF VerbosePPUParser}
        DebugLn(['TPPU.ReadInterface ModuleName=',ModuleName]);
        {$ENDIF}
      end;
    
    ibsourcefiles:
      begin
        while not EndOfEntry do
        begin
          Filename:=ReadEntryShortstring;// filename
          FileTime:=ReadEntryLongint;// file time
          {$IFDEF VerbosePPUParser}
          DebugLn(['TPPU.ReadInterface SourceFile=',Filename,' Time=',PPUTimeToStr(FileTime)]);
          {$ENDIF}
        end;
      end;
      
    ibloadunit:
      ReadUsedUnits;
      
    iblinkunitofiles,iblinkunitstaticlibs,iblinkunitsharedlibs,
    iblinkotherofiles,iblinkotherstaticlibs,iblinkothersharedlibs:
      ReadLinkContainer(EntryNr);
      
    ibImportSymbols:
      ReadImportSymbols;

    ibusedmacros:
      begin
        while not EndOfEntry do
        begin
          Conditional:=ReadEntryShortstring;
          DefinedAtStartUp:=boolean(ReadEntryByte);
          IsUsed:=boolean(ReadEntryByte);
          {$IFDEF VerbosePPUParser}
          DebugLn(['TPPU.ReadInterface Macro=',Conditional,' DefinedAtStartUp=',DefinedAtStartUp,' Used=',IsUsed]);
          {$ENDIF}
        end;
      end;

    ibderefdata:
      ReadDerefData;

    ibderefmap:
      ReadDerefMap;

    ibendinterface:
      break;
       
    else
      {$IFDEF VerbosePPUParser}
      DebugLn(['TPPU.ReadInterface Skipping unsupported entry ',EntryNr]);
      {$ENDIF}
      FEntryPos:=FEntry.size;
    end;
  until false;
end;

procedure TPPU.ReadDefinitions;
type
  tsettype  = (normset,smallset,varset);
  tordtype = (
    uvoid,
    u8bit,u16bit,u32bit,u64bit,
    s8bit,s16bit,s32bit,s64bit,
    bool8bit,bool16bit,bool32bit,bool64bit,
    uchar,uwidechar,scurrency
  );
  tobjecttyp = (odt_none,
    odt_class,
    odt_object,
    odt_interfacecom,
    odt_interfacecorba,
    odt_cppclass,
    odt_dispinterface
  );
  tvarianttype = (
    vt_normalvariant,vt_olevariant
  );
var
  EntryNr: Byte;
  IsFar: Byte;
begin
  if ReadEntry<>ibstartdefs then
  begin
    Error('missing definitions');
  end;
  repeat
    EntryNr:=ReadEntry;
    case EntryNr of
    
    ibpointerdef:
      begin
        {$IFDEF VerbosePPUParser} DebugLn(['TPPU.ReadDefinitions Pointer definition:']); {$ENDIF}
        ReadCommonDefinition;
        {$IFDEF VerbosePPUParser} DebugLn(['TPPU.ReadDefinitions Pointed type:']); {$ENDIF}
        ReadDereference;
        IsFar:=ReadEntryByte;
        {$IFDEF VerbosePPUParser} DebugLn(['TPPU.ReadDefinitions Is far: ',IsFar]); {$ENDIF}
      end;
      
    ibenddefs:
      break;

    else
      {$IFDEF VerbosePPUParser} DebugLn(['TPPU.ReadDefinitions Skipping unsupported: ',EntryNr]); {$ENDIF}
    end;
    {$IFDEF VerbosePPUParser}
    if not EndOfEntry then
      DebugLn(['TPPU.ReadDefinitions: Warning: Entry has more information stored']);
    {$ENDIF}
  until false;
end;

procedure TPPU.ReadCommonDefinition;
type
  { flags for a definition }
  tdefoption=(
    df_none,
    { type is unique, i.e. declared with type = type <tdef>; }
    df_unique,
    { type is a generic }
    df_generic,
    { type is a specialization of a generic type }
    df_specialization
  );
  tdefoptions=set of tdefoption;

  tdefstate=(
    ds_none,
    ds_vmt_written,
    ds_rtti_table_used,
    ds_init_table_used,
    ds_rtti_table_written,
    ds_init_table_written,
    ds_dwarf_dbg_info_used,
    ds_dwarf_dbg_info_written
  );
  tdefstates=set of tdefstate;
const
  defoptionNames : array[tdefoption] of string=(
     '?',
     'Unique Type',
     'Generic',
     'Specialization'
  );
  defstateNames : array[tdefstate] of string=(
     '?',
     'VMT Written',
     'RTTITable Used',
     'InitTable Used',
     'RTTITable Written',
     'InitTable Written',
     'Dwarf DbgInfo Used',
     'Dwarf DbgInfo Written'
  );
var
  defoptions: tdefoptions;
  defopt: tdefoption;
  defstates: tdefstates;
  defstate: tdefstate;
  TokenBuf: Pointer;
  TokenBufSize: LongInt;
  i: Integer;
begin
  ReadEntryLongint;// DefinitionID
  ReadDereference;

  ReadEntrySmallSet(defoptions);
  {$IFDEF VerbosePPUParser}
  dbgout(' DefOptions:');
  if defoptions<>[] then begin
    for defopt:=low(tdefoption) to high(tdefoption) do
      if defopt in defoptions then
        dbgout(' ',defoptionNames[defopt]);
  end;
  debugln;
  {$ENDIF}
  
  ReadEntrySmallSet(defstates);
  {$IFDEF VerbosePPUParser}
  dbgout(' DefStates:');
  if defstates<>[] then begin
    for defstate:=low(tdefstate) to high(tdefstate) do
      if defstate in defstates then
        dbgout(' ',defstateNames[defstate]);
  end;
  debugln;
  {$ENDIF}

  if df_generic in defoptions then begin
    TokenBufSize:=ReadEntryLongint;
    TokenBuf:=allocmem(TokenBufSize);
    try
      System.Move(Pointer(FEntryBuf+FEntryPos)^,TokenBuf^,TokenBufSize);
      inc(FEntryPos,TokenBufSize);
      i:=0;
      while i<TokenBufSize do begin
        // The tokens depends on compiler version
        // ToDo: write tokens
        inc(i);
      end;
    finally
      FreeMem(TokenBuf);
    end;
  end;

  if df_specialization in defoptions then
  begin
    ReadDereference;
  end;
end;

procedure TPPU.ReadDereference;
type
  tdereftype = (
    deref_nil,
    deref_unit,
    deref_symid,
    deref_defid
  );
var
  DerefPos: LongInt;
  pdata: PByte;
  n: Byte;
  i: Integer;
  b: tdereftype;
  idx: integer;
begin
  DerefPos:=ReadEntryLongint;
  if DerefPos>=FDerefDataSize then
    Error('Invalid Deref, DerefPos>=FDerefDataSize');
  {$IFDEF VerbosePPUParser}
  dbgout('(',IntToStr(DerefPos),')');
  {$ENDIF}
  pdata:=@FDerefData[DerefPos];
  n:=pdata^;
  if n<1 then
    Error('Invalid Deref, n<1');
  i:=1;
  while i<=n do begin
    b:=tdereftype(pdata[i]);
    inc(i);
    case b of
    deref_nil :
      {$IFDEF VerbosePPUParser}
      dbgout('Nil');
      {$ENDIF}
    deref_symid :
      begin
        idx:=pdata[i] shl 24 or pdata[i+1] shl 16 or pdata[i+2] shl 8 or pdata[i+3];
        inc(i,4);
        {$IFDEF VerbosePPUParser}
        dbgout('SymId ',IntToStr(idx));
        {$ENDIF}
      end;
    deref_defid :
      begin
        idx:=pdata[i] shl 24 or pdata[i+1] shl 16 or pdata[i+2] shl 8 or pdata[i+3];
        inc(i,4);
        {$IFDEF VerbosePPUParser}
        dbgout('DefId ',IntToStr(idx));
        {$ENDIF}
      end;
    deref_unit :
      begin
        idx:=pdata[i] shl 8 or pdata[i+1];
        inc(i,2);
        {$IFDEF VerbosePPUParser}
        dbgout('Unit ',IntToStr(idx));
        {$ENDIF}
      end;
    else
      begin
        Error('unsupported dereftyp: '+IntToStr(ord(b)));
        break;
      end;
    end;
  end;
  {$IFDEF VerbosePPUParser}
  debugln;
  {$ENDIF}
end;

function TPPU.ReadEntry: byte;
begin
  FEntryPos:=0;
  ReadBuf(FEntry,SizeOf(FEntry));
  if fChangeEndian then
    FEntry.size:=SwapEndian(FEntry.size);
  //DebugLn(['TPPU.ReadEntry ',FEntry.Nr,' ',FInputStream.Position]);
  if not (FEntry.id in [mainentryid,subentryid]) then
    Error('Invalid entry id '+IntToStr(FEntry.id));
  Result:=FEntry.nr;
  if FEntryBufSize<FEntry.size then begin
    FEntryBufSize:=FEntryBufSize*2;
    if FEntryBufSize<FEntry.size then
      FEntryBufSize:=FEntry.size;
    ReAllocMem(FEntryBuf,FEntryBufSize);
  end;
  if FEntry.size>0 then
    ReadBuf(FEntryBuf^,FEntry.size);
end;

function TPPU.EndOfEntry: boolean;
begin
  Result:=FEntryPos>=FEntry.Size;
end;

procedure TPPU.SkipUntilEntry(EntryNr: byte);
var
  b: Byte;
begin
  repeat
    b:=ReadEntry;
  until (b=ibend) or ((b=EntryNr) and (FEntry.id=mainentryid));
  if b<>EntryNr then
    Error('TPPU.SkipUntilEntry not found: '+IntToStr(EntryNr));
end;

procedure TPPU.InitInput(s: TStream);
begin
  FInputStream:=s;
  fChangeEndian:=PPUIsEndianBig;
end;

procedure TPPU.ReadBuf(var Buf; Count: longint);
begin
  FInputStream.Read(Buf,Count);
end;

function TPPU.ReadEntryByte: byte;
begin
  if FEntryPos>=FEntry.size then
    Error('TPPU.ReadEntryByte: out of bytes');
  Result:=PByte(FEntryBuf+FEntryPos)^;
  inc(FEntryPos);
end;

function TPPU.ReadEntryShortstring: shortstring;
var
  l: byte;
  s: shortstring;
begin
  l:=ReadEntryByte;
  s[0]:=chr(l);
  if FEntryPos+l>FEntry.size then
    Error('TPPU.ReadEntryShortstring: out of bytes ');
  System.Move(Pointer(FEntryBuf+FEntryPos)^,s[1],l);
  Result:=s;
  inc(FEntryPos,l);
end;

function TPPU.ReadEntryLongint: longint;
begin
  if FEntryPos+4>FEntry.size then
    Error('TPPU.ReadEntryLongint: out of bytes');
  Result:=PLongint(FEntryBuf+FEntryPos)^;
  inc(FEntryPos,4);
end;

procedure TPPU.ReadEntrySmallSet(var s);
var
  i: longint;
begin
  System.Move(PByte(FEntryBuf+FEntryPos)^,s,4);
  inc(FEntryPos,4);
  if fChangeEndian then
    for i:=0 to 3 do
      Pbyte(@s)[i]:=reverse_byte(Pbyte(@s)[i]);
end;

procedure TPPU.ReadUsedUnits;
var
  Unitname: ShortString;
  CRC: LongInt;
  IntfCRC: LongInt;
begin
  while not EndOfEntry do begin
    Unitname:=ReadEntryShortstring;
    CRC:=ReadEntryLongint;
    IntfCRC:=ReadEntryLongint;
    {$IFDEF VerbosePPUParser}
    DebugLn(['TPPU.ReadUsedUnits Unit=',Unitname,' CRC=',HexStr(cardinal(CRC),8),' IntfCRC=',HexStr(cardinal(IntfCRC),8)]);
    {$ENDIF}
  end;
end;

procedure TPPU.ReadLinkContainer(Nr: byte);
{$IFDEF VerbosePPUParser}
const
  { link options }
  link_none    = $0;
  link_always  = $1;
  link_static  = $2;
  link_smart   = $4;
  link_shared  = $8;
var
  Desc: String;
{$ENDIF}
var
  Filename: ShortString;
  Flags: LongInt;
begin
  while not EndOfEntry do begin
    Filename:=ReadEntryShortstring;
    Flags:=ReadEntryLongint;
    {$IFDEF VerbosePPUParser}
    case Nr of
    iblinkunitofiles:
      Desc:='Link unit object file: ';
    iblinkunitstaticlibs :
      Desc:='Link unit static lib: ';
    iblinkunitsharedlibs :
      Desc:='Link unit shared lib: ';
    iblinkotherofiles :
      Desc:='Link other object file: ';
    iblinkotherstaticlibs :
      Desc:='Link other static lib: ';
    iblinkothersharedlibs :
      Desc:='Link other shared lib: ';
    end;
    Desc:=Desc+Filename;
    if (Flags and link_always)<>0 then
     Desc:=Desc+' always';
    if (Flags and link_static)<>0 then
     Desc:=Desc+' static';
    if (Flags and link_smart)<>0 then
     Desc:=Desc+' smart';
    if (Flags and link_shared)<>0 then
     Desc:=Desc+' shared';
    DebugLn(['TPPU.ReadLinkContainer ',Desc]);
    {$ENDIF}
  end;
end;

procedure TPPU.ReadImportSymbols;
var
  LibName: ShortString;
  SymbolCount: LongInt;
  SymbolName: ShortString;
  SymbolOrdNr: LongInt;
  SymbolIsVar: Boolean;
  i: Integer;
begin
  while not EndOfEntry do begin
    LibName:=ReadEntryShortstring;
    SymbolCount:=ReadEntryLongint;
    {$IFDEF VerbosePPUParser}
    DebugLn(['TPPU.ReadImportSymbols External Library: ',LibName,' (',SymbolCount,' imports)']);
    {$ENDIF}
    for i:=0 to SymbolCount-1 do
    begin
      SymbolName:=ReadEntryShortstring;
      SymbolOrdNr:=ReadEntryLongint;
      SymbolIsVar:=ReadEntryByte<>0;
      {$IFDEF VerbosePPUParser}
      DebugLn(['TPPU.ReadImportSymbols ',SymbolName,' (OrdNr: ',SymbolOrdNr,' IsVar: ',SymbolIsVar,')']);
      {$ENDIF}
    end;
  end;
end;

procedure TPPU.ReadDerefData;
begin
  {$IFDEF VerbosePPUParser}
  DebugLn(['TPPU.ReadDerefData Deref Data length: ',FEntry.size-FEntryPos]);
  {$ENDIF}
  FDerefDataSize:=FEntry.size-FEntryPos;
  if FDerefDataSize>0 then begin
    FDerefData:=AllocMem(FDerefDataSize);
    System.Move(PByte(FEntryBuf+FEntryPos)^,FDerefData^,FDerefDataSize);
    FEntryPos:=FEntry.size;
  end;
end;

procedure TPPU.ReadDerefMap;
var
  Count: LongInt;
  MapName: ShortString;
  i: Integer;
begin
  Count:=ReadEntryLongint;
  for i:=0 to Count-1 do begin
    MapName:=ReadEntryShortstring;
    {$IFDEF VerbosePPUParser}
    DebugLn(['TPPU.ReadDerefMap ',i,' ',MapName]);
    {$ENDIF}
  end;
end;

procedure TPPU.Skip(Count: integer);
begin
  FInputStream.Seek(Count,soFromCurrent);
end;

procedure TPPU.Error(const Msg: string);
begin
  raise EPPUParserError.Create(Msg);
end;

constructor TPPU.Create;
begin

end;

destructor TPPU.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TPPU.Clear;
begin
  FillByte(FHeader,SizeOf(FHeader),0);
  FillByte(FEntry,SizeOf(FEntry),0);
  ReAllocMem(FEntryBuf,0);
  ReAllocMem(FDerefData,0);
  FEntryBufSize:=0;
end;

procedure TPPU.LoadFromStream(s: TStream; const Parts: TPPUParts);
begin
  Clear;
  InitInput(s);
  ReadHeader;
  
  // interface header
  if ppInterfaceHeader in Parts then
    ReadInterfaceHeader
  else
    SkipUntilEntry(ibendinterface);
    
  // interface definitions
  if ppInterfaceDefinitions in Parts then
    ReadDefinitions
  else
    SkipUntilEntry(ibenddefs);
    
  // Interface Symbols
  SkipUntilEntry(ibendsyms);
  
  // Interface Macros
  if ReadEntry<>ibexportedmacros then
    Error('missing exported macros');
  if boolean(ReadEntryByte) then begin
    // skip the definition section for macros (since they are never used)
    SkipUntilEntry(ibenddefs);
    // read the macro symbols
    SkipUntilEntry(ibendsyms);
  end else begin
    // no macros
  end;
  
  // Implementation Header
  SkipUntilEntry(ibendimplementation);
  
  // Implementation Definitions and Symbols
  if (FHeader.flags and uf_local_symtable)<>0 then begin
    SkipUntilEntry(ibenddefs);
    SkipUntilEntry(ibendsyms);
  end else begin
    // no definitions and no symbols
  end;

  FInputStream:=nil;
end;

procedure TPPU.LoadFromFile(const Filename: string; const Parts: TPPUParts);
var
  ms: TMemoryStream;
  fs: TFileStream;
begin
  fs:=TFileStream.Create(Filename,fmOpenRead);
  ms:=TMemoryStream.Create;
  try
    ms.CopyFrom(fs,fs.Size);
    ms.Position:=0;
    LoadFromStream(ms,Parts);
  finally
    ms.Free;
    fs.Free;
  end;
end;

procedure TPPU.Dump(const Prefix: string);
begin
  DebugLn([Prefix+'TPPU.Dump ']);
  DumpHeader(Prefix+'  ');
end;

procedure TPPU.DumpHeader(const Prefix: string);
begin
  DebugLn([Prefix,'Header']);
  DebugLn([Prefix,'  ID=',String(FHeader.ID)]);
  DebugLn([Prefix,'  Ver=',StrToIntDef(String(FHeader.ver),0)]);
  DebugLn([Prefix,'  Compiler=',FHeader.compiler shr 14,'.',
                            (FHeader.compiler shr 7) and $7f,'.',
                            FHeader.compiler and $7f]);
  DebugLn([Prefix,'  Target CPU=',PPUCpuToStr(FHeader.cpu)]);
  DebugLn([Prefix,'  Target OS=',PPUTargetToStr(FHeader.target)]);
  DebugLn([Prefix,'  Unit Flags=',PPUFlagsToStr(FHeader.flags)]);
  DebugLn([Prefix,'  Filesize (w/o header)=',FHeader.size]);
  DebugLn([Prefix,'  Checksum=',HexStr(cardinal(FHeader.checksum),8)]);
  DebugLn([Prefix,'  Interface CheckSum=',HexStr(cardinal(FHeader.interface_checksum),8)]);
  DebugLn([Prefix,'  Number of Definitions=',FHeader.deflistsize]);
  DebugLn([Prefix,'  Number of Symbols=',FHeader.symlistsize]);
end;

end.

