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

const
  PPU_ID_Size = 3;
  PPU_Ver_Size = 3;

type
  tppuheader=packed record
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

  tppuentry=packed record
    size : longint;
    id   : byte;
    nr   : byte;
  end;
  
  { TPPU }

  TPPU = class
  private
    fChangeEndian: boolean;
    FInputStream: TStream;
    FHeader: tppuheader;
    procedure ReadHeader;
    procedure InitInput(s: TStream);
    procedure ReadBuf(var Buf; Count: longint);
    procedure ReadWord(out w: word);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromStream(s: TStream);
    procedure LoadFromFile(const Filename: string);
    procedure Dump(const Prefix: string = '');
    procedure DumpHeader(const Prefix: string = '');
    property InputStream: TStream read FInputStream;
  end;

implementation

{ TPPU }

procedure TPPU.ReadHeader;
begin
  // read ID
  ReadBuf(FHeader.id,PPU_ID_Size);
  // read version
  ReadBuf(FHeader.ver,PPU_Ver_Size);
  // read rest of header
  ReadBuf(FHeader.compiler,SizeOf(tppuheader)-PPU_Ver_Size-PPU_ID_Size);
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

procedure TPPU.ReadWord(out w: word);
begin
  FInputStream.Read(w,2);
  if fChangeEndian then
    swapendian(w);
end;

constructor TPPU.Create;
begin

end;

destructor TPPU.Destroy;
begin
  inherited Destroy;
end;

procedure TPPU.Clear;
begin
  FillByte(FHeader,SizeOf(FHeader),0);
end;

procedure TPPU.LoadFromStream(s: TStream);
begin
  Clear;
  InitInput(s);
  ReadHeader;
  FInputStream:=nil;
end;

procedure TPPU.LoadFromFile(const Filename: string);
var
  ms: TMemoryStream;
  fs: TFileStream;
begin
  fs:=TFileStream.Create(Filename,fmOpenRead);
  ms:=TMemoryStream.Create;
  try
    ms.CopyFrom(fs,fs.Size);
    ms.Position:=0;
    LoadFromStream(ms);
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
  DebugLn([Prefix,'  Ver=',String(FHeader.ver)]);
  DebugLn([Prefix,'  Compiler=',FHeader.compiler]);
  DebugLn([Prefix,'  CPU=',FHeader.cpu]);
  DebugLn([Prefix,'  Target=',FHeader.target]);
  DebugLn([Prefix,'  Flags=',FHeader.flags]);
  DebugLn([Prefix,'  Size=',FHeader.size]);
  DebugLn([Prefix,'  Checksum=',FHeader.checksum]);
  DebugLn([Prefix,'  Interface_CheckSum=',FHeader.interface_checksum]);
  DebugLn([Prefix,'  deflistsize=',FHeader.deflistsize]);
  DebugLn([Prefix,'  symlistsize=',FHeader.symlistsize]);
end;

end.

