unit FpImgReaderMachoFile;

{$mode objfpc}{$H+}

interface

//todo: powerpc, x86_64

uses
  Classes, SysUtils, macho, FpImgReaderBase,
  FpDbgCommon;

type
  TMachOsection = class(TObject)
    is32   : Boolean;
    sec32  : section;
    sec64  : section_64;
  end;

  { TMachOFile }

  TMachOFile = class(TObject)
  private
    cmdbuf    : array of byte;
    FTargetInfo   : TTargetDescriptor;
  public
    header    : mach_header;
    commands  : array of pload_command;
    sections  : TFPList;
    UUID      : TGuid;
    constructor Create;
    destructor Destroy; override;
    function  LoadFromFile(ALoader: TDbgFileLoader): Boolean;
    property TargetInfo: TTargetDescriptor read FTargetInfo;
  end;


implementation


{ TMachOFile }

constructor TMachOFile.Create;
begin
  sections := TFPList.Create;
end;

destructor TMachOFile.Destroy;
var
  i : integer;
begin
  for i := 0 to sections.Count - 1 do TMachOsection(sections[i]).Free;
  sections.Free;
  inherited Destroy;
end;

function TMachOFile.LoadFromFile(ALoader: TDbgFileLoader): Boolean;
var
  i   : Integer;
  j   : Integer;
  ofs : Integer;
  sc32: psection;
  sc64: psection_64;
  idcm: puuid_command;
  s   : TMachOsection;
  hs  : integer;
  i64 : boolean;
begin
  Result :=  ALoader.Read(0, sizeof(header), @header) = sizeof(header);
  if not Result then Exit;
  i64 := (header.magic = MH_CIGAM_64) or (header.magic = MH_MAGIC_64);
  Result := (header.magic = MH_MAGIC) or (header.magic = MH_CIGAM) or i64;

  if i64 then
  begin
    hs := sizeof(mach_header_64);
    FTargetInfo.bitness := b64;
  end
  else
  begin
    hs := SizeOf(mach_header);
    FTargetInfo.bitness := b32;
  end;
  case header.cputype of
    CPU_TYPE_I386       : FTargetInfo.MachineType := mt386;
    CPU_TYPE_ARM        : FTargetInfo.MachineType := mtARM;
    CPU_TYPE_SPARC      : FTargetInfo.MachineType := mtSPARC;
    //CPU_TYPE_ALPHA      : FTargetInfo.MachineType := mtALPHA;
    CPU_TYPE_POWERPC    : FTargetInfo.MachineType := mtPPC;
    CPU_TYPE_POWERPC64  : FTargetInfo.MachineType := mtPPC;
    CPU_TYPE_X86_64     : FTargetInfo.MachineType := mtX86_64;
    CPU_TYPE_ARM64      : FTargetInfo.MachineType := mtARM;
  else
    FTargetInfo.machineType := mtNone;
  end;

  SetLength(cmdbuf, header.sizeofcmds);
  ALoader.Read(hs, header.sizeofcmds, @cmdbuf[0]);

  SetLength(commands, header.ncmds);
  ofs := 0;
  for i := 0 to header.ncmds - 1 do begin
    commands[i] := @cmdbuf[ofs];

    if commands[i]^.cmd = LC_SEGMENT then begin
      sc32 := @cmdbuf[ofs+sizeof(segment_command)];
      for j := 0 to psegment_command(commands[i])^.nsects- 1 do begin
        s := TMachOSection.Create;
        s.is32:=true;
        s.sec32:=sc32^;
        sections.add(s);
        inc(sc32);
      end;
    end
    else if commands[i]^.cmd = LC_SEGMENT_64 then begin
      sc64 := @cmdbuf[ofs+sizeof(segment_command_64)];
      for j := 0 to psegment_command_64(commands[i])^.nsects- 1 do begin
        s := TMachOSection.Create;
        s.is32:=False;
        s.sec64:=sc64^;
        sections.add(s);
        inc(sc64);
      end;
    end
    else if commands[i]^.cmd = LC_UUID then begin
      idcm := @cmdbuf[ofs];
      UUID:=PGuid(@(idcm^.uuid))^;
    end;
    inc(ofs, commands[i]^.cmdsize);
  end;

end;


end.

