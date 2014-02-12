unit FpdMemoryTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDbgPtr = QWord; // TODO, use from IdeDebuggerInterface, once that is done.

  TFpDbgMemReaderBase = class
  public
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; virtual; abstract;
    function ReadMemoryEx(AnAddress, AnAddressSpace: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; virtual; abstract;
    function ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr): Boolean; virtual; abstract;
    function RegisterSize(ARegNum: Cardinal): Integer; virtual; abstract;
    // Registernum from name
  end;

  // Todo, cpu/language specific operations, endianess, sign extend, float .... default int value for bool
  // TODO: currently it assumes target and own mem are in the same format
  TFpDbgMemConvertor = class
  public
    (* To copy a smaller int/cardinal (e.g. word) into a bigger (e.g. dword),
       adjust ADestPointer so it points to the low value part of the dest
    *)
    procedure AdjustIntPointer(var ADestPointer: Pointer; ASourceSize, ADestSize: Cardinal); virtual; abstract;
    (* Expects a signed integer value of ASourceSize bytes in the low value end
       of the memory (total memory ADataPointer, ADestSize)
       Does zero fill the memory, if no sign extend is needed
    *)
    procedure SignExtend(ADataPointer: Pointer; ASourceSize, ADestSize: Cardinal); virtual; abstract;
    (* Expects an unsigned integer value of ASourceSize bytes in the low value end
       of the memory (total memory ADataPointer, ADestSize)
       Basically zero fill the memory
    *)
    procedure UnsignSignedExtend(ADataPointer: Pointer; ASourceSize, ADestSize: Cardinal); virtual; abstract;
  end;

  { TFpDbgMemConvertorLittleEndian }

  TFpDbgMemConvertorLittleEndian = class(TFpDbgMemConvertor)
  public
    procedure AdjustIntPointer(var ADestPointer: Pointer; ASourceSize, ADestSize: Cardinal); override;
    procedure SignExtend(ADataPointer: Pointer; ASourceSize, ADestSize: Cardinal); override;
    procedure UnsignSignedExtend(ADataPointer: Pointer; ASourceSize, ADestSize: Cardinal); override;
  end;

  (* TFpDbgMemManager
   * allows to to pretend reading from the target, by using its own memory, or
       a constant.
     This is useful if an object expects to read data from the target, but the
       caller needs to "fake" another value.
     E.g. A TObject variable has the address of the (internal) pointer to the
       object data:
       SomeMethod expects "Address of variable". At that address in the target
       memory it expects another address, the pointer to the object data.
     But when doing "TObject(1234)" then 1234 is the pointer to the object data.
       1234 can not be read from the target memory. MemManager will pretend.
    * Provides access to TFpDbgMemConvertor
    * TODO: allow to pre-read and cache Target mem (e.g. before reading all fields of a record
  *)
  TFpDbgMemLocationType = (
    mlfInvalid,
    mlfTargetMem,            // an address in the target (debuggee) process
    mlfSelfMem,              // an address in this(the debuggers) process memory; the data is  in TARGET format (endian, ...)
    // the below will be mapped (and extended) according to endianess
    mlfTargetRegister,       // reads from the register
    mlfTargetRegisterSigned, // reads from the register and sign extends if needed (may be limited to 8 bytes)
    mlfConstant              // an (up to) SizeOf(TDbgPtr) (=8) Bytes Value (endian in format of debug process)
  );

  TFpDbgMemLocation = record
    Address: TDbgPtr;
    MType: TFpDbgMemLocationType;
  end;

  { TFpDbgMemManager }

  TFpDbgMemManager = class
  private
    FMemReader: TFpDbgMemReaderBase;
    FMemConvertor: TFpDbgMemConvertor;
  public
    constructor Create(AMemReader: TFpDbgMemReaderBase; AMemConvertor: TFpDbgMemConvertor);

    function ReadMemory(ALocation: TFpDbgMemLocation; ASize: Cardinal; ADest: Pointer): Boolean;
    function ReadMemoryEx(ALocation: TFpDbgMemLocation; AnAddressSpace: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean;
    function ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr): Boolean;

    function ReadAddress(ALocation: TFpDbgMemLocation; ASize: Cardinal): TFpDbgMemLocation;
    function ReadAddressEx(ALocation: TFpDbgMemLocation;  AnAddressSpace: TDbgPtr; ASize: Cardinal): TFpDbgMemLocation;

    property MemConvertor: TFpDbgMemConvertor read FMemConvertor;
  end;

function NilLoc: TFpDbgMemLocation; inline;
function InvalidLoc: TFpDbgMemLocation; inline;
function TargetLoc(AnAddress: TDbgPtr): TFpDbgMemLocation; inline;
function RegisterLoc(ARegNum: Cardinal): TFpDbgMemLocation; inline;
function RegisterSignedLoc(ARegNum: Cardinal): TFpDbgMemLocation; inline;
function SelfLoc(AnAddress: TDbgPtr): TFpDbgMemLocation; inline;
function SelfLoc(AnAddress: Pointer): TFpDbgMemLocation; inline;
function ConstLoc(AValue: QWord): TFpDbgMemLocation; inline;

function IsTargetAddr(ALocation: TFpDbgMemLocation): Boolean; inline;
function IsValidLoc(ALocation: TFpDbgMemLocation): Boolean; inline;     // Valid, Nil allowed
function IsReadableLoc(ALocation: TFpDbgMemLocation): Boolean; inline;  // Valid and not Nil
function IsTargetNil(ALocation: TFpDbgMemLocation): Boolean; inline;    // valid targed = nil
function IsTargetNotNil(ALocation: TFpDbgMemLocation): Boolean; inline; // valid targed <> nil

function LocToAddr(ALocation: TFpDbgMemLocation): TDbgPtr; inline;      // does not check valid
function LocToAddrOrNil(ALocation: TFpDbgMemLocation): TDbgPtr; inline; // save version

function dbgs(ALocation: TFpDbgMemLocation): String; overload;

implementation

function NilLoc: TFpDbgMemLocation;
begin
  Result.Address := 0;
  Result.MType := mlfTargetMem;
end;

function InvalidLoc: TFpDbgMemLocation;
begin
  Result.Address := 0;
  Result.MType := mlfInvalid;
end;

function TargetLoc(AnAddress: TDbgPtr): TFpDbgMemLocation;
begin
  Result.Address := AnAddress;
  Result.MType := mlfTargetMem;
end;

function RegisterLoc(ARegNum: Cardinal): TFpDbgMemLocation;
begin
  Result.Address := ARegNum;
  Result.MType := mlfTargetRegister;
end;

function RegisterSignedLoc(ARegNum: Cardinal): TFpDbgMemLocation;
begin
  Result.Address := ARegNum;
  Result.MType := mlfTargetRegisterSigned;
end;

function SelfLoc(AnAddress: TDbgPtr): TFpDbgMemLocation;
begin
  Result.Address := AnAddress;
  Result.MType := mlfSelfMem;
end;

function SelfLoc(AnAddress: Pointer): TFpDbgMemLocation;
begin
  Result.Address := TDbgPtr(AnAddress);
  Result.MType := mlfSelfMem;
end;

function ConstLoc(AValue: QWord): TFpDbgMemLocation;
begin
  Result.Address := AValue;
  Result.MType := mlfConstant;
end;

function IsTargetAddr(ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := ALocation.MType = mlfTargetMem;
end;

function IsValidLoc(ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := (ALocation.MType <> mlfInvalid);
end;

function IsReadableLoc(ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := (ALocation.MType <> mlfInvalid) and
            ( (not(ALocation.MType in [mlfTargetMem, mlfSelfMem])) or
              (ALocation.Address <> 0)
            );
end;

function IsTargetNil(ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := (ALocation.MType = mlfTargetMem) and (ALocation.Address = 0);
end;

function IsTargetNotNil(ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := (ALocation.MType = mlfTargetMem) and (ALocation.Address <> 0);
end;

function LocToAddr(ALocation: TFpDbgMemLocation): TDbgPtr;
begin
  assert(ALocation.MType = mlfTargetMem, 'LocToAddr for other than mlfTargetMem');
  Result := ALocation.Address;
end;

function LocToAddrOrNil(ALocation: TFpDbgMemLocation): TDbgPtr;
begin
  if (ALocation.MType = mlfTargetMem) then
    Result := ALocation.Address
  else
    Result := 0;
end;

function dbgs(ALocation: TFpDbgMemLocation): String;
begin
  Result := '';
  if not (ALocation.MType in [low(TFpDbgMemLocationType)..high(TFpDbgMemLocationType)]) then
    Result := 'Location=out-of-range'
  else
    WriteStr(Result, 'Location=', ALocation.Address, ',', ALocation.MType)
end;

{ TFpDbgMemConvertorLittleEndian }

procedure TFpDbgMemConvertorLittleEndian.AdjustIntPointer(var ADestPointer: Pointer;
  ASourceSize, ADestSize: Cardinal);
begin
  // no adjustment needed
end;

procedure TFpDbgMemConvertorLittleEndian.SignExtend(ADataPointer: Pointer; ASourceSize,
  ADestSize: Cardinal);
begin
  Assert(ASourceSize > 0, 'TFpDbgMemConvertorLittleEndian.SignExtend');
  if ASourceSize >= ADestSize then
    exit;

  if (PByte(ADataPointer + ASourceSize - 1)^ and $80) <> 0 then
    FillByte((ADataPointer + ASourceSize)^, ADestSize-ASourceSize, $ff)
  else
    FillByte((ADataPointer + ASourceSize)^, ADestSize-ASourceSize, $00)
end;

procedure TFpDbgMemConvertorLittleEndian.UnsignSignedExtend(ADataPointer: Pointer;
  ASourceSize, ADestSize: Cardinal);
begin
  Assert(ASourceSize > 0, 'TFpDbgMemConvertorLittleEndian.SignExtend');
  if ASourceSize >= ADestSize then
    exit;

  FillByte((ADataPointer + ASourceSize)^, ADestSize-ASourceSize, $00)
end;

{ TFpDbgMemManager }

constructor TFpDbgMemManager.Create(AMemReader: TFpDbgMemReaderBase;
  AMemConvertor: TFpDbgMemConvertor);
begin
  FMemReader := AMemReader;
  FMemConvertor := AMemConvertor;
end;

function TFpDbgMemManager.ReadMemory(ALocation: TFpDbgMemLocation; ASize: Cardinal;
  ADest: Pointer): Boolean;
const
  ConstValSize = SizeOf(ALocation.Address);
var
  Addr2: Pointer;
  i: Integer;
  TmpVal: TDbgPtr;
begin
  Result := False;
  case ALocation.MType of
    mlfInvalid: ;
    mlfTargetMem:
      Result := FMemReader.ReadMemory(ALocation.Address, ASize, ADest);
    mlfSelfMem:
      begin
        move(Pointer(ALocation.Address)^, ADest^, ASize);
        Result := True;
      end;
    mlfConstant, mlfTargetRegister, mlfTargetRegisterSigned:
      begin
        case ALocation.MType of
          mlfConstant: begin
              TmpVal := ALocation.Address;
              i := ConstValSize;
            end;
          mlfTargetRegister, mlfTargetRegisterSigned: begin
              i := FMemReader.RegisterSize(Cardinal(ALocation.Address));
              if i = 0 then
                exit; // failed
              if not FMemReader.ReadRegister(Cardinal(ALocation.Address), TmpVal) then
                exit; // failed
            end;
        end;

        if ASize < i then begin
          Addr2 := @TmpVal;
          FMemConvertor.AdjustIntPointer(Addr2, ASize, i);
          move(Addr2^, ADest^, i);
        end
        else begin
          Addr2 := ADest;
          FMemConvertor.AdjustIntPointer(Addr2, i, ASize);
          PQWord(Addr2)^ := TmpVal;
          if ALocation.MType = mlfTargetRegisterSigned then
            FMemConvertor.SignExtend(ADest, i, ASize)
          else
            FMemConvertor.UnsignSignedExtend(ADest, i, ASize);
        end;
        Result := True;
      end;
  end;
end;

function TFpDbgMemManager.ReadMemoryEx(ALocation: TFpDbgMemLocation; AnAddressSpace: TDbgPtr;
  ASize: Cardinal; ADest: Pointer): Boolean;
begin
  // AnAddressSpace is ignored, except for target address
  case ALocation.MType of
    mlfTargetMem: Result := FMemReader.ReadMemoryEx(ALocation.Address, AnAddressSpace, ASize, ADest);
    else
      Result := ReadMemory(ALocation, ASize, ADest);
  end;
end;

function TFpDbgMemManager.ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr): Boolean;
begin
  Result := FMemReader.ReadRegister(ARegNum, AValue);
end;

function TFpDbgMemManager.ReadAddress(ALocation: TFpDbgMemLocation;
  ASize: Cardinal): TFpDbgMemLocation;
var
  Dest: PQWord;
begin
  Assert(ASize < SizeOf(Result.Address), 'TFpDbgMemManager.ReadAddress');
  if ASize > SizeOf(Result.Address) then begin
    Result := InvalidLoc;
    exit;
  end;

  Result.Address := 0;
  Dest := @Result.Address;
  MemConvertor.AdjustIntPointer(Dest, ASize, SizeOf(Result.Address));
  if ReadMemory(ALocation, ASize, Dest) then
    Result.MType := mlfTargetMem
  else
    Result := InvalidLoc;
end;

function TFpDbgMemManager.ReadAddressEx(ALocation: TFpDbgMemLocation; AnAddressSpace: TDbgPtr;
  ASize: Cardinal): TFpDbgMemLocation;
var
  Dest: PQWord;
begin
  Assert(ASize < SizeOf(Result.Address), 'TFpDbgMemManager.ReadAddress');
  if ASize > SizeOf(Result.Address) then begin
    Result := InvalidLoc;
    exit;
  end;

  Result.Address := 0;
  Dest := @Result.Address;
  MemConvertor.AdjustIntPointer(Dest, ASize, SizeOf(Result.Address));
  if ReadMemoryEx(ALocation, AnAddressSpace, ASize, Dest) then
    Result.MType := mlfTargetMem
  else
    Result := InvalidLoc;
end;

end.

