unit FpdMemoryTools;

{$mode objfpc}{$H+}

(* Tools to read data from Target or Own memory.

   This is to deal in one place with all conversion of data from target format
   to debugger format.

   A typical read would involve the following steps:

   The code calls MemoryManager with an Address, a Size and a space for the data.

   If the data needs to be extended (SizeOf(data) > TargetSize), then
   MemConvertor is called to decide where in rovided space the read data  should
   initially be stored.

   Then the data is read using MemReader.

   And finally MemConvertor is given a chance to extend or convert the data.

*)

interface

uses
  Classes, SysUtils, math, DbgIntfBaseTypes, FpErrorMessages;

type

  TFpDbgMemReaderBase = class
  public
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; virtual; abstract;
    function ReadMemoryEx(AnAddress, AnAddressSpace: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; virtual; abstract;
    // ReadRegister may need TargetMemConvertor
    // Register with reduced size are treated as unsigned
    function ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr): Boolean; virtual; abstract;
    function RegisterSize(ARegNum: Cardinal): Integer; virtual; abstract;
    // Registernum from name
  end;

  (* Options for all read operation // TODO *)

  TFpDbgMemReadOptions = record
    (* potential flags
         target bit start/shift (method to map dwarf2/3 bit offse to dwarf 4
         target bit size
         target assume big/little endian
         float precisson
         AddressSpace

         ( rmBigEndian, rmLittleEndian, rmUseAddressSpace, UseBitSize)
         AddressSpace, BitOffset, precission
    *)
  end;


  TFpDbgMemReadDataType = (
    rdtAddress, rdtSignedInt, rdtUnsignedInt, rdtfloat,
    rdtEnum, rdtSet
  );

  TFpDbgMemConvData = record
    NewTargetAddress: TDbgPtr;
    NewDestAddress: Pointer;
    NewReadSize: Cardinal;
    PrivData1, PrivData2: Pointer;
  end;

  // Todo, cpu/language specific operations, endianess, sign extend, float .... default int value for bool
  // convert from debugge format to debuger format and back
  // TODO: currently it assumes target and own mem are in the same format
  TFpDbgMemConvertor = class
  public
    (* PrepareTargetRead
       called before every Read operation.
       In case of reading from a bit-offset more memory may be needed, and must be allocated here
    *)
    function PrepareTargetRead(AReadDataType: TFpDbgMemReadDataType;
                               ATargetPointer: TDbgPtr; ADestPointer: Pointer;
                               ATargetSize, ADestSize: Cardinal;
                               out AConvertorData: TFpDbgMemConvData
                              ): boolean; virtual; abstract;
    function PrepareTargetRead(AReadDataType: TFpDbgMemReadDataType;
                               ATargetPointer: TDbgPtr; ADestPointer: Pointer;
                               ATargetSize, ADestSize: Cardinal;
                               AnOpts: TFpDbgMemReadOptions;
                               out AConvertorData: TFpDbgMemConvData
                              ): boolean; virtual; abstract;

    (* FinishTargetRead
       called after every Read operation.
    *)
    function FinishTargetRead(AReadDataType: TFpDbgMemReadDataType;
                              ATargetPointer: TDbgPtr; ADestPointer: Pointer;
                              ATargetSize, ADestSize: Cardinal;
                              AConvertorData: TFpDbgMemConvData
                             ): boolean; virtual; abstract;
    function FinishTargetRead(AReadDataType: TFpDbgMemReadDataType;
                              ATargetPointer: TDbgPtr; ADestPointer: Pointer;
                              ATargetSize, ADestSize: Cardinal;
                              AnOpts: TFpDbgMemReadOptions;
                              AConvertorData: TFpDbgMemConvData
                             ): boolean; virtual; abstract;

    // just to free any data
    procedure FailedTargetRead(AConvertorData: TFpDbgMemConvData); virtual; abstract;

    (* AdjustIntPointer:
       To copy a smaller int/cardinal (e.g. word) into a bigger (e.g. dword),
       adjust ADestPointer so it points to the low value part of the dest
       No conversion
    *)
    procedure AdjustIntPointer(var ADataPointer: Pointer; ADataSize, ANewSize: Cardinal); virtual; abstract;
    //(* SignExtend:
    //   Expects a signed integer value of ASourceSize bytes in the low value end
    //   of the memory (total memory ADataPointer, ADestSize)
    //   Does zero fill the memory, if no sign extend is needed
    //*)
    //procedure SignExtend(ADataPointer: Pointer; ASourceSize, ADestSize: Cardinal); virtual; abstract;
    //(* Expects an unsigned integer value of ASourceSize bytes in the low value end
    //   of the memory (total memory ADataPointer, ADestSize)
    //   Basically zero fill the memory
    //*)
    //procedure UnsignedExtend(ADataPointer: Pointer; ASourceSize, ADestSize: Cardinal); virtual; abstract;
  end;

  { TFpDbgMemConvertorLittleEndian }

  TFpDbgMemConvertorLittleEndian = class(TFpDbgMemConvertor)
  public
    function PrepareTargetRead(AReadDataType: TFpDbgMemReadDataType;
                               ATargetPointer: TDbgPtr; ADestPointer: Pointer;
                               ATargetSize, ADestSize: Cardinal;
                               out AConvertorData: TFpDbgMemConvData
                              ): boolean; override;

    function FinishTargetRead(AReadDataType: TFpDbgMemReadDataType;
                              ATargetPointer: TDbgPtr; ADestPointer: Pointer;
                              ATargetSize, ADestSize: Cardinal;
                              AConvertorData: TFpDbgMemConvData
                             ): boolean; override;
    procedure FailedTargetRead(AConvertorData: TFpDbgMemConvData); override;

    procedure AdjustIntPointer(var ADataPointer: Pointer; ADataSize, ANewSize: Cardinal); override;


    //procedure SignExtend(ADataPointer: Pointer; ASourceSize, ADestSize: Cardinal); override;
    //procedure UnsignedExtend(ADataPointer: Pointer; ASourceSize, ADestSize: Cardinal); override;
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
    mlfUninitialized := 0,   // like invalid, but not known // 0 means objet fields will start wint this
    mlfInvalid,
    mlfTargetMem,            // an address in the target (debuggee) process
    mlfSelfMem,              // an address in this(the debuggers) process memory; the data is  in TARGET format (endian, ...)
    // the below will be mapped (and extended) according to endianess
    mlfTargetRegister,       // reads from the register
    mlfConstant              // an (up to) SizeOf(TDbgPtr) (=8) Bytes Value (endian in format of debug process)
  );

  TFpDbgMemLocation = record
    Address: TDbgPtr;
    MType: TFpDbgMemLocationType;
  end;


  { TFpDbgMemManager }

  TFpDbgMemManager = class
  private
    FLastError: TFpError;
    FMemReader: TFpDbgMemReaderBase;
    FTargetMemConvertor: TFpDbgMemConvertor;
    FSelfMemConvertor: TFpDbgMemConvertor; // used when resizing constants (or register values, which are already in self format)
  protected
    function ReadMemory(AReadDataType: TFpDbgMemReadDataType;
                        const ALocation: TFpDbgMemLocation; ATargetSize: Cardinal;
                        ADest: Pointer; ADestSize: Cardinal): Boolean;
  public
    constructor Create(AMemReader: TFpDbgMemReaderBase; AMemConvertor: TFpDbgMemConvertor);
    constructor Create(AMemReader: TFpDbgMemReaderBase; ATargenMemConvertor, ASelfMemConvertor: TFpDbgMemConvertor);
    procedure ClearLastError;

    function ReadMemory(const ALocation: TFpDbgMemLocation; ASize: Cardinal; ADest: Pointer): Boolean;
    function ReadMemoryEx(const ALocation: TFpDbgMemLocation; AnAddressSpace: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean;
    function ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr): Boolean;

    // location will be invalid, if read failed
    function ReadAddress(const ALocation: TFpDbgMemLocation; ASize: Cardinal): TFpDbgMemLocation;
    function ReadAddressEx(const ALocation: TFpDbgMemLocation;  AnAddressSpace: TDbgPtr; ASize: Cardinal): TFpDbgMemLocation;

    // ALocation and AnAddress MUST NOT be the same variable on the callers side
    function ReadAddress    (const ALocation: TFpDbgMemLocation; ASize: Cardinal;
                             out AnAddress: TFpDbgMemLocation): Boolean; inline;
    //function ReadAddress    (const ALocation: TFpDbgMemLocation; ASize: Cardinal;
    //                         out AnAddress: TFpDbgMemLocation;
    //                         AnOpts: TFpDbgMemReadOptions): Boolean;
    function ReadUnsignedInt(const ALocation: TFpDbgMemLocation; ASize: Cardinal;
                             out AValue: QWord): Boolean; inline;
    //function ReadUnsignedInt(const ALocation: TFpDbgMemLocation; ASize: Cardinal;
    //                         out AValue: QWord;
    //                         AnOpts: TFpDbgMemReadOptions): Boolean;
    function ReadSignedInt  (const ALocation: TFpDbgMemLocation; ASize: Cardinal;
                             out AValue: Int64): Boolean; inline;
    //function ReadSignedInt  (const ALocation: TFpDbgMemLocation; ASize: Cardinal;
    //                         out AValue: Int64;
    //                         AnOpts: TFpDbgMemReadOptions): Boolean;
    // //enum/set: may need bitorder swapped
    function ReadEnum       (const ALocation: TFpDbgMemLocation; ASize: Cardinal;
                             out AValue: QWord): Boolean; inline;
    //function ReadEnum       (const ALocation: TFpDbgMemLocation; ASize: Cardinal;
    //                         out AValue: QWord;
    //                         AnOpts: TFpDbgMemReadOptions): Boolean;
    function ReadSet        (const ALocation: TFpDbgMemLocation; ASize: Cardinal;
                             out AValue: TBytes): Boolean; inline;
    //function ReadSet        (const ALocation: TFpDbgMemLocation; ASize: Cardinal;
    //                         out AValue: TBytes;
    //                         AnOpts: TFpDbgMemReadOptions): Boolean;
    function ReadFloat      (const ALocation: TFpDbgMemLocation; ASize: Cardinal;
                             out AValue: Extended): Boolean; inline;
    //function ReadFloat      (const ALocation: TFpDbgMemLocation; ASize: Cardinal;
    //                         out AValue: Extended;
    //                         AnOpts: TFpDbgMemReadOptions): Boolean;

    property TargetMemConvertor: TFpDbgMemConvertor read FTargetMemConvertor;
    property SelfMemConvertor: TFpDbgMemConvertor read FSelfMemConvertor;
    property LastError: TFpError read FLastError;
  end;

function NilLoc: TFpDbgMemLocation; inline;
function InvalidLoc: TFpDbgMemLocation; inline;
function UnInitializedLoc: TFpDbgMemLocation; inline;
function TargetLoc(AnAddress: TDbgPtr): TFpDbgMemLocation; inline;
function RegisterLoc(ARegNum: Cardinal): TFpDbgMemLocation; inline;
function SelfLoc(AnAddress: TDbgPtr): TFpDbgMemLocation; inline;
function SelfLoc(AnAddress: Pointer): TFpDbgMemLocation; inline;
function ConstLoc(AValue: QWord): TFpDbgMemLocation; inline;

function IsTargetAddr(ALocation: TFpDbgMemLocation): Boolean; inline;
function IsInitializedLoc(ALocation: TFpDbgMemLocation): Boolean; inline;
function IsValidLoc(ALocation: TFpDbgMemLocation): Boolean; inline;     // Valid, Nil allowed
function IsReadableLoc(ALocation: TFpDbgMemLocation): Boolean; inline;  // Valid and not Nil // can be const or reg
function IsReadableMem(ALocation: TFpDbgMemLocation): Boolean; inline;  // Valid and target or sel <> nil
function IsTargetNil(ALocation: TFpDbgMemLocation): Boolean; inline;    // valid targed = nil
function IsTargetNotNil(ALocation: TFpDbgMemLocation): Boolean; inline; // valid targed <> nil

function LocToAddr(ALocation: TFpDbgMemLocation): TDbgPtr; inline;      // does not check valid
function LocToAddrOrNil(ALocation: TFpDbgMemLocation): TDbgPtr; inline; // save version

function EmptyMemReadOpts:TFpDbgMemReadOptions;

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

function UnInitializedLoc: TFpDbgMemLocation;
begin
  Result.Address := 0;
  Result.MType := mlfUninitialized;
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

function IsInitializedLoc(ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := ALocation.MType <> mlfUninitialized;
end;

function IsValidLoc(ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := not(ALocation.MType in [mlfInvalid, mlfUninitialized]);
end;

function IsReadableLoc(ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := (not(ALocation.MType in [mlfInvalid, mlfUninitialized])) and
            ( (not(ALocation.MType in [mlfTargetMem, mlfSelfMem])) or
              (ALocation.Address <> 0)
            );
end;

function IsReadableMem(ALocation: TFpDbgMemLocation): Boolean;
begin
  Result := (ALocation.MType in [mlfTargetMem, mlfSelfMem]) and
            (ALocation.Address <> 0);
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

function EmptyMemReadOpts: TFpDbgMemReadOptions;
begin
  //
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

function TFpDbgMemConvertorLittleEndian.PrepareTargetRead(AReadDataType: TFpDbgMemReadDataType;
  ATargetPointer: TDbgPtr; ADestPointer: Pointer; ATargetSize, ADestSize: Cardinal; out
  AConvertorData: TFpDbgMemConvData): boolean;
begin
  Result := ATargetSize <= ADestSize;
  if not Result then
    exit;
  // just read to begin of data
  AConvertorData.NewTargetAddress := ATargetPointer;
  AConvertorData.NewDestAddress   := ADestPointer;
  AConvertorData.NewReadSize      := Min(ATargetSize, ADestSize);
  case AReadDataType of
    rdtAddress, rdtSignedInt, rdtUnsignedInt,
    rdtEnum, rdtSet: ;
    rdtfloat:
      Result := (ATargetSize = AConvertorData.NewReadSize) and
                (ADestSize = SizeOf(Extended)) and // only can read to extended... TODO (if need more)
                ( (ATargetSize = SizeOf(Extended)) or
                  (ATargetSize = SizeOf(Double)) or
                  (ATargetSize = SizeOf(Single)) or
                  (ATargetSize = SizeOf(real48))
                )
    else begin
      Assert(False, 'TFpDbgMemConvertorLittleEndian.PrepareTargetRead');
      Result := False;
    end;
  end;
end;

function TFpDbgMemConvertorLittleEndian.FinishTargetRead(AReadDataType: TFpDbgMemReadDataType;
  ATargetPointer: TDbgPtr; ADestPointer: Pointer; ATargetSize, ADestSize: Cardinal;
  AConvertorData: TFpDbgMemConvData): boolean;
type
  Preal48 = ^real48;
begin
  Result := True;
  case AReadDataType of
    rdtAddress, rdtUnsignedInt, rdtEnum, rdtSet: begin
        if ATargetSize < ADestSize then
          FillByte((ADestPointer + ATargetSize)^, ADestSize-ATargetSize, $00)
      end;
    rdtSignedInt: begin
        if ATargetSize < ADestSize then
          if (ATargetSize > 0) and ((PByte(ADestPointer + ATargetSize - 1)^ and $80) <> 0)
          then
            FillByte((ADestPointer + ATargetSize)^, ADestSize-ATargetSize, $FF)
          else
            FillByte((ADestPointer + ATargetSize)^, ADestSize-ATargetSize, $00);
      end;
    rdtfloat: begin
      assert((ADestSize = SizeOf(Extended)));
      if (ATargetSize = SizeOf(Extended)) then
        //
      else
      if (ATargetSize = SizeOf(Double)) then
        PExtended(ADestPointer)^ := PDouble(ADestPointer)^
      else
      if (ATargetSize = SizeOf(real48)) then
        PExtended(ADestPointer)^ := Preal48(ADestPointer)^
      else
      if (ATargetSize = SizeOf(Single)) then
        PExtended(ADestPointer)^ := PSingle(ADestPointer)^
      else
        Result := False;
    end;
    else begin
      Assert(False, 'TFpDbgMemConvertorLittleEndian.FailedTargetRead');
      Result := False;
    end;
  end;
end;

procedure TFpDbgMemConvertorLittleEndian.FailedTargetRead(AConvertorData: TFpDbgMemConvData);
begin
  //
end;

procedure TFpDbgMemConvertorLittleEndian.AdjustIntPointer(var ADataPointer: Pointer;
  ADataSize, ANewSize: Cardinal);
begin
  Assert(ANewSize <= ADataSize, 'TFpDbgMemConvertorLittleEndian.AdjustIntPointer');
  // no adjustment needed
end;

//procedure TFpDbgMemConvertorLittleEndian.SignExtend(ADataPointer: Pointer; ASourceSize,
//  ADestSize: Cardinal);
//begin
//  Assert(ASourceSize > 0, 'TFpDbgMemConvertorLittleEndian.SignExtend');
//  if ASourceSize >= ADestSize then
//    exit;
//
//  if (PByte(ADataPointer + ASourceSize - 1)^ and $80) <> 0 then
//    FillByte((ADataPointer + ASourceSize)^, ADestSize-ASourceSize, $ff)
//  else
//    FillByte((ADataPointer + ASourceSize)^, ADestSize-ASourceSize, $00)
//end;
//
//procedure TFpDbgMemConvertorLittleEndian.UnsignedExtend(ADataPointer: Pointer;
//  ASourceSize, ADestSize: Cardinal);
//begin
//  Assert(ASourceSize > 0, 'TFpDbgMemConvertorLittleEndian.SignExtend');
//  if ASourceSize >= ADestSize then
//    exit;
//
//  FillByte((ADataPointer + ASourceSize)^, ADestSize-ASourceSize, $00)
//end;

{ TFpDbgMemManager }

function TFpDbgMemManager.ReadMemory(AReadDataType: TFpDbgMemReadDataType;
  const ALocation: TFpDbgMemLocation; ATargetSize: Cardinal; ADest: Pointer;
  ADestSize: Cardinal): Boolean;
var
  Addr2: Pointer;
  i: Integer;
  TmpVal: TDbgPtr;
  ConvData: TFpDbgMemConvData;
begin
  FLastError := NoError;
  Result := False;
  case ALocation.MType of
    mlfInvalid, mlfUninitialized:
      FLastError := CreateError(fpErrCanNotReadInvalidMem);
    mlfTargetMem, mlfSelfMem: begin
      Result := TargetMemConvertor.PrepareTargetRead(AReadDataType, ALocation.Address,
        ADest, ATargetSize, ADestSize, ConvData);
      if not Result then exit;

      if ALocation.MType = mlfTargetMem then begin
        Result := FMemReader.ReadMemory(ConvData.NewTargetAddress, ConvData.NewReadSize, ConvData.NewDestAddress);
        if not Result then
          FLastError := CreateError(fpErrCanNotReadMemAtAddr, [ALocation.Address]);
      end
      else
      begin
        try
          move(Pointer(ConvData.NewTargetAddress)^, ConvData.NewDestAddress^, ConvData.NewReadSize);
          Result := True;
        except
          Result := False;
        end;
      end;

      if Result then
        Result := TargetMemConvertor.FinishTargetRead(AReadDataType, ALocation.Address,
          ADest, ATargetSize, ADestSize, ConvData)
      else
        TargetMemConvertor.FailedTargetRead(ConvData);
    end;
    mlfConstant, mlfTargetRegister:
      begin
        case ALocation.MType of
          mlfConstant: begin
              TmpVal := ALocation.Address;
              i := SizeOf(ALocation.Address);
            end;
          mlfTargetRegister: begin
              i := FMemReader.RegisterSize(Cardinal(ALocation.Address));
              if i = 0 then
                exit; // failed
              if not FMemReader.ReadRegister(Cardinal(ALocation.Address), TmpVal) then
                exit; // failed
            end;
        end;
        if i > ATargetSize then
          i := ATargetSize;

        Addr2 := @TmpVal;
        if SizeOf(TmpVal) <> i then
          FSelfMemConvertor.AdjustIntPointer(Addr2, SizeOf(TmpVal), i);

        Result := FSelfMemConvertor.PrepareTargetRead(AReadDataType, TDbgPtr(Addr2),
          ADest, i, ADestSize, ConvData);
        if not Result then exit;

        move(Pointer(ConvData.NewTargetAddress)^, ConvData.NewDestAddress^, ConvData.NewReadSize);

        Result := TargetMemConvertor.FinishTargetRead(AReadDataType, TDbgPtr(Addr2),
          ADest, i, ADestSize, ConvData);
        Result := True;
      end;
  end;
  if (not Result) and (not IsError(FLastError)) then
    FLastError := CreateError(fpErrFailedReadMem);
end;

constructor TFpDbgMemManager.Create(AMemReader: TFpDbgMemReaderBase;
  AMemConvertor: TFpDbgMemConvertor);
begin
  FMemReader := AMemReader;
  FTargetMemConvertor := AMemConvertor;
  FSelfMemConvertor := AMemConvertor;
end;

constructor TFpDbgMemManager.Create(AMemReader: TFpDbgMemReaderBase; ATargenMemConvertor,
  ASelfMemConvertor: TFpDbgMemConvertor);
begin
  FMemReader := AMemReader;
  FTargetMemConvertor := ATargenMemConvertor;
  FSelfMemConvertor := ASelfMemConvertor;
end;

procedure TFpDbgMemManager.ClearLastError;
begin
  FLastError := NoError;
end;

function TFpDbgMemManager.ReadMemory(const ALocation: TFpDbgMemLocation; ASize: Cardinal;
  ADest: Pointer): Boolean;
var
  Addr2: Pointer;
  i: Integer;
  TmpVal: TDbgPtr;
  ConvData: TFpDbgMemConvData;
begin
  FLastError := NoError;
  Result := False;
  case ALocation.MType of
    mlfInvalid, mlfUninitialized: ;
    mlfTargetMem:
      begin
        Result := FMemReader.ReadMemory(ALocation.Address, ASize, ADest);
        if not Result then
          FLastError := CreateError(fpErrCanNotReadMemAtAddr, [ALocation.Address]);
      end;
    mlfSelfMem:
      begin
        move(Pointer(ALocation.Address)^, ADest^, ASize);
        Result := True;
      end;
    mlfConstant, mlfTargetRegister:
      begin
        case ALocation.MType of
          mlfConstant: begin
              TmpVal := ALocation.Address;
              i := SizeOf(ALocation.Address);
            end;
          mlfTargetRegister: begin
              i := FMemReader.RegisterSize(Cardinal(ALocation.Address));
              if i = 0 then
                exit; // failed
              if not FMemReader.ReadRegister(Cardinal(ALocation.Address), TmpVal) then
                exit; // failed
            end;
        end;

        Addr2 := @TmpVal;
        if SizeOf(TmpVal) <> i then
          FSelfMemConvertor.AdjustIntPointer(Addr2, SizeOf(TmpVal), i);

        Result := FSelfMemConvertor.PrepareTargetRead(rdtUnsignedInt, TDbgPtr(Addr2),
          ADest, i, ASize, ConvData);
        if not Result then exit;

        move(Pointer(ConvData.NewTargetAddress)^, ConvData.NewDestAddress^, ConvData.NewReadSize);

        Result := TargetMemConvertor.FinishTargetRead(rdtUnsignedInt, TDbgPtr(Addr2),
          ADest, i, ASize, ConvData);
        Result := True;
      end;
  end;
  if (not Result) and (not IsError(FLastError)) then
    FLastError := CreateError(fpErrFailedReadMem);
end;

function TFpDbgMemManager.ReadMemoryEx(const ALocation: TFpDbgMemLocation;
  AnAddressSpace: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean;
begin
  FLastError := NoError;
  // AnAddressSpace is ignored, when not actually reading from target address
  case ALocation.MType of
    mlfTargetMem: Result := FMemReader.ReadMemoryEx(ALocation.Address, AnAddressSpace, ASize, ADest);
    else
      Result := ReadMemory(ALocation, ASize, ADest);
  end;
  if (not Result) and (not IsError(FLastError)) then
    FLastError := CreateError(fpErrFailedReadMem);
end;

function TFpDbgMemManager.ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr): Boolean;
begin
  FLastError := NoError;
  Result := FMemReader.ReadRegister(ARegNum, AValue);
end;

function TFpDbgMemManager.ReadAddress(const ALocation: TFpDbgMemLocation;
  ASize: Cardinal): TFpDbgMemLocation;
begin
  Result.MType := mlfTargetMem;
  if not ReadMemory(rdtAddress, ALocation, ASize, @Result.Address, SizeOf(Result.Address)) then
    Result := InvalidLoc;
end;

function TFpDbgMemManager.ReadAddressEx(const ALocation: TFpDbgMemLocation;
  AnAddressSpace: TDbgPtr; ASize: Cardinal): TFpDbgMemLocation;
begin
  Result := InvalidLoc;
end;

function TFpDbgMemManager.ReadAddress(const ALocation: TFpDbgMemLocation; ASize: Cardinal; out
  AnAddress: TFpDbgMemLocation): Boolean;
begin
  Result := ReadMemory(rdtAddress, ALocation, ASize, @AnAddress, SizeOf(AnAddress));
  if Result
  then AnAddress.MType := mlfTargetMem
  else AnAddress.MType := mlfInvalid;
end;

function TFpDbgMemManager.ReadUnsignedInt(const ALocation: TFpDbgMemLocation; ASize: Cardinal;
  out AValue: QWord): Boolean;
begin
  Result := ReadMemory(rdtUnsignedInt, ALocation, ASize, @AValue, SizeOf(AValue));
end;

function TFpDbgMemManager.ReadSignedInt(const ALocation: TFpDbgMemLocation; ASize: Cardinal;
  out AValue: Int64): Boolean;
begin
  Result := ReadMemory(rdtSignedInt, ALocation, ASize, @AValue, SizeOf(AValue));
end;

function TFpDbgMemManager.ReadEnum(const ALocation: TFpDbgMemLocation; ASize: Cardinal; out
  AValue: QWord): Boolean;
begin
  Result := ReadMemory(rdtEnum, ALocation, ASize, @AValue, SizeOf(AValue));
end;

function TFpDbgMemManager.ReadSet(const ALocation: TFpDbgMemLocation; ASize: Cardinal; out
  AValue: TBytes): Boolean;
begin
  SetLength(AValue, ASize);
  Result := ASize > 0;
  if Result then
    Result := ReadMemory(rdtSet, ALocation, ASize, @AValue[0], ASize);
end;

function TFpDbgMemManager.ReadFloat(const ALocation: TFpDbgMemLocation; ASize: Cardinal; out
  AValue: Extended): Boolean;
begin
  Result := ReadMemory(rdtfloat, ALocation, ASize, @AValue, SizeOf(AValue));
end;

end.

