unit FpDbgCpuAarch64;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpDbgClasses, FpdMemoryTools, LazClasses, LazDebuggerIntfBaseTypes;

type

  { TDbgAarch64StackUnwinder }

  TDbgAarch64StackUnwinder = class(TDbgStackUnwinderEx)
  public
    procedure InitForFrame(ACurrentFrame: TDbgCallstackEntry;
                           out CodePointer, StackPointer, FrameBasePointer: TDBGPtr); override;
    // AFrameIndex: The frame-index to be read. Starts at 1 (since 0 is top-lever, and handled by GetTopFrame)
    function Unwind(AFrameIndex: integer;
                    var CodePointer, StackPointer, FrameBasePointer: TDBGPtr;
                    ACurrentFrame: TDbgCallstackEntry; // nil for top frame
                    out ANewFrame: TDbgCallstackEntry
                   ): TTDbgStackUnwindResult; override;
  end;

  { TAarch64DbgAsmInstruction }

  TAarch64DbgAsmInstruction = class(TDbgAsmInstruction)
  private
    FIsCallInstruction: boolean;
    FIsReturnInstruction: boolean;
  public
    constructor Create(AProcess: TDbgProcess);
    function IsCallInstruction: boolean; override;
    function IsReturnInstruction: boolean; override;
//    function IsJumpInstruction(IncludeConditional: Boolean = True; IncludeUncoditional: Boolean = True): boolean; override;
    function InstructionLength: Integer; override;
  end;

  { TAarch64AsmDecoder }

  TAarch64AsmDecoder = class(TDbgAsmDecoder)
  private
    FProcess: TDbgProcess;
    FLastInstr: TAarch64DbgAsmInstruction;
  protected
    //function GetLastErrorWasMemReadErr: Boolean; override;
    function GetMaxInstrSize: integer; override;
    function GetMinInstrSize: integer; override;
    //function GetCanReverseDisassemble: boolean; override;
  public
    constructor Create(AProcess: TDbgProcess); override;
    destructor Destroy; override;

    function GetInstructionInfo(AnAddress: TDBGPtr): TDbgAsmInstruction; override;
    function GetFrameBoundaryInfo(AnAddress: TDBGPtr; out
      AFrameBoundaryInfo: TDbgFrameBoundaryInfo; ARoutineStartAddr: TDBGPtr = 0
      ): TDbgFrameBoundaryKind; override;
    function GetFunctionFrameInfo(AnAddress: TDBGPtr; out AnIsOutsideFrame: Boolean): Boolean; override;
    function IsAfterCallInstruction(AnAddress: TDBGPtr): boolean; override;
    procedure Disassemble(var AnAddress: Pointer; out ACodeBytes: String; out ACode: String); override; overload;
  end;

  //TBreakPointAarch64Storage = array[0..3] of byte;
  TBreakPointAarch64Storage = cardinal;
  TBreakInfoAarch64 = object
  const
    {$IFDEF MSWINDOWS}
    _CODE: TBreakPointAarch64Storage = $D43E0000;
    {$ELSE}
    _CODE: TBreakPointAarch64Storage = $D4200000;
    {$ENDIF}
  end;

  TBreakPointAarch64Handler = specialize TGenericBreakPointTargetHandler<TBreakPointAarch64Storage, TBreakInfoAarch64>;


implementation

{ TDbgAarch64StackUnwinder }

procedure TDbgAarch64StackUnwinder.InitForFrame(ACurrentFrame: TDbgCallstackEntry; out
  CodePointer, StackPointer, FrameBasePointer: TDBGPtr);
var
  R: TDbgRegisterValue;
begin
    CodePointer      := ACurrentFrame.AnAddress;
    FrameBasePointer := ACurrentFrame.FrameAdress;
    StackPointer     := 0;
    //R := ACurrentFrame.RegisterValueList.FindRegisterByDwarfIndex(29);
    //if R <> nil then
    //  FrameBasePointer := R.NumValue;
    R := ACurrentFrame.RegisterValueList.FindRegisterByDwarfIndex(31);
    if R = nil then exit;
    StackPointer := R.NumValue;
end;

function TDbgAarch64StackUnwinder.Unwind(AFrameIndex: integer; var CodePointer, StackPointer,
  FrameBasePointer: TDBGPtr; ACurrentFrame: TDbgCallstackEntry; out ANewFrame: TDbgCallstackEntry
  ): TTDbgStackUnwindResult;
var
  OutSideFrame: Boolean;
  X30: TDbgRegisterValue;
  NewLink, NewFrameBase: TDbgPtr;
begin
  Result := suFailed;
  if StackPointer = 0 then
    exit;


  if Process.Disassembler.GetFunctionFrameInfo(CodePointer, OutSideFrame) and OutSideFrame then begin
    // TODO, if we are half in...
    X30 := ACurrentFrame.RegisterValueList.FindRegisterByDwarfIndex(30);
    if X30 = nil then
      exit;
    CodePointer := X30.NumValue;

    ANewFrame := TDbgCallstackEntry.Create(Thread, AFrameIndex, FrameBasePointer, CodePointer);
    ANewFrame.RegisterValueList.Assign(ACurrentFrame.RegisterValueList);
    ANewFrame.RegisterValueList.DbgRegisterAutoCreate['PC'].SetValue(CodePointer, IntToStr(CodePointer),8, 32);

    Result := suSuccess;
    exit;
  end;

  if not Process.ReadData(FrameBasePointer + 8, 8, NewLink) then
    exit;
  if not Process.ReadData(FrameBasePointer, 8, NewFrameBase) then
    exit;
  if NewFrameBase <= FrameBasePointer then
    exit;

  StackPointer := 0;
  if NewFrameBase <> 0 then
    StackPointer := FrameBasePointer + 16;

  FrameBasePointer := NewFrameBase;
  CodePointer := NewLink;

  ANewFrame := TDbgCallstackEntry.Create(Thread, AFrameIndex, NewFrameBase, CodePointer);
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate['X29'].SetValue(NewFrameBase, IntToStr(NewFrameBase),8, 29);
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate['X30'].SetValue(NewLink, IntToStr(NewLink),8, 30);
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate['SP'].SetValue(StackPointer, IntToStr(StackPointer),8, 31);
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate['PC'].SetValue(CodePointer, IntToStr(CodePointer),8, 32);

  Result := suSuccess;
end;

{ TAarch64DbgAsmInstruction }

constructor TAarch64DbgAsmInstruction.Create(AProcess: TDbgProcess);
begin
  inherited Create;
  AddReference;
end;

function TAarch64DbgAsmInstruction.IsCallInstruction: boolean;
begin
  Result := FIsCallInstruction;
end;

function TAarch64DbgAsmInstruction.IsReturnInstruction: boolean;
begin
  Result := FIsReturnInstruction;
end;

function TAarch64DbgAsmInstruction.InstructionLength: Integer;
begin
  Result := 4;
end;

{ TAarch64AsmDecoder }

function TAarch64AsmDecoder.GetMaxInstrSize: integer;
begin
  result := 4;
end;

function TAarch64AsmDecoder.GetMinInstrSize: integer;
begin
  result := 4;
end;

constructor TAarch64AsmDecoder.Create(AProcess: TDbgProcess);
begin
  FProcess := AProcess;
end;

destructor TAarch64AsmDecoder.Destroy;
begin
  ReleaseRefAndNil(FLastInstr);
  inherited Destroy;
end;

function TAarch64AsmDecoder.GetInstructionInfo(AnAddress: TDBGPtr): TDbgAsmInstruction;
var
  CodeBin: Cardinal;
begin
  if (FLastInstr = nil) or (FLastInstr.RefCount > 1) then begin
    ReleaseRefAndNil(FLastInstr);
    FLastInstr := TAarch64DbgAsmInstruction.Create(FProcess);
  end;

  FLastInstr.FIsCallInstruction   := False;
  FLastInstr.FIsReturnInstruction := False;
  Result := FLastInstr;

  if not FProcess.ReadData(AnAddress, 4, CodeBin) then
    exit;

  FLastInstr.FIsReturnInstruction := CodeBin = $D65F03C0;
  FLastInstr.FIsCallInstruction   := ((CodeBin and $FC000000) = $94000000)   // BL
                                  or ((CodeBin and $FFFFFC1F) = $D63F0000);  // BLR
end;

function TAarch64AsmDecoder.GetFrameBoundaryInfo(AnAddress: TDBGPtr; out
  AFrameBoundaryInfo: TDbgFrameBoundaryInfo; ARoutineStartAddr: TDBGPtr): TDbgFrameBoundaryKind;
var
  CodeBin: Cardinal;
begin
  Result := inherited GetFrameBoundaryInfo(AnAddress, AFrameBoundaryInfo, ARoutineStartAddr);

  if not FProcess.ReadData(AnAddress, 4, CodeBin) then
    exit;

(*
+ fd7b bfa9                 stp             x29, x30, [sp, #-16]!
+ fd03 0091                 mov             x29, sp
- f34f bfa9                 stp             x19, x19, [sp, #-16]!
- ffc3 0cd1                 sub             sp, sp, #0x330
*)

(*
- ffc3 0c91                 add             sp, sp, #0x330
- f307 41f8                 ldr             x19, [sp], #16
- fd7b c1a8                 ldp             x29, x30, [sp], #16
+ c003 5fd6                 ret
*)

  Result := bkInBody;

  if (CodeBin = $a9bf7bfd  ) then    //            stp             x29, x30, [sp, #-16]!
    Result := bkBeforePrologue;
  if (CodeBin = $910003fd  ) then    //            mov             x29, sp
    Result := bkInPrologue;
  if (CodeBin = $a9bf4ff3  ) then    //            stp             x19, x19, [sp, #-16]!
    Result := bkInPrologue;
  if (CodeBin = $d10cc3ff  ) then    //            sub             sp, sp, #0x330
    Result := bkInPrologue;

  if (CodeBin = $910cc3ff  ) then    //            add             sp, sp, #0x330
    Result := bkInEpilogue;
  if (CodeBin = $f84107f3  ) then    //            ldr             x19, [sp], #16
    Result := bkInEpilogue;
  if (CodeBin = $a8c17bfd  ) then    //            ldp             x29, x30, [sp], #16
    Result := bkInEpilogue;
  if (CodeBin = $d65f03c0  ) then    //            ret
    Result := bkAfterEpiloge;
end;

function TAarch64AsmDecoder.GetFunctionFrameInfo(AnAddress: TDBGPtr; out AnIsOutsideFrame: Boolean
  ): Boolean;
var
  CodeBin: Cardinal;
begin
  AnIsOutsideFrame := False;
  Result := False;

  if not FProcess.ReadData(AnAddress, 4, CodeBin) then
    exit;

(*
+ fd7b bfa9                 stp             x29, x30, [sp, #-16]!
+ fd03 0091                 mov             x29, sp
- f34f bfa9                 stp             x19, x19, [sp, #-16]!
- ffc3 0cd1                 sub             sp, sp, #0x330
...
- ffc3 0c91                 add             sp, sp, #0x330
- f307 41f8                 ldr             x19, [sp], #16
- fd7b c1a8                 ldp             x29, x30, [sp], #16
+ c003 5fd6                 ret
*)

  Result := True;
  if (CodeBin = $A9BF7BFD) or  // stp             x29, x30, [sp, #-16]!
     (CodeBin = $910003FD) or  // mov             x29, sp
     (CodeBin = $D65F03C0)     // ret
  then
    AnIsOutsideFrame := True;
end;

function TAarch64AsmDecoder.IsAfterCallInstruction(AnAddress: TDBGPtr): boolean;
begin
  Result := False;
end;

procedure TAarch64AsmDecoder.Disassemble(var AnAddress: Pointer; out ACodeBytes: String; out
  ACode: String);
begin
  ACode := '?';
  ACodeBytes :=
      IntToHex(PByte(AnAddress)[0], 2)
    + IntToHex(PByte(AnAddress)[1], 2)
    + IntToHex(PByte(AnAddress)[2], 2)
    + IntToHex(PByte(AnAddress)[3], 2);
  inc(AnAddress, 4);
end;


end.

