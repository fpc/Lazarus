unit FpDbgLinuxAarch64Classes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix,
  {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif},
  LazDebuggerIntfBaseTypes, FpDbgClasses, FpDbgCommon, FpDbgLinuxClasses, FpDbgLinuxExtra,
  FpdMemoryTools, LazClasses;

type

  TAarch64LinuxGeneralRegisters = record
    Regs: array [0..30] of QWord; // x0 to x30
    SP: QWord;
    PC: QWord;
    State: QWord;
  end;

  { TDbgLinuxAarch64Thread }

  TDbgLinuxAarch64Thread = class(TDbgLinuxThread)
  private
    FUserRegs, FStoredUserRegs: TAarch64LinuxGeneralRegisters;
    FHasThreadState: boolean;
    FUserRegsChanged: boolean;

    FUnwinder: TDbgStackUnwinder;

    function ReadThreadState: boolean;
  protected
    procedure ResetPauseStates; override;
    function GetInstructionPointerForHasBreakpointInfoForAddress: TDBGPtr; override;

  public
    destructor Destroy; override;
    function GetStackUnwinder: TDbgStackUnwinder; override;

    procedure BeforeContinue; override;

    procedure ApplyWatchPoints(AWatchPointData: TFpWatchPointData); override;
    function DetectHardwareWatchpoint: TFpInternalWatchpoint; override;

    procedure LoadRegisterValues; override;
    procedure SetRegisterValue(AName: string; AValue: QWord); override;
    procedure StoreRegisters; override;
    procedure RestoreRegisters; override;

    function ResetInstructionPointerAfterBreakpoint: boolean; override;
    function GetInstructionPointerRegisterValue: TDBGPtr; override;
    function GetStackBasePointerRegisterValue: TDbgPtr; override;
    function GetStackPointerRegisterValue: TDbgPtr; override;
    procedure SetInstructionPointerRegisterValue(AValue: TDbgPtr); override;
    procedure SetStackPointerRegisterValue(AValue: TDbgPtr); override;
  end;

  { TDbgLinuxAarch64Process }
  TDbgLinuxAarch64Process = class(TDbgLinuxProcess)
  protected
    function CreateBreakPointTargetHandler: TFpBreakPointTargetHandler; override;
    function DbgThreadClass: TDbgThreadClass; override;
  public
    class function isSupported(ATargetInfo: TTargetDescriptor): boolean; override;
    function CallParamDefaultLocation(AParamIdx: Integer): TFpDbgMemLocation; override;
  end;


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


implementation
var
  DBG_VERBOSE, DBG_WARNINGS, FPDBG_LINUX: PLazLoggerLogGroup;

const
  NT_PRSTATUS = 1;

type
  //TBreakPointAarch64Storage = array[0..3] of byte;
  TBreakPointAarch64Storage = cardinal;
  TBreakInfoX86 = object
  const
    //_CODE: TBreakPointAarch64Storage = ($00, $00, $20, $d4);
    //_CODE: TBreakPointAarch64Storage = $000020d4;
    _CODE: TBreakPointAarch64Storage = $D4200000;
  end;

type
  TBreakPointAarch64Handler = specialize TGenericBreakPointTargetHandler<TBreakPointAarch64Storage, TBreakInfoX86>;

{ TDbgLinuxAarch64Thread }

function TDbgLinuxAarch64Thread.ReadThreadState: boolean;
var
  io: iovec;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxAarch64Thread.ReadThreadState');{$ENDIF}
  assert(IsPaused, 'TDbgLinuxAarch64Thread.ReadThreadState: FIsPaused');

  result := true;
  if FHasThreadState then
    exit;
//  FFpRegsAvail:=False;
  io.iov_base:=@(FUserRegs.Regs[0]);
  io.iov_len:= sizeof(FUserRegs);
  if fpPTrace(PTRACE_GETREGSET, ID, pointer(PtrUInt(NT_PRSTATUS)), @io) <> 0 then
    begin
    DebugLn(DBG_WARNINGS, 'Failed to read thread registers from threadid '+inttostr(ID)+'. Errcode: '+inttostr(fpgeterrno));
    result := false;
    end;
  FHasThreadState := Result;
end;

procedure TDbgLinuxAarch64Thread.ResetPauseStates;
begin
  inherited ResetPauseStates;
  FHasThreadState := false;
end;

function TDbgLinuxAarch64Thread.GetInstructionPointerForHasBreakpointInfoForAddress: TDBGPtr;
begin
  Result := GetInstructionPointerRegisterValue;
end;

function TDbgLinuxAarch64Thread.GetStackUnwinder: TDbgStackUnwinder;
begin
  if FUnwinder = nil then
    FUnwinder := TDbgAarch64StackUnwinder.Create(Process);
  Result := FUnwinder;
end;

destructor TDbgLinuxAarch64Thread.Destroy;
begin
  inherited Destroy;
  FUnwinder.Free;
end;

procedure TDbgLinuxAarch64Thread.ApplyWatchPoints(AWatchPointData: TFpWatchPointData);
begin
  //
end;

function TDbgLinuxAarch64Thread.DetectHardwareWatchpoint: TFpInternalWatchpoint;
begin
  Result := nil;
end;

procedure TDbgLinuxAarch64Thread.LoadRegisterValues;
var
  i: integer;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxAarch64Thread.LoadRegisterValues');{$ENDIF}
  assert(IsPaused, 'TDbgLinuxAarch64Thread.LoadRegisterValues: FIsPaused');

  if not ReadThreadState then
    exit;

  for i := 0 to 30 do
    FRegisterValueList.DbgRegisterAutoCreate['r'+inttostr(i)].SetValue(FUserRegs.Regs[i], IntToStr(FUserRegs.Regs[i]), 8, i);

  FRegisterValueList.DbgRegisterAutoCreate['SP'].SetValue(FUserRegs.SP, IntToHex(FUserRegs.SP, 16), 8, 31);
  FRegisterValueList.DbgRegisterAutoCreate['PC'].SetValue(FUserRegs.PC, IntToHex(FUserRegs.PC, 16), 8, 32); // no dwarf idx

  FRegisterValueListValid:=true;
end;

procedure TDbgLinuxAarch64Thread.SetRegisterValue(AName: string; AValue: QWord);
var
  i: integer;
begin
  AName := LowerCase(AName);
  if (AName <> '') and (AName[1] = 'x') then begin
    delete(AName, 1, 1);
    i := StrToIntDef(AName, -1);
    if (i >= 0) and (i <= 30) then
      FUserRegs.Regs[i] := AValue
    else
      raise Exception.CreateFmt('Setting the [%s] register is not supported', [AName]);
  end
  else
  case AName of
    'sp': FUserRegs.SP := AValue;
    'pc': FUserRegs.PC := AValue;
    else raise Exception.CreateFmt('Setting the [%s] register is not supported', [AName]);
  end;
  FUserRegsChanged:=true;
end;

procedure TDbgLinuxAarch64Thread.StoreRegisters;
begin
  Assert(FHasThreadState);
  FStoredUserRegs := FUserRegs;
end;

procedure TDbgLinuxAarch64Thread.RestoreRegisters;
begin
  FUserRegs:=FStoredUserRegs;
  FUserRegsChanged := true;
  FRegisterValueListValid := False;
end;

procedure TDbgLinuxAarch64Thread.BeforeContinue;
var
  io: iovec;
begin
  if not IsPaused then
    exit;

  inherited;
  //if Process.CurrentWatchpoint <> nil then
  //  WriteDebugReg(6, 0);

  if FUserRegsChanged then
    begin
    io.iov_base:=@(FUserRegs.regs[0]);
    io.iov_len:= sizeof(FUserRegs);

    if fpPTrace(PTRACE_SETREGSET, ID, pointer(PtrUInt(NT_PRSTATUS)), @io) <> 0 then
      begin
      DebugLn(DBG_WARNINGS, 'Failed to set thread registers. Errcode: '+inttostr(fpgeterrno));
      end;
    FUserRegsChanged:=false;
    end;
end;

function TDbgLinuxAarch64Thread.ResetInstructionPointerAfterBreakpoint: boolean;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxAarch64Thread.ResetInstructionPointerAfterBreakpoint');{$ENDIF}
  assert(IsPaused, 'TDbgLinuxAarch64Thread.ResetInstructionPointerAfterBreakpoint: FIsPaused');

  if not ReadThreadState then
    exit(False);
  result := true;
end;

function TDbgLinuxAarch64Thread.GetInstructionPointerRegisterValue: TDBGPtr;
begin
  //{$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxAarch64Thread.GetInstructionPointerRegisterValue');{$ENDIF}
  assert(IsPaused, 'TDbgLinuxAarch64Thread.GetInstructionPointerRegisterValue: FIsPaused');

  Result := 0;
  if not ReadThreadState then
    exit;
  result := FUserRegs.PC;
end;

function TDbgLinuxAarch64Thread.GetStackBasePointerRegisterValue: TDbgPtr;
begin
  //{$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxAarch64Thread.GetInstructionPointerRegisterValue');{$ENDIF}
  assert(IsPaused, 'TDbgLinuxAarch64Thread.GetInstructionPointerRegisterValue: FIsPaused');

  Result := 0;
  if not ReadThreadState then
    exit;
  result := FUserRegs.Regs[29];
end;

function TDbgLinuxAarch64Thread.GetStackPointerRegisterValue: TDbgPtr;
begin
  //{$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgLinuxAarch64Thread.GetInstructionPointerRegisterValue');{$ENDIF}
  assert(IsPaused, 'TDbgLinuxAarch64Thread.GetInstructionPointerRegisterValue: FIsPaused');

  Result := 0;
  if not ReadThreadState then
    exit;
  result := FUserRegs.SP;
end;

procedure TDbgLinuxAarch64Thread.SetInstructionPointerRegisterValue(AValue: TDbgPtr);
begin
  if not FHasThreadState then
    exit;
  FUserRegs.PC := AValue
end;

procedure TDbgLinuxAarch64Thread.SetStackPointerRegisterValue(AValue: TDbgPtr);
begin
  if not FHasThreadState then
    exit;
  FUserRegs.SP := AValue
end;


{ TDbgLinuxAarch64Process }

function TDbgLinuxAarch64Process.CreateBreakPointTargetHandler: TFpBreakPointTargetHandler;
begin
    Result := TBreakPointAarch64Handler.Create(Self);
end;

function TDbgLinuxAarch64Process.DbgThreadClass: TDbgThreadClass;
begin
  Result := TDbgLinuxAarch64Thread;
end;

class function TDbgLinuxAarch64Process.isSupported(ATargetInfo: TTargetDescriptor): boolean;
begin
  result := (ATargetInfo.OS = osLinux) and
            (ATargetInfo.machineType in [mtARM64]);
end;

function TDbgLinuxAarch64Process.CallParamDefaultLocation(AParamIdx: Integer): TFpDbgMemLocation;
begin
  Result := InvalidLoc;
  if (AParamIdx >= 0) and (AParamIdx <= 28) then
  begin
    Result.MType := mlfTargetRegister;
    Result.Address := AParamIdx;
  end;
end;

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

initialization
  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
  FPDBG_LINUX := DebugLogger.FindOrRegisterLogGroup('FPDBG_LINUX' {$IFDEF DebuglnLinuxDebugEvents} , True {$ENDIF} );

  RegisterDbgOsClasses(TOSDbgClasses.Create(
    TDbgLinuxAarch64Process,
    TDbgLinuxAarch64Thread,
    TAarch64AsmDecoder
  ));

end.
