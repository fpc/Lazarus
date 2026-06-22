unit FpDbgCpuX86;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DbgIntfBaseTypes, FpDbgClasses, FpdMemoryTools, FpDbgDwarfCFI,
  FpDbgDwarfDataClasses, FpDbgDisasX86, FpDbgLoader, FpImgReaderBase, FpDbgUtil, FpDbgWinSeh64,
  {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif};

type

  { TDbgx86Thread }

  TDbgx86Thread = class(TDbgThread)
  protected
    FHasResetInstructionPointerAfterBreakpoint: boolean;

    function GetInstructionPointerForHasBreakpointInfoForAddress: TDBGPtr; override;
  public
    function GetCurrentStackFrameInfo: TDbgStackFrameInfo; override;
  end;

  TBreakInfoX86 = object
  const
    _CODE: Byte = $CC;
  end;

  TBreakPointx86Handler = specialize TGenericBreakPointTargetHandler<Byte, TBreakInfoX86>;

  { TDbgx86Process }

  TDbgx86Process = class(TDbgProcess)
  protected
    function CreateBreakPointTargetHandler: TFpBreakPointTargetHandler; override;
  end;

  { TDbgStackFrameSteppingInfoX86 }

  TDbgStackFrameSteppingInfoX86 = class(TDbgStackFrameInfo)
  private
    FProcessAfterRun: Boolean;
    FLeaveState: (lsNone, lsWasAtLeave1, lsWasAtLeave2, lsLeaveDone);
    Procedure DoAfterRun;
  protected
    procedure DoCheckNextInstruction(ANextInstruction: TDbgAsmInstruction;
      NextIsSingleStep: Boolean); override;
    function CalculateHasSteppedOut: Boolean; override;
  end;

  { TDbgStackUnwinderX86FramePointer }

  TDbgStackUnwinderX86FramePointer = class(TDbgStackUnwinderX86Base)
  private
    FLastFrameBaseIncreased: Boolean;
    FCodeReadErrCnt: integer;
  public
    procedure InitForThread(AThread: TDbgThread); override;
    function Unwind(AFrameIndex: integer; var CodePointer, StackPointer,
      FrameBasePointer: TDBGPtr; ACurrentFrame: TDbgCallstackEntry; out
      ANewFrame: TDbgCallstackEntry): TTDbgStackUnwindResult; override;
  end;

  { TDbgStackUnwinderX86DwarfCfi }

  TDbgStackUnwinderX86DwarfCfi = class(TDbgStackUnwinderX86Base)
  // Maybe have TDbgStackUnwinderX86Base as delegate instead of base
  private
  public
    //procedure InitForThread(AThread: TDbgThread); override;
    function Unwind(AFrameIndex: integer; var CodePointer, StackPointer,
      FrameBasePointer: TDBGPtr; ACurrentFrame: TDbgCallstackEntry; out
      ANewFrame: TDbgCallstackEntry): TTDbgStackUnwindResult; override;
  end;

  { TDbgStackUnwinderX86MultiMethod }

  TDbgStackUnwinderX86MultiMethod = class(TDbgStackUnwinderX86Base)
  // Base is for GetTopFrame // Alternatively forward to any of the sub-unwinder
  private
    FDwarfUnwinder: TDbgStackUnwinderX86DwarfCfi;
    FSehUnwinder: TDbgStackUnwinder;
    FFrameUnwinder: TDbgStackUnwinderX86FramePointer;
    FAsmUnwinder: TDbgStackUnwinderIntelDisAssembler;
    FFlags: TDbgUnwinderFlags;
  public
    constructor Create(AProcess: TDbgProcess);
    destructor Destroy; override;
    procedure InitForThread(AThread: TDbgThread); override;
    function Unwind(AFrameIndex: integer; var CodePointer, StackPointer,
      FrameBasePointer: TDBGPtr; ACurrentFrame: TDbgCallstackEntry; out
      ANewFrame: TDbgCallstackEntry): TTDbgStackUnwindResult; override;
    procedure SetUnwindFlags(AFlags: TDbgUnwinderFlags); override;
  end;

implementation

{ TDbgx86Thread }

function TDbgx86Thread.GetInstructionPointerForHasBreakpointInfoForAddress: TDBGPtr;
begin
  Result := GetInstructionPointerRegisterValue;
  if (Result <> 0) and not FHasResetInstructionPointerAfterBreakpoint then
    Result := Result - 1;
end;

function TDbgx86Thread.GetCurrentStackFrameInfo: TDbgStackFrameInfo;
begin
  Result := TDbgStackFrameSteppingInfoX86.Create(Self);
end;

{ TDbgx86Process }

function TDbgx86Process.CreateBreakPointTargetHandler: TFpBreakPointTargetHandler;
begin
  Result := TBreakPointx86Handler.Create(Self);
end;

{ TDbgStackFrameSteppingInfoX86 }

procedure TDbgStackFrameSteppingInfoX86.DoAfterRun;
var
  CurStackFrame: TDBGPtr;
begin
  FProcessAfterRun := False;
  case FLeaveState of
    lsWasAtLeave1: begin
        CurStackFrame   := Thread.GetStackBasePointerRegisterValue;
        FStoredStackPointer := Thread.GetStackPointerRegisterValue;
        if CurStackFrame <> FStoredStackFrame then
          FLeaveState := lsLeaveDone // real leave
        else
          FLeaveState := lsWasAtLeave2; // lea rsp,[rbp+$00] / pop ebp // epb in next command
      end;
    lsWasAtLeave2: begin
        // TODO: maybe check, if stackpointer only goes down by sizeof(pointer) "Pop bp"
        FStoredStackFrame   := Thread.GetStackBasePointerRegisterValue;
        FStoredStackPointer := Thread.GetStackPointerRegisterValue;
        FLeaveState := lsLeaveDone;
      end;
  end;
end;

procedure TDbgStackFrameSteppingInfoX86.DoCheckNextInstruction(
  ANextInstruction: TDbgAsmInstruction; NextIsSingleStep: Boolean);
begin
  if FProcessAfterRun then
    DoAfterRun;

  inherited DoCheckNextInstruction(ANextInstruction, NextIsSingleStep);

  if not NextIsSingleStep then begin
    if FLeaveState = lsWasAtLeave2 then
      FLeaveState := lsLeaveDone;
    exit;
  end;

  if ANextInstruction.IsReturnInstruction then begin
    FLeaveState := lsLeaveDone;
  end
  else if FLeaveState = lsNone then begin
    if ANextInstruction.IsLeaveStackFrame then
      FLeaveState := lsWasAtLeave1;
  end;

  FProcessAfterRun := FLeaveState in [lsWasAtLeave1, lsWasAtLeave2];
end;

function TDbgStackFrameSteppingInfoX86.CalculateHasSteppedOut: Boolean;
begin
  if FProcessAfterRun then
    DoAfterRun;

  Result := inherited CalculateHasSteppedOut;
end;

{ TDbgStackUnwinderX86FramePointer }

procedure TDbgStackUnwinderX86FramePointer.InitForThread(AThread: TDbgThread);
begin
  inherited InitForThread(AThread);
  FLastFrameBaseIncreased := True;
  FCodeReadErrCnt := 0;
end;

function TDbgStackUnwinderX86FramePointer.Unwind(AFrameIndex: integer;
  var CodePointer, StackPointer, FrameBasePointer: TDBGPtr;
  ACurrentFrame: TDbgCallstackEntry; out ANewFrame: TDbgCallstackEntry
  ): TTDbgStackUnwindResult;
var
  OutSideFrame, UnknownOutsideFrame, AfterCallOut, AfterCallIn: Boolean;
  Dummy: QWord;
  LastFrameBase, TmpCodePointer: TDBGPtr;
  NewRes: TTDbgStackUnwindResult;
begin
  ANewFrame := nil;
  Result := suFailed;
  NewRes := suSuccess;

  if StackPointer = 0 then
    exit;
  if not FLastFrameBaseIncreased then
    exit;

  OutSideFrame := False;
  UnknownOutsideFrame := False;
  TmpCodePointer := 0;
  LastFrameBase := FrameBasePointer;

  if not Process.Disassembler.GetFunctionFrameInfo(CodePointer, OutSideFrame) then begin
    UnknownOutsideFrame := True;
    if Process.Disassembler.LastErrorWasMemReadErr then begin
      inc(FCodeReadErrCnt);
      if FCodeReadErrCnt > 5 then // If the code cannot be read the stack pointer is wrong.
        exit;
      if AFrameIndex <= 1 then
        OutSideFrame := True; // Maybe after "TProc(nil)();" call, then no frame could have been set up
    end;
  end;
  if (not OutSideFrame) {and (AFrameIndex = 1)} and (ACurrentFrame.ProcSymbol <> nil) then begin
    // the frame must be outside frame, if it is at entrypoint / needed for exceptions
    OutSideFrame := CodePointer = LocToAddrOrNil(ACurrentFrame.ProcSymbol.Address);
    if OutSideFrame then
      UnknownOutsideFrame := False;
  end;

  if OutSideFrame or UnknownOutsideFrame then begin
    if not Process.ReadData(StackPointer, AddressSize, CodePointer) or (CodePointer = 0) then
      exit;

    if (not Process.ReadData(CodePointer, 1, Dummy) or (CodePointer = 0)) then begin
      OutSideFrame := False;
      NewRes := suGuessed;
    end
    else begin
      {$PUSH}{$R-}{$Q-}
      if UnknownOutsideFrame then begin
        NewRes := suGuessed;
        if Process.ReadData(FrameBasePointer + AddressSize, AddressSize, TmpCodePointer) and
          (TmpCodePointer <> 0)
        then begin
          AfterCallOut := Process.Disassembler.IsAfterCallInstruction(CodePointer);
          AfterCallIn := Process.Disassembler.IsAfterCallInstruction(TmpCodePointer);
          if AfterCallOut = AfterCallIn then
            OutSideFrame := StackPointer + 1 * AddressSize < FrameBasePointer + 2 * AddressSize
          else
            OutSideFrame := AfterCallOut;
        end;
      end;

      if OutSideFrame then begin
        StackPointer := StackPointer + 1 * AddressSize; // After popping return-addr from "StackPointer"
        if LastFrameBase > 0 then
          LastFrameBase := LastFrameBase - 1; // Make the loop think thas LastFrameBase was smaller
        // last stack has no frame
        //ACurrentFrame.RegisterValueList.DbgRegisterAutoCreate[nBP].SetValue(0, '0',AddressSize, BP);
      end;
      {$POP}
    end;
  end;

  if not OutSideFrame then begin
    {$PUSH}{$R-}{$Q-}
    StackPointer := FrameBasePointer + 2 * AddressSize; // After popping return-addr from "FrameBasePointer + AddressSize"
    if TmpCodePointer <> 0 then
      CodePointer := TmpCodePointer
    else
      if not Process.ReadData(FrameBasePointer + AddressSize, AddressSize, CodePointer) or (CodePointer = 0) then
        exit;
    {$POP}
    if not Process.ReadData(FrameBasePointer, AddressSize, FrameBasePointer) then
      exit;
  end;

  FLastFrameBaseIncreased := (FrameBasePointer <> 0) and (FrameBasePointer > LastFrameBase);
  if not FLastFrameBaseIncreased then
    NewRes := suGuessed;


  ANewFrame := TDbgCallstackEntry.create(Thread, AFrameIndex, FrameBasePointer, CodePointer);
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate[FNameIP].SetValue(CodePointer, IntToStr(CodePointer),AddressSize, FDwarfNumIP);
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate[FNameBP].SetValue(FrameBasePointer, IntToStr(FrameBasePointer),AddressSize, FDwarfNumBP);
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate[FNameSP].SetValue(StackPointer, IntToStr(StackPointer),AddressSize, FDwarfNumSP);
  FCodeReadErrCnt := 0;
  Result := NewRes;
end;

{ TDbgStackUnwinderX86DwarfCfi }

function TDbgStackUnwinderX86DwarfCfi.Unwind(AFrameIndex: integer;
  var CodePointer, StackPointer, FrameBasePointer: TDBGPtr;
  ACurrentFrame: TDbgCallstackEntry; out ANewFrame: TDbgCallstackEntry
  ): TTDbgStackUnwindResult;
var
  PrevStmtAddressOffs: Integer;
  Row: TDwarfCallFrameInformationRow;
  CIE: TDwarfCIE;
  CU: TDwarfCompilationUnit;
  StackReg, FrameReg: TDbgRegisterValue;
begin
  ANewFrame := nil;
  Result := suFailed;

  PrevStmtAddressOffs := 1;
  if AFrameIndex <= 1 then
    PrevStmtAddressOffs := 0;

  {$PUSH}{$R-}{$Q-}
  if Process.FindCallFrameInfo(CodePointer - PrevStmtAddressOffs, CIE, Row) and
     TryObtainNextCallFrame(
       ACurrentFrame, CIE, AddressSize, AFrameIndex, Thread, Row, Process, ANewFrame
     )
  {$POP}
  then begin
    if not Assigned(ANewFrame) then begin
      CU := (Process.DbgInfo as TFpDwarfInfo).CompilationUnitForAddr(CodePointer);
      if (CU = nil) or (CU.DwarfSymbolClassMap = nil) or (not CU.DwarfSymbolClassMap.IgnoreCfiStackEnd) then begin
        // Done.
        Result := suFailedAtEOS;
        exit;
      end;
    end
    else begin
      Result := suSuccess;
      CodePointer := ANewFrame.AnAddress;
      StackReg := ANewFrame.RegisterValueList.FindRegisterByDwarfIndex(FDwarfNumSP);
      FrameReg := ANewFrame.RegisterValueList.FindRegisterByDwarfIndex(FDwarfNumBP);
      StackPointer     := 0;
      FrameBasePointer := 0;
      if (StackReg <> nil) and (FrameReg <> nil) then begin
        StackPointer     := StackReg.NumValue;
        FrameBasePointer := FrameReg.NumValue;
      end;
    end;
  end;

end;

{ TDbgStackUnwinderX86MultiMethod }

constructor TDbgStackUnwinderX86MultiMethod.Create(AProcess: TDbgProcess);
begin
  FDwarfUnwinder := TDbgStackUnwinderX86DwarfCfi.Create(AProcess);
  if AProcess.Mode = dm64 then
    FSehUnwinder := TDbgStackUnwinderWindowsSeh64.Create(AProcess);
  FFrameUnwinder := TDbgStackUnwinderX86FramePointer.Create(AProcess);
  FAsmUnwinder   := TDbgStackUnwinderIntelDisAssembler.Create(AProcess);
  inherited Create(AProcess);
end;

destructor TDbgStackUnwinderX86MultiMethod.Destroy;
begin
  inherited Destroy;
  FDwarfUnwinder.Free;
  FSehUnwinder.Free;
  FFrameUnwinder.Free;
  FAsmUnwinder.Free;
end;

procedure TDbgStackUnwinderX86MultiMethod.InitForThread(AThread: TDbgThread);
begin
  inherited InitForThread(AThread);
  FDwarfUnwinder.InitForThread(AThread);
  if FSehUnwinder <> nil then FSehUnwinder.InitForThread(AThread);
  FFrameUnwinder.InitForThread(AThread);
  FAsmUnwinder.InitForThread(AThread);
  FFlags := [];
end;

function TDbgStackUnwinderX86MultiMethod.Unwind(AFrameIndex: integer;
  var CodePointer, StackPointer, FrameBasePointer: TDBGPtr;
  ACurrentFrame: TDbgCallstackEntry; out ANewFrame: TDbgCallstackEntry
  ): TTDbgStackUnwindResult;
var
  OrigCodePointer, OrigFrameBasePointer, OrigStackPointer: TDBGPtr;
  CodePointer2, FrameBasePointer2, StackPointer2: TDBGPtr;
  ANewFrame2: TDbgCallstackEntry;
  ResSeh, ResAsm: TTDbgStackUnwindResult;

  procedure InitPtr2;
  begin
    CodePointer2      := OrigCodePointer;
    FrameBasePointer2 := OrigFrameBasePointer;
    StackPointer2     := OrigStackPointer;
  end;
  procedure UsePtr2;
  begin
    CodePointer      := CodePointer2;
    FrameBasePointer := FrameBasePointer2;
    StackPointer     := StackPointer2;
  end;
  procedure UseFrame2;
  begin
    ANewFrame.Free;
    ANewFrame        := ANewFrame2;
  end;
begin
  OrigCodePointer      := CodePointer;
  OrigFrameBasePointer := FrameBasePointer;
  OrigStackPointer     := StackPointer;

  (* *** DWARF CFI *** *)

  Result := FDwarfUnwinder.Unwind(AFrameIndex,
    CodePointer, StackPointer, FrameBasePointer, ACurrentFrame, ANewFrame);

  if (Result = suSuccess) then begin
    FFrameUnwinder.FLastFrameBaseIncreased := True;
    if Process.Disassembler.IsAfterCallInstruction(CodePointer) then
      exit;
  end;
  if Result = suFailedAtEOS then
    exit;

  (* *** SEH *** *)

  if FSehUnwinder <> nil then begin
    InitPtr2;
    ResSeh := FSehUnwinder.Unwind(AFrameIndex, CodePointer2, StackPointer2, FrameBasePointer2, ACurrentFrame, ANewFrame2);
    case ResSeh of
      suSuccess: begin
        UsePtr2;
        UseFrame2;
        Result := suSuccess;
        exit;
      end;
      suGuessed: begin
        if (Result in [suGuessed, suSuccess]) and // CFI guessed too
           (StackPointer = StackPointer2) and (CodePointer = CodePointer2)
        then begin
          // use CFI res
          FreeAndNil(ANewFrame2);
          Result := suSuccess;
          exit;
        end;
        if Result = suFailed then begin
          // use the SEH as fallback
          Result := suGuessed;
          UseFrame2;
          UsePtr2;
          ANewFrame2 := nil;
        end;
      end;
    end;
    FreeAndNil(ANewFrame2);
  end;

  (* *** ASM *** *)

  ResAsm := suFailed;
  if not (ufSkipArtificialFrames in FFlags) then begin
    // Get Asm unwind
    InitPtr2;
    ResAsm := FAsmUnwinder.Unwind(AFrameIndex, CodePointer2, StackPointer2, FrameBasePointer2, ACurrentFrame, ANewFrame2);

    if (ResAsm = suSuccess) then begin
      // prefer Asm result over DwarfCfi
      FFrameUnwinder.FLastFrameBaseIncreased := True;
      if Process.Disassembler.IsAfterCallInstruction(CodePointer2) then begin
        UsePtr2;
        UseFrame2;
        Result := suSuccess;
        exit;
      end;
    end;


    if (ResAsm in [suSuccess, suGuessed]) and
       (Result in [suSuccess, suGuessed]) and
       (CodePointer = CodePointer2) and (StackPointer = StackPointer2) and (FrameBasePointer = FrameBasePointer2)
    then begin
      // Both results where unsure => but both equal
      FFrameUnwinder.FLastFrameBaseIncreased := True;
      ANewFrame2.Free;
      Result := suSuccess;
      exit;
    end;
  end;

  (* *** FramePointer *** *)

  ANewFrame.Free;
  // get frame-unwind
  CodePointer      := OrigCodePointer;
  FrameBasePointer := OrigFrameBasePointer;
  StackPointer     := OrigStackPointer;
  Result := FFrameUnwinder.Unwind(AFrameIndex,
      CodePointer, StackPointer, FrameBasePointer, ACurrentFrame, ANewFrame);

  if (Result = suSuccess) then begin
    if Process.Disassembler.IsAfterCallInstruction(CodePointer2) then
      exit;
  end;

  if (ResAsm in [suSuccess, suGuessed]) then begin
    ANewFrame.Free;
    CodePointer      := CodePointer2;
    FrameBasePointer := FrameBasePointer2;
    StackPointer     := StackPointer2;
    ANewFrame        := ANewFrame2;
    Result := suSuccess;
    exit;
  end;


end;

procedure TDbgStackUnwinderX86MultiMethod.SetUnwindFlags(AFlags: TDbgUnwinderFlags);
begin
  FFlags := AFlags;
end;

end.

