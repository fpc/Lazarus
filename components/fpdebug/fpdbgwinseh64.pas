unit FpDbgWinSeh64;

{$mode objfpc}{$H+}

interface

uses
  FpDbgClasses, FpDbgLoader, FpImgReaderBase, FpdMemoryTools, DbgIntfBaseTypes, LazLoggerBase,
  SysUtils;

type

  { TDbgStackUnwinderWindowsSeh64 }

  TDbgStackUnwinderWindowsSeh64 = class(TDbgStackUnwinderEx)
  // Base is for GetTopFrame // Alternatively forward to any of the sub-unwinder
  private
  public
    //constructor Create(AProcess: TDbgProcess);
    //destructor Destroy; override;
    //procedure InitForThread(AThread: TDbgThread); override;
    procedure InitForFrame(ACurrentFrame: TDbgCallstackEntry; out CodePointer, StackPointer,
      FrameBasePointer: TDBGPtr); override;
    function Unwind(AFrameIndex: integer; var CodePointer, StackPointer,
      FrameBasePointer: TDBGPtr; ACurrentFrame: TDbgCallstackEntry; out
      ANewFrame: TDbgCallstackEntry): TTDbgStackUnwindResult; override;
    //procedure SetUnwindFlags(AFlags: TDbgUnwinderFlags); override;
  end;


implementation

type
  ULONG = DWord;

  RUNTIME_FUNCTION = record
    BeginAddress, EndAddress: ULONG;
    UnwindData: ULONG;
  end;
  PRUNTIME_FUNCTION = ^RUNTIME_FUNCTION;

  TUnwindVersion  = 0..7;
  TUnwindFlags    = 0..31;
  TUnwindFrameReg = 0..15;
  TUnwindFrameOffs = 0..15;

  UNWIND_INFO = bitpacked record
    Version: TUnwindVersion;
    Flags: TUnwindFlags;
    //VersionFlags  : Byte;   // bits 0-2 = version, bits 3-7 = UNW_FLAG_*
    SizeOfProlog: Byte;
    CountOfCodes: Byte;
    FrameReg: TUnwindFrameReg;
    FrameOffs: TUnwindFrameOffs;
    //FrameRegAndOff: Byte;   // bits 0-3 = frame reg, bits 4-7 = frame offset/16
    //UnwindCodes: record end; // array of UNWIND_CODE
  end;
  PUNWIND_INFO = ^UNWIND_INFO;


  TUnwindOpCode = 0..15;
  TUnwindOpInfo = 0..15;

  UNWIND_CODE = bitpacked record
    CodeOffset: Byte;
    UnwindOpCode: TUnwindOpCode;
    UnwindOpInfo: TUnwindOpInfo;
    //UnwindOpAndInfo: Byte;  // bits 0-3 = op, bits 4-7 = op-info
  end;

const
  UNW_FLAG_NHANDLER  = $00; // no handler
  UNW_FLAG_EHANDLER  = $01; // except handler
  UNW_FLAG_UHANDLER  = $02; // finally handler
  UNW_FLAG_CHAININFO = $04; // chain to other

  UWOP_PUSH_NONVOL     = 0;
  UWOP_ALLOC_LARGE     = 1;
  UWOP_ALLOC_SMALL     = 2;
  UWOP_SET_FPREG       = 3;
  UWOP_SAVE_NONVOL     = 4;
  UWOP_SAVE_NONVOL_FAR = 5;
  UWOP_SAVE_XMM128     = 8;
  UWOP_SAVE_XMM128_FAR = 9;
  UWOP_PUSH_MACHFRAME  = 10;



function FindRuntimeFunction(APDataSection: PDbgImageSection; CodePointer, ImageBase: TDBGPtr;
  out ARuntimeFunction: RUNTIME_FUNCTION): Boolean;
var
  L, H, M: Integer;
  Rva1: TDBGPtr;
  Rva: ULONG;
  Entry: PRUNTIME_FUNCTION;
begin
  Result := False;
  {$PUSH}{$Q-}{$R-}
  Rva1 := CodePointer - ImageBase;
  if Rva1 > high(Rva) then exit;
  Rva  := ULONG(Rva1);
  {$POP}

  L := 0;
  H := APDataSection^.Size div SizeOf(RUNTIME_FUNCTION);
  while L < H do
  begin
    M := (L + H) div 2;
    Pointer(Entry) := APDataSection^.RawData + M * SizeOf(RUNTIME_FUNCTION);
    if Rva < Entry^.BeginAddress then
      H := M
    else if Rva >= Entry^.EndAddress then
      L := M + 1
    else
    begin
      ARuntimeFunction := Entry^;
      Result := True;
      exit;
    end;
  end;
end;


{ TDbgStackUnwinderWindowsSeh64 }

procedure TDbgStackUnwinderWindowsSeh64.InitForFrame(ACurrentFrame: TDbgCallstackEntry; out
  CodePointer, StackPointer, FrameBasePointer: TDBGPtr);
var
  R: TDbgRegisterValue;
begin
  inherited InitForFrame(ACurrentFrame, CodePointer, StackPointer, FrameBasePointer);
  R := ACurrentFrame.RegisterValueList.FindRegisterByDwarfIndex(7);
  if R <> nil then
    StackPointer := R.NumValue;
end;

function TDbgStackUnwinderWindowsSeh64.Unwind(AFrameIndex: integer; var CodePointer, StackPointer,
  FrameBasePointer: TDBGPtr; ACurrentFrame: TDbgCallstackEntry; out ANewFrame: TDbgCallstackEntry
  ): TTDbgStackUnwindResult;
const
  REGNUM_TO_DWARF_MAP: array [0..15] of integer = (
    0 {RAX}, 2 {RCX}, 1 {RDX}, 3 {RBX}, 7 {RSP}, 6 {RBP}, 4 {RSI}, 5 {RDI},
    8 {R8}, 9 {R9}, 10 {R10}, 11 {R11}, 12 {R12}, 13 {R13}, 14 {R14}, 15 {R15}
  );
  REGNUM_TO_NAME_MAP: array [0..15] of AnsiString = (
    'RAX', 'RCX', 'RDX', 'RBX', 'RSP', 'RBP', 'RSI', 'RDI',
    'R8', 'R9', 'R10', 'R11', 'R12', 'R13', 'R14', 'R15'
  );
var
  BndInfo: TDbgFrameBoundaryInfo;
  Instance: TDbgInstance;
  ImageLoader: TDbgImageLoader;
  PDataSect: PDbgImageSection;
  ImageBase, CodeOffsInFunc: QWord;
  RuntimeFunc: RUNTIME_FUNCTION;
  UnwindInfoAddr: TDBGPtr;
  UnwindInfo: UNWIND_INFO;
  UnwindCodesAddr: TDBGPtr; // PUNWIND_CODE
  UnwindCodes: packed array of UNWIND_CODE;
  ARegisterValueList: TDbgRegisterValueList;
  FramePointerVal: TDBGPtr;
  FlagAsGuess, CodePointerDone: Boolean;
  i: Integer;
  t: TDBGPtr;
  r: TDbgRegisterValue;
  OpInfo: Byte;
  ChainedAddr: TDBGPtr;
begin
  Result := suFailed;
  ANewFrame := nil;

  if Process.Disassembler.GetFrameBoundaryInfo(CodePointer, BndInfo) in [bkInEpilogue, bkMaybeInEpilogue, bkAfterEpiloge] then
    exit;

  Instance := Process.GetInstanceForAddress(CodePointer);
  if (Instance = nil) or (Instance.LoaderList = nil) then
    exit;
  ImageLoader := Instance.LoaderList.GetImageLoaderForAddress(CodePointer);
  if ImageLoader = nil then
    exit;

  PDataSect := ImageLoader.Section['.pdata'];
  if PDataSect = nil then
    exit;

  ImageBase := Instance.ImageBase;
  if not FindRuntimeFunction(PDataSect,  CodePointer, Instance.ImageBase, RuntimeFunc) then
    exit;

//  OrigCodePointer := CodePointer;
//  OrigStackPointer := StackPointer;
  {$PUSH}{$Q-}{$R-}
  CodeOffsInFunc := CodePointer - ImageBase - RuntimeFunc.BeginAddress;
  {$POP}

  ARegisterValueList := TDbgRegisterValueList.Create(True);
  FlagAsGuess := False;
  CodePointerDone := False;
  try
    // repeat for chained
    repeat
      if (RuntimeFunc.UnwindData and 1) <> 0 then begin
        // apparently this might be used as redirect
        // TODO: Offset of codepointer to RuntimeFunc.BeginAddress may now be wrong
        {$PUSH}{$Q-}{$R-}
        if not Process.ReadData(ImageBase + (RuntimeFunc.UnwindData and (not TDBGPtr(1))), SizeOf(RUNTIME_FUNCTION), RuntimeFunc) then
          exit;
        {$POP}
      end;

      {$PUSH}{$Q-}{$R-}
      UnwindInfoAddr  := ImageBase + (RuntimeFunc.UnwindData and (not TDBGPtr(1)));
      UnwindCodesAddr := UnwindInfoAddr + SizeOf(UNWIND_INFO);
      {$POP}

      if not Process.ReadData(UnwindInfoAddr, SizeOf(UNWIND_INFO), UnwindInfo) then
        exit;

      if not UnwindInfo.Version <= 2 then
        exit;

      FramePointerVal := 0;
      if UnwindInfo.FrameReg <> 0 then begin
        r := ACurrentFrame.RegisterValueList.FindRegisterByDwarfIndex(REGNUM_TO_DWARF_MAP[UnwindInfo.FrameReg]);
        if r = nil then
          FramePointerVal := r.NumValue
        else
          FlagAsGuess := True;
      end;


      UnwindCodes := nil;
      if UnwindInfo.CountOfCodes > 0 then begin
        SetLength(UnwindCodes, UnwindInfo.CountOfCodes);
        if not Process.ReadData(UnwindCodesAddr, UnwindInfo.CountOfCodes * SizeOf(UNWIND_CODE), UnwindCodes[0]) then
          exit;
      end;

      i := 0;
      while i < UnwindInfo.CountOfCodes do
      begin
        if CodeOffsInFunc < UnwindCodes[i].CodeOffset then begin
          inc(i);
          Continue;
        end;

        OpInfo := UnwindCodes[i].UnwindOpInfo;

        case UnwindCodes[i].UnwindOpCode of
          UWOP_PUSH_NONVOL: begin
            // register
            if Process.ReadAddress(StackPointer, t) then begin
              ARegisterValueList.DbgRegisterAutoCreate[REGNUM_TO_NAME_MAP[OpInfo]].SetValue(t, IntToStr(t), 8, REGNUM_TO_DWARF_MAP[OpInfo]);
              case OpInfo of
                4: StackPointer := t;
                5: FrameBasePointer := t;
              end;
            end
            else
              FlagAsGuess := True;
            {$PUSH}{$Q-}{$R-}
            Inc(StackPointer, 8);
            {$POP}
            Inc(i);
          end;

          UWOP_ALLOC_LARGE: begin
            // stack alloc, size (div 8) in in next one or two entries
            if OpInfo = 0 then begin
              {$PUSH}{$Q-}{$R-}
              Inc(StackPointer, WORD(UnwindCodes[i+1]) * 8);
              {$POP}
              Inc(i, 2);
            end
            else begin
              {$PUSH}{$Q-}{$R-}
              Inc(StackPointer, PDWORD(@UnwindCodes[i+1])^);
              {$POP}
              Inc(i, 3);
            end;
          end;

          UWOP_ALLOC_SMALL: begin
            // stack alloc
            {$PUSH}{$Q-}{$R-}
            Inc(StackPointer, OpInfo * 8 + 8);
            {$POP}
            Inc(i);
          end;

          UWOP_SET_FPREG: begin
            if UnwindInfo.FrameReg = 0 then begin
              FramePointerVal := StackPointer;
              FlagAsGuess := True; // there should be a frame-reg
            end;
            if FramePointerVal = 0 then
              exit;
            {$PUSH}{$Q-}{$R-}
            StackPointer := FramePointerVal - TDBGPtr(UnwindInfo.FrameOffs * 16);
            {$POP}
            Inc(i);
          end;

          UWOP_SAVE_NONVOL: begin
            if UnwindInfo.FrameReg = 0 then
              FramePointerVal := StackPointer;
            {$PUSH}{$Q-}{$R-}
            if Process.ReadAddress(FramePointerVal + WORD(UnwindCodes[i+1]) * 8, t) then begin
              ARegisterValueList.DbgRegisterAutoCreate[REGNUM_TO_NAME_MAP[OpInfo]].SetValue(t, IntToStr(t), 8, REGNUM_TO_DWARF_MAP[OpInfo]);
            {$POP}
            end
            else
              FlagAsGuess := True;
            Inc(i, 2);
          end;

          UWOP_SAVE_NONVOL_FAR: begin
            if UnwindInfo.FrameReg = 0 then
              FramePointerVal := StackPointer;
            {$PUSH}{$Q-}{$R-}
            if Process.ReadAddress(FramePointerVal + PDWORD(@UnwindCodes[i+1])^, t) then begin
              ARegisterValueList.DbgRegisterAutoCreate[REGNUM_TO_NAME_MAP[OpInfo]].SetValue(t, IntToStr(t), 8, REGNUM_TO_DWARF_MAP[OpInfo]);
            {$POP}
            end
            else
              FlagAsGuess := True;
            Inc(i, 3);
          end;

          UWOP_SAVE_XMM128: begin
            Inc(i, 2);
          end;

          UWOP_SAVE_XMM128_FAR: begin
            Inc(i, 3);
          end;

          UWOP_PUSH_MACHFRAME: begin
            // Interrupt frame
            // OpInfo=1: ignore 8 byte error code at start
            // TODO: restore other register
            if OpInfo > 1 then
              exit;
            {$PUSH}{$Q-}{$R-}
            if OpInfo = 1 then
              Inc(StackPointer, 8);
            if not Process.ReadAddress(StackPointer, CodePointer) then
              exit;
            if Process.ReadAddress(StackPointer + 8, t)
            then ARegisterValueList.DbgRegisterAutoCreate['CS'].SetValue(t, IntToStr(t), 8, 51)
            else FlagAsGuess := True;
            if Process.ReadAddress(StackPointer + 16, t)
            then ARegisterValueList.DbgRegisterAutoCreate['EFLAGS'].Setx86EFlagsValue(t)
            else FlagAsGuess := True;
            if Process.ReadAddress(StackPointer + 32, t)
            then ARegisterValueList.DbgRegisterAutoCreate['SS'].SetValue(t, IntToStr(t), 8, 52)
            else FlagAsGuess := True;
            if not Process.ReadAddress(StackPointer + 24, StackPointer) then
              exit;
            {$POP}
            CodePointerDone := True;
            inc(i);
          end;

        else
          exit;
        end;
      end;

      if (UnwindInfo.Flags and UNW_FLAG_CHAININFO) = 0 then
        break;

      // follow chained entry
      CodeOffsInFunc := high(CodeOffsInFunc); // chained entries are always fully processed

      // After the (padded-to-even) unwind codes comes a chained RUNTIME_FUNCTION
      {$PUSH}{$Q-}{$R-}
      ChainedAddr := UnwindCodesAddr
                  + ((UnwindInfo.CountOfCodes + 1) and (not 1)) * 2;
      if not Process.ReadData(ChainedAddr, SizeOf(RUNTIME_FUNCTION), RuntimeFunc) then
        exit;
      {$POP}
    until false;

    // read return address
    if not CodePointerDone then begin
      if not Process.ReadAddress(StackPointer, CodePointer) then
        exit;
      {$PUSH}{$Q-}{$R-}
      Inc(StackPointer, 8);
      {$POP}
    end;


    ANewFrame := TDbgCallstackEntry.create(Thread, AFrameIndex, FrameBasePointer, CodePointer);
    ANewFrame.RegisterValueList.Assign(ARegisterValueList);
    ANewFrame.RegisterValueList.DbgRegisterAutoCreate['RIP'].SetValue(CodePointer, IntToStr(CodePointer),8, 16);
    ANewFrame.RegisterValueList.DbgRegisterAutoCreate['RPB'].SetValue(FrameBasePointer, IntToStr(FrameBasePointer),8, 6);
    ANewFrame.RegisterValueList.DbgRegisterAutoCreate['RSP'].SetValue(StackPointer, IntToStr(StackPointer),8, 7);

    Result := suSuccess;
    if FlagAsGuess then
      Result := suGuessed;
  finally
    ARegisterValueList.Free;
  end;
end;

end.

