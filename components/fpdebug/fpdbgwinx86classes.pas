unit FpDbgWinX86Classes;

{$mode objfpc}{$H+}
{$IFDEF INLINE_OFF}{$INLINE OFF}{$ENDIF}

interface

uses
  Classes, windows, SysUtils,
  {$IF FPC_Fullversion>30202}
  {$ifNdef cpui386} ufloatx80, sfpux80, {$endif}
  {$ENDIF}
  // LazUtils
  LazLoggerBase,
  // DebuggerIntf
  LazDebuggerIntfFloatTypes, LazDebuggerIntfBaseTypes,
  // FpDebug
  FpDbgWinClasses, FpDbgClasses, FpDbgWinExtra, FpdMemoryTools, FpDbgCommon,
  FpDbgUtil, FpDbgCpuX86, FpDbgDisasX86;

type

  { TDbgWinX86Thread }

  TDbgWinX86Thread = class(TDbgWinThread)
  private
    FFailed_CONTEXT_EXTENDED_REGISTERS: boolean;
    FThreadContextChanged: boolean;
    FThreadContextChangeFlags: TFpContextChangeFlags;
    FCurrentContext: PFpContext; // FCurrentContext := Pointer((PtrUInt(@_UnAligendContext) + 15) and not PtrUInt($F));
    _UnAligendContext: TFpContext;
    _StoredContext: TFpContext;
  protected
    procedure LoadRegisterValues; override;
    function GetFpThreadContext(var AStorage: TFpContext; out ACtxPtr: PFpContext; ACtxFlags: TFpWinCtxFlags): Boolean;
    function SetFpThreadContext(ACtxPtr: PFpContext; ACtxFlags: TFpWinCtxFlags = cfSkip): Boolean;
    function HasContext: Boolean; override;
    procedure DumpContext; override;
  public
    procedure SetSingleStep; override;
    procedure ApplyWatchPoints(AWatchPointData: TFpWatchPointData); override;
    function DetectHardwareWatchpoint: TFpInternalWatchpoint; override;
    procedure BeforeContinue; override;
    function ResetInstructionPointerAfterBreakpoint: boolean; override;
    function GetAdjustedInstructionPointerRegisterValue: TDBGPtr; override;
    function ReadThreadState: boolean; override;

    procedure SetRegisterValue(AName: string; AValue: QWord); override;
    procedure StoreRegisters; override;
    procedure RestoreRegisters; override;
    function GetInstructionPointerRegisterValue: TDbgPtr; override;
    function GetStackBasePointerRegisterValue: TDbgPtr; override;
    procedure SetInstructionPointerRegisterValue(AValue: TDbgPtr); override;
    procedure SetStackPointerRegisterValue(AValue: TDbgPtr); override;
    function GetStackPointerRegisterValue: TDbgPtr; override;
  end;

  { TDbgWinX86Process }

  TDbgWinX86Process = class(TDbgWinProcess)
  public
    procedure Interrupt; override;
    class function isSupported(ATargetInfo: TTargetDescriptor): boolean; override;
  end;


implementation
var
  DBG_VERBOSE, DBG_WARNINGS, FPDBG_WINDOWS: PLazLoggerLogGroup;

const
  {$ifdef cpux86_64}
  CONTEXT_XSTATE = $00100040; // 64bit  // Early Win-7-SP1 needs $00100020
  {$else}
  CONTEXT_XSTATE = $00010040; // 32 bit
  {$endif}

const
  XSTATE_LEGACY_FLOATING_POINT = 0;
  XSTATE_LEGACY_SSE            = 1;
  XSTATE_GSSE                  = 2;
  XSTATE_AVX                   = XSTATE_GSSE;
  XSTATE_MPX_BNDREGS           = 3;
  XSTATE_MPX_BNDCSR            = 4;
  XSTATE_AVX512_KMASK          = 5;
  XSTATE_AVX512_ZMM_H          = 6;
  XSTATE_AVX512_ZMM            = 7;
  XSTATE_IPT                   = 8;
  XSTATE_CET_U                 = 11;
  XSTATE_LWP                   = 62;
  MAXIMUM_XSTATE_FEATURES      = 64;

  XSTATE_MASK_LEGACY_FLOATING_POINT = DWORD64(1 << XSTATE_LEGACY_FLOATING_POINT);
  XSTATE_MASK_LEGACY_SSE            = DWORD64(1 << XSTATE_LEGACY_SSE);
  XSTATE_MASK_LEGACY                = (XSTATE_MASK_LEGACY_FLOATING_POINT or XSTATE_MASK_LEGACY_SSE);
  XSTATE_MASK_GSSE                  = DWORD64(1 << XSTATE_GSSE);
  XSTATE_MASK_AVX                   = XSTATE_MASK_GSSE;

var
  // XState
  _GetEnabledXStateFeatures: function(): DWORD64; stdcall = nil;
  _InitializeContext:     function(Buffer: Pointer; ContextFlags: DWORD; Context: PPCONTEXT; ContextLength: PDWORD): BOOL; stdcall = nil;
  _GetXStateFeaturesMask: function(Context: PCONTEXT; FeatureMask: PDWORD64): BOOL; stdcall = nil;
  _LocateXStateFeature:   function(Context: PCONTEXT; FeatureId: DWORD; Length: PDWORD): PM128A; stdcall = nil;
  _SetXStateFeaturesMask: function(Context: PCONTEXT; FeatureMask: DWORD64): BOOL; stdcall = nil;
  _xstate_FeatureMask: DWORD64;

procedure LoadKernelEntryPoints;
var
  hMod: THandle;
begin
  hMod := GetModuleHandle(kernel32);
  DebugLn(DBG_WARNINGS and (hMod = 0), ['ERROR: Failed to get kernel32 handle']);
  if hMod = 0 then
    exit; //????

  // xstate
  Pointer(_GetEnabledXStateFeatures) := GetProcAddress(hMod, 'GetEnabledXStateFeatures');
  Pointer(_InitializeContext)        := GetProcAddress(hMod, 'InitializeContext');
  Pointer(_GetXStateFeaturesMask)    := GetProcAddress(hMod, 'GetXStateFeaturesMask');
  Pointer(_LocateXStateFeature)      := GetProcAddress(hMod, 'LocateXStateFeature');
  Pointer(_SetXStateFeaturesMask)    := GetProcAddress(hMod, 'SetXStateFeaturesMask');
  if (_GetEnabledXStateFeatures=nil) or (_InitializeContext=nil) or (_GetXStateFeaturesMask=nil) or
     (_LocateXStateFeature=nil) or (_SetXStateFeaturesMask=nil)
  then begin
    _GetEnabledXStateFeatures := nil;
  end
  else begin
    _xstate_FeatureMask := _GetEnabledXStateFeatures();
    if (_xstate_FeatureMask and XSTATE_MASK_GSSE) = 0 then
      _GetEnabledXStateFeatures := nil;
  end;

end;

{ TDbgWinX86Thread }

procedure TDbgWinX86Thread.LoadRegisterValues;
{$IF FPC_Fullversion>30202}{$ifNdef cpui386}
type
  PExtended = ^floatx80;
{$endif}{$ENDIF}
var
  Context: PCONTEXT;
  ContextSize: DWord;
  Buffer, Buffer2: Pointer;
  FeatureMask: DWORD64;
  Xmm, Ymm: PM128A;
  FeatureLength, FeatureLength2: DWORD;
  i: Integer;
  r: TDbgRegisterValue;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgWinThread.LoadRegisterValues');{$ENDIF}
  assert(MDebugEvent.dwProcessId <> 0, 'TDbgWinThread.LoadRegisterValues: MDebugEvent.dwProcessId <> 0');

  if FCurrentContext = nil then
    if not ReadThreadState then
      exit;

  DisableFloatExceptions;
  try

  {$ifdef cpui386}
  with FCurrentContext^.def do
  begin
    FRegisterValueList.DbgRegisterAutoCreate['eax'].SetValue(Eax, IntToStr(Eax),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['ecx'].SetValue(Ecx, IntToStr(Ecx),4,1);
    FRegisterValueList.DbgRegisterAutoCreate['edx'].SetValue(Edx, IntToStr(Edx),4,2);
    FRegisterValueList.DbgRegisterAutoCreate['ebx'].SetValue(Ebx, IntToStr(Ebx),4,3);
    FRegisterValueList.DbgRegisterAutoCreate['esp'].SetValue(Esp, IntToStr(Esp),4,4);
    FRegisterValueList.DbgRegisterAutoCreate['ebp'].SetValue(Ebp, IntToStr(Ebp),4,5);
    FRegisterValueList.DbgRegisterAutoCreate['esi'].SetValue(Esi, IntToStr(Esi),4,6);
    FRegisterValueList.DbgRegisterAutoCreate['edi'].SetValue(Edi, IntToStr(Edi),4,7);
    FRegisterValueList.DbgRegisterAutoCreate['eip'].SetValue(Eip, IntToStr(Eip),4,8);

    FRegisterValueList.DbgRegisterAutoCreate['eflags'].Setx86EFlagsValue(EFlags);

    FRegisterValueList.DbgRegisterAutoCreate['cs'].SetValue(SegCs, IntToStr(SegCs),4,51);
    FRegisterValueList.DbgRegisterAutoCreate['ss'].SetValue(SegSs, IntToStr(SegSs),4,52);
    FRegisterValueList.DbgRegisterAutoCreate['ds'].SetValue(SegDs, IntToStr(SegDs),4,53);
    FRegisterValueList.DbgRegisterAutoCreate['es'].SetValue(SegEs, IntToStr(SegEs),4,50);
    FRegisterValueList.DbgRegisterAutoCreate['fs'].SetValue(SegFs, IntToStr(SegFs),4,54);
    FRegisterValueList.DbgRegisterAutoCreate['gs'].SetValue(SegGs, IntToStr(SegGs),4,55);

    FRegisterValueList.DbgRegisterAutoCreate['st0'].SetValue(0, FloatToStr(PExtended(@FloatSave.RegisterArea[ 0])^),10,33);
    FRegisterValueList.DbgRegisterAutoCreate['st1'].SetValue(0, FloatToStr(PExtended(@FloatSave.RegisterArea[10])^),10,34);
    FRegisterValueList.DbgRegisterAutoCreate['st2'].SetValue(0, FloatToStr(PExtended(@FloatSave.RegisterArea[20])^),10,35);
    FRegisterValueList.DbgRegisterAutoCreate['st3'].SetValue(0, FloatToStr(PExtended(@FloatSave.RegisterArea[30])^),10,36);
    FRegisterValueList.DbgRegisterAutoCreate['st4'].SetValue(0, FloatToStr(PExtended(@FloatSave.RegisterArea[40])^),10,37);
    FRegisterValueList.DbgRegisterAutoCreate['st5'].SetValue(0, FloatToStr(PExtended(@FloatSave.RegisterArea[50])^),10,38);
    FRegisterValueList.DbgRegisterAutoCreate['st6'].SetValue(0, FloatToStr(PExtended(@FloatSave.RegisterArea[60])^),10,39);
    FRegisterValueList.DbgRegisterAutoCreate['st7'].SetValue(0, FloatToStr(PExtended(@FloatSave.RegisterArea[70])^),10,40);

    FRegisterValueList.DbgRegisterAutoCreate['fctrl'  ].SetValue(FloatSave.ControlWord,   IntToStr(FloatSave.ControlWord),2,510);
    FRegisterValueList.DbgRegisterAutoCreate['fstat'  ].SetValue(FloatSave.StatusWord,    IntToStr(FloatSave.StatusWord),2,511);
    FRegisterValueList.DbgRegisterAutoCreate['ftag'   ].SetValue(FloatSave.TagWord,       IntToStr(FloatSave.TagWord),1,512);
    //FRegisterValueList.DbgRegisterAutoCreate['fErrOp' ].SetValue(FloatSave.ErrorOpcode,   IntToStr(FloatSave.ErrorOpcode),2,513);
    FRegisterValueList.DbgRegisterAutoCreate['fErrOff'].SetValue(FloatSave.ErrorOffset,   IntToStr(FloatSave.ErrorOffset),4,514);
    FRegisterValueList.DbgRegisterAutoCreate['fErrSel'].SetValue(FloatSave.ErrorSelector, IntToStr(FloatSave.ErrorSelector),2,515);
    FRegisterValueList.DbgRegisterAutoCreate['fDatOff'].SetValue(FloatSave.DataOffset,    IntToStr(FloatSave.DataOffset),4,516);
    FRegisterValueList.DbgRegisterAutoCreate['fDatSel'].SetValue(FloatSave.DataSelector,  IntToStr(FloatSave.DataSelector),2,517);
    FRegisterValueList.DbgRegisterAutoCreate['fCr0NpxSt'].SetValue(FloatSave.Cr0NpxState, IntToStr(FloatSave.Cr0NpxState),4,518);

    if not FFailed_CONTEXT_EXTENDED_REGISTERS then begin
      FRegisterValueList.DbgRegisterAutoCreate['Xmm0'].SetValue(@ExtendedRegisters[10*16],16,17, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm1'].SetValue(@ExtendedRegisters[11*16],16,18, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm2'].SetValue(@ExtendedRegisters[12*16],16,19, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm3'].SetValue(@ExtendedRegisters[13*16],16,20, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm4'].SetValue(@ExtendedRegisters[14*16],16,21, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm5'].SetValue(@ExtendedRegisters[15*16],16,22, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm6'].SetValue(@ExtendedRegisters[16*16],16,23, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm7'].SetValue(@ExtendedRegisters[17*16],16,24, @XmmToFormat);

      FRegisterValueList.DbgRegisterAutoCreate['MxCsr'].SetValue(PDWORD(@ExtendedRegisters[24])^,  IntToStr(PDWORD(@ExtendedRegisters[24])^),4,620);
    end;
  end;
{$else}
  if (TDbgWinProcess(Process).Bitness = b32) then
  with FCurrentContext^.WOW do
  begin
    FRegisterValueList.DbgRegisterAutoCreate['eax'].SetValue(Eax, IntToStr(Eax),4,0);
    FRegisterValueList.DbgRegisterAutoCreate['ecx'].SetValue(Ecx, IntToStr(Ecx),4,1);
    FRegisterValueList.DbgRegisterAutoCreate['edx'].SetValue(Edx, IntToStr(Edx),4,2);
    FRegisterValueList.DbgRegisterAutoCreate['ebx'].SetValue(Ebx, IntToStr(Ebx),4,3);
    FRegisterValueList.DbgRegisterAutoCreate['esp'].SetValue(Esp, IntToStr(Esp),4,4);
    FRegisterValueList.DbgRegisterAutoCreate['ebp'].SetValue(Ebp, IntToStr(Ebp),4,5);
    FRegisterValueList.DbgRegisterAutoCreate['esi'].SetValue(Esi, IntToStr(Esi),4,6);
    FRegisterValueList.DbgRegisterAutoCreate['edi'].SetValue(Edi, IntToStr(Edi),4,7);
    FRegisterValueList.DbgRegisterAutoCreate['eip'].SetValue(Eip, IntToStr(Eip),4,8);

    FRegisterValueList.DbgRegisterAutoCreate['eflags'].Setx86EFlagsValue(EFlags); // dwarf 49

    FRegisterValueList.DbgRegisterAutoCreate['cs'].SetValue(SegCs, IntToStr(SegCs),4,51);
    FRegisterValueList.DbgRegisterAutoCreate['ss'].SetValue(SegSs, IntToStr(SegSs),4,52);
    FRegisterValueList.DbgRegisterAutoCreate['ds'].SetValue(SegDs, IntToStr(SegDs),4,53);
    FRegisterValueList.DbgRegisterAutoCreate['es'].SetValue(SegEs, IntToStr(SegEs),4,50);
    FRegisterValueList.DbgRegisterAutoCreate['fs'].SetValue(SegFs, IntToStr(SegFs),4,54);
    FRegisterValueList.DbgRegisterAutoCreate['gs'].SetValue(SegGs, IntToStr(SegGs),4,55);

  // TODO: 64bit extended is not 10 byte // currently downgrading to double
    {$IF FPC_Fullversion>30202}
    FRegisterValueList.DbgRegisterAutoCreate['st0'].SetValue(0, FloatToStr(PExtended(@FloatSave.RegisterArea[ 0])^),10,33);
    FRegisterValueList.DbgRegisterAutoCreate['st1'].SetValue(0, FloatToStr(PExtended(@FloatSave.RegisterArea[10])^),10,34);
    FRegisterValueList.DbgRegisterAutoCreate['st2'].SetValue(0, FloatToStr(PExtended(@FloatSave.RegisterArea[20])^),10,35);
    FRegisterValueList.DbgRegisterAutoCreate['st3'].SetValue(0, FloatToStr(PExtended(@FloatSave.RegisterArea[30])^),10,36);
    FRegisterValueList.DbgRegisterAutoCreate['st4'].SetValue(0, FloatToStr(PExtended(@FloatSave.RegisterArea[40])^),10,37);
    FRegisterValueList.DbgRegisterAutoCreate['st5'].SetValue(0, FloatToStr(PExtended(@FloatSave.RegisterArea[50])^),10,38);
    FRegisterValueList.DbgRegisterAutoCreate['st6'].SetValue(0, FloatToStr(PExtended(@FloatSave.RegisterArea[60])^),10,39);
    FRegisterValueList.DbgRegisterAutoCreate['st7'].SetValue(0, FloatToStr(PExtended(@FloatSave.RegisterArea[70])^),10,40);
    {$ENDIF}

    FRegisterValueList.DbgRegisterAutoCreate['fctrl'  ].SetValue(FloatSave.ControlWord,   IntToStr(FloatSave.ControlWord),2,510);
    FRegisterValueList.DbgRegisterAutoCreate['fstat'  ].SetValue(FloatSave.StatusWord,    IntToStr(FloatSave.StatusWord),2,511);
    FRegisterValueList.DbgRegisterAutoCreate['ftag'   ].SetValue(FloatSave.TagWord,       IntToStr(FloatSave.TagWord),1,512);
    //FRegisterValueList.DbgRegisterAutoCreate['fErrOp' ].SetValue(FloatSave.ErrorOpcode,   IntToStr(FloatSave.ErrorOpcode),2,513);
    FRegisterValueList.DbgRegisterAutoCreate['fErrOff'].SetValue(FloatSave.ErrorOffset,   IntToStr(FloatSave.ErrorOffset),4,514);
    FRegisterValueList.DbgRegisterAutoCreate['fErrSel'].SetValue(FloatSave.ErrorSelector, IntToStr(FloatSave.ErrorSelector),2,515);
    FRegisterValueList.DbgRegisterAutoCreate['fDatOff'].SetValue(FloatSave.DataOffset,    IntToStr(FloatSave.DataOffset),4,516);
    FRegisterValueList.DbgRegisterAutoCreate['fDatSel'].SetValue(FloatSave.DataSelector,  IntToStr(FloatSave.DataSelector),2,517);
    FRegisterValueList.DbgRegisterAutoCreate['fCr0NpxSt'].SetValue(FloatSave.Cr0NpxState, IntToStr(FloatSave.Cr0NpxState),4,518);

    if not FFailed_CONTEXT_EXTENDED_REGISTERS then begin
      FRegisterValueList.DbgRegisterAutoCreate['Xmm0'].SetValue(@ExtendedRegisters[10*16],16,17, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm1'].SetValue(@ExtendedRegisters[11*16],16,18, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm2'].SetValue(@ExtendedRegisters[12*16],16,19, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm3'].SetValue(@ExtendedRegisters[13*16],16,20, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm4'].SetValue(@ExtendedRegisters[14*16],16,21, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm5'].SetValue(@ExtendedRegisters[15*16],16,22, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm6'].SetValue(@ExtendedRegisters[16*16],16,23, @XmmToFormat);
      FRegisterValueList.DbgRegisterAutoCreate['Xmm7'].SetValue(@ExtendedRegisters[17*16],16,24, @XmmToFormat);

      FRegisterValueList.DbgRegisterAutoCreate['MxCsr'].SetValue(PDWORD(@ExtendedRegisters[24])^,  IntToStr(PDWORD(@ExtendedRegisters[24])^),4,620);
    end;
  end
  else
  with FCurrentContext^.def do
  begin
    FRegisterValueList.DbgRegisterAutoCreate['rax'].SetValue(rax, IntToStr(rax),8,0);
    FRegisterValueList.DbgRegisterAutoCreate['rbx'].SetValue(rbx, IntToStr(rbx),8,3);
    FRegisterValueList.DbgRegisterAutoCreate['rcx'].SetValue(rcx, IntToStr(rcx),8,2);
    FRegisterValueList.DbgRegisterAutoCreate['rdx'].SetValue(rdx, IntToStr(rdx),8,1);
    FRegisterValueList.DbgRegisterAutoCreate['rsi'].SetValue(rsi, IntToStr(rsi),8,4);
    FRegisterValueList.DbgRegisterAutoCreate['rdi'].SetValue(rdi, IntToStr(rdi),8,5);
    FRegisterValueList.DbgRegisterAutoCreate['rbp'].SetValue(rbp, IntToStr(rbp),8,6);
    FRegisterValueList.DbgRegisterAutoCreate['rsp'].SetValue(rsp, IntToStr(rsp),8,7);

    FRegisterValueList.DbgRegisterAutoCreate['r8'].SetValue(r8, IntToStr(r8),8,8);
    FRegisterValueList.DbgRegisterAutoCreate['r9'].SetValue(r9, IntToStr(r9),8,9);
    FRegisterValueList.DbgRegisterAutoCreate['r10'].SetValue(r10, IntToStr(r10),8,10);
    FRegisterValueList.DbgRegisterAutoCreate['r11'].SetValue(r11, IntToStr(r11),8,11);
    FRegisterValueList.DbgRegisterAutoCreate['r12'].SetValue(r12, IntToStr(r12),8,12);
    FRegisterValueList.DbgRegisterAutoCreate['r13'].SetValue(r13, IntToStr(r13),8,13);
    FRegisterValueList.DbgRegisterAutoCreate['r14'].SetValue(r14, IntToStr(r14),8,14);
    FRegisterValueList.DbgRegisterAutoCreate['r15'].SetValue(r15, IntToStr(r15),8,15);

    FRegisterValueList.DbgRegisterAutoCreate['rip'].SetValue(rip, IntToStr(rip),8,16);
    FRegisterValueList.DbgRegisterAutoCreate['eflags'].Setx86EFlagsValue(EFlags);

    FRegisterValueList.DbgRegisterAutoCreate['cs'].SetValue(SegCs, IntToStr(SegCs),8,51);
    FRegisterValueList.DbgRegisterAutoCreate['ss'].SetValue(SegSs, IntToStr(SegSs),8,52);
    FRegisterValueList.DbgRegisterAutoCreate['ds'].SetValue(SegDs, IntToStr(SegDs),8,53);
    FRegisterValueList.DbgRegisterAutoCreate['es'].SetValue(SegEs, IntToStr(SegEs),8,50);
    FRegisterValueList.DbgRegisterAutoCreate['fs'].SetValue(SegFs, IntToStr(SegFs),8,54);
    FRegisterValueList.DbgRegisterAutoCreate['gs'].SetValue(SegGs, IntToStr(SegGs),8,55);

  // TODO: 64bit extended is not 10 byte // currently downgrading to double
    {$IF FPC_Fullversion>30202}
    FRegisterValueList.DbgRegisterAutoCreate['st0'].SetValue(0, FloatToStr(PExtended(@FltSave.FloatRegisters[0])^),10,500);
    FRegisterValueList.DbgRegisterAutoCreate['st1'].SetValue(0, FloatToStr(PExtended(@FltSave.FloatRegisters[1])^),10,501);
    FRegisterValueList.DbgRegisterAutoCreate['st2'].SetValue(0, FloatToStr(PExtended(@FltSave.FloatRegisters[2])^),10,502);
    FRegisterValueList.DbgRegisterAutoCreate['st3'].SetValue(0, FloatToStr(PExtended(@FltSave.FloatRegisters[3])^),10,503);
    FRegisterValueList.DbgRegisterAutoCreate['st4'].SetValue(0, FloatToStr(PExtended(@FltSave.FloatRegisters[4])^),10,504);
    FRegisterValueList.DbgRegisterAutoCreate['st5'].SetValue(0, FloatToStr(PExtended(@FltSave.FloatRegisters[5])^),10,505);
    FRegisterValueList.DbgRegisterAutoCreate['st6'].SetValue(0, FloatToStr(PExtended(@FltSave.FloatRegisters[6])^),10,506);
    FRegisterValueList.DbgRegisterAutoCreate['st7'].SetValue(0, FloatToStr(PExtended(@FltSave.FloatRegisters[7])^),10,507);
    {$ENDIF}

    FRegisterValueList.DbgRegisterAutoCreate['fctrl'  ].SetValue(FltSave.ControlWord,   IntToStr(FltSave.ControlWord),2,510);
    FRegisterValueList.DbgRegisterAutoCreate['fstat'  ].SetValue(FltSave.StatusWord,    IntToStr(FltSave.StatusWord),2,511);
    FRegisterValueList.DbgRegisterAutoCreate['ftag'   ].SetValue(FltSave.TagWord,       IntToStr(FltSave.TagWord),1,512);
    FRegisterValueList.DbgRegisterAutoCreate['fErrOp' ].SetValue(FltSave.ErrorOpcode,   IntToStr(FltSave.ErrorOpcode),2,513);
    FRegisterValueList.DbgRegisterAutoCreate['fErrOff'].SetValue(FltSave.ErrorOffset,   IntToStr(FltSave.ErrorOffset),4,514);
    FRegisterValueList.DbgRegisterAutoCreate['fErrSel'].SetValue(FltSave.ErrorSelector, IntToStr(FltSave.ErrorSelector),2,515);
    FRegisterValueList.DbgRegisterAutoCreate['fDatOff'].SetValue(FltSave.DataOffset,    IntToStr(FltSave.DataOffset),4,516);
    FRegisterValueList.DbgRegisterAutoCreate['fDatSel'].SetValue(FltSave.DataSelector,  IntToStr(FltSave.DataSelector),2,517);

    FRegisterValueList.DbgRegisterAutoCreate['Xmm0' ].SetValue(@FltSave.XmmRegisters[ 0],16,17, @XmmToFormat);
    FRegisterValueList.DbgRegisterAutoCreate['Xmm1' ].SetValue(@FltSave.XmmRegisters[ 1],16,18, @XmmToFormat);
    FRegisterValueList.DbgRegisterAutoCreate['Xmm2' ].SetValue(@FltSave.XmmRegisters[ 2],16,19, @XmmToFormat);
    FRegisterValueList.DbgRegisterAutoCreate['Xmm3' ].SetValue(@FltSave.XmmRegisters[ 3],16,20, @XmmToFormat);
    FRegisterValueList.DbgRegisterAutoCreate['Xmm4' ].SetValue(@FltSave.XmmRegisters[ 4],16,21, @XmmToFormat);
    FRegisterValueList.DbgRegisterAutoCreate['Xmm5' ].SetValue(@FltSave.XmmRegisters[ 5],16,22, @XmmToFormat);
    FRegisterValueList.DbgRegisterAutoCreate['Xmm6' ].SetValue(@FltSave.XmmRegisters[ 6],16,23, @XmmToFormat);
    FRegisterValueList.DbgRegisterAutoCreate['Xmm7' ].SetValue(@FltSave.XmmRegisters[ 7],16,24, @XmmToFormat);
    FRegisterValueList.DbgRegisterAutoCreate['Xmm8' ].SetValue(@FltSave.XmmRegisters[ 8],16,25, @XmmToFormat);
    FRegisterValueList.DbgRegisterAutoCreate['Xmm9' ].SetValue(@FltSave.XmmRegisters[ 9],16,26, @XmmToFormat);
    FRegisterValueList.DbgRegisterAutoCreate['Xmm10'].SetValue(@FltSave.XmmRegisters[10],16,27, @XmmToFormat);
    FRegisterValueList.DbgRegisterAutoCreate['Xmm11'].SetValue(@FltSave.XmmRegisters[11],16,28, @XmmToFormat);
    FRegisterValueList.DbgRegisterAutoCreate['Xmm12'].SetValue(@FltSave.XmmRegisters[12],16,29, @XmmToFormat);
    FRegisterValueList.DbgRegisterAutoCreate['Xmm13'].SetValue(@FltSave.XmmRegisters[13],16,30, @XmmToFormat);
    FRegisterValueList.DbgRegisterAutoCreate['Xmm14'].SetValue(@FltSave.XmmRegisters[14],16,31, @XmmToFormat);
    FRegisterValueList.DbgRegisterAutoCreate['Xmm15'].SetValue(@FltSave.XmmRegisters[15],16,31, @XmmToFormat);

    FRegisterValueList.DbgRegisterAutoCreate['MxCsr'].SetValue(FltSave.MxCsr,  IntToStr(FltSave.MxCsr),4,620);
    FRegisterValueList.DbgRegisterAutoCreate['MxCsrM'].SetValue(FltSave.MxCsr_Mask,  IntToStr(FltSave.MxCsr_Mask),4,621);
  end;
  {$endif} // 64bit

  if _GetEnabledXStateFeatures <> nil then begin
    ContextSize := 0;

    if _InitializeContext(nil, CONTEXT_ALL or CONTEXT_XSTATE, nil, @ContextSize) or
       (GetLastError <> ERROR_INSUFFICIENT_BUFFER)
    then
      exit;

    Buffer := AllocMem(ContextSize+$40);
    if Buffer = nil then
      exit;
    Buffer2 := AlignPtr(Buffer, $40);

    try
      if not _InitializeContext(Buffer2, CONTEXT_ALL or CONTEXT_XSTATE, @Context, @ContextSize) then
        exit;
      if not _SetXStateFeaturesMask(Context, XSTATE_MASK_AVX) then
        exit;
      if not  GetThreadContext(Handle, Context^) then // context is VAR PARAM
        exit;

      Xmm := _LocateXStateFeature(Context, XSTATE_LEGACY_SSE, @FeatureLength);
      Ymm := _LocateXStateFeature(Context, XSTATE_AVX, @FeatureLength2);
      if (Xmm = nil) or (Ymm = nil) or (FeatureLength2 = 0) then
        exit;
      {$ifdef cpux86_64}
      if (TDbgWinProcess(Process).Bitness = b32) and (FeatureLength > 8 * SizeOf(M128A)) then
        FeatureLength := 8 * SizeOf(M128A);
      {$endif}

      if (_GetXStateFeaturesMask(Context, @FeatureMask)) and
         ((FeatureMask and XSTATE_MASK_AVX) = 0)
      then begin
        // AVX not init yet // upper half must be 0
        for i := 0 to FeatureLength div SizeOf(M128A) - 1 do begin
          r := FRegisterValueList.DbgRegisterAutoCreate['Ymm'+IntToStr(i)];
          r.SetValue(@Xmm[i],32,700+i, @YmmToFormat);
          FillByte(PByte(r.Data+16)^, 16, 0);
        end;
      end
      else begin
        for i := 0 to FeatureLength div SizeOf(M128A) - 1 do begin
          r := FRegisterValueList.DbgRegisterAutoCreate['Ymm'+IntToStr(i)];
          r.SetValue(@Xmm[i],32,700+i, @YmmToFormat);
          move(Ymm[i], PByte(r.Data+16)^, 16);
        end;
      end;

    finally
      Freemem(Buffer);
    end;
  end;

  finally
    FRegisterValueListValid:=true;
    EnableFloatExceptions;
  end;
end;

function TDbgWinX86Thread.GetFpThreadContext(var AStorage: TFpContext; out ACtxPtr: PFpContext;
  ACtxFlags: TFpWinCtxFlags): Boolean;
begin
  ACtxPtr := AlignPtr(@AStorage, $10);

  if not FFailed_CONTEXT_EXTENDED_REGISTERS then begin
    SetLastError(0);
    {$ifdef cpux86_64}
    if (TDbgWinProcess(Process).Bitness = b32) then begin
      case ACtxFlags of
        cfControl: ACtxPtr^.WOW.ContextFlags := WOW64_CONTEXT_CONTROL;
        cfFull:    ACtxPtr^.WOW.ContextFlags := WOW64_CONTEXT_SEGMENTS or WOW64_CONTEXT_INTEGER or WOW64_CONTEXT_CONTROL or WOW64_CONTEXT_DEBUG_REGISTERS or WOW64_CONTEXT_FLOATING_POINT or WOW64_CONTEXT_EXTENDED_REGISTERS;
      end;
      Result := (_Wow64GetThreadContext <> nil) and _Wow64GetThreadContext(Handle, ACtxPtr^.WOW);
    end
    else begin
    {$endif}
      case ACtxFlags of
        cfControl: ACtxPtr^.def.ContextFlags := CONTEXT_CONTROL;
        {$ifdef cpui386}
        cfFull:    ACtxPtr^.def.ContextFlags := CONTEXT_SEGMENTS or CONTEXT_INTEGER or CONTEXT_CONTROL or CONTEXT_DEBUG_REGISTERS or CONTEXT_FLOATING_POINT or CONTEXT_EXTENDED_REGISTERS;
        {$else}
        cfFull:    ACtxPtr^.def.ContextFlags := CONTEXT_SEGMENTS or CONTEXT_INTEGER or CONTEXT_CONTROL or CONTEXT_DEBUG_REGISTERS or CONTEXT_FLOATING_POINT;
        {$endif}
      end;
  (* or CONTEXT_FLOATING_POINT or CONTEXT_EXTENDED_REGISTERS *)
      Result := GetThreadContext(Handle, ACtxPtr^.def_w);
    {$ifdef cpux86_64}
    end;
    {$endif}
    if GetLastError <> 0 then
      FFailed_CONTEXT_EXTENDED_REGISTERS := True;
    DebugLn(DBG_WARNINGS and (not Result), ['Unable to get Context for ', ID, ': ', GetLastErrorText, ' ', FFailed_CONTEXT_EXTENDED_REGISTERS]);
  end;

  if FFailed_CONTEXT_EXTENDED_REGISTERS then begin

    SetLastError(0);
    {$ifdef cpux86_64}
    if (TDbgWinProcess(Process).Bitness = b32) then begin
      case ACtxFlags of
        cfControl: ACtxPtr^.WOW.ContextFlags := WOW64_CONTEXT_CONTROL;
        cfFull:    ACtxPtr^.WOW.ContextFlags := WOW64_CONTEXT_SEGMENTS or WOW64_CONTEXT_INTEGER or WOW64_CONTEXT_CONTROL or WOW64_CONTEXT_DEBUG_REGISTERS or WOW64_CONTEXT_FLOATING_POINT;
      end;
      Result := (_Wow64GetThreadContext <> nil) and _Wow64GetThreadContext(Handle, ACtxPtr^.WOW);
    end
    else begin
    {$endif}
      case ACtxFlags of
        cfControl: ACtxPtr^.def.ContextFlags := CONTEXT_CONTROL;
        cfFull:    ACtxPtr^.def.ContextFlags := CONTEXT_SEGMENTS or CONTEXT_INTEGER or CONTEXT_CONTROL or CONTEXT_DEBUG_REGISTERS or CONTEXT_FLOATING_POINT;
      end;
      Result := GetThreadContext(Handle, ACtxPtr^.def_w);
    {$ifdef cpux86_64}
    end;
    {$endif}
    DebugLn(DBG_WARNINGS and (not Result), ['Unable to get Context for ', ID, ': ', GetLastErrorText]);
  end;
end;

function TDbgWinX86Thread.SetFpThreadContext(ACtxPtr: PFpContext; ACtxFlags: TFpWinCtxFlags
  ): Boolean;
begin
  SetLastError(0);
  {$ifdef cpux86_64}
  if (TDbgWinProcess(Process).Bitness = b32) then begin
    case ACtxFlags of
      cfControl: ACtxPtr^.WOW.ContextFlags := WOW64_CONTEXT_CONTROL;
      cfFull:    ACtxPtr^.WOW.ContextFlags := WOW64_CONTEXT_SEGMENTS or WOW64_CONTEXT_INTEGER or WOW64_CONTEXT_CONTROL or WOW64_CONTEXT_DEBUG_REGISTERS;
    end;
    if ccfControl in FThreadContextChangeFlags then
      ACtxPtr^.def.ContextFlags := ACtxPtr^.def.ContextFlags or WOW64_CONTEXT_CONTROL;
    if ccfInteger in FThreadContextChangeFlags then
      ACtxPtr^.def.ContextFlags := ACtxPtr^.def.ContextFlags or WOW64_CONTEXT_INTEGER;
    Result := (_Wow64SetThreadContext <> nil) and _Wow64SetThreadContext(Handle, ACtxPtr^.WOW);
  end
  else begin
  {$endif}
    case ACtxFlags of
      cfControl: ACtxPtr^.def.ContextFlags := CONTEXT_CONTROL;
      cfFull:    ACtxPtr^.def.ContextFlags := CONTEXT_SEGMENTS or CONTEXT_INTEGER or CONTEXT_CONTROL or CONTEXT_DEBUG_REGISTERS;
    end;
    if ccfControl in FThreadContextChangeFlags then
      ACtxPtr^.def.ContextFlags := ACtxPtr^.def.ContextFlags or CONTEXT_CONTROL;
    if ccfInteger in FThreadContextChangeFlags then
      ACtxPtr^.def.ContextFlags := ACtxPtr^.def.ContextFlags or CONTEXT_INTEGER;
    Result := SetThreadContext(Handle, ACtxPtr^.def_w);
  {$ifdef cpux86_64}
  end;
  {$endif}
  DebugLn(DBG_WARNINGS and (not Result), ['Unable to set Context for ', ID, ': ', GetLastErrorText]);
end;

function TDbgWinX86Thread.HasContext: Boolean;
begin
  Result := FCurrentContext <> nil;
end;

procedure TDbgWinX86Thread.DumpContext;
  var
    f: Cardinal;
    n: integer;
begin
  {$PUSH}{$R-}
    {$ifdef cpui386}
    with FCurrentContext^.def do DebugLn(Format('DS: 0x%x, ES: 0x%x, FS: 0x%x, GS: 0x%x', [SegDs, SegEs, SegFs, SegGs]));
    with FCurrentContext^.def do DebugLn(Format('EAX: 0x%x, EBX: 0x%x, ECX: 0x%x, EDX: 0x%x, EDI: 0x%x, ESI: 0x%x', [Eax, Ebx, Ecx, Edx, Edi, Esi]));
    with FCurrentContext^.def do DebugLn(Format('CS: 0x%x, SS: 0x%x, EBP: 0x%x, EIP: 0x%x, ESP: 0x%x, EFlags: 0x%x [', [SegCs, SegSs, Ebp, Eip, Esp, EFlags]));
    {$else}
// TODO: if bitness
    with FCurrentContext^.def do DebugLn(Format('SegDS: 0x%4.4x, SegES: 0x%4.4x, SegFS: 0x%4.4x, SegGS: 0x%4.4x', [SegDs, SegEs, SegFs, SegGs]));
    with FCurrentContext^.def do DebugLn(Format('RAX: 0x%16.16x, RBX: 0x%16.16x, RCX: 0x%16.16x, RDX: 0x%16.16x, RDI: 0x%16.16x, RSI: 0x%16.16x, R9: 0x%16.16x, R10: 0x%16.16x, R11: 0x%16.16x, R12: 0x%16.16x, R13: 0x%16.16x, R14: 0x%16.16x, R15: 0x%16.16x', [Rax, Rbx, Rcx, Rdx, Rdi, Rsi, R9, R10, R11, R12, R13, R14, R15]));
    with FCurrentContext^.def do DebugLn(Format('SegCS: 0x%4.4x, SegSS: 0x%4.4x, RBP: 0x%16.16x, RIP: 0x%16.16x, RSP: 0x%16.16x, EFlags: 0x%8.8x [', [SegCs, SegSs, Rbp, Rip, Rsp, EFlags]));
    {$endif}
    // luckely flag and debug registers are named the same
    with FCurrentContext^.def do
    begin
      if EFlags and (1 shl 0) <> 0 then DebugLn('CF ');
      if EFlags and (1 shl 2) <> 0 then DebugLn('PF ');
      if EFlags and (1 shl 4) <> 0 then DebugLn('AF ');
      if EFlags and (1 shl 6) <> 0 then DebugLn('ZF ');
      if EFlags and (1 shl 7) <> 0 then DebugLn('SF ');
      if EFlags and (1 shl 8) <> 0 then DebugLn('TF ');
      if EFlags and (1 shl 9) <> 0 then DebugLn('IF ');
      if EFlags and (1 shl 10) <> 0 then DebugLn('DF ');
      if EFlags and (1 shl 11) <> 0 then DebugLn('OF ');
      if (EFlags shr 12) and 3 <> 0 then DebugLn('IOPL=', IntToSTr((EFlags shr 12) and 3));
      if EFlags and (1 shl 14) <> 0 then DebugLn('NT ');
      if EFlags and (1 shl 16) <> 0 then DebugLn('RF ');
      if EFlags and (1 shl 17) <> 0 then DebugLn('VM ');
      if EFlags and (1 shl 18) <> 0 then DebugLn('AC ');
      if EFlags and (1 shl 19) <> 0 then DebugLn('VIF ');
      if EFlags and (1 shl 20) <> 0 then DebugLn('VIP ');
      if EFlags and (1 shl 21) <> 0 then DebugLn('ID ');
      DebugLn(']');

      DebugLn(Format('DR0: 0x%x, DR1: 0x%x, DR2: 0x%x, DR3: 0x%x', [Dr0, Dr1, Dr2, Dr3]));
      DebugLn(' DR6: 0x', IntToHex(Dr6, SizeOf(Pointer) * 2), ' [');
      if Dr6 and $0001 <> 0 then DebugLn('B0 ');
      if Dr6 and $0002 <> 0 then DebugLn('B1 ');
      if Dr6 and $0004 <> 0 then DebugLn('B2 ');
      if Dr6 and $0008 <> 0 then DebugLn('B3 ');
      if Dr6 and $2000 <> 0 then DebugLn('BD ');
      if Dr6 and $4000 <> 0 then DebugLn('BS ');
      if Dr6 and $8000 <> 0 then DebugLn('BT ');
      DebugLn('] DR7: 0x', IntToHex(Dr7, SizeOf(Pointer) * 2), ' [');
      if Dr7 and $01 <> 0 then DebugLn('L0 ');
      if Dr7 and $02 <> 0 then DebugLn('G0 ');
      if Dr7 and $04 <> 0 then DebugLn('L1 ');
      if Dr7 and $08 <> 0 then DebugLn('G1 ');
      if Dr7 and $10 <> 0 then DebugLn('L2 ');
      if Dr7 and $20 <> 0 then DebugLn('G2 ');
      if Dr7 and $40 <> 0 then DebugLn('L3 ');
      if Dr7 and $80 <> 0 then DebugLn('G3 ');
      if Dr7 and $100 <> 0 then DebugLn('LE ');
      if Dr7 and $200 <> 0 then DebugLn('GE ');
      if Dr7 and $2000 <> 0 then DebugLn('GD ');
      f := Dr7 shr 16;
      for n := 0 to 3 do
      begin
        DebugLn('R/W', IntToSTr(n),':');
        case f and 3 of
          0: DebugLn('ex');
          1: DebugLn('wo');
          2: DebugLn('IO');
          3: DebugLn('rw');
        end;
        f := f shr 2;
        DebugLn(' LEN', IntToSTr(n),':', IntToSTr(f and 3 + 1), ' ');
        f := f shr 2;
      end;
      DebugLn(']');
    end;
    DebugLn('---');
  {$POP}
end;

procedure TDbgWinX86Thread.SetSingleStep;
begin
  NextIsSingleStep := True;

  if FCurrentContext = nil then
    if not ReadThreadState then
      exit;
  {$ifdef cpux86_64}
  if (TDbgWinProcess(Process).Bitness = b32) then
    FCurrentContext^.WOW.EFlags := FCurrentContext^.WOW.EFlags or FLAG_TRACE_BIT // TODO WOW_FLAG....
  else
  {$endif}
    FCurrentContext^.def.EFlags := FCurrentContext^.def.EFlags or FLAG_TRACE_BIT;
  FThreadContextChanged:=true;
end;

procedure TDbgWinX86Thread.ApplyWatchPoints(AWatchPointData: TFpWatchPointData);
begin
  if (udeKeepExternalWatchPointData in Process.HandleUserDebugEvents) and
     (TFpIntelWatchPointData(AWatchPointData).Dr7 = 0)
  then
    exit;
  if FCurrentContext = nil then
    if not ReadThreadState then
      exit;
  {$ifdef cpux86_64}
  if (TDbgWinProcess(Process).Bitness = b32) then begin
    with FCurrentContext^.WOW do begin
      Dr0 := DWORD(TFpIntelWatchPointData(AWatchPointData).Dr03[0]);
      Dr1 := DWORD(TFpIntelWatchPointData(AWatchPointData).Dr03[1]);
      Dr2 := DWORD(TFpIntelWatchPointData(AWatchPointData).Dr03[2]);
      Dr3 := DWORD(TFpIntelWatchPointData(AWatchPointData).Dr03[3]);
      Dr7 := (Dr7 and $0000FF00) or DWORD(TFpIntelWatchPointData(AWatchPointData).Dr7);
DebugLn(DBG_VERBOSE, '### WATCH ADDED  dr0 %x  dr1 %x  dr2 %x  dr3 %x      dr7 %x', [ dr0,dr1,dr2,dr3, dr7]);
    end;
  end
  else begin
  {$endif}
    with FCurrentContext^.def do begin
      Dr0 := TFpIntelWatchPointData(AWatchPointData).Dr03[0];
      Dr1 := TFpIntelWatchPointData(AWatchPointData).Dr03[1];
      Dr2 := TFpIntelWatchPointData(AWatchPointData).Dr03[2];
      Dr3 := TFpIntelWatchPointData(AWatchPointData).Dr03[3];
      Dr7 := (Dr7 and $0000FF00) or TFpIntelWatchPointData(AWatchPointData).Dr7;
DebugLn(DBG_VERBOSE, '### WATCH ADDED   dr0 %x  dr1 %x  dr2 %x  dr3 %x      dr7 %x', [ dr0,dr1,dr2,dr3, dr7]);
    end;
  {$ifdef cpux86_64}
  end;
  {$endif}
  FThreadContextChanged:=true;
end;

function TDbgWinX86Thread.DetectHardwareWatchpoint: TFpInternalWatchpoint;
var
  Dr6: DWORD64;
  wd: TFpIntelWatchPointData;
begin
  result := nil;
  if FCurrentContext = nil then
    if not ReadThreadState then
      exit;

  {$ifdef cpux86_64}
  if (TDbgWinProcess(Process).Bitness = b32) then begin
    Dr6 := DWORD64(FCurrentContext^.WOW.Dr6);
  end
  else begin
  {$endif}
    Dr6 := FCurrentContext^.def.Dr6;
  {$ifdef cpux86_64}
  end;
  {$endif}

  wd := TFpIntelWatchPointData(Process.WatchPointData);
  if dr6 and 1 = 1 then result := wd.Owner[0]
  else if dr6 and 2 = 2 then result := wd.Owner[1]
  else if dr6 and 4 = 4 then result := wd.Owner[2]
  else if dr6 and 8 = 8 then result := wd.Owner[3];
  if (Result = nil) and ((dr6 and 15) <> 0) then
    FHitExternalWatchPoint := True; // not set by the debugger
end;

procedure TDbgWinX86Thread.BeforeContinue;
begin
  inherited;
  if ID = MDebugEvent.dwThreadId then begin
    FHasExceptionCleared := False;

    {$ifdef cpux86_64}
    if (TDbgWinProcess(Process).Bitness = b32) then begin
      if (FCurrentContext <> nil) and
         (FCurrentContext^.WOW.Dr6 <> $ffff0ff0) then
      begin
        FCurrentContext^.WOW.Dr6:=$ffff0ff0;
        FThreadContextChanged:=true;
      end;
    end
    else begin
    {$endif}
      if (FCurrentContext <> nil) and
         (FCurrentContext^.def.Dr6 <> $ffff0ff0) then
      begin
        FCurrentContext^.def.Dr6:=$ffff0ff0;
        FThreadContextChanged:=true;
      end;
    {$ifdef cpux86_64}
    end;
    {$endif}
  end;

  if FThreadContextChanged then
  begin
    Assert(FCurrentContext <> nil, 'TDbgWinThread.BeforeContinue: none existing context was changed');
    if not SetFpThreadContext(FCurrentContext) then
      debugln(FPDBG_WINDOWS or DBG_WARNINGS, ['Failed to SetFpThreadContext()']);
  end;
  FThreadContextChanged := False;
  FThreadContextChangeFlags := [];
  FCurrentContext := nil;
end;

function TDbgWinX86Thread.ResetInstructionPointerAfterBreakpoint: boolean;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgWinThread.ResetInstructionPointerAfterBreakpoint');{$ENDIF}
  assert(MDebugEvent.dwProcessId <> 0, 'TDbgWinThread.ResetInstructionPointerAfterBreakpoint: MDebugEvent.dwProcessId <> 0');
  assert((MDebugEvent.Exception.ExceptionRecord.ExceptionCode = EXCEPTION_BREAKPOINT) or (MDebugEvent.Exception.ExceptionRecord.ExceptionCode = STATUS_WX86_BREAKPOINT), 'TDbgWinThread.ResetInstructionPointerAfterBreakpoint: (MDebugEvent.Exception.ExceptionRecord.ExceptionCode = EXCEPTION_BREAKPOINT) or (MDebugEvent.Exception.ExceptionRecord.ExceptionCode = STATUS_WX86_BREAKPOINT)');

  Result := False;

  if not ReadThreadState then
    exit;

  assert(not FHasResetInstructionPointerAfterBreakpoint, 'TDbgWinThread.ResetInstructionPointerAfterBreakpoint: not FHasResetInstructionPointerAfterBreakpoint');
  {$ifdef cpui386}
  FAtHardCodeBreakpoint := CheckForHardcodeBreakPoint(FCurrentContext^.def.Eip - 1);
  if not FAtHardCodeBreakpoint then
    dec(FCurrentContext^.def.Eip);
  {$else}
  if (TDbgWinProcess(Process).Bitness = b32) then begin
    FAtHardCodeBreakpoint := CheckForHardcodeBreakPoint(FCurrentContext^.WOW.Eip - 1);
    if not FAtHardCodeBreakpoint then
      dec(FCurrentContext^.WOW.Eip);
  end
  else begin
    FAtHardCodeBreakpoint := CheckForHardcodeBreakPoint(FCurrentContext^.def.Rip - 1);
    if not FAtHardCodeBreakpoint then
      dec(FCurrentContext^.def.Rip);
  end;
  {$endif}

  FThreadContextChanged := True;
  FHasResetInstructionPointerAfterBreakpoint := True;
  FLastHardcodedSize := 1;
  if Process.BreakTargetHandler is TBreakPointx86Handler then
    FLastHardcodedSize := TBreakPointx86Handler(Process.BreakTargetHandler).LastHardcodedSize;
  Result := True;
end;

function TDbgWinX86Thread.GetAdjustedInstructionPointerRegisterValue: TDbgPtr;
begin
  Result := inherited GetAdjustedInstructionPointerRegisterValue;
  if (Result <> 0) and FAtHardCodeBreakpoint then begin
    dec(Result, FLastHardcodedSize); // must be set, if FAtHardCodeBreakpoint
  end;
end;

function TDbgWinX86Thread.ReadThreadState: boolean;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgWinThread.ReadThreadState');{$ENDIF}
  assert(MDebugEvent.dwProcessId <> 0, 'TDbgWinThread.ReadThreadState: MDebugEvent.dwProcessId <> 0');

  if Process.ProcessID <> MDebugEvent.dwProcessId then begin
    DebugLn(DBG_WARNINGS, 'ERROR: attempt to read threadstate, for wrong process. Thread: %u Thread-Process: %u Event-Process %u', [Id, Process.ProcessID, MDebugEvent.dwProcessId]);
    exit(False);
  end;

  Result := True;
  if FCurrentContext <> nil then
    exit;

  Result := GetFpThreadContext(_UnAligendContext, FCurrentContext, cfFull);
  DebugLn((DBG_WARNINGS or DBG_VERBOSE) and (not Result), ['Failed to read thread-state for ', ID]);
  //FThreadContextChanged := False; TODO: why was that not here?
  FThreadContextChangeFlags := [];
  FRegisterValueListValid:=False;
  FHasResetInstructionPointerAfterBreakpoint := False;
  FAtHardCodeBreakpoint := False;
end;

procedure TDbgWinX86Thread.SetRegisterValue(AName: string; AValue: QWord);
begin
  //{$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgWinThread.SetRegisterValue');{$ENDIF}
  assert(MDebugEvent.dwProcessId <> 0, 'TDbgWinThread.SetRegisterValue: MDebugEvent.dwProcessId <> 0');

  if not ReadThreadState then
    exit;

  {$ifdef cpui386}
    assert((AValue and QWord($ffffffff00000000) = 0) or (AValue and QWord($ffffffff00000000) = QWord($ffffffff00000000)), 'TDbgWinThread.SetRegisterValue: ((AValue and QWord($ffffffff00000000) = 0) or ((AValue and QWord($ffffffff00000000) = QWord($ffffffff00000000)');
    case AName of
      'eip': FCurrentContext^.def.Eip := DWORD(AValue);
      'eax': FCurrentContext^.def.Eax := DWORD(AValue);
      'ecx': FCurrentContext^.def.Ecx := DWORD(AValue);
      'edx': FCurrentContext^.def.Edx := DWORD(AValue);
    else
      raise Exception.CreateFmt('Setting the [%s] register is not supported', [AName]);
    end;
  {$else}
  if (TDbgWinProcess(Process).Bitness = b32) then begin
    assert((AValue and QWord($ffffffff00000000) = 0) or (AValue and QWord($ffffffff00000000) = QWord($ffffffff00000000)), 'TDbgWinThread.SetRegisterValue: ((AValue and QWord($ffffffff00000000) = 0) or ((AValue and QWord($ffffffff00000000) = QWord($ffffffff00000000)');
    case AName of
      'eip': FCurrentContext^.WOW.Eip := DWORD(AValue);
      'eax': FCurrentContext^.WOW.Eax := DWORD(AValue);
      'ecx': FCurrentContext^.WOW.Ecx := DWORD(AValue);
      'edx': FCurrentContext^.WOW.Edx := DWORD(AValue);
    else
      raise Exception.CreateFmt('Setting the [%s] register is not supported', [AName]);
    end;
  end
  else begin
    case AName of
      'rip': FCurrentContext^.def.Rip := AValue;
      'rax': FCurrentContext^.def.Rax := AValue;
      'rcx': FCurrentContext^.def.Rcx := AValue;
      'rdx': FCurrentContext^.def.Rdx := AValue;
      'r8': FCurrentContext^.def.R8 := AValue;
      'r9': FCurrentContext^.def.R9 := AValue;
    else
      raise Exception.CreateFmt('Setting the [%s] register is not supported', [AName]);
    end;
  end;
  {$endif}
  FThreadContextChanged:=True;
  case AName of
    'eip', 'rip': Include(FThreadContextChangeFlags, ccfControl);
    else          Include(FThreadContextChangeFlags, ccfInteger);
  end;
end;

procedure TDbgWinX86Thread.StoreRegisters;
begin
  _StoredContext := _UnAligendContext;
end;

procedure TDbgWinX86Thread.RestoreRegisters;
begin
  _UnAligendContext := _StoredContext;
  FThreadContextChanged := True;
  FRegisterValueListValid := False;
end;

function TDbgWinX86Thread.GetInstructionPointerRegisterValue: TDbgPtr;
begin
  //{$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgWinThread.GetInstructionPointerRegisterValue');{$ENDIF}
  assert(MDebugEvent.dwProcessId <> 0, 'TDbgWinThread.GetInstructionPointerRegisterValue: MDebugEvent.dwProcessId <> 0');

  Result := 0;
  if FCurrentContext = nil then
    if not ReadThreadState then
      exit;
{$ifdef cpui386}
  Result := FCurrentContext^.def.Eip;
{$else}
  if (TDbgWinProcess(Process).Bitness = b32) then
    Result := FCurrentContext^.WOW.Eip
  else
    Result := FCurrentContext^.def.Rip;
{$endif}
end;

function TDbgWinX86Thread.GetStackBasePointerRegisterValue: TDbgPtr;
begin
  //{$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgWinThread.GetStackBasePointerRegisterValue');{$ENDIF}
  assert(MDebugEvent.dwProcessId <> 0, 'TDbgWinThread.GetStackBasePointerRegisterValue: MDebugEvent.dwProcessId <> 0');

  Result := 0;
  if FCurrentContext = nil then
    if not ReadThreadState then
      exit;
{$ifdef cpui386}
  Result := FCurrentContext^.def.Ebp;
{$else}
  if (TDbgWinProcess(Process).Bitness = b32) then
    Result := FCurrentContext^.WOW.Ebp
  else
    Result := FCurrentContext^.def.Rbp;
{$endif}
end;

procedure TDbgWinX86Thread.SetInstructionPointerRegisterValue(AValue: TDbgPtr);
begin
  if FCurrentContext = nil then
    exit;
{$ifdef cpui386}
  FCurrentContext^.def.Eip := AValue;
{$else}
  if (TDbgWinProcess(Process).Bitness = b32) then
    FCurrentContext^.WOW.Eip := AValue
  else
    FCurrentContext^.def.Rip := AValue;
{$endif}
  FThreadContextChanged:=True;
end;

procedure TDbgWinX86Thread.SetStackPointerRegisterValue(AValue: TDbgPtr);
begin
  if FCurrentContext = nil then
    exit;
{$ifdef cpui386}
  FCurrentContext^.def.Esp := AValue;
{$else}
  if (TDbgWinProcess(Process).Bitness = b32) then
    FCurrentContext^.WOW.Esp := AValue
  else
    FCurrentContext^.def.Rsp := AValue;
{$endif}
  FThreadContextChanged:=True;
end;

function TDbgWinX86Thread.GetStackPointerRegisterValue: TDbgPtr;
begin
  //{$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TDbgWinThread.GetStackPointerRegisterValue');{$ENDIF}
  assert(MDebugEvent.dwProcessId <> 0, 'TDbgWinThread.GetStackPointerRegisterValue: MDebugEvent.dwProcessId <> 0');

  Result := 0;
  if FCurrentContext = nil then
    if not ReadThreadState then
      exit;
{$ifdef cpui386}
  Result := FCurrentContext^.def.Esp;
{$else}
  if (TDbgWinProcess(Process).Bitness = b32) then
    Result := FCurrentContext^.WOW.Esp
  else
    Result := FCurrentContext^.def.Rsp;
{$endif}
end;

{ TDbgWinX86Process }

procedure TDbgWinX86Process.Interrupt;
var
  _UC: record
    C: TContext;
    D: array[1..16] of Byte;
  end;
  Context: PContext;
begin
  // Interrupting is implemented by suspending the thread and set DB0 to the
  // (to be) executed EIP. When the thread is resumed, it will generate a break
  // Single stepping doesn't work in all cases.

  // A context needs to be aligned to 16 bytes. Unfortunately, the compiler has
  // no directive for this, so align it somewhere in our "reserved" memory
  Context := AlignPtr(@_UC, $10);
  SuspendThread(Info.hThread);
  try
    Context^.ContextFlags := CONTEXT_CONTROL or CONTEXT_DEBUG_REGISTERS;
    if not GetThreadContext(Info.hThread, Context^)
    then begin
      DebugLn(DBG_WARNINGS, 'Proces %u interrupt: Unable to get context', [ProcessID]);
      Exit;
    end;

    Context^.ContextFlags := CONTEXT_DEBUG_REGISTERS;
    {$ifdef cpui386}
    Context^.Dr0 := Context^.Eip;
    {$else}
    Context^.Dr0 := Context^.Rip;
    {$endif}
    Context^.Dr7 := (Context^.Dr7 and $FFF0FFFF) or $1;

    if not SetThreadContext(Info.hThread, Context^)
    then begin
      DebugLn(DBG_WARNINGS, 'Proces %u interrupt: Unable to set context', [ProcessID]);
      Exit;
    end;
  finally
    ResumeTHread(Info.hThread);
  end;
end;

class function TDbgWinX86Process.isSupported(ATargetInfo: TTargetDescriptor): boolean;
begin
  result := (ATargetInfo.OS = osWindows) and
            (ATargetInfo.machineType in [mt386, mtX86_64]);
end;



initialization
  LoadKernelEntryPoints;

  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
  FPDBG_WINDOWS := DebugLogger.FindOrRegisterLogGroup('FPDBG_WINDOWS' {$IFDEF FPDBG_WINDOWS} , True {$ENDIF} );

  RegisterDbgOsClasses(TOSDbgClasses.Create(
    TDbgWinX86Process,
    TDbgWinX86Thread,
    TX86AsmDecoder
  ));

end.

