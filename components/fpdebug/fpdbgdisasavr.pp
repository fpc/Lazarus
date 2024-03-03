{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpdbgdisasavr.pp  -  Native Freepascal debugger - avr Disassembler
 ---------------------------------------------------------------------------

 This unit contains an avr disassembler for the Native Freepascal debugger

 ---------------------------------------------------------------------------

 @created(Mon Oct 18th 2019)
 @lastmod($Date$)
 @author()

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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit FpDbgDisasAvr;
{$mode objfpc}{$H+}
{$IFDEF INLINE_OFF}{$INLINE OFF}{$ENDIF}
interface

uses
  SysUtils,
  FpDbgUtil, FpDbgInfo, DbgIntfBaseTypes, FpdMemoryTools, {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif},
  FpDbgClasses;

type
  //The function Disassemble decodes the instruction at the given address.
  //Unrecognized instructions are assumed to be data statements [dw XXXX]

  TAvrAsmDecoder = class;

  TAVROpCode = (
    A_ADC, A_ADD, A_ADIW, A_AND, A_ANDI, A_ASR, A_BLD, A_BR, A_BREAK, A_BST,
    A_CALL, A_CBI, A_CL, A_COM, A_CP, A_CPC, A_CPI, A_CPSE, A_DEC, A_DES,
    A_EICALL, A_EIJMP, A_ELPM, A_EOR, A_FMUL, A_FMULS, A_FMULSU, A_ICALL, A_IJMP, A_IN,
    A_INC, A_JMP, A_LAC, A_LAS, A_LAT, A_LD, A_LDD, A_LDI, A_LDS, A_LPM,
    A_LSL, A_LSR, A_MOV, A_MOVW, A_MUL, A_MULS, A_MULSU, A_NEG, A_NOP, A_OR,
    A_ORI, A_OUT, A_POP, A_PUSH, A_RCALL, A_RET, A_RETI, A_RJMP, A_ROL, A_ROR,
    A_SBC, A_SBCI, A_SBI, A_SBIC, A_SBIS, A_SBIW, A_SBR, A_SBRC, A_SBRS, A_SE,
    A_SER, A_SLEEP, A_SPM, A_ST, A_STD, A_STS, A_SUB, A_SUBI, A_SWAP, A_WDR,
    A_XCH, A_INVALID);

  TAVRInstruction = record
    OpCode: TAVROpCode;
    OpCodeMod: integer;  // Use as index to general opcode modifiers, typically in range 0..7
    Size: integer;
    Oper: array[1..2] of integer;
  end;

  TOperandFormat = (ofNone, ofDecimal, ofHex8, ofHex16, ofHex32, ofReg, ofIndex, ofIndexOffset, ofRelAddr);
  // Information mostly for formatting
  TAvrInstructionInfo = record
    OpName: string;
    OperCount: integer;
    OperandFormats: array[1..2] of TOperandFormat;
  end;

  { TAvrDisassembler }

    TAvrDisassembler = object
    private
      function InvalidOpCode(instr: word): TAVRInstruction;
      function SetInstructionInfo(AOpCode: TAVROpCode; AOpcodeMod: integer;
        ASize: integer; AOper: array of integer): TAVRInstruction;
    public
      procedure Disassemble(var AAddress: Pointer; out AnInstruction: TAVRInstruction);
    end;

  { TAvrAsmInstruction }

  TAvrAsmInstruction = class(TDbgAsmInstruction)
  private const
    INSTR_CODEBIN_LEN = 4;
  private
    FProcess: TDbgProcess;
    FAddress: TDBGPtr;
    FCodeBin: array[0..INSTR_CODEBIN_LEN-1] of byte;
    FAvrInstruction: TAVRInstruction;
    FFlags: set of (diCodeRead, diCodeReadError, diDisAss);
  protected
    procedure ReadCode; inline;
    procedure Disassemble; inline;
  public
    constructor Create(AProcess: TDbgProcess);
    procedure SetAddress(AnAddress: TDBGPtr);
    function IsCallInstruction: boolean; override;
    function IsReturnInstruction: boolean; override;
    function IsLeaveStackFrame: boolean; override;
    function InstructionLength: Integer; override;
  end;

{ TAvrAsmDecoder }

  TAvrAsmDecoder = class(TDbgAsmDecoder)
  private const
    MaxPrologueSize = 64;  // Bytes, so ~32 instructions
    MaxEpilogueSize = MaxPrologueSize; // Perhaps a bit smaller, since the locals/parameters do not have to be initialized
    MAX_CODEBIN_LEN = MaxPrologueSize; // About 32 instructions

    // Opcodes for prologue / epilogue parsing
    OpCodeNOP = $0000;
    OpCodeLoadSPL = $B7CD;       // in r28, 0x3d
    OpCodeLoadSPH = $B7DE;       // in r29, 0x3e
    OpCodeLoadSRegR0 = $B60F;    // in r0, 0x3f
    OpCodeLoadSRegR16 = $B70F;   // in r16, 0x3f, avrtiny subarch
    OpCodeCli = $94F8;           // cli
    OpCodeSetSPH = $BFDE;        // out 0x3e, r29
    OpCodeSetSPL = $BFCD;        // out 0x3d, r28
    OpCodeSetSregR0 = $BE0F;     // out 0x3f, r0
    OpCodeSetSregR16 = $BF0F;    // out 0x3f, r16
    OpCodeRet = $9508;           // ret
    OpCodeReti = $9518;          // reti
    // Zero register gets zeroed at start of interrupt
    OpCodeZeroR1 = $2411;        // eor r1, r1
    OpCodeZeroR16 = $2700;       // eor r16, r16
    OpCodePushMask = $920F;      // PUSH 1001 001d dddd 1111
    OpCodePopMask = $900F;       // POP  1001 000d dddd 1111
    // Frame pointer is r28:r29, so fix register index in opcode
    OpCodeAdjustFrameLMask = $50C0;   // subi r28, k,  0101 kkkk dddd kkkk, rd = 16 + d
    OpCodeAdjustFrameHMask = $40D0;   // sbci r28, k,  0100 kkkk dddd kkkk, rd = 16 + d
    // Not yet implemented in FPC, but more compact option to set frame pointer
    OpCodeAdjustFrameMask  = $9720;   // sbiw r28, k,  1001 0111 kkdd kkkk, rd = 24 + d*2
    OpCodeStoreOnStackMask = $8208;   // std Y+2, r24  10q0 qq1r rrrr 1qqq
  private
    FProcess: TDbgProcess;
    FLastErrWasMem: Boolean;
    FCodeBin: array[0..MAX_CODEBIN_LEN-1] of byte;
    FLastInstr: TAvrAsmInstruction;
    function FParsePrologue(AnAddress, AStartPC, AEndPC: TDBGPtr; out
      returnAddressOffset: word; out AnIsOutsideFrame: Boolean): Boolean;
    function FParseEpilogue(AnAddress, AStartPC, AEndPC: TDBGPtr; out
      returnAddressOffset: word; out AnIsOutsideFrame: Boolean): Boolean;

    function FormatInstruction(instr: TAVRInstruction): string;
  protected
    function GetLastErrorWasMemReadErr: Boolean; override;
    function GetMaxInstrSize: integer; override;
    function GetMinInstrSize: integer; override;
    function GetCanReverseDisassemble: boolean; override;
    function ReadCodeAt(AnAddress: TDBGPtr; var ALen: Cardinal): Boolean; inline;

  public
    procedure Disassemble(var AAddress: Pointer; out ACodeBytes: String; out ACode: String; out AnInfo: TDbgInstInfo); override; overload;
    procedure Disassemble(var AAddress: Pointer; out ACodeBytes: String; out ACode: String); override; overload;
    function GetInstructionInfo(AnAddress: TDBGPtr): TDbgAsmInstruction; override;

    // Don't use, not really suited to AVR ABI
    function GetFunctionFrameInfo(AnAddress: TDBGPtr; out
      AnIsOutsideFrame: Boolean): Boolean; override;

    // Rather use the next function to locate the call return address.
    // AStartPC & AEndPC indicates proc limits to help with scanning for prologue/epilogue
    // returnAddressOffset gives the offset to return address relative to Y pointer (r28:r29) inside frame
    // else returnAddressOffset gives the offset to return address relative to SP
    function GetFunctionFrameReturnAddress(AnAddress, AStartPC, AEndPC: TDBGPtr; out
      returnAddressOffset: word; out AnIsOutsideFrame: Boolean): Boolean;

    constructor Create(AProcess: TDbgProcess); override;
    destructor Destroy;
  end;


  { TDbgStackUnwinderAVR }

  TDbgStackUnwinderAVR = class(TDbgStackUnwinder)
  private
    FThread: TDbgThread;
    FProcess: TDbgProcess;
    FAddressSize: Integer;
    FLastFrameBaseIncreased: Boolean;
    FCodeReadErrCnt: integer;
  protected
    property Process: TDbgProcess read FProcess;
    property Thread: TDbgThread read FThread;
    property AddressSize: Integer read FAddressSize;
  public
    constructor Create(AProcess: TDbgProcess);
    procedure InitForThread(AThread: TDbgThread); override;
    procedure InitForFrame(ACurrentFrame: TDbgCallstackEntry; out CodePointer,
      StackPointer, FrameBasePointer: TDBGPtr); override;
    procedure GetTopFrame(out CodePointer, StackPointer, FrameBasePointer: TDBGPtr;
      out ANewFrame: TDbgCallstackEntry); override;
    function Unwind(AFrameIndex: integer; var CodePointer, StackPointer,
      FrameBasePointer: TDBGPtr; ACurrentFrame: TDbgCallstackEntry; out
      ANewFrame: TDbgCallstackEntry): TTDbgStackUnwindResult; override;
  end;

implementation

uses
  StrUtils, LazClasses, Math,
  FpDbgAvrClasses;

var
  DBG_WARNINGS: PLazLoggerLogGroup;

const
  statusFlagNames: array[0..7] of char = ('c', 'z', 'n', 'v', 's', 'h', 't', 'i');

  branchConditionStr: array[0..15] of string = (
   'cs', 'eq', 'mi', 'vs', 'lt', 'hs', 'ts', 'ie',
   'cc', 'ne', 'pl', 'vc', 'ge', 'hc', 'tc', 'id');

  OpCodeInfo: array[A_ADC..A_INVALID] of TAvrInstructionInfo = (
    (OpName: 'adc'; OperCount: 2; OperandFormats: (ofReg, ofReg)),
    (OpName: 'add'; OperCount: 2; OperandFormats: (ofReg, ofReg)),
    (OpName: 'adiw'; OperCount: 2; OperandFormats: (ofReg, ofHex8)),
    (OpName: 'and'; OperCount: 2; OperandFormats: (ofReg, ofReg)),
    (OpName: 'andi'; OperCount: 2; OperandFormats: (ofReg, ofHex8)),
    (OpName: 'asr'; OperCount: 1; OperandFormats: (ofReg, ofNone)),
    (OpName: 'bld'; OperCount: 2; OperandFormats: (ofReg, ofDecimal)),
    (OpName: 'br'; OperCount: 1; OperandFormats: (ofRelAddr, ofNone)),
    (OpName: 'break'; OperCount: 0; OperandFormats: (ofNone, ofNone)),
    (OpName: 'bst'; OperCount: 2; OperandFormats: (ofReg, ofDecimal)),
    (OpName: 'call'; OperCount: 1; OperandFormats: (ofHex32, ofNone)),
    (OpName: 'cbi'; OperCount: 2; OperandFormats: (ofHex8, ofDecimal)),
    (OpName: 'cl'; OperCount: 0; OperandFormats: (ofNone, ofNone)),
    (OpName: 'com'; OperCount: 1; OperandFormats: (ofReg, ofNone)),
    (OpName: 'cp'; OperCount: 2; OperandFormats: (ofReg, ofReg)),
    (OpName: 'cpc'; OperCount: 2; OperandFormats: (ofReg, ofReg)),
    (OpName: 'cpi'; OperCount: 2; OperandFormats: (ofReg, ofHex8)),
    (OpName: 'cpse'; OperCount: 2; OperandFormats: (ofReg, ofReg)),
    (OpName: 'dec'; OperCount: 1; OperandFormats: (ofReg, ofNone)),
    (OpName: 'des'; OperCount: 1; OperandFormats: (ofHex8, ofNone)),
    (OpName: 'eicall'; OperCount: 0; OperandFormats: (ofNone, ofNone)),
    (OpName: 'eijmp'; OperCount: 0; OperandFormats: (ofNone, ofNone)),
    (OpName: 'elpm'; OperCount: 2; OperandFormats: (ofReg, ofIndex)),
    (OpName: 'eor'; OperCount: 2; OperandFormats: (ofReg, ofReg)),
    (OpName: 'fmul'; OperCount: 2; OperandFormats: (ofReg, ofReg)),
    (OpName: 'fmuls'; OperCount: 2; OperandFormats: (ofReg, ofReg)),
    (OpName: 'fmulsu'; OperCount: 2; OperandFormats: (ofReg, ofReg)),
    (OpName: 'icall'; OperCount: 0; OperandFormats: (ofNone, ofNone)),
    (OpName: 'ijmp'; OperCount: 0; OperandFormats: (ofNone, ofNone)),
    (OpName: 'in'; OperCount: 2; OperandFormats: (ofReg, ofHex8)),
    (OpName: 'inc'; OperCount: 1; OperandFormats: (ofReg, ofNone)),
    (OpName: 'jmp'; OperCount: 1; OperandFormats: (ofHex32, ofNone)),
    (OpName: 'lac'; OperCount: 2; OperandFormats: (ofIndex, ofReg)),
    (OpName: 'las'; OperCount: 2; OperandFormats: (ofIndex, ofReg)),
    (OpName: 'lat'; OperCount: 2; OperandFormats: (ofIndex, ofReg)),
    (OpName: 'ld'; OperCount: 2; OperandFormats: (ofReg, ofIndex)),
    (OpName: 'ldd'; OperCount: 2; OperandFormats: (ofReg, ofIndexOffset)),
    (OpName: 'ldi'; OperCount: 2; OperandFormats: (ofReg, ofHex8)),
    (OpName: 'lds'; OperCount: 2; OperandFormats: (ofReg, ofHex16)),
    (OpName: 'lpm'; OperCount: 2; OperandFormats: (ofReg, ofIndex)),
    (OpName: 'lsl'; OperCount: 1; OperandFormats: (ofReg, ofNone)),
    (OpName: 'lsr'; OperCount: 1; OperandFormats: (ofReg, ofNone)),
    (OpName: 'mov'; OperCount: 2; OperandFormats: (ofReg, ofReg)),
    (OpName: 'movw'; OperCount: 2; OperandFormats: (ofReg, ofReg)),
    (OpName: 'mul'; OperCount: 2; OperandFormats: (ofReg, ofReg)),
    (OpName: 'muls'; OperCount: 2; OperandFormats: (ofReg, ofReg)),
    (OpName: 'mulsu'; OperCount: 2; OperandFormats: (ofReg, ofReg)),
    (OpName: 'neg'; OperCount: 1; OperandFormats: (ofReg, ofNone)),
    (OpName: 'nop'; OperCount: 0; OperandFormats: (ofNone, ofNone)),
    (OpName: 'or'; OperCount: 2; OperandFormats: (ofReg, ofReg)),
    (OpName: 'ori'; OperCount: 2; OperandFormats: (ofReg, ofHex8)),
    (OpName: 'out'; OperCount: 2; OperandFormats: (ofHex8, ofReg)),
    (OpName: 'pop'; OperCount: 1; OperandFormats: (ofReg, ofNone)),
    (OpName: 'push'; OperCount: 1; OperandFormats: (ofReg, ofNone)),
    (OpName: 'rcall'; OperCount: 1; OperandFormats: (ofRelAddr, ofNone)),
    (OpName: 'ret'; OperCount: 0; OperandFormats: (ofNone, ofNone)),
    (OpName: 'reti'; OperCount: 0; OperandFormats: (ofNone, ofNone)),
    (OpName: 'rjmp'; OperCount: 1; OperandFormats: (ofRelAddr, ofNone)),
    (OpName: 'rol'; OperCount: 1; OperandFormats: (ofReg, ofNone)),
    (OpName: 'ror'; OperCount: 1; OperandFormats: (ofReg, ofNone)),
    (OpName: 'sbc'; OperCount: 2; OperandFormats: (ofReg, ofReg)),
    (OpName: 'sbci'; OperCount: 2; OperandFormats: (ofReg, ofDecimal)),
    (OpName: 'sbi'; OperCount: 2; OperandFormats: (ofHex8, ofDecimal)),
    (OpName: 'sbic'; OperCount: 2; OperandFormats: (ofHex8, ofDecimal)),
    (OpName: 'sbis'; OperCount: 2; OperandFormats: (ofHex8, ofDecimal)),
    (OpName: 'sbiw'; OperCount: 2; OperandFormats: (ofReg, ofDecimal)),
    (OpName: 'sbr'; OperCount: 2; OperandFormats: (ofReg, ofHex8)),
    (OpName: 'sbrc'; OperCount: 2; OperandFormats: (ofReg, ofDecimal)),
    (OpName: 'sbrs'; OperCount: 2; OperandFormats: (ofReg, ofDecimal)),
    (OpName: 'se'; OperCount: 0; OperandFormats: (ofNone, ofNone)),
    (OpName: 'ser'; OperCount: 0; OperandFormats: (ofNone, ofNone)),
    (OpName: 'sleep'; OperCount: 0; OperandFormats: (ofNone, ofNone)),
    (OpName: 'spm'; OperCount: 1; OperandFormats: (ofIndex, ofNone)),
    (OpName: 'st'; OperCount: 2; OperandFormats: (ofIndex, ofReg)),
    (OpName: 'std'; OperCount: 2; OperandFormats: (ofIndexOffset, ofReg)),
    (OpName: 'sts'; OperCount: 2; OperandFormats: (ofHex16, ofReg)),
    (OpName: 'sub'; OperCount: 2; OperandFormats: (ofReg, ofReg)),
    (OpName: 'subi'; OperCount: 2; OperandFormats: (ofReg, ofDecimal)),
    (OpName: 'swap'; OperCount: 1; OperandFormats: (ofReg, ofNone)),
    (OpName: 'wdr'; OperCount: 0; OperandFormats: (ofNone, ofNone)),
    (OpName: 'xch'; OperCount: 2; OperandFormats: (ofIndex, ofReg)),
    (OpName: '.dw'; OperCount: 1; OperandFormats: (ofHex16, ofNone))
  );

  // Constants for describing index registers of LD, LDD, ST, STS, LP, SPM
  operandNoIndex = 0;
  operandX = 1;
  operandXInc = 2;
  operandDecrX = 3;
  operandY = 4;
  operandYInc = 5;
  operandDecrY = 6;
  operandZ = 7;
  operandZInc = 8;
  operandDecrZ = 9;
  OperandIndexStr: array [operandX..operandDecrZ] of string = (
    'X', 'X+', '-X', 'Y', 'Y+', '-Y', 'Z', 'Z+', '-Z');

procedure get_r_d_10(o: word; out r, d: byte);
begin
  r := ((o shr 5) and $10) or (o and $f);
  d := (o shr 4) and $1f;
end;

procedure get_k_r16(o: word; out r, k: byte);
begin
  r := 16 + ((o shr 4) and $f);
  k := byte((o and $0f00) shr 4) or (o and $f);
end;

{ TAvrAsmInstruction }

procedure TAvrAsmInstruction.ReadCode;
begin
  if not (diCodeRead in FFlags) then begin
    if not FProcess.ReadData(FAddress, INSTR_CODEBIN_LEN, FCodeBin) then
      Include(FFlags, diCodeReadError);
    Include(FFlags, diCodeRead);
  end;
end;

procedure TAvrAsmInstruction.Disassemble;
var
  a: PByte;
  Disassembler: TAvrDisassembler;
begin
  if not (diDisAss in FFlags) then begin
    ReadCode;
    if diCodeReadError in FFlags then
      exit;
    a := @FCodeBin[0];
    Disassembler.Disassemble(a, FAvrInstruction);
    Include(FFlags, diDisAss);
  end;
end;

constructor TAvrAsmInstruction.Create(AProcess: TDbgProcess);
begin
  FProcess := AProcess;
  inherited Create;
  AddReference;
end;

procedure TAvrAsmInstruction.SetAddress(AnAddress: TDBGPtr);
begin
  FAddress := AnAddress;
  FFlags := [];
end;

function TAvrAsmInstruction.IsCallInstruction: boolean;
begin
  Result := False;
  ReadCode;
  if diCodeReadError in FFlags then
    exit;

  Disassemble;
  Result := FAvrInstruction.OpCode in [A_CALL, A_RCALL, A_ICALL, A_EICALL];
end;

function TAvrAsmInstruction.IsReturnInstruction: boolean;
begin
  Result := False;
  ReadCode;
  if diCodeReadError in FFlags then
    exit;

  Disassemble;
  Result := FAvrInstruction.OpCode in [A_RET, A_RETI];
end;

function TAvrAsmInstruction.IsLeaveStackFrame: boolean;
begin
  Result := false;
end;

function TAvrAsmInstruction.InstructionLength: Integer;
begin
  Result := 2;
  ReadCode;
  if diCodeReadError in FFlags then
    exit;

  Disassemble;
  Result := FAvrInstruction.Size;
end;

type
  TPrologueState = (psStart, psPush, psLoadSPL, psLoadSPH, psLoadSreg,
    psModifySPL, psModifySPH, psWriteSPH, psWriteSreg, psWriteSPL, psCopyParams);

function TAvrAsmDecoder.FParsePrologue(AnAddress, AStartPC, AEndPC: TDBGPtr;
  out returnAddressOffset: word; out AnIsOutsideFrame: Boolean): Boolean;
var
  ADataLen: Cardinal;
  AData: PByte;
  opcode, frameOffset: word;
  d, k: byte;
  stackState: TPrologueState;
begin
{ AVR function entry example
  3e:	df 93       	push	r29
  40:	cf 93       	push	r28
  42:	3f 92       	push	r3
  44:	2f 92       	push	r2

  // Load SP and calculate BP
  46:	cd b7       	in	r28, 0x3d	; 61    // SPL
  48:	de b7       	in	r29, 0x3e	; 62    // SPH
  4a:	c6 50       	subi	r28, 0x06	; 6
  4c:	d0 40       	sbci	r29, 0x00	; 0
  // Disable interrupts
  4e:	0f b6       	in	r0, 0x3f	; 63
  50:	f8 94       	cli
  // Then write out new SP
  52:	de bf       	out	0x3e, r29	; 62
  // Interleaved with restoring SREG
  54:	0f be       	out	0x3f, r0	; 63
  56:	cd bf       	out	0x3d, r28	; 61
  // Copy param to stack
  58:	8a 83       	std	Y+2, r24	; 0x02
  5a:	9b 83       	std	Y+3, r25	; 0x03
}
  // Also read next instruction, for simple procs/interrupts it may not be easy to spot the end of prologue
  // so the next instruction could tie break
  ADataLen := Min(MaxPrologueSize, AnAddress - AStartPC + 2);
  Result := ReadCodeAt(AStartPC, ADataLen);
  if not Result then
    exit;

  AData := @FCodeBin[0];
  // SP points to next empty slot, previous data is before current SP
  returnAddressOffset := 1;
  AnIsOutsideFrame := true;
  stackState := psStart;
  frameOffset := 0;

  // Loop until prologue buffer is empty, or stepped inside stack frame
  while (ADataLen > 1) and AnIsOutsideFrame do
  begin
    opcode := AData[0] or (AData[1] shl 8);
    inc(AData, 2);
    dec(ADataLen, 2);

    case opcode of
      OpCodeNOP: ;
      OpCodeLoadSPL: stackState := psLoadSPL;
      OpCodeLoadSPH: stackState := psLoadSPH;
      OpCodeLoadSRegR0, OpCodeLoadSRegR16: stackState := psLoadSreg;
      OpCodeCli: ;
      OpCodeSetSPH: stackState := psWriteSPH;
      OpCodeSetSregR0, OpCodeSetSregR16: stackState := psWriteSreg;
      OpCodeSetSPL: stackState := psWriteSPL;
      OpCodeZeroR1, OpCodeZeroR16: ;
    else
      // Push stack
      if (opcode and OpCodePushMask) = OpCodePushMask then
      begin
        if (stackState <= psPush) or (stackState = psLoadSreg) then
        begin
          inc(returnAddressOffset);
          if stackstate < psPush then
            stackState := psPush;
        end
        else
          DebugLn(DBG_WARNINGS, ['Invalid stack state for PUSH opcode: ', stackState]);
      end
      else if (opcode and OpCodeAdjustFrameLMask) = OpCodeAdjustFrameLMask then
      begin
        if stackState >= psLoadSPH then
        begin
          stackState := psModifySPL;
          // Decode register and constant
          get_k_r16(opcode, d, k);
          frameOffset := frameOffset + k;
        end
        else
          DebugLn(DBG_WARNINGS, ['Invalid stack state for OpCodeAdjustFrameLMask opcode: ', stackState]);
      end
      else if (opcode and OpCodeAdjustFrameHMask) = OpCodeAdjustFrameHMask then
      begin
        if stackState >= psLoadSPH then
        begin
          stackState := psModifySPH;
          // Decode register and constant
          get_k_r16(opcode, d, k);
          frameOffset := frameOffset + (k shl 8);
        end
        else
          DebugLn(DBG_WARNINGS, ['Invalid stack state for OpCodeAdjustFrameHMask opcode: ', stackState]);
      end
      else if (opcode and OpCodeAdjustFrameMask) = OpCodeAdjustFrameMask then
      begin
        if stackState >= psLoadSPH then
        begin
          stackState := psModifySPH;
          // Decode constant in SBIW opcode
          k := ((opcode and $00c0) shr 2) or (opcode and $f);
          frameOffset := frameOffset + k;
        end
        else
          DebugLn(DBG_WARNINGS, ['Invalid stack state for OpCodeAdjustFrameMask opcode: ', stackState]);
      end
      else if (opcode and OpCodeStoreOnStackMask) = OpCodeStoreOnStackMask then
      begin
        if stackState >= psWriteSPL then
        begin
          stackState := psCopyParams;
        end
        else
          DebugLn(DBG_WARNINGS, ['Invalid stack state for OpCodeStoreOnStackMask opcode: ', stackState]);
      end
      else  // Any other opcode isn't part of prologue
      begin
        AnIsOutsideFrame := false;
      end;
    end;
  end;

  // Check if frame pointer was updated
  if (stackState >= psWriteSPL) and (frameOffset > 0) then
  begin
    AnIsOutsideFrame := false;
    returnAddressOffset := returnAddressOffset + frameOffset;
  end
  else
    AnIsOutsideFrame := true;
end;

type
  // State sequence runs in reverse direction
  TEpilogueState = (esStart, esRet, esPop, esWriteSPH, esWriteSreg, esWriteSPL,
    esLoadSreg, esModifyFPH, esModifyFPL);

function TAvrAsmDecoder.FParseEpilogue(AnAddress, AStartPC, AEndPC: TDBGPtr;
  out returnAddressOffset: word; out AnIsOutsideFrame: Boolean): Boolean;
var
  ADataLen: Cardinal;
  AData: PByte;
  opcode, frameOffset: word;
  d, k: byte;
  stackState: TEpilogueState;
begin
{ AVR function epilogue example
  // Move FP to previous frame
  ba:	cd 5f       	subi	r28, 0xFD	; 253
  bc:	df 4f       	sbci	r29, 0xFF	; 255

  // Update SP
  be:	0f b6       	in	r0, 0x3f	; 63
  c0:	f8 94       	cli
  c2:	de bf       	out	0x3e, r29	; 62
  c4:	0f be       	out	0x3f, r0	; 63
  c6:	cd bf       	out	0x3d, r28	; 61

  // Restore saved regs
  c8:	cf 91       	pop	r28
  ca:	df 91       	pop	r29
  cc:	08 95       	ret
}

  ADataLen := Min(MaxEpilogueSize, AEndPC - AnAddress);
  Result := ReadCodeAt(AEndPC - ADataLen, ADataLen);
  if not Result then
    exit;

  AData := @FCodeBin[ADataLen - 2];
  // SP points to next empty slot, previous data is before current SP
  returnAddressOffset := 1;
  AnIsOutsideFrame := true;
  stackState := esStart;
  frameOffset := 0;

  // Loop until epilogue buffer is empty, or stepped inside stack frame
  while (ADataLen > 1) and AnIsOutsideFrame do
  begin
    opcode := AData[0] or (AData[1] shl 8);
    dec(AData, 2);
    dec(ADataLen, 2);

    case opcode of
      OpCodeNOP: ;
      OpCodeLoadSRegR0, OpCodeLoadSRegR16: stackState := esLoadSreg;
      OpCodeCli: ;
      OpCodeSetSPH: stackState := esWriteSPH;
      OpCodeSetSregR0, OpCodeSetSregR16: stackState := esWriteSreg;
      OpCodeSetSPL: stackState := esWriteSPL;
      OpCodeRet, OpCodeReti: stackState := esRet;
    else
      // Pop stack
      if (opcode and OpCodePopMask) = OpCodePopMask then
      begin
        if (stackState <= esPop) or (stackState = esWriteSreg) then
        begin
          inc(returnAddressOffset);
          if stackState < esPop then
            stackState := esPop;
        end
        else
          DebugLn(DBG_WARNINGS, ['Invalid stack state for POP opcode: ', stackState]);
      end
      else if (opcode and OpCodeAdjustFrameLMask) = OpCodeAdjustFrameLMask then
      begin
        if stackState < esModifyFPL then
        begin
          stackState := esModifyFPL;
          // Decode register and constant
          get_k_r16(opcode, d, k);
          // Normally subtract negative values to increase FP
          k := (256 - word(k));
          frameOffset := frameOffset + k;
        end
        else
          DebugLn(DBG_WARNINGS, ['Invalid stack state for OpCodeAdjustFrameLMask opcode: ', stackState]);
      end
      else if (opcode and OpCodeAdjustFrameHMask) = OpCodeAdjustFrameHMask then
      begin
        if stackState < esModifyFPH then
        begin
          stackState := esModifyFPH;
          // Decode register and constant
          get_k_r16(opcode, d, k);
          // Normally subtract negative values to increase FP, + carry bit
          k := (256 - word(k) - 1);
          frameOffset := frameOffset + (k shl 8);
        end
        else
          DebugLn(DBG_WARNINGS, ['Invalid stack state for OpCodeAdjustFrameHMask opcode: ', stackState]);
      end
      else  // Any other opcode isn't part of prologue
      begin
        AnIsOutsideFrame := false;
      end;
    end;
  end;

  // Check before frame pointer gets adjusted
  if (stackState >= esModifyFPL) and (frameOffset > 0) then
  begin
    AnIsOutsideFrame := false;
    returnAddressOffset := returnAddressOffset + frameOffset;
  end
  // Frame pointer inconsistent, so work relative to SP
  // Unreliable, SP can be adjusted inside frame
  //else if (stackState = esModifyFPH) then
  //begin
  //  AnIsOutsideFrame := true;
  //  returnAddressOffset := returnAddressOffset + frameOffset;
  //end
  //else if (stackState > esPop) then
  //begin
  //
  //end
  else
    AnIsOutsideFrame := true;
end;

function TAvrAsmDecoder.FormatInstruction(instr: TAVRInstruction): string;
const
  OutputFormats: array[0..2] of string = (
    '%s',
    '%:-7s %s',
    '%:-7s %s, %s');
var
  OpName: string;
  ops: array[1..2] of string;
  info: TAvrInstructionInfo;
  i: integer;
begin
  info := OpCodeInfo[instr.OpCode];
  OpName := info.OpName;

  // Special cases first
  case instr.OpCode of
    A_BR: OpName := OpName + branchConditionStr[instr.OpCodeMod];
    A_CL, A_SE: OpName := OpName + statusFlagNames[instr.OpCodeMod];
  end;

  // Instructions with variable number of operands
  if ((instr.OpCode in [A_ELPM, A_LPM]) and (instr.Oper[2] = operandNoIndex)) or
     ((instr.OpCode = A_SPM) and (instr.Oper[1] = operandNoIndex)) then
    info.OperCount := 0;

  if info.OperCount > 0 then
  begin
    for i := 1 to info.OperCount do
    begin
      case info.OperandFormats[i] of
        ofDecimal:     ops[i] := IntToStr(instr.Oper[i]);
        ofHex8:        ops[i] := '$'+IntToHex(instr.Oper[i], 2);
        ofHex16:       ops[i] := '$'+IntToHex(instr.Oper[i], 4);
        ofHex32:       ops[i] := '$'+IntToHex(instr.Oper[i], 8);
        ofReg:         ops[i] := 'r' + IntToStr(instr.Oper[i]);
        ofIndex:       ops[i] := OperandIndexStr[instr.Oper[i]];
        ofIndexOffset:
        begin
          // IndexOffset should be either Y+ or Z+
          if not(instr.OpCodeMod in [operandYInc, operandZInc]) then
            writeln('ERROR');
          ops[i] := OperandIndexStr[instr.OpCodeMod] + IntToStr(instr.Oper[i]);
        end;
        ofRelAddr:
        begin
          if instr.Oper[i] >= 0 then
            ops[i] := '.+' + IntToStr(instr.Oper[i])
          else
            ops[i] := '.' + IntToStr(instr.Oper[i]);
        end;
      else
        writeln('ERROR');
      end;
    end;

    if info.OperCount = 1 then
      Result := Format(OutputFormats[1], [OpName, ops[1]])
    else
      Result := Format(OutputFormats[2], [OpName, ops[1], ops[2]]);
  end
  else
    Result := Format(OutputFormats[0], [OpName]);
end;

function TAvrAsmDecoder.GetLastErrorWasMemReadErr: Boolean;
begin
  Result := FLastErrWasMem;
end;

function TAvrAsmDecoder.GetMaxInstrSize: integer;
begin
  Result := 4;
end;

function TAvrAsmDecoder.GetMinInstrSize: integer;
begin
  Result := 2;
end;

function TAvrAsmDecoder.GetCanReverseDisassemble: boolean;
begin
  Result := true;
end;

function TAvrAsmDecoder.ReadCodeAt(AnAddress: TDBGPtr; var ALen: Cardinal
  ): Boolean;
begin
  Result := FProcess.ReadData(AnAddress, ALen, FCodeBin[0], ALen);
  FLastErrWasMem := not Result;
end;

procedure TAvrAsmDecoder.Disassemble(var AAddress: Pointer; out
  ACodeBytes: String; out ACode: String; out AnInfo: TDbgInstInfo);
var
  decoder: TAvrDisassembler;
  k: byte;
  pcode: PByte;
  instr: TAVRInstruction;
begin
  AnInfo := default(TDbgInstInfo);
  pcode := AAddress;
  decoder.Disassemble(AAddress, instr);
  Inc(AAddress, instr.Size);

  ACodeBytes := '';
  for k := 0 to instr.Size-1 do
    ACodeBytes := ACodeBytes + HexStr(pcode[k], 2) + ' ';
  Delete(ACodeBytes, length(ACodeBytes), 1);
  ACode := FormatInstruction(instr);

  // Todo: Indicate whether currrent instruction has a destination code address
  //       call jump branch(?)
  //       Return information in AnInfo
end;

procedure TAvrAsmDecoder.Disassemble(var AAddress: Pointer; out
  ACodeBytes: String; out ACode: String);
var
  AnInfo: TDbgInstInfo;
begin
  Disassemble(AAddress, ACodeBytes, ACode, AnInfo);
end;

function TAvrAsmDecoder.GetInstructionInfo(AnAddress: TDBGPtr
  ): TDbgAsmInstruction;
begin
  if (FLastInstr = nil) or (FLastInstr.RefCount > 1) then begin
    ReleaseRefAndNil(FLastInstr);
    FLastInstr := TAvrAsmInstruction.Create(FProcess);
  end;

  FLastInstr.SetAddress(AnAddress);
  Result := FLastInstr;
end;

function TAvrAsmDecoder.GetFunctionFrameInfo(AnAddress: TDBGPtr; out
  AnIsOutsideFrame: Boolean): Boolean;
begin
  Result := False;
end;

function TAvrAsmDecoder.GetFunctionFrameReturnAddress(AnAddress, AStartPC,
  AEndPC: TDBGPtr; out returnAddressOffset: word; out AnIsOutsideFrame: Boolean
  ): Boolean;
begin
  { Cases to consider:
    A - if (AStartPC + MaxPrologueSize < AnAddress) and (AnAddress + MaxEpilogueSize < AEndPC)
        then currently inside stack frame. Parse prologue to figure out
        offset from frame pointer to return address.

    B - if (AStartPC + MaxPrologueSize < AnAddress)
        then possibly before final stack frame.  Need to parse prologue up to AnAddress
        to figure out how far the stack has moved since entry to calculate offset
        from SP to return address. If frame pointer has been configured before
        AnAddress, assume inside frame and return offset relative to frame pointer.

    C - if (AnAddress + MaxEpilogueSize < AEndPC)
        then possibly inside frame teardown.  Need to reverse parse epilogue up to AnAddress
        to figure out how much frame will unwind to calculate offset to return address.
        If frame pointer has been restored before AnAddress then ouside frame.
  }

  if (AnAddress = AStartPC) or (AnAddress = AEndPC) then
  begin
    // Frame not yet constructed, so return address is located via SP + offset
    returnAddressOffset := 1;
    AnIsOutsideFrame := true;
    exit;
  end
  //else if (AStartPC + MaxPrologueSize > AnAddress) then
  else if (AnAddress - AStartPC) < (AEndPC - AnAddress) then
    result := FParsePrologue(AnAddress, AStartPC, AEndPC, returnAddressOffset, AnIsOutsideFrame)
  else
    result := FParseEpilogue(AnAddress, AStartPC, AEndPC, returnAddressOffset, AnIsOutsideFrame);
end;

constructor TAvrAsmDecoder.Create(AProcess: TDbgProcess);
begin
  FProcess := AProcess;
end;

destructor TAvrAsmDecoder.Destroy;
begin
  ReleaseRefAndNil(FLastInstr);
  inherited Destroy;
end;

{ TAvrDisassembler }

function TAvrDisassembler.InvalidOpCode(instr: word): TAVRInstruction;
begin
  result.OpCode := A_INVALID;
  result.Size := 2;
  result.Oper[1] := instr;
end;

function TAvrDisassembler.SetInstructionInfo(AOpCode: TAVROpCode;
  AOpcodeMod: integer; ASize: integer; AOper: array of
  integer): TAVRInstruction;
var
  i: integer;
begin
  Result.OpCode := AOpCode;
  Result.OpCodeMod := AOpcodeMod;
  Result.Size := ASize;

  for i := 1 to 2 do
  begin
    if i <= OpCodeInfo[AOpCode].OperCount then
      Result.Oper[i] := AOper[i-1]
    else
      Result.Oper[i] := 0;
  end;
end;

procedure TAvrDisassembler.Disassemble(var AAddress: Pointer; out
  AnInstruction: TAVRInstruction);
var
  CodeIdx, r, d, k, q: byte;
  a: SmallInt;
  pcode: PByte;
  code, addr16: word;
  _set: boolean;
  instr: TAVRInstruction absolute AnInstruction;
begin
  pcode := AAddress;
  CodeIdx := 0;
  code := pcode[CodeIdx];
  inc(CodeIdx);
  code := code or (pcode[CodeIdx] shl 8);
  inc(CodeIdx);

  case (code and $f000) of
    $0000:
    begin
      case (code) of
        $0000: instr := SetInstructionInfo(A_NOP, 0, 2, []);
      else
        case (code and $fc00) of
          $0400:
          begin // CPC compare with carry 0000 01rd dddd rrrr
            get_r_d_10(code, r, d);
            instr := SetInstructionInfo(A_CPC, 0, 2, [d, r]);
          end;
          $0c00:
          begin // ADD without carry 0000 11 rd dddd rrrr
            get_r_d_10(code, r, d);
            instr := SetInstructionInfo(A_ADD, 0, 2, [d, r]);
          end;
          $0800:
          begin // SBC subtract with carry 0000 10rd dddd rrrr
            get_r_d_10(code, r, d);
            instr := SetInstructionInfo(A_SBC, 0, 2, [d, r]);
          end;
          else
            case (code and $ff00) of
              $0100:
              begin // MOVW – Copy Register Word 0000 0001 dddd rrrr
                d := ((code shr 4) and $f) shl 1;
                r := ((code) and $f) shl 1;
                instr := SetInstructionInfo(A_MOVW, 0, 2, [d, r]);
              end;
              $0200:
              begin // MULS – Multiply Signed 0000 0010 dddd rrrr
                r := 16 + (code and $f);
                d := 16 + ((code shr 4) and $f);
                instr := SetInstructionInfo(A_MULS, 0, 2, [d, r]);
              end;
              $0300:
              begin // MUL Multiply 0000 0011 fddd frrr
                r := 16 + (code and $7);
                d := 16 + ((code shr 4) and $7);
                case (code and $88) of
                  $00: instr := SetInstructionInfo(A_MULSU, 0, 2, [d, r]);
                  $08: instr := SetInstructionInfo(A_FMUL, 0, 2, [d, r]);
                  $80: instr := SetInstructionInfo(A_FMULS, 0, 2, [d, r]);
                  $88: instr := SetInstructionInfo(A_FMULSU, 0, 2, [d, r]);
                end;
              end;
              else
                instr := InvalidOpCode(code);
            end; // case (code and $ff00)
        end;  // case (code and $fc00)
      end; // case code of
    end;
    $1000:
    begin
      case (code and $fc00) of
        $1800:
        begin // SUB without carry 0000 10 rd dddd rrrr
          get_r_d_10(code, r, d);
          instr := SetInstructionInfo(A_SUB, 0, 2, [d, r]);
        end;
        $1000:
        begin // CPSE Compare, skip if equal 0000 00 rd dddd rrrr
          get_r_d_10(code, r, d);
          instr := SetInstructionInfo(A_CPSE, 0, 2, [d, r]);
        end;
        $1400:
        begin // CP Compare 0000 01 rd dddd rrrr
          get_r_d_10(code, r, d);
          instr := SetInstructionInfo(A_CP, 0, 2, [d, r]);
        end;
        $1c00:
        begin // ADD with carry 0001 11 rd dddd rrrr
          get_r_d_10(code, r, d);
          instr := SetInstructionInfo(A_ADC, 0, 2, [d, r]);
        end;
        else
          instr := InvalidOpCode(code);
      end;
    end;
    $2000:
    begin
      case (code and $fc00) of
        $2000:
        begin // AND 0010 00rd dddd rrrr
          get_r_d_10(code, r, d);
          instr := SetInstructionInfo(A_AND, 0, 2, [d, r]);
        end;
        $2400:
        begin // EOR 0010 01rd dddd rrrr
          get_r_d_10(code, r, d);
          instr := SetInstructionInfo(A_EOR, 0, 2, [d, r]);
        end;
        $2800:
        begin // OR Logical OR 0010 10rd dddd rrrr
          get_r_d_10(code, r, d);
          instr := SetInstructionInfo(A_OR, 0, 2, [d, r]);
        end;
        $2c00:
        begin // MOV 0010 11rd dddd rrrr
          get_r_d_10(code, r, d);
          instr := SetInstructionInfo(A_MOV, 0, 2, [d, r]);
        end;
        else
          instr := InvalidOpCode(code);
      end;
    end;
    $3000:
    begin // CPI 0011 KKKK rrrr KKKK
      get_k_r16(code, d, k);
      instr := SetInstructionInfo(A_CPI, 0, 2, [d, k]);
    end;
    $4000:
    begin // SBCI Subtract Immediate With Carry 0100 kkkk dddd kkkk
      get_k_r16(code, d, k);
      instr := SetInstructionInfo(A_SBCI, 0, 2, [d, k]);
    end;
    $5000:
    begin // SUB Subtract Immediate 0101 kkkk dddd kkkk
      get_k_r16(code, d, k);
      instr := SetInstructionInfo(A_SUBI, 0, 2, [d, k]);
    end;
    $6000:
    begin // ORI aka SBR Logical AND with Immediate 0110 kkkk dddd kkkk
      get_k_r16(code, d, k);
      instr := SetInstructionInfo(A_ORI, 0, 2, [d, k]);
    end;
    $7000:
    begin // ANDI Logical AND with Immediate 0111 kkkk dddd kkkk
      get_k_r16(code, d, k);
      instr := SetInstructionInfo(A_ANDI, 0, 2, [d, k]);
    end;
    $a000,
    $8000:
    begin
      case (code and $d008) of
        $a000,
        $8000:
        begin // LD (LDD) – Load Indirect using Z 10q0 qq0r rrrr 0qqq
              // ST (STD) - Store Indirect with Z 10q0 qq1r rrrr 0qqq
          // This check overlaps with the 16 bit LDS/STS instructions of reduced core tiny
          // Todo: only activate this check if debugging a tiny
          if false and ((code and $A000) = $A000) then
          begin
            r := 16 + ((code shr 4) and $0F);
            k := ((code shr 5) and $30) or (code and $0F);
            if (code and $0100) = 0 then
              k := k or $80
            else
              k := k or $40;

            if (code and $800) = 0 then
              instr := SetInstructionInfo(A_LDS, 0, 2, [r, k])
            else
              instr := SetInstructionInfo(A_STS, 0, 2, [k, r]);
          end
          else
          begin
            d := (code shr 4) and $1f;
            q := ((code and $2000) shr 8) or ((code and $0c00) shr 7) or (code and $7);
            if (code and $0200) <> 0 then // store
            begin
              if q > 0 then
                instr := SetInstructionInfo(A_STD, operandZInc, 2, [q, d])
              else
              begin
                q := OperandZ + (code and 3);
                instr := SetInstructionInfo(A_ST, 0, 2, [q, d]);
              end;
            end
            else  // load
            begin
              if q > 0 then
                instr := SetInstructionInfo(A_LDD, operandZInc, 2, [d, q])
              else
                instr := SetInstructionInfo(A_LD, 0, 2, [d, operandZ + (code and 3)]);
            end;
          end;
        end;
        $a008,
        $8008:
        begin // LD (LDD) – Load Indirect using Y 10q0 qq0r rrrr 1qqq
          d := (code shr 4) and $1f;
          q := ((code and $2000) shr 8) or ((code and $0c00) shr 7) or (code and $7);
          if (code and $0200) <> 0 then // store
          begin
            if q > 0 then
              instr := SetInstructionInfo(A_STD, operandYInc, 2, [q, d])
            else
              instr := SetInstructionInfo(A_ST, 0, 2, [operandY + (code and 3), d]);
          end
          else  // load
          begin
            if q > 0 then
              instr := SetInstructionInfo(A_LDD, operandYInc, 2, [d, q])
            else
              instr := SetInstructionInfo(A_LD, 0, 2, [d, operandY + (code and 3)]);
          end;
        end;
        else
          instr := InvalidOpCode(code);
      end;
    end;
    $9000:
    begin
      if ((code and $ff0f) = $9408) then  // clear/set SREG flags
      begin
        k := (code shr 4) and 7;
        if ((code and $0080) = 0) then
          instr := SetInstructionInfo(A_SE, k, 2, [])
        else
          instr := SetInstructionInfo(A_CL, k, 2, []);
      end
      else
        case (code) of
          $9409: instr := SetInstructionInfo(A_IJMP, 0, 2, []);
          $9419: instr := SetInstructionInfo(A_EIJMP, 0, 2, []);
          $9508: instr := SetInstructionInfo(A_RET, 0, 2, []);
          $9509: instr := SetInstructionInfo(A_ICALL, 0, 2, []);
          $9519: instr := SetInstructionInfo(A_EICALL, 0, 2, []);
          $9518: instr := SetInstructionInfo(A_RETI, 0, 2, []);
          $9588: instr := SetInstructionInfo(A_SLEEP, 0, 2, []);
          $9598: instr := SetInstructionInfo(A_BREAK, 0, 2, []);
          $95a8: instr := SetInstructionInfo(A_WDR, 0, 2, []);
          $95c8: instr := SetInstructionInfo(A_LPM, 0, 2, [0, operandNoIndex]);
          $95d8: instr := SetInstructionInfo(A_ELPM, 0, 2, [0, operandNoIndex]);
          $95e8: instr := SetInstructionInfo(A_SPM, 0, 2, [operandNoIndex]);
          $95f8: instr := SetInstructionInfo(A_SPM, 0, 2, [operandZInc]);
          else
          begin
            case (code and $fe0f) of
              $9000:
              begin // LDS Load Direct from fData Space, 32 bits
                r := (code shr 4) and $1f;
                addr16 := pcode[CodeIdx];
                inc(CodeIdx);
                addr16 := addr16 or (pcode[CodeIdx] shl 8);
                inc(CodeIdx);
                instr := SetInstructionInfo(A_LDS, 0, 4, [r, addr16]);
              end;
              $9005,
              $9004:
              begin // LPM Load Program Memory 1001 000d dddd 01oo
                r := (code shr 4) and $1f;
                if (code and 1 = 1) then
                  instr := SetInstructionInfo(A_LPM, 0, 2, [r, operandZInc])
                else
                  instr := SetInstructionInfo(A_LPM, 0, 2, [r, operandZ]);
              end;
              $9006,
              $9007:
              begin // ELPM Extended Load Program Memory 1001 000d dddd 01oo
                r := (code shr 4) and $1f;
                if (code and 1 = 1) then
                  instr := SetInstructionInfo(A_ELPM, 0, 2, [r, operandZInc])
                else
                  instr := SetInstructionInfo(A_ELPM, 0, 2, [r, operandZ]);
              end;
              $900c,
              $900d,
              $900e:
              begin // LD Load Indirect from fData using X 1001 000r rrrr 11oo
                r := (code shr 4) and $1f;
                instr := SetInstructionInfo(A_LD, 0, 2, [r, operandX + (code and 3)]);
              end;
              $920c,
              $920d,
              $920e:
              begin // ST Store Indirect fData Space X 1001 001r rrrr 11oo
                r := (code shr 4) and $1f;
                instr := SetInstructionInfo(A_ST, 0, 2, [operandX + (code and 3), r])
              end;
              $9009,
              $900a:
              begin // LD Load Indirect from fData using Y 1001 000r rrrr 10oo
                r := (code shr 4) and $1f;
                instr := SetInstructionInfo(A_LD, 0, 2, [r, operandY + (code and 3)])
              end;
              $9209,
              $920a:
              begin // ST Store Indirect fData Space Y 1001 001r rrrr 10oo
                r := (code shr 4) and $1f;
                instr := SetInstructionInfo(A_ST, 0, 2, [operandY + (code and 3), r])
              end;
              $9200:
              begin // STS Store Direct to Data Space, 32 bits
                r := (code shr 4) and $1f;
                addr16 := pcode[CodeIdx];
                inc(CodeIdx);
                addr16 := addr16 or (pcode[CodeIdx] shl 8);
                inc(CodeIdx);
                instr := SetInstructionInfo(A_STS, 0, 4, [addr16, r])
              end;
              $9001,
              $9002:
              begin // LD Load Indirect from Data using Z 1001 001r rrrr 00oo
                r := (code shr 4) and $1f;
                instr := SetInstructionInfo(A_LD, 0, 2, [r, operandZ + (code and 3)])
              end;
              $9201,
              $9202:
              begin // ST Store Indirect Data Space Z 1001 001r rrrr 0Xoo X=0 or XMega instructions X=1
                r := (code shr 4) and $1f;
                instr := SetInstructionInfo(A_ST, 0, 2, [operandZ + (code and 3), r])
              end;

              $9204..$9207:
              begin  // AVR8X instructions
                r := (code shr 4) and $1f;
                case (code and 3) of
                  0: instr := SetInstructionInfo(A_XCH, 0, 2, [operandZ, r]);
                  1: instr := SetInstructionInfo(A_LAS, 0, 2, [operandZ, r]);
                  2: instr := SetInstructionInfo(A_LAC, 0, 2, [operandZ, r]);
                  3: instr := SetInstructionInfo(A_LAT, 0, 2, [operandZ, r]);
                end;
              end;

              $900f:
              begin // POP 1001 000d dddd 1111
                r := (code shr 4) and $1f;
                instr := SetInstructionInfo(A_POP, 0, 2, [r])
              end;
              $920f:
              begin // PUSH 1001 001d dddd 1111
                r := (code shr 4) and $1f;
                instr := SetInstructionInfo(A_PUSH, 0, 2, [r])
              end;
              $9400:
              begin // COM – One’s Complement
                r := (code shr 4) and $1f;
                instr := SetInstructionInfo(A_COM, 0, 2, [r])
              end;
              $9401:
              begin // NEG – Two’s Complement
                r := (code shr 4) and $1f;
                instr := SetInstructionInfo(A_NEG, 0, 2, [r])
              end;
              $9402:
              begin // SWAP – Swap Nibbles
                r := (code shr 4) and $1f;
                instr := SetInstructionInfo(A_SWAP, 0, 2, [r])
              end;
              $9403:
              begin // INC – Increment
                r := (code shr 4) and $1f;
                instr := SetInstructionInfo(A_INC, 0, 2, [r])
              end;
              $9405:
              begin // ASR – Arithmetic Shift Right 1001 010d dddd 0101
                r := (code shr 4) and $1f;
                instr := SetInstructionInfo(A_ASR, 0, 2, [r])
              end;
              $9406:
              begin // LSR 1001 010d dddd 0110
                r := (code shr 4) and $1f;
                instr := SetInstructionInfo(A_LSR, 0, 2, [r])
              end;
              $9407:
              begin // ROR 1001 010d dddd 0111
                r := (code shr 4) and $1f;
                instr := SetInstructionInfo(A_ROR, 0, 2, [r])
              end;
              $940a:
              begin // DEC – Decrement
                r := (code shr 4) and $1f;
                instr := SetInstructionInfo(A_DEC, 0, 2, [r])
              end;
              $940c,
              $940d:
              begin // JMP Long Call to sub, 32 bits
                k := ((code and $01f0) shr 3) or (code and 1);
                addr16 := pcode[CodeIdx];
                inc(CodeIdx);
                addr16 := addr16 or (pcode[CodeIdx] shl 8);
                inc(CodeIdx);
                instr := SetInstructionInfo(A_JMP, 0, 4, [dword(k shl 16) or (dword(addr16) shl 1)]);
              end;
              $940e,
              $940f:
              begin // CALL Long Call to sub, 32 bits
                k := ((code and $01f0) shr 3) or (code and 1);
                addr16 := pcode[CodeIdx];
                inc(CodeIdx);
                addr16 := addr16 or (pcode[CodeIdx] shl 8);
                inc(CodeIdx);
                instr := SetInstructionInfo(A_CALL, 0, 4, [dword(k shl 16) or (dword(addr16) shl 1)]);
              end;
              else
              begin
                case (code and $ff00) of
                  $9600:
                  begin // ADIW - Add Immediate to Word 1001 0110 KKdd KKKK
                    r := 24 + ((code shr 3) and $6);
                    k := ((code and $00c0) shr 2) or (code and $f);
                    instr := SetInstructionInfo(A_ADIW, 0, 2, [r, k]);
                  end;
                  $9700:
                  begin // SBIW - Subtract Immediate from Word 1001 0110 KKdd KKKK
                    r := 24 + ((code shr 3) and $6);
                    k := ((code and $00c0) shr 2) or (code and $f);
                    instr := SetInstructionInfo(A_SBIW, 0, 2, [r, k]);
                  end;
                  $9800:
                  begin // CBI - Clear Bit in I/O Register 1001 1000 AAAA Abbb
                    d := (code shr 3) and $1f;
                    k := code and $7;
                    instr := SetInstructionInfo(A_CBI, 0, 2, [d, k]);
                  end;
                  $9900:
                  begin // SBIC - Skip if Bit in I/O Register is Cleared 1001 0111 AAAA Abbb
                    d := (code shr 3) and $1f;
                    k := code and $7;
                    instr := SetInstructionInfo(A_SBIC, 0, 2, [d, k]);
                  end;
                  $9a00:
                  begin // SBI - Set Bit in I/O Register 1001 1000 AAAA Abbb
                    d := (code shr 3) and $1f;
                    k := code and $7;
                    instr := SetInstructionInfo(A_SBI, 0, 2, [d, k]);
                  end;
                  $9b00:
                  begin // SBIS - Skip if Bit in I/O Register is Set 1001 1011 AAAA Abbb
                    d := (code shr 3) and $1f;
                    k := code and $7;
                    instr := SetInstructionInfo(A_SBIS, 0, 2, [d, k]);
                  end;
                  else
                    case (code and $fc00) of
                      $9c00:
                      begin // MUL - Multiply Unsigned 1001 11rd dddd rrrr
                        get_r_d_10(code, r, d);
                        instr := SetInstructionInfo(A_MUL, 0, 2, [d, k]);
                      end;
                      else
                        instr := InvalidOpCode(code);
                    end;
                end;
              end;
            end;
          end;
        end;
    end;
    $b000:
    begin
      case (code and $f800) of
        $b800:
        begin // OUT A,Rr 1011 1AAr rrrr AAAA
          r := (code shr 4) and $1f;
          d := ((((code shr 9) and 3) shl 4) or ((code) and $f));
          instr := SetInstructionInfo(A_OUT, 0, 2, [d, r]);
        end;
        $b000:
        begin // IN Rd,A 1011 0AAr rrrr AAAA
          r := (code shr 4) and $1f;
          d := ((((code shr 9) and 3) shl 4) or ((code) and $f));
          instr := SetInstructionInfo(A_IN, 0, 2, [r, d]);
        end;
        else
          instr := InvalidOpCode(code);
      end;
    end;
    $c000:
    begin // RJMP 1100 kkkk kkkk kkkk
      a := smallint((word(code) shl 4) and $ffff) div 16;
      instr := SetInstructionInfo(A_RJMP, 0, 2, [a shl 1]);
    end;
    $d000:
    begin // RCALL 1100 kkkk kkkk kkkk
      a := smallint((word(code) shl 4) and $ffff) div 16;
      instr := SetInstructionInfo(A_RCALL, 0, 2, [a shl 1]);
    end;
    $e000:
    begin // LDI Rd, K 1110 KKKK RRRR KKKK -- aka SER (LDI r, $ff)
      d := 16 + ((code shr 4) and $f);
      k := ((code and $0f00) shr 4) or (code and $f);
      instr := SetInstructionInfo(A_LDI, 0, 2, [d, k]);
    end;
    $f000:
    begin
      case (code and $fe00) of
        $f000,
        $f200,
        $f400,
        $f600:
        begin // All the fSREG branches
          a := smallint(smallint(code shl 6) shr 9) * 2; // offset
          k := code and 7;
          _set := (code and $0400) = 0; // this bit means BRXC otherwise BRXS
          if not _set then
            k := k + 8;
          instr := SetInstructionInfo(A_BR, k, 2, [a]);
        end;
        $f800,
        $f900:
        begin // BLD – Bit Load from T into a Bit in Register 1111 100r rrrr 0bbb
          d := (code shr 4) and $1f; // register index
          k := code and 7;
          instr := SetInstructionInfo(A_BLD, 0, 2, [d, k]);
        end;
        $fa00,
        $fb00:
        begin // BST – Bit Store into T from bit in Register 1111 100r rrrr 0bbb
          r := (code shr 4) and $1f; // register index
          k := code and 7;
          instr := SetInstructionInfo(A_BST, 0, 2, [r, k]);
        end;
        $fc00,
        $fe00:
        begin // SBRS/SBRC – Skip if Bit in Register is Set/Clear 1111 11sr rrrr 0bbb
          r := (code shr 4) and $1f; // register index
          k := code and 7;
          _set := (code and $0200) <> 0;
          if _set then
            instr := SetInstructionInfo(A_SBRS, 0, 2, [r, k])
          else
            instr := SetInstructionInfo(A_SBRC, 0, 2, [r, k]);
        end;
        else
          instr := InvalidOpCode(code);
      end;
    end;
    else
      instr := InvalidOpCode(code);
  end;
end;

{ TDbgStackUnwinderAVR }

constructor TDbgStackUnwinderAVR.Create(AProcess: TDbgProcess);
begin
  FProcess := AProcess;
  FAddressSize := 2;
  FCodeReadErrCnt := 0;
end;

procedure TDbgStackUnwinderAVR.InitForThread(AThread: TDbgThread);
begin
  FThread := AThread;
end;

procedure TDbgStackUnwinderAVR.InitForFrame(ACurrentFrame: TDbgCallstackEntry;
  out CodePointer, StackPointer, FrameBasePointer: TDBGPtr);
var
  R: TDbgRegisterValue;
begin
  CodePointer := ACurrentFrame.AnAddress;

  // Frame pointer is r29:r28 (if used)
  FrameBasePointer := ACurrentFrame.FrameAdress;

  //Could update using GetStackBasePointerRegisterValue

  //R := ACurrentFrame.RegisterValueList.FindRegisterByDwarfIndex(FDwarfNumBP);
  //if R <> nil then
  //  FrameBasePointer := R.NumValue;

  StackPointer := 0;
  R := ACurrentFrame.RegisterValueList.FindRegisterByDwarfIndex(SPindex);
  if R = nil then exit;
  StackPointer := R.NumValue;
end;

procedure TDbgStackUnwinderAVR.GetTopFrame(out CodePointer, StackPointer,
  FrameBasePointer: TDBGPtr; out ANewFrame: TDbgCallstackEntry);
var
  i: Integer;
  R: TDbgRegisterValue;
begin
  FLastFrameBaseIncreased := True;
  CodePointer      := Thread.GetInstructionPointerRegisterValue;
  StackPointer     := Thread.GetStackPointerRegisterValue;
  FrameBasePointer := Thread.GetStackBasePointerRegisterValue;
  ANewFrame        := TDbgCallstackEntry.create(Thread, 0, FrameBasePointer, CodePointer);

  // Frame pointer may not have been updated yet
  if FrameBasePointer > StackPointer then
    FrameBasePointer := StackPointer;

  i := Thread.RegisterValueList.Count;
  while i > 0 do begin
    dec(i);
    R := Thread.RegisterValueList[i];
    ANewFrame.RegisterValueList.DbgRegisterAutoCreate[R.Name].SetValue(R.NumValue, R.StrValue, R.Size, R.DwarfIdx);
  end;
end;

function TDbgStackUnwinderAVR.Unwind(AFrameIndex: integer; var CodePointer,
  StackPointer, FrameBasePointer: TDBGPtr; ACurrentFrame: TDbgCallstackEntry;
  out ANewFrame: TDbgCallstackEntry): TTDbgStackUnwindResult;
const
  MAX_FRAMES = 50000; // safety net
  // To read RAM, add data space offset to address
  DataOffset = $800000;
  Size = 2;
var
  LastFrameBase: TDBGPtr;
  OutSideFrame: Boolean;
  startPC, endPC: TDBGPtr;
  returnAddrStackOffset: word;
  b: byte;
begin
  ANewFrame := nil;
  Result := suFailed;

  if StackPointer = 0 then
    exit;
  if not FLastFrameBaseIncreased then
    exit;

  OutSideFrame := False;
  LastFrameBase := FrameBasePointer;

  // Get start/end PC of proc from debug info
  if not Self.Process.FindProcStartEndPC(CodePointer, startPC, endPC) then
  begin
    { Assume we are at beginning of proc. GetFunctionFrameReturnAddress should then
      assume we are outside the stack frame (or no stack frame exists).
    }
    endPC := CodePointer;
  end;

  if not TAvrAsmDecoder(Process.Disassembler).GetFunctionFrameReturnAddress(CodePointer, startPC, endPC, returnAddrStackOffset, OutSideFrame) then
  begin
    OutSideFrame := False;
  end;

  if OutSideFrame then begin
    // Before adjustment of frame pointer, or after restoration of frame pointer,
    // return PC should be located by offset from SP
    if not Process.ReadData(DataOffset or (StackPointer + returnAddrStackOffset), Size, CodePointer) or
      (CodePointer = 0) then
      exit;
    {$PUSH}{$R-}{$Q-}
    StackPointer := StackPointer + returnAddrStackOffset + 1;
    FrameBasePointer := StackPointer; // After popping return-addr from "StackPtr"
    {$POP}
  end
  else begin
    // Inside stack frame, return PC should be located by offset from FP
    if not Process.ReadData(DataOffset or (FrameBasePointer + returnAddrStackOffset), Size, CodePointer) or (CodePointer = 0) then exit;
    {$PUSH}{$R-}{$Q-}
    FrameBasePointer := StackPointer + returnAddrStackOffset + Size - 1; // After popping return-addr from stack
    // An estimate of SP, needed when attempting unwinding of next frame
    // If registers are spilled to stack this will be wrong.
    StackPointer := FrameBasePointer;
    {$POP}
  end;
  // Convert return address from BE to LE, shl 1 to get byte address
  CodePointer := BEtoN(word(CodePointer)) shl 1;

  FLastFrameBaseIncreased := (FrameBasePointer <> 0) and (FrameBasePointer > LastFrameBase);

  ANewFrame:= TDbgCallstackEntry.create(Thread, AFrameIndex, FrameBasePointer, CodePointer);
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate[nPC].SetValue(CodePointer, IntToStr(CodePointer),Size, PCindex);
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate[nSP].SetValue(StackPointer, IntToStr(StackPointer),Size, SPindex);
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate['r28'].SetValue(byte(FrameBasePointer), IntToStr(byte(FrameBasePointer)),Size, 28);
  b := byte(FrameBasePointer shr 8);
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate['r29'].SetValue(b, IntToStr(b), Size, 29);

  FCodeReadErrCnt := 0;
  Result := suSuccess;
end;

initialization
  DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );

end.
