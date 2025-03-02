{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpdbgdisasxtensa.pp  -  Native Freepascal debugger - xtensa Disassembler
 ---------------------------------------------------------------------------

 This unit contains an xtensa disassembler for the Native Freepascal debugger

 ---------------------------------------------------------------------------

 @created(Thu Sep 2nd 2021)
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
unit FpDbgDisasXtensa;
{$mode objfpc}{$H+}
interface

uses
  SysUtils,
  FpDbgUtil, FpDbgInfo, DbgIntfBaseTypes, FpdMemoryTools, {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif},
  FpDbgClasses;

type
  //The function Disassemble decodes the instruction at the given address.
  //Unrecognized instructions are assumed to be data statements [dw XXXX]

  TXtensaAsmDecoder = class;

  TXtensaOpCode = (
    A_ABS, A_ADD, A_ADDI, A_ADDMI, A_ADDX2, A_ADDX4, A_ADDX8, A_ALL4, A_ALL8,
    A_AND, A_ANDB, A_ANDBC, A_ANY4, A_ANY8, A_B, A_BREAK, A_CALL0, A_CALL4,
    A_CALL8, A_CALL12, A_CALLX0, A_CALLX4, A_CALLX8, A_CALLX12, A_CEIL,
    A_CLAMPS, A_DHI, A_DHU, A_DHWB, A_DHWBI, A_DII, A_DIU, A_DIWB, A_DIWBI,
    A_DPFL, A_DPFR, A_DPFRO, A_DPW, A_DPWO, A_DSYNC, A_ENTRY, A_ESYNC, A_EXCW,
    A_EXTUI, A_EXTW, A_FLOAT, A_FLOOR, A_IDTLB, A_IHI, A_IHU, A_III, A_IITLB,
    A_IIU, A_ILL, A_IPF, A_IPFL, A_ISYNC, A_J, A_JX, A_L8UI, A_L16SI, A_L16UI,
    A_L32AI, A_L32E, A_L32I, A_L32R, A_LDCT, A_LDDEC, A_LDINC, A_LICT, A_LICW,
    A_LOOP, A_LOOPGTZ, A_LOOPNEZ, A_LSI, A_LSIU, A_LSX, A_LSXU, A_MADD, A_MAX,
    A_MAXU, A_MEMW, A_MIN, A_MINU, A_MOV, A_MOVEQZ, A_MOVF, A_MOVGEZ, A_MOVI,
    A_MOVLTZ, A_MOVNEZ, A_MOVSP, A_MOVT, A_MSUB, A_MUL, A_MUL16, A_MULA, A_MULL,
    A_MULS, A_MULSH, A_MULUH, A_NEG, A_NOP, A_NSA, A_NSAU, A_OEQ, A_OLE, A_OLT,
    A_OR, A_ORB, A_ORBC, A_PDTLB, A_PITLB, A_QUOS, A_QUOU, A_RDTLB0, A_RDTLB1,
    A_REMS, A_REMU, A_RET, A_RETW, A_RFDD, A_RFDE, A_RFE, A_RFI, A_RFME, A_RFR,
    A_RFUE, A_RFWO, A_RFWU, A_RITLB0, A_RITLB1, A_ROTW, A_ROUND, A_RSIL, A_RSR,
    A_RUR, A_S8I, A_S16I, A_S32C1I, A_S32E, A_S32I, A_S32RI, A_SDCT, A_SEXT,
    A_SICT, A_SICW, A_SIMCALL, A_SLL, A_SLLI, A_SRA, A_SRAI, A_SRC, A_SRL,
    A_SRLI, A_SSA8B, A_SSA8L, A_SSAI, A_SSI, A_SSIU, A_SSL, A_SSR, A_SSX,
    A_SSXU, A_SUB, A_SUBX2, A_SUBX4, A_SUBX8, A_SYSCALL, A_TRUNC, A_UEQ,
    A_UFLOAT, A_ULE, A_ULT, A_UMUL, A_UN, A_UTRUNC, A_WAITI, A_WDTLB, A_WER,
    A_WFR, A_WITLB, A_WSR, A_WUR, A_XOR, A_XORB, A_XSR, A_INVALID);

  TAVRInstruction = record
    OpCode: TXtensaOpCode;
    OpCodeMod: integer;  // Use as index to general opcode modifiers, typically in range 0..7
    Size: integer;
    Oper: array[1..2] of integer;
  end;
  { TXtensaAsmInstruction }

  TXtensaAsmInstruction = class(TDbgAsmInstruction)
  private const
    INSTR_CODEBIN_LEN = 4;
  private
    FAsmDecoder: TXtensaAsmDecoder;
    FAddress: TDBGPtr;
    FCodeBin: array[0..INSTR_CODEBIN_LEN-1] of byte;
    FFlags: set of (diCodeRead, diCodeReadError);
  protected
    procedure ReadCode; inline;
  public
    constructor Create(AAsmDecoder: TXtensaAsmDecoder);
    procedure SetAddress(AnAddress: TDBGPtr);
    function IsCallInstruction: boolean; override;
    function IsReturnInstruction: boolean; override;
    function IsLeaveStackFrame: boolean; override;
    function InstructionLength: Integer; override;
  end;

{ TXtensaAsmDecoder }

  TXtensaAsmDecoder = class(TDbgAsmDecoder)
  public type
    TCallType = (ctCall0, ctCall4, ctCall8, ctCall12, ct_CallUnknown);
  private const
    MaxPrologueSize = 63;  // Bytes, so ~22 instructions
    MaxEpilogueSize = MaxPrologueSize; // Perhaps a bit smaller, since the locals/parameters do not have to be initialized
    MAX_CODEBIN_LEN = MaxPrologueSize; // About 22 instructions
  private
    FProcess: TDbgProcess;
    FLastErrWasMem: Boolean;
    FCodeBin: array[0..MAX_CODEBIN_LEN-1] of byte;
    FLastInstr: TXtensaAsmInstruction;
    function FParsePrologue(AnAddress, AStartPC, AEndPC: TDBGPtr; ACallType: TCallType; out
      returnAddressOffset: word; out AnIsOutsideFrame: Boolean): Boolean;
    function FParseEpilogue(AnAddress, AStartPC, AEndPC: TDBGPtr; out
      returnAddressOffset: word; out AnIsOutsideFrame: Boolean): Boolean;
  protected
    function GetLastErrorWasMemReadErr: Boolean; override;
    function GetMaxInstrSize: integer; override;
    function GetMinInstrSize: integer; override;
    function GetCanReverseDisassemble: boolean; override;
    function ReadCodeAt(AnAddress: TDBGPtr; var ALen: Cardinal): Boolean; inline;
  public
    procedure Disassemble(var AAddress: Pointer; out ACodeBytes: String; out ACode: String); override;
    function GetInstructionInfo(AnAddress: TDBGPtr): TDbgAsmInstruction; override;

    // Rather use GetFunctionFrameReturnAddress
    function GetFunctionFrameInfo(AnAddress: TDBGPtr; out
      AnIsOutsideFrame: Boolean): Boolean; override;

    function GetBreakInstruction(const ALocation: TDBGPtr; out
       BreakInstructionLength: Integer): QWord;
     function BreakInstructionOffset: Int8;

     // AStartPC & AEndPC indicates proc limits to help with scanning for prologue/epilogue
    function GetFunctionFrameReturnAddress(AnAddress, AStartPC, AEndPC: TDBGPtr; ACallType: TCallType; out
      returnAddressOffset: word; out AnIsOutsideFrame: Boolean): Boolean;

    constructor Create(AProcess: TDbgProcess); override;
    destructor Destroy;
  end;


implementation

uses
  StrUtils, LazClasses, Math;

var
  DBG_WARNINGS: PLazLoggerLogGroup;

const
  op          = '%s';
  op_a1       = '%s a%d';
  op_c1       = '%s %d';
  op_x1       = '%s %x';
  op_a1a2     = '%s a%d, a%d';
  op_a1b2     = '%s a%d, b%d';
  op_a1c2     = '%s a%d, %d';
  op_a1f2     = '%s a%d, f%d';
  op_a1m2     = '%s a%d, m%d';
  op_a1x2     = '%s a%d, %x';
  op_b1b2     = '%s b%d, b%d';
  op_b1c2     = '%s b%d, %d';
  op_c1c2     = '%s %d, %d';
  op_f1f2     = '%s f%d, f%d';
  op_f1a2     = '%s f%d, a%d';
  op_f1c2     = '%s f%d, %d';
  op_m1m2     = '%s m%d, m%d';
  op_m1a2     = '%s m%d, a%d';
  op_a1a2a3   = '%s a%d, a%d, a%d';
  op_a1a2b3   = '%s a%d, a%d, b%d';
  op_a1a2c3   = '%s a%d, a%d, %d';
  op_a1a2x3   = '%s a%d, a%d, %x';
  op_a1c2c3   = '%s a%d, %d, %d';
  op_a1c2x3   = '%s a%d, %d, %x';
  op_a1f2c3   = '%s a%d, f%d, %d';
  op_b1b2b3   = '%s b%d, b%d, b%d';
  op_b1f2f3   = '%s b%d, f%d, f%d';
  op_f1f2b3   = '%s f%d, f%d, b%d';
  op_f1f2c3   = '%s f%d, f%d, %d';
  op_f1f2f3   = '%s f%d, f%d, f%d';
  op_f1f2a3   = '%s f%d, f%d, a%d';
  op_f1a2b3   = '%s f%d, a%d, b%d';
  op_f1a2c3   = '%s f%d, a%d, %d';
  op_f1a2a3   = '%s f%d, a%d, a%d';
  op_m1a2m3m4 = '%s m%d, a%d, m%d, m%d';
  op_m1a2m3r4 = '%s m%d, a%d, m%d, m%d';
  op_a1a2c3c4 = '%s a%d, a%d, %d, %d';
  hreg: array of string = ('ll', 'hl', 'lh', 'hh');
  callID: array of string = ('call0', 'call4', 'call8', 'call12');
  b4const_signed: array of integer = (-1, 1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 16, 32, 64, 128, 256);
  b4const_unsigned: array of integer = (32768, 65536, 2, 3, 4, 5, 6, 7, 8, 10, 12, 16, 32, 64, 128, 256);

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
  // Not yet implemented in FPC, but more compact option
  OpCodeAdjustFrameMask  = $9720;   // sbiw r28, k,  1001 0111 kkdd kkkk, rd = 24 + d*2
  OpCodeStoreOnStackMask = $8208;   // std Y+2, r24  10q0 qq1r rrrr 1qqq

{ TXtensaAsmInstruction }

procedure TXtensaAsmInstruction.ReadCode;
begin
  if not (diCodeRead in FFlags) then begin
    if not FAsmDecoder.FProcess.ReadData(FAddress, INSTR_CODEBIN_LEN, FCodeBin) then
      Include(FFlags, diCodeReadError);
    Include(FFlags, diCodeRead);
  end;
end;

constructor TXtensaAsmInstruction.Create(AAsmDecoder: TXtensaAsmDecoder);
begin
  FAsmDecoder := AAsmDecoder;
  inherited Create;
  AddReference;
end;

procedure TXtensaAsmInstruction.SetAddress(AnAddress: TDBGPtr);
begin
  FAddress := AnAddress;
  FFlags := [];
end;

function TXtensaAsmInstruction.IsCallInstruction: boolean;
var
  op0, op1, op2, r, m: byte;
begin
  Result := False;
  ReadCode;
  op0 := lo(FCodeBin[0]);
  op1 := lo(FCodeBin[2]);
  op2 := hi(FCodeBin[2]);
  r := hi(FCodeBin[1]);
  m := FCodeBin[0] shr 6;

  if ((op0 = 0) and (op1 = 0) and (op2 = 0) and
      (r = 0) and (m = 3)) or                       // callx
     (op0 = 5) then                                 // call
    Result := true;
end;

function TXtensaAsmInstruction.IsReturnInstruction: boolean;
var
  op0, op1, op2, r, t, m, n: byte;
begin
  Result := False;
  ReadCode;

  op0 := lo(FCodeBin[0]);
  op1 := lo(FCodeBin[2]);
  op2 := hi(FCodeBin[2]);
  r := hi(FCodeBin[1]);
  t := hi(FCodeBin[0]);
  m := FCodeBin[0] shr 6;
  n := hi(FCodeBin[0]) and 3;

  if ((op0 = 0) and (op1 = 0) and (op2 = 0) and
      (r = 0) and (m = 2) and (n in [0, 1])) or        // ret, retw
     ((op0 = 13) and (r = 15) and (t in [0, 1])) then  // ret.n, retw.n
    Result := true;
end;

function TXtensaAsmInstruction.IsLeaveStackFrame: boolean;
begin
  Result := false;
end;

function TXtensaAsmInstruction.InstructionLength: Integer;
begin
  Result := 3;
  ReadCode;
  // op0 in 8..13
  if lo(FCodeBin[0]) in [8..13] then // narrow instruction series
    Result := 2;
end;

type
  TPrologueState = (psStart, psEntry, psStoreParams, psBody, psReturn);

function TXtensaAsmDecoder.FParsePrologue(AnAddress, AStartPC, AEndPC: TDBGPtr;
  ACallType: TCallType; out returnAddressOffset: word; out
  AnIsOutsideFrame: Boolean): Boolean;
var
  ADataLen: Cardinal;
  //AData: PByte;
  //stackState: TPrologueState;
begin
  ADataLen := Min(MaxPrologueSize, AnAddress - AStartPC + 3);
  Result := ReadCodeAt(AStartPC, ADataLen);
  if not Result then
    exit;
  //AData := @FCodeBin[0];

  // Example prologue for procedure call2(p2: integer);
  //  004136        	entry	a1, 32
  //  0129      	s32i.n	a2, a1, 0  // store p2 to stack

  AnIsOutsideFrame := true;
  //stackState := psStart;
end;

function TXtensaAsmDecoder.FParseEpilogue(AnAddress, AStartPC, AEndPC: TDBGPtr;
  out returnAddressOffset: word; out AnIsOutsideFrame: Boolean): Boolean;
begin
end;

function TXtensaAsmDecoder.GetLastErrorWasMemReadErr: Boolean;
begin
  Result := FLastErrWasMem;
end;

function TXtensaAsmDecoder.GetMaxInstrSize: integer;
begin
  Result := 3;
end;

function TXtensaAsmDecoder.GetMinInstrSize: integer;
begin
  Result := 2;
end;

function TXtensaAsmDecoder.GetCanReverseDisassemble: boolean;
begin
  Result := False;
end;

function TXtensaAsmDecoder.ReadCodeAt(AnAddress: TDBGPtr; var ALen: Cardinal
  ): Boolean;
begin
  Result := FProcess.ReadData(AnAddress, ALen, FCodeBin[0], ALen);
  FLastErrWasMem := not Result;
end;

procedure TXtensaAsmDecoder.Disassemble(var AAddress: Pointer; out
  ACodeBytes: String; out ACode: String);
var
  CodeIdx: byte;
  pcode: PByte;
  c0, c1, c2, op0, op1, op2, r, s, t: byte;
  imm: int32;
  m, n: byte;
  w, x, y, half: byte;
begin
  pcode := AAddress;
  CodeIdx := 0;
  c0 := pcode[CodeIdx];
  inc(CodeIdx);
  c1 := pcode[CodeIdx];
  inc(CodeIdx);

  // Values used for narrow and 3 byte instructions
  op0 := lo(c0);
  r := hi(c1);
  s := lo(c1);
  t := hi(c0);

  // Values used for 3 byte instructions, only read if not a narrow instruction
  if not(op0 in [8..13]) then
  begin
    c2 := pcode[CodeIdx];
    inc(CodeIdx);
    op1 := lo(c2);
    op2 := hi(c2);
  end;

  case op0 of  // Table 7-192 / Whole Opcode Space
    0 : case op1 of  // table 7-193 / QRST
          0 : case op2 of  // table 7-194 / RST0
                0 : case r of  // table 7-195 / ST0
                      0 : begin  // table 7-196 / SNM0
                            m := c0 shr 6;
                            n := hi(c0) and 3;
                            case m of
                              0: ACode := 'ill';
                              1: ACode := 'reserved';
                              2: case n of  // table 7-197 / JR
                                   0: ACode := 'ret';
                                   1: ACode := 'retw';
                                   2: ACode := format(op_a1, ['jx', s]);
                                   3: ACode := 'reserved';
                                 end;
                              3: case n of  // table 7-198 / CALLX
                                   0: ACode := format(op_a1, ['callx0', s]);
                                   1: ACode := format(op_a1, ['callx4', s]);
                                   2: ACode := format(op_a1, ['callx8', s]);
                                   3: ACode := format(op_a1, ['callx12', s]);
                                 end;
                            end;
                          end;
                      1 : ACode := format(op_a1a2, ['movsp', t, s]);
                      2 : case t of  // table 7-199 / SYNC
                            0 : ACode := 'isync';
                            1 : ACode := 'rsync';
                            2 : ACode := 'esync';
                            3 : ACode := 'dsync';
                            8 : ACode := 'excw';
                            12: ACode := 'memw';
                            13: ACode := 'extw';
                            15: ACode := 'nop';
                            else
                              ACode := 'reserved';
                          end;
                      3 : case t of  // table 7-200 / RFEI
                            0 : case s of  // table 7-201 / RFET
                                  0: ACode := 'rfe';
                                  1: ACode := 'rfue';
                                  2: ACode := 'rfde';
                                  4: ACode := 'rfwo';
                                  5: ACode := 'rfwu';
                                  otherwise
                                    ACode := 'reserved';
                                end;
                            1: ACode := format(op_c1, ['rfi', s]);
                            2: case s of // Table RFM
                                 0: ACode := 'rfme';
                                 1: ACode := 'clrex';
                                 otherwise
                                   ACode := 'reserved';
                               end;
                            3: case t of  // BLKSR - Listed but not described in document from 2022
                                 0: ACode := 'pfend.a';
                                 1: ACode := 'pfend.o';
                                 2: ACode := 'pfnxt.f';
                                 3: ACode := 'pfwait.a';
                                 4: ACode := 'pfwait.r';
                                 otherwise
                                   ACode := 'reserved';
                               end;
                            otherwise
                              ACode := 'reserved';
                          end;
                      4 : ACode := format(op_c1c2, ['break', s, t]);  // xtensa-esp32-elf-objdump decodes the operands in this sequence
                      5 : case s of  // SYSIM
                            0: ACode := 'syscall';
                            1: ACode := 'simcall';
                            2: ACode := 'halt';
                            otherwise
                              ACode := 'reserved';
                          end;
                      6 : ACode := format(op_a1c2,['rsil', t, s]);
                      7 : case s of  // WTLS
                            0: format(op_c1, ['wait', s]);
                            // 4..7: CSR parity error test, cannot find description
                            14:  ACode := 'lddr32.p';
                            15:  ACode := 'sddr32.p';
                            otherwise
                              ACode := 'reserved';
                          end;
                      8 : ACode := format(op_b1b2, ['any4', t, s]);
                      9 : ACode := format(op_b1b2, ['all4', t, s]);
                      10: ACode := format(op_b1b2, ['any8', t, s]);
                      11: ACode := format(op_b1b2, ['all8', t, s]);
                      otherwise
                        ACode := 'reserved';
                    end;
                1 : ACode := format(op_a1a2a3, ['and', r, s, t]);
                2 : ACode := format(op_a1a2a3, ['or', r, s, t]);
                3 : ACode := format(op_a1a2a3, ['xor', r, s, t]);
                4 : case r of  // table 7-202 / ST1
                      0 : ACode := format(op_a1, ['ssr', s]);
                      1 : ACode := format(op_a1, ['ssl', s]);
                      2 : ACode := format(op_a1, ['ssa8l', s]);
                      3 : ACode := format(op_a1, ['ssa8b', s]);
                      4 : ACode := format(op_c1, ['ssai', s or ((t and 1) shl 4)]);
                      6 : ACode := format(op_a1a2, ['rer', t, s]);
                      7 : ACode := format(op_a1a2, ['wer', t, s]);
                      8 : ACode := format(op_c1, ['rotw', SarShortint(shortint(t shl 4), 4)]);
                      10: ACode := format(op_a1, ['getex', t]);
                      14: ACode := format(op_a1a2, ['nsa', t, s]);
                      15: ACode := format(op_a1a2, ['nsau', t, s]);
                      otherwise
                        ACode := 'reserved';
                    end;
                5 : case r of  // table 7-203 / TLB
                      3 : ACode := format(op_a1a2, ['ritlb0', t, s]);
                      4 : ACode := format(op_a1, ['iitlb', s]);
                      5 : ACode := format(op_a1a2, ['pitlb', t, s]);
                      6 : ACode := format(op_a1a2, ['witlb', t, s]);
                      7 : ACode := format(op_a1a2, ['ritlb1', t, s]);
                      11: ACode := format(op_a1a2, ['rdtlb0', t, s]);  // or rptlb0
                      12: ACode := format(op_a1, ['idtlb', s]);
                      13: ACode := format(op_a1a2, ['pdtlb', t, s]);
                      14: ACode := format(op_a1a2, ['wdtlb', t, s]);   // or wptlb
                      15: ACode := format(op_a1a2, ['rdtlb1', t, s]);  // or rptlb1
                      otherwise
                        ACode := 'reserved';
                    end;
                6 : case s of  // table 7-204 / RT0
                      0 : ACode := format(op_a1a2, ['neg', r, t]);
                      1 : ACode := format(op_a1a2, ['abs', r, t]);
                      otherwise
                        ACode := 'reserved';
                    end;
                8 : ACode := format(op_a1a2a3, ['add', r, s, t]);
                9 : ACode := format(op_a1a2a3, ['addx2', r, s, t]);
                10: ACode := format(op_a1a2a3, ['addx4', r, s, t]);
                11: ACode := format(op_a1a2a3, ['addx8', r, s, t]);
                12: ACode := format(op_a1a2a3, ['sub', r, s, t]);
                13: ACode := format(op_a1a2a3, ['subx2', r, s, t]);
                14: ACode := format(op_a1a2a3, ['subx4', r, s, t]);
                15: ACode := format(op_a1a2a3, ['subx8', r, s, t]);
                otherwise
                  ACode := 'reserved';
              end;
          1 : case op2 of  // table 7-205 / RST1
                0, 1 : ACode := format(op_a1a2c3, ['slli', r, s, 32 - (t or ((op2 and 1) shl 4))]);
                2, 3 : ACode := format(op_a1a2c3, ['srai', r, t, s or ((op2 and 1) shl 4)]);
                4 : ACode := format(op_a1a2c3, ['srli', r, t, s]);
                6 : ACode := format(op_a1c2, ['xsr', r, c1]);
                7 : ACode := 'unclear'; // Table 7-206 / ACCER => encoding doesn't match instructions
                8 : ACode := format(op_a1a2a3, ['src', r, s, t]);
                9 : ACode := format(op_a1a2, ['srl', r, t]);
                10: ACode := format(op_a1a2, ['sll', r, s]);
                11: ACode := format(op_a1a2, ['sra', r, t]);
                12: ACode := format(op_a1a2a3, ['mul16u', r, s, t]);
                13: ACode := format(op_a1a2a3, ['mul16s', r, s, t]);
                15: case r of  // Table 7-207 / IMP
                      0 : ACode := format(op_a1a2, ['lict', t, s]);
                      1 : ACode := format(op_a1a2, ['sict', t, s]);
                      2 : ACode := format(op_a1a2, ['licw', t, s]);
                      3 : ACode := format(op_a1a2, ['sicw', t, s]);
                      4 : ACode := format(op_a1a2, ['l32ex', t, s]);
                      5 : ACode := format(op_a1a2, ['s32ex', t, s]);
                      8 : ACode := format(op_a1a2, ['ldct', t, s]);
                      9 : ACode := format(op_a1a2, ['sdct', t, s]);
                      14: case t of  // Table 7-208 / RFDX
                            0 : ACode := 'rfdo';
                            1 : ACode := 'rfdd';
                            otherwise
                              ACode := 'reserved';
                          end;
                      otherwise
                        ACode := 'reserved';
                    end;
                otherwise
                  ACode := 'reserved';
              end;
          2 : case op2 of  // table 7-209 / RST2
                0 : ACode := format(op_b1b2b3, ['andb', r, s, t]);
                1 : ACode := format(op_b1b2b3, ['andbc', r, s, t]);
                2 : ACode := format(op_b1b2b3, ['orb', r, s, t]);
                3 : ACode := format(op_b1b2b3, ['orbc', r, s, t]);
                4 : ACode := format(op_b1b2b3, ['xorb', r, s, t]);
                6 : ACode := format(op_a1a2a3, ['saltu', r, s, t]);
                7 : ACode := format(op_a1a2a3, ['salt', r, s, t]);
                8 : ACode := format(op_a1a2a3, ['mull', r, s, t]);
                10: ACode := format(op_a1a2a3, ['muluh', r, s, t]);
                11: ACode := format(op_a1a2a3, ['mulsh', r, s, t]);
                12: ACode := format(op_a1a2a3, ['quou', r, s, t]);
                13: ACode := format(op_a1a2a3, ['quos', r, s, t]);
                14: ACode := format(op_a1a2a3, ['remu', r, s, t]);
                15: ACode := format(op_a1a2a3, ['rems', r, s, t]);
                otherwise
                  ACode := 'reserved';
              end;
          3 : case op2 of  // table 7-210 / RST3
                0 : ACode := format(op_a1c2, ['rsr', t, c1]);
                1 : ACode := format(op_a1c2, ['wsr', t, c1]);
                2 : ACode := format(op_a1a2c3, ['sext', r, s, t+7]);
                3 : ACode := format(op_a1a2c3, ['clamps', r, s, t+7]);
                4 : ACode := format(op_a1a2a3, ['min', r, s, t]);
                5 : ACode := format(op_a1a2a3, ['max', r, s, t]);
                6 : ACode := format(op_a1a2a3, ['minu', r, s, t]);
                7 : ACode := format(op_a1a2a3, ['maxu', r, s, t]);
                8 : ACode := format(op_a1a2a3, ['moveqz', r, s, t]);
                9 : ACode := format(op_a1a2a3, ['movnez', r, s, t]);
                10: ACode := format(op_a1a2a3, ['movltz', r, s, t]);
                11: ACode := format(op_a1a2a3, ['movgez', r, s, t]);
                12: ACode := format(op_a1a2b3, ['movf', r, s, t]);
                13: ACode := format(op_a1a2b3, ['movt', r, s, t]);
                14: ACode := format(op_a1c2, ['rur', r, 16*s+t]);
                15: ACode := format(op_a1c2, ['wur', r, 16*s+t]);
                otherwise
                  ACode := 'reserved';
              end;
          4, 5: ACode := format(op_a1a2c3c4, ['extui', r, t, word(s) or (word(op1 and 1) shl 4), op2+1]);
          6 : ACode := 'cust0';
          7 : ACode := 'cust1';
          8 : case op2 of  // table 7-211 / LSCX
                0 : ACode := format(op_f1a2a3, ['lsx', r, t, s]);
                1 : ACode := format(op_f1a2a3, ['lsxu', r, t, s]);
                2 : ACode := format(op_f1a2a3, ['ldx', r, t, s]);
                3 : ACode := format(op_f1a2a3, ['ldxp', r, t, s]);
                4 : ACode := format(op_f1a2a3, ['ssx', r, t, s]);
                5 : ACode := format(op_f1a2a3, ['ssxu', r, t, s]);
                6 : ACode := format(op_f1a2a3, ['sdx', r, t, s]);
                7 : ACode := format(op_f1a2a3, ['sdxp', r, t, s]);
                otherwise
                  ACode := 'reserved';
              end;
          9 : case op2 of  // table 7-212 / LSC4
                0 : ACode := format(op_a1a2c3, ['l32e', t, s, -4*(16-r)]);
                1 : case r of // Table BLKPRF
                      1 : ACode := format(op_a1a2, ['dpfr.b', s, t]);
                      2 : ACode := format(op_a1a2, ['dpfw.b', s, t]);
                      3 : ACode := format(op_a1a2, ['dpfm.b', s, t]);
                      5 : ACode := format(op_a1a2, ['dpfr.bf', s, t]);
                      6 : ACode := format(op_a1a2, ['dpfw.bf', s, t]);
                      7 : ACode := format(op_a1a2, ['dpfm.bf', s, t]);
                      9 : ACode := format(op_a1a2, ['dhi.b', s, t]);
                      10: ACode := format(op_a1a2, ['dhwb.b', s, t]);
                      11: ACode := format(op_a1a2, ['dhwbi.b', s, t]);
                      otherwise
                        ACode := 'reserved';
                    end;
                // 2 : Table DISPL, instructions not described
                4 : ACode := format(op_a1a2c3, ['s32e', t, s, -4*(16-r)]);
                5 : ACode := format(op_a1a2c3, ['s32nb', t, s, 4*r]);
                // 10: Table DISPS, instructions not described
                otherwise
                  ACode := 'reserved';
              end;
          10: case op2 of  // table 7-213 / FP0
                0 : ACode := format(op_f1f2f3, ['add.s', r, s, t]);
                1 : ACode := format(op_f1f2f3, ['sub.s', r, s, t]);
                2 : ACode := format(op_f1f2f3, ['mul.s', r, s, t]);
                4 : ACode := format(op_f1f2f3, ['madd.s', r, s, t]);
                5 : ACode := format(op_f1f2f3, ['msub.s', r, s, t]);
                6 : ACode := format(op_f1f2f3, ['maddn.s', r, s, t]);
                7 : ACode := format(op_f1f2f3, ['divn.s', r, s, t]);
                8 : ACode := format(op_a1f2c3, ['round.s', r, s, t]);
                9 : ACode := format(op_a1f2c3, ['trunc.s', r, s, t]);
                10: ACode := format(op_a1f2c3, ['floor.s', r, s, t]);
                11: ACode := format(op_a1f2c3, ['ceil.s', r, s, t]);
                12: ACode := format(op_f1a2c3, ['float.s', r, s, t]);
                13: ACode := format(op_f1a2c3, ['ufloat.s', r, s, t]);
                14: ACode := format(op_a1f2c3, ['utrunc.s', r, s, t]);
                15: case t of  // table 7-214 / FP1OP
                      0 : ACode := format(op_f1f2, ['mov.s', r, s]);
                      1 : ACode := format(op_f1f2, ['abs.s', r, s]);
                      2 : ACode := format(op_f1f2, ['cvtd.s', r, s]);
                      3 : ACode := format(op_f1c2, ['const.s', r, s]);
                      4 : ACode := format(op_a1f2, ['rfr', r, s]);
                      5 : ACode := format(op_f1a2, ['wfr', r, s]);
                      6 : ACode := format(op_f1f2, ['neg.s', r, s]);
                      7 : ACode := format(op_f1f2, ['div0.s', r, s]);
                      8 : ACode := format(op_f1f2, ['recip0.s', r, s]);
                      9 : ACode := format(op_f1f2, ['sqrt0.s', r, s]);
                      10: ACode := format(op_f1f2, ['rsqrt0.s', r, s]);
                      11: ACode := format(op_f1f2, ['nexp01.s', r, s]);
                      12: ACode := format(op_f1f2, ['mksadj.s', r, s]);
                      13: ACode := format(op_f1f2, ['mkdadj.s', r, s]);
                      14: ACode := format(op_f1f2, ['addexp.s', r, s]);
                      15: ACode := format(op_f1f2, ['addexpm.s', r, s]);
                      otherwise
                        ACode := 'reserved';
                    end;
                otherwise
                  ACode := 'reserved';
              end;
          11: case op2 of  // table 7-215 / FP1
                1 : ACode := format(op_b1f2f3, ['un.s', r, s, t]);
                2 : ACode := format(op_b1f2f3, ['oeq.s', r, s, t]);
                3 : ACode := format(op_b1f2f3, ['ueq.s', r, s, t]);
                4 : ACode := format(op_b1f2f3, ['olt.s', r, s, t]);
                5 : ACode := format(op_b1f2f3, ['ult.s', r, s, t]);
                6 : ACode := format(op_b1f2f3, ['ole.s', r, s, t]);
                7 : ACode := format(op_b1f2f3, ['ule.s', r, s, t]);
                8 : ACode := format(op_f1f2a3, ['moveqz.s', r, s, t]);
                9 : ACode := format(op_f1f2a3, ['movnez.s', r, s, t]);
                10: ACode := format(op_f1f2a3, ['movltz.s', r, s, t]);
                11: ACode := format(op_f1f2a3, ['movgez.s', r, s, t]);
                12: ACode := format(op_f1f2b3, ['movf.s', r, s, t]);
                13: ACode := format(op_f1f2b3, ['movt.s', r, s, t]);
                otherwise
                  ACode := 'reserved';
              end;
          14: case op2 of  // Table DFP1
                1 : ACode := format(op_b1f2f3, ['un.d', r, s, t]);
                2 : ACode := format(op_b1f2f3, ['oeq.d', r, s, t]);
                3 : ACode := format(op_b1f2f3, ['ueq.d', r, s, t]);
                4 : ACode := format(op_b1f2f3, ['olt.d', r, s, t]);
                5 : ACode := format(op_b1f2f3, ['ult.d', r, s, t]);
                6 : ACode := format(op_b1f2f3, ['ole.d', r, s, t]);
                7 : ACode := format(op_b1f2f3, ['ule.d', r, s, t]);
                8 : ACode := format(op_f1a2a3, ['wfrd', r, s, t]);
                otherwise
                  ACode := 'reserved';
              end;
          15: case op2 of  // Table DFP0
                0 : ACode := format(op_f1f2f3, ['add.d', r, s, t]);
                1 : ACode := format(op_f1f2f3, ['sub.d', r, s, t]);
                2 : ACode := format(op_f1f2f3, ['mul.d', r, s, t]);
                4 : ACode := format(op_f1f2f3, ['madd.d', r, s, t]);
                5 : ACode := format(op_f1f2f3, ['msub.d', r, s, t]);
                6 : ACode := format(op_f1f2f3, ['maddn.d', r, s, t]);
                7 : ACode := format(op_f1f2f3, ['divn.d', r, s, t]);
                8 : ACode := format(op_a1f2c3, ['round.d', r, s, t]);
                9 : ACode := format(op_a1f2c3, ['trunc.d', r, s, t]);
                10: ACode := format(op_a1f2c3, ['floor.d', r, s, t]);
                11: ACode := format(op_a1f2c3, ['ceil.d', r, s, t]);
                otherwise
                  ACode := 'reserved';
              end;
          otherwise
            ACode := 'reserved';
        end;
    1 : begin
          imm := word(c1) or (word(c2) shl 8);
          imm := {(int32(AAddress+3) and $FFFFFFFC) +} int32((uint32(imm) shl 2) or $FFFC0000);
          ACode := format(op_a1c2, ['l32r', t, imm]);
        end;
    2 : case r of  // table 7-216 / LSAI
          0 : ACode := format(op_a1a2c3, ['l8ui', t, s, c2]);
          1 : ACode := format(op_a1a2c3, ['l16ui', t, s, 2*word(c2)]);
          2 : ACode := format(op_a1a2c3, ['l32i', t, s, 4*word(c2)]);
          4 : ACode := format(op_a1a2c3, ['s8i', t, s, c2]);
          5 : ACode := format(op_a1a2c3, ['s16i', t, s, 2*word(c2)]);
          6 : ACode := format(op_a1a2c3, ['s32i', t, s, 4*word(c2)]);
          7 : case t of  // table 7-217 / CACHE
                0 : ACode := format(op_a1c2, ['dpfr', s, c2]);
                1 : ACode := format(op_a1c2, ['dpfw', s, c2]);
                2 : ACode := format(op_a1c2, ['dpfro', s, c2]);
                3 : ACode := format(op_a1c2, ['dpfwo', s, c2]);
                4 : ACode := format(op_a1c2, ['dhwb', s, c2]);
                5 : ACode := format(op_a1c2, ['dhwbi', s, c2]);
                6 : ACode := format(op_a1c2, ['dhi', s, c2]);
                7 : ACode := format(op_a1c2, ['dii', s, c2]);
                8 : case op1 of  // table 7-218 / DCE
                      0 : ACode := format(op_a1c2, ['dpfl', s, 16*word(op2)]);
                      1 : ACode := format(op_a1c2, ['dci', s, 16*word(op2)]);
                      2 : ACode := format(op_a1c2, ['dhu', s, 16*word(op2)]);
                      3 : ACode := format(op_a1c2, ['diu', s, 16*word(op2)]);
                      4 : ACode := format(op_a1c2, ['diwb', s, 16*word(op2)]);
                      5 : ACode := format(op_a1c2, ['diwbi', s, 16*word(op2)]);
                      6 : ACode := format(op_a1c2, ['dcwb', s, 16*word(op2)]);
                      7 : ACode := format(op_a1c2, ['dcwbi', s, 16*word(op2)]);
                      otherwise
                        ACode := 'reserved';
                    end;
                12: ACode := format(op_a1c2, ['ipf', s, 4*word(c2)]);
                13: case op1 of  // table 7-219 / ICE
                      0 : ACode := format(op_a1c2, ['ipfl', s, 16*word(op2)]);
                      2 : ACode := format(op_a1c2, ['ihu', s, 16*word(op2)]);
                      3 : ACode := format(op_a1c2, ['iiu', s, 16*word(op2)]);
                      otherwise
                        ACode := 'reserved';
                    end;
                14: ACode := format(op_a1c2, ['ihi', s, 4*word(c2)]);
                15: ACode := format(op_a1c2, ['iii', s, 4*word(c2)]);
                otherwise
                  ACode := 'reserved';
              end;
          9 : ACode := format(op_a1a2c3, ['l16si', t, s, 2*word(c2)]);
          10: begin
                imm := c2 + (word(s) shl 8);
                if imm and $800 > 0 then
                  imm := int32(uint32(imm) or $FFFFF000);
                ACode := format(op_a1c2, ['movi', t, imm]);
              end;
          11: ACode := format(op_a1a2c3, ['l32ai', t, s, 4*word(c2)]);
          12: ACode := format(op_a1a2c3, ['addi', t, s, ShortInt(c2)]);
          13: ACode := format(op_a1a2c3, ['addmi', t, s, integer(ShortInt(c2)) shl 8]);  // xtensa-esp32-elf-objdump show the constant as hex value
          14: ACode := format(op_a1a2c3, ['s32c1i', t, s, 4*word(c2)]);
          15: ACode := format(op_a1a2c3, ['s32ri', t, s, 4*word(c2)]);
          otherwise
            ACode := 'reserved';
        end;
    3 : case r of // table 7-220 / LSCI
          0 : ACode := format(op_f1a2c3, ['lsi', t, s, 4*word(c2)]);
          1 : ACode := format(op_f1a2c3, ['ldi', t, s, 4*word(c2)]);
          4 : ACode := format(op_f1a2c3, ['ssi', t, s, 4*word(c2)]);
          5 : ACode := format(op_f1a2c3, ['sdi', t, s, 4*word(c2)]);
          8 : ACode := format(op_f1a2c3, ['lsiu', t, s, 4*word(c2)]);
          9 : ACode := format(op_f1a2c3, ['ldiu', t, s, 4*word(c2)]);
          12: ACode := format(op_f1a2c3, ['ssiu', t, s, 4*word(c2)]);
          13: ACode := format(op_f1a2c3, ['sdiu', t, s, 4*word(c2)]);
          otherwise
            ACode := 'reserved';
        end;
    4 : begin
          w := hi(c1) and 3;
          x := (hi(c1) shr 2) and 1;
          y := ((c0 shr 6) and 1) + 2;
          half := c2 and 3;
          case op2 of  // table 7-221 / MAC16D
            0 : case op1 of  // table 7-222 / MACID
                  8..11 : ACode := format(op_m1a2m3m4, ['mula.dd.'+hreg[half]+'.ldinc', w, s, x, y]);
                  otherwise
                    ACode := 'reserved';
                end;
            1 : case op1 of  // table 7-226 / MACCD
                  8..11 : ACode := format(op_m1a2m3m4, ['mula.dd.'+hreg[half]+'.lddec', w, s, x, y]);
                  otherwise
                    ACode := 'reserved';
                end;
            2 : case op1 of  // table 7-224 / MACDD
                  4..7  : ACode := format(op_m1m2, ['mul.dd.'+hreg[half], x, y]);
                  8..11 : ACode := format(op_m1m2, ['mula.dd.'+hreg[half], x, y]);
                  12..15: ACode := format(op_m1m2, ['muls.dd.'+hreg[half], x, y]);
                  otherwise
                    ACode := 'reserved';
                end;
            3 : case op1 of  // table 7-225 / MACAD
                  4..7  : ACode := format(op_a1m2, ['mul.ad.'+hreg[half], s, y]);
                  8..11 : ACode := format(op_a1m2, ['mula.ad.'+hreg[half], s, y]);
                  12..15: ACode := format(op_a1m2, ['muls.ad.'+hreg[half], s, y]);
                  otherwise
                    ACode := 'reserved';
                end;
            4 : case op1 of  // table 7-223 / MACIA
                  8..11 : ACode := format(op_m1a2m3r4, ['mula.da.'+hreg[half]+'.ldinc', w, s, x, t]);
                  otherwise
                    ACode := 'reserved';
                end;
            5 : case op1 of  // table 7-227 / MACCA
                  8..11 : ACode := format(op_m1a2m3r4, ['mula.da.'+hreg[half]+'.lddec', w, s, x, t]);
                  otherwise
                    ACode := 'reserved';
                end;
            6 : case op1 of  // table 7-228 / MACDA
                  4..7  : ACode := format(op_m1a2, ['mul.da.'+hreg[half], x, t]);
                  8..11 : ACode := format(op_m1a2, ['mula.da.'+hreg[half], x, t]);
                  12..15: ACode := format(op_m1a2, ['muls.da.'+hreg[half], x, t]);
                  otherwise
                    ACode := 'reserved';
                end;
            7 : case op1 of  // table 7-229 / MACAA
                  0..3  : ACode := format(op_a1a2, ['umul.aa.'+hreg[half], s, t]);
                  4..7  : ACode := format(op_a1a2, ['mul.aa.'+hreg[half], s, t]);
                  8..11 : ACode := format(op_a1a2, ['mula.aa.'+hreg[half], s, t]);
                  12..15: ACode := format(op_a1a2, ['muls.aa.'+hreg[half], s, t]);
                  otherwise
                    ACode := 'reserved';
                end;
            8 : case op1 of  // table 7-230 / MACI
                  0 : ACode := format(op_m1a2, ['ldinc', w, s]);
                  otherwise
                    ACode := 'reserved';
                end;
            9 : case op1 of  // table 7-231 / MACC
                  0 : ACode := format(op_m1a2, ['lddec', w, s]);
                  otherwise
                    ACode := 'reserved';
                end;
            otherwise
              ACode := 'reserved';
          end;
        end;
    5 : begin  // table 7-232 / CALLN
          n := (c0 shr 4) and 3;
          // Left shift into 32 bit
          imm := (int32(c2) shl 24) or (int32(c1) shl 16) or (int32(c0 and $C0) shl 8);
          // Then right shift to get 20 bit signed value
          imm := {(int32(AAddress) and $FFFFFFFC) +} int32((SarLongint(imm, 12) + 4) and $FFFFFFFC);
          ACode := format(op_c1, [callID[n], imm]);
        end;
    6 : begin
          m := (c0 shr 6);
          n := (c0 shr 4) and 3;
          case n of  // table 7-233 / SI
            0 : begin
                  // Left shift into 32 bit
                  imm := (int32(c2) shl 24) or (int32(c1) shl 16) or (int32(c0 and $C0) shl 8);
                  // Then right shift to get 18 bit signed value
                  imm := {int32(AAddress) +} SarLongint(imm, 14) + 4;
                  ACode := format(op_c1, ['j', imm]);
                end;
            1 : begin
                  // Left shift into 32 bit
                  imm := (int32(c2) shl 24) or (int32(c1) shl 16);
                  // Then right shift to get 12 bit signed value
                  imm := {int32(AAddress) +} SarLongint(imm, 20) + 4;
                  case m of  // table 7-234 / BZ
                    0 : ACode := format(op_a1c2, ['beqz', s, imm]);
                    1 : ACode := format(op_a1c2, ['bnez', s, imm]);
                    2 : ACode := format(op_a1c2, ['bltz', s, imm]);
                    3 : ACode := format(op_a1c2, ['bgez', s, imm]);
                    otherwise
                      ACode := 'reserved';
                  end;
                end;
            2 : begin
                  imm := {int32(AAddress) +} SmallInt(ShortInt(c2))+4;
                  case m of  // table 7-235 / BI0
                    0 : ACode := format(op_a1c2c3, ['beqi', s, b4const_signed[r], imm]);
                    1 : ACode := format(op_a1c2c3, ['bnei', s, b4const_signed[r], imm]);
                    2 : ACode := format(op_a1c2c3, ['blti', s, b4const_signed[r], imm]);
                    3 : ACode := format(op_a1c2c3, ['bgei', s, b4const_signed[r], imm]);
                    otherwise
                      ACode := 'reserved';
                  end;
                end;
            3 : begin
                  case m of  // table 7-236 / BI1
                    0 : ACode := format(op_a1c2, ['entry', s, 8*((c2 shl 4) or r)]);
                    1 : case r of  // table 7-237 / B1
                          0 : ACode := format(op_b1c2, ['bf', s, SmallInt(ShortInt(c2))+4]);
                          1 : ACode := format(op_b1c2, ['bt', s, SmallInt(ShortInt(c2))+4]);
                          8 : ACode := format(op_a1c2, ['loop', s, word(c2)+4]);
                          9 : ACode := format(op_a1c2, ['loopnez', s, word(c2)+4]);
                          10: ACode := format(op_a1c2, ['loopgtz', s, word(c2)+4]);
                          otherwise
                            ACode := 'reserved';
                        end;
                    2 : ACode := format(op_a1c2c3, ['bltui', s, b4const_unsigned[r], word(c2)+4]);  // Check
                    3 : ACode := format(op_a1c2c3, ['bgeui', s, b4const_unsigned[r], SmallInt(ShortInt(c2))+4]);  // Check
                    otherwise
                      ACode := 'reserved';
                  end;
                end;
            otherwise
              ACode := 'reserved';
          end;
        end;
    7 : begin
          imm := {int32(AAddress) +} ShortInt(SmallInt(c2))+4;
          case r of  // table7-238 / B
            0 : ACode := format(op_a1a2c3, ['bnone', s, t, imm]);
            1 : ACode := format(op_a1a2c3, ['beq', s, t, imm]);
            2 : ACode := format(op_a1a2c3, ['blt', s, t, imm]);
            3 : ACode := format(op_a1a2c3, ['bltu', s, t, imm]);
            4 : ACode := format(op_a1a2c3, ['ball', s, t, imm]);
            5 : ACode := format(op_a1a2c3, ['bbc', s, t, imm]);
            6, 7: ACode := format(op_a1c2c3, ['bbci', s, t + (r and 1) shl 4, imm]);
            8 : ACode := format(op_a1a2c3, ['bany', s, t, imm]);
            9 : ACode := format(op_a1a2c3, ['bne', s, t, imm]);
            10: ACode := format(op_a1a2c3, ['bge', s, t, imm]);
            11: ACode := format(op_a1a2c3, ['bgeu', s, t, imm]);
            12: ACode := format(op_a1a2c3, ['bnall', s, t, imm]);
            13: ACode := format(op_a1a2c3, ['bbs', s, t, imm]);
            14,15: ACode := format(op_a1c2c3, ['bbsi', s, t + (r and 1) shl 4, imm]);
            otherwise
              ACode := 'reserved';
          end;
        end;
    8 : ACode := format(op_a1a2c3, ['l32i.n', s, t, 4*word(r)]);
    9 : ACode := format(op_a1a2c3, ['s32i.n', s, t, 4*word(r)]);
    10: ACode := format(op_a1a2a3, ['add.n', r, s, t]);
    11: if t = 0 then
          ACode := format(op_a1a2c3, ['addi.n', r, s, -1])
        else
          ACode := format(op_a1a2c3, ['addi.n', r, s, t]);
    12: case t of  // table 7-239 / ST2
          0..7:   begin
                    imm := int32(r) or int32(t and 7) shl 4;
                    // Check if imm represents an encoded negative value, sign extend to 32 bit
                    if (imm shr 5) = 3 then
                      imm := int32(int32(imm) or $FFFFFF80);
                    ACode := format(op_a1c2, ['movi.n', s, imm]);
                  end;
          8..11:  begin
                    imm := {int32(AAddress) +} word(r) or (word(t and 3) shl 4) + 4;
                    ACode := format(op_a1c2, ['beqz.n', s, imm]);
                  end;
          12..15: begin
                   imm := {int32(AAddress) +} word(r) or (word(t and 3) shl 4) + 4;
                   ACode := format(op_a1c2, ['bnez.n', s, imm]);
                 end;
          otherwise
            ACode := 'reserved';
        end;
    13: case r of  // table 7-240 / ST3
          0 : ACode := format(op_a1a2, ['mov.n', t, s]);
          15: case t of  // table 7-241
                0 : ACode := format(op, ['ret.n']);
                1 : ACode := format(op, ['retw.n']);
                2 : ACode := format(op_c1, ['break.n', s]);
                3 : ACode := format(op, ['nop.n']);
                6 : case s of  // table ILH
                      0 : ACode := format(op, ['ill.n']);
                      1 : ACode := format(op, ['halt.n']);
                      otherwise
                        ACode := 'reserved';
                    end;
                otherwise
                  ACode := 'reserved';
              end;
          otherwise
            ACode := 'reserved';
        end;
    otherwise
      ACode := 'reserved';
  end;

  // memory, display as normal (BE) hex value
  ACodeBytes := '';
  for m := 0 to CodeIdx - 1 do
    ACodeBytes := HexStr(pcode[m], 2) + ACodeBytes;

  Inc(AAddress, CodeIdx);
end;

function TXtensaAsmDecoder.GetInstructionInfo(AnAddress: TDBGPtr
  ): TDbgAsmInstruction;
begin
  if (FLastInstr = nil) or (FLastInstr.RefCount > 1) then begin
    ReleaseRefAndNil(FLastInstr);
    FLastInstr := TXtensaAsmInstruction.Create(Self);
  end;

  FLastInstr.SetAddress(AnAddress);
  Result := FLastInstr;
end;

function TXtensaAsmDecoder.GetFunctionFrameInfo(AnAddress: TDBGPtr; out
  AnIsOutsideFrame: Boolean): Boolean;
begin
  Result := False;
end;

function TXtensaAsmDecoder.GetBreakInstruction(const ALocation: TDBGPtr; out
  BreakInstructionLength: Integer): QWord;
begin
  BreakInstructionLength := GetInstructionInfo(ALocation).InstructionLength;

  // Set the user fields s and t to 0
  if BreakInstructionLength = 2 then
    Result := $000000 { ILL} // $F0D2 { BREAK }
  else
    Result := $F06D { ILL.N }; // 4000 { BREAK.N }
end;

function TXtensaAsmDecoder.BreakInstructionOffset: Int8;
begin
  Result := 0; // ?
end;

function TXtensaAsmDecoder.GetFunctionFrameReturnAddress(AnAddress, AStartPC,
  AEndPC: TDBGPtr; ACallType: TCallType; out returnAddressOffset: word; out
  AnIsOutsideFrame: Boolean): Boolean;
begin
  result := false;
  exit;
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

  // Return address always in a0
  returnAddressOffset := 0;

  if (AnAddress = AStartPC) or (AnAddress = AEndPC) then
  begin
    AnIsOutsideFrame := true;
    exit;
  end
  else
    result := FParsePrologue(AnAddress, AStartPC, AEndPC, ACallType, returnAddressOffset, AnIsOutsideFrame);
end;

constructor TXtensaAsmDecoder.Create(AProcess: TDbgProcess);
begin
  FProcess := AProcess;
end;

destructor TXtensaAsmDecoder.Destroy;
begin
  ReleaseRefAndNil(FLastInstr);
  inherited Destroy;
end;

initialization
  DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );

end.
