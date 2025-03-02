{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpdbgdisasriscv.pp  -  Native Freepascal debugger - RISC-V Disassembler
 ---------------------------------------------------------------------------

 This unit contains an RISC-V disassembler for the Native Freepascal debugger

 ---------------------------------------------------------------------------

 @created(Tue Jul 16th, 2024)
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
unit FpDbgDisasRiscv;
{$mode objfpc}{$H+}
interface

uses
  SysUtils,
  FpDbgUtil, FpDbgInfo, DbgIntfBaseTypes, FpdMemoryTools, {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif},
  FpDbgClasses;

const
  RiscvABIRegisterNames: array[0..32] of string = (
    'zero',
    'ra',
    'sp',
    'gp',
    'tp',
    't0',
    't1',
    't2',
    's0',
    's1',
    'a0',
    'a1',
    'a2',
    'a3',
    'a4',
    'a5',
    'a6',
    'a7',
    's2',
    's3',
    's4',
    's5',
    's6',
    's7',
    's8',
    's9',
    's10',
    's11',
    't3',
    't4',
    't5',
    't6',
    'pc'
  );

type
  //The function Disassemble decodes the instruction at the given address.

  TRiscvAsmDecoder = class;

  TRiscvOpCode = (
    A_INVALID, A_CUSTOM, A_RESERVED,
    A_LB, A_LH, A_LW, A_LD, A_LBU, A_LHU, A_LWU,  // Load instructions
    A_FLH, A_FLW, A_FLD, A_FLQ,  // Load floating point
    A_FENCE_TSO, A_PAUSE, A_FENCE, A_FENCE_I, // fence instructions
    A_CBO_INVAL, A_CBO_CLEAN, A_CBO_FLUSH, A_CBO_ZERO, // cache block operations
    A_ADDI, A_SLLI, A_SLTI, A_SLTIU, A_XORI, A_SRLI, A_SRAI, A_ORI, A_ANDI, // immediate operands
    A_AUIPC, A_LUI,
    A_ADDIW, A_SLLIW, A_CLZW, A_CTZW, A_SRAIW, A_SRLIW, A_RORIW, A_SLLI_UW, // immediate operands - 32 bits
    A_SB, A_SH, A_SW, A_SD,  // Store instructions
    A_FSH, A_FSW, A_FSD, A_FSQ,  // Store floating point
    A_LR, A_SC, A_AMOADD, A_AMOSWAP, A_AMOXOR, A_AMOAND, A_AMOOR, A_AMOMIN, A_AMOMAX, A_AMOMINU, A_AMOMAXU, // AMO
    // Operand quadrant
    A_ADD, A_SUB, A_SLL, A_SLT, A_SLTU, A_XOR, A_SRL, A_SRA, A_OR, A_AND, // Operand
    A_MUL, A_MULH, A_MULHSU, A_MULHU, A_DIV, A_DIVU, A_REM, A_REMU,
    A_CLMUL, A_BSET, A_BCLR, A_ROL, A_CLMULR, A_SH1ADD, A_CLMULH, A_PACK, A_SH2ADD, A_XNOR,
    A_BINV, A_MINU, A_SH3ADD, A_BEXT, A_ROR, A_MAX, A_ORN, A_PACKH, A_MAXU, A_ANDN,
    // 32 bit Operand quadrant
    A_ADDW, A_SUBW, A_SLLW, A_SRLW, A_SRAW, A_MULW, A_DIVW, A_DIVUW, A_REMW, A_REMUW,
    A_ADD_UW, A_ROLW, A_SH1ADD_UW, A_ZEXT_H, A_SH2ADD_UW, A_RORW, A_SH3ADD_UW,
    // MADD
    A_FMADD, A_FMSUB, A_FNMADD, A_FNMSUB,
    A_FADD, A_FSUB, A_FMUL, A_FDIV, A_FSGNJ, A_FSGNJN, A_FSGNJX, A_FSQRT, A_FCVT_D_S,
    A_FCVT_S_D, A_FMIN, A_FMAX, A_FLE, A_FLT, A_FEQ, A_FCVT_Int_S, A_FCVT_UInt_S,
    A_FCVT_S_Int, A_FCVT_S_UInt,
    A_BEQ, A_BNE, A_BLT, A_BGE, A_BLTU, A_BGEU,
    A_JALR, A_JAL,
    A_ECALL, A_EBREAK,    // unprivileged instructions
    A_SRET, A_MRET, A_WFI, // privileged instructions
    // 16 bit specific
    A_NOP, A_ILL,
    // Zicsr extension
    A_CSRRW, A_CSRRS, A_CSRRC, A_CSRRWI, A_CSRRSI, A_CSRRCI,
    // Pseudo instructions
    A_RET
  );

  TRiscvFormat = (
    rvfInstructionDestSrcOffset, // lw rd, imm(rs1)
    rvfLoadFDestSrcOffset,       // flw rd, imm(rs1)
    rvfStoreSrc2Src1Offset,      // sw rs2, imm(rs1)
    rvfStoreFSrc2Src1Offset,     // fsw rs2, imm(rs1)
    rvfBranchSrc1Src2Imm,        // beq rs1, rs2, imm
    rvfInstruction,              // ebreak
    rvfFence,                    // fence [iorw], [iorw]
    rvfCustom,                   // custom-imm
    rvfInstructionSrcOffset,     // cbo.clean (rs1)
    rvfInstructionDestSrcImm,    // addi rd, rs1, imm
    rvfInstructionDestSrc1Src2,  // sub rd, rs2, rs1
    rvfInstructionSrc1Src2Imm,   // beq rs1, rs2, imm
    rvfAmoDestSrc2Src1,          // sc.w rd, rs2, rs1
    rvfAmoDestSrc1,              // lr.w rd, rs1
    rvfAmoDestSrc2Src1_addr,     // amoswap.w rd, rs2, (rs1)
    rvfInstructionDestImm,       // auipc rd, imm
    rvfInstructionDestSrc,       // clzw rd, rs1
    rvfFloatInstrDestSrc1,       // fsqrt.s rd, rs1
    rvfFloatInstrDestSrc1Src2,   // fsub.s rd, rs1, rs2 => rd = rs1 - rs2
    rvfFloatInstrDestSrc1Src2Src3, // fmadd.s rd, rs1, rs2, rs3 => (rs1 x rs2) + rs3
    rvfCSRDestCsrSrc1,           // csrrw rd, #csr, rs1
    rvfCSRDestCsrImm             // csrrw rd, #csr, imm
  );

  TAmoInfo = record
    aquire: boolean;
    release: boolean;
    width: byte; // 2=h, 3=d
  end;

  TRiscvInstruction = record
    OpCode: TRiscvOpCode;
    Size: integer;
    imm: int32;
    rd, rs1, rs2: byte;
    format: TRiscvFormat;
    case integer of
      0: (AmoInfo: TAmoInfo);
      1: (rs3, fpwidth: byte);
      2: (csr: int32);
  end;

  { TRiscvDisassembler }

  TRiscvDisassembler = object
  private type
    // Different encoding formats used for some immediate values
    TImmEncoding = (ieImm5376, ieImm54876, ieImm5326);
    TImmEncoding2 = (ie2Imm5386, ie2Imm5496, ie2Imm5276);
  private
    procedure decodeR(instr: uint32; out funct7, rs2, rs1, funct3, rd: byte);
    procedure decodeI(instr: uint32; out imm: int32; out rs1, funct3, rd: byte);
    procedure decodeS(instr: uint32; out imm: int32; out rs2, rs1, funct3: byte);
    procedure decodeB(instr: uint32; out imm: int32; out rs2, rs1, funct3: byte);
    procedure decodeU(instr: uint32; out imm: int32; out rd: byte);
    procedure decodeJ(instr: uint32; out imm: int32; out rd: byte);
    function decodeLoad(instr: uint32): TRiscvInstruction;
    function decodeLoadF(instr: uint32): TRiscvInstruction;
    function customMap(i: integer; instr: uint32): TRiscvInstruction;
    procedure decodeFence(imm: uint32; out fm, pred, succ: byte);
    function decodeMiscMem(instr: uint32): TRiscvInstruction;
    function decodeOpIm(instr: uint32): TRiscvInstruction;
    function decodeAUIPC_LUI(instr: uint32; opcode: byte): TRiscvInstruction;
    function decodeOpIm32(instr: uint32): TRiscvInstruction;
    function decodeStore(instr: uint32): TRiscvInstruction;
    function decodeStoreF(instr: uint32): TRiscvInstruction;
    function decodeAMO(instr: uint32): TRiscvInstruction;
    function decodeOp(instr: uint32): TRiscvInstruction;
    function decodeOp32(instr: uint32): TRiscvInstruction;
    procedure decodeR_fmt(instr: uint32; out rs3, fmt, rs2, rs1, roundingMode, rd: byte);
    function decodeMAdd(instr: uint32): TRiscvInstruction;
    function decodeMSub(instr: uint32): TRiscvInstruction;
    function decodeNMSub(instr: uint32): TRiscvInstruction;
    function decodeNMAdd(instr: uint32): TRiscvInstruction;
    function decodeOP_FP(instr: uint32): TRiscvInstruction;
    function decodeBranch(instr: uint32): TRiscvInstruction;
    function decodeJALR(instr: uint32): TRiscvInstruction;
    function decodeJAL(instr: uint32): TRiscvInstruction;
    function decodeSystem(instr: uint32): TRiscvInstruction;
    function formatOpcode(instr: TRiscvInstruction): string;
    procedure decodeCIW(instr: uint16; out imm: uint16; out rd_: byte);
    procedure decodeCLS(instr: uint16; out imm: uint16; out rs1_, rd_rs2_: byte; const immType: TImmEncoding);
    procedure decodeCI(instr: uint16; out rs1_rd: byte; out imm: byte);
    procedure decodeCJ(instr: uint16; out imm: int16);
    procedure decodeCB(instr: uint16; out rs1_: byte; out imm: int16);
    procedure decodeCSS(instr: uint16; out uimm: uint16; out rs2: byte; const immtype: TImmEncoding2);
    function decodeAddi4spn(instr: uint16):TRiscvInstruction;
    function decodeFLD(instr: uint16):TRiscvInstruction;
    function decodeLW(instr: uint16):TRiscvInstruction;
    function decodeFLW(instr: uint16):TRiscvInstruction;
    function decodeFSD(instr: uint16):TRiscvInstruction;
    function decodeSW(instr: uint16):TRiscvInstruction;
    function decodeFSW(instr: uint16):TRiscvInstruction;
    function decodeAddI(instr: uint16): TRiscvInstruction;
    function decodeCJAL(instr: uint16): TRiscvInstruction;
    function decodeLI(instr: uint16): TRiscvInstruction;
    function decodeLUI(instr: uint16): TRiscvInstruction;
    function decodeALU(instr: uint16):TRiscvInstruction;
    function decodeCJ(instr: uint16): TRiscvInstruction;
    function decodeCBNEQZ(instr: uint16; funct3: byte): TRiscvInstruction;
    function decodeSLLI(instr: uint16): TRiscvInstruction;
    function decodeFLDSP(instr: uint16): TRiscvInstruction;
    function decodeLWSP(instr: uint16): TRiscvInstruction;
    function decodeFLWSP(instr: uint16): TRiscvInstruction;
    function decodeJR(instr: uint16): TRiscvInstruction;
    function decodeCStore(instr: uint16; funct3: byte): TRiscvInstruction;
  public
    procedure Disassemble(var AAddress: Pointer; out AnInstruction: TRiscvInstruction);
  end;


  { TRiscvAsmInstruction }

  TRiscvAsmInstruction = class(TDbgAsmInstruction)
  private const
    INSTR_CODEBIN_LEN = 4;
  private
    FAsmDecoder: TRiscvAsmDecoder;
    FAddress: TDBGPtr;
    FCodeBin: array[0..INSTR_CODEBIN_LEN-1] of byte;
    FFlags: set of (diCodeRead, diCodeReadError);
  protected
    procedure ReadCode; inline;
  public
    constructor Create(AAsmDecoder: TRiscvAsmDecoder);
    procedure SetAddress(AnAddress: TDBGPtr);
    function IsCallInstruction: boolean; override;
    function IsReturnInstruction: boolean; override;
    function IsLeaveStackFrame: boolean; override;
    function InstructionLength: Integer; override;
  end;

{ TRiscvAsmDecoder }

  TRiscvAsmDecoder = class(TDbgAsmDecoder)
  private const
    MaxPrologueSize = 63;  // Bytes, so ~22 instructions
    MaxEpilogueSize = MaxPrologueSize; // Perhaps a bit smaller, since the locals/parameters do not have to be initialized
    MAX_CODEBIN_LEN = MaxPrologueSize; // About 22 instructions
  private
    FProcess: TDbgProcess;
    FLastErrWasMem: Boolean;
    FCodeBin: array[0..MAX_CODEBIN_LEN-1] of byte;
    FLastInstr: TRiscvAsmInstruction;
    function FParsePrologue(AnAddress, AStartPC, AEndPC: TDBGPtr; out
      returnAddressOffset, SPoffset: integer; out AnIsOutsideFrame: Boolean): Boolean;
    function FParseEpilogue(AnAddress, AStartPC, AEndPC: TDBGPtr; out
      returnAddressOffset: word; out AnIsOutsideFrame: Boolean): Boolean;

    function FormatInstruction(instr: TRiscvInstruction): string;
  protected
    function GetLastErrorWasMemReadErr: Boolean; override;
    function GetMaxInstrSize: integer; override;
    function GetMinInstrSize: integer; override;
    function GetCanReverseDisassemble: boolean; override;
    function ReadCodeAt(AnAddress: TDBGPtr; ALen: Cardinal): Boolean; inline;
  public
    procedure Disassemble(var AAddress: Pointer; out ACodeBytes: String; out ACode: String); override; overload;
    procedure Disassemble(var AAddress: Pointer; out ACodeBytes: String; out ACode: String; out AnInfo: TDbgInstInfo); override; overload;
    function GetInstructionInfo(AnAddress: TDBGPtr): TDbgAsmInstruction; override;

    // Rather use GetFunctionFrameReturnAddress
    function GetFunctionFrameInfo(AnAddress: TDBGPtr; out
      AnIsOutsideFrame: Boolean): Boolean; override;

    function GetBreakInstruction(const ALocation: TDBGPtr; out
       BreakInstructionLength: Integer): QWord;
     function BreakInstructionOffset: Int8;

     // AStartPC & AEndPC indicates proc limits to help with scanning for prologue/epilogue
    function GetFunctionFrameReturnAddress(AnAddress, AStartPC, AEndPC: TDBGPtr;
      out returnAddressOffset, SPoffset: integer; out AnIsOutsideFrame: Boolean): Boolean;

    constructor Create(AProcess: TDbgProcess); override;
    destructor Destroy;
  end;

  { TDbgStackUnwinderRiscv }

  TDbgStackUnwinderRiscv = class(TDbgStackUnwinder)
  private
    FThread: TDbgThread;
    FProcess: TDbgProcess;
    FAddressSize: Integer;
    FLastFrameBaseIncreased: Boolean;
    FSPchangeCount: integer;
    FPreviousPC: TDBGPtr;
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
  FpDbgRiscvClasses;

var
  DBG_WARNINGS: PLazLoggerLogGroup;

const
  RiscvOpcodeString: array[TRiscvOpCode] of string = (
    'invalid', 'custom', 'reserved',
    'lb', 'lh', 'lw', 'ld', 'lbu', 'lhu', 'lwu',
    'flh', 'flw', 'fld', 'flq',
    'fence.tso', 'pause', 'fence', 'fence.i',
    'cbo.inval', 'cbo.clean', 'cbo.flush', 'cbo.zero',
    'addi', 'slli', 'slti', 'sltiu', 'xori', 'srli', 'srai', 'ori', 'andi',
    'auipc', 'lui',
    'addiw', 'slliw', 'clzw', 'ctzw', 'sraiw', 'srliw', 'roriw', 'slliw',
    'sb', 'sh', 'sw', 'sd',
    'fsh', 'fsw', 'fsd', 'fsq',
    'lr', 'sc', 'amoadd', 'amoswap', 'amoxor', 'amoand', 'amoor', 'amomin', 'amomax', 'amominu', 'amomaxu',
    // Operand quadrant
    'add', 'sub', 'sll', 'slt', 'sltu', 'xor', 'srl', 'sra', 'or', 'and',
    'mul', 'mulh', 'mulhsu', 'mulhu', 'div', 'divu', 'rem', 'remu',
    'clmul', 'bset', 'bclr', 'rol', 'clmulr', 'sh1add', 'clmulh', 'pack', 'sh2add', 'xnor',
    'binv', 'minu', 'sh3add', 'bext', 'ror', 'max', 'orn', 'packh', 'maxu', 'andn',
    // 32 bit Operand quadrant
    'addw', 'subw', 'sllw', 'srlw', 'sraw', 'mulw', 'divw', 'divuw', 'remw', 'remuw',
    'add.uw', 'rolw', 'sh1add.uw', 'zext.h', 'sh2add.uw', 'rorw', 'sh3add.uw',
    // MADD
    'fmadd', 'fmsub', 'fnmadd', 'fnmsub',
    'fadd', 'fsub', 'fmul', 'fdiv', 'fsgnj', 'fsgnjn', 'fsgnnjx', 'fsqrt', 'fcvt.d.s',
    'fcvt.s.d', 'fmin', 'fmax', 'fle', 'flt', 'feq', 'fcvt.%s.s' {A_FCVT_Int_S}, 'fcvt.%s.s' {A_FCVT_UInt_S},
    'fcvt.s.%s' {A_FCVT_S_Int}, 'fcvt.s.%s' {A_FCVT_S_UInt},
    'beq', 'bne', 'blt', 'bge', 'bltu', 'bgeu',
    'jalr', 'jal',
    'ecall', 'ebreak',     // unprivileged instructions
    'sret', 'mret', 'wfi', // privileged instructions

    // 16 bit specific
    'nop', 'ill',
    // Zicsr extension
    'csrrw', 'csrrs', 'csrrc', 'csrrwi', 'csrrsi', 'csrrci',

    // Pseudo instructions
    'ret'
  );

function CsrRegNumToName(regnum: integer): string;
begin
  case regnum of
    $001: Result := 'fflags';  // Floating-Point Accrued Exceptions
    $002: Result := 'frm';  // Floating-Point Dynamic Rounding Mode
    $003: Result := 'fcsr';  // Floating-Point Control and Status Register (frm + fflags)
    $017: Result := 'jvt';  // Table jump base vector and control register
    $300: Result := 'mstatus';  // Machine Status (lower 32 bits).
    $301: Result := 'misa';  // Machine ISA
    $304: Result := 'mie';  // Machine Interrupt Enable Register
    $305: Result := 'mtvec';  // Machine Trap-Handler Base Address
    $307: Result := 'mtvt';  // Machine Trap-Handler Vector Table Base Address
    $310: Result := 'mstatush';  // Machine Status (upper 32 bits).
    $320: Result := 'mcountinhibit';  // (HPM) Machine Counter-Inhibit Register
    $323: Result := 'mhpmevent3';  // (HPM) Machine Performance-Monitoring Event Selector 3
    $324: Result := 'mhpmevent4';  // (HPM) Machine Performance-Monitoring Event Selector 4
    $325: Result := 'mhpmevent5';  // (HPM) Machine Performance-Monitoring Event Selector 5
    $326: Result := 'mhpmevent6';  // (HPM) Machine Performance-Monitoring Event Selector 6
    $327: Result := 'mhpmevent7';  // (HPM) Machine Performance-Monitoring Event Selector 7
    $328: Result := 'mhpmevent8';  // (HPM) Machine Performance-Monitoring Event Selector 8
    $329: Result := 'mhpmevent9';  // (HPM) Machine Performance-Monitoring Event Selector 9
    $32A: Result := 'mhpmevent10';  // (HPM) Machine Performance-Monitoring Event Selector 10
    $32B: Result := 'mhpmevent11';  // (HPM) Machine Performance-Monitoring Event Selector 11
    $32C: Result := 'mhpmevent12';  // (HPM) Machine Performance-Monitoring Event Selector 12
    $32D: Result := 'mhpmevent13';  // (HPM) Machine Performance-Monitoring Event Selector 13
    $32E: Result := 'mhpmevent14';  // (HPM) Machine Performance-Monitoring Event Selector 14
    $32F: Result := 'mhpmevent15';  // (HPM) Machine Performance-Monitoring Event Selector 15
    $330: Result := 'mhpmevent16';  // (HPM) Machine Performance-Monitoring Event Selector 16
    $331: Result := 'mhpmevent17';  // (HPM) Machine Performance-Monitoring Event Selector 17
    $332: Result := 'mhpmevent18';  // (HPM) Machine Performance-Monitoring Event Selector 18
    $333: Result := 'mhpmevent19';  // (HPM) Machine Performance-Monitoring Event Selector 19
    $334: Result := 'mhpmevent20';  // (HPM) Machine Performance-Monitoring Event Selector 20
    $335: Result := 'mhpmevent21';  // (HPM) Machine Performance-Monitoring Event Selector 21
    $336: Result := 'mhpmevent22';  // (HPM) Machine Performance-Monitoring Event Selector 22
    $337: Result := 'mhpmevent23';  // (HPM) Machine Performance-Monitoring Event Selector 23
    $338: Result := 'mhpmevent24';  // (HPM) Machine Performance-Monitoring Event Selector 24
    $339: Result := 'mhpmevent25';  // (HPM) Machine Performance-Monitoring Event Selector 25
    $33A: Result := 'mhpmevent26';  // (HPM) Machine Performance-Monitoring Event Selector 26
    $33B: Result := 'mhpmevent27';  // (HPM) Machine Performance-Monitoring Event Selector 27
    $33C: Result := 'mhpmevent28';  // (HPM) Machine Performance-Monitoring Event Selector 28
    $33D: Result := 'mhpmevent29';  // (HPM) Machine Performance-Monitoring Event Selector 29
    $33E: Result := 'mhpmevent30';  // (HPM) Machine Performance-Monitoring Event Selector 30
    $33F: Result := 'mhpmevent31';  // (HPM) Machine Performance-Monitoring Event Selector 31
    $340: Result := 'mscratch';  // Machine Scratch
    $341: Result := 'mepc';  // Machine Exception Program Counter
    $342: Result := 'mcause';  // Machine Trap Cause
    $343: Result := 'mtval';  // Machine Trap Value
    $344: Result := 'mip';  // Machine Interrupt Pending Register
    $345: Result := 'mnxti';  // Interrupt handler address and enable modifier
    $347: Result := 'mintthresh';  // Interrupt-level threshold
    $349: Result := 'mscratchcswl';  // Conditional scratch swap on level change
    $7A0: Result := 'tselect';  // Trigger Select Register
    $7A1: Result := 'tdata1';  // Trigger Data Register 1
    $7A2: Result := 'tdata2';  // Trigger Data Register 2
    $7A4: Result := 'tinfo';  // Trigger Info
    $7B0: Result := 'dcsr';  // Debug Control and Status
    $7B1: Result := 'dpc';  // Debug PC
    $7B2: Result := 'dscratch0';  // Debug Scratch Register 0
    $7B3: Result := 'dscratch1';  // Debug Scratch Register 1
    $B00: Result := 'mcycle';  // (HPM) Machine Cycle Counter
    $B02: Result := 'minstret';  // (HPM) Machine Instructions-Retired Counter
    $B03: Result := 'mhpmcounter3';  // (HPM) Machine Performance-Monitoring Counter 3
    $B04: Result := 'mhpmcounter4';  // (HPM) Machine Performance-Monitoring Counter 4
    $B05: Result := 'mhpmcounter5';  // (HPM) Machine Performance-Monitoring Counter 5
    $B06: Result := 'mhpmcounter6';  // (HPM) Machine Performance-Monitoring Counter 6
    $B07: Result := 'mhpmcounter7';  // (HPM) Machine Performance-Monitoring Counter 7
    $B08: Result := 'mhpmcounter8';  // (HPM) Machine Performance-Monitoring Counter 8
    $B09: Result := 'mhpmcounter9';  // (HPM) Machine Performance-Monitoring Counter 9
    $B0A: Result := 'mhpmcounter10';  // (HPM) Machine Performance-Monitoring Counter 10
    $B0B: Result := 'mhpmcounter11';  // (HPM) Machine Performance-Monitoring Counter 11
    $B0C: Result := 'mhpmcounter12';  // (HPM) Machine Performance-Monitoring Counter 12
    $B0D: Result := 'mhpmcounter13';  // (HPM) Machine Performance-Monitoring Counter 13
    $B0E: Result := 'mhpmcounter14';  // (HPM) Machine Performance-Monitoring Counter 14
    $B0F: Result := 'mhpmcounter15';  // (HPM) Machine Performance-Monitoring Counter 15
    $B10: Result := 'mhpmcounter16';  // (HPM) Machine Performance-Monitoring Counter 16
    $B11: Result := 'mhpmcounter17';  // (HPM) Machine Performance-Monitoring Counter 17
    $B12: Result := 'mhpmcounter18';  // (HPM) Machine Performance-Monitoring Counter 18
    $B13: Result := 'mhpmcounter19';  // (HPM) Machine Performance-Monitoring Counter 19
    $B14: Result := 'mhpmcounter20';  // (HPM) Machine Performance-Monitoring Counter 20
    $B15: Result := 'mhpmcounter21';  // (HPM) Machine Performance-Monitoring Counter 21
    $B16: Result := 'mhpmcounter22';  // (HPM) Machine Performance-Monitoring Counter 22
    $B17: Result := 'mhpmcounter23';  // (HPM) Machine Performance-Monitoring Counter 23
    $B18: Result := 'mhpmcounter24';  // (HPM) Machine Performance-Monitoring Counter 24
    $B19: Result := 'mhpmcounter25';  // (HPM) Machine Performance-Monitoring Counter 25
    $B1A: Result := 'mhpmcounter26';  // (HPM) Machine Performance-Monitoring Counter 26
    $B1B: Result := 'mhpmcounter27';  // (HPM) Machine Performance-Monitoring Counter 27
    $B1C: Result := 'mhpmcounter28';  // (HPM) Machine Performance-Monitoring Counter 28
    $B1D: Result := 'mhpmcounter29';  // (HPM) Machine Performance-Monitoring Counter 29
    $B1E: Result := 'mhpmcounter30';  // (HPM) Machine Performance-Monitoring Counter 30
    $B1F: Result := 'mhpmcounter31';  // (HPM) Machine Performance-Monitoring Counter 31
    $B80: Result := 'mcycleh';  // (HPM) Upper 32 Machine Cycle Counter
    $B82: Result := 'minstreth';  // (HPM) Upper 32 Machine Instructions-Retired Counter
    $B83: Result := 'mhpmcounterh3';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 3
    $B84: Result := 'mhpmcounterh4';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 4
    $B85: Result := 'mhpmcounterh5';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 5
    $B86: Result := 'mhpmcounterh6';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 6
    $B87: Result := 'mhpmcounterh7';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 7
    $B88: Result := 'mhpmcounterh8';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 8
    $B89: Result := 'mhpmcounterh9';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 9
    $B8A: Result := 'mhpmcounterh10';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 10
    $B8B: Result := 'mhpmcounterh11';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 11
    $B8C: Result := 'mhpmcounterh12';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 12
    $B8D: Result := 'mhpmcounterh13';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 13
    $B8E: Result := 'mhpmcounterh14';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 14
    $B8F: Result := 'mhpmcounterh15';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 15
    $B90: Result := 'mhpmcounterh16';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 16
    $B91: Result := 'mhpmcounterh17';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 17
    $B92: Result := 'mhpmcounterh18';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 18
    $B93: Result := 'mhpmcounterh19';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 19
    $B94: Result := 'mhpmcounterh20';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 20
    $B95: Result := 'mhpmcounterh21';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 21
    $B96: Result := 'mhpmcounterh22';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 22
    $B97: Result := 'mhpmcounterh23';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 23
    $B98: Result := 'mhpmcounterh24';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 24
    $B99: Result := 'mhpmcounterh25';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 25
    $B9A: Result := 'mhpmcounterh26';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 26
    $B9B: Result := 'mhpmcounterh27';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 27
    $B9C: Result := 'mhpmcounterh28';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 28
    $B9D: Result := 'mhpmcounterh29';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 29
    $B9E: Result := 'mhpmcounterh30';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 30
    $B9F: Result := 'mhpmcounterh31';  // (HPM) Upper 32 Machine Performance-Monitoring Counter 31
    $C00: Result := 'cycle';  // Cycle Counter
    $C01: Result := 'time';  // Time
    $C02: Result := 'instret';  // Instructions-Retired Counter
    $C03: Result := 'hpmcounter3';  // (HPM) Performance-Monitoring Counter 3
    $C04: Result := 'hpmcounter4';  // (HPM) Performance-Monitoring Counter 4
    $C05: Result := 'hpmcounter5';  // (HPM) Performance-Monitoring Counter 5
    $C06: Result := 'hpmcounter6';  // (HPM) Performance-Monitoring Counter 6
    $C07: Result := 'hpmcounter7';  // (HPM) Performance-Monitoring Counter 7
    $C08: Result := 'hpmcounter8';  // (HPM) Performance-Monitoring Counter 8
    $C09: Result := 'hpmcounter9';  // (HPM) Performance-Monitoring Counter 9
    $C0A: Result := 'hpmcounter10';  // (HPM) Performance-Monitoring Counter 10
    $C0B: Result := 'hpmcounter11';  // (HPM) Performance-Monitoring Counter 11
    $C0C: Result := 'hpmcounter12';  // (HPM) Performance-Monitoring Counter 12
    $C0D: Result := 'hpmcounter13';  // (HPM) Performance-Monitoring Counter 13
    $C0E: Result := 'hpmcounter14';  // (HPM) Performance-Monitoring Counter 14
    $C0F: Result := 'hpmcounter15';  // (HPM) Performance-Monitoring Counter 15
    $C10: Result := 'hpmcounter16';  // (HPM) Performance-Monitoring Counter 16
    $C11: Result := 'hpmcounter17';  // (HPM) Performance-Monitoring Counter 17
    $C12: Result := 'hpmcounter18';  // (HPM) Performance-Monitoring Counter 18
    $C13: Result := 'hpmcounter19';  // (HPM) Performance-Monitoring Counter 19
    $C14: Result := 'hpmcounter20';  // (HPM) Performance-Monitoring Counter 20
    $C15: Result := 'hpmcounter21';  // (HPM) Performance-Monitoring Counter 21
    $C16: Result := 'hpmcounter22';  // (HPM) Performance-Monitoring Counter 22
    $C17: Result := 'hpmcounter23';  // (HPM) Performance-Monitoring Counter 23
    $C18: Result := 'hpmcounter24';  // (HPM) Performance-Monitoring Counter 24
    $C19: Result := 'hpmcounter25';  // (HPM) Performance-Monitoring Counter 25
    $C1A: Result := 'hpmcounter26';  // (HPM) Performance-Monitoring Counter 26
    $C1B: Result := 'hpmcounter27';  // (HPM) Performance-Monitoring Counter 27
    $C1C: Result := 'hpmcounter28';  // (HPM) Performance-Monitoring Counter 28
    $C1D: Result := 'hpmcounter29';  // (HPM) Performance-Monitoring Counter 29
    $C1E: Result := 'hpmcounter30';  // (HPM) Performance-Monitoring Counter 30
    $C1F: Result := 'hpmcounter31';  // (HPM) Performance-Monitoring Counter 31
    $C80: Result := 'cycleh';  // Upper 32 Cycle Counter
    $C81: Result := 'timeh';  // Upper 32 Time
    $C82: Result := 'instreth';  // Upper 32 Instructions-Retired Counter
    $C83: Result := 'hpmcounter3';  // (HPM) Upper 32 Performance-Monitoring Counter 3
    $C84: Result := 'hpmcounter4';  // (HPM) Upper 32 Performance-Monitoring Counter 4
    $C85: Result := 'hpmcounter5';  // (HPM) Upper 32 Performance-Monitoring Counter 5
    $C86: Result := 'hpmcounter6';  // (HPM) Upper 32 Performance-Monitoring Counter 6
    $C87: Result := 'hpmcounter7';  // (HPM) Upper 32 Performance-Monitoring Counter 7
    $C88: Result := 'hpmcounter8';  // (HPM) Upper 32 Performance-Monitoring Counter 8
    $C89: Result := 'hpmcounter9';  // (HPM) Upper 32 Performance-Monitoring Counter 9
    $C8A: Result := 'hpmcounter10';  // (HPM) Upper 32 Performance-Monitoring Counter 10
    $C8B: Result := 'hpmcounter11';  // (HPM) Upper 32 Performance-Monitoring Counter 11
    $C8C: Result := 'hpmcounter12';  // (HPM) Upper 32 Performance-Monitoring Counter 12
    $C8D: Result := 'hpmcounter13';  // (HPM) Upper 32 Performance-Monitoring Counter 13
    $C8E: Result := 'hpmcounter14';  // (HPM) Upper 32 Performance-Monitoring Counter 14
    $C8F: Result := 'hpmcounter15';  // (HPM) Upper 32 Performance-Monitoring Counter 15
    $C90: Result := 'hpmcounter16';  // (HPM) Upper 32 Performance-Monitoring Counter 16
    $C91: Result := 'hpmcounter17';  // (HPM) Upper 32 Performance-Monitoring Counter 17
    $C92: Result := 'hpmcounter18';  // (HPM) Upper 32 Performance-Monitoring Counter 18
    $C93: Result := 'hpmcounter19';  // (HPM) Upper 32 Performance-Monitoring Counter 19
    $C94: Result := 'hpmcounter20';  // (HPM) Upper 32 Performance-Monitoring Counter 20
    $C95: Result := 'hpmcounter21';  // (HPM) Upper 32 Performance-Monitoring Counter 21
    $C96: Result := 'hpmcounter22';  // (HPM) Upper 32 Performance-Monitoring Counter 22
    $C97: Result := 'hpmcounter23';  // (HPM) Upper 32 Performance-Monitoring Counter 23
    $C98: Result := 'hpmcounter24';  // (HPM) Upper 32 Performance-Monitoring Counter 24
    $C99: Result := 'hpmcounter25';  // (HPM) Upper 32 Performance-Monitoring Counter 25
    $C9A: Result := 'hpmcounter26';  // (HPM) Upper 32 Performance-Monitoring Counter 26
    $C9B: Result := 'hpmcounter27';  // (HPM) Upper 32 Performance-Monitoring Counter 27
    $C9C: Result := 'hpmcounter28';  // (HPM) Upper 32 Performance-Monitoring Counter 28
    $C9D: Result := 'hpmcounter29';  // (HPM) Upper 32 Performance-Monitoring Counter 29
    $C9E: Result := 'hpmcounter30';  // (HPM) Upper 32 Performance-Monitoring Counter 30
    $C9F: Result := 'hpmcounter31';  // (HPM) Upper 32 Performance-Monitoring Counter 31
    $F11: Result := 'mvendorid';  // Machine Vendor ID
    $F12: Result := 'marchid';  // Machine Architecture ID
    $F13: Result := 'mimpid';  // Machine Implementation ID
    $F14: Result := 'mhartid';  // Hardware Thread ID
    $F15: Result := 'mconfigptr';  // Machine Configuration Pointer
    $FB1: Result := 'mintstatus';  // Current interrupt levels
  else
    Result := '$'+HexStr(regnum, 3);
  end;
end;

{ TRiscvAsmInstruction }

procedure TRiscvAsmInstruction.ReadCode;
begin
  if not (diCodeRead in FFlags) then begin
    if not FAsmDecoder.FProcess.ReadData(FAddress, INSTR_CODEBIN_LEN, FCodeBin) then
      Include(FFlags, diCodeReadError);
    Include(FFlags, diCodeRead);
  end;
end;

constructor TRiscvAsmInstruction.Create(AAsmDecoder: TRiscvAsmDecoder);
begin
  FAsmDecoder := AAsmDecoder;
  inherited Create;
  AddReference;
end;

procedure TRiscvAsmInstruction.SetAddress(AnAddress: TDBGPtr);
begin
  FAddress := AnAddress;
  FFlags := [];
end;

function TRiscvAsmInstruction.IsCallInstruction: boolean;
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

function TRiscvAsmInstruction.IsReturnInstruction: boolean;
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

function TRiscvAsmInstruction.IsLeaveStackFrame: boolean;
begin
  Result := false;
end;

function TRiscvAsmInstruction.InstructionLength: Integer;
begin
  Result := 3;
  ReadCode;
  // op0 in 8..13
  if lo(FCodeBin[0]) in [8..13] then // narrow instruction series
    Result := 2;
end;

type
  TPrologueState = (
    psStart,               // Start of state machine
    psStackCreated,        // First time call to addi sp,sp, -const1
    psSavedReturnAddress,  // Call to sw ra,const(sp).  Return address is stored at: current sp + const1
    psAdjustedSP,          // Second adjustemnt of SP (addi sp,sp,-const2). Return address is located at sp + const1 + const2
    psBody);               // Start of code body, end of prologue

function TRiscvAsmDecoder.FParsePrologue(AnAddress, AStartPC, AEndPC: TDBGPtr;
  out returnAddressOffset, SPoffset: integer; out
  AnIsOutsideFrame: Boolean): Boolean;
var
  decoder: TRiscvDisassembler;
  ADataLen: Cardinal;
  AData: PByte;
  stackState: TPrologueState;
  instr: TRiscvInstruction;
begin
{ RV32 function entry example

40380c9a <esp_timer_impl_set_alarm_id>:
40380c9a:	1141                	addi	sp,sp,-16   // Make space for saved registers
40380c9c:	c606                	sw	ra,12(sp)   // Save return address
40380c9e:	c422                	sw	s0,8(sp)    // Callee saved register (possibly frame pointer)
40380ca0:	c226                	sw	s1,4(sp)    // Callee saved register
40380ca2:	c04a                	sw	s2,0(sp)    // Callee saved register
40380ca4:	4785                	addi	a5, zero, 1 // start of body

FPC also adjusts SP after storing parameters:
42000088 <main>:
42000088:	ff410113          	addi	sp,sp,-12   // Make space for saved registers
4200008c:	00112423          	sw	ra,8(sp)    // Save return address
42000090:	00812223          	sw	s0,4(sp)    // Callee saved register (in this case used as frame pointer)
42000094:	00912023          	sw	s1,0(sp)    // Callee saved register
42000098:	00c10413          	addi	s0,sp,12    // Adjust frame pointer
4200009c:	fcc10113          	addi	sp,sp,-52   // Extend stack frame to 64 bytes total, not clear why?
420000a0:	00000097          	auipc	ra,0x0      // start of body
420000a4:	0c4080e7          	jalr	196(ra) # 42000164 <FPC_INIT_FUNC_TABLE>
}
  // Also read next instruction, for simple procs/interrupts it may not be easy to spot the end of prologue
  // so the next instruction could tie break
  ADataLen := Min(MaxPrologueSize, AnAddress - AStartPC);
  Result := ReadCodeAt(AStartPC, ADataLen);
  if not Result then
    exit;

  AData := @FCodeBin[0];
  returnAddressOffset := 0;
  SPoffset := 0;
  AnIsOutsideFrame := true;
  stackState := psStart;

  // Loop until prologue buffer is empty, or stepped inside stack frame
  while (ADataLen > 1) and AnIsOutsideFrame do
  begin
    decoder.Disassemble(AData, instr);
    inc(AData, instr.Size);
    dec(ADataLen, instr.Size);

    case instr.OpCode of
      // addi rd, rs1, imm
      A_ADDI:
      begin
        if (instr.rs1 = SPindex) then
        begin
          if (instr.rd = SPindex) then
          begin
            inc(SPoffset, -instr.imm);
            if instr.imm < 0 then
            begin
              if stackState = psStart then
              begin
                stackState := psStackCreated;
              end
              else if (stackState = psStackCreated) then
                inc(returnAddressOffset, -instr.imm)
              else
                AnIsOutsideFrame := false; // Or error
            end;
          end;
        end
        else
          AnIsOutsideFrame := false;
      end;

      // sw rs2, offset(rs1)
      A_SW:
      begin
        if instr.rs1 = SPindex then
        begin
          if instr.rs2 = ReturnAddressIndex then
            inc(returnAddressOffset, instr.imm);
        end
        else
          AnIsOutsideFrame := false;
      end;
    else
      AnIsOutsideFrame := false;
    end;
  end;
end;

function TRiscvAsmDecoder.FParseEpilogue(AnAddress, AStartPC, AEndPC: TDBGPtr;
  out returnAddressOffset: word; out AnIsOutsideFrame: Boolean): Boolean;
begin
end;

function TRiscvAsmDecoder.FormatInstruction(instr: TRiscvInstruction): string;
var
  s1, s2: string;
begin
  Result := RiscvOpcodeString[instr.OpCode];
  if instr.OpCode = A_INVALID then exit;

  case instr.format of
    rvfInstructionDestSrcOffset: // lw rd, imm(rs1)
      Result := format('%-8s %s, %d(%s)', [Result, RiscvABIRegisterNames[instr.rd], instr.imm, RiscvABIRegisterNames[instr.rs1]]);

    rvfLoadFDestSrcOffset:      // flw rd, imm(rs1)
      Result := format('%-8s ft%d, %d(%s)', [Result, instr.rd, instr.imm, RiscvABIRegisterNames[instr.rs1]]);

    rvfStoreSrc2Src1Offset:     // sw rs2, imm(rs1)
      Result := format('%-8s %s, %d(%s)', [Result, RiscvABIRegisterNames[instr.rs2], instr.imm, RiscvABIRegisterNames[instr.rs1]]);

    rvfStoreFSrc2Src1Offset:    // fsw rs2, imm(rs1)
      Result := format('%-8s ft%d, %d(%s)', [Result, instr.rs2, instr.imm, RiscvABIRegisterNames[instr.rs1]]);

    rvfBranchSrc1Src2Imm:       // beq rs1, rs2, imm
      Result := format('%-8s %s, %s, %d', [Result, RiscvABIRegisterNames[instr.rs1], RiscvABIRegisterNames[instr.rs2], instr.imm]);

    rvfInstruction: ;          // ebreak

    rvfFence:                  // fence [iorw], [iorw]
    begin
      // fence iorw, iorw is written as fence
      if (instr.imm and $FF) = $FF then exit;

      s1 := '';
      if instr.imm and (1 shl 7) > 0 then
        s1 := 'i';
      if instr.imm and (1 shl 6) > 0 then
        s1 := s1 + 'o';
      if instr.imm and (1 shl 5) > 0 then
        s1 := s1 + 'r';
      if instr.imm and (1 shl 4) > 0 then
        s1 := s1 + 'w';

      s2 := '';
      if instr.imm and (1 shl 3) > 0 then
        s2 := 'i';
      if instr.imm and (1 shl 2) > 0 then
        s2 := s2 + 'o';
      if instr.imm and (1 shl 1) > 0 then
        s2 := s2 + 'r';
      if instr.imm and 1 > 0 then
        s2 := s2 + 'w';

      if (s1 = '') and (s2 = '') then
        Result := format('%-8s', [Result])
      else
        Result := format('%-8s %s, %s', [Result, s1, s2]);
    end;

    rvfCustom:                 // custom-imm
      Result := format('%s-%d', [Result, instr.imm]);

    rvfInstructionSrcOffset:   // cbo.clean (rs1)
      Result := format('%-8s (%s)', [Result, RiscvABIRegisterNames[instr.rs1]]);

    rvfInstructionDestSrcImm:  // addi rd, rs1, imm
      Result := format('%-8s %s, %s, %d', [Result, RiscvABIRegisterNames[instr.rd], RiscvABIRegisterNames[instr.rs1], instr.imm]);

    rvfInstructionDestSrc1Src2:  // sub rd, rs1, rs2
      Result := format('%-8s %s, %s, %s', [Result, RiscvABIRegisterNames[instr.rd], RiscvABIRegisterNames[instr.rs1], RiscvABIRegisterNames[instr.rs2]]);

    rvfInstructionSrc1Src2Imm: // beq rs1, rs2, imm
      Result := format('%-8s %s, %s, %d', [Result, RiscvABIRegisterNames[instr.rs1], RiscvABIRegisterNames[instr.rs2], instr.imm]);

    rvfAmoDestSrc2Src1, rvfAmoDestSrc1, rvfAmoDestSrc2Src1_addr:
    begin
      s1 := '';
      if instr.AmoInfo.width = 2 then
        s1 := '.w'
      else if instr.AmoInfo.width = 3 then
        s1 := '.d';

      s2 := '';
      if instr.AmoInfo.aquire then
        s2 := s2 + 'aq';
      if instr.AmoInfo.release then
        s2 := s2 + 'rel';
      if s2 <> '' then
        s2 := '.' + s2;

      Result := Result + s1 + s2;
      if instr.format = rvfAmoDestSrc2Src1 then        // sc.w rd, rs2, rs1
        Result := format('%-8s %s, %s, %s', [Result, RiscvABIRegisterNames[instr.rd], RiscvABIRegisterNames[instr.rs2], RiscvABIRegisterNames[instr.rs1]])
      else if instr.format = rvfAmoDestSrc1 then            // lr.w rd, rs1
        Result := format('%-8s %s, %s', [Result, RiscvABIRegisterNames[instr.rd], RiscvABIRegisterNames[instr.rs1]])
      else if instr.format = rvfAmoDestSrc2Src1_addr then   // amoswap.w.aq rd, rs2, (rs1)
        Result := format('%-8s %s, %s, (%s)', [Result, RiscvABIRegisterNames[instr.rd], RiscvABIRegisterNames[instr.rs2], RiscvABIRegisterNames[instr.rs1]]);
    end;

    rvfInstructionDestImm:     // auipc rd, imm
      Result := format('%-8s %s, 0x%x', [Result, RiscvABIRegisterNames[instr.rd], instr.imm]);

    rvfInstructionDestSrc:     // clzw rd, rs1
      Result := format('%-8s %s, %s', [Result, RiscvABIRegisterNames[instr.rd], RiscvABIRegisterNames[instr.rs1]]);

    rvfFloatInstrDestSrc1, rvfFloatInstrDestSrc1Src2, rvfFloatInstrDestSrc1Src2Src3:
    begin
      case instr.fpwidth of
        0: s1 := '.S';
        1: s1 := '.D';
        2: s1 := '.H';
        3: s1 := '.Q';
        else
          s1 := '?';
      end;

      Result := Result + s1;
      if instr.format = rvfFloatInstrDestSrc1 then // fsqrt.s rd, rs1
        Result := format('%-8s ft%d, ft%d', [Result, instr.rd, instr.rs1])
      else if instr.format = rvfFloatInstrDestSrc1Src2 then // fsub.s rd, rs1, rs2 => rd = rs1 - rs2
        Result := format('%-8s ft%d, ft%d, ft%d', [Result, instr.rd, instr.rs1, instr.rs2])
      else if instr.format = rvfFloatInstrDestSrc1Src2Src3 then // fmadd.s rd, rs1, rs2, rs3 => (rs1 x rs2) + rs3
        Result := format('%-8s ft%d, ft%d, ft%d, ft%d', [Result, instr.rd, instr.rs1, instr.rs2, instr.rs3]);
    end;

    rvfCSRDestCsrSrc1:         // csrrw rd, #csr, rs1
      Result := format('%-8s %s, %s, %s', [Result, RiscvABIRegisterNames[instr.rd], CsrRegNumToName(instr.csr), RiscvABIRegisterNames[instr.rs1]]);
    rvfCSRDestCsrImm:           // csrrw rd, #csr, imm
      Result := format('%-8s %s, %s, %s', [Result, RiscvABIRegisterNames[instr.rd], CsrRegNumToName(instr.csr), instr.imm]);
  else
    Result := 'Invalid format specifier';
  end;
end;

function TRiscvAsmDecoder.GetLastErrorWasMemReadErr: Boolean;
begin
  Result := FLastErrWasMem;
end;

function TRiscvAsmDecoder.GetMaxInstrSize: integer;
begin
  Result := 4;
end;

function TRiscvAsmDecoder.GetMinInstrSize: integer;
begin
  Result := 2;
end;

function TRiscvAsmDecoder.GetCanReverseDisassemble: boolean;
begin
  Result := False;
end;

function TRiscvAsmDecoder.ReadCodeAt(AnAddress: TDBGPtr; ALen: Cardinal
  ): Boolean;
begin
  Result := FProcess.ReadData(AnAddress, ALen, FCodeBin[0], ALen);
  FLastErrWasMem := not Result;
end;

procedure TRiscvAsmDecoder.Disassemble(var AAddress: Pointer; out
  ACodeBytes: String; out ACode: String);
var
  AnInfo: TDbgInstInfo;
begin
  Disassemble(AAddress, ACodeBytes, ACode, AnInfo);
end;

procedure TRiscvAsmDecoder.Disassemble(var AAddress: Pointer; out ACodeBytes: String;
  out ACode: String; out AnInfo: TDbgInstInfo);
var
  decoder: TRiscvDisassembler;
  k: byte;
  pcode: PByte;
  instr: TRiscvInstruction;
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

function TRiscvAsmDecoder.GetInstructionInfo(AnAddress: TDBGPtr
  ): TDbgAsmInstruction;
begin
  if (FLastInstr = nil) or (FLastInstr.RefCount > 1) then begin
    ReleaseRefAndNil(FLastInstr);
    FLastInstr := TRiscvAsmInstruction.Create(Self);
  end;

  FLastInstr.SetAddress(AnAddress);
  Result := FLastInstr;
end;

function TRiscvAsmDecoder.GetFunctionFrameInfo(AnAddress: TDBGPtr; out
  AnIsOutsideFrame: Boolean): Boolean;
begin
  Result := False;
end;

function TRiscvAsmDecoder.GetBreakInstruction(const ALocation: TDBGPtr; out
  BreakInstructionLength: Integer): QWord;
begin
  BreakInstructionLength := GetInstructionInfo(ALocation).InstructionLength;

  // Set the user fields s and t to 0
  if BreakInstructionLength = 2 then
    Result := $000000 { ILL} // $F0D2 { BREAK }
  else
    Result := $F06D { ILL.N }; // 4000 { BREAK.N }
end;

function TRiscvAsmDecoder.BreakInstructionOffset: Int8;
begin
  Result := 0; // ?
end;

function TRiscvAsmDecoder.GetFunctionFrameReturnAddress(AnAddress, AStartPC,
  AEndPC: TDBGPtr; out returnAddressOffset, SPoffset: integer; out
  AnIsOutsideFrame: Boolean): Boolean;
begin
  result := false;
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

  // Return address always in register ra
  returnAddressOffset := 0;
  SPoffset := 0;

  if (AnAddress = AStartPC) or (AnAddress = AEndPC) then
  begin
    AnIsOutsideFrame := true;
    Exit(True);
  end
  else
    result := FParsePrologue(AnAddress, AStartPC, AEndPC, returnAddressOffset,
                             SPoffset, AnIsOutsideFrame);
end;

constructor TRiscvAsmDecoder.Create(AProcess: TDbgProcess);
begin
  FProcess := AProcess;
end;

destructor TRiscvAsmDecoder.Destroy;
begin
  ReleaseRefAndNil(FLastInstr);
  inherited Destroy;
end;

{ TDbgStackUnwinderRiscv }

constructor TDbgStackUnwinderRiscv.Create(AProcess: TDbgProcess);
begin
  FProcess := AProcess;
  FAddressSize := 4;  // For RV32
  FCodeReadErrCnt := 0;
end;

procedure TDbgStackUnwinderRiscv.InitForThread(AThread: TDbgThread);
begin
  FThread := AThread;
end;

procedure TDbgStackUnwinderRiscv.InitForFrame(
  ACurrentFrame: TDbgCallstackEntry; out CodePointer, StackPointer,
  FrameBasePointer: TDBGPtr);
var
  R: TDbgRegisterValue;
begin
  CodePointer := ACurrentFrame.AnAddress;

  // Frame pointer is x8 (if used).  Assume no frame pointer for now
  FrameBasePointer := ACurrentFrame.FrameAdress;

  StackPointer := 0;
  R := ACurrentFrame.RegisterValueList.FindRegisterByDwarfIndex(SPindexDwarf);
  if R = nil then exit;
  StackPointer := R.NumValue;
end;

procedure TDbgStackUnwinderRiscv.GetTopFrame(out CodePointer, StackPointer,
  FrameBasePointer: TDBGPtr; out ANewFrame: TDbgCallstackEntry);
var
  i: Integer;
  R: TDbgRegisterValue;
begin
  FLastFrameBaseIncreased := True;
  FSPchangeCount := 1;
  CodePointer      := Thread.GetInstructionPointerRegisterValue;
  StackPointer     := Thread.GetStackPointerRegisterValue;
  FrameBasePointer := Thread.GetStackBasePointerRegisterValue;
  ANewFrame        := TDbgCallstackEntry.create(Thread, 0, FrameBasePointer, CodePointer);
  FPreviousPC := CodePointer;

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

function TDbgStackUnwinderRiscv.Unwind(AFrameIndex: integer; var CodePointer,
  StackPointer, FrameBasePointer: TDBGPtr; ACurrentFrame: TDbgCallstackEntry;
  out ANewFrame: TDbgCallstackEntry): TTDbgStackUnwindResult;

{
  add	sp,sp,-12   // Move SP (x2) down by 12 bytes
  sw	ra,8(sp)    // Store ra (x1, return address) at SP + 8
  sw	s0,4(sp)    // Store s0 (x8, frame pointer) at SP + 4
  sw	s1,0(sp)    // Store s1 (x9) at SP
  add	s0,sp,12    // Adjust frame pointer to point to start of this stack
  add	sp,sp,-52   // Move SP down to align to 64 byte boundary (?)
}

const
  MAX_FRAMES = 50; // safety net
  Size = 4; // Default size of pointer
var
  LastFrameBase: TDBGPtr;
  OutSideFrame: Boolean;
  startPC, endPC: TDBGPtr;
  returnAddrStackOffset, SPoffset: integer;
begin
  ANewFrame := nil;
  Result := suFailed;

  if (StackPointer > $40000000) or (StackPointer < $3F000000) or
     (CodePointer < $40000001) or {not FLastFrameBaseIncreased then}
     (AFrameIndex - FSPchangeCount > 0) then
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

  if not TRiscvAsmDecoder(Process.Disassembler).GetFunctionFrameReturnAddress(CodePointer, startPC, endPC,
                          returnAddrStackOffset, SPoffset, OutSideFrame) then
    OutSideFrame := False;

  if OutSideFrame then begin
    // Before adjustment of frame pointer, or after restoration of frame pointer,
    // If SP not yet adjusted then return address should be in register ra
    if returnAddrStackOffset = 0 then
      CodePointer := TDbgRiscvProcess(FProcess).MainThread.RegisterValueList[ReturnAddressIndexDwarf].NumValue
    else if not Process.ReadData(int64(StackPointer) + returnAddrStackOffset, Size, CodePointer) or
      (CodePointer = 0) then
      exit;
    {$PUSH}{$R-}{$Q-}
    StackPointer := StackPointer + SPoffset;
    FrameBasePointer := StackPointer; // After popping return-addr from "StackPtr"
    {$POP}
  end
  else begin
    // Inside stack frame, return PC should be located by offset from FP
    if not Process.ReadData(int64(FrameBasePointer) + returnAddrStackOffset, Size, CodePointer) or (CodePointer = 0) then exit;
    {$PUSH}{$R-}{$Q-}
    FrameBasePointer := StackPointer + SPoffset; // After popping return-addr from stack
    // An estimate of SP, needed when attempting unwinding of next frame
    StackPointer := FrameBasePointer;
    {$POP}
  end;

  FLastFrameBaseIncreased := (FrameBasePointer <> 0) and (FrameBasePointer > LastFrameBase);
  if FLastFrameBaseIncreased or (FPreviousPC <> CodePointer) then
    inc(FSPchangeCount)
  else
    exit;
  FPreviousPC := CodePointer;

  ANewFrame:= TDbgCallstackEntry.create(Thread, AFrameIndex, FrameBasePointer, CodePointer);
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate[nPC].SetValue(CodePointer, IntToStr(CodePointer), Size, PCIndexDwarf);
  ANewFrame.RegisterValueList.DbgRegisterAutoCreate[nSP].SetValue(StackPointer, IntToStr(StackPointer), Size, SPindex);

  FCodeReadErrCnt := 0;
  Result := suSuccess;
end;

{ TRiscvDisassembler }

procedure TRiscvDisassembler.Disassemble(var AAddress: Pointer; out
  AnInstruction: TRiscvInstruction);
var
  pcode: PByte;
  opcode, ophi, oplo: byte;
  instr: uint32;
begin
  pcode := AAddress;
  AnInstruction.OpCode := A_INVALID;
  oplo := pcode[0] and 3;

  if oplo < 3 then
  begin
    // compressed (16 bit) instruction
    instr := pcode[0] or (pcode[1] shl 8);
    ophi := instr shr 13;
    case oplo of
      0:
      case ophi of
        0: AnInstruction := decodeAddi4spn(instr);
        1: AnInstruction := decodeFLD(instr);
        2: AnInstruction := decodeLW(instr);
        3: AnInstruction := decodeFLW(instr);
        4: AnInstruction.OpCode := A_RESERVED;
        5: AnInstruction := decodeFSD(instr);
        6: AnInstruction := decodeSW(instr);
        7: AnInstruction := decodeFSW(instr);
      end;

      1:
      case ophi of
        0: AnInstruction := decodeAddI(instr);
        1: AnInstruction := decodeCJAL(instr);
        2: AnInstruction := decodeLI(instr);
        3: AnInstruction := decodeLUI(instr);
        4: AnInstruction := decodeALU(instr);
        5: AnInstruction := decodeCJ(instr);
        6, 7: AnInstruction := decodeCBNEQZ(instr, ophi);
      end;

      2:
      case ophi of
        0: AnInstruction := decodeSLLI(instr);
        1: AnInstruction := decodeFLDSP(instr);
        2: AnInstruction := decodeLWSP(instr);
        3: AnInstruction := decodeFLWSP(instr);
        4: AnInstruction := decodeJR(instr);
        5, 6, 7: AnInstruction := decodeCStore(instr, ophi);
      end;
    end;
    AnInstruction.Size := 2;
  end
  else
  begin
    instr := pcode[0] or (pcode[1] shl 8) or (pcode[2] shl 16) or (pcode[3] shl 24);
    opcode := pcode[0] and $7F;
    // Lookup based on table 70
    case opcode of
      %0000011: AnInstruction := decodeLoad(instr);
      %0000111: AnInstruction := decodeLoadF(instr);
      %0001011: AnInstruction := customMap(0, instr);
      %0001111: AnInstruction := decodeMiscMem(instr);
      %0010011: AnInstruction := decodeOpIm(instr);
      %0010111: AnInstruction := decodeAUIPC_LUI(instr, opcode);
      %0011011: AnInstruction := decodeOpIm32(instr);
      //%0011111: ;

      %0100011: AnInstruction := decodeStore(instr);
      %0100111: AnInstruction := decodeStoreF(instr);
      %0101011: AnInstruction := customMap(1, instr);
      %0101111: AnInstruction := decodeAMO(instr);
      %0110011: AnInstruction := decodeOp(instr);
      %0110111: AnInstruction := decodeAUIPC_LUI(instr, opcode);
      %0111011: AnInstruction := decodeOp32(instr);
      //%0111111: ;

      %1000011: AnInstruction := decodeMAdd(instr);
      %1000111: AnInstruction := decodeMSub(instr);
      %1001011: AnInstruction := decodeNMSub(instr);
      %1001111: AnInstruction := decodeMAdd(instr);
      %1010011: AnInstruction := decodeOP_FP(instr);
      %1010111: ;//TODO ;
      %1011011: AnInstruction := customMap(2, instr);
      //%1011111: ;

      %1100011: AnInstruction := decodeBranch(instr);
      %1100111: AnInstruction := decodeJALR(instr);
      %1101011: AnInstruction.OpCode := A_RESERVED;
      %1101111: AnInstruction := decodeJAL(instr);
      %1110011: AnInstruction := decodeSystem(instr);
      %1110111: ;
      %1111011: AnInstruction := customMap(3, instr);
      //%1111111: ;
    end;
    AnInstruction.Size := 4;
  end;
end;

procedure TRiscvDisassembler.decodeR(instr: uint32; out funct7, rs2, rs1, funct3, rd: byte);
begin
  funct7 := instr shr 25;
  rs2 := (instr shr 20) and $1F;
  rs1 := (instr shr 15) and $1F;
  funct3 := (instr shr 12) and 7;
  rd := (instr shr 7) and $1F;
end;

procedure TRiscvDisassembler.decodeI(instr: uint32; out imm: int32; out rs1, funct3, rd: byte);
begin
  imm := SarLongint(int32(instr), 20);
  rs1 := (instr shr 15) and $1F;
  funct3 := (instr shr 12) and 7;
  rd := (instr shr 7) and $1F;
end;

procedure TRiscvDisassembler.decodeS(instr: uint32; out imm: int32; out rs2, rs1, funct3: byte);
begin
  imm := SarLongint(int32(instr), 25) shl 5;
  rs2 := (instr shr 20) and $1F;
  rs1 := (instr shr 15) and $1F;
  funct3 := (instr shr 12) and 7;
  imm := imm or ((instr shr 7) and $1F);
end;

procedure TRiscvDisassembler.decodeB(instr: uint32; out imm: int32; out rs2, rs1, funct3: byte);
var
  imm12, imm10_5, imm4_1, imm11: uint16;
begin
  imm12 := instr shr 31;
  imm10_5 := (instr shr 25) and $1F;
  imm4_1 := (instr shr 8) and $0F;
  imm11 :=  (instr shr 7) and 1;
  imm := (imm12 shl 12) or (imm11 shl 11) or (imm10_5 shl 5) or (imm4_1 shl 1);
  rs2 := (instr shr 20) and $1F;
  rs1 := (instr shr 15) and $1F;
  funct3 := (instr shr 12) and 7;
end;

procedure TRiscvDisassembler.decodeU(instr: uint32; out imm: int32; out rd: byte);
begin
  imm := instr shr 12; //instr and $FFFFF000;
  rd := (instr shr 7) and $1F;
end;

procedure TRiscvDisassembler.decodeJ(instr: uint32; out imm: int32; out rd: byte);
var
  imm20, imm19_12, imm11, imm10_1: uint16;
begin
  imm20 := instr shr 31;
  imm19_12 := (instr shr 12) and $FF;
  imm11 :=  (instr shr 20) and 1;
  imm10_1 := (instr shr 21) and $1FF;
  imm := (imm20 shl 20) or (imm19_12 shl 12) or (imm11 shl 11) or (imm10_1 shl 1);
  rd := (instr shr 7) and $1F;
end;

function TRiscvDisassembler.decodeLoad(instr: uint32): TRiscvInstruction;
var
  imm: int32;
  rs1, funct3, rd: byte;
begin
  FillByte(Result, SizeOf(Result), 0);

  // lw rd, imm(rs1)
  Result.format := rvfInstructionDestSrcOffset;

  Result.Size := 4;
  decodeI(instr, imm, rs1, funct3, rd);
  Result.rd:= rd;
  Result.rs1 := rs1;
  Result.imm := imm;

  case funct3 of
    0: Result.OpCode := A_LB;
    1: Result.OpCode := A_LH;
    2: Result.OpCode := A_LW;
    3: Result.OpCode := A_LD; // 64 bit
    4: Result.OpCode := A_LBU;
    5: Result.OpCode := A_LHU;
    6: Result.OpCode := A_LWU; // 64 bit
    //7: Result.OpCode := A_LDU;
    else
      Result.OpCode := A_INVALID;
  end;
end;

function TRiscvDisassembler.decodeLoadF(instr: uint32): TRiscvInstruction;
var
  imm: int32;
  rs1, width, rd: byte;
begin
  FillByte(Result, SizeOf(Result), 0);

  Result.format := rvfLoadFDestSrcOffset;
  Result.Size := 4;
  decodeI(instr, imm, rs1, width, rd);
  Result.rd := rd;
  Result.rs1 := rs1;
  Result.imm := imm;

  case width of
    1: Result.OpCode := A_FLH;
    2: Result.OpCode := A_FLW;
    3: Result.OpCode := A_FLD;
    4: Result.OpCode := A_FLQ;
    else
      Result.OpCode := A_INVALID;
  end;
end;

function TRiscvDisassembler.customMap(i: integer; instr: uint32): TRiscvInstruction;
begin
  FillByte(Result, SizeOf(Result), 0);
  Result.format := rvfCustom;
  Result.Size := 4;
  Result.imm := i;
end;

procedure TRiscvDisassembler.decodeFence(imm: uint32; out fm, pred, succ: byte);
begin
  fm := imm shr 8;
  pred := (imm shr 4) and $F;
  succ := imm and $F;
end;

function TRiscvDisassembler.decodeMiscMem(instr: uint32): TRiscvInstruction;
var
  imm: int32;
  fm, pred, succ, rs1, funct3, rd: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  Result.OpCode := A_INVALID;
  Result.Size := 4;
  decodeI(instr, imm, rs1, funct3, rd);

  case funct3 of
    0:
    begin
      decodeFence(imm, fm, pred, succ);
      // Fence mode (fm) = 0 for normal fence, %1000 for fence.tso
      if (fm = 8) and (pred = 3) and (succ = 3) then
      begin
        Result.OpCode := A_FENCE_TSO;
        Result.format := rvfInstruction;
      end
      else if fm = 0 then
      begin
        if (pred = 1) and (succ = 0) and (rs1 = 0) and (rd = 0) then
        begin
          Result.OpCode := A_PAUSE;
          Result.format := rvfInstruction;
        end
        else
        begin
          // Pred/succ bits are mapped in the following sequence: msb to lsb: i,o,r,w
          Result.OpCode := A_FENCE;
          Result.format := rvfFence;
        end
      end
    end; // funct3 = 0

    1:
    begin
      Result.OpCode := A_FENCE_I;
      Result.format := rvfInstruction;
    end; // funct3 = 1

    2:
    begin
      if rd = 0 then
      begin
        Result.format := rvfInstructionSrcOffset;
        Result.rs1 := rs1;
        Result.rd := rd;
        case imm of
          0: Result.OpCode := A_CBO_INVAL;
          1: Result.OpCode := A_CBO_CLEAN;
          2: Result.OpCode := A_CBO_FLUSH;
          4: Result.OpCode := A_CBO_ZERO;
        end;
      end;
    end; // funct3 = 2
  end; // case funct3
end;

function TRiscvDisassembler.decodeOpIm(instr: uint32): TRiscvInstruction;
var
  imm: int32;
  immhi, immlo, rs1, funct3, rd: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  Result.OpCode := A_INVALID;
  Result.format := rvfInstruction;
  Result.Size := 4;

  decodeI(instr, imm, rs1, funct3, rd);
  immhi := byte(imm shr 6);
  immlo := byte(imm and $3F);

  Result.rd := rd;
  Result.rs1 := rs1;
  case funct3 of
    0:
    begin
      Result.OpCode := A_ADDI;
      Result.imm := imm;
      Result.format := rvfInstructionDestSrcImm;
    end;

    1:
    begin
      Result.imm := immlo;
      Result.format := rvfInstructionDestSrcImm;
      if immhi = 0 then
        Result.OpCode := A_SLLI
      else
        Result.format := rvfInstruction;
    end;

    2:
    begin
      Result.OpCode := A_SLTI;
      Result.imm := imm;
      Result.format := rvfInstructionDestSrcImm;
    end;

    3:
    begin
      Result.OpCode := A_SLTIU;
      Result.imm := imm;
      Result.format := rvfInstructionDestSrcImm;
    end;

    4:
    begin
      Result.OpCode := A_XORI;
      Result.imm := imm;
      Result.format := rvfInstructionDestSrcImm;
    end;

    5:
    begin
      Result.imm := immlo;
      Result.format := rvfInstructionDestSrcImm;
      if immhi = 0 then
        Result.OpCode := A_SRLI
      else if immhi = 16 then
        Result.OpCode := A_SRAI
      else
        Result.format := rvfInstruction;
    end;

    6:
    begin
      Result.OpCode := A_ORI;
      Result.imm := imm;
      Result.format := rvfInstructionDestSrcImm;
    end;

    7:
    begin
      Result.OpCode := A_ANDI;
      Result.imm := imm;
      Result.format := rvfInstructionDestSrcImm;
    end;
  end;
end;

function TRiscvDisassembler.decodeAUIPC_LUI(instr: uint32; opcode: byte): TRiscvInstruction;
begin
  FillChar(Result, SizeOf(Result), 0);
  decodeU(instr, Result.imm, Result.rd);
  Result.format := rvfInstructionDestImm;
  Result.Size := 4;

  if opcode = %0010111 then
    Result.OpCode := A_AUIPC
  else if opcode = %0110111 then
    Result.OpCode := A_LUI
  else
    Result.OpCode := A_INVALID;
end;

function TRiscvDisassembler.decodeOpIm32(instr: uint32): TRiscvInstruction;
var
  imm: int32;
  immhi, immlo, rs1, funct3, rd: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  Result.OpCode := A_INVALID;
  Result.format := rvfInstruction;
  Result.Size := 4;

  decodeI(instr, imm, rs1, funct3, rd);
  immhi := imm shr 5;
  immlo := imm and $1F;

  Result.rd := rd;
  Result.rs1 := rs1;
  case funct3 of
    0:
    begin
      Result.OpCode := A_ADDIW;
      Result.imm := imm;
      Result.format := rvfInstructionDestSrcImm;
    end;

    1:
    begin
      if immhi = 0 then
      begin
        Result.OpCode := A_SLLIW;
        Result.imm := immlo;
        Result.format := rvfInstructionDestSrcImm;
      end
      else if (immhi = 48) then
      begin
        if (immlo = 0) then
        begin
          Result.OpCode := A_CLZW;
          Result.format := rvfInstructionDestSrc;
        end
        else if (immlo = 1) then
        begin
          Result.OpCode := A_CTZW;
          Result.format := rvfInstructionDestSrc;
        end
      end
      else if (immhi = 2) then
      begin
        Result.OpCode := A_SLLI_UW;
        Result.format := rvfInstructionDestSrcImm;
      end;
    end;

    5:
    begin
      Result.imm := immlo;
      Result.format := rvfInstructionDestSrcImm;
      if immhi = 0 then
        Result.OpCode := A_SRLIW
      else if immhi = 32 then
        Result.OpCode := A_SRAIW
      else if immhi = 48 then
        Result.OpCode := A_RORIW
      else
        Result.format := rvfInstruction;
    end;

    6:
    begin
      Result.OpCode := A_ORI;
      Result.imm := imm;
      Result.format := rvfInstructionDestSrcImm;
    end;

    7:
    begin
      Result.OpCode := A_ANDI;
      Result.imm := imm;
      Result.format := rvfInstructionDestSrcImm;
    end;
  end;
end;

function TRiscvDisassembler.decodeStore(instr: uint32): TRiscvInstruction;
var
  funct3: byte;
begin
  FillByte(Result, SizeOf(Result), 0);

  // sd rs2, imm(rs1)
  Result.format := rvfStoreSrc2Src1Offset;

  Result.Size := 4;
  decodeS(instr, Result.imm, Result.rs2, Result.rs1, funct3);

  case funct3 of
    0: Result.OpCode := A_SB;
    1: Result.OpCode := A_SH;
    2: Result.OpCode := A_SW;
    3: Result.OpCode := A_SD; // 64 bit
    else
      Result.OpCode := A_INVALID;
  end;
end;

function TRiscvDisassembler.decodeStoreF(instr: uint32): TRiscvInstruction;
var
  width: byte;
begin
  FillByte(Result, SizeOf(Result), 0);

  Result.format := rvfStoreFSrc2Src1Offset;
  Result.Size := 4;
  decodeS(instr, Result.imm, Result.rs2, Result.rs1, width);

  case width of
    1: Result.OpCode := A_FSH;
    2: Result.OpCode := A_FSW;
    3: Result.OpCode := A_FSD;
    4: Result.OpCode := A_FSQ;
    else
      Result.OpCode := A_INVALID;
  end;
end;

function TRiscvDisassembler.decodeAMO(instr: uint32): TRiscvInstruction;
var
  funct7, funct3: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeR(instr, funct7, Result.rs2, Result.rs1, funct3, Result.rd);
  Result.AmoInfo.aquire := (funct7 and 1) = 1;
  Result.AmoInfo.release := (funct7 and 2) = 2;
  Result.AmoInfo.width := funct3;
  Result.format := rvfAmoDestSrc2Src1_addr;

  case (funct7 shr 2) of
    0: Result.OpCode := A_AMOADD;
    1: Result.OpCode := A_AMOSWAP;
    2:
    begin
      Result.OpCode := A_LR;
      Result.format := rvfAmoDestSrc1;
    end;

    3:
    begin
      Result.OpCode := A_SC;
      Result.format := rvfAmoDestSrc2Src1;
    end;

    4: Result.OpCode := A_AMOXOR;
    8: Result.OpCode := A_AMOOR;
    12: Result.OpCode := A_AMOAND;
    16: Result.OpCode := A_AMOMIN;
    20: Result.OpCode := A_AMOMAX;
    24: Result.OpCode := A_AMOMINU;
    28: Result.OpCode := A_AMOMAXU;
    else
    begin
      Result.OpCode := A_INVALID;
      Result.format := rvfInstruction;
    end;
  end;
end;

function TRiscvDisassembler.decodeOp(instr: uint32): TRiscvInstruction;
var
  funct3, funct7: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeR(instr, funct7, Result.rs2, Result.rs1, funct3, Result.rd);
  Result.format := rvfInstructionDestSrc1Src2;
  Result.OpCode := A_INVALID;

  case funct3 of
    0:
    begin
      if funct7 = 0 then
        Result.OpCode := A_ADD
      else if funct7 = 1 then
        Result.OpCode := A_MUL
      else if funct7 = 32 then
        Result.OpCode := A_SUB;
    end;

    1:
    begin
      if funct7 = 0 then
        Result.OpCode := A_SLL
      else if funct7 = 1 then
        Result.OpCode := A_MULH
      else if funct7 = 5 then
        Result.OpCode := A_CLMUL
      else if funct7 = 20 then
        Result.OpCode := A_BSET
      else if funct7 = 36 then
        Result.OpCode := A_BCLR
      else if funct7 = 48 then
        Result.OpCode := A_ROL
    end;

    2:
    begin
      if funct7 = 0 then
        Result.OpCode := A_SLT
      else if funct7 = 1 then
        Result.OpCode := A_MULHSU
      else if funct7 = 5 then
        Result.OpCode := A_CLMULR
      else if funct7 = 16 then
        Result.OpCode := A_SH1ADD
    end;

    3:
    begin
      if funct7 = 0 then
        Result.OpCode := A_SLTU
      else if funct7 = 1 then
        Result.OpCode := A_MULHU
      else if funct7 = 5 then
        Result.OpCode := A_CLMULH
    end;

    4:
    begin
      if funct7 = 0 then
        Result.OpCode := A_XOR
      else if funct7 = 1 then
        Result.OpCode := A_DIV
      else if funct7 = 4 then
        Result.OpCode := A_PACK // if rs2 = 0, alias: ZEXT.H
      else if funct7 = 16 then
        Result.OpCode := A_SH2ADD
      else if funct7 = 32 then
        Result.OpCode := A_XNOR
      else if funct7 = 52 then
        Result.OpCode := A_BINV
    end;

    5:
    begin
      if funct7 = 0 then
        Result.OpCode := A_SRL
      else if funct7 = 1 then
        Result.OpCode := A_DIVU
      else if funct7 = 5 then
        Result.OpCode := A_MINU
      else if funct7 = 16 then
        Result.OpCode := A_SH3ADD
      else if funct7 = 32 then
        Result.OpCode := A_SRA
      else if funct7 = 36 then
        Result.OpCode := A_BEXT
      else if funct7 = 48 then
        Result.OpCode := A_ROR
    end;

    6:
    begin
      if funct7 = 0 then
        Result.OpCode := A_OR
      else if funct7 = 1 then
        Result.OpCode := A_REM
      else if funct7 = 5 then
        Result.OpCode := A_MAX
      else if funct7 = 32 then
        Result.OpCode := A_ORN
    end;

    7:
    begin
      if funct7 = 0 then
        Result.OpCode := A_AND
      else if funct7 = 1 then
        Result.OpCode := A_REMU
      else if funct7 = 4 then
        Result.OpCode := A_PACKH
      else if funct7 = 5 then
        Result.OpCode := A_MAXU
      else if funct7 = 32 then
        Result.OpCode := A_ANDN;
    end;
  end;
end;

function TRiscvDisassembler.decodeOp32(instr: uint32): TRiscvInstruction;
var
  funct3, funct7: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeR(instr, funct7, Result.rs2, Result.rs1, funct3, Result.rd);
  Result.format := rvfInstructionDestSrc1Src2;
  Result.OpCode := A_INVALID;

  case funct3 of
    0:
    begin
      if funct7 = 0 then
        Result.OpCode := A_ADDW
      else if funct7 = 1 then
        Result.OpCode := A_MULW
      else if funct7 = 4 then
        Result.OpCode := A_ADD_UW
      else if funct7 = 32 then
        Result.OpCode := A_SUBW;
    end;

    1:
    begin
      if funct7 = 0 then
        Result.OpCode := A_SLLW
      else if funct7 = 48 then
        Result.OpCode := A_ROLW;
    end;

    2:
    begin
      if funct7 = 16 then
        Result.OpCode := A_SH1ADD_UW;
    end;

    //3:
    //begin
    //  if funct7 = 0 then
    //    Result.OpCode := A_SLTU;
    //end;

    4:
    begin
      if funct7 = 1 then
        Result.OpCode := A_DIVW
      else if funct7 = 4 then
      begin
        Result.OpCode := A_ZEXT_H;
        Result.format := rvfInstructionDestSrc;
      end
      else if funct7 = 16 then
        Result.OpCode := A_SH2ADD_UW;
    end;

    5:
    begin
      if funct7 = 0 then
        Result.OpCode := A_SRLW
      else if funct7 = 1 then
        Result.OpCode := A_DIVUW
      else if funct7 = 32 then
        Result.OpCode := A_SRAW
      else if funct7 = 48 then
        Result.OpCode := A_RORW;
    end;

    6:
    begin
      if funct7 = 0 then
        Result.OpCode := A_OR
      else if funct7 = 1 then
        Result.OpCode := A_REMW
      else if funct7 = 16 then
        Result.OpCode := A_SH3ADD_UW
    end;

    7:
    begin
      if funct7 = 0 then
        Result.OpCode := A_AND
      else if funct7 = 1 then
        Result.OpCode := A_REMUW;
    end;
  end;
end;

procedure TRiscvDisassembler.decodeR_fmt(instr: uint32; out rs3, fmt, rs2, rs1, roundingMode, rd: byte);
begin
  rs3 := instr shr 27;
  fmt := (instr shr 25) and 3;
  rs2 := (instr shr 20) and $1F;
  rs1 := (instr shr 15) and $1F;
  roundingMode := (instr shr 12) and 7;
  rd := (instr shr 7) and $1F;
end;

function TRiscvDisassembler.decodeMAdd(instr: uint32): TRiscvInstruction;
var
  roundingMode: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeR_fmt(instr, Result.rs3, Result.fpwidth, Result.rs2, Result.rs1, roundingMode, Result.rd);
  Result.Size := 4;
  Result.format := rvfFloatInstrDestSrc1Src2Src3;
  // Rounding mode seems to be ignored
  Result.OpCode := A_FMADD;
end;

function TRiscvDisassembler.decodeMSub(instr: uint32): TRiscvInstruction;
var
  roundingMode: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeR_fmt(instr, Result.rs3, Result.fpwidth, Result.rs2, Result.rs1, roundingMode, Result.rd);
  Result.Size := 4;
  Result.format := rvfFloatInstrDestSrc1Src2Src3;
  // Rounding mode seems to be ignored
  Result.OpCode := A_FMSUB;
end;

function TRiscvDisassembler.decodeNMSub(instr: uint32): TRiscvInstruction;
var
  roundingMode: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeR_fmt(instr, Result.rs3, Result.fpwidth, Result.rs2, Result.rs1, roundingMode, Result.rd);
  Result.Size := 4;
  Result.format := rvfFloatInstrDestSrc1Src2Src3;
  // Rounding mode seems to be ignored
  Result.OpCode := A_FNMSUB;
end;

function TRiscvDisassembler.decodeNMAdd(instr: uint32): TRiscvInstruction;
var
  roundingMode: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeR_fmt(instr, Result.rs3, Result.fpwidth, Result.rs2, Result.rs1, roundingMode, Result.rd);
  Result.Size := 4;
  Result.format := rvfFloatInstrDestSrc1Src2Src3;
  // Rounding mode seems to be ignored
  Result.OpCode := A_FNMADD;
end;

function TRiscvDisassembler.decodeOP_FP(instr: uint32): TRiscvInstruction;
var
  funct3, roundingMode: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeR_fmt(instr, funct3, Result.fpwidth, Result.rs2, Result.rs1, roundingMode, Result.rd);
  Result.Size := 4;
  Result.format := rvfFloatInstrDestSrc1Src2;

  // Rounding mode seems to be ignored
  case funct3 of
    0: Result.OpCode := A_FADD;
    1: Result.OpCode := A_FSUB;
    2: Result.OpCode := A_FMUL;
    3: Result.OpCode := A_FDIV;
    4:
    begin
      case roundingMode of
        0: Result.OpCode := A_FSGNJ;
        1: Result.OpCode := A_FSGNJN;
        2: Result.OpCode := A_FSGNJX;
      end;
    end;

    5:
    begin
      if Result.rs2 = 0 then
      begin
        Result.OpCode := A_FSQRT;
        Result.format := rvfFloatInstrDestSrc1;
      end;
    end;

    8:
    begin
      Result.format := rvfFloatInstrDestSrc1;
      if (Result.rs2 = 0) and (Result.fpwidth = 1) then
        Result.OpCode := A_FCVT_D_S
      else if (Result.rs2 = 1) and (Result.fpwidth = 0) then
        Result.OpCode := A_FCVT_S_D;
    end;

    11:
    begin
      case roundingMode of
        0: Result.OpCode := A_FMIN;
        1: Result.OpCode := A_FMAX;
      end;
    end;

    20:
    begin
      case roundingMode of
        0: Result.OpCode := A_FLE;
        1: Result.OpCode := A_FLT;
        2: Result.OpCode := A_FEQ;
      end;
    end;

    24:
    begin
      Result.format := rvfFloatInstrDestSrc1;
      case Result.rs2 of
        0, 2: Result.OpCode := A_FCVT_Int_S;
        1, 3: Result.OpCode := A_FCVT_UInt_S;
      end;
    end;

    26:
    begin
      case Result.rs2 of
        0, 2: Result.OpCode := A_FCVT_S_Int;
        1, 3: Result.OpCode := A_FCVT_S_UInt;
      end;
    end;
  end;

end;

function TRiscvDisassembler.decodeBranch(instr: uint32): TRiscvInstruction;
var
  funct3: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeB(instr, Result.imm, Result.rs2, Result.rs1, funct3);
  Result.format := rvfInstructionSrc1Src2Imm;
  case funct3 of
    0: Result.OpCode := A_BEQ;
    1: Result.OpCode := A_BNE;
    4: Result.OpCode := A_BLT;
    5: Result.OpCode := A_BGE;
    6: Result.OpCode := A_BLTU;
    7: Result.OpCode := A_BGEU;
    else
      Result.OpCode := A_INVALID;
  end;
end;

function TRiscvDisassembler.decodeJALR(instr: uint32): TRiscvInstruction;
var
  funct3: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeI(instr, Result.imm, Result.rs1, funct3, Result.rd);
  Result.format := rvfInstructionDestSrcOffset;
  if funct3 = 0 then
  begin
    if (Result.rs1 = 1) and (Result.rd = 0) and (Result.imm = 0) then
    begin
      Result.OpCode := A_RET;
      Result.format := rvfInstruction;
    end
    else
      Result.OpCode := A_JALR;
  end
  else
    Result.OpCode := A_INVALID;
end;

function TRiscvDisassembler.decodeJAL(instr: uint32): TRiscvInstruction;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeJ(instr, Result.imm, Result.rd);
  Result.format := rvfInstructionDestImm;
  Result.OpCode := A_JAL
end;

function TRiscvDisassembler.decodeSystem(instr: uint32): TRiscvInstruction;
var
  funct3, funct7: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeI(instr, Result.imm, Result.rs1, funct3, Result.rd);
  Result.format := rvfInstruction;
  Result.OpCode := A_INVALID;

  if (funct3 = 0) and (Result.rd = 0) and (Result.rs1 = 0) then
  begin
    if (Result.imm = 0) then
      Result.OpCode := A_ECALL
    else if (Result.imm = 1) then
      Result.OpCode := A_EBREAK
    else
    begin
      Result.rs2 := Result.imm and $1F;
      funct7 := Result.imm shr 5;
      if (funct7 = $08) and (Result.rs2 = $02) then
        Result.OpCode := A_SRET
      else if (funct7 = $08) and (Result.rs2 = $05) then
        Result.OpCode := A_WFI
      else if (funct7 = $18) and (Result.rs2 = $02) then
        Result.OpCode := A_MRET;
    end;
  end
  else
  begin
    Result.csr := Result.imm;
    case funct3 of
      1:
      begin
        Result.OpCode := A_CSRRW;
        Result.format := rvfCSRDestCsrSrc1;
      end;

      2:
      begin
        Result.OpCode := A_CSRRS;
        Result.format := rvfCSRDestCsrSrc1;
      end;

      3:
      begin
        Result.OpCode := A_CSRRC;
        Result.format := rvfCSRDestCsrSrc1;
      end;

      5:
      begin
        Result.OpCode := A_CSRRWI;
        Result.imm := Result.rs1;
        Result.format := rvfCSRDestCsrImm;
      end;

      6:
      begin
        Result.OpCode := A_CSRRSI;
        Result.imm := Result.rs1;
        Result.format := rvfCSRDestCsrImm;
      end;

      7:
      begin
        Result.OpCode := A_CSRRCI;
        Result.imm := Result.rs1;
        Result.format := rvfCSRDestCsrImm;
      end;
    end;
  end;
end;

function TRiscvDisassembler.formatOpcode(instr: TRiscvInstruction): string;
var
  s1, s2: string;
begin
  Result := RiscvOpcodeString[instr.OpCode];
  if instr.OpCode = A_INVALID then exit;

  case instr.format of
    rvfInstructionDestSrcOffset: // lw rd, imm(rs1)
      Result := format('%-8s %s, %d(%s)', [Result, RiscvABIRegisterNames[instr.rd], instr.imm, RiscvABIRegisterNames[instr.rs1]]);

    rvfLoadFDestSrcOffset:      // flw rd, imm(rs1)
      Result := format('%-8s ft%d, %d(%s)', [Result, instr.rd, instr.imm, RiscvABIRegisterNames[instr.rs1]]);

    rvfStoreSrc2Src1Offset:     // sw rs2, imm(rs1)
      Result := format('%-8s %s, %d(%s)', [Result, RiscvABIRegisterNames[instr.rs2], instr.imm, RiscvABIRegisterNames[instr.rs1]]);

    rvfStoreFSrc2Src1Offset:    // fsw rs2, imm(rs1)
      Result := format('%-8s ft%d, %d(%s)', [Result, instr.rs2, instr.imm, RiscvABIRegisterNames[instr.rs1]]);

    rvfBranchSrc1Src2Imm:       // beq rs1, rs2, imm
      Result := format('%-8s %s, %s, %d', [Result, RiscvABIRegisterNames[instr.rs1], RiscvABIRegisterNames[instr.rs2], instr.imm]);

    rvfInstruction: ;          // ebreak

    rvfFence:                  // fence [iorw], [iorw]
    begin
      // fence iorw, iorw is written as fence
      if (instr.imm and $FF) = $FF then exit;

      s1 := '';
      if instr.imm and (1 shl 7) > 0 then
        s1 := 'i';
      if instr.imm and (1 shl 6) > 0 then
        s1 := s1 + 'o';
      if instr.imm and (1 shl 5) > 0 then
        s1 := s1 + 'r';
      if instr.imm and (1 shl 4) > 0 then
        s1 := s1 + 'w';

      s2 := '';
      if instr.imm and (1 shl 3) > 0 then
        s2 := 'i';
      if instr.imm and (1 shl 2) > 0 then
        s2 := s2 + 'o';
      if instr.imm and (1 shl 1) > 0 then
        s2 := s2 + 'r';
      if instr.imm and 1 > 0 then
        s2 := s2 + 'w';

      if (s1 = '') and (s2 = '') then
        Result := format('%-8s', [Result])
      else
        Result := format('%-8s %s, %s', [Result, s1, s2]);
    end;

    rvfCustom:                 // custom-imm
      Result := format('%s-%d', [Result, instr.imm]);

    rvfInstructionSrcOffset:   // cbo.clean (rs1)
      Result := format('%-8s (%s)', [Result, RiscvABIRegisterNames[instr.rs1]]);

    rvfInstructionDestSrcImm:  // addi rd, rs1, imm
      Result := format('%-8s %s, %s, %d', [Result, RiscvABIRegisterNames[instr.rd], RiscvABIRegisterNames[instr.rs1], instr.imm]);

    rvfInstructionDestSrc1Src2:  // sub rd, rs1, rs2
      Result := format('%-8s %s, %s, %s', [Result, RiscvABIRegisterNames[instr.rd], RiscvABIRegisterNames[instr.rs1], RiscvABIRegisterNames[instr.rs2]]);

    rvfInstructionSrc1Src2Imm: // beq rs1, rs2, imm
      Result := format('%-8s %s, %s, %d', [Result, RiscvABIRegisterNames[instr.rs1], RiscvABIRegisterNames[instr.rs2], instr.imm]);

    rvfAmoDestSrc2Src1, rvfAmoDestSrc1, rvfAmoDestSrc2Src1_addr:
    begin
      s1 := '';
      if instr.AmoInfo.width = 2 then
        s1 := '.w'
      else if instr.AmoInfo.width = 3 then
        s1 := '.d';

      s2 := '';
      if instr.AmoInfo.aquire then
        s2 := s2 + 'aq';
      if instr.AmoInfo.release then
        s2 := s2 + 'rel';
      if s2 <> '' then
        s2 := '.' + s2;

      Result := Result + s1 + s2;
      if instr.format = rvfAmoDestSrc2Src1 then        // sc.w rd, rs2, rs1
        Result := format('%-8s %s, %s, %s', [Result, RiscvABIRegisterNames[instr.rd], RiscvABIRegisterNames[instr.rs2], RiscvABIRegisterNames[instr.rs1]])
      else if instr.format = rvfAmoDestSrc1 then            // lr.w rd, rs1
        Result := format('%-8s %s, %s', [Result, RiscvABIRegisterNames[instr.rd], RiscvABIRegisterNames[instr.rs1]])
      else if instr.format = rvfAmoDestSrc2Src1_addr then   // amoswap.w.aq rd, rs2, (rs1)
        Result := format('%-8s %s, %s, (%s)', [Result, RiscvABIRegisterNames[instr.rd], RiscvABIRegisterNames[instr.rs2], RiscvABIRegisterNames[instr.rs1]]);
    end;

    rvfInstructionDestImm:     // auipc rd, imm
      Result := format('%-8s %s, 0x%x', [Result, RiscvABIRegisterNames[instr.rd], instr.imm]);

    rvfInstructionDestSrc:     // clzw rd, rs1
      Result := format('%-8s %s, %s', [Result, RiscvABIRegisterNames[instr.rd], RiscvABIRegisterNames[instr.rs1]]);

    rvfFloatInstrDestSrc1, rvfFloatInstrDestSrc1Src2, rvfFloatInstrDestSrc1Src2Src3:
    begin
      case instr.fpwidth of
        0: s1 := '.S';
        1: s1 := '.D';
        2: s1 := '.H';
        3: s1 := '.Q';
        else
          s1 := '?';
      end;

      Result := Result + s1;
      if instr.format = rvfFloatInstrDestSrc1 then // fsqrt.s rd, rs1
        Result := format('%-8s ft%d, ft%d', [Result, instr.rd, instr.rs1])
      else if instr.format = rvfFloatInstrDestSrc1Src2 then // fsub.s rd, rs1, rs2 => rd = rs1 - rs2
        Result := format('%-8s ft%d, ft%d, ft%d', [Result, instr.rd, instr.rs1, instr.rs2])
      else if instr.format = rvfFloatInstrDestSrc1Src2Src3 then // fmadd.s rd, rs1, rs2, rs3 => (rs1 x rs2) + rs3
        Result := format('%-8s ft%d, ft%d, ft%d, ft%d', [Result, instr.rd, instr.rs1, instr.rs2, instr.rs3]);
    end;

    rvfCSRDestCsrSrc1:         // csrrw rd, #csr, rs1
      Result := format('%-8s %s, %s, %s', [Result, RiscvABIRegisterNames[instr.rd], CsrRegNumToName(instr.csr), RiscvABIRegisterNames[instr.rs1]]);
    rvfCSRDestCsrImm:           // csrrw rd, #csr, imm
      Result := format('%-8s %s, %s, %s', [Result, RiscvABIRegisterNames[instr.rd], CsrRegNumToName(instr.csr), instr.imm]);
  else
    Result := 'Invalid format specifier';
  end;
end;

{ =========================== 16 bit decoding ============================
Format Meaning              15 14 13 12 11 10 09 08 07 06 05 04 03 02 0100
  CR   Register             funct4     |  rd/rs1      |   rs2        | op
  CI   Immediate            funct3  |im|  rd/rs1      |   imm        | op
  CSS  Stack-relative Store funct3  |     imm         |   rs2        | op
  CIW  Wide Immediate       funct3  |     imm               |  rd'   | op
  CL   Load                 funct3  |    imm |  rs1'  | imm |  rd'   | op
  CS   Store                funct3  |    imm |  rs1'  | imm |  rs2'  | op
  CA   Arithmetic           funct6           | rd/rs' |fnc2 |  rs2'  | op
  CB   Branch/Arithmetic    funct3  | offset | rd/rs' |  offset      | op
  CJ   Jump                 funct3  |               jump target      | op
}

// Imm is 8 bits
procedure TRiscvDisassembler.decodeCIW(instr: uint16; out imm: uint16; out rd_: byte);
begin
  imm := (instr and $1FFF) shr 5;
  rd_ := ((instr and $1C) shr 2) + 8;
end;

// Compressed load & store decoding
// Imm is 5 bits wide and shifted according to immType
procedure TRiscvDisassembler.decodeCLS(instr: uint16; out imm: uint16; out rs1_, rd_rs2_: byte; const immType: TImmEncoding);
begin
  case immType of
    ieImm5376:  imm := ((instr and $1C00) shr 7) or ((instr and $0060) shl 1);
    ieImm54876: imm := ((instr and $0400) shr 2) or ((instr and $1800) shr 7) or ((instr and $60) shl 1);
    ieImm5326:  imm := ((instr and $1C00) shr 7) or ((instr and $0040) shr 4) or (instr and $20) shl 1;
  end;
  rs1_ := ((instr and $380) shr 7) + 8;
  rd_rs2_ := ((instr and $1C) shr 2) + 8;
end;

procedure TRiscvDisassembler.decodeCI(instr: uint16; out rs1_rd: byte; out imm: byte);
begin
  rs1_rd := ((instr and $0F80) shr 7);
  imm := ((instr and $1000) shr 7) or ((instr and $7C) shr 2);
end;

procedure TRiscvDisassembler.decodeCJ(instr: uint16; out imm: int16);
begin
  // instr[12:2] =  12 11 10 09 08 07 06 05 04 03 02
  // imm         =  11 04 09 08 10 06 07 03 02 01 05
  imm := ((instr and $1000) shr 1) or   // 12 => 11
         ((instr and $0800) shr 7) or   // 11 => 4
         ((instr and $0600) shr 1) or   // 10:9 => 9:8
         ((instr and $0100) shl 2) or   // 8 => 10
         ((instr and $0080) shr 1) or   // 7 => 6
         ((instr and $0040) shl 1) or   // 6 => 7
         ((instr and $0038) shr 2) or   // 5:3 => 3:1
         ((instr and $0004) shl 3);     // 2 => 5

  // Check if sign must be extended
  if (imm and $800) > 0 then
    imm := imm or int16($F000);
end;

procedure TRiscvDisassembler.decodeCB(instr: uint16; out rs1_: byte; out imm: int16);
begin
{  Format Meaning              15 14 13 12 11 10 09 08 07 06 05 04 03 02 0100
     CB   Branch/Arithmetic    funct3  | offset | rd/rs  |  offset      | op
}
  rs1_ := ((instr and $0380) shr 7) + 8;
  // instr[12:2] =  12 11 10 06 05 04 03 02
  // imm         =  08 04 03 07 06 02 01 05
  imm := ((instr and $1000) shr 4) or   // 12 => 8
         ((instr and $0C00) shr 7) or   // 11:10 => 4:3
         ((instr and $0060) shl 1) or   // 6:5 => 7:6
         ((instr and $0018) shr 2) or   // 4:3 => 2:1
         ((instr and $0004) shl 3);     // 2 => 5

  if (imm and $100) > 0 then
    imm := imm or int16($FE00);
end;

procedure TRiscvDisassembler.decodeCSS(instr: uint16; out uimm: uint16; out rs2: byte; const immtype: TImmEncoding2);
begin
  rs2 := (instr shr 2) and $1F;
  uimm := instr and $1F80;
  case immtype of
    // instr[12:10] => uimm[5:3] | instr[9:7] => uimm[8:6]
    ie2Imm5386:  uimm := ((uimm and $1C00) shr 7) or ((uimm and $0380) shr 1);

    // instr[12:11] => uimm[5:4] | instr[10:7] => uimm[9:6]
    ie2Imm5496:  uimm := ((uimm and $1800) shr 7) or ((uimm and $0780) shr 1);

    // instr[12:9] => uimm[5:2] | instr[8:7] => uimm[7:6]
    ie2Imm5276:  uimm := ((uimm and $1E00) shr 7) or ((uimm and $0180) shr 1);
  end;
end;

function TRiscvDisassembler.decodeAddi4spn(instr: uint16):TRiscvInstruction;
var
  imm: uint16;
begin
  FillByte(Result, SizeOf(Result), 0);
  Result.OpCode := A_INVALID;
  decodeCIW(instr, imm, Result.rd);

  if imm > 0 then
  begin
    // [7:6|5:2|1|0]
    // [5:4|9:6|2|3]
    Result.imm := ((imm and $C0) shr 2) or  // 7:6 => 5:4
                  ((imm and $3C) shl 4) or  // 5:2 => 9:6
                  ((imm and $02) shl 1) or  // 1 => 2
                  ((imm and $01) shl 3);    // 0 => 3
    Result.rs1 := 2;
    Result.OpCode := A_ADDI;
    Result.format := rvfInstructionDestSrcImm;
  end
  else if Result.rd = 0 then
  begin
    Result.OpCode := A_ILL;
    Result.format := rvfInstruction;
  end;
end;

function TRiscvDisassembler.decodeFLD(instr: uint16):TRiscvInstruction;
var
  imm: uint16;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeCLS(instr, imm, Result.rs1, Result.rd, ieImm5376);
  Result.imm := imm;
  // For RV32/64.  For RV128 this would be LQ
  Result.OpCode := A_FLD;
  Result.format := rvfLoadFDestSrcOffset;
end;

function TRiscvDisassembler.decodeLW(instr: uint16):TRiscvInstruction;
var
  imm: uint16;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeCLS(instr, imm, Result.rs1, Result.rd, ieImm5326);
  Result.imm := imm;
  Result.OpCode := A_LW;
  Result.format := rvfInstructionDestSrcOffset;
end;

function TRiscvDisassembler.decodeFLW(instr: uint16):TRiscvInstruction;
var
  imm: uint16;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeCLS(instr, imm, Result.rs1, Result.rd, ieImm5326);
  Result.imm := imm;
  // For RV32.  For RV64/128 this would be LD
  Result.OpCode := A_FLW;
  Result.format := rvfLoadFDestSrcOffset;
end;

function TRiscvDisassembler.decodeFSD(instr: uint16):TRiscvInstruction;
var
  imm: uint16;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeCLS(instr, imm, Result.rs1, Result.rs2, ieImm5326);
  Result.imm := imm;
  // For RV32/64.  For RV128 this would be SQ
  Result.OpCode := A_FSD;
  Result.format := rvfStoreFSrc2Src1Offset;
end;

function TRiscvDisassembler.decodeSW(instr: uint16):TRiscvInstruction;
var
  imm: uint16;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeCLS(instr, imm, Result.rs1, Result.rs2, ieImm5326);
  Result.imm := imm;
  Result.OpCode := A_SW;
  Result.format := rvfStoreSrc2Src1Offset;
end;

function TRiscvDisassembler.decodeFSW(instr: uint16):TRiscvInstruction;
var
  imm: uint16;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeCLS(instr, imm, Result.rs1, Result.rs2, ieImm5376);
  Result.imm := imm;
  // For RV32.  For RV64/128 this would be SD
  Result.OpCode := A_FSW;
  Result.format := rvfStoreFSrc2Src1Offset;
end;

function TRiscvDisassembler.decodeAddI(instr: uint16): TRiscvInstruction;
var
  imm: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeCI(instr, Result.rd, imm);
  Result.imm := imm;
  // Check sign bit
  if imm > $1F then
    Result.imm := Result.imm or int32($FFFFFFC0);
  Result.rs1 := Result.rd;
  // For RV32.  For RV64/128 this would be SD
  if (imm = 0) and (Result.rd = 0) then
  begin
    Result.OpCode := A_NOP;
    Result.format := rvfInstruction;
  end
  else
  begin
    Result.OpCode := A_ADDI;
    Result.format := rvfInstructionDestSrcImm;
  end;
end;

function TRiscvDisassembler.decodeCJAL(instr: uint16): TRiscvInstruction;
var
  imm: int16;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeCJ(instr, imm);
  Result.imm := imm;
  Result.OpCode := A_JAL;
  Result.rd := 1;
  Result.format := rvfInstructionDestImm;
end;

function TRiscvDisassembler.decodeLI(instr: uint16): TRiscvInstruction;
var
  imm: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeCI(instr, Result.rd, imm);
  Result.rs1 := 0;

  Result.imm := imm;
  // Check sign bit
  if (imm and $20) > 0 then
    Result.imm := Result.imm or int32($FFFFFFC0);
  Result.OpCode := A_ADDI;
  Result.format := rvfInstructionDestSrcImm;
end;

function TRiscvDisassembler.decodeLUI(instr: uint16): TRiscvInstruction;
var
  imm: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeCI(instr, Result.rd, imm);
  Result.rs1 := 0;

  if imm = 0 then
  begin
    Result.OpCode := A_RESERVED;
    Result.format := rvfInstruction;
  end
  else if Result.rd = 2 then
  begin
    Result.imm := (int32(imm and $20) shl 4) or // 5 => 9
                  (int32(imm and $10) shl 0) or // 4 => 4
                  (int32(imm and $08) shl 3) or // 3 => 6
                  (int32(imm and $06) shl 6) or // 2:1 => 8:7
                  (int32(imm and $01) shl 5);   // 0 => 5
    Result.rs1 := 2;
    // Check sign bit
    if (imm and $20) > 0 then
      Result.imm := Result.imm or int32($FFFFFC00);
    Result.OpCode := A_ADDI;
    Result.format := rvfInstructionDestSrcImm;
  end
  else // if rd = 0 then hint
  begin
    Result.imm := imm;
    // Check sign bit
    if (imm and $20) > 0 then
    Result.imm := Result.imm or int32($FFFFFC00);
    Result.OpCode := A_LUI;
    Result.format := rvfInstructionDestImm;
  end;
end;

function TRiscvDisassembler.decodeALU(instr: uint16):TRiscvInstruction;
var
  imm5, imm40, rs_rd, funct2: byte;
begin
  FillByte(Result, SizeOf(Result), 0);

  imm5 := (instr shr 12) and 1;
  rs_rd := ((instr shr 7) and $07) + 8;
  imm40 := (instr shr 2) and $1F;
  funct2 := (instr shr 10) and $03;

  Result.rd := rs_rd;
  Result.rs1 := rs_rd;
  case funct2 of
    0:
    begin
      Result.imm := (imm5 shl 5) or imm40;
      Result.OpCode := A_SRLI;
      Result.format := rvfInstructionDestSrcImm;
    end;

    1:
    begin
      Result.imm := (imm5 shl 5) or imm40;
      Result.OpCode := A_SRAI;
      Result.format := rvfInstructionDestSrcImm;
    end;

    2:
    begin
      Result.imm := (imm5 shl 5) or imm40;
      // Check sign
      if imm5 = 1 then
        Result.imm := Result.imm or int32($FFFFFFC0);
      Result.OpCode := A_ANDI;
      Result.format := rvfInstructionDestSrcImm;
    end;

    3:
    begin
      if imm5 = 1 then
      begin
        // TODO: Ignore 32 bit operations W on RV64/RV128
        Result.OpCode := A_RESERVED;
        Result.format := rvfInstruction;
      end
      else
      begin
        funct2 := imm40 shr 3;
        Result.rs2 := (imm40 and $03) + 8;
        Result.format := rvfInstructionDestSrc1Src2;
        case funct2 of
          0: Result.OpCode := A_SUB;
          1: Result.OpCode := A_XOR;
          2: Result.OpCode := A_OR;
          3: Result.OpCode := A_AND;
        end;
      end;
    end;
  end;
end;

function TRiscvDisassembler.decodeCJ(instr: uint16): TRiscvInstruction;
var
  imm: int16;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeCJ(instr, imm);
  Result.imm := imm;
  Result.OpCode := A_JAL;
  Result.rd := 0;
  Result.format := rvfInstructionDestImm;
end;

function TRiscvDisassembler.decodeCBNEQZ(instr: uint16; funct3: byte): TRiscvInstruction;
var
  imm: int16;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeCB(instr, Result.rs1, imm);
  Result.imm := imm;
  Result.format := rvfInstructionSrc1Src2Imm;
  Result.rs2 := 0;
  if funct3 = 6 then
    Result.OpCode := A_BEQ
  else
    Result.OpCode := A_BNE;
end;

function TRiscvDisassembler.decodeSLLI(instr: uint16): TRiscvInstruction;
var
  imm: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeCI(instr, Result.rd, imm);
  // For RV32C, if imm = 0, then HINT
  Result.imm := imm;
  Result.rs1 := Result.rd;
  if Result.imm < $20 then  // Restriction only for RV32C
  begin
    Result.OpCode := A_SLLI;
    Result.format := rvfInstructionDestSrcImm;
  end
  else
  begin
    Result.OpCode := A_CUSTOM;
    Result.format := rvfCustom;
  end;
end;

function TRiscvDisassembler.decodeFLDSP(instr: uint16): TRiscvInstruction;
var
  imm: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeCI(instr, Result.rd, imm);

  Result.imm := (imm and $38) or
                (imm and $07) shl 6;
  Result.rs1 := 2;
  // TODO: RV128C => C.LQSP
  Result.OpCode := A_FLD;
  Result.format := rvfLoadFDestSrcOffset;
end;

function TRiscvDisassembler.decodeLWSP(instr: uint16): TRiscvInstruction;
var
  imm: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeCI(instr, Result.rd, imm);

  // uimm:  05 04 03 02 01 00
  //  imm:  05 04 03 02 07 06
  Result.imm := (imm and $3C) or
                (imm and $03) shl 6;
  Result.rs1 := 2;
  Result.OpCode := A_LW;
  Result.format := rvfInstructionDestSrcOffset;
end;

function TRiscvDisassembler.decodeFLWSP(instr: uint16): TRiscvInstruction;
var
  imm: byte;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeCI(instr, Result.rd, imm);

  Result.imm := (imm and $30) or
                (imm and $0F) shl 6;
  Result.rs1 := 2;
  // TODO: RV32/RV128C => C.LDSP
  if Result.rd = 8 then
  begin
    Result.OpCode := A_RESERVED;
    Result.format := rvfInstruction;
  end
  else
  begin
    Result.OpCode := A_FLW;
    Result.format := rvfLoadFDestSrcOffset;
  end;
end;

function TRiscvDisassembler.decodeJR(instr: uint16): TRiscvInstruction;
var
  imm: byte;
  imm5: boolean;
begin
  FillByte(Result, SizeOf(Result), 0);
  decodeCI(instr, Result.rd, imm);

  imm5 := imm and $20 = $20;
  Result.rs2 := (imm and $1F);
  Result.rs1 := Result.rd;

  if not imm5 then
  begin
    // TODO: RV32/RV128C => C.LDSP
    if Result.rs2 = 0 then
    begin
      if Result.rs1 = 0 then
      begin
        Result.OpCode := A_RESERVED;
        Result.format := rvfInstruction;
      end
      else
      begin
        Result.rd := 0;
        if (Result.rs1 = 1) and (Result.imm = 0) then
        begin
          Result.OpCode := A_RET;
          Result.format := rvfInstruction;
        end
        else
        begin
        Result.OpCode := A_JALR;
        Result.format := rvfInstructionDestSrcOffset;
        end;
      end
    end
    else // rs2 > 0
    begin
      // if Result.rd = 0 then hint else
      Result.rs1 := 0;
      Result.OpCode := A_ADD;
      Result.format := rvfInstructionDestSrc1Src2;
    end;
  end
  else // imm5 > 0
  begin
    if (Result.rd = 0) and (Result.rs2 = 0) then
    begin
      Result.OpCode := A_EBREAK;
      Result.format := rvfInstruction;
    end
    else if Result.rs2 = 0 then
    begin
      Result.rd := 1;
      Result.OpCode := A_JALR;
      Result.format := rvfInstructionDestSrcOffset;
    end
    else  // if rs2 > 0 else hint
    begin
      Result.OpCode := A_ADD;
      Result.format := rvfInstructionDestSrc1Src2;
    end;
  end;
end;

function TRiscvDisassembler.decodeCStore(instr: uint16; funct3: byte): TRiscvInstruction;
var
  uimm: uint16;
begin
  FillByte(Result, SizeOf(Result), 0);
  Result.rs1 := 2;

  case funct3 of
    5:
    begin
      // if RV32 then
      decodeCSS(instr, uimm, Result.rs2, ie2Imm5386);
      Result.imm := uimm;
      Result.OpCode := A_FSD;
      Result.format := rvfStoreFSrc2Src1Offset;
    end;

    6:
    begin
      decodeCSS(instr, uimm, Result.rs2, ie2Imm5276);
      Result.imm := uimm;
      Result.OpCode := A_SW;
      Result.format := rvfStoreSrc2Src1Offset;
    end;

    7:
    begin
      // if RV32 then
      decodeCSS(instr, uimm, Result.rs2, ie2Imm5276);
      Result.imm := uimm;
      Result.OpCode := A_FSD;
      Result.format := rvfStoreFSrc2Src1Offset;
    end;
  end;
end;

initialization
  DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );

end.
