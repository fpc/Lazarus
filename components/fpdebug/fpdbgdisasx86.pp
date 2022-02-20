{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpdbgdisasx86.pp  -  Native Freepascal debugger - x86 Disassembler
 ---------------------------------------------------------------------------

 This unit contains a x86 disassembler for the Native Freepascal debugger

 ---------------------------------------------------------------------------

 @created(Mon Apr 22th WET 2006)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.nl>)

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
unit FpDbgDisasX86;
{$mode objfpc}{$H+}
interface

{.$define debug_OperandSize}
{.$define verbose_string_instructions}

uses
  SysUtils, FpDbgUtil, FpDbgInfo, DbgIntfBaseTypes, FpdMemoryTools,
  FpDbgClasses, {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif}, LazClasses;

{                   
  The function Disassemble decodes the instruction at the given address.
  After decoding, the address increased to the next instruction.
  The following chars are used to indicate problems with instruction
  sequenses:
  ** invalid opcode
  -- reserved opcode
  () ignored opcode
  ?? unspecified
  !! internal error, a group got called for an opcode which wasn't decoded there

  The disassembler starts to decode the first byte according to

  Intel(r) 64 and IA-32 Architectures Software Developer’s Manual Volume 2:
  Table A-2. One-byte Opcode Map: (00H — F7H)
  and
  Table A-2. One-byte Opcode Map: (08H — FFH)

  The 3DNow!(tm) instructions are decoded according to
  AMD64 Architecture Programmer’s Manual Volume 3:
  Table A-13. Immediate Byte for 3DNow!(tm) Opcodes, Low Nibble 0–7h
  and
  Table A-14. Immediate Byte for 3DNow!(tm) Opcodes, Low Nibble 8–Fh

  The routines Addxx use the same abbriviations as used in those tables

}  


type
  // rexB, rexX, rexR, rexW see
  // Intel(r) 64 and IA-32 Architectures Software Developer’s Manual Volume 2:
  // Table 2-4. REX Prefix Fields [BITS: 0100WRXB]

  TFlag = (
    flagRex, // $4x: set for any $4X
    flagSib,
    flagModRM,
    rexB,    // $4x bit 0:  Extension of the ModR/M r/m field, SIB base field, or Opcode reg field
    rexX,    // $4x bit 1:  Extension of the SIB index field
    rexR,    // $4x bit 2:  Extension of the ModR/M reg field
    rexW,    // $4x bit 3:  64 Bit Operand Size
    pre66,   // $66:  	Operand-size override prefix  (32 and 64 bit) or SIMD prefix
    preAdr,  // $67:    Address-size override prefix
    preLock,
    preF2,   // $F2: Repeat string/input/output or SIMD prefix
    preF3,   // $F3: Repeat string/input/output or SIMD prefix
    oprDefault64,  // In 64bit mode set default operand size to 64 (d64 note in Table A-x)
    oprForce64,    // In 64bit mode set operand size to 64 (f64 note in Table A-x)
    flagVex,
    vexL,
    vexW,
    vexX,
    vexB
  );
  TFlags = set of TFlag;
  
  // Keep 8,16,32,64 together
  TOperandSize = (os8, os16, os32, os64, os48, os80, os128);
  TAddressSize = (as16, as32, as64);
  TRegisterType = (reg0, reg8, reg16, reg32, reg64, regMmx, regXmm, regSegment, regControl, regDebug, regX87);
  TModRMType = (modReg, modMem);
  TModRMTypes = set of TModRMType;
  TSimdOpcode = (soInvalid, soNone, so66, soF2, soF3);

  TInstructionFlag = (
    ifOnly32, ifOnly64,
    ifPrefixLock, ifPrefixRep, ifPrefixRepE, ifPrefixRepNe
  );

  TOpCode = (
    OPX_InternalUnknown,
    OPX_Invalid,
    OPX_InvalidX87, OPX_ReservedX87, OPX_Not87,
    OPX_3dnow,
    OPX_Group1a, OPX_NotGroup1,
    OPX_NotGroup2,
    OPX_NotGroup3,
    OPX_Group4, OPX_NotGroup4,
    OPX_Group5, OPX_NotGroup5,
    OPX_Group6, OPX_NotGroup6,
    OPX_Group7, OPX_NotGroup7,
    OPX_Group8, OPX_NotGroup8,
    OPX_Group9, OPX_NotGroup9,
    OPX_Group10, OPX_NotGroup10,
    OPX_Group11, OPX_NotGroup11,
    OPX_Group12, OPX_NotGroup12,
    OPX_Group13, OPX_NotGroup13,
    OPX_Group14, OPX_NotGroup14,
    OPX_Group15, OPX_NotGroup15,
    OPX_Group16, OPX_NotGroup16,
    OPX_GroupP, OPX_NotGroupP,
    OPXsysenter, OPXsysexit,

    OPfadd, OPfmul, OPfcom, OPfcomp, OPfsub, OPfsubr, OPfdiv, OPfdivr,
    OPfld, OPfxch, OPfst, OPfstp, OPfldenv, OPfldcw, OPfnstenv, OPfnstcw,
    OPfchs, OPfabs, OPftst, OPfxam, OPfld1, OPfldl2t, OPfldl2e, OPfldpi, OPfldlg2, OPfldln2, OPfldz,
    OPf2xm1, OPfyl2x, OPfptan, OPfpatan, OPfxtract, OPfprem1, OPfdecstp,
    OPfincstp, OPfprem, OPfyl2xp1, OPfsqrt, OPfsincos, OPfrndint,
    OPfscale, OPfsin, OPfcos, OPnop, OPfiadd, OPfimull,
    OPficom, OPficomp, OPfisub, OPfisubr, OPfidiv, OPfidivr,
    OPfcmovb, OPfcmove, OPfcmovbe, OPfcmovu, OPfucompp,
    OPfild, OPfisttp, OPfist, OPfistp, OPfcmovnb, OPfcmovne, OPfcmovnbe, OPfcmovnu,
    OPfucomi, OPfcomi, OPfnclex, OPfninit, OPfrstor, OPfnsave, OPfnstsw,
    OPffree, OPfucomp, OPfaddp, OPfmullp, OPfsubrp, OPfsubp, OPfdivrp,
    OPfdivp, OPfbld, OPfbstp, OPfcompp, OPfucomip, OPfcomip,

    OPpi2fw, OPpi2fd, OPpf2iw, OPpf2id, OPpfnacc, OPpfpnacc, OPpfcmpge,
    OPpfmin, OPpfrcp, OPpfrsqrt, OPpfsub, OPpfadd, OPpgcmpgt, OPpfmax,
    OPpfrcpit1, OPpfrsqit1, OPpfsubr, OPpfacc, OPpfcmpeq, OPpfmul,
    OPpfrcpit2, OPpmulhrw, OPpswapd, OPpavgusb,

    OPadd, OPor, OPadc, OPsbb, OPand, OPsub, OPxor, OPcmp, OPpop,
    OProl, OPror, OPrcl, OPrcr, OPshl, OPshr, OPsal, OPsar,
    OPtest, OPnot, OPneg, OPmul, OPimul, OPdiv, OPidiv,
    OPinc, OPdec,
    OPcall, OPjmp, OPpush,
    OPsldt, OPstr, OPlldt, OPltr, OPverr, OPverw,
    OPvmrun, OPvmmcall, OPvmload, OPvmsave, OPstgi, OPclgi, OPskinit, OPinvlpga,
    OPsgdt, OPsidt, OPlgdt, OPlidt, OPsmsw, OPlmsw, OPswapgs, OPrdtscp, OPinvlpg,
    OPbts, OPbtr, OPbtc, OPbt,
    OPcmpxchg16b, OPcmpxchg8b,

    OPmov, OPpsrlw, OPpsraw, OPpsllw, OPpsrld, OPpsrad, OPpslld, OPpsrlq, OPpsrldq, OPpsllq,
    OPfxsave, OPfxrstor, OPldmxcsr, OPstmxcsr, OPlfence, OPmfence, OPclflush,
    OPprefetch_exclusive, OPprefetch_modified, OPX_prefetch,

    OPpunpcklbw, OPpunpcklwd, OPpunpcklqd, OPpacksswb, OPpcmpgtb, OPpcmpgtw, OPpcmpgtd,
    OPpackuswb, OPpunpkhbw, OPpunpkhwd, OPpunpkhdq, OPpackssdw, OPpunpcklqdq, OPpunpckhqdq,
    OPpaddq, OPpmullw, OPpsubusb, OPpsubusw, OPpminub, OPpand, OPpaddusb, OPpaddusw, OPpmaxub, OPpandn,
    OPpavgb, OPpavgw, OPpmulhuw, OPpmulhw, OPpsubsb, OPpsubsw, OPpminsw,
    OPpor, OPpaddsb, OPpaddsw, OPpmaxsw, OPpxor, OPpmuludq, OPpmaddwd,
    OPpsadbw, OPpsubb, OPpsubw, OPpsubd, OPpsubq, OPpaddb, OPpaddw, OPpaddd,

    OPlar, OPlsl, OPsyscall, OPclts, OPsysret, OPinvd, OPwbinvd, OPud2,
    OPfemms, OPmovups, OPmovss, OPmovupd, OPmovsd, OPmovhlps,
    OPmovsldup, OPmovlpd, OPmovddup, OPmovlps, OPunpcklps, OPunpcklpd, OPunpckhps, OPunpckhpd, OPmovlhps,
    OPmovshdup, OPmovhpd, OPmovhps, OPmovaps, OPmovapd,
    OPcvtpi2ps, OPcvtsi2ss, OPcvtpi2pd, OPcvtsi2sd, OPmovntps, OPmovntpd,
    OPcvttps2pi, OPcvttss2si, OPcvttpd2pi, OPcvttsd2si, OPcvtps2pi, OPcvtss2si, OPcvtpd2pi, OPcvtsd2si,
    OPucomiss, OPucomissd, OPcomiss, OPcomissd, OPwrmsr, OPrdtsc, OPrdmsr, OPrdpmc,

    OPcmov__, OPmovmskps, OPmovmskpd, OPsqrtps, OPsqrtss, OPsqrtpd, OPsqrtsd,
    OPaddps, OPaddss, OPaddpd, OPaddsd, OPmulps, OPmulss, OPmulpd, OPmulsd,
    OPsubps, OPsubss, OPsubpd, OPsubsd, OPminps, OPminss, OPminpd, OPminsd,
    OPdivps, OPdivss, OPdivpd, OPdivsd, OPmaxps, OPmaxss, OPmaxpd, OPmaxsd,
    OPrsqrtps, OPrsqrtss, OPrcpps, OPrcpss, OPandps, OPandpd, OPandnps, OPandnpd,
    OPorps, OPorpd, OPxorps, OPxorpd,
    OPcvtps2pd, OPcvtss2sd, OPcvtpd2ps, OPcvtsd2ss, OPcvtdq2ps, OPcvttps2dq, OPcvtps2dq,
    OPmovd, OPmovq, OPmovdqu, OPmovdqa, OPpshufw, OPpshufhw, OPpshufd, OPpshuflw,
    OPpcmpeqb, OPpcmpeqw, OPpcmpeqd, OPemms, OPhaddpd, OPhaddps, OPhsubpd, OPhsubps,
    OPj__,
    OPset__, OPcpuid, OPshld, OPrsm, OPshrd,
    OPcmpxchg, OPlss, OPlfs, OPlgs, OPmovzx, OPbsf, OPbsr, OPmovsx, OPxadd,
    OPcmpps, OPcmpss, OPcmppd, OPcmpsd, OPmovnti, OPpinsrw, OPpextrw, OPshufps, OPshufpd, OPbswp,
    OPaddsubpd, OPaddsubps, OPmovq2dq, OPmovdq2q, OPpmovmskb, OPcvtdq2pd, OPcvttpd2dq, OPcvtpd2dq,
    OPmovntq, OPmovntdqu, OPlddqu, OPmaskmovq, OPmaskmovdqu,
    OPdaa, OPdas, OPaaa, OPaas, OPpusha, OPpushad, OPpopa, OPpopad, OPbound, OPmovsxd,
    OParpl, OPinsb, OPinsw, OPinsd, OPoutsb, OPoutsw, OPoutsd,
    OPxchg, OPlea, OPcdqe, OPcwde, OPcbw, OPcqo, OPcqd, OPcwd,
    OPwait_fwait,
    OPpushfq, OPpushfd, OPpushf, OPpopfq, OPpopfd, OPpopf, OPsahf, OPlahf,
    OPmovsb, OPmovsq, OPmovsw, OPcmpsb, OPcmpsq, OPcmpsw,
    OPstosb, OPstosq, OPstosd, OPstosw, OPlodsb, OPlodsq, OPlodsd, OPlodsw,
    OPscasb, OPscasq, OPscasd, OPscasw, OPret, OPles, OPlds,
    OPenter, OPleave, OPretf, OPint3, OPint, OPint0, OPiretq, OPiretd, OPiret,
    OPaam, OPaad, OPsalc, OPxlat, OPloopne, OPloope, OPloop, OPjrcxz,
    OPin, OPout, OPint1, OPhlt, OPcmc, OPclc, OPstc, OPcli, OPsti, OPcld, OPstd
  );

  TOpCodeSuffix = (
    OPSx_none,
    // Condition
    OPSx_o, OPSx_no, OPSx_b, OPSx_nb, OPSx_z, OPSx_nz, OPSx_be, OPSx_nbe, OPSx_s, OPSx_ns, OPSx_p, OPSx_np, OPSx_l, OPSx_nl, OPSx_le, OPSx_nle
  );


  TOperandFlag = (ofMemory);
  TOperandFlags = set of TOperandFlag;

  TInstruction = record
    OpCode: TOpCode;
    OpCodeSuffix: TOpCodeSuffix;
    Flags: set of TInstructionFlag;
    Segment: String;

    Operand: array[1..4] of record
      CodeIndex: integer;
      Value: String;
      Size: TOperandSize;
      ByteCount: Byte;
      ByteCount2: Byte;
      FormatFlags: THexValueFormatFlags;
      Flags: TOperandFlags;
    end;
    OperCnt: Integer;

    ParseFlags: TFlags;
  end;
  PInstruction = ^TInstruction;

  { TX86Disassembler }

  TX86Disassembler = object
  private
    ProcessMode: TFPDMode;
    Code: PByte;
    CodeIdx: Byte;
    OperIdx: Integer;
    ModRMIdx: Byte;
    Flags: TFlags;
    SimdOpcode: TSimdOpcode;

    //--- result ---
    Instruction: PInstruction;

    //--- add operands ---
    // the the Zz as used in AddZz routines are encoded according to:
    //
    // Intel(r) 64 and IA-32 Architectures Software Developer’s Manual Volume 2:
    //  A.2 KEY TO ABBREVIATIONS
    //
    // and
    //
    // AMD64 Architecture Programmer’s Manual Volume 3:
    //  Appendix A Opcode and Operand Encodings
    //
    //---
    procedure AddAp;
    procedure AddCd_q;
    procedure AddDd_q;
    procedure AddEb;
    procedure AddEd;
    procedure AddEd_q;
    procedure AddEv;
    procedure AddEw;
    procedure AddFv;
    procedure AddGb;
    procedure AddGd;
    procedure AddGd_q;
    procedure AddGv;
    procedure AddGw;
    procedure AddGz;
    procedure AddIb;
    procedure AddIv;
    procedure AddIw;
    procedure AddIz;
    procedure AddJb;
    procedure AddJz;
    procedure AddM;
    procedure AddMa;
    procedure AddMb;
    procedure AddMd;
    procedure AddMdq;
    procedure AddMd_q;
    procedure AddMp;
    procedure AddMq;
    procedure AddMs;
    procedure AddMw_Rv;
    procedure AddOb;
    procedure AddOv;
    procedure AddPd_q;
    procedure AddPq;
    procedure AddPRq;
    procedure AddQd;
    procedure AddQq;
    procedure AddRd_q;
    procedure AddSw;
    procedure AddVdq;
    procedure AddVdq_sd;
    procedure AddVdq_ss;
    procedure AddVd_q;
    procedure AddVpd;
    procedure AddVps;
    procedure AddVq;
    procedure AddVRdq;
    procedure AddVRpd;
    procedure AddVRps;
    procedure AddVRq;
    procedure AddVsd;
    procedure AddVss;
    procedure AddWdq;
    procedure AddWpd;
    procedure AddWps;
    procedure AddWq;
    procedure AddWsd;
    procedure AddWss;
    {$ifdef verbose_string_instructions}
    procedure AddXb;
    procedure AddXv;
    procedure AddXz;
    procedure AddYb;
    procedure AddYv;
    procedure AddYz;
    {$endif}
    //---

    procedure AddModReg;
    procedure AddModReg(AType: TRegisterType);
    procedure AddModReg(AType: TRegisterType; ASize: TOperandSize);
    procedure AddModRM(AReqTypes: TModRMTypes; ASize: TOperandSize; AType: TRegisterType);
    procedure AddOperand(const AValue: String; ASize: TOperandSize; AByteCount: Byte=0; AFormatFlags: THexValueFormatFlags=[]; AFlags: TOperandFlags=[]; AByteCount2: Byte=0);
    procedure AddOperand(const AValue: String; AByteCount: Byte=0;  AFormatFlags: THexValueFormatFlags=[]; AFlags: TOperandFlags=[]);
    procedure AddStdOperands(AIndex: Byte);
    procedure AddStdReg(AIndex: Byte);
    procedure AddStdReg(AIndex: Byte; AType: TRegisterType);

    procedure Check32;
    procedure Check64;
    procedure CheckLock;
    procedure CheckRepeat;
    procedure CheckRepeatX;
    procedure CheckSIMD;

    procedure Do2ByteOpcode;
    procedure Do3DNow;
    procedure DoDisassemble;
    procedure DoGroup1;
    procedure DoGroup2;
    procedure DoGroup3;
    procedure DoGroup4;
    procedure DoGroup5;
    procedure DoGroup6;
    procedure DoGroup7;
    procedure DoGroup8;
    procedure DoGroup9;
    procedure DoGroup10;
    procedure DoGroup11;
    procedure DoGroup12;
    procedure DoGroup13;
    procedure DoGroup14;
    procedure DoGroup15;
    procedure DoGroup16;
    procedure DoGroupP;
    procedure DoX87;

    function AddressSize32: TAddressSize;
    function DecodePrefix(AOpcode, AOpcode66, AOpcodeF2, AOpcodeF3: TOpCode): TSimdOpcode;
    procedure Default64;
    procedure Force64;
    function Ignore64(s: String): String;
    function OperandSize: TOperandSize;
    function SizeReg32(const AReg: String): String;
    function SizeReg32(const AReg: String; ASize: TOperandSize): String;
    procedure StdCond(AIndex: Byte);
    function StdReg(AIndex: Byte; AType: TRegisterType; AExtReg: Boolean): String;
    function StdReg(AIndex: Byte): String;
  public
    procedure Disassemble(AMode: TFPDMode; var AAddress: Pointer; out AnInstruction: TInstruction);
  end;

  TX86AsmDecoder = class;

  { TX86AsmInstruction }

  TX86AsmInstruction = class(TDbgAsmInstruction)
  const
    INSTR_CODEBIN_LEN = 16;
  private
    FProcess: TDbgProcess;
    FAddress: TDBGPtr;
    FCodeBin: array[0..INSTR_CODEBIN_LEN-1] of byte;
    FInstruction: TInstruction;
    FInstrLen: Integer;
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
    //function ModifiesBasePointer: boolean; override;
    function ModifiesStackPointer: boolean; override;
    function IsJumpInstruction(IncludeConditional: Boolean = True; IncludeUncoditional: Boolean = True): boolean; override;
    function InstructionLength: Integer; override;
    function X86OpCode: TOpCode;
    property X86Instruction: TInstruction read FInstruction; // only valid after call to X86OpCode
  end;

  { TX86AsmDecoder }

  TX86AsmDecoder = class(TDbgAsmDecoder)
  private const
    MAX_CODEBIN_LEN = 50;
    //FMaxInstructionSize = 16;
    //FMinInstructionSize = 1;
  private
    FProcess: TDbgProcess;
    FLastErrWasMem: Boolean;
    FCodeBin: array[0..MAX_CODEBIN_LEN-1] of byte;
    FLastInstr: TX86AsmInstruction;
  protected
    function GetLastErrorWasMemReadErr: Boolean; override;
    function GetMaxInstrSize: integer; override;
    function GetMinInstrSize: integer; override;
    function GetCanReverseDisassemble: boolean; override;
    function ReadCodeAt(AnAddress: TDBGPtr; var ALen: Cardinal): Boolean; inline;
  public
    constructor Create(AProcess: TDbgProcess); override;
    destructor Destroy; override;

    procedure Disassemble(var AAddress: Pointer; out ACodeBytes: String; out ACode: String); override;
    function GetInstructionInfo(AnAddress: TDBGPtr): TDbgAsmInstruction; override;

    function GetFunctionFrameInfo(AnAddress: TDBGPtr; out
      AnIsOutsideFrame: Boolean): Boolean; override;
  end;

implementation
var
  DBG_WARNINGS: PLazLoggerLogGroup;

const
  ADDRESS_BYTES: array[TAddressSize] of Byte = (2, 4, 8);
  OPERAND_BYTES: array[TOperandSize] of Byte = (1, 2, 4, 8, 6, 10, 16);
  OPERAND_REG: array[os8..os64] of TRegisterType = (reg8, reg16, reg32, reg64);
  STD_REGS = [reg8..reg64];
  ADDRESS_REG: array[TAddressSize] of TRegisterType = (reg16, reg32, reg64);

  // reg8, reg16, reg32, reg64, regMmx, regXmm, regSegment, regControl, regDebug, regX87
  REGISTER_SIZE: array[TFPDMode, reg8..High(TRegisterType)] of TOperandSize = (
    { dm32 } (os8, os16, os32, os64, os64, os128, os16, os32, os32, os80),
    { dm64 } (os8, os16, os32, os64, os64, os128, os16, os64, os64, os80)
  );



  OPCODE_NAME: array [TOpCode] of String = (
       '???', // OPX_InternalUnknown
       '???',
      '**x87**', '-x87-', '!x87!',
      '-3dnow-',
      '**group1a**', '!group1!',
      '!group2!',
      '!group3!',
      '**group4**', '!group4!',
      '**group5**', '!group5!',
      '**group6**', '!group6!',
      '**group7**', '!group7!',
      '**group8**', '!group8!',
      '**group9**', '!group9!',
      '**group10**', '!group10!',
      '**group11**', '!group11!',
      '**group12**', '!group12!',
      '**group13**', '!group13!',
      '**group14**', '!group14!',
      '**group15**', '!group15!',
      '**group16**', '!group16!',
      '**groupp**', '!groupp!',
      '**sysenter**', '**sysexit**',

      'fadd', 'fmul', 'fcom', 'fcomp', 'fsub', 'fsubr', 'fdiv', 'fdivr',
      'fld', 'fxch', 'fst', 'fstp', 'fldenv', 'fldcw', 'fnstenv', 'fnstcw',
      'fchs', 'fabs', 'ftst', 'fxam', 'fld1', 'fldl2t', 'fldl2e', 'fldpi', 'fldlg2', 'fldln2', 'fldz',
      'f2xm1', 'fyl2x', 'fptan', 'fpatan', 'fxtract', 'fprem1', 'fdecstp',
      'fincstp', 'fprem', 'fyl2xp1', 'fsqrt', 'fsincos', 'frndint',
      'fscale', 'fsin', 'fcos', 'nop', 'fiadd', 'fimull',
      'ficom', 'ficomp', 'fisub', 'fisubr', 'fidiv', 'fidivr',
      'fcmovb', 'fcmove', 'fcmovbe', 'fcmovu', 'fucompp',
      'fild', 'fisttp', 'fist', 'fistp', 'fcmovnb', 'fcmovne', 'fcmovnbe', 'fcmovnu',
      'fucomi', 'fcomi', 'fnclex', 'fninit', 'frstor', 'fnsave', 'fnstsw',
      'ffree', 'fucomp', 'faddp', 'fmullp', 'fsubrp', 'fsubp', 'fdivrp',
      'fdivp', 'fbld', 'fbstp', 'fcompp', 'fucomip', 'fcomip',

      'pi2fw', 'pi2fd', 'pf2iw', 'pf2id', 'pfnacc', 'pfpnacc', 'pfcmpge',
      'pfmin', 'pfrcp', 'pfrsqrt', 'pfsub', 'pfadd', 'pgcmpgt', 'pfmax',
      'pfrcpit1', 'pfrsqit1', 'pfsubr', 'pfacc', 'pfcmpeq', 'pfmul',
      'pfrcpit2', 'pmulhrw', 'pswapd', 'pavgusb',

      'add', 'or', 'adc', 'sbb', 'and', 'sub', 'xor', 'cmp','pop',
      'rol', 'ror', 'rcl', 'rcr', 'shl', 'shr', 'sal', 'sar',
      'test', 'not', 'neg', 'mul', 'imul', 'div', 'idiv',
      'inc', 'dec',
      'call', 'jmp', 'push',
      'sldt', 'str', 'lldt', 'ltr', 'verr', 'verw',
      'vmrun', 'vmmcall', 'vmload', 'vmsave', 'stgi', 'clgi', 'skinit', 'invlpga',
      'sgdt', 'sidt', 'lgdt', 'lidt', 'smsw', 'lmsw', 'swapgs', 'rdtscp', 'invlpg',
      'bts', 'btr', 'btc', 'bt',
      'cmpxchg16b', 'cmpxchg8b',

      'mov', 'psrlw', 'psraw', 'psllw', 'psrld', 'psrad', 'pslld', 'psrlq', 'psrldq', 'psllq',
      'fxsave', 'fxrstor', 'ldmxcsr', 'stmxcsr', 'lfence', 'mfence', 'clflush',
      'prefetch exclusive', 'prefetch modified', '--prefetch--',

      'punpcklbw', 'punpcklwd', 'punpcklqd', 'packsswb', 'pcmpgtb', 'pcmpgtw', 'pcmpgtd',
      'packuswb', 'punpkhbw', 'punpkhwd', 'punpkhdq', 'packssdw', 'punpcklqdq', 'punpckhqdq',
      'paddq', 'pmullw', 'psubusb', 'psubusw', 'pminub', 'pand', 'paddusb', 'paddusw', 'pmaxub', 'pandn',
      'pavgb', 'pavgw', 'pmulhuw', 'pmulhw', 'psubsb', 'psubsw', 'pminsw',
      'por', 'paddsb', 'paddsw', 'pmaxsw', 'pxor', 'pmuludq', 'pmaddwd',
      'psadbw', 'psubb', 'psubw', 'psubd', 'psubq', 'paddb', 'paddw', 'paddd',

      'lar', 'lsl', 'syscall', 'clts', 'sysret', 'invd', 'wbinvd', 'ud2',
      'femms', 'movups', 'movss', 'movupd', 'movsd', 'movhlps',
      'movsldup', 'movlpd', 'movddup', 'movlps', 'unpcklps', 'unpcklpd', 'unpckhps', 'unpckhpd', 'movlhps',
      'movshdup', 'movhpd', 'movhps', 'movaps', 'movapd',
      'cvtpi2ps', 'cvtsi2ss', 'cvtpi2pd', 'cvtsi2sd', 'movntps', 'movntpd',
      'cvttps2pi', 'cvttss2si', 'cvttpd2pi', 'cvttsd2si', 'cvtps2pi', 'cvtss2si', 'cvtpd2pi', 'cvtsd2si',
      'ucomiss', 'ucomissd', 'comiss', 'comissd', 'wrmsr', 'rdtsc', 'rdmsr', 'rdpmc',

      'cmov', 'movmskps', 'movmskpd', 'sqrtps', 'sqrtss', 'sqrtpd', 'sqrtsd',
      'addps', 'addss', 'addpd', 'addsd', 'mulps', 'mulss', 'mulpd', 'mulsd',
      'subps', 'subss', 'subpd', 'subsd', 'minps', 'minss', 'minpd', 'minsd',
      'divps', 'divss', 'divpd', 'divsd', 'maxps', 'maxss', 'maxpd', 'maxsd',
      'rsqrtps', 'rsqrtss', 'rcpps', 'rcpss', 'andps', 'andpd', 'andnps', 'andnpd',
      'orps', 'orpd', 'xorps', 'xorpd',
      'cvtps2pd', 'cvtss2sd', 'cvtpd2ps', 'cvtsd2ss', 'cvtdq2ps', 'cvttps2dq', 'cvtps2dq',
      'movd', 'movq', 'movdqu', 'movdqa', 'pshufw', 'pshufhw', 'pshufd', 'pshuflw',
      'pcmpeqb', 'pcmpeqw', 'pcmpeqd', 'emms', 'haddpd', 'haddps', 'hsubpd', 'hsubps',
      'j', // cond jump + suffix
      'set', 'cpuid', 'shld', 'rsm', 'shrd',
      'cmpxchg', 'lss', 'lfs', 'lgs', 'movzx', 'bsf', 'bsr', 'movsx', 'xadd',
      'cmpps', 'cmpss', 'cmppd', 'cmpsd', 'movnti', 'pinsrw', 'pextrw', 'shufps', 'shufpd', 'bswp',
      'addsubpd', 'addsubps', 'movq2dq', 'movdq2q', 'pmovmskb', 'cvtdq2pd', 'cvttpd2dq', 'cvtpd2dq',
      'movntq', 'movntdqu', 'lddqu', 'maskmovq', 'maskmovdqu',
      'daa', 'das', 'aaa', 'aas', 'pusha', 'pushad', 'popa', 'popad', 'bound', 'movsxd',
      'arpl', 'insb', 'insw', 'insd', 'outsb', 'outsw', 'outsd',
      'xchg', 'lea', 'cdqe', 'cwde', 'cbw', 'cqo', 'cqd', 'cwd',
      'wait/fwait',
      'pushfq', 'pushfd', 'pushf', 'popfq', 'popfd', 'popf', 'sahf', 'lahf',
      'movsb', 'movsq', 'movsw', 'cmpsb', 'cmpsq', 'cmpsw',
      'stosb', 'stosq', 'stosd', 'stosw', 'lodsb', 'lodsq', 'lodsd', 'lodsw',
      'scasb', 'scasq', 'scasd', 'scasw', 'ret', 'les', 'lds',
      'enter', 'leave', 'retf', 'int3', 'int', 'int0', 'iretq', 'iretd', 'iret',
      'aam', 'aad', 'salc', 'xlat', 'loopne', 'loope', 'loop', 'jrcxz',
      'in', 'out', 'int1', 'hlt', 'cmc', 'clc', 'stc', 'cli', 'sti', 'cld', 'std'
  );
  OPCODE_SUFFIX_NAME: array [TOpCodeSuffix] of String = (
    '',
    'o', 'no', 'b', 'nb', 'z', 'nz', 'be', 'nbe', 's', 'ns', 'p', 'np', 'l', 'nl', 'le', 'nle'
  );

{ TX86Disassembler }

procedure TX86Disassembler.Check32;
begin
  // only valid in 32-bit ProcessMode
  if (ProcessMode = dm64) then
    Include(Instruction^.Flags, ifOnly32);
end;

procedure TX86Disassembler.Check64;
begin
  // only valid in 64-bit ProcessMode
  if (ProcessMode = dm64) then
    Include(Instruction^.Flags, ifOnly64);
end;

function TX86Disassembler.Ignore64(s: String): String;
begin
  // ignored in 64-bit ProcessMode
  if (ProcessMode = dm64) then
    Result := '('+s+')'
  else
    Result := s;
end;

procedure TX86Disassembler.Default64;
begin
  if ProcessMode = dm64 then
    Include(Flags, oprDefault64);
end;

procedure TX86Disassembler.Force64;
begin
  if ProcessMode = dm64 then
    Include(Flags, oprForce64);
end;


procedure TX86Disassembler.CheckLock;
  function CheckMem: boolean;
  var
    n: Byte;
  begin
    Result := True;
    for n := 1 to OperIdx do
      if ofMemory in Instruction^.Operand[n].Flags then Exit;
    Result := False;
  end;
begin
  if (preLock in Flags) and CheckMem
  then begin
    Exclude(Flags, preLock);
    Include(Instruction^.Flags, ifPrefixLock);
  end;
end;

procedure TX86Disassembler.CheckRepeat;
begin
  if preF3 in Flags
  then begin
    Exclude(Flags, preF3);
    Include(Instruction^.Flags, ifPrefixRep);
  end;
end;

procedure TX86Disassembler.CheckRepeatX;
begin
  if preF3 in Flags
  then begin
    Exclude(Flags, preF3);
    Include(Instruction^.Flags, ifPrefixRepE);
    Exit;
  end;
  if preF2 in Flags
  then begin
    Exclude(Flags, preF2);
    Include(Instruction^.Flags, ifPrefixRepNe);
    Exit;
  end;
end;

procedure TX86Disassembler.CheckSIMD;
var
  check: TFlags;
begin
  check := Flags * [pre66, preF3, preF2];
  if check = []
  then SimdOpcode := soNone
  else if check - [preF3] = []
  then SimdOpcode := soF3
  else if check - [preF2] = []
  then SimdOpcode := soF2
  else if check - [pre66] = []
  then SimdOpcode := so66
  else SimdOpcode := soInvalid;
end;

function TX86Disassembler.DecodePrefix(AOpcode, AOpcode66, AOpcodeF2, AOpcodeF3: TOpCode): TSimdOpcode;
begin
  CheckSIMD;

  case SimdOpcode of
    soNone: Instruction^.Opcode := AOpcode;
    so66:   Instruction^.Opcode := AOpcode66;
    soF2:   Instruction^.Opcode := AOpcodeF2;
    soF3:   Instruction^.Opcode := AOpcodeF3;
  else
    Instruction^.Opcode := OPX_Invalid;
  end;

  if Instruction^.Opcode = OPX_Invalid
  then begin
    Result := soInvalid;
  end
  else begin
    Flags := Flags - [pre66, preF2, preF3];
    Result := SimdOpcode;
  end;
end;

function TX86Disassembler.AddressSize32: TAddressSize;
begin
  // effective address size for default 32 AnInstruction.operand size
  if (ProcessMode = dm64)
  then begin
    if preAdr in Flags
    then Result := as32
    else Result := as64;
  end
  else begin
    if preAdr in Flags
    then Result := as16
    else Result := as32;
  end;
end;

function TX86Disassembler.OperandSize: TOperandSize;
begin
  // effective AnInstruction.operand size

  // Intel(r) 64 and IA-32 Architectures Software Developer’s Manual Volume 1:
  // 3.6 OPERAND-SIZE AND ADDRESS-SIZE ATTRIBUTES
  //
  // Table 3-3 D-flag = 1 for 32 bit processes ->
  // default 32, prefix 16
  //
  // Table 3-4
  // REX.W 64, default 32, prefix 16 (REX.W overrules prefix)
  //
  // So for both dm32 and dm64 the default size is 32 unless overridden by flags

  // A.3 ONE, TWO, AND THREE-BYTE Instruction^.Opcode MAPS
  // Some instructions default or force to 64bit in dm64

  if [oprForce64, rexW] * Flags <> []
  then begin
    Result := os64;
  end
  else begin
    if pre66 in Flags
    then Result := os16
    else if oprDefault64 in Flags
    then Result := os64
    else Result := os32;
  end;
end;

procedure TX86Disassembler.AddOperand(const AValue: String; ASize: TOperandSize; AByteCount: Byte = 0; AFormatFlags: THexValueFormatFlags = []; AFlags: TOperandFlags = []; AByteCount2: Byte = 0);
begin
  Inc(OperIdx);
  if OperIdx > High(Instruction^.Operand)
  then begin
    Debugln(DBG_WARNINGS, 'AddOperand: Only %d operands supported, got %d', [High(Instruction^.Operand), OperIdx]);
    Exit;
  end;

  Instruction^.Operand[OperIdx].Size := ASize;
  Instruction^.Operand[OperIdx].ByteCount := AByteCount;
  Instruction^.Operand[OperIdx].ByteCount2 := AByteCount2;
  Instruction^.Operand[OperIdx].FormatFlags := AFormatFlags;
  Instruction^.Operand[OperIdx].Value := AValue;
  Instruction^.Operand[OperIdx].Flags := AFlags;
end;

procedure TX86Disassembler.AddOperand(const AValue: String; AByteCount: Byte = 0; AFormatFlags: THexValueFormatFlags = []; AFlags: TOperandFlags = []);
begin
  AddOperand(AValue, OperandSize, AByteCount, AFormatFlags, AFlags);
end;

function TX86Disassembler.SizeReg32(const AReg: String; ASize: TOperandSize): String;
begin
  // prefix a reg for default 32 AnInstruction.operand size
  case ASize of
    os64: Result := 'r' + AReg;
    os32: Result := 'e' + AReg;
  else
    Result := AReg;
  end;
end;

function TX86Disassembler.SizeReg32(const AReg: String): String;
begin
  Result := SizeReg32(AReg, OperandSize);
end;

procedure TX86Disassembler.StdCond(AIndex: Byte);
const
  COND: array[0..$F] of TOpCodeSuffix = (
    OPSx_o, OPSx_no, OPSx_b, OPSx_nb, OPSx_z, OPSx_nz, OPSx_be, OPSx_nbe, OPSx_s, OPSx_ns, OPSx_p, OPSx_np, OPSx_l, OPSx_nl, OPSx_le, OPSx_nle
  );
begin
  Instruction^.OpCodeSuffix := COND[AIndex and $F];
end;

function TX86Disassembler.StdReg(AIndex: Byte; AType: TRegisterType; AExtReg: Boolean): String;
const
  REGS: array[0..7] of string = ('ax', 'cx', 'dx', 'bx', 'sp', 'bp', 'si', 'di');
  REG8_: array[0..7] of String = ('al', 'cl', 'dl', 'bl', 'ah', 'ch', 'dh', 'bh');
  REG8r: array[0..7] of String = ('al', 'cl', 'dl', 'bl', 'spl', 'bpl', 'sil', 'dil');
  SREG: array[0..7] of String = ('es', 'cs', 'ss', 'ds', 'fs', 'gs', '**', '**');
  POSTFIX: array[reg16..reg64] of String = ('w', 'd', '');
  OSMAP: array[reg8..reg64] of TOperandSize = (os8, os16, os32, os64);
begin
  AIndex := AIndex and $7;
  case AType of
    reg8: begin
      if AExtReg
      then begin
        Result := Format('r%db', [8 + AIndex]);
      end
      else begin
        if flagRex in Flags
        then Result := REG8r[AIndex]
        else Result := REG8_[AIndex];
      end;
    end;
    reg16..reg64: begin
      if AExtReg
      then Result := Format('r%d', [8 + AIndex]) + POSTFIX[AType]
      else Result := SizeReg32(REGS[AIndex], OSMAP[AType]);
    end;
    regX87: begin
      Result := Format('st(%d)', [AIndex]);
    end;
    regMmx: begin
      Result := Format('mmx%d', [AIndex]);
    end;
    regXmm: begin
      if AExtReg then Inc(AIndex, 8);
      Result := Format('xmm%d', [AIndex]);
    end;
    regSegment: begin
      Result := SREG[AIndex];
    end;
    regControl: begin
      if AExtReg then Inc(AIndex, 8);
      Result := Format('cr%d', [AIndex]);
    end;
    regDebug: begin
      if AExtReg then Inc(AIndex, 8);
      Result := Format('dr%d', [AIndex]);
    end;
  end;
end;

function TX86Disassembler.StdReg(AIndex: Byte): String;
begin
  Result := StdReg(AIndex, OPERAND_REG[OperandSize], rexB in Flags);
end;

procedure TX86Disassembler.AddStdReg(AIndex: Byte; AType: TRegisterType);
begin
  AddOperand(StdReg(AIndex, AType, rexB in Flags), REGISTER_SIZE[ProcessMode, AType]);
end;

procedure TX86Disassembler.AddStdReg(AIndex: Byte);
begin
  AddOperand(StdReg(AIndex));
end;

procedure TX86Disassembler.AddModReg(AType: TRegisterType; ASize: TOperandSize);
begin
  Include(Flags, flagModRM);
  AddOperand(StdReg(Code[ModRMIdx] shr 3, AType, rexR in Flags), ASize);
end;

procedure TX86Disassembler.AddModReg(AType: TRegisterType);
begin
  Include(Flags, flagModRM);
  AddOperand(StdReg(Code[ModRMIdx] shr 3, AType, rexR in Flags), REGISTER_SIZE[ProcessMode, AType]);
end;

procedure TX86Disassembler.AddModReg;
begin
  Include(Flags, flagModRM);
  AddOperand(StdReg(Code[ModRMIdx] shr 3, OPERAND_REG[OperandSize], rexR in Flags));
end;

procedure TX86Disassembler.AddModRM(AReqTypes: TModRMTypes; ASize: TOperandSize; AType: TRegisterType);
var
  Mode, Rm: Byte;
  procedure Mem16;
  const
    REGS16: array[0..7] of string = ('bx+si', 'bx+di', 'bp+si', 'bp+di', 'si', 'di', 'bp', 'bx');
  begin
    case Mode of
      0: begin
        if rm = 6 // disp16 -> exception to the regs
        then AddOperand('%s', ASize, 2, [hvfSigned, hvfIncludeHexchar], [ofMemory])
        else AddOperand(REGS16[rm], ASize, 0, [], [ofMemory]);
      end;
      1: AddOperand(REGS16[rm] + '%s', ASize, 1, [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar], [ofMemory]);
      2: AddOperand(REGS16[rm] + '%s', ASize, 2, [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar], [ofMemory]);
    end;
  end;

var
  AddrSize: TAddressSize;
  Sib: record
    Scale, Index, Base: Byte;
  end;
  Oper: record
    Size: Byte;
    Flags: THexValueFormatFlags;
    Value: String;
  end;
begin
  Include(Flags, flagModRM);
  Mode := Code[ModRMIdx] shr 6;
  Rm := Code[ModRMIdx] and $7;

  // Check for reg (ProcessMode = 3) first;
  if Mode = 3
  then begin
    if modReg in AReqTypes
    then AddStdReg(rm, AType)
    else AddOperand('**');
    Exit;
  end;

  // Check if mem is allowed
  if not (modMem in AReqTypes)
  then begin
    AddOperand('**', 0, [], [ofMemory]);
    Exit;
  end;

  Oper.Size := 0;
  Oper.Flags := [];
  Oper.Value := '';

  // Here only mem access
  AddrSize := AddressSize32;
  if AddrSize = as16
  then begin
    Mem16;
    Exit;
  end;

  if rm = 4
  then begin
    // sib folows
    Include(Flags, flagSib);
    sib.Scale := Code[ModRMIdx+1] shr 6;
    sib.Index := (Code[ModRMIdx+1] shr 3) and $7;
    sib.Base := Code[ModRMIdx+1] and $7;

    // base
    if (Mode = 0) and (sib.Base = 5)
    then begin
      // disp32
      Oper.Value := '%s';
      Oper.Size := 4;
      if (sib.Index <> 4) or (rexX in Flags)
      then Oper.Flags := [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar] // [reg + base]
      else Oper.Flags := [hvfSigned, hvfIncludeHexchar];                   // [base]
    end
    else begin
      Oper.Value := StdReg(sib.Base, ADDRESS_REG[AddrSize], rexB in Flags);
      if (sib.Index <> 4) or (rexX in Flags)
      then Oper.Value := '+' + Oper.Value;  // [reg + base]
    end;

    // reg
    if (rexX in Flags) or (sib.Index <> 4)
    then begin
      if sib.Scale > 0
      then Oper.Value := Format('*%u', [1 shl sib.Scale]) + Oper.Value;

      // get index
      Oper.Value := StdReg(sib.Index, ADDRESS_REG[AddrSize], rexX in Flags) + Oper.Value
    end;
  end
  else begin
    // no sib
    Oper.Value := StdReg(rm, ADDRESS_REG[AddrSize], rexB in Flags);
  end;

  case Mode of
    0: begin
      // exceptions to std encoding
      if rm = 5
      then begin
        // disp32
        if AddrSize = as64
        then begin
          Oper.Value := 'rip%s';
          Oper.Flags := [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar];
        end
        else begin
          Oper.Value := '%s';
          Oper.Flags := [hvfSigned, hvfIncludeHexchar];
        end;
        Oper.Size := 4;
      end;
    end;
    1: begin
      Oper.Value := Oper.Value + '%s';
      Oper.Size := 1;
      Oper.Flags := [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar];
    end;
    2: begin
      Oper.Value := Oper.Value + '%s';
      Oper.Size := 4;
      Oper.Flags := [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar];
    end;
  end;
  AddOperand(Oper.Value, ASize, Oper.Size, Oper.Flags, [ofMemory]);
end;

procedure TX86Disassembler.AddAp;
begin
  if OperandSize = os16 //XXXX:XXXX
  then AddOperand('$%1:s:%0:s', os32, 2, [], [], 2)
  else AddOperand('$%1:s:%0:s', os48, 4, [], [], 2)
end;

procedure TX86Disassembler.AddCd_q;
begin
  AddModReg(regControl);
end;

procedure TX86Disassembler.AddDd_q;
begin
  AddModReg(regDebug);
end;

procedure TX86Disassembler.AddEb;
begin
  AddModRM([modReg, modMem], os8, reg8);
end;

procedure TX86Disassembler.AddEd;
begin
  AddModRM([modReg, modMem], os32, reg32);
end;

procedure TX86Disassembler.AddEd_q;
begin
  if flagRex in Flags
  then AddModRM([modReg, modMem], os64, reg64)
  else AddModRM([modReg, modMem], os32, reg32);
end;

procedure TX86Disassembler.AddEv;
begin
  AddModRM([modReg, modMem], OperandSize, OPERAND_REG[OperandSize]);
end;

procedure TX86Disassembler.AddEw;
begin
  AddModRM([modReg, modMem], os16, reg16);
end;

procedure TX86Disassembler.AddFv;
begin
  case OperandSize of
    os64: AddOperand('rflags');
    os32: AddOperand('eflags');
  else
    AddOperand('flags');
  end;
end;

procedure TX86Disassembler.AddGb;
begin
  AddModReg(reg8);
end;

procedure TX86Disassembler.AddGd;
begin
  AddModReg(reg32);
end;

procedure TX86Disassembler.AddGd_q;
begin
  if flagRex in Flags
  then AddModReg(reg64)
  else AddModReg(reg32);
end;

procedure TX86Disassembler.AddGv;
begin
  AddModReg;
end;

procedure TX86Disassembler.AddGw;
begin
  AddModReg(reg16);
end;

procedure TX86Disassembler.AddGz;
begin
  if OperandSize = os16
  then AddModReg(reg16)
  else AddModReg(reg32);
end;

procedure TX86Disassembler.AddIb;
begin
  AddOperand('%s', os8, 1, [hvfIncludeHexchar]);
end;

procedure TX86Disassembler.AddIv;
begin
  AddOperand('%s', OPERAND_BYTES[OperandSize], [hvfIncludeHexchar]);
end;

procedure TX86Disassembler.AddIw;
begin
  AddOperand('%s', os16, 2, [hvfIncludeHexchar]);
end;

procedure TX86Disassembler.AddIz;
begin
  if OperandSize = os16
  then AddOperand('%s', os16, 2, [hvfIncludeHexchar])
  else AddOperand('%s', os32, 4, [hvfIncludeHexchar]);
end;

procedure TX86Disassembler.AddJb;
begin
  AddOperand('%s', os8, 1, [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar]);
end;

procedure TX86Disassembler.AddJz;
begin
  if OperandSize = os16
  then AddOperand('%s', os16, 2, [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar])
  else AddOperand('%s', os32, 4, [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar]);
end;

procedure TX86Disassembler.AddM;
begin
  AddModRM([modMem], OperandSize, reg0 {do not care});
end;

procedure TX86Disassembler.AddMa;
begin
  AddModRM([modMem], OperandSize, reg0 {do not care});
end;

procedure TX86Disassembler.AddMb;
begin
  AddModRM([modMem], os8, reg0 {do not care});
end;

procedure TX86Disassembler.AddMd;
begin
  AddModRM([modMem], os32, reg0 {do not care});
end;

procedure TX86Disassembler.AddMd_q;
begin
  if flagRex in Flags
  then AddModRM([modMem], os64, reg0 {do not care})
  else AddModRM([modMem], os32, reg0 {do not care});
end;

procedure TX86Disassembler.AddMdq;
begin
  AddModRM([modMem], os128, reg0 {do not care})
end;

procedure TX86Disassembler.AddMp;
begin
  if OperandSize = os16 //XXXX:XXXX
  then AddModRM([modMem], os32, reg0 {do not care})
  else AddModRM([modMem], os48, reg0 {do not care});
end;

procedure TX86Disassembler.AddMq;
begin
  AddModRM([modMem], os64, reg0 {do not care});
end;

procedure TX86Disassembler.AddMs;
begin
  if (ProcessMode = dm64)
  then AddModRM([modMem], os80, reg0 {do not care})
  else AddModRM([modMem], os48, reg0 {do not care});
end;

procedure TX86Disassembler.AddMw_Rv;
begin
  if Code[ModRMIdx] shr 6 = 3 // ProcessMode = 3 -> reg
  then AddModRM([modReg], OperandSize, OPERAND_REG[OperandSize])
  else AddModRM([modMem], os16, reg0 {do not care});
end;

procedure TX86Disassembler.AddOb;
begin
  AddOperand('%s', os8, ADDRESS_BYTES[AddressSize32], [hvfIncludeHexchar], [ofMemory])
end;

procedure TX86Disassembler.AddOv;
begin
  AddOperand('%s', ADDRESS_BYTES[AddressSize32], [hvfIncludeHexchar], [ofMemory])
end;

procedure TX86Disassembler.AddPd_q;
begin
  if flagRex in Flags
  then AddModReg(regMmx, os64)
  else AddModReg(regMmx, os32);
end;

procedure TX86Disassembler.AddPq;
begin
  AddModReg(regMmx);
end;

procedure TX86Disassembler.AddPRq;
begin
  AddModRM([modReg], os64, regMmx);
end;

procedure TX86Disassembler.AddQd;
begin
  AddModRM([modReg, modMem], os32, regMmx);
end;

procedure TX86Disassembler.AddQq;
begin
  AddModRM([modReg, modMem], os64, regMmx);
end;

procedure TX86Disassembler.AddRd_q;
begin
  if (ProcessMode = dm64)
  then AddModRM([modReg], os64, reg64)
  else AddModRM([modReg], os32, reg32);
end;

procedure TX86Disassembler.AddSw;
begin
  AddModReg(regSegment);
end;

procedure TX86Disassembler.AddVd_q;
begin
  if flagRex in Flags
  then AddModReg(regXmm, os64)
  else AddModReg(regXmm, os32);
end;

procedure TX86Disassembler.AddVdq;
begin
  AddModReg(regXmm, os128);
end;

procedure TX86Disassembler.AddVdq_sd;
begin
  AddModReg(regXmm, os64); // only lower 64 bit
end;

procedure TX86Disassembler.AddVdq_ss;
begin
  AddModReg(regXmm, os32); // only lower 32 bit
end;

procedure TX86Disassembler.AddVpd;
begin
  AddModReg(regXmm, os128);
end;

procedure TX86Disassembler.AddVps;
begin
  AddModReg(regXmm, os128);
end;

procedure TX86Disassembler.AddVq;
begin
  AddModReg(regXmm, os64);
end;

procedure TX86Disassembler.AddVsd;
begin
  AddModReg(regXmm, os64);
end;

procedure TX86Disassembler.AddVss;
begin
  AddModReg(regXmm, os32);
end;

procedure TX86Disassembler.AddVRdq;
begin
  AddModRM([modReg], os128, regXmm);
end;

procedure TX86Disassembler.AddVRpd;
begin
  AddModRM([modReg], os128, regXmm);
end;

procedure TX86Disassembler.AddVRps;
begin
  AddModRM([modReg], os128, regXmm);
end;

procedure TX86Disassembler.AddVRq;
begin
  AddModRM([modReg], os64, regXmm);
end;

procedure TX86Disassembler.AddWdq;
begin
  AddModRM([modReg, modMem], os128, regXmm);
end;

procedure TX86Disassembler.AddWpd;
begin
  AddModRM([modReg, modMem], os128, regXmm);
end;

procedure TX86Disassembler.AddWps;
begin
  AddModRM([modReg, modMem], os128, regXmm);
end;

procedure TX86Disassembler.AddWq;
begin
  AddModRM([modReg, modMem], os64, regXmm);
end;

procedure TX86Disassembler.AddWsd;
begin
  AddModRM([modReg, modMem], os64, regXmm);
end;

procedure TX86Disassembler.AddWss;
begin
  AddModRM([modReg, modMem], os32, regXmm);
end;

{$ifdef verbose_string_instructions}
procedure TX86Disassembler.AddXb;
begin
  AddOperand('Xb');
end;

procedure TX86Disassembler.AddXv;
begin
  AddOperand('Xv');
end;

procedure TX86Disassembler.AddXz;
begin
  AddOperand('Xz');
end;

procedure TX86Disassembler.AddYb;
begin
  AddOperand('Yb');
end;

procedure TX86Disassembler.AddYv;
begin
  AddOperand('Yv');
end;

procedure TX86Disassembler.AddYz;
begin
  AddOperand('Yz');
end;
{$endif}

procedure TX86Disassembler.AddStdOperands(AIndex: Byte);
begin
  case AIndex and $7 of
    0: begin AddEb; AddGb; end;
    1: begin AddEv; AddGv; end;
    2: begin AddGb; AddEb; end;
    3: begin AddGv; AddEv; end;
    4: begin AddOperand('al', os8); AddIb; end;
    5: begin AddOperand(SizeReg32('ax')); AddIz; end;
  else
    AddOperand('!!');
  end;
end;

procedure TX86Disassembler.DoX87;
var
  Index: Byte;
  ModRM: Byte;

  procedure AddMem14_28Env;
  begin
    AddModRM([modMem], OperandSize, reg0 {do not care});
  end;

  procedure AddMem98_108Env;
  begin
    AddModRM([modMem], OperandSize, reg0 {do not care});
  end;

  procedure AddMem16;
  begin
    AddModRM([modMem], os16, reg0 {do not care});
  end;

  procedure AddMem32;
  begin
    AddModRM([modMem], os32, reg0 {do not care});
  end;

  procedure AddMem64;
  begin
    AddModRM([modMem], os64, reg0 {do not care});
  end;

  procedure AddMem80;
  begin
    AddModRM([modMem], os80, reg0 {do not care});
  end;

  procedure AddReg0;
  begin
    AddOperand('st(0)', os80);
  end;

  procedure AddRegN;
  begin
    AddOperand(Format('st(%u)', [Code[ModRMIdx] and $7]), os80);
  end;

  procedure DoD8;
  const
    OPC: array[0..7] of TOpCode = (OPfadd, OPfmul, OPfcom, OPfcomp, OPfsub, OPfsubr, OPfdiv, OPfdivr);
  begin
    Instruction^.Opcode := OPC[Index];
    case ModRM of
      $00..$BF: AddMem32
    else
      AddReg0; AddRegN;
    end;
  end;

  procedure DoD9;
  const
    OPC: array[0..7] of TOpCode = (OPfld, OPfxch, OPfst, OPfstp, OPfldenv, OPfldcw, OPfnstenv, OPfnstcw);
    OPCx: array[0..$1F] of TOpCode = (
      OPfchs, OPfabs, OPX_InvalidX87, OPX_InvalidX87, OPftst, OPfxam, OPX_InvalidX87, OPX_InvalidX87,
      OPfld1, OPfldl2t, OPfldl2e, OPfldpi, OPfldlg2, OPfldln2, OPfldz, OPX_InvalidX87,
      OPf2xm1, OPfyl2x, OPfptan, OPfpatan, OPfxtract, OPfprem1, OPfdecstp, OPfincstp,
      OPfprem, OPfyl2xp1, OPfsqrt, OPfsincos, OPfrndint, OPfscale, OPfsin, OPfcos
    );
  begin
    case ModRM of
      $00..$BF: begin
        Instruction^.Opcode := OPC[Index];
        case Index of
          0, 2, 3: AddMem32;
          1: Instruction^.Opcode := OPX_InvalidX87;
          4, 6 : AddMem14_28Env;
          5, 7: AddMem16;
        end;
      end;
      $C0..$CF: begin Instruction^.Opcode := OPC[Index]; AddReg0; AddRegN; end;
      $D0:      begin Instruction^.Opcode := OPnop; end;
      $D8..$DF: begin Instruction^.Opcode := OPX_ReservedX87; end;
      $E0..$E1,
      $E4..$E5,
      $E8..$FF: begin Instruction^.Opcode := OPCx[ModRM and $1F]; end;
    else
      Instruction^.Opcode := OPX_InvalidX87;
    end;
  end;

  procedure DoDA;
  const
    OPC: array[0..7] of TOpCode = (OPfiadd, OPfimull, OPficom, OPficomp, OPfisub, OPfisubr, OPfidiv, OPfidivr);
    OPCx: array[0..3] of TOpCode = (OPfcmovb, OPfcmove, OPfcmovbe, OPfcmovu);
  begin
    case ModRM of
      $00..$BF: begin Instruction^.Opcode := OPC[Index]; AddMem32; end;
      $C0..$DF: begin Instruction^.Opcode := OPCx[Index]; AddReg0; AddRegN; end;
      $E9:      begin Instruction^.Opcode := OPfucompp; end;
    else
      Instruction^.Opcode := OPX_InvalidX87;
    end;
  end;

  procedure DoDB;
  const
    OPC: array[0..7] of TOpCode = (OPfild, OPfisttp, OPfist, OPfistp, OPX_InvalidX87, OPfld, OPX_InvalidX87, OPfstp);
    OPCx: array[0..7] of TOpCode = (OPfcmovnb, OPfcmovne, OPfcmovnbe, OPfcmovnu, OPX_InvalidX87, OPfucomi, OPfcomi, OPX_InvalidX87);
  begin
    case ModRM of
      $00..$BF: begin
        case Index of
          0..3: begin Instruction^.Opcode := OPC[Index]; AddMem32; end;
          5, 7: begin Instruction^.Opcode := OPC[Index]; AddMem80; end;
        else
          Instruction^.Opcode := OPX_InvalidX87;
        end;
      end;
      $C0..$DF,
      $E8..$F7: begin Instruction^.Opcode := OPCx[Index];  AddReg0; AddRegN; end;
      $E0..$E1: begin Instruction^.Opcode := OPX_ReservedX87; end;
      $E2:      begin Instruction^.Opcode := OPfnclex; end;
      $E3:      begin Instruction^.Opcode := OPfninit; end;
      $E4:      begin Instruction^.Opcode := OPX_ReservedX87; end;
    else
      Instruction^.Opcode := OPX_InvalidX87;
    end;
  end;

  procedure DoDC;
  const
    OPC: array[0..7] of TOpCode = (OPfadd, OPfmul, OPfcom, OPfcomp, OPfsub, OPfsubr, OPfdiv, OPfdivr);
    OPCx: array[0..7] of TOpCode = (OPfadd, OPfmul, OPX_InvalidX87, OPX_InvalidX87, OPfsubr, OPfsub, OPfdivr, OPfdiv);
  begin
    case ModRM of
      $00..$BF: begin Instruction^.Opcode := OPC[Index]; AddMem64; end;
      $C0..$CF,
      $E0..$FF: begin Instruction^.Opcode := OPCx[Index]; AddRegN; AddReg0; end;
    else
      Instruction^.Opcode := OPX_ReservedX87;
    end;
  end;

  procedure DoDD;
  const
    OPC: array[0..7] of TOpCode = (OPfld, OPfisttp, OPfst, OPfstp, OPfrstor, OPX_InvalidX87, OPfnsave, OPfnstsw);
    OPCx: array[0..7] of TOpCode = (OPffree, OPX_InvalidX87, OPfst, OPfstp, OPX_InvalidX87, OPfucomp, OPX_InvalidX87, OPX_InvalidX87);
  begin
    case ModRM of
      $00..$BF: begin
        case Index of
          0..3: begin Instruction^.Opcode := OPC[Index]; AddMem64; end;
          4, 6: begin Instruction^.Opcode := OPC[Index]; AddMem98_108Env; end;
          5: Instruction^.Opcode := OPX_InvalidX87;
          7:    begin Instruction^.Opcode := OPC[Index]; AddMem16; end;
        end;
      end;
      $C0..$C7,
      $D0..$DF,
      $E8..$EF: begin Instruction^.Opcode := OPCx[Index]; AddRegN; end;
      $E0..$E7: begin Instruction^.Opcode := OPCx[Index]; AddRegN; AddReg0; end;
      $C8..$CF: Instruction^.Opcode := OPX_ReservedX87;
    else
      Instruction^.Opcode := OPX_InvalidX87;
    end;
  end;

  procedure DoDE;
  const
    OPC: array[0..7] of TOpCode = (OPfiadd, OPfimull, OPficom, OPficomp, OPfisub, OPfisubr, OPfidiv, OPfidivr);
    OPCx: array[0..7] of TOpCode = (OPfaddp, OPfmullp, OPX_InvalidX87, OPX_InvalidX87, OPfsubrp, OPfsubp, OPfdivrp, OPfdivp);
  begin
    case ModRM of
      $00..$BF: begin Instruction^.Opcode := OPC[Index]; AddMem16; end;
      $C0..$CF,
      $E0..$FF: begin Instruction^.Opcode := OPCx[Index]; AddRegN; AddReg0; end;
      $D9:      begin Instruction^.Opcode := OPfcompp; end;
      $D0..$D7: Instruction^.Opcode := OPX_ReservedX87;
    else
      Instruction^.Opcode := OPX_InvalidX87;
    end;
  end;

  procedure DoDF;
  const
    OPC: array[0..7] of TOpCode = (OPfild, OPfisttp, OPfist, OPfistp, OPfbld, OPfild, OPfbstp, OPfistp);
  begin
    case ModRM of
      $00..$BF: begin
        case Index of
          0..3: begin Instruction^.Opcode := OPC[Index]; AddMem16; end;
          4, 6: begin Instruction^.Opcode := OPC[Index]; AddMem80; end;
          5, 7: begin Instruction^.Opcode := OPC[Index]; AddMem64; end;
        end;
      end;
      $E0:      begin Instruction^.Opcode := OPfnstsw;  AddOperand('ax', os16); end;
      $E8..$EF: begin Instruction^.Opcode := OPfucomip; AddReg0; AddRegN; end;
      $F0..$F7: begin Instruction^.Opcode := OPfcomip;  AddReg0; AddRegN; end;
      $C0..$DF: Instruction^.Opcode := OPX_ReservedX87;
    else
      Instruction^.Opcode := OPX_InvalidX87;
    end;
  end;

begin
  Include(Flags, flagModRM);

  ModRM := Code[ModRMIdx];
  Index := (ModRM shr 3) and $7;
  case Code[CodeIdx] of
    $D8: DoD8;
    $D9: DoD9;
    $DA: DoDA;
    $DB: DoDB;
    $DC: DoDC;
    $DD: DoDD;
    $DE: DoDE;
    $DF: DoDF;
  else
    Instruction^.Opcode := OPX_Not87;
  end;
end;

procedure TX86Disassembler.Do3DNow;
var
  n, idx: Byte;
begin
  // 0Fh 0Fh [ModRM] [SIB] [displacement] imm8_opcode
  // sigh, we need to get the operands first, luckely they are all te same.
  AddPq;
  AddQq;
  // to adjust the instruction length, add an empty AnInstruction.operand for the Instruction^.Opcode
  AddOperand('', 1);
  // calc index of imm_opcode
  idx := 0;
  if flagModRM in Flags then Inc(idx);
  if flagSib in Flags then Inc(idx);
  for n := 1 to OperIdx do
  begin
    Inc(idx, Instruction^.Operand[n].ByteCount);
    Inc(idx, Instruction^.Operand[n].ByteCount2);
  end;
  // now we can lookup the Instruction^.Opcode
  case Code[CodeIdx + idx] of
    $0C: Instruction^.Opcode := OPpi2fw;
    $0D: Instruction^.Opcode := OPpi2fd;
    $1C: Instruction^.Opcode := OPpf2iw;
    $1D: Instruction^.Opcode := OPpf2id;
    $8A: Instruction^.Opcode := OPpfnacc;
    $8E: Instruction^.Opcode := OPpfpnacc;
    $90: Instruction^.Opcode := OPpfcmpge;
    $94: Instruction^.Opcode := OPpfmin;
    $96: Instruction^.Opcode := OPpfrcp;
    $97: Instruction^.Opcode := OPpfrsqrt;
    $9A: Instruction^.Opcode := OPpfsub;
    $9E: Instruction^.Opcode := OPpfadd;
    $A0: Instruction^.Opcode := OPpgcmpgt;
    $A4: Instruction^.Opcode := OPpfmax;
    $A6: Instruction^.Opcode := OPpfrcpit1;
    $A7: Instruction^.Opcode := OPpfrsqit1;
    $AA: Instruction^.Opcode := OPpfsubr;
    $AE: Instruction^.Opcode := OPpfacc;
    $B0: Instruction^.Opcode := OPpfcmpeq;
    $B4: Instruction^.Opcode := OPpfmul;
    $B6: Instruction^.Opcode := OPpfrcpit2;
    $B7: Instruction^.Opcode := OPpmulhrw;
    $BB: Instruction^.Opcode := OPpswapd;
    $BF: Instruction^.Opcode := OPpavgusb;
  else
    Instruction^.Opcode := OPX_3dnow;
  end;
end;

procedure TX86Disassembler.DoGroup1;
const
  OPC: array[0..7] of TOpCode = (OPadd, OPor, OPadc, OPsbb, OPand, OPsub, OPxor, OPcmp);
var
  Index: Byte;
begin
  Include(Flags, flagModRM);
  Index := (Code[ModRMIdx] shr 3) and 7;

  // group 1a
  if Code[CodeIdx] = $8F
  then begin
    if Index = 0
    then begin
      Instruction^.Opcode := OPpop;
      AddEv;
    end
    else Instruction^.Opcode := OPX_group1a;
    Exit;
  end;

  // Group 1
  Instruction^.Opcode := OPC[Index];
  case Code[CodeIdx] of
    $80: begin AddEb; AddIb; end;
    $81: begin AddEv; AddIz; end;
    $82: begin AddEb; AddIb; Check32; end;
    $83: begin AddEv; AddIb; end;
  else
    Instruction^.Opcode := OPX_NotGroup1;
    Exit;
  end;
  if (Index <> 7)
  then  CheckLock;
end;

procedure TX86Disassembler.DoGroup2;
const
  OPC: array[0..7] of TOpCode = (OProl, OPror, OPrcl, OPrcr, OPshl, OPshr, OPsal, OPsar);
var
  Index: Byte;
begin
  Include(Flags, flagModRM);
  Index := (Code[ModRMIdx] shr 3) and 7;
  Instruction^.Opcode := OPC[Index];
  case Code[CodeIdx] of
    $C0: begin AddEb; AddIb; end;
    $C1: begin AddEv; AddIb; end;
    $D0: begin AddEb; AddOperand('1', os8); end;
    $D1: begin AddEv; AddOperand('1', os8); end;
    $D2: begin AddEb; AddOperand('cl', os8); end;
    $D3: begin AddEv; AddOperand('cl', os8); end;
  else
    Instruction^.Opcode := OPX_NotGroup2;
  end;
end;

procedure TX86Disassembler.DoGroup3;
const
  OPC: array[0..7] of TOpCode = (OPtest, OPtest, OPnot, OPneg, OPmul, OPimul, OPdiv, OPidiv);
var
  Index: Byte;
begin
  Include(Flags, flagModRM);
  Index := (Code[ModRMIdx] shr 3) and 7;
  Instruction^.Opcode := OPC[Index];
  case Code[CodeIdx] of
    $F6: begin
      if (Index = 0) or (Index = 1)
      then begin
        AddEb; AddIb;
      end
      else begin
        AddEb;
      end;
    end;
    $F7: begin
      if (Index = 0) or (Index = 1)
      then begin
        AddEv; AddIz;
      end
      else begin
        AddEv;
      end;
    end;
  else
    Instruction^.Opcode := OPX_NotGroup3;
    Exit;
  end;
  if (Index = 2) or (Index = 3)
  then CheckLock;
end;

procedure TX86Disassembler.DoGroup4;
var
  Index: Byte;
begin
  Include(Flags, flagModRM);
  if Code[CodeIdx] <> $FE
  then begin
    Instruction^.Opcode := OPX_NotGroup4;
    Exit;
  end;

  Index := (Code[ModRMIdx] shr 3) and 7;
  case Index of
    0: Instruction^.Opcode := OPinc;
    1: Instruction^.Opcode := OPdec;
  else
    Instruction^.Opcode := OPX_Group4;
    Exit;
  end;
  AddEb;
  CheckLock;
end;

procedure TX86Disassembler.DoGroup5;
var
  Index: Byte;
begin
  Include(Flags, flagModRM);
  if Code[CodeIdx] <> $FF
  then begin
    Instruction^.Opcode := OPX_NotGroup5;
    Exit;
  end;

  Index := (Code[ModRMIdx] shr 3) and 7;
  case Index of
    0: begin AddEv; Instruction^.Opcode := OPinc;  end;
    1: begin AddEv; Instruction^.Opcode := OPdec;  end;
    2: begin Force64; AddEv; Instruction^.Opcode := OPcall; end;
    3: begin AddMp; Instruction^.Opcode := OPcall; end;
    4: begin Force64; AddEv; Instruction^.Opcode := OPjmp; end;
    5: begin AddMp; Instruction^.Opcode := OPjmp;  end;
    6: begin Default64; AddEv; Instruction^.Opcode := OPpush; end;
  else
    Instruction^.Opcode := OPX_Group5;
  end;
  if Index in [0,1] then
    CheckLock;
end;

procedure TX86Disassembler.DoGroup6;
var
  Index: Byte;
begin
  Include(Flags, flagModRM);
  if Code[CodeIdx] <> $00
  then begin
    Instruction^.Opcode := OPX_NotGroup5;
    Exit;
  end;
  Index := (Code[ModRMIdx] shr 3) and 7;
  case Index of
    0: begin AddMw_Rv; Instruction^.Opcode := OPsldt; end;
    1: begin AddMw_Rv; Instruction^.Opcode := OPstr;  end;
    2: begin AddEw;    Instruction^.Opcode := OPlldt; end;
    3: begin AddEw;    Instruction^.Opcode := OPltr;  end;
    4: begin AddEw;    Instruction^.Opcode := OPverr; end;
    5: begin AddEw;    Instruction^.Opcode := OPverw; end;
  else
    Instruction^.Opcode := OPX_Group6;
  end;
end;

procedure TX86Disassembler.DoGroup7;
const
  RM3: array [0..7] of TOpCode = (OPvmrun, OPvmmcall, OPvmload, OPvmsave, OPstgi, OPclgi, OPskinit, OPinvlpga);
var
  Mode: Byte;
  Index: Byte;
  RM: Byte;
begin
  Include(Flags, flagModRM);
  if Code[CodeIdx] <> $01
  then begin
    Instruction^.Opcode := OPX_NotGroup7;
    Exit;
  end;
  Mode :=  (Code[ModRMIdx] shr 6) and 3;
  Index := (Code[ModRMIdx] shr 3) and 7;
  RM :=    (Code[ModRMIdx]      ) and 7;
  case Index of
    0: begin AddMs; Instruction^.Opcode := OPsgdt; end;
    1: begin AddMs; Instruction^.Opcode := OPsidt;  end;
    2: begin AddMs; Instruction^.Opcode := OPlgdt; end;
    3: begin
      if Mode = 3
      then begin
        Instruction^.Opcode := RM3[RM];
      end
      else begin
        AddMs; Instruction^.Opcode := OPlidt;
      end;
    end;
    4: begin AddMw_Rv; Instruction^.Opcode := OPsmsw; end;
    //5 : invalid
    6: begin AddEw;    Instruction^.Opcode := OPlmsw; end;
    7: begin
      if Mode = 3
      then begin
        case RM of
          0: Instruction^.Opcode := OPswapgs;
          1: Instruction^.Opcode := OPrdtscp;
        else
          Instruction^.Opcode := OPX_Group7;
        end;
      end
      else begin
        AddMb; Instruction^.Opcode := OPinvlpg;
      end;
    end;
  else
    Instruction^.Opcode := OPX_Group7;
  end;
end;

procedure TX86Disassembler.DoGroup8;
const
  RM8: array [0..7] of TOpCode = (OPX_Group8, OPX_Group8, OPX_Group8, OPX_Group8, OPbt, OPbts, OPbtr, OPbtc);
var
  Index: Byte;
begin
  Include(Flags, flagModRM);
  if Code[CodeIdx] <> $BA
  then begin
    Instruction^.Opcode := OPX_NotGroup8;
    Exit;
  end;
  Index := (Code[ModRMIdx] shr 3) and 7;
  if Index < 4
  then begin
    Instruction^.Opcode := OPX_Group8;
    Exit;
  end;
  AddEv; AddIb;
  Instruction^.Opcode := RM8[Index];
  if Index in [5..7] then
    CheckLock;
end;

procedure TX86Disassembler.DoGroup9;
var
  Index: Byte;
begin
  Include(Flags, flagModRM);
  if Code[CodeIdx] <> $C7
  then begin
    Instruction^.Opcode := OPX_NotGroup9;
    Exit;
  end;
  Index := (Code[ModRMIdx] shr 3) and 7;
  if Index = 1
  then begin
    if OperandSize = os64
    then begin
      Instruction^.Opcode :=  OPcmpxchg16b;
      AddMdq;
    end
    else begin
      Instruction^.Opcode := OPcmpxchg8b;
      AddMq;
    end;
    CheckLock;
  end
  else begin
    Instruction^.Opcode := OPX_Group9;
  end;
end;

procedure TX86Disassembler.DoGroup10;
begin
  Include(Flags, flagModRM);
  if Code[CodeIdx] <> $B9
  then begin
    Instruction^.Opcode := OPX_NotGroup10;
    Exit;
  end;
  // whole goup is invalid ??
  Instruction^.Opcode := OPX_Group10;
end;

procedure TX86Disassembler.DoGroup11;
var
  Index: Byte;
begin
  Include(Flags, flagModRM);
  Index := (Code[ModRMIdx] shr 3) and 7;
  if Index <> 0
  then begin
    Instruction^.Opcode := OPX_NotGroup11;
    Exit;
  end;

  case Code[CodeIdx] of
    $C6: begin AddEb; AddIb; end;
    $C7: begin AddEv; AddIz; end;
  else
    Instruction^.Opcode := OPX_Group11;
    Exit;
  end;
  Instruction^.Opcode := OPmov;
end;

procedure TX86Disassembler.DoGroup12;
const
  OPC: array[0..7] of TOpCode = (OPX_Invalid, OPX_Invalid, OPpsrlw, OPX_Invalid, OPpsraw, OPX_Invalid, OPpsllw, OPX_Invalid);
var
  Index: Byte;
begin
  Include(Flags, flagModRM);
  if Code[CodeIdx] <> $71
  then begin
    Instruction^.Opcode := OPX_NotGroup12;
    Exit;
  end;
  Index := (Code[ModRMIdx] shr 3) and 7;
  case DecodePrefix(OPC[Index], OPC[Index], OPX_Invalid, OPX_Invalid) of
    soNone: begin AddPRq;  AddIb; end;
    so66:   begin AddVRdq; AddIb; end;
  else
    Instruction^.Opcode := OPX_Group12;
  end;
end;

procedure TX86Disassembler.DoGroup13;
const
  OPC: array[0..7] of TOpCode = (OPX_Invalid, OPX_Invalid, OPpsrld, OPX_Invalid, OPpsrad, OPX_Invalid, OPpslld, OPX_Invalid);
var
  Index: Byte;
begin
  Include(Flags, flagModRM);
  if Code[CodeIdx] <> $72
  then begin
    Instruction^.Opcode := OPX_NotGroup13;
    Exit;
  end;
  Index := (Code[ModRMIdx] shr 3) and 7;
  case DecodePrefix(OPC[Index], OPC[Index], OPX_Invalid, OPX_Invalid) of
    soNone: begin AddPRq;  AddIb; end;
    so66:   begin AddVRdq; AddIb; end;
  else
    Instruction^.Opcode := OPX_Group13;
  end;
end;

procedure TX86Disassembler.DoGroup14;
const
  OPC: array[0..7] of TOpCode = (OPX_Invalid, OPX_Invalid, OPpsrlq, OPpsrldq, OPX_Invalid, OPX_Invalid, OPpsllq, OPpsrldq);
var
  Index: Byte;
begin
  Include(Flags, flagModRM);
  if Code[CodeIdx] <> $73
  then begin
    Instruction^.Opcode := OPX_NotGroup14;
    Exit;
  end;
  Index := (Code[ModRMIdx] shr 3) and 7;
  case DecodePrefix(OPC[Index], OPC[Index], OPX_Invalid, OPX_Invalid) of
    soNone: begin
      if (Index = 3) or (Index = 7)
      then Instruction^.Opcode := OPX_Group14
      else begin AddPRq; AddIb; end;
    end;
    so66: begin AddVRdq; AddIb; end;
  else
    Instruction^.Opcode := OPX_Group14;
  end;
end;

procedure TX86Disassembler.DoGroup15;
var
  Index: Byte;
  Mode: Byte;
begin
  Include(Flags, flagModRM);
  if Code[CodeIdx] <> $AE
  then begin
    Instruction^.Opcode := OPX_NotGroup15;
    Exit;
  end;
  Index := (Code[ModRMIdx] shr 3) and 7;
  if (Flags * [pre66, preF3, preF2] <> [])
  or (Index = 4)
  then begin
    Instruction^.Opcode := OPX_Group15;
    Exit;
  end;
  Mode :=  (Code[ModRMIdx] shr 6) and 3;
  case Index of
    0: begin Instruction^.Opcode := OPfxsave;  AddM;  end;
    1: begin Instruction^.Opcode := OPfxrstor; AddM;  end;
    2: begin Instruction^.Opcode := OPldmxcsr; AddMd; end;
    3: begin Instruction^.Opcode := OPstmxcsr; AddMd; end;
    5: Instruction^.Opcode := OPlfence;
    6: Instruction^.Opcode := OPmfence;
    7: if Mode = 3 then Instruction^.Opcode := OPlfence
                   else begin Instruction^.Opcode := OPclflush; AddMb; end;
  end;
end;

procedure TX86Disassembler.DoGroup16;
var
  Index: Byte;
begin
  Include(Flags, flagModRM);
  if Code[CodeIdx] <> $18
  then begin
    Instruction^.Opcode := OPX_NotGroup16;
    Exit;
  end;
  Index := (Code[ModRMIdx] shr 3) and 7;
  if Index=0 then ;
  Instruction^.Opcode := OPX_Invalid;
end;

procedure TX86Disassembler.DoGroupP;
var
  Index: Byte;
begin
  Include(Flags, flagModRM);
  if Code[CodeIdx] <> $0D
  then begin
    Instruction^.Opcode := OPX_NotGroupP;
    Exit;
  end;
  Index := (Code[ModRMIdx] shr 3) and 7;
  case Index of
    0:   Instruction^.Opcode := OPprefetch_exclusive;
    1,3: Instruction^.Opcode := OPprefetch_modified;
  else
    Instruction^.Opcode := OPX_prefetch;
  end;
end;

procedure TX86Disassembler.Do2ByteOpcode;
const
  INVALID = '**2byte**';

const
  OPR_6x: array[0..$F] of TOpCode = (
    OPpunpcklbw, OPpunpcklwd, OPpunpcklqd, OPpacksswb,
    OPpcmpgtb, OPpcmpgtw, OPpcmpgtd, OPpackuswb,
    OPpunpkhbw, OPpunpkhwd, OPpunpkhdq, OPpackssdw,
    OPpunpcklqdq, OPpunpckhqdq, OPX_Invalid, OPX_Invalid
  );
  OPR_Dx: array[0..$F] of TOpCode = (
    OPX_Invalid, OPpsrlw, OPpsrld, OPpsrlq,
    OPpaddq, OPpmullw, OPX_Invalid, OPX_Invalid,
    OPpsubusb, OPpsubusw, OPpminub, OPpand,
    OPpaddusb, OPpaddusw, OPpmaxub, OPpandn
  );
  OPR_Ex: array[0..$F] of TOpCode = (
    OPpavgb, OPpsraw, OPpsrad, OPpavgw,
    OPpmulhuw, OPpmulhw, OPX_Invalid, OPX_Invalid,
    OPpsubsb, OPpsubsw, OPpminsw, OPpor,
    OPpaddsb, OPpaddsw, OPpmaxsw, OPpxor
  );
  OPR_Fx: array[0..$F] of TOpCode = (
    OPX_Invalid, OPpsllw, OPpslld, OPpsllq,
    OPpmuludq, OPpmaddwd, OPpsadbw, OPX_Invalid,
    OPpsubb, OPpsubw, OPpsubd, OPpsubq,
    OPpaddb, OPpaddw, OPpaddd, OPX_Invalid
  );
var
  idx: Integer;
begin
  Inc(CodeIdx);
  Inc(ModRMIdx);
  case Code[CodeIdx] of
    $00: begin
      DoGroup6;
    end;
    $01: begin
      DoGroup7;
    end;
    $02: begin
      Instruction^.Opcode := OPlar;
      AddGv; AddEw;
    end;
    $03: begin
      Instruction^.Opcode := OPlsl;
      AddGv; AddEw;
    end;
    // $04: invalid
    $05: begin
      Instruction^.Opcode := OPsyscall;
    end;
    $06: begin
      Instruction^.Opcode := OPclts;
    end;
    $07: begin
      Instruction^.Opcode := OPsysret;
    end;
    $08: begin
      Instruction^.Opcode := OPinvd;
    end;
    $09: begin
      Instruction^.Opcode := OPwbinvd;
    end;
    // $0A: invalid
    $0B: begin
      Instruction^.Opcode := OPud2;
    end;
    // $0C: invalid
    $0D: begin
      DoGroupP;
    end;
    $0E: begin
      Instruction^.Opcode := OPfemms;
    end;
    $0F: begin
      Do3DNow;
    end;
    //---
    $10: begin
      case DecodePrefix(OPmovups, OPmovupd, OPmovsd, OPmovss) of
        soNone: begin AddVps;    AddWps; end;
        so66:   begin AddVpd;    AddWpd; end;
        soF2:   begin AddVdq_sd; AddWsd; end;
        soF3:   begin AddVdq_ss; AddWss; end;
      end;
    end;
    $11: begin
      case DecodePrefix(OPmovups, OPmovupd, OPmovsd, OPmovss) of
        soNone: begin AddWps; AddVps; end;
        so66:   begin AddWpd; AddVpd; end;
        soF2:   begin AddWsd; AddVsd; end;
        soF3:   begin AddWss; AddVss; end;
      end;
    end;
    $12: begin
      case DecodePrefix(OPmovhlps, OPmovlpd, OPmovddup, OPmovsldup) of
        soNone: begin
          // Instruction^.Opcode differs on type found
          // it is specified as Mq or VRq
          // So when getting Wq, we Add both and know the type
          AddVps; AddWq;
          if ofMemory in Instruction^.Operand[2].Flags
          then Instruction^.Opcode := OPmovlps;
        end;
        so66: begin AddVsd; AddMq;  end;
        soF2: begin AddVpd; AddWsd; end;
        soF3: begin AddVps; AddWps; end;
      end;
    end;
    $13: begin
      case DecodePrefix(OPmovlps, OPmovlpd, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddMq; AddVps; end;
        so66:   begin AddMq; AddVsd; end;
      end;
    end;
    $14: begin
      case DecodePrefix(OPunpcklps, OPunpcklpd, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddVps; AddWq; end;
        so66:   begin AddVpd; AddWq; end;
      end;
    end;
    $15: begin
      case DecodePrefix(OPunpckhps, OPunpckhpd, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddVps; AddWq; end;
        so66:   begin AddVpd; AddWq; end;
      end;
    end;
    $16: begin
      case DecodePrefix(OPmovlhps, OPmovhpd, OPX_Invalid, OPmovshdup) of
        soNone: begin
          // Instruction^.Opcode differs on type found
          // it is specified as Mq or VRq
          // So when getting Wq, we Add both and know the type
          AddVps; AddWq;
          if ofMemory in Instruction^.Operand[2].Flags
          then Instruction^.Opcode := OPmovhps;
        end;
        so66: begin AddVsd; AddMq; end;
        soF3: begin AddVps; AddWps; end;
      end;
    end;
    $17: begin
      case DecodePrefix(OPmovhps, OPmovhpd, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddMq; AddVps; end;
        so66:   begin AddMq; AddVsd; end;
      end;
    end;
    $18: begin
      DoGroup16;
    end;
    $19..$1F: begin
      Instruction^.Opcode := OPnop;
      AddEv;
    end;
    //---
    $20: begin
      Instruction^.Opcode := OPmov;
      AddRd_q; AddCd_q;
    end;
    $21: begin
      Instruction^.Opcode := OPmov;
      AddRd_q; AddDd_q;
    end;
    $22: begin
      Instruction^.Opcode := OPmov;
      AddCd_q; AddRd_q;
    end;
    $23: begin
      Instruction^.Opcode := OPmov;
      AddDd_q; AddRd_q;
    end;
    // $24..$27: OPX_Invalid
    $28: begin
      case DecodePrefix(OPmovaps, OPmovapd, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddVps; AddWps; end;
        so66:   begin AddVpd; AddWpd; end;
      end;
    end;
    $29: begin
      case DecodePrefix(OPmovaps, OPmovapd, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddWps; AddVps; end;
        so66:   begin AddWpd; AddVpd; end;
      end;
    end;
    $2A: begin
      case DecodePrefix(OPcvtpi2ps, OPcvtpi2pd, OPcvtsi2sd, OPcvtsi2ss) of
        soNone: begin AddVps; AddQq;   end;
        so66:   begin AddVpd; AddQq;   end;
        soF2:   begin AddVsd; AddEd_q; end;
        soF3:   begin AddVss; AddEd_q; end;
      end;
    end;
    $2B: begin
      case DecodePrefix(OPmovntps, OPmovntpd, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddMdq; AddVps; end;
        so66:   begin AddMdq; AddVpd; end;
      end;
    end;
    $2C: begin
      case DecodePrefix(OPcvttps2pi, OPcvttpd2pi, OPcvttsd2si, OPcvttss2si) of
        soNone: begin AddPq;   AddWps; end;
        so66:   begin AddPq;   AddWpd; end;
        soF2:   begin AddGd_q; AddWsd; end;
        soF3:   begin AddGd_q; AddWss; end;
      end;
    end;
    $2D: begin
      case DecodePrefix(OPcvtps2pi, OPcvtpd2pi, OPcvtsd2si, OPcvtss2si) of
        soNone: begin AddPq;   AddWps; end;
        so66:   begin AddPq;   AddWpd; end;
        soF2:   begin AddGd_q; AddWsd; end;
        soF3:   begin AddGd_q; AddWss; end;
      end;
    end;
    $2E: begin
      case DecodePrefix(OPucomiss, OPucomissd, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddVss; AddWss; end;
        so66:   begin AddVsd; AddWsd; end;
      end;
    end;
    $2F: begin
      case DecodePrefix(OPcomiss, OPcomissd, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddVss; AddWss; end;
        so66:   begin AddVsd; AddWsd; end;
      end;
    end;
    //---
    $30: begin
      Instruction^.Opcode := OPwrmsr;
    end;
    $31: begin
      Instruction^.Opcode := OPrdtsc;
    end;
    $32: begin
      Instruction^.Opcode := OPrdmsr;
    end;
    $33: begin
      Instruction^.Opcode := OPrdpmc;
    end;
    $34: begin
      Instruction^.Opcode := OPXsysenter;
    end;
    $35: begin
      Instruction^.Opcode := OPXsysexit;
    end;
    // $36..$3F: OPX_Invalid
    //---
    $40..$4F: begin
      Instruction^.Opcode := OPcmov__; StdCond(Code[CodeIdx]);
      AddGv; AddEv;
    end;
    //---
    $50: begin
      case DecodePrefix(OPmovmskps, OPmovmskpd, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddGd; AddVRps; end;
        so66:   begin AddGd; AddVRpd; end;
      end;
    end;
    $51..$59, $5C..$5F: begin
      case Code[CodeIdx] of
        $51: DecodePrefix(OPsqrtps,  OPsqrtpd,    OPsqrtsd,    OPsqrtss);
        $52: DecodePrefix(OPrsqrtps, OPX_Invalid, OPX_Invalid, OPrsqrtss);
        $53: DecodePrefix(OPrcpps,   OPX_Invalid, OPX_Invalid, OPrcpss);
        $54: DecodePrefix(OPandps,   OPandpd,     OPX_Invalid, OPX_Invalid);
        $55: DecodePrefix(OPandnps,  OPandnpd,    OPX_Invalid, OPX_Invalid);
        $56: DecodePrefix(OPorps,    OPorpd,      OPX_Invalid, OPX_Invalid);
        $57: DecodePrefix(OPxorps,   OPxorpd,     OPX_Invalid, OPX_Invalid);
        $58: DecodePrefix(OPaddps,   OPaddpd,     OPaddsd,     OPaddss);
        $59: DecodePrefix(OPmulps,   OPmulpd,     OPmulsd,     OPmulss);
        $5C: DecodePrefix(OPsubps,   OPsubpd,     OPsubsd,     OPsubss);
        $5D: DecodePrefix(OPminps,   OPminpd,     OPminsd,     OPminss);
        $5E: DecodePrefix(OPdivps,   OPdivpd,     OPdivsd,     OPdivss);
        $5F: DecodePrefix(OPmaxps,   OPmaxpd,     OPmaxsd,     OPmaxss);
      end;

      case SimdOpcode of
        soNone: begin AddVps; AddWps; end;
        so66:   begin AddVpd; AddWpd; end;
        soF2:   begin AddVsd; AddWsd; end;
        soF3:   begin AddVss; AddWss; end;
      end;
    end;
    $5A: begin
      case DecodePrefix(OPcvtps2pd, OPcvtpd2ps, OPcvtsd2ss, OPcvtss2sd) of
        soNone: begin AddVpd; AddWps; end;
        so66:   begin AddVps; AddWpd; end;
        soF2:   begin AddVss; AddWsd; end;
        soF3:   begin AddVsd; AddWss; end;
      end;
    end;
    $5B: begin
      case DecodePrefix(OPcvtdq2ps, OPcvtps2dq, OPX_Invalid, OPcvttps2dq) of
        soNone: begin AddVps; AddWdq; end;
        so66:   begin AddVdq; AddWps; end;
        soF3:   begin AddVdq; AddWps; end;
      end;
    end;
    // $5C..$5F: see $51
    //---
    $60..$6D: begin
      idx := Code[CodeIdx] and $F;
      CheckSIMD;
      case DecodePrefix(OPR_6x[idx], OPR_6x[idx], OPX_Invalid, OPX_Invalid) of
        soNone: begin
          if Code[CodeIdx] in [$6C, $6D]
          then Instruction^.Opcode := OPX_Invalid
          else begin AddPq;  AddQd; end;
        end;
        so66: begin
          AddVdq;
          if Code[CodeIdx] = $6B then AddWdq else AddWq;
        end;
      end;
    end;
    $6E: begin
      case DecodePrefix(OPmovd, OPmovd, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddPq;  AddEd_q; end;
        so66:   begin AddVdq; AddEd_q; end;
      end;
    end;
    $6F: begin
      case DecodePrefix(OPmovq, OPmovdqa, OPX_Invalid, OPmovdqu) of
        soNone: begin AddPq;  AddQq;  end;
        so66:   begin AddVdq; AddWdq; end;
        soF3:   begin AddVdq; AddWdq; end;
      end;
    end;
    //---
    $70: begin
      case DecodePrefix(OPpshufw, OPpshufd, OPpshuflw, OPpshufhw) of
        soNone: begin AddPq;  AddQq;  AddIb; end;
        so66:   begin AddVdq; AddWdq; AddIb; end;
        soF2:   begin AddVq;  AddWq;  AddIb; end;
        soF3:   begin AddVq;  AddWq;  AddIb; end;
      end;
    end;
    $71: begin
      if Flags * [preF3, preF2] = []
      then DoGroup12
      else Instruction^.Opcode := OPX_Invalid;
    end;
    $72: begin
      if Flags * [preF3, preF2] = []
      then DoGroup13
      else Instruction^.Opcode := OPX_Invalid;
    end;
    $73: begin
      if Flags * [preF3, preF2] = []
      then DoGroup14
      else Instruction^.Opcode := OPX_Invalid;
    end;
    $74: begin
      case DecodePrefix(OPpcmpeqb, OPpcmpeqb, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddPq;  AddQq;  end;
        so66:   begin AddVdq; AddWdq; end;
      end;
    end;
    $75: begin
      case DecodePrefix(OPpcmpeqw, OPpcmpeqw, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddPq;  AddQq;  end;
        so66:   begin AddVdq; AddWdq; end;
      end;
    end;
    $76: begin
      case DecodePrefix(OPpcmpeqd, OPpcmpeqd, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddPq;  AddQq;  end;
        so66:   begin AddVdq; AddWdq; end;
      end;
    end;
    $77: begin
      if Flags * [preF3, preF2, pre66] = []
      then Instruction^.Opcode := OPemms
      else Instruction^.Opcode := OPX_Invalid;
    end;
    // $78..$7B: OPX_Invalid
    $7C: begin
      case DecodePrefix(OPX_Invalid, OPhaddpd, OPhaddps, OPX_Invalid) of
        so66: begin AddVpd; AddWpd; end;
        soF2: begin AddVps; AddWps; end;
      end;
    end;
    $7D: begin
      case DecodePrefix(OPX_Invalid, OPhsubpd, OPhsubps, OPX_Invalid) of
        so66: begin AddVpd; AddWpd; end;
        soF2: begin AddVps; AddWps; end;
      end;
    end;
    $7E: begin
      case DecodePrefix(OPmovd, OPmovd, OPX_Invalid, OPmovq) of
        soNone: begin AddEd_q; AddPd_q; end;
        so66:   begin AddEd_q; AddVd_q; end;
        soF3:   begin AddVq;   AddWq;   end;
      end;
    end;
    $7F: begin
      case DecodePrefix(OPmovq, OPmovdqa, OPX_Invalid, OPmovdqu) of
        soNone: begin AddQq;  AddPq;  end;
        so66:   begin AddWdq; AddVdq; end;
        soF3:   begin AddWdq; AddVdq; end;
      end;
    end;
    //---
    $80..$8F: begin
      Force64;
      Instruction^.Opcode := OPj__; StdCond(Code[CodeIdx]);
      AddJz;
    end;
    //---
    $90..$9F: begin
      Instruction^.Opcode := OPset__; StdCond(Code[CodeIdx]);
      AddEb;
    end;
    //---
    $A0: begin
      Default64;
      Instruction^.Opcode := OPpush;
      AddOperand('fs');
    end;
    $A1: begin
      Default64;
      Instruction^.Opcode := OPpop;
      AddOperand('fs');
    end;
    $A2: begin
      Instruction^.Opcode := OPcpuid;
    end;
    $A3: begin
      Instruction^.Opcode := OPbt;
      AddEv; AddGv;
    end;
    $A4: begin
      Instruction^.Opcode := OPshld;
      AddEv; AddGv; AddIb;
    end;
    $A5: begin
      Instruction^.Opcode := OPshld;
      AddEv; AddGv;
      AddOperand('cl');
    end;
    // $A6..$A7: OPX_Invalid
    $A8: begin
      Default64;
      Instruction^.Opcode := OPpush;
      AddOperand('gs');
    end;
    $A9: begin
      Default64;
      Instruction^.Opcode := OPpop;
      AddOperand('gs');
    end;
    $AA: begin
      Instruction^.Opcode := OPrsm;
    end;
    $AB: begin
      Instruction^.Opcode := OPbts;
      AddEv; AddGv;
    end;
    $AC: begin
      Instruction^.Opcode := OPshrd;
      AddEv; AddGv; AddIb;
    end;
    $AD: begin
      Instruction^.Opcode := OPshld;
      AddEv; AddGv;
      AddOperand('cl');
    end;
    $AE: begin
      DoGroup15;
    end;
    $AF: begin
      Instruction^.Opcode := OPimul;
      AddGv; AddEv;
    end;
    //---
    $B0: begin
      AddEb; AddGb;
      Instruction^.Opcode := OPcmpxchg; CheckLock;
    end;
    $B1: begin
      AddEv; AddGv;
      Instruction^.Opcode := OPcmpxchg; CheckLock;
    end;
    $B2: begin
      Instruction^.Opcode := OPlss;
      AddGz; AddMp;
    end;
    $B3: begin
      Instruction^.Opcode := OPbtr;
      AddEv; AddGv;
    end;
    $B4: begin
      Instruction^.Opcode := OPlfs;
      AddGz; AddMp;
    end;
    $B5: begin
      Instruction^.Opcode := OPlgs;
      AddGz; AddMp;
    end;
    $B6: begin
      Instruction^.Opcode := OPmovzx;
      AddGv; AddEb;
    end;
    $B7: begin
      Instruction^.Opcode := Opmovzx;
      AddGv; AddEw;
    end;
    // $B8: OPX_Invalid
    $B9: begin
      DoGroup10;
    end;
    $BA: begin
      DoGroup8;
    end;
    $BB: begin
      Instruction^.Opcode := OPbtc;
      AddEv; AddGv;
    end;
    $BC: begin
      Instruction^.Opcode := OPbsf;
      AddGv; AddEv;
    end;
    $BD: begin
      Instruction^.Opcode := OPbsr;
      AddGv; AddEv;
    end;
    $BE: begin
      Instruction^.Opcode := OPmovsx;
      AddGv; AddEb;
    end;
    $BF: begin
      Instruction^.Opcode := OPmovsx;
      AddGv; AddEw;
    end;
    //---
    $C0: begin
      AddEb; AddGb;
      Instruction^.Opcode := OPxadd; CheckLock;
    end;
    $C1: begin
      AddEv; AddGv;
      Instruction^.Opcode := OPxadd; CheckLock;
    end;
    $C2: begin
      case DecodePrefix(OPcmpps, OPcmppd, OPcmpsd, OPcmpss) of
        soNone: begin AddVps; AddWps; AddIb end;
        so66:   begin AddVpd; AddWpd; AddIb end;
        soF2:   begin AddVsd; AddWsd; AddIb end;
        soF3:   begin AddVss; AddWss; AddIb end;
      end;
    end;
    $C3: begin
      if Flags * [preF3, preF2, pre66] = []
      then begin
        Instruction^.Opcode := OPmovnti;
        AddMd_q; AddGd_q;
      end
      else Instruction^.Opcode := OPX_Invalid;
    end;
    $C4: begin
      case DecodePrefix(OPpinsrw, OPpinsrw, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddPq;  AddEw; AddIb end;
        so66:   begin AddVdq; AddEw; AddIb end;
      end;
    end;
    $C5: begin
      case DecodePrefix(OPpextrw, OPpextrw, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddGd; AddPRq;  AddIb end;
        so66:   begin AddGd; AddVRdq; AddIb end;
      end;
    end;
    $C6: begin
      case DecodePrefix(OPshufps, OPshufpd, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddVps; AddWps; AddIb end;
        so66:   begin AddVpd; AddWpd; AddIb end;
      end;
    end;
    $C7: begin
      DoGroup9;
    end;
    $C8..$CF: begin
      Instruction^.Opcode := OPbswp;
      AddStdReg(Code[CodeIdx]);
    end;
    //---
    $D0: begin
      case DecodePrefix(OPX_Invalid, OPaddsubpd, OPaddsubps, OPX_Invalid) of
        so66: begin AddVpd; AddWpd; end;
        soF2: begin AddVps; AddWps; end;
      end;
    end;
    $D1..$D5, $D8..$DF: begin
      idx := Code[CodeIdx] and $F;
      case DecodePrefix(OPR_Dx[idx], OPR_Dx[idx], OPX_Invalid, OPX_Invalid) of
        soNone: begin AddPq;  AddQq;  end;
        so66:   begin AddVdq; AddWdq; end;
      end;
    end;
    $D6: begin
      case DecodePrefix(OPX_Invalid, OPmovq, OPmovdq2q, OPmovq2dq) of
        so66: begin AddWq;  AddVq;  end;
        soF2: begin AddPq;  AddVRq; end;
        soF3: begin AddVdq; AddPRq; end;
      end;
    end;
    $D7: begin
      case DecodePrefix(OPpmovmskb, OPpmovmskb, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddGd; AddPRq;  end;
        so66:   begin AddGd; AddVRdq; end;
      end;
    end;
    // $D8..$DF: see $D1
    //---
    $E0..$E5, $E8..$EF: begin
      idx := Code[CodeIdx] and $F;
      case DecodePrefix(OPR_Ex[idx], OPR_Ex[idx], OPX_Invalid, OPX_Invalid) of
        soNone: begin AddPq;  AddQq;  end;
        so66:   begin AddVdq; AddWdq; end;
      end;
    end;
    $E6: begin
      case DecodePrefix(OPX_Invalid, OPcvttpd2dq, OPcvtpd2dq, OPcvtdq2pd) of
        so66: begin AddVq;  AddWpd; end;
        soF2: begin AddVq;  AddWpd; end;
        soF3: begin AddVpd; AddWq;  end;
      end;
    end;
    $E7: begin
      case DecodePrefix(OPmovntq, OPmovntdqu, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddMq;  AddPq;  end;
        so66:   begin AddMdq; AddVdq; end;
      end;
    end;
    // $E8..$EF: see $E0
    $F0: begin
      if preF2 in Flags
      then begin
        Instruction^.Opcode := OPlddqu;
        AddVpd; AddMdq;
      end
      else Instruction^.Opcode := OPX_Invalid;
    end;
    $F1..$F6, $F8..$FE: begin
      idx := Code[CodeIdx] and $F;
      case DecodePrefix(OPR_Fx[idx], OPR_Fx[idx], OPX_Invalid, OPX_Invalid) of
        soNone: begin AddPq;  AddQq;  end;
        so66:   begin AddVdq; AddWdq; end;
      end;
    end;
    $F7: begin
      case DecodePrefix(OPmaskmovq, OPmaskmovdqu, OPX_Invalid, OPX_Invalid) of
        soNone: begin AddPq;  AddPRq;  end;
        so66:   begin AddVdq; AddVRdq; end;
      end;
    end;
    // $F8..$FE: see $F1
    // $FF: OPX_Invalid
  else
    Instruction^.Opcode := OPX_Invalid;
  end;
end;

procedure TX86Disassembler.DoDisassemble;
begin
  Instruction^.Opcode := OPX_InternalUnknown;
  repeat
    ModRMIdx := CodeIdx + 1;
    case Code[CodeIdx] of
      $00..$05: begin
        AddStdOperands(Code[CodeIdx]);
        Instruction^.Opcode := OPadd; CheckLock;
      end;
      $06: begin
        Instruction^.Opcode := OPpush; Check32;
        AddOperand('es');
      end;
      $07: begin
        Instruction^.Opcode := OPpop; Check32;
        AddOperand('es');
      end;
      $08..$0D: begin
        AddStdOperands(Code[CodeIdx]);
        Instruction^.Opcode := OPor; CheckLock;
      end;
      $0E: begin
        Instruction^.Opcode := OPpush; Check32;
        AddOperand('cs');
      end;
      $0F: begin
        Do2ByteOpcode;
      end;
      //---
      $10..$15: begin
        AddStdOperands(Code[CodeIdx]);
        Instruction^.Opcode := OPadc; CheckLock;
      end;
      $16: begin
        Instruction^.Opcode := OPpush; Check32;
        AddOperand('ss');
      end;
      $17: begin
        Instruction^.Opcode := OPpop; Check32;
        AddOperand('ss');
      end;
      $18..$1D: begin
        AddStdOperands(Code[CodeIdx]);
        Instruction^.Opcode := OPsbb; CheckLock;
      end;
      $1E: begin
        Instruction^.Opcode := OPpush; Check32;
        AddOperand('ds');
      end;
      $1F: begin
        Instruction^.Opcode := OPpop; Check32;
        AddOperand('ds');
      end;
      //---
      $20..$25: begin
        AddStdOperands(Code[CodeIdx]);
        Instruction^.Opcode := OPand; CheckLock;
      end;
      $26: begin
        Instruction^.Segment := Instruction^.Segment + Ignore64('es:');
      end;
      $27: begin
        Instruction^.Opcode := OPdaa; Check32;
      end;
      $28..$2D: begin
        AddStdOperands(Code[CodeIdx]);
        Instruction^.Opcode := OPsub; CheckLock;
      end;
      $2E: begin
        Instruction^.Segment := Instruction^.Segment + Ignore64('cs:');
      end;
      $2F: begin
        Instruction^.Opcode := OPdas; Check32;
      end;
      //---
      $30..$35: begin
        AddStdOperands(Code[CodeIdx]);
        Instruction^.Opcode := OPxor; CheckLock;
      end;
      $36: begin
        Instruction^.Segment := Instruction^.Segment + Ignore64('ss:');
      end;
      $37: begin
        Instruction^.Opcode := OPaaa; Check32;
      end;
      $38..$3D: begin
        Instruction^.Opcode := OPcmp;
        AddStdOperands(Code[CodeIdx]);
      end;
      $3E: begin
        Instruction^.Segment := Instruction^.Segment + Ignore64('ds:');
      end;
      $3F: begin
        Instruction^.Opcode := OPaas; Check32;
      end;
      //---
      $40..$4F: begin
        if (ProcessMode = dm64)
        then begin
          if (Code[CodeIdx] and 1) <> 0 then Include(Flags, rexB);
          if (Code[CodeIdx] and 2) <> 0 then Include(Flags, rexX);
          if (Code[CodeIdx] and 4) <> 0 then Include(Flags, rexR);
          if (Code[CodeIdx] and 8) <> 0 then Include(Flags, rexW);
          Include(Flags, flagRex);
        end
        else begin
          AddStdReg(Code[CodeIdx]);
          if Code[CodeIdx] <= $47
          then Instruction^.Opcode := OPinc
          else Instruction^.Opcode := OPdec;
          CheckLock;
        end;
      end;
      //---
      $50..$57: begin
        Default64;
        Instruction^.Opcode := OPpush;
        AddStdReg(Code[CodeIdx]);
      end;
      $58..$5F: begin
        Default64;
        Instruction^.Opcode := OPpop;
        AddStdReg(Code[CodeIdx]);
      end;
      //---
      $60: begin
        if OperandSize = os16
        then Instruction^.Opcode := OPpusha
        else Instruction^.Opcode := OPpushad;
        Check32;
      end;
      $61: begin
        if OperandSize = os16
        then Instruction^.Opcode := OPpopa
        else Instruction^.Opcode := OPpopad;
        Check32;
      end;
      $62: begin
        Instruction^.Opcode := OPbound; Check32;
        AddGv; AddMa;
      end;
      $63: begin
        if (ProcessMode = dm64)
        then begin
          Instruction^.Opcode := (OPmovsxd);
          AddGv; AddEd;
        end
        else begin
          Instruction^.Opcode := OParpl; Check32;
          AddEw; AddGw;
        end;
      end;
      $64: begin
        Instruction^.Segment := Instruction^.Segment + 'fs:';
      end;
      $65: begin
        Instruction^.Segment := Instruction^.Segment + 'gs:';
      end;
      $66: begin
        Include(FLags, pre66);
      end;
      $67: begin
        Include(FLags, preAdr);
      end;
      $68: begin
        Default64;
        Instruction^.Opcode := OPpush;
        AddIz;
      end;
      $69: begin
        Instruction^.Opcode := OPimul;
        AddGv; AddEv; AddIz;
      end;
      $6A: begin
        Default64;
        Instruction^.Opcode := OPpush;
        AddIb;
      end;
      $6B: begin
        Instruction^.Opcode := OPimul;
        AddGv; AddEv; AddIb;
      end;
      $6C: begin
        Instruction^.Opcode := OPinsb; CheckRepeat;
        {$ifdef verbose_string_instructions}
        AddYb;
        AddOperand('dx', os16);
        {$endif}
      end;
      $6D: begin
        if OperandSize = os16
        then Instruction^.Opcode := OPinsw
        else Instruction^.Opcode := OPinsd;
        CheckRepeat;
        {$ifdef verbose_string_instructions}
        AddYz;
        AddOperand('dx', os16);
        {$endif}
      end;
      $6E: begin
        Instruction^.Opcode := OPoutsb; CheckRepeat;
        {$ifdef verbose_string_instructions}
        AddOperand('dx', os16);
        AddXb;
        {$endif}
      end;
      $6F: begin
        if OperandSize = os16
        then Instruction^.Opcode := OPoutsw
        else Instruction^.Opcode := OPoutsd;
        CheckRepeat;
        {$ifdef verbose_string_instructions}
        AddOperand('dx', os16);
        AddXz;
        {$endif}
      end;
      $70..$7F: begin
        Force64;
        Instruction^.Opcode := OPj__;  StdCond(Code[CodeIdx]);
        AddJb;
      end;
      //---
      $80..$83: begin
        DoGroup1;
      end;
      $84: begin
        Instruction^.Opcode := OPtest;
        AddEb; AddGb;
      end;
      $85: begin
        Instruction^.Opcode := OPtest;
        AddEv; AddGv;
      end;
      $86: begin
        AddEb; AddGb;
        Instruction^.Opcode := OPxchg; CheckLock;
      end;
      $87: begin
        AddEv; AddGv;
        Instruction^.Opcode := OPxchg; CheckLock;
      end;
      $88..$8B: begin
        Instruction^.Opcode := OPmov;
        AddStdOperands(Code[CodeIdx]);
      end;
      $8C: begin
        Instruction^.Opcode := OPmov;
        AddMw_Rv; AddSw;
      end;
      $8D: begin
        Instruction^.Opcode := OPlea;
        AddGv; AddM;
      end;
      $8E: begin
        Instruction^.Opcode := OPmov;
        AddSw; AddEw;
      end;
      $8F: begin
        Default64;
        DoGroup1;
      end;
      //---
      $90..$97: begin
        if (Code[CodeIdx] = $90) and not (rexB in Flags)
        then Instruction^.Opcode := OPnop
        else begin
          Instruction^.Opcode := OPxchg;
          AddStdReg(Code[CodeIdx]);
          AddOperand(SizeReg32('ax'));
        end;
      end;
      $98: begin
        case OperandSize of
          os64: Instruction^.Opcode := OPcdqe;
          os32: Instruction^.Opcode := OPcwde;
        else
          Instruction^.Opcode := OPcbw;
        end;
      end;
      $99: begin
        case OperandSize of
          os64: Instruction^.Opcode := OPcqo;
          os32: Instruction^.Opcode := OPcqd;
        else
          Instruction^.Opcode := OPcwd;
        end;
      end;
      $9A: begin
        Instruction^.Opcode := OPcall; Check32;
        AddAp;
      end;
      $9B: begin
        Instruction^.Opcode := OPwait_fwait;
      end;
      $9C: begin
        Default64;
        case OperandSize of
          os64: Instruction^.Opcode := OPpushfq;
          os32: Instruction^.Opcode := OPpushfd;
        else
          Instruction^.Opcode := OPpushf;
        end;
        AddFv;
      end;
      $9D: begin
        Default64;
        case OperandSize of
          os64: Instruction^.Opcode := OPpopfq;
          os32: Instruction^.Opcode := OPpopfd;
        else
          Instruction^.Opcode := OPpopf;
        end;
        AddFv;
      end;
      $9E: begin
        Instruction^.Opcode := OPsahf;
      end;
      $9F: begin
        Instruction^.Opcode := OPlahf;
      end;
      //---
      $A0: begin
        Instruction^.Opcode := OPmov;
        AddOperand('al', os8);
        AddOb;
      end;
      $A1: begin
        Instruction^.Opcode := OPmov;
        AddOperand(SizeReg32('ax'));
        AddOv;
      end;
      $A2: begin
        Instruction^.Opcode := OPmov;
        AddOb;
        AddOperand('al', os8);
      end;
      $A3: begin
        Instruction^.Opcode := OPmov;
        AddOv;
        AddOperand(SizeReg32('ax'));
      end;
      $A4: begin
        Instruction^.Opcode := OPmovsb; CheckRepeat;
        {$ifdef verbose_string_instructions}
        AddYb; AddXb;
        {$endif}
      end;
      $A5: begin
        case OperandSize of
          os64: Instruction^.Opcode := OPmovsq;
          os32: Instruction^.Opcode := OPmovsd;
        else
          Instruction^.Opcode := OPmovsw;
        end;
        CheckRepeat;
        {$ifdef verbose_string_instructions}
        AddYv; AddXv;
        {$endif}
      end;
      $A6: begin
        Instruction^.Opcode := OPcmpsb; CheckRepeatX;
        {$ifdef verbose_string_instructions}
        AddXb; AddYb;
        {$endif}
      end;
      $A7: begin
        case OperandSize of
          os64: Instruction^.Opcode := OPcmpsq;
          os32: Instruction^.Opcode := OPcmpsd;
        else
          Instruction^.Opcode := OPcmpsw;
        end;
        CheckRepeatX;
        {$ifdef verbose_string_instructions}
        AddYv; AddXv;
        {$endif}
      end;
      $A8: begin
        Instruction^.Opcode := OPtest;
        AddOperand('al', os8);
        AddIb;
      end;
      $A9: begin
        Instruction^.Opcode := OPtest;
        AddOperand(SizeReg32('ax'));
        AddIv;
      end;
      $AA: begin
        Instruction^.Opcode := OPstosb; CheckRepeat;
        {$ifdef verbose_string_instructions}
        AddYb;
        AddOperand('al', os8);
        {$endif}
      end;
      $AB: begin
        case OperandSize of
          os64: Instruction^.Opcode := OPstosq;
          os32: Instruction^.Opcode := OPstosd;
        else
          Instruction^.Opcode := OPstosw;
        end;
        CheckRepeat;;
        {$ifdef verbose_string_instructions}
        AddYv;
        AddOperand(SizeReg32('ax'));
        {$endif}
      end;
      $AC: begin
        Instruction^.Opcode := OPlodsb; CheckRepeat;
        {$ifdef verbose_string_instructions}
        AddOperand('al', os8);
        AddXb;
        {$endif}
      end;
      $AD: begin
        case OperandSize of
          os64: Instruction^.Opcode := OPlodsq;
          os32: Instruction^.Opcode := OPlodsd;
        else
          Instruction^.Opcode := OPlodsw;
        end;
        CheckRepeat;
        {$ifdef verbose_string_instructions}
        AddOperand(SizeReg32('ax'));
        AddXv;
        {$endif}
      end;
      $AE: begin
        Instruction^.Opcode := OPscasb; CheckRepeatX;
        {$ifdef verbose_string_instructions}
        AddOperand('al', os8);
        AddYb;
        {$endif}
      end;
      $AF: begin
        case OperandSize of
          os64: Instruction^.Opcode := OPscasq;
          os32: Instruction^.Opcode := OPscasd;
        else
          Instruction^.Opcode := OPscasw;
        end;
        CheckRepeatX;
        {$ifdef verbose_string_instructions}
        AddOperand(SizeReg32('ax'));
        AddYv;
        {$endif}
      end;
      //---
      $B0..$B7: begin
        Instruction^.Opcode := OPmov;
        AddStdReg(Code[CodeIdx], reg8);
        AddIb;
      end;
      $B8..$BF: begin
        Instruction^.Opcode := OPmov;
        AddStdReg(Code[CodeIdx]);
        AddIv;
      end;
      //---
      $C0..$C1: begin
        DoGroup2;
      end;
      $C2: begin
        Force64;
        Instruction^.Opcode := OPret;
        AddIw;
      end;
      $C3: begin
        Force64;
        Instruction^.Opcode := OPret;
      end;
      $C4: begin
        Instruction^.Opcode := OPles;
        AddGz; AddMp;
      end;
      $C5: begin
        Instruction^.Opcode := OPlds;
        AddGz; AddMp;
      end;
      $C6..$C7: begin
        DoGroup11;
      end;
      $C8: begin
        Instruction^.Opcode := OPenter;
        AddIw; AddIb;
      end;
      $C9: begin
        Default64;
        Instruction^.Opcode := OPleave;
      end;
      $CA: begin
        Instruction^.Opcode := OPretf;
        AddIw;
      end;
      $CB: begin
        Instruction^.Opcode := OPretf;
      end;
      $CC: begin
        Instruction^.Opcode := OPint3;
      end;
      $CD: begin
        Instruction^.Opcode := OPint;
        AddIb;
      end;
      $CE: begin
        Instruction^.Opcode := OPint0; Check32;
      end;
      $CF: begin
        case OperandSize of
          os64: Instruction^.Opcode := OPiretq;
          os32: Instruction^.Opcode := OPiretd;
        else
          Instruction^.Opcode := OPiret;
        end;
      end;
      //---
      $D0..$D3: begin
        DoGroup2;
      end;
      $D4: begin
        Instruction^.Opcode := OPaam; Check32;
      end;
      $D5: begin
        Instruction^.Opcode := OPaad; Check32;
      end;
      $D6: begin
        Instruction^.Opcode := OPsalc; Check32;
      end;
      $D7: begin
        Instruction^.Opcode := OPxlat;
      end;
      $D8..$DF: begin
        DoX87;
      end;
      //---
      $E0: begin
        Force64;
        Instruction^.Opcode := OPloopne;
        AddJb;
      end;
      $E1: begin
        Force64;
        Instruction^.Opcode := OPloope;
        AddJb;
      end;
      $E2: begin
        Force64;
        Instruction^.Opcode := OPloop;
        AddJb;
      end;
      $E3: begin
        Force64;
        Instruction^.Opcode := OPjrcxz;
        AddJb;
      end;
      $E4: begin
        Instruction^.Opcode := OPin;
        AddOperand('al', os8);
        AddIb;
      end;
      $E5: begin
        Instruction^.Opcode := OPin;
        AddOperand(SizeReg32('ax'));
        AddIb;
      end;
      $E6: begin
        Instruction^.Opcode := OPout;
        AddIb;
        AddOperand('al', os8);
      end;
      $E7: begin
        Instruction^.Opcode := OPout;
        AddIb;
        AddOperand(SizeReg32('ax'));
      end;
      $E8: begin
        Force64;
        Instruction^.Opcode := OPcall;
        AddJz;
      end;
      $E9: begin
        Force64;
        Instruction^.Opcode := OPjmp;
        AddJz;
      end;
      $EA: begin
        Instruction^.Opcode := OPjmp; Check32;
        AddAp;
      end;
      $EB: begin
        Force64;
        Instruction^.Opcode := OPjmp;
        AddJb;
      end;
      $EC: begin
        Instruction^.Opcode := OPin;
        AddOperand('al', os8);
        AddOperand('dx', os16);
      end;
      $ED: begin
        Instruction^.Opcode := OPin;
        AddOperand(SizeReg32('ax'));
        AddOperand('dx', os16);
      end;
      $EE: begin
        Instruction^.Opcode := OPout;
        AddOperand('dx', os16);
        AddOperand('al', os8);
      end;
      $EF: begin
        Instruction^.Opcode := OPout;
        AddOperand('dx', os16);
        AddOperand(SizeReg32('ax'));
      end;
      $F0: begin
        Include(Flags, preLock);
      end;
      $F1: begin
        Instruction^.Opcode := OPint1;
      end;
      $F2: begin
        Include(Flags, preF2);
      end;
      $F3: begin
        Include(Flags, preF3);
      end;
      $F4: begin
        Instruction^.Opcode := OPhlt;
      end;
      $F5: begin
        Instruction^.Opcode := OPcmc;
      end;
      $F6..$F7: begin
        DoGroup3;
      end;
      $F8: begin
        Instruction^.Opcode := OPclc;
      end;
      $F9: begin
        Instruction^.Opcode := OPstc;
      end;
      $FA: begin
        Instruction^.Opcode := OPcli;
      end;
      $FB: begin
        Instruction^.Opcode := OPsti;
      end;
      $FC: begin
        Instruction^.Opcode := OPcld;
      end;
      $FD: begin
        Instruction^.Opcode := OPstd;
      end;
      $FE: begin
        DoGroup4;
      end;
      $FF: begin
        DoGroup5;
      end;
    else
      Instruction^.Opcode := OPX_Invalid; // HexValue(Code[CodeIdx], 1, []);
    end;

    Inc(CodeIdx);
    if CodeIdx > 16 // max instruction length
    then begin
      Debugln(DBG_WARNINGS, 'Disassemble: instruction longer than 16 bytes');
      Exit;
    end;
  until Instruction^.Opcode <> OPX_InternalUnknown;
end;

procedure TX86Disassembler.Disassemble(AMode: TFPDMode; var AAddress: Pointer; out AnInstruction: TInstruction);
var
  n: Integer;
begin
  ProcessMode := AMode;
  Code := AAddress;
  Instruction := @AnInstruction;
  Instruction^.Opcode := OPX_Invalid;
  Instruction^.OpCodeSuffix := OPSx_none;
  Instruction^.Flags := [];
  Instruction^.Segment := '';

  Flags := [];
  CodeIdx := 0;
  OperIdx := 0;
  SimdOpcode := soInvalid;

  DoDisassemble;

  Instruction^.OperCnt := OperIdx;
  Instruction^.ParseFlags := Flags;

  if flagModRM in Flags then Inc(CodeIdx);
  if flagSib in Flags then Inc(CodeIdx);

  for n := 1 to OperIdx do
  begin
    AnInstruction.Operand[n].CodeIndex := CodeIdx;
    Inc(CodeIdx, AnInstruction.Operand[n].ByteCount);
    Inc(CodeIdx, AnInstruction.Operand[n].ByteCount2);
  end;
  Inc(AAddress, CodeIdx);
end;

{ TX86AsmInstruction }

procedure TX86AsmInstruction.ReadCode;
begin
  if not (diCodeRead in FFlags) then begin
    if not FProcess.ReadData(FAddress, INSTR_CODEBIN_LEN, FCodeBin) then
      Include(FFlags, diCodeReadError);
    Include(FFlags, diCodeRead);
  end;
end;

procedure TX86AsmInstruction.Disassemble;
var
  a: PByte;
  Disassembler: TX86Disassembler;
begin
  if not (diDisAss in FFlags) then begin
    ReadCode;
    if diCodeReadError in FFlags then
      exit;
    a := @FCodeBin[0];
    Disassembler.Disassemble(FProcess.Mode, a, FInstruction);
    FInstrLen := a - @FCodeBin[0];
    Include(FFlags, diDisAss);
  end;
end;

constructor TX86AsmInstruction.Create(AProcess: TDbgProcess);
begin
  FProcess := AProcess;
  inherited Create;
  AddReference;
end;

procedure TX86AsmInstruction.SetAddress(AnAddress: TDBGPtr);
begin
  FAddress := AnAddress;
  FFlags := [];
end;

function TX86AsmInstruction.IsCallInstruction: boolean;
var
  a: PByte;
begin
  Result := False;
  ReadCode;
  if diCodeReadError in FFlags then
    exit;
  a := @FCodeBin[0];

  if (FProcess.Mode = dm64) then begin
    while (a < @FCodeBin[0] + INSTR_CODEBIN_LEN) and (a^ in [$40..$4F, $64..$67]) do
      inc(a);
    if not (a^ in [$E8, $FF]) then
      exit;
  end
  else begin
    while (a < @FCodeBin[0] + INSTR_CODEBIN_LEN) and (a^ in [$26, $2E, $36, $3E, $64..$67]) do
      inc(a);
    if not (a^ in [$9A, $E8, $FF]) then
      exit;
  end;

  Disassemble;
  Result := FInstruction.OpCode = OPcall;
end;

function TX86AsmInstruction.IsReturnInstruction: boolean;
var
  a: PByte;
begin
  ReadCode;
  if diCodeReadError in FFlags then
    exit(False);
  a := @FCodeBin[0];

  // CF: IRET
  Result := (a^ in [$C2, $C3, $CA, $CB, $CF]);
end;

function TX86AsmInstruction.IsLeaveStackFrame: boolean;
var
  a: PByte;
begin
  ReadCode;
  if diCodeReadError in FFlags then
    exit(False);
  a := @FCodeBin[0];
  // C9: leave
  Result := (a^ = $C9);
  if Result then
    exit;
  if (FProcess.Mode = dm64) then begin
    Result :=
      // 48 8D 65 00 / 5D: lea rsp,[rbp+$00] / pop ebp
      ( (a^ = $48) and (a[1] = $8D) and (a[2] = $65) and (a[3] = $00)
        and (a[4] = $5D)
      ) or
      // 48 89 ec / 5D: mov esp,ebp / pop ebp
      ( (a^ = $48) and (a[1] = $89) and (a[2] = $EC)
        and (a[3] = $5D)
      );
  end
  else begin
    Result :=
      // 8D 65 00 / 5D: lea rsp,[rbp+$00] / pop ebp
      ( (a[0] = $8D) and (a[1] = $65) and (a[2] = $00)
       and (a[3] = $5D)
      ) or
      // 89 ec / 5D: mov esp,ebp / pop ebp
      ( (a[0] = $89) and (a[1] = $EC)
       and (a[2] = $5D)
      );
  end;
end;

function TX86AsmInstruction.ModifiesStackPointer: boolean;
var
  a: PByte;
begin
  (* Enter, Leave
     mov sp, ...
     lea sp, ...
     pop / push

     BUT NOT ret
  *)
  Result := False;
  ReadCode;
  if diCodeReadError in FFlags then
    exit;
  a := @FCodeBin[0];

  if (FProcess.Mode = dm64) then begin
    while (a < @FCodeBin[0] + INSTR_CODEBIN_LEN) and (a^ in [$40..$4F, $64..$67]) do
      inc(a);

    // Pop/Push
    if (a^ in [$50..$61, $68, $8F, $9C, $9d])
    then
      exit(True);
  end
  else begin
    while (a < @FCodeBin[0] + INSTR_CODEBIN_LEN) and (a^ in [$26, $2E, $36, $3E, $64..$67]) do
      inc(a);

    // Pop/Push
    if (a^ in [$06, $07, $0E, $16, $17, $1E, $1F, $50..$61, $68, $6A, $8F, $9C, $9d])
    then
      exit(True);
  end;

  // Pop/Push
  if (a^ in [$FF])
  then begin
    Disassemble;
    exit(FInstruction.OpCode = OPpush);
  end;

  if (a^ = $0F) and (a[1] in [$A0, $A1, $A8, $A9]) then
    exit(True);

  // Enter/Leave
  if (a^ in [$C8, $C9])
  then
    exit(True);

  // Mov/Lea
  if (a^ in [$89, $8B, $8D]) and
     (  ((a[1] and $38) = $20) or ((a[1] and $03) = $04)  )  // SP is involved
  then begin
    //Disassemble;
    exit(True);  // does report some "false positives"
  end;
end;

function TX86AsmInstruction.IsJumpInstruction(IncludeConditional: Boolean;
  IncludeUncoditional: Boolean): boolean;
var
  a: PByte;
begin
  (* Excluding
     E1, E2  loop
     E3   JCXZ   Jump short if eCX register is 0
  *)
  Result := False;
  ReadCode;
  if diCodeReadError in FFlags then
    exit;
  a := @FCodeBin[0];

  if IncludeConditional and (a^ in [$70..$7F]) then
    exit(True);
  if IncludeConditional and (a^ = $0F) and (a[1] in [$80..$8F]) then
    exit(True);

  if IncludeUncoditional and (a^ in [$E9..$EB]) then
    exit(True);

  if IncludeUncoditional and (a^ in [$FF]) then begin
    Disassemble;
    exit(FInstruction.OpCode = OPjmp);
  end;

end;

function TX86AsmInstruction.InstructionLength: Integer;
begin
  Disassemble;
  if diCodeReadError in FFlags then
    exit(0);
  Result := FInstrLen;
end;

function TX86AsmInstruction.X86OpCode: TOpCode;
begin
  Disassemble;
  if diCodeReadError in FFlags then
    exit(OPX_Invalid);
  Result := FInstruction.OpCode;
end;

procedure TX86AsmDecoder.Disassemble(var AAddress: Pointer;
  out ACodeBytes: String; out ACode: String);
const
  MEMPTR: array[TOperandSize] of string = ('byte ptr ', 'word ptr ', 'dword ptr ', 'qword ptr ', '', 'tbyte ptr ', '16byte ptr ');
{$ifdef debug_OperandSize}
  OSTEXT: array[TOperandSize] of string = ('os8', 'os16', 'os32', 'os64', 'os48', 'os80', 'os128');
{$endif}
var
  Disassembler: TX86Disassembler;
  Instr: TInstruction;
  S, Soper: String;
  n, i: Integer;
  HasMem: Boolean;
  OpcodeName: String;
  Code: PByte;
begin
  Code := AAddress;
  Disassembler.Disassemble(FProcess.Mode, AAddress, Instr);

  Soper := '';
  HasMem := False;
  for n := 1 to Instr.OperCnt do
  begin
    if Instr.Operand[n].ByteCount = 0
    then S := Instr.Operand[n].Value
    else begin
      i := Instr.Operand[n].CodeIndex;
      if Instr.Operand[n].ByteCount2 = 0
      then S := Format(Instr.Operand[n].Value, [HexValue(Code[i], Instr.Operand[n].ByteCount, Instr.Operand[n].FormatFlags)])
      else S := Format(Instr.Operand[n].Value, [HexValue(Code[i], Instr.Operand[n].ByteCount, Instr.Operand[n].FormatFlags), HexValue(Code[i + Instr.Operand[n].ByteCount], Instr.Operand[n].ByteCount2, Instr.Operand[n].FormatFlags)])
    end;

    if Soper <> '' then Soper := Soper + ',';
    if ofMemory in Instr.Operand[n].Flags
    then begin
      if (Instr.OperCnt = 1)
//      or (Instr.Operand[n].Size <> os32)
      or (Instr.Operand[1].Size <> Instr.Operand[2].Size)
      then Soper := Soper + MEMPTR[Instr.Operand[n].Size];
      Soper := Soper + Instr.Segment + '[' + S + ']';
      HasMem := True;
    end
    else Soper := Soper + S;
  end;
{$ifdef debug_OperandSize}
  Soper := Soper + ' | ';
  for n := 1 to OperIdx do
  begin
    Soper := Soper + ' ' + OSTEXT[Instr.Operand[n].Size];
  end;
{$endif}

  OpcodeName := OPCODE_NAME[Instr.OpCode];
  OpcodeName := OpcodeName + OPCODE_SUFFIX_NAME[Instr.OpCodeSuffix];
  if Instr.Flags * [ifOnly32, ifOnly64] <> [] then
    OpcodeName := '**'+OpcodeName + '**';
  if ifPrefixRep in Instr.Flags then
    OpcodeName := 'rep '+OpcodeName ;
  if ifPrefixRepE in Instr.Flags then
    OpcodeName := 'repe '+OpcodeName ;
  if ifPrefixRepNe in Instr.Flags then
    OpcodeName := 'repne '+OpcodeName ;
  if ifPrefixLock in Instr.Flags then
    OpcodeName := 'lock '+OpcodeName ;

  S := '';
  if preLock in Instr.ParseFlags then S := S + '**lock**';
  if preF3 in Instr.ParseFlags then S := S + '?rep?';
  if preF2 in Instr.ParseFlags then S := S + '?repne?';
  S := S + OpcodeName;
  if not HasMem and (Instr.Segment <> '') then S := S + ' ?' + Instr.Segment + '?';
  ACode := S + ' ' + Soper;

  // memory
  S := '';
  while Code < AAddress do
  begin
    S := S + HexStr(Code^, 2);
    inc(Code);
  end;
  ACodeBytes := S;
end;

function TX86AsmDecoder.GetInstructionInfo(AnAddress: TDBGPtr): TDbgAsmInstruction;
begin
  if (FLastInstr = nil) or (FLastInstr.RefCount > 1) then begin
    ReleaseRefAndNil(FLastInstr);
    FLastInstr := TX86AsmInstruction.Create(FProcess);
  end;

  FLastInstr.SetAddress(AnAddress);
  Result := FLastInstr;
end;

{ TX86AsmDecoder }

function TX86AsmDecoder.GetLastErrorWasMemReadErr: Boolean;
begin
  Result := FLastErrWasMem;
end;

function TX86AsmDecoder.GetMaxInstrSize: integer;
begin
  Result := 16;
end;

function TX86AsmDecoder.GetMinInstrSize: integer;
begin
  Result := 1;
end;

function TX86AsmDecoder.GetCanReverseDisassemble: boolean;
begin
  {$IFDEF FPDEBUG_WITH_REVERSE_DISASM}
  Result := true;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TX86AsmDecoder.ReadCodeAt(AnAddress: TDBGPtr; var ALen: Cardinal
  ): Boolean;
begin
  Result := FProcess.ReadData(AnAddress, ALen, FCodeBin[0], ALen);
  FLastErrWasMem := not Result;
end;

constructor TX86AsmDecoder.Create(AProcess: TDbgProcess);
begin
  FProcess := AProcess;
end;

destructor TX86AsmDecoder.Destroy;
begin
  ReleaseRefAndNil(FLastInstr);
  inherited Destroy;
end;

function TX86AsmDecoder.GetFunctionFrameInfo(AnAddress: TDBGPtr; out
  AnIsOutsideFrame: Boolean): Boolean;
var
  ADataLen: Cardinal;
  AData: PByte;
begin
  ADataLen := MAX_CODEBIN_LEN;
  Result := False;
  if not ReadCodeAt(AnAddress, ADataLen) then
    exit;
  AData := @FCodeBin[0];

  while (ADataLen > 0) and (AData^ = $90) do begin // nop
    inc(AData);
    dec(ADataLen);
  end;
  Result := ADataLen > 0;
  if not Result then
    exit;

  AnIsOutsideFrame := False;
  if AData^ = $55 then begin // push ebp
    AnIsOutsideFrame := True;
    exit;
  end;

  if AData^ = $C3 then begin // ret
    AnIsOutsideFrame := True;
    exit;
  end;

  if AData^ in [$50..$54, $56..$57] then begin // push
    while (ADataLen > 1) and (AData^ in [$50..$57]) do begin
      inc(AData);
      dec(ADataLen);
    end;
    if AData^ = $55 then begin // push ebp
      AnIsOutsideFrame := True;
      exit;
    end;
    //48 8D A4 24 50FBFFFF         lea rsp,[rsp-$000004B0]
    //48 8D 64 24 C0               lea rsp,[rsp-$40]
    //but NOT  48 8D A4 24 B040000         lea rsp,[rsp+$000004B0]
    if (ADataLen >= 4) and (AData[0] = $48) and (AData[1] = $8D) and (AData[3] = $24) and (
         (                     (AData[2] = $64) and ((AData[4] and $80) <> 0) ) or
         ( (ADataLen >= 8) and (AData[2] = $A4) and ((AData[7] and $80) <> 0) )
       )
    then begin // mov rbp,rsp // AFTER push ebp
      AnIsOutsideFrame := True;
      exit;
    end;
  end;

  //if (ADataLen >= 2) and (AData[0] = $89) and (AData[1] = $E5) // 32 bit mov ebp, esp
  if (ADataLen >= 3) and (AData[0] = $48) and (AData[1] = $89) and (AData[2] = $E5)
  then begin // mov rbp,rsp // AFTER push ebp
    // Need 1 byte before, to check for "push ebp"
    exit;
  end;
end;


initialization
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
end.
