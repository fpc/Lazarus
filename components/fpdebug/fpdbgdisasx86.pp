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
  SysUtils,
  FpDbgUtil, FpDbgInfo, DbgIntfBaseTypes, FpdMemoryTools, LazLoggerBase;

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
}  


procedure Disassemble(var AAddress: Pointer; const A64Bit: Boolean; out ACodeBytes: String; out ACode: String);

// returns byte len of call instruction at AAddress // 0 if not a call intruction
function IsCallInstruction(AAddress: Pointer; const A64Bit: Boolean): Integer;

implementation
var
  DBG_WARNINGS: PLazLoggerLogGroup;

type
  TFlag = (flagRex, flagSib, flagModRM, rexB, rexX, rexR, rexW, preOpr, preAdr, preLock, preRep{N}, preRepNE);
  TFlags = set of TFlag;
  
  // Keep 8,16,32,64 together
  TOperandSize = (os8, os16, os32, os64, os48, os80, os128);
  TAddressSize = (as16, as32, as64);
  TRegisterType = (reg0, reg8, reg16, reg32, reg64, regMmx, regXmm, regSegment, regControl, regDebug, regX87);
  TModRMType = (modReg, modMem);
  TModRMTypes = set of TModRMType;
  

const
  ADDRESS_BYTES: array[TAddressSize] of Byte = (2, 4, 8);
  OPERAND_BYTES: array[TOperandSize] of Byte = (1, 2, 4, 8, 6, 10, 16);
  OPERAND_REG: array[os8..os64] of TRegisterType = (reg8, reg16, reg32, reg64);
  STD_REGS = [reg8..reg64];

type
  TOperandFlag = (ofMemory);
  TOperandFlags = set of TOperandFlag;

procedure Disassemble(var AAddress: Pointer; const A64Bit: Boolean; out ACodeBytes: String; out ACode: String);
var
  Code: PByte;
  CodeIdx: Byte;
  Operand: array[1..4] of record
    Value: String;
    Size: TOperandSize;
    ByteCount: Byte;
    ByteCount2: Byte;
    FormatFlags: THexValueFormatFlags;
    Flags: TOperandFlags;
  end;
  OperIdx: Byte;
  ModRMIdx: Byte;
  Opcode: String;
  Segment: String;
  Flags: TFlags;

  function Check32(const Opcode: String): String;
  begin
    // only valid in 32-bit mode
    if A64Bit
    then Result := '**' + Opcode + '**'
    else Result := Opcode;
  end;

  function Check64(const Opcode: String): String;
  begin
    // only valid in 64-bit mode
    if A64Bit
    then Result := Opcode
    else Result := '**' + Opcode + '**';
  end;

  function Ignore64(const Opcode: String): String;
  begin
    // ignored in 64-bit mode
    if A64Bit
    then Result := '(' + Opcode + ')'
    else Result := Opcode;
  end;
  
  function CheckLock(const AOpcode: String): String;
    function CheckMem: boolean;
    var
      n: Byte;
    begin
      Result := True;
      for n := 1 to OperIdx do
        if ofMemory in Operand[n].Flags then Exit;
      Result := False;
    end;
  begin
    if (preLock in Flags) and CheckMem
    then begin
      Exclude(Flags, preLock);
      Result := 'lock ' + AOpcode;
      Exit;
    end;
    Result := AOpcode;
  end;

  function CheckRepeat(const AOpcode: String): String;
  begin
    if preRep in Flags
    then begin
      Exclude(Flags, preRep);
      Result := 'rep ' + AOpcode;
      Exit;
    end;
    Result := AOpcode;
  end;
  
  function CheckRepeatX(const AOpcode: String): String;
  begin
    if preRep in Flags
    then begin
      Exclude(Flags, preRep);
      Result := 'repe ' + AOpcode;
      Exit;
    end;
    if preRepNE in Flags
    then begin
      Exclude(Flags, preRepNE);
      Result := 'repne ' + AOpcode;
      Exit;
    end;
    Result := AOpcode;
  end;
  
  //===================

  function DecodePrefix(const AOpcode, AOpcodeRep, AOpcodeOpr, AOpcodeRepNE: string): Integer;
  var
    S: String;
  begin
    if preRep in Flags
    then begin
      S := AOpcodeRep;
      Exclude(Flags, preRep);
      Result := 1;
    end
    else if preOpr in Flags
    then begin
      S := AOpcodeOpr;
      Exclude(Flags, preOpr);
      Result := 2;
    end
    else if preRepNE in Flags
    then begin
      S := AOpcodeRepNE;
      Exclude(Flags, preRepNE);
      Result := 3;
    end
    else begin
      S := AOpcode;
      Result := 0;
    end;
    if S = ''
    then Result := -1
    else Opcode := S;
  end;

  function AddressSize32: TAddressSize;
  begin
    // effective address size for default 32 operand size
    if A64Bit
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

  function OperandSize32: TOperandSize;
  begin
    // effective operand size for default 32 operand size
    if rexW in FLags
    then begin
      Result := os64;
    end
    else begin
      if preOpr in Flags
      then Result := os16
      else Result := os32;
    end;
  end;

  procedure AddOperand(const AValue: String; ASize: TOperandSize; AByteCount: Byte = 0; AFormatFlags: THexValueFormatFlags = []; AFlags: TOperandFlags = []; AByteCount2: Byte = 0);
  begin
    Inc(OperIdx);
    if OperIdx > High(Operand)
    then begin
      Debugln(DBG_WARNINGS, 'AddOperand: Only %d operands supported, got %d', [High(Operand), OperIdx]);
      Exit;
    end;

    Operand[OperIdx].Size := ASize;
    Operand[OperIdx].ByteCount := AByteCount;
    Operand[OperIdx].ByteCount2 := AByteCount2;
    Operand[OperIdx].FormatFlags := AFormatFlags;
    Operand[OperIdx].Value := AValue;
    Operand[OperIdx].Flags := AFlags;
  end;

  procedure AddOperand(const AValue: String; AByteCount: Byte = 0; AFormatFlags: THexValueFormatFlags = []; AFlags: TOperandFlags = []);
  begin
    AddOperand(AValue, OperandSize32, AByteCount, AFormatFlags, AFlags);
  end;

  function SizeReg32(const AReg: String; ASize: TOperandSize): String;
  begin
    // prefix a reg for default 32 operand size
    case ASize of
      os64: Result := 'r' + AReg;
      os32: Result := 'e' + AReg;
    else
      Result := AReg;
    end;
  end;

  function SizeReg32(const AReg: String): String;
  begin
    Result := SizeReg32(AReg, OperandSize32);
  end;

  function StdCond(AIndex: Byte): String;
  const
    COND: array[0..$F] of String = ('o', 'no', 'b', 'nb', 'z', 'nz', 'be', 'nbe', 's', 'ns', 'p', 'np', 'l', 'nl', 'le', 'nle');
  begin
    Result := COND[AIndex and $F];
  end;

  function StdReg(AIndex: Byte; AType: TRegisterType; AExtReg: Boolean): String;
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

  function StdReg(AIndex: Byte): String;
  begin
    Result := StdReg(AIndex, OPERAND_REG[OperandSize32], rexR in Flags);
  end;

  procedure AddStdReg(AIndex: Byte; AType: TRegisterType; AExtReg: Boolean);
  const
    // reg8, reg16, reg32, reg64, regMmx, regXmm, regSegment, regControl, regDebug, regX87
    REGSIZE: array[Boolean, reg8..High(TRegisterType)] of TOperandSize = (
    {32}(os8, os16, os32, os64, os64, os128, os16, os32, os32, os80),
    {64}(os8, os16, os32, os64, os64, os128, os16, os64, os64, os80)
    );
  begin
    AddOperand(StdReg(AIndex, AType, AExtReg), REGSIZE[A64Bit, AType]);
  end;

  procedure AddStdReg(AIndex: Byte);
  begin
    AddOperand(StdReg(AIndex, OPERAND_REG[OperandSize32], rexR in Flags));
  end;

  procedure AddModReg(AType: TRegisterType; ASize: TOperandSize);
  begin
    Include(Flags, flagModRM);
    AddOperand(StdReg(Code[ModRMIdx] shr 3, AType, False), ASize);
  end;

  procedure AddModReg(AType: TRegisterType; AExtReg: Boolean);
  begin
    Include(Flags, flagModRM);
    AddStdReg(Code[ModRMIdx] shr 3, AType, AExtReg);
  end;

  procedure AddModReg;
  begin
    Include(Flags, flagModRM);
    AddStdReg(Code[ModRMIdx] shr 3);
  end;

  procedure AddModRM(AReqTypes: TModRMTypes; ASize: TOperandSize; AType: TRegisterType);
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

    // Check for reg (mode = 3) first;
    if mode = 3
    then begin
      if modReg in AReqTypes
      then AddStdReg(rm, AType, False)
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
      if (mode = 0) and (sib.Base = 5)
      then begin
        // disp32
        Oper.Value := '%s';
        Oper.Size := 4;
        if (sib.Index <> 4) or (rexX in Flags)
        then Oper.Flags := [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar] // [reg + base]
        else Oper.Flags := [hvfSigned, hvfIncludeHexchar];                   // [base]
      end
      else begin
        if AddrSize = as32
        then Oper.Value := StdReg(sib.Base, reg32, rexB in Flags)
        else Oper.Value := StdReg(sib.Base, reg64, rexB in Flags);
        if (sib.Index <> 4) or (rexX in Flags)
        then Oper.Value := '+' + Oper.Value;  // [reg + base]
      end;

      // reg
      if (rexX in Flags) or (sib.Index <> 4)
      then begin
        if sib.Scale > 0
        then Oper.Value := Format('*%u', [1 shl sib.Scale]) + Oper.Value;

        // get index
        if AddrSize = as32
        then Oper.Value := StdReg(sib.Index, reg32, rexX in Flags) + Oper.Value
        else Oper.Value := StdReg(sib.Index, reg64, rexX in Flags) + Oper.Value;
      end;
    end
    else begin
      // no sib
      if AddrSize = as32
      then Oper.Value := StdReg(rm, reg32, rexB in Flags)
      else Oper.Value := StdReg(rm, reg64, rexB in Flags);
    end;
    
    case mode of
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
  //===================

  procedure AddAp;
  begin
    if OperandSize32 = os16 //XXXX:XXXX
    then AddOperand('$%1:s:%0:s', os32, 2, [], [], 2)
    else AddOperand('$%1:s:%0:s', os48, 4, [], [], 2)
  end;
  
  procedure AddCd_q;
  begin
    AddModReg(regControl, rexR in Flags);
  end;

  procedure AddDd_q;
  begin
    AddModReg(regDebug, rexR in Flags);
  end;
  
  procedure AddEb;
  begin
    AddModRM([modReg, modMem], os8, reg8);
  end;
  
  procedure AddEd;
  begin
    AddModRM([modReg, modMem], os32, reg32);
  end;

  procedure AddEd_q;
  begin
    if flagRex in Flags
    then AddModRM([modReg, modMem], os64, reg64)
    else AddModRM([modReg, modMem], os32, reg32);
  end;
  
  procedure AddEv;
  begin
    AddModRM([modReg, modMem], OperandSize32, OPERAND_REG[OperandSize32]);
  end;
  
  procedure AddEw;
  begin
    AddModRM([modReg, modMem], os16, reg16);
  end;
  
  procedure AddFv;
  begin
    case OperandSize32 of
      os64: AddOperand('rflags');
      os32: AddOperand('eflags');
    else
      AddOperand('flags');
    end;
  end;
  
  procedure AddGb;
  begin
    AddModReg(reg8, rexR in Flags);
  end;

  procedure AddGd;
  begin
    AddModReg(reg32, rexR in Flags);
  end;

  procedure AddGd_q;
  begin
    if flagRex in Flags
    then AddModReg(reg64, rexR in Flags)
    else AddModReg(reg32, rexR in Flags);
  end;

  procedure AddGv;
  begin
    AddModReg;
  end;
  
  procedure AddGw;
  begin
    AddModReg(reg16, rexR in Flags);
  end;

  procedure AddGz;
  begin
    if OperandSize32 = os16
    then AddModReg(reg16, rexR in Flags)
    else AddModReg(reg32, rexR in Flags);
  end;
  
  procedure AddIb;
  begin
    AddOperand('%s', os8, 1, [hvfIncludeHexchar]);
  end;
  
  procedure AddIv;
  begin
    AddOperand('%s', OPERAND_BYTES[OperandSize32], [hvfIncludeHexchar]);
  end;
  
  procedure AddIw;
  begin
    AddOperand('%s', os16, 2, [hvfIncludeHexchar]);
  end;
  
  procedure AddIz;
  begin
    if OperandSize32 = os16
    then AddOperand('%s', os16, 2, [hvfIncludeHexchar])
    else AddOperand('%s', os32, 4, [hvfIncludeHexchar]);
  end;
  
  procedure AddJb;
  begin
    AddOperand('%s', os8, 1, [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar]);
  end;
  
  procedure AddJz;
  begin
    if OperandSize32 = os16
    then AddOperand('%s', os16, 2, [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar])
    else AddOperand('%s', os32, 4, [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar]);
  end;
  
  procedure AddM;
  begin
    AddModRM([modMem], OperandSize32, reg0 {do not care});
  end;

  procedure AddMa;
  begin
    AddModRM([modMem], OperandSize32, reg0 {do not care});
  end;

  procedure AddMb;
  begin
    AddModRM([modMem], os8, reg0 {do not care});
  end;

  procedure AddMd;
  begin
    AddModRM([modMem], os32, reg0 {do not care});
  end;
  
  procedure AddMd_q;
  begin
    if flagRex in Flags
    then AddModRM([modMem], os64, reg0 {do not care})
    else AddModRM([modMem], os32, reg0 {do not care});
  end;

  procedure AddMdq;
  begin
    AddModRM([modMem], os128, reg0 {do not care})
  end;

  procedure AddMp;
  begin
    if OperandSize32 = os16 //XXXX:XXXX
    then AddModRM([modMem], os32, reg0 {do not care})
    else AddModRM([modMem], os48, reg0 {do not care});
  end;

  procedure AddMq;
  begin
    AddModRM([modMem], os64, reg0 {do not care});
  end;

  procedure AddMs;
  begin
    if A64Bit
    then AddModRM([modMem], os80, reg0 {do not care})
    else AddModRM([modMem], os48, reg0 {do not care});
  end;

  procedure AddMw_Rv;
  begin
    if Code[ModRMIdx] shr 6 = 3 // mode = 3 -> reg
    then AddModRM([modReg], OperandSize32, OPERAND_REG[OperandSize32])
    else AddModRM([modMem], os16, reg0 {do not care});
  end;

  procedure AddOb;
  begin
    AddOperand('%s', os8, ADDRESS_BYTES[AddressSize32], [hvfIncludeHexchar], [ofMemory])
  end;

  procedure AddOv;
  begin
    AddOperand('%s', ADDRESS_BYTES[AddressSize32], [hvfIncludeHexchar], [ofMemory])
  end;

  procedure AddPd_q;
  begin
    if flagRex in Flags
    then AddModReg(regMmx, os64)
    else AddModReg(regMmx, os32);
  end;

  procedure AddPq;
  begin
    AddModReg(regMmx, False);
  end;

  procedure AddPRq;
  begin
    AddModRM([modReg], os64, regMmx);
  end;
  
  procedure AddQd;
  begin
    AddModRM([modReg, modMem], os32, regMmx);
  end;
  
  procedure AddQq;
  begin
    AddModRM([modReg, modMem], os64, regMmx);
  end;

  procedure AddRd_q;
  begin
    if A64Bit
    then AddModRM([modReg], os64, reg64)
    else AddModRM([modReg], os32, reg32);
  end;
  
  procedure AddSw;
  begin
    AddModReg(regSegment, False);
  end;

  procedure AddVd_q;
  begin
    if flagRex in Flags
    then AddModReg(regXmm, os64)
    else AddModReg(regXmm, os32);
  end;

  procedure AddVdq;
  begin
    AddModReg(regXmm, os128);
  end;

  procedure AddVdq_sd;
  begin
    AddModReg(regXmm, os64); // only lower 64 bit
  end;

  procedure AddVdq_ss;
  begin
    AddModReg(regXmm, os32); // only lower 32 bit
  end;

  procedure AddVpd;
  begin
    AddModReg(regXmm, os128);
  end;

  procedure AddVps;
  begin
    AddModReg(regXmm, os128);
  end;

  procedure AddVq;
  begin
    AddModReg(regXmm, os64);
  end;

  procedure AddVsd;
  begin
    AddModReg(regXmm, os64);
  end;

  procedure AddVss;
  begin
    AddModReg(regXmm, os32);
  end;

  procedure AddVRdq;
  begin
    AddModRM([modReg], os128, regXmm);
  end;

  procedure AddVRpd;
  begin
    AddModRM([modReg], os128, regXmm);
  end;

  procedure AddVRps;
  begin
    AddModRM([modReg], os128, regXmm);
  end;

  procedure AddVRq;
  begin
    AddModRM([modReg], os64, regXmm);
  end;

  procedure AddWdq;
  begin
    AddModRM([modReg, modMem], os128, regXmm);
  end;
  
  procedure AddWpd;
  begin
    AddModRM([modReg, modMem], os128, regXmm);
  end;

  procedure AddWps;
  begin
    AddModRM([modReg, modMem], os128, regXmm);
  end;

  procedure AddWq;
  begin
    AddModRM([modReg, modMem], os64, regXmm);
  end;

  procedure AddWsd;
  begin
    AddModRM([modReg, modMem], os64, regXmm);
  end;

  procedure AddWss;
  begin
    AddModRM([modReg, modMem], os32, regXmm);
  end;

{$ifdef verbose_string_instructions}
  procedure AddXb;
  begin
    AddOperand('Xb');
  end;
  
  procedure AddXv;
  begin
    AddOperand('Xv');
  end;
  
  procedure AddXz;
  begin
    AddOperand('Xz');
  end;
  
  procedure AddYb;
  begin
    AddOperand('Yb');
  end;
  
  procedure AddYv;
  begin
    AddOperand('Yv');
  end;
  
  procedure AddYz;
  begin
    AddOperand('Yz');
  end;
{$endif}
  //===================
  
  procedure AddStdOperands(AIndex: Byte);
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
  
  //===================

  procedure DoX87;
  const
    INVALID = '**x87**';
    RESERVED = '-x87-';
  var
    Index: Byte;
    ModRM: Byte;
    
    procedure AddMem14_28Env;
    begin
      AddModRM([modMem], OperandSize32, reg0 {do not care});
    end;

    procedure AddMem98_108Env;
    begin
      AddModRM([modMem], OperandSize32, reg0 {do not care});
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

    procedure AddReg(AIndex: Byte);
    begin
      AddOperand(Format('st(%u)', [index]), os80);
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
      OPC: array[0..7] of String = ('fadd', 'fmul', 'fcom', 'fcomp', 'fsub', 'fsubr', 'fdiv', 'fdivr');
    begin
      Opcode := OPC[Index];
      case ModRM of
        $00..$BF: AddMem32
      else
        AddReg0; AddRegN;
      end;
    end;

    procedure DoD9;
    const
      OPC: array[0..7] of String = ('fld', 'fxch', 'fst', 'fstp', 'fldenv', 'fldcw', 'fnstenv', 'fnstcw');
      OPCx: array[0..$1F] of String = (
        'fchs', 'fabs', '', '', 'ftst', 'fxam', '', '',
        'fld1', 'fldl2t', 'fldl2e', 'fldpi', 'fldlg2', 'fldln2', 'fldz', '',
        'f2xm1', 'fyl2x', 'fptan', 'fpatan', 'fxtract', 'fprem1', 'fdecstp', 'fincstp',
        'fprem', 'fyl2xp1', 'fsqrt', 'fsincos', 'frndint', 'fscale', 'fsin', 'fcos'
      );
    begin
      case ModRM of
        $00..$BF: begin
          Opcode := OPC[Index];
          case Index of
            0, 2, 3: AddMem32;
            1: Opcode := INVALID;
            4, 6 : AddMem14_28Env;
            5, 7: AddMem16;
          end;
        end;
        $C0..$CF: begin Opcode := OPC[Index]; AddReg0; AddRegN; end;
        $D0:      begin Opcode := 'nop'; end;
        $D8..$DF: begin Opcode := RESERVED; end;
        $E0..$E1,
        $E4..$E5,
        $E8..$FF: begin Opcode := OPCx[ModRM and $1F]; end;
      else
        Opcode := INVALID;
      end;
    end;

    procedure DoDA;
    const
      OPC: array[0..7] of String = ('fiadd', 'fimull', 'ficom', 'ficomp', 'fisub', 'fisubr', 'fidiv', 'fidivr');
      OPCx: array[0..3] of string = ('fcmovb', 'fcmove', 'fcmovbe', 'fcmovu');
    begin
      case ModRM of
        $00..$BF: begin Opcode := OPC[Index];  AddMem32;         end;
        $C0..$DF: begin Opcode := OPCx[Index]; AddReg0; AddRegN; end;
        $E9:      begin Opcode := 'fucompp'; end;
      else
        Opcode := INVALID;
      end;
    end;

    procedure DoDB;
    const
      OPC: array[0..7] of String = ('fild', 'fisttp', 'fist', 'fistp', '', 'fld', '', 'fstp');
      OPCx: array[0..7] of String = ('fcmovnb', 'fcmovne', 'fcmovnbe', 'fcmovnu', '', 'fucomi', 'fcomi', '');
    begin
      case ModRM of
        $00..$BF: begin
          case Index of
            0..3: begin Opcode := OPC[Index]; AddMem32; end;
            5, 7: begin Opcode := OPC[Index]; AddMem80; end;
          else
            Opcode := INVALID;
          end;
        end;
        $C0..$DF,
        $E8..$F7: begin Opcode := OPCx[Index];  AddReg0; AddRegN; end;
        $E0..$E1: begin Opcode := RESERVED; end;
        $E2:      begin Opcode := 'fnclex'; end;
        $E3:      begin Opcode := 'fninit'; end;
        $E4:      begin Opcode := RESERVED; end;
      else
        Opcode := INVALID;
      end;
    end;

    procedure DoDC;
    const
      OPC: array[0..7] of String = ('fadd', 'fmul', 'fcom', 'fcomp', 'fsub', 'fsubr', 'fdiv', 'fdivr');
      OPCx: array[0..7] of String = ('fadd', 'fmul', '', '', 'fsubr', 'fsub', 'fdivr', 'fdiv');
    begin
      case ModRM of
        $00..$BF: begin Opcode := OPC[Index]; AddMem64; end;
        $C0..$CF,
        $E0..$FF: begin Opcode := OPCx[Index]; AddRegN; AddReg0; end;
      else
        Opcode := RESERVED;
      end;
    end;

    procedure DoDD;
    const
      OPC: array[0..7] of String = ('fld', 'fisttp', 'fst', 'fstp', 'frstor', '', 'fnsave', 'fnstsw');
      OPCx: array[0..7] of String = ('ffree', '', 'fst', 'fstp', '', 'fucomp', '', '');
    begin
      case ModRM of
        $00..$BF: begin
          case Index of
            0..3: begin Opcode := OPC[Index]; AddMem64; end;
            4, 6: begin Opcode := OPC[Index]; AddMem98_108Env; end;
            5: Opcode := INVALID;
            7:    begin Opcode := OPC[Index]; AddMem16; end;
          end;
        end;
        $C0..$C7,
        $D0..$DF,
        $E8..$EF: begin Opcode := OPCx[Index]; AddRegN; end;
        $E0..$E7: begin Opcode := OPCx[Index]; AddRegN; AddReg0; end;
        $C8..$CF: Opcode := RESERVED;
      else
        Opcode := INVALID;
      end
    end;

    procedure DoDE;
    const
      OPC: array[0..7] of String = ('fiadd', 'fimull', 'ficom', 'ficomp', 'fisub', 'fisubr', 'fidiv', 'fidivr');
      OPCx: array[0..7] of String = ('faddp', 'fmullp', '', '', 'fsubrp', 'fsubp', 'fdivrp', 'fdivp');
    begin
      case ModRM of
        $00..$BF: begin Opcode := OPC[Index]; AddMem16; end;
        $C0..$CF,
        $E0..$FF: begin Opcode := OPCx[Index]; AddRegN; AddReg0; end;
        $D9:      begin Opcode := 'fcompp'; end;
        $D0..$D7: Opcode := RESERVED;
      else
        Opcode := INVALID;
      end;
    end;

    procedure DoDF;
    const
      OPC: array[0..7] of String = ('fild', 'fisttp', 'fist', 'fistp', 'fbld', 'fild', 'fbstp', 'fistp');
    begin
      case ModRM of
        $00..$BF: begin
          case Index of
            0..3: begin Opcode := OPC[Index]; AddMem16; end;
            4, 6: begin Opcode := OPC[Index]; AddMem80; end;
            5, 7: begin Opcode := OPC[Index]; AddMem64; end;
          end;
        end;
        $E0:      begin Opcode := 'fnstsw';  AddOperand('ax', os16); end;
        $E8..$EF: begin Opcode := 'fucomip'; AddReg0; AddRegN; end;
        $F0..$F7: begin Opcode := 'fcomip';  AddReg0; AddRegN; end;
        $C0..$DF: Opcode := RESERVED;
      else
        Opcode := INVALID;
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
      Opcode := '!x87!';
    end;
  end;
  
  procedure Do3DNow;
  var
    n, idx: Byte;
  begin
    // 0Fh 0Fh [ModRM] [SIB] [displacement] imm8_opcode
    // sigh, we need to get the operands first, luckely they are all te same.
    AddPq;
    AddQq;
    // to adjust the instruction length, add an empty operand for the opcode
    AddOperand('', 1);
    // calc index of imm_opcode
    idx := 0;
    if flagModRM in Flags then Inc(idx);
    if flagSib in Flags then Inc(idx);
    for n := 1 to OperIdx do
    begin
      Inc(idx, Operand[n].ByteCount);
      Inc(idx, Operand[n].ByteCount2);
    end;
    // now we can lookup the opcode
    case Code[CodeIdx + idx] of
      $0C: Opcode := 'pi2fw';
      $0D: Opcode := 'pi2fd';
      $1C: Opcode := 'pf2iw';
      $1D: Opcode := 'pf2id';
      $8A: Opcode := 'pfnacc';
      $8E: Opcode := 'pfpnacc';
      $90: Opcode := 'pfcmpge';
      $94: Opcode := 'pfmin';
      $96: Opcode := 'pfrcp';
      $97: Opcode := 'pfrsqrt';
      $9A: Opcode := 'pfsub';
      $9E: Opcode := 'pfadd';
      $A0: Opcode := 'pgcmpgt';
      $A4: Opcode := 'pfmax';
      $A6: Opcode := 'pfrcpit1';
      $A7: Opcode := 'pfrsqit1';
      $AA: Opcode := 'pfsubr';
      $AE: Opcode := 'pfacc';
      $B0: Opcode := 'pfcmpeq';
      $B4: Opcode := 'pfmul';
      $B6: Opcode := 'pfrcpit2';
      $B7: Opcode := 'pmulhrw';
      $BB: Opcode := 'pswapd';
      $BF: Opcode := 'pavgusb';
    else
      Opcode := '-3dnow-';
    end;
  end;
  
  // Group
  
  procedure DoGroup1;
  const
    OPC: array[0..7] of String = ('add', 'or', 'adc', 'sbb', 'and', 'sub', 'xor', 'cmp');
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
        Opcode := 'pop';
        AddEv;
      end
      else Opcode := '**group1a**';
      Exit;
    end;

    // Group 1
    Opcode := OPC[Index];
    case Code[CodeIdx] of
      $80: begin AddEb; AddIb; end;
      $81: begin AddEv; AddIz; end;
      $82: begin AddEb; AddIb; Opcode := Check32(Opcode); end;
      $83: begin AddEv; AddIb; end;
    else
      Opcode := '!group1!';
      Exit;
    end;
    if (Index <> 7)
    then  Opcode := CheckLock(Opcode);
  end;
  
  procedure DoGroup2;
  const
    OPC: array[0..7] of String = ('rol', 'ror', 'rcl', 'rcr', 'shl', 'shr', 'sal', 'sar');
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    Index := (Code[ModRMIdx] shr 3) and 7;
    Opcode := OPC[Index];
    case Code[CodeIdx] of
      $C0: begin AddEb; AddIb; end;
      $C1: begin AddEv; AddIb; end;
      $D0: begin AddEb; AddOperand('1', os8); end;
      $D1: begin AddEv; AddOperand('1', os8); end;
      $D2: begin AddEb; AddOperand('cl', os8); end;
      $D3: begin AddEv; AddOperand('cl', os8); end;
    else
      Opcode := '!group2!';
    end;
  end;
  
  procedure DoGroup3;
  const
    OPC: array[0..7] of String = ('test', 'test', 'not', 'neg', 'mul', 'imul', 'div', 'idiv');
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    Index := (Code[ModRMIdx] shr 3) and 7;
    Opcode := OPC[Index];
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
      Opcode := '!group3!';
      Exit;
    end;
    if (Index = 2) or (Index = 3)
    then Opcode := CheckLock(Opcode);
  end;
  
  procedure DoGroup4;
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    if Code[CodeIdx] <> $FE
    then begin
      Opcode := '!group4!';
      Exit;
    end;
    
    Index := (Code[ModRMIdx] shr 3) and 7;
    case Index of
      0: Opcode := 'inc';
      1: Opcode := 'dec';
    else
      Opcode := '**group4**';
      Exit;
    end;
    AddEb;
    Opcode := CheckLock(Opcode);
  end;
  
  procedure DoGroup5;
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    if Code[CodeIdx] <> $FF
    then begin
      Opcode := '!group5!';
      Exit;
    end;

    Index := (Code[ModRMIdx] shr 3) and 7;
    case Index of
      0: begin AddEv; Opcode := CheckLock('inc'); end;
      1: begin AddEv; Opcode := CheckLock('dec'); end;
      2: begin AddEv; Opcode := 'call';           end;
      3: begin AddMp; Opcode := 'call';           end;
      4: begin AddEv; Opcode := 'jmp';            end;
      5: begin AddMp; Opcode := 'jmp';            end;
      6: begin AddEv; Opcode := 'push';           end;
    else
      Opcode := '**group5**';
      Exit;
    end;
  end;

  procedure DoGroup6;
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    if Code[CodeIdx] <> $00
    then begin
      Opcode := '!group6!';
      Exit;
    end;
    Index := (Code[ModRMIdx] shr 3) and 7;
    case Index of
      0: begin AddMw_Rv; Opcode := 'sldt'; end;
      1: begin AddMw_Rv; Opcode := 'str';  end;
      2: begin AddEw;    Opcode := 'lldt'; end;
      3: begin AddEw;    Opcode := 'ltr';  end;
      4: begin AddEw;    Opcode := 'verr'; end;
      5: begin AddEw;    Opcode := 'verw'; end;
    else
      Opcode := '**group6**';
      Exit;
    end;
  end;

  procedure DoGroup7;
  const
    RM3: array [0..7] of String = ('vmrun', 'vmmcall', 'vmload', 'vmsave', 'stgi', 'clgi', 'skinit', 'invlpga');
  var
    Mode: Byte;
    Index: Byte;
    RM: Byte;
  begin
    Include(Flags, flagModRM);
    if Code[CodeIdx] <> $01
    then begin
      Opcode := '!group7!';
      Exit;
    end;
    Mode :=  (Code[ModRMIdx] shr 6) and 3;
    Index := (Code[ModRMIdx] shr 3) and 7;
    RM :=    (Code[ModRMIdx]      ) and 7;
    case Index of
      0: begin AddMs; Opcode := 'sgdt'; end;
      1: begin AddMs; Opcode := 'sidt';  end;
      2: begin AddMs; Opcode := 'lgdt'; end;
      3: begin
        if Mode = 3
        then begin
          Opcode := RM3[RM];
        end
        else begin
          AddMs; Opcode := 'lidt';
        end;
      end;
      4: begin AddMw_Rv; Opcode := 'smsw'; end;
      //5 : invalid
      6: begin AddEw;    Opcode := 'lmsw'; end;
      7: begin
        if Mode = 3
        then begin
          case RM of
            0: Opcode := 'swapgs';
            1: Opcode := 'rdtscp';
          else
            Opcode := '**group7**';
          end;
        end
        else begin
          AddMb; Opcode := 'invlpg';
        end;
      end;
    else
      Opcode := '**group7**';
      Exit;
    end;
  end;
  
  procedure DoGroup8;
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    if Code[CodeIdx] <> $BA
    then begin
      Opcode := '!group8!';
      Exit;
    end;
    Index := (Code[ModRMIdx] shr 3) and 7;
    if Index < 4
    then begin
      Opcode := '**group8**';
      Exit;
    end;
    AddEv; AddIb;
    case Index of
      4: Opcode := 'bt';
      5: Opcode := CheckLock('bts');
      6: Opcode := CheckLock('btr');
      7: Opcode := CheckLock('btc');
    end;
  end;

  procedure DoGroup9;
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    if Code[CodeIdx] <> $C7
    then begin
      Opcode := '!group9!';
      Exit;
    end;
    Index := (Code[ModRMIdx] shr 3) and 7;
    if Index = 1
    then begin
      if OperandSize32 = os64
      then begin
        Opcode := CheckLock('cmpxchg16b');
        AddMdq;
      end
      else begin
        Opcode := CheckLock('cmpxchg8b');
        AddMq;
      end;
    end
    else Opcode := '**group9**';
  end;

  procedure DoGroup10;
  begin
    Include(Flags, flagModRM);
    if Code[CodeIdx] <> $B9
    then begin
      Opcode := '!group10!';
      Exit;
    end;
    // whole goup is invalid ??
    Opcode := '**group10**';
  end;

  procedure DoGroup11;
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    Index := (Code[ModRMIdx] shr 3) and 7;
    if Index <> 0
    then begin
      Opcode := '**group11**';
      Exit;
    end;
    
    case Code[CodeIdx] of
      $C6: begin AddEb; AddIb; end;
      $C7: begin AddEv; AddIz; end;
    else
      Opcode := '!group5!';
      Exit;
    end;
    Opcode := 'mov';
  end;
  
  procedure DoGroup12;
  const
    OPC: array[0..7] of String = ('', '', 'psrlw', '', 'psraw', '', 'psllw', '');
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    if Code[CodeIdx] <> $71
    then begin
      Opcode := '!group12!';
      Exit;
    end;
    Index := (Code[ModRMIdx] shr 3) and 7;
    case DecodePrefix(OPC[Index], '', OPC[Index], '') of
      0: begin AddPRq;  AddIb; end;
      2: begin AddVRdq; AddIb;  end;
    else
      Opcode := '**group12**';
    end;
  end;

  procedure DoGroup13;
  const
    OPC: array[0..7] of String = ('', '', 'psrld', '', 'psrad', '', 'pslld', '');
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    if Code[CodeIdx] <> $72
    then begin
      Opcode := '!group13!';
      Exit;
    end;
    Index := (Code[ModRMIdx] shr 3) and 7;
    case DecodePrefix(OPC[Index], '', OPC[Index], '') of
      0: begin AddPRq;  AddIb; end;
      2: begin AddVRdq; AddIb;  end;
    else
      Opcode := '**group13**';
    end;
  end;

  procedure DoGroup14;
  const
    OPC: array[0..7] of String = ('', '', 'psrlq', 'psrldq', '', '', 'psllq', 'psrldq');
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    if Code[CodeIdx] <> $73
    then begin
      Opcode := '!group14!';
      Exit;
    end;
    Index := (Code[ModRMIdx] shr 3) and 7;
    case DecodePrefix(OPC[Index], '', OPC[Index], '') of
      0: begin
        if (Index = 3) or (Index = 7)
        then Opcode := '**group14**'
        else begin AddPRq; AddIb; end;
      end;
      2: begin AddVRdq; AddIb; end;
    else
      Opcode := '**group14**';
    end;
  end;

  procedure DoGroup15;
  var
    Index: Byte;
    Mode: Byte;
  begin
    Include(Flags, flagModRM);
    if Code[CodeIdx] <> $AE
    then begin
      Opcode := '!group15!';
      Exit;
    end;
    Index := (Code[ModRMIdx] shr 3) and 7;
    if (Flags * [preOpr, preRep, preRepNE] <> [])
    or (Index = 4)
    then begin
      Opcode := '**group15**';
      Exit;
    end;
    Mode :=  (Code[ModRMIdx] shr 6) and 3;
    case Index of
      0: begin Opcode := 'fxsave';  AddM;  end;
      1: begin Opcode := 'fxrstor'; AddM;  end;
      2: begin Opcode := 'ldmxcsr'; AddMd; end;
      3: begin Opcode := 'stmxcsr'; AddMd; end;
      5: Opcode := 'lfence';
      6: Opcode := 'mfence';
      7: if Mode = 3 then Opcode := 'lfence'
                     else begin Opcode := 'clflush'; AddMb; end;
    end;
  end;

  procedure DoGroup16;
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    if Code[CodeIdx] <> $18
    then begin
      Opcode := '!group16!';
      Exit;
    end;
    Index := (Code[ModRMIdx] shr 3) and 7;
    if Index=0 then ;
  end;

  procedure DoGroupP;
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    if Code[CodeIdx] <> $0D
    then begin
      Opcode := '!groupp!';
      Exit;
    end;
    Index := (Code[ModRMIdx] shr 3) and 7;
    case Index of
      0: Opcode := 'prefetch exclusive';
      1,3: Opcode := 'prefetch modified';
    else
      Opcode := '--prefetch--'
    end;
  end;
  
  //---
  
  procedure Do2ByteOpcode;
  const
    INVALID = '**2byte**';

  const
    OPR_6x: array[0..$F] of String = (
      'punpcklbw', 'punpcklwd', 'punpcklqd', 'packsswb',
      'pcmpgtb', 'pcmpgtw', 'pcmpgtd', 'packuswb',
      'punpkhbw', 'punpkhwd', 'punpkhdq', 'packssdw',
      'punpcklqdq', 'punpckhqdq', '', ''
    );
    OPR_Dx: array[0..$F] of String = (
      '', 'psrlw', 'psrld', 'psrlq',
      'paddq', 'pmullw', '', '',
      'psubusb', 'psubusw', 'pminub', 'pand',
      'paddusb', 'paddusw', 'pmaxub', 'pandn'
    );
    OPR_Ex: array[0..$F] of String = (
      'pavgb', 'psraw', 'psrad', 'pavgw',
      'pmulhuw', 'pmulhw', '', '',
      'psubsb', 'psubsw', 'pminsw', 'por',
      'paddsb', 'paddsw', 'pmaxsw', 'pxor'
    );
    OPR_Fx: array[0..$F] of String = (
      '', 'psllw', 'pslld', 'psllq',
      'pmuludq', 'pmaddwd', 'psadbw', '',
      'psubb', 'psubw', 'psubd', 'psubq',
      'paddb', 'paddw', 'paddd', ''
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
        Opcode := 'lar';
        AddGv; AddEw;
      end;
      $03: begin
        Opcode := 'lsl';
        AddGv; AddEw;
      end;
      // $04: invalid
      $05: begin
        Opcode := 'syscall';
      end;
      $06: begin
        Opcode := 'clts';
      end;
      $07: begin
        Opcode := 'sysret';
      end;
      $08: begin
        Opcode := 'invd';
      end;
      $09: begin
        Opcode := 'wbinvd';
      end;
      // $0A: invalid
      $0B: begin
        Opcode := 'ud2';
      end;
      // $0C: invalid
      $0D: begin
        DoGroupP;
      end;
      $0E: begin
        Opcode := 'femms';
      end;
      $0F: begin
        Do3DNow;
      end;
      //---
      $10: begin
        case DecodePrefix('movups', 'movss', 'movupd', 'movsd') of
          0: begin AddVps;    AddWps; end;
          1: begin AddVdq_ss; AddWss; end;
          2: begin AddVpd;    AddWpd; end;
          3: begin AddVdq_sd; AddWsd; end;
        end;
      end;
      $11: begin
        case DecodePrefix('movups', 'movss', 'movupd', 'movsd') of
          0: begin AddWps; AddVps; end;
          1: begin AddWss; AddVss; end;
          2: begin AddWpd; AddVpd; end;
          3: begin AddWsd; AddVsd; end;
        end;
      end;
      $12: begin
        case DecodePrefix('movhlps', 'movsldup', 'movlpd', 'movddup') of
          0: begin
            // Opcode differs on type found
            // it is specified as Mq or VRq
            // So when getting Wq, we Add both and know the type
            AddVps; AddWq;
            if ofMemory in Operand[2].Flags
            then Opcode := 'movlps';
          end;
          1: begin AddVps; AddWps; end;
          2: begin AddVsd; AddMq;  end;
          3: begin AddVpd; AddWsd; end;
        end;
      end;
      $13: begin
        case DecodePrefix('movlps', '', 'movlpd', '') of
          0: begin AddMq; AddVps; end;
          2: begin AddMq; AddVsd; end;
        end;
      end;
      $14: begin
        case DecodePrefix('unpcklps', '', 'unpcklpd', '') of
          0: begin AddVps; AddWq; end;
          2: begin AddVpd; AddWq; end;
        else
          Opcode := INVALID;
        end;
      end;
      $15: begin
        case DecodePrefix('unpckhps', '', 'unpckhpd', '') of
          0: begin AddVps; AddWq; end;
          2: begin AddVpd; AddWq; end;
        else
          Opcode := INVALID;
        end;
      end;
      $16: begin
        case DecodePrefix('movlhps', 'movshdup', 'movhpd', '') of
          0: begin
            // Opcode differs on type found
            // it is specified as Mq or VRq
            // So when getting Wq, we Add both and know the type
            AddVps; AddWq;
            if ofMemory in Operand[2].Flags
            then Opcode := 'movhps';
          end;
          1: begin AddVps; AddWps; end;
          2: begin AddVsd; AddMq; end;
        else
          Opcode := INVALID;
        end;
      end;
      $17: begin
        case DecodePrefix('movhps', '', 'movhpd', '') of
          0: begin AddMq; AddVps; end;
          2: begin AddMq; AddVsd; end;
        else
          Opcode := INVALID;
        end;
      end;
      $18: begin
        DoGroup16;
      end;
      $19..$1F: begin
        Include(Flags, flagModRM);
        Opcode := 'nop';
      end;
      //---
      $20: begin
        Opcode := 'mov';
        AddRd_q; AddCd_q;
      end;
      $21: begin
        Opcode := 'mov';
        AddRd_q; AddDd_q;
      end;
      $22: begin
        Opcode := 'mov';
        AddCd_q; AddRd_q;
      end;
      $23: begin
        Opcode := 'mov';
        AddDd_q; AddRd_q;
      end;
      // $24..$27: invalid
      $28: begin
        case DecodePrefix('movaps', '', 'movapd', '') of
          0: begin AddVps; AddWps; end;
          2: begin AddVpd; AddWpd; end;
        else
          Opcode := INVALID;
        end;
      end;
      $29: begin
        case DecodePrefix('movaps', '', 'movapd', '') of
          0: begin AddWps; AddVps; end;
          2: begin AddWpd; AddVpd; end;
        else
          Opcode := INVALID;
        end;
      end;
      $2A: begin
        case DecodePrefix('cvtpi2ps', 'cvtsi2ss', 'cvtpi2pd', 'cvtsi2sd') of
          0: begin AddVps; AddQq;   end;
          1: begin AddVss; AddEd_q; end;
          2: begin AddVpd; AddQq;   end;
          3: begin AddVsd; AddEd_q; end;
        end;
      end;
      $2B: begin
        case DecodePrefix('movntps', '', 'movntpd', '') of
          0: begin AddMdq; AddVps; end;
          2: begin AddMdq; AddVpd; end;
        else
          Opcode := INVALID;
        end;
      end;
      $2C: begin
        case DecodePrefix('cvttps2pi', 'cvttss2pi', 'cvttpd2pi', 'cvttsd2pi') of
          0: begin AddPq;   AddWps; end;
          1: begin AddGd_q; AddWss; end;
          2: begin AddPq;   AddWpd; end;
          3: begin AddGd_q; AddWsd; end;
        end;
      end;
      $2D: begin
        case DecodePrefix('cvtps2pi', 'cvtss2pi', 'cvtpd2pi', 'cvtsd2pi') of
          0: begin AddPq;   AddWps; end;
          1: begin AddGd_q; AddWss; end;
          2: begin AddPq;   AddWpd; end;
          3: begin AddGd_q; AddWsd; end;
        end;
      end;
      $2E: begin
        case DecodePrefix('ucomiss', '', 'ucomissd', '') of
          0: begin AddVss; AddWss; end;
          2: begin AddVsd; AddWsd; end;
        else
          Opcode := INVALID;
        end;
      end;
      $2F: begin
        case DecodePrefix('comiss', '', 'comissd', '') of
          0: begin AddVss; AddWss; end;
          2: begin AddVsd; AddWsd; end;
        else
          Opcode := INVALID;
        end;
      end;
      //---
      $30: begin
        Opcode := 'wrmsr';
      end;
      $31: begin
        Opcode := 'rdtsc';
      end;
      $32: begin
        Opcode := 'rdmsr';
      end;
      $33: begin
        Opcode := 'rdpmc';
      end;
      $34: begin
        Opcode := '**sysenter**';
      end;
      $35: begin
        Opcode := '**sysexit**';
      end;
      // $36..$3F: invalid
      //---
      $40..$4F: begin
        Opcode := 'cmov' + StdCond(Code[CodeIdx]);
        AddGv; AddEv;
      end;
      //---
      $50: begin
        case DecodePrefix('movmskps', '', 'movmskpd', '') of
          0: begin AddGd; AddVRps; end;
          2: begin AddGd; AddVRpd; end;
        else
          Opcode := INVALID;
        end;
      end;
      $51, $58..$59, $5C..$5F: begin
        case Code[CodeIdx] of
          $51: Idx := DecodePrefix('sqrtps', 'sqrtss', 'sqrtpd', 'sqrtsd');
          $58: Idx := DecodePrefix('addps', 'addss', 'addpd', 'addsd');
          $59: Idx := DecodePrefix('mulps', 'mulss', 'mulpd', 'mulsd');
          $5C: Idx := DecodePrefix('subps', 'subss', 'subpd', 'subsd');
          $5D: Idx := DecodePrefix('minps', 'minss', 'minpd', 'minsd');
          $5E: Idx := DecodePrefix('divps', 'divss', 'divpd', 'divsd');
          $5F: Idx := DecodePrefix('maxps', 'maxss', 'maxpd', 'maxsd');
        else
          Idx := -1;
        end;

        case Idx of
          0: begin AddVps; AddWps; end;
          1: begin AddVss; AddWss; end;
          2: begin AddVpd; AddWpd; end;
          3: begin AddVsd; AddWsd; end;
        else
          Opcode := INVALID;
        end;
      end;
      $52: begin
        case DecodePrefix('rsqrtps', 'rsqrtss', '', '') of
          0: begin AddVps; AddWps; end;
          1: begin AddVss; AddWss; end;
        else
          Opcode := INVALID;
        end;
      end;
      $53: begin
        case DecodePrefix('rcpps', 'rcpss', '', '') of
          0: begin AddVps; AddWps; end;
          1: begin AddVss; AddWss; end;
        else
          Opcode := INVALID;
        end;
      end;
      $54: begin
        case DecodePrefix('andps', '', 'andpd', '') of
          0: begin AddVps; AddWps; end;
          2: begin AddVpd; AddWpd; end;
        else
          Opcode := INVALID;
        end;
      end;
      $55: begin
        case DecodePrefix('andnps', '', 'andnpd', '') of
          0: begin AddVps; AddWps; end;
          2: begin AddVpd; AddWpd; end;
        else
          Opcode := INVALID;
        end;
      end;
      $56: begin
        case DecodePrefix('orps', '', 'orpd', '') of
          0: begin AddVps; AddWps; end;
          2: begin AddVpd; AddWpd; end;
        else
          Opcode := INVALID;
        end;
      end;
      $57: begin
        case DecodePrefix('xorps', '', 'xorpd', '') of
          0: begin AddVps; AddWps; end;
          2: begin AddVpd; AddWpd; end;
        else
          Opcode := INVALID;
        end;
      end;
      // $58..$59: see $51
      $5A: begin
        case DecodePrefix('cvtps2pd', 'cvtss2sd', 'cvtpd2ps', 'cvtsd2ss') of
          0: begin AddVpd; AddWps; end;
          1: begin AddVsd; AddWss; end;
          2: begin AddVps; AddWpd; end;
          3: begin AddVss; AddWsd; end;
        end;
      end;
      $5B: begin
        case DecodePrefix('cvtdq2ps', 'cvttps2dq', 'cvtps2dq', '') of
          0: begin AddVps; AddWdq; end;
          1: begin AddVdq; AddWps; end;
          2: begin AddVdq; AddWps; end;
        else
          Opcode := INVALID;
        end;
      end;
      // $5C..$5F: see $51
      //---
      $60..$6D: begin
        idx := Code[CodeIdx] and $F;
        idx := DecodePrefix(OPR_6x[idx], '', OPR_6x[idx], '');

        if (idx = 0) and (Code[CodeIdx] in [$6C, $6D])
        then idx := -1;

        case Idx of
          0: begin AddPq;  AddQd; end;
          2: begin
            AddVdq;
            if Code[CodeIdx] = $6B then AddWdq else AddWq;
          end;
        else
          Opcode := INVALID;
        end;
      end;
      $6E: begin
        case DecodePrefix('movd', '', 'movd', '') of
          0: begin AddPq;  AddEd_q; end;
          2: begin AddVdq; AddEd_q; end;
        else
          Opcode := INVALID;
        end;
      end;
      $6F: begin
        case DecodePrefix('movq', 'movdqu', 'movdqa', '') of
          0: begin AddPq;  AddQq;  end;
          1: begin AddVdq; AddWdq; end;
          2: begin AddVdq; AddWdq; end;
        else
          Opcode := INVALID;
        end;
      end;
      //---
      $70: begin
        case DecodePrefix('pshufw', 'pshufhw', 'pshufd', 'pshuflw') of
          0: begin AddPq;  AddQq;  AddIb; end;
          1: begin AddVq;  AddWq;  AddIb; end;
          2: begin AddVdq; AddWdq; AddIb; end;
          3: begin AddVq;  AddWq;  AddIb; end;
        end;
      end;
      $71: begin
        if Flags * [preRep, preRepNE] = []
        then DoGroup12
        else Opcode := INVALID;
      end;
      $72: begin
        if Flags * [preRep, preRepNE] = []
        then DoGroup13
        else Opcode := INVALID;
      end;
      $73: begin
        if Flags * [preRep, preRepNE] = []
        then DoGroup14
        else Opcode := INVALID;
      end;
      $74: begin
        case DecodePrefix('pcmpeqb', '', 'pcmpeqb', '') of
          0: begin AddPq;  AddQq;  end;
          2: begin AddVdq; AddWdq; end;
        else
          Opcode := INVALID;
        end;
      end;
      $75: begin
        case DecodePrefix('pcmpeqw', '', 'pcmpeqw', '') of
          0: begin AddPq;  AddQq;  end;
          2: begin AddVdq; AddWdq; end;
        else
          Opcode := INVALID;
        end;
      end;
      $76: begin
        case DecodePrefix('pcmpeqd', '', 'pcmpeqd', '') of
          0: begin AddPq;  AddQq;  end;
          2: begin AddVdq; AddWdq; end;
        else
          Opcode := INVALID;
        end;
      end;
      $77: begin
        if Flags * [preRep, preRepNE, preOpr] = []
        then Opcode := 'emms'
        else Opcode := INVALID;
      end;
      // $78..$7B: invalid
      $7C: begin
        case DecodePrefix('', '', 'haddpd', 'haddps') of
          2: begin AddVpd; AddWpd; end;
          3: begin AddVps; AddWps; end;
        else
          Opcode := INVALID;
        end;
      end;
      $7D: begin
        case DecodePrefix('', '', 'hsubpd', 'hsubps') of
          2: begin AddVpd; AddWpd; end;
          3: begin AddVps; AddWps; end;
        else
          Opcode := INVALID;
        end;
      end;
      $7E: begin
        case DecodePrefix('movd', 'movq', 'movd', '') of
          0: begin AddEd_q; AddPd_q; end;
          1: begin AddVq;   AddWq;   end;
          2: begin AddEd_q; AddVd_q; end;
        else
          Opcode := INVALID;
        end;
      end;
      $7F: begin
        case DecodePrefix('movq', 'movdqu', 'movdqa', '') of
          0: begin AddQq;  AddPq;  end;
          1: begin AddWdq; AddVdq; end;
          2: begin AddWdq; AddVdq; end;
        else
          Opcode := INVALID;
        end;
      end;
      //---
      $80..$8F: begin
        Opcode := 'j' + StdCond(Code[CodeIdx]);
        AddJz;
      end;
      //---
      $90..$9F: begin
        Opcode := 'set' + StdCond(Code[CodeIdx]);
        AddEb;
      end;
      //---
      $A0: begin
        Opcode := 'push';
        AddOperand('fs');
      end;
      $A1: begin
        Opcode := 'pop';
        AddOperand('fs');
      end;
      $A2: begin
        Opcode := 'cpuid';
      end;
      $A3: begin
        Opcode := 'bt';
        AddEv; AddGv;
      end;
      $A4: begin
        Opcode := 'shld';
        AddEv; AddGv; AddIb;
      end;
      $A5: begin
        Opcode := 'shld';
        AddEv; AddGv;
        AddOperand('cl');
      end;
      // $A6..$A7: invalid
      $A8: begin
        Opcode := 'push';
        AddOperand('gs');
      end;
      $A9: begin
        Opcode := 'pop';
        AddOperand('gs');
      end;
      $AA: begin
        Opcode := 'rsm';
      end;
      $AB: begin
        Opcode := 'bts';
        AddEv; AddGv;
      end;
      $AC: begin
        Opcode := 'shrd';
        AddEv; AddGv; AddIb;
      end;
      $AD: begin
        Opcode := 'shld';
        AddEv; AddGv;
        AddOperand('cl');
      end;
      $AE: begin
        DoGroup15;
      end;
      $AF: begin
        Opcode := 'imul';
        AddGv; AddEv;
      end;
      //---
      $B0: begin
        AddEb; AddGb;
        Opcode := CheckLock('cmpxchg');
      end;
      $B1: begin
        AddEv; AddGv;
        Opcode := CheckLock('cmpxchg');
      end;
      $B2: begin
        Opcode := 'lss';
        AddGz; AddMp;
      end;
      $B3: begin
        Opcode := 'btr';
        AddEv; AddGv;
      end;
      $B4: begin
        Opcode := 'lfs';
        AddGz; AddMp;
      end;
      $B5: begin
        Opcode := 'lgs';
        AddGz; AddMp;
      end;
      $B6: begin
        Opcode := 'movzx';
        AddGv; AddEb;
      end;
      $B7: begin
        Opcode := 'movzx';
        AddGv; AddEw;
      end;
      // $B8: invalid
      $B9: begin
        DoGroup10;
      end;
      $BA: begin
        DoGroup8;
      end;
      $BB: begin
        Opcode := 'btc';
        AddEv; AddGv;
      end;
      $BC: begin
        Opcode := 'bsf';
        AddGv; AddEv;
      end;
      $BD: begin
        Opcode := 'bsr';
        AddGv; AddEv;
      end;
      $BE: begin
        Opcode := 'movsx';
        AddGv; AddEb;
      end;
      $BF: begin
        Opcode := 'movsx';
        AddGv; AddEw;
      end;
      //---
      $C0: begin
        AddEb; AddGb;
        Opcode := CheckLock('xadd');
      end;
      $C1: begin
        AddEv; AddGv;
        Opcode := CheckLock('xadd');
      end;
      $C2: begin
        case DecodePrefix('cmpps', 'cmpss', 'cmppd', 'cmpsd') of
          0: begin AddVps; AddWps; AddIb end;
          1: begin AddVss; AddWss; AddIb end;
          2: begin AddVpd; AddWpd; AddIb end;
          3: begin AddVsd; AddWsd; AddIb end;
        end;
      end;
      $C3: begin
        if Flags * [preRep, preRepNE, preOpr] = []
        then begin
          Opcode := 'movnti';
          AddMd_q; AddGd_q;
        end
        else Opcode := INVALID;
      end;
      $C4: begin
        case DecodePrefix('pinsrw', '', 'pinsrw', '') of
          0: begin AddPq;  AddEw; AddIb end;
          2: begin AddVdq; AddEw; AddIb end;
        else
          Opcode := INVALID;
        end;
      end;
      $C5: begin
        case DecodePrefix('pextrw', '', 'pextrw', '') of
          0: begin AddGd; AddPRq;  AddIb end;
          2: begin AddGd; AddVRdq; AddIb end;
        else
          Opcode := INVALID;
        end;
      end;
      $C6: begin
        case DecodePrefix('shufps', '', 'shufpd', '') of
          0: begin AddVps; AddWps; AddIb end;
          2: begin AddVpd; AddWpd; AddIb end;
        else
          Opcode := INVALID;
        end;
      end;
      $C7: begin
        DoGroup9;
      end;
      $C8..$CF: begin
        Opcode := 'bswp';
        AddStdReg(Code[CodeIdx]);
      end;
      //---
      $D0: begin
        case DecodePrefix('', '', 'addsubpd', 'addsubps') of
          2: begin AddVpd; AddWpd; end;
          3: begin AddVps; AddWps; end;
        else
          Opcode := INVALID;
        end;
      end;
      $D1..$D5, $D8..$DF: begin
        idx := Code[CodeIdx] and $F;
        idx := DecodePrefix(OPR_Dx[idx], '', OPR_Dx[idx], '');

        case Idx of
          0: begin AddPq;  AddQq;  end;
          2: begin AddVdq; AddWdq; end;
        else
          Opcode := INVALID;
        end;
      end;
      $D6: begin
        case DecodePrefix('', 'movq2dq', 'movq', 'movdq2q') of
          1: begin AddVdq; AddPRq; end;
          2: begin AddWq;  AddVq;  end;
          3: begin AddPq;  AddVRq; end;
        else
          Opcode := INVALID;
        end;
      end;
      $D7: begin
        case DecodePrefix('pmovmskb', '', 'pmovmskb', '') of
          0: begin AddGd; AddPRq;  end;
          2: begin AddGd; AddVRdq; end;
        else
          Opcode := INVALID;
        end;
      end;
      // $D8..$DF: see $D1
      //---
      $E0..$E5, $E8..$EF: begin
        idx := Code[CodeIdx] and $F;
        idx := DecodePrefix(OPR_Ex[idx], '', OPR_Ex[idx], '');

        case Idx of
          0: begin AddPq;  AddQq;  end;
          2: begin AddVdq; AddWdq; end;
        else
          Opcode := INVALID;
        end;
      end;
      $E6: begin
        case DecodePrefix('', 'cvtdq2pd', 'cvttpd2dq', 'cvtpd2dq') of
          1: begin AddVpd; AddWq;  end;
          2: begin AddVq;  AddWpd; end;
          3: begin AddVq;  AddWpd; end;
        else
          Opcode := INVALID;
        end;
      end;
      $E7: begin
        case DecodePrefix('movntq', '', 'movntdqu', '') of
          0: begin AddMq;  AddPq;  end;
          2: begin AddMdq; AddVdq; end;
        else
          Opcode := INVALID;
        end;
      end;
      // $E8..$EF: see $E0
      $F0: begin
        if preRepNE in Flags
        then begin
          Opcode := 'lddqu';
          AddVpd; AddMdq;
        end
        else Opcode := INVALID;
      end;
      $F1..$F6, $F8..$FE: begin
        idx := Code[CodeIdx] and $F;
        idx := DecodePrefix(OPR_Fx[idx], '', OPR_Fx[idx], '');

        case Idx of
          0: begin AddPq;  AddQq;  end;
          2: begin AddVdq; AddWdq; end;
        else
          Opcode := INVALID;
        end;
      end;
      $F7: begin
        case DecodePrefix('maskmovq', '', 'maskmovdqu', '') of
          0: begin AddPq;  AddPRq;  end;
          2: begin AddVdq; AddVRdq; end;
        else
          Opcode := INVALID;
        end;
      end;
      // $F8..$FE: see $F1
      // $FF: invalid
    else
      Opcode := INVALID;
    end;
  end;

  procedure DoDisassemble;
  begin
    repeat
      ModRMIdx := CodeIdx + 1;
      case Code[CodeIdx] of
        $00..$05: begin
          AddStdOperands(Code[CodeIdx]);
          Opcode := CheckLock('add');
        end;
        $06: begin
          Opcode := Check32('push');
          AddOperand('es');
        end;
        $07: begin
          Opcode := Check32('pop');
          AddOperand('es');
        end;
        $08..$0D: begin
          AddStdOperands(Code[CodeIdx]);
          Opcode := CheckLock('or');
        end;
        $0E: begin
          Opcode := Check32('push');
          AddOperand('cs');
        end;
        $0F: begin
          Do2ByteOpcode;
        end;
        //---
        $10..$15: begin
          AddStdOperands(Code[CodeIdx]);
          Opcode := CheckLock('adc');
        end;
        $16: begin
          Opcode := Check32('push');
          AddOperand('ss');
        end;
        $17: begin
          Opcode := Check32('pop');
          AddOperand('ss');
        end;
        $18..$1D: begin
          AddStdOperands(Code[CodeIdx]);
          Opcode := CheckLock('sbb');
        end;
        $1E: begin
          Opcode := Check32('push');
          AddOperand('ds');
        end;
        $1F: begin
          Opcode := Check32('pop');
          AddOperand('ds');
        end;
        //---
        $20..$25: begin
          AddStdOperands(Code[CodeIdx]);
          Opcode := CheckLock('and');
        end;
        $26: begin
          Segment := Segment + Ignore64('es:');
        end;
        $27: begin
          Opcode := Check32('daa');
        end;
        $28..$2D: begin
          AddStdOperands(Code[CodeIdx]);
          Opcode := CheckLock('sub');
        end;
        $2E: begin
          Segment := Segment + Ignore64('cs:');
        end;
        $2F: begin
          Opcode := Check32('das');
        end;
        //---
        $30..$35: begin
          AddStdOperands(Code[CodeIdx]);
          Opcode := CheckLock('xor');
        end;
        $36: begin
          Segment := Segment + Ignore64('ss:');
        end;
        $37: begin
          Opcode := Check32('aaa');
        end;
        $38..$3D: begin
          Opcode := 'cmp';
          AddStdOperands(Code[CodeIdx]);
        end;
        $3E: begin
          Segment := Segment + Ignore64('ds:');
        end;
        $3F: begin
          Opcode := Check32('aas');
        end;
        //---
        $40..$4F: begin
          if A64Bit
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
            then Opcode := CheckLock('inc')
            else Opcode := CheckLock('dec');
          end;
        end;
        //---
        $50..$57: begin
          Opcode := 'push';
          AddStdReg(Code[CodeIdx]);
        end;
        $58..$5F: begin
          Opcode := 'pop';
          AddStdReg(Code[CodeIdx]);
        end;
        //---
        $60: begin
          if OperandSize32 = os16
          then Opcode := Check32('pusha')
          else Opcode := Check32('pushad');
        end;
        $61: begin
          if OperandSize32 = os16
          then Opcode := Check32('popa')
          else Opcode := Check32('popad');
        end;
        $62: begin
          Opcode := Check32('bound');
          AddGv; AddMa;
        end;
        $63: begin
          if A64Bit
          then begin
            Opcode := ('movsxd');
            AddGv; AddEd;
          end
          else begin
            Opcode := Check32('arpl');
            AddEw; AddGw;
          end;
        end;
        $64: begin
          Segment := Segment + 'fs:';
        end;
        $65: begin
          Segment := Segment + 'gs:';
        end;
        $66: begin
          Include(FLags, preOpr);
        end;
        $67: begin
          Include(FLags, preAdr);
        end;
        $68: begin
          Opcode := 'push';
          AddIz;
        end;
        $69: begin
          Opcode := 'imul';
          AddGv; AddEv; AddIz;
        end;
        $6A: begin
          Opcode := 'push';
          AddIb;
        end;
        $6B: begin
          Opcode := 'imul';
          AddGv; AddEv; AddIb;
        end;
        $6C: begin
          Opcode := CheckRepeat('insb');
          {$ifdef verbose_string_instructions}
          AddYb;
          AddOperand('dx', os16);
          {$endif}
        end;
        $6D: begin
          if OperandSize32 = os16
          then Opcode := CheckRepeat('insw')
          else Opcode := CheckRepeat('insd');
          {$ifdef verbose_string_instructions}
          AddYz;
          AddOperand('dx', os16);
          {$endif}
        end;
        $6E: begin
          Opcode := CheckRepeat('outsb');
          {$ifdef verbose_string_instructions}
          AddOperand('dx', os16);
          AddXb;
          {$endif}
        end;
        $6F: begin
          if OperandSize32 = os16
          then Opcode := CheckRepeat('outsw')
          else Opcode := CheckRepeat('outsd');
          {$ifdef verbose_string_instructions}
          AddOperand('dx', os16);
          AddXz;
          {$endif}
        end;
        $70..$7F: begin
          Opcode := 'j' + StdCond(Code[CodeIdx]);
          AddJb;
        end;
        //---
        $80..$83: begin
          DoGroup1;
        end;
        $84: begin
          Opcode := 'test';
          AddEb; AddGb;
        end;
        $85: begin
          Opcode := 'test';
          AddEv; AddGv;
        end;
        $86: begin
          AddEb; AddGb;
          Opcode := CheckLock('xchg');
        end;
        $87: begin
          AddEv; AddGv;
          Opcode := CheckLock('xchg');
        end;
        $88..$8B: begin
          Opcode := 'mov';
          AddStdOperands(Code[CodeIdx]);
        end;
        $8C: begin
          Opcode := 'mov';
          AddMw_Rv; AddSw;
        end;
        $8D: begin
          Opcode := 'lea';
          AddGv; AddM;
        end;
        $8E: begin
          Opcode := 'mov';
          AddSw; AddEw;
        end;
        $8F: begin
          DoGroup1;
        end;
        //---
        $90..$97: begin
          if (Code[CodeIdx] = $90) and not (rexR in Flags)
          then Opcode := 'nop'
          else begin
            Opcode := 'xchg';
            AddStdReg(Code[CodeIdx]);
            AddOperand(SizeReg32('ax'));
          end;
        end;
        $98: begin
          case OperandSize32 of
            os64: Opcode := 'cdqe';
            os32: Opcode := 'cwde';
          else
            Opcode := 'cbw';
          end;
        end;
        $99: begin
          case OperandSize32 of
            os64: Opcode := 'cqo';
            os32: Opcode := 'cqd';
          else
            Opcode := 'cwd';
          end;
        end;
        $9A: begin
          Opcode := Check32('call');
          AddAp;
        end;
        $9B: begin
          Opcode := 'wait/fwait';
        end;
        $9C: begin
          case OperandSize32 of
            os64: Opcode := 'pushfq';
            os32: Opcode := 'pushfd';
          else
            Opcode := 'pushf';
          end;
          AddFv;
        end;
        $9D: begin
          case OperandSize32 of
            os64: Opcode := 'popfq';
            os32: Opcode := 'popfd';
          else
            Opcode := 'popf';
          end;
          AddFv;
        end;
        $9E: begin
          Opcode := 'sahf';
        end;
        $9F: begin
          Opcode := 'lahf';
        end;
        //---
        $A0: begin
          Opcode := 'mov';
          AddOperand('al', os8);
          AddOb;
        end;
        $A1: begin
          Opcode := 'mov';
          AddOperand(SizeReg32('ax'));
          AddOv;
        end;
        $A2: begin
          Opcode := 'mov';
          AddOb;
          AddOperand('al', os8);
        end;
        $A3: begin
          Opcode := 'mov';
          AddOv;
          AddOperand(SizeReg32('ax'));
        end;
        $A4: begin
          Opcode := CheckRepeat('movsb');
          {$ifdef verbose_string_instructions}
          AddYb; AddXb;
          {$endif}
        end;
        $A5: begin
          case OperandSize32 of
            os64: Opcode := CheckRepeat('movsq');
            os32: Opcode := CheckRepeat('movsd');
          else
            Opcode := CheckRepeat('movsw');
          end;
          {$ifdef verbose_string_instructions}
          AddYv; AddXv;
          {$endif}
        end;
        $A6: begin
          Opcode := CheckRepeatX('cmpsb');
          {$ifdef verbose_string_instructions}
          AddXb; AddYb;
          {$endif}
        end;
        $A7: begin
          case OperandSize32 of
            os64: Opcode := CheckRepeatX('cmpsq');
            os32: Opcode := CheckRepeatX('cmpsd');
          else
            Opcode := CheckRepeatX('cmpsw');
          end;
          {$ifdef verbose_string_instructions}
          AddYv; AddXv;
          {$endif}
        end;
        $A8: begin
          Opcode := 'test';
          AddOperand('al', os8);
          AddIb;
        end;
        $A9: begin
          Opcode := 'test';
          AddOperand(SizeReg32('ax'));
          AddIv;
        end;
        $AA: begin
          Opcode := CheckRepeat('stosb');
          {$ifdef verbose_string_instructions}
          AddYb;
          AddOperand('al', os8);
          {$endif}
        end;
        $AB: begin
          case OperandSize32 of
            os64: Opcode := CheckRepeat('stosq');
            os32: Opcode := CheckRepeat('stosd');
          else
            Opcode := CheckRepeat('stosw');
          end;
          {$ifdef verbose_string_instructions}
          AddYv;
          AddOperand(SizeReg32('ax'));
          {$endif}
        end;
        $AC: begin
          Opcode := CheckRepeat('lodsb');
          {$ifdef verbose_string_instructions}
          AddOperand('al', os8);
          AddXb;
          {$endif}
        end;
        $AD: begin
          case OperandSize32 of
            os64: Opcode := CheckRepeat('lodsq');
            os32: Opcode := CheckRepeat('lodsd');
          else
            Opcode := CheckRepeat('lodsw');
          end;
          {$ifdef verbose_string_instructions}
          AddOperand(SizeReg32('ax'));
          AddXv;
          {$endif}
        end;
        $AE: begin
          Opcode := CheckRepeatX('scasb');
          {$ifdef verbose_string_instructions}
          AddOperand('al', os8);
          AddYb;
          {$endif}
        end;
        $AF: begin
          case OperandSize32 of
            os64: Opcode := CheckRepeatX('scasq');
            os32: Opcode := CheckRepeatX('scasd');
          else
            Opcode := CheckRepeatX('scasw');
          end;
          {$ifdef verbose_string_instructions}
          AddOperand(SizeReg32('ax'));
          AddYv;
          {$endif}
        end;
        //---
        $B0..$B7: begin
          Opcode := 'mov';
          AddStdReg(Code[CodeIdx], reg8, rexR in Flags);
          AddIb;
        end;
        $B8..$BF: begin
          Opcode := 'mov';
          AddStdReg(Code[CodeIdx]);
          AddIv;
        end;
        //---
        $C0..$C1: begin
          DoGroup2;
        end;
        $C2: begin
          Opcode := 'ret';
          AddIw;
        end;
        $C3: begin
          Opcode := 'ret';
        end;
        $C4: begin
          Opcode := 'les';
          AddGz; AddMp;
        end;
        $C5: begin
          Opcode := 'lds';
          AddGz; AddMp;
        end;
        $C6..$C7: begin
          DoGroup11;
        end;
        $C8: begin
          Opcode := 'enter';
          AddIw; AddIb;
        end;
        $C9: begin
          Opcode := 'leave';
        end;
        $CA: begin
          Opcode := 'retf';
          AddIw;
        end;
        $CB: begin
          Opcode := 'retf';
        end;
        $CC: begin
          Opcode := 'int3';
        end;
        $CD: begin
          Opcode := 'int';
          AddIb;
        end;
        $CE: begin
          Opcode := Check32('int0');
        end;
        $CF: begin
          case OperandSize32 of
            os64: Opcode := 'iretq';
            os32: Opcode := 'iretd';
          else
            Opcode := 'iret';
          end;
        end;
        //---
        $D0..$D3: begin
          DoGroup2;
        end;
        $D4: begin
          Opcode := Check32('aam');
        end;
        $D5: begin
          Opcode := Check32('aad');
        end;
        $D6: begin
          Opcode := Check32('salc');
        end;
        $D7: begin
          Opcode := 'xlat';
        end;
        $D8..$DF: begin
          DoX87;
        end;
        //---
        $E0: begin
          Opcode := 'loopne';
          AddJb;
        end;
        $E1: begin
          Opcode := 'loope';
          AddJb;
        end;
        $E2: begin
          Opcode := 'loop';
          AddJb;
        end;
        $E3: begin
          Opcode := 'jrcxz';
          AddJb;
        end;
        $E4: begin
          Opcode := 'in';
          AddOperand('al', os8);
          AddIb;
        end;
        $E5: begin
          Opcode := 'in';
          AddOperand(SizeReg32('ax'));
          AddIb;
        end;
        $E6: begin
          Opcode := 'out';
          AddIb;
          AddOperand('al', os8);
        end;
        $E7: begin
          Opcode := 'out';
          AddIb;
          AddOperand(SizeReg32('ax'));
        end;
        $E8: begin
          Opcode := 'call';
          AddJz;
        end;
        $E9: begin
          Opcode := 'jmp';
          AddJz;
        end;
        $EA: begin
          Opcode := Check32('jmp');
          AddAp;
        end;
        $EB: begin
          Opcode := 'jmp';
          AddJb;
        end;
        $EC: begin
          Opcode := 'in';
          AddOperand('al', os8);
          AddOperand('dx', os16);
        end;
        $ED: begin
          Opcode := 'in';
          AddOperand(SizeReg32('ax'));
          AddOperand('dx', os16);
        end;
        $EE: begin
          Opcode := 'out';
          AddOperand('dx', os16);
          AddOperand('al', os8);
        end;
        $EF: begin
          Opcode := 'out';
          AddOperand('dx', os16);
          AddOperand(SizeReg32('ax'));
        end;
        $F0: begin
          Include(Flags, preLock);
        end;
        $F1: begin
          Opcode := 'int1';
        end;
        $F2: begin
          Include(Flags, preRepNE);
        end;
        $F3: begin
          Include(Flags, preRep);
        end;
        $F4: begin
          Opcode := 'hlt';
        end;
        $F5: begin
          Opcode := 'cmc';
        end;
        $F6..$F7: begin
          DoGroup3;
        end;
        $F8: begin
          Opcode := 'clc';
        end;
        $F9: begin
          Opcode := 'stc';
        end;
        $FA: begin
          Opcode := 'cli';
        end;
        $FB: begin
          Opcode := 'sti';
        end;
        $FC: begin
          Opcode := 'cld';
        end;
        $FD: begin
          Opcode := 'std';
        end;
        $FE: begin
          DoGroup4;
        end;
        $FF: begin
          DoGroup5;
        end;
      else
        Opcode := HexValue(Code[CodeIdx], 1, []);
      end;

      Inc(CodeIdx);
      if CodeIdx > 16 // max instruction length
      then begin
        Debugln(DBG_WARNINGS, 'Disassemble: instruction longer than 16 bytes');
        Exit;
      end;
    until Opcode <> '';
  end;

const
  MEMPTR: array[TOperandSize] of string = ('byte ptr ', 'word ptr ', 'dword ptr ', 'qword ptr ', '', 'tbyte ptr ', '16byte ptr ');
{$ifdef debug_OperandSize}
  OSTEXT: array[TOperandSize] of string = ('os8', 'os16', 'os32', 'os64', 'os48', 'os80', 'os128');
{$endif}
var
  S, Soper: String;
  n: Integer;
  HasMem: Boolean;
begin
  Code := AAddress;

  Segment := '';
  Opcode := '';
  Flags := [];
  CodeIdx := 0;
  OperIdx := 0;
  DoDisassemble;

  if flagModRM in Flags then Inc(CodeIdx);
  if flagSib in Flags then Inc(CodeIdx);

  Soper := '';
  HasMem := False;
  for n := 1 to OperIdx do
  begin
    if Operand[n].ByteCount = 0
    then S := Operand[n].Value
    else begin
      if Operand[n].ByteCount2 = 0
      then S := Format(Operand[n].Value, [HexValue(Code[CodeIdx], Operand[n].ByteCount, Operand[n].FormatFlags)])
      else S := Format(Operand[n].Value, [HexValue(Code[CodeIdx], Operand[n].ByteCount, Operand[n].FormatFlags), HexValue(Code[CodeIdx + Operand[n].ByteCount], Operand[n].ByteCount2, Operand[n].FormatFlags)])
    end;

    if Soper <> '' then Soper := Soper + ',';
    if ofMemory in Operand[n].Flags
    then begin
      if (OperIdx = 1)
//      or (Operand[n].Size <> os32)
      or (Operand[1].Size <> Operand[2].Size)
      then Soper := Soper + MEMPTR[Operand[n].Size];
      Soper := Soper + Segment + '[' + S + ']';
      HasMem := True;
    end
    else Soper := Soper + S;
    Inc(CodeIdx, Operand[n].ByteCount);
    Inc(CodeIdx, Operand[n].ByteCount2);
  end;
{$ifdef debug_OperandSize}
  Soper := Soper + ' | ';
  for n := 1 to OperIdx do
  begin
    Soper := Soper + ' ' + OSTEXT[Operand[n].Size];
  end;
{$endif}

  S := '';
  if preLock in Flags then S := S + '**lock**';
  if preRep in Flags then S := S + '?rep?';
  if preRepNE in Flags then S := S + '?repne?';
  S := S + Opcode;
  if not HasMem and (Segment <> '') then S := S + ' ?' + Segment + '?';
  ACode := S + ' ' + Soper;

  // memory
  S := '';
  for n := 0 to CodeIdx - 1 do
  begin
    S := S + HexStr(Code[n], 2);
  end;
  ACodeBytes := S;
  Inc(AAddress, CodeIdx);
end;

function IsCallInstruction(AAddress: Pointer; const A64Bit: Boolean): Integer;
var
  OutBytes, Code: String;
  a: PByte;
begin
  Result := 0;
  a := AAddress;
  // skip prefix bytes
  while (a < AAddress + 16) and (a^ in [$26, $2E, $36, $3E, $40..$4F, $64..$67, $F0, $F2, $F3]) do
    inc(a);
  // check if it may be a call
  if not (a^ in [$9A, $E8, $FF]) then
    exit;

  Disassemble(AAddress, A64Bit, OutBytes, Code);
  if (Length(Code) < 5) or
     (code[1] <> 'c') or (code[2] <> 'a') or (code[3] <> 'l') or (code[4] <> 'l') or
     (code[5] <> ' ')
  then
    exit;
  Result := Length(OutBytes) div 2;
end;


initialization
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
end.
