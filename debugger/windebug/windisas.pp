{ $Id$ }
{
 ---------------------------------------------------------------------------
 windisas.pp  -  Native windows debugger - Disassembler
 ---------------------------------------------------------------------------

 This unit contains a disassembler for the Native windows debugger

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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit WinDisas;
{$mode objfpc}{$H+}
interface

uses
  SysUtils, Windows, WindExtra;

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


procedure Disassemble(const AProcess: Handle; const A64Bit: Boolean; var Address: TDbgPtr; out ACodeBytes: String; out ACode: String);
function Disassemble(const AProcess: Handle; const A64Bit: Boolean; var Address: TDbgPtr): String;

implementation

type
  TFlag = (flagRex, flagSib, flagModRM, rexB, rexX, rexR, rexW, preOpr, preAdr, preLock, preRep{N}, preRepNE);
  TFlags = set of TFlag;
  
  TOperandSize = (os8, os16, os32, os64);
  TAddressSize = (as16, as32, as64);
  TRegisterType = (reg8, reg16, reg32, reg64, regMmx, regXmm, regSegment, regControl, regDebug);

const
  OPERAND_BYTES: array[TOperandSize] of Byte = (1, 2, 4, 8);
  OPERAND_REG: array[TOperandSize] of TRegisterType = (reg8, reg16, reg32, reg64);

type
  THexValueFormatFlag = (hvfSigned, hvfPrefixPositive, hvfIncludeHexchar);
  THexValueFormatFlags = set of THexValueFormatFlag;

function HexValue(const AValue; ASize: Byte; AFlags: THexValueFormatFlags): String;
var
  i: Int64;
  p: PByte;
begin
  if ASize > 8
  then begin
    Result := 'HexValue: size to large';
    Exit;
  end;
  if ASize = 0
  then begin
    Result := '';
    Exit;
  end;

  p := @AValue;
  if p[ASize - 1] < $80
  then Exclude(AFlags, hvfSigned);

  if hvfSigned in AFlags
  then i := -1
  else i := 0;
  
  Move(AValue, i, ASize);
  if hvfSigned in AFlags
  then begin
    i := not i + 1;
    Result := '-';
  end
  else begin
    if hvfPrefixPositive in AFlags
    then Result := '+';
  end;
  if hvfIncludeHexchar in AFlags
  then Result := Result + '$';
  
  Result := Result + HexStr(i, ASize * 2);
end;

function Disassemble(const AProcess: Handle; const A64Bit: Boolean; var Address: TDbgPtr): String;
var
  S: String;
begin
  Disassemble(AProcess, A64bit, Address, S, Result);
end;

procedure Disassemble(const AProcess: Handle; const A64Bit: Boolean; var Address: TDbgPtr; out ACodeBytes: String; out ACode: String);
var
  Code: array[0..20] of Byte;
  CodeIdx: Byte;
  Operand: array[1..4] of record
    Size: Byte;
    FormatFlags: THexValueFormatFlags;
    Value: String;
    IsMemory: Boolean;
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
        if Operand[n].IsMemory then Exit;
      Result := False;
    end;
  begin
    if (preLock in Flags) and CheckMem
    then begin
      Exclude(Flags, preLock);
      Result := 'lock: ' + AOpcode;
      Exit;
    end;
    Result := AOpcode;
  end;

  function CheckRepeat(const AOpcode: String): String;
  begin
    if preRep in Flags
    then begin
      Exclude(Flags, preRep);
      Result := 'rep: ' + AOpcode;
      Exit;
    end;
    Result := AOpcode;
  end;
  
  function CheckRepeatX(const AOpcode: String): String;
  begin
    if preRep in Flags
    then begin
      Exclude(Flags, preRep);
      Result := 'repe: ' + AOpcode;
      Exit;
    end;
    if preRepNE in Flags
    then begin
      Exclude(Flags, preRepNE);
      Result := 'repe: ' + AOpcode;
      Exit;
    end;
    Result := AOpcode;
  end;
  
  //===================

  procedure AddOperand(const AValue: String; ASize: Byte = 0; AFormatFlags: THexValueFormatFlags = []; AIsMemory: Boolean = False);
  begin
    Inc(OperIdx);
    if OperIdx > High(Operand)
    then begin
      Log('AddOperand: Only %d operands supported, got %d', [High(Operand), OperIdx]);
      Exit;
    end;

    Operand[OperIdx].Size := ASize;
    Operand[OperIdx].FormatFlags := AFormatFlags;
    Operand[OperIdx].Value := AValue;
    Operand[OperIdx].IsMemory := AIsMemory;
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
  
  function GetModReg(AType: TRegisterType; AExtReg: Boolean): String;
  begin
    Include(Flags, flagModRM);
    Result := StdReg((Code[ModRMIdx] shr 3), AType, AExtReg);
  end;
  
  procedure GetModReg;
  begin
    Include(Flags, flagModRM);
    AddOperand(StdReg((Code[ModRMIdx] shr 3), OPERAND_REG[OperandSize32], rexR in Flags));
  end;

  
  procedure GetModRM(AAllowReg, AAllowMem: Boolean; ASize: TOperandSize);
  var
    Mode, Rm: Byte;
    procedure Mem16;
    const
      REGS16: array[0..7] of string = ('bx+si', 'bx+di', 'bp+si', 'bp+di', 'si', 'di', 'bp', 'bx');
    begin
      case Mode of
        0: begin
          if rm = 6 // disp16 -> exeption to the regs
          then AddOperand('%s', 2, [hvfSigned, hvfIncludeHexchar], True)
          else AddOperand(REGS16[rm], 0, [], True);
        end;
        1: AddOperand(REGS16[rm] + '%s', 1, [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar], True);
        2: AddOperand(REGS16[rm] + '%s', 2, [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar], True);
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
      if not AAllowReg
      then AddOperand('**')
      else AddOperand(StdReg(rm, OPERAND_REG[ASize], False));
      Exit;
    end;
    
    // Check if mem is allowed
    if not AAllowMem
    then begin
      AddOperand('**', 0, [], True);
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
    AddOperand(Oper.Value, Oper.Size, Oper.Flags, True);
  end;
  //===================

  procedure GetAp;
  begin
    AddOperand('Ap');
  end;
  
  procedure GetCd_q;
  begin
    GetModReg(regControl, rexR in Flags);
  end;

  procedure GetDd_q;
  begin
    GetModReg(regDebug, rexR in Flags);
  end;
  
  procedure GetEb;
  begin
    GetModRM(True, True, os8);
  end;
  
  procedure GetEd;
  begin
    GetModRM(True, True, os32);
  end;

  procedure GetEd_q;
  begin
    if flagRex in Flags
    then GetModRM(True, True, os64)
    else GetModRM(True, True, os32);
  end;
  
  procedure GetEv;
  begin
    GetModRM(True, True, OperandSize32);
  end;
  
  procedure GetEw;
  begin
    GetModRM(True, True, os16);
  end;
  
  procedure GetFv;
  begin
    case OperandSize32 of
      os64: AddOperand('rflags');
      os32: AddOperand('eflags');
    else
      AddOperand('flags');
    end;
  end;
  
  procedure GetGb;
  begin
    GetModReg(reg8, rexR in Flags);
  end;

  procedure GetGd;
  begin
    GetModReg(reg32, rexR in Flags);
  end;

  procedure GetGd_q;
  begin
    if flagRex in Flags
    then GetModReg(reg64, rexR in Flags)
    else GetModReg(reg32, rexR in Flags);
  end;

  procedure GetGv;
  begin
    GetModReg;
  end;
  
  procedure GetGw;
  begin
    GetModReg(reg16, rexR in Flags);
  end;

  procedure GetGz;
  begin
    if OperandSize32 = os16
    then GetModReg(reg16, rexR in Flags)
    else GetModReg(reg32, rexR in Flags);
  end;
  
  procedure GetIb;
  begin
    AddOperand('%s', 1, [hvfIncludeHexchar]);
  end;
  
  procedure GetIv;
  begin
    AddOperand('%s', OPERAND_BYTES[OperandSize32], [hvfIncludeHexchar]);
  end;
  
  procedure GetIw;
  begin
    AddOperand('%s', 2, [hvfIncludeHexchar]);
  end;
  
  procedure GetIz;
  var
    b: Byte;
  begin
    if OperandSize32 = os16
    then b := 2
    else b := 4;
    AddOperand('%s', b, [hvfIncludeHexchar]);
  end;
  
  procedure GetJb;
  begin
    AddOperand('%s', 1, [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar]);
  end;
  
  procedure GetJz;
  var
    b: Byte;
  begin
    if OperandSize32 = os16
    then b := 2
    else b := 4;
    AddOperand('%s', b, [hvfSigned, hvfPrefixPositive, hvfIncludeHexchar]);
  end;
  
  procedure GetM;
  begin
    GetModRM(False, True, os8 {dont care} );
  end;

  procedure GetMa;
  begin
    AddOperand('Ma');
  end;

  procedure GetMb;
  begin
    AddOperand('Ma');
  end;

  procedure GetMd_q;
  begin
    AddOperand('Md/q');
  end;

  procedure GetMdq;
  begin
    AddOperand('Mdq');
  end;

  procedure GetMp;
  begin
    AddOperand('Mp');
  end;

  procedure GetMq;
  begin
    AddOperand('Mq');
  end;

  procedure GetMs;
  begin
    AddOperand('Ms');
  end;

  procedure GetMw_Rv;
  begin
    AddOperand('Mw/Rv');
  end;

  // there is no much difference in displaying Ob or Ov, both read form mem
  procedure GetOb;
  begin
    AddOperand('%s', OPERAND_BYTES[OperandSize32], [hvfIncludeHexchar], True)
  end;

  procedure GetOv;
  begin
    AddOperand('%s', OPERAND_BYTES[OperandSize32], [hvfIncludeHexchar], True)
  end;

  procedure GetPd_q;
  begin
    AddOperand('Pd/q');
  end;

  procedure GetPq;
  begin
    AddOperand('Pq');
  end;

  procedure GetPRq;
  begin
    AddOperand('PRq');
  end;
  
  procedure GetQd;
  begin
    AddOperand('Qd');
  end;
  
  procedure GetQq;
  begin
    AddOperand('Qq');
  end;

  procedure GetRd_q;
  begin
    GetModRM(True, False, OperandSize32);
  end;
  
  procedure GetSw;
  begin
    GetModReg(regSegment, False);
  end;

  procedure GetVd_q;
  begin
    AddOperand('Vd/q');
  end;

  procedure GetVdq;
  begin
    AddOperand('Vdq');
  end;

  procedure GetVdq_sd;
  begin
    AddOperand('Vdq/sd');
  end;

  procedure GetVdq_ss;
  begin
    AddOperand('Vdq/ss');
  end;

  procedure GetVpd;
  begin
    AddOperand('Vsd');
  end;

  procedure GetVps;
  begin
    AddOperand('Vps');
  end;

  procedure GetVq;
  begin
    AddOperand('Vq');
  end;

  procedure GetVsd;
  begin
    AddOperand('Vsd');
  end;

  procedure GetVss;
  begin
    AddOperand('Vss');
  end;

  procedure GetVRdq;
  begin
    AddOperand('VRdq');
  end;

  procedure GetVRpd;
  begin
    AddOperand('VRpd');
  end;

  procedure GetVRps;
  begin
    AddOperand('VRps');
  end;

  procedure GetVRq;
  begin
    AddOperand('VRq');
  end;

  procedure GetWdq;
  begin
    AddOperand('Wdq');
  end;
  
  procedure GetWpd;
  begin
    AddOperand('Wpd');
  end;

  procedure GetWps;
  begin
    AddOperand('Wps');
  end;

  procedure GetWq;
  begin
    AddOperand('Wq');
  end;

  procedure GetWsd;
  begin
    AddOperand('Wsd');
  end;

  procedure GetWss;
  begin
    AddOperand('Wss');
  end;

  procedure GetXb;
  begin
    AddOperand('Xb');
  end;
  
  procedure GetXv;
  begin
    AddOperand('Xv');
  end;
  
  procedure GetXz;
  begin
    AddOperand('Xz');
  end;
  
  procedure GetYb;
  begin
    AddOperand('Yb');
  end;
  
  procedure GetYv;
  begin
    AddOperand('Yv');
  end;
  
  procedure GetYz;
  begin
    AddOperand('Yz');
  end;
  
  //===================
  
  procedure GetStdOperands(AIndex: Byte);
  begin
    case AIndex and $7 of
      0: begin GetEb; GetGb; end;
      1: begin GetEv; GetGv; end;
      2: begin GetGb; GetEb; end;
      3: begin GetGv; GetEv; end;
      4: begin AddOperand('al'); GetIb; end;
      5: begin AddOperand(SizeReg32('ax')); GetIz; end;
    else
      AddOperand('!!');
    end;
  end;
  
  //===================

  procedure DoX87;
  begin
    Opcode := 'x87';
  end;
  
  procedure Do3DNow;
  begin
    Opcode := 'Do3DNow';
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
        GetEv;
      end
      else Opcode := '**group1a**';
      Exit;
    end;

    // Group 1
    Opcode := OPC[Index];
    case Code[CodeIdx] of
      $80: begin GetEb; GetIb; end;
      $81: begin GetEv; GetIz; end;
      $82: begin GetEb; GetIb; Opcode := Check32(Opcode); end;
      $83: begin GetEv; GetIb; end;
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
      $C0: begin GetEb; GetIb; end;
      $C1: begin GetEv; GetIb; end;
      $D0: begin GetEb; AddOperand('1'); end;
      $D1: begin GetEv; AddOperand('1'); end;
      $D2: begin GetEb; AddOperand('cl'); end;
      $D3: begin GetEv; AddOperand('cl'); end;
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
          GetEb; GetIb;
        end
        else begin
          GetEb;
        end;
      end;
      $F7: begin
        if (Index = 0) or (Index = 1)
        then begin
          GetEv; GetIz;
        end
        else begin
          GetEv;
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
    GetEb;
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
      0: begin GetEv; Opcode := CheckLock('inc'); end;
      1: begin GetEv; Opcode := CheckLock('dec'); end;
      2: begin GetEv; Opcode := 'call';           end;
      3: begin GetMp; Opcode := 'call';           end;
      4: begin GetEv; Opcode := 'jmp';            end;
      5: begin GetMp; Opcode := 'jmp';            end;
      6: begin GetEv; Opcode := 'push';           end;
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
      0: begin GetMw_Rv; Opcode := 'sldt'; end;
      1: begin GetMw_Rv; Opcode := 'str';  end;
      2: begin GetEw;    Opcode := 'lldt'; end;
      3: begin GetEw;    Opcode := 'ltr';  end;
      4: begin GetEw;    Opcode := 'verr'; end;
      5: begin GetEw;    Opcode := 'verw'; end;
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
      0: begin GetMs; Opcode := 'sgdt'; end;
      1: begin GetMs; Opcode := 'sidt';  end;
      2: begin GetMs; Opcode := 'lgdt'; end;
      3: begin
        if Mode = 3
        then begin
          Opcode := RM3[RM];
        end
        else begin
          GetMs; Opcode := 'lidt';
        end;
      end;
      4: begin GetMw_Rv; Opcode := 'smsw'; end;
      //5 : invalid
      6: begin GetEw;    Opcode := 'lmsw'; end;
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
          GetMb; Opcode := 'invlpg';
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
    GetEv; GetIb;
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
      Opcode := 'cmpxchg8b';
      GetMq;
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
    // whole goup is invalid
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
      $C6: begin GetEb; GetIb; end;
      $C7: begin GetEv; GetIz; end;
    else
      Opcode := '!group5!';
      Exit;
    end;
    Opcode := 'mov';
  end;
  
  procedure DoGroup12;
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
  end;

  procedure DoGroup13;
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
  end;

  procedure DoGroup14;
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
  end;

  procedure DoGroup15;
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    if Code[CodeIdx] <> $AE
    then begin
      Opcode := '!group15!';
      Exit;
    end;
    Index := (Code[ModRMIdx] shr 3) and 7;
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
      then begin
        Opcode := INVALID;
        Result := -1;
      end
      else Opcode := S;
    end;
  
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
    case Code[CodeIdx] of
      $00: begin
        DoGroup6;
      end;
      $01: begin
        DoGroup7;
      end;
      $02: begin
        Opcode := 'lar';
        GetGv; GetEw;
      end;
      $03: begin
        Opcode := 'lsl';
        GetGv; GetEw;
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
          0: begin GetVps;    GetWps; end;
          1: begin GetVdq_ss; GetWss; end;
          2: begin GetVpd;    GetWpd; end;
          3: begin GetVdq_sd; GetWsd; end;
        end;
      end;
      $11: begin
        case DecodePrefix('movups', 'movss', 'movupd', 'movsd') of
          0: begin GetWps; GetVps; end;
          1: begin GetWss; GetVss; end;
          2: begin GetWpd; GetVpd; end;
          3: begin GetWsd; GetVsd; end;
        end;
      end;
      $12: begin
        case DecodePrefix('movhlps', 'movsldup', 'movlpd', 'movddup') of
          0: begin
            // Opcode differs on type found
            // it is specified as Mq or VRq
            // So when getting Wq, we get both and know the type
            GetVps; GetWq;
            if Operand[2].IsMemory
            then Opcode := 'movlps';
          end;
          1: begin GetVps; GetWps; end;
          2: begin GetVsd; GetMq;  end;
          3: begin GetVpd; GetWsd; end;
        end;
      end;
      $13: begin
        case DecodePrefix('movlps', '', 'movlpd', '') of
          0: begin GetMq; GetVps; end;
          2: begin GetMq; GetVsd; end;
        end;
      end;
      $14: begin
        case DecodePrefix('unpcklps', '', 'unpcklpd', '') of
          0: begin GetVps; GetWq; end;
          2: begin GetVpd; GetWq; end;
        end;
      end;
      $15: begin
        case DecodePrefix('unpckhps', '', 'unpckhpd', '') of
          0: begin GetVps; GetWq; end;
          2: begin GetVpd; GetWq; end;
        end;
      end;
      $16: begin
        case DecodePrefix('movlhps', 'movshdup', 'movhpd', '') of
          0: begin
            // Opcode differs on type found
            // it is specified as Mq or VRq
            // So when getting Wq, we get both and know the type
            GetVps; GetWq;
            if Operand[2].IsMemory
            then Opcode := 'movhps';
          end;
          1: begin GetVps; GetWps; end;
          2: begin GetVsd; GetMq; end;
        end;
      end;
      $17: begin
        case DecodePrefix('movhps', '', 'movhpd', '') of
          0: begin GetMq; GetVps; end;
          2: begin GetMq; GetVsd; end;
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
        GetRd_q; GetCd_q;
      end;
      $21: begin
        Opcode := 'mov';
        GetRd_q; GetDd_q;
      end;
      $22: begin
        Opcode := 'mov';
        GetCd_q; GetRd_q;
      end;
      $23: begin
        Opcode := 'mov';
        GetDd_q; GetRd_q;
      end;
      // $24..$27: invalid
      $28: begin
        case DecodePrefix('movaps', '', 'movapd', '') of
          0: begin GetVps; GetWps; end;
          2: begin GetVpd; GetWpd; end;
        end;
      end;
      $29: begin
        case DecodePrefix('movaps', '', 'movapd', '') of
          0: begin GetWps; GetVps; end;
          2: begin GetWpd; GetVpd; end;
        end;
      end;
      $2A: begin
        case DecodePrefix('cvtpi2ps', 'cvtsi2ss', 'cvtpi2pd', 'cvtsi2sd') of
          0: begin GetVps; GetQq;   end;
          1: begin GetVss; GetEd_q; end;
          2: begin GetVpd; GetQq;   end;
          3: begin GetVsd; GetEd_q; end;
        end;
      end;
      $2B: begin
        case DecodePrefix('movntps', '', 'movntpd', '') of
          0: begin GetMdq; GetVps; end;
          2: begin GetMdq; GetVpd; end;
        end;
      end;
      $2C: begin
        case DecodePrefix('cvttps2pi', 'cvttss2pi', 'cvttpd2pi', 'cvttsd2pi') of
          0: begin GetPq;   GetWps; end;
          1: begin GetGd_q; GetWss; end;
          2: begin GetPq;   GetWpd; end;
          3: begin GetGd_q; GetWsd; end;
        end;
      end;
      $2D: begin
        case DecodePrefix('cvtps2pi', 'cvtss2pi', 'cvtpd2pi', 'cvtsd2pi') of
          0: begin GetPq;   GetWps; end;
          1: begin GetGd_q; GetWss; end;
          2: begin GetPq;   GetWpd; end;
          3: begin GetGd_q; GetWsd; end;
        end;
      end;
      $2E: begin
        case DecodePrefix('ucomiss', '', 'ucomissd', '') of
          0: begin GetVss; GetWss; end;
          2: begin GetVsd; GetWsd; end;
        end;
      end;
      $2F: begin
        case DecodePrefix('comiss', '', 'comissd', '') of
          0: begin GetVss; GetWss; end;
          2: begin GetVsd; GetWsd; end;
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
        GetGv; GetEv;
      end;
      //---
      $50: begin
        case DecodePrefix('movmskps', '', 'movmskpd', '') of
          0: begin GetGd; GetVRps; end;
          2: begin GetGd; GetVRpd; end;
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
          0: begin GetVps; GetWps; end;
          1: begin GetVss; GetWss; end;
          2: begin GetVpd; GetWpd; end;
          3: begin GetVsd; GetWsd; end;
        end;
      end;
      $52: begin
        case DecodePrefix('rsqrtps', 'rsqrtss', '', '') of
          0: begin GetVps; GetWps; end;
          1: begin GetVss; GetWss; end;
        end;
      end;
      $53: begin
        case DecodePrefix('rcpps', 'rcpss', '', '') of
          0: begin GetVps; GetWps; end;
          1: begin GetVss; GetWss; end;
        end;
      end;
      $54: begin
        case DecodePrefix('andps', '', 'andpd', '') of
          0: begin GetVps; GetWps; end;
          2: begin GetVpd; GetWpd; end;
        end;
      end;
      $55: begin
        case DecodePrefix('andnps', '', 'andnpd', '') of
          0: begin GetVps; GetWps; end;
          2: begin GetVpd; GetWpd; end;
        end;
      end;
      $56: begin
        case DecodePrefix('orps', '', 'orpd', '') of
          0: begin GetVps; GetWps; end;
          2: begin GetVpd; GetWpd; end;
        end;
      end;
      $57: begin
        case DecodePrefix('xorps', '', 'xorpd', '') of
          0: begin GetVps; GetWps; end;
          2: begin GetVpd; GetWpd; end;
        end;
      end;
      // $58..$59: see $51
      $5A: begin
        case DecodePrefix('cvtps2pd', 'cvtss2sd', 'cvtpd2ps', 'cvtsd2ss') of
          0: begin GetVpd; GetWps; end;
          1: begin GetVsd; GetWss; end;
          2: begin GetVps; GetWpd; end;
          3: begin GetVss; GetWsd; end;
        end;
      end;
      $5B: begin
        case DecodePrefix('cvtdq2ps', 'cvttps2dq', 'cvtps2dq', '') of
          0: begin GetVps; GetWdq; end;
          1: begin GetVdq; GetWps; end;
          2: begin GetVdq; GetWps; end;
        end;
      end;
      // $5C..$5F: see $51
      //---
      $60..$6D: begin
        idx := Code[CodeIdx] and $F;
        idx := DecodePrefix(OPR_6x[idx], '', OPR_6x[idx], '');

        if (idx = 0) and (Code[CodeIdx] in [$6C, $6D])
        then begin
          Opcode := INVALID;
          Exit;
        end;

        case Idx of
          0: begin GetPq;  GetQd; end;
          2: begin
            GetVdq;
            if Code[CodeIdx] = $6B then GetWdq else GetWq;
          end;
        end;
      end;
      $6E: begin
        case DecodePrefix('movd', '', 'movd', '') of
          0: begin GetPq;  GetEd_q; end;
          2: begin GetVdq; GetEd_q; end;
        end;
      end;
      $6F: begin
        case DecodePrefix('movq', 'movdqu', 'movdqa', '') of
          0: begin GetPq;  GetQq;  end;
          1: begin GetVdq; GetWdq; end;
          2: begin GetVdq; GetWdq; end;
        end;
      end;
      //---
      $70: begin
        case DecodePrefix('pshufw', 'pshufhw', 'pshufd', 'pshuflw') of
          0: begin GetPq;  GetQq;  GetIb; end;
          1: begin GetVq;  GetWq;  GetIb; end;
          2: begin GetVdq; GetWdq; GetIb; end;
          3: begin GetVq;  GetWq;  GetIb; end;
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
          0: begin GetPq;  GetQq;  end;
          2: begin GetVdq; GetWdq; end;
        end;
      end;
      $75: begin
        case DecodePrefix('pcmpeqw', '', 'pcmpeqw', '') of
          0: begin GetPq;  GetQq;  end;
          2: begin GetVdq; GetWdq; end;
        end;
      end;
      $76: begin
        case DecodePrefix('pcmpeqd', '', 'pcmpeqd', '') of
          0: begin GetPq;  GetQq;  end;
          2: begin GetVdq; GetWdq; end;
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
          2: begin GetVpd; GetWpd; end;
          3: begin GetVps; GetWps; end;
        end;
      end;
      $7D: begin
        case DecodePrefix('', '', 'hsubpd', 'hsubps') of
          2: begin GetVpd; GetWpd; end;
          3: begin GetVps; GetWps; end;
        end;
      end;
      $7E: begin
        case DecodePrefix('movd', 'movq', 'movd', '') of
          0: begin GetEd_q; GetPd_q; end;
          1: begin GetVq;   GetWq;   end;
          2: begin GetEd_q; GetVd_q; end;
        end;
      end;
      $7F: begin
        case DecodePrefix('movq', 'movdqu', 'movdqa', '') of
          0: begin GetQq;  GetPq;  end;
          1: begin GetWdq; GetVdq; end;
          2: begin GetWdq; GetVdq; end;
        end;
      end;
      //---
      $80..$8F: begin
        Opcode := 'j' + StdCond(Code[CodeIdx]);
        GetJz;
      end;
      //---
      $90..$9F: begin
        Opcode := 'set' + StdCond(Code[CodeIdx]);
        GetEb;
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
        GetEv; GetGv;
      end;
      $A4: begin
        Opcode := 'shld';
        GetEv; GetGv; GetIb;
      end;
      $A5: begin
        Opcode := 'shld';
        GetEv; GetGv;
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
        GetEv; GetGv;
      end;
      $AC: begin
        Opcode := 'shrd';
        GetEv; GetGv; GetIb;
      end;
      $AD: begin
        Opcode := 'shld';
        GetEv; GetGv;
        AddOperand('cl');
      end;
      $AE: begin
        DoGroup15;
      end;
      $AF: begin
        Opcode := 'imul';
        GetGv; GetEv;
      end;
      //---
      $B0: begin
        GetEb; GetGb;
        Opcode := CheckLock('cmpxchg');
      end;
      $B1: begin
        GetEv; GetGv;
        Opcode := CheckLock('cmpxchg');
      end;
      $B2: begin
        Opcode := 'lss';
        GetGz; GetMp;
      end;
      $B3: begin
        Opcode := 'btr';
        GetEv; GetGv;
      end;
      $B4: begin
        Opcode := 'lfs';
        GetGz; GetMp;
      end;
      $B5: begin
        Opcode := 'lgs';
        GetGz; GetMp;
      end;
      $B6: begin
        Opcode := 'movzx';
        GetGv; GetEb;
      end;
      $B7: begin
        Opcode := 'movzx';
        GetGv; GetEw;
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
        GetEv; GetGv;
      end;
      $BC: begin
        Opcode := 'bsf';
        GetGv; GetEv;
      end;
      $BD: begin
        Opcode := 'bsr';
        GetGv; GetEv;
      end;
      $BE: begin
        Opcode := 'movsx';
        GetGv; GetEb;
      end;
      $BF: begin
        Opcode := 'movsx';
        GetGv; GetEw;
      end;
      //---
      $C0: begin
        GetEb; GetGb;
        Opcode := CheckLock('xadd');
      end;
      $C1: begin
        GetEv; GetGv;
        Opcode := CheckLock('xadd');
      end;
      $C2: begin
        case DecodePrefix('cmpps', 'cmpss', 'cmppd', 'cmpsd') of
          0: begin GetVps; GetWps; GetIb end;
          1: begin GetVss; GetWss; GetIb end;
          2: begin GetVpd; GetWpd; GetIb end;
          3: begin GetVsd; GetWsd; GetIb end;
        end;
      end;
      $C3: begin
        if Flags * [preRep, preRepNE, preOpr] = []
        then begin
          Opcode := 'movnti';
          GetMd_q; GetGd_q;
        end
        else Opcode := INVALID;
      end;
      $C4: begin
        case DecodePrefix('pinsrw', '', 'pinsrw', '') of
          0: begin GetPq;  GetEw; GetIb end;
          2: begin GetVdq; GetEw; GetIb end;
        end;
      end;
      $C5: begin
        case DecodePrefix('pextrw', '', 'pextrw', '') of
          0: begin GetGd; GetPRq;  GetIb end;
          2: begin GetGd; GetVRdq; GetIb end;
        end;
      end;
      $C6: begin
        case DecodePrefix('shufps', '', 'shufpd', '') of
          0: begin GetVps; GetWps; GetIb end;
          2: begin GetVpd; GetWpd; GetIb end;
        end;
      end;
      $C7: begin
        DoGroup9;
      end;
      $C8..$CF: begin
        Opcode := 'bswp';
        AddOperand(StdReg(Code[CodeIdx]));
      end;
      //---
      $D0: begin
        case DecodePrefix('', '', 'addsubpd', 'addsubps') of
          2: begin GetVpd; GetWpd; end;
          3: begin GetVps; GetWps; end;
        end;
      end;
      $D1..$D5, $D8..$DF: begin
        idx := Code[CodeIdx] and $F;
        idx := DecodePrefix(OPR_Dx[idx], '', OPR_Dx[idx], '');

        case Idx of
          0: begin GetPq;  GetQq;  end;
          2: begin GetVdq; GetWdq; end;
        end;
      end;
      $D6: begin
        case DecodePrefix('', 'movq2dq', 'movq', 'movdq2q') of
          1: begin GetVdq; GetPRq; end;
          2: begin GetWq;  GetVq;  end;
          3: begin GetPq;  GetVRq; end;
        end;
      end;
      $D7: begin
        case DecodePrefix('pmovmskb', '', 'pmovmskb', '') of
          0: begin GetGd; GetPRq;  end;
          2: begin GetGd; GetVRdq; end;
        end;
      end;
      // $D8..$DF: see $D1
      //---
      $E0..$E5, $E8..$EF: begin
        idx := Code[CodeIdx] and $F;
        idx := DecodePrefix(OPR_Ex[idx], '', OPR_Ex[idx], '');

        case Idx of
          0: begin GetPq;  GetQq;  end;
          2: begin GetVdq; GetWdq; end;
        end;
      end;
      $E6: begin
        case DecodePrefix('', 'cvtdq2pd', 'cvttpd2dq', 'cvtpd2dq') of
          1: begin GetVpd; GetWq;  end;
          2: begin GetVq;  GetWpd; end;
          3: begin GetVq;  GetWpd; end;
        end;
      end;
      $E7: begin
        case DecodePrefix('movntq', '', 'movntdqu', '') of
          0: begin GetMq;  GetPq;  end;
          2: begin GetMdq; GetVdq; end;
        end;
      end;
      // $E8..$EF: see $E0
      $F0: begin
        if preRepNE in Flags
        then begin
          Opcode := 'lddqu';
          GetVpd; GetMdq;
        end
        else Opcode := INVALID;
      end;
      $F1..$F6, $F8..$FE: begin
        idx := Code[CodeIdx] and $F;
        idx := DecodePrefix(OPR_Fx[idx], '', OPR_Fx[idx], '');

        case Idx of
          0: begin GetPq;  GetQq;  end;
          2: begin GetVdq; GetWdq; end;
        end;
      end;
      $F7: begin
        case DecodePrefix('maskmovq', '', 'maskmovdqu', '') of
          0: begin GetPq;  GetPRq;  end;
          2: begin GetVdq; GetVRdq; end;
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
          GetStdOperands(Code[CodeIdx]);
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
          GetStdOperands(Code[CodeIdx]);
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
          GetStdOperands(Code[CodeIdx]);
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
          GetStdOperands(Code[CodeIdx]);
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
          GetStdOperands(Code[CodeIdx]);
          Opcode := CheckLock('and');
        end;
        $26: begin
          Segment := Segment + Ignore64('es:');
        end;
        $27: begin
          Opcode := Check32('daa');
        end;
        $28..$2D: begin
          GetStdOperands(Code[CodeIdx]);
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
          GetStdOperands(Code[CodeIdx]);
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
          GetStdOperands(Code[CodeIdx]);
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
            AddOperand(StdReg(Code[CodeIdx]));
            if Code[CodeIdx] <= $47
            then Opcode := CheckLock('inc')
            else Opcode := CheckLock('dec');
          end;
        end;
        //---
        $50..$57: begin
          Opcode := 'push';
          AddOperand(StdReg(Code[CodeIdx]));
        end;
        $58..$5F: begin
          Opcode := 'pop';
          AddOperand(StdReg(Code[CodeIdx]));
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
          GetGv; GetMa;
        end;
        $63: begin
          if A64Bit
          then begin
            Opcode := ('movsxd');
            GetGv; GetEd;
          end
          else begin
            Opcode := Check32('arpl');
            GetEw; GetGw;
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
          GetIz;
        end;
        $69: begin
          Opcode := 'imul';
          GetGv; GetEv; GetIz;
        end;
        $6A: begin
          Opcode := 'push';
          GetIb;
        end;
        $6B: begin
          Opcode := 'imul';
          GetGv; GetEv; GetIb;
        end;
        $6C: begin
          Opcode := CheckRepeat('insb');
          GetYb;
          AddOperand('dx');
        end;
        $6D: begin
          if OperandSize32 = os16
          then Opcode := CheckRepeat('insw')
          else Opcode := CheckRepeat('insd');
          GetYz;
          AddOperand('dx');
        end;
        $6E: begin
          Opcode := CheckRepeat('outsb');
          AddOperand('dx');
          GetXb;
        end;
        $6F: begin
          if OperandSize32 = os16
          then Opcode := CheckRepeat('outsw')
          else Opcode := CheckRepeat('outsd');
          AddOperand('dx');
          GetXz;
        end;
        $70..$7F: begin
          Opcode := 'j' + StdCond(Code[CodeIdx]);
          GetJb;
        end;
        //---
        $80..$83: begin
          DoGroup1;
        end;
        $84: begin
          Opcode := 'test';
          GetEb; GetGb;
        end;
        $85: begin
          Opcode := 'test';
          GetEv; GetGv;
        end;
        $86: begin
          GetEb; GetGb;
          Opcode := CheckLock('xchg');
        end;
        $87: begin
          GetEv; GetGv;
          Opcode := CheckLock('xchg');
        end;
        $88..$8B: begin
          Opcode := 'mov';
          GetStdOperands(Code[CodeIdx]);
        end;
        $8C: begin
          Opcode := 'mov';
          GetMw_Rv; GetSw;
        end;
        $8D: begin
          Opcode := 'lea';
          GetGv; GetM;
        end;
        $8E: begin
          Opcode := 'mov';
          GetSw; GetEw;
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
            AddOperand(StdReg(Code[CodeIdx]));
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
          GetAp;
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
          GetFv;
        end;
        $9D: begin
          case OperandSize32 of
            os64: Opcode := 'popfq';
            os32: Opcode := 'popfd';
          else
            Opcode := 'popf';
          end;
          GetFv;
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
          AddOperand('al');
          GetOb;
        end;
        $A1: begin
          Opcode := 'mov';
          AddOperand(SizeReg32('ax'));
          GetOv;
        end;
        $A2: begin
          Opcode := 'mov';
          GetOb;
          AddOperand('al');
        end;
        $A3: begin
          Opcode := 'mov';
          GetOb;
          AddOperand(SizeReg32('ax'));
        end;
        $A4: begin
          Opcode := CheckRepeat('movsb');
          GetYb; GetXb;
        end;
        $A5: begin
          case OperandSize32 of
            os64: Opcode := CheckRepeat('movsq');
            os32: Opcode := CheckRepeat('movsd');
          else
            Opcode := CheckRepeat('movsw');
          end;
          GetYv;
          GetXv;
        end;
        $A6: begin
          Opcode := CheckRepeatX('cmpsb');
          GetXb; GetYb;
        end;
        $A7: begin
          case OperandSize32 of
            os64: Opcode := CheckRepeatX('cmpsq');
            os32: Opcode := CheckRepeatX('cmpsd');
          else
            Opcode := CheckRepeatX('cmpsw');
          end;
          GetYv; GetXv;
        end;
        $A8: begin
          Opcode := 'test';
          AddOperand('al');
          GetIb;
        end;
        $A9: begin
          Opcode := 'test';
          AddOperand(SizeReg32('ax'));
          GetIv;
        end;
        $AA: begin
          Opcode := CheckRepeat('stosb');
          GetYb;
          AddOperand('al');
        end;
        $AB: begin
          case OperandSize32 of
            os64: Opcode := CheckRepeat('stosq');
            os32: Opcode := CheckRepeat('stosd');
          else
            Opcode := CheckRepeat('stosw');
          end;
          GetYv;
          AddOperand(SizeReg32('ax'));
        end;
        $AC: begin
          Opcode := CheckRepeat('lodsb');
          AddOperand('al');
          GetXb;
        end;
        $AD: begin
          case OperandSize32 of
            os64: Opcode := CheckRepeat('lodsq');
            os32: Opcode := CheckRepeat('lodsd');
          else
            Opcode := CheckRepeat('lodsw');
          end;
          AddOperand(SizeReg32('ax'));
          GetXv;
        end;
        $AE: begin
          Opcode := CheckRepeatX('scasb');
          AddOperand('al');
          GetYb;
        end;
        $AF: begin
          case OperandSize32 of
            os64: Opcode := CheckRepeatX('scasq');
            os32: Opcode := CheckRepeatX('scasd');
          else
            Opcode := CheckRepeatX('scasw');
          end;
          AddOperand(SizeReg32('ax'));
          GetYv;
        end;
        //---
        $B0..$B7: begin
          Opcode := 'mov';
          AddOperand(StdReg(Code[CodeIdx], reg8, rexR in Flags));
          GetIb;
        end;
        $B8..$BF: begin
          Opcode := 'mov';
          AddOperand(StdReg(Code[CodeIdx]));
          GetIv;
        end;
        //---
        $C0..$C1: begin
          DoGroup2;
        end;
        $C2: begin
          Opcode := 'ret';
          GetIw;
        end;
        $C3: begin
          Opcode := 'ret';
        end;
        $C4: begin
          Opcode := 'les';
          GetGz; GetMp;
        end;
        $C5: begin
          Opcode := 'lds';
          GetGz; GetMp;
        end;
        $C6..$C7: begin
          DoGroup11;
        end;
        $C8: begin
          Opcode := 'enter';
          GetIw; GetIb;
        end;
        $C9: begin
          Opcode := 'leave';
        end;
        $CA: begin
          Opcode := 'retf';
          GetIw;
        end;
        $CB: begin
          Opcode := 'retf';
        end;
        $CC: begin
          Opcode := 'int3';
        end;
        $CD: begin
          Opcode := 'int';
          GetIb;
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
          GetJb;
        end;
        $E1: begin
          Opcode := 'loope';
          GetJb;
        end;
        $E2: begin
          Opcode := 'loop';
          GetJb;
        end;
        $E3: begin
          Opcode := 'jrcxz';
          GetJb;
        end;
        $E4: begin
          Opcode := 'in';
          AddOperand('al');
          GetIb;
        end;
        $E5: begin
          Opcode := 'in';
          AddOperand(SizeReg32('ax'));
          GetIb;
        end;
        $E6: begin
          Opcode := 'out';
          GetIb;
          AddOperand('al');
        end;
        $E7: begin
          Opcode := 'out';
          GetIb;
          AddOperand(SizeReg32('ax'));
        end;
        $E8: begin
          Opcode := 'call';
          GetJz;
        end;
        $E9: begin
          Opcode := 'jmp';
          GetJz;
        end;
        $EA: begin
          Opcode := Check32('jmp');
          GetAp;
        end;
        $EB: begin
          Opcode := 'jmp';
          GetJb;
        end;
        $EC: begin
          Opcode := 'in';
          AddOperand('al');
          AddOperand('dx');
        end;
        $ED: begin
          Opcode := 'in';
          AddOperand(SizeReg32('ax'));
          AddOperand('dx');
        end;
        $EE: begin
          Opcode := 'out';
          AddOperand('dx');
          AddOperand('al');
        end;
        $EF: begin
          Opcode := 'out';
          AddOperand('dx');
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
      if CodeIdx > High(Code)
      then begin
        Log('Disassemble: instruction longer than %d bytes', [SizeOf(Code)]);
        Exit;
      end;
    until Opcode <> '';
  end;
  
var
  BytesRead: Cardinal;
  S, Soper: String;
  n: Integer;
  HasMem: Boolean;
begin
  if not ReadProcessMemory(AProcess, Pointer(Address), @Code, SizeOf(Code), BytesRead) and (BytesRead = SizeOf(Code))
  then begin
    Log('Disassemble: Failed to read memory at %s, got %u bytes', [FormatAddress(Address), BytesRead]);
    ACode := '??';
    ACodeBytes := '??';
    Inc(Address);
    Exit;
  end;

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
    if Operand[n].Size = 0
    then S := Operand[n].Value
    else S := Format(Operand[n].Value, [HexValue(Code[CodeIdx], Operand[n].Size, Operand[n].FormatFlags)]);

    if Soper <> '' then Soper := Soper + ',';
    if Operand[n].IsMemory
    then begin
      Soper := Soper + Segment + '[' + S + ']';
      HasMem := True;
    end
    else Soper := Soper + S;
    Inc(CodeIdx, Operand[n].Size);
  end;
  S := '';
  if preLock in Flags then S := S + '**lock:**';
  if preRep in Flags then S := S + '?rep:?';
  if preRepNE in Flags then S := S + '?repne:?';
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
  Inc(Address, CodeIdx);
end;


end.
