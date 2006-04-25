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
}  


procedure Disassemble(const AProcess: Handle; const A64Bit: Boolean; var Address: TDbgPtr; out ACodeBytes: String; out ACode: String);
function Disassemble(const AProcess: Handle; const A64Bit: Boolean; var Address: TDbgPtr): String;

implementation

type
  TFlag = (flagRex, flagSib, flagModRM, rexB, rexX, rexR, rexW, preOpr, preAdr);
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
  Segment: String;
  Prefix: String;
  Flags: TFlags;

  function Check32(const Opcode: String): String;
  begin
    // only valid in 32-bit mode
    if A64Bit
    then Result := '*' + Opcode + '*'
    else Result := Opcode;
  end;

  function Check64(const Opcode: String): String;
  begin
    // only valid in 64-bit mode
    if A64Bit
    then Result := Opcode
    else Result := '*' + Opcode + '*';
  end;

  function Ignore64(const Opcode: String): String;
  begin
    // ignored in 64-bit mode
    if A64Bit
    then Result := '(' + Opcode + ')'
    else Result := Opcode;
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
  
  procedure GetEb;
  begin
    GetModRM(True, True, os8);
  end;
  
  procedure GetEd;
  begin
    GetModRM(True, True, os32);
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
  
  procedure GetM;
  begin
    GetModRM(False, True, os8 {dont care} );
  end;
  
  procedure GetMa;
  begin
    AddOperand('Ma');
  end;

  procedure GetMp;
  begin
    AddOperand('Mp');
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
  
  // there is no much difference in displaying Ob or Ov, both read form mem
  procedure GetOb;
  begin
    AddOperand('%s', OPERAND_BYTES[OperandSize32], [hvfIncludeHexchar], True)
  end;

  procedure GetOv;
  begin
    AddOperand('%s', OPERAND_BYTES[OperandSize32], [hvfIncludeHexchar], True)
  end;

  procedure GetSw;
  begin
    GetModReg(regSegment, False);
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

  function DoCondJump: String;
  const
    JUMP: array[0..$F] of String = ('jo', 'jno', 'jb', 'jnb', 'jz', 'jnz', 'jbe', 'jnbe', 'js', 'jns', 'jp', 'jnp', 'jl', 'jnl', 'jle', 'jnle');
  begin
    Result := JUMP[Code[CodeIdx] and $F];
    GetJb;
  end;
  
  function DoX87: String;
  begin
    Result := 'x87';
  end;
  
  // Group
  
  function DoGroup1: String;
  const
    OPCODE: array[0..7] of String = ('add', 'or', 'adc', 'sbb', 'and', 'sub', 'xor', 'cmp');
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    Index := (Code[ModRMIdx] shr 3) and 7;
    Result := OPCODE[Index];
    case Code[CodeIdx] of
      $80: begin GetEb; GetIb; end;
      $81: begin GetEv; GetIz; end;
      $82: begin GetEb; GetIb; Result := Check32(Result); end;
      $83: begin GetEv; GetIb; end;
      $8F: begin
        if Index = 0
        then begin
          Result := 'pop';
          GetEv
        end
        else Result := '*group1*';
      end;
    else
      Result := '!group1!';
    end;
  end;
  
  function DoGroup2: String;
  const
    OPCODE: array[0..7] of String = ('rol', 'ror', 'rcl', 'rcr', 'shl', 'shr', 'sal', 'sar');
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    Index := (Code[ModRMIdx] shr 3) and 7;
    Result := OPCODE[Index];
    case Code[CodeIdx] of
      $C0: begin GetEb; GetIb; end;
      $C1: begin GetEv; GetIb; end;
      $D0: begin GetEb; AddOperand('1'); end;
      $D1: begin GetEv; AddOperand('1'); end;
      $D2: begin GetEb; AddOperand('cl'); end;
      $D3: begin GetEv; AddOperand('cl'); end;
    else
      Result := '!group2!';
    end;
  end;
  
  function DoGroup3: String;
  const
    OPCODE: array[0..7] of String = ('test', 'test', 'not', 'neg', 'mul', 'imul', 'div', 'idiv');
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    Index := (Code[ModRMIdx] shr 3) and 7;
    Result := OPCODE[Index];
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
      Result := '!group3!';
    end;
  end;
  
  function DoGroup4: String;
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    if Code[CodeIdx] <> $FE
    then begin
      Result := '!group4!';
      Exit;
    end;
    
    Index := (Code[ModRMIdx] shr 3) and 7;
    case Index of
      0: Result := 'inc';
      1: Result := 'dec';
    else
      Result := '*group4*';
      Exit;
    end;
    GetEb;
  end;
  
  function DoGroup5: String;
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    if Code[CodeIdx] <> $FF
    then begin
      Result := '!group5!';
      Exit;
    end;

    Index := (Code[ModRMIdx] shr 3) and 7;
    case Index of
      0: begin
        Result := 'inc';
        GetEv;
      end;
      1: begin
        Result := 'dec';
        GetEv;
      end;
      2: begin
        Result := 'call';
        GetEv;
      end;
      3: begin
        Result := 'call';
        GetMp;
      end;
      4: begin
        Result := 'jmp';
        GetEv;
      end;
      5: begin
        Result := 'jmp';
        GetMp;
      end;
      6: begin
        Result := 'push';
        GetEv;
      end;
    else
      Result := '*group5*';
      Exit;
    end;
  end;

  function DoGroup11: String;
  var
    Index: Byte;
  begin
    Include(Flags, flagModRM);
    Index := (Code[ModRMIdx] shr 3) and 7;
    if Index <> 0
    then begin
      Result := '*group5*';
      Exit;
    end;
    
    case Code[CodeIdx] of
      $C6: begin GetEb; GetIb; end;
      $C7: begin GetEv; GetIz; end;
    else
      Result := '!group5!';
      Exit;
    end;
    Result := 'mov';
  end;
  
  //---
  
  function Do2ByteOpcode: String;
  begin
    Result := '2byte';
  end;


var
  BytesRead: Cardinal;
  Opcode, S, Soper: String;
  n: Integer;
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
  Prefix := '';
  Opcode := '';
  Flags := [];
  CodeIdx := 0;
  OperIdx := 0;

  repeat
    ModRMIdx := CodeIdx + 1;
    case Code[CodeIdx] of
      $00..$05: begin
        Opcode := 'add';
        GetStdOperands(Code[CodeIdx]);
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
        Opcode := 'or';
        GetStdOperands(Code[CodeIdx]);
      end;
      $0E: begin
        Opcode := Check32('push');
        AddOperand('cs');
      end;
      $0F: begin
        Opcode := Do2ByteOpcode;
      end;
      //---
      $10..$15: begin
        Opcode := 'adc';
        GetStdOperands(Code[CodeIdx]);
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
        Opcode := 'sbb';
        GetStdOperands(Code[CodeIdx]);
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
        Opcode := 'and';
        GetStdOperands(Code[CodeIdx]);
      end;
      $26: begin            
        Segment := Segment + Ignore64('es:');
      end;
      $27: begin            
        Opcode := Check32('daa');
      end;
      $28..$2D: begin
        Opcode := 'sub';
        GetStdOperands(Code[CodeIdx]);
      end;
      $2E: begin
        Segment := Segment + Ignore64('cs:');
      end;
      $2F: begin            
        Opcode := Check32('das');
      end;
      //---
      $30..$35: begin
        Opcode := 'xor';
        GetStdOperands(Code[CodeIdx]);
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
          if Code[CodeIdx] <= $47
          then Opcode := 'inc'
          else Opcode := 'dec';
          AddOperand(StdReg(Code[CodeIdx]));
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
        Opcode := 'insb';
        GetYb;
        AddOperand('dx');
      end;
      $6D: begin       
        if OperandSize32 = os16
        then Opcode := 'insw'
        else Opcode := 'insd';
        GetYz;
        AddOperand('dx');
      end;
      $6E: begin       
        Opcode := 'outsb';
        AddOperand('dx');
        GetXb;
      end;
      $6F: begin       
        if OperandSize32 = os16
        then Opcode := 'outsw'
        else Opcode := 'outsd';
        AddOperand('dx');
        GetXz;
      end;
      $70..$7F: begin
        Opcode := DoCondJump;
      end;
      //---
      $80..$83: begin
        Opcode := DoGroup1;
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
        Opcode := 'xchg';
        GetEb; GetGb;
      end;
      $87: begin
        Opcode := 'xchg';
        GetEv; GetGv;
      end;
      $88..$8B: begin
        Opcode := 'mov';
        GetStdOperands(Code[CodeIdx]);
      end;
      $8C: begin
        // Intel specifies Ew
        // AMD specifies Mw/Rv (which is IMO the same as Ew)
        Opcode := 'mov';
        GetEw; GetSw;
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
        Opcode := DoGroup1;
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
        Opcode := 'movsb';
        GetYb; GetXb;
      end;
      $A5: begin
        case OperandSize32 of
          os64: Opcode := 'movsq';
          os32: Opcode := 'movsd';
        else
          Opcode := 'movsw';
        end;
        GetYv;
        GetXv;
      end;
      $A6: begin
        Opcode := 'cmpsb';
        GetXb; GetYb;
      end;
      $A7: begin
        case OperandSize32 of
          os64: Opcode := 'cmpsq';
          os32: Opcode := 'cmpsd';
        else
          Opcode := 'cmpsw';
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
        Opcode := 'stosb';
        GetYb;
        AddOperand('al');
      end;
      $AB: begin
        case OperandSize32 of
          os64: Opcode := 'stosq';
          os32: Opcode := 'stosd';
        else
          Opcode := 'stosw';
        end;
        GetYv;
        AddOperand(SizeReg32('ax'));
      end;
      $AC: begin
        Opcode := 'lodsb';
        AddOperand('al');
        GetXb;
      end;
      $AD: begin
        case OperandSize32 of
          os64: Opcode := 'lodsq';
          os32: Opcode := 'lodsd';
        else
          Opcode := 'lodsw';
        end;
        AddOperand(SizeReg32('ax'));
        GetXv;
      end;
      $AE: begin
        Opcode := 'scasb';
        AddOperand('al');
        GetYb;
      end;
      $AF: begin
        case OperandSize32 of
          os64: Opcode := 'scasq';
          os32: Opcode := 'scasd';
        else
          Opcode := 'scasw';
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
        Opcode := DoGroup2;
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
        Opcode := DoGroup11;
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
        Opcode := DoGroup2;
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
        Opcode := DoX87;
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
        Prefix := Prefix + 'lock:'
      end;
      $F1: begin
        Opcode := 'int1';
      end;
      $F2: begin
        Prefix := Prefix + 'repne:'
      end;
      $F3: begin
        Prefix := Prefix + 'rep:'
      end;
      $F4: begin
        Opcode := 'hlt';
      end;
      $F5: begin
        Opcode := 'cmc';
      end;
      $F6..$F7: begin
        Opcode := DoGroup3;
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
        Opcode := DoGroup4;
      end;
      $FF: begin
        Opcode := DoGroup5;
      end;
    else
      Opcode := HexValue(Code[0], 1, []);
      Inc(Address);
      Exit;
    end;   

    Inc(CodeIdx);
    if CodeIdx > High(Code)
    then begin
      Log('Disassemble: instruction longer than %d bytes', [SizeOf(Code)]);
      Exit;
    end;
  until Opcode <> '';
  
  if flagModRM in Flags then Inc(CodeIdx);
  if flagSib in Flags then Inc(CodeIdx);

  Soper := '';
  for n := 1 to OperIdx do
  begin
    if Operand[n].Size = 0
    then S := Operand[n].Value
    else S := Format(Operand[n].Value, [HexValue(Code[CodeIdx], Operand[n].Size, Operand[n].FormatFlags)]);

    if Soper <> '' then Soper := Soper + ',';
    if Operand[n].IsMemory
    then Soper := Soper + Segment + '[' + S + ']'
    else Soper := Soper + S;
    Inc(CodeIdx, Operand[n].Size);
  end;
  ACode := Prefix + Opcode + ' ' + Soper;

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
