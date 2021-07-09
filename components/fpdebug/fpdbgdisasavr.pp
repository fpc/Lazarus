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
interface

uses
  SysUtils,
  FpDbgUtil, FpDbgInfo, DbgIntfBaseTypes, FpdMemoryTools, {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif},
  FpDbgClasses;

type
  //The function Disassemble decodes the instruction at the given address.
  //Unrecognized instructions are assumed to be data statements [dw XXXX]

  TAvrAsmDecoder = class;

  { TX86DisassemblerInstruction }

  { TAvrAsmInstruction }

  TAvrAsmInstruction = class(TDbgAsmInstruction)
  private const
    INSTR_CODEBIN_LEN = 4;
  private
    FAsmDecoder: TAvrAsmDecoder;
    FAddress: TDBGPtr;
    FCodeBin: array[0..INSTR_CODEBIN_LEN-1] of byte;
    FFlags: set of (diCodeRead, diCodeReadError);
  protected
    procedure ReadCode; inline;
  public
    constructor Create(AAsmDecoder: TAvrAsmDecoder);
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
  private
    FProcess: TDbgProcess;
    FLastErrWasMem: Boolean;
    FCodeBin: array[0..MAX_CODEBIN_LEN-1] of byte;
    FLastInstr: TAvrAsmInstruction;
    function FParsePrologue(AnAddress, AStartPC, AEndPC: TDBGPtr; out
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

    // Don't use, ot really suited to AVR ABI
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


implementation

uses
  StrUtils, LazClasses, Math;

var
  DBG_WARNINGS: PLazLoggerLogGroup;

const
  opRegReg = '%s r%d, r%d';
  opRegConst = '%s r%d, %d';
  opRegConstHex8 = '%s r%d, $%.2X';
  opRegConstHex16 = '%s r%d, $%.4X';
  opRegConstHex32 = '%s r%d, $%.8X';
  opConstReg = '%s %d, r%d';
  opConstHex8Reg = '%s $%.2X, r%d';
  opConstHex16Reg = '%s $%.4X, r%d';
  opConstHex32Reg = '%s $%.4X, r%d';
  opRegStr = '%s r%d, %s';
  opStrReg = '%s %s, r%d';
  opOnly = '%s';
  opStr = '%s %s';
  opConst = '%s %d';
  opConstHex8 = '%s $%.2X';
  opConstHex16 = '%s $%.4X';
  opConstHex8Const = '%s $%.2X, %d';
  opReg = '%s r%d';

  statusFlagNames: array[0..7] of char = ('c', 'z', 'n', 'v', 's', 'h', 't', 'i');
  branchIfSetNames: array[0..7] of string = ('brcs', 'breq', 'brmi', 'brvs', 'brlt', 'brhs', 'brts', 'brie');
  branchIfClrNames: array[0..7] of string = ('brcc', 'brne', 'brpl', 'brvc', 'brge', 'brhc', 'brtc', 'brid');

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

// If non-valid opcode, assume it is a data statement
function InvalidOpCode(instr: word): string;
begin
  result := format(opConstHex16, ['dw', instr]);
end;

{ TAvrAsmInstruction }

procedure TAvrAsmInstruction.ReadCode;
begin
  if not (diCodeRead in FFlags) then begin
    if not FAsmDecoder.FProcess.ReadData(FAddress, INSTR_CODEBIN_LEN, FCodeBin) then
      Include(FFlags, diCodeReadError);
    Include(FFlags, diCodeRead);
  end;
end;

constructor TAvrAsmInstruction.Create(AAsmDecoder: TAvrAsmDecoder);
begin
  FAsmDecoder := AAsmDecoder;
  inherited Create;
  AddReference;
end;

procedure TAvrAsmInstruction.SetAddress(AnAddress: TDBGPtr);
begin
  FAddress := AnAddress;
  FFlags := [];
end;

function TAvrAsmInstruction.IsCallInstruction: boolean;
var
  LoByte, HiByte: byte;
begin
  Result := False;
  ReadCode;
  LoByte := FCodeBin[0];
  HiByte := FCodeBin[1];
  if ((HiByte and $FE) = $94) and ((LoByte and $0E) = $0E) or // call
     ((HiByte = $95) and (LoByte in [$09, $19])) or           // icall / eicall
     ((HiByte and $D0) = $D0) then                            // rcall
    Result := true;
end;

function TAvrAsmInstruction.IsReturnInstruction: boolean;
var
  LoByte, HiByte: byte;
begin
  Result := False;
  ReadCode;
  LoByte := FCodeBin[0];
  HiByte := FCodeBin[1];
  if ((HiByte = $95) and (LoByte in [$08, $18])) then  // ret / reti
    Result := true;
end;

function TAvrAsmInstruction.IsLeaveStackFrame: boolean;
begin
  Result := false;
end;

function TAvrAsmInstruction.InstructionLength: Integer;
var
  LoByte, HiByte: byte;
begin
  Result := 2;
  ReadCode;
  LoByte := FCodeBin[0];
  HiByte := FCodeBin[1];
  if ((HiByte and $FE) = $94) and ((LoByte and $0E) in [$0C, $0E]) or   // jmp / call
     ((HiByte and $FE) in [$90, $92]) and ((LoByte and $0F) = $0) then  // lds / sts
    Result := 4;
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
      // Push a register onto stack
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

  // Also read previous instruction, for simple procs/interrupts it may not be easy to spot the start of epilogue
  // so the previous instruction could tie break
  ADataLen := Min(MaxEpilogueSize, AEndPC - AnAddress + 2);
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
      // Push a register onto stack
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
  ACodeBytes: String; out ACode: String);
var
  CodeIdx, r, d, k, q: byte;
  a: SmallInt;
  pcode: PByte;
  code, addr16: word;
  s1: string;
  _set: boolean;
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
        $0000: ACode := 'nop';
        else
        begin
          case (code and $fc00) of
            $0400:
            begin // CPC compare with carry 0000 01rd dddd rrrr
              get_r_d_10(code, r, d);
              ACode := format(opRegReg, ['cpc', d, r]);
            end;
            $0c00:
            begin // ADD without carry 0000 11 rd dddd rrrr
              get_r_d_10(code, r, d);
              ACode := format(opRegReg, ['add', d, r]);
            end;
            $0800:
            begin // SBC subtract with carry 0000 10rd dddd rrrr
              get_r_d_10(code, r, d);
              ACode := format(opRegReg, ['sbc', d, r]);
            end;
            else
              case (code and $ff00) of
                $0100:
                begin // MOVW – Copy Register Word 0000 0001 dddd rrrr
                  d := ((code shr 4) and $f) shl 1;
                  r := ((code) and $f) shl 1;
                  ACode := format(opRegReg, ['movw', d, r]);
                end;
                $0200:
                begin // MULS – Multiply Signed 0000 0010 dddd rrrr
                  r := 16 + (code and $f);
                  d := 16 + ((code shr 4) and $f);
                  ACode := format(opRegReg, ['muls', d, r]);
                end;
                $0300:
                begin // MUL Multiply 0000 0011 fddd frrr
                  r := 16 + (code and $7);
                  d := 16 + ((code shr 4) and $7);
                  case (code and $88) of
                    $00: ACode := format(opRegReg, ['mulsu', d, r]);
                    $08: ACode := format(opRegReg, ['fmul', d, r]);
                    $80: ACode := format(opRegReg, ['fmuls', d, r]);
                    $88: ACode := format(opRegReg, ['fmulsu', d, r]);
                  end;
                end;
                else
                  ACode := InvalidOpCode(code);
              end;
          end;
        end;
      end;
    end;
    $1000:
    begin
      case (code and $fc00) of
        $1800:
        begin // SUB without carry 0000 10 rd dddd rrrr
          get_r_d_10(code, r, d);
          ACode := format(opRegReg, ['sub', d, r]);
        end;
        $1000:
        begin // CPSE Compare, skip if equal 0000 00 rd dddd rrrr
          get_r_d_10(code, r, d);
          ACode := format(opRegReg, ['cpse', d, r]);
        end;
        $1400:
        begin // CP Compare 0000 01 rd dddd rrrr
          get_r_d_10(code, r, d);
          ACode := format(opRegReg, ['cp', d, r]);
        end;
        $1c00:
        begin // ADD with carry 0001 11 rd dddd rrrr
          get_r_d_10(code, r, d);
          ACode := format(opRegReg, ['adc', d, r]);
        end;
        else
          ACode := InvalidOpCode(code);
      end;
    end;
    $2000:
    begin
      case (code and $fc00) of
        $2000:
        begin // AND 0010 00rd dddd rrrr
          get_r_d_10(code, r, d);
          ACode := format(opRegReg, ['and', d, r]);
        end;
        $2400:
        begin // EOR 0010 01rd dddd rrrr
          get_r_d_10(code, r, d);
          ACode := format(opRegReg, ['eor', d, r]);
        end;
        $2800:
        begin // OR Logical OR 0010 10rd dddd rrrr
          get_r_d_10(code, r, d);
          ACode := format(opRegReg, ['or', d, r]);
        end;
        $2c00:
        begin // MOV 0010 11rd dddd rrrr
          get_r_d_10(code, r, d);
          ACode := format(opRegReg, ['mov', d, r]);
        end;
        else
          ACode := InvalidOpCode(code);
      end;
    end;
    $3000:
    begin // CPI 0011 KKKK rrrr KKKK
      get_k_r16(code, d, k);
      ACode := format(opRegConstHex8, ['cpi', d, k]);
    end;
    $4000:
    begin // SBCI Subtract Immediate With Carry 0100 kkkk dddd kkkk
      get_k_r16(code, d, k);
      ACode := format(opRegConstHex8, ['sbci', d, k]);
    end;
    $5000:
    begin // SUB Subtract Immediate 0101 kkkk dddd kkkk
      get_k_r16(code, d, k);
      ACode := format(opRegConstHex8, ['subi', d, k]);
    end;
    $6000:
    begin // ORI aka SBR Logical AND with Immediate 0110 kkkk dddd kkkk
      get_k_r16(code, d, k);
      ACode := format(opRegConstHex8, ['ori', d, k]);
    end;
    $7000:
    begin // ANDI Logical AND with Immediate 0111 kkkk dddd kkkk
      get_k_r16(code, d, k);
      ACode := format(opRegConstHex8, ['andi', d, k]);
    end;
    $a000,
    $8000:
    begin
      case (code and $d008) of
        $a000,
        $8000:
        begin // LD (LDD) – Load Indirect using Z 10q0 qq0r rrrr 0qqq
          d := (code shr 4) and $1f;
          q := ((code and $2000) shr 8) or ((code and $0c00) shr 7) or (code and $7);
          if (code and $0200) <> 0 then // store
          begin
            if q > 0 then
              ACode := format(opStrReg, ['std', 'Z+'+IntToStr(q), d])
            else
            begin
              case (code and 3) of
                0: s1 := 'Z';
                1: s1 := 'Z+';
                3: s1 := '-Z';
              end;
              ACode := format(opStrReg, ['st', s1, d]);
            end;
          end
          else  // load
          begin
            if q > 0 then
              ACode := format(opRegStr, ['ldd', d, 'Z+'+IntToStr(q)])
            else
            begin
              case (code and 3) of
                0: s1 := 'Z';
                1: s1 := 'Z+';
                3: s1 := '-Z';
              end;
              ACode := format(opRegStr, ['ld', d, s1]);
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
              ACode := format(opStrReg, ['std', 'Y+'+IntToStr(q), d])
            else
            begin
              case (code and 3) of
                0: s1 := 'Y';
                1: s1 := 'Y+';
                3: s1 := '-Y';
              end;
              ACode := format(opStrReg, ['st', s1, d]);
            end;
          end
          else  // load
          begin
            if q > 0 then
              ACode := format(opRegStr, ['ldd', d, 'Y+'+IntToStr(q)])
            else
            begin
              case (code and 3) of
                0: s1 := 'Y';
                1: s1 := 'Y+';
                3: s1 := '-Y';
              end;
              ACode := format(opRegStr, ['ld', d, s1]);
            end;
          end;
        end;
        else
          ACode := InvalidOpCode(code);
      end;
    end;
    $9000:
    begin
      if ((code and $ff0f) = $9408) then  // clear/set SREG flags
      begin
        k := (code shr 4) and 7;
        if ((code and $0080) = 0) then
          s1 := 'se'
        else
          s1 := 'cl';
        ACode := format(opOnly, [s1 + statusFlagNames[k]]);
      end
      else
        case (code) of
          $9409: ACode := format(opOnly, ['ijmp']);
          $9419: ACode := format(opOnly, ['eijmp']);
          $9508: ACode := format(opOnly, ['ret']);
          $9509: ACode := format(opOnly, ['icall']);
          $9519: ACode := format(opOnly, ['eicall']);
          $9518: ACode := format(opOnly, ['reti']);
          $9588: ACode := format(opOnly, ['sleep']);
          $9598: ACode := format(opOnly, ['break']);
          $95a8: ACode := format(opOnly, ['wdr']);
          $95c8: ACode := format(opOnly, ['lpm']);
          $95d8: ACode := format(opOnly, ['elpm']);
          $95e8: ACode := format(opOnly, ['spm']);
          $95f8: ACode := format(opStr, ['spm', 'Z+']);

          $9408, $9418, $9428, $9438, $9448, $9458, $9468,
          $9478:
          begin // BSET 1001 0100 0ddd 1000
            d := (code shr 4) and 7;
            ACode := format(opConst, ['bset', d]);
          end;
          $9488, $9498, $94a8, $94b8, $94c8, $94d8, $94e8,
          $94f8: // bit 7 is 'clear vs set'
          begin // BCLR 1001 0100 1ddd 1000
            d := (code shr 4) and 7;
            ACode := format(opConst, ['bclr', d]);
          end;
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
                ACode := format(opRegConstHex16, ['lds', r, addr16]);
              end;
              $9005,
              $9004:
              begin // LPM Load Program Memory 1001 000d dddd 01oo
                r := (code shr 4) and $1f;
                if (code and 1 = 1) then
                  s1 := 'Z+'
                else
                  s1 := 'Z';
                ACode := format(opRegStr, ['lpm', r, s1]);
              end;
              $9006,
              $9007:
              begin // ELPM Extended Load Program Memory 1001 000d dddd 01oo
                r := (code shr 4) and $1f;
                if (code and 1 = 1) then
                  s1 := 'Z+'
                else
                  s1 := 'Z';
                ACode := format(opRegStr, ['elpm', r, s1]);
              end;
              $900c,
              $900d,
              $900e:
              begin // LD Load Indirect from fData using X 1001 000r rrrr 11oo
                r := (code shr 4) and $1f;
                if (code and 3 = 1) then
                  s1 := 'X+'
                else if (code and 3 = 2) then
                  s1 := '-X'
                else
                  s1 := 'X';
                ACode := format(opRegStr, ['ld', r, s1]);
              end;
              $920c,
              $920d,
              $920e:
              begin // ST Store Indirect fData Space X 1001 001r rrrr 11oo
                r := (code shr 4) and $1f;
                if (code and 3 = 1) then
                  s1 := 'X+'
                else if (code and 3 = 2) then
                  s1 := '-X'
                else
                  s1 := 'X';
                ACode := format(opStrReg, ['st', s1, r]);
              end;
              $9009,
              $900a:
              begin // LD Load Indirect from fData using Y 1001 000r rrrr 10oo
                r := (code shr 4) and $1f;
                if (code and 3 = 1) then
                  s1 := 'Y+'
                else if (code and 3 = 2) then
                  s1 := '-Y';
                ACode := format(opRegStr, ['ld', r, s1]);
              end;
              $9209,
              $920a:
              begin // ST Store Indirect fData Space Y 1001 001r rrrr 10oo
                r := (code shr 4) and $1f;
                if (code and 3 = 1) then
                  s1 := 'Y+'
                else if (code and 3 = 2) then
                  s1 := '-Y';
                ACode := format(opStrReg, ['st', s1, r]);
              end;
              $9200:
              begin // STS Store Direct to Data Space, 32 bits
                r := (code shr 4) and $1f;
                addr16 := pcode[CodeIdx];
                inc(CodeIdx);
                addr16 := addr16 or (pcode[CodeIdx] shl 8);
                inc(CodeIdx);
                ACode := format(opConstHex16Reg, ['sts', addr16, r]);
              end;
              $9001,
              $9002:
              begin // LD Load Indirect from Data using Z 1001 001r rrrr 00oo
                r := (code shr 4) and $1f;
                if (code and 3 = 1) then
                  s1 := 'Z+'
                else
                  s1 := '-Z';
                ACode := format(opRegStr, ['ld', r, s1]);
              end;
              $9201,
              $9202:
              begin // ST Store Indirect Data Space Z 1001 001r rrrr 0Xoo X=0 or XMega instructions X=1
                r := (code shr 4) and $1f;
                if (code and 4) = 0 then  // normal AVR8 instruction
                begin
                  if (code and 3 = 1) then
                    s1 := 'Z+'
                  else
                    s1 := '-Z';
                  ACode := format(opStrReg, ['st', s1, r]);
                end
                else
                begin  // AVR8X instructions
                  case (code and 3) of
                    0: s1 := 'xch';
                    1: s1 := 'las';
                    2: s1 := 'lac';
                    3: s1 := 'lat';
                  end;
                  ACode := format(opStrReg, [s1, 'Z', r]);
                end;
              end;
              $900f:
              begin // POP 1001 000d dddd 1111
                r := (code shr 4) and $1f;
                ACode := format(opReg, ['pop', r]);
              end;
              $920f:
              begin // PUSH 1001 001d dddd 1111
                r := (code shr 4) and $1f;
                ACode := format(opReg, ['push', r]);
              end;
              $9400:
              begin // COM – One’s Complement
                r := (code shr 4) and $1f;
                ACode := format(opReg, ['com', r]);
              end;
              $9401:
              begin // NEG – Two’s Complement
                r := (code shr 4) and $1f;
                ACode := format(opReg, ['neg', r]);
              end;
              $9402:
              begin // SWAP – Swap Nibbles
                r := (code shr 4) and $1f;
                ACode := format(opReg, ['swap', r]);
              end;
              $9403:
              begin // INC – Increment
                r := (code shr 4) and $1f;
                ACode := format(opReg, ['inc', r]);
              end;
              $9405:
              begin // ASR – Arithmetic Shift Right 1001 010d dddd 0101
                r := (code shr 4) and $1f;
                ACode := format(opReg, ['asr', r]);
              end;
              $9406:
              begin // LSR 1001 010d dddd 0110
                r := (code shr 4) and $1f;
                ACode := format(opReg, ['lsr', r]);
              end;
              $9407:
              begin // ROR 1001 010d dddd 0111
                r := (code shr 4) and $1f;
                ACode := format(opReg, ['ror', r]);
              end;
              $940a:
              begin // DEC – Decrement
                r := (code shr 4) and $1f;
                ACode := format(opReg, ['dec', r]);
              end;
              $940c,
              $940d:
              begin // JMP Long Call to sub, 32 bits
                k := ((code and $01f0) shr 3) or (code and 1);
                addr16 := pcode[CodeIdx];
                inc(CodeIdx);
                addr16 := addr16 or (pcode[CodeIdx] shl 8);
                inc(CodeIdx);
                ACode := format(opConstHex8, ['jmp', (dword(k shl 16) or dword(addr16)) shl 1]);
              end;
              $940e,
              $940f:
              begin // CALL Long Call to sub, 32 bits
                k := ((code and $01f0) shr 3) or (code and 1);
                addr16 := pcode[CodeIdx];
                inc(CodeIdx);
                addr16 := addr16 or (pcode[CodeIdx] shl 8);
                inc(CodeIdx);
                ACode := format(opConstHex8, ['call', (dword(k shl 16) or dword(addr16)) shl 1]);
              end;
              else
              begin
                case (code and $ff00) of
                  $9600:
                  begin // ADIW - Add Immediate to Word 1001 0110 KKdd KKKK
                    r := 24 + ((code shr 3) and $6);
                    k := ((code and $00c0) shr 2) or (code and $f);
                    ACode := format(opRegConstHex8, ['adiw', r, k]);
                  end;
                  $9700:
                  begin // SBIW - Subtract Immediate from Word 1001 0110 KKdd KKKK
                    r := 24 + ((code shr 3) and $6);
                    k := ((code and $00c0) shr 2) or (code and $f);
                    ACode := format(opRegConstHex8, ['sbiw', r, k]);
                  end;
                  $9800:
                  begin // CBI - Clear Bit in I/O Register 1001 1000 AAAA Abbb
                    d := (code shr 3) and $1f;
                    k := code and $7;
                    ACode := format(opConstHex8Const, ['cbi', d, k]);
                  end;
                  $9900:
                  begin // SBIC - Skip if Bit in I/O Register is Cleared 1001 0111 AAAA Abbb
                    d := (code shr 3) and $1f;
                    k := code and $7;
                    ACode := format(opConstHex8Const, ['sbic', d, k]);
                  end;
                  $9a00:
                  begin // SBI - Set Bit in I/O Register 1001 1000 AAAA Abbb
                    d := (code shr 3) and $1f;
                    k := code and $7;
                    ACode := format(opConstHex8Const, ['sbi', d, k]);
                  end;
                  $9b00:
                  begin // SBIS - Skip if Bit in I/O Register is Set 1001 1011 AAAA Abbb
                    d := (code shr 3) and $1f;
                    k := code and $7;
                    ACode := format(opConstHex8Const, ['sbis', d, k]);
                  end;
                  else
                    case (code and $fc00) of
                      $9c00:
                      begin // MUL - Multiply Unsigned 1001 11rd dddd rrrr
                        get_r_d_10(code, r, d);
                        ACode := format(opRegReg, ['mul', d, r]);
                      end;
                      else
                        ACode := InvalidOpCode(code);
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
          ACode := format(opConstHex8Reg, ['out', d, r]);
        end;
        $b000:
        begin // IN Rd,A 1011 0AAr rrrr AAAA
          r := (code shr 4) and $1f;
          d := ((((code shr 9) and 3) shl 4) or ((code) and $f));
          ACode := format(opRegConstHex8, ['in', r, d]);
        end;
        else
          ACode := InvalidOpCode(code);
      end;
    end;
    $c000:
    begin // RJMP 1100 kkkk kkkk kkkk
      a := smallint((word(code) shl 4) and $ffff) div 16;
      ACode := format(opStr, ['rjmp', '.'+IntToStr(a shl 1)]);
    end;
    $d000:
    begin // RCALL 1100 kkkk kkkk kkkk
      a := smallint((word(code) shl 4) and $ffff) div 16;
      ACode := format(opStr, ['rcall', '.'+IntToStr(a shl 1)]);
    end;
    $e000:
    begin // LDI Rd, K 1110 KKKK RRRR KKKK -- aka SER (LDI r, $ff)
      d := 16 + ((code shr 4) and $f);
      k := ((code and $0f00) shr 4) or (code and $f);
      ACode := format(opRegConstHex8, ['ldi', d, k]);
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
           if (_set) then
             ACode := format(opStr, [branchIfSetNames[k], '.'+IntToStr(a)])
           else
             ACode := format(opStr, [branchIfClrNames[k], '.'+IntToStr(a)]);
        end;
        $f800,
        $f900:
        begin // BLD – Bit Load from T into a Bit in Register 1111 100r rrrr 0bbb
          d := (code shr 4) and $1f; // register index
          k := code and 7;
          ACode := format(opRegConst, ['bld', d, k]);
        end;
        $fa00,
        $fb00:
        begin // BST – Bit Store into T from bit in Register 1111 100r rrrr 0bbb
          r := (code shr 4) and $1f; // register index
          k := code and 7;
          ACode := format(opRegConst, ['bst', r, k]);
        end;
        $fc00,
        $fe00:
        begin // SBRS/SBRC – Skip if Bit in Register is Set/Clear 1111 11sr rrrr 0bbb
          r := (code shr 4) and $1f; // register index
          k := code and 7;
          _set := (code and $0200) <> 0;
          if _set then
            ACode := format(opRegConst, ['sbrs', r, k])
          else
            ACode := format(opRegConst, ['sbrc', r, k]);
        end;
        else
          ACode := InvalidOpCode(code);
      end;
    end;
    else
      ACode := InvalidOpCode(code);
  end;

  // memory
  ACodeBytes := '';
  for k := 0 to CodeIdx - 1 do
    ACodeBytes := ACodeBytes + HexStr(pcode[k], 2);

  Inc(AAddress, CodeIdx);
end;

function TAvrAsmDecoder.GetInstructionInfo(AnAddress: TDBGPtr
  ): TDbgAsmInstruction;
begin
  if (FLastInstr = nil) or (FLastInstr.RefCount > 1) then begin
    ReleaseRefAndNil(FLastInstr);
    FLastInstr := TAvrAsmInstruction.Create(Self);
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
var
  ADataLen: Cardinal;
  AData: PByte;
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

initialization
  DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );

end.
