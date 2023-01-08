{
 ---------------------------------------------------------------------------
 FpDbgDwarfCFI.pas - Native Freepascal debugger - Call Frame Information
 ---------------------------------------------------------------------------

 This unit contains classes to process the Dwarf Call Frame Information
 information

 ---------------------------------------------------------------------------

 @created(Wed Jun 16th WET 2022)
 @lastmod($Date$)
 @author(Joost van der Sluis <joost@@cnoc.nl>)

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
unit FpDbgDwarfCFI;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  // LazUtils
  Maps,
  LazLogger,
  // DebuggerIntf
  DbgIntfBaseTypes,
  // FpDebug
  FpDbgCommon,
  FpDbgUtil,
  FpDbgDwarfConst,
  FpDbgClasses;

type
  PDwarfCIEEntryHeader32 = ^TDwarfCIEEntryHeader32;
  TDwarfCIEEntryHeader32 = record
    Length: LongWord;
    CIEId: LongWord;
    Version: Byte;
    Augmentation: array[0..1023] of char;
  end;

  PDwarfCIEEntryHeader64 = ^TDwarfCIEEntryHeader64;
  TDwarfCIEEntryHeader64 = record
    Signature: LongWord;
    Length: QWord;
    CIEId: QWord;
    Version: Byte;
    Augmentation: array[0..1023] of char;
  end;

  PDwarfFDEEntryHeader32 = ^TDwarfFDEEntryHeader32;
  TDwarfFDEEntryHeader32 = record
    Length: LongWord;
    CIEPointer: LongWord;
    InitialLocation: LongWord;
    Initialrange: LongWord;
  end;

  PDwarfFDEEntryHeader64 = ^TDwarfFDEEntryHeader64;
  TDwarfFDEEntryHeader64 = record
    Signature: LongWord;
    Length: QWord;
    CIEPointer: QWord;
    InitialLocation: PQWord;
  end;

  TDwarfCallFrameInformationInstructions = array of Byte;

  { TDwarfCIE }

  TDwarfCIE = class
  private
    FAddressSize: Byte;
    FAugmentation: string;
    FCodeAlignmentFactor: QWord;
    FDataAlignmentFactor: Int64;
    FInitialInstructions: TDwarfCallFrameInformationInstructions;
    FReturnAddressRegister: QWord;
    FSegmentSize: Byte;
    FVersion: Byte;
  public
    constructor Create(AVersion: Byte; AnAugmentation: string);
    property Version: Byte read FVersion;
    property Augmentation: string read FAugmentation;
    property AddressSize: Byte read FAddressSize write FAddressSize;
    property SegmentSize: Byte read FSegmentSize write FSegmentSize;
    property CodeAlignmentFactor: QWord read FCodeAlignmentFactor write FCodeAlignmentFactor;
    property DataAlignmentFactor: Int64 read FDataAlignmentFactor write FDataAlignmentFactor;
    property ReturnAddressRegister: QWord read FReturnAddressRegister write FReturnAddressRegister;
    property InitialInstructions: TDwarfCallFrameInformationInstructions read FInitialInstructions write FInitialInstructions;
  end;

  TDwarfCallFrameInformationRegisterRule = (
    cfiUndefined,
    cfiSameValue,
    cfiOffset,
    cfiValOffset,
    cfiRegister,
    cfiExpression,
    cfiValExpression,
    cfiArchitectural
  );
  TDwarfCallFrameInformationCFARule = (
    cfaUndefined,
    cfaRegister,
    cfaExpression
  );

  { TDwarfCallFrameInformationRule }

  TDwarfCallFrameInformationRule = record
    Offset: Int64;
    &Register: QWord;
    // Dwarf-expressions are unsupported
    // Expression: TDwarfLocationExpression;
    case byte of
      0: (RegisterRule: TDwarfCallFrameInformationRegisterRule);
      1: (CFARule: TDwarfCallFrameInformationCFARule);
  end;
  TDwarfCallFrameInformationRow = record
    // The DWARF CFI specification defines the Location and CFARule as the
    // first two columns of the array.
    // By keeping them separate, the code is much cleaner though.
    Location: TDBGPtr;
    CFARule: TDwarfCallFrameInformationRule;
    RegisterArray: array of TDwarfCallFrameInformationRule;
  end;

  { TDwarfFDE }

  TDwarfFDE = class
  private
    FAddressRange: QWord;
    FCIEPointer: QWord;
    FInitialLocation: TDBGPtr;
    FInstructions: TDwarfCallFrameInformationInstructions;
    FSegmentSelector: TDBGPtr;
  public
    constructor Create(ACIEPointer: QWord; AnInitialLocation, ASegmentSelector: TDBGPtr; AnAddressRange: QWord);
    property CIEPointer: QWord read FCIEPointer;
    property InitialLocation: TDBGPtr read FInitialLocation;
    property SegmentSelector: TDBGPtr read FSegmentSelector;
    property AddressRange: QWord read FAddressRange;
    property Instructions: TDwarfCallFrameInformationInstructions read FInstructions write FInstructions;
  end;

  { TDwarfCallFrameInformation }

  TDwarfCallFrameInformation = class
  private
    FFDEMap: TMap;
    FCIEMap: TMap;
    FInitialInstructionsCache: TDwarfCallFrameInformationRow;
    FRowStack: array of TDwarfCallFrameInformationRow;
  protected
    function SetCallFrameInformationRegisterCell(var InformationRow: TDwarfCallFrameInformationRow; &Register: QWord; Rule: TDwarfCallFrameInformationRegisterRule; Offset: Int64; SourceRegister: Byte): Boolean;
    function SetCallFrameInformationCFACell(var InformationRow: TDwarfCallFrameInformationRow; Rule: TDwarfCallFrameInformationCFARule; &Register: Byte; Offset: Int64): Boolean;
    // Process Call Frame Instructions on a given (CFI) row. Return True if succesfull.
    // When False returned the InformationRow can not be trusted.
    function ProcessInstructions(const CIE: TDwarfCIE; var InformationRow: TDwarfCallFrameInformationRow; const Instructions: TDwarfCallFrameInformationInstructions; const InitialAddress, SearchAddress: TDBGPtr): Boolean;
    procedure InitializeABIRules(TargetInfo: TTargetDescriptor; var Row: TDwarfCallFrameInformationRow);
    function CloneRow(const SourceRow: TDwarfCallFrameInformationRow): TDwarfCallFrameInformationRow;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddCIE(AnOffset: QWord; ACIE: TDwarfCIE);
    procedure AddFDE(AFDE: TDwarfFDE);
    function FindFDEForAddress(AnAddress: TDBGPtr): TDwarfFDE;
    function FindCIEForOffset(AnOffset: QWord): TDwarfCIE;
    function GetRow(TargetInfo: TTargetDescriptor; AnAddress: TDBGPtr; out CIE: TDwarfCIE; out Row: TDwarfCallFrameInformationRow): Boolean;

    class function TryObtainNextCallFrame(
      CurrentCallStackEntry: TDbgCallstackEntry;
      CIE: TDwarfCIE;
      Size, NextIdx: Integer;
      Thread: TDbgThread;
      Row: TDwarfCallFrameInformationRow;
      Process: TDbgProcess;
      out NewCallStackEntry: TDbgCallstackEntry): Boolean;
  end;

implementation

var
  FPDBG_DWARF_CFI_WARNINGS: PLazLoggerLogGroup;

type

  { TCFIMap }

  TCFIMap = class(TMap)
  protected
    procedure ReleaseData(ADataPtr: Pointer); override;
  end;

{ TCFIMap }

procedure TCFIMap.ReleaseData(ADataPtr: Pointer);
begin
  TObject(ADataPtr^).Free;
  inherited ReleaseData(ADataPtr);
end;

{ TDwarfCallFrameInformation }

function TDwarfCallFrameInformation.SetCallFrameInformationRegisterCell(var InformationRow: TDwarfCallFrameInformationRow; &Register: QWord; Rule: TDwarfCallFrameInformationRegisterRule; Offset: Int64; SourceRegister: Byte): Boolean;
var
  Row: Byte;
begin
  Result := True;
  if &Register > High(Byte) then
    begin
    DebugLn(FPDBG_DWARF_CFI_WARNINGS, ['Call frame instruction register-number out of bounds']);
    Exit(False);
    end;
  Row := &Register;

  if Length(InformationRow.RegisterArray) < Row+1 then
    SetLength(InformationRow.RegisterArray, Row+1);
  InformationRow.RegisterArray[Row].RegisterRule := Rule;
  InformationRow.RegisterArray[Row].&Register := SourceRegister;
  InformationRow.RegisterArray[Row].Offset := Offset;
end;

function TDwarfCallFrameInformation.SetCallFrameInformationCFACell(var InformationRow: TDwarfCallFrameInformationRow; Rule: TDwarfCallFrameInformationCFARule; &Register: Byte; Offset: Int64): Boolean;
begin
  Result := True;
  InformationRow.CFARule.CFARule := Rule;
  InformationRow.CFARule.&Register := &Register;
  InformationRow.CFARule.Offset := Offset;
end;

function TDwarfCallFrameInformation.ProcessInstructions(const CIE: TDwarfCIE; var InformationRow: TDwarfCallFrameInformationRow; const Instructions: TDwarfCallFrameInformationInstructions; const InitialAddress, SearchAddress: TDBGPtr): Boolean;
var
  p: PByte;
  pw: PWord absolute p;
  pl: PLongWord absolute p;
  Instruction: Byte;
  CurrentLocation: TDBGPtr;
  uparam1, uparam2: QWord;
  sparam: Int64;
begin
  if Length(Instructions) = 0 then
    begin
    Result := True;
    Exit;
    end;
  Result := False;
  p := @Instructions[0];
  CurrentLocation:=InitialAddress;
  while p < Length(Instructions)+@Instructions[0] do
    begin
    Instruction := p^;
    Inc(p);
    case Instruction of
      DW_CFA_nop:
        begin
        end;
      DW_CFA_set_loc:
        begin
        if not cie.AddressSize in [1, 2, 4, 8] then
          DebugLn(FPDBG_DWARF_CFI_WARNINGS, ['Unsupported address size'])
        else
          CurrentLocation := ReadUnsignedFromExpression(p, CIE.AddressSize);
        end;
      DW_CFA_advance_loc1:
        begin
        Inc(CurrentLocation, p^);
        if CurrentLocation>SearchAddress then
          Exit(True);
        Inc(p);
        end;
      DW_CFA_advance_loc2:
        begin
        Inc(CurrentLocation, pw^);
        if CurrentLocation>SearchAddress then
          Exit(True);
        Inc(p,2);
        end;
      DW_CFA_advance_loc4:
        begin
        Inc(CurrentLocation, pl^);
        if CurrentLocation>SearchAddress then
          Exit(True);
        Inc(p,4);
        end;
      DW_CFA_offset_extended:
        begin
        uparam1 := ULEB128toOrdinal(p); // Register number
        uparam2 := ULEB128toOrdinal(p); // Factored offset
        if not SetCallFrameInformationRegisterCell(InformationRow, uparam1, cfiOffset, uparam2*CIE.DataAlignmentFactor, 0) then
          Exit;
        end;
      DW_CFA_offset_extended_sf:
        begin
        uparam1 := ULEB128toOrdinal(p); // Register number
        sparam := SLEB128toOrdinal(p); // Factored offset
        if not SetCallFrameInformationRegisterCell(InformationRow, uparam1, cfiOffset, sparam*CIE.DataAlignmentFactor, 0) then
          Exit;
        end;
      DW_CFA_restore:
        begin
        uparam1 := ULEB128toOrdinal(p); // Register number
        if not SetCallFrameInformationRegisterCell(InformationRow, uparam1, FInitialInstructionsCache.RegisterArray[uparam1].RegisterRule, FInitialInstructionsCache.RegisterArray[uparam1].Offset, FInitialInstructionsCache.RegisterArray[uparam1].&Register) then
          Exit;
        end;
      DW_CFA_val_offset:
        begin
        uparam1 := ULEB128toOrdinal(p); // Register number
        uparam2 := ULEB128toOrdinal(p); // Factored offset
        if not SetCallFrameInformationRegisterCell(InformationRow, uparam1, cfiValOffset, uparam2*CIE.DataAlignmentFactor, 0) then
          Exit;
        end;
      DW_CFA_val_offset_sf:
        begin
        uparam1 := ULEB128toOrdinal(p); // Register number
        sparam := SLEB128toOrdinal(p); // Factored offset
        if not SetCallFrameInformationRegisterCell(InformationRow, uparam1, cfiValOffset, sparam*CIE.DataAlignmentFactor, 0) then
          Exit;
        end;
      DW_CFA_undefined:
        begin
        uparam1 := ULEB128toOrdinal(p); // Register number
        if not SetCallFrameInformationRegisterCell(InformationRow, uparam1, cfiUndefined, 0, 0) then
          Exit;
        end;
      DW_CFA_same_value:
        begin
        uparam1 := ULEB128toOrdinal(p); // Register number
        if not SetCallFrameInformationRegisterCell(InformationRow, uparam1, cfiSameValue, 0, 0) then
          Exit;
        end;
      DW_CFA_register:
        begin
        uparam1 := ULEB128toOrdinal(p); // Register number
        uparam2 := ULEB128toOrdinal(p); // Register number
        if not SetCallFrameInformationRegisterCell(InformationRow, uparam1, cfiRegister, 0, uparam2) then
          Exit;
        end;
      DW_CFA_remember_state:
        begin
        SetLength(FRowStack, Length(FRowStack) + 1);
        FRowStack[High(FRowStack)] := CloneRow(InformationRow);
        end;
      DW_CFA_restore_state:
        begin
        InformationRow := FRowStack[High(FRowStack)];
        SetLength(FRowStack, Length(FRowStack) - 1);
        end;
      DW_CFA_def_cfa:
        begin
        uparam1 := ULEB128toOrdinal(p); // Register number
        uparam2 := ULEB128toOrdinal(p); // Non-factored offset
        if not SetCallFrameInformationCFACell(InformationRow, cfaRegister, uparam1, uparam2) then
          Exit;
        end;
      DW_CFA_def_cfa_sf:
        begin
        uparam1 := ULEB128toOrdinal(p); // Register number
        sparam := SLEB128toOrdinal(p); // Factored offset
        if not SetCallFrameInformationCFACell(InformationRow, cfaRegister, uparam1, sparam*CIE.DataAlignmentFactor) then
          Exit;
        end;
      DW_CFA_def_cfa_register:
        begin
        uparam1 := ULEB128toOrdinal(p); // Register
        if InformationRow.CFARule.CFARule = cfaRegister then
          begin
          if not SetCallFrameInformationCFACell(InformationRow, cfaRegister, uparam1, InformationRow.CFARule.Offset) then
            Exit;
          end
        else
          DebugLn(FPDBG_DWARF_CFI_WARNINGS, ['Invalid DW_CFA_def_cfa_register rule']);
        end;
      DW_CFA_def_cfa_offset:
        begin
        uparam1 := ULEB128toOrdinal(p); // Non-factored offset
        if InformationRow.CFARule.CFARule = cfaRegister then
          begin
          if not SetCallFrameInformationCFACell(InformationRow, cfaRegister, InformationRow.CFARule.&Register, uparam1) then
            Exit;
          end
        else
          DebugLn(FPDBG_DWARF_CFI_WARNINGS, ['Invalid DW_CFA_def_cfa_offset rule']);
        end;
      DW_CFA_def_cfa_offset_sf:
        begin
        sparam := SLEB128toOrdinal(p); // Factored offset
        if InformationRow.CFARule.CFARule = cfaRegister then
          begin
          if not SetCallFrameInformationCFACell(InformationRow, cfaRegister, InformationRow.CFARule.&Register, sparam*CIE.DataAlignmentFactor) then
            Exit;
          end
        else
          DebugLn(FPDBG_DWARF_CFI_WARNINGS, ['Invalid DW_CFA_def_cfa_offset_sf rule']);
        end;
      else
        begin
        case Instruction and $c0 of
          DW_CFA_advance_loc:
            begin
            Inc(CurrentLocation, (Instruction and $3f)*CIE.CodeAlignmentFactor);
            if CurrentLocation>SearchAddress then
              Exit(True);
            end;
          DW_CFA_offset:
            begin
            uparam1 := Instruction and $3f; // Register number
            uparam2 := ULEB128toOrdinal(p); // Factored offset
            if not SetCallFrameInformationRegisterCell(InformationRow, uparam1, cfiOffset, uparam2*CIE.DataAlignmentFactor, 0) then
              Exit;
            end;
          DW_CFA_restore:
            begin
            uparam1 := Instruction and $3f; // Register number
            if not SetCallFrameInformationRegisterCell(InformationRow, uparam1, FInitialInstructionsCache.RegisterArray[uparam1].RegisterRule, FInitialInstructionsCache.RegisterArray[uparam1].Offset, FInitialInstructionsCache.RegisterArray[uparam1].&Register) then
              Exit;
            end
        else
          DebugLn(FPDBG_DWARF_CFI_WARNINGS, ['Unsupported call frame instruction: ', Instruction]);
          Exit;
        end;
        end;
    end;
    end;
  // This function only proceeded if we reach this point. If the handling is aborted
  // for any reason, the result is unreliable and can not be used.
  Result := True;
end;

procedure TDwarfCallFrameInformation.InitializeABIRules(TargetInfo: TTargetDescriptor; var Row: TDwarfCallFrameInformationRow);
begin
  if TargetInfo.machineType=mtX86_64 then
    begin
    // According to the x86_64 ABI the CFA (call frame address) is defined as
    // "the value of %rsp at the call site in the previous frame"
    // Register 7 is %rsp
    SetCallFrameInformationRegisterCell(Row, 7, cfiValOffset, 0, 0);

    // From the x86_64 ABI:
    // "Registers %rbp, %rbx and %r12 through %r15 “belong” to the calling
    // function and the called function is required to preserve their values."
    // Register 3 is %rbx and 6 is %rbp
    SetCallFrameInformationRegisterCell(Row, 3, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 6, cfiSameValue, 0, 0);

    SetCallFrameInformationRegisterCell(Row, 12, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 13, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 14, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 15, cfiSameValue, 0, 0);
    end
  else if TargetInfo.machineType=mt386 then
    begin
    SetCallFrameInformationRegisterCell(Row, 4, cfiValOffset, 0, 0);

    // The i386 ABI is not as clear as the x86_64 ABI. These are based on
    // some educated guesses:
    SetCallFrameInformationRegisterCell(Row, 3, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 5, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 6, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 7, cfiSameValue, 0, 0);
    end
  else if TargetInfo.machineType=mtAVR8 then
    begin
    // Registers r2..r17, r28, r29 are call saved
    SetCallFrameInformationRegisterCell(Row, 2, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 3, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 4, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 5, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 6, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 7, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 8, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 9, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 10, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 11, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 12, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 13, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 14, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 15, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 16, cfiSameValue, 0, 0);
    SetCallFrameInformationRegisterCell(Row, 17, cfiSameValue, 0, 0);
    end;
end;

function TDwarfCallFrameInformation.CloneRow(const SourceRow: TDwarfCallFrameInformationRow): TDwarfCallFrameInformationRow;
begin
  Result := SourceRow;
  // Create a deep-copy of the array.
  Result.RegisterArray := copy(SourceRow.RegisterArray, 0, Length(SourceRow.RegisterArray));
end;

constructor TDwarfCallFrameInformation.Create;
begin
  FFDEMap := TCFIMap.Create(itu8, SizeOf(Pointer));
  FCIEMap := TCFIMap.Create(itu8, SizeOf(Pointer));
end;

destructor TDwarfCallFrameInformation.Destroy;
begin
  FFDEMap.Free;
  FCIEMap.Free;
  inherited Destroy;
end;

procedure TDwarfCallFrameInformation.AddCIE(AnOffset: QWord; ACIE: TDwarfCIE);
begin
  FCIEMap.Add(AnOffset, ACIE);
end;

procedure TDwarfCallFrameInformation.AddFDE(AFDE: TDwarfFDE);
begin
  FFDEMap.Add(AFDE.InitialLocation, AFDE);
end;

function TDwarfCallFrameInformation.FindFDEForAddress(AnAddress: TDBGPtr): TDwarfFDE;
var
  Iter: TLockedMapIterator;
  FDE: TDwarfFDE;
begin
  Result := nil;

  Iter := TLockedMapIterator.Create(FFDEMap);
  try
    if not Iter.Locate(AnAddress) then
      begin
      if not Iter.BOM then
        Iter.Previous;
      end;

    if not Iter.BOM then
      begin
      // iter is at the closest defined address before AAddress
      FDE := TDwarfFDE(Iter.DataPtr^);

      if AnAddress <= FDE.InitialLocation + FDE.AddressRange then
        begin
        Result := FDE;
        end;
      end;
  finally
    Iter.Free;
  end;
end;

function TDwarfCallFrameInformation.FindCIEForOffset(AnOffset: QWord): TDwarfCIE;
var
  Iter: TLockedMapIterator;
begin
  Result := nil;

  Iter := TLockedMapIterator.Create(FCIEMap);
  try
    if not Iter.Locate(AnOffset) then
      begin
      if not Iter.BOM then
        Iter.Previous;
      end;

    if not Iter.BOM then
      begin
      // iter is at the closest defined address before AAddress
      Result := TDwarfCIE(Iter.DataPtr^);
      end;
  finally
    Iter.Free;
  end;
end;

function TDwarfCallFrameInformation.GetRow(TargetInfo: TTargetDescriptor; AnAddress: TDBGPtr; out CIE: TDwarfCIE; out Row: TDwarfCallFrameInformationRow): Boolean;
var
  FDE: TDwarfFDE;
begin
  Result := False;
  FRowStack := [];
  Row := Default(TDwarfCallFrameInformationRow);
  FDE := FindFDEForAddress(AnAddress);
  if Assigned(FDE) then
    begin
    InitializeABIRules(TargetInfo, Row);
    CIE := FindCIEForOffset(FDE.CIEPointer);
    if ProcessInstructions(CIE, Row, CIE.InitialInstructions, FDE.InitialLocation, AnAddress) then
      begin
      FInitialInstructionsCache := CloneRow(Row);
      Result := ProcessInstructions(CIE, Row, FDE.Instructions, FDE.InitialLocation, AnAddress);
      end;
    end;
end;

class function TDwarfCallFrameInformation.TryObtainNextCallFrame(
  CurrentCallStackEntry: TDbgCallstackEntry;
  CIE: TDwarfCIE;
  Size, NextIdx: Integer;
  Thread: TDbgThread;
  Row: TDwarfCallFrameInformationRow;
  Process: TDbgProcess;
  out NewCallStackEntry: TDbgCallstackEntry): Boolean;

  function ProcessCFIColumn(Row: TDwarfCallFrameInformationRow; Column: Byte; CFA: QWord; AddressSize: Integer; Entry: TDbgCallstackEntry; out Value: TDbgPtr): Boolean;
  var
    Rule: TDwarfCallFrameInformationRule;
    Reg: TDbgRegisterValue;
  begin
    Result := True;
    Value := 0;
    Rule := Row.RegisterArray[Column];
    case Rule.RegisterRule of
      cfiUndefined:
        begin
        Result := False;
        end;
      cfiSameValue:
        begin
        Reg := CurrentCallStackEntry.RegisterValueList.FindRegisterByDwarfIndex(Column);
        if Assigned(Reg) then
          Value := Reg.NumValue
        else
          Result := False;
        end;
      cfiOffset:
        begin
        Process.ReadData(CFA+Rule.Offset, AddressSize, Value);
        end;
      cfiValOffset:
        begin
        Value := CFA+Rule.Offset;
        end;
      cfiRegister:
        begin
        Reg := CurrentCallStackEntry.RegisterValueList.FindRegisterByDwarfIndex(Rule.&Register);
        if Assigned(Reg) then
          Value := Reg.NumValue
        else
          Result := False;
        end
      else
        begin
        DebugLn(FPDBG_DWARF_CFI_WARNINGS, 'Encountered unsupported CFI registerrule.');
        Result := False;
        end;
    end; // case
  end;

var
  Rule: TDwarfCallFrameInformationRule;
  Reg: TDbgRegisterValue;
  i: Integer;
  ReturnAddress, Value: TDbgPtr;
  FrameBase: TDBGPtr;
  RegName: String;
begin
  Result := False;
  NewCallStackEntry := nil;
  // Get CFA (framebase)
  Rule := Row.CFARule;
  case Rule.CFARule of
    cfaRegister:
      begin
      Reg := CurrentCallStackEntry.RegisterValueList.FindRegisterByDwarfIndex(Rule.&Register);
      if Assigned(Reg) then
        begin
        FrameBase := Reg.NumValue;
        FrameBase := FrameBase + Rule.Offset;
        end
      else
        begin
        DebugLn(FPDBG_DWARF_CFI_WARNINGS, 'CFI requested a register [' +IntToStr(Rule.&Register)+ '] that is not available.');
        Exit;
        end;
      end;
    cfaExpression:
      begin
      DebugLn(FPDBG_DWARF_CFI_WARNINGS, 'CFI-expressions are not supported. Not possible to obtain the CFA.');
      Exit;
      end;
    else
      begin
      DebugLn(FPDBG_DWARF_CFI_WARNINGS, 'CFI available but no rule to obtain the CFA.');
      Exit;
      end;
  end; // case

  Result := True;
  // Get return ReturnAddress
  if not ProcessCFIColumn(Row, CIE.ReturnAddressRegister, FrameBase, Size, CurrentCallStackEntry, ReturnAddress) then
    // Yes, we were succesfull, but there is no return ReturnAddress, so keep
    // NewCallStackEntry nil
    begin
    Result := True;
    Exit;
    end;

  if ReturnAddress=0 then
    // Yes, we were succesfull, but there is no frame left, so keep
    // NewCallStackEntry nil
    begin
    Result := True;
    Exit;
    end;

  // We do not strace-back to the return address, we need the calling-address.
  // This is difficult though. But we assume that ReturnAddress-1 is part of
  // the instruction that made the call.
  NewCallStackEntry := TDbgCallstackEntry.create(Thread, NextIdx, FrameBase, ReturnAddress-1);

  // Fill other registers
  for i := 0 to High(Row.RegisterArray) do
    begin
    if ProcessCFIColumn(Row, i, FrameBase, Size, CurrentCallStackEntry, Value) then
      begin
      Reg := CurrentCallStackEntry.RegisterValueList.FindRegisterByDwarfIndex(i);
      if Assigned(Reg) then
        RegName := Reg.Name
      else
        RegName := IntToStr(i);
      NewCallStackEntry.RegisterValueList.DbgRegisterAutoCreate[RegName].SetValue(Value, IntToStr(Value),Size, i);
      end;
    end;
end;

{ TDwarfFDE }

constructor TDwarfFDE.Create(ACIEPointer: QWord; AnInitialLocation, ASegmentSelector: TDBGPtr; AnAddressRange: QWord);
begin
  FCIEPointer := ACIEPointer;
  FInitialLocation := AnInitialLocation;
  FSegmentSelector := ASegmentSelector;
  FAddressRange := AnAddressRange;
end;

{ TDwarfCIE }

constructor TDwarfCIE.Create(AVersion: Byte; AnAugmentation: string);
begin
  FVersion := AVersion;
  FAugmentation := AnAugmentation;
end;

initialization
  FPDBG_DWARF_CFI_WARNINGS  := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_CFI_WARNINGS' {$IFDEF FPDBG_DWARF_CFI_WARNINGS} , True {$ENDIF} );
end.

