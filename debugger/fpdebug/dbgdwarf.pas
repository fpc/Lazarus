{ $Id$ }
{
 ---------------------------------------------------------------------------
 dbgdwarf.pas  -  Native Freepascal debugger - Dwarf symbol reader
 ---------------------------------------------------------------------------

 This unit contains helper classes for loading and resolving of DWARF debug
 symbols

 ---------------------------------------------------------------------------

 @created(Mon Aug 1st WET 2006)
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
unit DbgDwarf;

{$mode objfpc}{$H+}

interface

uses
  Classes, Types, SysUtils, DbgClasses, DbgDwarfConst, Maps, Math,
  DbgLoader, DbgWinExtra, contnrs;
  
type
  // compilation unit header
  {$PACKRECORDS 1}
  PDwarfCUHeader32 = ^TDwarfCUHeader32;
  TDwarfCUHeader32 = record
    Length: LongWord;
    Version: Word;
    AbbrevOffset: LongWord;
    AddressSize: Byte;
  end;

  PDwarfCUHeader64 = ^TDwarfCUHeader64;
  TDwarfCUHeader64 = record
    Signature: LongWord;
    Length: QWord;
    Version: Word;
    AbbrevOffset: QWord;
    AddressSize: Byte;
  end;
  
  // Line number program header
  PDwarfLNPInfoHeader = ^TDwarfLNPInfoHeader;
  TDwarfLNPInfoHeader = record
    MinimumInstructionLength: Byte;
    DefaultIsStmt: Byte;
    LineBase: ShortInt;
    LineRange: Byte;
    OpcodeBase: Byte;
    StandardOpcodeLengths: record end; {array[1..OpcodeBase-1] of Byte}
    {IncludeDirectories: asciiz, asciiz..z}
    {FileNames: asciiz, asciiz..z}
  end;

  PDwarfLNPHeader32 = ^TDwarfLNPHeader32;
  TDwarfLNPHeader32 = record
    UnitLength: LongWord;
    Version: Word;
    HeaderLength: LongWord;
    Info: TDwarfLNPInfoHeader;
  end;

  PDwarfLNPHeader64 = ^TDwarfLNPHeader64;
  TDwarfLNPHeader64 = record
    Signature: LongWord;
    UnitLength: QWord;
    Version: Word;
    HeaderLength: QWord;
    Info: TDwarfLNPInfoHeader;
  end;
  
  {$PACKRECORDS C}
  
const
  DWARF_HEADER64_SIGNATURE = $FFFFFFFF;

type
  TPointerDynArray = array of Pointer;

  TDbgDwarf = class;

  TDwarfAbbrev = record
    tag: Cardinal;
    index: Integer;
    count: Integer;
    Children: Boolean;
  end;
  
  { TDwarfScopeInfo }

  TDwarfScopeInfo = class(Tobject)
  private
    FParent: TDwarfScopeInfo;
    FPrev: TDwarfScopeInfo;
    FNext: TDwarfScopeInfo;
    FChild: TDwarfScopeInfo;
    FChildValid: Boolean; // set is the child is parsed
    FEntry: Pointer;
    procedure SetChild(const AValue: TDwarfScopeInfo);
    procedure SetNext(const AValue: TDwarfScopeInfo);
  protected
  public
    constructor Create(AEntry: Pointer);
    destructor Destroy; override;
    property Parent: TDwarfScopeInfo read FParent;
    property Prev: TDwarfScopeInfo read FPrev;
    property Next: TDwarfScopeInfo read FNext write SetNext;
    property Child: TDwarfScopeInfo read FChild write SetChild;
    property ChildValid: Boolean read FChildValid;
    property Entry: Pointer read FEntry;
  end;

  TDwarfCompilationUnit = class;

  { TDwarfLineInfoStateMachine }

  TDwarfLineInfoStateMachine = class(TObject)
  private
    FOwner: TDwarfCompilationUnit;
    FLineInfoPtr: Pointer;
    FMaxPtr: Pointer;
    FEnded: Boolean;

    FAddress: QWord;
    FFileName: String;
    FLine: Cardinal;
    FColumn: Cardinal;
    FIsStmt: Boolean;
    FBasicBlock: Boolean;
    FEndSequence: Boolean;
    FPrologueEnd: Boolean;
    FEpilogueBegin: Boolean;
    FIsa: QWord;
    
    procedure SetFileName(AIndex: Cardinal);
  protected
  public
    constructor Create(AOwner: TDwarfCompilationUnit; ALineInfoPtr, AMaxPtr: Pointer);
    function Clone: TDwarfLineInfoStateMachine;
    function NextLine: Boolean;
    procedure Reset;
  
    property Address: QWord read FAddress;
    property FileName: String read FFileName;
    property Line: Cardinal read FLine;
    property Column: Cardinal read FColumn;
    property IsStmt: Boolean read FIsStmt;
    property BasicBlock: Boolean read FBasicBlock;
    property EndSequence: Boolean read FEndSequence;
    property PrologueEnd: Boolean read FPrologueEnd;
    property EpilogueBegin: Boolean read FEpilogueBegin;
    property Isa: QWord read FIsa;
    
    property Ended: Boolean read FEnded;
  end;

  PDwarfAddressInfo = ^TDwarfAddressInfo;
  TDwarfAddressInfo = record
    Scope: TDwarfScopeInfo;
    StartPC: QWord;
    EndPC: QWord;
    StateMachine: TDwarfLineInfoStateMachine; // set if info found
    Name: PChar;
  end;

  TDwarfLocateEntryFlag = (
    lefCreateAttribList,
    lefContinuable,  // forces the located scope or the startscope to be contuniable
                     // meaning that tree traversion can continue from a scope
    lefSearchChild,
    lefSearchSibling // search toplevel siblings
  );
  TDwarfLocateEntryFlags = set of TDwarfLocateEntryFlag;

  { TDwarfCompilationUnit }

  TDwarfCompilationUnitClass = class of TDwarfCompilationUnit;
  TDwarfCompilationUnit = class
  private
    FOwner: TDbgDwarf;
    FVerbose: Boolean;
    FValid: Boolean; // set if the compilationunit has compile unit tag.
  
    // --- Header ---
    FLength: QWord;  // length of info
    FVersion: Word;
    FAbbrevOffset: QWord;
    FAddressSize: Byte;  // the adress size of the target in bytes
    FIsDwarf64: Boolean; // Set if the dwarf info in this unit is 64bit
    // ------
    
    FInfoData: Pointer;
    FFileName: String;
    FIdentifierCase: Integer;

    FMap: TMap;
    FDefinitions: array of record
      Attribute: Cardinal;
      Form: Cardinal;
    end;
    FAbbrevIndex: Integer;
    FLastAbbrev: Cardinal;
    FLastAbbrevPtr: Pointer;

    FLineInfo: record
      Header: Pointer;
      DataStart: Pointer;
      DataEnd: Pointer;

      Valid: Boolean;
      Addr64: Boolean;
      MinimumInstructionLength: Byte;
      DefaultIsStmt: Boolean;
      LineBase: ShortInt;
      LineRange: Byte;
      StandardOpcodeLengths: array of Byte; //record end; {array[1..OpcodeBase-1] of Byte}
      Directories: TStringList;
      FileNames: TStringList;
      // the line info is build incrementy when needed
      StateMachine: TDwarfLineInfoStateMachine;
      StateMachines: TFPObjectList; // list of state machines to be freed
    end;
    
    FLineNumberMap: TStringList;

    FAddressMap: TMap;
    FAddressMapBuild: Boolean;
    
    FMinPC: QWord;  // the min and max PC value found in this unit.
    FMaxPC: QWord;  //
    FScope: TDwarfScopeInfo;
    
    procedure BuildAddressMap;
    procedure BuildLineInfo(AAddressInfo: PDwarfAddressInfo; ADoAll: Boolean);
    function  MakeAddress(AData: Pointer): QWord;
    procedure LoadAbbrevs(ANeeded: Cardinal);
  protected
    function LocateEntry(ATag: Cardinal; AStartScope: TDwarfScopeInfo; AFlags: TDwarfLocateEntryFlags; out AResultScope: TDwarfScopeInfo; out AList: TPointerDynArray): Boolean;
    function LocateAttribute(AEntry: Pointer; AAttribute: Cardinal; const AList: TPointerDynArray; out AAttribPtr: Pointer; out AForm: Cardinal): Boolean;

    function ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: Integer): Boolean;
    function ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: Int64): Boolean;
    function ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: Cardinal): Boolean;
    function ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: QWord): Boolean;
    function ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: String): Boolean;
    function ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: PChar): Boolean;
    function ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: TByteDynArray): Boolean;
  public
    constructor Create(AOwner: TDbgDwarf; ADataOffset: QWord; ALength: QWord; AVersion: Word; AAbbrevOffset: QWord; AAddressSize: Byte; AIsDwarf64: Boolean); virtual;
    destructor Destroy; override;
    function GetDefinition(AAbbrev: Cardinal; out ADefinition: TDwarfAbbrev): Boolean;
    function GetLineAddress(const AFileName: String; ALine: Cardinal): TDbgPtr;
    property FileName: String read FFileName;
    property Valid: Boolean read FValid;
  end;
  
  { TDwarfVerboseCompilationUnit }

  TDwarfVerboseCompilationUnit = class(TDwarfCompilationUnit)
  private
  public
    constructor Create(AOwner: TDbgDwarf; ADataOffset: QWord; ALength: QWord; AVersion: Word; AAbbrevOffset: QWord; AAddressSize: Byte; AIsDwarf64: Boolean); override;
  end;


  { TDwarfAbbrevDecoder }

  TDwarfAbbrevDecoder = class(TObject)
  private
    FCU: TDwarfCompilationUnit;
    procedure InternalDecode(AData: Pointer; AMaxData: Pointer; const AIndent: String = '');
  protected
    procedure DecodeLocation(AData: PByte; ASize: QWord; const AIndent: String = '');
    procedure DecodeLocationList(AReference: QWord; const AIndent: String = '');
    function MakeAddressString(AData: Pointer): string;
  public
    constructor Create(ACompilationUnit: TDwarfCompilationUnit);
    procedure Decode;
  end;
  
  { TDwarfStatementDecoder }

  TDwarfStatementDecoder = class(TObject)
  private
    FCU: TDwarfCompilationUnit;
    procedure InternalDecode(AData: Pointer; AMaxData: Pointer; const AIndent: String = '');
  protected
  public
    constructor Create(ACompilationUnit: TDwarfCompilationUnit);
    procedure Decode;
  end;
  
  { TVerboseDwarfCallframeDecoder }

  TVerboseDwarfCallframeDecoder = class(TObject)
  private
    FLoader: TDbgImageLoader;
    procedure InternalDecode(AData: Pointer; ASize, AStart: QWord);
  protected
  public
    constructor Create(ALoader: TDbgImageLoader);
    procedure Decode;
  end;
  
  
  { TDbgDwarfProcSymbol }

  TDbgDwarfProcSymbol = class(TDbgSymbol)
  private
    FCU: TDwarfCompilationUnit;
    FAddress: TDbgPtr;
    FAddressInfo: PDwarfAddressInfo;
    FStateMachine: TDwarfLineInfoStateMachine;
    function StateMachineValid: Boolean;
  protected
    function GetChild(AIndex: Integer): TDbgSymbol; override;
    function GetColumn: Cardinal; override;
    function GetCount: Integer; override;
    function GetFile: String; override;
//    function GetFlags: TDbgSymbolFlags; override;
    function GetLine: Cardinal; override;
    function GetParent: TDbgSymbol; override;
//    function GetReference: TDbgSymbol; override;
    function GetSize: Integer; override;
  public
    constructor Create(ACompilationUnit: TDwarfCompilationUnit; AInfo: PDwarfAddressInfo; AAddress: TDbgPtr);
    destructor Destroy; override;
  end;


  
type
  TDwarfSection = (dsAbbrev, dsARanges, dsFrame,  dsInfo, dsLine, dsLoc, dsMacinfo, dsPubNames, dsPubTypes, dsRanges, dsStr);

  TDwarfSectionInfo = record
    Section: TDwarfSection;
    VirtualAdress: QWord;
    Size: QWord; // the virtual size
    RawData: Pointer;
  end;
  PDwarfSectionInfo = ^TDwarfSectionInfo;

const
  DWARF_SECTION_NAME: array[TDwarfSection] of String = (
    '.debug_abbrev', '.debug_aranges', '.debug_frame', '.debug_info',
    '.debug_line', '.debug_loc', '.debug_macinfo', '.debug_pubnames',
    '.debug_pubtypes', '.debug_ranges', '.debug_str'
  );
  
  { TDbgDwarf }

type
  TDbgDwarf = class(TDbgInfo)
  private
    FCompilationUnits: TList;
    FImageBase: QWord;
    FSections: array[TDwarfSection] of TDwarfSectionInfo;
    function GetCompilationUnit(AIndex: Integer): TDwarfCompilationUnit;
  protected
    function GetCompilationUnitClass: TDwarfCompilationUnitClass; virtual;
  public
    constructor Create(ALoader: TDbgImageLoader); override;
    destructor Destroy; override;
    function FindSymbol(AAddress: TDbgPtr): TDbgSymbol; override;
    function GetLineAddress(const AFileName: String; ALine: Cardinal): TDbgPtr; override;
    function LoadCompilationUnits: Integer;
    function PointerFromRVA(ARVA: QWord): Pointer;
    function PointerFromVA(ASection: TDwarfSection; AVA: QWord): Pointer;
    property CompilationUnits[AIndex: Integer]: TDwarfCompilationUnit read GetCompilationUnit;
  end;

  { TDbgVerboseDwarf }

  TDbgVerboseDwarf = class(TDbgDwarf)
  private
  protected
    function GetCompilationUnitClass: TDwarfCompilationUnitClass; override;
  public
  end;
  

function DwarfTagToString(AValue: Integer): String;
function DwarfAttributeToString(AValue: Integer): String;
function DwarfAttributeFormToString(AValue: Integer): String;

function ULEB128toOrdinal(var p: PByte): QWord;
function SLEB128toOrdinal(var p: PByte): Int64;

implementation

function ULEB128toOrdinal(var p: PByte): QWord;
var
  n: Byte;
  Stop: Boolean;
begin
  Result := 0;
  n := 0;
  repeat
    Stop := (p^ and $80) = 0;
    Result := Result + (p^ and $7F) shl n;
    Inc(n, 7);
    Inc(p);
  until Stop or (n > 128);
end;

function SLEB128toOrdinal(var p: PByte): Int64;
var
  n: Byte;
  Stop: Boolean;
begin
  Result := 0;
  n := 0;
  repeat
    Stop := (p^ and $80) = 0;
    Result := Result + (p^ and $7F) shl n;
    Inc(n, 7);
    Inc(p);
  until Stop or (n > 128);

  // sign extend when msbit = 1
  if (p[-1] and $40) <> 0
  then Result := Result or (Int64(-1) shl n);
end;


function DwarfTagToString(AValue: Integer): String;
begin
  case AValue of
    DW_TAG_array_type              : Result := 'DW_TAG_array_type';
    DW_TAG_class_type              : Result := 'DW_TAG_class_type';
    DW_TAG_entry_point             : Result := 'DW_TAG_entry_point';
    DW_TAG_enumeration_type        : Result := 'DW_TAG_enumeration_type';
    DW_TAG_formal_parameter        : Result := 'DW_TAG_formal_parameter';
    DW_TAG_imported_declaration    : Result := 'DW_TAG_imported_declaration';
    DW_TAG_label                   : Result := 'DW_TAG_label';
    DW_TAG_lexical_block           : Result := 'DW_TAG_lexical_block';
    DW_TAG_member                  : Result := 'DW_TAG_member';
    DW_TAG_pointer_type            : Result := 'DW_TAG_pointer_type';
    DW_TAG_reference_type          : Result := 'DW_TAG_reference_type';
    DW_TAG_compile_unit            : Result := 'DW_TAG_compile_unit';
    DW_TAG_string_type             : Result := 'DW_TAG_string_type';
    DW_TAG_structure_type          : Result := 'DW_TAG_structure_type';
    DW_TAG_subroutine_type         : Result := 'DW_TAG_subroutine_type';
    DW_TAG_typedef                 : Result := 'DW_TAG_typedef';
    DW_TAG_union_type              : Result := 'DW_TAG_union_type';
    DW_TAG_unspecified_parameters  : Result := 'DW_TAG_unspecified_parameters';
    DW_TAG_variant                 : Result := 'DW_TAG_variant';
    DW_TAG_common_block            : Result := 'DW_TAG_common_block';
    DW_TAG_common_inclusion        : Result := 'DW_TAG_common_inclusion';
    DW_TAG_inheritance             : Result := 'DW_TAG_inheritance';
    DW_TAG_inlined_subroutine      : Result := 'DW_TAG_inlined_subroutine';
    DW_TAG_module                  : Result := 'DW_TAG_module';
    DW_TAG_ptr_to_member_type      : Result := 'DW_TAG_ptr_to_member_type';
    DW_TAG_set_type                : Result := 'DW_TAG_set_type';
    DW_TAG_subrange_type           : Result := 'DW_TAG_subrange_type';
    DW_TAG_with_stmt               : Result := 'DW_TAG_with_stmt';
    DW_TAG_access_declaration      : Result := 'DW_TAG_access_declaration';
    DW_TAG_base_type               : Result := 'DW_TAG_base_type';
    DW_TAG_catch_block             : Result := 'DW_TAG_catch_block';
    DW_TAG_const_type              : Result := 'DW_TAG_const_type';
    DW_TAG_constant                : Result := 'DW_TAG_constant';
    DW_TAG_enumerator              : Result := 'DW_TAG_enumerator';
    DW_TAG_file_type               : Result := 'DW_TAG_file_type';
    DW_TAG_friend                  : Result := 'DW_TAG_friend';
    DW_TAG_namelist                : Result := 'DW_TAG_namelist';
    DW_TAG_namelist_item           : Result := 'DW_TAG_namelist_item';
    DW_TAG_packed_type             : Result := 'DW_TAG_packed_type';
    DW_TAG_subprogram              : Result := 'DW_TAG_subprogram';
    DW_TAG_template_type_parameter : Result := 'DW_TAG_template_type_parameter';
    DW_TAG_template_value_parameter: Result := 'DW_TAG_template_value_parameter';
    DW_TAG_thrown_type             : Result := 'DW_TAG_thrown_type';
    DW_TAG_try_block               : Result := 'DW_TAG_try_block';
    DW_TAG_variant_part            : Result := 'DW_TAG_variant_part';
    DW_TAG_variable                : Result := 'DW_TAG_variable';
    DW_TAG_volatile_type           : Result := 'DW_TAG_volatile_type';
    DW_TAG_dwarf_procedure         : Result := 'DW_TAG_dwarf_procedure';
    DW_TAG_restrict_type           : Result := 'DW_TAG_restrict_type';
    DW_TAG_interface_type          : Result := 'DW_TAG_interface_type';
    DW_TAG_namespace               : Result := 'DW_TAG_namespace';
    DW_TAG_imported_module         : Result := 'DW_TAG_imported_module';
    DW_TAG_unspecified_type        : Result := 'DW_TAG_unspecified_type';
    DW_TAG_partial_unit            : Result := 'DW_TAG_partial_unit';
    DW_TAG_imported_unit           : Result := 'DW_TAG_imported_unit';
    DW_TAG_condition               : Result := 'DW_TAG_condition';
    DW_TAG_shared_type             : Result := 'DW_TAG_shared_type';
    DW_TAG_lo_user                 : Result := 'DW_TAG_lo_user';
    DW_TAG_hi_user                 : Result := 'DW_TAG_hi_user';
  else
    Result := Format('DW_TAG_%d', [AValue]);
  end;
end;

function DwarfChildrenToString(AValue: Integer): String;
begin
  case AValue of
    DW_CHILDREN_no  : Result := 'DW_CHILDREN_no';
    DW_CHILDREN_yes : Result := 'DW_CHILDREN_yes';
  else
    Result := Format('DW_CHILDREN_%d', [AValue]);
  end;
end;


function DwarfAttributeToString(AValue: Integer): String;
begin
  case AValue of
    DW_AT_sibling             : Result := 'DW_AT_sibling';
    DW_AT_location            : Result := 'DW_AT_location';
    DW_AT_name                : Result := 'DW_AT_name';
    DW_AT_ordering            : Result := 'DW_AT_ordering';
    DW_AT_byte_size           : Result := 'DW_AT_byte_size';
    DW_AT_bit_offset          : Result := 'DW_AT_bit_offset';
    DW_AT_bit_size            : Result := 'DW_AT_bit_size';
    DW_AT_stmt_list           : Result := 'DW_AT_stmt_list';
    DW_AT_low_pc              : Result := 'DW_AT_low_pc';
    DW_AT_high_pc             : Result := 'DW_AT_high_pc';
    DW_AT_language            : Result := 'DW_AT_language';
    DW_AT_discr               : Result := 'DW_AT_discr';
    DW_AT_discr_value         : Result := 'DW_AT_discr_value';
    DW_AT_visibility          : Result := 'DW_AT_visibility';
    DW_AT_import              : Result := 'DW_AT_import';
    DW_AT_string_length       : Result := 'DW_AT_string_length';
    DW_AT_common_reference    : Result := 'DW_AT_common_reference';
    DW_AT_comp_dir            : Result := 'DW_AT_comp_dir';
    DW_AT_const_value         : Result := 'DW_AT_const_value';
    DW_AT_containing_type     : Result := 'DW_AT_containing_type';
    DW_AT_default_value       : Result := 'DW_AT_default_value';
    DW_AT_inline              : Result := 'DW_AT_inline';
    DW_AT_is_optional         : Result := 'DW_AT_is_optional';
    DW_AT_lower_bound         : Result := 'DW_AT_lower_bound';
    DW_AT_producer            : Result := 'DW_AT_producer';
    DW_AT_prototyped          : Result := 'DW_AT_prototyped';
    DW_AT_return_addr         : Result := 'DW_AT_return_addr';
    DW_AT_start_scope         : Result := 'DW_AT_start_scope';
    DW_AT_bit_stride          : Result := 'DW_AT_bit_stride';
    DW_AT_upper_bound         : Result := 'DW_AT_upper_bound';
    DW_AT_abstract_origin     : Result := 'DW_AT_abstract_origin';
    DW_AT_accessibility       : Result := 'DW_AT_accessibility';
    DW_AT_address_class       : Result := 'DW_AT_address_class';
    DW_AT_artificial          : Result := 'DW_AT_artificial';
    DW_AT_base_types          : Result := 'DW_AT_base_types';
    DW_AT_calling_convention  : Result := 'DW_AT_calling_convention';
    DW_AT_count               : Result := 'DW_AT_count';
    DW_AT_data_member_location: Result := 'DW_AT_data_member_location';
    DW_AT_decl_column         : Result := 'DW_AT_decl_column';
    DW_AT_decl_file           : Result := 'DW_AT_decl_file';
    DW_AT_decl_line           : Result := 'DW_AT_decl_line';
    DW_AT_declaration         : Result := 'DW_AT_declaration';
    DW_AT_discr_list          : Result := 'DW_AT_discr_list';
    DW_AT_encoding            : Result := 'DW_AT_encoding';
    DW_AT_external            : Result := 'DW_AT_external';
    DW_AT_frame_base          : Result := 'DW_AT_frame_base';
    DW_AT_friend              : Result := 'DW_AT_friend';
    DW_AT_identifier_case     : Result := 'DW_AT_identifier_case';
    DW_AT_macro_info          : Result := 'DW_AT_macro_info';
    DW_AT_namelist_item       : Result := 'DW_AT_namelist_item';
    DW_AT_priority            : Result := 'DW_AT_priority';
    DW_AT_segment             : Result := 'DW_AT_segment';
    DW_AT_specification       : Result := 'DW_AT_specification';
    DW_AT_static_link         : Result := 'DW_AT_static_link';
    DW_AT_type                : Result := 'DW_AT_type';
    DW_AT_use_location        : Result := 'DW_AT_use_location';
    DW_AT_variable_parameter  : Result := 'DW_AT_variable_parameter';
    DW_AT_virtuality          : Result := 'DW_AT_virtuality';
    DW_AT_vtable_elem_location: Result := 'DW_AT_vtable_elem_location';
    DW_AT_allocated           : Result := 'DW_AT_allocated';
    DW_AT_associated          : Result := 'DW_AT_associated';
    DW_AT_data_location       : Result := 'DW_AT_data_location';
    DW_AT_byte_stride         : Result := 'DW_AT_byte_stride';
    DW_AT_entry_pc            : Result := 'DW_AT_entry_pc';
    DW_AT_use_UTF8            : Result := 'DW_AT_use_UTF8';
    DW_AT_extension           : Result := 'DW_AT_extension';
    DW_AT_ranges              : Result := 'DW_AT_ranges';
    DW_AT_trampoline          : Result := 'DW_AT_trampoline';
    DW_AT_call_column         : Result := 'DW_AT_call_column';
    DW_AT_call_file           : Result := 'DW_AT_call_file';
    DW_AT_call_line           : Result := 'DW_AT_call_line';
    DW_AT_description         : Result := 'DW_AT_description';
    DW_AT_binary_scale        : Result := 'DW_AT_binary_scale';
    DW_AT_decimal_scale       : Result := 'DW_AT_decimal_scale';
    DW_AT_small               : Result := 'DW_AT_small';
    DW_AT_decimal_sign        : Result := 'DW_AT_decimal_sign';
    DW_AT_digit_count         : Result := 'DW_AT_digit_count';
    DW_AT_picture_string      : Result := 'DW_AT_picture_string';
    DW_AT_mutable             : Result := 'DW_AT_mutable';
    DW_AT_threads_scaled      : Result := 'DW_AT_threads_scaled';
    DW_AT_explicit            : Result := 'DW_AT_explicit';
    DW_AT_object_pointer      : Result := 'DW_AT_object_pointer';
    DW_AT_endianity           : Result := 'DW_AT_endianity';
    DW_AT_elemental           : Result := 'DW_AT_elemental';
    DW_AT_pure                : Result := 'DW_AT_pure';
    DW_AT_recursive           : Result := 'DW_AT_recursive';
    DW_AT_lo_user             : Result := 'DW_AT_lo_user';
    DW_AT_hi_user             : Result := 'DW_AT_hi_user';
  else
    Result := Format('DW_AT_%d', [AValue]);
  end;
end;

function DwarfAttributeFormToString(AValue: Integer): String;
begin
  case AValue of
    DW_FORM_addr     : Result := 'DW_FORM_addr';
    DW_FORM_block2   : Result := 'DW_FORM_block2';
    DW_FORM_block4   : Result := 'DW_FORM_block4';
    DW_FORM_data2    : Result := 'DW_FORM_data2';
    DW_FORM_data4    : Result := 'DW_FORM_data4';
    DW_FORM_data8    : Result := 'DW_FORM_data8';
    DW_FORM_string   : Result := 'DW_FORM_string';
    DW_FORM_block    : Result := 'DW_FORM_block';
    DW_FORM_block1   : Result := 'DW_FORM_block1';
    DW_FORM_data1    : Result := 'DW_FORM_data1';
    DW_FORM_flag     : Result := 'DW_FORM_flag';
    DW_FORM_sdata    : Result := 'DW_FORM_sdata';
    DW_FORM_strp     : Result := 'DW_FORM_strp';
    DW_FORM_udata    : Result := 'DW_FORM_udata';
    DW_FORM_ref_addr : Result := 'DW_FORM_ref_addr';
    DW_FORM_ref1     : Result := 'DW_FORM_ref1';
    DW_FORM_ref2     : Result := 'DW_FORM_ref2';
    DW_FORM_ref4     : Result := 'DW_FORM_ref4';
    DW_FORM_ref8     : Result := 'DW_FORM_ref8';
    DW_FORM_ref_udata: Result := 'DW_FORM_ref_udata';
    DW_FORM_indirect : Result := 'DW_FORM_indirect';
  else
    Result := Format('DW_FORM_%d', [AValue]);
  end;
end;

function DwarfLanguageToString(AValue: Integer): String;
begin
  case AValue of
    DW_LANG_C89              : Result := 'DW_LANG_C89';
    DW_LANG_C                : Result := 'DW_LANG_C';
    DW_LANG_Ada83            : Result := 'DW_LANG_Ada83 (reserved)';
    DW_LANG_C_plus_plus      : Result := 'DW_LANG_C_plus_plus';
    DW_LANG_Cobol74          : Result := 'DW_LANG_Cobol74 (reserved)';
    DW_LANG_Cobol85          : Result := 'DW_LANG_Cobol85 (reserved)';
    DW_LANG_Fortran77        : Result := 'DW_LANG_Fortran77';
    DW_LANG_Fortran90        : Result := 'DW_LANG_Fortran90';
    DW_LANG_Pascal83         : Result := 'DW_LANG_Pascal83';
    DW_LANG_Modula2          : Result := 'DW_LANG_Modula2';
    DW_LANG_Java             : Result := 'DW_LANG_Java';
    DW_LANG_C99              : Result := 'DW_LANG_C99';
    DW_LANG_Ada95            : Result := 'DW_LANG_Ada95 (reserved)';
    DW_LANG_Fortran95        : Result := 'DW_LANG_Fortran95';
    DW_LANG_PLI              : Result := 'DW_LANG_PLI (reserved)';
    DW_LANG_ObjC             : Result := 'DW_LANG_ObjC';
    DW_LANG_ObjC_plus_plus   : Result := 'DW_LANG_ObjC_plus_plus';
    DW_LANG_UPC              : Result := 'DW_LANG_UPC';
    DW_LANG_D                : Result := 'DW_LANG_D';
    DW_LANG_lo_user..DW_LANG_hi_user: Result := Format('DW_LANG_user_%d', [AValue]);
  else
    Result := Format('DW_LANG_%d', [AValue]);
  end;
end;


function DwarfBaseTypeEncodingToString(AValue: Integer): String;
begin
  case AValue of
    DW_ATE_address           : Result := 'DW_ATE_address';
    DW_ATE_boolean           : Result := 'DW_ATE_boolean';
    DW_ATE_complex_float     : Result := 'DW_ATE_complex_float';
    DW_ATE_float             : Result := 'DW_ATE_float';
    DW_ATE_signed            : Result := 'DW_ATE_signed';
    DW_ATE_signed_char       : Result := 'DW_ATE_signed_char';
    DW_ATE_unsigned          : Result := 'DW_ATE_unsigned';
    DW_ATE_unsigned_char     : Result := 'DW_ATE_unsigned_char';
    DW_ATE_imaginary_float   : Result := 'DW_ATE_imaginary_float';
    DW_ATE_packed_decimal    : Result := 'DW_ATE_packed_decimal';
    DW_ATE_numeric_string    : Result := 'DW_ATE_numeric_string';
    DW_ATE_edited            : Result := 'DW_ATE_edited';
    DW_ATE_signed_fixed      : Result := 'DW_ATE_signed_fixed';
    DW_ATE_unsigned_fixed    : Result := 'DW_ATE_unsigned_fixed';
    DW_ATE_decimal_float     : Result := 'DW_ATE_decimal_float';
    DW_ATE_lo_user..DW_ATE_hi_user : Result := Format('DW_ATE_user_%d', [AValue]);
  else
    Result := Format('DW_ATE_%d', [AValue]);
  end;
end;

function DwarfAccessibilityToString(AValue: Integer): String;
begin
  case AValue of
    DW_ACCESS_public    : Result := 'DW_ACCESS_public';
    DW_ACCESS_protected : Result := 'DW_ACCESS_protected';
    DW_ACCESS_private   : Result := 'DW_ACCESS_private';
  else
    Result := Format('DW_ACCESS_%d', [AValue]);
  end;
end;


function DwarfVisibilityToString(AValue: Integer): String;
begin
  case AValue of
    DW_VIS_local     : Result := 'DW_VIS_local';
    DW_VIS_exported  : Result := 'DW_VIS_exported';
    DW_VIS_qualified : Result := 'DW_VIS_qualified';
  else
    Result := Format('DW_FORM_%d', [AValue]);
  end;
end;


function DwarfVirtualityToString(AValue: Integer): String;
begin
  case AValue of
    DW_VIRTUALITY_none         : Result := 'DW_VIRTUALITY_none';
    DW_VIRTUALITY_virtual      : Result := 'DW_VIRTUALITY_virtual';
    DW_VIRTUALITY_pure_virtual : Result := 'DW_VIRTUALITY_pure_virtual';
  else
    Result := Format('DW_VIRTUALITY_%d', [AValue]);
  end;
end;

  { Identifier case encodings }

function DwarfIdentifierCaseToString(AValue: Integer): String;
begin
  case AValue of
    DW_ID_case_sensitive   : Result := 'DW_ID_case_sensitive';
    DW_ID_up_case          : Result := 'DW_ID_up_case';
    DW_ID_down_case        : Result := 'DW_ID_down_case';
    DW_ID_case_insensitive : Result := 'DW_ID_case_insensitive';
  else
    Result := Format('DW_ID_%d', [AValue]);
  end;
end;


{ TDbgDwarfSymbol }

constructor TDbgDwarfProcSymbol.Create(ACompilationUnit: TDwarfCompilationUnit; AInfo: PDwarfAddressInfo; AAddress: TDbgPtr);
begin
  FAddress := AAddress;
  FAddressInfo := AInfo;
  
  FCU := ACompilationUnit;

  inherited Create(
    String(FAddressInfo^.Name),
    skProcedure, //todo: skFunction
    FAddressInfo^.StartPC
  );

//BuildLineInfo(
    
//   AFile: String = ''; ALine: Integer = -1; AFlags: TDbgSymbolFlags = []; const AReference: TDbgSymbol = nil);


end;

destructor TDbgDwarfProcSymbol.Destroy;
begin
  FreeAndNil(FStateMachine);
  inherited Destroy;
end;

function TDbgDwarfProcSymbol.GetChild(AIndex: Integer): TDbgSymbol;
begin
  Result:=inherited GetChild(AIndex);
end;

function TDbgDwarfProcSymbol.GetColumn: Cardinal;
begin
  if StateMachineValid
  then Result := FStateMachine.Column
  else Result := inherited GetColumn;
end;

function TDbgDwarfProcSymbol.GetCount: Integer;
begin
  Result:=inherited GetCount;
end;

function TDbgDwarfProcSymbol.GetFile: String;
begin
  if StateMachineValid
  then Result := FStateMachine.FileName
  else Result := inherited GetFile;
end;

function TDbgDwarfProcSymbol.GetLine: Cardinal;
begin
  if StateMachineValid
  then Result := FStateMachine.Line
  else Result := inherited GetLine;
end;

function TDbgDwarfProcSymbol.GetParent: TDbgSymbol;
begin
  Result:=inherited GetParent;
end;

function TDbgDwarfProcSymbol.GetSize: Integer;
begin
  Result := FAddressInfo^.EndPC - FAddressInfo^.StartPC;
end;

function TDbgDwarfProcSymbol.StateMachineValid: Boolean;
var
  SM1, SM2: TDwarfLineInfoStateMachine;
begin
  Result := FStateMachine <> nil;
  if Result then Exit;

  if FAddressInfo^.StateMachine = nil
  then begin
    FCU.BuildLineInfo(FAddressInfo, False);
    if FAddressInfo^.StateMachine = nil then Exit;
  end;

  // we cannot restore a statemachine to its current state
  // so we shouldn't modify FAddressInfo^.StateMachine
  // so use clones to navigate
  SM1 := FAddressInfo^.StateMachine.Clone;
  if FAddress < SM1.Address
  then begin
    // The address we want to find is before the start of this symbol ??
    SM1.Free;
    Exit;
  end;
  SM2 := FAddressInfo^.StateMachine.Clone;

  repeat
    if (FAddress = SM1.Address)
    or not SM2.NextLine
    or (FAddress < SM2.Address)
    then begin
      // found
      FStateMachine := SM1;
      SM2.Free;
      Result := True;
      Exit;
    end;
  until not SM1.NextLine;
  
  //if all went well we shouldn't come here
  SM1.Free;
  SM2.Free;
end;

{ TDbgDwarf }

constructor TDbgDwarf.Create(ALoader: TDbgImageLoader);
var
  Section: TDwarfSection;
  p: PDbgImageSection;
begin
  inherited Create(ALoader);
  FCompilationUnits := TList.Create;
  FImageBase := ALoader.ImageBase;
  for Section := Low(Section) to High(Section) do
  begin
    p := ALoader.Section[DWARF_SECTION_NAME[Section]];
    if p = nil then Continue;
    FSections[Section].Section := Section;
    FSections[Section].RawData := p^.RawData;
    FSections[Section].Size := p^.Size;
    FSections[Section].VirtualAdress := p^.VirtualAdress;
  end;
end;

destructor TDbgDwarf.Destroy;
var
  n: integer;
begin
  for n := 0 to FCompilationUnits.Count - 1 do
    TObject(FCompilationUnits[n]).Free;
  FreeAndNil(FCompilationUnits);
  inherited Destroy;
end;

function TDbgDwarf.FindSymbol(AAddress: TDbgPtr): TDbgSymbol;
var
  n: Integer;
  CU: TDwarfCompilationUnit;
  Iter: TMapIterator;
  Info: PDwarfAddressInfo;
  MinMaxSet: boolean;
begin
  Result := nil;
  for n := 0 to FCompilationUnits.Count - 1 do
  begin
    CU := TDwarfCompilationUnit(FCompilationUnits[n]);
    if not CU.Valid then Continue;
    MinMaxSet := CU.FMinPC <> CU.FMaxPC;
    if MinMaxSet and ((AAddress < CU.FMinPC) or (AAddress > CU.FMaxPC))
    then Continue;
    
    CU.BuildAddressMap;

    Iter := TMapIterator.Create(CU.FAddressMap);
    try
      if Iter.EOM
      then begin
        if MinMaxSet
        then Exit //  minmaxset and no procs defined ???
        else Continue;
      end;

      if not Iter.Locate(AAddress)
      then begin
        if not Iter.BOM
        then Iter.Previous;

        if Iter.BOM
        then begin
          if MinMaxSet
          then Exit //  minmaxset and no proc @ minpc ???
          else Continue;
        end;
      end;
      
      // iter is at the closest defined adress before AAddress
      Info := Iter.DataPtr;
      if AAddress > Info^.EndPC
      then begin
        if MinMaxSet
        then Exit //  minmaxset and no proc @ maxpc ???
        else Continue;
      end;
      
      Result := TDbgDwarfProcSymbol.Create(CU, Iter.DataPtr, AAddress);
    finally
      Iter.Free;
    end;
  end;
end;

function TDbgDwarf.GetCompilationUnit(AIndex: Integer): TDwarfCompilationUnit;
begin
  Result := TDwarfCompilationUnit(FCompilationUnits[Aindex]);
end;

function TDbgDwarf.GetCompilationUnitClass: TDwarfCompilationUnitClass;
begin
  Result := TDwarfCompilationUnit;
end;

function TDbgDwarf.GetLineAddress(const AFileName: String; ALine: Cardinal): TDbgPtr;
var
  n: Integer;
  CU: TDwarfCompilationUnit;
begin
  for n := 0 to FCompilationUnits.Count - 1 do
  begin
    CU := TDwarfCompilationUnit(FCompilationUnits[n]);
    Result := CU.GetLineAddress(AFileName, ALine);
    if Result <> 0 then Exit;
  end;
  Result := 0;
end;

function TDbgDwarf.LoadCompilationUnits: Integer;
var
  p: Pointer;
  CU32: PDwarfCUHeader32 absolute p;
  CU64: PDwarfCUHeader64 absolute p;
  CU: TDwarfCompilationUnit;
  CUClass: TDwarfCompilationUnitClass;
begin
  CUClass := GetCompilationUnitClass;
  p := FSections[dsInfo].RawData;
  while p <> nil do
  begin
    if CU64^.Signature = DWARF_HEADER64_SIGNATURE
    then begin
      CU := CUClass.Create(
              Self,
              PtrUInt(CU64 + 1) - PtrUInt(FSections[dsInfo].RawData),
              CU64^.Length - SizeOf(CU64^) + SizeOf(CU64^.Signature) + SizeOf(CU64^.Length),
              CU64^.Version,
              CU64^.AbbrevOffset,
              CU64^.AddressSize,
              True);
      p := Pointer(@CU64^.Version) + CU64^.Length;
    end
    else begin
      if CU32^.Length = 0 then Break;
      CU := CUClass.Create(
              Self,
              PtrUInt(CU32 + 1) - PtrUInt(FSections[dsInfo].RawData),
              CU32^.Length - SizeOf(CU32^) + SizeOf(CU32^.Length),
              CU32^.Version,
              CU32^.AbbrevOffset,
              CU32^.AddressSize,
              False);
      p := Pointer(@CU32^.Version) + CU32^.Length;
    end;
    FCompilationUnits.Add(CU);
    if CU.Valid then SetHasInfo;
  end;
  Result := FCompilationUnits.Count;
end;

function TDbgDwarf.PointerFromRVA(ARVA: QWord): Pointer;
begin
  Result := Pointer(PtrUInt(FImageBase + ARVA));
end;

function TDbgDwarf.PointerFromVA(ASection: TDwarfSection; AVA: QWord): Pointer;
begin
  Result := FSections[ASection].RawData + AVA - FImageBase - FSections[ASection].VirtualAdress;
end;

{ TDbgVerboseDwarf }

function TDbgVerboseDwarf.GetCompilationUnitClass: TDwarfCompilationUnitClass;
begin
  Result:= TDwarfVerboseCompilationUnit;
end;

{ TDwarfScopeInfo }

constructor TDwarfScopeInfo.Create(AEntry: Pointer);
begin
  inherited Create;
  FEntry := AEntry;
end;

destructor TDwarfScopeInfo.Destroy;
begin
  if (FParent <> nil) and (FParent.FChild = Self)
  then FParent.FChild := FNext;

  if FPrev <> nil
  then FPrev.FNext := FNext;
  
  if FNext <> nil
  then FNext.FPrev := FPrev;
  
  inherited Destroy;
end;

procedure TDwarfScopeInfo.SetChild(const AValue: TDwarfScopeInfo);
begin
  FChild := AValue;
  FChildValid := True;
  if FChild = nil then Exit;
  FChild.FParent := Self;
end;

procedure TDwarfScopeInfo.SetNext(const AValue: TDwarfScopeInfo);
begin
  FNext := AValue;
  if FNext = nil then Exit;
  FNext.FPrev := Self;
  FNext.FParent := FParent;
end;

{ TDwarfLineInfoStateMachine }

function TDwarfLineInfoStateMachine.Clone: TDwarfLineInfoStateMachine;
begin
  Result := TDwarfLineInfoStateMachine.Create(FOwner, FLineInfoPtr, FMaxPtr);
  Result.FAddress := FAddress;
  Result.FFileName := FFileName;
  Result.FLine := FLine;
  Result.FColumn := FColumn;
  Result.FIsStmt := FIsStmt;
  Result.FBasicBlock := FBasicBlock;
  Result.FEndSequence := FEndSequence;
  Result.FPrologueEnd := FPrologueEnd;
  Result.FEpilogueBegin := FEpilogueBegin;
  Result.FIsa := FIsa;
  Result.FEnded := FEnded;
end;

constructor TDwarfLineInfoStateMachine.Create(AOwner: TDwarfCompilationUnit; ALineInfoPtr, AMaxPtr: Pointer);
begin
  inherited Create;
  FOwner := AOwner;
  FLineInfoPtr := ALineInfoPtr;
  FMaxPtr := AMaxPtr;
  Reset;
end;

function TDwarfLineInfoStateMachine.NextLine: Boolean;
var
  pb: PByte absolute FLineInfoPtr;
  p: Pointer;
  Opcode: Byte;
  instrlen: Cardinal;
  diridx: Cardinal;
begin
  Result := False;
  if FEndSequence
  then begin
    Reset;
  end
  else begin
    FBasicBlock := False;
    FPrologueEnd := False;
    FEpilogueBegin := False;
  end;
  
  while pb <= FMaxPtr do
  begin
    Opcode := pb^;
    Inc(pb);
    if Opcode <= Length(FOwner.FLineInfo.StandardOpcodeLengths)
    then begin
      // Standard opcode
      case Opcode of
        DW_LNS_copy: begin
          Result := True;
          Exit;
        end;
        DW_LNS_advance_pc: begin
          Inc(FAddress, ULEB128toOrdinal(pb));
        end;
        DW_LNS_advance_line: begin
          Inc(FLine, SLEB128toOrdinal(pb));
        end;
        DW_LNS_set_file: begin
          SetFileName(ULEB128toOrdinal(pb));
        end;
        DW_LNS_set_column: begin
          FColumn := ULEB128toOrdinal(pb);
        end;
        DW_LNS_negate_stmt: begin
          FIsStmt := not FIsStmt;
        end;
        DW_LNS_set_basic_block: begin
          FBasicBlock := True;
        end;
        DW_LNS_const_add_pc: begin
          Opcode := 255 - Length(FOwner.FLineInfo.StandardOpcodeLengths);
          if FOwner.FLineInfo.LineRange = 0
          then Inc(FAddress, Opcode * FOwner.FLineInfo.MinimumInstructionLength)
          else Inc(FAddress, (Opcode div FOwner.FLineInfo.LineRange) * FOwner.FLineInfo.MinimumInstructionLength);
        end;
        DW_LNS_fixed_advance_pc: begin
          Inc(FAddress, PWord(pb)^);
          Inc(pb, 2);
        end;
        DW_LNS_set_prologue_end: begin
          FPrologueEnd := True;
        end;
        DW_LNS_set_epilogue_begin: begin
          FEpilogueBegin := True;
        end;
        DW_LNS_set_isa: begin
          FIsa := ULEB128toOrdinal(pb);
        end;
        // Extended opcode
        DW_LNS_extended_opcode: begin
          instrlen := ULEB128toOrdinal(pb); // instruction length

          case pb^ of
            DW_LNE_end_sequence: begin
              FEndSequence := True;
              Result := True;
              Exit;
            end;
            DW_LNE_set_address: begin
              if FOwner.FLineInfo.Addr64
              then FAddress := PQWord(pb+1)^
              else FAddress := PLongWord(pb+1)^;
            end;
            DW_LNE_define_file: begin
              // don't move pb, it's done at the end by instruction length
              p := pb;
              FFileName := String(PChar(p));
              Inc(p, Length(FFileName) + 1);

              //diridx
              diridx := ULEB128toOrdinal(p);
              if diridx < FOwner.FLineInfo.Directories.Count
              then FFileName := FOwner.FLineInfo.Directories[diridx] + FFileName
              else FFileName := Format('Unknown dir(%u)', [diridx]) + DirectorySeparator + FFileName;
              //last modified
              //ULEB128toOrdinal(p);
              //length
              //ULEB128toOrdinal(p));
            end;
          else
            // unknown extendend opcode
          end;
          Inc(pb, instrlen);
        end;
      else
        // unknown opcode
        Inc(pb, FOwner.FLineInfo.StandardOpcodeLengths[Opcode])
      end;
      Continue;
    end;

    // Special opcode
    Dec(Opcode, Length(FOwner.FLineInfo.StandardOpcodeLengths)+1);
    if FOwner.FLineInfo.LineRange = 0
    then begin
      Inc(FAddress, Opcode * FOwner.FLineInfo.MinimumInstructionLength);
    end
    else begin
      Inc(FAddress, (Opcode div FOwner.FLineInfo.LineRange) * FOwner.FLineInfo.MinimumInstructionLength);
      Inc(FLine, FOwner.FLineInfo.LineBase + (Opcode mod FOwner.FLineInfo.LineRange));
    end;
    Result := True;
    Exit;
  end;
  Result := False;
  FEnded := True;
end;

procedure TDwarfLineInfoStateMachine.Reset;
begin
  FAddress := 0;
  SetFileName(1);
  FLine := 1;
  FColumn := 0;
  FIsStmt := FOwner.FLineInfo.DefaultIsStmt;
  FBasicBlock := False;
  FEndSequence := False;
  FPrologueEnd := False;
  FEpilogueBegin := False;
  FIsa := 0;
end;

procedure TDwarfLineInfoStateMachine.SetFileName(AIndex: Cardinal);
begin
  if (Aindex > 0) and (AIndex <= FOwner.FLineInfo.FileNames.Count)
  then FFileName := FOwner.FLineInfo.FileNames[AIndex - 1]
  else FFileName := Format('Unknown fileindex(%u)', [AIndex]);
end;

{ TDwarfCompilationUnit }

procedure TDwarfCompilationUnit.BuildLineInfo(AAddressInfo: PDwarfAddressInfo; ADoAll: Boolean);
var
  Iter: TMapIterator;
  Info: PDwarfAddressInfo;
  SM: TDwarfLineInfoStateMachine absolute FLineInfo.StateMachine;
  idx: Integer;
  LineMap: TMap;
begin
  if not ADoAll
  then begin
    if AAddressInfo = nil then Exit;
    if AAddressInfo^.StateMachine <> nil then Exit;
  end;
  if SM.Ended then Exit;

  Iter := TMapIterator.Create(FAddressMap);

  while SM.NextLine do
  begin
    idx := FLineNumberMap.IndexOf(SM.FileName);
    if idx = -1
    then begin
      LineMap := TMap.Create(itu4, SizeOf(SM.Address));
      FLineNumberMap.AddObject(SM.FileName, LineMap);
    end
    else begin
      LineMap := TMap(FLineNumberMap.Objects[idx]);
    end;
    if not LineMap.HasId(SM.Line)
    then LineMap.Add(SM.Line, SM.Address);
  
    if Iter.Locate(SM.Address)
    then begin
      // set lineinfo
      Info := Iter.DataPtr;
      if Info^.StateMachine = nil
      then begin
        Info^.StateMachine := SM.Clone;
        FLineInfo.StateMachines.Add(Info^.StateMachine);
      end;
      if not ADoAll and (Info = AAddressInfo)
      then Break;
    end;
  end;
    
  Iter.Free;
end;

procedure TDwarfCompilationUnit.BuildAddressMap;
var
  AttribList: TPointerDynArray;
  Attrib: Pointer;
  Form: Cardinal;
  Info: TDwarfAddressInfo;
  Scope, ResultScope: TDwarfScopeInfo;
begin
  if FAddressMapBuild then Exit;

  Scope := FScope;
  while Scope <> nil do
  begin
    if LocateEntry(DW_TAG_subprogram, Scope, [lefCreateAttribList, lefContinuable, lefSearchChild], ResultScope, AttribList)
    then begin
      Info.Scope := ResultScope;
      if LocateAttribute(ResultScope.Entry, DW_AT_low_pc, AttribList, Attrib, Form)
      then begin
        ReadValue(Attrib, Form, Info.StartPC);

        if LocateAttribute(ResultScope.Entry, DW_AT_high_pc, AttribList, Attrib, Form)
        then ReadValue(Attrib, Form, Info.EndPC)
        else Info.EndPC := Info.StartPC;

        if LocateAttribute(ResultScope.Entry, DW_AT_name, AttribList, Attrib, Form)
        then ReadValue(Attrib, Form, Info.Name)
        else Info.Name := 'undefined';

        Info.StateMachine := nil;
        if Info.StartPC <> 0
        then begin
          if FAddressMap.HasId(Info.StartPC)
          then WriteLN('WARNING duplicate start adress: ', IntToHex(Info.StartPC, FAddressSize * 2))
          else FAddressMap.Add(Info.StartPC, Info);
        end;
      end;

      // TAG found, try continue with the found scope
      Scope := ResultScope.Child;
      if Scope <> nil then Continue;
      Scope := ResultScope;
    end;

    while (Scope.Next = nil) and (Scope.Parent <> nil) do Scope := Scope.Parent;
    Scope := Scope.Next;
  end;

  FAddressMapBuild := True;
end;

constructor TDwarfCompilationUnit.Create(AOwner: TDbgDwarf; ADataOffset: QWord; ALength: QWord; AVersion: Word; AAbbrevOffset: QWord; AAddressSize: Byte; AIsDwarf64: Boolean);
  procedure FillLineInfo(AData: Pointer);
  var
    LNP32: PDwarfLNPHeader32 absolute AData;
    LNP64: PDwarfLNPHeader64 absolute AData;
    Info: PDwarfLNPInfoHeader;

    UnitLength: QWord;
    Version: Word;
    HeaderLength: QWord;
    Name: PChar;
    diridx: Cardinal;
    S: String;
    pb: PByte absolute Name;
  begin
    FLineInfo.Header := AData;
    if LNP64^.Signature = DWARF_HEADER64_SIGNATURE
    then begin
      FLineInfo.Addr64 := True;
      UnitLength := LNP64^.UnitLength;
      FLineInfo.DataEnd := Pointer(@LNP64^.Version) + UnitLength;
      Version := LNP64^.Version;
      HeaderLength := LNP64^.HeaderLength;
      Info := @LNP64^.Info;
    end
    else begin
      FLineInfo.Addr64 := False;
      UnitLength := LNP32^.UnitLength;
      FLineInfo.DataEnd := Pointer(@LNP32^.Version) + UnitLength;
      Version := LNP32^.Version;
      HeaderLength := LNP32^.HeaderLength;
      Info := @LNP32^.Info;
    end;
    FLineInfo.DataStart := PByte(Info) + HeaderLength;


    FLineInfo.MinimumInstructionLength := Info^.MinimumInstructionLength;
    FLineInfo.DefaultIsStmt := Info^.DefaultIsStmt <> 0;
    FLineInfo.LineBase := Info^.LineBase;
    FLineInfo.LineRange := Info^.LineRange;

    // opcodelengths
    SetLength(FLineInfo.StandardOpcodeLengths, Info^.OpcodeBase - 1);
    Move(Info^.StandardOpcodeLengths, FLineInfo.StandardOpcodeLengths[0], Info^.OpcodeBase - 1);

    // directories & filenames
    FLineInfo.Directories := TStringList.Create;
    FLineInfo.Directories.Add(''); // current dir
    Name := @Info^.StandardOpcodeLengths;
    Inc(Name, Info^.OpcodeBase-1);
    // directories
    while Name^ <> #0 do
    begin
      S := String(Name);
      Inc(pb, Length(S)+1);
      FLineInfo.Directories.Add(S + DirectorySeparator);
    end;
    Inc(Name);

    // filenames
    FLineInfo.FileNames := TStringList.Create;
    while Name^ <> #0 do
    begin
      S := String(Name);
      Inc(pb, Length(S)+1);
      //diridx
      diridx := ULEB128toOrdinal(pb);
      if diridx < FLineInfo.Directories.Count
      then S := FLineInfo.Directories[diridx] + S
      else S := Format('Unknown dir(%u)', [diridx]) + DirectorySeparator + S;
      FLineInfo.FileNames.Add(S);
      //last modified
      ULEB128toOrdinal(pb);
      //length
      ULEB128toOrdinal(pb);
    end;

    FLineInfo.StateMachine := TDwarfLineInfoStateMachine.Create(Self, FLineInfo.DataStart, FLineInfo.DataEnd);
    FLineInfo.StateMachines := TFPObjectList.Create(True);

    FLineInfo.Valid := True;
  end;

var
  AttribList: TPointerDynArray;
  Attrib: Pointer;
  Form: Cardinal;
  StatementListOffs, Offs: QWord;
  Scope: TDwarfScopeInfo;
begin
  inherited Create;
  FOwner := AOwner;
  FInfoData := FOwner.FSections[dsInfo].RawData + ADataOffset;
  FLength := ALength;
  FVersion := AVersion;
  FAbbrevOffset := AAbbrevOffset;
  // check for address as offset
  if FAbbrevOffset > FOwner.FSections[dsAbbrev].Size
  then begin
    Offs := FAbbrevOffset - FOwner.FImageBase - FOwner.FSections[dsAbbrev].VirtualAdress;
    if (Offs >= 0) and (Offs < FOwner.FSections[dsAbbrev].Size)
    then begin
      WriteLN('WARNING: Got Abbrev offset as address, adjusting..');
      FAbbrevOffset := Offs;
    end;
  end;

  FAddressSize := AAddressSize;
  FIsDwarf64 := AIsDwarf64;

  FMap := TMap.Create(itu4, SizeOf(TDwarfAbbrev));
  SetLength(FDefinitions, 256);
  // initialize last abbrev with start
//  FLastAbbrevPtr := FOwner.PointerFromVA(dsAbbrev, FAbbrevOffset);
  FLastAbbrevPtr := FOwner.FSections[dsAbbrev].RawData + FAbbrevOffset;

  // use internally 64 bit target pointer
  FAddressMap := TMap.Create(itu8, SizeOf(TDwarfAddressInfo));
  FLineNumberMap := TStringList.Create;
  FLineNumberMap.Sorted := True;
  FLineNumberMap.Duplicates := dupError;
  

  FScope := TDwarfScopeInfo.Create(FInfoData);
  // retrieve some info about this unit
  if not LocateEntry(DW_TAG_compile_unit, FScope, [lefCreateAttribList, lefSearchChild], Scope, AttribList)
  then begin
    WriteLN('WARNING compilation unit has no compile_unit tag');
    Exit;
  end;
  FValid := True;

  if LocateAttribute(Scope.Entry, DW_AT_name, AttribList, Attrib, Form)
  then ReadValue(Attrib, Form, FFileName);
  
  if not LocateAttribute(Scope.Entry, DW_AT_identifier_case, AttribList, Attrib, Form)
  and not ReadValue(Attrib, Form, FIdentifierCase)
  then FIdentifierCase := DW_ID_case_sensitive;
  
  if LocateAttribute(Scope.Entry, DW_AT_stmt_list, AttribList, Attrib, Form)
  and ReadValue(Attrib, Form, StatementListOffs)
  then begin
    // check for address as offset
    if StatementListOffs < FOwner.FSections[dsLine].Size
    then begin
      FillLineInfo(FOwner.FSections[dsLine].RawData + StatementListOffs);
    end
    else begin
      Offs := StatementListOffs - FOwner.FImageBase - FOwner.FSections[dsLine].VirtualAdress;
      if (Offs >= 0) and (Offs < FOwner.FSections[dsLine].Size)
      then begin
        WriteLN('WARNING: Got Lineinfo offset as address, adjusting..');
        FillLineInfo(FOwner.FSections[dsLine].RawData + Offs);
      end;
    end;
  end;

  if LocateAttribute(Scope.Entry, DW_AT_low_pc, AttribList, Attrib, Form)
  then ReadValue(Attrib, Form, FMinPC);

  if LocateAttribute(Scope.Entry, DW_AT_high_pc, AttribList, Attrib, Form)
  then ReadValue(Attrib, Form, FMaxPC);
  
  if FMinPC = 0 then FMinPC := FMaxPC;
  if FMaxPC = 0 then FMAxPC := FMinPC;
end;

destructor TDwarfCompilationUnit.Destroy;
  procedure FreeScope;
  var
    Scope, OldScope: TDwarfScopeInfo;
  begin
    // could have done recursively
    Scope := FScope;
    while Scope <> nil do
    begin
      while Scope.Child <> nil do Scope := Scope.Child;
      OldScope := Scope;
      if Scope.Next = nil
      then Scope := Scope.Parent
      else Scope := Scope.Next;
      OldScope.Free;
    end;
    FScope := nil;
  end;
  
  procedure FreeLineNumberMap;
  var
    n: Integer;
  begin
    for n := 0 to FLineNumberMap.Count - 1 do
      FLineNumberMap.Objects[n].Free;
    FreeAndNil(FLineNumberMap);
  end;

begin
  FreeScope;
  FreeAndNil(FMap);
  FreeAndNil(FAddressMap);
  FreeLineNumberMap;
  FreeAndNil(FLineInfo.StateMachines);
  FreeAndNil(FLineInfo.StateMachine);
  FreeAndNil(FLineInfo.Directories);
  FreeAndNil(FLineInfo.FileNames);

  inherited Destroy;
end;

function TDwarfCompilationUnit.GetDefinition(AAbbrev: Cardinal; out ADefinition: TDwarfAbbrev): Boolean;
begin
  LoadAbbrevs(AAbbrev);
  Result := FMap.GetData(AAbbrev, ADefinition);
end;

function TDwarfCompilationUnit.GetLineAddress(const AFileName: String; ALine: Cardinal): TDbgPtr;
  function FindIndex: Integer;
  var
    Name: String;
  begin
    // try fullname first
    Result := FLineNumberMap.IndexOf(AFileName);
    if Result <> -1 then Exit;
    
    Name := ExtractFileName(AFileName);
    Result := FLineNumberMap.IndexOf(Name);
    if Result <> -1 then Exit;

    Name := UpperCase(Name);
    for Result := 0 to FLineNumberMap.Count - 1 do
    begin
      if Name = UpperCase(ExtractFileName(FLineNumberMap[Result]))
      then Exit;
    end;
    Result := -1
  end;
var
  idx: Integer;
  Map: TMap;
begin
  Result := 0;
  if not Valid then Exit;

  // make sure all filenames are there
  BuildLineInfo(nil, True);
  idx := FindIndex;
  if idx = -1 then Exit;
  
  Map := TMap(FLineNumberMap.Objects[idx]);
  Map.GetData(ALine, Result);
end;

procedure TDwarfCompilationUnit.LoadAbbrevs(ANeeded: Cardinal);
  procedure MakeRoom(AMinSize: Integer);
  var
    len: Integer;
  begin
    len := Length(FDefinitions);
    if len > AMinSize then Exit;
    if len > $4000
    then Inc(len, $4000)
    else len := len * 2;
    SetLength(FDefinitions, len);
  end;
var
  MaxData: Pointer;
  pb: PByte absolute FLastAbbrevPtr;
  pw: PWord absolute FLastAbbrevPtr;
  Def: TDwarfAbbrev;
  abbrev, attrib, form: Cardinal;
  n: Integer;
begin
  if ANeeded <= FLastAbbrev then Exit;

  abbrev := 0;
  // we don't know the number of abbrevs for this unit,
  // but we cannot go beyond the section limit, so use that as safetylimit
  // in case of corrupt data
  MaxData := FOwner.FSections[dsAbbrev].RawData + FOwner.FSections[dsAbbrev].Size;
  while (pb < MaxData) and (pb^ <> 0) and (abbrev < ANeeded) do
  begin
    abbrev := ULEB128toOrdinal(pb);
    Def.tag := ULEB128toOrdinal(pb);

    if FMap.HasId(abbrev)
    then begin
      WriteLN('Duplicate abbrev=', abbrev, ' found. Ignoring....');
      while pw^ <> 0 do Inc(pw);
      Inc(pw);
      abbrev := 0;
      Continue;
    end;

    if FVerbose
    then begin
      WriteLN('  abbrev:  ', abbrev);
      WriteLN('  tag:     ', Def.tag, '=', DwarfTagToString(Def.tag));
      WriteLN('  children:', pb^, '=', DwarfChildrenToString(pb^));
    end;
    Def.Children := pb^ = DW_CHILDREN_yes;
    Inc(pb);

    n := 0;
    Def.Index := FAbbrevIndex;
    while pw^ <> 0 do
    begin
      attrib := ULEB128toOrdinal(pb);
      form := ULEB128toOrdinal(pb);

      MakeRoom(FAbbrevIndex + 1);
      FDefinitions[FAbbrevIndex].Attribute := attrib;
      FDefinitions[FAbbrevIndex].Form := form;
      Inc(FAbbrevIndex);

      if FVerbose
      then WriteLN('   [', n:4, '] attrib: ', attrib, '=', DwarfAttributeToString(attrib), ', form: ', form, '=', DwarfAttributeFormToString(form));
      Inc(n);
    end;
    Def.Count := n;
    FMap.Add(abbrev, Def);

    Inc(pw);
  end;
  if abbrev <> 0
  then FLastAbbrev := abbrev;
end;

function TDwarfCompilationUnit.LocateAttribute(AEntry: Pointer; AAttribute: Cardinal; const AList: TPointerDynArray; out AAttribPtr: Pointer; out AForm: Cardinal): Boolean;
var
  Abbrev: Cardinal;
  Def: TDwarfAbbrev;
  n: Integer;
begin
  Abbrev := ULEB128toOrdinal(AEntry);
  if not GetDefinition(Abbrev, Def)
  then begin
    //???
    WriteLN('Error: Abbrev not found: ', Abbrev);
    Result := False;
    Exit;
  end;
  
  for n := Def.Index to Def.Index + Def.Count - 1 do
  begin
    if FDefinitions[n].Attribute = AAttribute
    then begin
      Result := True;
      AAttribPtr := AList[n - Def.Index];
      AForm := FDefinitions[n].Form;
      Exit;
    end;
  end;
  Result := False;
end;

//----------------------------------------
// Params
//   ATag: a tag to search for
//   AStartScope: a startpoint in the data
//   ABuildList: if set, build the attrib list
//   ACurrentOnly: if set, process only current entry
//   AResultScope: the located scope info
//   AList: an array where pointers to all attribs are stored
//----------------------------------------
function TDwarfCompilationUnit.LocateEntry(ATag: Cardinal; AStartScope: TDwarfScopeInfo; AFlags: TDwarfLocateEntryFlags; out AResultScope: TDwarfScopeInfo; out AList: TPointerDynArray): Boolean;
  procedure SkipLEB(var p: Pointer);
  begin
    while (PByte(p)^ and $80) <> 0 do Inc(p);
    Inc(p);
  end;

  procedure SkipStr(var p: Pointer);
  begin
    while PByte(p)^ <> 0 do Inc(p);
    Inc(p);
  end;
  
  procedure ParseAttribs(const ADef: TDwarfAbbrev; ABuildList: Boolean; var p: Pointer);
  var
    idx: Integer;
    Form: Cardinal;
    UValue: QWord;
  begin
    for idx := ADef.Index to ADef.Index + ADef.Count - 1 do
    begin
      if ABuildList
      then AList[idx - ADef.Index] := p;

      Form := FDefinitions[idx].Form;
      while Form = DW_FORM_indirect do Form := ULEB128toOrdinal(p);

      case Form of
        DW_FORM_addr     : begin
          Inc(p, FAddressSize);
        end;
        DW_FORM_block    : begin
          UValue := ULEB128toOrdinal(p);
          Inc(p, UValue);
        end;
        DW_FORM_block1   : begin
          Inc(p, PByte(p)^ + 1);
        end;
        DW_FORM_block2   : begin
          Inc(p, PWord(p)^ + 2);
        end;
        DW_FORM_block4   : begin
          Inc(p, PLongWord(p)^ + 4);
        end;
        DW_FORM_data1    : begin
          Inc(p, 1);
        end;
        DW_FORM_data2    : begin
          Inc(p, 2);
        end;
        DW_FORM_data4    : begin
          Inc(p, 4);
        end;
        DW_FORM_data8    : begin
          Inc(p, 8);
        end;
        DW_FORM_sdata    : begin
          SkipLEB(p);
        end;
        DW_FORM_udata    : begin
          SkipLEB(p);
        end;
        DW_FORM_flag     : begin
          Inc(p, 1);
        end;
        DW_FORM_ref1     : begin
          Inc(p, 1);
        end;
        DW_FORM_ref2     : begin
          Inc(p, 2);
        end;
        DW_FORM_ref4     : begin
          Inc(p, 4);
        end;
        DW_FORM_ref8     : begin
          Inc(p, 8);
        end;
        DW_FORM_ref_udata: begin
          SkipLEB(p);
        end;
        DW_FORM_ref_addr : begin
          Inc(p, FAddressSize);
        end;
        DW_FORM_string   : begin
          SkipStr(p);
        end;
        DW_FORM_strp     : begin
          Inc(p, FAddressSize);
        end;
        DW_FORM_indirect : begin
        end;
      else
        WriteLN('Error: Unknown Form: ', Form);
        Break;
      end;
    end;
  end;
  
  function CanExit(AResult: Boolean): Boolean;
  begin
    Result := True;
    if AResult
    then begin
      if not (lefContinuable in AFlags) then Exit; // ready, so ok.
      if AResultScope.Child <> nil then Exit; // we have a child so we are continuable
      if AResultScope.Next <> nil then Exit; // we have a next so we are continuable
    end
    else begin
      if AFlags * [lefSearchSibling, lefSearchChild] = []
      then begin
        if not (lefContinuable in AFlags) then Exit; // no furteher search, so ok.
        if AStartScope.Child <> nil then Exit; // we have a child so we are continuable
        if AStartScope.Next <> nil then Exit; // we have a next so we are continuable
      end;
    end;
    Result := False;
  end;

var
  Abbrev: Cardinal;
  Def: TDwarfAbbrev;
  Level: Integer;
  MaxData: Pointer;
  p: Pointer;
  Scope: TDwarfScopeInfo;
  BuildList: Boolean; // set once if we need to fill the list
  Searching: Boolean; // set as long as we need searching for a tag.
                      // we cannot use result for this, since we might want a topnode search while we need to be continuable
begin
  Result := False;
  if AStartScope = nil then Exit;
  BuildList := False;
  Searching := True;
  Level := 0;
  MaxData := FInfoData + FLength;
  Scope := AStartScope;
  p := Scope.Entry;
  while (p <= MaxData) and (Level >= 0) do
  begin
    p := Scope.Entry;
    Abbrev := ULEB128toOrdinal(p);
    if Abbrev = 0
    then begin
      Dec(Level);
      Scope := Scope.Parent;
      if Scope = nil then Exit;

      if Level < 0 then
      begin
        // p is now the entry of the next of the startparent
        // let's see if we need to set it
        if not (lefContinuable in AFlags) then Exit;
        if AStartScope.Parent = nil then Exit;
        if AStartScope.Parent.Next <> nil then Exit;
        AStartScope.Parent.Next := TDwarfScopeInfo.Create(p);
        Exit;
      end;
      
      if Scope.Next = nil
      then Scope.Next := TDwarfScopeInfo.Create(p);
//      if Level = 0 then Exit;
      if CanExit(Result) then Exit;
      if (Level = 0) and not (lefSearchSibling in AFlags) then Exit;

      Scope := Scope.Next;
      Continue;
    end;
    
    if not GetDefinition(Abbrev, Def)
    then begin
      WriteLN('Error: Abbrev not found: ', Abbrev);
      Break;
    end;

    if Searching
    then begin
      Result := Def.Tag = ATag;
      if Result
      then begin
        Searching := False;
        AResultScope := Scope;
        if lefCreateAttribList in AFlags
        then begin
          SetLength(AList, Def.Count);
          BuildList := True;
        end
        else begin
          AList := nil;
          if not (lefContinuable in AFlags)
          then Exit
        end;
      end
      else begin
        if CanExit(False) then Exit;
        Searching := (lefSearchChild in AFlags)
                  or ((level = 0) and (lefSearchSibling in AFlags));
      end;
    end;

    if not BuildList
    then begin
      // check if we can shortcut the searches
      if (Scope.Child <> nil)
      and ((lefSearchChild in AFlags) or (Scope.Next = nil))
      then begin
        Inc(Level);
        Scope := Scope.Child;
        Continue;
      end;

      if Scope.Next <> nil
      then begin
        // scope.Childvalid is true, otherwise we can not have a next.
        // So no need to check
        if lefSearchSibling in AFlags
        then begin
          Scope := Scope.Next;
          Continue;
        end;
        if Level = 0 then Exit;
      end;
      
      // bummer, we need to parse our attribs, if we want them or not
    end;
    
    ParseAttribs(Def, BuildList, p);
    BuildList := False;

    // if we have a result or don't want to search we're done here
    if CanExit(Result) then Exit;

    // check for shortcuts
    if [lefContinuable, lefSearchChild] * AFlags <> []
    then begin
      if Scope.Child <> nil
      then begin
        Inc(Level);
        Scope := Scope.Child;
        Continue;
      end;
    end
    else if lefSearchSibling in AFlags
    then begin
      if Scope.Next <> nil
      then begin
        Scope := Scope.Next;
        Continue;
      end;
    end;

    // Def.children can be set while no children are found
    // we cannot have a next without a defined child
    if Def.Children
    then begin
      if not Scope.ChildValid
      then begin
        if Scope.Child = nil
        then Scope.Child := TDwarfScopeInfo.Create(p);
        if CanExit(Result) then Exit;
      end;
      Inc(Level);
      Scope := Scope.Child;
      Continue;
    end
    else begin
      Scope.Child := nil; // force childvalid to be set
    end;
    
    if Scope.Next = nil
    then Scope.Next := TDwarfScopeInfo.Create(p);
    if CanExit(Result) then Exit;
    if (Level = 0) and not (lefSearchSibling in AFlags) then Exit;

    Scope := Scope.Next;
  end;
end;

function TDwarfCompilationUnit.MakeAddress(AData: Pointer): QWord;
begin
  if FAddressSize = 4
  then Result := PLongWord(AData)^
  else Result := PQWord(AData)^;
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: Cardinal): Boolean;
begin
  Result := True;
  case AForm of
    DW_FORM_addr,
    DW_FORM_ref_addr : begin
      AValue := MakeAddress(AAttribute);
    end;
    DW_FORM_flag,
    DW_FORM_ref1,
    DW_FORM_data1    : begin
      AValue := PByte(AAttribute)^;
    end;
    DW_FORM_ref2,
    DW_FORM_data2    : begin
      AValue := PWord(AAttribute)^;
    end;
    DW_FORM_ref4,
    DW_FORM_data4    : begin
      AValue := PLongWord(AAttribute)^;
    end;
    DW_FORM_ref8,
    DW_FORM_data8    : begin
      AValue := PQWord(AAttribute)^;
    end;
    DW_FORM_sdata    : begin
      AValue := SLEB128toOrdinal(AAttribute);
    end;
    DW_FORM_ref_udata,
    DW_FORM_udata    : begin
      AValue := ULEB128toOrdinal(AAttribute);
    end;
  else
    Result := False;
  end;
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: Int64): Boolean;
begin
  Result := True;
  case AForm of
    DW_FORM_addr,
    DW_FORM_ref_addr : begin
      AValue := MakeAddress(AAttribute);
    end;
    DW_FORM_flag,
    DW_FORM_ref1,
    DW_FORM_data1    : begin
      AValue := PShortInt(AAttribute)^;
    end;
    DW_FORM_ref2,
    DW_FORM_data2    : begin
      AValue := PSmallInt(AAttribute)^;
    end;
    DW_FORM_ref4,
    DW_FORM_data4    : begin
      AValue := PLongInt(AAttribute)^;
    end;
    DW_FORM_ref8,
    DW_FORM_data8    : begin
      AValue := PInt64(AAttribute)^;
    end;
    DW_FORM_sdata    : begin
      AValue := SLEB128toOrdinal(AAttribute);
    end;
    DW_FORM_ref_udata,
    DW_FORM_udata    : begin
      AValue := ULEB128toOrdinal(AAttribute);
    end;
  else
    Result := False;
  end;
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: Integer): Boolean;
begin
  Result := True;
  case AForm of
    DW_FORM_addr,
    DW_FORM_ref_addr : begin
      AValue := MakeAddress(AAttribute);
    end;
    DW_FORM_flag,
    DW_FORM_ref1,
    DW_FORM_data1    : begin
      AValue := PShortInt(AAttribute)^;
    end;
    DW_FORM_ref2,
    DW_FORM_data2    : begin
      AValue := PSmallInt(AAttribute)^;
    end;
    DW_FORM_ref4,
    DW_FORM_data4    : begin
      AValue := PLongInt(AAttribute)^;
    end;
    DW_FORM_ref8,
    DW_FORM_data8    : begin
      AValue := PInt64(AAttribute)^;
    end;
    DW_FORM_sdata    : begin
      AValue := SLEB128toOrdinal(AAttribute);
    end;
    DW_FORM_ref_udata,
    DW_FORM_udata    : begin
      AValue := ULEB128toOrdinal(AAttribute);
    end;
  else
    Result := False;
  end;
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: PChar): Boolean;
begin
  Result := True;
  case AForm of
    DW_FORM_string: begin
      AValue := PChar(AAttribute);
    end;
    DW_FORM_strp:   begin
      AValue := 'TODO';
    end;
  else
    Result := False;
  end;
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: QWord): Boolean;
begin
  Result := True;
  case AForm of
    DW_FORM_addr,
    DW_FORM_ref_addr : begin
      AValue := MakeAddress(AAttribute);
    end;
    DW_FORM_flag,
    DW_FORM_ref1,
    DW_FORM_data1    : begin
      AValue := PByte(AAttribute)^;
    end;
    DW_FORM_ref2,
    DW_FORM_data2    : begin
      AValue := PWord(AAttribute)^;
    end;
    DW_FORM_ref4,
    DW_FORM_data4    : begin
      AValue := PLongWord(AAttribute)^;
    end;
    DW_FORM_ref8,
    DW_FORM_data8    : begin
      AValue := PQWord(AAttribute)^;
    end;
    DW_FORM_sdata    : begin
      AValue := SLEB128toOrdinal(AAttribute);
    end;
    DW_FORM_ref_udata,
    DW_FORM_udata    : begin
      AValue := ULEB128toOrdinal(AAttribute);
    end;
  else
    Result := False;
  end;
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: String): Boolean;
begin
  Result := True;
  case AForm of
    DW_FORM_string: begin
      AValue := PChar(AAttribute);
    end;
    DW_FORM_strp:   begin
      AValue := 'TODO';
    end;
  else
    Result := False;
  end;
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: TByteDynArray): Boolean;
var
  Size: Cardinal;
begin
  Result := True;
  case AForm of
    DW_FORM_block    : begin
      Size := ULEB128toOrdinal(AAttribute);
    end;
    DW_FORM_block1   : begin
      Size := PByte(AAttribute)^;
      Inc(AAttribute, 1);
    end;
    DW_FORM_block2   : begin
      Size := PWord(AAttribute)^;
      Inc(AAttribute, 2);
    end;
    DW_FORM_block4   : begin
      Size := PLongWord(AAttribute)^;
      Inc(AAttribute, 4);
    end;
  else
    Result := False;
    Size := 0;
  end;
  SetLength(AValue, Size);
  Move(AAttribute^, AValue[0], Size);
end;

{ TDwarfVerboseCompilationUnit }

constructor TDwarfVerboseCompilationUnit.Create(AOwner: TDbgDwarf; ADataOffset: QWord; ALength: QWord; AVersion: Word; AAbbrevOffset: QWord; AAddressSize: Byte; AIsDwarf64: Boolean);
begin
  FVerbose := True;
  
  WriteLN('-- compilation unit --');
  WriteLN(' data offset: ', ADataOffset);
  WriteLN(' length: ', ALength);
  WriteLN(' version: ', AVersion);
  WriteLN(' abbrev offset: ', AAbbrevOffset);
  WriteLN(' address size: ', AAddressSize);
  WriteLn(' 64bit: ', AIsDwarf64);
  WriteLN('----------------------');
  inherited;
end;

{ TDwarfAbbrevDecoder }

constructor TDwarfAbbrevDecoder.Create(ACompilationUnit: TDwarfCompilationUnit);
begin
  inherited Create;
  FCU := ACompilationUnit;
end;

procedure TDwarfAbbrevDecoder.Decode;
var
  Iter: TMapIterator;
  Info: TDwarfAddressInfo;
  Scope: TDwarfScopeInfo;
begin
  // force all abbrevs to be loaded
  FCU.LoadAbbrevs(High(Cardinal));
  InternalDecode(FCU.FInfoData, FCU.FInfoData + FCU.FLength);

  WriteLN('addresses: ');
  Iter := TMapIterator.Create(FCU.FAddressMap);
  while not Iter.EOM do
  begin
    Iter.GetData(Info);
    Write('  ');
    Scope := Info.Scope.Parent;
    while Scope <> nil do
    begin
      Write('.');
      Scope := Scope.Parent;
    end;
    WriteLN(Info.Name, ': $', IntToHex(Info.StartPC, FCU.FAddressSize * 2), '..$', IntToHex(Info.EndPC, FCU.FAddressSize * 2));
    Iter.Next;
  end;
  Iter.Free;

end;

procedure TDwarfAbbrevDecoder.DecodeLocation(AData: PByte; ASize: QWord; const AIndent: String);
var
  MaxData: PByte;
  v: Int64;
begin
  MaxData := AData + ASize - 1;
  while AData <= MaxData do
  begin
    Write(AIndent);
    case AData^ of
      DW_OP_addr: begin
        Write('DW_OP_addr ', MakeAddressString(@AData[1]));
        Inc(AData, 4);
      end;
      DW_OP_deref: begin
        Write('DW_OP_deref');
      end;
      DW_OP_const1u: begin
        Write('DW_OP_const1u ', AData[1]);
        Inc(AData, 1);
      end;
      DW_OP_const1s: begin
        Write('DW_OP_const1s ', PShortInt(@AData[1])^);
        Inc(AData, 1);
      end;
      DW_OP_const2u: begin
        Write('DW_OP_const2u ', PWord(@AData[1])^);
        Inc(AData, 2);
      end;
      DW_OP_const2s: begin
        Write('DW_OP_const2s ', PSmallInt(@AData[1])^);
        Inc(AData, 2);
      end;
      DW_OP_const4u: begin
        Write('DW_OP_const4u ', PLongWord(@AData[1])^);
        Inc(AData, 4);
      end;
      DW_OP_const4s: begin
        Write('DW_OP_const4s ', PLongInt(@AData[1])^);
        Inc(AData, 4);
      end;
      DW_OP_const8u: begin
        Write('DW_OP_const8u ', PQWord(@AData[1])^);
        Inc(AData, 8);
      end;
      DW_OP_const8s: begin
        Write('DW_OP_const8s ', PInt64(@AData[1])^);
        Inc(AData, 8);
      end;
      DW_OP_constu: begin
        Inc(AData);
        Write('DW_OP_constu ', ULEB128toOrdinal(AData));;
        Dec(AData);
      end;
      DW_OP_consts: begin
        Inc(AData);
        Write('DW_OP_consts ', SLEB128toOrdinal(AData));;
        Dec(AData);
      end;
      DW_OP_dup: begin
        Write('DW_OP_dup');
      end;
      DW_OP_drop: begin
        Write('DW_OP_drop');
      end;
      DW_OP_over: begin
        Write('DW_OP_over');
      end;
      DW_OP_pick: begin
        Write('DW_OP_pick ', AData[1]);
        Inc(AData, 1);
      end;
      DW_OP_swap: begin
        Write('DW_OP_swap');
      end;
      DW_OP_rot: begin
        Write('DW_OP_rot');
      end;
      DW_OP_xderef: begin
        Write('DW_OP_xderef');
      end;
      DW_OP_abs: begin
        Write('DW_OP_abs');
      end;
      DW_OP_and: begin
        Write('DW_OP_and');
      end;
      DW_OP_div: begin
        Write('DW_OP_div');
      end;
      DW_OP_minus: begin
        Write('DW_OP_minus');
      end;
      DW_OP_mod: begin
        Write('DW_OP_mod');
      end;
      DW_OP_mul: begin
        Write('DW_OP_mul');
      end;
      DW_OP_neg: begin
        Write('DW_OP_neg');
      end;
      DW_OP_not: begin
        Write('DW_OP_not');
      end;
      DW_OP_or: begin
        Write('DW_OP_or');
      end;
      DW_OP_plus: begin
        Write('DW_OP_plus');
      end;
      DW_OP_plus_uconst: begin
        Inc(AData);
        Write('DW_OP_plus_uconst ', ULEB128toOrdinal(AData));;
        Dec(AData);
      end;
      DW_OP_shl: begin
        Write('DW_OP_shl');
      end;
      DW_OP_shr: begin
        Write('DW_OP_shr');
      end;
      DW_OP_shra: begin
        Write('DW_OP_shra');
      end;
      DW_OP_xor: begin
        Write('DW_OP_xor');
      end;
      DW_OP_skip: begin
        Write('DW_OP_skip ', PSmallInt(@AData[1])^);
        Inc(AData, 2);
      end;
      DW_OP_bra: begin
        Write('DW_OP_bra ', PSmallInt(@AData[1])^);
        Inc(AData, 2);
      end;
      DW_OP_eq: begin
        Write('DW_OP_eq');
      end;
      DW_OP_ge: begin
        Write('DW_OP_ge');
      end;
      DW_OP_gt: begin
        Write('DW_OP_gt');
      end;
      DW_OP_le: begin
        Write('DW_OP_le');
      end;
      DW_OP_lt: begin
        Write('DW_OP_lt');
      end;
      DW_OP_ne: begin
        Write('DW_OP_ne');
      end;
      DW_OP_lit0..DW_OP_lit31: begin
        Write('DW_OP_lit', AData^ - DW_OP_lit0);
      end;
      DW_OP_reg0..DW_OP_reg31: begin
        Write('DW_OP_reg', AData^ - DW_OP_reg0);
      end;
      DW_OP_breg0..DW_OP_breg31: begin
        Write('DW_OP_breg', AData^ - DW_OP_breg0);
        Inc(AData);
        v := SLEB128toOrdinal(AData);
        Dec(AData);
        if v >= 0
        then Write('+');
        Write(v);
      end;
      DW_OP_regx: begin
        Inc(AData);
        Write('DW_OP_regx ', ULEB128toOrdinal(AData));
        Dec(AData);
      end;
      DW_OP_fbreg: begin
        Inc(AData);
        Write('DW_OP_fbreg ', SLEB128toOrdinal(AData));
        Dec(AData);
      end;
      DW_OP_bregx: begin
        Inc(AData);
        Write('DW_OP_bregx ', ULEB128toOrdinal(AData));
        v := SLEB128toOrdinal(AData);
        Dec(AData);
        if v >= 0
        then Write('+');
        Write(v);
      end;
      DW_OP_piece: begin
        Inc(AData);
        Write('DW_OP_piece ', ULEB128toOrdinal(AData));
        Dec(AData);
      end;
      DW_OP_deref_size: begin
        Write('DW_OP_deref_size ', AData[1]);
        Inc(AData);
      end;
      DW_OP_xderef_size: begin
        Write('DW_OP_xderef_size', AData[1]);
        Inc(AData);
      end;
      DW_OP_nop: begin
        Write('DW_OP_nop');
      end;
      DW_OP_push_object_address: begin
        Write('DW_OP_push_object_address');
      end;
      DW_OP_call2: begin
        Write('DW_OP_call2 ', PWord(@AData[1])^);
        Inc(AData, 2);
      end;
      DW_OP_call4: begin
        Write('DW_OP_call4 ', PLongWord(@AData[1])^);
        Inc(AData, 4);
      end;
      DW_OP_call_ref: begin
        Write('DW_OP_call_ref ', MakeAddressString(@AData[1]));
        Inc(AData, 4);
      end;
      DW_OP_form_tls_address: begin
        Write('DW_OP_form_tls_address');
      end;
      DW_OP_call_frame_cfa: begin
        Write('DW_OP_call_frame_cfa');
      end;
      DW_OP_bit_piece: begin
        Inc(AData);
        Write('DW_OP_bit_piece ', ULEB128toOrdinal(AData), ' ', ULEB128toOrdinal(AData));
        Dec(AData);
      end;
      DW_OP_lo_user..DW_OP_hi_user: begin
        Write('DW_OP_user=', AData^);
      end;
    else
      Write('Unknown DW_OP_', AData^);
    end;
    Inc(AData);
    WriteLn;
  end;
end;

procedure TDwarfAbbrevDecoder.DecodeLocationList(AReference: QWord; const AIndent: String);
begin

end;

procedure TDwarfAbbrevDecoder.InternalDecode(AData: Pointer; AMaxData: Pointer; const AIndent: String);
  procedure Dump(var p: PByte; count: QWord);
  var
    n: integer;
  begin
    for n := 1 to Min(80, count) do
    begin
      Write(IntToHex(p^, 2), ' ');
      Inc(p);
    end;
    if Count > 80
    then begin
      Inc(p, Count - 80);
      Write('...');
    end;
  end;
  procedure DumpStr(var p: PChar);
  begin
    while p^ <> #0 do
    begin
      case p^ of
        #32..#127: Write(p^);
      else
        Write('<', IntToHex(Ord(p^), 2), '>');
      end;
      Inc(p);
    end;
    Inc(p);
  end;

var
  Attribute: Cardinal;
  Abbrev, Form: Cardinal;
  Def: TDwarfAbbrev;
  idx: Integer;
  Value: QWord;
  ValueSize: QWord;
  ValuePtr, p: Pointer;
  Indent: String;
  Level: Integer;
begin
  Indent := AIndent;
  Level := 0;
  while (AData <= AMaxData) and (Level >= 0) do
  begin
    Abbrev := ULEB128toOrdinal(AData);
    if Abbrev = 0
    then begin
      Dec(Level);
      SetLength(Indent, Length(Indent) - 2);
      if Level >= 0
      then  WriteLn(Indent, ' \--');
      Continue;
    end;
    Write(Indent, 'abbrev: ', Abbrev);
    if not FCU.GetDefinition(abbrev, Def)
    then begin
      WriteLN;
      WriteLN('Error: Abbrev not found: ', Abbrev);
      Exit;
    end;
    Write(', tag: ', Def.tag, '=', DwarfTagToString(Def.tag));
    if Def.Children
    then begin
      WriteLN(', has children');
      Inc(Level);
    end
    else WriteLn;

    for idx := Def.Index to Def.Index + Def.Count - 1 do
    begin
      Form := FCU.FDefinitions[idx].Form;
      Attribute := FCU.FDefinitions[idx].Attribute;
      Write(Indent, ' attrib: ', Attribute, '=', DwarfAttributeToString(Attribute));
      Write(', form: ', Form, '=', DwarfAttributeFormToString(Form));
      
      ValueSize := 0;
      ValuePtr := nil;
      Value := 0;
      repeat
        Write(', value: ');
        case Form of
          DW_FORM_addr     : begin
            Value := FCU.MakeAddress(AData);
            ValuePtr := Pointer(PtrUInt(Value));
            ValueSize := FCU.FAddressSize;
            Write('$'+IntToHex(Value, FCU.FAddressSize * 2));
            Inc(AData, FCU.FAddressSize);
          end;
          DW_FORM_block    : begin
            ValueSize := ULEB128toOrdinal(AData);
            ValuePtr := AData;
            Write('Size=', ValueSize, ', Data=');
            Dump(AData, ValueSize);
          end;
          DW_FORM_block1   : begin
            ValueSize := PByte(AData)^;
            Inc(AData, 1);
            ValuePtr := AData;
            Write('Size=', ValueSize, ', Data=');
            Dump(AData, ValueSize);
          end;
          DW_FORM_block2   : begin
            ValueSize := PWord(AData)^;
            Inc(AData, 2);
            ValuePtr := AData;
            Write('Size=', ValueSize, ', Data=');
            Dump(AData, ValueSize);
          end;
          DW_FORM_block4   : begin
            ValueSize := PLongWord(AData)^;
            Inc(AData, 4);
            ValuePtr := AData;
            Write('Size=', ValueSize, ', Data=');
            Dump(AData, ValueSize);
          end;
          DW_FORM_data1    : begin
            Value := PByte(AData)^;
            ValueSize := 1;
            Write('$'+IntToHex(Value, 2));
            Inc(AData, 1);
          end;
          DW_FORM_data2    : begin
            Value := PWord(AData)^;
            ValueSize := 2;
            Write('$'+IntToHex(Value, 4));
            Inc(AData, 2);
          end;
          DW_FORM_data4    : begin
            Value := PLongWord(AData)^;
            ValueSize := 4;
            Write('$'+IntToHex(Value, 8));
            Inc(AData, 4);
          end;
          DW_FORM_data8    : begin
            Value := PQWord(AData)^;
            ValueSize := 8;
            Write('$'+IntToHex(Value, 16));
            Inc(AData, 8);
          end;
          DW_FORM_sdata    : begin
            p := AData;
            Value := ULEB128toOrdinal(AData);
            ValueSize := PtrUInt(AData) - PtrUInt(p);
            Write('$'+IntToHex(Value, ValueSize * 2));
          end;
          DW_FORM_udata    : begin
            p := AData;
            Value := ULEB128toOrdinal(AData);
            ValueSize := PtrUInt(AData) - PtrUInt(p);
            Write('$'+IntToHex(Value, ValueSize * 2));
          end;
          DW_FORM_flag     : begin
            Value := PByte(AData)^;
            ValueSize := 1;
            Write('$'+IntToHex(Value, 2));
            Inc(AData, 1);
          end;
          DW_FORM_ref1     : begin
            Value := PByte(AData)^;
            ValueSize := 1;
            Write('$'+IntToHex(Value, 2));
            Inc(AData, 1);
          end;
          DW_FORM_ref2     : begin
            Value := PWord(AData)^;
            ValueSize := 2;
            Write('$'+IntToHex(Value, 4));
            Inc(AData, 2);
          end;
          DW_FORM_ref4     : begin
            Value := PLongWord(AData)^;
            ValueSize := 4;
            Write('$'+IntToHex(Value, 8));
            Inc(AData, 4);
          end;
          DW_FORM_ref8     : begin
            Value := PQWord(AData)^;
            ValueSize := 8;
            Write('$'+IntToHex(Value, 16));
            Inc(AData, 8);
          end;
          DW_FORM_ref_udata: begin
            p := AData;
            Value := ULEB128toOrdinal(AData);
            ValueSize := PtrUInt(AData) - PtrUInt(p);
            Write('$'+IntToHex(Value, ValueSize * 2));
          end;
          DW_FORM_ref_addr : begin
            Value := FCU.MakeAddress(AData);
            ValuePtr := Pointer(PtrUInt(Value));
            ValueSize := FCU.FAddressSize;
            Write('$'+IntToHex(Value, FCU.FAddressSize * 2));
            Inc(AData, FCU.FAddressSize);
          end;
          DW_FORM_string   : begin
            ValuePtr := AData;
            DumpStr(AData);
            ValueSize := PtrUInt(AData) - PtrUInt(ValuePtr);
          end;
          DW_FORM_strp     : begin
            Value := FCU.MakeAddress(AData);
            ValueSize := FCU.FAddressSize;
            Write('$'+IntToHex(Value, FCU.FAddressSize * 2));
            Inc(AData, FCU.FAddressSize);
          end;
          DW_FORM_indirect : begin
            Form := ULEB128toOrdinal(AData);
            Write('indirect form: ', Form, '=', DwarfAttributeFormToString(Form));
            Continue;
          end;
        else
          WriteLN('Error: Unknown Form: ', Form);
          Exit;
        end;
        Break;
      until False;

      case Attribute of
        DW_AT_accessibility: begin
          WriteLn('=', DwarfAccessibilityToString(Value));
        end;
        DW_AT_data_member_location: begin
          WriteLn('-->');
          DecodeLocation(ValuePtr, ValueSize, Indent + '  ');
        end;
        DW_AT_encoding: begin
          WriteLn('=', DwarfBaseTypeEncodingToString(Value));
        end;
        DW_AT_language: begin
          WriteLn('=', DwarfLanguageToString(Value));
        end;
        DW_AT_identifier_case: begin
          WriteLn('=', DwarfIdentifierCaseToString(Value));
        end;
        DW_AT_location: begin
          if ValuePtr = nil
          then begin
            WriteLn('-->');
            DecodeLocationList(Value, AIndent + '  ');
          end
          else begin
            WriteLn('-->');
            DecodeLocation(ValuePtr, ValueSize, Indent + '  ');
          end;
        end;
        DW_AT_type: begin
          WriteLn('-->');
          try
            p := FCU.FOwner.FSections[dsInfo].RawData + Value - FCU.FOwner.FImageBase - FCU.FOwner.FSections[dsInfo].VirtualAdress;
            InternalDecode(p, p, Indent + '  ');
          except
            on E: Exception do WriteLN(AIndent, '  ', E.Message);
          end;
        end;
      else
        WriteLN;
      end;
    end;

    if Def.Children
    then begin
      WriteLn(Indent, ' /--');
      Indent := Indent + ' |';
    end;
  end;
end;


function TDwarfAbbrevDecoder.MakeAddressString(AData: Pointer): string;
begin
  if FCU.FAddressSize = 4
  then Result := '$'+IntToHex(PLongWord(AData)^, 8)
  else Result := '$'+IntToHex(PQWord(AData)^, 16);
end;

{ TDwarfStatementDecoder }

constructor TDwarfStatementDecoder.Create(ACompilationUnit: TDwarfCompilationUnit);
begin
  inherited Create;
  FCU := ACompilationUnit;
end;

procedure TDwarfStatementDecoder.Decode;
begin
  if FCU.FLineInfo.Header = nil
  then begin
    WriteLN('No lineinfo');
    Exit;
  end;
  InternalDecode(FCU.FLineInfo.Header, FCU.FOwner.FSections[dsInfo].RawData + FCU.FOwner.FSections[dsInfo].Size);
end;

procedure TDwarfStatementDecoder.InternalDecode(AData: Pointer; AMaxData: Pointer; const AIndent: String);
var
  Info: PDwarfLNPInfoHeader;

  Address: QWord;
  Line: Int64;
  FileNr: Cardinal;
  Column: Cardinal;
  IsStmt: Boolean;
  BasicBlock: Boolean;
  PrologueEnd: Boolean;
  EpilogueBegin: Boolean;
  Isa: QWord;

  procedure AddRow(ALast: Boolean = False);
  begin
    Write('> ');
    Write('Address=$', IntToHex(Address, FCU.FAddressSize * 2));
    Write(', Line=',Line);
    Write(', FileNr=',FileNr);
    Write(', Column=',Column);
    Write(', IsStmt=',IsStmt);
    Write(', BasicBlock=',BasicBlock);
    Write(', PrologueEnd=',PrologueEnd);
    Write(', EpilogueBegin=',EpilogueBegin);
    Write(', Isa=',Isa);
    WriteLn;
    if ALast
    then WriteLn('> ---------');
  end;
  
  procedure DoAdjust(AOpcode: Byte);
  begin
    Dec(AOpcode, Info^.OpcodeBase);
    if Info^.LineRange = 0
    then begin
      Inc(Address, AOpcode * Info^.MinimumInstructionLength);
    end
    else begin
      Inc(Address, (AOpcode div Info^.LineRange) * Info^.MinimumInstructionLength);
      Inc(Line, Info^.LineBase + (AOpcode mod Info^.LineRange));
    end;
  end;
  
  procedure DoReset;
  begin
    Address := 0;
    Line := 1;
    FileNr := 1;
    Column := 0;
    IsStmt := Info^.DefaultIsStmt <> 0;
    BasicBlock := False;
    PrologueEnd := False;
    EpilogueBegin := False;
    Isa := 0;
  end;


var
  LNP32: PDwarfLNPHeader32 absolute AData;
  LNP64: PDwarfLNPHeader64 absolute AData;
  UnitLength: QWord;
  Version: Word;
  HeaderLength: QWord;
  n: integer;
  ptr: Pointer;
  p: Pointer;
  pb: PByte absolute p;
  pc: PChar absolute p;
  DataEnd: Pointer;
  DataStart: Pointer;
  UValue: QWord;
  SValue: Int64;
begin
  WriteLn('FileName: ', FCU.FFileName);

  if LNP64^.Signature = DWARF_HEADER64_SIGNATURE
  then begin
    UnitLength := LNP64^.UnitLength;
    DataEnd := Pointer(@LNP64^.Version) + UnitLength;
    Version := LNP64^.Version;
    HeaderLength := LNP64^.HeaderLength;
    Info := @LNP64^.Info;
  end
  else begin
    UnitLength := LNP32^.UnitLength;
    DataEnd := Pointer(@LNP32^.Version) + UnitLength;
    Version := LNP32^.Version;
    HeaderLength := LNP32^.HeaderLength;
    Info := @LNP32^.Info;
  end;
  DataStart := PByte(Info) + HeaderLength;

  WriteLN('UnitLength: ', UnitLength);
  WriteLN('Version: ', Version);
  WriteLN('HeaderLength: ', HeaderLength);

  WriteLN('MinimumInstructionLength: ', Info^.MinimumInstructionLength);
  WriteLN('DefaultIsStmt: ', Info^.DefaultIsStmt);
  WriteLN('LineBase: ', Info^.LineBase);
  WriteLN('LineRange: ', Info^.LineRange);
  WriteLN('OpcodeBase: ', Info^.OpcodeBase);
  p := @Info^.StandardOpcodeLengths;
  WriteLN('StandardOpcodeLengths:');
  for n := 1 to Info^.OpcodeBase - 1 do
  begin
    WriteLN('  [', n, '] ', pb^);
    Inc(pb);
  end;

  WriteLN('IncludeDirectories:');
  while pc^ <> #0 do
  begin
    Write('  ');
    repeat
      Write(pc^);
      Inc(pc);
    until pc^ = #0;
    WriteLN;
    Inc(pc);
  end;
  Inc(pc);
  WriteLN('FileNames:');
  while pc^ <> #0 do
  begin
    Write('  ');
    repeat
      Write(pc^);
      Inc(pc);
    until pc^ = #0;
    Inc(pc);
    Write(', diridx=', ULEB128toOrdinal(p));
    Write(', last modified=', ULEB128toOrdinal(p));
    Write(', length=', ULEB128toOrdinal(p));
    WriteLN;
  end;
  
  WriteLN('Program:');

  p := DataStart;
  DoReset;
  
  while p < DataEnd do
  begin
    Write('  ');
    if (pb^ > 0) and (pb^ < Info^.OpcodeBase)
    then begin
      // Standard opcode
      case pb^ of
        DW_LNS_copy: begin
          Inc(p);
          WriteLn('DW_LNS_copy');
          AddRow;
          BasicBlock := False;
          PrologueEnd := False;
          EpilogueBegin := False;
        end;
        DW_LNS_advance_pc: begin
          Inc(p);
          UValue := ULEB128toOrdinal(p);
          Inc(Address, UValue);
          WriteLn('DW_LNS_advance_pc ', UValue);
        end;
        DW_LNS_advance_line: begin
          Inc(p);
          SValue := SLEB128toOrdinal(p);
          Inc(Line, SValue);
          WriteLn('DW_LNS_advance_line ', SValue);
        end;
        DW_LNS_set_file: begin
          Inc(p);
          UValue := ULEB128toOrdinal(p);
          WriteLn('DW_LNS_set_file ', UVAlue);
          FileNr := UValue;
        end;
        DW_LNS_set_column: begin
          Inc(p);
          UValue := ULEB128toOrdinal(p);
          WriteLn('DW_LNS_set_column ', UValue);
          Column := UValue;
        end;
        DW_LNS_negate_stmt: begin
          Inc(p);
          WriteLn('DW_LNS_negate_stmt');
          IsStmt := not IsStmt;
        end;
        DW_LNS_set_basic_block: begin
          Inc(p);
          WriteLn('DW_LNS_set_basic_block');
          BasicBlock := True;
        end;
        DW_LNS_const_add_pc: begin
          Inc(p);
          WriteLn('DW_LNS_const_add_pc');
          DoAdjust(255);
        end;
        DW_LNS_fixed_advance_pc: begin
          Inc(p);
          Inc(Address, PWord(p)^);
          WriteLN('DW_LNS_fixed_advance_pc ', PWord(p)^);
          Inc(p, 2);
        end;
        DW_LNS_set_prologue_end: begin
          Inc(p);
          WriteLn('DW_LNS_set_prologue_end');
          PrologueEnd := True;
        end;
        DW_LNS_set_epilogue_begin: begin
          Inc(p);
          WriteLn('DW_LNS_set_epilogue_begin');
          EpilogueBegin := True;
        end;
        DW_LNS_set_isa: begin
          Inc(p);
          UValue := ULEB128toOrdinal(p);
          Isa := UValue;
          WriteLn('DW_LNS_set_isa ', UValue);
        end;
      else
        Write('unknown opcode: ', pb^);
        Inc(p, PByte(@Info^.StandardOpcodeLengths)[pb^-1]);
      end;
      Continue;
    end;

    if pb^ = 0
    then begin
      // Extended opcode
      Inc(p);
      UValue := ULEB128toOrdinal(p); // instruction length
      
      case pb^ of
        DW_LNE_end_sequence: begin
          WriteLN('DW_LNE_end_sequence');
          AddRow(True);
          DoReset;
          //Inc(p, UValue);
          //Break;
        end;
        DW_LNE_set_address: begin
          if LNP64^.Signature = DWARF_HEADER64_SIGNATURE
          then Address := PQWord(pb+1)^
          else Address := PLongWord(pb+1)^;
          WriteLN('DW_LNE_set_address $', IntToHex(Address, FCU.FAddressSize * 2));
        end;
        DW_LNE_define_file: begin
          ptr := p;
          Inc(ptr);
          Write('DW_LNE_define_file name=');
          repeat
            Write(PChar(ptr)^);
            Inc(ptr);
          until PChar(ptr)^ = #0;
          Inc(ptr);
          Write(', diridx=', ULEB128toOrdinal(ptr));
          Write(', last modified=', ULEB128toOrdinal(ptr));
          Write(', length=', ULEB128toOrdinal(ptr));
          WriteLN;
        end;
      else
        Write('unknown extended opcode: ', pb^);
      end;
      Inc(p, UValue);
    end
    else begin
      WriteLn('Special opcode: ', pb^);
      // Special opcode
      DoAdjust(pb^);
      AddRow;
      BasicBlock := False;
      PrologueEnd := False;
      EpilogueBegin := False;
      Inc(p);
    end;
  end;
end;

{ TVerboseDwarfCallframeDecoder }

constructor TVerboseDwarfCallframeDecoder.Create(ALoader: TDbgImageLoader);
begin
  inherited Create;
  FLoader := Aloader;
end;

procedure TVerboseDwarfCallframeDecoder.Decode;
var
  Section: PDbgImageSection;
begin
  Section := FLoader.Section[DWARF_SECTION_NAME[dsFrame]];
  if Section <> nil
  then InternalDecode(Section^.RawData, Section^.Size, Section^.VirtualAdress);
end;

procedure TVerboseDwarfCallframeDecoder.InternalDecode(AData: Pointer; ASize: QWord; AStart: QWord);
var
  Is64bit: boolean;

  procedure DecodeInstructions(p: Pointer; MaxAddr: Pointer);
  var
    pb: PByte absolute p;
    pw: PWord absolute p;
    pc: PCardinal absolute p;
    pq: PQWord absolute p;
    q: QWord;
  begin
    repeat
      Write(' ');
      Inc(pb);
      case pb[-1] of
        DW_CFA_nop: begin
          WriteLn('DW_CFA_nop');
        end;
        DW_CFA_set_loc: begin
          // address
          Write('DW_CFA_set_loc $');
          if Is64Bit
          then begin
            WriteLn(IntToHex(pq^, 16));
            Inc(pq);
          end
          else begin
            WriteLn(IntToHex(pc^, 8));
            Inc(pc);
          end;
        end;
        DW_CFA_advance_loc1: begin
          // 1-byte delta
          WriteLn('DW_CFA_advance_loc1 ', pb^, ' * caf');
          Inc(pb);
        end;
        DW_CFA_advance_loc2: begin
          // 2-byte delta
          WriteLn('DW_CFA_advance_loc2 ', pw^, ' * caf');
          Inc(pw);
        end;
        DW_CFA_advance_loc4: begin
          // 4-byte delta
          WriteLn('DW_CFA_advance_loc4 ', pc^, ' * caf');
          Inc(pw);
        end;
        DW_CFA_offset_extended: begin
          // ULEB128 register, ULEB128 offset
          WriteLn('DW_CFA_offset_extended R', ULEB128toOrdinal(p), ' + ', ULEB128toOrdinal(p), ' * daf');
        end;
        DW_CFA_restore_extended: begin
          // ULEB128 register
          WriteLn('DW_CFA_restore_extended R', ULEB128toOrdinal(p));
        end;
        DW_CFA_undefined: begin
          // ULEB128 register
          WriteLn('DW_CFA_undefined R', ULEB128toOrdinal(p));
        end;
        DW_CFA_same_value: begin
          // ULEB128 register
          WriteLn('DW_CFA_same_value R', ULEB128toOrdinal(p));
        end;
        DW_CFA_register: begin
          // ULEB128 register, ULEB128 register
          WriteLn('DW_CFA_register R', ULEB128toOrdinal(p), ' R', ULEB128toOrdinal(p));
        end;
        DW_CFA_remember_state: begin
          WriteLn('DW_CFA_remember_state');
        end;
        DW_CFA_restore_state: begin
          WriteLn('DW_CFA_restore_state');
        end;
        DW_CFA_def_cfa: begin
          // ULEB128 register, ULEB128 offset
          WriteLn('DW_CFA_def_cfa R', ULEB128toOrdinal(p), ' + ', ULEB128toOrdinal(p));
        end;
        DW_CFA_def_cfa_register: begin
          // ULEB128 register
          WriteLn('DW_CFA_def_cfa_register R', ULEB128toOrdinal(p));
        end;
        DW_CFA_def_cfa_offset: begin
          // ULEB128 offset
          WriteLn('DW_CFA_def_cfa_offset ', ULEB128toOrdinal(p));
        end;
        // --- DWARF3 ---
        DW_CFA_def_cfa_expression: begin
          // BLOCK
          q := ULEB128toOrdinal(p);
          WriteLn('DW_CFA_def_cfa_expression, lenght=',q);
          Inc(p, q);
        end;
        DW_CFA_expression: begin
          // ULEB128 register, BLOCK
          Write('DW_CFA_expression R', ULEB128toOrdinal(p), ' lenght=',q);
          q := ULEB128toOrdinal(p);
          WriteLn(q);
          Inc(p, q);
        end;
        DW_CFA_offset_extended_sf: begin
          // ULEB128 register, SLEB128 offset
          WriteLn('DW_CFA_offset_extended_sf R', ULEB128toOrdinal(p), ' + ', SLEB128toOrdinal(p), ' * daf');
        end;
        DW_CFA_def_cfa_sf: begin
          // ULEB128 register, SLEB128 offset
          WriteLn('DW_CFA_def_cfa_sf R', ULEB128toOrdinal(p), ' + ', SLEB128toOrdinal(p), ' * daf');
        end;
        DW_CFA_def_cfa_offset_sf: begin
          // SLEB128 offset
          WriteLn('DW_CFA_def_cfa_offset_sf ', SLEB128toOrdinal(p), ' * daf' );
        end;
        DW_CFA_val_offset: begin
          // ULEB128         , ULEB128
          WriteLn('DW_CFA_val_offset R', ULEB128toOrdinal(p), ' + ', ULEB128toOrdinal(p), ' * daf');
        end;
        DW_CFA_val_offset_sf: begin
          // ULEB128         , SLEB128
          WriteLn('DW_CFA_val_offset_sf R', ULEB128toOrdinal(p), ' + ', SLEB128toOrdinal(p), ' * daf');
        end;
        DW_CFA_val_expression: begin
          // ULEB128         , BLOCK
          Write('DW_CFA_val_expression R', ULEB128toOrdinal(p), ' lenght=',q);
          q := ULEB128toOrdinal(p);
          WriteLn(q);
          Inc(p, q);
        end;
        // ---  ---
        DW_CFA_lo_user..DW_CFA_hi_user: begin
          WriteLn('DW_CFA_user=', pb^);
        end;
        // ---  ---
        DW_CFA_advance_loc..DW_CFA_offset-1: begin
          // delta
          WriteLn('DW_CFA_advance_loc ', pb[-1] and $3F, ' * caf');
        end;
        DW_CFA_offset..DW_CFA_restore-1: begin
          // register  ULEB128 offset
          WriteLn('DW_CFA_offset R', pb[-1] and $3F, ' + ', ULEB128toOrdinal(p),' * caf');
        end;
        DW_CFA_restore..$FF: begin
         // register
          WriteLn('DW_CFA_restore R', pb[-1] and $3F);
        end;
      else
        WriteLn('Undefined $', IntToHex(pb[-1], 2));
      end;
    until p >= MaxAddr;
  end;

var
  p, next: Pointer;
  pb: PByte absolute p;
  pw: PWord absolute p;
  pi: PInteger absolute p;
  pc: PCardinal absolute p;
  pq: PQWord absolute p;
  
  len: QWord;
  version: Byte;
  IsCie: Boolean;

  s: String;
begin
  p := AData;
  while p < Adata + ASize do
  begin
    WriteLn('[', PtrUInt(p) - PtrUInt(AData), ']');

    Is64bit := pi^ = -1;
    if Is64bit
    then begin
      Inc(pi);
      len := pq^;
      Inc(pq);
      IsCie := Int64(pq^) = -1;
    end
    else begin
      len := pc^;
      Inc(pc);
      IsCie := pi^ = -1;
    end;
    next := p + len;

    if IsCie
    then WriteLn('=== CIE ===')
    else WriteLn('--- FDE ---');

    WriteLn('Length: ', len);

    if IsCie
    then begin
      Inc(pi);
      version := pb^;
      WriteLn('Version: ', version);
      Inc(pb);
      S := Pchar(p);
      WriteLn('Augmentation: ', S);
      Inc(p, Length(s) + 1);
      WriteLn('Code alignment factor (caf): ', ULEB128toOrdinal(p));
      WriteLn('Data alignment factor (daf): ', SLEB128toOrdinal(p));
      Write('Return addr: R');
      if version <= 2
      then begin
        WriteLn(pb^);
        Inc(pb);
      end
      else WriteLn(ULEB128toOrdinal(p));
    end
    else begin
      if pc^ > ASize
      then WriteLn('CIE: $', IntToHex(pc^, 8), ' (=adress ?) -> offset: ', pc^ - AStart - FLoader.ImageBase)
      else WriteLn('CIE: ', pc^);
      Inc(pc);
      WriteLn('InitialLocation: $', IntToHex(pc^, 8));
      Inc(pc);
      WriteLn('Address range: ', pc^);
      Inc(pc);
    end;
    WriteLn('Instructions:');
    DecodeInstructions(p, next);

    p := next;
  end;
end;

end.

