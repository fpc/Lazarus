unit TestDwarfSetupArray;

{$mode objfpc}{$H+}
{$IF FPC_FULLVERSION>=20701}
  {$OPTIMIZATION NOREMOVEEMPTYPROCS}
  {$OPTIMIZATION NOORDERFIELDS}
{$ENDIF}
{$OPTIMIZATION OFF}
{$A1}

(*
  Data generated from testdata\dwarfsetupbasic.lpr
*)

interface

uses
  FpDbgDwarfConst,
  TestHelperClasses;

const
  TTestSetupArrayProcMainAddr = $00400000;

type

  {%region  Types defined in the DWARF }
  TDynIntArray =   Array of integer;
  TStatIntArray1 = Array [0..10] of integer;
  TStatIntArray2 = Array [5..10] of integer;
  {%endregion  Types defined in the DWARF }

type
  { TTestDwarfSetupBasic }

  { TTestLoaderSetupArray }

  TTestLoaderSetupArray = class(TTestDummyImageLoader)
  public
    constructor Create; override;
    procedure  PoissonTestFrame;
  public
    SectionDbgInfo: TTestDummySectionInfoEntries;

     Unitdwarfsetuparray_lpr_0, VarVARDYNINTARRAY_1, VarVARSTATINTARRAY1_2, VarVARSTATINTARRAY2_3, Progmain_4, ProgPDWARFSETUPARRAY_init_implicit_5, ProgPDWARFSETUPARRAY_finalize_implicit_6, TypeDeclTDYNINTARRAY_7, TypePtr_8, TypeTDYNINTARRAY_9, Type_10, Type_11, TypeDeclTSTATINTARRAY1_12, TypeTSTATINTARRAY1_13, Type_14, Type_15, TypeDeclTSTATINTARRAY2_16, TypeTSTATINTARRAY2_17, Type_18, Type_19, TypeDeclLONGINT_20, TypeLONGINT_21, Type_22, TypeDeclSHORTINT_23, TypeSHORTINT_24, Type_25
    : TTestDwarfInfoEntry;

    // global vars
    GlobalVar: record
      PAD_Before: QWord; // padding will be filled with bad data

      VarDynIntArray: TDynIntArray;
      VarStatIntArray1: TStatIntArray1;
      VarStatIntArray2: TStatIntArray2;

      PAD_After: QWord;
    end;

  end;

implementation

{ TTestLoaderSetupArray }

constructor TTestLoaderSetupArray.Create;
begin
  inherited Create;
  PoissonTestFrame;

  SectionDbgInfo := TestImgReader.TestSection['.debug_info'] as TTestDummySectionInfoEntries;
  Unitdwarfsetuparray_lpr_0 := SectionDbgInfo.GetFirstInfoEntryObj;

  // Generated with fpc 2.6.2 32 bit win

Unitdwarfsetuparray_lpr_0.Tag := DW_TAG_compile_unit;
Unitdwarfsetuparray_lpr_0.Children := 1;
Unitdwarfsetuparray_lpr_0.Add(DW_AT_name, DW_FORM_string, 'dwarfsetuparray.lpr'+#0);
Unitdwarfsetuparray_lpr_0.Add(DW_AT_producer, DW_FORM_string, 'Free Pascal 2.6.2 2013/02/16'+#0);
Unitdwarfsetuparray_lpr_0.Add(DW_AT_comp_dir, DW_FORM_string, 'B:/lazarus_latest/components/fpdebug/test/testdata/'+#0);
Unitdwarfsetuparray_lpr_0.Add(DW_AT_language, DW_FORM_data1, [$09]);
Unitdwarfsetuparray_lpr_0.Add(DW_AT_identifier_case, DW_FORM_data1, [$03]);
Unitdwarfsetuparray_lpr_0.Add(DW_AT_stmt_list, DW_FORM_data4, [$00, $00, $00, $00]);
Unitdwarfsetuparray_lpr_0.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00400000);
Unitdwarfsetuparray_lpr_0.AddAddr(DW_AT_high_pc, DW_FORM_addr, $004FFFFF);

  VarVARDYNINTARRAY_1 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  VarVARDYNINTARRAY_1.Tag := DW_TAG_variable;
  VarVARDYNINTARRAY_1.Children := 0;
  VarVARDYNINTARRAY_1.Add(DW_AT_name, DW_FORM_string, 'VARDYNINTARRAY'+#0);
  VarVARDYNINTARRAY_1.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarDYNINTARRAY)])); // $03, $00, $00, $00, $00
  VarVARDYNINTARRAY_1.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTDYNINTARRAY_7); // $41, $01, $00, $00

  VarVARSTATINTARRAY1_2 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  VarVARSTATINTARRAY1_2.Tag := DW_TAG_variable;
  VarVARSTATINTARRAY1_2.Children := 0;
  VarVARSTATINTARRAY1_2.Add(DW_AT_name, DW_FORM_string, 'VARSTATINTARRAY1'+#0);
  VarVARSTATINTARRAY1_2.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSTATINTARRAY1)])); // $03, $00, $90, $40, $00
  VarVARSTATINTARRAY1_2.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSTATINTARRAY1_12); // $77, $01, $00, $00

  VarVARSTATINTARRAY2_3 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  VarVARSTATINTARRAY2_3.Tag := DW_TAG_variable;
  VarVARSTATINTARRAY2_3.Children := 0;
  VarVARSTATINTARRAY2_3.Add(DW_AT_name, DW_FORM_string, 'VARSTATINTARRAY2'+#0);
  VarVARSTATINTARRAY2_3.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSTATINTARRAY2)])); // $03, $00, $00, $00, $00
  VarVARSTATINTARRAY2_3.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSTATINTARRAY2_16); // $AE, $01, $00, $00

  Progmain_4 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  Progmain_4.Tag := DW_TAG_subprogram;
  Progmain_4.Children := 0;
  Progmain_4.Add(DW_AT_name, DW_FORM_string, 'main'+#0);
  Progmain_4.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
  Progmain_4.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
  Progmain_4.Add(DW_AT_external, DW_FORM_flag, [$01]);
  Progmain_4.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00400000);
  Progmain_4.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00400FFF);

  ProgPDWARFSETUPARRAY_init_implicit_5 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  ProgPDWARFSETUPARRAY_init_implicit_5.Tag := DW_TAG_subprogram;
  ProgPDWARFSETUPARRAY_init_implicit_5.Children := 0;
  ProgPDWARFSETUPARRAY_init_implicit_5.Add(DW_AT_name, DW_FORM_string, 'P$DWARFSETUPARRAY_init_implicit'+#0);
  ProgPDWARFSETUPARRAY_init_implicit_5.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
  ProgPDWARFSETUPARRAY_init_implicit_5.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
  ProgPDWARFSETUPARRAY_init_implicit_5.Add(DW_AT_external, DW_FORM_flag, [$01]);
  ProgPDWARFSETUPARRAY_init_implicit_5.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00401000);
  ProgPDWARFSETUPARRAY_init_implicit_5.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00401FFF);

  ProgPDWARFSETUPARRAY_finalize_implicit_6 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  ProgPDWARFSETUPARRAY_finalize_implicit_6.Tag := DW_TAG_subprogram;
  ProgPDWARFSETUPARRAY_finalize_implicit_6.Children := 0;
  ProgPDWARFSETUPARRAY_finalize_implicit_6.Add(DW_AT_name, DW_FORM_string, 'P$DWARFSETUPARRAY_finalize_implicit'+#0);
  ProgPDWARFSETUPARRAY_finalize_implicit_6.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
  ProgPDWARFSETUPARRAY_finalize_implicit_6.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
  ProgPDWARFSETUPARRAY_finalize_implicit_6.Add(DW_AT_external, DW_FORM_flag, [$01]);
  ProgPDWARFSETUPARRAY_finalize_implicit_6.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00402000);
  ProgPDWARFSETUPARRAY_finalize_implicit_6.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00402FFF);

  TypeDeclTDYNINTARRAY_7 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  TypeDeclTDYNINTARRAY_7.Tag := DW_TAG_typedef;
  TypeDeclTDYNINTARRAY_7.Children := 0;
  TypeDeclTDYNINTARRAY_7.Add(DW_AT_name, DW_FORM_string, 'TDYNINTARRAY'+#0);
  TypeDeclTDYNINTARRAY_7.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_8); // $53, $01, $00, $00

  TypePtr_8 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  TypePtr_8.Tag := DW_TAG_pointer_type;
  TypePtr_8.Children := 0;
  TypePtr_8.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTDYNINTARRAY_9); // $58, $01, $00, $00

  TypeTDYNINTARRAY_9 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  TypeTDYNINTARRAY_9.Tag := DW_TAG_array_type;
  TypeTDYNINTARRAY_9.Children := 1;
  TypeTDYNINTARRAY_9.Add(DW_AT_name, DW_FORM_string, 'TDYNINTARRAY'+#0);
  TypeTDYNINTARRAY_9.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_20); // $E5, $01, $00, $00

    Type_10 := TypeTDYNINTARRAY_9.GetNewChild;
    Type_10.Tag := DW_TAG_subrange_type;
    Type_10.Children := 0;
    Type_10.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
    Type_10.AddULEB(DW_AT_byte_stride, DW_FORM_udata, 4);
    Type_10.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_20); // $E5, $01, $00, $00

  Type_11 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  Type_11.Tag := DW_TAG_reference_type;
  Type_11.Children := 0;
  Type_11.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTDYNINTARRAY_7); // $41, $01, $00, $00

  TypeDeclTSTATINTARRAY1_12 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  TypeDeclTSTATINTARRAY1_12.Tag := DW_TAG_typedef;
  TypeDeclTSTATINTARRAY1_12.Children := 0;
  TypeDeclTSTATINTARRAY1_12.Add(DW_AT_name, DW_FORM_string, 'TSTATINTARRAY1'+#0);
  TypeDeclTSTATINTARRAY1_12.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSTATINTARRAY1_13); // $8B, $01, $00, $00

  TypeTSTATINTARRAY1_13 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  TypeTSTATINTARRAY1_13.Tag := DW_TAG_array_type;
  TypeTSTATINTARRAY1_13.Children := 1;
  TypeTSTATINTARRAY1_13.Add(DW_AT_name, DW_FORM_string, 'TSTATINTARRAY1'+#0);
  TypeTSTATINTARRAY1_13.AddULEB(DW_AT_byte_size, DW_FORM_udata, 44);
  TypeTSTATINTARRAY1_13.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_20); // $E5, $01, $00, $00

    Type_14 := TypeTSTATINTARRAY1_13.GetNewChild;
    Type_14.Tag := DW_TAG_subrange_type;
    Type_14.Children := 0;
    Type_14.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
    Type_14.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 10);
    Type_14.AddULEB(DW_AT_byte_stride, DW_FORM_udata, 4);
    Type_14.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_23); // $02, $02, $00, $00

  Type_15 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  Type_15.Tag := DW_TAG_reference_type;
  Type_15.Children := 0;
  Type_15.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSTATINTARRAY1_12); // $77, $01, $00, $00

  TypeDeclTSTATINTARRAY2_16 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  TypeDeclTSTATINTARRAY2_16.Tag := DW_TAG_typedef;
  TypeDeclTSTATINTARRAY2_16.Children := 0;
  TypeDeclTSTATINTARRAY2_16.Add(DW_AT_name, DW_FORM_string, 'TSTATINTARRAY2'+#0);
  TypeDeclTSTATINTARRAY2_16.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSTATINTARRAY2_17); // $C2, $01, $00, $00

  TypeTSTATINTARRAY2_17 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  TypeTSTATINTARRAY2_17.Tag := DW_TAG_array_type;
  TypeTSTATINTARRAY2_17.Children := 1;
  TypeTSTATINTARRAY2_17.Add(DW_AT_name, DW_FORM_string, 'TSTATINTARRAY2'+#0);
  TypeTSTATINTARRAY2_17.AddULEB(DW_AT_byte_size, DW_FORM_udata, 24);
  TypeTSTATINTARRAY2_17.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_20); // $E5, $01, $00, $00

    Type_18 := TypeTSTATINTARRAY2_17.GetNewChild;
    Type_18.Tag := DW_TAG_subrange_type;
    Type_18.Children := 0;
    Type_18.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 5);
    Type_18.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 10);
    Type_18.AddULEB(DW_AT_byte_stride, DW_FORM_udata, 4);
    Type_18.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_23); // $02, $02, $00, $00

  Type_19 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  Type_19.Tag := DW_TAG_reference_type;
  Type_19.Children := 0;
  Type_19.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSTATINTARRAY2_16); // $AE, $01, $00, $00

  TypeDeclLONGINT_20 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  TypeDeclLONGINT_20.Tag := DW_TAG_typedef;
  TypeDeclLONGINT_20.Children := 0;
  TypeDeclLONGINT_20.Add(DW_AT_name, DW_FORM_string, 'LONGINT'+#0);
  TypeDeclLONGINT_20.AddRef(DW_AT_type, DW_FORM_ref4, @TypeLONGINT_21); // $F2, $01, $00, $00

  TypeLONGINT_21 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  TypeLONGINT_21.Tag := DW_TAG_base_type;
  TypeLONGINT_21.Children := 0;
  TypeLONGINT_21.Add(DW_AT_name, DW_FORM_string, 'LONGINT'+#0);
  TypeLONGINT_21.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeLONGINT_21.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

  Type_22 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  Type_22.Tag := DW_TAG_reference_type;
  Type_22.Children := 0;
  Type_22.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_20); // $E5, $01, $00, $00

  TypeDeclSHORTINT_23 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  TypeDeclSHORTINT_23.Tag := DW_TAG_typedef;
  TypeDeclSHORTINT_23.Children := 0;
  TypeDeclSHORTINT_23.Add(DW_AT_name, DW_FORM_string, 'SHORTINT'+#0);
  TypeDeclSHORTINT_23.AddRef(DW_AT_type, DW_FORM_ref4, @TypeSHORTINT_24); // $10, $02, $00, $00

  TypeSHORTINT_24 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  TypeSHORTINT_24.Tag := DW_TAG_base_type;
  TypeSHORTINT_24.Children := 0;
  TypeSHORTINT_24.Add(DW_AT_name, DW_FORM_string, 'SHORTINT'+#0);
  TypeSHORTINT_24.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeSHORTINT_24.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_25 := Unitdwarfsetuparray_lpr_0.GetNewChild;
  Type_25.Tag := DW_TAG_reference_type;
  Type_25.Children := 0;
  Type_25.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_23); // $02, $02, $00, $00

  //
  SectionDbgInfo.CreateSectionData;
  SectionDbgInfo.AbbrevSection.CreateSectionData;
end;

procedure TTestLoaderSetupArray.PoissonTestFrame;
begin
// do not poison managed types
  // Ensure any out of bound reads get bad data
//  FillByte(GlobalVar, SizeOf(GlobalVar), $D5);

end;

end.

