unit TestDwarfSetupBasic;

{$mode objfpc}{$H+}
{$IF FPC_FULLVERSION>=20701}
  {$OPTIMIZATION NOREMOVEEMPTYPROCS}
  {$OPTIMIZATION NOORDERFIELDS}
{$ENDIF}
{$OPTIMIZATION OFF}
{$A2}

(*
  Data generated from testdata\dwarfsetupbasic.lpr
*)

interface

uses
  FpDbgDwarfConst,
  TestHelperClasses;

const
  TTestSetupBasicProcMainAddr = $00400000;

type

  {%region  Types defined in the DWARF }
  TEnum0 = (e0a);
  TEnum1 = (e1a, e1b, e1c);
  TEnum2 = (e2a, e2b, e2c, e2d, e2e, e2f, e2g, e2h, e2i);
  TEnum3 = (e3a, e3b, e3c, e3d, e3e, e3f, e3g, e3h,
            e3i, e3j, e3k, e3l, e3m, e3n, e3o, e3p,
            e3q);
  TEnumX = (eXa, eXb := 10, eXc := 3);

  TSet0 = Set of TEnum0;
  TSet1 = Set of TEnum1;
  TSet2 = Set of TEnum2;
  TSet3 = Set of TEnum3;
  TSetX1 = Set of (s1a, s1b, s1c);

  {%endregion}

type
  { TTestDwarfSetupBasic }

  TTestLoaderSetupBasic = class(TTestDummyImageLoader)
  public
    constructor Create; override;
    procedure  PoissonTestFrame;
  public
    SectionDbgInfo: TTestDummySectionInfoEntries;

    Unitdwarfsetupbasic_lpr_0, VarVARENUM0_1, VarVARENUM1_2, VarVARENUM2_3, VarVARENUM3_4, VarVARENUMX_5, VarVARSET0_6, VarVARSET1_7, VarVARSET2_8, VarVARSET3_9, VarVARSETX1_10, Progmain_11, TypeDeclTENUM0_12, TypeTENUM0_13, TypeE0A_14, Type_15, TypeDeclTENUM1_16, TypeTENUM1_17, TypeE1A_18, TypeE1B_19, TypeE1C_20, Type_21, TypeDeclTENUM2_22, TypeTENUM2_23, TypeE2A_24, TypeE2B_25, TypeE2C_26, TypeE2D_27, TypeE2E_28, TypeE2F_29, TypeE2G_30, TypeE2H_31, TypeE2I_32, Type_33, TypeDeclTENUM3_34, TypeTENUM3_35, TypeE3A_36, TypeE3B_37, TypeE3C_38, TypeE3D_39, TypeE3E_40, TypeE3F_41, TypeE3G_42, TypeE3H_43, TypeE3I_44, TypeE3J_45, TypeE3K_46, TypeE3L_47, TypeE3M_48, TypeE3N_49, TypeE3O_50, TypeE3P_51, TypeE3Q_52, Type_53, TypeDeclTENUMX_54, TypeTENUMX_55, TypeEXA_56, TypeEXB_57, TypeEXC_58, Type_59, TypeDeclTSET0_60, TypeTSET0_61, Type_62, Type_63, TypeDeclTSET1_64, TypeTSET1_65, Type_66, Type_67, TypeDeclTSET2_68, TypeTSET2_69, Type_70, Type_71, TypeDeclTSET3_72, TypeTSET3_73, Type_74, Type_75, TypeDeclTSETX1_76, TypeTSETX1_77, Type_78, Type_79, Type_80, TypeS1A_81, TypeS1B_82, TypeS1C_83, Type_84
    : TTestDwarfInfoEntry;

    // global vars
    GlobalVar: record
      PAD_Before: QWord; // padding will be filled with bad data

      VarEnum0, PAD_VarEnum0: TEnum0;
      VarEnum1, PAD_VarEnum1: TEnum1;
      VarEnum2, PAD_VarEnum2: TEnum2;
      VarEnum3, PAD_VarEnum3: TEnum3;
      VarEnumX, PAD_VarEnumX: TEnumX;

      VarSet0, PAD_VarSet0: TSet0;
      VarSet1, PAD_VarSet1: TSet1;
      VarSet2, PAD_VarSet2: TSet2;
      VarSet3, PAD_VarSet3: TSet3;
      VarSetX1, PAD_VarSetX1: TSetX1;

      PAD_After: QWord;
    end;

  end;


implementation

{ TTestLoaderSetup1 }

constructor TTestLoaderSetupBasic.Create;
//var
//  StackOffs: LongInt;
begin
  inherited Create;
  PoissonTestFrame;

  SectionDbgInfo := TestImgReader.TestSection['.debug_info'] as TTestDummySectionInfoEntries;
  Unitdwarfsetupbasic_lpr_0 := SectionDbgInfo.GetFirstInfoEntryObj;

  // Generated with fpc 2.6.2 32 bit win

Unitdwarfsetupbasic_lpr_0.Tag := DW_TAG_compile_unit;
Unitdwarfsetupbasic_lpr_0.Children := 1;
Unitdwarfsetupbasic_lpr_0.Add(DW_AT_name, DW_FORM_string, 'dwarfsetupbasic.lpr'+#0);
Unitdwarfsetupbasic_lpr_0.Add(DW_AT_producer, DW_FORM_string, 'Free Pascal 2.6.2 2013/02/16'+#0);
Unitdwarfsetupbasic_lpr_0.Add(DW_AT_comp_dir, DW_FORM_string, 'B:/lazarus_latest/components/fpdebug/test/testdata/'+#0);
Unitdwarfsetupbasic_lpr_0.Add(DW_AT_language, DW_FORM_data1, [$09]);
Unitdwarfsetupbasic_lpr_0.Add(DW_AT_identifier_case, DW_FORM_data1, [$03]);
Unitdwarfsetupbasic_lpr_0.Add(DW_AT_stmt_list, DW_FORM_data4, [$00, $00, $00, $00]);
Unitdwarfsetupbasic_lpr_0.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00400000);
Unitdwarfsetupbasic_lpr_0.AddAddr(DW_AT_high_pc, DW_FORM_addr, $004FFFFF);

  VarVARENUM0_1 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARENUM0_1.Tag := DW_TAG_variable;
  VarVARENUM0_1.Children := 0;
  VarVARENUM0_1.Add(DW_AT_name, DW_FORM_string, 'VARENUM0'+#0);
  VarVARENUM0_1.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarENUM0)])); // $03, $00, $90, $40, $00
  VarVARENUM0_1.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM0_12); // $55, $01, $00, $00

  VarVARENUM1_2 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARENUM1_2.Tag := DW_TAG_variable;
  VarVARENUM1_2.Children := 0;
  VarVARENUM1_2.Add(DW_AT_name, DW_FORM_string, 'VARENUM1'+#0);
  VarVARENUM1_2.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarENUM1)])); // $03, $10, $90, $40, $00
  VarVARENUM1_2.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM1_16); // $79, $01, $00, $00

  VarVARENUM2_3 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARENUM2_3.Tag := DW_TAG_variable;
  VarVARENUM2_3.Children := 0;
  VarVARENUM2_3.Add(DW_AT_name, DW_FORM_string, 'VARENUM2'+#0);
  VarVARENUM2_3.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarENUM2)])); // $03, $20, $90, $40, $00
  VarVARENUM2_3.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM2_22); // $AF, $01, $00, $00

  VarVARENUM3_4 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARENUM3_4.Tag := DW_TAG_variable;
  VarVARENUM3_4.Children := 0;
  VarVARENUM3_4.Add(DW_AT_name, DW_FORM_string, 'VARENUM3'+#0);
  VarVARENUM3_4.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarENUM3)])); // $03, $30, $90, $40, $00
  VarVARENUM3_4.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM3_34); // $1B, $02, $00, $00

  VarVARENUMX_5 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARENUMX_5.Tag := DW_TAG_variable;
  VarVARENUMX_5.Children := 0;
  VarVARENUMX_5.Add(DW_AT_name, DW_FORM_string, 'VARENUMX'+#0);
  VarVARENUMX_5.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarENUMX)])); // $03, $40, $90, $40, $00
  VarVARENUMX_5.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMX_54); // $CF, $02, $00, $00

  VarVARSET0_6 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSET0_6.Tag := DW_TAG_variable;
  VarVARSET0_6.Children := 0;
  VarVARSET0_6.Add(DW_AT_name, DW_FORM_string, 'VARSET0'+#0);
  VarVARSET0_6.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSET0)])); // $03, $50, $90, $40, $00
  VarVARSET0_6.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET0_60); // $05, $03, $00, $00

  VarVARSET1_7 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSET1_7.Tag := DW_TAG_variable;
  VarVARSET1_7.Children := 0;
  VarVARSET1_7.Add(DW_AT_name, DW_FORM_string, 'VARSET1'+#0);
  VarVARSET1_7.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSET1)])); // $03, $60, $90, $40, $00
  VarVARSET1_7.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET1_64); // $29, $03, $00, $00

  VarVARSET2_8 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSET2_8.Tag := DW_TAG_variable;
  VarVARSET2_8.Children := 0;
  VarVARSET2_8.Add(DW_AT_name, DW_FORM_string, 'VARSET2'+#0);
  VarVARSET2_8.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSET2)])); // $03, $70, $90, $40, $00
  VarVARSET2_8.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET2_68); // $4D, $03, $00, $00

  VarVARSET3_9 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSET3_9.Tag := DW_TAG_variable;
  VarVARSET3_9.Children := 0;
  VarVARSET3_9.Add(DW_AT_name, DW_FORM_string, 'VARSET3'+#0);
  VarVARSET3_9.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSET3)])); // $03, $80, $90, $40, $00
  VarVARSET3_9.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET3_72); // $71, $03, $00, $00

  VarVARSETX1_10 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSETX1_10.Tag := DW_TAG_variable;
  VarVARSETX1_10.Children := 0;
  VarVARSETX1_10.Add(DW_AT_name, DW_FORM_string, 'VARSETX1'+#0);
  VarVARSETX1_10.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSETX1)])); // $03, $90, $90, $40, $00
  VarVARSETX1_10.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETX1_76); // $95, $03, $00, $00

  Progmain_11 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Progmain_11.Tag := DW_TAG_subprogram;
  Progmain_11.Children := 0;
  Progmain_11.Add(DW_AT_name, DW_FORM_string, 'main'+#0);
  Progmain_11.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
  Progmain_11.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
  Progmain_11.Add(DW_AT_external, DW_FORM_flag, [$01]);
  Progmain_11.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00400000);
  Progmain_11.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00400FFF);

  TypeDeclTENUM0_12 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTENUM0_12.Tag := DW_TAG_typedef;
  TypeDeclTENUM0_12.Children := 0;
  TypeDeclTENUM0_12.Add(DW_AT_name, DW_FORM_string, 'TENUM0'+#0);
  TypeDeclTENUM0_12.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTENUM0_13); // $61, $01, $00, $00

  TypeTENUM0_13 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTENUM0_13.Tag := DW_TAG_enumeration_type;
  TypeTENUM0_13.Children := 1;
  TypeTENUM0_13.Add(DW_AT_name, DW_FORM_string, 'TENUM0'+#0);
  TypeTENUM0_13.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeE0A_14 := TypeTENUM0_13.GetNewChild;
    TypeE0A_14.Tag := DW_TAG_enumerator;
    TypeE0A_14.Children := 0;
    TypeE0A_14.Add(DW_AT_name, DW_FORM_string, 'E0A'+#0);
    TypeE0A_14.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

  Type_15 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_15.Tag := DW_TAG_reference_type;
  Type_15.Children := 0;
  Type_15.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM0_12); // $55, $01, $00, $00

  TypeDeclTENUM1_16 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTENUM1_16.Tag := DW_TAG_typedef;
  TypeDeclTENUM1_16.Children := 0;
  TypeDeclTENUM1_16.Add(DW_AT_name, DW_FORM_string, 'TENUM1'+#0);
  TypeDeclTENUM1_16.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTENUM1_17); // $85, $01, $00, $00

  TypeTENUM1_17 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTENUM1_17.Tag := DW_TAG_enumeration_type;
  TypeTENUM1_17.Children := 1;
  TypeTENUM1_17.Add(DW_AT_name, DW_FORM_string, 'TENUM1'+#0);
  TypeTENUM1_17.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeE1A_18 := TypeTENUM1_17.GetNewChild;
    TypeE1A_18.Tag := DW_TAG_enumerator;
    TypeE1A_18.Children := 0;
    TypeE1A_18.Add(DW_AT_name, DW_FORM_string, 'E1A'+#0);
    TypeE1A_18.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeE1B_19 := TypeTENUM1_17.GetNewChild;
    TypeE1B_19.Tag := DW_TAG_enumerator;
    TypeE1B_19.Children := 0;
    TypeE1B_19.Add(DW_AT_name, DW_FORM_string, 'E1B'+#0);
    TypeE1B_19.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeE1C_20 := TypeTENUM1_17.GetNewChild;
    TypeE1C_20.Tag := DW_TAG_enumerator;
    TypeE1C_20.Children := 0;
    TypeE1C_20.Add(DW_AT_name, DW_FORM_string, 'E1C'+#0);
    TypeE1C_20.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

  Type_21 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_21.Tag := DW_TAG_reference_type;
  Type_21.Children := 0;
  Type_21.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM1_16); // $79, $01, $00, $00

  TypeDeclTENUM2_22 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTENUM2_22.Tag := DW_TAG_typedef;
  TypeDeclTENUM2_22.Children := 0;
  TypeDeclTENUM2_22.Add(DW_AT_name, DW_FORM_string, 'TENUM2'+#0);
  TypeDeclTENUM2_22.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTENUM2_23); // $BB, $01, $00, $00

  TypeTENUM2_23 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTENUM2_23.Tag := DW_TAG_enumeration_type;
  TypeTENUM2_23.Children := 1;
  TypeTENUM2_23.Add(DW_AT_name, DW_FORM_string, 'TENUM2'+#0);
  TypeTENUM2_23.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeE2A_24 := TypeTENUM2_23.GetNewChild;
    TypeE2A_24.Tag := DW_TAG_enumerator;
    TypeE2A_24.Children := 0;
    TypeE2A_24.Add(DW_AT_name, DW_FORM_string, 'E2A'+#0);
    TypeE2A_24.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeE2B_25 := TypeTENUM2_23.GetNewChild;
    TypeE2B_25.Tag := DW_TAG_enumerator;
    TypeE2B_25.Children := 0;
    TypeE2B_25.Add(DW_AT_name, DW_FORM_string, 'E2B'+#0);
    TypeE2B_25.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeE2C_26 := TypeTENUM2_23.GetNewChild;
    TypeE2C_26.Tag := DW_TAG_enumerator;
    TypeE2C_26.Children := 0;
    TypeE2C_26.Add(DW_AT_name, DW_FORM_string, 'E2C'+#0);
    TypeE2C_26.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

    TypeE2D_27 := TypeTENUM2_23.GetNewChild;
    TypeE2D_27.Tag := DW_TAG_enumerator;
    TypeE2D_27.Children := 0;
    TypeE2D_27.Add(DW_AT_name, DW_FORM_string, 'E2D'+#0);
    TypeE2D_27.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

    TypeE2E_28 := TypeTENUM2_23.GetNewChild;
    TypeE2E_28.Tag := DW_TAG_enumerator;
    TypeE2E_28.Children := 0;
    TypeE2E_28.Add(DW_AT_name, DW_FORM_string, 'E2E'+#0);
    TypeE2E_28.Add(DW_AT_const_value, DW_FORM_data4, [$04, $00, $00, $00]);

    TypeE2F_29 := TypeTENUM2_23.GetNewChild;
    TypeE2F_29.Tag := DW_TAG_enumerator;
    TypeE2F_29.Children := 0;
    TypeE2F_29.Add(DW_AT_name, DW_FORM_string, 'E2F'+#0);
    TypeE2F_29.Add(DW_AT_const_value, DW_FORM_data4, [$05, $00, $00, $00]);

    TypeE2G_30 := TypeTENUM2_23.GetNewChild;
    TypeE2G_30.Tag := DW_TAG_enumerator;
    TypeE2G_30.Children := 0;
    TypeE2G_30.Add(DW_AT_name, DW_FORM_string, 'E2G'+#0);
    TypeE2G_30.Add(DW_AT_const_value, DW_FORM_data4, [$06, $00, $00, $00]);

    TypeE2H_31 := TypeTENUM2_23.GetNewChild;
    TypeE2H_31.Tag := DW_TAG_enumerator;
    TypeE2H_31.Children := 0;
    TypeE2H_31.Add(DW_AT_name, DW_FORM_string, 'E2H'+#0);
    TypeE2H_31.Add(DW_AT_const_value, DW_FORM_data4, [$07, $00, $00, $00]);

    TypeE2I_32 := TypeTENUM2_23.GetNewChild;
    TypeE2I_32.Tag := DW_TAG_enumerator;
    TypeE2I_32.Children := 0;
    TypeE2I_32.Add(DW_AT_name, DW_FORM_string, 'E2I'+#0);
    TypeE2I_32.Add(DW_AT_const_value, DW_FORM_data4, [$08, $00, $00, $00]);

  Type_33 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_33.Tag := DW_TAG_reference_type;
  Type_33.Children := 0;
  Type_33.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM2_22); // $AF, $01, $00, $00

  TypeDeclTENUM3_34 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTENUM3_34.Tag := DW_TAG_typedef;
  TypeDeclTENUM3_34.Children := 0;
  TypeDeclTENUM3_34.Add(DW_AT_name, DW_FORM_string, 'TENUM3'+#0);
  TypeDeclTENUM3_34.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTENUM3_35); // $27, $02, $00, $00

  TypeTENUM3_35 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTENUM3_35.Tag := DW_TAG_enumeration_type;
  TypeTENUM3_35.Children := 1;
  TypeTENUM3_35.Add(DW_AT_name, DW_FORM_string, 'TENUM3'+#0);
  TypeTENUM3_35.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeE3A_36 := TypeTENUM3_35.GetNewChild;
    TypeE3A_36.Tag := DW_TAG_enumerator;
    TypeE3A_36.Children := 0;
    TypeE3A_36.Add(DW_AT_name, DW_FORM_string, 'E3A'+#0);
    TypeE3A_36.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeE3B_37 := TypeTENUM3_35.GetNewChild;
    TypeE3B_37.Tag := DW_TAG_enumerator;
    TypeE3B_37.Children := 0;
    TypeE3B_37.Add(DW_AT_name, DW_FORM_string, 'E3B'+#0);
    TypeE3B_37.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeE3C_38 := TypeTENUM3_35.GetNewChild;
    TypeE3C_38.Tag := DW_TAG_enumerator;
    TypeE3C_38.Children := 0;
    TypeE3C_38.Add(DW_AT_name, DW_FORM_string, 'E3C'+#0);
    TypeE3C_38.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

    TypeE3D_39 := TypeTENUM3_35.GetNewChild;
    TypeE3D_39.Tag := DW_TAG_enumerator;
    TypeE3D_39.Children := 0;
    TypeE3D_39.Add(DW_AT_name, DW_FORM_string, 'E3D'+#0);
    TypeE3D_39.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

    TypeE3E_40 := TypeTENUM3_35.GetNewChild;
    TypeE3E_40.Tag := DW_TAG_enumerator;
    TypeE3E_40.Children := 0;
    TypeE3E_40.Add(DW_AT_name, DW_FORM_string, 'E3E'+#0);
    TypeE3E_40.Add(DW_AT_const_value, DW_FORM_data4, [$04, $00, $00, $00]);

    TypeE3F_41 := TypeTENUM3_35.GetNewChild;
    TypeE3F_41.Tag := DW_TAG_enumerator;
    TypeE3F_41.Children := 0;
    TypeE3F_41.Add(DW_AT_name, DW_FORM_string, 'E3F'+#0);
    TypeE3F_41.Add(DW_AT_const_value, DW_FORM_data4, [$05, $00, $00, $00]);

    TypeE3G_42 := TypeTENUM3_35.GetNewChild;
    TypeE3G_42.Tag := DW_TAG_enumerator;
    TypeE3G_42.Children := 0;
    TypeE3G_42.Add(DW_AT_name, DW_FORM_string, 'E3G'+#0);
    TypeE3G_42.Add(DW_AT_const_value, DW_FORM_data4, [$06, $00, $00, $00]);

    TypeE3H_43 := TypeTENUM3_35.GetNewChild;
    TypeE3H_43.Tag := DW_TAG_enumerator;
    TypeE3H_43.Children := 0;
    TypeE3H_43.Add(DW_AT_name, DW_FORM_string, 'E3H'+#0);
    TypeE3H_43.Add(DW_AT_const_value, DW_FORM_data4, [$07, $00, $00, $00]);

    TypeE3I_44 := TypeTENUM3_35.GetNewChild;
    TypeE3I_44.Tag := DW_TAG_enumerator;
    TypeE3I_44.Children := 0;
    TypeE3I_44.Add(DW_AT_name, DW_FORM_string, 'E3I'+#0);
    TypeE3I_44.Add(DW_AT_const_value, DW_FORM_data4, [$08, $00, $00, $00]);

    TypeE3J_45 := TypeTENUM3_35.GetNewChild;
    TypeE3J_45.Tag := DW_TAG_enumerator;
    TypeE3J_45.Children := 0;
    TypeE3J_45.Add(DW_AT_name, DW_FORM_string, 'E3J'+#0);
    TypeE3J_45.Add(DW_AT_const_value, DW_FORM_data4, [$09, $00, $00, $00]);

    TypeE3K_46 := TypeTENUM3_35.GetNewChild;
    TypeE3K_46.Tag := DW_TAG_enumerator;
    TypeE3K_46.Children := 0;
    TypeE3K_46.Add(DW_AT_name, DW_FORM_string, 'E3K'+#0);
    TypeE3K_46.Add(DW_AT_const_value, DW_FORM_data4, [$0A, $00, $00, $00]);

    TypeE3L_47 := TypeTENUM3_35.GetNewChild;
    TypeE3L_47.Tag := DW_TAG_enumerator;
    TypeE3L_47.Children := 0;
    TypeE3L_47.Add(DW_AT_name, DW_FORM_string, 'E3L'+#0);
    TypeE3L_47.Add(DW_AT_const_value, DW_FORM_data4, [$0B, $00, $00, $00]);

    TypeE3M_48 := TypeTENUM3_35.GetNewChild;
    TypeE3M_48.Tag := DW_TAG_enumerator;
    TypeE3M_48.Children := 0;
    TypeE3M_48.Add(DW_AT_name, DW_FORM_string, 'E3M'+#0);
    TypeE3M_48.Add(DW_AT_const_value, DW_FORM_data4, [$0C, $00, $00, $00]);

    TypeE3N_49 := TypeTENUM3_35.GetNewChild;
    TypeE3N_49.Tag := DW_TAG_enumerator;
    TypeE3N_49.Children := 0;
    TypeE3N_49.Add(DW_AT_name, DW_FORM_string, 'E3N'+#0);
    TypeE3N_49.Add(DW_AT_const_value, DW_FORM_data4, [$0D, $00, $00, $00]);

    TypeE3O_50 := TypeTENUM3_35.GetNewChild;
    TypeE3O_50.Tag := DW_TAG_enumerator;
    TypeE3O_50.Children := 0;
    TypeE3O_50.Add(DW_AT_name, DW_FORM_string, 'E3O'+#0);
    TypeE3O_50.Add(DW_AT_const_value, DW_FORM_data4, [$0E, $00, $00, $00]);

    TypeE3P_51 := TypeTENUM3_35.GetNewChild;
    TypeE3P_51.Tag := DW_TAG_enumerator;
    TypeE3P_51.Children := 0;
    TypeE3P_51.Add(DW_AT_name, DW_FORM_string, 'E3P'+#0);
    TypeE3P_51.Add(DW_AT_const_value, DW_FORM_data4, [$0F, $00, $00, $00]);

    TypeE3Q_52 := TypeTENUM3_35.GetNewChild;
    TypeE3Q_52.Tag := DW_TAG_enumerator;
    TypeE3Q_52.Children := 0;
    TypeE3Q_52.Add(DW_AT_name, DW_FORM_string, 'E3Q'+#0);
    TypeE3Q_52.Add(DW_AT_const_value, DW_FORM_data4, [$10, $00, $00, $00]);

  Type_53 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_53.Tag := DW_TAG_reference_type;
  Type_53.Children := 0;
  Type_53.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM3_34); // $1B, $02, $00, $00

  TypeDeclTENUMX_54 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTENUMX_54.Tag := DW_TAG_typedef;
  TypeDeclTENUMX_54.Children := 0;
  TypeDeclTENUMX_54.Add(DW_AT_name, DW_FORM_string, 'TENUMX'+#0);
  TypeDeclTENUMX_54.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTENUMX_55); // $DB, $02, $00, $00

  TypeTENUMX_55 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTENUMX_55.Tag := DW_TAG_enumeration_type;
  TypeTENUMX_55.Children := 1;
  TypeTENUMX_55.Add(DW_AT_name, DW_FORM_string, 'TENUMX'+#0);
  TypeTENUMX_55.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeEXA_56 := TypeTENUMX_55.GetNewChild;
    TypeEXA_56.Tag := DW_TAG_enumerator;
    TypeEXA_56.Children := 0;
    TypeEXA_56.Add(DW_AT_name, DW_FORM_string, 'EXA'+#0);
    TypeEXA_56.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeEXB_57 := TypeTENUMX_55.GetNewChild;
    TypeEXB_57.Tag := DW_TAG_enumerator;
    TypeEXB_57.Children := 0;
    TypeEXB_57.Add(DW_AT_name, DW_FORM_string, 'EXB'+#0);
    TypeEXB_57.Add(DW_AT_const_value, DW_FORM_data4, [$0A, $00, $00, $00]);

    TypeEXC_58 := TypeTENUMX_55.GetNewChild;
    TypeEXC_58.Tag := DW_TAG_enumerator;
    TypeEXC_58.Children := 0;
    TypeEXC_58.Add(DW_AT_name, DW_FORM_string, 'EXC'+#0);
    TypeEXC_58.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

  Type_59 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_59.Tag := DW_TAG_reference_type;
  Type_59.Children := 0;
  Type_59.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMX_54); // $CF, $02, $00, $00

  TypeDeclTSET0_60 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSET0_60.Tag := DW_TAG_typedef;
  TypeDeclTSET0_60.Children := 0;
  TypeDeclTSET0_60.Add(DW_AT_name, DW_FORM_string, 'TSET0'+#0);
  TypeDeclTSET0_60.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSET0_61); // $10, $03, $00, $00

  TypeTSET0_61 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSET0_61.Tag := DW_TAG_set_type;
  TypeTSET0_61.Children := 0;
  TypeTSET0_61.Add(DW_AT_name, DW_FORM_string, 'TSET0'+#0);
  TypeTSET0_61.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00, $1D]);
  TypeTSET0_61.AddRef(DW_AT_type, DW_FORM_ref4, @Type_62); // $1D, $03, $00, $00

  Type_62 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_62.Tag := DW_TAG_subrange_type;
  Type_62.Children := 0;
  Type_62.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_62.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 0);
  Type_62.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM0_12); // $55, $01, $00, $00

  Type_63 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_63.Tag := DW_TAG_reference_type;
  Type_63.Children := 0;
  Type_63.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET0_60); // $05, $03, $00, $00

  TypeDeclTSET1_64 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSET1_64.Tag := DW_TAG_typedef;
  TypeDeclTSET1_64.Children := 0;
  TypeDeclTSET1_64.Add(DW_AT_name, DW_FORM_string, 'TSET1'+#0);
  TypeDeclTSET1_64.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSET1_65); // $34, $03, $00, $00

  TypeTSET1_65 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSET1_65.Tag := DW_TAG_set_type;
  TypeTSET1_65.Children := 0;
  TypeTSET1_65.Add(DW_AT_name, DW_FORM_string, 'TSET1'+#0);
  TypeTSET1_65.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00, $41]);
  TypeTSET1_65.AddRef(DW_AT_type, DW_FORM_ref4, @Type_66); // $41, $03, $00, $00

  Type_66 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_66.Tag := DW_TAG_subrange_type;
  Type_66.Children := 0;
  Type_66.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_66.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 2);
  Type_66.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM1_16); // $79, $01, $00, $00

  Type_67 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_67.Tag := DW_TAG_reference_type;
  Type_67.Children := 0;
  Type_67.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET1_64); // $29, $03, $00, $00

  TypeDeclTSET2_68 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSET2_68.Tag := DW_TAG_typedef;
  TypeDeclTSET2_68.Children := 0;
  TypeDeclTSET2_68.Add(DW_AT_name, DW_FORM_string, 'TSET2'+#0);
  TypeDeclTSET2_68.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSET2_69); // $58, $03, $00, $00

  TypeTSET2_69 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSET2_69.Tag := DW_TAG_set_type;
  TypeTSET2_69.Children := 0;
  TypeTSET2_69.Add(DW_AT_name, DW_FORM_string, 'TSET2'+#0);
  TypeTSET2_69.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00, $65]);
  TypeTSET2_69.AddRef(DW_AT_type, DW_FORM_ref4, @Type_70); // $65, $03, $00, $00

  Type_70 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_70.Tag := DW_TAG_subrange_type;
  Type_70.Children := 0;
  Type_70.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_70.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 8);
  Type_70.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM2_22); // $AF, $01, $00, $00

  Type_71 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_71.Tag := DW_TAG_reference_type;
  Type_71.Children := 0;
  Type_71.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET2_68); // $4D, $03, $00, $00

  TypeDeclTSET3_72 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSET3_72.Tag := DW_TAG_typedef;
  TypeDeclTSET3_72.Children := 0;
  TypeDeclTSET3_72.Add(DW_AT_name, DW_FORM_string, 'TSET3'+#0);
  TypeDeclTSET3_72.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSET3_73); // $7C, $03, $00, $00

  TypeTSET3_73 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSET3_73.Tag := DW_TAG_set_type;
  TypeTSET3_73.Children := 0;
  TypeTSET3_73.Add(DW_AT_name, DW_FORM_string, 'TSET3'+#0);
  TypeTSET3_73.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00, $89]);
  TypeTSET3_73.AddRef(DW_AT_type, DW_FORM_ref4, @Type_74); // $89, $03, $00, $00

  Type_74 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_74.Tag := DW_TAG_subrange_type;
  Type_74.Children := 0;
  Type_74.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_74.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 16);
  Type_74.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM3_34); // $1B, $02, $00, $00

  Type_75 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_75.Tag := DW_TAG_reference_type;
  Type_75.Children := 0;
  Type_75.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET3_72); // $71, $03, $00, $00

  TypeDeclTSETX1_76 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETX1_76.Tag := DW_TAG_typedef;
  TypeDeclTSETX1_76.Children := 0;
  TypeDeclTSETX1_76.Add(DW_AT_name, DW_FORM_string, 'TSETX1'+#0);
  TypeDeclTSETX1_76.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETX1_77); // $A1, $03, $00, $00

  TypeTSETX1_77 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETX1_77.Tag := DW_TAG_set_type;
  TypeTSETX1_77.Children := 0;
  TypeTSETX1_77.Add(DW_AT_name, DW_FORM_string, 'TSETX1'+#0);
  TypeTSETX1_77.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00, $AF]);
  TypeTSETX1_77.AddRef(DW_AT_type, DW_FORM_ref4, @Type_78); // $AF, $03, $00, $00

  Type_78 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_78.Tag := DW_TAG_subrange_type;
  Type_78.Children := 0;
  Type_78.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_78.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 2);
  Type_78.AddRef(DW_AT_type, DW_FORM_ref4, @Type_80); // $BB, $03, $00, $00

  Type_79 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_79.Tag := DW_TAG_reference_type;
  Type_79.Children := 0;
  Type_79.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETX1_76); // $95, $03, $00, $00

  Type_80 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_80.Tag := DW_TAG_enumeration_type;
  Type_80.Children := 1;
  Type_80.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeS1A_81 := Type_80.GetNewChild;
    TypeS1A_81.Tag := DW_TAG_enumerator;
    TypeS1A_81.Children := 0;
    TypeS1A_81.Add(DW_AT_name, DW_FORM_string, 'S1A'+#0);
    TypeS1A_81.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeS1B_82 := Type_80.GetNewChild;
    TypeS1B_82.Tag := DW_TAG_enumerator;
    TypeS1B_82.Children := 0;
    TypeS1B_82.Add(DW_AT_name, DW_FORM_string, 'S1B'+#0);
    TypeS1B_82.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeS1C_83 := Type_80.GetNewChild;
    TypeS1C_83.Tag := DW_TAG_enumerator;
    TypeS1C_83.Children := 0;
    TypeS1C_83.Add(DW_AT_name, DW_FORM_string, 'S1C'+#0);
    TypeS1C_83.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

  Type_84 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_84.Tag := DW_TAG_reference_type;
  Type_84.Children := 0;
  Type_84.AddRef(DW_AT_type, DW_FORM_ref4, @Type_80); // $BB, $03, $00, $00

  //
  SectionDbgInfo.CreateSectionData;
  SectionDbgInfo.AbbrevSection.CreateSectionData;
end;

procedure TTestLoaderSetupBasic.PoissonTestFrame;
begin
  // Ensure any out of bound reads get bad data
  FillByte(GlobalVar, SizeOf(GlobalVar), $D5);

end;

end.

