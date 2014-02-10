unit TestDwarfSetupBasic;

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
  TTestSetupBasicProcMainAddr = $00400000;

type

  {%region  Types defined in the DWARF }
  TEnum0 = (e0a);
  TEnum1 = (e1a, e1b, e1c);
  TEnum2 = (e2a, e2b, e2c, e2d, e2e, e2f, e2g, e2h, e2i);
  TEnum3 = (e3a, e3b, e3c, e3d, e3e, e3f, e3g, e3h,
            e3i, e3j, e3k, e3l, e3m, e3n, e3o, e3p,
            e3q);
  TEnumX = (eXa, eXc := 3, eXb := 10);
  TEnumR3 = e3c..e3l;

  TSet0 = Set of TEnum0;
  TSet1 = Set of TEnum1;
  TSet2 = Set of TEnum2;
  TSet3 = Set of TEnum3;
  TSetX1 = Set of (s1a, s1b, s1c);
  TSetB1 = Set of Byte;
  TSetB2 = Set of 5..80;
  TSetC1 = Set of char;
  TSetR3 = Set of TEnumR3;
  {%endregion}

type
  { TTestDwarfSetupBasic }

  TTestLoaderSetupBasic = class(TTestDummyImageLoader)
  public
    constructor Create; override;
    procedure  PoissonTestFrame;
  public
    SectionDbgInfo: TTestDummySectionInfoEntries;

     Unitdwarfsetupbasic_lpr_0, VarVARENUM0_1, VarVARENUM1_2, VarVARENUM2_3, VarVARENUM3_4, VarVARENUMX_5, VarVARENUMR3_6, VarVARSET0_7, VarVARSET1_8, VarVARSET2_9, VarVARSET3_10, VarVARSETX1_11, VarVARSETX2_12, VarVARSETB1_13, VarVARSETB2_14, VarVARSETC1_15, VarVARSETC2_16, VarVARSETR3_17, VarVARBYTE_18, VarVARWORD_19, VarVARLONG_20, VarVARRECENUM1_21, VarVARRECENUM2_22, VarVARRECSET1_23, VarVARRECSET2_24, VarVARRECSET3_25, VarVARRECSETB1_26, VarVARRECSETB2_27, Progmain_28, TypeDeclBYTE_29, TypeBYTE_30, Type_31, TypeDeclWORD_32, TypeWORD_33, Type_34, TypeDeclLONGWORD_35, TypeLONGWORD_36, Type_37, TypeDeclTENUM0_38, TypeTENUM0_39, TypeE0A_40, Type_41, TypeDeclTENUM1_42, TypeTENUM1_43, TypeE1A_44, TypeE1B_45, TypeE1C_46, Type_47, TypeDeclTENUM2_48, TypeTENUM2_49, TypeE2A_50, TypeE2B_51, TypeE2C_52, TypeE2D_53, TypeE2E_54, TypeE2F_55, TypeE2G_56, TypeE2H_57, TypeE2I_58, Type_59, TypeDeclTENUM3_60, TypeTENUM3_61, TypeE3A_62, TypeE3B_63, TypeE3C_64, TypeE3D_65, TypeE3E_66, TypeE3F_67, TypeE3G_68, TypeE3H_69, TypeE3I_70, TypeE3J_71, TypeE3K_72, TypeE3L_73, TypeE3M_74, TypeE3N_75, TypeE3O_76, TypeE3P_77, TypeE3Q_78, Type_79, TypeDeclTENUMX_80, TypeTENUMX_81, TypeEXA_82, TypeEXC_83, TypeEXB_84, Type_85, TypeDeclTENUMR3_86, TypeTENUMR3_87, TypeE3C_88, TypeE3D_89, TypeE3E_90, TypeE3F_91, TypeE3G_92, TypeE3H_93, TypeE3I_94, TypeE3J_95, TypeE3K_96, TypeE3L_97, Type_98, TypeDeclTSET0_99, TypeTSET0_100, Type_101, Type_102, TypeDeclTSET1_103, TypeTSET1_104, Type_105, Type_106, TypeDeclTSET2_107, TypeTSET2_108, Type_109, Type_110, TypeDeclTSET3_111, TypeTSET3_112, Type_113, Type_114, TypeDeclTSETX1_115, TypeTSETX1_116, Type_117, Type_118, TypeDeclTSETB1_119, TypeTSETB1_120, Type_121, Type_122, TypeDeclTSETB2_123, TypeTSETB2_124, Type_125, Type_126, TypeDeclTSETC1_127, TypeTSETC1_128, Type_129, Type_130, TypeDeclTSETR3_131, TypeTSETR3_132, Type_133, Type_134, Type_135, Type_136, Type_137, Type_138, Type_139, Type_140, Type_141, VarENUM1_142, Type_143, Type_144, VarENUM2_145, Type_146, Type_147, VarSET1_148, Type_149, Type_150, VarSET2_151, Type_152, Type_153, VarSET3_154, Type_155, Type_156, VarSETB1_157, Type_158, Type_159, VarSETB2_160, Type_161, Type_162, TypeS1A_163, TypeS1B_164, TypeS1C_165, Type_166, Type_167, Type_168, Type_169, TypeSXA_170, TypeSXB_171, TypeSXC_172, TypeSXD_173, Type_174, TypeDeclCHAR_175, TypeChar_176, Type_177
    : TTestDwarfInfoEntry;

    // global vars
    GlobalVar: record
      PAD_Before: QWord; // padding will be filled with bad data

      VarEnum0, PAD_VarEnum0: TEnum0;
      VarEnum1, PAD_VarEnum1: TEnum1;
      VarEnum2, PAD_VarEnum2: TEnum2;
      VarEnum3, PAD_VarEnum3: TEnum3;
      VarEnumX, PAD_VarEnumX: TEnumX;
      VarEnumR3, PAD_VarEnumR3: TEnumR3;

      VarSet0, PAD_VarSet0: TSet0;
      VarSet1, PAD_VarSet1: TSet1;
      VarSet2, PAD_VarSet2: TSet2;
      VarSet3, PAD_VarSet3: TSet3;
      VarSetX1, PAD_VarSetX1: TSetX1;

      VarSetX2, PAD_VarSetX2: set of (sxa, sxb, sxc, sxd);
      VarSetB1, PAD_VarSetB1: TSetB1;
      VarSetB2, PAD_VarSetB2: TSetB2;
      VarSetC1, PAD_VarSetC1: TSetC1;
      VarSetC2, PAD_VarSetC2: set of char;
      VarSetR3, PAD_VarSetR3: TSetR3;

      VarByte, PAD_VarByte: Byte;
      VarWord, PAD_VarWord: word;
      VarLong, PAD_VarLong: cardinal;

      VarRecEnum1, PAD_VarRecEnum1: packed record  Enum1: TEnum1;  end;
      VarRecEnum2, PAD_VarRecEnum2: packed record  Enum2: TEnum2;  end;
      VarRecSet1, PAD_VarRecSet1:  packed record  Set1:  TSet1;   end;
      VarRecSet2, PAD_VarRecSet2:  packed record  Set2:  TSet2;   end;
      VarRecSet3, PAD_VarRecSet3:  packed record  Set3:  TSet3;   end;
      VarRecSetB1, PAD_VarRecSetB1: packed record  SetB1: TSetB1;  end;
      VarRecSetB2, PAD_VarRecSetB2: packed record  SetB2: TSetB2;  end;

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
  VarVARENUM0_1.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM0_38); // $06, $03, $00, $00

  VarVARENUM1_2 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARENUM1_2.Tag := DW_TAG_variable;
  VarVARENUM1_2.Children := 0;
  VarVARENUM1_2.Add(DW_AT_name, DW_FORM_string, 'VARENUM1'+#0);
  VarVARENUM1_2.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarENUM1)])); // $03, $10, $90, $40, $00
  VarVARENUM1_2.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM1_42); // $2A, $03, $00, $00

  VarVARENUM2_3 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARENUM2_3.Tag := DW_TAG_variable;
  VarVARENUM2_3.Children := 0;
  VarVARENUM2_3.Add(DW_AT_name, DW_FORM_string, 'VARENUM2'+#0);
  VarVARENUM2_3.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarENUM2)])); // $03, $20, $90, $40, $00
  VarVARENUM2_3.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM2_48); // $60, $03, $00, $00

  VarVARENUM3_4 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARENUM3_4.Tag := DW_TAG_variable;
  VarVARENUM3_4.Children := 0;
  VarVARENUM3_4.Add(DW_AT_name, DW_FORM_string, 'VARENUM3'+#0);
  VarVARENUM3_4.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarENUM3)])); // $03, $30, $90, $40, $00
  VarVARENUM3_4.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM3_60); // $CC, $03, $00, $00

  VarVARENUMX_5 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARENUMX_5.Tag := DW_TAG_variable;
  VarVARENUMX_5.Children := 0;
  VarVARENUMX_5.Add(DW_AT_name, DW_FORM_string, 'VARENUMX'+#0);
  VarVARENUMX_5.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarENUMX)])); // $03, $40, $90, $40, $00
  VarVARENUMX_5.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMX_80); // $80, $04, $00, $00

  VarVARENUMR3_6 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARENUMR3_6.Tag := DW_TAG_variable;
  VarVARENUMR3_6.Children := 0;
  VarVARENUMR3_6.Add(DW_AT_name, DW_FORM_string, 'VARENUMR3'+#0);
  VarVARENUMR3_6.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarENUMR3)])); // $03, $00, $00, $00, $00
  VarVARENUMR3_6.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMR3_86); // $B6, $04, $00, $00

  VarVARSET0_7 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSET0_7.Tag := DW_TAG_variable;
  VarVARSET0_7.Children := 0;
  VarVARSET0_7.Add(DW_AT_name, DW_FORM_string, 'VARSET0'+#0);
  VarVARSET0_7.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSET0)])); // $03, $50, $90, $40, $00
  VarVARSET0_7.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET0_99); // $31, $05, $00, $00

  VarVARSET1_8 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSET1_8.Tag := DW_TAG_variable;
  VarVARSET1_8.Children := 0;
  VarVARSET1_8.Add(DW_AT_name, DW_FORM_string, 'VARSET1'+#0);
  VarVARSET1_8.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSET1)])); // $03, $60, $90, $40, $00
  VarVARSET1_8.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET1_103); // $55, $05, $00, $00

  VarVARSET2_9 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSET2_9.Tag := DW_TAG_variable;
  VarVARSET2_9.Children := 0;
  VarVARSET2_9.Add(DW_AT_name, DW_FORM_string, 'VARSET2'+#0);
  VarVARSET2_9.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSET2)])); // $03, $70, $90, $40, $00
  VarVARSET2_9.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET2_107); // $79, $05, $00, $00

  VarVARSET3_10 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSET3_10.Tag := DW_TAG_variable;
  VarVARSET3_10.Children := 0;
  VarVARSET3_10.Add(DW_AT_name, DW_FORM_string, 'VARSET3'+#0);
  VarVARSET3_10.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSET3)])); // $03, $80, $90, $40, $00
  VarVARSET3_10.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET3_111); // $9D, $05, $00, $00

  VarVARSETX1_11 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSETX1_11.Tag := DW_TAG_variable;
  VarVARSETX1_11.Children := 0;
  VarVARSETX1_11.Add(DW_AT_name, DW_FORM_string, 'VARSETX1'+#0);
  VarVARSETX1_11.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSETX1)])); // $03, $90, $90, $40, $00
  VarVARSETX1_11.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETX1_115); // $C1, $05, $00, $00

  VarVARSETX2_12 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSETX2_12.Tag := DW_TAG_variable;
  VarVARSETX2_12.Children := 0;
  VarVARSETX2_12.Add(DW_AT_name, DW_FORM_string, 'VARSETX2'+#0);
  VarVARSETX2_12.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSETX2)])); // $03, $A0, $90, $40, $00
  VarVARSETX2_12.AddRef(DW_AT_type, DW_FORM_ref4, @Type_135); // $82, $06, $00, $00

  VarVARSETB1_13 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSETB1_13.Tag := DW_TAG_variable;
  VarVARSETB1_13.Children := 0;
  VarVARSETB1_13.Add(DW_AT_name, DW_FORM_string, 'VARSETB1'+#0);
  VarVARSETB1_13.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSETB1)])); // $03, $B0, $90, $40, $00
  VarVARSETB1_13.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETB1_119); // $E7, $05, $00, $00

  VarVARSETB2_14 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSETB2_14.Tag := DW_TAG_variable;
  VarVARSETB2_14.Children := 0;
  VarVARSETB2_14.Add(DW_AT_name, DW_FORM_string, 'VARSETB2'+#0);
  VarVARSETB2_14.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSETB2)])); // $03, $D0, $90, $40, $00
  VarVARSETB2_14.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETB2_123); // $0E, $06, $00, $00

  VarVARSETC1_15 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSETC1_15.Tag := DW_TAG_variable;
  VarVARSETC1_15.Children := 0;
  VarVARSETC1_15.Add(DW_AT_name, DW_FORM_string, 'VARSETC1'+#0);
  VarVARSETC1_15.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSETC1)])); // $03, $F0, $90, $40, $00
  VarVARSETC1_15.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETC1_127); // $35, $06, $00, $00

  VarVARSETC2_16 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSETC2_16.Tag := DW_TAG_variable;
  VarVARSETC2_16.Children := 0;
  VarVARSETC2_16.Add(DW_AT_name, DW_FORM_string, 'VARSETC2'+#0);
  VarVARSETC2_16.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSETC2)])); // $03, $00, $00, $00, $00
  VarVARSETC2_16.AddRef(DW_AT_type, DW_FORM_ref4, @Type_138); // $95, $06, $00, $00

  VarVARSETR3_17 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSETR3_17.Tag := DW_TAG_variable;
  VarVARSETR3_17.Children := 0;
  VarVARSETR3_17.Add(DW_AT_name, DW_FORM_string, 'VARSETR3'+#0);
  VarVARSETR3_17.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSETR3)])); // $03, $00, $00, $00, $00
  VarVARSETR3_17.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETR3_131); // $5C, $06, $00, $00

  VarVARBYTE_18 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARBYTE_18.Tag := DW_TAG_variable;
  VarVARBYTE_18.Children := 0;
  VarVARBYTE_18.Add(DW_AT_name, DW_FORM_string, 'VARBYTE'+#0);
  VarVARBYTE_18.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarBYTE)])); // $03, $10, $91, $40, $00
  VarVARBYTE_18.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_29); // $B9, $02, $00, $00

  VarVARWORD_19 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARWORD_19.Tag := DW_TAG_variable;
  VarVARWORD_19.Children := 0;
  VarVARWORD_19.Add(DW_AT_name, DW_FORM_string, 'VARWORD'+#0);
  VarVARWORD_19.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarWORD)])); // $03, $00, $00, $00, $00
  VarVARWORD_19.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_32); // $D0, $02, $00, $00

  VarVARLONG_20 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARLONG_20.Tag := DW_TAG_variable;
  VarVARLONG_20.Children := 0;
  VarVARLONG_20.Add(DW_AT_name, DW_FORM_string, 'VARLONG'+#0);
  VarVARLONG_20.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarLONG)])); // $03, $00, $00, $00, $00
  VarVARLONG_20.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_35); // $E7, $02, $00, $00

  VarVARRECENUM1_21 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECENUM1_21.Tag := DW_TAG_variable;
  VarVARRECENUM1_21.Children := 0;
  VarVARRECENUM1_21.Add(DW_AT_name, DW_FORM_string, 'VARRECENUM1'+#0);
  VarVARRECENUM1_21.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECENUM1)])); // $03, $20, $91, $40, $00
  VarVARRECENUM1_21.AddRef(DW_AT_type, DW_FORM_ref4, @Type_141); // $A9, $06, $00, $00

  VarVARRECENUM2_22 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECENUM2_22.Tag := DW_TAG_variable;
  VarVARRECENUM2_22.Children := 0;
  VarVARRECENUM2_22.Add(DW_AT_name, DW_FORM_string, 'VARRECENUM2'+#0);
  VarVARRECENUM2_22.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECENUM2)])); // $03, $00, $00, $00, $00
  VarVARRECENUM2_22.AddRef(DW_AT_type, DW_FORM_ref4, @Type_144); // $BF, $06, $00, $00

  VarVARRECSET1_23 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSET1_23.Tag := DW_TAG_variable;
  VarVARRECSET1_23.Children := 0;
  VarVARRECSET1_23.Add(DW_AT_name, DW_FORM_string, 'VARRECSET1'+#0);
  VarVARRECSET1_23.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSET1)])); // $03, $00, $00, $00, $00
  VarVARRECSET1_23.AddRef(DW_AT_type, DW_FORM_ref4, @Type_147); // $D5, $06, $00, $00

  VarVARRECSET2_24 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSET2_24.Tag := DW_TAG_variable;
  VarVARRECSET2_24.Children := 0;
  VarVARRECSET2_24.Add(DW_AT_name, DW_FORM_string, 'VARRECSET2'+#0);
  VarVARRECSET2_24.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSET2)])); // $03, $00, $00, $00, $00
  VarVARRECSET2_24.AddRef(DW_AT_type, DW_FORM_ref4, @Type_150); // $EA, $06, $00, $00

  VarVARRECSET3_25 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSET3_25.Tag := DW_TAG_variable;
  VarVARRECSET3_25.Children := 0;
  VarVARRECSET3_25.Add(DW_AT_name, DW_FORM_string, 'VARRECSET3'+#0);
  VarVARRECSET3_25.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSET3)])); // $03, $00, $00, $00, $00
  VarVARRECSET3_25.AddRef(DW_AT_type, DW_FORM_ref4, @Type_153); // $FF, $06, $00, $00

  VarVARRECSETB1_26 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSETB1_26.Tag := DW_TAG_variable;
  VarVARRECSETB1_26.Children := 0;
  VarVARRECSETB1_26.Add(DW_AT_name, DW_FORM_string, 'VARRECSETB1'+#0);
  VarVARRECSETB1_26.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSETB1)])); // $03, $00, $00, $00, $00
  VarVARRECSETB1_26.AddRef(DW_AT_type, DW_FORM_ref4, @Type_156); // $14, $07, $00, $00

  VarVARRECSETB2_27 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSETB2_27.Tag := DW_TAG_variable;
  VarVARRECSETB2_27.Children := 0;
  VarVARRECSETB2_27.Add(DW_AT_name, DW_FORM_string, 'VARRECSETB2'+#0);
  VarVARRECSETB2_27.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSETB2)])); // $03, $00, $00, $00, $00
  VarVARRECSETB2_27.AddRef(DW_AT_type, DW_FORM_ref4, @Type_159); // $2A, $07, $00, $00

  Progmain_28 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Progmain_28.Tag := DW_TAG_subprogram;
  Progmain_28.Children := 0;
  Progmain_28.Add(DW_AT_name, DW_FORM_string, 'main'+#0);
  Progmain_28.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
  Progmain_28.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
  Progmain_28.Add(DW_AT_external, DW_FORM_flag, [$01]);
  Progmain_28.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00400000);
  Progmain_28.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00400FFF);

  TypeDeclBYTE_29 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclBYTE_29.Tag := DW_TAG_typedef;
  TypeDeclBYTE_29.Children := 0;
  TypeDeclBYTE_29.Add(DW_AT_name, DW_FORM_string, 'BYTE'+#0);
  TypeDeclBYTE_29.AddRef(DW_AT_type, DW_FORM_ref4, @TypeBYTE_30); // $C3, $02, $00, $00

  TypeBYTE_30 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeBYTE_30.Tag := DW_TAG_base_type;
  TypeBYTE_30.Children := 0;
  TypeBYTE_30.Add(DW_AT_name, DW_FORM_string, 'BYTE'+#0);
  TypeBYTE_30.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeBYTE_30.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_31 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_31.Tag := DW_TAG_reference_type;
  Type_31.Children := 0;
  Type_31.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_29); // $B9, $02, $00, $00

  TypeDeclWORD_32 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclWORD_32.Tag := DW_TAG_typedef;
  TypeDeclWORD_32.Children := 0;
  TypeDeclWORD_32.Add(DW_AT_name, DW_FORM_string, 'WORD'+#0);
  TypeDeclWORD_32.AddRef(DW_AT_type, DW_FORM_ref4, @TypeWORD_33); // $DA, $02, $00, $00

  TypeWORD_33 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeWORD_33.Tag := DW_TAG_base_type;
  TypeWORD_33.Children := 0;
  TypeWORD_33.Add(DW_AT_name, DW_FORM_string, 'WORD'+#0);
  TypeWORD_33.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeWORD_33.Add(DW_AT_byte_size, DW_FORM_data1, [$02]);

  Type_34 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_34.Tag := DW_TAG_reference_type;
  Type_34.Children := 0;
  Type_34.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_32); // $D0, $02, $00, $00

  TypeDeclLONGWORD_35 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclLONGWORD_35.Tag := DW_TAG_typedef;
  TypeDeclLONGWORD_35.Children := 0;
  TypeDeclLONGWORD_35.Add(DW_AT_name, DW_FORM_string, 'LONGWORD'+#0);
  TypeDeclLONGWORD_35.AddRef(DW_AT_type, DW_FORM_ref4, @TypeLONGWORD_36); // $F5, $02, $00, $00

  TypeLONGWORD_36 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeLONGWORD_36.Tag := DW_TAG_base_type;
  TypeLONGWORD_36.Children := 0;
  TypeLONGWORD_36.Add(DW_AT_name, DW_FORM_string, 'LONGWORD'+#0);
  TypeLONGWORD_36.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeLONGWORD_36.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

  Type_37 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_37.Tag := DW_TAG_reference_type;
  Type_37.Children := 0;
  Type_37.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_35); // $E7, $02, $00, $00

  TypeDeclTENUM0_38 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTENUM0_38.Tag := DW_TAG_typedef;
  TypeDeclTENUM0_38.Children := 0;
  TypeDeclTENUM0_38.Add(DW_AT_name, DW_FORM_string, 'TENUM0'+#0);
  TypeDeclTENUM0_38.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTENUM0_39); // $12, $03, $00, $00

  TypeTENUM0_39 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTENUM0_39.Tag := DW_TAG_enumeration_type;
  TypeTENUM0_39.Children := 1;
  TypeTENUM0_39.Add(DW_AT_name, DW_FORM_string, 'TENUM0'+#0);
  TypeTENUM0_39.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeE0A_40 := TypeTENUM0_39.GetNewChild;
    TypeE0A_40.Tag := DW_TAG_enumerator;
    TypeE0A_40.Children := 0;
    TypeE0A_40.Add(DW_AT_name, DW_FORM_string, 'E0A'+#0);
    TypeE0A_40.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

  Type_41 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_41.Tag := DW_TAG_reference_type;
  Type_41.Children := 0;
  Type_41.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM0_38); // $06, $03, $00, $00

  TypeDeclTENUM1_42 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTENUM1_42.Tag := DW_TAG_typedef;
  TypeDeclTENUM1_42.Children := 0;
  TypeDeclTENUM1_42.Add(DW_AT_name, DW_FORM_string, 'TENUM1'+#0);
  TypeDeclTENUM1_42.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTENUM1_43); // $36, $03, $00, $00

  TypeTENUM1_43 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTENUM1_43.Tag := DW_TAG_enumeration_type;
  TypeTENUM1_43.Children := 1;
  TypeTENUM1_43.Add(DW_AT_name, DW_FORM_string, 'TENUM1'+#0);
  TypeTENUM1_43.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeE1A_44 := TypeTENUM1_43.GetNewChild;
    TypeE1A_44.Tag := DW_TAG_enumerator;
    TypeE1A_44.Children := 0;
    TypeE1A_44.Add(DW_AT_name, DW_FORM_string, 'E1A'+#0);
    TypeE1A_44.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeE1B_45 := TypeTENUM1_43.GetNewChild;
    TypeE1B_45.Tag := DW_TAG_enumerator;
    TypeE1B_45.Children := 0;
    TypeE1B_45.Add(DW_AT_name, DW_FORM_string, 'E1B'+#0);
    TypeE1B_45.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeE1C_46 := TypeTENUM1_43.GetNewChild;
    TypeE1C_46.Tag := DW_TAG_enumerator;
    TypeE1C_46.Children := 0;
    TypeE1C_46.Add(DW_AT_name, DW_FORM_string, 'E1C'+#0);
    TypeE1C_46.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

  Type_47 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_47.Tag := DW_TAG_reference_type;
  Type_47.Children := 0;
  Type_47.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM1_42); // $2A, $03, $00, $00

  TypeDeclTENUM2_48 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTENUM2_48.Tag := DW_TAG_typedef;
  TypeDeclTENUM2_48.Children := 0;
  TypeDeclTENUM2_48.Add(DW_AT_name, DW_FORM_string, 'TENUM2'+#0);
  TypeDeclTENUM2_48.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTENUM2_49); // $6C, $03, $00, $00

  TypeTENUM2_49 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTENUM2_49.Tag := DW_TAG_enumeration_type;
  TypeTENUM2_49.Children := 1;
  TypeTENUM2_49.Add(DW_AT_name, DW_FORM_string, 'TENUM2'+#0);
  TypeTENUM2_49.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeE2A_50 := TypeTENUM2_49.GetNewChild;
    TypeE2A_50.Tag := DW_TAG_enumerator;
    TypeE2A_50.Children := 0;
    TypeE2A_50.Add(DW_AT_name, DW_FORM_string, 'E2A'+#0);
    TypeE2A_50.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeE2B_51 := TypeTENUM2_49.GetNewChild;
    TypeE2B_51.Tag := DW_TAG_enumerator;
    TypeE2B_51.Children := 0;
    TypeE2B_51.Add(DW_AT_name, DW_FORM_string, 'E2B'+#0);
    TypeE2B_51.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeE2C_52 := TypeTENUM2_49.GetNewChild;
    TypeE2C_52.Tag := DW_TAG_enumerator;
    TypeE2C_52.Children := 0;
    TypeE2C_52.Add(DW_AT_name, DW_FORM_string, 'E2C'+#0);
    TypeE2C_52.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

    TypeE2D_53 := TypeTENUM2_49.GetNewChild;
    TypeE2D_53.Tag := DW_TAG_enumerator;
    TypeE2D_53.Children := 0;
    TypeE2D_53.Add(DW_AT_name, DW_FORM_string, 'E2D'+#0);
    TypeE2D_53.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

    TypeE2E_54 := TypeTENUM2_49.GetNewChild;
    TypeE2E_54.Tag := DW_TAG_enumerator;
    TypeE2E_54.Children := 0;
    TypeE2E_54.Add(DW_AT_name, DW_FORM_string, 'E2E'+#0);
    TypeE2E_54.Add(DW_AT_const_value, DW_FORM_data4, [$04, $00, $00, $00]);

    TypeE2F_55 := TypeTENUM2_49.GetNewChild;
    TypeE2F_55.Tag := DW_TAG_enumerator;
    TypeE2F_55.Children := 0;
    TypeE2F_55.Add(DW_AT_name, DW_FORM_string, 'E2F'+#0);
    TypeE2F_55.Add(DW_AT_const_value, DW_FORM_data4, [$05, $00, $00, $00]);

    TypeE2G_56 := TypeTENUM2_49.GetNewChild;
    TypeE2G_56.Tag := DW_TAG_enumerator;
    TypeE2G_56.Children := 0;
    TypeE2G_56.Add(DW_AT_name, DW_FORM_string, 'E2G'+#0);
    TypeE2G_56.Add(DW_AT_const_value, DW_FORM_data4, [$06, $00, $00, $00]);

    TypeE2H_57 := TypeTENUM2_49.GetNewChild;
    TypeE2H_57.Tag := DW_TAG_enumerator;
    TypeE2H_57.Children := 0;
    TypeE2H_57.Add(DW_AT_name, DW_FORM_string, 'E2H'+#0);
    TypeE2H_57.Add(DW_AT_const_value, DW_FORM_data4, [$07, $00, $00, $00]);

    TypeE2I_58 := TypeTENUM2_49.GetNewChild;
    TypeE2I_58.Tag := DW_TAG_enumerator;
    TypeE2I_58.Children := 0;
    TypeE2I_58.Add(DW_AT_name, DW_FORM_string, 'E2I'+#0);
    TypeE2I_58.Add(DW_AT_const_value, DW_FORM_data4, [$08, $00, $00, $00]);

  Type_59 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_59.Tag := DW_TAG_reference_type;
  Type_59.Children := 0;
  Type_59.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM2_48); // $60, $03, $00, $00

  TypeDeclTENUM3_60 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTENUM3_60.Tag := DW_TAG_typedef;
  TypeDeclTENUM3_60.Children := 0;
  TypeDeclTENUM3_60.Add(DW_AT_name, DW_FORM_string, 'TENUM3'+#0);
  TypeDeclTENUM3_60.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTENUM3_61); // $D8, $03, $00, $00

  TypeTENUM3_61 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTENUM3_61.Tag := DW_TAG_enumeration_type;
  TypeTENUM3_61.Children := 1;
  TypeTENUM3_61.Add(DW_AT_name, DW_FORM_string, 'TENUM3'+#0);
  TypeTENUM3_61.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeE3A_62 := TypeTENUM3_61.GetNewChild;
    TypeE3A_62.Tag := DW_TAG_enumerator;
    TypeE3A_62.Children := 0;
    TypeE3A_62.Add(DW_AT_name, DW_FORM_string, 'E3A'+#0);
    TypeE3A_62.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeE3B_63 := TypeTENUM3_61.GetNewChild;
    TypeE3B_63.Tag := DW_TAG_enumerator;
    TypeE3B_63.Children := 0;
    TypeE3B_63.Add(DW_AT_name, DW_FORM_string, 'E3B'+#0);
    TypeE3B_63.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeE3C_64 := TypeTENUM3_61.GetNewChild;
    TypeE3C_64.Tag := DW_TAG_enumerator;
    TypeE3C_64.Children := 0;
    TypeE3C_64.Add(DW_AT_name, DW_FORM_string, 'E3C'+#0);
    TypeE3C_64.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

    TypeE3D_65 := TypeTENUM3_61.GetNewChild;
    TypeE3D_65.Tag := DW_TAG_enumerator;
    TypeE3D_65.Children := 0;
    TypeE3D_65.Add(DW_AT_name, DW_FORM_string, 'E3D'+#0);
    TypeE3D_65.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

    TypeE3E_66 := TypeTENUM3_61.GetNewChild;
    TypeE3E_66.Tag := DW_TAG_enumerator;
    TypeE3E_66.Children := 0;
    TypeE3E_66.Add(DW_AT_name, DW_FORM_string, 'E3E'+#0);
    TypeE3E_66.Add(DW_AT_const_value, DW_FORM_data4, [$04, $00, $00, $00]);

    TypeE3F_67 := TypeTENUM3_61.GetNewChild;
    TypeE3F_67.Tag := DW_TAG_enumerator;
    TypeE3F_67.Children := 0;
    TypeE3F_67.Add(DW_AT_name, DW_FORM_string, 'E3F'+#0);
    TypeE3F_67.Add(DW_AT_const_value, DW_FORM_data4, [$05, $00, $00, $00]);

    TypeE3G_68 := TypeTENUM3_61.GetNewChild;
    TypeE3G_68.Tag := DW_TAG_enumerator;
    TypeE3G_68.Children := 0;
    TypeE3G_68.Add(DW_AT_name, DW_FORM_string, 'E3G'+#0);
    TypeE3G_68.Add(DW_AT_const_value, DW_FORM_data4, [$06, $00, $00, $00]);

    TypeE3H_69 := TypeTENUM3_61.GetNewChild;
    TypeE3H_69.Tag := DW_TAG_enumerator;
    TypeE3H_69.Children := 0;
    TypeE3H_69.Add(DW_AT_name, DW_FORM_string, 'E3H'+#0);
    TypeE3H_69.Add(DW_AT_const_value, DW_FORM_data4, [$07, $00, $00, $00]);

    TypeE3I_70 := TypeTENUM3_61.GetNewChild;
    TypeE3I_70.Tag := DW_TAG_enumerator;
    TypeE3I_70.Children := 0;
    TypeE3I_70.Add(DW_AT_name, DW_FORM_string, 'E3I'+#0);
    TypeE3I_70.Add(DW_AT_const_value, DW_FORM_data4, [$08, $00, $00, $00]);

    TypeE3J_71 := TypeTENUM3_61.GetNewChild;
    TypeE3J_71.Tag := DW_TAG_enumerator;
    TypeE3J_71.Children := 0;
    TypeE3J_71.Add(DW_AT_name, DW_FORM_string, 'E3J'+#0);
    TypeE3J_71.Add(DW_AT_const_value, DW_FORM_data4, [$09, $00, $00, $00]);

    TypeE3K_72 := TypeTENUM3_61.GetNewChild;
    TypeE3K_72.Tag := DW_TAG_enumerator;
    TypeE3K_72.Children := 0;
    TypeE3K_72.Add(DW_AT_name, DW_FORM_string, 'E3K'+#0);
    TypeE3K_72.Add(DW_AT_const_value, DW_FORM_data4, [$0A, $00, $00, $00]);

    TypeE3L_73 := TypeTENUM3_61.GetNewChild;
    TypeE3L_73.Tag := DW_TAG_enumerator;
    TypeE3L_73.Children := 0;
    TypeE3L_73.Add(DW_AT_name, DW_FORM_string, 'E3L'+#0);
    TypeE3L_73.Add(DW_AT_const_value, DW_FORM_data4, [$0B, $00, $00, $00]);

    TypeE3M_74 := TypeTENUM3_61.GetNewChild;
    TypeE3M_74.Tag := DW_TAG_enumerator;
    TypeE3M_74.Children := 0;
    TypeE3M_74.Add(DW_AT_name, DW_FORM_string, 'E3M'+#0);
    TypeE3M_74.Add(DW_AT_const_value, DW_FORM_data4, [$0C, $00, $00, $00]);

    TypeE3N_75 := TypeTENUM3_61.GetNewChild;
    TypeE3N_75.Tag := DW_TAG_enumerator;
    TypeE3N_75.Children := 0;
    TypeE3N_75.Add(DW_AT_name, DW_FORM_string, 'E3N'+#0);
    TypeE3N_75.Add(DW_AT_const_value, DW_FORM_data4, [$0D, $00, $00, $00]);

    TypeE3O_76 := TypeTENUM3_61.GetNewChild;
    TypeE3O_76.Tag := DW_TAG_enumerator;
    TypeE3O_76.Children := 0;
    TypeE3O_76.Add(DW_AT_name, DW_FORM_string, 'E3O'+#0);
    TypeE3O_76.Add(DW_AT_const_value, DW_FORM_data4, [$0E, $00, $00, $00]);

    TypeE3P_77 := TypeTENUM3_61.GetNewChild;
    TypeE3P_77.Tag := DW_TAG_enumerator;
    TypeE3P_77.Children := 0;
    TypeE3P_77.Add(DW_AT_name, DW_FORM_string, 'E3P'+#0);
    TypeE3P_77.Add(DW_AT_const_value, DW_FORM_data4, [$0F, $00, $00, $00]);

    TypeE3Q_78 := TypeTENUM3_61.GetNewChild;
    TypeE3Q_78.Tag := DW_TAG_enumerator;
    TypeE3Q_78.Children := 0;
    TypeE3Q_78.Add(DW_AT_name, DW_FORM_string, 'E3Q'+#0);
    TypeE3Q_78.Add(DW_AT_const_value, DW_FORM_data4, [$10, $00, $00, $00]);

  Type_79 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_79.Tag := DW_TAG_reference_type;
  Type_79.Children := 0;
  Type_79.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM3_60); // $CC, $03, $00, $00

  TypeDeclTENUMX_80 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTENUMX_80.Tag := DW_TAG_typedef;
  TypeDeclTENUMX_80.Children := 0;
  TypeDeclTENUMX_80.Add(DW_AT_name, DW_FORM_string, 'TENUMX'+#0);
  TypeDeclTENUMX_80.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTENUMX_81); // $8C, $04, $00, $00

  TypeTENUMX_81 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTENUMX_81.Tag := DW_TAG_enumeration_type;
  TypeTENUMX_81.Children := 1;
  TypeTENUMX_81.Add(DW_AT_name, DW_FORM_string, 'TENUMX'+#0);
  TypeTENUMX_81.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeEXA_82 := TypeTENUMX_81.GetNewChild;
    TypeEXA_82.Tag := DW_TAG_enumerator;
    TypeEXA_82.Children := 0;
    TypeEXA_82.Add(DW_AT_name, DW_FORM_string, 'EXA'+#0);
    TypeEXA_82.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeEXC_83 := TypeTENUMX_81.GetNewChild;
    TypeEXC_83.Tag := DW_TAG_enumerator;
    TypeEXC_83.Children := 0;
    TypeEXC_83.Add(DW_AT_name, DW_FORM_string, 'EXC'+#0);
    TypeEXC_83.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

    TypeEXB_84 := TypeTENUMX_81.GetNewChild;
    TypeEXB_84.Tag := DW_TAG_enumerator;
    TypeEXB_84.Children := 0;
    TypeEXB_84.Add(DW_AT_name, DW_FORM_string, 'EXB'+#0);
    TypeEXB_84.Add(DW_AT_const_value, DW_FORM_data4, [$0A, $00, $00, $00]);

  Type_85 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_85.Tag := DW_TAG_reference_type;
  Type_85.Children := 0;
  Type_85.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMX_80); // $80, $04, $00, $00

  TypeDeclTENUMR3_86 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTENUMR3_86.Tag := DW_TAG_typedef;
  TypeDeclTENUMR3_86.Children := 0;
  TypeDeclTENUMR3_86.Add(DW_AT_name, DW_FORM_string, 'TENUMR3'+#0);
  TypeDeclTENUMR3_86.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTENUMR3_87); // $C3, $04, $00, $00

  TypeTENUMR3_87 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTENUMR3_87.Tag := DW_TAG_enumeration_type;
  TypeTENUMR3_87.Children := 1;
  TypeTENUMR3_87.Add(DW_AT_name, DW_FORM_string, 'TENUMR3'+#0);
  TypeTENUMR3_87.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);
  TypeTENUMR3_87.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM3_60); // $CC, $03, $00, $00

    TypeE3C_88 := TypeTENUMR3_87.GetNewChild;
    TypeE3C_88.Tag := DW_TAG_enumerator;
    TypeE3C_88.Children := 0;
    TypeE3C_88.Add(DW_AT_name, DW_FORM_string, 'E3C'+#0);
    TypeE3C_88.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

    TypeE3D_89 := TypeTENUMR3_87.GetNewChild;
    TypeE3D_89.Tag := DW_TAG_enumerator;
    TypeE3D_89.Children := 0;
    TypeE3D_89.Add(DW_AT_name, DW_FORM_string, 'E3D'+#0);
    TypeE3D_89.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

    TypeE3E_90 := TypeTENUMR3_87.GetNewChild;
    TypeE3E_90.Tag := DW_TAG_enumerator;
    TypeE3E_90.Children := 0;
    TypeE3E_90.Add(DW_AT_name, DW_FORM_string, 'E3E'+#0);
    TypeE3E_90.Add(DW_AT_const_value, DW_FORM_data4, [$04, $00, $00, $00]);

    TypeE3F_91 := TypeTENUMR3_87.GetNewChild;
    TypeE3F_91.Tag := DW_TAG_enumerator;
    TypeE3F_91.Children := 0;
    TypeE3F_91.Add(DW_AT_name, DW_FORM_string, 'E3F'+#0);
    TypeE3F_91.Add(DW_AT_const_value, DW_FORM_data4, [$05, $00, $00, $00]);

    TypeE3G_92 := TypeTENUMR3_87.GetNewChild;
    TypeE3G_92.Tag := DW_TAG_enumerator;
    TypeE3G_92.Children := 0;
    TypeE3G_92.Add(DW_AT_name, DW_FORM_string, 'E3G'+#0);
    TypeE3G_92.Add(DW_AT_const_value, DW_FORM_data4, [$06, $00, $00, $00]);

    TypeE3H_93 := TypeTENUMR3_87.GetNewChild;
    TypeE3H_93.Tag := DW_TAG_enumerator;
    TypeE3H_93.Children := 0;
    TypeE3H_93.Add(DW_AT_name, DW_FORM_string, 'E3H'+#0);
    TypeE3H_93.Add(DW_AT_const_value, DW_FORM_data4, [$07, $00, $00, $00]);

    TypeE3I_94 := TypeTENUMR3_87.GetNewChild;
    TypeE3I_94.Tag := DW_TAG_enumerator;
    TypeE3I_94.Children := 0;
    TypeE3I_94.Add(DW_AT_name, DW_FORM_string, 'E3I'+#0);
    TypeE3I_94.Add(DW_AT_const_value, DW_FORM_data4, [$08, $00, $00, $00]);

    TypeE3J_95 := TypeTENUMR3_87.GetNewChild;
    TypeE3J_95.Tag := DW_TAG_enumerator;
    TypeE3J_95.Children := 0;
    TypeE3J_95.Add(DW_AT_name, DW_FORM_string, 'E3J'+#0);
    TypeE3J_95.Add(DW_AT_const_value, DW_FORM_data4, [$09, $00, $00, $00]);

    TypeE3K_96 := TypeTENUMR3_87.GetNewChild;
    TypeE3K_96.Tag := DW_TAG_enumerator;
    TypeE3K_96.Children := 0;
    TypeE3K_96.Add(DW_AT_name, DW_FORM_string, 'E3K'+#0);
    TypeE3K_96.Add(DW_AT_const_value, DW_FORM_data4, [$0A, $00, $00, $00]);

    TypeE3L_97 := TypeTENUMR3_87.GetNewChild;
    TypeE3L_97.Tag := DW_TAG_enumerator;
    TypeE3L_97.Children := 0;
    TypeE3L_97.Add(DW_AT_name, DW_FORM_string, 'E3L'+#0);
    TypeE3L_97.Add(DW_AT_const_value, DW_FORM_data4, [$0B, $00, $00, $00]);

  Type_98 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_98.Tag := DW_TAG_reference_type;
  Type_98.Children := 0;
  Type_98.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMR3_86); // $B6, $04, $00, $00

  TypeDeclTSET0_99 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSET0_99.Tag := DW_TAG_typedef;
  TypeDeclTSET0_99.Children := 0;
  TypeDeclTSET0_99.Add(DW_AT_name, DW_FORM_string, 'TSET0'+#0);
  TypeDeclTSET0_99.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSET0_100); // $3C, $05, $00, $00

  TypeTSET0_100 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSET0_100.Tag := DW_TAG_set_type;
  TypeTSET0_100.Children := 0;
  TypeTSET0_100.Add(DW_AT_name, DW_FORM_string, 'TSET0'+#0);
  TypeTSET0_100.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  TypeTSET0_100.AddRef(DW_AT_type, DW_FORM_ref4, @Type_101); // $49, $05, $00, $00

  Type_101 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_101.Tag := DW_TAG_subrange_type;
  Type_101.Children := 0;
  Type_101.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_101.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 0);
  Type_101.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM0_38); // $06, $03, $00, $00

  Type_102 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_102.Tag := DW_TAG_reference_type;
  Type_102.Children := 0;
  Type_102.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET0_99); // $31, $05, $00, $00

  TypeDeclTSET1_103 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSET1_103.Tag := DW_TAG_typedef;
  TypeDeclTSET1_103.Children := 0;
  TypeDeclTSET1_103.Add(DW_AT_name, DW_FORM_string, 'TSET1'+#0);
  TypeDeclTSET1_103.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSET1_104); // $60, $05, $00, $00

  TypeTSET1_104 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSET1_104.Tag := DW_TAG_set_type;
  TypeTSET1_104.Children := 0;
  TypeTSET1_104.Add(DW_AT_name, DW_FORM_string, 'TSET1'+#0);
  TypeTSET1_104.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  TypeTSET1_104.AddRef(DW_AT_type, DW_FORM_ref4, @Type_105); // $6D, $05, $00, $00

  Type_105 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_105.Tag := DW_TAG_subrange_type;
  Type_105.Children := 0;
  Type_105.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_105.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 2);
  Type_105.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM1_42); // $2A, $03, $00, $00

  Type_106 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_106.Tag := DW_TAG_reference_type;
  Type_106.Children := 0;
  Type_106.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET1_103); // $55, $05, $00, $00

  TypeDeclTSET2_107 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSET2_107.Tag := DW_TAG_typedef;
  TypeDeclTSET2_107.Children := 0;
  TypeDeclTSET2_107.Add(DW_AT_name, DW_FORM_string, 'TSET2'+#0);
  TypeDeclTSET2_107.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSET2_108); // $84, $05, $00, $00

  TypeTSET2_108 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSET2_108.Tag := DW_TAG_set_type;
  TypeTSET2_108.Children := 0;
  TypeTSET2_108.Add(DW_AT_name, DW_FORM_string, 'TSET2'+#0);
  TypeTSET2_108.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  TypeTSET2_108.AddRef(DW_AT_type, DW_FORM_ref4, @Type_109); // $91, $05, $00, $00

  Type_109 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_109.Tag := DW_TAG_subrange_type;
  Type_109.Children := 0;
  Type_109.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_109.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 8);
  Type_109.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM2_48); // $60, $03, $00, $00

  Type_110 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_110.Tag := DW_TAG_reference_type;
  Type_110.Children := 0;
  Type_110.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET2_107); // $79, $05, $00, $00

  TypeDeclTSET3_111 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSET3_111.Tag := DW_TAG_typedef;
  TypeDeclTSET3_111.Children := 0;
  TypeDeclTSET3_111.Add(DW_AT_name, DW_FORM_string, 'TSET3'+#0);
  TypeDeclTSET3_111.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSET3_112); // $A8, $05, $00, $00

  TypeTSET3_112 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSET3_112.Tag := DW_TAG_set_type;
  TypeTSET3_112.Children := 0;
  TypeTSET3_112.Add(DW_AT_name, DW_FORM_string, 'TSET3'+#0);
  TypeTSET3_112.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  TypeTSET3_112.AddRef(DW_AT_type, DW_FORM_ref4, @Type_113); // $B5, $05, $00, $00

  Type_113 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_113.Tag := DW_TAG_subrange_type;
  Type_113.Children := 0;
  Type_113.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_113.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 16);
  Type_113.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM3_60); // $CC, $03, $00, $00

  Type_114 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_114.Tag := DW_TAG_reference_type;
  Type_114.Children := 0;
  Type_114.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET3_111); // $9D, $05, $00, $00

  TypeDeclTSETX1_115 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETX1_115.Tag := DW_TAG_typedef;
  TypeDeclTSETX1_115.Children := 0;
  TypeDeclTSETX1_115.Add(DW_AT_name, DW_FORM_string, 'TSETX1'+#0);
  TypeDeclTSETX1_115.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETX1_116); // $CD, $05, $00, $00

  TypeTSETX1_116 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETX1_116.Tag := DW_TAG_set_type;
  TypeTSETX1_116.Children := 0;
  TypeTSETX1_116.Add(DW_AT_name, DW_FORM_string, 'TSETX1'+#0);
  TypeTSETX1_116.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  TypeTSETX1_116.AddRef(DW_AT_type, DW_FORM_ref4, @Type_117); // $DB, $05, $00, $00

  Type_117 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_117.Tag := DW_TAG_subrange_type;
  Type_117.Children := 0;
  Type_117.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_117.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 2);
  Type_117.AddRef(DW_AT_type, DW_FORM_ref4, @Type_162); // $40, $07, $00, $00

  Type_118 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_118.Tag := DW_TAG_reference_type;
  Type_118.Children := 0;
  Type_118.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETX1_115); // $C1, $05, $00, $00

  TypeDeclTSETB1_119 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETB1_119.Tag := DW_TAG_typedef;
  TypeDeclTSETB1_119.Children := 0;
  TypeDeclTSETB1_119.Add(DW_AT_name, DW_FORM_string, 'TSETB1'+#0);
  TypeDeclTSETB1_119.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETB1_120); // $F3, $05, $00, $00

  TypeTSETB1_120 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETB1_120.Tag := DW_TAG_set_type;
  TypeTSETB1_120.Children := 0;
  TypeTSETB1_120.Add(DW_AT_name, DW_FORM_string, 'TSETB1'+#0);
  TypeTSETB1_120.Add(DW_AT_byte_size, DW_FORM_data2, [$20, $00]);
  TypeTSETB1_120.AddRef(DW_AT_type, DW_FORM_ref4, @Type_121); // $01, $06, $00, $00

  Type_121 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_121.Tag := DW_TAG_subrange_type;
  Type_121.Children := 0;
  Type_121.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_121.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 255);
  Type_121.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_29); // $B9, $02, $00, $00

  Type_122 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_122.Tag := DW_TAG_reference_type;
  Type_122.Children := 0;
  Type_122.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETB1_119); // $E7, $05, $00, $00

  TypeDeclTSETB2_123 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETB2_123.Tag := DW_TAG_typedef;
  TypeDeclTSETB2_123.Children := 0;
  TypeDeclTSETB2_123.Add(DW_AT_name, DW_FORM_string, 'TSETB2'+#0);
  TypeDeclTSETB2_123.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETB2_124); // $1A, $06, $00, $00

  TypeTSETB2_124 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETB2_124.Tag := DW_TAG_set_type;
  TypeTSETB2_124.Children := 0;
  TypeTSETB2_124.Add(DW_AT_name, DW_FORM_string, 'TSETB2'+#0);
  TypeTSETB2_124.Add(DW_AT_byte_size, DW_FORM_data2, [$20, $00]);
  TypeTSETB2_124.AddRef(DW_AT_type, DW_FORM_ref4, @Type_125); // $28, $06, $00, $00

  Type_125 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_125.Tag := DW_TAG_subrange_type;
  Type_125.Children := 0;
  Type_125.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_125.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 80);
  Type_125.AddRef(DW_AT_type, DW_FORM_ref4, @Type_167); // $63, $07, $00, $00

  Type_126 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_126.Tag := DW_TAG_reference_type;
  Type_126.Children := 0;
  Type_126.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETB2_123); // $0E, $06, $00, $00

  TypeDeclTSETC1_127 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETC1_127.Tag := DW_TAG_typedef;
  TypeDeclTSETC1_127.Children := 0;
  TypeDeclTSETC1_127.Add(DW_AT_name, DW_FORM_string, 'TSETC1'+#0);
  TypeDeclTSETC1_127.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETC1_128); // $41, $06, $00, $00

  TypeTSETC1_128 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETC1_128.Tag := DW_TAG_set_type;
  TypeTSETC1_128.Children := 0;
  TypeTSETC1_128.Add(DW_AT_name, DW_FORM_string, 'TSETC1'+#0);
  TypeTSETC1_128.Add(DW_AT_byte_size, DW_FORM_data2, [$20, $00]);
  TypeTSETC1_128.AddRef(DW_AT_type, DW_FORM_ref4, @Type_129); // $4F, $06, $00, $00

  Type_129 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_129.Tag := DW_TAG_subrange_type;
  Type_129.Children := 0;
  Type_129.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_129.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 255);
  Type_129.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclCHAR_175); // $9B, $07, $00, $00

  Type_130 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_130.Tag := DW_TAG_reference_type;
  Type_130.Children := 0;
  Type_130.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETC1_127); // $35, $06, $00, $00

  TypeDeclTSETR3_131 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETR3_131.Tag := DW_TAG_typedef;
  TypeDeclTSETR3_131.Children := 0;
  TypeDeclTSETR3_131.Add(DW_AT_name, DW_FORM_string, 'TSETR3'+#0);
  TypeDeclTSETR3_131.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETR3_132); // $68, $06, $00, $00

  TypeTSETR3_132 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETR3_132.Tag := DW_TAG_set_type;
  TypeTSETR3_132.Children := 0;
  TypeTSETR3_132.Add(DW_AT_name, DW_FORM_string, 'TSETR3'+#0);
  TypeTSETR3_132.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  TypeTSETR3_132.AddRef(DW_AT_type, DW_FORM_ref4, @Type_133); // $76, $06, $00, $00

  Type_133 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_133.Tag := DW_TAG_subrange_type;
  Type_133.Children := 0;
  Type_133.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_133.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 11);
  Type_133.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMR3_86); // $B6, $04, $00, $00

  Type_134 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_134.Tag := DW_TAG_reference_type;
  Type_134.Children := 0;
  Type_134.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETR3_131); // $5C, $06, $00, $00

  Type_135 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_135.Tag := DW_TAG_set_type;
  Type_135.Children := 0;
  Type_135.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  Type_135.AddRef(DW_AT_type, DW_FORM_ref4, @Type_136); // $89, $06, $00, $00

  Type_136 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_136.Tag := DW_TAG_subrange_type;
  Type_136.Children := 0;
  Type_136.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_136.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 3);
  Type_136.AddRef(DW_AT_type, DW_FORM_ref4, @Type_169); // $6F, $07, $00, $00

  Type_137 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_137.Tag := DW_TAG_reference_type;
  Type_137.Children := 0;
  Type_137.AddRef(DW_AT_type, DW_FORM_ref4, @Type_135); // $82, $06, $00, $00

  Type_138 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_138.Tag := DW_TAG_set_type;
  Type_138.Children := 0;
  Type_138.Add(DW_AT_byte_size, DW_FORM_data2, [$20, $00]);
  Type_138.AddRef(DW_AT_type, DW_FORM_ref4, @Type_139); // $9C, $06, $00, $00

  Type_139 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_139.Tag := DW_TAG_subrange_type;
  Type_139.Children := 0;
  Type_139.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_139.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 255);
  Type_139.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclCHAR_175); // $9B, $07, $00, $00

  Type_140 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_140.Tag := DW_TAG_reference_type;
  Type_140.Children := 0;
  Type_140.AddRef(DW_AT_type, DW_FORM_ref4, @Type_138); // $95, $06, $00, $00

  Type_141 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_141.Tag := DW_TAG_structure_type;
  Type_141.Children := 1;
  Type_141.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarENUM1_142 := Type_141.GetNewChild;
    VarENUM1_142.Tag := DW_TAG_member;
    VarENUM1_142.Children := 0;
    VarENUM1_142.Add(DW_AT_name, DW_FORM_string, 'ENUM1'+#0);
    VarENUM1_142.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarENUM1_142.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM1_42); // $2A, $03, $00, $00

  Type_143 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_143.Tag := DW_TAG_reference_type;
  Type_143.Children := 0;
  Type_143.AddRef(DW_AT_type, DW_FORM_ref4, @Type_141); // $A9, $06, $00, $00

  Type_144 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_144.Tag := DW_TAG_structure_type;
  Type_144.Children := 1;
  Type_144.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarENUM2_145 := Type_144.GetNewChild;
    VarENUM2_145.Tag := DW_TAG_member;
    VarENUM2_145.Children := 0;
    VarENUM2_145.Add(DW_AT_name, DW_FORM_string, 'ENUM2'+#0);
    VarENUM2_145.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarENUM2_145.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM2_48); // $60, $03, $00, $00

  Type_146 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_146.Tag := DW_TAG_reference_type;
  Type_146.Children := 0;
  Type_146.AddRef(DW_AT_type, DW_FORM_ref4, @Type_144); // $BF, $06, $00, $00

  Type_147 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_147.Tag := DW_TAG_structure_type;
  Type_147.Children := 1;
  Type_147.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarSET1_148 := Type_147.GetNewChild;
    VarSET1_148.Tag := DW_TAG_member;
    VarSET1_148.Children := 0;
    VarSET1_148.Add(DW_AT_name, DW_FORM_string, 'SET1'+#0);
    VarSET1_148.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarSET1_148.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET1_103); // $55, $05, $00, $00

  Type_149 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_149.Tag := DW_TAG_reference_type;
  Type_149.Children := 0;
  Type_149.AddRef(DW_AT_type, DW_FORM_ref4, @Type_147); // $D5, $06, $00, $00

  Type_150 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_150.Tag := DW_TAG_structure_type;
  Type_150.Children := 1;
  Type_150.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarSET2_151 := Type_150.GetNewChild;
    VarSET2_151.Tag := DW_TAG_member;
    VarSET2_151.Children := 0;
    VarSET2_151.Add(DW_AT_name, DW_FORM_string, 'SET2'+#0);
    VarSET2_151.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarSET2_151.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET2_107); // $79, $05, $00, $00

  Type_152 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_152.Tag := DW_TAG_reference_type;
  Type_152.Children := 0;
  Type_152.AddRef(DW_AT_type, DW_FORM_ref4, @Type_150); // $EA, $06, $00, $00

  Type_153 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_153.Tag := DW_TAG_structure_type;
  Type_153.Children := 1;
  Type_153.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarSET3_154 := Type_153.GetNewChild;
    VarSET3_154.Tag := DW_TAG_member;
    VarSET3_154.Children := 0;
    VarSET3_154.Add(DW_AT_name, DW_FORM_string, 'SET3'+#0);
    VarSET3_154.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarSET3_154.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET3_111); // $9D, $05, $00, $00

  Type_155 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_155.Tag := DW_TAG_reference_type;
  Type_155.Children := 0;
  Type_155.AddRef(DW_AT_type, DW_FORM_ref4, @Type_153); // $FF, $06, $00, $00

  Type_156 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_156.Tag := DW_TAG_structure_type;
  Type_156.Children := 1;
  Type_156.AddULEB(DW_AT_byte_size, DW_FORM_udata, 32);

    VarSETB1_157 := Type_156.GetNewChild;
    VarSETB1_157.Tag := DW_TAG_member;
    VarSETB1_157.Children := 0;
    VarSETB1_157.Add(DW_AT_name, DW_FORM_string, 'SETB1'+#0);
    VarSETB1_157.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarSETB1_157.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETB1_119); // $E7, $05, $00, $00

  Type_158 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_158.Tag := DW_TAG_reference_type;
  Type_158.Children := 0;
  Type_158.AddRef(DW_AT_type, DW_FORM_ref4, @Type_156); // $14, $07, $00, $00

  Type_159 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_159.Tag := DW_TAG_structure_type;
  Type_159.Children := 1;
  Type_159.AddULEB(DW_AT_byte_size, DW_FORM_udata, 32);

    VarSETB2_160 := Type_159.GetNewChild;
    VarSETB2_160.Tag := DW_TAG_member;
    VarSETB2_160.Children := 0;
    VarSETB2_160.Add(DW_AT_name, DW_FORM_string, 'SETB2'+#0);
    VarSETB2_160.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarSETB2_160.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETB2_123); // $0E, $06, $00, $00

  Type_161 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_161.Tag := DW_TAG_reference_type;
  Type_161.Children := 0;
  Type_161.AddRef(DW_AT_type, DW_FORM_ref4, @Type_159); // $2A, $07, $00, $00

  Type_162 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_162.Tag := DW_TAG_enumeration_type;
  Type_162.Children := 1;
  Type_162.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeS1A_163 := Type_162.GetNewChild;
    TypeS1A_163.Tag := DW_TAG_enumerator;
    TypeS1A_163.Children := 0;
    TypeS1A_163.Add(DW_AT_name, DW_FORM_string, 'S1A'+#0);
    TypeS1A_163.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeS1B_164 := Type_162.GetNewChild;
    TypeS1B_164.Tag := DW_TAG_enumerator;
    TypeS1B_164.Children := 0;
    TypeS1B_164.Add(DW_AT_name, DW_FORM_string, 'S1B'+#0);
    TypeS1B_164.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeS1C_165 := Type_162.GetNewChild;
    TypeS1C_165.Tag := DW_TAG_enumerator;
    TypeS1C_165.Children := 0;
    TypeS1C_165.Add(DW_AT_name, DW_FORM_string, 'S1C'+#0);
    TypeS1C_165.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

  Type_166 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_166.Tag := DW_TAG_reference_type;
  Type_166.Children := 0;
  Type_166.AddRef(DW_AT_type, DW_FORM_ref4, @Type_162); // $40, $07, $00, $00

  Type_167 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_167.Tag := DW_TAG_subrange_type;
  Type_167.Children := 0;
  Type_167.AddULEB(DW_AT_lower_bound, DW_FORM_udata, 5);
  Type_167.AddULEB(DW_AT_upper_bound, DW_FORM_udata, 80);
  Type_167.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_29); // $B9, $02, $00, $00

  Type_168 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_168.Tag := DW_TAG_reference_type;
  Type_168.Children := 0;
  Type_168.AddRef(DW_AT_type, DW_FORM_ref4, @Type_167); // $63, $07, $00, $00

  Type_169 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_169.Tag := DW_TAG_enumeration_type;
  Type_169.Children := 1;
  Type_169.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeSXA_170 := Type_169.GetNewChild;
    TypeSXA_170.Tag := DW_TAG_enumerator;
    TypeSXA_170.Children := 0;
    TypeSXA_170.Add(DW_AT_name, DW_FORM_string, 'SXA'+#0);
    TypeSXA_170.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeSXB_171 := Type_169.GetNewChild;
    TypeSXB_171.Tag := DW_TAG_enumerator;
    TypeSXB_171.Children := 0;
    TypeSXB_171.Add(DW_AT_name, DW_FORM_string, 'SXB'+#0);
    TypeSXB_171.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeSXC_172 := Type_169.GetNewChild;
    TypeSXC_172.Tag := DW_TAG_enumerator;
    TypeSXC_172.Children := 0;
    TypeSXC_172.Add(DW_AT_name, DW_FORM_string, 'SXC'+#0);
    TypeSXC_172.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

    TypeSXD_173 := Type_169.GetNewChild;
    TypeSXD_173.Tag := DW_TAG_enumerator;
    TypeSXD_173.Children := 0;
    TypeSXD_173.Add(DW_AT_name, DW_FORM_string, 'SXD'+#0);
    TypeSXD_173.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

  Type_174 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_174.Tag := DW_TAG_reference_type;
  Type_174.Children := 0;
  Type_174.AddRef(DW_AT_type, DW_FORM_ref4, @Type_169); // $6F, $07, $00, $00

  TypeDeclCHAR_175 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclCHAR_175.Tag := DW_TAG_typedef;
  TypeDeclCHAR_175.Children := 0;
  TypeDeclCHAR_175.Add(DW_AT_name, DW_FORM_string, 'CHAR'+#0);
  TypeDeclCHAR_175.AddRef(DW_AT_type, DW_FORM_ref4, @TypeChar_176); // $A5, $07, $00, $00

  TypeChar_176 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeChar_176.Tag := DW_TAG_base_type;
  TypeChar_176.Children := 0;
  TypeChar_176.Add(DW_AT_name, DW_FORM_string, 'Char'+#0);
  TypeChar_176.Add(DW_AT_encoding, DW_FORM_data1, [$08]);
  TypeChar_176.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_177 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_177.Tag := DW_TAG_reference_type;
  Type_177.Children := 0;
  Type_177.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclCHAR_175); // $9B, $07, $00, $00

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

