unit TestDwarfSetup1;

{$mode objfpc}{$H+}

interface

uses
  FpDbgDwarfConst,
  TestHelperClasses;

type

  {%region  Types defined in the DWARF }
  TTestSetup1Class = class;

  { TTestSetup1Record }

  TTestSetup1Record = record
    FWord: Word;
    FBool: Boolean;
    FTest: TTestSetup1Class;
  end;
  //PTestRecord = ^TTestSetup1Record;

  { TTestSetup1Class }

  TTestSetup1Class = class
  public
    FWord: Word;
    FWordL: QWord;
    FInt: ShortInt;
    FIntL: Int64;
    FBool: Boolean;
    FTest: TTestSetup1Class;
    procedure f0(a:integer); virtual;
  end;

  { TTestSetup1Object }

  TTestSetup1Object = object
  public
    FWord: Word;
    FWordL: QWord;
    FInt: ShortInt;
    FIntL: Int64;
    FBool: Boolean;
    FTest: TTestSetup1Class;
    procedure f0(a:integer); virtual;
  end;

  PTestObject = ^TTestSetup1Object;

  Pint = ^ integer;
  PPInt = ^Pint;
  PPPInt = ^PPint;

  (*  procedure Bar(var VObj1: TTestSetup1Object);
      var
        int1: Integer;
        pint1: ^Integer;
        bool1: Boolean;

        Obj1: TTestSetup1Class;
        PObj1: ^TTestSetup1Class;
        OldObj1: TTestSetup1Object;
        POldObj1: PTestObject;
        Rec1: TTestSetup1Record;
        PRec1: ^TTestSetup1Record;
        Rec2: record    FWord: Word;    FBool: Boolean;  end;

        pi: Pint;
        ppi: PPint;
        pppi: PPPint;

        subr: 1..9;
        subr2: -11..-9;
        subr3: #9..'m';
  *)
  {%endregion}


  { TTestLoaderSetup1 }

  TTestLoaderSetup1 = class(TTestDummyImageLoader)
  public
    constructor Create; override;
  public
    SectionDbgInfo: TTestDummySectionInfoEntries;
    //CompUnit, Prog1, Prog2,
    //GlobVar1, TypeInt, TypeIntDecl: TTestDwarfInfoEntry;

   Unittestprog2_pas_0, VarGLOBOBJ_1, VarGLOBINT_2, Progmain_3, ProgBAR_4, VarVOBJ1_5, VarINT1_6, VarPINT1_7, VarBOOL1_8, VarOBJ1_9, VarPOBJ1_10, VarOLDOBJ1_11, VarPOLDOBJ1_12, VarREC1_13, VarPREC1_14, VarREC2_15, VarPI_16, VarPPI_17, VarPPPI_18, VarSUBR_19, VarSUBR2_20, VarSUBR3_21, TypePtr_22, Type_23, TypePtr_24, Type_25, TypePtr_26, Type_27, Type_28, VarFWORD_29, VarFBOOL_30, Type_31, Type_32, Type_33, Type_34, Type_35, TypeChar_36, Type_37, TypeDeclBYTE_38, TypeBYTE_39, Type_40, TypeDeclSHORTINT_41, TypeSHORTINT_42, Type_43, TypeDeclWORD_44, TypeWORD_45, Type_46, TypeDeclLONGINT_47, TypeLONGINT_48, Type_49, TypeDeclBOOLEAN_50, TypeBoolean_51, Type_52, TypeDeclTTESTSETUP1CLASS_53, TypePtr_54, TypeTTESTSETUP1CLASS_55, XX_56, VarFWORD_57, VarFWORDL_58, VarFINT_59, VarFINTL_60, VarFBOOL_61, VarFTEST_62, ProgF0_63, Varthis_64, VarA_65, Type_66, TypeDeclTTESTSETUP1RECORD_67, TypeTTESTSETUP1RECORD_68, VarFWORD_69, VarFBOOL_70, VarFTEST_71, Type_72, TypeDeclTTESTSETUP1OBJECT_73, TypeTTESTSETUP1OBJECT_74, Var_vptrTTESTSETUP1OBJECT_75, VarFWORD_76, VarFWORDL_77, VarFINT_78, VarFINTL_79, VarFBOOL_80, VarFTEST_81, ProgF0_82, Varthis_83, VarA_84, Type_85, TypeDeclPTESTOBJECT_86, TypePtr_87, Type_88, TypeDeclPINT_89, TypePtr_90, Type_91, TypeDeclPPINT_92, TypePtr_93, Type_94, TypeDeclPPPINT_95, TypePtr_96, Type_97, TypeDeclTOBJECT_98, TypePtr_99, TypeTOBJECT_100, Var_vptrTOBJECT_101, ProgCREATE_102, Varthis_103, Varvmt_104, ProgDESTROY_105, Varthis_106, Varvmt_107, ProgNEWINSTANCE_108, Varself_109, ProgFREEINSTANCE_110, Varthis_111, ProgSAFECALLEXCEPTION_112, Varthis_113, VarEXCEPTOBJECT_114, VarEXCEPTADDR_115, ProgDEFAULTHANDLER_116, Varthis_117, VarMESSAGE_118, ProgFREE_119, Varthis_120, ProgINITINSTANCE_121, Varself_122, VarINSTANCE_123, ProgCLEANUPINSTANCE_124, Varthis_125, ProgCLASSTYPE_126, Varself_127, ProgCLASSINFO_128, Varself_129, ProgCLASSNAME_130, Varself_131, Varresult_132, ProgCLASSNAMEIS_133, Varself_134, VarNAME_135, ProgCLASSPARENT_136, Varself_137, ProgINSTANCESIZE_138, Varself_139, ProgINHERITSFROM_140, Varself_141, VarACLASS_142, ProgSTRINGMESSAGETABLE_143, Varself_144, ProgMETHODADDRESS_145, Varself_146, VarNAME_147, ProgMETHODNAME_148, Varself_149, VarADDRESS_150, Varresult_151, ProgFIELDADDRESS_152, Varthis_153, VarNAME_154, ProgAFTERCONSTRUCTION_155, Varthis_156, ProgBEFOREDESTRUCTION_157, Varthis_158, ProgDEFAULTHANDLERSTR_159, Varthis_160, VarMESSAGE_161, ProgDISPATCH_162, Varthis_163, VarMESSAGE_164, ProgDISPATCHSTR_165, Varthis_166, VarMESSAGE_167, ProgGETINTERFACE_168, Varthis_169, VarIID_170, VarOBJ_171, ProgGETINTERFACE_172, Varthis_173, VarIIDSTR_174, VarOBJ_175, ProgGETINTERFACEBYSTR_176, Varthis_177, VarIIDSTR_178, VarOBJ_179, ProgGETINTERFACEWEAK_180, Varthis_181, VarIID_182, VarOBJ_183, ProgGETINTERFACEENTRY_184, Varself_185, VarIID_186, ProgGETINTERFACEENTRYBYSTR_187, Varself_188, VarIIDSTR_189, ProgGETINTERFACETABLE_190, Varself_191, ProgUNITNAME_192, Varself_193, Varresult_194, ProgEQUALS_195, Varthis_196, VarOBJ_197, ProgGETHASHCODE_198, Varthis_199, ProgTOSTRING_200, Varthis_201, Varresult_202, Type_203, TypeDeclQWORD_204, TypeQWord_205, Type_206, TypeDeclINT64_207, TypeInt64_208, Type_209, TypeDeclPOINTER_210, TypePtr_211, Type_212, TypePtr_213, Type_214, TypeDeclHRESULT_215, TypeHRESULT_216, Type_217, TypeDeclformal_218, TypeFormalDef_219, Type_220, TypePtr_221, Type_222, TypeDeclTCLASS_223, TypePtr_224, Type_225, TypePtr_226, Type_227, TypePtr_228, Type_229, TypeDeclSHORTSTRING_230, TypeShortString_231, Varlength_232, Varst_233, Type_234, Type_235, Type_236, TypePtr_237, Type_238, TypePtr_239, Type_240, TypePtr_241, Type_242, TypePtr_243, Type_244, TypePtr_245, Type_246, TypeDeclPSTRINGMESSAGETABLE_247, TypePtr_248, Type_249, TypePtr_250, Type_251, TypePtr_252, Type_253, TypePtr_254, Type_255, TypeDeclTGUID_256, TypeTGUID_257, VarDATA1_258, VarDATA2_259, VarDATA3_260, VarDATA4_261, VarD1_262, VarD2_263, VarD3_264, VarD4_265, VarTIME_LOW_266, VarTIME_MID_267, VarTIME_HI_AND_VERSION_268, VarCLOCK_SEQ_HI_AND_RESERVED_269, VarCLOCK_SEQ_LOW_270, VarNODE_271, Type_272, TypeDeclPINTERFACEENTRY_273, TypePtr_274, Type_275, TypePtr_276, Type_277, TypePtr_278, Type_279, TypeDeclPINTERFACETABLE_280, TypePtr_281, Type_282, TypePtr_283, Type_284, TypeDeclANSISTRING_285, TypePtr_286, Type_287, TypePtr_288, Type_289, TypeDecl__vtbl_ptr_type_290, Type_291, Type_292, TypeDeclCHAR_293, TypeChar_294, Type_295, TypeDeclTSTRINGMESSAGETABLE_296, TypeTSTRINGMESSAGETABLE_297, VarCOUNT_298, VarMSGSTRTABLE_299, Type_300, TypeDeclLONGWORD_301, TypeLONGWORD_302, Type_303, Type_304, Type_305, Type_306, Type_307, Type_308, Type_309, Type_310, Type_311, Type_312, TypeDeclTINTERFACEENTRY_313, TypeTINTERFACEENTRY_314, VarIID_315, VarVTABLE_316, VarIOFFSET_317, VarIIDSTR_318, VarITYPE_319, Var__PAD_DUMMY_320, Type_321, TypeDeclTINTERFACETABLE_322, TypeTINTERFACETABLE_323, VarENTRYCOUNT_324, VarENTRIES_325, Type_326, Type_327, Type_328, Type_329, TypeDeclPGUID_330, TypePtr_331, Type_332, TypeDeclPSHORTSTRING_333, TypePtr_334, Type_335, TypeDeclTINTERFACEENTRYTYPE_336, TypeTINTERFACEENTRYTYPE_337, TypeETSTANDARD_338, TypeETVIRTUALMETHODRESULT_339, TypeETSTATICMETHODRESULT_340, TypeETFIELDVALUE_341, TypeETVIRTUALMETHODCLASS_342, TypeETSTATICMETHODCLASS_343, TypeETFIELDVALUECLASS_344, Type_345, Type_346, Type_347, Type_348, TypeDeclTMSGSTRTABLE_349, TypeTMSGSTRTABLE_350, VarNAME_351, VarMETHOD_352, Type_353
   : TTestDwarfInfoEntry;


    TestStackFrame: record
      Obj1: TTestSetup1Class;
      bool: Boolean;
      PInt1: ^Integer;
      Rec1: TTestSetup1Record;
      Int1: Integer; // -8
      VObj1: PTestObject; // -4 // Bar(var VOBj1: TTestSetup1Object)
      EndPoint: Cardinal;
    end;
  end;


implementation

{ TTestSetup1Class }

procedure TTestSetup1Class.f0(a: integer);
begin

end;

{ TTestSetup1Object }

procedure TTestSetup1Object.f0(a: integer);
begin

end;

{ TTestLoaderSetup1 }

constructor TTestLoaderSetup1.Create;
var
  StackOffs: LongInt;
begin
  inherited Create;

  SectionDbgInfo := TestImgReader.TestSection['.debug_info'] as TTestDummySectionInfoEntries;
  Unittestprog2_pas_0 := SectionDbgInfo.GetFirstInfoEntryObj;

  // Generated with fpc 2.6.2 32 bit win



Unittestprog2_pas_0.Tag := DW_TAG_compile_unit;
Unittestprog2_pas_0.Children := 1;
Unittestprog2_pas_0.Add(DW_AT_name, DW_FORM_string, 'testprog2.pas'+#0);
Unittestprog2_pas_0.Add(DW_AT_producer, DW_FORM_string, 'Free Pascal 2.6.2 2013/02/16'+#0);
Unittestprog2_pas_0.Add(DW_AT_comp_dir, DW_FORM_string, 'B:/lazarus_latest/components/fpdebug/test/testcode/'+#0);
Unittestprog2_pas_0.Add(DW_AT_language, DW_FORM_data1, [$09]);
Unittestprog2_pas_0.Add(DW_AT_identifier_case, DW_FORM_data1, [$03]);
Unittestprog2_pas_0.Add(DW_AT_stmt_list, DW_FORM_data4, [$00, $00, $00, $00]);
Unittestprog2_pas_0.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00400000);
Unittestprog2_pas_0.AddAddr(DW_AT_high_pc, DW_FORM_addr, $004F1470);

  VarGLOBOBJ_1 := Unittestprog2_pas_0.GetNewChild;
  VarGLOBOBJ_1.Tag := DW_TAG_variable;
  VarGLOBOBJ_1.Children := 0;
  VarGLOBOBJ_1.Add(DW_AT_name, DW_FORM_string, 'GLOBOBJ'+#0);
  VarGLOBOBJ_1.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB($00409000)])); // $03, $00, $90, $40, $00
  VarGLOBOBJ_1.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_73); // $BC, $03, $00, $00

  VarGLOBINT_2 := Unittestprog2_pas_0.GetNewChild;
  VarGLOBINT_2.Tag := DW_TAG_variable;
  VarGLOBINT_2.Children := 0;
  VarGLOBINT_2.Add(DW_AT_name, DW_FORM_string, 'GLOBINT'+#0);
  VarGLOBINT_2.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB($00409030)])); // $03, $30, $90, $40, $00
  VarGLOBINT_2.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_47); // $6A, $02, $00, $00

  Progmain_3 := Unittestprog2_pas_0.GetNewChild;
  Progmain_3.Tag := DW_TAG_subprogram;
  Progmain_3.Children := 0;
  Progmain_3.Add(DW_AT_name, DW_FORM_string, 'main'+#0);
  Progmain_3.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
  Progmain_3.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
  Progmain_3.Add(DW_AT_external, DW_FORM_flag, [$01]);
  Progmain_3.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00421440);
  Progmain_3.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00431463);

  ProgBAR_4 := Unittestprog2_pas_0.GetNewChild;
  ProgBAR_4.Tag := DW_TAG_subprogram;
  ProgBAR_4.Children := 1;
  ProgBAR_4.Add(DW_AT_name, DW_FORM_string, 'BAR'+#0);
  ProgBAR_4.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
  ProgBAR_4.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
  ProgBAR_4.Add(DW_AT_external, DW_FORM_flag, [$01]);
  ProgBAR_4.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00400000);
  ProgBAR_4.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00410000);

    StackOffs := @TestStackFrame.VObj1 - @TestStackFrame.EndPoint;
    VarVOBJ1_5 := ProgBAR_4.GetNewChild;
    VarVOBJ1_5.Tag := DW_TAG_formal_parameter;
    VarVOBJ1_5.Children := 0;
    VarVOBJ1_5.Add(DW_AT_name, DW_FORM_string, 'VOBJ1'+#0);
    VarVOBJ1_5.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $7C, $06
    VarVOBJ1_5.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_73); // $BC, $03, $00, $00

    StackOffs := @TestStackFrame.Int1 - @TestStackFrame.EndPoint;
    VarINT1_6 := ProgBAR_4.GetNewChild;
    VarINT1_6.Tag := DW_TAG_variable;
    VarINT1_6.Children := 0;
    VarINT1_6.Add(DW_AT_name, DW_FORM_string, 'INT1'+#0);
    VarINT1_6.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $78
    VarINT1_6.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_47); // $6A, $02, $00, $00

    StackOffs := @TestStackFrame.PInt1 - @TestStackFrame.EndPoint;
    VarPINT1_7 := ProgBAR_4.GetNewChild;
    VarPINT1_7.Tag := DW_TAG_variable;
    VarPINT1_7.Children := 0;
    VarPINT1_7.Add(DW_AT_name, DW_FORM_string, 'PINT1'+#0);
    VarPINT1_7.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $74
    VarPINT1_7.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_22); // $B5, $01, $00, $00

    StackOffs := @TestStackFrame.bool - @TestStackFrame.EndPoint;
    VarBOOL1_8 := ProgBAR_4.GetNewChild;
    VarBOOL1_8.Tag := DW_TAG_variable;
    VarBOOL1_8.Children := 0;
    VarBOOL1_8.Add(DW_AT_name, DW_FORM_string, 'BOOL1'+#0);
    VarBOOL1_8.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $70
    VarBOOL1_8.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_50); // $87, $02, $00, $00

    StackOffs := @TestStackFrame.Obj1 - @TestStackFrame.EndPoint;
    VarOBJ1_9 := ProgBAR_4.GetNewChild;
    VarOBJ1_9.Tag := DW_TAG_variable;
    VarOBJ1_9.Children := 0;
    VarOBJ1_9.Add(DW_AT_name, DW_FORM_string, 'OBJ1'+#0);
    VarOBJ1_9.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $6C
    VarOBJ1_9.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_53); // $A4, $02, $00, $00

    VarPOBJ1_10 := ProgBAR_4.GetNewChild;
    VarPOBJ1_10.Tag := DW_TAG_variable;
    VarPOBJ1_10.Children := 0;
    VarPOBJ1_10.Add(DW_AT_name, DW_FORM_string, 'POBJ1'+#0);
    VarPOBJ1_10.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-24)])); // $75, $68
    VarPOBJ1_10.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_24); // $BF, $01, $00, $00

    VarOLDOBJ1_11 := ProgBAR_4.GetNewChild;
    VarOLDOBJ1_11.Tag := DW_TAG_variable;
    VarOLDOBJ1_11.Children := 0;
    VarOLDOBJ1_11.Add(DW_AT_name, DW_FORM_string, 'OLDOBJ1'+#0);
    VarOLDOBJ1_11.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-72)])); // $75, $B8, $7F
    VarOLDOBJ1_11.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_73); // $BC, $03, $00, $00

    VarPOLDOBJ1_12 := ProgBAR_4.GetNewChild;
    VarPOLDOBJ1_12.Tag := DW_TAG_variable;
    VarPOLDOBJ1_12.Children := 0;
    VarPOLDOBJ1_12.Add(DW_AT_name, DW_FORM_string, 'POLDOBJ1'+#0);
    VarPOLDOBJ1_12.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-76)])); // $75, $B4, $7F
    VarPOLDOBJ1_12.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTOBJECT_86); // $8F, $04, $00, $00

    StackOffs := @TestStackFrame.Rec1 - @TestStackFrame.EndPoint;
    VarREC1_13 := ProgBAR_4.GetNewChild;
    VarREC1_13.Tag := DW_TAG_variable;
    VarREC1_13.Children := 0;
    VarREC1_13.Add(DW_AT_name, DW_FORM_string, 'REC1'+#0);
    VarREC1_13.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $AC, $7F
    VarREC1_13.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD_67); // $61, $03, $00, $00

    VarPREC1_14 := ProgBAR_4.GetNewChild;
    VarPREC1_14.Tag := DW_TAG_variable;
    VarPREC1_14.Children := 0;
    VarPREC1_14.Add(DW_AT_name, DW_FORM_string, 'PREC1'+#0);
    VarPREC1_14.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-88)])); // $75, $A8, $7F
    VarPREC1_14.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_26); // $C9, $01, $00, $00

    VarREC2_15 := ProgBAR_4.GetNewChild;
    VarREC2_15.Tag := DW_TAG_variable;
    VarREC2_15.Children := 0;
    VarREC2_15.Add(DW_AT_name, DW_FORM_string, 'REC2'+#0);
    VarREC2_15.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-92)])); // $75, $A4, $7F
    VarREC2_15.AddRef(DW_AT_type, DW_FORM_ref4, @Type_28); // $D3, $01, $00, $00

    VarPI_16 := ProgBAR_4.GetNewChild;
    VarPI_16.Tag := DW_TAG_variable;
    VarPI_16.Children := 0;
    VarPI_16.Add(DW_AT_name, DW_FORM_string, 'PI'+#0);
    VarPI_16.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-96)])); // $75, $A0, $7F
    VarPI_16.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINT_89); // $AA, $04, $00, $00

    VarPPI_17 := ProgBAR_4.GetNewChild;
    VarPPI_17.Tag := DW_TAG_variable;
    VarPPI_17.Children := 0;
    VarPPI_17.Add(DW_AT_name, DW_FORM_string, 'PPI'+#0);
    VarPPI_17.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-100)])); // $75, $9C, $7F
    VarPPI_17.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPPINT_92); // $BE, $04, $00, $00

    VarPPPI_18 := ProgBAR_4.GetNewChild;
    VarPPPI_18.Tag := DW_TAG_variable;
    VarPPPI_18.Children := 0;
    VarPPPI_18.Add(DW_AT_name, DW_FORM_string, 'PPPI'+#0);
    VarPPPI_18.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-104)])); // $75, $98, $7F
    VarPPPI_18.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPPPINT_95); // $D3, $04, $00, $00

    VarSUBR_19 := ProgBAR_4.GetNewChild;
    VarSUBR_19.Tag := DW_TAG_variable;
    VarSUBR_19.Children := 0;
    VarSUBR_19.Add(DW_AT_name, DW_FORM_string, 'SUBR'+#0);
    VarSUBR_19.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-108)])); // $75, $94, $7F
    VarSUBR_19.AddRef(DW_AT_type, DW_FORM_ref4, @Type_32); // $F7, $01, $00, $00

    VarSUBR2_20 := ProgBAR_4.GetNewChild;
    VarSUBR2_20.Tag := DW_TAG_variable;
    VarSUBR2_20.Children := 0;
    VarSUBR2_20.Add(DW_AT_name, DW_FORM_string, 'SUBR2'+#0);
    VarSUBR2_20.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-112)])); // $75, $90, $7F
    VarSUBR2_20.AddRef(DW_AT_type, DW_FORM_ref4, @Type_34); // $03, $02, $00, $00

    VarSUBR3_21 := ProgBAR_4.GetNewChild;
    VarSUBR3_21.Tag := DW_TAG_variable;
    VarSUBR3_21.Children := 0;
    VarSUBR3_21.Add(DW_AT_name, DW_FORM_string, 'SUBR3'+#0);
    VarSUBR3_21.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-116)])); // $75, $8C, $7F
    VarSUBR3_21.AddRef(DW_AT_type, DW_FORM_ref4, @TypeChar_36); // $0F, $02, $00, $00

    TypePtr_22 := ProgBAR_4.GetNewChild;
    TypePtr_22.Tag := DW_TAG_pointer_type;
    TypePtr_22.Children := 0;
    TypePtr_22.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_47); // $6A, $02, $00, $00

    Type_23 := ProgBAR_4.GetNewChild;
    Type_23.Tag := DW_TAG_reference_type;
    Type_23.Children := 0;
    Type_23.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_22); // $B5, $01, $00, $00

    TypePtr_24 := ProgBAR_4.GetNewChild;
    TypePtr_24.Tag := DW_TAG_pointer_type;
    TypePtr_24.Children := 0;
    TypePtr_24.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_53); // $A4, $02, $00, $00

    Type_25 := ProgBAR_4.GetNewChild;
    Type_25.Tag := DW_TAG_reference_type;
    Type_25.Children := 0;
    Type_25.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_24); // $BF, $01, $00, $00

    TypePtr_26 := ProgBAR_4.GetNewChild;
    TypePtr_26.Tag := DW_TAG_pointer_type;
    TypePtr_26.Children := 0;
    TypePtr_26.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD_67); // $61, $03, $00, $00

    Type_27 := ProgBAR_4.GetNewChild;
    Type_27.Tag := DW_TAG_reference_type;
    Type_27.Children := 0;
    Type_27.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_26); // $C9, $01, $00, $00

    Type_28 := ProgBAR_4.GetNewChild;
    Type_28.Tag := DW_TAG_structure_type;
    Type_28.Children := 1;
    Type_28.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 4);

      VarFWORD_29 := Type_28.GetNewChild;
      VarFWORD_29.Tag := DW_TAG_member;
      VarFWORD_29.Children := 0;
      VarFWORD_29.Add(DW_AT_name, DW_FORM_string, 'FWORD'+#0);
      VarFWORD_29.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
      VarFWORD_29.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_44); // $53, $02, $00, $00

      VarFBOOL_30 := Type_28.GetNewChild;
      VarFBOOL_30.Tag := DW_TAG_member;
      VarFBOOL_30.Children := 0;
      VarFBOOL_30.Add(DW_AT_name, DW_FORM_string, 'FBOOL'+#0);
      VarFBOOL_30.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(2)])); // $23, $02
      VarFBOOL_30.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_50); // $87, $02, $00, $00

    Type_31 := ProgBAR_4.GetNewChild;
    Type_31.Tag := DW_TAG_reference_type;
    Type_31.Children := 0;
    Type_31.AddRef(DW_AT_type, DW_FORM_ref4, @Type_28); // $D3, $01, $00, $00

    Type_32 := ProgBAR_4.GetNewChild;
    Type_32.Tag := DW_TAG_subrange_type;
    Type_32.Children := 0;
    Type_32.AddSLEB(DW_AT_lower_bound, DW_FORM_udata, 1);
    Type_32.AddSLEB(DW_AT_upper_bound, DW_FORM_udata, 9);
    Type_32.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_38); // $1D, $02, $00, $00

    Type_33 := ProgBAR_4.GetNewChild;
    Type_33.Tag := DW_TAG_reference_type;
    Type_33.Children := 0;
    Type_33.AddRef(DW_AT_type, DW_FORM_ref4, @Type_32); // $F7, $01, $00, $00

    Type_34 := ProgBAR_4.GetNewChild;
    Type_34.Tag := DW_TAG_subrange_type;
    Type_34.Children := 0;
    Type_34.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 117);
    Type_34.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 119);
    Type_34.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_41); // $34, $02, $00, $00

    Type_35 := ProgBAR_4.GetNewChild;
    Type_35.Tag := DW_TAG_reference_type;
    Type_35.Children := 0;
    Type_35.AddRef(DW_AT_type, DW_FORM_ref4, @Type_34); // $03, $02, $00, $00

    TypeChar_36 := ProgBAR_4.GetNewChild;
    TypeChar_36.Tag := DW_TAG_base_type;
    TypeChar_36.Children := 0;
    TypeChar_36.Add(DW_AT_name, DW_FORM_string, 'Char'+#0);
    TypeChar_36.Add(DW_AT_encoding, DW_FORM_data1, [$08]);
    TypeChar_36.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

    Type_37 := ProgBAR_4.GetNewChild;
    Type_37.Tag := DW_TAG_reference_type;
    Type_37.Children := 0;
    Type_37.AddRef(DW_AT_type, DW_FORM_ref4, @TypeChar_36); // $0F, $02, $00, $00

  TypeDeclBYTE_38 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclBYTE_38.Tag := DW_TAG_typedef;
  TypeDeclBYTE_38.Children := 0;
  TypeDeclBYTE_38.Add(DW_AT_name, DW_FORM_string, 'BYTE'+#0);
  TypeDeclBYTE_38.AddRef(DW_AT_type, DW_FORM_ref4, @TypeBYTE_39); // $27, $02, $00, $00

  TypeBYTE_39 := Unittestprog2_pas_0.GetNewChild;
  TypeBYTE_39.Tag := DW_TAG_base_type;
  TypeBYTE_39.Children := 0;
  TypeBYTE_39.Add(DW_AT_name, DW_FORM_string, 'BYTE'+#0);
  TypeBYTE_39.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeBYTE_39.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_40 := Unittestprog2_pas_0.GetNewChild;
  Type_40.Tag := DW_TAG_reference_type;
  Type_40.Children := 0;
  Type_40.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_38); // $1D, $02, $00, $00

  TypeDeclSHORTINT_41 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclSHORTINT_41.Tag := DW_TAG_typedef;
  TypeDeclSHORTINT_41.Children := 0;
  TypeDeclSHORTINT_41.Add(DW_AT_name, DW_FORM_string, 'SHORTINT'+#0);
  TypeDeclSHORTINT_41.AddRef(DW_AT_type, DW_FORM_ref4, @TypeSHORTINT_42); // $42, $02, $00, $00

  TypeSHORTINT_42 := Unittestprog2_pas_0.GetNewChild;
  TypeSHORTINT_42.Tag := DW_TAG_base_type;
  TypeSHORTINT_42.Children := 0;
  TypeSHORTINT_42.Add(DW_AT_name, DW_FORM_string, 'SHORTINT'+#0);
  TypeSHORTINT_42.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeSHORTINT_42.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_43 := Unittestprog2_pas_0.GetNewChild;
  Type_43.Tag := DW_TAG_reference_type;
  Type_43.Children := 0;
  Type_43.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_41); // $34, $02, $00, $00

  TypeDeclWORD_44 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclWORD_44.Tag := DW_TAG_typedef;
  TypeDeclWORD_44.Children := 0;
  TypeDeclWORD_44.Add(DW_AT_name, DW_FORM_string, 'WORD'+#0);
  TypeDeclWORD_44.AddRef(DW_AT_type, DW_FORM_ref4, @TypeWORD_45); // $5D, $02, $00, $00

  TypeWORD_45 := Unittestprog2_pas_0.GetNewChild;
  TypeWORD_45.Tag := DW_TAG_base_type;
  TypeWORD_45.Children := 0;
  TypeWORD_45.Add(DW_AT_name, DW_FORM_string, 'WORD'+#0);
  TypeWORD_45.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeWORD_45.Add(DW_AT_byte_size, DW_FORM_data1, [$02]);

  Type_46 := Unittestprog2_pas_0.GetNewChild;
  Type_46.Tag := DW_TAG_reference_type;
  Type_46.Children := 0;
  Type_46.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_44); // $53, $02, $00, $00

  TypeDeclLONGINT_47 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclLONGINT_47.Tag := DW_TAG_typedef;
  TypeDeclLONGINT_47.Children := 0;
  TypeDeclLONGINT_47.Add(DW_AT_name, DW_FORM_string, 'LONGINT'+#0);
  TypeDeclLONGINT_47.AddRef(DW_AT_type, DW_FORM_ref4, @TypeLONGINT_48); // $77, $02, $00, $00

  TypeLONGINT_48 := Unittestprog2_pas_0.GetNewChild;
  TypeLONGINT_48.Tag := DW_TAG_base_type;
  TypeLONGINT_48.Children := 0;
  TypeLONGINT_48.Add(DW_AT_name, DW_FORM_string, 'LONGINT'+#0);
  TypeLONGINT_48.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeLONGINT_48.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

  Type_49 := Unittestprog2_pas_0.GetNewChild;
  Type_49.Tag := DW_TAG_reference_type;
  Type_49.Children := 0;
  Type_49.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_47); // $6A, $02, $00, $00

  TypeDeclBOOLEAN_50 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclBOOLEAN_50.Tag := DW_TAG_typedef;
  TypeDeclBOOLEAN_50.Children := 0;
  TypeDeclBOOLEAN_50.Add(DW_AT_name, DW_FORM_string, 'BOOLEAN'+#0);
  TypeDeclBOOLEAN_50.AddRef(DW_AT_type, DW_FORM_ref4, @TypeBoolean_51); // $94, $02, $00, $00

  TypeBoolean_51 := Unittestprog2_pas_0.GetNewChild;
  TypeBoolean_51.Tag := DW_TAG_base_type;
  TypeBoolean_51.Children := 0;
  TypeBoolean_51.Add(DW_AT_name, DW_FORM_string, 'Boolean'+#0);
  TypeBoolean_51.Add(DW_AT_encoding, DW_FORM_data1, [$02]);
  TypeBoolean_51.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_52 := Unittestprog2_pas_0.GetNewChild;
  Type_52.Tag := DW_TAG_reference_type;
  Type_52.Children := 0;
  Type_52.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_50); // $87, $02, $00, $00

  TypeDeclTTESTSETUP1CLASS_53 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclTTESTSETUP1CLASS_53.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1CLASS_53.Children := 0;
  TypeDeclTTESTSETUP1CLASS_53.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1CLASS'+#0);
  TypeDeclTTESTSETUP1CLASS_53.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_54); // $BA, $02, $00, $00

  TypePtr_54 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_54.Tag := DW_TAG_pointer_type;
  TypePtr_54.Children := 0;
  TypePtr_54.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1CLASS_55); // $BF, $02, $00, $00

  TypeTTESTSETUP1CLASS_55 := Unittestprog2_pas_0.GetNewChild;
  TypeTTESTSETUP1CLASS_55.Tag := DW_TAG_structure_type;
  TypeTTESTSETUP1CLASS_55.Children := 1;
  TypeTTESTSETUP1CLASS_55.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1CLASS'+#0);
  TypeTTESTSETUP1CLASS_55.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 40);

    XX_56 := TypeTTESTSETUP1CLASS_55.GetNewChild;
    XX_56.Tag := DW_TAG_inheritance;
    XX_56.Children := 0;
    XX_56.Add(DW_AT_accessibility, DW_FORM_data1, [$01]);
    XX_56.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    XX_56.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTOBJECT_100); // $FB, $04, $00, $00

    VarFWORD_57 := TypeTTESTSETUP1CLASS_55.GetNewChild;
    VarFWORD_57.Tag := DW_TAG_member;
    VarFWORD_57.Children := 0;
    VarFWORD_57.Add(DW_AT_name, DW_FORM_string, 'FWORD'+#0);
    VarFWORD_57.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarFWORD_57.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_44); // $53, $02, $00, $00

    VarFWORDL_58 := TypeTTESTSETUP1CLASS_55.GetNewChild;
    VarFWORDL_58.Tag := DW_TAG_member;
    VarFWORDL_58.Children := 0;
    VarFWORDL_58.Add(DW_AT_name, DW_FORM_string, 'FWORDL'+#0);
    VarFWORDL_58.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(8)])); // $23, $08
    VarFWORDL_58.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_204); // $23, $0B, $00, $00

    VarFINT_59 := TypeTTESTSETUP1CLASS_55.GetNewChild;
    VarFINT_59.Tag := DW_TAG_member;
    VarFINT_59.Children := 0;
    VarFINT_59.Add(DW_AT_name, DW_FORM_string, 'FINT'+#0);
    VarFINT_59.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(16)])); // $23, $10
    VarFINT_59.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_41); // $34, $02, $00, $00

    VarFINTL_60 := TypeTTESTSETUP1CLASS_55.GetNewChild;
    VarFINTL_60.Tag := DW_TAG_member;
    VarFINTL_60.Children := 0;
    VarFINTL_60.Add(DW_AT_name, DW_FORM_string, 'FINTL'+#0);
    VarFINTL_60.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(24)])); // $23, $18
    VarFINTL_60.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclINT64_207); // $3C, $0B, $00, $00

    VarFBOOL_61 := TypeTTESTSETUP1CLASS_55.GetNewChild;
    VarFBOOL_61.Tag := DW_TAG_member;
    VarFBOOL_61.Children := 0;
    VarFBOOL_61.Add(DW_AT_name, DW_FORM_string, 'FBOOL'+#0);
    VarFBOOL_61.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(32)])); // $23, $20
    VarFBOOL_61.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_50); // $87, $02, $00, $00

    VarFTEST_62 := TypeTTESTSETUP1CLASS_55.GetNewChild;
    VarFTEST_62.Tag := DW_TAG_member;
    VarFTEST_62.Children := 0;
    VarFTEST_62.Add(DW_AT_name, DW_FORM_string, 'FTEST'+#0);
    VarFTEST_62.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(36)])); // $23, $24
    VarFTEST_62.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_53); // $A4, $02, $00, $00

    ProgF0_63 := TypeTTESTSETUP1CLASS_55.GetNewChild;
    ProgF0_63.Tag := DW_TAG_subprogram;
    ProgF0_63.Children := 1;
    ProgF0_63.Add(DW_AT_name, DW_FORM_string, 'F0'+#0);
    ProgF0_63.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgF0_63.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgF0_63.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgF0_63.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgF0_63.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $19]));
    ProgF0_63.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00000000);
    ProgF0_63.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00000000);

      Varthis_64 := ProgF0_63.GetNewChild;
      Varthis_64.Tag := DW_TAG_formal_parameter;
      Varthis_64.Children := 0;
      Varthis_64.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_64.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-8)])); // $75, $78
      Varthis_64.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_64.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_53); // $A4, $02, $00, $00

      VarA_65 := ProgF0_63.GetNewChild;
      VarA_65.Tag := DW_TAG_formal_parameter;
      VarA_65.Children := 0;
      VarA_65.Add(DW_AT_name, DW_FORM_string, 'A'+#0);
      VarA_65.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-4)])); // $75, $7C
      VarA_65.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_47); // $6A, $02, $00, $00

  Type_66 := Unittestprog2_pas_0.GetNewChild;
  Type_66.Tag := DW_TAG_reference_type;
  Type_66.Children := 0;
  Type_66.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_53); // $A4, $02, $00, $00

  TypeDeclTTESTSETUP1RECORD_67 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclTTESTSETUP1RECORD_67.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1RECORD_67.Children := 0;
  TypeDeclTTESTSETUP1RECORD_67.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1RECORD'+#0);
  TypeDeclTTESTSETUP1RECORD_67.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1RECORD_68); // $78, $03, $00, $00

  TypeTTESTSETUP1RECORD_68 := Unittestprog2_pas_0.GetNewChild;
  TypeTTESTSETUP1RECORD_68.Tag := DW_TAG_structure_type;
  TypeTTESTSETUP1RECORD_68.Children := 1;
  TypeTTESTSETUP1RECORD_68.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1RECORD'+#0);
  TypeTTESTSETUP1RECORD_68.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 8);

    VarFWORD_69 := TypeTTESTSETUP1RECORD_68.GetNewChild;
    VarFWORD_69.Tag := DW_TAG_member;
    VarFWORD_69.Children := 0;
    VarFWORD_69.Add(DW_AT_name, DW_FORM_string, 'FWORD'+#0);
    VarFWORD_69.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarFWORD_69.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_44); // $53, $02, $00, $00

    VarFBOOL_70 := TypeTTESTSETUP1RECORD_68.GetNewChild;
    VarFBOOL_70.Tag := DW_TAG_member;
    VarFBOOL_70.Children := 0;
    VarFBOOL_70.Add(DW_AT_name, DW_FORM_string, 'FBOOL'+#0);
    VarFBOOL_70.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(2)])); // $23, $02
    VarFBOOL_70.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_50); // $87, $02, $00, $00

    VarFTEST_71 := TypeTTESTSETUP1RECORD_68.GetNewChild;
    VarFTEST_71.Tag := DW_TAG_member;
    VarFTEST_71.Children := 0;
    VarFTEST_71.Add(DW_AT_name, DW_FORM_string, 'FTEST'+#0);
    VarFTEST_71.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarFTEST_71.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_53); // $A4, $02, $00, $00

  Type_72 := Unittestprog2_pas_0.GetNewChild;
  Type_72.Tag := DW_TAG_reference_type;
  Type_72.Children := 0;
  Type_72.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD_67); // $61, $03, $00, $00

  TypeDeclTTESTSETUP1OBJECT_73 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclTTESTSETUP1OBJECT_73.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1OBJECT_73.Children := 0;
  TypeDeclTTESTSETUP1OBJECT_73.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1OBJECT'+#0);
  TypeDeclTTESTSETUP1OBJECT_73.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1OBJECT_74); // $D3, $03, $00, $00

  TypeTTESTSETUP1OBJECT_74 := Unittestprog2_pas_0.GetNewChild;
  TypeTTESTSETUP1OBJECT_74.Tag := DW_TAG_structure_type;
  TypeTTESTSETUP1OBJECT_74.Children := 1;
  TypeTTESTSETUP1OBJECT_74.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1OBJECT'+#0);
  TypeTTESTSETUP1OBJECT_74.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 48);

    Var_vptrTTESTSETUP1OBJECT_75 := TypeTTESTSETUP1OBJECT_74.GetNewChild;
    Var_vptrTTESTSETUP1OBJECT_75.Tag := DW_TAG_member;
    Var_vptrTTESTSETUP1OBJECT_75.Children := 0;
    Var_vptrTTESTSETUP1OBJECT_75.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
    Var_vptrTTESTSETUP1OBJECT_75.Add(DW_AT_name, DW_FORM_string, '_vptr$TTESTSETUP1OBJECT'+#0);
    Var_vptrTTESTSETUP1OBJECT_75.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(40)])); // $23, $28
    Var_vptrTTESTSETUP1OBJECT_75.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_210); // $55, $0B, $00, $00

    VarFWORD_76 := TypeTTESTSETUP1OBJECT_74.GetNewChild;
    VarFWORD_76.Tag := DW_TAG_member;
    VarFWORD_76.Children := 0;
    VarFWORD_76.Add(DW_AT_name, DW_FORM_string, 'FWORD'+#0);
    VarFWORD_76.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarFWORD_76.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_44); // $53, $02, $00, $00

    VarFWORDL_77 := TypeTTESTSETUP1OBJECT_74.GetNewChild;
    VarFWORDL_77.Tag := DW_TAG_member;
    VarFWORDL_77.Children := 0;
    VarFWORDL_77.Add(DW_AT_name, DW_FORM_string, 'FWORDL'+#0);
    VarFWORDL_77.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(8)])); // $23, $08
    VarFWORDL_77.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_204); // $23, $0B, $00, $00

    VarFINT_78 := TypeTTESTSETUP1OBJECT_74.GetNewChild;
    VarFINT_78.Tag := DW_TAG_member;
    VarFINT_78.Children := 0;
    VarFINT_78.Add(DW_AT_name, DW_FORM_string, 'FINT'+#0);
    VarFINT_78.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(16)])); // $23, $10
    VarFINT_78.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_41); // $34, $02, $00, $00

    VarFINTL_79 := TypeTTESTSETUP1OBJECT_74.GetNewChild;
    VarFINTL_79.Tag := DW_TAG_member;
    VarFINTL_79.Children := 0;
    VarFINTL_79.Add(DW_AT_name, DW_FORM_string, 'FINTL'+#0);
    VarFINTL_79.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(24)])); // $23, $18
    VarFINTL_79.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclINT64_207); // $3C, $0B, $00, $00

    VarFBOOL_80 := TypeTTESTSETUP1OBJECT_74.GetNewChild;
    VarFBOOL_80.Tag := DW_TAG_member;
    VarFBOOL_80.Children := 0;
    VarFBOOL_80.Add(DW_AT_name, DW_FORM_string, 'FBOOL'+#0);
    VarFBOOL_80.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(32)])); // $23, $20
    VarFBOOL_80.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_50); // $87, $02, $00, $00

    VarFTEST_81 := TypeTTESTSETUP1OBJECT_74.GetNewChild;
    VarFTEST_81.Tag := DW_TAG_member;
    VarFTEST_81.Children := 0;
    VarFTEST_81.Add(DW_AT_name, DW_FORM_string, 'FTEST'+#0);
    VarFTEST_81.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(36)])); // $23, $24
    VarFTEST_81.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_53); // $A4, $02, $00, $00

    ProgF0_82 := TypeTTESTSETUP1OBJECT_74.GetNewChild;
    ProgF0_82.Tag := DW_TAG_subprogram;
    ProgF0_82.Children := 1;
    ProgF0_82.Add(DW_AT_name, DW_FORM_string, 'F0'+#0);
    ProgF0_82.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgF0_82.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgF0_82.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgF0_82.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgF0_82.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $03]));
    ProgF0_82.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00000000);
    ProgF0_82.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00000000);

      Varthis_83 := ProgF0_82.GetNewChild;
      Varthis_83.Tag := DW_TAG_formal_parameter;
      Varthis_83.Children := 0;
      Varthis_83.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_83.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-8), DW_OP_deref])); // $75, $78, $06
      Varthis_83.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_83.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_73); // $BC, $03, $00, $00

      VarA_84 := ProgF0_82.GetNewChild;
      VarA_84.Tag := DW_TAG_formal_parameter;
      VarA_84.Children := 0;
      VarA_84.Add(DW_AT_name, DW_FORM_string, 'A'+#0);
      VarA_84.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-4)])); // $75, $7C
      VarA_84.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_47); // $6A, $02, $00, $00

  Type_85 := Unittestprog2_pas_0.GetNewChild;
  Type_85.Tag := DW_TAG_reference_type;
  Type_85.Children := 0;
  Type_85.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_73); // $BC, $03, $00, $00

  TypeDeclPTESTOBJECT_86 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclPTESTOBJECT_86.Tag := DW_TAG_typedef;
  TypeDeclPTESTOBJECT_86.Children := 0;
  TypeDeclPTESTOBJECT_86.Add(DW_AT_name, DW_FORM_string, 'PTESTOBJECT'+#0);
  TypeDeclPTESTOBJECT_86.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_87); // $A0, $04, $00, $00

  TypePtr_87 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_87.Tag := DW_TAG_pointer_type;
  TypePtr_87.Children := 0;
  TypePtr_87.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_73); // $BC, $03, $00, $00

  Type_88 := Unittestprog2_pas_0.GetNewChild;
  Type_88.Tag := DW_TAG_reference_type;
  Type_88.Children := 0;
  Type_88.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTOBJECT_86); // $8F, $04, $00, $00

  TypeDeclPINT_89 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclPINT_89.Tag := DW_TAG_typedef;
  TypeDeclPINT_89.Children := 0;
  TypeDeclPINT_89.Add(DW_AT_name, DW_FORM_string, 'PINT'+#0);
  TypeDeclPINT_89.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_90); // $B4, $04, $00, $00

  TypePtr_90 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_90.Tag := DW_TAG_pointer_type;
  TypePtr_90.Children := 0;
  TypePtr_90.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_47); // $6A, $02, $00, $00

  Type_91 := Unittestprog2_pas_0.GetNewChild;
  Type_91.Tag := DW_TAG_reference_type;
  Type_91.Children := 0;
  Type_91.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINT_89); // $AA, $04, $00, $00

  TypeDeclPPINT_92 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclPPINT_92.Tag := DW_TAG_typedef;
  TypeDeclPPINT_92.Children := 0;
  TypeDeclPPINT_92.Add(DW_AT_name, DW_FORM_string, 'PPINT'+#0);
  TypeDeclPPINT_92.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_93); // $C9, $04, $00, $00

  TypePtr_93 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_93.Tag := DW_TAG_pointer_type;
  TypePtr_93.Children := 0;
  TypePtr_93.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINT_89); // $AA, $04, $00, $00

  Type_94 := Unittestprog2_pas_0.GetNewChild;
  Type_94.Tag := DW_TAG_reference_type;
  Type_94.Children := 0;
  Type_94.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPPINT_92); // $BE, $04, $00, $00

  TypeDeclPPPINT_95 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclPPPINT_95.Tag := DW_TAG_typedef;
  TypeDeclPPPINT_95.Children := 0;
  TypeDeclPPPINT_95.Add(DW_AT_name, DW_FORM_string, 'PPPINT'+#0);
  TypeDeclPPPINT_95.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_96); // $DF, $04, $00, $00

  TypePtr_96 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_96.Tag := DW_TAG_pointer_type;
  TypePtr_96.Children := 0;
  TypePtr_96.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPPINT_92); // $BE, $04, $00, $00

  Type_97 := Unittestprog2_pas_0.GetNewChild;
  Type_97.Tag := DW_TAG_reference_type;
  Type_97.Children := 0;
  Type_97.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPPPINT_95); // $D3, $04, $00, $00

  TypeDeclTOBJECT_98 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclTOBJECT_98.Tag := DW_TAG_typedef;
  TypeDeclTOBJECT_98.Children := 0;
  TypeDeclTOBJECT_98.Add(DW_AT_name, DW_FORM_string, 'TOBJECT'+#0);
  TypeDeclTOBJECT_98.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_99); // $F6, $04, $00, $00

  TypePtr_99 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_99.Tag := DW_TAG_pointer_type;
  TypePtr_99.Children := 0;
  TypePtr_99.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTOBJECT_100); // $FB, $04, $00, $00

  TypeTOBJECT_100 := Unittestprog2_pas_0.GetNewChild;
  TypeTOBJECT_100.Tag := DW_TAG_structure_type;
  TypeTOBJECT_100.Children := 1;
  TypeTOBJECT_100.Add(DW_AT_name, DW_FORM_string, 'TOBJECT'+#0);
  TypeTOBJECT_100.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 4);

    Var_vptrTOBJECT_101 := TypeTOBJECT_100.GetNewChild;
    Var_vptrTOBJECT_101.Tag := DW_TAG_member;
    Var_vptrTOBJECT_101.Children := 0;
    Var_vptrTOBJECT_101.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
    Var_vptrTOBJECT_101.Add(DW_AT_name, DW_FORM_string, '_vptr$TOBJECT'+#0);
    Var_vptrTOBJECT_101.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    Var_vptrTOBJECT_101.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_210); // $55, $0B, $00, $00

    ProgCREATE_102 := TypeTOBJECT_100.GetNewChild;
    ProgCREATE_102.Tag := DW_TAG_subprogram;
    ProgCREATE_102.Children := 1;
    ProgCREATE_102.Add(DW_AT_name, DW_FORM_string, 'CREATE'+#0);
    ProgCREATE_102.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCREATE_102.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCREATE_102.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCREATE_102.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

      Varthis_103 := ProgCREATE_102.GetNewChild;
      Varthis_103.Tag := DW_TAG_formal_parameter;
      Varthis_103.Children := 0;
      Varthis_103.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_103.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_103.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

      Varvmt_104 := ProgCREATE_102.GetNewChild;
      Varvmt_104.Tag := DW_TAG_formal_parameter;
      Varvmt_104.Children := 0;
      Varvmt_104.Add(DW_AT_name, DW_FORM_string, 'vmt'+#0);
      Varvmt_104.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_210); // $55, $0B, $00, $00

    ProgDESTROY_105 := TypeTOBJECT_100.GetNewChild;
    ProgDESTROY_105.Tag := DW_TAG_subprogram;
    ProgDESTROY_105.Children := 1;
    ProgDESTROY_105.Add(DW_AT_name, DW_FORM_string, 'DESTROY'+#0);
    ProgDESTROY_105.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgDESTROY_105.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgDESTROY_105.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgDESTROY_105.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgDESTROY_105.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $0C]));

      Varthis_106 := ProgDESTROY_105.GetNewChild;
      Varthis_106.Tag := DW_TAG_formal_parameter;
      Varthis_106.Children := 0;
      Varthis_106.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_106.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_106.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

      Varvmt_107 := ProgDESTROY_105.GetNewChild;
      Varvmt_107.Tag := DW_TAG_formal_parameter;
      Varvmt_107.Children := 0;
      Varvmt_107.Add(DW_AT_name, DW_FORM_string, 'vmt'+#0);
      Varvmt_107.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_210); // $55, $0B, $00, $00

    ProgNEWINSTANCE_108 := TypeTOBJECT_100.GetNewChild;
    ProgNEWINSTANCE_108.Tag := DW_TAG_subprogram;
    ProgNEWINSTANCE_108.Children := 1;
    ProgNEWINSTANCE_108.Add(DW_AT_name, DW_FORM_string, 'NEWINSTANCE'+#0);
    ProgNEWINSTANCE_108.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgNEWINSTANCE_108.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgNEWINSTANCE_108.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgNEWINSTANCE_108.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgNEWINSTANCE_108.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $0D]));
    ProgNEWINSTANCE_108.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

      Varself_109 := ProgNEWINSTANCE_108.GetNewChild;
      Varself_109.Tag := DW_TAG_formal_parameter;
      Varself_109.Children := 0;
      Varself_109.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_109.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_109.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_213); // $68, $0B, $00, $00

    ProgFREEINSTANCE_110 := TypeTOBJECT_100.GetNewChild;
    ProgFREEINSTANCE_110.Tag := DW_TAG_subprogram;
    ProgFREEINSTANCE_110.Children := 1;
    ProgFREEINSTANCE_110.Add(DW_AT_name, DW_FORM_string, 'FREEINSTANCE'+#0);
    ProgFREEINSTANCE_110.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgFREEINSTANCE_110.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgFREEINSTANCE_110.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgFREEINSTANCE_110.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgFREEINSTANCE_110.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $0E]));

      Varthis_111 := ProgFREEINSTANCE_110.GetNewChild;
      Varthis_111.Tag := DW_TAG_formal_parameter;
      Varthis_111.Children := 0;
      Varthis_111.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_111.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_111.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

    ProgSAFECALLEXCEPTION_112 := TypeTOBJECT_100.GetNewChild;
    ProgSAFECALLEXCEPTION_112.Tag := DW_TAG_subprogram;
    ProgSAFECALLEXCEPTION_112.Children := 1;
    ProgSAFECALLEXCEPTION_112.Add(DW_AT_name, DW_FORM_string, 'SAFECALLEXCEPTION'+#0);
    ProgSAFECALLEXCEPTION_112.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgSAFECALLEXCEPTION_112.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgSAFECALLEXCEPTION_112.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgSAFECALLEXCEPTION_112.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgSAFECALLEXCEPTION_112.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $0F]));
    ProgSAFECALLEXCEPTION_112.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclHRESULT_215); // $72, $0B, $00, $00

      Varthis_113 := ProgSAFECALLEXCEPTION_112.GetNewChild;
      Varthis_113.Tag := DW_TAG_formal_parameter;
      Varthis_113.Children := 0;
      Varthis_113.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_113.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_113.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

      VarEXCEPTOBJECT_114 := ProgSAFECALLEXCEPTION_112.GetNewChild;
      VarEXCEPTOBJECT_114.Tag := DW_TAG_formal_parameter;
      VarEXCEPTOBJECT_114.Children := 0;
      VarEXCEPTOBJECT_114.Add(DW_AT_name, DW_FORM_string, 'EXCEPTOBJECT'+#0);
      VarEXCEPTOBJECT_114.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

      VarEXCEPTADDR_115 := ProgSAFECALLEXCEPTION_112.GetNewChild;
      VarEXCEPTADDR_115.Tag := DW_TAG_formal_parameter;
      VarEXCEPTADDR_115.Children := 0;
      VarEXCEPTADDR_115.Add(DW_AT_name, DW_FORM_string, 'EXCEPTADDR'+#0);
      VarEXCEPTADDR_115.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_210); // $55, $0B, $00, $00

    ProgDEFAULTHANDLER_116 := TypeTOBJECT_100.GetNewChild;
    ProgDEFAULTHANDLER_116.Tag := DW_TAG_subprogram;
    ProgDEFAULTHANDLER_116.Children := 1;
    ProgDEFAULTHANDLER_116.Add(DW_AT_name, DW_FORM_string, 'DEFAULTHANDLER'+#0);
    ProgDEFAULTHANDLER_116.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgDEFAULTHANDLER_116.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgDEFAULTHANDLER_116.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgDEFAULTHANDLER_116.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgDEFAULTHANDLER_116.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $10]));

      Varthis_117 := ProgDEFAULTHANDLER_116.GetNewChild;
      Varthis_117.Tag := DW_TAG_formal_parameter;
      Varthis_117.Children := 0;
      Varthis_117.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_117.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_117.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

      VarMESSAGE_118 := ProgDEFAULTHANDLER_116.GetNewChild;
      VarMESSAGE_118.Tag := DW_TAG_formal_parameter;
      VarMESSAGE_118.Children := 0;
      VarMESSAGE_118.Add(DW_AT_name, DW_FORM_string, 'MESSAGE'+#0);
      VarMESSAGE_118.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_218); // $8F, $0B, $00, $00

    ProgFREE_119 := TypeTOBJECT_100.GetNewChild;
    ProgFREE_119.Tag := DW_TAG_subprogram;
    ProgFREE_119.Children := 1;
    ProgFREE_119.Add(DW_AT_name, DW_FORM_string, 'FREE'+#0);
    ProgFREE_119.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgFREE_119.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgFREE_119.Add(DW_AT_external, DW_FORM_flag, [$01]);

      Varthis_120 := ProgFREE_119.GetNewChild;
      Varthis_120.Tag := DW_TAG_formal_parameter;
      Varthis_120.Children := 0;
      Varthis_120.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_120.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_120.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

    ProgINITINSTANCE_121 := TypeTOBJECT_100.GetNewChild;
    ProgINITINSTANCE_121.Tag := DW_TAG_subprogram;
    ProgINITINSTANCE_121.Children := 1;
    ProgINITINSTANCE_121.Add(DW_AT_name, DW_FORM_string, 'INITINSTANCE'+#0);
    ProgINITINSTANCE_121.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgINITINSTANCE_121.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgINITINSTANCE_121.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgINITINSTANCE_121.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

      Varself_122 := ProgINITINSTANCE_121.GetNewChild;
      Varself_122.Tag := DW_TAG_formal_parameter;
      Varself_122.Children := 0;
      Varself_122.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_122.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_122.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_221); // $AD, $0B, $00, $00

      VarINSTANCE_123 := ProgINITINSTANCE_121.GetNewChild;
      VarINSTANCE_123.Tag := DW_TAG_formal_parameter;
      VarINSTANCE_123.Children := 0;
      VarINSTANCE_123.Add(DW_AT_name, DW_FORM_string, 'INSTANCE'+#0);
      VarINSTANCE_123.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_210); // $55, $0B, $00, $00

    ProgCLEANUPINSTANCE_124 := TypeTOBJECT_100.GetNewChild;
    ProgCLEANUPINSTANCE_124.Tag := DW_TAG_subprogram;
    ProgCLEANUPINSTANCE_124.Children := 1;
    ProgCLEANUPINSTANCE_124.Add(DW_AT_name, DW_FORM_string, 'CLEANUPINSTANCE'+#0);
    ProgCLEANUPINSTANCE_124.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLEANUPINSTANCE_124.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLEANUPINSTANCE_124.Add(DW_AT_external, DW_FORM_flag, [$01]);

      Varthis_125 := ProgCLEANUPINSTANCE_124.GetNewChild;
      Varthis_125.Tag := DW_TAG_formal_parameter;
      Varthis_125.Children := 0;
      Varthis_125.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_125.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_125.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

    ProgCLASSTYPE_126 := TypeTOBJECT_100.GetNewChild;
    ProgCLASSTYPE_126.Tag := DW_TAG_subprogram;
    ProgCLASSTYPE_126.Children := 1;
    ProgCLASSTYPE_126.Add(DW_AT_name, DW_FORM_string, 'CLASSTYPE'+#0);
    ProgCLASSTYPE_126.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLASSTYPE_126.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLASSTYPE_126.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCLASSTYPE_126.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTCLASS_223); // $B7, $0B, $00, $00

      Varself_127 := ProgCLASSTYPE_126.GetNewChild;
      Varself_127.Tag := DW_TAG_formal_parameter;
      Varself_127.Children := 0;
      Varself_127.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_127.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_127.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_226); // $CD, $0B, $00, $00

    ProgCLASSINFO_128 := TypeTOBJECT_100.GetNewChild;
    ProgCLASSINFO_128.Tag := DW_TAG_subprogram;
    ProgCLASSINFO_128.Children := 1;
    ProgCLASSINFO_128.Add(DW_AT_name, DW_FORM_string, 'CLASSINFO'+#0);
    ProgCLASSINFO_128.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLASSINFO_128.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLASSINFO_128.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCLASSINFO_128.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_210); // $55, $0B, $00, $00

      Varself_129 := ProgCLASSINFO_128.GetNewChild;
      Varself_129.Tag := DW_TAG_formal_parameter;
      Varself_129.Children := 0;
      Varself_129.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_129.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_129.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_228); // $D7, $0B, $00, $00

    ProgCLASSNAME_130 := TypeTOBJECT_100.GetNewChild;
    ProgCLASSNAME_130.Tag := DW_TAG_subprogram;
    ProgCLASSNAME_130.Children := 1;
    ProgCLASSNAME_130.Add(DW_AT_name, DW_FORM_string, 'CLASSNAME'+#0);
    ProgCLASSNAME_130.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLASSNAME_130.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLASSNAME_130.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCLASSNAME_130.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_230); // $E1, $0B, $00, $00

      Varself_131 := ProgCLASSNAME_130.GetNewChild;
      Varself_131.Tag := DW_TAG_formal_parameter;
      Varself_131.Children := 0;
      Varself_131.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_131.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_131.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_237); // $32, $0C, $00, $00

      Varresult_132 := ProgCLASSNAME_130.GetNewChild;
      Varresult_132.Tag := DW_TAG_variable;
      Varresult_132.Children := 0;
      Varresult_132.Add(DW_AT_name, DW_FORM_string, 'result'+#0);
      Varresult_132.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_230); // $E1, $0B, $00, $00

    ProgCLASSNAMEIS_133 := TypeTOBJECT_100.GetNewChild;
    ProgCLASSNAMEIS_133.Tag := DW_TAG_subprogram;
    ProgCLASSNAMEIS_133.Children := 1;
    ProgCLASSNAMEIS_133.Add(DW_AT_name, DW_FORM_string, 'CLASSNAMEIS'+#0);
    ProgCLASSNAMEIS_133.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLASSNAMEIS_133.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLASSNAMEIS_133.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCLASSNAMEIS_133.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_50); // $87, $02, $00, $00

      Varself_134 := ProgCLASSNAMEIS_133.GetNewChild;
      Varself_134.Tag := DW_TAG_formal_parameter;
      Varself_134.Children := 0;
      Varself_134.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_134.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_134.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_239); // $3C, $0C, $00, $00

      VarNAME_135 := ProgCLASSNAMEIS_133.GetNewChild;
      VarNAME_135.Tag := DW_TAG_formal_parameter;
      VarNAME_135.Children := 0;
      VarNAME_135.Add(DW_AT_name, DW_FORM_string, 'NAME'+#0);
      VarNAME_135.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_230); // $E1, $0B, $00, $00

    ProgCLASSPARENT_136 := TypeTOBJECT_100.GetNewChild;
    ProgCLASSPARENT_136.Tag := DW_TAG_subprogram;
    ProgCLASSPARENT_136.Children := 1;
    ProgCLASSPARENT_136.Add(DW_AT_name, DW_FORM_string, 'CLASSPARENT'+#0);
    ProgCLASSPARENT_136.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLASSPARENT_136.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLASSPARENT_136.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCLASSPARENT_136.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTCLASS_223); // $B7, $0B, $00, $00

      Varself_137 := ProgCLASSPARENT_136.GetNewChild;
      Varself_137.Tag := DW_TAG_formal_parameter;
      Varself_137.Children := 0;
      Varself_137.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_137.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_137.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_241); // $46, $0C, $00, $00

    ProgINSTANCESIZE_138 := TypeTOBJECT_100.GetNewChild;
    ProgINSTANCESIZE_138.Tag := DW_TAG_subprogram;
    ProgINSTANCESIZE_138.Children := 1;
    ProgINSTANCESIZE_138.Add(DW_AT_name, DW_FORM_string, 'INSTANCESIZE'+#0);
    ProgINSTANCESIZE_138.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgINSTANCESIZE_138.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgINSTANCESIZE_138.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgINSTANCESIZE_138.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_47); // $6A, $02, $00, $00

      Varself_139 := ProgINSTANCESIZE_138.GetNewChild;
      Varself_139.Tag := DW_TAG_formal_parameter;
      Varself_139.Children := 0;
      Varself_139.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_139.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_139.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_243); // $50, $0C, $00, $00

    ProgINHERITSFROM_140 := TypeTOBJECT_100.GetNewChild;
    ProgINHERITSFROM_140.Tag := DW_TAG_subprogram;
    ProgINHERITSFROM_140.Children := 1;
    ProgINHERITSFROM_140.Add(DW_AT_name, DW_FORM_string, 'INHERITSFROM'+#0);
    ProgINHERITSFROM_140.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgINHERITSFROM_140.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgINHERITSFROM_140.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgINHERITSFROM_140.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_50); // $87, $02, $00, $00

      Varself_141 := ProgINHERITSFROM_140.GetNewChild;
      Varself_141.Tag := DW_TAG_formal_parameter;
      Varself_141.Children := 0;
      Varself_141.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_141.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_141.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_245); // $5A, $0C, $00, $00

      VarACLASS_142 := ProgINHERITSFROM_140.GetNewChild;
      VarACLASS_142.Tag := DW_TAG_formal_parameter;
      VarACLASS_142.Children := 0;
      VarACLASS_142.Add(DW_AT_name, DW_FORM_string, 'ACLASS'+#0);
      VarACLASS_142.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTCLASS_223); // $B7, $0B, $00, $00

    ProgSTRINGMESSAGETABLE_143 := TypeTOBJECT_100.GetNewChild;
    ProgSTRINGMESSAGETABLE_143.Tag := DW_TAG_subprogram;
    ProgSTRINGMESSAGETABLE_143.Children := 1;
    ProgSTRINGMESSAGETABLE_143.Add(DW_AT_name, DW_FORM_string, 'STRINGMESSAGETABLE'+#0);
    ProgSTRINGMESSAGETABLE_143.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgSTRINGMESSAGETABLE_143.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgSTRINGMESSAGETABLE_143.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgSTRINGMESSAGETABLE_143.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSTRINGMESSAGETABLE_247); // $64, $0C, $00, $00

      Varself_144 := ProgSTRINGMESSAGETABLE_143.GetNewChild;
      Varself_144.Tag := DW_TAG_formal_parameter;
      Varself_144.Children := 0;
      Varself_144.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_144.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_144.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_250); // $87, $0C, $00, $00

    ProgMETHODADDRESS_145 := TypeTOBJECT_100.GetNewChild;
    ProgMETHODADDRESS_145.Tag := DW_TAG_subprogram;
    ProgMETHODADDRESS_145.Children := 1;
    ProgMETHODADDRESS_145.Add(DW_AT_name, DW_FORM_string, 'METHODADDRESS'+#0);
    ProgMETHODADDRESS_145.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgMETHODADDRESS_145.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgMETHODADDRESS_145.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgMETHODADDRESS_145.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_210); // $55, $0B, $00, $00

      Varself_146 := ProgMETHODADDRESS_145.GetNewChild;
      Varself_146.Tag := DW_TAG_formal_parameter;
      Varself_146.Children := 0;
      Varself_146.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_146.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_146.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_252); // $91, $0C, $00, $00

      VarNAME_147 := ProgMETHODADDRESS_145.GetNewChild;
      VarNAME_147.Tag := DW_TAG_formal_parameter;
      VarNAME_147.Children := 0;
      VarNAME_147.Add(DW_AT_name, DW_FORM_string, 'NAME'+#0);
      VarNAME_147.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_230); // $E1, $0B, $00, $00

    ProgMETHODNAME_148 := TypeTOBJECT_100.GetNewChild;
    ProgMETHODNAME_148.Tag := DW_TAG_subprogram;
    ProgMETHODNAME_148.Children := 1;
    ProgMETHODNAME_148.Add(DW_AT_name, DW_FORM_string, 'METHODNAME'+#0);
    ProgMETHODNAME_148.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgMETHODNAME_148.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgMETHODNAME_148.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgMETHODNAME_148.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_230); // $E1, $0B, $00, $00

      Varself_149 := ProgMETHODNAME_148.GetNewChild;
      Varself_149.Tag := DW_TAG_formal_parameter;
      Varself_149.Children := 0;
      Varself_149.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_149.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_149.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_254); // $9B, $0C, $00, $00

      VarADDRESS_150 := ProgMETHODNAME_148.GetNewChild;
      VarADDRESS_150.Tag := DW_TAG_formal_parameter;
      VarADDRESS_150.Children := 0;
      VarADDRESS_150.Add(DW_AT_name, DW_FORM_string, 'ADDRESS'+#0);
      VarADDRESS_150.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_210); // $55, $0B, $00, $00

      Varresult_151 := ProgMETHODNAME_148.GetNewChild;
      Varresult_151.Tag := DW_TAG_variable;
      Varresult_151.Children := 0;
      Varresult_151.Add(DW_AT_name, DW_FORM_string, 'result'+#0);
      Varresult_151.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_230); // $E1, $0B, $00, $00

    ProgFIELDADDRESS_152 := TypeTOBJECT_100.GetNewChild;
    ProgFIELDADDRESS_152.Tag := DW_TAG_subprogram;
    ProgFIELDADDRESS_152.Children := 1;
    ProgFIELDADDRESS_152.Add(DW_AT_name, DW_FORM_string, 'FIELDADDRESS'+#0);
    ProgFIELDADDRESS_152.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgFIELDADDRESS_152.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgFIELDADDRESS_152.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgFIELDADDRESS_152.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_210); // $55, $0B, $00, $00

      Varthis_153 := ProgFIELDADDRESS_152.GetNewChild;
      Varthis_153.Tag := DW_TAG_formal_parameter;
      Varthis_153.Children := 0;
      Varthis_153.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_153.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_153.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

      VarNAME_154 := ProgFIELDADDRESS_152.GetNewChild;
      VarNAME_154.Tag := DW_TAG_formal_parameter;
      VarNAME_154.Children := 0;
      VarNAME_154.Add(DW_AT_name, DW_FORM_string, 'NAME'+#0);
      VarNAME_154.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_230); // $E1, $0B, $00, $00

    ProgAFTERCONSTRUCTION_155 := TypeTOBJECT_100.GetNewChild;
    ProgAFTERCONSTRUCTION_155.Tag := DW_TAG_subprogram;
    ProgAFTERCONSTRUCTION_155.Children := 1;
    ProgAFTERCONSTRUCTION_155.Add(DW_AT_name, DW_FORM_string, 'AFTERCONSTRUCTION'+#0);
    ProgAFTERCONSTRUCTION_155.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgAFTERCONSTRUCTION_155.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgAFTERCONSTRUCTION_155.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgAFTERCONSTRUCTION_155.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgAFTERCONSTRUCTION_155.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $11]));

      Varthis_156 := ProgAFTERCONSTRUCTION_155.GetNewChild;
      Varthis_156.Tag := DW_TAG_formal_parameter;
      Varthis_156.Children := 0;
      Varthis_156.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_156.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_156.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

    ProgBEFOREDESTRUCTION_157 := TypeTOBJECT_100.GetNewChild;
    ProgBEFOREDESTRUCTION_157.Tag := DW_TAG_subprogram;
    ProgBEFOREDESTRUCTION_157.Children := 1;
    ProgBEFOREDESTRUCTION_157.Add(DW_AT_name, DW_FORM_string, 'BEFOREDESTRUCTION'+#0);
    ProgBEFOREDESTRUCTION_157.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgBEFOREDESTRUCTION_157.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgBEFOREDESTRUCTION_157.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgBEFOREDESTRUCTION_157.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgBEFOREDESTRUCTION_157.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $12]));

      Varthis_158 := ProgBEFOREDESTRUCTION_157.GetNewChild;
      Varthis_158.Tag := DW_TAG_formal_parameter;
      Varthis_158.Children := 0;
      Varthis_158.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_158.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_158.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

    ProgDEFAULTHANDLERSTR_159 := TypeTOBJECT_100.GetNewChild;
    ProgDEFAULTHANDLERSTR_159.Tag := DW_TAG_subprogram;
    ProgDEFAULTHANDLERSTR_159.Children := 1;
    ProgDEFAULTHANDLERSTR_159.Add(DW_AT_name, DW_FORM_string, 'DEFAULTHANDLERSTR'+#0);
    ProgDEFAULTHANDLERSTR_159.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgDEFAULTHANDLERSTR_159.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgDEFAULTHANDLERSTR_159.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgDEFAULTHANDLERSTR_159.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgDEFAULTHANDLERSTR_159.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $13]));

      Varthis_160 := ProgDEFAULTHANDLERSTR_159.GetNewChild;
      Varthis_160.Tag := DW_TAG_formal_parameter;
      Varthis_160.Children := 0;
      Varthis_160.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_160.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_160.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

      VarMESSAGE_161 := ProgDEFAULTHANDLERSTR_159.GetNewChild;
      VarMESSAGE_161.Tag := DW_TAG_formal_parameter;
      VarMESSAGE_161.Children := 0;
      VarMESSAGE_161.Add(DW_AT_name, DW_FORM_string, 'MESSAGE'+#0);
      VarMESSAGE_161.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_218); // $8F, $0B, $00, $00

    ProgDISPATCH_162 := TypeTOBJECT_100.GetNewChild;
    ProgDISPATCH_162.Tag := DW_TAG_subprogram;
    ProgDISPATCH_162.Children := 1;
    ProgDISPATCH_162.Add(DW_AT_name, DW_FORM_string, 'DISPATCH'+#0);
    ProgDISPATCH_162.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgDISPATCH_162.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgDISPATCH_162.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgDISPATCH_162.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgDISPATCH_162.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $14]));

      Varthis_163 := ProgDISPATCH_162.GetNewChild;
      Varthis_163.Tag := DW_TAG_formal_parameter;
      Varthis_163.Children := 0;
      Varthis_163.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_163.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_163.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

      VarMESSAGE_164 := ProgDISPATCH_162.GetNewChild;
      VarMESSAGE_164.Tag := DW_TAG_formal_parameter;
      VarMESSAGE_164.Children := 0;
      VarMESSAGE_164.Add(DW_AT_name, DW_FORM_string, 'MESSAGE'+#0);
      VarMESSAGE_164.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_218); // $8F, $0B, $00, $00

    ProgDISPATCHSTR_165 := TypeTOBJECT_100.GetNewChild;
    ProgDISPATCHSTR_165.Tag := DW_TAG_subprogram;
    ProgDISPATCHSTR_165.Children := 1;
    ProgDISPATCHSTR_165.Add(DW_AT_name, DW_FORM_string, 'DISPATCHSTR'+#0);
    ProgDISPATCHSTR_165.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgDISPATCHSTR_165.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgDISPATCHSTR_165.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgDISPATCHSTR_165.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgDISPATCHSTR_165.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $15]));

      Varthis_166 := ProgDISPATCHSTR_165.GetNewChild;
      Varthis_166.Tag := DW_TAG_formal_parameter;
      Varthis_166.Children := 0;
      Varthis_166.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_166.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_166.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

      VarMESSAGE_167 := ProgDISPATCHSTR_165.GetNewChild;
      VarMESSAGE_167.Tag := DW_TAG_formal_parameter;
      VarMESSAGE_167.Children := 0;
      VarMESSAGE_167.Add(DW_AT_name, DW_FORM_string, 'MESSAGE'+#0);
      VarMESSAGE_167.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_218); // $8F, $0B, $00, $00

    ProgGETINTERFACE_168 := TypeTOBJECT_100.GetNewChild;
    ProgGETINTERFACE_168.Tag := DW_TAG_subprogram;
    ProgGETINTERFACE_168.Children := 1;
    ProgGETINTERFACE_168.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACE'+#0);
    ProgGETINTERFACE_168.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACE_168.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACE_168.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACE_168.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_50); // $87, $02, $00, $00

      Varthis_169 := ProgGETINTERFACE_168.GetNewChild;
      Varthis_169.Tag := DW_TAG_formal_parameter;
      Varthis_169.Children := 0;
      Varthis_169.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_169.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_169.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

      VarIID_170 := ProgGETINTERFACE_168.GetNewChild;
      VarIID_170.Tag := DW_TAG_formal_parameter;
      VarIID_170.Children := 0;
      VarIID_170.Add(DW_AT_name, DW_FORM_string, 'IID'+#0);
      VarIID_170.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTGUID_256); // $A5, $0C, $00, $00

      VarOBJ_171 := ProgGETINTERFACE_168.GetNewChild;
      VarOBJ_171.Tag := DW_TAG_formal_parameter;
      VarOBJ_171.Children := 0;
      VarOBJ_171.Add(DW_AT_name, DW_FORM_string, 'OBJ'+#0);
      VarOBJ_171.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_218); // $8F, $0B, $00, $00

    ProgGETINTERFACE_172 := TypeTOBJECT_100.GetNewChild;
    ProgGETINTERFACE_172.Tag := DW_TAG_subprogram;
    ProgGETINTERFACE_172.Children := 1;
    ProgGETINTERFACE_172.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACE'+#0);
    ProgGETINTERFACE_172.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACE_172.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACE_172.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACE_172.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_50); // $87, $02, $00, $00

      Varthis_173 := ProgGETINTERFACE_172.GetNewChild;
      Varthis_173.Tag := DW_TAG_formal_parameter;
      Varthis_173.Children := 0;
      Varthis_173.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_173.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_173.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

      VarIIDSTR_174 := ProgGETINTERFACE_172.GetNewChild;
      VarIIDSTR_174.Tag := DW_TAG_formal_parameter;
      VarIIDSTR_174.Children := 0;
      VarIIDSTR_174.Add(DW_AT_name, DW_FORM_string, 'IIDSTR'+#0);
      VarIIDSTR_174.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_230); // $E1, $0B, $00, $00

      VarOBJ_175 := ProgGETINTERFACE_172.GetNewChild;
      VarOBJ_175.Tag := DW_TAG_formal_parameter;
      VarOBJ_175.Children := 0;
      VarOBJ_175.Add(DW_AT_name, DW_FORM_string, 'OBJ'+#0);
      VarOBJ_175.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_218); // $8F, $0B, $00, $00

    ProgGETINTERFACEBYSTR_176 := TypeTOBJECT_100.GetNewChild;
    ProgGETINTERFACEBYSTR_176.Tag := DW_TAG_subprogram;
    ProgGETINTERFACEBYSTR_176.Children := 1;
    ProgGETINTERFACEBYSTR_176.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACEBYSTR'+#0);
    ProgGETINTERFACEBYSTR_176.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACEBYSTR_176.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACEBYSTR_176.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACEBYSTR_176.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_50); // $87, $02, $00, $00

      Varthis_177 := ProgGETINTERFACEBYSTR_176.GetNewChild;
      Varthis_177.Tag := DW_TAG_formal_parameter;
      Varthis_177.Children := 0;
      Varthis_177.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_177.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_177.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

      VarIIDSTR_178 := ProgGETINTERFACEBYSTR_176.GetNewChild;
      VarIIDSTR_178.Tag := DW_TAG_formal_parameter;
      VarIIDSTR_178.Children := 0;
      VarIIDSTR_178.Add(DW_AT_name, DW_FORM_string, 'IIDSTR'+#0);
      VarIIDSTR_178.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_230); // $E1, $0B, $00, $00

      VarOBJ_179 := ProgGETINTERFACEBYSTR_176.GetNewChild;
      VarOBJ_179.Tag := DW_TAG_formal_parameter;
      VarOBJ_179.Children := 0;
      VarOBJ_179.Add(DW_AT_name, DW_FORM_string, 'OBJ'+#0);
      VarOBJ_179.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_218); // $8F, $0B, $00, $00

    ProgGETINTERFACEWEAK_180 := TypeTOBJECT_100.GetNewChild;
    ProgGETINTERFACEWEAK_180.Tag := DW_TAG_subprogram;
    ProgGETINTERFACEWEAK_180.Children := 1;
    ProgGETINTERFACEWEAK_180.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACEWEAK'+#0);
    ProgGETINTERFACEWEAK_180.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACEWEAK_180.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACEWEAK_180.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACEWEAK_180.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_50); // $87, $02, $00, $00

      Varthis_181 := ProgGETINTERFACEWEAK_180.GetNewChild;
      Varthis_181.Tag := DW_TAG_formal_parameter;
      Varthis_181.Children := 0;
      Varthis_181.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_181.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_181.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

      VarIID_182 := ProgGETINTERFACEWEAK_180.GetNewChild;
      VarIID_182.Tag := DW_TAG_formal_parameter;
      VarIID_182.Children := 0;
      VarIID_182.Add(DW_AT_name, DW_FORM_string, 'IID'+#0);
      VarIID_182.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTGUID_256); // $A5, $0C, $00, $00

      VarOBJ_183 := ProgGETINTERFACEWEAK_180.GetNewChild;
      VarOBJ_183.Tag := DW_TAG_formal_parameter;
      VarOBJ_183.Children := 0;
      VarOBJ_183.Add(DW_AT_name, DW_FORM_string, 'OBJ'+#0);
      VarOBJ_183.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_218); // $8F, $0B, $00, $00

    ProgGETINTERFACEENTRY_184 := TypeTOBJECT_100.GetNewChild;
    ProgGETINTERFACEENTRY_184.Tag := DW_TAG_subprogram;
    ProgGETINTERFACEENTRY_184.Children := 1;
    ProgGETINTERFACEENTRY_184.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACEENTRY'+#0);
    ProgGETINTERFACEENTRY_184.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACEENTRY_184.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACEENTRY_184.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACEENTRY_184.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTERFACEENTRY_273); // $A5, $0D, $00, $00

      Varself_185 := ProgGETINTERFACEENTRY_184.GetNewChild;
      Varself_185.Tag := DW_TAG_formal_parameter;
      Varself_185.Children := 0;
      Varself_185.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_185.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_185.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_276); // $C4, $0D, $00, $00

      VarIID_186 := ProgGETINTERFACEENTRY_184.GetNewChild;
      VarIID_186.Tag := DW_TAG_formal_parameter;
      VarIID_186.Children := 0;
      VarIID_186.Add(DW_AT_name, DW_FORM_string, 'IID'+#0);
      VarIID_186.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTGUID_256); // $A5, $0C, $00, $00

    ProgGETINTERFACEENTRYBYSTR_187 := TypeTOBJECT_100.GetNewChild;
    ProgGETINTERFACEENTRYBYSTR_187.Tag := DW_TAG_subprogram;
    ProgGETINTERFACEENTRYBYSTR_187.Children := 1;
    ProgGETINTERFACEENTRYBYSTR_187.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACEENTRYBYSTR'+#0);
    ProgGETINTERFACEENTRYBYSTR_187.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACEENTRYBYSTR_187.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACEENTRYBYSTR_187.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACEENTRYBYSTR_187.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTERFACEENTRY_273); // $A5, $0D, $00, $00

      Varself_188 := ProgGETINTERFACEENTRYBYSTR_187.GetNewChild;
      Varself_188.Tag := DW_TAG_formal_parameter;
      Varself_188.Children := 0;
      Varself_188.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_188.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_188.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_278); // $CE, $0D, $00, $00

      VarIIDSTR_189 := ProgGETINTERFACEENTRYBYSTR_187.GetNewChild;
      VarIIDSTR_189.Tag := DW_TAG_formal_parameter;
      VarIIDSTR_189.Children := 0;
      VarIIDSTR_189.Add(DW_AT_name, DW_FORM_string, 'IIDSTR'+#0);
      VarIIDSTR_189.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_230); // $E1, $0B, $00, $00

    ProgGETINTERFACETABLE_190 := TypeTOBJECT_100.GetNewChild;
    ProgGETINTERFACETABLE_190.Tag := DW_TAG_subprogram;
    ProgGETINTERFACETABLE_190.Children := 1;
    ProgGETINTERFACETABLE_190.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACETABLE'+#0);
    ProgGETINTERFACETABLE_190.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACETABLE_190.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACETABLE_190.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACETABLE_190.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTERFACETABLE_280); // $D8, $0D, $00, $00

      Varself_191 := ProgGETINTERFACETABLE_190.GetNewChild;
      Varself_191.Tag := DW_TAG_formal_parameter;
      Varself_191.Children := 0;
      Varself_191.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_191.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_191.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_283); // $F7, $0D, $00, $00

    ProgUNITNAME_192 := TypeTOBJECT_100.GetNewChild;
    ProgUNITNAME_192.Tag := DW_TAG_subprogram;
    ProgUNITNAME_192.Children := 1;
    ProgUNITNAME_192.Add(DW_AT_name, DW_FORM_string, 'UNITNAME'+#0);
    ProgUNITNAME_192.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgUNITNAME_192.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgUNITNAME_192.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgUNITNAME_192.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclANSISTRING_285); // $01, $0E, $00, $00

      Varself_193 := ProgUNITNAME_192.GetNewChild;
      Varself_193.Tag := DW_TAG_formal_parameter;
      Varself_193.Children := 0;
      Varself_193.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_193.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_193.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_288); // $1B, $0E, $00, $00

      Varresult_194 := ProgUNITNAME_192.GetNewChild;
      Varresult_194.Tag := DW_TAG_variable;
      Varresult_194.Children := 0;
      Varresult_194.Add(DW_AT_name, DW_FORM_string, 'result'+#0);
      Varresult_194.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclANSISTRING_285); // $01, $0E, $00, $00

    ProgEQUALS_195 := TypeTOBJECT_100.GetNewChild;
    ProgEQUALS_195.Tag := DW_TAG_subprogram;
    ProgEQUALS_195.Children := 1;
    ProgEQUALS_195.Add(DW_AT_name, DW_FORM_string, 'EQUALS'+#0);
    ProgEQUALS_195.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgEQUALS_195.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgEQUALS_195.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgEQUALS_195.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgEQUALS_195.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $16]));
    ProgEQUALS_195.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_50); // $87, $02, $00, $00

      Varthis_196 := ProgEQUALS_195.GetNewChild;
      Varthis_196.Tag := DW_TAG_formal_parameter;
      Varthis_196.Children := 0;
      Varthis_196.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_196.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_196.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

      VarOBJ_197 := ProgEQUALS_195.GetNewChild;
      VarOBJ_197.Tag := DW_TAG_formal_parameter;
      VarOBJ_197.Children := 0;
      VarOBJ_197.Add(DW_AT_name, DW_FORM_string, 'OBJ'+#0);
      VarOBJ_197.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

    ProgGETHASHCODE_198 := TypeTOBJECT_100.GetNewChild;
    ProgGETHASHCODE_198.Tag := DW_TAG_subprogram;
    ProgGETHASHCODE_198.Children := 1;
    ProgGETHASHCODE_198.Add(DW_AT_name, DW_FORM_string, 'GETHASHCODE'+#0);
    ProgGETHASHCODE_198.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETHASHCODE_198.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETHASHCODE_198.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETHASHCODE_198.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgGETHASHCODE_198.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $17]));
    ProgGETHASHCODE_198.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_47); // $6A, $02, $00, $00

      Varthis_199 := ProgGETHASHCODE_198.GetNewChild;
      Varthis_199.Tag := DW_TAG_formal_parameter;
      Varthis_199.Children := 0;
      Varthis_199.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_199.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_199.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

    ProgTOSTRING_200 := TypeTOBJECT_100.GetNewChild;
    ProgTOSTRING_200.Tag := DW_TAG_subprogram;
    ProgTOSTRING_200.Children := 1;
    ProgTOSTRING_200.Add(DW_AT_name, DW_FORM_string, 'TOSTRING'+#0);
    ProgTOSTRING_200.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgTOSTRING_200.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgTOSTRING_200.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgTOSTRING_200.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgTOSTRING_200.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $18]));
    ProgTOSTRING_200.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclANSISTRING_285); // $01, $0E, $00, $00

      Varthis_201 := ProgTOSTRING_200.GetNewChild;
      Varthis_201.Tag := DW_TAG_formal_parameter;
      Varthis_201.Children := 0;
      Varthis_201.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_201.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_201.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

      Varresult_202 := ProgTOSTRING_200.GetNewChild;
      Varresult_202.Tag := DW_TAG_variable;
      Varresult_202.Children := 0;
      Varresult_202.Add(DW_AT_name, DW_FORM_string, 'result'+#0);
      Varresult_202.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclANSISTRING_285); // $01, $0E, $00, $00

  Type_203 := Unittestprog2_pas_0.GetNewChild;
  Type_203.Tag := DW_TAG_reference_type;
  Type_203.Children := 0;
  Type_203.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_98); // $E9, $04, $00, $00

  TypeDeclQWORD_204 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclQWORD_204.Tag := DW_TAG_typedef;
  TypeDeclQWORD_204.Children := 0;
  TypeDeclQWORD_204.Add(DW_AT_name, DW_FORM_string, 'QWORD'+#0);
  TypeDeclQWORD_204.AddRef(DW_AT_type, DW_FORM_ref4, @TypeQWord_205); // $2E, $0B, $00, $00

  TypeQWord_205 := Unittestprog2_pas_0.GetNewChild;
  TypeQWord_205.Tag := DW_TAG_base_type;
  TypeQWord_205.Children := 0;
  TypeQWord_205.Add(DW_AT_name, DW_FORM_string, 'QWord'+#0);
  TypeQWord_205.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeQWord_205.Add(DW_AT_byte_size, DW_FORM_data1, [$08]);

  Type_206 := Unittestprog2_pas_0.GetNewChild;
  Type_206.Tag := DW_TAG_reference_type;
  Type_206.Children := 0;
  Type_206.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_204); // $23, $0B, $00, $00

  TypeDeclINT64_207 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclINT64_207.Tag := DW_TAG_typedef;
  TypeDeclINT64_207.Children := 0;
  TypeDeclINT64_207.Add(DW_AT_name, DW_FORM_string, 'INT64'+#0);
  TypeDeclINT64_207.AddRef(DW_AT_type, DW_FORM_ref4, @TypeInt64_208); // $47, $0B, $00, $00

  TypeInt64_208 := Unittestprog2_pas_0.GetNewChild;
  TypeInt64_208.Tag := DW_TAG_base_type;
  TypeInt64_208.Children := 0;
  TypeInt64_208.Add(DW_AT_name, DW_FORM_string, 'Int64'+#0);
  TypeInt64_208.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeInt64_208.Add(DW_AT_byte_size, DW_FORM_data1, [$08]);

  Type_209 := Unittestprog2_pas_0.GetNewChild;
  Type_209.Tag := DW_TAG_reference_type;
  Type_209.Children := 0;
  Type_209.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclINT64_207); // $3C, $0B, $00, $00

  TypeDeclPOINTER_210 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclPOINTER_210.Tag := DW_TAG_typedef;
  TypeDeclPOINTER_210.Children := 0;
  TypeDeclPOINTER_210.Add(DW_AT_name, DW_FORM_string, 'POINTER'+#0);
  TypeDeclPOINTER_210.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_211); // $62, $0B, $00, $00

  TypePtr_211 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_211.Tag := DW_TAG_pointer_type;
  TypePtr_211.Children := 0;

  Type_212 := Unittestprog2_pas_0.GetNewChild;
  Type_212.Tag := DW_TAG_reference_type;
  Type_212.Children := 0;
  Type_212.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_210); // $55, $0B, $00, $00

  TypePtr_213 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_213.Tag := DW_TAG_pointer_type;
  TypePtr_213.Children := 0;
  TypePtr_213.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_290); // $25, $0E, $00, $00

  Type_214 := Unittestprog2_pas_0.GetNewChild;
  Type_214.Tag := DW_TAG_reference_type;
  Type_214.Children := 0;
  Type_214.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_213); // $68, $0B, $00, $00

  TypeDeclHRESULT_215 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclHRESULT_215.Tag := DW_TAG_typedef;
  TypeDeclHRESULT_215.Children := 0;
  TypeDeclHRESULT_215.Add(DW_AT_name, DW_FORM_string, 'HRESULT'+#0);
  TypeDeclHRESULT_215.AddRef(DW_AT_type, DW_FORM_ref4, @TypeHRESULT_216); // $7F, $0B, $00, $00

  TypeHRESULT_216 := Unittestprog2_pas_0.GetNewChild;
  TypeHRESULT_216.Tag := DW_TAG_base_type;
  TypeHRESULT_216.Children := 0;
  TypeHRESULT_216.Add(DW_AT_name, DW_FORM_string, 'HRESULT'+#0);
  TypeHRESULT_216.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeHRESULT_216.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

  Type_217 := Unittestprog2_pas_0.GetNewChild;
  Type_217.Tag := DW_TAG_reference_type;
  Type_217.Children := 0;
  Type_217.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclHRESULT_215); // $72, $0B, $00, $00

  TypeDeclformal_218 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclformal_218.Tag := DW_TAG_typedef;
  TypeDeclformal_218.Children := 0;
  TypeDeclformal_218.Add(DW_AT_name, DW_FORM_string, 'formal'+#0);
  TypeDeclformal_218.AddRef(DW_AT_type, DW_FORM_ref4, @TypeFormalDef_219); // $9B, $0B, $00, $00

  TypeFormalDef_219 := Unittestprog2_pas_0.GetNewChild;
  TypeFormalDef_219.Tag := DW_TAG_base_type;
  TypeFormalDef_219.Children := 0;
  TypeFormalDef_219.Add(DW_AT_name, DW_FORM_string, 'FormalDef'+#0);
  TypeFormalDef_219.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeFormalDef_219.Add(DW_AT_byte_size, DW_FORM_data1, [$00]);

  Type_220 := Unittestprog2_pas_0.GetNewChild;
  Type_220.Tag := DW_TAG_reference_type;
  Type_220.Children := 0;
  Type_220.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_218); // $8F, $0B, $00, $00

  TypePtr_221 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_221.Tag := DW_TAG_pointer_type;
  TypePtr_221.Children := 0;
  TypePtr_221.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_290); // $25, $0E, $00, $00

  Type_222 := Unittestprog2_pas_0.GetNewChild;
  Type_222.Tag := DW_TAG_reference_type;
  Type_222.Children := 0;
  Type_222.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_221); // $AD, $0B, $00, $00

  TypeDeclTCLASS_223 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclTCLASS_223.Tag := DW_TAG_typedef;
  TypeDeclTCLASS_223.Children := 0;
  TypeDeclTCLASS_223.Add(DW_AT_name, DW_FORM_string, 'TCLASS'+#0);
  TypeDeclTCLASS_223.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_224); // $C3, $0B, $00, $00

  TypePtr_224 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_224.Tag := DW_TAG_pointer_type;
  TypePtr_224.Children := 0;
  TypePtr_224.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_290); // $25, $0E, $00, $00

  Type_225 := Unittestprog2_pas_0.GetNewChild;
  Type_225.Tag := DW_TAG_reference_type;
  Type_225.Children := 0;
  Type_225.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTCLASS_223); // $B7, $0B, $00, $00

  TypePtr_226 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_226.Tag := DW_TAG_pointer_type;
  TypePtr_226.Children := 0;
  TypePtr_226.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_290); // $25, $0E, $00, $00

  Type_227 := Unittestprog2_pas_0.GetNewChild;
  Type_227.Tag := DW_TAG_reference_type;
  Type_227.Children := 0;
  Type_227.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_226); // $CD, $0B, $00, $00

  TypePtr_228 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_228.Tag := DW_TAG_pointer_type;
  TypePtr_228.Children := 0;
  TypePtr_228.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_290); // $25, $0E, $00, $00

  Type_229 := Unittestprog2_pas_0.GetNewChild;
  Type_229.Tag := DW_TAG_reference_type;
  Type_229.Children := 0;
  Type_229.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_228); // $D7, $0B, $00, $00

  TypeDeclSHORTSTRING_230 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclSHORTSTRING_230.Tag := DW_TAG_typedef;
  TypeDeclSHORTSTRING_230.Children := 0;
  TypeDeclSHORTSTRING_230.Add(DW_AT_name, DW_FORM_string, 'SHORTSTRING'+#0);
  TypeDeclSHORTSTRING_230.AddRef(DW_AT_type, DW_FORM_ref4, @TypeShortString_231); // $F2, $0B, $00, $00

  TypeShortString_231 := Unittestprog2_pas_0.GetNewChild;
  TypeShortString_231.Tag := DW_TAG_structure_type;
  TypeShortString_231.Children := 1;
  TypeShortString_231.Add(DW_AT_name, DW_FORM_string, 'ShortString'+#0);
  TypeShortString_231.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 256);

    Varlength_232 := TypeShortString_231.GetNewChild;
    Varlength_232.Tag := DW_TAG_member;
    Varlength_232.Children := 0;
    Varlength_232.Add(DW_AT_name, DW_FORM_string, 'length'+#0);
    Varlength_232.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    Varlength_232.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_38); // $1D, $02, $00, $00

    Varst_233 := TypeShortString_231.GetNewChild;
    Varst_233.Tag := DW_TAG_member;
    Varst_233.Children := 0;
    Varst_233.Add(DW_AT_name, DW_FORM_string, 'st'+#0);
    Varst_233.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(1)])); // $23, $01
    Varst_233.AddRef(DW_AT_type, DW_FORM_ref4, @Type_234); // $1C, $0C, $00, $00

  Type_234 := Unittestprog2_pas_0.GetNewChild;
  Type_234.Tag := DW_TAG_array_type;
  Type_234.Children := 1;
  Type_234.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 256);
  Type_234.AddSLEB(DW_AT_byte_stride, DW_FORM_udata, 1);
  Type_234.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclCHAR_293); // $42, $0E, $00, $00

    Type_235 := Type_234.GetNewChild;
    Type_235.Tag := DW_TAG_subrange_type;
    Type_235.Children := 0;
    Type_235.AddSLEB(DW_AT_lower_bound, DW_FORM_udata, 0);
    Type_235.AddSLEB(DW_AT_upper_bound, DW_FORM_udata, 255);
    Type_235.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_38); // $1D, $02, $00, $00

  Type_236 := Unittestprog2_pas_0.GetNewChild;
  Type_236.Tag := DW_TAG_reference_type;
  Type_236.Children := 0;
  Type_236.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_230); // $E1, $0B, $00, $00

  TypePtr_237 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_237.Tag := DW_TAG_pointer_type;
  TypePtr_237.Children := 0;
  TypePtr_237.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_290); // $25, $0E, $00, $00

  Type_238 := Unittestprog2_pas_0.GetNewChild;
  Type_238.Tag := DW_TAG_reference_type;
  Type_238.Children := 0;
  Type_238.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_237); // $32, $0C, $00, $00

  TypePtr_239 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_239.Tag := DW_TAG_pointer_type;
  TypePtr_239.Children := 0;
  TypePtr_239.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_290); // $25, $0E, $00, $00

  Type_240 := Unittestprog2_pas_0.GetNewChild;
  Type_240.Tag := DW_TAG_reference_type;
  Type_240.Children := 0;
  Type_240.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_239); // $3C, $0C, $00, $00

  TypePtr_241 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_241.Tag := DW_TAG_pointer_type;
  TypePtr_241.Children := 0;
  TypePtr_241.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_290); // $25, $0E, $00, $00

  Type_242 := Unittestprog2_pas_0.GetNewChild;
  Type_242.Tag := DW_TAG_reference_type;
  Type_242.Children := 0;
  Type_242.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_241); // $46, $0C, $00, $00

  TypePtr_243 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_243.Tag := DW_TAG_pointer_type;
  TypePtr_243.Children := 0;
  TypePtr_243.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_290); // $25, $0E, $00, $00

  Type_244 := Unittestprog2_pas_0.GetNewChild;
  Type_244.Tag := DW_TAG_reference_type;
  Type_244.Children := 0;
  Type_244.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_243); // $50, $0C, $00, $00

  TypePtr_245 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_245.Tag := DW_TAG_pointer_type;
  TypePtr_245.Children := 0;
  TypePtr_245.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_290); // $25, $0E, $00, $00

  Type_246 := Unittestprog2_pas_0.GetNewChild;
  Type_246.Tag := DW_TAG_reference_type;
  Type_246.Children := 0;
  Type_246.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_245); // $5A, $0C, $00, $00

  TypeDeclPSTRINGMESSAGETABLE_247 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclPSTRINGMESSAGETABLE_247.Tag := DW_TAG_typedef;
  TypeDeclPSTRINGMESSAGETABLE_247.Children := 0;
  TypeDeclPSTRINGMESSAGETABLE_247.Add(DW_AT_name, DW_FORM_string, 'PSTRINGMESSAGETABLE'+#0);
  TypeDeclPSTRINGMESSAGETABLE_247.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_248); // $7D, $0C, $00, $00

  TypePtr_248 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_248.Tag := DW_TAG_pointer_type;
  TypePtr_248.Children := 0;
  TypePtr_248.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSTRINGMESSAGETABLE_296); // $59, $0E, $00, $00

  Type_249 := Unittestprog2_pas_0.GetNewChild;
  Type_249.Tag := DW_TAG_reference_type;
  Type_249.Children := 0;
  Type_249.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSTRINGMESSAGETABLE_247); // $64, $0C, $00, $00

  TypePtr_250 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_250.Tag := DW_TAG_pointer_type;
  TypePtr_250.Children := 0;
  TypePtr_250.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_290); // $25, $0E, $00, $00

  Type_251 := Unittestprog2_pas_0.GetNewChild;
  Type_251.Tag := DW_TAG_reference_type;
  Type_251.Children := 0;
  Type_251.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_250); // $87, $0C, $00, $00

  TypePtr_252 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_252.Tag := DW_TAG_pointer_type;
  TypePtr_252.Children := 0;
  TypePtr_252.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_290); // $25, $0E, $00, $00

  Type_253 := Unittestprog2_pas_0.GetNewChild;
  Type_253.Tag := DW_TAG_reference_type;
  Type_253.Children := 0;
  Type_253.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_252); // $91, $0C, $00, $00

  TypePtr_254 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_254.Tag := DW_TAG_pointer_type;
  TypePtr_254.Children := 0;
  TypePtr_254.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_290); // $25, $0E, $00, $00

  Type_255 := Unittestprog2_pas_0.GetNewChild;
  Type_255.Tag := DW_TAG_reference_type;
  Type_255.Children := 0;
  Type_255.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_254); // $9B, $0C, $00, $00

  TypeDeclTGUID_256 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclTGUID_256.Tag := DW_TAG_typedef;
  TypeDeclTGUID_256.Children := 0;
  TypeDeclTGUID_256.Add(DW_AT_name, DW_FORM_string, 'TGUID'+#0);
  TypeDeclTGUID_256.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTGUID_257); // $B0, $0C, $00, $00

  TypeTGUID_257 := Unittestprog2_pas_0.GetNewChild;
  TypeTGUID_257.Tag := DW_TAG_structure_type;
  TypeTGUID_257.Children := 1;
  TypeTGUID_257.Add(DW_AT_name, DW_FORM_string, 'TGUID'+#0);
  TypeTGUID_257.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 16);

    VarDATA1_258 := TypeTGUID_257.GetNewChild;
    VarDATA1_258.Tag := DW_TAG_member;
    VarDATA1_258.Children := 0;
    VarDATA1_258.Add(DW_AT_name, DW_FORM_string, 'DATA1'+#0);
    VarDATA1_258.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarDATA1_258.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_301); // $B0, $0E, $00, $00

    VarDATA2_259 := TypeTGUID_257.GetNewChild;
    VarDATA2_259.Tag := DW_TAG_member;
    VarDATA2_259.Children := 0;
    VarDATA2_259.Add(DW_AT_name, DW_FORM_string, 'DATA2'+#0);
    VarDATA2_259.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarDATA2_259.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_44); // $53, $02, $00, $00

    VarDATA3_260 := TypeTGUID_257.GetNewChild;
    VarDATA3_260.Tag := DW_TAG_member;
    VarDATA3_260.Children := 0;
    VarDATA3_260.Add(DW_AT_name, DW_FORM_string, 'DATA3'+#0);
    VarDATA3_260.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(6)])); // $23, $06
    VarDATA3_260.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_44); // $53, $02, $00, $00

    VarDATA4_261 := TypeTGUID_257.GetNewChild;
    VarDATA4_261.Tag := DW_TAG_member;
    VarDATA4_261.Children := 0;
    VarDATA4_261.Add(DW_AT_name, DW_FORM_string, 'DATA4'+#0);
    VarDATA4_261.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(8)])); // $23, $08
    VarDATA4_261.AddRef(DW_AT_type, DW_FORM_ref4, @Type_304); // $CF, $0E, $00, $00

    VarD1_262 := TypeTGUID_257.GetNewChild;
    VarD1_262.Tag := DW_TAG_member;
    VarD1_262.Children := 0;
    VarD1_262.Add(DW_AT_name, DW_FORM_string, 'D1'+#0);
    VarD1_262.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarD1_262.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_301); // $B0, $0E, $00, $00

    VarD2_263 := TypeTGUID_257.GetNewChild;
    VarD2_263.Tag := DW_TAG_member;
    VarD2_263.Children := 0;
    VarD2_263.Add(DW_AT_name, DW_FORM_string, 'D2'+#0);
    VarD2_263.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarD2_263.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_44); // $53, $02, $00, $00

    VarD3_264 := TypeTGUID_257.GetNewChild;
    VarD3_264.Tag := DW_TAG_member;
    VarD3_264.Children := 0;
    VarD3_264.Add(DW_AT_name, DW_FORM_string, 'D3'+#0);
    VarD3_264.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(6)])); // $23, $06
    VarD3_264.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_44); // $53, $02, $00, $00

    VarD4_265 := TypeTGUID_257.GetNewChild;
    VarD4_265.Tag := DW_TAG_member;
    VarD4_265.Children := 0;
    VarD4_265.Add(DW_AT_name, DW_FORM_string, 'D4'+#0);
    VarD4_265.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(8)])); // $23, $08
    VarD4_265.AddRef(DW_AT_type, DW_FORM_ref4, @Type_307); // $E3, $0E, $00, $00

    VarTIME_LOW_266 := TypeTGUID_257.GetNewChild;
    VarTIME_LOW_266.Tag := DW_TAG_member;
    VarTIME_LOW_266.Children := 0;
    VarTIME_LOW_266.Add(DW_AT_name, DW_FORM_string, 'TIME_LOW'+#0);
    VarTIME_LOW_266.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarTIME_LOW_266.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_301); // $B0, $0E, $00, $00

    VarTIME_MID_267 := TypeTGUID_257.GetNewChild;
    VarTIME_MID_267.Tag := DW_TAG_member;
    VarTIME_MID_267.Children := 0;
    VarTIME_MID_267.Add(DW_AT_name, DW_FORM_string, 'TIME_MID'+#0);
    VarTIME_MID_267.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarTIME_MID_267.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_44); // $53, $02, $00, $00

    VarTIME_HI_AND_VERSION_268 := TypeTGUID_257.GetNewChild;
    VarTIME_HI_AND_VERSION_268.Tag := DW_TAG_member;
    VarTIME_HI_AND_VERSION_268.Children := 0;
    VarTIME_HI_AND_VERSION_268.Add(DW_AT_name, DW_FORM_string, 'TIME_HI_AND_VERSION'+#0);
    VarTIME_HI_AND_VERSION_268.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(6)])); // $23, $06
    VarTIME_HI_AND_VERSION_268.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_44); // $53, $02, $00, $00

    VarCLOCK_SEQ_HI_AND_RESERVED_269 := TypeTGUID_257.GetNewChild;
    VarCLOCK_SEQ_HI_AND_RESERVED_269.Tag := DW_TAG_member;
    VarCLOCK_SEQ_HI_AND_RESERVED_269.Children := 0;
    VarCLOCK_SEQ_HI_AND_RESERVED_269.Add(DW_AT_name, DW_FORM_string, 'CLOCK_SEQ_HI_AND_RESERVED'+#0);
    VarCLOCK_SEQ_HI_AND_RESERVED_269.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(8)])); // $23, $08
    VarCLOCK_SEQ_HI_AND_RESERVED_269.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_38); // $1D, $02, $00, $00

    VarCLOCK_SEQ_LOW_270 := TypeTGUID_257.GetNewChild;
    VarCLOCK_SEQ_LOW_270.Tag := DW_TAG_member;
    VarCLOCK_SEQ_LOW_270.Children := 0;
    VarCLOCK_SEQ_LOW_270.Add(DW_AT_name, DW_FORM_string, 'CLOCK_SEQ_LOW'+#0);
    VarCLOCK_SEQ_LOW_270.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(9)])); // $23, $09
    VarCLOCK_SEQ_LOW_270.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_38); // $1D, $02, $00, $00

    VarNODE_271 := TypeTGUID_257.GetNewChild;
    VarNODE_271.Tag := DW_TAG_member;
    VarNODE_271.Children := 0;
    VarNODE_271.Add(DW_AT_name, DW_FORM_string, 'NODE'+#0);
    VarNODE_271.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(10)])); // $23, $0A
    VarNODE_271.AddRef(DW_AT_type, DW_FORM_ref4, @Type_310); // $F7, $0E, $00, $00

  Type_272 := Unittestprog2_pas_0.GetNewChild;
  Type_272.Tag := DW_TAG_reference_type;
  Type_272.Children := 0;
  Type_272.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTGUID_256); // $A5, $0C, $00, $00

  TypeDeclPINTERFACEENTRY_273 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclPINTERFACEENTRY_273.Tag := DW_TAG_typedef;
  TypeDeclPINTERFACEENTRY_273.Children := 0;
  TypeDeclPINTERFACEENTRY_273.Add(DW_AT_name, DW_FORM_string, 'PINTERFACEENTRY'+#0);
  TypeDeclPINTERFACEENTRY_273.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_274); // $BA, $0D, $00, $00

  TypePtr_274 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_274.Tag := DW_TAG_pointer_type;
  TypePtr_274.Children := 0;
  TypePtr_274.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACEENTRY_313); // $0B, $0F, $00, $00

  Type_275 := Unittestprog2_pas_0.GetNewChild;
  Type_275.Tag := DW_TAG_reference_type;
  Type_275.Children := 0;
  Type_275.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTERFACEENTRY_273); // $A5, $0D, $00, $00

  TypePtr_276 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_276.Tag := DW_TAG_pointer_type;
  TypePtr_276.Children := 0;
  TypePtr_276.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_290); // $25, $0E, $00, $00

  Type_277 := Unittestprog2_pas_0.GetNewChild;
  Type_277.Tag := DW_TAG_reference_type;
  Type_277.Children := 0;
  Type_277.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_276); // $C4, $0D, $00, $00

  TypePtr_278 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_278.Tag := DW_TAG_pointer_type;
  TypePtr_278.Children := 0;
  TypePtr_278.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_290); // $25, $0E, $00, $00

  Type_279 := Unittestprog2_pas_0.GetNewChild;
  Type_279.Tag := DW_TAG_reference_type;
  Type_279.Children := 0;
  Type_279.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_278); // $CE, $0D, $00, $00

  TypeDeclPINTERFACETABLE_280 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclPINTERFACETABLE_280.Tag := DW_TAG_typedef;
  TypeDeclPINTERFACETABLE_280.Children := 0;
  TypeDeclPINTERFACETABLE_280.Add(DW_AT_name, DW_FORM_string, 'PINTERFACETABLE'+#0);
  TypeDeclPINTERFACETABLE_280.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_281); // $ED, $0D, $00, $00

  TypePtr_281 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_281.Tag := DW_TAG_pointer_type;
  TypePtr_281.Children := 0;
  TypePtr_281.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACETABLE_322); // $94, $0F, $00, $00

  Type_282 := Unittestprog2_pas_0.GetNewChild;
  Type_282.Tag := DW_TAG_reference_type;
  Type_282.Children := 0;
  Type_282.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTERFACETABLE_280); // $D8, $0D, $00, $00

  TypePtr_283 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_283.Tag := DW_TAG_pointer_type;
  TypePtr_283.Children := 0;
  TypePtr_283.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_290); // $25, $0E, $00, $00

  Type_284 := Unittestprog2_pas_0.GetNewChild;
  Type_284.Tag := DW_TAG_reference_type;
  Type_284.Children := 0;
  Type_284.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_283); // $F7, $0D, $00, $00

  TypeDeclANSISTRING_285 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclANSISTRING_285.Tag := DW_TAG_typedef;
  TypeDeclANSISTRING_285.Children := 0;
  TypeDeclANSISTRING_285.Add(DW_AT_name, DW_FORM_string, 'ANSISTRING'+#0);
  TypeDeclANSISTRING_285.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_286); // $11, $0E, $00, $00

  TypePtr_286 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_286.Tag := DW_TAG_pointer_type;
  TypePtr_286.Children := 0;
  TypePtr_286.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclCHAR_293); // $42, $0E, $00, $00

  Type_287 := Unittestprog2_pas_0.GetNewChild;
  Type_287.Tag := DW_TAG_reference_type;
  Type_287.Children := 0;
  Type_287.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclANSISTRING_285); // $01, $0E, $00, $00

  TypePtr_288 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_288.Tag := DW_TAG_pointer_type;
  TypePtr_288.Children := 0;
  TypePtr_288.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_290); // $25, $0E, $00, $00

  Type_289 := Unittestprog2_pas_0.GetNewChild;
  Type_289.Tag := DW_TAG_reference_type;
  Type_289.Children := 0;
  Type_289.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_288); // $1B, $0E, $00, $00

  TypeDecl__vtbl_ptr_type_290 := Unittestprog2_pas_0.GetNewChild;
  TypeDecl__vtbl_ptr_type_290.Tag := DW_TAG_typedef;
  TypeDecl__vtbl_ptr_type_290.Children := 0;
  TypeDecl__vtbl_ptr_type_290.Add(DW_AT_name, DW_FORM_string, '__vtbl_ptr_type'+#0);
  TypeDecl__vtbl_ptr_type_290.AddRef(DW_AT_type, DW_FORM_ref4, @Type_291); // $3A, $0E, $00, $00

  Type_291 := Unittestprog2_pas_0.GetNewChild;
  Type_291.Tag := DW_TAG_structure_type;
  Type_291.Children := 0;
  Type_291.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 20);

  Type_292 := Unittestprog2_pas_0.GetNewChild;
  Type_292.Tag := DW_TAG_reference_type;
  Type_292.Children := 0;
  Type_292.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_290); // $25, $0E, $00, $00

  TypeDeclCHAR_293 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclCHAR_293.Tag := DW_TAG_typedef;
  TypeDeclCHAR_293.Children := 0;
  TypeDeclCHAR_293.Add(DW_AT_name, DW_FORM_string, 'CHAR'+#0);
  TypeDeclCHAR_293.AddRef(DW_AT_type, DW_FORM_ref4, @TypeChar_294); // $4C, $0E, $00, $00

  TypeChar_294 := Unittestprog2_pas_0.GetNewChild;
  TypeChar_294.Tag := DW_TAG_base_type;
  TypeChar_294.Children := 0;
  TypeChar_294.Add(DW_AT_name, DW_FORM_string, 'Char'+#0);
  TypeChar_294.Add(DW_AT_encoding, DW_FORM_data1, [$08]);
  TypeChar_294.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_295 := Unittestprog2_pas_0.GetNewChild;
  Type_295.Tag := DW_TAG_reference_type;
  Type_295.Children := 0;
  Type_295.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclCHAR_293); // $42, $0E, $00, $00

  TypeDeclTSTRINGMESSAGETABLE_296 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclTSTRINGMESSAGETABLE_296.Tag := DW_TAG_typedef;
  TypeDeclTSTRINGMESSAGETABLE_296.Children := 0;
  TypeDeclTSTRINGMESSAGETABLE_296.Add(DW_AT_name, DW_FORM_string, 'TSTRINGMESSAGETABLE'+#0);
  TypeDeclTSTRINGMESSAGETABLE_296.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSTRINGMESSAGETABLE_297); // $72, $0E, $00, $00

  TypeTSTRINGMESSAGETABLE_297 := Unittestprog2_pas_0.GetNewChild;
  TypeTSTRINGMESSAGETABLE_297.Tag := DW_TAG_structure_type;
  TypeTSTRINGMESSAGETABLE_297.Children := 1;
  TypeTSTRINGMESSAGETABLE_297.Add(DW_AT_name, DW_FORM_string, 'TSTRINGMESSAGETABLE'+#0);
  TypeTSTRINGMESSAGETABLE_297.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 12);

    VarCOUNT_298 := TypeTSTRINGMESSAGETABLE_297.GetNewChild;
    VarCOUNT_298.Tag := DW_TAG_member;
    VarCOUNT_298.Children := 0;
    VarCOUNT_298.Add(DW_AT_name, DW_FORM_string, 'COUNT'+#0);
    VarCOUNT_298.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarCOUNT_298.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_47); // $6A, $02, $00, $00

    VarMSGSTRTABLE_299 := TypeTSTRINGMESSAGETABLE_297.GetNewChild;
    VarMSGSTRTABLE_299.Tag := DW_TAG_member;
    VarMSGSTRTABLE_299.Children := 0;
    VarMSGSTRTABLE_299.Add(DW_AT_name, DW_FORM_string, 'MSGSTRTABLE'+#0);
    VarMSGSTRTABLE_299.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarMSGSTRTABLE_299.AddRef(DW_AT_type, DW_FORM_ref4, @Type_327); // $E4, $0F, $00, $00

  Type_300 := Unittestprog2_pas_0.GetNewChild;
  Type_300.Tag := DW_TAG_reference_type;
  Type_300.Children := 0;
  Type_300.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSTRINGMESSAGETABLE_296); // $59, $0E, $00, $00

  TypeDeclLONGWORD_301 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclLONGWORD_301.Tag := DW_TAG_typedef;
  TypeDeclLONGWORD_301.Children := 0;
  TypeDeclLONGWORD_301.Add(DW_AT_name, DW_FORM_string, 'LONGWORD'+#0);
  TypeDeclLONGWORD_301.AddRef(DW_AT_type, DW_FORM_ref4, @TypeLONGWORD_302); // $BE, $0E, $00, $00

  TypeLONGWORD_302 := Unittestprog2_pas_0.GetNewChild;
  TypeLONGWORD_302.Tag := DW_TAG_base_type;
  TypeLONGWORD_302.Children := 0;
  TypeLONGWORD_302.Add(DW_AT_name, DW_FORM_string, 'LONGWORD'+#0);
  TypeLONGWORD_302.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeLONGWORD_302.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

  Type_303 := Unittestprog2_pas_0.GetNewChild;
  Type_303.Tag := DW_TAG_reference_type;
  Type_303.Children := 0;
  Type_303.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_301); // $B0, $0E, $00, $00

  Type_304 := Unittestprog2_pas_0.GetNewChild;
  Type_304.Tag := DW_TAG_array_type;
  Type_304.Children := 1;
  Type_304.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 8);
  Type_304.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_38); // $1D, $02, $00, $00

    Type_305 := Type_304.GetNewChild;
    Type_305.Tag := DW_TAG_subrange_type;
    Type_305.Children := 0;
    Type_305.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
    Type_305.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 7);
    Type_305.AddSLEB(DW_AT_byte_stride, DW_FORM_udata, 1);
    Type_305.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_41); // $34, $02, $00, $00

  Type_306 := Unittestprog2_pas_0.GetNewChild;
  Type_306.Tag := DW_TAG_reference_type;
  Type_306.Children := 0;
  Type_306.AddRef(DW_AT_type, DW_FORM_ref4, @Type_304); // $CF, $0E, $00, $00

  Type_307 := Unittestprog2_pas_0.GetNewChild;
  Type_307.Tag := DW_TAG_array_type;
  Type_307.Children := 1;
  Type_307.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 8);
  Type_307.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_38); // $1D, $02, $00, $00

    Type_308 := Type_307.GetNewChild;
    Type_308.Tag := DW_TAG_subrange_type;
    Type_308.Children := 0;
    Type_308.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
    Type_308.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 7);
    Type_308.AddSLEB(DW_AT_byte_stride, DW_FORM_udata, 1);
    Type_308.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_41); // $34, $02, $00, $00

  Type_309 := Unittestprog2_pas_0.GetNewChild;
  Type_309.Tag := DW_TAG_reference_type;
  Type_309.Children := 0;
  Type_309.AddRef(DW_AT_type, DW_FORM_ref4, @Type_307); // $E3, $0E, $00, $00

  Type_310 := Unittestprog2_pas_0.GetNewChild;
  Type_310.Tag := DW_TAG_array_type;
  Type_310.Children := 1;
  Type_310.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 6);
  Type_310.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_38); // $1D, $02, $00, $00

    Type_311 := Type_310.GetNewChild;
    Type_311.Tag := DW_TAG_subrange_type;
    Type_311.Children := 0;
    Type_311.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
    Type_311.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 5);
    Type_311.AddSLEB(DW_AT_byte_stride, DW_FORM_udata, 1);
    Type_311.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_41); // $34, $02, $00, $00

  Type_312 := Unittestprog2_pas_0.GetNewChild;
  Type_312.Tag := DW_TAG_reference_type;
  Type_312.Children := 0;
  Type_312.AddRef(DW_AT_type, DW_FORM_ref4, @Type_310); // $F7, $0E, $00, $00

  TypeDeclTINTERFACEENTRY_313 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclTINTERFACEENTRY_313.Tag := DW_TAG_typedef;
  TypeDeclTINTERFACEENTRY_313.Children := 0;
  TypeDeclTINTERFACEENTRY_313.Add(DW_AT_name, DW_FORM_string, 'TINTERFACEENTRY'+#0);
  TypeDeclTINTERFACEENTRY_313.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTINTERFACEENTRY_314); // $20, $0F, $00, $00

  TypeTINTERFACEENTRY_314 := Unittestprog2_pas_0.GetNewChild;
  TypeTINTERFACEENTRY_314.Tag := DW_TAG_structure_type;
  TypeTINTERFACEENTRY_314.Children := 1;
  TypeTINTERFACEENTRY_314.Add(DW_AT_name, DW_FORM_string, 'TINTERFACEENTRY'+#0);
  TypeTINTERFACEENTRY_314.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 20);

    VarIID_315 := TypeTINTERFACEENTRY_314.GetNewChild;
    VarIID_315.Tag := DW_TAG_member;
    VarIID_315.Children := 0;
    VarIID_315.Add(DW_AT_name, DW_FORM_string, 'IID'+#0);
    VarIID_315.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarIID_315.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPGUID_330); // $F8, $0F, $00, $00

    VarVTABLE_316 := TypeTINTERFACEENTRY_314.GetNewChild;
    VarVTABLE_316.Tag := DW_TAG_member;
    VarVTABLE_316.Children := 0;
    VarVTABLE_316.Add(DW_AT_name, DW_FORM_string, 'VTABLE'+#0);
    VarVTABLE_316.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarVTABLE_316.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_210); // $55, $0B, $00, $00

    VarIOFFSET_317 := TypeTINTERFACEENTRY_314.GetNewChild;
    VarIOFFSET_317.Tag := DW_TAG_member;
    VarIOFFSET_317.Children := 0;
    VarIOFFSET_317.Add(DW_AT_name, DW_FORM_string, 'IOFFSET'+#0);
    VarIOFFSET_317.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(8)])); // $23, $08
    VarIOFFSET_317.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_301); // $B0, $0E, $00, $00

    VarIIDSTR_318 := TypeTINTERFACEENTRY_314.GetNewChild;
    VarIIDSTR_318.Tag := DW_TAG_member;
    VarIIDSTR_318.Children := 0;
    VarIIDSTR_318.Add(DW_AT_name, DW_FORM_string, 'IIDSTR'+#0);
    VarIIDSTR_318.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(12)])); // $23, $0C
    VarIIDSTR_318.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSHORTSTRING_333); // $0D, $10, $00, $00

    VarITYPE_319 := TypeTINTERFACEENTRY_314.GetNewChild;
    VarITYPE_319.Tag := DW_TAG_member;
    VarITYPE_319.Children := 0;
    VarITYPE_319.Add(DW_AT_name, DW_FORM_string, 'ITYPE'+#0);
    VarITYPE_319.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(16)])); // $23, $10
    VarITYPE_319.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACEENTRYTYPE_336); // $29, $10, $00, $00

    Var__PAD_DUMMY_320 := TypeTINTERFACEENTRY_314.GetNewChild;
    Var__PAD_DUMMY_320.Tag := DW_TAG_member;
    Var__PAD_DUMMY_320.Children := 0;
    Var__PAD_DUMMY_320.Add(DW_AT_name, DW_FORM_string, '__PAD_DUMMY'+#0);
    Var__PAD_DUMMY_320.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(16)])); // $23, $10
    Var__PAD_DUMMY_320.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_210); // $55, $0B, $00, $00

  Type_321 := Unittestprog2_pas_0.GetNewChild;
  Type_321.Tag := DW_TAG_reference_type;
  Type_321.Children := 0;
  Type_321.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACEENTRY_313); // $0B, $0F, $00, $00

  TypeDeclTINTERFACETABLE_322 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclTINTERFACETABLE_322.Tag := DW_TAG_typedef;
  TypeDeclTINTERFACETABLE_322.Children := 0;
  TypeDeclTINTERFACETABLE_322.Add(DW_AT_name, DW_FORM_string, 'TINTERFACETABLE'+#0);
  TypeDeclTINTERFACETABLE_322.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTINTERFACETABLE_323); // $A9, $0F, $00, $00

  TypeTINTERFACETABLE_323 := Unittestprog2_pas_0.GetNewChild;
  TypeTINTERFACETABLE_323.Tag := DW_TAG_structure_type;
  TypeTINTERFACETABLE_323.Children := 1;
  TypeTINTERFACETABLE_323.Add(DW_AT_name, DW_FORM_string, 'TINTERFACETABLE'+#0);
  TypeTINTERFACETABLE_323.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 24);

    VarENTRYCOUNT_324 := TypeTINTERFACETABLE_323.GetNewChild;
    VarENTRYCOUNT_324.Tag := DW_TAG_member;
    VarENTRYCOUNT_324.Children := 0;
    VarENTRYCOUNT_324.Add(DW_AT_name, DW_FORM_string, 'ENTRYCOUNT'+#0);
    VarENTRYCOUNT_324.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarENTRYCOUNT_324.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_301); // $B0, $0E, $00, $00

    VarENTRIES_325 := TypeTINTERFACETABLE_323.GetNewChild;
    VarENTRIES_325.Tag := DW_TAG_member;
    VarENTRIES_325.Children := 0;
    VarENTRIES_325.Add(DW_AT_name, DW_FORM_string, 'ENTRIES'+#0);
    VarENTRIES_325.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarENTRIES_325.AddRef(DW_AT_type, DW_FORM_ref4, @Type_346); // $FF, $10, $00, $00

  Type_326 := Unittestprog2_pas_0.GetNewChild;
  Type_326.Tag := DW_TAG_reference_type;
  Type_326.Children := 0;
  Type_326.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACETABLE_322); // $94, $0F, $00, $00

  Type_327 := Unittestprog2_pas_0.GetNewChild;
  Type_327.Tag := DW_TAG_array_type;
  Type_327.Children := 1;
  Type_327.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 8);
  Type_327.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTMSGSTRTABLE_349); // $13, $11, $00, $00

    Type_328 := Type_327.GetNewChild;
    Type_328.Tag := DW_TAG_subrange_type;
    Type_328.Children := 0;
    Type_328.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
    Type_328.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 0);
    Type_328.AddSLEB(DW_AT_byte_stride, DW_FORM_udata, 8);
    Type_328.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_41); // $34, $02, $00, $00

  Type_329 := Unittestprog2_pas_0.GetNewChild;
  Type_329.Tag := DW_TAG_reference_type;
  Type_329.Children := 0;
  Type_329.AddRef(DW_AT_type, DW_FORM_ref4, @Type_327); // $E4, $0F, $00, $00

  TypeDeclPGUID_330 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclPGUID_330.Tag := DW_TAG_typedef;
  TypeDeclPGUID_330.Children := 0;
  TypeDeclPGUID_330.Add(DW_AT_name, DW_FORM_string, 'PGUID'+#0);
  TypeDeclPGUID_330.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_331); // $03, $10, $00, $00

  TypePtr_331 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_331.Tag := DW_TAG_pointer_type;
  TypePtr_331.Children := 0;
  TypePtr_331.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTGUID_256); // $A5, $0C, $00, $00

  Type_332 := Unittestprog2_pas_0.GetNewChild;
  Type_332.Tag := DW_TAG_reference_type;
  Type_332.Children := 0;
  Type_332.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPGUID_330); // $F8, $0F, $00, $00

  TypeDeclPSHORTSTRING_333 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclPSHORTSTRING_333.Tag := DW_TAG_typedef;
  TypeDeclPSHORTSTRING_333.Children := 0;
  TypeDeclPSHORTSTRING_333.Add(DW_AT_name, DW_FORM_string, 'PSHORTSTRING'+#0);
  TypeDeclPSHORTSTRING_333.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_334); // $1F, $10, $00, $00

  TypePtr_334 := Unittestprog2_pas_0.GetNewChild;
  TypePtr_334.Tag := DW_TAG_pointer_type;
  TypePtr_334.Children := 0;
  TypePtr_334.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_230); // $E1, $0B, $00, $00

  Type_335 := Unittestprog2_pas_0.GetNewChild;
  Type_335.Tag := DW_TAG_reference_type;
  Type_335.Children := 0;
  Type_335.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSHORTSTRING_333); // $0D, $10, $00, $00

  TypeDeclTINTERFACEENTRYTYPE_336 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclTINTERFACEENTRYTYPE_336.Tag := DW_TAG_typedef;
  TypeDeclTINTERFACEENTRYTYPE_336.Children := 0;
  TypeDeclTINTERFACEENTRYTYPE_336.Add(DW_AT_name, DW_FORM_string, 'TINTERFACEENTRYTYPE'+#0);
  TypeDeclTINTERFACEENTRYTYPE_336.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTINTERFACEENTRYTYPE_337); // $42, $10, $00, $00

  TypeTINTERFACEENTRYTYPE_337 := Unittestprog2_pas_0.GetNewChild;
  TypeTINTERFACEENTRYTYPE_337.Tag := DW_TAG_enumeration_type;
  TypeTINTERFACEENTRYTYPE_337.Children := 1;
  TypeTINTERFACEENTRYTYPE_337.Add(DW_AT_name, DW_FORM_string, 'TINTERFACEENTRYTYPE'+#0);
  TypeTINTERFACEENTRYTYPE_337.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeETSTANDARD_338 := TypeTINTERFACEENTRYTYPE_337.GetNewChild;
    TypeETSTANDARD_338.Tag := DW_TAG_enumerator;
    TypeETSTANDARD_338.Children := 0;
    TypeETSTANDARD_338.Add(DW_AT_name, DW_FORM_string, 'ETSTANDARD'+#0);
    TypeETSTANDARD_338.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeETVIRTUALMETHODRESULT_339 := TypeTINTERFACEENTRYTYPE_337.GetNewChild;
    TypeETVIRTUALMETHODRESULT_339.Tag := DW_TAG_enumerator;
    TypeETVIRTUALMETHODRESULT_339.Children := 0;
    TypeETVIRTUALMETHODRESULT_339.Add(DW_AT_name, DW_FORM_string, 'ETVIRTUALMETHODRESULT'+#0);
    TypeETVIRTUALMETHODRESULT_339.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeETSTATICMETHODRESULT_340 := TypeTINTERFACEENTRYTYPE_337.GetNewChild;
    TypeETSTATICMETHODRESULT_340.Tag := DW_TAG_enumerator;
    TypeETSTATICMETHODRESULT_340.Children := 0;
    TypeETSTATICMETHODRESULT_340.Add(DW_AT_name, DW_FORM_string, 'ETSTATICMETHODRESULT'+#0);
    TypeETSTATICMETHODRESULT_340.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

    TypeETFIELDVALUE_341 := TypeTINTERFACEENTRYTYPE_337.GetNewChild;
    TypeETFIELDVALUE_341.Tag := DW_TAG_enumerator;
    TypeETFIELDVALUE_341.Children := 0;
    TypeETFIELDVALUE_341.Add(DW_AT_name, DW_FORM_string, 'ETFIELDVALUE'+#0);
    TypeETFIELDVALUE_341.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

    TypeETVIRTUALMETHODCLASS_342 := TypeTINTERFACEENTRYTYPE_337.GetNewChild;
    TypeETVIRTUALMETHODCLASS_342.Tag := DW_TAG_enumerator;
    TypeETVIRTUALMETHODCLASS_342.Children := 0;
    TypeETVIRTUALMETHODCLASS_342.Add(DW_AT_name, DW_FORM_string, 'ETVIRTUALMETHODCLASS'+#0);
    TypeETVIRTUALMETHODCLASS_342.Add(DW_AT_const_value, DW_FORM_data4, [$04, $00, $00, $00]);

    TypeETSTATICMETHODCLASS_343 := TypeTINTERFACEENTRYTYPE_337.GetNewChild;
    TypeETSTATICMETHODCLASS_343.Tag := DW_TAG_enumerator;
    TypeETSTATICMETHODCLASS_343.Children := 0;
    TypeETSTATICMETHODCLASS_343.Add(DW_AT_name, DW_FORM_string, 'ETSTATICMETHODCLASS'+#0);
    TypeETSTATICMETHODCLASS_343.Add(DW_AT_const_value, DW_FORM_data4, [$05, $00, $00, $00]);

    TypeETFIELDVALUECLASS_344 := TypeTINTERFACEENTRYTYPE_337.GetNewChild;
    TypeETFIELDVALUECLASS_344.Tag := DW_TAG_enumerator;
    TypeETFIELDVALUECLASS_344.Children := 0;
    TypeETFIELDVALUECLASS_344.Add(DW_AT_name, DW_FORM_string, 'ETFIELDVALUECLASS'+#0);
    TypeETFIELDVALUECLASS_344.Add(DW_AT_const_value, DW_FORM_data4, [$06, $00, $00, $00]);

  Type_345 := Unittestprog2_pas_0.GetNewChild;
  Type_345.Tag := DW_TAG_reference_type;
  Type_345.Children := 0;
  Type_345.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACEENTRYTYPE_336); // $29, $10, $00, $00

  Type_346 := Unittestprog2_pas_0.GetNewChild;
  Type_346.Tag := DW_TAG_array_type;
  Type_346.Children := 1;
  Type_346.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 20);
  Type_346.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACEENTRY_313); // $0B, $0F, $00, $00

    Type_347 := Type_346.GetNewChild;
    Type_347.Tag := DW_TAG_subrange_type;
    Type_347.Children := 0;
    Type_347.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
    Type_347.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 0);
    Type_347.AddSLEB(DW_AT_byte_stride, DW_FORM_udata, 20);
    Type_347.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_41); // $34, $02, $00, $00

  Type_348 := Unittestprog2_pas_0.GetNewChild;
  Type_348.Tag := DW_TAG_reference_type;
  Type_348.Children := 0;
  Type_348.AddRef(DW_AT_type, DW_FORM_ref4, @Type_346); // $FF, $10, $00, $00

  TypeDeclTMSGSTRTABLE_349 := Unittestprog2_pas_0.GetNewChild;
  TypeDeclTMSGSTRTABLE_349.Tag := DW_TAG_typedef;
  TypeDeclTMSGSTRTABLE_349.Children := 0;
  TypeDeclTMSGSTRTABLE_349.Add(DW_AT_name, DW_FORM_string, 'TMSGSTRTABLE'+#0);
  TypeDeclTMSGSTRTABLE_349.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTMSGSTRTABLE_350); // $25, $11, $00, $00

  TypeTMSGSTRTABLE_350 := Unittestprog2_pas_0.GetNewChild;
  TypeTMSGSTRTABLE_350.Tag := DW_TAG_structure_type;
  TypeTMSGSTRTABLE_350.Children := 1;
  TypeTMSGSTRTABLE_350.Add(DW_AT_name, DW_FORM_string, 'TMSGSTRTABLE'+#0);
  TypeTMSGSTRTABLE_350.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 8);

    VarNAME_351 := TypeTMSGSTRTABLE_350.GetNewChild;
    VarNAME_351.Tag := DW_TAG_member;
    VarNAME_351.Children := 0;
    VarNAME_351.Add(DW_AT_name, DW_FORM_string, 'NAME'+#0);
    VarNAME_351.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarNAME_351.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSHORTSTRING_333); // $0D, $10, $00, $00

    VarMETHOD_352 := TypeTMSGSTRTABLE_350.GetNewChild;
    VarMETHOD_352.Tag := DW_TAG_member;
    VarMETHOD_352.Children := 0;
    VarMETHOD_352.Add(DW_AT_name, DW_FORM_string, 'METHOD'+#0);
    VarMETHOD_352.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarMETHOD_352.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_210); // $55, $0B, $00, $00

  Type_353 := Unittestprog2_pas_0.GetNewChild;
  Type_353.Tag := DW_TAG_reference_type;
  Type_353.Children := 0;
  Type_353.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTMSGSTRTABLE_349); // $13, $11, $00, $00

  //
  SectionDbgInfo.CreateSectionData;
  SectionDbgInfo.AbbrevSection.CreateSectionData;
end;

end.

