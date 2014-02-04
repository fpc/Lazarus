unit TestDwarfSetup1;

{$mode objfpc}{$H+}

(*
  Data generated from testdata\dwarfsetup1.lpr
*)

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
  PTestSetup1Record = ^TTestSetup1Record;

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
  PTestSetup1Class = ^TTestSetup1Class;

  TTestSetup1ClassChild = class(TTestSetup1Class)
    FInt64: Int64;
    FQWord: QWord;
  end;
  PTestSetup1ClassChild = ^TTestSetup1ClassChild;

  TTestSetup1ClassClass = class of TTestSetup1Class;
  TTestSetup1ClassChildClass = class of TTestSetup1ClassChild;

  { TTestSetup1Object }

  TTestSetup1Object = object
  public
    FWord: Word;
    FWordL: QWord;
    FInt: ShortInt;
    FIntL: Int64;
    FBool: Boolean;
    FBool2: LongBool;
    FBool3: ByteBool;
    FTest: TTestSetup1Class;
    procedure f0(a:integer); virtual;
  end;
  PTestSetup1Object  = ^TTestSetup1Object;

  Pint = ^ integer;
  PPInt = ^Pint;
  PPPInt = ^PPint;
  PQWord = ^QWord;

var // Globals
  GlobTestSetup1Record: TTestSetup1Record;
  GlobTestSetup1RecordP: PTestSetup1Record;

  GlobTestSetup1Class: TTestSetup1Class;
  GlobTestSetup1ClassP: PTestSetup1Class;
  GlobTestSetup1ClassChild: TTestSetup1ClassChild;
  GlobTestSetup1ClassChildP: PTestSetup1ClassChild;
  GlobTestSetup1ClassClass: TTestSetup1ClassClass;
  GlobTestSetup1ClassChildClass: TTestSetup1ClassChildClass;

  GlobTestSetup1Object: TTestSetup1Object;
  GlobTestSetup1ObjectP: PTestSetup1Object;

  GlobTestSetup1Pointer: Pointer;
  GlobTestSetup1QWord: QWord;
  {%endregion}

type
  { TTestLoaderSetup1 }

  TTestLoaderSetup1 = class(TTestDummyImageLoader)
  public
    constructor Create; override;
  public
    SectionDbgInfo: TTestDummySectionInfoEntries;
    //CompUnit, Prog1, Prog2,
    //GlobVar1, TypeInt, TypeIntDecl: TTestDwarfInfoEntry;

    Unitdwarfsetup1_lpr_0, VarGLOBTESTSETUP1RECORD_1, VarGLOBTESTSETUP1RECORDP_2, VarGLOBTESTSETUP1CLASS_3, VarGLOBTESTSETUP1CLASSP_4, VarGLOBTESTSETUP1CLASSCHILD_5, VarGLOBTESTSETUP1CLASSCHILDP_6, VarGLOBTESTSETUP1CLASSCLASS_7, VarGLOBTESTSETUP1CLASSCHILDCLASS_8, VarGLOBTESTSETUP1OBJECT_9, VarGLOBTESTSETUP1OBJECTP_10, VarGLOBTESTSETUP1POINTER_11, VarGLOBTESTSETUP1QWORD_12, Progmain_13, ProgTESTSETUP1BAR_14, VarPARAMTESTSETUP1RECORD_15, VarPARAMTESTRECORD_16, VarPARAMTESTSETUP1CLASS_17, VarPARAMTESTSETUP1CLASSP_18, VarPARAMTESTSETUP1CLASSCHILD_19, VarPARAMTESTSETUP1CLASSCHILDP_20, VarPARAMTESTSETUP1CLASSCLASS_21, VarPARAMTESTSETUP1CLASSCHILDCLASS_22, VarPARAMTESTSETUP1OBJECT_23, VarPARAMTESTSETUP1OBJECTP_24, VarVPARAMTESTSETUP1RECORD_25, VarVPARAMTESTRECORD_26, VarVPARAMTESTSETUP1CLASS_27, VarVPARAMTESTSETUP1CLASSP_28, VarVPARAMTESTSETUP1CLASSCHILD_29, VarVPARAMTESTSETUP1CLASSCHILDP_30, VarVPARAMTESTSETUP1CLASSCLASS_31, VarVPARAMTESTSETUP1CLASSCHILDCLASS_32, VarVPARAMTESTSETUP1OBJECT_33, VarVPARAMTESTSETUP1OBJECTP_34, VarINT1_35, VarPINT1_36, VarBOOL1_37, VarOBJ1_38, VarPOBJ1_39, VarOLDOBJ1_40, VarPOLDOBJ1_41, VarREC1_42, VarPREC1_43, VarREC2_44, VarPI_45, VarPPI_46, VarPPPI_47, VarSUBR_48, VarSUBR2_49, VarSUBR3_50, TypePtr_51, Type_52, TypePtr_53, Type_54, TypePtr_55, Type_56, Type_57, VarFWORD_58, VarFBOOL_59, Type_60, Type_61, Type_62, Type_63, Type_64, TypeChar_65, Type_66, TypeDeclBYTE_67, TypeBYTE_68, Type_69, TypeDeclSHORTINT_70, TypeSHORTINT_71, Type_72, TypeDeclWORD_73, TypeWORD_74, Type_75, TypeDeclLONGINT_76, TypeLONGINT_77, Type_78, TypeDeclQWORD_79, TypeQWord_80, Type_81, TypeDeclBOOLEAN_82, TypeBoolean_83, Type_84, TypeDeclPOINTER_85, TypePtr_86, Type_87, TypeDeclTTESTSETUP1CLASS_88, TypePtr_89, TypeTTESTSETUP1CLASS_90, XX_91, VarFWORD_92, VarFWORDL_93, VarFINT_94, VarFINTL_95, VarFBOOL_96, VarFTEST_97, ProgF0_98, Varthis_99, VarA_100, Type_101, TypeDeclTTESTSETUP1RECORD_102, TypeTTESTSETUP1RECORD_103, VarFWORD_104, VarFBOOL_105, VarFTEST_106, Type_107, TypeDeclPTESTSETUP1RECORD_108, TypePtr_109, Type_110, TypeDeclPTESTSETUP1CLASS_111, TypePtr_112, Type_113, TypeDeclTTESTSETUP1CLASSCHILD_114, TypePtr_115, TypeTTESTSETUP1CLASSCHILD_116, XX_117, VarFINT64_118, VarFQWORD_119, Type_120, TypeDeclPTESTSETUP1CLASSCHILD_121, TypePtr_122, Type_123, TypeDeclTTESTSETUP1CLASSCLASS_124, TypePtr_125, Type_126, TypeDeclTTESTSETUP1CLASSCHILDCLASS_127, TypePtr_128, Type_129, TypeDeclTTESTSETUP1OBJECT_130, TypeTTESTSETUP1OBJECT_131, Var_vptrTTESTSETUP1OBJECT_132, VarFWORD_133, VarFWORDL_134, VarFINT_135, VarFINTL_136, VarFBOOL_137, VarFBOOL2_138, VarFBOOL3_139, VarFTEST_140, ProgF0_141, Varthis_142, VarA_143, Type_144, TypeDeclPTESTSETUP1OBJECT_145, TypePtr_146, Type_147, TypeDeclPINT_148, TypePtr_149, Type_150, TypeDeclPPINT_151, TypePtr_152, Type_153, TypeDeclPPPINT_154, TypePtr_155, Type_156, TypeDeclPQWORD_157, TypePtr_158, Type_159, TypeDeclTOBJECT_160, TypePtr_161, TypeTOBJECT_162, Var_vptrTOBJECT_163, ProgCREATE_164, Varthis_165, Varvmt_166, ProgDESTROY_167, Varthis_168, Varvmt_169, ProgNEWINSTANCE_170, Varself_171, ProgFREEINSTANCE_172, Varthis_173, ProgSAFECALLEXCEPTION_174, Varthis_175, VarEXCEPTOBJECT_176, VarEXCEPTADDR_177, ProgDEFAULTHANDLER_178, Varthis_179, VarMESSAGE_180, ProgFREE_181, Varthis_182, ProgINITINSTANCE_183, Varself_184, VarINSTANCE_185, ProgCLEANUPINSTANCE_186, Varthis_187, ProgCLASSTYPE_188, Varself_189, ProgCLASSINFO_190, Varself_191, ProgCLASSNAME_192, Varself_193, Varresult_194, ProgCLASSNAMEIS_195, Varself_196, VarNAME_197, ProgCLASSPARENT_198, Varself_199, ProgINSTANCESIZE_200, Varself_201, ProgINHERITSFROM_202, Varself_203, VarACLASS_204, ProgSTRINGMESSAGETABLE_205, Varself_206, ProgMETHODADDRESS_207, Varself_208, VarNAME_209, ProgMETHODNAME_210, Varself_211, VarADDRESS_212, Varresult_213, ProgFIELDADDRESS_214, Varthis_215, VarNAME_216, ProgAFTERCONSTRUCTION_217, Varthis_218, ProgBEFOREDESTRUCTION_219, Varthis_220, ProgDEFAULTHANDLERSTR_221, Varthis_222, VarMESSAGE_223, ProgDISPATCH_224, Varthis_225, VarMESSAGE_226, ProgDISPATCHSTR_227, Varthis_228, VarMESSAGE_229, ProgGETINTERFACE_230, Varthis_231, VarIID_232, VarOBJ_233, ProgGETINTERFACE_234, Varthis_235, VarIIDSTR_236, VarOBJ_237, ProgGETINTERFACEBYSTR_238, Varthis_239, VarIIDSTR_240, VarOBJ_241, ProgGETINTERFACEWEAK_242, Varthis_243, VarIID_244, VarOBJ_245, ProgGETINTERFACEENTRY_246, Varself_247, VarIID_248, ProgGETINTERFACEENTRYBYSTR_249, Varself_250, VarIIDSTR_251, ProgGETINTERFACETABLE_252, Varself_253, ProgUNITNAME_254, Varself_255, Varresult_256, ProgEQUALS_257, Varthis_258, VarOBJ_259, ProgGETHASHCODE_260, Varthis_261, ProgTOSTRING_262, Varthis_263, Varresult_264, Type_265, TypeDeclINT64_266, TypeInt64_267, Type_268, TypeDecl__vtbl_ptr_type_269, Type_270, Type_271, TypeDeclLONGBOOL_272, TypeLongBool_273, Type_274, TypeDeclBYTEBOOL_275, TypeByteBool_276, Type_277, TypePtr_278, Type_279, TypeDeclHRESULT_280, TypeHRESULT_281, Type_282, TypeDeclformal_283, TypeFormalDef_284, Type_285, TypePtr_286, Type_287, TypeDeclTCLASS_288, TypePtr_289, Type_290, TypePtr_291, Type_292, TypePtr_293, Type_294, TypeDeclSHORTSTRING_295, TypeShortString_296, Varlength_297, Varst_298, Type_299, Type_300, Type_301, TypePtr_302, Type_303, TypePtr_304, Type_305, TypePtr_306, Type_307, TypePtr_308, Type_309, TypePtr_310, Type_311, TypeDeclPSTRINGMESSAGETABLE_312, TypePtr_313, Type_314, TypePtr_315, Type_316, TypePtr_317, Type_318, TypePtr_319, Type_320, TypeDeclTGUID_321, TypeTGUID_322, VarDATA1_323, VarDATA2_324, VarDATA3_325, VarDATA4_326, VarD1_327, VarD2_328, VarD3_329, VarD4_330, VarTIME_LOW_331, VarTIME_MID_332, VarTIME_HI_AND_VERSION_333, VarCLOCK_SEQ_HI_AND_RESERVED_334, VarCLOCK_SEQ_LOW_335, VarNODE_336, Type_337, TypeDeclPINTERFACEENTRY_338, TypePtr_339, Type_340, TypePtr_341, Type_342, TypePtr_343, Type_344, TypeDeclPINTERFACETABLE_345, TypePtr_346, Type_347, TypePtr_348, Type_349, TypeDeclANSISTRING_350, TypePtr_351, Type_352, TypePtr_353, Type_354, TypeDeclCHAR_355, TypeChar_356, Type_357, TypeDeclTSTRINGMESSAGETABLE_358, TypeTSTRINGMESSAGETABLE_359, VarCOUNT_360, VarMSGSTRTABLE_361, Type_362, TypeDeclLONGWORD_363, TypeLONGWORD_364, Type_365, Type_366, Type_367, Type_368, Type_369, Type_370, Type_371, Type_372, Type_373, Type_374, TypeDeclTINTERFACEENTRY_375, TypeTINTERFACEENTRY_376, VarIID_377, VarVTABLE_378, VarIOFFSET_379, VarIIDSTR_380, VarITYPE_381, Var__PAD_DUMMY_382, Type_383, TypeDeclTINTERFACETABLE_384, TypeTINTERFACETABLE_385, VarENTRYCOUNT_386, VarENTRIES_387, Type_388, Type_389, Type_390, Type_391, TypeDeclPGUID_392, TypePtr_393, Type_394, TypeDeclPSHORTSTRING_395, TypePtr_396, Type_397, TypeDeclTINTERFACEENTRYTYPE_398, TypeTINTERFACEENTRYTYPE_399, TypeETSTANDARD_400, TypeETVIRTUALMETHODRESULT_401, TypeETSTATICMETHODRESULT_402, TypeETFIELDVALUE_403, TypeETVIRTUALMETHODCLASS_404, TypeETSTATICMETHODCLASS_405, TypeETFIELDVALUECLASS_406, Type_407, Type_408, Type_409, Type_410, TypeDeclTMSGSTRTABLE_411, TypeTMSGSTRTABLE_412, VarNAME_413, VarMETHOD_414, Type_415
    : TTestDwarfInfoEntry;


    TestStackFrame: record
      // locals
      int1: Integer;
      pint1: ^Integer;
      bool1: Boolean;

      Obj1: TTestSetup1Class;
      PObj1: ^TTestSetup1Class;
      OldObj1: TTestSetup1Object;
      POldObj1: PTestSetup1Object;
      Rec1: TTestSetup1Record;
      PRec1: ^TTestSetup1Record;
      Rec2: record    FWord: Word;    FBool: Boolean;  end;

      pi: Pint;
      ppi: PPint;
      pppi: PPPint;

      subr: 1..9;
      subr2: -11..-9;
      subr3: #9..'m';

      // param
      ParamTestSetup1Record: TTestSetup1Record;
      ParamTestRecord: PTestSetup1Record;

      ParamTestSetup1Class: TTestSetup1Class;
      ParamTestSetup1ClassP: PTestSetup1Class;
      ParamTestSetup1ClassChild: TTestSetup1ClassChild;
      ParamTestSetup1ClassChildP: PTestSetup1ClassChild;
      ParamTestSetup1ClassClass: TTestSetup1ClassClass;
      ParamTestSetup1ClassChildClass: TTestSetup1ClassChildClass;

      ParamTestSetup1Object: TTestSetup1Object;
      ParamTestSetup1ObjectP: PTestSetup1Object;

      // simulate varparam / declare one pointer level
      VParamTestSetup1Record: ^TTestSetup1Record;
      VParamTestRecord: ^PTestSetup1Record;

      VParamTestSetup1Class: ^TTestSetup1Class;
      VParamTestSetup1ClassP: ^PTestSetup1Class;
      VParamTestSetup1ClassChild: ^TTestSetup1ClassChild;
      VParamTestSetup1ClassChildP: ^PTestSetup1ClassChild;
      VParamTestSetup1ClassClass: ^TTestSetup1ClassClass;
      VParamTestSetup1ClassChildClass: ^TTestSetup1ClassChildClass;

      VParamTestSetup1Object: ^TTestSetup1Object;
      VParamTestSetup1ObjectP: ^PTestSetup1Object;

      //
      EndPoint: pointer;
    end;
  end;


implementation

{ TTestSetup1Object }

procedure TTestSetup1Object.f0(a: integer);
begin
    //
end;

{ TTestSetup1Class }

procedure TTestSetup1Class.f0(a: integer);
begin
  //
end;

{ TTestLoaderSetup1 }

constructor TTestLoaderSetup1.Create;
var
  StackOffs: LongInt;
begin
  inherited Create;

  SectionDbgInfo := TestImgReader.TestSection['.debug_info'] as TTestDummySectionInfoEntries;
  Unitdwarfsetup1_lpr_0 := SectionDbgInfo.GetFirstInfoEntryObj;

  // Generated with fpc 2.6.2 32 bit win

Unitdwarfsetup1_lpr_0.Tag := DW_TAG_compile_unit;
Unitdwarfsetup1_lpr_0.Children := 1;
Unitdwarfsetup1_lpr_0.Add(DW_AT_name, DW_FORM_string, 'dwarfsetup1.lpr'+#0);
Unitdwarfsetup1_lpr_0.Add(DW_AT_producer, DW_FORM_string, 'Free Pascal 2.6.2 2013/02/16'+#0);
Unitdwarfsetup1_lpr_0.Add(DW_AT_comp_dir, DW_FORM_string, 'B:/lazarus_latest/components/fpdebug/test/testdata/'+#0);
Unitdwarfsetup1_lpr_0.Add(DW_AT_language, DW_FORM_data1, [$09]);
Unitdwarfsetup1_lpr_0.Add(DW_AT_identifier_case, DW_FORM_data1, [$03]);
Unitdwarfsetup1_lpr_0.Add(DW_AT_stmt_list, DW_FORM_data4, [$00, $00, $00, $00]);
Unitdwarfsetup1_lpr_0.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00400000);
Unitdwarfsetup1_lpr_0.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00501510);

  VarGLOBTESTSETUP1RECORD_1 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1RECORD_1.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1RECORD_1.Children := 0;
  VarGLOBTESTSETUP1RECORD_1.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1RECORD'+#0);
  VarGLOBTESTSETUP1RECORD_1.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB($00409000)])); // $03, $00, $90, $40, $00
  VarGLOBTESTSETUP1RECORD_1.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD_102); // $90, $07, $00, $00

  VarGLOBTESTSETUP1RECORDP_2 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1RECORDP_2.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1RECORDP_2.Children := 0;
  VarGLOBTESTSETUP1RECORDP_2.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1RECORDP'+#0);
  VarGLOBTESTSETUP1RECORDP_2.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB($00409010)])); // $03, $10, $90, $40, $00
  VarGLOBTESTSETUP1RECORDP_2.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1RECORD_108); // $EB, $07, $00, $00

  VarGLOBTESTSETUP1CLASS_3 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1CLASS_3.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1CLASS_3.Children := 0;
  VarGLOBTESTSETUP1CLASS_3.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1CLASS'+#0);
  VarGLOBTESTSETUP1CLASS_3.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB($00409020)])); // $03, $20, $90, $40, $00
  VarGLOBTESTSETUP1CLASS_3.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_88); // $D3, $06, $00, $00

  VarGLOBTESTSETUP1CLASSP_4 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1CLASSP_4.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1CLASSP_4.Children := 0;
  VarGLOBTESTSETUP1CLASSP_4.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1CLASSP'+#0);
  VarGLOBTESTSETUP1CLASSP_4.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB($00409030)])); // $03, $30, $90, $40, $00
  VarGLOBTESTSETUP1CLASSP_4.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1CLASS_111); // $0C, $08, $00, $00

  VarGLOBTESTSETUP1CLASSCHILD_5 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1CLASSCHILD_5.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1CLASSCHILD_5.Children := 0;
  VarGLOBTESTSETUP1CLASSCHILD_5.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1CLASSCHILD'+#0);
  VarGLOBTESTSETUP1CLASSCHILD_5.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB($00409040)])); // $03, $40, $90, $40, $00
  VarGLOBTESTSETUP1CLASSCHILD_5.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCHILD_114); // $2C, $08, $00, $00

  VarGLOBTESTSETUP1CLASSCHILDP_6 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1CLASSCHILDP_6.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1CLASSCHILDP_6.Children := 0;
  VarGLOBTESTSETUP1CLASSCHILDP_6.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1CLASSCHILDP'+#0);
  VarGLOBTESTSETUP1CLASSCHILDP_6.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB($00409050)])); // $03, $50, $90, $40, $00
  VarGLOBTESTSETUP1CLASSCHILDP_6.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1CLASSCHILD_121); // $91, $08, $00, $00

  VarGLOBTESTSETUP1CLASSCLASS_7 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1CLASSCLASS_7.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1CLASSCLASS_7.Children := 0;
  VarGLOBTESTSETUP1CLASSCLASS_7.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1CLASSCLASS'+#0);
  VarGLOBTESTSETUP1CLASSCLASS_7.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB($00409060)])); // $03, $60, $90, $40, $00
  VarGLOBTESTSETUP1CLASSCLASS_7.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCLASS_124); // $B6, $08, $00, $00

  VarGLOBTESTSETUP1CLASSCHILDCLASS_8 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1CLASSCHILDCLASS_8.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1CLASSCHILDCLASS_8.Children := 0;
  VarGLOBTESTSETUP1CLASSCHILDCLASS_8.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1CLASSCHILDCLASS'+#0);
  VarGLOBTESTSETUP1CLASSCHILDCLASS_8.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB($00409070)])); // $03, $70, $90, $40, $00
  VarGLOBTESTSETUP1CLASSCHILDCLASS_8.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCHILDCLASS_127); // $DB, $08, $00, $00

  VarGLOBTESTSETUP1OBJECT_9 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1OBJECT_9.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1OBJECT_9.Children := 0;
  VarGLOBTESTSETUP1OBJECT_9.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1OBJECT'+#0);
  VarGLOBTESTSETUP1OBJECT_9.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB($00409080)])); // $03, $80, $90, $40, $00
  VarGLOBTESTSETUP1OBJECT_9.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_130); // $05, $09, $00, $00

  VarGLOBTESTSETUP1OBJECTP_10 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1OBJECTP_10.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1OBJECTP_10.Children := 0;
  VarGLOBTESTSETUP1OBJECTP_10.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1OBJECTP'+#0);
  VarGLOBTESTSETUP1OBJECTP_10.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB($004090C0)])); // $03, $C0, $90, $40, $00
  VarGLOBTESTSETUP1OBJECTP_10.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1OBJECT_145); // $F6, $09, $00, $00

  VarGLOBTESTSETUP1POINTER_11 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1POINTER_11.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1POINTER_11.Children := 0;
  VarGLOBTESTSETUP1POINTER_11.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1POINTER'+#0);
  VarGLOBTESTSETUP1POINTER_11.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB($00000000)])); // $03, $00, $00, $00, $00
  VarGLOBTESTSETUP1POINTER_11.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_85); // $C0, $06, $00, $00

  VarGLOBTESTSETUP1QWORD_12 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1QWORD_12.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1QWORD_12.Children := 0;
  VarGLOBTESTSETUP1QWORD_12.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1QWORD'+#0);
  VarGLOBTESTSETUP1QWORD_12.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB($00000000)])); // $03, $00, $00, $00, $00
  VarGLOBTESTSETUP1QWORD_12.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_79); // $8A, $06, $00, $00

  Progmain_13 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Progmain_13.Tag := DW_TAG_subprogram;
  Progmain_13.Children := 0;
  Progmain_13.Add(DW_AT_name, DW_FORM_string, 'main'+#0);
  Progmain_13.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
  Progmain_13.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
  Progmain_13.Add(DW_AT_external, DW_FORM_flag, [$01]);
  Progmain_13.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00421490);
  Progmain_13.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00421510);

  ProgTESTSETUP1BAR_14 := Unitdwarfsetup1_lpr_0.GetNewChild;
  ProgTESTSETUP1BAR_14.Tag := DW_TAG_subprogram;
  ProgTESTSETUP1BAR_14.Children := 1;
  ProgTESTSETUP1BAR_14.Add(DW_AT_name, DW_FORM_string, 'TESTSETUP1BAR'+#0);
  ProgTESTSETUP1BAR_14.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
  ProgTESTSETUP1BAR_14.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
  ProgTESTSETUP1BAR_14.Add(DW_AT_external, DW_FORM_flag, [$01]);
  ProgTESTSETUP1BAR_14.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_85); // $C0, $06, $00, $00
  ProgTESTSETUP1BAR_14.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00400000);
  ProgTESTSETUP1BAR_14.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00410000);
//    StackOffs := @TestStackFrame.VObj1 - @TestStackFrame.EndPoint;

    StackOffs := @TestStackFrame.PARAMTESTSETUP1RECORD - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1RECORD_15 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarPARAMTESTSETUP1RECORD_15.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1RECORD_15.Children := 0;
    VarPARAMTESTSETUP1RECORD_15.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1RECORD'+#0);
    VarPARAMTESTSETUP1RECORD_15.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $EC, $7E
    VarPARAMTESTSETUP1RECORD_15.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD_102); // $90, $07, $00, $00

    StackOffs := @TestStackFrame.PARAMTESTRECORD - @TestStackFrame.EndPoint;
    VarPARAMTESTRECORD_16 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarPARAMTESTRECORD_16.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTRECORD_16.Children := 0;
    VarPARAMTESTRECORD_16.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTRECORD'+#0);
    VarPARAMTESTRECORD_16.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $78
    VarPARAMTESTRECORD_16.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1RECORD_108); // $EB, $07, $00, $00

    StackOffs := @TestStackFrame.PARAMTESTSETUP1CLASS - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1CLASS_17 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarPARAMTESTSETUP1CLASS_17.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1CLASS_17.Children := 0;
    VarPARAMTESTSETUP1CLASS_17.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1CLASS'+#0);
    VarPARAMTESTSETUP1CLASS_17.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $74
    VarPARAMTESTSETUP1CLASS_17.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_88); // $D3, $06, $00, $00

    StackOffs := @TestStackFrame.PARAMTESTSETUP1CLASSP - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1CLASSP_18 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarPARAMTESTSETUP1CLASSP_18.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1CLASSP_18.Children := 0;
    VarPARAMTESTSETUP1CLASSP_18.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1CLASSP'+#0);
    VarPARAMTESTSETUP1CLASSP_18.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $C8, $00
    VarPARAMTESTSETUP1CLASSP_18.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1CLASS_111); // $0C, $08, $00, $00

    StackOffs := @TestStackFrame.PARAMTESTSETUP1CLASSCHILD - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1CLASSCHILD_19 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarPARAMTESTSETUP1CLASSCHILD_19.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1CLASSCHILD_19.Children := 0;
    VarPARAMTESTSETUP1CLASSCHILD_19.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1CLASSCHILD'+#0);
    VarPARAMTESTSETUP1CLASSCHILD_19.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $C4, $00
    VarPARAMTESTSETUP1CLASSCHILD_19.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCHILD_114); // $2C, $08, $00, $00

    StackOffs := @TestStackFrame.PARAMTESTSETUP1CLASSCHILDP - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1CLASSCHILDP_20 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarPARAMTESTSETUP1CLASSCHILDP_20.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1CLASSCHILDP_20.Children := 0;
    VarPARAMTESTSETUP1CLASSCHILDP_20.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1CLASSCHILDP'+#0);
    VarPARAMTESTSETUP1CLASSCHILDP_20.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $C0, $00
    VarPARAMTESTSETUP1CLASSCHILDP_20.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1CLASSCHILD_121); // $91, $08, $00, $00

    StackOffs := @TestStackFrame.PARAMTESTSETUP1CLASSCLASS - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1CLASSCLASS_21 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarPARAMTESTSETUP1CLASSCLASS_21.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1CLASSCLASS_21.Children := 0;
    VarPARAMTESTSETUP1CLASSCLASS_21.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1CLASSCLASS'+#0);
    VarPARAMTESTSETUP1CLASSCLASS_21.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $3C
    VarPARAMTESTSETUP1CLASSCLASS_21.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCLASS_124); // $B6, $08, $00, $00

    StackOffs := @TestStackFrame.PARAMTESTSETUP1CLASSCHILDCLASS - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1CLASSCHILDCLASS_22 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarPARAMTESTSETUP1CLASSCHILDCLASS_22.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1CLASSCHILDCLASS_22.Children := 0;
    VarPARAMTESTSETUP1CLASSCHILDCLASS_22.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1CLASSCHILDCLASS'+#0);
    VarPARAMTESTSETUP1CLASSCHILDCLASS_22.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $38
    VarPARAMTESTSETUP1CLASSCHILDCLASS_22.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCHILDCLASS_127); // $DB, $08, $00, $00

    StackOffs := @TestStackFrame.PARAMTESTSETUP1OBJECT - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1OBJECT_23 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarPARAMTESTSETUP1OBJECT_23.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1OBJECT_23.Children := 0;
    VarPARAMTESTSETUP1OBJECT_23.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1OBJECT'+#0);
    VarPARAMTESTSETUP1OBJECT_23.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $B0, $7E
    VarPARAMTESTSETUP1OBJECT_23.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_130); // $05, $09, $00, $00

    StackOffs := @TestStackFrame.PARAMTESTSETUP1OBJECTP - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1OBJECTP_24 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarPARAMTESTSETUP1OBJECTP_24.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1OBJECTP_24.Children := 0;
    VarPARAMTESTSETUP1OBJECTP_24.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1OBJECTP'+#0);
    VarPARAMTESTSETUP1OBJECTP_24.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $30
    VarPARAMTESTSETUP1OBJECTP_24.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1OBJECT_145); // $F6, $09, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTSETUP1RECORD - @TestStackFrame.EndPoint;
    VarVPARAMTESTSETUP1RECORD_25 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarVPARAMTESTSETUP1RECORD_25.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTSETUP1RECORD_25.Children := 0;
    VarVPARAMTESTSETUP1RECORD_25.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTSETUP1RECORD'+#0);
    VarVPARAMTESTSETUP1RECORD_25.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $2C, $06
    VarVPARAMTESTSETUP1RECORD_25.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD_102); // $90, $07, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTRECORD - @TestStackFrame.EndPoint;
    VarVPARAMTESTRECORD_26 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarVPARAMTESTRECORD_26.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTRECORD_26.Children := 0;
    VarVPARAMTESTRECORD_26.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTRECORD'+#0);
    VarVPARAMTESTRECORD_26.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $28, $06
    VarVPARAMTESTRECORD_26.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1RECORD_108); // $EB, $07, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTSETUP1CLASS - @TestStackFrame.EndPoint;
    VarVPARAMTESTSETUP1CLASS_27 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarVPARAMTESTSETUP1CLASS_27.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTSETUP1CLASS_27.Children := 0;
    VarVPARAMTESTSETUP1CLASS_27.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTSETUP1CLASS'+#0);
    VarVPARAMTESTSETUP1CLASS_27.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $24, $06
    VarVPARAMTESTSETUP1CLASS_27.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_88); // $D3, $06, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTSETUP1CLASSP - @TestStackFrame.EndPoint;
    VarVPARAMTESTSETUP1CLASSP_28 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarVPARAMTESTSETUP1CLASSP_28.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTSETUP1CLASSP_28.Children := 0;
    VarVPARAMTESTSETUP1CLASSP_28.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTSETUP1CLASSP'+#0);
    VarVPARAMTESTSETUP1CLASSP_28.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $20, $06
    VarVPARAMTESTSETUP1CLASSP_28.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1CLASS_111); // $0C, $08, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTSETUP1CLASSCHILD - @TestStackFrame.EndPoint;
    VarVPARAMTESTSETUP1CLASSCHILD_29 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarVPARAMTESTSETUP1CLASSCHILD_29.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTSETUP1CLASSCHILD_29.Children := 0;
    VarVPARAMTESTSETUP1CLASSCHILD_29.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTSETUP1CLASSCHILD'+#0);
    VarVPARAMTESTSETUP1CLASSCHILD_29.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $1C, $06
    VarVPARAMTESTSETUP1CLASSCHILD_29.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCHILD_114); // $2C, $08, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTSETUP1CLASSCHILDP - @TestStackFrame.EndPoint;
    VarVPARAMTESTSETUP1CLASSCHILDP_30 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarVPARAMTESTSETUP1CLASSCHILDP_30.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTSETUP1CLASSCHILDP_30.Children := 0;
    VarVPARAMTESTSETUP1CLASSCHILDP_30.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTSETUP1CLASSCHILDP'+#0);
    VarVPARAMTESTSETUP1CLASSCHILDP_30.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $18, $06
    VarVPARAMTESTSETUP1CLASSCHILDP_30.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1CLASSCHILD_121); // $91, $08, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTSETUP1CLASSCLASS - @TestStackFrame.EndPoint;
    VarVPARAMTESTSETUP1CLASSCLASS_31 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarVPARAMTESTSETUP1CLASSCLASS_31.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTSETUP1CLASSCLASS_31.Children := 0;
    VarVPARAMTESTSETUP1CLASSCLASS_31.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTSETUP1CLASSCLASS'+#0);
    VarVPARAMTESTSETUP1CLASSCLASS_31.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $14, $06
    VarVPARAMTESTSETUP1CLASSCLASS_31.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCLASS_124); // $B6, $08, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTSETUP1CLASSCHILDCLASS - @TestStackFrame.EndPoint;
    VarVPARAMTESTSETUP1CLASSCHILDCLASS_32 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarVPARAMTESTSETUP1CLASSCHILDCLASS_32.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTSETUP1CLASSCHILDCLASS_32.Children := 0;
    VarVPARAMTESTSETUP1CLASSCHILDCLASS_32.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTSETUP1CLASSCHILDCLASS'+#0);
    VarVPARAMTESTSETUP1CLASSCHILDCLASS_32.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $10, $06
    VarVPARAMTESTSETUP1CLASSCHILDCLASS_32.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCHILDCLASS_127); // $DB, $08, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTSETUP1OBJECT - @TestStackFrame.EndPoint;
    VarVPARAMTESTSETUP1OBJECT_33 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarVPARAMTESTSETUP1OBJECT_33.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTSETUP1OBJECT_33.Children := 0;
    VarVPARAMTESTSETUP1OBJECT_33.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTSETUP1OBJECT'+#0);
    VarVPARAMTESTSETUP1OBJECT_33.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $0C, $06
    VarVPARAMTESTSETUP1OBJECT_33.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_130); // $05, $09, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTSETUP1OBJECTP - @TestStackFrame.EndPoint;
    VarVPARAMTESTSETUP1OBJECTP_34 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarVPARAMTESTSETUP1OBJECTP_34.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTSETUP1OBJECTP_34.Children := 0;
    VarVPARAMTESTSETUP1OBJECTP_34.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTSETUP1OBJECTP'+#0);
    VarVPARAMTESTSETUP1OBJECTP_34.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $08, $06
    VarVPARAMTESTSETUP1OBJECTP_34.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1OBJECT_145); // $F6, $09, $00, $00

    StackOffs := @TestStackFrame.INT1 - @TestStackFrame.EndPoint;
    VarINT1_35 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarINT1_35.Tag := DW_TAG_variable;
    VarINT1_35.Children := 0;
    VarINT1_35.Add(DW_AT_name, DW_FORM_string, 'INT1'+#0);
    VarINT1_35.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $6C
    VarINT1_35.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_76); // $6D, $06, $00, $00

    StackOffs := @TestStackFrame.PINT1 - @TestStackFrame.EndPoint;
    VarPINT1_36 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarPINT1_36.Tag := DW_TAG_variable;
    VarPINT1_36.Children := 0;
    VarPINT1_36.Add(DW_AT_name, DW_FORM_string, 'PINT1'+#0);
    VarPINT1_36.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $68
    VarPINT1_36.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_51); // $B8, $05, $00, $00

    StackOffs := @TestStackFrame.BOOL1 - @TestStackFrame.EndPoint;
    VarBOOL1_37 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarBOOL1_37.Tag := DW_TAG_variable;
    VarBOOL1_37.Children := 0;
    VarBOOL1_37.Add(DW_AT_name, DW_FORM_string, 'BOOL1'+#0);
    VarBOOL1_37.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $64
    VarBOOL1_37.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_82); // $A3, $06, $00, $00

    StackOffs := @TestStackFrame.OBJ1 - @TestStackFrame.EndPoint;
    VarOBJ1_38 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarOBJ1_38.Tag := DW_TAG_variable;
    VarOBJ1_38.Children := 0;
    VarOBJ1_38.Add(DW_AT_name, DW_FORM_string, 'OBJ1'+#0);
    VarOBJ1_38.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $60
    VarOBJ1_38.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_88); // $D3, $06, $00, $00

    StackOffs := @TestStackFrame.POBJ1 - @TestStackFrame.EndPoint;
    VarPOBJ1_39 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarPOBJ1_39.Tag := DW_TAG_variable;
    VarPOBJ1_39.Children := 0;
    VarPOBJ1_39.Add(DW_AT_name, DW_FORM_string, 'POBJ1'+#0);
    VarPOBJ1_39.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $5C
    VarPOBJ1_39.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_53); // $C2, $05, $00, $00

    StackOffs := @TestStackFrame.OLDOBJ1 - @TestStackFrame.EndPoint;
    VarOLDOBJ1_40 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarOLDOBJ1_40.Tag := DW_TAG_variable;
    VarOLDOBJ1_40.Children := 0;
    VarOLDOBJ1_40.Add(DW_AT_name, DW_FORM_string, 'OLDOBJ1'+#0);
    VarOLDOBJ1_40.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $A0, $7F
    VarOLDOBJ1_40.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_130); // $05, $09, $00, $00

    StackOffs := @TestStackFrame.POLDOBJ1 - @TestStackFrame.EndPoint;
    VarPOLDOBJ1_41 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarPOLDOBJ1_41.Tag := DW_TAG_variable;
    VarPOLDOBJ1_41.Children := 0;
    VarPOLDOBJ1_41.Add(DW_AT_name, DW_FORM_string, 'POLDOBJ1'+#0);
    VarPOLDOBJ1_41.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $9C, $7F
    VarPOLDOBJ1_41.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1OBJECT_145); // $F6, $09, $00, $00

    StackOffs := @TestStackFrame.REC1 - @TestStackFrame.EndPoint;
    VarREC1_42 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarREC1_42.Tag := DW_TAG_variable;
    VarREC1_42.Children := 0;
    VarREC1_42.Add(DW_AT_name, DW_FORM_string, 'REC1'+#0);
    VarREC1_42.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $94, $7F
    VarREC1_42.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD_102); // $90, $07, $00, $00

    StackOffs := @TestStackFrame.PREC1 - @TestStackFrame.EndPoint;
    VarPREC1_43 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarPREC1_43.Tag := DW_TAG_variable;
    VarPREC1_43.Children := 0;
    VarPREC1_43.Add(DW_AT_name, DW_FORM_string, 'PREC1'+#0);
    VarPREC1_43.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $90, $7F
    VarPREC1_43.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_55); // $CC, $05, $00, $00

    StackOffs := @TestStackFrame.REC2 - @TestStackFrame.EndPoint;
    VarREC2_44 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarREC2_44.Tag := DW_TAG_variable;
    VarREC2_44.Children := 0;
    VarREC2_44.Add(DW_AT_name, DW_FORM_string, 'REC2'+#0);
    VarREC2_44.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $8C, $7F
    VarREC2_44.AddRef(DW_AT_type, DW_FORM_ref4, @Type_57); // $D6, $05, $00, $00

    StackOffs := @TestStackFrame.PI - @TestStackFrame.EndPoint;
    VarPI_45 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarPI_45.Tag := DW_TAG_variable;
    VarPI_45.Children := 0;
    VarPI_45.Add(DW_AT_name, DW_FORM_string, 'PI'+#0);
    VarPI_45.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $88, $7F
    VarPI_45.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINT_148); // $17, $0A, $00, $00

    StackOffs := @TestStackFrame.PPI - @TestStackFrame.EndPoint;
    VarPPI_46 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarPPI_46.Tag := DW_TAG_variable;
    VarPPI_46.Children := 0;
    VarPPI_46.Add(DW_AT_name, DW_FORM_string, 'PPI'+#0);
    VarPPI_46.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $84, $7F
    VarPPI_46.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPPINT_151); // $2B, $0A, $00, $00

    StackOffs := @TestStackFrame.PPPI - @TestStackFrame.EndPoint;
    VarPPPI_47 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarPPPI_47.Tag := DW_TAG_variable;
    VarPPPI_47.Children := 0;
    VarPPPI_47.Add(DW_AT_name, DW_FORM_string, 'PPPI'+#0);
    VarPPPI_47.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $80, $7F
    VarPPPI_47.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPPPINT_154); // $40, $0A, $00, $00

    StackOffs := @TestStackFrame.SUBR - @TestStackFrame.EndPoint;
    VarSUBR_48 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarSUBR_48.Tag := DW_TAG_variable;
    VarSUBR_48.Children := 0;
    VarSUBR_48.Add(DW_AT_name, DW_FORM_string, 'SUBR'+#0);
    VarSUBR_48.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $FC, $7E
    VarSUBR_48.AddRef(DW_AT_type, DW_FORM_ref4, @Type_61); // $FA, $05, $00, $00

    StackOffs := @TestStackFrame.SUBR2 - @TestStackFrame.EndPoint;
    VarSUBR2_49 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarSUBR2_49.Tag := DW_TAG_variable;
    VarSUBR2_49.Children := 0;
    VarSUBR2_49.Add(DW_AT_name, DW_FORM_string, 'SUBR2'+#0);
    VarSUBR2_49.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $F8, $7E
    VarSUBR2_49.AddRef(DW_AT_type, DW_FORM_ref4, @Type_63); // $06, $06, $00, $00

    StackOffs := @TestStackFrame.SUBR3 - @TestStackFrame.EndPoint;
    VarSUBR3_50 := ProgTESTSETUP1BAR_14.GetNewChild;
    VarSUBR3_50.Tag := DW_TAG_variable;
    VarSUBR3_50.Children := 0;
    VarSUBR3_50.Add(DW_AT_name, DW_FORM_string, 'SUBR3'+#0);
    VarSUBR3_50.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $F4, $7E
    VarSUBR3_50.AddRef(DW_AT_type, DW_FORM_ref4, @TypeChar_65); // $12, $06, $00, $00

    TypePtr_51 := ProgTESTSETUP1BAR_14.GetNewChild;
    TypePtr_51.Tag := DW_TAG_pointer_type;
    TypePtr_51.Children := 0;
    TypePtr_51.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_76); // $6D, $06, $00, $00

    Type_52 := ProgTESTSETUP1BAR_14.GetNewChild;
    Type_52.Tag := DW_TAG_reference_type;
    Type_52.Children := 0;
    Type_52.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_51); // $B8, $05, $00, $00

    TypePtr_53 := ProgTESTSETUP1BAR_14.GetNewChild;
    TypePtr_53.Tag := DW_TAG_pointer_type;
    TypePtr_53.Children := 0;
    TypePtr_53.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_88); // $D3, $06, $00, $00

    Type_54 := ProgTESTSETUP1BAR_14.GetNewChild;
    Type_54.Tag := DW_TAG_reference_type;
    Type_54.Children := 0;
    Type_54.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_53); // $C2, $05, $00, $00

    TypePtr_55 := ProgTESTSETUP1BAR_14.GetNewChild;
    TypePtr_55.Tag := DW_TAG_pointer_type;
    TypePtr_55.Children := 0;
    TypePtr_55.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD_102); // $90, $07, $00, $00

    Type_56 := ProgTESTSETUP1BAR_14.GetNewChild;
    Type_56.Tag := DW_TAG_reference_type;
    Type_56.Children := 0;
    Type_56.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_55); // $CC, $05, $00, $00

    Type_57 := ProgTESTSETUP1BAR_14.GetNewChild;
    Type_57.Tag := DW_TAG_structure_type;
    Type_57.Children := 1;
    Type_57.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 4);

      VarFWORD_58 := Type_57.GetNewChild;
      VarFWORD_58.Tag := DW_TAG_member;
      VarFWORD_58.Children := 0;
      VarFWORD_58.Add(DW_AT_name, DW_FORM_string, 'FWORD'+#0);
      VarFWORD_58.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
      VarFWORD_58.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_73); // $56, $06, $00, $00

      VarFBOOL_59 := Type_57.GetNewChild;
      VarFBOOL_59.Tag := DW_TAG_member;
      VarFBOOL_59.Children := 0;
      VarFBOOL_59.Add(DW_AT_name, DW_FORM_string, 'FBOOL'+#0);
      VarFBOOL_59.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(2)])); // $23, $02
      VarFBOOL_59.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_82); // $A3, $06, $00, $00

    Type_60 := ProgTESTSETUP1BAR_14.GetNewChild;
    Type_60.Tag := DW_TAG_reference_type;
    Type_60.Children := 0;
    Type_60.AddRef(DW_AT_type, DW_FORM_ref4, @Type_57); // $D6, $05, $00, $00

    Type_61 := ProgTESTSETUP1BAR_14.GetNewChild;
    Type_61.Tag := DW_TAG_subrange_type;
    Type_61.Children := 0;
    Type_61.AddSLEB(DW_AT_lower_bound, DW_FORM_udata, 1);
    Type_61.AddSLEB(DW_AT_upper_bound, DW_FORM_udata, 9);
    Type_61.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_67); // $20, $06, $00, $00

    Type_62 := ProgTESTSETUP1BAR_14.GetNewChild;
    Type_62.Tag := DW_TAG_reference_type;
    Type_62.Children := 0;
    Type_62.AddRef(DW_AT_type, DW_FORM_ref4, @Type_61); // $FA, $05, $00, $00

    Type_63 := ProgTESTSETUP1BAR_14.GetNewChild;
    Type_63.Tag := DW_TAG_subrange_type;
    Type_63.Children := 0;
    Type_63.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 117);
    Type_63.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 119);
    Type_63.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_70); // $37, $06, $00, $00

    Type_64 := ProgTESTSETUP1BAR_14.GetNewChild;
    Type_64.Tag := DW_TAG_reference_type;
    Type_64.Children := 0;
    Type_64.AddRef(DW_AT_type, DW_FORM_ref4, @Type_63); // $06, $06, $00, $00

    TypeChar_65 := ProgTESTSETUP1BAR_14.GetNewChild;
    TypeChar_65.Tag := DW_TAG_base_type;
    TypeChar_65.Children := 0;
    TypeChar_65.Add(DW_AT_name, DW_FORM_string, 'Char'+#0);
    TypeChar_65.Add(DW_AT_encoding, DW_FORM_data1, [$08]);
    TypeChar_65.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

    Type_66 := ProgTESTSETUP1BAR_14.GetNewChild;
    Type_66.Tag := DW_TAG_reference_type;
    Type_66.Children := 0;
    Type_66.AddRef(DW_AT_type, DW_FORM_ref4, @TypeChar_65); // $12, $06, $00, $00

  TypeDeclBYTE_67 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclBYTE_67.Tag := DW_TAG_typedef;
  TypeDeclBYTE_67.Children := 0;
  TypeDeclBYTE_67.Add(DW_AT_name, DW_FORM_string, 'BYTE'+#0);
  TypeDeclBYTE_67.AddRef(DW_AT_type, DW_FORM_ref4, @TypeBYTE_68); // $2A, $06, $00, $00

  TypeBYTE_68 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeBYTE_68.Tag := DW_TAG_base_type;
  TypeBYTE_68.Children := 0;
  TypeBYTE_68.Add(DW_AT_name, DW_FORM_string, 'BYTE'+#0);
  TypeBYTE_68.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeBYTE_68.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_69 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_69.Tag := DW_TAG_reference_type;
  Type_69.Children := 0;
  Type_69.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_67); // $20, $06, $00, $00

  TypeDeclSHORTINT_70 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclSHORTINT_70.Tag := DW_TAG_typedef;
  TypeDeclSHORTINT_70.Children := 0;
  TypeDeclSHORTINT_70.Add(DW_AT_name, DW_FORM_string, 'SHORTINT'+#0);
  TypeDeclSHORTINT_70.AddRef(DW_AT_type, DW_FORM_ref4, @TypeSHORTINT_71); // $45, $06, $00, $00

  TypeSHORTINT_71 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeSHORTINT_71.Tag := DW_TAG_base_type;
  TypeSHORTINT_71.Children := 0;
  TypeSHORTINT_71.Add(DW_AT_name, DW_FORM_string, 'SHORTINT'+#0);
  TypeSHORTINT_71.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeSHORTINT_71.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_72 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_72.Tag := DW_TAG_reference_type;
  Type_72.Children := 0;
  Type_72.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_70); // $37, $06, $00, $00

  TypeDeclWORD_73 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclWORD_73.Tag := DW_TAG_typedef;
  TypeDeclWORD_73.Children := 0;
  TypeDeclWORD_73.Add(DW_AT_name, DW_FORM_string, 'WORD'+#0);
  TypeDeclWORD_73.AddRef(DW_AT_type, DW_FORM_ref4, @TypeWORD_74); // $60, $06, $00, $00

  TypeWORD_74 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeWORD_74.Tag := DW_TAG_base_type;
  TypeWORD_74.Children := 0;
  TypeWORD_74.Add(DW_AT_name, DW_FORM_string, 'WORD'+#0);
  TypeWORD_74.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeWORD_74.Add(DW_AT_byte_size, DW_FORM_data1, [$02]);

  Type_75 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_75.Tag := DW_TAG_reference_type;
  Type_75.Children := 0;
  Type_75.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_73); // $56, $06, $00, $00

  TypeDeclLONGINT_76 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclLONGINT_76.Tag := DW_TAG_typedef;
  TypeDeclLONGINT_76.Children := 0;
  TypeDeclLONGINT_76.Add(DW_AT_name, DW_FORM_string, 'LONGINT'+#0);
  TypeDeclLONGINT_76.AddRef(DW_AT_type, DW_FORM_ref4, @TypeLONGINT_77); // $7A, $06, $00, $00

  TypeLONGINT_77 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeLONGINT_77.Tag := DW_TAG_base_type;
  TypeLONGINT_77.Children := 0;
  TypeLONGINT_77.Add(DW_AT_name, DW_FORM_string, 'LONGINT'+#0);
  TypeLONGINT_77.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeLONGINT_77.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

  Type_78 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_78.Tag := DW_TAG_reference_type;
  Type_78.Children := 0;
  Type_78.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_76); // $6D, $06, $00, $00

  TypeDeclQWORD_79 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclQWORD_79.Tag := DW_TAG_typedef;
  TypeDeclQWORD_79.Children := 0;
  TypeDeclQWORD_79.Add(DW_AT_name, DW_FORM_string, 'QWORD'+#0);
  TypeDeclQWORD_79.AddRef(DW_AT_type, DW_FORM_ref4, @TypeQWord_80); // $95, $06, $00, $00

  TypeQWord_80 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeQWord_80.Tag := DW_TAG_base_type;
  TypeQWord_80.Children := 0;
  TypeQWord_80.Add(DW_AT_name, DW_FORM_string, 'QWord'+#0);
  TypeQWord_80.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeQWord_80.Add(DW_AT_byte_size, DW_FORM_data1, [$08]);

  Type_81 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_81.Tag := DW_TAG_reference_type;
  Type_81.Children := 0;
  Type_81.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_79); // $8A, $06, $00, $00

  TypeDeclBOOLEAN_82 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclBOOLEAN_82.Tag := DW_TAG_typedef;
  TypeDeclBOOLEAN_82.Children := 0;
  TypeDeclBOOLEAN_82.Add(DW_AT_name, DW_FORM_string, 'BOOLEAN'+#0);
  TypeDeclBOOLEAN_82.AddRef(DW_AT_type, DW_FORM_ref4, @TypeBoolean_83); // $B0, $06, $00, $00

  TypeBoolean_83 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeBoolean_83.Tag := DW_TAG_base_type;
  TypeBoolean_83.Children := 0;
  TypeBoolean_83.Add(DW_AT_name, DW_FORM_string, 'Boolean'+#0);
  TypeBoolean_83.Add(DW_AT_encoding, DW_FORM_data1, [$02]);
  TypeBoolean_83.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_84 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_84.Tag := DW_TAG_reference_type;
  Type_84.Children := 0;
  Type_84.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_82); // $A3, $06, $00, $00

  TypeDeclPOINTER_85 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPOINTER_85.Tag := DW_TAG_typedef;
  TypeDeclPOINTER_85.Children := 0;
  TypeDeclPOINTER_85.Add(DW_AT_name, DW_FORM_string, 'POINTER'+#0);
  TypeDeclPOINTER_85.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_86); // $CD, $06, $00, $00

  TypePtr_86 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_86.Tag := DW_TAG_pointer_type;
  TypePtr_86.Children := 0;

  Type_87 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_87.Tag := DW_TAG_reference_type;
  Type_87.Children := 0;
  Type_87.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_85); // $C0, $06, $00, $00

  TypeDeclTTESTSETUP1CLASS_88 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTTESTSETUP1CLASS_88.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1CLASS_88.Children := 0;
  TypeDeclTTESTSETUP1CLASS_88.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1CLASS'+#0);
  TypeDeclTTESTSETUP1CLASS_88.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_89); // $E9, $06, $00, $00

  TypePtr_89 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_89.Tag := DW_TAG_pointer_type;
  TypePtr_89.Children := 0;
  TypePtr_89.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1CLASS_90); // $EE, $06, $00, $00

  TypeTTESTSETUP1CLASS_90 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTTESTSETUP1CLASS_90.Tag := DW_TAG_structure_type;
  TypeTTESTSETUP1CLASS_90.Children := 1;
  TypeTTESTSETUP1CLASS_90.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1CLASS'+#0);
  TypeTTESTSETUP1CLASS_90.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 40);

    XX_91 := TypeTTESTSETUP1CLASS_90.GetNewChild;
    XX_91.Tag := DW_TAG_inheritance;
    XX_91.Children := 0;
    XX_91.Add(DW_AT_accessibility, DW_FORM_data1, [$01]);
    XX_91.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    XX_91.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTOBJECT_162); // $7E, $0A, $00, $00

    VarFWORD_92 := TypeTTESTSETUP1CLASS_90.GetNewChild;
    VarFWORD_92.Tag := DW_TAG_member;
    VarFWORD_92.Children := 0;
    VarFWORD_92.Add(DW_AT_name, DW_FORM_string, 'FWORD'+#0);
    VarFWORD_92.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarFWORD_92.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_73); // $56, $06, $00, $00

    VarFWORDL_93 := TypeTTESTSETUP1CLASS_90.GetNewChild;
    VarFWORDL_93.Tag := DW_TAG_member;
    VarFWORDL_93.Children := 0;
    VarFWORDL_93.Add(DW_AT_name, DW_FORM_string, 'FWORDL'+#0);
    VarFWORDL_93.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(8)])); // $23, $08
    VarFWORDL_93.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_79); // $8A, $06, $00, $00

    VarFINT_94 := TypeTTESTSETUP1CLASS_90.GetNewChild;
    VarFINT_94.Tag := DW_TAG_member;
    VarFINT_94.Children := 0;
    VarFINT_94.Add(DW_AT_name, DW_FORM_string, 'FINT'+#0);
    VarFINT_94.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(16)])); // $23, $10
    VarFINT_94.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_70); // $37, $06, $00, $00

    VarFINTL_95 := TypeTTESTSETUP1CLASS_90.GetNewChild;
    VarFINTL_95.Tag := DW_TAG_member;
    VarFINTL_95.Children := 0;
    VarFINTL_95.Add(DW_AT_name, DW_FORM_string, 'FINTL'+#0);
    VarFINTL_95.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(24)])); // $23, $18
    VarFINTL_95.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclINT64_266); // $A6, $10, $00, $00

    VarFBOOL_96 := TypeTTESTSETUP1CLASS_90.GetNewChild;
    VarFBOOL_96.Tag := DW_TAG_member;
    VarFBOOL_96.Children := 0;
    VarFBOOL_96.Add(DW_AT_name, DW_FORM_string, 'FBOOL'+#0);
    VarFBOOL_96.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(32)])); // $23, $20
    VarFBOOL_96.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_82); // $A3, $06, $00, $00

    VarFTEST_97 := TypeTTESTSETUP1CLASS_90.GetNewChild;
    VarFTEST_97.Tag := DW_TAG_member;
    VarFTEST_97.Children := 0;
    VarFTEST_97.Add(DW_AT_name, DW_FORM_string, 'FTEST'+#0);
    VarFTEST_97.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(36)])); // $23, $24
    VarFTEST_97.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_88); // $D3, $06, $00, $00

    ProgF0_98 := TypeTTESTSETUP1CLASS_90.GetNewChild;
    ProgF0_98.Tag := DW_TAG_subprogram;
    ProgF0_98.Children := 1;
    ProgF0_98.Add(DW_AT_name, DW_FORM_string, 'F0'+#0);
    ProgF0_98.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgF0_98.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgF0_98.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgF0_98.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgF0_98.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $19]));
    ProgF0_98.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00440000);
    ProgF0_98.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00450000);

      Varthis_99 := ProgF0_98.GetNewChild;
      Varthis_99.Tag := DW_TAG_formal_parameter;
      Varthis_99.Children := 0;
      Varthis_99.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_99.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-8)])); // $75, $78
      Varthis_99.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_99.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_88); // $D3, $06, $00, $00

      VarA_100 := ProgF0_98.GetNewChild;
      VarA_100.Tag := DW_TAG_formal_parameter;
      VarA_100.Children := 0;
      VarA_100.Add(DW_AT_name, DW_FORM_string, 'A'+#0);
      VarA_100.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-4)])); // $75, $7C
      VarA_100.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_76); // $6D, $06, $00, $00

  Type_101 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_101.Tag := DW_TAG_reference_type;
  Type_101.Children := 0;
  Type_101.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_88); // $D3, $06, $00, $00

  TypeDeclTTESTSETUP1RECORD_102 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTTESTSETUP1RECORD_102.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1RECORD_102.Children := 0;
  TypeDeclTTESTSETUP1RECORD_102.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1RECORD'+#0);
  TypeDeclTTESTSETUP1RECORD_102.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1RECORD_103); // $A7, $07, $00, $00

  TypeTTESTSETUP1RECORD_103 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTTESTSETUP1RECORD_103.Tag := DW_TAG_structure_type;
  TypeTTESTSETUP1RECORD_103.Children := 1;
  TypeTTESTSETUP1RECORD_103.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1RECORD'+#0);
  TypeTTESTSETUP1RECORD_103.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 8);

    VarFWORD_104 := TypeTTESTSETUP1RECORD_103.GetNewChild;
    VarFWORD_104.Tag := DW_TAG_member;
    VarFWORD_104.Children := 0;
    VarFWORD_104.Add(DW_AT_name, DW_FORM_string, 'FWORD'+#0);
    VarFWORD_104.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarFWORD_104.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_73); // $56, $06, $00, $00

    VarFBOOL_105 := TypeTTESTSETUP1RECORD_103.GetNewChild;
    VarFBOOL_105.Tag := DW_TAG_member;
    VarFBOOL_105.Children := 0;
    VarFBOOL_105.Add(DW_AT_name, DW_FORM_string, 'FBOOL'+#0);
    VarFBOOL_105.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(2)])); // $23, $02
    VarFBOOL_105.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_82); // $A3, $06, $00, $00

    VarFTEST_106 := TypeTTESTSETUP1RECORD_103.GetNewChild;
    VarFTEST_106.Tag := DW_TAG_member;
    VarFTEST_106.Children := 0;
    VarFTEST_106.Add(DW_AT_name, DW_FORM_string, 'FTEST'+#0);
    VarFTEST_106.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarFTEST_106.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_88); // $D3, $06, $00, $00

  Type_107 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_107.Tag := DW_TAG_reference_type;
  Type_107.Children := 0;
  Type_107.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD_102); // $90, $07, $00, $00

  TypeDeclPTESTSETUP1RECORD_108 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPTESTSETUP1RECORD_108.Tag := DW_TAG_typedef;
  TypeDeclPTESTSETUP1RECORD_108.Children := 0;
  TypeDeclPTESTSETUP1RECORD_108.Add(DW_AT_name, DW_FORM_string, 'PTESTSETUP1RECORD'+#0);
  TypeDeclPTESTSETUP1RECORD_108.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_109); // $02, $08, $00, $00

  TypePtr_109 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_109.Tag := DW_TAG_pointer_type;
  TypePtr_109.Children := 0;
  TypePtr_109.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD_102); // $90, $07, $00, $00

  Type_110 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_110.Tag := DW_TAG_reference_type;
  Type_110.Children := 0;
  Type_110.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1RECORD_108); // $EB, $07, $00, $00

  TypeDeclPTESTSETUP1CLASS_111 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPTESTSETUP1CLASS_111.Tag := DW_TAG_typedef;
  TypeDeclPTESTSETUP1CLASS_111.Children := 0;
  TypeDeclPTESTSETUP1CLASS_111.Add(DW_AT_name, DW_FORM_string, 'PTESTSETUP1CLASS'+#0);
  TypeDeclPTESTSETUP1CLASS_111.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_112); // $22, $08, $00, $00

  TypePtr_112 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_112.Tag := DW_TAG_pointer_type;
  TypePtr_112.Children := 0;
  TypePtr_112.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_88); // $D3, $06, $00, $00

  Type_113 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_113.Tag := DW_TAG_reference_type;
  Type_113.Children := 0;
  Type_113.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1CLASS_111); // $0C, $08, $00, $00

  TypeDeclTTESTSETUP1CLASSCHILD_114 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTTESTSETUP1CLASSCHILD_114.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1CLASSCHILD_114.Children := 0;
  TypeDeclTTESTSETUP1CLASSCHILD_114.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1CLASSCHILD'+#0);
  TypeDeclTTESTSETUP1CLASSCHILD_114.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_115); // $47, $08, $00, $00

  TypePtr_115 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_115.Tag := DW_TAG_pointer_type;
  TypePtr_115.Children := 0;
  TypePtr_115.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1CLASSCHILD_116); // $4C, $08, $00, $00

  TypeTTESTSETUP1CLASSCHILD_116 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTTESTSETUP1CLASSCHILD_116.Tag := DW_TAG_structure_type;
  TypeTTESTSETUP1CLASSCHILD_116.Children := 1;
  TypeTTESTSETUP1CLASSCHILD_116.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1CLASSCHILD'+#0);
  TypeTTESTSETUP1CLASSCHILD_116.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 56);

    XX_117 := TypeTTESTSETUP1CLASSCHILD_116.GetNewChild;
    XX_117.Tag := DW_TAG_inheritance;
    XX_117.Children := 0;
    XX_117.Add(DW_AT_accessibility, DW_FORM_data1, [$01]);
    XX_117.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    XX_117.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1CLASS_90); // $EE, $06, $00, $00

    VarFINT64_118 := TypeTTESTSETUP1CLASSCHILD_116.GetNewChild;
    VarFINT64_118.Tag := DW_TAG_member;
    VarFINT64_118.Children := 0;
    VarFINT64_118.Add(DW_AT_name, DW_FORM_string, 'FINT64'+#0);
    VarFINT64_118.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(40)])); // $23, $28
    VarFINT64_118.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclINT64_266); // $A6, $10, $00, $00

    VarFQWORD_119 := TypeTTESTSETUP1CLASSCHILD_116.GetNewChild;
    VarFQWORD_119.Tag := DW_TAG_member;
    VarFQWORD_119.Children := 0;
    VarFQWORD_119.Add(DW_AT_name, DW_FORM_string, 'FQWORD'+#0);
    VarFQWORD_119.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(48)])); // $23, $30
    VarFQWORD_119.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_79); // $8A, $06, $00, $00

  Type_120 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_120.Tag := DW_TAG_reference_type;
  Type_120.Children := 0;
  Type_120.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCHILD_114); // $2C, $08, $00, $00

  TypeDeclPTESTSETUP1CLASSCHILD_121 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPTESTSETUP1CLASSCHILD_121.Tag := DW_TAG_typedef;
  TypeDeclPTESTSETUP1CLASSCHILD_121.Children := 0;
  TypeDeclPTESTSETUP1CLASSCHILD_121.Add(DW_AT_name, DW_FORM_string, 'PTESTSETUP1CLASSCHILD'+#0);
  TypeDeclPTESTSETUP1CLASSCHILD_121.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_122); // $AC, $08, $00, $00

  TypePtr_122 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_122.Tag := DW_TAG_pointer_type;
  TypePtr_122.Children := 0;
  TypePtr_122.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCHILD_114); // $2C, $08, $00, $00

  Type_123 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_123.Tag := DW_TAG_reference_type;
  Type_123.Children := 0;
  Type_123.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1CLASSCHILD_121); // $91, $08, $00, $00

  TypeDeclTTESTSETUP1CLASSCLASS_124 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTTESTSETUP1CLASSCLASS_124.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1CLASSCLASS_124.Children := 0;
  TypeDeclTTESTSETUP1CLASSCLASS_124.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1CLASSCLASS'+#0);
  TypeDeclTTESTSETUP1CLASSCLASS_124.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_125); // $D1, $08, $00, $00

  TypePtr_125 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_125.Tag := DW_TAG_pointer_type;
  TypePtr_125.Children := 0;
  TypePtr_125.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_126 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_126.Tag := DW_TAG_reference_type;
  Type_126.Children := 0;
  Type_126.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCLASS_124); // $B6, $08, $00, $00

  TypeDeclTTESTSETUP1CLASSCHILDCLASS_127 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTTESTSETUP1CLASSCHILDCLASS_127.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1CLASSCHILDCLASS_127.Children := 0;
  TypeDeclTTESTSETUP1CLASSCHILDCLASS_127.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1CLASSCHILDCLASS'+#0);
  TypeDeclTTESTSETUP1CLASSCHILDCLASS_127.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_128); // $FB, $08, $00, $00

  TypePtr_128 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_128.Tag := DW_TAG_pointer_type;
  TypePtr_128.Children := 0;
  TypePtr_128.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_129 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_129.Tag := DW_TAG_reference_type;
  Type_129.Children := 0;
  Type_129.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCHILDCLASS_127); // $DB, $08, $00, $00

  TypeDeclTTESTSETUP1OBJECT_130 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTTESTSETUP1OBJECT_130.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1OBJECT_130.Children := 0;
  TypeDeclTTESTSETUP1OBJECT_130.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1OBJECT'+#0);
  TypeDeclTTESTSETUP1OBJECT_130.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1OBJECT_131); // $1C, $09, $00, $00

  TypeTTESTSETUP1OBJECT_131 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTTESTSETUP1OBJECT_131.Tag := DW_TAG_structure_type;
  TypeTTESTSETUP1OBJECT_131.Children := 1;
  TypeTTESTSETUP1OBJECT_131.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1OBJECT'+#0);
  TypeTTESTSETUP1OBJECT_131.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 56);

    Var_vptrTTESTSETUP1OBJECT_132 := TypeTTESTSETUP1OBJECT_131.GetNewChild;
    Var_vptrTTESTSETUP1OBJECT_132.Tag := DW_TAG_member;
    Var_vptrTTESTSETUP1OBJECT_132.Children := 0;
    Var_vptrTTESTSETUP1OBJECT_132.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
    Var_vptrTTESTSETUP1OBJECT_132.Add(DW_AT_name, DW_FORM_string, '_vptr$TTESTSETUP1OBJECT'+#0);
    Var_vptrTTESTSETUP1OBJECT_132.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(48)])); // $23, $30
    Var_vptrTTESTSETUP1OBJECT_132.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_85); // $C0, $06, $00, $00

    VarFWORD_133 := TypeTTESTSETUP1OBJECT_131.GetNewChild;
    VarFWORD_133.Tag := DW_TAG_member;
    VarFWORD_133.Children := 0;
    VarFWORD_133.Add(DW_AT_name, DW_FORM_string, 'FWORD'+#0);
    VarFWORD_133.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarFWORD_133.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_73); // $56, $06, $00, $00

    VarFWORDL_134 := TypeTTESTSETUP1OBJECT_131.GetNewChild;
    VarFWORDL_134.Tag := DW_TAG_member;
    VarFWORDL_134.Children := 0;
    VarFWORDL_134.Add(DW_AT_name, DW_FORM_string, 'FWORDL'+#0);
    VarFWORDL_134.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(8)])); // $23, $08
    VarFWORDL_134.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_79); // $8A, $06, $00, $00

    VarFINT_135 := TypeTTESTSETUP1OBJECT_131.GetNewChild;
    VarFINT_135.Tag := DW_TAG_member;
    VarFINT_135.Children := 0;
    VarFINT_135.Add(DW_AT_name, DW_FORM_string, 'FINT'+#0);
    VarFINT_135.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(16)])); // $23, $10
    VarFINT_135.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_70); // $37, $06, $00, $00

    VarFINTL_136 := TypeTTESTSETUP1OBJECT_131.GetNewChild;
    VarFINTL_136.Tag := DW_TAG_member;
    VarFINTL_136.Children := 0;
    VarFINTL_136.Add(DW_AT_name, DW_FORM_string, 'FINTL'+#0);
    VarFINTL_136.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(24)])); // $23, $18
    VarFINTL_136.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclINT64_266); // $A6, $10, $00, $00

    VarFBOOL_137 := TypeTTESTSETUP1OBJECT_131.GetNewChild;
    VarFBOOL_137.Tag := DW_TAG_member;
    VarFBOOL_137.Children := 0;
    VarFBOOL_137.Add(DW_AT_name, DW_FORM_string, 'FBOOL'+#0);
    VarFBOOL_137.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(32)])); // $23, $20
    VarFBOOL_137.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_82); // $A3, $06, $00, $00

    VarFBOOL2_138 := TypeTTESTSETUP1OBJECT_131.GetNewChild;
    VarFBOOL2_138.Tag := DW_TAG_member;
    VarFBOOL2_138.Children := 0;
    VarFBOOL2_138.Add(DW_AT_name, DW_FORM_string, 'FBOOL2'+#0);
    VarFBOOL2_138.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(36)])); // $23, $24
    VarFBOOL2_138.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGBOOL_272); // $DC, $10, $00, $00

    VarFBOOL3_139 := TypeTTESTSETUP1OBJECT_131.GetNewChild;
    VarFBOOL3_139.Tag := DW_TAG_member;
    VarFBOOL3_139.Children := 0;
    VarFBOOL3_139.Add(DW_AT_name, DW_FORM_string, 'FBOOL3'+#0);
    VarFBOOL3_139.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(40)])); // $23, $28
    VarFBOOL3_139.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTEBOOL_275); // $FB, $10, $00, $00

    VarFTEST_140 := TypeTTESTSETUP1OBJECT_131.GetNewChild;
    VarFTEST_140.Tag := DW_TAG_member;
    VarFTEST_140.Children := 0;
    VarFTEST_140.Add(DW_AT_name, DW_FORM_string, 'FTEST'+#0);
    VarFTEST_140.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(44)])); // $23, $2C
    VarFTEST_140.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_88); // $D3, $06, $00, $00

    ProgF0_141 := TypeTTESTSETUP1OBJECT_131.GetNewChild;
    ProgF0_141.Tag := DW_TAG_subprogram;
    ProgF0_141.Children := 1;
    ProgF0_141.Add(DW_AT_name, DW_FORM_string, 'F0'+#0);
    ProgF0_141.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgF0_141.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgF0_141.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgF0_141.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgF0_141.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $03]));
    ProgF0_141.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00000000);
    ProgF0_141.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00000000);

      Varthis_142 := ProgF0_141.GetNewChild;
      Varthis_142.Tag := DW_TAG_formal_parameter;
      Varthis_142.Children := 0;
      Varthis_142.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_142.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-8), DW_OP_deref])); // $75, $78, $06
      Varthis_142.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_142.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_130); // $05, $09, $00, $00

      VarA_143 := ProgF0_141.GetNewChild;
      VarA_143.Tag := DW_TAG_formal_parameter;
      VarA_143.Children := 0;
      VarA_143.Add(DW_AT_name, DW_FORM_string, 'A'+#0);
      VarA_143.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-4)])); // $75, $7C
      VarA_143.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_76); // $6D, $06, $00, $00

  Type_144 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_144.Tag := DW_TAG_reference_type;
  Type_144.Children := 0;
  Type_144.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_130); // $05, $09, $00, $00

  TypeDeclPTESTSETUP1OBJECT_145 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPTESTSETUP1OBJECT_145.Tag := DW_TAG_typedef;
  TypeDeclPTESTSETUP1OBJECT_145.Children := 0;
  TypeDeclPTESTSETUP1OBJECT_145.Add(DW_AT_name, DW_FORM_string, 'PTESTSETUP1OBJECT'+#0);
  TypeDeclPTESTSETUP1OBJECT_145.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_146); // $0D, $0A, $00, $00

  TypePtr_146 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_146.Tag := DW_TAG_pointer_type;
  TypePtr_146.Children := 0;
  TypePtr_146.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_130); // $05, $09, $00, $00

  Type_147 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_147.Tag := DW_TAG_reference_type;
  Type_147.Children := 0;
  Type_147.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1OBJECT_145); // $F6, $09, $00, $00

  TypeDeclPINT_148 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPINT_148.Tag := DW_TAG_typedef;
  TypeDeclPINT_148.Children := 0;
  TypeDeclPINT_148.Add(DW_AT_name, DW_FORM_string, 'PINT'+#0);
  TypeDeclPINT_148.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_149); // $21, $0A, $00, $00

  TypePtr_149 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_149.Tag := DW_TAG_pointer_type;
  TypePtr_149.Children := 0;
  TypePtr_149.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_76); // $6D, $06, $00, $00

  Type_150 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_150.Tag := DW_TAG_reference_type;
  Type_150.Children := 0;
  Type_150.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINT_148); // $17, $0A, $00, $00

  TypeDeclPPINT_151 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPPINT_151.Tag := DW_TAG_typedef;
  TypeDeclPPINT_151.Children := 0;
  TypeDeclPPINT_151.Add(DW_AT_name, DW_FORM_string, 'PPINT'+#0);
  TypeDeclPPINT_151.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_152); // $36, $0A, $00, $00

  TypePtr_152 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_152.Tag := DW_TAG_pointer_type;
  TypePtr_152.Children := 0;
  TypePtr_152.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINT_148); // $17, $0A, $00, $00

  Type_153 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_153.Tag := DW_TAG_reference_type;
  Type_153.Children := 0;
  Type_153.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPPINT_151); // $2B, $0A, $00, $00

  TypeDeclPPPINT_154 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPPPINT_154.Tag := DW_TAG_typedef;
  TypeDeclPPPINT_154.Children := 0;
  TypeDeclPPPINT_154.Add(DW_AT_name, DW_FORM_string, 'PPPINT'+#0);
  TypeDeclPPPINT_154.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_155); // $4C, $0A, $00, $00

  TypePtr_155 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_155.Tag := DW_TAG_pointer_type;
  TypePtr_155.Children := 0;
  TypePtr_155.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPPINT_151); // $2B, $0A, $00, $00

  Type_156 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_156.Tag := DW_TAG_reference_type;
  Type_156.Children := 0;
  Type_156.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPPPINT_154); // $40, $0A, $00, $00

  TypeDeclPQWORD_157 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPQWORD_157.Tag := DW_TAG_typedef;
  TypeDeclPQWORD_157.Children := 0;
  TypeDeclPQWORD_157.Add(DW_AT_name, DW_FORM_string, 'PQWORD'+#0);
  TypeDeclPQWORD_157.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_158); // $62, $0A, $00, $00

  TypePtr_158 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_158.Tag := DW_TAG_pointer_type;
  TypePtr_158.Children := 0;
  TypePtr_158.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_79); // $8A, $06, $00, $00

  Type_159 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_159.Tag := DW_TAG_reference_type;
  Type_159.Children := 0;
  Type_159.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPQWORD_157); // $56, $0A, $00, $00

  TypeDeclTOBJECT_160 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTOBJECT_160.Tag := DW_TAG_typedef;
  TypeDeclTOBJECT_160.Children := 0;
  TypeDeclTOBJECT_160.Add(DW_AT_name, DW_FORM_string, 'TOBJECT'+#0);
  TypeDeclTOBJECT_160.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_161); // $79, $0A, $00, $00

  TypePtr_161 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_161.Tag := DW_TAG_pointer_type;
  TypePtr_161.Children := 0;
  TypePtr_161.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTOBJECT_162); // $7E, $0A, $00, $00

  TypeTOBJECT_162 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTOBJECT_162.Tag := DW_TAG_structure_type;
  TypeTOBJECT_162.Children := 1;
  TypeTOBJECT_162.Add(DW_AT_name, DW_FORM_string, 'TOBJECT'+#0);
  TypeTOBJECT_162.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 4);

    Var_vptrTOBJECT_163 := TypeTOBJECT_162.GetNewChild;
    Var_vptrTOBJECT_163.Tag := DW_TAG_member;
    Var_vptrTOBJECT_163.Children := 0;
    Var_vptrTOBJECT_163.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
    Var_vptrTOBJECT_163.Add(DW_AT_name, DW_FORM_string, '_vptr$TOBJECT'+#0);
    Var_vptrTOBJECT_163.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    Var_vptrTOBJECT_163.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_85); // $C0, $06, $00, $00

    ProgCREATE_164 := TypeTOBJECT_162.GetNewChild;
    ProgCREATE_164.Tag := DW_TAG_subprogram;
    ProgCREATE_164.Children := 1;
    ProgCREATE_164.Add(DW_AT_name, DW_FORM_string, 'CREATE'+#0);
    ProgCREATE_164.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCREATE_164.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCREATE_164.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCREATE_164.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

      Varthis_165 := ProgCREATE_164.GetNewChild;
      Varthis_165.Tag := DW_TAG_formal_parameter;
      Varthis_165.Children := 0;
      Varthis_165.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_165.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_165.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

      Varvmt_166 := ProgCREATE_164.GetNewChild;
      Varvmt_166.Tag := DW_TAG_formal_parameter;
      Varvmt_166.Children := 0;
      Varvmt_166.Add(DW_AT_name, DW_FORM_string, 'vmt'+#0);
      Varvmt_166.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_85); // $C0, $06, $00, $00

    ProgDESTROY_167 := TypeTOBJECT_162.GetNewChild;
    ProgDESTROY_167.Tag := DW_TAG_subprogram;
    ProgDESTROY_167.Children := 1;
    ProgDESTROY_167.Add(DW_AT_name, DW_FORM_string, 'DESTROY'+#0);
    ProgDESTROY_167.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgDESTROY_167.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgDESTROY_167.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgDESTROY_167.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgDESTROY_167.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $0C]));

      Varthis_168 := ProgDESTROY_167.GetNewChild;
      Varthis_168.Tag := DW_TAG_formal_parameter;
      Varthis_168.Children := 0;
      Varthis_168.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_168.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_168.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

      Varvmt_169 := ProgDESTROY_167.GetNewChild;
      Varvmt_169.Tag := DW_TAG_formal_parameter;
      Varvmt_169.Children := 0;
      Varvmt_169.Add(DW_AT_name, DW_FORM_string, 'vmt'+#0);
      Varvmt_169.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_85); // $C0, $06, $00, $00

    ProgNEWINSTANCE_170 := TypeTOBJECT_162.GetNewChild;
    ProgNEWINSTANCE_170.Tag := DW_TAG_subprogram;
    ProgNEWINSTANCE_170.Children := 1;
    ProgNEWINSTANCE_170.Add(DW_AT_name, DW_FORM_string, 'NEWINSTANCE'+#0);
    ProgNEWINSTANCE_170.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgNEWINSTANCE_170.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgNEWINSTANCE_170.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgNEWINSTANCE_170.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgNEWINSTANCE_170.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $0D]));
    ProgNEWINSTANCE_170.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

      Varself_171 := ProgNEWINSTANCE_170.GetNewChild;
      Varself_171.Tag := DW_TAG_formal_parameter;
      Varself_171.Children := 0;
      Varself_171.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_171.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_171.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_278); // $1A, $11, $00, $00

    ProgFREEINSTANCE_172 := TypeTOBJECT_162.GetNewChild;
    ProgFREEINSTANCE_172.Tag := DW_TAG_subprogram;
    ProgFREEINSTANCE_172.Children := 1;
    ProgFREEINSTANCE_172.Add(DW_AT_name, DW_FORM_string, 'FREEINSTANCE'+#0);
    ProgFREEINSTANCE_172.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgFREEINSTANCE_172.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgFREEINSTANCE_172.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgFREEINSTANCE_172.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgFREEINSTANCE_172.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $0E]));

      Varthis_173 := ProgFREEINSTANCE_172.GetNewChild;
      Varthis_173.Tag := DW_TAG_formal_parameter;
      Varthis_173.Children := 0;
      Varthis_173.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_173.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_173.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

    ProgSAFECALLEXCEPTION_174 := TypeTOBJECT_162.GetNewChild;
    ProgSAFECALLEXCEPTION_174.Tag := DW_TAG_subprogram;
    ProgSAFECALLEXCEPTION_174.Children := 1;
    ProgSAFECALLEXCEPTION_174.Add(DW_AT_name, DW_FORM_string, 'SAFECALLEXCEPTION'+#0);
    ProgSAFECALLEXCEPTION_174.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgSAFECALLEXCEPTION_174.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgSAFECALLEXCEPTION_174.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgSAFECALLEXCEPTION_174.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgSAFECALLEXCEPTION_174.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $0F]));
    ProgSAFECALLEXCEPTION_174.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclHRESULT_280); // $24, $11, $00, $00

      Varthis_175 := ProgSAFECALLEXCEPTION_174.GetNewChild;
      Varthis_175.Tag := DW_TAG_formal_parameter;
      Varthis_175.Children := 0;
      Varthis_175.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_175.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_175.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

      VarEXCEPTOBJECT_176 := ProgSAFECALLEXCEPTION_174.GetNewChild;
      VarEXCEPTOBJECT_176.Tag := DW_TAG_formal_parameter;
      VarEXCEPTOBJECT_176.Children := 0;
      VarEXCEPTOBJECT_176.Add(DW_AT_name, DW_FORM_string, 'EXCEPTOBJECT'+#0);
      VarEXCEPTOBJECT_176.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

      VarEXCEPTADDR_177 := ProgSAFECALLEXCEPTION_174.GetNewChild;
      VarEXCEPTADDR_177.Tag := DW_TAG_formal_parameter;
      VarEXCEPTADDR_177.Children := 0;
      VarEXCEPTADDR_177.Add(DW_AT_name, DW_FORM_string, 'EXCEPTADDR'+#0);
      VarEXCEPTADDR_177.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_85); // $C0, $06, $00, $00

    ProgDEFAULTHANDLER_178 := TypeTOBJECT_162.GetNewChild;
    ProgDEFAULTHANDLER_178.Tag := DW_TAG_subprogram;
    ProgDEFAULTHANDLER_178.Children := 1;
    ProgDEFAULTHANDLER_178.Add(DW_AT_name, DW_FORM_string, 'DEFAULTHANDLER'+#0);
    ProgDEFAULTHANDLER_178.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgDEFAULTHANDLER_178.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgDEFAULTHANDLER_178.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgDEFAULTHANDLER_178.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgDEFAULTHANDLER_178.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $10]));

      Varthis_179 := ProgDEFAULTHANDLER_178.GetNewChild;
      Varthis_179.Tag := DW_TAG_formal_parameter;
      Varthis_179.Children := 0;
      Varthis_179.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_179.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_179.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

      VarMESSAGE_180 := ProgDEFAULTHANDLER_178.GetNewChild;
      VarMESSAGE_180.Tag := DW_TAG_formal_parameter;
      VarMESSAGE_180.Children := 0;
      VarMESSAGE_180.Add(DW_AT_name, DW_FORM_string, 'MESSAGE'+#0);
      VarMESSAGE_180.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_283); // $41, $11, $00, $00

    ProgFREE_181 := TypeTOBJECT_162.GetNewChild;
    ProgFREE_181.Tag := DW_TAG_subprogram;
    ProgFREE_181.Children := 1;
    ProgFREE_181.Add(DW_AT_name, DW_FORM_string, 'FREE'+#0);
    ProgFREE_181.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgFREE_181.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgFREE_181.Add(DW_AT_external, DW_FORM_flag, [$01]);

      Varthis_182 := ProgFREE_181.GetNewChild;
      Varthis_182.Tag := DW_TAG_formal_parameter;
      Varthis_182.Children := 0;
      Varthis_182.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_182.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_182.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

    ProgINITINSTANCE_183 := TypeTOBJECT_162.GetNewChild;
    ProgINITINSTANCE_183.Tag := DW_TAG_subprogram;
    ProgINITINSTANCE_183.Children := 1;
    ProgINITINSTANCE_183.Add(DW_AT_name, DW_FORM_string, 'INITINSTANCE'+#0);
    ProgINITINSTANCE_183.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgINITINSTANCE_183.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgINITINSTANCE_183.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgINITINSTANCE_183.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

      Varself_184 := ProgINITINSTANCE_183.GetNewChild;
      Varself_184.Tag := DW_TAG_formal_parameter;
      Varself_184.Children := 0;
      Varself_184.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_184.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_184.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_286); // $5F, $11, $00, $00

      VarINSTANCE_185 := ProgINITINSTANCE_183.GetNewChild;
      VarINSTANCE_185.Tag := DW_TAG_formal_parameter;
      VarINSTANCE_185.Children := 0;
      VarINSTANCE_185.Add(DW_AT_name, DW_FORM_string, 'INSTANCE'+#0);
      VarINSTANCE_185.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_85); // $C0, $06, $00, $00

    ProgCLEANUPINSTANCE_186 := TypeTOBJECT_162.GetNewChild;
    ProgCLEANUPINSTANCE_186.Tag := DW_TAG_subprogram;
    ProgCLEANUPINSTANCE_186.Children := 1;
    ProgCLEANUPINSTANCE_186.Add(DW_AT_name, DW_FORM_string, 'CLEANUPINSTANCE'+#0);
    ProgCLEANUPINSTANCE_186.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLEANUPINSTANCE_186.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLEANUPINSTANCE_186.Add(DW_AT_external, DW_FORM_flag, [$01]);

      Varthis_187 := ProgCLEANUPINSTANCE_186.GetNewChild;
      Varthis_187.Tag := DW_TAG_formal_parameter;
      Varthis_187.Children := 0;
      Varthis_187.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_187.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_187.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

    ProgCLASSTYPE_188 := TypeTOBJECT_162.GetNewChild;
    ProgCLASSTYPE_188.Tag := DW_TAG_subprogram;
    ProgCLASSTYPE_188.Children := 1;
    ProgCLASSTYPE_188.Add(DW_AT_name, DW_FORM_string, 'CLASSTYPE'+#0);
    ProgCLASSTYPE_188.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLASSTYPE_188.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLASSTYPE_188.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCLASSTYPE_188.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTCLASS_288); // $69, $11, $00, $00

      Varself_189 := ProgCLASSTYPE_188.GetNewChild;
      Varself_189.Tag := DW_TAG_formal_parameter;
      Varself_189.Children := 0;
      Varself_189.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_189.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_189.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_291); // $7F, $11, $00, $00

    ProgCLASSINFO_190 := TypeTOBJECT_162.GetNewChild;
    ProgCLASSINFO_190.Tag := DW_TAG_subprogram;
    ProgCLASSINFO_190.Children := 1;
    ProgCLASSINFO_190.Add(DW_AT_name, DW_FORM_string, 'CLASSINFO'+#0);
    ProgCLASSINFO_190.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLASSINFO_190.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLASSINFO_190.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCLASSINFO_190.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_85); // $C0, $06, $00, $00

      Varself_191 := ProgCLASSINFO_190.GetNewChild;
      Varself_191.Tag := DW_TAG_formal_parameter;
      Varself_191.Children := 0;
      Varself_191.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_191.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_191.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_293); // $89, $11, $00, $00

    ProgCLASSNAME_192 := TypeTOBJECT_162.GetNewChild;
    ProgCLASSNAME_192.Tag := DW_TAG_subprogram;
    ProgCLASSNAME_192.Children := 1;
    ProgCLASSNAME_192.Add(DW_AT_name, DW_FORM_string, 'CLASSNAME'+#0);
    ProgCLASSNAME_192.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLASSNAME_192.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLASSNAME_192.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCLASSNAME_192.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_295); // $93, $11, $00, $00

      Varself_193 := ProgCLASSNAME_192.GetNewChild;
      Varself_193.Tag := DW_TAG_formal_parameter;
      Varself_193.Children := 0;
      Varself_193.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_193.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_193.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_302); // $E4, $11, $00, $00

      Varresult_194 := ProgCLASSNAME_192.GetNewChild;
      Varresult_194.Tag := DW_TAG_variable;
      Varresult_194.Children := 0;
      Varresult_194.Add(DW_AT_name, DW_FORM_string, 'result'+#0);
      Varresult_194.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_295); // $93, $11, $00, $00

    ProgCLASSNAMEIS_195 := TypeTOBJECT_162.GetNewChild;
    ProgCLASSNAMEIS_195.Tag := DW_TAG_subprogram;
    ProgCLASSNAMEIS_195.Children := 1;
    ProgCLASSNAMEIS_195.Add(DW_AT_name, DW_FORM_string, 'CLASSNAMEIS'+#0);
    ProgCLASSNAMEIS_195.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLASSNAMEIS_195.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLASSNAMEIS_195.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCLASSNAMEIS_195.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_82); // $A3, $06, $00, $00

      Varself_196 := ProgCLASSNAMEIS_195.GetNewChild;
      Varself_196.Tag := DW_TAG_formal_parameter;
      Varself_196.Children := 0;
      Varself_196.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_196.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_196.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_304); // $EE, $11, $00, $00

      VarNAME_197 := ProgCLASSNAMEIS_195.GetNewChild;
      VarNAME_197.Tag := DW_TAG_formal_parameter;
      VarNAME_197.Children := 0;
      VarNAME_197.Add(DW_AT_name, DW_FORM_string, 'NAME'+#0);
      VarNAME_197.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_295); // $93, $11, $00, $00

    ProgCLASSPARENT_198 := TypeTOBJECT_162.GetNewChild;
    ProgCLASSPARENT_198.Tag := DW_TAG_subprogram;
    ProgCLASSPARENT_198.Children := 1;
    ProgCLASSPARENT_198.Add(DW_AT_name, DW_FORM_string, 'CLASSPARENT'+#0);
    ProgCLASSPARENT_198.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLASSPARENT_198.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLASSPARENT_198.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCLASSPARENT_198.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTCLASS_288); // $69, $11, $00, $00

      Varself_199 := ProgCLASSPARENT_198.GetNewChild;
      Varself_199.Tag := DW_TAG_formal_parameter;
      Varself_199.Children := 0;
      Varself_199.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_199.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_199.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_306); // $F8, $11, $00, $00

    ProgINSTANCESIZE_200 := TypeTOBJECT_162.GetNewChild;
    ProgINSTANCESIZE_200.Tag := DW_TAG_subprogram;
    ProgINSTANCESIZE_200.Children := 1;
    ProgINSTANCESIZE_200.Add(DW_AT_name, DW_FORM_string, 'INSTANCESIZE'+#0);
    ProgINSTANCESIZE_200.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgINSTANCESIZE_200.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgINSTANCESIZE_200.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgINSTANCESIZE_200.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_76); // $6D, $06, $00, $00

      Varself_201 := ProgINSTANCESIZE_200.GetNewChild;
      Varself_201.Tag := DW_TAG_formal_parameter;
      Varself_201.Children := 0;
      Varself_201.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_201.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_201.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_308); // $02, $12, $00, $00

    ProgINHERITSFROM_202 := TypeTOBJECT_162.GetNewChild;
    ProgINHERITSFROM_202.Tag := DW_TAG_subprogram;
    ProgINHERITSFROM_202.Children := 1;
    ProgINHERITSFROM_202.Add(DW_AT_name, DW_FORM_string, 'INHERITSFROM'+#0);
    ProgINHERITSFROM_202.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgINHERITSFROM_202.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgINHERITSFROM_202.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgINHERITSFROM_202.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_82); // $A3, $06, $00, $00

      Varself_203 := ProgINHERITSFROM_202.GetNewChild;
      Varself_203.Tag := DW_TAG_formal_parameter;
      Varself_203.Children := 0;
      Varself_203.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_203.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_203.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_310); // $0C, $12, $00, $00

      VarACLASS_204 := ProgINHERITSFROM_202.GetNewChild;
      VarACLASS_204.Tag := DW_TAG_formal_parameter;
      VarACLASS_204.Children := 0;
      VarACLASS_204.Add(DW_AT_name, DW_FORM_string, 'ACLASS'+#0);
      VarACLASS_204.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTCLASS_288); // $69, $11, $00, $00

    ProgSTRINGMESSAGETABLE_205 := TypeTOBJECT_162.GetNewChild;
    ProgSTRINGMESSAGETABLE_205.Tag := DW_TAG_subprogram;
    ProgSTRINGMESSAGETABLE_205.Children := 1;
    ProgSTRINGMESSAGETABLE_205.Add(DW_AT_name, DW_FORM_string, 'STRINGMESSAGETABLE'+#0);
    ProgSTRINGMESSAGETABLE_205.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgSTRINGMESSAGETABLE_205.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgSTRINGMESSAGETABLE_205.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgSTRINGMESSAGETABLE_205.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSTRINGMESSAGETABLE_312); // $16, $12, $00, $00

      Varself_206 := ProgSTRINGMESSAGETABLE_205.GetNewChild;
      Varself_206.Tag := DW_TAG_formal_parameter;
      Varself_206.Children := 0;
      Varself_206.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_206.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_206.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_315); // $39, $12, $00, $00

    ProgMETHODADDRESS_207 := TypeTOBJECT_162.GetNewChild;
    ProgMETHODADDRESS_207.Tag := DW_TAG_subprogram;
    ProgMETHODADDRESS_207.Children := 1;
    ProgMETHODADDRESS_207.Add(DW_AT_name, DW_FORM_string, 'METHODADDRESS'+#0);
    ProgMETHODADDRESS_207.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgMETHODADDRESS_207.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgMETHODADDRESS_207.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgMETHODADDRESS_207.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_85); // $C0, $06, $00, $00

      Varself_208 := ProgMETHODADDRESS_207.GetNewChild;
      Varself_208.Tag := DW_TAG_formal_parameter;
      Varself_208.Children := 0;
      Varself_208.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_208.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_208.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_317); // $43, $12, $00, $00

      VarNAME_209 := ProgMETHODADDRESS_207.GetNewChild;
      VarNAME_209.Tag := DW_TAG_formal_parameter;
      VarNAME_209.Children := 0;
      VarNAME_209.Add(DW_AT_name, DW_FORM_string, 'NAME'+#0);
      VarNAME_209.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_295); // $93, $11, $00, $00

    ProgMETHODNAME_210 := TypeTOBJECT_162.GetNewChild;
    ProgMETHODNAME_210.Tag := DW_TAG_subprogram;
    ProgMETHODNAME_210.Children := 1;
    ProgMETHODNAME_210.Add(DW_AT_name, DW_FORM_string, 'METHODNAME'+#0);
    ProgMETHODNAME_210.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgMETHODNAME_210.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgMETHODNAME_210.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgMETHODNAME_210.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_295); // $93, $11, $00, $00

      Varself_211 := ProgMETHODNAME_210.GetNewChild;
      Varself_211.Tag := DW_TAG_formal_parameter;
      Varself_211.Children := 0;
      Varself_211.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_211.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_211.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_319); // $4D, $12, $00, $00

      VarADDRESS_212 := ProgMETHODNAME_210.GetNewChild;
      VarADDRESS_212.Tag := DW_TAG_formal_parameter;
      VarADDRESS_212.Children := 0;
      VarADDRESS_212.Add(DW_AT_name, DW_FORM_string, 'ADDRESS'+#0);
      VarADDRESS_212.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_85); // $C0, $06, $00, $00

      Varresult_213 := ProgMETHODNAME_210.GetNewChild;
      Varresult_213.Tag := DW_TAG_variable;
      Varresult_213.Children := 0;
      Varresult_213.Add(DW_AT_name, DW_FORM_string, 'result'+#0);
      Varresult_213.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_295); // $93, $11, $00, $00

    ProgFIELDADDRESS_214 := TypeTOBJECT_162.GetNewChild;
    ProgFIELDADDRESS_214.Tag := DW_TAG_subprogram;
    ProgFIELDADDRESS_214.Children := 1;
    ProgFIELDADDRESS_214.Add(DW_AT_name, DW_FORM_string, 'FIELDADDRESS'+#0);
    ProgFIELDADDRESS_214.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgFIELDADDRESS_214.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgFIELDADDRESS_214.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgFIELDADDRESS_214.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_85); // $C0, $06, $00, $00

      Varthis_215 := ProgFIELDADDRESS_214.GetNewChild;
      Varthis_215.Tag := DW_TAG_formal_parameter;
      Varthis_215.Children := 0;
      Varthis_215.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_215.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_215.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

      VarNAME_216 := ProgFIELDADDRESS_214.GetNewChild;
      VarNAME_216.Tag := DW_TAG_formal_parameter;
      VarNAME_216.Children := 0;
      VarNAME_216.Add(DW_AT_name, DW_FORM_string, 'NAME'+#0);
      VarNAME_216.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_295); // $93, $11, $00, $00

    ProgAFTERCONSTRUCTION_217 := TypeTOBJECT_162.GetNewChild;
    ProgAFTERCONSTRUCTION_217.Tag := DW_TAG_subprogram;
    ProgAFTERCONSTRUCTION_217.Children := 1;
    ProgAFTERCONSTRUCTION_217.Add(DW_AT_name, DW_FORM_string, 'AFTERCONSTRUCTION'+#0);
    ProgAFTERCONSTRUCTION_217.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgAFTERCONSTRUCTION_217.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgAFTERCONSTRUCTION_217.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgAFTERCONSTRUCTION_217.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgAFTERCONSTRUCTION_217.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $11]));

      Varthis_218 := ProgAFTERCONSTRUCTION_217.GetNewChild;
      Varthis_218.Tag := DW_TAG_formal_parameter;
      Varthis_218.Children := 0;
      Varthis_218.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_218.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_218.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

    ProgBEFOREDESTRUCTION_219 := TypeTOBJECT_162.GetNewChild;
    ProgBEFOREDESTRUCTION_219.Tag := DW_TAG_subprogram;
    ProgBEFOREDESTRUCTION_219.Children := 1;
    ProgBEFOREDESTRUCTION_219.Add(DW_AT_name, DW_FORM_string, 'BEFOREDESTRUCTION'+#0);
    ProgBEFOREDESTRUCTION_219.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgBEFOREDESTRUCTION_219.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgBEFOREDESTRUCTION_219.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgBEFOREDESTRUCTION_219.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgBEFOREDESTRUCTION_219.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $12]));

      Varthis_220 := ProgBEFOREDESTRUCTION_219.GetNewChild;
      Varthis_220.Tag := DW_TAG_formal_parameter;
      Varthis_220.Children := 0;
      Varthis_220.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_220.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_220.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

    ProgDEFAULTHANDLERSTR_221 := TypeTOBJECT_162.GetNewChild;
    ProgDEFAULTHANDLERSTR_221.Tag := DW_TAG_subprogram;
    ProgDEFAULTHANDLERSTR_221.Children := 1;
    ProgDEFAULTHANDLERSTR_221.Add(DW_AT_name, DW_FORM_string, 'DEFAULTHANDLERSTR'+#0);
    ProgDEFAULTHANDLERSTR_221.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgDEFAULTHANDLERSTR_221.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgDEFAULTHANDLERSTR_221.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgDEFAULTHANDLERSTR_221.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgDEFAULTHANDLERSTR_221.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $13]));

      Varthis_222 := ProgDEFAULTHANDLERSTR_221.GetNewChild;
      Varthis_222.Tag := DW_TAG_formal_parameter;
      Varthis_222.Children := 0;
      Varthis_222.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_222.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_222.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

      VarMESSAGE_223 := ProgDEFAULTHANDLERSTR_221.GetNewChild;
      VarMESSAGE_223.Tag := DW_TAG_formal_parameter;
      VarMESSAGE_223.Children := 0;
      VarMESSAGE_223.Add(DW_AT_name, DW_FORM_string, 'MESSAGE'+#0);
      VarMESSAGE_223.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_283); // $41, $11, $00, $00

    ProgDISPATCH_224 := TypeTOBJECT_162.GetNewChild;
    ProgDISPATCH_224.Tag := DW_TAG_subprogram;
    ProgDISPATCH_224.Children := 1;
    ProgDISPATCH_224.Add(DW_AT_name, DW_FORM_string, 'DISPATCH'+#0);
    ProgDISPATCH_224.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgDISPATCH_224.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgDISPATCH_224.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgDISPATCH_224.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgDISPATCH_224.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $14]));

      Varthis_225 := ProgDISPATCH_224.GetNewChild;
      Varthis_225.Tag := DW_TAG_formal_parameter;
      Varthis_225.Children := 0;
      Varthis_225.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_225.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_225.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

      VarMESSAGE_226 := ProgDISPATCH_224.GetNewChild;
      VarMESSAGE_226.Tag := DW_TAG_formal_parameter;
      VarMESSAGE_226.Children := 0;
      VarMESSAGE_226.Add(DW_AT_name, DW_FORM_string, 'MESSAGE'+#0);
      VarMESSAGE_226.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_283); // $41, $11, $00, $00

    ProgDISPATCHSTR_227 := TypeTOBJECT_162.GetNewChild;
    ProgDISPATCHSTR_227.Tag := DW_TAG_subprogram;
    ProgDISPATCHSTR_227.Children := 1;
    ProgDISPATCHSTR_227.Add(DW_AT_name, DW_FORM_string, 'DISPATCHSTR'+#0);
    ProgDISPATCHSTR_227.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgDISPATCHSTR_227.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgDISPATCHSTR_227.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgDISPATCHSTR_227.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgDISPATCHSTR_227.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $15]));

      Varthis_228 := ProgDISPATCHSTR_227.GetNewChild;
      Varthis_228.Tag := DW_TAG_formal_parameter;
      Varthis_228.Children := 0;
      Varthis_228.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_228.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_228.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

      VarMESSAGE_229 := ProgDISPATCHSTR_227.GetNewChild;
      VarMESSAGE_229.Tag := DW_TAG_formal_parameter;
      VarMESSAGE_229.Children := 0;
      VarMESSAGE_229.Add(DW_AT_name, DW_FORM_string, 'MESSAGE'+#0);
      VarMESSAGE_229.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_283); // $41, $11, $00, $00

    ProgGETINTERFACE_230 := TypeTOBJECT_162.GetNewChild;
    ProgGETINTERFACE_230.Tag := DW_TAG_subprogram;
    ProgGETINTERFACE_230.Children := 1;
    ProgGETINTERFACE_230.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACE'+#0);
    ProgGETINTERFACE_230.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACE_230.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACE_230.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACE_230.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_82); // $A3, $06, $00, $00

      Varthis_231 := ProgGETINTERFACE_230.GetNewChild;
      Varthis_231.Tag := DW_TAG_formal_parameter;
      Varthis_231.Children := 0;
      Varthis_231.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_231.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_231.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

      VarIID_232 := ProgGETINTERFACE_230.GetNewChild;
      VarIID_232.Tag := DW_TAG_formal_parameter;
      VarIID_232.Children := 0;
      VarIID_232.Add(DW_AT_name, DW_FORM_string, 'IID'+#0);
      VarIID_232.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTGUID_321); // $57, $12, $00, $00

      VarOBJ_233 := ProgGETINTERFACE_230.GetNewChild;
      VarOBJ_233.Tag := DW_TAG_formal_parameter;
      VarOBJ_233.Children := 0;
      VarOBJ_233.Add(DW_AT_name, DW_FORM_string, 'OBJ'+#0);
      VarOBJ_233.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_283); // $41, $11, $00, $00

    ProgGETINTERFACE_234 := TypeTOBJECT_162.GetNewChild;
    ProgGETINTERFACE_234.Tag := DW_TAG_subprogram;
    ProgGETINTERFACE_234.Children := 1;
    ProgGETINTERFACE_234.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACE'+#0);
    ProgGETINTERFACE_234.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACE_234.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACE_234.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACE_234.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_82); // $A3, $06, $00, $00

      Varthis_235 := ProgGETINTERFACE_234.GetNewChild;
      Varthis_235.Tag := DW_TAG_formal_parameter;
      Varthis_235.Children := 0;
      Varthis_235.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_235.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_235.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

      VarIIDSTR_236 := ProgGETINTERFACE_234.GetNewChild;
      VarIIDSTR_236.Tag := DW_TAG_formal_parameter;
      VarIIDSTR_236.Children := 0;
      VarIIDSTR_236.Add(DW_AT_name, DW_FORM_string, 'IIDSTR'+#0);
      VarIIDSTR_236.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_295); // $93, $11, $00, $00

      VarOBJ_237 := ProgGETINTERFACE_234.GetNewChild;
      VarOBJ_237.Tag := DW_TAG_formal_parameter;
      VarOBJ_237.Children := 0;
      VarOBJ_237.Add(DW_AT_name, DW_FORM_string, 'OBJ'+#0);
      VarOBJ_237.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_283); // $41, $11, $00, $00

    ProgGETINTERFACEBYSTR_238 := TypeTOBJECT_162.GetNewChild;
    ProgGETINTERFACEBYSTR_238.Tag := DW_TAG_subprogram;
    ProgGETINTERFACEBYSTR_238.Children := 1;
    ProgGETINTERFACEBYSTR_238.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACEBYSTR'+#0);
    ProgGETINTERFACEBYSTR_238.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACEBYSTR_238.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACEBYSTR_238.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACEBYSTR_238.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_82); // $A3, $06, $00, $00

      Varthis_239 := ProgGETINTERFACEBYSTR_238.GetNewChild;
      Varthis_239.Tag := DW_TAG_formal_parameter;
      Varthis_239.Children := 0;
      Varthis_239.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_239.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_239.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

      VarIIDSTR_240 := ProgGETINTERFACEBYSTR_238.GetNewChild;
      VarIIDSTR_240.Tag := DW_TAG_formal_parameter;
      VarIIDSTR_240.Children := 0;
      VarIIDSTR_240.Add(DW_AT_name, DW_FORM_string, 'IIDSTR'+#0);
      VarIIDSTR_240.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_295); // $93, $11, $00, $00

      VarOBJ_241 := ProgGETINTERFACEBYSTR_238.GetNewChild;
      VarOBJ_241.Tag := DW_TAG_formal_parameter;
      VarOBJ_241.Children := 0;
      VarOBJ_241.Add(DW_AT_name, DW_FORM_string, 'OBJ'+#0);
      VarOBJ_241.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_283); // $41, $11, $00, $00

    ProgGETINTERFACEWEAK_242 := TypeTOBJECT_162.GetNewChild;
    ProgGETINTERFACEWEAK_242.Tag := DW_TAG_subprogram;
    ProgGETINTERFACEWEAK_242.Children := 1;
    ProgGETINTERFACEWEAK_242.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACEWEAK'+#0);
    ProgGETINTERFACEWEAK_242.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACEWEAK_242.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACEWEAK_242.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACEWEAK_242.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_82); // $A3, $06, $00, $00

      Varthis_243 := ProgGETINTERFACEWEAK_242.GetNewChild;
      Varthis_243.Tag := DW_TAG_formal_parameter;
      Varthis_243.Children := 0;
      Varthis_243.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_243.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_243.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

      VarIID_244 := ProgGETINTERFACEWEAK_242.GetNewChild;
      VarIID_244.Tag := DW_TAG_formal_parameter;
      VarIID_244.Children := 0;
      VarIID_244.Add(DW_AT_name, DW_FORM_string, 'IID'+#0);
      VarIID_244.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTGUID_321); // $57, $12, $00, $00

      VarOBJ_245 := ProgGETINTERFACEWEAK_242.GetNewChild;
      VarOBJ_245.Tag := DW_TAG_formal_parameter;
      VarOBJ_245.Children := 0;
      VarOBJ_245.Add(DW_AT_name, DW_FORM_string, 'OBJ'+#0);
      VarOBJ_245.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_283); // $41, $11, $00, $00

    ProgGETINTERFACEENTRY_246 := TypeTOBJECT_162.GetNewChild;
    ProgGETINTERFACEENTRY_246.Tag := DW_TAG_subprogram;
    ProgGETINTERFACEENTRY_246.Children := 1;
    ProgGETINTERFACEENTRY_246.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACEENTRY'+#0);
    ProgGETINTERFACEENTRY_246.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACEENTRY_246.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACEENTRY_246.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACEENTRY_246.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTERFACEENTRY_338); // $57, $13, $00, $00

      Varself_247 := ProgGETINTERFACEENTRY_246.GetNewChild;
      Varself_247.Tag := DW_TAG_formal_parameter;
      Varself_247.Children := 0;
      Varself_247.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_247.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_247.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_341); // $76, $13, $00, $00

      VarIID_248 := ProgGETINTERFACEENTRY_246.GetNewChild;
      VarIID_248.Tag := DW_TAG_formal_parameter;
      VarIID_248.Children := 0;
      VarIID_248.Add(DW_AT_name, DW_FORM_string, 'IID'+#0);
      VarIID_248.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTGUID_321); // $57, $12, $00, $00

    ProgGETINTERFACEENTRYBYSTR_249 := TypeTOBJECT_162.GetNewChild;
    ProgGETINTERFACEENTRYBYSTR_249.Tag := DW_TAG_subprogram;
    ProgGETINTERFACEENTRYBYSTR_249.Children := 1;
    ProgGETINTERFACEENTRYBYSTR_249.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACEENTRYBYSTR'+#0);
    ProgGETINTERFACEENTRYBYSTR_249.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACEENTRYBYSTR_249.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACEENTRYBYSTR_249.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACEENTRYBYSTR_249.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTERFACEENTRY_338); // $57, $13, $00, $00

      Varself_250 := ProgGETINTERFACEENTRYBYSTR_249.GetNewChild;
      Varself_250.Tag := DW_TAG_formal_parameter;
      Varself_250.Children := 0;
      Varself_250.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_250.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_250.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_343); // $80, $13, $00, $00

      VarIIDSTR_251 := ProgGETINTERFACEENTRYBYSTR_249.GetNewChild;
      VarIIDSTR_251.Tag := DW_TAG_formal_parameter;
      VarIIDSTR_251.Children := 0;
      VarIIDSTR_251.Add(DW_AT_name, DW_FORM_string, 'IIDSTR'+#0);
      VarIIDSTR_251.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_295); // $93, $11, $00, $00

    ProgGETINTERFACETABLE_252 := TypeTOBJECT_162.GetNewChild;
    ProgGETINTERFACETABLE_252.Tag := DW_TAG_subprogram;
    ProgGETINTERFACETABLE_252.Children := 1;
    ProgGETINTERFACETABLE_252.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACETABLE'+#0);
    ProgGETINTERFACETABLE_252.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACETABLE_252.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACETABLE_252.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACETABLE_252.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTERFACETABLE_345); // $8A, $13, $00, $00

      Varself_253 := ProgGETINTERFACETABLE_252.GetNewChild;
      Varself_253.Tag := DW_TAG_formal_parameter;
      Varself_253.Children := 0;
      Varself_253.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_253.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_253.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_348); // $A9, $13, $00, $00

    ProgUNITNAME_254 := TypeTOBJECT_162.GetNewChild;
    ProgUNITNAME_254.Tag := DW_TAG_subprogram;
    ProgUNITNAME_254.Children := 1;
    ProgUNITNAME_254.Add(DW_AT_name, DW_FORM_string, 'UNITNAME'+#0);
    ProgUNITNAME_254.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgUNITNAME_254.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgUNITNAME_254.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgUNITNAME_254.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclANSISTRING_350); // $B3, $13, $00, $00

      Varself_255 := ProgUNITNAME_254.GetNewChild;
      Varself_255.Tag := DW_TAG_formal_parameter;
      Varself_255.Children := 0;
      Varself_255.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_255.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_255.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_353); // $CD, $13, $00, $00

      Varresult_256 := ProgUNITNAME_254.GetNewChild;
      Varresult_256.Tag := DW_TAG_variable;
      Varresult_256.Children := 0;
      Varresult_256.Add(DW_AT_name, DW_FORM_string, 'result'+#0);
      Varresult_256.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclANSISTRING_350); // $B3, $13, $00, $00

    ProgEQUALS_257 := TypeTOBJECT_162.GetNewChild;
    ProgEQUALS_257.Tag := DW_TAG_subprogram;
    ProgEQUALS_257.Children := 1;
    ProgEQUALS_257.Add(DW_AT_name, DW_FORM_string, 'EQUALS'+#0);
    ProgEQUALS_257.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgEQUALS_257.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgEQUALS_257.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgEQUALS_257.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgEQUALS_257.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $16]));
    ProgEQUALS_257.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_82); // $A3, $06, $00, $00

      Varthis_258 := ProgEQUALS_257.GetNewChild;
      Varthis_258.Tag := DW_TAG_formal_parameter;
      Varthis_258.Children := 0;
      Varthis_258.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_258.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_258.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

      VarOBJ_259 := ProgEQUALS_257.GetNewChild;
      VarOBJ_259.Tag := DW_TAG_formal_parameter;
      VarOBJ_259.Children := 0;
      VarOBJ_259.Add(DW_AT_name, DW_FORM_string, 'OBJ'+#0);
      VarOBJ_259.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

    ProgGETHASHCODE_260 := TypeTOBJECT_162.GetNewChild;
    ProgGETHASHCODE_260.Tag := DW_TAG_subprogram;
    ProgGETHASHCODE_260.Children := 1;
    ProgGETHASHCODE_260.Add(DW_AT_name, DW_FORM_string, 'GETHASHCODE'+#0);
    ProgGETHASHCODE_260.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETHASHCODE_260.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETHASHCODE_260.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETHASHCODE_260.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgGETHASHCODE_260.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $17]));
    ProgGETHASHCODE_260.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_76); // $6D, $06, $00, $00

      Varthis_261 := ProgGETHASHCODE_260.GetNewChild;
      Varthis_261.Tag := DW_TAG_formal_parameter;
      Varthis_261.Children := 0;
      Varthis_261.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_261.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_261.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

    ProgTOSTRING_262 := TypeTOBJECT_162.GetNewChild;
    ProgTOSTRING_262.Tag := DW_TAG_subprogram;
    ProgTOSTRING_262.Children := 1;
    ProgTOSTRING_262.Add(DW_AT_name, DW_FORM_string, 'TOSTRING'+#0);
    ProgTOSTRING_262.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgTOSTRING_262.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgTOSTRING_262.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgTOSTRING_262.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgTOSTRING_262.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $18]));
    ProgTOSTRING_262.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclANSISTRING_350); // $B3, $13, $00, $00

      Varthis_263 := ProgTOSTRING_262.GetNewChild;
      Varthis_263.Tag := DW_TAG_formal_parameter;
      Varthis_263.Children := 0;
      Varthis_263.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_263.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_263.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

      Varresult_264 := ProgTOSTRING_262.GetNewChild;
      Varresult_264.Tag := DW_TAG_variable;
      Varresult_264.Children := 0;
      Varresult_264.Add(DW_AT_name, DW_FORM_string, 'result'+#0);
      Varresult_264.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclANSISTRING_350); // $B3, $13, $00, $00

  Type_265 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_265.Tag := DW_TAG_reference_type;
  Type_265.Children := 0;
  Type_265.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_160); // $6C, $0A, $00, $00

  TypeDeclINT64_266 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclINT64_266.Tag := DW_TAG_typedef;
  TypeDeclINT64_266.Children := 0;
  TypeDeclINT64_266.Add(DW_AT_name, DW_FORM_string, 'INT64'+#0);
  TypeDeclINT64_266.AddRef(DW_AT_type, DW_FORM_ref4, @TypeInt64_267); // $B1, $10, $00, $00

  TypeInt64_267 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeInt64_267.Tag := DW_TAG_base_type;
  TypeInt64_267.Children := 0;
  TypeInt64_267.Add(DW_AT_name, DW_FORM_string, 'Int64'+#0);
  TypeInt64_267.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeInt64_267.Add(DW_AT_byte_size, DW_FORM_data1, [$08]);

  Type_268 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_268.Tag := DW_TAG_reference_type;
  Type_268.Children := 0;
  Type_268.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclINT64_266); // $A6, $10, $00, $00

  TypeDecl__vtbl_ptr_type_269 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDecl__vtbl_ptr_type_269.Tag := DW_TAG_typedef;
  TypeDecl__vtbl_ptr_type_269.Children := 0;
  TypeDecl__vtbl_ptr_type_269.Add(DW_AT_name, DW_FORM_string, '__vtbl_ptr_type'+#0);
  TypeDecl__vtbl_ptr_type_269.AddRef(DW_AT_type, DW_FORM_ref4, @Type_270); // $D4, $10, $00, $00

  Type_270 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_270.Tag := DW_TAG_structure_type;
  Type_270.Children := 0;
  Type_270.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 20);

  Type_271 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_271.Tag := DW_TAG_reference_type;
  Type_271.Children := 0;
  Type_271.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  TypeDeclLONGBOOL_272 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclLONGBOOL_272.Tag := DW_TAG_typedef;
  TypeDeclLONGBOOL_272.Children := 0;
  TypeDeclLONGBOOL_272.Add(DW_AT_name, DW_FORM_string, 'LONGBOOL'+#0);
  TypeDeclLONGBOOL_272.AddRef(DW_AT_type, DW_FORM_ref4, @TypeLongBool_273); // $EA, $10, $00, $00

  TypeLongBool_273 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeLongBool_273.Tag := DW_TAG_base_type;
  TypeLongBool_273.Children := 0;
  TypeLongBool_273.Add(DW_AT_name, DW_FORM_string, 'LongBool'+#0);
  TypeLongBool_273.Add(DW_AT_encoding, DW_FORM_data1, [$02]);
  TypeLongBool_273.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

  Type_274 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_274.Tag := DW_TAG_reference_type;
  Type_274.Children := 0;
  Type_274.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGBOOL_272); // $DC, $10, $00, $00

  TypeDeclBYTEBOOL_275 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclBYTEBOOL_275.Tag := DW_TAG_typedef;
  TypeDeclBYTEBOOL_275.Children := 0;
  TypeDeclBYTEBOOL_275.Add(DW_AT_name, DW_FORM_string, 'BYTEBOOL'+#0);
  TypeDeclBYTEBOOL_275.AddRef(DW_AT_type, DW_FORM_ref4, @TypeByteBool_276); // $09, $11, $00, $00

  TypeByteBool_276 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeByteBool_276.Tag := DW_TAG_base_type;
  TypeByteBool_276.Children := 0;
  TypeByteBool_276.Add(DW_AT_name, DW_FORM_string, 'ByteBool'+#0);
  TypeByteBool_276.Add(DW_AT_encoding, DW_FORM_data1, [$02]);
  TypeByteBool_276.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_277 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_277.Tag := DW_TAG_reference_type;
  Type_277.Children := 0;
  Type_277.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTEBOOL_275); // $FB, $10, $00, $00

  TypePtr_278 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_278.Tag := DW_TAG_pointer_type;
  TypePtr_278.Children := 0;
  TypePtr_278.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_279 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_279.Tag := DW_TAG_reference_type;
  Type_279.Children := 0;
  Type_279.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_278); // $1A, $11, $00, $00

  TypeDeclHRESULT_280 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclHRESULT_280.Tag := DW_TAG_typedef;
  TypeDeclHRESULT_280.Children := 0;
  TypeDeclHRESULT_280.Add(DW_AT_name, DW_FORM_string, 'HRESULT'+#0);
  TypeDeclHRESULT_280.AddRef(DW_AT_type, DW_FORM_ref4, @TypeHRESULT_281); // $31, $11, $00, $00

  TypeHRESULT_281 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeHRESULT_281.Tag := DW_TAG_base_type;
  TypeHRESULT_281.Children := 0;
  TypeHRESULT_281.Add(DW_AT_name, DW_FORM_string, 'HRESULT'+#0);
  TypeHRESULT_281.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeHRESULT_281.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

  Type_282 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_282.Tag := DW_TAG_reference_type;
  Type_282.Children := 0;
  Type_282.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclHRESULT_280); // $24, $11, $00, $00

  TypeDeclformal_283 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclformal_283.Tag := DW_TAG_typedef;
  TypeDeclformal_283.Children := 0;
  TypeDeclformal_283.Add(DW_AT_name, DW_FORM_string, 'formal'+#0);
  TypeDeclformal_283.AddRef(DW_AT_type, DW_FORM_ref4, @TypeFormalDef_284); // $4D, $11, $00, $00

  TypeFormalDef_284 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeFormalDef_284.Tag := DW_TAG_base_type;
  TypeFormalDef_284.Children := 0;
  TypeFormalDef_284.Add(DW_AT_name, DW_FORM_string, 'FormalDef'+#0);
  TypeFormalDef_284.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeFormalDef_284.Add(DW_AT_byte_size, DW_FORM_data1, [$00]);

  Type_285 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_285.Tag := DW_TAG_reference_type;
  Type_285.Children := 0;
  Type_285.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_283); // $41, $11, $00, $00

  TypePtr_286 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_286.Tag := DW_TAG_pointer_type;
  TypePtr_286.Children := 0;
  TypePtr_286.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_287 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_287.Tag := DW_TAG_reference_type;
  Type_287.Children := 0;
  Type_287.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_286); // $5F, $11, $00, $00

  TypeDeclTCLASS_288 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTCLASS_288.Tag := DW_TAG_typedef;
  TypeDeclTCLASS_288.Children := 0;
  TypeDeclTCLASS_288.Add(DW_AT_name, DW_FORM_string, 'TCLASS'+#0);
  TypeDeclTCLASS_288.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_289); // $75, $11, $00, $00

  TypePtr_289 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_289.Tag := DW_TAG_pointer_type;
  TypePtr_289.Children := 0;
  TypePtr_289.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_290 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_290.Tag := DW_TAG_reference_type;
  Type_290.Children := 0;
  Type_290.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTCLASS_288); // $69, $11, $00, $00

  TypePtr_291 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_291.Tag := DW_TAG_pointer_type;
  TypePtr_291.Children := 0;
  TypePtr_291.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_292 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_292.Tag := DW_TAG_reference_type;
  Type_292.Children := 0;
  Type_292.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_291); // $7F, $11, $00, $00

  TypePtr_293 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_293.Tag := DW_TAG_pointer_type;
  TypePtr_293.Children := 0;
  TypePtr_293.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_294 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_294.Tag := DW_TAG_reference_type;
  Type_294.Children := 0;
  Type_294.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_293); // $89, $11, $00, $00

  TypeDeclSHORTSTRING_295 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclSHORTSTRING_295.Tag := DW_TAG_typedef;
  TypeDeclSHORTSTRING_295.Children := 0;
  TypeDeclSHORTSTRING_295.Add(DW_AT_name, DW_FORM_string, 'SHORTSTRING'+#0);
  TypeDeclSHORTSTRING_295.AddRef(DW_AT_type, DW_FORM_ref4, @TypeShortString_296); // $A4, $11, $00, $00

  TypeShortString_296 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeShortString_296.Tag := DW_TAG_structure_type;
  TypeShortString_296.Children := 1;
  TypeShortString_296.Add(DW_AT_name, DW_FORM_string, 'ShortString'+#0);
  TypeShortString_296.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 256);

    Varlength_297 := TypeShortString_296.GetNewChild;
    Varlength_297.Tag := DW_TAG_member;
    Varlength_297.Children := 0;
    Varlength_297.Add(DW_AT_name, DW_FORM_string, 'length'+#0);
    Varlength_297.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    Varlength_297.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_67); // $20, $06, $00, $00

    Varst_298 := TypeShortString_296.GetNewChild;
    Varst_298.Tag := DW_TAG_member;
    Varst_298.Children := 0;
    Varst_298.Add(DW_AT_name, DW_FORM_string, 'st'+#0);
    Varst_298.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(1)])); // $23, $01
    Varst_298.AddRef(DW_AT_type, DW_FORM_ref4, @Type_299); // $CE, $11, $00, $00

  Type_299 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_299.Tag := DW_TAG_array_type;
  Type_299.Children := 1;
  Type_299.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 256);
  Type_299.AddSLEB(DW_AT_byte_stride, DW_FORM_udata, 1);
  Type_299.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclCHAR_355); // $D7, $13, $00, $00

    Type_300 := Type_299.GetNewChild;
    Type_300.Tag := DW_TAG_subrange_type;
    Type_300.Children := 0;
    Type_300.AddSLEB(DW_AT_lower_bound, DW_FORM_udata, 0);
    Type_300.AddSLEB(DW_AT_upper_bound, DW_FORM_udata, 255);
    Type_300.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_67); // $20, $06, $00, $00

  Type_301 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_301.Tag := DW_TAG_reference_type;
  Type_301.Children := 0;
  Type_301.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_295); // $93, $11, $00, $00

  TypePtr_302 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_302.Tag := DW_TAG_pointer_type;
  TypePtr_302.Children := 0;
  TypePtr_302.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_303 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_303.Tag := DW_TAG_reference_type;
  Type_303.Children := 0;
  Type_303.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_302); // $E4, $11, $00, $00

  TypePtr_304 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_304.Tag := DW_TAG_pointer_type;
  TypePtr_304.Children := 0;
  TypePtr_304.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_305 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_305.Tag := DW_TAG_reference_type;
  Type_305.Children := 0;
  Type_305.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_304); // $EE, $11, $00, $00

  TypePtr_306 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_306.Tag := DW_TAG_pointer_type;
  TypePtr_306.Children := 0;
  TypePtr_306.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_307 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_307.Tag := DW_TAG_reference_type;
  Type_307.Children := 0;
  Type_307.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_306); // $F8, $11, $00, $00

  TypePtr_308 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_308.Tag := DW_TAG_pointer_type;
  TypePtr_308.Children := 0;
  TypePtr_308.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_309 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_309.Tag := DW_TAG_reference_type;
  Type_309.Children := 0;
  Type_309.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_308); // $02, $12, $00, $00

  TypePtr_310 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_310.Tag := DW_TAG_pointer_type;
  TypePtr_310.Children := 0;
  TypePtr_310.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_311 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_311.Tag := DW_TAG_reference_type;
  Type_311.Children := 0;
  Type_311.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_310); // $0C, $12, $00, $00

  TypeDeclPSTRINGMESSAGETABLE_312 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPSTRINGMESSAGETABLE_312.Tag := DW_TAG_typedef;
  TypeDeclPSTRINGMESSAGETABLE_312.Children := 0;
  TypeDeclPSTRINGMESSAGETABLE_312.Add(DW_AT_name, DW_FORM_string, 'PSTRINGMESSAGETABLE'+#0);
  TypeDeclPSTRINGMESSAGETABLE_312.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_313); // $2F, $12, $00, $00

  TypePtr_313 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_313.Tag := DW_TAG_pointer_type;
  TypePtr_313.Children := 0;
  TypePtr_313.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSTRINGMESSAGETABLE_358); // $EE, $13, $00, $00

  Type_314 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_314.Tag := DW_TAG_reference_type;
  Type_314.Children := 0;
  Type_314.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSTRINGMESSAGETABLE_312); // $16, $12, $00, $00

  TypePtr_315 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_315.Tag := DW_TAG_pointer_type;
  TypePtr_315.Children := 0;
  TypePtr_315.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_316 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_316.Tag := DW_TAG_reference_type;
  Type_316.Children := 0;
  Type_316.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_315); // $39, $12, $00, $00

  TypePtr_317 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_317.Tag := DW_TAG_pointer_type;
  TypePtr_317.Children := 0;
  TypePtr_317.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_318 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_318.Tag := DW_TAG_reference_type;
  Type_318.Children := 0;
  Type_318.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_317); // $43, $12, $00, $00

  TypePtr_319 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_319.Tag := DW_TAG_pointer_type;
  TypePtr_319.Children := 0;
  TypePtr_319.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_320 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_320.Tag := DW_TAG_reference_type;
  Type_320.Children := 0;
  Type_320.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_319); // $4D, $12, $00, $00

  TypeDeclTGUID_321 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTGUID_321.Tag := DW_TAG_typedef;
  TypeDeclTGUID_321.Children := 0;
  TypeDeclTGUID_321.Add(DW_AT_name, DW_FORM_string, 'TGUID'+#0);
  TypeDeclTGUID_321.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTGUID_322); // $62, $12, $00, $00

  TypeTGUID_322 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTGUID_322.Tag := DW_TAG_structure_type;
  TypeTGUID_322.Children := 1;
  TypeTGUID_322.Add(DW_AT_name, DW_FORM_string, 'TGUID'+#0);
  TypeTGUID_322.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 16);

    VarDATA1_323 := TypeTGUID_322.GetNewChild;
    VarDATA1_323.Tag := DW_TAG_member;
    VarDATA1_323.Children := 0;
    VarDATA1_323.Add(DW_AT_name, DW_FORM_string, 'DATA1'+#0);
    VarDATA1_323.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarDATA1_323.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_363); // $45, $14, $00, $00

    VarDATA2_324 := TypeTGUID_322.GetNewChild;
    VarDATA2_324.Tag := DW_TAG_member;
    VarDATA2_324.Children := 0;
    VarDATA2_324.Add(DW_AT_name, DW_FORM_string, 'DATA2'+#0);
    VarDATA2_324.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarDATA2_324.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_73); // $56, $06, $00, $00

    VarDATA3_325 := TypeTGUID_322.GetNewChild;
    VarDATA3_325.Tag := DW_TAG_member;
    VarDATA3_325.Children := 0;
    VarDATA3_325.Add(DW_AT_name, DW_FORM_string, 'DATA3'+#0);
    VarDATA3_325.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(6)])); // $23, $06
    VarDATA3_325.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_73); // $56, $06, $00, $00

    VarDATA4_326 := TypeTGUID_322.GetNewChild;
    VarDATA4_326.Tag := DW_TAG_member;
    VarDATA4_326.Children := 0;
    VarDATA4_326.Add(DW_AT_name, DW_FORM_string, 'DATA4'+#0);
    VarDATA4_326.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(8)])); // $23, $08
    VarDATA4_326.AddRef(DW_AT_type, DW_FORM_ref4, @Type_366); // $64, $14, $00, $00

    VarD1_327 := TypeTGUID_322.GetNewChild;
    VarD1_327.Tag := DW_TAG_member;
    VarD1_327.Children := 0;
    VarD1_327.Add(DW_AT_name, DW_FORM_string, 'D1'+#0);
    VarD1_327.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarD1_327.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_363); // $45, $14, $00, $00

    VarD2_328 := TypeTGUID_322.GetNewChild;
    VarD2_328.Tag := DW_TAG_member;
    VarD2_328.Children := 0;
    VarD2_328.Add(DW_AT_name, DW_FORM_string, 'D2'+#0);
    VarD2_328.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarD2_328.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_73); // $56, $06, $00, $00

    VarD3_329 := TypeTGUID_322.GetNewChild;
    VarD3_329.Tag := DW_TAG_member;
    VarD3_329.Children := 0;
    VarD3_329.Add(DW_AT_name, DW_FORM_string, 'D3'+#0);
    VarD3_329.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(6)])); // $23, $06
    VarD3_329.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_73); // $56, $06, $00, $00

    VarD4_330 := TypeTGUID_322.GetNewChild;
    VarD4_330.Tag := DW_TAG_member;
    VarD4_330.Children := 0;
    VarD4_330.Add(DW_AT_name, DW_FORM_string, 'D4'+#0);
    VarD4_330.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(8)])); // $23, $08
    VarD4_330.AddRef(DW_AT_type, DW_FORM_ref4, @Type_369); // $78, $14, $00, $00

    VarTIME_LOW_331 := TypeTGUID_322.GetNewChild;
    VarTIME_LOW_331.Tag := DW_TAG_member;
    VarTIME_LOW_331.Children := 0;
    VarTIME_LOW_331.Add(DW_AT_name, DW_FORM_string, 'TIME_LOW'+#0);
    VarTIME_LOW_331.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarTIME_LOW_331.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_363); // $45, $14, $00, $00

    VarTIME_MID_332 := TypeTGUID_322.GetNewChild;
    VarTIME_MID_332.Tag := DW_TAG_member;
    VarTIME_MID_332.Children := 0;
    VarTIME_MID_332.Add(DW_AT_name, DW_FORM_string, 'TIME_MID'+#0);
    VarTIME_MID_332.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarTIME_MID_332.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_73); // $56, $06, $00, $00

    VarTIME_HI_AND_VERSION_333 := TypeTGUID_322.GetNewChild;
    VarTIME_HI_AND_VERSION_333.Tag := DW_TAG_member;
    VarTIME_HI_AND_VERSION_333.Children := 0;
    VarTIME_HI_AND_VERSION_333.Add(DW_AT_name, DW_FORM_string, 'TIME_HI_AND_VERSION'+#0);
    VarTIME_HI_AND_VERSION_333.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(6)])); // $23, $06
    VarTIME_HI_AND_VERSION_333.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_73); // $56, $06, $00, $00

    VarCLOCK_SEQ_HI_AND_RESERVED_334 := TypeTGUID_322.GetNewChild;
    VarCLOCK_SEQ_HI_AND_RESERVED_334.Tag := DW_TAG_member;
    VarCLOCK_SEQ_HI_AND_RESERVED_334.Children := 0;
    VarCLOCK_SEQ_HI_AND_RESERVED_334.Add(DW_AT_name, DW_FORM_string, 'CLOCK_SEQ_HI_AND_RESERVED'+#0);
    VarCLOCK_SEQ_HI_AND_RESERVED_334.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(8)])); // $23, $08
    VarCLOCK_SEQ_HI_AND_RESERVED_334.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_67); // $20, $06, $00, $00

    VarCLOCK_SEQ_LOW_335 := TypeTGUID_322.GetNewChild;
    VarCLOCK_SEQ_LOW_335.Tag := DW_TAG_member;
    VarCLOCK_SEQ_LOW_335.Children := 0;
    VarCLOCK_SEQ_LOW_335.Add(DW_AT_name, DW_FORM_string, 'CLOCK_SEQ_LOW'+#0);
    VarCLOCK_SEQ_LOW_335.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(9)])); // $23, $09
    VarCLOCK_SEQ_LOW_335.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_67); // $20, $06, $00, $00

    VarNODE_336 := TypeTGUID_322.GetNewChild;
    VarNODE_336.Tag := DW_TAG_member;
    VarNODE_336.Children := 0;
    VarNODE_336.Add(DW_AT_name, DW_FORM_string, 'NODE'+#0);
    VarNODE_336.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(10)])); // $23, $0A
    VarNODE_336.AddRef(DW_AT_type, DW_FORM_ref4, @Type_372); // $8C, $14, $00, $00

  Type_337 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_337.Tag := DW_TAG_reference_type;
  Type_337.Children := 0;
  Type_337.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTGUID_321); // $57, $12, $00, $00

  TypeDeclPINTERFACEENTRY_338 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPINTERFACEENTRY_338.Tag := DW_TAG_typedef;
  TypeDeclPINTERFACEENTRY_338.Children := 0;
  TypeDeclPINTERFACEENTRY_338.Add(DW_AT_name, DW_FORM_string, 'PINTERFACEENTRY'+#0);
  TypeDeclPINTERFACEENTRY_338.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_339); // $6C, $13, $00, $00

  TypePtr_339 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_339.Tag := DW_TAG_pointer_type;
  TypePtr_339.Children := 0;
  TypePtr_339.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACEENTRY_375); // $A0, $14, $00, $00

  Type_340 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_340.Tag := DW_TAG_reference_type;
  Type_340.Children := 0;
  Type_340.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTERFACEENTRY_338); // $57, $13, $00, $00

  TypePtr_341 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_341.Tag := DW_TAG_pointer_type;
  TypePtr_341.Children := 0;
  TypePtr_341.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_342 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_342.Tag := DW_TAG_reference_type;
  Type_342.Children := 0;
  Type_342.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_341); // $76, $13, $00, $00

  TypePtr_343 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_343.Tag := DW_TAG_pointer_type;
  TypePtr_343.Children := 0;
  TypePtr_343.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_344 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_344.Tag := DW_TAG_reference_type;
  Type_344.Children := 0;
  Type_344.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_343); // $80, $13, $00, $00

  TypeDeclPINTERFACETABLE_345 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPINTERFACETABLE_345.Tag := DW_TAG_typedef;
  TypeDeclPINTERFACETABLE_345.Children := 0;
  TypeDeclPINTERFACETABLE_345.Add(DW_AT_name, DW_FORM_string, 'PINTERFACETABLE'+#0);
  TypeDeclPINTERFACETABLE_345.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_346); // $9F, $13, $00, $00

  TypePtr_346 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_346.Tag := DW_TAG_pointer_type;
  TypePtr_346.Children := 0;
  TypePtr_346.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACETABLE_384); // $29, $15, $00, $00

  Type_347 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_347.Tag := DW_TAG_reference_type;
  Type_347.Children := 0;
  Type_347.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTERFACETABLE_345); // $8A, $13, $00, $00

  TypePtr_348 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_348.Tag := DW_TAG_pointer_type;
  TypePtr_348.Children := 0;
  TypePtr_348.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_349 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_349.Tag := DW_TAG_reference_type;
  Type_349.Children := 0;
  Type_349.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_348); // $A9, $13, $00, $00

  TypeDeclANSISTRING_350 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclANSISTRING_350.Tag := DW_TAG_typedef;
  TypeDeclANSISTRING_350.Children := 0;
  TypeDeclANSISTRING_350.Add(DW_AT_name, DW_FORM_string, 'ANSISTRING'+#0);
  TypeDeclANSISTRING_350.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_351); // $C3, $13, $00, $00

  TypePtr_351 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_351.Tag := DW_TAG_pointer_type;
  TypePtr_351.Children := 0;
  TypePtr_351.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclCHAR_355); // $D7, $13, $00, $00

  Type_352 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_352.Tag := DW_TAG_reference_type;
  Type_352.Children := 0;
  Type_352.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclANSISTRING_350); // $B3, $13, $00, $00

  TypePtr_353 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_353.Tag := DW_TAG_pointer_type;
  TypePtr_353.Children := 0;
  TypePtr_353.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_269); // $BF, $10, $00, $00

  Type_354 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_354.Tag := DW_TAG_reference_type;
  Type_354.Children := 0;
  Type_354.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_353); // $CD, $13, $00, $00

  TypeDeclCHAR_355 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclCHAR_355.Tag := DW_TAG_typedef;
  TypeDeclCHAR_355.Children := 0;
  TypeDeclCHAR_355.Add(DW_AT_name, DW_FORM_string, 'CHAR'+#0);
  TypeDeclCHAR_355.AddRef(DW_AT_type, DW_FORM_ref4, @TypeChar_356); // $E1, $13, $00, $00

  TypeChar_356 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeChar_356.Tag := DW_TAG_base_type;
  TypeChar_356.Children := 0;
  TypeChar_356.Add(DW_AT_name, DW_FORM_string, 'Char'+#0);
  TypeChar_356.Add(DW_AT_encoding, DW_FORM_data1, [$08]);
  TypeChar_356.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_357 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_357.Tag := DW_TAG_reference_type;
  Type_357.Children := 0;
  Type_357.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclCHAR_355); // $D7, $13, $00, $00

  TypeDeclTSTRINGMESSAGETABLE_358 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTSTRINGMESSAGETABLE_358.Tag := DW_TAG_typedef;
  TypeDeclTSTRINGMESSAGETABLE_358.Children := 0;
  TypeDeclTSTRINGMESSAGETABLE_358.Add(DW_AT_name, DW_FORM_string, 'TSTRINGMESSAGETABLE'+#0);
  TypeDeclTSTRINGMESSAGETABLE_358.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSTRINGMESSAGETABLE_359); // $07, $14, $00, $00

  TypeTSTRINGMESSAGETABLE_359 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTSTRINGMESSAGETABLE_359.Tag := DW_TAG_structure_type;
  TypeTSTRINGMESSAGETABLE_359.Children := 1;
  TypeTSTRINGMESSAGETABLE_359.Add(DW_AT_name, DW_FORM_string, 'TSTRINGMESSAGETABLE'+#0);
  TypeTSTRINGMESSAGETABLE_359.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 12);

    VarCOUNT_360 := TypeTSTRINGMESSAGETABLE_359.GetNewChild;
    VarCOUNT_360.Tag := DW_TAG_member;
    VarCOUNT_360.Children := 0;
    VarCOUNT_360.Add(DW_AT_name, DW_FORM_string, 'COUNT'+#0);
    VarCOUNT_360.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarCOUNT_360.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_76); // $6D, $06, $00, $00

    VarMSGSTRTABLE_361 := TypeTSTRINGMESSAGETABLE_359.GetNewChild;
    VarMSGSTRTABLE_361.Tag := DW_TAG_member;
    VarMSGSTRTABLE_361.Children := 0;
    VarMSGSTRTABLE_361.Add(DW_AT_name, DW_FORM_string, 'MSGSTRTABLE'+#0);
    VarMSGSTRTABLE_361.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarMSGSTRTABLE_361.AddRef(DW_AT_type, DW_FORM_ref4, @Type_389); // $79, $15, $00, $00

  Type_362 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_362.Tag := DW_TAG_reference_type;
  Type_362.Children := 0;
  Type_362.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSTRINGMESSAGETABLE_358); // $EE, $13, $00, $00

  TypeDeclLONGWORD_363 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclLONGWORD_363.Tag := DW_TAG_typedef;
  TypeDeclLONGWORD_363.Children := 0;
  TypeDeclLONGWORD_363.Add(DW_AT_name, DW_FORM_string, 'LONGWORD'+#0);
  TypeDeclLONGWORD_363.AddRef(DW_AT_type, DW_FORM_ref4, @TypeLONGWORD_364); // $53, $14, $00, $00

  TypeLONGWORD_364 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeLONGWORD_364.Tag := DW_TAG_base_type;
  TypeLONGWORD_364.Children := 0;
  TypeLONGWORD_364.Add(DW_AT_name, DW_FORM_string, 'LONGWORD'+#0);
  TypeLONGWORD_364.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeLONGWORD_364.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

  Type_365 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_365.Tag := DW_TAG_reference_type;
  Type_365.Children := 0;
  Type_365.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_363); // $45, $14, $00, $00

  Type_366 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_366.Tag := DW_TAG_array_type;
  Type_366.Children := 1;
  Type_366.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 8);
  Type_366.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_67); // $20, $06, $00, $00

    Type_367 := Type_366.GetNewChild;
    Type_367.Tag := DW_TAG_subrange_type;
    Type_367.Children := 0;
    Type_367.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
    Type_367.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 7);
    Type_367.AddSLEB(DW_AT_byte_stride, DW_FORM_udata, 1);
    Type_367.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_70); // $37, $06, $00, $00

  Type_368 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_368.Tag := DW_TAG_reference_type;
  Type_368.Children := 0;
  Type_368.AddRef(DW_AT_type, DW_FORM_ref4, @Type_366); // $64, $14, $00, $00

  Type_369 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_369.Tag := DW_TAG_array_type;
  Type_369.Children := 1;
  Type_369.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 8);
  Type_369.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_67); // $20, $06, $00, $00

    Type_370 := Type_369.GetNewChild;
    Type_370.Tag := DW_TAG_subrange_type;
    Type_370.Children := 0;
    Type_370.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
    Type_370.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 7);
    Type_370.AddSLEB(DW_AT_byte_stride, DW_FORM_udata, 1);
    Type_370.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_70); // $37, $06, $00, $00

  Type_371 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_371.Tag := DW_TAG_reference_type;
  Type_371.Children := 0;
  Type_371.AddRef(DW_AT_type, DW_FORM_ref4, @Type_369); // $78, $14, $00, $00

  Type_372 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_372.Tag := DW_TAG_array_type;
  Type_372.Children := 1;
  Type_372.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 6);
  Type_372.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_67); // $20, $06, $00, $00

    Type_373 := Type_372.GetNewChild;
    Type_373.Tag := DW_TAG_subrange_type;
    Type_373.Children := 0;
    Type_373.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
    Type_373.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 5);
    Type_373.AddSLEB(DW_AT_byte_stride, DW_FORM_udata, 1);
    Type_373.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_70); // $37, $06, $00, $00

  Type_374 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_374.Tag := DW_TAG_reference_type;
  Type_374.Children := 0;
  Type_374.AddRef(DW_AT_type, DW_FORM_ref4, @Type_372); // $8C, $14, $00, $00

  TypeDeclTINTERFACEENTRY_375 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTINTERFACEENTRY_375.Tag := DW_TAG_typedef;
  TypeDeclTINTERFACEENTRY_375.Children := 0;
  TypeDeclTINTERFACEENTRY_375.Add(DW_AT_name, DW_FORM_string, 'TINTERFACEENTRY'+#0);
  TypeDeclTINTERFACEENTRY_375.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTINTERFACEENTRY_376); // $B5, $14, $00, $00

  TypeTINTERFACEENTRY_376 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTINTERFACEENTRY_376.Tag := DW_TAG_structure_type;
  TypeTINTERFACEENTRY_376.Children := 1;
  TypeTINTERFACEENTRY_376.Add(DW_AT_name, DW_FORM_string, 'TINTERFACEENTRY'+#0);
  TypeTINTERFACEENTRY_376.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 20);

    VarIID_377 := TypeTINTERFACEENTRY_376.GetNewChild;
    VarIID_377.Tag := DW_TAG_member;
    VarIID_377.Children := 0;
    VarIID_377.Add(DW_AT_name, DW_FORM_string, 'IID'+#0);
    VarIID_377.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarIID_377.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPGUID_392); // $8D, $15, $00, $00

    VarVTABLE_378 := TypeTINTERFACEENTRY_376.GetNewChild;
    VarVTABLE_378.Tag := DW_TAG_member;
    VarVTABLE_378.Children := 0;
    VarVTABLE_378.Add(DW_AT_name, DW_FORM_string, 'VTABLE'+#0);
    VarVTABLE_378.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarVTABLE_378.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_85); // $C0, $06, $00, $00

    VarIOFFSET_379 := TypeTINTERFACEENTRY_376.GetNewChild;
    VarIOFFSET_379.Tag := DW_TAG_member;
    VarIOFFSET_379.Children := 0;
    VarIOFFSET_379.Add(DW_AT_name, DW_FORM_string, 'IOFFSET'+#0);
    VarIOFFSET_379.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(8)])); // $23, $08
    VarIOFFSET_379.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_363); // $45, $14, $00, $00

    VarIIDSTR_380 := TypeTINTERFACEENTRY_376.GetNewChild;
    VarIIDSTR_380.Tag := DW_TAG_member;
    VarIIDSTR_380.Children := 0;
    VarIIDSTR_380.Add(DW_AT_name, DW_FORM_string, 'IIDSTR'+#0);
    VarIIDSTR_380.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(12)])); // $23, $0C
    VarIIDSTR_380.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSHORTSTRING_395); // $A2, $15, $00, $00

    VarITYPE_381 := TypeTINTERFACEENTRY_376.GetNewChild;
    VarITYPE_381.Tag := DW_TAG_member;
    VarITYPE_381.Children := 0;
    VarITYPE_381.Add(DW_AT_name, DW_FORM_string, 'ITYPE'+#0);
    VarITYPE_381.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(16)])); // $23, $10
    VarITYPE_381.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACEENTRYTYPE_398); // $BE, $15, $00, $00

    Var__PAD_DUMMY_382 := TypeTINTERFACEENTRY_376.GetNewChild;
    Var__PAD_DUMMY_382.Tag := DW_TAG_member;
    Var__PAD_DUMMY_382.Children := 0;
    Var__PAD_DUMMY_382.Add(DW_AT_name, DW_FORM_string, '__PAD_DUMMY'+#0);
    Var__PAD_DUMMY_382.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(16)])); // $23, $10
    Var__PAD_DUMMY_382.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_85); // $C0, $06, $00, $00

  Type_383 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_383.Tag := DW_TAG_reference_type;
  Type_383.Children := 0;
  Type_383.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACEENTRY_375); // $A0, $14, $00, $00

  TypeDeclTINTERFACETABLE_384 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTINTERFACETABLE_384.Tag := DW_TAG_typedef;
  TypeDeclTINTERFACETABLE_384.Children := 0;
  TypeDeclTINTERFACETABLE_384.Add(DW_AT_name, DW_FORM_string, 'TINTERFACETABLE'+#0);
  TypeDeclTINTERFACETABLE_384.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTINTERFACETABLE_385); // $3E, $15, $00, $00

  TypeTINTERFACETABLE_385 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTINTERFACETABLE_385.Tag := DW_TAG_structure_type;
  TypeTINTERFACETABLE_385.Children := 1;
  TypeTINTERFACETABLE_385.Add(DW_AT_name, DW_FORM_string, 'TINTERFACETABLE'+#0);
  TypeTINTERFACETABLE_385.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 24);

    VarENTRYCOUNT_386 := TypeTINTERFACETABLE_385.GetNewChild;
    VarENTRYCOUNT_386.Tag := DW_TAG_member;
    VarENTRYCOUNT_386.Children := 0;
    VarENTRYCOUNT_386.Add(DW_AT_name, DW_FORM_string, 'ENTRYCOUNT'+#0);
    VarENTRYCOUNT_386.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarENTRYCOUNT_386.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_363); // $45, $14, $00, $00

    VarENTRIES_387 := TypeTINTERFACETABLE_385.GetNewChild;
    VarENTRIES_387.Tag := DW_TAG_member;
    VarENTRIES_387.Children := 0;
    VarENTRIES_387.Add(DW_AT_name, DW_FORM_string, 'ENTRIES'+#0);
    VarENTRIES_387.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarENTRIES_387.AddRef(DW_AT_type, DW_FORM_ref4, @Type_408); // $94, $16, $00, $00

  Type_388 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_388.Tag := DW_TAG_reference_type;
  Type_388.Children := 0;
  Type_388.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACETABLE_384); // $29, $15, $00, $00

  Type_389 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_389.Tag := DW_TAG_array_type;
  Type_389.Children := 1;
  Type_389.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 8);
  Type_389.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTMSGSTRTABLE_411); // $A8, $16, $00, $00

    Type_390 := Type_389.GetNewChild;
    Type_390.Tag := DW_TAG_subrange_type;
    Type_390.Children := 0;
    Type_390.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
    Type_390.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 0);
    Type_390.AddSLEB(DW_AT_byte_stride, DW_FORM_udata, 8);
    Type_390.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_70); // $37, $06, $00, $00

  Type_391 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_391.Tag := DW_TAG_reference_type;
  Type_391.Children := 0;
  Type_391.AddRef(DW_AT_type, DW_FORM_ref4, @Type_389); // $79, $15, $00, $00

  TypeDeclPGUID_392 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPGUID_392.Tag := DW_TAG_typedef;
  TypeDeclPGUID_392.Children := 0;
  TypeDeclPGUID_392.Add(DW_AT_name, DW_FORM_string, 'PGUID'+#0);
  TypeDeclPGUID_392.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_393); // $98, $15, $00, $00

  TypePtr_393 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_393.Tag := DW_TAG_pointer_type;
  TypePtr_393.Children := 0;
  TypePtr_393.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTGUID_321); // $57, $12, $00, $00

  Type_394 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_394.Tag := DW_TAG_reference_type;
  Type_394.Children := 0;
  Type_394.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPGUID_392); // $8D, $15, $00, $00

  TypeDeclPSHORTSTRING_395 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPSHORTSTRING_395.Tag := DW_TAG_typedef;
  TypeDeclPSHORTSTRING_395.Children := 0;
  TypeDeclPSHORTSTRING_395.Add(DW_AT_name, DW_FORM_string, 'PSHORTSTRING'+#0);
  TypeDeclPSHORTSTRING_395.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_396); // $B4, $15, $00, $00

  TypePtr_396 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_396.Tag := DW_TAG_pointer_type;
  TypePtr_396.Children := 0;
  TypePtr_396.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_295); // $93, $11, $00, $00

  Type_397 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_397.Tag := DW_TAG_reference_type;
  Type_397.Children := 0;
  Type_397.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSHORTSTRING_395); // $A2, $15, $00, $00

  TypeDeclTINTERFACEENTRYTYPE_398 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTINTERFACEENTRYTYPE_398.Tag := DW_TAG_typedef;
  TypeDeclTINTERFACEENTRYTYPE_398.Children := 0;
  TypeDeclTINTERFACEENTRYTYPE_398.Add(DW_AT_name, DW_FORM_string, 'TINTERFACEENTRYTYPE'+#0);
  TypeDeclTINTERFACEENTRYTYPE_398.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTINTERFACEENTRYTYPE_399); // $D7, $15, $00, $00

  TypeTINTERFACEENTRYTYPE_399 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTINTERFACEENTRYTYPE_399.Tag := DW_TAG_enumeration_type;
  TypeTINTERFACEENTRYTYPE_399.Children := 1;
  TypeTINTERFACEENTRYTYPE_399.Add(DW_AT_name, DW_FORM_string, 'TINTERFACEENTRYTYPE'+#0);
  TypeTINTERFACEENTRYTYPE_399.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeETSTANDARD_400 := TypeTINTERFACEENTRYTYPE_399.GetNewChild;
    TypeETSTANDARD_400.Tag := DW_TAG_enumerator;
    TypeETSTANDARD_400.Children := 0;
    TypeETSTANDARD_400.Add(DW_AT_name, DW_FORM_string, 'ETSTANDARD'+#0);
    TypeETSTANDARD_400.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeETVIRTUALMETHODRESULT_401 := TypeTINTERFACEENTRYTYPE_399.GetNewChild;
    TypeETVIRTUALMETHODRESULT_401.Tag := DW_TAG_enumerator;
    TypeETVIRTUALMETHODRESULT_401.Children := 0;
    TypeETVIRTUALMETHODRESULT_401.Add(DW_AT_name, DW_FORM_string, 'ETVIRTUALMETHODRESULT'+#0);
    TypeETVIRTUALMETHODRESULT_401.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeETSTATICMETHODRESULT_402 := TypeTINTERFACEENTRYTYPE_399.GetNewChild;
    TypeETSTATICMETHODRESULT_402.Tag := DW_TAG_enumerator;
    TypeETSTATICMETHODRESULT_402.Children := 0;
    TypeETSTATICMETHODRESULT_402.Add(DW_AT_name, DW_FORM_string, 'ETSTATICMETHODRESULT'+#0);
    TypeETSTATICMETHODRESULT_402.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

    TypeETFIELDVALUE_403 := TypeTINTERFACEENTRYTYPE_399.GetNewChild;
    TypeETFIELDVALUE_403.Tag := DW_TAG_enumerator;
    TypeETFIELDVALUE_403.Children := 0;
    TypeETFIELDVALUE_403.Add(DW_AT_name, DW_FORM_string, 'ETFIELDVALUE'+#0);
    TypeETFIELDVALUE_403.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

    TypeETVIRTUALMETHODCLASS_404 := TypeTINTERFACEENTRYTYPE_399.GetNewChild;
    TypeETVIRTUALMETHODCLASS_404.Tag := DW_TAG_enumerator;
    TypeETVIRTUALMETHODCLASS_404.Children := 0;
    TypeETVIRTUALMETHODCLASS_404.Add(DW_AT_name, DW_FORM_string, 'ETVIRTUALMETHODCLASS'+#0);
    TypeETVIRTUALMETHODCLASS_404.Add(DW_AT_const_value, DW_FORM_data4, [$04, $00, $00, $00]);

    TypeETSTATICMETHODCLASS_405 := TypeTINTERFACEENTRYTYPE_399.GetNewChild;
    TypeETSTATICMETHODCLASS_405.Tag := DW_TAG_enumerator;
    TypeETSTATICMETHODCLASS_405.Children := 0;
    TypeETSTATICMETHODCLASS_405.Add(DW_AT_name, DW_FORM_string, 'ETSTATICMETHODCLASS'+#0);
    TypeETSTATICMETHODCLASS_405.Add(DW_AT_const_value, DW_FORM_data4, [$05, $00, $00, $00]);

    TypeETFIELDVALUECLASS_406 := TypeTINTERFACEENTRYTYPE_399.GetNewChild;
    TypeETFIELDVALUECLASS_406.Tag := DW_TAG_enumerator;
    TypeETFIELDVALUECLASS_406.Children := 0;
    TypeETFIELDVALUECLASS_406.Add(DW_AT_name, DW_FORM_string, 'ETFIELDVALUECLASS'+#0);
    TypeETFIELDVALUECLASS_406.Add(DW_AT_const_value, DW_FORM_data4, [$06, $00, $00, $00]);

  Type_407 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_407.Tag := DW_TAG_reference_type;
  Type_407.Children := 0;
  Type_407.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACEENTRYTYPE_398); // $BE, $15, $00, $00

  Type_408 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_408.Tag := DW_TAG_array_type;
  Type_408.Children := 1;
  Type_408.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 20);
  Type_408.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACEENTRY_375); // $A0, $14, $00, $00

    Type_409 := Type_408.GetNewChild;
    Type_409.Tag := DW_TAG_subrange_type;
    Type_409.Children := 0;
    Type_409.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
    Type_409.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 0);
    Type_409.AddSLEB(DW_AT_byte_stride, DW_FORM_udata, 20);
    Type_409.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_70); // $37, $06, $00, $00

  Type_410 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_410.Tag := DW_TAG_reference_type;
  Type_410.Children := 0;
  Type_410.AddRef(DW_AT_type, DW_FORM_ref4, @Type_408); // $94, $16, $00, $00

  TypeDeclTMSGSTRTABLE_411 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTMSGSTRTABLE_411.Tag := DW_TAG_typedef;
  TypeDeclTMSGSTRTABLE_411.Children := 0;
  TypeDeclTMSGSTRTABLE_411.Add(DW_AT_name, DW_FORM_string, 'TMSGSTRTABLE'+#0);
  TypeDeclTMSGSTRTABLE_411.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTMSGSTRTABLE_412); // $BA, $16, $00, $00

  TypeTMSGSTRTABLE_412 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTMSGSTRTABLE_412.Tag := DW_TAG_structure_type;
  TypeTMSGSTRTABLE_412.Children := 1;
  TypeTMSGSTRTABLE_412.Add(DW_AT_name, DW_FORM_string, 'TMSGSTRTABLE'+#0);
  TypeTMSGSTRTABLE_412.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 8);

    VarNAME_413 := TypeTMSGSTRTABLE_412.GetNewChild;
    VarNAME_413.Tag := DW_TAG_member;
    VarNAME_413.Children := 0;
    VarNAME_413.Add(DW_AT_name, DW_FORM_string, 'NAME'+#0);
    VarNAME_413.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarNAME_413.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSHORTSTRING_395); // $A2, $15, $00, $00

    VarMETHOD_414 := TypeTMSGSTRTABLE_412.GetNewChild;
    VarMETHOD_414.Tag := DW_TAG_member;
    VarMETHOD_414.Children := 0;
    VarMETHOD_414.Add(DW_AT_name, DW_FORM_string, 'METHOD'+#0);
    VarMETHOD_414.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarMETHOD_414.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_85); // $C0, $06, $00, $00

  Type_415 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_415.Tag := DW_TAG_reference_type;
  Type_415.Children := 0;
  Type_415.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTMSGSTRTABLE_411); // $A8, $16, $00, $00


  //
  SectionDbgInfo.CreateSectionData;
  SectionDbgInfo.AbbrevSection.CreateSectionData;
end;

end.

