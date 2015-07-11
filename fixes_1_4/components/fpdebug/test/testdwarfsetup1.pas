unit TestDwarfSetup1;

{$mode objfpc}{$H+}
{$IF FPC_FULLVERSION>=20701}
  {$OPTIMIZATION NOREMOVEEMPTYPROCS}
  {$OPTIMIZATION NOORDERFIELDS}
{$ENDIF}
{$OPTIMIZATION OFF}
{$A2}

(*
  Data generated from testdata\dwarfsetup1.lpr
// main DW_AT_low_pc at $00400000
// TESTSETUP1BAR DW_AT_low_pc at $00401000
// CLASSPROC0 DW_AT_low_pc at $00402000
// OBJPROC0 DW_AT_low_pc at $00403000
// OBJPROC1 DW_AT_low_pc at $00404000
// OBJPROC1 DW_AT_low_pc at $00405000
*)

//TODO: Adjust member offsets in dwarf to offsets found in exe (in case different fpc version / settings

interface

uses
  FpDbgDwarfConst,
  TestHelperClasses;

const
  TTestSetup1ProcBarAddr = $00401000;

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

  TTestSetup1Record2 = record // same size
    FWord: Word;
    FBool: Boolean;
    FTest: TTestSetup1Class;
  end;
  PTestSetup1Record2 = ^TTestSetup1Record2;

  TTestSetup1Record3 = record // other size
    FWord: Word;
  end;
  PTestSetup1Record3 = ^TTestSetup1Record3;

  { TTestSetup1Class }

  TTestSetup1Class = class
  public
    FBool: Boolean;
    FWord: Word;
    FWordL: QWord;
    FInt: ShortInt;
    FIntL: Int64;
    FTest: TTestSetup1Class;
    FByte: Byte;
    procedure ClassProc0(a:integer); virtual;
  end;
  PTestSetup1Class = ^TTestSetup1Class;

  TTestSetup1ClassChild = class(TTestSetup1Class)
    FInt64: Int64;
    FQWord: QWord;
  end;
  PTestSetup1ClassChild = ^TTestSetup1ClassChild;

  TTestSetup1Class2 = class
  public
    FInt: Integer;
    FWord: Word;
  end;
  PTestSetup1Class2 = ^TTestSetup1Class2;

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
    procedure ObjProc0(o1:integer); virtual;
    //only with a wirtual method, will there be a vptr entry
  end;
  PTestSetup1Object  = ^TTestSetup1Object;

  { TTestSetup1Object2 }

  TTestSetup1Object2 = object // same size
  public
    FWord: Word;
    FWordL: QWord;
    FInt: ShortInt;
    FIntL: Int64;
    FBool: Boolean;
    FBool2: LongBool;
    FBool3: ByteBool;
    FTest: TTestSetup1Class;
    procedure ObjProc1(o2:integer); virtual;
  end;
  PTestSetup1Object2  = ^TTestSetup1Object2;

  { TTestSetup1Object3 }

  TTestSetup1Object3 = object // diff size
  public
    FWord: Word;
    procedure ObjProc1(o2:integer); virtual;
  end;
  PTestSetup1Object3  = ^TTestSetup1Object3;

  TTestSetup1Object4 = object // looks like a record....
  public
    FWord: Word;
  end;
  PTestSetup1Object4  = ^TTestSetup1Object4;

  Pint = ^ integer;
  PPInt = ^Pint;
  PPPInt = ^PPint;
  PQWord = ^QWord;
  {%endregion}

type
  { TTestLoaderSetup1 }

  TTestLoaderSetup1 = class(TTestDummyImageLoader)
  public
    constructor Create; override;
    procedure  PoissonTestFrame;
  public
    SectionDbgInfo: TTestDummySectionInfoEntries;
    //CompUnit, Prog1, Prog2,
    //GlobVar1, TypeInt, TypeIntDecl: TTestDwarfInfoEntry;

    Unitdwarfsetup1_lpr_0, VarGLOBTESTSETUP1RECORD_1, VarGLOBTESTSETUP1RECORDP_2, VarGLOBTESTSETUP1RECORD2_3, VarGLOBTESTSETUP1RECORD2P_4, VarGLOBTESTSETUP1RECORD3_5, VarGLOBTESTSETUP1RECORD3P_6, VarGLOBTESTSETUP1CLASS_7, VarGLOBTESTSETUP1CLASSP_8, VarGLOBTESTSETUP1CLASSCHILD_9, VarGLOBTESTSETUP1CLASSCHILDP_10, VarGLOBTESTSETUP1CLASS2_11, VarGLOBTESTSETUP1CLASS2P_12, VarGLOBTESTSETUP1CLASSCLASS_13, VarGLOBTESTSETUP1CLASSCHILDCLASS_14, VarGLOBTESTSETUP1OBJECT_15, VarGLOBTESTSETUP1OBJECTP_16, VarGLOBTESTSETUP1OBJECT2_17, VarGLOBTESTSETUP1OBJECT2P_18, VarGLOBTESTSETUP1OBJECT3_19, VarGLOBTESTSETUP1OBJECT3P_20, VarGLOBTESTSETUP1OBJECT4_21, VarGLOBTESTSETUP1OBJECT4P_22, VarGLOBTESTSETUP1POINTER_23, VarGLOBTESTSETUP1QWORD_24, Progmain_25, ProgTESTSETUP1BAR_26, VarPARAMTESTSETUP1RECORD_27, VarPARAMTESTSETUP1RECORDP_28, VarPARAMTESTSETUP1CLASS_29, VarPARAMTESTSETUP1CLASSP_30, VarPARAMTESTSETUP1CLASSCHILD_31, VarPARAMTESTSETUP1CLASSCHILDP_32, VarPARAMTESTSETUP1CLASSCLASS_33, VarPARAMTESTSETUP1CLASSCHILDCLASS_34, VarPARAMTESTSETUP1OBJECT_35, VarPARAMTESTSETUP1OBJECTP_36, VarVPARAMTESTSETUP1RECORD_37, VarVPARAMTESTRECORD_38, VarVPARAMTESTSETUP1CLASS_39, VarVPARAMTESTSETUP1CLASSP_40, VarVPARAMTESTSETUP1CLASSCHILD_41, VarVPARAMTESTSETUP1CLASSCHILDP_42, VarVPARAMTESTSETUP1CLASSCLASS_43, VarVPARAMTESTSETUP1CLASSCHILDCLASS_44, VarVPARAMTESTSETUP1OBJECT_45, VarVPARAMTESTSETUP1OBJECTP_46, VarINT1_47, VarPINT1_48, VarBOOL1_49, VarOBJ1_50, VarPOBJ1_51, VarOLDOBJ1_52, VarPOLDOBJ1_53, VarREC1_54, VarPREC1_55, VarREC2_56, VarPI_57, VarPPI_58, VarPPPI_59, VarSUBR_60, VarSUBR2_61, VarSUBR3_62, TypePtr_63, Type_64, TypePtr_65, Type_66, TypePtr_67, Type_68, Type_69, VarFWORD_70, VarFBOOL_71, Type_72, Type_73, Type_74, Type_75, Type_76, TypeChar_77, Type_78, TypeDeclBYTE_79, TypeBYTE_80, Type_81, TypeDeclSHORTINT_82, TypeSHORTINT_83, Type_84, TypeDeclWORD_85, TypeWORD_86, Type_87, TypeDeclLONGINT_88, TypeLONGINT_89, Type_90, TypeDeclQWORD_91, TypeQWord_92, Type_93, TypeDeclBOOLEAN_94, TypeBoolean_95, Type_96, TypeDeclPOINTER_97, TypePtr_98, Type_99, TypeDeclTTESTSETUP1CLASS_100, TypePtr_101, TypeTTESTSETUP1CLASS_102, XX_103, VarFBOOL_104, VarFWORD_105, VarFWORDL_106, VarFINT_107, VarFINTL_108, VarFTEST_109, VarFBYTE_110, ProgCLASSPROC0_111, Varthis_112, VarA_113, Type_114, TypeDeclTTESTSETUP1RECORD_115, TypeTTESTSETUP1RECORD_116, VarFWORD_117, VarFBOOL_118, VarFTEST_119, Type_120, TypeDeclPTESTSETUP1RECORD_121, TypePtr_122, Type_123, TypeDeclTTESTSETUP1RECORD2_124, TypeTTESTSETUP1RECORD2_125, VarFWORD_126, VarFBOOL_127, VarFTEST_128, Type_129, TypeDeclPTESTSETUP1RECORD2_130, TypePtr_131, Type_132, TypeDeclTTESTSETUP1RECORD3_133, TypeTTESTSETUP1RECORD3_134, VarFWORD_135, Type_136, TypeDeclPTESTSETUP1RECORD3_137, TypePtr_138, Type_139, TypeDeclPTESTSETUP1CLASS_140, TypePtr_141, Type_142, TypeDeclTTESTSETUP1CLASSCHILD_143, TypePtr_144, TypeTTESTSETUP1CLASSCHILD_145, XX_146, VarFINT64_147, VarFQWORD_148, Type_149, TypeDeclPTESTSETUP1CLASSCHILD_150, TypePtr_151, Type_152, TypeDeclTTESTSETUP1CLASS2_153, TypePtr_154, TypeTTESTSETUP1CLASS2_155, XX_156, VarFINT_157, VarFWORD_158, Type_159, TypeDeclPTESTSETUP1CLASS2_160, TypePtr_161, Type_162, TypeDeclTTESTSETUP1CLASSCLASS_163, TypePtr_164, Type_165, TypeDeclTTESTSETUP1CLASSCHILDCLASS_166, TypePtr_167, Type_168, TypeDeclTTESTSETUP1OBJECT_169, TypeTTESTSETUP1OBJECT_170, Var_vptrTTESTSETUP1OBJECT_171, VarFWORD_172, VarFWORDL_173, VarFINT_174, VarFINTL_175, VarFBOOL_176, VarFBOOL2_177, VarFBOOL3_178, VarFTEST_179, ProgOBJPROC0_180, Varthis_181, VarO1_182, Type_183, TypeDeclPTESTSETUP1OBJECT_184, TypePtr_185, Type_186, TypeDeclTTESTSETUP1OBJECT2_187, TypeTTESTSETUP1OBJECT2_188, Var_vptrTTESTSETUP1OBJECT2_189, VarFWORD_190, VarFWORDL_191, VarFINT_192, VarFINTL_193, VarFBOOL_194, VarFBOOL2_195, VarFBOOL3_196, VarFTEST_197, ProgOBJPROC1_198, Varthis_199, VarO2_200, Type_201, TypeDeclPTESTSETUP1OBJECT2_202, TypePtr_203, Type_204, TypeDeclTTESTSETUP1OBJECT3_205, TypeTTESTSETUP1OBJECT3_206, Var_vptrTTESTSETUP1OBJECT3_207, VarFWORD_208, ProgOBJPROC1_209, Varthis_210, VarO2_211, Type_212, TypeDeclPTESTSETUP1OBJECT3_213, TypePtr_214, Type_215, TypeDeclTTESTSETUP1OBJECT4_216, TypeTTESTSETUP1OBJECT4_217, VarFWORD_218, Type_219, TypeDeclPTESTSETUP1OBJECT4_220, TypePtr_221, Type_222, TypeDeclPINT_223, TypePtr_224, Type_225, TypeDeclPPINT_226, TypePtr_227, Type_228, TypeDeclPPPINT_229, TypePtr_230, Type_231, TypeDeclPQWORD_232, TypePtr_233, Type_234, TypeDeclTOBJECT_235, TypePtr_236, TypeTOBJECT_237, Var_vptrTOBJECT_238, ProgCREATE_239, Varthis_240, Varvmt_241, ProgDESTROY_242, Varthis_243, Varvmt_244, ProgNEWINSTANCE_245, Varself_246, ProgFREEINSTANCE_247, Varthis_248, ProgSAFECALLEXCEPTION_249, Varthis_250, VarEXCEPTOBJECT_251, VarEXCEPTADDR_252, ProgDEFAULTHANDLER_253, Varthis_254, VarMESSAGE_255, ProgFREE_256, Varthis_257, ProgINITINSTANCE_258, Varself_259, VarINSTANCE_260, ProgCLEANUPINSTANCE_261, Varthis_262, ProgCLASSTYPE_263, Varself_264, ProgCLASSINFO_265, Varself_266, ProgCLASSNAME_267, Varself_268, Varresult_269, ProgCLASSNAMEIS_270, Varself_271, VarNAME_272, ProgCLASSPARENT_273, Varself_274, ProgINSTANCESIZE_275, Varself_276, ProgINHERITSFROM_277, Varself_278, VarACLASS_279, ProgSTRINGMESSAGETABLE_280, Varself_281, ProgMETHODADDRESS_282, Varself_283, VarNAME_284, ProgMETHODNAME_285, Varself_286, VarADDRESS_287, Varresult_288, ProgFIELDADDRESS_289, Varthis_290, VarNAME_291, ProgAFTERCONSTRUCTION_292, Varthis_293, ProgBEFOREDESTRUCTION_294, Varthis_295, ProgDEFAULTHANDLERSTR_296, Varthis_297, VarMESSAGE_298, ProgDISPATCH_299, Varthis_300, VarMESSAGE_301, ProgDISPATCHSTR_302, Varthis_303, VarMESSAGE_304, ProgGETINTERFACE_305, Varthis_306, VarIID_307, VarOBJ_308, ProgGETINTERFACE_309, Varthis_310, VarIIDSTR_311, VarOBJ_312, ProgGETINTERFACEBYSTR_313, Varthis_314, VarIIDSTR_315, VarOBJ_316, ProgGETINTERFACEWEAK_317, Varthis_318, VarIID_319, VarOBJ_320, ProgGETINTERFACEENTRY_321, Varself_322, VarIID_323, ProgGETINTERFACEENTRYBYSTR_324, Varself_325, VarIIDSTR_326, ProgGETINTERFACETABLE_327, Varself_328, ProgUNITNAME_329, Varself_330, Varresult_331, ProgEQUALS_332, Varthis_333, VarOBJ_334, ProgGETHASHCODE_335, Varthis_336, ProgTOSTRING_337, Varthis_338, Varresult_339, Type_340, TypeDeclINT64_341, TypeInt64_342, Type_343, TypeDecl__vtbl_ptr_type_344, Type_345, Type_346, TypeDeclLONGBOOL_347, TypeLongBool_348, Type_349, TypeDeclBYTEBOOL_350, TypeByteBool_351, Type_352, TypePtr_353, Type_354, TypeDeclHRESULT_355, TypeHRESULT_356, Type_357, TypeDeclformal_358, TypeFormalDef_359, Type_360, TypePtr_361, Type_362, TypeDeclTCLASS_363, TypePtr_364, Type_365, TypePtr_366, Type_367, TypePtr_368, Type_369, TypeDeclSHORTSTRING_370, TypeShortString_371, Varlength_372, Varst_373, Type_374, Type_375, Type_376, TypePtr_377, Type_378, TypePtr_379, Type_380, TypePtr_381, Type_382, TypePtr_383, Type_384, TypePtr_385, Type_386, TypeDeclPSTRINGMESSAGETABLE_387, TypePtr_388, Type_389, TypePtr_390, Type_391, TypePtr_392, Type_393, TypePtr_394, Type_395, TypeDeclTGUID_396, TypeTGUID_397, VarDATA1_398, VarDATA2_399, VarDATA3_400, VarDATA4_401, VarD1_402, VarD2_403, VarD3_404, VarD4_405, VarTIME_LOW_406, VarTIME_MID_407, VarTIME_HI_AND_VERSION_408, VarCLOCK_SEQ_HI_AND_RESERVED_409, VarCLOCK_SEQ_LOW_410, VarNODE_411, Type_412, TypeDeclPINTERFACEENTRY_413, TypePtr_414, Type_415, TypePtr_416, Type_417, TypePtr_418, Type_419, TypeDeclPINTERFACETABLE_420, TypePtr_421, Type_422, TypePtr_423, Type_424, TypeDeclANSISTRING_425, TypePtr_426, Type_427, TypePtr_428, Type_429, TypeDeclCHAR_430, TypeChar_431, Type_432, TypeDeclTSTRINGMESSAGETABLE_433, TypeTSTRINGMESSAGETABLE_434, VarCOUNT_435, VarMSGSTRTABLE_436, Type_437, TypeDeclLONGWORD_438, TypeLONGWORD_439, Type_440, Type_441, Type_442, Type_443, Type_444, Type_445, Type_446, Type_447, Type_448, Type_449, TypeDeclTINTERFACEENTRY_450, TypeTINTERFACEENTRY_451, VarIID_452, VarVTABLE_453, VarIOFFSET_454, VarIIDSTR_455, VarITYPE_456, Var__PAD_DUMMY_457, Type_458, TypeDeclTINTERFACETABLE_459, TypeTINTERFACETABLE_460, VarENTRYCOUNT_461, VarENTRIES_462, Type_463, Type_464, Type_465, Type_466, TypeDeclPGUID_467, TypePtr_468, Type_469, TypeDeclPSHORTSTRING_470, TypePtr_471, Type_472, TypeDeclTINTERFACEENTRYTYPE_473, TypeTINTERFACEENTRYTYPE_474, TypeETSTANDARD_475, TypeETVIRTUALMETHODRESULT_476, TypeETSTATICMETHODRESULT_477, TypeETFIELDVALUE_478, TypeETVIRTUALMETHODCLASS_479, TypeETSTATICMETHODCLASS_480, TypeETFIELDVALUECLASS_481, Type_482, Type_483, Type_484, Type_485, TypeDeclTMSGSTRTABLE_486, TypeTMSGSTRTABLE_487, VarNAME_488, VarMETHOD_489, Type_490
    : TTestDwarfInfoEntry;


    // global vars
    GlobTestSetup1: record
      PAD_Before: QWord; // padding will be filled with bad data
      VarRecord, PAD_VarRecord: TTestSetup1Record;
      VarRecordP, PAD_VarRecordP: PTestSetup1Record;
      VarRecord2, PAD_VarRecord2: TTestSetup1Record2;
      VarRecord2P, PAD_VarRecord2P: PTestSetup1Record2;
      VarRecord3, PAD_VarRecord3: TTestSetup1Record3;
      VarRecord3P, PAD_VarRecord3P: PTestSetup1Record3;

      VarClass, PAD_VarClass: TTestSetup1Class;
      VarClassP, PAD_VarClassP: PTestSetup1Class;
      VarClassChild, PAD_VarClassChild: TTestSetup1ClassChild;
      VarClassChildP, PAD_VarClassChildP: PTestSetup1ClassChild;
      VarClass2, PAD_VarClass2: TTestSetup1Class2;
      VarClass2P, PAD_VarClass2P: PTestSetup1Class2;
      VarClassClass, PAD_VarClassClass: TTestSetup1ClassClass;
      VarClassChildClass, PAD_VarClassChildClass: TTestSetup1ClassChildClass;

      VarObject, PAD_VarObject: TTestSetup1Object;
      VarObjectP, PAD_VarObjectP: PTestSetup1Object;
      VarObject2, PAD_VarObject2: TTestSetup1Object2;
      VarObject2P, PAD_VarObject2P: PTestSetup1Object2;
      VarObject3, PAD_VarObject3: TTestSetup1Object3;
      VarObject3P, PAD_VarObject3P: PTestSetup1Object3;
      VarObject4, PAD_VarObject4: TTestSetup1Object4;
      VarObject4P, PAD_VarObject4P: PTestSetup1Object4;

      VarPointer, PAD_VarPointer: Pointer;
      VarQWord, PAD_VarQWord: QWord;

      PAD_After: QWord;
    end;

    // stackframe for "Bar"
    TestStackFrame: record
      PAD_Before: QWord; // padding will be filled with bad data
      // locals
      int1, PAD_int1: Integer;
      pint1, PAD_pint1: ^Integer;
      bool1, PAD_bool1: Boolean;

      Obj1, PAD_Obj1: TTestSetup1Class;
      PObj1, PAD_PObj1: ^TTestSetup1Class;
      OldObj1, PAD_OldObj1: TTestSetup1Object;
      POldObj1, PAD_POldObj1: PTestSetup1Object;
      Rec1, PAD_Rec1: TTestSetup1Record;
      PRec1, PAD_PRec1: ^TTestSetup1Record;
      Rec2, PAD_Rec2: record    FWord: Word;    FBool: Boolean;  end;

      pi, PAD_pi: Pint;
      ppi, PAD_ppi: PPint;
      pppi, PAD_pppi: PPPint;

      subr, PAD_subr: 1..9;
      subr2, PAD_subr2: -11..-9;
      subr3, PAD_subr3: #9..'m';

      // param
      ParamTestSetup1Record, PAD_ParamTestSetup1Record: TTestSetup1Record;
      ParamTestSetup1RecordP, PAD_ParamTestSetup1RecordP: PTestSetup1Record;

      ParamTestSetup1Class, PAD_ParamTestSetup1Class: TTestSetup1Class;
      ParamTestSetup1ClassP, PAD_ParamTestSetup1ClassP: PTestSetup1Class;
      ParamTestSetup1ClassChild, PAD_ParamTestSetup1ClassChild: TTestSetup1ClassChild;
      ParamTestSetup1ClassChildP, PAD_ParamTestSetup1ClassChildP: PTestSetup1ClassChild;
      ParamTestSetup1ClassClass, PAD_ParamTestSetup1ClassClass: TTestSetup1ClassClass;
      ParamTestSetup1ClassChildClass, PAD_ParamTestSetup1ClassChildClass: TTestSetup1ClassChildClass;

      ParamTestSetup1Object, PAD_ParamTestSetup1Object: TTestSetup1Object;
      ParamTestSetup1ObjectP, PAD_ParamTestSetup1ObjectP: PTestSetup1Object;

      // simulate varparam / declare one pointer level
      VParamTestSetup1Record, PAD_VParamTestSetup1Record: ^TTestSetup1Record;
      VParamTestRecord, PAD_VParamTestRecord: ^PTestSetup1Record;

      VParamTestSetup1Class, PAD_VParamTestSetup1Class: ^TTestSetup1Class;
      VParamTestSetup1ClassP, PAD_VParamTestSetup1ClassP: ^PTestSetup1Class;
      VParamTestSetup1ClassChild, PAD_VParamTestSetup1ClassChild: ^TTestSetup1ClassChild;
      VParamTestSetup1ClassChildP, PAD_VParamTestSetup1ClassChildP: ^PTestSetup1ClassChild;
      VParamTestSetup1ClassClass, PAD_VParamTestSetup1ClassClass: ^TTestSetup1ClassClass;
      VParamTestSetup1ClassChildClass, PAD_VParamTestSetup1ClassChildClass: ^TTestSetup1ClassChildClass;

      VParamTestSetup1Object, PAD_VParamTestSetup1Object: ^TTestSetup1Object;
      VParamTestSetup1ObjectP, PAD_VParamTestSetup1ObjectP: ^PTestSetup1Object;

      //
      EndPoint: pointer;
    end;
  end;


implementation

{ TTestSetup1Object3 }

procedure TTestSetup1Object3.ObjProc1(o2: integer);
begin
  FWord := 0;//
end;

{ TTestSetup1Object2 }

procedure TTestSetup1Object2.ObjProc1(o2: integer);
begin
  FWord := 0;//
end;

{ TTestSetup1Object }

procedure TTestSetup1Object.ObjProc0(o1: integer);
begin
  FWord := 0;//
end;

{ TTestSetup1Class }

procedure TTestSetup1Class.ClassProc0(a: integer);
begin
  FWord := 0;//
end;

{ TTestLoaderSetup1 }

constructor TTestLoaderSetup1.Create;
var
  StackOffs: LongInt;
begin
  inherited Create;
  PoissonTestFrame;

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
Unitdwarfsetup1_lpr_0.AddAddr(DW_AT_high_pc, DW_FORM_addr, $004FFFFF);

  VarGLOBTESTSETUP1RECORD_1 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1RECORD_1.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1RECORD_1.Children := 0;
  VarGLOBTESTSETUP1RECORD_1.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1RECORD'+#0);
  VarGLOBTESTSETUP1RECORD_1.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarRECORD)])); // $03, $00, $90, $40, $00
  VarGLOBTESTSETUP1RECORD_1.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD_115); // $3D, $09, $00, $00

  VarGLOBTESTSETUP1RECORDP_2 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1RECORDP_2.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1RECORDP_2.Children := 0;
  VarGLOBTESTSETUP1RECORDP_2.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1RECORDP'+#0);
  VarGLOBTESTSETUP1RECORDP_2.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarRECORDP)])); // $03, $10, $90, $40, $00
  VarGLOBTESTSETUP1RECORDP_2.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1RECORD_121); // $98, $09, $00, $00

  VarGLOBTESTSETUP1RECORD2_3 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1RECORD2_3.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1RECORD2_3.Children := 0;
  VarGLOBTESTSETUP1RECORD2_3.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1RECORD2'+#0);
  VarGLOBTESTSETUP1RECORD2_3.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarRECORD2)])); // $03, $00, $00, $00, $00
  VarGLOBTESTSETUP1RECORD2_3.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD2_124); // $B9, $09, $00, $00

  VarGLOBTESTSETUP1RECORD2P_4 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1RECORD2P_4.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1RECORD2P_4.Children := 0;
  VarGLOBTESTSETUP1RECORD2P_4.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1RECORD2P'+#0);
  VarGLOBTESTSETUP1RECORD2P_4.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarRECORD2P)])); // $03, $00, $00, $00, $00
  VarGLOBTESTSETUP1RECORD2P_4.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1RECORD2_130); // $16, $0A, $00, $00

  VarGLOBTESTSETUP1RECORD3_5 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1RECORD3_5.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1RECORD3_5.Children := 0;
  VarGLOBTESTSETUP1RECORD3_5.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1RECORD3'+#0);
  VarGLOBTESTSETUP1RECORD3_5.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarRECORD3)])); // $03, $00, $00, $00, $00
  VarGLOBTESTSETUP1RECORD3_5.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD3_133); // $38, $0A, $00, $00

  VarGLOBTESTSETUP1RECORD3P_6 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1RECORD3P_6.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1RECORD3P_6.Children := 0;
  VarGLOBTESTSETUP1RECORD3P_6.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1RECORD3P'+#0);
  VarGLOBTESTSETUP1RECORD3P_6.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarRECORD3P)])); // $03, $00, $00, $00, $00
  VarGLOBTESTSETUP1RECORD3P_6.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1RECORD3_137); // $79, $0A, $00, $00

  VarGLOBTESTSETUP1CLASS_7 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1CLASS_7.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1CLASS_7.Children := 0;
  VarGLOBTESTSETUP1CLASS_7.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1CLASS'+#0);
  VarGLOBTESTSETUP1CLASS_7.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarCLASS)])); // $03, $20, $90, $40, $00
  VarGLOBTESTSETUP1CLASS_7.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_100); // $6A, $08, $00, $00

  VarGLOBTESTSETUP1CLASSP_8 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1CLASSP_8.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1CLASSP_8.Children := 0;
  VarGLOBTESTSETUP1CLASSP_8.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1CLASSP'+#0);
  VarGLOBTESTSETUP1CLASSP_8.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarCLASSP)])); // $03, $30, $90, $40, $00
  VarGLOBTESTSETUP1CLASSP_8.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1CLASS_140); // $9B, $0A, $00, $00

  VarGLOBTESTSETUP1CLASSCHILD_9 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1CLASSCHILD_9.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1CLASSCHILD_9.Children := 0;
  VarGLOBTESTSETUP1CLASSCHILD_9.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1CLASSCHILD'+#0);
  VarGLOBTESTSETUP1CLASSCHILD_9.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarCLASSCHILD)])); // $03, $40, $90, $40, $00
  VarGLOBTESTSETUP1CLASSCHILD_9.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCHILD_143); // $BB, $0A, $00, $00

  VarGLOBTESTSETUP1CLASSCHILDP_10 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1CLASSCHILDP_10.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1CLASSCHILDP_10.Children := 0;
  VarGLOBTESTSETUP1CLASSCHILDP_10.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1CLASSCHILDP'+#0);
  VarGLOBTESTSETUP1CLASSCHILDP_10.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarCLASSCHILDP)])); // $03, $50, $90, $40, $00
  VarGLOBTESTSETUP1CLASSCHILDP_10.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1CLASSCHILD_150); // $20, $0B, $00, $00

  VarGLOBTESTSETUP1CLASS2_11 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1CLASS2_11.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1CLASS2_11.Children := 0;
  VarGLOBTESTSETUP1CLASS2_11.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1CLASS2'+#0);
  VarGLOBTESTSETUP1CLASS2_11.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarCLASS2)])); // $03, $00, $00, $00, $00
  VarGLOBTESTSETUP1CLASS2_11.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS2_153); // $45, $0B, $00, $00

  VarGLOBTESTSETUP1CLASS2P_12 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1CLASS2P_12.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1CLASS2P_12.Children := 0;
  VarGLOBTESTSETUP1CLASS2P_12.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1CLASS2P'+#0);
  VarGLOBTESTSETUP1CLASS2P_12.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarCLASS2P)])); // $03, $00, $00, $00, $00
  VarGLOBTESTSETUP1CLASS2P_12.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1CLASS2_160); // $9F, $0B, $00, $00

  VarGLOBTESTSETUP1CLASSCLASS_13 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1CLASSCLASS_13.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1CLASSCLASS_13.Children := 0;
  VarGLOBTESTSETUP1CLASSCLASS_13.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1CLASSCLASS'+#0);
  VarGLOBTESTSETUP1CLASSCLASS_13.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarCLASSCLASS)])); // $03, $60, $90, $40, $00
  VarGLOBTESTSETUP1CLASSCLASS_13.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCLASS_163); // $C0, $0B, $00, $00

  VarGLOBTESTSETUP1CLASSCHILDCLASS_14 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1CLASSCHILDCLASS_14.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1CLASSCHILDCLASS_14.Children := 0;
  VarGLOBTESTSETUP1CLASSCHILDCLASS_14.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1CLASSCHILDCLASS'+#0);
  VarGLOBTESTSETUP1CLASSCHILDCLASS_14.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarCLASSCHILDCLASS)])); // $03, $70, $90, $40, $00
  VarGLOBTESTSETUP1CLASSCHILDCLASS_14.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCHILDCLASS_166); // $E5, $0B, $00, $00

  VarGLOBTESTSETUP1OBJECT_15 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1OBJECT_15.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1OBJECT_15.Children := 0;
  VarGLOBTESTSETUP1OBJECT_15.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1OBJECT'+#0);
  VarGLOBTESTSETUP1OBJECT_15.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarOBJECT)])); // $03, $80, $90, $40, $00
  VarGLOBTESTSETUP1OBJECT_15.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_169); // $0F, $0C, $00, $00

  VarGLOBTESTSETUP1OBJECTP_16 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1OBJECTP_16.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1OBJECTP_16.Children := 0;
  VarGLOBTESTSETUP1OBJECTP_16.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1OBJECTP'+#0);
  VarGLOBTESTSETUP1OBJECTP_16.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarOBJECTP)])); // $03, $B0, $90, $40, $00
  VarGLOBTESTSETUP1OBJECTP_16.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1OBJECT_184); // $07, $0D, $00, $00

  VarGLOBTESTSETUP1OBJECT2_17 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1OBJECT2_17.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1OBJECT2_17.Children := 0;
  VarGLOBTESTSETUP1OBJECT2_17.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1OBJECT2'+#0);
  VarGLOBTESTSETUP1OBJECT2_17.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarOBJECT2)])); // $03, $00, $00, $00, $00
  VarGLOBTESTSETUP1OBJECT2_17.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT2_187); // $28, $0D, $00, $00

  VarGLOBTESTSETUP1OBJECT2P_18 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1OBJECT2P_18.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1OBJECT2P_18.Children := 0;
  VarGLOBTESTSETUP1OBJECT2P_18.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1OBJECT2P'+#0);
  VarGLOBTESTSETUP1OBJECT2P_18.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarOBJECT2P)])); // $03, $00, $00, $00, $00
  VarGLOBTESTSETUP1OBJECT2P_18.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1OBJECT2_202); // $23, $0E, $00, $00

  VarGLOBTESTSETUP1OBJECT3_19 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1OBJECT3_19.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1OBJECT3_19.Children := 0;
  VarGLOBTESTSETUP1OBJECT3_19.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1OBJECT3'+#0);
  VarGLOBTESTSETUP1OBJECT3_19.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarOBJECT3)])); // $03, $00, $00, $00, $00
  VarGLOBTESTSETUP1OBJECT3_19.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT3_205); // $45, $0E, $00, $00

  VarGLOBTESTSETUP1OBJECT3P_20 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1OBJECT3P_20.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1OBJECT3P_20.Children := 0;
  VarGLOBTESTSETUP1OBJECT3P_20.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1OBJECT3P'+#0);
  VarGLOBTESTSETUP1OBJECT3P_20.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarOBJECT3P)])); // $03, $00, $00, $00, $00
  VarGLOBTESTSETUP1OBJECT3P_20.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1OBJECT3_213); // $DC, $0E, $00, $00

  VarGLOBTESTSETUP1OBJECT4_21 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1OBJECT4_21.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1OBJECT4_21.Children := 0;
  VarGLOBTESTSETUP1OBJECT4_21.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1OBJECT4'+#0);
  VarGLOBTESTSETUP1OBJECT4_21.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarOBJECT4)])); // $03, $00, $00, $00, $00
  VarGLOBTESTSETUP1OBJECT4_21.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT4_216); // $FE, $0E, $00, $00

  VarGLOBTESTSETUP1OBJECT4P_22 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1OBJECT4P_22.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1OBJECT4P_22.Children := 0;
  VarGLOBTESTSETUP1OBJECT4P_22.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1OBJECT4P'+#0);
  VarGLOBTESTSETUP1OBJECT4P_22.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarOBJECT4P)])); // $03, $00, $00, $00, $00
  VarGLOBTESTSETUP1OBJECT4P_22.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1OBJECT4_220); // $3F, $0F, $00, $00

  VarGLOBTESTSETUP1POINTER_23 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1POINTER_23.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1POINTER_23.Children := 0;
  VarGLOBTESTSETUP1POINTER_23.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1POINTER'+#0);
  VarGLOBTESTSETUP1POINTER_23.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarPOINTER)])); // $03, $00, $00, $00, $00
  VarGLOBTESTSETUP1POINTER_23.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_97); // $57, $08, $00, $00

  VarGLOBTESTSETUP1QWORD_24 := Unitdwarfsetup1_lpr_0.GetNewChild;
  VarGLOBTESTSETUP1QWORD_24.Tag := DW_TAG_variable;
  VarGLOBTESTSETUP1QWORD_24.Children := 0;
  VarGLOBTESTSETUP1QWORD_24.Add(DW_AT_name, DW_FORM_string, 'GLOBTESTSETUP1QWORD'+#0);
  VarGLOBTESTSETUP1QWORD_24.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobTestSetup1.VarQWORD)])); // $03, $00, $00, $00, $00
  VarGLOBTESTSETUP1QWORD_24.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_91); // $21, $08, $00, $00

  Progmain_25 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Progmain_25.Tag := DW_TAG_subprogram;
  Progmain_25.Children := 0;
  Progmain_25.Add(DW_AT_name, DW_FORM_string, 'main'+#0);
  Progmain_25.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
  Progmain_25.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
  Progmain_25.Add(DW_AT_external, DW_FORM_flag, [$01]);
  Progmain_25.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00400000);
  Progmain_25.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00400FFF);

  ProgTESTSETUP1BAR_26 := Unitdwarfsetup1_lpr_0.GetNewChild;
  ProgTESTSETUP1BAR_26.Tag := DW_TAG_subprogram;
  ProgTESTSETUP1BAR_26.Children := 1;
  ProgTESTSETUP1BAR_26.Add(DW_AT_name, DW_FORM_string, 'TESTSETUP1BAR'+#0);
  ProgTESTSETUP1BAR_26.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
  ProgTESTSETUP1BAR_26.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
  ProgTESTSETUP1BAR_26.Add(DW_AT_external, DW_FORM_flag, [$01]);
  ProgTESTSETUP1BAR_26.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_97); // $57, $08, $00, $00
  ProgTESTSETUP1BAR_26.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00401000);
  ProgTESTSETUP1BAR_26.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00401FFF);

    StackOffs := @TestStackFrame.PARAMTESTSETUP1RECORD - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1RECORD_27 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarPARAMTESTSETUP1RECORD_27.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1RECORD_27.Children := 0;
    VarPARAMTESTSETUP1RECORD_27.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1RECORD'+#0);
    VarPARAMTESTSETUP1RECORD_27.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $84, $7F
    VarPARAMTESTSETUP1RECORD_27.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD_115); // $3D, $09, $00, $00

    StackOffs := @TestStackFrame.PARAMTESTSETUP1RECORDP - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1RECORDP_28 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarPARAMTESTSETUP1RECORDP_28.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1RECORDP_28.Children := 0;
    VarPARAMTESTSETUP1RECORDP_28.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1RECORDP'+#0);
    VarPARAMTESTSETUP1RECORDP_28.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $78
    VarPARAMTESTSETUP1RECORDP_28.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1RECORD_121); // $98, $09, $00, $00

    StackOffs := @TestStackFrame.PARAMTESTSETUP1CLASS - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1CLASS_29 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarPARAMTESTSETUP1CLASS_29.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1CLASS_29.Children := 0;
    VarPARAMTESTSETUP1CLASS_29.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1CLASS'+#0);
    VarPARAMTESTSETUP1CLASS_29.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $74
    VarPARAMTESTSETUP1CLASS_29.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_100); // $6A, $08, $00, $00

    StackOffs := @TestStackFrame.PARAMTESTSETUP1CLASSP - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1CLASSP_30 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarPARAMTESTSETUP1CLASSP_30.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1CLASSP_30.Children := 0;
    VarPARAMTESTSETUP1CLASSP_30.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1CLASSP'+#0);
    VarPARAMTESTSETUP1CLASSP_30.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $C8, $00
    VarPARAMTESTSETUP1CLASSP_30.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1CLASS_140); // $9B, $0A, $00, $00

    StackOffs := @TestStackFrame.PARAMTESTSETUP1CLASSCHILD - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1CLASSCHILD_31 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarPARAMTESTSETUP1CLASSCHILD_31.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1CLASSCHILD_31.Children := 0;
    VarPARAMTESTSETUP1CLASSCHILD_31.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1CLASSCHILD'+#0);
    VarPARAMTESTSETUP1CLASSCHILD_31.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $C4, $00
    VarPARAMTESTSETUP1CLASSCHILD_31.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCHILD_143); // $BB, $0A, $00, $00

    StackOffs := @TestStackFrame.PARAMTESTSETUP1CLASSCHILDP - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1CLASSCHILDP_32 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarPARAMTESTSETUP1CLASSCHILDP_32.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1CLASSCHILDP_32.Children := 0;
    VarPARAMTESTSETUP1CLASSCHILDP_32.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1CLASSCHILDP'+#0);
    VarPARAMTESTSETUP1CLASSCHILDP_32.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $C0, $00
    VarPARAMTESTSETUP1CLASSCHILDP_32.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1CLASSCHILD_150); // $20, $0B, $00, $00

    StackOffs := @TestStackFrame.PARAMTESTSETUP1CLASSCLASS - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1CLASSCLASS_33 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarPARAMTESTSETUP1CLASSCLASS_33.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1CLASSCLASS_33.Children := 0;
    VarPARAMTESTSETUP1CLASSCLASS_33.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1CLASSCLASS'+#0);
    VarPARAMTESTSETUP1CLASSCLASS_33.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $3C
    VarPARAMTESTSETUP1CLASSCLASS_33.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCLASS_163); // $C0, $0B, $00, $00

    StackOffs := @TestStackFrame.PARAMTESTSETUP1CLASSCHILDCLASS - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1CLASSCHILDCLASS_34 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarPARAMTESTSETUP1CLASSCHILDCLASS_34.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1CLASSCHILDCLASS_34.Children := 0;
    VarPARAMTESTSETUP1CLASSCHILDCLASS_34.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1CLASSCHILDCLASS'+#0);
    VarPARAMTESTSETUP1CLASSCHILDCLASS_34.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $38
    VarPARAMTESTSETUP1CLASSCHILDCLASS_34.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCHILDCLASS_166); // $E5, $0B, $00, $00

    StackOffs := @TestStackFrame.PARAMTESTSETUP1OBJECT - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1OBJECT_35 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarPARAMTESTSETUP1OBJECT_35.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1OBJECT_35.Children := 0;
    VarPARAMTESTSETUP1OBJECT_35.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1OBJECT'+#0);
    VarPARAMTESTSETUP1OBJECT_35.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $E0, $7E
    VarPARAMTESTSETUP1OBJECT_35.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_169); // $0F, $0C, $00, $00

    StackOffs := @TestStackFrame.PARAMTESTSETUP1OBJECTP - @TestStackFrame.EndPoint;
    VarPARAMTESTSETUP1OBJECTP_36 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarPARAMTESTSETUP1OBJECTP_36.Tag := DW_TAG_formal_parameter;
    VarPARAMTESTSETUP1OBJECTP_36.Children := 0;
    VarPARAMTESTSETUP1OBJECTP_36.Add(DW_AT_name, DW_FORM_string, 'PARAMTESTSETUP1OBJECTP'+#0);
    VarPARAMTESTSETUP1OBJECTP_36.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $30
    VarPARAMTESTSETUP1OBJECTP_36.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1OBJECT_184); // $07, $0D, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTSETUP1RECORD - @TestStackFrame.EndPoint;
    VarVPARAMTESTSETUP1RECORD_37 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarVPARAMTESTSETUP1RECORD_37.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTSETUP1RECORD_37.Children := 0;
    VarVPARAMTESTSETUP1RECORD_37.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTSETUP1RECORD'+#0);
    VarVPARAMTESTSETUP1RECORD_37.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $2C, $06
    VarVPARAMTESTSETUP1RECORD_37.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD_115); // $3D, $09, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTRECORD - @TestStackFrame.EndPoint;
    VarVPARAMTESTRECORD_38 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarVPARAMTESTRECORD_38.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTRECORD_38.Children := 0;
    VarVPARAMTESTRECORD_38.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTRECORD'+#0);
    VarVPARAMTESTRECORD_38.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $28, $06
    VarVPARAMTESTRECORD_38.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1RECORD_121); // $98, $09, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTSETUP1CLASS - @TestStackFrame.EndPoint;
    VarVPARAMTESTSETUP1CLASS_39 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarVPARAMTESTSETUP1CLASS_39.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTSETUP1CLASS_39.Children := 0;
    VarVPARAMTESTSETUP1CLASS_39.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTSETUP1CLASS'+#0);
    VarVPARAMTESTSETUP1CLASS_39.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $24, $06
    VarVPARAMTESTSETUP1CLASS_39.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_100); // $6A, $08, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTSETUP1CLASSP - @TestStackFrame.EndPoint;
    VarVPARAMTESTSETUP1CLASSP_40 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarVPARAMTESTSETUP1CLASSP_40.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTSETUP1CLASSP_40.Children := 0;
    VarVPARAMTESTSETUP1CLASSP_40.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTSETUP1CLASSP'+#0);
    VarVPARAMTESTSETUP1CLASSP_40.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $20, $06
    VarVPARAMTESTSETUP1CLASSP_40.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1CLASS_140); // $9B, $0A, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTSETUP1CLASSCHILD - @TestStackFrame.EndPoint;
    VarVPARAMTESTSETUP1CLASSCHILD_41 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarVPARAMTESTSETUP1CLASSCHILD_41.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTSETUP1CLASSCHILD_41.Children := 0;
    VarVPARAMTESTSETUP1CLASSCHILD_41.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTSETUP1CLASSCHILD'+#0);
    VarVPARAMTESTSETUP1CLASSCHILD_41.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $1C, $06
    VarVPARAMTESTSETUP1CLASSCHILD_41.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCHILD_143); // $BB, $0A, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTSETUP1CLASSCHILDP - @TestStackFrame.EndPoint;
    VarVPARAMTESTSETUP1CLASSCHILDP_42 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarVPARAMTESTSETUP1CLASSCHILDP_42.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTSETUP1CLASSCHILDP_42.Children := 0;
    VarVPARAMTESTSETUP1CLASSCHILDP_42.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTSETUP1CLASSCHILDP'+#0);
    VarVPARAMTESTSETUP1CLASSCHILDP_42.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $18, $06
    VarVPARAMTESTSETUP1CLASSCHILDP_42.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1CLASSCHILD_150); // $20, $0B, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTSETUP1CLASSCLASS - @TestStackFrame.EndPoint;
    VarVPARAMTESTSETUP1CLASSCLASS_43 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarVPARAMTESTSETUP1CLASSCLASS_43.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTSETUP1CLASSCLASS_43.Children := 0;
    VarVPARAMTESTSETUP1CLASSCLASS_43.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTSETUP1CLASSCLASS'+#0);
    VarVPARAMTESTSETUP1CLASSCLASS_43.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $14, $06
    VarVPARAMTESTSETUP1CLASSCLASS_43.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCLASS_163); // $C0, $0B, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTSETUP1CLASSCHILDCLASS - @TestStackFrame.EndPoint;
    VarVPARAMTESTSETUP1CLASSCHILDCLASS_44 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarVPARAMTESTSETUP1CLASSCHILDCLASS_44.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTSETUP1CLASSCHILDCLASS_44.Children := 0;
    VarVPARAMTESTSETUP1CLASSCHILDCLASS_44.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTSETUP1CLASSCHILDCLASS'+#0);
    VarVPARAMTESTSETUP1CLASSCHILDCLASS_44.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $10, $06
    VarVPARAMTESTSETUP1CLASSCHILDCLASS_44.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCHILDCLASS_166); // $E5, $0B, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTSETUP1OBJECT - @TestStackFrame.EndPoint;
    VarVPARAMTESTSETUP1OBJECT_45 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarVPARAMTESTSETUP1OBJECT_45.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTSETUP1OBJECT_45.Children := 0;
    VarVPARAMTESTSETUP1OBJECT_45.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTSETUP1OBJECT'+#0);
    VarVPARAMTESTSETUP1OBJECT_45.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $0C, $06
    VarVPARAMTESTSETUP1OBJECT_45.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_169); // $0F, $0C, $00, $00

    StackOffs := @TestStackFrame.VPARAMTESTSETUP1OBJECTP - @TestStackFrame.EndPoint;
    VarVPARAMTESTSETUP1OBJECTP_46 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarVPARAMTESTSETUP1OBJECTP_46.Tag := DW_TAG_formal_parameter;
    VarVPARAMTESTSETUP1OBJECTP_46.Children := 0;
    VarVPARAMTESTSETUP1OBJECTP_46.Add(DW_AT_name, DW_FORM_string, 'VPARAMTESTSETUP1OBJECTP'+#0);
    VarVPARAMTESTSETUP1OBJECTP_46.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs), DW_OP_deref])); // $75, $08, $06
    VarVPARAMTESTSETUP1OBJECTP_46.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1OBJECT_184); // $07, $0D, $00, $00

    StackOffs := @TestStackFrame.INT1 - @TestStackFrame.EndPoint;
    VarINT1_47 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarINT1_47.Tag := DW_TAG_variable;
    VarINT1_47.Children := 0;
    VarINT1_47.Add(DW_AT_name, DW_FORM_string, 'INT1'+#0);
    VarINT1_47.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $6C
    VarINT1_47.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_88); // $04, $08, $00, $00

    StackOffs := @TestStackFrame.PINT1 - @TestStackFrame.EndPoint;
    VarPINT1_48 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarPINT1_48.Tag := DW_TAG_variable;
    VarPINT1_48.Children := 0;
    VarPINT1_48.Add(DW_AT_name, DW_FORM_string, 'PINT1'+#0);
    VarPINT1_48.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $68
    VarPINT1_48.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_63); // $4F, $07, $00, $00

    StackOffs := @TestStackFrame.BOOL1 - @TestStackFrame.EndPoint;
    VarBOOL1_49 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarBOOL1_49.Tag := DW_TAG_variable;
    VarBOOL1_49.Children := 0;
    VarBOOL1_49.Add(DW_AT_name, DW_FORM_string, 'BOOL1'+#0);
    VarBOOL1_49.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $64
    VarBOOL1_49.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_94); // $3A, $08, $00, $00

    StackOffs := @TestStackFrame.OBJ1 - @TestStackFrame.EndPoint;
    VarOBJ1_50 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarOBJ1_50.Tag := DW_TAG_variable;
    VarOBJ1_50.Children := 0;
    VarOBJ1_50.Add(DW_AT_name, DW_FORM_string, 'OBJ1'+#0);
    VarOBJ1_50.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $60
    VarOBJ1_50.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_100); // $6A, $08, $00, $00

    StackOffs := @TestStackFrame.POBJ1 - @TestStackFrame.EndPoint;
    VarPOBJ1_51 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarPOBJ1_51.Tag := DW_TAG_variable;
    VarPOBJ1_51.Children := 0;
    VarPOBJ1_51.Add(DW_AT_name, DW_FORM_string, 'POBJ1'+#0);
    VarPOBJ1_51.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $5C
    VarPOBJ1_51.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_65); // $59, $07, $00, $00

    StackOffs := @TestStackFrame.OLDOBJ1 - @TestStackFrame.EndPoint;
    VarOLDOBJ1_52 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarOLDOBJ1_52.Tag := DW_TAG_variable;
    VarOLDOBJ1_52.Children := 0;
    VarOLDOBJ1_52.Add(DW_AT_name, DW_FORM_string, 'OLDOBJ1'+#0);
    VarOLDOBJ1_52.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $B8, $7F
    VarOLDOBJ1_52.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_169); // $0F, $0C, $00, $00

    StackOffs := @TestStackFrame.POLDOBJ1 - @TestStackFrame.EndPoint;
    VarPOLDOBJ1_53 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarPOLDOBJ1_53.Tag := DW_TAG_variable;
    VarPOLDOBJ1_53.Children := 0;
    VarPOLDOBJ1_53.Add(DW_AT_name, DW_FORM_string, 'POLDOBJ1'+#0);
    VarPOLDOBJ1_53.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $B4, $7F
    VarPOLDOBJ1_53.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1OBJECT_184); // $07, $0D, $00, $00

    StackOffs := @TestStackFrame.REC1 - @TestStackFrame.EndPoint;
    VarREC1_54 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarREC1_54.Tag := DW_TAG_variable;
    VarREC1_54.Children := 0;
    VarREC1_54.Add(DW_AT_name, DW_FORM_string, 'REC1'+#0);
    VarREC1_54.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $AC, $7F
    VarREC1_54.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD_115); // $3D, $09, $00, $00

    StackOffs := @TestStackFrame.PREC1 - @TestStackFrame.EndPoint;
    VarPREC1_55 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarPREC1_55.Tag := DW_TAG_variable;
    VarPREC1_55.Children := 0;
    VarPREC1_55.Add(DW_AT_name, DW_FORM_string, 'PREC1'+#0);
    VarPREC1_55.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $A8, $7F
    VarPREC1_55.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_67); // $63, $07, $00, $00

    StackOffs := @TestStackFrame.REC2 - @TestStackFrame.EndPoint;
    VarREC2_56 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarREC2_56.Tag := DW_TAG_variable;
    VarREC2_56.Children := 0;
    VarREC2_56.Add(DW_AT_name, DW_FORM_string, 'REC2'+#0);
    VarREC2_56.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $A4, $7F
    VarREC2_56.AddRef(DW_AT_type, DW_FORM_ref4, @Type_69); // $6D, $07, $00, $00

    StackOffs := @TestStackFrame.PI - @TestStackFrame.EndPoint;
    VarPI_57 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarPI_57.Tag := DW_TAG_variable;
    VarPI_57.Children := 0;
    VarPI_57.Add(DW_AT_name, DW_FORM_string, 'PI'+#0);
    VarPI_57.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $A0, $7F
    VarPI_57.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINT_223); // $61, $0F, $00, $00

    StackOffs := @TestStackFrame.PPI - @TestStackFrame.EndPoint;
    VarPPI_58 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarPPI_58.Tag := DW_TAG_variable;
    VarPPI_58.Children := 0;
    VarPPI_58.Add(DW_AT_name, DW_FORM_string, 'PPI'+#0);
    VarPPI_58.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $9C, $7F
    VarPPI_58.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPPINT_226); // $75, $0F, $00, $00

    StackOffs := @TestStackFrame.PPPI - @TestStackFrame.EndPoint;
    VarPPPI_59 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarPPPI_59.Tag := DW_TAG_variable;
    VarPPPI_59.Children := 0;
    VarPPPI_59.Add(DW_AT_name, DW_FORM_string, 'PPPI'+#0);
    VarPPPI_59.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $98, $7F
    VarPPPI_59.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPPPINT_229); // $8A, $0F, $00, $00

    StackOffs := @TestStackFrame.SUBR - @TestStackFrame.EndPoint;
    VarSUBR_60 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarSUBR_60.Tag := DW_TAG_variable;
    VarSUBR_60.Children := 0;
    VarSUBR_60.Add(DW_AT_name, DW_FORM_string, 'SUBR'+#0);
    VarSUBR_60.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $94, $7F
    VarSUBR_60.AddRef(DW_AT_type, DW_FORM_ref4, @Type_73); // $91, $07, $00, $00

    StackOffs := @TestStackFrame.SUBR2 - @TestStackFrame.EndPoint;
    VarSUBR2_61 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarSUBR2_61.Tag := DW_TAG_variable;
    VarSUBR2_61.Children := 0;
    VarSUBR2_61.Add(DW_AT_name, DW_FORM_string, 'SUBR2'+#0);
    VarSUBR2_61.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $90, $7F
    VarSUBR2_61.AddRef(DW_AT_type, DW_FORM_ref4, @Type_75); // $9D, $07, $00, $00

    StackOffs := @TestStackFrame.SUBR3 - @TestStackFrame.EndPoint;
    VarSUBR3_62 := ProgTESTSETUP1BAR_26.GetNewChild;
    VarSUBR3_62.Tag := DW_TAG_variable;
    VarSUBR3_62.Children := 0;
    VarSUBR3_62.Add(DW_AT_name, DW_FORM_string, 'SUBR3'+#0);
    VarSUBR3_62.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $8C, $7F
    VarSUBR3_62.AddRef(DW_AT_type, DW_FORM_ref4, @TypeChar_77); // $A9, $07, $00, $00

    TypePtr_63 := ProgTESTSETUP1BAR_26.GetNewChild;
    TypePtr_63.Tag := DW_TAG_pointer_type;
    TypePtr_63.Children := 0;
    TypePtr_63.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_88); // $04, $08, $00, $00

    Type_64 := ProgTESTSETUP1BAR_26.GetNewChild;
    Type_64.Tag := DW_TAG_reference_type;
    Type_64.Children := 0;
    Type_64.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_63); // $4F, $07, $00, $00

    TypePtr_65 := ProgTESTSETUP1BAR_26.GetNewChild;
    TypePtr_65.Tag := DW_TAG_pointer_type;
    TypePtr_65.Children := 0;
    TypePtr_65.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_100); // $6A, $08, $00, $00

    Type_66 := ProgTESTSETUP1BAR_26.GetNewChild;
    Type_66.Tag := DW_TAG_reference_type;
    Type_66.Children := 0;
    Type_66.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_65); // $59, $07, $00, $00

    TypePtr_67 := ProgTESTSETUP1BAR_26.GetNewChild;
    TypePtr_67.Tag := DW_TAG_pointer_type;
    TypePtr_67.Children := 0;
    TypePtr_67.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD_115); // $3D, $09, $00, $00

    Type_68 := ProgTESTSETUP1BAR_26.GetNewChild;
    Type_68.Tag := DW_TAG_reference_type;
    Type_68.Children := 0;
    Type_68.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_67); // $63, $07, $00, $00

    Type_69 := ProgTESTSETUP1BAR_26.GetNewChild;
    Type_69.Tag := DW_TAG_structure_type;
    Type_69.Children := 1;
    Type_69.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 4);

      VarFWORD_70 := Type_69.GetNewChild;
      VarFWORD_70.Tag := DW_TAG_member;
      VarFWORD_70.Children := 0;
      VarFWORD_70.Add(DW_AT_name, DW_FORM_string, 'FWORD'+#0);
      VarFWORD_70.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
      VarFWORD_70.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_85); // $ED, $07, $00, $00

      VarFBOOL_71 := Type_69.GetNewChild;
      VarFBOOL_71.Tag := DW_TAG_member;
      VarFBOOL_71.Children := 0;
      VarFBOOL_71.Add(DW_AT_name, DW_FORM_string, 'FBOOL'+#0);
      VarFBOOL_71.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(2)])); // $23, $02
      VarFBOOL_71.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_94); // $3A, $08, $00, $00

    Type_72 := ProgTESTSETUP1BAR_26.GetNewChild;
    Type_72.Tag := DW_TAG_reference_type;
    Type_72.Children := 0;
    Type_72.AddRef(DW_AT_type, DW_FORM_ref4, @Type_69); // $6D, $07, $00, $00

    Type_73 := ProgTESTSETUP1BAR_26.GetNewChild;
    Type_73.Tag := DW_TAG_subrange_type;
    Type_73.Children := 0;
    Type_73.AddSLEB(DW_AT_lower_bound, DW_FORM_udata, 1);
    Type_73.AddSLEB(DW_AT_upper_bound, DW_FORM_udata, 9);
    Type_73.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_79); // $B7, $07, $00, $00

    Type_74 := ProgTESTSETUP1BAR_26.GetNewChild;
    Type_74.Tag := DW_TAG_reference_type;
    Type_74.Children := 0;
    Type_74.AddRef(DW_AT_type, DW_FORM_ref4, @Type_73); // $91, $07, $00, $00

    Type_75 := ProgTESTSETUP1BAR_26.GetNewChild;
    Type_75.Tag := DW_TAG_subrange_type;
    Type_75.Children := 0;
    Type_75.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 117);
    Type_75.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 119);
    Type_75.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_82); // $CE, $07, $00, $00

    Type_76 := ProgTESTSETUP1BAR_26.GetNewChild;
    Type_76.Tag := DW_TAG_reference_type;
    Type_76.Children := 0;
    Type_76.AddRef(DW_AT_type, DW_FORM_ref4, @Type_75); // $9D, $07, $00, $00

    TypeChar_77 := ProgTESTSETUP1BAR_26.GetNewChild;
    TypeChar_77.Tag := DW_TAG_base_type;
    TypeChar_77.Children := 0;
    TypeChar_77.Add(DW_AT_name, DW_FORM_string, 'Char'+#0);
    TypeChar_77.Add(DW_AT_encoding, DW_FORM_data1, [$08]);
    TypeChar_77.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

    Type_78 := ProgTESTSETUP1BAR_26.GetNewChild;
    Type_78.Tag := DW_TAG_reference_type;
    Type_78.Children := 0;
    Type_78.AddRef(DW_AT_type, DW_FORM_ref4, @TypeChar_77); // $A9, $07, $00, $00

  TypeDeclBYTE_79 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclBYTE_79.Tag := DW_TAG_typedef;
  TypeDeclBYTE_79.Children := 0;
  TypeDeclBYTE_79.Add(DW_AT_name, DW_FORM_string, 'BYTE'+#0);
  TypeDeclBYTE_79.AddRef(DW_AT_type, DW_FORM_ref4, @TypeBYTE_80); // $C1, $07, $00, $00

  TypeBYTE_80 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeBYTE_80.Tag := DW_TAG_base_type;
  TypeBYTE_80.Children := 0;
  TypeBYTE_80.Add(DW_AT_name, DW_FORM_string, 'BYTE'+#0);
  TypeBYTE_80.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeBYTE_80.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_81 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_81.Tag := DW_TAG_reference_type;
  Type_81.Children := 0;
  Type_81.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_79); // $B7, $07, $00, $00

  TypeDeclSHORTINT_82 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclSHORTINT_82.Tag := DW_TAG_typedef;
  TypeDeclSHORTINT_82.Children := 0;
  TypeDeclSHORTINT_82.Add(DW_AT_name, DW_FORM_string, 'SHORTINT'+#0);
  TypeDeclSHORTINT_82.AddRef(DW_AT_type, DW_FORM_ref4, @TypeSHORTINT_83); // $DC, $07, $00, $00

  TypeSHORTINT_83 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeSHORTINT_83.Tag := DW_TAG_base_type;
  TypeSHORTINT_83.Children := 0;
  TypeSHORTINT_83.Add(DW_AT_name, DW_FORM_string, 'SHORTINT'+#0);
  TypeSHORTINT_83.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeSHORTINT_83.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_84 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_84.Tag := DW_TAG_reference_type;
  Type_84.Children := 0;
  Type_84.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_82); // $CE, $07, $00, $00

  TypeDeclWORD_85 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclWORD_85.Tag := DW_TAG_typedef;
  TypeDeclWORD_85.Children := 0;
  TypeDeclWORD_85.Add(DW_AT_name, DW_FORM_string, 'WORD'+#0);
  TypeDeclWORD_85.AddRef(DW_AT_type, DW_FORM_ref4, @TypeWORD_86); // $F7, $07, $00, $00

  TypeWORD_86 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeWORD_86.Tag := DW_TAG_base_type;
  TypeWORD_86.Children := 0;
  TypeWORD_86.Add(DW_AT_name, DW_FORM_string, 'WORD'+#0);
  TypeWORD_86.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeWORD_86.Add(DW_AT_byte_size, DW_FORM_data1, [$02]);

  Type_87 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_87.Tag := DW_TAG_reference_type;
  Type_87.Children := 0;
  Type_87.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_85); // $ED, $07, $00, $00

  TypeDeclLONGINT_88 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclLONGINT_88.Tag := DW_TAG_typedef;
  TypeDeclLONGINT_88.Children := 0;
  TypeDeclLONGINT_88.Add(DW_AT_name, DW_FORM_string, 'LONGINT'+#0);
  TypeDeclLONGINT_88.AddRef(DW_AT_type, DW_FORM_ref4, @TypeLONGINT_89); // $11, $08, $00, $00

  TypeLONGINT_89 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeLONGINT_89.Tag := DW_TAG_base_type;
  TypeLONGINT_89.Children := 0;
  TypeLONGINT_89.Add(DW_AT_name, DW_FORM_string, 'LONGINT'+#0);
  TypeLONGINT_89.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeLONGINT_89.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

  Type_90 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_90.Tag := DW_TAG_reference_type;
  Type_90.Children := 0;
  Type_90.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_88); // $04, $08, $00, $00

  TypeDeclQWORD_91 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclQWORD_91.Tag := DW_TAG_typedef;
  TypeDeclQWORD_91.Children := 0;
  TypeDeclQWORD_91.Add(DW_AT_name, DW_FORM_string, 'QWORD'+#0);
  TypeDeclQWORD_91.AddRef(DW_AT_type, DW_FORM_ref4, @TypeQWord_92); // $2C, $08, $00, $00

  TypeQWord_92 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeQWord_92.Tag := DW_TAG_base_type;
  TypeQWord_92.Children := 0;
  TypeQWord_92.Add(DW_AT_name, DW_FORM_string, 'QWord'+#0);
  TypeQWord_92.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeQWord_92.Add(DW_AT_byte_size, DW_FORM_data1, [$08]);

  Type_93 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_93.Tag := DW_TAG_reference_type;
  Type_93.Children := 0;
  Type_93.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_91); // $21, $08, $00, $00

  TypeDeclBOOLEAN_94 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclBOOLEAN_94.Tag := DW_TAG_typedef;
  TypeDeclBOOLEAN_94.Children := 0;
  TypeDeclBOOLEAN_94.Add(DW_AT_name, DW_FORM_string, 'BOOLEAN'+#0);
  TypeDeclBOOLEAN_94.AddRef(DW_AT_type, DW_FORM_ref4, @TypeBoolean_95); // $47, $08, $00, $00

  TypeBoolean_95 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeBoolean_95.Tag := DW_TAG_base_type;
  TypeBoolean_95.Children := 0;
  TypeBoolean_95.Add(DW_AT_name, DW_FORM_string, 'Boolean'+#0);
  TypeBoolean_95.Add(DW_AT_encoding, DW_FORM_data1, [$02]);
  TypeBoolean_95.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_96 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_96.Tag := DW_TAG_reference_type;
  Type_96.Children := 0;
  Type_96.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_94); // $3A, $08, $00, $00

  TypeDeclPOINTER_97 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPOINTER_97.Tag := DW_TAG_typedef;
  TypeDeclPOINTER_97.Children := 0;
  TypeDeclPOINTER_97.Add(DW_AT_name, DW_FORM_string, 'POINTER'+#0);
  TypeDeclPOINTER_97.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_98); // $64, $08, $00, $00

  TypePtr_98 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_98.Tag := DW_TAG_pointer_type;
  TypePtr_98.Children := 0;

  Type_99 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_99.Tag := DW_TAG_reference_type;
  Type_99.Children := 0;
  Type_99.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_97); // $57, $08, $00, $00

  TypeDeclTTESTSETUP1CLASS_100 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTTESTSETUP1CLASS_100.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1CLASS_100.Children := 0;
  TypeDeclTTESTSETUP1CLASS_100.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1CLASS'+#0);
  TypeDeclTTESTSETUP1CLASS_100.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_101); // $80, $08, $00, $00

  TypePtr_101 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_101.Tag := DW_TAG_pointer_type;
  TypePtr_101.Children := 0;
  TypePtr_101.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1CLASS_102); // $85, $08, $00, $00

  TypeTTESTSETUP1CLASS_102 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTTESTSETUP1CLASS_102.Tag := DW_TAG_structure_type;
  TypeTTESTSETUP1CLASS_102.Children := 1;
  TypeTTESTSETUP1CLASS_102.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1CLASS'+#0);
  TypeTTESTSETUP1CLASS_102.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 32);

    XX_103 := TypeTTESTSETUP1CLASS_102.GetNewChild;
    XX_103.Tag := DW_TAG_inheritance;
    XX_103.Children := 0;
    XX_103.Add(DW_AT_accessibility, DW_FORM_data1, [$01]);
    XX_103.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    XX_103.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTOBJECT_237); // $C8, $0F, $00, $00

    VarFBOOL_104 := TypeTTESTSETUP1CLASS_102.GetNewChild;
    VarFBOOL_104.Tag := DW_TAG_member;
    VarFBOOL_104.Children := 0;
    VarFBOOL_104.Add(DW_AT_name, DW_FORM_string, 'FBOOL'+#0);
    VarFBOOL_104.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarFBOOL_104.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_94); // $3A, $08, $00, $00

    VarFWORD_105 := TypeTTESTSETUP1CLASS_102.GetNewChild;
    VarFWORD_105.Tag := DW_TAG_member;
    VarFWORD_105.Children := 0;
    VarFWORD_105.Add(DW_AT_name, DW_FORM_string, 'FWORD'+#0);
    VarFWORD_105.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(6)])); // $23, $06
    VarFWORD_105.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_85); // $ED, $07, $00, $00

    VarFWORDL_106 := TypeTTESTSETUP1CLASS_102.GetNewChild;
    VarFWORDL_106.Tag := DW_TAG_member;
    VarFWORDL_106.Children := 0;
    VarFWORDL_106.Add(DW_AT_name, DW_FORM_string, 'FWORDL'+#0);
    VarFWORDL_106.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(8)])); // $23, $08
    VarFWORDL_106.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_91); // $21, $08, $00, $00

    VarFINT_107 := TypeTTESTSETUP1CLASS_102.GetNewChild;
    VarFINT_107.Tag := DW_TAG_member;
    VarFINT_107.Children := 0;
    VarFINT_107.Add(DW_AT_name, DW_FORM_string, 'FINT'+#0);
    VarFINT_107.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(16)])); // $23, $10
    VarFINT_107.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_82); // $CE, $07, $00, $00

    VarFINTL_108 := TypeTTESTSETUP1CLASS_102.GetNewChild;
    VarFINTL_108.Tag := DW_TAG_member;
    VarFINTL_108.Children := 0;
    VarFINTL_108.Add(DW_AT_name, DW_FORM_string, 'FINTL'+#0);
    VarFINTL_108.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(18)])); // $23, $12
    VarFINTL_108.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclINT64_341); // $F0, $15, $00, $00

    VarFTEST_109 := TypeTTESTSETUP1CLASS_102.GetNewChild;
    VarFTEST_109.Tag := DW_TAG_member;
    VarFTEST_109.Children := 0;
    VarFTEST_109.Add(DW_AT_name, DW_FORM_string, 'FTEST'+#0);
    VarFTEST_109.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(26)])); // $23, $1A
    VarFTEST_109.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_100); // $6A, $08, $00, $00

    VarFBYTE_110 := TypeTTESTSETUP1CLASS_102.GetNewChild;
    VarFBYTE_110.Tag := DW_TAG_member;
    VarFBYTE_110.Children := 0;
    VarFBYTE_110.Add(DW_AT_name, DW_FORM_string, 'FBYTE'+#0);
    VarFBYTE_110.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(30)])); // $23, $1E
    VarFBYTE_110.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_79); // $B7, $07, $00, $00

    ProgCLASSPROC0_111 := TypeTTESTSETUP1CLASS_102.GetNewChild;
    ProgCLASSPROC0_111.Tag := DW_TAG_subprogram;
    ProgCLASSPROC0_111.Children := 1;
    ProgCLASSPROC0_111.Add(DW_AT_name, DW_FORM_string, 'CLASSPROC0'+#0);
    ProgCLASSPROC0_111.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLASSPROC0_111.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLASSPROC0_111.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCLASSPROC0_111.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgCLASSPROC0_111.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $19]));
    ProgCLASSPROC0_111.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00402000);
    ProgCLASSPROC0_111.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00402FFF);

      Varthis_112 := ProgCLASSPROC0_111.GetNewChild;
      Varthis_112.Tag := DW_TAG_formal_parameter;
      Varthis_112.Children := 0;
      Varthis_112.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_112.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-8)])); // $75, $78
      Varthis_112.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_112.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_100); // $6A, $08, $00, $00

      StackOffs := 0; // @TestStackFrame.A - @TestStackFrame.EndPoint;
      VarA_113 := ProgCLASSPROC0_111.GetNewChild;
      VarA_113.Tag := DW_TAG_formal_parameter;
      VarA_113.Children := 0;
      VarA_113.Add(DW_AT_name, DW_FORM_string, 'A'+#0);
      VarA_113.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $7C
      VarA_113.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_88); // $04, $08, $00, $00

  Type_114 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_114.Tag := DW_TAG_reference_type;
  Type_114.Children := 0;
  Type_114.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_100); // $6A, $08, $00, $00

  TypeDeclTTESTSETUP1RECORD_115 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTTESTSETUP1RECORD_115.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1RECORD_115.Children := 0;
  TypeDeclTTESTSETUP1RECORD_115.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1RECORD'+#0);
  TypeDeclTTESTSETUP1RECORD_115.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1RECORD_116); // $54, $09, $00, $00

  TypeTTESTSETUP1RECORD_116 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTTESTSETUP1RECORD_116.Tag := DW_TAG_structure_type;
  TypeTTESTSETUP1RECORD_116.Children := 1;
  TypeTTESTSETUP1RECORD_116.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1RECORD'+#0);
  TypeTTESTSETUP1RECORD_116.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 8);

    VarFWORD_117 := TypeTTESTSETUP1RECORD_116.GetNewChild;
    VarFWORD_117.Tag := DW_TAG_member;
    VarFWORD_117.Children := 0;
    VarFWORD_117.Add(DW_AT_name, DW_FORM_string, 'FWORD'+#0);
    VarFWORD_117.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarFWORD_117.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_85); // $ED, $07, $00, $00

    VarFBOOL_118 := TypeTTESTSETUP1RECORD_116.GetNewChild;
    VarFBOOL_118.Tag := DW_TAG_member;
    VarFBOOL_118.Children := 0;
    VarFBOOL_118.Add(DW_AT_name, DW_FORM_string, 'FBOOL'+#0);
    VarFBOOL_118.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(2)])); // $23, $02
    VarFBOOL_118.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_94); // $3A, $08, $00, $00

    VarFTEST_119 := TypeTTESTSETUP1RECORD_116.GetNewChild;
    VarFTEST_119.Tag := DW_TAG_member;
    VarFTEST_119.Children := 0;
    VarFTEST_119.Add(DW_AT_name, DW_FORM_string, 'FTEST'+#0);
    VarFTEST_119.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarFTEST_119.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_100); // $6A, $08, $00, $00

  Type_120 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_120.Tag := DW_TAG_reference_type;
  Type_120.Children := 0;
  Type_120.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD_115); // $3D, $09, $00, $00

  TypeDeclPTESTSETUP1RECORD_121 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPTESTSETUP1RECORD_121.Tag := DW_TAG_typedef;
  TypeDeclPTESTSETUP1RECORD_121.Children := 0;
  TypeDeclPTESTSETUP1RECORD_121.Add(DW_AT_name, DW_FORM_string, 'PTESTSETUP1RECORD'+#0);
  TypeDeclPTESTSETUP1RECORD_121.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_122); // $AF, $09, $00, $00

  TypePtr_122 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_122.Tag := DW_TAG_pointer_type;
  TypePtr_122.Children := 0;
  TypePtr_122.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD_115); // $3D, $09, $00, $00

  Type_123 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_123.Tag := DW_TAG_reference_type;
  Type_123.Children := 0;
  Type_123.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1RECORD_121); // $98, $09, $00, $00

  TypeDeclTTESTSETUP1RECORD2_124 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTTESTSETUP1RECORD2_124.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1RECORD2_124.Children := 0;
  TypeDeclTTESTSETUP1RECORD2_124.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1RECORD2'+#0);
  TypeDeclTTESTSETUP1RECORD2_124.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1RECORD2_125); // $D1, $09, $00, $00

  TypeTTESTSETUP1RECORD2_125 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTTESTSETUP1RECORD2_125.Tag := DW_TAG_structure_type;
  TypeTTESTSETUP1RECORD2_125.Children := 1;
  TypeTTESTSETUP1RECORD2_125.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1RECORD2'+#0);
  TypeTTESTSETUP1RECORD2_125.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 8);

    VarFWORD_126 := TypeTTESTSETUP1RECORD2_125.GetNewChild;
    VarFWORD_126.Tag := DW_TAG_member;
    VarFWORD_126.Children := 0;
    VarFWORD_126.Add(DW_AT_name, DW_FORM_string, 'FWORD'+#0);
    VarFWORD_126.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarFWORD_126.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_85); // $ED, $07, $00, $00

    VarFBOOL_127 := TypeTTESTSETUP1RECORD2_125.GetNewChild;
    VarFBOOL_127.Tag := DW_TAG_member;
    VarFBOOL_127.Children := 0;
    VarFBOOL_127.Add(DW_AT_name, DW_FORM_string, 'FBOOL'+#0);
    VarFBOOL_127.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(2)])); // $23, $02
    VarFBOOL_127.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_94); // $3A, $08, $00, $00

    VarFTEST_128 := TypeTTESTSETUP1RECORD2_125.GetNewChild;
    VarFTEST_128.Tag := DW_TAG_member;
    VarFTEST_128.Children := 0;
    VarFTEST_128.Add(DW_AT_name, DW_FORM_string, 'FTEST'+#0);
    VarFTEST_128.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarFTEST_128.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_100); // $6A, $08, $00, $00

  Type_129 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_129.Tag := DW_TAG_reference_type;
  Type_129.Children := 0;
  Type_129.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD2_124); // $B9, $09, $00, $00

  TypeDeclPTESTSETUP1RECORD2_130 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPTESTSETUP1RECORD2_130.Tag := DW_TAG_typedef;
  TypeDeclPTESTSETUP1RECORD2_130.Children := 0;
  TypeDeclPTESTSETUP1RECORD2_130.Add(DW_AT_name, DW_FORM_string, 'PTESTSETUP1RECORD2'+#0);
  TypeDeclPTESTSETUP1RECORD2_130.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_131); // $2E, $0A, $00, $00

  TypePtr_131 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_131.Tag := DW_TAG_pointer_type;
  TypePtr_131.Children := 0;
  TypePtr_131.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD2_124); // $B9, $09, $00, $00

  Type_132 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_132.Tag := DW_TAG_reference_type;
  Type_132.Children := 0;
  Type_132.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1RECORD2_130); // $16, $0A, $00, $00

  TypeDeclTTESTSETUP1RECORD3_133 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTTESTSETUP1RECORD3_133.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1RECORD3_133.Children := 0;
  TypeDeclTTESTSETUP1RECORD3_133.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1RECORD3'+#0);
  TypeDeclTTESTSETUP1RECORD3_133.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1RECORD3_134); // $50, $0A, $00, $00

  TypeTTESTSETUP1RECORD3_134 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTTESTSETUP1RECORD3_134.Tag := DW_TAG_structure_type;
  TypeTTESTSETUP1RECORD3_134.Children := 1;
  TypeTTESTSETUP1RECORD3_134.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1RECORD3'+#0);
  TypeTTESTSETUP1RECORD3_134.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 2);

    VarFWORD_135 := TypeTTESTSETUP1RECORD3_134.GetNewChild;
    VarFWORD_135.Tag := DW_TAG_member;
    VarFWORD_135.Children := 0;
    VarFWORD_135.Add(DW_AT_name, DW_FORM_string, 'FWORD'+#0);
    VarFWORD_135.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarFWORD_135.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_85); // $ED, $07, $00, $00

  Type_136 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_136.Tag := DW_TAG_reference_type;
  Type_136.Children := 0;
  Type_136.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD3_133); // $38, $0A, $00, $00

  TypeDeclPTESTSETUP1RECORD3_137 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPTESTSETUP1RECORD3_137.Tag := DW_TAG_typedef;
  TypeDeclPTESTSETUP1RECORD3_137.Children := 0;
  TypeDeclPTESTSETUP1RECORD3_137.Add(DW_AT_name, DW_FORM_string, 'PTESTSETUP1RECORD3'+#0);
  TypeDeclPTESTSETUP1RECORD3_137.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_138); // $91, $0A, $00, $00

  TypePtr_138 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_138.Tag := DW_TAG_pointer_type;
  TypePtr_138.Children := 0;
  TypePtr_138.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1RECORD3_133); // $38, $0A, $00, $00

  Type_139 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_139.Tag := DW_TAG_reference_type;
  Type_139.Children := 0;
  Type_139.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1RECORD3_137); // $79, $0A, $00, $00

  TypeDeclPTESTSETUP1CLASS_140 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPTESTSETUP1CLASS_140.Tag := DW_TAG_typedef;
  TypeDeclPTESTSETUP1CLASS_140.Children := 0;
  TypeDeclPTESTSETUP1CLASS_140.Add(DW_AT_name, DW_FORM_string, 'PTESTSETUP1CLASS'+#0);
  TypeDeclPTESTSETUP1CLASS_140.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_141); // $B1, $0A, $00, $00

  TypePtr_141 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_141.Tag := DW_TAG_pointer_type;
  TypePtr_141.Children := 0;
  TypePtr_141.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_100); // $6A, $08, $00, $00

  Type_142 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_142.Tag := DW_TAG_reference_type;
  Type_142.Children := 0;
  Type_142.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1CLASS_140); // $9B, $0A, $00, $00

  TypeDeclTTESTSETUP1CLASSCHILD_143 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTTESTSETUP1CLASSCHILD_143.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1CLASSCHILD_143.Children := 0;
  TypeDeclTTESTSETUP1CLASSCHILD_143.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1CLASSCHILD'+#0);
  TypeDeclTTESTSETUP1CLASSCHILD_143.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_144); // $D6, $0A, $00, $00

  TypePtr_144 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_144.Tag := DW_TAG_pointer_type;
  TypePtr_144.Children := 0;
  TypePtr_144.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1CLASSCHILD_145); // $DB, $0A, $00, $00

  TypeTTESTSETUP1CLASSCHILD_145 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTTESTSETUP1CLASSCHILD_145.Tag := DW_TAG_structure_type;
  TypeTTESTSETUP1CLASSCHILD_145.Children := 1;
  TypeTTESTSETUP1CLASSCHILD_145.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1CLASSCHILD'+#0);
  TypeTTESTSETUP1CLASSCHILD_145.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 48);

    XX_146 := TypeTTESTSETUP1CLASSCHILD_145.GetNewChild;
    XX_146.Tag := DW_TAG_inheritance;
    XX_146.Children := 0;
    XX_146.Add(DW_AT_accessibility, DW_FORM_data1, [$01]);
    XX_146.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    XX_146.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1CLASS_102); // $85, $08, $00, $00

    VarFINT64_147 := TypeTTESTSETUP1CLASSCHILD_145.GetNewChild;
    VarFINT64_147.Tag := DW_TAG_member;
    VarFINT64_147.Children := 0;
    VarFINT64_147.Add(DW_AT_name, DW_FORM_string, 'FINT64'+#0);
    VarFINT64_147.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(32)])); // $23, $20
    VarFINT64_147.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclINT64_341); // $F0, $15, $00, $00

    VarFQWORD_148 := TypeTTESTSETUP1CLASSCHILD_145.GetNewChild;
    VarFQWORD_148.Tag := DW_TAG_member;
    VarFQWORD_148.Children := 0;
    VarFQWORD_148.Add(DW_AT_name, DW_FORM_string, 'FQWORD'+#0);
    VarFQWORD_148.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(40)])); // $23, $28
    VarFQWORD_148.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_91); // $21, $08, $00, $00

  Type_149 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_149.Tag := DW_TAG_reference_type;
  Type_149.Children := 0;
  Type_149.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCHILD_143); // $BB, $0A, $00, $00

  TypeDeclPTESTSETUP1CLASSCHILD_150 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPTESTSETUP1CLASSCHILD_150.Tag := DW_TAG_typedef;
  TypeDeclPTESTSETUP1CLASSCHILD_150.Children := 0;
  TypeDeclPTESTSETUP1CLASSCHILD_150.Add(DW_AT_name, DW_FORM_string, 'PTESTSETUP1CLASSCHILD'+#0);
  TypeDeclPTESTSETUP1CLASSCHILD_150.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_151); // $3B, $0B, $00, $00

  TypePtr_151 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_151.Tag := DW_TAG_pointer_type;
  TypePtr_151.Children := 0;
  TypePtr_151.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCHILD_143); // $BB, $0A, $00, $00

  Type_152 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_152.Tag := DW_TAG_reference_type;
  Type_152.Children := 0;
  Type_152.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1CLASSCHILD_150); // $20, $0B, $00, $00

  TypeDeclTTESTSETUP1CLASS2_153 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTTESTSETUP1CLASS2_153.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1CLASS2_153.Children := 0;
  TypeDeclTTESTSETUP1CLASS2_153.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1CLASS2'+#0);
  TypeDeclTTESTSETUP1CLASS2_153.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_154); // $5C, $0B, $00, $00

  TypePtr_154 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_154.Tag := DW_TAG_pointer_type;
  TypePtr_154.Children := 0;
  TypePtr_154.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1CLASS2_155); // $61, $0B, $00, $00

  TypeTTESTSETUP1CLASS2_155 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTTESTSETUP1CLASS2_155.Tag := DW_TAG_structure_type;
  TypeTTESTSETUP1CLASS2_155.Children := 1;
  TypeTTESTSETUP1CLASS2_155.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1CLASS2'+#0);
  TypeTTESTSETUP1CLASS2_155.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 10);

    XX_156 := TypeTTESTSETUP1CLASS2_155.GetNewChild;
    XX_156.Tag := DW_TAG_inheritance;
    XX_156.Children := 0;
    XX_156.Add(DW_AT_accessibility, DW_FORM_data1, [$01]);
    XX_156.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    XX_156.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTOBJECT_237); // $C8, $0F, $00, $00

    VarFINT_157 := TypeTTESTSETUP1CLASS2_155.GetNewChild;
    VarFINT_157.Tag := DW_TAG_member;
    VarFINT_157.Children := 0;
    VarFINT_157.Add(DW_AT_name, DW_FORM_string, 'FINT'+#0);
    VarFINT_157.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarFINT_157.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_88); // $04, $08, $00, $00

    VarFWORD_158 := TypeTTESTSETUP1CLASS2_155.GetNewChild;
    VarFWORD_158.Tag := DW_TAG_member;
    VarFWORD_158.Children := 0;
    VarFWORD_158.Add(DW_AT_name, DW_FORM_string, 'FWORD'+#0);
    VarFWORD_158.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(8)])); // $23, $08
    VarFWORD_158.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_85); // $ED, $07, $00, $00

  Type_159 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_159.Tag := DW_TAG_reference_type;
  Type_159.Children := 0;
  Type_159.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS2_153); // $45, $0B, $00, $00

  TypeDeclPTESTSETUP1CLASS2_160 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPTESTSETUP1CLASS2_160.Tag := DW_TAG_typedef;
  TypeDeclPTESTSETUP1CLASS2_160.Children := 0;
  TypeDeclPTESTSETUP1CLASS2_160.Add(DW_AT_name, DW_FORM_string, 'PTESTSETUP1CLASS2'+#0);
  TypeDeclPTESTSETUP1CLASS2_160.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_161); // $B6, $0B, $00, $00

  TypePtr_161 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_161.Tag := DW_TAG_pointer_type;
  TypePtr_161.Children := 0;
  TypePtr_161.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS2_153); // $45, $0B, $00, $00

  Type_162 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_162.Tag := DW_TAG_reference_type;
  Type_162.Children := 0;
  Type_162.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1CLASS2_160); // $9F, $0B, $00, $00

  TypeDeclTTESTSETUP1CLASSCLASS_163 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTTESTSETUP1CLASSCLASS_163.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1CLASSCLASS_163.Children := 0;
  TypeDeclTTESTSETUP1CLASSCLASS_163.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1CLASSCLASS'+#0);
  TypeDeclTTESTSETUP1CLASSCLASS_163.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_164); // $DB, $0B, $00, $00

  TypePtr_164 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_164.Tag := DW_TAG_pointer_type;
  TypePtr_164.Children := 0;
  TypePtr_164.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_165 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_165.Tag := DW_TAG_reference_type;
  Type_165.Children := 0;
  Type_165.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCLASS_163); // $C0, $0B, $00, $00

  TypeDeclTTESTSETUP1CLASSCHILDCLASS_166 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTTESTSETUP1CLASSCHILDCLASS_166.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1CLASSCHILDCLASS_166.Children := 0;
  TypeDeclTTESTSETUP1CLASSCHILDCLASS_166.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1CLASSCHILDCLASS'+#0);
  TypeDeclTTESTSETUP1CLASSCHILDCLASS_166.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_167); // $05, $0C, $00, $00

  TypePtr_167 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_167.Tag := DW_TAG_pointer_type;
  TypePtr_167.Children := 0;
  TypePtr_167.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_168 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_168.Tag := DW_TAG_reference_type;
  Type_168.Children := 0;
  Type_168.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASSCHILDCLASS_166); // $E5, $0B, $00, $00

  TypeDeclTTESTSETUP1OBJECT_169 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTTESTSETUP1OBJECT_169.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1OBJECT_169.Children := 0;
  TypeDeclTTESTSETUP1OBJECT_169.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1OBJECT'+#0);
  TypeDeclTTESTSETUP1OBJECT_169.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1OBJECT_170); // $26, $0C, $00, $00

  TypeTTESTSETUP1OBJECT_170 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTTESTSETUP1OBJECT_170.Tag := DW_TAG_structure_type;
  TypeTTESTSETUP1OBJECT_170.Children := 1;
  TypeTTESTSETUP1OBJECT_170.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1OBJECT'+#0);
  TypeTTESTSETUP1OBJECT_170.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 36);

    Var_vptrTTESTSETUP1OBJECT_171 := TypeTTESTSETUP1OBJECT_170.GetNewChild;
    Var_vptrTTESTSETUP1OBJECT_171.Tag := DW_TAG_member;
    Var_vptrTTESTSETUP1OBJECT_171.Children := 0;
    Var_vptrTTESTSETUP1OBJECT_171.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
    Var_vptrTTESTSETUP1OBJECT_171.Add(DW_AT_name, DW_FORM_string, '_vptr$TTESTSETUP1OBJECT'+#0);
    Var_vptrTTESTSETUP1OBJECT_171.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(32)])); // $23, $20
    Var_vptrTTESTSETUP1OBJECT_171.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_97); // $57, $08, $00, $00

    VarFWORD_172 := TypeTTESTSETUP1OBJECT_170.GetNewChild;
    VarFWORD_172.Tag := DW_TAG_member;
    VarFWORD_172.Children := 0;
    VarFWORD_172.Add(DW_AT_name, DW_FORM_string, 'FWORD'+#0);
    VarFWORD_172.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarFWORD_172.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_85); // $ED, $07, $00, $00

    VarFWORDL_173 := TypeTTESTSETUP1OBJECT_170.GetNewChild;
    VarFWORDL_173.Tag := DW_TAG_member;
    VarFWORDL_173.Children := 0;
    VarFWORDL_173.Add(DW_AT_name, DW_FORM_string, 'FWORDL'+#0);
    VarFWORDL_173.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(2)])); // $23, $02
    VarFWORDL_173.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_91); // $21, $08, $00, $00

    VarFINT_174 := TypeTTESTSETUP1OBJECT_170.GetNewChild;
    VarFINT_174.Tag := DW_TAG_member;
    VarFINT_174.Children := 0;
    VarFINT_174.Add(DW_AT_name, DW_FORM_string, 'FINT'+#0);
    VarFINT_174.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(10)])); // $23, $0A
    VarFINT_174.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_82); // $CE, $07, $00, $00

    VarFINTL_175 := TypeTTESTSETUP1OBJECT_170.GetNewChild;
    VarFINTL_175.Tag := DW_TAG_member;
    VarFINTL_175.Children := 0;
    VarFINTL_175.Add(DW_AT_name, DW_FORM_string, 'FINTL'+#0);
    VarFINTL_175.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(12)])); // $23, $0C
    VarFINTL_175.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclINT64_341); // $F0, $15, $00, $00

    VarFBOOL_176 := TypeTTESTSETUP1OBJECT_170.GetNewChild;
    VarFBOOL_176.Tag := DW_TAG_member;
    VarFBOOL_176.Children := 0;
    VarFBOOL_176.Add(DW_AT_name, DW_FORM_string, 'FBOOL'+#0);
    VarFBOOL_176.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(20)])); // $23, $14
    VarFBOOL_176.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_94); // $3A, $08, $00, $00

    VarFBOOL2_177 := TypeTTESTSETUP1OBJECT_170.GetNewChild;
    VarFBOOL2_177.Tag := DW_TAG_member;
    VarFBOOL2_177.Children := 0;
    VarFBOOL2_177.Add(DW_AT_name, DW_FORM_string, 'FBOOL2'+#0);
    VarFBOOL2_177.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(22)])); // $23, $16
    VarFBOOL2_177.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGBOOL_347); // $26, $16, $00, $00

    VarFBOOL3_178 := TypeTTESTSETUP1OBJECT_170.GetNewChild;
    VarFBOOL3_178.Tag := DW_TAG_member;
    VarFBOOL3_178.Children := 0;
    VarFBOOL3_178.Add(DW_AT_name, DW_FORM_string, 'FBOOL3'+#0);
    VarFBOOL3_178.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(26)])); // $23, $1A
    VarFBOOL3_178.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTEBOOL_350); // $45, $16, $00, $00

    VarFTEST_179 := TypeTTESTSETUP1OBJECT_170.GetNewChild;
    VarFTEST_179.Tag := DW_TAG_member;
    VarFTEST_179.Children := 0;
    VarFTEST_179.Add(DW_AT_name, DW_FORM_string, 'FTEST'+#0);
    VarFTEST_179.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(28)])); // $23, $1C
    VarFTEST_179.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_100); // $6A, $08, $00, $00

    ProgOBJPROC0_180 := TypeTTESTSETUP1OBJECT_170.GetNewChild;
    ProgOBJPROC0_180.Tag := DW_TAG_subprogram;
    ProgOBJPROC0_180.Children := 1;
    ProgOBJPROC0_180.Add(DW_AT_name, DW_FORM_string, 'OBJPROC0'+#0);
    ProgOBJPROC0_180.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgOBJPROC0_180.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgOBJPROC0_180.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgOBJPROC0_180.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgOBJPROC0_180.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $03]));
    ProgOBJPROC0_180.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00403000);
    ProgOBJPROC0_180.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00403FFF);

      Varthis_181 := ProgOBJPROC0_180.GetNewChild;
      Varthis_181.Tag := DW_TAG_formal_parameter;
      Varthis_181.Children := 0;
      Varthis_181.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_181.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-8), DW_OP_deref])); // $75, $78, $06
      Varthis_181.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_181.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_169); // $0F, $0C, $00, $00

      StackOffs := 0; // @TestStackFrame.O1 - @TestStackFrame.EndPoint;
      VarO1_182 := ProgOBJPROC0_180.GetNewChild;
      VarO1_182.Tag := DW_TAG_formal_parameter;
      VarO1_182.Children := 0;
      VarO1_182.Add(DW_AT_name, DW_FORM_string, 'O1'+#0);
      VarO1_182.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $7C
      VarO1_182.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_88); // $04, $08, $00, $00

  Type_183 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_183.Tag := DW_TAG_reference_type;
  Type_183.Children := 0;
  Type_183.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_169); // $0F, $0C, $00, $00

  TypeDeclPTESTSETUP1OBJECT_184 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPTESTSETUP1OBJECT_184.Tag := DW_TAG_typedef;
  TypeDeclPTESTSETUP1OBJECT_184.Children := 0;
  TypeDeclPTESTSETUP1OBJECT_184.Add(DW_AT_name, DW_FORM_string, 'PTESTSETUP1OBJECT'+#0);
  TypeDeclPTESTSETUP1OBJECT_184.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_185); // $1E, $0D, $00, $00

  TypePtr_185 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_185.Tag := DW_TAG_pointer_type;
  TypePtr_185.Children := 0;
  TypePtr_185.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT_169); // $0F, $0C, $00, $00

  Type_186 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_186.Tag := DW_TAG_reference_type;
  Type_186.Children := 0;
  Type_186.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1OBJECT_184); // $07, $0D, $00, $00

  TypeDeclTTESTSETUP1OBJECT2_187 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTTESTSETUP1OBJECT2_187.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1OBJECT2_187.Children := 0;
  TypeDeclTTESTSETUP1OBJECT2_187.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1OBJECT2'+#0);
  TypeDeclTTESTSETUP1OBJECT2_187.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1OBJECT2_188); // $40, $0D, $00, $00

  TypeTTESTSETUP1OBJECT2_188 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTTESTSETUP1OBJECT2_188.Tag := DW_TAG_structure_type;
  TypeTTESTSETUP1OBJECT2_188.Children := 1;
  TypeTTESTSETUP1OBJECT2_188.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1OBJECT2'+#0);
  TypeTTESTSETUP1OBJECT2_188.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 36);

    Var_vptrTTESTSETUP1OBJECT2_189 := TypeTTESTSETUP1OBJECT2_188.GetNewChild;
    Var_vptrTTESTSETUP1OBJECT2_189.Tag := DW_TAG_member;
    Var_vptrTTESTSETUP1OBJECT2_189.Children := 0;
    Var_vptrTTESTSETUP1OBJECT2_189.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
    Var_vptrTTESTSETUP1OBJECT2_189.Add(DW_AT_name, DW_FORM_string, '_vptr$TTESTSETUP1OBJECT2'+#0);
    Var_vptrTTESTSETUP1OBJECT2_189.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(32)])); // $23, $20
    Var_vptrTTESTSETUP1OBJECT2_189.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_97); // $57, $08, $00, $00

    VarFWORD_190 := TypeTTESTSETUP1OBJECT2_188.GetNewChild;
    VarFWORD_190.Tag := DW_TAG_member;
    VarFWORD_190.Children := 0;
    VarFWORD_190.Add(DW_AT_name, DW_FORM_string, 'FWORD'+#0);
    VarFWORD_190.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarFWORD_190.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_85); // $ED, $07, $00, $00

    VarFWORDL_191 := TypeTTESTSETUP1OBJECT2_188.GetNewChild;
    VarFWORDL_191.Tag := DW_TAG_member;
    VarFWORDL_191.Children := 0;
    VarFWORDL_191.Add(DW_AT_name, DW_FORM_string, 'FWORDL'+#0);
    VarFWORDL_191.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(2)])); // $23, $02
    VarFWORDL_191.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_91); // $21, $08, $00, $00

    VarFINT_192 := TypeTTESTSETUP1OBJECT2_188.GetNewChild;
    VarFINT_192.Tag := DW_TAG_member;
    VarFINT_192.Children := 0;
    VarFINT_192.Add(DW_AT_name, DW_FORM_string, 'FINT'+#0);
    VarFINT_192.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(10)])); // $23, $0A
    VarFINT_192.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_82); // $CE, $07, $00, $00

    VarFINTL_193 := TypeTTESTSETUP1OBJECT2_188.GetNewChild;
    VarFINTL_193.Tag := DW_TAG_member;
    VarFINTL_193.Children := 0;
    VarFINTL_193.Add(DW_AT_name, DW_FORM_string, 'FINTL'+#0);
    VarFINTL_193.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(12)])); // $23, $0C
    VarFINTL_193.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclINT64_341); // $F0, $15, $00, $00

    VarFBOOL_194 := TypeTTESTSETUP1OBJECT2_188.GetNewChild;
    VarFBOOL_194.Tag := DW_TAG_member;
    VarFBOOL_194.Children := 0;
    VarFBOOL_194.Add(DW_AT_name, DW_FORM_string, 'FBOOL'+#0);
    VarFBOOL_194.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(20)])); // $23, $14
    VarFBOOL_194.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_94); // $3A, $08, $00, $00

    VarFBOOL2_195 := TypeTTESTSETUP1OBJECT2_188.GetNewChild;
    VarFBOOL2_195.Tag := DW_TAG_member;
    VarFBOOL2_195.Children := 0;
    VarFBOOL2_195.Add(DW_AT_name, DW_FORM_string, 'FBOOL2'+#0);
    VarFBOOL2_195.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(22)])); // $23, $16
    VarFBOOL2_195.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGBOOL_347); // $26, $16, $00, $00

    VarFBOOL3_196 := TypeTTESTSETUP1OBJECT2_188.GetNewChild;
    VarFBOOL3_196.Tag := DW_TAG_member;
    VarFBOOL3_196.Children := 0;
    VarFBOOL3_196.Add(DW_AT_name, DW_FORM_string, 'FBOOL3'+#0);
    VarFBOOL3_196.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(26)])); // $23, $1A
    VarFBOOL3_196.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTEBOOL_350); // $45, $16, $00, $00

    VarFTEST_197 := TypeTTESTSETUP1OBJECT2_188.GetNewChild;
    VarFTEST_197.Tag := DW_TAG_member;
    VarFTEST_197.Children := 0;
    VarFTEST_197.Add(DW_AT_name, DW_FORM_string, 'FTEST'+#0);
    VarFTEST_197.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(28)])); // $23, $1C
    VarFTEST_197.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1CLASS_100); // $6A, $08, $00, $00

    ProgOBJPROC1_198 := TypeTTESTSETUP1OBJECT2_188.GetNewChild;
    ProgOBJPROC1_198.Tag := DW_TAG_subprogram;
    ProgOBJPROC1_198.Children := 1;
    ProgOBJPROC1_198.Add(DW_AT_name, DW_FORM_string, 'OBJPROC1'+#0);
    ProgOBJPROC1_198.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgOBJPROC1_198.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgOBJPROC1_198.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgOBJPROC1_198.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgOBJPROC1_198.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $03]));
    ProgOBJPROC1_198.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00404000);
    ProgOBJPROC1_198.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00404FFF);

      Varthis_199 := ProgOBJPROC1_198.GetNewChild;
      Varthis_199.Tag := DW_TAG_formal_parameter;
      Varthis_199.Children := 0;
      Varthis_199.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_199.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-8), DW_OP_deref])); // $75, $78, $06
      Varthis_199.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_199.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT2_187); // $28, $0D, $00, $00

      StackOffs := 0; // @TestStackFrame.O2 - @TestStackFrame.EndPoint;
      VarO2_200 := ProgOBJPROC1_198.GetNewChild;
      VarO2_200.Tag := DW_TAG_formal_parameter;
      VarO2_200.Children := 0;
      VarO2_200.Add(DW_AT_name, DW_FORM_string, 'O2'+#0);
      VarO2_200.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $7C
      VarO2_200.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_88); // $04, $08, $00, $00

  Type_201 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_201.Tag := DW_TAG_reference_type;
  Type_201.Children := 0;
  Type_201.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT2_187); // $28, $0D, $00, $00

  TypeDeclPTESTSETUP1OBJECT2_202 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPTESTSETUP1OBJECT2_202.Tag := DW_TAG_typedef;
  TypeDeclPTESTSETUP1OBJECT2_202.Children := 0;
  TypeDeclPTESTSETUP1OBJECT2_202.Add(DW_AT_name, DW_FORM_string, 'PTESTSETUP1OBJECT2'+#0);
  TypeDeclPTESTSETUP1OBJECT2_202.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_203); // $3B, $0E, $00, $00

  TypePtr_203 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_203.Tag := DW_TAG_pointer_type;
  TypePtr_203.Children := 0;
  TypePtr_203.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT2_187); // $28, $0D, $00, $00

  Type_204 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_204.Tag := DW_TAG_reference_type;
  Type_204.Children := 0;
  Type_204.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1OBJECT2_202); // $23, $0E, $00, $00

  TypeDeclTTESTSETUP1OBJECT3_205 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTTESTSETUP1OBJECT3_205.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1OBJECT3_205.Children := 0;
  TypeDeclTTESTSETUP1OBJECT3_205.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1OBJECT3'+#0);
  TypeDeclTTESTSETUP1OBJECT3_205.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1OBJECT3_206); // $5D, $0E, $00, $00

  TypeTTESTSETUP1OBJECT3_206 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTTESTSETUP1OBJECT3_206.Tag := DW_TAG_structure_type;
  TypeTTESTSETUP1OBJECT3_206.Children := 1;
  TypeTTESTSETUP1OBJECT3_206.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1OBJECT3'+#0);
  TypeTTESTSETUP1OBJECT3_206.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 6);

    Var_vptrTTESTSETUP1OBJECT3_207 := TypeTTESTSETUP1OBJECT3_206.GetNewChild;
    Var_vptrTTESTSETUP1OBJECT3_207.Tag := DW_TAG_member;
    Var_vptrTTESTSETUP1OBJECT3_207.Children := 0;
    Var_vptrTTESTSETUP1OBJECT3_207.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
    Var_vptrTTESTSETUP1OBJECT3_207.Add(DW_AT_name, DW_FORM_string, '_vptr$TTESTSETUP1OBJECT3'+#0);
    Var_vptrTTESTSETUP1OBJECT3_207.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(2)])); // $23, $02
    Var_vptrTTESTSETUP1OBJECT3_207.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_97); // $57, $08, $00, $00

    VarFWORD_208 := TypeTTESTSETUP1OBJECT3_206.GetNewChild;
    VarFWORD_208.Tag := DW_TAG_member;
    VarFWORD_208.Children := 0;
    VarFWORD_208.Add(DW_AT_name, DW_FORM_string, 'FWORD'+#0);
    VarFWORD_208.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarFWORD_208.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_85); // $ED, $07, $00, $00

    ProgOBJPROC1_209 := TypeTTESTSETUP1OBJECT3_206.GetNewChild;
    ProgOBJPROC1_209.Tag := DW_TAG_subprogram;
    ProgOBJPROC1_209.Children := 1;
    ProgOBJPROC1_209.Add(DW_AT_name, DW_FORM_string, 'OBJPROC1'+#0);
    ProgOBJPROC1_209.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgOBJPROC1_209.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgOBJPROC1_209.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgOBJPROC1_209.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgOBJPROC1_209.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $03]));
    ProgOBJPROC1_209.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00405000);
    ProgOBJPROC1_209.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00405FFF);

      Varthis_210 := ProgOBJPROC1_209.GetNewChild;
      Varthis_210.Tag := DW_TAG_formal_parameter;
      Varthis_210.Children := 0;
      Varthis_210.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_210.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(-8), DW_OP_deref])); // $75, $78, $06
      Varthis_210.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_210.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT3_205); // $45, $0E, $00, $00

      StackOffs := 0; // @TestStackFrame.O2 - @TestStackFrame.EndPoint;
      VarO2_211 := ProgOBJPROC1_209.GetNewChild;
      VarO2_211.Tag := DW_TAG_formal_parameter;
      VarO2_211.Children := 0;
      VarO2_211.Add(DW_AT_name, DW_FORM_string, 'O2'+#0);
      VarO2_211.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_breg5, SLEB(StackOffs)])); // $75, $7C
      VarO2_211.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_88); // $04, $08, $00, $00

  Type_212 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_212.Tag := DW_TAG_reference_type;
  Type_212.Children := 0;
  Type_212.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT3_205); // $45, $0E, $00, $00

  TypeDeclPTESTSETUP1OBJECT3_213 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPTESTSETUP1OBJECT3_213.Tag := DW_TAG_typedef;
  TypeDeclPTESTSETUP1OBJECT3_213.Children := 0;
  TypeDeclPTESTSETUP1OBJECT3_213.Add(DW_AT_name, DW_FORM_string, 'PTESTSETUP1OBJECT3'+#0);
  TypeDeclPTESTSETUP1OBJECT3_213.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_214); // $F4, $0E, $00, $00

  TypePtr_214 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_214.Tag := DW_TAG_pointer_type;
  TypePtr_214.Children := 0;
  TypePtr_214.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT3_205); // $45, $0E, $00, $00

  Type_215 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_215.Tag := DW_TAG_reference_type;
  Type_215.Children := 0;
  Type_215.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1OBJECT3_213); // $DC, $0E, $00, $00

  TypeDeclTTESTSETUP1OBJECT4_216 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTTESTSETUP1OBJECT4_216.Tag := DW_TAG_typedef;
  TypeDeclTTESTSETUP1OBJECT4_216.Children := 0;
  TypeDeclTTESTSETUP1OBJECT4_216.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1OBJECT4'+#0);
  TypeDeclTTESTSETUP1OBJECT4_216.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTTESTSETUP1OBJECT4_217); // $16, $0F, $00, $00

  TypeTTESTSETUP1OBJECT4_217 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTTESTSETUP1OBJECT4_217.Tag := DW_TAG_structure_type;
  TypeTTESTSETUP1OBJECT4_217.Children := 1;
  TypeTTESTSETUP1OBJECT4_217.Add(DW_AT_name, DW_FORM_string, 'TTESTSETUP1OBJECT4'+#0);
  TypeTTESTSETUP1OBJECT4_217.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 2);

    VarFWORD_218 := TypeTTESTSETUP1OBJECT4_217.GetNewChild;
    VarFWORD_218.Tag := DW_TAG_member;
    VarFWORD_218.Children := 0;
    VarFWORD_218.Add(DW_AT_name, DW_FORM_string, 'FWORD'+#0);
    VarFWORD_218.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarFWORD_218.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_85); // $ED, $07, $00, $00

  Type_219 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_219.Tag := DW_TAG_reference_type;
  Type_219.Children := 0;
  Type_219.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT4_216); // $FE, $0E, $00, $00

  TypeDeclPTESTSETUP1OBJECT4_220 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPTESTSETUP1OBJECT4_220.Tag := DW_TAG_typedef;
  TypeDeclPTESTSETUP1OBJECT4_220.Children := 0;
  TypeDeclPTESTSETUP1OBJECT4_220.Add(DW_AT_name, DW_FORM_string, 'PTESTSETUP1OBJECT4'+#0);
  TypeDeclPTESTSETUP1OBJECT4_220.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_221); // $57, $0F, $00, $00

  TypePtr_221 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_221.Tag := DW_TAG_pointer_type;
  TypePtr_221.Children := 0;
  TypePtr_221.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTTESTSETUP1OBJECT4_216); // $FE, $0E, $00, $00

  Type_222 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_222.Tag := DW_TAG_reference_type;
  Type_222.Children := 0;
  Type_222.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPTESTSETUP1OBJECT4_220); // $3F, $0F, $00, $00

  TypeDeclPINT_223 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPINT_223.Tag := DW_TAG_typedef;
  TypeDeclPINT_223.Children := 0;
  TypeDeclPINT_223.Add(DW_AT_name, DW_FORM_string, 'PINT'+#0);
  TypeDeclPINT_223.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_224); // $6B, $0F, $00, $00

  TypePtr_224 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_224.Tag := DW_TAG_pointer_type;
  TypePtr_224.Children := 0;
  TypePtr_224.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_88); // $04, $08, $00, $00

  Type_225 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_225.Tag := DW_TAG_reference_type;
  Type_225.Children := 0;
  Type_225.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINT_223); // $61, $0F, $00, $00

  TypeDeclPPINT_226 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPPINT_226.Tag := DW_TAG_typedef;
  TypeDeclPPINT_226.Children := 0;
  TypeDeclPPINT_226.Add(DW_AT_name, DW_FORM_string, 'PPINT'+#0);
  TypeDeclPPINT_226.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_227); // $80, $0F, $00, $00

  TypePtr_227 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_227.Tag := DW_TAG_pointer_type;
  TypePtr_227.Children := 0;
  TypePtr_227.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINT_223); // $61, $0F, $00, $00

  Type_228 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_228.Tag := DW_TAG_reference_type;
  Type_228.Children := 0;
  Type_228.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPPINT_226); // $75, $0F, $00, $00

  TypeDeclPPPINT_229 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPPPINT_229.Tag := DW_TAG_typedef;
  TypeDeclPPPINT_229.Children := 0;
  TypeDeclPPPINT_229.Add(DW_AT_name, DW_FORM_string, 'PPPINT'+#0);
  TypeDeclPPPINT_229.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_230); // $96, $0F, $00, $00

  TypePtr_230 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_230.Tag := DW_TAG_pointer_type;
  TypePtr_230.Children := 0;
  TypePtr_230.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPPINT_226); // $75, $0F, $00, $00

  Type_231 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_231.Tag := DW_TAG_reference_type;
  Type_231.Children := 0;
  Type_231.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPPPINT_229); // $8A, $0F, $00, $00

  TypeDeclPQWORD_232 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPQWORD_232.Tag := DW_TAG_typedef;
  TypeDeclPQWORD_232.Children := 0;
  TypeDeclPQWORD_232.Add(DW_AT_name, DW_FORM_string, 'PQWORD'+#0);
  TypeDeclPQWORD_232.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_233); // $AC, $0F, $00, $00

  TypePtr_233 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_233.Tag := DW_TAG_pointer_type;
  TypePtr_233.Children := 0;
  TypePtr_233.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_91); // $21, $08, $00, $00

  Type_234 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_234.Tag := DW_TAG_reference_type;
  Type_234.Children := 0;
  Type_234.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPQWORD_232); // $A0, $0F, $00, $00

  TypeDeclTOBJECT_235 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTOBJECT_235.Tag := DW_TAG_typedef;
  TypeDeclTOBJECT_235.Children := 0;
  TypeDeclTOBJECT_235.Add(DW_AT_name, DW_FORM_string, 'TOBJECT'+#0);
  TypeDeclTOBJECT_235.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_236); // $C3, $0F, $00, $00

  TypePtr_236 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_236.Tag := DW_TAG_pointer_type;
  TypePtr_236.Children := 0;
  TypePtr_236.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTOBJECT_237); // $C8, $0F, $00, $00

  TypeTOBJECT_237 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTOBJECT_237.Tag := DW_TAG_structure_type;
  TypeTOBJECT_237.Children := 1;
  TypeTOBJECT_237.Add(DW_AT_name, DW_FORM_string, 'TOBJECT'+#0);
  TypeTOBJECT_237.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 4);

    Var_vptrTOBJECT_238 := TypeTOBJECT_237.GetNewChild;
    Var_vptrTOBJECT_238.Tag := DW_TAG_member;
    Var_vptrTOBJECT_238.Children := 0;
    Var_vptrTOBJECT_238.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
    Var_vptrTOBJECT_238.Add(DW_AT_name, DW_FORM_string, '_vptr$TOBJECT'+#0);
    Var_vptrTOBJECT_238.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    Var_vptrTOBJECT_238.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_97); // $57, $08, $00, $00

    ProgCREATE_239 := TypeTOBJECT_237.GetNewChild;
    ProgCREATE_239.Tag := DW_TAG_subprogram;
    ProgCREATE_239.Children := 1;
    ProgCREATE_239.Add(DW_AT_name, DW_FORM_string, 'CREATE'+#0);
    ProgCREATE_239.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCREATE_239.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCREATE_239.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCREATE_239.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

      Varthis_240 := ProgCREATE_239.GetNewChild;
      Varthis_240.Tag := DW_TAG_formal_parameter;
      Varthis_240.Children := 0;
      Varthis_240.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_240.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_240.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

      Varvmt_241 := ProgCREATE_239.GetNewChild;
      Varvmt_241.Tag := DW_TAG_formal_parameter;
      Varvmt_241.Children := 0;
      Varvmt_241.Add(DW_AT_name, DW_FORM_string, 'vmt'+#0);
      Varvmt_241.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_97); // $57, $08, $00, $00

    ProgDESTROY_242 := TypeTOBJECT_237.GetNewChild;
    ProgDESTROY_242.Tag := DW_TAG_subprogram;
    ProgDESTROY_242.Children := 1;
    ProgDESTROY_242.Add(DW_AT_name, DW_FORM_string, 'DESTROY'+#0);
    ProgDESTROY_242.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgDESTROY_242.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgDESTROY_242.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgDESTROY_242.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgDESTROY_242.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $0C]));

      Varthis_243 := ProgDESTROY_242.GetNewChild;
      Varthis_243.Tag := DW_TAG_formal_parameter;
      Varthis_243.Children := 0;
      Varthis_243.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_243.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_243.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

      Varvmt_244 := ProgDESTROY_242.GetNewChild;
      Varvmt_244.Tag := DW_TAG_formal_parameter;
      Varvmt_244.Children := 0;
      Varvmt_244.Add(DW_AT_name, DW_FORM_string, 'vmt'+#0);
      Varvmt_244.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_97); // $57, $08, $00, $00

    ProgNEWINSTANCE_245 := TypeTOBJECT_237.GetNewChild;
    ProgNEWINSTANCE_245.Tag := DW_TAG_subprogram;
    ProgNEWINSTANCE_245.Children := 1;
    ProgNEWINSTANCE_245.Add(DW_AT_name, DW_FORM_string, 'NEWINSTANCE'+#0);
    ProgNEWINSTANCE_245.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgNEWINSTANCE_245.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgNEWINSTANCE_245.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgNEWINSTANCE_245.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgNEWINSTANCE_245.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $0D]));
    ProgNEWINSTANCE_245.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

      Varself_246 := ProgNEWINSTANCE_245.GetNewChild;
      Varself_246.Tag := DW_TAG_formal_parameter;
      Varself_246.Children := 0;
      Varself_246.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_246.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_246.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_353); // $64, $16, $00, $00

    ProgFREEINSTANCE_247 := TypeTOBJECT_237.GetNewChild;
    ProgFREEINSTANCE_247.Tag := DW_TAG_subprogram;
    ProgFREEINSTANCE_247.Children := 1;
    ProgFREEINSTANCE_247.Add(DW_AT_name, DW_FORM_string, 'FREEINSTANCE'+#0);
    ProgFREEINSTANCE_247.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgFREEINSTANCE_247.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgFREEINSTANCE_247.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgFREEINSTANCE_247.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgFREEINSTANCE_247.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $0E]));

      Varthis_248 := ProgFREEINSTANCE_247.GetNewChild;
      Varthis_248.Tag := DW_TAG_formal_parameter;
      Varthis_248.Children := 0;
      Varthis_248.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_248.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_248.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

    ProgSAFECALLEXCEPTION_249 := TypeTOBJECT_237.GetNewChild;
    ProgSAFECALLEXCEPTION_249.Tag := DW_TAG_subprogram;
    ProgSAFECALLEXCEPTION_249.Children := 1;
    ProgSAFECALLEXCEPTION_249.Add(DW_AT_name, DW_FORM_string, 'SAFECALLEXCEPTION'+#0);
    ProgSAFECALLEXCEPTION_249.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgSAFECALLEXCEPTION_249.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgSAFECALLEXCEPTION_249.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgSAFECALLEXCEPTION_249.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgSAFECALLEXCEPTION_249.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $0F]));
    ProgSAFECALLEXCEPTION_249.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclHRESULT_355); // $6E, $16, $00, $00

      Varthis_250 := ProgSAFECALLEXCEPTION_249.GetNewChild;
      Varthis_250.Tag := DW_TAG_formal_parameter;
      Varthis_250.Children := 0;
      Varthis_250.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_250.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_250.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

      VarEXCEPTOBJECT_251 := ProgSAFECALLEXCEPTION_249.GetNewChild;
      VarEXCEPTOBJECT_251.Tag := DW_TAG_formal_parameter;
      VarEXCEPTOBJECT_251.Children := 0;
      VarEXCEPTOBJECT_251.Add(DW_AT_name, DW_FORM_string, 'EXCEPTOBJECT'+#0);
      VarEXCEPTOBJECT_251.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

      VarEXCEPTADDR_252 := ProgSAFECALLEXCEPTION_249.GetNewChild;
      VarEXCEPTADDR_252.Tag := DW_TAG_formal_parameter;
      VarEXCEPTADDR_252.Children := 0;
      VarEXCEPTADDR_252.Add(DW_AT_name, DW_FORM_string, 'EXCEPTADDR'+#0);
      VarEXCEPTADDR_252.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_97); // $57, $08, $00, $00

    ProgDEFAULTHANDLER_253 := TypeTOBJECT_237.GetNewChild;
    ProgDEFAULTHANDLER_253.Tag := DW_TAG_subprogram;
    ProgDEFAULTHANDLER_253.Children := 1;
    ProgDEFAULTHANDLER_253.Add(DW_AT_name, DW_FORM_string, 'DEFAULTHANDLER'+#0);
    ProgDEFAULTHANDLER_253.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgDEFAULTHANDLER_253.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgDEFAULTHANDLER_253.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgDEFAULTHANDLER_253.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgDEFAULTHANDLER_253.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $10]));

      Varthis_254 := ProgDEFAULTHANDLER_253.GetNewChild;
      Varthis_254.Tag := DW_TAG_formal_parameter;
      Varthis_254.Children := 0;
      Varthis_254.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_254.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_254.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

      VarMESSAGE_255 := ProgDEFAULTHANDLER_253.GetNewChild;
      VarMESSAGE_255.Tag := DW_TAG_formal_parameter;
      VarMESSAGE_255.Children := 0;
      VarMESSAGE_255.Add(DW_AT_name, DW_FORM_string, 'MESSAGE'+#0);
      VarMESSAGE_255.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_358); // $8B, $16, $00, $00

    ProgFREE_256 := TypeTOBJECT_237.GetNewChild;
    ProgFREE_256.Tag := DW_TAG_subprogram;
    ProgFREE_256.Children := 1;
    ProgFREE_256.Add(DW_AT_name, DW_FORM_string, 'FREE'+#0);
    ProgFREE_256.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgFREE_256.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgFREE_256.Add(DW_AT_external, DW_FORM_flag, [$01]);

      Varthis_257 := ProgFREE_256.GetNewChild;
      Varthis_257.Tag := DW_TAG_formal_parameter;
      Varthis_257.Children := 0;
      Varthis_257.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_257.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_257.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

    ProgINITINSTANCE_258 := TypeTOBJECT_237.GetNewChild;
    ProgINITINSTANCE_258.Tag := DW_TAG_subprogram;
    ProgINITINSTANCE_258.Children := 1;
    ProgINITINSTANCE_258.Add(DW_AT_name, DW_FORM_string, 'INITINSTANCE'+#0);
    ProgINITINSTANCE_258.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgINITINSTANCE_258.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgINITINSTANCE_258.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgINITINSTANCE_258.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

      Varself_259 := ProgINITINSTANCE_258.GetNewChild;
      Varself_259.Tag := DW_TAG_formal_parameter;
      Varself_259.Children := 0;
      Varself_259.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_259.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_259.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_361); // $A9, $16, $00, $00

      VarINSTANCE_260 := ProgINITINSTANCE_258.GetNewChild;
      VarINSTANCE_260.Tag := DW_TAG_formal_parameter;
      VarINSTANCE_260.Children := 0;
      VarINSTANCE_260.Add(DW_AT_name, DW_FORM_string, 'INSTANCE'+#0);
      VarINSTANCE_260.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_97); // $57, $08, $00, $00

    ProgCLEANUPINSTANCE_261 := TypeTOBJECT_237.GetNewChild;
    ProgCLEANUPINSTANCE_261.Tag := DW_TAG_subprogram;
    ProgCLEANUPINSTANCE_261.Children := 1;
    ProgCLEANUPINSTANCE_261.Add(DW_AT_name, DW_FORM_string, 'CLEANUPINSTANCE'+#0);
    ProgCLEANUPINSTANCE_261.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLEANUPINSTANCE_261.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLEANUPINSTANCE_261.Add(DW_AT_external, DW_FORM_flag, [$01]);

      Varthis_262 := ProgCLEANUPINSTANCE_261.GetNewChild;
      Varthis_262.Tag := DW_TAG_formal_parameter;
      Varthis_262.Children := 0;
      Varthis_262.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_262.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_262.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

    ProgCLASSTYPE_263 := TypeTOBJECT_237.GetNewChild;
    ProgCLASSTYPE_263.Tag := DW_TAG_subprogram;
    ProgCLASSTYPE_263.Children := 1;
    ProgCLASSTYPE_263.Add(DW_AT_name, DW_FORM_string, 'CLASSTYPE'+#0);
    ProgCLASSTYPE_263.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLASSTYPE_263.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLASSTYPE_263.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCLASSTYPE_263.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTCLASS_363); // $B3, $16, $00, $00

      Varself_264 := ProgCLASSTYPE_263.GetNewChild;
      Varself_264.Tag := DW_TAG_formal_parameter;
      Varself_264.Children := 0;
      Varself_264.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_264.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_264.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_366); // $C9, $16, $00, $00

    ProgCLASSINFO_265 := TypeTOBJECT_237.GetNewChild;
    ProgCLASSINFO_265.Tag := DW_TAG_subprogram;
    ProgCLASSINFO_265.Children := 1;
    ProgCLASSINFO_265.Add(DW_AT_name, DW_FORM_string, 'CLASSINFO'+#0);
    ProgCLASSINFO_265.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLASSINFO_265.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLASSINFO_265.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCLASSINFO_265.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_97); // $57, $08, $00, $00

      Varself_266 := ProgCLASSINFO_265.GetNewChild;
      Varself_266.Tag := DW_TAG_formal_parameter;
      Varself_266.Children := 0;
      Varself_266.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_266.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_266.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_368); // $D3, $16, $00, $00

    ProgCLASSNAME_267 := TypeTOBJECT_237.GetNewChild;
    ProgCLASSNAME_267.Tag := DW_TAG_subprogram;
    ProgCLASSNAME_267.Children := 1;
    ProgCLASSNAME_267.Add(DW_AT_name, DW_FORM_string, 'CLASSNAME'+#0);
    ProgCLASSNAME_267.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLASSNAME_267.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLASSNAME_267.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCLASSNAME_267.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_370); // $DD, $16, $00, $00

      Varself_268 := ProgCLASSNAME_267.GetNewChild;
      Varself_268.Tag := DW_TAG_formal_parameter;
      Varself_268.Children := 0;
      Varself_268.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_268.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_268.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_377); // $2E, $17, $00, $00

      Varresult_269 := ProgCLASSNAME_267.GetNewChild;
      Varresult_269.Tag := DW_TAG_variable;
      Varresult_269.Children := 0;
      Varresult_269.Add(DW_AT_name, DW_FORM_string, 'result'+#0);
      Varresult_269.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_370); // $DD, $16, $00, $00

    ProgCLASSNAMEIS_270 := TypeTOBJECT_237.GetNewChild;
    ProgCLASSNAMEIS_270.Tag := DW_TAG_subprogram;
    ProgCLASSNAMEIS_270.Children := 1;
    ProgCLASSNAMEIS_270.Add(DW_AT_name, DW_FORM_string, 'CLASSNAMEIS'+#0);
    ProgCLASSNAMEIS_270.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLASSNAMEIS_270.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLASSNAMEIS_270.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCLASSNAMEIS_270.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_94); // $3A, $08, $00, $00

      Varself_271 := ProgCLASSNAMEIS_270.GetNewChild;
      Varself_271.Tag := DW_TAG_formal_parameter;
      Varself_271.Children := 0;
      Varself_271.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_271.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_271.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_379); // $38, $17, $00, $00

      VarNAME_272 := ProgCLASSNAMEIS_270.GetNewChild;
      VarNAME_272.Tag := DW_TAG_formal_parameter;
      VarNAME_272.Children := 0;
      VarNAME_272.Add(DW_AT_name, DW_FORM_string, 'NAME'+#0);
      VarNAME_272.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_370); // $DD, $16, $00, $00

    ProgCLASSPARENT_273 := TypeTOBJECT_237.GetNewChild;
    ProgCLASSPARENT_273.Tag := DW_TAG_subprogram;
    ProgCLASSPARENT_273.Children := 1;
    ProgCLASSPARENT_273.Add(DW_AT_name, DW_FORM_string, 'CLASSPARENT'+#0);
    ProgCLASSPARENT_273.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgCLASSPARENT_273.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgCLASSPARENT_273.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgCLASSPARENT_273.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTCLASS_363); // $B3, $16, $00, $00

      Varself_274 := ProgCLASSPARENT_273.GetNewChild;
      Varself_274.Tag := DW_TAG_formal_parameter;
      Varself_274.Children := 0;
      Varself_274.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_274.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_274.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_381); // $42, $17, $00, $00

    ProgINSTANCESIZE_275 := TypeTOBJECT_237.GetNewChild;
    ProgINSTANCESIZE_275.Tag := DW_TAG_subprogram;
    ProgINSTANCESIZE_275.Children := 1;
    ProgINSTANCESIZE_275.Add(DW_AT_name, DW_FORM_string, 'INSTANCESIZE'+#0);
    ProgINSTANCESIZE_275.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgINSTANCESIZE_275.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgINSTANCESIZE_275.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgINSTANCESIZE_275.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_88); // $04, $08, $00, $00

      Varself_276 := ProgINSTANCESIZE_275.GetNewChild;
      Varself_276.Tag := DW_TAG_formal_parameter;
      Varself_276.Children := 0;
      Varself_276.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_276.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_276.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_383); // $4C, $17, $00, $00

    ProgINHERITSFROM_277 := TypeTOBJECT_237.GetNewChild;
    ProgINHERITSFROM_277.Tag := DW_TAG_subprogram;
    ProgINHERITSFROM_277.Children := 1;
    ProgINHERITSFROM_277.Add(DW_AT_name, DW_FORM_string, 'INHERITSFROM'+#0);
    ProgINHERITSFROM_277.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgINHERITSFROM_277.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgINHERITSFROM_277.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgINHERITSFROM_277.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_94); // $3A, $08, $00, $00

      Varself_278 := ProgINHERITSFROM_277.GetNewChild;
      Varself_278.Tag := DW_TAG_formal_parameter;
      Varself_278.Children := 0;
      Varself_278.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_278.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_278.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_385); // $56, $17, $00, $00

      VarACLASS_279 := ProgINHERITSFROM_277.GetNewChild;
      VarACLASS_279.Tag := DW_TAG_formal_parameter;
      VarACLASS_279.Children := 0;
      VarACLASS_279.Add(DW_AT_name, DW_FORM_string, 'ACLASS'+#0);
      VarACLASS_279.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTCLASS_363); // $B3, $16, $00, $00

    ProgSTRINGMESSAGETABLE_280 := TypeTOBJECT_237.GetNewChild;
    ProgSTRINGMESSAGETABLE_280.Tag := DW_TAG_subprogram;
    ProgSTRINGMESSAGETABLE_280.Children := 1;
    ProgSTRINGMESSAGETABLE_280.Add(DW_AT_name, DW_FORM_string, 'STRINGMESSAGETABLE'+#0);
    ProgSTRINGMESSAGETABLE_280.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgSTRINGMESSAGETABLE_280.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgSTRINGMESSAGETABLE_280.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgSTRINGMESSAGETABLE_280.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSTRINGMESSAGETABLE_387); // $60, $17, $00, $00

      Varself_281 := ProgSTRINGMESSAGETABLE_280.GetNewChild;
      Varself_281.Tag := DW_TAG_formal_parameter;
      Varself_281.Children := 0;
      Varself_281.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_281.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_281.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_390); // $83, $17, $00, $00

    ProgMETHODADDRESS_282 := TypeTOBJECT_237.GetNewChild;
    ProgMETHODADDRESS_282.Tag := DW_TAG_subprogram;
    ProgMETHODADDRESS_282.Children := 1;
    ProgMETHODADDRESS_282.Add(DW_AT_name, DW_FORM_string, 'METHODADDRESS'+#0);
    ProgMETHODADDRESS_282.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgMETHODADDRESS_282.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgMETHODADDRESS_282.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgMETHODADDRESS_282.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_97); // $57, $08, $00, $00

      Varself_283 := ProgMETHODADDRESS_282.GetNewChild;
      Varself_283.Tag := DW_TAG_formal_parameter;
      Varself_283.Children := 0;
      Varself_283.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_283.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_283.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_392); // $8D, $17, $00, $00

      VarNAME_284 := ProgMETHODADDRESS_282.GetNewChild;
      VarNAME_284.Tag := DW_TAG_formal_parameter;
      VarNAME_284.Children := 0;
      VarNAME_284.Add(DW_AT_name, DW_FORM_string, 'NAME'+#0);
      VarNAME_284.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_370); // $DD, $16, $00, $00

    ProgMETHODNAME_285 := TypeTOBJECT_237.GetNewChild;
    ProgMETHODNAME_285.Tag := DW_TAG_subprogram;
    ProgMETHODNAME_285.Children := 1;
    ProgMETHODNAME_285.Add(DW_AT_name, DW_FORM_string, 'METHODNAME'+#0);
    ProgMETHODNAME_285.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgMETHODNAME_285.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgMETHODNAME_285.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgMETHODNAME_285.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_370); // $DD, $16, $00, $00

      Varself_286 := ProgMETHODNAME_285.GetNewChild;
      Varself_286.Tag := DW_TAG_formal_parameter;
      Varself_286.Children := 0;
      Varself_286.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_286.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_286.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_394); // $97, $17, $00, $00

      VarADDRESS_287 := ProgMETHODNAME_285.GetNewChild;
      VarADDRESS_287.Tag := DW_TAG_formal_parameter;
      VarADDRESS_287.Children := 0;
      VarADDRESS_287.Add(DW_AT_name, DW_FORM_string, 'ADDRESS'+#0);
      VarADDRESS_287.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_97); // $57, $08, $00, $00

      Varresult_288 := ProgMETHODNAME_285.GetNewChild;
      Varresult_288.Tag := DW_TAG_variable;
      Varresult_288.Children := 0;
      Varresult_288.Add(DW_AT_name, DW_FORM_string, 'result'+#0);
      Varresult_288.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_370); // $DD, $16, $00, $00

    ProgFIELDADDRESS_289 := TypeTOBJECT_237.GetNewChild;
    ProgFIELDADDRESS_289.Tag := DW_TAG_subprogram;
    ProgFIELDADDRESS_289.Children := 1;
    ProgFIELDADDRESS_289.Add(DW_AT_name, DW_FORM_string, 'FIELDADDRESS'+#0);
    ProgFIELDADDRESS_289.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgFIELDADDRESS_289.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgFIELDADDRESS_289.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgFIELDADDRESS_289.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_97); // $57, $08, $00, $00

      Varthis_290 := ProgFIELDADDRESS_289.GetNewChild;
      Varthis_290.Tag := DW_TAG_formal_parameter;
      Varthis_290.Children := 0;
      Varthis_290.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_290.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_290.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

      VarNAME_291 := ProgFIELDADDRESS_289.GetNewChild;
      VarNAME_291.Tag := DW_TAG_formal_parameter;
      VarNAME_291.Children := 0;
      VarNAME_291.Add(DW_AT_name, DW_FORM_string, 'NAME'+#0);
      VarNAME_291.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_370); // $DD, $16, $00, $00

    ProgAFTERCONSTRUCTION_292 := TypeTOBJECT_237.GetNewChild;
    ProgAFTERCONSTRUCTION_292.Tag := DW_TAG_subprogram;
    ProgAFTERCONSTRUCTION_292.Children := 1;
    ProgAFTERCONSTRUCTION_292.Add(DW_AT_name, DW_FORM_string, 'AFTERCONSTRUCTION'+#0);
    ProgAFTERCONSTRUCTION_292.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgAFTERCONSTRUCTION_292.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgAFTERCONSTRUCTION_292.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgAFTERCONSTRUCTION_292.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgAFTERCONSTRUCTION_292.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $11]));

      Varthis_293 := ProgAFTERCONSTRUCTION_292.GetNewChild;
      Varthis_293.Tag := DW_TAG_formal_parameter;
      Varthis_293.Children := 0;
      Varthis_293.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_293.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_293.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

    ProgBEFOREDESTRUCTION_294 := TypeTOBJECT_237.GetNewChild;
    ProgBEFOREDESTRUCTION_294.Tag := DW_TAG_subprogram;
    ProgBEFOREDESTRUCTION_294.Children := 1;
    ProgBEFOREDESTRUCTION_294.Add(DW_AT_name, DW_FORM_string, 'BEFOREDESTRUCTION'+#0);
    ProgBEFOREDESTRUCTION_294.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgBEFOREDESTRUCTION_294.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgBEFOREDESTRUCTION_294.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgBEFOREDESTRUCTION_294.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgBEFOREDESTRUCTION_294.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $12]));

      Varthis_295 := ProgBEFOREDESTRUCTION_294.GetNewChild;
      Varthis_295.Tag := DW_TAG_formal_parameter;
      Varthis_295.Children := 0;
      Varthis_295.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_295.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_295.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

    ProgDEFAULTHANDLERSTR_296 := TypeTOBJECT_237.GetNewChild;
    ProgDEFAULTHANDLERSTR_296.Tag := DW_TAG_subprogram;
    ProgDEFAULTHANDLERSTR_296.Children := 1;
    ProgDEFAULTHANDLERSTR_296.Add(DW_AT_name, DW_FORM_string, 'DEFAULTHANDLERSTR'+#0);
    ProgDEFAULTHANDLERSTR_296.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgDEFAULTHANDLERSTR_296.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgDEFAULTHANDLERSTR_296.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgDEFAULTHANDLERSTR_296.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgDEFAULTHANDLERSTR_296.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $13]));

      Varthis_297 := ProgDEFAULTHANDLERSTR_296.GetNewChild;
      Varthis_297.Tag := DW_TAG_formal_parameter;
      Varthis_297.Children := 0;
      Varthis_297.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_297.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_297.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

      VarMESSAGE_298 := ProgDEFAULTHANDLERSTR_296.GetNewChild;
      VarMESSAGE_298.Tag := DW_TAG_formal_parameter;
      VarMESSAGE_298.Children := 0;
      VarMESSAGE_298.Add(DW_AT_name, DW_FORM_string, 'MESSAGE'+#0);
      VarMESSAGE_298.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_358); // $8B, $16, $00, $00

    ProgDISPATCH_299 := TypeTOBJECT_237.GetNewChild;
    ProgDISPATCH_299.Tag := DW_TAG_subprogram;
    ProgDISPATCH_299.Children := 1;
    ProgDISPATCH_299.Add(DW_AT_name, DW_FORM_string, 'DISPATCH'+#0);
    ProgDISPATCH_299.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgDISPATCH_299.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgDISPATCH_299.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgDISPATCH_299.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgDISPATCH_299.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $14]));

      Varthis_300 := ProgDISPATCH_299.GetNewChild;
      Varthis_300.Tag := DW_TAG_formal_parameter;
      Varthis_300.Children := 0;
      Varthis_300.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_300.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_300.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

      VarMESSAGE_301 := ProgDISPATCH_299.GetNewChild;
      VarMESSAGE_301.Tag := DW_TAG_formal_parameter;
      VarMESSAGE_301.Children := 0;
      VarMESSAGE_301.Add(DW_AT_name, DW_FORM_string, 'MESSAGE'+#0);
      VarMESSAGE_301.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_358); // $8B, $16, $00, $00

    ProgDISPATCHSTR_302 := TypeTOBJECT_237.GetNewChild;
    ProgDISPATCHSTR_302.Tag := DW_TAG_subprogram;
    ProgDISPATCHSTR_302.Children := 1;
    ProgDISPATCHSTR_302.Add(DW_AT_name, DW_FORM_string, 'DISPATCHSTR'+#0);
    ProgDISPATCHSTR_302.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgDISPATCHSTR_302.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgDISPATCHSTR_302.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgDISPATCHSTR_302.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgDISPATCHSTR_302.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $15]));

      Varthis_303 := ProgDISPATCHSTR_302.GetNewChild;
      Varthis_303.Tag := DW_TAG_formal_parameter;
      Varthis_303.Children := 0;
      Varthis_303.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_303.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_303.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

      VarMESSAGE_304 := ProgDISPATCHSTR_302.GetNewChild;
      VarMESSAGE_304.Tag := DW_TAG_formal_parameter;
      VarMESSAGE_304.Children := 0;
      VarMESSAGE_304.Add(DW_AT_name, DW_FORM_string, 'MESSAGE'+#0);
      VarMESSAGE_304.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_358); // $8B, $16, $00, $00

    ProgGETINTERFACE_305 := TypeTOBJECT_237.GetNewChild;
    ProgGETINTERFACE_305.Tag := DW_TAG_subprogram;
    ProgGETINTERFACE_305.Children := 1;
    ProgGETINTERFACE_305.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACE'+#0);
    ProgGETINTERFACE_305.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACE_305.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACE_305.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACE_305.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_94); // $3A, $08, $00, $00

      Varthis_306 := ProgGETINTERFACE_305.GetNewChild;
      Varthis_306.Tag := DW_TAG_formal_parameter;
      Varthis_306.Children := 0;
      Varthis_306.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_306.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_306.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

      VarIID_307 := ProgGETINTERFACE_305.GetNewChild;
      VarIID_307.Tag := DW_TAG_formal_parameter;
      VarIID_307.Children := 0;
      VarIID_307.Add(DW_AT_name, DW_FORM_string, 'IID'+#0);
      VarIID_307.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTGUID_396); // $A1, $17, $00, $00

      VarOBJ_308 := ProgGETINTERFACE_305.GetNewChild;
      VarOBJ_308.Tag := DW_TAG_formal_parameter;
      VarOBJ_308.Children := 0;
      VarOBJ_308.Add(DW_AT_name, DW_FORM_string, 'OBJ'+#0);
      VarOBJ_308.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_358); // $8B, $16, $00, $00

    ProgGETINTERFACE_309 := TypeTOBJECT_237.GetNewChild;
    ProgGETINTERFACE_309.Tag := DW_TAG_subprogram;
    ProgGETINTERFACE_309.Children := 1;
    ProgGETINTERFACE_309.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACE'+#0);
    ProgGETINTERFACE_309.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACE_309.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACE_309.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACE_309.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_94); // $3A, $08, $00, $00

      Varthis_310 := ProgGETINTERFACE_309.GetNewChild;
      Varthis_310.Tag := DW_TAG_formal_parameter;
      Varthis_310.Children := 0;
      Varthis_310.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_310.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_310.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

      VarIIDSTR_311 := ProgGETINTERFACE_309.GetNewChild;
      VarIIDSTR_311.Tag := DW_TAG_formal_parameter;
      VarIIDSTR_311.Children := 0;
      VarIIDSTR_311.Add(DW_AT_name, DW_FORM_string, 'IIDSTR'+#0);
      VarIIDSTR_311.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_370); // $DD, $16, $00, $00

      VarOBJ_312 := ProgGETINTERFACE_309.GetNewChild;
      VarOBJ_312.Tag := DW_TAG_formal_parameter;
      VarOBJ_312.Children := 0;
      VarOBJ_312.Add(DW_AT_name, DW_FORM_string, 'OBJ'+#0);
      VarOBJ_312.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_358); // $8B, $16, $00, $00

    ProgGETINTERFACEBYSTR_313 := TypeTOBJECT_237.GetNewChild;
    ProgGETINTERFACEBYSTR_313.Tag := DW_TAG_subprogram;
    ProgGETINTERFACEBYSTR_313.Children := 1;
    ProgGETINTERFACEBYSTR_313.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACEBYSTR'+#0);
    ProgGETINTERFACEBYSTR_313.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACEBYSTR_313.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACEBYSTR_313.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACEBYSTR_313.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_94); // $3A, $08, $00, $00

      Varthis_314 := ProgGETINTERFACEBYSTR_313.GetNewChild;
      Varthis_314.Tag := DW_TAG_formal_parameter;
      Varthis_314.Children := 0;
      Varthis_314.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_314.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_314.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

      VarIIDSTR_315 := ProgGETINTERFACEBYSTR_313.GetNewChild;
      VarIIDSTR_315.Tag := DW_TAG_formal_parameter;
      VarIIDSTR_315.Children := 0;
      VarIIDSTR_315.Add(DW_AT_name, DW_FORM_string, 'IIDSTR'+#0);
      VarIIDSTR_315.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_370); // $DD, $16, $00, $00

      VarOBJ_316 := ProgGETINTERFACEBYSTR_313.GetNewChild;
      VarOBJ_316.Tag := DW_TAG_formal_parameter;
      VarOBJ_316.Children := 0;
      VarOBJ_316.Add(DW_AT_name, DW_FORM_string, 'OBJ'+#0);
      VarOBJ_316.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_358); // $8B, $16, $00, $00

    ProgGETINTERFACEWEAK_317 := TypeTOBJECT_237.GetNewChild;
    ProgGETINTERFACEWEAK_317.Tag := DW_TAG_subprogram;
    ProgGETINTERFACEWEAK_317.Children := 1;
    ProgGETINTERFACEWEAK_317.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACEWEAK'+#0);
    ProgGETINTERFACEWEAK_317.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACEWEAK_317.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACEWEAK_317.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACEWEAK_317.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_94); // $3A, $08, $00, $00

      Varthis_318 := ProgGETINTERFACEWEAK_317.GetNewChild;
      Varthis_318.Tag := DW_TAG_formal_parameter;
      Varthis_318.Children := 0;
      Varthis_318.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_318.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_318.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

      VarIID_319 := ProgGETINTERFACEWEAK_317.GetNewChild;
      VarIID_319.Tag := DW_TAG_formal_parameter;
      VarIID_319.Children := 0;
      VarIID_319.Add(DW_AT_name, DW_FORM_string, 'IID'+#0);
      VarIID_319.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTGUID_396); // $A1, $17, $00, $00

      VarOBJ_320 := ProgGETINTERFACEWEAK_317.GetNewChild;
      VarOBJ_320.Tag := DW_TAG_formal_parameter;
      VarOBJ_320.Children := 0;
      VarOBJ_320.Add(DW_AT_name, DW_FORM_string, 'OBJ'+#0);
      VarOBJ_320.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_358); // $8B, $16, $00, $00

    ProgGETINTERFACEENTRY_321 := TypeTOBJECT_237.GetNewChild;
    ProgGETINTERFACEENTRY_321.Tag := DW_TAG_subprogram;
    ProgGETINTERFACEENTRY_321.Children := 1;
    ProgGETINTERFACEENTRY_321.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACEENTRY'+#0);
    ProgGETINTERFACEENTRY_321.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACEENTRY_321.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACEENTRY_321.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACEENTRY_321.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTERFACEENTRY_413); // $A1, $18, $00, $00

      Varself_322 := ProgGETINTERFACEENTRY_321.GetNewChild;
      Varself_322.Tag := DW_TAG_formal_parameter;
      Varself_322.Children := 0;
      Varself_322.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_322.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_322.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_416); // $C0, $18, $00, $00

      VarIID_323 := ProgGETINTERFACEENTRY_321.GetNewChild;
      VarIID_323.Tag := DW_TAG_formal_parameter;
      VarIID_323.Children := 0;
      VarIID_323.Add(DW_AT_name, DW_FORM_string, 'IID'+#0);
      VarIID_323.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTGUID_396); // $A1, $17, $00, $00

    ProgGETINTERFACEENTRYBYSTR_324 := TypeTOBJECT_237.GetNewChild;
    ProgGETINTERFACEENTRYBYSTR_324.Tag := DW_TAG_subprogram;
    ProgGETINTERFACEENTRYBYSTR_324.Children := 1;
    ProgGETINTERFACEENTRYBYSTR_324.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACEENTRYBYSTR'+#0);
    ProgGETINTERFACEENTRYBYSTR_324.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACEENTRYBYSTR_324.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACEENTRYBYSTR_324.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACEENTRYBYSTR_324.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTERFACEENTRY_413); // $A1, $18, $00, $00

      Varself_325 := ProgGETINTERFACEENTRYBYSTR_324.GetNewChild;
      Varself_325.Tag := DW_TAG_formal_parameter;
      Varself_325.Children := 0;
      Varself_325.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_325.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_325.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_418); // $CA, $18, $00, $00

      VarIIDSTR_326 := ProgGETINTERFACEENTRYBYSTR_324.GetNewChild;
      VarIIDSTR_326.Tag := DW_TAG_formal_parameter;
      VarIIDSTR_326.Children := 0;
      VarIIDSTR_326.Add(DW_AT_name, DW_FORM_string, 'IIDSTR'+#0);
      VarIIDSTR_326.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_370); // $DD, $16, $00, $00

    ProgGETINTERFACETABLE_327 := TypeTOBJECT_237.GetNewChild;
    ProgGETINTERFACETABLE_327.Tag := DW_TAG_subprogram;
    ProgGETINTERFACETABLE_327.Children := 1;
    ProgGETINTERFACETABLE_327.Add(DW_AT_name, DW_FORM_string, 'GETINTERFACETABLE'+#0);
    ProgGETINTERFACETABLE_327.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETINTERFACETABLE_327.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETINTERFACETABLE_327.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETINTERFACETABLE_327.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTERFACETABLE_420); // $D4, $18, $00, $00

      Varself_328 := ProgGETINTERFACETABLE_327.GetNewChild;
      Varself_328.Tag := DW_TAG_formal_parameter;
      Varself_328.Children := 0;
      Varself_328.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_328.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_328.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_423); // $F3, $18, $00, $00

    ProgUNITNAME_329 := TypeTOBJECT_237.GetNewChild;
    ProgUNITNAME_329.Tag := DW_TAG_subprogram;
    ProgUNITNAME_329.Children := 1;
    ProgUNITNAME_329.Add(DW_AT_name, DW_FORM_string, 'UNITNAME'+#0);
    ProgUNITNAME_329.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgUNITNAME_329.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgUNITNAME_329.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgUNITNAME_329.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclANSISTRING_425); // $FD, $18, $00, $00

      Varself_330 := ProgUNITNAME_329.GetNewChild;
      Varself_330.Tag := DW_TAG_formal_parameter;
      Varself_330.Children := 0;
      Varself_330.Add(DW_AT_name, DW_FORM_string, 'self'+#0);
      Varself_330.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varself_330.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_428); // $17, $19, $00, $00

      Varresult_331 := ProgUNITNAME_329.GetNewChild;
      Varresult_331.Tag := DW_TAG_variable;
      Varresult_331.Children := 0;
      Varresult_331.Add(DW_AT_name, DW_FORM_string, 'result'+#0);
      Varresult_331.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclANSISTRING_425); // $FD, $18, $00, $00

    ProgEQUALS_332 := TypeTOBJECT_237.GetNewChild;
    ProgEQUALS_332.Tag := DW_TAG_subprogram;
    ProgEQUALS_332.Children := 1;
    ProgEQUALS_332.Add(DW_AT_name, DW_FORM_string, 'EQUALS'+#0);
    ProgEQUALS_332.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgEQUALS_332.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgEQUALS_332.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgEQUALS_332.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgEQUALS_332.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $16]));
    ProgEQUALS_332.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_94); // $3A, $08, $00, $00

      Varthis_333 := ProgEQUALS_332.GetNewChild;
      Varthis_333.Tag := DW_TAG_formal_parameter;
      Varthis_333.Children := 0;
      Varthis_333.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_333.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_333.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

      VarOBJ_334 := ProgEQUALS_332.GetNewChild;
      VarOBJ_334.Tag := DW_TAG_formal_parameter;
      VarOBJ_334.Children := 0;
      VarOBJ_334.Add(DW_AT_name, DW_FORM_string, 'OBJ'+#0);
      VarOBJ_334.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

    ProgGETHASHCODE_335 := TypeTOBJECT_237.GetNewChild;
    ProgGETHASHCODE_335.Tag := DW_TAG_subprogram;
    ProgGETHASHCODE_335.Children := 1;
    ProgGETHASHCODE_335.Add(DW_AT_name, DW_FORM_string, 'GETHASHCODE'+#0);
    ProgGETHASHCODE_335.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgGETHASHCODE_335.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgGETHASHCODE_335.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgGETHASHCODE_335.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgGETHASHCODE_335.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $17]));
    ProgGETHASHCODE_335.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_88); // $04, $08, $00, $00

      Varthis_336 := ProgGETHASHCODE_335.GetNewChild;
      Varthis_336.Tag := DW_TAG_formal_parameter;
      Varthis_336.Children := 0;
      Varthis_336.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_336.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_336.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

    ProgTOSTRING_337 := TypeTOBJECT_237.GetNewChild;
    ProgTOSTRING_337.Tag := DW_TAG_subprogram;
    ProgTOSTRING_337.Children := 1;
    ProgTOSTRING_337.Add(DW_AT_name, DW_FORM_string, 'TOSTRING'+#0);
    ProgTOSTRING_337.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
    ProgTOSTRING_337.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
    ProgTOSTRING_337.Add(DW_AT_external, DW_FORM_flag, [$01]);
    ProgTOSTRING_337.Add(DW_AT_virtuality, DW_FORM_data1, [$01]);
    ProgTOSTRING_337.Add(DW_AT_vtable_elem_location, DW_FORM_block1, BytesLen1([$10, $18]));
    ProgTOSTRING_337.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclANSISTRING_425); // $FD, $18, $00, $00

      Varthis_338 := ProgTOSTRING_337.GetNewChild;
      Varthis_338.Tag := DW_TAG_formal_parameter;
      Varthis_338.Children := 0;
      Varthis_338.Add(DW_AT_name, DW_FORM_string, 'this'+#0);
      Varthis_338.Add(DW_AT_artificial, DW_FORM_flag, [$01]);
      Varthis_338.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

      Varresult_339 := ProgTOSTRING_337.GetNewChild;
      Varresult_339.Tag := DW_TAG_variable;
      Varresult_339.Children := 0;
      Varresult_339.Add(DW_AT_name, DW_FORM_string, 'result'+#0);
      Varresult_339.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclANSISTRING_425); // $FD, $18, $00, $00

  Type_340 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_340.Tag := DW_TAG_reference_type;
  Type_340.Children := 0;
  Type_340.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTOBJECT_235); // $B6, $0F, $00, $00

  TypeDeclINT64_341 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclINT64_341.Tag := DW_TAG_typedef;
  TypeDeclINT64_341.Children := 0;
  TypeDeclINT64_341.Add(DW_AT_name, DW_FORM_string, 'INT64'+#0);
  TypeDeclINT64_341.AddRef(DW_AT_type, DW_FORM_ref4, @TypeInt64_342); // $FB, $15, $00, $00

  TypeInt64_342 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeInt64_342.Tag := DW_TAG_base_type;
  TypeInt64_342.Children := 0;
  TypeInt64_342.Add(DW_AT_name, DW_FORM_string, 'Int64'+#0);
  TypeInt64_342.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeInt64_342.Add(DW_AT_byte_size, DW_FORM_data1, [$08]);

  Type_343 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_343.Tag := DW_TAG_reference_type;
  Type_343.Children := 0;
  Type_343.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclINT64_341); // $F0, $15, $00, $00

  TypeDecl__vtbl_ptr_type_344 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDecl__vtbl_ptr_type_344.Tag := DW_TAG_typedef;
  TypeDecl__vtbl_ptr_type_344.Children := 0;
  TypeDecl__vtbl_ptr_type_344.Add(DW_AT_name, DW_FORM_string, '__vtbl_ptr_type'+#0);
  TypeDecl__vtbl_ptr_type_344.AddRef(DW_AT_type, DW_FORM_ref4, @Type_345); // $1E, $16, $00, $00

  Type_345 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_345.Tag := DW_TAG_structure_type;
  Type_345.Children := 0;
  Type_345.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 20);

  Type_346 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_346.Tag := DW_TAG_reference_type;
  Type_346.Children := 0;
  Type_346.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  TypeDeclLONGBOOL_347 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclLONGBOOL_347.Tag := DW_TAG_typedef;
  TypeDeclLONGBOOL_347.Children := 0;
  TypeDeclLONGBOOL_347.Add(DW_AT_name, DW_FORM_string, 'LONGBOOL'+#0);
  TypeDeclLONGBOOL_347.AddRef(DW_AT_type, DW_FORM_ref4, @TypeLongBool_348); // $34, $16, $00, $00

  TypeLongBool_348 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeLongBool_348.Tag := DW_TAG_base_type;
  TypeLongBool_348.Children := 0;
  TypeLongBool_348.Add(DW_AT_name, DW_FORM_string, 'LongBool'+#0);
  TypeLongBool_348.Add(DW_AT_encoding, DW_FORM_data1, [$02]);
  TypeLongBool_348.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

  Type_349 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_349.Tag := DW_TAG_reference_type;
  Type_349.Children := 0;
  Type_349.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGBOOL_347); // $26, $16, $00, $00

  TypeDeclBYTEBOOL_350 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclBYTEBOOL_350.Tag := DW_TAG_typedef;
  TypeDeclBYTEBOOL_350.Children := 0;
  TypeDeclBYTEBOOL_350.Add(DW_AT_name, DW_FORM_string, 'BYTEBOOL'+#0);
  TypeDeclBYTEBOOL_350.AddRef(DW_AT_type, DW_FORM_ref4, @TypeByteBool_351); // $53, $16, $00, $00

  TypeByteBool_351 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeByteBool_351.Tag := DW_TAG_base_type;
  TypeByteBool_351.Children := 0;
  TypeByteBool_351.Add(DW_AT_name, DW_FORM_string, 'ByteBool'+#0);
  TypeByteBool_351.Add(DW_AT_encoding, DW_FORM_data1, [$02]);
  TypeByteBool_351.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_352 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_352.Tag := DW_TAG_reference_type;
  Type_352.Children := 0;
  Type_352.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTEBOOL_350); // $45, $16, $00, $00

  TypePtr_353 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_353.Tag := DW_TAG_pointer_type;
  TypePtr_353.Children := 0;
  TypePtr_353.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_354 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_354.Tag := DW_TAG_reference_type;
  Type_354.Children := 0;
  Type_354.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_353); // $64, $16, $00, $00

  TypeDeclHRESULT_355 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclHRESULT_355.Tag := DW_TAG_typedef;
  TypeDeclHRESULT_355.Children := 0;
  TypeDeclHRESULT_355.Add(DW_AT_name, DW_FORM_string, 'HRESULT'+#0);
  TypeDeclHRESULT_355.AddRef(DW_AT_type, DW_FORM_ref4, @TypeHRESULT_356); // $7B, $16, $00, $00

  TypeHRESULT_356 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeHRESULT_356.Tag := DW_TAG_base_type;
  TypeHRESULT_356.Children := 0;
  TypeHRESULT_356.Add(DW_AT_name, DW_FORM_string, 'HRESULT'+#0);
  TypeHRESULT_356.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeHRESULT_356.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

  Type_357 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_357.Tag := DW_TAG_reference_type;
  Type_357.Children := 0;
  Type_357.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclHRESULT_355); // $6E, $16, $00, $00

  TypeDeclformal_358 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclformal_358.Tag := DW_TAG_typedef;
  TypeDeclformal_358.Children := 0;
  TypeDeclformal_358.Add(DW_AT_name, DW_FORM_string, 'formal'+#0);
  TypeDeclformal_358.AddRef(DW_AT_type, DW_FORM_ref4, @TypeFormalDef_359); // $97, $16, $00, $00

  TypeFormalDef_359 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeFormalDef_359.Tag := DW_TAG_base_type;
  TypeFormalDef_359.Children := 0;
  TypeFormalDef_359.Add(DW_AT_name, DW_FORM_string, 'FormalDef'+#0);
  TypeFormalDef_359.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeFormalDef_359.Add(DW_AT_byte_size, DW_FORM_data1, [$00]);

  Type_360 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_360.Tag := DW_TAG_reference_type;
  Type_360.Children := 0;
  Type_360.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclformal_358); // $8B, $16, $00, $00

  TypePtr_361 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_361.Tag := DW_TAG_pointer_type;
  TypePtr_361.Children := 0;
  TypePtr_361.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_362 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_362.Tag := DW_TAG_reference_type;
  Type_362.Children := 0;
  Type_362.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_361); // $A9, $16, $00, $00

  TypeDeclTCLASS_363 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTCLASS_363.Tag := DW_TAG_typedef;
  TypeDeclTCLASS_363.Children := 0;
  TypeDeclTCLASS_363.Add(DW_AT_name, DW_FORM_string, 'TCLASS'+#0);
  TypeDeclTCLASS_363.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_364); // $BF, $16, $00, $00

  TypePtr_364 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_364.Tag := DW_TAG_pointer_type;
  TypePtr_364.Children := 0;
  TypePtr_364.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_365 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_365.Tag := DW_TAG_reference_type;
  Type_365.Children := 0;
  Type_365.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTCLASS_363); // $B3, $16, $00, $00

  TypePtr_366 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_366.Tag := DW_TAG_pointer_type;
  TypePtr_366.Children := 0;
  TypePtr_366.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_367 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_367.Tag := DW_TAG_reference_type;
  Type_367.Children := 0;
  Type_367.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_366); // $C9, $16, $00, $00

  TypePtr_368 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_368.Tag := DW_TAG_pointer_type;
  TypePtr_368.Children := 0;
  TypePtr_368.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_369 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_369.Tag := DW_TAG_reference_type;
  Type_369.Children := 0;
  Type_369.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_368); // $D3, $16, $00, $00

  TypeDeclSHORTSTRING_370 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclSHORTSTRING_370.Tag := DW_TAG_typedef;
  TypeDeclSHORTSTRING_370.Children := 0;
  TypeDeclSHORTSTRING_370.Add(DW_AT_name, DW_FORM_string, 'SHORTSTRING'+#0);
  TypeDeclSHORTSTRING_370.AddRef(DW_AT_type, DW_FORM_ref4, @TypeShortString_371); // $EE, $16, $00, $00

  TypeShortString_371 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeShortString_371.Tag := DW_TAG_structure_type;
  TypeShortString_371.Children := 1;
  TypeShortString_371.Add(DW_AT_name, DW_FORM_string, 'ShortString'+#0);
  TypeShortString_371.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 256);

    Varlength_372 := TypeShortString_371.GetNewChild;
    Varlength_372.Tag := DW_TAG_member;
    Varlength_372.Children := 0;
    Varlength_372.Add(DW_AT_name, DW_FORM_string, 'length'+#0);
    Varlength_372.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    Varlength_372.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_79); // $B7, $07, $00, $00

    Varst_373 := TypeShortString_371.GetNewChild;
    Varst_373.Tag := DW_TAG_member;
    Varst_373.Children := 0;
    Varst_373.Add(DW_AT_name, DW_FORM_string, 'st'+#0);
    Varst_373.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(1)])); // $23, $01
    Varst_373.AddRef(DW_AT_type, DW_FORM_ref4, @Type_374); // $18, $17, $00, $00

  Type_374 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_374.Tag := DW_TAG_array_type;
  Type_374.Children := 1;
  Type_374.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 256);
  Type_374.AddSLEB(DW_AT_byte_stride, DW_FORM_udata, 1);
  Type_374.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclCHAR_430); // $21, $19, $00, $00

    Type_375 := Type_374.GetNewChild;
    Type_375.Tag := DW_TAG_subrange_type;
    Type_375.Children := 0;
    Type_375.AddSLEB(DW_AT_lower_bound, DW_FORM_udata, 0);
    Type_375.AddSLEB(DW_AT_upper_bound, DW_FORM_udata, 255);
    Type_375.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_79); // $B7, $07, $00, $00

  Type_376 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_376.Tag := DW_TAG_reference_type;
  Type_376.Children := 0;
  Type_376.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_370); // $DD, $16, $00, $00

  TypePtr_377 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_377.Tag := DW_TAG_pointer_type;
  TypePtr_377.Children := 0;
  TypePtr_377.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_378 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_378.Tag := DW_TAG_reference_type;
  Type_378.Children := 0;
  Type_378.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_377); // $2E, $17, $00, $00

  TypePtr_379 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_379.Tag := DW_TAG_pointer_type;
  TypePtr_379.Children := 0;
  TypePtr_379.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_380 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_380.Tag := DW_TAG_reference_type;
  Type_380.Children := 0;
  Type_380.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_379); // $38, $17, $00, $00

  TypePtr_381 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_381.Tag := DW_TAG_pointer_type;
  TypePtr_381.Children := 0;
  TypePtr_381.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_382 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_382.Tag := DW_TAG_reference_type;
  Type_382.Children := 0;
  Type_382.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_381); // $42, $17, $00, $00

  TypePtr_383 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_383.Tag := DW_TAG_pointer_type;
  TypePtr_383.Children := 0;
  TypePtr_383.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_384 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_384.Tag := DW_TAG_reference_type;
  Type_384.Children := 0;
  Type_384.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_383); // $4C, $17, $00, $00

  TypePtr_385 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_385.Tag := DW_TAG_pointer_type;
  TypePtr_385.Children := 0;
  TypePtr_385.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_386 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_386.Tag := DW_TAG_reference_type;
  Type_386.Children := 0;
  Type_386.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_385); // $56, $17, $00, $00

  TypeDeclPSTRINGMESSAGETABLE_387 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPSTRINGMESSAGETABLE_387.Tag := DW_TAG_typedef;
  TypeDeclPSTRINGMESSAGETABLE_387.Children := 0;
  TypeDeclPSTRINGMESSAGETABLE_387.Add(DW_AT_name, DW_FORM_string, 'PSTRINGMESSAGETABLE'+#0);
  TypeDeclPSTRINGMESSAGETABLE_387.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_388); // $79, $17, $00, $00

  TypePtr_388 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_388.Tag := DW_TAG_pointer_type;
  TypePtr_388.Children := 0;
  TypePtr_388.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSTRINGMESSAGETABLE_433); // $38, $19, $00, $00

  Type_389 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_389.Tag := DW_TAG_reference_type;
  Type_389.Children := 0;
  Type_389.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSTRINGMESSAGETABLE_387); // $60, $17, $00, $00

  TypePtr_390 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_390.Tag := DW_TAG_pointer_type;
  TypePtr_390.Children := 0;
  TypePtr_390.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_391 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_391.Tag := DW_TAG_reference_type;
  Type_391.Children := 0;
  Type_391.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_390); // $83, $17, $00, $00

  TypePtr_392 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_392.Tag := DW_TAG_pointer_type;
  TypePtr_392.Children := 0;
  TypePtr_392.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_393 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_393.Tag := DW_TAG_reference_type;
  Type_393.Children := 0;
  Type_393.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_392); // $8D, $17, $00, $00

  TypePtr_394 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_394.Tag := DW_TAG_pointer_type;
  TypePtr_394.Children := 0;
  TypePtr_394.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_395 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_395.Tag := DW_TAG_reference_type;
  Type_395.Children := 0;
  Type_395.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_394); // $97, $17, $00, $00

  TypeDeclTGUID_396 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTGUID_396.Tag := DW_TAG_typedef;
  TypeDeclTGUID_396.Children := 0;
  TypeDeclTGUID_396.Add(DW_AT_name, DW_FORM_string, 'TGUID'+#0);
  TypeDeclTGUID_396.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTGUID_397); // $AC, $17, $00, $00

  TypeTGUID_397 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTGUID_397.Tag := DW_TAG_structure_type;
  TypeTGUID_397.Children := 1;
  TypeTGUID_397.Add(DW_AT_name, DW_FORM_string, 'TGUID'+#0);
  TypeTGUID_397.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 16);

    VarDATA1_398 := TypeTGUID_397.GetNewChild;
    VarDATA1_398.Tag := DW_TAG_member;
    VarDATA1_398.Children := 0;
    VarDATA1_398.Add(DW_AT_name, DW_FORM_string, 'DATA1'+#0);
    VarDATA1_398.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarDATA1_398.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_438); // $8F, $19, $00, $00

    VarDATA2_399 := TypeTGUID_397.GetNewChild;
    VarDATA2_399.Tag := DW_TAG_member;
    VarDATA2_399.Children := 0;
    VarDATA2_399.Add(DW_AT_name, DW_FORM_string, 'DATA2'+#0);
    VarDATA2_399.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarDATA2_399.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_85); // $ED, $07, $00, $00

    VarDATA3_400 := TypeTGUID_397.GetNewChild;
    VarDATA3_400.Tag := DW_TAG_member;
    VarDATA3_400.Children := 0;
    VarDATA3_400.Add(DW_AT_name, DW_FORM_string, 'DATA3'+#0);
    VarDATA3_400.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(6)])); // $23, $06
    VarDATA3_400.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_85); // $ED, $07, $00, $00

    VarDATA4_401 := TypeTGUID_397.GetNewChild;
    VarDATA4_401.Tag := DW_TAG_member;
    VarDATA4_401.Children := 0;
    VarDATA4_401.Add(DW_AT_name, DW_FORM_string, 'DATA4'+#0);
    VarDATA4_401.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(8)])); // $23, $08
    VarDATA4_401.AddRef(DW_AT_type, DW_FORM_ref4, @Type_441); // $AE, $19, $00, $00

    VarD1_402 := TypeTGUID_397.GetNewChild;
    VarD1_402.Tag := DW_TAG_member;
    VarD1_402.Children := 0;
    VarD1_402.Add(DW_AT_name, DW_FORM_string, 'D1'+#0);
    VarD1_402.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarD1_402.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_438); // $8F, $19, $00, $00

    VarD2_403 := TypeTGUID_397.GetNewChild;
    VarD2_403.Tag := DW_TAG_member;
    VarD2_403.Children := 0;
    VarD2_403.Add(DW_AT_name, DW_FORM_string, 'D2'+#0);
    VarD2_403.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarD2_403.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_85); // $ED, $07, $00, $00

    VarD3_404 := TypeTGUID_397.GetNewChild;
    VarD3_404.Tag := DW_TAG_member;
    VarD3_404.Children := 0;
    VarD3_404.Add(DW_AT_name, DW_FORM_string, 'D3'+#0);
    VarD3_404.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(6)])); // $23, $06
    VarD3_404.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_85); // $ED, $07, $00, $00

    VarD4_405 := TypeTGUID_397.GetNewChild;
    VarD4_405.Tag := DW_TAG_member;
    VarD4_405.Children := 0;
    VarD4_405.Add(DW_AT_name, DW_FORM_string, 'D4'+#0);
    VarD4_405.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(8)])); // $23, $08
    VarD4_405.AddRef(DW_AT_type, DW_FORM_ref4, @Type_444); // $C2, $19, $00, $00

    VarTIME_LOW_406 := TypeTGUID_397.GetNewChild;
    VarTIME_LOW_406.Tag := DW_TAG_member;
    VarTIME_LOW_406.Children := 0;
    VarTIME_LOW_406.Add(DW_AT_name, DW_FORM_string, 'TIME_LOW'+#0);
    VarTIME_LOW_406.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarTIME_LOW_406.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_438); // $8F, $19, $00, $00

    VarTIME_MID_407 := TypeTGUID_397.GetNewChild;
    VarTIME_MID_407.Tag := DW_TAG_member;
    VarTIME_MID_407.Children := 0;
    VarTIME_MID_407.Add(DW_AT_name, DW_FORM_string, 'TIME_MID'+#0);
    VarTIME_MID_407.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarTIME_MID_407.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_85); // $ED, $07, $00, $00

    VarTIME_HI_AND_VERSION_408 := TypeTGUID_397.GetNewChild;
    VarTIME_HI_AND_VERSION_408.Tag := DW_TAG_member;
    VarTIME_HI_AND_VERSION_408.Children := 0;
    VarTIME_HI_AND_VERSION_408.Add(DW_AT_name, DW_FORM_string, 'TIME_HI_AND_VERSION'+#0);
    VarTIME_HI_AND_VERSION_408.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(6)])); // $23, $06
    VarTIME_HI_AND_VERSION_408.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_85); // $ED, $07, $00, $00

    VarCLOCK_SEQ_HI_AND_RESERVED_409 := TypeTGUID_397.GetNewChild;
    VarCLOCK_SEQ_HI_AND_RESERVED_409.Tag := DW_TAG_member;
    VarCLOCK_SEQ_HI_AND_RESERVED_409.Children := 0;
    VarCLOCK_SEQ_HI_AND_RESERVED_409.Add(DW_AT_name, DW_FORM_string, 'CLOCK_SEQ_HI_AND_RESERVED'+#0);
    VarCLOCK_SEQ_HI_AND_RESERVED_409.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(8)])); // $23, $08
    VarCLOCK_SEQ_HI_AND_RESERVED_409.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_79); // $B7, $07, $00, $00

    VarCLOCK_SEQ_LOW_410 := TypeTGUID_397.GetNewChild;
    VarCLOCK_SEQ_LOW_410.Tag := DW_TAG_member;
    VarCLOCK_SEQ_LOW_410.Children := 0;
    VarCLOCK_SEQ_LOW_410.Add(DW_AT_name, DW_FORM_string, 'CLOCK_SEQ_LOW'+#0);
    VarCLOCK_SEQ_LOW_410.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(9)])); // $23, $09
    VarCLOCK_SEQ_LOW_410.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_79); // $B7, $07, $00, $00

    VarNODE_411 := TypeTGUID_397.GetNewChild;
    VarNODE_411.Tag := DW_TAG_member;
    VarNODE_411.Children := 0;
    VarNODE_411.Add(DW_AT_name, DW_FORM_string, 'NODE'+#0);
    VarNODE_411.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(10)])); // $23, $0A
    VarNODE_411.AddRef(DW_AT_type, DW_FORM_ref4, @Type_447); // $D6, $19, $00, $00

  Type_412 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_412.Tag := DW_TAG_reference_type;
  Type_412.Children := 0;
  Type_412.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTGUID_396); // $A1, $17, $00, $00

  TypeDeclPINTERFACEENTRY_413 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPINTERFACEENTRY_413.Tag := DW_TAG_typedef;
  TypeDeclPINTERFACEENTRY_413.Children := 0;
  TypeDeclPINTERFACEENTRY_413.Add(DW_AT_name, DW_FORM_string, 'PINTERFACEENTRY'+#0);
  TypeDeclPINTERFACEENTRY_413.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_414); // $B6, $18, $00, $00

  TypePtr_414 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_414.Tag := DW_TAG_pointer_type;
  TypePtr_414.Children := 0;
  TypePtr_414.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACEENTRY_450); // $EA, $19, $00, $00

  Type_415 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_415.Tag := DW_TAG_reference_type;
  Type_415.Children := 0;
  Type_415.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTERFACEENTRY_413); // $A1, $18, $00, $00

  TypePtr_416 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_416.Tag := DW_TAG_pointer_type;
  TypePtr_416.Children := 0;
  TypePtr_416.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_417 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_417.Tag := DW_TAG_reference_type;
  Type_417.Children := 0;
  Type_417.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_416); // $C0, $18, $00, $00

  TypePtr_418 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_418.Tag := DW_TAG_pointer_type;
  TypePtr_418.Children := 0;
  TypePtr_418.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_419 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_419.Tag := DW_TAG_reference_type;
  Type_419.Children := 0;
  Type_419.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_418); // $CA, $18, $00, $00

  TypeDeclPINTERFACETABLE_420 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPINTERFACETABLE_420.Tag := DW_TAG_typedef;
  TypeDeclPINTERFACETABLE_420.Children := 0;
  TypeDeclPINTERFACETABLE_420.Add(DW_AT_name, DW_FORM_string, 'PINTERFACETABLE'+#0);
  TypeDeclPINTERFACETABLE_420.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_421); // $E9, $18, $00, $00

  TypePtr_421 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_421.Tag := DW_TAG_pointer_type;
  TypePtr_421.Children := 0;
  TypePtr_421.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACETABLE_459); // $73, $1A, $00, $00

  Type_422 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_422.Tag := DW_TAG_reference_type;
  Type_422.Children := 0;
  Type_422.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTERFACETABLE_420); // $D4, $18, $00, $00

  TypePtr_423 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_423.Tag := DW_TAG_pointer_type;
  TypePtr_423.Children := 0;
  TypePtr_423.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_424 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_424.Tag := DW_TAG_reference_type;
  Type_424.Children := 0;
  Type_424.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_423); // $F3, $18, $00, $00

  TypeDeclANSISTRING_425 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclANSISTRING_425.Tag := DW_TAG_typedef;
  TypeDeclANSISTRING_425.Children := 0;
  TypeDeclANSISTRING_425.Add(DW_AT_name, DW_FORM_string, 'ANSISTRING'+#0);
  TypeDeclANSISTRING_425.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_426); // $0D, $19, $00, $00

  TypePtr_426 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_426.Tag := DW_TAG_pointer_type;
  TypePtr_426.Children := 0;
  TypePtr_426.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclCHAR_430); // $21, $19, $00, $00

  Type_427 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_427.Tag := DW_TAG_reference_type;
  Type_427.Children := 0;
  Type_427.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclANSISTRING_425); // $FD, $18, $00, $00

  TypePtr_428 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_428.Tag := DW_TAG_pointer_type;
  TypePtr_428.Children := 0;
  TypePtr_428.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDecl__vtbl_ptr_type_344); // $09, $16, $00, $00

  Type_429 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_429.Tag := DW_TAG_reference_type;
  Type_429.Children := 0;
  Type_429.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_428); // $17, $19, $00, $00

  TypeDeclCHAR_430 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclCHAR_430.Tag := DW_TAG_typedef;
  TypeDeclCHAR_430.Children := 0;
  TypeDeclCHAR_430.Add(DW_AT_name, DW_FORM_string, 'CHAR'+#0);
  TypeDeclCHAR_430.AddRef(DW_AT_type, DW_FORM_ref4, @TypeChar_431); // $2B, $19, $00, $00

  TypeChar_431 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeChar_431.Tag := DW_TAG_base_type;
  TypeChar_431.Children := 0;
  TypeChar_431.Add(DW_AT_name, DW_FORM_string, 'Char'+#0);
  TypeChar_431.Add(DW_AT_encoding, DW_FORM_data1, [$08]);
  TypeChar_431.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_432 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_432.Tag := DW_TAG_reference_type;
  Type_432.Children := 0;
  Type_432.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclCHAR_430); // $21, $19, $00, $00

  TypeDeclTSTRINGMESSAGETABLE_433 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTSTRINGMESSAGETABLE_433.Tag := DW_TAG_typedef;
  TypeDeclTSTRINGMESSAGETABLE_433.Children := 0;
  TypeDeclTSTRINGMESSAGETABLE_433.Add(DW_AT_name, DW_FORM_string, 'TSTRINGMESSAGETABLE'+#0);
  TypeDeclTSTRINGMESSAGETABLE_433.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSTRINGMESSAGETABLE_434); // $51, $19, $00, $00

  TypeTSTRINGMESSAGETABLE_434 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTSTRINGMESSAGETABLE_434.Tag := DW_TAG_structure_type;
  TypeTSTRINGMESSAGETABLE_434.Children := 1;
  TypeTSTRINGMESSAGETABLE_434.Add(DW_AT_name, DW_FORM_string, 'TSTRINGMESSAGETABLE'+#0);
  TypeTSTRINGMESSAGETABLE_434.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 12);

    VarCOUNT_435 := TypeTSTRINGMESSAGETABLE_434.GetNewChild;
    VarCOUNT_435.Tag := DW_TAG_member;
    VarCOUNT_435.Children := 0;
    VarCOUNT_435.Add(DW_AT_name, DW_FORM_string, 'COUNT'+#0);
    VarCOUNT_435.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarCOUNT_435.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_88); // $04, $08, $00, $00

    VarMSGSTRTABLE_436 := TypeTSTRINGMESSAGETABLE_434.GetNewChild;
    VarMSGSTRTABLE_436.Tag := DW_TAG_member;
    VarMSGSTRTABLE_436.Children := 0;
    VarMSGSTRTABLE_436.Add(DW_AT_name, DW_FORM_string, 'MSGSTRTABLE'+#0);
    VarMSGSTRTABLE_436.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarMSGSTRTABLE_436.AddRef(DW_AT_type, DW_FORM_ref4, @Type_464); // $C3, $1A, $00, $00

  Type_437 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_437.Tag := DW_TAG_reference_type;
  Type_437.Children := 0;
  Type_437.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSTRINGMESSAGETABLE_433); // $38, $19, $00, $00

  TypeDeclLONGWORD_438 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclLONGWORD_438.Tag := DW_TAG_typedef;
  TypeDeclLONGWORD_438.Children := 0;
  TypeDeclLONGWORD_438.Add(DW_AT_name, DW_FORM_string, 'LONGWORD'+#0);
  TypeDeclLONGWORD_438.AddRef(DW_AT_type, DW_FORM_ref4, @TypeLONGWORD_439); // $9D, $19, $00, $00

  TypeLONGWORD_439 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeLONGWORD_439.Tag := DW_TAG_base_type;
  TypeLONGWORD_439.Children := 0;
  TypeLONGWORD_439.Add(DW_AT_name, DW_FORM_string, 'LONGWORD'+#0);
  TypeLONGWORD_439.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeLONGWORD_439.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

  Type_440 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_440.Tag := DW_TAG_reference_type;
  Type_440.Children := 0;
  Type_440.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_438); // $8F, $19, $00, $00

  Type_441 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_441.Tag := DW_TAG_array_type;
  Type_441.Children := 1;
  Type_441.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 8);
  Type_441.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_79); // $B7, $07, $00, $00

    Type_442 := Type_441.GetNewChild;
    Type_442.Tag := DW_TAG_subrange_type;
    Type_442.Children := 0;
    Type_442.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
    Type_442.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 7);
    Type_442.AddSLEB(DW_AT_byte_stride, DW_FORM_udata, 1);
    Type_442.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_82); // $CE, $07, $00, $00

  Type_443 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_443.Tag := DW_TAG_reference_type;
  Type_443.Children := 0;
  Type_443.AddRef(DW_AT_type, DW_FORM_ref4, @Type_441); // $AE, $19, $00, $00

  Type_444 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_444.Tag := DW_TAG_array_type;
  Type_444.Children := 1;
  Type_444.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 8);
  Type_444.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_79); // $B7, $07, $00, $00

    Type_445 := Type_444.GetNewChild;
    Type_445.Tag := DW_TAG_subrange_type;
    Type_445.Children := 0;
    Type_445.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
    Type_445.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 7);
    Type_445.AddSLEB(DW_AT_byte_stride, DW_FORM_udata, 1);
    Type_445.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_82); // $CE, $07, $00, $00

  Type_446 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_446.Tag := DW_TAG_reference_type;
  Type_446.Children := 0;
  Type_446.AddRef(DW_AT_type, DW_FORM_ref4, @Type_444); // $C2, $19, $00, $00

  Type_447 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_447.Tag := DW_TAG_array_type;
  Type_447.Children := 1;
  Type_447.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 6);
  Type_447.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_79); // $B7, $07, $00, $00

    Type_448 := Type_447.GetNewChild;
    Type_448.Tag := DW_TAG_subrange_type;
    Type_448.Children := 0;
    Type_448.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
    Type_448.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 5);
    Type_448.AddSLEB(DW_AT_byte_stride, DW_FORM_udata, 1);
    Type_448.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_82); // $CE, $07, $00, $00

  Type_449 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_449.Tag := DW_TAG_reference_type;
  Type_449.Children := 0;
  Type_449.AddRef(DW_AT_type, DW_FORM_ref4, @Type_447); // $D6, $19, $00, $00

  TypeDeclTINTERFACEENTRY_450 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTINTERFACEENTRY_450.Tag := DW_TAG_typedef;
  TypeDeclTINTERFACEENTRY_450.Children := 0;
  TypeDeclTINTERFACEENTRY_450.Add(DW_AT_name, DW_FORM_string, 'TINTERFACEENTRY'+#0);
  TypeDeclTINTERFACEENTRY_450.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTINTERFACEENTRY_451); // $FF, $19, $00, $00

  TypeTINTERFACEENTRY_451 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTINTERFACEENTRY_451.Tag := DW_TAG_structure_type;
  TypeTINTERFACEENTRY_451.Children := 1;
  TypeTINTERFACEENTRY_451.Add(DW_AT_name, DW_FORM_string, 'TINTERFACEENTRY'+#0);
  TypeTINTERFACEENTRY_451.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 20);

    VarIID_452 := TypeTINTERFACEENTRY_451.GetNewChild;
    VarIID_452.Tag := DW_TAG_member;
    VarIID_452.Children := 0;
    VarIID_452.Add(DW_AT_name, DW_FORM_string, 'IID'+#0);
    VarIID_452.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarIID_452.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPGUID_467); // $D7, $1A, $00, $00

    VarVTABLE_453 := TypeTINTERFACEENTRY_451.GetNewChild;
    VarVTABLE_453.Tag := DW_TAG_member;
    VarVTABLE_453.Children := 0;
    VarVTABLE_453.Add(DW_AT_name, DW_FORM_string, 'VTABLE'+#0);
    VarVTABLE_453.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarVTABLE_453.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_97); // $57, $08, $00, $00

    VarIOFFSET_454 := TypeTINTERFACEENTRY_451.GetNewChild;
    VarIOFFSET_454.Tag := DW_TAG_member;
    VarIOFFSET_454.Children := 0;
    VarIOFFSET_454.Add(DW_AT_name, DW_FORM_string, 'IOFFSET'+#0);
    VarIOFFSET_454.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(8)])); // $23, $08
    VarIOFFSET_454.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_438); // $8F, $19, $00, $00

    VarIIDSTR_455 := TypeTINTERFACEENTRY_451.GetNewChild;
    VarIIDSTR_455.Tag := DW_TAG_member;
    VarIIDSTR_455.Children := 0;
    VarIIDSTR_455.Add(DW_AT_name, DW_FORM_string, 'IIDSTR'+#0);
    VarIIDSTR_455.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(12)])); // $23, $0C
    VarIIDSTR_455.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSHORTSTRING_470); // $EC, $1A, $00, $00

    VarITYPE_456 := TypeTINTERFACEENTRY_451.GetNewChild;
    VarITYPE_456.Tag := DW_TAG_member;
    VarITYPE_456.Children := 0;
    VarITYPE_456.Add(DW_AT_name, DW_FORM_string, 'ITYPE'+#0);
    VarITYPE_456.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(16)])); // $23, $10
    VarITYPE_456.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACEENTRYTYPE_473); // $08, $1B, $00, $00

    Var__PAD_DUMMY_457 := TypeTINTERFACEENTRY_451.GetNewChild;
    Var__PAD_DUMMY_457.Tag := DW_TAG_member;
    Var__PAD_DUMMY_457.Children := 0;
    Var__PAD_DUMMY_457.Add(DW_AT_name, DW_FORM_string, '__PAD_DUMMY'+#0);
    Var__PAD_DUMMY_457.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(16)])); // $23, $10
    Var__PAD_DUMMY_457.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_97); // $57, $08, $00, $00

  Type_458 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_458.Tag := DW_TAG_reference_type;
  Type_458.Children := 0;
  Type_458.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACEENTRY_450); // $EA, $19, $00, $00

  TypeDeclTINTERFACETABLE_459 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTINTERFACETABLE_459.Tag := DW_TAG_typedef;
  TypeDeclTINTERFACETABLE_459.Children := 0;
  TypeDeclTINTERFACETABLE_459.Add(DW_AT_name, DW_FORM_string, 'TINTERFACETABLE'+#0);
  TypeDeclTINTERFACETABLE_459.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTINTERFACETABLE_460); // $88, $1A, $00, $00

  TypeTINTERFACETABLE_460 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTINTERFACETABLE_460.Tag := DW_TAG_structure_type;
  TypeTINTERFACETABLE_460.Children := 1;
  TypeTINTERFACETABLE_460.Add(DW_AT_name, DW_FORM_string, 'TINTERFACETABLE'+#0);
  TypeTINTERFACETABLE_460.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 24);

    VarENTRYCOUNT_461 := TypeTINTERFACETABLE_460.GetNewChild;
    VarENTRYCOUNT_461.Tag := DW_TAG_member;
    VarENTRYCOUNT_461.Children := 0;
    VarENTRYCOUNT_461.Add(DW_AT_name, DW_FORM_string, 'ENTRYCOUNT'+#0);
    VarENTRYCOUNT_461.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarENTRYCOUNT_461.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_438); // $8F, $19, $00, $00

    VarENTRIES_462 := TypeTINTERFACETABLE_460.GetNewChild;
    VarENTRIES_462.Tag := DW_TAG_member;
    VarENTRIES_462.Children := 0;
    VarENTRIES_462.Add(DW_AT_name, DW_FORM_string, 'ENTRIES'+#0);
    VarENTRIES_462.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarENTRIES_462.AddRef(DW_AT_type, DW_FORM_ref4, @Type_483); // $DE, $1B, $00, $00

  Type_463 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_463.Tag := DW_TAG_reference_type;
  Type_463.Children := 0;
  Type_463.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACETABLE_459); // $73, $1A, $00, $00

  Type_464 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_464.Tag := DW_TAG_array_type;
  Type_464.Children := 1;
  Type_464.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 8);
  Type_464.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTMSGSTRTABLE_486); // $F2, $1B, $00, $00

    Type_465 := Type_464.GetNewChild;
    Type_465.Tag := DW_TAG_subrange_type;
    Type_465.Children := 0;
    Type_465.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
    Type_465.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 0);
    Type_465.AddSLEB(DW_AT_byte_stride, DW_FORM_udata, 8);
    Type_465.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_82); // $CE, $07, $00, $00

  Type_466 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_466.Tag := DW_TAG_reference_type;
  Type_466.Children := 0;
  Type_466.AddRef(DW_AT_type, DW_FORM_ref4, @Type_464); // $C3, $1A, $00, $00

  TypeDeclPGUID_467 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPGUID_467.Tag := DW_TAG_typedef;
  TypeDeclPGUID_467.Children := 0;
  TypeDeclPGUID_467.Add(DW_AT_name, DW_FORM_string, 'PGUID'+#0);
  TypeDeclPGUID_467.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_468); // $E2, $1A, $00, $00

  TypePtr_468 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_468.Tag := DW_TAG_pointer_type;
  TypePtr_468.Children := 0;
  TypePtr_468.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTGUID_396); // $A1, $17, $00, $00

  Type_469 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_469.Tag := DW_TAG_reference_type;
  Type_469.Children := 0;
  Type_469.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPGUID_467); // $D7, $1A, $00, $00

  TypeDeclPSHORTSTRING_470 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclPSHORTSTRING_470.Tag := DW_TAG_typedef;
  TypeDeclPSHORTSTRING_470.Children := 0;
  TypeDeclPSHORTSTRING_470.Add(DW_AT_name, DW_FORM_string, 'PSHORTSTRING'+#0);
  TypeDeclPSHORTSTRING_470.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_471); // $FE, $1A, $00, $00

  TypePtr_471 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypePtr_471.Tag := DW_TAG_pointer_type;
  TypePtr_471.Children := 0;
  TypePtr_471.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTSTRING_370); // $DD, $16, $00, $00

  Type_472 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_472.Tag := DW_TAG_reference_type;
  Type_472.Children := 0;
  Type_472.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSHORTSTRING_470); // $EC, $1A, $00, $00

  TypeDeclTINTERFACEENTRYTYPE_473 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTINTERFACEENTRYTYPE_473.Tag := DW_TAG_typedef;
  TypeDeclTINTERFACEENTRYTYPE_473.Children := 0;
  TypeDeclTINTERFACEENTRYTYPE_473.Add(DW_AT_name, DW_FORM_string, 'TINTERFACEENTRYTYPE'+#0);
  TypeDeclTINTERFACEENTRYTYPE_473.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTINTERFACEENTRYTYPE_474); // $21, $1B, $00, $00

  TypeTINTERFACEENTRYTYPE_474 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTINTERFACEENTRYTYPE_474.Tag := DW_TAG_enumeration_type;
  TypeTINTERFACEENTRYTYPE_474.Children := 1;
  TypeTINTERFACEENTRYTYPE_474.Add(DW_AT_name, DW_FORM_string, 'TINTERFACEENTRYTYPE'+#0);
  TypeTINTERFACEENTRYTYPE_474.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeETSTANDARD_475 := TypeTINTERFACEENTRYTYPE_474.GetNewChild;
    TypeETSTANDARD_475.Tag := DW_TAG_enumerator;
    TypeETSTANDARD_475.Children := 0;
    TypeETSTANDARD_475.Add(DW_AT_name, DW_FORM_string, 'ETSTANDARD'+#0);
    TypeETSTANDARD_475.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeETVIRTUALMETHODRESULT_476 := TypeTINTERFACEENTRYTYPE_474.GetNewChild;
    TypeETVIRTUALMETHODRESULT_476.Tag := DW_TAG_enumerator;
    TypeETVIRTUALMETHODRESULT_476.Children := 0;
    TypeETVIRTUALMETHODRESULT_476.Add(DW_AT_name, DW_FORM_string, 'ETVIRTUALMETHODRESULT'+#0);
    TypeETVIRTUALMETHODRESULT_476.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeETSTATICMETHODRESULT_477 := TypeTINTERFACEENTRYTYPE_474.GetNewChild;
    TypeETSTATICMETHODRESULT_477.Tag := DW_TAG_enumerator;
    TypeETSTATICMETHODRESULT_477.Children := 0;
    TypeETSTATICMETHODRESULT_477.Add(DW_AT_name, DW_FORM_string, 'ETSTATICMETHODRESULT'+#0);
    TypeETSTATICMETHODRESULT_477.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

    TypeETFIELDVALUE_478 := TypeTINTERFACEENTRYTYPE_474.GetNewChild;
    TypeETFIELDVALUE_478.Tag := DW_TAG_enumerator;
    TypeETFIELDVALUE_478.Children := 0;
    TypeETFIELDVALUE_478.Add(DW_AT_name, DW_FORM_string, 'ETFIELDVALUE'+#0);
    TypeETFIELDVALUE_478.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

    TypeETVIRTUALMETHODCLASS_479 := TypeTINTERFACEENTRYTYPE_474.GetNewChild;
    TypeETVIRTUALMETHODCLASS_479.Tag := DW_TAG_enumerator;
    TypeETVIRTUALMETHODCLASS_479.Children := 0;
    TypeETVIRTUALMETHODCLASS_479.Add(DW_AT_name, DW_FORM_string, 'ETVIRTUALMETHODCLASS'+#0);
    TypeETVIRTUALMETHODCLASS_479.Add(DW_AT_const_value, DW_FORM_data4, [$04, $00, $00, $00]);

    TypeETSTATICMETHODCLASS_480 := TypeTINTERFACEENTRYTYPE_474.GetNewChild;
    TypeETSTATICMETHODCLASS_480.Tag := DW_TAG_enumerator;
    TypeETSTATICMETHODCLASS_480.Children := 0;
    TypeETSTATICMETHODCLASS_480.Add(DW_AT_name, DW_FORM_string, 'ETSTATICMETHODCLASS'+#0);
    TypeETSTATICMETHODCLASS_480.Add(DW_AT_const_value, DW_FORM_data4, [$05, $00, $00, $00]);

    TypeETFIELDVALUECLASS_481 := TypeTINTERFACEENTRYTYPE_474.GetNewChild;
    TypeETFIELDVALUECLASS_481.Tag := DW_TAG_enumerator;
    TypeETFIELDVALUECLASS_481.Children := 0;
    TypeETFIELDVALUECLASS_481.Add(DW_AT_name, DW_FORM_string, 'ETFIELDVALUECLASS'+#0);
    TypeETFIELDVALUECLASS_481.Add(DW_AT_const_value, DW_FORM_data4, [$06, $00, $00, $00]);

  Type_482 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_482.Tag := DW_TAG_reference_type;
  Type_482.Children := 0;
  Type_482.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACEENTRYTYPE_473); // $08, $1B, $00, $00

  Type_483 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_483.Tag := DW_TAG_array_type;
  Type_483.Children := 1;
  Type_483.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 20);
  Type_483.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTINTERFACEENTRY_450); // $EA, $19, $00, $00

    Type_484 := Type_483.GetNewChild;
    Type_484.Tag := DW_TAG_subrange_type;
    Type_484.Children := 0;
    Type_484.AddULEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
    Type_484.AddULEB(DW_AT_upper_bound, DW_FORM_sdata, 0);
    Type_484.AddSLEB(DW_AT_byte_stride, DW_FORM_udata, 20);
    Type_484.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_82); // $CE, $07, $00, $00

  Type_485 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_485.Tag := DW_TAG_reference_type;
  Type_485.Children := 0;
  Type_485.AddRef(DW_AT_type, DW_FORM_ref4, @Type_483); // $DE, $1B, $00, $00

  TypeDeclTMSGSTRTABLE_486 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeDeclTMSGSTRTABLE_486.Tag := DW_TAG_typedef;
  TypeDeclTMSGSTRTABLE_486.Children := 0;
  TypeDeclTMSGSTRTABLE_486.Add(DW_AT_name, DW_FORM_string, 'TMSGSTRTABLE'+#0);
  TypeDeclTMSGSTRTABLE_486.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTMSGSTRTABLE_487); // $04, $1C, $00, $00

  TypeTMSGSTRTABLE_487 := Unitdwarfsetup1_lpr_0.GetNewChild;
  TypeTMSGSTRTABLE_487.Tag := DW_TAG_structure_type;
  TypeTMSGSTRTABLE_487.Children := 1;
  TypeTMSGSTRTABLE_487.Add(DW_AT_name, DW_FORM_string, 'TMSGSTRTABLE'+#0);
  TypeTMSGSTRTABLE_487.AddSLEB(DW_AT_byte_size, DW_FORM_udata, 8);

    VarNAME_488 := TypeTMSGSTRTABLE_487.GetNewChild;
    VarNAME_488.Tag := DW_TAG_member;
    VarNAME_488.Children := 0;
    VarNAME_488.Add(DW_AT_name, DW_FORM_string, 'NAME'+#0);
    VarNAME_488.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarNAME_488.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSHORTSTRING_470); // $EC, $1A, $00, $00

    VarMETHOD_489 := TypeTMSGSTRTABLE_487.GetNewChild;
    VarMETHOD_489.Tag := DW_TAG_member;
    VarMETHOD_489.Children := 0;
    VarMETHOD_489.Add(DW_AT_name, DW_FORM_string, 'METHOD'+#0);
    VarMETHOD_489.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(4)])); // $23, $04
    VarMETHOD_489.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPOINTER_97); // $57, $08, $00, $00

  Type_490 := Unitdwarfsetup1_lpr_0.GetNewChild;
  Type_490.Tag := DW_TAG_reference_type;
  Type_490.Children := 0;
  Type_490.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTMSGSTRTABLE_486); // $F2, $1B, $00, $00

  //
  SectionDbgInfo.CreateSectionData;
  SectionDbgInfo.AbbrevSection.CreateSectionData;
end;

procedure TTestLoaderSetup1.PoissonTestFrame;
begin
  // Ensure any out of bound reads get bad data
  FillByte(TestStackFrame, SizeOf(TestStackFrame), $D5);
  FillByte(GlobTestSetup1, SizeOf(GlobTestSetup1), $D5);

end;

end.

