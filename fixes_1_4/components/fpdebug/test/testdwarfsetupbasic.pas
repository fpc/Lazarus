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
  PByte = ^Byte;
  PWord = ^Word;
  PLongWord = ^LongWord;
  PQWord = ^QWord;

  PShortInt = ^ShortInt;
  PSmallInt = ^SmallInt;
  PInteger = ^Integer;
  PInt64 = ^Int64;

  TSub1 = 1..9;
  TSub2 = 1000..90000;
  TSub3 = byte(10)..byte(250);
  TSub4 = -1..9;
  TSub5 = -11..-2;

  PSub1 = ^TSub1;
  PSub2 = ^TSub2;
  PSub3 = ^TSub3;
  PSub4 = ^TSub4;
  PSub5 = ^TSub5;

  PBoolean = ^Boolean;

  TEnum0 = (e0a);
  TEnum1 = (e1a, e1b, e1c);
  TEnum2 = (e2a, e2b, e2c, e2d, e2e, e2f, e2g, e2h, e2i);
  TEnum3 = (e3a, e3b, e3c, e3d, e3e, e3f, e3g, e3h,
            e3i, e3j, e3k, e3l, e3m, e3n, e3o, e3p,
            e3q);
  TEnumX = (eXa, eXc := 3, eXb := 10);
  TEnumR3 = e3c..e3l;

  PEnum0 = ^TEnum0;
  PEnum1 = ^TEnum1;
  PEnum2 = ^TEnum2;
  PEnum3 = ^TEnum3;
  PEnumX = ^TEnumX;
  PEnumR3 = ^TEnumR3;

  TSet0 = Set of TEnum0;
  TSet1 = Set of TEnum1;
  TSet2 = Set of TEnum2;
  TSet3 = Set of TEnum3;
  TSetX1 = Set of (s1a, s1b, s1c);
  TSetB1 = Set of Byte;
  TSetB2 = Set of 5..80;
  TSetC1 = Set of char;
  TSetR3 = Set of TEnumR3;

  TSetP0 = packed Set of TEnum0;
  TSetP1 = packed Set of TEnum1;
  TSetP2 = packed Set of TEnum2;
  TSetP3 = packed Set of TEnum3;
  TSetPX1 = packed Set of (sp1a, sp1b, sp1c);
  TSetPB1 = packed Set of Byte;
  TSetPB2 = packed Set of 5..80;
  TSetPC1 = packed Set of char;
  TSetPR3 = packed Set of TEnumR3;

  PSet0 = ^TSet0;
  PSet1 = ^TSet1;
  PSet2 = ^TSet2;
  PSet3 = ^TSet3;
  PSetX1 = ^TSetX1;
  PSetB1 = ^TSetB1;
  PSetB2 = ^TSetB2;
  PSetC1 = ^TSetC1;
  PSetR3 = ^TSetR3;

  PSetP0 = ^TSetP0;
  PSetP1 = ^TSetP1;
  PSetP2 = ^TSetP2;
  PSetP3 = ^TSetP3;
  PSetPX1 = ^TSetPX1;
  PSetPB1 = ^TSetPB1;
  PSetPB2 = ^TSetPB2;
  PSetPC1 = ^TSetPC1;
  PSetPR3 = ^TSetPR3;


  TRecByte = packed record VarByte:  Byte; end;
  TRecWord = packed record VarWord:  Word; end;
  TRecLong = packed record VarLong:  LongWord; end;
  TRecQWord = packed record VarQWord: QWord; end;

  TRecInt8 = packed record VarInt8:  ShortInt; end;
  TRecInt16 = packed record VarInt16: SmallInt; end;
  TRecInt32 = packed record VarInt32: Integer; end;
  TRecInt64 = packed record VarInt64: Int64; end;

  TRecSub1 = packed record VarSub1: TSub1; end;
  TRecSub2 = packed record VarSub2: TSub2; end;
  TRecSub3 = packed record VarSub3: TSub3; end;
  TRecSub4 = packed record VarSub4: TSub4; end;
  TRecSub5 = packed record VarSub5: TSub5; end;

  TRecPByte = packed record VarPByte:  PByte; end;
  TRecPWord = packed record VarPWord:  PWord; end;
  TRecPLong = packed record VarPLong:  PLongWord; end;
  TRecPQWord = packed record VarPQWord: PQWord; end;

  TRecPInt8 = packed record VarPInt8:  PShortInt; end;
  TRecPInt16 = packed record VarPInt16: PSmallInt; end;
  TRecPInt32 = packed record VarPInt32: PInteger; end;
  TRecPInt64 = packed record VarPInt64: PInt64; end;

  TRecPSub1 = packed record VarPSub1: PSub1; end;
  TRecPSub2 = packed record VarPSub2: PSub2; end;
  TRecPSub3 = packed record VarPSub3: PSub3; end;
  TRecPSub4 = packed record VarPSub4: PSub4; end;
  TRecPSub5 = packed record VarPSub5: PSub5; end;


  TRecBoolean = packed record VarBoolean: Boolean; end;
  TRecPBoolean = packed record VarPBoolean: PBoolean; end;


  TRecEnum0 = packed record VarEnum0: TEnum0; end;
  TRecEnum1 = packed record VarEnum1: TEnum1; end;
  TRecEnum2 = packed record VarEnum2: TEnum2; end;
  TRecEnum3 = packed record VarEnum3: TEnum3; end;
  TRecEnumX = packed record VarEnumX: TEnumX; end;
  TRecEnumR3 = packed record VarEnumR3: TEnumR3; end;

  TRecPEnum0 = packed record VarPEnum0: PEnum0; end;
  TRecPEnum1 = packed record VarPEnum1: PEnum1; end;
  TRecPEnum2 = packed record VarPEnum2: PEnum2; end;
  TRecPEnum3 = packed record VarPEnum3: PEnum3; end;
  TRecPEnumX = packed record VarPEnumX: PEnumX; end;
  TRecPEnumR3 = packed record VarPEnumR3: PEnumR3; end;


  TRecSet0 = packed record VarSet0: TSet0; end;
  TRecSet1 = packed record VarSet1: TSet1; end;
  TRecSet2 = packed record VarSet2: TSet2; end;
  TRecSet3 = packed record VarSet3: TSet3; end;
  TRecSetX1 = packed record VarSetX1: TSetX1; end;
  TRecSetX2 = packed record VarSetX2: set of (trsxa, trsxb, trsxc, trsxd); end;
  TRecSetB1 = packed record VarSetB1: TSetB1; end;
  TRecSetB2 = packed record VarSetB2: TSetB2; end;
  TRecSetC1 = packed record VarSetC1: TSetC1; end;
  TRecSetC2 = packed record VarSetC2: set of char; end;
  TRecSetR3 = packed record VarSetR3: TSetR3; end;

  TRecPSet0 = packed record VarPSet0: PSet0; end;
  TRecPSet1 = packed record VarPSet1: PSet1; end;
  TRecPSet2 = packed record VarPSet2: PSet2; end;
  TRecPSet3 = packed record VarPSet3: PSet3; end;
  TRecPSetX1 = packed record VarPSetX1: PSetX1; end;
  TRecPSetB1 = packed record VarPSetB1: PSetB1; end;
  TRecPSetB2 = packed record VarPSetB2: PSetB2; end;
  TRecPSetC1 = packed record VarPSetC1: PSetC1; end;
  TRecPSetR3 = packed record VarPSetR3: PSetR3; end;

  TRecPSetP0 = packed record VarPSetP0: PSetP0; end;
  TRecPSetP1 = packed record VarPSetP1: PSetP1; end;
  TRecPSetP2 = packed record VarPSetP2: PSetP2; end;
  TRecPSetP3 = packed record VarPSetP3: PSetP3; end;
  TRecPSetPX1 = packed record VarPSetPX1: PSetPX1; end;
  TRecPSetPB1 = packed record VarPSetPB1: PSetPB1; end;
  TRecPSetPB2 = packed record VarPSetPB2: PSetPB2; end;
  TRecPSetPC1 = packed record VarPSetPC1: PSetPC1; end;
  TRecPSetPR3 = packed record VarPSetPR3: PSetPR3; end;

  {%endregion}

type
  { TTestDwarfSetupBasic }

  TTestLoaderSetupBasic = class(TTestDummyImageLoader)
  public
    constructor Create; override;
    procedure  PoissonTestFrame;
  public
    SectionDbgInfo: TTestDummySectionInfoEntries;

     Unitdwarfsetupbasic_lpr_0, VarVARBYTE_1, VarVARWORD_2, VarVARLONG_3, VarVARQWORD_4, VarVARINT8_5, VarVARINT16_6, VarVARINT32_7, VarVARINT64_8, VarVARSUB1_9, VarVARSUB2_10, VarVARSUB3_11, VarVARSUB4_12, VarVARSUB5_13, VarVARPBYTE_14, VarVARPWORD_15, VarVARPLONG_16, VarVARPQWORD_17, VarVARPINT8_18, VarVARPINT16_19, VarVARPINT32_20, VarVARPINT64_21, VarVARPSUB1_22, VarVARPSUB2_23, VarVARPSUB3_24, VarVARPSUB4_25, VarVARPSUB5_26, VarVARBOOLEAN_27, VarVARPBOOLEAN_28, VarVARENUM0_29, VarVARENUM1_30, VarVARENUM2_31, VarVARENUM3_32, VarVARENUM4_33, VarVARENUMX_34, VarVARENUMR3_35, VarVARPENUM0_36, VarVARPENUM1_37, VarVARPENUM2_38, VarVARPENUM3_39, VarVARPENUMX_40, VarVARPENUMR3_41, VarVARSET0_42, VarVARSET1_43, VarVARSET2_44, VarVARSET3_45, VarVARSETX1_46, VarVARSETX2_47, VarVARSETB1_48, VarVARSETB2_49, VarVARSETC1_50, VarVARSETC2_51, VarVARSETR3_52, VarVARPSET0_53, VarVARPSET1_54, VarVARPSET2_55, VarVARPSET3_56, VarVARPSETX1_57, VarVARPSETB1_58, VarVARPSETB2_59, VarVARPSETC1_60, VarVARPSETR3_61, VarVARPSETP0_62, VarVARPSETP1_63, VarVARPSETP2_64, VarVARPSETP3_65, VarVARPSETPX1_66, VarVARPSETPB1_67, VarVARPSETPB2_68, VarVARPSETPC1_69, VarVARPSETPR3_70, VarVARRECBYTE_71, VarVARRECWORD_72, VarVARRECLONG_73, VarVARRECQWORD_74, VarVARRECINT8_75, VarVARRECINT16_76, VarVARRECINT32_77, VarVARRECINT64_78, VarVARRECSUB1_79, VarVARRECSUB2_80, VarVARRECSUB3_81, VarVARRECSUB4_82, VarVARRECSUB5_83, VarVARRECPBYTE_84, VarVARRECPWORD_85, VarVARRECPLONG_86, VarVARRECPQWORD_87, VarVARRECPINT8_88, VarVARRECPINT16_89, VarVARRECPINT32_90, VarVARRECPINT64_91, VarVARRECPSUB1_92, VarVARRECPSUB2_93, VarVARRECPSUB3_94, VarVARRECPSUB4_95, VarVARRECPSUB5_96, VarVARRECBOOLEAN_97, VarVARRECPBOOLEAN_98, VarVARRECENUM0_99, VarVARRECENUM1_100, VarVARRECENUM2_101, VarVARRECENUM3_102, VarVARRECENUMX_103, VarVARRECENUMR3_104, VarVARRECPENUM0_105, VarVARRECPENUM1_106, VarVARRECPENUM2_107, VarVARRECPENUM3_108, VarVARRECPENUMX_109, VarVARRECPENUMR3_110, VarVARRECSET0_111, VarVARRECSET1_112, VarVARRECSET2_113, VarVARRECSET3_114, VarVARRECSETX1_115, VarVARRECSETX2_116, VarVARRECSETB1_117, VarVARRECSETB2_118, VarVARRECSETC1_119, VarVARRECSETC2_120, VarVARRECSETR3_121, VarVARRECPSET0_122, VarVARRECPSET1_123, VarVARRECPSET2_124, VarVARRECPSET3_125, VarVARRECPSETX1_126, VarVARRECPSETB1_127, VarVARRECPSETB2_128, VarVARRECPSETC1_129, VarVARRECPSETR3_130, VarVARRECPSETP0_131, VarVARRECPSETP1_132, VarVARRECPSETP2_133, VarVARRECPSETP3_134, VarVARRECPSETPX1_135, VarVARRECPSETPB1_136, VarVARRECPSETPB2_137, VarVARRECPSETPC1_138, VarVARRECPSETPR3_139, Progmain_140, TypeDeclBYTE_141, TypeBYTE_142, Type_143, TypeDeclSHORTINT_144, TypeSHORTINT_145, Type_146, TypeDeclWORD_147, TypeWORD_148, Type_149, TypeDeclSMALLINT_150, TypeSMALLINT_151, Type_152, TypeDeclLONGWORD_153, TypeLONGWORD_154, Type_155, TypeDeclLONGINT_156, TypeLONGINT_157, Type_158, TypeDeclQWORD_159, TypeQWord_160, Type_161, TypeDeclINT64_162, TypeInt64_163, Type_164, TypeDeclBOOLEAN_165, TypeBoolean_166, Type_167, TypeDeclPBYTE_168, TypePtr_169, Type_170, TypeDeclPWORD_171, TypePtr_172, Type_173, TypeDeclPLONGWORD_174, TypePtr_175, Type_176, TypeDeclPQWORD_177, TypePtr_178, Type_179, TypeDeclPSHORTINT_180, TypePtr_181, Type_182, TypeDeclPSMALLINT_183, TypePtr_184, Type_185, TypeDeclPINTEGER_186, TypePtr_187, Type_188, TypeDeclPINT64_189, TypePtr_190, Type_191, TypeDeclTSUB1_192, TypeTSUB1_193, Type_194, TypeDeclTSUB2_195, TypeTSUB2_196, Type_197, TypeDeclTSUB3_198, TypeTSUB3_199, Type_200, TypeDeclTSUB4_201, TypeTSUB4_202, Type_203, TypeDeclTSUB5_204, TypeTSUB5_205, Type_206, TypeDeclPSUB1_207, TypePtr_208, Type_209, TypeDeclPSUB2_210, TypePtr_211, Type_212, TypeDeclPSUB3_213, TypePtr_214, Type_215, TypeDeclPSUB4_216, TypePtr_217, Type_218, TypeDeclPSUB5_219, TypePtr_220, Type_221, TypeDeclPBOOLEAN_222, TypePtr_223, Type_224, TypeDeclTENUM0_225, TypeTENUM0_226, TypeE0A_227, Type_228, TypeDeclTENUM1_229, TypeTENUM1_230, TypeE1A_231, TypeE1B_232, TypeE1C_233, Type_234, TypeDeclTENUM2_235, TypeTENUM2_236, TypeE2A_237, TypeE2B_238, TypeE2C_239, TypeE2D_240, TypeE2E_241, TypeE2F_242, TypeE2G_243, TypeE2H_244, TypeE2I_245, Type_246, TypeDeclTENUM3_247, TypeTENUM3_248, TypeE3A_249, TypeE3B_250, TypeE3C_251, TypeE3D_252, TypeE3E_253, TypeE3F_254, TypeE3G_255, TypeE3H_256, TypeE3I_257, TypeE3J_258, TypeE3K_259, TypeE3L_260, TypeE3M_261, TypeE3N_262, TypeE3O_263, TypeE3P_264, TypeE3Q_265, Type_266, TypeDeclTENUMX_267, TypeTENUMX_268, TypeEXA_269, TypeEXC_270, TypeEXB_271, Type_272, TypeDeclTENUMR3_273, TypeTENUMR3_274, TypeE3C_275, TypeE3D_276, TypeE3E_277, TypeE3F_278, TypeE3G_279, TypeE3H_280, TypeE3I_281, TypeE3J_282, TypeE3K_283, TypeE3L_284, Type_285, TypeDeclPENUM0_286, TypePtr_287, Type_288, TypeDeclPENUM1_289, TypePtr_290, Type_291, TypeDeclPENUM2_292, TypePtr_293, Type_294, TypeDeclPENUM3_295, TypePtr_296, Type_297, TypeDeclPENUMX_298, TypePtr_299, Type_300, TypeDeclPENUMR3_301, TypePtr_302, Type_303, TypeDeclTSET0_304, TypeTSET0_305, Type_306, Type_307, TypeDeclTSET1_308, TypeTSET1_309, Type_310, Type_311, TypeDeclTSET2_312, TypeTSET2_313, Type_314, Type_315, TypeDeclTSET3_316, TypeTSET3_317, Type_318, Type_319, TypeDeclTSETX1_320, TypeTSETX1_321, Type_322, Type_323, TypeDeclTSETB1_324, TypeTSETB1_325, Type_326, Type_327, TypeDeclTSETB2_328, TypeTSETB2_329, Type_330, Type_331, TypeDeclTSETC1_332, TypeTSETC1_333, Type_334, Type_335, TypeDeclTSETR3_336, TypeTSETR3_337, Type_338, Type_339, TypeDeclTSETP0_340, TypeTSETP0_341, Type_342, Type_343, TypeDeclTSETP1_344, TypeTSETP1_345, Type_346, Type_347, TypeDeclTSETP2_348, TypeTSETP2_349, Type_350, Type_351, TypeDeclTSETP3_352, TypeTSETP3_353, Type_354, Type_355, TypeDeclTSETPX1_356, TypeTSETPX1_357, Type_358, Type_359, TypeDeclTSETPB1_360, TypeTSETPB1_361, Type_362, Type_363, TypeDeclTSETPB2_364, TypeTSETPB2_365, Type_366, Type_367, TypeDeclTSETPC1_368, TypeTSETPC1_369, Type_370, Type_371, TypeDeclTSETPR3_372, TypeTSETPR3_373, Type_374, Type_375, TypeDeclPSET0_376, TypePtr_377, Type_378, TypeDeclPSET1_379, TypePtr_380, Type_381, TypeDeclPSET2_382, TypePtr_383, Type_384, TypeDeclPSET3_385, TypePtr_386, Type_387, TypeDeclPSETX1_388, TypePtr_389, Type_390, TypeDeclPSETB1_391, TypePtr_392, Type_393, TypeDeclPSETB2_394, TypePtr_395, Type_396, TypeDeclPSETC1_397, TypePtr_398, Type_399, TypeDeclPSETR3_400, TypePtr_401, Type_402, TypeDeclPSETP0_403, TypePtr_404, Type_405, TypeDeclPSETP1_406, TypePtr_407, Type_408, TypeDeclPSETP2_409, TypePtr_410, Type_411, TypeDeclPSETP3_412, TypePtr_413, Type_414, TypeDeclPSETPX1_415, TypePtr_416, Type_417, TypeDeclPSETPB1_418, TypePtr_419, Type_420, TypeDeclPSETPB2_421, TypePtr_422, Type_423, TypeDeclPSETPC1_424, TypePtr_425, Type_426, TypeDeclPSETPR3_427, TypePtr_428, Type_429, Type_430, TypeE4A_431, TypeE4B_432, TypeE4C_433, TypeE4D_434, Type_435, Type_436, Type_437, Type_438, Type_439, Type_440, Type_441, TypeDeclTRECBYTE_442, TypeTRECBYTE_443, VarVARBYTE_444, Type_445, TypeDeclTRECWORD_446, TypeTRECWORD_447, VarVARWORD_448, Type_449, TypeDeclTRECLONG_450, TypeTRECLONG_451, VarVARLONG_452, Type_453, TypeDeclTRECQWORD_454, TypeTRECQWORD_455, VarVARQWORD_456, Type_457, TypeDeclTRECINT8_458, TypeTRECINT8_459, VarVARINT8_460, Type_461, TypeDeclTRECINT16_462, TypeTRECINT16_463, VarVARINT16_464, Type_465, TypeDeclTRECINT32_466, TypeTRECINT32_467, VarVARINT32_468, Type_469, TypeDeclTRECINT64_470, TypeTRECINT64_471, VarVARINT64_472, Type_473, TypeDeclTRECSUB1_474, TypeTRECSUB1_475, VarVARSUB1_476, Type_477, TypeDeclTRECSUB2_478, TypeTRECSUB2_479, VarVARSUB2_480, Type_481, TypeDeclTRECSUB3_482, TypeTRECSUB3_483, VarVARSUB3_484, Type_485, TypeDeclTRECSUB4_486, TypeTRECSUB4_487, VarVARSUB4_488, Type_489, TypeDeclTRECSUB5_490, TypeTRECSUB5_491, VarVARSUB5_492, Type_493, TypeDeclTRECPBYTE_494, TypeTRECPBYTE_495, VarVARPBYTE_496, Type_497, TypeDeclTRECPWORD_498, TypeTRECPWORD_499, VarVARPWORD_500, Type_501, TypeDeclTRECPLONG_502, TypeTRECPLONG_503, VarVARPLONG_504, Type_505, TypeDeclTRECPQWORD_506, TypeTRECPQWORD_507, VarVARPQWORD_508, Type_509, TypeDeclTRECPINT8_510, TypeTRECPINT8_511, VarVARPINT8_512, Type_513, TypeDeclTRECPINT16_514, TypeTRECPINT16_515, VarVARPINT16_516, Type_517, TypeDeclTRECPINT32_518, TypeTRECPINT32_519, VarVARPINT32_520, Type_521, TypeDeclTRECPINT64_522, TypeTRECPINT64_523, VarVARPINT64_524, Type_525, TypeDeclTRECPSUB1_526, TypeTRECPSUB1_527, VarVARPSUB1_528, Type_529, TypeDeclTRECPSUB2_530, TypeTRECPSUB2_531, VarVARPSUB2_532, Type_533, TypeDeclTRECPSUB3_534, TypeTRECPSUB3_535, VarVARPSUB3_536, Type_537, TypeDeclTRECPSUB4_538, TypeTRECPSUB4_539, VarVARPSUB4_540, Type_541, TypeDeclTRECPSUB5_542, TypeTRECPSUB5_543, VarVARPSUB5_544, Type_545, TypeDeclTRECBOOLEAN_546, TypeTRECBOOLEAN_547, VarVARBOOLEAN_548, Type_549, TypeDeclTRECPBOOLEAN_550, TypeTRECPBOOLEAN_551, VarVARPBOOLEAN_552, Type_553, TypeDeclTRECENUM0_554, TypeTRECENUM0_555, VarVARENUM0_556, Type_557, TypeDeclTRECENUM1_558, TypeTRECENUM1_559, VarVARENUM1_560, Type_561, TypeDeclTRECENUM2_562, TypeTRECENUM2_563, VarVARENUM2_564, Type_565, TypeDeclTRECENUM3_566, TypeTRECENUM3_567, VarVARENUM3_568, Type_569, TypeDeclTRECENUMX_570, TypeTRECENUMX_571, VarVARENUMX_572, Type_573, TypeDeclTRECENUMR3_574, TypeTRECENUMR3_575, VarVARENUMR3_576, Type_577, TypeDeclTRECPENUM0_578, TypeTRECPENUM0_579, VarVARPENUM0_580, Type_581, TypeDeclTRECPENUM1_582, TypeTRECPENUM1_583, VarVARPENUM1_584, Type_585, TypeDeclTRECPENUM2_586, TypeTRECPENUM2_587, VarVARPENUM2_588, Type_589, TypeDeclTRECPENUM3_590, TypeTRECPENUM3_591, VarVARPENUM3_592, Type_593, TypeDeclTRECPENUMX_594, TypeTRECPENUMX_595, VarVARPENUMX_596, Type_597, TypeDeclTRECPENUMR3_598, TypeTRECPENUMR3_599, VarVARPENUMR3_600, Type_601, TypeDeclTRECSET0_602, TypeTRECSET0_603, VarVARSET0_604, Type_605, TypeDeclTRECSET1_606, TypeTRECSET1_607, VarVARSET1_608, Type_609, TypeDeclTRECSET2_610, TypeTRECSET2_611, VarVARSET2_612, Type_613, TypeDeclTRECSET3_614, TypeTRECSET3_615, VarVARSET3_616, Type_617, TypeDeclTRECSETX1_618, TypeTRECSETX1_619, VarVARSETX1_620, Type_621, TypeDeclTRECSETX2_622, TypeTRECSETX2_623, VarVARSETX2_624, Type_625, TypeDeclTRECSETB1_626, TypeTRECSETB1_627, VarVARSETB1_628, Type_629, TypeDeclTRECSETB2_630, TypeTRECSETB2_631, VarVARSETB2_632, Type_633, TypeDeclTRECSETC1_634, TypeTRECSETC1_635, VarVARSETC1_636, Type_637, TypeDeclTRECSETC2_638, TypeTRECSETC2_639, VarVARSETC2_640, Type_641, TypeDeclTRECSETR3_642, TypeTRECSETR3_643, VarVARSETR3_644, Type_645, TypeDeclTRECPSET0_646, TypeTRECPSET0_647, VarVARPSET0_648, Type_649, TypeDeclTRECPSET1_650, TypeTRECPSET1_651, VarVARPSET1_652, Type_653, TypeDeclTRECPSET2_654, TypeTRECPSET2_655, VarVARPSET2_656, Type_657, TypeDeclTRECPSET3_658, TypeTRECPSET3_659, VarVARPSET3_660, Type_661, TypeDeclTRECPSETX1_662, TypeTRECPSETX1_663, VarVARPSETX1_664, Type_665, TypeDeclTRECPSETB1_666, TypeTRECPSETB1_667, VarVARPSETB1_668, Type_669, TypeDeclTRECPSETB2_670, TypeTRECPSETB2_671, VarVARPSETB2_672, Type_673, TypeDeclTRECPSETC1_674, TypeTRECPSETC1_675, VarVARPSETC1_676, Type_677, TypeDeclTRECPSETR3_678, TypeTRECPSETR3_679, VarVARPSETR3_680, Type_681, TypeDeclTRECPSETP0_682, TypeTRECPSETP0_683, VarVARPSETP0_684, Type_685, TypeDeclTRECPSETP1_686, TypeTRECPSETP1_687, VarVARPSETP1_688, Type_689, TypeDeclTRECPSETP2_690, TypeTRECPSETP2_691, VarVARPSETP2_692, Type_693, TypeDeclTRECPSETP3_694, TypeTRECPSETP3_695, VarVARPSETP3_696, Type_697, TypeDeclTRECPSETPX1_698, TypeTRECPSETPX1_699, VarVARPSETPX1_700, Type_701, TypeDeclTRECPSETPB1_702, TypeTRECPSETPB1_703, VarVARPSETPB1_704, Type_705, TypeDeclTRECPSETPB2_706, TypeTRECPSETPB2_707, VarVARPSETPB2_708, Type_709, TypeDeclTRECPSETPC1_710, TypeTRECPSETPC1_711, VarVARPSETPC1_712, Type_713, TypeDeclTRECPSETPR3_714, TypeTRECPSETPR3_715, VarVARPSETPR3_716, Type_717, Type_718, VarVARBYTE_719, Type_720, Type_721, VarVARWORD_722, Type_723, Type_724, VarVARLONG_725, Type_726, Type_727, VarVARQWORD_728, Type_729, Type_730, VarVARINT8_731, Type_732, Type_733, VarVARINT16_734, Type_735, Type_736, VarVARINT32_737, Type_738, Type_739, VarVARINT64_740, Type_741, Type_742, VarVARSUB1_743, Type_744, Type_745, VarVARSUB2_746, Type_747, Type_748, VarVARSUB3_749, Type_750, Type_751, VarVARSUB4_752, Type_753, Type_754, VarVARSUB5_755, Type_756, Type_757, VarVARPBYTE_758, Type_759, Type_760, VarVARPWORD_761, Type_762, Type_763, VarVARPLONG_764, Type_765, Type_766, VarVARPQWORD_767, Type_768, Type_769, VarVARPINT8_770, Type_771, Type_772, VarVARPINT16_773, Type_774, Type_775, VarVARPINT32_776, Type_777, Type_778, VarVARPINT64_779, Type_780, Type_781, VarVARPSUB1_782, Type_783, Type_784, VarVARPSUB2_785, Type_786, Type_787, VarVARPSUB3_788, Type_789, Type_790, VarVARPSUB4_791, Type_792, Type_793, VarVARPSUB5_794, Type_795, Type_796, VarVARBOOLEAN_797, Type_798, Type_799, VarVARPBOOLEAN_800, Type_801, Type_802, VarVARENUM0_803, Type_804, Type_805, VarVARENUM1_806, Type_807, Type_808, VarVARENUM2_809, Type_810, Type_811, VarVARENUM3_812, Type_813, Type_814, VarVARENUMX_815, Type_816, Type_817, VarVARENUMR3_818, Type_819, Type_820, VarVARPENUM0_821, Type_822, Type_823, VarVARPENUM1_824, Type_825, Type_826, VarVARPENUM2_827, Type_828, Type_829, VarVARPENUM3_830, Type_831, Type_832, VarVARPENUMX_833, Type_834, Type_835, VarVARPENUMR3_836, Type_837, Type_838, VarVARSET0_839, Type_840, Type_841, VarVARSET1_842, Type_843, Type_844, VarVARSET2_845, Type_846, Type_847, VarVARSET3_848, Type_849, Type_850, VarVARSETX1_851, Type_852, Type_853, VarVARSETX2_854, Type_855, Type_856, VarVARSETB1_857, Type_858, Type_859, VarVARSETB2_860, Type_861, Type_862, VarVARSETC1_863, Type_864, Type_865, VarVARSETC2_866, Type_867, Type_868, VarVARSETR3_869, Type_870, Type_871, VarVARPSET0_872, Type_873, Type_874, VarVARPSET1_875, Type_876, Type_877, VarVARPSET2_878, Type_879, Type_880, VarVARPSET3_881, Type_882, Type_883, VarVARPSETX1_884, Type_885, Type_886, VarVARPSETB1_887, Type_888, Type_889, VarVARPSETB2_890, Type_891, Type_892, VarVARPSETC1_893, Type_894, Type_895, VarVARPSETR3_896, Type_897, Type_898, VarVARPSETP0_899, Type_900, Type_901, VarVARPSETP1_902, Type_903, Type_904, VarVARPSETP2_905, Type_906, Type_907, VarVARPSETP3_908, Type_909, Type_910, VarVARPSETPX1_911, Type_912, Type_913, VarVARPSETPB1_914, Type_915, Type_916, VarVARPSETPB2_917, Type_918, Type_919, VarVARPSETPC1_920, Type_921, Type_922, VarVARPSETPR3_923, Type_924, Type_925, TypeS1A_926, TypeS1B_927, TypeS1C_928, Type_929, Type_930, Type_931, Type_932, TypeSP1A_933, TypeSP1B_934, TypeSP1C_935, Type_936, Type_937, Type_938, Type_939, TypeSXA_940, TypeSXB_941, TypeSXC_942, TypeSXD_943, Type_944, TypeDeclCHAR_945, TypeChar_946, Type_947, Type_948, Type_949, Type_950, Type_951, Type_952, Type_953, Type_954, Type_955, Type_956, Type_957, Type_958, Type_959, Type_960, TypeTRSXA_961, TypeTRSXB_962, TypeTRSXC_963, TypeTRSXD_964, Type_965, Type_966, TypeRSXA_967, TypeRSXB_968, TypeRSXC_969, TypeRSXD_970, Type_971
    : TTestDwarfInfoEntry;

    // global vars
    GlobalVar: record
      PAD_Before: QWord; // padding will be filled with bad data

      VarByte, PAD_VarByte:  Byte;
      VarWord, PAD_VarWord:  Word;
      VarLong, PAD_VarLong:  LongWord;
      VarQWord, PAD_VarQWord: QWord;

      VarInt8, PAD_VarInt8:  ShortInt;
      VarInt16, PAD_VarInt16: SmallInt;
      VarInt32, PAD_VarInt32: Integer;
      VarInt64, PAD_VarInt64: Int64;

      VarSub1, PAD_VarSub1: TSub1;
      VarSub2, PAD_VarSub2: TSub2;
      VarSub3, PAD_VarSub3: TSub3;
      VarSub4, PAD_VarSub4: TSub4;
      VarSub5, PAD_VarSub5: TSub5;

      VarPByte, PAD_VarPByte:  PByte;
      VarPWord, PAD_VarPWord:  PWord;
      VarPLong, PAD_VarPLong:  PLongWord;
      VarPQWord, PAD_VarPQWord: PQWord;

      VarPInt8, PAD_VarPInt8:  PShortInt;
      VarPInt16, PAD_VarPInt16: PSmallInt;
      VarPInt32, PAD_VarPInt32: PInteger;
      VarPInt64, PAD_VarPInt64: PInt64;

      VarPSub1, PAD_VarPSub1: PSub1;
      VarPSub2, PAD_VarPSub2: PSub2;
      VarPSub3, PAD_VarPSub3: PSub3;
      VarPSub4, PAD_VarPSub4: PSub4;
      VarPSub5, PAD_VarPSub5: PSub5;



      VarBoolean, PAD_VarBoolean: Boolean;
      VarPBoolean, PAD_VarPBoolean: PBoolean;


      VarEnum0, PAD_VarEnum0: TEnum0;
      VarEnum1, PAD_VarEnum1: TEnum1;
      VarEnum2, PAD_VarEnum2: TEnum2;
      VarEnum3, PAD_VarEnum3: TEnum3;
      VarEnum4, PAD_VarEnum4: (e4a,e4b,e4c,e4d);
      VarEnumX, PAD_VarEnumX: TEnumX;
      VarEnumR3, PAD_VarEnumR3: TEnumR3;

      VarPEnum0, PAD_VarPEnum0: PEnum0;
      VarPEnum1, PAD_VarPEnum1: PEnum1;
      VarPEnum2, PAD_VarPEnum2: PEnum2;
      VarPEnum3, PAD_VarPEnum3: PEnum3;
      VarPEnumX, PAD_VarPEnumX: PEnumX;
      VarPEnumR3, PAD_VarPEnumR3: PEnumR3;


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

      VarPSet0, PAD_VarPSet0: PSet0;
      VarPSet1, PAD_VarPSet1: PSet1;
      VarPSet2, PAD_VarPSet2: PSet2;
      VarPSet3, PAD_VarPSet3: PSet3;
      VarPSetX1, PAD_VarPSetX1: PSetX1;
      VarPSetB1, PAD_VarPSetB1: PSetB1;
      VarPSetB2, PAD_VarPSetB2: PSetB2;
      VarPSetC1, PAD_VarPSetC1: PSetC1;
      VarPSetR3, PAD_VarPSetR3: PSetR3;

      VarPSetP0, PAD_VarPSetP0: PSetP0;
      VarPSetP1, PAD_VarPSetP1: PSetP1;
      VarPSetP2, PAD_VarPSetP2: PSetP2;
      VarPSetP3, PAD_VarPSetP3: PSetP3;
      VarPSetPX1, PAD_VarPSetPX1: PSetPX1;
      VarPSetPB1, PAD_VarPSetPB1: PSetPB1;
      VarPSetPB2, PAD_VarPSetPB2: PSetPB2;
      VarPSetPC1, PAD_VarPSetPC1: PSetPC1;
      VarPSetPR3, PAD_VarPSetPR3: PSetPR3;


      VarRecByte, PAD_VarRecByte: packed record VarByte:  Byte; end;
      VarRecWord, PAD_VarRecWord: packed record VarWord:  Word; end;
      VarRecLong, PAD_VarRecLong: packed record VarLong:  LongWord; end;
      VarRecQWord, PAD_VarRecQWord: packed record VarQWord: QWord; end;

      VarRecInt8, PAD_VarRecInt8: packed record VarInt8:  ShortInt; end;
      VarRecInt16, PAD_VarRecInt16: packed record VarInt16: SmallInt; end;
      VarRecInt32, PAD_VarRecInt32: packed record VarInt32: Integer; end;
      VarRecInt64, PAD_VarRecInt64: packed record VarInt64: Int64; end;

      VarRecSub1, PAD_VarRecSub1: packed record VarSub1: TSub1; end;
      VarRecSub2, PAD_VarRecSub2: packed record VarSub2: TSub2; end;
      VarRecSub3, PAD_VarRecSub3: packed record VarSub3: TSub3; end;
      VarRecSub4, PAD_VarRecSub4: packed record VarSub4: TSub4; end;
      VarRecSub5, PAD_VarRecSub5: packed record VarSub5: TSub5; end;

      VarRecPByte, PAD_VarRecPByte: packed record VarPByte:  PByte; end;
      VarRecPWord, PAD_VarRecPWord: packed record VarPWord:  PWord; end;
      VarRecPLong, PAD_VarRecPLong: packed record VarPLong:  PLongWord; end;
      VarRecPQWord, PAD_VarRecPQWord: packed record VarPQWord: PQWord; end;

      VarRecPInt8, PAD_VarRecPInt8: packed record VarPInt8:  PShortInt; end;
      VarRecPInt16, PAD_VarRecPInt16: packed record VarPInt16: PSmallInt; end;
      VarRecPInt32, PAD_VarRecPInt32: packed record VarPInt32: PInteger; end;
      VarRecPInt64, PAD_VarRecPInt64: packed record VarPInt64: PInt64; end;

      VarRecPSub1, PAD_VarRecPSub1: packed record VarPSub1: PSub1; end;
      VarRecPSub2, PAD_VarRecPSub2: packed record VarPSub2: PSub2; end;
      VarRecPSub3, PAD_VarRecPSub3: packed record VarPSub3: PSub3; end;
      VarRecPSub4, PAD_VarRecPSub4: packed record VarPSub4: PSub4; end;
      VarRecPSub5, PAD_VarRecPSub5: packed record VarPSub5: PSub5; end;


      VarRecBoolean, PAD_VarRecBoolean: packed record VarBoolean: Boolean; end;
      VarRecPBoolean, PAD_VarRecPBoolean: packed record VarPBoolean: PBoolean; end;


      VarRecEnum0, PAD_VarRecEnum0: packed record VarEnum0: TEnum0; end;
      VarRecEnum1, PAD_VarRecEnum1: packed record VarEnum1: TEnum1; end;
      VarRecEnum2, PAD_VarRecEnum2: packed record VarEnum2: TEnum2; end;
      VarRecEnum3, PAD_VarRecEnum3: packed record VarEnum3: TEnum3; end;
      VarRecEnumX, PAD_VarRecEnumX: packed record VarEnumX: TEnumX; end;
      VarRecEnumR3, PAD_VarRecEnumR3: packed record VarEnumR3: TEnumR3; end;

      VarRecPEnum0, PAD_VarRecPEnum0: packed record VarPEnum0: PEnum0; end;
      VarRecPEnum1, PAD_VarRecPEnum1: packed record VarPEnum1: PEnum1; end;
      VarRecPEnum2, PAD_VarRecPEnum2: packed record VarPEnum2: PEnum2; end;
      VarRecPEnum3, PAD_VarRecPEnum3: packed record VarPEnum3: PEnum3; end;
      VarRecPEnumX, PAD_VarRecPEnumX: packed record VarPEnumX: PEnumX; end;
      VarRecPEnumR3, PAD_VarRecPEnumR3: packed record VarPEnumR3: PEnumR3; end;


      VarRecSet0, PAD_VarRecSet0: packed record VarSet0: TSet0; end;
      VarRecSet1, PAD_VarRecSet1: packed record VarSet1: TSet1; end;
      VarRecSet2, PAD_VarRecSet2: packed record VarSet2: TSet2; end;
      VarRecSet3, PAD_VarRecSet3: packed record VarSet3: TSet3; end;
      VarRecSetX1, PAD_VarRecSetX1: packed record VarSetX1: TSetX1; end;
      VarRecSetX2, PAD_VarRecSetX2: packed record VarSetX2: set of (rsxa, rsxb, rsxc, rsxd); end;
      VarRecSetB1, PAD_VarRecSetB1: packed record VarSetB1: TSetB1; end;
      VarRecSetB2, PAD_VarRecSetB2: packed record VarSetB2: TSetB2; end;
      VarRecSetC1, PAD_VarRecSetC1: packed record VarSetC1: TSetC1; end;
      VarRecSetC2, PAD_VarRecSetC2: packed record VarSetC2: set of char; end;
      VarRecSetR3, PAD_VarRecSetR3: packed record VarSetR3: TSetR3; end;

      VarRecPSet0, PAD_VarRecPSet0: packed record VarPSet0: PSet0; end;
      VarRecPSet1, PAD_VarRecPSet1: packed record VarPSet1: PSet1; end;
      VarRecPSet2, PAD_VarRecPSet2: packed record VarPSet2: PSet2; end;
      VarRecPSet3, PAD_VarRecPSet3: packed record VarPSet3: PSet3; end;
      VarRecPSetX1, PAD_VarRecPSetX1: packed record VarPSetX1: PSetX1; end;
      VarRecPSetB1, PAD_VarRecPSetB1: packed record VarPSetB1: PSetB1; end;
      VarRecPSetB2, PAD_VarRecPSetB2: packed record VarPSetB2: PSetB2; end;
      VarRecPSetC1, PAD_VarRecPSetC1: packed record VarPSetC1: PSetC1; end;
      VarRecPSetR3, PAD_VarRecPSetR3: packed record VarPSetR3: PSetR3; end;

      VarRecPSetP0, PAD_VarRecPSetP0: packed record VarPSetP0: PSetP0; end;
      VarRecPSetP1, PAD_VarRecPSetP1: packed record VarPSetP1: PSetP1; end;
      VarRecPSetP2, PAD_VarRecPSetP2: packed record VarPSetP2: PSetP2; end;
      VarRecPSetP3, PAD_VarRecPSetP3: packed record VarPSetP3: PSetP3; end;
      VarRecPSetPX1, PAD_VarRecPSetPX1: packed record VarPSetPX1: PSetPX1; end;
      VarRecPSetPB1, PAD_VarRecPSetPB1: packed record VarPSetPB1: PSetPB1; end;
      VarRecPSetPB2, PAD_VarRecPSetPB2: packed record VarPSetPB2: PSetPB2; end;
      VarRecPSetPC1, PAD_VarRecPSetPC1: packed record VarPSetPC1: PSetPC1; end;
      VarRecPSetPR3, PAD_VarRecPSetPR3: packed record VarPSetPR3: PSetPR3; end;


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

  VarVARBYTE_1 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARBYTE_1.Tag := DW_TAG_variable;
  VarVARBYTE_1.Children := 0;
  VarVARBYTE_1.Add(DW_AT_name, DW_FORM_string, 'VARBYTE'+#0);
  VarVARBYTE_1.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarBYTE)])); // $03, $00, $90, $40, $00
  VarVARBYTE_1.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_141); // $6A, $0C, $00, $00

  VarVARWORD_2 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARWORD_2.Tag := DW_TAG_variable;
  VarVARWORD_2.Children := 0;
  VarVARWORD_2.Add(DW_AT_name, DW_FORM_string, 'VARWORD'+#0);
  VarVARWORD_2.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarWORD)])); // $03, $00, $00, $00, $00
  VarVARWORD_2.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_147); // $A0, $0C, $00, $00

  VarVARLONG_3 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARLONG_3.Tag := DW_TAG_variable;
  VarVARLONG_3.Children := 0;
  VarVARLONG_3.Add(DW_AT_name, DW_FORM_string, 'VARLONG'+#0);
  VarVARLONG_3.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarLONG)])); // $03, $00, $00, $00, $00
  VarVARLONG_3.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_153); // $D6, $0C, $00, $00

  VarVARQWORD_4 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARQWORD_4.Tag := DW_TAG_variable;
  VarVARQWORD_4.Children := 0;
  VarVARQWORD_4.Add(DW_AT_name, DW_FORM_string, 'VARQWORD'+#0);
  VarVARQWORD_4.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarQWORD)])); // $03, $00, $00, $00, $00
  VarVARQWORD_4.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_159); // $12, $0D, $00, $00

  VarVARINT8_5 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARINT8_5.Tag := DW_TAG_variable;
  VarVARINT8_5.Children := 0;
  VarVARINT8_5.Add(DW_AT_name, DW_FORM_string, 'VARINT8'+#0);
  VarVARINT8_5.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarINT8)])); // $03, $00, $00, $00, $00
  VarVARINT8_5.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_144); // $81, $0C, $00, $00

  VarVARINT16_6 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARINT16_6.Tag := DW_TAG_variable;
  VarVARINT16_6.Children := 0;
  VarVARINT16_6.Add(DW_AT_name, DW_FORM_string, 'VARINT16'+#0);
  VarVARINT16_6.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarINT16)])); // $03, $00, $00, $00, $00
  VarVARINT16_6.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSMALLINT_150); // $B7, $0C, $00, $00

  VarVARINT32_7 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARINT32_7.Tag := DW_TAG_variable;
  VarVARINT32_7.Children := 0;
  VarVARINT32_7.Add(DW_AT_name, DW_FORM_string, 'VARINT32'+#0);
  VarVARINT32_7.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarINT32)])); // $03, $00, $00, $00, $00
  VarVARINT32_7.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_156); // $F5, $0C, $00, $00

  VarVARINT64_8 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARINT64_8.Tag := DW_TAG_variable;
  VarVARINT64_8.Children := 0;
  VarVARINT64_8.Add(DW_AT_name, DW_FORM_string, 'VARINT64'+#0);
  VarVARINT64_8.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarINT64)])); // $03, $00, $00, $00, $00
  VarVARINT64_8.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclINT64_162); // $2B, $0D, $00, $00

  VarVARSUB1_9 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSUB1_9.Tag := DW_TAG_variable;
  VarVARSUB1_9.Children := 0;
  VarVARSUB1_9.Add(DW_AT_name, DW_FORM_string, 'VARSUB1'+#0);
  VarVARSUB1_9.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSUB1)])); // $03, $00, $00, $00, $00
  VarVARSUB1_9.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB1_192); // $1A, $0E, $00, $00

  VarVARSUB2_10 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSUB2_10.Tag := DW_TAG_variable;
  VarVARSUB2_10.Children := 0;
  VarVARSUB2_10.Add(DW_AT_name, DW_FORM_string, 'VARSUB2'+#0);
  VarVARSUB2_10.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSUB2)])); // $03, $00, $00, $00, $00
  VarVARSUB2_10.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB2_195); // $37, $0E, $00, $00

  VarVARSUB3_11 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSUB3_11.Tag := DW_TAG_variable;
  VarVARSUB3_11.Children := 0;
  VarVARSUB3_11.Add(DW_AT_name, DW_FORM_string, 'VARSUB3'+#0);
  VarVARSUB3_11.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSUB3)])); // $03, $00, $00, $00, $00
  VarVARSUB3_11.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB3_198); // $57, $0E, $00, $00

  VarVARSUB4_12 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSUB4_12.Tag := DW_TAG_variable;
  VarVARSUB4_12.Children := 0;
  VarVARSUB4_12.Add(DW_AT_name, DW_FORM_string, 'VARSUB4'+#0);
  VarVARSUB4_12.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSUB4)])); // $03, $00, $00, $00, $00
  VarVARSUB4_12.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB4_201); // $75, $0E, $00, $00

  VarVARSUB5_13 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSUB5_13.Tag := DW_TAG_variable;
  VarVARSUB5_13.Children := 0;
  VarVARSUB5_13.Add(DW_AT_name, DW_FORM_string, 'VARSUB5'+#0);
  VarVARSUB5_13.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSUB5)])); // $03, $00, $00, $00, $00
  VarVARSUB5_13.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB5_204); // $92, $0E, $00, $00

  VarVARPBYTE_14 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPBYTE_14.Tag := DW_TAG_variable;
  VarVARPBYTE_14.Children := 0;
  VarVARPBYTE_14.Add(DW_AT_name, DW_FORM_string, 'VARPBYTE'+#0);
  VarVARPBYTE_14.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPBYTE)])); // $03, $00, $00, $00, $00
  VarVARPBYTE_14.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPBYTE_168); // $61, $0D, $00, $00

  VarVARPWORD_15 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPWORD_15.Tag := DW_TAG_variable;
  VarVARPWORD_15.Children := 0;
  VarVARPWORD_15.Add(DW_AT_name, DW_FORM_string, 'VARPWORD'+#0);
  VarVARPWORD_15.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPWORD)])); // $03, $00, $00, $00, $00
  VarVARPWORD_15.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPWORD_171); // $76, $0D, $00, $00

  VarVARPLONG_16 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPLONG_16.Tag := DW_TAG_variable;
  VarVARPLONG_16.Children := 0;
  VarVARPLONG_16.Add(DW_AT_name, DW_FORM_string, 'VARPLONG'+#0);
  VarVARPLONG_16.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPLONG)])); // $03, $00, $00, $00, $00
  VarVARPLONG_16.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPLONGWORD_174); // $8B, $0D, $00, $00

  VarVARPQWORD_17 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPQWORD_17.Tag := DW_TAG_variable;
  VarVARPQWORD_17.Children := 0;
  VarVARPQWORD_17.Add(DW_AT_name, DW_FORM_string, 'VARPQWORD'+#0);
  VarVARPQWORD_17.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPQWORD)])); // $03, $00, $00, $00, $00
  VarVARPQWORD_17.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPQWORD_177); // $A4, $0D, $00, $00

  VarVARPINT8_18 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPINT8_18.Tag := DW_TAG_variable;
  VarVARPINT8_18.Children := 0;
  VarVARPINT8_18.Add(DW_AT_name, DW_FORM_string, 'VARPINT8'+#0);
  VarVARPINT8_18.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPINT8)])); // $03, $00, $00, $00, $00
  VarVARPINT8_18.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSHORTINT_180); // $BA, $0D, $00, $00

  VarVARPINT16_19 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPINT16_19.Tag := DW_TAG_variable;
  VarVARPINT16_19.Children := 0;
  VarVARPINT16_19.Add(DW_AT_name, DW_FORM_string, 'VARPINT16'+#0);
  VarVARPINT16_19.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPINT16)])); // $03, $00, $00, $00, $00
  VarVARPINT16_19.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSMALLINT_183); // $D3, $0D, $00, $00

  VarVARPINT32_20 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPINT32_20.Tag := DW_TAG_variable;
  VarVARPINT32_20.Children := 0;
  VarVARPINT32_20.Add(DW_AT_name, DW_FORM_string, 'VARPINT32'+#0);
  VarVARPINT32_20.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPINT32)])); // $03, $00, $00, $00, $00
  VarVARPINT32_20.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTEGER_186); // $EC, $0D, $00, $00

  VarVARPINT64_21 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPINT64_21.Tag := DW_TAG_variable;
  VarVARPINT64_21.Children := 0;
  VarVARPINT64_21.Add(DW_AT_name, DW_FORM_string, 'VARPINT64'+#0);
  VarVARPINT64_21.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPINT64)])); // $03, $00, $00, $00, $00
  VarVARPINT64_21.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINT64_189); // $04, $0E, $00, $00

  VarVARPSUB1_22 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSUB1_22.Tag := DW_TAG_variable;
  VarVARPSUB1_22.Children := 0;
  VarVARPSUB1_22.Add(DW_AT_name, DW_FORM_string, 'VARPSUB1'+#0);
  VarVARPSUB1_22.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSUB1)])); // $03, $00, $00, $00, $00
  VarVARPSUB1_22.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB1_207); // $AF, $0E, $00, $00

  VarVARPSUB2_23 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSUB2_23.Tag := DW_TAG_variable;
  VarVARPSUB2_23.Children := 0;
  VarVARPSUB2_23.Add(DW_AT_name, DW_FORM_string, 'VARPSUB2'+#0);
  VarVARPSUB2_23.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSUB2)])); // $03, $00, $00, $00, $00
  VarVARPSUB2_23.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB2_210); // $C4, $0E, $00, $00

  VarVARPSUB3_24 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSUB3_24.Tag := DW_TAG_variable;
  VarVARPSUB3_24.Children := 0;
  VarVARPSUB3_24.Add(DW_AT_name, DW_FORM_string, 'VARPSUB3'+#0);
  VarVARPSUB3_24.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSUB3)])); // $03, $00, $00, $00, $00
  VarVARPSUB3_24.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB3_213); // $D9, $0E, $00, $00

  VarVARPSUB4_25 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSUB4_25.Tag := DW_TAG_variable;
  VarVARPSUB4_25.Children := 0;
  VarVARPSUB4_25.Add(DW_AT_name, DW_FORM_string, 'VARPSUB4'+#0);
  VarVARPSUB4_25.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSUB4)])); // $03, $00, $00, $00, $00
  VarVARPSUB4_25.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB4_216); // $EE, $0E, $00, $00

  VarVARPSUB5_26 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSUB5_26.Tag := DW_TAG_variable;
  VarVARPSUB5_26.Children := 0;
  VarVARPSUB5_26.Add(DW_AT_name, DW_FORM_string, 'VARPSUB5'+#0);
  VarVARPSUB5_26.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSUB5)])); // $03, $00, $00, $00, $00
  VarVARPSUB5_26.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB5_219); // $03, $0F, $00, $00

  VarVARBOOLEAN_27 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARBOOLEAN_27.Tag := DW_TAG_variable;
  VarVARBOOLEAN_27.Children := 0;
  VarVARBOOLEAN_27.Add(DW_AT_name, DW_FORM_string, 'VARBOOLEAN'+#0);
  VarVARBOOLEAN_27.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarBOOLEAN)])); // $03, $00, $00, $00, $00
  VarVARBOOLEAN_27.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_165); // $44, $0D, $00, $00

  VarVARPBOOLEAN_28 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPBOOLEAN_28.Tag := DW_TAG_variable;
  VarVARPBOOLEAN_28.Children := 0;
  VarVARPBOOLEAN_28.Add(DW_AT_name, DW_FORM_string, 'VARPBOOLEAN'+#0);
  VarVARPBOOLEAN_28.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPBOOLEAN)])); // $03, $00, $00, $00, $00
  VarVARPBOOLEAN_28.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPBOOLEAN_222); // $18, $0F, $00, $00

  VarVARENUM0_29 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARENUM0_29.Tag := DW_TAG_variable;
  VarVARENUM0_29.Children := 0;
  VarVARENUM0_29.Add(DW_AT_name, DW_FORM_string, 'VARENUM0'+#0);
  VarVARENUM0_29.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarENUM0)])); // $03, $10, $90, $40, $00
  VarVARENUM0_29.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM0_225); // $30, $0F, $00, $00

  VarVARENUM1_30 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARENUM1_30.Tag := DW_TAG_variable;
  VarVARENUM1_30.Children := 0;
  VarVARENUM1_30.Add(DW_AT_name, DW_FORM_string, 'VARENUM1'+#0);
  VarVARENUM1_30.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarENUM1)])); // $03, $20, $90, $40, $00
  VarVARENUM1_30.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM1_229); // $54, $0F, $00, $00

  VarVARENUM2_31 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARENUM2_31.Tag := DW_TAG_variable;
  VarVARENUM2_31.Children := 0;
  VarVARENUM2_31.Add(DW_AT_name, DW_FORM_string, 'VARENUM2'+#0);
  VarVARENUM2_31.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarENUM2)])); // $03, $30, $90, $40, $00
  VarVARENUM2_31.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM2_235); // $8A, $0F, $00, $00

  VarVARENUM3_32 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARENUM3_32.Tag := DW_TAG_variable;
  VarVARENUM3_32.Children := 0;
  VarVARENUM3_32.Add(DW_AT_name, DW_FORM_string, 'VARENUM3'+#0);
  VarVARENUM3_32.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarENUM3)])); // $03, $40, $90, $40, $00
  VarVARENUM3_32.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM3_247); // $F6, $0F, $00, $00

  VarVARENUM4_33 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARENUM4_33.Tag := DW_TAG_variable;
  VarVARENUM4_33.Children := 0;
  VarVARENUM4_33.Add(DW_AT_name, DW_FORM_string, 'VARENUM4'+#0);
  VarVARENUM4_33.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarENUM4)])); // $03, $00, $00, $00, $00
  VarVARENUM4_33.AddRef(DW_AT_type, DW_FORM_ref4, @Type_430); // $21, $16, $00, $00

  VarVARENUMX_34 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARENUMX_34.Tag := DW_TAG_variable;
  VarVARENUMX_34.Children := 0;
  VarVARENUMX_34.Add(DW_AT_name, DW_FORM_string, 'VARENUMX'+#0);
  VarVARENUMX_34.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarENUMX)])); // $03, $50, $90, $40, $00
  VarVARENUMX_34.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMX_267); // $AA, $10, $00, $00

  VarVARENUMR3_35 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARENUMR3_35.Tag := DW_TAG_variable;
  VarVARENUMR3_35.Children := 0;
  VarVARENUMR3_35.Add(DW_AT_name, DW_FORM_string, 'VARENUMR3'+#0);
  VarVARENUMR3_35.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarENUMR3)])); // $03, $00, $00, $00, $00
  VarVARENUMR3_35.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMR3_273); // $E0, $10, $00, $00

  VarVARPENUM0_36 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPENUM0_36.Tag := DW_TAG_variable;
  VarVARPENUM0_36.Children := 0;
  VarVARPENUM0_36.Add(DW_AT_name, DW_FORM_string, 'VARPENUM0'+#0);
  VarVARPENUM0_36.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPENUM0)])); // $03, $00, $00, $00, $00
  VarVARPENUM0_36.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUM0_286); // $5B, $11, $00, $00

  VarVARPENUM1_37 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPENUM1_37.Tag := DW_TAG_variable;
  VarVARPENUM1_37.Children := 0;
  VarVARPENUM1_37.Add(DW_AT_name, DW_FORM_string, 'VARPENUM1'+#0);
  VarVARPENUM1_37.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPENUM1)])); // $03, $00, $00, $00, $00
  VarVARPENUM1_37.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUM1_289); // $71, $11, $00, $00

  VarVARPENUM2_38 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPENUM2_38.Tag := DW_TAG_variable;
  VarVARPENUM2_38.Children := 0;
  VarVARPENUM2_38.Add(DW_AT_name, DW_FORM_string, 'VARPENUM2'+#0);
  VarVARPENUM2_38.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPENUM2)])); // $03, $00, $00, $00, $00
  VarVARPENUM2_38.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUM2_292); // $87, $11, $00, $00

  VarVARPENUM3_39 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPENUM3_39.Tag := DW_TAG_variable;
  VarVARPENUM3_39.Children := 0;
  VarVARPENUM3_39.Add(DW_AT_name, DW_FORM_string, 'VARPENUM3'+#0);
  VarVARPENUM3_39.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPENUM3)])); // $03, $00, $00, $00, $00
  VarVARPENUM3_39.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUM3_295); // $9D, $11, $00, $00

  VarVARPENUMX_40 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPENUMX_40.Tag := DW_TAG_variable;
  VarVARPENUMX_40.Children := 0;
  VarVARPENUMX_40.Add(DW_AT_name, DW_FORM_string, 'VARPENUMX'+#0);
  VarVARPENUMX_40.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPENUMX)])); // $03, $00, $00, $00, $00
  VarVARPENUMX_40.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUMX_298); // $B3, $11, $00, $00

  VarVARPENUMR3_41 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPENUMR3_41.Tag := DW_TAG_variable;
  VarVARPENUMR3_41.Children := 0;
  VarVARPENUMR3_41.Add(DW_AT_name, DW_FORM_string, 'VARPENUMR3'+#0);
  VarVARPENUMR3_41.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPENUMR3)])); // $03, $00, $00, $00, $00
  VarVARPENUMR3_41.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUMR3_301); // $C9, $11, $00, $00

  VarVARSET0_42 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSET0_42.Tag := DW_TAG_variable;
  VarVARSET0_42.Children := 0;
  VarVARSET0_42.Add(DW_AT_name, DW_FORM_string, 'VARSET0'+#0);
  VarVARSET0_42.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSET0)])); // $03, $60, $90, $40, $00
  VarVARSET0_42.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET0_304); // $E0, $11, $00, $00

  VarVARSET1_43 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSET1_43.Tag := DW_TAG_variable;
  VarVARSET1_43.Children := 0;
  VarVARSET1_43.Add(DW_AT_name, DW_FORM_string, 'VARSET1'+#0);
  VarVARSET1_43.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSET1)])); // $03, $70, $90, $40, $00
  VarVARSET1_43.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET1_308); // $04, $12, $00, $00

  VarVARSET2_44 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSET2_44.Tag := DW_TAG_variable;
  VarVARSET2_44.Children := 0;
  VarVARSET2_44.Add(DW_AT_name, DW_FORM_string, 'VARSET2'+#0);
  VarVARSET2_44.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSET2)])); // $03, $80, $90, $40, $00
  VarVARSET2_44.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET2_312); // $28, $12, $00, $00

  VarVARSET3_45 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSET3_45.Tag := DW_TAG_variable;
  VarVARSET3_45.Children := 0;
  VarVARSET3_45.Add(DW_AT_name, DW_FORM_string, 'VARSET3'+#0);
  VarVARSET3_45.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSET3)])); // $03, $90, $90, $40, $00
  VarVARSET3_45.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET3_316); // $4C, $12, $00, $00

  VarVARSETX1_46 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSETX1_46.Tag := DW_TAG_variable;
  VarVARSETX1_46.Children := 0;
  VarVARSETX1_46.Add(DW_AT_name, DW_FORM_string, 'VARSETX1'+#0);
  VarVARSETX1_46.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSETX1)])); // $03, $A0, $90, $40, $00
  VarVARSETX1_46.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETX1_320); // $70, $12, $00, $00

  VarVARSETX2_47 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSETX2_47.Tag := DW_TAG_variable;
  VarVARSETX2_47.Children := 0;
  VarVARSETX2_47.Add(DW_AT_name, DW_FORM_string, 'VARSETX2'+#0);
  VarVARSETX2_47.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSETX2)])); // $03, $B0, $90, $40, $00
  VarVARSETX2_47.AddRef(DW_AT_type, DW_FORM_ref4, @Type_436); // $4D, $16, $00, $00

  VarVARSETB1_48 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSETB1_48.Tag := DW_TAG_variable;
  VarVARSETB1_48.Children := 0;
  VarVARSETB1_48.Add(DW_AT_name, DW_FORM_string, 'VARSETB1'+#0);
  VarVARSETB1_48.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSETB1)])); // $03, $C0, $90, $40, $00
  VarVARSETB1_48.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETB1_324); // $96, $12, $00, $00

  VarVARSETB2_49 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSETB2_49.Tag := DW_TAG_variable;
  VarVARSETB2_49.Children := 0;
  VarVARSETB2_49.Add(DW_AT_name, DW_FORM_string, 'VARSETB2'+#0);
  VarVARSETB2_49.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSETB2)])); // $03, $E0, $90, $40, $00
  VarVARSETB2_49.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETB2_328); // $BD, $12, $00, $00

  VarVARSETC1_50 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSETC1_50.Tag := DW_TAG_variable;
  VarVARSETC1_50.Children := 0;
  VarVARSETC1_50.Add(DW_AT_name, DW_FORM_string, 'VARSETC1'+#0);
  VarVARSETC1_50.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSETC1)])); // $03, $00, $91, $40, $00
  VarVARSETC1_50.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETC1_332); // $E4, $12, $00, $00

  VarVARSETC2_51 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSETC2_51.Tag := DW_TAG_variable;
  VarVARSETC2_51.Children := 0;
  VarVARSETC2_51.Add(DW_AT_name, DW_FORM_string, 'VARSETC2'+#0);
  VarVARSETC2_51.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSETC2)])); // $03, $00, $00, $00, $00
  VarVARSETC2_51.AddRef(DW_AT_type, DW_FORM_ref4, @Type_439); // $60, $16, $00, $00

  VarVARSETR3_52 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARSETR3_52.Tag := DW_TAG_variable;
  VarVARSETR3_52.Children := 0;
  VarVARSETR3_52.Add(DW_AT_name, DW_FORM_string, 'VARSETR3'+#0);
  VarVARSETR3_52.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarSETR3)])); // $03, $00, $00, $00, $00
  VarVARSETR3_52.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETR3_336); // $0B, $13, $00, $00

  VarVARPSET0_53 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSET0_53.Tag := DW_TAG_variable;
  VarVARPSET0_53.Children := 0;
  VarVARPSET0_53.Add(DW_AT_name, DW_FORM_string, 'VARPSET0'+#0);
  VarVARPSET0_53.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSET0)])); // $03, $00, $00, $00, $00
  VarVARPSET0_53.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSET0_376); // $94, $14, $00, $00

  VarVARPSET1_54 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSET1_54.Tag := DW_TAG_variable;
  VarVARPSET1_54.Children := 0;
  VarVARPSET1_54.Add(DW_AT_name, DW_FORM_string, 'VARPSET1'+#0);
  VarVARPSET1_54.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSET1)])); // $03, $00, $00, $00, $00
  VarVARPSET1_54.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSET1_379); // $A9, $14, $00, $00

  VarVARPSET2_55 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSET2_55.Tag := DW_TAG_variable;
  VarVARPSET2_55.Children := 0;
  VarVARPSET2_55.Add(DW_AT_name, DW_FORM_string, 'VARPSET2'+#0);
  VarVARPSET2_55.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSET2)])); // $03, $00, $00, $00, $00
  VarVARPSET2_55.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSET2_382); // $BE, $14, $00, $00

  VarVARPSET3_56 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSET3_56.Tag := DW_TAG_variable;
  VarVARPSET3_56.Children := 0;
  VarVARPSET3_56.Add(DW_AT_name, DW_FORM_string, 'VARPSET3'+#0);
  VarVARPSET3_56.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSET3)])); // $03, $00, $00, $00, $00
  VarVARPSET3_56.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSET3_385); // $D3, $14, $00, $00

  VarVARPSETX1_57 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSETX1_57.Tag := DW_TAG_variable;
  VarVARPSETX1_57.Children := 0;
  VarVARPSETX1_57.Add(DW_AT_name, DW_FORM_string, 'VARPSETX1'+#0);
  VarVARPSETX1_57.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSETX1)])); // $03, $00, $00, $00, $00
  VarVARPSETX1_57.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETX1_388); // $E8, $14, $00, $00

  VarVARPSETB1_58 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSETB1_58.Tag := DW_TAG_variable;
  VarVARPSETB1_58.Children := 0;
  VarVARPSETB1_58.Add(DW_AT_name, DW_FORM_string, 'VARPSETB1'+#0);
  VarVARPSETB1_58.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSETB1)])); // $03, $00, $00, $00, $00
  VarVARPSETB1_58.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETB1_391); // $FE, $14, $00, $00

  VarVARPSETB2_59 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSETB2_59.Tag := DW_TAG_variable;
  VarVARPSETB2_59.Children := 0;
  VarVARPSETB2_59.Add(DW_AT_name, DW_FORM_string, 'VARPSETB2'+#0);
  VarVARPSETB2_59.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSETB2)])); // $03, $00, $00, $00, $00
  VarVARPSETB2_59.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETB2_394); // $14, $15, $00, $00

  VarVARPSETC1_60 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSETC1_60.Tag := DW_TAG_variable;
  VarVARPSETC1_60.Children := 0;
  VarVARPSETC1_60.Add(DW_AT_name, DW_FORM_string, 'VARPSETC1'+#0);
  VarVARPSETC1_60.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSETC1)])); // $03, $00, $00, $00, $00
  VarVARPSETC1_60.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETC1_397); // $2A, $15, $00, $00

  VarVARPSETR3_61 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSETR3_61.Tag := DW_TAG_variable;
  VarVARPSETR3_61.Children := 0;
  VarVARPSETR3_61.Add(DW_AT_name, DW_FORM_string, 'VARPSETR3'+#0);
  VarVARPSETR3_61.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSETR3)])); // $03, $00, $00, $00, $00
  VarVARPSETR3_61.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETR3_400); // $40, $15, $00, $00

  VarVARPSETP0_62 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSETP0_62.Tag := DW_TAG_variable;
  VarVARPSETP0_62.Children := 0;
  VarVARPSETP0_62.Add(DW_AT_name, DW_FORM_string, 'VARPSETP0'+#0);
  VarVARPSETP0_62.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSETP0)])); // $03, $00, $00, $00, $00
  VarVARPSETP0_62.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETP0_403); // $56, $15, $00, $00

  VarVARPSETP1_63 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSETP1_63.Tag := DW_TAG_variable;
  VarVARPSETP1_63.Children := 0;
  VarVARPSETP1_63.Add(DW_AT_name, DW_FORM_string, 'VARPSETP1'+#0);
  VarVARPSETP1_63.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSETP1)])); // $03, $00, $00, $00, $00
  VarVARPSETP1_63.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETP1_406); // $6C, $15, $00, $00

  VarVARPSETP2_64 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSETP2_64.Tag := DW_TAG_variable;
  VarVARPSETP2_64.Children := 0;
  VarVARPSETP2_64.Add(DW_AT_name, DW_FORM_string, 'VARPSETP2'+#0);
  VarVARPSETP2_64.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSETP2)])); // $03, $00, $00, $00, $00
  VarVARPSETP2_64.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETP2_409); // $82, $15, $00, $00

  VarVARPSETP3_65 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSETP3_65.Tag := DW_TAG_variable;
  VarVARPSETP3_65.Children := 0;
  VarVARPSETP3_65.Add(DW_AT_name, DW_FORM_string, 'VARPSETP3'+#0);
  VarVARPSETP3_65.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSETP3)])); // $03, $00, $00, $00, $00
  VarVARPSETP3_65.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETP3_412); // $98, $15, $00, $00

  VarVARPSETPX1_66 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSETPX1_66.Tag := DW_TAG_variable;
  VarVARPSETPX1_66.Children := 0;
  VarVARPSETPX1_66.Add(DW_AT_name, DW_FORM_string, 'VARPSETPX1'+#0);
  VarVARPSETPX1_66.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSETPX1)])); // $03, $00, $00, $00, $00
  VarVARPSETPX1_66.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPX1_415); // $AE, $15, $00, $00

  VarVARPSETPB1_67 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSETPB1_67.Tag := DW_TAG_variable;
  VarVARPSETPB1_67.Children := 0;
  VarVARPSETPB1_67.Add(DW_AT_name, DW_FORM_string, 'VARPSETPB1'+#0);
  VarVARPSETPB1_67.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSETPB1)])); // $03, $00, $00, $00, $00
  VarVARPSETPB1_67.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPB1_418); // $C5, $15, $00, $00

  VarVARPSETPB2_68 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSETPB2_68.Tag := DW_TAG_variable;
  VarVARPSETPB2_68.Children := 0;
  VarVARPSETPB2_68.Add(DW_AT_name, DW_FORM_string, 'VARPSETPB2'+#0);
  VarVARPSETPB2_68.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSETPB2)])); // $03, $00, $00, $00, $00
  VarVARPSETPB2_68.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPB2_421); // $DC, $15, $00, $00

  VarVARPSETPC1_69 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSETPC1_69.Tag := DW_TAG_variable;
  VarVARPSETPC1_69.Children := 0;
  VarVARPSETPC1_69.Add(DW_AT_name, DW_FORM_string, 'VARPSETPC1'+#0);
  VarVARPSETPC1_69.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSETPC1)])); // $03, $00, $00, $00, $00
  VarVARPSETPC1_69.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPC1_424); // $F3, $15, $00, $00

  VarVARPSETPR3_70 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARPSETPR3_70.Tag := DW_TAG_variable;
  VarVARPSETPR3_70.Children := 0;
  VarVARPSETPR3_70.Add(DW_AT_name, DW_FORM_string, 'VARPSETPR3'+#0);
  VarVARPSETPR3_70.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarPSETPR3)])); // $03, $00, $00, $00, $00
  VarVARPSETPR3_70.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPR3_427); // $0A, $16, $00, $00

  VarVARRECBYTE_71 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECBYTE_71.Tag := DW_TAG_variable;
  VarVARRECBYTE_71.Children := 0;
  VarVARRECBYTE_71.Add(DW_AT_name, DW_FORM_string, 'VARRECBYTE'+#0);
  VarVARRECBYTE_71.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECBYTE)])); // $03, $00, $00, $00, $00
  VarVARRECBYTE_71.AddRef(DW_AT_type, DW_FORM_ref4, @Type_718); // $33, $24, $00, $00

  VarVARRECWORD_72 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECWORD_72.Tag := DW_TAG_variable;
  VarVARRECWORD_72.Children := 0;
  VarVARRECWORD_72.Add(DW_AT_name, DW_FORM_string, 'VARRECWORD'+#0);
  VarVARRECWORD_72.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECWORD)])); // $03, $00, $00, $00, $00
  VarVARRECWORD_72.AddRef(DW_AT_type, DW_FORM_ref4, @Type_721); // $4B, $24, $00, $00

  VarVARRECLONG_73 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECLONG_73.Tag := DW_TAG_variable;
  VarVARRECLONG_73.Children := 0;
  VarVARRECLONG_73.Add(DW_AT_name, DW_FORM_string, 'VARRECLONG'+#0);
  VarVARRECLONG_73.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECLONG)])); // $03, $00, $00, $00, $00
  VarVARRECLONG_73.AddRef(DW_AT_type, DW_FORM_ref4, @Type_724); // $63, $24, $00, $00

  VarVARRECQWORD_74 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECQWORD_74.Tag := DW_TAG_variable;
  VarVARRECQWORD_74.Children := 0;
  VarVARRECQWORD_74.Add(DW_AT_name, DW_FORM_string, 'VARRECQWORD'+#0);
  VarVARRECQWORD_74.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECQWORD)])); // $03, $00, $00, $00, $00
  VarVARRECQWORD_74.AddRef(DW_AT_type, DW_FORM_ref4, @Type_727); // $7B, $24, $00, $00

  VarVARRECINT8_75 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECINT8_75.Tag := DW_TAG_variable;
  VarVARRECINT8_75.Children := 0;
  VarVARRECINT8_75.Add(DW_AT_name, DW_FORM_string, 'VARRECINT8'+#0);
  VarVARRECINT8_75.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECINT8)])); // $03, $00, $00, $00, $00
  VarVARRECINT8_75.AddRef(DW_AT_type, DW_FORM_ref4, @Type_730); // $94, $24, $00, $00

  VarVARRECINT16_76 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECINT16_76.Tag := DW_TAG_variable;
  VarVARRECINT16_76.Children := 0;
  VarVARRECINT16_76.Add(DW_AT_name, DW_FORM_string, 'VARRECINT16'+#0);
  VarVARRECINT16_76.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECINT16)])); // $03, $00, $00, $00, $00
  VarVARRECINT16_76.AddRef(DW_AT_type, DW_FORM_ref4, @Type_733); // $AC, $24, $00, $00

  VarVARRECINT32_77 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECINT32_77.Tag := DW_TAG_variable;
  VarVARRECINT32_77.Children := 0;
  VarVARRECINT32_77.Add(DW_AT_name, DW_FORM_string, 'VARRECINT32'+#0);
  VarVARRECINT32_77.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECINT32)])); // $03, $00, $00, $00, $00
  VarVARRECINT32_77.AddRef(DW_AT_type, DW_FORM_ref4, @Type_736); // $C5, $24, $00, $00

  VarVARRECINT64_78 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECINT64_78.Tag := DW_TAG_variable;
  VarVARRECINT64_78.Children := 0;
  VarVARRECINT64_78.Add(DW_AT_name, DW_FORM_string, 'VARRECINT64'+#0);
  VarVARRECINT64_78.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECINT64)])); // $03, $00, $00, $00, $00
  VarVARRECINT64_78.AddRef(DW_AT_type, DW_FORM_ref4, @Type_739); // $DE, $24, $00, $00

  VarVARRECSUB1_79 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSUB1_79.Tag := DW_TAG_variable;
  VarVARRECSUB1_79.Children := 0;
  VarVARRECSUB1_79.Add(DW_AT_name, DW_FORM_string, 'VARRECSUB1'+#0);
  VarVARRECSUB1_79.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSUB1)])); // $03, $00, $00, $00, $00
  VarVARRECSUB1_79.AddRef(DW_AT_type, DW_FORM_ref4, @Type_742); // $F7, $24, $00, $00

  VarVARRECSUB2_80 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSUB2_80.Tag := DW_TAG_variable;
  VarVARRECSUB2_80.Children := 0;
  VarVARRECSUB2_80.Add(DW_AT_name, DW_FORM_string, 'VARRECSUB2'+#0);
  VarVARRECSUB2_80.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSUB2)])); // $03, $00, $00, $00, $00
  VarVARRECSUB2_80.AddRef(DW_AT_type, DW_FORM_ref4, @Type_745); // $0F, $25, $00, $00

  VarVARRECSUB3_81 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSUB3_81.Tag := DW_TAG_variable;
  VarVARRECSUB3_81.Children := 0;
  VarVARRECSUB3_81.Add(DW_AT_name, DW_FORM_string, 'VARRECSUB3'+#0);
  VarVARRECSUB3_81.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSUB3)])); // $03, $00, $00, $00, $00
  VarVARRECSUB3_81.AddRef(DW_AT_type, DW_FORM_ref4, @Type_748); // $27, $25, $00, $00

  VarVARRECSUB4_82 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSUB4_82.Tag := DW_TAG_variable;
  VarVARRECSUB4_82.Children := 0;
  VarVARRECSUB4_82.Add(DW_AT_name, DW_FORM_string, 'VARRECSUB4'+#0);
  VarVARRECSUB4_82.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSUB4)])); // $03, $00, $00, $00, $00
  VarVARRECSUB4_82.AddRef(DW_AT_type, DW_FORM_ref4, @Type_751); // $3F, $25, $00, $00

  VarVARRECSUB5_83 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSUB5_83.Tag := DW_TAG_variable;
  VarVARRECSUB5_83.Children := 0;
  VarVARRECSUB5_83.Add(DW_AT_name, DW_FORM_string, 'VARRECSUB5'+#0);
  VarVARRECSUB5_83.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSUB5)])); // $03, $00, $00, $00, $00
  VarVARRECSUB5_83.AddRef(DW_AT_type, DW_FORM_ref4, @Type_754); // $57, $25, $00, $00

  VarVARRECPBYTE_84 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPBYTE_84.Tag := DW_TAG_variable;
  VarVARRECPBYTE_84.Children := 0;
  VarVARRECPBYTE_84.Add(DW_AT_name, DW_FORM_string, 'VARRECPBYTE'+#0);
  VarVARRECPBYTE_84.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPBYTE)])); // $03, $00, $00, $00, $00
  VarVARRECPBYTE_84.AddRef(DW_AT_type, DW_FORM_ref4, @Type_757); // $6F, $25, $00, $00

  VarVARRECPWORD_85 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPWORD_85.Tag := DW_TAG_variable;
  VarVARRECPWORD_85.Children := 0;
  VarVARRECPWORD_85.Add(DW_AT_name, DW_FORM_string, 'VARRECPWORD'+#0);
  VarVARRECPWORD_85.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPWORD)])); // $03, $00, $00, $00, $00
  VarVARRECPWORD_85.AddRef(DW_AT_type, DW_FORM_ref4, @Type_760); // $88, $25, $00, $00

  VarVARRECPLONG_86 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPLONG_86.Tag := DW_TAG_variable;
  VarVARRECPLONG_86.Children := 0;
  VarVARRECPLONG_86.Add(DW_AT_name, DW_FORM_string, 'VARRECPLONG'+#0);
  VarVARRECPLONG_86.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPLONG)])); // $03, $00, $00, $00, $00
  VarVARRECPLONG_86.AddRef(DW_AT_type, DW_FORM_ref4, @Type_763); // $A1, $25, $00, $00

  VarVARRECPQWORD_87 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPQWORD_87.Tag := DW_TAG_variable;
  VarVARRECPQWORD_87.Children := 0;
  VarVARRECPQWORD_87.Add(DW_AT_name, DW_FORM_string, 'VARRECPQWORD'+#0);
  VarVARRECPQWORD_87.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPQWORD)])); // $03, $00, $00, $00, $00
  VarVARRECPQWORD_87.AddRef(DW_AT_type, DW_FORM_ref4, @Type_766); // $BA, $25, $00, $00

  VarVARRECPINT8_88 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPINT8_88.Tag := DW_TAG_variable;
  VarVARRECPINT8_88.Children := 0;
  VarVARRECPINT8_88.Add(DW_AT_name, DW_FORM_string, 'VARRECPINT8'+#0);
  VarVARRECPINT8_88.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPINT8)])); // $03, $00, $00, $00, $00
  VarVARRECPINT8_88.AddRef(DW_AT_type, DW_FORM_ref4, @Type_769); // $D4, $25, $00, $00

  VarVARRECPINT16_89 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPINT16_89.Tag := DW_TAG_variable;
  VarVARRECPINT16_89.Children := 0;
  VarVARRECPINT16_89.Add(DW_AT_name, DW_FORM_string, 'VARRECPINT16'+#0);
  VarVARRECPINT16_89.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPINT16)])); // $03, $00, $00, $00, $00
  VarVARRECPINT16_89.AddRef(DW_AT_type, DW_FORM_ref4, @Type_772); // $ED, $25, $00, $00

  VarVARRECPINT32_90 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPINT32_90.Tag := DW_TAG_variable;
  VarVARRECPINT32_90.Children := 0;
  VarVARRECPINT32_90.Add(DW_AT_name, DW_FORM_string, 'VARRECPINT32'+#0);
  VarVARRECPINT32_90.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPINT32)])); // $03, $00, $00, $00, $00
  VarVARRECPINT32_90.AddRef(DW_AT_type, DW_FORM_ref4, @Type_775); // $07, $26, $00, $00

  VarVARRECPINT64_91 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPINT64_91.Tag := DW_TAG_variable;
  VarVARRECPINT64_91.Children := 0;
  VarVARRECPINT64_91.Add(DW_AT_name, DW_FORM_string, 'VARRECPINT64'+#0);
  VarVARRECPINT64_91.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPINT64)])); // $03, $00, $00, $00, $00
  VarVARRECPINT64_91.AddRef(DW_AT_type, DW_FORM_ref4, @Type_778); // $21, $26, $00, $00

  VarVARRECPSUB1_92 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSUB1_92.Tag := DW_TAG_variable;
  VarVARRECPSUB1_92.Children := 0;
  VarVARRECPSUB1_92.Add(DW_AT_name, DW_FORM_string, 'VARRECPSUB1'+#0);
  VarVARRECPSUB1_92.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSUB1)])); // $03, $00, $00, $00, $00
  VarVARRECPSUB1_92.AddRef(DW_AT_type, DW_FORM_ref4, @Type_781); // $3B, $26, $00, $00

  VarVARRECPSUB2_93 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSUB2_93.Tag := DW_TAG_variable;
  VarVARRECPSUB2_93.Children := 0;
  VarVARRECPSUB2_93.Add(DW_AT_name, DW_FORM_string, 'VARRECPSUB2'+#0);
  VarVARRECPSUB2_93.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSUB2)])); // $03, $00, $00, $00, $00
  VarVARRECPSUB2_93.AddRef(DW_AT_type, DW_FORM_ref4, @Type_784); // $54, $26, $00, $00

  VarVARRECPSUB3_94 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSUB3_94.Tag := DW_TAG_variable;
  VarVARRECPSUB3_94.Children := 0;
  VarVARRECPSUB3_94.Add(DW_AT_name, DW_FORM_string, 'VARRECPSUB3'+#0);
  VarVARRECPSUB3_94.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSUB3)])); // $03, $00, $00, $00, $00
  VarVARRECPSUB3_94.AddRef(DW_AT_type, DW_FORM_ref4, @Type_787); // $6D, $26, $00, $00

  VarVARRECPSUB4_95 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSUB4_95.Tag := DW_TAG_variable;
  VarVARRECPSUB4_95.Children := 0;
  VarVARRECPSUB4_95.Add(DW_AT_name, DW_FORM_string, 'VARRECPSUB4'+#0);
  VarVARRECPSUB4_95.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSUB4)])); // $03, $00, $00, $00, $00
  VarVARRECPSUB4_95.AddRef(DW_AT_type, DW_FORM_ref4, @Type_790); // $86, $26, $00, $00

  VarVARRECPSUB5_96 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSUB5_96.Tag := DW_TAG_variable;
  VarVARRECPSUB5_96.Children := 0;
  VarVARRECPSUB5_96.Add(DW_AT_name, DW_FORM_string, 'VARRECPSUB5'+#0);
  VarVARRECPSUB5_96.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSUB5)])); // $03, $00, $00, $00, $00
  VarVARRECPSUB5_96.AddRef(DW_AT_type, DW_FORM_ref4, @Type_793); // $9F, $26, $00, $00

  VarVARRECBOOLEAN_97 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECBOOLEAN_97.Tag := DW_TAG_variable;
  VarVARRECBOOLEAN_97.Children := 0;
  VarVARRECBOOLEAN_97.Add(DW_AT_name, DW_FORM_string, 'VARRECBOOLEAN'+#0);
  VarVARRECBOOLEAN_97.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECBOOLEAN)])); // $03, $00, $00, $00, $00
  VarVARRECBOOLEAN_97.AddRef(DW_AT_type, DW_FORM_ref4, @Type_796); // $B8, $26, $00, $00

  VarVARRECPBOOLEAN_98 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPBOOLEAN_98.Tag := DW_TAG_variable;
  VarVARRECPBOOLEAN_98.Children := 0;
  VarVARRECPBOOLEAN_98.Add(DW_AT_name, DW_FORM_string, 'VARRECPBOOLEAN'+#0);
  VarVARRECPBOOLEAN_98.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPBOOLEAN)])); // $03, $00, $00, $00, $00
  VarVARRECPBOOLEAN_98.AddRef(DW_AT_type, DW_FORM_ref4, @Type_799); // $D3, $26, $00, $00

  VarVARRECENUM0_99 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECENUM0_99.Tag := DW_TAG_variable;
  VarVARRECENUM0_99.Children := 0;
  VarVARRECENUM0_99.Add(DW_AT_name, DW_FORM_string, 'VARRECENUM0'+#0);
  VarVARRECENUM0_99.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECENUM0)])); // $03, $00, $00, $00, $00
  VarVARRECENUM0_99.AddRef(DW_AT_type, DW_FORM_ref4, @Type_802); // $EF, $26, $00, $00

  VarVARRECENUM1_100 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECENUM1_100.Tag := DW_TAG_variable;
  VarVARRECENUM1_100.Children := 0;
  VarVARRECENUM1_100.Add(DW_AT_name, DW_FORM_string, 'VARRECENUM1'+#0);
  VarVARRECENUM1_100.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECENUM1)])); // $03, $00, $00, $00, $00
  VarVARRECENUM1_100.AddRef(DW_AT_type, DW_FORM_ref4, @Type_805); // $08, $27, $00, $00

  VarVARRECENUM2_101 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECENUM2_101.Tag := DW_TAG_variable;
  VarVARRECENUM2_101.Children := 0;
  VarVARRECENUM2_101.Add(DW_AT_name, DW_FORM_string, 'VARRECENUM2'+#0);
  VarVARRECENUM2_101.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECENUM2)])); // $03, $00, $00, $00, $00
  VarVARRECENUM2_101.AddRef(DW_AT_type, DW_FORM_ref4, @Type_808); // $21, $27, $00, $00

  VarVARRECENUM3_102 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECENUM3_102.Tag := DW_TAG_variable;
  VarVARRECENUM3_102.Children := 0;
  VarVARRECENUM3_102.Add(DW_AT_name, DW_FORM_string, 'VARRECENUM3'+#0);
  VarVARRECENUM3_102.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECENUM3)])); // $03, $00, $00, $00, $00
  VarVARRECENUM3_102.AddRef(DW_AT_type, DW_FORM_ref4, @Type_811); // $3A, $27, $00, $00

  VarVARRECENUMX_103 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECENUMX_103.Tag := DW_TAG_variable;
  VarVARRECENUMX_103.Children := 0;
  VarVARRECENUMX_103.Add(DW_AT_name, DW_FORM_string, 'VARRECENUMX'+#0);
  VarVARRECENUMX_103.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECENUMX)])); // $03, $00, $00, $00, $00
  VarVARRECENUMX_103.AddRef(DW_AT_type, DW_FORM_ref4, @Type_814); // $53, $27, $00, $00

  VarVARRECENUMR3_104 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECENUMR3_104.Tag := DW_TAG_variable;
  VarVARRECENUMR3_104.Children := 0;
  VarVARRECENUMR3_104.Add(DW_AT_name, DW_FORM_string, 'VARRECENUMR3'+#0);
  VarVARRECENUMR3_104.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECENUMR3)])); // $03, $00, $00, $00, $00
  VarVARRECENUMR3_104.AddRef(DW_AT_type, DW_FORM_ref4, @Type_817); // $6C, $27, $00, $00

  VarVARRECPENUM0_105 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPENUM0_105.Tag := DW_TAG_variable;
  VarVARRECPENUM0_105.Children := 0;
  VarVARRECPENUM0_105.Add(DW_AT_name, DW_FORM_string, 'VARRECPENUM0'+#0);
  VarVARRECPENUM0_105.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPENUM0)])); // $03, $00, $00, $00, $00
  VarVARRECPENUM0_105.AddRef(DW_AT_type, DW_FORM_ref4, @Type_820); // $86, $27, $00, $00

  VarVARRECPENUM1_106 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPENUM1_106.Tag := DW_TAG_variable;
  VarVARRECPENUM1_106.Children := 0;
  VarVARRECPENUM1_106.Add(DW_AT_name, DW_FORM_string, 'VARRECPENUM1'+#0);
  VarVARRECPENUM1_106.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPENUM1)])); // $03, $00, $00, $00, $00
  VarVARRECPENUM1_106.AddRef(DW_AT_type, DW_FORM_ref4, @Type_823); // $A0, $27, $00, $00

  VarVARRECPENUM2_107 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPENUM2_107.Tag := DW_TAG_variable;
  VarVARRECPENUM2_107.Children := 0;
  VarVARRECPENUM2_107.Add(DW_AT_name, DW_FORM_string, 'VARRECPENUM2'+#0);
  VarVARRECPENUM2_107.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPENUM2)])); // $03, $00, $00, $00, $00
  VarVARRECPENUM2_107.AddRef(DW_AT_type, DW_FORM_ref4, @Type_826); // $BA, $27, $00, $00

  VarVARRECPENUM3_108 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPENUM3_108.Tag := DW_TAG_variable;
  VarVARRECPENUM3_108.Children := 0;
  VarVARRECPENUM3_108.Add(DW_AT_name, DW_FORM_string, 'VARRECPENUM3'+#0);
  VarVARRECPENUM3_108.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPENUM3)])); // $03, $00, $00, $00, $00
  VarVARRECPENUM3_108.AddRef(DW_AT_type, DW_FORM_ref4, @Type_829); // $D4, $27, $00, $00

  VarVARRECPENUMX_109 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPENUMX_109.Tag := DW_TAG_variable;
  VarVARRECPENUMX_109.Children := 0;
  VarVARRECPENUMX_109.Add(DW_AT_name, DW_FORM_string, 'VARRECPENUMX'+#0);
  VarVARRECPENUMX_109.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPENUMX)])); // $03, $00, $00, $00, $00
  VarVARRECPENUMX_109.AddRef(DW_AT_type, DW_FORM_ref4, @Type_832); // $EE, $27, $00, $00

  VarVARRECPENUMR3_110 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPENUMR3_110.Tag := DW_TAG_variable;
  VarVARRECPENUMR3_110.Children := 0;
  VarVARRECPENUMR3_110.Add(DW_AT_name, DW_FORM_string, 'VARRECPENUMR3'+#0);
  VarVARRECPENUMR3_110.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPENUMR3)])); // $03, $00, $00, $00, $00
  VarVARRECPENUMR3_110.AddRef(DW_AT_type, DW_FORM_ref4, @Type_835); // $08, $28, $00, $00

  VarVARRECSET0_111 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSET0_111.Tag := DW_TAG_variable;
  VarVARRECSET0_111.Children := 0;
  VarVARRECSET0_111.Add(DW_AT_name, DW_FORM_string, 'VARRECSET0'+#0);
  VarVARRECSET0_111.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSET0)])); // $03, $00, $00, $00, $00
  VarVARRECSET0_111.AddRef(DW_AT_type, DW_FORM_ref4, @Type_838); // $23, $28, $00, $00

  VarVARRECSET1_112 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSET1_112.Tag := DW_TAG_variable;
  VarVARRECSET1_112.Children := 0;
  VarVARRECSET1_112.Add(DW_AT_name, DW_FORM_string, 'VARRECSET1'+#0);
  VarVARRECSET1_112.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSET1)])); // $03, $00, $00, $00, $00
  VarVARRECSET1_112.AddRef(DW_AT_type, DW_FORM_ref4, @Type_841); // $3B, $28, $00, $00

  VarVARRECSET2_113 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSET2_113.Tag := DW_TAG_variable;
  VarVARRECSET2_113.Children := 0;
  VarVARRECSET2_113.Add(DW_AT_name, DW_FORM_string, 'VARRECSET2'+#0);
  VarVARRECSET2_113.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSET2)])); // $03, $00, $00, $00, $00
  VarVARRECSET2_113.AddRef(DW_AT_type, DW_FORM_ref4, @Type_844); // $53, $28, $00, $00

  VarVARRECSET3_114 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSET3_114.Tag := DW_TAG_variable;
  VarVARRECSET3_114.Children := 0;
  VarVARRECSET3_114.Add(DW_AT_name, DW_FORM_string, 'VARRECSET3'+#0);
  VarVARRECSET3_114.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSET3)])); // $03, $00, $00, $00, $00
  VarVARRECSET3_114.AddRef(DW_AT_type, DW_FORM_ref4, @Type_847); // $6B, $28, $00, $00

  VarVARRECSETX1_115 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSETX1_115.Tag := DW_TAG_variable;
  VarVARRECSETX1_115.Children := 0;
  VarVARRECSETX1_115.Add(DW_AT_name, DW_FORM_string, 'VARRECSETX1'+#0);
  VarVARRECSETX1_115.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSETX1)])); // $03, $00, $00, $00, $00
  VarVARRECSETX1_115.AddRef(DW_AT_type, DW_FORM_ref4, @Type_850); // $83, $28, $00, $00

  VarVARRECSETX2_116 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSETX2_116.Tag := DW_TAG_variable;
  VarVARRECSETX2_116.Children := 0;
  VarVARRECSETX2_116.Add(DW_AT_name, DW_FORM_string, 'VARRECSETX2'+#0);
  VarVARRECSETX2_116.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSETX2)])); // $03, $00, $00, $00, $00
  VarVARRECSETX2_116.AddRef(DW_AT_type, DW_FORM_ref4, @Type_853); // $9C, $28, $00, $00

  VarVARRECSETB1_117 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSETB1_117.Tag := DW_TAG_variable;
  VarVARRECSETB1_117.Children := 0;
  VarVARRECSETB1_117.Add(DW_AT_name, DW_FORM_string, 'VARRECSETB1'+#0);
  VarVARRECSETB1_117.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSETB1)])); // $03, $00, $00, $00, $00
  VarVARRECSETB1_117.AddRef(DW_AT_type, DW_FORM_ref4, @Type_856); // $B5, $28, $00, $00

  VarVARRECSETB2_118 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSETB2_118.Tag := DW_TAG_variable;
  VarVARRECSETB2_118.Children := 0;
  VarVARRECSETB2_118.Add(DW_AT_name, DW_FORM_string, 'VARRECSETB2'+#0);
  VarVARRECSETB2_118.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSETB2)])); // $03, $00, $00, $00, $00
  VarVARRECSETB2_118.AddRef(DW_AT_type, DW_FORM_ref4, @Type_859); // $CE, $28, $00, $00

  VarVARRECSETC1_119 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSETC1_119.Tag := DW_TAG_variable;
  VarVARRECSETC1_119.Children := 0;
  VarVARRECSETC1_119.Add(DW_AT_name, DW_FORM_string, 'VARRECSETC1'+#0);
  VarVARRECSETC1_119.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSETC1)])); // $03, $00, $00, $00, $00
  VarVARRECSETC1_119.AddRef(DW_AT_type, DW_FORM_ref4, @Type_862); // $E7, $28, $00, $00

  VarVARRECSETC2_120 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSETC2_120.Tag := DW_TAG_variable;
  VarVARRECSETC2_120.Children := 0;
  VarVARRECSETC2_120.Add(DW_AT_name, DW_FORM_string, 'VARRECSETC2'+#0);
  VarVARRECSETC2_120.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSETC2)])); // $03, $00, $00, $00, $00
  VarVARRECSETC2_120.AddRef(DW_AT_type, DW_FORM_ref4, @Type_865); // $00, $29, $00, $00

  VarVARRECSETR3_121 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECSETR3_121.Tag := DW_TAG_variable;
  VarVARRECSETR3_121.Children := 0;
  VarVARRECSETR3_121.Add(DW_AT_name, DW_FORM_string, 'VARRECSETR3'+#0);
  VarVARRECSETR3_121.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECSETR3)])); // $03, $00, $00, $00, $00
  VarVARRECSETR3_121.AddRef(DW_AT_type, DW_FORM_ref4, @Type_868); // $19, $29, $00, $00

  VarVARRECPSET0_122 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSET0_122.Tag := DW_TAG_variable;
  VarVARRECPSET0_122.Children := 0;
  VarVARRECPSET0_122.Add(DW_AT_name, DW_FORM_string, 'VARRECPSET0'+#0);
  VarVARRECPSET0_122.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSET0)])); // $03, $00, $00, $00, $00
  VarVARRECPSET0_122.AddRef(DW_AT_type, DW_FORM_ref4, @Type_871); // $32, $29, $00, $00

  VarVARRECPSET1_123 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSET1_123.Tag := DW_TAG_variable;
  VarVARRECPSET1_123.Children := 0;
  VarVARRECPSET1_123.Add(DW_AT_name, DW_FORM_string, 'VARRECPSET1'+#0);
  VarVARRECPSET1_123.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSET1)])); // $03, $00, $00, $00, $00
  VarVARRECPSET1_123.AddRef(DW_AT_type, DW_FORM_ref4, @Type_874); // $4B, $29, $00, $00

  VarVARRECPSET2_124 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSET2_124.Tag := DW_TAG_variable;
  VarVARRECPSET2_124.Children := 0;
  VarVARRECPSET2_124.Add(DW_AT_name, DW_FORM_string, 'VARRECPSET2'+#0);
  VarVARRECPSET2_124.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSET2)])); // $03, $00, $00, $00, $00
  VarVARRECPSET2_124.AddRef(DW_AT_type, DW_FORM_ref4, @Type_877); // $64, $29, $00, $00

  VarVARRECPSET3_125 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSET3_125.Tag := DW_TAG_variable;
  VarVARRECPSET3_125.Children := 0;
  VarVARRECPSET3_125.Add(DW_AT_name, DW_FORM_string, 'VARRECPSET3'+#0);
  VarVARRECPSET3_125.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSET3)])); // $03, $00, $00, $00, $00
  VarVARRECPSET3_125.AddRef(DW_AT_type, DW_FORM_ref4, @Type_880); // $7D, $29, $00, $00

  VarVARRECPSETX1_126 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSETX1_126.Tag := DW_TAG_variable;
  VarVARRECPSETX1_126.Children := 0;
  VarVARRECPSETX1_126.Add(DW_AT_name, DW_FORM_string, 'VARRECPSETX1'+#0);
  VarVARRECPSETX1_126.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSETX1)])); // $03, $00, $00, $00, $00
  VarVARRECPSETX1_126.AddRef(DW_AT_type, DW_FORM_ref4, @Type_883); // $96, $29, $00, $00

  VarVARRECPSETB1_127 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSETB1_127.Tag := DW_TAG_variable;
  VarVARRECPSETB1_127.Children := 0;
  VarVARRECPSETB1_127.Add(DW_AT_name, DW_FORM_string, 'VARRECPSETB1'+#0);
  VarVARRECPSETB1_127.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSETB1)])); // $03, $00, $00, $00, $00
  VarVARRECPSETB1_127.AddRef(DW_AT_type, DW_FORM_ref4, @Type_886); // $B0, $29, $00, $00

  VarVARRECPSETB2_128 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSETB2_128.Tag := DW_TAG_variable;
  VarVARRECPSETB2_128.Children := 0;
  VarVARRECPSETB2_128.Add(DW_AT_name, DW_FORM_string, 'VARRECPSETB2'+#0);
  VarVARRECPSETB2_128.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSETB2)])); // $03, $00, $00, $00, $00
  VarVARRECPSETB2_128.AddRef(DW_AT_type, DW_FORM_ref4, @Type_889); // $CA, $29, $00, $00

  VarVARRECPSETC1_129 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSETC1_129.Tag := DW_TAG_variable;
  VarVARRECPSETC1_129.Children := 0;
  VarVARRECPSETC1_129.Add(DW_AT_name, DW_FORM_string, 'VARRECPSETC1'+#0);
  VarVARRECPSETC1_129.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSETC1)])); // $03, $00, $00, $00, $00
  VarVARRECPSETC1_129.AddRef(DW_AT_type, DW_FORM_ref4, @Type_892); // $E4, $29, $00, $00

  VarVARRECPSETR3_130 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSETR3_130.Tag := DW_TAG_variable;
  VarVARRECPSETR3_130.Children := 0;
  VarVARRECPSETR3_130.Add(DW_AT_name, DW_FORM_string, 'VARRECPSETR3'+#0);
  VarVARRECPSETR3_130.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSETR3)])); // $03, $00, $00, $00, $00
  VarVARRECPSETR3_130.AddRef(DW_AT_type, DW_FORM_ref4, @Type_895); // $FE, $29, $00, $00

  VarVARRECPSETP0_131 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSETP0_131.Tag := DW_TAG_variable;
  VarVARRECPSETP0_131.Children := 0;
  VarVARRECPSETP0_131.Add(DW_AT_name, DW_FORM_string, 'VARRECPSETP0'+#0);
  VarVARRECPSETP0_131.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSETP0)])); // $03, $00, $00, $00, $00
  VarVARRECPSETP0_131.AddRef(DW_AT_type, DW_FORM_ref4, @Type_898); // $18, $2A, $00, $00

  VarVARRECPSETP1_132 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSETP1_132.Tag := DW_TAG_variable;
  VarVARRECPSETP1_132.Children := 0;
  VarVARRECPSETP1_132.Add(DW_AT_name, DW_FORM_string, 'VARRECPSETP1'+#0);
  VarVARRECPSETP1_132.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSETP1)])); // $03, $00, $00, $00, $00
  VarVARRECPSETP1_132.AddRef(DW_AT_type, DW_FORM_ref4, @Type_901); // $32, $2A, $00, $00

  VarVARRECPSETP2_133 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSETP2_133.Tag := DW_TAG_variable;
  VarVARRECPSETP2_133.Children := 0;
  VarVARRECPSETP2_133.Add(DW_AT_name, DW_FORM_string, 'VARRECPSETP2'+#0);
  VarVARRECPSETP2_133.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSETP2)])); // $03, $00, $00, $00, $00
  VarVARRECPSETP2_133.AddRef(DW_AT_type, DW_FORM_ref4, @Type_904); // $4C, $2A, $00, $00

  VarVARRECPSETP3_134 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSETP3_134.Tag := DW_TAG_variable;
  VarVARRECPSETP3_134.Children := 0;
  VarVARRECPSETP3_134.Add(DW_AT_name, DW_FORM_string, 'VARRECPSETP3'+#0);
  VarVARRECPSETP3_134.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSETP3)])); // $03, $00, $00, $00, $00
  VarVARRECPSETP3_134.AddRef(DW_AT_type, DW_FORM_ref4, @Type_907); // $66, $2A, $00, $00

  VarVARRECPSETPX1_135 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSETPX1_135.Tag := DW_TAG_variable;
  VarVARRECPSETPX1_135.Children := 0;
  VarVARRECPSETPX1_135.Add(DW_AT_name, DW_FORM_string, 'VARRECPSETPX1'+#0);
  VarVARRECPSETPX1_135.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSETPX1)])); // $03, $00, $00, $00, $00
  VarVARRECPSETPX1_135.AddRef(DW_AT_type, DW_FORM_ref4, @Type_910); // $80, $2A, $00, $00

  VarVARRECPSETPB1_136 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSETPB1_136.Tag := DW_TAG_variable;
  VarVARRECPSETPB1_136.Children := 0;
  VarVARRECPSETPB1_136.Add(DW_AT_name, DW_FORM_string, 'VARRECPSETPB1'+#0);
  VarVARRECPSETPB1_136.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSETPB1)])); // $03, $00, $00, $00, $00
  VarVARRECPSETPB1_136.AddRef(DW_AT_type, DW_FORM_ref4, @Type_913); // $9B, $2A, $00, $00

  VarVARRECPSETPB2_137 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSETPB2_137.Tag := DW_TAG_variable;
  VarVARRECPSETPB2_137.Children := 0;
  VarVARRECPSETPB2_137.Add(DW_AT_name, DW_FORM_string, 'VARRECPSETPB2'+#0);
  VarVARRECPSETPB2_137.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSETPB2)])); // $03, $00, $00, $00, $00
  VarVARRECPSETPB2_137.AddRef(DW_AT_type, DW_FORM_ref4, @Type_916); // $B6, $2A, $00, $00

  VarVARRECPSETPC1_138 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSETPC1_138.Tag := DW_TAG_variable;
  VarVARRECPSETPC1_138.Children := 0;
  VarVARRECPSETPC1_138.Add(DW_AT_name, DW_FORM_string, 'VARRECPSETPC1'+#0);
  VarVARRECPSETPC1_138.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSETPC1)])); // $03, $00, $00, $00, $00
  VarVARRECPSETPC1_138.AddRef(DW_AT_type, DW_FORM_ref4, @Type_919); // $D1, $2A, $00, $00

  VarVARRECPSETPR3_139 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  VarVARRECPSETPR3_139.Tag := DW_TAG_variable;
  VarVARRECPSETPR3_139.Children := 0;
  VarVARRECPSETPR3_139.Add(DW_AT_name, DW_FORM_string, 'VARRECPSETPR3'+#0);
  VarVARRECPSETPR3_139.Add(DW_AT_location, DW_FORM_block1, BytesLen1([DW_OP_addr, AddrB(@GlobalVar.VarRECPSETPR3)])); // $03, $00, $00, $00, $00
  VarVARRECPSETPR3_139.AddRef(DW_AT_type, DW_FORM_ref4, @Type_922); // $EC, $2A, $00, $00

  Progmain_140 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Progmain_140.Tag := DW_TAG_subprogram;
  Progmain_140.Children := 0;
  Progmain_140.Add(DW_AT_name, DW_FORM_string, 'main'+#0);
  Progmain_140.Add(DW_AT_prototyped, DW_FORM_flag, [$01]);
  Progmain_140.Add(DW_AT_calling_convention, DW_FORM_data1, [$41]);
  Progmain_140.Add(DW_AT_external, DW_FORM_flag, [$01]);
  Progmain_140.AddAddr(DW_AT_low_pc, DW_FORM_addr, $00400000);
  Progmain_140.AddAddr(DW_AT_high_pc, DW_FORM_addr, $00400FFF);

  TypeDeclBYTE_141 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclBYTE_141.Tag := DW_TAG_typedef;
  TypeDeclBYTE_141.Children := 0;
  TypeDeclBYTE_141.Add(DW_AT_name, DW_FORM_string, 'BYTE'+#0);
  TypeDeclBYTE_141.AddRef(DW_AT_type, DW_FORM_ref4, @TypeBYTE_142); // $74, $0C, $00, $00

  TypeBYTE_142 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeBYTE_142.Tag := DW_TAG_base_type;
  TypeBYTE_142.Children := 0;
  TypeBYTE_142.Add(DW_AT_name, DW_FORM_string, 'BYTE'+#0);
  TypeBYTE_142.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeBYTE_142.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_143 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_143.Tag := DW_TAG_reference_type;
  Type_143.Children := 0;
  Type_143.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_141); // $6A, $0C, $00, $00

  TypeDeclSHORTINT_144 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclSHORTINT_144.Tag := DW_TAG_typedef;
  TypeDeclSHORTINT_144.Children := 0;
  TypeDeclSHORTINT_144.Add(DW_AT_name, DW_FORM_string, 'SHORTINT'+#0);
  TypeDeclSHORTINT_144.AddRef(DW_AT_type, DW_FORM_ref4, @TypeSHORTINT_145); // $8F, $0C, $00, $00

  TypeSHORTINT_145 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeSHORTINT_145.Tag := DW_TAG_base_type;
  TypeSHORTINT_145.Children := 0;
  TypeSHORTINT_145.Add(DW_AT_name, DW_FORM_string, 'SHORTINT'+#0);
  TypeSHORTINT_145.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeSHORTINT_145.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_146 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_146.Tag := DW_TAG_reference_type;
  Type_146.Children := 0;
  Type_146.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_144); // $81, $0C, $00, $00

  TypeDeclWORD_147 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclWORD_147.Tag := DW_TAG_typedef;
  TypeDeclWORD_147.Children := 0;
  TypeDeclWORD_147.Add(DW_AT_name, DW_FORM_string, 'WORD'+#0);
  TypeDeclWORD_147.AddRef(DW_AT_type, DW_FORM_ref4, @TypeWORD_148); // $AA, $0C, $00, $00

  TypeWORD_148 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeWORD_148.Tag := DW_TAG_base_type;
  TypeWORD_148.Children := 0;
  TypeWORD_148.Add(DW_AT_name, DW_FORM_string, 'WORD'+#0);
  TypeWORD_148.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeWORD_148.Add(DW_AT_byte_size, DW_FORM_data1, [$02]);

  Type_149 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_149.Tag := DW_TAG_reference_type;
  Type_149.Children := 0;
  Type_149.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_147); // $A0, $0C, $00, $00

  TypeDeclSMALLINT_150 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclSMALLINT_150.Tag := DW_TAG_typedef;
  TypeDeclSMALLINT_150.Children := 0;
  TypeDeclSMALLINT_150.Add(DW_AT_name, DW_FORM_string, 'SMALLINT'+#0);
  TypeDeclSMALLINT_150.AddRef(DW_AT_type, DW_FORM_ref4, @TypeSMALLINT_151); // $C5, $0C, $00, $00

  TypeSMALLINT_151 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeSMALLINT_151.Tag := DW_TAG_base_type;
  TypeSMALLINT_151.Children := 0;
  TypeSMALLINT_151.Add(DW_AT_name, DW_FORM_string, 'SMALLINT'+#0);
  TypeSMALLINT_151.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeSMALLINT_151.Add(DW_AT_byte_size, DW_FORM_data1, [$02]);

  Type_152 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_152.Tag := DW_TAG_reference_type;
  Type_152.Children := 0;
  Type_152.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSMALLINT_150); // $B7, $0C, $00, $00

  TypeDeclLONGWORD_153 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclLONGWORD_153.Tag := DW_TAG_typedef;
  TypeDeclLONGWORD_153.Children := 0;
  TypeDeclLONGWORD_153.Add(DW_AT_name, DW_FORM_string, 'LONGWORD'+#0);
  TypeDeclLONGWORD_153.AddRef(DW_AT_type, DW_FORM_ref4, @TypeLONGWORD_154); // $E4, $0C, $00, $00

  TypeLONGWORD_154 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeLONGWORD_154.Tag := DW_TAG_base_type;
  TypeLONGWORD_154.Children := 0;
  TypeLONGWORD_154.Add(DW_AT_name, DW_FORM_string, 'LONGWORD'+#0);
  TypeLONGWORD_154.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeLONGWORD_154.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

  Type_155 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_155.Tag := DW_TAG_reference_type;
  Type_155.Children := 0;
  Type_155.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_153); // $D6, $0C, $00, $00

  TypeDeclLONGINT_156 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclLONGINT_156.Tag := DW_TAG_typedef;
  TypeDeclLONGINT_156.Children := 0;
  TypeDeclLONGINT_156.Add(DW_AT_name, DW_FORM_string, 'LONGINT'+#0);
  TypeDeclLONGINT_156.AddRef(DW_AT_type, DW_FORM_ref4, @TypeLONGINT_157); // $02, $0D, $00, $00

  TypeLONGINT_157 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeLONGINT_157.Tag := DW_TAG_base_type;
  TypeLONGINT_157.Children := 0;
  TypeLONGINT_157.Add(DW_AT_name, DW_FORM_string, 'LONGINT'+#0);
  TypeLONGINT_157.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeLONGINT_157.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

  Type_158 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_158.Tag := DW_TAG_reference_type;
  Type_158.Children := 0;
  Type_158.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_156); // $F5, $0C, $00, $00

  TypeDeclQWORD_159 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclQWORD_159.Tag := DW_TAG_typedef;
  TypeDeclQWORD_159.Children := 0;
  TypeDeclQWORD_159.Add(DW_AT_name, DW_FORM_string, 'QWORD'+#0);
  TypeDeclQWORD_159.AddRef(DW_AT_type, DW_FORM_ref4, @TypeQWord_160); // $1D, $0D, $00, $00

  TypeQWord_160 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeQWord_160.Tag := DW_TAG_base_type;
  TypeQWord_160.Children := 0;
  TypeQWord_160.Add(DW_AT_name, DW_FORM_string, 'QWord'+#0);
  TypeQWord_160.Add(DW_AT_encoding, DW_FORM_data1, [$07]);
  TypeQWord_160.Add(DW_AT_byte_size, DW_FORM_data1, [$08]);

  Type_161 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_161.Tag := DW_TAG_reference_type;
  Type_161.Children := 0;
  Type_161.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_159); // $12, $0D, $00, $00

  TypeDeclINT64_162 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclINT64_162.Tag := DW_TAG_typedef;
  TypeDeclINT64_162.Children := 0;
  TypeDeclINT64_162.Add(DW_AT_name, DW_FORM_string, 'INT64'+#0);
  TypeDeclINT64_162.AddRef(DW_AT_type, DW_FORM_ref4, @TypeInt64_163); // $36, $0D, $00, $00

  TypeInt64_163 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeInt64_163.Tag := DW_TAG_base_type;
  TypeInt64_163.Children := 0;
  TypeInt64_163.Add(DW_AT_name, DW_FORM_string, 'Int64'+#0);
  TypeInt64_163.Add(DW_AT_encoding, DW_FORM_data1, [$05]);
  TypeInt64_163.Add(DW_AT_byte_size, DW_FORM_data1, [$08]);

  Type_164 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_164.Tag := DW_TAG_reference_type;
  Type_164.Children := 0;
  Type_164.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclINT64_162); // $2B, $0D, $00, $00

  TypeDeclBOOLEAN_165 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclBOOLEAN_165.Tag := DW_TAG_typedef;
  TypeDeclBOOLEAN_165.Children := 0;
  TypeDeclBOOLEAN_165.Add(DW_AT_name, DW_FORM_string, 'BOOLEAN'+#0);
  TypeDeclBOOLEAN_165.AddRef(DW_AT_type, DW_FORM_ref4, @TypeBoolean_166); // $51, $0D, $00, $00

  TypeBoolean_166 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeBoolean_166.Tag := DW_TAG_base_type;
  TypeBoolean_166.Children := 0;
  TypeBoolean_166.Add(DW_AT_name, DW_FORM_string, 'Boolean'+#0);
  TypeBoolean_166.Add(DW_AT_encoding, DW_FORM_data1, [$02]);
  TypeBoolean_166.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_167 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_167.Tag := DW_TAG_reference_type;
  Type_167.Children := 0;
  Type_167.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_165); // $44, $0D, $00, $00

  TypeDeclPBYTE_168 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPBYTE_168.Tag := DW_TAG_typedef;
  TypeDeclPBYTE_168.Children := 0;
  TypeDeclPBYTE_168.Add(DW_AT_name, DW_FORM_string, 'PBYTE'+#0);
  TypeDeclPBYTE_168.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_169); // $6C, $0D, $00, $00

  TypePtr_169 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_169.Tag := DW_TAG_pointer_type;
  TypePtr_169.Children := 0;
  TypePtr_169.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_141); // $6A, $0C, $00, $00

  Type_170 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_170.Tag := DW_TAG_reference_type;
  Type_170.Children := 0;
  Type_170.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPBYTE_168); // $61, $0D, $00, $00

  TypeDeclPWORD_171 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPWORD_171.Tag := DW_TAG_typedef;
  TypeDeclPWORD_171.Children := 0;
  TypeDeclPWORD_171.Add(DW_AT_name, DW_FORM_string, 'PWORD'+#0);
  TypeDeclPWORD_171.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_172); // $81, $0D, $00, $00

  TypePtr_172 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_172.Tag := DW_TAG_pointer_type;
  TypePtr_172.Children := 0;
  TypePtr_172.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_147); // $A0, $0C, $00, $00

  Type_173 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_173.Tag := DW_TAG_reference_type;
  Type_173.Children := 0;
  Type_173.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPWORD_171); // $76, $0D, $00, $00

  TypeDeclPLONGWORD_174 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPLONGWORD_174.Tag := DW_TAG_typedef;
  TypeDeclPLONGWORD_174.Children := 0;
  TypeDeclPLONGWORD_174.Add(DW_AT_name, DW_FORM_string, 'PLONGWORD'+#0);
  TypeDeclPLONGWORD_174.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_175); // $9A, $0D, $00, $00

  TypePtr_175 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_175.Tag := DW_TAG_pointer_type;
  TypePtr_175.Children := 0;
  TypePtr_175.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_153); // $D6, $0C, $00, $00

  Type_176 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_176.Tag := DW_TAG_reference_type;
  Type_176.Children := 0;
  Type_176.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPLONGWORD_174); // $8B, $0D, $00, $00

  TypeDeclPQWORD_177 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPQWORD_177.Tag := DW_TAG_typedef;
  TypeDeclPQWORD_177.Children := 0;
  TypeDeclPQWORD_177.Add(DW_AT_name, DW_FORM_string, 'PQWORD'+#0);
  TypeDeclPQWORD_177.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_178); // $B0, $0D, $00, $00

  TypePtr_178 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_178.Tag := DW_TAG_pointer_type;
  TypePtr_178.Children := 0;
  TypePtr_178.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_159); // $12, $0D, $00, $00

  Type_179 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_179.Tag := DW_TAG_reference_type;
  Type_179.Children := 0;
  Type_179.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPQWORD_177); // $A4, $0D, $00, $00

  TypeDeclPSHORTINT_180 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSHORTINT_180.Tag := DW_TAG_typedef;
  TypeDeclPSHORTINT_180.Children := 0;
  TypeDeclPSHORTINT_180.Add(DW_AT_name, DW_FORM_string, 'PSHORTINT'+#0);
  TypeDeclPSHORTINT_180.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_181); // $C9, $0D, $00, $00

  TypePtr_181 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_181.Tag := DW_TAG_pointer_type;
  TypePtr_181.Children := 0;
  TypePtr_181.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_144); // $81, $0C, $00, $00

  Type_182 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_182.Tag := DW_TAG_reference_type;
  Type_182.Children := 0;
  Type_182.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSHORTINT_180); // $BA, $0D, $00, $00

  TypeDeclPSMALLINT_183 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSMALLINT_183.Tag := DW_TAG_typedef;
  TypeDeclPSMALLINT_183.Children := 0;
  TypeDeclPSMALLINT_183.Add(DW_AT_name, DW_FORM_string, 'PSMALLINT'+#0);
  TypeDeclPSMALLINT_183.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_184); // $E2, $0D, $00, $00

  TypePtr_184 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_184.Tag := DW_TAG_pointer_type;
  TypePtr_184.Children := 0;
  TypePtr_184.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSMALLINT_150); // $B7, $0C, $00, $00

  Type_185 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_185.Tag := DW_TAG_reference_type;
  Type_185.Children := 0;
  Type_185.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSMALLINT_183); // $D3, $0D, $00, $00

  TypeDeclPINTEGER_186 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPINTEGER_186.Tag := DW_TAG_typedef;
  TypeDeclPINTEGER_186.Children := 0;
  TypeDeclPINTEGER_186.Add(DW_AT_name, DW_FORM_string, 'PINTEGER'+#0);
  TypeDeclPINTEGER_186.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_187); // $FA, $0D, $00, $00

  TypePtr_187 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_187.Tag := DW_TAG_pointer_type;
  TypePtr_187.Children := 0;
  TypePtr_187.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_156); // $F5, $0C, $00, $00

  Type_188 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_188.Tag := DW_TAG_reference_type;
  Type_188.Children := 0;
  Type_188.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTEGER_186); // $EC, $0D, $00, $00

  TypeDeclPINT64_189 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPINT64_189.Tag := DW_TAG_typedef;
  TypeDeclPINT64_189.Children := 0;
  TypeDeclPINT64_189.Add(DW_AT_name, DW_FORM_string, 'PINT64'+#0);
  TypeDeclPINT64_189.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_190); // $10, $0E, $00, $00

  TypePtr_190 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_190.Tag := DW_TAG_pointer_type;
  TypePtr_190.Children := 0;
  TypePtr_190.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclINT64_162); // $2B, $0D, $00, $00

  Type_191 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_191.Tag := DW_TAG_reference_type;
  Type_191.Children := 0;
  Type_191.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINT64_189); // $04, $0E, $00, $00

  TypeDeclTSUB1_192 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSUB1_192.Tag := DW_TAG_typedef;
  TypeDeclTSUB1_192.Children := 0;
  TypeDeclTSUB1_192.Add(DW_AT_name, DW_FORM_string, 'TSUB1'+#0);
  TypeDeclTSUB1_192.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSUB1_193); // $25, $0E, $00, $00

  TypeTSUB1_193 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSUB1_193.Tag := DW_TAG_subrange_type;
  TypeTSUB1_193.Children := 0;
  TypeTSUB1_193.Add(DW_AT_name, DW_FORM_string, 'TSUB1'+#0);
  TypeTSUB1_193.AddULEB(DW_AT_lower_bound, DW_FORM_udata, 1);
  TypeTSUB1_193.AddULEB(DW_AT_upper_bound, DW_FORM_udata, 9);
  TypeTSUB1_193.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_141); // $6A, $0C, $00, $00

  Type_194 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_194.Tag := DW_TAG_reference_type;
  Type_194.Children := 0;
  Type_194.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB1_192); // $1A, $0E, $00, $00

  TypeDeclTSUB2_195 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSUB2_195.Tag := DW_TAG_typedef;
  TypeDeclTSUB2_195.Children := 0;
  TypeDeclTSUB2_195.Add(DW_AT_name, DW_FORM_string, 'TSUB2'+#0);
  TypeDeclTSUB2_195.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSUB2_196); // $42, $0E, $00, $00

  TypeTSUB2_196 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSUB2_196.Tag := DW_TAG_subrange_type;
  TypeTSUB2_196.Children := 0;
  TypeTSUB2_196.Add(DW_AT_name, DW_FORM_string, 'TSUB2'+#0);
  TypeTSUB2_196.AddULEB(DW_AT_lower_bound, DW_FORM_udata, 1000);
  TypeTSUB2_196.AddULEB(DW_AT_upper_bound, DW_FORM_udata, 90000);
  TypeTSUB2_196.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_153); // $D6, $0C, $00, $00

  Type_197 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_197.Tag := DW_TAG_reference_type;
  Type_197.Children := 0;
  Type_197.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB2_195); // $37, $0E, $00, $00

  TypeDeclTSUB3_198 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSUB3_198.Tag := DW_TAG_typedef;
  TypeDeclTSUB3_198.Children := 0;
  TypeDeclTSUB3_198.Add(DW_AT_name, DW_FORM_string, 'TSUB3'+#0);
  TypeDeclTSUB3_198.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSUB3_199); // $62, $0E, $00, $00

  TypeTSUB3_199 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSUB3_199.Tag := DW_TAG_subrange_type;
  TypeTSUB3_199.Children := 0;
  TypeTSUB3_199.Add(DW_AT_name, DW_FORM_string, 'TSUB3'+#0);
  TypeTSUB3_199.AddULEB(DW_AT_lower_bound, DW_FORM_udata, 10);
  TypeTSUB3_199.AddULEB(DW_AT_upper_bound, DW_FORM_udata, 250);
  TypeTSUB3_199.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_141); // $6A, $0C, $00, $00

  Type_200 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_200.Tag := DW_TAG_reference_type;
  Type_200.Children := 0;
  Type_200.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB3_198); // $57, $0E, $00, $00

  TypeDeclTSUB4_201 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSUB4_201.Tag := DW_TAG_typedef;
  TypeDeclTSUB4_201.Children := 0;
  TypeDeclTSUB4_201.Add(DW_AT_name, DW_FORM_string, 'TSUB4'+#0);
  TypeDeclTSUB4_201.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSUB4_202); // $80, $0E, $00, $00

  TypeTSUB4_202 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSUB4_202.Tag := DW_TAG_subrange_type;
  TypeTSUB4_202.Children := 0;
  TypeTSUB4_202.Add(DW_AT_name, DW_FORM_string, 'TSUB4'+#0);
  TypeTSUB4_202.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, -1);
  TypeTSUB4_202.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 9);
  TypeTSUB4_202.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_144); // $81, $0C, $00, $00

  Type_203 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_203.Tag := DW_TAG_reference_type;
  Type_203.Children := 0;
  Type_203.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB4_201); // $75, $0E, $00, $00

  TypeDeclTSUB5_204 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSUB5_204.Tag := DW_TAG_typedef;
  TypeDeclTSUB5_204.Children := 0;
  TypeDeclTSUB5_204.Add(DW_AT_name, DW_FORM_string, 'TSUB5'+#0);
  TypeDeclTSUB5_204.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSUB5_205); // $9D, $0E, $00, $00

  TypeTSUB5_205 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSUB5_205.Tag := DW_TAG_subrange_type;
  TypeTSUB5_205.Children := 0;
  TypeTSUB5_205.Add(DW_AT_name, DW_FORM_string, 'TSUB5'+#0);
  TypeTSUB5_205.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, -11);
  TypeTSUB5_205.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, -2);
  TypeTSUB5_205.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_144); // $81, $0C, $00, $00

  Type_206 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_206.Tag := DW_TAG_reference_type;
  Type_206.Children := 0;
  Type_206.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB5_204); // $92, $0E, $00, $00

  TypeDeclPSUB1_207 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSUB1_207.Tag := DW_TAG_typedef;
  TypeDeclPSUB1_207.Children := 0;
  TypeDeclPSUB1_207.Add(DW_AT_name, DW_FORM_string, 'PSUB1'+#0);
  TypeDeclPSUB1_207.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_208); // $BA, $0E, $00, $00

  TypePtr_208 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_208.Tag := DW_TAG_pointer_type;
  TypePtr_208.Children := 0;
  TypePtr_208.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB1_192); // $1A, $0E, $00, $00

  Type_209 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_209.Tag := DW_TAG_reference_type;
  Type_209.Children := 0;
  Type_209.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB1_207); // $AF, $0E, $00, $00

  TypeDeclPSUB2_210 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSUB2_210.Tag := DW_TAG_typedef;
  TypeDeclPSUB2_210.Children := 0;
  TypeDeclPSUB2_210.Add(DW_AT_name, DW_FORM_string, 'PSUB2'+#0);
  TypeDeclPSUB2_210.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_211); // $CF, $0E, $00, $00

  TypePtr_211 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_211.Tag := DW_TAG_pointer_type;
  TypePtr_211.Children := 0;
  TypePtr_211.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB2_195); // $37, $0E, $00, $00

  Type_212 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_212.Tag := DW_TAG_reference_type;
  Type_212.Children := 0;
  Type_212.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB2_210); // $C4, $0E, $00, $00

  TypeDeclPSUB3_213 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSUB3_213.Tag := DW_TAG_typedef;
  TypeDeclPSUB3_213.Children := 0;
  TypeDeclPSUB3_213.Add(DW_AT_name, DW_FORM_string, 'PSUB3'+#0);
  TypeDeclPSUB3_213.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_214); // $E4, $0E, $00, $00

  TypePtr_214 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_214.Tag := DW_TAG_pointer_type;
  TypePtr_214.Children := 0;
  TypePtr_214.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB3_198); // $57, $0E, $00, $00

  Type_215 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_215.Tag := DW_TAG_reference_type;
  Type_215.Children := 0;
  Type_215.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB3_213); // $D9, $0E, $00, $00

  TypeDeclPSUB4_216 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSUB4_216.Tag := DW_TAG_typedef;
  TypeDeclPSUB4_216.Children := 0;
  TypeDeclPSUB4_216.Add(DW_AT_name, DW_FORM_string, 'PSUB4'+#0);
  TypeDeclPSUB4_216.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_217); // $F9, $0E, $00, $00

  TypePtr_217 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_217.Tag := DW_TAG_pointer_type;
  TypePtr_217.Children := 0;
  TypePtr_217.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB4_201); // $75, $0E, $00, $00

  Type_218 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_218.Tag := DW_TAG_reference_type;
  Type_218.Children := 0;
  Type_218.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB4_216); // $EE, $0E, $00, $00

  TypeDeclPSUB5_219 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSUB5_219.Tag := DW_TAG_typedef;
  TypeDeclPSUB5_219.Children := 0;
  TypeDeclPSUB5_219.Add(DW_AT_name, DW_FORM_string, 'PSUB5'+#0);
  TypeDeclPSUB5_219.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_220); // $0E, $0F, $00, $00

  TypePtr_220 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_220.Tag := DW_TAG_pointer_type;
  TypePtr_220.Children := 0;
  TypePtr_220.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB5_204); // $92, $0E, $00, $00

  Type_221 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_221.Tag := DW_TAG_reference_type;
  Type_221.Children := 0;
  Type_221.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB5_219); // $03, $0F, $00, $00

  TypeDeclPBOOLEAN_222 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPBOOLEAN_222.Tag := DW_TAG_typedef;
  TypeDeclPBOOLEAN_222.Children := 0;
  TypeDeclPBOOLEAN_222.Add(DW_AT_name, DW_FORM_string, 'PBOOLEAN'+#0);
  TypeDeclPBOOLEAN_222.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_223); // $26, $0F, $00, $00

  TypePtr_223 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_223.Tag := DW_TAG_pointer_type;
  TypePtr_223.Children := 0;
  TypePtr_223.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_165); // $44, $0D, $00, $00

  Type_224 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_224.Tag := DW_TAG_reference_type;
  Type_224.Children := 0;
  Type_224.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPBOOLEAN_222); // $18, $0F, $00, $00

  TypeDeclTENUM0_225 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTENUM0_225.Tag := DW_TAG_typedef;
  TypeDeclTENUM0_225.Children := 0;
  TypeDeclTENUM0_225.Add(DW_AT_name, DW_FORM_string, 'TENUM0'+#0);
  TypeDeclTENUM0_225.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTENUM0_226); // $3C, $0F, $00, $00

  TypeTENUM0_226 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTENUM0_226.Tag := DW_TAG_enumeration_type;
  TypeTENUM0_226.Children := 1;
  TypeTENUM0_226.Add(DW_AT_name, DW_FORM_string, 'TENUM0'+#0);
  TypeTENUM0_226.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeE0A_227 := TypeTENUM0_226.GetNewChild;
    TypeE0A_227.Tag := DW_TAG_enumerator;
    TypeE0A_227.Children := 0;
    TypeE0A_227.Add(DW_AT_name, DW_FORM_string, 'E0A'+#0);
    TypeE0A_227.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

  Type_228 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_228.Tag := DW_TAG_reference_type;
  Type_228.Children := 0;
  Type_228.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM0_225); // $30, $0F, $00, $00

  TypeDeclTENUM1_229 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTENUM1_229.Tag := DW_TAG_typedef;
  TypeDeclTENUM1_229.Children := 0;
  TypeDeclTENUM1_229.Add(DW_AT_name, DW_FORM_string, 'TENUM1'+#0);
  TypeDeclTENUM1_229.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTENUM1_230); // $60, $0F, $00, $00

  TypeTENUM1_230 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTENUM1_230.Tag := DW_TAG_enumeration_type;
  TypeTENUM1_230.Children := 1;
  TypeTENUM1_230.Add(DW_AT_name, DW_FORM_string, 'TENUM1'+#0);
  TypeTENUM1_230.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeE1A_231 := TypeTENUM1_230.GetNewChild;
    TypeE1A_231.Tag := DW_TAG_enumerator;
    TypeE1A_231.Children := 0;
    TypeE1A_231.Add(DW_AT_name, DW_FORM_string, 'E1A'+#0);
    TypeE1A_231.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeE1B_232 := TypeTENUM1_230.GetNewChild;
    TypeE1B_232.Tag := DW_TAG_enumerator;
    TypeE1B_232.Children := 0;
    TypeE1B_232.Add(DW_AT_name, DW_FORM_string, 'E1B'+#0);
    TypeE1B_232.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeE1C_233 := TypeTENUM1_230.GetNewChild;
    TypeE1C_233.Tag := DW_TAG_enumerator;
    TypeE1C_233.Children := 0;
    TypeE1C_233.Add(DW_AT_name, DW_FORM_string, 'E1C'+#0);
    TypeE1C_233.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

  Type_234 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_234.Tag := DW_TAG_reference_type;
  Type_234.Children := 0;
  Type_234.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM1_229); // $54, $0F, $00, $00

  TypeDeclTENUM2_235 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTENUM2_235.Tag := DW_TAG_typedef;
  TypeDeclTENUM2_235.Children := 0;
  TypeDeclTENUM2_235.Add(DW_AT_name, DW_FORM_string, 'TENUM2'+#0);
  TypeDeclTENUM2_235.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTENUM2_236); // $96, $0F, $00, $00

  TypeTENUM2_236 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTENUM2_236.Tag := DW_TAG_enumeration_type;
  TypeTENUM2_236.Children := 1;
  TypeTENUM2_236.Add(DW_AT_name, DW_FORM_string, 'TENUM2'+#0);
  TypeTENUM2_236.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeE2A_237 := TypeTENUM2_236.GetNewChild;
    TypeE2A_237.Tag := DW_TAG_enumerator;
    TypeE2A_237.Children := 0;
    TypeE2A_237.Add(DW_AT_name, DW_FORM_string, 'E2A'+#0);
    TypeE2A_237.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeE2B_238 := TypeTENUM2_236.GetNewChild;
    TypeE2B_238.Tag := DW_TAG_enumerator;
    TypeE2B_238.Children := 0;
    TypeE2B_238.Add(DW_AT_name, DW_FORM_string, 'E2B'+#0);
    TypeE2B_238.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeE2C_239 := TypeTENUM2_236.GetNewChild;
    TypeE2C_239.Tag := DW_TAG_enumerator;
    TypeE2C_239.Children := 0;
    TypeE2C_239.Add(DW_AT_name, DW_FORM_string, 'E2C'+#0);
    TypeE2C_239.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

    TypeE2D_240 := TypeTENUM2_236.GetNewChild;
    TypeE2D_240.Tag := DW_TAG_enumerator;
    TypeE2D_240.Children := 0;
    TypeE2D_240.Add(DW_AT_name, DW_FORM_string, 'E2D'+#0);
    TypeE2D_240.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

    TypeE2E_241 := TypeTENUM2_236.GetNewChild;
    TypeE2E_241.Tag := DW_TAG_enumerator;
    TypeE2E_241.Children := 0;
    TypeE2E_241.Add(DW_AT_name, DW_FORM_string, 'E2E'+#0);
    TypeE2E_241.Add(DW_AT_const_value, DW_FORM_data4, [$04, $00, $00, $00]);

    TypeE2F_242 := TypeTENUM2_236.GetNewChild;
    TypeE2F_242.Tag := DW_TAG_enumerator;
    TypeE2F_242.Children := 0;
    TypeE2F_242.Add(DW_AT_name, DW_FORM_string, 'E2F'+#0);
    TypeE2F_242.Add(DW_AT_const_value, DW_FORM_data4, [$05, $00, $00, $00]);

    TypeE2G_243 := TypeTENUM2_236.GetNewChild;
    TypeE2G_243.Tag := DW_TAG_enumerator;
    TypeE2G_243.Children := 0;
    TypeE2G_243.Add(DW_AT_name, DW_FORM_string, 'E2G'+#0);
    TypeE2G_243.Add(DW_AT_const_value, DW_FORM_data4, [$06, $00, $00, $00]);

    TypeE2H_244 := TypeTENUM2_236.GetNewChild;
    TypeE2H_244.Tag := DW_TAG_enumerator;
    TypeE2H_244.Children := 0;
    TypeE2H_244.Add(DW_AT_name, DW_FORM_string, 'E2H'+#0);
    TypeE2H_244.Add(DW_AT_const_value, DW_FORM_data4, [$07, $00, $00, $00]);

    TypeE2I_245 := TypeTENUM2_236.GetNewChild;
    TypeE2I_245.Tag := DW_TAG_enumerator;
    TypeE2I_245.Children := 0;
    TypeE2I_245.Add(DW_AT_name, DW_FORM_string, 'E2I'+#0);
    TypeE2I_245.Add(DW_AT_const_value, DW_FORM_data4, [$08, $00, $00, $00]);

  Type_246 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_246.Tag := DW_TAG_reference_type;
  Type_246.Children := 0;
  Type_246.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM2_235); // $8A, $0F, $00, $00

  TypeDeclTENUM3_247 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTENUM3_247.Tag := DW_TAG_typedef;
  TypeDeclTENUM3_247.Children := 0;
  TypeDeclTENUM3_247.Add(DW_AT_name, DW_FORM_string, 'TENUM3'+#0);
  TypeDeclTENUM3_247.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTENUM3_248); // $02, $10, $00, $00

  TypeTENUM3_248 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTENUM3_248.Tag := DW_TAG_enumeration_type;
  TypeTENUM3_248.Children := 1;
  TypeTENUM3_248.Add(DW_AT_name, DW_FORM_string, 'TENUM3'+#0);
  TypeTENUM3_248.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeE3A_249 := TypeTENUM3_248.GetNewChild;
    TypeE3A_249.Tag := DW_TAG_enumerator;
    TypeE3A_249.Children := 0;
    TypeE3A_249.Add(DW_AT_name, DW_FORM_string, 'E3A'+#0);
    TypeE3A_249.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeE3B_250 := TypeTENUM3_248.GetNewChild;
    TypeE3B_250.Tag := DW_TAG_enumerator;
    TypeE3B_250.Children := 0;
    TypeE3B_250.Add(DW_AT_name, DW_FORM_string, 'E3B'+#0);
    TypeE3B_250.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeE3C_251 := TypeTENUM3_248.GetNewChild;
    TypeE3C_251.Tag := DW_TAG_enumerator;
    TypeE3C_251.Children := 0;
    TypeE3C_251.Add(DW_AT_name, DW_FORM_string, 'E3C'+#0);
    TypeE3C_251.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

    TypeE3D_252 := TypeTENUM3_248.GetNewChild;
    TypeE3D_252.Tag := DW_TAG_enumerator;
    TypeE3D_252.Children := 0;
    TypeE3D_252.Add(DW_AT_name, DW_FORM_string, 'E3D'+#0);
    TypeE3D_252.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

    TypeE3E_253 := TypeTENUM3_248.GetNewChild;
    TypeE3E_253.Tag := DW_TAG_enumerator;
    TypeE3E_253.Children := 0;
    TypeE3E_253.Add(DW_AT_name, DW_FORM_string, 'E3E'+#0);
    TypeE3E_253.Add(DW_AT_const_value, DW_FORM_data4, [$04, $00, $00, $00]);

    TypeE3F_254 := TypeTENUM3_248.GetNewChild;
    TypeE3F_254.Tag := DW_TAG_enumerator;
    TypeE3F_254.Children := 0;
    TypeE3F_254.Add(DW_AT_name, DW_FORM_string, 'E3F'+#0);
    TypeE3F_254.Add(DW_AT_const_value, DW_FORM_data4, [$05, $00, $00, $00]);

    TypeE3G_255 := TypeTENUM3_248.GetNewChild;
    TypeE3G_255.Tag := DW_TAG_enumerator;
    TypeE3G_255.Children := 0;
    TypeE3G_255.Add(DW_AT_name, DW_FORM_string, 'E3G'+#0);
    TypeE3G_255.Add(DW_AT_const_value, DW_FORM_data4, [$06, $00, $00, $00]);

    TypeE3H_256 := TypeTENUM3_248.GetNewChild;
    TypeE3H_256.Tag := DW_TAG_enumerator;
    TypeE3H_256.Children := 0;
    TypeE3H_256.Add(DW_AT_name, DW_FORM_string, 'E3H'+#0);
    TypeE3H_256.Add(DW_AT_const_value, DW_FORM_data4, [$07, $00, $00, $00]);

    TypeE3I_257 := TypeTENUM3_248.GetNewChild;
    TypeE3I_257.Tag := DW_TAG_enumerator;
    TypeE3I_257.Children := 0;
    TypeE3I_257.Add(DW_AT_name, DW_FORM_string, 'E3I'+#0);
    TypeE3I_257.Add(DW_AT_const_value, DW_FORM_data4, [$08, $00, $00, $00]);

    TypeE3J_258 := TypeTENUM3_248.GetNewChild;
    TypeE3J_258.Tag := DW_TAG_enumerator;
    TypeE3J_258.Children := 0;
    TypeE3J_258.Add(DW_AT_name, DW_FORM_string, 'E3J'+#0);
    TypeE3J_258.Add(DW_AT_const_value, DW_FORM_data4, [$09, $00, $00, $00]);

    TypeE3K_259 := TypeTENUM3_248.GetNewChild;
    TypeE3K_259.Tag := DW_TAG_enumerator;
    TypeE3K_259.Children := 0;
    TypeE3K_259.Add(DW_AT_name, DW_FORM_string, 'E3K'+#0);
    TypeE3K_259.Add(DW_AT_const_value, DW_FORM_data4, [$0A, $00, $00, $00]);

    TypeE3L_260 := TypeTENUM3_248.GetNewChild;
    TypeE3L_260.Tag := DW_TAG_enumerator;
    TypeE3L_260.Children := 0;
    TypeE3L_260.Add(DW_AT_name, DW_FORM_string, 'E3L'+#0);
    TypeE3L_260.Add(DW_AT_const_value, DW_FORM_data4, [$0B, $00, $00, $00]);

    TypeE3M_261 := TypeTENUM3_248.GetNewChild;
    TypeE3M_261.Tag := DW_TAG_enumerator;
    TypeE3M_261.Children := 0;
    TypeE3M_261.Add(DW_AT_name, DW_FORM_string, 'E3M'+#0);
    TypeE3M_261.Add(DW_AT_const_value, DW_FORM_data4, [$0C, $00, $00, $00]);

    TypeE3N_262 := TypeTENUM3_248.GetNewChild;
    TypeE3N_262.Tag := DW_TAG_enumerator;
    TypeE3N_262.Children := 0;
    TypeE3N_262.Add(DW_AT_name, DW_FORM_string, 'E3N'+#0);
    TypeE3N_262.Add(DW_AT_const_value, DW_FORM_data4, [$0D, $00, $00, $00]);

    TypeE3O_263 := TypeTENUM3_248.GetNewChild;
    TypeE3O_263.Tag := DW_TAG_enumerator;
    TypeE3O_263.Children := 0;
    TypeE3O_263.Add(DW_AT_name, DW_FORM_string, 'E3O'+#0);
    TypeE3O_263.Add(DW_AT_const_value, DW_FORM_data4, [$0E, $00, $00, $00]);

    TypeE3P_264 := TypeTENUM3_248.GetNewChild;
    TypeE3P_264.Tag := DW_TAG_enumerator;
    TypeE3P_264.Children := 0;
    TypeE3P_264.Add(DW_AT_name, DW_FORM_string, 'E3P'+#0);
    TypeE3P_264.Add(DW_AT_const_value, DW_FORM_data4, [$0F, $00, $00, $00]);

    TypeE3Q_265 := TypeTENUM3_248.GetNewChild;
    TypeE3Q_265.Tag := DW_TAG_enumerator;
    TypeE3Q_265.Children := 0;
    TypeE3Q_265.Add(DW_AT_name, DW_FORM_string, 'E3Q'+#0);
    TypeE3Q_265.Add(DW_AT_const_value, DW_FORM_data4, [$10, $00, $00, $00]);

  Type_266 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_266.Tag := DW_TAG_reference_type;
  Type_266.Children := 0;
  Type_266.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM3_247); // $F6, $0F, $00, $00

  TypeDeclTENUMX_267 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTENUMX_267.Tag := DW_TAG_typedef;
  TypeDeclTENUMX_267.Children := 0;
  TypeDeclTENUMX_267.Add(DW_AT_name, DW_FORM_string, 'TENUMX'+#0);
  TypeDeclTENUMX_267.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTENUMX_268); // $B6, $10, $00, $00

  TypeTENUMX_268 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTENUMX_268.Tag := DW_TAG_enumeration_type;
  TypeTENUMX_268.Children := 1;
  TypeTENUMX_268.Add(DW_AT_name, DW_FORM_string, 'TENUMX'+#0);
  TypeTENUMX_268.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeEXA_269 := TypeTENUMX_268.GetNewChild;
    TypeEXA_269.Tag := DW_TAG_enumerator;
    TypeEXA_269.Children := 0;
    TypeEXA_269.Add(DW_AT_name, DW_FORM_string, 'EXA'+#0);
    TypeEXA_269.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeEXC_270 := TypeTENUMX_268.GetNewChild;
    TypeEXC_270.Tag := DW_TAG_enumerator;
    TypeEXC_270.Children := 0;
    TypeEXC_270.Add(DW_AT_name, DW_FORM_string, 'EXC'+#0);
    TypeEXC_270.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

    TypeEXB_271 := TypeTENUMX_268.GetNewChild;
    TypeEXB_271.Tag := DW_TAG_enumerator;
    TypeEXB_271.Children := 0;
    TypeEXB_271.Add(DW_AT_name, DW_FORM_string, 'EXB'+#0);
    TypeEXB_271.Add(DW_AT_const_value, DW_FORM_data4, [$0A, $00, $00, $00]);

  Type_272 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_272.Tag := DW_TAG_reference_type;
  Type_272.Children := 0;
  Type_272.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMX_267); // $AA, $10, $00, $00

  TypeDeclTENUMR3_273 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTENUMR3_273.Tag := DW_TAG_typedef;
  TypeDeclTENUMR3_273.Children := 0;
  TypeDeclTENUMR3_273.Add(DW_AT_name, DW_FORM_string, 'TENUMR3'+#0);
  TypeDeclTENUMR3_273.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTENUMR3_274); // $ED, $10, $00, $00

  TypeTENUMR3_274 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTENUMR3_274.Tag := DW_TAG_enumeration_type;
  TypeTENUMR3_274.Children := 1;
  TypeTENUMR3_274.Add(DW_AT_name, DW_FORM_string, 'TENUMR3'+#0);
  TypeTENUMR3_274.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);
  TypeTENUMR3_274.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM3_247); // $F6, $0F, $00, $00

    TypeE3C_275 := TypeTENUMR3_274.GetNewChild;
    TypeE3C_275.Tag := DW_TAG_enumerator;
    TypeE3C_275.Children := 0;
    TypeE3C_275.Add(DW_AT_name, DW_FORM_string, 'E3C'+#0);
    TypeE3C_275.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

    TypeE3D_276 := TypeTENUMR3_274.GetNewChild;
    TypeE3D_276.Tag := DW_TAG_enumerator;
    TypeE3D_276.Children := 0;
    TypeE3D_276.Add(DW_AT_name, DW_FORM_string, 'E3D'+#0);
    TypeE3D_276.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

    TypeE3E_277 := TypeTENUMR3_274.GetNewChild;
    TypeE3E_277.Tag := DW_TAG_enumerator;
    TypeE3E_277.Children := 0;
    TypeE3E_277.Add(DW_AT_name, DW_FORM_string, 'E3E'+#0);
    TypeE3E_277.Add(DW_AT_const_value, DW_FORM_data4, [$04, $00, $00, $00]);

    TypeE3F_278 := TypeTENUMR3_274.GetNewChild;
    TypeE3F_278.Tag := DW_TAG_enumerator;
    TypeE3F_278.Children := 0;
    TypeE3F_278.Add(DW_AT_name, DW_FORM_string, 'E3F'+#0);
    TypeE3F_278.Add(DW_AT_const_value, DW_FORM_data4, [$05, $00, $00, $00]);

    TypeE3G_279 := TypeTENUMR3_274.GetNewChild;
    TypeE3G_279.Tag := DW_TAG_enumerator;
    TypeE3G_279.Children := 0;
    TypeE3G_279.Add(DW_AT_name, DW_FORM_string, 'E3G'+#0);
    TypeE3G_279.Add(DW_AT_const_value, DW_FORM_data4, [$06, $00, $00, $00]);

    TypeE3H_280 := TypeTENUMR3_274.GetNewChild;
    TypeE3H_280.Tag := DW_TAG_enumerator;
    TypeE3H_280.Children := 0;
    TypeE3H_280.Add(DW_AT_name, DW_FORM_string, 'E3H'+#0);
    TypeE3H_280.Add(DW_AT_const_value, DW_FORM_data4, [$07, $00, $00, $00]);

    TypeE3I_281 := TypeTENUMR3_274.GetNewChild;
    TypeE3I_281.Tag := DW_TAG_enumerator;
    TypeE3I_281.Children := 0;
    TypeE3I_281.Add(DW_AT_name, DW_FORM_string, 'E3I'+#0);
    TypeE3I_281.Add(DW_AT_const_value, DW_FORM_data4, [$08, $00, $00, $00]);

    TypeE3J_282 := TypeTENUMR3_274.GetNewChild;
    TypeE3J_282.Tag := DW_TAG_enumerator;
    TypeE3J_282.Children := 0;
    TypeE3J_282.Add(DW_AT_name, DW_FORM_string, 'E3J'+#0);
    TypeE3J_282.Add(DW_AT_const_value, DW_FORM_data4, [$09, $00, $00, $00]);

    TypeE3K_283 := TypeTENUMR3_274.GetNewChild;
    TypeE3K_283.Tag := DW_TAG_enumerator;
    TypeE3K_283.Children := 0;
    TypeE3K_283.Add(DW_AT_name, DW_FORM_string, 'E3K'+#0);
    TypeE3K_283.Add(DW_AT_const_value, DW_FORM_data4, [$0A, $00, $00, $00]);

    TypeE3L_284 := TypeTENUMR3_274.GetNewChild;
    TypeE3L_284.Tag := DW_TAG_enumerator;
    TypeE3L_284.Children := 0;
    TypeE3L_284.Add(DW_AT_name, DW_FORM_string, 'E3L'+#0);
    TypeE3L_284.Add(DW_AT_const_value, DW_FORM_data4, [$0B, $00, $00, $00]);

  Type_285 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_285.Tag := DW_TAG_reference_type;
  Type_285.Children := 0;
  Type_285.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMR3_273); // $E0, $10, $00, $00

  TypeDeclPENUM0_286 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPENUM0_286.Tag := DW_TAG_typedef;
  TypeDeclPENUM0_286.Children := 0;
  TypeDeclPENUM0_286.Add(DW_AT_name, DW_FORM_string, 'PENUM0'+#0);
  TypeDeclPENUM0_286.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_287); // $67, $11, $00, $00

  TypePtr_287 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_287.Tag := DW_TAG_pointer_type;
  TypePtr_287.Children := 0;
  TypePtr_287.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM0_225); // $30, $0F, $00, $00

  Type_288 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_288.Tag := DW_TAG_reference_type;
  Type_288.Children := 0;
  Type_288.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUM0_286); // $5B, $11, $00, $00

  TypeDeclPENUM1_289 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPENUM1_289.Tag := DW_TAG_typedef;
  TypeDeclPENUM1_289.Children := 0;
  TypeDeclPENUM1_289.Add(DW_AT_name, DW_FORM_string, 'PENUM1'+#0);
  TypeDeclPENUM1_289.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_290); // $7D, $11, $00, $00

  TypePtr_290 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_290.Tag := DW_TAG_pointer_type;
  TypePtr_290.Children := 0;
  TypePtr_290.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM1_229); // $54, $0F, $00, $00

  Type_291 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_291.Tag := DW_TAG_reference_type;
  Type_291.Children := 0;
  Type_291.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUM1_289); // $71, $11, $00, $00

  TypeDeclPENUM2_292 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPENUM2_292.Tag := DW_TAG_typedef;
  TypeDeclPENUM2_292.Children := 0;
  TypeDeclPENUM2_292.Add(DW_AT_name, DW_FORM_string, 'PENUM2'+#0);
  TypeDeclPENUM2_292.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_293); // $93, $11, $00, $00

  TypePtr_293 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_293.Tag := DW_TAG_pointer_type;
  TypePtr_293.Children := 0;
  TypePtr_293.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM2_235); // $8A, $0F, $00, $00

  Type_294 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_294.Tag := DW_TAG_reference_type;
  Type_294.Children := 0;
  Type_294.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUM2_292); // $87, $11, $00, $00

  TypeDeclPENUM3_295 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPENUM3_295.Tag := DW_TAG_typedef;
  TypeDeclPENUM3_295.Children := 0;
  TypeDeclPENUM3_295.Add(DW_AT_name, DW_FORM_string, 'PENUM3'+#0);
  TypeDeclPENUM3_295.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_296); // $A9, $11, $00, $00

  TypePtr_296 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_296.Tag := DW_TAG_pointer_type;
  TypePtr_296.Children := 0;
  TypePtr_296.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM3_247); // $F6, $0F, $00, $00

  Type_297 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_297.Tag := DW_TAG_reference_type;
  Type_297.Children := 0;
  Type_297.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUM3_295); // $9D, $11, $00, $00

  TypeDeclPENUMX_298 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPENUMX_298.Tag := DW_TAG_typedef;
  TypeDeclPENUMX_298.Children := 0;
  TypeDeclPENUMX_298.Add(DW_AT_name, DW_FORM_string, 'PENUMX'+#0);
  TypeDeclPENUMX_298.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_299); // $BF, $11, $00, $00

  TypePtr_299 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_299.Tag := DW_TAG_pointer_type;
  TypePtr_299.Children := 0;
  TypePtr_299.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMX_267); // $AA, $10, $00, $00

  Type_300 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_300.Tag := DW_TAG_reference_type;
  Type_300.Children := 0;
  Type_300.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUMX_298); // $B3, $11, $00, $00

  TypeDeclPENUMR3_301 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPENUMR3_301.Tag := DW_TAG_typedef;
  TypeDeclPENUMR3_301.Children := 0;
  TypeDeclPENUMR3_301.Add(DW_AT_name, DW_FORM_string, 'PENUMR3'+#0);
  TypeDeclPENUMR3_301.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_302); // $D6, $11, $00, $00

  TypePtr_302 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_302.Tag := DW_TAG_pointer_type;
  TypePtr_302.Children := 0;
  TypePtr_302.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMR3_273); // $E0, $10, $00, $00

  Type_303 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_303.Tag := DW_TAG_reference_type;
  Type_303.Children := 0;
  Type_303.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUMR3_301); // $C9, $11, $00, $00

  TypeDeclTSET0_304 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSET0_304.Tag := DW_TAG_typedef;
  TypeDeclTSET0_304.Children := 0;
  TypeDeclTSET0_304.Add(DW_AT_name, DW_FORM_string, 'TSET0'+#0);
  TypeDeclTSET0_304.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSET0_305); // $EB, $11, $00, $00

  TypeTSET0_305 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSET0_305.Tag := DW_TAG_set_type;
  TypeTSET0_305.Children := 0;
  TypeTSET0_305.Add(DW_AT_name, DW_FORM_string, 'TSET0'+#0);
  TypeTSET0_305.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  TypeTSET0_305.AddRef(DW_AT_type, DW_FORM_ref4, @Type_306); // $F8, $11, $00, $00

  Type_306 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_306.Tag := DW_TAG_subrange_type;
  Type_306.Children := 0;
  Type_306.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_306.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 0);
  Type_306.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM0_225); // $30, $0F, $00, $00

  Type_307 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_307.Tag := DW_TAG_reference_type;
  Type_307.Children := 0;
  Type_307.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET0_304); // $E0, $11, $00, $00

  TypeDeclTSET1_308 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSET1_308.Tag := DW_TAG_typedef;
  TypeDeclTSET1_308.Children := 0;
  TypeDeclTSET1_308.Add(DW_AT_name, DW_FORM_string, 'TSET1'+#0);
  TypeDeclTSET1_308.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSET1_309); // $0F, $12, $00, $00

  TypeTSET1_309 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSET1_309.Tag := DW_TAG_set_type;
  TypeTSET1_309.Children := 0;
  TypeTSET1_309.Add(DW_AT_name, DW_FORM_string, 'TSET1'+#0);
  TypeTSET1_309.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  TypeTSET1_309.AddRef(DW_AT_type, DW_FORM_ref4, @Type_310); // $1C, $12, $00, $00

  Type_310 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_310.Tag := DW_TAG_subrange_type;
  Type_310.Children := 0;
  Type_310.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_310.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 2);
  Type_310.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM1_229); // $54, $0F, $00, $00

  Type_311 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_311.Tag := DW_TAG_reference_type;
  Type_311.Children := 0;
  Type_311.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET1_308); // $04, $12, $00, $00

  TypeDeclTSET2_312 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSET2_312.Tag := DW_TAG_typedef;
  TypeDeclTSET2_312.Children := 0;
  TypeDeclTSET2_312.Add(DW_AT_name, DW_FORM_string, 'TSET2'+#0);
  TypeDeclTSET2_312.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSET2_313); // $33, $12, $00, $00

  TypeTSET2_313 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSET2_313.Tag := DW_TAG_set_type;
  TypeTSET2_313.Children := 0;
  TypeTSET2_313.Add(DW_AT_name, DW_FORM_string, 'TSET2'+#0);
  TypeTSET2_313.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  TypeTSET2_313.AddRef(DW_AT_type, DW_FORM_ref4, @Type_314); // $40, $12, $00, $00

  Type_314 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_314.Tag := DW_TAG_subrange_type;
  Type_314.Children := 0;
  Type_314.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_314.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 8);
  Type_314.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM2_235); // $8A, $0F, $00, $00

  Type_315 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_315.Tag := DW_TAG_reference_type;
  Type_315.Children := 0;
  Type_315.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET2_312); // $28, $12, $00, $00

  TypeDeclTSET3_316 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSET3_316.Tag := DW_TAG_typedef;
  TypeDeclTSET3_316.Children := 0;
  TypeDeclTSET3_316.Add(DW_AT_name, DW_FORM_string, 'TSET3'+#0);
  TypeDeclTSET3_316.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSET3_317); // $57, $12, $00, $00

  TypeTSET3_317 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSET3_317.Tag := DW_TAG_set_type;
  TypeTSET3_317.Children := 0;
  TypeTSET3_317.Add(DW_AT_name, DW_FORM_string, 'TSET3'+#0);
  TypeTSET3_317.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  TypeTSET3_317.AddRef(DW_AT_type, DW_FORM_ref4, @Type_318); // $64, $12, $00, $00

  Type_318 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_318.Tag := DW_TAG_subrange_type;
  Type_318.Children := 0;
  Type_318.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_318.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 16);
  Type_318.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM3_247); // $F6, $0F, $00, $00

  Type_319 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_319.Tag := DW_TAG_reference_type;
  Type_319.Children := 0;
  Type_319.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET3_316); // $4C, $12, $00, $00

  TypeDeclTSETX1_320 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETX1_320.Tag := DW_TAG_typedef;
  TypeDeclTSETX1_320.Children := 0;
  TypeDeclTSETX1_320.Add(DW_AT_name, DW_FORM_string, 'TSETX1'+#0);
  TypeDeclTSETX1_320.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETX1_321); // $7C, $12, $00, $00

  TypeTSETX1_321 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETX1_321.Tag := DW_TAG_set_type;
  TypeTSETX1_321.Children := 0;
  TypeTSETX1_321.Add(DW_AT_name, DW_FORM_string, 'TSETX1'+#0);
  TypeTSETX1_321.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  TypeTSETX1_321.AddRef(DW_AT_type, DW_FORM_ref4, @Type_322); // $8A, $12, $00, $00

  Type_322 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_322.Tag := DW_TAG_subrange_type;
  Type_322.Children := 0;
  Type_322.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_322.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 2);
  Type_322.AddRef(DW_AT_type, DW_FORM_ref4, @Type_925); // $07, $2B, $00, $00

  Type_323 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_323.Tag := DW_TAG_reference_type;
  Type_323.Children := 0;
  Type_323.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETX1_320); // $70, $12, $00, $00

  TypeDeclTSETB1_324 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETB1_324.Tag := DW_TAG_typedef;
  TypeDeclTSETB1_324.Children := 0;
  TypeDeclTSETB1_324.Add(DW_AT_name, DW_FORM_string, 'TSETB1'+#0);
  TypeDeclTSETB1_324.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETB1_325); // $A2, $12, $00, $00

  TypeTSETB1_325 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETB1_325.Tag := DW_TAG_set_type;
  TypeTSETB1_325.Children := 0;
  TypeTSETB1_325.Add(DW_AT_name, DW_FORM_string, 'TSETB1'+#0);
  TypeTSETB1_325.Add(DW_AT_byte_size, DW_FORM_data2, [$20, $00]);
  TypeTSETB1_325.AddRef(DW_AT_type, DW_FORM_ref4, @Type_326); // $B0, $12, $00, $00

  Type_326 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_326.Tag := DW_TAG_subrange_type;
  Type_326.Children := 0;
  Type_326.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_326.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 255);
  Type_326.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_141); // $6A, $0C, $00, $00

  Type_327 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_327.Tag := DW_TAG_reference_type;
  Type_327.Children := 0;
  Type_327.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETB1_324); // $96, $12, $00, $00

  TypeDeclTSETB2_328 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETB2_328.Tag := DW_TAG_typedef;
  TypeDeclTSETB2_328.Children := 0;
  TypeDeclTSETB2_328.Add(DW_AT_name, DW_FORM_string, 'TSETB2'+#0);
  TypeDeclTSETB2_328.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETB2_329); // $C9, $12, $00, $00

  TypeTSETB2_329 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETB2_329.Tag := DW_TAG_set_type;
  TypeTSETB2_329.Children := 0;
  TypeTSETB2_329.Add(DW_AT_name, DW_FORM_string, 'TSETB2'+#0);
  TypeTSETB2_329.Add(DW_AT_byte_size, DW_FORM_data2, [$20, $00]);
  TypeTSETB2_329.AddRef(DW_AT_type, DW_FORM_ref4, @Type_330); // $D7, $12, $00, $00

  Type_330 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_330.Tag := DW_TAG_subrange_type;
  Type_330.Children := 0;
  Type_330.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_330.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 80);
  Type_330.AddRef(DW_AT_type, DW_FORM_ref4, @Type_930); // $2A, $2B, $00, $00

  Type_331 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_331.Tag := DW_TAG_reference_type;
  Type_331.Children := 0;
  Type_331.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETB2_328); // $BD, $12, $00, $00

  TypeDeclTSETC1_332 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETC1_332.Tag := DW_TAG_typedef;
  TypeDeclTSETC1_332.Children := 0;
  TypeDeclTSETC1_332.Add(DW_AT_name, DW_FORM_string, 'TSETC1'+#0);
  TypeDeclTSETC1_332.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETC1_333); // $F0, $12, $00, $00

  TypeTSETC1_333 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETC1_333.Tag := DW_TAG_set_type;
  TypeTSETC1_333.Children := 0;
  TypeTSETC1_333.Add(DW_AT_name, DW_FORM_string, 'TSETC1'+#0);
  TypeTSETC1_333.Add(DW_AT_byte_size, DW_FORM_data2, [$20, $00]);
  TypeTSETC1_333.AddRef(DW_AT_type, DW_FORM_ref4, @Type_334); // $FE, $12, $00, $00

  Type_334 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_334.Tag := DW_TAG_subrange_type;
  Type_334.Children := 0;
  Type_334.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_334.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 255);
  Type_334.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclCHAR_945); // $94, $2B, $00, $00

  Type_335 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_335.Tag := DW_TAG_reference_type;
  Type_335.Children := 0;
  Type_335.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETC1_332); // $E4, $12, $00, $00

  TypeDeclTSETR3_336 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETR3_336.Tag := DW_TAG_typedef;
  TypeDeclTSETR3_336.Children := 0;
  TypeDeclTSETR3_336.Add(DW_AT_name, DW_FORM_string, 'TSETR3'+#0);
  TypeDeclTSETR3_336.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETR3_337); // $17, $13, $00, $00

  TypeTSETR3_337 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETR3_337.Tag := DW_TAG_set_type;
  TypeTSETR3_337.Children := 0;
  TypeTSETR3_337.Add(DW_AT_name, DW_FORM_string, 'TSETR3'+#0);
  TypeTSETR3_337.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  TypeTSETR3_337.AddRef(DW_AT_type, DW_FORM_ref4, @Type_338); // $25, $13, $00, $00

  Type_338 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_338.Tag := DW_TAG_subrange_type;
  Type_338.Children := 0;
  Type_338.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_338.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 11);
  Type_338.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMR3_273); // $E0, $10, $00, $00

  Type_339 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_339.Tag := DW_TAG_reference_type;
  Type_339.Children := 0;
  Type_339.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETR3_336); // $0B, $13, $00, $00

  TypeDeclTSETP0_340 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETP0_340.Tag := DW_TAG_typedef;
  TypeDeclTSETP0_340.Children := 0;
  TypeDeclTSETP0_340.Add(DW_AT_name, DW_FORM_string, 'TSETP0'+#0);
  TypeDeclTSETP0_340.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETP0_341); // $3D, $13, $00, $00

  TypeTSETP0_341 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETP0_341.Tag := DW_TAG_set_type;
  TypeTSETP0_341.Children := 0;
  TypeTSETP0_341.Add(DW_AT_name, DW_FORM_string, 'TSETP0'+#0);
  TypeTSETP0_341.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  TypeTSETP0_341.AddRef(DW_AT_type, DW_FORM_ref4, @Type_342); // $4B, $13, $00, $00

  Type_342 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_342.Tag := DW_TAG_subrange_type;
  Type_342.Children := 0;
  Type_342.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_342.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 0);
  Type_342.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM0_225); // $30, $0F, $00, $00

  Type_343 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_343.Tag := DW_TAG_reference_type;
  Type_343.Children := 0;
  Type_343.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETP0_340); // $31, $13, $00, $00

  TypeDeclTSETP1_344 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETP1_344.Tag := DW_TAG_typedef;
  TypeDeclTSETP1_344.Children := 0;
  TypeDeclTSETP1_344.Add(DW_AT_name, DW_FORM_string, 'TSETP1'+#0);
  TypeDeclTSETP1_344.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETP1_345); // $63, $13, $00, $00

  TypeTSETP1_345 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETP1_345.Tag := DW_TAG_set_type;
  TypeTSETP1_345.Children := 0;
  TypeTSETP1_345.Add(DW_AT_name, DW_FORM_string, 'TSETP1'+#0);
  TypeTSETP1_345.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  TypeTSETP1_345.AddRef(DW_AT_type, DW_FORM_ref4, @Type_346); // $71, $13, $00, $00

  Type_346 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_346.Tag := DW_TAG_subrange_type;
  Type_346.Children := 0;
  Type_346.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_346.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 2);
  Type_346.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM1_229); // $54, $0F, $00, $00

  Type_347 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_347.Tag := DW_TAG_reference_type;
  Type_347.Children := 0;
  Type_347.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETP1_344); // $57, $13, $00, $00

  TypeDeclTSETP2_348 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETP2_348.Tag := DW_TAG_typedef;
  TypeDeclTSETP2_348.Children := 0;
  TypeDeclTSETP2_348.Add(DW_AT_name, DW_FORM_string, 'TSETP2'+#0);
  TypeDeclTSETP2_348.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETP2_349); // $89, $13, $00, $00

  TypeTSETP2_349 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETP2_349.Tag := DW_TAG_set_type;
  TypeTSETP2_349.Children := 0;
  TypeTSETP2_349.Add(DW_AT_name, DW_FORM_string, 'TSETP2'+#0);
  TypeTSETP2_349.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  TypeTSETP2_349.AddRef(DW_AT_type, DW_FORM_ref4, @Type_350); // $97, $13, $00, $00

  Type_350 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_350.Tag := DW_TAG_subrange_type;
  Type_350.Children := 0;
  Type_350.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_350.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 8);
  Type_350.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM2_235); // $8A, $0F, $00, $00

  Type_351 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_351.Tag := DW_TAG_reference_type;
  Type_351.Children := 0;
  Type_351.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETP2_348); // $7D, $13, $00, $00

  TypeDeclTSETP3_352 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETP3_352.Tag := DW_TAG_typedef;
  TypeDeclTSETP3_352.Children := 0;
  TypeDeclTSETP3_352.Add(DW_AT_name, DW_FORM_string, 'TSETP3'+#0);
  TypeDeclTSETP3_352.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETP3_353); // $AF, $13, $00, $00

  TypeTSETP3_353 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETP3_353.Tag := DW_TAG_set_type;
  TypeTSETP3_353.Children := 0;
  TypeTSETP3_353.Add(DW_AT_name, DW_FORM_string, 'TSETP3'+#0);
  TypeTSETP3_353.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  TypeTSETP3_353.AddRef(DW_AT_type, DW_FORM_ref4, @Type_354); // $BD, $13, $00, $00

  Type_354 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_354.Tag := DW_TAG_subrange_type;
  Type_354.Children := 0;
  Type_354.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_354.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 16);
  Type_354.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM3_247); // $F6, $0F, $00, $00

  Type_355 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_355.Tag := DW_TAG_reference_type;
  Type_355.Children := 0;
  Type_355.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETP3_352); // $A3, $13, $00, $00

  TypeDeclTSETPX1_356 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETPX1_356.Tag := DW_TAG_typedef;
  TypeDeclTSETPX1_356.Children := 0;
  TypeDeclTSETPX1_356.Add(DW_AT_name, DW_FORM_string, 'TSETPX1'+#0);
  TypeDeclTSETPX1_356.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETPX1_357); // $D6, $13, $00, $00

  TypeTSETPX1_357 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETPX1_357.Tag := DW_TAG_set_type;
  TypeTSETPX1_357.Children := 0;
  TypeTSETPX1_357.Add(DW_AT_name, DW_FORM_string, 'TSETPX1'+#0);
  TypeTSETPX1_357.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  TypeTSETPX1_357.AddRef(DW_AT_type, DW_FORM_ref4, @Type_358); // $E5, $13, $00, $00

  Type_358 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_358.Tag := DW_TAG_subrange_type;
  Type_358.Children := 0;
  Type_358.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_358.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 2);
  Type_358.AddRef(DW_AT_type, DW_FORM_ref4, @Type_932); // $36, $2B, $00, $00

  Type_359 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_359.Tag := DW_TAG_reference_type;
  Type_359.Children := 0;
  Type_359.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETPX1_356); // $C9, $13, $00, $00

  TypeDeclTSETPB1_360 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETPB1_360.Tag := DW_TAG_typedef;
  TypeDeclTSETPB1_360.Children := 0;
  TypeDeclTSETPB1_360.Add(DW_AT_name, DW_FORM_string, 'TSETPB1'+#0);
  TypeDeclTSETPB1_360.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETPB1_361); // $FE, $13, $00, $00

  TypeTSETPB1_361 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETPB1_361.Tag := DW_TAG_set_type;
  TypeTSETPB1_361.Children := 0;
  TypeTSETPB1_361.Add(DW_AT_name, DW_FORM_string, 'TSETPB1'+#0);
  TypeTSETPB1_361.Add(DW_AT_byte_size, DW_FORM_data2, [$20, $00]);
  TypeTSETPB1_361.AddRef(DW_AT_type, DW_FORM_ref4, @Type_362); // $0D, $14, $00, $00

  Type_362 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_362.Tag := DW_TAG_subrange_type;
  Type_362.Children := 0;
  Type_362.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_362.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 255);
  Type_362.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_141); // $6A, $0C, $00, $00

  Type_363 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_363.Tag := DW_TAG_reference_type;
  Type_363.Children := 0;
  Type_363.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETPB1_360); // $F1, $13, $00, $00

  TypeDeclTSETPB2_364 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETPB2_364.Tag := DW_TAG_typedef;
  TypeDeclTSETPB2_364.Children := 0;
  TypeDeclTSETPB2_364.Add(DW_AT_name, DW_FORM_string, 'TSETPB2'+#0);
  TypeDeclTSETPB2_364.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETPB2_365); // $27, $14, $00, $00

  TypeTSETPB2_365 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETPB2_365.Tag := DW_TAG_set_type;
  TypeTSETPB2_365.Children := 0;
  TypeTSETPB2_365.Add(DW_AT_name, DW_FORM_string, 'TSETPB2'+#0);
  TypeTSETPB2_365.Add(DW_AT_byte_size, DW_FORM_data2, [$20, $00]);
  TypeTSETPB2_365.AddRef(DW_AT_type, DW_FORM_ref4, @Type_366); // $36, $14, $00, $00

  Type_366 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_366.Tag := DW_TAG_subrange_type;
  Type_366.Children := 0;
  Type_366.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_366.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 80);
  Type_366.AddRef(DW_AT_type, DW_FORM_ref4, @Type_937); // $5C, $2B, $00, $00

  Type_367 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_367.Tag := DW_TAG_reference_type;
  Type_367.Children := 0;
  Type_367.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETPB2_364); // $1A, $14, $00, $00

  TypeDeclTSETPC1_368 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETPC1_368.Tag := DW_TAG_typedef;
  TypeDeclTSETPC1_368.Children := 0;
  TypeDeclTSETPC1_368.Add(DW_AT_name, DW_FORM_string, 'TSETPC1'+#0);
  TypeDeclTSETPC1_368.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETPC1_369); // $50, $14, $00, $00

  TypeTSETPC1_369 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETPC1_369.Tag := DW_TAG_set_type;
  TypeTSETPC1_369.Children := 0;
  TypeTSETPC1_369.Add(DW_AT_name, DW_FORM_string, 'TSETPC1'+#0);
  TypeTSETPC1_369.Add(DW_AT_byte_size, DW_FORM_data2, [$20, $00]);
  TypeTSETPC1_369.AddRef(DW_AT_type, DW_FORM_ref4, @Type_370); // $5F, $14, $00, $00

  Type_370 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_370.Tag := DW_TAG_subrange_type;
  Type_370.Children := 0;
  Type_370.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_370.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 255);
  Type_370.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclCHAR_945); // $94, $2B, $00, $00

  Type_371 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_371.Tag := DW_TAG_reference_type;
  Type_371.Children := 0;
  Type_371.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETPC1_368); // $43, $14, $00, $00

  TypeDeclTSETPR3_372 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTSETPR3_372.Tag := DW_TAG_typedef;
  TypeDeclTSETPR3_372.Children := 0;
  TypeDeclTSETPR3_372.Add(DW_AT_name, DW_FORM_string, 'TSETPR3'+#0);
  TypeDeclTSETPR3_372.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTSETPR3_373); // $79, $14, $00, $00

  TypeTSETPR3_373 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTSETPR3_373.Tag := DW_TAG_set_type;
  TypeTSETPR3_373.Children := 0;
  TypeTSETPR3_373.Add(DW_AT_name, DW_FORM_string, 'TSETPR3'+#0);
  TypeTSETPR3_373.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  TypeTSETPR3_373.AddRef(DW_AT_type, DW_FORM_ref4, @Type_374); // $88, $14, $00, $00

  Type_374 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_374.Tag := DW_TAG_subrange_type;
  Type_374.Children := 0;
  Type_374.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_374.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 11);
  Type_374.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMR3_273); // $E0, $10, $00, $00

  Type_375 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_375.Tag := DW_TAG_reference_type;
  Type_375.Children := 0;
  Type_375.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETPR3_372); // $6C, $14, $00, $00

  TypeDeclPSET0_376 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSET0_376.Tag := DW_TAG_typedef;
  TypeDeclPSET0_376.Children := 0;
  TypeDeclPSET0_376.Add(DW_AT_name, DW_FORM_string, 'PSET0'+#0);
  TypeDeclPSET0_376.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_377); // $9F, $14, $00, $00

  TypePtr_377 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_377.Tag := DW_TAG_pointer_type;
  TypePtr_377.Children := 0;
  TypePtr_377.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET0_304); // $E0, $11, $00, $00

  Type_378 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_378.Tag := DW_TAG_reference_type;
  Type_378.Children := 0;
  Type_378.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSET0_376); // $94, $14, $00, $00

  TypeDeclPSET1_379 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSET1_379.Tag := DW_TAG_typedef;
  TypeDeclPSET1_379.Children := 0;
  TypeDeclPSET1_379.Add(DW_AT_name, DW_FORM_string, 'PSET1'+#0);
  TypeDeclPSET1_379.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_380); // $B4, $14, $00, $00

  TypePtr_380 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_380.Tag := DW_TAG_pointer_type;
  TypePtr_380.Children := 0;
  TypePtr_380.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET1_308); // $04, $12, $00, $00

  Type_381 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_381.Tag := DW_TAG_reference_type;
  Type_381.Children := 0;
  Type_381.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSET1_379); // $A9, $14, $00, $00

  TypeDeclPSET2_382 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSET2_382.Tag := DW_TAG_typedef;
  TypeDeclPSET2_382.Children := 0;
  TypeDeclPSET2_382.Add(DW_AT_name, DW_FORM_string, 'PSET2'+#0);
  TypeDeclPSET2_382.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_383); // $C9, $14, $00, $00

  TypePtr_383 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_383.Tag := DW_TAG_pointer_type;
  TypePtr_383.Children := 0;
  TypePtr_383.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET2_312); // $28, $12, $00, $00

  Type_384 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_384.Tag := DW_TAG_reference_type;
  Type_384.Children := 0;
  Type_384.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSET2_382); // $BE, $14, $00, $00

  TypeDeclPSET3_385 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSET3_385.Tag := DW_TAG_typedef;
  TypeDeclPSET3_385.Children := 0;
  TypeDeclPSET3_385.Add(DW_AT_name, DW_FORM_string, 'PSET3'+#0);
  TypeDeclPSET3_385.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_386); // $DE, $14, $00, $00

  TypePtr_386 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_386.Tag := DW_TAG_pointer_type;
  TypePtr_386.Children := 0;
  TypePtr_386.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET3_316); // $4C, $12, $00, $00

  Type_387 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_387.Tag := DW_TAG_reference_type;
  Type_387.Children := 0;
  Type_387.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSET3_385); // $D3, $14, $00, $00

  TypeDeclPSETX1_388 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSETX1_388.Tag := DW_TAG_typedef;
  TypeDeclPSETX1_388.Children := 0;
  TypeDeclPSETX1_388.Add(DW_AT_name, DW_FORM_string, 'PSETX1'+#0);
  TypeDeclPSETX1_388.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_389); // $F4, $14, $00, $00

  TypePtr_389 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_389.Tag := DW_TAG_pointer_type;
  TypePtr_389.Children := 0;
  TypePtr_389.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETX1_320); // $70, $12, $00, $00

  Type_390 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_390.Tag := DW_TAG_reference_type;
  Type_390.Children := 0;
  Type_390.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETX1_388); // $E8, $14, $00, $00

  TypeDeclPSETB1_391 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSETB1_391.Tag := DW_TAG_typedef;
  TypeDeclPSETB1_391.Children := 0;
  TypeDeclPSETB1_391.Add(DW_AT_name, DW_FORM_string, 'PSETB1'+#0);
  TypeDeclPSETB1_391.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_392); // $0A, $15, $00, $00

  TypePtr_392 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_392.Tag := DW_TAG_pointer_type;
  TypePtr_392.Children := 0;
  TypePtr_392.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETB1_324); // $96, $12, $00, $00

  Type_393 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_393.Tag := DW_TAG_reference_type;
  Type_393.Children := 0;
  Type_393.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETB1_391); // $FE, $14, $00, $00

  TypeDeclPSETB2_394 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSETB2_394.Tag := DW_TAG_typedef;
  TypeDeclPSETB2_394.Children := 0;
  TypeDeclPSETB2_394.Add(DW_AT_name, DW_FORM_string, 'PSETB2'+#0);
  TypeDeclPSETB2_394.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_395); // $20, $15, $00, $00

  TypePtr_395 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_395.Tag := DW_TAG_pointer_type;
  TypePtr_395.Children := 0;
  TypePtr_395.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETB2_328); // $BD, $12, $00, $00

  Type_396 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_396.Tag := DW_TAG_reference_type;
  Type_396.Children := 0;
  Type_396.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETB2_394); // $14, $15, $00, $00

  TypeDeclPSETC1_397 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSETC1_397.Tag := DW_TAG_typedef;
  TypeDeclPSETC1_397.Children := 0;
  TypeDeclPSETC1_397.Add(DW_AT_name, DW_FORM_string, 'PSETC1'+#0);
  TypeDeclPSETC1_397.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_398); // $36, $15, $00, $00

  TypePtr_398 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_398.Tag := DW_TAG_pointer_type;
  TypePtr_398.Children := 0;
  TypePtr_398.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETC1_332); // $E4, $12, $00, $00

  Type_399 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_399.Tag := DW_TAG_reference_type;
  Type_399.Children := 0;
  Type_399.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETC1_397); // $2A, $15, $00, $00

  TypeDeclPSETR3_400 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSETR3_400.Tag := DW_TAG_typedef;
  TypeDeclPSETR3_400.Children := 0;
  TypeDeclPSETR3_400.Add(DW_AT_name, DW_FORM_string, 'PSETR3'+#0);
  TypeDeclPSETR3_400.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_401); // $4C, $15, $00, $00

  TypePtr_401 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_401.Tag := DW_TAG_pointer_type;
  TypePtr_401.Children := 0;
  TypePtr_401.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETR3_336); // $0B, $13, $00, $00

  Type_402 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_402.Tag := DW_TAG_reference_type;
  Type_402.Children := 0;
  Type_402.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETR3_400); // $40, $15, $00, $00

  TypeDeclPSETP0_403 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSETP0_403.Tag := DW_TAG_typedef;
  TypeDeclPSETP0_403.Children := 0;
  TypeDeclPSETP0_403.Add(DW_AT_name, DW_FORM_string, 'PSETP0'+#0);
  TypeDeclPSETP0_403.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_404); // $62, $15, $00, $00

  TypePtr_404 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_404.Tag := DW_TAG_pointer_type;
  TypePtr_404.Children := 0;
  TypePtr_404.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETP0_340); // $31, $13, $00, $00

  Type_405 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_405.Tag := DW_TAG_reference_type;
  Type_405.Children := 0;
  Type_405.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETP0_403); // $56, $15, $00, $00

  TypeDeclPSETP1_406 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSETP1_406.Tag := DW_TAG_typedef;
  TypeDeclPSETP1_406.Children := 0;
  TypeDeclPSETP1_406.Add(DW_AT_name, DW_FORM_string, 'PSETP1'+#0);
  TypeDeclPSETP1_406.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_407); // $78, $15, $00, $00

  TypePtr_407 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_407.Tag := DW_TAG_pointer_type;
  TypePtr_407.Children := 0;
  TypePtr_407.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETP1_344); // $57, $13, $00, $00

  Type_408 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_408.Tag := DW_TAG_reference_type;
  Type_408.Children := 0;
  Type_408.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETP1_406); // $6C, $15, $00, $00

  TypeDeclPSETP2_409 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSETP2_409.Tag := DW_TAG_typedef;
  TypeDeclPSETP2_409.Children := 0;
  TypeDeclPSETP2_409.Add(DW_AT_name, DW_FORM_string, 'PSETP2'+#0);
  TypeDeclPSETP2_409.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_410); // $8E, $15, $00, $00

  TypePtr_410 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_410.Tag := DW_TAG_pointer_type;
  TypePtr_410.Children := 0;
  TypePtr_410.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETP2_348); // $7D, $13, $00, $00

  Type_411 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_411.Tag := DW_TAG_reference_type;
  Type_411.Children := 0;
  Type_411.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETP2_409); // $82, $15, $00, $00

  TypeDeclPSETP3_412 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSETP3_412.Tag := DW_TAG_typedef;
  TypeDeclPSETP3_412.Children := 0;
  TypeDeclPSETP3_412.Add(DW_AT_name, DW_FORM_string, 'PSETP3'+#0);
  TypeDeclPSETP3_412.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_413); // $A4, $15, $00, $00

  TypePtr_413 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_413.Tag := DW_TAG_pointer_type;
  TypePtr_413.Children := 0;
  TypePtr_413.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETP3_352); // $A3, $13, $00, $00

  Type_414 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_414.Tag := DW_TAG_reference_type;
  Type_414.Children := 0;
  Type_414.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETP3_412); // $98, $15, $00, $00

  TypeDeclPSETPX1_415 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSETPX1_415.Tag := DW_TAG_typedef;
  TypeDeclPSETPX1_415.Children := 0;
  TypeDeclPSETPX1_415.Add(DW_AT_name, DW_FORM_string, 'PSETPX1'+#0);
  TypeDeclPSETPX1_415.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_416); // $BB, $15, $00, $00

  TypePtr_416 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_416.Tag := DW_TAG_pointer_type;
  TypePtr_416.Children := 0;
  TypePtr_416.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETPX1_356); // $C9, $13, $00, $00

  Type_417 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_417.Tag := DW_TAG_reference_type;
  Type_417.Children := 0;
  Type_417.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPX1_415); // $AE, $15, $00, $00

  TypeDeclPSETPB1_418 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSETPB1_418.Tag := DW_TAG_typedef;
  TypeDeclPSETPB1_418.Children := 0;
  TypeDeclPSETPB1_418.Add(DW_AT_name, DW_FORM_string, 'PSETPB1'+#0);
  TypeDeclPSETPB1_418.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_419); // $D2, $15, $00, $00

  TypePtr_419 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_419.Tag := DW_TAG_pointer_type;
  TypePtr_419.Children := 0;
  TypePtr_419.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETPB1_360); // $F1, $13, $00, $00

  Type_420 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_420.Tag := DW_TAG_reference_type;
  Type_420.Children := 0;
  Type_420.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPB1_418); // $C5, $15, $00, $00

  TypeDeclPSETPB2_421 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSETPB2_421.Tag := DW_TAG_typedef;
  TypeDeclPSETPB2_421.Children := 0;
  TypeDeclPSETPB2_421.Add(DW_AT_name, DW_FORM_string, 'PSETPB2'+#0);
  TypeDeclPSETPB2_421.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_422); // $E9, $15, $00, $00

  TypePtr_422 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_422.Tag := DW_TAG_pointer_type;
  TypePtr_422.Children := 0;
  TypePtr_422.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETPB2_364); // $1A, $14, $00, $00

  Type_423 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_423.Tag := DW_TAG_reference_type;
  Type_423.Children := 0;
  Type_423.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPB2_421); // $DC, $15, $00, $00

  TypeDeclPSETPC1_424 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSETPC1_424.Tag := DW_TAG_typedef;
  TypeDeclPSETPC1_424.Children := 0;
  TypeDeclPSETPC1_424.Add(DW_AT_name, DW_FORM_string, 'PSETPC1'+#0);
  TypeDeclPSETPC1_424.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_425); // $00, $16, $00, $00

  TypePtr_425 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_425.Tag := DW_TAG_pointer_type;
  TypePtr_425.Children := 0;
  TypePtr_425.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETPC1_368); // $43, $14, $00, $00

  Type_426 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_426.Tag := DW_TAG_reference_type;
  Type_426.Children := 0;
  Type_426.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPC1_424); // $F3, $15, $00, $00

  TypeDeclPSETPR3_427 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclPSETPR3_427.Tag := DW_TAG_typedef;
  TypeDeclPSETPR3_427.Children := 0;
  TypeDeclPSETPR3_427.Add(DW_AT_name, DW_FORM_string, 'PSETPR3'+#0);
  TypeDeclPSETPR3_427.AddRef(DW_AT_type, DW_FORM_ref4, @TypePtr_428); // $17, $16, $00, $00

  TypePtr_428 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypePtr_428.Tag := DW_TAG_pointer_type;
  TypePtr_428.Children := 0;
  TypePtr_428.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETPR3_372); // $6C, $14, $00, $00

  Type_429 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_429.Tag := DW_TAG_reference_type;
  Type_429.Children := 0;
  Type_429.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPR3_427); // $0A, $16, $00, $00

  Type_430 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_430.Tag := DW_TAG_enumeration_type;
  Type_430.Children := 1;
  Type_430.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeE4A_431 := Type_430.GetNewChild;
    TypeE4A_431.Tag := DW_TAG_enumerator;
    TypeE4A_431.Children := 0;
    TypeE4A_431.Add(DW_AT_name, DW_FORM_string, 'E4A'+#0);
    TypeE4A_431.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeE4B_432 := Type_430.GetNewChild;
    TypeE4B_432.Tag := DW_TAG_enumerator;
    TypeE4B_432.Children := 0;
    TypeE4B_432.Add(DW_AT_name, DW_FORM_string, 'E4B'+#0);
    TypeE4B_432.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeE4C_433 := Type_430.GetNewChild;
    TypeE4C_433.Tag := DW_TAG_enumerator;
    TypeE4C_433.Children := 0;
    TypeE4C_433.Add(DW_AT_name, DW_FORM_string, 'E4C'+#0);
    TypeE4C_433.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

    TypeE4D_434 := Type_430.GetNewChild;
    TypeE4D_434.Tag := DW_TAG_enumerator;
    TypeE4D_434.Children := 0;
    TypeE4D_434.Add(DW_AT_name, DW_FORM_string, 'E4D'+#0);
    TypeE4D_434.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

  Type_435 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_435.Tag := DW_TAG_reference_type;
  Type_435.Children := 0;
  Type_435.AddRef(DW_AT_type, DW_FORM_ref4, @Type_430); // $21, $16, $00, $00

  Type_436 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_436.Tag := DW_TAG_set_type;
  Type_436.Children := 0;
  Type_436.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  Type_436.AddRef(DW_AT_type, DW_FORM_ref4, @Type_437); // $54, $16, $00, $00

  Type_437 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_437.Tag := DW_TAG_subrange_type;
  Type_437.Children := 0;
  Type_437.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_437.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 3);
  Type_437.AddRef(DW_AT_type, DW_FORM_ref4, @Type_939); // $68, $2B, $00, $00

  Type_438 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_438.Tag := DW_TAG_reference_type;
  Type_438.Children := 0;
  Type_438.AddRef(DW_AT_type, DW_FORM_ref4, @Type_436); // $4D, $16, $00, $00

  Type_439 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_439.Tag := DW_TAG_set_type;
  Type_439.Children := 0;
  Type_439.Add(DW_AT_byte_size, DW_FORM_data2, [$20, $00]);
  Type_439.AddRef(DW_AT_type, DW_FORM_ref4, @Type_440); // $67, $16, $00, $00

  Type_440 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_440.Tag := DW_TAG_subrange_type;
  Type_440.Children := 0;
  Type_440.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_440.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 255);
  Type_440.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclCHAR_945); // $94, $2B, $00, $00

  Type_441 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_441.Tag := DW_TAG_reference_type;
  Type_441.Children := 0;
  Type_441.AddRef(DW_AT_type, DW_FORM_ref4, @Type_439); // $60, $16, $00, $00

  TypeDeclTRECBYTE_442 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECBYTE_442.Tag := DW_TAG_typedef;
  TypeDeclTRECBYTE_442.Children := 0;
  TypeDeclTRECBYTE_442.Add(DW_AT_name, DW_FORM_string, 'TRECBYTE'+#0);
  TypeDeclTRECBYTE_442.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECBYTE_443); // $82, $16, $00, $00

  TypeTRECBYTE_443 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECBYTE_443.Tag := DW_TAG_structure_type;
  TypeTRECBYTE_443.Children := 1;
  TypeTRECBYTE_443.Add(DW_AT_name, DW_FORM_string, 'TRECBYTE'+#0);
  TypeTRECBYTE_443.AddULEB(DW_AT_byte_size, DW_FORM_udata, 1);

    VarVARBYTE_444 := TypeTRECBYTE_443.GetNewChild;
    VarVARBYTE_444.Tag := DW_TAG_member;
    VarVARBYTE_444.Children := 0;
    VarVARBYTE_444.Add(DW_AT_name, DW_FORM_string, 'VARBYTE'+#0);
    VarVARBYTE_444.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARBYTE_444.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_141); // $6A, $0C, $00, $00

  Type_445 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_445.Tag := DW_TAG_reference_type;
  Type_445.Children := 0;
  Type_445.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECBYTE_442); // $74, $16, $00, $00

  TypeDeclTRECWORD_446 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECWORD_446.Tag := DW_TAG_typedef;
  TypeDeclTRECWORD_446.Children := 0;
  TypeDeclTRECWORD_446.Add(DW_AT_name, DW_FORM_string, 'TRECWORD'+#0);
  TypeDeclTRECWORD_446.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECWORD_447); // $B1, $16, $00, $00

  TypeTRECWORD_447 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECWORD_447.Tag := DW_TAG_structure_type;
  TypeTRECWORD_447.Children := 1;
  TypeTRECWORD_447.Add(DW_AT_name, DW_FORM_string, 'TRECWORD'+#0);
  TypeTRECWORD_447.AddULEB(DW_AT_byte_size, DW_FORM_udata, 2);

    VarVARWORD_448 := TypeTRECWORD_447.GetNewChild;
    VarVARWORD_448.Tag := DW_TAG_member;
    VarVARWORD_448.Children := 0;
    VarVARWORD_448.Add(DW_AT_name, DW_FORM_string, 'VARWORD'+#0);
    VarVARWORD_448.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARWORD_448.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_147); // $A0, $0C, $00, $00

  Type_449 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_449.Tag := DW_TAG_reference_type;
  Type_449.Children := 0;
  Type_449.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECWORD_446); // $A3, $16, $00, $00

  TypeDeclTRECLONG_450 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECLONG_450.Tag := DW_TAG_typedef;
  TypeDeclTRECLONG_450.Children := 0;
  TypeDeclTRECLONG_450.Add(DW_AT_name, DW_FORM_string, 'TRECLONG'+#0);
  TypeDeclTRECLONG_450.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECLONG_451); // $E0, $16, $00, $00

  TypeTRECLONG_451 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECLONG_451.Tag := DW_TAG_structure_type;
  TypeTRECLONG_451.Children := 1;
  TypeTRECLONG_451.Add(DW_AT_name, DW_FORM_string, 'TRECLONG'+#0);
  TypeTRECLONG_451.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARLONG_452 := TypeTRECLONG_451.GetNewChild;
    VarVARLONG_452.Tag := DW_TAG_member;
    VarVARLONG_452.Children := 0;
    VarVARLONG_452.Add(DW_AT_name, DW_FORM_string, 'VARLONG'+#0);
    VarVARLONG_452.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARLONG_452.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_153); // $D6, $0C, $00, $00

  Type_453 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_453.Tag := DW_TAG_reference_type;
  Type_453.Children := 0;
  Type_453.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECLONG_450); // $D2, $16, $00, $00

  TypeDeclTRECQWORD_454 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECQWORD_454.Tag := DW_TAG_typedef;
  TypeDeclTRECQWORD_454.Children := 0;
  TypeDeclTRECQWORD_454.Add(DW_AT_name, DW_FORM_string, 'TRECQWORD'+#0);
  TypeDeclTRECQWORD_454.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECQWORD_455); // $10, $17, $00, $00

  TypeTRECQWORD_455 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECQWORD_455.Tag := DW_TAG_structure_type;
  TypeTRECQWORD_455.Children := 1;
  TypeTRECQWORD_455.Add(DW_AT_name, DW_FORM_string, 'TRECQWORD'+#0);
  TypeTRECQWORD_455.AddULEB(DW_AT_byte_size, DW_FORM_udata, 8);

    VarVARQWORD_456 := TypeTRECQWORD_455.GetNewChild;
    VarVARQWORD_456.Tag := DW_TAG_member;
    VarVARQWORD_456.Children := 0;
    VarVARQWORD_456.Add(DW_AT_name, DW_FORM_string, 'VARQWORD'+#0);
    VarVARQWORD_456.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARQWORD_456.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_159); // $12, $0D, $00, $00

  Type_457 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_457.Tag := DW_TAG_reference_type;
  Type_457.Children := 0;
  Type_457.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECQWORD_454); // $01, $17, $00, $00

  TypeDeclTRECINT8_458 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECINT8_458.Tag := DW_TAG_typedef;
  TypeDeclTRECINT8_458.Children := 0;
  TypeDeclTRECINT8_458.Add(DW_AT_name, DW_FORM_string, 'TRECINT8'+#0);
  TypeDeclTRECINT8_458.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECINT8_459); // $41, $17, $00, $00

  TypeTRECINT8_459 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECINT8_459.Tag := DW_TAG_structure_type;
  TypeTRECINT8_459.Children := 1;
  TypeTRECINT8_459.Add(DW_AT_name, DW_FORM_string, 'TRECINT8'+#0);
  TypeTRECINT8_459.AddULEB(DW_AT_byte_size, DW_FORM_udata, 1);

    VarVARINT8_460 := TypeTRECINT8_459.GetNewChild;
    VarVARINT8_460.Tag := DW_TAG_member;
    VarVARINT8_460.Children := 0;
    VarVARINT8_460.Add(DW_AT_name, DW_FORM_string, 'VARINT8'+#0);
    VarVARINT8_460.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARINT8_460.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_144); // $81, $0C, $00, $00

  Type_461 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_461.Tag := DW_TAG_reference_type;
  Type_461.Children := 0;
  Type_461.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECINT8_458); // $33, $17, $00, $00

  TypeDeclTRECINT16_462 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECINT16_462.Tag := DW_TAG_typedef;
  TypeDeclTRECINT16_462.Children := 0;
  TypeDeclTRECINT16_462.Add(DW_AT_name, DW_FORM_string, 'TRECINT16'+#0);
  TypeDeclTRECINT16_462.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECINT16_463); // $71, $17, $00, $00

  TypeTRECINT16_463 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECINT16_463.Tag := DW_TAG_structure_type;
  TypeTRECINT16_463.Children := 1;
  TypeTRECINT16_463.Add(DW_AT_name, DW_FORM_string, 'TRECINT16'+#0);
  TypeTRECINT16_463.AddULEB(DW_AT_byte_size, DW_FORM_udata, 2);

    VarVARINT16_464 := TypeTRECINT16_463.GetNewChild;
    VarVARINT16_464.Tag := DW_TAG_member;
    VarVARINT16_464.Children := 0;
    VarVARINT16_464.Add(DW_AT_name, DW_FORM_string, 'VARINT16'+#0);
    VarVARINT16_464.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARINT16_464.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSMALLINT_150); // $B7, $0C, $00, $00

  Type_465 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_465.Tag := DW_TAG_reference_type;
  Type_465.Children := 0;
  Type_465.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECINT16_462); // $62, $17, $00, $00

  TypeDeclTRECINT32_466 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECINT32_466.Tag := DW_TAG_typedef;
  TypeDeclTRECINT32_466.Children := 0;
  TypeDeclTRECINT32_466.Add(DW_AT_name, DW_FORM_string, 'TRECINT32'+#0);
  TypeDeclTRECINT32_466.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECINT32_467); // $A3, $17, $00, $00

  TypeTRECINT32_467 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECINT32_467.Tag := DW_TAG_structure_type;
  TypeTRECINT32_467.Children := 1;
  TypeTRECINT32_467.Add(DW_AT_name, DW_FORM_string, 'TRECINT32'+#0);
  TypeTRECINT32_467.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARINT32_468 := TypeTRECINT32_467.GetNewChild;
    VarVARINT32_468.Tag := DW_TAG_member;
    VarVARINT32_468.Children := 0;
    VarVARINT32_468.Add(DW_AT_name, DW_FORM_string, 'VARINT32'+#0);
    VarVARINT32_468.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARINT32_468.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_156); // $F5, $0C, $00, $00

  Type_469 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_469.Tag := DW_TAG_reference_type;
  Type_469.Children := 0;
  Type_469.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECINT32_466); // $94, $17, $00, $00

  TypeDeclTRECINT64_470 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECINT64_470.Tag := DW_TAG_typedef;
  TypeDeclTRECINT64_470.Children := 0;
  TypeDeclTRECINT64_470.Add(DW_AT_name, DW_FORM_string, 'TRECINT64'+#0);
  TypeDeclTRECINT64_470.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECINT64_471); // $D5, $17, $00, $00

  TypeTRECINT64_471 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECINT64_471.Tag := DW_TAG_structure_type;
  TypeTRECINT64_471.Children := 1;
  TypeTRECINT64_471.Add(DW_AT_name, DW_FORM_string, 'TRECINT64'+#0);
  TypeTRECINT64_471.AddULEB(DW_AT_byte_size, DW_FORM_udata, 8);

    VarVARINT64_472 := TypeTRECINT64_471.GetNewChild;
    VarVARINT64_472.Tag := DW_TAG_member;
    VarVARINT64_472.Children := 0;
    VarVARINT64_472.Add(DW_AT_name, DW_FORM_string, 'VARINT64'+#0);
    VarVARINT64_472.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARINT64_472.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclINT64_162); // $2B, $0D, $00, $00

  Type_473 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_473.Tag := DW_TAG_reference_type;
  Type_473.Children := 0;
  Type_473.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECINT64_470); // $C6, $17, $00, $00

  TypeDeclTRECSUB1_474 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECSUB1_474.Tag := DW_TAG_typedef;
  TypeDeclTRECSUB1_474.Children := 0;
  TypeDeclTRECSUB1_474.Add(DW_AT_name, DW_FORM_string, 'TRECSUB1'+#0);
  TypeDeclTRECSUB1_474.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECSUB1_475); // $06, $18, $00, $00

  TypeTRECSUB1_475 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECSUB1_475.Tag := DW_TAG_structure_type;
  TypeTRECSUB1_475.Children := 1;
  TypeTRECSUB1_475.Add(DW_AT_name, DW_FORM_string, 'TRECSUB1'+#0);
  TypeTRECSUB1_475.AddULEB(DW_AT_byte_size, DW_FORM_udata, 1);

    VarVARSUB1_476 := TypeTRECSUB1_475.GetNewChild;
    VarVARSUB1_476.Tag := DW_TAG_member;
    VarVARSUB1_476.Children := 0;
    VarVARSUB1_476.Add(DW_AT_name, DW_FORM_string, 'VARSUB1'+#0);
    VarVARSUB1_476.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSUB1_476.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB1_192); // $1A, $0E, $00, $00

  Type_477 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_477.Tag := DW_TAG_reference_type;
  Type_477.Children := 0;
  Type_477.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECSUB1_474); // $F8, $17, $00, $00

  TypeDeclTRECSUB2_478 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECSUB2_478.Tag := DW_TAG_typedef;
  TypeDeclTRECSUB2_478.Children := 0;
  TypeDeclTRECSUB2_478.Add(DW_AT_name, DW_FORM_string, 'TRECSUB2'+#0);
  TypeDeclTRECSUB2_478.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECSUB2_479); // $35, $18, $00, $00

  TypeTRECSUB2_479 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECSUB2_479.Tag := DW_TAG_structure_type;
  TypeTRECSUB2_479.Children := 1;
  TypeTRECSUB2_479.Add(DW_AT_name, DW_FORM_string, 'TRECSUB2'+#0);
  TypeTRECSUB2_479.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARSUB2_480 := TypeTRECSUB2_479.GetNewChild;
    VarVARSUB2_480.Tag := DW_TAG_member;
    VarVARSUB2_480.Children := 0;
    VarVARSUB2_480.Add(DW_AT_name, DW_FORM_string, 'VARSUB2'+#0);
    VarVARSUB2_480.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSUB2_480.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB2_195); // $37, $0E, $00, $00

  Type_481 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_481.Tag := DW_TAG_reference_type;
  Type_481.Children := 0;
  Type_481.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECSUB2_478); // $27, $18, $00, $00

  TypeDeclTRECSUB3_482 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECSUB3_482.Tag := DW_TAG_typedef;
  TypeDeclTRECSUB3_482.Children := 0;
  TypeDeclTRECSUB3_482.Add(DW_AT_name, DW_FORM_string, 'TRECSUB3'+#0);
  TypeDeclTRECSUB3_482.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECSUB3_483); // $64, $18, $00, $00

  TypeTRECSUB3_483 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECSUB3_483.Tag := DW_TAG_structure_type;
  TypeTRECSUB3_483.Children := 1;
  TypeTRECSUB3_483.Add(DW_AT_name, DW_FORM_string, 'TRECSUB3'+#0);
  TypeTRECSUB3_483.AddULEB(DW_AT_byte_size, DW_FORM_udata, 1);

    VarVARSUB3_484 := TypeTRECSUB3_483.GetNewChild;
    VarVARSUB3_484.Tag := DW_TAG_member;
    VarVARSUB3_484.Children := 0;
    VarVARSUB3_484.Add(DW_AT_name, DW_FORM_string, 'VARSUB3'+#0);
    VarVARSUB3_484.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSUB3_484.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB3_198); // $57, $0E, $00, $00

  Type_485 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_485.Tag := DW_TAG_reference_type;
  Type_485.Children := 0;
  Type_485.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECSUB3_482); // $56, $18, $00, $00

  TypeDeclTRECSUB4_486 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECSUB4_486.Tag := DW_TAG_typedef;
  TypeDeclTRECSUB4_486.Children := 0;
  TypeDeclTRECSUB4_486.Add(DW_AT_name, DW_FORM_string, 'TRECSUB4'+#0);
  TypeDeclTRECSUB4_486.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECSUB4_487); // $93, $18, $00, $00

  TypeTRECSUB4_487 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECSUB4_487.Tag := DW_TAG_structure_type;
  TypeTRECSUB4_487.Children := 1;
  TypeTRECSUB4_487.Add(DW_AT_name, DW_FORM_string, 'TRECSUB4'+#0);
  TypeTRECSUB4_487.AddULEB(DW_AT_byte_size, DW_FORM_udata, 1);

    VarVARSUB4_488 := TypeTRECSUB4_487.GetNewChild;
    VarVARSUB4_488.Tag := DW_TAG_member;
    VarVARSUB4_488.Children := 0;
    VarVARSUB4_488.Add(DW_AT_name, DW_FORM_string, 'VARSUB4'+#0);
    VarVARSUB4_488.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSUB4_488.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB4_201); // $75, $0E, $00, $00

  Type_489 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_489.Tag := DW_TAG_reference_type;
  Type_489.Children := 0;
  Type_489.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECSUB4_486); // $85, $18, $00, $00

  TypeDeclTRECSUB5_490 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECSUB5_490.Tag := DW_TAG_typedef;
  TypeDeclTRECSUB5_490.Children := 0;
  TypeDeclTRECSUB5_490.Add(DW_AT_name, DW_FORM_string, 'TRECSUB5'+#0);
  TypeDeclTRECSUB5_490.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECSUB5_491); // $C2, $18, $00, $00

  TypeTRECSUB5_491 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECSUB5_491.Tag := DW_TAG_structure_type;
  TypeTRECSUB5_491.Children := 1;
  TypeTRECSUB5_491.Add(DW_AT_name, DW_FORM_string, 'TRECSUB5'+#0);
  TypeTRECSUB5_491.AddULEB(DW_AT_byte_size, DW_FORM_udata, 1);

    VarVARSUB5_492 := TypeTRECSUB5_491.GetNewChild;
    VarVARSUB5_492.Tag := DW_TAG_member;
    VarVARSUB5_492.Children := 0;
    VarVARSUB5_492.Add(DW_AT_name, DW_FORM_string, 'VARSUB5'+#0);
    VarVARSUB5_492.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSUB5_492.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB5_204); // $92, $0E, $00, $00

  Type_493 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_493.Tag := DW_TAG_reference_type;
  Type_493.Children := 0;
  Type_493.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECSUB5_490); // $B4, $18, $00, $00

  TypeDeclTRECPBYTE_494 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPBYTE_494.Tag := DW_TAG_typedef;
  TypeDeclTRECPBYTE_494.Children := 0;
  TypeDeclTRECPBYTE_494.Add(DW_AT_name, DW_FORM_string, 'TRECPBYTE'+#0);
  TypeDeclTRECPBYTE_494.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPBYTE_495); // $F2, $18, $00, $00

  TypeTRECPBYTE_495 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPBYTE_495.Tag := DW_TAG_structure_type;
  TypeTRECPBYTE_495.Children := 1;
  TypeTRECPBYTE_495.Add(DW_AT_name, DW_FORM_string, 'TRECPBYTE'+#0);
  TypeTRECPBYTE_495.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPBYTE_496 := TypeTRECPBYTE_495.GetNewChild;
    VarVARPBYTE_496.Tag := DW_TAG_member;
    VarVARPBYTE_496.Children := 0;
    VarVARPBYTE_496.Add(DW_AT_name, DW_FORM_string, 'VARPBYTE'+#0);
    VarVARPBYTE_496.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPBYTE_496.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPBYTE_168); // $61, $0D, $00, $00

  Type_497 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_497.Tag := DW_TAG_reference_type;
  Type_497.Children := 0;
  Type_497.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPBYTE_494); // $E3, $18, $00, $00

  TypeDeclTRECPWORD_498 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPWORD_498.Tag := DW_TAG_typedef;
  TypeDeclTRECPWORD_498.Children := 0;
  TypeDeclTRECPWORD_498.Add(DW_AT_name, DW_FORM_string, 'TRECPWORD'+#0);
  TypeDeclTRECPWORD_498.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPWORD_499); // $24, $19, $00, $00

  TypeTRECPWORD_499 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPWORD_499.Tag := DW_TAG_structure_type;
  TypeTRECPWORD_499.Children := 1;
  TypeTRECPWORD_499.Add(DW_AT_name, DW_FORM_string, 'TRECPWORD'+#0);
  TypeTRECPWORD_499.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPWORD_500 := TypeTRECPWORD_499.GetNewChild;
    VarVARPWORD_500.Tag := DW_TAG_member;
    VarVARPWORD_500.Children := 0;
    VarVARPWORD_500.Add(DW_AT_name, DW_FORM_string, 'VARPWORD'+#0);
    VarVARPWORD_500.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPWORD_500.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPWORD_171); // $76, $0D, $00, $00

  Type_501 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_501.Tag := DW_TAG_reference_type;
  Type_501.Children := 0;
  Type_501.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPWORD_498); // $15, $19, $00, $00

  TypeDeclTRECPLONG_502 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPLONG_502.Tag := DW_TAG_typedef;
  TypeDeclTRECPLONG_502.Children := 0;
  TypeDeclTRECPLONG_502.Add(DW_AT_name, DW_FORM_string, 'TRECPLONG'+#0);
  TypeDeclTRECPLONG_502.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPLONG_503); // $56, $19, $00, $00

  TypeTRECPLONG_503 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPLONG_503.Tag := DW_TAG_structure_type;
  TypeTRECPLONG_503.Children := 1;
  TypeTRECPLONG_503.Add(DW_AT_name, DW_FORM_string, 'TRECPLONG'+#0);
  TypeTRECPLONG_503.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPLONG_504 := TypeTRECPLONG_503.GetNewChild;
    VarVARPLONG_504.Tag := DW_TAG_member;
    VarVARPLONG_504.Children := 0;
    VarVARPLONG_504.Add(DW_AT_name, DW_FORM_string, 'VARPLONG'+#0);
    VarVARPLONG_504.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPLONG_504.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPLONGWORD_174); // $8B, $0D, $00, $00

  Type_505 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_505.Tag := DW_TAG_reference_type;
  Type_505.Children := 0;
  Type_505.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPLONG_502); // $47, $19, $00, $00

  TypeDeclTRECPQWORD_506 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPQWORD_506.Tag := DW_TAG_typedef;
  TypeDeclTRECPQWORD_506.Children := 0;
  TypeDeclTRECPQWORD_506.Add(DW_AT_name, DW_FORM_string, 'TRECPQWORD'+#0);
  TypeDeclTRECPQWORD_506.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPQWORD_507); // $89, $19, $00, $00

  TypeTRECPQWORD_507 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPQWORD_507.Tag := DW_TAG_structure_type;
  TypeTRECPQWORD_507.Children := 1;
  TypeTRECPQWORD_507.Add(DW_AT_name, DW_FORM_string, 'TRECPQWORD'+#0);
  TypeTRECPQWORD_507.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPQWORD_508 := TypeTRECPQWORD_507.GetNewChild;
    VarVARPQWORD_508.Tag := DW_TAG_member;
    VarVARPQWORD_508.Children := 0;
    VarVARPQWORD_508.Add(DW_AT_name, DW_FORM_string, 'VARPQWORD'+#0);
    VarVARPQWORD_508.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPQWORD_508.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPQWORD_177); // $A4, $0D, $00, $00

  Type_509 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_509.Tag := DW_TAG_reference_type;
  Type_509.Children := 0;
  Type_509.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPQWORD_506); // $79, $19, $00, $00

  TypeDeclTRECPINT8_510 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPINT8_510.Tag := DW_TAG_typedef;
  TypeDeclTRECPINT8_510.Children := 0;
  TypeDeclTRECPINT8_510.Add(DW_AT_name, DW_FORM_string, 'TRECPINT8'+#0);
  TypeDeclTRECPINT8_510.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPINT8_511); // $BD, $19, $00, $00

  TypeTRECPINT8_511 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPINT8_511.Tag := DW_TAG_structure_type;
  TypeTRECPINT8_511.Children := 1;
  TypeTRECPINT8_511.Add(DW_AT_name, DW_FORM_string, 'TRECPINT8'+#0);
  TypeTRECPINT8_511.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPINT8_512 := TypeTRECPINT8_511.GetNewChild;
    VarVARPINT8_512.Tag := DW_TAG_member;
    VarVARPINT8_512.Children := 0;
    VarVARPINT8_512.Add(DW_AT_name, DW_FORM_string, 'VARPINT8'+#0);
    VarVARPINT8_512.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPINT8_512.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSHORTINT_180); // $BA, $0D, $00, $00

  Type_513 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_513.Tag := DW_TAG_reference_type;
  Type_513.Children := 0;
  Type_513.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPINT8_510); // $AE, $19, $00, $00

  TypeDeclTRECPINT16_514 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPINT16_514.Tag := DW_TAG_typedef;
  TypeDeclTRECPINT16_514.Children := 0;
  TypeDeclTRECPINT16_514.Add(DW_AT_name, DW_FORM_string, 'TRECPINT16'+#0);
  TypeDeclTRECPINT16_514.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPINT16_515); // $F0, $19, $00, $00

  TypeTRECPINT16_515 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPINT16_515.Tag := DW_TAG_structure_type;
  TypeTRECPINT16_515.Children := 1;
  TypeTRECPINT16_515.Add(DW_AT_name, DW_FORM_string, 'TRECPINT16'+#0);
  TypeTRECPINT16_515.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPINT16_516 := TypeTRECPINT16_515.GetNewChild;
    VarVARPINT16_516.Tag := DW_TAG_member;
    VarVARPINT16_516.Children := 0;
    VarVARPINT16_516.Add(DW_AT_name, DW_FORM_string, 'VARPINT16'+#0);
    VarVARPINT16_516.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPINT16_516.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSMALLINT_183); // $D3, $0D, $00, $00

  Type_517 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_517.Tag := DW_TAG_reference_type;
  Type_517.Children := 0;
  Type_517.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPINT16_514); // $E0, $19, $00, $00

  TypeDeclTRECPINT32_518 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPINT32_518.Tag := DW_TAG_typedef;
  TypeDeclTRECPINT32_518.Children := 0;
  TypeDeclTRECPINT32_518.Add(DW_AT_name, DW_FORM_string, 'TRECPINT32'+#0);
  TypeDeclTRECPINT32_518.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPINT32_519); // $25, $1A, $00, $00

  TypeTRECPINT32_519 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPINT32_519.Tag := DW_TAG_structure_type;
  TypeTRECPINT32_519.Children := 1;
  TypeTRECPINT32_519.Add(DW_AT_name, DW_FORM_string, 'TRECPINT32'+#0);
  TypeTRECPINT32_519.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPINT32_520 := TypeTRECPINT32_519.GetNewChild;
    VarVARPINT32_520.Tag := DW_TAG_member;
    VarVARPINT32_520.Children := 0;
    VarVARPINT32_520.Add(DW_AT_name, DW_FORM_string, 'VARPINT32'+#0);
    VarVARPINT32_520.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPINT32_520.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTEGER_186); // $EC, $0D, $00, $00

  Type_521 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_521.Tag := DW_TAG_reference_type;
  Type_521.Children := 0;
  Type_521.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPINT32_518); // $15, $1A, $00, $00

  TypeDeclTRECPINT64_522 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPINT64_522.Tag := DW_TAG_typedef;
  TypeDeclTRECPINT64_522.Children := 0;
  TypeDeclTRECPINT64_522.Add(DW_AT_name, DW_FORM_string, 'TRECPINT64'+#0);
  TypeDeclTRECPINT64_522.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPINT64_523); // $5A, $1A, $00, $00

  TypeTRECPINT64_523 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPINT64_523.Tag := DW_TAG_structure_type;
  TypeTRECPINT64_523.Children := 1;
  TypeTRECPINT64_523.Add(DW_AT_name, DW_FORM_string, 'TRECPINT64'+#0);
  TypeTRECPINT64_523.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPINT64_524 := TypeTRECPINT64_523.GetNewChild;
    VarVARPINT64_524.Tag := DW_TAG_member;
    VarVARPINT64_524.Children := 0;
    VarVARPINT64_524.Add(DW_AT_name, DW_FORM_string, 'VARPINT64'+#0);
    VarVARPINT64_524.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPINT64_524.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINT64_189); // $04, $0E, $00, $00

  Type_525 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_525.Tag := DW_TAG_reference_type;
  Type_525.Children := 0;
  Type_525.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPINT64_522); // $4A, $1A, $00, $00

  TypeDeclTRECPSUB1_526 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSUB1_526.Tag := DW_TAG_typedef;
  TypeDeclTRECPSUB1_526.Children := 0;
  TypeDeclTRECPSUB1_526.Add(DW_AT_name, DW_FORM_string, 'TRECPSUB1'+#0);
  TypeDeclTRECPSUB1_526.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSUB1_527); // $8E, $1A, $00, $00

  TypeTRECPSUB1_527 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSUB1_527.Tag := DW_TAG_structure_type;
  TypeTRECPSUB1_527.Children := 1;
  TypeTRECPSUB1_527.Add(DW_AT_name, DW_FORM_string, 'TRECPSUB1'+#0);
  TypeTRECPSUB1_527.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSUB1_528 := TypeTRECPSUB1_527.GetNewChild;
    VarVARPSUB1_528.Tag := DW_TAG_member;
    VarVARPSUB1_528.Children := 0;
    VarVARPSUB1_528.Add(DW_AT_name, DW_FORM_string, 'VARPSUB1'+#0);
    VarVARPSUB1_528.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSUB1_528.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB1_207); // $AF, $0E, $00, $00

  Type_529 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_529.Tag := DW_TAG_reference_type;
  Type_529.Children := 0;
  Type_529.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSUB1_526); // $7F, $1A, $00, $00

  TypeDeclTRECPSUB2_530 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSUB2_530.Tag := DW_TAG_typedef;
  TypeDeclTRECPSUB2_530.Children := 0;
  TypeDeclTRECPSUB2_530.Add(DW_AT_name, DW_FORM_string, 'TRECPSUB2'+#0);
  TypeDeclTRECPSUB2_530.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSUB2_531); // $C0, $1A, $00, $00

  TypeTRECPSUB2_531 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSUB2_531.Tag := DW_TAG_structure_type;
  TypeTRECPSUB2_531.Children := 1;
  TypeTRECPSUB2_531.Add(DW_AT_name, DW_FORM_string, 'TRECPSUB2'+#0);
  TypeTRECPSUB2_531.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSUB2_532 := TypeTRECPSUB2_531.GetNewChild;
    VarVARPSUB2_532.Tag := DW_TAG_member;
    VarVARPSUB2_532.Children := 0;
    VarVARPSUB2_532.Add(DW_AT_name, DW_FORM_string, 'VARPSUB2'+#0);
    VarVARPSUB2_532.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSUB2_532.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB2_210); // $C4, $0E, $00, $00

  Type_533 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_533.Tag := DW_TAG_reference_type;
  Type_533.Children := 0;
  Type_533.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSUB2_530); // $B1, $1A, $00, $00

  TypeDeclTRECPSUB3_534 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSUB3_534.Tag := DW_TAG_typedef;
  TypeDeclTRECPSUB3_534.Children := 0;
  TypeDeclTRECPSUB3_534.Add(DW_AT_name, DW_FORM_string, 'TRECPSUB3'+#0);
  TypeDeclTRECPSUB3_534.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSUB3_535); // $F2, $1A, $00, $00

  TypeTRECPSUB3_535 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSUB3_535.Tag := DW_TAG_structure_type;
  TypeTRECPSUB3_535.Children := 1;
  TypeTRECPSUB3_535.Add(DW_AT_name, DW_FORM_string, 'TRECPSUB3'+#0);
  TypeTRECPSUB3_535.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSUB3_536 := TypeTRECPSUB3_535.GetNewChild;
    VarVARPSUB3_536.Tag := DW_TAG_member;
    VarVARPSUB3_536.Children := 0;
    VarVARPSUB3_536.Add(DW_AT_name, DW_FORM_string, 'VARPSUB3'+#0);
    VarVARPSUB3_536.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSUB3_536.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB3_213); // $D9, $0E, $00, $00

  Type_537 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_537.Tag := DW_TAG_reference_type;
  Type_537.Children := 0;
  Type_537.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSUB3_534); // $E3, $1A, $00, $00

  TypeDeclTRECPSUB4_538 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSUB4_538.Tag := DW_TAG_typedef;
  TypeDeclTRECPSUB4_538.Children := 0;
  TypeDeclTRECPSUB4_538.Add(DW_AT_name, DW_FORM_string, 'TRECPSUB4'+#0);
  TypeDeclTRECPSUB4_538.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSUB4_539); // $24, $1B, $00, $00

  TypeTRECPSUB4_539 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSUB4_539.Tag := DW_TAG_structure_type;
  TypeTRECPSUB4_539.Children := 1;
  TypeTRECPSUB4_539.Add(DW_AT_name, DW_FORM_string, 'TRECPSUB4'+#0);
  TypeTRECPSUB4_539.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSUB4_540 := TypeTRECPSUB4_539.GetNewChild;
    VarVARPSUB4_540.Tag := DW_TAG_member;
    VarVARPSUB4_540.Children := 0;
    VarVARPSUB4_540.Add(DW_AT_name, DW_FORM_string, 'VARPSUB4'+#0);
    VarVARPSUB4_540.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSUB4_540.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB4_216); // $EE, $0E, $00, $00

  Type_541 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_541.Tag := DW_TAG_reference_type;
  Type_541.Children := 0;
  Type_541.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSUB4_538); // $15, $1B, $00, $00

  TypeDeclTRECPSUB5_542 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSUB5_542.Tag := DW_TAG_typedef;
  TypeDeclTRECPSUB5_542.Children := 0;
  TypeDeclTRECPSUB5_542.Add(DW_AT_name, DW_FORM_string, 'TRECPSUB5'+#0);
  TypeDeclTRECPSUB5_542.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSUB5_543); // $56, $1B, $00, $00

  TypeTRECPSUB5_543 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSUB5_543.Tag := DW_TAG_structure_type;
  TypeTRECPSUB5_543.Children := 1;
  TypeTRECPSUB5_543.Add(DW_AT_name, DW_FORM_string, 'TRECPSUB5'+#0);
  TypeTRECPSUB5_543.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSUB5_544 := TypeTRECPSUB5_543.GetNewChild;
    VarVARPSUB5_544.Tag := DW_TAG_member;
    VarVARPSUB5_544.Children := 0;
    VarVARPSUB5_544.Add(DW_AT_name, DW_FORM_string, 'VARPSUB5'+#0);
    VarVARPSUB5_544.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSUB5_544.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB5_219); // $03, $0F, $00, $00

  Type_545 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_545.Tag := DW_TAG_reference_type;
  Type_545.Children := 0;
  Type_545.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSUB5_542); // $47, $1B, $00, $00

  TypeDeclTRECBOOLEAN_546 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECBOOLEAN_546.Tag := DW_TAG_typedef;
  TypeDeclTRECBOOLEAN_546.Children := 0;
  TypeDeclTRECBOOLEAN_546.Add(DW_AT_name, DW_FORM_string, 'TRECBOOLEAN'+#0);
  TypeDeclTRECBOOLEAN_546.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECBOOLEAN_547); // $8A, $1B, $00, $00

  TypeTRECBOOLEAN_547 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECBOOLEAN_547.Tag := DW_TAG_structure_type;
  TypeTRECBOOLEAN_547.Children := 1;
  TypeTRECBOOLEAN_547.Add(DW_AT_name, DW_FORM_string, 'TRECBOOLEAN'+#0);
  TypeTRECBOOLEAN_547.AddULEB(DW_AT_byte_size, DW_FORM_udata, 1);

    VarVARBOOLEAN_548 := TypeTRECBOOLEAN_547.GetNewChild;
    VarVARBOOLEAN_548.Tag := DW_TAG_member;
    VarVARBOOLEAN_548.Children := 0;
    VarVARBOOLEAN_548.Add(DW_AT_name, DW_FORM_string, 'VARBOOLEAN'+#0);
    VarVARBOOLEAN_548.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARBOOLEAN_548.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_165); // $44, $0D, $00, $00

  Type_549 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_549.Tag := DW_TAG_reference_type;
  Type_549.Children := 0;
  Type_549.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECBOOLEAN_546); // $79, $1B, $00, $00

  TypeDeclTRECPBOOLEAN_550 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPBOOLEAN_550.Tag := DW_TAG_typedef;
  TypeDeclTRECPBOOLEAN_550.Children := 0;
  TypeDeclTRECPBOOLEAN_550.Add(DW_AT_name, DW_FORM_string, 'TRECPBOOLEAN'+#0);
  TypeDeclTRECPBOOLEAN_550.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPBOOLEAN_551); // $C3, $1B, $00, $00

  TypeTRECPBOOLEAN_551 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPBOOLEAN_551.Tag := DW_TAG_structure_type;
  TypeTRECPBOOLEAN_551.Children := 1;
  TypeTRECPBOOLEAN_551.Add(DW_AT_name, DW_FORM_string, 'TRECPBOOLEAN'+#0);
  TypeTRECPBOOLEAN_551.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPBOOLEAN_552 := TypeTRECPBOOLEAN_551.GetNewChild;
    VarVARPBOOLEAN_552.Tag := DW_TAG_member;
    VarVARPBOOLEAN_552.Children := 0;
    VarVARPBOOLEAN_552.Add(DW_AT_name, DW_FORM_string, 'VARPBOOLEAN'+#0);
    VarVARPBOOLEAN_552.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPBOOLEAN_552.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPBOOLEAN_222); // $18, $0F, $00, $00

  Type_553 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_553.Tag := DW_TAG_reference_type;
  Type_553.Children := 0;
  Type_553.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPBOOLEAN_550); // $B1, $1B, $00, $00

  TypeDeclTRECENUM0_554 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECENUM0_554.Tag := DW_TAG_typedef;
  TypeDeclTRECENUM0_554.Children := 0;
  TypeDeclTRECENUM0_554.Add(DW_AT_name, DW_FORM_string, 'TRECENUM0'+#0);
  TypeDeclTRECENUM0_554.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECENUM0_555); // $FB, $1B, $00, $00

  TypeTRECENUM0_555 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECENUM0_555.Tag := DW_TAG_structure_type;
  TypeTRECENUM0_555.Children := 1;
  TypeTRECENUM0_555.Add(DW_AT_name, DW_FORM_string, 'TRECENUM0'+#0);
  TypeTRECENUM0_555.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARENUM0_556 := TypeTRECENUM0_555.GetNewChild;
    VarVARENUM0_556.Tag := DW_TAG_member;
    VarVARENUM0_556.Children := 0;
    VarVARENUM0_556.Add(DW_AT_name, DW_FORM_string, 'VARENUM0'+#0);
    VarVARENUM0_556.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARENUM0_556.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM0_225); // $30, $0F, $00, $00

  Type_557 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_557.Tag := DW_TAG_reference_type;
  Type_557.Children := 0;
  Type_557.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECENUM0_554); // $EC, $1B, $00, $00

  TypeDeclTRECENUM1_558 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECENUM1_558.Tag := DW_TAG_typedef;
  TypeDeclTRECENUM1_558.Children := 0;
  TypeDeclTRECENUM1_558.Add(DW_AT_name, DW_FORM_string, 'TRECENUM1'+#0);
  TypeDeclTRECENUM1_558.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECENUM1_559); // $2D, $1C, $00, $00

  TypeTRECENUM1_559 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECENUM1_559.Tag := DW_TAG_structure_type;
  TypeTRECENUM1_559.Children := 1;
  TypeTRECENUM1_559.Add(DW_AT_name, DW_FORM_string, 'TRECENUM1'+#0);
  TypeTRECENUM1_559.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARENUM1_560 := TypeTRECENUM1_559.GetNewChild;
    VarVARENUM1_560.Tag := DW_TAG_member;
    VarVARENUM1_560.Children := 0;
    VarVARENUM1_560.Add(DW_AT_name, DW_FORM_string, 'VARENUM1'+#0);
    VarVARENUM1_560.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARENUM1_560.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM1_229); // $54, $0F, $00, $00

  Type_561 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_561.Tag := DW_TAG_reference_type;
  Type_561.Children := 0;
  Type_561.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECENUM1_558); // $1E, $1C, $00, $00

  TypeDeclTRECENUM2_562 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECENUM2_562.Tag := DW_TAG_typedef;
  TypeDeclTRECENUM2_562.Children := 0;
  TypeDeclTRECENUM2_562.Add(DW_AT_name, DW_FORM_string, 'TRECENUM2'+#0);
  TypeDeclTRECENUM2_562.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECENUM2_563); // $5F, $1C, $00, $00

  TypeTRECENUM2_563 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECENUM2_563.Tag := DW_TAG_structure_type;
  TypeTRECENUM2_563.Children := 1;
  TypeTRECENUM2_563.Add(DW_AT_name, DW_FORM_string, 'TRECENUM2'+#0);
  TypeTRECENUM2_563.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARENUM2_564 := TypeTRECENUM2_563.GetNewChild;
    VarVARENUM2_564.Tag := DW_TAG_member;
    VarVARENUM2_564.Children := 0;
    VarVARENUM2_564.Add(DW_AT_name, DW_FORM_string, 'VARENUM2'+#0);
    VarVARENUM2_564.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARENUM2_564.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM2_235); // $8A, $0F, $00, $00

  Type_565 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_565.Tag := DW_TAG_reference_type;
  Type_565.Children := 0;
  Type_565.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECENUM2_562); // $50, $1C, $00, $00

  TypeDeclTRECENUM3_566 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECENUM3_566.Tag := DW_TAG_typedef;
  TypeDeclTRECENUM3_566.Children := 0;
  TypeDeclTRECENUM3_566.Add(DW_AT_name, DW_FORM_string, 'TRECENUM3'+#0);
  TypeDeclTRECENUM3_566.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECENUM3_567); // $91, $1C, $00, $00

  TypeTRECENUM3_567 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECENUM3_567.Tag := DW_TAG_structure_type;
  TypeTRECENUM3_567.Children := 1;
  TypeTRECENUM3_567.Add(DW_AT_name, DW_FORM_string, 'TRECENUM3'+#0);
  TypeTRECENUM3_567.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARENUM3_568 := TypeTRECENUM3_567.GetNewChild;
    VarVARENUM3_568.Tag := DW_TAG_member;
    VarVARENUM3_568.Children := 0;
    VarVARENUM3_568.Add(DW_AT_name, DW_FORM_string, 'VARENUM3'+#0);
    VarVARENUM3_568.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARENUM3_568.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM3_247); // $F6, $0F, $00, $00

  Type_569 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_569.Tag := DW_TAG_reference_type;
  Type_569.Children := 0;
  Type_569.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECENUM3_566); // $82, $1C, $00, $00

  TypeDeclTRECENUMX_570 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECENUMX_570.Tag := DW_TAG_typedef;
  TypeDeclTRECENUMX_570.Children := 0;
  TypeDeclTRECENUMX_570.Add(DW_AT_name, DW_FORM_string, 'TRECENUMX'+#0);
  TypeDeclTRECENUMX_570.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECENUMX_571); // $C3, $1C, $00, $00

  TypeTRECENUMX_571 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECENUMX_571.Tag := DW_TAG_structure_type;
  TypeTRECENUMX_571.Children := 1;
  TypeTRECENUMX_571.Add(DW_AT_name, DW_FORM_string, 'TRECENUMX'+#0);
  TypeTRECENUMX_571.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARENUMX_572 := TypeTRECENUMX_571.GetNewChild;
    VarVARENUMX_572.Tag := DW_TAG_member;
    VarVARENUMX_572.Children := 0;
    VarVARENUMX_572.Add(DW_AT_name, DW_FORM_string, 'VARENUMX'+#0);
    VarVARENUMX_572.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARENUMX_572.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMX_267); // $AA, $10, $00, $00

  Type_573 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_573.Tag := DW_TAG_reference_type;
  Type_573.Children := 0;
  Type_573.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECENUMX_570); // $B4, $1C, $00, $00

  TypeDeclTRECENUMR3_574 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECENUMR3_574.Tag := DW_TAG_typedef;
  TypeDeclTRECENUMR3_574.Children := 0;
  TypeDeclTRECENUMR3_574.Add(DW_AT_name, DW_FORM_string, 'TRECENUMR3'+#0);
  TypeDeclTRECENUMR3_574.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECENUMR3_575); // $F6, $1C, $00, $00

  TypeTRECENUMR3_575 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECENUMR3_575.Tag := DW_TAG_structure_type;
  TypeTRECENUMR3_575.Children := 1;
  TypeTRECENUMR3_575.Add(DW_AT_name, DW_FORM_string, 'TRECENUMR3'+#0);
  TypeTRECENUMR3_575.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARENUMR3_576 := TypeTRECENUMR3_575.GetNewChild;
    VarVARENUMR3_576.Tag := DW_TAG_member;
    VarVARENUMR3_576.Children := 0;
    VarVARENUMR3_576.Add(DW_AT_name, DW_FORM_string, 'VARENUMR3'+#0);
    VarVARENUMR3_576.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARENUMR3_576.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMR3_273); // $E0, $10, $00, $00

  Type_577 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_577.Tag := DW_TAG_reference_type;
  Type_577.Children := 0;
  Type_577.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECENUMR3_574); // $E6, $1C, $00, $00

  TypeDeclTRECPENUM0_578 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPENUM0_578.Tag := DW_TAG_typedef;
  TypeDeclTRECPENUM0_578.Children := 0;
  TypeDeclTRECPENUM0_578.Add(DW_AT_name, DW_FORM_string, 'TRECPENUM0'+#0);
  TypeDeclTRECPENUM0_578.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPENUM0_579); // $2B, $1D, $00, $00

  TypeTRECPENUM0_579 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPENUM0_579.Tag := DW_TAG_structure_type;
  TypeTRECPENUM0_579.Children := 1;
  TypeTRECPENUM0_579.Add(DW_AT_name, DW_FORM_string, 'TRECPENUM0'+#0);
  TypeTRECPENUM0_579.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPENUM0_580 := TypeTRECPENUM0_579.GetNewChild;
    VarVARPENUM0_580.Tag := DW_TAG_member;
    VarVARPENUM0_580.Children := 0;
    VarVARPENUM0_580.Add(DW_AT_name, DW_FORM_string, 'VARPENUM0'+#0);
    VarVARPENUM0_580.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPENUM0_580.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUM0_286); // $5B, $11, $00, $00

  Type_581 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_581.Tag := DW_TAG_reference_type;
  Type_581.Children := 0;
  Type_581.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPENUM0_578); // $1B, $1D, $00, $00

  TypeDeclTRECPENUM1_582 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPENUM1_582.Tag := DW_TAG_typedef;
  TypeDeclTRECPENUM1_582.Children := 0;
  TypeDeclTRECPENUM1_582.Add(DW_AT_name, DW_FORM_string, 'TRECPENUM1'+#0);
  TypeDeclTRECPENUM1_582.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPENUM1_583); // $60, $1D, $00, $00

  TypeTRECPENUM1_583 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPENUM1_583.Tag := DW_TAG_structure_type;
  TypeTRECPENUM1_583.Children := 1;
  TypeTRECPENUM1_583.Add(DW_AT_name, DW_FORM_string, 'TRECPENUM1'+#0);
  TypeTRECPENUM1_583.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPENUM1_584 := TypeTRECPENUM1_583.GetNewChild;
    VarVARPENUM1_584.Tag := DW_TAG_member;
    VarVARPENUM1_584.Children := 0;
    VarVARPENUM1_584.Add(DW_AT_name, DW_FORM_string, 'VARPENUM1'+#0);
    VarVARPENUM1_584.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPENUM1_584.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUM1_289); // $71, $11, $00, $00

  Type_585 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_585.Tag := DW_TAG_reference_type;
  Type_585.Children := 0;
  Type_585.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPENUM1_582); // $50, $1D, $00, $00

  TypeDeclTRECPENUM2_586 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPENUM2_586.Tag := DW_TAG_typedef;
  TypeDeclTRECPENUM2_586.Children := 0;
  TypeDeclTRECPENUM2_586.Add(DW_AT_name, DW_FORM_string, 'TRECPENUM2'+#0);
  TypeDeclTRECPENUM2_586.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPENUM2_587); // $95, $1D, $00, $00

  TypeTRECPENUM2_587 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPENUM2_587.Tag := DW_TAG_structure_type;
  TypeTRECPENUM2_587.Children := 1;
  TypeTRECPENUM2_587.Add(DW_AT_name, DW_FORM_string, 'TRECPENUM2'+#0);
  TypeTRECPENUM2_587.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPENUM2_588 := TypeTRECPENUM2_587.GetNewChild;
    VarVARPENUM2_588.Tag := DW_TAG_member;
    VarVARPENUM2_588.Children := 0;
    VarVARPENUM2_588.Add(DW_AT_name, DW_FORM_string, 'VARPENUM2'+#0);
    VarVARPENUM2_588.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPENUM2_588.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUM2_292); // $87, $11, $00, $00

  Type_589 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_589.Tag := DW_TAG_reference_type;
  Type_589.Children := 0;
  Type_589.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPENUM2_586); // $85, $1D, $00, $00

  TypeDeclTRECPENUM3_590 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPENUM3_590.Tag := DW_TAG_typedef;
  TypeDeclTRECPENUM3_590.Children := 0;
  TypeDeclTRECPENUM3_590.Add(DW_AT_name, DW_FORM_string, 'TRECPENUM3'+#0);
  TypeDeclTRECPENUM3_590.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPENUM3_591); // $CA, $1D, $00, $00

  TypeTRECPENUM3_591 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPENUM3_591.Tag := DW_TAG_structure_type;
  TypeTRECPENUM3_591.Children := 1;
  TypeTRECPENUM3_591.Add(DW_AT_name, DW_FORM_string, 'TRECPENUM3'+#0);
  TypeTRECPENUM3_591.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPENUM3_592 := TypeTRECPENUM3_591.GetNewChild;
    VarVARPENUM3_592.Tag := DW_TAG_member;
    VarVARPENUM3_592.Children := 0;
    VarVARPENUM3_592.Add(DW_AT_name, DW_FORM_string, 'VARPENUM3'+#0);
    VarVARPENUM3_592.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPENUM3_592.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUM3_295); // $9D, $11, $00, $00

  Type_593 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_593.Tag := DW_TAG_reference_type;
  Type_593.Children := 0;
  Type_593.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPENUM3_590); // $BA, $1D, $00, $00

  TypeDeclTRECPENUMX_594 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPENUMX_594.Tag := DW_TAG_typedef;
  TypeDeclTRECPENUMX_594.Children := 0;
  TypeDeclTRECPENUMX_594.Add(DW_AT_name, DW_FORM_string, 'TRECPENUMX'+#0);
  TypeDeclTRECPENUMX_594.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPENUMX_595); // $FF, $1D, $00, $00

  TypeTRECPENUMX_595 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPENUMX_595.Tag := DW_TAG_structure_type;
  TypeTRECPENUMX_595.Children := 1;
  TypeTRECPENUMX_595.Add(DW_AT_name, DW_FORM_string, 'TRECPENUMX'+#0);
  TypeTRECPENUMX_595.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPENUMX_596 := TypeTRECPENUMX_595.GetNewChild;
    VarVARPENUMX_596.Tag := DW_TAG_member;
    VarVARPENUMX_596.Children := 0;
    VarVARPENUMX_596.Add(DW_AT_name, DW_FORM_string, 'VARPENUMX'+#0);
    VarVARPENUMX_596.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPENUMX_596.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUMX_298); // $B3, $11, $00, $00

  Type_597 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_597.Tag := DW_TAG_reference_type;
  Type_597.Children := 0;
  Type_597.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPENUMX_594); // $EF, $1D, $00, $00

  TypeDeclTRECPENUMR3_598 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPENUMR3_598.Tag := DW_TAG_typedef;
  TypeDeclTRECPENUMR3_598.Children := 0;
  TypeDeclTRECPENUMR3_598.Add(DW_AT_name, DW_FORM_string, 'TRECPENUMR3'+#0);
  TypeDeclTRECPENUMR3_598.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPENUMR3_599); // $35, $1E, $00, $00

  TypeTRECPENUMR3_599 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPENUMR3_599.Tag := DW_TAG_structure_type;
  TypeTRECPENUMR3_599.Children := 1;
  TypeTRECPENUMR3_599.Add(DW_AT_name, DW_FORM_string, 'TRECPENUMR3'+#0);
  TypeTRECPENUMR3_599.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPENUMR3_600 := TypeTRECPENUMR3_599.GetNewChild;
    VarVARPENUMR3_600.Tag := DW_TAG_member;
    VarVARPENUMR3_600.Children := 0;
    VarVARPENUMR3_600.Add(DW_AT_name, DW_FORM_string, 'VARPENUMR3'+#0);
    VarVARPENUMR3_600.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPENUMR3_600.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUMR3_301); // $C9, $11, $00, $00

  Type_601 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_601.Tag := DW_TAG_reference_type;
  Type_601.Children := 0;
  Type_601.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPENUMR3_598); // $24, $1E, $00, $00

  TypeDeclTRECSET0_602 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECSET0_602.Tag := DW_TAG_typedef;
  TypeDeclTRECSET0_602.Children := 0;
  TypeDeclTRECSET0_602.Add(DW_AT_name, DW_FORM_string, 'TRECSET0'+#0);
  TypeDeclTRECSET0_602.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECSET0_603); // $6A, $1E, $00, $00

  TypeTRECSET0_603 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECSET0_603.Tag := DW_TAG_structure_type;
  TypeTRECSET0_603.Children := 1;
  TypeTRECSET0_603.Add(DW_AT_name, DW_FORM_string, 'TRECSET0'+#0);
  TypeTRECSET0_603.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARSET0_604 := TypeTRECSET0_603.GetNewChild;
    VarVARSET0_604.Tag := DW_TAG_member;
    VarVARSET0_604.Children := 0;
    VarVARSET0_604.Add(DW_AT_name, DW_FORM_string, 'VARSET0'+#0);
    VarVARSET0_604.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSET0_604.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET0_304); // $E0, $11, $00, $00

  Type_605 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_605.Tag := DW_TAG_reference_type;
  Type_605.Children := 0;
  Type_605.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECSET0_602); // $5C, $1E, $00, $00

  TypeDeclTRECSET1_606 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECSET1_606.Tag := DW_TAG_typedef;
  TypeDeclTRECSET1_606.Children := 0;
  TypeDeclTRECSET1_606.Add(DW_AT_name, DW_FORM_string, 'TRECSET1'+#0);
  TypeDeclTRECSET1_606.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECSET1_607); // $99, $1E, $00, $00

  TypeTRECSET1_607 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECSET1_607.Tag := DW_TAG_structure_type;
  TypeTRECSET1_607.Children := 1;
  TypeTRECSET1_607.Add(DW_AT_name, DW_FORM_string, 'TRECSET1'+#0);
  TypeTRECSET1_607.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARSET1_608 := TypeTRECSET1_607.GetNewChild;
    VarVARSET1_608.Tag := DW_TAG_member;
    VarVARSET1_608.Children := 0;
    VarVARSET1_608.Add(DW_AT_name, DW_FORM_string, 'VARSET1'+#0);
    VarVARSET1_608.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSET1_608.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET1_308); // $04, $12, $00, $00

  Type_609 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_609.Tag := DW_TAG_reference_type;
  Type_609.Children := 0;
  Type_609.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECSET1_606); // $8B, $1E, $00, $00

  TypeDeclTRECSET2_610 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECSET2_610.Tag := DW_TAG_typedef;
  TypeDeclTRECSET2_610.Children := 0;
  TypeDeclTRECSET2_610.Add(DW_AT_name, DW_FORM_string, 'TRECSET2'+#0);
  TypeDeclTRECSET2_610.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECSET2_611); // $C8, $1E, $00, $00

  TypeTRECSET2_611 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECSET2_611.Tag := DW_TAG_structure_type;
  TypeTRECSET2_611.Children := 1;
  TypeTRECSET2_611.Add(DW_AT_name, DW_FORM_string, 'TRECSET2'+#0);
  TypeTRECSET2_611.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARSET2_612 := TypeTRECSET2_611.GetNewChild;
    VarVARSET2_612.Tag := DW_TAG_member;
    VarVARSET2_612.Children := 0;
    VarVARSET2_612.Add(DW_AT_name, DW_FORM_string, 'VARSET2'+#0);
    VarVARSET2_612.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSET2_612.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET2_312); // $28, $12, $00, $00

  Type_613 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_613.Tag := DW_TAG_reference_type;
  Type_613.Children := 0;
  Type_613.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECSET2_610); // $BA, $1E, $00, $00

  TypeDeclTRECSET3_614 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECSET3_614.Tag := DW_TAG_typedef;
  TypeDeclTRECSET3_614.Children := 0;
  TypeDeclTRECSET3_614.Add(DW_AT_name, DW_FORM_string, 'TRECSET3'+#0);
  TypeDeclTRECSET3_614.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECSET3_615); // $F7, $1E, $00, $00

  TypeTRECSET3_615 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECSET3_615.Tag := DW_TAG_structure_type;
  TypeTRECSET3_615.Children := 1;
  TypeTRECSET3_615.Add(DW_AT_name, DW_FORM_string, 'TRECSET3'+#0);
  TypeTRECSET3_615.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARSET3_616 := TypeTRECSET3_615.GetNewChild;
    VarVARSET3_616.Tag := DW_TAG_member;
    VarVARSET3_616.Children := 0;
    VarVARSET3_616.Add(DW_AT_name, DW_FORM_string, 'VARSET3'+#0);
    VarVARSET3_616.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSET3_616.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET3_316); // $4C, $12, $00, $00

  Type_617 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_617.Tag := DW_TAG_reference_type;
  Type_617.Children := 0;
  Type_617.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECSET3_614); // $E9, $1E, $00, $00

  TypeDeclTRECSETX1_618 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECSETX1_618.Tag := DW_TAG_typedef;
  TypeDeclTRECSETX1_618.Children := 0;
  TypeDeclTRECSETX1_618.Add(DW_AT_name, DW_FORM_string, 'TRECSETX1'+#0);
  TypeDeclTRECSETX1_618.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECSETX1_619); // $27, $1F, $00, $00

  TypeTRECSETX1_619 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECSETX1_619.Tag := DW_TAG_structure_type;
  TypeTRECSETX1_619.Children := 1;
  TypeTRECSETX1_619.Add(DW_AT_name, DW_FORM_string, 'TRECSETX1'+#0);
  TypeTRECSETX1_619.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARSETX1_620 := TypeTRECSETX1_619.GetNewChild;
    VarVARSETX1_620.Tag := DW_TAG_member;
    VarVARSETX1_620.Children := 0;
    VarVARSETX1_620.Add(DW_AT_name, DW_FORM_string, 'VARSETX1'+#0);
    VarVARSETX1_620.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSETX1_620.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETX1_320); // $70, $12, $00, $00

  Type_621 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_621.Tag := DW_TAG_reference_type;
  Type_621.Children := 0;
  Type_621.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECSETX1_618); // $18, $1F, $00, $00

  TypeDeclTRECSETX2_622 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECSETX2_622.Tag := DW_TAG_typedef;
  TypeDeclTRECSETX2_622.Children := 0;
  TypeDeclTRECSETX2_622.Add(DW_AT_name, DW_FORM_string, 'TRECSETX2'+#0);
  TypeDeclTRECSETX2_622.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECSETX2_623); // $59, $1F, $00, $00

  TypeTRECSETX2_623 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECSETX2_623.Tag := DW_TAG_structure_type;
  TypeTRECSETX2_623.Children := 1;
  TypeTRECSETX2_623.Add(DW_AT_name, DW_FORM_string, 'TRECSETX2'+#0);
  TypeTRECSETX2_623.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARSETX2_624 := TypeTRECSETX2_623.GetNewChild;
    VarVARSETX2_624.Tag := DW_TAG_member;
    VarVARSETX2_624.Children := 0;
    VarVARSETX2_624.Add(DW_AT_name, DW_FORM_string, 'VARSETX2'+#0);
    VarVARSETX2_624.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSETX2_624.AddRef(DW_AT_type, DW_FORM_ref4, @Type_948); // $AB, $2B, $00, $00

  Type_625 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_625.Tag := DW_TAG_reference_type;
  Type_625.Children := 0;
  Type_625.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECSETX2_622); // $4A, $1F, $00, $00

  TypeDeclTRECSETB1_626 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECSETB1_626.Tag := DW_TAG_typedef;
  TypeDeclTRECSETB1_626.Children := 0;
  TypeDeclTRECSETB1_626.Add(DW_AT_name, DW_FORM_string, 'TRECSETB1'+#0);
  TypeDeclTRECSETB1_626.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECSETB1_627); // $8B, $1F, $00, $00

  TypeTRECSETB1_627 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECSETB1_627.Tag := DW_TAG_structure_type;
  TypeTRECSETB1_627.Children := 1;
  TypeTRECSETB1_627.Add(DW_AT_name, DW_FORM_string, 'TRECSETB1'+#0);
  TypeTRECSETB1_627.AddULEB(DW_AT_byte_size, DW_FORM_udata, 32);

    VarVARSETB1_628 := TypeTRECSETB1_627.GetNewChild;
    VarVARSETB1_628.Tag := DW_TAG_member;
    VarVARSETB1_628.Children := 0;
    VarVARSETB1_628.Add(DW_AT_name, DW_FORM_string, 'VARSETB1'+#0);
    VarVARSETB1_628.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSETB1_628.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETB1_324); // $96, $12, $00, $00

  Type_629 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_629.Tag := DW_TAG_reference_type;
  Type_629.Children := 0;
  Type_629.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECSETB1_626); // $7C, $1F, $00, $00

  TypeDeclTRECSETB2_630 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECSETB2_630.Tag := DW_TAG_typedef;
  TypeDeclTRECSETB2_630.Children := 0;
  TypeDeclTRECSETB2_630.Add(DW_AT_name, DW_FORM_string, 'TRECSETB2'+#0);
  TypeDeclTRECSETB2_630.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECSETB2_631); // $BD, $1F, $00, $00

  TypeTRECSETB2_631 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECSETB2_631.Tag := DW_TAG_structure_type;
  TypeTRECSETB2_631.Children := 1;
  TypeTRECSETB2_631.Add(DW_AT_name, DW_FORM_string, 'TRECSETB2'+#0);
  TypeTRECSETB2_631.AddULEB(DW_AT_byte_size, DW_FORM_udata, 32);

    VarVARSETB2_632 := TypeTRECSETB2_631.GetNewChild;
    VarVARSETB2_632.Tag := DW_TAG_member;
    VarVARSETB2_632.Children := 0;
    VarVARSETB2_632.Add(DW_AT_name, DW_FORM_string, 'VARSETB2'+#0);
    VarVARSETB2_632.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSETB2_632.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETB2_328); // $BD, $12, $00, $00

  Type_633 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_633.Tag := DW_TAG_reference_type;
  Type_633.Children := 0;
  Type_633.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECSETB2_630); // $AE, $1F, $00, $00

  TypeDeclTRECSETC1_634 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECSETC1_634.Tag := DW_TAG_typedef;
  TypeDeclTRECSETC1_634.Children := 0;
  TypeDeclTRECSETC1_634.Add(DW_AT_name, DW_FORM_string, 'TRECSETC1'+#0);
  TypeDeclTRECSETC1_634.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECSETC1_635); // $EF, $1F, $00, $00

  TypeTRECSETC1_635 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECSETC1_635.Tag := DW_TAG_structure_type;
  TypeTRECSETC1_635.Children := 1;
  TypeTRECSETC1_635.Add(DW_AT_name, DW_FORM_string, 'TRECSETC1'+#0);
  TypeTRECSETC1_635.AddULEB(DW_AT_byte_size, DW_FORM_udata, 32);

    VarVARSETC1_636 := TypeTRECSETC1_635.GetNewChild;
    VarVARSETC1_636.Tag := DW_TAG_member;
    VarVARSETC1_636.Children := 0;
    VarVARSETC1_636.Add(DW_AT_name, DW_FORM_string, 'VARSETC1'+#0);
    VarVARSETC1_636.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSETC1_636.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETC1_332); // $E4, $12, $00, $00

  Type_637 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_637.Tag := DW_TAG_reference_type;
  Type_637.Children := 0;
  Type_637.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECSETC1_634); // $E0, $1F, $00, $00

  TypeDeclTRECSETC2_638 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECSETC2_638.Tag := DW_TAG_typedef;
  TypeDeclTRECSETC2_638.Children := 0;
  TypeDeclTRECSETC2_638.Add(DW_AT_name, DW_FORM_string, 'TRECSETC2'+#0);
  TypeDeclTRECSETC2_638.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECSETC2_639); // $21, $20, $00, $00

  TypeTRECSETC2_639 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECSETC2_639.Tag := DW_TAG_structure_type;
  TypeTRECSETC2_639.Children := 1;
  TypeTRECSETC2_639.Add(DW_AT_name, DW_FORM_string, 'TRECSETC2'+#0);
  TypeTRECSETC2_639.AddULEB(DW_AT_byte_size, DW_FORM_udata, 32);

    VarVARSETC2_640 := TypeTRECSETC2_639.GetNewChild;
    VarVARSETC2_640.Tag := DW_TAG_member;
    VarVARSETC2_640.Children := 0;
    VarVARSETC2_640.Add(DW_AT_name, DW_FORM_string, 'VARSETC2'+#0);
    VarVARSETC2_640.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSETC2_640.AddRef(DW_AT_type, DW_FORM_ref4, @Type_951); // $BE, $2B, $00, $00

  Type_641 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_641.Tag := DW_TAG_reference_type;
  Type_641.Children := 0;
  Type_641.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECSETC2_638); // $12, $20, $00, $00

  TypeDeclTRECSETR3_642 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECSETR3_642.Tag := DW_TAG_typedef;
  TypeDeclTRECSETR3_642.Children := 0;
  TypeDeclTRECSETR3_642.Add(DW_AT_name, DW_FORM_string, 'TRECSETR3'+#0);
  TypeDeclTRECSETR3_642.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECSETR3_643); // $53, $20, $00, $00

  TypeTRECSETR3_643 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECSETR3_643.Tag := DW_TAG_structure_type;
  TypeTRECSETR3_643.Children := 1;
  TypeTRECSETR3_643.Add(DW_AT_name, DW_FORM_string, 'TRECSETR3'+#0);
  TypeTRECSETR3_643.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARSETR3_644 := TypeTRECSETR3_643.GetNewChild;
    VarVARSETR3_644.Tag := DW_TAG_member;
    VarVARSETR3_644.Children := 0;
    VarVARSETR3_644.Add(DW_AT_name, DW_FORM_string, 'VARSETR3'+#0);
    VarVARSETR3_644.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSETR3_644.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETR3_336); // $0B, $13, $00, $00

  Type_645 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_645.Tag := DW_TAG_reference_type;
  Type_645.Children := 0;
  Type_645.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECSETR3_642); // $44, $20, $00, $00

  TypeDeclTRECPSET0_646 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSET0_646.Tag := DW_TAG_typedef;
  TypeDeclTRECPSET0_646.Children := 0;
  TypeDeclTRECPSET0_646.Add(DW_AT_name, DW_FORM_string, 'TRECPSET0'+#0);
  TypeDeclTRECPSET0_646.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSET0_647); // $85, $20, $00, $00

  TypeTRECPSET0_647 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSET0_647.Tag := DW_TAG_structure_type;
  TypeTRECPSET0_647.Children := 1;
  TypeTRECPSET0_647.Add(DW_AT_name, DW_FORM_string, 'TRECPSET0'+#0);
  TypeTRECPSET0_647.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSET0_648 := TypeTRECPSET0_647.GetNewChild;
    VarVARPSET0_648.Tag := DW_TAG_member;
    VarVARPSET0_648.Children := 0;
    VarVARPSET0_648.Add(DW_AT_name, DW_FORM_string, 'VARPSET0'+#0);
    VarVARPSET0_648.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSET0_648.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSET0_376); // $94, $14, $00, $00

  Type_649 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_649.Tag := DW_TAG_reference_type;
  Type_649.Children := 0;
  Type_649.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSET0_646); // $76, $20, $00, $00

  TypeDeclTRECPSET1_650 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSET1_650.Tag := DW_TAG_typedef;
  TypeDeclTRECPSET1_650.Children := 0;
  TypeDeclTRECPSET1_650.Add(DW_AT_name, DW_FORM_string, 'TRECPSET1'+#0);
  TypeDeclTRECPSET1_650.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSET1_651); // $B7, $20, $00, $00

  TypeTRECPSET1_651 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSET1_651.Tag := DW_TAG_structure_type;
  TypeTRECPSET1_651.Children := 1;
  TypeTRECPSET1_651.Add(DW_AT_name, DW_FORM_string, 'TRECPSET1'+#0);
  TypeTRECPSET1_651.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSET1_652 := TypeTRECPSET1_651.GetNewChild;
    VarVARPSET1_652.Tag := DW_TAG_member;
    VarVARPSET1_652.Children := 0;
    VarVARPSET1_652.Add(DW_AT_name, DW_FORM_string, 'VARPSET1'+#0);
    VarVARPSET1_652.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSET1_652.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSET1_379); // $A9, $14, $00, $00

  Type_653 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_653.Tag := DW_TAG_reference_type;
  Type_653.Children := 0;
  Type_653.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSET1_650); // $A8, $20, $00, $00

  TypeDeclTRECPSET2_654 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSET2_654.Tag := DW_TAG_typedef;
  TypeDeclTRECPSET2_654.Children := 0;
  TypeDeclTRECPSET2_654.Add(DW_AT_name, DW_FORM_string, 'TRECPSET2'+#0);
  TypeDeclTRECPSET2_654.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSET2_655); // $E9, $20, $00, $00

  TypeTRECPSET2_655 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSET2_655.Tag := DW_TAG_structure_type;
  TypeTRECPSET2_655.Children := 1;
  TypeTRECPSET2_655.Add(DW_AT_name, DW_FORM_string, 'TRECPSET2'+#0);
  TypeTRECPSET2_655.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSET2_656 := TypeTRECPSET2_655.GetNewChild;
    VarVARPSET2_656.Tag := DW_TAG_member;
    VarVARPSET2_656.Children := 0;
    VarVARPSET2_656.Add(DW_AT_name, DW_FORM_string, 'VARPSET2'+#0);
    VarVARPSET2_656.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSET2_656.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSET2_382); // $BE, $14, $00, $00

  Type_657 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_657.Tag := DW_TAG_reference_type;
  Type_657.Children := 0;
  Type_657.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSET2_654); // $DA, $20, $00, $00

  TypeDeclTRECPSET3_658 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSET3_658.Tag := DW_TAG_typedef;
  TypeDeclTRECPSET3_658.Children := 0;
  TypeDeclTRECPSET3_658.Add(DW_AT_name, DW_FORM_string, 'TRECPSET3'+#0);
  TypeDeclTRECPSET3_658.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSET3_659); // $1B, $21, $00, $00

  TypeTRECPSET3_659 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSET3_659.Tag := DW_TAG_structure_type;
  TypeTRECPSET3_659.Children := 1;
  TypeTRECPSET3_659.Add(DW_AT_name, DW_FORM_string, 'TRECPSET3'+#0);
  TypeTRECPSET3_659.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSET3_660 := TypeTRECPSET3_659.GetNewChild;
    VarVARPSET3_660.Tag := DW_TAG_member;
    VarVARPSET3_660.Children := 0;
    VarVARPSET3_660.Add(DW_AT_name, DW_FORM_string, 'VARPSET3'+#0);
    VarVARPSET3_660.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSET3_660.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSET3_385); // $D3, $14, $00, $00

  Type_661 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_661.Tag := DW_TAG_reference_type;
  Type_661.Children := 0;
  Type_661.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSET3_658); // $0C, $21, $00, $00

  TypeDeclTRECPSETX1_662 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSETX1_662.Tag := DW_TAG_typedef;
  TypeDeclTRECPSETX1_662.Children := 0;
  TypeDeclTRECPSETX1_662.Add(DW_AT_name, DW_FORM_string, 'TRECPSETX1'+#0);
  TypeDeclTRECPSETX1_662.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSETX1_663); // $4E, $21, $00, $00

  TypeTRECPSETX1_663 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSETX1_663.Tag := DW_TAG_structure_type;
  TypeTRECPSETX1_663.Children := 1;
  TypeTRECPSETX1_663.Add(DW_AT_name, DW_FORM_string, 'TRECPSETX1'+#0);
  TypeTRECPSETX1_663.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETX1_664 := TypeTRECPSETX1_663.GetNewChild;
    VarVARPSETX1_664.Tag := DW_TAG_member;
    VarVARPSETX1_664.Children := 0;
    VarVARPSETX1_664.Add(DW_AT_name, DW_FORM_string, 'VARPSETX1'+#0);
    VarVARPSETX1_664.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETX1_664.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETX1_388); // $E8, $14, $00, $00

  Type_665 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_665.Tag := DW_TAG_reference_type;
  Type_665.Children := 0;
  Type_665.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSETX1_662); // $3E, $21, $00, $00

  TypeDeclTRECPSETB1_666 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSETB1_666.Tag := DW_TAG_typedef;
  TypeDeclTRECPSETB1_666.Children := 0;
  TypeDeclTRECPSETB1_666.Add(DW_AT_name, DW_FORM_string, 'TRECPSETB1'+#0);
  TypeDeclTRECPSETB1_666.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSETB1_667); // $83, $21, $00, $00

  TypeTRECPSETB1_667 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSETB1_667.Tag := DW_TAG_structure_type;
  TypeTRECPSETB1_667.Children := 1;
  TypeTRECPSETB1_667.Add(DW_AT_name, DW_FORM_string, 'TRECPSETB1'+#0);
  TypeTRECPSETB1_667.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETB1_668 := TypeTRECPSETB1_667.GetNewChild;
    VarVARPSETB1_668.Tag := DW_TAG_member;
    VarVARPSETB1_668.Children := 0;
    VarVARPSETB1_668.Add(DW_AT_name, DW_FORM_string, 'VARPSETB1'+#0);
    VarVARPSETB1_668.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETB1_668.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETB1_391); // $FE, $14, $00, $00

  Type_669 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_669.Tag := DW_TAG_reference_type;
  Type_669.Children := 0;
  Type_669.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSETB1_666); // $73, $21, $00, $00

  TypeDeclTRECPSETB2_670 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSETB2_670.Tag := DW_TAG_typedef;
  TypeDeclTRECPSETB2_670.Children := 0;
  TypeDeclTRECPSETB2_670.Add(DW_AT_name, DW_FORM_string, 'TRECPSETB2'+#0);
  TypeDeclTRECPSETB2_670.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSETB2_671); // $B8, $21, $00, $00

  TypeTRECPSETB2_671 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSETB2_671.Tag := DW_TAG_structure_type;
  TypeTRECPSETB2_671.Children := 1;
  TypeTRECPSETB2_671.Add(DW_AT_name, DW_FORM_string, 'TRECPSETB2'+#0);
  TypeTRECPSETB2_671.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETB2_672 := TypeTRECPSETB2_671.GetNewChild;
    VarVARPSETB2_672.Tag := DW_TAG_member;
    VarVARPSETB2_672.Children := 0;
    VarVARPSETB2_672.Add(DW_AT_name, DW_FORM_string, 'VARPSETB2'+#0);
    VarVARPSETB2_672.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETB2_672.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETB2_394); // $14, $15, $00, $00

  Type_673 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_673.Tag := DW_TAG_reference_type;
  Type_673.Children := 0;
  Type_673.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSETB2_670); // $A8, $21, $00, $00

  TypeDeclTRECPSETC1_674 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSETC1_674.Tag := DW_TAG_typedef;
  TypeDeclTRECPSETC1_674.Children := 0;
  TypeDeclTRECPSETC1_674.Add(DW_AT_name, DW_FORM_string, 'TRECPSETC1'+#0);
  TypeDeclTRECPSETC1_674.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSETC1_675); // $ED, $21, $00, $00

  TypeTRECPSETC1_675 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSETC1_675.Tag := DW_TAG_structure_type;
  TypeTRECPSETC1_675.Children := 1;
  TypeTRECPSETC1_675.Add(DW_AT_name, DW_FORM_string, 'TRECPSETC1'+#0);
  TypeTRECPSETC1_675.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETC1_676 := TypeTRECPSETC1_675.GetNewChild;
    VarVARPSETC1_676.Tag := DW_TAG_member;
    VarVARPSETC1_676.Children := 0;
    VarVARPSETC1_676.Add(DW_AT_name, DW_FORM_string, 'VARPSETC1'+#0);
    VarVARPSETC1_676.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETC1_676.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETC1_397); // $2A, $15, $00, $00

  Type_677 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_677.Tag := DW_TAG_reference_type;
  Type_677.Children := 0;
  Type_677.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSETC1_674); // $DD, $21, $00, $00

  TypeDeclTRECPSETR3_678 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSETR3_678.Tag := DW_TAG_typedef;
  TypeDeclTRECPSETR3_678.Children := 0;
  TypeDeclTRECPSETR3_678.Add(DW_AT_name, DW_FORM_string, 'TRECPSETR3'+#0);
  TypeDeclTRECPSETR3_678.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSETR3_679); // $22, $22, $00, $00

  TypeTRECPSETR3_679 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSETR3_679.Tag := DW_TAG_structure_type;
  TypeTRECPSETR3_679.Children := 1;
  TypeTRECPSETR3_679.Add(DW_AT_name, DW_FORM_string, 'TRECPSETR3'+#0);
  TypeTRECPSETR3_679.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETR3_680 := TypeTRECPSETR3_679.GetNewChild;
    VarVARPSETR3_680.Tag := DW_TAG_member;
    VarVARPSETR3_680.Children := 0;
    VarVARPSETR3_680.Add(DW_AT_name, DW_FORM_string, 'VARPSETR3'+#0);
    VarVARPSETR3_680.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETR3_680.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETR3_400); // $40, $15, $00, $00

  Type_681 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_681.Tag := DW_TAG_reference_type;
  Type_681.Children := 0;
  Type_681.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSETR3_678); // $12, $22, $00, $00

  TypeDeclTRECPSETP0_682 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSETP0_682.Tag := DW_TAG_typedef;
  TypeDeclTRECPSETP0_682.Children := 0;
  TypeDeclTRECPSETP0_682.Add(DW_AT_name, DW_FORM_string, 'TRECPSETP0'+#0);
  TypeDeclTRECPSETP0_682.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSETP0_683); // $57, $22, $00, $00

  TypeTRECPSETP0_683 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSETP0_683.Tag := DW_TAG_structure_type;
  TypeTRECPSETP0_683.Children := 1;
  TypeTRECPSETP0_683.Add(DW_AT_name, DW_FORM_string, 'TRECPSETP0'+#0);
  TypeTRECPSETP0_683.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETP0_684 := TypeTRECPSETP0_683.GetNewChild;
    VarVARPSETP0_684.Tag := DW_TAG_member;
    VarVARPSETP0_684.Children := 0;
    VarVARPSETP0_684.Add(DW_AT_name, DW_FORM_string, 'VARPSETP0'+#0);
    VarVARPSETP0_684.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETP0_684.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETP0_403); // $56, $15, $00, $00

  Type_685 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_685.Tag := DW_TAG_reference_type;
  Type_685.Children := 0;
  Type_685.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSETP0_682); // $47, $22, $00, $00

  TypeDeclTRECPSETP1_686 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSETP1_686.Tag := DW_TAG_typedef;
  TypeDeclTRECPSETP1_686.Children := 0;
  TypeDeclTRECPSETP1_686.Add(DW_AT_name, DW_FORM_string, 'TRECPSETP1'+#0);
  TypeDeclTRECPSETP1_686.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSETP1_687); // $8C, $22, $00, $00

  TypeTRECPSETP1_687 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSETP1_687.Tag := DW_TAG_structure_type;
  TypeTRECPSETP1_687.Children := 1;
  TypeTRECPSETP1_687.Add(DW_AT_name, DW_FORM_string, 'TRECPSETP1'+#0);
  TypeTRECPSETP1_687.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETP1_688 := TypeTRECPSETP1_687.GetNewChild;
    VarVARPSETP1_688.Tag := DW_TAG_member;
    VarVARPSETP1_688.Children := 0;
    VarVARPSETP1_688.Add(DW_AT_name, DW_FORM_string, 'VARPSETP1'+#0);
    VarVARPSETP1_688.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETP1_688.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETP1_406); // $6C, $15, $00, $00

  Type_689 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_689.Tag := DW_TAG_reference_type;
  Type_689.Children := 0;
  Type_689.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSETP1_686); // $7C, $22, $00, $00

  TypeDeclTRECPSETP2_690 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSETP2_690.Tag := DW_TAG_typedef;
  TypeDeclTRECPSETP2_690.Children := 0;
  TypeDeclTRECPSETP2_690.Add(DW_AT_name, DW_FORM_string, 'TRECPSETP2'+#0);
  TypeDeclTRECPSETP2_690.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSETP2_691); // $C1, $22, $00, $00

  TypeTRECPSETP2_691 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSETP2_691.Tag := DW_TAG_structure_type;
  TypeTRECPSETP2_691.Children := 1;
  TypeTRECPSETP2_691.Add(DW_AT_name, DW_FORM_string, 'TRECPSETP2'+#0);
  TypeTRECPSETP2_691.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETP2_692 := TypeTRECPSETP2_691.GetNewChild;
    VarVARPSETP2_692.Tag := DW_TAG_member;
    VarVARPSETP2_692.Children := 0;
    VarVARPSETP2_692.Add(DW_AT_name, DW_FORM_string, 'VARPSETP2'+#0);
    VarVARPSETP2_692.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETP2_692.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETP2_409); // $82, $15, $00, $00

  Type_693 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_693.Tag := DW_TAG_reference_type;
  Type_693.Children := 0;
  Type_693.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSETP2_690); // $B1, $22, $00, $00

  TypeDeclTRECPSETP3_694 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSETP3_694.Tag := DW_TAG_typedef;
  TypeDeclTRECPSETP3_694.Children := 0;
  TypeDeclTRECPSETP3_694.Add(DW_AT_name, DW_FORM_string, 'TRECPSETP3'+#0);
  TypeDeclTRECPSETP3_694.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSETP3_695); // $F6, $22, $00, $00

  TypeTRECPSETP3_695 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSETP3_695.Tag := DW_TAG_structure_type;
  TypeTRECPSETP3_695.Children := 1;
  TypeTRECPSETP3_695.Add(DW_AT_name, DW_FORM_string, 'TRECPSETP3'+#0);
  TypeTRECPSETP3_695.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETP3_696 := TypeTRECPSETP3_695.GetNewChild;
    VarVARPSETP3_696.Tag := DW_TAG_member;
    VarVARPSETP3_696.Children := 0;
    VarVARPSETP3_696.Add(DW_AT_name, DW_FORM_string, 'VARPSETP3'+#0);
    VarVARPSETP3_696.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETP3_696.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETP3_412); // $98, $15, $00, $00

  Type_697 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_697.Tag := DW_TAG_reference_type;
  Type_697.Children := 0;
  Type_697.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSETP3_694); // $E6, $22, $00, $00

  TypeDeclTRECPSETPX1_698 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSETPX1_698.Tag := DW_TAG_typedef;
  TypeDeclTRECPSETPX1_698.Children := 0;
  TypeDeclTRECPSETPX1_698.Add(DW_AT_name, DW_FORM_string, 'TRECPSETPX1'+#0);
  TypeDeclTRECPSETPX1_698.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSETPX1_699); // $2C, $23, $00, $00

  TypeTRECPSETPX1_699 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSETPX1_699.Tag := DW_TAG_structure_type;
  TypeTRECPSETPX1_699.Children := 1;
  TypeTRECPSETPX1_699.Add(DW_AT_name, DW_FORM_string, 'TRECPSETPX1'+#0);
  TypeTRECPSETPX1_699.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETPX1_700 := TypeTRECPSETPX1_699.GetNewChild;
    VarVARPSETPX1_700.Tag := DW_TAG_member;
    VarVARPSETPX1_700.Children := 0;
    VarVARPSETPX1_700.Add(DW_AT_name, DW_FORM_string, 'VARPSETPX1'+#0);
    VarVARPSETPX1_700.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETPX1_700.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPX1_415); // $AE, $15, $00, $00

  Type_701 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_701.Tag := DW_TAG_reference_type;
  Type_701.Children := 0;
  Type_701.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSETPX1_698); // $1B, $23, $00, $00

  TypeDeclTRECPSETPB1_702 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSETPB1_702.Tag := DW_TAG_typedef;
  TypeDeclTRECPSETPB1_702.Children := 0;
  TypeDeclTRECPSETPB1_702.Add(DW_AT_name, DW_FORM_string, 'TRECPSETPB1'+#0);
  TypeDeclTRECPSETPB1_702.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSETPB1_703); // $64, $23, $00, $00

  TypeTRECPSETPB1_703 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSETPB1_703.Tag := DW_TAG_structure_type;
  TypeTRECPSETPB1_703.Children := 1;
  TypeTRECPSETPB1_703.Add(DW_AT_name, DW_FORM_string, 'TRECPSETPB1'+#0);
  TypeTRECPSETPB1_703.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETPB1_704 := TypeTRECPSETPB1_703.GetNewChild;
    VarVARPSETPB1_704.Tag := DW_TAG_member;
    VarVARPSETPB1_704.Children := 0;
    VarVARPSETPB1_704.Add(DW_AT_name, DW_FORM_string, 'VARPSETPB1'+#0);
    VarVARPSETPB1_704.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETPB1_704.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPB1_418); // $C5, $15, $00, $00

  Type_705 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_705.Tag := DW_TAG_reference_type;
  Type_705.Children := 0;
  Type_705.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSETPB1_702); // $53, $23, $00, $00

  TypeDeclTRECPSETPB2_706 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSETPB2_706.Tag := DW_TAG_typedef;
  TypeDeclTRECPSETPB2_706.Children := 0;
  TypeDeclTRECPSETPB2_706.Add(DW_AT_name, DW_FORM_string, 'TRECPSETPB2'+#0);
  TypeDeclTRECPSETPB2_706.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSETPB2_707); // $9C, $23, $00, $00

  TypeTRECPSETPB2_707 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSETPB2_707.Tag := DW_TAG_structure_type;
  TypeTRECPSETPB2_707.Children := 1;
  TypeTRECPSETPB2_707.Add(DW_AT_name, DW_FORM_string, 'TRECPSETPB2'+#0);
  TypeTRECPSETPB2_707.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETPB2_708 := TypeTRECPSETPB2_707.GetNewChild;
    VarVARPSETPB2_708.Tag := DW_TAG_member;
    VarVARPSETPB2_708.Children := 0;
    VarVARPSETPB2_708.Add(DW_AT_name, DW_FORM_string, 'VARPSETPB2'+#0);
    VarVARPSETPB2_708.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETPB2_708.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPB2_421); // $DC, $15, $00, $00

  Type_709 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_709.Tag := DW_TAG_reference_type;
  Type_709.Children := 0;
  Type_709.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSETPB2_706); // $8B, $23, $00, $00

  TypeDeclTRECPSETPC1_710 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSETPC1_710.Tag := DW_TAG_typedef;
  TypeDeclTRECPSETPC1_710.Children := 0;
  TypeDeclTRECPSETPC1_710.Add(DW_AT_name, DW_FORM_string, 'TRECPSETPC1'+#0);
  TypeDeclTRECPSETPC1_710.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSETPC1_711); // $D4, $23, $00, $00

  TypeTRECPSETPC1_711 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSETPC1_711.Tag := DW_TAG_structure_type;
  TypeTRECPSETPC1_711.Children := 1;
  TypeTRECPSETPC1_711.Add(DW_AT_name, DW_FORM_string, 'TRECPSETPC1'+#0);
  TypeTRECPSETPC1_711.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETPC1_712 := TypeTRECPSETPC1_711.GetNewChild;
    VarVARPSETPC1_712.Tag := DW_TAG_member;
    VarVARPSETPC1_712.Children := 0;
    VarVARPSETPC1_712.Add(DW_AT_name, DW_FORM_string, 'VARPSETPC1'+#0);
    VarVARPSETPC1_712.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETPC1_712.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPC1_424); // $F3, $15, $00, $00

  Type_713 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_713.Tag := DW_TAG_reference_type;
  Type_713.Children := 0;
  Type_713.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSETPC1_710); // $C3, $23, $00, $00

  TypeDeclTRECPSETPR3_714 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclTRECPSETPR3_714.Tag := DW_TAG_typedef;
  TypeDeclTRECPSETPR3_714.Children := 0;
  TypeDeclTRECPSETPR3_714.Add(DW_AT_name, DW_FORM_string, 'TRECPSETPR3'+#0);
  TypeDeclTRECPSETPR3_714.AddRef(DW_AT_type, DW_FORM_ref4, @TypeTRECPSETPR3_715); // $0C, $24, $00, $00

  TypeTRECPSETPR3_715 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeTRECPSETPR3_715.Tag := DW_TAG_structure_type;
  TypeTRECPSETPR3_715.Children := 1;
  TypeTRECPSETPR3_715.Add(DW_AT_name, DW_FORM_string, 'TRECPSETPR3'+#0);
  TypeTRECPSETPR3_715.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETPR3_716 := TypeTRECPSETPR3_715.GetNewChild;
    VarVARPSETPR3_716.Tag := DW_TAG_member;
    VarVARPSETPR3_716.Children := 0;
    VarVARPSETPR3_716.Add(DW_AT_name, DW_FORM_string, 'VARPSETPR3'+#0);
    VarVARPSETPR3_716.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETPR3_716.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPR3_427); // $0A, $16, $00, $00

  Type_717 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_717.Tag := DW_TAG_reference_type;
  Type_717.Children := 0;
  Type_717.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTRECPSETPR3_714); // $FB, $23, $00, $00

  Type_718 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_718.Tag := DW_TAG_structure_type;
  Type_718.Children := 1;
  Type_718.AddULEB(DW_AT_byte_size, DW_FORM_udata, 1);

    VarVARBYTE_719 := Type_718.GetNewChild;
    VarVARBYTE_719.Tag := DW_TAG_member;
    VarVARBYTE_719.Children := 0;
    VarVARBYTE_719.Add(DW_AT_name, DW_FORM_string, 'VARBYTE'+#0);
    VarVARBYTE_719.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARBYTE_719.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_141); // $6A, $0C, $00, $00

  Type_720 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_720.Tag := DW_TAG_reference_type;
  Type_720.Children := 0;
  Type_720.AddRef(DW_AT_type, DW_FORM_ref4, @Type_718); // $33, $24, $00, $00

  Type_721 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_721.Tag := DW_TAG_structure_type;
  Type_721.Children := 1;
  Type_721.AddULEB(DW_AT_byte_size, DW_FORM_udata, 2);

    VarVARWORD_722 := Type_721.GetNewChild;
    VarVARWORD_722.Tag := DW_TAG_member;
    VarVARWORD_722.Children := 0;
    VarVARWORD_722.Add(DW_AT_name, DW_FORM_string, 'VARWORD'+#0);
    VarVARWORD_722.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARWORD_722.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclWORD_147); // $A0, $0C, $00, $00

  Type_723 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_723.Tag := DW_TAG_reference_type;
  Type_723.Children := 0;
  Type_723.AddRef(DW_AT_type, DW_FORM_ref4, @Type_721); // $4B, $24, $00, $00

  Type_724 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_724.Tag := DW_TAG_structure_type;
  Type_724.Children := 1;
  Type_724.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARLONG_725 := Type_724.GetNewChild;
    VarVARLONG_725.Tag := DW_TAG_member;
    VarVARLONG_725.Children := 0;
    VarVARLONG_725.Add(DW_AT_name, DW_FORM_string, 'VARLONG'+#0);
    VarVARLONG_725.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARLONG_725.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGWORD_153); // $D6, $0C, $00, $00

  Type_726 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_726.Tag := DW_TAG_reference_type;
  Type_726.Children := 0;
  Type_726.AddRef(DW_AT_type, DW_FORM_ref4, @Type_724); // $63, $24, $00, $00

  Type_727 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_727.Tag := DW_TAG_structure_type;
  Type_727.Children := 1;
  Type_727.AddULEB(DW_AT_byte_size, DW_FORM_udata, 8);

    VarVARQWORD_728 := Type_727.GetNewChild;
    VarVARQWORD_728.Tag := DW_TAG_member;
    VarVARQWORD_728.Children := 0;
    VarVARQWORD_728.Add(DW_AT_name, DW_FORM_string, 'VARQWORD'+#0);
    VarVARQWORD_728.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARQWORD_728.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclQWORD_159); // $12, $0D, $00, $00

  Type_729 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_729.Tag := DW_TAG_reference_type;
  Type_729.Children := 0;
  Type_729.AddRef(DW_AT_type, DW_FORM_ref4, @Type_727); // $7B, $24, $00, $00

  Type_730 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_730.Tag := DW_TAG_structure_type;
  Type_730.Children := 1;
  Type_730.AddULEB(DW_AT_byte_size, DW_FORM_udata, 1);

    VarVARINT8_731 := Type_730.GetNewChild;
    VarVARINT8_731.Tag := DW_TAG_member;
    VarVARINT8_731.Children := 0;
    VarVARINT8_731.Add(DW_AT_name, DW_FORM_string, 'VARINT8'+#0);
    VarVARINT8_731.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARINT8_731.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSHORTINT_144); // $81, $0C, $00, $00

  Type_732 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_732.Tag := DW_TAG_reference_type;
  Type_732.Children := 0;
  Type_732.AddRef(DW_AT_type, DW_FORM_ref4, @Type_730); // $94, $24, $00, $00

  Type_733 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_733.Tag := DW_TAG_structure_type;
  Type_733.Children := 1;
  Type_733.AddULEB(DW_AT_byte_size, DW_FORM_udata, 2);

    VarVARINT16_734 := Type_733.GetNewChild;
    VarVARINT16_734.Tag := DW_TAG_member;
    VarVARINT16_734.Children := 0;
    VarVARINT16_734.Add(DW_AT_name, DW_FORM_string, 'VARINT16'+#0);
    VarVARINT16_734.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARINT16_734.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclSMALLINT_150); // $B7, $0C, $00, $00

  Type_735 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_735.Tag := DW_TAG_reference_type;
  Type_735.Children := 0;
  Type_735.AddRef(DW_AT_type, DW_FORM_ref4, @Type_733); // $AC, $24, $00, $00

  Type_736 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_736.Tag := DW_TAG_structure_type;
  Type_736.Children := 1;
  Type_736.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARINT32_737 := Type_736.GetNewChild;
    VarVARINT32_737.Tag := DW_TAG_member;
    VarVARINT32_737.Children := 0;
    VarVARINT32_737.Add(DW_AT_name, DW_FORM_string, 'VARINT32'+#0);
    VarVARINT32_737.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARINT32_737.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclLONGINT_156); // $F5, $0C, $00, $00

  Type_738 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_738.Tag := DW_TAG_reference_type;
  Type_738.Children := 0;
  Type_738.AddRef(DW_AT_type, DW_FORM_ref4, @Type_736); // $C5, $24, $00, $00

  Type_739 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_739.Tag := DW_TAG_structure_type;
  Type_739.Children := 1;
  Type_739.AddULEB(DW_AT_byte_size, DW_FORM_udata, 8);

    VarVARINT64_740 := Type_739.GetNewChild;
    VarVARINT64_740.Tag := DW_TAG_member;
    VarVARINT64_740.Children := 0;
    VarVARINT64_740.Add(DW_AT_name, DW_FORM_string, 'VARINT64'+#0);
    VarVARINT64_740.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARINT64_740.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclINT64_162); // $2B, $0D, $00, $00

  Type_741 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_741.Tag := DW_TAG_reference_type;
  Type_741.Children := 0;
  Type_741.AddRef(DW_AT_type, DW_FORM_ref4, @Type_739); // $DE, $24, $00, $00

  Type_742 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_742.Tag := DW_TAG_structure_type;
  Type_742.Children := 1;
  Type_742.AddULEB(DW_AT_byte_size, DW_FORM_udata, 1);

    VarVARSUB1_743 := Type_742.GetNewChild;
    VarVARSUB1_743.Tag := DW_TAG_member;
    VarVARSUB1_743.Children := 0;
    VarVARSUB1_743.Add(DW_AT_name, DW_FORM_string, 'VARSUB1'+#0);
    VarVARSUB1_743.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSUB1_743.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB1_192); // $1A, $0E, $00, $00

  Type_744 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_744.Tag := DW_TAG_reference_type;
  Type_744.Children := 0;
  Type_744.AddRef(DW_AT_type, DW_FORM_ref4, @Type_742); // $F7, $24, $00, $00

  Type_745 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_745.Tag := DW_TAG_structure_type;
  Type_745.Children := 1;
  Type_745.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARSUB2_746 := Type_745.GetNewChild;
    VarVARSUB2_746.Tag := DW_TAG_member;
    VarVARSUB2_746.Children := 0;
    VarVARSUB2_746.Add(DW_AT_name, DW_FORM_string, 'VARSUB2'+#0);
    VarVARSUB2_746.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSUB2_746.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB2_195); // $37, $0E, $00, $00

  Type_747 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_747.Tag := DW_TAG_reference_type;
  Type_747.Children := 0;
  Type_747.AddRef(DW_AT_type, DW_FORM_ref4, @Type_745); // $0F, $25, $00, $00

  Type_748 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_748.Tag := DW_TAG_structure_type;
  Type_748.Children := 1;
  Type_748.AddULEB(DW_AT_byte_size, DW_FORM_udata, 1);

    VarVARSUB3_749 := Type_748.GetNewChild;
    VarVARSUB3_749.Tag := DW_TAG_member;
    VarVARSUB3_749.Children := 0;
    VarVARSUB3_749.Add(DW_AT_name, DW_FORM_string, 'VARSUB3'+#0);
    VarVARSUB3_749.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSUB3_749.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB3_198); // $57, $0E, $00, $00

  Type_750 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_750.Tag := DW_TAG_reference_type;
  Type_750.Children := 0;
  Type_750.AddRef(DW_AT_type, DW_FORM_ref4, @Type_748); // $27, $25, $00, $00

  Type_751 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_751.Tag := DW_TAG_structure_type;
  Type_751.Children := 1;
  Type_751.AddULEB(DW_AT_byte_size, DW_FORM_udata, 1);

    VarVARSUB4_752 := Type_751.GetNewChild;
    VarVARSUB4_752.Tag := DW_TAG_member;
    VarVARSUB4_752.Children := 0;
    VarVARSUB4_752.Add(DW_AT_name, DW_FORM_string, 'VARSUB4'+#0);
    VarVARSUB4_752.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSUB4_752.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB4_201); // $75, $0E, $00, $00

  Type_753 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_753.Tag := DW_TAG_reference_type;
  Type_753.Children := 0;
  Type_753.AddRef(DW_AT_type, DW_FORM_ref4, @Type_751); // $3F, $25, $00, $00

  Type_754 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_754.Tag := DW_TAG_structure_type;
  Type_754.Children := 1;
  Type_754.AddULEB(DW_AT_byte_size, DW_FORM_udata, 1);

    VarVARSUB5_755 := Type_754.GetNewChild;
    VarVARSUB5_755.Tag := DW_TAG_member;
    VarVARSUB5_755.Children := 0;
    VarVARSUB5_755.Add(DW_AT_name, DW_FORM_string, 'VARSUB5'+#0);
    VarVARSUB5_755.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSUB5_755.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSUB5_204); // $92, $0E, $00, $00

  Type_756 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_756.Tag := DW_TAG_reference_type;
  Type_756.Children := 0;
  Type_756.AddRef(DW_AT_type, DW_FORM_ref4, @Type_754); // $57, $25, $00, $00

  Type_757 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_757.Tag := DW_TAG_structure_type;
  Type_757.Children := 1;
  Type_757.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPBYTE_758 := Type_757.GetNewChild;
    VarVARPBYTE_758.Tag := DW_TAG_member;
    VarVARPBYTE_758.Children := 0;
    VarVARPBYTE_758.Add(DW_AT_name, DW_FORM_string, 'VARPBYTE'+#0);
    VarVARPBYTE_758.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPBYTE_758.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPBYTE_168); // $61, $0D, $00, $00

  Type_759 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_759.Tag := DW_TAG_reference_type;
  Type_759.Children := 0;
  Type_759.AddRef(DW_AT_type, DW_FORM_ref4, @Type_757); // $6F, $25, $00, $00

  Type_760 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_760.Tag := DW_TAG_structure_type;
  Type_760.Children := 1;
  Type_760.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPWORD_761 := Type_760.GetNewChild;
    VarVARPWORD_761.Tag := DW_TAG_member;
    VarVARPWORD_761.Children := 0;
    VarVARPWORD_761.Add(DW_AT_name, DW_FORM_string, 'VARPWORD'+#0);
    VarVARPWORD_761.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPWORD_761.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPWORD_171); // $76, $0D, $00, $00

  Type_762 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_762.Tag := DW_TAG_reference_type;
  Type_762.Children := 0;
  Type_762.AddRef(DW_AT_type, DW_FORM_ref4, @Type_760); // $88, $25, $00, $00

  Type_763 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_763.Tag := DW_TAG_structure_type;
  Type_763.Children := 1;
  Type_763.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPLONG_764 := Type_763.GetNewChild;
    VarVARPLONG_764.Tag := DW_TAG_member;
    VarVARPLONG_764.Children := 0;
    VarVARPLONG_764.Add(DW_AT_name, DW_FORM_string, 'VARPLONG'+#0);
    VarVARPLONG_764.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPLONG_764.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPLONGWORD_174); // $8B, $0D, $00, $00

  Type_765 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_765.Tag := DW_TAG_reference_type;
  Type_765.Children := 0;
  Type_765.AddRef(DW_AT_type, DW_FORM_ref4, @Type_763); // $A1, $25, $00, $00

  Type_766 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_766.Tag := DW_TAG_structure_type;
  Type_766.Children := 1;
  Type_766.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPQWORD_767 := Type_766.GetNewChild;
    VarVARPQWORD_767.Tag := DW_TAG_member;
    VarVARPQWORD_767.Children := 0;
    VarVARPQWORD_767.Add(DW_AT_name, DW_FORM_string, 'VARPQWORD'+#0);
    VarVARPQWORD_767.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPQWORD_767.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPQWORD_177); // $A4, $0D, $00, $00

  Type_768 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_768.Tag := DW_TAG_reference_type;
  Type_768.Children := 0;
  Type_768.AddRef(DW_AT_type, DW_FORM_ref4, @Type_766); // $BA, $25, $00, $00

  Type_769 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_769.Tag := DW_TAG_structure_type;
  Type_769.Children := 1;
  Type_769.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPINT8_770 := Type_769.GetNewChild;
    VarVARPINT8_770.Tag := DW_TAG_member;
    VarVARPINT8_770.Children := 0;
    VarVARPINT8_770.Add(DW_AT_name, DW_FORM_string, 'VARPINT8'+#0);
    VarVARPINT8_770.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPINT8_770.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSHORTINT_180); // $BA, $0D, $00, $00

  Type_771 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_771.Tag := DW_TAG_reference_type;
  Type_771.Children := 0;
  Type_771.AddRef(DW_AT_type, DW_FORM_ref4, @Type_769); // $D4, $25, $00, $00

  Type_772 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_772.Tag := DW_TAG_structure_type;
  Type_772.Children := 1;
  Type_772.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPINT16_773 := Type_772.GetNewChild;
    VarVARPINT16_773.Tag := DW_TAG_member;
    VarVARPINT16_773.Children := 0;
    VarVARPINT16_773.Add(DW_AT_name, DW_FORM_string, 'VARPINT16'+#0);
    VarVARPINT16_773.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPINT16_773.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSMALLINT_183); // $D3, $0D, $00, $00

  Type_774 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_774.Tag := DW_TAG_reference_type;
  Type_774.Children := 0;
  Type_774.AddRef(DW_AT_type, DW_FORM_ref4, @Type_772); // $ED, $25, $00, $00

  Type_775 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_775.Tag := DW_TAG_structure_type;
  Type_775.Children := 1;
  Type_775.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPINT32_776 := Type_775.GetNewChild;
    VarVARPINT32_776.Tag := DW_TAG_member;
    VarVARPINT32_776.Children := 0;
    VarVARPINT32_776.Add(DW_AT_name, DW_FORM_string, 'VARPINT32'+#0);
    VarVARPINT32_776.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPINT32_776.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINTEGER_186); // $EC, $0D, $00, $00

  Type_777 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_777.Tag := DW_TAG_reference_type;
  Type_777.Children := 0;
  Type_777.AddRef(DW_AT_type, DW_FORM_ref4, @Type_775); // $07, $26, $00, $00

  Type_778 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_778.Tag := DW_TAG_structure_type;
  Type_778.Children := 1;
  Type_778.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPINT64_779 := Type_778.GetNewChild;
    VarVARPINT64_779.Tag := DW_TAG_member;
    VarVARPINT64_779.Children := 0;
    VarVARPINT64_779.Add(DW_AT_name, DW_FORM_string, 'VARPINT64'+#0);
    VarVARPINT64_779.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPINT64_779.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPINT64_189); // $04, $0E, $00, $00

  Type_780 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_780.Tag := DW_TAG_reference_type;
  Type_780.Children := 0;
  Type_780.AddRef(DW_AT_type, DW_FORM_ref4, @Type_778); // $21, $26, $00, $00

  Type_781 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_781.Tag := DW_TAG_structure_type;
  Type_781.Children := 1;
  Type_781.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSUB1_782 := Type_781.GetNewChild;
    VarVARPSUB1_782.Tag := DW_TAG_member;
    VarVARPSUB1_782.Children := 0;
    VarVARPSUB1_782.Add(DW_AT_name, DW_FORM_string, 'VARPSUB1'+#0);
    VarVARPSUB1_782.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSUB1_782.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB1_207); // $AF, $0E, $00, $00

  Type_783 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_783.Tag := DW_TAG_reference_type;
  Type_783.Children := 0;
  Type_783.AddRef(DW_AT_type, DW_FORM_ref4, @Type_781); // $3B, $26, $00, $00

  Type_784 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_784.Tag := DW_TAG_structure_type;
  Type_784.Children := 1;
  Type_784.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSUB2_785 := Type_784.GetNewChild;
    VarVARPSUB2_785.Tag := DW_TAG_member;
    VarVARPSUB2_785.Children := 0;
    VarVARPSUB2_785.Add(DW_AT_name, DW_FORM_string, 'VARPSUB2'+#0);
    VarVARPSUB2_785.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSUB2_785.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB2_210); // $C4, $0E, $00, $00

  Type_786 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_786.Tag := DW_TAG_reference_type;
  Type_786.Children := 0;
  Type_786.AddRef(DW_AT_type, DW_FORM_ref4, @Type_784); // $54, $26, $00, $00

  Type_787 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_787.Tag := DW_TAG_structure_type;
  Type_787.Children := 1;
  Type_787.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSUB3_788 := Type_787.GetNewChild;
    VarVARPSUB3_788.Tag := DW_TAG_member;
    VarVARPSUB3_788.Children := 0;
    VarVARPSUB3_788.Add(DW_AT_name, DW_FORM_string, 'VARPSUB3'+#0);
    VarVARPSUB3_788.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSUB3_788.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB3_213); // $D9, $0E, $00, $00

  Type_789 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_789.Tag := DW_TAG_reference_type;
  Type_789.Children := 0;
  Type_789.AddRef(DW_AT_type, DW_FORM_ref4, @Type_787); // $6D, $26, $00, $00

  Type_790 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_790.Tag := DW_TAG_structure_type;
  Type_790.Children := 1;
  Type_790.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSUB4_791 := Type_790.GetNewChild;
    VarVARPSUB4_791.Tag := DW_TAG_member;
    VarVARPSUB4_791.Children := 0;
    VarVARPSUB4_791.Add(DW_AT_name, DW_FORM_string, 'VARPSUB4'+#0);
    VarVARPSUB4_791.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSUB4_791.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB4_216); // $EE, $0E, $00, $00

  Type_792 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_792.Tag := DW_TAG_reference_type;
  Type_792.Children := 0;
  Type_792.AddRef(DW_AT_type, DW_FORM_ref4, @Type_790); // $86, $26, $00, $00

  Type_793 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_793.Tag := DW_TAG_structure_type;
  Type_793.Children := 1;
  Type_793.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSUB5_794 := Type_793.GetNewChild;
    VarVARPSUB5_794.Tag := DW_TAG_member;
    VarVARPSUB5_794.Children := 0;
    VarVARPSUB5_794.Add(DW_AT_name, DW_FORM_string, 'VARPSUB5'+#0);
    VarVARPSUB5_794.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSUB5_794.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSUB5_219); // $03, $0F, $00, $00

  Type_795 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_795.Tag := DW_TAG_reference_type;
  Type_795.Children := 0;
  Type_795.AddRef(DW_AT_type, DW_FORM_ref4, @Type_793); // $9F, $26, $00, $00

  Type_796 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_796.Tag := DW_TAG_structure_type;
  Type_796.Children := 1;
  Type_796.AddULEB(DW_AT_byte_size, DW_FORM_udata, 1);

    VarVARBOOLEAN_797 := Type_796.GetNewChild;
    VarVARBOOLEAN_797.Tag := DW_TAG_member;
    VarVARBOOLEAN_797.Children := 0;
    VarVARBOOLEAN_797.Add(DW_AT_name, DW_FORM_string, 'VARBOOLEAN'+#0);
    VarVARBOOLEAN_797.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARBOOLEAN_797.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBOOLEAN_165); // $44, $0D, $00, $00

  Type_798 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_798.Tag := DW_TAG_reference_type;
  Type_798.Children := 0;
  Type_798.AddRef(DW_AT_type, DW_FORM_ref4, @Type_796); // $B8, $26, $00, $00

  Type_799 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_799.Tag := DW_TAG_structure_type;
  Type_799.Children := 1;
  Type_799.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPBOOLEAN_800 := Type_799.GetNewChild;
    VarVARPBOOLEAN_800.Tag := DW_TAG_member;
    VarVARPBOOLEAN_800.Children := 0;
    VarVARPBOOLEAN_800.Add(DW_AT_name, DW_FORM_string, 'VARPBOOLEAN'+#0);
    VarVARPBOOLEAN_800.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPBOOLEAN_800.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPBOOLEAN_222); // $18, $0F, $00, $00

  Type_801 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_801.Tag := DW_TAG_reference_type;
  Type_801.Children := 0;
  Type_801.AddRef(DW_AT_type, DW_FORM_ref4, @Type_799); // $D3, $26, $00, $00

  Type_802 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_802.Tag := DW_TAG_structure_type;
  Type_802.Children := 1;
  Type_802.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARENUM0_803 := Type_802.GetNewChild;
    VarVARENUM0_803.Tag := DW_TAG_member;
    VarVARENUM0_803.Children := 0;
    VarVARENUM0_803.Add(DW_AT_name, DW_FORM_string, 'VARENUM0'+#0);
    VarVARENUM0_803.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARENUM0_803.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM0_225); // $30, $0F, $00, $00

  Type_804 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_804.Tag := DW_TAG_reference_type;
  Type_804.Children := 0;
  Type_804.AddRef(DW_AT_type, DW_FORM_ref4, @Type_802); // $EF, $26, $00, $00

  Type_805 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_805.Tag := DW_TAG_structure_type;
  Type_805.Children := 1;
  Type_805.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARENUM1_806 := Type_805.GetNewChild;
    VarVARENUM1_806.Tag := DW_TAG_member;
    VarVARENUM1_806.Children := 0;
    VarVARENUM1_806.Add(DW_AT_name, DW_FORM_string, 'VARENUM1'+#0);
    VarVARENUM1_806.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARENUM1_806.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM1_229); // $54, $0F, $00, $00

  Type_807 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_807.Tag := DW_TAG_reference_type;
  Type_807.Children := 0;
  Type_807.AddRef(DW_AT_type, DW_FORM_ref4, @Type_805); // $08, $27, $00, $00

  Type_808 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_808.Tag := DW_TAG_structure_type;
  Type_808.Children := 1;
  Type_808.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARENUM2_809 := Type_808.GetNewChild;
    VarVARENUM2_809.Tag := DW_TAG_member;
    VarVARENUM2_809.Children := 0;
    VarVARENUM2_809.Add(DW_AT_name, DW_FORM_string, 'VARENUM2'+#0);
    VarVARENUM2_809.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARENUM2_809.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM2_235); // $8A, $0F, $00, $00

  Type_810 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_810.Tag := DW_TAG_reference_type;
  Type_810.Children := 0;
  Type_810.AddRef(DW_AT_type, DW_FORM_ref4, @Type_808); // $21, $27, $00, $00

  Type_811 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_811.Tag := DW_TAG_structure_type;
  Type_811.Children := 1;
  Type_811.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARENUM3_812 := Type_811.GetNewChild;
    VarVARENUM3_812.Tag := DW_TAG_member;
    VarVARENUM3_812.Children := 0;
    VarVARENUM3_812.Add(DW_AT_name, DW_FORM_string, 'VARENUM3'+#0);
    VarVARENUM3_812.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARENUM3_812.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUM3_247); // $F6, $0F, $00, $00

  Type_813 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_813.Tag := DW_TAG_reference_type;
  Type_813.Children := 0;
  Type_813.AddRef(DW_AT_type, DW_FORM_ref4, @Type_811); // $3A, $27, $00, $00

  Type_814 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_814.Tag := DW_TAG_structure_type;
  Type_814.Children := 1;
  Type_814.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARENUMX_815 := Type_814.GetNewChild;
    VarVARENUMX_815.Tag := DW_TAG_member;
    VarVARENUMX_815.Children := 0;
    VarVARENUMX_815.Add(DW_AT_name, DW_FORM_string, 'VARENUMX'+#0);
    VarVARENUMX_815.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARENUMX_815.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMX_267); // $AA, $10, $00, $00

  Type_816 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_816.Tag := DW_TAG_reference_type;
  Type_816.Children := 0;
  Type_816.AddRef(DW_AT_type, DW_FORM_ref4, @Type_814); // $53, $27, $00, $00

  Type_817 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_817.Tag := DW_TAG_structure_type;
  Type_817.Children := 1;
  Type_817.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARENUMR3_818 := Type_817.GetNewChild;
    VarVARENUMR3_818.Tag := DW_TAG_member;
    VarVARENUMR3_818.Children := 0;
    VarVARENUMR3_818.Add(DW_AT_name, DW_FORM_string, 'VARENUMR3'+#0);
    VarVARENUMR3_818.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARENUMR3_818.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTENUMR3_273); // $E0, $10, $00, $00

  Type_819 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_819.Tag := DW_TAG_reference_type;
  Type_819.Children := 0;
  Type_819.AddRef(DW_AT_type, DW_FORM_ref4, @Type_817); // $6C, $27, $00, $00

  Type_820 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_820.Tag := DW_TAG_structure_type;
  Type_820.Children := 1;
  Type_820.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPENUM0_821 := Type_820.GetNewChild;
    VarVARPENUM0_821.Tag := DW_TAG_member;
    VarVARPENUM0_821.Children := 0;
    VarVARPENUM0_821.Add(DW_AT_name, DW_FORM_string, 'VARPENUM0'+#0);
    VarVARPENUM0_821.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPENUM0_821.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUM0_286); // $5B, $11, $00, $00

  Type_822 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_822.Tag := DW_TAG_reference_type;
  Type_822.Children := 0;
  Type_822.AddRef(DW_AT_type, DW_FORM_ref4, @Type_820); // $86, $27, $00, $00

  Type_823 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_823.Tag := DW_TAG_structure_type;
  Type_823.Children := 1;
  Type_823.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPENUM1_824 := Type_823.GetNewChild;
    VarVARPENUM1_824.Tag := DW_TAG_member;
    VarVARPENUM1_824.Children := 0;
    VarVARPENUM1_824.Add(DW_AT_name, DW_FORM_string, 'VARPENUM1'+#0);
    VarVARPENUM1_824.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPENUM1_824.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUM1_289); // $71, $11, $00, $00

  Type_825 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_825.Tag := DW_TAG_reference_type;
  Type_825.Children := 0;
  Type_825.AddRef(DW_AT_type, DW_FORM_ref4, @Type_823); // $A0, $27, $00, $00

  Type_826 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_826.Tag := DW_TAG_structure_type;
  Type_826.Children := 1;
  Type_826.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPENUM2_827 := Type_826.GetNewChild;
    VarVARPENUM2_827.Tag := DW_TAG_member;
    VarVARPENUM2_827.Children := 0;
    VarVARPENUM2_827.Add(DW_AT_name, DW_FORM_string, 'VARPENUM2'+#0);
    VarVARPENUM2_827.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPENUM2_827.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUM2_292); // $87, $11, $00, $00

  Type_828 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_828.Tag := DW_TAG_reference_type;
  Type_828.Children := 0;
  Type_828.AddRef(DW_AT_type, DW_FORM_ref4, @Type_826); // $BA, $27, $00, $00

  Type_829 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_829.Tag := DW_TAG_structure_type;
  Type_829.Children := 1;
  Type_829.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPENUM3_830 := Type_829.GetNewChild;
    VarVARPENUM3_830.Tag := DW_TAG_member;
    VarVARPENUM3_830.Children := 0;
    VarVARPENUM3_830.Add(DW_AT_name, DW_FORM_string, 'VARPENUM3'+#0);
    VarVARPENUM3_830.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPENUM3_830.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUM3_295); // $9D, $11, $00, $00

  Type_831 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_831.Tag := DW_TAG_reference_type;
  Type_831.Children := 0;
  Type_831.AddRef(DW_AT_type, DW_FORM_ref4, @Type_829); // $D4, $27, $00, $00

  Type_832 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_832.Tag := DW_TAG_structure_type;
  Type_832.Children := 1;
  Type_832.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPENUMX_833 := Type_832.GetNewChild;
    VarVARPENUMX_833.Tag := DW_TAG_member;
    VarVARPENUMX_833.Children := 0;
    VarVARPENUMX_833.Add(DW_AT_name, DW_FORM_string, 'VARPENUMX'+#0);
    VarVARPENUMX_833.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPENUMX_833.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUMX_298); // $B3, $11, $00, $00

  Type_834 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_834.Tag := DW_TAG_reference_type;
  Type_834.Children := 0;
  Type_834.AddRef(DW_AT_type, DW_FORM_ref4, @Type_832); // $EE, $27, $00, $00

  Type_835 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_835.Tag := DW_TAG_structure_type;
  Type_835.Children := 1;
  Type_835.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPENUMR3_836 := Type_835.GetNewChild;
    VarVARPENUMR3_836.Tag := DW_TAG_member;
    VarVARPENUMR3_836.Children := 0;
    VarVARPENUMR3_836.Add(DW_AT_name, DW_FORM_string, 'VARPENUMR3'+#0);
    VarVARPENUMR3_836.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPENUMR3_836.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPENUMR3_301); // $C9, $11, $00, $00

  Type_837 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_837.Tag := DW_TAG_reference_type;
  Type_837.Children := 0;
  Type_837.AddRef(DW_AT_type, DW_FORM_ref4, @Type_835); // $08, $28, $00, $00

  Type_838 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_838.Tag := DW_TAG_structure_type;
  Type_838.Children := 1;
  Type_838.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARSET0_839 := Type_838.GetNewChild;
    VarVARSET0_839.Tag := DW_TAG_member;
    VarVARSET0_839.Children := 0;
    VarVARSET0_839.Add(DW_AT_name, DW_FORM_string, 'VARSET0'+#0);
    VarVARSET0_839.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSET0_839.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET0_304); // $E0, $11, $00, $00

  Type_840 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_840.Tag := DW_TAG_reference_type;
  Type_840.Children := 0;
  Type_840.AddRef(DW_AT_type, DW_FORM_ref4, @Type_838); // $23, $28, $00, $00

  Type_841 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_841.Tag := DW_TAG_structure_type;
  Type_841.Children := 1;
  Type_841.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARSET1_842 := Type_841.GetNewChild;
    VarVARSET1_842.Tag := DW_TAG_member;
    VarVARSET1_842.Children := 0;
    VarVARSET1_842.Add(DW_AT_name, DW_FORM_string, 'VARSET1'+#0);
    VarVARSET1_842.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSET1_842.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET1_308); // $04, $12, $00, $00

  Type_843 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_843.Tag := DW_TAG_reference_type;
  Type_843.Children := 0;
  Type_843.AddRef(DW_AT_type, DW_FORM_ref4, @Type_841); // $3B, $28, $00, $00

  Type_844 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_844.Tag := DW_TAG_structure_type;
  Type_844.Children := 1;
  Type_844.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARSET2_845 := Type_844.GetNewChild;
    VarVARSET2_845.Tag := DW_TAG_member;
    VarVARSET2_845.Children := 0;
    VarVARSET2_845.Add(DW_AT_name, DW_FORM_string, 'VARSET2'+#0);
    VarVARSET2_845.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSET2_845.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET2_312); // $28, $12, $00, $00

  Type_846 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_846.Tag := DW_TAG_reference_type;
  Type_846.Children := 0;
  Type_846.AddRef(DW_AT_type, DW_FORM_ref4, @Type_844); // $53, $28, $00, $00

  Type_847 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_847.Tag := DW_TAG_structure_type;
  Type_847.Children := 1;
  Type_847.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARSET3_848 := Type_847.GetNewChild;
    VarVARSET3_848.Tag := DW_TAG_member;
    VarVARSET3_848.Children := 0;
    VarVARSET3_848.Add(DW_AT_name, DW_FORM_string, 'VARSET3'+#0);
    VarVARSET3_848.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSET3_848.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSET3_316); // $4C, $12, $00, $00

  Type_849 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_849.Tag := DW_TAG_reference_type;
  Type_849.Children := 0;
  Type_849.AddRef(DW_AT_type, DW_FORM_ref4, @Type_847); // $6B, $28, $00, $00

  Type_850 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_850.Tag := DW_TAG_structure_type;
  Type_850.Children := 1;
  Type_850.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARSETX1_851 := Type_850.GetNewChild;
    VarVARSETX1_851.Tag := DW_TAG_member;
    VarVARSETX1_851.Children := 0;
    VarVARSETX1_851.Add(DW_AT_name, DW_FORM_string, 'VARSETX1'+#0);
    VarVARSETX1_851.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSETX1_851.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETX1_320); // $70, $12, $00, $00

  Type_852 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_852.Tag := DW_TAG_reference_type;
  Type_852.Children := 0;
  Type_852.AddRef(DW_AT_type, DW_FORM_ref4, @Type_850); // $83, $28, $00, $00

  Type_853 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_853.Tag := DW_TAG_structure_type;
  Type_853.Children := 1;
  Type_853.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARSETX2_854 := Type_853.GetNewChild;
    VarVARSETX2_854.Tag := DW_TAG_member;
    VarVARSETX2_854.Children := 0;
    VarVARSETX2_854.Add(DW_AT_name, DW_FORM_string, 'VARSETX2'+#0);
    VarVARSETX2_854.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSETX2_854.AddRef(DW_AT_type, DW_FORM_ref4, @Type_954); // $D2, $2B, $00, $00

  Type_855 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_855.Tag := DW_TAG_reference_type;
  Type_855.Children := 0;
  Type_855.AddRef(DW_AT_type, DW_FORM_ref4, @Type_853); // $9C, $28, $00, $00

  Type_856 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_856.Tag := DW_TAG_structure_type;
  Type_856.Children := 1;
  Type_856.AddULEB(DW_AT_byte_size, DW_FORM_udata, 32);

    VarVARSETB1_857 := Type_856.GetNewChild;
    VarVARSETB1_857.Tag := DW_TAG_member;
    VarVARSETB1_857.Children := 0;
    VarVARSETB1_857.Add(DW_AT_name, DW_FORM_string, 'VARSETB1'+#0);
    VarVARSETB1_857.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSETB1_857.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETB1_324); // $96, $12, $00, $00

  Type_858 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_858.Tag := DW_TAG_reference_type;
  Type_858.Children := 0;
  Type_858.AddRef(DW_AT_type, DW_FORM_ref4, @Type_856); // $B5, $28, $00, $00

  Type_859 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_859.Tag := DW_TAG_structure_type;
  Type_859.Children := 1;
  Type_859.AddULEB(DW_AT_byte_size, DW_FORM_udata, 32);

    VarVARSETB2_860 := Type_859.GetNewChild;
    VarVARSETB2_860.Tag := DW_TAG_member;
    VarVARSETB2_860.Children := 0;
    VarVARSETB2_860.Add(DW_AT_name, DW_FORM_string, 'VARSETB2'+#0);
    VarVARSETB2_860.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSETB2_860.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETB2_328); // $BD, $12, $00, $00

  Type_861 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_861.Tag := DW_TAG_reference_type;
  Type_861.Children := 0;
  Type_861.AddRef(DW_AT_type, DW_FORM_ref4, @Type_859); // $CE, $28, $00, $00

  Type_862 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_862.Tag := DW_TAG_structure_type;
  Type_862.Children := 1;
  Type_862.AddULEB(DW_AT_byte_size, DW_FORM_udata, 32);

    VarVARSETC1_863 := Type_862.GetNewChild;
    VarVARSETC1_863.Tag := DW_TAG_member;
    VarVARSETC1_863.Children := 0;
    VarVARSETC1_863.Add(DW_AT_name, DW_FORM_string, 'VARSETC1'+#0);
    VarVARSETC1_863.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSETC1_863.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETC1_332); // $E4, $12, $00, $00

  Type_864 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_864.Tag := DW_TAG_reference_type;
  Type_864.Children := 0;
  Type_864.AddRef(DW_AT_type, DW_FORM_ref4, @Type_862); // $E7, $28, $00, $00

  Type_865 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_865.Tag := DW_TAG_structure_type;
  Type_865.Children := 1;
  Type_865.AddULEB(DW_AT_byte_size, DW_FORM_udata, 32);

    VarVARSETC2_866 := Type_865.GetNewChild;
    VarVARSETC2_866.Tag := DW_TAG_member;
    VarVARSETC2_866.Children := 0;
    VarVARSETC2_866.Add(DW_AT_name, DW_FORM_string, 'VARSETC2'+#0);
    VarVARSETC2_866.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSETC2_866.AddRef(DW_AT_type, DW_FORM_ref4, @Type_957); // $E5, $2B, $00, $00

  Type_867 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_867.Tag := DW_TAG_reference_type;
  Type_867.Children := 0;
  Type_867.AddRef(DW_AT_type, DW_FORM_ref4, @Type_865); // $00, $29, $00, $00

  Type_868 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_868.Tag := DW_TAG_structure_type;
  Type_868.Children := 1;
  Type_868.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARSETR3_869 := Type_868.GetNewChild;
    VarVARSETR3_869.Tag := DW_TAG_member;
    VarVARSETR3_869.Children := 0;
    VarVARSETR3_869.Add(DW_AT_name, DW_FORM_string, 'VARSETR3'+#0);
    VarVARSETR3_869.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARSETR3_869.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclTSETR3_336); // $0B, $13, $00, $00

  Type_870 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_870.Tag := DW_TAG_reference_type;
  Type_870.Children := 0;
  Type_870.AddRef(DW_AT_type, DW_FORM_ref4, @Type_868); // $19, $29, $00, $00

  Type_871 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_871.Tag := DW_TAG_structure_type;
  Type_871.Children := 1;
  Type_871.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSET0_872 := Type_871.GetNewChild;
    VarVARPSET0_872.Tag := DW_TAG_member;
    VarVARPSET0_872.Children := 0;
    VarVARPSET0_872.Add(DW_AT_name, DW_FORM_string, 'VARPSET0'+#0);
    VarVARPSET0_872.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSET0_872.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSET0_376); // $94, $14, $00, $00

  Type_873 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_873.Tag := DW_TAG_reference_type;
  Type_873.Children := 0;
  Type_873.AddRef(DW_AT_type, DW_FORM_ref4, @Type_871); // $32, $29, $00, $00

  Type_874 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_874.Tag := DW_TAG_structure_type;
  Type_874.Children := 1;
  Type_874.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSET1_875 := Type_874.GetNewChild;
    VarVARPSET1_875.Tag := DW_TAG_member;
    VarVARPSET1_875.Children := 0;
    VarVARPSET1_875.Add(DW_AT_name, DW_FORM_string, 'VARPSET1'+#0);
    VarVARPSET1_875.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSET1_875.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSET1_379); // $A9, $14, $00, $00

  Type_876 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_876.Tag := DW_TAG_reference_type;
  Type_876.Children := 0;
  Type_876.AddRef(DW_AT_type, DW_FORM_ref4, @Type_874); // $4B, $29, $00, $00

  Type_877 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_877.Tag := DW_TAG_structure_type;
  Type_877.Children := 1;
  Type_877.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSET2_878 := Type_877.GetNewChild;
    VarVARPSET2_878.Tag := DW_TAG_member;
    VarVARPSET2_878.Children := 0;
    VarVARPSET2_878.Add(DW_AT_name, DW_FORM_string, 'VARPSET2'+#0);
    VarVARPSET2_878.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSET2_878.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSET2_382); // $BE, $14, $00, $00

  Type_879 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_879.Tag := DW_TAG_reference_type;
  Type_879.Children := 0;
  Type_879.AddRef(DW_AT_type, DW_FORM_ref4, @Type_877); // $64, $29, $00, $00

  Type_880 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_880.Tag := DW_TAG_structure_type;
  Type_880.Children := 1;
  Type_880.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSET3_881 := Type_880.GetNewChild;
    VarVARPSET3_881.Tag := DW_TAG_member;
    VarVARPSET3_881.Children := 0;
    VarVARPSET3_881.Add(DW_AT_name, DW_FORM_string, 'VARPSET3'+#0);
    VarVARPSET3_881.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSET3_881.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSET3_385); // $D3, $14, $00, $00

  Type_882 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_882.Tag := DW_TAG_reference_type;
  Type_882.Children := 0;
  Type_882.AddRef(DW_AT_type, DW_FORM_ref4, @Type_880); // $7D, $29, $00, $00

  Type_883 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_883.Tag := DW_TAG_structure_type;
  Type_883.Children := 1;
  Type_883.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETX1_884 := Type_883.GetNewChild;
    VarVARPSETX1_884.Tag := DW_TAG_member;
    VarVARPSETX1_884.Children := 0;
    VarVARPSETX1_884.Add(DW_AT_name, DW_FORM_string, 'VARPSETX1'+#0);
    VarVARPSETX1_884.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETX1_884.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETX1_388); // $E8, $14, $00, $00

  Type_885 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_885.Tag := DW_TAG_reference_type;
  Type_885.Children := 0;
  Type_885.AddRef(DW_AT_type, DW_FORM_ref4, @Type_883); // $96, $29, $00, $00

  Type_886 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_886.Tag := DW_TAG_structure_type;
  Type_886.Children := 1;
  Type_886.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETB1_887 := Type_886.GetNewChild;
    VarVARPSETB1_887.Tag := DW_TAG_member;
    VarVARPSETB1_887.Children := 0;
    VarVARPSETB1_887.Add(DW_AT_name, DW_FORM_string, 'VARPSETB1'+#0);
    VarVARPSETB1_887.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETB1_887.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETB1_391); // $FE, $14, $00, $00

  Type_888 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_888.Tag := DW_TAG_reference_type;
  Type_888.Children := 0;
  Type_888.AddRef(DW_AT_type, DW_FORM_ref4, @Type_886); // $B0, $29, $00, $00

  Type_889 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_889.Tag := DW_TAG_structure_type;
  Type_889.Children := 1;
  Type_889.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETB2_890 := Type_889.GetNewChild;
    VarVARPSETB2_890.Tag := DW_TAG_member;
    VarVARPSETB2_890.Children := 0;
    VarVARPSETB2_890.Add(DW_AT_name, DW_FORM_string, 'VARPSETB2'+#0);
    VarVARPSETB2_890.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETB2_890.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETB2_394); // $14, $15, $00, $00

  Type_891 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_891.Tag := DW_TAG_reference_type;
  Type_891.Children := 0;
  Type_891.AddRef(DW_AT_type, DW_FORM_ref4, @Type_889); // $CA, $29, $00, $00

  Type_892 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_892.Tag := DW_TAG_structure_type;
  Type_892.Children := 1;
  Type_892.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETC1_893 := Type_892.GetNewChild;
    VarVARPSETC1_893.Tag := DW_TAG_member;
    VarVARPSETC1_893.Children := 0;
    VarVARPSETC1_893.Add(DW_AT_name, DW_FORM_string, 'VARPSETC1'+#0);
    VarVARPSETC1_893.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETC1_893.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETC1_397); // $2A, $15, $00, $00

  Type_894 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_894.Tag := DW_TAG_reference_type;
  Type_894.Children := 0;
  Type_894.AddRef(DW_AT_type, DW_FORM_ref4, @Type_892); // $E4, $29, $00, $00

  Type_895 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_895.Tag := DW_TAG_structure_type;
  Type_895.Children := 1;
  Type_895.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETR3_896 := Type_895.GetNewChild;
    VarVARPSETR3_896.Tag := DW_TAG_member;
    VarVARPSETR3_896.Children := 0;
    VarVARPSETR3_896.Add(DW_AT_name, DW_FORM_string, 'VARPSETR3'+#0);
    VarVARPSETR3_896.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETR3_896.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETR3_400); // $40, $15, $00, $00

  Type_897 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_897.Tag := DW_TAG_reference_type;
  Type_897.Children := 0;
  Type_897.AddRef(DW_AT_type, DW_FORM_ref4, @Type_895); // $FE, $29, $00, $00

  Type_898 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_898.Tag := DW_TAG_structure_type;
  Type_898.Children := 1;
  Type_898.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETP0_899 := Type_898.GetNewChild;
    VarVARPSETP0_899.Tag := DW_TAG_member;
    VarVARPSETP0_899.Children := 0;
    VarVARPSETP0_899.Add(DW_AT_name, DW_FORM_string, 'VARPSETP0'+#0);
    VarVARPSETP0_899.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETP0_899.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETP0_403); // $56, $15, $00, $00

  Type_900 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_900.Tag := DW_TAG_reference_type;
  Type_900.Children := 0;
  Type_900.AddRef(DW_AT_type, DW_FORM_ref4, @Type_898); // $18, $2A, $00, $00

  Type_901 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_901.Tag := DW_TAG_structure_type;
  Type_901.Children := 1;
  Type_901.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETP1_902 := Type_901.GetNewChild;
    VarVARPSETP1_902.Tag := DW_TAG_member;
    VarVARPSETP1_902.Children := 0;
    VarVARPSETP1_902.Add(DW_AT_name, DW_FORM_string, 'VARPSETP1'+#0);
    VarVARPSETP1_902.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETP1_902.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETP1_406); // $6C, $15, $00, $00

  Type_903 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_903.Tag := DW_TAG_reference_type;
  Type_903.Children := 0;
  Type_903.AddRef(DW_AT_type, DW_FORM_ref4, @Type_901); // $32, $2A, $00, $00

  Type_904 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_904.Tag := DW_TAG_structure_type;
  Type_904.Children := 1;
  Type_904.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETP2_905 := Type_904.GetNewChild;
    VarVARPSETP2_905.Tag := DW_TAG_member;
    VarVARPSETP2_905.Children := 0;
    VarVARPSETP2_905.Add(DW_AT_name, DW_FORM_string, 'VARPSETP2'+#0);
    VarVARPSETP2_905.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETP2_905.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETP2_409); // $82, $15, $00, $00

  Type_906 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_906.Tag := DW_TAG_reference_type;
  Type_906.Children := 0;
  Type_906.AddRef(DW_AT_type, DW_FORM_ref4, @Type_904); // $4C, $2A, $00, $00

  Type_907 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_907.Tag := DW_TAG_structure_type;
  Type_907.Children := 1;
  Type_907.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETP3_908 := Type_907.GetNewChild;
    VarVARPSETP3_908.Tag := DW_TAG_member;
    VarVARPSETP3_908.Children := 0;
    VarVARPSETP3_908.Add(DW_AT_name, DW_FORM_string, 'VARPSETP3'+#0);
    VarVARPSETP3_908.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETP3_908.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETP3_412); // $98, $15, $00, $00

  Type_909 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_909.Tag := DW_TAG_reference_type;
  Type_909.Children := 0;
  Type_909.AddRef(DW_AT_type, DW_FORM_ref4, @Type_907); // $66, $2A, $00, $00

  Type_910 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_910.Tag := DW_TAG_structure_type;
  Type_910.Children := 1;
  Type_910.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETPX1_911 := Type_910.GetNewChild;
    VarVARPSETPX1_911.Tag := DW_TAG_member;
    VarVARPSETPX1_911.Children := 0;
    VarVARPSETPX1_911.Add(DW_AT_name, DW_FORM_string, 'VARPSETPX1'+#0);
    VarVARPSETPX1_911.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETPX1_911.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPX1_415); // $AE, $15, $00, $00

  Type_912 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_912.Tag := DW_TAG_reference_type;
  Type_912.Children := 0;
  Type_912.AddRef(DW_AT_type, DW_FORM_ref4, @Type_910); // $80, $2A, $00, $00

  Type_913 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_913.Tag := DW_TAG_structure_type;
  Type_913.Children := 1;
  Type_913.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETPB1_914 := Type_913.GetNewChild;
    VarVARPSETPB1_914.Tag := DW_TAG_member;
    VarVARPSETPB1_914.Children := 0;
    VarVARPSETPB1_914.Add(DW_AT_name, DW_FORM_string, 'VARPSETPB1'+#0);
    VarVARPSETPB1_914.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETPB1_914.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPB1_418); // $C5, $15, $00, $00

  Type_915 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_915.Tag := DW_TAG_reference_type;
  Type_915.Children := 0;
  Type_915.AddRef(DW_AT_type, DW_FORM_ref4, @Type_913); // $9B, $2A, $00, $00

  Type_916 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_916.Tag := DW_TAG_structure_type;
  Type_916.Children := 1;
  Type_916.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETPB2_917 := Type_916.GetNewChild;
    VarVARPSETPB2_917.Tag := DW_TAG_member;
    VarVARPSETPB2_917.Children := 0;
    VarVARPSETPB2_917.Add(DW_AT_name, DW_FORM_string, 'VARPSETPB2'+#0);
    VarVARPSETPB2_917.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETPB2_917.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPB2_421); // $DC, $15, $00, $00

  Type_918 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_918.Tag := DW_TAG_reference_type;
  Type_918.Children := 0;
  Type_918.AddRef(DW_AT_type, DW_FORM_ref4, @Type_916); // $B6, $2A, $00, $00

  Type_919 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_919.Tag := DW_TAG_structure_type;
  Type_919.Children := 1;
  Type_919.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETPC1_920 := Type_919.GetNewChild;
    VarVARPSETPC1_920.Tag := DW_TAG_member;
    VarVARPSETPC1_920.Children := 0;
    VarVARPSETPC1_920.Add(DW_AT_name, DW_FORM_string, 'VARPSETPC1'+#0);
    VarVARPSETPC1_920.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETPC1_920.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPC1_424); // $F3, $15, $00, $00

  Type_921 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_921.Tag := DW_TAG_reference_type;
  Type_921.Children := 0;
  Type_921.AddRef(DW_AT_type, DW_FORM_ref4, @Type_919); // $D1, $2A, $00, $00

  Type_922 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_922.Tag := DW_TAG_structure_type;
  Type_922.Children := 1;
  Type_922.AddULEB(DW_AT_byte_size, DW_FORM_udata, 4);

    VarVARPSETPR3_923 := Type_922.GetNewChild;
    VarVARPSETPR3_923.Tag := DW_TAG_member;
    VarVARPSETPR3_923.Children := 0;
    VarVARPSETPR3_923.Add(DW_AT_name, DW_FORM_string, 'VARPSETPR3'+#0);
    VarVARPSETPR3_923.Add(DW_AT_data_member_location, DW_FORM_block1, BytesLen1([DW_OP_plus_uconst, ULEB(0)])); // $23, $00
    VarVARPSETPR3_923.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclPSETPR3_427); // $0A, $16, $00, $00

  Type_924 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_924.Tag := DW_TAG_reference_type;
  Type_924.Children := 0;
  Type_924.AddRef(DW_AT_type, DW_FORM_ref4, @Type_922); // $EC, $2A, $00, $00

  Type_925 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_925.Tag := DW_TAG_enumeration_type;
  Type_925.Children := 1;
  Type_925.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeS1A_926 := Type_925.GetNewChild;
    TypeS1A_926.Tag := DW_TAG_enumerator;
    TypeS1A_926.Children := 0;
    TypeS1A_926.Add(DW_AT_name, DW_FORM_string, 'S1A'+#0);
    TypeS1A_926.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeS1B_927 := Type_925.GetNewChild;
    TypeS1B_927.Tag := DW_TAG_enumerator;
    TypeS1B_927.Children := 0;
    TypeS1B_927.Add(DW_AT_name, DW_FORM_string, 'S1B'+#0);
    TypeS1B_927.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeS1C_928 := Type_925.GetNewChild;
    TypeS1C_928.Tag := DW_TAG_enumerator;
    TypeS1C_928.Children := 0;
    TypeS1C_928.Add(DW_AT_name, DW_FORM_string, 'S1C'+#0);
    TypeS1C_928.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

  Type_929 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_929.Tag := DW_TAG_reference_type;
  Type_929.Children := 0;
  Type_929.AddRef(DW_AT_type, DW_FORM_ref4, @Type_925); // $07, $2B, $00, $00

  Type_930 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_930.Tag := DW_TAG_subrange_type;
  Type_930.Children := 0;
  Type_930.AddULEB(DW_AT_lower_bound, DW_FORM_udata, 5);
  Type_930.AddULEB(DW_AT_upper_bound, DW_FORM_udata, 80);
  Type_930.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_141); // $6A, $0C, $00, $00

  Type_931 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_931.Tag := DW_TAG_reference_type;
  Type_931.Children := 0;
  Type_931.AddRef(DW_AT_type, DW_FORM_ref4, @Type_930); // $2A, $2B, $00, $00

  Type_932 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_932.Tag := DW_TAG_enumeration_type;
  Type_932.Children := 1;
  Type_932.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeSP1A_933 := Type_932.GetNewChild;
    TypeSP1A_933.Tag := DW_TAG_enumerator;
    TypeSP1A_933.Children := 0;
    TypeSP1A_933.Add(DW_AT_name, DW_FORM_string, 'SP1A'+#0);
    TypeSP1A_933.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeSP1B_934 := Type_932.GetNewChild;
    TypeSP1B_934.Tag := DW_TAG_enumerator;
    TypeSP1B_934.Children := 0;
    TypeSP1B_934.Add(DW_AT_name, DW_FORM_string, 'SP1B'+#0);
    TypeSP1B_934.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeSP1C_935 := Type_932.GetNewChild;
    TypeSP1C_935.Tag := DW_TAG_enumerator;
    TypeSP1C_935.Children := 0;
    TypeSP1C_935.Add(DW_AT_name, DW_FORM_string, 'SP1C'+#0);
    TypeSP1C_935.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

  Type_936 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_936.Tag := DW_TAG_reference_type;
  Type_936.Children := 0;
  Type_936.AddRef(DW_AT_type, DW_FORM_ref4, @Type_932); // $36, $2B, $00, $00

  Type_937 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_937.Tag := DW_TAG_subrange_type;
  Type_937.Children := 0;
  Type_937.AddULEB(DW_AT_lower_bound, DW_FORM_udata, 5);
  Type_937.AddULEB(DW_AT_upper_bound, DW_FORM_udata, 80);
  Type_937.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclBYTE_141); // $6A, $0C, $00, $00

  Type_938 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_938.Tag := DW_TAG_reference_type;
  Type_938.Children := 0;
  Type_938.AddRef(DW_AT_type, DW_FORM_ref4, @Type_937); // $5C, $2B, $00, $00

  Type_939 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_939.Tag := DW_TAG_enumeration_type;
  Type_939.Children := 1;
  Type_939.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeSXA_940 := Type_939.GetNewChild;
    TypeSXA_940.Tag := DW_TAG_enumerator;
    TypeSXA_940.Children := 0;
    TypeSXA_940.Add(DW_AT_name, DW_FORM_string, 'SXA'+#0);
    TypeSXA_940.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeSXB_941 := Type_939.GetNewChild;
    TypeSXB_941.Tag := DW_TAG_enumerator;
    TypeSXB_941.Children := 0;
    TypeSXB_941.Add(DW_AT_name, DW_FORM_string, 'SXB'+#0);
    TypeSXB_941.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeSXC_942 := Type_939.GetNewChild;
    TypeSXC_942.Tag := DW_TAG_enumerator;
    TypeSXC_942.Children := 0;
    TypeSXC_942.Add(DW_AT_name, DW_FORM_string, 'SXC'+#0);
    TypeSXC_942.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

    TypeSXD_943 := Type_939.GetNewChild;
    TypeSXD_943.Tag := DW_TAG_enumerator;
    TypeSXD_943.Children := 0;
    TypeSXD_943.Add(DW_AT_name, DW_FORM_string, 'SXD'+#0);
    TypeSXD_943.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

  Type_944 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_944.Tag := DW_TAG_reference_type;
  Type_944.Children := 0;
  Type_944.AddRef(DW_AT_type, DW_FORM_ref4, @Type_939); // $68, $2B, $00, $00

  TypeDeclCHAR_945 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeDeclCHAR_945.Tag := DW_TAG_typedef;
  TypeDeclCHAR_945.Children := 0;
  TypeDeclCHAR_945.Add(DW_AT_name, DW_FORM_string, 'CHAR'+#0);
  TypeDeclCHAR_945.AddRef(DW_AT_type, DW_FORM_ref4, @TypeChar_946); // $9E, $2B, $00, $00

  TypeChar_946 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  TypeChar_946.Tag := DW_TAG_base_type;
  TypeChar_946.Children := 0;
  TypeChar_946.Add(DW_AT_name, DW_FORM_string, 'Char'+#0);
  TypeChar_946.Add(DW_AT_encoding, DW_FORM_data1, [$08]);
  TypeChar_946.Add(DW_AT_byte_size, DW_FORM_data1, [$01]);

  Type_947 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_947.Tag := DW_TAG_reference_type;
  Type_947.Children := 0;
  Type_947.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclCHAR_945); // $94, $2B, $00, $00

  Type_948 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_948.Tag := DW_TAG_set_type;
  Type_948.Children := 0;
  Type_948.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  Type_948.AddRef(DW_AT_type, DW_FORM_ref4, @Type_949); // $B2, $2B, $00, $00

  Type_949 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_949.Tag := DW_TAG_subrange_type;
  Type_949.Children := 0;
  Type_949.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_949.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 3);
  Type_949.AddRef(DW_AT_type, DW_FORM_ref4, @Type_960); // $F9, $2B, $00, $00

  Type_950 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_950.Tag := DW_TAG_reference_type;
  Type_950.Children := 0;
  Type_950.AddRef(DW_AT_type, DW_FORM_ref4, @Type_948); // $AB, $2B, $00, $00

  Type_951 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_951.Tag := DW_TAG_set_type;
  Type_951.Children := 0;
  Type_951.Add(DW_AT_byte_size, DW_FORM_data2, [$20, $00]);
  Type_951.AddRef(DW_AT_type, DW_FORM_ref4, @Type_952); // $C5, $2B, $00, $00

  Type_952 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_952.Tag := DW_TAG_subrange_type;
  Type_952.Children := 0;
  Type_952.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_952.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 255);
  Type_952.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclCHAR_945); // $94, $2B, $00, $00

  Type_953 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_953.Tag := DW_TAG_reference_type;
  Type_953.Children := 0;
  Type_953.AddRef(DW_AT_type, DW_FORM_ref4, @Type_951); // $BE, $2B, $00, $00

  Type_954 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_954.Tag := DW_TAG_set_type;
  Type_954.Children := 0;
  Type_954.Add(DW_AT_byte_size, DW_FORM_data2, [$04, $00]);
  Type_954.AddRef(DW_AT_type, DW_FORM_ref4, @Type_955); // $D9, $2B, $00, $00

  Type_955 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_955.Tag := DW_TAG_subrange_type;
  Type_955.Children := 0;
  Type_955.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_955.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 3);
  Type_955.AddRef(DW_AT_type, DW_FORM_ref4, @Type_966); // $2D, $2C, $00, $00

  Type_956 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_956.Tag := DW_TAG_reference_type;
  Type_956.Children := 0;
  Type_956.AddRef(DW_AT_type, DW_FORM_ref4, @Type_954); // $D2, $2B, $00, $00

  Type_957 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_957.Tag := DW_TAG_set_type;
  Type_957.Children := 0;
  Type_957.Add(DW_AT_byte_size, DW_FORM_data2, [$20, $00]);
  Type_957.AddRef(DW_AT_type, DW_FORM_ref4, @Type_958); // $EC, $2B, $00, $00

  Type_958 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_958.Tag := DW_TAG_subrange_type;
  Type_958.Children := 0;
  Type_958.AddSLEB(DW_AT_lower_bound, DW_FORM_sdata, 0);
  Type_958.AddSLEB(DW_AT_upper_bound, DW_FORM_sdata, 255);
  Type_958.AddRef(DW_AT_type, DW_FORM_ref4, @TypeDeclCHAR_945); // $94, $2B, $00, $00

  Type_959 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_959.Tag := DW_TAG_reference_type;
  Type_959.Children := 0;
  Type_959.AddRef(DW_AT_type, DW_FORM_ref4, @Type_957); // $E5, $2B, $00, $00

  Type_960 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_960.Tag := DW_TAG_enumeration_type;
  Type_960.Children := 1;
  Type_960.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeTRSXA_961 := Type_960.GetNewChild;
    TypeTRSXA_961.Tag := DW_TAG_enumerator;
    TypeTRSXA_961.Children := 0;
    TypeTRSXA_961.Add(DW_AT_name, DW_FORM_string, 'TRSXA'+#0);
    TypeTRSXA_961.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeTRSXB_962 := Type_960.GetNewChild;
    TypeTRSXB_962.Tag := DW_TAG_enumerator;
    TypeTRSXB_962.Children := 0;
    TypeTRSXB_962.Add(DW_AT_name, DW_FORM_string, 'TRSXB'+#0);
    TypeTRSXB_962.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeTRSXC_963 := Type_960.GetNewChild;
    TypeTRSXC_963.Tag := DW_TAG_enumerator;
    TypeTRSXC_963.Children := 0;
    TypeTRSXC_963.Add(DW_AT_name, DW_FORM_string, 'TRSXC'+#0);
    TypeTRSXC_963.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

    TypeTRSXD_964 := Type_960.GetNewChild;
    TypeTRSXD_964.Tag := DW_TAG_enumerator;
    TypeTRSXD_964.Children := 0;
    TypeTRSXD_964.Add(DW_AT_name, DW_FORM_string, 'TRSXD'+#0);
    TypeTRSXD_964.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

  Type_965 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_965.Tag := DW_TAG_reference_type;
  Type_965.Children := 0;
  Type_965.AddRef(DW_AT_type, DW_FORM_ref4, @Type_960); // $F9, $2B, $00, $00

  Type_966 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_966.Tag := DW_TAG_enumeration_type;
  Type_966.Children := 1;
  Type_966.Add(DW_AT_byte_size, DW_FORM_data1, [$04]);

    TypeRSXA_967 := Type_966.GetNewChild;
    TypeRSXA_967.Tag := DW_TAG_enumerator;
    TypeRSXA_967.Children := 0;
    TypeRSXA_967.Add(DW_AT_name, DW_FORM_string, 'RSXA'+#0);
    TypeRSXA_967.Add(DW_AT_const_value, DW_FORM_data4, [$00, $00, $00, $00]);

    TypeRSXB_968 := Type_966.GetNewChild;
    TypeRSXB_968.Tag := DW_TAG_enumerator;
    TypeRSXB_968.Children := 0;
    TypeRSXB_968.Add(DW_AT_name, DW_FORM_string, 'RSXB'+#0);
    TypeRSXB_968.Add(DW_AT_const_value, DW_FORM_data4, [$01, $00, $00, $00]);

    TypeRSXC_969 := Type_966.GetNewChild;
    TypeRSXC_969.Tag := DW_TAG_enumerator;
    TypeRSXC_969.Children := 0;
    TypeRSXC_969.Add(DW_AT_name, DW_FORM_string, 'RSXC'+#0);
    TypeRSXC_969.Add(DW_AT_const_value, DW_FORM_data4, [$02, $00, $00, $00]);

    TypeRSXD_970 := Type_966.GetNewChild;
    TypeRSXD_970.Tag := DW_TAG_enumerator;
    TypeRSXD_970.Children := 0;
    TypeRSXD_970.Add(DW_AT_name, DW_FORM_string, 'RSXD'+#0);
    TypeRSXD_970.Add(DW_AT_const_value, DW_FORM_data4, [$03, $00, $00, $00]);

  Type_971 := Unitdwarfsetupbasic_lpr_0.GetNewChild;
  Type_971.Tag := DW_TAG_reference_type;
  Type_971.Children := 0;
  Type_971.AddRef(DW_AT_type, DW_FORM_ref4, @Type_966); // $2D, $2C, $00, $00

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

