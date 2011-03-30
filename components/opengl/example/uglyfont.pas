// Copyright: Soji Yamakawa (CaptainYS, E-Mail: PEB01130*nifty+com  <- Replace * with @, + with .)
//
// I don't abandon the copyright, but you can use this code and the header
// (uglyfont.cpp and uglyfont.h) for your product regardless of the purpose,
// i.e., free or commercial, open source or proprietary.
//
// However, I do not take any responsibility for the consequence of using
// this code and header.  Please use on your own risks.
//
// January 5, 2005
//
// Soji Yamakawa
unit uglyfont;

interface

uses
  gl;

// YsDrawUglyFont function draws text using an ugly vector-font set.
// The size of a letter are 1.0x1.0 (no thickness in z direction).
// The size and the location on the display can be controlled by
// glScale and glTranslate (not glRasterPos)
// This function uses OpenGL's display list 1400 to 1655.  If it conflicts with
// your program, modify const int YsUglyFontBase=1400;

procedure YsDrawUglyFont(str: string; centering: integer; useDisplayList: integer = 1);
procedure glTextOut(x, y, z: double; sx, sy, sz: integer; center: integer; str: string);

// The following integer arrays define the ugly font geometry
// Coordinate ranges in the arrays are 0<=x<=100 and 0<=y<=100.
const
  YsUglyFontWid: double = 100;
  YsUglyFontHei: double = 100;

var
  (*   *) Ptn032: array [0..1] of integer = (32, -1);
  (* ! *) Ptn033: array [0..19] of integer = (33, 0, 3, 50, 100, 75, 100, 50, 25, 0, 4, 50, 16, 62, 16, 62, 0, 50, 0, -1);
  (* " *) Ptn034: array [0..11] of integer = (34, 2, 4, 37, 100, 37, 83, 62, 100, 62, 83, -1);
  (* # *) Ptn035: array [0..19] of integer = (35, 2, 8, 12, 66, 87, 66, 12, 33, 87, 33, 37, 91, 37, 8, 62, 8, 62, 91, -1);
  (* $ *) Ptn036: array [0..37] of integer = (36, 1, 12, 87, 75, 75, 83, 25, 83, 12, 75, 12, 58, 25, 50, 75, 50, 87, 41, 87, 25, 75, 16, 25, 16, 12, 25, 2, 4, 37, 91, 37, 8, 62, 8, 62, 91, -1);
  (* % *) Ptn037: array [0..31] of integer = (37, 1, 2, 87, 100, 12, 0, 1, 5, 12, 100, 37, 100, 37, 75, 12, 75, 12, 100, 1, 5, 87, 0, 87, 25, 62, 25, 62, 0, 87, 0, -1);
  (* & *) Ptn038: array [0..25] of integer = (38, 1, 11, 87, 33, 62, 0, 25, 0, 0, 16, 0, 41, 75, 83, 75, 91, 62, 100, 37, 100, 25, 83, 87, 0, -1);
  (* ' *) Ptn039: array [0..9] of integer = (39, 0, 3, 50, 83, 50, 100, 62, 100, -1);
  (* ( *) Ptn040: array [0..15] of integer = (40, 1, 6, 62, 100, 37, 83, 25, 58, 25, 41, 37, 16, 62, 0, -1);
  (* ) *) Ptn041: array [0..15] of integer = (41, 1, 6, 37, 100, 62, 83, 75, 58, 75, 41, 62, 16, 37, 0, -1);
  (* * *) Ptn042: array [0..19] of integer = (42, 2, 8, 50, 100, 50, 0, 0, 50, 100, 50, 87, 91, 12, 8, 87, 8, 12, 91, -1);
  (* + *) Ptn043: array [0..11] of integer = (43, 2, 4, 12, 50, 87, 50, 50, 75, 50, 25, -1);
  (* , *) Ptn044: array [0..11] of integer = (44, 1, 4, 37, 25, 62, 25, 62, 8, 37, -8, -1);
  (* - *) Ptn045: array [0..7] of integer = (45, 1, 2, 12, 50, 87, 50, -1);
  (* . *) Ptn046: array [0..11] of integer = (46, 0, 4, 37, 16, 62, 16, 62, 0, 37, 0, -1);
  (* / *) Ptn047: array [0..7] of integer = (47, 1, 2, 100, 100, 0, 0, -1);
  (* 0 *) Ptn048: array [0..27] of integer = (48, 1, 9, 25, 100, 75, 100, 100, 83, 100, 16, 75, 0, 25, 0, 0, 16, 0, 83, 25, 100, 1, 2, 87, 91, 12, 8, -1);
  (* 1 *) Ptn049: array [0..9] of integer = (49, 1, 3, 25, 83, 50, 100, 50, 0, -1);
  (* 2 *) Ptn050: array [0..19] of integer = (50, 1, 8, 12, 83, 37, 100, 75, 100, 100, 83, 100, 66, 12, 16, 12, 0, 100, 0, -1);
  (* 3 *) Ptn051: array [0..25] of integer = (51, 1, 11, 12, 83, 37, 100, 75, 100, 100, 83, 100, 66, 75, 50, 100, 33, 100, 16, 75, 0, 25, 0, 0, 16, -1);
  (* 4 *) Ptn052: array [0..15] of integer = (52, 1, 3, 37, 100, 12, 25, 87, 25, 1, 2, 62, 75, 62, 0, -1);
  (* 5 *) Ptn053: array [0..23] of integer = (53, 1, 10, 87, 100, 12, 100, 12, 41, 37, 58, 62, 58, 87, 41, 87, 16, 62, 0, 37, 0, 12, 16, -1);
  (* 6 *) Ptn054: array [0..27] of integer = (54, 1, 12, 87, 83, 62, 100, 25, 100, 0, 83, 0, 16, 25, 0, 75, 0, 100, 16, 100, 33, 75, 50, 25, 50, 0, 33, -1);
  (* 7 *) Ptn055: array [0..13] of integer = (55, 1, 5, 12, 83, 12, 100, 87, 100, 50, 33, 50, 0, -1);
  (* 8 *) Ptn056: array [0..39] of integer = (56, 1, 9, 100, 83, 75, 100, 25, 100, 0, 83, 0, 66, 25, 50, 75, 50, 100, 66, 100, 83, 1, 8, 25, 50, 0, 33, 0, 16, 25, 0, 75, 0, 100, 16, 100, 33, 75, 50, -1);
  (* 9 *) Ptn057: array [0..27] of integer = (57, 1, 12, 0, 16, 25, 0, 75, 0, 100, 16, 100, 83, 75, 100, 25, 100, 0, 83, 0, 58, 25, 41, 75, 41, 100, 58, -1);
  (* : *) Ptn058: array [0..21] of integer = (58, 0, 4, 37, 91, 62, 91, 62, 75, 37, 75, 0, 4, 37, 25, 62, 25, 62, 8, 37, 8, -1);
  (* ; *) Ptn059: array [0..27] of integer = (59, 0, 4, 37, 91, 62, 91, 62, 75, 37, 75, 0, 4, 37, 25, 62, 25, 62, 8, 37, 8, 1, 2, 62, 8, 37, -8, -1);
  (* < *) Ptn060: array [0..9] of integer = (60, 1, 3, 87, 100, 12, 50, 87, 0, -1);
  (* = *) Ptn061: array [0..11] of integer = (61, 2, 4, 12, 66, 87, 66, 12, 33, 87, 33, -1);
  (* > *) Ptn062: array [0..9] of integer = (62, 1, 3, 12, 0, 87, 50, 12, 100, -1);
  (* ? *) Ptn063: array [0..29] of integer = (63, 1, 8, 12, 83, 37, 100, 75, 100, 100, 83, 100, 66, 75, 50, 50, 50, 50, 25, 0, 4, 50, 16, 62, 16, 62, 8, 50, 8, -1);
  (* @ *) Ptn064: array [0..39] of integer = (64, 1, 18, 62, 50, 50, 58, 25, 58, 12, 41, 12, 25, 25, 16, 50, 16, 62, 41, 75, 25, 87, 66, 75, 91, 62, 100, 25, 100, 0, 75, 0, 16, 25, 0, 62, 0, 87, 16, -1);
  (* A *) Ptn065: array [0..15] of integer = (65, 1, 3, 0, 0, 50, 100, 100, 0, 1, 2, 25, 50, 75, 50, -1);
  (* B *) Ptn066: array [0..29] of integer = (66, 1, 10, 0, 0, 0, 100, 75, 100, 87, 91, 87, 58, 75, 50, 100, 33, 100, 8, 87, 0, 0, 0, 1, 2, 75, 50, 0, 50, -1);
  (* C *) Ptn067: array [0..19] of integer = (67, 1, 8, 100, 83, 75, 100, 25, 100, 0, 83, 0, 16, 25, 0, 75, 0, 100, 16, -1);
  (* D *) Ptn068: array [0..17] of integer = (68, 1, 7, 0, 100, 75, 100, 100, 83, 100, 16, 75, 0, 0, 0, 0, 100, -1);
  (* E *) Ptn069: array [0..17] of integer = (69, 1, 4, 100, 100, 0, 100, 0, 0, 100, 0, 1, 2, 0, 50, 87, 50, -1);
  (* F *) Ptn070: array [0..15] of integer = (70, 1, 3, 100, 100, 0, 100, 0, 0, 1, 2, 0, 50, 75, 50, -1);
  (* G *) Ptn071: array [0..23] of integer = (71, 1, 10, 100, 83, 75, 100, 25, 100, 0, 83, 0, 16, 25, 0, 75, 0, 100, 16, 100, 41, 62, 41, -1);
  (* H *) Ptn072: array [0..15] of integer = (72, 2, 6, 0, 100, 0, 0, 100, 100, 100, 0, 0, 50, 100, 50, -1);
  (* I *) Ptn073: array [0..15] of integer = (73, 2, 6, 37, 100, 62, 100, 37, 0, 62, 0, 50, 0, 50, 100, -1);
  (* J *) Ptn074: array [0..21] of integer = (74, 1, 2, 75, 100, 100, 100, 1, 6, 87, 100, 87, 16, 62, 0, 37, 0, 12, 16, 12, 33, -1);
  (* K *) Ptn075: array [0..19] of integer = (75, 1, 2, 12, 100, 12, 0, 1, 2, 12, 33, 100, 100, 1, 2, 25, 41, 100, 0, -1);
  (* L *) Ptn076: array [0..9] of integer = (76, 1, 3, 0, 100, 0, 0, 100, 0, -1);
  (* M *) Ptn077: array [0..13] of integer = (77, 1, 5, 0, 0, 0, 100, 50, 50, 100, 100, 100, 0, -1);
  (* N *) Ptn078: array [0..11] of integer = (78, 1, 4, 0, 0, 0, 100, 100, 0, 100, 100, -1);
  (* O *) Ptn079: array [0..21] of integer = (79, 1, 9, 0, 83, 25, 100, 75, 100, 100, 83, 100, 16, 75, 0, 25, 0, 0, 16, 0, 83, -1);
  (* P *) Ptn080: array [0..17] of integer = (80, 1, 7, 0, 0, 0, 100, 75, 100, 100, 83, 100, 66, 75, 50, 0, 50, -1);
  (* Q *) Ptn081: array [0..27] of integer = (81, 1, 9, 25, 0, 0, 16, 0, 83, 25, 100, 75, 100, 100, 83, 100, 16, 75, 0, 25, 0, 1, 2, 62, 25, 100, 0, -1);
  (* R *) Ptn082: array [0..25] of integer = (82, 1, 7, 0, 0, 0, 100, 75, 100, 100, 83, 100, 66, 75, 50, 0, 50, 1, 3, 75, 50, 100, 33, 100, 0, -1);
  (* S *) Ptn083: array [0..27] of integer = (83, 1, 12, 100, 83, 75, 100, 25, 100, 0, 83, 0, 66, 25, 50, 75, 50, 100, 33, 100, 16, 75, 0, 25, 0, 0, 16, -1);
  (* T *) Ptn084: array [0..11] of integer = (84, 2, 4, 0, 100, 100, 100, 50, 100, 50, 0, -1);
  (* U *) Ptn085: array [0..15] of integer = (85, 1, 6, 0, 100, 0, 16, 25, 0, 75, 0, 100, 16, 100, 100, -1);
  (* V *) Ptn086: array [0..9] of integer = (86, 1, 3, 0, 100, 50, 0, 100, 100, -1);
  (* W *) Ptn087: array [0..13] of integer = (87, 1, 5, 0, 100, 25, 0, 50, 66, 75, 0, 100, 100, -1);
  (* X *) Ptn088: array [0..11] of integer = (88, 2, 4, 0, 0, 100, 100, 100, 0, 0, 100, -1);
  (* Y *) Ptn089: array [0..15] of integer = (89, 1, 3, 0, 100, 50, 50, 50, 0, 1, 2, 50, 50, 100, 100, -1);
  (* Z *) Ptn090: array [0..11] of integer = (90, 1, 4, 0, 100, 100, 100, 0, 0, 100, 0, -1);
  (* [ *) Ptn091: array [0..11] of integer = (91, 1, 4, 62, 100, 37, 100, 37, 0, 62, 0, -1);
  (* \ *) Ptn092: array [0..7] of integer = (92, 1, 2, 0, 100, 100, 0, -1);
  (* ] *) Ptn093: array [0..11] of integer = (93, 1, 4, 37, 100, 62, 100, 62, 0, 37, 0, -1);
  (* ^ *) Ptn094: array [0..9] of integer = (94, 1, 3, 0, 66, 50, 91, 100, 66, -1);
  (* _ *) Ptn095: array [0..7] of integer = (95, 1, 2, 0, 8, 100, 8, -1);
  (* ` *) Ptn096: array [0..9] of integer = (96, 0, 3, 37, 100, 50, 100, 50, 83, -1);
  (* a *) Ptn097: array [0..29] of integer = (97, 1, 5, 12, 50, 25, 58, 75, 58, 87, 50, 87, 0, 1, 7, 87, 33, 25, 33, 12, 25, 12, 8, 25, 0, 75, 0, 87, 8, -1);
  (* b *) Ptn098: array [0..17] of integer = (98, 1, 7, 12, 100, 12, 0, 75, 0, 87, 8, 87, 50, 75, 58, 12, 58, -1);
  (* c *) Ptn099: array [0..19] of integer = (99, 1, 8, 87, 50, 75, 58, 25, 58, 12, 50, 12, 8, 25, 0, 75, 0, 87, 8, -1);
  (* d *) Ptn100: array [0..19] of integer = (100, 1, 8, 87, 100, 87, 0, 25, 0, 12, 8, 12, 50, 25, 58, 75, 58, 87, 50, -1);
  (* e *) Ptn101: array [0..23] of integer = (101, 1, 10, 12, 33, 87, 33, 87, 50, 75, 58, 25, 58, 12, 50, 12, 8, 25, 0, 75, 0, 87, 8, -1);
  (* f *) Ptn102: array [0..17] of integer = (102, 1, 4, 75, 100, 62, 100, 50, 91, 50, 0, 1, 2, 25, 58, 75, 58, -1);
  (* g *) Ptn103: array [0..31] of integer = (103, 1, 5, 87, 58, 87, 0, 75, -8, 25, -8, 12, 0, 1, 8, 87, 50, 75, 58, 25, 58, 12, 50, 12, 33, 25, 25, 75, 25, 87, 33, -1);
  (* h *) Ptn104: array [0..19] of integer = (104, 1, 2, 12, 0, 12, 100, 1, 5, 12, 50, 25, 58, 75, 58, 87, 50, 87, 0, -1);
  (* i *) Ptn105: array [0..13] of integer = (105, 1, 2, 50, 75, 50, 66, 1, 2, 50, 58, 50, 0, -1);
  (* j *) Ptn106: array [0..17] of integer = (106, 1, 2, 50, 75, 50, 66, 1, 4, 50, 58, 50, 0, 37, -8, 12, -8, -1);
  (* k *) Ptn107: array [0..15] of integer = (107, 1, 2, 12, 100, 12, 0, 1, 3, 87, 0, 12, 33, 75, 58, -1);
  (* l *) Ptn108: array [0..9] of integer = (108, 1, 3, 37, 100, 50, 100, 50, 0, -1);
  (* m *) Ptn109: array [0..21] of integer = (109, 1, 5, 12, 0, 12, 58, 75, 58, 87, 50, 87, 0, 1, 3, 37, 58, 50, 50, 50, 0, -1);
  (* n *) Ptn110: array [0..13] of integer = (110, 1, 5, 12, 0, 12, 58, 75, 58, 87, 50, 87, 0, -1);
  (* o *) Ptn111: array [0..21] of integer = (111, 1, 9, 25, 0, 12, 8, 12, 50, 25, 58, 75, 58, 87, 50, 87, 8, 75, 0, 25, 0, -1);
  (* p *) Ptn112: array [0..23] of integer = (112, 1, 2, 12, 58, 12, -16, 1, 7, 12, 50, 25, 58, 75, 58, 87, 50, 87, 8, 75, 0, 12, 0, -1);
  (* q *) Ptn113: array [0..23] of integer = (113, 1, 2, 87, 58, 87, -16, 1, 7, 87, 50, 75, 58, 25, 58, 12, 50, 12, 8, 25, 0, 87, 0, -1);
  (* r *) Ptn114: array [0..15] of integer = (114, 1, 2, 25, 58, 25, 0, 1, 3, 25, 50, 62, 58, 87, 58, -1);
  (* s *) Ptn115: array [0..23] of integer = (115, 1, 10, 87, 50, 75, 58, 25, 58, 12, 50, 12, 41, 87, 16, 87, 8, 75, 0, 25, 0, 12, 8, -1);
  (* t *) Ptn116: array [0..17] of integer = (116, 1, 2, 25, 58, 75, 58, 1, 4, 37, 75, 37, 8, 50, 0, 75, 0, -1);
  (* u *) Ptn117: array [0..19] of integer = (117, 1, 5, 12, 58, 12, 8, 25, 0, 62, 0, 87, 8, 1, 2, 87, 58, 87, 0, -1);
  (* v *) Ptn118: array [0..9] of integer = (118, 1, 3, 12, 58, 50, 0, 87, 58, -1);
  (* w *) Ptn119: array [0..13] of integer = (119, 1, 5, 12, 58, 25, 0, 50, 41, 75, 0, 87, 58, -1);
  (* x *) Ptn120: array [0..11] of integer = (120, 2, 4, 87, 0, 12, 58, 87, 58, 12, 0, -1);
  (* y *) Ptn121: array [0..11] of integer = (121, 2, 4, 87, 58, 12, -25, 12, 58, 50, 16, -1);
  (* z *) Ptn122: array [0..11] of integer = (122, 1, 4, 12, 58, 87, 58, 12, 0, 87, 0, -1);
  (* { *) Ptn123: array [0..21] of integer = (123, 1, 6, 75, 100, 50, 100, 37, 91, 37, 8, 50, 0, 75, 0, 1, 2, 37, 50, 25, 50, -1);
  (* | *) Ptn124: array [0..7] of integer = (124, 1, 2, 50, 100, 50, 0, -1);
  (* } *) Ptn125: array [0..21] of integer = (125, 1, 6, 25, 0, 50, 0, 62, 8, 62, 91, 50, 100, 25, 100, 1, 2, 62, 50, 75, 50, -1);
  (* ~ *) Ptn126: array [0..7] of integer = (126, 1, 2, 0, 91, 100, 91, -1);

  YsUglyFontSet: array [0..255] of pinteger = (nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, @Ptn032[0], @Ptn033[0], @Ptn034[0], @Ptn035[0], @Ptn036[0], @Ptn037[0], @Ptn038[0], @Ptn039[0], @Ptn040[0], @Ptn041[0], @Ptn042[0], @Ptn043[0], @Ptn044[0], @Ptn045[0], @Ptn046[0], @Ptn047[0], @Ptn048[0], @Ptn049[0], @Ptn050[0], @Ptn051[0], @Ptn052[0], @Ptn053[0], @Ptn054[0], @Ptn055[0], @Ptn056[0], @Ptn057[0], @Ptn058[0], @Ptn059[0], @Ptn060[0], @Ptn061[0], @Ptn062[0], @Ptn063[0], @Ptn064[0], @Ptn065[0], @Ptn066[0], @Ptn067[0], @Ptn068[0], @Ptn069[0], @Ptn070[0], @Ptn071[0], @Ptn072[0], @Ptn073[0], @Ptn074[0], @Ptn075[0], @Ptn076[0], @Ptn077[0], @Ptn078[0], @Ptn079[0], @Ptn080[0], @Ptn081[0], @Ptn082[0], @Ptn083[0], @Ptn084[0], @Ptn085[0], @Ptn086[0], @Ptn087[0], @Ptn088[0], @Ptn089[0], @Ptn090[0], @Ptn091[0], @Ptn092[0], @Ptn093[0], @Ptn094[0], @Ptn095[0], @Ptn096[0], @Ptn097[0], @Ptn098[0], @Ptn099[0], @Ptn100[0], @Ptn101[0], @Ptn102[0], @Ptn103[0], @Ptn104[0], @Ptn105[0], @Ptn106[0], @Ptn107[0], @Ptn108[0], @Ptn109[0], @Ptn110[0], @Ptn111[0], @Ptn112[0], @Ptn113[0], @Ptn114[0], @Ptn115[0], @Ptn116[0], @Ptn117[0], @Ptn118[0], @Ptn119[0], @Ptn120[0], @Ptn121[0], @Ptn122[0], @Ptn123[0], @Ptn124[0], @Ptn125[0], @Ptn126[0], nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil);

const
  YsUglyFontBase: integer = 1400;

implementation

procedure YsDrawUglyFontPattern(ptn: pinteger);
var
  j: integer;
  ptr: pinteger;
begin
  if ptn <> nil then
  begin

    ptr := ptn;
    Inc(ptr); // Skip character code
    while ptr[0] <> -1 do
    begin
      case ptr[0] of
        0: glBegin(GL_POLYGON);
        1: glBegin(GL_LINE_STRIP);
        2: glBegin(GL_LINES);
      end;

      for j := 0 to Pred(ptr[1]) do
        glVertex2i(ptr[2 + j * 2], ptr[3 + j * 2]);

      glEnd;

      ptr := ptr + 2 + ptr[1] * 2;
    end;
  end;
  glTranslated(YsUglyFontWid * 8 / 7, 0, 0);
end;

procedure YsMakeUglyFontDisplayList inline;
var
  i: integer;
begin
  //check if list is already filled
  if glIsList(YsUglyFontBase) <> GL_TRUE then
    //create a list for each character
    for i := 0 to Pred(256) do
    begin
      glNewList(YsUglyFontBase + i, GL_COMPILE);
      YsDrawUglyFontPattern(YsUglyFontSet[i]);
      glEndList;
    end;
end;

procedure YsDrawUglyFont(str: string; centering: integer; useDisplayList: integer);
var
  l: integer;
  i: integer;
begin
  l := Length(str);
  glPushMatrix;

  if centering <> 0 then
    glTranslated(-l / 2, -0.5, 0);

  glScaled(1 / (YsUglyFontWid * 8 / 7), 1 / YsUglyFontHei, 1);

  if useDisplayList <> 0 then
  begin
    YsMakeUglyFontDisplayList;
    glPushAttrib(GL_LIST_BIT);
    glListBase(YsUglyFontBase);
    glCallLists(l, GL_UNSIGNED_BYTE, @str[1]);
    glPopAttrib;
  end
  else
  begin
    i := 0;
    while str[i] <> #0 do
    begin
      YsDrawUglyFontPattern(YsUglyFontSet[Ord(str[i])]);
      Inc(i);
    end;
  end;
  glPopMatrix;
end;

//simple GL wrapper
procedure glTextOut(x, y, z: double; sx, sy, sz: integer; center: integer; str: string);
begin
  glPushMatrix;
  glTranslatef(x, y, z);
  glScalef(sx, -sy, sz);
  YsDrawUglyFont(str, center);
  glPopMatrix;
end;

end.

