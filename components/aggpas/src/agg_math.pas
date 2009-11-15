//----------------------------------------------------------------------------
// Anti-Grain Geometry - Version 2.4 (Public License)
// Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)
//
// Anti-Grain Geometry - Version 2.4 Release Milano 3 (AggPas 2.4 RM3)
// Pascal Port By: Milan Marusinec alias Milano
//                 milan@marusinec.sk
//                 http://www.aggpas.org
// Copyright (c) 2005-2006
//
// Permission to copy, use, modify, sell and distribute this software
// is granted provided this copyright notice appears in all copies.
// This software is provided "as is" without express or implied
// warranty, and with no claim as to its suitability for any purpose.
//
//----------------------------------------------------------------------------
// Contact: mcseem@antigrain.com
//          mcseemagg@yahoo.com
//          http://www.antigrain.com
//
//----------------------------------------------------------------------------
// Bessel function (besj) was adapted for use in AGG library by Andy Wilk
// Contact: castor.vulgaris@gmail.com
//
// [Pascal Port History] -----------------------------------------------------
//
// 23.06.2006-Milano: ptrcomp adjustments
// 18.12.2005-Milano: Unit port establishment
//
{ agg_math.pas }
unit
 agg_math ;

INTERFACE

{$I agg_mode.inc }
{$Q- }
{$R- }
uses
 Math ,
 agg_basics ,
 agg_vertex_sequence ;

{ GLOBAL VARIABLES & CONSTANTS }
type
 double_xy_ptr = ^double_xy;
 double_xy = record
   x ,y : double;

  end;

 poly_xy_ptr = ^poly_xy;
 poly_xy = array[0..0 ] of double_xy;

 storage_xy_ptr = ^storage_xy;
 storage_xy = record
   poly : poly_xy_ptr;
   size : unsigned;

  end;

const
 intersection_epsilon : double = 1.0e-30;

// Tables for fast sqrt
const
 g_sqrt_table : array[0..1023 ] of int16u = (0 ,
  2048,2896,3547,4096,4579,5017,5418,5793,6144,6476,6792,7094,7384,7663,7932,8192,8444,
  8689,8927,9159,9385,9606,9822,10033,10240,10443,10642,10837,11029,11217,11403,11585,
  11765,11942,12116,12288,12457,12625,12790,12953,13114,13273,13430,13585,13738,13890,
  14040,14189,14336,14482,14626,14768,14910,15050,15188,15326,15462,15597,15731,15864,
  15995,16126,16255,16384,16512,16638,16764,16888,17012,17135,17257,17378,17498,17618,
  17736,17854,17971,18087,18203,18318,18432,18545,18658,18770,18882,18992,19102,19212,
  19321,19429,19537,19644,19750,19856,19961,20066,20170,20274,20377,20480,20582,20684,
  20785,20886,20986,21085,21185,21283,21382,21480,21577,21674,21771,21867,21962,22058,
  22153,22247,22341,22435,22528,22621,22713,22806,22897,22989,23080,23170,23261,23351,
  23440,23530,23619,23707,23796,23884,23971,24059,24146,24232,24319,24405,24491,24576,
  24661,24746,24831,24915,24999,25083,25166,25249,25332,25415,25497,25580,25661,25743,
  25824,25905,25986,26067,26147,26227,26307,26387,26466,26545,26624,26703,26781,26859,
  26937,27015,27092,27170,27247,27324,27400,27477,27553,27629,27705,27780,27856,27931,
  28006,28081,28155,28230,28304,28378,28452,28525,28599,28672,28745,28818,28891,28963,
  29035,29108,29180,29251,29323,29394,29466,29537,29608,29678,29749,29819,29890,29960,
  30030,30099,30169,30238,30308,30377,30446,30515,30583,30652,30720,30788,30856,30924,
  30992,31059,31127,31194,31261,31328,31395,31462,31529,31595,31661,31727,31794,31859,
  31925,31991,32056,32122,32187,32252,32317,32382,32446,32511,32575,32640,32704,32768,
  32832,32896,32959,33023,33086,33150,33213,33276,33339,33402,33465,33527,33590,33652,
  33714,33776,33839,33900,33962,34024,34086,34147,34208,34270,34331,34392,34453,34514,
  34574,34635,34695,34756,34816,34876,34936,34996,35056,35116,35176,35235,35295,35354,
  35413,35472,35531,35590,35649,35708,35767,35825,35884,35942,36001,36059,36117,36175,
  36233,36291,36348,36406,36464,36521,36578,36636,36693,36750,36807,36864,36921,36978,
  37034,37091,37147,37204,37260,37316,37372,37429,37485,37540,37596,37652,37708,37763,
  37819,37874,37929,37985,38040,38095,38150,38205,38260,38315,38369,38424,38478,38533,
  38587,38642,38696,38750,38804,38858,38912,38966,39020,39073,39127,39181,39234,39287,
  39341,39394,39447,39500,39553,39606,39659,39712,39765,39818,39870,39923,39975,40028,
  40080,40132,40185,40237,40289,40341,40393,40445,40497,40548,40600,40652,40703,40755,
  40806,40857,40909,40960,41011,41062,41113,41164,41215,41266,41317,41368,41418,41469,
  41519,41570,41620,41671,41721,41771,41821,41871,41922,41972,42021,42071,42121,42171,
  42221,42270,42320,42369,42419,42468,42518,42567,42616,42665,42714,42763,42813,42861,
  42910,42959,43008,43057,43105,43154,43203,43251,43300,43348,43396,43445,43493,43541,
  43589,43637,43685,43733,43781,43829,43877,43925,43972,44020,44068,44115,44163,44210,
  44258,44305,44352,44400,44447,44494,44541,44588,44635,44682,44729,44776,44823,44869,
  44916,44963,45009,45056,45103,45149,45195,45242,45288,45334,45381,45427,45473,45519,
  45565,45611,45657,45703,45749,45795,45840,45886,45932,45977,46023,46069,46114,46160,
  46205,46250,46296,46341,46386,46431,46477,46522,46567,46612,46657,46702,46746,46791,
  46836,46881,46926,46970,47015,47059,47104,47149,47193,47237,47282,47326,47370,47415,
  47459,47503,47547,47591,47635,47679,47723,47767,47811,47855,47899,47942,47986,48030,
  48074,48117,48161,48204,48248,48291,48335,48378,48421,48465,48508,48551,48594,48637,
  48680,48723,48766,48809,48852,48895,48938,48981,49024,49067,49109,49152,49195,49237,
  49280,49322,49365,49407,49450,49492,49535,49577,49619,49661,49704,49746,49788,49830,
  49872,49914,49956,49998,50040,50082,50124,50166,50207,50249,50291,50332,50374,50416,
  50457,50499,50540,50582,50623,50665,50706,50747,50789,50830,50871,50912,50954,50995,
  51036,51077,51118,51159,51200,51241,51282,51323,51364,51404,51445,51486,51527,51567,
  51608,51649,51689,51730,51770,51811,51851,51892,51932,51972,52013,52053,52093,52134,
  52174,52214,52254,52294,52334,52374,52414,52454,52494,52534,52574,52614,52654,52694,
  52734,52773,52813,52853,52892,52932,52972,53011,53051,53090,53130,53169,53209,53248,
  53287,53327,53366,53405,53445,53484,53523,53562,53601,53640,53679,53719,53758,53797,
  53836,53874,53913,53952,53991,54030,54069,54108,54146,54185,54224,54262,54301,54340,
  54378,54417,54455,54494,54532,54571,54609,54647,54686,54724,54762,54801,54839,54877,
  54915,54954,54992,55030,55068,55106,55144,55182,55220,55258,55296,55334,55372,55410,
  55447,55485,55523,55561,55599,55636,55674,55712,55749,55787,55824,55862,55900,55937,
  55975,56012,56049,56087,56124,56162,56199,56236,56273,56311,56348,56385,56422,56459,
  56497,56534,56571,56608,56645,56682,56719,56756,56793,56830,56867,56903,56940,56977,
  57014,57051,57087,57124,57161,57198,57234,57271,57307,57344,57381,57417,57454,57490,
  57527,57563,57599,57636,57672,57709,57745,57781,57817,57854,57890,57926,57962,57999,
  58035,58071,58107,58143,58179,58215,58251,58287,58323,58359,58395,58431,58467,58503,
  58538,58574,58610,58646,58682,58717,58753,58789,58824,58860,58896,58931,58967,59002,
  59038,59073,59109,59144,59180,59215,59251,59286,59321,59357,59392,59427,59463,59498,
  59533,59568,59603,59639,59674,59709,59744,59779,59814,59849,59884,59919,59954,59989,
  60024,60059,60094,60129,60164,60199,60233,60268,60303,60338,60373,60407,60442,60477,
  60511,60546,60581,60615,60650,60684,60719,60753,60788,60822,60857,60891,60926,60960,
  60995,61029,61063,61098,61132,61166,61201,61235,61269,61303,61338,61372,61406,61440,
  61474,61508,61542,61576,61610,61644,61678,61712,61746,61780,61814,61848,61882,61916,
  61950,61984,62018,62051,62085,62119,62153,62186,62220,62254,62287,62321,62355,62388,
  62422,62456,62489,62523,62556,62590,62623,62657,62690,62724,62757,62790,62824,62857,
  62891,62924,62957,62991,63024,63057,63090,63124,63157,63190,63223,63256,63289,63323,
  63356,63389,63422,63455,63488,63521,63554,63587,63620,63653,63686,63719,63752,63785,
  63817,63850,63883,63916,63949,63982,64014,64047,64080,64113,64145,64178,64211,64243,
  64276,64309,64341,64374,64406,64439,64471,64504,64536,64569,64601,64634,64666,64699,
  64731,64763,64796,64828,64861,64893,64925,64957,64990,65022,65054,65086,65119,65151,
  65183,65215,65247,65279,65312,65344,65376,65408,65440,65472,65504 );

 g_elder_bit_table : array[0..255 ] of int8 = (
  0,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
  5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7 );

{ GLOBAL PROCEDURES }
 function  calc_point_location(x1 ,y1 ,x2 ,y2 ,x ,y : double ) : double;

 function  point_in_triangle(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x ,y : double ) : boolean;

 function  calc_distance(x1 ,y1 ,x2 ,y2 : double ) : double;

 function  calc_line_point_distance(x1 ,y1 ,x2 ,y2 ,x ,y : double ) : double;

 function  calc_intersection(ax ,ay ,bx ,by ,cx ,cy ,dx ,dy : double; x ,y : double_ptr ) : boolean;

 function  intersection_exists(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 : double ) : boolean;

 procedure calc_orthogonal(thickness ,x1 ,y1 ,x2 ,y2 : double; x ,y : double_ptr );

 procedure dilate_triangle(x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double; x ,y : double_ptr; d : double );

 function  calc_triangle_area(x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double ) : double;

 function  calc_polygon_area(st : storage_xy_ptr ) : double;

 function  calc_polygon_area_vs(st : vertex_sequence_ptr ) : double;

 function  fast_sqrt(val : unsigned ) : unsigned;

 function  besj(x : double; n : int ) : double;

 function  cross_product(x1 ,y1 ,x2 ,y2 ,x ,y : double ) : double;


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ calc_point_location }
function calc_point_location;
begin
 result:=(x - x2 ) * (y2 - y1 ) - (y - y2 ) * (x2 - x1 );

end;

{ point_in_triangle }
function point_in_triangle;
var
 cp1 ,cp2 ,cp3 : boolean;

begin
 cp1:=calc_point_location(x1 ,y1 ,x2 ,y2 ,x ,y ) < 0.0;
 cp2:=calc_point_location(x2 ,y2 ,x3 ,y3 ,x ,y ) < 0.0;
 cp3:=calc_point_location(x3 ,y3 ,x1 ,y1 ,x ,y ) < 0.0;

 result:=(cp1 = cp2 ) and (cp2 = cp3 ) and (cp3 = cp1 );

end;

{ calc_distance }
function calc_distance;
var
 dx ,dy : double;

begin
 dx:=x2 - x1;
 dy:=y2 - y1;

 result:=sqrt(dx * dx + dy * dy );

end;

{ calc_line_point_distance }
function calc_line_point_distance;
var
 d  ,
 dx ,
 dy : double;

begin
 dx:=x2 - x1;
 dy:=y2 - y1;
 d :=sqrt(dx * dx + dy * dy );

 if d < intersection_epsilon then
  result:=calc_distance(x1 ,y1 ,x ,y )
 else
  result:=((x - x2 ) * dy - (y - y2 ) * dx) / d;

end;

{ calc_intersection }
function calc_intersection;
var
 r ,

 num ,
 den : double;

begin
 num:=(ay - cy ) * (dx - cx ) - (ax - cx ) * (dy - cy );
 den:=(bx - ax ) * (dy - cy ) - (by - ay ) * (dx - cx );

 if Abs(den ) < intersection_epsilon then
  result:=false
  
 else
  begin
   r :=num / den;
   x^:=ax + r * (bx - ax );
   y^:=ay + r * (by - ay );

   result:=true;

  end;

end;

{ intersection_exists }
function intersection_exists;
var
 dx1 ,dy1 ,
 dx2 ,dy2 : double;

begin
 dx1:=x2 - x1;
 dy1:=y2 - y1;
 dx2:=x4 - x3;
 dy2:=y4 - y3;

 result:=
  (((x3 - x2 ) * dy1 - (y3 - y2 ) * dx1 < 0.0 ) <>
   ((x4 - x2 ) * dy1 - (y4 - y2 ) * dx1 < 0.0 ) ) and
  (((x1 - x4 ) * dy2 - (y1 - y4 ) * dx2 < 0.0 ) <>
   ((x2 - x4 ) * dy2 - (y2 - y4 ) * dx2 < 0.0 ) );
   
end;

{ calc_orthogonal }
procedure calc_orthogonal;
var
 d ,dx ,dy : double;

begin
 dx:=x2 - x1;
 dy:=y2 - y1;
 d :=sqrt(dx * dx + dy * dy );
 x^:=thickness * dy / d;
 y^:=thickness * dx / d;

end;

{ dilate_triangle }
procedure dilate_triangle;
var
 loc ,
 dx1 ,dy1 ,
 dx2 ,dy2 ,
 dx3 ,dy3 : double;

begin
 dx1:=0.0;
 dy1:=0.0;
 dx2:=0.0;
 dy2:=0.0;
 dx3:=0.0;
 dy3:=0.0;
 loc:=calc_point_location(x1 ,y1 ,x2 ,y2 ,x3 ,y3 );

 if Abs(loc ) > intersection_epsilon then
  begin
   if calc_point_location(x1 ,y1 ,x2 ,y2 ,x3 ,y3 ) > 0.0 then
    d:=-d;

   calc_orthogonal(d ,x1 ,y1 ,x2 ,y2 ,@dx1 ,@dy1 );
   calc_orthogonal(d ,x2 ,y2 ,x3 ,y3 ,@dx2 ,@dy2 );
   calc_orthogonal(d ,x3 ,y3 ,x1 ,y1 ,@dx3 ,@dy3 );

  end;

 x^:=x1 + dx1; inc(ptrcomp(x ) ,sizeof(double ) );
 y^:=y1 - dy1; inc(ptrcomp(y ) ,sizeof(double ) );
 x^:=x2 + dx1; inc(ptrcomp(x ) ,sizeof(double ) );
 y^:=y2 - dy1; inc(ptrcomp(y ) ,sizeof(double ) );
 x^:=x2 + dx2; inc(ptrcomp(x ) ,sizeof(double ) );
 y^:=y2 - dy2; inc(ptrcomp(y ) ,sizeof(double ) );
 x^:=x3 + dx2; inc(ptrcomp(x ) ,sizeof(double ) );
 y^:=y3 - dy2; inc(ptrcomp(y ) ,sizeof(double ) );
 x^:=x3 + dx3; inc(ptrcomp(x ) ,sizeof(double ) );
 y^:=y3 - dy3; inc(ptrcomp(y ) ,sizeof(double ) );
 x^:=x1 + dx3; inc(ptrcomp(x ) ,sizeof(double ) );
 y^:=y1 - dy3; inc(ptrcomp(y ) ,sizeof(double ) );

end;

{ calc_triangle_area }
function calc_triangle_area;
begin
 result:=(x1 * y2 - x2 * y1 + x2 * y3 - x3 * y2 + x3 * y1 - x1 * y3 ) * 0.5;

end;

{ calc_polygon_area }
function calc_polygon_area;
var
 i : unsigned;
 v : double_xy_ptr;

 x ,y ,sum ,xs ,ys : double;

begin
 sum:=0.0;
 x  :=st.poly[0 ].x;
 y  :=st.poly[0 ].y;
 xs :=x;
 ys :=y;

 if st.size > 0 then
  for i:=1 to st.size - 1 do
   begin
    v:=@st.poly[i ];

    sum:=sum + (x * v.y - y * v.x );

    x:=v.x;
    y:=v.y;

   end;

 result:=(sum + x * ys - y * xs ) * 0.5;

end;

{ calc_polygon_area_vs }
function calc_polygon_area_vs;
var
 i : unsigned;
 v : vertex_dist_ptr;

 x ,y ,sum ,xs ,ys : double;

begin
 sum:=0.0;
 x  :=vertex_dist_ptr(st.array_operator(0 ) ).x;
 y  :=vertex_dist_ptr(st.array_operator(0 ) ).y;
 xs :=x;
 ys :=y;

 if st.size > 0 then
  for i:=1 to st.size - 1 do
   begin
    v:=st.array_operator(i );

    sum:=sum + (x * v.y - y * v.x );

    x:=v.x;
    y:=v.y;

   end;

 result:=(sum + x * ys - y * xs ) * 0.5;

end;

{ fast_sqrt }
function fast_sqrt;
var
 bit : int;

 t ,shift : unsigned;

begin
 t  :=val;
 bit:=0;

 shift:=11;

//The following piece of code is just an emulation of the
//Ix86 assembler command "bsr" (see below). However on old
//Intels (like Intel MMX 233MHz) this code is about twice
//faster (sic!) then just one "bsr". On PIII and PIV the
//bsr is optimized quite well.
 bit:=t shr 24;

 if bit <> 0 then
  bit:=g_elder_bit_table[bit ] + 24
  
 else
  begin
   bit:=(t shr 16 ) and $FF;

   if bit <> 0 then
    bit:=g_elder_bit_table[bit ] + 16
   else
    begin
     bit:=(t shr 8 ) and $FF;

     if bit <> 0 then
      bit:=g_elder_bit_table[bit ] + 8
     else
      bit:=g_elder_bit_table[t ];

    end;

  end;

// This is calculation sqrt itself.
 bit:=bit - 9;

 if bit > 0 then
  begin
   bit  :=(shr_int32(bit ,1 ) ) + (bit and 1 );
   shift:=shift - bit;
   val  :=val shr (bit shl 1 );

  end;

 result:=g_sqrt_table[val ] shr shift;

end;

//--------------------------------------------------------------------besj
// Function BESJ calculates Bessel function of first kind of order n
// Arguments:
//     n - an integer (>=0), the order
//     x - value at which the Bessel function is required
//--------------------
// C++ Mathematical Library
// Convereted from equivalent FORTRAN library
// Converetd by Gareth Walker for use by course 392 computational project
// All functions tested and yield the same results as the corresponding
// FORTRAN versions.
//
// If you have any problems using these functions please report them to
// M.Muldoon@UMIST.ac.uk
//
// Documentation available on the web
// http://www.ma.umist.ac.uk/mrm/Teaching/392/libs/392.html
// Version 1.0   8/98
// 29 October, 1999
//--------------------
// Adapted for use in AGG library by Andy Wilk (castor.vulgaris@gmail.com)
//------------------------------------------------------------------------
{ besj }
function besj;
var
 i ,m1 ,m2 ,m8 ,imax : int;

 d ,b ,b1 ,c2 ,c3 ,c4 ,c6 : double;

begin
 if n < 0 then
  begin
   result:=0;

   exit;

  end;

 d:=1E-6;
 b:=0;

 if Abs(x ) <= d then
  begin
   if n <> 0 then
    result:=0
   else
    result:=1;

   exit;

  end;

 b1:=0; // b1 is the value from the previous iteration

// Set up a starting order for recurrence
 m1:=trunc(Abs(x ) + 6 );

 if Abs(x ) > 5 then
  m1:=trunc(Abs(1.4 * x + 60 / x ) );

 m2:=trunc(n + 2 + Abs(x ) / 4 );
 
 if m1 > m2 then
  m2:=m1;

// Apply recurrence down from curent max order
 repeat
  c3:=0;
  c2:=1E-30;
  c4:=0;
  m8:=1;

  if m2 div 2 * 2 = m2 then
   m8:=-1;

  imax:=m2 - 2; 

  for i:=1 to imax do
   begin
    c6:=2 * (m2 - i ) * c2 / x - c3;
    c3:=c2;
    c2:=c6;

    if m2 - i - 1 = n then
     b:=c6;

    m8:=-1 * m8;

    if m8 > 0 then
     c4:=c4 + 2 * c6;

   end;

  c6:=2 * c2 / x - c3;

  if n = 0 then
   b:=c6;

  c4:=c4 + c6;
  b :=b / c4;
  
  if Abs(b - b1 ) < d then
   begin
    result:=b;

    exit;

   end;

  b1:=b;
  
  inc(m2 ,3 );

 until false;

end;

{ CROSS_PRODUCT }
function cross_product(x1 ,y1 ,x2 ,y2 ,x ,y : double ) : double;
begin
 result:=(x - x2 ) * (y2 - y1 ) - (y - y2 ) * (x2 - x1 );

end;

END.

