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
// [Pascal Port History] -----------------------------------------------------
//
// 03.01.2007-Milano: Adjustments for ExpatWrap (Win,Linux & Mac)
// 23.06.2006-Milano: ptrcomp adjustments
// 24.04.2006-Milano: Unit port establishment
//
{ agg_svg_parser.pas }
unit
 agg_svg_parser_lcl ;

INTERFACE

{$DEFINE EXPAT_WRAPPER }
{$I agg_mode.inc }

uses
 Classes, SysUtils ,
 agg_basics ,
 agg_color ,
 agg_svg_path_tokenizer ,
 agg_svg_path_renderer ,
 agg_svg_exception ,
 agg_trans_affine ,
 agg_math_stroke ,
 expat ,
 FileUtil ;

{ TYPES DEFINITION }
const
 buf_size = 512;

type
 parser_ptr = ^parser;

 { parser }

 parser = object
   m_path      : path_renderer_ptr;
   m_tokenizer : path_tokenizer;

   m_buf   ,
   m_title : char_ptr;

   m_title_len  : unsigned;
   m_title_flag ,
   m_path_flag  : boolean;
   m_attr_name  ,
   m_attr_value : char_ptr;

   m_attr_name_len   ,
   m_attr_name_aloc  ,
   m_attr_value_len  ,
   m_attr_value_aloc : unsigned;

   constructor Construct(path : path_renderer_ptr );
   destructor  Destruct;

   procedure parse(fname : string ); overload; // UTF8
   procedure parse(sourcestream: TStream); overload;
   function  title : char_ptr;

  // XML event handlers
   procedure parse_attr     (attr : char_ptr_ptr ); overload;
   procedure parse_path     (attr : char_ptr_ptr );
   procedure parse_poly     (attr : char_ptr_ptr; close_flag : boolean );
   procedure parse_rect     (attr : char_ptr_ptr );
   procedure parse_line     (attr : char_ptr_ptr );
   procedure parse_style    (str : agg_basics.char_ptr );
   procedure parse_transform(str : agg_basics.char_ptr );

   function  parse_matrix   (str : agg_basics.char_ptr ) : unsigned;
   function  parse_translate(str : agg_basics.char_ptr ) : unsigned;
   function  parse_rotate   (str : agg_basics.char_ptr ) : unsigned;
   function  parse_scale    (str : agg_basics.char_ptr ) : unsigned;
   function  parse_skew_x   (str : agg_basics.char_ptr ) : unsigned;
   function  parse_skew_y   (str : agg_basics.char_ptr ) : unsigned;

   function  parse_attr      (name ,value : agg_basics.char_ptr ) : boolean; overload;
   function  parse_name_value(nv_start ,nv_end : agg_basics.char_ptr ) : boolean;

   procedure copy_name (start ,end_ : agg_basics.char_ptr );
   procedure copy_value(start ,end_ : agg_basics.char_ptr );

  end;

{ GLOBAL PROCEDURES }
 procedure start_element(data : pointer; el : char_ptr; attr : char_ptr_ptr ); {$IFDEF EXPAT_WRAPPER }cdecl; {$ENDIF }
 procedure end_element  (data : pointer; el : char_ptr ); {$IFDEF EXPAT_WRAPPER }cdecl; {$ENDIF }
 procedure content      (data : pointer; s : char_ptr; len : int ); {$IFDEF EXPAT_WRAPPER }cdecl; {$ENDIF }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
type
 named_color_ptr = ^named_color;
 named_color = record
   name : array[0..21 ] of char;

   r ,g ,b ,a : int8u;

  end;

const
 colors_num = 148;

 colors : array[0..colors_num - 1 ] of named_color =
  ((name:'aliceblue';            r:240; g:248; b:255; a:255 ) ,
   (name:'antiquewhite';         r:250; g:235; b:215; a:255 ) ,
   (name:'aqua';                 r:0;   g:255; b:255; a:255 ) ,
   (name:'aquamarine';           r:127; g:255; b:212; a:255 ) ,
   (name:'azure';                r:240; g:255; b:255; a:255 ) ,
   (name:'beige';                r:245; g:245; b:220; a:255 ) ,
   (name:'bisque';               r:255; g:228; b:196; a:255 ) ,
   (name:'black';                r:0;   g:0;   b:0;   a:255 ) ,
   (name:'blanchedalmond';       r:255; g:235; b:205; a:255 ) ,
   (name:'blue';                 r:0;   g:0;   b:255; a:255 ) ,
   (name:'blueviolet';           r:138; g:43;  b:226; a:255 ) ,
   (name:'brown';                r:165; g:42;  b:42;  a:255 ) ,
   (name:'burlywood';            r:222; g:184; b:135; a:255 ) ,
   (name:'cadetblue';            r:95;  g:158; b:160; a:255 ) ,
   (name:'chartreuse';           r:127; g:255; b:0;   a:255 ) ,
   (name:'chocolate';            r:210; g:105; b:30;  a:255 ) ,
   (name:'coral';                r:255; g:127; b:80;  a:255 ) ,
   (name:'cornflowerblue';       r:100; g:149; b:237; a:255 ) ,
   (name:'cornsilk';             r:255; g:248; b:220; a:255 ) ,
   (name:'crimson';              r:220; g:20;  b:60;  a:255 ) ,
   (name:'cyan';                 r:0;   g:255; b:255; a:255 ) ,
   (name:'darkblue';             r:0;   g:0;   b:139; a:255 ) ,
   (name:'darkcyan';             r:0;   g:139; b:139; a:255 ) ,
   (name:'darkgoldenrod';        r:184; g:134; b:11;  a:255 ) ,
   (name:'darkgray';             r:169; g:169; b:169; a:255 ) ,
   (name:'darkgreen';            r:0;   g:100; b:0;   a:255 ) ,
   (name:'darkgrey';             r:169; g:169; b:169; a:255 ) ,
   (name:'darkkhaki';            r:189; g:183; b:107; a:255 ) ,
   (name:'darkmagenta';          r:139; g:0;   b:139; a:255 ) ,
   (name:'darkolivegreen';       r:85;  g:107; b:47;  a:255 ) ,
   (name:'darkorange';           r:255; g:140; b:0;   a:255 ) ,
   (name:'darkorchid';           r:153; g:50;  b:204; a:255 ) ,
   (name:'darkred';              r:139; g:0;   b:0;   a:255 ) ,
   (name:'darksalmon';           r:233; g:150; b:122; a:255 ) ,
   (name:'darkseagreen';         r:143; g:188; b:143; a:255 ) ,
   (name:'darkslateblue';        r:72;  g:61;  b:139; a:255 ) ,
   (name:'darkslategray';        r:47;  g:79;  b:79;  a:255 ) ,
   (name:'darkslategrey';        r:47;  g:79;  b:79;  a:255 ) ,
   (name:'darkturquoise';        r:0;   g:206; b:209; a:255 ) ,
   (name:'darkviolet';           r:148; g:0;   b:211; a:255 ) ,
   (name:'deeppink';             r:255; g:20;  b:147; a:255 ) ,
   (name:'deepskyblue';          r:0;   g:191; b:255; a:255 ) ,
   (name:'dimgray';              r:105; g:105; b:105; a:255 ) ,
   (name:'dimgrey';              r:105; g:105; b:105; a:255 ) ,
   (name:'dodgerblue';           r:30;  g:144; b:255; a:255 ) ,
   (name:'firebrick';            r:178; g:34;  b:34;  a:255 ) ,
   (name:'floralwhite';          r:255; g:250; b:240; a:255 ) ,
   (name:'forestgreen';          r:34;  g:139; b:34;  a:255 ) ,
   (name:'fuchsia';              r:255; g:0;   b:255; a:255 ) ,
   (name:'gainsboro';            r:220; g:220; b:220; a:255 ) ,
   (name:'ghostwhite';           r:248; g:248; b:255; a:255 ) ,
   (name:'gold';                 r:255; g:215; b:0;   a:255 ) ,
   (name:'goldenrod';            r:218; g:165; b:32;  a:255 ) ,
   (name:'gray';                 r:128; g:128; b:128; a:255 ) ,
   (name:'green';                r:0;   g:128; b:0;   a:255 ) ,
   (name:'greenyellow';          r:173; g:255; b:47;  a:255 ) ,
   (name:'grey';                 r:128; g:128; b:128; a:255 ) ,
   (name:'honeydew';             r:240; g:255; b:240; a:255 ) ,
   (name:'hotpink';              r:255; g:105; b:180; a:255 ) ,
   (name:'indianred';            r:205; g:92;  b:92;  a:255 ) ,
   (name:'indigo';               r:75;  g:0;   b:130; a:255 ) ,
   (name:'ivory';                r:255; g:255; b:240; a:255 ) ,
   (name:'khaki';                r:240; g:230; b:140; a:255 ) ,
   (name:'lavender';             r:230; g:230; b:250; a:255 ) ,
   (name:'lavenderblush';        r:255; g:240; b:245; a:255 ) ,
   (name:'lawngreen';            r:124; g:252; b:0;   a:255 ) ,
   (name:'lemonchiffon';         r:255; g:250; b:205; a:255 ) ,
   (name:'lightblue';            r:173; g:216; b:230; a:255 ) ,
   (name:'lightcoral';           r:240; g:128; b:128; a:255 ) ,
   (name:'lightcyan';            r:224; g:255; b:255; a:255 ) ,
   (name:'lightgoldenrodyellow'; r:250; g:250; b:210; a:255 ) ,
   (name:'lightgray';            r:211; g:211; b:211; a:255 ) ,
   (name:'lightgreen';           r:144; g:238; b:144; a:255 ) ,
   (name:'lightgrey';            r:211; g:211; b:211; a:255 ) ,
   (name:'lightpink';            r:255; g:182; b:193; a:255 ) ,
   (name:'lightsalmon';          r:255; g:160; b:122; a:255 ) ,
   (name:'lightseagreen';        r:32;  g:178; b:170; a:255 ) ,
   (name:'lightskyblue';         r:135; g:206; b:250; a:255 ) ,
   (name:'lightslategray';       r:119; g:136; b:153; a:255 ) ,
   (name:'lightslategrey';       r:119; g:136; b:153; a:255 ) ,
   (name:'lightsteelblue';       r:176; g:196; b:222; a:255 ) ,
   (name:'lightyellow';          r:255; g:255; b:224; a:255 ) ,
   (name:'lime';                 r:0;   g:255; b:0;   a:255 ) ,
   (name:'limegreen';            r:50;  g:205; b:50;  a:255 ) ,
   (name:'linen';                r:250; g:240; b:230; a:255 ) ,
   (name:'magenta';              r:255; g:0;   b:255; a:255 ) ,
   (name:'maroon';               r:128; g:0;   b:0;   a:255 ) ,
   (name:'mediumaquamarine';     r:102; g:205; b:170; a:255 ) ,
   (name:'mediumblue';           r:0;   g:0;   b:205; a:255 ) ,
   (name:'mediumorchid';         r:186; g:85;  b:211; a:255 ) ,
   (name:'mediumpurple';         r:147; g:112; b:219; a:255 ) ,
   (name:'mediumseagreen';       r:60;  g:179; b:113; a:255 ) ,
   (name:'mediumslateblue';      r:123; g:104; b:238; a:255 ) ,
   (name:'mediumspringgreen';    r:0;   g:250; b:154; a:255 ) ,
   (name:'mediumturquoise';      r:72;  g:209; b:204; a:255 ) ,
   (name:'mediumvioletred';      r:199; g:21;  b:133; a:255 ) ,
   (name:'midnightblue';         r:25;  g:25;  b:112; a:255 ) ,
   (name:'mintcream';            r:245; g:255; b:250; a:255 ) ,
   (name:'mistyrose';            r:255; g:228; b:225; a:255 ) ,
   (name:'moccasin';             r:255; g:228; b:181; a:255 ) ,
   (name:'navajowhite';          r:255; g:222; b:173; a:255 ) ,
   (name:'navy';                 r:0;   g:0;   b:128; a:255 ) ,
   (name:'oldlace';              r:253; g:245; b:230; a:255 ) ,
   (name:'olive';                r:128; g:128; b:0;   a:255 ) ,
   (name:'olivedrab';            r:107; g:142; b:35;  a:255 ) ,
   (name:'orange';               r:255; g:165; b:0;   a:255 ) ,
   (name:'orangered';            r:255; g:69;  b:0;   a:255 ) ,
   (name:'orchid';               r:218; g:112; b:214; a:255 ) ,
   (name:'palegoldenrod';        r:238; g:232; b:170; a:255 ) ,
   (name:'palegreen';            r:152; g:251; b:152; a:255 ) ,
   (name:'paleturquoise';        r:175; g:238; b:238; a:255 ) ,
   (name:'palevioletred';        r:219; g:112; b:147; a:255 ) ,
   (name:'papayawhip';           r:255; g:239; b:213; a:255 ) ,
   (name:'peachpuff';            r:255; g:218; b:185; a:255 ) ,
   (name:'peru';                 r:205; g:133; b:63;  a:255 ) ,
   (name:'pink';                 r:255; g:192; b:203; a:255 ) ,
   (name:'plum';                 r:221; g:160; b:221; a:255 ) ,
   (name:'powderblue';           r:176; g:224; b:230; a:255 ) ,
   (name:'purple';               r:128; g:0;   b:128; a:255 ) ,
   (name:'red';                  r:255; g:0;   b:0;   a:255 ) ,
   (name:'rosybrown';            r:188; g:143; b:143; a:255 ) ,
   (name:'royalblue';            r:65;  g:105; b:225; a:255 ) ,
   (name:'saddlebrown';          r:139; g:69;  b:19;  a:255 ) ,
   (name:'salmon';               r:250; g:128; b:114; a:255 ) ,
   (name:'sandybrown';           r:244; g:164; b:96;  a:255 ) ,
   (name:'seagreen';             r:46;  g:139; b:87;  a:255 ) ,
   (name:'seashell';             r:255; g:245; b:238; a:255 ) ,
   (name:'sienna';               r:160; g:82;  b:45;  a:255 ) ,
   (name:'silver';               r:192; g:192; b:192; a:255 ) ,
   (name:'skyblue';              r:135; g:206; b:235; a:255 ) ,
   (name:'slateblue';            r:106; g:90;  b:205; a:255 ) ,
   (name:'slategray';            r:112; g:128; b:144; a:255 ) ,
   (name:'slategrey';            r:112; g:128; b:144; a:255 ) ,
   (name:'snow';                 r:255; g:250; b:250; a:255 ) ,
   (name:'springgreen';          r:0;   g:255; b:127; a:255 ) ,
   (name:'steelblue';            r:70;  g:130; b:180; a:255 ) ,
   (name:'tan';                  r:210; g:180; b:140; a:255 ) ,
   (name:'teal';                 r:0;   g:128; b:128; a:255 ) ,
   (name:'thistle';              r:216; g:191; b:216; a:255 ) ,
   (name:'tomato';               r:255; g:99;  b:71;  a:255 ) ,
   (name:'turquoise';            r:64;  g:224; b:208; a:255 ) ,
   (name:'violet';               r:238; g:130; b:238; a:255 ) ,
   (name:'wheat';                r:245; g:222; b:179; a:255 ) ,
   (name:'white';                r:255; g:255; b:255; a:255 ) ,
   (name:'whitesmoke';           r:245; g:245; b:245; a:255 ) ,
   (name:'yellow';               r:255; g:255; b:0;   a:255 ) ,
   (name:'yellowgreen';          r:154; g:205; b:50;  a:255 ) ,
   (name:'zzzzzzzzzzz';          r:0;   g:0;   b:0;   a:0   ) );

 pageEqHigh : shortstring =
  #1#2#3#4#5#6#7#8#9#10#11#12#13#14#15#16 +
  #17#18#19#20#21#22#23#24#25#26#27#28#29#30#31#32 +
  #33#34#35#36#37#38#39#40#41#42#43#44#45#46#47#48 +
  #49#50#51#52#53#54#55#56#57#58#59#60#61#62#63#64 +
  #65#66#67#68#69#70#71#72#73#74#75#76#77#78#79#80 +
  #81#82#83#84#85#86#87#88#89#90#91#92#93#94#95#96 +
  #65#66#67#68#69#70#71#72#73#74#75#76#77#78#79#80 +
  #81#82#83#84#85#86#87#88#89#90#123#124#125#126#127#128 +
  #129#130#131#132#133#134#135#136#137#138#139#140#141#142#143#144 +
  #145#146#147#148#149#150#151#152#153#154#155#156#157#158#159#160 +
  #161#162#163#164#165#166#167#168#169#170#171#172#173#174#175#176 +
  #177#178#179#180#181#182#183#184#185#186#187#188#189#190#191#192 +
  #193#194#195#196#197#198#199#200#201#202#203#204#205#206#207#208 +
  #209#210#211#212#213#214#215#216#217#218#219#220#221#222#223#224 +
  #225#226#227#228#229#230#231#232#233#234#235#236#237#238#239#240 +
  #241#242#243#244#245#246#247#248#249#250#251#252#253#254#255;

{ UNIT IMPLEMENTATION }
{ START_ELEMENT }
procedure start_element;
var
 this : parser_ptr;
 
begin
 this:=parser_ptr(data );

 if StrComp(PChar(el ) ,'title' ) = 0 then
  this.m_title_flag:=true
 else
  if StrComp(PChar(el ) ,'g' ) = 0 then
   begin
    this.m_path.push_attr;
    this.parse_attr(attr );

   end
  else
   if StrComp(PChar(el ) ,'path' ) = 0 then
    begin
     if this.m_path_flag then
      raise svg_exception.Construct(PChar('start_element: Nested path' ) );

     this.m_path.begin_path;
     this.parse_path(attr );
     this.m_path.end_path;

     this.m_path_flag:=true;

    end
   else
    if StrComp(PChar(el ) ,'rect' ) = 0 then
     this.parse_rect(attr )
    else
     if StrComp(PChar(el ) ,'line' ) = 0 then
      this.parse_line(attr )
     else
      if StrComp(PChar(el ) ,'polyline' ) = 0 then
       this.parse_poly(attr ,false )
      else
       if StrComp(PChar(el ) ,'polygon' ) = 0 then
        this.parse_poly(attr ,true );

     //else
     // if StrComp(PChar(el ) ,'<OTHER_ELEMENTS>' ) = 0 then
     //  begin
     //  end
     //...

end;

{ END_ELEMENT }
procedure end_element;
var
 this : parser_ptr;

begin
 this:=parser_ptr(data );

 if StrComp(PChar(el ) ,'title' ) = 0 then
  this.m_title_flag:=false
 else
  if StrComp(PChar(el ) ,'g' ) = 0 then
   this.m_path.pop_attr
  else
   if StrComp(PChar(el ) ,'path' ) = 0 then
    this.m_path_flag:=false;

 //else
 // if StrComp(PChar(el ) ,'<OTHER_ELEMENTS>' ) = 0 then
 //  begin
 //  end
 // ...

end;

{ CONTENT }
procedure content;
var
 this : parser_ptr;

begin
 this:=parser_ptr(data );

// m_title_flag signals that the <title> tag is being parsed now.
// The following code concatenates the pieces of content of the <title> tag.
 if this.m_title_flag then
  begin
   if len + this.m_title_len > 255 then
    len:=255 - this.m_title_len;

   if len > 0 then
    begin
     move(
      s^ ,
      char_ptr(ptrcomp(this.m_title ) + this.m_title_len )^ ,
      len );

     inc(this.m_title_len ,len );

     char_ptr(ptrcomp(this.m_title ) + this.m_title_len )^:=#0;

    end;

  end;

end;

{ hex_unsigned }
function hex_unsigned(hexstr : agg_basics.char_ptr ) : unsigned;

function xyint(x ,y : integer ) : integer;
var
 f : integer;
 m : boolean;

begin
 m:=false;

 if y < 0 then
  begin
   y:=y * -1;
   m:=true;

  end;

 result:=x;

 if y > 1 then
  for f:=1 to y - 1 do
   result:=result * x;

 if m then
  result:=result * -1;

end;

var
 h   : shortstring;
 fcb : byte;
 yps ,
 mul ,
 num : unsigned;

label
 Err ,Esc ;

const
 hex : string[16 ] = '0123456789ABCDEF';

begin
 h:='';

 while hexstr^ <> #0 do
  begin
   h:=h + pageEqHigh[byte(hexstr^ ) ];

   inc(ptrcomp(hexstr ) );

  end;

 if length(h ) > 0 then
  begin
   case h[length(h ) ] of
    '0'..'9' ,'A'..'F' :
    else
     goto Err;

   end;

   num:=pos(h[length(h ) ] ,hex ) - 1;
   yps:=2;
   mul:=xyint(4 ,yps );

   if length(h ) > 1 then
    for fcb:=length(h ) - 1 downto 1 do
     begin
      case h[fcb ] of
       '0'..'9' ,'A'..'F' :
       else
        goto Err;

      end;

      inc(num ,(pos(h[fcb ] ,hex ) - 1 ) * mul );
      inc(yps ,2 );

      mul:=xyint(4 ,yps );

     end;

   goto Esc;

  end;

Err:
 num:=0;

Esc:
 result:=num;

end;

{ parse_color }
function parse_color(str : agg_basics.char_ptr ) : aggclr;
var
 u : unsigned;
 p : named_color_ptr;
 m : shortstring;

begin
 while str^ = ' ' do
  inc(ptrcomp(str ) );

 if str^ = '#' then
  begin
   inc(ptrcomp(str ) );

   u:=hex_unsigned(str );

   result.Construct(rgb8_packed(u ) );

  end
 else
  begin
   p:=NIL;

   for u:=0 to colors_num - 1 do
    if StrComp(colors[u ].name ,PChar(str ) ) = 0 then
     begin
      p:=@colors[u ];

      break;

     end;

   if p = NIL then
    begin
     m:='parse_color: Invalid color name ' + StrPas(PChar(str ) ) + #0;

     raise svg_exception.Construct(PChar(@m[1 ] ) );

    end;

   result.ConstrInt(p.r ,p.g ,p.b ,p.a );

  end;

end;

{ parse_double }
function parse_double(str : agg_basics.char_ptr ) : double;
begin
 while str^ = ' ' do
  inc(ptrcomp(str ) );

 result:=get_double(pointer(PChar(str ) ) );

end;

{ islower }
function islower(ch : char ) : boolean;
begin
 case ch of
  #97..#122 :
   result:=true;

  else
   result:=false;

 end;

end;

{ is_numeric }
function is_numeric(ch : char ) : boolean;
begin
 result:=Pos(ch ,'0123456789+-.eE' ) <> 0;

end;

{ parse_transform_args }
function parse_transform_args(str : agg_basics.char_ptr; args : double_ptr; max_na : unsigned; na : unsigned_ptr ) : unsigned;
var
 ptr ,end_ : agg_basics.char_ptr;

begin
 na^:=0;
 ptr:=str;

 while (ptr^ <> #0 ) and
       (ptr^ <> '(' ) do
  inc(ptrcomp(ptr ) );

 if ptr^ = #0 then
  raise svg_exception.Construct(PChar('parse_transform_args: Invalid syntax' ) );

 end_:=ptr;

 while (end_^ <> #0 ) and
       (end_^ <> ')' ) do
  inc(ptrcomp(end_ ) );

 if end_^ = #0 then
  raise svg_exception.Construct(PChar('parse_transform_args: Invalid syntax' ) );

 while ptrcomp(ptr ) < ptrcomp(end_ ) do
  if is_numeric(ptr^ ) then
   begin
    if na^ >= max_na then
     raise svg_exception.Construct(PChar('parse_transform_args: Too many arguments' ) );

    double_ptr(ptrcomp(args ) + na^ * sizeof(double ) )^:=get_double(ptr );

    inc(na^ );

    while (ptrcomp(ptr ) < ptrcomp(end_ ) ) and
          is_numeric(ptr^ ) do
     inc(ptrcomp(ptr ) );

   end
  else
   inc(ptrcomp(ptr ) );

 result:=unsigned(ptrcomp(end_ ) - ptrcomp(str ) );

end;

{ CONSTRUCT }
constructor parser.Construct(path : path_renderer_ptr );
begin
 m_path:=path;

 m_tokenizer.Construct;

 agg_getmem(pointer(m_buf ) ,buf_size );
 agg_getmem(pointer(m_title ) ,256 );

 m_title_len :=0;
 m_title_flag:=false;
 m_path_flag :=false;

 m_attr_name_aloc :=128;
 m_attr_value_aloc:=1024;

 agg_getmem(pointer(m_attr_name ) ,m_attr_name_aloc );
 agg_getmem(pointer(m_attr_value ) ,m_attr_value_aloc );

 m_attr_name_len :=127;
 m_attr_value_len:=1023;

 m_title^:=#0;

end;

{ DESTRUCT }
destructor parser.Destruct;
begin
 agg_freemem(pointer(m_attr_value ) ,m_attr_value_aloc );
 agg_freemem(pointer(m_attr_name ) ,m_attr_name_aloc );
 agg_freemem(pointer(m_title ) ,256 );
 agg_freemem(pointer(m_buf ) ,buf_size );

end;

procedure parser.parse(fname: string);
var
  fs: TFileStream;
begin
  fs:=TFileStream.Create(UTF8ToSys(fname),fmOpenRead+fmShareDenyWrite);
  try
    parse(fs);
  finally
    fs.Free;
  end;
end;

{ PARSE }
procedure parser.parse(sourcestream: TStream);
var
 p  : XML_Parser;
 ts : char_ptr;

 done : boolean;
 len  : int;

 Msg : ansistring;
begin
 p:=XML_ParserCreate(NIL );

 if p = NIL then
  raise svg_exception.Construct(PChar('Couldn''t allocate memory for parser' ) );
 try
   XML_SetUserData            (p ,@self );
   XML_SetElementHandler      (p ,@start_element ,@end_element );
   XML_SetCharacterDataHandler(p ,@content );

   done:=false;

   repeat
    len:=sourcestream.Read(m_buf^,buf_size);

    done:=len < buf_size;
    writeln('parser.parse ',done,' ',len,' ',buf_size);

    if XML_Parse(p ,pointer(m_buf ) ,len ,int(done ) ) = XML_STATUS_ERROR then
     begin
      XML_ParserFree(p );
      Msg:=PChar(XML_ErrorString(XML_GetErrorCode(p)));
      Msg:=' at line '+IntToStr(XML_GetCurrentLineNumber(p));
      raise svg_exception.Construct(PChar(Msg) );

     end;

   until done;

   ts:=m_title;

   while ts^ <> #0 do
    begin
     if byte(ts^ ) < byte(' ' ) then
      ts^:=' ';

     inc(ptrcomp(ts ) );

    end;

 finally
   XML_ParserFree(p );
 end;
end;

{ TITLE }
function parser.title;
begin
 result:=m_title;

end;

{ PARSE_ATTR }
procedure parser.parse_attr(attr : char_ptr_ptr );
var
 i : int;

begin
 i:=0;

 while char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ <> NIL do
  begin
   if StrComp(PChar(char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ ) ,'style' ) = 0 then
    parse_style(agg_basics.char_ptr_ptr(ptrcomp(attr ) + (i + 1 ) * sizeof(char_ptr ) )^ )
   else
    parse_attr(
     agg_basics.char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ ,
     agg_basics.char_ptr_ptr(ptrcomp(attr ) + (i + 1 ) * sizeof(char_ptr ) )^ );

   inc(i ,2 );

  end;

end;

{ PARSE_PATH }
procedure parser.parse_path;
var
 i : int;

 tmp : array[0..3 ] of agg_basics.char_ptr;

begin
 i:=0;

 while char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ <> NIL do
  begin
  // The <path> tag can consist of the path itself ("d=")
  // as well as of other parameters like "style=", "transform=", etc.
  // In the last case we simply rely on the function of parsing
  // attributes (see 'else' branch).
   if StrComp(PChar(char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ ) ,'d' ) = 0 then
    begin
     m_tokenizer.set_path_str(agg_basics.char_ptr_ptr(ptrcomp(attr ) + (i + 1 ) * sizeof(char_ptr ) )^ );

     m_path.parse_path(@m_tokenizer );

    end
   else
    begin
    // Create a temporary single pair "name-value" in order
    // to avoid multiple calls for the same attribute.
     tmp[0 ]:=agg_basics.char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^;
     tmp[1 ]:=agg_basics.char_ptr_ptr(ptrcomp(attr ) + (i + 1 ) * sizeof(char_ptr ) )^;
     tmp[2 ]:=NIL;
     tmp[3 ]:=NIL;

     parse_attr(@tmp );

    end;

   inc(i ,2 );

  end;

end;

{ PARSE_POLY }
procedure parser.parse_poly;
var
 i : int;

 x ,y : double;

begin
 x:=0.0;
 y:=0.0;

 m_path.begin_path;

 i:=0;

 while char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ <> NIL do
  begin
   if not parse_attr(
           agg_basics.char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ ,
           agg_basics.char_ptr_ptr(ptrcomp(attr ) + (i + 1 ) * sizeof(char_ptr ) )^ ) then
    if StrComp(PChar(agg_basics.char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ ) ,'points' ) = 0 then
     begin
      m_tokenizer.set_path_str(agg_basics.char_ptr_ptr(ptrcomp(attr ) + (i + 1 ) * sizeof(char_ptr ) )^ );

      if not m_tokenizer.next then
       raise svg_exception.Construct(PChar('parse_poly: Too few coordinates' ) );

      x:=m_tokenizer.last_number;

      if not m_tokenizer.next then
       raise svg_exception.Construct(PChar('parse_poly: Too few coordinates' ) );

      y:=m_tokenizer.last_number;

      m_path.move_to(x ,y );

      while m_tokenizer.next do
       begin
        x:=m_tokenizer.last_number;

        if not m_tokenizer.next then
         raise svg_exception.Construct(PChar('parse_poly: Odd number of coordinates' ) );

        y:=m_tokenizer.last_number;

        m_path.line_to(x ,y );

       end;

     end;

   inc(i ,2 );

  end;

 m_path.end_path; 

end;

{ PARSE_RECT }
procedure parser.parse_rect;
var
 i : int;

 x ,y ,w ,h : double;

begin
 x:=0.0;
 y:=0.0;
 w:=0.0;
 h:=0.0;

 m_path.begin_path;

 i:=0;

 while char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ <> NIL do
  begin
   if not parse_attr(
           agg_basics.char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ ,
           agg_basics.char_ptr_ptr(ptrcomp(attr ) + (i + 1 ) * sizeof(char_ptr ) )^ ) then
    begin
     if StrComp(PChar(char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ ) ,'x' ) = 0 then
      x:=parse_double(agg_basics.char_ptr_ptr(ptrcomp(attr ) + (i + 1 ) * sizeof(char_ptr ) )^ );

     if StrComp(PChar(char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ ) ,'y' ) = 0 then
      y:=parse_double(agg_basics.char_ptr_ptr(ptrcomp(attr ) + (i + 1 ) * sizeof(char_ptr ) )^ );

     if StrComp(PChar(char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ ) ,'width' ) = 0 then
      w:=parse_double(agg_basics.char_ptr_ptr(ptrcomp(attr ) + (i + 1 ) * sizeof(char_ptr ) )^ );

     if StrComp(PChar(char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ ) ,'height' ) = 0 then
      h:=parse_double(agg_basics.char_ptr_ptr(ptrcomp(attr ) + (i + 1 ) * sizeof(char_ptr ) )^ );

     // rx - to be implemented
     // ry - to be implemented

    end;

   inc(i ,2 );

  end;

 if (w <> 0.0 ) and
    (h <> 0.0 ) then
  begin
   if w < 0.0 then
    raise svg_exception.Construct(PChar('parse_rect: Invalid width: ' ) );

   if h < 0.0 then
    raise svg_exception.Construct(PChar('parse_rect: Invalid height: ' ) );

   m_path.move_to(x     ,y );
   m_path.line_to(x + w ,y );
   m_path.line_to(x + w ,y + h );
   m_path.line_to(x     ,y + h );
   m_path.close_subpath;

  end;

 m_path.end_path;

end;

{ PARSE_LINE }
procedure parser.parse_line;
var
 i : int;

 x1 ,y1 ,x2 ,y2 : double;

begin
 x1:=0.0;
 y1:=0.0;
 x2:=0.0;
 y2:=0.0;

 m_path.begin_path;

 i:=0;

 while char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ <> NIL do
  begin
   if not parse_attr(
           agg_basics.char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ ,
           agg_basics.char_ptr_ptr(ptrcomp(attr ) + (i + 1 ) * sizeof(char_ptr ) )^ ) then
    begin
     if StrComp(PChar(char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ ) ,'x1' ) = 0 then
      x1:=parse_double(agg_basics.char_ptr_ptr(ptrcomp(attr ) + (i + 1 ) * sizeof(char_ptr ) )^ );

     if StrComp(PChar(char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ ) ,'y1' ) = 0 then
      y1:=parse_double(agg_basics.char_ptr_ptr(ptrcomp(attr ) + (i + 1 ) * sizeof(char_ptr ) )^ );

     if StrComp(PChar(char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ ) ,'x2' ) = 0 then
      x2:=parse_double(agg_basics.char_ptr_ptr(ptrcomp(attr ) + (i + 1 ) * sizeof(char_ptr ) )^ );

     if StrComp(PChar(char_ptr_ptr(ptrcomp(attr ) + i * sizeof(char_ptr ) )^ ) ,'y2' ) = 0 then
      y2:=parse_double(agg_basics.char_ptr_ptr(ptrcomp(attr ) + (i + 1 ) * sizeof(char_ptr ) )^ );

    end;

   inc(i ,2 );

  end;

 m_path.move_to(x1 ,y1 );
 m_path.line_to(x2 ,y2 );
 m_path.end_path;

end;

{ PARSE_STYLE }
procedure parser.parse_style;
var
 nv_start ,nv_end : agg_basics.char_ptr;

begin
 while str^ <> #0 do
  begin
  // Left Trim
   while (str^ <> #0 ) and
         (str^ = ' ' ) do
    inc(ptrcomp(str ) );

   nv_start:=str;

   while (str^ <> #0 ) and
         (str^ <> ';' ) do
    inc(ptrcomp(str ) );

   nv_end:=str;

  // Right Trim
   while (ptrcomp(nv_end ) > ptrcomp(nv_start ) ) and
         ((nv_end^ = ';' ) or
          (nv_end^ = ' ' ) ) do
    dec(ptrcomp(nv_end ) );

   inc(ptrcomp(nv_end ) );

   parse_name_value(nv_start ,nv_end );

   if str^ <> #0 then
    inc(ptrcomp(str ) );

  end;

end;

{ PARSE_TRANSFORM }
procedure parser.parse_transform;
begin
 while str^ <> #0 do
  begin
   if islower(str^ ) then
    if StrLComp(PChar(str ) ,'matrix' ,6 ) = 0 then
     inc(ptrcomp(str ) ,parse_matrix(str ) )
    else
     if StrLComp(PChar(str ) ,'translate' ,9 ) = 0 then
      inc(ptrcomp(str ) ,parse_translate(str ) )
     else
      if StrLComp(PChar(str ) ,'rotate' ,6 ) = 0 then
       inc(ptrcomp(str ) ,parse_rotate(str ) )
      else
       if StrLComp(PChar(str ) ,'scale' ,5 ) = 0 then
        inc(ptrcomp(str ) ,parse_scale(str ) )
       else
        if StrLComp(PChar(str ) ,'skewX' ,5 ) = 0 then
         inc(ptrcomp(str ) ,parse_skew_x(str ) )
        else
         if StrLComp(PChar(str ) ,'skewY' ,5 ) = 0  then
          inc(ptrcomp(str ) ,parse_skew_y(str ) )
         else
          inc(ptrcomp(str ) )

   else
    inc(ptrcomp(str ) );

  end;

end;

{ PARSE_MATRIX }
function parser.parse_matrix;
var
 args : array[0..5 ] of double;

 na ,len : unsigned;

 ta : trans_affine;

begin
 na :=0;
 len:=parse_transform_args(str ,@args ,6 ,@na );

 if na <> 6 then
  raise svg_exception.Construct(PChar('parse_matrix: Invalid number of arguments' ) );

 ta.Construct(args[0 ] ,args[1 ] ,args[2 ] ,args[3 ] ,args[4 ] ,args[5 ] );

 m_path.transform.premultiply(@ta );

 result:=len;

end;

{ PARSE_TRANSLATE }
function parser.parse_translate;
var
 args : array[0..1 ] of double;

 na ,len : unsigned;

 tat : trans_affine_translation;

begin
 na :=0;
 len:=parse_transform_args(str ,@args ,2 ,@na );

 if na = 1 then
  args[1 ]:=0.0;

 tat.Construct(args[0 ] ,args[1 ] );

 m_path.transform.premultiply(@tat );

 result:=len;

end;

{ PARSE_ROTATE }
function parser.parse_rotate;
var
 args : array[0..2 ] of double;

 na ,len : unsigned;

 tar : trans_affine_rotation;
 
 tat ,t : trans_affine_translation;

begin
 na :=0;
 len:=parse_transform_args(str ,@args ,3 ,@na );

 if na = 1 then
  begin
   tar.Construct(deg2rad(args[0 ] ) );

   m_path.transform.premultiply(@tar  );

  end
 else
  if na = 3 then
   begin
    t.Construct(-args[1 ] ,-args[2 ] );

    tar.Construct(deg2rad(args[0 ] ) );
    tat.Construct(args[1 ] ,args[2 ] );

    t.multiply(@tar );
    t.multiply(@tat );

    m_path.transform.premultiply(@t );

   end
  else
   raise svg_exception.Construct(PChar('parse_rotate: Invalid number of arguments' ) );

 result:=len;

end;

{ PARSE_SCALE }
function parser.parse_scale;
var
 args : array[0..1 ] of double;

 na ,len : unsigned;

 tas : trans_affine_scaling;

begin
 na :=0;
 len:=parse_transform_args(str ,@args ,2 ,@na );

 if na = 1 then
  args[1 ]:=args[0 ];

 tas.Construct(args[0 ] ,args[1 ] );

 m_path.transform.premultiply(@tas );

 result:=len;

end;

{ PARSE_SKEW_X }
function parser.parse_skew_x;
var
 arg : double;

 na ,len : unsigned;

 tas : trans_affine_skewing;

begin
 na :=0;
 len:=parse_transform_args(str ,@arg ,1 ,@na );

 tas.Construct(deg2rad(arg ) ,0.0 );

 m_path.transform.premultiply(@tas );

 result:=len;

end;

{ PARSE_SKEW_Y }
function parser.parse_skew_y;
var
 arg : double;

 na ,len : unsigned;

 tas : trans_affine_skewing;

begin
 na :=0;
 len:=parse_transform_args(str ,@arg ,1 ,@na );

 tas.Construct(0.0 ,deg2rad(arg ) );

 m_path.transform.premultiply(@tas );

 result:=len;

end;

{ PARSE_ATTR }
function parser.parse_attr(name ,value : agg_basics.char_ptr ) : boolean;
var
 clr : aggclr;

begin
 result:=true;

 if StrComp(PChar(name ) ,'style' ) = 0 then
  parse_style(value )
 else
  if StrComp(PChar(name ) ,'fill' ) = 0 then
   if StrComp(PChar(value ) ,'none' ) = 0 then
    m_path.fill_none
   else
    begin
     clr:=parse_color(value );

     m_path.fill(@clr );

    end
  else
   if StrComp(PChar(name ) ,'fill-opacity' ) = 0 then
    m_path.fill_opacity(parse_double(value ) )
   else
    if StrComp(PChar(name ) ,'stroke' ) = 0 then
     if StrComp(PChar(value ) ,'none' ) = 0 then
      m_path.stroke_none
     else
      begin
       clr:=parse_color(value );

       m_path.stroke(@clr );

      end
    else
     if StrComp(PChar(name ) ,'stroke-width' ) = 0 then
      m_path.stroke_width(parse_double(value ) )
     else
      if StrComp(PChar(name ) ,'stroke-linecap' ) = 0 then
       begin
        if StrComp(PChar(value ) ,'butt' ) = 0 then
         m_path.line_cap(butt_cap )
        else
         if StrComp(PChar(value ) ,'round' ) = 0 then
          m_path.line_cap(round_cap )
         else
          if StrComp(PChar(value ) ,'square' ) = 0 then
           m_path.line_cap(square_cap );

       end
      else
       if StrComp(PChar(name ) ,'stroke-linejoin' ) = 0 then
        begin
         if StrComp(PChar(value ) ,'miter' ) = 0 then
          m_path.line_join(miter_join )
         else
          if StrComp(PChar(value ) ,'round' ) = 0 then
           m_path.line_join(round_join )
          else
           if StrComp(PChar(value ) ,'bevel' ) = 0 then
            m_path.line_join(bevel_join );

        end
       else
        if StrComp(PChar(name ) ,'stroke-miterlimit' ) = 0 then
         m_path.miter_limit(parse_double(value ) )
        else
         if StrComp(PChar(name ) ,'stroke-opacity' ) = 0 then
          m_path.stroke_opacity(parse_double(value ) )
         else
          if StrComp(PChar(name ) ,'transform' ) = 0 then
           parse_transform(value )

        //else
        // if StrComp(PChar(el ) ,'<OTHER_ATTRIBUTES>' ) = 0 then
        //  begin
        //  end
        // ...

          else
           result:=false;

end;

{ PARSE_NAME_VALUE }
function parser.parse_name_value;
var
 str ,val : agg_basics.char_ptr;

begin
 str:=nv_start;

 while (ptrcomp(str ) < ptrcomp(nv_end ) ) and
       (str^ <> ':' ) do
  inc(ptrcomp(str ) );

 val:=str;

// Right Trim
 while (ptrcomp(str ) > ptrcomp(nv_start ) ) and
       ((str^ = ':' ) or
        (str^ = ' ' ) ) do
  dec(ptrcomp(str ) );

 inc(ptrcomp(str ) );

 copy_name(nv_start ,str );

 while (ptrcomp(val ) < ptrcomp(nv_end ) ) and
       ((val^ = ':' ) or
        (val^ = ' ' ) ) do
  inc(ptrcomp(val ) );

 copy_value(val ,nv_end );

 result:=parse_attr(agg_basics.char_ptr(m_attr_name ) ,agg_basics.char_ptr(m_attr_value ) );

end;

{ COPY_NAME }
procedure parser.copy_name;
var
 len : unsigned;

begin
 len:=ptrcomp(end_ ) - ptrcomp(start );

 if (m_attr_name_len = 0 ) or
    (len > m_attr_name_len ) then
  begin
   agg_freemem(pointer(m_attr_name ) ,m_attr_name_aloc );

   m_attr_name_aloc:=len + 1;

   agg_freemem(pointer(m_attr_name ) ,m_attr_name_aloc );

   m_attr_name_len:=len;

  end;

 if len <> 0 then
  move(start^ ,m_attr_name^ ,len );

 char_ptr(ptrcomp(m_attr_name ) + len )^:=#0;

end;

{ COPY_VALUE }
procedure parser.copy_value;
var
 len : unsigned;

begin
 len:=ptrcomp(end_ ) - ptrcomp(start );

 if (m_attr_value_len = 0 ) or
    (len > m_attr_value_len ) then
  begin
   agg_freemem(pointer(m_attr_value ) ,m_attr_value_aloc );

   m_attr_value_aloc:=len + 1;

   agg_getmem(pointer(m_attr_value ) ,m_attr_value_aloc );

   m_attr_value_len:=len;

  end;

 if len <> 0 then
  move(start^ ,m_attr_value^ ,len );

 char_ptr(ptrcomp(m_attr_value ) + len )^:=#0;

end;

END.

