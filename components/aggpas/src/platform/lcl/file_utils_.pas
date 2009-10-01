//
// AggPas 2.4 RM3 demo framework file utility library
// Milan Marusinec alias Milano (c) 2006
//
unit
 file_utils_ ;

INTERFACE

{$I agg_mode.inc }
{$I- }
uses
 agg_basics ;

{ TYPES DEFINITION }
type
 api_file_ptr = ^api_file;
 api_file = record
   fileName : shortstring;
   isOpened : boolean;

   fSize ,
   fRead : int;

  // FSOpenFork parameters
   df : file;

  end;

{ GLOBAL PROCEDURES }
 function  cut_str(s : shortstring ) : shortstring;
 function  up_str (s : shortstring ) : shortstring;
 function  cmp_str(s : shortstring ) : shortstring;

 function  str_dir(s : shortstring ) : shortstring;
 function  dir_str(s : shortstring ) : shortstring;

 function  str_disk(fn : shortstring ) : shortstring;
 function  str_path(fn : shortstring ) : shortstring;
 function  str_name(fn : shortstring ) : shortstring;
 function  str_ext (fn : shortstring ) : shortstring;

 function  fold_name  (p ,n ,x : shortstring ) : shortstring;
 procedure spread_name(fn : shortstring; var p ,n ,x : shortstring );

 function  file_exists(fn : shortstring ) : boolean;

 function  api_open_file (var af : api_file; fname : shortstring ) : boolean;
 function  api_read_file (var af : api_file; buff : pointer; aloc : int; var read : int ) : boolean;
 function  api_close_file(var af : api_file ) : boolean;

 function  param_count : int;
 function  param_str(i : int ) : shortstring;

 
IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
type
 tSCAN = (

  SCAN_0 ,
  SCAN_1 ,SCAN_2 ,SCAN_3 ,SCAN_4 ,SCAN_5 ,SCAN_6 ,SCAN_7 ,SCAN_8 ,SCAN_9 ,
  SCAN_A ,SCAN_B ,SCAN_C ,SCAN_D ,SCAN_E ,SCAN_F ,SCAN_G ,SCAN_H ,SCAN_I ,
  SCAN_J ,SCAN_K ,SCAN_L ,SCAN_M ,SCAN_N ,SCAN_O ,SCAN_P ,SCAN_Q ,SCAN_R ,
  SCAN_S ,SCAN_T ,SCAN_U ,SCAN_V ,SCAN_W ,SCAN_X ,SCAN_Y ,SCAN_Z

  );

 tITEM = (

  ITEM_0 ,
  ITEM_1 ,ITEM_2 ,ITEM_3 ,ITEM_4 ,ITEM_5 ,ITEM_6 ,ITEM_7 ,ITEM_8 ,ITEM_9 ,
  ITEM_A ,ITEM_B ,ITEM_C ,ITEM_D ,ITEM_E ,ITEM_F ,ITEM_G ,ITEM_H ,ITEM_I ,
  ITEM_J ,ITEM_K ,ITEM_L ,ITEM_M ,ITEM_N ,ITEM_O ,ITEM_P ,ITEM_Q ,ITEM_R ,
  ITEM_S ,ITEM_T ,ITEM_U ,ITEM_V ,ITEM_W ,ITEM_X ,ITEM_Y ,ITEM_Z

  );

const
 dir_slash = '/';

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
{ CUT_STR }
function cut_str;
var
 fcb : byte;
 scn : tSCAN;

begin
 result:='';

 scn:=SCAN_1;

 if length(s ) > 0 then
  for fcb:=length(s ) downto 1 do
   case scn of
    SCAN_1 :
     case s[fcb ] of
      ' ' :
      else
       begin
        result:=s[fcb ];

        scn:=SCAN_2;

       end;

     end;

    SCAN_2 :
     result:=s[fcb ] + result;

   end;

end;

{ CMP_STR }
function cmp_str;
begin
 cmp_str:=up_str(cut_str(s ) );

end;

{ UP_STR }
function up_str;
var
 fcb : byte;

begin
 if length(s ) > 0 then
  for fcb:=1 to length(s ) do
   if byte(s[fcb ] ) > 0 then
    s[fcb ]:=pageEqHigh[byte(s[fcb ] ) ];

 result:=s;

end;

{ STR_DIR }
function str_dir;
begin
 s:=cut_str(s );

 if length(s ) > 0 then
  if s[length(s ) ] <> dir_slash then
   s:=s + dir_slash;

 result:=s;

end;

{ DIR_STR }
function dir_str;
begin
 s:=cut_str(s );

 if length(s ) > 0 then
  if s[length(s ) ] = dir_slash then
   dec(byte(s[0 ] ) );

 result:=s;

end;

{ STR_DISK }
function str_disk;
var
 fcb : byte;
 str : shortstring;
 itm : tITEM;

begin
 str:='';
 itm:=ITEM_1;

 if length(fn ) > 0 then
  for fcb:=1 to length(fn ) do
   case itm of
    ITEM_1 :
     case fn[fcb ] of
      'a'..'z' ,'A'..'Z' :
       begin
        str:=fn[fcb ];
        itm:=ITEM_2;

       end;

      '\' ,'/' :
       begin
        str:=fn[fcb ];
        itm:=ITEM_3;

       end;

      else
       break;

     end;

    ITEM_2 :
     case fn[fcb ] of
      ':' :
       begin
        str:=str + fn[fcb ];
        itm:=ITEM_F;

        break;

       end;

      else
       break;

     end;

    ITEM_3 :
     case fn[fcb ] of
      '\' ,'/' :
       begin
        str:=str + fn[fcb ];
        itm:=ITEM_4;

       end;

      else
       break;

     end;

    ITEM_4 :
     case fn[fcb ] of
      '\' ,'/' ,':' ,'<' ,'>' ,'.' ,'"' ,'|' ,#0..#31 :
       break;

      else
       begin
        str:=str + fn[fcb ];
        itm:=ITEM_F;

       end;

     end;

    ITEM_F :
     case fn[fcb ] of
      '\' ,'/' :
       break;

      else
       str:=str + fn[fcb ];

     end;

   end;

 if itm = ITEM_F then
  result:=str
 else
  result:='';

end;

{ STR_PATH }
function str_path;
var
 fcb : byte;
 pth ,
 str : shortstring;
 itm : tITEM;

begin
 pth:='';
 str:='';
 itm:=ITEM_1;

 if length(fn ) > 0 then
  for fcb:=1 to length(fn ) do
   case itm of
    ITEM_1 :
     case fn[fcb ] of
      '\' ,'/' :
       begin
        str:=fn[fcb ];
        itm:=ITEM_2;

       end;

      else
       begin
        str:=fn[fcb ];
        itm:=ITEM_3;

       end;

     end;

    ITEM_2 :
     case fn[fcb ] of
      '\' ,'/' :
       begin
        str:=str + fn[fcb ];
        itm:=ITEM_3;

       end;

      else
       begin
        pth:=str;
        str:=fn[fcb ];
        itm:=ITEM_A;

       end;

     end;

    ITEM_3 :
     case fn[fcb ] of
      '\' ,'/' :
       begin
        pth:=fn[fcb ];
        str:='';
        itm:=ITEM_A;

       end;

      else
       str:=str + fn[fcb ];

     end;

    ITEM_A :
     case fn[fcb ] of
      '\' ,'/' :
       begin
        pth:=pth + str + fn[fcb ];
        str:='';

       end;

      else
       str:=str + fn[fcb ];

     end;

   end;

 result:=pth;

end;

{ STR_NAME }
function str_name;
var
 fcb : byte;
 str ,
 ext : shortstring;
 itm : tITEM;

begin
 str:='';
 ext:='';
 itm:=ITEM_1;

 if length(fn ) > 0 then
  for fcb:=1 to length(fn ) do
   case itm of
    ITEM_1 :
     case fn[fcb ] of
      '\' ,'/' :
       itm:=ITEM_2;

      'a'..'z' ,'A'..'Z' :
       begin
        ext:=fn[fcb ];
        itm:=ITEM_4;

       end;

      '.' :
       begin
        str:='';
        ext:=fn[fcb ];
        itm:=ITEM_B;

       end;

      else
       begin
        str:=fn[fcb ];
        itm:=ITEM_A;

       end;

     end;

    ITEM_2 :
     case fn[fcb ] of
      '\' ,'/' :
       itm:=ITEM_3;

      '.' :
       begin
        str:='';
        ext:=fn[fcb ];
        itm:=ITEM_B;

       end;

      else
       begin
        str:=fn[fcb ];
        itm:=ITEM_A;

       end;

     end;

    ITEM_3 :
     case fn[fcb ] of
      '\' ,'/' :
       begin
        str:='';
        itm:=ITEM_A;

       end;

     end;

    ITEM_4 :
     case fn[fcb ] of
      '\' ,'/' :
       begin
        str:='';
        itm:=ITEM_A;

       end;

      ':' :
       itm:=ITEM_5;

      '.' :
       begin
        str:=ext;
        ext:=fn[fcb ];
        itm:=ITEM_B;

       end;

      else
       begin
        str:=ext + fn[fcb ];
        ext:='';
        itm:=ITEM_A;

       end;

     end;

    ITEM_5 :
     case fn[fcb ] of
      '\' ,'/' :
       begin
        str:='';
        itm:=ITEM_A;

       end;

      '.' :
       begin
        str:='';
        ext:=fn[fcb ];
        itm:=ITEM_B;

       end;

      else
       begin
        str:=fn[fcb ];
        itm:=ITEM_A;

       end;

     end;

    ITEM_A :
     case fn[fcb ] of
      '\' ,'/' :
       begin
        str:='';
        ext:='';

       end;

      '.' :
       begin
        ext:=fn[fcb ];
        itm:=ITEM_B;

       end;

      else
       str:=str + fn[fcb ];

     end;

    ITEM_B :
     case fn[fcb ] of
      '\' ,'/' :
       begin
        str:='';
        ext:='';
        itm:=ITEM_A;

       end;

      '.' :
       begin
        str:=str + ext;
        ext:=fn[fcb ];

       end;

     end;

   end;

 result:=str;

end;

{ STR_EXT }
function str_ext;
var
 fcb : byte;
 ext : shortstring;
 itm : tITEM;

begin
 ext:='';
 itm:=ITEM_1;

 if length(fn ) > 0 then
  for fcb:=1 to length(fn ) do
   case itm of
    ITEM_1 :
     case fn[fcb ] of
      '\' ,'/' :
       itm:=ITEM_2;

      '.' :
       begin
        ext:=fn[fcb ];
        itm:=ITEM_B;

       end;

      else
       itm:=ITEM_A;

     end;

    ITEM_2 :
     case fn[fcb ] of
      '\' ,'/' :
       itm:=ITEM_3;

      '.' :
       begin
        ext:=fn[fcb ];
        itm:=ITEM_B;

       end;

      else
       itm:=ITEM_A;

     end;

    ITEM_3 :
     case fn[fcb ] of
      '\' ,'/' :
       itm:=ITEM_A;

     end;

    ITEM_A :
     case fn[fcb ] of
      '.' :
       begin
        ext:=fn[fcb ];
        itm:=ITEM_B;

       end;

     end;

    ITEM_B :
     case fn[fcb ] of
      '\' ,'/' :
       begin
        ext:='';
        itm:=ITEM_A;

       end;

      '.' :
       ext:=fn[fcb ];

      else
       ext:=ext + fn[fcb ];

     end;

   end;

 result:=cut_str(ext );

 if result = '.' then
  result:='';

end;

{ FOLD_NAME }
function fold_name;
var
 dsk ,
 nme ,
 pth ,
 ext : shortstring;

begin
 dsk:=str_disk(p );
 pth:=str_dir (str_path(p ) );
 nme:=str_name(n );
 ext:=str_ext (x );

 result:=dsk + pth + nme + ext;

end;

{ SPREAD_NAME }
procedure spread_name;
begin
 p:=str_disk(fn ) + str_dir(str_path(fn ) );
 n:=str_name(fn );
 x:=str_ext (fn );

end;

{ FILE_EXISTS }
function file_exists;
var
 f : file;

begin
 AssignFile(f ,fn );
 reset     (f );

 if IOResult = 0 then
  begin
   close(f );

   result:=true;

  end
 else
  result:=false;

end;

{ API_OPEN_FILE }
function api_open_file;
begin
 result:=false;

 fillchar(af ,sizeof(api_file ) ,0 );

 af.fileName:=fname;
 af.isOpened:=false;

 IOResult;

 AssignFile(af.df ,fname );
 reset     (af.df ,1 );

 if IOResult = 0 then
  begin
   af.isOpened:=true;

   af.fSize:=filesize(af.df );
   af.fRead:=0;

  end;

 result:=af.isOpened;

end;

{ API_READ_FILE }
function api_read_file;
begin
 result:=false;
 read  :=0;

 if af.isOpened then
  begin
   if aloc > af.fSize - af.fRead then
    aloc:=af.fSize - af.fRead;

   blockread(af.df ,buff^ ,aloc ,read );

   if aloc = read then
    begin
     inc(af.fRead ,read );

     result:=true;

    end
   else
    read:=0;  

  end;

end;

{ API_CLOSE_FILE }
function api_close_file;
begin
 result:=false;
 
 if af.isOpened then
  begin
   system.close(af.df );

   af.isOpened:=false;

   result:=true;

  end;

end;

{ PARAM_COUNT }
function param_count;
begin
 result:=ParamCount;

end;

{ PARAM_STR }
function param_str;
begin
 result:=ParamStr(i );

end;

END.

