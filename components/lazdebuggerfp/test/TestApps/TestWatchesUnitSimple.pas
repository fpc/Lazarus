unit TestWatchesUnitSimple;

interface
uses sysutils, types;

procedure Test1;

// TODO types in another unit / yet brea in this unit
// Nested procedure

implementation

var
  SimpleGlob_Short1, SimpleGlob_Short2, SimpleGlob_Short3, SimpleGlob_Short4, SimpleGlob_Short5: ShortInt;
  SimpleGlob_Small1, SimpleGlob_Small2, SimpleGlob_Small3, SimpleGlob_Small4, SimpleGlob_Small5: Smallint;
  SimpleGlob_Int1, SimpleGlob_Int2, SimpleGlob_Int3, SimpleGlob_Int4, SimpleGlob_Int5: Integer;
  SimpleGlob_QInt1, SimpleGlob_QInt2, SimpleGlob_QInt3, SimpleGlob_QInt4, SimpleGlob_QInt5: Int64;

  SimpleGlob_Byte1, SimpleGlob_Byte2, SimpleGlob_Byte3, SimpleGlob_Byte4, SimpleGlob_Byte5: Byte;
  SimpleGlob_Word1, SimpleGlob_Word2, SimpleGlob_Word3, SimpleGlob_Word4, SimpleGlob_Word5: Word;
  SimpleGlob_DWord1, SimpleGlob_DWord2, SimpleGlob_DWord3, SimpleGlob_DWord4, SimpleGlob_DWord5: DWord;
  SimpleGlob_QWord1, SimpleGlob_QWord2, SimpleGlob_QWord3, SimpleGlob_QWord4, SimpleGlob_QWord5: QWord;

  SimpleGlob_Single1, SimpleGlob_Single2, SimpleGlob_Single3, SimpleGlob_Single4, SimpleGlob_Single5: Single;
  SimpleGlob_Double1, SimpleGlob_Double2, SimpleGlob_Double3, SimpleGlob_Double4, SimpleGlob_Double5: Double;
  SimpleGlob_Ext1, SimpleGlob_Ext2, SimpleGlob_Ext3, SimpleGlob_Ext4, SimpleGlob_Ext5: Extended;



procedure Test1Sub(
  SimpleArg_Short1: ShortInt;
  SimpleArg_Small1: Smallint;
  SimpleArg_Int1: Integer;
  SimpleArg_QInt1: Int64;

  SimpleArg_Byte1: Byte;
  SimpleArg_Word1: Word;
  SimpleArg_DWord1: DWord;
  SimpleArg_QWord1: QWord;

  SimpleArg_Single1: Single;
  SimpleArg_Double1: Double;
  SimpleArg_Ext1: Extended;

  var SimpleVArg_Short1: ShortInt;
  var SimpleVArg_Small1: Smallint;
  var SimpleVArg_Int1 : Integer;
  var SimpleVArg_QInt1: Int64;

  var SimpleVArg_Byte1: Byte;
  var SimpleVArg_Word1: Word;
  var SimpleVArg_DWord1: DWord;
  var SimpleVArg_QWord1: QWord;

  var SimpleVArg_Single1: Single;
  var SimpleVArg_Double1: Double;
  var SimpleVArg_Ext1: Extended

  );
var
  SimpleLocal_Short1: ShortInt;
  SimpleLocal_Small1: Smallint;
  SimpleLocal_Int1 : Integer;
  SimpleLocal_QInt1: Int64;

  SimpleLocal_Byte1: Byte;
  SimpleLocal_Word1: Word;
  SimpleLocal_DWord1: DWord;
  SimpleLocal_QWord1: QWord;

  SimpleLocal_Single1: Single;
  SimpleLocal_Double1: Double;
  SimpleLocal_Ext1: Extended;
begin
  SimpleLocal_Int1 := 3901;
  SimpleGlob_Int1  := 2901;
  SimpleGlob_Int2 := 0;
  SimpleGlob_Int3 := -1;
  SimpleGlob_Int4 := high(Integer);
  SimpleGlob_Int5 := low(Integer);

  inc(SimpleVArg_Int1); // BREAK Single 1
end;


procedure Test1;
var
  i1: shortint;
  i2: smallint;
  i3: Integer;
  i4: Int64;
  u1: byte;
  u2: word;
  u3: dword;
  u4: qword;
  d1: Single;
  d2: double;
  d3: Extended;
begin
  i1 := -91;
  i2 := -191;
  i3 := -1901;
  i4 := -190000000000001;
  u1 := 91;
  u2 := 191;
  u3 := 1901;
  u4 := 190000000000001;
  d1 := -1234;
  d2 := -2345;
  d3 := -3456;
  Test1Sub(
    -92, -192, -1902, -190000000000002,    92, 192, 1902, 190000000000002,
    1234, 2345, 3456,
    i1, i2, i3, i4,   u1, u2, u3, u4,  d1, d2, d3
  );
end;

end.

