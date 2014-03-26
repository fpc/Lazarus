unit TestWatchesUnitSimple;

interface
uses sysutils, types;

procedure Test1;

// TODO types in another unit / yet brea in this unit
// Nested procedure

implementation

type

  { TSimpleClass1 }

  TSimpleClass1 = class
  public
    SimpleField_Short1: ShortInt;
    SimpleField_Small1: Smallint;
    SimpleField_Int1: LongInt;
    SimpleField_QInt1: Int64;

    SimpleField_Byte1: Byte;
    SimpleField_Word1: Word;
    SimpleField_DWord1: LongWord;
    SimpleField_QWord1: QWord;

    SimpleField_Single1: Single;
    SimpleField_Double1: Double;
    SimpleField_Ext1: Extended;
    procedure Test1Method;
  end;

  { TSimpleClass2 }

  TSimpleClass2 = class(TSimpleClass1)
    procedure Test2Method; // read inherited fields
  end;

var
  SimpleGlob_Short1, SimpleGlob_Short2, SimpleGlob_Short3, SimpleGlob_Short4, SimpleGlob_Short5: ShortInt;
  SimpleGlob_Small1, SimpleGlob_Small2, SimpleGlob_Small3, SimpleGlob_Small4, SimpleGlob_Small5: Smallint;
  SimpleGlob_Int1, SimpleGlob_Int2, SimpleGlob_Int3, SimpleGlob_Int4, SimpleGlob_Int5: LongInt;
  SimpleGlob_QInt1, SimpleGlob_QInt2, SimpleGlob_QInt3, SimpleGlob_QInt4, SimpleGlob_QInt5: Int64;

  SimpleGlob_Byte1, SimpleGlob_Byte2, SimpleGlob_Byte3, SimpleGlob_Byte4, SimpleGlob_Byte5: Byte;
  SimpleGlob_Word1, SimpleGlob_Word2, SimpleGlob_Word3, SimpleGlob_Word4, SimpleGlob_Word5: Word;
  SimpleGlob_DWord1, SimpleGlob_DWord2, SimpleGlob_DWord3, SimpleGlob_DWord4, SimpleGlob_DWord5: LongWord;
  SimpleGlob_QWord1, SimpleGlob_QWord2, SimpleGlob_QWord3, SimpleGlob_QWord4, SimpleGlob_QWord5: QWord;

  SimpleGlob_Single1, SimpleGlob_Single2, SimpleGlob_Single3, SimpleGlob_Single4, SimpleGlob_Single5: Single;
  SimpleGlob_Double1, SimpleGlob_Double2, SimpleGlob_Double3, SimpleGlob_Double4, SimpleGlob_Double5: Double;
  SimpleGlob_Ext1, SimpleGlob_Ext2, SimpleGlob_Ext3, SimpleGlob_Ext4, SimpleGlob_Ext5: Extended;

  SimpleGlob_Comp1, SimpleGlob_Comp2, SimpleGlob_Comp3: Comp;

  SimpleGlob_Class1: TSimpleClass1;
  SimpleGlob_Class2: TSimpleClass2;

procedure Test1Sub(
  SimpleArg_Short1: ShortInt;
  SimpleArg_Small1: Smallint;
  SimpleArg_Int1: LongInt;
  SimpleArg_QInt1: Int64;

  SimpleArg_Byte1: Byte;
  SimpleArg_Word1: Word;
  SimpleArg_DWord1: LongWord;
  SimpleArg_QWord1: QWord;

  SimpleArg_Single1: Single;
  SimpleArg_Double1: Double;
  SimpleArg_Ext1: Extended;

  var SimpleVArg_Short1: ShortInt;
  var SimpleVArg_Small1: Smallint;
  var SimpleVArg_Int1 : LongInt;
  var SimpleVArg_QInt1: Int64;

  var SimpleVArg_Byte1: Byte;
  var SimpleVArg_Word1: Word;
  var SimpleVArg_DWord1: LongWord;
  var SimpleVArg_QWord1: QWord;

  var SimpleVArg_Single1: Single;
  var SimpleVArg_Double1: Double;
  var SimpleVArg_Ext1: Extended;

  SimpleArg_Class1: TSimpleClass1;
  SimpleArg_Class2: TSimpleClass2;
  var SimpleVArg_Class1: TSimpleClass1;
  var SimpleVArg_Class2: TSimpleClass2

  );
var
  SimpleLocal_Short1: ShortInt;
  SimpleLocal_Small1: Smallint;
  SimpleLocal_Int1 : LongInt;
  SimpleLocal_QInt1: Int64;

  SimpleLocal_Byte1: Byte;
  SimpleLocal_Word1: Word;
  SimpleLocal_DWord1: LongWord;
  SimpleLocal_QWord1: QWord;

  SimpleLocal_Single1: Single;
  SimpleLocal_Double1: Double;
  SimpleLocal_Ext1: Extended;

  SimplePArg_Int1, SimplePVArg_Int1, SimplePLocal_Int1, SimplePGlob_Int1: PLongInt;

begin
  SimpleLocal_Short1 := 39;
  SimpleGlob_Short1  := 29;
  SimpleGlob_Short2 := 0;
  SimpleGlob_Short3 := -1;
  SimpleGlob_Short4 := high(ShortInt);
  SimpleGlob_Short5 := low(ShortInt);

  SimpleLocal_Small1 := 391;
  SimpleGlob_Small1  := 291;
  SimpleGlob_Small2 := 0;
  SimpleGlob_Small3 := -1;
  SimpleGlob_Small4 := high(SmallInt);
  SimpleGlob_Small5 := low(SmallInt);

  SimpleLocal_Int1 := 3901;
  SimpleGlob_Int1  := 2901;
  SimpleGlob_Int2 := 0;
  SimpleGlob_Int3 := -1;
  SimpleGlob_Int4 := high(LongInt);
  SimpleGlob_Int5 := low(LongInt);

  SimpleLocal_QInt1 := 39001;
  SimpleGlob_QInt1  := 29001;
  SimpleGlob_QInt2 := 0;
  SimpleGlob_QInt3 := -1;
  SimpleGlob_QInt4 := high(Int64);
  SimpleGlob_QInt5 := low(Int64);

  SimpleLocal_Byte1 := 59;
  SimpleGlob_Byte1  := 49;
  SimpleGlob_Byte2 := $7f;
  SimpleGlob_Byte3 := $80;
  SimpleGlob_Byte4 := high(Byte);
  SimpleGlob_Byte5 := low(Byte);

  SimpleLocal_Word1 := 591;
  SimpleGlob_Word1  := 491;
  SimpleGlob_Word2 := $7fff;
  SimpleGlob_Word3 := $8000;
  SimpleGlob_Word4 := high(Word);
  SimpleGlob_Word5 := low(Word);

  SimpleLocal_DWord1 := 5901;
  SimpleGlob_DWord1  := 4901;
  SimpleGlob_DWord2 := $7fffffff;
  SimpleGlob_DWord3 := $80000000;
  SimpleGlob_DWord4 := high(LongWORD);
  SimpleGlob_DWord5 := low(LongWORD);

  SimpleLocal_QWord1 := 59001;
  SimpleGlob_QWord1  := 49001;
  SimpleGlob_QWord2 := $7fffffffffffffff;
  SimpleGlob_QWord3 := qword($8000000000000000);
  SimpleGlob_QWord4 := high(QWord);
  SimpleGlob_QWord5 := low(QWord);




  SimplePArg_Int1   := @SimpleArg_Int1;
  SimplePVArg_Int1  := @SimpleVArg_Int1;
  SimplePLocal_Int1 := @SimpleLocal_Int1;
  SimplePGlob_Int1  := @SimpleGlob_Int1;

  inc(SimpleVArg_Int1); // BREAK Single 1
end;

{ TSimpleClass1 }

procedure TSimpleClass1.Test1Method;
begin
  inc(SimpleGlob_Byte1); // BREAK Single 2
end;

{ TSimpleClass2 }

procedure TSimpleClass2.Test2Method;
begin
  inc(SimpleGlob_Byte1); // BREAK Single 3
end;

procedure Test1;
var
  i1: shortint;
  i2: smallint;
  i3: LongInt;
  i4: Int64;
  u1: byte;
  u2: word;
  u3: Longword;
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

  SimpleGlob_Class1 := TSimpleClass1.Create;
  SimpleGlob_Class2 := TSimpleClass2.Create;

  with SimpleGlob_Class1 do begin
    SimpleField_Short1  := 11;
    SimpleField_Small1  := 12;
    SimpleField_Int1    := 13;
    SimpleField_QInt1   := 14;

    SimpleField_Byte1   := 15;
    SimpleField_Word1   := 16;
    SimpleField_DWord1  := 17;
    SimpleField_QWord1  := 18;

    SimpleField_Single1 := 21;
    SimpleField_Double1 := 21;
    SimpleField_Ext1    := 21;
  end;
  with SimpleGlob_Class2 do begin
    SimpleField_Short1  := 111;
    SimpleField_Small1  := 112;
    SimpleField_Int1    := 113;
    SimpleField_QInt1   := 114;

    SimpleField_Byte1   := 115;
    SimpleField_Word1   := 116;
    SimpleField_DWord1  := 117;
    SimpleField_QWord1  := 118;

    SimpleField_Single1 := 121.3;
    SimpleField_Double1 := 121.4;
    SimpleField_Ext1    := 121.5;
  end;

  SimpleGlob_Comp1 := 0;
  SimpleGlob_Comp1 := 1;
  SimpleGlob_Comp1 := -1;

  Test1Sub(
    -92, -192, -1902, -190000000000002,    92, 192, 1902, 190000000000002,
    1234, 2345, 3456,
    i1, i2, i3, i4,   u1, u2, u3, u4,  d1, d2, d3,
    SimpleGlob_Class1, SimpleGlob_Class2, SimpleGlob_Class1, SimpleGlob_Class2
  );

  SimpleGlob_Class1.Test1Method;
  SimpleGlob_Class2.Test2Method;
end;

end.

