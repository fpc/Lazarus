unit TestWatchesUnitSimple;
{$mode objfpc}{$H+}{$NOTES off}

interface
uses sysutils, types;

procedure Test1;

// TODO types in another unit / yet brea in this unit
// Nested procedure

implementation

type

  TSimpleEnum1 = (eval1, eval2, eval3);
  TSimpleSet1 = set of TSimpleEnum1;

  PSimpleEnum1 = ^TSimpleEnum1;
  PSimpleSet1 = ^TSimpleSet1;

  { TSimpleClass1 }

  TSimpleClass1 = class
  public
    Field_Short1: ShortInt;
    Field_Small1: Smallint;
    Field_Int1: LongInt;
    Field_QInt1: Int64;

    Field_Byte1: Byte;
    Field_Word1: Word;
    Field_DWord1: LongWord;
    Field_QWord1: QWord;

    Field_Single1: Single;
    Field_Double1: Double;
    Field_Ext1: Extended;
    Field_Comp1: Comp;

    Field_Bool1, Field_Bool2: Boolean;
    Field_Enum1, Field_Enum2: TSimpleEnum1;
    Field_Set1, Field_Set2: TSimpleSet1;


    Field_PShort1: PShortInt;
    Field_PSmall1: PSmallint;
    Field_PInt1: PLongInt;
    Field_PQInt1: PInt64;

    Field_PByte1: PByte;
    Field_PWord1: PWord;
    Field_PDWord1: PLongWord;
    Field_PQWord1: PQWord;

    Field_PSingle1: PSingle;
    Field_PDouble1: PDouble;
    Field_PExt1: PExtended;
    Field_PComp1: PComp;

    Field_PBool1, Field_PBool2: PBoolean;
    Field_PEnum1, Field_PEnum2: PSimpleEnum1;
    Field_PSet1, Field_PSet2: PSimpleSet1;

    procedure InitFields;
    procedure Test1Method(
      Arg_Short1: ShortInt;
      Arg_Small1: Smallint;
      Arg_Int1: LongInt;
      Arg_QInt1: Int64;

      Arg_Byte1: Byte;
      Arg_Word1: Word;
      Arg_DWord1: LongWord;
      Arg_QWord1: QWord;

      Arg_Single1: Single;
      Arg_Double1: Double;
      Arg_Ext1: Extended;
      Arg_Comp1: Comp;

      Arg_Bool1, Arg_Bool2: Boolean;
      Arg_Enum1, Arg_Enum2: TSimpleEnum1;
      Arg_Set1, Arg_Set2: TSimpleSet1;


      Arg_PShort1: PShortInt;
      Arg_PSmall1: PSmallint;
      Arg_PInt1: PLongInt;
      Arg_PQInt1: PInt64;

      Arg_PByte1: PByte;
      Arg_PWord1: PWord;
      Arg_PDWord1: PLongWord;
      Arg_PQWord1: PQWord;

      Arg_PSingle1: PSingle;
      Arg_PDouble1: PDouble;
      Arg_PExt1: PExtended;
      Arg_PComp1: PComp;

      Arg_PBool1, Arg_PBool2: PBoolean;
      Arg_PEnum1, Arg_PEnum2: PSimpleEnum1;
      Arg_PSet1, Arg_PSet2: PSimpleSet1;


      var VArg_Short1: ShortInt;
      var VArg_Small1: Smallint;
      var VArg_Int1: LongInt;
      var VArg_QInt1: Int64;

      var VArg_Byte1: Byte;
      var VArg_Word1: Word;
      var VArg_DWord1: LongWord;
      var VArg_QWord1: QWord;

      var VArg_Single1: Single;
      var VArg_Double1: Double;
      var VArg_Ext1: Extended;
      var VArg_Comp1: Comp;

      var VArg_Bool1, VArg_Bool2: Boolean;
      var VArg_Enum1, VArg_Enum2: TSimpleEnum1;
      var VArg_Set1, VArg_Set2: TSimpleSet1;


      var VArg_PShort1: PShortInt;
      var VArg_PSmall1: PSmallint;
      var VArg_PInt1: PLongInt;
      var VArg_PQInt1: PInt64;

      var VArg_PByte1: PByte;
      var VArg_PWord1: PWord;
      var VArg_PDWord1: PLongWord;
      var VArg_PQWord1: PQWord;

      var VArg_PSingle1: PSingle;
      var VArg_PDouble1: PDouble;
      var VArg_PExt1: PExtended;
      var VArg_PComp1: PComp;

      var VArg_PBool1, VArg_PBool2: PBoolean;
      var VArg_PEnum1, VArg_PEnum2: PSimpleEnum1;
      var VArg_PSet1, VArg_PSet2: PSimpleSet1
    );
  end;

  { TSimpleClass2 }

  TSimpleClass2 = class(TSimpleClass1)
    procedure Test2Method; // read inherited fields
  end;

var
  SimpleGlob_Short1: ShortInt;
  SimpleGlob_Small1: Smallint;
  SimpleGlob_Int1: LongInt;
  SimpleGlob_QInt1: Int64;

  SimpleGlob_Byte1: Byte;
  SimpleGlob_Word1: Word;
  SimpleGlob_DWord1: LongWord;
  SimpleGlob_QWord1: QWord;

  SimpleGlob_Single1: Single;
  SimpleGlob_Double1: Double;
  SimpleGlob_Ext1: Extended;
  SimpleGlob_Comp1: Comp;

  SimpleGlob_Bool1, SimpleGlob_Bool2: Boolean;
  SimpleGlob_Enum1, SimpleGlob_Enum2: TSimpleEnum1;
  SimpleGlob_Set1, SimpleGlob_Set2: TSimpleSet1;


  SimpleGlob_PShort1: PShortInt;
  SimpleGlob_PSmall1: PSmallint;
  SimpleGlob_PInt1: PLongInt;
  SimpleGlob_PQInt1: PInt64;

  SimpleGlob_PByte1: PByte;
  SimpleGlob_PWord1: PWord;
  SimpleGlob_PDWord1: PLongWord;
  SimpleGlob_PQWord1: PQWord;

  SimpleGlob_PSingle1: PSingle;
  SimpleGlob_PDouble1: PDouble;
  SimpleGlob_PExt1: PExtended;
  SimpleGlob_PComp1: PComp;

  SimpleGlob_PBool1, SimpleGlob_PBool2: PBoolean;
  SimpleGlob_PEnum1, SimpleGlob_PEnum2: PSimpleEnum1;
  SimpleGlob_PSet1, SimpleGlob_PSet2: PSimpleSet1;

  // Copy of pointers for var param
  SimpleGlob_P2Short1: PShortInt;
  SimpleGlob_P2Small1: PSmallint;
  SimpleGlob_P2Int1: PLongInt;
  SimpleGlob_P2QInt1: PInt64;

  SimpleGlob_P2Byte1: PByte;
  SimpleGlob_P2Word1: PWord;
  SimpleGlob_P2DWord1: PLongWord;
  SimpleGlob_P2QWord1: PQWord;

  SimpleGlob_P2Single1: PSingle;
  SimpleGlob_P2Double1: PDouble;
  SimpleGlob_P2Ext1: PExtended;
  SimpleGlob_P2Comp1: PComp;

  SimpleGlob_P2Bool1, SimpleGlob_P2Bool2: PBoolean;
  SimpleGlob_P2Enum1, SimpleGlob_P2Enum2: PSimpleEnum1;
  SimpleGlob_P2Set1, SimpleGlob_P2Set2: PSimpleSet1;


  // Additional Globals
  SimpleGlob_Short2, SimpleGlob_Short3, SimpleGlob_Short4, SimpleGlob_Short5: ShortInt;
  SimpleGlob_Small2, SimpleGlob_Small3, SimpleGlob_Small4, SimpleGlob_Small5: Smallint;
  SimpleGlob_Int2, SimpleGlob_Int3, SimpleGlob_Int4, SimpleGlob_Int5: LongInt;
  SimpleGlob_QInt2, SimpleGlob_QInt3, SimpleGlob_QInt4, SimpleGlob_QInt5: Int64;

  SimpleGlob_Byte2, SimpleGlob_Byte3, SimpleGlob_Byte4, SimpleGlob_Byte5: Byte;
  SimpleGlob_Word2, SimpleGlob_Word3, SimpleGlob_Word4, SimpleGlob_Word5: Word;
  SimpleGlob_DWord2, SimpleGlob_DWord3, SimpleGlob_DWord4, SimpleGlob_DWord5: LongWord;
  SimpleGlob_QWord2, SimpleGlob_QWord3, SimpleGlob_QWord4, SimpleGlob_QWord5: QWord;

  SimpleGlob_Single2, SimpleGlob_Single3, SimpleGlob_Single4, SimpleGlob_Single5: Single;
  SimpleGlob_Double2, SimpleGlob_Double3, SimpleGlob_Double4, SimpleGlob_Double5: Double;
  SimpleGlob_Ext2, SimpleGlob_Ext3, SimpleGlob_Ext4, SimpleGlob_Ext5: Extended;

  SimpleGlob_Comp2, SimpleGlob_Comp3: Comp;

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
  Local_Short1: ShortInt;
  Local_Small1: Smallint;
  Local_Int1 : LongInt;
  Local_QInt1: Int64;

  Local_Byte1: Byte;
  Local_Word1: Word;
  Local_DWord1: LongWord;
  Local_QWord1: QWord;

  Local_Single1: Single;
  Local_Double1: Double;
  Local_Ext1: Extended;

  SimplePArg_Int1, SimplePVArg_Int1, SimplePLocal_Int1, SimplePGlob_Int1: PLongInt;

begin
  Local_Short1 := 39;
  SimpleGlob_Short1  := 29;
  SimpleGlob_Short2 := 0;
  SimpleGlob_Short3 := -1;
  SimpleGlob_Short4 := high(ShortInt);
  SimpleGlob_Short5 := low(ShortInt);

  Local_Small1 := 391;
  SimpleGlob_Small1  := 291;
  SimpleGlob_Small2 := 0;
  SimpleGlob_Small3 := -1;
  SimpleGlob_Small4 := high(SmallInt);
  SimpleGlob_Small5 := low(SmallInt);

  Local_Int1 := 3901;
  SimpleGlob_Int1  := 2901;
  SimpleGlob_Int2 := 0;
  SimpleGlob_Int3 := -1;
  SimpleGlob_Int4 := high(LongInt);
  SimpleGlob_Int5 := low(LongInt);

  Local_QInt1 := 39001;
  SimpleGlob_QInt1  := 29001;
  SimpleGlob_QInt2 := 0;
  SimpleGlob_QInt3 := -1;
  SimpleGlob_QInt4 := high(Int64);
  SimpleGlob_QInt5 := low(Int64);

  Local_Byte1 := 59;
  SimpleGlob_Byte1  := 49;
  SimpleGlob_Byte2 := $7f;
  SimpleGlob_Byte3 := $80;
  SimpleGlob_Byte4 := high(Byte);
  SimpleGlob_Byte5 := low(Byte);

  Local_Word1 := 591;
  SimpleGlob_Word1  := 491;
  SimpleGlob_Word2 := $7fff;
  SimpleGlob_Word3 := $8000;
  SimpleGlob_Word4 := high(Word);
  SimpleGlob_Word5 := low(Word);

  Local_DWord1 := 5901;
  SimpleGlob_DWord1  := 4901;
  SimpleGlob_DWord2 := $7fffffff;
  SimpleGlob_DWord3 := $80000000;
  SimpleGlob_DWord4 := high(LongWORD);
  SimpleGlob_DWord5 := low(LongWORD);

  Local_QWord1 := 59001;
  SimpleGlob_QWord1  := 49001;
  SimpleGlob_QWord2 := $7fffffffffffffff;
  SimpleGlob_QWord3 := qword($8000000000000000);
  SimpleGlob_QWord4 := high(QWord);
  SimpleGlob_QWord5 := low(QWord);

  SimpleGlob_Single1 := 99.2;
  SimpleGlob_Double1 := 199.3;
  SimpleGlob_Ext1    := 299.4;
  SimpleGlob_Comp1   := -2;


  SimplePArg_Int1   := @SimpleArg_Int1;
  SimplePVArg_Int1  := @SimpleVArg_Int1;
  SimplePLocal_Int1 := @Local_Int1;
  SimplePGlob_Int1  := @SimpleGlob_Int1;

  inc(SimpleVArg_Int1); // BREAK Single 1
end;

{ TSimpleClass1 }

procedure TSimpleClass1.InitFields;
begin
  {%region Field }
  Field_Short1    := 11;
  Field_Small1    := 112;
  Field_Int1      := 1123;
  Field_QInt1     := 11234;

  Field_Byte1     := 22;
  Field_Word1     := 223;
  Field_DWord1    := 2234;
  Field_QWord1    := 22345;

  Field_Single1   := 0.5;
  Field_Double1   := 0.25;
  Field_Ext1      := 0.75;
  Field_Comp1     := -9;

  Field_Bool1      := True;
  Field_Bool2      := False;
  Field_Enum1     := (eval2);
  Field_Enum2     := (eval1);
  Field_Set1      := [eval2];
  Field_Set2      := [];


  Field_PShort1   := @Field_Short1;
  Field_PSmall1   := @Field_Small1;
  Field_PInt1     := @Field_Int1;
  Field_PQInt1    := @Field_QInt1;

  Field_PByte1    := @Field_Byte1;
  Field_PWord1    := @Field_Word1;
  Field_PDWord1   := @Field_DWord1;
  Field_PQWord1   := @Field_QWord1;

  Field_PSingle1  := @Field_Single1;
  Field_PDouble1  := @Field_Double1;
  Field_PExt1     := @Field_Ext1;
  Field_PComp1    := @Field_Comp1;

  Field_PBool1     := @Field_Bool1;
  Field_PBool2     := @Field_Bool2;
  Field_PEnum1    := @Field_Enum1;
  Field_PEnum2    := @Field_Enum2;
  Field_PSet1     := @Field_Set1;
  Field_PSet2     := @Field_Set2;
  {%endregion Field }
end;

procedure TSimpleClass1.Test1Method(Arg_Short1: ShortInt; Arg_Small1: Smallint;
  Arg_Int1: LongInt; Arg_QInt1: Int64; Arg_Byte1: Byte; Arg_Word1: Word; Arg_DWord1: LongWord;
  Arg_QWord1: QWord; Arg_Single1: Single; Arg_Double1: Double; Arg_Ext1: Extended;
  Arg_Comp1: Comp; Arg_Bool1, Arg_Bool2: Boolean; Arg_Enum1, Arg_Enum2: TSimpleEnum1;
  Arg_Set1, Arg_Set2: TSimpleSet1; Arg_PShort1: PShortInt; Arg_PSmall1: PSmallint;
  Arg_PInt1: PLongInt; Arg_PQInt1: PInt64; Arg_PByte1: PByte; Arg_PWord1: PWord;
  Arg_PDWord1: PLongWord; Arg_PQWord1: PQWord; Arg_PSingle1: PSingle; Arg_PDouble1: PDouble;
  Arg_PExt1: PExtended; Arg_PComp1: PComp; Arg_PBool1, Arg_PBool2: PBoolean; Arg_PEnum1,
  Arg_PEnum2: PSimpleEnum1; Arg_PSet1, Arg_PSet2: PSimpleSet1; var VArg_Short1: ShortInt;
  var VArg_Small1: Smallint; var VArg_Int1: LongInt; var VArg_QInt1: Int64;
  var VArg_Byte1: Byte; var VArg_Word1: Word; var VArg_DWord1: LongWord;
  var VArg_QWord1: QWord; var VArg_Single1: Single; var VArg_Double1: Double;
  var VArg_Ext1: Extended; var VArg_Comp1: Comp; var VArg_Bool1, VArg_Bool2: Boolean;
  var VArg_Enum1, VArg_Enum2: TSimpleEnum1; var VArg_Set1, VArg_Set2: TSimpleSet1;
  var VArg_PShort1: PShortInt; var VArg_PSmall1: PSmallint; var VArg_PInt1: PLongInt;
  var VArg_PQInt1: PInt64; var VArg_PByte1: PByte; var VArg_PWord1: PWord;
  var VArg_PDWord1: PLongWord; var VArg_PQWord1: PQWord; var VArg_PSingle1: PSingle;
  var VArg_PDouble1: PDouble; var VArg_PExt1: PExtended; var VArg_PComp1: PComp;
  var VArg_PBool1, VArg_PBool2: PBoolean; var VArg_PEnum1, VArg_PEnum2: PSimpleEnum1;
  var VArg_PSet1, VArg_PSet2: PSimpleSet1);
var
  Local_Short1: ShortInt;
  Local_Small1: Smallint;
  Local_Int1: LongInt;
  Local_QInt1: Int64;

  Local_Byte1: Byte;
  Local_Word1: Word;
  Local_DWord1: LongWord;
  Local_QWord1: QWord;

  Local_Single1: Single;
  Local_Double1: Double;
  Local_Ext1: Extended;
  Local_Comp1: Comp;

  Local_Bool1, Local_Bool2: Boolean;
  Local_Enum1, Local_Enum2: TSimpleEnum1;
  Local_Set1, Local_Set2: TSimpleSet1;


  Local_PShort1: PShortInt;
  Local_PSmall1: PSmallint;
  Local_PInt1: PLongInt;
  Local_PQInt1: PInt64;

  Local_PByte1: PByte;
  Local_PWord1: PWord;
  Local_PDWord1: PLongWord;
  Local_PQWord1: PQWord;

  Local_PSingle1: PSingle;
  Local_PDouble1: PDouble;
  Local_PExt1: PExtended;
  Local_PComp1: PComp;

  Local_PBool1, Local_PBool2: PBoolean;
  Local_PEnum1, Local_PEnum2: PSimpleEnum1;
  Local_PSet1, Local_PSet2: PSimpleSet1;
begin
  InitFields;

  {%region local }
  Local_Short1    := 11;
  Local_Small1    := 112;
  Local_Int1      := 1123;
  Local_QInt1     := 11234;

  Local_Byte1     := 22;
  Local_Word1     := 223;
  Local_DWord1    := 2234;
  Local_QWord1    := 22345;

  Local_Single1   := 0.5;
  Local_Double1   := 0.25;
  Local_Ext1      := 0.75;
  Local_Comp1     := -9;

  Local_Bool1      := True;
  Local_Bool2      := False;
  Local_Enum1     := (eval2);
  Local_Enum2     := (eval1);
  Local_Set1      := [eval2];
  Local_Set2      := [];


  Local_PShort1   := @Local_Short1;
  Local_PSmall1   := @Local_Small1;
  Local_PInt1     := @Local_Int1;
  Local_PQInt1    := @Local_QInt1;

  Local_PByte1    := @Local_Byte1;
  Local_PWord1    := @Local_Word1;
  Local_PDWord1   := @Local_DWord1;
  Local_PQWord1   := @Local_QWord1;

  Local_PSingle1  := @Local_Single1;
  Local_PDouble1  := @Local_Double1;
  Local_PExt1     := @Local_Ext1;
  Local_PComp1    := @Local_Comp1;

  Local_PBool1     := @Local_Bool1;
  Local_PBool2     := @Local_Bool2;
  Local_PEnum1    := @Local_Enum1;
  Local_PEnum2    := @Local_Enum2;
  Local_PSet1     := @Local_Set1;
  Local_PSet2     := @Local_Set2;
  {%endregion local }

  {%region ARG}

  Arg_PShort1   := @Arg_Short1;
  Arg_PSmall1   := @Arg_Small1;
  Arg_PInt1     := @Arg_Int1;
  Arg_PQInt1    := @Arg_QInt1;

  Arg_PByte1    := @Arg_Byte1;
  Arg_PWord1    := @Arg_Word1;
  Arg_PDWord1   := @Arg_DWord1;
  Arg_PQWord1   := @Arg_QWord1;

  Arg_PSingle1  := @Arg_Single1;
  Arg_PDouble1  := @Arg_Double1;
  Arg_PExt1     := @Arg_Ext1;
  Arg_PComp1    := @Arg_Comp1;

  Arg_PBool1    := @Arg_Bool1;
  Arg_PBool2    := @Arg_Bool2;
  Arg_PEnum1    := @Arg_Enum1;
  Arg_PEnum2    := @Arg_Enum2;
  Arg_PSet1     := @Arg_Set1;
  Arg_PSet2     := @Arg_Set2;
  {%endregion ARG}

  {%region VARG}

  VArg_PShort1   := @VArg_Short1;
  VArg_PSmall1   := @VArg_Small1;
  VArg_PInt1     := @VArg_Int1;
  VArg_PQInt1    := @VArg_QInt1;

  VArg_PByte1    := @VArg_Byte1;
  VArg_PWord1    := @VArg_Word1;
  VArg_PDWord1   := @VArg_DWord1;
  VArg_PQWord1   := @VArg_QWord1;

  VArg_PSingle1  := @VArg_Single1;
  VArg_PDouble1  := @VArg_Double1;
  VArg_PExt1     := @VArg_Ext1;
  VArg_PComp1    := @VArg_Comp1;

  VArg_PBool1     := @VArg_Bool1;
  VArg_PBool2     := @VArg_Bool2;
  VArg_PEnum1    := @VArg_Enum1;
  VArg_PEnum2    := @VArg_Enum2;
  VArg_PSet1     := @VArg_Set1;
  VArg_PSet2     := @VArg_Set2;
  {%endregion VARG}


  inc(SimpleGlob_Byte2); // BREAK Single 2
end;

{ TSimpleClass2 }

procedure TSimpleClass2.Test2Method;
begin
  InitFields;
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
    Field_Short1  := 11;
    Field_Small1  := 12;
    Field_Int1    := 13;
    Field_QInt1   := 14;

    Field_Byte1   := 15;
    Field_Word1   := 16;
    Field_DWord1  := 17;
    Field_QWord1  := 18;

    Field_Single1 := 21;
    Field_Double1 := 21;
    Field_Ext1    := 21;
  end;
  with SimpleGlob_Class2 do begin
    Field_Short1  := 111;
    Field_Small1  := 112;
    Field_Int1    := 113;
    Field_QInt1   := 114;

    Field_Byte1   := 115;
    Field_Word1   := 116;
    Field_DWord1  := 117;
    Field_QWord1  := 118;

    Field_Single1 := 121.3;
    Field_Double1 := 121.4;
    Field_Ext1    := 121.5;
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


  {%region global }
  SimpleGlob_Short1    := 11;
  SimpleGlob_Small1    := 112;
  SimpleGlob_Int1      := 1123;
  SimpleGlob_QInt1     := 11234;

  SimpleGlob_Byte1     := 22;
  SimpleGlob_Word1     := 223;
  SimpleGlob_DWord1    := 2234;
  SimpleGlob_QWord1    := 22345;

  SimpleGlob_Single1   := 0.5;
  SimpleGlob_Double1   := 0.25;
  SimpleGlob_Ext1      := 0.75;
  SimpleGlob_Comp1     := -9;

  SimpleGlob_Bool1      := True;
  SimpleGlob_Bool2      := False;
  SimpleGlob_Enum1     := (eval2);
  SimpleGlob_Enum2     := (eval1);
  SimpleGlob_Set1      := [eval2];
  SimpleGlob_Set2      := [];


  SimpleGlob_PShort1   := @SimpleGlob_Short1;
  SimpleGlob_PSmall1   := @SimpleGlob_Small1;
  SimpleGlob_PInt1     := @SimpleGlob_Int1;
  SimpleGlob_PQInt1    := @SimpleGlob_QInt1;

  SimpleGlob_PByte1    := @SimpleGlob_Byte1;
  SimpleGlob_PWord1    := @SimpleGlob_Word1;
  SimpleGlob_PDWord1   := @SimpleGlob_DWord1;
  SimpleGlob_PQWord1   := @SimpleGlob_QWord1;

  SimpleGlob_PSingle1  := @SimpleGlob_Single1;
  SimpleGlob_PDouble1  := @SimpleGlob_Double1;
  SimpleGlob_PExt1     := @SimpleGlob_Ext1;
  SimpleGlob_PComp1    := @SimpleGlob_Comp1;

  SimpleGlob_PBool1     := @SimpleGlob_Bool1;
  SimpleGlob_PBool2     := @SimpleGlob_Bool2;
  SimpleGlob_PEnum1    := @SimpleGlob_Enum1;
  SimpleGlob_PEnum2    := @SimpleGlob_Enum2;
  SimpleGlob_PSet1     := @SimpleGlob_Set1;
  SimpleGlob_PSet2     := @SimpleGlob_Set2;
  {%endregion global }

  // Copy of pointers for var param
  SimpleGlob_P2Short1  := @SimpleGlob_Short1;
  SimpleGlob_P2Small1  := @SimpleGlob_Small1;
  SimpleGlob_P2Int1    := @SimpleGlob_Int1;
  SimpleGlob_P2QInt1   := @SimpleGlob_QInt1;

  SimpleGlob_P2Byte1   := @SimpleGlob_Byte1;
  SimpleGlob_P2Word1   := @SimpleGlob_Word1;
  SimpleGlob_P2DWord1  := @SimpleGlob_DWord1;
  SimpleGlob_P2QWord1  := @SimpleGlob_QWord1;

  SimpleGlob_P2Single1 := @SimpleGlob_Single1;
  SimpleGlob_P2Double1 := @SimpleGlob_Double1;
  SimpleGlob_P2Ext1    := @SimpleGlob_Ext1;
  SimpleGlob_P2Comp1   := @SimpleGlob_Comp1;

  SimpleGlob_P2Bool1     := @SimpleGlob_Bool1;
  SimpleGlob_P2Bool2     := @SimpleGlob_Bool2;
  SimpleGlob_P2Enum1    := @SimpleGlob_Enum1;
  SimpleGlob_P2Enum2    := @SimpleGlob_Enum2;
  SimpleGlob_P2Set1     := @SimpleGlob_Set1;
  SimpleGlob_P2Set2     := @SimpleGlob_Set2;




  SimpleGlob_Class1.Test1Method(
    SimpleGlob_Short1,
    SimpleGlob_Small1,
    SimpleGlob_Int1,
    SimpleGlob_QInt1,

    SimpleGlob_Byte1,
    SimpleGlob_Word1,
    SimpleGlob_DWord1,
    SimpleGlob_QWord1,

    SimpleGlob_Single1,
    SimpleGlob_Double1,
    SimpleGlob_Ext1,
    SimpleGlob_Comp1,

    SimpleGlob_Bool1, SimpleGlob_Bool2,
    SimpleGlob_Enum1, SimpleGlob_Enum2,
    SimpleGlob_Set1, SimpleGlob_Set2,


    SimpleGlob_PShort1,
    SimpleGlob_PSmall1,
    SimpleGlob_PInt1,
    SimpleGlob_PQInt1,

    SimpleGlob_PByte1,
    SimpleGlob_PWord1,
    SimpleGlob_PDWord1,
    SimpleGlob_PQWord1,

    SimpleGlob_PSingle1,
    SimpleGlob_PDouble1,
    SimpleGlob_PExt1,
    SimpleGlob_PComp1,

    SimpleGlob_PBool1, SimpleGlob_PBool2,
    SimpleGlob_PEnum1, SimpleGlob_PEnum2,
    SimpleGlob_PSet1, SimpleGlob_PSet2,


    SimpleGlob_Short1,
    SimpleGlob_Small1,
    SimpleGlob_Int1,
    SimpleGlob_QInt1,

    SimpleGlob_Byte1,
    SimpleGlob_Word1,
    SimpleGlob_DWord1,
    SimpleGlob_QWord1,

    SimpleGlob_Single1,
    SimpleGlob_Double1,
    SimpleGlob_Ext1,
    SimpleGlob_Comp1,

    SimpleGlob_Bool1, SimpleGlob_Bool2,
    SimpleGlob_Enum1, SimpleGlob_Enum2,
    SimpleGlob_Set1, SimpleGlob_Set2,


    SimpleGlob_P2Short1,
    SimpleGlob_P2Small1,
    SimpleGlob_P2Int1,
    SimpleGlob_P2QInt1,

    SimpleGlob_P2Byte1,
    SimpleGlob_P2Word1,
    SimpleGlob_P2DWord1,
    SimpleGlob_P2QWord1,

    SimpleGlob_P2Single1,
    SimpleGlob_P2Double1,
    SimpleGlob_P2Ext1,
    SimpleGlob_P2Comp1,

    SimpleGlob_P2Bool1, SimpleGlob_P2Bool2,
    SimpleGlob_P2Enum1, SimpleGlob_P2Enum2,
    SimpleGlob_P2Set1, SimpleGlob_P2Set2

  );
  SimpleGlob_Class2.Test2Method;
end;

end.

