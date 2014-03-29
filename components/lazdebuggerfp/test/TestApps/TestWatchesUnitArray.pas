unit TestWatchesUnitArray;
{$mode objfpc}{$H+}{$NOTES off}

interface

uses sysutils, types;

type
  TArrayClass1 = class;
  TArrayRec = packed record
    FieldInt1, FieldInt2: Integer;
    FieldDynInt1: array of Integer;
    FieldStatInt1: array [4..9] of Integer;
    FieldByte1: Byte;
  end;
  TArrayRec2 = packed record
    FieldByte1, FieldByte2, FieldByte3: Byte;
  end;

  TArrayDynInt        = array of Integer;
  TArrayDynClass      = array of TArrayClass1;
  TArrayDynRec        = array of TArrayRec;

  TArrayDynDynInt     = array of array of Integer;
  TArrayDynDynClass   = array of array of TArrayClass1;
  TArrayDynDynRec     = array of array of TArrayRec;

  TArrayDynStatInt    = array of array [1..5] of Integer;
  TArrayDynStatClass  = array of array [1..5] of TArrayClass1;
  TArrayDynStatRec    = array of array [1..5] of TArrayRec;

  TArrayStatInt       = array [-2..5] of Integer;
  TArrayStatClass     = array [-2..5] of TArrayClass1;
  TArrayStatRec       = array [-2..5] of TArrayRec;

  TArrayStatStatInt   = array [-9..-5] of array [1..3] of Integer;
  TArrayStatStatClass = array [-9..-5] of array [1..3] of TArrayClass1;
  TArrayStatStatRec   = array [-9..-5] of array [1..3] of TArrayRec;

  TArrayStatDynInt    = array [-9..-5] of array of Integer;
  TArrayStatDynClass  = array [-9..-5] of array of TArrayClass1;
  TArrayStatDynRec    = array [-9..-5] of array of TArrayRec;

  // TYPES fol param
  TArrayStatIntParam1 = array [4..9] of Integer;
  TArrayStatIntParam2 = array [-4..9] of Integer;

  { TArrayClass1 }

  TArrayClass1 = class
  public
    Field_Int1, Field_Int2: Integer;
    Field_DynAInt1: array of Integer;
    Field_StatAInt1: array [4..9] of Integer;
    Field_StatAInt2: array [-4..9] of Integer;

    Field_DynInt1, Field_DynInt2: TArrayDynInt;
    Field_DynClass1: TArrayDynClass;
    Field_DynRec1: TArrayDynRec;
    Field_DynRec2: array of TArrayRec2;

    Field_DynDynInt1: TArrayDynDynInt;
    Field_DynDynClass1: TArrayDynDynClass;
    Field_DynDynRec1: TArrayDynDynRec;

    Field_DynStatInt1: TArrayDynStatInt;
    Field_DynStatClass1: TArrayDynStatClass;
    Field_DynStatRec1: TArrayDynStatRec;

    Field_StatInt1: TArrayStatInt;
    Field_StatClass1: TArrayStatClass;
    Field_StatRec1: TArrayStatRec;

    Field_StatStatInt1: TArrayStatStatInt;
    Field_StatStatClass1: TArrayStatStatClass;
    Field_StatStatRec1: TArrayStatStatRec;

    Field_StatDynInt1: TArrayStatDynInt;
    Field_StatDynClass1: TArrayStatDynClass;
    Field_StatDynRec1: TArrayStatDynRec;

    procedure Test1Method(
      Arg_DynAInt1: array of Integer;
      Arg_StatAInt1: TArrayStatIntParam1;
      Arg_StatAInt2: TArrayStatIntParam2;

      Arg_DynInt1, Arg_DynInt2: TArrayDynInt;
      Arg_DynClass1: TArrayDynClass;
      Arg_DynRec1: TArrayDynRec;
      Arg_DynRec2: array of TArrayRec2;

      Arg_DynDynInt1: TArrayDynDynInt;
      Arg_DynDynClass1: TArrayDynDynClass;
      Arg_DynDynRec1: TArrayDynDynRec;

      Arg_DynStatInt1: TArrayDynStatInt;
      Arg_DynStatClass1: TArrayDynStatClass;
      Arg_DynStatRec1: TArrayDynStatRec;

      Arg_StatInt1: TArrayStatInt;
      Arg_StatClass1: TArrayStatClass;
      Arg_StatRec1: TArrayStatRec;

      Arg_StatStatInt1: TArrayStatStatInt;
      Arg_StatStatClass1: TArrayStatStatClass;
      Arg_StatStatRec1: TArrayStatStatRec;

      Arg_StatDynInt1: TArrayStatDynInt;
      Arg_StatDynClass1: TArrayStatDynClass;
      Arg_StatDynRec1: TArrayStatDynRec;


      var VArg_DynAInt1: array of Integer;
      var VArg_StatAInt1: TArrayStatIntParam1;
      var VArg_StatAInt2: TArrayStatIntParam2;

      var VArg_DynInt1, VArg_DynInt2: TArrayDynInt;
      var VArg_DynClass1: TArrayDynClass;
      var VArg_DynRec1: TArrayDynRec;
      var VArg_DynRec2: array of TArrayRec2;

      var VArg_DynDynInt1: TArrayDynDynInt;
      var VArg_DynDynClass1: TArrayDynDynClass;
      var VArg_DynDynRec1: TArrayDynDynRec;

      var VArg_DynStatInt1: TArrayDynStatInt;
      var VArg_DynStatClass1: TArrayDynStatClass;
      var VArg_DynStatRec1: TArrayDynStatRec;

      var VArg_StatInt1: TArrayStatInt;
      var VArg_StatClass1: TArrayStatClass;
      var VArg_StatRec1: TArrayStatRec;

      var VArg_StatStatInt1: TArrayStatStatInt;
      var VArg_StatStatClass1: TArrayStatStatClass;
      var VArg_StatStatRec1: TArrayStatStatRec;

      var VArg_StatDynInt1: TArrayStatDynInt;
      var VArg_StatDynClass1: TArrayStatDynClass;
      var VArg_StatDynRec1: TArrayStatDynRec
    );
  end;

var
  ArrayGlob_DynAInt1: array of Integer;
  ArrayGlob_StatAInt1: array [4..9] of Integer;
  ArrayGlob_StatAInt2: array [-4..9] of Integer;

  ArrayGlob_DynInt1, ArrayGlob_DynInt2: TArrayDynInt;
  ArrayGlob_DynClass1: TArrayDynClass;
  ArrayGlob_DynRec1: TArrayDynRec;
  ArrayGlob_DynRec2: array of TArrayRec2;

  ArrayGlob_DynDynInt1: TArrayDynDynInt;
  ArrayGlob_DynDynClass1: TArrayDynDynClass;
  ArrayGlob_DynDynRec1: TArrayDynDynRec;

  ArrayGlob_DynStatInt1: TArrayDynStatInt;
  ArrayGlob_DynStatClass1: TArrayDynStatClass;
  ArrayGlob_DynStatRec1: TArrayDynStatRec;

  ArrayGlob_StatInt1: TArrayStatInt;
  ArrayGlob_StatClass1: TArrayStatClass;
  ArrayGlob_StatRec1: TArrayStatRec;

  ArrayGlob_StatStatInt1: TArrayStatStatInt;
  ArrayGlob_StatStatClass1: TArrayStatStatClass;
  ArrayGlob_StatStatRec1: TArrayStatStatRec;

  ArrayGlob_StatDynInt1: TArrayStatDynInt;
  ArrayGlob_StatDynClass1: TArrayStatDynClass;
  ArrayGlob_StatDynRec1: TArrayStatDynRec;

procedure Test1;

implementation

{ TArrayClass1 }

procedure TArrayClass1.Test1Method(Arg_DynAInt1: array of Integer;
  Arg_StatAInt1: TArrayStatIntParam1; Arg_StatAInt2: TArrayStatIntParam2; Arg_DynInt1,
  Arg_DynInt2: TArrayDynInt; Arg_DynClass1: TArrayDynClass; Arg_DynRec1: TArrayDynRec;
  Arg_DynRec2: array of TArrayRec2; Arg_DynDynInt1: TArrayDynDynInt;
  Arg_DynDynClass1: TArrayDynDynClass; Arg_DynDynRec1: TArrayDynDynRec;
  Arg_DynStatInt1: TArrayDynStatInt; Arg_DynStatClass1: TArrayDynStatClass;
  Arg_DynStatRec1: TArrayDynStatRec; Arg_StatInt1: TArrayStatInt;
  Arg_StatClass1: TArrayStatClass; Arg_StatRec1: TArrayStatRec;
  Arg_StatStatInt1: TArrayStatStatInt; Arg_StatStatClass1: TArrayStatStatClass;
  Arg_StatStatRec1: TArrayStatStatRec; Arg_StatDynInt1: TArrayStatDynInt;
  Arg_StatDynClass1: TArrayStatDynClass; Arg_StatDynRec1: TArrayStatDynRec;
  var VArg_DynAInt1: array of Integer; var VArg_StatAInt1: TArrayStatIntParam1;
  var VArg_StatAInt2: TArrayStatIntParam2; var VArg_DynInt1, VArg_DynInt2: TArrayDynInt;
  var VArg_DynClass1: TArrayDynClass; var VArg_DynRec1: TArrayDynRec;
  var VArg_DynRec2: array of TArrayRec2; var VArg_DynDynInt1: TArrayDynDynInt;
  var VArg_DynDynClass1: TArrayDynDynClass; var VArg_DynDynRec1: TArrayDynDynRec;
  var VArg_DynStatInt1: TArrayDynStatInt; var VArg_DynStatClass1: TArrayDynStatClass;
  var VArg_DynStatRec1: TArrayDynStatRec; var VArg_StatInt1: TArrayStatInt;
  var VArg_StatClass1: TArrayStatClass; var VArg_StatRec1: TArrayStatRec;
  var VArg_StatStatInt1: TArrayStatStatInt; var VArg_StatStatClass1: TArrayStatStatClass;
  var VArg_StatStatRec1: TArrayStatStatRec; var VArg_StatDynInt1: TArrayStatDynInt;
  var VArg_StatDynClass1: TArrayStatDynClass; var VArg_StatDynRec1: TArrayStatDynRec);
var
  Local_DynAInt1: array of Integer;
  Local_StatAInt1: array [4..9] of Integer;
  Local_StatAInt2: array [-4..9] of Integer;

  Local_DynInt1, Local_DynInt2: TArrayDynInt;
  Local_DynClass1: TArrayDynClass;
  Local_DynRec1: TArrayDynRec;
  Local_DynRec2: array of TArrayRec2;

  Local_DynDynInt1: TArrayDynDynInt;
  Local_DynDynClass1: TArrayDynDynClass;
  Local_DynDynRec1: TArrayDynDynRec;

  Local_DynStatInt1: TArrayDynStatInt;
  Local_DynStatClass1: TArrayDynStatClass;
  Local_DynStatRec1: TArrayDynStatRec;

  Local_StatInt1: TArrayStatInt;
  Local_StatClass1: TArrayStatClass;
  Local_StatRec1: TArrayStatRec;

  Local_StatStatInt1: TArrayStatStatInt;
  Local_StatStatClass1: TArrayStatStatClass;
  Local_StatStatRec1: TArrayStatStatRec;

  Local_StatDynInt1: TArrayStatDynInt;
  Local_StatDynClass1: TArrayStatDynClass;
  Local_StatDynRec1: TArrayStatDynRec;
begin

  {%region  Fields }
  SetLength(Field_DynAInt1,   5);
    Field_DynAInt1[0] := 100;
    Field_DynAInt1[1] := 101;
    Field_DynAInt1[2] := 102;

  Field_StatAInt1[4] := 6600;
  Field_StatAInt1[5] := 6601;
  Field_StatAInt1[6] := 6602;
  Field_StatAInt1[9] := 6699;

  Field_StatAInt2[-4] := 3300;
  Field_StatAInt2[-3] := 3301;
  Field_StatAInt2[-2] := 3302;
  Field_StatAInt2[-1] := 3303;
  Field_StatAInt2[0] := 3304;
  Field_StatAInt2[1] := 3305;


  SetLength(Field_DynInt1,20);
    Field_DynInt1[0] := 5511;
    Field_DynInt1[1] := 5512;
    Field_DynInt1[2] := 5513;
    Field_DynInt1[3] := 5514;
    Field_DynInt1[4] := -5511;
    Field_DynInt1[19] := 5500;
  Field_DynInt2 := nil;

  SetLength(Field_DynClass1, 6);
    Field_DynClass1[0] := TArrayClass1.Create;
      Field_DynClass1[0].Field_Int1 := 98700;
      Field_DynClass1[0].Field_Int2 := 98701;
      SetLength(Field_DynClass1[0].Field_DynAInt1, 2);
        Field_DynClass1[0].Field_DynAInt1[0] := 9900;
        Field_DynClass1[0].Field_DynAInt1[1] := 9901;
    Field_DynClass1[1] := TArrayClass1.Create;
      Field_DynClass1[1].Field_Int1 := 88700;
      Field_DynClass1[1].Field_Int2 := 88701;
      SetLength(Field_DynClass1[1].Field_DynAInt1, 2);
        Field_DynClass1[1].Field_DynAInt1[0] := 8900;
        Field_DynClass1[1].Field_DynAInt1[1] := 8901;
    Field_DynClass1[2] := TArrayClass1.Create;
      Field_DynClass1[2].Field_Int1 := 78700;
      Field_DynClass1[2].Field_Int2 := 78701;
      SetLength(Field_DynClass1[2].Field_DynAInt1, 3);
        Field_DynClass1[2].Field_DynAInt1[0] := 7900;
        Field_DynClass1[2].Field_DynAInt1[1] := 7901;
        Field_DynClass1[2].Field_DynAInt1[2] := 7902;

  SetLength(Field_DynRec1,   7);
    Field_DynRec1[0].FieldInt1 := 200;
    Field_DynRec1[0].FieldInt2 := 201;
    Field_DynRec1[1].FieldInt1 := 210;
    Field_DynRec1[1].FieldInt2 := 211;
    Field_DynRec1[2].FieldInt1 := 220;
    Field_DynRec1[2].FieldInt2 := 221;

  SetLength(Field_DynRec2,   7);
    Field_DynRec2[0].FieldByte1 := 200;
    Field_DynRec2[0].FieldByte2 := 201;
    Field_DynRec2[1].FieldByte1 := 210;
    Field_DynRec2[1].FieldByte2 := 211;
    Field_DynRec2[2].FieldByte1 := 220;
    Field_DynRec2[2].FieldByte2 := 221;

  SetLength(Field_DynDynInt1,   5,3);
    Field_DynDynInt1[0][0] := 1000;
    Field_DynDynInt1[0][1] := 1001;
    Field_DynDynInt1[0][2] := 1002;
    Field_DynDynInt1[1][0] := 1010;
    Field_DynDynInt1[1][1] := 1011;
    Field_DynDynInt1[1][2] := 1012;
    Field_DynDynInt1[2][0] := 1020;
    Field_DynDynInt1[2][1] := 1021;
    Field_DynDynInt1[2][2] := 1022;
    Field_DynDynInt1[3][0] := 1;
    Field_DynDynInt1[3][1] := 2;
    Field_DynDynInt1[3][2] := 1;

  SetLength(Field_DynDynClass1, 5,2);
    Field_DynDynClass1[0,0] := TArrayClass1.Create;
      Field_DynDynClass1[0][0].Field_Int1 := 5000;
    Field_DynDynClass1[0,1] := TArrayClass1.Create;
      Field_DynDynClass1[0][1].Field_Int1 := 5001;
    Field_DynDynClass1[1,0] := nil;
    Field_DynDynClass1[1,1] := TArrayClass1.Create;
      Field_DynDynClass1[1][1].Field_Int1 := 5011;
    Field_DynDynClass1[2,0] := nil;
    Field_DynDynClass1[2,1] := nil;


  SetLength(Field_DynDynRec1,   5,6);

  SetLength(Field_DynStatInt1,   3);
  SetLength(Field_DynStatClass1, 4);
  SetLength(Field_DynStatRec1,   5);

  //SetLength(Field_StatInt1, );
  //SetLength(Field_StatClass1, );
  //SetLength(Field_StatRec1, );

  //SetLength(Field_StatStatInt1, );
    Field_StatStatInt1[-9, 1] := 4091;
    Field_StatStatInt1[-9, 2] := 4092;
    Field_StatStatInt1[-9, 3] := 4093;
    Field_StatStatInt1[-8, 1] := 4081;
    Field_StatStatInt1[-8, 2] := 4082;
    Field_StatStatInt1[-8, 3] := 4083;
    Field_StatStatInt1[-7, 1] := 4071;
    Field_StatStatInt1[-7, 2] := 4072;
    Field_StatStatInt1[-7, 3] := 4073;
  //SetLength(Field_StatStatClass1, );
  //SetLength(Field_StatStatRec1, );

  SetLength(Field_StatDynInt1[-9],   3);
  SetLength(Field_StatDynClass1[-9], 3);
  SetLength(Field_StatDynRec1[-9],   3);

  {%endregion  Fields }


  {%region  Local }
  SetLength(Local_DynAInt1,   5);
    Local_DynAInt1[0] := 100;
    Local_DynAInt1[1] := 101;
    Local_DynAInt1[2] := 102;

  Local_StatAInt1[4] := 6600;
  Local_StatAInt1[5] := 6601;
  Local_StatAInt1[6] := 6602;
  Local_StatAInt1[9] := 6699;

  Local_StatAInt2[-4] := 3300;
  Local_StatAInt2[-3] := 3301;
  Local_StatAInt2[-2] := 3302;
  Local_StatAInt2[-1] := 3303;
  Local_StatAInt2[0] := 3304;
  Local_StatAInt2[1] := 3305;


  SetLength(Local_DynInt1,20);
    Local_DynInt1[0] := 5511;
    Local_DynInt1[1] := 5512;
    Local_DynInt1[2] := 5513;
    Local_DynInt1[3] := 5514;
    Local_DynInt1[4] := -5511;
    Local_DynInt1[19] := 5500;
  Local_DynInt2 := nil;

  SetLength(Local_DynClass1, 6);
    Local_DynClass1[0] := TArrayClass1.Create;
      Local_DynClass1[0].Field_Int1 := 98700;
      Local_DynClass1[0].Field_Int2 := 98701;
      SetLength(Local_DynClass1[0].Field_DynAInt1, 2);
        Local_DynClass1[0].Field_DynAInt1[0] := 9900;
        Local_DynClass1[0].Field_DynAInt1[1] := 9901;
    Local_DynClass1[1] := TArrayClass1.Create;
      Local_DynClass1[1].Field_Int1 := 88700;
      Local_DynClass1[1].Field_Int2 := 88701;
      SetLength(Local_DynClass1[1].Field_DynAInt1, 2);
        Local_DynClass1[1].Field_DynAInt1[0] := 8900;
        Local_DynClass1[1].Field_DynAInt1[1] := 8901;
    Local_DynClass1[2] := TArrayClass1.Create;
      Local_DynClass1[2].Field_Int1 := 78700;
      Local_DynClass1[2].Field_Int2 := 78701;
      SetLength(Local_DynClass1[2].Field_DynAInt1, 3);
        Local_DynClass1[2].Field_DynAInt1[0] := 7900;
        Local_DynClass1[2].Field_DynAInt1[1] := 7901;
        Local_DynClass1[2].Field_DynAInt1[2] := 7902;

  SetLength(Local_DynRec1,   7);
    Local_DynRec1[0].FieldInt1 := 200;
    Local_DynRec1[0].FieldInt2 := 201;
    Local_DynRec1[1].FieldInt1 := 210;
    Local_DynRec1[1].FieldInt2 := 211;
    Local_DynRec1[2].FieldInt1 := 220;
    Local_DynRec1[2].FieldInt2 := 221;

  SetLength(Local_DynRec2,   7);
    Local_DynRec2[0].FieldByte1 := 200;
    Local_DynRec2[0].FieldByte2 := 201;
    Local_DynRec2[1].FieldByte1 := 210;
    Local_DynRec2[1].FieldByte2 := 211;
    Local_DynRec2[2].FieldByte1 := 220;
    Local_DynRec2[2].FieldByte2 := 221;

  SetLength(Local_DynDynInt1,   5,3);
    Local_DynDynInt1[0][0] := 1000;
    Local_DynDynInt1[0][1] := 1001;
    Local_DynDynInt1[0][2] := 1002;
    Local_DynDynInt1[1][0] := 1010;
    Local_DynDynInt1[1][1] := 1011;
    Local_DynDynInt1[1][2] := 1012;
    Local_DynDynInt1[2][0] := 1020;
    Local_DynDynInt1[2][1] := 1021;
    Local_DynDynInt1[2][2] := 1022;
    Local_DynDynInt1[3][0] := 1;
    Local_DynDynInt1[3][1] := 2;
    Local_DynDynInt1[3][2] := 1;

  SetLength(Local_DynDynClass1, 5,2);
    Local_DynDynClass1[0,0] := TArrayClass1.Create;
      Local_DynDynClass1[0][0].Field_Int1 := 5000;
    Local_DynDynClass1[0,1] := TArrayClass1.Create;
      Local_DynDynClass1[0][1].Field_Int1 := 5001;
    Local_DynDynClass1[1,0] := nil;
    Local_DynDynClass1[1,1] := TArrayClass1.Create;
      Local_DynDynClass1[1][1].Field_Int1 := 5011;
    Local_DynDynClass1[2,0] := nil;
    Local_DynDynClass1[2,1] := nil;


  SetLength(Local_DynDynRec1,   5,6);

  SetLength(Local_DynStatInt1,   3);
  SetLength(Local_DynStatClass1, 4);
  SetLength(Local_DynStatRec1,   5);

  //SetLength(Local_StatInt1, );
  //SetLength(Local_StatClass1, );
  //SetLength(Local_StatRec1, );

  //SetLength(Local_StatStatInt1, );
    Local_StatStatInt1[-9, 1] := 4091;
    Local_StatStatInt1[-9, 2] := 4092;
    Local_StatStatInt1[-9, 3] := 4093;
    Local_StatStatInt1[-8, 1] := 4081;
    Local_StatStatInt1[-8, 2] := 4082;
    Local_StatStatInt1[-8, 3] := 4083;
    Local_StatStatInt1[-7, 1] := 4071;
    Local_StatStatInt1[-7, 2] := 4072;
    Local_StatStatInt1[-7, 3] := 4073;
  //SetLength(Local_StatStatClass1, );
  //SetLength(Local_StatStatRec1, );

  SetLength(Local_StatDynInt1[-9],   3);
  SetLength(Local_StatDynClass1[-9], 3);
  SetLength(Local_StatDynRec1[-9],   3);

  {%endregion  Local }

  ArrayGlob_DynInt1[9] := -5511; // BREAK
end;

procedure Test1;
var
  ArrayClass1: TArrayClass1;
begin
  {%region  ArrayGlob }
  SetLength(ArrayGlob_DynAInt1,   5);
    ArrayGlob_DynAInt1[0] := 100;
    ArrayGlob_DynAInt1[1] := 101;
    ArrayGlob_DynAInt1[2] := 102;

  ArrayGlob_StatAInt1[4] := 6600;
  ArrayGlob_StatAInt1[5] := 6601;
  ArrayGlob_StatAInt1[6] := 6602;
  ArrayGlob_StatAInt1[9] := 6699;

  ArrayGlob_StatAInt2[-4] := 3300;
  ArrayGlob_StatAInt2[-3] := 3301;
  ArrayGlob_StatAInt2[-2] := 3302;
  ArrayGlob_StatAInt2[-1] := 3303;
  ArrayGlob_StatAInt2[0] := 3304;
  ArrayGlob_StatAInt2[1] := 3305;


  SetLength(ArrayGlob_DynInt1,20);
    ArrayGlob_DynInt1[0] := 5511;
    ArrayGlob_DynInt1[1] := 5512;
    ArrayGlob_DynInt1[2] := 5513;
    ArrayGlob_DynInt1[3] := 5514;
    ArrayGlob_DynInt1[4] := -5511;
    ArrayGlob_DynInt1[19] := 5500;
  ArrayGlob_DynInt2 := nil;

  SetLength(ArrayGlob_DynClass1, 6);
    ArrayGlob_DynClass1[0] := TArrayClass1.Create;
      ArrayGlob_DynClass1[0].Field_Int1 := 98700;
      ArrayGlob_DynClass1[0].Field_Int2 := 98701;
      SetLength(ArrayGlob_DynClass1[0].Field_DynAInt1, 2);
        ArrayGlob_DynClass1[0].Field_DynAInt1[0] := 9900;
        ArrayGlob_DynClass1[0].Field_DynAInt1[1] := 9901;
    ArrayGlob_DynClass1[1] := TArrayClass1.Create;
      ArrayGlob_DynClass1[1].Field_Int1 := 88700;
      ArrayGlob_DynClass1[1].Field_Int2 := 88701;
      SetLength(ArrayGlob_DynClass1[1].Field_DynAInt1, 2);
        ArrayGlob_DynClass1[1].Field_DynAInt1[0] := 8900;
        ArrayGlob_DynClass1[1].Field_DynAInt1[1] := 8901;
    ArrayGlob_DynClass1[2] := TArrayClass1.Create;
      ArrayGlob_DynClass1[2].Field_Int1 := 78700;
      ArrayGlob_DynClass1[2].Field_Int2 := 78701;
      SetLength(ArrayGlob_DynClass1[2].Field_DynAInt1, 3);
        ArrayGlob_DynClass1[2].Field_DynAInt1[0] := 7900;
        ArrayGlob_DynClass1[2].Field_DynAInt1[1] := 7901;
        ArrayGlob_DynClass1[2].Field_DynAInt1[2] := 7902;

  SetLength(ArrayGlob_DynRec1,   7);
    ArrayGlob_DynRec1[0].FieldInt1 := 200;
    ArrayGlob_DynRec1[0].FieldInt2 := 201;
    ArrayGlob_DynRec1[1].FieldInt1 := 210;
    ArrayGlob_DynRec1[1].FieldInt2 := 211;
    ArrayGlob_DynRec1[2].FieldInt1 := 220;
    ArrayGlob_DynRec1[2].FieldInt2 := 221;

  SetLength(ArrayGlob_DynRec2,   7);
    ArrayGlob_DynRec2[0].FieldByte1 := 200;
    ArrayGlob_DynRec2[0].FieldByte2 := 201;
    ArrayGlob_DynRec2[1].FieldByte1 := 210;
    ArrayGlob_DynRec2[1].FieldByte2 := 211;
    ArrayGlob_DynRec2[2].FieldByte1 := 220;
    ArrayGlob_DynRec2[2].FieldByte2 := 221;

  SetLength(ArrayGlob_DynDynInt1,   5,3);
    ArrayGlob_DynDynInt1[0][0] := 1000;
    ArrayGlob_DynDynInt1[0][1] := 1001;
    ArrayGlob_DynDynInt1[0][2] := 1002;
    ArrayGlob_DynDynInt1[1][0] := 1010;
    ArrayGlob_DynDynInt1[1][1] := 1011;
    ArrayGlob_DynDynInt1[1][2] := 1012;
    ArrayGlob_DynDynInt1[2][0] := 1020;
    ArrayGlob_DynDynInt1[2][1] := 1021;
    ArrayGlob_DynDynInt1[2][2] := 1022;
    ArrayGlob_DynDynInt1[3][0] := 1;
    ArrayGlob_DynDynInt1[3][1] := 2;
    ArrayGlob_DynDynInt1[3][2] := 1;

  SetLength(ArrayGlob_DynDynClass1, 5,2);
    ArrayGlob_DynDynClass1[0,0] := TArrayClass1.Create;
      ArrayGlob_DynDynClass1[0][0].Field_Int1 := 5000;
    ArrayGlob_DynDynClass1[0,1] := TArrayClass1.Create;
      ArrayGlob_DynDynClass1[0][1].Field_Int1 := 5001;
    ArrayGlob_DynDynClass1[1,0] := nil;
    ArrayGlob_DynDynClass1[1,1] := TArrayClass1.Create;
      ArrayGlob_DynDynClass1[1][1].Field_Int1 := 5011;
    ArrayGlob_DynDynClass1[2,0] := nil;
    ArrayGlob_DynDynClass1[2,1] := nil;


  SetLength(ArrayGlob_DynDynRec1,   5,6);

  SetLength(ArrayGlob_DynStatInt1,   3);
  SetLength(ArrayGlob_DynStatClass1, 4);
  SetLength(ArrayGlob_DynStatRec1,   5);

  //SetLength(ArrayGlob_StatInt1, );
  //SetLength(ArrayGlob_StatClass1, );
  //SetLength(ArrayGlob_StatRec1, );

  //SetLength(ArrayGlob_StatStatInt1, );
    ArrayGlob_StatStatInt1[-9, 1] := 4091;
    ArrayGlob_StatStatInt1[-9, 2] := 4092;
    ArrayGlob_StatStatInt1[-9, 3] := 4093;
    ArrayGlob_StatStatInt1[-8, 1] := 4081;
    ArrayGlob_StatStatInt1[-8, 2] := 4082;
    ArrayGlob_StatStatInt1[-8, 3] := 4083;
    ArrayGlob_StatStatInt1[-7, 1] := 4071;
    ArrayGlob_StatStatInt1[-7, 2] := 4072;
    ArrayGlob_StatStatInt1[-7, 3] := 4073;
  //SetLength(ArrayGlob_StatStatClass1, );
  //SetLength(ArrayGlob_StatStatRec1, );

  SetLength(ArrayGlob_StatDynInt1[-9],   3);
  SetLength(ArrayGlob_StatDynClass1[-9], 3);
  SetLength(ArrayGlob_StatDynRec1[-9],   3);

  {%endregion  ArrayGlob }

  ArrayClass1 := TArrayClass1.Create;
  ArrayClass1.Test1Method(
    ArrayGlob_DynAInt1,
    ArrayGlob_StatAInt1,
    ArrayGlob_StatAInt2,

    ArrayGlob_DynInt1, ArrayGlob_DynInt2,
    ArrayGlob_DynClass1,
    ArrayGlob_DynRec1,
    ArrayGlob_DynRec2,

    ArrayGlob_DynDynInt1,
    ArrayGlob_DynDynClass1,
    ArrayGlob_DynDynRec1,

    ArrayGlob_DynStatInt1,
    ArrayGlob_DynStatClass1,
    ArrayGlob_DynStatRec1,

    ArrayGlob_StatInt1,
    ArrayGlob_StatClass1,
    ArrayGlob_StatRec1,

    ArrayGlob_StatStatInt1,
    ArrayGlob_StatStatClass1,
    ArrayGlob_StatStatRec1,

    ArrayGlob_StatDynInt1,
    ArrayGlob_StatDynClass1,
    ArrayGlob_StatDynRec1,


    ArrayGlob_DynAInt1,
    ArrayGlob_StatAInt1,
    ArrayGlob_StatAInt2,

    ArrayGlob_DynInt1, ArrayGlob_DynInt2,
    ArrayGlob_DynClass1,
    ArrayGlob_DynRec1,
    ArrayGlob_DynRec2,

    ArrayGlob_DynDynInt1,
    ArrayGlob_DynDynClass1,
    ArrayGlob_DynDynRec1,

    ArrayGlob_DynStatInt1,
    ArrayGlob_DynStatClass1,
    ArrayGlob_DynStatRec1,

    ArrayGlob_StatInt1,
    ArrayGlob_StatClass1,
    ArrayGlob_StatRec1,

    ArrayGlob_StatStatInt1,
    ArrayGlob_StatStatClass1,
    ArrayGlob_StatStatRec1,

    ArrayGlob_StatDynInt1,
    ArrayGlob_StatDynClass1,
    ArrayGlob_StatDynRec1
  );
end;

end.

