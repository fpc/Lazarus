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

  TArrayStatStatInt   = array [-9..-5] of array [1..5] of Integer;
  TArrayStatStatClass = array [-9..-5] of array [1..5] of TArrayClass1;
  TArrayStatStatRec   = array [-9..-5] of array [1..5] of TArrayRec;

  TArrayStatDynInt    = array [-9..-5] of array of Integer;
  TArrayStatDynClass  = array [-9..-5] of array of TArrayClass1;
  TArrayStatDynRec    = array [-9..-5] of array of TArrayRec;


  { TArrayClass1 }

  TArrayClass1 = class
  public
    FieldInt1, FieldInt2: Integer;
    FieldDynAInt1: array of Integer;
    FieldStatAInt1: array [4..9] of Integer;

    FieldDynInt1: TArrayDynInt;
    FieldDynClass1: TArrayDynClass;
    FieldDynRec1: TArrayDynRec;

    FieldDynDynInt1: TArrayDynDynInt;
    FieldDynDynClass1: TArrayDynDynClass;
    FieldDynDynRec1: TArrayDynDynRec;

    FieldDynStatInt1: TArrayDynStatInt;
    FieldDynStatClass1: TArrayDynStatClass;
    FieldDynStatRec1: TArrayDynStatRec;

    FieldStatInt1: TArrayStatInt;
    FieldStatClass1: TArrayStatClass;
    FieldStatRec1: TArrayStatRec;

    FieldStatStatInt1: TArrayStatStatInt;
    FieldStatStatClass1: TArrayStatStatClass;
    FieldStatStatRec1: TArrayStatStatRec;

    FieldStatDynInt1: TArrayStatDynInt;
    FieldStatDynClass1: TArrayStatDynClass;
    FieldStatDynRec1: TArrayStatDynRec;

    procedure Test1Method;
  end;

var
  ArrayGlob_DynInt1, ArrayGlob_DynInt2: array of Integer;
  ArrayGlob_StatInt1: array [4..9] of Integer;
  ArrayGlob_StatInt2: array [-4..9] of Integer;

procedure Test1;

implementation

{ TArrayClass1 }

procedure TArrayClass1.Test1Method;
begin
  ArrayGlob_DynInt2 := nil;

  SetLength(ArrayGlob_DynInt1,20);
  ArrayGlob_DynInt1[0] := 5511;
  ArrayGlob_DynInt1[1] := 5512;
  ArrayGlob_DynInt1[2] := 5513;
  ArrayGlob_DynInt1[3] := 5514;
  ArrayGlob_DynInt1[4] := -5511;
  ArrayGlob_DynInt1[19] := 5500;

  ArrayGlob_StatInt1[4] := 6600;
  ArrayGlob_StatInt1[5] := 6601;
  ArrayGlob_StatInt1[6] := 6602;
  ArrayGlob_StatInt1[9] := 6699;

  ArrayGlob_StatInt2[-4] := 3300;
  ArrayGlob_StatInt2[-3] := 3301;
  ArrayGlob_StatInt2[-2] := 3302;
  ArrayGlob_StatInt2[-1] := 3303;
  ArrayGlob_StatInt2[0] := 3304;
  ArrayGlob_StatInt2[1] := 3305;


  SetLength(FieldDynInt1,   5);
    FieldDynInt1[0] := 100;
    FieldDynInt1[1] := 101;
    FieldDynInt1[2] := 102;

  SetLength(FieldDynClass1, 6);
    FieldDynClass1[0] := TArrayClass1.Create;
      FieldDynClass1[0].FieldInt1 := 98700;
      FieldDynClass1[0].FieldInt2 := 98701;
      SetLength(FieldDynClass1[0].FieldDynAInt1, 2);
        FieldDynClass1[0].FieldDynAInt1[0] := 9900;
        FieldDynClass1[0].FieldDynAInt1[1] := 9901;
    FieldDynClass1[1] := TArrayClass1.Create;
      FieldDynClass1[1].FieldInt1 := 88700;
      FieldDynClass1[1].FieldInt2 := 88701;
      SetLength(FieldDynClass1[1].FieldDynAInt1, 2);
        FieldDynClass1[1].FieldDynAInt1[0] := 8900;
        FieldDynClass1[1].FieldDynAInt1[1] := 8901;
    FieldDynClass1[2] := TArrayClass1.Create;
      FieldDynClass1[2].FieldInt1 := 78700;
      FieldDynClass1[2].FieldInt2 := 78701;
      SetLength(FieldDynClass1[2].FieldDynAInt1, 3);
        FieldDynClass1[2].FieldDynAInt1[0] := 7900;
        FieldDynClass1[2].FieldDynAInt1[1] := 7901;
        FieldDynClass1[2].FieldDynAInt1[2] := 7902;

  SetLength(FieldDynRec1,   7);
    FieldDynRec1[0].FieldInt1 := 200;
    FieldDynRec1[0].FieldInt2 := 201;
    FieldDynRec1[1].FieldInt1 := 210;
    FieldDynRec1[1].FieldInt2 := 211;
    FieldDynRec1[2].FieldInt1 := 220;
    FieldDynRec1[2].FieldInt2 := 221;

  SetLength(FieldDynDynInt1,   5,3);
    FieldDynDynInt1[0][0] := 1000;
    FieldDynDynInt1[0][1] := 1001;
    FieldDynDynInt1[0][2] := 1002;
    FieldDynDynInt1[0][3] := 1003;
    FieldDynDynInt1[1][0] := 1010;
    FieldDynDynInt1[1][1] := 1011;

  SetLength(FieldDynDynClass1, 5,4);
  SetLength(FieldDynDynRec1,   5,6);

  SetLength(FieldDynStatInt1,   3);
  SetLength(FieldDynStatClass1, 4);
  SetLength(FieldDynStatRec1,   5);

  //SetLength(FieldStatInt1, );
  //SetLength(FieldStatClass1, );
  //SetLength(FieldStatRec1, );

  //SetLength(FieldStatStatInt1, );
  //SetLength(FieldStatStatClass1, );
  //SetLength(FieldStatStatRec1, );

  SetLength(FieldStatDynInt1[-9],   3);
  SetLength(FieldStatDynClass1[-9], 3);
  SetLength(FieldStatDynRec1[-9],   3);



  ArrayGlob_DynInt1[9] := -5511; // BREAK
end;

procedure Test1;
var
  ArrayClass1: TArrayClass1;
begin
  ArrayClass1 := TArrayClass1.Create;
  ArrayClass1.Test1Method;
end;

end.

