unit TestWatchesUnitArray; // Array and Pointer
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
  TArrayDynRec2       = array of TArrayRec2;

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


  PArrayDynInt        = ^TArrayDynInt;
  PArrayDynClass      = ^TArrayDynClass;
  PArrayDynRec        = ^TArrayDynRec;
  PArrayDynRec2       = ^TArrayDynRec2;

  PArrayDynDynInt     = ^TArrayDynDynInt;
  PArrayDynDynClass   = ^TArrayDynDynClass;
  PArrayDynDynRec     = ^TArrayDynDynRec;

  PArrayDynStatInt    = ^TArrayDynStatInt;
  PArrayDynStatClass  = ^TArrayDynStatClass;
  PArrayDynStatRec    = ^TArrayDynStatRec;

  PArrayStatInt       = ^TArrayStatInt;
  PArrayStatClass     = ^TArrayStatClass;
  PArrayStatRec       = ^TArrayStatRec;

  PArrayStatStatInt   = ^TArrayStatStatInt;
  PArrayStatStatClass = ^TArrayStatStatClass;
  PArrayStatStatRec   = ^TArrayStatStatRec;

  PArrayStatDynInt    = ^TArrayStatDynInt;
  PArrayStatDynClass  = ^TArrayStatDynClass;
  PArrayStatDynRec    = ^TArrayStatDynRec;

  PInteger = ^Integer;
  PPInteger = ^PInteger;
  PWord = ^Word;
  PPWord = ^PWord;

  // TYPES fol param
  TArrayStatIntParam1 = array [4..9] of Integer;
  TArrayStatIntParam2 = array [-4..9] of Integer;

  PArrayStatIntParam1 = ^TArrayStatIntParam1;
  PArrayStatIntParam2 = ^TArrayStatIntParam2;
  PPointer            = ^Pointer;

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

    Field_IntPointer: PInteger;
    Field_WordPointer: PWord;
    Field_Pointer: Pointer;

    // Pointer
    Field_PDynAInt1: PArrayDynInt;
    Field_PStatAInt1: PArrayStatIntParam1;
    Field_PStatAInt2: PArrayStatIntParam2;

    Field_PDynInt1, Field_PDynInt2: PArrayDynInt;
    Field_PDynClass1: PArrayDynClass;
    Field_PDynRec1: PArrayDynRec;
    Field_PDynRec2: PArrayDynRec2;

    Field_PDynDynInt1: PArrayDynDynInt;
    Field_PDynDynClass1: PArrayDynDynClass;
    Field_PDynDynRec1: PArrayDynDynRec;

    Field_PDynStatInt1: PArrayDynStatInt;
    Field_PDynStatClass1: PArrayDynStatClass;
    Field_PDynStatRec1: PArrayDynStatRec;

    Field_PStatInt1: PArrayStatInt;
    Field_PStatClass1: PArrayStatClass;
    Field_PStatRec1: PArrayStatRec;

    Field_PStatStatInt1: PArrayStatStatInt;
    Field_PStatStatClass1: PArrayStatStatClass;
    Field_PStatStatRec1: PArrayStatStatRec;

    Field_PStatDynInt1: PArrayStatDynInt;
    Field_PStatDynClass1: PArrayStatDynClass;
    Field_PStatDynRec1: PArrayStatDynRec;

    Field_PIntPointer: PPInteger;
    Field_PWordPointer: PPWord;
    Field_PPointer: PPointer;


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

      Arg_IntPointer: PInteger;
      Arg_WordPointer: PWord;
      Arg_Pointer: Pointer;

      // Pointer
      Arg_PDynAInt1: PArrayDynInt;
      Arg_PStatAInt1: PArrayStatIntParam1;
      Arg_PStatAInt2: PArrayStatIntParam2;

      Arg_PDynInt1, Arg_PDynInt2: PArrayDynInt;
      Arg_PDynClass1: PArrayDynClass;
      Arg_PDynRec1: PArrayDynRec;
      Arg_PDynRec2: PArrayDynRec2;

      Arg_PDynDynInt1: PArrayDynDynInt;
      Arg_PDynDynClass1: PArrayDynDynClass;
      Arg_PDynDynRec1: PArrayDynDynRec;

      Arg_PDynStatInt1: PArrayDynStatInt;
      Arg_PDynStatClass1: PArrayDynStatClass;
      Arg_PDynStatRec1: PArrayDynStatRec;

      Arg_PStatInt1: PArrayStatInt;
      Arg_PStatClass1: PArrayStatClass;
      Arg_PStatRec1: PArrayStatRec;

      Arg_PStatStatInt1: PArrayStatStatInt;
      Arg_PStatStatClass1: PArrayStatStatClass;
      Arg_PStatStatRec1: PArrayStatStatRec;

      Arg_PStatDynInt1: PArrayStatDynInt;
      Arg_PStatDynClass1: PArrayStatDynClass;
      Arg_PStatDynRec1: PArrayStatDynRec;

      Arg_PIntPointer: PPInteger;
      Arg_PWordPointer: PPWord;
      Arg_PPointer: PPointer;


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
      var VArg_StatDynRec1: TArrayStatDynRec;

      var VArg_IntPointer: PInteger;
      var VArg_WordPointer: PWord;
      var VArg_Pointer: Pointer;

      // Pointer
      var VArg_PDynAInt1: PArrayDynInt;
      var VArg_PStatAInt1: PArrayStatIntParam1;
      var VArg_PStatAInt2: PArrayStatIntParam2;

      var VArg_PDynInt1, VArg_PDynInt2: PArrayDynInt;
      var VArg_PDynClass1: PArrayDynClass;
      var VArg_PDynRec1: PArrayDynRec;
      var VArg_PDynRec2: PArrayDynRec2;

      var VArg_PDynDynInt1: PArrayDynDynInt;
      var VArg_PDynDynClass1: PArrayDynDynClass;
      var VArg_PDynDynRec1: PArrayDynDynRec;

      var VArg_PDynStatInt1: PArrayDynStatInt;
      var VArg_PDynStatClass1: PArrayDynStatClass;
      var VArg_PDynStatRec1: PArrayDynStatRec;

      var VArg_PStatInt1: PArrayStatInt;
      var VArg_PStatClass1: PArrayStatClass;
      var VArg_PStatRec1: PArrayStatRec;

      var VArg_PStatStatInt1: PArrayStatStatInt;
      var VArg_PStatStatClass1: PArrayStatStatClass;
      var VArg_PStatStatRec1: PArrayStatStatRec;

      var VArg_PStatDynInt1: PArrayStatDynInt;
      var VArg_PStatDynClass1: PArrayStatDynClass;
      var VArg_PStatDynRec1: PArrayStatDynRec;

      var VArg_PIntPointer: PPInteger;
      var VArg_PWordPointer: PPWord;
      var VArg_PPointer: PPointer
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

  ArrayGlob_IntPointer: PInteger;
  ArrayGlob_WordPointer: PWord;
  ArrayGlob_Pointer: Pointer;

  // Pointer
  ArrayGlob_PDynAInt1: PArrayDynInt;
  ArrayGlob_PStatAInt1: PArrayStatIntParam1;
  ArrayGlob_PStatAInt2: PArrayStatIntParam2;

  ArrayGlob_PDynInt1, ArrayGlob_PDynInt2: PArrayDynInt;
  ArrayGlob_PDynClass1: PArrayDynClass;
  ArrayGlob_PDynRec1: PArrayDynRec;
  ArrayGlob_PDynRec2: PArrayDynRec2;

  ArrayGlob_PDynDynInt1: PArrayDynDynInt;
  ArrayGlob_PDynDynClass1: PArrayDynDynClass;
  ArrayGlob_PDynDynRec1: PArrayDynDynRec;

  ArrayGlob_PDynStatInt1: PArrayDynStatInt;
  ArrayGlob_PDynStatClass1: PArrayDynStatClass;
  ArrayGlob_PDynStatRec1: PArrayDynStatRec;

  ArrayGlob_PStatInt1: PArrayStatInt;
  ArrayGlob_PStatClass1: PArrayStatClass;
  ArrayGlob_PStatRec1: PArrayStatRec;

  ArrayGlob_PStatStatInt1: PArrayStatStatInt;
  ArrayGlob_PStatStatClass1: PArrayStatStatClass;
  ArrayGlob_PStatStatRec1: PArrayStatStatRec;

  ArrayGlob_PStatDynInt1: PArrayStatDynInt;
  ArrayGlob_PStatDynClass1: PArrayStatDynClass;
  ArrayGlob_PStatDynRec1: PArrayStatDynRec;

  ArrayGlob_PIntPointer: PPInteger;
  ArrayGlob_PWordPointer: PPWord;
  ArrayGlob_PPointer: PPointer;

  // copy of Pointers, to be used as Var Param
  ArrayGlob_P2DynAInt1: PArrayDynInt;
  ArrayGlob_P2StatAInt1: PArrayStatIntParam1;
  ArrayGlob_P2StatAInt2: PArrayStatIntParam2;

  ArrayGlob_P2DynInt1, ArrayGlob_P2DynInt2: PArrayDynInt;
  ArrayGlob_P2DynClass1: PArrayDynClass;
  ArrayGlob_P2DynRec1: PArrayDynRec;
  ArrayGlob_P2DynRec2: PArrayDynRec2;

  ArrayGlob_P2DynDynInt1: PArrayDynDynInt;
  ArrayGlob_P2DynDynClass1: PArrayDynDynClass;
  ArrayGlob_P2DynDynRec1: PArrayDynDynRec;

  ArrayGlob_P2DynStatInt1: PArrayDynStatInt;
  ArrayGlob_P2DynStatClass1: PArrayDynStatClass;
  ArrayGlob_P2DynStatRec1: PArrayDynStatRec;

  ArrayGlob_P2StatInt1: PArrayStatInt;
  ArrayGlob_P2StatClass1: PArrayStatClass;
  ArrayGlob_P2StatRec1: PArrayStatRec;

  ArrayGlob_P2StatStatInt1: PArrayStatStatInt;
  ArrayGlob_P2StatStatClass1: PArrayStatStatClass;
  ArrayGlob_P2StatStatRec1: PArrayStatStatRec;

  ArrayGlob_P2StatDynInt1: PArrayStatDynInt;
  ArrayGlob_P2StatDynClass1: PArrayStatDynClass;
  ArrayGlob_P2StatDynRec1: PArrayStatDynRec;

  ArrayGlob_P2IntPointer: PPInteger;
  ArrayGlob_P2WordPointer: PPWord;
  ArrayGlob_P2Pointer: PPointer;

  // dummy, ensure "pointer" is in debug info
  ArrayGlob_DummyPointer: Pointer;
  ArrayGlob_DummyPInteger: PInteger;

  ArrayAdd0, ArrayAdd1, ArrayAdd2, ArraySub1, ArraySub2 : Int64;

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
  Arg_IntPointer: PInteger; Arg_WordPointer: PWord; Arg_Pointer: Pointer;
  Arg_PDynAInt1: PArrayDynInt; Arg_PStatAInt1: PArrayStatIntParam1;
  Arg_PStatAInt2: PArrayStatIntParam2; Arg_PDynInt1, Arg_PDynInt2: PArrayDynInt;
  Arg_PDynClass1: PArrayDynClass; Arg_PDynRec1: PArrayDynRec; Arg_PDynRec2: PArrayDynRec2;
  Arg_PDynDynInt1: PArrayDynDynInt; Arg_PDynDynClass1: PArrayDynDynClass;
  Arg_PDynDynRec1: PArrayDynDynRec; Arg_PDynStatInt1: PArrayDynStatInt;
  Arg_PDynStatClass1: PArrayDynStatClass; Arg_PDynStatRec1: PArrayDynStatRec;
  Arg_PStatInt1: PArrayStatInt; Arg_PStatClass1: PArrayStatClass;
  Arg_PStatRec1: PArrayStatRec; Arg_PStatStatInt1: PArrayStatStatInt;
  Arg_PStatStatClass1: PArrayStatStatClass; Arg_PStatStatRec1: PArrayStatStatRec;
  Arg_PStatDynInt1: PArrayStatDynInt; Arg_PStatDynClass1: PArrayStatDynClass;
  Arg_PStatDynRec1: PArrayStatDynRec; Arg_PIntPointer: PPInteger; Arg_PWordPointer: PPWord;
  Arg_PPointer: PPointer; var VArg_DynAInt1: array of Integer;
  var VArg_StatAInt1: TArrayStatIntParam1; var VArg_StatAInt2: TArrayStatIntParam2;
  var VArg_DynInt1, VArg_DynInt2: TArrayDynInt; var VArg_DynClass1: TArrayDynClass;
  var VArg_DynRec1: TArrayDynRec; var VArg_DynRec2: array of TArrayRec2;
  var VArg_DynDynInt1: TArrayDynDynInt; var VArg_DynDynClass1: TArrayDynDynClass;
  var VArg_DynDynRec1: TArrayDynDynRec; var VArg_DynStatInt1: TArrayDynStatInt;
  var VArg_DynStatClass1: TArrayDynStatClass; var VArg_DynStatRec1: TArrayDynStatRec;
  var VArg_StatInt1: TArrayStatInt; var VArg_StatClass1: TArrayStatClass;
  var VArg_StatRec1: TArrayStatRec; var VArg_StatStatInt1: TArrayStatStatInt;
  var VArg_StatStatClass1: TArrayStatStatClass; var VArg_StatStatRec1: TArrayStatStatRec;
  var VArg_StatDynInt1: TArrayStatDynInt; var VArg_StatDynClass1: TArrayStatDynClass;
  var VArg_StatDynRec1: TArrayStatDynRec; var VArg_IntPointer: PInteger;
  var VArg_WordPointer: PWord; var VArg_Pointer: Pointer; var VArg_PDynAInt1: PArrayDynInt;
  var VArg_PStatAInt1: PArrayStatIntParam1; var VArg_PStatAInt2: PArrayStatIntParam2;
  var VArg_PDynInt1, VArg_PDynInt2: PArrayDynInt; var VArg_PDynClass1: PArrayDynClass;
  var VArg_PDynRec1: PArrayDynRec; var VArg_PDynRec2: PArrayDynRec2;
  var VArg_PDynDynInt1: PArrayDynDynInt; var VArg_PDynDynClass1: PArrayDynDynClass;
  var VArg_PDynDynRec1: PArrayDynDynRec; var VArg_PDynStatInt1: PArrayDynStatInt;
  var VArg_PDynStatClass1: PArrayDynStatClass; var VArg_PDynStatRec1: PArrayDynStatRec;
  var VArg_PStatInt1: PArrayStatInt; var VArg_PStatClass1: PArrayStatClass;
  var VArg_PStatRec1: PArrayStatRec; var VArg_PStatStatInt1: PArrayStatStatInt;
  var VArg_PStatStatClass1: PArrayStatStatClass; var VArg_PStatStatRec1: PArrayStatStatRec;
  var VArg_PStatDynInt1: PArrayStatDynInt; var VArg_PStatDynClass1: PArrayStatDynClass;
  var VArg_PStatDynRec1: PArrayStatDynRec; var VArg_PIntPointer: PPInteger;
  var VArg_PWordPointer: PPWord; var VArg_PPointer: PPointer);
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

  Local_IntPointer: PInteger;
  Local_WordPointer: PWord;
  Local_Pointer: Pointer;

  // Pointer
  Local_PDynAInt1: PArrayDynInt;
  Local_PStatAInt1: PArrayStatIntParam1;
  Local_PStatAInt2: PArrayStatIntParam2;

  Local_PDynInt1, Local_PDynInt2: PArrayDynInt;
  Local_PDynClass1: PArrayDynClass;
  Local_PDynRec1: PArrayDynRec;
  Local_PDynRec2: PArrayDynRec2;

  Local_PDynDynInt1: PArrayDynDynInt;
  Local_PDynDynClass1: PArrayDynDynClass;
  Local_PDynDynRec1: PArrayDynDynRec;

  Local_PDynStatInt1: PArrayDynStatInt;
  Local_PDynStatClass1: PArrayDynStatClass;
  Local_PDynStatRec1: PArrayDynStatRec;

  Local_PStatInt1: PArrayStatInt;
  Local_PStatClass1: PArrayStatClass;
  Local_PStatRec1: PArrayStatRec;

  Local_PStatStatInt1: PArrayStatStatInt;
  Local_PStatStatClass1: PArrayStatStatClass;
  Local_PStatStatRec1: PArrayStatStatRec;

  Local_PStatDynInt1: PArrayStatDynInt;
  Local_PStatDynClass1: PArrayStatDynClass;
  Local_PStatDynRec1: PArrayStatDynRec;

  Local_PIntPointer: PPInteger;
  Local_PWordPointer: PPWord;
  Local_PPointer: PPointer;

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

  Field_IntPointer := @Field_DynInt1[2];
  Field_WordPointer := @Field_DynInt1[2];
  Field_Pointer := @Field_DynInt1[2];

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
  SetLength(Field_DynDynInt1[0],  2);
    Field_DynDynInt1[0][0] := 1000;
    Field_DynDynInt1[0][1] := 1001;
    //Field_DynDynInt1[0][2] := 1002;
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

  // Pointer
  Field_PDynAInt1        := @Field_DynAInt1;
  Field_PStatAInt1       := @Field_StatAInt1;
  Field_PStatAInt2       := @Field_StatAInt2;

  Field_PDynInt1         := @Field_DynInt1;
  Field_PDynInt2         := @Field_DynInt2;
  Field_PDynClass1       := @Field_DynClass1;
  Field_PDynRec1         := @Field_DynRec1;
  Field_PDynRec2         := @Field_DynRec2;

  Field_PDynDynInt1      := @Field_DynDynInt1;
  Field_PDynDynClass1    := @Field_DynDynClass1;
  Field_PDynDynRec1      := @Field_DynDynRec1;

  Field_PDynStatInt1     := @Field_DynStatInt1;
  Field_PDynStatClass1   := @Field_DynStatClass1;
  Field_PDynStatRec1     := @Field_DynStatRec1;

  Field_PStatInt1        := @Field_StatInt1;
  Field_PStatClass1      := @Field_StatClass1;
  Field_PStatRec1        := @Field_StatRec1;

  Field_PStatStatInt1    := @Field_StatStatInt1;
  Field_PStatStatClass1  := @Field_StatStatClass1;
  Field_PStatStatRec1    := @Field_StatStatRec1;

  Field_PStatDynInt1     := @Field_StatDynInt1;
  Field_PStatDynClass1   := @Field_StatDynClass1;
  Field_PStatDynRec1     := @Field_StatDynRec1;

  Field_PIntPointer := @Field_IntPointer;
  Field_PWordPointer := @Field_WordPointer;
  Field_PPointer := @Field_Pointer;

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

  Local_IntPointer := @Local_DynInt1[2];
  Local_WordPointer := @Local_DynInt1[2];
  Local_Pointer := @Local_DynInt1[2];

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
  SetLength(Local_DynDynInt1[0],  2);
    Local_DynDynInt1[0][0] := 1000;
    Local_DynDynInt1[0][1] := 1001;
    //Local_DynDynInt1[0][2] := 1002;
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

  // Pointer
  Local_PDynAInt1        := @Local_DynAInt1;
  Local_PStatAInt1       := @Local_StatAInt1;
  Local_PStatAInt2       := @Local_StatAInt2;

  Local_PDynInt1         := @Local_DynInt1;
  Local_PDynInt2         := @Local_DynInt2;
  Local_PDynClass1       := @Local_DynClass1;
  Local_PDynRec1         := @Local_DynRec1;
  Local_PDynRec2         := @Local_DynRec2;

  Local_PDynDynInt1      := @Local_DynDynInt1;
  Local_PDynDynClass1    := @Local_DynDynClass1;
  Local_PDynDynRec1      := @Local_DynDynRec1;

  Local_PDynStatInt1     := @Local_DynStatInt1;
  Local_PDynStatClass1   := @Local_DynStatClass1;
  Local_PDynStatRec1     := @Local_DynStatRec1;

  Local_PStatInt1        := @Local_StatInt1;
  Local_PStatClass1      := @Local_StatClass1;
  Local_PStatRec1        := @Local_StatRec1;

  Local_PStatStatInt1    := @Local_StatStatInt1;
  Local_PStatStatClass1  := @Local_StatStatClass1;
  Local_PStatStatRec1    := @Local_StatStatRec1;

  Local_PStatDynInt1     := @Local_StatDynInt1;
  Local_PStatDynClass1   := @Local_StatDynClass1;
  Local_PStatDynRec1     := @Local_StatDynRec1;

  Local_PIntPointer := @Local_IntPointer;
  Local_PWordPointer := @Local_WordPointer;
  Local_PPointer := @Local_Pointer;

  {%endregion  Local }


  {%region  ARG }
  // Pointer
  Arg_PDynAInt1        := @Arg_DynAInt1;
  Arg_PStatAInt1       := @Arg_StatAInt1;
  Arg_PStatAInt2       := @Arg_StatAInt2;

  Arg_PDynInt1         := @Arg_DynInt1;
  Arg_PDynInt2         := @Arg_DynInt2;
  Arg_PDynClass1       := @Arg_DynClass1;
  Arg_PDynRec1         := @Arg_DynRec1;
  Arg_PDynRec2         := @Arg_DynRec2;

  Arg_PDynDynInt1      := @Arg_DynDynInt1;
  Arg_PDynDynClass1    := @Arg_DynDynClass1;
  Arg_PDynDynRec1      := @Arg_DynDynRec1;

  Arg_PDynStatInt1     := @Arg_DynStatInt1;
  Arg_PDynStatClass1   := @Arg_DynStatClass1;
  Arg_PDynStatRec1     := @Arg_DynStatRec1;

  Arg_PStatInt1        := @Arg_StatInt1;
  Arg_PStatClass1      := @Arg_StatClass1;
  Arg_PStatRec1        := @Arg_StatRec1;

  Arg_PStatStatInt1    := @Arg_StatStatInt1;
  Arg_PStatStatClass1  := @Arg_StatStatClass1;
  Arg_PStatStatRec1    := @Arg_StatStatRec1;

  Arg_PStatDynInt1     := @Arg_StatDynInt1;
  Arg_PStatDynClass1   := @Arg_StatDynClass1;
  Arg_PStatDynRec1     := @Arg_StatDynRec1;

  Arg_PIntPointer := @Arg_IntPointer;
  Arg_PWordPointer := @Arg_WordPointer;
  Arg_PPointer := @Arg_Pointer;
  {%endregion  ARG}

  {%region  ARG }
  // Pointer
  VArg_PDynAInt1        := @VArg_DynAInt1;
  VArg_PStatAInt1       := @VArg_StatAInt1;
  VArg_PStatAInt2       := @VArg_StatAInt2;

  VArg_PDynInt1         := @VArg_DynInt1;
  VArg_PDynInt2         := @VArg_DynInt2;
  VArg_PDynClass1       := @VArg_DynClass1;
  VArg_PDynRec1         := @VArg_DynRec1;
  VArg_PDynRec2         := @VArg_DynRec2;

  VArg_PDynDynInt1      := @VArg_DynDynInt1;
  VArg_PDynDynClass1    := @VArg_DynDynClass1;
  VArg_PDynDynRec1      := @VArg_DynDynRec1;

  VArg_PDynStatInt1     := @VArg_DynStatInt1;
  VArg_PDynStatClass1   := @VArg_DynStatClass1;
  VArg_PDynStatRec1     := @VArg_DynStatRec1;

  VArg_PStatInt1        := @VArg_StatInt1;
  VArg_PStatClass1      := @VArg_StatClass1;
  VArg_PStatRec1        := @VArg_StatRec1;

  VArg_PStatStatInt1    := @VArg_StatStatInt1;
  VArg_PStatStatClass1  := @VArg_StatStatClass1;
  VArg_PStatStatRec1    := @VArg_StatStatRec1;

  VArg_PStatDynInt1     := @VArg_StatDynInt1;
  VArg_PStatDynClass1   := @VArg_StatDynClass1;
  VArg_PStatDynRec1     := @VArg_StatDynRec1;

  VArg_PIntPointer := @VArg_IntPointer;
  VArg_PWordPointer := @VArg_WordPointer;
  VArg_PPointer := @VArg_Pointer;
  {%endregion  ARG}

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

  ArrayGlob_IntPointer := @ArrayGlob_DynInt1[2];
  ArrayGlob_WordPointer := @ArrayGlob_DynInt1[2];
  ArrayGlob_Pointer := @ArrayGlob_DynInt1[2];

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
  SetLength(ArrayGlob_DynDynInt1[0],  2);
    ArrayGlob_DynDynInt1[0][0] := 1000;
    ArrayGlob_DynDynInt1[0][1] := 1001;
    //ArrayGlob_DynDynInt1[0][2] := 1002;
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

  // Pointer
  ArrayGlob_PDynAInt1        := @ArrayGlob_DynAInt1;
  ArrayGlob_PStatAInt1       := @ArrayGlob_StatAInt1;
  ArrayGlob_PStatAInt2       := @ArrayGlob_StatAInt2;

  ArrayGlob_PDynInt1         := @ArrayGlob_DynInt1;
  ArrayGlob_PDynInt2         := @ArrayGlob_DynInt2;
  ArrayGlob_PDynClass1       := @ArrayGlob_DynClass1;
  ArrayGlob_PDynRec1         := @ArrayGlob_DynRec1;
  ArrayGlob_PDynRec2         := @ArrayGlob_DynRec2;

  ArrayGlob_PDynDynInt1      := @ArrayGlob_DynDynInt1;
  ArrayGlob_PDynDynClass1    := @ArrayGlob_DynDynClass1;
  ArrayGlob_PDynDynRec1      := @ArrayGlob_DynDynRec1;

  ArrayGlob_PDynStatInt1     := @ArrayGlob_DynStatInt1;
  ArrayGlob_PDynStatClass1   := @ArrayGlob_DynStatClass1;
  ArrayGlob_PDynStatRec1     := @ArrayGlob_DynStatRec1;

  ArrayGlob_PStatInt1        := @ArrayGlob_StatInt1;
  ArrayGlob_PStatClass1      := @ArrayGlob_StatClass1;
  ArrayGlob_PStatRec1        := @ArrayGlob_StatRec1;

  ArrayGlob_PStatStatInt1    := @ArrayGlob_StatStatInt1;
  ArrayGlob_PStatStatClass1  := @ArrayGlob_StatStatClass1;
  ArrayGlob_PStatStatRec1    := @ArrayGlob_StatStatRec1;

  ArrayGlob_PStatDynInt1     := @ArrayGlob_StatDynInt1;
  ArrayGlob_PStatDynClass1   := @ArrayGlob_StatDynClass1;
  ArrayGlob_PStatDynRec1     := @ArrayGlob_StatDynRec1;

  ArrayGlob_PIntPointer := @ArrayGlob_IntPointer;
  ArrayGlob_PWordPointer := @ArrayGlob_WordPointer;
  ArrayGlob_PPointer := @ArrayGlob_Pointer;

  {%endregion  ArrayGlob }

  // copy for var param
  ArrayGlob_P2DynAInt1       := ArrayGlob_PDynAInt1;
  ArrayGlob_P2StatAInt1      := ArrayGlob_PStatAInt1;
  ArrayGlob_P2StatAInt2      := ArrayGlob_PStatAInt2;

  ArrayGlob_P2DynInt1        := ArrayGlob_PDynInt1;
  ArrayGlob_P2DynInt2        := ArrayGlob_PDynInt2;
  ArrayGlob_P2DynClass1      := ArrayGlob_PDynClass1;
  ArrayGlob_P2DynRec1        := ArrayGlob_PDynRec1;
  ArrayGlob_P2DynRec2        := ArrayGlob_PDynRec2;

  ArrayGlob_P2DynDynInt1     := ArrayGlob_PDynDynInt1;
  ArrayGlob_P2DynDynClass1   := ArrayGlob_PDynDynClass1;
  ArrayGlob_P2DynDynRec1     := ArrayGlob_PDynDynRec1;

  ArrayGlob_P2DynStatInt1    := ArrayGlob_PDynStatInt1;
  ArrayGlob_P2DynStatClass1  := ArrayGlob_PDynStatClass1;
  ArrayGlob_P2DynStatRec1    := ArrayGlob_PDynStatRec1;

  ArrayGlob_P2StatInt1       := ArrayGlob_PStatInt1;
  ArrayGlob_P2StatClass1     := ArrayGlob_PStatClass1;
  ArrayGlob_P2StatRec1       := ArrayGlob_PStatRec1;

  ArrayGlob_P2StatStatInt1   := ArrayGlob_PStatStatInt1;
  ArrayGlob_P2StatStatClass1 := ArrayGlob_PStatStatClass1;
  ArrayGlob_P2StatStatRec1   := ArrayGlob_PStatStatRec1;

  ArrayGlob_P2StatDynInt1    := ArrayGlob_PStatDynInt1;
  ArrayGlob_P2StatDynClass1  := ArrayGlob_PStatDynClass1;
  ArrayGlob_P2StatDynRec1    := ArrayGlob_PStatDynRec1;

  ArrayGlob_P2IntPointer := ArrayGlob_PIntPointer;
  ArrayGlob_P2WordPointer := ArrayGlob_PWordPointer;
  ArrayGlob_P2Pointer := ArrayGlob_PPointer;


  ArrayAdd0 := 0;
  ArrayAdd1 := 1;
  ArrayAdd2 := 2;
  ArraySub1 := -1;
  ArraySub2 := -2;

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

    ArrayGlob_IntPointer,
    ArrayGlob_WordPointer,
    ArrayGlob_Pointer,

    // Pointer
    ArrayGlob_PDynAInt1,
    ArrayGlob_PStatAInt1,
    ArrayGlob_PStatAInt2,

    ArrayGlob_PDynInt1,
    ArrayGlob_PDynInt2,
    ArrayGlob_PDynClass1,
    ArrayGlob_PDynRec1,
    ArrayGlob_PDynRec2,

    ArrayGlob_PDynDynInt1,
    ArrayGlob_PDynDynClass1,
    ArrayGlob_PDynDynRec1,

    ArrayGlob_PDynStatInt1,
    ArrayGlob_PDynStatClass1,
    ArrayGlob_PDynStatRec1,

    ArrayGlob_PStatInt1,
    ArrayGlob_PStatClass1,
    ArrayGlob_PStatRec1,

    ArrayGlob_PStatStatInt1,
    ArrayGlob_PStatStatClass1,
    ArrayGlob_PStatStatRec1,

    ArrayGlob_PStatDynInt1,
    ArrayGlob_PStatDynClass1,
    ArrayGlob_PStatDynRec1,

    ArrayGlob_PIntPointer,
    ArrayGlob_PWordPointer,
    ArrayGlob_PPointer,

    // VAR
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

    ArrayGlob_IntPointer,
    ArrayGlob_WordPointer,
    ArrayGlob_Pointer,

    // Pointer
    ArrayGlob_P2DynAInt1,
    ArrayGlob_P2StatAInt1,
    ArrayGlob_P2StatAInt2,

    ArrayGlob_P2DynInt1,
    ArrayGlob_P2DynInt2,
    ArrayGlob_P2DynClass1,
    ArrayGlob_P2DynRec1,
    ArrayGlob_P2DynRec2,

    ArrayGlob_P2DynDynInt1,
    ArrayGlob_P2DynDynClass1,
    ArrayGlob_P2DynDynRec1,

    ArrayGlob_P2DynStatInt1,
    ArrayGlob_P2DynStatClass1,
    ArrayGlob_P2DynStatRec1,

    ArrayGlob_P2StatInt1,
    ArrayGlob_P2StatClass1,
    ArrayGlob_P2StatRec1,

    ArrayGlob_P2StatStatInt1,
    ArrayGlob_P2StatStatClass1,
    ArrayGlob_P2StatStatRec1,

    ArrayGlob_P2StatDynInt1,
    ArrayGlob_P2StatDynClass1,
    ArrayGlob_P2StatDynRec1,

    ArrayGlob_P2IntPointer,
    ArrayGlob_P2WordPointer,
    ArrayGlob_P2Pointer

  );
end;

end.

