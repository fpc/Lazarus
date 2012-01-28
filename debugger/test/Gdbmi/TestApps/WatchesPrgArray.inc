(* Struture // NEW structure
  program WatchesPrg;

  type
   {$DEFINE Global_Type}

  {$DEFINE Global_Implementation}

  procedure FooFuncArray(  {$DEFINE FooFunc_Param}  }
      {$DEFINE FooFunc_Local}  // type / var
    begin
      {$DEFINE FooFunc_Body}
    end;

  var
    {$DEFINE Global_Var}

  begin
    {$DEFINE Global_Body}
    FooFunc(   {$DEFINE Global_Call_FooFunc}   );
    {$DEFINE Global_Body_NIL}
    FooFunc(   {$DEFINE Global_Call_FooFunc}   );
  end;

*)

  {%region  TYPE}
    {$IFDEF Global_Type}
    //type
      TObjectInArray = class public foo: Integer; end;
      TRecordInArray = record foo: Integer; end;

      TDynIntArray = Array of Integer;
      PDynIntArray = ^TDynIntArray;
      TStatIntArray = Array [5..9] of Integer;
      PStatIntArray = ^TStatIntArray;

      TDynObjArray = Array of TObjectInArray;
      PDynObjArray = ^TDynIntArray;
      TStatObjArray = Array [5..9] of TObjectInArray;
      PStatObjArray = ^TStatIntArray;

      TDynRecArray = Array of TRecordInArray;
      PDynRecArray = ^TDynIntArray;
      TStatRecArray = Array [5..9] of TRecordInArray;
      PStatRecArray = ^TStatIntArray;

      TDynIntArray2 = Array of TDynIntArray;


    // dyn array with record
      TRecForArray1 = record  // has NO named pointer to it
        a : integer;
        b : integer;
      end;
      TRecForArray3 = record  // has NO named pointer to it
        a : integer;
        b : integer;
      end;

      TDArrayTRec1 = array of TRecForArray1;

      TRecForArray2 = record  // has named pointer to it
        c : integer;
        recs : TDArrayTRec1;
      end;
      PRecForArray2 = ^TRecForArray2;
      TRecForArray4 = record  // has named pointer to it
        c : integer;
        recs : TDArrayTRec1;
      end;
      PRecForArray4 = ^TRecForArray4;

      // dyn arrays
      TDynArrayTRec1      = array of TRecForArray3;
        TDynArrayTRec1NP  = array of TRecForArray3; // for use as name-less pointer to this
      TDynArrayPRec1      = array of ^TRecForArray3;
      TDynDynArrayTRec1   = array of array of TRecForArray1;
      TDynDynArrayPRec1   = array of array of ^TRecForArray1;
      TDynStatArrayTRec1  = array of array [3..5] of TRecForArray1;
      TDynStatArrayPRec1  = array of array [3..5] of ^TRecForArray1;

      TDynArrayTRec2      = array of TRecForArray4;
      TDynArrayPRec2      = array of ^TRecForArray4;
      TDynArrayPPRec2     = array of ^PRecForArray4; // double pointer
      TDynDynArrayTRec2   = array of array of TRecForArray2;
      TDynDynArrayPRec2   = array of array of ^TRecForArray2;
      TDynStatArrayTRec2  = array of array [3..5] of TRecForArray2;
      TDynStatArrayPRec2  = array of array [3..5] of ^TRecForArray2;


      // stat arrays
      TStatArrayTRec1     = array [3..5] of TRecForArray3;
        TStatArrayTRec1NP = array [3..5] of TRecForArray3; // for use as name-less pointer to this
      TStatArrayPRec1     = array [3..5] of ^TRecForArray3;
      TStatDynArrayTRec1  = array [3..5] of array of TRecForArray1;
      TStatDynArrayPRec1  = array [3..5] of array of ^TRecForArray1;
      TStatStatArrayTRec1 = array [3..5] of array [3..5] of TRecForArray1;
      TStatStatArrayPRec1 = array [3..5] of array [3..5] of ^TRecForArray1;

      TStatArrayTRec2     = array [3..5] of TRecForArray4;
      TStatArrayPRec2     = array [3..5] of ^TRecForArray4;
      TStatArrayPPRec2    = array [3..5] of ^PRecForArray4; // double pointer
      TStatDynArrayTRec2  = array [3..5] of array of TRecForArray2;
      TStatDynArrayPRec2  = array [3..5] of array of ^TRecForArray2;
      TStatStatArrayTRec2 = array [3..5] of array [3..5] of TRecForArray2;
      TStatStatArrayPRec2 = array [3..5] of array [3..5] of ^TRecForArray2;


      // dyn arrays of named arrays
      TDynDynTRec1Array   = array of TDynArrayTRec1;
      TDynDynPRec1Array   = array of TDynArrayPRec1;
      TDynStatTRec1Array  = array of TStatArrayTRec1;
      TDynStatPRec1Array  = array of TStatArrayPRec1;
      TDynPDynTRec1Array   = array of ^TDynArrayTRec1;
      TDynPStatTRec1Array  = array of ^TStatArrayTRec1;
      TDynPDynTRec1NPArray = array of ^TDynArrayTRec1NP;
      TDynPStatTRec1NPArray= array of ^TStatArrayTRec1NP;

      TDynDynTRec2Array   = array of TDynArrayTRec2;
      TDynDynPrec2Array   = array of TDynArrayPRec2;
      TDynDynPPrec2Array  = array of TDynArrayPPRec2; // double pointer
      TDynStatTRec2Array  = array of TStatArrayTRec2;
      TDynStatPRec2Array  = array of TStatArrayPRec2;
      TDynStatPPRec2Array = array of TStatArrayPPRec2; // double pointer

      // stat arrays of named arrays
      TStatDynTRec1Array   = array [3..5] of TDynArrayTRec1;
      TStatDynPrec1Array   = array [3..5] of TDynArrayPRec1;
      TStatStatTRec1Array  = array [3..5] of TStatArrayTRec1;
      TStatStatPRec1Array  = array [3..5] of TStatArrayPRec1;
      TStatPDynTRec1Array   = array [3..5] of ^TDynArrayTRec1;// TODO name-less pointer to an array [3..5]
      TStatPStatTRec1Array  = array [3..5] of ^TStatArrayTRec1;

      TStatDynTRec2Array   = array [3..5] of TDynArrayTRec2;
      TStatDynPrec2Array   = array [3..5] of TDynArrayPRec2;
      TStatDynPPrec2Array  = array [3..5] of TDynArrayPPRec2; // double pointer
      TStatStatTRec2Array  = array [3..5] of TStatArrayTRec2;
      TStatStatPRec2Array  = array [3..5] of TStatArrayPRec2;
      TStatStatPPRec2Array = array [3..5] of TStatArrayPPRec2; // double pointer


// POINTER

      // dyn arrays
      PDynArrayTRec1      = ^TDynArrayTRec1;
      PDynArrayPRec1      = ^TDynArrayPRec1;
      PDynDynArrayTRec1   = ^TDynDynArrayTRec1;
      PDynDynArrayPRec1   = ^TDynDynArrayPRec1;
      PDynStatArrayTRec1  = ^TDynStatArrayTRec1;
      PDynStatArrayPRec1  = ^TDynStatArrayPRec1;

      PDynArrayTRec2      = ^TDynArrayTRec2;
      PDynArrayPRec2      = ^TDynArrayPRec2;
      PDynArrayPPRec2     = ^TDynArrayPPRec2;
      PDynDynArrayTRec2   = ^TDynDynArrayTRec2;
      PDynDynArrayPRec2   = ^TDynDynArrayPRec2;
      PDynStatArrayTRec2  = ^TDynStatArrayTRec2;
      PDynStatArrayPRec2  = ^TDynStatArrayPRec2;


      // stat arrays
      PStatArrayTRec1     = ^TStatArrayTRec1;
      PStatArrayPRec1     = ^TStatArrayPRec1;
      PStatDynArrayTRec1  = ^TStatDynArrayTRec1;
      PStatDynArrayPRec1  = ^TStatDynArrayPRec1;
      PStatStatArrayTRec1 = ^TStatStatArrayTRec1;
      PStatStatArrayPRec1 = ^TStatStatArrayPRec1;

      PStatArrayTRec2     = ^TStatArrayTRec2;
      PStatArrayPRec2     = ^TStatArrayPRec2;
      PStatArrayPPRec2    = ^TStatArrayPPRec2;
      PStatDynArrayTRec2  = ^TStatDynArrayTRec2;
      PStatDynArrayPRec2  = ^TStatDynArrayPRec2;
      PStatStatArrayTRec2 = ^TStatStatArrayTRec2;
      PStatStatArrayPRec2 = ^TStatStatArrayPRec2;


      // dyn arrays of named arrays
      PDynDynTRec1Array   = ^TDynDynTRec1Array;
      PDynDynPrec1Array   = ^TDynDynPrec1Array;
      PDynStatTRec1Array  = ^TDynStatTRec1Array;
      PDynStatPRec1Array  = ^TDynStatPRec1Array;
      PDynPDynTRec1Array  = ^TDynPDynTRec1Array;
      PDynPStatTRec1Array = ^TDynPStatTRec1Array;
      PDynPDynTRec1NPArray = ^TDynPDynTRec1NPArray;
      PDynPStatTRec1NPArray= ^TDynPStatTRec1NPArray;

      PDynDynTRec2Array   = ^TDynDynTRec2Array;
      PDynDynPrec2Array   = ^TDynDynPrec2Array;
      PDynDynPPrec2Array  = ^TDynDynPPrec2Array;
      PDynStatTRec2Array  = ^TDynStatTRec2Array;
      PDynStatPRec2Array  = ^TDynStatPRec2Array;
      PDynStatPPRec2Array = ^TDynStatPPRec2Array;

      // stat arrays of named arrays
      PStatDynTRec1Array   = ^TStatDynTRec1Array;
      PStatDynPrec1Array   = ^TStatDynPrec1Array;
      PStatStatTRec1Array  = ^TStatStatTRec1Array;
      PStatStatPRec1Array  = ^TStatStatPRec1Array;
      PStatPDynTRec1Array  = ^TStatPDynTRec1Array;
      PStatPStatTRec1Array = ^TStatPStatTRec1Array;

      PStatDynTRec2Array   = ^TStatDynTRec2Array;
      PStatDynPrec2Array   = ^TStatDynPrec2Array;
      PStatDynPPrec2Array  = ^TStatDynPPrec2Array;
      PStatStatTRec2Array  = ^TStatStatTRec2Array;
      PStatStatPRec2Array  = ^TStatStatPRec2Array;
      PStatStatPPRec2Array = ^TStatStatPPRec2Array;

      TEnunForArray = (eaOne, eaTwo, eaThree);
      TEnumArrayTRec1     = array [TEnunForArray] of TRecForArray3;
      TSubEnumArrayTRec1  = array [eaTwo..eaThree] of TRecForArray3;
      PEnumArrayTRec1     = ^TEnumArrayTRec1;
      PSubEnumArrayTRec1  = ^TSubEnumArrayTRec1;

      TStatDualArrayTRec2 = array [3..5, 3..5] of TRecForArray2;


    {$ENDIF}
  {%endregion}

  {%region VARIABLES}
    {$IFDEF FooFunc_Param}
    //procedure FooFunc(

      // dyn arrays
      ArgTDynArrayTRec1: TDynArrayTRec1;            var VArgTDynArrayTRec1: TDynArrayTRec1;
      ArgTDynArrayPRec1: TDynArrayPRec1;            var VArgTDynArrayPRec1: TDynArrayPRec1;
      ArgTDynDynArrayTRec1: TDynDynArrayTRec1;      var VArgTDynDynArrayTRec1: TDynDynArrayTRec1;
      ArgTDynDynArrayPRec1: TDynDynArrayPRec1;      var VArgTDynDynArrayPRec1: TDynDynArrayPRec1;
      ArgTDynStatArrayTRec1: TDynStatArrayTRec1;    var VArgTDynStatArrayTRec1: TDynStatArrayTRec1;
      ArgTDynStatArrayPRec1: TDynStatArrayPRec1;    var VArgTDynStatArrayPRec1: TDynStatArrayPRec1;

      ArgTDynArrayTRec2: TDynArrayTRec2;            var VArgTDynArrayTRec2: TDynArrayTRec2;
      ArgTDynArrayPRec2: TDynArrayPRec2;            var VArgTDynArrayPRec2: TDynArrayPRec2;
      ArgTDynArrayPPRec2: TDynArrayPPRec2;          var VArgTDynArrayPPRec2: TDynArrayPPRec2;
      ArgTDynDynArrayTRec2: TDynDynArrayTRec2;      var VArgTDynDynArrayTRec2: TDynDynArrayTRec2;
      ArgTDynDynArrayPRec2: TDynDynArrayPRec2;      var VArgTDynDynArrayPRec2: TDynDynArrayPRec2;
      ArgTDynStatArrayTRec2: TDynStatArrayTRec2;    var VArgTDynStatArrayTRec2: TDynStatArrayTRec2;
      ArgTDynStatArrayPRec2: TDynStatArrayPRec2;    var VArgTDynStatArrayPRec2: TDynStatArrayPRec2;


      // stat arrays
      ArgTStatArrayTRec1: TStatArrayTRec1;          var VArgTStatArrayTRec1: TStatArrayTRec1;
      ArgTStatArrayPRec1: TStatArrayPRec1;          var VArgTStatArrayPRec1: TStatArrayPRec1;
      ArgTStatDynArrayTRec1: TStatDynArrayTRec1;    var VArgTStatDynArrayTRec1: TStatDynArrayTRec1;
      ArgTStatDynArrayPRec1: TStatDynArrayPRec1;    var VArgTStatDynArrayPRec1: TStatDynArrayPRec1;
      ArgTStatStatArrayTRec1: TStatStatArrayTRec1;  var VArgTStatStatArrayTRec1: TStatStatArrayTRec1;
      ArgTStatStatArrayPRec1: TStatStatArrayPRec1;  var VArgTStatStatArrayPRec1: TStatStatArrayPRec1;

      ArgTStatArrayTRec2: TStatArrayTRec2;          var VArgTStatArrayTRec2: TStatArrayTRec2;
      ArgTStatArrayPRec2: TStatArrayPRec2;          var VArgTStatArrayPRec2: TStatArrayPRec2;
      ArgTStatArrayPPRec2: TStatArrayPPRec2;        var VArgTStatArrayPPRec2: TStatArrayPPRec2;
      ArgTStatDynArrayTRec2: TStatDynArrayTRec2;    var VArgTStatDynArrayTRec2: TStatDynArrayTRec2;
      ArgTStatDynArrayPRec2: TStatDynArrayPRec2;    var VArgTStatDynArrayPRec2: TStatDynArrayPRec2;
      ArgTStatStatArrayTRec2: TStatStatArrayTRec2;  var VArgTStatStatArrayTRec2: TStatStatArrayTRec2;
      ArgTStatStatArrayPRec2: TStatStatArrayPRec2;  var VArgTStatStatArrayPRec2: TStatStatArrayPRec2;


      // dyn arrays of named arrays
      ArgTDynDynTRec1Array: TDynDynTRec1Array;      var VArgTDynDynTRec1Array: TDynDynTRec1Array;
      ArgTDynDynPrec1Array: TDynDynPrec1Array;      var VArgTDynDynPrec1Array: TDynDynPrec1Array;
      ArgTDynStatTRec1Array: TDynStatTRec1Array;    var VArgTDynStatTRec1Array: TDynStatTRec1Array;
      ArgTDynStatPRec1Array: TDynStatPRec1Array;    var VArgTDynStatPRec1Array: TDynStatPRec1Array;
      ArgTDynPDynTRec1Array: TDynPDynTRec1Array;     var VArgTDynPDynTRec1Array: TDynPDynTRec1Array;
      ArgTDynPStatTRec1Array: TDynPStatTRec1Array;   var VArgTDynPStatTRec1Array: TDynPStatTRec1Array;
      ArgTDynPDynTRec1NPArray: TDynPDynTRec1NPArray; var VArgTDynPDynTRec1NPArray: TDynPDynTRec1NPArray;
      ArgTDynPStatTRec1NPArray: TDynPStatTRec1NPArray;var VArgTDynPStatTRec1NPArray: TDynPStatTRec1NPArray;

      ArgTDynDynTRec2Array: TDynDynTRec2Array;      var VArgTDynDynTRec2Array: TDynDynTRec2Array;
      ArgTDynDynPrec2Array: TDynDynPrec2Array;      var VArgTDynDynPrec2Array: TDynDynPrec2Array;
      ArgTDynDynPPrec2Array: TDynDynPPrec2Array;    var VArgTDynDynPPrec2Array: TDynDynPPrec2Array;
      ArgTDynStatTRec2Array: TDynStatTRec2Array;    var VArgTDynStatTRec2Array: TDynStatTRec2Array;
      ArgTDynStatPRec2Array: TDynStatPRec2Array;    var VArgTDynStatPRec2Array: TDynStatPRec2Array;
      ArgTDynStatPPRec2Array: TDynStatPPRec2Array;  var VArgTDynStatPPRec2Array: TDynStatPPRec2Array;

      // stat arrays of named arrays
      ArgTStatDynTRec1Array: TStatDynTRec1Array;      var VArgTStatDynTRec1Array: TStatDynTRec1Array;
      ArgTStatDynPrec1Array: TStatDynPrec1Array;      var VArgTStatDynPrec1Array: TStatDynPrec1Array;
      ArgTStatStatTRec1Array: TStatStatTRec1Array;    var VArgTStatStatTRec1Array: TStatStatTRec1Array;
      ArgTStatStatPRec1Array: TStatStatPRec1Array;    var VArgTStatStatPRec1Array: TStatStatPRec1Array;
      ArgTStatPDynTRec1Array: TStatPDynTRec1Array;     var VArgTStatPDynTRec1Array: TStatPDynTRec1Array;
      ArgTStatPStatTRec1Array: TStatPStatTRec1Array;   var VArgTStatPStatTRec1Array: TStatPStatTRec1Array;

      ArgTStatDynTRec2Array: TStatDynTRec2Array;      var VArgTStatDynTRec2Array: TStatDynTRec2Array;
      ArgTStatDynPrec2Array: TStatDynPrec2Array;      var VArgTStatDynPrec2Array: TStatDynPrec2Array;
      ArgTStatDynPPrec2Array: TStatDynPPrec2Array;    var VArgTStatDynPPrec2Array: TStatDynPPrec2Array;
      ArgTStatStatTRec2Array: TStatStatTRec2Array;    var VArgTStatStatTRec2Array: TStatStatTRec2Array;
      ArgTStatStatPRec2Array: TStatStatPRec2Array;    var VArgTStatStatPRec2Array: TStatStatPRec2Array;
      ArgTStatStatPPRec2Array: TStatStatPPRec2Array;  var VArgTStatStatPPRec2Array: TStatStatPPRec2Array;

// POINTER

      // dyn arrays
      ArgPDynArrayTRec1: PDynArrayTRec1;            var VArgPDynArrayTRec1: PDynArrayTRec1;
      ArgPDynArrayPRec1: PDynArrayPRec1;            var VArgPDynArrayPRec1: PDynArrayPRec1;
      ArgPDynDynArrayTRec1: PDynDynArrayTRec1;      var VArgPDynDynArrayTRec1: PDynDynArrayTRec1;
      ArgPDynDynArrayPRec1: PDynDynArrayPRec1;      var VArgPDynDynArrayPRec1: PDynDynArrayPRec1;
      ArgPDynStatArrayTRec1: PDynStatArrayTRec1;    var VArgPDynStatArrayTRec1: PDynStatArrayTRec1;
      ArgPDynStatArrayPRec1: PDynStatArrayPRec1;    var VArgPDynStatArrayPRec1: PDynStatArrayPRec1;

      ArgPDynArrayTRec2: PDynArrayTRec2;            var VArgPDynArrayTRec2: PDynArrayTRec2;
      ArgPDynArrayPRec2: PDynArrayPRec2;            var VArgPDynArrayPRec2: PDynArrayPRec2;
      ArgPDynArrayPPRec2: PDynArrayPPRec2;          var VArgPDynArrayPPRec2: PDynArrayPPRec2;
      ArgPDynDynArrayTRec2: PDynDynArrayTRec2;      var VArgPDynDynArrayTRec2: PDynDynArrayTRec2;
      ArgPDynDynArrayPRec2: PDynDynArrayPRec2;      var VArgPDynDynArrayPRec2: PDynDynArrayPRec2;
      ArgPDynStatArrayTRec2: PDynStatArrayTRec2;    var VArgPDynStatArrayTRec2: PDynStatArrayTRec2;
      ArgPDynStatArrayPRec2: PDynStatArrayPRec2;    var VArgPDynStatArrayPRec2: PDynStatArrayPRec2;


      // stat arrays
      ArgPStatArrayTRec1: PStatArrayTRec1;          var VArgPStatArrayTRec1: PStatArrayTRec1;
      ArgPStatArrayPRec1: PStatArrayPRec1;          var VArgPStatArrayPRec1: PStatArrayPRec1;
      ArgPStatDynArrayTRec1: PStatDynArrayTRec1;    var VArgPStatDynArrayTRec1: PStatDynArrayTRec1;
      ArgPStatDynArrayPRec1: PStatDynArrayPRec1;    var VArgPStatDynArrayPRec1: PStatDynArrayPRec1;
      ArgPStatStatArrayTRec1: PStatStatArrayTRec1;  var VArgPStatStatArrayTRec1: PStatStatArrayTRec1;
      ArgPStatStatArrayPRec1: PStatStatArrayPRec1;  var VArgPStatStatArrayPRec1: PStatStatArrayPRec1;

      ArgPStatArrayTRec2: PStatArrayTRec2;          var VArgPStatArrayTRec2: PStatArrayTRec2;
      ArgPStatArrayPRec2: PStatArrayPRec2;          var VArgPStatArrayPRec2: PStatArrayPRec2;
      ArgPStatArrayPPRec2: PStatArrayPPRec2;        var VArgPStatArrayPPRec2: PStatArrayPPRec2;
      ArgPStatDynArrayTRec2: PStatDynArrayTRec2;    var VArgPStatDynArrayTRec2: PStatDynArrayTRec2;
      ArgPStatDynArrayPRec2: PStatDynArrayPRec2;    var VArgPStatDynArrayPRec2: PStatDynArrayPRec2;
      ArgPStatStatArrayTRec2: PStatStatArrayTRec2;  var VArgPStatStatArrayTRec2: PStatStatArrayTRec2;
      ArgPStatStatArrayPRec2: PStatStatArrayPRec2;  var VArgPStatStatArrayPRec2: PStatStatArrayPRec2;


      // dyn arrays of named arrays
      ArgPDynDynTRec1Array: PDynDynTRec1Array;      var VArgPDynDynTRec1Array: PDynDynTRec1Array;
      ArgPDynDynPrec1Array: PDynDynPrec1Array;      var VArgPDynDynPrec1Array: PDynDynPrec1Array;
      ArgPDynStatTRec1Array: PDynStatTRec1Array;    var VArgPDynStatTRec1Array: PDynStatTRec1Array;
      ArgPDynStatPRec1Array: PDynStatPRec1Array;    var VArgPDynStatPRec1Array: PDynStatPRec1Array;
      ArgPDynPDynTRec1Array: PDynPDynTRec1Array;     var VArgPDynPDynTRec1Array: PDynPDynTRec1Array;
      ArgPDynPStatTRec1Array: PDynPStatTRec1Array;   var VArgPDynPStatTRec1Array: PDynPStatTRec1Array;
      ArgPDynPDynTRec1NPArray: PDynPDynTRec1NPArray;  var VArgPDynPDynTRec1NPArray: PDynPDynTRec1NPArray;
      ArgPDynPStatTRec1NPArray: PDynPStatTRec1NPArray;var VArgPDynPStatTRec1NPArray: PDynPStatTRec1NPArray;

      ArgPDynDynTRec2Array: PDynDynTRec2Array;      var VArgPDynDynTRec2Array: PDynDynTRec2Array;
      ArgPDynDynPrec2Array: PDynDynPrec2Array;      var VArgPDynDynPrec2Array: PDynDynPrec2Array;
      ArgPDynDynPPrec2Array: PDynDynPPrec2Array;    var VArgPDynDynPPrec2Array: PDynDynPPrec2Array;
      ArgPDynStatTRec2Array: PDynStatTRec2Array;    var VArgPDynStatTRec2Array: PDynStatTRec2Array;
      ArgPDynStatPRec2Array: PDynStatPRec2Array;    var VArgPDynStatPRec2Array: PDynStatPRec2Array;
      ArgPDynStatPPRec2Array: PDynStatPPRec2Array;  var VArgPDynStatPPRec2Array: PDynStatPPRec2Array;

      // stat arrays of named arrays
      ArgPStatDynTRec1Array: PStatDynTRec1Array;      var VArgPStatDynTRec1Array: PStatDynTRec1Array;
      ArgPStatDynPrec1Array: PStatDynPrec1Array;      var VArgPStatDynPrec1Array: PStatDynPrec1Array;
      ArgPStatStatTRec1Array: PStatStatTRec1Array;    var VArgPStatStatTRec1Array: PStatStatTRec1Array;
      ArgPStatStatPRec1Array: PStatStatPRec1Array;    var VArgPStatStatPRec1Array: PStatStatPRec1Array;
      ArgPStatPDynTRec1Array: PStatPDynTRec1Array;     var VArgPStatPDynTRec1Array: PStatPDynTRec1Array;
      ArgPStatPStatTRec1Array: PStatPStatTRec1Array;   var VArgPStatPStatTRec1Array: PStatPStatTRec1Array;

      ArgPStatDynTRec2Array: PStatDynTRec2Array;      var VArgPStatDynTRec2Array: PStatDynTRec2Array;
      ArgPStatDynPrec2Array: PStatDynPrec2Array;      var VArgPStatDynPrec2Array: PStatDynPrec2Array;
      ArgPStatDynPPrec2Array: PStatDynPPrec2Array;    var VArgPStatDynPPrec2Array: PStatDynPPrec2Array;
      ArgPStatStatTRec2Array: PStatStatTRec2Array;    var VArgPStatStatTRec2Array: PStatStatTRec2Array;
      ArgPStatStatPRec2Array: PStatStatPRec2Array;    var VArgPStatStatPRec2Array: PStatStatPRec2Array;
      ArgPStatStatPPRec2Array: PStatStatPPRec2Array;  var VArgPStatStatPPRec2Array: PStatStatPPRec2Array;

    {$ENDIF}
    {$IFDEF Global_Call_FooFunc}
    //FooFunc(
      // dyn arrays
      GlobTDynArrayTRec1,  GlobTDynArrayTRec1,
      GlobTDynArrayPRec1,  GlobTDynArrayPRec1,
      GlobTDynDynArrayTRec1,  GlobTDynDynArrayTRec1,
      GlobTDynDynArrayPRec1,  GlobTDynDynArrayPRec1,
      GlobTDynStatArrayTRec1,  GlobTDynStatArrayTRec1,
      GlobTDynStatArrayPRec1,  GlobTDynStatArrayPRec1,

      GlobTDynArrayTRec2,  GlobTDynArrayTRec2,
      GlobTDynArrayPRec2,  GlobTDynArrayPRec2,
      GlobTDynArrayPPRec2,  GlobTDynArrayPPRec2,
      GlobTDynDynArrayTRec2,  GlobTDynDynArrayTRec2,
      GlobTDynDynArrayPRec2,  GlobTDynDynArrayPRec2,
      GlobTDynStatArrayTRec2,  GlobTDynStatArrayTRec2,
      GlobTDynStatArrayPRec2,  GlobTDynStatArrayPRec2,


      // stat arrays
      GlobTStatArrayTRec1,  GlobTStatArrayTRec1,
      GlobTStatArrayPRec1,  GlobTStatArrayPRec1,
      GlobTStatDynArrayTRec1,  GlobTStatDynArrayTRec1,
      GlobTStatDynArrayPRec1,  GlobTStatDynArrayPRec1,
      GlobTStatStatArrayTRec1,  GlobTStatStatArrayTRec1,
      GlobTStatStatArrayPRec1,  GlobTStatStatArrayPRec1,

      GlobTStatArrayTRec2,  GlobTStatArrayTRec2,
      GlobTStatArrayPRec2,  GlobTStatArrayPRec2,
      GlobTStatArrayPPRec2,  GlobTStatArrayPPRec2,
      GlobTStatDynArrayTRec2,  GlobTStatDynArrayTRec2,
      GlobTStatDynArrayPRec2,  GlobTStatDynArrayPRec2,
      GlobTStatStatArrayTRec2,  GlobTStatStatArrayTRec2,
      GlobTStatStatArrayPRec2,  GlobTStatStatArrayPRec2,


      // dyn arrays of named arrays
      GlobTDynDynTRec1Array,  GlobTDynDynTRec1Array,
      GlobTDynDynPrec1Array,  GlobTDynDynPrec1Array,
      GlobTDynStatTRec1Array,  GlobTDynStatTRec1Array,
      GlobTDynStatPRec1Array,  GlobTDynStatPRec1Array,
      GlobTDynPDynTRec1Array,  GlobTDynPDynTRec1Array,
      GlobTDynPStatTRec1Array,  GlobTDynPStatTRec1Array,
      GlobTDynPDynTRec1NPArray, GlobTDynPDynTRec1NPArray,
      GlobTDynPStatTRec1NPArray, GlobTDynPStatTRec1NPArray,

      GlobTDynDynTRec2Array,  GlobTDynDynTRec2Array,
      GlobTDynDynPrec2Array,  GlobTDynDynPrec2Array,
      GlobTDynDynPPrec2Array,  GlobTDynDynPPrec2Array,
      GlobTDynStatTRec2Array,  GlobTDynStatTRec2Array,
      GlobTDynStatPRec2Array,  GlobTDynStatPRec2Array,
      GlobTDynStatPPRec2Array,  GlobTDynStatPPRec2Array,

      // stat arrays of named arrays
      GlobTStatDynTRec1Array,  GlobTStatDynTRec1Array,
      GlobTStatDynPrec1Array,  GlobTStatDynPrec1Array,
      GlobTStatStatTRec1Array,  GlobTStatStatTRec1Array,
      GlobTStatStatPRec1Array,  GlobTStatStatPRec1Array,
      GlobTStatPDynTRec1Array,  GlobTStatPDynTRec1Array,
      GlobTStatPStatTRec1Array,  GlobTStatPStatTRec1Array,

      GlobTStatDynTRec2Array,  GlobTStatDynTRec2Array,
      GlobTStatDynPrec2Array,  GlobTStatDynPrec2Array,
      GlobTStatDynPPrec2Array,  GlobTStatDynPPrec2Array,
      GlobTStatStatTRec2Array,  GlobTStatStatTRec2Array,
      GlobTStatStatPRec2Array,  GlobTStatStatPRec2Array,
      GlobTStatStatPPRec2Array,  GlobTStatStatPPRec2Array,


      // POINTER

      // dyn arrays
      GlobPDynArrayTRec1,  GlobPDynArrayTRec1,
      GlobPDynArrayPRec1,  GlobPDynArrayPRec1,
      GlobPDynDynArrayTRec1,  GlobPDynDynArrayTRec1,
      GlobPDynDynArrayPRec1,  GlobPDynDynArrayPRec1,
      GlobPDynStatArrayTRec1,  GlobPDynStatArrayTRec1,
      GlobPDynStatArrayPRec1,  GlobPDynStatArrayPRec1,

      GlobPDynArrayTRec2,  GlobPDynArrayTRec2,
      GlobPDynArrayPRec2,  GlobPDynArrayPRec2,
      GlobPDynArrayPPRec2,  GlobPDynArrayPPRec2,
      GlobPDynDynArrayTRec2,  GlobPDynDynArrayTRec2,
      GlobPDynDynArrayPRec2,  GlobPDynDynArrayPRec2,
      GlobPDynStatArrayTRec2,  GlobPDynStatArrayTRec2,
      GlobPDynStatArrayPRec2,  GlobPDynStatArrayPRec2,


      // stat arrays
      GlobPStatArrayTRec1,  GlobPStatArrayTRec1,
      GlobPStatArrayPRec1,  GlobPStatArrayPRec1,
      GlobPStatDynArrayTRec1,  GlobPStatDynArrayTRec1,
      GlobPStatDynArrayPRec1,  GlobPStatDynArrayPRec1,
      GlobPStatStatArrayTRec1,  GlobPStatStatArrayTRec1,
      GlobPStatStatArrayPRec1,  GlobPStatStatArrayPRec1,

      GlobPStatArrayTRec2,  GlobPStatArrayTRec2,
      GlobPStatArrayPRec2,  GlobPStatArrayPRec2,
      GlobPStatArrayPPRec2,  GlobPStatArrayPPRec2,
      GlobPStatDynArrayTRec2,  GlobPStatDynArrayTRec2,
      GlobPStatDynArrayPRec2,  GlobPStatDynArrayPRec2,
      GlobPStatStatArrayTRec2,  GlobPStatStatArrayTRec2,
      GlobPStatStatArrayPRec2,  GlobPStatStatArrayPRec2,

      // dyn arrays of named arrays
      GlobPDynDynTRec1Array,  GlobPDynDynTRec1Array,
      GlobPDynDynPrec1Array,  GlobPDynDynPrec1Array,
      GlobPDynStatTRec1Array,  GlobPDynStatTRec1Array,
      GlobPDynStatPRec1Array,  GlobPDynStatPRec1Array,
      GlobPDynPDynTRec1Array,  GlobPDynPDynTRec1Array,
      GlobPDynPStatTRec1Array,  GlobPDynPStatTRec1Array,
      GlobPDynPDynTRec1NPArray, GlobPDynPDynTRec1NPArray,
      GlobPDynPStatTRec1NPArray, GlobPDynPStatTRec1NPArray,

      GlobPDynDynTRec2Array,  GlobPDynDynTRec2Array,
      GlobPDynDynPrec2Array,  GlobPDynDynPrec2Array,
      GlobPDynDynPPrec2Array,  GlobPDynDynPPrec2Array,
      GlobPDynStatTRec2Array,  GlobPDynStatTRec2Array,
      GlobPDynStatPRec2Array,  GlobPDynStatPRec2Array,
      GlobPDynStatPPRec2Array,  GlobPDynStatPPRec2Array,

      // stat arrays of named arrays
      GlobPStatDynTRec1Array,  GlobPStatDynTRec1Array,
      GlobPStatDynPrec1Array,  GlobPStatDynPrec1Array,
      GlobPStatStatTRec1Array,  GlobPStatStatTRec1Array,
      GlobPStatStatPRec1Array,  GlobPStatStatPRec1Array,
      GlobPStatPDynTRec1Array,  GlobPStatPDynTRec1Array,
      GlobPStatPStatTRec1Array,  GlobPStatPStatTRec1Array,

      GlobPStatDynTRec2Array,  GlobPStatDynTRec2Array,
      GlobPStatDynPrec2Array,  GlobPStatDynPrec2Array,
      GlobPStatDynPPrec2Array,  GlobPStatDynPPrec2Array,
      GlobPStatStatTRec2Array,  GlobPStatStatTRec2Array,
      GlobPStatStatPRec2Array,  GlobPStatStatPRec2Array,
      GlobPStatStatPPRec2Array,  GlobPStatStatPPRec2Array,

    {$ENDIF}

    {$IFDEF FooFunc_Local}
    var
      VarArrayHelperI: Integer;

      VarDynIntArray: TDynIntArray;
      VarDynIntArrayP: ^TDynIntArray;
      VarStatIntArray: TStatIntArray;
      VarPDynIntArray: PDynIntArray;
      VarPStatIntArray: PStatIntArray;
      VarDynIntArrayA: Array of Integer;
      VarStatIntArrayA: Array [5..9] of Integer;

      VarDynObjArray: TDynObjArray;
      VarStatObjArray: TStatObjArray;
      VarPDynObjArray: PDynObjArray;
      VarPStatObjArray: PStatObjArray;
      VarDynObjArrayA: Array of TObjectInArray;
      VarStatObjArrayA: Array [5..9] of TObjectInArray;

      VarDynRecArray: TDynRecArray;
      VarStatRecArray: TStatRecArray;
      VarPDynRecArray: PDynRecArray;
      VarPStatRecArray: PStatRecArray;
      VarDynRecArrayA: Array of TRecordInArray;
      VarStatRecArrayA: Array [5..9] of TRecordInArray;

      VarDynIntArray2: TDynIntArray2;

      VarDynIntArray2A: Array of TDynIntArray;
      VarStatIntArray2A: Array [5..9] of TDynIntArray;
      VarDynIntArray2P: Array of PDynIntArray;
      VarStatIntArray2P: Array [5..9] of PDynIntArray;

    {$ENDIF}

    {$IFDEF Global_Var}
    //var
  // dyn array with record
    myRec2 : PRecForArray2;

      // target for pointer
      GlobTDynArrayTRec1NP, GlobTDynArrayTRec1NP2: TDynArrayTRec1NP;
      GlobTStatArrayTRec1NP, GlobTStatArrayTRec1NP2: TStatArrayTRec1NP;

      // dyn arrays
      GlobTDynArrayTRec1: TDynArrayTRec1;
      GlobTDynArrayPRec1: TDynArrayPRec1;
      GlobTDynDynArrayTRec1: TDynDynArrayTRec1;
      GlobTDynDynArrayPRec1: TDynDynArrayPRec1;
      GlobTDynStatArrayTRec1: TDynStatArrayTRec1;
      GlobTDynStatArrayPRec1: TDynStatArrayPRec1;

      GlobTDynArrayTRec2: TDynArrayTRec2;
      GlobTDynArrayPRec2: TDynArrayPRec2;
      GlobTDynArrayPPRec2: TDynArrayPPRec2;
      GlobTDynDynArrayTRec2: TDynDynArrayTRec2;
      GlobTDynDynArrayPRec2: TDynDynArrayPRec2;
      GlobTDynStatArrayTRec2: TDynStatArrayTRec2;
      GlobTDynStatArrayPRec2: TDynStatArrayPRec2;


      // stat arrays
      GlobTStatArrayTRec1: TStatArrayTRec1;
      GlobTStatArrayPRec1: TStatArrayPRec1;
      GlobTStatDynArrayTRec1: TStatDynArrayTRec1;
      GlobTStatDynArrayPRec1: TStatDynArrayPRec1;
      GlobTStatStatArrayTRec1: TStatStatArrayTRec1;
      GlobTStatStatArrayPRec1: TStatStatArrayPRec1;

      GlobTStatArrayTRec2: TStatArrayTRec2;
      GlobTStatArrayPRec2: TStatArrayPRec2;
      GlobTStatArrayPPRec2: TStatArrayPPRec2;
      GlobTStatDynArrayTRec2: TStatDynArrayTRec2;
      GlobTStatDynArrayPRec2: TStatDynArrayPRec2;
      GlobTStatStatArrayTRec2: TStatStatArrayTRec2;
      GlobTStatStatArrayPRec2: TStatStatArrayPRec2;

      // dyn arrays of named arrays
      GlobTDynDynTRec1Array: TDynDynTRec1Array;
      GlobTDynDynPrec1Array: TDynDynPrec1Array;
      GlobTDynStatTRec1Array: TDynStatTRec1Array;
      GlobTDynStatPRec1Array: TDynStatPRec1Array;
      GlobTDynPDynTRec1Array: TDynPDynTRec1Array;
      GlobTDynPStatTRec1Array: TDynPStatTRec1Array;
      GlobTDynPDynTRec1NPArray: TDynPDynTRec1NPArray;
      GlobTDynPStatTRec1NPArray: TDynPStatTRec1NPArray;

      GlobTDynDynTRec2Array: TDynDynTRec2Array;
      GlobTDynDynPrec2Array: TDynDynPrec2Array;
      GlobTDynDynPPrec2Array: TDynDynPPrec2Array;
      GlobTDynStatTRec2Array: TDynStatTRec2Array;
      GlobTDynStatPRec2Array: TDynStatPRec2Array;
      GlobTDynStatPPRec2Array: TDynStatPPRec2Array;

      // stat arrays of named arrays
      GlobTStatDynTRec1Array: TStatDynTRec1Array;
      GlobTStatDynPrec1Array: TStatDynPrec1Array;
      GlobTStatStatTRec1Array: TStatStatTRec1Array;
      GlobTStatStatPRec1Array: TStatStatPRec1Array;
      GlobTStatPDynTRec1Array: TStatPDynTRec1Array;
      GlobTStatPStatTRec1Array: TStatPStatTRec1Array;

      GlobTStatDynTRec2Array: TStatDynTRec2Array;
      GlobTStatDynPrec2Array: TStatDynPrec2Array;
      GlobTStatDynPPrec2Array: TStatDynPPrec2Array;
      GlobTStatStatTRec2Array: TStatStatTRec2Array;
      GlobTStatStatPRec2Array: TStatStatPRec2Array;
      GlobTStatStatPPRec2Array: TStatStatPPRec2Array;


      GlobTEnumArrayTRec1:     TEnumArrayTRec1;      // = array [TEnunForArray] of TRecForArray3;
      GlobTSubEnumArrayTRec1:  TSubEnumArrayTRec1;   //  = array [eaTwo..eaThree] of TRecForArray3;

      GlobTStatDualArrayTRec2: TStatDualArrayTRec2;  // = array [3..5, 3..5] of TRecForArray2;

// POINTER

      // dyn arrays
      GlobPDynArrayTRec1: PDynArrayTRec1;
      GlobPDynArrayPRec1: PDynArrayPRec1;
      GlobPDynDynArrayTRec1: PDynDynArrayTRec1;
      GlobPDynDynArrayPRec1: PDynDynArrayPRec1;
      GlobPDynStatArrayTRec1: PDynStatArrayTRec1;
      GlobPDynStatArrayPRec1: PDynStatArrayPRec1;

      GlobPDynArrayTRec2: PDynArrayTRec2;
      GlobPDynArrayPRec2: PDynArrayPRec2;
      GlobPDynArrayPPRec2: PDynArrayPPRec2;
      GlobPDynDynArrayTRec2: PDynDynArrayTRec2;
      GlobPDynDynArrayPRec2: PDynDynArrayPRec2;
      GlobPDynStatArrayTRec2: PDynStatArrayTRec2;
      GlobPDynStatArrayPRec2: PDynStatArrayPRec2;


      // stat arrays
      GlobPStatArrayTRec1: PStatArrayTRec1;
      GlobPStatArrayPRec1: PStatArrayPRec1;
      GlobPStatDynArrayTRec1: PStatDynArrayTRec1;
      GlobPStatDynArrayPRec1: PStatDynArrayPRec1;
      GlobPStatStatArrayTRec1: PStatStatArrayTRec1;
      GlobPStatStatArrayPRec1: PStatStatArrayPRec1;

      GlobPStatArrayTRec2: PStatArrayTRec2;
      GlobPStatArrayPRec2: PStatArrayPRec2;
      GlobPStatArrayPPRec2: PStatArrayPPRec2;
      GlobPStatDynArrayTRec2: PStatDynArrayTRec2;
      GlobPStatDynArrayPRec2: PStatDynArrayPRec2;
      GlobPStatStatArrayTRec2: PStatStatArrayTRec2;
      GlobPStatStatArrayPRec2: PStatStatArrayPRec2;

      // dyn arrays of named arrays
      GlobPDynDynTRec1Array: PDynDynTRec1Array;
      GlobPDynDynPrec1Array: PDynDynPrec1Array;
      GlobPDynStatTRec1Array: PDynStatTRec1Array;
      GlobPDynStatPRec1Array: PDynStatPRec1Array;
      GlobPDynPDynTRec1Array: PDynPDynTRec1Array;
      GlobPDynPStatTRec1Array: PDynPStatTRec1Array;
      GlobPDynPDynTRec1NPArray: PDynPDynTRec1NPArray;
      GlobPDynPStatTRec1NPArray: PDynPStatTRec1NPArray;

      GlobPDynDynTRec2Array: PDynDynTRec2Array;
      GlobPDynDynPrec2Array: PDynDynPrec2Array;
      GlobPDynDynPPrec2Array: PDynDynPPrec2Array;
      GlobPDynStatTRec2Array: PDynStatTRec2Array;
      GlobPDynStatPRec2Array: PDynStatPRec2Array;
      GlobPDynStatPPRec2Array: PDynStatPPRec2Array;

      // dyn arrays of named arrays
      GlobPStatDynTRec1Array: PStatDynTRec1Array;
      GlobPStatDynPrec1Array: PStatDynPrec1Array;
      GlobPStatStatTRec1Array: PStatStatTRec1Array;
      GlobPStatStatPRec1Array: PStatStatPRec1Array;
      GlobPStatPDynTRec1Array: PStatPDynTRec1Array;
      GlobPStatPStatTRec1Array: PStatPStatTRec1Array;

      GlobPStatDynTRec2Array: PStatDynTRec2Array;
      GlobPStatDynPrec2Array: PStatDynPrec2Array;
      GlobPStatDynPPrec2Array: PStatDynPPrec2Array;
      GlobPStatStatTRec2Array: PStatStatTRec2Array;
      GlobPStatStatPRec2Array: PStatStatPRec2Array;
      GlobPStatStatPPRec2Array: PStatStatPPRec2Array;

      GlobPEnumArrayTRec1:     PEnumArrayTRec1;      // = ^TEnumArrayTRec1;
      GlobPSubEnumArrayTRec1:  PSubEnumArrayTRec1;   // = ^TSubEnumArrayTRec1;

// UN-NAMED types
      // dyn arrays
      GlobADynArrayTRec1      : array of TRecForArray3;
      GlobADynArrayPRec1      : array of ^TRecForArray3;
      GlobADynDynArrayTRec1   : array of array of TRecForArray1;
      GlobADynDynArrayPRec1   : array of array of ^TRecForArray1;
      GlobADynStatArrayTRec1  : array of array [3..5] of TRecForArray1;
      GlobADynStatArrayPRec1  : array of array [3..5] of ^TRecForArray1;

      GlobADynArrayTRec2      : array of TRecForArray4;
      GlobADynArrayPRec2      : array of ^TRecForArray4;
      GlobADynArrayPPRec2     : array of ^PRecForArray4; // double pointer
      GlobADynDynArrayTRec2   : array of array of TRecForArray2;
      GlobADynDynArrayPRec2   : array of array of ^TRecForArray2;
      GlobADynStatArrayTRec2  : array of array [3..5] of TRecForArray2;
      GlobADynStatArrayPRec2  : array of array [3..5] of ^TRecForArray2;


      // stat arrays
      GlobAStatArrayTRec1     : array [3..5] of TRecForArray3;
      GlobAStatArrayPRec1     : array [3..5] of ^TRecForArray3;
      GlobAStatDynArrayTRec1  : array [3..5] of array of TRecForArray1;
      GlobAStatDynArrayPRec1  : array [3..5] of array of ^TRecForArray1;
      GlobAStatStatArrayTRec1 : array [3..5] of array [3..5] of TRecForArray1;
      GlobAStatStatArrayPRec1 : array [3..5] of array [3..5] of ^TRecForArray1;

      GlobAStatArrayTRec2     : array [3..5] of TRecForArray4;
      GlobAStatArrayPRec2     : array [3..5] of ^TRecForArray4;
      GlobAStatArrayPPRec2    : array [3..5] of ^PRecForArray4; // double pointer
      GlobAStatDynArrayTRec2  : array [3..5] of array of TRecForArray2;
      GlobAStatDynArrayPRec2  : array [3..5] of array of ^TRecForArray2;
      GlobAStatStatArrayTRec2 : array [3..5] of array [3..5] of TRecForArray2;
      GlobAStatStatArrayPRec2 : array [3..5] of array [3..5] of ^TRecForArray2;


      // dyn arrays of named arrays
      GlobADynDynTRec1Array   : array of TDynArrayTRec1;
      GlobADynDynPrec1Array   : array of TDynArrayPRec1;
      GlobADynStatTRec1Array  : array of TStatArrayTRec1;
      GlobADynStatPRec1Array  : array of TStatArrayPRec1;
      GlobADynPDynTRec1Array   : array of ^TDynArrayTRec1;
      GlobADynPStatTRec1Array  : array of ^TStatArrayTRec1;

      GlobADynDynTRec2Array   : array of TDynArrayTRec2;
      GlobADynDynPrec2Array   : array of TDynArrayPRec2;
      GlobADynDynPPrec2Array  : array of TDynArrayPPRec2; // double pointer
      GlobADynStatTRec2Array  : array of TStatArrayTRec2;
      GlobADynStatPRec2Array  : array of TStatArrayPRec2;
      GlobADynStatPPRec2Array : array of TStatArrayPPRec2; // double pointer

      // stat arrays of named arrays
      GlobAStatDynTRec1Array   : array of TDynArrayTRec1;
      GlobAStatDynPrec1Array   : array of TDynArrayPRec1;
      GlobAStatStatTRec1Array  : array of TStatArrayTRec1;
      GlobAStatStatPRec1Array  : array of TStatArrayPRec1;
      GlobAStatPDynTRec1Array   : array of ^TDynArrayTRec1;
      GlobAStatPStatTRec1Array  : array of ^TStatArrayTRec1;

      GlobAStatDynTRec2Array   : array of TDynArrayTRec2;
      GlobAStatDynPrec2Array   : array of TDynArrayPRec2;
      GlobAStatDynPPrec2Array  : array of TDynArrayPPRec2; // double pointer
      GlobAStatStatTRec2Array  : array of TStatArrayTRec2;
      GlobAStatStatPRec2Array  : array of TStatArrayPRec2;
      GlobAStatStatPPRec2Array : array of TStatArrayPPRec2; // double pointer

      GlobADynDynDynArrayTRec1      : array of array of array of TRecForArray1;
      GlobADynDynDynDynArrayTRec1   : array of array of array of array of TRecForArray1;
      GlobADynArrayTRec5            : array of record a: byte; c:integer; end;
      GlobAStatArrayTRec5           : array [3..5] of record a: byte; c:integer; end;

      GlobAEnumArrayTRec1:     array [TEnunForArray] of TRecForArray3;
      GlobASubEnumArrayTRec1:  array [eaTwo..eaThree] of TRecForArray3;

      GlobAStatDualArrayTRec2: array [3..5, 3..5] of TRecForArray2;

      // TODO
      GlobAStatDynDynArrayTRec2  : array [3..5] of array of array of TRecForArray2;
      GlobADynDynStatArrayTRec2  : array of array of array [3..5] of TRecForArray2;


    {$ENDIF}
  {%endregion}

  {%region CODE (initilization)}
    {$IFDEF FooFunc_Body}
    //begin
      SetLength(VarDynIntArray,2);
      SetLength(VarDynIntArrayA,2);
      VarPDynIntArray := @VarDynIntArray;
      VarPStatIntArray := @VarStatIntArray;
      VarDynIntArrayP := @VarDynIntArray;
      for VarArrayHelperI := low(VarDynIntArray) to high (VarDynIntArray) do
        VarDynIntArray[VarArrayHelperI] := 2 * VarArrayHelperI;
      for VarArrayHelperI := low(VarStatIntArray) to high (VarStatIntArray) do
        VarStatIntArray[VarArrayHelperI] := 2 * VarArrayHelperI;
      for VarArrayHelperI := low(VarDynIntArrayA) to high (VarDynIntArrayA) do
        VarDynIntArrayA[VarArrayHelperI] := 2 * VarArrayHelperI;
      for VarArrayHelperI := low(VarStatIntArrayA) to high (VarStatIntArrayA) do
        VarStatIntArrayA[VarArrayHelperI] := 2 * VarArrayHelperI;

      SetLength(VarDynObjArray,2);
      SetLength(VarDynObjArrayA,2);
      VarPDynObjArray := @VarDynObjArray;
      VarPStatObjArray := @VarStatObjArray;
      for VarArrayHelperI := low(VarDynObjArray) to high (VarDynObjArray) do begin
        VarDynObjArray[VarArrayHelperI] := TObjectInArray.Create;
        VarDynObjArray[VarArrayHelperI].Foo := 2 * VarArrayHelperI;
      end;
      for VarArrayHelperI := low(VarStatObjArray) to high (VarStatObjArray) do begin
        VarStatObjArray[VarArrayHelperI] := TObjectInArray.Create;
        VarStatObjArray[VarArrayHelperI].Foo := 2 * VarArrayHelperI;
      end;
      for VarArrayHelperI := low(VarDynObjArrayA) to high (VarDynObjArrayA) do begin
        VarDynObjArrayA[VarArrayHelperI] := TObjectInArray.Create;
        VarDynObjArrayA[VarArrayHelperI].Foo := 2 * VarArrayHelperI;
      end;
      for VarArrayHelperI := low(VarStatObjArrayA) to high (VarStatObjArrayA) do begin
        VarStatObjArrayA[VarArrayHelperI] := TObjectInArray.Create;
        VarStatObjArrayA[VarArrayHelperI].Foo := 2 * VarArrayHelperI;
      end;

      SetLength(VarDynObjArray,2);
      SetLength(VarDynObjArrayA,2);
      VarPDynObjArray := @VarDynObjArray;
      VarPStatObjArray := @VarStatObjArray;
      for VarArrayHelperI := low(VarDynObjArray) to high (VarDynObjArray) do
        VarDynObjArray[VarArrayHelperI].Foo := 2 * VarArrayHelperI;
      for VarArrayHelperI := low(VarStatObjArray) to high (VarStatObjArray) do
        VarStatObjArray[VarArrayHelperI].Foo := 2 * VarArrayHelperI;
      for VarArrayHelperI := low(VarDynObjArrayA) to high (VarDynObjArrayA) do
        VarDynObjArrayA[VarArrayHelperI].Foo := 2 * VarArrayHelperI;
      for VarArrayHelperI := low(VarStatObjArrayA) to high (VarStatObjArrayA) do
        VarStatObjArrayA[VarArrayHelperI].Foo := 2 * VarArrayHelperI;

      SetLength(VarDynIntArray2, 2);
      SetLength(VarDynIntArray2[0], 3);

      SetLength(VarDynIntArray2A, 2);
      SetLength(VarDynIntArray2A[0], 3);
      SetLength(VarStatIntArray2A[5], 3);

      SetLength(VarDynIntArray2p, 2);
      VarDynIntArray2p[0] := @VarDynIntArray;
      VarStatIntArray2P[5] := @VarDynIntArray;
    {$ENDIF}

    {$IFDEF Global_Body}
    //begin
      new(myRec2);
      myRec2^.c := 16;
      SetLength(myRec2^.recs, 16);
      myRec2^.recs[0].a := 10;
      myRec2^.recs[0].b := 20;
      myRec2^.recs[1].a := 11;
      myRec2^.recs[1].b := 21;


    // target for pointer
    //GlobTDynArrayTRec1NP, GlobTDynArrayTRec1NP2: TDynArrayTRec1NP;
    SetLength(GlobTDynArrayTRec1NP, 3);
    GlobTDynArrayTRec1NP[0].a := 500;
    GlobTDynArrayTRec1NP[1].a := 501;
    SetLength(GlobTDynArrayTRec1NP2, 3);
    GlobTDynArrayTRec1NP2[0].a := 505;
    GlobTDynArrayTRec1NP2[1].a := 506;
    //GlobTStatArrayTRec1NP, GlobTStatArrayTRec1NP2: TStatArrayTRec1NP;
    GlobTStatArrayTRec1NP[3].a := 510;
    GlobTStatArrayTRec1NP[4].a := 511;
    GlobTStatArrayTRec1NP2[3].a := 515;
    GlobTStatArrayTRec1NP2[4].a := 516;


  // dyn arrays
      //GlobTDynArrayTRec1: TDynArrayTRec1;
      SetLength(GlobTDynArrayTRec1, 3);
      globtdynarraytrec1[0].a := 90;            globtdynarraytrec1[0].b := 243;
      globtdynarraytrec1[1].a := 91;            globtdynarraytrec1[1].b := 243;
      //GlobTDynArrayPRec1: TDynArrayPRec1;
      SetLength(GlobTDynArrayPRec1, 3);
      GlobTDynArrayPRec1[0] := @GlobTDynArrayTRec1[0];
      GlobTDynArrayPRec1[1] := @GlobTDynArrayTRec1[1];
      //GlobTDynDynArrayTRec1: TDynDynArrayTRec1;
      SetLength(GlobTDynDynArrayTRec1, 3);
      SetLength(GlobTDynDynArrayTRec1[0], 3);
      globtdyndynarraytrec1[0][0].a := 80;            globtdyndynarraytrec1[0][0].b := 243;
      globtdyndynarraytrec1[0][1].a := 81;            globtdyndynarraytrec1[0][1].b := 243;
      SetLength(GlobTDynDynArrayTRec1[1], 3);
      globtdyndynarraytrec1[1][0].a := 85;            globtdyndynarraytrec1[1][0].b := 243;
      globtdyndynarraytrec1[1][1].a := 86;            globtdyndynarraytrec1[1][1].b := 243;
      //GlobTDynDynArrayPRec1: TDynDynArrayPRec1;
      SetLength(GlobTDynDynArrayPRec1, 3);
      SetLength(GlobTDynDynArrayPRec1[0], 3);
      GlobTDynDynArrayPRec1[0][0] := @GlobTDynDynArrayTRec1[0][0];
      GlobTDynDynArrayPRec1[0][1] := @GlobTDynDynArrayTRec1[0][1];
      SetLength(GlobTDynDynArrayPRec1[1], 3);
      GlobTDynDynArrayPRec1[1][0] := @GlobTDynDynArrayTRec1[1][0];
      GlobTDynDynArrayPRec1[1][1] := @GlobTDynDynArrayTRec1[1][1];
      //GlobTDynStatArrayTRec1: TDynStatArrayTRec1;
      SetLength(GlobTDynStatArrayTRec1, 3);
      globtdynstatarraytrec1[0][3].a := 70;            globtdynstatarraytrec1[0][3].b := 243;
      globtdynstatarraytrec1[0][4].a := 71;            globtdynstatarraytrec1[0][4].b := 243;
      globtdynstatarraytrec1[1][3].a := 75;            globtdynstatarraytrec1[1][3].b := 243;
      globtdynstatarraytrec1[1][4].a := 76;            globtdynstatarraytrec1[1][4].b := 243;
      //GlobTDynStatArrayPRec1: TDynStatArrayPRec1;
      SetLength(GlobTDynStatArrayPRec1, 3);
      GlobTDynStatArrayPRec1[0][3] := @GlobTDynStatArrayTRec1[0][3];
      GlobTDynStatArrayPRec1[0][4] := @GlobTDynStatArrayTRec1[0][4];
      GlobTDynStatArrayPRec1[1][3] := @GlobTDynStatArrayTRec1[1][3];
      GlobTDynStatArrayPRec1[1][4] := @GlobTDynStatArrayTRec1[1][4];

      //GlobTDynArrayTRec2: TDynArrayTRec2;
      SetLength(GlobTDynArrayTRec2, 3);
      globtdynarraytrec2[0].c := 90;            globtdynarraytrec2[0].recs := nil;
      globtdynarraytrec2[1].c := 91;            globtdynarraytrec2[1].recs := nil;
      //GlobTDynArrayPRec2: TDynArrayPRec2;
      SetLength(GlobTDynArrayPRec2, 3);
      GlobTDynArrayPRec2[0] := @GlobTDynArrayTRec2[0];
      GlobTDynArrayPRec2[1] := @GlobTDynArrayTRec2[1];
      //GlobTDynDynArrayTRec2: TDynDynArrayTRec2;
      SetLength(GlobTDynDynArrayTRec2, 3);
      SetLength(GlobTDynDynArrayTRec2[0], 3);
      globtdyndynarraytrec2[0][0].c := 80;            globtdyndynarraytrec2[0][0].recs := nil;
      globtdyndynarraytrec2[0][1].c := 81;            globtdyndynarraytrec2[0][1].recs := nil;
      SetLength(GlobTDynDynArrayTRec2[1], 3);
      globtdyndynarraytrec2[1][0].c := 85;            globtdyndynarraytrec2[1][0].recs := nil;
      globtdyndynarraytrec2[1][1].c := 86;            globtdyndynarraytrec2[1][1].recs := nil;
      //GlobTDynDynArrayPRec2: TDynDynArrayPRec2;
      SetLength(GlobTDynDynArrayPRec2, 3);
      SetLength(GlobTDynDynArrayPRec2[0], 3);
      GlobTDynDynArrayPRec2[0][0] := @GlobTDynDynArrayTRec2[0][0];
      GlobTDynDynArrayPRec2[0][1] := @GlobTDynDynArrayTRec2[0][1];
      SetLength(GlobTDynDynArrayPRec2[1], 3);
      GlobTDynDynArrayPRec2[1][0] := @GlobTDynDynArrayTRec2[1][0];
      GlobTDynDynArrayPRec2[1][1] := @GlobTDynDynArrayTRec2[1][1];
      //GlobTDynStatArrayTRec2: TDynStatArrayTRec2;
      SetLength(GlobTDynStatArrayTRec2, 3);
      globtdynstatarraytrec2[0][3].c := 70;            globtdynstatarraytrec2[0][3].recs := nil;
      globtdynstatarraytrec2[0][4].c := 71;            globtdynstatarraytrec2[0][4].recs := nil;
      globtdynstatarraytrec2[1][3].c := 75;            globtdynstatarraytrec2[1][3].recs := nil;
      globtdynstatarraytrec2[1][4].c := 76;            globtdynstatarraytrec2[1][4].recs := nil;
      //GlobTDynStatArrayPRec2: TDynStatArrayPRec2;
      SetLength(GlobTDynStatArrayPRec2, 3);
      GlobTDynStatArrayPRec2[0][3] := @GlobTDynStatArrayTRec2[0][3];
      GlobTDynStatArrayPRec2[0][4] := @GlobTDynStatArrayTRec2[0][4];
      GlobTDynStatArrayPRec2[1][3] := @GlobTDynStatArrayTRec2[1][3];
      GlobTDynStatArrayPRec2[1][4] := @GlobTDynStatArrayTRec2[1][4];


  // stat arrays
      //GlobTStatArrayTRec1: TStatArrayTRec1;
      globtstatarraytrec1[3].a := 50;            globtstatarraytrec1[3].b := 243;
      globtstatarraytrec1[4].a := 51;            globtstatarraytrec1[4].b := 243;
      //GlobTStatArrayPRec1: TStatArrayPRec1;
      GlobTStatArrayPRec1[3] := @GlobTStatArrayTRec1[3];
      GlobTStatArrayPRec1[4] := @GlobTStatArrayTRec1[4];
      //GlobTStatDynArrayTRec1: TStatDynArrayTRec1;
      SetLength(GlobTStatDynArrayTRec1[3], 3);
      globtstatdynarraytrec1[3][0].a := 40;            globtstatdynarraytrec1[3][0].b := 243;
      globtstatdynarraytrec1[3][1].a := 41;            globtstatdynarraytrec1[3][1].b := 243;
      SetLength(GlobTStatDynArrayTRec1[4], 3);
      globtstatdynarraytrec1[4][0].a := 45;            globtstatdynarraytrec1[4][0].b := 243;
      globtstatdynarraytrec1[4][1].a := 46;            globtstatdynarraytrec1[4][1].b := 243;
      //GlobTStatDynArrayPRec1: TStatDynArrayPRec1;
      SetLength(GlobTStatDynArrayPRec1[3], 3);
      GlobTStatDynArrayPRec1[3][0] := @GlobTDynDynArrayTRec1[3][0];
      GlobTStatDynArrayPRec1[3][1] := @GlobTDynDynArrayTRec1[3][1];
      SetLength(GlobTStatDynArrayPRec1[4], 3);
      GlobTStatDynArrayPRec1[4][0] := @GlobTDynDynArrayTRec1[4][0];
      GlobTStatDynArrayPRec1[4][1] := @GlobTDynDynArrayTRec1[4][1];
      //GlobTStatStatArrayTRec1: TStatStatArrayTRec1;
      globtstatstatarraytrec1[3][3].a := 30;            globtstatstatarraytrec1[3][3].b := 243;
      globtstatstatarraytrec1[3][4].a := 31;            globtstatstatarraytrec1[3][4].b := 243;
      globtstatstatarraytrec1[4][3].a := 35;            globtstatstatarraytrec1[4][3].b := 243;
      globtstatstatarraytrec1[4][4].a := 36;            globtstatstatarraytrec1[4][4].b := 243;
      //GlobTStatStatArrayPRec1: TStatStatArrayPRec1;
      GlobTStatStatArrayPRec1[3][3] := @GlobTStatStatArrayTRec1[3][3];
      GlobTStatStatArrayPRec1[3][4] := @GlobTStatStatArrayTRec1[3][4];
      GlobTStatStatArrayPRec1[4][3] := @GlobTStatStatArrayTRec1[4][3];
      GlobTStatStatArrayPRec1[4][4] := @GlobTStatStatArrayTRec1[4][4];

      //GlobTStatArrayTRec2: TStatArrayTRec2;
      globtstatarraytrec2[3].c := 50;            globtstatarraytrec2[3].recs := nil;
      globtstatarraytrec2[4].c := 51;            globtstatarraytrec2[4].recs := nil;
      //GlobTStatArrayPRec2: TStatArrayPRec2;
      GlobTStatArrayPRec2[3] := @GlobTStatArrayTRec2[3];
      GlobTStatArrayPRec2[4] := @GlobTStatArrayTRec2[4];
      //GlobTStatDynArrayTRec2: TStatDynArrayTRec2;
      SetLength(GlobTStatDynArrayTRec2[3], 3);
      globtstatdynarraytrec2[3][0].c := 40;            globtstatdynarraytrec2[3][0].recs := nil;
      globtstatdynarraytrec2[3][1].c := 41;            globtstatdynarraytrec2[3][1].recs := nil;
      SetLength(GlobTStatDynArrayTRec2[4], 3);
      globtstatdynarraytrec2[4][0].c := 45;            globtstatdynarraytrec2[4][0].recs := nil;
      globtstatdynarraytrec2[4][1].c := 46;            globtstatdynarraytrec2[4][1].recs := nil;
      //GlobTStatDynArrayPRec2: TStatDynArrayPRec2;
      SetLength(GlobTStatDynArrayPRec2[3], 3);
      GlobTStatDynArrayPRec2[3][0] := @GlobTDynDynArrayTRec2[3][0];
      GlobTStatDynArrayPRec2[3][1] := @GlobTDynDynArrayTRec2[3][1];
      SetLength(GlobTStatDynArrayPRec2[4], 3);
      GlobTStatDynArrayPRec2[4][0] := @GlobTDynDynArrayTRec2[4][0];
      GlobTStatDynArrayPRec2[4][1] := @GlobTDynDynArrayTRec2[4][1];
      //GlobTStatStatArrayTRec2: TStatStatArrayTRec2;
      globtstatstatarraytrec2[3][3].c := 30;            globtstatstatarraytrec2[3][3].recs := nil;
      globtstatstatarraytrec2[3][4].c := 31;            globtstatstatarraytrec2[3][4].recs := nil;
      globtstatstatarraytrec2[4][3].c := 35;            globtstatstatarraytrec2[4][3].recs := nil;
      globtstatstatarraytrec2[4][4].c := 36;            globtstatstatarraytrec2[4][4].recs := nil;
      //GlobTStatStatArrayPRec2: TStatStatArrayPRec2;
      GlobTStatStatArrayPRec2[3][3] := @GlobTStatStatArrayTRec2[3][3];
      GlobTStatStatArrayPRec2[3][4] := @GlobTStatStatArrayTRec2[3][4];
      GlobTStatStatArrayPRec2[4][3] := @GlobTStatStatArrayTRec2[4][3];
      GlobTStatStatArrayPRec2[4][4] := @GlobTStatStatArrayTRec2[4][4];



  // dyn arrays of named arrays
      //GlobTDynDynTRec1Array: TDynDynTRec1Array;
      SetLength(GlobTDynDynTRec1Array, 3);
      SetLength(GlobTDynDynTRec1Array[0], 3);
      globtdyndyntrec1array[0][0].a := 80;            globtdyndyntrec1array[0][0].b := 243;
      globtdyndyntrec1array[0][1].a := 81;            globtdyndyntrec1array[0][1].b := 243;
      SetLength(GlobTDynDynTRec1Array[1], 3);
      globtdyndyntrec1array[1][0].a := 85;            globtdyndyntrec1array[1][0].b := 243;
      globtdyndyntrec1array[1][1].a := 86;            globtdyndyntrec1array[1][1].b := 243;
      //GlobTDynDynPrec1Array: TDynDynPrec1Array;
      SetLength(GlobTDynDynPrec1Array, 3);
      SetLength(GlobTDynDynPrec1Array[0], 3);
      GlobTDynDynPrec1Array[0][0] := @GlobTDynDynTRec1Array[0][0];
      GlobTDynDynPrec1Array[0][1] := @GlobTDynDynTRec1Array[0][1];
      SetLength(GlobTDynDynPrec1Array[1], 3);
      GlobTDynDynPrec1Array[1][0] := @GlobTDynDynTRec1Array[1][0];
      GlobTDynDynPrec1Array[1][1] := @GlobTDynDynTRec1Array[1][1];
      //GlobTDynStatTRec1Array: TDynStatTRec1Array;
      SetLength(GlobTDynStatTRec1Array, 3);
      globtdynstattrec1array[0][3].a := 70;            globtdynstattrec1array[0][3].b := 243;
      globtdynstattrec1array[0][4].a := 71;            globtdynstattrec1array[0][4].b := 243;
      globtdynstattrec1array[1][3].a := 75;            globtdynstattrec1array[1][3].b := 243;
      globtdynstattrec1array[1][4].a := 76;            globtdynstattrec1array[1][4].b := 243;
      //GlobTDynStatPRec1Array: TDynStatPRec1Array;
      SetLength(GlobTDynStatPRec1Array, 3);
      GlobTDynStatPRec1Array[0][3] := @GlobTDynStatTRec1Array[0][3];
      GlobTDynStatPRec1Array[0][4] := @GlobTDynStatTRec1Array[0][4];
      GlobTDynStatPRec1Array[1][3] := @GlobTDynStatTRec1Array[1][3];
      GlobTDynStatPRec1Array[1][4] := @GlobTDynStatTRec1Array[1][4];
      //GlobTDynPDynTRec1Array: TDynPDynTRec1Array;
      SetLength(GlobTDynPDynTRec1Array, 3);
      GlobTDynPDynTRec1Array[0] := @GlobTDynDynTRec1Array[0];
      GlobTDynPDynTRec1Array[1] := @GlobTDynDynTRec1Array[1];
      //GlobTDynPStatTRec1Array: TDynPStatTRec1Array;
      SetLength(GlobTDynPStatTRec1Array, 3);
      GlobTDynPStatTRec1Array[0] := @GlobTDynStatTRec1Array[0];
      GlobTDynPStatTRec1Array[1] := @GlobTDynStatTRec1Array[1];
      //TDynPDynTRec1NPArray = array of ^TDynArrayTRec1NP;
      SetLength(GlobTDynPDynTRec1NPArray, 3);
      GlobTDynPDynTRec1NPArray[0] := @GlobTDynArrayTRec1NP;  // 500, 501
      GlobTDynPDynTRec1NPArray[1] := @GlobTDynArrayTRec1NP2; // 505, 506;
      //TDynPStatTRec1NPArray= array of ^TStatArrayTRec1NP;
      SetLength(GlobTDynPStatTRec1NPArray, 3);
      GlobTDynPStatTRec1NPArray[0] := @GlobTStatArrayTRec1NP;  // 510, 511
      GlobTDynPStatTRec1NPArray[1] := @GlobTStatArrayTRec1NP2; // 515, 516


      //GlobTDynDynTRec2Array: TDynDynTRec2Array;
      SetLength(GlobTDynDynTRec2Array, 3);
      SetLength(GlobTDynDynTRec2Array[0], 3);
      globtdyndyntrec2array[0][0].c := 80;            globtdyndyntrec2array[0][0].recs := nil;
      globtdyndyntrec2array[0][1].c := 81;            globtdyndyntrec2array[0][1].recs := nil;
      SetLength(GlobTDynDynTRec2Array[1], 3);
      globtdyndyntrec2array[1][0].c := 85;            globtdyndyntrec2array[1][0].recs := nil;
      globtdyndyntrec2array[1][1].c := 86;            globtdyndyntrec2array[1][1].recs := nil;
      //GlobTDynDynPRec2Array: TDynDynPRec2Array;
      SetLength(GlobTDynDynPRec2Array, 3);
      SetLength(GlobTDynDynPRec2Array[0], 3);
      GlobTDynDynPRec2Array[0][0] := @GlobTDynDynTRec2Array[0][0];
      GlobTDynDynPRec2Array[0][1] := @GlobTDynDynTRec2Array[0][1];
      SetLength(GlobTDynDynPRec2Array[1], 3);
      GlobTDynDynPRec2Array[1][0] := @GlobTDynDynTRec2Array[1][0];
      GlobTDynDynPRec2Array[1][1] := @GlobTDynDynTRec2Array[1][1];
      //GlobTDynDynPPrec2Array: TDynDynPPrec2Array;
      SetLength(GlobTDynDynPPrec2Array, 3);
      SetLength(GlobTDynDynPPrec2Array[0], 3);
      GlobTDynDynPPrec2Array[0][0] := @GlobTDynDynPRec2Array[0][0];
      GlobTDynDynPPrec2Array[0][1] := @GlobTDynDynPRec2Array[0][1];
      SetLength(GlobTDynDynPPrec2Array[1], 3);
      GlobTDynDynPPrec2Array[1][0] := @GlobTDynDynPRec2Array[1][0];
      GlobTDynDynPPrec2Array[1][1] := @GlobTDynDynPRec2Array[1][1];
      //GlobTDynStatTRec2Array: TDynStatTRec2Array;
      SetLength(GlobTDynStatTRec2Array, 3);
      globtdynstattrec2array[0][3].c := 70;            globtdynstattrec2array[0][3].recs := nil;
      globtdynstattrec2array[0][4].c := 71;            globtdynstattrec2array[0][4].recs := nil;
      globtdynstattrec2array[1][3].c := 75;            globtdynstattrec2array[1][3].recs := nil;
      globtdynstattrec2array[1][4].c := 76;            globtdynstattrec2array[1][4].recs := nil;
      //GlobTDynStatPRec2Array: TDynStatPRec2Array;
      SetLength(GlobTDynStatPRec2Array, 3);
      GlobTDynStatPRec2Array[0][3] := @GlobTDynStatTRec2Array[0][3];
      GlobTDynStatPRec2Array[0][4] := @GlobTDynStatTRec2Array[0][4];
      GlobTDynStatPRec2Array[1][3] := @GlobTDynStatTRec2Array[1][3];
      GlobTDynStatPRec2Array[1][4] := @GlobTDynStatTRec2Array[1][4];
      //GlobTDynStatPPRec2Array: TDynStatPPRec2Array;
      SetLength(GlobTDynStatPPRec2Array, 3);
      GlobTDynStatPPRec2Array[0][3] := @GlobTDynStatPRec2Array[0][3];
      GlobTDynStatPPRec2Array[0][4] := @GlobTDynStatPRec2Array[0][4];
      GlobTDynStatPPRec2Array[1][3] := @GlobTDynStatPRec2Array[1][3];
      GlobTDynStatPPRec2Array[1][4] := @GlobTDynStatPRec2Array[1][4];



  // stat arrays of named arrays
      //GlobTStatDynTRec1Array: TStatDynTRec1Array;
      SetLength(GlobTStatDynTRec1Array[3], 3);
      globtstatdyntrec1array[3][0].a := 80;            globtstatdyntrec1array[3][0].b := 243;
      globtstatdyntrec1array[3][1].a := 81;            globtstatdyntrec1array[3][1].b := 243;
      SetLength(GlobTStatDynTRec1Array[4], 3);
      globtstatdyntrec1array[4][0].a := 85;            globtstatdyntrec1array[4][0].b := 243;
      globtstatdyntrec1array[4][1].a := 86;            globtstatdyntrec1array[4][1].b := 243;
      //GlobTStatDynPrec1Array: TStatDynPrec1Array;
      SetLength(GlobTStatDynPrec1Array[3], 3);
      GlobTStatDynPrec1Array[3][0] := @GlobTStatDynTRec1Array[3][0];
      GlobTStatDynPrec1Array[3][1] := @GlobTStatDynTRec1Array[3][1];
      SetLength(GlobTStatDynPrec1Array[4], 3);
      GlobTStatDynPrec1Array[4][0] := @GlobTStatDynTRec1Array[4][0];
      GlobTStatDynPrec1Array[4][1] := @GlobTStatDynTRec1Array[4][1];
      //GlobTStatStatTRec1Array: TStatStatTRec1Array;
      globtstatstattrec1array[3][3].a := 70;            globtstatstattrec1array[3][3].b := 243;
      globtstatstattrec1array[3][4].a := 71;            globtstatstattrec1array[3][4].b := 243;
      globtstatstattrec1array[4][3].a := 75;            globtstatstattrec1array[4][3].b := 243;
      globtstatstattrec1array[4][4].a := 76;            globtstatstattrec1array[4][4].b := 243;
      //GlobTStatStatPRec1Array: TStatStatPRec1Array;
      GlobTStatStatPRec1Array[3][3] := @GlobTStatStatTRec1Array[3][3];
      GlobTStatStatPRec1Array[3][4] := @GlobTStatStatTRec1Array[3][4];
      GlobTStatStatPRec1Array[4][3] := @GlobTStatStatTRec1Array[4][3];
      GlobTStatStatPRec1Array[4][4] := @GlobTStatStatTRec1Array[4][4];
      //GlobTStatPDynTRec1Array: TStatPDynTRec1Array;
      GlobTStatPDynTRec1Array[3] := @GlobTStatDynTRec1Array[3];
      GlobTStatPDynTRec1Array[4] := @GlobTStatDynTRec1Array[4];
      //GlobTStatPStatTRec1Array: TStatPStatTRec1Array;
      GlobTStatPStatTRec1Array[3] := @GlobTStatStatTRec1Array[3];
      GlobTStatPStatTRec1Array[4] := @GlobTStatStatTRec1Array[4];


      //GlobTStatDynTRec2Array: TStatDynTRec2Array;
      SetLength(GlobTStatDynTRec2Array[3], 3);
      globtstatdyntrec2array[3][0].c := 80;            globtstatdyntrec2array[3][0].recs := nil;
      globtstatdyntrec2array[3][1].c := 81;            globtstatdyntrec2array[3][1].recs := nil;
      SetLength(GlobTStatDynTRec2Array[4], 3);
      globtstatdyntrec2array[4][0].c := 85;            globtstatdyntrec2array[4][0].recs := nil;
      globtstatdyntrec2array[4][1].c := 86;            globtstatdyntrec2array[4][1].recs := nil;
      //GlobTStatDynPRec2Array: TStatDynPRec2Array;
      SetLength(GlobTStatDynPRec2Array[3], 3);
      GlobTStatDynPRec2Array[3][0] := @GlobTStatDynTRec2Array[3][0];
      GlobTStatDynPRec2Array[3][1] := @GlobTStatDynTRec2Array[3][1];
      SetLength(GlobTStatDynPRec2Array[4], 3);
      GlobTStatDynPRec2Array[4][0] := @GlobTStatDynTRec2Array[4][0];
      GlobTStatDynPRec2Array[4][1] := @GlobTStatDynTRec2Array[4][1];
      //GlobTStatDynPPrec2Array: TStatDynPPrec2Array;
      SetLength(GlobTStatDynPPrec2Array[3], 3);
      GlobTStatDynPPrec2Array[3][0] := @GlobTStatDynPRec2Array[3][0];
      GlobTStatDynPPrec2Array[3][1] := @GlobTStatDynPRec2Array[3][1];
      SetLength(GlobTStatDynPPrec2Array[4], 3);
      GlobTStatDynPPrec2Array[4][0] := @GlobTStatDynPRec2Array[4][0];
      GlobTStatDynPPrec2Array[4][1] := @GlobTStatDynPRec2Array[4][1];
      //GlobTStatStatTRec2Array: TStatStatTRec2Array;
      globtstatstattrec2array[3][3].c := 70;            globtstatstattrec2array[3][3].recs := nil;
      globtstatstattrec2array[3][4].c := 71;            globtstatstattrec2array[3][4].recs := nil;
      globtstatstattrec2array[4][3].c := 75;            globtstatstattrec2array[4][3].recs := nil;
      globtstatstattrec2array[4][4].c := 76;            globtstatstattrec2array[4][4].recs := nil;
      //GlobTStatStatPRec2Array: TStatStatPRec2Array;
      GlobTStatStatPRec2Array[3][3] := @GlobTStatStatTRec2Array[3][3];
      GlobTStatStatPRec2Array[3][4] := @GlobTStatStatTRec2Array[3][4];
      GlobTStatStatPRec2Array[4][3] := @GlobTStatStatTRec2Array[4][3];
      GlobTStatStatPRec2Array[4][4] := @GlobTStatStatTRec2Array[4][4];
      //GlobTStatStatPPRec2Array: TStatStatPPRec2Array;
      GlobTStatStatPPRec2Array[3][3] := @GlobTStatStatPRec2Array[3][3];
      GlobTStatStatPPRec2Array[3][4] := @GlobTStatStatPRec2Array[3][4];
      GlobTStatStatPPRec2Array[4][3] := @GlobTStatStatPRec2Array[4][3];
      GlobTStatStatPPRec2Array[4][4] := @GlobTStatStatPRec2Array[4][4];


      globtenumarraytrec1[eaone].a      := 200;            globtenumarraytrec1[eaone].b := 243;
      globtenumarraytrec1[eatwo].a      := 201;            globtenumarraytrec1[eatwo].b := 243;
      globtsubenumarraytrec1[eatwo].a   := 205;            globtsubenumarraytrec1[eatwo].b := 243;
      globtsubenumarraytrec1[eathree].a := 205;            globtsubenumarraytrec1[eathree].b := 243;

      globtstatdualarraytrec2[3,3].c := 300;            globtstatdualarraytrec2[3,3].recs := nil;
      globtstatdualarraytrec2[3,4].c := 301;            globtstatdualarraytrec2[3,4].recs := nil;
      globtstatdualarraytrec2[4,5].c := 302;            globtstatdualarraytrec2[4,5].recs := nil;


// POINTER

      // dyn arrays
      GlobPDynArrayTRec1 := @GlobTDynArrayTRec1;
      GlobPDynArrayPRec1 := @GlobTDynArrayPRec1;
      GlobPDynDynArrayTRec1 := @GlobTDynDynArrayTRec1;
      GlobPDynDynArrayPRec1 := @GlobTDynDynArrayPRec1;
      GlobPDynStatArrayTRec1 := @GlobTDynStatArrayTRec1;
      GlobPDynStatArrayPRec1 := @GlobTDynStatArrayPRec1;

      GlobPDynArrayTRec2 := @GlobTDynArrayTRec2;
      GlobPDynArrayPRec2 := @GlobTDynArrayPRec2;
      GlobPDynArrayPPRec2 := @GlobTDynArrayPPRec2;
      GlobPDynDynArrayTRec2 := @GlobTDynDynArrayTRec2;
      GlobPDynDynArrayPRec2 := @GlobTDynDynArrayPRec2;
      GlobPDynStatArrayTRec2 := @GlobTDynStatArrayTRec2;
      GlobPDynStatArrayPRec2 := @GlobTDynStatArrayPRec2;


      // stat arrays
      GlobPStatArrayTRec1 := @GlobTStatArrayTRec1;
      GlobPStatArrayPRec1 := @GlobTStatArrayPRec1;
      GlobPStatDynArrayTRec1 := @GlobTStatDynArrayTRec1;
      GlobPStatDynArrayPRec1 := @GlobTStatDynArrayPRec1;
      GlobPStatStatArrayTRec1 := @GlobTStatStatArrayTRec1;
      GlobPStatStatArrayPRec1 := @GlobTStatStatArrayPRec1;

      GlobPStatArrayTRec2 := @GlobTStatArrayTRec2;
      GlobPStatArrayPRec2 := @GlobTStatArrayPRec2;
      GlobPStatArrayPPRec2 := @GlobTStatArrayPPRec2;
      GlobPStatDynArrayTRec2 := @GlobTStatDynArrayTRec2;
      GlobPStatDynArrayPRec2 := @GlobTStatDynArrayPRec2;
      GlobPStatStatArrayTRec2 := @GlobTStatStatArrayTRec2;
      GlobPStatStatArrayPRec2 := @GlobTStatStatArrayPRec2;


      // dyn arrays of named arrays
      GlobPDynDynTRec1Array := @GlobTDynDynTRec1Array;
      GlobPDynDynPrec1Array := @GlobTDynDynPrec1Array;
      GlobPDynStatTRec1Array := @GlobTDynStatTRec1Array;
      GlobPDynStatPRec1Array := @GlobTDynStatPRec1Array;
      GlobPDynPDynTRec1Array := @GlobTDynPDynTRec1Array;
      GlobPDynPStatTRec1Array := @GlobTDynPStatTRec1Array;

      GlobPDynDynTRec2Array := @GlobTDynDynTRec2Array;
      GlobPDynDynPrec2Array := @GlobTDynDynPrec2Array;
      GlobPDynDynPPrec2Array := @GlobTDynDynPPrec2Array;
      GlobPDynStatTRec2Array := @GlobTDynStatTRec2Array;
      GlobPDynStatPRec2Array := @GlobTDynStatPRec2Array;
      GlobPDynStatPPRec2Array := @GlobTDynStatPPRec2Array;

      // stat arrays of named arrays
      GlobPStatDynTRec1Array := @GlobTStatDynTRec1Array;
      GlobPStatDynPrec1Array := @GlobTStatDynPrec1Array;
      GlobPStatStatTRec1Array := @GlobTStatStatTRec1Array;
      GlobPStatStatPRec1Array := @GlobTStatStatPRec1Array;
      GlobPStatPDynTRec1Array := @GlobTStatPDynTRec1Array;
      GlobPStatPStatTRec1Array := @GlobTStatPStatTRec1Array;

      GlobPStatDynTRec2Array := @GlobTStatDynTRec2Array;
      GlobPStatDynPrec2Array := @GlobTStatDynPrec2Array;
      GlobPStatDynPPrec2Array := @GlobTStatDynPPrec2Array;
      GlobPStatStatTRec2Array := @GlobTStatStatTRec2Array;
      GlobPStatStatPRec2Array := @GlobTStatStatPRec2Array;
      GlobPStatStatPPRec2Array := @GlobTStatStatPPRec2Array;

      GlobPEnumArrayTRec1    := @GlobTEnumArrayTRec1;
      GlobPSubEnumArrayTRec1 := @GlobTSubEnumArrayTRec1;


// UN-NAMED types

  // dyn arrays
      //GlobADynArrayTRec1: TDynArrayTRec1;
      SetLength(GlobADynArrayTRec1, 3);
      globadynarraytrec1[0].a := 90;            globadynarraytrec1[0].b := 243;
      globadynarraytrec1[1].a := 91;            globadynarraytrec1[1].b := 243;
      //GlobADynArrayPRec1: TDynArrayPRec1;
      SetLength(GlobADynArrayPRec1, 3);
      GlobADynArrayPRec1[0] := @GlobADynArrayTRec1[0];
      GlobADynArrayPRec1[1] := @GlobADynArrayTRec1[1];
      //GlobADynDynArrayTRec1: TDynDynArrayTRec1;
      SetLength(GlobADynDynArrayTRec1, 3);
      SetLength(GlobADynDynArrayTRec1[0], 3);
      globadyndynarraytrec1[0][0].a := 80;            globadyndynarraytrec1[0][0].b := 243;
      globadyndynarraytrec1[0][1].a := 81;            globadyndynarraytrec1[0][1].b := 243;
      SetLength(GlobADynDynArrayTRec1[1], 3);
      globadyndynarraytrec1[1][0].a := 85;            globadyndynarraytrec1[1][0].b := 243;
      globadyndynarraytrec1[1][1].a := 86;            globadyndynarraytrec1[1][1].b := 243;
      //GlobADynDynArrayPRec1: TDynDynArrayPRec1;
      SetLength(GlobADynDynArrayPRec1, 3);
      SetLength(GlobADynDynArrayPRec1[0], 3);
      GlobADynDynArrayPRec1[0][0] := @GlobADynDynArrayTRec1[0][0];
      GlobADynDynArrayPRec1[0][1] := @GlobADynDynArrayTRec1[0][1];
      SetLength(GlobADynDynArrayPRec1[1], 3);
      GlobADynDynArrayPRec1[1][0] := @GlobADynDynArrayTRec1[1][0];
      GlobADynDynArrayPRec1[1][1] := @GlobADynDynArrayTRec1[1][1];
      //GlobADynStatArrayTRec1: TDynStatArrayTRec1;
      SetLength(GlobADynStatArrayTRec1, 3);
      globadynstatarraytrec1[0][3].a := 70;            globadynstatarraytrec1[0][3].b := 243;
      globadynstatarraytrec1[0][4].a := 71;            globadynstatarraytrec1[0][4].b := 243;
      globadynstatarraytrec1[1][3].a := 75;            globadynstatarraytrec1[1][3].b := 243;
      globadynstatarraytrec1[1][4].a := 76;            globadynstatarraytrec1[1][4].b := 243;
      //GlobADynStatArrayPRec1: TDynStatArrayPRec1;
      SetLength(GlobADynStatArrayPRec1, 3);
      GlobADynStatArrayPRec1[0][3] := @GlobADynStatArrayTRec1[0][3];
      GlobADynStatArrayPRec1[0][4] := @GlobADynStatArrayTRec1[0][4];
      GlobADynStatArrayPRec1[1][3] := @GlobADynStatArrayTRec1[1][3];
      GlobADynStatArrayPRec1[1][4] := @GlobADynStatArrayTRec1[1][4];

      //GlobADynArrayTRec2: TDynArrayTRec2;
      SetLength(GlobADynArrayTRec2, 3);
      globadynarraytrec2[0].c := 90;            globadynarraytrec2[0].recs := nil;
      globadynarraytrec2[1].c := 91;            globadynarraytrec2[1].recs := nil;
      //GlobADynArrayPRec2: TDynArrayPRec2;
      SetLength(GlobADynArrayPRec2, 3);
      GlobADynArrayPRec2[0] := @GlobADynArrayTRec2[0];
      GlobADynArrayPRec2[1] := @GlobADynArrayTRec2[1];
      //GlobADynDynArrayTRec2: TDynDynArrayTRec2;
      SetLength(GlobADynDynArrayTRec2, 3);
      SetLength(GlobADynDynArrayTRec2[0], 3);
      globadyndynarraytrec2[0][0].c := 80;            globadyndynarraytrec2[0][0].recs := nil;
      globadyndynarraytrec2[0][1].c := 81;            globadyndynarraytrec2[0][1].recs := nil;
      SetLength(GlobADynDynArrayTRec2[1], 3);
      globadyndynarraytrec2[1][0].c := 85;            globadyndynarraytrec2[1][0].recs := nil;
      globadyndynarraytrec2[1][1].c := 86;            globadyndynarraytrec2[1][1].recs := nil;
      //GlobADynDynArrayPRec2: TDynDynArrayPRec2;
      SetLength(GlobADynDynArrayPRec2, 3);
      SetLength(GlobADynDynArrayPRec2[0], 3);
      GlobADynDynArrayPRec2[0][0] := @GlobADynDynArrayTRec2[0][0];
      GlobADynDynArrayPRec2[0][1] := @GlobADynDynArrayTRec2[0][1];
      SetLength(GlobADynDynArrayPRec2[1], 3);
      GlobADynDynArrayPRec2[1][0] := @GlobADynDynArrayTRec2[1][0];
      GlobADynDynArrayPRec2[1][1] := @GlobADynDynArrayTRec2[1][1];
      //GlobADynStatArrayTRec2: TDynStatArrayTRec2;
      SetLength(GlobADynStatArrayTRec2, 3);
      globadynstatarraytrec2[0][3].c := 70;            globadynstatarraytrec2[0][3].recs := nil;
      globadynstatarraytrec2[0][4].c := 71;            globadynstatarraytrec2[0][4].recs := nil;
      globadynstatarraytrec2[1][3].c := 75;            globadynstatarraytrec2[1][3].recs := nil;
      globadynstatarraytrec2[1][4].c := 76;            globadynstatarraytrec2[1][4].recs := nil;
      //GlobADynStatArrayPRec2: TDynStatArrayPRec2;
      SetLength(GlobADynStatArrayPRec2, 3);
      GlobADynStatArrayPRec2[0][3] := @GlobADynStatArrayTRec2[0][3];
      GlobADynStatArrayPRec2[0][4] := @GlobADynStatArrayTRec2[0][4];
      GlobADynStatArrayPRec2[1][3] := @GlobADynStatArrayTRec2[1][3];
      GlobADynStatArrayPRec2[1][4] := @GlobADynStatArrayTRec2[1][4];


  // stat arrays
      //GlobAStatArrayTRec1: TStatArrayTRec1;
      globastatarraytrec1[3].a := 50;            globastatarraytrec1[3].b := 243;
      globastatarraytrec1[4].a := 51;            globastatarraytrec1[4].b := 243;
      //GlobAStatArrayPRec1: TStatArrayPRec1;
      GlobAStatArrayPRec1[3] := @GlobAStatArrayTRec1[3];
      GlobAStatArrayPRec1[4] := @GlobAStatArrayTRec1[4];
      //GlobAStatDynArrayTRec1: TStatDynArrayTRec1;
      SetLength(GlobAStatDynArrayTRec1[3], 3);
      globastatdynarraytrec1[3][0].a := 40;            globastatdynarraytrec1[3][0].b := 243;
      globastatdynarraytrec1[3][1].a := 41;            globastatdynarraytrec1[3][1].b := 243;
      SetLength(GlobAStatDynArrayTRec1[4], 3);
      globastatdynarraytrec1[4][0].a := 45;            globastatdynarraytrec1[4][0].b := 243;
      globastatdynarraytrec1[4][1].a := 46;            globastatdynarraytrec1[4][1].b := 243;
      //GlobAStatDynArrayPRec1: TStatDynArrayPRec1;
      SetLength(GlobAStatDynArrayPRec1[3], 3);
      GlobAStatDynArrayPRec1[3][0] := @GlobADynDynArrayTRec1[3][0];
      GlobAStatDynArrayPRec1[3][1] := @GlobADynDynArrayTRec1[3][1];
      SetLength(GlobAStatDynArrayPRec1[4], 3);
      GlobAStatDynArrayPRec1[4][0] := @GlobADynDynArrayTRec1[4][0];
      GlobAStatDynArrayPRec1[4][1] := @GlobADynDynArrayTRec1[4][1];
      //GlobAStatStatArrayTRec1: TStatStatArrayTRec1;
      globastatstatarraytrec1[3][3].a := 30;            globastatstatarraytrec1[3][3].b := 243;
      globastatstatarraytrec1[3][4].a := 31;            globastatstatarraytrec1[3][4].b := 243;
      globastatstatarraytrec1[4][3].a := 35;            globastatstatarraytrec1[4][3].b := 243;
      globastatstatarraytrec1[4][4].a := 36;            globastatstatarraytrec1[4][4].b := 243;
      //GlobAStatStatArrayPRec1: TStatStatArrayPRec1;
      GlobAStatStatArrayPRec1[3][3] := @GlobAStatStatArrayTRec1[3][3];
      GlobAStatStatArrayPRec1[3][4] := @GlobAStatStatArrayTRec1[3][4];
      GlobAStatStatArrayPRec1[4][3] := @GlobAStatStatArrayTRec1[4][3];
      GlobAStatStatArrayPRec1[4][4] := @GlobAStatStatArrayTRec1[4][4];

      //GlobAStatArrayTRec2: TStatArrayTRec2;
      globastatarraytrec2[3].c := 50;            globastatarraytrec2[3].recs := nil;
      globastatarraytrec2[4].c := 51;            globastatarraytrec2[4].recs := nil;
      //GlobAStatArrayPRec2: TStatArrayPRec2;
      GlobAStatArrayPRec2[3] := @GlobAStatArrayTRec2[3];
      GlobAStatArrayPRec2[4] := @GlobAStatArrayTRec2[4];
      //GlobAStatDynArrayTRec2: TStatDynArrayTRec2;
      SetLength(GlobAStatDynArrayTRec2[3], 3);
      globastatdynarraytrec2[3][0].c := 40;            globastatdynarraytrec2[3][0].recs := nil;
      globastatdynarraytrec2[3][1].c := 41;            globastatdynarraytrec2[3][1].recs := nil;
      SetLength(GlobAStatDynArrayTRec2[4], 3);
      globastatdynarraytrec2[4][0].c := 45;            globastatdynarraytrec2[4][0].recs := nil;
      globastatdynarraytrec2[4][1].c := 46;            globastatdynarraytrec2[4][1].recs := nil;
      //GlobAStatDynArrayPRec2: TStatDynArrayPRec2;
      SetLength(GlobAStatDynArrayPRec2[3], 3);
      GlobAStatDynArrayPRec2[3][0] := @GlobADynDynArrayTRec2[3][0];
      GlobAStatDynArrayPRec2[3][1] := @GlobADynDynArrayTRec2[3][1];
      SetLength(GlobAStatDynArrayPRec2[4], 3);
      GlobAStatDynArrayPRec2[4][0] := @GlobADynDynArrayTRec2[4][0];
      GlobAStatDynArrayPRec2[4][1] := @GlobADynDynArrayTRec2[4][1];
      //GlobAStatStatArrayTRec2: TStatStatArrayTRec2;
      globastatstatarraytrec2[3][3].c := 30;            globastatstatarraytrec2[3][3].recs := nil;
      globastatstatarraytrec2[3][4].c := 31;            globastatstatarraytrec2[3][4].recs := nil;
      globastatstatarraytrec2[4][3].c := 35;            globastatstatarraytrec2[4][3].recs := nil;
      globastatstatarraytrec2[4][4].c := 36;            globastatstatarraytrec2[4][4].recs := nil;
      //GlobAStatStatArrayPRec2: TStatStatArrayPRec2;
      GlobAStatStatArrayPRec2[3][3] := @GlobAStatStatArrayTRec2[3][3];
      GlobAStatStatArrayPRec2[3][4] := @GlobAStatStatArrayTRec2[3][4];
      GlobAStatStatArrayPRec2[4][3] := @GlobAStatStatArrayTRec2[4][3];
      GlobAStatStatArrayPRec2[4][4] := @GlobAStatStatArrayTRec2[4][4];



  // dyn arrays of named arrays
      //GlobADynDynTRec1Array: TDynDynTRec1Array;
      SetLength(GlobADynDynTRec1Array, 3);
      SetLength(GlobADynDynTRec1Array[0], 3);
      globadyndyntrec1array[0][0].a := 80;            globadyndyntrec1array[0][0].b := 243;
      globadyndyntrec1array[0][1].a := 81;            globadyndyntrec1array[0][1].b := 243;
      SetLength(GlobADynDynTRec1Array[1], 3);
      globadyndyntrec1array[1][0].a := 85;            globadyndyntrec1array[1][0].b := 243;
      globadyndyntrec1array[1][1].a := 86;            globadyndyntrec1array[1][1].b := 243;
      //GlobADynDynPrec1Array: TDynDynPrec1Array;
      SetLength(GlobADynDynPrec1Array, 3);
      SetLength(GlobADynDynPrec1Array[0], 3);
      GlobADynDynPrec1Array[0][0] := @GlobADynDynTRec1Array[0][0];
      GlobADynDynPrec1Array[0][1] := @GlobADynDynTRec1Array[0][1];
      SetLength(GlobADynDynPrec1Array[1], 3);
      GlobADynDynPrec1Array[1][0] := @GlobADynDynTRec1Array[1][0];
      GlobADynDynPrec1Array[1][1] := @GlobADynDynTRec1Array[1][1];
      //GlobADynStatTRec1Array: TDynStatTRec1Array;
      SetLength(GlobADynStatTRec1Array, 3);
      globadynstattrec1array[0][3].a := 70;            globadynstattrec1array[0][3].b := 243;
      globadynstattrec1array[0][4].a := 71;            globadynstattrec1array[0][4].b := 243;
      globadynstattrec1array[1][3].a := 75;            globadynstattrec1array[1][3].b := 243;
      globadynstattrec1array[1][4].a := 76;            globadynstattrec1array[1][4].b := 243;
      //GlobADynStatPRec1Array: TDynStatPRec1Array;
      SetLength(GlobADynStatPRec1Array, 3);
      GlobADynStatPRec1Array[0][3] := @GlobADynStatTRec1Array[0][3];
      GlobADynStatPRec1Array[0][4] := @GlobADynStatTRec1Array[0][4];
      GlobADynStatPRec1Array[1][3] := @GlobADynStatTRec1Array[1][3];
      GlobADynStatPRec1Array[1][4] := @GlobADynStatTRec1Array[1][4];
      //GlobADynPDynTRec1Array: TDynPDynTRec1Array;
      SetLength(GlobADynPDynTRec1Array, 3);
      GlobADynPDynTRec1Array[0] := @GlobADynDynTRec1Array[0];
      GlobADynPDynTRec1Array[1] := @GlobADynDynTRec1Array[1];
      //GlobADynPStatTRec1Array: TDynPStatTRec1Array;
      SetLength(GlobADynPStatTRec1Array, 3);
      GlobADynPStatTRec1Array[0] := @GlobADynStatTRec1Array[0];
      GlobADynPStatTRec1Array[1] := @GlobADynStatTRec1Array[1];
      //TDynPDynTRec1NPArray = array of ^TDynArrayTRec1NP;
      GlobPDynPDynTRec1NPArray := @GlobTDynPDynTRec1NPArray;
      //TDynPStatTRec1NPArray= array of ^TStatArrayTRec1NP;
      GlobPDynPStatTRec1NPArray := @GlobTDynPStatTRec1NPArray;


      //GlobADynDynTRec2Array: TDynDynTRec2Array;
      SetLength(GlobADynDynTRec2Array, 3);
      SetLength(GlobADynDynTRec2Array[0], 3);
      globadyndyntrec2array[0][0].c := 80;            globadyndyntrec2array[0][0].recs := nil;
      globadyndyntrec2array[0][1].c := 81;            globadyndyntrec2array[0][1].recs := nil;
      SetLength(GlobADynDynTRec2Array[1], 3);
      globadyndyntrec2array[1][0].c := 85;            globadyndyntrec2array[1][0].recs := nil;
      globadyndyntrec2array[1][1].c := 86;            globadyndyntrec2array[1][1].recs := nil;
      //GlobADynDynPRec2Array: TDynDynPRec2Array;
      SetLength(GlobADynDynPRec2Array, 3);
      SetLength(GlobADynDynPRec2Array[0], 3);
      GlobADynDynPRec2Array[0][0] := @GlobADynDynTRec2Array[0][0];
      GlobADynDynPRec2Array[0][1] := @GlobADynDynTRec2Array[0][1];
      SetLength(GlobADynDynPRec2Array[1], 3);
      GlobADynDynPRec2Array[1][0] := @GlobADynDynTRec2Array[1][0];
      GlobADynDynPRec2Array[1][1] := @GlobADynDynTRec2Array[1][1];
      //GlobADynDynPPrec2Array: TDynDynPPrec2Array;
      SetLength(GlobADynDynPPrec2Array, 3);
      SetLength(GlobADynDynPPrec2Array[0], 3);
      GlobADynDynPPrec2Array[0][0] := @GlobADynDynPRec2Array[0][0];
      GlobADynDynPPrec2Array[0][1] := @GlobADynDynPRec2Array[0][1];
      SetLength(GlobADynDynPPrec2Array[1], 3);
      GlobADynDynPPrec2Array[1][0] := @GlobADynDynPRec2Array[1][0];
      GlobADynDynPPrec2Array[1][1] := @GlobADynDynPRec2Array[1][1];
      //GlobADynStatTRec2Array: TDynStatTRec2Array;
      SetLength(GlobADynStatTRec2Array, 3);
      globadynstattrec2array[0][3].c := 70;            globadynstattrec2array[0][3].recs := nil;
      globadynstattrec2array[0][4].c := 71;            globadynstattrec2array[0][4].recs := nil;
      globadynstattrec2array[1][3].c := 75;            globadynstattrec2array[1][3].recs := nil;
      globadynstattrec2array[1][4].c := 76;            globadynstattrec2array[1][4].recs := nil;
      //GlobADynStatPRec2Array: TDynStatPRec2Array;
      SetLength(GlobADynStatPRec2Array, 3);
      GlobADynStatPRec2Array[0][3] := @GlobADynStatTRec2Array[0][3];
      GlobADynStatPRec2Array[0][4] := @GlobADynStatTRec2Array[0][4];
      GlobADynStatPRec2Array[1][3] := @GlobADynStatTRec2Array[1][3];
      GlobADynStatPRec2Array[1][4] := @GlobADynStatTRec2Array[1][4];
      //GlobADynStatPPRec2Array: TDynStatPPRec2Array;
      SetLength(GlobADynStatPPRec2Array, 3);
      GlobADynStatPPRec2Array[0][3] := @GlobADynStatPRec2Array[0][3];
      GlobADynStatPPRec2Array[0][4] := @GlobADynStatPRec2Array[0][4];
      GlobADynStatPPRec2Array[1][3] := @GlobADynStatPRec2Array[1][3];
      GlobADynStatPPRec2Array[1][4] := @GlobADynStatPRec2Array[1][4];



  // stat arrays of named arrays
      //GlobAStatDynTRec1Array: TStatDynTRec1Array;
      SetLength(GlobAStatDynTRec1Array, 3);
      SetLength(GlobAStatDynTRec1Array[0], 3);
      globastatdyntrec1array[0][0].a := 80;            globastatdyntrec1array[0][0].b := 243;
      globastatdyntrec1array[0][1].a := 81;            globastatdyntrec1array[0][1].b := 243;
      SetLength(GlobAStatDynTRec1Array[1], 3);
      globastatdyntrec1array[1][0].a := 85;            globastatdyntrec1array[1][0].b := 243;
      globastatdyntrec1array[1][1].a := 86;            globastatdyntrec1array[1][1].b := 243;
      //GlobAStatDynPrec1Array: TStatDynPrec1Array;
      SetLength(GlobAStatDynPrec1Array, 3);
      SetLength(GlobAStatDynPrec1Array[0], 3);
      GlobAStatDynPrec1Array[0][0] := @GlobAStatDynTRec1Array[0][0];
      GlobAStatDynPrec1Array[0][1] := @GlobAStatDynTRec1Array[0][1];
      SetLength(GlobAStatDynPrec1Array[1], 3);
      GlobAStatDynPrec1Array[1][0] := @GlobAStatDynTRec1Array[1][0];
      GlobAStatDynPrec1Array[1][1] := @GlobAStatDynTRec1Array[1][1];
      //GlobAStatStatTRec1Array: TStatStatTRec1Array;
      SetLength(GlobAStatStatTRec1Array, 3);
      globastatstattrec1array[0][3].a := 70;            globastatstattrec1array[0][3].b := 243;
      globastatstattrec1array[0][4].a := 71;            globastatstattrec1array[0][4].b := 243;
      globastatstattrec1array[1][3].a := 75;            globastatstattrec1array[1][3].b := 243;
      globastatstattrec1array[1][4].a := 76;            globastatstattrec1array[1][4].b := 243;
      //GlobAStatStatPRec1Array: TStatStatPRec1Array;
      SetLength(GlobAStatStatPRec1Array, 3);
      GlobAStatStatPRec1Array[0][3] := @GlobAStatStatTRec1Array[0][3];
      GlobAStatStatPRec1Array[0][4] := @GlobAStatStatTRec1Array[0][4];
      GlobAStatStatPRec1Array[1][3] := @GlobAStatStatTRec1Array[1][3];
      GlobAStatStatPRec1Array[1][4] := @GlobAStatStatTRec1Array[1][4];
      //GlobAStatPDynTRec1Array: TStatPDynTRec1Array;
      SetLength(GlobAStatPDynTRec1Array, 3);
      GlobAStatPDynTRec1Array[0] := @GlobAStatDynTRec1Array[0];
      GlobAStatPDynTRec1Array[1] := @GlobAStatDynTRec1Array[1];
      //GlobAStatPStatTRec1Array: TStatPStatTRec1Array;
      SetLength(GlobAStatPStatTRec1Array, 3);
      GlobAStatPStatTRec1Array[0] := @GlobAStatStatTRec1Array[0];
      GlobAStatPStatTRec1Array[1] := @GlobAStatStatTRec1Array[1];


      //GlobAStatDynTRec2Array: TStatDynTRec2Array;
      SetLength(GlobAStatDynTRec2Array, 3);
      SetLength(GlobAStatDynTRec2Array[0], 3);
      globastatdyntrec2array[0][0].c := 80;            globastatdyntrec2array[0][0].recs := nil;
      globastatdyntrec2array[0][1].c := 81;            globastatdyntrec2array[0][1].recs := nil;
      SetLength(GlobAStatDynTRec2Array[1], 3);
      globastatdyntrec2array[1][0].c := 85;            globastatdyntrec2array[1][0].recs := nil;
      globastatdyntrec2array[1][1].c := 86;            globastatdyntrec2array[1][1].recs := nil;
      //GlobAStatDynPRec2Array: TStatDynPRec2Array;
      SetLength(GlobAStatDynPRec2Array, 3);
      SetLength(GlobAStatDynPRec2Array[0], 3);
      GlobAStatDynPRec2Array[0][0] := @GlobAStatDynTRec2Array[0][0];
      GlobAStatDynPRec2Array[0][1] := @GlobAStatDynTRec2Array[0][1];
      SetLength(GlobAStatDynPRec2Array[1], 3);
      GlobAStatDynPRec2Array[1][0] := @GlobAStatDynTRec2Array[1][0];
      GlobAStatDynPRec2Array[1][1] := @GlobAStatDynTRec2Array[1][1];
      //GlobAStatDynPPrec2Array: TStatDynPPrec2Array;
      SetLength(GlobAStatDynPPrec2Array, 3);
      SetLength(GlobAStatDynPPrec2Array[0], 3);
      GlobAStatDynPPrec2Array[0][0] := @GlobAStatDynPRec2Array[0][0];
      GlobAStatDynPPrec2Array[0][1] := @GlobAStatDynPRec2Array[0][1];
      SetLength(GlobAStatDynPPrec2Array[1], 3);
      GlobAStatDynPPrec2Array[1][0] := @GlobAStatDynPRec2Array[1][0];
      GlobAStatDynPPrec2Array[1][1] := @GlobAStatDynPRec2Array[1][1];
      //GlobAStatStatTRec2Array: TStatStatTRec2Array;
      SetLength(GlobAStatStatTRec2Array, 3);
      globastatstattrec2array[0][3].c := 70;            globastatstattrec2array[0][3].recs := nil;
      globastatstattrec2array[0][4].c := 71;            globastatstattrec2array[0][4].recs := nil;
      globastatstattrec2array[1][3].c := 75;            globastatstattrec2array[1][3].recs := nil;
      globastatstattrec2array[1][4].c := 76;            globastatstattrec2array[1][4].recs := nil;
      //GlobAStatStatPRec2Array: TStatStatPRec2Array;
      SetLength(GlobAStatStatPRec2Array, 3);
      GlobAStatStatPRec2Array[0][3] := @GlobAStatStatTRec2Array[0][3];
      GlobAStatStatPRec2Array[0][4] := @GlobAStatStatTRec2Array[0][4];
      GlobAStatStatPRec2Array[1][3] := @GlobAStatStatTRec2Array[1][3];
      GlobAStatStatPRec2Array[1][4] := @GlobAStatStatTRec2Array[1][4];
      //GlobAStatStatPPRec2Array: TStatStatPPRec2Array;
      SetLength(GlobAStatStatPPRec2Array, 3);
      GlobAStatStatPPRec2Array[0][3] := @GlobAStatStatPRec2Array[0][3];
      GlobAStatStatPPRec2Array[0][4] := @GlobAStatStatPRec2Array[0][4];
      GlobAStatStatPPRec2Array[1][3] := @GlobAStatStatPRec2Array[1][3];
      GlobAStatStatPPRec2Array[1][4] := @GlobAStatStatPRec2Array[1][4];

      SetLength(GlobADynDynDynArrayTRec1,3);
      GlobADynDynDynArrayTRec1[0] := GlobADynDynArrayTRec1;
      GlobADynDynDynArrayTRec1[1] := GlobADynDynArrayTRec1;

      SetLength(GlobADynDynDynDynArrayTRec1,3);
      GlobADynDynDynDynArrayTRec1[0] := GlobADynDynDynArrayTRec1;
      GlobADynDynDynDynArrayTRec1[1] := GlobADynDynDynArrayTRec1;

      SetLength(GlobADynArrayTRec5, 3);
      globadynarraytrec5[0].a := 10;
      globadynarraytrec5[0].c := 12;
      globadynarraytrec5[1].a := 11;
      globadynarraytrec5[1].c := 13;

      globastatarraytrec5[3].a := 15;
      globastatarraytrec5[3].c := 17;
      globastatarraytrec5[4].a := 16;
      globastatarraytrec5[4].c := 18;

      globaenumarraytrec1[eaone].a      := 200;            globaenumarraytrec1[eaone].b := 243;
      globaenumarraytrec1[eatwo].a      := 201;            globaenumarraytrec1[eatwo].b := 243;
      globasubenumarraytrec1[eatwo].a   := 205;            globasubenumarraytrec1[eatwo].b := 243;
      globasubenumarraytrec1[eathree].a := 205;            globasubenumarraytrec1[eathree].b := 243;

      globastatdualarraytrec2[3,3].c := 300;            globastatdualarraytrec2[3,3].recs := nil;
      globastatdualarraytrec2[3,4].c := 301;            globastatdualarraytrec2[3,4].recs := nil;
      globastatdualarraytrec2[4,3].c := 302;            globastatdualarraytrec2[4,3].recs := nil;


      SetLength(GlobAStatDynDynArrayTRec2[3], 3);
      SetLength(GlobAStatDynDynArrayTRec2[4], 3);

      SetLength(GlobADynDynStatArrayTRec2, 3);

    {$ENDIF}

    {$IFDEF Global_Body_NIL}
    //begin
    {$ENDIF}

  {%endregion}


