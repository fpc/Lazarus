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
      TDynDynPrec1Array   = array of TDynArrayPRec1;
      TDynStatTRec1Array  = array of TStatArrayTRec1;
      TDynStatPRec1Array  = array of TStatArrayPRec1;
      TDynPDynTRec1Array   = array of ^TDynArrayTRec1;
      TDynPStatTRec1Array  = array of ^TStatArrayTRec1;

      TDynDynTRec2Array   = array of TDynArrayTRec2;
      TDynDynPrec2Array   = array of TDynArrayPRec2;
      TDynDynPPrec2Array  = array of TDynArrayPPRec2; // double pointer
      TDynStatTRec2Array  = array of TStatArrayTRec2;
      TDynStatPRec2Array  = array of TStatArrayPRec2;
      TDynStatPPRec2Array = array of TStatArrayPPRec2; // double pointer

      // stat arrays of named arrays
      TStatDynTRec1Array   = array of TDynArrayTRec1;
      TStatDynPrec1Array   = array of TDynArrayPRec1;
      TStatStatTRec1Array  = array of TStatArrayTRec1;
      TStatStatPRec1Array  = array of TStatArrayPRec1;
      TStatPDynTRec1Array   = array of ^TDynArrayTRec1;
      TStatPStatTRec1Array  = array of ^TStatArrayTRec1;

      TStatDynTRec2Array   = array of TDynArrayTRec2;
      TStatDynPrec2Array   = array of TDynArrayPRec2;
      TStatDynPPrec2Array  = array of TDynArrayPPRec2; // double pointer
      TStatStatTRec2Array  = array of TStatArrayTRec2;
      TStatStatPRec2Array  = array of TStatArrayPRec2;
      TStatStatPPRec2Array = array of TStatArrayPPRec2; // double pointer


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
      ArgTDynArrayTRec1: TDynArrayTRec1;            VArgTDynArrayTRec1: TDynArrayTRec1;
      ArgTDynArrayPRec1: TDynArrayPRec1;            VArgTDynArrayPRec1: TDynArrayPRec1;
      ArgTDynDynArrayTRec1: TDynDynArrayTRec1;      VArgTDynDynArrayTRec1: TDynDynArrayTRec1;
      ArgTDynDynArrayPRec1: TDynDynArrayPRec1;      VArgTDynDynArrayPRec1: TDynDynArrayPRec1;
      ArgTDynStatArrayTRec1: TDynStatArrayTRec1;    VArgTDynStatArrayTRec1: TDynStatArrayTRec1;
      ArgTDynStatArrayPRec1: TDynStatArrayPRec1;    VArgTDynStatArrayPRec1: TDynStatArrayPRec1;

      ArgTDynArrayTRec2: TDynArrayTRec2;            VArgTDynArrayTRec2: TDynArrayTRec2;
      ArgTDynArrayPRec2: TDynArrayPRec2;            VArgTDynArrayPRec2: TDynArrayPRec2;
      ArgTDynArrayPPRec2: TDynArrayPPRec2;          VArgTDynArrayPPRec2: TDynArrayPPRec2;
      ArgTDynDynArrayTRec2: TDynDynArrayTRec2;      VArgTDynDynArrayTRec2: TDynDynArrayTRec2;
      ArgTDynDynArrayPRec2: TDynDynArrayPRec2;      VArgTDynDynArrayPRec2: TDynDynArrayPRec2;
      ArgTDynStatArrayTRec2: TDynStatArrayTRec2;    VArgTDynStatArrayTRec2: TDynStatArrayTRec2;
      ArgTDynStatArrayPRec2: TDynStatArrayPRec2;    VArgTDynStatArrayPRec2: TDynStatArrayPRec2;


      // stat arrays
      ArgTStatArrayTRec1: TStatArrayTRec1;          VArgTStatArrayTRec1: TStatArrayTRec1;
      ArgTStatArrayPRec1: TStatArrayPRec1;          VArgTStatArrayPRec1: TStatArrayPRec1;
      ArgTStatDynArrayTRec1: TStatDynArrayTRec1;    VArgTStatDynArrayTRec1: TStatDynArrayTRec1;
      ArgTStatDynArrayPRec1: TStatDynArrayPRec1;    VArgTStatDynArrayPRec1: TStatDynArrayPRec1;
      ArgTStatStatArrayTRec1: TStatStatArrayTRec1;  VArgTStatStatArrayTRec1: TStatStatArrayTRec1;
      ArgTStatStatArrayPRec1: TStatStatArrayPRec1;  VArgTStatStatArrayPRec1: TStatStatArrayPRec1;

      ArgTStatArrayTRec2: TStatArrayTRec2;          VArgTStatArrayTRec2: TStatArrayTRec2;
      ArgTStatArrayPRec2: TStatArrayPRec2;          VArgTStatArrayPRec2: TStatArrayPRec2;
      ArgTStatArrayPPRec2: TStatArrayPPRec2;        VArgTStatArrayPPRec2: TStatArrayPPRec2;
      ArgTStatDynArrayTRec2: TStatDynArrayTRec2;    VArgTStatDynArrayTRec2: TStatDynArrayTRec2;
      ArgTStatDynArrayPRec2: TStatDynArrayPRec2;    VArgTStatDynArrayPRec2: TStatDynArrayPRec2;
      ArgTStatStatArrayTRec2: TStatStatArrayTRec2;  VArgTStatStatArrayTRec2: TStatStatArrayTRec2;
      ArgTStatStatArrayPRec2: TStatStatArrayPRec2;  VArgTStatStatArrayPRec2: TStatStatArrayPRec2;


      // dyn arrays of named arrays
      ArgTDynDynTRec1Array: TDynDynTRec1Array;      VArgTDynDynTRec1Array: TDynDynTRec1Array;
      ArgTDynDynPrec1Array: TDynDynPrec1Array;      VArgTDynDynPrec1Array: TDynDynPrec1Array;
      ArgTDynStatTRec1Array: TDynStatTRec1Array;    VArgTDynStatTRec1Array: TDynStatTRec1Array;
      ArgTDynStatPRec1Array: TDynStatPRec1Array;    VArgTDynStatPRec1Array: TDynStatPRec1Array;
      ArgTDynPDynTRec1Array: TDynPDynTRec1Array;     VArgTDynPDynTRec1Array: TDynPDynTRec1Array;
      ArgTDynPStatTRec1Array: TDynPStatTRec1Array;   VArgTDynPStatTRec1Array: TDynPStatTRec1Array;

      ArgTDynDynTRec2Array: TDynDynTRec2Array;      VArgTDynDynTRec2Array: TDynDynTRec2Array;
      ArgTDynDynPrec2Array: TDynDynPrec2Array;      VArgTDynDynPrec2Array: TDynDynPrec2Array;
      ArgTDynDynPPrec2Array: TDynDynPPrec2Array;    VArgTDynDynPPrec2Array: TDynDynPPrec2Array;
      ArgTDynStatTRec2Array: TDynStatTRec2Array;    VArgTDynStatTRec2Array: TDynStatTRec2Array;
      ArgTDynStatPRec2Array: TDynStatPRec2Array;    VArgTDynStatPRec2Array: TDynStatPRec2Array;
      ArgTDynStatPPRec2Array: TDynStatPPRec2Array;  VArgTDynStatPPRec2Array: TDynStatPPRec2Array;

      // stat arrays of named arrays
      ArgTStatDynTRec1Array: TStatDynTRec1Array;      VArgTStatDynTRec1Array: TStatDynTRec1Array;
      ArgTStatDynPrec1Array: TStatDynPrec1Array;      VArgTStatDynPrec1Array: TStatDynPrec1Array;
      ArgTStatStatTRec1Array: TStatStatTRec1Array;    VArgTStatStatTRec1Array: TStatStatTRec1Array;
      ArgTStatStatPRec1Array: TStatStatPRec1Array;    VArgTStatStatPRec1Array: TStatStatPRec1Array;
      ArgTStatPDynTRec1Array: TStatPDynTRec1Array;     VArgTStatPDynTRec1Array: TStatPDynTRec1Array;
      ArgTStatPStatTRec1Array: TStatPStatTRec1Array;   VArgTStatPStatTRec1Array: TStatPStatTRec1Array;

      ArgTStatDynTRec2Array: TStatDynTRec2Array;      VArgTStatDynTRec2Array: TStatDynTRec2Array;
      ArgTStatDynPrec2Array: TStatDynPrec2Array;      VArgTStatDynPrec2Array: TStatDynPrec2Array;
      ArgTStatDynPPrec2Array: TStatDynPPrec2Array;    VArgTStatDynPPrec2Array: TStatDynPPrec2Array;
      ArgTStatStatTRec2Array: TStatStatTRec2Array;    VArgTStatStatTRec2Array: TStatStatTRec2Array;
      ArgTStatStatPRec2Array: TStatStatPRec2Array;    VArgTStatStatPRec2Array: TStatStatPRec2Array;
      ArgTStatStatPPRec2Array: TStatStatPPRec2Array;  VArgTStatStatPPRec2Array: TStatStatPPRec2Array;

// POINTER

      // dyn arrays
      ArgPDynArrayTRec1: PDynArrayTRec1;            VArgPDynArrayTRec1: PDynArrayTRec1;
      ArgPDynArrayPRec1: PDynArrayPRec1;            VArgPDynArrayPRec1: PDynArrayPRec1;
      ArgPDynDynArrayTRec1: PDynDynArrayTRec1;      VArgPDynDynArrayTRec1: PDynDynArrayTRec1;
      ArgPDynDynArrayPRec1: PDynDynArrayPRec1;      VArgPDynDynArrayPRec1: PDynDynArrayPRec1;
      ArgPDynStatArrayTRec1: PDynStatArrayTRec1;    VArgPDynStatArrayTRec1: PDynStatArrayTRec1;
      ArgPDynStatArrayPRec1: PDynStatArrayPRec1;    VArgPDynStatArrayPRec1: PDynStatArrayPRec1;

      ArgPDynArrayTRec2: PDynArrayTRec2;            VArgPDynArrayTRec2: PDynArrayTRec2;
      ArgPDynArrayPRec2: PDynArrayPRec2;            VArgPDynArrayPRec2: PDynArrayPRec2;
      ArgPDynArrayPPRec2: PDynArrayPPRec2;          VArgPDynArrayPPRec2: PDynArrayPPRec2;
      ArgPDynDynArrayTRec2: PDynDynArrayTRec2;      VArgPDynDynArrayTRec2: PDynDynArrayTRec2;
      ArgPDynDynArrayPRec2: PDynDynArrayPRec2;      VArgPDynDynArrayPRec2: PDynDynArrayPRec2;
      ArgPDynStatArrayTRec2: PDynStatArrayTRec2;    VArgPDynStatArrayTRec2: PDynStatArrayTRec2;
      ArgPDynStatArrayPRec2: PDynStatArrayPRec2;    VArgPDynStatArrayPRec2: PDynStatArrayPRec2;


      // stat arrays
      ArgPStatArrayTRec1: PStatArrayTRec1;          VArgPStatArrayTRec1: PStatArrayTRec1;
      ArgPStatArrayPRec1: PStatArrayPRec1;          VArgPStatArrayPRec1: PStatArrayPRec1;
      ArgPStatDynArrayTRec1: PStatDynArrayTRec1;    VArgPStatDynArrayTRec1: PStatDynArrayTRec1;
      ArgPStatDynArrayPRec1: PStatDynArrayPRec1;    VArgPStatDynArrayPRec1: PStatDynArrayPRec1;
      ArgPStatStatArrayTRec1: PStatStatArrayTRec1;  VArgPStatStatArrayTRec1: PStatStatArrayTRec1;
      ArgPStatStatArrayPRec1: PStatStatArrayPRec1;  VArgPStatStatArrayPRec1: PStatStatArrayPRec1;

      ArgPStatArrayTRec2: PStatArrayTRec2;          VArgPStatArrayTRec2: PStatArrayTRec2;
      ArgPStatArrayPRec2: PStatArrayPRec2;          VArgPStatArrayPRec2: PStatArrayPRec2;
      ArgPStatArrayPPRec2: PStatArrayPPRec2;        VArgPStatArrayPPRec2: PStatArrayPPRec2;
      ArgPStatDynArrayTRec2: PStatDynArrayTRec2;    VArgPStatDynArrayTRec2: PStatDynArrayTRec2;
      ArgPStatDynArrayPRec2: PStatDynArrayPRec2;    VArgPStatDynArrayPRec2: PStatDynArrayPRec2;
      ArgPStatStatArrayTRec2: PStatStatArrayTRec2;  VArgPStatStatArrayTRec2: PStatStatArrayTRec2;
      ArgPStatStatArrayPRec2: PStatStatArrayPRec2;  VArgPStatStatArrayPRec2: PStatStatArrayPRec2;


      // dyn arrays of named arrays
      ArgPDynDynTRec1Array: PDynDynTRec1Array;      VArgPDynDynTRec1Array: PDynDynTRec1Array;
      ArgPDynDynPrec1Array: PDynDynPrec1Array;      VArgPDynDynPrec1Array: PDynDynPrec1Array;
      ArgPDynStatTRec1Array: PDynStatTRec1Array;    VArgPDynStatTRec1Array: PDynStatTRec1Array;
      ArgPDynStatPRec1Array: PDynStatPRec1Array;    VArgPDynStatPRec1Array: PDynStatPRec1Array;
      ArgPDynPDynTRec1Array: PDynPDynTRec1Array;     VArgPDynPDynTRec1Array: PDynPDynTRec1Array;
      ArgPDynPStatTRec1Array: PDynPStatTRec1Array;   VArgPDynPStatTRec1Array: PDynPStatTRec1Array;

      ArgPDynDynTRec2Array: PDynDynTRec2Array;      VArgPDynDynTRec2Array: PDynDynTRec2Array;
      ArgPDynDynPrec2Array: PDynDynPrec2Array;      VArgPDynDynPrec2Array: PDynDynPrec2Array;
      ArgPDynDynPPrec2Array: PDynDynPPrec2Array;    VArgPDynDynPPrec2Array: PDynDynPPrec2Array;
      ArgPDynStatTRec2Array: PDynStatTRec2Array;    VArgPDynStatTRec2Array: PDynStatTRec2Array;
      ArgPDynStatPRec2Array: PDynStatPRec2Array;    VArgPDynStatPRec2Array: PDynStatPRec2Array;
      ArgPDynStatPPRec2Array: PDynStatPPRec2Array;  VArgPDynStatPPRec2Array: PDynStatPPRec2Array;

      // stat arrays of named arrays
      ArgPStatDynTRec1Array: PStatDynTRec1Array;      VArgPStatDynTRec1Array: PStatDynTRec1Array;
      ArgPStatDynPrec1Array: PStatDynPrec1Array;      VArgPStatDynPrec1Array: PStatDynPrec1Array;
      ArgPStatStatTRec1Array: PStatStatTRec1Array;    VArgPStatStatTRec1Array: PStatStatTRec1Array;
      ArgPStatStatPRec1Array: PStatStatPRec1Array;    VArgPStatStatPRec1Array: PStatStatPRec1Array;
      ArgPStatPDynTRec1Array: PStatPDynTRec1Array;     VArgPStatPDynTRec1Array: PStatPDynTRec1Array;
      ArgPStatPStatTRec1Array: PStatPStatTRec1Array;   VArgPStatPStatTRec1Array: PStatPStatTRec1Array;

      ArgPStatDynTRec2Array: PStatDynTRec2Array;      VArgPStatDynTRec2Array: PStatDynTRec2Array;
      ArgPStatDynPrec2Array: PStatDynPrec2Array;      VArgPStatDynPrec2Array: PStatDynPrec2Array;
      ArgPStatDynPPrec2Array: PStatDynPPrec2Array;    VArgPStatDynPPrec2Array: PStatDynPPrec2Array;
      ArgPStatStatTRec2Array: PStatStatTRec2Array;    VArgPStatStatTRec2Array: PStatStatTRec2Array;
      ArgPStatStatPRec2Array: PStatStatPRec2Array;    VArgPStatStatPRec2Array: PStatStatPRec2Array;
      ArgPStatStatPPRec2Array: PStatStatPPRec2Array;  VArgPStatStatPPRec2Array: PStatStatPPRec2Array;

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


  // dyn arrays
      //GlobTDynArrayTRec1: TDynArrayTRec1;
      SetLength(GlobTDynArrayTRec1, 3);
      GlobTDynArrayTRec1[0].a := 90;
      GlobTDynArrayTRec1[1].a := 91;
      //GlobTDynArrayPRec1: TDynArrayPRec1;
      SetLength(GlobTDynArrayPRec1, 3);
      GlobTDynArrayPRec1[0] := @GlobTDynArrayTRec1[0];
      GlobTDynArrayPRec1[1] := @GlobTDynArrayTRec1[1];
      //GlobTDynDynArrayTRec1: TDynDynArrayTRec1;
      SetLength(GlobTDynDynArrayTRec1, 3);
      SetLength(GlobTDynDynArrayTRec1[0], 3);
      GlobTDynDynArrayTRec1[0][0].a := 80;
      GlobTDynDynArrayTRec1[0][1].a := 81;
      SetLength(GlobTDynDynArrayTRec1[1], 3);
      GlobTDynDynArrayTRec1[1][0].a := 85;
      GlobTDynDynArrayTRec1[1][1].a := 86;
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
      GlobTDynStatArrayTRec1[0][3].a := 70;
      GlobTDynStatArrayTRec1[0][4].a := 71;
      GlobTDynStatArrayTRec1[1][3].a := 75;
      GlobTDynStatArrayTRec1[1][4].a := 76;
      //GlobTDynStatArrayPRec1: TDynStatArrayPRec1;
      SetLength(GlobTDynStatArrayPRec1, 3);
      GlobTDynStatArrayPRec1[0][3] := @GlobTDynStatArrayTRec1[0][3];
      GlobTDynStatArrayPRec1[0][4] := @GlobTDynStatArrayTRec1[0][4];
      GlobTDynStatArrayPRec1[1][3] := @GlobTDynStatArrayTRec1[1][3];
      GlobTDynStatArrayPRec1[1][4] := @GlobTDynStatArrayTRec1[1][4];

      //GlobTDynArrayTRec2: TDynArrayTRec2;
      SetLength(GlobTDynArrayTRec2, 3);
      GlobTDynArrayTRec2[0].c := 90;
      GlobTDynArrayTRec2[1].c := 91;
      //GlobTDynArrayPRec2: TDynArrayPRec2;
      SetLength(GlobTDynArrayPRec2, 3);
      GlobTDynArrayPRec2[0] := @GlobTDynArrayTRec2[0];
      GlobTDynArrayPRec2[1] := @GlobTDynArrayTRec2[1];
      //GlobTDynDynArrayTRec2: TDynDynArrayTRec2;
      SetLength(GlobTDynDynArrayTRec2, 3);
      SetLength(GlobTDynDynArrayTRec2[0], 3);
      GlobTDynDynArrayTRec2[0][0].c := 80;
      GlobTDynDynArrayTRec2[0][1].c := 81;
      SetLength(GlobTDynDynArrayTRec2[1], 3);
      GlobTDynDynArrayTRec2[1][0].c := 85;
      GlobTDynDynArrayTRec2[1][1].c := 86;
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
      GlobTDynStatArrayTRec2[0][3].c := 70;
      GlobTDynStatArrayTRec2[0][4].c := 71;
      GlobTDynStatArrayTRec2[1][3].c := 75;
      GlobTDynStatArrayTRec2[1][4].c := 76;
      //GlobTDynStatArrayPRec2: TDynStatArrayPRec2;
      SetLength(GlobTDynStatArrayPRec2, 3);
      GlobTDynStatArrayPRec2[0][3] := @GlobTDynStatArrayTRec2[0][3];
      GlobTDynStatArrayPRec2[0][4] := @GlobTDynStatArrayTRec2[0][4];
      GlobTDynStatArrayPRec2[1][3] := @GlobTDynStatArrayTRec2[1][3];
      GlobTDynStatArrayPRec2[1][4] := @GlobTDynStatArrayTRec2[1][4];


  // stat arrays
      //GlobTStatArrayTRec1: TStatArrayTRec1;
      GlobTStatArrayTRec1[3].a := 50;
      GlobTStatArrayTRec1[4].a := 51;
      //GlobTStatArrayPRec1: TStatArrayPRec1;
      GlobTStatArrayPRec1[3] := @GlobTStatArrayTRec1[3];
      GlobTStatArrayPRec1[4] := @GlobTStatArrayTRec1[4];
      //GlobTStatDynArrayTRec1: TStatDynArrayTRec1;
      SetLength(GlobTStatDynArrayTRec1[3], 3);
      GlobTStatDynArrayTRec1[3][0].a := 40;
      GlobTStatDynArrayTRec1[3][1].a := 41;
      SetLength(GlobTStatDynArrayTRec1[4], 3);
      GlobTStatDynArrayTRec1[4][0].a := 45;
      GlobTStatDynArrayTRec1[4][1].a := 46;
      //GlobTStatDynArrayPRec1: TStatDynArrayPRec1;
      SetLength(GlobTStatDynArrayPRec1[3], 3);
      GlobTStatDynArrayPRec1[3][0] := @GlobTDynDynArrayTRec1[3][0];
      GlobTStatDynArrayPRec1[3][1] := @GlobTDynDynArrayTRec1[3][1];
      SetLength(GlobTStatDynArrayPRec1[4], 3);
      GlobTStatDynArrayPRec1[4][0] := @GlobTDynDynArrayTRec1[4][0];
      GlobTStatDynArrayPRec1[4][1] := @GlobTDynDynArrayTRec1[4][1];
      //GlobTStatStatArrayTRec1: TStatStatArrayTRec1;
      GlobTStatStatArrayTRec1[3][3].a := 30;
      GlobTStatStatArrayTRec1[3][4].a := 31;
      GlobTStatStatArrayTRec1[4][3].a := 35;
      GlobTStatStatArrayTRec1[4][4].a := 36;
      //GlobTStatStatArrayPRec1: TStatStatArrayPRec1;
      GlobTStatStatArrayPRec1[3][3] := @GlobTStatStatArrayTRec1[3][3];
      GlobTStatStatArrayPRec1[3][4] := @GlobTStatStatArrayTRec1[3][4];
      GlobTStatStatArrayPRec1[4][3] := @GlobTStatStatArrayTRec1[4][3];
      GlobTStatStatArrayPRec1[4][4] := @GlobTStatStatArrayTRec1[4][4];

      //GlobTStatArrayTRec2: TStatArrayTRec2;
      GlobTStatArrayTRec2[3].c := 50;
      GlobTStatArrayTRec2[4].c := 51;
      //GlobTStatArrayPRec2: TStatArrayPRec2;
      GlobTStatArrayPRec2[3] := @GlobTStatArrayTRec2[3];
      GlobTStatArrayPRec2[4] := @GlobTStatArrayTRec2[4];
      //GlobTStatDynArrayTRec2: TStatDynArrayTRec2;
      SetLength(GlobTStatDynArrayTRec2[3], 3);
      GlobTStatDynArrayTRec2[3][0].c := 40;
      GlobTStatDynArrayTRec2[3][1].c := 41;
      SetLength(GlobTStatDynArrayTRec2[4], 3);
      GlobTStatDynArrayTRec2[4][0].c := 45;
      GlobTStatDynArrayTRec2[4][1].c := 46;
      //GlobTStatDynArrayPRec2: TStatDynArrayPRec2;
      SetLength(GlobTStatDynArrayPRec2[3], 3);
      GlobTStatDynArrayPRec2[3][0] := @GlobTDynDynArrayTRec2[3][0];
      GlobTStatDynArrayPRec2[3][1] := @GlobTDynDynArrayTRec2[3][1];
      SetLength(GlobTStatDynArrayPRec2[4], 3);
      GlobTStatDynArrayPRec2[4][0] := @GlobTDynDynArrayTRec2[4][0];
      GlobTStatDynArrayPRec2[4][1] := @GlobTDynDynArrayTRec2[4][1];
      //GlobTStatStatArrayTRec2: TStatStatArrayTRec2;
      GlobTStatStatArrayTRec2[3][3].c := 30;
      GlobTStatStatArrayTRec2[3][4].c := 31;
      GlobTStatStatArrayTRec2[4][3].c := 35;
      GlobTStatStatArrayTRec2[4][4].c := 36;
      //GlobTStatStatArrayPRec2: TStatStatArrayPRec2;
      GlobTStatStatArrayPRec2[3][3] := @GlobTStatStatArrayTRec2[3][3];
      GlobTStatStatArrayPRec2[3][4] := @GlobTStatStatArrayTRec2[3][4];
      GlobTStatStatArrayPRec2[4][3] := @GlobTStatStatArrayTRec2[4][3];
      GlobTStatStatArrayPRec2[4][4] := @GlobTStatStatArrayTRec2[4][4];



  // dyn arrays of named arrays
      //GlobTDynDynTRec1Array: TDynDynTRec1Array;
      SetLength(GlobTDynDynTRec1Array, 3);
      SetLength(GlobTDynDynTRec1Array[0], 3);
      GlobTDynDynTRec1Array[0][0].a := 80;
      GlobTDynDynTRec1Array[0][1].a := 81;
      SetLength(GlobTDynDynTRec1Array[1], 3);
      GlobTDynDynTRec1Array[1][0].a := 85;
      GlobTDynDynTRec1Array[1][1].a := 86;
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
      GlobTDynStatTRec1Array[0][3].a := 70;
      GlobTDynStatTRec1Array[0][4].a := 71;
      GlobTDynStatTRec1Array[1][3].a := 75;
      GlobTDynStatTRec1Array[1][4].a := 76;
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


      //GlobTDynDynTRec2Array: TDynDynTRec2Array;
      SetLength(GlobTDynDynTRec2Array, 3);
      SetLength(GlobTDynDynTRec2Array[0], 3);
      GlobTDynDynTRec2Array[0][0].c := 80;
      GlobTDynDynTRec2Array[0][1].c := 81;
      SetLength(GlobTDynDynTRec2Array[1], 3);
      GlobTDynDynTRec2Array[1][0].c := 85;
      GlobTDynDynTRec2Array[1][1].c := 86;
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
      GlobTDynStatTRec2Array[0][3].c := 70;
      GlobTDynStatTRec2Array[0][4].c := 71;
      GlobTDynStatTRec2Array[1][3].c := 75;
      GlobTDynStatTRec2Array[1][4].c := 76;
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
      SetLength(GlobTStatDynTRec1Array, 3);
      SetLength(GlobTStatDynTRec1Array[0], 3);
      GlobTStatDynTRec1Array[0][0].a := 80;
      GlobTStatDynTRec1Array[0][1].a := 81;
      SetLength(GlobTStatDynTRec1Array[1], 3);
      GlobTStatDynTRec1Array[1][0].a := 85;
      GlobTStatDynTRec1Array[1][1].a := 86;
      //GlobTStatDynPrec1Array: TStatDynPrec1Array;
      SetLength(GlobTStatDynPrec1Array, 3);
      SetLength(GlobTStatDynPrec1Array[0], 3);
      GlobTStatDynPrec1Array[0][0] := @GlobTStatDynTRec1Array[0][0];
      GlobTStatDynPrec1Array[0][1] := @GlobTStatDynTRec1Array[0][1];
      SetLength(GlobTStatDynPrec1Array[1], 3);
      GlobTStatDynPrec1Array[1][0] := @GlobTStatDynTRec1Array[1][0];
      GlobTStatDynPrec1Array[1][1] := @GlobTStatDynTRec1Array[1][1];
      //GlobTStatStatTRec1Array: TStatStatTRec1Array;
      SetLength(GlobTStatStatTRec1Array, 3);
      GlobTStatStatTRec1Array[0][3].a := 70;
      GlobTStatStatTRec1Array[0][4].a := 71;
      GlobTStatStatTRec1Array[1][3].a := 75;
      GlobTStatStatTRec1Array[1][4].a := 76;
      //GlobTStatStatPRec1Array: TStatStatPRec1Array;
      SetLength(GlobTStatStatPRec1Array, 3);
      GlobTStatStatPRec1Array[0][3] := @GlobTStatStatTRec1Array[0][3];
      GlobTStatStatPRec1Array[0][4] := @GlobTStatStatTRec1Array[0][4];
      GlobTStatStatPRec1Array[1][3] := @GlobTStatStatTRec1Array[1][3];
      GlobTStatStatPRec1Array[1][4] := @GlobTStatStatTRec1Array[1][4];
      //GlobTStatPDynTRec1Array: TStatPDynTRec1Array;
      SetLength(GlobTStatPDynTRec1Array, 3);
      GlobTStatPDynTRec1Array[0] := @GlobTStatDynTRec1Array[0];
      GlobTStatPDynTRec1Array[1] := @GlobTStatDynTRec1Array[1];
      //GlobTStatPStatTRec1Array: TStatPStatTRec1Array;
      SetLength(GlobTStatPStatTRec1Array, 3);
      GlobTStatPStatTRec1Array[0] := @GlobTStatStatTRec1Array[0];
      GlobTStatPStatTRec1Array[1] := @GlobTStatStatTRec1Array[1];


      //GlobTStatDynTRec2Array: TStatDynTRec2Array;
      SetLength(GlobTStatDynTRec2Array, 3);
      SetLength(GlobTStatDynTRec2Array[0], 3);
      GlobTStatDynTRec2Array[0][0].c := 80;
      GlobTStatDynTRec2Array[0][1].c := 81;
      SetLength(GlobTStatDynTRec2Array[1], 3);
      GlobTStatDynTRec2Array[1][0].c := 85;
      GlobTStatDynTRec2Array[1][1].c := 86;
      //GlobTStatDynPRec2Array: TStatDynPRec2Array;
      SetLength(GlobTStatDynPRec2Array, 3);
      SetLength(GlobTStatDynPRec2Array[0], 3);
      GlobTStatDynPRec2Array[0][0] := @GlobTStatDynTRec2Array[0][0];
      GlobTStatDynPRec2Array[0][1] := @GlobTStatDynTRec2Array[0][1];
      SetLength(GlobTStatDynPRec2Array[1], 3);
      GlobTStatDynPRec2Array[1][0] := @GlobTStatDynTRec2Array[1][0];
      GlobTStatDynPRec2Array[1][1] := @GlobTStatDynTRec2Array[1][1];
      //GlobTStatDynPPrec2Array: TStatDynPPrec2Array;
      SetLength(GlobTStatDynPPrec2Array, 3);
      SetLength(GlobTStatDynPPrec2Array[0], 3);
      GlobTStatDynPPrec2Array[0][0] := @GlobTStatDynPRec2Array[0][0];
      GlobTStatDynPPrec2Array[0][1] := @GlobTStatDynPRec2Array[0][1];
      SetLength(GlobTStatDynPPrec2Array[1], 3);
      GlobTStatDynPPrec2Array[1][0] := @GlobTStatDynPRec2Array[1][0];
      GlobTStatDynPPrec2Array[1][1] := @GlobTStatDynPRec2Array[1][1];
      //GlobTStatStatTRec2Array: TStatStatTRec2Array;
      SetLength(GlobTStatStatTRec2Array, 3);
      GlobTStatStatTRec2Array[0][3].c := 70;
      GlobTStatStatTRec2Array[0][4].c := 71;
      GlobTStatStatTRec2Array[1][3].c := 75;
      GlobTStatStatTRec2Array[1][4].c := 76;
      //GlobTStatStatPRec2Array: TStatStatPRec2Array;
      SetLength(GlobTStatStatPRec2Array, 3);
      GlobTStatStatPRec2Array[0][3] := @GlobTStatStatTRec2Array[0][3];
      GlobTStatStatPRec2Array[0][4] := @GlobTStatStatTRec2Array[0][4];
      GlobTStatStatPRec2Array[1][3] := @GlobTStatStatTRec2Array[1][3];
      GlobTStatStatPRec2Array[1][4] := @GlobTStatStatTRec2Array[1][4];
      //GlobTStatStatPPRec2Array: TStatStatPPRec2Array;
      SetLength(GlobTStatStatPPRec2Array, 3);
      GlobTStatStatPPRec2Array[0][3] := @GlobTStatStatPRec2Array[0][3];
      GlobTStatStatPPRec2Array[0][4] := @GlobTStatStatPRec2Array[0][4];
      GlobTStatStatPPRec2Array[1][3] := @GlobTStatStatPRec2Array[1][3];
      GlobTStatStatPPRec2Array[1][4] := @GlobTStatStatPRec2Array[1][4];


      GlobTEnumArrayTRec1[eaOne].a      := 200;
      GlobTEnumArrayTRec1[eaTwo].a      := 201;
      GlobTSubEnumArrayTRec1[eaTwo].a   := 205;
      GlobTSubEnumArrayTRec1[eaThree].a := 205;

      GlobTStatDualArrayTRec2[3,3].c := 300;
      GlobTStatDualArrayTRec2[3,4].c := 301;
      GlobTStatDualArrayTRec2[4,5].c := 302;


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
      GlobADynArrayTRec1[0].a := 90;
      GlobADynArrayTRec1[1].a := 91;
      //GlobADynArrayPRec1: TDynArrayPRec1;
      SetLength(GlobADynArrayPRec1, 3);
      GlobADynArrayPRec1[0] := @GlobADynArrayTRec1[0];
      GlobADynArrayPRec1[1] := @GlobADynArrayTRec1[1];
      //GlobADynDynArrayTRec1: TDynDynArrayTRec1;
      SetLength(GlobADynDynArrayTRec1, 3);
      SetLength(GlobADynDynArrayTRec1[0], 3);
      GlobADynDynArrayTRec1[0][0].a := 80;
      GlobADynDynArrayTRec1[0][1].a := 81;
      SetLength(GlobADynDynArrayTRec1[1], 3);
      GlobADynDynArrayTRec1[1][0].a := 85;
      GlobADynDynArrayTRec1[1][1].a := 86;
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
      GlobADynStatArrayTRec1[0][3].a := 70;
      GlobADynStatArrayTRec1[0][4].a := 71;
      GlobADynStatArrayTRec1[1][3].a := 75;
      GlobADynStatArrayTRec1[1][4].a := 76;
      //GlobADynStatArrayPRec1: TDynStatArrayPRec1;
      SetLength(GlobADynStatArrayPRec1, 3);
      GlobADynStatArrayPRec1[0][3] := @GlobADynStatArrayTRec1[0][3];
      GlobADynStatArrayPRec1[0][4] := @GlobADynStatArrayTRec1[0][4];
      GlobADynStatArrayPRec1[1][3] := @GlobADynStatArrayTRec1[1][3];
      GlobADynStatArrayPRec1[1][4] := @GlobADynStatArrayTRec1[1][4];

      //GlobADynArrayTRec2: TDynArrayTRec2;
      SetLength(GlobADynArrayTRec2, 3);
      GlobADynArrayTRec2[0].c := 90;
      GlobADynArrayTRec2[1].c := 91;
      //GlobADynArrayPRec2: TDynArrayPRec2;
      SetLength(GlobADynArrayPRec2, 3);
      GlobADynArrayPRec2[0] := @GlobADynArrayTRec2[0];
      GlobADynArrayPRec2[1] := @GlobADynArrayTRec2[1];
      //GlobADynDynArrayTRec2: TDynDynArrayTRec2;
      SetLength(GlobADynDynArrayTRec2, 3);
      SetLength(GlobADynDynArrayTRec2[0], 3);
      GlobADynDynArrayTRec2[0][0].c := 80;
      GlobADynDynArrayTRec2[0][1].c := 81;
      SetLength(GlobADynDynArrayTRec2[1], 3);
      GlobADynDynArrayTRec2[1][0].c := 85;
      GlobADynDynArrayTRec2[1][1].c := 86;
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
      GlobADynStatArrayTRec2[0][3].c := 70;
      GlobADynStatArrayTRec2[0][4].c := 71;
      GlobADynStatArrayTRec2[1][3].c := 75;
      GlobADynStatArrayTRec2[1][4].c := 76;
      //GlobADynStatArrayPRec2: TDynStatArrayPRec2;
      SetLength(GlobADynStatArrayPRec2, 3);
      GlobADynStatArrayPRec2[0][3] := @GlobADynStatArrayTRec2[0][3];
      GlobADynStatArrayPRec2[0][4] := @GlobADynStatArrayTRec2[0][4];
      GlobADynStatArrayPRec2[1][3] := @GlobADynStatArrayTRec2[1][3];
      GlobADynStatArrayPRec2[1][4] := @GlobADynStatArrayTRec2[1][4];


  // stat arrays
      //GlobAStatArrayTRec1: TStatArrayTRec1;
      GlobAStatArrayTRec1[3].a := 50;
      GlobAStatArrayTRec1[4].a := 51;
      //GlobAStatArrayPRec1: TStatArrayPRec1;
      GlobAStatArrayPRec1[3] := @GlobAStatArrayTRec1[3];
      GlobAStatArrayPRec1[4] := @GlobAStatArrayTRec1[4];
      //GlobAStatDynArrayTRec1: TStatDynArrayTRec1;
      SetLength(GlobAStatDynArrayTRec1[3], 3);
      GlobAStatDynArrayTRec1[3][0].a := 40;
      GlobAStatDynArrayTRec1[3][1].a := 41;
      SetLength(GlobAStatDynArrayTRec1[4], 3);
      GlobAStatDynArrayTRec1[4][0].a := 45;
      GlobAStatDynArrayTRec1[4][1].a := 46;
      //GlobAStatDynArrayPRec1: TStatDynArrayPRec1;
      SetLength(GlobAStatDynArrayPRec1[3], 3);
      GlobAStatDynArrayPRec1[3][0] := @GlobADynDynArrayTRec1[3][0];
      GlobAStatDynArrayPRec1[3][1] := @GlobADynDynArrayTRec1[3][1];
      SetLength(GlobAStatDynArrayPRec1[4], 3);
      GlobAStatDynArrayPRec1[4][0] := @GlobADynDynArrayTRec1[4][0];
      GlobAStatDynArrayPRec1[4][1] := @GlobADynDynArrayTRec1[4][1];
      //GlobAStatStatArrayTRec1: TStatStatArrayTRec1;
      GlobAStatStatArrayTRec1[3][3].a := 30;
      GlobAStatStatArrayTRec1[3][4].a := 31;
      GlobAStatStatArrayTRec1[4][3].a := 35;
      GlobAStatStatArrayTRec1[4][4].a := 36;
      //GlobAStatStatArrayPRec1: TStatStatArrayPRec1;
      GlobAStatStatArrayPRec1[3][3] := @GlobAStatStatArrayTRec1[3][3];
      GlobAStatStatArrayPRec1[3][4] := @GlobAStatStatArrayTRec1[3][4];
      GlobAStatStatArrayPRec1[4][3] := @GlobAStatStatArrayTRec1[4][3];
      GlobAStatStatArrayPRec1[4][4] := @GlobAStatStatArrayTRec1[4][4];

      //GlobAStatArrayTRec2: TStatArrayTRec2;
      GlobAStatArrayTRec2[3].c := 50;
      GlobAStatArrayTRec2[4].c := 51;
      //GlobAStatArrayPRec2: TStatArrayPRec2;
      GlobAStatArrayPRec2[3] := @GlobAStatArrayTRec2[3];
      GlobAStatArrayPRec2[4] := @GlobAStatArrayTRec2[4];
      //GlobAStatDynArrayTRec2: TStatDynArrayTRec2;
      SetLength(GlobAStatDynArrayTRec2[3], 3);
      GlobAStatDynArrayTRec2[3][0].c := 40;
      GlobAStatDynArrayTRec2[3][1].c := 41;
      SetLength(GlobAStatDynArrayTRec2[4], 3);
      GlobAStatDynArrayTRec2[4][0].c := 45;
      GlobAStatDynArrayTRec2[4][1].c := 46;
      //GlobAStatDynArrayPRec2: TStatDynArrayPRec2;
      SetLength(GlobAStatDynArrayPRec2[3], 3);
      GlobAStatDynArrayPRec2[3][0] := @GlobADynDynArrayTRec2[3][0];
      GlobAStatDynArrayPRec2[3][1] := @GlobADynDynArrayTRec2[3][1];
      SetLength(GlobAStatDynArrayPRec2[4], 3);
      GlobAStatDynArrayPRec2[4][0] := @GlobADynDynArrayTRec2[4][0];
      GlobAStatDynArrayPRec2[4][1] := @GlobADynDynArrayTRec2[4][1];
      //GlobAStatStatArrayTRec2: TStatStatArrayTRec2;
      GlobAStatStatArrayTRec2[3][3].c := 30;
      GlobAStatStatArrayTRec2[3][4].c := 31;
      GlobAStatStatArrayTRec2[4][3].c := 35;
      GlobAStatStatArrayTRec2[4][4].c := 36;
      //GlobAStatStatArrayPRec2: TStatStatArrayPRec2;
      GlobAStatStatArrayPRec2[3][3] := @GlobAStatStatArrayTRec2[3][3];
      GlobAStatStatArrayPRec2[3][4] := @GlobAStatStatArrayTRec2[3][4];
      GlobAStatStatArrayPRec2[4][3] := @GlobAStatStatArrayTRec2[4][3];
      GlobAStatStatArrayPRec2[4][4] := @GlobAStatStatArrayTRec2[4][4];



  // dyn arrays of named arrays
      //GlobADynDynTRec1Array: TDynDynTRec1Array;
      SetLength(GlobADynDynTRec1Array, 3);
      SetLength(GlobADynDynTRec1Array[0], 3);
      GlobADynDynTRec1Array[0][0].a := 80;
      GlobADynDynTRec1Array[0][1].a := 81;
      SetLength(GlobADynDynTRec1Array[1], 3);
      GlobADynDynTRec1Array[1][0].a := 85;
      GlobADynDynTRec1Array[1][1].a := 86;
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
      GlobADynStatTRec1Array[0][3].a := 70;
      GlobADynStatTRec1Array[0][4].a := 71;
      GlobADynStatTRec1Array[1][3].a := 75;
      GlobADynStatTRec1Array[1][4].a := 76;
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


      //GlobADynDynTRec2Array: TDynDynTRec2Array;
      SetLength(GlobADynDynTRec2Array, 3);
      SetLength(GlobADynDynTRec2Array[0], 3);
      GlobADynDynTRec2Array[0][0].c := 80;
      GlobADynDynTRec2Array[0][1].c := 81;
      SetLength(GlobADynDynTRec2Array[1], 3);
      GlobADynDynTRec2Array[1][0].c := 85;
      GlobADynDynTRec2Array[1][1].c := 86;
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
      GlobADynStatTRec2Array[0][3].c := 70;
      GlobADynStatTRec2Array[0][4].c := 71;
      GlobADynStatTRec2Array[1][3].c := 75;
      GlobADynStatTRec2Array[1][4].c := 76;
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
      GlobAStatDynTRec1Array[0][0].a := 80;
      GlobAStatDynTRec1Array[0][1].a := 81;
      SetLength(GlobAStatDynTRec1Array[1], 3);
      GlobAStatDynTRec1Array[1][0].a := 85;
      GlobAStatDynTRec1Array[1][1].a := 86;
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
      GlobAStatStatTRec1Array[0][3].a := 70;
      GlobAStatStatTRec1Array[0][4].a := 71;
      GlobAStatStatTRec1Array[1][3].a := 75;
      GlobAStatStatTRec1Array[1][4].a := 76;
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
      GlobAStatDynTRec2Array[0][0].c := 80;
      GlobAStatDynTRec2Array[0][1].c := 81;
      SetLength(GlobAStatDynTRec2Array[1], 3);
      GlobAStatDynTRec2Array[1][0].c := 85;
      GlobAStatDynTRec2Array[1][1].c := 86;
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
      GlobAStatStatTRec2Array[0][3].c := 70;
      GlobAStatStatTRec2Array[0][4].c := 71;
      GlobAStatStatTRec2Array[1][3].c := 75;
      GlobAStatStatTRec2Array[1][4].c := 76;
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
      GlobADynArrayTRec5[0].a := 10;
      GlobADynArrayTRec5[0].c := 12;
      GlobADynArrayTRec5[1].a := 11;
      GlobADynArrayTRec5[1].c := 13;

      GlobAStatArrayTRec5[3].a := 15;
      GlobAStatArrayTRec5[3].c := 17;
      GlobAStatArrayTRec5[4].a := 16;
      GlobAStatArrayTRec5[4].c := 18;

      GlobAEnumArrayTRec1[eaOne].a      := 200;
      GlobAEnumArrayTRec1[eaTwo].a      := 201;
      GlobASubEnumArrayTRec1[eaTwo].a   := 205;
      GlobASubEnumArrayTRec1[eaThree].a := 205;

      GlobAStatDualArrayTRec2[3,3].c := 300;
      GlobAStatDualArrayTRec2[3,4].c := 301;
      GlobAStatDualArrayTRec2[4,3].c := 302;
    {$ENDIF}

    {$IFDEF Global_Body_NIL}
    //begin
    {$ENDIF}

  {%endregion}


