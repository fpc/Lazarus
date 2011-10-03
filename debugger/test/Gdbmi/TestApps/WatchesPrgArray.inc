
{%region FooFunc}
  {$IFDEF FooFunc_Param}
  //procedure FooFunc(
  {$ENDIF}

  {$IFDEF FooFunc_LocalVar}
  //var
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
{%endregion FooFunc}

{%region GLOBAL}
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
  Record1 = record
    a : integer;
    b : integer;
  end;
  Record1Array = array of Record1;
  Record2 = record
    c : integer;
    recs : Record1Array;
  end;
  PRecord2 = ^Record2;

  {$ENDIF}

  {$IFDEF Global_Var}
  //var
// dyn array with record
  myRec2 : PRecord2;
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
  {$ENDIF}

  {$IFDEF Global_Body_NIL}
  //begin
  {$ENDIF}

  {$IFDEF Global_Call_FooFunc}
  //FooFunc(
  {$ENDIF}
{%endregion GLOBAL}

