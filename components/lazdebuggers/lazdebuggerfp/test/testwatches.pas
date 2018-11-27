unit TestWatches;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TestBase, TestDbgControl,
  TestDbgTestSuites, TTestDebuggerClasses, TestOutputLogger,
  TTestWatchUtilities, TestCommonSources, TestDbgConfig, DbgIntfDebuggerBase,
  DbgIntfBaseTypes, Forms;

type

  { TTestWatches }

  TTestWatches = class(TDBGTestCase)
  published
    procedure TestWatchesScope;
  end;

implementation
var
  ControlTestWatch: Pointer;

procedure TTestWatches.TestWatchesScope;

  procedure AddWatchesForClassMethods(t: TWatchExpectationList; AName: String; AStackOffs: Integer);
  var
    n: String;
    f: Integer;
    //IntGlobFlags: TWatchExpErrorHandlingFlags;
  begin
    // Test all outer, class, glopbal scopes are visible - from each stackframe

    n := AName;
    f := -AStackOffs;
    if f >= 0 then begin
      t.Add(n, 'Int_MethodMainChildNestedTwice',  30, f);
      t.Add(n, 'Int_MethodMainChildNested'     ,  40, f);
      t.Add(n, 'Int_MethodMainChild'           ,  50, f);
      t.Add(n, 'Int_MethodMainChild_Late'      ,  51, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Int_TClassMainChild'           ,  70, f);
      t.Add(n, 'Int_TClassMainChild_Prot'      ,  71, f);
      t.Add(n, 'Int_TClassMainChild_Priv'      ,  72, f);
      t.Add(n, 'Int_TClassMain'                ,  80, f);
      t.Add(n, 'Int_TClassMain_Prot'           ,  81, f);
      t.Add(n, 'Int_TClassMain_Priv'           ,  82, f);
      t.Add(n, 'Int_TClassMainBase'            , 170, f);
      t.Add(n, 'Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'Int_TClassMainBase_Priv'       , 172, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'Int_TClassMainBaseBase_Priv'   , 272, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Int_GlobalPrg'                 , 101, f);
      t.Add(n, 'Int_GlobalUnit1'               , 201, f);
      t.Add(n, 'Int_GlobalUnit2'               , 202, f);
  //    t.Add(n, 'WatchesScopeUnit1.Int_GlobalPrg'   , 101, f)^.AddFlag(ehExpectNotFound);
  //    t.Add(n, 'WatchesScopeUnit1.Int_GlobalUnit1' , 201, f);
  //    t.Add(n, 'WatchesScopeUnit2.Int_GlobalUnit1' , 201, f)^.AddFlag(ehExpectNotFound);
  //    t.Add(n, 'WatchesScopeUnit2.Int_GlobalUnit2' , 202, f);

      t.Add(n, 'Self.Int_MethodMainChildNestedTwice',  30, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Self.Int_MethodMainChildNested'     ,  40, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Self.Int_MethodMainChild'           ,  50, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Self.Int_MethodMainChild_Late'      ,  51, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Self.Int_TClassMainChild'           ,  70, f);
      t.Add(n, 'Self.Int_TClassMainChild_Prot'      ,  71, f);
      t.Add(n, 'Self.Int_TClassMainChild_Priv'      ,  72, f);
      t.Add(n, 'Self.Int_TClassMain'                ,  80, f);
      t.Add(n, 'Self.Int_TClassMain_Prot'           ,  81, f);
      t.Add(n, 'Self.Int_TClassMain_Priv'           ,  82, f);
      t.Add(n, 'Self.Int_TClassMainBase'            , 170, f);
      t.Add(n, 'Self.Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'Self.Int_TClassMainBase_Priv'       , 172, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Self.Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'Self.Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'Self.Int_TClassMainBaseBase_Priv'   , 272, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Self.Int_GlobalPrg'                 , 101, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Self.Int_GlobalUnit1'               , 201, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Self.Int_GlobalUnit2'               , 202, f)^.AddFlag(ehExpectNotFound);

      t.Add(n, 'TClassMainChild(Self).Int_TClassMainChild'           ,  70, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainChild_Prot'      ,  71, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainChild_Priv'      ,  72, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMain'                ,  80, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMain_Prot'           ,  81, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMain_Priv'           ,  82, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBase'            , 170, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBase_Priv'       , 172, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBaseBase_Priv'   , 272, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);

      t.Add(n, 'TClassMain(Self).Int_TClassMainChild'           ,  70, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMain(Self).Int_TClassMainChild_Prot'      ,  71, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMain(Self).Int_TClassMainChild_Priv'      ,  72, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMain(Self).Int_TClassMain'                ,  80, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMain_Prot'           ,  81, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMain_Priv'           ,  82, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBase'            , 170, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBase_Priv'       , 172, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBaseBase_Priv'   , 272, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);

      t.Add(n, 'TClassMainBase(Self).Int_TClassMainChild'           ,  70, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMainBase(Self).Int_TClassMainChild_Prot'      ,  71, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMainBase(Self).Int_TClassMainChild_Priv'      ,  72, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMainBase(Self).Int_TClassMain'                ,  80, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMainBase(Self).Int_TClassMain_Prot'           ,  81, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMainBase(Self).Int_TClassMain_Priv'           ,  82, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMainBase(Self).Int_TClassMainBase'            , 170, f);
      t.Add(n, 'TClassMainBase(Self).Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'TClassMainBase(Self).Int_TClassMainBase_Priv'       , 172, f);
      t.Add(n, 'TClassMainBase(Self).Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'TClassMainBase(Self).Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'TClassMainBase(Self).Int_TClassMainBaseBase_Priv'   , 272, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);

      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Class' , 3001, f);
      t.Add(n + '; Hide, view MainChild', 'Self.Int_HideTest_Class' , 3001, f);
      t.Add(n + '; Hide, view MainChild', 'TClassMain(Self).Int_HideTest_Class' , 0, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n + '; Hide, view MainChild', 'TClassMainBase(Self).Int_HideTest_Class' , 1001, f);
      t.Add(n + '; Hide, view MainChild', 'TObject(Self).Int_HideTest_Class' , 0, f)^.AddFlag([ehExpectNotFound]);

      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Unit' , 3010, f);
    end;

    n := AName + ' (Stack: MethodMainChildNested)';
    f := 1 - AStackOffs;
    if f >= 0 then begin
      t.Add(n, 'Int_MethodMainChildNestedTwice',  30, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_MethodMainChildNested'     ,  40, f);
      t.Add(n, 'Int_MethodMainChild'           ,  50, f);
      t.Add(n, 'Int_MethodMainChild_Late'      ,  51, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Int_TClassMainChild'           ,  70, f);
      t.Add(n, 'Int_TClassMainChild_Prot'      ,  71, f);
      t.Add(n, 'Int_TClassMainChild_Priv'      ,  72, f);
      t.Add(n, 'Int_TClassMain'                ,  80, f);
      t.Add(n, 'Int_TClassMain_Prot'           ,  81, f);
      t.Add(n, 'Int_TClassMain_Priv'           ,  82, f);
      t.Add(n, 'Int_TClassMainBase'            , 170, f);
      t.Add(n, 'Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'Int_TClassMainBase_Priv'       , 172, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'Int_TClassMainBaseBase_Priv'   , 272, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Int_GlobalPrg'                 , 101, f);
      t.Add(n, 'Int_GlobalUnit1'               , 201, f);
      t.Add(n, 'Int_GlobalUnit2'               , 202, f);

      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Class' , 3001, f);
      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Unit' , 3010, f);
    end;

    n := AName + ' (Stack: MethodMainChild)';
    f := 2 - AStackOffs;
    if f >= 0 then begin
      t.Add(n, 'Int_MethodMainChildNestedTwice',  30, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_MethodMainChildNested'     ,  40, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_MethodMainChild'           ,  50, f);
      t.Add(n, 'Int_MethodMainChild_Late'      ,  51, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Int_TClassMainChild'           ,  70, f);
      t.Add(n, 'Int_TClassMainChild_Prot'      ,  71, f);
      t.Add(n, 'Int_TClassMainChild_Priv'      ,  72, f);
      t.Add(n, 'Int_TClassMain'                ,  80, f);
      t.Add(n, 'Int_TClassMain_Prot'           ,  81, f);
      t.Add(n, 'Int_TClassMain_Priv'           ,  82, f);
      t.Add(n, 'Int_TClassMainBase'            , 170, f);
      t.Add(n, 'Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'Int_TClassMainBase_Priv'       , 172, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'Int_TClassMainBaseBase_Priv'   , 272, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Int_GlobalPrg'                 , 101, f);
      t.Add(n, 'Int_GlobalUnit1'               , 201, f);
      t.Add(n, 'Int_GlobalUnit2'               , 202, f);

      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Class' , 3001, f);
      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Unit' , 3010, f);
    end;

    n := AName + ' (Stack: MethodMain)';
    f := 3 - AStackOffs;
    if f >= 0 then begin
      t.Add(n, 'Int_MethodMainChildNestedTwice',  30, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_MethodMainChildNested'     ,  40, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_MethodMainChild'           ,  50, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_MethodMainChild_Late'      ,  51, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainChild'           ,  70, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainChild_Prot'      ,  71, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainChild_Priv'      ,  72, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMain'                ,  80, f);
      t.Add(n, 'Int_TClassMain_Prot'           ,  81, f);
      t.Add(n, 'Int_TClassMain_Priv'           ,  82, f);
      t.Add(n, 'Int_TClassMainBase'            , 170, f);
      t.Add(n, 'Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'Int_TClassMainBase_Priv'       , 172, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'Int_TClassMainBaseBase_Priv'   , 272, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Int_GlobalPrg'                 , 101, f);
      t.Add(n, 'Int_GlobalUnit1'               , 201, f);
      t.Add(n, 'Int_GlobalUnit2'               , 202, f);

      t.Add(n + ', Hide, view glob Prg', 'Int_HideTest_Class' , 3000, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
    end;

    n := AName + ' (Stack: MethodMainBase)';
    f := 4 - AStackOffs;
    if f >= 0 then begin
      t.Add(n, 'Int_MethodMainChildNestedTwice',  30, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_MethodMainChildNested'     ,  40, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_MethodMainChild'           ,  50, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_MethodMainChild_Late'      ,  51, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainChild'           ,  70, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainChild_Prot'      ,  71, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainChild_Priv'      ,  72, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMain'                ,  80, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMain_Prot'           ,  81, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMain_Priv'           ,  82, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainBase'            , 170, f);
      t.Add(n, 'Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'Int_TClassMainBase_Priv'       , 172, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'Int_TClassMainBaseBase_Priv'   , 272, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Int_GlobalPrg'                 , 101, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Int_GlobalUnit1'               , 201, f);
      t.Add(n, 'Int_GlobalUnit2'               , 202, f);

      t.Add(n, 'TClassMain(Self).Int_MethodMainChildNestedTwice',  30, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMain(Self).Int_MethodMainChildNested'     ,  40, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMain(Self).Int_MethodMainChild'           ,  50, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMain(Self).Int_MethodMainChild_Late'      ,  51, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMain(Self).Int_TClassMainChild'           ,  70, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMain(Self).Int_TClassMainChild_Prot'      ,  71, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMain(Self).Int_TClassMainChild_Priv'      ,  72, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMain(Self).Int_TClassMain'                ,  80, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMain_Prot'           ,  81, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMain_Priv'           ,  82, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBase'            , 170, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBase_Priv'       , 172, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBaseBase_Priv'   , 272, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'TClassMain(Self).Int_GlobalPrg'                 , 101, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMain(Self).Int_GlobalUnit1'               , 201, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMain(Self).Int_GlobalUnit2'               , 202, f)^.AddFlag(ehExpectNotFound);

      t.Add(n, 'TClassMainChild(Self).Int_MethodMainChildNestedTwice',  30, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMainChild(Self).Int_MethodMainChildNested'     ,  40, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMainChild(Self).Int_MethodMainChild'           ,  50, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMainChild(Self).Int_MethodMainChild_Late'      ,  51, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainChild'           ,  70, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainChild_Prot'      ,  71, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainChild_Priv'      ,  72, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMain'                ,  80, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMain_Prot'           ,  81, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMain_Priv'           ,  82, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBase'            , 170, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBase_Priv'       , 172, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBaseBase_Priv'   , 272, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);

      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Class' , 1001, f);
      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Unit' , 1010, f);
    end;

    n := AName + ' (Stack: MethodMainBaseBase)';
    f := 5 - AStackOffs;
    if f >= 0 then begin
      t.Add(n, 'Int_MethodMainChildNestedTwice',  30, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_MethodMainChildNested'     ,  40, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_MethodMainChild'           ,  50, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_MethodMainChild_Late'      ,  51, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainChild'           ,  70, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainChild_Prot'      ,  71, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainChild_Priv'      ,  72, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMain'                ,  80, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMain_Prot'           ,  81, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMain_Priv'           ,  82, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainBase'            , 170, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainBase_Prot'       , 171, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainBase_Priv'       , 172, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'Int_TClassMainBaseBase_Priv'   , 272, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Int_GlobalPrg'                 , 101, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Int_GlobalUnit1'               , 201, f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);
      t.Add(n, 'Int_GlobalUnit2'               , 202, f);

      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Class' , 2001, f);
      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Unit' , 2010, f);
    end;

    n := AName + ' (Stack: main)';
    f := 6 - AStackOffs;
    if f >= 0 then begin
      t.Add(n, 'Int_MethodMainChildNestedTwice',  30, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_MethodMainChildNested'     ,  40, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_MethodMainChild'           ,  50, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_MethodMainChild_Late'      ,  51, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainChild'           ,  70, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainChild_Prot'      ,  71, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainChild_Priv'      ,  72, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMain'                ,  80, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMain_Prot'           ,  81, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMain_Priv'           ,  82, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainBase'            , 170, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainBase_Prot'       , 171, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainBase_Priv'       , 172, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainBaseBase'        , 270, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainBaseBase_Prot'   , 271, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_TClassMainBaseBase_Priv'   , 272, f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'Int_GlobalPrg'                 , 101, f);
      t.Add(n, 'Int_GlobalUnit1'               , 201, f);
      t.Add(n, 'Int_GlobalUnit2'               , 202, f);

      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Class' , 3000, f);
      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Unit' , 3010, f);
    end;
  end;

  procedure AddWatchesForFoo(t: TWatchExpectationList; AName: String; AStackOffs: Integer; Twice2: Boolean = False);
  var
    n: String;
    f: Integer;
    //IntGlobFlags: TWatchExpErrorHandlingFlags;
  begin

    n := AName;
    f := -AStackOffs;
    if f >= 0 then begin
      if Twice2 then begin
        t.Add(n, 'Int_Hide_Foo',  5, f);
        t.Add(n, 'Result',  'abc2', f)^.AddFlag(ehIgnKindPtr);
      end
      else begin
        t.Add(n, 'Int_Hide_Foo',  4, f);
        t.Add(n, 'Result',  'abc', f)^.AddFlag(ehIgnKindPtr);
      end;
    end;

    n := AName + ' FooNested';
    f := 1 - AStackOffs;
    if f >= 0 then begin
      t.Add(n, 'Int_Hide_Foo',  3, f);
      t.Add(n, 'Result',  'bar', f)^.AddFlag(ehIgnKindPtr);
    end;

    n := AName + ' Foo';
    f := 2 - AStackOffs;
    if f >= 0 then begin
      t.Add(n, 'Int_Hide_Foo',  2, f);
      t.Add(n, 'Result',  99, f);
    end;

    n := AName + ' Prg';
    f := 3 - AStackOffs;
    if f >= 0 then begin
      t.Add(n, 'Int_Hide_Foo',  1, f);
      t.Add(n, 'Result',  0, f)^.AddFlag(ehExpectNotFound);
    end;

  end;

var
  ExeName: String;
  dbg: TDebuggerIntf;
  t: TWatchExpectationList;
  Src: TCommonSource;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestWatch) then exit;
  t := nil;

  Src := GetCommonSourceFor('WatchesScopePrg.Pas');
  TestCompile(Src, ExeName);

  AssertTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));
  dbg := Debugger.LazDebugger;

  try
    t := TWatchExpectationList.Create(Self);
    t.AcceptSkSimple := [skInteger, skCardinal, skBoolean, skChar, skFloat, skString, skAnsiString, skCurrency, skVariant, skWideString];
    t.AddTypeNameAlias('integer', 'integer|longint');


    Debugger.SetBreakPoint(Src, 'FuncFooNestedTwice');
    Debugger.SetBreakPoint(Src, 'FuncFooNestedTwice2');
    Debugger.SetBreakPoint(Src, 'FuncFooNested');
    Debugger.SetBreakPoint(Src, 'FuncFoo');

    Debugger.SetBreakPoint(Src, 'MethodMainChildNestedTwice');
    Debugger.SetBreakPoint(Src, 'MethodMainChildNested');
    Debugger.SetBreakPoint(Src, 'MethodMainChild');
    Debugger.SetBreakPoint(Src, 'MethodMain');
    Debugger.SetBreakPoint(Src, 'WatchesScopeUnit1.pas', 'MethodMainBase');
    Debugger.SetBreakPoint(Src, 'WatchesScopeUnit2.pas', 'MethodMainBaseBase');
    Debugger.SetBreakPoint(Src, 'Prg');
    AssertDebuggerNotInErrorState;

    dbg.Run;
    Debugger.WaitForFinishRun();
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForFoo(t, 'Scope in FuncFooNestedTwice', 0);
    t.EvaluateWatches;
    t.CheckResults;

    dbg.Run;
    Debugger.WaitForFinishRun();
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForFoo(t, 'Scope in FuncFooNestedTwice2', 0, True);
    t.EvaluateWatches;
    t.CheckResults;

    dbg.Run;
    Debugger.WaitForFinishRun();
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForFoo(t, 'Scope in FuncFooNested', 1);
    t.EvaluateWatches;
    t.CheckResults;

    dbg.Run;
    Debugger.WaitForFinishRun();
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForFoo(t, 'Scope in FuncFoo', 2);
    t.EvaluateWatches;
    t.CheckResults;

(*


    t.Add('TestEnum', weEnum('te3', 'TTestEnum'));
    t.Add('TTestEnum(x)', weEnum('te2', 'TTestEnum'));
    t.Add('TTestEnum(y)', weEnum('te1', 'TTestEnum'));
    t.Add('TTestEnum(1)', weEnum('te1', 'TTestEnum'));

    t.Add('Integer', weEnum('XX', '')); // TODO
    t.Add('TTestEnum', weEnum('TTestEnum = (te1, te2, te3)', '')); // TODO



    t.Add('TestEnum', weEnum('te3', 'TTestEnum'));
    t.Add('TTestEnum(x)', weEnum('te2', 'TTestEnum'));
    t.Add('TTestEnum(y)', weEnum('te1', 'TTestEnum'))^.AddFlag(ehExpectNotFound);
    t.Add('TTestEnum(1)', weEnum('te1', 'TTestEnum'));

    t.Add('Integer', weEnum('XX', '')); // TODO
    t.Add('TTestEnum', weEnum('TTestEnum = (te1, te2, te3)', '')); // TODO

*)

    dbg.Run;
    Debugger.WaitForFinishRun(); // MethodMainChildNestedTwice
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForClassMethods(t, 'Scope in MethodMainChildNestedTwice', 0);
    t.EvaluateWatches;
    t.CheckResults;

    dbg.Run;
    Debugger.WaitForFinishRun(); // MethodMainChildNested
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForClassMethods(t, 'Scope in MethodMainChildNested', 1);
    t.EvaluateWatches;
    t.CheckResults;

    dbg.Run;
    Debugger.WaitForFinishRun(); // MethodMainChild
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForClassMethods(t, 'Scope in MethodMainChild', 2);
    t.EvaluateWatches;
    t.CheckResults;

    dbg.Run;
    Debugger.WaitForFinishRun(); // MethodMain
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForClassMethods(t, 'Scope in MethodMain', 3);
    t.EvaluateWatches;
    t.CheckResults;

    dbg.Run;
    Debugger.WaitForFinishRun(); // MethodMainBase
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForClassMethods(t, 'Scope in MethodMainBase', 4);
    t.EvaluateWatches;
    t.CheckResults;

    dbg.Run;
    Debugger.WaitForFinishRun(); // MethodMainBaseBase
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForClassMethods(t, 'Scope in MethodMainBaseBase', 5);
    t.EvaluateWatches;
    t.CheckResults;

    dbg.Run;
    Debugger.WaitForFinishRun(); // MethodMainBaseBase
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForClassMethods(t, 'Scope in Prg', 6);
    AddWatchesForFoo(t, 'Scope in Prg', 3);
    t.EvaluateWatches;
    t.CheckResults;

  finally
    t.Free;
    Debugger.ClearDebuggerMonitors;
    Debugger.FreeDebugger;

    AssertTestErrors;
  end;
end;


initialization
  RegisterDbgTest(TTestWatches);
  ControlTestWatch         := TestControlRegisterTest('TTestWatch');

end.

