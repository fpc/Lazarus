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
    procedure TestWatchesValue;
  end;

implementation

var
  ControlTestWatch, ControlTestWatchScope, ControlTestWatchValue: Pointer;

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
      t.Add(n, 'Int_MethodMainChild_Late'      ,  51, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Int_TClassMainChild'           ,  70, f);
      t.Add(n, 'Int_TClassMainChild_Prot'      ,  71, f);
      t.Add(n, 'Int_TClassMainChild_Priv'      ,  72, f);
      t.Add(n, 'Int_TClassMain'                ,  80, f);
      t.Add(n, 'Int_TClassMain_Prot'           ,  81, f);
      t.Add(n, 'Int_TClassMain_Priv'           ,  82, f);
      t.Add(n, 'Int_TClassMainBase'            , 170, f);
      t.Add(n, 'Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'Int_TClassMainBase_Priv'       , 172, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'Int_TClassMainBaseBase_Priv'   , 272, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Int_GlobalPrg'                 , 101, f);
      t.Add(n, 'Int_GlobalUnit1'               , 201, f);
      t.Add(n, 'Int_GlobalUnit2'               , 202, f);
  //    t.Add(n, 'WatchesScopeUnit1.Int_GlobalPrg'   , 101, f).ExpectNotFound;
  //    t.Add(n, 'WatchesScopeUnit1.Int_GlobalUnit1' , 201, f);
  //    t.Add(n, 'WatchesScopeUnit2.Int_GlobalUnit1' , 201, f).ExpectNotFound;
  //    t.Add(n, 'WatchesScopeUnit2.Int_GlobalUnit2' , 202, f);

      t.Add(n, 'Self.Int_MethodMainChildNestedTwice',  30, f).ExpectNotFound;
      t.Add(n, 'Self.Int_MethodMainChildNested'     ,  40, f).ExpectNotFound;
      t.Add(n, 'Self.Int_MethodMainChild'           ,  50, f).ExpectNotFound;
      t.Add(n, 'Self.Int_MethodMainChild_Late'      ,  51, f).ExpectNotFound;
      t.Add(n, 'Self.Int_TClassMainChild'           ,  70, f);
      t.Add(n, 'Self.Int_TClassMainChild_Prot'      ,  71, f);
      t.Add(n, 'Self.Int_TClassMainChild_Priv'      ,  72, f);
      t.Add(n, 'Self.Int_TClassMain'                ,  80, f);
      t.Add(n, 'Self.Int_TClassMain_Prot'           ,  81, f);
      t.Add(n, 'Self.Int_TClassMain_Priv'           ,  82, f);
      t.Add(n, 'Self.Int_TClassMainBase'            , 170, f);
      t.Add(n, 'Self.Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'Self.Int_TClassMainBase_Priv'       , 172, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Self.Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'Self.Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'Self.Int_TClassMainBaseBase_Priv'   , 272, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Self.Int_GlobalPrg'                 , 101, f).ExpectNotFound;
      t.Add(n, 'Self.Int_GlobalUnit1'               , 201, f).ExpectNotFound;
      t.Add(n, 'Self.Int_GlobalUnit2'               , 202, f).ExpectNotFound;

      t.Add(n, 'TClassMainChild(Self).Int_TClassMainChild'           ,  70, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainChild_Prot'      ,  71, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainChild_Priv'      ,  72, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMain'                ,  80, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMain_Prot'           ,  81, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMain_Priv'           ,  82, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBase'            , 170, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBase_Priv'       , 172, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBaseBase_Priv'   , 272, f).ExpectNotFound.NotImplemented;

      t.Add(n, 'TClassMain(Self).Int_TClassMainChild'           ,  70, f).ExpectNotFound;
      t.Add(n, 'TClassMain(Self).Int_TClassMainChild_Prot'      ,  71, f).ExpectNotFound;
      t.Add(n, 'TClassMain(Self).Int_TClassMainChild_Priv'      ,  72, f).ExpectNotFound;
      t.Add(n, 'TClassMain(Self).Int_TClassMain'                ,  80, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMain_Prot'           ,  81, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMain_Priv'           ,  82, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBase'            , 170, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBase_Priv'       , 172, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'TClassMain(Self).Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBaseBase_Priv'   , 272, f).ExpectNotFound.NotImplemented;

      t.Add(n, 'TClassMainBase(Self).Int_TClassMainChild'           ,  70, f).ExpectNotFound;
      t.Add(n, 'TClassMainBase(Self).Int_TClassMainChild_Prot'      ,  71, f).ExpectNotFound;
      t.Add(n, 'TClassMainBase(Self).Int_TClassMainChild_Priv'      ,  72, f).ExpectNotFound;
      t.Add(n, 'TClassMainBase(Self).Int_TClassMain'                ,  80, f).ExpectNotFound;
      t.Add(n, 'TClassMainBase(Self).Int_TClassMain_Prot'           ,  81, f).ExpectNotFound;
      t.Add(n, 'TClassMainBase(Self).Int_TClassMain_Priv'           ,  82, f).ExpectNotFound;
      t.Add(n, 'TClassMainBase(Self).Int_TClassMainBase'            , 170, f);
      t.Add(n, 'TClassMainBase(Self).Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'TClassMainBase(Self).Int_TClassMainBase_Priv'       , 172, f);
      t.Add(n, 'TClassMainBase(Self).Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'TClassMainBase(Self).Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'TClassMainBase(Self).Int_TClassMainBaseBase_Priv'   , 272, f).ExpectNotFound.NotImplemented;

      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Class' , 3001, f);
      t.Add(n + '; Hide, view MainChild', 'Self.Int_HideTest_Class' , 3001, f);
      t.Add(n + '; Hide, view MainChild', 'TClassMain(Self).Int_HideTest_Class' , 0, f).ExpectNotFound.NotImplemented;
      t.Add(n + '; Hide, view MainChild', 'TClassMainBase(Self).Int_HideTest_Class' , 1001, f);
      t.Add(n + '; Hide, view MainChild', 'TObject(Self).Int_HideTest_Class' , 0, f).ExpectNotFound;

      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Unit' , 3010, f);

      t.Add(n, 'TMethodMainChildNestedTwiceEnum(1)', weEnum('mmCNT2'), f);
      t.Add(n, 'TMethodMainChildNestedEnum(1)',      weEnum('mmCN2'),  f);
      t.Add(n, 'TMethodMainChildEnum(1)',            weEnum('mmC2'),   f);
      t.Add(n, 'TMainEnum(1)',                       weEnum('mm2'),    f);
      t.Add(n, 'TMainBaseEnum(1)',                   weEnum('mmB2'),   f);
      t.Add(n, 'TMainGlobEnum(1)',                   weEnum('mmG2'),   f);

      t.Add(n, 'THideMainEnum(1)', weEnum('hmCNT2'), f);

    end;

    n := AName + ' (Stack: MethodMainChildNested)';
    f := 1 - AStackOffs;
    if f >= 0 then begin
      t.Add(n, 'Int_MethodMainChildNestedTwice',  30, f).ExpectNotFound;
      t.Add(n, 'Int_MethodMainChildNested'     ,  40, f);
      t.Add(n, 'Int_MethodMainChild'           ,  50, f);
      t.Add(n, 'Int_MethodMainChild_Late'      ,  51, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Int_TClassMainChild'           ,  70, f);
      t.Add(n, 'Int_TClassMainChild_Prot'      ,  71, f);
      t.Add(n, 'Int_TClassMainChild_Priv'      ,  72, f);
      t.Add(n, 'Int_TClassMain'                ,  80, f);
      t.Add(n, 'Int_TClassMain_Prot'           ,  81, f);
      t.Add(n, 'Int_TClassMain_Priv'           ,  82, f);
      t.Add(n, 'Int_TClassMainBase'            , 170, f);
      t.Add(n, 'Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'Int_TClassMainBase_Priv'       , 172, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'Int_TClassMainBaseBase_Priv'   , 272, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Int_GlobalPrg'                 , 101, f);
      t.Add(n, 'Int_GlobalUnit1'               , 201, f);
      t.Add(n, 'Int_GlobalUnit2'               , 202, f);

      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Class' , 3001, f);
      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Unit' , 3010, f);

      t.Add(n, 'TMethodMainChildNestedTwiceEnum(1)', weEnum('mmCNT2'), f).ExpectNotFound;
      t.Add(n, 'TMethodMainChildNestedEnum(1)',      weEnum('mmCN2'),  f);
      t.Add(n, 'TMethodMainChildEnum(1)',            weEnum('mmC2'),   f);
      t.Add(n, 'TMainEnum(1)',                       weEnum('mm2'),    f);
      t.Add(n, 'TMainBaseEnum(1)',                   weEnum('mmB2'),   f);
      t.Add(n, 'TMainGlobEnum(1)',                   weEnum('mmG2'),   f);

      t.Add(n, 'THideMainEnum(1)', weEnum('hmCN2'), f);
    end;

    n := AName + ' (Stack: MethodMainChild)';
    f := 2 - AStackOffs;
    if f >= 0 then begin
      t.Add(n, 'Int_MethodMainChildNestedTwice',  30, f).ExpectNotFound;
      t.Add(n, 'Int_MethodMainChildNested'     ,  40, f).ExpectNotFound;
      t.Add(n, 'Int_MethodMainChild'           ,  50, f);
      t.Add(n, 'Int_MethodMainChild_Late'      ,  51, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Int_TClassMainChild'           ,  70, f);
      t.Add(n, 'Int_TClassMainChild_Prot'      ,  71, f);
      t.Add(n, 'Int_TClassMainChild_Priv'      ,  72, f);
      t.Add(n, 'Int_TClassMain'                ,  80, f);
      t.Add(n, 'Int_TClassMain_Prot'           ,  81, f);
      t.Add(n, 'Int_TClassMain_Priv'           ,  82, f);
      t.Add(n, 'Int_TClassMainBase'            , 170, f);
      t.Add(n, 'Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'Int_TClassMainBase_Priv'       , 172, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'Int_TClassMainBaseBase_Priv'   , 272, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Int_GlobalPrg'                 , 101, f);
      t.Add(n, 'Int_GlobalUnit1'               , 201, f);
      t.Add(n, 'Int_GlobalUnit2'               , 202, f);

      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Class' , 3001, f);
      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Unit' , 3010, f);

      t.Add(n, 'TMethodMainChildNestedTwiceEnum(1)', weEnum('mmCNT2'), f).ExpectNotFound;
      t.Add(n, 'TMethodMainChildNestedEnum(1)',      weEnum('mmCN2'),  f).ExpectNotFound;
      t.Add(n, 'TMethodMainChildEnum(1)',            weEnum('mmC2'),   f);
      t.Add(n, 'TMainEnum(1)',                       weEnum('mm2'),    f);
      t.Add(n, 'TMainBaseEnum(1)',                   weEnum('mmB2'),   f);
      t.Add(n, 'TMainGlobEnum(1)',                   weEnum('mmG2'),   f);

      t.Add(n, 'THideMainEnum(1)', weEnum('hmC2'), f);
    end;

    n := AName + ' (Stack: MethodMain)';
    f := 3 - AStackOffs;
    if f >= 0 then begin
      t.Add(n, 'Int_MethodMainChildNestedTwice',  30, f).ExpectNotFound;
      t.Add(n, 'Int_MethodMainChildNested'     ,  40, f).ExpectNotFound;
      t.Add(n, 'Int_MethodMainChild'           ,  50, f).ExpectNotFound;
      t.Add(n, 'Int_MethodMainChild_Late'      ,  51, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainChild'           ,  70, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainChild_Prot'      ,  71, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainChild_Priv'      ,  72, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMain'                ,  80, f);
      t.Add(n, 'Int_TClassMain_Prot'           ,  81, f);
      t.Add(n, 'Int_TClassMain_Priv'           ,  82, f);
      t.Add(n, 'Int_TClassMainBase'            , 170, f);
      t.Add(n, 'Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'Int_TClassMainBase_Priv'       , 172, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'Int_TClassMainBaseBase_Priv'   , 272, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Int_GlobalPrg'                 , 101, f);
      t.Add(n, 'Int_GlobalUnit1'               , 201, f);
      t.Add(n, 'Int_GlobalUnit2'               , 202, f);

      t.Add(n + ', Hide, view glob Prg', 'Int_HideTest_Class' , 3000, f).ExpectNotFound.NotImplemented;

      t.Add(n, 'TMethodMainChildNestedTwiceEnum(1)', weEnum('mmCNT2'), f).ExpectNotFound;
      t.Add(n, 'TMethodMainChildNestedEnum(1)',      weEnum('mmCN2'),  f).ExpectNotFound;
      t.Add(n, 'TMethodMainChildEnum(1)',            weEnum('mmC2'),   f).ExpectNotFound;
      t.Add(n, 'TMainEnum(1)',                       weEnum('mm2'),    f);
      t.Add(n, 'TMainBaseEnum(1)',                   weEnum('mmB2'),   f);
      t.Add(n, 'TMainGlobEnum(1)',                   weEnum('mmG2'),   f);

      t.Add(n, 'THideMainEnum(1)', weEnum('hmB2'), f)^.AddFlag(ehNotImplementedData);
    end;

    n := AName + ' (Stack: MethodMainBase)';
    f := 4 - AStackOffs;
    if f >= 0 then begin
      t.Add(n, 'Int_MethodMainChildNestedTwice',  30, f).ExpectNotFound;
      t.Add(n, 'Int_MethodMainChildNested'     ,  40, f).ExpectNotFound;
      t.Add(n, 'Int_MethodMainChild'           ,  50, f).ExpectNotFound;
      t.Add(n, 'Int_MethodMainChild_Late'      ,  51, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainChild'           ,  70, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainChild_Prot'      ,  71, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainChild_Priv'      ,  72, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMain'                ,  80, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMain_Prot'           ,  81, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMain_Priv'           ,  82, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainBase'            , 170, f);
      t.Add(n, 'Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'Int_TClassMainBase_Priv'       , 172, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'Int_TClassMainBaseBase_Priv'   , 272, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Int_GlobalPrg'                 , 101, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Int_GlobalUnit1'               , 201, f);
      t.Add(n, 'Int_GlobalUnit2'               , 202, f);

      t.Add(n, 'TClassMain(Self).Int_MethodMainChildNestedTwice',  30, f).ExpectNotFound;
      t.Add(n, 'TClassMain(Self).Int_MethodMainChildNested'     ,  40, f).ExpectNotFound;
      t.Add(n, 'TClassMain(Self).Int_MethodMainChild'           ,  50, f).ExpectNotFound;
      t.Add(n, 'TClassMain(Self).Int_MethodMainChild_Late'      ,  51, f).ExpectNotFound;
      t.Add(n, 'TClassMain(Self).Int_TClassMainChild'           ,  70, f).ExpectNotFound;
      t.Add(n, 'TClassMain(Self).Int_TClassMainChild_Prot'      ,  71, f).ExpectNotFound;
      t.Add(n, 'TClassMain(Self).Int_TClassMainChild_Priv'      ,  72, f).ExpectNotFound;
      t.Add(n, 'TClassMain(Self).Int_TClassMain'                ,  80, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMain_Prot'           ,  81, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMain_Priv'           ,  82, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBase'            , 170, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBase_Priv'       , 172, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'TClassMain(Self).Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'TClassMain(Self).Int_TClassMainBaseBase_Priv'   , 272, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'TClassMain(Self).Int_GlobalPrg'                 , 101, f).ExpectNotFound;
      t.Add(n, 'TClassMain(Self).Int_GlobalUnit1'               , 201, f).ExpectNotFound;
      t.Add(n, 'TClassMain(Self).Int_GlobalUnit2'               , 202, f).ExpectNotFound;

      t.Add(n, 'TClassMainChild(Self).Int_MethodMainChildNestedTwice',  30, f).ExpectNotFound;
      t.Add(n, 'TClassMainChild(Self).Int_MethodMainChildNested'     ,  40, f).ExpectNotFound;
      t.Add(n, 'TClassMainChild(Self).Int_MethodMainChild'           ,  50, f).ExpectNotFound;
      t.Add(n, 'TClassMainChild(Self).Int_MethodMainChild_Late'      ,  51, f).ExpectNotFound;
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainChild'           ,  70, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainChild_Prot'      ,  71, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainChild_Priv'      ,  72, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMain'                ,  80, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMain_Prot'           ,  81, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMain_Priv'           ,  82, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBase'            , 170, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBase_Prot'       , 171, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBase_Priv'       , 172, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'TClassMainChild(Self).Int_TClassMainBaseBase_Priv'   , 272, f).ExpectNotFound.NotImplemented;

      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Class' , 1001, f);
      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Unit' , 1010, f);

      t.Add(n, 'TMethodMainChildNestedTwiceEnum(1)', weEnum('mmCNT2'), f).ExpectNotFound;
      t.Add(n, 'TMethodMainChildNestedEnum(1)',      weEnum('mmCN2'),  f).ExpectNotFound;
      t.Add(n, 'TMethodMainChildEnum(1)',            weEnum('mmC2'),   f).ExpectNotFound;
      t.Add(n, 'TMainEnum(1)',                       weEnum('mm2'),    f).ExpectNotFound.NotImplemented; // found in other unit
      t.Add(n, 'TMainBaseEnum(1)',                   weEnum('mmB2'),   f);
      t.Add(n, 'TMainGlobEnum(1)',                   weEnum('mmG2'),   f).ExpectNotFound.NotImplemented;

      t.Add(n, 'THideMainEnum(1)', weEnum('hmB2'), f);  // found via unit / but otherwise ehNotImplemented;
    end;

    n := AName + ' (Stack: MethodMainBaseBase)';
    f := 5 - AStackOffs;
    if f >= 0 then begin
      t.Add(n, 'Int_MethodMainChildNestedTwice',  30, f).ExpectNotFound;
      t.Add(n, 'Int_MethodMainChildNested'     ,  40, f).ExpectNotFound;
      t.Add(n, 'Int_MethodMainChild'           ,  50, f).ExpectNotFound;
      t.Add(n, 'Int_MethodMainChild_Late'      ,  51, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainChild'           ,  70, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainChild_Prot'      ,  71, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainChild_Priv'      ,  72, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMain'                ,  80, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMain_Prot'           ,  81, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMain_Priv'           ,  82, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainBase'            , 170, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainBase_Prot'       , 171, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainBase_Priv'       , 172, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainBaseBase'        , 270, f);
      t.Add(n, 'Int_TClassMainBaseBase_Prot'   , 271, f);
      t.Add(n, 'Int_TClassMainBaseBase_Priv'   , 272, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Int_GlobalPrg'                 , 101, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Int_GlobalUnit1'               , 201, f).ExpectNotFound.NotImplemented;
      t.Add(n, 'Int_GlobalUnit2'               , 202, f);

      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Class' , 2001, f);
      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Unit' , 2010, f);

      t.Add(n, 'TMethodMainChildNestedTwiceEnum(1)', weEnum('mmCNT2'), f).ExpectNotFound;
      t.Add(n, 'TMethodMainChildNestedEnum(1)',      weEnum('mmCN2'),  f).ExpectNotFound;
      t.Add(n, 'TMethodMainChildEnum(1)',            weEnum('mmC2'),   f).ExpectNotFound;
      t.Add(n, 'TMainEnum(1)',                       weEnum('mm2'),    f).ExpectNotFound.NotImplemented; // found in other unit
      t.Add(n, 'TMainBaseEnum(1)',                   weEnum('mmB2'),   f).ExpectNotFound.NotImplemented; // found in other unit
      t.Add(n, 'TMainGlobEnum(1)',                   weEnum('mmG2'),   f).ExpectNotFound.NotImplemented;

      t.Add(n, 'THideMainEnum(1)', weEnum('x'), f).ExpectNotFound.NotImplemented; // will find main unit
    end;

    n := AName + ' (Stack: main)';
    f := 6 - AStackOffs;
    if f >= 0 then begin
      t.Add(n, 'Int_MethodMainChildNestedTwice',  30, f).ExpectNotFound;
      t.Add(n, 'Int_MethodMainChildNested'     ,  40, f).ExpectNotFound;
      t.Add(n, 'Int_MethodMainChild'           ,  50, f).ExpectNotFound;
      t.Add(n, 'Int_MethodMainChild_Late'      ,  51, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainChild'           ,  70, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainChild_Prot'      ,  71, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainChild_Priv'      ,  72, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMain'                ,  80, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMain_Prot'           ,  81, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMain_Priv'           ,  82, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainBase'            , 170, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainBase_Prot'       , 171, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainBase_Priv'       , 172, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainBaseBase'        , 270, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainBaseBase_Prot'   , 271, f).ExpectNotFound;
      t.Add(n, 'Int_TClassMainBaseBase_Priv'   , 272, f).ExpectNotFound;
      t.Add(n, 'Int_GlobalPrg'                 , 101, f);
      t.Add(n, 'Int_GlobalUnit1'               , 201, f);
      t.Add(n, 'Int_GlobalUnit2'               , 202, f);

      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Class' , 3000, f);
      t.Add(n + '; Hide, view MainChild', 'Int_HideTest_Unit' , 3010, f);

      t.Add(n, 'TMethodMainChildNestedTwiceEnum(1)', weEnum('mmCNT2'), f).ExpectNotFound;
      t.Add(n, 'TMethodMainChildNestedEnum(1)',      weEnum('mmCN2'),  f).ExpectNotFound;
      t.Add(n, 'TMethodMainChildEnum(1)',            weEnum('mmC2'),   f).ExpectNotFound;
      t.Add(n, 'TMainEnum(1)',                       weEnum('mm2'),    f).ExpectNotFound.NotImplemented; // found in unit scope
      t.Add(n, 'TMainBaseEnum(1)',                   weEnum('mmB2'),   f).ExpectNotFound.NotImplemented; // found in unit scope
      t.Add(n, 'TMainGlobEnum(1)',                   weEnum('mmG2'),   f);

      t.Add(n, 'THideMainEnum(1)', weEnum('hmG2'), f)^.AddFlag([ehNotImplementedData]); // may find the class scope
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
        t.Add(n, 'Result',  'abc2', f).IgnKindPtr.IgnKind(stDwarf3Up);;
      end
      else begin
        t.Add(n, 'Int_Hide_Foo',  4, f);
        t.Add(n, 'Result',  'abc', f).IgnKindPtr.IgnKind(stDwarf3Up);;
      end;
    end;

    n := AName + ' FooNested';
    f := 1 - AStackOffs;
    if f >= 0 then begin
      t.Add(n, 'Int_Hide_Foo',  3, f);
      t.Add(n, 'Result',  'bar', f).IgnKindPtr.IgnKind(stDwarf3Up);;
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
      t.Add(n, 'Result',  0, f).ExpectNotFound;
    end;

  end;

var
  ExeName: String;
  dbg: TDebuggerIntf;
  t: TWatchExpectationList;
  Src: TCommonSource;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestWatchScope) then exit;
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

    (* ************ Nested Functions ************* *)

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

    (* ************ Class ************* *)

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

    (* ************ Program level ************* *)

    dbg.Run;
    Debugger.WaitForFinishRun();
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

procedure TTestWatches.TestWatchesValue;

  type
    TTestLoc = (tlAny, tlConst, tlParam, tlArrayWrap);

  procedure AddWatches(t: TWatchExpectationList; AName: String; APrefix: String; AOffs: Integer; AChr1: Char;
    ALoc: TTestLoc = tlAny; APostFix: String = '');
  var
    p, e: String;
    n: Integer;
  begin
    p := APrefix;
    e := APostFix;
    n := AOffs;

    t.Add(AName, p+'Byte'+e,       weCardinal(1+n,                    'Byte',     1));
    t.Add(AName, p+'Word'+e,       weCardinal(100+n,                  'Word',     2));
    t.Add(AName, p+'Longword'+e,   weCardinal(1000+n,                 'Longword', 4));
    t.Add(AName, p+'QWord'+e,      weCardinal(10000+n,                'QWord',    5));
    t.Add(AName, p+'Shortint'+e,   weInteger (50+n,                   'Shortint', 1));
    t.Add(AName, p+'Smallint'+e,   weInteger (500+n,                  'Smallint', 2));
    t.Add(AName, p+'Longint'+e,    weInteger (5000+n,                 'Longint',  4));
    t.Add(AName, p+'Int64'+e,      weInteger (50000+n,                'Int64',    8));
    t.Add(AName, p+'IntRange'+e,   weInteger (-50+n,                  'TIntRange',0));
    t.Add(AName, p+'CardinalRange'+e, weInteger(50+n,                 'TCardinalRange',0));

    t.Add(AName, p+'Byte_2'+e,     weCardinal(240+n,                  'Byte',     1));
    t.Add(AName, p+'Word_2'+e,     weCardinal(65501+n,                'Word',     2));
    t.Add(AName, p+'Longword_2'+e, weCardinal(4123456789+n,           'Longword', 4));
    t.Add(AName, p+'QWord_2'+e,    weCardinal(15446744073709551610+n, 'QWord',    8));
    t.Add(AName, p+'Shortint_2'+e, weInteger (112+n,                  'Shortint', 1));
    t.Add(AName, p+'Smallint_2'+e, weInteger (32012+n,                'Smallint', 2));
    t.Add(AName, p+'Longint_2'+e,  weInteger (20123456+n,             'Longint',  4));
    t.Add(AName, p+'Int64_2'+e,    weInteger (9123372036854775801+n,  'Int64',    8));

    t.Add(AName, p+'Shortint_3'+e, weInteger(-112+n,                 'Shortint', 1));
    t.Add(AName, p+'Smallint_3'+e, weInteger(-32012+n,               'Smallint', 2));
    t.Add(AName, p+'Longint_3'+e,  weInteger(-20123456+n,            'Longint',  4));
    t.Add(AName, p+'Int64_3'+e,    weInteger(-9123372036854775801+n, 'Int64',    8));

    t.Add(AName, p+'Real'+e,       weFloat(50.25+n,                 'Real'       ));
    t.Add(AName, p+'Single'+e,     weSingle(100.125+n,              'Single'     ));
    t.Add(AName, p+'Double'+e,     weDouble(1000.125+n,             'Double'     ));
    t.Add(AName, p+'Extended'+e,   weFloat(10000.175+n,             ''   )); // Double ?
    //t.Add(p+'Comp'+e,       weInteger(150.125+n,              'Comp'       ));
    t.Add(AName, p+'Currency'+e,   weFloat(125.123+n,               'Currency'   ))^.AddFlag([ehNotImplementedData]);

    t.Add(AName, p+'Real_2'+e,     weFloat(-50.25+n,                'Real'       ));
    t.Add(AName, p+'Single_2'+e,   weSingle(-100.125+n,             'Single'     ));
    t.Add(AName, p+'Double_2'+e,   weDouble(-1000.125+n,            'Double'     ));
    t.Add(AName, p+'Extended_2'+e, weFloat(-10000.175+n,            ''   )); // Double ?
    //t.Add(p+'Comp_2'+e,     weFloat(-150.125+n,             'Comp'       ));
    t.Add(AName, p+'Currency_2'+e, weFloat(-125.123+n,              'Currency'   ))^.AddFlag([ehNotImplementedData]);

    t.Add(AName, p+'Char'+e,       weChar(AChr1));
    t.Add(AName, p+'Char2'+e,      weChar(#0));
    t.Add(AName, p+'Char3'+e,      weChar(' '));

//if not(ALoc in [tlConst, tlArrayWrap]) then begin
if not(ALoc in [tlConst]) then begin
    t.Add(AName, p+'String1'+e,    weShortStr(AChr1, 'ShortStr1'));
    t.Add(AName, p+'String1e'+e,   weShortStr('',    'ShortStr1'));
    t.Add(AName, p+'String10'+e,   weShortStr(AChr1+'bc1',               'ShortStr10'));
    t.Add(AName, p+'String10e'+e,  weShortStr('',                        'ShortStr10'));
    t.Add(AName, p+'String10x'+e,  weShortStr(AChr1+'S'#0'B'#9'b'#10#13, 'ShortStr10'));
    t.Add(AName, p+'String255'+e,  weShortStr(AChr1+'bcd0123456789', 'ShortStr255'));

    t.Add(AName, p+'Ansi1'+e,      weAnsiStr(Succ(AChr1)))     .IgnKindPtr(stDwarf2).IgnKind(stDwarf3Up);
    t.Add(AName, p+'Ansi2'+e,      weAnsiStr(AChr1+'abcd0123')).IgnKindPtr(stDwarf2).IgnKind(stDwarf3Up);
    t.Add(AName, p+'Ansi3'+e,      weAnsiStr(''))              .IgnKindPtr(stDwarf2).IgnKind(stDwarf3Up);
    t.Add(AName, p+'Ansi4'+e,      weAnsiStr(AChr1+'A'#0'B'#9'b'#10#13))  // cut off at #0 in dwarf2
             .IgnKindPtr(stDwarf2).IgnData(stDwarf2).IgnKind(stDwarf3Up);
    t.Add(AName, p+'Ansi5'+e,      weAnsiStr(AChr1+'bcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghij'
      ) )    .IgnKindPtr(stDwarf2)                  .IgnKind(stDwarf3Up);


//TODO wePchar
    t.Add(AName, p+'PChar'+e,      wePointer(weAnsiStr(''), 'PChar'));
    t.Add(AName, p+'PChar2'+e,     wePointer(weAnsiStr(AChr1+'abcd0123'), 'TPChr')).SkipIf(ALoc = tlConst);

    // char by index
    // TODO: no typename => calculated value ?
    t.Add(AName, p+'String10'+e+'[2]',   weChar('b', '')).CharFromIndex;
    t.Add(AName, p+'Ansi2'+e+'[2]',      weChar('a', '')).CharFromIndex;
    t.Add(AName, p+'PChar2'+e+'[1]',     weChar('a', '')).CharFromIndex.SkipIf(ALoc = tlConst);
    t.Add(AName, p+'String10'+e+'[1]',   weChar(AChr1, '')).CharFromIndex;
    t.Add(AName, p+'Ansi2'+e+'[1]',      weChar(AChr1, '')).CharFromIndex;
    t.Add(AName, p+'PChar2'+e+'[0]',     weChar(AChr1, '')).CharFromIndex.SkipIf(ALoc = tlConst);


    t.Add(AName, p+'WideChar'+e,       weChar(AChr1)); // TODO: widechar
    t.Add(AName, p+'WideChar2'+e,      weChar(#0));
    t.Add(AName, p+'WideChar3'+e,      weChar(' '));

    t.Add(AName, p+'WideString1'+e,    weWideStr(Succ(AChr1)))              .IgnKindPtr;
    t.Add(AName, p+'WideString2'+e,    weWideStr(AChr1+'abcX0123'))         .IgnKindPtr;
    t.Add(AName, p+'WideString3'+e,    weWideStr(''))                       .IgnKindPtr;
    t.Add(AName, p+'WideString4'+e,    weWideStr(AChr1+'A'#0'X'#9'b'#10#13)).IgnKindPtr.IgnData; // cut off at #0
    t.Add(AName, p+'WideString5'+e,    weWideStr(AChr1+'XcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghij',
      'TWStrTA'))                                                         .IgnKindPtr;

    t.Add(AName, p+'WideString2'+e+'[1]',  weWideChar(AChr1))     .CharFromIndex;
    t.Add(AName, p+'WideString2'+e+'[2]',  weWideChar('a'))       .CharFromIndex;
    t.Add(AName, p+'WideString5'+e+'[1]',  weWideChar(AChr1))     .CharFromIndex;
    t.Add(AName, p+'WideString5'+e+'[2]',  weWideChar('X'))       .CharFromIndex;

//TODO wePWidechar
    t.Add(AName, p+'PWideChar'+e,      wePointer(weWideStr(''), 'PWideChar'));
    t.Add(AName, p+'PWideChar2'+e,     wePointer(weWideStr(AChr1+'abcX0123'), 'TPWChr')).SkipIf(ALoc = tlConst);

    t.Add(AName, p+'UnicodeString1'+e,    weUniStr(Succ(AChr1)))              .IgnKindPtr(stDwarf2);
    t.Add(AName, p+'UnicodeString2'+e,    weUniStr(AChr1+'aBcX0123'))         .IgnKindPtr(stDwarf2);
    t.Add(AName, p+'UnicodeString3'+e,    weUniStr(''))                       .IgnKindPtr(stDwarf2);
    t.Add(AName, p+'UnicodeString4'+e,    weUniStr(AChr1+'B'#0'X'#9'b'#10#13)).IgnKindPtr(stDwarf2).IgnData(stDwarf2); // #00 terminated in dwarf2
    t.Add(AName, p+'UnicodeString5'+e,    weUniStr(AChr1+'YcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghij',
      'TUStrTA'))                                                         .IgnKindPtr(stDwarf2);

//todo dwarf 3
    t.Add(AName, p+'UnicodeString2'+e+'[1]',    weWideChar(AChr1))       .CharFromIndex(stDwarf2).IgnTypeName(stDwarf3Up);
    t.Add(AName, p+'UnicodeString2'+e+'[2]',    weWideChar('a'))         .CharFromIndex(stDwarf2).IgnTypeName(stDwarf3Up);
    t.Add(AName, p+'UnicodeString5'+e+'[1]',    weWideChar(AChr1))       .CharFromIndex(stDwarf2).IgnTypeName(stDwarf3Up);
    t.Add(AName, p+'UnicodeString5'+e+'[2]',    weWideChar('Y'))         .CharFromIndex(stDwarf2).IgnTypeName(stDwarf3Up);


    // TODO
    t.Add(AName, p+'ShortRec'+e,     weMatch(''''+AChr1+''', *''b'', *'''+AChr1+'''', skRecord));


    t.add(AName, p+'CharDynArray'+e,  weDynArray([]                                        ));
    t.add(AName, p+'CharDynArray2'+e, weDynArray(weChar(['N', AChr1, 'M'])                 )).SkipIf(ALoc = tlConst);
    t.add(AName, p+'CharDynArray3'+e, weDynArray([],                        'TCharDynArray'));
    t.Add(AName, p+'CharDynArray4'+e, weDynArray(weChar(['J', AChr1, 'M']), 'TCharDynArray')).SkipIf(ALoc = tlConst);

    t.Add(AName, p+'WCharDynArray'+e, weDynArray([]                        ));
    t.Add(AName, p+'WCharDynArray2'+e,weDynArray(weChar(['W', AChr1, 'M']) )).SkipIf(ALoc = tlConst);
    t.Add(AName, p+'WCharDynArray3'+e,weDynArray([]                        ));
    t.Add(AName, p+'WCharDynArray4'+e,weDynArray(weChar(['K', AChr1, 'M']) )).SkipIf(ALoc = tlConst);

    t.add(AName, p+'IntDynArray'+e,   weDynArray([]                                           ));
    t.add(AName, p+'IntDynArray2'+e,  weDynArray(weInteger([11, 30+AOffs, 60])                )).SkipIf(ALoc = tlConst);
    t.add(AName, p+'IntDynArray3'+e,  weDynArray([],                            'TIntDynArray'));
    t.Add(AName, p+'IntDynArray4'+e,  weDynArray(weInteger([12, 30+AOffs, 60]), 'TIntDynArray')).SkipIf(ALoc = tlConst);

    t.add(AName, p+'AnsiDynArray'+e,  weDynArray([]                                                     ));
    t.add(AName, p+'AnsiDynArray2'+e, weDynArray(weAnsiStr(['N123', AChr1+'ab', 'M'#9])                 )).SkipIf(ALoc = tlConst);
    t.add(AName, p+'AnsiDynArray3'+e, weDynArray([],                                     'TAnsiDynArray'));
    t.Add(AName, p+'AnsiDynArray4'+e, weDynArray(weAnsiStr(['J123', AChr1+'ab', 'M'#9]), 'TAnsiDynArray')).SkipIf(ALoc = tlConst);

    t.add(AName, p+'ShortStrDynArray'+e,  weDynArray([]                                                          ));
    t.add(AName, p+'ShortStrDynArray2'+e, weDynArray(weShortStr(['N123', AChr1+'ac', 'M'#9])                     ))
      .SkipIf(ALoc = tlConst);
    t.add(AName, p+'ShortStrDynArray3'+e, weDynArray([],                                      'TShortStrDynArray'));
    t.Add(AName, p+'ShortStrDynArray4'+e, weDynArray(weShortStr(['J123', AChr1+'ac', 'M'#9]), 'TShortStrDynArray'))
      .SkipIf(ALoc = tlConst);


    t.Add(AName, p+'CharStatArray'+e,  weStatArray(weChar([AChr1, 'b', AChr1, 'B', 'c'])                  ))
      .SkipIf(ALoc = tlParam);
    t.Add(AName, p+'CharStatArray2'+e, weStatArray(weChar([AChr1, 'c', AChr1, 'B', 'c']), 'TCharStatArray'));

    t.Add(AName, p+'WCharStatArray'+e, weStatArray(weChar([AChr1, 'b', AChr1, 'B', 'd'])                   ))
      .SkipIf(ALoc = tlParam);
    t.Add(AName, p+'WCharStatArray2'+e,weStatArray(weChar([AChr1, 'c', AChr1, 'B', 'd']), 'TwCharStatArray'));

    t.Add(AName, p+'IntStatArray'+e,  weStatArray(weInteger([-1, 300+AOffs, 2, 0, 1])                 ))
      .SkipIf(ALoc = tlParam);
    t.Add(AName, p+'IntStatArray2'+e, weStatArray(weInteger([-2, 200+AOffs, 2, 0, 1]), 'TIntStatArray'));

    t.Add(AName, p+'AnsiStatArray'+e,  weStatArray(weAnsiStr([AChr1, 'b123', AChr1+'ab', 'B', 'cdef'#9])                  ))
      .SkipIf(ALoc = tlParam);
    t.Add(AName, p+'AnsiStatArray2'+e, weStatArray(weAnsiStr([AChr1, 'c123', AChr1+'ad', 'D', 'cxx'#9] ), 'TAnsiStatArray'));

    t.Add(AName, p+'ShortStrStatArray'+e,  weStatArray(weShortStr([AChr1, 'b123', AChr1+'ab', 'C', 'cdef'#9])                  ))
      .SkipIf(ALoc = tlParam);
    t.Add(AName, p+'ShortStrStatArray2'+e, weStatArray(weShortStr([AChr1, 'c123', AChr1+'ad', 'C', 'cxx'#9] ), 'TShortStrStatArray'));


    t.Add(AName, p+'DynDynArrayInt'+e, weDynArray([
        weDynArray(weInteger([11+AOffs,0,-22])),
        weDynArray(weInteger([110+AOffs])),
        weDynArray(weInteger([11+AOffs,0,-22])),
        weDynArray(weInteger([])),
        weDynArray(weInteger([11,12,11,10]))
      ], 'TDynDynArrayInt'));

if not(ALoc in [tlArrayWrap]) then begin
    t.Add(AName, p+'DynDynArrayInt'+e+'[0]', weDynArray(weInteger([11+AOffs,0,-22])) );
    t.Add(AName, p+'DynDynArrayInt'+e+'[1]', weDynArray(weInteger([110+AOffs])) );
    t.Add(AName, p+'DynDynArrayInt'+e+'[2]', weDynArray(weInteger([11+AOffs,0,-22])) );
    t.Add(AName, p+'DynDynArrayInt'+e+'[3]', weDynArray(weInteger([])) );
    t.Add(AName, p+'DynDynArrayInt'+e+'[4]', weDynArray(weInteger([11,12,11,10])) );

    t.Add(AName, p+'DynDynArrayInt2'+e+'[0]', weDynArray(weInteger([11+AOffs,0,-22])) );
    t.Add(AName, p+'DynDynArrayInt2'+e+'[1]', weDynArray(weInteger([110+AOffs])) );
    t.Add(AName, p+'DynDynArrayInt2'+e+'[2]', weDynArray(weInteger([11+AOffs,0,-22])) );
    t.Add(AName, p+'DynDynArrayInt2'+e+'[3]', weDynArray(weInteger([])) );
    t.Add(AName, p+'DynDynArrayInt2'+e+'[4]', weDynArray(weInteger([11,12,11,10])) );
end;


/////    t.Add(AName, p+'pre__FiveDynArray'+e,  weStatArray(weChar([AChr1, 'b', AChr1, 'B', 'c'])                  ))

//TODO: element by index

end;

  t.Add(AName, p+'Enum'+e, weEnum('EnVal3', 'TEnum'));
  t.Add(AName, p+'Enum1'+e, weEnum('EnVal2', 'TEnumSub'));
  t.Add(AName, p+'Enum2'+e, weEnum('EnVal21', 'TEnum2'));
  t.Add(AName, p+'Enum3'+e, weEnum('EnVal25', 'TEnum2'));

  t.Add(AName, p+'Set'+e, weSet(['EnVal2', 'EnVal4'], 'TSet'));

  t.Add(AName, p+'IntfUnknown'+e, weMatch('.?', skInterface)).Skip(); // only run eval / do not crash

  end;

var
  ExeName: String;
  dbg: TDebuggerIntf;
  t: TWatchExpectationList;
  Src: TCommonSource;
  n, cl: Integer;
  BrkPrg, BrkFooBegin, BrkFoo, BrkFooVar, BrkFooVarBegin,
    BrkFooConstRef: TDBGBreakPoint;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestWatchValue) then exit;
  t := nil;

  Src := GetCommonSourceFor('WatchesValuePrg.Pas');
  TestCompile(Src, ExeName);

  AssertTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));
  dbg := Debugger.LazDebugger;

  try
    t := TWatchExpectationList.Create(Self);
    t.AcceptSkSimple := [skInteger, skCardinal, skBoolean, skChar, skFloat, skString, skAnsiString, skCurrency, skVariant, skWideString];
    t.AddTypeNameAlias('integer', 'integer|longint');
    t.AddTypeNameAlias('ShortStr255', 'ShortStr255|ShortString');
    t.AddTypeNameAlias('TEnumSub', 'TEnum|TEnumSub');


    BrkPrg         := Debugger.SetBreakPoint(Src, 'Prg');
    BrkFooBegin    := Debugger.SetBreakPoint(Src, 'FooBegin');
    BrkFoo         := Debugger.SetBreakPoint(Src, 'Foo');
    BrkFooVarBegin := Debugger.SetBreakPoint(Src, 'FooVarBegin');
    BrkFooVar      := Debugger.SetBreakPoint(Src, 'FooVar');
    BrkFooConstRef := Debugger.SetBreakPoint(Src, 'FooConstRef');
    AssertDebuggerNotInErrorState;

    (* ************ Nested Functions ************* *)

    dbg.Run;
    Debugger.WaitForFinishRun();
    AssertDebuggerState(dsPause);
    BrkPrg.Enabled := False;
    TestLogger.debugln(['line: ',Debugger.LazDebugger.GetLocation.SrcLine]);
    // At BreakPoint: Prg
    t.Clear;

//t.Add( 'gvaString10[1]',   weShortStr('Lbc1',               'ShortStr10'));
//t.Add( 'gvString10',   weShortStr('Bbc1',               'ShortStr10'));
//t.Add( 'gvaShortRec[1]',       weMatch('''L'', *''b'', *''L''', skRecord));
//t.Add( 'gvAnsiDynArray2',   weShortStr('Lbc1',               'ShortStr10'));
//t.Add( 'gvaDynDynArrayInt[0]',   weShortStr('Lbc1',               'ShortStr10'));
//t.Add( 'argCharDynArray',   weShortStr('Lbc1',               'ShortStr10'));
//t.Add( 'gvUnicodeString2',    weWideStr('BaBcX0123'))         ;
//t.Add('gvUnicodeString2[1]',    weWideChar('B')) ;
//t.EvaluateWatches;
//t.CheckResults;
//exit;

    AddWatches(t, 'glob const', 'gc', 000, 'A', tlConst);
    AddWatches(t, 'glob var',   'gv', 001, 'B');
    AddWatches(t, 'glob MyClass1',     'MyClass1.mc',  002, 'C');
    AddWatches(t, 'glob MyBaseClass1', 'MyClass1.mbc', 003, 'D');
    AddWatches(t, 'glob MyClass1',     'TMyClass(MyClass2).mc',  004, 'E');
    AddWatches(t, 'glob MyBaseClass1', 'TMyClass(MyClass2).mbc', 005, 'F');
    AddWatches(t, 'glob var dyn array of [0]',   'gva', 005, 'K', tlArrayWrap, '[0]' );
    AddWatches(t, 'glob var dyn array of [1]',   'gva', 006, 'L', tlArrayWrap, '[1]');
    t.EvaluateWatches;
    t.CheckResults;


    dbg.Run;
    Debugger.WaitForFinishRun();
    AssertDebuggerState(dsPause);
    BrkFooBegin.Enabled := False;
    TestLogger.debugln(['line: ',Debugger.LazDebugger.GetLocation.SrcLine]);
    // At BreakPoint: FooBegin
    t.Clear;
    AddWatches(t, 'fooBegin local', 'fooloc', 002, 'C');
    AddWatches(t, 'fooBegin args', 'arg', 001, 'B', tlParam);
    t.EvaluateWatches;
    // Do not check values. // Just ensure no crash occurs
    // Registers are wrong in prologue.


    cl := Debugger.LazDebugger.GetLocation.SrcLine;
    dbg.Run;
    Debugger.WaitForFinishRun();
    //// below might have been caused by the break on FooVarBegin, if there was no code.
    //if (cl > 1) and (cl = Debugger.LazDebugger.GetLocation.SrcLine) then begin dbg.Run; Debugger.WaitForFinishRun(); end; // TODO: bug, stopping twice the same breakpoint
    AssertDebuggerState(dsPause);
    BrkFoo.Enabled := False;
    TestLogger.debugln(['line: ',Debugger.LazDebugger.GetLocation.SrcLine]);
    // At BreakPoint: Foo
    t.Clear;
    AddWatches(t, 'foo local', 'fooloc', 002, 'C');
    AddWatches(t, 'foo args', 'arg', 001, 'B', tlParam);
    AddWatches(t, 'foo ArgMyClass1',     'ArgMyClass1.mc',  002, 'C');
    AddWatches(t, 'foo ArgMyBaseClass1', 'ArgMyClass1.mbc', 003, 'D');
    AddWatches(t, 'foo ArgMyClass1',     'TMyClass(ArgMyClass2).mc',  004, 'E');
    AddWatches(t, 'foo ArgMyBaseClass1', 'TMyClass(ArgMyClass2).mbc', 005, 'F');
    t.EvaluateWatches;
    t.CheckResults;


    dbg.Run;
    Debugger.WaitForFinishRun();
    AssertDebuggerState(dsPause);
    BrkFooVarBegin.Enabled := False;
    TestLogger.debugln(['line: ',Debugger.LazDebugger.GetLocation.SrcLine]);
    // At BreakPoint: FooVarBegin
    t.Clear;
    AddWatches(t, 'foo var args', 'argvar', 001, 'B', tlParam);
    t.EvaluateWatches;
    // Do not check values. // Just ensure no crash occurs
    // Registers are wrong in prologue.


    dbg.Run;
    Debugger.WaitForFinishRun();
    //if (cl > 1) and (cl = Debugger.LazDebugger.GetLocation.SrcLine) then begin dbg.Run; Debugger.WaitForFinishRun(); end; // TODO: bug, stopping twice the same breakpoint
    AssertDebuggerState(dsPause);
    BrkFooVar.Enabled := False;
    TestLogger.debugln(['line: ',Debugger.LazDebugger.GetLocation.SrcLine]);
    // At BreakPoint: FooVar
    t.Clear;
    AddWatches(t, 'foo var args', 'argvar', 001, 'B', tlParam);
    AddWatches(t, 'foo var ArgMyClass1',     'ArgVarMyClass1.mc',  002, 'C');
    AddWatches(t, 'foo var ArgMyBaseClass1', 'ArgVarMyClass1.mbc', 003, 'D');
    AddWatches(t, 'foo var ArgMyClass1',     'TMyClass(ArgVarMyClass2).mc',  004, 'E');
    AddWatches(t, 'foo var ArgMyBaseClass1', 'TMyClass(ArgVarMyClass2).mbc', 005, 'F');
    t.EvaluateWatches;
    t.CheckResults;


    dbg.Run;
    Debugger.WaitForFinishRun();
    AssertDebuggerState(dsPause);
    BrkFooConstRef.Enabled := False;
    TestLogger.debugln(['line: ',Debugger.LazDebugger.GetLocation.SrcLine]);
    // At BreakPoint: FooConstRef;
    t.Clear;
    AddWatches(t, 'foo const ref args', 'argconstref', 001, 'B', tlParam);
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
  ControlTestWatchScope    := TestControlRegisterTest('Scope', ControlTestWatch);
  ControlTestWatchValue    := TestControlRegisterTest('Value', ControlTestWatch);

end.

