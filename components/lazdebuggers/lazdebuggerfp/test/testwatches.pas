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

      t.Add(n, 'TMethodMainChildNestedTwiceEnum(1)', weEnum('mmCNT2'), f)^.AddFlag(ehExpectNotFound);
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

      t.Add(n, 'TMethodMainChildNestedTwiceEnum(1)', weEnum('mmCNT2'), f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TMethodMainChildNestedEnum(1)',      weEnum('mmCN2'),  f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TMethodMainChildEnum(1)',            weEnum('mmC2'),   f);
      t.Add(n, 'TMainEnum(1)',                       weEnum('mm2'),    f);
      t.Add(n, 'TMainBaseEnum(1)',                   weEnum('mmB2'),   f);
      t.Add(n, 'TMainGlobEnum(1)',                   weEnum('mmG2'),   f);

      t.Add(n, 'THideMainEnum(1)', weEnum('hmC2'), f);
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

      t.Add(n, 'TMethodMainChildNestedTwiceEnum(1)', weEnum('mmCNT2'), f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TMethodMainChildNestedEnum(1)',      weEnum('mmCN2'),  f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TMethodMainChildEnum(1)',            weEnum('mmC2'),   f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TMainEnum(1)',                       weEnum('mm2'),    f);
      t.Add(n, 'TMainBaseEnum(1)',                   weEnum('mmB2'),   f);
      t.Add(n, 'TMainGlobEnum(1)',                   weEnum('mmG2'),   f);

      t.Add(n, 'THideMainEnum(1)', weEnum('hmB2'), f)^.AddFlag(ehNotImplementedData);
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

      t.Add(n, 'TMethodMainChildNestedTwiceEnum(1)', weEnum('mmCNT2'), f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TMethodMainChildNestedEnum(1)',      weEnum('mmCN2'),  f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TMethodMainChildEnum(1)',            weEnum('mmC2'),   f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TMainEnum(1)',                       weEnum('mm2'),    f)^.AddFlag([ehExpectNotFound, ehNotImplemented]); // found in other unit
      t.Add(n, 'TMainBaseEnum(1)',                   weEnum('mmB2'),   f);
      t.Add(n, 'TMainGlobEnum(1)',                   weEnum('mmG2'),   f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);

      t.Add(n, 'THideMainEnum(1)', weEnum('hmB2'), f);  // found via unit / but otherwise ehNotImplemented;
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

      t.Add(n, 'TMethodMainChildNestedTwiceEnum(1)', weEnum('mmCNT2'), f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TMethodMainChildNestedEnum(1)',      weEnum('mmCN2'),  f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TMethodMainChildEnum(1)',            weEnum('mmC2'),   f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TMainEnum(1)',                       weEnum('mm2'),    f)^.AddFlag([ehExpectNotFound, ehNotImplemented]); // found in other unit
      t.Add(n, 'TMainBaseEnum(1)',                   weEnum('mmB2'),   f)^.AddFlag([ehExpectNotFound, ehNotImplemented]); // found in other unit
      t.Add(n, 'TMainGlobEnum(1)',                   weEnum('mmG2'),   f)^.AddFlag([ehExpectNotFound, ehNotImplemented]);

      t.Add(n, 'THideMainEnum(1)', weEnum('x'), f)^.AddFlag([ehExpectNotFound, ehNotImplemented]); // will find main unit
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

      t.Add(n, 'TMethodMainChildNestedTwiceEnum(1)', weEnum('mmCNT2'), f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TMethodMainChildNestedEnum(1)',      weEnum('mmCN2'),  f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TMethodMainChildEnum(1)',            weEnum('mmC2'),   f)^.AddFlag(ehExpectNotFound);
      t.Add(n, 'TMainEnum(1)',                       weEnum('mm2'),    f)^.AddFlag([ehExpectNotFound, ehNotImplemented]); // found in unit scope
      t.Add(n, 'TMainBaseEnum(1)',                   weEnum('mmB2'),   f)^.AddFlag([ehExpectNotFound, ehNotImplemented]); // found in unit scope
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

  //function IncStr(s: AnsiString; n: Integer): AnsiString;
  //begin
  //  Result := s;
  //  if s <> '' then s[1] := char(ord(s[1])+n);
  //end;

  procedure AddWatches(t: TWatchExpectationList; AName: String; APrefix: String; AOffs: Integer; AChr1: Char);
  var
    p: String;
    n: Integer;
  begin
    p := APrefix;
    n := AOffs;

    t.Add(AName, p+'Byte',       weCardinal(1+n,                    'Byte',     1));
    t.Add(AName, p+'Word',       weCardinal(100+n,                  'Word',     2));
    t.Add(AName, p+'Longword',   weCardinal(1000+n,                 'Longword', 4));
    t.Add(AName, p+'QWord',      weCardinal(10000+n,                'QWord',    5));
    t.Add(AName, p+'Shortint',   weInteger (50+n,                   'Shortint', 1));
    t.Add(AName, p+'Smallint',   weInteger (500+n,                  'Smallint', 2));
    t.Add(AName, p+'Longint',    weInteger (5000+n,                 'Longint',  4));
    t.Add(AName, p+'Int64',      weInteger (50000+n,                'Int64',    8));

    t.Add(AName, p+'Byte_2',     weCardinal(250+n,                  'Byte',     1));
    t.Add(AName, p+'Word_2',     weCardinal(65501+n,                'Word',     2));
    t.Add(AName, p+'Longword_2', weCardinal(4123456789+n,           'Longword', 4));
    t.Add(AName, p+'QWord_2',    weCardinal(15446744073709551610+n, 'QWord',    8));
    t.Add(AName, p+'Shortint_2', weInteger (122+n,                  'Shortint', 1));
    t.Add(AName, p+'Smallint_2', weInteger (32012+n,                'Smallint', 2));
    t.Add(AName, p+'Longint_2',  weInteger (20123456+n,             'Longint',  4));
    t.Add(AName, p+'Int64_2',    weInteger (9123372036854775801+n,  'Int64',    8));

    t.Add(AName, p+'Shortint_3', weInteger(-122+n,                 'Shortint', 1));
    t.Add(AName, p+'Smallint_3', weInteger(-32012+n,               'Smallint', 2));
    t.Add(AName, p+'Longint_3',  weInteger(-20123456+n,            'Longint',  4));
    t.Add(AName, p+'Int64_3',    weInteger(-9123372036854775801+n, 'Int64',    8));

    t.Add(AName, p+'Real',       weFloat(50.25+n,                 'Real'       ));
    t.Add(AName, p+'Single',     weSingle(100.125+n,              'Single'     ));
    t.Add(AName, p+'Double',     weDouble(1000.125+n,             'Double'     ));
    t.Add(AName, p+'Extended',   weFloat(10000.175+n,             ''   )); // Double ?
    //t.Add(p+'Comp',       weInteger(150.125+n,              'Comp'       ));
    t.Add(AName, p+'Currency',   weFloat(125.123+n,               'Currency'   ))^.AddFlag([ehNotImplementedData]);

    t.Add(AName, p+'Real_2',     weFloat(-50.25+n,                'Real'       ));
    t.Add(AName, p+'Single_2',   weSingle(-100.125+n,             'Single'     ));
    t.Add(AName, p+'Double_2',   weDouble(-1000.125+n,            'Double'     ));
    t.Add(AName, p+'Extended_2', weFloat(-10000.175+n,            ''   )); // Double ?
    //t.Add(p+'Comp_2',     weFloat(-150.125+n,             'Comp'       ));
    t.Add(AName, p+'Currency_2', weFloat(-125.123+n,              'Currency'   ))^.AddFlag([ehNotImplementedData]);

    t.Add(AName, p+'Char',       weChar(AChr1));
    t.Add(AName, p+'Char2',      weChar(#0));
    t.Add(AName, p+'Char3',      weChar(' '));

if not(p = 'gc') then begin
    t.Add(AName, p+'String1',    weShortStr(AChr1, 'ShortStr1'));
    t.Add(AName, p+'String1e',   weShortStr('',    'ShortStr1'));
    t.Add(AName, p+'String10',   weShortStr(AChr1+'bc1',               'ShortStr10'));
    t.Add(AName, p+'String10e',  weShortStr('',                        'ShortStr10'));
    t.Add(AName, p+'String10x',  weShortStr(AChr1+'S'#0'B'#9'b'#10#13, 'ShortStr10'));
    t.Add(AName, p+'String255',  weShortStr(AChr1+'bcd0123456789', 'ShortStr255'));

    t.Add(AName, p+'Ansi1',      weAnsiStr(Succ(AChr1)))^.AddFlag([ehIgnKindPtr]);
    t.Add(AName, p+'Ansi2',      weAnsiStr(AChr1+'abcd0123'))^.AddFlag([ehIgnKindPtr]);
    t.Add(AName, p+'Ansi3',      weAnsiStr(''))^.AddFlag([ehIgnKindPtr]);
    t.Add(AName, p+'Ansi4',      weAnsiStr(AChr1+'A'#0'B'#9'b'#10#13))^.AddFlag([ehIgnKindPtr, ehIgnData]); // cut off at #0
    t.Add(AName, p+'Ansi5',      weAnsiStr(AChr1+'bcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghij'
      ) )^.AddFlag([ehIgnKindPtr]);

//TODO wePchar
    t.Add(AName, p+'PChar',      wePointer(weAnsiStr(''), 'PChar'));

    t.Add(AName, p+'WideChar',       weChar(AChr1));
    t.Add(AName, p+'WideChar2',      weChar(#0));
    t.Add(AName, p+'WideChar3',      weChar(' '));

    t.Add(AName, p+'WideString1',    weWideStr(Succ(AChr1)))^.AddFlag([ehIgnKindPtr]);
    t.Add(AName, p+'WideString2',    weWideStr(AChr1+'abcX0123'))^.AddFlag([ehIgnKindPtr]);
    t.Add(AName, p+'WideString3',    weWideStr(''))^.AddFlag([ehIgnKindPtr]);
    t.Add(AName, p+'WideString4',    weWideStr(AChr1+'A'#0'X'#9'b'#10#13))^.AddFlag([ehIgnKindPtr, ehIgnData]); // cut off at #0
    t.Add(AName, p+'WideString5',    weWideStr(AChr1+'XcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghij',
      'TWStrTA'))^.AddFlag([ehIgnKindPtr]);

//TODO wePWidechar
    t.Add(AName, p+'PWideChar',      wePointer(weAnsiStr(''), 'PWideChar'));

    // TODO
    t.Add(AName, p+'ShortRec',     weMatch(''''+AChr1+''', *''b'', *'''+AChr1+'''', skRecord));
    t.Add(AName, p+'CharDynArray', weMatch('\(\)|nil', skArray));
    t.Add(AName, p+'WCharDynArray', weMatch('\(\)|nil', skArray));
    t.Add(AName, p+'CharStatArray', weMatch('\('''+AChr1+''', *''b'', *'''+AChr1+'''', skArray));
    t.Add(AName, p+'WCharStatArray', weMatch('\('''+AChr1+''', *''b'', *'''+AChr1+'''', skArray));
end;

  end;

var
  ExeName: String;
  dbg: TDebuggerIntf;
  t: TWatchExpectationList;
  Src: TCommonSource;
  n: Integer;
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


    Debugger.SetBreakPoint(Src, 'Prg');
    AssertDebuggerNotInErrorState;

    (* ************ Nested Functions ************* *)

    dbg.Run;
    Debugger.WaitForFinishRun();
    AssertDebuggerState(dsPause);
    t.Clear;

    AddWatches(t, 'glob const', 'gc', 000, 'A');
    AddWatches(t, 'glob var',   'gv', 001, 'B');

    //t.Add( 'gcInt64_3',    weInteger(-9123372036854775801+0, 'Int64',    8));
    //t.Add( 'gcString1',    weShortStr(IncStr('A',0)));

    //n := 001;
    //t.Add('gvByte',       weCardinal(1+n,                    'Byte',     1));
    //t.Add('gvWord',       weCardinal(100+n,                  'Word',     2));
    //t.Add('gvLongword',   weCardinal(1000+n,                 'Longword', 4));
    //t.Add('gvQWord',      weCardinal(10000+n,                'QWord',    5));
    //t.Add('gvShortint',   weInteger (50+n,                   'Shortint', 1));
    //t.Add('gvSmallint',   weInteger (500+n,                  'Smallint', 2));
    //t.Add('gvLongint',    weInteger (5000+n,                 'Longint',  4));
    //t.Add('gvInt64',      weInteger (50000+n,                'Int64',    8));
    //
    //t.Add('gvByte_2',     weCardinal(250+n,                  'Byte',     1));
    //t.Add('gvWord_2',     weCardinal(65501+n,                'Word',     2));
    //t.Add('gvLongword_2', weCardinal(4123456789+n,           'Longword', 4));
    //t.Add('gvQWord_2',    weCardinal(15446744073709551610+n, 'QWord',    8));
    //t.Add('gvShortint_2', weInteger (122+n,                  'Shortint', 1));
    //t.Add('gvSmallint_2', weInteger (32012+n,                'Smallint', 2));
    //t.Add('gvLongint_2',  weInteger (20123456+n,             'Longint',  4));
    //t.Add('gvInt64_2',    weInteger (9123372036854775801+n,  'Int64',    8));
    //
    //t.Add('gvShortint_3', weInteger(-122+n,                 'Shortint', 1));
    //t.Add('gvSmallint_3', weInteger(-32012+n,               'Smallint', 2));
    //t.Add('gvLongint_3',  weInteger(-20123456+n,            'Longint',  4));
    //t.Add('gvInt64_3',    weInteger(-9123372036854775801+n, 'Int64',    8));
    //
    //t.Add('gvReal',       weFloat(50.25+n,                 'Real'       ));
    //t.Add('gvSingle',     weSingle(100.125+n,              'Single'     ));
    //t.Add('gvDouble',     weDouble(1000.125+n,             'Double'     ));
    //t.Add('gvExtended',   weFloat(10000.175+n,             'Extended'   ));
    ////t.Add('gvComp',       weInteger(150.125+n,              'Comp'       ));
    //t.Add('gvCurrency',   weFloat(125.123+n,               'Currency'   ));
    ////
    //t.Add('gvReal_2',     weFloat(-50.25+n,                'Real'       ));
    //t.Add('gvSingle_2',   weSingle(-100.125+n,             'Single'     ));
    //t.Add('gvDouble_2',   weDouble(-1000.125+n,            'Double'     ));
    //t.Add('gvExtended_2', weFloat(-10000.175+n,            'Extended'   ));
    ////t.Add('gvComp_2',     weFloat(-150.125+n,             'Comp'       ));
    //t.Add('gvCurrency_2', weFloat(-125.123+n,              'Currency'   ));


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

