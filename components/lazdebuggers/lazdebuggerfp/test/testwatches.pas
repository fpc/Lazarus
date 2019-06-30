unit TestWatches;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, TestBase, TestDbgControl,
  TestDbgTestSuites, TestOutputLogger, TTestWatchUtilities, TestCommonSources,
  TestDbgConfig, DbgIntfDebuggerBase, DbgIntfBaseTypes, Forms;

type

  { TTestWatches }

  TTestWatches = class(TDBGTestCase)
  private
    procedure RunToPause(var ABrk: TDBGBreakPoint);
  published
    procedure TestWatchesScope;
    procedure TestWatchesValue;
    procedure TestWatchesAddressOf;
    procedure TestWatchesTypeCast;
  end;

implementation

var
  ControlTestWatch, ControlTestWatchScope, ControlTestWatchValue,
  ControlTestWatchAddressOf, ControlTestWatchTypeCast: Pointer;

procedure TTestWatches.RunToPause(var ABrk: TDBGBreakPoint);
begin
  Debugger.RunToNextPause(dcRun);
  AssertDebuggerState(dsPause);
  ABrk.Enabled := False;
end;

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
  t: TWatchExpectationList;
  Src: TCommonSource;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestWatchScope) then exit;
  t := nil;

  Src := GetCommonSourceFor('WatchesScopePrg.Pas');
  TestCompile(Src, ExeName);

  AssertTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));

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

    Debugger.RunToNextPause(dcRun);
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForFoo(t, 'Scope in FuncFooNestedTwice', 0);
    t.EvaluateWatches;
    t.CheckResults;

    Debugger.RunToNextPause(dcRun);
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForFoo(t, 'Scope in FuncFooNestedTwice2', 0, True);
    t.EvaluateWatches;
    t.CheckResults;

    Debugger.RunToNextPause(dcRun);
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForFoo(t, 'Scope in FuncFooNested', 1);
    t.EvaluateWatches;
    t.CheckResults;

    Debugger.RunToNextPause(dcRun);
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForFoo(t, 'Scope in FuncFoo', 2);
    t.EvaluateWatches;
    t.CheckResults;

    (* ************ Class ************* *)

    Debugger.RunToNextPause(dcRun); // MethodMainChildNestedTwice
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForClassMethods(t, 'Scope in MethodMainChildNestedTwice', 0);
    t.EvaluateWatches;
    t.CheckResults;

    Debugger.RunToNextPause(dcRun); // MethodMainChildNested
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForClassMethods(t, 'Scope in MethodMainChildNested', 1);
    t.EvaluateWatches;
    t.CheckResults;

    Debugger.RunToNextPause(dcRun); // MethodMainChild
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForClassMethods(t, 'Scope in MethodMainChild', 2);
    t.EvaluateWatches;
    t.CheckResults;

    Debugger.RunToNextPause(dcRun); // MethodMain
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForClassMethods(t, 'Scope in MethodMain', 3);
    t.EvaluateWatches;
    t.CheckResults;

    Debugger.RunToNextPause(dcRun); // MethodMainBase
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForClassMethods(t, 'Scope in MethodMainBase', 4);
    t.EvaluateWatches;
    t.CheckResults;

    Debugger.RunToNextPause(dcRun); // MethodMainBaseBase
    AssertDebuggerState(dsPause);
    t.Clear;
    AddWatchesForClassMethods(t, 'Scope in MethodMainBaseBase', 5);
    t.EvaluateWatches;
    t.CheckResults;

    (* ************ Program level ************* *)

    Debugger.RunToNextPause(dcRun);
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
    TTestLoc = (tlAny, tlConst, tlParam, tlArrayWrap, tlPointer);

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

// tlConst => strings are stored as shortstring
    t.Add(AName, p+'String1'+e,    weShortStr(AChr1, 'ShortStr1'))                      .IgnTypeName([], ALoc = tlConst);
    t.Add(AName, p+'String1e'+e,   weShortStr('',    'ShortStr1'))                      .IgnTypeName([], ALoc = tlConst);
    t.Add(AName, p+'String10'+e,   weShortStr(AChr1+'bc1',               'ShortStr10')) .IgnTypeName([], ALoc = tlConst);
    t.Add(AName, p+'String10e'+e,  weShortStr('',                        'ShortStr10')) .IgnTypeName([], ALoc = tlConst);
    t.Add(AName, p+'String10x'+e,  weShortStr(AChr1+'S'#0'B'#9'b'#10#13, 'ShortStr10')) .IgnTypeName([], ALoc = tlConst);
    t.Add(AName, p+'String255'+e,  weShortStr(AChr1+'bcd0123456789', 'ShortStr255'));

    t.Add(AName, p+'Ansi1'+e,      weAnsiStr(Succ(AChr1)))     .IgnKindPtr(stDwarf2).IgnKind(stDwarf3Up)
      .IgnTypeName([], ALoc = tlConst).IgnKind([], ALoc = tlConst);
    t.Add(AName, p+'Ansi2'+e,      weAnsiStr(AChr1+'abcd0123')).IgnKindPtr(stDwarf2).IgnKind(stDwarf3Up)
      .IgnTypeName([], ALoc = tlConst).IgnKind([], ALoc = tlConst);
    t.Add(AName, p+'Ansi3'+e,      weAnsiStr(''))              .IgnKindPtr(stDwarf2).IgnKind(stDwarf3Up)
      .IgnTypeName([], ALoc = tlConst).IgnKind([], ALoc = tlConst);
    t.Add(AName, p+'Ansi4'+e,      weAnsiStr(AChr1+'A'#0'B'#9'b'#10#13))  // cut off at #0 in dwarf2 / except tlConst, because it is a shortstring (kind of works by accident)
             .IgnKindPtr(stDwarf2).IgnData(stDwarf2, ALoc <> tlConst).IgnKind(stDwarf3Up)
      .IgnTypeName([], ALoc = tlConst).IgnKind([], ALoc = tlConst);
    t.Add(AName, p+'Ansi5'+e,      weAnsiStr(AChr1+'bcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghij'
      ) )    .IgnKindPtr(stDwarf2)                  .IgnKind(stDwarf3Up)
      .IgnTypeName([], ALoc = tlConst).IgnKind([], ALoc = tlConst);

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

if not(ALoc in [tlConst]) then begin
    t.Add(AName, p+'WideString1'+e,    weWideStr(Succ(AChr1)))              .IgnKindPtr;
    t.Add(AName, p+'WideString2'+e,    weWideStr(AChr1+'abcX0123'))         .IgnKindPtr;
    t.Add(AName, p+'WideString3'+e,    weWideStr(''))                       .IgnKindPtr;
    t.Add(AName, p+'WideString4'+e,    weWideStr(AChr1+'A'#0'X'#9'b'#10#13)).IgnKindPtr
      .IgnData(stDwarf2).IgnData([], Compiler.Version < 030100); // cut off at #0
    t.Add(AName, p+'WideString5'+e,    weWideStr(AChr1+'XcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghij',
      'TWStrTA'))                                                         .IgnKindPtr;

    t.Add(AName, p+'WideString2'+e+'[1]',  weWideChar(AChr1))     .CharFromIndex.IgnTypeName(stDwarf3Up);
    t.Add(AName, p+'WideString2'+e+'[2]',  weWideChar('a'))       .CharFromIndex.IgnTypeName(stDwarf3Up);
    t.Add(AName, p+'WideString5'+e+'[1]',  weWideChar(AChr1))     .CharFromIndex.IgnTypeName(stDwarf3Up);
    t.Add(AName, p+'WideString5'+e+'[2]',  weWideChar('X'))       .CharFromIndex.IgnTypeName(stDwarf3Up);
end;

//TODO wePWidechar
    t.Add(AName, p+'PWideChar'+e,      wePointer(weWideStr(''), 'PWideChar'));
    t.Add(AName, p+'PWideChar2'+e,     wePointer(weWideStr(AChr1+'abcX0123'), 'TPWChr')).SkipIf(ALoc = tlConst);

if not(ALoc in [tlConst]) then begin
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
end;


    // TODO
    t.Add(AName, p+'ShortRec'+e,     weMatch(''''+AChr1+''', *''b'', *'''+AChr1+'''', skRecord));


    if not(ALoc in [tlPointer]) then begin
    t.add(AName, p+'CharDynArray'+e,  weDynArray([]                                        ));
    t.add(AName, p+'CharDynArray2'+e, weDynArray(weChar(['N', AChr1, 'M'])                 )).SkipIf(ALoc = tlConst);
    t.AddIndexFromPrevious(['0','1','2'], [0,1,2]);
    end;
    t.add(AName, p+'CharDynArray3'+e, weDynArray([],                        'TCharDynArray'));
    t.Add(AName, p+'CharDynArray4'+e, weDynArray(weChar(['J', AChr1, 'M']), 'TCharDynArray')).SkipIf(ALoc = tlConst);
    t.AddIndexFromPrevious(['0','1','2'], [0,1,2]);

    if not(ALoc in [tlPointer]) then begin
    t.Add(AName, p+'WCharDynArray'+e, weDynArray([]                        ));
    t.Add(AName, p+'WCharDynArray2'+e,weDynArray(weWideChar(['W', AChr1, 'M']) )).SkipIf(ALoc = tlConst);
    t.AddIndexFromPrevious(['0','1','2'], [0,1,2]);
    end;
    t.Add(AName, p+'WCharDynArray3'+e,weDynArray([]                        ));
    t.Add(AName, p+'WCharDynArray4'+e,weDynArray(weWideChar(['K', AChr1, 'M']) )).SkipIf(ALoc = tlConst);
    t.AddIndexFromPrevious(['0','1','2'], [0,1,2]);

    if not(ALoc in [tlPointer]) then begin
    t.add(AName, p+'IntDynArray'+e,   weDynArray([]                                           ));
    t.add(AName, p+'IntDynArray2'+e,  weDynArray(weInteger([11, 30+AOffs, 60])                )).SkipIf(ALoc = tlConst);
    t.AddIndexFromPrevious(['0','1','2'], [0,1,2]);
    end;
    t.add(AName, p+'IntDynArray3'+e,  weDynArray([],                            'TIntDynArray'));
    t.Add(AName, p+'IntDynArray4'+e,  weDynArray(weInteger([12, 30+AOffs, 60]), 'TIntDynArray')).SkipIf(ALoc = tlConst);
    t.AddIndexFromPrevious(['0','1','2'], [0,1,2]);

    t.add(AName, p+'IntDynArray5'+e,  weDynArray([],                            'TIntDynArray'));

    if not(ALoc in [tlPointer]) then begin
    t.add(AName, p+'AnsiDynArray'+e,  weDynArray([]                                                     ));
    t.add(AName, p+'AnsiDynArray2'+e, weDynArray(weAnsiStr(['N123', AChr1+'ab', 'M'#9])                 )).SkipIf(ALoc = tlConst);
// TODO: currently gets skPointer instead of skAnsiString (dwarf 2)
//    t.AddIndexFromPrevious(['0','1','2'], [0,1,2]);
    end;
    t.add(AName, p+'AnsiDynArray3'+e, weDynArray([],                                     'TAnsiDynArray'));
    t.Add(AName, p+'AnsiDynArray4'+e, weDynArray(weAnsiStr(['J123', AChr1+'ab', 'M'#9]), 'TAnsiDynArray')).SkipIf(ALoc = tlConst);
// TODO: currently gets skPointer instead of skAnsiString (dwarf 2)
//    t.AddIndexFromPrevious(['0','1','2'], [0,1,2]);

    if not(ALoc in [tlPointer]) then begin
    t.add(AName, p+'ShortStrDynArray'+e,  weDynArray([]                                                          ));
    t.add(AName, p+'ShortStrDynArray2'+e, weDynArray(weShortStr(['N123', AChr1+'ac', 'M'#9], 'ShortStr10')                     ))
      .SkipIf(ALoc = tlConst);
    t.AddIndexFromPrevious(['0','1','2'], [0,1,2]);
    end;
    t.add(AName, p+'ShortStrDynArray3'+e, weDynArray([],                                      'TShortStrDynArray'));
    t.Add(AName, p+'ShortStrDynArray4'+e, weDynArray(weShortStr(['J123', AChr1+'ac', 'M'#9], 'ShortStr10'), 'TShortStrDynArray'))
      .SkipIf(ALoc = tlConst);
    t.AddIndexFromPrevious(['0','1','2'], [0,1,2]);


if not(ALoc in [tlConst]) then begin
    t.Add(AName, p+'DynDynArrayInt'+e, weDynArray([
        weDynArray(weInteger([11+AOffs,0,-22])),
        weDynArray(weInteger([110+AOffs])),
        weDynArray(weInteger([11+AOffs,0,-22])),
        weDynArray(weInteger([])),
        weDynArray(weInteger([11,12,11,10]))
      ], 'TDynDynArrayInt'));

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


/////    t.Add(AName, p+'pre__FiveDynArray'+e,  weStatArray(weChar([AChr1, 'b', AChr1, 'B', 'c'])                  ))
t.Add(AName, p+'FiveDynArray'+e,            weMatch('.*',skArray));
t.Add(AName, p+'FiveDynArray'+e+'[0]',      weMatch('.*',skRecord));
//    t.Add(AName, p+'FiveDynArray'+e,            we());
//    t.Add(AName, p+'FiveDynArrayPack'+e,        we());
//    t.Add(AName, p+'FivePackDynArray'+e,        we());
//    t.Add(AName, p+'FivePackDynArrayPack'+e,    we());
//    t.Add(AName, p+'RecFiveDynArray'+e,         we());
//    t.Add(AName, p+'RecFiveDynPackArray'+e,     we());
//    t.Add(AName, p+'RecFivePackDynArray'+e,     we());
//    t.Add(AName, p+'RecFivePackDynPackArray'+e, we());
//    t.Add(AName, p+'FiveDynArray2'+e,           we());
//    t.Add(AName, p+'FiveDynArrayPack2'+e,       we());
//    t.Add(AName, p+'FivePackDynArray2'+e,       we());
//    t.Add(AName, p+'FivePackDynArrayPack2'+e,   we());

end;



    t.Add(AName, p+'CharStatArray'+e,  weStatArray(weChar([AChr1, 'b', AChr1, 'B', 'c'])                  ))
      .SkipIf(ALoc = tlParam).SkipIf(ALoc = tlPointer);
    t.Add(AName, p+'CharStatArray2'+e, weStatArray(weChar([AChr1, 'c', AChr1, 'B', 'c']), 'TCharStatArray'));

    t.Add(AName, p+'WCharStatArray'+e, weStatArray(weChar([AChr1, 'b', AChr1, 'B', 'd'])                   ))
      .SkipIf(ALoc = tlParam).SkipIf(ALoc = tlPointer);
    t.Add(AName, p+'WCharStatArray2'+e,weStatArray(weChar([AChr1, 'c', AChr1, 'B', 'd']), 'TwCharStatArray'));

    t.Add(AName, p+'IntStatArray'+e,  weStatArray(weInteger([-1, 300+AOffs, 2, 0, 1])                 ))
      .SkipIf(ALoc = tlParam).SkipIf(ALoc = tlPointer);
    t.Add(AName, p+'IntStatArray2'+e, weStatArray(weInteger([-2, 200+AOffs, 2, 0, 1]), 'TIntStatArray'));

    t.Add(AName, p+'AnsiStatArray'+e,  weStatArray(weAnsiStr([AChr1, 'b123', AChr1+'ab', 'B', 'cdef'#9])                  ))
      .SkipIf(ALoc = tlParam).SkipIf(ALoc = tlPointer);
    t.Add(AName, p+'AnsiStatArray2'+e, weStatArray(weAnsiStr([AChr1, 'c123', AChr1+'ad', 'D', 'cxx'#9] ), 'TAnsiStatArray'));

    t.Add(AName, p+'ShortStrStatArray'+e,  weStatArray(weShortStr([AChr1, 'b123', AChr1+'ab', 'C', 'cdef'#9])                  ))
      .SkipIf(ALoc = tlParam).SkipIf(ALoc = tlPointer);
    t.Add(AName, p+'ShortStrStatArray2'+e, weStatArray(weShortStr([AChr1, 'c123', AChr1+'ad', 'C', 'cxx'#9] ), 'TShortStrStatArray'));

//    t.Add(AName, p+'FiveStatArray{e}             _O2_ TFiveStatArray            _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FiveStatArray;
//    t.Add(AName, p+'FiveStatArrayPack{e}         _O2_ TFiveStatArrayPack        _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FiveStatArrayPack;
//    t.Add(AName, p+'FivePackStatArray{e}         _O2_ TFivePackStatArray        _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FivePackStatArray;
//    t.Add(AName, p+'FivePackStatArrayPack{e}     _O2_ TFivePackStatArrayPack    _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FivePackStatArrayPack;
//    t.Add(AName, p+'RecFiveStatArray{e}          _O2_ TRecFiveStatArray         _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_RecFiveStatArray;
//    t.Add(AName, p+'RecFiveStatPackArray{e}      _O2_ TRecFiveStatPackArray     _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_RecFiveStatPackArray;
//    t.Add(AName, p+'RecFivePackStatArray{e}      _O2_ TRecFivePackStatArray     _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_RecFivePackStatArray;
//    t.Add(AName, p+'RecFivePackStatPackArray{e}  _O2_ TRecFivePackStatPackArray _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_RecFivePackStatPackArray;


//TODO: element by index


    t.Add(AName, p+'ArrayEnum1'+e, weStatArray(weInteger([500+n,701,702,703])  ))
      .SkipIf(ALoc = tlParam).SkipIf(ALoc = tlPointer);
    t.AddIndexFromPrevious(['EnVal1','EnVal2','EnVal3','EnVal4',
     'gvEnum', 'gvEnumA', 'gvEnum1',  'gcEnum', 'gcEnumA', 'gcEnum1',   p+'Enum'+e, p+'EnumA'+e, p+'Enum1'+e],
     [0,1,2,3,  2,0,1,  2,0,1,  2,0,1]);
    t.Add(AName, p+'ArrayEnum3'+e, weStatArray(weInteger([200+n,701,702,703]), 'TArrayEnum'));
    t.AddIndexFromPrevious(['EnVal1','EnVal2','EnVal3','EnVal4',
     'gvEnum', 'gvEnumA', 'gvEnum1',  'gcEnum', 'gcEnumA', 'gcEnum1',   p+'Enum'+e, p+'EnumA'+e, p+'Enum1'+e],
     [0,1,2,3,  2,0,1,  2,0,1,  2,0,1]);

    t.Add(AName, p+'ArrayEnumSub1'+e, weStatArray(weInteger([600+n,801])  ))
      .SkipIf(ALoc = tlParam).SkipIf(ALoc = tlPointer);
    t.AddIndexFromPrevious(['EnVal1','EnVal2',
     'gvEnumA', 'gvEnum1',  'gcEnumA', 'gcEnum1',   p+'EnumA'+e, p+'Enum1'+e],
     [0,1,  0,1,  0,1,  0,1]);
    t.Add(AName, p+'ArrayEnumSub3'+e, weStatArray(weInteger([100+n,801]), 'TArrayEnumSub'));
    t.AddIndexFromPrevious(['EnVal1','EnVal2',
     'gvEnumA', 'gvEnum1',  'gcEnumA', 'gcEnum1',   p+'EnumA'+e, p+'Enum1'+e],
     [0,1,  0,1,  0,1,  0,1]);

    t.Add(AName, p+'ArrayEnum2'+e, weStatArray(weInteger([300+n,701,702,703])  ))
      .SkipIf(ALoc = tlParam).SkipIf(ALoc = tlPointer);
    t.AddIndexFromPrevious(['EnVal1','EnVal2','EnVal3','EnVal4',
     'gvEnum', 'gvEnumA', 'gvEnum1',  'gcEnum', 'gcEnumA', 'gcEnum1',   p+'Enum'+e, p+'EnumA'+e, p+'Enum1'+e],
     [0,1,2,3,  2,0,1,  2,0,1,  2,0,1]);
    t.Add(AName, p+'ArrayEnum4'+e, weStatArray(weInteger([800+n,701,702,703]), 'TArrayEnumElem'));
    t.AddIndexFromPrevious(['EnVal1','EnVal2','EnVal3','EnVal4',
     'gvEnum', 'gvEnumA', 'gvEnum1',  'gcEnum', 'gcEnumA', 'gcEnum1',   p+'Enum'+e, p+'EnumA'+e, p+'Enum1'+e],
     [0,1,2,3,  2,0,1,  2,0,1,  2,0,1]);

    t.Add(AName, p+'ArrayEnumSub2'+e, weStatArray(weInteger([400+n,801])  ))
      .SkipIf(ALoc = tlParam).SkipIf(ALoc = tlPointer);
    t.AddIndexFromPrevious(['EnVal1','EnVal2',
     'gvEnumA', 'gvEnum1',  'gcEnumA', 'gcEnum1',   p+'EnumA'+e, p+'Enum1'+e],
     [0,1,  0,1,  0,1,  0,1]);
    t.Add(AName, p+'ArrayEnumSub4'+e, weStatArray(weInteger([700+n,801]), 'TArrayEnumSubElem'));
    t.AddIndexFromPrevious(['EnVal1','EnVal2',
     'gvEnumA', 'gvEnum1',  'gcEnumA', 'gcEnum1',   p+'EnumA'+e, p+'Enum1'+e],
     [0,1,  0,1,  0,1,  0,1]);




  t.Add(AName, p+'Enum'+e, weEnum('EnVal3', 'TEnum'));
  t.Add(AName, p+'Enum1'+e, weEnum('EnVal2', 'TEnumSub'));
  t.Add(AName, p+'Enum2'+e, weEnum('EnVal21', 'TEnum2'));
  t.Add(AName, p+'Enum3'+e, weEnum('EnVal25', 'TEnum2'));

  t.Add(AName, p+'Set'+e, weSet(['EnVal2', 'EnVal4'], 'TSet')).Skip([stDwarf]);

  t.Add(AName, p+'IntfUnknown'+e, weMatch('.?', skInterface)).Skip(); // only run eval / do not crash

  end;

var
  ExeName: String;
  t: TWatchExpectationList;
  Src: TCommonSource;
  BrkPrg, BrkFooBegin, BrkFoo, BrkFooVar, BrkFooVarBegin,
    BrkFooConstRef: TDBGBreakPoint;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestWatchValue) then exit;
  t := nil;

  Src := GetCommonSourceFor('WatchesValuePrg.Pas');
  TestCompile(Src, ExeName);

  AssertTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));

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

    RunToPause(BrkPrg);
    t.Clear;

//t.Add( 'gvaString10[1]',   weShortStr('Lbc1',               'ShortStr10'));
//t.Add( 'gvaShortRec[1]',       weMatch('''L'', *''b'', *''L''', skRecord));
//t.Add( 'gvAnsiDynArray2',   weShortStr('Lbc1',               'ShortStr10'));
//t.Add( 'gvaDynDynArrayInt[0]',   weShortStr('Lbc1',               'ShortStr10'));
//t.Add( 'argCharDynArray',   weShortStr('Lbc1',               'ShortStr10'));
//t.Add( 'gvUnicodeString2',    weWideStr('BaBcX0123'))         ;
//t.Add('gvUnicodeString2[1]',    weWideChar('B')) ;
//t.Add( 'gcString10',   weShortStr('Bbc1',               'ShortStr10'));
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
    AddWatches(t, 'glob var pointer',            'gvp_', 001, 'B', tlPointer, '^'); // pointer
    t.EvaluateWatches;
    t.CheckResults;


    RunToPause(BrkFooBegin);
    t.Clear;
    AddWatches(t, 'fooBegin local', 'fooloc', 002, 'C');
    AddWatches(t, 'fooBegin args', 'arg', 001, 'B', tlParam);
    t.EvaluateWatches;
    // Do not check values. // Just ensure no crash occurs
    // Registers are wrong in prologue.


    //cl := Debugger.LazDebugger.GetLocation.SrcLine;
    RunToPause(BrkFoo);
    //// below might have been caused by the break on FooVarBegin, if there was no code.
    //if (cl > 1) and (cl = Debugger.LazDebugger.GetLocation.SrcLine) then begin dbg.Run; Debugger.WaitForFinishRun(); end; // TODO: bug, stopping twice the same breakpoint
    t.Clear;
    AddWatches(t, 'foo local', 'fooloc', 002, 'C');
    AddWatches(t, 'foo args', 'arg', 001, 'B', tlParam);
    AddWatches(t, 'foo ArgMyClass1',     'ArgMyClass1.mc',  002, 'C');
    AddWatches(t, 'foo ArgMyBaseClass1', 'ArgMyClass1.mbc', 003, 'D');
    AddWatches(t, 'foo ArgMyClass1',     'TMyClass(ArgMyClass2).mc',  004, 'E');
    AddWatches(t, 'foo ArgMyBaseClass1', 'TMyClass(ArgMyClass2).mbc', 005, 'F');
    t.EvaluateWatches;
    t.CheckResults;


    RunToPause(BrkFooVarBegin);
    t.Clear;
    AddWatches(t, 'foo var args', 'argvar', 001, 'B', tlParam);
    t.EvaluateWatches;
    // Do not check values. // Just ensure no crash occurs
    // Registers are wrong in prologue.


    RunToPause(BrkFooVar);
    t.Clear;
    AddWatches(t, 'foo var args', 'argvar', 001, 'B', tlParam);
    AddWatches(t, 'foo var ArgMyClass1',     'ArgVarMyClass1.mc',  002, 'C');
    AddWatches(t, 'foo var ArgMyBaseClass1', 'ArgVarMyClass1.mbc', 003, 'D');
    AddWatches(t, 'foo var ArgMyClass1',     'TMyClass(ArgVarMyClass2).mc',  004, 'E');
    AddWatches(t, 'foo var ArgMyBaseClass1', 'TMyClass(ArgVarMyClass2).mbc', 005, 'F');
    t.EvaluateWatches;
    t.CheckResults;


    RunToPause(BrkFooConstRef);
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

procedure TTestWatches.TestWatchesAddressOf;

  type
    TTestLoc = (tlAny, tlConst, tlParam, tlArrayWrap, tlPointer);

  procedure AddWatches(t: TWatchExpectationList; AName: String; APrefix: String; ALoc: TTestLoc = tlAny; APostFix: String = '');
  var
    p, e: String;
  begin
    p := APrefix;
    e := APostFix;

    t.AddWithoutExpect(AName, p+'Byte'+e);
    t.AddWithoutExpect(AName, p+'Word'+e);
    t.AddWithoutExpect(AName, p+'Longword'+e);
    t.AddWithoutExpect(AName, p+'QWord'+e);
    t.AddWithoutExpect(AName, p+'Shortint'+e);
    t.AddWithoutExpect(AName, p+'Smallint'+e);
    t.AddWithoutExpect(AName, p+'Longint'+e);
    t.AddWithoutExpect(AName, p+'Int64'+e);
    t.AddWithoutExpect(AName, p+'IntRange'+e);
    t.AddWithoutExpect(AName, p+'CardinalRange'+e);

    t.AddWithoutExpect(AName, p+'Byte_2'+e);
    t.AddWithoutExpect(AName, p+'Word_2'+e);
    t.AddWithoutExpect(AName, p+'Longword_2'+e);
    t.AddWithoutExpect(AName, p+'QWord_2'+e);
    t.AddWithoutExpect(AName, p+'Shortint_2'+e);
    t.AddWithoutExpect(AName, p+'Smallint_2'+e);
    t.AddWithoutExpect(AName, p+'Longint_2'+e);
    t.AddWithoutExpect(AName, p+'Int64_2'+e);

    t.AddWithoutExpect(AName, p+'Shortint_3'+e);
    t.AddWithoutExpect(AName, p+'Smallint_3'+e);
    t.AddWithoutExpect(AName, p+'Longint_3'+e);
    t.AddWithoutExpect(AName, p+'Int64_3'+e);

    t.AddWithoutExpect(AName, p+'Real'+e);
    t.AddWithoutExpect(AName, p+'Single'+e);
    t.AddWithoutExpect(AName, p+'Double'+e);
    t.AddWithoutExpect(AName, p+'Extended'+e);
    //t.AddWithoutExpect(p+'Comp'+e);
    t.AddWithoutExpect(AName, p+'Currency'+e);

    t.AddWithoutExpect(AName, p+'Real_2'+e);
    t.AddWithoutExpect(AName, p+'Single_2'+e);
    t.AddWithoutExpect(AName, p+'Double_2'+e);
    t.AddWithoutExpect(AName, p+'Extended_2'+e); // Double ?
    //t.AddWithoutExpect(p+'Comp_2'+e);
    t.AddWithoutExpect(AName, p+'Currency_2'+e);

    t.AddWithoutExpect(AName, p+'Char'+e);
    t.AddWithoutExpect(AName, p+'Char2'+e);
    t.AddWithoutExpect(AName, p+'Char3'+e);

    t.AddWithoutExpect(AName, p+'String1'+e);
    t.AddWithoutExpect(AName, p+'String1e'+e);
    t.AddWithoutExpect(AName, p+'String10'+e);
    t.AddWithoutExpect(AName, p+'String10e'+e);
    t.AddWithoutExpect(AName, p+'String10x'+e);
    t.AddWithoutExpect(AName, p+'String255'+e);

    t.AddWithoutExpect(AName, p+'Ansi1'+e);
    t.AddWithoutExpect(AName, p+'Ansi2'+e);
    t.AddWithoutExpect(AName, p+'Ansi3'+e);
    t.AddWithoutExpect(AName, p+'Ansi4'+e);
    t.AddWithoutExpect(AName, p+'Ansi5'+e);

//TODO wePchar
    t.AddWithoutExpect(AName, p+'PChar'+e);
    t.AddWithoutExpect(AName, p+'PChar2'+e);

    // char by index
    // TODO: no typename => calculated value ?
////    t.AddWithoutExpect(AName, p+'String10'+e+'[2]').CharFromIndex;
////    t.AddWithoutExpect(AName, p+'Ansi2'+e+'[2]').CharFromIndex;
////    t.AddWithoutExpect(AName, p+'PChar2'+e+'[1]').CharFromIndex;
////    t.AddWithoutExpect(AName, p+'String10'+e+'[1]').CharFromIndex;
////    t.AddWithoutExpect(AName, p+'Ansi2'+e+'[1]').CharFromIndex;
////    t.AddWithoutExpect(AName, p+'PChar2'+e+'[0]').CharFromIndex;


    t.AddWithoutExpect(AName, p+'WideChar'+e); // TODO: widechar
    t.AddWithoutExpect(AName, p+'WideChar2'+e);
    t.AddWithoutExpect(AName, p+'WideChar3'+e);

    t.AddWithoutExpect(AName, p+'WideString1'+e);
    t.AddWithoutExpect(AName, p+'WideString2'+e);
    t.AddWithoutExpect(AName, p+'WideString3'+e);
    t.AddWithoutExpect(AName, p+'WideString4'+e);
    t.AddWithoutExpect(AName, p+'WideString5'+e);

////    t.AddWithoutExpect(AName, p+'WideString2'+e+'[1]')     .CharFromIndex;
////    t.AddWithoutExpect(AName, p+'WideString2'+e+'[2]')     .CharFromIndex;
////    t.AddWithoutExpect(AName, p+'WideString5'+e+'[1]')     .CharFromIndex;
////    t.AddWithoutExpect(AName, p+'WideString5'+e+'[2]')     .CharFromIndex;

//TODO wePWidechar
    t.AddWithoutExpect(AName, p+'PWideChar'+e);
    t.AddWithoutExpect(AName, p+'PWideChar2'+e);

    t.AddWithoutExpect(AName, p+'UnicodeString1'+e);
    t.AddWithoutExpect(AName, p+'UnicodeString2'+e);
    t.AddWithoutExpect(AName, p+'UnicodeString3'+e);
    t.AddWithoutExpect(AName, p+'UnicodeString4'+e);
    t.AddWithoutExpect(AName, p+'UnicodeString5'+e);

//todo dwarf 3
////    t.AddWithoutExpect(AName, p+'UnicodeString2'+e+'[1]')       .CharFromIndex(stDwarf2);
////    t.AddWithoutExpect(AName, p+'UnicodeString2'+e+'[2]')       .CharFromIndex(stDwarf2);
////    t.AddWithoutExpect(AName, p+'UnicodeString5'+e+'[1]')       .CharFromIndex(stDwarf2);
////    t.AddWithoutExpect(AName, p+'UnicodeString5'+e+'[2]')       .CharFromIndex(stDwarf2);


    // TODO
    if not(ALoc in [tlConst]) then begin
    t.AddWithoutExpect(AName, p+'ShortRec'+e);


    t.AddWithoutExpect(AName, p+'CharDynArray3'+e);
    t.AddWithoutExpect(AName, p+'CharDynArray4'+e);

    t.AddWithoutExpect(AName, p+'WCharDynArray3'+e);
    t.AddWithoutExpect(AName, p+'WCharDynArray4'+e);

    t.AddWithoutExpect(AName, p+'IntDynArray3'+e);
    t.AddWithoutExpect(AName, p+'IntDynArray4'+e);

    t.AddWithoutExpect(AName, p+'AnsiDynArray3'+e);
    t.AddWithoutExpect(AName, p+'AnsiDynArray4'+e);

    t.AddWithoutExpect(AName, p+'ShortStrDynArray3'+e);
    t.AddWithoutExpect(AName, p+'ShortStrDynArray4'+e);


    t.AddWithoutExpect(AName, p+'DynDynArrayInt'+e);

////    t.AddWithoutExpect(AName, p+'DynDynArrayInt'+e+'[0]');
////    t.AddWithoutExpect(AName, p+'DynDynArrayInt'+e+'[1]');
////    t.AddWithoutExpect(AName, p+'DynDynArrayInt'+e+'[2]');
////    t.AddWithoutExpect(AName, p+'DynDynArrayInt'+e+'[3]');
////    t.AddWithoutExpect(AName, p+'DynDynArrayInt'+e+'[4]');
////
////    t.AddWithoutExpect(AName, p+'DynDynArrayInt2'+e+'[0]');
////    t.AddWithoutExpect(AName, p+'DynDynArrayInt2'+e+'[1]');
////    t.AddWithoutExpect(AName, p+'DynDynArrayInt2'+e+'[2]');
////    t.AddWithoutExpect(AName, p+'DynDynArrayInt2'+e+'[3]');
////    t.AddWithoutExpect(AName, p+'DynDynArrayInt2'+e+'[4]');


/////    t.AddWithoutExpect(AName, p+'pre__FiveDynArray'+e                  ))
t.AddWithoutExpect(AName, p+'FiveDynArray'+e);
////t.AddWithoutExpect(AName, p+'FiveDynArray'+e+'[0]');
//    t.AddWithoutExpect(AName, p+'FiveDynArray'+e);
//    t.AddWithoutExpect(AName, p+'FiveDynArrayPack'+e);
//    t.AddWithoutExpect(AName, p+'FivePackDynArray'+e);
//    t.AddWithoutExpect(AName, p+'FivePackDynArrayPack'+e);
//    t.AddWithoutExpect(AName, p+'RecFiveDynArray'+e);
//    t.AddWithoutExpect(AName, p+'RecFiveDynPackArray'+e);
//    t.AddWithoutExpect(AName, p+'RecFivePackDynArray'+e);
//    t.AddWithoutExpect(AName, p+'RecFivePackDynPackArray'+e);
//    t.AddWithoutExpect(AName, p+'FiveDynArray2'+e);
//    t.AddWithoutExpect(AName, p+'FiveDynArrayPack2'+e);
//    t.AddWithoutExpect(AName, p+'FivePackDynArray2'+e);
//    t.AddWithoutExpect(AName, p+'FivePackDynArrayPack2'+e);



    t.AddWithoutExpect(AName, p+'CharStatArray2'+e);
    t.AddWithoutExpect(AName, p+'WCharStatArray2'+e);
    t.AddWithoutExpect(AName, p+'IntStatArray2'+e);
    t.AddWithoutExpect(AName, p+'AnsiStatArray2'+e);
    t.AddWithoutExpect(AName, p+'ShortStrStatArray2'+e);
    end;


//    t.AddWithoutExpect(AName, p+'FiveStatArray{e}             _O2_ TFiveStatArray            _EQ_ ((a:-9;b:44), (a:-8-AddWithoutExpect;b:33), (a:-7;b:22));          //@@ _pre3_FiveStatArray;
//    t.AddWithoutExpect(AName, p+'FiveStatArrayPack{e}         _O2_ TFiveStatArrayPack        _EQ_ ((a:-9;b:44), (a:-8-AddWithoutExpect;b:33), (a:-7;b:22));          //@@ _pre3_FiveStatArrayPack;
//    t.AddWithoutExpect(AName, p+'FivePackStatArray{e}         _O2_ TFivePackStatArray        _EQ_ ((a:-9;b:44), (a:-8-AddWithoutExpect;b:33), (a:-7;b:22));          //@@ _pre3_FivePackStatArray;
//    t.AddWithoutExpect(AName, p+'FivePackStatArrayPack{e}     _O2_ TFivePackStatArrayPack    _EQ_ ((a:-9;b:44), (a:-8-AddWithoutExpect;b:33), (a:-7;b:22));          //@@ _pre3_FivePackStatArrayPack;
//    t.AddWithoutExpect(AName, p+'RecFiveStatArray{e}          _O2_ TRecFiveStatArray         _EQ_ ((a:-9;b:44), (a:-8-AddWithoutExpect;b:33), (a:-7;b:22));          //@@ _pre3_RecFiveStatArray;
//    t.AddWithoutExpect(AName, p+'RecFiveStatPackArray{e}      _O2_ TRecFiveStatPackArray     _EQ_ ((a:-9;b:44), (a:-8-AddWithoutExpect;b:33), (a:-7;b:22));          //@@ _pre3_RecFiveStatPackArray;
//    t.AddWithoutExpect(AName, p+'RecFivePackStatArray{e}      _O2_ TRecFivePackStatArray     _EQ_ ((a:-9;b:44), (a:-8-AddWithoutExpect;b:33), (a:-7;b:22));          //@@ _pre3_RecFivePackStatArray;
//    t.AddWithoutExpect(AName, p+'RecFivePackStatPackArray{e}  _O2_ TRecFivePackStatPackArray _EQ_ ((a:-9;b:44), (a:-8-AddWithoutExpect;b:33), (a:-7;b:22));          //@@ _pre3_RecFivePackStatPackArray;


//TODO: element by index


  t.AddWithoutExpect(AName, p+'Enum'+e);
  t.AddWithoutExpect(AName, p+'Enum1'+e);
  t.AddWithoutExpect(AName, p+'Enum2'+e);
  t.AddWithoutExpect(AName, p+'Enum3'+e);

  t.AddWithoutExpect(AName, p+'Set'+e).Skip([stDwarf]);

  t.AddWithoutExpect(AName, p+'IntfUnknown'+e);

  end;

  procedure CmpWatches(t1, t2: TWatchExpectationList);
  var
    i, Thread: Integer;
    v1, v2: String;
  begin
    AssertTrue('Same count', t1.Count = t2.Count);
    t1.EvaluateWatches;
    t2.EvaluateWatches;
    Thread := Debugger.Threads.Threads.CurrentThreadId;
    for i := 0 to t1.Count - 1 do begin
      v1 := t1.Tests[i]^.TstWatch.Values[Thread, 0].Value;
      v2 := t2.Tests[i]^.TstWatch.Values[Thread, 0].Value;

      // check, if v2 has the derefed value at the end
      if (length(v1) < Length(v2)) and (pos(') ', v2) = Length(v1)) then
        v2 := copy(v2, 1, Length(v1));

      TestEquals(t1.Tests[i]^.TstTestName + ': ' + t1.Tests[i]^.TstWatch.Expression + ' <> ' + t2.Tests[i]^.TstWatch.Expression,
        v1, v2);
    end;
  end;

  procedure AssertFailedWatches(t1: TWatchExpectationList);
  var
    i, Thread: Integer;
    v1: TWatchValue;
    s: string;
  begin
    t1.EvaluateWatches;
    Thread := Debugger.Threads.Threads.CurrentThreadId;
    for i := 0 to t1.Count - 1 do begin
      v1 := t1.Tests[i]^.TstWatch.Values[Thread, 0];
      WriteStr(s, v1.Validity);

      TestTrue(t1.Tests[i]^.TstTestName + ': ' + t1.Tests[i]^.TstWatch.Expression + ' >> ' + v1.Value + ' / ' + s,
        v1.Validity in [ddsError{, ddsInvalid}]);
    end;
  end;

var
  ExeName: String;
  t, tp: TWatchExpectationList;
  Src: TCommonSource;
  BrkPrg, BrkFoo, BrkFooVar, BrkFooConstRef: TDBGBreakPoint;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestWatchAddressOf) then exit;
  t := nil;
  tp := nil;

  Src := GetCommonSourceFor('WatchesValuePrg.Pas');
  TestCompile(Src, ExeName);

  AssertTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));

  try
    t := TWatchExpectationList.Create(Self);
    tp := TWatchExpectationList.Create(Self);

    BrkPrg         := Debugger.SetBreakPoint(Src, 'Prg');
    BrkFoo         := Debugger.SetBreakPoint(Src, 'Foo');
    BrkFooVar      := Debugger.SetBreakPoint(Src, 'FooVar');
    BrkFooConstRef := Debugger.SetBreakPoint(Src, 'FooConstRef');
    AssertDebuggerNotInErrorState;

    (* ************ Nested Functions ************* *)

    RunToPause(BrkPrg);

    t.Clear;
    AddWatches(t,  'glob const',         '@gc', tlConst);
    AssertFailedWatches(t);

    t.Clear;
    tp.Clear;
    AddWatches(t,  'glob var',         '@gv');
    AddWatches(tp, 'glob var pointer', 'gvp_'); // pointer
    CmpWatches(t, tp);

// TODO: field / field on nil object


    RunToPause(BrkFoo);
    t.Clear;
    tp.Clear;
    AddWatches(t,  'foo local',         '@fooloc');
    AddWatches(tp, 'foo local pointer', 'fooloc_pl_');
    CmpWatches(t, tp);

    t.Clear;
    tp.Clear;
    AddWatches(t,  'foo local',         '@arg');
    AddWatches(tp, 'foo local pointer', 'fooloc_pa_');
    CmpWatches(t, tp);


    RunToPause(BrkFooVar);
    t.Clear;
    tp.Clear;
    AddWatches(t,  'foo var args',         '@argvar', tlParam);
    AddWatches(tp, 'foo var args pointer', 'fooloc_pv_');
    CmpWatches(t, tp);


    //RunToPause(BrkFooConstRef);
    //t.Clear;
    //AddWatches(t, 'foo const ref args', 'argconstref', tlParam);
    //CmpWatches(t, tp);


  finally
    t.Free;
    tp.Free;
    Debugger.ClearDebuggerMonitors;
    Debugger.FreeDebugger;

    AssertTestErrors;
  end;
end;

procedure TTestWatches.TestWatchesTypeCast;

  type
    TTestLoc = (tlAny, tlConst, tlParam, tlArrayWrap, tlPointer);
  var
    t2: TWatchExpectationList;

  procedure AddWatchesConv(t: TWatchExpectationList; AName: String; APrefix: String; AOffs: Integer; AChr1: Char;
    ALoc: TTestLoc = tlAny; APostFix: String = '');

    function SignedIntAnd(AVal: Int64; AMask: Qword): Int64;
    begin
      Result := AVal and AMask;
      if (Result and (AMask xor (AMask >> 1))) <> 0 then
        Result := Result or (not AMask);
    end;
  const
    UIntConvert: array[0..3] of record TypeName: String; Mask: qword; end = (
      ( TypeName: 'Byte';     Mask: $FF),
      ( TypeName: 'Word';     Mask: $FFFF),
      ( TypeName: 'LongWord'; Mask: $FFFFFFFF),
      ( TypeName: 'QWord';    Mask: qword($FFFFFFFFFFFFFFFF))
      //( TypeName: 'Pointer';  Mask: ),
    );
    SIntConvert: array[0..3] of record TypeName: String; Mask: QWord; end = (
      ( TypeName: 'ShortInt';  Mask: $FF),
      ( TypeName: 'SmallInt';  Mask: $FFFF),
      ( TypeName: 'LongInt';   Mask: $FFFFFFFF),
      ( TypeName: 'Int64';     Mask: qword($FFFFFFFFFFFFFFFF))
    );

  var
    p, e, tn: String;
    i, n: Integer;
    tm: QWord  ;
  begin
    p := APrefix;
    e := APostFix;
    n := AOffs;

    {$PUSH}{$Q-}{$R-}
    for i := low(UIntConvert) to high(UIntConvert) do begin
      tn := UIntConvert[i].TypeName;
      tm := UIntConvert[i].Mask;
      t.Add(AName+' '+tn, tn+'('+p+'Byte'+e+')',          weCardinal(qword((1+n)                    and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Word'+e+')',          weCardinal(qword((100+n)                  and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Longword'+e+')',      weCardinal(qword((1000+n)                 and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'QWord'+e+')',         weCardinal(qword((10000+n)                and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Shortint'+e+')',      weCardinal(qword((50+n)                   and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Smallint'+e+')',      weCardinal(qword((500+n)                  and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Longint'+e+')',       weCardinal(qword((5000+n)                 and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Int64'+e+')',         weCardinal(qword((50000+n)                and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'IntRange'+e+')',      weCardinal(qword((-50+n)                  and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'CardinalRange'+e+')', weCardinal(qword((50+n)                   and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Byte_2'+e+')',        weCardinal(qword((240+n)                  and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Word_2'+e+')',        weCardinal(qword((65501+n)                and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Longword_2'+e+')',    weCardinal(qword((4123456789+n)           and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'QWord_2'+e+')',       weCardinal(qword((15446744073709551610+n) and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Shortint_2'+e+')',    weCardinal(qword((112+n)                  and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Smallint_2'+e+')',    weCardinal(qword((32012+n)                and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Longint_2'+e+')',     weCardinal(qword((20123456+n)             and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Int64_2'+e+')',       weCardinal(qword((9123372036854775801+n)  and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Shortint_3'+e+')',    weCardinal(qword((-112+n)                 and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Smallint_3'+e+')',    weCardinal(qword((-32012+n)               and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Longint_3'+e+')',     weCardinal(qword((-20123456+n)            and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Int64_3'+e+')',       weCardinal(qword((-9123372036854775801+n) and tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Char'+e+')',          weCardinal(ord(AChr1), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Char2'+e+')',         weCardinal(ord(#0),    tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Char3'+e+')',         weCardinal(ord(' '),   tn, -1));
    end;
    for i := low(SIntConvert) to high(SIntConvert) do begin
      tn := SIntConvert[i].TypeName;
      tm := SIntConvert[i].Mask;
      t.Add(AName+' '+tn, tn+'('+p+'Byte'+e+')',          weInteger(SignedIntAnd(1+n                   , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Word'+e+')',          weInteger(SignedIntAnd(100+n                 , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Longword'+e+')',      weInteger(SignedIntAnd(1000+n                , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'QWord'+e+')',         weInteger(SignedIntAnd(10000+n               , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Shortint'+e+')',      weInteger(SignedIntAnd(50+n                  , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Smallint'+e+')',      weInteger(SignedIntAnd(500+n                 , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Longint'+e+')',       weInteger(SignedIntAnd(5000+n                , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Int64'+e+')',         weInteger(SignedIntAnd(50000+n               , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'IntRange'+e+')',      weInteger(SignedIntAnd(-50+n                 , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'CardinalRange'+e+')', weInteger(SignedIntAnd(50+n                  , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Byte_2'+e+')',        weInteger(SignedIntAnd(240+n                 , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Word_2'+e+')',        weInteger(SignedIntAnd(65501+n               , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Longword_2'+e+')',    weInteger(SignedIntAnd(4123456789+n          , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'QWord_2'+e+')',       weInteger(SignedIntAnd(15446744073709551610+n, tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Shortint_2'+e+')',    weInteger(SignedIntAnd(112+n                 , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Smallint_2'+e+')',    weInteger(SignedIntAnd(32012+n               , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Longint_2'+e+')',     weInteger(SignedIntAnd(20123456+n            , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Int64_2'+e+')',       weInteger(SignedIntAnd(9123372036854775801+n , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Shortint_3'+e+')',    weInteger(SignedIntAnd(-112+n                , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Smallint_3'+e+')',    weInteger(SignedIntAnd(-32012+n              , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Longint_3'+e+')',     weInteger(SignedIntAnd(-20123456+n           , tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Int64_3'+e+')',       weInteger(SignedIntAnd(-9123372036854775801+n, tm), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Char'+e+')',          weInteger(ord(AChr1), tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Char2'+e+')',         weInteger(ord(#0),    tn, -1));
      t.Add(AName+' '+tn, tn+'('+p+'Char3'+e+')',         weInteger(ord(' '),   tn, -1));
    end;
    {$POP}

    t.Add(AName+' Char', 'Char('+p+'Byte'+e+')',          weChar(Chr(1+n), 'Char'));

    t.Add(AName+' Char', 'Char('+p+'FiveRec'+e+')',          weMatch('.', skSimple)).ExpectError();

  end;

  procedure AddWatchesCast(t: TWatchExpectationList; AName: String; APrefix: String; AOffs: Integer; AChr1: Char;
    ALoc: TTestLoc = tlAny; APostFix: String = '');
  var
    p, e, val: String;
    Thread: Integer;
  begin
    p := APrefix;
    e := APostFix;
    t2.Clear;

      t2.AddWithoutExpect(AName, p+'Instance1_Int'+e);
      t2.AddWithoutExpect(AName, p+'Ansi5_Int'+e);
      t2.AddWithoutExpect(AName, p+'IntDynArray4_Int'+e);
      t2.EvaluateWatches;
      Thread := Debugger.Threads.Threads.CurrentThreadId;

    if not(ALoc in [tlConst]) then begin
      val := t2.Tests[0]^.TstWatch.Values[Thread, 0].Value;
      t.Add(AName+' Int', 'PtrUInt('+p+'Instance1'+e+')',   weCardinal(StrToQWordDef(val, qword(-7)), 'PtrUInt', -1));
      t.Add(AName+' TClass1', 'TClass1('+p+'Instance1_Int'+e+')',   weMatch('FAnsi *=[ $0-9A-F()]*'''+AChr1+'T', skClass));
      t.Add(AName+' TClass1', 'TClass1('+val+')',   weMatch('FAnsi *=[ $0-9A-F()]*'''+AChr1+'T', skClass));

      val := t2.Tests[1]^.TstWatch.Values[Thread, 0].Value;
      t.Add(AName+' Ansi', 'PtrUInt('+p+'Ansi5'+e+')',   weCardinal(StrToQWordDef(val, qword(-7)), 'PtrUInt', -1));
if not(Compiler.SymbolType in stDwarf3Up) then begin
      t.Add(AName+' AnsiString', 'AnsiString('+p+'Ansi5_Int'+e+')',
        weAnsiStr(AChr1+'bcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghij')
      ).IgnKindPtr(stDwarf2)    .IgnKind(stDwarf3Up);
      t.Add(AName+' AnsiString', 'AnsiString('+val+')',
        weAnsiStr(AChr1+'bcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghij')
      ).IgnKindPtr(stDwarf2)    .IgnKind(stDwarf3Up);
end;

      val := t2.Tests[2]^.TstWatch.Values[Thread, 0].Value;
      t.Add(AName+' DynArray', 'PtrUInt('+p+'IntDynArray4'+e+')',   weCardinal(StrToQWordDef(val, qword(-7)), 'PtrUInt', -1));
if not(Compiler.SymbolType in stDwarf3Up) then begin
      t.Add(AName, 'TIntDynArray('+p+'IntDynArray4_Int'+e+')',  weDynArray(weInteger([12, 30+AOffs, 60]), 'TIntDynArray'));
      t.Add(AName, 'TIntDynArray('+val+')',  weDynArray(weInteger([12, 30+AOffs, 60]), 'TIntDynArray'));

end;
    end;

    t.Add(AName+' Cardinal', 'Cardinal('+p+'Rec3S'+e+')',  weMatch('.', skSimple)).ExpectError();
    t.Add(AName+' QWord', 'QWord('+p+'Rec3S'+e+')',        weMatch('.', skSimple)).ExpectError();

    t.Add(AName+' QWord', 'TRecord3QWord('+p+'Rec3S'+e+')',        weMatch('a *=.*18446744073709551594.*b *=.*44.*c *= .', skRecord));

  end;

var
  ExeName: String;
  t: TWatchExpectationList;
  Src: TCommonSource;
  BrkPrg, BrkFoo, BrkFooVar, BrkFooConstRef: TDBGBreakPoint;
begin
  if SkipTest then exit;
  if not TestControlCanTest(ControlTestWatchTypeCast) then exit;
  t := nil;
  t2 := nil;

  Src := GetCommonSourceFor('WatchesValuePrg.Pas');
  TestCompile(Src, ExeName);

  AssertTrue('Start debugger', Debugger.StartDebugger(AppDir, ExeName));

  try
    t := TWatchExpectationList.Create(Self);
    t2 := TWatchExpectationList.Create(Self);
    t.AcceptSkSimple := [skInteger, skCardinal, skBoolean, skChar, skFloat, skString, skAnsiString, skCurrency, skVariant, skWideString];
    t.AddTypeNameAlias('integer', 'integer|longint');
    t.AddTypeNameAlias('ShortStr255', 'ShortStr255|ShortString');
    t.AddTypeNameAlias('TEnumSub', 'TEnum|TEnumSub');

    BrkPrg         := Debugger.SetBreakPoint(Src, 'Prg');
    BrkFoo         := Debugger.SetBreakPoint(Src, 'Foo');
    BrkFooVar      := Debugger.SetBreakPoint(Src, 'FooVar');
    BrkFooConstRef := Debugger.SetBreakPoint(Src, 'FooConstRef');
    AssertDebuggerNotInErrorState;

    (* ************ Nested Functions ************* *)

    RunToPause(BrkPrg);

    t.Clear;
    AddWatchesConv(t, 'glob const', 'gc', 000, 'A', tlConst);
    AddWatchesConv(t, 'glob var',   'gv', 001, 'B');

    AddWatchesCast(t, 'glob const', 'gc', 000, 'A', tlConst);
    AddWatchesCast(t, 'glob var',   'gv', 001, 'B');
    AddWatchesCast(t, 'glob MyClass1',     'MyClass1.mc',  002, 'C');
    AddWatchesCast(t, 'glob MyBaseClass1', 'MyClass1.mbc', 003, 'D');
    AddWatchesCast(t, 'glob MyClass1',     'TMyClass(MyClass2).mc',  004, 'E');
    AddWatchesCast(t, 'glob MyBaseClass1', 'TMyClass(MyClass2).mbc', 005, 'F');
    AddWatchesCast(t, 'glob var dyn array of [0]',   'gva', 005, 'K', tlArrayWrap, '[0]' );
    AddWatchesCast(t, 'glob var dyn array of [1]',   'gva', 006, 'L', tlArrayWrap, '[1]');
    AddWatchesCast(t, 'glob var pointer',            'gvp_', 001, 'B', tlPointer, '^'); // pointer
    t.EvaluateWatches;
    t.CheckResults;


    RunToPause(BrkFoo);
    t.Clear;
    AddWatchesCast(t, 'foo local', 'fooloc', 002, 'C');
    t.EvaluateWatches;
    t.CheckResults;


    RunToPause(BrkFooVar);
    t.Clear;
    AddWatchesCast(t, 'foo var args', 'argvar', 001, 'B', tlParam);
    AddWatchesCast(t, 'foo var ArgMyBaseClass1', 'TMyClass(ArgVarMyClass2).mbc', 005, 'F');
    t.EvaluateWatches;
    t.CheckResults;

    RunToPause(BrkFooConstRef);
    t.Clear;
    AddWatchesCast(t, 'foo const ref args', 'argconstref', 001, 'B', tlParam);
    t.EvaluateWatches;
    t.CheckResults;


  finally
    t.Free;
    t2.Free;
    Debugger.ClearDebuggerMonitors;
    Debugger.FreeDebugger;

    AssertTestErrors;
  end;
end;


initialization
  RegisterDbgTest(TTestWatches);
  ControlTestWatch          := TestControlRegisterTest('TTestWatch');
  ControlTestWatchScope     := TestControlRegisterTest('Scope', ControlTestWatch);
  ControlTestWatchValue     := TestControlRegisterTest('Value', ControlTestWatch);
  ControlTestWatchAddressOf := TestControlRegisterTest('AddressOf', ControlTestWatch);
  ControlTestWatchTypeCast  := TestControlRegisterTest('TypeCast', ControlTestWatch);

end.

