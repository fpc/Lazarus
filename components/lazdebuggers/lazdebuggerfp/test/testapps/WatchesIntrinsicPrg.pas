program intrinsic;
uses Classes;

type
  TDummy = packed record a: integer; end;
  TDummy1 = packed record a,b,c: integer; end;
  TDummy2 = packed record
    a: TDummy1;
  end;

  PFoo = ^TFoo;
  TFoo = class
    Value: integer;
    Idx: integer;
    Left, Right: TFoo;
    Next: TFoo;
    LeftP, RightP: PFoo;
    NextP: PFoo;
    Dummy: TDummy;
    Dummy2: TDummy2;
  end;

var
  f1, f2, f3, f4, f5, f6, f7, f8: TFoo;
  fa: array [0..9] of TFoo;


type
{$Interfaces COM}
  IIntf1 = interface ['{5CD5BFCA-3865-4CE9-916C-775E0DFB6BF5}']
    procedure Foo;
    procedure Bar;
  end;

  TIntf1 = class(TInterfacedObject, IIntf1)
    a,b,c: integer;
    procedure Foo;
    procedure Bar;
  end;

type
{$Interfaces CORBA}
  IIntf2 = interface ['{C3E936EE-479B-404E-AEE8-C94CB6DDE462}']
    procedure Foo;
    procedure Bar;
  end;
  TIntf2 = class(TObject, IIntf2)
    x,y,c: integer;
    procedure Foo;
    procedure Bar;
  end;

procedure TIntf2.Foo; begin  WriteLn(5); end;
procedure TIntf2.Bar; begin  WriteLn(6); end;
procedure TIntf1.Foo; begin  WriteLn(1); end;
procedure TIntf1.Bar; begin  WriteLn(2); end;

var
  AnIntf1: IIntf1;  AnIntf2: IIntf2;
  AnObj1:  TIntf1;  AnObj2:  TIntf2;

begin
  f1 := TFoo.Create; f1.Value := 1; f1.Dummy.a := 991; f1.Dummy2.a.a := 1991;
  f2 := TFoo.Create; f2.Value := 2; f2.Dummy.a := 992; f2.Dummy2.a.a := 1992;
  f3 := TFoo.Create; f3.Value := 3; f3.Dummy.a := 993; f3.Dummy2.a.a := 1993;
  f4 := TFoo.Create; f4.Value := 4; f4.Dummy.a := 994; f4.Dummy2.a.a := 1994;
  f5 := TFoo.Create; f5.Value := 5; f5.Dummy.a := 995; f5.Dummy2.a.a := 1995;
  f6 := TFoo.Create; f6.Value := 6; f6.Dummy.a := 996; f6.Dummy2.a.a := 1996;
  f7 := TFoo.Create; f7.Value := 7; f7.Dummy.a := 997; f7.Dummy2.a.a := 1997;
  f8 := TFoo.Create; f8.Value := 8; f8.Dummy.a := 998; f8.Dummy2.a.a := 1998;

  f1.Next := f2;   f1.NextP := @f2;
  f2.Next := f3;   f2.NextP := @f3;
  f3.Next := f4;   f3.NextP := @f4;
  f4.Next := f1;   f4.NextP := @f1;

  f1.Left := f2;
    f2.Left := f3;
      f3.Left := f4;
      f3.Right := f5;
    f2.Right := f6;
      f6.Right := f3;  // not recursive
        // left = 4
        // right = 5
  f1.Right:= f7;
    f7.Right:= f8;
      f8.Right:= f7;

  fa[4] := f1;  f1.Idx := 3;
  fa[3] := f2;  f2.Idx := 5;
  fa[5] := f3;  f3.Idx := 0;
  fa[0] := f4;  f4.Idx := 0;  // recurse self

  AnObj1 := TIntf1.Create; AnObj1.a := 123; AnObj1.b := 987; AnObj1.c := 551177;
  AnObj2 := TIntf2.Create; AnObj2.x := 321; AnObj2.y := 789; AnObj2.c := 441188;
  AnIntf1 := AnObj1;
  AnIntf2 := AnObj2;



  fa[9] := fa[8]; // TEST_BREAKPOINT=Prg
  fa[9] := fa[8];

end.

