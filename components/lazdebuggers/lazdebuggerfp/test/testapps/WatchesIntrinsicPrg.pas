program WatchesIntrinsicPrg;
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
    More:  array [3..9] of TFoo;
    More2: array [0..1,3..9] of TFoo;
    MoreIdx: array [0..4] of Integer;
    constructor Create(v: integer);
  end;

var
  f1, f2, f3, f4, f5, f6, f7, f8: TFoo;
  fa: array [0..9] of TFoo;
  bytes: array [0..30] of byte;


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

{ TFoo }

constructor TFoo.Create(v: integer);
var
  i: Integer;
begin
  Value := v;
  Dummy.a := 990+v;
  Dummy2.a.a := 1990+v;
  for i := 0 to 4 do MoreIdx[i] := i;
end;

procedure TIntf2.Foo; begin  WriteLn(5); end;
procedure TIntf2.Bar; begin  WriteLn(6); end;
procedure TIntf1.Foo; begin  WriteLn(1); end;
procedure TIntf1.Bar; begin  WriteLn(2); end;

var
  AnIntf1: IIntf1;  AnIntf2: IIntf2;
  AnObj1:  TIntf1;  AnObj2:  TIntf2;
  i: Integer;
  o: TObject;

begin
  for i := 0 to high(bytes) do bytes[i] := i+1;
  o := TObject.Create;

  f1 := TFoo.Create(1);
  f2 := TFoo.Create(2);
  f3 := TFoo.Create(3);
  f4 := TFoo.Create(4);
  f5 := TFoo.Create(5);
  f6 := TFoo.Create(6);
  f7 := TFoo.Create(7);
  f8 := TFoo.Create(8);

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

  f1.More[3] := nil;
  f1.More[4] := TFoo.Create(100004);
  f1.More[5] := TFoo.Create(100005);
  f1.More[6] := TFoo.Create(100006);
  f1.More[7] := TFoo.Create(100007);
  f1.More[8] := f2;
  f1.More[9] := f3;

  f2.More[3] := f4;
  f2.More[4] := TFoo.Create(200004);
  f2.More[5] := TFoo.Create(200005);
  f2.More[6] := TFoo.Create(200006);

  f3.More[3] := TFoo.Create(300003);
  f3.More[4] := TFoo.Create(300004);
  f3.More[5] := TFoo.Create(300005);

  f4.More[3] := TFoo.Create(400003);
  f4.More[4] := TFoo.Create(400004);

  for i := 0 to 1 do begin
    f1.More2[i,3] := nil;
    f1.More2[i,4] := TFoo.Create(100004+(i+1)*10000000);
    f1.More2[i,5] := TFoo.Create(100005+(i+1)*10000000);
    f1.More2[i,6] := TFoo.Create(100006+(i+1)*10000000);
    f1.More2[i,7] := TFoo.Create(100007+(i+1)*10000000);
    f1.More2[i,8] := f2;
    f1.More2[i,9] := f3;

    f2.More2[i,3] := f4;
    f2.More2[i,4] := TFoo.Create(200004+(i+1)*10000000);
    f2.More2[i,5] := TFoo.Create(200005+(i+1)*10000000);
    f2.More2[i,6] := TFoo.Create(200006+(i+1)*10000000);

    f3.More2[i,3] := TFoo.Create(300003+(i+1)*10000000);
    f3.More2[i,4] := TFoo.Create(300004+(i+1)*10000000);
    f3.More2[i,5] := TFoo.Create(300005+(i+1)*10000000);

    f4.More2[i,3] := TFoo.Create(400003+(i+1)*10000000);
    f4.More2[i,4] := TFoo.Create(400004+(i+1)*10000000);
  end;


  AnObj1 := TIntf1.Create; AnObj1.a := 123; AnObj1.b := 987; AnObj1.c := 551177;
  AnObj2 := TIntf2.Create; AnObj2.x := 321; AnObj2.y := 789; AnObj2.c := 441188;
  AnIntf1 := AnObj1;
  AnIntf2 := AnObj2;



  fa[9] := fa[8]; // TEST_BREAKPOINT=Prg
  fa[9] := fa[8];

end.

