program generic_base;

{$mode objfpc}{$H+}

uses
  Classes;

type

  { TBase }

  TBase = class
    procedure BaseProc;
    procedure BaseProc2; virtual; abstract;
  end;

  { TSubBase }

  TSubBase = class(TBase)
    procedure BaseProc; reintroduce;
    procedure BaseProc2; virtual; abstract;
  end;

  { TFoo1 }

  TFoo1 = class(TStream)
    procedure Foo;
  end;

  { TFoo2 }

  TFoo2 = class(TStream)
    procedure Foo;
  end;

  { TBar }

  TBar = class(TStream)
    procedure Bar;
  end;

  { TGen1 }

  generic TGen1<Base: TObject; _B> = class(Base)
    procedure GenBar(P1: _B);
  end;

  { TGen2 }

  generic TGen2<_B; _X, Base: TObject> = class(Base)
    procedure GenBar(P1: _B; P2: _X);
  end;

  { TGen3 }

  generic TGen3<_B; Base: TBase> = class(Base)
    procedure GenBar(P1: _B);
    procedure BaseProc2{declaration:TBase.BaseProc2}; reintroduce; virtual; abstract;
  end;

  generic TGenGen1<XBase: TObject; _BB> = class(specialize TGen1<XBase, _BB>)
  end;


  TSpec1Foo1 = specialize TGen1<TFoo1, TPoint>;
  TSpec1Foo2 = specialize TGen1<TFoo2, TPoint>;

  TSpec2Foo1 = specialize TGen2<Integer, TBase, TFoo1>;

  TSpecGG1Foo1 = specialize TGenGen1<TFoo1, TPoint>;

  //TSpec3Foo1 = specialize TGen2<Integer, TPoint, TFoo1>;

  { TSpec1ClsFoo1 }

  TSpec1ClsFoo1 = class(specialize TGen1<TFoo1, TPoint>)
    procedure Spec1a;
  end;

  TSpec1ClsFoo1X = class(specialize TGen1<TFoo1, TPoint>)
    procedure Foo{declaration:TFoo1.Foo}; virtual; abstract;
  end;

  { TSpec1ClsFoo2 }

  TSpec1ClsFoo2 = class(specialize TGen1<TFoo2, TPoint>)
    procedure Spec1a;
  end;

  TSpec1ClsFoo2X = class(specialize TGen1<TFoo2, TPoint>)
    procedure Foo{declaration:TFoo2.Foo}; virtual; abstract;
  end;

  { TSpec3ClsSubBase }

  TSpec3ClsSubBase = class(specialize TGen3<TBase, TSubBase>)
    procedure Spec1a;
  end;

{ TBase }

procedure TBase.BaseProc;
begin

end;

{ TSubBase }

procedure TSubBase.BaseProc;
begin

end;

{ TSpec1ClsFoo2 }

procedure TSpec1ClsFoo2.Spec1a;
begin
  //
end;

{ TFoo1 }

procedure TFoo1.Foo;
var
  s1f1: TSpec1Foo1;
  s1f2: TSpec1Foo2;
  s1CF1: TSpec1ClsFoo1;
  s1CF2: TSpec1ClsFoo2;

  s2f1: TSpec2Foo1;
  sgg1f1: TSpecGG1Foo1;

  s1x: specialize TGen1<TFoo1, TRect>;
begin
  s1f1.Foo{declaration:TFoo1.Foo};
  s1f2.Foo{declaration:TFoo2.Foo};
  s1CF1.Foo{declaration:TFoo1.Foo};
  s1CF2.Foo{declaration:TFoo2.Foo};

  s2f1.Foo{declaration:TFoo1.Foo};
  sgg1f1.Foo{ TODO declaration:TFoo1.Foo};

  s1x.Foo{declaration:TFoo1.Foo};
end;

{ TFoo2 }

procedure TFoo2.Foo;
begin

end;

{ TBar }

procedure TBar.Bar;
begin

end;

{ TGen1 }

procedure TGen1.GenBar(P1: _B);
begin

end;

{ TGen2 }

procedure TGen2.GenBar(P1: _B; P2: _X);
begin

end;

{ TGen3 }

procedure TGen3.GenBar(P1: _B);
begin
  BaseProc{declaration:TBase.BaseProc};
end;

{ TSpec1ClsFoo1 }

procedure TSpec1ClsFoo1.Spec1a;
begin
  inherited foo{declaration:TFoo1.Foo};
  foo{declaration:TFoo1.Foo};
end;

{ TSpec3ClsSubBase }

procedure TSpec3ClsSubBase.Spec1a;
begin

end;


var
  g1: TSpec1Foo1;
  g1a: TSpec1ClsFoo1;
  g1x: specialize TGen1<TFoo1, TRect>;

  s3: TSpec3ClsSubBase;
begin
  g1a.Foo{declaration:TFoo1.Foo};
  g1.Foo{declaration:TFoo1.Foo};

  s3.BaseProc{declaration:TSubBase.BaseProc};
end.

