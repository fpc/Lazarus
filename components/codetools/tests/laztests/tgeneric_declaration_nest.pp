program tgeneric_declaration_nest;
{$mode objfpc}{$H+}
type

  TBase = class
    procedure DoBase;
  end;

  generic TGen<B> = class
    FData: B;
  end;

  TTestClass = class
  protected type
    generic TInnerGen<T: TBase> = class
      f: T;
      procedure Bar;
    end;
  protected type
    TBar = specialize TInnerGen<TTestClass>;
  public
    procedure Foo;
  end;

  TOuterBar = specialize TTestClass.TInnerGen<TTestClass>;


var
  tc: TTestClass;
  tg: TOuterBar;

{ TTestClass.TInnerGen }

procedure TTestClass.TInnerGen{declaration:TTestClass.TInnerGen}.Bar{declaration:TTestClass.TInnerGen.Bar};
var a: t{declaration:TTestClass.TInnerGen.T};
begin
  Bar{declaration:TTestClass.TInnerGen.Bar};
  c{guesstype:TBase} := f.Create{declaration:system.TObject.Create};
  f.{completion:DoBase,Create,AfterConstruction}DoBase{declaration:TBase.DoBase};
end;

{ TTestClass }

procedure TTestClass.Foo;
begin

end;


begin
  tc.Foo{declaration:TTestClass.Foo};
  tg.Bar{declaration:TTestClass.TInnerGen.Bar};
  x{ TODO guesstype:TTestClass.TBar} := TTestClass.TBar.Create;
end.
