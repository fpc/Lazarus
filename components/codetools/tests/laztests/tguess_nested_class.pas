program tguess_nested_class;
{$Mode objfpc}{$ModeSwitch advancedrecords}
type
  TOuter = (aa,bb);

  TNest = class
    protected type
      TNotNestBar = (e,f);
    public
      procedure func;
  end;

  TRec = record
    public type
      TRecType = (a1,a2);
  end;
  TObj = object
    protected type
      TObjType = (b1,b2);
  end;


  TFoo = class
  protected type
    TBar = (a,b);
    TBarX = (xa,xb);

    TNest = class
      protected type
        TNestBar = (c,d);
        TBarX = (xc,xd);
      public
        procedure func;
        function f1: TOuter; virtual; abstract;
        function f2: TBar; virtual; abstract;
        function f3: TNestBar; virtual; abstract;
        function f4: TFoo.TBar; virtual; abstract;
        function f5: TNest.TNestBar; virtual; abstract;
        function f6: TFoo.TNest.TNestBar; virtual; abstract;
        function f7: TRec.TRecType; virtual; abstract;
        function f8: TObj.TObjType; virtual; abstract;
        function f9: TBarX; virtual; abstract;
        function fa: TFoo.TBarX; virtual; abstract;
        function fb: TFoo.TNest.TBarX; virtual; abstract;
    end;
  public
    procedure func;
    function f1: TOuter; virtual; abstract;
    function f2: TBar; virtual; abstract;
    //function f3: TNestBar; virtual; abstract;
    function f4: TFoo.TBar; virtual; abstract;
    function f5: TNest.TNestBar; virtual; abstract;
    function f6: TFoo.TNest.TNestBar; virtual; abstract;
    function f7: TRec.TRecType; virtual; abstract;
    function f8: TObj.TObjType; virtual; abstract;
    function f9: TBarX; virtual; abstract;
    function fa: TFoo.TBarX; virtual; abstract;
    function fb: TFoo.TNest.TBarX; virtual; abstract;
  end;

function f1: TOuter; begin end;
//function f2: TBar; begin end;
//function f3: TNestBar; begin end;
function f4: TFoo.TBar; begin end;
//function f5: TNest.TNestBar; begin end;
function f6: TFoo.TNest.TNestBar; begin end;
function f7: TRec.TRecType; begin end;
function f8: TObj.TObjType; begin end;
//function f9: TBarX; virtual; abstract;
function fa: TFoo.TBarX; begin end;
function fb: TFoo.TNest.TBarX; begin end;


procedure TNest.func;
var
  foo: TFoo; nest: TFoo.TNest;

begin
  i{guesstype:integer} := 1;
  x{guesstype:TFoo.TBar} := a;
  y{guesstype:TFoo.TNest.TNestBar} := c;
  z{TODO: guesstype:TNotNestBar} := e;
  xx{guesstype:TFoo.TBarX} := xa;
  xy{guesstype:TFoo.TNest.TBarX} := xc;

  x1{guesstype:TOuter} := foo.f1;
  x2{guesstype:TFoo.TBar} := foo.f2;
  //x3 := foo.f3;
  x4{guesstype:TFoo.TBar} := foo.f4;
  x5{guesstype:TFoo.TNest.TNestBar} := foo.f5;
  x6{guesstype:TFoo.TNest.TNestBar} := foo.f6;
  x7{guesstype:TRec.TRecType} := foo.f7;
  x8{guesstype:TObj.TObjType} := foo.f8;
  x9{guesstype:TFoo.TBarX} := foo.f9;
  x11{guesstype:TFoo.TBarX} := foo.fa;
  x12{guesstype:TFoo.TNest.TBarX} := foo.fb;

  y1{guesstype:TOuter} := nest.f1;
  y2{guesstype:TFoo.TBar} := nest.f2;
  y3{guesstype:TFoo.TNest.TNestBar} := nest.f3;
  y4{guesstype:TFoo.TBar} := nest.f4;
  y5{guesstype:TFoo.TNest.TNestBar} := nest.f5;
  y6{guesstype:TFoo.TNest.TNestBar} := nest.f6;
  y7{guesstype:TRec.TRecType} := nest.f7;
  y8{guesstype:TObj.TObjType} := nest.f8;
  y9{guesstype:TFoo.TNest.TBarX} := nest.f9;
  y11{guesstype:TFoo.TBarX} := nest.fa;
  y12{guesstype:TFoo.TNest.TBarX} := nest.fb;

  z1{guesstype:TOuter} := f1;
  //z2 := f2;
  //z3 := f3;
  z4{guesstype:TFoo.TBar} := f4;
  //z5 := f5;
  z6{guesstype:TFoo.TNest.TNestBar} := f6;
  z7{guesstype:TRec.TRecType} := f7;
  z8{guesstype:TObj.TObjType} := f8;

end;

{ TFoo.TNest }

procedure TFoo.TNest.func;
var
  foo: TFoo;   nest: TFoo.TNest;

begin
  i{guesstype:integer} := 1;
  x{guesstype:TBar} := a;
  y{TODO: guesstype:TNestBar} := c;
  z{guesstype:TNest.TNotNestBar} := e;
  xx{TODO: guesstype:TFoo.TBarX} := xa;
  xy{TODO: guesstype:TBarX} := xc;

  x1{guesstype:TOuter} := foo.f1;
  x2{guesstype:TFoo.TBar} := foo.f2;
  //x3 := foo.f3;
  x4{guesstype:TFoo.TBar} := foo.f4;
  x5{guesstype:TNestBar} := foo.f5;
  x6{guesstype:TNestBar} := foo.f6;
  x7{guesstype:TRec.TRecType} := foo.f7;
  x8{guesstype:TObj.TObjType} := foo.f8;
  x9{guesstype:TFoo.TBarX} := foo.f9;
  x11{guesstype:TFoo.TBarX} := foo.fa;
  x12{guesstype:TBarX} := foo.fb;

  y1{guesstype:TOuter} := nest.f1;
  y2{guesstype:TFoo.TBar} := nest.f2;
  y3{guesstype:TNestBar} := nest.f3;
  y4{guesstype:TFoo.TBar} := nest.f4;
  y5{guesstype:TNestBar} := nest.f5;
  y6{guesstype:TNestBar} := nest.f6;
  y7{guesstype:TRec.TRecType} := nest.f7;
  y8{guesstype:TObj.TObjType} := nest.f8;
  y9{guesstype:TBarX} := nest.f9;
  y11{guesstype:TFoo.TBarX} := nest.fa;
  y12{guesstype:TBarX} := nest.fb;

  z1{guesstype:TOuter} := f1;
  //z2 := f2;
  //z3 := f3;
  z4{guesstype:TFoo.TBar} := f4;
  //z5 := f5;
  z6{guesstype:TNestBar} := f6;
  z7{guesstype:TRec.TRecType} := f7;
  z8{guesstype:TObj.TObjType} := f8;

end;

{ TFoo }

procedure TFoo.func;
var
  foo: TFoo;   nest: TFoo.TNest;
begin
  i{guesstype:integer} := 1;
  x{TODO: guesstype:TBar} := a;
  y{TODO: guesstype:TNest.TNestBar} := c;
  z{TODO:  guesstype:tguess_nested_class.TNest.TNotNestBar} := e;  // works, but test can't test for project name
  xx{TODO: guesstype:TBarX} := xa;
  xy{TODO: guesstype:TNest.TBarX} := xc;

  x1{guesstype:TOuter} := foo.f1;
  x2{guesstype:TBar} := foo.f2;
  //x3 := foo.f3;
  x4{guesstype:TBar} := foo.f4;
  x5{TODO: guesstype:TNest.TNestBar} := foo.f5;
  x6{TODO: guesstype:TNest.TNestBar} := foo.f6;
  x7{guesstype:TRec.TRecType} := foo.f7;
  x8{guesstype:TObj.TObjType} := foo.f8;
  x9{guesstype:TBarX} := foo.f9;
  x11{guesstype:TBarX} := foo.fa;
  x12{TODO: guesstype:TNest.TBarX} := foo.fb;

  y1{guesstype:TOuter} := nest.f1;
  y2{guesstype:TBar} := nest.f2;
  y3{TODO: guesstype:TNest.TNestBar} := nest.f3;
  y4{guesstype:TBar} := nest.f4;
  y5{TODO: guesstype:TNest.TNestBar} := nest.f5;
  y6{TODO: guesstype:TNest.TNestBar} := nest.f6;
  y7{guesstype:TRec.TRecType} := nest.f7;
  y8{guesstype:TObj.TObjType} := nest.f8;
  y9{TODO: guesstype:TNest.TBarX} := nest.f9;
  y11{guesstype:TBarX} := nest.fa;
  y12{TODO: guesstype:TNest.TBarX} := nest.fb;

  z1{guesstype:TOuter} := f1;
  z2 := f2;
  //z3 := f3;
  z4{guesstype:TBar} := f4;
  z5 := f5;
  z6{TODO: guesstype:TNest.TNestBar} := f6;
  z7{guesstype:TRec.TRecType} := f7;
  z8{guesstype:TObj.TObjType} := f8;

end;

var
  foo: TFoo;
  nest: TFoo.TNest;

begin
  i{guesstype:integer} := 1;
  x{guesstype:TFoo.TBar} := a;
  y{guesstype:TFoo.TNest.TNestBar} := c;
  z{guesstype:TNest.TNotNestBar} := e;
  xx{guesstype:TFoo.TBarX} := xa;
  xy{guesstype:TFoo.TNest.TBarX} := xc;

  x1{guesstype:TOuter} := foo.f1;
  x2{guesstype:TFoo.TBar} := foo.f2;
  //x3 := foo.f3;
  x4{guesstype:TFoo.TBar} := foo.f4;
  x5{guesstype:TFoo.TNest.TNestBar} := foo.f5;
  x6{guesstype:TFoo.TNest.TNestBar} := foo.f6;
  x7{guesstype:TRec.TRecType} := foo.f7;
  x8{guesstype:TObj.TObjType} := foo.f8;
  x9{guesstype:TFoo.TBarX} := foo.f9;
  x11{guesstype:TFoo.TBarX} := foo.fa;
  x12{guesstype:TFoo.TNest.TBarX} := foo.fb;

  y1{guesstype:TOuter} := nest.f1;
  y2{guesstype:TFoo.TBar} := nest.f2;
  y3{guesstype:TFoo.TNest.TNestBar} := nest.f3;
  y4{guesstype:TFoo.TBar} := nest.f4;
  y5{guesstype:TFoo.TNest.TNestBar} := nest.f5;
  y6{guesstype:TFoo.TNest.TNestBar} := nest.f6;
  y7{guesstype:TRec.TRecType} := nest.f7;
  y8{guesstype:TObj.TObjType} := nest.f8;
  y9{guesstype:TFoo.TNest.TBarX} := nest.f9;
  y11{guesstype:TFoo.TBarX} := nest.fa;
  y12{guesstype:TFoo.TNest.TBarX} := nest.fb;

  z1{guesstype:TOuter} := f1;
  //z2 := f2;
  //z3 := f3;
  z4{guesstype:TFoo.TBar} := f4;
  //z5 := f5;
  z6{guesstype:TFoo.TNest.TNestBar} := f6;
  z7{guesstype:TRec.TRecType} := f7;
  z8{guesstype:TObj.TObjType} := f8;

  r{guesstype:TRec.TRecType} := a1;
  o{guesstype:TObj.TObjType} := b1;
end.

