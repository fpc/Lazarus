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
  i{anytype:integer} := 1;
  x{anytype:TFoo.TBar} := a;
  y{anytype:TFoo.TNest.TNestBar} := c;
  z{anytype:TNotNestBar} := e;
  xx{anytype:TFoo.TBarX} := xa;
  xy{anytype:TFoo.TNest.TBarX} := xc;

  x1{anytype:TOuter} := foo.f1;
  x2{anytype:TFoo.TBar} := foo.f2;
  //x3 := foo.f3;
  x4{anytype:TFoo.TBar} := foo.f4;
  x5{anytype:TFoo.TNest.TNestBar} := foo.f5;
  x6{anytype:TFoo.TNest.TNestBar} := foo.f6;
  x7{anytype:TRec.TRecType} := foo.f7;
  x8{anytype:TObj.TObjType} := foo.f8;
  x9{anytype:TFoo.TBarX} := foo.f9;
  x11{anytype:TFoo.TBarX} := foo.fa;
  x12{anytype:TFoo.TNest.TBarX} := foo.fb;

  y1{anytype:TOuter} := nest.f1;
  y2{anytype:TFoo.TBar} := nest.f2;
  y3{anytype:TFoo.TNest.TNestBar} := nest.f3;
  y4{anytype:TFoo.TBar} := nest.f4;
  y5{anytype:TFoo.TNest.TNestBar} := nest.f5;
  y6{anytype:TFoo.TNest.TNestBar} := nest.f6;
  y7{anytype:TRec.TRecType} := nest.f7;
  y8{anytype:TObj.TObjType} := nest.f8;
  y9{anytype:TFoo.TNest.TBarX} := nest.f9;
  y11{anytype:TFoo.TBarX} := nest.fa;
  y12{anytype:TFoo.TNest.TBarX} := nest.fb;

  z1{anytype:TOuter} := f1;
  //z2 := f2;
  //z3 := f3;
  z4{anytype:TFoo.TBar} := f4;
  //z5 := f5;
  z6{anytype:TFoo.TNest.TNestBar} := f6;
  z7{anytype:TRec.TRecType} := f7;
  z8{anytype:TObj.TObjType} := f8;

end;

{ TFoo.TNest }

procedure TFoo.TNest.func;
var
  foo: TFoo;   nest: TFoo.TNest;

begin
  i{anytype:integer} := 1;
  x{anytype:TBar} := a;
  y{anytype:TNestBar} := c;
  z{guesstype:TNest.TNotNestBar}{TODO: vartype:TNest.TNotNestBar} := e;  // TOOD: vartype => this wrongly has a unit name prefix
  xx{TODO: guesstype:TFoo.TBarX}{vartype:TFoo.TBarX} := xa;
  xy{anytype:TBarX} := xc;

  x1{anytype:TOuter} := foo.f1;
  x2{anytype:TFoo.TBar} := foo.f2;
  //x3 := foo.f3;
  x4{anytype:TFoo.TBar} := foo.f4;
  x5{anytype:TNestBar} := foo.f5;
  x6{anytype:TNestBar} := foo.f6;
  x7{anytype:TRec.TRecType} := foo.f7;
  x8{anytype:TObj.TObjType} := foo.f8;
  x9{anytype:TFoo.TBarX} := foo.f9;
  x11{anytype:TFoo.TBarX} := foo.fa;
  x12{anytype:TBarX} := foo.fb;

  y1{anytype:TOuter} := nest.f1;
  y2{anytype:TFoo.TBar} := nest.f2;
  y3{anytype:TNestBar} := nest.f3;
  y4{anytype:TFoo.TBar} := nest.f4;
  y5{anytype:TNestBar} := nest.f5;
  y6{anytype:TNestBar} := nest.f6;
  y7{anytype:TRec.TRecType} := nest.f7;
  y8{anytype:TObj.TObjType} := nest.f8;
  y9{anytype:TBarX} := nest.f9;
  y11{anytype:TFoo.TBarX} := nest.fa;
  y12{anytype:TBarX} := nest.fb;

  z1{anytype:TOuter} := f1;
  //z2 := f2;
  //z3 := f3;
  z4{anytype:TFoo.TBar} := f4;
  //z5 := f5;
  z6{anytype:TNestBar} := f6;
  z7{anytype:TRec.TRecType} := f7;
  z8{anytype:TObj.TObjType} := f8;

end;

{ TFoo }

procedure TFoo.func;
var
  foo: TFoo;   nest: TFoo.TNest;
begin
  i{anytype:integer} := 1;
  x{anytype:TBar} := a;
  y{TODO: anytype:TNest.TNestBar} := c;
  z{vartype:tguess_nested_class.TNest.TNotNestBar} := e;
  //z{ vartype:TNest.TNotNestBar} := e;  // works, but test can't test for project name
  xx{anytype:TBarX} := xa;
  xy{TODO: anytype:TNest.TBarX} := xc;

  x1{anytype:TOuter} := foo.f1;
  x2{anytype:TBar} := foo.f2;
  //x3 := foo.f3;
  x4{anytype:TBar} := foo.f4;
  x5{TODO: anytype:TNest.TNestBar} := foo.f5;
  x6{TODO: anytype:TNest.TNestBar} := foo.f6;
  x7{anytype:TRec.TRecType} := foo.f7;
  x8{anytype:TObj.TObjType} := foo.f8;
  x9{anytype:TBarX} := foo.f9;
  x11{anytype:TBarX} := foo.fa;
  x12{TODO: anytype:TNest.TBarX} := foo.fb;

  y1{anytype:TOuter} := nest.f1;
  y2{anytype:TBar} := nest.f2;
  y3{TODO: anytype:TNest.TNestBar} := nest.f3;
  y4{anytype:TBar} := nest.f4;
  y5{TODO: anytype:TNest.TNestBar} := nest.f5;
  y6{TODO: anytype:TNest.TNestBar} := nest.f6;
  y7{anytype:TRec.TRecType} := nest.f7;
  y8{anytype:TObj.TObjType} := nest.f8;
  y9{TODO: anytype:TNest.TBarX} := nest.f9;
  y11{anytype:TBarX} := nest.fa;
  y12{TODO: anytype:TNest.TBarX} := nest.fb;

  z1{anytype:TOuter} := f1;
  z2 := f2;
  //z3 := f3;
  z4{anytype:TBar} := f4;
  z5 := f5;
  z6{TODO: anytype:TNest.TNestBar} := f6;
  z7{anytype:TRec.TRecType} := f7;
  z8{anytype:TObj.TObjType} := f8;

end;

var
  foo: TFoo;
  nest: TFoo.TNest;

begin
  i{anytype:integer} := 1;
  x{anytype:TFoo.TBar} := a;
  y{anytype:TFoo.TNest.TNestBar} := c;
  z{anytype:TNest.TNotNestBar} := e;
  xx{anytype:TFoo.TBarX} := xa;
  xy{anytype:TFoo.TNest.TBarX} := xc;

  x1{anytype:TOuter} := foo.f1;
  x2{anytype:TFoo.TBar} := foo.f2;
  //x3 := foo.f3;
  x4{anytype:TFoo.TBar} := foo.f4;
  x5{anytype:TFoo.TNest.TNestBar} := foo.f5;
  x6{anytype:TFoo.TNest.TNestBar} := foo.f6;
  x7{anytype:TRec.TRecType} := foo.f7;
  x8{anytype:TObj.TObjType} := foo.f8;
  x9{anytype:TFoo.TBarX} := foo.f9;
  x11{anytype:TFoo.TBarX} := foo.fa;
  x12{anytype:TFoo.TNest.TBarX} := foo.fb;

  y1{anytype:TOuter} := nest.f1;
  y2{anytype:TFoo.TBar} := nest.f2;
  y3{anytype:TFoo.TNest.TNestBar} := nest.f3;
  y4{anytype:TFoo.TBar} := nest.f4;
  y5{anytype:TFoo.TNest.TNestBar} := nest.f5;
  y6{anytype:TFoo.TNest.TNestBar} := nest.f6;
  y7{anytype:TRec.TRecType} := nest.f7;
  y8{anytype:TObj.TObjType} := nest.f8;
  y9{anytype:TFoo.TNest.TBarX} := nest.f9;
  y11{anytype:TFoo.TBarX} := nest.fa;
  y12{anytype:TFoo.TNest.TBarX} := nest.fb;

  z1{anytype:TOuter} := f1;
  //z2 := f2;
  //z3 := f3;
  z4{anytype:TFoo.TBar} := f4;
  //z5 := f5;
  z6{anytype:TFoo.TNest.TNestBar} := f6;
  z7{anytype:TRec.TRecType} := f7;
  z8{anytype:TObj.TObjType} := f8;

  r{anytype:TRec.TRecType} := a1;
  o{anytype:TObj.TObjType} := b1;
end.

