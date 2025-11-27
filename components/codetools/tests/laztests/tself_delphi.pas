program tself_delphi;
{$Mode Delphi}

type

  { TBase }

  TBase = class
    FBase: integer;
    class procedure ProcBase1;
    procedure ProcBase2;
    function ProcBase3: TBase;
  end;

  { TTest1 }

  TTest1 = class(TBase)
    FTest1: integer;
    class procedure Foo;
    procedure Foo2;
  end;

  { TTest2 }

  TTest2<T2> = class
    FTest2: integer;
    class procedure Foo;
    procedure Foo2(aArg: T2);
    function Foo3(aArg: T2): T2;
  end;

  { TTest2a }

  TTest2a<T2> = class(TBase)
    FTest2a: integer;
    class procedure Foo;
    procedure Foo2(aArg: T2);
    function Foo3(aArg: T2): T2;
  end;

  { TTest3 }

  TTest3<T2: TObject> = class
    FTest3: integer;
    class procedure Foo;
    procedure Foo2(aArg: T2);
    function Foo3(aArg: T2): T2;
  end;

  { TWrap1 }

  TWrap1 = class
  public
    FBar: integer;
  public type

    { TTest4 }

    TTest4<T2: TObject> = class
      FTest4: integer;
      class procedure Foo;
      procedure Foo2(aArg: T2);
      function Foo3(aArg: T2): T2;
    end;

  public
    FOuter: integer;
    class procedure NoGen;
    class procedure Foo<P>;
    class procedure Foo1<P>(a:P);
    procedure Foo2<P>(aArg: P);
  end;

  TTest5<T2: TObject> = class(TBase)
  public
    FBar: integer;
  public type

    { TInner }

    TInner = class
      FTest5: integer;
      class procedure Foo;
      procedure Foo2(aArg: T2);
      function Foo3(aArg: T2): T2;
    end;
  end;

var
  Bar: integer;

{ TBase }

class procedure TBase.ProcBase1;
begin
  // TODO: !FBase,!ProcBase2,!ProcBase3
         {completion:ProcBase1,InstanceSize,Create,Self,Bar}
  self.  {completion:ProcBase1,InstanceSize,Create}
    ProcBase1 {declaration:TBase.ProcBase1}
    ;
end;

procedure TBase.ProcBase2;
begin
         {completion:FBase,ProcBase1,ProcBase2,ProcBase3,Free,InstanceSize,Create,Self,Bar}
  self.  {completion:FBase,ProcBase1,ProcBase2,ProcBase3,Free,InstanceSize,Create}
    ProcBase1 {declaration:TBase.ProcBase1}
    ;
end;

function TBase.ProcBase3: TBase;
begin
         {completion:FBase,ProcBase1,ProcBase2,ProcBase3,Free,InstanceSize,Create,Self,Bar}
  self.  {completion:FBase,ProcBase1,ProcBase2,ProcBase3,Free,InstanceSize,Create}
    ProcBase1 {declaration:TBase.ProcBase1}
    ;
end;

{ TTest1 }

class procedure TTest1.Foo;
begin
  // TODO: !FBase,!FTest,!ProcBase2,!ProcBase3,!Foo2
         {completion:Foo,ProcBase1,InstanceSize,Create,Self,Bar}
  self.  {completion:Foo,ProcBase1,InstanceSize,Create}
    Foo  {declaration:TTest1.Foo}
    ;
end;

procedure TTest1.Foo2;
begin
         {completion:FTest1,Foo,Foo2,FBase,ProcBase1,ProcBase2,ProcBase3,Free,InstanceSize,Create,Self,Bar}
  self.  {completion:FTest1,Foo,Foo2,FBase,ProcBase1,ProcBase2,ProcBase3,Free,InstanceSize,Create}
    Foo  {declaration:TTest1.Foo}
    ;
end;

{ TTest2 }

class procedure TTest2<T2>.Foo;
begin
  // TODO: !FTest2,!Foo2,!Foo3
         {completion:Foo,!FBase,!ProcBase1,!ProcBase2,!ProcBase3,InstanceSize,Create,Self,Bar}
  self.  {completion:Foo,!FBase,!ProcBase1,!ProcBase2,!ProcBase3,InstanceSize,Create}
    Foo  {declaration:TTest2.Foo}
    ;

  Foo  {declaration:TTest2.Foo}
  ;
end;

procedure TTest2<T2>.Foo2(aArg: T2);
begin
         {completion:FTest2,Foo,Foo2,Foo3,!FBase,!ProcBase1,!ProcBase2,!ProcBase3,Free,InstanceSize,Create,Self,Bar}
  self.  {completion:FTest2,Foo,Foo2,Foo3,!FBase,!ProcBase1,!ProcBase2,!ProcBase3,Free,InstanceSize,Create}
    Foo  {declaration:TTest2.Foo}
    ;

  Foo  {declaration:TTest2.Foo}
  ;
end;

function TTest2<T2>.Foo3(aArg: T2): T2;
begin
         {completion:FTest2,Foo,Foo2,Foo3,!FBase,!ProcBase1,!ProcBase2,!ProcBase3,Free,InstanceSize,Create,Self,Bar}
  self.  {completion:FTest2,Foo,Foo2,Foo3,!FBase,!ProcBase1,!ProcBase2,!ProcBase3,Free,InstanceSize,Create}
    Foo  {declaration:TTest2.Foo}
    ;

  Foo  {declaration:TTest2.Foo}
  ;
end;

{ TTest2a }

class procedure TTest2a<T2>.Foo;
begin
  // TODO: !FTest2a,!Foo2,!Foo3,!FBase,!ProcBase2,!ProcBase3
         {completion:Foo,Foo2,Foo3,ProcBase1,InstanceSize,Create,Self,Bar}
  self.  {completion:Foo,Foo2,Foo3,ProcBase1,InstanceSize,Create}
    Foo  {declaration:TTest2a.Foo}
    ;

  Foo  {declaration:TTest2a.Foo}
  ;
end;

procedure TTest2a<T2>.Foo2(aArg: T2);
begin
         {completion:FTest2a,Foo,Foo2,Foo3,FBase,ProcBase1,ProcBase2,ProcBase3,Free,InstanceSize,Create,Self,Bar}
  self.  {completion:FTest2a,Foo,Foo2,Foo3,FBase,ProcBase1,ProcBase2,ProcBase3,Free,InstanceSize,Create}
    Foo  {declaration:TTest2a.Foo}
    ;

  Foo  {declaration:TTest2a.Foo}
  ;
end;

function TTest2a<T2>.Foo3(aArg: T2): T2;
begin
         {completion:FTest2a,Foo,Foo2,Foo3,FBase,ProcBase1,ProcBase2,ProcBase3,Free,InstanceSize,Create,Self,Bar}
  self.  {completion:FTest2a,Foo,Foo2,Foo3,FBase,ProcBase1,ProcBase2,ProcBase3,Free,InstanceSize,Create}
    Foo  {declaration:TTest2a.Foo}
    ;

  Foo  {declaration:TTest2a.Foo}
  ;
end;

{ TTest3 }

class procedure TTest3<T2>.Foo;
begin
  // TODO: !FTest3,!Foo2,!Foo33
         {completion:Foo,!FBase,!ProcBase1,!ProcBase2,!ProcBase3,InstanceSize,Create,Self,Bar}
  self.  {completion:Foo,!FBase,!ProcBase1,!ProcBase2,!ProcBase3,InstanceSize,Create}
    Foo  {declaration:TTest3.Foo}
    ;

  Foo  {declaration:TTest3.Foo}
  ;
end;

procedure TTest3<T2>.Foo2(aArg: T2);
begin
         {completion:FTest3,Foo,Foo2,Foo3,!FBase,!ProcBase1,!ProcBase2,!ProcBase3,Free,InstanceSize,Create,Self,Bar}
  self.  {completion:FTest3,Foo,Foo2,Foo3,!FBase,!ProcBase1,!ProcBase2,!ProcBase3,Free,InstanceSize,Create}
    Foo  {declaration:TTest3.Foo}
    ;

  Foo  {declaration:TTest3.Foo}
  ;
end;

function TTest3<T2>.Foo3(aArg: T2): T2;
begin
         {completion:FTest3,Foo,Foo2,Foo3,!FBase,!ProcBase1,!ProcBase2,!ProcBase3,Free,InstanceSize,Create,Self,Bar}
  self.  {completion:FTest3,Foo,Foo2,Foo3,!FBase,!ProcBase1,!ProcBase2,!ProcBase3,Free,InstanceSize,Create}
    Foo  {declaration:TTest3.Foo}
    ;

  Foo  {declaration:TTest3.Foo}
  ;
end;

{ TWrap1.TTest4 }

class procedure TWrap1.TTest4<T2>.Foo;
begin
  // TODO: !Foo2,!Foo3,!FTest4,!FBar,!FOuter
         { TODO: completion:Foo,InstanceSize,Create,Self,Bar}
  self.  { TODO: completion:Foo,InstanceSize,Create}
    Foo  { TODO: declaration:TWrap1.TTest4.Foo}
    ;

  Foo  { TODO: declaration:TWrap1.TTest4.Foo}
  ;
end;

procedure TWrap1.TTest4<T2>.Foo2(aArg: T2);
begin
         { TODO: completion:Foo,Foo2,Foo3,FTest4,Free,InstanceSize,Create,Self,Bar}
  self.  { TODO: completion:Foo,Foo2,Foo3,FTest4,Free,InstanceSize,Create}
    Foo  { TODO: declaration:TWrap1.TTest4.Foo}
    ;

  Foo  { TODO: declaration:TWrap1.TTest4.Foo}
  ;
end;

function TWrap1.TTest4<T2>.Foo3(aArg: T2): T2;
begin
         { TODO: completion:Foo,Foo2,Foo3,FTest4,Free,InstanceSize,Create,Self,Bar}
  self.  { TODO: completion:Foo,Foo2,Foo3,FTest4,Free,InstanceSize,Create}
    Foo  { TODO: declaration:TWrap1.TTest4.Foo}
    ;

  Foo  { TODO: declaration:TWrap1.TTest4.Foo}
  ;
end;

{ TWrap1 }

class procedure TWrap1.NoGen;
begin
  // TODO: !Foo2,!FBar,!FOuter,!Foo2
         {completion:Foo,Foo1,NoGen,InstanceSize,Create,Self,Bar}
  self.  {completion:Foo,Foo1,NoGen,InstanceSize,Create}
    NoGen  {declaration:TWrap1.NoGen}
    ;

  NoGen  {declaration:TWrap1.NoGen}
  ;
end;

class procedure TWrap1.Foo<P>;
begin
  // TODO: !Foo2,!FBar,!FOuter,!Foo2
         {completion:Foo,Foo1,NoGen,InstanceSize,Create,Self,Bar}
  self.  {completion:Foo,Foo1,NoGen,InstanceSize,Create}
    NoGen  {declaration:TWrap1.NoGen}
    ;

  NoGen  {declaration:TWrap1.NoGen}
  ;
end;

class procedure TWrap1.Foo1<P>(a: P);
begin
  // TODO: !Foo2,!FBar,!FOuter,!Foo2
         {completion:Foo,Foo1,NoGen,InstanceSize,Create,Self,Bar}
  self.  {completion:Foo,Foo1,NoGen,InstanceSize,Create}
    NoGen  {declaration:TWrap1.NoGen}
    ;

  NoGen  {declaration:TWrap1.NoGen}
  ;
end;

procedure TWrap1.Foo2<P>(aArg: P);
begin
         {completion:Foo,Foo1,Foo2,FBar,FOuter,NoGen,Free,InstanceSize,Create,Self,Bar}
  self.  {completion:Foo,Foo1,Foo2,FBar,FOuter,NoGen,Free,InstanceSize,Create}
    NoGen  {declaration:TWrap1.NoGen}
    ;

  NoGen  {declaration:TWrap1.NoGen}
  ;
end;

{ TTest5.TInner }

class procedure TTest5<T2>.TInner.Foo;
begin
  // TODO: !Foo2,!Foo3,!FTest5 , !Fbar
         {completion:Foo,InstanceSize,Create,Self,Bar}
  self.  {completion:Foo,!Fbar,InstanceSize,Create}
    Foo  {declaration:TTest5.TInner.Foo}
    ;

  Foo  {declaration:TTest5.TInner.Foo}
  ;
end;

procedure TTest5<T2>.TInner.Foo2(aArg: T2);
begin
  // TODO: !Fbar
         {completion:Foo,Foo2,Foo3,FTest5,Free,InstanceSize,Create,Self,Bar}
  self.  {completion:Foo,Foo2,Foo3,FTest5,!Fbar,Free,InstanceSize,Create}
    Foo  {declaration:TTest5.TInner.Foo}
    ;

  Foo  {declaration:TTest5.TInner.Foo}
  ;
end;

function TTest5<T2>.TInner.Foo3(aArg: T2): T2;
begin
  // TODO: !Fbar
         {completion:Foo,Foo2,Foo3,FTest5,Free,InstanceSize,Create,Self,Bar}
  self.  {completion:Foo,Foo2,Foo3,FTest5,!Fbar,Free,InstanceSize,Create}
    Foo  {declaration:TTest5.TInner.Foo}
    ;

  Foo  {declaration:TTest5.TInner.Foo}
  ;
end;

begin
end.

