unit u_specialize_inline;
{$mode ObjFPC}{$H+}
{$Interfaces CORBA}

(* unit for: program tspecialize_inline;
   Define some generics in a separate unit, to test gen-params resolving
*)

interface

type

  //TObject = integer;

  (* Generics for class *)

  generic TGenClassCommon<T> = class F:T; end;

  TBaseForGenClassA = class
  public type
    generic TGenMain1<A> = class public type TX = A; end; // not used
  end;

  generic TGenClassA<T> = class(TBaseForGenClassA)
  public
    FDataA: T;
    class function P1(a: T): T; virtual; abstract;
  public type
    TTypeA = T;
    TSelf = TGenClassA;
    TNestClassA = class
    public type
      TNestTypeA = T;
      TNestTypeA2 = TTypeA;
    public
      FNestDataA: T;
      FNestDataA2: TTypeA;
      FNestDataA3: TNestTypeA;
      FNestDataA4: TNestTypeA2;
    end;
  end;

  generic TGenClassB<B: class; T> = class(B)
  public
    FDataA: T;
    function P1(a: T): T; virtual; abstract;
  public type
    TTypeB1 = B;
    TTypeB2 = T;
    TSelf = TGenClassB;
    TNestClassB = class(B)
    public
      FNestDataB: T;
    public type
      TSelf = TNestClassB;
    end;
  //private type
    TGenMain1 = class public type TX = byte; end; // not used
  end;

  TGenParamWrapperClassC = class
  private type
    generic TGenClassC<T: class> = class
    public type
      TTypeC = T;
      TSelf = TGenClassC;
    public
      FDataC: T;
      FDataC2: TTypeC;
    end;
  end;

  generic TGEN_GenParamWrapperClassC<T> = class
  public
    F: T;
  public type
    TWrap = TGenParamWrapperClassC;
  end;

  generic TGenClassD<T: TGenParamWrapperClassC; T2: class> = class
  public type
    TTypeD = T.specialize TGenClassC<T2>.TTypeC;
  public
    FDataA: T.specialize TGenClassC<T2>.TTypeC;
    FDataA2: TTypeD;
    function P1(a: T.specialize TGenClassC<T2>.TTypeC): T.specialize TGenClassC<T2>.TTypeC; virtual; abstract;
    function P2(a: TTypeD): TTypeD; virtual; abstract;
    function PXD(a: TTypeD): TTypeD; virtual; abstract;
  end;

  // switch T and T2 // make sure they aren't mixed with params from other generics
  generic TGenClassE<T2: u_specialize_inline.specialize TGEN_GenParamWrapperClassC<byte>.TWrap; T: System.TObject> = class(T)
  public type
    TTypeD = T2.specialize TGenClassC<T>.TTypeC;
    TSelf = TGenClassE;
  public
    FDataA: T2.specialize TGenClassC<T>.TTypeC;
    FDataA2: TTypeD;
    function P1(a: T2.specialize TGenClassC<T>.TTypeC): T2.specialize TGenClassC<T>.TTypeC; virtual; abstract;
    function P2(a: TTypeD): TTypeD; virtual; abstract;
  end;

  generic TGenClassF<T: specialize TGEN_GenParamWrapperClassC<byte>.TWrap; T2: class> = class(T.specialize TGenClassC<T2>.TTypeC)
  public type
    TTypeD = T.specialize TGenClassC<T2>.TTypeC;
  public
    FDataA: T.specialize TGenClassC<T2>.TTypeC;
    FDataA2: TTypeD;
    function P1(a: T.specialize TGenClassC<T2>.TTypeC): T.specialize TGenClassC<T2>.TTypeC; virtual; abstract;
    function P2(a: TTypeD): TTypeD; virtual; abstract;
  end;

  generic TGenClassG<T: TObject; T2: class> = class(specialize TGenClassA<T>.TNestClassA)
  public type
    TTypeG = specialize TGenClassA<T2>.TNestClassA;
  public
    FDataA: specialize TGenClassA<T>.TNestClassA;
    FDataA2: TTypeG;
    function P1(a: specialize TGenClassA<T>.TNestClassA): specialize TGenClassA<T>.TNestClassA; virtual; abstract;
    function P2(a: TTypeG): TTypeG; virtual; abstract;
  end;


  (* Generics for incerface *)

  TI1 = interface end;

  generic TGenIFaceA<T> = interface
    function Foo(a: T): T;
  end;

  generic TGenIFaceB<B: TI1; T> = interface(B)
    function Bar(a: T): T;
  end;

  generic TGenIClassC<T> = class
  public type
    TIFaceC = interface
      function Bar(a: T): T;
    end;
  end;

  (* Generics for object *)

  TBaseObj = object
    FBaseData: word;
  end;

  TGenParamWrapperObjA = class
  public type
    TBaseType = TBaseObj;
  end;

  generic TGenObjA<T> = object
  public
    FDataA: T;
  public type
    TNestObjA = object
    public
      FNestDataA: T;
    end;
  end;

  generic TGenObjB<B: TGenParamWrapperObjA; T> = object(B.TBaseType)
  public
    FDataA: T;
  public type
    TNestObjB = object(B.TBaseType)
    public
      FNestDataA: T;
    end;
  end;

  (* Generics in wrapper class / qualified access *)

  TWrapper1 = class
  public type

    generic TGenClassA<T> = class
    public
      FDataA: T;
    public type
      TNestClassA = class
      public
        FNestDataA: T;
      end;
    end;

    generic TGenClassB<B: class; T> = class(B)
    public
      FDataA: T;
    public type
      TNestClassB = class(B)
      public
        FNestDataA: T;
      end;
    end;

  end;


implementation

end.

