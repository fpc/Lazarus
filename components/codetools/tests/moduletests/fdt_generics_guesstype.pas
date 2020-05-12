{
  ./testcodetools --format=plain --suite=TestFindDeclaration_Generics_GuessType
}
program fdt_generics_guesstype;

{$mode objfpc}{$H+}

type

  { TEnumerator }

  generic TEnumerator<T> = class abstract
  public
    property Current: T read DoGetCurrent;
  end;

  { TEnumerable }

  generic TEnumerable<T> = class abstract
  public
    function GetEnumerator: specialize TEnumerator<T>; virtual; abstract;
  end;

  { TEnumerableWithPointers }

  generic TEnumerableWithPointers<T> = class(specialize TEnumerable<T>)
  end;

  { TCustomList }

  generic TCustomList<T> = class abstract(specialize TEnumerableWithPointers<T>)
  end;

  { TCustomListEnumerator }

  generic TCustomListEnumerator<T> = class abstract(specialize TEnumerator<T>)
  protected
    function GetCurrent: T; virtual; abstract;
  end;

  { TCustomListWithPointers }

  generic TCustomListWithPointers<T> = class(specialize TCustomList<T>)
  end;

  { TListP1 }

  generic TListP1<B, B2, B3> = class(specialize TCustomListWithPointers<B>)
  public
    type
      TEnumerator = class(specialize TCustomListEnumerator<B>);
    function GetEnumerator: TEnumerator; virtual; abstract;
  end;

  { TListP2 }

  generic TListP2<B2, B, B3> = class(specialize TCustomListWithPointers<B>)
  public
    type
      TEnumerator = class(specialize TCustomListEnumerator<B>);
    function GetEnumerator: TEnumerator; virtual; abstract;
  end;

  { TListP3 }

  generic TListP3<B2, B3, B> = class(specialize TCustomListWithPointers<B>)
  public
    type
      TEnumerator = class(specialize TCustomListEnumerator<B>);
    function GetEnumerator: TEnumerator; virtual; abstract;
  end;

  generic TObjectListP1_1<A: class; A2, A3> = class(specialize TListP1<A, A2, A3>)
  end;
  generic TObjectListP1_2<A: class; A2, A3> = class(specialize TListP2<A2, A, A3>)
  end;
  generic TObjectListP1_3<A: class; A2, A3> = class(specialize TListP3<A2, A3, A>)
  end;

  generic TObjectListP2_1<A2; A: class; A3> = class(specialize TListP1<A, A2, A3>)
  end;
  generic TObjectListP2_2<A2; A: class; A3> = class(specialize TListP2<A2, A, A3>)
  end;
  generic TObjectListP2_3<A2; A: class; A3> = class(specialize TListP3<A2, A3, A>)
  end;

  generic TObjectListP3_1<A2, A3; A: class> = class(specialize TListP1<A, A2, A3>)
  end;
  generic TObjectListP3_2<A2, A3; A: class> = class(specialize TListP2<A2, A, A3>)
  end;
  generic TObjectListP3_3<A2, A3; A: class> = class(specialize TListP3<A2, A3, A>)
  end;

  TObj = class
  end;
  TObj1 = class
  end;
  TObj2 = class
  end;

  TOL_P1_1 = specialize TObjectListP1_1<TObj, TObj1, TObj2>;
  TOL_P1_2 = specialize TObjectListP1_2<TObj, TObj1, TObj2>;
  TOL_P1_3 = specialize TObjectListP1_3<TObj, TObj1, TObj2>;
  TOL_P2_1 = specialize TObjectListP2_1<TObj1, TObj, TObj2>;
  TOL_P2_2 = specialize TObjectListP2_2<TObj1, TObj, TObj2>;
  TOL_P2_3 = specialize TObjectListP2_3<TObj1, TObj, TObj2>;
  TOL_P3_1 = specialize TObjectListP3_1<TObj1, TObj2, TObj>;
  TOL_P3_2 = specialize TObjectListP3_2<TObj1, TObj2, TObj>;
  TOL_P3_3 = specialize TObjectListP3_3<TObj1, TObj2, TObj>;
  TOL2 = class(specialize TObjectListP1_1<TObj, TObj1, TObj2>);
  TOL3 = TOL2;
  TOL4 = class(TOL2);

var
  OL_P1_1: TOL_P1_1;
  OL_P1_2: TOL_P1_2;
  OL_P1_3: TOL_P2_3;
  OL_P2_1: TOL_P2_1;
  OL_P2_2: TOL_P2_2;
  OL_P2_3: TOL_P2_3;
  OL_P3_1: TOL_P3_1;
  OL_P3_2: TOL_P3_2;
  OL_P3_3: TOL_P3_3;
  OL2: TOL2;
  OL3: TOL3;
  OL4: TOL4;

begin
  for o_p1_1{guesstype:TObj} in OL_P1_1 do ;
  for o_p1_2{guesstype:TObj} in OL_P1_2 do ;
  for o_p1_3{guesstype:TObj} in OL_P1_3 do ;
  for o_p2_1{guesstype:TObj} in OL_P2_1 do ;
  for o_p2_2{guesstype:TObj} in OL_P2_2 do ;
  for o_p2_3{guesstype:TObj} in OL_P2_3 do ;
  for o_p3_1{guesstype:TObj} in OL_P3_1 do ;
  for o_p3_2{guesstype:TObj} in OL_P3_2 do ;
  for o_p3_3{guesstype:TObj} in OL_P3_3 do ;
  for o2{guesstype:TObj} in OL2 do ;
  for o3{guesstype:TObj} in OL3 do ;
  for o4{guesstype:TObj} in OL4 do ;
end.


