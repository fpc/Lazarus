{
 Test all with:
     ./runtests --format=plain --suite=TTestAvgLvlTree

 Test specific with:
     ./runtests --format=plain --suite=TestAVLTreeAddsDeletes
     ./runtests --format=plain --suite=TestIndexedAVLTreeAddsDeletes
}
unit TestAvgLvlTree;

{$mode objfpc}{$H+}

{ $DEFINE VerboseTestSequence}

interface

uses
  Classes, SysUtils, fpcunit, testglobals, AvgLvlTree, LazLogger;

type
  { TTestAvgLvlTree }

  TTestAvgLvlTree = class(TTestCase)
  private
    fTreeClass: TAvgLvlTreeClass;
    procedure TestSequence(Args: array of const);
    procedure TestAVLTree;
  published
    procedure TestAVLTreeAddsDeletes;
    procedure TestIndexedAVLTreeAddsDeletes;
  end;

implementation

{ TTestAvgLvlTree }

procedure TTestAvgLvlTree.TestSequence(Args: array of const);
var
  Tree: TAvgLvlTree;
  i: Integer;
  Value: LongInt;
begin
  Tree:=fTreeClass.Create;
  //DebugLn(Tree.ReportAsString);
  Tree.ConsistencyCheck;

  for i:=Low(Args) to high(Args) do begin
    if Args[i].VType<>vtInteger then continue;
    Value:=Args[i].vinteger;
    if Value>0 then begin
      {$IFDEF VerboseTestSequence}
      DebugLn(['  add value ',Value]);
      {$ENDIF}
      Tree.Add({%H-}Pointer(Value));
    end else begin
      Value:=-Value;
      {$IFDEF VerboseTestSequence}
      debugln(['  remove value ',Value]);
      {$ENDIF}
      Tree.Remove({%H-}Pointer(Value));
    end;
    {$IFDEF VerboseTestSequence}
    DebugLn(Tree.ReportAsString);
    {$ENDIF}
    Tree.ConsistencyCheck;
  end;

  Tree.Clear;
  //DebugLn(Tree.ReportAsString);
  Tree.ConsistencyCheck;

  Tree.Free;
end;

procedure TTestAvgLvlTree.TestAVLTree;
begin
  // rotate left
  TestSequence([]);
  TestSequence([1]);
  TestSequence([1,2]);
  TestSequence([1,2,3]);
  TestSequence([1,2,3,4]);
  TestSequence([1,2,3,4,5]);
  TestSequence([1,2,3,4,5,6]);
  TestSequence([1,2,3,4,5,6,7,8,9,10]);

  // rotate right
  TestSequence([10,9,8,7,6,5,4,3,2,1]);

  // double rotate right, left
  TestSequence([5,7,6]);

  // double rotate left, right
  TestSequence([5,3,4]);

  // test deletes
  TestSequence([1,2,3,-1,-2,-3]);
  TestSequence([1,2,3,-1,-3,-2]);
  TestSequence([1,2,3,-2,-1,-3]);
  TestSequence([1,2,3,-2,-3,-1]);
  TestSequence([1,2,3,-3,-1,-2]);
  TestSequence([1,2,3,-3,-2,-1]);
end;

procedure TTestAvgLvlTree.TestAVLTreeAddsDeletes;
begin
  fTreeClass:=TAvgLvlTree;
  TestAVLTree;
end;

procedure TTestAvgLvlTree.TestIndexedAVLTreeAddsDeletes;
begin
  fTreeClass:=TIndexedAVLTree;
  TestAVLTree;
end;

initialization
  AddToLazUtilsTestSuite(TTestAvgLvlTree);

end.

