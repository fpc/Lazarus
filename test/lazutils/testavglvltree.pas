{
 Test all with:
     ./runtests --format=plain --suite=TTestAvgLvlTree
     ./runtests --format=plain --suite=TTestAVLTree

 Test specific with:
     ./runtests --format=plain --suite=TestAvgLvlTreeAddsDeletes
     ./runtests --format=plain --suite=TestIndexedAVLTreeAddsDeletes
     ./runtests --format=plain --suite=TestAVLTreeAddsDeletes
}
unit TestAvgLvlTree;

{$mode objfpc}{$H+}

{ $DEFINE VerboseTestSequence}

interface

uses
  Classes, SysUtils, AVL_Tree, fpcunit, testglobals, AvgLvlTree, LazLogger;

type
  { TTestAvgLvlTree }

  TTestAvgLvlTree = class(TTestCase)
  private
    fTreeClass: TAvgLvlTreeClass;
    function CreateTree(Args: array of const): TAvgLvlTree;
    procedure TestSequence(Args: array of const);
    procedure TestAscendingSequence(InitArgs: array of const; AscSeq: array of const);
    procedure TestAvgLvlTree;
  published
    procedure TestAvgLvlTreeAddsDeletes;
    procedure TestIndexedAVLTreeAddsDeletes;
  end;

  { TTestAVLTree }

  TTestAVLTree = class(TTestCase)
  private
    fTreeClass: TAVLTreeClass;
    function CreateTree(Args: array of const): TAVLTree;
    procedure TestSequence(Args: array of const);
    procedure TestAscendingSequence(InitArgs: array of const; AscSeq: array of const);
    procedure TestAVLTree;
  published
    procedure TestAVLTreeAddsDeletes;
  end;

implementation

{ TTestAVLTree }

function TTestAVLTree.CreateTree(Args: array of const): TAVLTree;
var
  i: Integer;
  Value: LongInt;
begin
  Result:=fTreeClass.Create;
  //DebugLn(Result.ReportAsString);
  Result.ConsistencyCheck;

  for i:=Low(Args) to high(Args) do begin
    if Args[i].VType<>vtInteger then continue;
    Value:=Args[i].vinteger;
    if Value>0 then begin
      {$IFDEF VerboseTestSequence}
      DebugLn(['  add value ',Value]);
      {$ENDIF}
      Result.Add({%H-}Pointer(Value));
    end else begin
      Value:=-Value;
      {$IFDEF VerboseTestSequence}
      debugln(['  remove value ',Value]);
      {$ENDIF}
      Result.Remove({%H-}Pointer(Value));
    end;
    {$IFDEF VerboseTestSequence}
    DebugLn(Result.ReportAsString);
    {$ENDIF}
    Result.ConsistencyCheck;
  end;
end;

procedure TTestAVLTree.TestSequence(Args: array of const);
var
  Tree: TAVLTree;
begin
  Tree:=CreateTree(Args);
  Tree.Clear;
  //DebugLn(Tree.ReportAsString);
  Tree.ConsistencyCheck;
  Tree.Free;
end;

procedure TTestAVLTree.TestAscendingSequence(InitArgs: array of const;
  AscSeq: array of const);
var
  Tree: TAVLTree;
  LastAdded, Succesor: TAVLTreeNode;
  i: Integer;
  Value: LongInt;
begin
  Tree:=CreateTree(InitArgs);

  LastAdded:=nil;
  Succesor:=nil;
  for i:=Low(AscSeq) to high(AscSeq) do begin
    if AscSeq[i].VType<>vtInteger then continue;
    Value:=AscSeq[i].vinteger;
    {$IFDEF VerboseTestSequence}
    DebugLn(['  add ascending value ',Value]);
    {$ENDIF}
    LastAdded:=Tree.AddAscendingSequence({%H-}Pointer(Value),LastAdded,Succesor);
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

procedure TTestAVLTree.TestAVLTree;
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

  // test AddAscendingSequence
  TestAscendingSequence([],[1]);
  TestAscendingSequence([],[1,2]);
  TestAscendingSequence([],[1,2,3]);
  TestAscendingSequence([],[2,1]);
  TestAscendingSequence([],[3,2,1]);
  TestAscendingSequence([1],[2,3,4,5]);
  TestAscendingSequence([2],[1,3,4,5]);
  TestAscendingSequence([3],[1,2,4,5,6]);
  TestAscendingSequence([3,4],[1,2,5,6,7]);
end;

procedure TTestAVLTree.TestAVLTreeAddsDeletes;
begin
  fTreeClass:=TAVLTree;
  TestAVLTree;
end;

{ TTestAvgLvlTree }

function TTestAvgLvlTree.CreateTree(Args: array of const): TAvgLvlTree;
var
  i: Integer;
  Value: LongInt;
begin
  Result:=fTreeClass.Create;
  //DebugLn(Result.ReportAsString);
  Result.ConsistencyCheck;

  for i:=Low(Args) to high(Args) do begin
    if Args[i].VType<>vtInteger then continue;
    Value:=Args[i].vinteger;
    if Value>0 then begin
      {$IFDEF VerboseTestSequence}
      DebugLn(['  add value ',Value]);
      {$ENDIF}
      Result.Add({%H-}Pointer(Value));
    end else begin
      Value:=-Value;
      {$IFDEF VerboseTestSequence}
      debugln(['  remove value ',Value]);
      {$ENDIF}
      Result.Remove({%H-}Pointer(Value));
    end;
    {$IFDEF VerboseTestSequence}
    DebugLn(Result.ReportAsString);
    {$ENDIF}
    Result.ConsistencyCheck;
  end;
end;

procedure TTestAvgLvlTree.TestSequence(Args: array of const);
var
  Tree: TAvgLvlTree;
begin
  Tree:=CreateTree(Args);
  Tree.Clear;
  //DebugLn(Tree.ReportAsString);
  Tree.ConsistencyCheck;
  Tree.Free;
end;

procedure TTestAvgLvlTree.TestAscendingSequence(InitArgs: array of const;
  AscSeq: array of const);
var
  Tree: TAvgLvlTree;
  LastAdded, Succesor: TAvgLvlTreeNode;
  i: Integer;
  Value: LongInt;
begin
  Tree:=CreateTree(InitArgs);

  LastAdded:=nil;
  Succesor:=nil;
  for i:=Low(AscSeq) to high(AscSeq) do begin
    if AscSeq[i].VType<>vtInteger then continue;
    Value:=AscSeq[i].vinteger;
    {$IFDEF VerboseTestSequence}
    DebugLn(['  add ascending value ',Value]);
    {$ENDIF}
    LastAdded:=Tree.AddAscendingSequence({%H-}Pointer(Value),LastAdded,Succesor);
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

procedure TTestAvgLvlTree.TestAvgLvlTree;
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

  // test AddAscendingSequence
  TestAscendingSequence([],[1]);
  TestAscendingSequence([],[1,2]);
  TestAscendingSequence([],[1,2,3]);
  TestAscendingSequence([],[2,1]);
  TestAscendingSequence([],[3,2,1]);
  TestAscendingSequence([1],[2,3,4,5]);
  TestAscendingSequence([2],[1,3,4,5]);
  TestAscendingSequence([3],[1,2,4,5,6]);
  TestAscendingSequence([3,4],[1,2,5,6,7]);
end;

procedure TTestAvgLvlTree.TestAvgLvlTreeAddsDeletes;
begin
  fTreeClass:=TAvgLvlTree;
  TestAvgLvlTree;
end;

procedure TTestAvgLvlTree.TestIndexedAVLTreeAddsDeletes;
begin
  fTreeClass:=TIndexedAVLTree;
  TestAvgLvlTree;
end;

initialization
  AddToLazUtilsTestSuite(TTestAvgLvlTree);
  AddToLazUtilsTestSuite(TTestAVLTree);

end.

