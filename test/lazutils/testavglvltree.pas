{
 Test all with:
     ./runtests --format=plain --suite=TTest_AvgLvlTree
     ./runtests --format=plain --suite=TTest_AVLTree

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
  Classes, SysUtils, fpcunit, testglobals, LazLogger,
  AVL_Tree, // the unit from FPC
  // Laz_AVL_Tree, the unit copied from FPC when compiling Lazarus for older compilers
  AvgLvlTree // unit from LazUtils, an extended version
  ;

type
  { TTest_AvgLvlTree - the LazUtils extensions }

  TTest_AvgLvlTree = class(TTestCase)
  private
    fTreeClass: AvgLvlTree.TAvgLvlTreeClass;
    function CreateTree(Args: array of const): AvgLvlTree.TAvgLvlTree;
    procedure TestSequence(Args: array of const);
    procedure TestAscendingSequence(InitArgs: array of const; AscSeq: array of const);
    procedure TestAvgLvlTree;
  published
    procedure TestAvgLvlTreeAddsDeletes;
    procedure TestIndexedAVLTreeAddsDeletes;
  end;

  {$IF FPC_FULLVERSION<30101}
  TAVLTreeClass = class of TAVLTree;
  {$ENDIF}

  { TTest_AVLTree - the FPC unit}

  TTest_AVLTree = class(TTestCase)
  private
    fTreeClass: TAVLTreeClass;
    function CreateTree(Args: array of const): TAVLTree;
    procedure TestSequence(Args: array of const);
    {$IF FPC_FULLVERSION>=30101}
    procedure TestAscendingSequence(InitArgs: array of const; AscSeq: array of const);
    {$ENDIF}
    procedure TestAVLTree;
  published
    procedure TestAVLTreeAddsDeletes;
  end;

implementation

{ TTest_AVLTree }

function TTest_AVLTree.CreateTree(Args: array of const): TAVLTree;
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

procedure TTest_AVLTree.TestSequence(Args: array of const);
var
  Tree: AVL_Tree.TAVLTree;
begin
  Tree:=CreateTree(Args);
  Tree.Clear;
  //DebugLn(Tree.ReportAsString);
  Tree.ConsistencyCheck;
  Tree.Free;
end;

{$IF FPC_FULLVERSION>=30101}
procedure TTest_AVLTree.TestAscendingSequence(InitArgs: array of const;
  AscSeq: array of const);
var
  Tree: AVL_Tree.TAVLTree;
  LastAdded, Succesor: AVL_Tree.TAVLTreeNode;
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
{$ENDIF}

procedure TTest_AVLTree.TestAVLTree;
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

  {$IF FPC_FULLVERSION>=30101}
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
  {$ENDIF}
end;

procedure TTest_AVLTree.TestAVLTreeAddsDeletes;
begin
  fTreeClass:=AVL_Tree.TAVLTree;
  TestAVLTree;
end;

{ TTest_AvgLvlTree }

function TTest_AvgLvlTree.CreateTree(Args: array of const): AvgLvlTree.TAvgLvlTree;
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

procedure TTest_AvgLvlTree.TestSequence(Args: array of const);
var
  Tree: AvgLvlTree.TAvgLvlTree;
begin
  Tree:=CreateTree(Args);
  Tree.Clear;
  //DebugLn(Tree.ReportAsString);
  Tree.ConsistencyCheck;
  Tree.Free;
end;

procedure TTest_AvgLvlTree.TestAscendingSequence(InitArgs: array of const;
  AscSeq: array of const);
var
  Tree: AvgLvlTree.TAvgLvlTree;
  LastAdded, Succesor: AvgLvlTree.TAvgLvlTreeNode;
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

procedure TTest_AvgLvlTree.TestAvgLvlTree;
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

procedure TTest_AvgLvlTree.TestAvgLvlTreeAddsDeletes;
begin
  fTreeClass:=AvgLvlTree.TAvgLvlTree;
  TestAvgLvlTree;
end;

procedure TTest_AvgLvlTree.TestIndexedAVLTreeAddsDeletes;
begin
  fTreeClass:=AvgLvlTree.TIndexedAVLTree;
  TestAvgLvlTree;
end;

initialization
  AddToLazUtilsTestSuite(TTest_AvgLvlTree);
  AddToLazUtilsTestSuite(TTest_AVLTree);

end.

