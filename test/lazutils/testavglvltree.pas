{
 Test all with:
     ./runtests --format=plain --suite=TTestAvgLvlTree

 Test specific with:
     ./runtests --format=plain --suite=TestAVLTreeAddsDeletes
}
unit TestAvgLvlTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testglobals, AvgLvlTree, LazLogger;

type
  { TTestAvgLvlTree }

  TTestAvgLvlTree = class(TTestCase)
  private
    procedure TestSequence(Args: array of const);
  published
    procedure TestAVLTreeAddsDeletes;
  end;

implementation

{ TTestAvgLvlTree }

procedure TTestAvgLvlTree.TestSequence(Args: array of const);
{ $DEFINE VerboseTestSequence}
var
  Tree: TAvgLvlTree;
  i: Integer;
  Value: LongInt;
begin
  Tree:=TAvgLvlTree.Create;
  //writeln(Tree.ReportAsString);
  Tree.ConsistencyCheck;

  for i:=Low(Args) to high(Args) do begin
    if Args[i].VType<>vtInteger then continue;
    Value:=Args[i].vinteger;
    if Value>0 then begin
      {$IFDEF VerboseTestSequence}
      DebugLn(['  add value ',Value]);
      {$ENDIF}
      Tree.Add(Pointer(Value));
    end else begin
      {$IFDEF VerboseTestSequence}
      debugln(['  remove value ',Value]);
      {$ENDIF}
      Tree.Remove(Pointer(Value));
    end;
    {$IFDEF VerboseTestSequence}
    DebugLn(Tree.ReportAsString);
    {$ENDIF}
    Tree.ConsistencyCheck;
  end;

  Tree.Clear;
  //writeln(Tree.ReportAsString);
  Tree.ConsistencyCheck;

  Tree.Free;
end;

procedure TTestAvgLvlTree.TestAVLTreeAddsDeletes;
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
end;

initialization
  AddToLazUtilsTestSuite(TTestAvgLvlTree);

end.

