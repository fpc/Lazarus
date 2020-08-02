{
 Test with:
   ./testcodetools --format=plain --suite=TTestRefactoring
   ./testcodetools --format=plain --suite=TestExplodeWith
}
unit TestRefactoring;

{$i runtestscodetools.inc}

interface

uses
  Classes, SysUtils, CodeToolManager, CodeCache, CodeTree,
  BasicCodeTools, LazLogger, LazFileUtils, fpcunit, testregistry,
  TestFinddeclaration;

const
  ExplodeWithMarker = 'explodewith:';
type

  { TTestRefactoring }

  TTestRefactoring = class(TTestCase)
  private
  published
    procedure TestExplodeWith;
  end;

implementation

{ TTestRefactoring }

procedure TTestRefactoring.TestExplodeWith;
type
  TWithBlock = record
    CodeXYPos: TCodeXYPosition;
    WithExpr: string;
    StatementStartPos: integer;
    StatementEndPos: integer;
  end;
  PWithBlock = ^TWithBlock;
var
  Code: TCodeBuffer;
  Tool: TCodeTool;
  Node, StatementNode: TCodeTreeNode;
  CodeXYPos: TCodeXYPosition;
  ListOfWiths: array of TWithBlock;
  i, NewStartPos, NewEndPos, p, CommentStartPos, CommentEndPos: Integer;
  Filename, OldSource, Src, ID, ExpectedInsertion: String;
  aWith: PWithBlock;
begin
  Filename:=ExpandFileNameUTF8('moduletests/rt_explodewith.pas');
  Code:=CodeToolBoss.LoadFile(Filename,true,false);
  AssertEquals('Load file error: '+Filename,true,Code<>nil);
  if not CodeToolBoss.Explore(Code,Tool,true) then
    AssertEquals('Parse error: ','',CodeToolBoss.ErrorMessage);
  // collect all With-Blocks
  Node:=Tool.Tree.Root;
  SetLength(ListOfWiths{%H-},0);
  while Node<>nil do begin
    if Node.Desc=ctnWithVariable then begin
      Tool.CleanPosToCaret(Node.StartPos,CodeXYPos);
      StatementNode:=Tool.FindWithBlockStatement(Node);
      if StatementNode<>nil then begin
        SetLength(ListOfWiths,length(ListOfWiths)+1);
        aWith:=@ListOfWiths[High(ListOfWiths)];
        aWith^.CodeXYPos:=CodeXYPos;
        aWith^.WithExpr:=Tool.ExtractWithBlockExpression(Node,[]);
        aWith^.StatementStartPos:=FindPrevNonSpace(Code.Source,StatementNode.StartPos);
        aWith^.StatementEndPos:=StatementNode.EndPos;
      end;
    end;
    Node:=Node.Next;
  end;

  for i:=0 to High(ListOfWiths) do begin
    aWith:=@ListOfWiths[i];
    CodeXYPos:=aWith^.CodeXYPos;
    //debugln(['TTestRefactoring.TestExplodeWith ',dbgs(CodeXYPos)]);
    OldSource:=Code.Source;
    try
      if CodeToolBoss.RemoveWithBlock(Code,CodeXYPos.X,CodeXYPos.Y) then begin
        // success
        // => check changes
        // get new bounds
        NewStartPos:=aWith^.StatementStartPos;
        NewEndPos:=aWith^.StatementEndPos;
        Code.AdjustPosition(NewStartPos);
        Code.AdjustPosition(NewEndPos);
        if (NewStartPos<1) or (NewStartPos>Code.SourceLength)
        or (NewEndPos<1) or (NewEndPos>Code.SourceLength)
        or (NewEndPos<NewStartPos)
        then begin
          debugln(['TTestRefactoring.TestExplodeWith WrongCode: ']);
          debugln(Code.Source);
          Fail('CodeToolBoss.RemoveWithBlock failed at '+dbgs(CodeXYPos));
        end;
        // check each marker
        Src:=Code.Source;
        //debugln(['TTestRefactoring.TestExplodeWith NewBlock=',copy(Src,NewStartPos,NewEndPos-NewStartPos)]);
        p:=NewStartPos;
        repeat
          CommentStartPos:=FindNextComment(Src,p,NewEndPos);
          if CommentStartPos>=NewEndPos then break;
          p:=CommentStartPos;
          CommentEndPos:=FindCommentEnd(Src,CommentStartPos,Tool.Scanner.NestedComments);
          if Src[p]='{' then begin
            inc(p);
            if copy(Src,p,length(ExplodeWithMarker))=ExplodeWithMarker then begin
              inc(p,length(ExplodeWithMarker));
              ID:=copy(Src,p,CommentEndPos-p-1);
              if ID=aWith^.WithExpr then begin
                // this marker expects an insertion
                ExpectedInsertion:=Id+'.';
                if copy(Src,CommentEndPos,length(ExpectedInsertion))<>ExpectedInsertion
                then begin
                  Fail('CodeToolBoss.RemoveWithBlock failed at '+dbgs(CodeXYPos)
                    +': Expected insertion "'+ExpectedInsertion+'"'
                    +' at '+Code.AbsoluteToLineColStr(CommentEndPos)
                    +', but found "'+dbgstr(Src,CommentStartPos,20)+'"');
                end;
              end;
            end;
          end;
          p:=CommentEndPos;
        until false;


      end else begin
        Fail('CodeToolBoss.RemoveWithBlock failed at '+dbgs(CodeXYPos)+': '+CodeToolBoss.ErrorMessage);
      end;
    finally
      Code.Source:=OldSource;
    end;
  end;
end;

initialization
  RegisterTests([TTestRefactoring]);
end.

