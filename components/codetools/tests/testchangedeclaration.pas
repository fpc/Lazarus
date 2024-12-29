unit TestChangeDeclaration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LazLogger, LazFileUtils, fpcunit, testregistry, AVL_Tree,
  CodeToolManager, CodeCache, PascalParserTool, BasicCodeTools, CTUnitGraph,
  TestFinddeclaration, TestStdCodetools;

type

  { TTestChangeDeclaration }

  TTestChangeDeclaration = class(TCustomTestCTStdCodetools)
  protected
    FDefFilename: string;
    procedure SetUp; override;
    procedure TestRenameIdentifier(DeclCode: TCodeBuffer; SearchInCodeBufs: TFPList); overload;
    procedure TestRenameIdentifier(DeclCode: TCodeBuffer; Module1: TCodeBuffer = nil); overload;
  published
    procedure TestCTAddProcedureModifier;
    procedure TestCTRenameIdentifier_MultiInLine;
    procedure TestCTRenameIdentifier_MultiInLine_Amp;
  end;

implementation

procedure TTestChangeDeclaration.SetUp;
begin
  inherited SetUp;
  FDefFilename:='TestChangeDeclaration.pas';
end;

procedure TTestChangeDeclaration.TestRenameIdentifier(DeclCode: TCodeBuffer;
  SearchInCodeBufs: TFPList);
var
  p, DeclPos, l: SizeInt;
  NewIdentifier, OldIdentifier, Src, CurIdentifier: String;
  SearchInCode: TCodeBuffer;
  i, j: integer;
  DeclXY: TPoint;
  TreeOfPCodeXYPosition: TAVLTree;
  ListOfPCodeXYPosition: TFPList;
  Cache: TFindIdentifierReferenceCache;
  RefXYPos: TCodeXYPosition;
  isConflicted: boolean;
begin
  Cache:=nil;
  ListOfPCodeXYPosition:=nil;
  TreeOfPCodeXYPosition:=nil;
  try
    // parse %rename directive
    Src:=DeclCode.Source;
    DeclPos:=Pos('{%rename:',Src);
    if DeclPos<1 then
      Fail('missing declaration marker');
    inc(DeclPos,length('{%rename:'));
    NewIdentifier:=GetIdentifier(@Src[DeclPos]);
    if NewIdentifier='' then
      Fail('missing rename-to identifier');
    while Src[DeclPos]<>'}' do inc(DeclPos);
    inc(DeclPos);
    OldIdentifier:=GetIdentifier(@Src[DeclPos]);
    if OldIdentifier='' then
      Fail('missing rename-from identifier');

    DeclCode.AbsoluteToLineCol(DeclPos,DeclXY.Y,DeclXY.X);

    // find all references
    if SearchInCodeBufs=nil then
      SearchInCodeBufs:=TFPList.Create;
    if SearchInCodeBufs.IndexOf(DeclCode)<0 then
      SearchInCodeBufs.Add(DeclCode);
    for i:=0 to SearchInCodeBufs.Count-1 do begin
      SearchInCode:=TCodeBuffer(SearchInCodeBufs[i]);
      if not CodeToolBoss.FindReferences(DeclCode,DeclXY.X,DeclXY.Y,SearchInCode,false,ListOfPCodeXYPosition,Cache) then
        Fail('FindReferences failed at '+DeclCode.Filename+'('+dbgs(DeclXY.Y)+','+dbgs(DeclXY.X)+')');

      // check that all %R directives were found
      p:=1;
      Src:=SearchInCode.Source;
      l:=length(Src);
      while p<=l do begin
        if (Src[p]='{') and (Src[p+1]='%') and (Src[p+2]='R') and (Src[p+3]='}') then begin
          inc(p,4);
          RefXYPos.Code:=SearchInCode;
          SearchInCode.AbsoluteToLineCol(p,RefXYPos.Y,RefXYPos.X);
          j:=IndexOfCodePosition(ListOfPCodeXYPosition,@RefXYPos);
          if j<0 then begin
            if ListOfPCodeXYPosition=nil then
              debugln(['TTestChangeDeclaration.TestRenameIdentifier ListOfPCodeXYPosition empty'])
            else begin
              debugln(['TTestChangeDeclaration.TestRenameIdentifier ListOfPCodeXYPosition: Count=',ListOfPCodeXYPosition.Count]);
              for j:=0 to ListOfPCodeXYPosition.Count-1 do begin
                debugln(['  ',i,':',dbgs(PCodeXYPosition(ListOfPCodeXYPosition[j])^)]);
              end;
            end;
            Fail('missing reference: '+dbgs(RefXYPos));
          end;
        end else
          inc(p);
      end;

      // add to tree
      if ListOfPCodeXYPosition<>nil then begin
        if TreeOfPCodeXYPosition=nil then
          TreeOfPCodeXYPosition:=CodeToolBoss.CreateTreeOfPCodeXYPosition;
        CodeToolBoss.AddListToTreeOfPCodeXYPosition(ListOfPCodeXYPosition,
                                              TreeOfPCodeXYPosition,true,false); // this empties ListOfPCodeXYPosition
      end;
    end;

    if (TreeOfPCodeXYPosition=nil) or (TreeOfPCodeXYPosition.Count=0) then
      Fail('TreeOfPCodeXYPosition empty');

    // rename references
    if not CodeToolBoss.RenameIdentifier(TreeOfPCodeXYPosition,OldIdentifier,NewIdentifier,DeclCode,@DeclXY,isConflicted) then
      Fail('RenameIdentifier failed');

    // check all {%R} directives were replaced
    for i:=0 to SearchInCodeBufs.Count-1 do begin
      SearchInCode:=TCodeBuffer(SearchInCodeBufs[i]);
      // check that all %R directives were found
      p:=1;
      Src:=SearchInCode.Source;
      l:=length(Src);
      while p<=l do begin
        if (Src[p]='{') and (Src[p+1]='%') and (Src[p+2]='R') and (Src[p+3]='}') then begin
          inc(p,4);
          CurIdentifier:=copy(Src,p,length(NewIdentifier));
          if CurIdentifier<>NewIdentifier then
            Fail('reference differ: expected "'+NewIdentifier+'", but found "'+CurIdentifier+'"');
        end else
          inc(p);
      end;
    end;

  finally
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
    CodeToolBoss.FreeTreeOfPCodeXYPosition(TreeOfPCodeXYPosition);
    Cache.Free;
    SearchInCodeBufs.Free;
  end;

end;

procedure TTestChangeDeclaration.TestRenameIdentifier(DeclCode: TCodeBuffer; Module1: TCodeBuffer);
var
  SearchInModules: TFPList;
begin
  SearchInModules:=TFPList.Create;
  if Module1<>nil then
    SearchInModules.Add(Module1);
  TestRenameIdentifier(DeclCode,SearchInModules);
end;

procedure TTestChangeDeclaration.TestCTAddProcedureModifier;

  procedure Test(ProcCode, aModifier, Expected: string);
  var
    Code: TCodeBuffer;
    Src, ProcHead: String;
  begin
    Src:='unit '+FDefFilename+';'+sLineBreak
      +'interface'+sLineBreak
      +ProcCode+sLineBreak
      +'implementation'+sLineBreak
      +'end.';
    Code:=CodeToolBoss.CreateFile(FDefFilename);
    Code.Source:=Src;
    if not CodeToolBoss.AddProcModifier(Code,3,3,aModifier) then
    begin
      Fail('AddProcModifier failed: '+CodeToolBoss.ErrorMessage);
    end else begin
      if not CodeToolBoss.ExtractProcedureHeader(Code,3,3,
        [phpWithStart,phpWithResultType,phpWithOfObject,phpWithProcModifiers,phpWithComments,phpDoNotAddSemicolon],
        ProcHead)
      then
        Fail('ExtractProcedureHeader failed: '+CodeToolBoss.ErrorMessage);
      if ProcHead<>Expected then begin
        writeln('Test ProcCode="',ProcCode,'"');
        Src:=Code.Source;
        writeln('SrcSTART:======================');
        writeln(Src);
        writeln('SrcEND:========================');
        AssertEquals('ProcHead',Expected,ProcHead);
      end;
    end;
  end;

begin
  // remove first unit
  Test('procedure DoIt;','overload','procedure DoIt; overload;');
  Test('procedure DoIt ;','overload','procedure DoIt; overload ;');
  Test('procedure DoIt ; ;','overload','procedure DoIt; overload ;');
  Test('procedure DoIt; overload;','overload','procedure DoIt; overload;');
  Test('procedure DoIt; {$IFDEF FPC}overload{$ENDIF};','overload','procedure DoIt; {$IFDEF FPC}overload{$ENDIF};');
  Test('procedure DoIt; procedure Bla;','overload','procedure DoIt; overload;');
  Test('  procedure DoIt;'+sLineBreak+'  procedure Bla;',
    'overload','procedure DoIt; overload;');
  Test('  procedure DoIt; external name ''doit'';'+sLineBreak+'  procedure Bla;',
    'overload','procedure DoIt; external name ''doit''; overload;');
end;

procedure TTestChangeDeclaration.TestCTRenameIdentifier_MultiInLine;
var
  DeclCode: TCodeBuffer;
begin
  DeclCode:=CodeToolBoss.CreateFile(FDefFilename);
  DeclCode.Source:='unit '+FDefFilename+';'+sLineBreak
    +'interface'+sLineBreak
    +'type'+sLineBreak
    +'  {%rename:TWhale}TFoo = word;'+sLineBreak
    +'  TBar = low({%R}TFoo)..high({%R}TFoo);'
    +'implementation'+sLineBreak
    +'type'+sLineBreak
    +'  TBird = low({%R}TFoo)..high({%R}TFoo);'
    +'end.';
  TestRenameIdentifier(DeclCode);
end;

procedure TTestChangeDeclaration.TestCTRenameIdentifier_MultiInLine_Amp;
var
  DeclCode: TCodeBuffer;
begin
  DeclCode:=CodeToolBoss.CreateFile(FDefFilename);
  DeclCode.Source:='unit '+FDefFilename+';'+sLineBreak
    +'interface'+sLineBreak
    +'type'+sLineBreak
    +'  {%rename:&Type}TFoo = word;'+sLineBreak
    +'  TBar = low({%R}TFoo)..high({%R}TFoo);'
    +'implementation'+sLineBreak
    +'type'+sLineBreak
    +'  TBird = low({%R}TFoo)..high({%R}TFoo);'
    +'end.';
  TestRenameIdentifier(DeclCode);
end;

initialization
  RegisterTests([TTestChangeDeclaration]);
end.

