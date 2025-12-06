unit TestBlockIndent;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, testregistry, TestBase, math, Types,
  SynEdit, SynEditKeyCmds, SynEditTypes, Forms,
  LazLoggerDummy;
  //LazLogger;

type

  { TTestBlockIndent }

  TTestBlockIndent = class(TTestBase)
  protected
    function TestTextSpace: TStringArray;
    function TestTextTabs: TStringArray;
    function TestTextColumn: TStringArray;
    function TestTextColumn2: TStringArray;

    function ReplaceIndent(AText: TStringArray; AFirstLine: Integer;
                           const ANewIndents: Array of String): TStringArray;
    function AddIndent(AText: TStringArray; AFirstLine, ALastLine, ASpaceCount: Integer): TStringArray;
    function DelIndent(AText: TStringArray; AFirstLine, ALastLine, ASpaceCount: Integer): TStringArray;
    function InsertIntoText(AText: TStringArray; AnInsText: String; const AnXPos: array of integer): TStringArray;
    function DelFromText(AText: TStringArray; const AnXPosAndLen: array of integer): TStringArray;

    procedure TestSelAndText(Name: String; LogX1, LogY1, LogX2, LogY2: Integer;
                             const ExpLines: Array of String;
                             SelIsForward: Boolean = True);
    procedure TestColSelAndText(Name: String; LogX1, LogY1, LogX2, LogY2: Integer;
                             const ExpLines: Array of String;
                             SelIsForward: Boolean = True);
    //procedure DoTest(X1,Y1, X2,Y2: Boolean; CountIndent: Integer;
    //                 ExpX1, ExpY1, ExpX2, ExpY2: Integer;
    //                 ExpText: A
  published
    procedure TestIndent;
    procedure TestUnIndent;
    procedure TestIndentWithTab;
    procedure TestUnIndentWithTab;
    procedure TestIndentColSel;
    procedure TestUnindentColSel;
  end;


implementation

{ TTestBlockIndent }

function TTestBlockIndent.TestTextSpace: TStringArray;
begin
  SetLength(Result, 9);
  Result[0] := 'abc';
  Result[1] := '  def';
  Result[2] := ' 123';
  Result[3] := '';
  Result[4] := '  QWE';
  Result[5] := '    mno';
  Result[6] := '  ZX';
  Result[7] := '      321';
  Result[8] := '';
end;

function TTestBlockIndent.TestTextTabs: TStringArray;
begin
  SetLength(Result, 13);
  Result[0] := 'abc';
  Result[1] := #9'def';
  Result[2] := #9' 123';
  Result[3] := ' '#9'mno';
  Result[4] := #9#9'QWE';
  Result[5] := #9#9' mno...';
  Result[6] := #9#9'  ABCDEF';
  Result[7] := #9#9'   321';
  Result[8] := #9'   '#9'QWE';
  Result[9] := #9'   '#9' mno...';
  Result[10]:= #9'   '#9'  ABCDEF';
  Result[11]:= #9'   '#9'   321';
  Result[12] := '';
end;

function TTestBlockIndent.TestTextColumn: TStringArray;
begin
  SetLength(Result, 8);
  Result[0] := 'abc def ghi mno pqr';
  Result[1] := 'abc de'#9#9'123456789';
  Result[2] := 'äbc ÄÖÜ 123 456 789';   // ä/Ä takes 2 bytes (logical pos)
  Result[3] := 'ab ｍef ghi mno pqr';   // ｍ  takes 3 bytes (logical pos), but also takes 1 extra phys pos
  Result[4] := 'ABCDEFGHIJKLMNOPQRSTUABCDEFGHIJKLMNOPQRSTU';
  Result[5] := ' abd   '#9#9' xyz';
  Result[6] := 'ABCDEFGHIJKLMNOPQRSTUABCDEFGHIJKLMNOPQRSTU';
  Result[7] := '';
end;

function TTestBlockIndent.TestTextColumn2: TStringArray;
begin
  SetLength(Result, 5);
  Result[0] := 'ABCDEFGHIJKLMNOPQRSTUABCDEFGHIJKLMNOPQRSTU';
  Result[1] := 'abc          def';
  Result[2] := 'abc '#9#9'   def';
  Result[3] := 'ABCDEFGHIJKLMNOPQRSTUABCDEFGHIJKLMNOPQRSTU';
  Result[4] := '';
end;

function GetLeadWSLen(s: String): integer;
var
  Run : PChar;
begin
  Run := PChar(s);
  while (Run[0] in [' ', #9]) do
    Inc(Run);
  Result := Run - PChar(s);
end;

function TTestBlockIndent.ReplaceIndent(AText: TStringArray; AFirstLine: Integer;
  const ANewIndents: array of String): TStringArray;
var
  i: Integer;
begin
  SetLength(Result, length(AText));
  for i := 0 to Length(AText) - 1 do
    Result[i] := AText[i];
  for i := 0 to Length(ANewIndents) - 1 do begin
    Result[AFirstLine + i] := ANewIndents[i]
      + copy(AText[AFirstLine + i], GetLeadWSLen(AText[AFirstLine + i]) + 1, length(AText[AFirstLine + i]));
  end;
end;

function TTestBlockIndent.AddIndent(AText: TStringArray; AFirstLine, ALastLine,
  ASpaceCount: Integer): TStringArray;
var
  i: Integer;
begin
  SetLength(Result, length(AText));
  for i := 0 to Length(AText) - 1 do
    Result[i] := AText[i];
  for i := AFirstLine to ALastLine do begin
    Result[i] := copy(AText[i], 1, GetLeadWSLen(AText[i]))
               + StringOfChar(' ', ASpaceCount)
               + copy(AText[i], GetLeadWSLen(AText[i]) + 1, length(AText[i]));
  end;
end;

function TTestBlockIndent.DelIndent(AText: TStringArray; AFirstLine, ALastLine,
  ASpaceCount: Integer): TStringArray;
var
  i: Integer;
begin
  SetLength(Result, length(AText));
  for i := 0 to Length(AText) - 1 do
    Result[i] := AText[i];
  for i := AFirstLine to ALastLine do begin
    Result[i] := copy(AText[i], 1, Max( GetLeadWSLen(AText[i]) - ASpaceCount, 0))
               + copy(AText[i], GetLeadWSLen(AText[i]) + 1, length(AText[i]));
  end;
end;

function TTestBlockIndent.InsertIntoText(AText: TStringArray; AnInsText: String;
  const AnXPos: array of integer): TStringArray;
var
  i: Integer;
begin
  SetLength(Result, Length(AText));
  for i := 0 to Length(AText) - 1 do begin
    if (i < Length(AnXPos)) and (AnXPos[i] > 0) then
      Result[i] := copy(AText[i], 1, AnXPos[i]-1) + AnInsText + copy(AText[i], AnXPos[i], Length(AText[i]))
    else
      Result[i] := AText[i];
  end;
end;

function TTestBlockIndent.DelFromText(AText: TStringArray; const AnXPosAndLen: array of integer
  ): TStringArray;
var
  i, x: Integer;
begin
  SetLength(Result, Length(AText));
  for i := 0 to Length(AText) - 1 do begin
    x := i * 2;
    if (x < Length(AnXPosAndLen)) and (AnXPosAndLen[x] > 0) then
      Result[i] := copy(AText[i], 1, AnXPosAndLen[x]-1) + copy(AText[i], AnXPosAndLen[x] + AnXPosAndLen[x+1], Length(AText[i]))
    else
      Result[i] := AText[i];
  end;
end;

procedure TTestBlockIndent.TestSelAndText(Name: String; LogX1, LogY1, LogX2, LogY2: Integer;
  const ExpLines: array of String; SelIsForward: Boolean);
begin
  if SelIsForward then
    TestIsCaretAndSel(Name, LogX1, LogY1, LogX2, LogY2)
  else
    TestIsCaretAndSelBackward(Name, LogX1, LogY1, LogX2, LogY2);
  TestIsFullText(Name, ExpLines);
end;

procedure TTestBlockIndent.TestColSelAndText(Name: String; LogX1, LogY1, LogX2, LogY2: Integer;
  const ExpLines: array of String; SelIsForward: Boolean);
var
  BB, BE: TPoint;
begin
  TestIsCaret(Name, LogX2, LogY2);
  if SynEdit.IsBackwardSel then begin
    BB := SynEdit.BlockEnd;
    BE := SynEdit.BlockBegin;
  end
  else begin
    BB := SynEdit.BlockBegin;
    BE := SynEdit.BlockEnd;
  end;

  if (BB.X <> LogX1) or (BB.Y <> LogY1) then
    TestFail(Name, 'IsBlockBegin(Log)',
             Format('X/Y=(%d, %d)', [LogX1, LogY1]),
             Format('X/Y=(%d, %d)', [BB.X, BB.Y]));
  if (BE.X <> LogX2) or (BE.Y <> LogY2) then
    TestFail(Name, 'IsBlockEnd(Log)',
             Format('X/Y=(%d, %d)', [LogX1, LogY1]),
             Format('X/Y=(%d, %d)', [BE.X, BE.Y]));

  TestIsFullText(Name, ExpLines);
end;

procedure TTestBlockIndent.TestIndent;
var
  i, j: Integer;
begin
  ReCreateEdit;
  SetLines(TestTextSpace);
  PushBaseName('');

  {%region simple indent}
  SynEdit.Options := SynEdit.Options - [eoGroupUndo, eoTrimTrailingSpaces];
  for i := 1 to 5 do begin
    PopPushBaseName('simple Indent BlockIndent='+ IntToStr(i));
    SetLines(TestTextSpace);

    SynEdit.BlockIndent := i;
    SynEdit.TabWidth := i+8; // make sure it does not interfere
    SetCaretAndSel(3,1, 9,8); // ab[c ... 3]21
    SynEdit.CommandProcessor(ecBlockIndent, '', nil);
    TestSelAndText('indented',  3+i,1,  9+i,8,  AddIndent(TestTextSpace, 0, 7, i));

    SynEdit.Undo;
    TestSelAndText('indent undone',  3,1,  9,8,  TestTextSpace);

    SynEdit.Redo;
    TestSelAndText('indent redone',  3+i,1,  9+i,8,  AddIndent(TestTextSpace, 0, 7, i));

    SynEdit.Undo;
    TestSelAndText('indent undone 2nd',  3,1,  9,8,  TestTextSpace);

    SynEdit.Redo;
    TestSelAndText('indent redone 2nd',  3+i,1,  9+i,8,  AddIndent(TestTextSpace, 0, 7, i));


    SetLines(TestTextSpace);
    SetCaretAndSel(3,2, 2,3); // [def ... ]123
    SynEdit.CommandProcessor(ecBlockIndent, '', nil);
    TestSelAndText('indented 2 line only',  3+i,2,  2+i,3,  AddIndent(TestTextSpace, 1, 2, i));

    SetLines(TestTextSpace);
    SetCaretAndSelBackward(3,2, 2,3); // [def ... ]123
    SynEdit.CommandProcessor(ecBlockIndent, '', nil);
    TestSelAndText('indented 2 line only (backward)',  3+i,2,  2+i,3,  AddIndent(TestTextSpace, 1, 2, i), False);

    SetLines(TestTextSpace);
    SetCaretAndSel(3,2, 4,2); // [d]ef
    SynEdit.CommandProcessor(ecBlockIndent, '', nil);
    TestSelAndText('indented 1 line only',  3+i,2,  4+i,2,  AddIndent(TestTextSpace, 1, 1, i));
  end;
  {%endregion}

  {%region double indent, no GroupUndo}
  SynEdit.Options := SynEdit.Options - [eoGroupUndo, eoTrimTrailingSpaces];
  for i := 1 to 5 do begin
    for j := 1 to 5 do begin
      PopPushBaseName('double Indent (no GroupUndo) BlockIndent='+ IntToStr(i)+', '+ IntToStr(j));
      SetLines(TestTextSpace);

      SynEdit.BlockIndent := i;
      SynEdit.TabWidth := i+8; // make sure it does not interfere
      SetCaretAndSel(3,1, 9,8); // ab[c ... 3]21
      SynEdit.CommandProcessor(ecBlockIndent, '', nil);
      TestSelAndText('indented',  3+i,1,  9+i,8,  AddIndent(TestTextSpace, 0, 7, i));

      SynEdit.BlockIndent := j;
      SynEdit.TabWidth := j+8; // make sure it does not interfere
      SynEdit.CommandProcessor(ecBlockIndent, '', nil);
      TestSelAndText('2nd indented',  3+i+j,1,  9+i+j,8,  AddIndent(TestTextSpace, 0, 7, i+j));

      SynEdit.Undo;
      TestSelAndText('one indent undone',  3+i,1,  9+i,8,  AddIndent(TestTextSpace, 0, 7, i));

      SynEdit.Redo;
      TestSelAndText('one indent redone',  3+i+j,1,  9+i+j,8,  AddIndent(TestTextSpace, 0, 7, i+j));

      SynEdit.Undo;
      TestSelAndText('one indent undone (2nd)',  3+i,1,  9+i,8,  AddIndent(TestTextSpace, 0, 7, i));
      SynEdit.Undo;
      TestSelAndText('two indent undone (2nd)',  3,1,  9,8,  TestTextSpace);

      SynEdit.Redo;
      TestSelAndText('one indent redone(2nd)',  3+i,1,  9+i,8,  AddIndent(TestTextSpace, 0, 7, i));
      SynEdit.Redo;
      TestSelAndText('two indent redone(2nd)',  3+i+j,1,  9+i+j,8,  AddIndent(TestTextSpace, 0, 7, i+j));

      SynEdit.Undo;
      TestSelAndText('one indent undone (3rd)',  3+i,1,  9+i,8,  AddIndent(TestTextSpace, 0, 7, i));
    end;
  end;
  {%endregion}

  {%region double indent, with GroupUndo}
  SynEdit.Options := SynEdit.Options + [eoGroupUndo] - [eoTrimTrailingSpaces];
  for i := 1 to 5 do begin
    for j := 1 to 5 do begin
      PopPushBaseName('double Indent (no GroupUndo) BlockIndent='+ IntToStr(i)+', '+ IntToStr(j));
      SetLines(TestTextSpace);

      SynEdit.BlockIndent := i;
      SynEdit.TabWidth := i+8; // make sure it does not interfere
      SetCaretAndSel(3,1, 9,8); // ab[c ... 3]21
      SynEdit.CommandProcessor(ecBlockIndent, '', nil);
      TestSelAndText('indented',  3+i,1,  9+i,8,  AddIndent(TestTextSpace, 0, 7, i));

      SynEdit.BlockIndent := j;
      SynEdit.TabWidth := j+8; // make sure it does not interfere
      SynEdit.CommandProcessor(ecBlockIndent, '', nil);
      TestSelAndText('2nd indented',  3+i+j,1,  9+i+j,8,  AddIndent(TestTextSpace, 0, 7, i+j));

      SynEdit.Undo;
      TestSelAndText('indent undone',  3,1,  9,8,  TestTextSpace);

      SynEdit.Redo;
      TestSelAndText('indent redone',  3+i+j,1,  9+i+j,8,  AddIndent(TestTextSpace, 0, 7, i+j));

      SynEdit.Undo;
      TestSelAndText('indent undone (2nd)',  3,1,  9,8,  TestTextSpace);

      SynEdit.Redo;
      TestSelAndText('indent redone(2nd)',  3+i+j,1,  9+i+j,8,  AddIndent(TestTextSpace, 0, 7, i+j));
    end;
  end;
  {%endregion}
end;

procedure TTestBlockIndent.TestUnIndent;
var
  i, j: Integer;
begin
  ReCreateEdit;
  SetLines(TestTextSpace);
  PushBaseName('');

  {%region simple unindent}
    SynEdit.Options := SynEdit.Options - [eoGroupUndo, eoTrimTrailingSpaces];
    for i := 1 to 5 do begin
      PopPushBaseName('simple Unindent BlockIndent='+ IntToStr(i));
      SetLines(TestTextSpace);

      SynEdit.BlockIndent := i;
      SynEdit.TabWidth := i+8; // make sure it does not interfere
      SetCaretAndSel(3,1, 9,8); // ab[c ... 32]1
      SynEdit.CommandProcessor(ecBlockUnindent, '', nil);
      // first line can not be unindented, caret/blockbegin is unchanged
      TestSelAndText('Unindented',  3,1,  Max(9-i,1),8,  DelIndent(TestTextSpace, 0, 7, i));

      SynEdit.Undo;
      TestSelAndText('unindent undone',  3,1,  9,8,  TestTextSpace);
      SynEdit.Redo;
      TestSelAndText('Unindent redone',  3,1,  Max(9-i,1),8,  DelIndent(TestTextSpace, 0, 7, i));

      SynEdit.Undo;
      TestSelAndText('unindent undone(2nd)',  3,1,  9,8,  TestTextSpace);
      SynEdit.Redo;
      TestSelAndText('Unindent redone(2nd)',  3,1,  Max(9-i,1),8,  DelIndent(TestTextSpace, 0, 7, i));



      SetLines(TestTextSpace);
      SetCaretAndSel(3,2, 2,3); // [def ... ]123
      SynEdit.CommandProcessor(ecBlockUnindent, '', nil);
      // line 3 can only be unindented by max 1
      TestSelAndText('Unindented 2 line only',  Max(3-i,1),2,  2-Min(i,1),3,  DelIndent(TestTextSpace, 1, 2, i));

      SetLines(TestTextSpace);
      SetCaretAndSelBackward(3,2, 2,3); // [def ... ]123
      SynEdit.CommandProcessor(ecBlockUnindent, '', nil);
      TestSelAndText('Unindented 2 line only (backward)',  Max(3-i,1),2,  2-Min(i,1),3,  DelIndent(TestTextSpace, 1, 2, i), False);

      SetLines(TestTextSpace);
      SetCaretAndSel(3,2, 4,2); // [d]ef
      SynEdit.CommandProcessor(ecBlockUnindent, '', nil);
      TestSelAndText('Unindented 1 line only',  3-Min(i,2),2,  4-Min(i,2),2,  DelIndent(TestTextSpace, 1, 1, i));
    end;
  {%endregion}

  {%region double indent, no GroupUndo}
    SynEdit.Options := SynEdit.Options - [eoGroupUndo, eoTrimTrailingSpaces];
    for i := 1 to 5 do begin
      for j := 1 to 5 do begin
        PopPushBaseName('double Indent (no GroupUndo) BlockIndent='+ IntToStr(i)+', '+ IntToStr(j));
        SetLines(TestTextSpace);

        SynEdit.BlockIndent := i;
        SynEdit.TabWidth := i+8; // make sure it does not interfere
        SetCaretAndSel(3,1, 9,8); // ab[c ... 32]1
        SynEdit.CommandProcessor(ecBlockUnindent, '', nil);
        TestSelAndText('indented',  3,1,  9-Min(i,6),8,  DelIndent(TestTextSpace, 0, 7, i));

        SynEdit.BlockIndent := j;
        SynEdit.TabWidth := j+8; // make sure it does not interfere
        SynEdit.CommandProcessor(ecBlockUnindent, '', nil);
        TestSelAndText('2nd indented',  3,1,  9-Min(i+j,6),8,  DelIndent(TestTextSpace, 0, 7, i+j));

        SynEdit.Undo;
        TestSelAndText('one indent undone',  3,1,  9-Min(i,6),8,  DelIndent(TestTextSpace, 0, 7, i));

        SynEdit.Redo;
        TestSelAndText('one indent redone',  3,1,  9-Min(i+j,6),8,  DelIndent(TestTextSpace, 0, 7, i+j));

        SynEdit.Undo;
        TestSelAndText('one indent undone (2nd)',  3,1,  9-Min(i,6),8,  DelIndent(TestTextSpace, 0, 7, i));
        SynEdit.Undo;
        TestSelAndText('two indent undone (2nd)',  3,1,  9,8,  TestTextSpace);

        SynEdit.Redo;
        TestSelAndText('one indent redone(2nd)',  3,1,  9-Min(i,6),8,  DelIndent(TestTextSpace, 0, 7, i));
        SynEdit.Redo;
        TestSelAndText('two indent redone(2nd)',  3,1,  9-Min(i+j,6),8,  DelIndent(TestTextSpace, 0, 7, i+j));

        SynEdit.Undo;
        TestSelAndText('one indent undone (3rd)',  3,1,  9-Min(i,6),8,  DelIndent(TestTextSpace, 0, 7, i));
      end;
    end;
  {%endregion}
end;

procedure TTestBlockIndent.TestIndentWithTab;
begin
  ReCreateEdit;

  {%region Unindent BlockIndent=2 Tab=4}
    PushBaseName('Unindent BlockIndent=2 Tab=4');
    SynEdit.Options := SynEdit.Options - [eoGroupUndo, eoTrimTrailingSpaces];
    SetLines(TestTextTabs);
    SynEdit.BlockIndent := 2;
    SynEdit.TabWidth := 4;

    SetCaretAndSel(3,1, 10,12); // ab[c ... 3]21
    TestIsCaretPhys('self-test',13,12);
    SynEdit.CommandProcessor(ecBlockIndent, '', nil);
    TestSelAndText('Unindented',  5,1,  12,12, ReplaceIndent(TestTextTabs, 0,
      [ '  ',  #9'  ',                      //   'abc'   //   #9'def'
        #9'   ',  ' '#9'  ',                //   #9' 123'   //   ' '#9'mno'
        #9#9'  ',  #9#9'   ',               //   #9#9'QWE'   //   #9#9' mno'
        #9#9'    ',  #9#9'     ',           //   #9#9'  ABCDEF'   //   #9#9'   321'
        #9'   '#9'  ',  #9'   '#9'   ',     //   #9'   '#9'QWE'   //    #9'   '#9' mno'
        #9'   '#9'    ',  #9'   '#9'     '  //   #9'   '#9'  ABCDE'   //   #9'   '#9'   321'
      ] ));
    TestIsCaretPhys('self-test',15,12);

    SynEdit.Undo;
    TestSelAndText('unindent undone',  3,1,  10,12,  TestTextTabs);
    SynEdit.Redo;
    TestSelAndText('Unindent redone',  5,1,  12,12, ReplaceIndent(TestTextTabs, 0,
      [ '  ',  #9'  ',                      //   'abc'   //   #9'def'
        #9'   ',  ' '#9'  ',                //   #9' 123'   //   ' '#9'mno'
        #9#9'  ',  #9#9'   ',               //   #9#9'QWE'   //   #9#9' mno'
        #9#9'    ',  #9#9'     ',           //   #9#9'  ABCDEF'   //   #9#9'   321'
        #9'   '#9'  ',  #9'   '#9'   ',     //   #9'   '#9'QWE'   //    #9'   '#9' mno'
        #9'   '#9'    ',  #9'   '#9'     '  //   #9'   '#9'  ABCDE'   //   #9'   '#9'   321'
      ] ));

    SynEdit.Undo;
    TestSelAndText('unindent undone(2nd)',  3,1,  10,12,  TestTextTabs);
    SynEdit.Redo;
    TestSelAndText('Unindent redone(2nd',  5,1,  12,12, ReplaceIndent(TestTextTabs, 0,
      [ '  ',  #9'  ',                      //   'abc'   //   #9'def'
        #9'   ',  ' '#9'  ',                //   #9' 123'   //   ' '#9'mno'
        #9#9'  ',  #9#9'   ',               //   #9#9'QWE'   //   #9#9' mno'
        #9#9'    ',  #9#9'     ',           //   #9#9'  ABCDEF'   //   #9#9'   321'
        #9'   '#9'  ',  #9'   '#9'   ',     //   #9'   '#9'QWE'   //    #9'   '#9' mno'
        #9'   '#9'    ',  #9'   '#9'     '  //   #9'   '#9'  ABCDE'   //   #9'   '#9'   321'
      ] ));


  {%endregion}
end;

procedure TTestBlockIndent.TestUnIndentWithTab;
begin
  ReCreateEdit;

  {%region Unindent BlockIndent=2 Tab=4}
    PushBaseName('Unindent BlockIndent=2 Tab=4');
    SynEdit.Options := SynEdit.Options - [eoGroupUndo, eoTrimTrailingSpaces];
    SetLines(TestTextTabs);
    SynEdit.BlockIndent := 2;
    SynEdit.TabWidth := 4;

    SetCaretAndSel(3,1, 10,12); // ab[c ... 3]21
    TestIsCaretPhys('self-test',13,12);
    SynEdit.CommandProcessor(ecBlockUnindent, '', nil);
    // first line can not be unindented, caret/blockbegin is unchanged
    TestSelAndText('Unindented',  3,1,  8,12, ReplaceIndent(TestTextTabs, 0,
      [ '',  '  ',               //   'abc'   //   #9'def'
        '   ',  '  ',            //   #9' 123'   //   ' '#9'mno'
        #9'  ',  #9'   ',        //   #9#9'QWE'   //   #9#9' mno'
        #9#9,  #9#9' ',          //   #9#9'  ABCDEF'   //   #9#9'   321'
        #9'  ',  #9'   ',        //   #9'   '#9'QWE'   //    #9'   '#9' mno'
        #9'   '#9,  #9'   '#9' ' //   #9'   '#9'  ABCDE'   //   #9'   '#9'   321'
      ] ));
    TestIsCaretPhys('self-test',11,12);

    SynEdit.Undo;
    TestSelAndText('unindent undone',  3,1,  10,12,  TestTextTabs);
    SynEdit.Redo;
    TestSelAndText('Unindent redone',  3,1,  8,12, ReplaceIndent(TestTextTabs, 0,
      [ '',  '  ',               //   'abc'   //   #9'def'
        '   ',  '  ',            //   #9' 123'   //   ' '#9'mno'
        #9'  ',  #9'   ',        //   #9#9'QWE'   //   #9#9' mno'
        #9#9,  #9#9' ',          //   #9#9'  ABCDEF'   //   #9#9'   321'
        #9'  ',  #9'   ',        //   #9'   '#9'QWE'   //    #9'   '#9' mno'
        #9'   '#9,  #9'   '#9' ' //   #9'   '#9'  ABCDE'   //   #9'   '#9'   321'
      ] ));

    SynEdit.Undo;
    TestSelAndText('unindent undone(2nd)',  3,1,  10,12,  TestTextTabs);
    SynEdit.Redo;
    TestSelAndText('Unindent redone(2nd',  3,1,  8,12, ReplaceIndent(TestTextTabs, 0,
      [ '',  '  ',               //   'abc'   //   #9'def'
        '   ',  '  ',            //   #9' 123'   //   ' '#9'mno'
        #9'  ',  #9'   ',        //   #9#9'QWE'   //   #9#9' mno'
        #9#9,  #9#9' ',          //   #9#9'  ABCDEF'   //   #9#9'   321'
        #9'  ',  #9'   ',        //   #9'   '#9'QWE'   //    #9'   '#9' mno'
        #9'   '#9,  #9'   '#9' ' //   #9'   '#9'  ABCDE'   //   #9'   '#9'   321'
      ] ));


    // unindent a 2nd time
    SynEdit.CommandProcessor(ecBlockUnindent, '', nil);
    // first line can not be unindented, caret/blockbegin is unchanged
    TestSelAndText('Unindented twice',  3,1,  6,12, ReplaceIndent(TestTextTabs, 0,
      [ '',  '',                 //   'abc'   //   #9'def'
        ' ',  '',                //   #9' 123'   //   ' '#9'mno'
        #9,  #9' ',              //   #9#9'QWE'   //   #9#9' mno'
        #9'  ',  #9'   ',        //   #9#9'  ABCDEF'   //   #9#9'   321'
        #9,  #9' ',              //   #9'   '#9'QWE'   //    #9'   '#9' mno'
        #9'  ',  #9'   '         //   #9'   '#9'  ABCDE'   //   #9'   '#9'   321'
      ] ));
    TestIsCaretPhys('self-test',9,12);

    SynEdit.Undo;
    TestSelAndText('Unindent twice, undone once',  3,1,  8,12, ReplaceIndent(TestTextTabs, 0,
      [ '',  '  ',               //   'abc'   //   #9'def'
        '   ',  '  ',            //   #9' 123'   //   ' '#9'mno'
        #9'  ',  #9'   ',        //   #9#9'QWE'   //   #9#9' mno'
        #9#9,  #9#9' ',          //   #9#9'  ABCDEF'   //   #9#9'   321'
        #9'  ',  #9'   ',        //   #9'   '#9'QWE'   //    #9'   '#9' mno'
        #9'   '#9,  #9'   '#9' ' //   #9'   '#9'  ABCDE'   //   #9'   '#9'   321'
      ] ));

    SynEdit.Redo;
    TestSelAndText('Unindented twice, redone',  3,1,  6,12, ReplaceIndent(TestTextTabs, 0,
      [ '',  '',                 //   'abc'   //   #9'def'
        ' ',  '',                //   #9' 123'   //   ' '#9'mno'
        #9,  #9' ',              //   #9#9'QWE'   //   #9#9' mno'
        #9'  ',  #9'   ',        //   #9#9'  ABCDEF'   //   #9#9'   321'
        #9,  #9' ',              //   #9'   '#9'QWE'   //    #9'   '#9' mno'
        #9'  ',  #9'   '         //   #9'   '#9'  ABCDE'   //   #9'   '#9'   321'
      ] ));

    SynEdit.Undo;
    TestSelAndText('Unindent twice, undone once(2nd)',  3,1,  8,12, ReplaceIndent(TestTextTabs, 0,
      [ '',  '  ',               //   'abc'   //   #9'def'
        '   ',  '  ',            //   #9' 123'   //   ' '#9'mno'
        #9'  ',  #9'   ',        //   #9#9'QWE'   //   #9#9' mno'
        #9#9,  #9#9' ',          //   #9#9'  ABCDEF'   //   #9#9'   321'
        #9'  ',  #9'   ',        //   #9'   '#9'QWE'   //    #9'   '#9' mno'
        #9'   '#9,  #9'   '#9' ' //   #9'   '#9'  ABCDE'   //   #9'   '#9'   321'
      ] ));

    SynEdit.Undo;
    TestSelAndText('unindent twice, undone twice(2nd)',  3,1,  10,12,  TestTextTabs);

    SynEdit.Redo;
    TestSelAndText('Unindent twice, redone 1 of 2 (2nd)',  3,1,  8,12, ReplaceIndent(TestTextTabs, 0,
      [ '',  '  ',               //   'abc'   //   #9'def'
        '   ',  '  ',            //   #9' 123'   //   ' '#9'mno'
        #9'  ',  #9'   ',        //   #9#9'QWE'   //   #9#9' mno'
        #9#9,  #9#9' ',          //   #9#9'  ABCDEF'   //   #9#9'   321'
        #9'  ',  #9'   ',        //   #9'   '#9'QWE'   //    #9'   '#9' mno'
        #9'   '#9,  #9'   '#9' ' //   #9'   '#9'  ABCDE'   //   #9'   '#9'   321'
      ] ));

    SynEdit.Redo;
    TestSelAndText('Unindented twice, redone 2 of 2 (2nd)',  3,1,  6,12, ReplaceIndent(TestTextTabs, 0,
      [ '',  '',                 //   'abc'   //   #9'def'
        ' ',  '',                //   #9' 123'   //   ' '#9'mno'
        #9,  #9' ',              //   #9#9'QWE'   //   #9#9' mno'
        #9'  ',  #9'   ',        //   #9#9'  ABCDEF'   //   #9#9'   321'
        #9,  #9' ',              //   #9'   '#9'QWE'   //    #9'   '#9' mno'
        #9'  ',  #9'   '         //   #9'   '#9'  ABCDE'   //   #9'   '#9'   321'
      ] ));
  {%endregion}

  {%region Unindent BlockIndent=3 Tab=4}
    PushBaseName('Unindent BlockIndent=3 Tab=4');
    SynEdit.Options := SynEdit.Options - [eoGroupUndo, eoTrimTrailingSpaces];
    SetLines(TestTextTabs);
    SynEdit.BlockIndent := 3;
    SynEdit.TabWidth := 4;

    SetCaretAndSel(3,1, 10,12); // ab[c ... 3]21
    TestIsCaretPhys('self-test',13,12);
    SynEdit.CommandProcessor(ecBlockUnindent, '', nil);
    // first line can not be unindented, caret/blockbegin is unchanged
    TestSelAndText('Unindented',  3,1,  7,12, ReplaceIndent(TestTextTabs, 0,
      [ '',  ' ',              //   'abc'   //   #9'def'
        '  ',  ' ',            //   #9' 123'   //   ' '#9'mno'
        #9' ',  #9'  ',        //   #9#9'QWE'   //   #9#9' mno'
        #9'   ',  #9#9,        //   #9#9'  ABCDEF'   //   #9#9'   321'
        #9' ',  #9'  ',        //   #9'   '#9'QWE'   //    #9'   '#9' mno'
        #9'   ',  #9'   '#9    //   #9'   '#9'  ABCDE'   //   #9'   '#9'   321'
      ] ));
    TestIsCaretPhys('self-test',10,12);

    SynEdit.Undo;
    TestSelAndText('unindent undone',  3,1,  10,12,  TestTextTabs);
    SynEdit.Redo;
    TestSelAndText('Unindent redone',  3,1,  7,12, ReplaceIndent(TestTextTabs, 0,
      [ '',  ' ',              //   'abc'   //   #9'def'
        '  ',  ' ',            //   #9' 123'   //   ' '#9'mno'
        #9' ',  #9'  ',        //   #9#9'QWE'   //   #9#9' mno'
        #9'   ',  #9#9,        //   #9#9'  ABCDEF'   //   #9#9'   321'
        #9' ',  #9'  ',        //   #9'   '#9'QWE'   //    #9'   '#9' mno'
        #9'   ',  #9'   '#9    //   #9'   '#9'  ABCDE'   //   #9'   '#9'   321'
      ] ));

    SynEdit.Undo;
    TestSelAndText('unindent undone(2nd)',  3,1,  10,12,  TestTextTabs);
    SynEdit.Redo;
    TestSelAndText('Unindent redone(2nd',  3,1,  7,12, ReplaceIndent(TestTextTabs, 0,
      [ '',  ' ',              //   'abc'   //   #9'def'
        '  ',  ' ',            //   #9' 123'   //   ' '#9'mno'
        #9' ',  #9'  ',        //   #9#9'QWE'   //   #9#9' mno'
        #9'   ',  #9#9,        //   #9#9'  ABCDEF'   //   #9#9'   321'
        #9' ',  #9'  ',        //   #9'   '#9'QWE'   //    #9'   '#9' mno'
        #9'   ',  #9'   '#9    //   #9'   '#9'  ABCDE'   //   #9'   '#9'   321'
      ] ));
  {%endregion}

end;

procedure TTestBlockIndent.TestIndentColSel;
var
  TheCommand: integer;

  procedure SetSelect(x1,y1, x2,y2: integer);
  begin
    SynEdit.LogicalCaretXY := Point(x2, y2);
    SynEdit.BlockBegin     := Point(x1, y1);
    SynEdit.BlockEnd       := Point(x2, y2);
    SynEdit.SelectionMode  := smColumn;
  end;

  procedure DoTest(AName: String; x1,y1, x2,y2: integer;
    ExpectSelStartX, ExpCaretX: integer;
    AnInsText: String; const AnXPos: array of integer;
    ATestUndoRedo: Integer = 2
    );
  var
    TestTextSub: TStringArray;
  begin
    TestTextSub := InsertIntoText(TestTextColumn, AnInsText, AnXPos);
    SetLines(TestTextColumn);
    PushBaseName('');

    SetSelect(x1,y1, x2,y2);
    DebugLn();
    DebugLn('#############################################');
    DebugLn(MyDbg(SynEdit.Text, True));Application.ProcessMessages;
    SynEdit.CommandProcessor(TheCommand, '', nil);
    DebugLn(MyDbg(SynEdit.Text, True));Application.ProcessMessages;
    TestColSelAndText('indend',  ExpectSelStartX,y1, ExpCaretX,y2, TestTextSub );
    if ATestUndoRedo = 0 then exit;

    SynEdit.CommandProcessor(ecUndo, '', nil);
    TestColSelAndText('undo 1',  x1,y1, x2,y2, TestTextColumn  );
    SynEdit.CommandProcessor(ecRedo, '', nil);
    TestColSelAndText('redo 1',  ExpectSelStartX,y1, ExpCaretX,y2, TestTextSub );
    if ATestUndoRedo = 1 then exit;

    SynEdit.CommandProcessor(ecUndo, '', nil);
    TestColSelAndText('undo 2',  x1,y1, x2,y2, TestTextColumn );
    SynEdit.CommandProcessor(ecRedo, '', nil);
    TestColSelAndText('redo 2',  ExpectSelStartX,y1, ExpCaretX,y2, TestTextSub );

    PopBaseName;
  end;


  procedure DoTest2(AName: String; x1,y1, x2,y2: integer;
    ExpectSelStartX1, ExpCaretX1, ExpectSelStartX2, ExpCaretX2: integer;
    AnInsText: String; const AnXPos1, AnXPos2: array of integer;
    ExpGroupUndo: boolean
    );
  var
    TestTextSub1, TestTextSub2: TStringArray;
    undo: Integer;
  begin
    TestTextSub1 := InsertIntoText(TestTextColumn, AnInsText, AnXPos1);
    TestTextSub2 := InsertIntoText(TestTextSub1, AnInsText, AnXPos2);
    for undo := 0 to 2 do begin
      DoTest(AName, x1, y1, x2, y2, ExpectSelStartX1, ExpCaretX1, AnInsText, AnXPos1, undo);

      SynEdit.CommandProcessor(TheCommand, '', nil);
      DebugLn(MyDbg(SynEdit.Text, True)); Application.ProcessMessages;
      TestColSelAndText('indend 2',  ExpectSelStartX2,y1, ExpCaretX2,y2, TestTextSub2 );

      SynEdit.CommandProcessor(ecUndo, '', nil);
      if ExpGroupUndo then
        TestColSelAndText('undo 2-1',  x1,y1, x2,y2, TestTextColumn )
      else
        TestColSelAndText('undo 2-1',  ExpectSelStartX1,y1, ExpCaretX1,y2, TestTextSub1 );
      //
      SynEdit.CommandProcessor(ecRedo, '', nil);
      TestColSelAndText('redo 2-1',  ExpectSelStartX2,y1, ExpCaretX2,y2, TestTextSub2 );


      SynEdit.CommandProcessor(ecUndo, '', nil);
      if not ExpGroupUndo then begin
        TestColSelAndText('undo 2-2a',  ExpectSelStartX1,y1, ExpCaretX1,y2, TestTextSub1 );
        SynEdit.CommandProcessor(ecUndo, '', nil);
      end;
      TestColSelAndText('undo 2-2b',  x1,y1, x2,y2, TestTextColumn );
      //
      SynEdit.CommandProcessor(ecRedo, '', nil);
      if not ExpGroupUndo then begin
        TestColSelAndText('redo 2-2b',  ExpectSelStartX1,y1, ExpCaretX1,y2, TestTextSub1 );
        SynEdit.CommandProcessor(ecRedo, '', nil);
      end;
      TestColSelAndText('redo 2-2a',  ExpectSelStartX2,y1, ExpCaretX2,y2, TestTextSub2 );
    end;
  end;


begin
  ReCreateEdit;
  SetLines(TestTextColumn);
  PushBaseName('');

  SynEdit.Options := SynEdit.Options - [eoGroupUndo] + [eoShowSpecialChars];
  SynEdit.TabWidth := 5;
  SynEdit.BlockIndent    := 2;
  SynEdit.BlockTabIndent := 0;
  TheCommand := ecBlockIndent;


(*  Line 1 Log = Phys
    Line 2 Tabs
    Line 3 Log = Phys // +1 for each äÄÖÜ
    Line 4 Log = Phys // +1 after ｍ
1   5   9   12
abc def ghi mno pqr'
1   5  7  8    9
abc de'#9 #9  '123456789'
abc de		123456789'
1   6   13  17
äbc ÄÖÜ 123 456 789';   // ä/Ä takes 2 bytes (logical pos)
1  4 7  10  14
ab ｍef ghi mno pqr';   // ｍ  takes 3 bytes (logical pos), but also takes 1 extra phys pos
1   5   9   12
ABCDEFGHIJKLMNOPQRSTUABCDEFGHIJKLMNOPQRSTU';
1   5   8 9     11
 abd   '#9#9  ' xyz';
ABCDEFGHIJKLMNOPQRSTUABCDEFGHIJKLMNOPQRSTU';

*)

  DoTest2('',   5,1, 12,4,    5, 14,   5, 16,    '  ',   [5,5,6,4], [5,5,6,4], False);
  DoTest2('',   6,3, 12,1,    6, 14,   6, 16,    '  ',   [5,5,6], [5,5,6], False); // only 3 lines, can't select half ｍ
  DoTest2('',  12,1,  6,3,   14,  6,  16,  6,    '  ',   [5,5,6], [5,5,6], False); // only 3 lines, can't select half ｍ
  DoTest2('',  12,4,  5,1,   14,  5,  16,  5,    '  ',   [5,5,6,4], [5,5,6,4], False);

//abc def ghi mno pqr
//abc de___>____>123456
// check the inserted spaces are AFTER the tab(s), but in the selection (incl at the end of)
  DoTest2('half tab - only 1 tab',   8,1, 11,4,    8, 13,   8, 15,    '  ',   [8,7,12,9], [8,10,12,9], False);
  DoTest2('half tab - only 1 tab',   8,1, 12,4,    8, 14,   8, 16,    '  ',   [8,8,12,9], [8,8,12,9], False);

  DoTest2('half tab - only 1 tab',   9,1, 12,4,    9, 14,   9, 16,    '  ',   [9,8,13,10], [9,8,13,10], False);
  // NEXT: the 2nd indent goes after the 2nd tab, as the selection grew
  DoTest2('half tab - both tab',     9,1, 16,4,    9, 18,   9, 20,    '  ',   [9,8,13,10], [9,11,13,10], False);
  DoTest2('half tab - both tab',     9,1, 17,4,    9, 19,   9, 21,    '  ',   [9,9,13,10], [9,9,13,10], False);

  DoTest2('start tab - only 1 tab',   7,1, 12,4,    7, 14,   7, 16,    '  ',   [7,8,10,8], [7,8,10,8], False);
  DoTest2('start tab - both tab',     7,1, 17,4,    7, 19,   7, 21,    '  ',   [7,9,10,8], [7,9,10,8], False);

  DoTest2('before tab - only 1 tab',   6,1, 12,4,    6, 14,   6, 16,    '  ',   [6,6,8,7], [6,6,8,7], False);

// space go after tabs
// tabs go at start
  SynEdit.BlockIndent    := 2;
  SynEdit.BlockTabIndent := 1;

  // sel to short insert space before tabs
  SetLines(TestTextColumn);
  SetSelect(6,5, 10,7);
  SynEdit.CommandProcessor(TheCommand, '', nil);
  TestColSelAndText('indend',  6,5, 13,7,
    InsertIntoText(InsertIntoText(TestTextColumn, '  ', [0,0,0,0, 6,6,6]), #9, [0,0,0,0, 6,6,6])
  );

  SetLines(TestTextColumn);
  SetSelect(6,5, 11,7);
  SynEdit.CommandProcessor(TheCommand, '', nil);
  TestColSelAndText('indend',  6,5, 14,7,
    InsertIntoText(InsertIntoText(TestTextColumn, '  ', [0,0,0,0, 6,9,6]), #9, [0,0,0,0, 6,6,6])
  );

  SetLines(TestTextColumn);
  SetSelect(6,5, 20,7);
  SynEdit.CommandProcessor(TheCommand, '', nil);
  TestColSelAndText('indend',  6,5, 23,7,
    InsertIntoText(InsertIntoText(TestTextColumn, '  ', [0,0,0,0, 6,10,6]), #9, [0,0,0,0, 6,6,6])
  );


  SynEdit.Options := SynEdit.Options + [eoGroupUndo] + [eoShowSpecialChars];
  SynEdit.BlockIndent    := 2;
  SynEdit.BlockTabIndent := 0;

  DoTest2('',   5,1, 12,4,    5, 14,   5, 16,    '  ',   [5,5,6,4], [5,5,6,4], True);
  DoTest2('',   6,3, 12,1,    6, 14,   6, 16,    '  ',   [5,5,6], [5,5,6], True); // only 3 lines, can't select half ｍ
  DoTest2('',  12,1,  6,3,   14,  6,  16,  6,    '  ',   [5,5,6], [5,5,6], True); // only 3 lines, can't select half ｍ
  DoTest2('',  12,4,  5,1,   14,  5,  16,  5,    '  ',   [5,5,6,4], [5,5,6,4], True);

  (* **************************************************************************** *)

  SynEdit.Options := SynEdit.Options - [eoGroupUndo];
  SynEdit.TabWidth := 5;
  SynEdit.BlockIndent    := 2;
  SynEdit.BlockTabIndent := 0;
  TheCommand := ecBlockIndentMove;


  DoTest2('',   5,1, 12,4,    7, 14,   9, 16,    '  ',   [4,4,5,3], [4,4,5,3], False);
  DoTest2('',   6,3, 12,1,    8, 14,  10, 16,    '  ',   [4,4,5], [4,4,5], False); // only 3 lines, can't select half ｍ
  DoTest2('',  12,1,  6,3,   14,  8,  16, 10,    '  ',   [4,4,5], [4,4,5], False); // only 3 lines, can't select half ｍ
  DoTest2('',  12,4,  5,1,   14,  7,  16,  9,    '  ',   [4,4,5,3], [4,4,5,3], False);

// check the inserted spaces are AFTER the tab(s), but in the selection (incl at the end of)
  DoTest2('half tab - only 1 tab',   8,1, 10,4,   10, 12,  12, 14,    '  ',   [8,7,12,9], [8,7,12,9], False);
  // NEXT: the 2nd indent goes after the 2nd tab, as the selection moves
  DoTest2('half tab - only 1 tab',   9,1, 12,4,   11, 14,  13, 16,    '  ',   [8,7,12,9], [8,10,12,9], False);
  DoTest2('half tab - both tab',     9,1, 16,4,   11, 18,  13, 20,    '  ',   [8,7,12,9], [8,10,12,10], False);
  DoTest2('half tab - both tab',     9,1, 17,4,   11, 19,  13, 21,    '  ',   [8,7,12,9], [8,10,12,10], False);

  DoTest2('start tab - only 1 tab',   7,1, 12,4,    9, 14,  11, 16,    '  ',   [7,7,10,8], [7,7,10,8], False);
  DoTest2('start tab - both tab',     7,1, 17,4,    9, 19,  11, 21,    '  ',   [7,7,10,8], [7,7,10,8], False);

// space go after tabs
// tabs go at start
  SynEdit.BlockIndent    := 2;
  SynEdit.BlockTabIndent := 1;

  // sel to short insert space before tabs
  SetLines(TestTextColumn);
  SetSelect(6,5, 10,7);
  SynEdit.CommandProcessor(TheCommand, '', nil);
  TestColSelAndText('indend',  9,5, 13,7,
    InsertIntoText(InsertIntoText(TestTextColumn, '  ', [0,0,0,0, 6,6,6]), #9, [0,0,0,0, 6,5,6])
  );

  SetLines(TestTextColumn);
  SetSelect(6,5, 11,7);
  SynEdit.CommandProcessor(TheCommand, '', nil);
  TestColSelAndText('indend',  9,5, 14,7,
    InsertIntoText(InsertIntoText(TestTextColumn, '  ', [0,0,0,0, 6,6,6]), #9, [0,0,0,0, 6,5,6])
  );

  SetLines(TestTextColumn);
  SetSelect(11,5, 20,7);
  SynEdit.CommandProcessor(TheCommand, '', nil);
  TestColSelAndText('indend', 14,5, 23,7,
    InsertIntoText(InsertIntoText(TestTextColumn, '  ', [0,0,0,0, 11,9,11]), #9, [0,0,0,0, 11,5,11])
  );


  SynEdit.Options := SynEdit.Options + [eoGroupUndo];
  SynEdit.BlockIndent    := 2;
  SynEdit.BlockTabIndent := 0;

  DoTest2('',   5,1, 12,4,    7, 14,   9, 16,    '  ',   [4,4,5,3], [4,4,5,3], True);
  DoTest2('',   5,3, 12,1,    7, 14,   9, 16,    '  ',   [4,4,5], [4,4,5], True); // only 3 lines, can't select half ｍ
  DoTest2('',  12,1,  5,3,   14,  7,  16,  9,    '  ',   [4,4,5], [4,4,5], True); // only 3 lines, can't select half ｍ
  DoTest2('',  12,4,  5,1,   14,  7,  16,  9,    '  ',   [4,4,5,3], [4,4,5,3], True);



end;

procedure TTestBlockIndent.TestUnindentColSel;
var
  TheCommand: integer;

  procedure SetSelect(x1,y1, x2,y2: integer);
  begin
    SynEdit.LogicalCaretXY := Point(x2, y2);
    SynEdit.BlockBegin     := Point(x1, y1);
    SynEdit.BlockEnd       := Point(x2, y2);
    SynEdit.SelectionMode  := smColumn;
  end;

  procedure DoTest(AName: String; x1,y1, x2,y2: integer;
    ExpectSelStartX, ExpCaretX: integer;
    const AnXDelPos: array of integer;
    AnInsText: String; const AnXPos: array of integer;
    ATestUndoRedo: Integer = 2
    );
  var
    TestTextSub: TStringArray;
  begin
    TestTextSub := DelFromText(TestTextColumn2, AnXDelPos);
    TestTextSub := InsertIntoText(TestTextSub, AnInsText, AnXPos);

    SetLines(TestTextColumn2);
    PushBaseName('');

    SetSelect(x1,y1, x2,y2);
    DebugLn();
    DebugLn('#############################################');
    DebugLn(MyDbg(SynEdit.Text, True));Application.ProcessMessages;
    SynEdit.CommandProcessor(TheCommand, '', nil);
    DebugLn(MyDbg(SynEdit.Text, True));Application.ProcessMessages;
    TestColSelAndText('unindend',  ExpectSelStartX,y1, ExpCaretX,y2, TestTextSub );
    if ATestUndoRedo = 0 then exit;

    SynEdit.CommandProcessor(ecUndo, '', nil);
Application.ProcessMessages;
    TestColSelAndText('undo 1',  x1,y1, x2,y2, TestTextColumn2  );
    SynEdit.CommandProcessor(ecRedo, '', nil);
Application.ProcessMessages;
    TestColSelAndText('redo 1',  ExpectSelStartX,y1, ExpCaretX,y2, TestTextSub );
    if ATestUndoRedo = 1 then exit;

    SynEdit.CommandProcessor(ecUndo, '', nil);
Application.ProcessMessages;
    TestColSelAndText('undo 2',  x1,y1, x2,y2, TestTextColumn2 );
    SynEdit.CommandProcessor(ecRedo, '', nil);
Application.ProcessMessages;
    TestColSelAndText('redo 2',  ExpectSelStartX,y1, ExpCaretX,y2, TestTextSub );

    PopBaseName;
  end;


  procedure DoTest2(AName: String; x1,y1, x2,y2: integer;
    ExpectSelStartX1, ExpCaretX1, ExpectSelStartX2, ExpCaretX2: integer;
    const AnXDelPos1, AnXDelPos2: array of integer;
    AnInsText1: String; const AnXPos1: array of integer;
    AnInsText2: String; const AnXPos2: array of integer;
    ExpGroupUndo: boolean;
    SkipRedo: boolean = False // in case the selection gets 0 column width / undo does not store that
    );
  var
    TestTextSub1, TestTextSub2: TStringArray;
    undo: Integer;
  begin
    TestTextSub1 := DelFromText(TestTextColumn2, AnXDelPos1);
    TestTextSub1 := InsertIntoText(TestTextSub1, AnInsText1, AnXPos1);
    TestTextSub2 := DelFromText(TestTextSub1, AnXDelPos2);
    TestTextSub2 := InsertIntoText(TestTextSub2, AnInsText2, AnXPos2);

    for undo := 0 to 2 do begin
      DoTest(AName, x1, y1, x2, y2, ExpectSelStartX1, ExpCaretX1, AnXDelPos1, AnInsText1, AnXPos1, undo);

      SynEdit.CommandProcessor(TheCommand, '', nil);
      DebugLn(MyDbg(SynEdit.Text, True)); Application.ProcessMessages;
      TestColSelAndText('unindend 2',  ExpectSelStartX2,y1, ExpCaretX2,y2, TestTextSub2 );

      SynEdit.CommandProcessor(ecUndo, '', nil);
      if ExpGroupUndo then
        TestColSelAndText('undo 2-1',  x1,y1, x2,y2, TestTextColumn2 )
      else
        TestColSelAndText('undo 2-1',  ExpectSelStartX1,y1, ExpCaretX1,y2, TestTextSub1 );
      //
      SynEdit.CommandProcessor(ecRedo, '', nil);
      if not SkipRedo then
        TestColSelAndText('redo 2-1',  ExpectSelStartX2,y1, ExpCaretX2,y2, TestTextSub2 );


      SynEdit.CommandProcessor(ecUndo, '', nil);
      if not ExpGroupUndo then begin
        TestColSelAndText('undo 2-2a',  ExpectSelStartX1,y1, ExpCaretX1,y2, TestTextSub1 );
        SynEdit.CommandProcessor(ecUndo, '', nil);
      end;
      TestColSelAndText('undo 2-2b',  x1,y1, x2,y2, TestTextColumn2 );
      //
      SynEdit.CommandProcessor(ecRedo, '', nil);
      if not ExpGroupUndo then begin
        TestColSelAndText('redo 2-2b',  ExpectSelStartX1,y1, ExpCaretX1,y2, TestTextSub1 );
        SynEdit.CommandProcessor(ecRedo, '', nil);
      end;
      if not SkipRedo then
        TestColSelAndText('redo 2-2a',  ExpectSelStartX2,y1, ExpCaretX2,y2, TestTextSub2 );
    end;
  end;

begin

(*
1   5    10
ABCDEFGHIJKLMNOPQRSTUABCDEFGHIJKLMNOPQRSTU';
1            14
abc          def';
1    5 6     10
abc '#9#9'   def;
abc >____>   def;
ABCDEFGHIJKLMNOPQRSTUABCDEFGHIJKLMNOPQRSTU';
*)
  SynEdit.Options := SynEdit.Options - [eoGroupUndo] + [eoShowSpecialChars];
  SynEdit.TabWidth := 5;
  SynEdit.BlockIndent    := 2;
  SynEdit.BlockTabIndent := 0;
  TheCommand := ecBlockUnindent;

  DoTest2('',  4,1, 15,4,   4, 15,   4, 15, [0,0, 12,2, 8,2], [0,0, 10,2, 6,2], '',[], '    ',[0,0,6], False); // no unindent in last row / no shrink sel
                                        // 12 as 4 spaces where added
  DoTest2('',  4,1, 12,3,   4, 10,   4, 12, [0,0, 12,2, 8,2], [0,0, 10,2, 6,2], '',[], '    ',[0,0,6], False);

  // at end of tab / don't del outside select
  DoTest2('',  4,1,  7,3,   4, 9,   4, 7, [0,0, 9,2, 6,1], [0,0, 7,2, 7,2], '   ',[0,0,6], '',[], False);
  DoTest2('', 11,2,  4,3,   9, 4,   7, 4, [0,0, 9,2, 6,1], [0,0, 7,2, 7,2], '   ',[0,0,6], '',[], False);

  SynEdit.BlockIndent    := 8;
  DoTest2('', 15,2,  4,3,   7, 4,   5, 4, [0,0, 6,8, 6,4], [0,0, 4,2, 4,2], '',[], '',[], False);
  // at end of tab / don't del outside select
  DoTest2('', 13,2,  4,3,   5, 4,   4, 4, [0,0, 5,8, 5,4], [0,0, 4,1, 4,1], '',[], '',[], False, True);


  SynEdit.BlockIndent    := 1;
  SynEdit.BlockTabIndent := 1;
  DoTest2('', 15,2,  4,3,  12, 4,   9, 4, [0,0, 11,3, 4,4], [0,0, 8,3, 4,2], #9'',[0,0,4], '',[], False);

  SynEdit.TabWidth := 1;
  SynEdit.BlockIndent    := 0;
  SynEdit.BlockTabIndent := 2;
  DoTest2('', 11,3,  4,3,   9, 4,   7, 4, [0,0, 0,0, 4,2], [0,0, 0,0, 4,2], '',[], '',[], False);
  SynEdit.BlockTabIndent := 3;
  DoTest2('', 11,3,  4,3,   8, 4,   5, 4, [0,0, 0,0, 4,3], [0,0, 0,0, 4,3], '',[], '',[], False);


  SynEdit.Options := SynEdit.Options + [eoGroupUndo] + [eoShowSpecialChars];
  SynEdit.TabWidth := 5;
  SynEdit.BlockIndent    := 2;
  SynEdit.BlockTabIndent := 0;

  DoTest2('',  4,1, 12,3,   4, 10,   4, 12, [0,0, 12,2, 8,2], [0,0, 10,2, 6,2], '',[], '    ',[0,0,6], True);

  (* **************************************************************************** *)

  SynEdit.Options := SynEdit.Options - [eoGroupUndo] + [eoShowSpecialChars];
  SynEdit.TabWidth := 5;
  SynEdit.BlockIndent    := 2;
  SynEdit.BlockTabIndent := 0;
  TheCommand := ecBlockUnindentMove;

  DoTest2('', 12,2,  11,3,  10,13,   8,11, [0,0, 10,2, 6,2], [0,0, 8,2, 8,2], '    ',[0,0,6], '',[], False);

end;

initialization

  RegisterTest(TTestBlockIndent); 
end.

