unit TestBlockIndent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, TestBase, math,
  SynEdit, SynEditTypes, SynEditTextTrimmer, SynEditKeyCmds,
  LCLType, LCLProc;

type

  { TTestBlockIndent }

  TTestBlockIndent = class(TTestBase)
  protected
    function TestTextSpace: TStringArray;
    function TestTextTabs: TStringArray;
    function ReplaceIndent(AText: TStringArray; AFirstLine: Integer;
                           ANewIndents: Array of String): TStringArray;
    function AddIndent(AText: TStringArray; AFirstLine, ALastLine, ASpaceCount: Integer): TStringArray;
    function DelIndent(AText: TStringArray; AFirstLine, ALastLine, ASpaceCount: Integer): TStringArray;
    procedure TestSelAndText(Name: String; LogX1, LogY1, LogX2, LogY2: Integer;
                             ExpLines: Array of String;
                             SelIsForward: Boolean = True);
    //procedure DoTest(X1,Y1, X2,Y2: Boolean; CountIndent: Integer;
    //                 ExpX1, ExpY1, ExpX2, ExpY2: Integer;
    //                 ExpText: A
  published
    procedure TestIndent;
    procedure TestUnIndent;
    procedure TestIndentWithTab;
    procedure TestUnIndentWithTab;
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
  ANewIndents: Array of String): TStringArray;
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

procedure TTestBlockIndent.TestSelAndText(Name: String; LogX1, LogY1, LogX2, LogY2: Integer;
  ExpLines: array of String; SelIsForward: Boolean);
begin
  if SelIsForward then
    TestIsCaretAndSel(Name, LogX1, LogY1, LogX2, LogY2)
  else
    TestIsCaretAndSelBackward(Name, LogX1, LogY1, LogX2, LogY2);
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

initialization

  RegisterTest(TTestBlockIndent); 
end.

