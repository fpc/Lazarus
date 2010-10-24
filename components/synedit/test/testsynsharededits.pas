unit TestSynSharedEdits;
(* Test multiply edits sharing a textbuffer
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, TestBase, LCLProc,
  SynEdit, SynEditMarks, SynEditKeyCmds;

type

  { TTestSynSharedEdits }

  TTestSynSharedEdits = class(TTestBase)
  private
    FSynEdits : Array of TTestSynEdit;
  protected
    //procedure SetUp; override;
    procedure TearDown; override;
    procedure ReCreateEdit; reintroduce; overload;
    procedure ReCreateEdit(Count: Integer); overload;
    procedure SelectEdit(Index: Integer);
    function TestText1: TStringArray;
    procedure CheckMarks(Name: String; EditIndex: Integer; MarkList: Array of Integer);
    function AddMark(EditIndex: Integer; Y, X: Integer) : TSynEditMark;
  published
    procedure TestMarks;
    procedure TestBookMarks;
  end;

implementation


{ TTestSynSharedEdits }

procedure TTestSynSharedEdits.TearDown;
var
  i: Integer;
begin
  FSynEdit := nil;
  for i := 0 to length(FSynEdits) - 1 do
    FSynEdits[i].Free;
  SetLength(FSynEdits, 0);
  inherited TearDown;
end;

procedure TTestSynSharedEdits.ReCreateEdit;
begin
  ReCreateEdit(1);
end;

procedure TTestSynSharedEdits.ReCreateEdit(Count: Integer);
var
  i: Integer;
begin
  for i := 0 to length(FSynEdits) - 1 do
    FSynEdits[i].Free;

  SetLength(FSynEdits, Count);
  Form.Height := 600;
  Form.Width :=  500;
  for i := 0 to Count - 1 do begin
    FSynEdits[i] := TTestSynEdit.Create(Form);
    FSynEdits[i].Parent := Form;
    FSynEdits[i].Top := 200 * i;
    FSynEdits[i].Left := 0;
    FSynEdits[i].Width:= 500;
    FSynEdits[i].Height := 200; // FSynEdits[i].Font.Height * 20 + 2;
  end;

  FSynEdit := FSynEdits[0];
end;

procedure TTestSynSharedEdits.SelectEdit(Index: Integer);
begin
  FSynEdit := FSynEdits[Index];
end;

function TTestSynSharedEdits.TestText1: TStringArray;
begin
  SetLength(Result, 8);
  Result[0] := 'abc';
  Result[1] := 'def 123;';
  Result[2] := 'xyz 098765';
  Result[3] := '';
  Result[4] := '1';
  Result[5] := '    mno...';
  Result[6] := '  ABCDEF';
  Result[7] := '  321';
end;

procedure TTestSynSharedEdits.CheckMarks(Name: String; EditIndex: Integer;
  MarkList: array of Integer);
var
  m: TSynEditMark;
  i: Integer;
begin
  AssertEquals(Name + ' Edit: '+IntToStr(EditIndex)+' Count: ', length(MarkList) div 2, FSynEdits[EditIndex].Marks.Count);

  for i := 0 to (length(MarkList) div 2) - 1 do begin
    m := FSynEdits[EditIndex].Marks[i];
    AssertEquals(Name + ' Edit: '+IntToStr(EditIndex)+' Mark( '+IntToStr(i)+' ) Line: ',    MarkList[i*2+0], m.Line);
    AssertEquals(Name + ' Edit: '+IntToStr(EditIndex)+' Mark( '+IntToStr(i)+' ) Column: ',  MarkList[i*2+1], m.Column);
    AssertEquals(Name + ' Edit: '+IntToStr(EditIndex)+' Mark( '+IntToStr(i)+' ) IndexOf: ', i,               FSynEdits[EditIndex].Marks.IndexOf(m));
  end;
end;

function TTestSynSharedEdits.AddMark(EditIndex: Integer; Y, X: Integer): TSynEditMark;
begin
  Result := TSynEditMark.Create(SynEdit);
  Result.Line := Y;
  Result.Column := X;
  FSynEdits[EditIndex].Marks.Add(Result);
end;

procedure TTestSynSharedEdits.TestMarks;
begin
  ReCreateEdit(2);

  // default setting is shared marklist
  FSynEdits[1].ShareOptions := [eosShareMarks];
  FSynEdits[1].ShareTextBufferFrom(FSynEdits[0]);
  SetLines(TestText1);

  CheckMarks('empty', 0, []);
  CheckMarks('empty', 1, []);

  AddMark(0, 3,2);
  CheckMarks('3/2', 0, [ 3,2 ]);
  CheckMarks('3/2', 1, [ 3,2 ]);

  AddMark(1, 5,2);
  CheckMarks('3/2 - 5/1', 0, [ 3,2,   5,2 ]);
  CheckMarks('3/2 - 5/1', 1, [ 3,2,   5,2 ]);



  ReCreateEdit(2);

  // setting is not shared marklist
  FSynEdits[1].ShareOptions := [];
  FSynEdits[1].ShareTextBufferFrom(FSynEdits[0]);
  SetLines(TestText1);

  CheckMarks('empty', 0, []);
  CheckMarks('empty', 1, []);

  AddMark(0, 3,2);
  CheckMarks('3/2', 0, [ 3,2 ]);
  CheckMarks('3/2', 1, []);

  AddMark(1, 5,2);
  CheckMarks('3/2 - 5/1', 0, [ 3,2 ]);
  CheckMarks('3/2 - 5/1', 1, [ 5,2 ]);


end;

procedure TTestSynSharedEdits.TestBookMarks;
  procedure CheckBMarks(Name: String; EditIndex: Integer; MarkList: Array of Integer);
  var
    m: TSynEditMark;
    l: Array of Integer;
    i, j: Integer;
  begin
    Name := BaseTestName + Name;
    AssertEquals(Name + ' Edit: '+IntToStr(EditIndex)+' Count: ', length(MarkList) div 2, FSynEdits[EditIndex].Marks.Count);

    SetLength(l, FSynEdits[EditIndex].Lines.Count);
    for i := 0 to length(l)-1 do
      l[i] := 0;

    for i := 0 to (length(MarkList) div 2) - 1 do begin
      l[MarkList[i*2+0]] := l[MarkList[i*2+0]] + 1;
      m := FSynEdits[EditIndex].Marks[i];
      AssertEquals(Name + ' Edit: '+IntToStr(EditIndex)+' Mark( '+IntToStr(i)+' ) Line: ',    MarkList[i*2+0], m.Line);
      AssertEquals(Name + ' Edit: '+IntToStr(EditIndex)+' Mark( '+IntToStr(i)+' ) BookNum: ',  MarkList[i*2+1], m.BookmarkNumber);
      AssertEquals(Name + ' Edit: '+IntToStr(EditIndex)+' Mark( '+IntToStr(i)+' ) IndexOf: ', i,               FSynEdits[EditIndex].Marks.IndexOf(m));
    end;

    for i := 0 to length(l)-1 do begin
      j := 0;
      if FSynEdits[EditIndex].Marks.Line[i] <> nil then
        j := FSynEdits[EditIndex].Marks.Line[i].Count;
      AssertEquals(Name + ' Edit: '+IntToStr(EditIndex)+' Count in line '+IntToStr(i)+': ',    l[i], j);
    end;

  end;

var
  e1, e2, e3: Integer;
begin
  {%region edits already shared }
  ReCreateEdit(2);
  // default setting is shared marklist
  FSynEdits[1].ShareOptions := [eosShareMarks];
  FSynEdits[1].ShareTextBufferFrom(FSynEdits[0]);
  SetLines(TestText1);
  PushBaseName('2 shared');

  PushBaseName('');
  for e1 := 0 to 1 do begin
    for e2 := 0 to 1 do begin
      PopPushBaseName('e1='+IntToStr(e1) + ' e2='+IntToStr(e2) );
      // Set Mark
      FSynEdits[e1].SetBookMark(1, 2, 4);
      CheckBMarks('bm 1', 0, [ 4, 1  ]);
      CheckBMarks('bm 1', 1, [ 4, 1  ]);

      // goto
      SelectEdit(0);
      SynEdit.GotoBookMark(1);
      TestIsCaret('Goto1 /0', 2, 4);
      SelectEdit(1);
      SynEdit.GotoBookMark(1);
      TestIsCaret('Goto1 /1', 2, 4);

      // clear
      FSynEdits[e2].ClearBookMark(1);
      CheckBMarks('bm clear', 0, [] );
      CheckBMarks('bm clear', 1, [] );
    end;
  end;
  PopBaseName;

  PushBaseName('');
  for e1 := 0 to 1 do begin
    for e2 := 0 to 1 do begin
      for e3 := 0 to 1 do begin
        PopPushBaseName('e1='+IntToStr(e1) + ' e2='+IntToStr(e2) + ' e3='+IntToStr(e3));
        FSynEdits[e1].ClearBookMark(1);
        FSynEdits[e1].ClearBookMark(2);
        // set 1 on ed1
        FSynEdits[e1].SetBookMark(1, 2, 4);
        CheckBMarks('bm set 1', 0, [ 4, 1  ]);
        CheckBMarks('bm set 1', 1, [ 4, 1  ]);

        // set 2 on ed2
        FSynEdits[e2].SetBookMark(2, 2, 5);
        CheckBMarks('bm set 2', 0, [ 4,1,   5,2  ]);
        CheckBMarks('bm set 2', 1, [ 4,1,   5,2  ]);

        // move 1 on ed3
        FSynEdits[e3].SetBookMark(1, 2, 3);
        CheckBMarks('bm move 1', 0, [ 3,1,   5,2  ]);
        CheckBMarks('bm move 1', 1, [ 3,1,   5,2  ]);

        // move 2 over 1 on ed3
        FSynEdits[e3].SetBookMark(2, 3, 3);
        CheckBMarks('bm move 2 over 1', 0, [ 3,2  ]);
        CheckBMarks('bm move 2 over 1', 1, [ 3,2  ]);
      end;
    end;
  end;
  PopBaseName;
  {%endregion}


  {%region edits shared after bookmark set }
  ReCreateEdit(2);
  // default setting is shared marklist
  FSynEdits[1].ShareOptions := [eosShareMarks];
  SetLines(TestText1);
  PopPushBaseName('2 shared after bm set');

  FSynEdits[0].SetBookMark(1, 2, 4);
  CheckBMarks('bm 1', 0, [ 4, 1  ]);
  CheckBMarks('bm 1', 1, [  ]);

  FSynEdits[1].ShareTextBufferFrom(FSynEdits[0]);

  CheckBMarks('bm shared 1', 0, [ 4, 1  ]);
  CheckBMarks('bm shared 1', 1, [ 4, 1  ]);

  FSynEdits[1].ClearBookMark(1);

  CheckBMarks('clear', 0, []);
  CheckBMarks('clear', 1, []);

  // unshare edits
  FSynEdits[0].SetBookMark(1, 2, 4);
  CheckBMarks('bm 1', 0, [ 4, 1  ]);
  CheckBMarks('bm 1', 1, [ 4, 1  ]);

  FSynEdits[1].UnShareTextBuffer;
  CheckBMarks('unshare 1', 0, [ 4, 1  ]);
  CheckBMarks('unshare 1', 1, [  ]);

  FSynEdits[1].ClearBookMark(1);
  CheckBMarks('unshare 1', 0, [ 4, 1  ]);  // not affected
  CheckBMarks('unshare 1', 1, [  ]);

  FSynEdits[0].ClearBookMark(1);
  CheckBMarks('unshare 1', 0, [  ]);
  CheckBMarks('unshare 1', 1, [  ]);
  {%endregion}
end;

initialization

  RegisterTest(TTestSynSharedEdits); 
end.

