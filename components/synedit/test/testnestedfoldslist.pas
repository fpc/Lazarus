unit TestNestedFoldsList;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, TestBase, testregistry, SynEdit, SynHighlighterPas,
  SynEditHighlighterFoldBase, LazLoggerBase, TestHighlightPas;

type

  { TTestNestedFoldsList }

  TTestNestedFoldsList = class(TTestBaseHighlighterPas)
  private
    FNestList: TLazSynEditNestedFoldsList;
  protected
    procedure TearDown; override;
    procedure ReCreateEdit; reintroduce;
    procedure ReCreateList;
    function TestText1: TStringArray;
  published
    procedure Test1;
  end;


implementation

{ TTestNestedFoldsList }

procedure TTestNestedFoldsList.TearDown;
begin
  FreeAndNil(FNestList);
  inherited TearDown;
end;

procedure TTestNestedFoldsList.ReCreateEdit;
begin
  FreeAndNil(FNestList);
  inherited ReCreateEdit;
  ReCreateList;
end;

procedure TTestNestedFoldsList.ReCreateList;
begin
  FreeAndNil(FNestList);
  FNestList := TLazSynEditNestedFoldsList.Create(SynEdit.TextBuffer, PasHighLighter);
  FNestList.ResetFilter;
  FNestList.FoldGroup := 0;
  FNestList.FoldFlags := [sfbIncludeDisabled];
  FNestList.IncludeOpeningOnLine := True;
end;

function TTestNestedFoldsList.TestText1: TStringArray;
begin
  SetLength(Result, 26);
  Result[0]  := 'program Foo;';
  Result[1]  := 'begin';
  Result[2]  := '  while FTheDebugger.DebugProcessRunning and not(FTheDebugger.State in [dsError, dsDestroying]) do';
  Result[3]  := '  begin';
  Result[4]  := '';
  Result[5]  := '    {$IFDEF DBG_ASYNC_WAIT}';
  Result[6]  := '    if GotPrompt > 0 then begin';
  Result[7]  := '    {$ENDIF}';
  Result[8]  := '';
  Result[9]  := '      if (S = ''(gdb) '') or';
  Result[10] := '         ( (S = '') and FDidKillNow )';
  Result[11] := '      then';
  Result[12] := '        {$IFDEF DBG_ASYNC_WAIT}';
  Result[13] := '        begin';
  Result[14] := '          if (not FGotStopped) and (not FDidKillNow) and (GotPrompt = 0) then';
  Result[15] := '            GotPrompt := 1';
  Result[16] := '          else';
  Result[17] := '            break;';
  Result[18] := '        end;';
  Result[19] := '        {$ELSE}';
  Result[20] := '        Break;';
  Result[21] := '        {$ENDIF}';
  Result[22] := '';
  Result[23] := '    end;';
  Result[24] := '  end;';
  Result[25] := '';
end;

procedure TTestNestedFoldsList.Test1;
var
  start, start2, nth, grp, i, j: Integer;
begin
  // this test currently only checks that no ASSERT is triggered

//  PushBaseName('');
  for start := 0 to 24 do begin
  for start2 := 0 to 3  do begin
  for nth := 1 to 4 do begin
  for grp := 0 to 1 do begin
    ReCreateEdit;
    //PopPushBaseName('Text 1 -- [cfbtBeginEnd..cfbtNone], [], 0');
    SetLines(TestText1);
    EnableFolds([cfbtBeginEnd..cfbtNone], []);
    FNestList.FoldGroup := grp;

    for i := start to 25 do begin
      FNestList.Line := i;
      if i mod nth = 0 then
        for j := start2*2 to FNestList.Count - 1 do begin
          FNestList.HLNode[j];
          FNestList.NodeEndLine[j]
        end;
    end;

    ReCreateEdit;
    //PopPushBaseName('Text 1 -- [cfbtBeginEnd..cfbtNone], [], 0');
    SetLines(TestText1);
    EnableFolds([cfbtBeginEnd..cfbtNone], []);
    FNestList.FoldGroup := grp;
    FNestList.FoldFlags := [];

    for i := start to 25 do begin
      FNestList.Line := i;
      if i mod nth = 0 then
        for j := start2*2 to FNestList.Count - 1 do begin
          FNestList.HLNode[j];
          FNestList.NodeEndLine[j]
        end;
    end;


  end;
  end;
  end;
  end;

end;


initialization

  RegisterTest(TTestNestedFoldsList);

end.

