unit TestHighlightFoldBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, TestBase, Forms, LCLProc,
  SynEdit, SynEditTypes, SynEditHighlighterFoldBase;

type

  // used by Fold / MarkupWord

  { TTestBaseHighlighterPas }

  { TTestBaseHighlighterFoldBase }

  TTestBaseHighlighterFoldBase = class(TTestBase)
  protected
    FTheHighLighter: TSynCustomFoldHighlighter;
    function CreateTheHighLighter: TSynCustomFoldHighlighter; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ReCreateEdit; reintroduce;

    procedure CheckFoldOpenCounts(Name: String; Expected: Array of Integer);
    procedure CheckFoldInfoCounts(Name: String; Filter: TSynFoldActions; Expected: Array of Integer);
    procedure CheckFoldInfoCounts(Name: String; Filter: TSynFoldActions; Group: Integer; Expected: Array of Integer);

    function FoldActionsToString(AFoldActions: TSynFoldActions): String;
  end;


implementation

{ TTestBaseHighlighterFoldBase }

procedure TTestBaseHighlighterFoldBase.SetUp;
begin
  FTheHighLighter := nil;
  inherited SetUp;
end;

procedure TTestBaseHighlighterFoldBase.TearDown;
begin
  if Assigned(SynEdit) then
    SynEdit.Highlighter := nil;
  FreeAndNil(FTheHighLighter);
  inherited TearDown;
end;

procedure TTestBaseHighlighterFoldBase.ReCreateEdit;
begin
  if Assigned(SynEdit) then
    SynEdit.Highlighter := nil;
  FreeAndNil(FTheHighLighter);
  inherited ReCreateEdit;
  FTheHighLighter := CreateTheHighLighter;
  SynEdit.Highlighter := FTheHighLighter;
end;

procedure TTestBaseHighlighterFoldBase.CheckFoldOpenCounts(Name: String;
  Expected: array of Integer);
var
  i: Integer;
begin
  for i := 0 to high(Expected) do
    AssertEquals(Name + 'OpenCount Line OLD='+IntToStr(i),  Expected[i], FTheHighLighter.FoldOpenCount(i));

  for i := 0 to high(Expected) do
    AssertEquals(Name + 'OpenCount Line='+IntToStr(i),  Expected[i], FTheHighLighter.FoldBlockOpeningCount(i));
end;

procedure TTestBaseHighlighterFoldBase.CheckFoldInfoCounts(Name: String;
  Filter: TSynFoldActions; Expected: array of Integer);
begin
  CheckFoldInfoCounts(Name, Filter, 0, Expected);
end;

procedure TTestBaseHighlighterFoldBase.CheckFoldInfoCounts(Name: String;
  Filter: TSynFoldActions; Group: Integer; Expected: array of Integer);
var
  i: Integer;
  l: TLazSynFoldNodeInfoList;
begin
  for i := 0 to high(Expected) do begin
    l := FTheHighLighter.FoldNodeInfo[i];
    AssertEquals(Name + 'InfoCount(Ex) Line='+IntToStr(i),
                 Expected[i],
                 l.CountEx(Filter, Group));
    l.ClearFilter;
    l.ActionFilter := Filter;
    l.GroupFilter := Group;
    AssertEquals(Name + 'InfoCount Line='+IntToStr(i),
                 Expected[i],
                 FTheHighLighter.FoldNodeInfo[i].Count);
  end;
end;

function TTestBaseHighlighterFoldBase.FoldActionsToString(AFoldActions: TSynFoldActions): String;
var
  s: string;
  i: TSynFoldAction;
begin
  Result:='';
  for i := low(TSynFoldAction) to high(TSynFoldAction) do
    if i in AFoldActions then begin
      WriteStr(s, i);
      Result := Result + s + ',';
    end;
  if Result <> '' then SetLength(Result, Length(Result)-1);
end;

end.

