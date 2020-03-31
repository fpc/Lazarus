unit PoFamilyLists;

{$mode objfpc}{$H+}

interface

Uses
  Classes, SysUtils, ContNrs, LCLProc, LazFileUtils,
  //{$IFDEF UNIX}{$IFNDEF DisableCWString}, cwstring{$ENDIF}{$ENDIF},
  PoFamilies, PoCheckerConsts, Translations;

type

  { TPoFamilyList }

  TPoFamilyList = class
  private
    FLangID: TLangID;
    FList: TFPObjectList;
    FOnTestEnd: TTestEndEvent;
    FOnTestStart: TTestStartEvent;
    FPoFamilyStats: TPoFamilyStats;
    FPoFamilyListStats: TTranslationStatistics;
    FTestTypes: TPoTestTypes;
    function GetItem(Index: Integer): TPoFamily;
    //procedure SetItem(Index: Integer; AValue: TPoFamily);
  protected
    procedure DoTestStart(const ATestName, APoFileName: String);
    procedure DoTestEnd(const ATestName: String; const ErrorCount: Integer);
  public
    InfoLog: TStringList;
    StatLog: TStringList;
    DupLog: TStringList;
    constructor Create(AMasterList: TStrings; ALangID: TLangID);
    destructor Destroy; override;
    procedure Add(PoFamily: TPofamily);
    function Count: Integer;
    function StatPerc(Value: Integer): Double;
    procedure RunTests;
    property LangID: TLangID read FLangID;
    property Items[Index: Integer]: TPoFamily read GetItem; // write SetItem;
    property PoFamilyStats: TPoFamilyStats read FPoFamilyStats;
    property PoFamilyListStats: TTranslationStatistics read FPoFamilyListStats;
    property TestTypes: TPoTestTypes read FTestTypes write FTestTypes;
    property OnTestStart: TTestStartEvent read FOnTestStart write FOnTestStart;
    property OnTestEnd: TTestEndEvent read FOnTestEnd write FOnTestEnd;
  end;

var
  PoFamilyList: TPoFamilyList;

implementation

{ TPoFamilyList }

function TPoFamilyList.GetItem(Index: Integer): TPoFamily;
begin
  Result := TPoFamily(FList.Items[Index]);
end;


procedure TPoFamilyList.DoTestStart(const ATestName, APoFileName: String);
begin
  if Assigned(FOnTestStart) then FOnTestStart(ATestName, APoFileName);
end;

procedure TPoFamilyList.DoTestEnd(const ATestName: String; const ErrorCount: Integer);
begin
  if Assigned(FOnTestEnd) then FOnTestEnd(ATestName, ErrorCount);
end;

constructor TPoFamilyList.Create(AMasterList: TStrings; ALangID: TLangID);
var
  i: Integer;
  MasterName, ChildName, MasterMsg, ChildMsg: String;
  APoFamily: TPoFamily;
begin
  InfoLog := TStringList.Create;
  StatLog := TStringList.Create;
  DupLog := TStringList.Create;
  FList := TFPObjectList.Create(True);
  MasterMsg := '';
  ChildMsg := '';
  FPoFamilyStats := TPoFamilyStats.Create;
  FLangID := ALangID;
  for i :=  0 to AMasterList.Count - 1 do
  begin
    MasterName := AMasterList[i];
    ChildName := '';
    if FileExistsUtf8(MasterName) then
    begin
      if (ALangID <> lang_all) then
        ChildName := ChangeFileExt(MasterName, '.' + LanguageAbbr[ALangID] + '.po');
      //debugln('TPoFamilyList.Create: i = ',DbgS(i),' Adding TPoFamily.Create(''',ExtractFileName(MasterName),
      //        ''',',ExtractFileName(ChildName),''')');
      if (ALangID = lang_all) or FileExistsUtf8(ChildName) then
      begin
        APoFamily := TPoFamily.Create(MasterName, ChildName, ALangID);
        Add(APoFamily);
      end
      else
        ChildMsg := ChildMsg + Format('"%s"',[ChildName]) + LineEnding;
    end
    else
      MasterMsg := MasterMsg + Format('"%s"',[MasterName]) + LineEnding;
  end;
  if MasterMsg <> '' then
    MasterMsg := MasterMsg + LineEnding;
  if ChildMsg <> '' then
    ChildMsg := ChildMsg + LineEnding;
  MasterMsg := MasterMsg + ChildMsg;
  if MasterMsg <> '' then
    InfoLog.AddText(Format(sFilesNotFoundAndRemoved,[MasterMsg]));
end;

destructor TPoFamilyList.Destroy;
begin
  //debugln('TPoFamilyList.Destroy: FList.Count = ',DbgS(FList.Count));
  PoFamilyStats.Free;
  InfoLog.Free;
  StatLog.Free;
  DupLog.Free;
  FList.Free;
  inherited Destroy;
end;

procedure TPoFamilyList.Add(PoFamily: TPofamily);
begin
  FList.Add(PoFamily);
end;

function TPoFamilyList.Count: Integer;
begin
  Result := FList.Count;
end;

function TPoFamilyList.StatPerc(Value: Integer): Double;
begin
  Result := 100 * Value / (PoFamilyListStats.Translated + PoFamilyListStats.Untranslated + PoFamilyListStats.Fuzzy);
end;

procedure TPoFamilyList.RunTests;
var
  ErrorCount, NonFuzzyErrorCount, WarningCount: Integer;
  Index, ThisErrorCount, ThisNonFuzzyErrorCount, ThisWarningCount: Integer;
  ThisTranslatedCount, ThisUntranslatedCount, ThisFuzzyCount: Integer;
  PoFamily: TPoFamily;
begin
  ErrorCount := NoError;
  NonFuzzyErrorCount := NoError;
  WarningCount := NoError;
  FPoFamilyListStats.Translated := 0;
  FPoFamilyListStats.Untranslated := 0;
  FPoFamilyListStats.Fuzzy := 0;
  FPoFamilyStats.Clear;
  for Index := 0 to FList.Count - 1 do
  begin
    PoFamily := GetItem(Index);
    PoFamily.OnTestStart := FOnTestStart;
    PoFamily.OnTestEnd := FOnTestEnd;
    PoFamily.TestTypes := FTesttypes;
    PoFamily.RunTests(ThisErrorCount, ThisNonFuzzyErrorCount, ThisWarningCount, ThisTranslatedCount, ThisUntranslatedCount, ThisFuzzyCount, InfoLog, StatLog, DupLog);
    PoFamily.PoFamilyStats.AddItemsTo(FPoFamilyStats);
    ErrorCount := ErrorCount + ThisErrorCount;
    NonFuzzyErrorCount := NonFuzzyErrorCount + ThisNonFuzzyErrorCount;
    WarningCount := WarningCount + ThisWarningCount;
    FPoFamilyListStats.Translated := PoFamilyListStats.Translated + ThisTranslatedCount;
    FPoFamilyListStats.Untranslated := PoFamilyListStats.Untranslated + ThisUntranslatedCount;
    FPoFamilyListStats.Fuzzy := PoFamilyListStats.Fuzzy + ThisFuzzyCount;
  end;

  if NonFuzzyErrorCount > 0 then
    InfoLog.Add(Format(sTotalErrorsNonFuzzy, [ErrorCount, NonFuzzyErrorCount]))
  else
    InfoLog.Add(Format(sTotalErrors, [ErrorCount]));

  if FLangID <> lang_all then
  begin
    InfoLog.Add(Format(sTotalUntranslatedStrings, [IntToStr(PoFamilyListStats.Untranslated)]));
    InfoLog.Add(Format(sTotalFuzzyStrings, [IntToStr(PoFamilyListStats.Fuzzy)]));
    InfoLog.Add('');
    InfoLog.Add(Format(sTotalTranslatedStrings, [IntToStr(PoFamilyListStats.Translated), StatPerc(PoFamilyListStats.Translated)]));

    StatLog.Add(Format(sTotalUntranslatedStrings, [IntToStr(PoFamilyListStats.Untranslated)]));
    StatLog.Add(Format(sTotalFuzzyStrings, [IntToStr(PoFamilyListStats.Fuzzy)]));
    StatLog.Add('');
    StatLog.Add(Format(sTotalTranslatedStrings, [IntToStr(PoFamilyListStats.Translated), StatPerc(PoFamilyListStats.Translated)]));
  end;

  DupLog.Add(Format(sTotalWarnings, [WarningCount]));
end;

initialization
  PoFamilyList := nil;

end.

