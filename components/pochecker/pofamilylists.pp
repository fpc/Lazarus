unit PoFamilyLists;

{$mode objfpc}{$H+}

interface

Uses
  Classes, SysUtils, ContNrs, LCLProc, LazFileUtils,
  //{$IFDEF UNIX}{$IFNDEF DisableCWString}, cwstring{$ENDIF}{$ENDIF},
  PoFamilies, PoCheckerConsts;

const
  langAll = '*';


type

  { TPoFamilyList }

  TPoFamilyList = class
  private
    FLangID: TLangID;
    FList: TFPObjectList;
    FOnTestEnd: TTestEndEvent;
    FOnTestStart: TTestStartEvent;
    FPoFamilyStats: TPoFamilyStats;
    FTestOptions: TPoTestOptions;
    FTestTypes: TPoTestTypes;
    function GetItem(Index: Integer): TPoFamily;
    //procedure SetItem(Index: Integer; AValue: TPoFamily);
  protected
    procedure DoTestStart(const ATestName, APoFileName: String);
    procedure DoTestEnd(const ATestName: String; const ErrorCount: Integer);
  public
    constructor Create(AMasterList: TStrings; ALangID: TLangID; out Msg: String);
    destructor Destroy; override;
    procedure Add(PoFamily: TPofamily);
    function Count: Integer;
    procedure RunTests(out ErrorCount, WarningCount, TotalTranslatedCount, TotalUntranslatedCount, TotalFuzzyCount: Integer; ErrorLog: TStrings);
    property Items[Index: Integer]: TPoFamily read GetItem; // write SetItem;
    property PoFamilyStats: TPoFamilyStats read FPoFamilyStats;
    property TestTypes: TPoTestTypes read FTestTypes write FTestTypes;
    property TestOptions: TPoTestOptions read FTestOptions write FTestOptions;
    property OnTestStart: TTestStartEvent read FOnTestStart write FOnTestStart;
    property OnTestEnd: TTestEndEvent read FOnTestEnd write FOnTestEnd;
  end;

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

constructor TPoFamilyList.Create(AMasterList: TStrings; ALangID: TLangID; out Msg: String);
var
  i: Integer;
  MasterName, ChildName: String;
  APoFamily: TPoFamily;
begin
  FList := TFPObjectList.Create(True);
  Msg := '';
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
        APoFamily := TPoFamily.Create(MasterName, ChildName);
        Add(APoFamily);
      end
      else
        Msg := Msg + LineEnding +  Format('"%s"',[ChildName]);
    end
    else
      Msg := Msg + LineEnding +  Format('"%s"',[MasterName]);
  end;
end;

destructor TPoFamilyList.Destroy;
begin
  //debugln('TPoFamilyList.Destroy: FList.Count = ',DbgS(FList.Count));
  PoFamilyStats.Free;
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

procedure TPoFamilyList.RunTests(out ErrorCount, WarningCount, TotalTranslatedCount, TotalUntranslatedCount, TotalFuzzyCount: Integer;
  ErrorLog: TStrings);
var
  Index, ThisErrorCount, ThisWarningCount: Integer;
  ThisTranslatedCount, ThisUntranslatedCount, ThisFuzzyCount: Integer;
  PoFamily: TPoFamily;
  //ThisLog: TStringList;
begin
  if (FLangID = lang_all) then
    Include(FTestOptions,ptoFindAllChildren)
  else
    Exclude(FTestOptions,ptoFindAllChildren);
  ErrorLog.Clear;
  //ThisLog := TStringList.Create;
  ErrorCount := NoError;
  WarningCount := NoError;
  TotalTranslatedCount := 0;
  TotalUntranslatedCount := 0;
  TotalFuzzyCount := 0;
  FPoFamilyStats.Clear;
  try
    for Index := 0 to FList.Count - 1 do
    begin
      PoFamily := GetItem(Index);
      PoFamily.OnTestStart := FOnTestStart;
      PoFamily.OnTestEnd := FOnTestEnd;
      PoFamily.TestTypes := FTesttypes;
      PoFamily.TestOptions := FTestOptions;
      PoFamily.RunTests(ThisErrorCount, ThisWarningCount, ThisTranslatedCount, ThisUntranslatedCount, ThisFuzzyCount, ErrorLog);
      PoFamily.PoFamilyStats.AddItemsTo(FPoFamilyStats);
      ErrorCount := ErrorCount + ThisErrorCount;
      WarningCount := WarningCount + ThisWarningCount;
      TotalTranslatedCount := TotalTranslatedCount + ThisTranslatedCount;
      TotalUntranslatedCount := TotalUntranslatedCount + ThisUntranslatedCount;
      TotalFuzzyCount := TotalFuzzyCount + ThisFuzzyCount;
      //ThisLog.AddStrings(ErrorLog)

    end;

  finally
    //ThisLog.Free;
  end;
end;

end.

