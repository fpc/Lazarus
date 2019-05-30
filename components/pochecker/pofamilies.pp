unit PoFamilies;

{ $define DebugSimplePoFiles}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ContNrs, Math,
  // LCL
  LCLProc,
  // LazUtils
  FileUtil, LazFileUtils, Translations, StringHashList,
  // PoChecker
  PoCheckerConsts;

Type

  TPoTestType = (pttCheckNrOfItems, pttCheckFormatArgs, pttCheckMissingIdentifiers,
                 pttCheckMismatchedOriginals);
  TPoTestTypes = Set of TPoTestType;

  TPoTestOption = (ptoFindAllChildren);
  TPoTestOptions = set of TPoTestOption;

const
    optRunAllTests: TPoTestTypes = [];

    PoTestTypeNames: array[TPoTestType] of String = (
      sCheckNumberOfItems,
      sCheckForIncompatibleFormatArguments,
      sCheckMissingIdentifiers,
      sCheckForMismatchesInUntranslatedStrings
    );

Type
  { TPoFamily }

  TTestStartEvent = procedure(const ATestName, APoFileName: String) of object;
  TTestEndEvent = procedure(const ATestName: String; const ErrorCount: Integer) of object;

  TPoFamilyStats = class;

  TPoFamily = class
  private
    FMaster: TPOFile;
    FChild: TPOFile;
    FMasterName: String;
    FChildName: String;
    FOnTestStart: TTestStartEvent;
    FOnTestEnd: TTestEndEvent;
    FPoFamilyStats: TPoFamilyStats;
    FTestOptions: TPoTestOptions;
    FTestTypes: TPoTestTypes;
    procedure SetChildName(AValue: String);
    procedure SetMasterName(AValue: String);
    function GetShortMasterName: String;
    function GetShortChildName: String;
  protected
    procedure DoTestStart(const ATestName, APoFileName: String);
    procedure DoTestEnd(const ATestName: String; const ErrorCount: Integer);
  public
    constructor Create(const AMasterName, AChildName: String);
    destructor Destroy; override;

  protected
    procedure CheckNrOfItems(out ErrorCount: Integer; ErrorLog: TStrings);
    procedure CheckFormatArgs(out ErrorCount, NonFuzzyErrorCount: Integer; ErrorLog: TStrings);
    procedure CheckMissingIdentifiers(out ErrorCount: Integer; ErrorLog: TStrings);
    procedure CheckMismatchedOriginals(out ErrorCount: Integer; ErrorLog: TStrings);
    procedure CheckDuplicateOriginals(out WarningCount: Integer; ErrorLog: TStrings);
    procedure CheckStatistics(ErrorCnt: Integer);

  public
    procedure RunTests(out ErrorCount, NonFuzzyErrorCount, WarningCount,
      TranslatedCount, UntranslatedCount, FuzzyCount: Integer; ErrorLog, StatLog, DupLog: TStringList);

    property Master: TPOFile read FMaster;
    property Child: TPOFile read FChild;
    property MasterName: String read FMasterName write SetMasterName;
    property ChildName: String read FChildName write SetChildName;
    property TestTypes: TPoTestTypes read FTestTypes write FTestTypes;
    property TestOptions: TPoTestOptions read FTestOptions write FTestOptions;
    property PoFamilyStats: TPoFamilyStats read FPoFamilyStats;
    property ShortMasterName: String read  GetShortMasterName;
    property ShortChildName: String read GetShortChildName;
    property OnTestStart: TTestStartEvent read FOnTestStart write FOnTestStart;
    property OnTestEnd: TTestEndEvent read FOnTestEnd write FOnTestEnd;
  end;

  { TPoFamilyStats }

  { TStat }

  TStat = class
  private
    FPoName: String;
    FNrTotal: Integer;
    FNrTranslated: Integer;
    FNrUnTranslated: Integer;
    FNrFuzzy: Integer;
    FNrErrors: Integer;
  public
    constructor Create(APoName: String; ANrTotal, ANrTranslated, ANrUntranslated, ANrFuzzy, ANrErrors: Integer);
    function ShortPoName: String;
    property PoName: string read FPoName;
    property NrTotal: Integer read FNrTotal;
    property NrTranslated: Integer read FNrTranslated;
    property NrUnTranslated: Integer read FNrUnTranslated;
    property NrFuzzy: Integer read FNrFuzzy;
    property NrErrors: Integer read FNrErrors;
    function PercTranslated: Double; inline;
    function PercUnTranslated: Double; inline;
    function PercFuzzy: Double; inline;
    function FracTranslated: Double;
    function FracUnTranslated: Double;
    function FracFuzzy: Double;
  end;

  TPoFamilyStats = class
  private
    FList: TFPObjectList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TStat;
  public
    procedure Clear;
    procedure Add(AName: String; ANrTotal, ANrTranslated, ANrUnTranslated, ANrFuzzy, ANrErrors: Integer);
    procedure AddItemsTo(APoFamilyStats: TPoFamilyStats);
    constructor Create;
    destructor Destroy; override;
    procedure AddStatisticsToLog(ALog: TStrings);

    property Items[Index: Integer]: TStat read GetItems;
    property Count: Integer read GetCount;
  end;

function ExtractMasterNameFromChildName(const AChildName: String): String;
procedure LocalizePoTestTypeNames;

const
  NoError = 0;

implementation

const
  sCommentIdentifier = '#: ';
  //sCharSetIdentifier = '"Content-Type: text/plain; charset=';
  sMsgID = 'msgid "';
  sMsgStr = 'msgstr "';
  //sMsgCtxt = 'msgctxt "';
  //sFlags = '#, ';
  //sPrevMsgID = '#| msgid "';
  //sPrevStr = '#| "';

  Divider = '-------------------------------------------------------';

  sFormatArgsID = '%s %s';
  sFormatArgsValues = '%s%s"   (= %s)';

  sMismatchOriginalsID = '%s';
  sMismatchOriginalsM = '%s: %s';
  sMismatchOriginalsC = '%s: %s';

  sShortCheckFormatArgs = 'CheckFormatArgs';
  sShortCheckNrOfItems =  'CheckNrOfItems';
  sShortCheckMissingIdentifiers = 'CheckMissingIdentifiers';
  sShortCheckMismatchedOriginals = 'CheckMismatchedOriginals';

//Helper functions

function ExtractMasterNameFromChildName(const AChildName: String): String;
{
  Input: AChildName is like: somename.some_language_specifier.po
  Output: Result  = somename.pot
}
var
  CurUnitName: String;
  CurLang: String;
begin
  Result := '';
  if GetPOFilenameParts(AChildName, CurUnitName, CurLang) then
    Result := CurUnitName + ExtensionSeparator + 'pot';
end;

procedure LocalizePoTestTypeNames;
begin
  PoTestTypeNames[pttCheckNrOfItems] := sCheckNumberOfItems;
  PoTestTypeNames[pttCheckFormatArgs] := sCheckForIncompatibleFormatArguments;
  PoTestTypeNames[pttCheckMissingIdentifiers] := sCheckMissingIdentifiers;
  PoTestTypeNames[pttCheckMismatchedOriginals] := sCheckForMismatchesInUntranslatedStrings;
end;

{ TStat }

constructor TStat.Create(APoName: String; ANrTotal, ANrTranslated, ANrUntranslated, ANrFuzzy, ANrErrors: Integer);
begin
  FPoName := APoName;
  FNrTotal := ANrTotal;
  FNrTranslated := ANrTranslated;
  FNrUntranslated := ANrUntranslated;
  FNrFuzzy := ANrFuzzy;
  FNrErrors := ANrErrors;
end;

function TStat.ShortPoName: String;
begin
  Result := ExtractFilename(FPoName);
end;

function TStat.PercTranslated: Double;
begin
  Result := 100 * FracTranslated;
end;

function TStat.PercUnTranslated: Double;
begin
  Result := 100 * FracUnTranslated;
end;

function TStat.PercFuzzy: Double;
begin
  Result := 100 * FracFuzzy;
end;

function TStat.FracTranslated: Double;
begin
  Result := (FNrTranslated / FNrTotal);
end;

function TStat.FracUnTranslated: Double;
begin
  Result := (FNrUnTranslated / FNrTotal);
end;

function TStat.FracFuzzy: Double;
begin
  Result := (FNrFuzzy / FNrTotal);
end;

{ TPoFamilyStats }

function TPoFamilyStats.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TPoFamilyStats.GetItems(Index: Integer): TStat;
begin
  Result := TStat(FList.Items[Index]);
end;

procedure TPoFamilyStats.Clear;
begin
  FList.Clear;
end;

procedure TPoFamilyStats.Add(AName: String; ANrTotal, ANrTranslated, ANrUnTranslated, ANrFuzzy, ANrErrors: Integer);
begin
  FList.Add(TStat.Create(AName, ANrTotal, ANrTranslated, ANrUntranslated, ANrFuzzy, ANrErrors));
end;

procedure TPoFamilyStats.AddItemsTo(APoFamilyStats: TPoFamilyStats);
var
  i: Integer;
  AStat: TStat;
begin
  for i := 0 to FList.Count - 1 do
  begin
    AStat := GetItems(i);
    APoFamilyStats.Add(AStat.PoName, AStat.NrTotal, AStat.NrTranslated,
                       AStat.NrUntranslated, AStat.NrFuzzy, AStat.NrErrors);
  end;
end;

constructor TPoFamilyStats.Create;
begin
  FList := TFPObjectList.Create(True);
end;

destructor TPoFamilyStats.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TPoFamilyStats.AddStatisticsToLog(ALog: TStrings);
var
  i: Integer;
  Stat: TStat;
  function Bar(Nr, Total: Integer; RoundDown: Boolean): String;
  const
    Max = 50;
  var
    Count: Integer;
  begin
    if RoundDown then
      Count := Floor(Max * (Nr/Total))
    else
      Count := Ceil(Max * (Nr/Total));
    Result := StringOfChar('x', Count);
    Result := Result + StringOfChar(#32, Max - Count);
  end;
begin
  if (FList.Count = 0) then Exit;
  for i := 0 to FList.Count - 1 do
  begin
    Stat := TStat(FList.Items[i]);
    ALog.Add(Stat.PoName);
    ALog.Add(Format(sPercTranslated,[Bar(Stat.NrTranslated, Stat.NrTotal, True),Stat.PercTranslated]));
    ALog.Add(Format(sPercUntranslated,[Bar(Stat.NrUnTranslated, Stat.NrTotal, False), Stat.PercUnTranslated]));
    ALog.Add(Format(sPercFuzzy,[Bar(Stat.NrFuzzy, Stat.NrTotal, False), Stat.PercFuzzy]));
    ALog.Add('');
    ALog.Add('');
  end;
end;

{ TPoFamily }

procedure TPoFamily.SetMasterName(AValue: String);
begin
  if FMasterName = AValue then Exit;
  FMaster.Free;
  FMaster := nil;
  FMasterName := '';
  if (AValue <> '') then FMaster := TPOFile.Create(AValue, True);
  FMasterName := AValue;
end;

function TPoFamily.GetShortMasterName: String;
begin
  Result := ExtractFileName(FMasterName);
end;

function TPoFamily.GetShortChildName: String;
begin
  Result := ExtractFileName(FChildName);
end;

procedure TPoFamily.DoTestStart(const ATestName, APoFileName: String);
begin
  if Assigned(FOnTestStart) then FOnTestStart(ATestName, APoFileName);
end;

procedure TPoFamily.DoTestEnd(const ATestName: String; const ErrorCount: Integer);
begin
  if Assigned(FOnTestEnd) then FOnTestEnd(ATestName, ErrorCount);
end;


procedure TPoFamily.SetChildName(AValue: String);
begin
  if FChildName = AValue then Exit;
  FChild.Free;
  FChild := nil;
  FChildName := '';
  if (AValue <> '') then FChild := TPOFile.Create(AValue, True);
  FChildName := AValue;
end;

constructor TPoFamily.Create(const AMasterName, AChildName: String);
begin
  if (AMasterName <> '') then
  begin
    FMaster := TPOFile.Create(AMasterName, True);
    FMasterName := AMasterName;
    //debugln('TPoFamily.Create: created ',FMasterName);
  end;
  if (AChildName <> '') then
  begin
    FChild := TPOFile.Create(AChildName, True);
    FChildName := AChildName;
    //debugln('TPoFamily.Create: created ',FChildName);
  end;
  FPoFamilyStats := TPoFamilyStats.Create;
end;

destructor TPoFamily.Destroy;
begin
  if Assigned(FMaster) then FMaster.Free;
  if Assigned(FChild) then FChild.Free;
  FPoFamilyStats.Free;
  inherited Destroy;
end;

procedure TPoFamily.CheckNrOfItems(out ErrorCount: Integer; ErrorLog: TStrings);
begin
  //debugln('TPoFamily.CheckNrOfItems');
  DoTestStart(PoTestTypeNames[pttCheckNrOfItems], ShortChildName);
  if (FMaster.Count <> FChild.Count) then
  begin
    ErrorCount := 1;
    ErrorLog.Add(Divider);
    ErrorLog.Add(Format(sErrorsByTest,[sShortCheckNrOfItems]));
    ErrorLog.Add(ShortChildName);
    ErrorLog.Add(Divider);
    ErrorLog.Add('');
    ErrorLog.Add(sNrOfItemsMismatch);
    ErrorLog.Add(Format(sNrOfItemsMismatchD,[ShortMasterName,FMaster.Count]));
    ErrorLog.Add(Format(sNrOfItemsMismatchD,[ShortChildName,FChild.Count]));
    ErrorLog.Add(Divider);
    ErrorLog.Add('');
    ErrorLog.Add('');
  end
  else ErrorCount := NoError;
  DoTestEnd(PoTestTypeNames[pttCheckNrOfItems], ErrorCount);
  //debugln('TPoFamily.CheckNrOfItemsMismatch: ',Dbgs(ErrorCount),' Errors');
end;

procedure TPoFamily.CheckFormatArgs(out ErrorCount, NonFuzzyErrorCount: Integer
  ; ErrorLog: TStrings);
var
  i: Integer;
  CPoItem: TPOFileItem;
  IsBadFormat: Boolean;
begin
  //debugln('TPoFamily.CheckFormatArgs');
  DoTestStart(PoTestTypeNames[pttCheckFormatArgs], ShortChildName);
  ErrorCount := NoError;
  NonFuzzyErrorCount := NoError;
  for i := 0 to FChild.Count - 1 do
  begin
    CPoItem := FChild.PoItems[i];
    if Assigned(CPoItem) then
    begin
      IsBadFormat := (Pos(sBadFormatFlag, CPoItem.Flags) > 0);
      //Check only non-virtual translations.
      //Virtual translations can have formatting errors if copied from an item with no-object-pascal-format flag set.
      //These translations will not be used anyway (they are fuzzy) and are physically missing in translation file.
      if (CPoItem.VirtualTranslation = false) and (Length(CPoItem.Translation) > 0) and IsBadFormat then
      begin
        if (ErrorCount = 0) then
        begin
          ErrorLog.Add(Divider);
          ErrorLog.Add(Format(sErrorsByTest,[sShortCheckFormatArgs]));
          ErrorLog.Add(ShortChildName);
          ErrorLog.Add(Divider);
          ErrorLog.Add('');
        end;
        Inc(ErrorCount);
        if CPoItem.InitialFuzzyState = false then
          Inc(NonFuzzyErrorCount);
        ErrorLog.Add(Format(sIncompatibleFormatArgs,[CPoItem.LineNr]));
        ErrorLog.Add(Format(sFormatArgsID,[sCommentIdentifier, CPoItem.IdentifierLow]));
        ErrorLog.Add(Format(sFormatArgsValues,[sMsgID,CPoItem.Original,sOriginal]));
        ErrorLog.Add(Format(sFormatArgsValues,[sMsgStr,CPoItem.Translation,sTranslation]));
        if CPoItem.InitialFuzzyState = true then
          ErrorLog.Add(sNoteTranslationIsFuzzy);
        ErrorLog.Add('');
      end;
    end;
  end;
  if (ErrorCount > 0) then
  begin
    ErrorLog.Add(Format(sNrErrorsFound,[ErrorCount]));
    ErrorLog.Add(Divider);
    ErrorLog.Add('');
    ErrorLog.Add('');
  end;
  DoTestEnd(PoTestTypeNames[pttCheckFormatArgs], ErrorCount);
  //debugln('TPoFamily.CheckIncompatibleFormatArgs: ',Dbgs(ErrorCount),' Errors');
end;

procedure TPoFamily.CheckMissingIdentifiers(out ErrorCount: Integer;
  ErrorLog: TStrings);
var
  i: Integer;
  MPoItem, CPoItem: TPOFileItem;
begin
  //debugln('TPoFamily.CheckMissingIdentifiers');
  DoTestStart(PoTestTypeNames[pttCheckMissingIdentifiers], ShortChildName);
  ErrorCount := NoError;
  for i := 0 to FMaster.Count - 1 do
  begin
    MPoItem := FMaster.PoItems[i];
    if Assigned(MPoItem) and (MPoItem.IdentifierLow <> '') then
    begin
      CPoItem := FChild.FindPoItem(MPoItem.IdentifierLow);
      if not Assigned(CPoItem) then
      begin
        if (ErrorCount = 0) then
        begin
          ErrorLog.Add(Divider);
          ErrorLog.Add(Format(sErrorsByTest,[sShortCheckMissingIdentifiers]));
          ErrorLog.Add(ShortChildName);
          ErrorLog.Add(Divider);
          ErrorLog.Add('');
        end;
        Inc(ErrorCount);
        ErrorLog.Add(Format(sLineInFileName,
                            [MPoItem.LineNr,ShortMasterName]));
        ErrorLog.Add(Format(sIdentifierNotFoundIn,
                            [MPoItem.IdentifierLow,ShortChildName]));
        ErrorLog.Add('');
      end;
    end;
  end;
  //Now reverse the search
  for i := 0 to FChild.Count - 1 do
  begin
    CPoItem := FChild.PoItems[i];
    if Assigned(CPoItem) and (CPoItem.IdentifierLow <> '') then
    begin
      MPoItem := FMaster.FindPoItem(CPoItem.IdentifierLow);
      if not Assigned(MPoItem) then
      begin
        if (ErrorCount = 0) then
        begin
          ErrorLog.Add(Divider);
          ErrorLog.Add(Format(sErrorsByTest,[sShortCheckMissingIdentifiers]));
          ErrorLog.Add(ShortChildName);
          ErrorLog.Add(Divider);
          ErrorLog.Add('');
        end;
        Inc(ErrorCount);
        ErrorLog.Add(Format(sLineNr,
                            [CPoItem.LineNr]));
        ErrorLog.Add(Format(sMissingMasterIdentifier,
                            [CPoItem.IdentifierLow,ShortChildName,ShortMasterName]));
        ErrorLog.Add('');
      end;
    end;
  end;
  if (ErrorCount > 0) then
  begin
    ErrorLog.Add(Format(sNrErrorsFound,[ErrorCount]));
    ErrorLog.Add(Divider);
    ErrorLog.Add('');
    ErrorLog.Add('');
  end;
  DoTestEnd(PoTestTypeNames[pttCheckMissingIdentifiers], ErrorCount);
  //debugln('TPoFamily.CheckMissingIdentifiers: ',Dbgs(ErrorCount),' Errors');
end;

procedure TPoFamily.CheckMismatchedOriginals(out ErrorCount: Integer;
  ErrorLog: TStrings);
var
  i: Integer;
  MPoItem, CPoItem: TPOFileItem;
begin
  //debugln('TPoFamily.CheckMismatchedOriginals');
  DoTestStart(PoTestTypeNames[pttCheckMismatchedOriginals], ShortChildName);
  ErrorCount := NoError;
  for i := 0 to FMaster.Count - 1 do
  begin
    MPoItem := FMaster.PoItems[i];
    CPoItem := FChild.FindPoItem(MpoItem.IdentifierLow);
    if Assigned(CPoItem) then
    begin
      if (MPoItem.Original <> CPoItem.Original) then
      begin
        if (ErrorCount = 0) then
        begin
          ErrorLog.Add(Divider);
          ErrorLog.Add(Format(sErrorsByTest,[sShortCheckMismatchedOriginals]));
          ErrorLog.Add(ShortChildName);
          ErrorLog.Add(Divider);
          ErrorLog.Add('');
        end;
        Inc(ErrorCount);
        ErrorLog.Add(Format(sLineInFileName,[CpoItem.LineNr, ShortChildName]));
        ErrorLog.Add(Format(sMismatchOriginalsID,[CPoItem.IdentifierLow]));
        ErrorLog.Add(Format(sMismatchOriginalsM,[ShortMasterName,MPoItem.Original]));
        ErrorLog.Add(Format(sMismatchOriginalsC,[ShortChildName, CPoItem.Original]));
        ErrorLog.Add('');
      end;
    end;
  end;
  if (ErrorCount > 0) then
  begin
    ErrorLog.Add(Format(sNrErrorsFound,[ErrorCount]));
    ErrorLog.Add(Divider);
    ErrorLog.Add('');
    ErrorLog.Add('');
  end;
  DoTestEnd(PoTestTypeNames[pttCheckMismatchedOriginals], ErrorCount);
  //debugln('TPoFamily.CheckMismatchedOriginals: ',Dbgs(ErrorCount),' Errors');
end;

procedure TPoFamily.CheckDuplicateOriginals(out WarningCount: Integer;
  ErrorLog: TStrings);
var
  i: Integer;
  PoItem: TPOFileItem;
  DupItemsList: TStringHashList;
  LastHash, CurHash: Cardinal;
begin
  //debugln('TPoFamily.CheckMismatchedOriginals');
  WarningCount := 0;

  DupItemsList := TStringHashList.Create(true);
  for i := 0 to FMaster.Items.Count - 1 do begin
    PoItem := TPOFileItem(FMaster.Items[i]);
    if PoItem.Duplicate = true then
      DupItemsList.Add(PoItem.Original, PoItem);
  end;

  //debugln('TPoFamily.CehckDuplicateOriginals');
  //debugln('DupItemsList.Count = ',DbgS(DupItemsList.Count));
  LastHash := 0;
  for i := DupItemsList.Count - 1 downto 0 do
  begin
    PoItem := TPoFileItem(DupItemsList.List[i]^.Data);
    if Assigned(PoItem) then
    begin
      CurHash := DupItemsList.List[i]^.HashValue;
      if (WarningCount = 0) then
      begin
        ErrorLog.Add(Format(sFile, [ShortMasterName]));
        ErrorLog.Add('');
      end;
      if (CurHash <> LastHash) then
      begin//new value for PoItem.Original
        LastHash := CurHash;
        Inc(WarningCount);
        if (WarningCount > 1) then ErrorLog.Add('');
        ErrorLog.Add(Format(sDuplicateOriginals,[PoItem.Original]));
        //debugln(format('The (untranslated) value "%s" is used for more than 1 entry:',[PoItem.Original]));
      end;
      ErrorLog.Add(format(sDuplicateLineNrWithValue,[PoItem.LineNr,PoItem.IdentifierLow]));
      //debugln(format(sDuplicateLineNrWithValue,[PoItem.LineNr,PoItem.IdentifierLow]));
    end;
  end;

  if (WarningCount > 0) then
  begin
    ErrorLog.Add('');
    ErrorLog.Add(Format(sNrWarningsFound,[WarningCount]));
    ErrorLog.Add(Divider);
    ErrorLog.Add('');
  end;

  DupItemsList.Free;

  //debugln('TPoFamily.CheckDuplicateOriginals: ',Dbgs(WarningCount),' Errors');
end;

procedure TPoFamily.CheckStatistics(ErrorCnt: Integer);
var
  NrTranslated, NrUntranslated, NrFuzzy, NrTotal: Integer;
begin
  //debugln('TPoFamily.CheckStatistics');
  NrTranslated := FChild.Statistics.Translated;
  NrUntranslated := FChild.Statistics.Untranslated;
  NrFuzzy := FChild.Statistics.Fuzzy;
  NrTotal := NrTranslated + NrUntranslated + NrFuzzy;
  if (NrTotal > 0) then
  begin
    FPoFamilyStats.Add(ChildName, NrTotal, NrTranslated, NrUntranslated, NrFuzzy, ErrorCnt);
  end;
  //debugln('TPoFamily.CheckIncompatibleFormatArgs: ',Dbgs(ErrorCount),' Errors');
end;

{
procedure TPoFamily.RunTests
Pre conditions:
  * Master and a matching Child must be assigned at start ot testing
  * If a Child is assigned it must be child of Master
}
procedure TPoFamily.RunTests(out ErrorCount, NonFuzzyErrorCount, WarningCount,
  TranslatedCount, UntranslatedCount, FuzzyCount: Integer; ErrorLog, StatLog,
  DupLog: TStringList);
var
  SL: TStringList;
  CurrErrCnt, CurrNonFuzzyErrCnt, CurrWarnCnt, ThisErrCnt: Integer;
  i: Integer;
  CurrChildName: String;
  S: String;
begin
  SL := nil;
  FPoFamilyStats.Clear;
  ErrorCount := NoError;
  NonFuzzyErrorCount := NoError;
  WarningCount := NoError;
  TranslatedCount := 0;
  UntranslatedCount := 0;
  FuzzyCount := 0;
  if (not Assigned(FMaster)) and (not Assigned(FChild)) then
  begin
    {$ifdef DebugSimplePoFiles}
    debugln('TPoFamily.RunTests: Both master and child are unassigned.');
    {$endif}
    Exit;
  end;
  if not Assigned(FMaster) then
  begin
    S := ExtractMasterNameFromChildName(FChildName);
    if (S <> '') and FileExistsUtf8(S) then
    begin
      SetMasterName(S);
    end
    else
    begin
      {$ifdef DebugSimplePoFiles}
      Debugln('TPoFamily.RunTests: Cannot find master for ',ShortChildName);
      {$endif}
      Exit;
    end
  end;

  if (ptoFindAllChildren in FTestOptions) then
  begin
    SL := FindAllTranslatedPoFiles(FMasterName);
    //We want current Child (if currently assigned) at index 0
    if Assigned(FChild) then
    begin
      for i := 0 to SL.Count - 1 do
      begin
        if (CompareFileNames(Sl.Strings[i], FChildName) = 0) then
        begin
          if (i <> 0) then SL.Exchange(i,0);
          Break;
        end;
      end;
    end;
  end
  else
  begin
    SL := TStringList.Create;
    if Assigned(FChild) then Sl.Add(FChildName);
  end;

//  for i := 0 to sl.count - 1 do debugln(extractfilename(sl.strings[i]));

  try

    //First run checks that are Master-only
    CheckDuplicateOriginals(CurrWarnCnt, DupLog);
    WarningCount := CurrWarnCnt + WarningCount;

    {$ifdef DebugSimplePoFiles}
    Debugln('TPoFamily.RunTests: number of childs for testing = ',DbgS(Sl.Count));
    {$endif}

    if (FTestTypes <> []) and (Sl.Count = 0) then
    begin
      {$ifdef DebugSimplePoFiles}
      Debugln('TPoFamily.RunTests: Warning: No child selected or found for selected tests');
      {$endif}
      ErrorLog.Add(Divider);
      ErrorLog.Add('Warning: No child selected (or found) for selected tests.');
      ErrorLog.Add(Divider);
    end;

    //then iterate all Children
    for i := 0 to SL.Count - 1 do
    begin
      ThisErrCnt:= 0;
      CurrChildName := SL.Strings[i];
      //debugln('TPoFamily.RunTests: setting ChildName to ',CurrChildName);
      SetChildName(CurrChildName);

      if (pttCheckNrOfItems in FTestTypes) then
      begin
        CheckNrOfItems(CurrErrCnt, ErrorLog);
        ErrorCount := CurrErrCnt + ErrorCount;
        ThisErrCnt := ThisErrCnt + CurrErrCnt;
      end;

      if (pttCheckFormatArgs in FTestTypes) then
      begin
        CheckFormatArgs(CurrErrCnt, CurrNonFuzzyErrCnt, ErrorLog);
        ErrorCount := CurrErrCnt + ErrorCount;
        ThisErrCnt := ThisErrCnt + CurrErrCnt;
        NonFuzzyErrorCount := CurrNonFuzzyErrCnt + NonFuzzyErrorCount;
      end;


      if (pttCheckMissingIdentifiers in FTestTypes) then
      begin
        CheckMissingIdentifiers(CurrErrCnt, ErrorLog);
        ErrorCount := CurrErrCnt + ErrorCount;
        ThisErrCnt := ThisErrCnt + CurrErrCnt;
      end;

      if (pttCheckMismatchedOriginals in FTestTypes) then
      begin
        CheckMismatchedOriginals(CurrErrCnt, ErrorLog);
        ErrorCount := CurrErrCnt + ErrorCount;
        ThisErrCnt := ThisErrCnt + CurrErrCnt;
      end;

      //Always run this as the last test please
      TranslatedCount := FChild.Statistics.Translated;
      UntranslatedCount := FChild.Statistics.Untranslated;
      FuzzyCount := FChild.Statistics.Fuzzy;
      CheckStatistics(ThisErrCnt);
       {
        if (ptt in FTestTypes) then
        begin
          Check(CurrErrCnt, ErrorLog);
          ErrorCount := CurrErrCnt + ErrorCount;
        end;
        }
    end;
    //Generate statistics
    if FPoFamilyStats.Count > 0 then
      FPoFamilyStats.AddStatisticsToLog(StatLog);
  finally
    SL.Free;
  end;
  //debugln('TPoFamilyRunTests: ErrorCount = ',DbgS(ErrorCount));
end;

procedure InitTestOptions;
var
  Index: TPoTestType;
begin
  for Index := Low(TPoTestType) to High(TPoTestType) do optRunAllTests := optRunAllTests + [Index];
end;

Initialization

InitTestOptions;

end.

