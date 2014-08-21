unit PoFamilies;

{ $define DebugSimplePoFiles}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, StringHashList,
  //{$IFDEF UNIX}{$IFNDEF DisableCWString}, cwstring{$ENDIF}{$ENDIF},
  SimplePoFiles, pocheckerconsts;

Type

  TPoTestType = (pttCheckNrOfItems, pttCheckFormatArgs, pttCheckMissingIdentifiers,
                 pttCheckMismatchedOriginals, pttCheckDuplicateOriginals, pttCheckStatistics);
  TPoTestTypes = Set of TPoTestType;

  TPoTestOption = (ptoFindAllChildren, ptoIgnoreFuzzyStrings);
  TPoTestOptions = set of TPoTestOption;

const
    optRunAllTests: TPoTestTypes = [];

    PoTestTypeNames: array[TPoTestType] of String = (
      sCheckNumberOfItems,
      sCheckForIncompatibleFormatArguments,
      sCheckMissingIdentifiers,
      sCheckForMismatchesInUntranslatedStrings,
      sCheckForDuplicateUntranslatedValues,
      sCheckStatistics
    );

Type
  { TPoFamily }

  TTestStartEvent = procedure(const ATestName, APoFileName: String) of object;
  TTestEndEvent = procedure(const ATestName: String; const ErrorCount: Integer) of object;

  TPoFamily = class
  private
    FMaster: TSimplePoFile;
    FChild: TSimplePoFile;
    FMasterName: String;
    FChildName: String;
    FOnTestStart: TTestStartEvent;
    FOnTestEnd: TTestEndEvent;
    procedure SetChildName(AValue: String);
    procedure SetMasterName(AValue: String);
    function GetShortMasterName: String;
    function GetShortChildName: String;
  protected
    procedure DoTestStart(const ATestName, APoFileName: String);
    procedure DoTestEnd(const ATestName: String; const ErrorCount: Integer);
  public
    constructor Create;
    constructor Create(const MasterName: String);
    constructor Create(const AMasterName, AChildName: String);
    destructor Destroy; override;

  protected
    procedure CheckNrOfItems(out ErrorCount: Integer; ErrorLog: TStrings);
    procedure CheckFormatArgs(out ErrorCount: Integer; ErrorLog: TStrings; IgnoreFuzzyStrings: Boolean);
    procedure CheckMissingIdentifiers(out ErrorCount: Integer; ErrorLog: TStrings);
    procedure CheckMismatchedOriginals(out ErrorCount: Integer; ErrorLog: TStrings);
    procedure CheckDuplicateOriginals(out WarningCount: Integer; ErrorLog: TStrings);
    procedure CheckStatistics(out WarningCount: Integer; ErrorLog: TStrings);

  public
    procedure RunTests(const TestTypes: TPoTestTypes; const TestOptions: TPoTestOptions;
                       out ErrorCount, WarningCount: Integer; ErrorLog: TStrings);

    property Master: TSimplePoFile read FMaster;
    property Child: TSimplePoFile read FChild;
    property MasterName: String read FMasterName write SetMasterName;
    property ChildName: String read FChildName write SetChildName;
    property ShortMasterName: String read  GetShortMasterName;
    property ShortChildName: String read GetShortChildName;
    property OnTestStart: TTestStartEvent read FOnTestStart write FOnTestStart;
    property OnTestEnd: TTestEndEvent read FOnTestEnd write FOnTestEnd;
  end;

function ExtractFormatArgs(S: String; out ArgumentError: Integer): String;
function IsMasterPoName(const Fn: String): Boolean;
function ExtractMasterNameFromChildName(const AChildName: String): String;
function FindAllTranslatedPoFiles(const Filename: string): TStringList;


implementation

const
  NoError = 0;
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

  sCheckFormatArgs = 'CheckFormatArgs';
  sCheckNrOfItems =  'CheckNrOfItems';
  sCheckMissingIdentifiers = 'CheckMissingIdentifiers';
  sCheckMismatchedOriginals = 'CheckMismatchedOriginals';
  sCheckDuplicateOriginals = 'CheckDuplicateOriginals';

//Helper functions

function ExtractFormatArgs(S: String; out ArgumentError: Integer): String;
const
  FormatArgs = 'DEFGMNPSUX';
  FormatChar = '%';
  FormatSpecs = ':-.0123456789';
var
  p: PtrInt;
  NewStr, Symb: String;
begin
  NewStr := '';
  ArgumentError := 0;
  p := UTF8Pos(FormatChar, S);
  while (Length(S)>0) and (p>0) and (ArgumentError=0) do
  begin
    UTF8Delete(S, 1, p);
    if Length(S)>0 then
    begin
      Symb := UTF8UpperCase(UTF8Copy(S, 1, 1));
      while (Length(S)>1) and (UTF8Pos(Symb, FormatSpecs)>0) do
      begin
        //weak syntax check for formatting options, skip them if found
        UTF8Delete(S, 1, 1);
        Symb := UTF8UpperCase(UTF8Copy(S, 1, 1));
      end;
      if Symb <> FormatChar then
      begin
        NewStr := NewStr+Symb;
        if UTF8Pos(Symb, FormatArgs)=0 then
          ArgumentError := Utf8Length(NewStr);
      end;
      //removing processed symbol
      UTF8Delete(S, 1, 1);
      //searching for next argument
      p := UTF8Pos(FormatChar, S);
    end
    else
      //in this case formatting symbol doesn't have its argument
      ArgumentError := Utf8Length(NewStr) + 1;
  end;
  Result := NewStr;
end;

function IsMasterPoName(const Fn: String): Boolean;
//Returns True if Fn is like '[Path/To/]somename.po'
var
  Ext: String;
  S: String;
begin
  S := ExtractFileName(Fn);
  Ext := ExtractFileExt(S);
  S := Copy(S, 1, Length(S) - Length(Ext));
  Result := (Length(S) > 0) and
            (CompareText(Ext, ExtensionSeparator + 'po') = 0) and
            (Pos(ExtensionSeparator, S) = 0);
end;

function ExtractMasterNameFromChildName(const AChildName: String): String;
{
  Pre condition: AChildName is like: somename.some_language_specifier.po
  Post condition: Result  = somename.po
}
var
  Ext: String;
  EndSep: Set of Char;
  Len: Integer;
begin
  EndSep := AllowDirectorySeparators + AllowDriveSeparators + [ExtensionSeparator];
  Ext := ExtractFileExt(AChildName);
  Result := Copy(AChildName, 1, Length(AChildName) - Length(Ext));
  Len := Length(Result);
  While (Len > 0) and (not (Result[Len] in EndSep)) do Dec(Len);

  //debugln('Len = ',DbgS(Len));
  //debugln('Length(Result) = ',DbgS(Length(result)));
  //if Len > 0 then debugln('Result[Len] = ',Result[len]);

  if (Len > 1) and (Len < Length(Result)) and (Result[Len] = ExtensionSeparator) then
    Result := Copy(Result, 1, Len - 1) + Ext
  else
    Result := '';
end;

function FindAllTranslatedPoFiles(const Filename: string): TStringList;
var
  Path: String;
  Name: String;
  NameOnly: String;
  Ext: String;
  FileInfo: TSearchRec;
  CurExt: String;
begin
  Result := TStringList.Create;
  Path := ExtractFilePath(Filename);
  Name := ExtractFilename(Filename);
  Ext := ExtractFileExt(Filename);
  NameOnly := LeftStr(Name,length(Name)-length(Ext));
  if FindFirstUTF8(Path+GetAllFilesMask,faAnyFile,FileInfo)=0 then
  begin
    repeat
      if (FileInfo.Name = '.') or (FileInfo.Name = '..') or (FileInfo.Name = '')
      or (CompareFilenames(FileInfo.Name,Name) = 0) then continue;
      CurExt:=ExtractFileExt(FileInfo.Name);
      if (CompareFilenames(CurExt,'.po') <> 0)
      or (CompareFilenames(LeftStr(FileInfo.Name,length(NameOnly)),NameOnly) <> 0)
      then
        continue;
      Result.Add(Path+FileInfo.Name);
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
end;



function CompareFormatArgs(S1, S2: String): Boolean;
var
  Extr1, Extr2: String;
  ArgErr1, ArgErr2: Integer;
begin
  Result := true;
  //do not check arguments if strings are equal to save time and avoid some
  //false positives, e.g. for '{%Region}' string in lazarusidestrconsts
  if S1 <> S2 then
  begin
    Extr1 := ExtractFormatArgs(S1, ArgErr1);
    Extr2 := ExtractFormatArgs(S2, ArgErr2);
    //writeln('Extr1 = ',Extr1,' ArgErr1 = ',ArgErr1);
    //writeln('Extr2 = ',Extr1,' ArgErr2 = ',ArgErr2);
    if (ArgErr1 = 0) then
    begin
      if (ArgErr2 = 0) then
      begin
        Result := Utf8CompareText(Extr1, Extr2) = 0;
      end
      else
      begin
        //Extr2 can have dangling %'s
        //e.g. Extr1 = "%s %d" Extr2 = "%s %d {%H}", it does not make sense, but it's not illegal
        if (ArgErr2 = Utf8Length(Extr1)+1) and not (ArgErr2 > Utf8Length(Extr2)) then Extr2 := Utf8Copy(Extr2,1,ArgErr2-1);
        Result := Utf8CompareText(Extr1, Extr2) = 0;
      end;
    end
    else
    begin  //ArgErr1 <> 0
      //Assume Extr1 is always legal, otherwise the IDE would crash in it's default language...
      //Only compare until the last valid argument in Extr1
      if (ArgErr1 = Utf8Length(Extr1)) then Utf8Delete(Extr1, ArgErr1, 1);
      if Utf8Length(Extr2) > Utf8Length(Extr1) then Extr2 := Utf8Copy(Extr2, 1, Utf8Length(Extr1));
      Result := Utf8CompareText(Extr1, Extr2) = 0;
    end;
    //writeln('CompareFormatArgs: Result = ',Result);
  end;
end;

{ TPoFamily }

procedure TPoFamily.SetMasterName(AValue: String);
begin
  if FMasterName = AValue then Exit;
  FMaster.Free;
  FMaster := nil;
  FMasterName := '';
  if (AValue <> '') then FMaster := TSimplePoFile.Create(AValue{, True});
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
  if (AValue <> '') then FChild := TSimplePoFile.Create(AValue{, True});
  FChildName := AValue;
end;

constructor TPoFamily.Create;
begin
  Create('','');
end;

constructor TPoFamily.Create(const MasterName: String);
begin
  Create(MasterName, '');
end;

constructor TPoFamily.Create(const AMasterName, AChildName: String);
begin
  if (AMasterName <> '') then
  begin
    FMaster := TSimplePoFile.Create(AMasterName, True);
    FMasterName := AMasterName;
    //debugln('TPoFamily.Create: created ',FMasterName);
  end;
  if (AChildName <> '') then
  begin
    FChild := TSimplePoFile.Create(AChildName, True);
    FChildName := AChildName;
    //debugln('TPoFamily.Create: created ',FChildName);
  end;
end;

destructor TPoFamily.Destroy;
begin
  if Assigned(FMaster) then FMaster.Free;
  if Assigned(FChild) then FChild.Free;
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
    ErrorLog.Add(Format(sErrorsByTest,[sCheckNrOfItems]));
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

procedure TPoFamily.CheckFormatArgs(out ErrorCount: Integer; ErrorLog: TStrings; IgnoreFuzzyStrings: Boolean);
var
  i: Integer;
  CPoItem: TPOFileItem;
  IsFuzzy: Boolean;
begin
  //debugln('TPoFamily.CheckFormatArgs');
  DoTestStart(PoTestTypeNames[pttCheckFormatArgs], ShortChildName);
  ErrorCount := NoError;
  //for i := 0 to FMaster.Count - 1 do
  for i := 0 to FChild.Count - 1 do
  begin
    //debugln('  i = ',DbgS(i));
    //MPoItem := FMaster.PoItems[i];
    CPoItem := FChild.PoItems[i];
    //CPoItem := FChild.FindPoItem(MPoItem.Identifier);
    if Assigned(CPoItem) then
    begin
      IsFuzzy := (Pos('fuzzy', CPoItem.Flags) > 0);
      //if (IgnoreFuzzyStrings and IsFuzzy) then debugln('Skipping fuzzy translation: ',CPoItem.Translation);
      if (Length(CPoItem.Translation) > 0) and (not (IgnoreFuzzyStrings and IsFuzzy)) and (not CompareFormatArgs(CPoItem.Original, CPoItem.Translation)) then
      begin
        if (ErrorCount = 0) then
        begin
          ErrorLog.Add(Divider);
          ErrorLog.Add(Format(sErrorsByTest,[sCheckFormatArgs]));
          ErrorLog.Add(ShortChildName);
          ErrorLog.Add(Divider);
          ErrorLog.Add('');
        end;
        Inc(ErrorCount);
        ErrorLog.Add(Format(sIncompatibleFormatArgs,[CPoItem.LineNr]));
        ErrorLog.Add(Format(sFormatArgsID,[sCommentIdentifier, CPoItem.Identifier]));
        ErrorLog.Add(Format(sFormatArgsValues,[sMsgID,CPoItem.Original,sOriginal]));
        ErrorLog.Add(Format(sFormatArgsValues,[sMsgStr,CPoItem.Translation,sTranslation]));
        if IsFuzzy then ErrorLog.Add(sNoteTranslationIsFuzzy);
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
    if Assigned(MPoItem) and (MPoItem.Identifier <> '') then
    begin
      CPoItem := FChild.FindPoItem(MPoItem.Identifier);
      if not Assigned(CPoItem) then
      begin
        if (ErrorCount = 0) then
        begin
          ErrorLog.Add(Divider);
          ErrorLog.Add(Format(sErrorsByTest,[sCheckMissingIdentifiers]));
          ErrorLog.Add(ShortChildName);
          ErrorLog.Add(Divider);
          ErrorLog.Add('');
        end;
        Inc(ErrorCount);
        ErrorLog.Add(Format(sLineInFileName,
                            [MPoItem.LineNr,ShortMasterName]));
        ErrorLog.Add(Format(sIdentifierNotFoundIn,
                            [MPoItem.Identifier,ShortChildName]));
        ErrorLog.Add('');
      end;
    end;
  end;
  //Now reverse the search
  for i := 0 to FChild.Count - 1 do
  begin
    CPoItem := FChild.PoItems[i];
    if Assigned(CPoItem) and (CPoItem.Identifier <> '') then
    begin
      MPoItem := FMaster.FindPoItem(CPoItem.Identifier);
      if not Assigned(MPoItem) then
      begin
        if (ErrorCount = 0) then
        begin
          ErrorLog.Add(Divider);
          ErrorLog.Add(Format(sErrorsByTest,[sCheckMissingIdentifiers]));
          ErrorLog.Add(ShortChildName);
          ErrorLog.Add(Divider);
          ErrorLog.Add('');
        end;
        Inc(ErrorCount);
        ErrorLog.Add(Format(sLineNr,
                            [CPoItem.LineNr]));
        ErrorLog.Add(Format(sMissingMasterIdentifier,
                            [CPoItem.Identifier,ShortChildName,ShortMasterName]));
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
    CPoItem := FChild.FindPoItem(MpoItem.Identifier);
    if Assigned(CPoItem) then
    begin
      if (MPoItem.Original <> CPoItem.Original) then
      begin
        if (ErrorCount = 0) then
        begin
          ErrorLog.Add(Divider);
          ErrorLog.Add(Format(sErrorsByTest,[sCheckMismatchedOriginals]));
          ErrorLog.Add(ShortChildName);
          ErrorLog.Add(Divider);
          ErrorLog.Add('');
        end;
        Inc(ErrorCount);
        ErrorLog.Add(Format(sLineInFileName,[CpoItem.LineNr, ShortChildName]));
        ErrorLog.Add(Format(sMismatchOriginalsID,[CPoItem.Identifier]));
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
  LastHash, CurHash: Cardinal;
begin
  //debugln('TPoFamily.CheckMismatchedOriginals');
  DoTestStart(PoTestTypeNames[pttCheckDuplicateOriginals], ShortMasterName);
  WarningCount := 0;

  //debugln('TPoFamily.CehckDuplicateOriginals');
  //debugln('FMaster.OriginalList.Count = ',DbgS(FMaster.OriginalList.Count));
  LastHash := 0;
  for i := 0 to FMaster.OriginalList.Count - 1 do
  begin
    PoItem := TPoFileItem(FMaster.OriginalList.List[i]^.Data);
    if Assigned(PoItem) then
    begin
      CurHash := FMaster.OriginalList.List[i]^.HashValue ;
      if ((PoItem.Tag and tgHasDup) = tgHasDup) then
      begin
        if (WarningCount = 0) then
        begin
          ErrorLog.Add(Divider);
          ErrorLog.Add(Format(sErrorsByTest,[sCheckDuplicateOriginals]));
          ErrorLog.Add(ShortMasterName);
          ErrorLog.Add(Divider);
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
        ErrorLog.Add(format(sDuplicateLineNrWithValue,[PoItem.LineNr,PoItem.Identifier]));
        //debugln(format(sDuplicateLineNrWithValue,[PoItem.LineNr,PoItem.Identifier]));
      end;
    end;
  end;

  if (WarningCount > 0) then
  begin
    ErrorLog.Add('');
    ErrorLog.Add(Format(sNrWarningsFound,[WarningCount]));
    ErrorLog.Add(Divider);
    ErrorLog.Add('');
    ErrorLog.Add('');
  end;

  DoTestEnd(PoTestTypeNames[pttCheckDuplicateOriginals], WarningCount);
  //debugln('TPoFamily.CheckDuplicateOriginals: ',Dbgs(WarningCount),' Errors');
end;

procedure TPoFamily.CheckStatistics(out WarningCount: Integer; ErrorLog: TStrings);
var
  i: Integer;
  CPoItem: TPOFileItem;
  NrTranslated, NrUntranslated, NrFuzzy, NrTotal: Integer;
begin
  //debugln('TPoFamily.CheckFormatArgs');
  DoTestStart(sCheckStatistics, ShortChildName);
  NrTranslated := 0;
  NrUntranslated := 0;
  NrFuzzy := 0;
  //for i := 0 to FMaster.Count - 1 do
  for i := 0 to FChild.Count - 1 do
  begin
    //debugln('  i = ',DbgS(i));
    //MPoItem := FMaster.PoItems[i];
    CPoItem := FChild.PoItems[i];
    //CPoItem := FChild.FindPoItem(MPoItem.Identifier);
    if Assigned(CPoItem) then
    begin
      if (Length(CPoItem.Translation) > 0) then
      begin
        if (Pos('fuzzy', CPoItem.Flags) <> 0) then
          Inc(NrFuzzy)
        else
          Inc(NrTranslated);
      end
      else
      begin
        Inc(NrUntranslated)
      end;
    end;
  end;
  NrTotal := NrTranslated + NrUntranslated + NrFuzzy;
  if (NrTotal > 0) then
  begin
    ErrorLog.Add(Divider);
    ErrorLog.Add(sTranslationStatistics);
    ErrorLog.Add(ShortChildName);
    ErrorLog.Add(Format(sPercTranslated,[100.0*(NrTranslated/NrTotal)]));
    ErrorLog.Add(Format(sPercUntranslated,[100.0*(NrUntranslated/NrTotal)]));
    ErrorLog.Add(Format(sPercFuzzy,[100.0*(NrFuzzy/NrTotal)]));
    ErrorLog.Add('');
    ErrorLog.Add('');
  end;
  WarningCount := NrTotal;
  DoTestEnd(PoTestTypeNames[pttCheckFormatArgs], WarningCount);
  //debugln('TPoFamily.CheckIncompatibleFormatArgs: ',Dbgs(ErrorCount),' Errors');
end;

{
procedure TPoFamily.RunTests(const Options: TPoTestTypes; out
Pre conditions:
  * Master and a matching Child must be assigned at start ot testing
  * If a Child is assigned it must be child of Master
}
procedure TPoFamily.RunTests(const TestTypes: TPoTestTypes;  const TestOptions: TPoTestOptions;
                             out ErrorCount, WarningCount: Integer; ErrorLog: TStrings);
var
  SL: TStringList;
  CurrErrCnt, CurrWarnCnt: Integer;
  i: Integer;
  CurrChildName: String;
  S: String;
begin
  SL := nil;
  ErrorCount := NoError;
  WarningCount := NoError;
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
  if not Assigned(FChild) and not ((pttCheckDuplicateOriginals in Testtypes) or (ptoFindAllChildren in TestOptions)) then
  begin
    {$ifdef DebugSimplePoFiles}
    Debugln('TPoFamily.RunTests: no child assigned for ',ShortMasterName);
    {$endif}
    Exit;
  end;

  if (ptoFindAllChildren in TestOptions) then
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
    if (pttCheckDuplicateOriginals in TestTypes) then
    begin
      CheckDuplicateOriginals(CurrWarnCnt, ErrorLog);
      WarningCount := CurrWarnCnt + WarningCount;
    end;

    {$ifdef DebugSimplePoFiles}
    Debugln('TPoFamily.RunTests: number of childs for testing = ',DbgS(Sl.Count));
    {$endif}

    if (TestTypes - [pttCheckDuplicateOriginals] <> []) and (Sl.Count = 0) then
    begin
      {$ifdef DebugSimplePoFiles}
      Debugln('TPoFamily.RunTests: Warning: No child selected or found for selected tests');
      {$endif}
      Inc(WarningCount);
      ErrorLog.Add(Divider);
      ErrorLog.Add('Warning: No child selected (or found) for selected tests.');
      ErrorLog.Add(Divider);
    end;

    //then iterate all Children
    for i := 0 to SL.Count - 1 do
    begin
      CurrChildName := SL.Strings[i];
      //debugln('TPoFamily.RunTests: setting ChildName to ',CurrChildName);
      SetChildName(CurrChildName);

      if (pttCheckNrOfItems in TestTypes) then
      begin
        CheckNrOfItems(CurrErrCnt, ErrorLog);
        ErrorCount := CurrErrCnt + ErrorCount;
      end;

      if (pttCheckFormatArgs in TestTypes) then
      begin
        CheckFormatArgs(CurrErrCnt, ErrorLog, (ptoIgnoreFuzzyStrings in TestOptions));
        ErrorCount := CurrErrCnt + ErrorCount;
      end;


      if (pttCheckMissingIdentifiers in TestTypes) then
      begin
        CheckMissingIdentifiers(CurrErrCnt, ErrorLog);
        ErrorCount := CurrErrCnt + ErrorCount;
      end;

      if (pttCheckMismatchedOriginals in TestTypes) then
      begin
        CheckMismatchedOriginals(CurrErrCnt, ErrorLog);
        ErrorCount := CurrErrCnt + ErrorCount;
      end;

      if (pttCheckStatistics in TestTypes) then
      begin
        CheckStatistics(CurrErrCnt, ErrorLog);
        ErrorCount := CurrErrCnt + ErrorCount;
      end;
       {
        if (ptt in TestTypes) then
        begin
          Check(CurrErrCnt, ErrorLog);
          ErrorCount := CurrErrCnt + ErrorCount;
        end;
        }
    end;
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

