unit PoFamilies;

{ $define DebugSimplePoFiles}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, StringHashList,
  //{$IFDEF UNIX}{$IFNDEF DisableCWString}, cwstring{$ENDIF}{$ENDIF},
  SimplePoFiles;

Type

  TPoTestOption = (ptoCheckNrOfItems, ptoCheckFormatArgs, ptoCheckMissingIdentifiers,
                   ptoCheckMismatchedOriginals, ptoCheckDuplicateOriginals,
                   ptoFindAllChilds);
  TPoTestOptions = Set of TPoTestOption;

const
    optRunAllTests: TPoTestOptions = [];
    optRunAllTestsOnAllChilds: TPoTestOptions = [];

    PoTestOptionNames: array[TPoTestOption] of String = (
      'Check number of items',
      'Check for incompatible format arguments',
      'Check missing identifiers',
      'Check for mismatches in untranslated strings',
      'Check for duplicate untranslated values',
      'Find all translated po-files'
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
    procedure CheckFormatArgs(out ErrorCount: Integer; ErrorLog: TStrings);
    procedure CheckMissingIdentifiers(out ErrorCount: Integer; ErrorLog: TStrings);
    procedure CheckMismatchedOriginals(out ErrorCount: Integer; ErrorLog: TStrings);
    procedure CheckDuplicateOriginals(out ErrorCount: Integer; ErrorLog: TStrings);

  public
    procedure RunTests(const Options: TPoTestOptions; out ErrorCount: Integer; ErrorLog: TStrings);

    property Master: TSimplePoFile read FMaster;
    property Child: TSimplePoFile read FChild;
    property MasterName: String read FMasterName write SetMasterName;
    property ChildName: String read FChildName write SetChildName;
    property ShortMasterName: String read  GetShortMasterName;
    property ShortChildName: String read GetShortChildName;
    property OnTestStart: TTestStartEvent read FOnTestStart write FOnTestStart;
    property OnTestEnd: TTestEndEvent read FOnTestEnd write FOnTestEnd;
  end;

function ExtractFormatArgs(S: String): String;
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

resourcestring
  sOriginal = 'Original';
  sTranslation = 'Translation';
  sErrorsByTest = 'Errors / warnings reported by %s for:';
  sCheckFormatArgs = 'CheckFormatArgs';
  sCheckMissingIdentifiers = 'CheckMissingIdentifiers';
  sCheckNrOfItems =  'CheckNrOfItems';
  sCheckMismatchedOriginals = 'CheckMismatchedOriginals';
  sCheckDuplicateOriginals = 'CheckDiplicateOriginals';
  sIncompatibleFormatArgs = '[Line: %d] Incompatible format() arguments for:' ;

  sNrErrorsFound = 'Found %d errors / warnings.';
  sLineInFileName = '[Line %d] in %s:';
  sIdentifierNotFoundIn = 'Identifier [%s] not found in %s';
  sMissingMasterIdentifier = 'Identifier [%s] found in %s, but it does not exist in %s';
  sLineNr = '[Line: %d]';

  sFormatArgsID = '%s %s';
  sFormatArgsValues = '%s%s"   (= %s)';

  sNrOfItemsMisMatch = 'Mismatch in number of items for master and child';
  sNrOfItemsMismatchM = '%s: %d items';
  sNrOfItemsMismatchC = '%s: %d items';

  sMismatchOriginalsID = '%s';
  sMismatchOriginalsM = '%s: %s';
  sMismatchOriginalsC = '%s: %s';

  sDuplicateOriginals = 'This resourcestring:';
  sDuplicateIdentifier = '#: %s';
  sDuplicateOriginal = 'msgid "%s"';
  sDuplicateContext = 'msgctxt "%s"';
  sDuplicateOriginals2 = 'has the same value as idenftifier %s at line %d';
  //sDuplicateOriginals3 = 'For this entry it is recommended to set: msgctxt="%s"';

//Helper functions

function ExtractFormatArgs(S: String): String;
const
  FormatSpecs = ['D','E','F','G','M','N','P','S','U','X'];
var
  i,p: Integer;
  InFormat: Boolean;
  NewStr: String;
  c: Char;
begin
  SetLength(NewStr, Length(S));
  InFormat := False;
  p := 0;
  for i := 1 to length(S) do
  begin
    c := S[i];
    if (c = '%') then InFormat := not InFormat;
    //debugln('i = ',dbgs(i),' c = ',c,' InFormat = ',dbgs(informat));
    if InFormat and (UpCase(c) in (FormatSpecs+['%']))  then
    begin
      begin
        Inc(p);
        NewStr[p] := c;
      end;
    end
    else
    begin
      if (c = '%') and (i > 1) and (S[i-1] = '%')  and (p > 0) and (NewStr[p] = '%') then
      begin//2 consecutive % means a literal % and is not a format specifier
        //debugln('p = ',dbgs(p), 'i = ',dbgs(i));
        NewStr[p] := '#';
        Dec(p);
      end;
    end;
    if InFormat and (Upcase(c) in FormatSpecs) then InFormat := False;
  end;
  SetLength(NewStr, p);
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
begin
  Result := CompareText(ExtractFormatArgs(S1), ExtractFormatArgs(S2)) = 0;
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
  DoTestStart(PoTestOptionNames[ptoCheckNrOfItems], ShortChildName);
  if (FMaster.Count <> FChild.Count) then
  begin
    ErrorCount := 1;
    ErrorLog.Add(Divider);
    ErrorLog.Add(Format(sErrorsByTest,[sCheckNrOfItems]));
    ErrorLog.Add(ShortChildName);
    ErrorLog.Add(Divider);
    ErrorLog.Add('');
    ErrorLog.Add(sNrOfItemsMismatch);
    ErrorLog.Add(Format(sNrOfItemsMismatchM,[ShortMasterName,FMaster.Count]));
    ErrorLog.Add(Format(sNrOfItemsMismatchC,[ShortChildName,FChild.Count]));
    ErrorLog.Add(Divider);
    ErrorLog.Add('');
    ErrorLog.Add('');
  end
  else ErrorCount := NoError;
  DoTestEnd(PoTestOptionNames[ptoCheckNrOfItems], ErrorCount);
  //debugln('TPoFamily.CheckNrOfItemsMismatch: ',Dbgs(ErrorCount),' Errors');
end;

procedure TPoFamily.CheckFormatArgs(out ErrorCount: Integer; ErrorLog: TStrings);
var
  i: Integer;
  CPoItem: TPOFileItem;
begin
  //debugln('TPoFamily.CheckFormatArgs');
  DoTestStart(PoTestOptionNames[ptoCheckFormatArgs], ShortChildName);
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
      if (Pos('%', CPoItem.Translation) > 0) and not CompareFormatArgs(CPoItem.Original, CPoItem.Translation) then
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
  DoTestEnd(PoTestOptionNames[ptoCheckFormatArgs], ErrorCount);
  //debugln('TPoFamily.CheckIncompatibleFormatArgs: ',Dbgs(ErrorCount),' Errors');
end;

procedure TPoFamily.CheckMissingIdentifiers(out ErrorCount: Integer;
  ErrorLog: TStrings);
var
  i: Integer;
  MPoItem, CPoItem: TPOFileItem;
begin
  //debugln('TPoFamily.CheckMissingIdentifiers');
  DoTestStart(PoTestOptionNames[ptoCheckMissingIdentifiers], ShortChildName);
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
  DoTestEnd(PoTestOptionNames[ptoCheckMissingIdentifiers], ErrorCount);
  //debugln('TPoFamily.CheckMissingIdentifiers: ',Dbgs(ErrorCount),' Errors');
end;

procedure TPoFamily.CheckMismatchedOriginals(out ErrorCount: Integer;
  ErrorLog: TStrings);
var
  i: Integer;
  MPoItem, CPoItem: TPOFileItem;
begin
  //debugln('TPoFamily.CheckMismatchedOriginals');
  DoTestStart(PoTestOptionNames[ptoCheckMismatchedOriginals], ShortChildName);
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
  DoTestEnd(PoTestOptionNames[ptoCheckMismatchedOriginals], ErrorCount);
  //debugln('TPoFamily.CheckMismatchedOriginals: ',Dbgs(ErrorCount),' Errors');
end;

procedure TPoFamily.CheckDuplicateOriginals(out ErrorCount: Integer;
  ErrorLog: TStrings);
var
  i: Integer;
  PoItem, Dup: TPOFileItem;
begin
  //debugln('TPoFamily.CheckMismatchedOriginals');
  DoTestStart(PoTestOptionNames[ptoCheckDuplicateOriginals], ShortMasterName);
  ErrorCount := NoError;
  for i := FMaster.Count - 1 downto 0 do
  begin
    PoItem := FMaster.PoItems[i];
    Dup := FMaster.OriginalToItem(PoItem.Original);
    if Assigned(Dup) and (Dup.Identifier <> PoItem.Identifier) {and (Dup.Context = '')} then
    begin
      if (ErrorCount = 0) then
      begin
        ErrorLog.Add(Divider);
        ErrorLog.Add(Format(sErrorsByTest,[sCheckDuplicateOriginals]));
        ErrorLog.Add(ShortMasterName);
        ErrorLog.Add(Divider);
        ErrorLog.Add('');
      end;
      Inc(ErrorCount);
      ErrorLog.Add(Format(sLineNr,[PoItem.LineNr]));
      ErrorLog.Add(sDuplicateOriginals);
      ErrorLog.Add(Format(sDuplicateIdentifier,[PoItem.Identifier]));
      ErrorLog.Add(Format(sDuplicateOriginal,[PoItem.Original]));
      ErrorLog.Add(Format(sDuplicateContext,[PoItem.Context]));
      ErrorLog.Add(Format(sDuplicateOriginals2,[Dup.Identifier,Dup.LineNr]));
      //ErrorLog.Add(Format(sDuplicateOriginals3,[PoItem.Identifier]));
      ErrorLog.Add('');
    end;
  end;
  if (ErrorCount > 0) then
  begin
    ErrorLog.Add(Format(sNrErrorsFound,[ErrorCount]));
    ErrorLog.Add(Divider);
    ErrorLog.Add('');
    ErrorLog.Add('');
  end;
  DoTestEnd(PoTestOptionNames[ptoCheckDuplicateOriginals], ErrorCount);
  //debugln('TPoFamily.CheckDuplicateOriginals: ',Dbgs(ErrorCount),' Errors');
end;

{
procedure TPoFamily.RunTests(const Options: TPoTestOptions; out
Pre conditions:
  * Master and a matching Child must be assigned at start ot testing
  * If a Child is assigned it must be child of Master
}
procedure TPoFamily.RunTests(const Options: TPoTestOptions; out
  ErrorCount: Integer; ErrorLog: TStrings);
var
  SL: TStringList;
  CurrErrCnt: Integer;
  i: Integer;
  CurrChildName: String;
  S: String;
begin
  SL := nil;
  ErrorCount := NoError;
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
  if not Assigned(FChild) and ([ptoFindAllChilds, ptoCheckDuplicateOriginals] * Options = []) then
  begin
    {$ifdef DebugSimplePoFiles}
    Debugln('TPoFamily.RunTests: no child assigned for ',ShortMasterName);
    {$endif}
    Exit;
  end;

  if (ptoFindAllChilds in Options) then
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
    Sl.Add(FChildName);
  end;

//  for i := 0 to sl.count - 1 do debugln(extractfilename(sl.strings[i]));

  try

    //First run checks that are Master-only
    if (ptoCheckDuplicateOriginals in Options) then
    begin
      CheckDuplicateOriginals(CurrErrCnt, ErrorLog);
      ErrorCount := CurrErrCnt + ErrorCount;
    end;

    //then iterate all Children
    for i := 0 to SL.Count - 1 do
    begin
      CurrChildName := SL.Strings[i];
      //debugln('TPoFamily.RunTests: setting ChildName to ',CurrChildName);
      SetChildName(CurrChildName);

      if (ptoCheckNrOfItems in Options) then
      begin
        CheckNrOfItems(CurrErrCnt, ErrorLog);
        ErrorCount := CurrErrCnt + ErrorCount;
      end;

      if (ptoCheckFormatArgs in Options) then
      begin
        CheckFormatArgs(CurrErrCnt, ErrorLog);
        ErrorCount := CurrErrCnt + ErrorCount;
      end;


      if (ptoCheckMissingIdentifiers in Options) then
      begin
        CheckMissingIdentifiers(CurrErrCnt, ErrorLog);
        ErrorCount := CurrErrCnt + ErrorCount;
      end;

      if (ptoCheckMismatchedOriginals in Options) then
      begin
        CheckMismatchedOriginals(CurrErrCnt, ErrorLog);
        ErrorCount := CurrErrCnt + ErrorCount;
      end;

       {
        if (pto in Options) then
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
  Index: TPoTestOption;
begin
  for Index := Low(TPoTestOption) to High(TPotestOption) do optRunAllTestsOnAllChilds := optRunAllTestsOnAllChilds + [Index];
  optRunAllTests := optRunAllTestsOnAllChilds - [ptoFindAllChilds];
end;

Initialization

InitTestOptions;

end.

