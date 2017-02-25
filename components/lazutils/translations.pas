{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Author of SimplePoFiles: Bart Broersma

  Merge by: Giuliano Colla

  Abstract:
    Methods and classes for loading translations/localizations from po files.

    This unit is a merge of the Translations unit by Mattias Gaertner and the
    SimplePoFiles unit by Bart Broersma. Its purpose is to provide a single unit
    for easier maintenance.
    In addition the traditional functions, it provides facilities for checking and
    maintaining translations.

    A number of new properties and methods have been introduced, or exposed, namely:

    in TPOFileItem - Property LineNr
                     Property Identifier (deprecated but left in for compatibility)

    in TPOFile - Method CheckFormatArguments
                 Method CleanUp
                 Method FindPoItem
                 Property PoName
                 Property FormatChecked
    and many more - see the type declaration for details

  Example 1: Load a specific .po file:

    procedure TForm1.FormCreate(Sender: TObject);
    var
      PODirectory: String;
    begin
      PODirectory:='/path/to/lazarus/lcl/languages/';
      TranslateUnitResourceStrings('LCLStrConsts',PODirectory+'lcl.%s.po',
                                   'nl','');
      MessageDlg('Title','Text',mtInformation,[mbOk,mbCancel,mbYes],0);
    end;


  Example 2: Load the current language file using the GetLanguageIDs function
    of the gettext unit in the project lpr file:

    uses
      ...
      Translations, LCLProc;

    procedure TranslateLCL;
    var
      PODirectory, Lang, FallbackLang: String;
    begin
      PODirectory:='/path/to/lazarus/lcl/languages/';
      Lang:='';
      FallbackLang:='';
      LCLGetLanguageIDs(Lang,FallbackLang); // in unit LCLProc
      Translations.TranslateUnitResourceStrings('LCLStrConsts',
                                PODirectory+'lclstrconsts.%s.po',Lang,FallbackLang);
    end;

    begin
      TranslateLCL;
      Application.Initialize;
      Application.CreateForm(TForm1, Form1);
      Application.Run;
    end.

    Note for Mac OS X:
      The supported language IDs should be added into the application
      bundle property list to CFBundleLocalizations key, see
      lazarus.app/Contents/Info.plist for example.
}
unit Translations;

{$mode objfpc}{$H+}{$INLINE ON}

interface

uses
  Classes, SysUtils,
  {$IF FPC_FULLVERSION>=30001}jsonscanner,{$ENDIF} jsonparser, fpjson,
  // LazUtils
  FileUtil, LazFileUtils, LazUTF8, LazUTF8Classes, LConvEncoding, LazLogger,
  AvgLvlTree, StringHashList;

type
  TStringsType = (
    stLrj, // Lazarus resource string table in JSON format
    stRst, // FPC resource string table (before FPC 2.7.1)
    stRsj  // FPC resource string table in JSON format (since FPC 2.7.1)
    );
  TTranslateUnitResult = (turOK, turNoLang, turNoFBLang, turEmptyParam);

type
  { TPOFileItem }

  TPOFileItem = class
  public
    Tag: Integer;
    LineNr: Integer; // required by pochecker
    Comments: string;
    IdentifierLow: string; // lowercase
    Original: string;
    Translation: string;
    Flags: string;
    PreviousID: string;
    Context: string;
    constructor Create(const TheIdentifierLow, TheOriginal, TheTranslated: string);
    procedure ModifyFlag(const AFlag: string; Check: boolean);
    property Identifier: string read IdentifierLow; deprecated;
  end;

  { TPOFile }

  TPOFile = class
  protected
    FItems: TFPList;// list of TPOFileItem
    FIdentifierLowToItem: TStringToPointerTree; // lowercase identifier to TPOFileItem
    FIdentLowVarToItem: TStringHashList; // of TPOFileItem
    FOriginalToItem: TStringHashList; // of TPOFileItem
    FCharSet: String;
    FHeader: TPOFileItem;
    FAllEntries: boolean;
    FTag: Integer;
    FModified: boolean;
    FHelperList: TStringList;
    FModuleList: TStringList;
    // New fields
    FPoName: string;
    FNrTranslated: Integer;
    FNrUntranslated: Integer;
    FNrFuzzy: Integer;
    FNrErrors: Integer;
    FFormatChecked: Boolean;
    procedure RemoveTaggedItems(aTag: Integer);
    procedure RemoveUntaggedModules;
    function Remove(Index: Integer): TPOFileItem;
    procedure UpdateCounters(Item: TPOFileItem; Removed: Boolean);
    // used by pochecker
    function GetCount: Integer;
    procedure SetCharSet(const AValue: String);
    function GetPoItem(Index: Integer): TPoFileItem;
    procedure ReadPOText(AStream: TStream);
  public
    constructor Create(Full:Boolean=True);  //when loading from internal resource Full needs to be False
    constructor Create(const AFilename: String; Full: boolean=false; AllowChangeFuzzyFlag: boolean=true);
    constructor Create(AStream: TStream; Full: boolean=false; AllowChangeFuzzyFlag: boolean=true);
    destructor Destroy; override;
    procedure ReadPOText(const Txt: string);
    procedure Add(const Identifier, OriginalValue, TranslatedValue, Comments,
                        Context, Flags, PreviousID: string; SetFuzzy: boolean = false; LineNr: Integer = -1);
    function Translate(const Identifier, OriginalValue: String): String;
    Property CharSet: String read FCharSet;
    procedure Report;
    procedure Report(StartIndex, StopIndex: Integer; const DisplayHeader: Boolean); //pochecker
    procedure Report(Log: TStrings; StartIndex, StopIndex: Integer; const DisplayHeader: Boolean); //pochecker
    procedure CreateHeader;
    procedure UpdateStrings(InputLines:TStrings; SType: TStringsType);
    procedure SaveToStrings(OutLst: TStrings);
    procedure SaveToFile(const AFilename: string);
    procedure UpdateItem(const Identifier: string; Original: string);
    procedure UpdateTranslation(BasePOFile: TPOFile);
    procedure ClearModuleList;
    procedure AddToModuleList(Identifier: string);
    procedure UntagAll;

    procedure RemoveIdentifier(const AIdentifier: string);
    procedure RemoveOriginal(const AOriginal: string);
    procedure RemoveIdentifiers(AIdentifiers: TStrings);
    procedure RemoveOriginals(AOriginals: TStrings);

    property Tag: integer read FTag write FTag;
    property Modified: boolean read FModified;
    property Items: TFPList read FItems;
    // used by pochecker /pohelper
  public
    procedure CheckFormatArguments(AllowChangeFuzzyFlag: boolean=true);
    procedure CleanUp; // removes previous ID from non-fuzzy entries
    property PoName: String read FPoName;
    property PoRename: String write FPoName;
    property NrTranslated: Integer read FNrTranslated;
    property NrUntranslated: Integer read FNrUntranslated;
    property NrFuzzy: Integer read FNrFuzzy;
    property NrErrors: Integer read FNrErrors;
    function FindPoItem(const Identifier: String): TPoFileItem;
    function OriginalToItem(const Data: String): TPoFileItem;
    property OriginalList: TStringHashList read FOriginalToItem;
    property PoItems[Index: Integer]: TPoFileItem read GetPoItem;
    property Count: Integer read GetCount;
    property Header: TPOFileItem read FHeader;
    property FormatChecked: boolean read FFormatChecked;
  end;

  EPOFileError = class(Exception)
  public
    ResFileName: string;
    POFileName: string;
  end;

var
  SystemCharSetIsUTF8: Boolean = true;// the LCL interfaces expect UTF-8 as default
    // if you don't use UTF-8, install a proper widestring manager and set this
    // to false.

// translate resource strings for one unit
function TranslateUnitResourceStrings(const ResUnitName, BaseFilename,
  Lang, FallbackLang: string):TTranslateUnitResult; overload;
function TranslateUnitResourceStrings(const ResUnitName, AFilename: string
  ): boolean; overload;
function TranslateUnitResourceStrings(const ResUnitName:string; po: TPOFile): boolean; overload;

// translate all resource strings
function TranslateResourceStrings(po: TPOFile): boolean;
function TranslateResourceStrings(const AFilename: string): boolean;
procedure TranslateResourceStrings(const BaseFilename, Lang, FallbackLang: string);

function UTF8ToSystemCharSet(const s: string): string; inline;

function UpdatePoFile(RSTFiles: TStrings; const POFilename: string): boolean;
procedure UpdatePoFileTranslations(const BasePOFilename: string; BasePOFile: TPOFile = nil);

const
  tgHasDup = $01;
  sFuzzyFlag = 'fuzzy';
  sBadFormatFlag = 'badformat';


implementation

{$DEFINE CHECK_FORMAT}

function IsKey(Txt, Key: PChar): boolean;
begin
  if Txt=nil then exit(false);
  if Key=nil then exit(true);
  repeat
    if Key^=#0 then exit(true);
    if Txt^<>Key^ then exit(false);
    inc(Key);
    inc(Txt);
  until false;
end;

function GetUTF8String(TxtStart, TxtEnd: PChar): string; inline;
begin
  Result:=UTF8CStringToUTF8String(TxtStart,TxtEnd-TxtStart);
end;

function ComparePOItems(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TPOFileItem(Item1).IdentifierLow,
                        TPOFileItem(Item2).IdentifierLow);
end;

function UTF8ToSystemCharSet(const s: string): string; inline;
begin
  if SystemCharSetIsUTF8 then
    exit(s);
  {$IFDEF NoUTF8Translations}
  Result:=s;
  {$ELSE}
  Result:=UTF8ToSys(s);
  {$ENDIF}
end;

function SkipLineEndings(var P: PChar; var DecCount: Integer): Integer;
  procedure Skip;
  begin
    Dec(DecCount);
    Inc(P);
  end;
begin
  Result  := 0;
  while (P^ in [#10,#13]) do begin
    Inc(Result);
    if (P^=#13) then begin
      Skip;
      if P^=#10 then
        Skip;
    end else
      Skip;
  end;
end;

function CompareMultilinedStrings(const S1,S2: string): Integer;
var
  C1,C2,L1,L2: Integer;
  P1,P2: PChar;
begin
  L1 := Length(S1);
  L2 := Length(S2);
  P1 := pchar(S1);
  P2 := pchar(S2);
  Result := ord(P1^) - ord(P2^);

  while (Result=0) and (L1>0) and (L2>0) and (P1^<>#0) do begin
    if (P1^<>P2^) or (P1^ in [#10,#13]) then begin
      C1 := SkipLineEndings(P1, L1);
      C2 := SkipLineEndings(P2, L2);
      if (C1<>C2) then
        // different amount of lineendings
        result := C1-C2
      else
      if (C1=0) then
        // there are no lineendings at all, will end loop
        result := Ord(P1^)-Ord(P2^);
    end;
    Inc(P1); Inc(P2);
    Dec(L1); Dec(L2);
  end;

  // if strings are the same, check that all chars have been consumed
  // just in case there are unexpected chars in between, in this case
  // L1=L2=0;
  if Result=0 then
    Result := L1-L2;
end;

function StrToPoStr(const s:string):string;
var
  SrcPos, DestPos: Integer;
  NewLength: Integer;
begin
  NewLength:=length(s);
  for SrcPos:=1 to length(s) do
    if s[SrcPos] in ['"','\'] then inc(NewLength);
  if NewLength=length(s) then begin
    Result:=s;
  end else begin
    SetLength(Result,NewLength);
    DestPos:=1;
    for SrcPos:=1 to length(s) do begin
      case s[SrcPos] of
      '"','\':
        begin
          Result[DestPos]:='\';
          inc(DestPos);
          Result[DestPos]:=s[SrcPos];
          inc(DestPos);
        end;
      else
        Result[DestPos]:=s[SrcPos];
        inc(DestPos);
      end;
    end;
  end;
end;

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

function FindAllTranslatedPoFiles(const Filename: string): TStringList;
var
  Path: String;
  Name: String;
  NameOnly: String;
  Ext: String;
  FileInfo: TSearchRec;
  CurExt: String;
begin
  Result:=TStringList.Create;
  Path:=ExtractFilePath(Filename);
  Name:=ExtractFilename(Filename);
  Ext:=ExtractFileExt(Filename);
  NameOnly:=LeftStr(Name,length(Name)-length(Ext));
  if FindFirstUTF8(Path+GetAllFilesMask,faAnyFile,FileInfo)=0 then begin
    repeat
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
      or (CompareFilenames(FileInfo.Name,Name)=0) then continue;
      CurExt:=ExtractFileExt(FileInfo.Name);
      if (CompareFilenames(CurExt,'.po')<>0)
      or (CompareFilenames(LeftStr(FileInfo.Name,length(NameOnly)),NameOnly)<>0)
      then
        continue;
      Result.Add(Path+FileInfo.Name);
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
end;

procedure UpdatePoFileTranslations(const BasePOFilename: string; BasePOFile: TPOFile);
var
  j: Integer;
  Lines: TStringList;
  FreeBasePOFile: Boolean;
  TranslatedPOFile: TPOFile;
  E: EPOFileError;
begin
  // Update translated PO files
  FreeBasePOFile := false;
  Lines := FindAllTranslatedPoFiles(BasePOFilename);
  try
    for j:=0 to Lines.Count-1 do begin
      TranslatedPOFile := TPOFile.Create(Lines[j], true);
      try
        TranslatedPOFile.Tag:=1;
        if BasePOFile=nil then begin
          BasePOFile := TPOFile.Create(BasePOFilename, true);
          FreeBasePOFile := true;
        end;
        TranslatedPOFile.UpdateTranslation(BasePOFile);
        try
          TranslatedPOFile.SaveToFile(Lines[j]);
        except
          on Ex: Exception do begin
            E := EPOFileError.Create(Ex.Message);
            E.ResFileName:=Lines[j];
            E.POFileName:=BasePOFileName;
            raise E;
          end;
        end;
      finally
        TranslatedPOFile.Free;
      end;
    end;
  finally
    if FreeBasePOFile then
      BasePOFile.Free;
    Lines.Free;
  end;
end;

function UpdatePOFile(RSTFiles: TStrings; const POFilename: string): boolean;
var
  InputLines: TStringListUTF8;
  Filename: string;
  BasePoFile: TPoFile;
  i: Integer;
  E: EPOFileError;
begin
  Result := false;

  if (RSTFiles=nil) or (RSTFiles.Count=0) then begin
    if FileExistsUTF8(POFilename) then begin
      // just update translated po RSTFiles
      UpdatePoFileTranslations(POFilename);
    end;
    exit;
  end;

  InputLines := TStringListUTF8.Create;
  try
    // Read base po items
    if FileExistsUTF8(POFilename) then
      BasePOFile := TPOFile.Create(POFilename, true)
    else
      BasePOFile := TPOFile.Create;
    BasePOFile.Tag:=1;

    // Update po file with lrj, rst/rsj of RSTFiles
    for i:=0 to RSTFiles.Count-1 do begin
      Filename:=RSTFiles[i];
      if (CompareFileExt(Filename,'.lrj')=0) or
         (CompareFileExt(Filename,'.rst')=0) or
         (CompareFileExt(Filename,'.rsj')=0) then
        try
          //DebugLn('');
          //DebugLn(['AddFiles2Po Filename="',Filename,'"']);
          InputLines.Clear;
          InputLines.LoadFromFile(FileName);

          if CompareFileExt(Filename,'.lrj')=0 then
            BasePOFile.UpdateStrings(InputLines, stLrj)
          else
            if CompareFileExt(Filename,'.rsj')=0 then
              BasePOFile.UpdateStrings(InputLines, stRsj)
            else
              BasePOFile.UpdateStrings(InputLines, stRst);
        except
          on Ex: Exception do begin
            E := EPOFileError.Create(Ex.Message);
            E.ResFileName:=FileName;
            E.POFileName:=POFileName;
            raise E;
          end;
        end;
    end;
    BasePOFile.SaveToFile(POFilename);
    Result := BasePOFile.Modified;

    UpdatePoFileTranslations(POFilename,BasePoFile);

  finally
    InputLines.Free;
    BasePOFile.Free;
  end;
end;

function Translate (Name,Value : AnsiString; {%H-}Hash : Longint; arg:pointer) : AnsiString;
var
  po: TPOFile;
begin
  po:=TPOFile(arg);
  // get UTF8 string
  result := po.Translate(Name,Value);
  // convert UTF8 to current local
  if result<>'' then
    result:=UTF8ToSystemCharSet(result);
end;

function TranslateUnitResourceStrings(const ResUnitName, AFilename: string
  ): boolean;
var po: TPOFile;
begin
  //debugln('TranslateUnitResourceStrings) ResUnitName="',ResUnitName,'" AFilename="',AFilename,'"');
  if (ResUnitName='') or (AFilename='') or (not FileExistsUTF8(AFilename)) then
    exit;
  result:=false;
  po:=nil;
  try
    po:=TPOFile.Create(AFilename);
    result:=TranslateUnitResourceStrings(ResUnitName,po);
  finally
    po.free;
  end;
end;

function TranslateUnitResourceStrings(const ResUnitName: string; po: TPOFile): boolean;
begin
  Result:=false;
  try
    SetUnitResourceStrings(ResUnitName,@Translate,po);
    Result:=true;
  except
    on e: Exception do begin
      {$IFnDEF DisableChecks}
      DebugLn('Exception while translating ', ResUnitName);
      DebugLn(e.Message);
      DumpExceptionBackTrace;
      {$ENDIF}
    end;
  end;
end;

function TranslateUnitResourceStrings(const ResUnitName, BaseFilename,
  Lang, FallbackLang: string):TTranslateUnitResult;
begin
  Result:=turOK;                //Result: OK
  if (ResUnitName='') or (BaseFilename='') then
    Result:=turEmptyParam       //Result: empty Parameter
  else begin
    if (FallbackLang<>'') and FileExistsUTF8(Format(BaseFilename,[FallbackLang])) then
      TranslateUnitResourceStrings(ResUnitName,Format(BaseFilename,[FallbackLang]))
    else
      Result:=turNoFBLang;      //Result: missing FallbackLang file
    if (Lang<>'') and FileExistsUTF8(Format(BaseFilename,[Lang])) then
      TranslateUnitResourceStrings(ResUnitName,Format(BaseFilename,[Lang]))
    else
      Result:=turNoLang;        //Result: missing Lang file
  end;
end;

function TranslateResourceStrings(po: TPOFile): boolean;
begin
  Result:=false;
  try
    SetResourceStrings(@Translate,po);
    Result:=true;
  except
    on e: Exception do begin
      {$IFnDEF DisableChecks}
      DebugLn('Exception while translating:');
      DebugLn(e.Message);
      DumpExceptionBackTrace;
      {$ENDIF}
    end;
  end;
end;

function TranslateResourceStrings(const AFilename: string): boolean;
var po: TPOFile;
begin
  //debugln('TranslateResourceStrings) ResUnitName,'" AFilename="',AFilename,'"');
  if (AFilename='') or (not FileExistsUTF8(AFilename)) then
    exit;
  result:=false;
  po:=nil;
  try
    po:=TPOFile.Create(AFilename);
    result:=TranslateResourceStrings(po);
  finally
    po.free;
  end;
end;

procedure TranslateResourceStrings(const BaseFilename, Lang, FallbackLang: string);
begin
  if (BaseFilename='') then exit;

  //debugln('TranslateResourceStrings BaseFilename="',BaseFilename,'"');
  if (FallbackLang<>'') then
    TranslateResourceStrings(Format(BaseFilename,[FallbackLang]));
  if (Lang<>'') then
    TranslateResourceStrings(Format(BaseFilename,[Lang]));
end;

{ TPOFile }

procedure TPOFile.RemoveUntaggedModules;
var
  Module: string;
  Item,VItem: TPOFileItem;
  i, p: Integer;
  VarName: String;
begin
  if FModuleList=nil then
    exit;

  // remove all module references that were not tagged
  for i:=FItems.Count-1 downto 0 do begin
    Item := TPOFileItem(FItems[i]);
    p := pos('.',Item.IdentifierLow);
    if P=0 then
      continue; // module not found (?)

    Module :=LeftStr(Item.IdentifierLow, p-1);
    if (FModuleList.IndexOf(Module)<0) then
      continue; // module was not modified this time

    if Item.Tag=FTag then
      continue; // PO item was updated

    // this item is not more in updated modules, delete it
    FIdentifierLowToItem.Remove(Item.IdentifierLow);
    // delete it also from VarToItem
    VarName := RightStr(Item.IdentifierLow, Length(Item.IdentifierLow)-P);
    VItem := TPoFileItem(FIdentLowVarToItem.Data[VarName]);
    if (VItem=Item) then
      FIdentLowVarToItem.Remove(VarName);

    FOriginalToItem.Remove(Item.Original, Item);
    FItems.Delete(i);
    Item.Free;
  end;
end;

function TPOFile.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TPOFile.SetCharSet(const AValue: String);
begin
  if (CompareText(FCharSet, AValue) = 0) then Exit;
  if (AValue = '') then FCharSet := 'UTF-8'
  else FCharSet := AValue;
end;

function TPOFile.GetPoItem(Index: Integer): TPoFileItem;
begin
  Result := TPoFileItem(FItems.Items[Index]);
end;

procedure TPOFile.ReadPOText(AStream: TStream);
var
  Size: Integer;
  s: string;
begin
  Size:=AStream.Size-AStream.Position;
  if Size<=0 then exit;
  SetLength(s,Size);
  AStream.Read(s[1],Size);
  ReadPOText(s);
end;

constructor TPOFile.Create(Full:Boolean=True);
begin
  inherited Create;
  FAllEntries:=Full;
  FItems:=TFPList.Create;
  FIdentifierLowToItem:=TStringToPointerTree.Create(true);
  FIdentLowVarToItem:=TStringHashList.Create(true);
  FOriginalToItem:=TStringHashList.Create(true);
end;

constructor TPOFile.Create(const AFilename: String; Full: boolean=false; AllowChangeFuzzyFlag: boolean=true);
var
  f: TStream;
begin
  FPoName := AFilename;
  f := TFileStreamUTF8.Create(AFilename, fmOpenRead or fmShareDenyNone);
  try
    Create(f, Full, AllowChangeFuzzyFlag);
    if FHeader=nil then
      CreateHeader;
  finally
    f.Free;
  end;
end;

constructor TPOFile.Create(AStream: TStream; Full: boolean=false; AllowChangeFuzzyFlag: boolean=true);
begin
  Create;

  FAllEntries := Full;

  ReadPOText(AStream);

  {$IFDEF CHECK_FORMAT}
  //AllowChangeFuzzyFlag allows not to change fuzzy flag for items with bad format arguments,
  //so there can be arguments with only badformat flag set. This is needed for POChecker.
  CheckFormatArguments(AllowChangeFuzzyFlag); // Verify that translation will not generate crashes
  if AllowChangeFuzzyFlag then
    CleanUp; // Removes previous ID from non-fuzzy entries (not needed for POChecker)
  {$ENDIF}
end;

destructor TPOFile.Destroy;
var
  i: Integer;
begin
  if FModuleList<>nil then
    FModuleList.Free;
  if FHelperList<>nil then
    FHelperList.Free;
  if FHeader<>nil then
    FHeader.Free;
  for i:=0 to FItems.Count-1 do
    TObject(FItems[i]).Free;
  FItems.Free;
  FIdentLowVarToItem.Free;
  FIdentifierLowToItem.Free;
  FOriginalToItem.Free;
  inherited Destroy;
end;

procedure TPOFile.ReadPOText(const Txt: string);
{ Read a .po file. Structure:

Example
#: lazarusidestrconsts:lisdonotshowsplashscreen
msgid "                      Do not show splash screen"
msgstr ""

}
type
  TMsg = (
    mid,
    mstr,
    mctx
    );
var
  l: Integer;
  LineLen: Integer;
  p: PChar;
  LineStart: PChar;
  LineEnd: PChar;
  Cnt: Integer;
  LineNr: Integer;
  Identifier: String;
  PrevMsgID: String;
  Comments: String;
  Flags: string;
  TextEnd: PChar;
  i: Integer;
  OldLineStartPos: PtrUInt;
  NewSrc: String;
  s: String;
  Handled: Boolean;
  CurMsg: TMsg;
  Msg: array[TMsg] of string;
  MsgStrFlag: boolean;

  procedure ResetVars;
  begin
    CurMsg:=mid;
    Msg[mid]:='';
    Msg[mstr]:='';
    Msg[mctx]:='';
    Identifier := '';
    Comments := '';
    Flags := '';
    PrevMsgID := '';
    MsgStrFlag := false;
  end;

  procedure AddEntry (LineNr: Integer);
  var
    Item: TPOFileItem;
    SetFuzzy: boolean;
  begin
    if Identifier<>'' then begin
      SetFuzzy:=false;
      // check for unresolved duplicates in po file
      Item := TPOFileItem(FOriginalToItem.Data[Msg[mid]]);
      if (Item<>nil) then begin
        // fix old duplicate context
        if Item.Context='' then
          Item.Context:=Item.IdentifierLow;
        // set context of new duplicate
        if Msg[mctx]='' then
          Msg[mctx] := Identifier;
        // if old duplicate was translated and new one is not,
        // provide an initial translation and set a flag to mark it fuzzy
        if (Msg[mstr]='') and (Item.Translation<>'') then begin
          // copy flags to new duplicate
          if Flags='' then
            Flags := Item.Flags;
          Msg[mstr] := Item.Translation;
          // if old item is fuzzy, copy PreviousID too
          if pos('fuzzy', Item.Flags)<>0 then
            PrevMsgID := Item.PreviousID;
          // mark newly translated item fuzzy
          SetFuzzy:=true;
        end;
      end;
      Add(Identifier,Msg[mid],Msg[mstr],Comments,Msg[mctx],Flags,PrevMsgID,SetFuzzy,LineNr);
      ResetVars;
    end else
    if (Msg[CurMsg]<>'') and (FHeader=nil) then begin
      FHeader := TPOFileItem.Create('',Msg[mid],Msg[CurMsg]);
      FHeader.Comments:=Comments;
      ResetVars;
    end
  end;

begin
  if Txt='' then exit;
  s:=Txt;
  l:=length(s);
  p:=PChar(s);
  LineStart:=p;
  TextEnd:=p+l;
  Cnt := 0;
  LineNr := 0;
  ResetVars;

  while LineStart<TextEnd do begin
    LineEnd:=LineStart;
    while (not (LineEnd^ in [#0,#10,#13])) do inc(LineEnd);
    LineLen:=LineEnd-LineStart;
    Inc(Cnt); // we must count also empty lines
    if LineLen>0 then begin
      Handled:=false;
      case LineStart^ of
      '#':
        begin
          if MsgStrFlag=true then begin
            //we detected comments after previous MsgStr. Consider it as start of new entry
            AddEntry(LineNr);
            inc(Cnt); // for empty line before comment
            LineNr := Cnt; // the comment line is the line number for this entry
            end;
          case LineStart[1] of
          ':':
            if LineStart[2]=' ' then begin
              // '#: '
              Identifier:=copy(s,LineStart-p+4,LineLen-3);
              // the RTL creates identifier paths with point instead of colons
              // fix it:
              for i:=1 to length(Identifier) do
                if Identifier[i]=':' then
                  Identifier[i]:='.';
              Handled:=true;
            end;
          '|':
            if IsKey(LineStart,'#| msgid "') then begin
              PrevMsgID:=PrevMsgID+GetUTF8String(LineStart+length('#| msgid "'),LineEnd-1);
              Handled:=true;
            end else if IsKey(LineStart, '#| "') then begin
              PrevMsgID := PrevMsgID + GetUTF8String(LineStart+length('#| "'),LineEnd-1);
              Handled:=true;
            end;
          ',':
            if LineStart[2]=' ' then begin
              // '#, '
              Flags := GetUTF8String(LineStart+3,LineEnd);
              Handled:=true;
            end;
          end;
          if not Handled then begin
            // '#'
            if Comments<>'' then
              Comments := Comments + LineEnding;
            // if comment is valid then store it, otherwise omit it
            if (LineStart[1]=' ') or (LineStart[1]='.') then
              Comments := Comments + GetUTF8String(LineStart+1,LineEnd)
            else
              GetUTF8String(LineStart+1,LineEnd);
            Handled:=true;
          end;
        end;
      'm':
        if (LineStart[1]='s') and (LineStart[2]='g') then begin
          case LineStart[3] of
          'i':
            if IsKey(LineStart,'msgid "') then begin
              CurMsg:=mid;
              Msg[CurMsg]:=Msg[CurMsg]+GetUTF8String(LineStart+length('msgid "'),LineEnd-1);
              Handled:=true;
            end;
          's':
            if IsKey(LineStart,'msgstr "') then begin
              MsgStrFlag:=true;
              CurMsg:=mstr;
              Msg[CurMsg]:=Msg[CurMsg]+GetUTF8String(LineStart+length('msgstr "'),LineEnd-1);
              Handled:=true;
            end;
          'c':
            if IsKey(LineStart, 'msgctxt "') then begin
              CurMsg:=mctx;
              Msg[CurMsg]:=Msg[CurMsg]+GetUTF8String(LineStart+length('msgctxt "'), LineEnd-1);
              Handled:=true;
            end;
          end;
        end;
      '"':
        begin
          if (Msg[mid]='')
          and IsKey(LineStart,'"Content-Type: text/plain; charset=') then
          begin
            FCharSet:=GetUTF8String(LineStart+length('"Content-Type: text/plain; charset='),LineEnd);
            if SysUtils.CompareText(FCharSet,'UTF-8')<>0 then begin
              // convert encoding to UTF-8
              OldLineStartPos:=PtrUInt(LineStart-PChar(s))+1;
              NewSrc:=ConvertEncoding(copy(s,OldLineStartPos,length(s)),
                                      FCharSet,EncodingUTF8);
              // replace text and update all pointers
              s:=copy(s,1,OldLineStartPos-1)+NewSrc;
              l:=length(s);
              p:=PChar(s);
              TextEnd:=p+l;
              LineStart:=p+(OldLineStartPos-1);
              LineEnd:=LineStart;
              while (not (LineEnd^ in [#0,#10,#13])) do inc(LineEnd);
              LineLen:=LineEnd-LineStart;
            end;
          end;
          // continuation
          Msg[CurMsg]:=Msg[CurMsg]+GetUTF8String(LineStart+1,LineEnd-1);
          Handled:=true;
        end;
      end;
      if not Handled then
        AddEntry(LineNr);
    end;
    LineStart:=LineEnd+1;
    while (LineStart^ in [#10,#13]) do inc(LineStart);
  end;
  AddEntry(LineNr);
end;

procedure TPOFile.RemoveIdentifiers(AIdentifiers: TStrings);
var
  I: Integer;
begin
  for I := 0 to AIdentifiers.Count - 1 do
    RemoveIdentifier(AIdentifiers[I]);
end;

procedure TPOFile.RemoveOriginals(AOriginals: TStrings);
var
  I: Integer;
begin
  for I := 0 to AOriginals.Count - 1 do
    RemoveOriginal(AOriginals[I]);
end;

procedure TPOFile.RemoveIdentifier(const AIdentifier: string);
var
  Index: Integer;
  Item: TPOFileItem;
begin
  if Length(AIdentifier) > 0 then
  begin
    Item := TPOFileItem(FIdentifierLowToItem[LowerCase(AIdentifier)]);
    if Item <> nil then
    begin
      Index := FItems.IndexOf(Item);
      // We should always find our item, unless there is data corruption.
      if Index >= 0 then
      begin
        Remove(Index);
        Item.Free;
      end;
    end;
  end;
end;

procedure TPOFile.RemoveOriginal(const AOriginal: string);
var
  Index: Integer;
  Item: TPOFileItem;
begin
  if Length(AOriginal) > 0 then
    // This search is expensive, it could be reimplemented using
    // yet another hash map which maps to items by "original" value
    // with stripped line ending characters.
    for Index := FItems.Count - 1 downto 0 do
    begin
      Item := TPOFileItem(FItems[Index]);
      if CompareMultilinedStrings(Item.Original, AOriginal) = 0 then
      begin
        Remove(Index);
        Item.Free;
      end;
    end;
end;

function TPOFile.Remove(Index: Integer): TPOFileItem;
var
  P: Integer;
begin
  Result := TPOFileItem(FItems[Index]);
  FOriginalToItem.Remove(Result.Original, Result);
  FIdentifierLowToItem.Remove(Result.IdentifierLow);
  P := Pos('.', Result.IdentifierLow);
  if P>0 then
    FIdentLowVarToItem.Remove(Copy(Result.IdentifierLow, P+1, Length(Result.IdentifierLow)));
  FItems.Delete(Index);
  UpdateCounters(Result, True);
end;

procedure TPOFile.Add(const Identifier, OriginalValue, TranslatedValue,
  Comments, Context, Flags, PreviousID: string; SetFuzzy: boolean = false; LineNr: Integer = -1);
var
  Item: TPOFileItem;
  p: Integer;
begin
  if (not FAllEntries) and (TranslatedValue='') then exit;
  Item:=TPOFileItem.Create(lowercase(Identifier),OriginalValue,TranslatedValue);
  Item.Comments:=Comments;
  Item.Context:=Context;
  Item.Flags:=Flags;
  if SetFuzzy = true then
    Item.ModifyFlag(sFuzzyFlag, true);
  Item.PreviousID:=PreviousID;
  Item.Tag:=FTag;
  Item.LineNr := LineNr;

  UpdateCounters(Item, False);

  FItems.Add(Item);

  //debugln(['TPOFile.Add Identifier=',Identifier,' Orig="',dbgstr(OriginalValue),'" Transl="',dbgstr(TranslatedValue),'"']);
  FIdentifierLowToItem[Item.IdentifierLow]:=Item;
  P := Pos('.', Identifier);
  if P>0 then
    FIdentLowVarToItem.Add(copy(Item.IdentifierLow, P+1, Length(Item.IdentifierLow)), Item);

  if OriginalValue<>'' then
    FOriginalToItem.Add(OriginalValue,Item);
end;

procedure TPOFile.UpdateCounters(Item: TPOFileItem; Removed: Boolean);
var
  IncrementBy: Integer;
begin
  if Removed then
    IncrementBy := -1
  else
    IncrementBy := 1;
  if Item.Translation = '' then
    Inc(FNrUntranslated, IncrementBy)
  else if Pos(sFuzzyFlag, Item.Flags)<>0 then
    Inc(FNrFuzzy, IncrementBy)
  else
    Inc(FNrTranslated, IncrementBy);
end;

function TPOFile.Translate(const Identifier, OriginalValue: String): String;
var
  Item: TPOFileItem;
  l: Integer;
begin
  Item:=TPOFileItem(FIdentifierLowToItem[lowercase(Identifier)]);
  if Item=nil then
    Item:=TPOFileItem(FOriginalToItem.Data[OriginalValue]);
  //Load translation only if it exists and is NOT fuzzy.
  //This matches gettext behaviour and allows to avoid a lot of crashes related
  //to formatting arguments mismatches.
  if (Item<>nil) and (pos(sFuzzyFlag, lowercase(Item.Flags))=0)
{$IFDEF CHECK_FORMAT}
  //Load translation only if it is not flagged as badformat.
  //This allows to avoid even more crashes related
  //to formatting arguments mismatches.
  and (pos(sBadFormatFlag, lowercase(Item.Flags))=0)
{$ENDIF}
  then begin
    Result:=Item.Translation;
    if Result='' then
      Raise Exception.Create('TPOFile.Translate Inconsistency');
  end else
    Result:=OriginalValue;
  //Remove lineending at the end of the string if present.
  //This is the case e.g. for multiline strings and not desired when assigning e.g. to
  //Caption property (can negatively affect form layout). In other cases it should not matter.
  l:=Length(Result);
  if l>1 then
  begin
    //Every string with #13 and/or #10 character at the end was treated as multiline, this means that
    //extra lineending could have been added to it.
    if RightStr(Result,2)=#13#10 then
    begin
      if l>2 then //do not leave the string empty
        SetLength(Result,l-2);
    end
    else
      if (Result[l]=#13) or (Result[l]=#10) then
        SetLength(Result,l-1);
  end;
end;

procedure TPOFile.Report;
var
  Item: TPOFileItem;
  i: Integer;
begin
  DebugLn('Header:');
  DebugLn('---------------------------------------------');

  if FHeader=nil then
    DebugLn('No header found in po file')
  else begin
    DebugLn('Comments=',FHeader.Comments);
    DebugLn('Identifier=',FHeader.IdentifierLow);
    DebugLn('msgid=',FHeader.Original);
    DebugLn('msgstr=', FHeader.Translation);
  end;
  DebugLn;

  DebugLn('Entries:');
  DebugLn('---------------------------------------------');
  for i:=0 to FItems.Count-1 do begin
    DebugLn(['#', i ,': ']);
    Item := TPOFileItem(FItems[i]);
    DebugLn('Comments=',Item.Comments);
    DebugLn('Identifier=',Item.IdentifierLow);
    DebugLn('msgid=',Item.Original);
    DebugLn('msgstr=', Item.Translation);
    DebugLn;
  end;

end;

procedure TPOFile.Report(StartIndex, StopIndex: Integer;
  const DisplayHeader: Boolean);
var
  Item: TPOFileItem;
  i: Integer;
begin
  if DisplayHeader then
  begin
    DebugLn('Header:');
    DebugLn('---------------------------------------------');

    if FHeader=nil then
      DebugLn('No header found in po file')
    else begin
      DebugLn('Comments=',FHeader.Comments);
      DebugLn('Identifier=',FHeader.IdentifierLow);
      DebugLn('msgid=',FHeader.Original);
      DebugLn('msgstr=', FHeader.Translation);
    end;
    DebugLn;
  end;

  if (StartIndex > StopIndex) then
  begin
    i := StopIndex;
    StopIndex := StartIndex;
    StartIndex := i;
  end;
  if (StopIndex > Count - 1) then StopIndex := Count - 1;
  if (StartIndex < 0) then StartIndex := 0;

  DebugLn(['Entries [', StartIndex, '..', StopIndex, ']:']);
  DebugLn('---------------------------------------------');
  for i := StartIndex to StopIndex do begin
    DebugLn(['#', i, ': ']);
    Item := TPOFileItem(FItems[i]);
    DebugLn('Identifier=',Item.IdentifierLow);
    DebugLn('msgid=',Item.Original);
    DebugLn('msgstr=', Item.Translation);
    DebugLn('Comments=',Item.Comments);
    DebugLn;
  end;
end;

procedure TPOFile.Report(Log: TStrings; StartIndex, StopIndex: Integer;
  const DisplayHeader: Boolean);
var
  Item: TPOFileItem;
  i: Integer;
begin
  if DisplayHeader then
  begin
    Log.Add('Header:');
    Log.Add('---------------------------------------------');

    if FHeader=nil then
      Log.Add('No header found in po file')
    else begin
      Log.Add('Comments='+FHeader.Comments);
      Log.Add('Identifier='+FHeader.IdentifierLow);
      Log.Add('msgid='+FHeader.Original);
      Log.Add('msgstr='+ FHeader.Translation);
    end;
    Log.Add('');
  end;

  if (StartIndex > StopIndex) then
  begin
    i := StopIndex;
    StopIndex := StartIndex;
    StartIndex := i;
  end;
  if (StopIndex > Count - 1) then StopIndex := Count - 1;
  if (StartIndex < 0) then StartIndex := 0;

  Log.Add(Format('Entries [%d..%d]:', [StartIndex, StopIndex]));
  Log.Add('---------------------------------------------');
  for i := StartIndex to StopIndex do begin
    Log.Add(Format('#%d: ', [i]));
    Item := TPOFileItem(FItems[i]);
    Log.Add('Identifier='+Item.IdentifierLow);
    Log.Add('msgid='+Item.Original);
    Log.Add('msgstr='+ Item.Translation);
    Log.Add('Comments='+Item.Comments);
    Log.Add('');
  end;
end;

procedure TPOFile.CreateHeader;
begin
  if FHeader=nil then
    FHeader := TPOFileItem.Create('','','');
  FHeader.Translation:='Content-Type: text/plain; charset=UTF-8';
  FHeader.Comments:='';
end;

procedure TPOFile.UpdateStrings(InputLines: TStrings; SType: TStringsType);
var
  i, j, n: integer;
  p: LongInt;
  Identifier, Value, Line: string;
  Ch: Char;
  MultiLinedValue: boolean;

  procedure NextLine;
  begin
    if i<InputLines.Count then
      inc(i);
    if i<InputLines.Count then
      Line := InputLines[i]
    else
      Line := '';
    n := Length(Line);
    p := 1;
  end;

  procedure NormalizeValue;
  begin
    if MultiLinedValue then begin
      // check that we end on lineending, multilined
      // resource strings from rst usually do not end
      // in lineending, fix here.
      if not (Value[Length(Value)] in [#13,#10]) then
        Value := Value + LineEnding;

      //treat #10#13 sequences as #13#10 for consistency,
      //e.g. #10#13#13#13#10#13#10 should become #13#10#13#13#10#13#10
      p:=2;
      while p<=Length(Value) do begin
        if (Value[p]=#13) and (Value[p-1]=#10) then begin
          Value[p]:=#10;
          Value[p-1]:=#13;
        end;
        // further analysis shouldn't affect found #13#10 pair
        if (Value[p]=#10) and (Value[p-1]=#13) then
          inc(p);
        inc(p);
      end;
      Value := AdjustLineBreaks(Value);
    end;
    // po requires special characters as #number
    p:=1;
    while p<=length(Value) do begin
      j := UTF8CharacterLength(pchar(@Value[p]));
      if (j=1) and (Value[p] in [#0..#9,#11,#12,#14..#31,#127..#255]) then
        Value := copy(Value,1,p-1)+'#'+IntToStr(ord(Value[p]))+copy(Value,p+1,length(Value))
      else
        inc(p,j);
    end;
  end;

  procedure UpdateFromRSJ;
  var
    Parser: TJSONParser;
    JsonItems, SourceBytes: TJSONArray;
    JsonData, JsonItem: TJSONObject;
    K, L: Integer;
    Data: TJSONData;
  begin
    Parser := TJSONParser.Create(InputLines.Text{$IF FPC_FULLVERSION>=30001},jsonscanner.DefaultOptions{$ENDIF});
    try
      JsonData := Parser.Parse as TJSONObject;
      try
        JsonItems := JsonData.Arrays['strings'];
        for K := 0 to JsonItems.Count - 1 do
        begin
          MultiLinedValue := false;
          JsonItem := JsonItems.Items[K] as TJSONObject;
          Data:=JsonItem.Find('sourcebytes');
          if Data is TJSONArray then begin
            // fpc 3.1.1 writes the bytes of the source without encoding change
            // while 'value' contains the string encoded as UTF16 with \u hexcodes.
            SourceBytes := TJSONArray(Data);
            SetLength(Value,SourceBytes.Count);
            for L := 1 to length(Value) do begin
              Value[L] := chr(SourceBytes.Integers[L-1]);
              if Value[L] in [#13,#10] then
                MultilinedValue := True;
            end;
          end else begin
            Value:=JsonItem.Get('value');
            // check if the value we got is multilined
            L := 1;
            while (L<=Length(Value)) and (MultiLinedValue = false) do begin
              if Value[L] in [#13,#10] then
                MultilinedValue := True;
              inc(L);
            end;
          end;
          if Value<>'' then begin
            NormalizeValue;
            UpdateItem(JsonItem.Get('name'), Value);
          end;
        end;
      finally
        JsonData.Free;
      end;
    finally
      Parser.Free;
    end;
  end;

begin
  ClearModuleList;
  UntagAll;
  if (SType = stLrj) or (SType = stRsj) then
    // .lrj/.rsj file
    UpdateFromRSJ
  else
  begin
    // for each string in lrt/rst/rsj list check if it's already in PO
    // if not add it
    MultilinedValue := false;
    Value := '';
    Identifier := '';
    i := 0;
    while i < InputLines.Count do begin

      Line := InputLines[i];
      n := Length(Line);

      if n=0 then
        // empty line
      else begin
        // .rst file
        if Line[1]='#' then begin
          // rst file: comment

          Value := '';
          Identifier := '';
          MultilinedValue := false;

        end else begin

          p:=Pos('=',Line);
          if P>0 then begin

            Identifier := copy(Line,1,p-1);
            inc(p); // points to ' after =

            Value := '';
            while p<=n do begin

              if Line[p]='''' then begin
                inc(p);
                j:=p;
                while (p<=n)and(Line[p]<>'''') do
                  inc(p);
                Value := Value + copy(Line, j, P-j);
                inc(p);
                continue;
              end else
              if Line[p] = '#' then begin
                // a #decimal
                repeat
                  inc(p);
                  j:=p;
                  while (p<=n)and(Line[p] in ['0'..'9']) do
                    inc(p);

                  Ch := Chr(StrToInt(copy(Line, j, p-j)));
                  Value := Value + Ch;
                  if Ch in [#13,#10] then
                    MultilinedValue := True;

                  if (p=n) and (Line[p]='+') then
                    NextLine;

                until (p>n) or (Line[p]<>'#');
              end else
              if Line[p]='+' then
                NextLine
              else
                inc(p); // this is an unexpected string
            end;

            if Value<>'' then begin
              NormalizeValue;
              UpdateItem(Identifier, Value);
            end;

          end; // if p>0 then begin
        end;
      end;

      inc(i);
    end;
  end;

  RemoveUntaggedModules;
end;

procedure TPOFile.SaveToStrings(OutLst: TStrings);
var
  j: Integer;

  procedure WriteLst(const AProp, AValue: string );
  var
    i: Integer;
    s: string;
  begin
    if (AValue='') and (AProp='') then
      exit;

    FHelperList.Text:=AValue;
    if FHelperList.Count=1 then begin
      if AProp='' then
        OutLst.Add(FHelperList[0])
      else begin
        if AProp='#' then
          //comments are not quoted
          OutLst.Add(AProp+FHelperList[0])
        else
          OutLst.Add(AProp+' "'+FHelperList[0]+'"');
      end;
    end else begin
      //comments are not quoted, instead prepend each line with '#'
      if (AProp<>'') and (AProp<>'#') then
        OutLst.Add(AProp+' ""');
      for i:=0 to FHelperList.Count-1 do begin
        s := FHelperList[i];
        if (AProp<>'') and (AProp<>'#') then begin
          s := '"' + s + '\n"';
          if AProp='#| msgid' then
            s := '#| ' + s;
        end else
          if AProp='#' then
            s := AProp + s;
        OutLst.Add(s)
      end;
    end;
  end;

  procedure WriteItem(Item: TPOFileItem);
  begin
    if Item.Comments<>'' then
      WriteLst('#', Item.Comments);
    if Item.IdentifierLow<>'' then
      OutLst.Add('#: '+Item.IdentifierLow);
    if Trim(Item.Flags)<>'' then
      OutLst.Add('#, '+Trim(Item.Flags));
    if Item.PreviousID<>'' then
      WriteLst('#| msgid', strToPoStr(Item.PreviousID));
    if Item.Context<>'' then
      WriteLst('msgctxt', Item.Context);
    WriteLst('msgid', StrToPoStr(Item.Original));
    WriteLst('msgstr', StrToPoStr(Item.Translation));
    OutLst.Add('');
  end;

begin
  if FHeader=nil then
    CreateHeader;

  if FHelperList=nil then
    FHelperList:=TStringList.Create;

  // write header
  WriteItem(FHeader);

  // Sort list of items by identifier
  FItems.Sort(@ComparePOItems);

  for j:=0 to Fitems.Count-1 do
    WriteItem(TPOFileItem(FItems[j]));
end;

// Remove all entries that have Tag=aTag
procedure TPOFile.RemoveTaggedItems(aTag: Integer);
var
  Item: TPOFileItem;
  i: Integer;
begin
  for i:=FItems.Count-1 downto 0 do
  begin
    Item := TPOFileItem(FItems[i]);
    if Item.Tag = aTag then
    begin
      Remove(i);
      Item.Free;
    end;
  end;
end;

procedure TPOFile.SaveToFile(const AFilename: string);
var
  OutLst: TStringListUTF8;
begin
  OutLst := TStringListUTF8.Create;
  try
    SaveToStrings(OutLst);
    OutLst.SaveToFile(AFilename);
  finally
    OutLst.Free;
  end;
end;

procedure TPOFile.UpdateItem(const Identifier: string; Original: string);
var
  Item: TPOFileItem;
  AContext,AComment,ATranslation,AFlags,APrevStr: string;
  SetFuzzy: boolean;
begin
  if FHelperList=nil then
    FHelperList := TStringList.Create;

  // try to find PO entry by identifier
  Item:=TPOFileItem(FIdentifierLowToItem[lowercase(Identifier)]);
  if Item<>nil then begin
    // found, update item value
    AddToModuleList(Identifier);

    if CompareMultilinedStrings(Item.Original, Original)<>0 then begin
      FModified := True;
      if Item.Translation<>'' then begin
        Item.ModifyFlag(sFuzzyFlag, true);
        Item.PreviousID:=Item.Original;
      end;
    end;
    Item.Original:=Original;
    Item.Tag:=FTag;
    exit;
  end;

  // try to find po entry based only on it's value
  SetFuzzy := false;
  AContext := '';
  AComment := '';
  ATranslation := '';
  AFlags := '';
  APrevStr := '';
  Item := TPOFileItem(FOriginalToItem.Data[Original]);
  if Item<>nil then begin
    // old item don't have context, add one
    if Item.Context='' then
      Item.Context := Item.IdentifierLow;

    // if old item is already translated use translation
    if Item.Translation<>'' then begin
      ATranslation := Item.Translation;
      // if old item is fuzzy, copy PreviousID too
      if pos(sFuzzyFlag, Item.Flags)<>0 then
        APrevStr := Item.PreviousID;
      // set a flag to mark item fuzzy if it is not already
      SetFuzzy := true;
    end;

    AFlags := Item.Flags;

    // update identifier list
    AContext := Identifier;
  end;

  // this appear to be a new item
  FModified := true;
  Add(Identifier, Original, ATranslation, AComment, AContext, AFlags, APrevStr, SetFuzzy);
end;

procedure TPOFile.UpdateTranslation(BasePOFile: TPOFile);
var
  Item: TPOFileItem;
  i: Integer;
begin
  UntagAll;
  ClearModuleList;
  for i:=0 to BasePOFile.Items.Count-1 do begin
    Item := TPOFileItem(BasePOFile.Items[i]);
    UpdateItem(Item.IdentifierLow, Item.Original);
  end;
  RemoveTaggedItems(0); // get rid of any item not existing in BasePOFile
end;

procedure TPOFile.ClearModuleList;
begin
  if FModuleList<>nil then
    FModuleList.Clear;
end;

procedure TPOFile.AddToModuleList(Identifier: string);
var
  p: Integer;
begin
  if FModuleList=nil then begin
    FModuleList := TStringList.Create;
    FModuleList.Duplicates:=dupIgnore;
  end;
  p := pos('.', Identifier);
  if p>0 then
    FModuleList.Add(LeftStr(Identifier, P-1));
end;

procedure TPOFile.UntagAll;
var
  Item: TPOFileItem;
  i: Integer;
begin
  for i:=0 to Items.Count-1 do begin
    Item := TPOFileItem(Items[i]);
    Item.Tag:=0;
  end;
end;

procedure TPOFile.CheckFormatArguments(AllowChangeFuzzyFlag: boolean=true);
var
  I: Integer;
  aPoItem: TPOFileItem;
  isFuzzy: boolean;
  isBadFormat: boolean;
begin
  FNrErrors := 0;
  for I := 0 to FItems.Count -1 do begin
    aPoItem := TPOFileItem(FItems.Items[I]);
    if aPoItem.Translation = '' then Continue;
    isFuzzy     := pos(sFuzzyFlag,aPoItem.Flags) <> 0;
    isBadFormat := pos(sBadFormatFlag,aPoItem.Flags) <> 0;
    if (pos('%',aPoItem.Original) <> 0) or (pos('%',aPoItem.Translation) <> 0) then begin
      if not CompareFormatArgs(aPoItem.Original,aPoItem.Translation) then begin
        inc(FNrErrors);
        if (not isFuzzy) and AllowChangeFuzzyFlag then begin
          aPoItem.ModifyFlag(sFuzzyFlag,true);
          inc(FNrFuzzy);
          dec(FNrTranslated);
          FModified := true;
        end;
        if not isBadFormat then begin
          aPoItem.ModifyFlag(sBadFormatFlag,true);
          FModified := true;
        end;
      end
      else begin //remove badformat flag (if present) from correct item
        if isBadFormat then begin
          aPoItem.ModifyFlag(sBadFormatFlag,False);
          FModified := true;
        end;
      end;
    end
    else begin // possibly an offending string has been removed
      if isBadFormat then begin
        aPoItem.ModifyFlag(sBadFormatFlag,False);
        FModified := true;
      end;
    end;
  end;
  FFormatChecked := true;
end;

procedure TPOFile.CleanUp;
var
  i: Integer;
  aPoItem: TPOFileItem;
  isFuzzy: boolean;
begin
  for i := 0 to FItems.Count -1 do begin
    aPoItem := TPOFileItem(FItems.Items[i]);
    isFuzzy := pos(sFuzzyFlag,aPoItem.Flags) <> 0;
    if not isFuzzy then
      // remove PreviousID from non-fuzzy Items
      if aPoItem.PreviousID <> '' then begin
        aPoItem.PreviousID := '';
        FModified := true;
      end;
    // is Context of some use ?
    {if aPoItem.Context = '' then begin
      aPoItem.Context := aPoItem.IdentifierLow;
      FModified := True;
      end;}
  end;
end;

function TPOFile.FindPoItem(const Identifier: String): TPoFileItem;
begin
  Result := TPOFileItem(FIdentifierLowToItem[lowercase(Identifier)]);
end;

function TPOFile.OriginalToItem(const Data: String): TPoFileItem;
begin
  // TODO: Should we take into account CompareMultilinedStrings ?
  Result := TPOFileItem(FOriginalToItem.Data[Data]);
end;

{ TPOFileItem }

constructor TPOFileItem.Create(const TheIdentifierLow, TheOriginal,
  TheTranslated: string);
begin
  IdentifierLow:=TheIdentifierLow;
  Original:=TheOriginal;
  Translation:=TheTranslated;
end;

procedure TPOFileItem.ModifyFlag(const AFlag: string; Check: boolean);
var
  i: Integer;
  F: TStringList;
begin
  F := TStringList.Create;
  try
    F.CommaText := Flags;
    i := F.IndexOf(AFlag);
    if (i<0) and Check then
      F.Add(AFlag)
    else
    if (i>=0) and (not Check) then
      F.Delete(i);
    Flags := F.CommaText;
  finally
    F.Free;
  end;
end;

end.

