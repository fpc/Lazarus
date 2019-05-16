{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Initial authors: Mattias Gaertner, Bart Broersma, Giuliano Colla

  Abstract:
    Methods and classes for loading/checking/maintaining translations from po files.

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
  FileUtil, LazFileUtils, LazUTF8, LazUTF8Classes, LConvEncoding, LazLoggerBase,
  AvgLvlTree, StringHashList;

type
  TStringsType = (
    stLrj, // Lazarus resource string table in JSON format
    stRst, // FPC resource string table (before FPC 2.7.1)
    stRsj  // FPC resource string table in JSON format (since FPC 2.7.1)
    );
  TTranslateUnitResult = (turOK, turNoLang, turNoFBLang, turEmptyParam);

  TTranslationStatistics = record
    Translated: Integer;
    Untranslated: Integer;
    Fuzzy: Integer;
  end;

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
    Duplicate: boolean;
    constructor Create(const TheIdentifierLow, TheOriginal, TheTranslated: string);
    // Can accept the comma separated list of flags
    // Returns true if the Flags property has been modified
    function ModifyFlag(const AFlags: string; Check: boolean): boolean;
  end;

  { TPOFile }

  TPOFile = class
  private
    FAllowChangeFuzzyFlag: boolean;
    FStatisticsUpdated: boolean;
    FStatistics: TTranslationStatistics;
    function GetStatistics: TTranslationStatistics;
  protected
    FItems: TFPList;// list of TPOFileItem
    FIdentifierLowToItem: TStringToPointerTree; // lowercase identifier to TPOFileItem
    FOriginalToItem: TStringHashList; // of TPOFileItem
    FCharSet: String;
    FHeader: TPOFileItem;
    FAllEntries: boolean;
    FTag: Integer;
    FModified: boolean;
    FHelperList: TStringList;
    // New fields
    FPoName: string;
    function Remove(Index: Integer): TPOFileItem;
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
    function Translate(const Identifier, OriginalValue: String): String;
    Property CharSet: String read FCharSet;
    procedure Report;
    procedure Report(StartIndex, StopIndex: Integer; const DisplayHeader: Boolean); //pochecker
    procedure Report(Log: TStrings; StartIndex, StopIndex: Integer; const DisplayHeader: Boolean); //pochecker
    procedure CreateHeader;
    procedure UpdateStrings(InputLines:TStrings; SType: TStringsType);
    procedure SaveToStrings(OutLst: TStrings);
    procedure SaveToFile(const AFilename: string);
    procedure UpdateItem(const Identifier, Original: string; const Flags: string = '';
      const ProcessingTranslation: boolean = false);
    procedure FillItem(var CurrentItem: TPOFileItem; Identifier, Original,
      Translation, Comments, Context, Flags, PreviousID: string; LineNr: Integer = -1);
    procedure UpdateTranslation(BasePOFile: TPOFile);

    procedure UntagAll;
    procedure RemoveTaggedItems(aTag: Integer);

    procedure RemoveIdentifier(const AIdentifier: string);
    procedure RemoveOriginal(const AOriginal: string);
    procedure RemoveIdentifiers(AIdentifiers: TStrings);
    procedure RemoveOriginals(AOriginals: TStrings);

    property Tag: integer read FTag write FTag;
    property Modified: boolean read FModified;
    property Items: TFPList read FItems;
    // used by pochecker /pohelper
  public
    procedure CleanUp; // removes previous ID from non-fuzzy entries
    property PoName: String read FPoName;
    property PoRename: String write FPoName;
    property Statistics: TTranslationStatistics read GetStatistics;
    procedure InvalidateStatistics;
    function FindPoItem(const Identifier: String): TPoFileItem;
    function OriginalToItem(const Data: String): TPoFileItem;
    property PoItems[Index: Integer]: TPoFileItem read GetPoItem;
    property Count: Integer read GetCount;
    property Header: TPOFileItem read FHeader;
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

function GetPOFilenameParts(const Filename: string; out AUnitName, Language: string): boolean;
function FindAllTranslatedPoFiles(const Filename: string): TStringList;

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
  sFuzzyFlag = 'fuzzy';
  sBadFormatFlag = 'badformat';
  sFormatFlag = 'object-pascal-format';
  sNoFormatFlag = 'no-object-pascal-format';


implementation

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
    if s[SrcPos] in ['"','\',#9] then inc(NewLength);
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
      #9:
        begin
          Result[DestPos]:='\';
          inc(DestPos);
          Result[DestPos]:='t';
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

function GetPOFilenameParts(const Filename: string; out AUnitName, Language: string): boolean;
var
  NameWithoutExt, Ext: string;
  ExtLength: Integer;
begin
  Result:=false;
  AUnitName:='';
  Language:='';
  if CompareFileExt(Filename, '.po', false)=0 then
  begin
    NameWithoutExt:=ExtractFileNameWithoutExt(Filename);
    Ext:=ExtractFileExt(NameWithoutExt);
    ExtLength:=Length(Ext);
    if ExtLength>1 then
    begin
      AUnitName:=copy(NameWithoutExt, 1, Length(NameWithoutExt)-ExtLength);
      Language:=copy(Ext, 2, ExtLength-1);
      Result:=true;
    end;
  end;
end;

function FindAllTranslatedPoFiles(const Filename: string): TStringList;
var
  Path: String;
  NameOnly: String;
  FileInfo: TSearchRec;
  CurUnitName: String;
  CurLang: String;
begin
  Result:=TStringList.Create;
  Path:=ExtractFilePath(Filename);
  NameOnly:=ExtractFileNameOnly(Filename);
  if FindFirstUTF8(Path+GetAllFilesMask,faAnyFile,FileInfo)=0 then
    repeat
      if GetPOFilenameParts(FileInfo.Name, CurUnitName, CurLang) and (NameOnly=CurUnitName) then
        Result.Add(Path+FileInfo.Name);
    until FindNextUTF8(FileInfo)<>0;
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
    // untagging is done only once for BasePoFile
    BasePOFile.UntagAll;

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
    // once all rst/rsj/lrj files are processed, remove all unneeded (missing in them) items
    BasePOFile.RemoveTaggedItems(0);

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
var
  po: TPOFile;
begin
  //debugln('TranslateResourceStrings) AFilename="',AFilename,'"');
  if (AFilename='') or (not FileExistsUTF8(AFilename)) then
    exit;
  Result:=false;
  po:=nil;
  try
    po:=TPOFile.Create(AFilename);
    Result:=TranslateResourceStrings(po);
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
  // changing 'fuzzy' flag is allowed by default
  FAllowChangeFuzzyFlag:=true;
  FItems:=TFPList.Create;
  FIdentifierLowToItem:=TStringToPointerTree.Create(true);
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
  //AllowChangeFuzzyFlag allows not to change fuzzy flag for items with bad format arguments,
  //so there can be arguments with only badformat flag set. This is needed for POChecker.
  FAllowChangeFuzzyFlag := AllowChangeFuzzyFlag;

  ReadPOText(AStream);

  if AllowChangeFuzzyFlag then
    CleanUp; // Removes previous ID from non-fuzzy entries (not needed for POChecker)
  InvalidateStatistics;
end;

destructor TPOFile.Destroy;
var
  i: Integer;
begin
  if FHelperList<>nil then
    FHelperList.Free;
  if FHeader<>nil then
    FHeader.Free;
  for i:=0 to FItems.Count-1 do
    TObject(FItems[i]).Free;
  FItems.Free;
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
    mctxt
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
    Msg[mctxt]:='';
    Identifier := '';
    Comments := '';
    Flags := '';
    PrevMsgID := '';
    MsgStrFlag := false;
  end;

  procedure AddEntry (LineNr: Integer);
  var
    Item: TPOFileItem;
  begin
    Item := nil;
    if Identifier<>'' then begin
      FillItem(Item,Identifier,Msg[mid],Msg[mstr],Comments,Msg[mctxt],Flags,PrevMsgID,LineNr);
      ResetVars;
    end
    else if (Msg[CurMsg]<>'') and (FHeader=nil) then begin
      FHeader := TPOFileItem.Create('',Msg[mid],Msg[CurMsg]);
      FHeader.Comments:=Comments;
      ResetVars;
    end;
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
              CurMsg:=mctxt;
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
  FModified := false;
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

function TPOFile.GetStatistics: TTranslationStatistics;
var
  Item: TPOFileItem;
  i: Integer;
begin
  if FStatisticsUpdated = false then
  begin
    FStatistics.Translated := 0;
    FStatistics.Untranslated := 0;
    FStatistics.Fuzzy := 0;
    for i:=0 to Items.Count-1 do
    begin
      Item := TPOFileItem(FItems[i]);
      if Item.Translation = '' then
        Inc(FStatistics.Untranslated)
      else
        if Pos(sFuzzyFlag, Item.Flags)<>0 then
          Inc(FStatistics.Fuzzy)
        else
          Inc(FStatistics.Translated);
    end;
    FStatisticsUpdated := true;
  end;
  Result.Translated := FStatistics.Translated;
  Result.Untranslated := FStatistics.Untranslated;
  Result.Fuzzy := FStatistics.Fuzzy;
end;

function TPOFile.Remove(Index: Integer): TPOFileItem;
begin
  Result := TPOFileItem(FItems[Index]);
  FOriginalToItem.Remove(Result.Original, Result);
  FIdentifierLowToItem.Remove(Result.IdentifierLow);
  FItems.Delete(Index);
end;

function TPOFile.Translate(const Identifier, OriginalValue: String): String;
var
  Item: TPOFileItem;
begin
  Item:=TPOFileItem(FIdentifierLowToItem[lowercase(Identifier)]);
  if Item=nil then
    Item:=TPOFileItem(FOriginalToItem.Data[OriginalValue]);
  //Load translation only if it exists and is NOT fuzzy.
  //This matches gettext behaviour and allows to avoid a lot of crashes related
  //to formatting arguments mismatches.
  if (Item<>nil) and (pos(sFuzzyFlag, Item.Flags)=0)
  //Load translation only if it is not flagged as badformat.
  //This allows to avoid even more crashes related
  //to formatting arguments mismatches.
  and (pos(sBadFormatFlag, Item.Flags)=0) then
  begin
    if Item.Translation<>'' then
      Result:=Item.Translation
    else
      Result:=Item.Original;
    if Result='' then
      Raise Exception.Create('TPOFile.Translate Inconsistency');
  end else
    Result:=OriginalValue;
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

    // escape special characters as #number, do not confuse translators
    p:=1;
    while p<=length(Value) do begin
      j := UTF8CodepointSize(pchar(@Value[p]));
      if (j=1) and (Value[p] in [#0..#8,#11,#12,#14..#31,#127..#255]) then
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
          JsonItem := JsonItems.Items[K] as TJSONObject;
          Data:=JsonItem.Find('sourcebytes');
          if Data is TJSONArray then begin
            // fpc 3.1.1 writes the bytes of the source without encoding change
            // while 'value' contains the string encoded as UTF16 with \u hexcodes.
            SourceBytes := TJSONArray(Data);
            SetLength(Value,SourceBytes.Count);
            for L := 1 to length(Value) do
              Value[L] := chr(SourceBytes.Integers[L-1]);
          end else
            Value:=JsonItem.Get('value');
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
  if (SType = stLrj) or (SType = stRsj) then
    // .lrj/.rsj file
    UpdateFromRSJ
  else
  begin
    // for each string in lrt/rst/rsj list check if it's already in PO
    // if not add it
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
end;

procedure TPOFile.SaveToStrings(OutLst: TStrings);
var
  j: Integer;

  procedure WriteLst(const AProp, AValue: string );
  var
    i: Integer;
    s: string;
    ValueHasTrailingLineEnding: boolean;
  begin
    if (AValue='') and (AProp='') then
      exit;

    if AValue<>'' then
      ValueHasTrailingLineEnding:=AValue[Length(AValue)] in [#13,#10]
    else
      ValueHasTrailingLineEnding:=false;

    FHelperList.Text:=AValue;
    if FHelperList.Count=1 then begin
      if AProp='' then
        OutLst.Add(FHelperList[0])
      else begin
        if AProp='#' then
          //comments are not quoted
          OutLst.Add(AProp+FHelperList[0])
        else
          if ValueHasTrailingLineEnding then
            OutLst.Add(AProp+' "'+FHelperList[0]+'\n"')
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
          if (i<FHelperList.Count-1) or ValueHasTrailingLineEnding then
            s := '"' + s + '\n"'
          else
            s := '"' + s + '"';
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

procedure TPOFile.UpdateItem(const Identifier, Original: string; const Flags: string; const ProcessingTranslation: boolean);
var
  Item: TPOFileItem;
  ItemHasFuzzyFlag: boolean;
  ItemOldFlags: string;
begin
  // try to find PO entry by identifier
  Item:=TPOFileItem(FIdentifierLowToItem[lowercase(Identifier)]);
  if Item<>nil then begin
    // found, update item value
    if CompareMultilinedStrings(Item.Original, Original)<>0 then begin
      FModified := True;
      if Item.Translation <> '' then begin
        if (Item.PreviousID = '') or (pos(sFuzzyFlag, Item.Flags) = 0) then
          Item.PreviousID:=Item.Original;
        Item.ModifyFlag(sFuzzyFlag, true);
      end;
      Item.Original:=Original;
    end;
    if ProcessingTranslation then
    begin
      // synchronize translation flags with base .po file, but keep fuzzy flag state
      ItemOldFlags := Item.Flags;
      ItemHasFuzzyFlag := pos(sFuzzyFlag, Item.Flags) <> 0;
      Item.Flags := lowercase(Flags);
      Item.ModifyFlag(sFuzzyFlag, ItemHasFuzzyFlag);
      if ItemOldFlags <> Item.Flags then
        FModified := True;
    end;
  end
  else // in this case new item will be added
    FModified := true;
  FillItem(Item, Identifier, Original, '', '', '', Flags, '');
end;

procedure TPOFile.FillItem(var CurrentItem: TPOFileItem; Identifier, Original,
  Translation, Comments, Context, Flags, PreviousID: string; LineNr: Integer = -1);

  function VerifyItemFormatting(var Item: TPOFileItem): boolean;
  var
    HasBadFormatFlag: boolean;
    i: integer;
  begin
    // this function verifies item formatting and sets its flags if the formatting is bad
    Result := true;
    if Item.Translation <> '' then
    begin
      Result := (pos(sNoFormatFlag, Item.Flags) <> 0) or CompareFormatArgs(Item.Original,Item.Translation);
      if not Result then
      begin
        if pos(sFuzzyFlag, Item.Flags) = 0 then
        begin
          if FAllowChangeFuzzyFlag = true then
          begin
            Item.ModifyFlag(sFuzzyFlag, true);
            FModified := true;
          end;
        end;
      end;
      HasBadFormatFlag := pos(sBadFormatFlag, Item.Flags) <> 0;
      if HasBadFormatFlag <> not Result then
      begin
        Item.ModifyFlag(sBadFormatFlag, not Result);
        FModified := true;
      end;
    end
    else
    begin
      if pos(sFuzzyFlag, Item.Flags)<>0 then
      begin
        Item.ModifyFlag(sFuzzyFlag, false);
        FModified := true;
      end;
      if pos(sBadFormatFlag, Item.Flags) <> 0 then
      begin
        Item.ModifyFlag(sBadFormatFlag, false);
        FModified := true;
      end;
    end;

    if Item.Original <> '' then
    begin
      i:=0;
      if (ExtractFormatArgs(Item.Original, i) <> '') and (i = 0) then
      begin
        if Item.ModifyFlag(sFormatFlag, pos(sNoFormatFlag, Item.Flags) = 0) then
          FModified := true;
      end
      else
        if Item.ModifyFlag(sFormatFlag, false) then
          FModified := true;
    end;
  end;

var
  FoundItem: TPOFileItem;
begin
  FoundItem := TPOFileItem(FOriginalToItem.Data[Original]);

  if CurrentItem = nil then
  begin
    if (not FAllEntries) and (((FoundItem=nil) or (FoundItem.Translation='')) and (Translation='')) then
      exit;
    CurrentItem:=TPOFileItem.Create(lowercase(Identifier), Original, Translation);
    CurrentItem.Comments := Comments;
    CurrentItem.Context := Context;
    CurrentItem.Flags := lowercase(Flags);
    CurrentItem.PreviousID := PreviousID;
    CurrentItem.LineNr := LineNr;
    FItems.Add(CurrentItem);
    //debugln(['TPOFile.FillItem Identifier=',Identifier,' Orig="',dbgstr(OriginalValue),'" Transl="',dbgstr(TranslatedValue),'"']);
    FIdentifierLowToItem[CurrentItem.IdentifierLow]:=CurrentItem;
  end;

  CurrentItem.Tag := FTag;

  if FoundItem <> nil then
  begin
    if FoundItem.IdentifierLow<>CurrentItem.IdentifierLow then
    begin
      // if old item doesn't have context, add one
      if FoundItem.Context='' then
        FoundItem.Context := FoundItem.IdentifierLow;
      // if current item doesn't have context, add one
      if CurrentItem.Context='' then
        CurrentItem.Context := CurrentItem.IdentifierLow;
      // marking items as duplicate (needed only by POChecker)
      FoundItem.Duplicate := true;
      CurrentItem.Duplicate := true;
      // if old item is already translated and current item not, use translation
      // note, that we do not copy fuzzy translations in order not to potentially mislead translators
      if (CurrentItem.Translation='') and (FoundItem.Translation<>'') and (pos(sFuzzyFlag, FoundItem.Flags) = 0) then
      begin
        CurrentItem.Translation := FoundItem.Translation;
        if CurrentItem.Flags='' then
          CurrentItem.Flags := FoundItem.Flags;
        CurrentItem.ModifyFlag(sFuzzyFlag, true);
        FModified := True;
      end;
    end;
  end;

  VerifyItemFormatting(CurrentItem);

  if Original <> '' then
  begin
    if (FoundItem = nil) or ((FoundItem.Translation = '') and (CurrentItem.Translation <> '')) or
     ((FoundItem.Translation <> '') and (CurrentItem.Translation <> '') and
      (pos(sFuzzyFlag, FoundItem.Flags) <> 0) and (pos(sFuzzyFlag, CurrentItem.Flags) = 0)) then
    begin
      if FoundItem <> nil then
        FOriginalToItem.Remove(Original);
      FOriginalToItem.Add(Original,CurrentItem);
    end;
  end;

end;

procedure TPOFile.UpdateTranslation(BasePOFile: TPOFile);
var
  Item: TPOFileItem;
  i: Integer;
begin
  UntagAll;
  for i:=0 to BasePOFile.Items.Count-1 do begin
    Item := TPOFileItem(BasePOFile.Items[i]);
    UpdateItem(Item.IdentifierLow, Item.Original, Item.Flags, true);
  end;
  RemoveTaggedItems(0); // get rid of any item not existing in BasePOFile
  InvalidateStatistics;
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
  end;
end;

procedure TPOFile.InvalidateStatistics;
begin
  FStatisticsUpdated := false;
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
  Duplicate:=false;
  IdentifierLow:=TheIdentifierLow;
  Original:=TheOriginal;
  Translation:=TheTranslated;
end;

function TPOFileItem.ModifyFlag(const AFlags: string; Check: boolean): boolean;
var
  F, MF: TStringList;

  procedure ProcessFlag(const AFlag: string);
  var
    i: Integer;
  begin
    i := F.IndexOf(AFlag);
    if (i<0) and Check then
    begin
      F.Add(AFlag);
      Result := true;
    end
    else
    begin
      if (i>=0) and (not Check) then
      begin
        F.Delete(i);
        Result := true;
      end;
    end;
  end;

var
  i: Integer;
begin
  Result := false;
  MF := nil;
  F := TStringList.Create;
  try
    F.CommaText := Flags;

    if Pos(',', AFlags) = 0 then
      ProcessFlag(AFlags)
    else
    begin
      MF := TStringList.Create;
      MF.CommaText := AFlags;
      for i := 0 to MF.Count - 1 do
        ProcessFlag(MF[i]);
    end;

    if not Result then
      exit;
    Flags := F.CommaText;
    Flags := StringReplace(Flags, ',', ', ', [rfReplaceAll]);
  finally
    F.Free;
    MF.Free;
  end;
end;

end.

