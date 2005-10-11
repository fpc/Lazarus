{ $Id$}
{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Methods and classes for loading the IDE translations/localizations.
}
unit Translations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, GetText, FileUtil, LazarusIDEStrConsts,
  StringHashList, LazConf
  {$IFDEF linux}{$IFNDEF NoUTF8Translations}
  ,LazCWString
  {$ENDIF}{$ENDIF};

  { IDE Language (Human, not computer) }

type
  { TLazarusTranslation }

  TLazarusTranslation = class
  private
    FID: string;
  public
    property ID: string read FID;
  end;
  PLazarusTranslation = ^TLazarusTranslation;
  
  
  { TLazarusTranslations }
  
  TLazarusTranslations = class
  private
    FCount: integer;
    FItems: PLazarusTranslation;
    function GetItems(Index: integer): TLazarusTranslation;
  public
    destructor Destroy; override;
    procedure Add(const ID: string);
    function IndexOf(const ID: string): integer;
    procedure Clear;
  public
    property Count: integer read FCount;
    property Items[Index: integer]: TLazarusTranslation read GetItems; default;
  end;
  
  { TPOFileItem }

  TPOFileItem = class
  public
    Identifier: string;
    Original: string;
    Translation: string;
    constructor Create(const TheIdentifier, TheOriginal, TheTranslated: string);
  end;
  
  { TPOFile }

  TPOFile = class
  protected
    FItems: TFPList;// list of TPOFileItem
    FIdentifierToItem: TStringHashList;
    FOriginalToItem: TStringHashList;
  public
    constructor Create(const AFilename: String);
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    procedure ReadPOText(const s: string);
    procedure Add(const Identifier, OriginalValue, TranslatedValue: string);
    function Translate(const Identifier, OriginalValue: String): String;
  end;

  EPOFileError = class(Exception);

// translate all resource strings
procedure TranslateResourceStrings(const BaseDirectory, CustomLang: string);

// get language name for ID
function GetLazarusLanguageLocalizedName(const ID: string): String;

// collect all available translations
procedure CollectTranslations(const LazarusDir: string);

var
  LazarusTranslations: TLazarusTranslations;


implementation

function GetLazarusLanguageLocalizedName(const ID: string): String;
begin
  if ID='' then
    Result:=rsLanguageAutomatic
  else if CompareText(ID,'en')=0 then
    Result:=rsLanguageEnglish
  else if CompareText(ID,'de')=0 then
    Result:=rsLanguageDeutsch
  else if CompareText(ID,'ca')=0 then
    Result:=rsLanguageCatalan
  else if CompareText(ID,'fr')=0 then
    Result:=rsLanguageFrench
  else if CompareText(ID,'it')=0 then
    Result:=rsLanguageItalian
  else if CompareText(ID,'itiso')=0 then
    Result:=rsLanguageItalianISO
  else if CompareText(ID,'pl')=0 then
    Result:=rsLanguagePolish
  else if CompareText(ID,'pliso')=0 then
    Result:=rsLanguagePolishISO
  else if CompareText(ID,'plwin')=0 then
    Result:=rsLanguagePolishWin
  else if CompareText(ID,'ru')=0 then
    Result:=rsLanguageRussian
  else if CompareText(ID,'ruwin')=0 then
    Result:=rsLanguageRussianWin
  else if CompareText(ID,'ruutf')=0 then
    Result:=rsLanguageRussianUTF
  else if CompareText(ID,'es')=0 then
    Result:=rsLanguageSpanish
  else if CompareText(ID,'fi')=0 then
    Result:=rsLanguageFinnish
  else if CompareText(ID,'fiwin')=0 then
    Result:=rsLanguageFinnishWin
  else if CompareText(ID,'he')=0 then
    Result:=rsLanguageHebrew
  else if CompareText(ID,'ar')=0 then
    Result:=rsLanguageArabic
  else if CompareText(ID,'pb')=0 then
    Result:=rsLanguagePortugues
  else if CompareText(ID,'ua')=0 then
    Result:=rsLanguageUkranian
  else
    Result:=ID;
end;

procedure CollectTranslations(const LazarusDir: string);
var
  FileInfo: TSearchRec;
  ID: String;
  SearchMask: String;
begin
  // search for all languages/lazarusidestrconsts.xxx.po files
  if LazarusTranslations=nil then
    LazarusTranslations:=TLazarusTranslations.Create
  else
    LazarusTranslations.Clear;
  // add automatic and english translation
  LazarusTranslations.Add('');
  LazarusTranslations.Add('en');
  // search existing translations
  SearchMask:=AppendPathDelim(LazarusDir)+'languages'+PathDelim+'lazaruside.*.po';
  //writeln('CollectTranslations ',SearchMask);
  if SysUtils.FindFirst(SearchMask,faAnyFile,FileInfo)=0
  then begin
    repeat
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
      then continue;
      ID:=copy(FileInfo.Name,length('lazaruside.')+1,
               length(FileInfo.Name)-length('lazaruside..po'));
      //writeln('CollectTranslations A ',FileInfo.Name,' ID=',ID);
      if (ID<>'') and (Pos('.',ID)<1) and (LazarusTranslations.IndexOf(ID)<0)
      then begin
        //writeln('CollectTranslations ID=',ID);
        LazarusTranslations.Add(ID);
      end;
    until SysUtils.FindNext(FileInfo)<>0;
  end;
  SysUtils.FindClose(FileInfo);
end;

function UTF8ToSystemCharSet(const s: string): string;
begin
  Result:=s;
  {$IFNDEF NoUTF8Translations}
  Result:=Utf8ToAnsi(Result);
  {$ENDIF}
end;

function DoTranslateUnitResourceStrings(const ResUnitName, AFilename: string
  ): boolean;
var
  mo: TMOFile;
  TableID, StringID, TableCount: Integer;
  s: String;
  DefValue: String;
  po: TPOFile;
begin
  Result:=false;
  //debugln('DoTranslateUnitResourceStrings) ResUnitName="',ResUnitName,'" AFilename="',AFilename,'"');
  if (ResUnitName='') or (AFilename='') or (not FileExists(AFilename)) then
    exit;
  try
    mo := nil;
    po := nil;
      
    if CompareFileExt(AFilename,'.mo')=0 then begin
      // read .mo file
      mo := TMOFile.Create(AFilename);
    end else begin
      // read .po file
      po := TPOFile.Create(AFilename)
    end;
    try
      for TableID:=0 to ResourceStringTableCount - 1 do begin
        TableCount := ResourceStringCount(TableID);

        // check if this table belongs to the ResUnitName
        if TableCount=0 then continue;
        s:=GetResourceStringName(TableID,0);
        if CompareText(ResUnitName+'.',LeftStr(s,length(ResUnitName)+1))<>0
        then continue;

        // translate all resource strings of the unit
        for StringID := 0 to TableCount - 1 do begin
          DefValue:=GetResourceStringDefaultValue(TableID,StringID);
          // get UTF8 string
          if mo<>nil then
            s := mo.Translate(DefValue,GetResourceStringHash(TableID,StringID))
          else
            s := po.Translate(GetResourceStringName(TableID,StringID),DefValue);

          if Length(s) > 0 then begin
            // convert UTF8 to current local
            s:=UTF8ToSystemCharSet(s);
            SetResourceStringValue(TableID,StringID,s);
          end;
        end;
      end;
    finally
      mo.Free;
      po.Free;
    end;
    Result:=true;
  except
    on e: Exception do;
  end;
end;

procedure TranslateUnitResourceStrings(const ResUnitName, BaseFilename,
  Lang, FallbackLang: string);
begin
  if (ResUnitName='') or (BaseFilename='') then exit;

  //debugln('TranslateUnitResourceStrings BaseFilename="',BaseFilename,'"');
  if (FallbackLang<>'') then
    DoTranslateUnitResourceStrings(ResUnitName,Format(BaseFilename,[FallbackLang]));
  if (Lang<>'') then
    DoTranslateUnitResourceStrings(ResUnitName,Format(BaseFilename,[Lang]));
end;

{-------------------------------------------------------------------------------
  TranslateResourceStrings

  Params: none
  Result: none

  Translates all resourcestrings of the resource string files:
    - lclstrconsts.pas
    - codetoolsstrconsts.pas
    - lazarusidestrconsts.pas
-------------------------------------------------------------------------------}
procedure TranslateResourceStrings(const BaseDirectory, CustomLang: string);
const
  {$IFDEF UseMOTranslations}
  Ext = '.%s.mo';
  {$ELSE}
  Ext = '.%s.po';
  {$ENDIF}
var
  Lang, FallbackLang: String;
  Dir: String;
begin
  //debugln('TranslateResourceStrings A CustomLang=',CustomLang);
  if LazarusTranslations=nil then CollectTranslations(BaseDirectory);
  if CustomLang='' then begin
    GetLanguageIDs(Lang,FallbackLang);
  end else begin
    Lang:=CustomLang;
    FallbackLang:='';
  end;
  //debugln('TranslateResourceStrings A Lang=',Lang,' FallbackLang=',FallbackLang);
  Dir:=AppendPathDelim(BaseDirectory);
  // LCL
  TranslateUnitResourceStrings('LclStrConsts',
    Dir+'lcl/languages/lcl'+Ext,Lang,FallbackLang);
  // IDE without objectinspector
  TranslateUnitResourceStrings('LazarusIDEStrConsts',
    Dir+'languages/lazaruside'+Ext,Lang,FallbackLang);
  // objectinspector
  TranslateUnitResourceStrings('ObjInspStrConsts',
    Dir+'languages/objinspstrconsts'+Ext,Lang,FallbackLang);
  // CodeTools
  TranslateUnitResourceStrings('CodeToolsStrConsts',
    Dir+'components/codetools/languages/codetools'+Ext,Lang,FallbackLang);
  // SynEdit
  TranslateUnitResourceStrings('SynEditStrConst',
    Dir+'components/synedit/languages/synedit'+Ext,Lang,FallbackLang);
  // SynMacroRecorder
  TranslateUnitResourceStrings('SynMacroRecorder',
    Dir+'components/synedit/languages/synmacrorecorder'+Ext,Lang,FallbackLang);
end;


{ TLazarusTranslations }

function TLazarusTranslations.GetItems(Index: integer): TLazarusTranslation;
begin
  Result:=FItems[Index];
end;

destructor TLazarusTranslations.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLazarusTranslations.Add(const ID: string);
var
  NewTranslation: TLazarusTranslation;
begin
  if IndexOf(ID)>=0 then
    raise Exception.Create('TLazarusTranslations.Add '
                          +'ID="'+ID+'" already exists.');
  NewTranslation:=TLazarusTranslation.Create;
  NewTranslation.FID:=ID;
  inc(FCount);
  ReallocMem(FItems,SizeOf(Pointer)*FCount);
  FItems[FCount-1]:=NewTranslation;
end;

function TLazarusTranslations.IndexOf(const ID: string): integer;
begin
  Result:=FCount-1;
  while (Result>=0) and (CompareText(ID,FItems[Result].ID)<>0) do
    dec(Result);
end;

procedure TLazarusTranslations.Clear;
var
  i: Integer;
begin
  for i:=0 to FCount-1 do FItems[i].Free;
  FCount:=0;
  ReallocMem(FItems,0);
end;

{ TPOFile }

constructor TPOFile.Create(const AFilename: String);
var
  f: TStream;
begin
  f := TFileStream.Create(AFilename, fmOpenRead);
  try
    Self.Create(f);
  finally
    f.Free;
  end;
end;

constructor TPOFile.Create(AStream: TStream);
var
  Size: Integer;
  s: string;
begin
  inherited Create;
  
  FItems:=TFPList.Create;
  FIdentifierToItem:=TStringHashList.Create(false);
  FOriginalToItem:=TStringHashList.Create(true);

  Size:=AStream.Size-AStream.Position;
  if Size<=0 then exit;
  SetLength(s,Size);
  AStream.Read(s[1],Size);
  ReadPOText(s);
end;

destructor TPOFile.Destroy;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do
    TObject(FItems[i]).Free;
  FItems.Free;
  FIdentifierToItem.Free;
  FOriginalToItem.Free;
  inherited Destroy;
end;

procedure TPOFile.ReadPOText(const s: string);
{ Read a .po file. Structure:

Example
#: lazarusidestrconsts:lisdonotshowsplashscreen
msgid "                      Do not show splash screen"
msgstr ""

}
const
  sCommentIdentifier: PChar = '#: ';
  sMsgID: PChar = 'msgid "';
  sMsgStr: PChar = 'msgstr "';
var
  l: Integer;
  LineLen: Integer;
  p: PChar;
  LineStart: PChar;
  LineEnd: PChar;
  Identifier: String;
  MsgID: String;
  MsgStr: String;
  TextEnd: PChar;
begin
  if s='' then exit;
  l:=length(s);
  p:=PChar(s);
  LineStart:=p;
  TextEnd:=p+l;
  while LineStart<TextEnd do begin
    LineEnd:=LineStart;
    while (not (LineEnd^ in [#0,#10,#13])) do inc(LineEnd);
    LineLen:=LineEnd-LineStart;
    if LineLen>0 then begin
      if CompareMem(LineStart,sCommentIdentifier,3) then begin
        Identifier:=copy(s,LineStart-p+4,LineLen-3);
      end else if CompareMem(LineStart,sMsgID,7) then begin
        MsgID:=copy(s,LineStart-p+8,LineLen-8);
      end else if CompareMem(LineStart,sMsgStr,8) then begin
        //MsgStr:=copy(s,LineStart-p+9,LineLen-9);
        MsgStr:=UTF8CStringToUTF8String(LineStart+8,LineLen-9);
        Add(Identifier,MsgID,MsgStr);
      end;
    end;
    LineStart:=LineEnd+1;
    while (LineStart<TextEnd) and (LineStart^ in [#10,#13]) do inc(LineStart);
  end;
end;

procedure TPOFile.Add(const Identifier, OriginalValue, TranslatedValue: string
  );
var
  Item: TPOFileItem;
begin
  if (TranslatedValue='') then exit;
  //debugln('TPOFile.Add Identifier="',Identifier,'" OriginalValue="',OriginalValue,'" TranslatedValue="',TranslatedValue,'"');
  Item:=TPOFileItem.Create(Identifier,OriginalValue,TranslatedValue);
  FItems.Add(Item);
  FIdentifierToItem.Add(Identifier,Item);
  FOriginalToItem.Add(OriginalValue,Item);
end;

function TPOFile.Translate(const Identifier, OriginalValue: String): String;
var
  Item: TPOFileItem;
begin
  Item:=TPOFileItem(FIdentifierToItem.Data[Identifier]);
  if Item=nil then
    Item:=TPOFileItem(FOriginalToItem.Data[OriginalValue]);
  if Item<>nil then begin
    Result:=Item.Translation;
    if Result='' then RaiseGDBException('TPOFile.Translate Inconsistency');
  end else
    Result:=OriginalValue;
end;

{ TPOFileItem }

constructor TPOFileItem.Create(const TheIdentifier, TheOriginal,
  TheTranslated: string);
begin
  Identifier:=TheIdentifier;
  Original:=TheOriginal;
  Translation:=TheTranslated;
end;

initialization
  LazarusTranslations:=nil;
  
finalization
  LazarusTranslations.Free;
  LazarusTranslations:=nil;

end.

