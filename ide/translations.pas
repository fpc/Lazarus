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
  Classes, SysUtils, GetText, FileUtil, LazarusIDEStrConsts;
  
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

// translate all resource strings
procedure TranslateResourceStrings(const BaseDirectory, CustomLang: string);

// get language name for ID
function GetLazarusLanguageLocalizedName(const ID: string): String;

// collect all available translations
procedure CollectTranslations(const LazarusDir: string);

var
  LazarusTranslations: TLazarusTranslations;


implementation

uses
  Dos, LazConf;

function GetLazarusLanguageLocalizedName(const ID: string): String;
begin
  if ID='' then
    Result:=rsLanguageAutomatic
  else if AnsiCompareText(ID,'en')=0 then
    Result:=rsLanguageEnglish
  else if AnsiCompareText(ID,'de')=0 then
    Result:=rsLanguageDeutsch
  else if AnsiCompareText(ID,'ca')=0 then
    Result:=rsLanguageCatalan
  else if AnsiCompareText(ID,'fr')=0 then
    Result:=rsLanguageFrench
  else if AnsiCompareText(ID,'it')=0 then
    Result:=rsLanguageItalian
  else if AnsiCompareText(ID,'itiso')=0 then
    Result:=rsLanguageItalianISO
  else if AnsiCompareText(ID,'pl')=0 then
    Result:=rsLanguagePolish
  else if AnsiCompareText(ID,'pliso')=0 then
    Result:=rsLanguagePolishISO
  else if AnsiCompareText(ID,'plwin')=0 then
    Result:=rsLanguagePolishWin
  else if AnsiCompareText(ID,'ru')=0 then
    Result:=rsLanguageRussian
  else if AnsiCompareText(ID,'ruwin')=0 then
    Result:=rsLanguageRussianWin
  else if AnsiCompareText(ID,'es')=0 then
    Result:=rsLanguageSpanish
  else if AnsiCompareText(ID,'fi')=0 then
    Result:=rsLanguageFinnish
  else if AnsiCompareText(ID,'he')=0 then
    Result:=rsLanguageHebrew
  else if AnsiCompareText(ID,'ar')=0 then
    Result:=rsLanguageArabic
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

procedure TranslateUnitResourceStrings(const ResUnitName, AFilename: string);
var
  mo: TMOFile;
  TableID, StringID, TableCount: Integer;
  s: String;
begin
  if (ResUnitName='') or (AFilename='') or (not FileExists(AFilename)) then
    exit;
  try
    mo := TMOFile.Create(AFilename);
    try
      for TableID:=0 to ResourceStringTableCount - 1 do begin
        TableCount := ResourceStringCount(TableID);

        // check if this table belongs to the ResUnitName
        if TableCount=0 then continue;
        s:=GetResourceStringName(TableID,0);
        if AnsiCompareText(ResUnitName+'.',LeftStr(s,length(ResUnitName)+1))<>0
        then continue;

        // translate all resource strings of the unit
        for StringID := 0 to TableCount - 1 do begin
          s := mo.Translate(GetResourceStringDefaultValue(TableID,StringID),
            GetResourceStringHash(TableID,StringID));
          if Length(s) > 0 then begin
            SetResourceStringValue(TableID,StringID,s);
          end;
        end;
      end;
    finally
      mo.Free;
    end;
  except
    on e: Exception do;
  end;
end;

procedure TranslateUnitResourceStrings(const ResUnitName, BaseFilename,
  Lang, FallbackLang: string);
begin
  if (ResUnitName='') or (BaseFilename='')
  or ((Lang='') and (FallbackLang='')) then exit;

  if FallbackLang<>'' then
    TranslateUnitResourceStrings(ResUnitName,Format(BaseFilename,[FallbackLang]));
  if Lang<>'' then
    TranslateUnitResourceStrings(ResUnitName,Format(BaseFilename,[Lang]));
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
var
  Lang, FallbackLang: String;
  Dir: String;
begin
  if LazarusTranslations=nil then CollectTranslations(BaseDirectory);
  if CustomLang='' then begin
    GetLanguageIDs(Lang,FallbackLang);
  end else begin
    Lang:=CustomLang;
    FallbackLang:='';
  end;
  Dir:=AppendPathDelim(BaseDirectory);
  // LCL
  TranslateUnitResourceStrings('LclStrConsts',
    Dir+'lcl/languages/lcl.%s.mo',Lang,FallbackLang);
  // IDE without objectinspector
  TranslateUnitResourceStrings('LazarusIDEStrConsts',
    Dir+'languages/lazaruside.%s.mo',Lang,FallbackLang);
  // objectinspector
  TranslateUnitResourceStrings('ObjInspStrConsts',
    Dir+'languages/objinspstrconsts.%s.mo',Lang,FallbackLang);
  // CodeTools
  TranslateUnitResourceStrings('CodeToolsStrConsts',
    Dir+'components/codetools/languages/codetools.%s.mo',Lang,FallbackLang);
  // SynEdit
  TranslateUnitResourceStrings('SynEditStrConst',
    Dir+'components/synedit/languages/synedit.%s.mo',Lang,FallbackLang);
  // SynMacroRecorder
  TranslateUnitResourceStrings('SynMacroRecorder',
    Dir+'components/synedit/languages/synmacrorecorder.%s.mo',Lang,FallbackLang);
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
  while (Result>=0) and (AnsiCompareText(ID,FItems[Result].ID)<>0) do
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

initialization
  LazarusTranslations:=nil;
  
finalization
  LazarusTranslations.Free;
  LazarusTranslations:=nil;

end.

