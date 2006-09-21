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
unit IDETranslations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GetText, LCLProc, Translations, FileUtil,
  LazarusIDEStrConsts;
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

function GetLazarusLanguageLocalizedName(const ID: string): String;
begin
  if ID='' then
    Result:=rsLanguageAutomatic
  else if CompareText(ID,'en')=0 then
    Result:=rsLanguageEnglish
  else if CompareText(ID,'de')=0 then
    Result:=rsLanguageGerman
  else if CompareText(ID,'ca')=0 then
    Result:=rsLanguageCatalan
  else if CompareText(ID,'fr')=0 then
    Result:=rsLanguageFrench
  else if CompareText(ID,'it')=0 then
    Result:=rsLanguageItalian
  else if CompareText(ID,'pl')=0 then
    Result:=rsLanguagePolish
  else if CompareText(ID,'pliso')=0 then
    Result:=rsLanguagePolishISO
  else if CompareText(ID,'plwin')=0 then
    Result:=rsLanguagePolishWin
  else if CompareText(ID,'ru')=0 then
    Result:=rsLanguageRussian
  else if CompareText(ID,'es')=0 then
    Result:=rsLanguageSpanish
  else if CompareText(ID,'fi')=0 then
    Result:=rsLanguageFinnish
  else if CompareText(ID,'he')=0 then
    Result:=rsLanguageHebrew
  else if CompareText(ID,'ar')=0 then
    Result:=rsLanguageArabic
  else if CompareText(ID,'pb')=0 then
    Result:=rsLanguagePortugues
  else if CompareText(ID,'ua')=0 then
    Result:=rsLanguageUkrainian
  else if CompareText(ID,'nl')=0 then
    Result:=rsLanguageDutch
  else if CompareText(ID,'ja')=0 then
    Result:=rsLanguageJapanese
  else if CompareText(ID,'zh-cn')=0 then
    Result:=rsLanguageChinese
  else if CompareText(ID,'id')=0 then
    Result:=rsLanguageIndonesian
  else if CompareText(ID,'af_ZA')=0 then
    Result:=rsLanguageAfrikaans
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
  Ext = '.%s.po';
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

initialization
  LazarusTranslations:=nil;
  
finalization
  LazarusTranslations.Free;
  LazarusTranslations:=nil;

end.

