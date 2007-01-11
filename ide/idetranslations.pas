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
  Classes, SysUtils, GetText, LCLProc, Translations, FileUtil, avl_tree,
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

function ConvertRSTFiles(RSTDirectory, PODirectory: string): Boolean;
function ConvertRSTFile(const RSTFilename, OutputFilename: string): Boolean;

var
  LazarusTranslations: TLazarusTranslations = nil;
  SystemLanguageID1, SystemLanguageID2: string;

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
  else if CompareText(ID,'zh_CN')=0 then
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

type
  TConstItem = class
  public
    ModuleName, ConstName, Value: String;
  end;
  
function CompareConstItems(Data1, Data2: Pointer): integer;
begin
  Result:=CompareText(TConstItem(Data1).ConstName,TConstItem(Data2).ConstName);
end;

function ReadRSTFile(const InFilename: string;
  TreeOfConstItems: TAVLTree): Boolean;
var
  s: string;
  NextLineStartPos: integer;

  procedure ReadLine(var Line: string);
  var
    p: LongInt;
  begin
    p:=NextLineStartPos;
    while (p<=length(s)) and (not (s[p] in [#10,#13])) do inc(p);
    Line:=copy(s,NextLineStartPos,p-NextLineStartPos);
    inc(p);
    if (p<=length(s)) and (s[p] in [#10,#13]) and (s[p]<>s[p-1]) then inc(p);
    NextLineStartPos:=p;
  end;

var
  Line: String;
  item: TConstItem;
  DotPos, EqPos, i, j: Integer;
  ModuleName: String;
  ConstName: String;
  Value: String;
  fs: TFileStream;
begin
  Result:=false;
  try
    fs:=TFileStream.Create(InFilename,fmOpenRead);
    SetLength(s,fs.Size);
    if s='' then exit;
    fs.Read(s[1],length(s));
    fs.Free;
    NextLineStartPos:=1;

    while NextLineStartPos<=length(s) do begin
      ReadLine(Line);
      If (Length(Line)=0) or (Line[1]='#') then
        continue;

      DotPos := Pos('.', Line);
      EqPos := Pos('=', Line);
      if DotPos > EqPos then // paranoia checking.
        DotPos := 0;
      ModuleName := Copy(Line, 1, DotPos - 1);
      ConstName := Copy(Line, DotPos + 1, EqPos - DotPos - 1);

      Value := '';
      i := EqPos + 1;
      while i <= Length(Line) do begin
        if Line[i] = '''' then begin
          Inc(i);
          j := i;
          while (i <= Length(Line)) and (Line[i] <> '''') do
            Inc(i);
          Value := Value + Copy(Line, j, i - j);
          Inc(i);
        end else if Line[i] = '#' then begin
          Inc(i);
          j := i;
          while (i <= Length(Line)) and (Line[i] in ['0'..'9']) do
            Inc(i);
          Value := Value + Chr(StrToInt(Copy(Line, j, i - j)));
        end else if Line[i] = '+' then begin
          ReadLine(Line);
          i := 1;
        end else
          Inc(i);
      end;
      Item:=TConstItem.Create;
      Item.ModuleName:=ModuleName;
      Item.ConstName:=ConstName;
      Item.Value:=Value;
      TreeOfConstItems.Add(Item);
    end;

    Result:=true;
  except
  end;
end;

function ConvertToGettextPO(TreeOfConstItems: TAVLTree;
  const OutFilename: string): Boolean;
var
  j: Integer;
  item: TConstItem;
  s: String;
  c: Char;
  Node: TAVLTreeNode;
  ms: TMemoryStream;
  e: string;
  
  procedure WriteStr(const s: string);
  begin
    if s<>'' then ms.Write(s[1],length(s));
  end;
  
begin
  Result:=false;
  try
    e:=LineEnding;
    ms:=TMemoryStream.Create;

    Node:=TreeOfConstItems.FindLowest;
    while Node<>nil do begin
      item := TConstItem(Node.Data);

      // Convert string to C-style syntax
      s := '';
      for j := 1 to Length(item.Value) do begin
        c := item.Value[j];
        case c of
          #9:  s := s + '\t';
          #10: s := s + '\n';
          #0..#8,#11..#31,#128..#255:
            s := s + '\' +
              Chr(Ord(c) shr 6 + 48) +
              Chr((Ord(c) shr 3) and 7 + 48) +
              Chr(Ord(c) and 7 + 48);
          '\': s := s + '\\';
          '"': s := s + '\"';
        else s := s + c;
        end;
      end;

      // Write msg entry
      WriteStr('#: ');
      WriteStr(item.ModuleName);
      WriteStr(':');
      WriteStr(item.ConstName);
      WriteStr(e);
      WriteStr('msgid "');
      WriteStr(s);
      WriteStr('"');
      WriteStr(e);
      WriteStr('msgstr ""');
      WriteStr(e);
      WriteStr(e);

      Node:=TreeOfConstItems.FindSuccessor(Node);
    end;

    ms.Position:=0;
    ms.SaveToFile(OutFilename);
    Result:=true;
  except
  end;
end;

function ConvertRSTFile(const RSTFilename, OutputFilename: string): Boolean;
var
  TreeOfConstItems: TAVLTree;
begin
  Result:=false;
  //DebugLn(['ConvertRSTFile RSTFilename=',RSTFilename,' OutputFilename=',OutputFilename]);
  TreeOfConstItems:=TAVLTree.Create(@CompareConstItems);
  try
    // read .rst file
    if not ReadRSTFile(RSTFilename,TreeOfConstItems) then begin
      DebugLn(['ConvertRSTFile reading failed: RSTFilename=',RSTFilename]);
      exit;
    end;
    // write .po file
    if not ConvertToGettextPO(TreeOfConstItems,OutputFilename) then begin
      DebugLn(['ConvertRSTFile writing failed: OutputFilename=',OutputFilename]);
      exit;
    end;
  finally
    if TreeOfConstItems<>nil then begin
      TreeOfConstItems.FreeAndClear;
      TreeOfConstItems.Free;
    end;
  end;
  Result:=true;
end;

function ConvertRSTFiles(RSTDirectory, PODirectory: string): Boolean;
var
  FileInfo: TSearchRec;
  RSTFilename: String;
  OutputFilename: String;
begin
  Result:=true;
  if (RSTDirectory='') then exit;// nothing to do
  RSTDirectory:=AppendPathDelim(RSTDirectory);

  // find all .rst files in package output directory
  PODirectory:=AppendPathDelim(PODirectory);
  //DebugLn(['ConvertPackageRSTFiles PODirectory=',PODirectory]);
  if SysUtils.FindFirst(RSTDirectory+'*.rst',faAnyFile,FileInfo)=0
  then begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
        continue;
      RSTFilename:=RSTDirectory+FileInfo.Name;
      OutputFilename:=PODirectory+ChangeFileExt(FileInfo.Name,'.po');
      //DebugLn(['ConvertPackageRSTFiles RSTFilename=',RSTFilename,' OutputFilename=',OutputFilename]);
      if (not FileExists(OutputFilename))
      or (FileAge(RSTFilename)>FileAge(OutputFilename)) then begin
        if not ConvertRSTFile(RSTFilename,OutputFilename) then begin
          DebugLn(['ConvertPackageRSTFiles FAILED: RSTFilename=',RSTFilename,' OutputFilename=',OutputFilename]);
          exit(false);
        end;
      end;
    until SysUtils.FindNext(FileInfo)<>0;
  end;
  SysUtils.FindClose(FileInfo);
  Result:=true;
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
    Lang:=SystemLanguageID1;
    FallbackLang:=SystemLanguageID2;
  end else begin
    Lang:=CustomLang;
    FallbackLang:='';
  end;
  //debugln('TranslateResourceStrings A Lang=',Lang,' FallbackLang=',FallbackLang);
  Dir:=AppendPathDelim(BaseDirectory);
  // IDE
  TranslateUnitResourceStrings('LazarusIDEStrConsts',
    Dir+'languages/lazaruside'+Ext,Lang,FallbackLang);
  // LCL
  TranslateUnitResourceStrings('LclStrConsts',
    Dir+'lcl/languages/lclstrconsts'+Ext,Lang,FallbackLang);
  // IDEIntf
  TranslateUnitResourceStrings('ObjInspStrConsts',
    Dir+'ideintf/languages/objinspstrconsts'+Ext,Lang,FallbackLang);
  // CodeTools
  TranslateUnitResourceStrings('CodeToolsStrConsts',
    Dir+'components/codetools/languages/codetoolsstrconsts'+Ext,Lang,FallbackLang);
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
  GetLanguageIDs(SystemLanguageID1,SystemLanguageID2);

finalization
  LazarusTranslations.Free;
  LazarusTranslations:=nil;

end.

