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
    Methods and classes for loading translations/localizations from po files.
}
unit Translations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, GetText, FileUtil, StringHashList
  {$IFDEF linux}{$IFNDEF NoUTF8Translations}
  ,LazCWString
  {$ENDIF}{$ENDIF};

type
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

// translate resource strings for one unit
procedure TranslateUnitResourceStrings(const ResUnitName, BaseFilename,
  Lang, FallbackLang: string);

// GetLanguageIDs is part of the fcl in 2.0.1 and later
{$ifdef ver2_0_0}
procedure GetLanguageIDs(var Lang, FallbackLang: string);
{$endif}

implementation

// GetLanguageIDs is part of the fcl in 2.0.1 and later
{$ifdef ver2_0_0}
{$ifdef win32}
uses
  windows;
procedure GetLanguageIDs(var Lang, FallbackLang: string);
var
  Buffer: array[1..4] of char;
  Country: string;
  UserLCID: LCID;
begin
  //defaults
  Lang := '';
  FallbackLang:='';
  UserLCID := GetUserDefaultLCID;
  if GetLocaleInfo(UserLCID, LOCALE_SABBREVLANGNAME, @Buffer, 4)<>0 then
    FallbackLang := lowercase(copy(Buffer,1,2));
  if GetLocaleInfo(UserLCID, LOCALE_SABBREVCTRYNAME, @Buffer, 4)<>0 then begin
    Country := copy(Buffer,1,2);

    // some 2 letter codes are not the first two letters of the 3 letter code
    // there are probably more, but first let us see if there are translations
    if (Buffer='PRT') then Country:='PT';

    Lang := FallbackLang+'_'+Country;
  end;
end;

{$else}

procedure GetLanguageIDs(var Lang, FallbackLang: string);
begin
  lang := GetEnvironmentVariable('LC_ALL');
  if Length(lang) = 0 then
  begin
    lang := GetEnvironmentVariable('LC_MESSAGES');
    if Length(lang) = 0 then
    begin
      lang := GetEnvironmentVariable('LANG');
      if Length(lang) = 0 then
        exit;   // no language defined via environment variables
    end;
  end;
  FallbackLang := Copy(lang, 1, 2);
end;
{$endif}
{$endif}

function UTF8ToSystemCharSet(const s: string): string; inline;
begin
  {$IFDEF NoUTF8Translations}
  Result:=s;
  {$ELSE}
  Result:=Utf8ToAnsi(s);
  {$ENDIF}
end;

function DoTranslateUnitResourceStrings(const ResUnitName, AFilename: string
  ): boolean;
var
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
    po := nil;
    // read .po file
    po := TPOFile.Create(AFilename);
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
          s := po.Translate(GetResourceStringName(TableID,StringID),DefValue);

          if Length(s) > 0 then begin
            // convert UTF8 to current local
            s:=UTF8ToSystemCharSet(s);
            SetResourceStringValue(TableID,StringID,s);
          end;
        end;
      end;
    finally
      po.Free;
    end;
    Result:=true;
  except
    on e: Exception do begin
      DebugLn('Exception while translating ', ResUnitName);
      DebugLn(e.Message);
    end;
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

end.

