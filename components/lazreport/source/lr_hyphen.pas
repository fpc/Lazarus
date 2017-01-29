{ A hyphenation bindings and classes unit using libhyphen hyphenator

  Copyright (C) 2013 Theo

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.

  Modified for it's inclusion into LazReport by: Jesus Reyes A.
}
{$MODE objfpc}{$H+}
unit lr_hyphen;

interface

uses
  dynlibs, Classes, SysUtils;

const
  MAX_NAME = 20;
  MAX_CHARS = 100;

type

  EHyphenationException = class(Exception);

  PHyphenTrans = ^HyphenTrans;
  HyphenTrans = record
    ch: char;
    new_state: integer;
  end;

  PHyphenState = ^HyphenState;
  HyphenState = record
    match: PChar;
    repl: PChar;
    replindex: char;
    replcut: char;
    fallback_state: integer;
    num_trans: integer;
    trans: PHyphenTrans;
  end;

  PHyphenDict = ^HyphenDict;
  HyphenDict = record
    lhmin: char;
    rhmin: char;
    clhmin: char;
    crhmin: char;
    num_states: integer;
    cset: array[0..MAX_NAME - 1] of char;
    utf8: integer;
    states: PHyphenState;
    nextlevel: PHyphenDict;
  end;

  Thnj_hyphen_load = function(const fn: PChar): PHyphenDict cdecl;
  Thnj_hyphen_free = procedure(var dict: HyphenDict) cdecl;
  Thnj_hyphen_hyphenate = function(var dict: HyphenDict; const word: PChar;
    word_size: integer; hyphens: PChar): integer cdecl;
  Thnj_hyphen_hyphenate2 = function(var dict: HyphenDict; const word: PChar;
    word_size: integer; hyphens: PChar; hyphenated_word: PChar;
    var rep: PPChar; var pos: Pointer; var cut: Pointer): integer cdecl;

  { THyphen }

  THyphen = class
  private
    FDictionary: string;
    Pdict: PHyphenDict;
    procedure FreeDict;
    function CheckLibrary: boolean;
    function CheckDictionary: boolean;
    function GetLoaded: boolean;
    procedure LoadDict(DictionaryPath: string);
    procedure SetDictionary(AValue: string);
  public
    constructor create;
    destructor Destroy; override;

    function BreakWord(Word:string):string;

    property Dictionary: string read FDictionary write SetDictionary;
    property Loaded: boolean read GetLoaded;
  end;


var
  hnj_hyphen_load: Thnj_hyphen_load;
  hnj_hyphen_free: Thnj_hyphen_free;
  hnj_hyphen_hyphenate: Thnj_hyphen_hyphenate;
  hnj_hyphen_hyphenate2: Thnj_hyphen_hyphenate2;
  HypLibLoaded: boolean = False;
  HypLibHandle: THandle;

function LoadLibHyphen(LibraryName: string): boolean;

implementation


function LoadLibHyphen(libraryName: string): boolean;
begin
  if libraryName = '' then
{$IFDEF windows}
 libraryName := 'hyphen.dll';
{$ENDIF}
  {$IFDEF unix}
  libraryName := 'libhyphen.so';
{$ENDIF}

  Result := HypLibLoaded;
  if Result then
    exit;

  HypLibHandle := LoadLibrary(PAnsiChar(libraryName));
  if HypLibHandle <> 0 then
  begin
    Result := True;

    hnj_hyphen_load := Thnj_hyphen_load(GetProcAddress(HypLibHandle, 'hnj_hyphen_load'));
    if not Assigned(hnj_hyphen_load) then
      Result := False;
    hnj_hyphen_free := Thnj_hyphen_free(GetProcAddress(HypLibHandle, 'hnj_hyphen_free'));
    if not Assigned(hnj_hyphen_free) then
      Result := False;
    hnj_hyphen_hyphenate := Thnj_hyphen_hyphenate(
      GetProcAddress(HypLibHandle, 'hnj_hyphen_hyphenate'));
    if not Assigned(hnj_hyphen_hyphenate) then
      Result := False;
    hnj_hyphen_hyphenate2 := Thnj_hyphen_hyphenate2(
      GetProcAddress(HypLibHandle, 'hnj_hyphen_hyphenate2'));
    if not Assigned(hnj_hyphen_hyphenate2) then
      Result := False;

    HypLibLoaded := Result;
  end;
end;

{ THyphen }

procedure THyphen.FreeDict;
begin
  if pdict <> nil then
    hnj_hyphen_free(pdict^);
  pdict := nil;
end;

function THyphen.CheckLibrary: boolean;
begin
  result := HypLibLoaded;
  if not result then begin
    // try to load the library
    LoadLibHyphen('');
    result := HypLibLoaded;
  end;
end;

function THyphen.CheckDictionary: boolean;
begin
  result := false;
  if (FDictionary='') or not FileExists(FDictionary) then
    exit;
  if PDict=nil then
    LoadDict(FDictionary);
  result := PDict<>nil;
end;

function THyphen.GetLoaded: boolean;
begin
  result := HypLibLoaded and (PDict<>nil);
end;

procedure THyphen.SetDictionary(AValue: string);
begin
  if FDictionary=AValue then
    Exit;
  FDictionary:=AValue;
  if PDict<>nil then
    FreeDict;
end;

constructor THyphen.create;
begin
  inherited create;
end;

destructor THyphen.Destroy;
begin
  FreeDict;
  inherited Destroy;
end;

function THyphen.BreakWord(Word: string): string;
var
  i: cardinal;
  hyphens: PChar;
  rep: PPChar;
  pos: Pointer;
  cut: Pointer;
  len: integer;
begin
  result := '';

  if not CheckLibrary then
    raise EHyphenationException.Create('hyphen libarary not loaded');

  if not CheckDictionary then
    raise EHyphenationException.Create('hyphen dictionary not loaded');

  len := Length(word);
  hyphens := StrAlloc(Len + 5);
  rep := nil;
  pos := nil;
  cut := nil;
  try
    if hnj_hyphen_hyphenate2(pdict^, PChar(word), Len, hyphens, nil, rep, pos, cut) = 0 then
    begin
      for i := 1 to length(hyphens)-1  do
        if Odd(Ord(hyphens[i])) then begin
          result := result + chr(i+1);
        end;
    end;
  finally
    StrDispose(hyphens);
  end;

end;

procedure THyphen.LoadDict(DictionaryPath: string);
begin
  FreeDict;
  pdict := hnj_hyphen_load(PChar(DictionaryPath))
end;

finalization

  if (HypLibHandle <> 0) and HypLibLoaded then
  begin
    if HypLibHandle <> 0 then
      FreeLibrary(HypLibHandle);
    HypLibLoaded := False;
  end;


end.

