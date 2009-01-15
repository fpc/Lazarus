unit JcfMiscFunctions;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is JcfMiscFunctions, released May 2003.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved.
Contributor(s):
Anthony Steele.
functions Str2Float and Float2Str from Ralf Steinhaeusser
procedures AdvanceTextPos and LastLineLength rewritten for speed by Adem Baba
SetObjectFontToSystemFont by Jean-Fabien Connault

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}


{ AFS 15 Jan 2k

  This project uses very little in the way of internal function libs
  as most is covered by JCL
  I was using ComponentFunctions from my JEDI VCL kit
  however that is causing linkage problems with the IDE plugin - it is a package
  and 2 packages can't package the same stuff,
  also it creates version dependencies - it bombed with the different version
  of ComponentFunctions that I have at work

  So I am importing just what I need from ComponentFunctions here
}

{$I JcfGlobal.inc}

interface

uses Classes;

function GetApplicationFolder: string;

function GetLastDir(psPath: string): string;

function Str2Float(s: string): double;
function Float2Str(const d: double): string;


{not really a file fn - string file name manipulation}
function SetFileNameExtension(const psFileName, psExt: string): string;

procedure AdvanceTextPos(const AText: string; var ARow, ACol: integer);
function LastLineLength(const AString: string): integer;

{ split into lines at CrLf or Lf}
function SplitIntoLines(s: string): TStrings;

procedure SplitIntoChangeSections(const s1, s2, SameStart, SameEnd: TStrings);


{$IFDEF DELPHI_5}
{ these functions are in Delphi 6 and up }
function IncludeTrailingPathDelimiter(const psPath: string): string;
function FileIsReadOnly(const psFile: string): boolean;
{$ENDIF}


implementation

uses
  { delphi }
  SysUtils, Forms,
  { local }
  JcfStringUtils;

function GetApplicationFolder: string;
begin
  Result := ExtractFilePath(Application.ExeName);
end;

{ these come from Ralf Steinhaeusser
  you see, in Germany, the default decimal sep char is a ',' not a '.'
  values with a '.' will not be read correctly  by StrToFloat
  and values written will contain a ','

  We want the config files to be portable so
  *always* use the '.' character when reading or writing
  This is not for localised display, but for consistent storage
}
//  like StrToFloat but expects a "." instead of the decimal-seperator-character
function Str2Float(s: string): double;
var
  code: integer;
begin
  // de-localise the string if need be
  if (DecimalSeparator <> '.') and (Pos(DecimalSeparator, s) > 0) then
  begin
    StrReplace(s, DecimalSeparator, '.');
  end;

  Val(s, Result, Code);
  if code <> 0 then
    raise EConvertError.Create('Str2Float: ' + s +
      ' is not a valid floating point string');
end;

// Like FloatToStr, but gives back a dot (.) as decimalseparator
function Float2Str(const d: double): string;
var
  OrgSep: char;
begin
  OrgSep := DecimalSeparator;
  DecimalSeparator := '.';
  Result := FloatToStr(d);
  DecimalSeparator := OrgSep;
end;


function GetLastDir(psPath: string): string;
var
  liPos: integer;
begin
  Result := '';
  if psPath = '' then
    exit;

  { is this a path ? }
  if not (DirectoryExists(psPath)) and FileExists(psPath) then
  begin
    // must be a file - remove the last bit
    liPos := StrLastPos(DirDelimiter, psPath);
    if liPos > 0 then
      psPath := StrLeft(psPath, liPos - 1);
  end;

  liPos := StrLastPos(DirDelimiter, psPath);
  if liPos > 0 then
    Result := StrRestOf(psPath, liPos + 1);
end;

function SetFileNameExtension(const psFileName, psExt: string): string;
var
  liMainFileNameLength: integer;
  lsOldExt: string;
begin
  if PathExtractFileNameNoExt(psFileName) = '' then
  begin
    Result := '';
    exit;
  end;

  lsOldExt := ExtractFileExt(psFileName);
  liMainFileNameLength := Length(psFileName) - Length(lsOldExt);
  Result   := StrLeft(psFileName, liMainFileNameLength);

  Result := Result + '.' + psExt;
end;

function PosLast(const ASubString, AString: string;
  const ALastPos: integer = 0): integer; {AdemBaba}
var
  {This is at least two or three times faster than Jedi's StrLastPos. I tested it}
  LastChar1: Char;
  Index1:    integer;
  Index2:    integer;
  Index3:    integer;
  Length1:   integer;
  Length2:   integer;
  Found1:    boolean;
begin
  Result  := 0;
  Length1 := Length(AString);
  Length2 := Length(ASubString);
  if ALastPos <> 0 then
    Length1 := ALastPos;
  if Length2 > Length1 then
    Exit
  else
  begin
    LastChar1 := ASubString[Length2];
    Index1    := Length1;
    while Index1 > 0 do
    begin
      if (AString[Index1] = LastChar1) then
      begin
        Index2 := Index1;
        Index3 := Length2;
        Found1 := Index2 >= Length2;
        while Found1 and (Index2 > 0) and (Index3 > 0) do
        begin
          Found1 := (AString[Index2] = ASubString[Index3]);
          Dec(Index2);
          Dec(Index3);
        end;
        if Found1 then
        begin
          Result := Index2 + 1;
          Exit;
        end;
      end;
      Dec(Index1);
    end;
  end;
end;

procedure PosLastAndCount(const ASubString, AString: String;
  out ALastPos: integer; out ACount: integer);
var
  {This gets the last occurance and count in one go. It saves time}
  LastChar1: Char;
  Index1:    integer;
  Index2:    integer;
  Index3:    integer;
  Length1:   integer;
  Length2:   integer;
  Found1:    boolean;
begin
  ACount   := 0;
  ALastPos := 0;
  Length1  := Length(AString);
  Length2  := Length(ASubString);
  if Length2 > Length1 then
    Exit
  else
  begin
    LastChar1 := ASubString[Length2];
    Index1    := Length1;
    while Index1 > 0 do
    begin
      if (AString[Index1] = LastChar1) then
      begin
        Index2 := Index1;
        Index3 := Length2;
        Found1 := Index2 >= Length2;
        while Found1 and (Index2 > 0) and (Index3 > 0) do
        begin
          Found1 := (AString[Index2] = ASubString[Index3]);
          Dec(Index2);
          Dec(Index3);
        end;
        if Found1 then
        begin
          if ALastPos = 0 then
            ALastPos := Index2 + 1;
          Inc(ACount);
          Index1 := Index2;
          Continue;
        end;
      end;
      Dec(Index1);
    end;
  end;
end;

{ given an existing source pos, and a text string that adds at that pos,
  calculate the new text pos
  - if the text does not contain a newline, add its length onto the Xpos
  - if the text contains newlines, then add on to the Y pos, and
    set the X pos to the text length after the last newline }
{AdemBaba}
procedure AdvanceTextPos(const AText: string; var ARow, ACol: integer);
var
  Length1: integer;
  Count1:  integer;
  Pos1:    integer;
begin
  {This is more than 3 times faster than the original.
  I have meticilously checked that it conforms with the original}
  Length1 := Length(AText);
  case Length1 of
    0: ; {Trivial case}
    1:
    begin
      case AText[1] of
        NativeCarriageReturn, NativeLineFeed:
        begin {#13 or #10}
          Inc(ACol);
          ARow := 1; // XPos is indexed from 1
        end;
        else
          Inc(ARow, Length1)
      end;
    end;
    2:
    begin
      if (AText[1] = NativeCarriageReturn) and (AText[2] = NativeLineFeed) then
      begin
        Inc(ACol);
        ARow := 1; // XPos is indexed from 1
      end
      else
        Inc(ARow, Length1);
    end;
    else
      PosLastAndCount(NativeLineBreak, AText, Pos1, Count1);
      if Pos1 <= 0 then
        Inc(ARow, Length1)
      else
      begin // multiline
        Inc(ACol, Count1);
        ARow := Length1 - (Pos1 + 1); {2 = Length(AnsiLineBreak)}

        if ARow < 1 then
          ARow := 1;
      end;
  end;
end;

function LastLineLength(const AString: string): integer;
var { in a multiline sting, how many chars on last line (after last return) }
  Pos1: integer;
begin
  Pos1 := PosLast(NativeLineBreak, AString); {AdemBaba}
  if Pos1 <= 0 then
    Result := Length(AString)
  else
    Result := Length(AString) - (Pos1 + Length(NativeLineBreak));
end;

function SplitIntoLines(s: string): TStrings;
var
  liIndex, liPos, liPosLf: integer;
  liLineEndPos, liCopyLen: integer;
  sPart: string;
begin
  Result := TStringList.Create();

  if (s = '') then
    exit;

  liIndex := 1;
  while True do
  begin
    liPos := StrSearch(NativeCrLf, s, liIndex);

    liPosLf := StrSearch(NativeLineFeed, s, liIndex);

    if ((liPosLf > 0) and
      ((liPos = 0) or (liPosLf < (liPos + 1)))) then
    begin
      liLineEndPos := liPosLf;
      liCopyLen := liLineEndPos - liIndex + 1;
      sPart := Copy(s, liIndex, liCopyLen);
      Result.Add(sPart);
      liIndex := liLineEndPos + 1;
    end
    else if liPos > 0 then
    begin
      liLineEndPos := liPos + 1;
      liCopyLen := liLineEndPos - liIndex + 1;
      sPart := Copy(s, liIndex, liCopyLen);
      Result.Add(sPart);
      liIndex := liLineEndPos + 1;
    end
    else
    begin
      // pick up the last bit
      if liIndex < Length(s) then
      begin
         sPart := Copy(s, liIndex, Length(s));
         Result.Add(sPart);
      end;
      
      break;
    end;

  end;
end;

procedure SplitIntoChangeSections(const s1, s2, SameStart, SameEnd: TStrings);
begin
  SameStart.Clear;
  SameEnd.Clear;

  // get the identical portion at the start
  while (s1.Count > 0) and (s2.Count > 0) and (s1[0] = s2[0]) do
  begin
    SameStart.Add(s1[0]);
    s1.Delete(0);
    s2.Delete(0);
  end;

  // get the identical portion at the start
  while (s1.Count > 0) and (s2.Count > 0) and
    (s1[s1.Count - 1] = s2[s2.Count - 1]) do
  begin
    SameEnd.Insert(0, s1[s1.Count - 1]);
    s1.Delete(s1.Count - 1);
    s2.Delete(s2.Count - 1);
  end;

end;

{$IFDEF DELPHI_5}

{ these functions are in Delphi 6 and up }

function IncludeTrailingPathDelimiter(const psPath: string): string;
begin
  Result := psPath;
  if StrRight(psPath, 1) <> DirDelimiter then
    Result := Result + DirDelimiter;
end;

{ dummy for D5}
function FileIsReadOnly(const psFile: string): boolean;
begin
  Result := False;
end;

{$ENDIF}

end.
