{%MainUnit ../lconvencoding.pp}

{******************************************************************************
                               Asian Unicode Functions
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  The clipboard is able to work with the windows and gtk behaviour/features.
}

function SingleByteToUTF8Ex(const s: string; CodeP: integer): string;
var
  len:  integer;
  i, j:    integer;
  Src:  PChar;
  Dest: PChar;
  c:    char;
  tempstr: ansistring;
  tempint: integer;
begin
  SetLength(tempstr, 4);
  if s = '' then
  begin
    Result := s;
    exit;
  end;
  len := length(s);
  SetLength(Result, len * 4);// Asia UTF-8 is at most 4 bytes
  Src  := PChar(s);
  Dest := PChar(Result);
  i    := 1;
  while i < len do
  begin
    c := Src^;
    Inc(Src);
    i := i + 1;
    if Ord(c) < 128 then
    begin
      Dest^ := c;
      Inc(Dest);
    end
    else
    begin
      tempint := Byte(c) shl 8;
      if i <= len then
      begin
        tempint := tempint + Byte(Src^);
        i := i + 1;
      end;

      Inc(Src);

      case CodeP of
        936:
          tempint := Uni936C[SearchTable(CP936CC, tempint)];
        950:
          tempint := Uni950C[SearchTable(CP950CC, tempint)];
        949:
          tempint := Uni949C[SearchTable(CP949CC, tempint)];
        932:
          tempint := Uni932C[SearchTable(CP932CC, tempint)];
        else
          tempint := -1;
      end;

      if tempint <> -1 then
      begin
        TempStr := UnicodeToUTF8(tempint);

        for j := 1 to Length(TempStr) do
        begin
          Dest^ := TempStr[j];
          Inc(Dest);
        end;
      end;
    end;
  end;
  SetLength(Result, PtrUInt(Dest) - PtrUInt(Result));
end;

function CP936ToUTF8(const s: string): string;
begin
  Result := SingleByteToUTF8Ex(s, 936);
end;

function CP950ToUTF8(const s: string): string;
begin
  Result := SingleByteToUTF8Ex(s, 950);
end;

function CP949ToUTF8(const s: string): string;
begin
  Result := SingleByteToUTF8Ex(s, 949);
end;

function CP932ToUTF8(const s: string): string;
begin
  Result := SingleByteToUTF8Ex(s, 932);
end;

function UnicodeToCP936(Unicode: cardinal): integer;
begin
  case Unicode of
    0..127: Result := Unicode;
    else
      Result := CP936CU[SearchTable(Uni936U, Unicode)];
  end;
end;

function UnicodeToCP950(Unicode: cardinal): integer;
begin
  case Unicode of
    0..127: Result := Unicode;
    else
      Result := CP950CU[SearchTable(Uni950U, Unicode)];
  end;
end;

function UnicodeToCP949(Unicode: cardinal): integer;
begin
  case Unicode of
    0..127: Result := Unicode;
    else
      Result := CP949CU[SearchTable(Uni949U, Unicode)];
  end;
end;

function UnicodeToCP932(Unicode: cardinal): integer;
begin
  case Unicode of
    0..127: Result := Unicode;
    else
      Result := CP932CU[SearchTable(Uni932U, Unicode)];
  end;
end;

function UTF8ToSingleByteEx(const s: string;
  const UTF8CharConvFunc: TUnicodeToCharID): string;
var
  len:  integer;
  Src:  PChar;
  Dest: PChar;
  c:    char;
  Unicode: longword;
  CharLen: integer;
  i:    integer;
begin
  if s = '' then
  begin
    Result := '';
    exit;
  end;
  len := length(s);
  SetLength(Result, len);
  Src  := PChar(s);
  Dest := PChar(Result);
  while len > 0 do
  begin
    c := Src^;
    if c < #128 then
    begin
      Dest^ := c;
      Inc(Dest);
      Inc(Src);
      Dec(len);
    end
    else
    begin
      Unicode := UTF8CharacterToUnicode(Src, CharLen);
      Inc(Src, CharLen);
      Dec(len, CharLen);
      i := UTF8CharConvFunc(Unicode);
      //writeln(Format('%X', [i]));
      if i >= 0 then
      begin
        if i > $ff then
        begin
          Dest^ := chr(i shr 8);
          Inc(Dest);
          Dest^ := chr(i);
        end
        else
          Dest^ := chr(i);
        Inc(Dest);
      end;
    end;
  end;
  //SetLength(Result, Dest - PChar(Result));
  SetLength(Result, PtrUInt(Dest) - PtrUInt(Result));
end;

function UTF8ToCP936(const s: string): string;
begin
  Result := UTF8ToSingleByteEx(s, @UnicodeToCP936);
end;

function UTF8ToCP950(const s: string): string;
begin
  Result := UTF8ToSingleByteEx(s, @UnicodeToCP950);
end;

function UTF8ToCP949(const s: string): string;
begin
  Result := UTF8ToSingleByteEx(s, @UnicodeToCP949);
end;

function UTF8ToCP932(const s: string): string;
begin
  Result := UTF8ToSingleByteEx(s, @UnicodeToCP932);
end;

