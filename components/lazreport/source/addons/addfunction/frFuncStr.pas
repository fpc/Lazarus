{*******************************************************}
{                                                       }
{         Add FastReport String Lbrary                  }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{         Copyright (c) 2001 by Stalker SoftWare        }
{                                                       }
{*******************************************************}

unit frFuncStr;

interface

{$A+,B-,E-,R-}
{.$I FR.inc}

uses
  SysUtils;

type
  TfrCharSet = set of Char;

 // RxLib
 function frWordPosition(const N: Integer; const S: string; const WordDelims: TfrCharSet): Integer;
 function frExtractWord(N: Integer; const S: string; const WordDelims: TfrCharSet): string;
 function frWordCount(const S: string; const WordDelims: TfrCharSet): Integer;
 function frIsWordPresent(const W, S: string; const WordDelims: TfrCharSet): Boolean;
 function frNPos(const C: string; S: string; N: Integer): Integer;
 function frReplaceStr(const S, Srch, Replace: string): string;

 // StLib
 function frReplicate(cStr: String; nLen :Integer) :String;
 function frPadRight(cStr: String; nLen: Integer; cChar :String) :String;
 function frPadLeft(cStr: String; nLen: Integer; cChar :String) :String;
 function frPadCenter( cStr: String; nWidth: Integer; cChar: String): String;
 function frEndPos(cStr, cSubStr: String) :Integer;
 function frCompareStr(cStr1, cStr2: String) :Integer;

 function frLeftCopy(cStr: String; nNum: Integer): String;
 function frRightCopy(cStr: String; nNum: Integer): String;

 // Delphi
 function frDelete(cStr: String; nIndex, nCount:Integer) :String;
 function frInsert(cStr1, cStr2: String; nIndex:Integer) :String;

implementation

{--------------------------------------------------------------------}
{ Return position first character N words in string S, use           }
{ const WordDelims (type TCharSet) as delimiter between words        }
{--------------------------------------------------------------------}
function frWordPosition(const N: Integer; const S: string; const WordDelims: TfrCharSet): Integer;
var
  Count, I: Integer;

begin

 Count := 0;
 I := 1;
 Result := 0;
 while (I <= Length(S)) and (Count <> N) do begin
   { skip over delimiters }
   while (I <= Length(S)) and (S[I] in WordDelims) do Inc(I);
   { if we're not beyond end of S, we're at the start of a word }
   if I <= Length(S) then Inc(Count);
   { if not finished, find the end of the current word }
   if Count <> N then
     while (I <= Length(S)) and not (S[I] in WordDelims) do Inc(I)
   else Result := I;
 end; { while }

end; { frWordPosition }

{--------------------------------------------------------------------}
{ Extract N word from string S, use WordDelims as                    }
{ delimiter between words                                            }
{--------------------------------------------------------------------}
function frExtractWord(N: Integer; const S: string; const WordDelims: TfrCharSet): string;
var
  I: Integer;
  Len: Integer;
  
begin

 Len := 0;
 I := frWordPosition(N, S, WordDelims);
 if I <> 0 then
   { find the end of the current word }
   while (I <= Length(S)) and not(S[I] in WordDelims) do begin
     { add the I'th character to result }
     Inc(Len);
     SetLength(Result, Len);
     Result[Len] := S[I];
     Inc(I);
   end; { while }
 SetLength(Result, Len);

end; { frExtractWord }

{--------------------------------------------------------------------}
{ Count words in string S, use WordDelims as delimiter               }
{ between words                                                      }
{--------------------------------------------------------------------}
function frWordCount(const S: string; const WordDelims: TfrCharSet): Integer;
var
  SLen, I: Cardinal;

begin

 Result := 0;
 I := 1;
 SLen := Length(S);
 while I <= SLen do begin
   while (I <= SLen) and (S[I] in WordDelims) do Inc(I);
   if I <= SLen then Inc(Result);
   while (I <= SLen) and not(S[I] in WordDelims) do Inc(I);
 end; { while }

end; { frWordCount }

{--------------------------------------------------------------------}
{ Check existing word W in string S, use                             }
{ WordDelims as possible delimiters between words                    }
{--------------------------------------------------------------------}
function frIsWordPresent(const W, S: string; const WordDelims: TfrCharSet): Boolean;
var
  Count, I: Integer;

begin

 Result := False;
 Count := frWordCount(S, WordDelims);
 for I := 1 to Count do
   if frExtractWord(I, S, WordDelims) = W then begin
     Result := True;
     Exit;
   end; { if }

end; { frIsWordPresent }

{--------------------------------------------------------------------}
{ Find position N substring C in string S                            }
{--------------------------------------------------------------------}
function frNPos(const C: string; S: string; N: Integer): Integer;
var
  I, P, K: Integer;

begin

 Result := 0;
 K := 0;
 for I := 1 to N do begin
   P := Pos(C, S);
   Inc(K, P);
   if (I = N) and (P > 0) then begin
     Result := K;
     Exit;
   end; { if }
   if P > 0 then Delete(S, 1, P)
   else Exit;
 end; { for }

end; { frNPos }

{--------------------------------------------------------------------}
{ Function exchange in string S all substrings Srch on               }
{ other substring, delivered as Replace.                             }
{--------------------------------------------------------------------}
function frReplaceStr(const S, Srch, Replace: string): string;
var
  I: Integer;
  Source: string;

begin

 Source := S;
 Result := '';
 repeat
   I := Pos(Srch, Source);
   if I > 0 then begin
     Result := Result + Copy(Source, 1, I - 1) + Replace;
     Source := Copy(Source, I + Length(Srch), MaxInt);
   end
   else Result := Result + Source;
 until I <= 0;

end; { frReplaceStr }

{--------------------------------------------------------------------}
{ Return nLen chars as cStr                                          }
{--------------------------------------------------------------------}
function frReplicate(cStr: String; nLen :Integer) :String;
var
  nCou :Integer;

begin

 Result := '';
 for nCou := 1 to nLen do
   Result := Result + cStr;

end; { Replicate }

{--------------------------------------------------------------------}
{ Return string filled chars cChar from left to nLen                 }
{--------------------------------------------------------------------}
function frPadLeft(cStr: String; nLen: Integer; cChar :String) :String;
var
  S :String;

begin

 S := Trim(cStr);
 Result := frReplicate(cChar, nLen-Length(S))+S;

end ; { frPadLeft }

{--------------------------------------------------------------------}
{ Return string filled chars cChar from right to nLen                }
{--------------------------------------------------------------------}
function frPadRight(cStr: String; nLen: Integer; cChar :String) :String ;
var
  S :String;

begin

 S := Trim(cStr);
 Result := S+frReplicate(cChar, nLen-Length(S));

end; { frPadRight }

{--------------------------------------------------------------------}
{ Return centered string filled chars cChar with both side           }
{--------------------------------------------------------------------}
function frPadCenter( cStr: String; nWidth: Integer; cChar: String): String;
var
  nPerSide :Integer;
  cResult  :String;

begin

 nPerSide := (nWidth - Length(cStr)) div 2;
 cResult := frPadLeft(cStr, (Length(cStr) + nPerSide), cChar);
 Result := frPadRight(cResult, nWidth, cChar);

end; { frPadCenter }

{----------------------------------------------------------------}
{ Find in string substring from end                              }
{ Return position substing if found, else return 0               }
{----------------------------------------------------------------}
function frEndPos(cStr, cSubStr: String) :Integer;
var
  nCou   :Integer;
  nLenSS :Integer;
  nLenS  :Integer;

begin

 nLenSS := Length(cSubStr);
 nLenS  := Length(cStr);
 Result := 0 ;

 if nLenSS > nLenS then Exit;

 for nCou := nLenS downto 1 do
   if Copy( cStr, nCou, nLenSS ) = cSubStr then begin
     Result := nCou;
     Exit;
   end; { if }

end; { frEndPos }

{--------------------------------------------------------------------}
{ Return substring from first char to nNum                           }
{--------------------------------------------------------------------}
function frLeftCopy( cStr: String; nNum: Integer ): String;
begin
 Result := Copy( cStr, 1, nNum );
end; { frLeftCopy }

{--------------------------------------------------------------------}
{ Return substring from last char to position nNum                   }
{--------------------------------------------------------------------}
function frRightCopy( cStr: String; nNum: Integer ): String;
begin
 Result := '';
 if nNum > Length( cStr ) then Exit;
 Result := Copy( cStr, (Length(cStr) - nNum + 1), Length(cStr) );
end; { frRightCopy }

{--------------------------------------------------------------------}
{ Delete nCount chars in string cStr from position nIndex            }
{--------------------------------------------------------------------}
function frDelete(cStr: String; nIndex, nCount:Integer) :String;
begin
 Delete(cStr,nIndex,nCount);
 Result := cStr;
end; { frDelete }

{--------------------------------------------------------------------}
{ Insert string cStr2 into string cStr1, from position nIndex        }
{--------------------------------------------------------------------}
function frInsert(cStr1, cStr2: String; nIndex:Integer) :String;
begin
 Insert(cStr1,cStr2,nIndex);
 Result := cStr2;
end; { frDelete }

{----------------------------------------------------------------}
{ Compare cStr1 and cStr2 and return number of the position      }
{ difference strings                                             }
{----------------------------------------------------------------}
function frCompareStr(cStr1, cStr2: String) :Integer;
var
  nLenMax :Integer;
  nCou    :Integer;

begin

 Result := 0;

 if Length( cStr1 ) > Length( cStr2 ) then
   nLenMax := Length( cStr1 )
 else
   nLenMax := Length( cStr2 );

 for nCou := 1 to nLenMax do
   if Copy( cStr1, nCou, 1) <> Copy( cStr2, nCou, 1) then begin
     Result := nCou;
     Exit;
   end; { if }

end; { frCompareStr }

end.
