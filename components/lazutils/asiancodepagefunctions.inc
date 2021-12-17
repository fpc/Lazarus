{%MainUnit ../lconvencoding.pp}

{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Asian Unicode Functions.
  The clipboard is able to work with the windows and gtk behaviour/features.
}

function DBCSToUTF8(const s: string; const ArrayUni, ArrayCP: array of word; CodeP: integer): string;
const
  cp936unodered:array[0..254] of Uint32=($a2ab,$a2ac,$a2ad,$a2ae,$a2af,$a2b0,$a2e3,$a2e4,$a2ef,$a2f0,$a2fd,$a2fe,$a4f4,$a4f5,$a4f6,$a4f7,$a4f8,$a4f9,$a4fa,$a4fb,$a4fc,$a4fd,$a4fe,$a5f7,$a5f8,$a5f9,$a5fa,$a5fb,$a5fc,$a5fd,$a5fe,$a6b9,$a6ba,$a6bb,$a6bc,$a6bd,$a6be,$a6bf,$a6c0,$a6d9,$a6da,$a6db,$a6dc,$a6dd,$a6de,$a6df,$a6ec,$a6ed,$a6f3,$a6f6,$a6f7,$a6f8,$a6f9,$a6fa,$a6fb,$a6fc,$a6fd,$a6fe,$a7c2,$a7c3,$a7c4,$a7c5,$a7c6,$a7c7,$a7c8,$a7c9,$a7ca,$a7cb,$a7cc,$a7cd,$a7ce,$a7cf,$a7d0,$a7f2,$a7f3,$a7f4,$a7f5,$a7f6,$a7f7,$a7f8,$a7f9,$a7fa,$a7fb,$a7fc,$a7fd,$a7fe,$a896,$a897,$a898,$a899,$a89a,$a89b,$a89c,$a89d,$a89e,$a89f,$a8a0,$a8bc,$a8bf,$a8c1,$a8c2,$a8c3,$a8c4,$a8ea,$a8eb,$a8ec,$a8ed,$a8ee,$a8ef,$a8f0,$a8f1,$a8f2,$a8f3,$a8f4,$a8f5,$a8f6,$a8f7,$a8f8,$a8f9,$a8fa,$a8fb,$a8fc,$a8fd,$a8fe,$a958,$a95b,$a95d,$a95e,$a95f,$a989,$a98a,$a98b,$a98c,$a98d,$a98e,$a98f,$a990,$a991,$a992,$a993,$a994,$a995,$a997,$a998,$a999,$a99a,$a99b,$a99c,$a99d,$a99e,$a99f,$a9a0,$a9a1,$a9a2,$a9a3,$a9f0,$a9f1,$a9f2,$a9f3,$a9f4,$a9f5,$a9f6,$a9f7,$a9f8,$a9f9,$a9fa,$a9fb,$a9fc,$a9fd,$a9fe,$d7fa,$d7fb,$d7fc,$d7fd,$d7fe,$fe50,$fe51,$fe52,$fe53,$fe54,$fe55,$fe56,$fe57,$fe58,$fe59,$fe5a,$fe5b,$fe5c,$fe5d,$fe5e,$fe5f,$fe60,$fe61,$fe62,$fe63,$fe64,$fe65,$fe66,$fe67,$fe68,$fe69,$fe6a,$fe6b,$fe6c,$fe6d,$fe6e,$fe6f,$fe70,$fe71,$fe72,$fe73,$fe74,$fe75,$fe76,$fe77,$fe78,$fe79,$fe7a,$fe7b,$fe7c,$fe7d,$fe7e,$fe80,$fe81,$fe82,$fe83,$fe84,$fe85,$fe86,$fe87,$fe88,$fe89,$fe8a,$fe8b,$fe8c,$fe8d,$fe8e,$fe8f,$fe90,$fe91,$fe92,$fe93,$fe94,$fe95,$fe96,$fe97,$fe98,$fe99,$fe9a,$fe9b,$fe9c,$fe9d,$fe9e,$fe9f,$fea0);
  cp936unoderedstart:Uint32=$e766;
var
  len, l,i: Integer;
  Src, Dest: PChar;
  c: char;
  code,code1: word;
  Hbyte,Lbyte:byte;
begin
  if s = '' then exit('');
  len := length(s);
  SetLength(Result, len * 4);// Asia UTF-8 is at most 4 bytes
  Src  := PChar(s);
  Dest := PChar(Result);
  repeat
    c := Src^;
    Hbyte := byte(Src^);
    Inc(Src);

    if Ord(c) < 128 then
    begin
      if (c=#0) and (Src-PChar(s)>=len) then break;
      Dest^ := c;
      Inc(Dest);
    end
    else
    begin
      code := Byte(c) shl 8;
      c:=Src^;
      Lbyte := byte(Src^);
      if (c=#0) and (Src-PChar(s)>=len) then break;
      code := code + Byte(c);
      code1:=code;
      Inc(Src);

      code := ArrayUni[SearchTable(ArrayCP, code)];
      if code=0 then
      begin
        case CodeP of
          936:
            begin

              if (HByte>=$AA) and (HByte<=$AF) and (LByte>=$A1) and (LByte<=$FE) then
              begin
                   code:= $e000 + (94 * (HByte-$aa)) + (LByte-$a1);
              end
              else
              if (HByte>=$F8) and (HByte<=$FE) and (LByte>=$A1) and (LByte<=$FE) then
              begin
                   code:= $e234 + (94 * (HByte-$f8)) + (LByte-$a1);
              end
              else
              if (HByte>=$A1) and (HByte<=$A7) and (LByte>=$40) and (LByte<=$A0) then
              begin
                   code:= $e4C6 + (97 * (HByte-$A1)) + (LByte-$40);
              end
              else
              begin
                for i:=0 to length(cp936unodered)-1 do
                begin
                  if code1=cp936unodered[i] then
                  begin
                       code:=cp936unoderedstart+i;
                       break;
                  end;
                end;
              end;
            end;
          950:
            begin
                 if (HByte>=$81) and (HByte<=$8d) and (LByte>=$40) and (LByte<=$FE) then
                 begin
                      code:= $eeb8 + (157 * (Hbyte-$81)) ;
                      if (Lbyte<$80) then
                       code:=code + (Lbyte-$40)
                      else
                       code:=code + (Lbyte-$62);
                 end
                 else
                 if (HByte>=$8e) and (HByte<=$a0) and (LByte>=$40) and (LByte<=$FE) then
                 begin
                   code:= $e311 + (157 * (Hbyte-$8e));
                   if (Lbyte<$80) then
                    code:=code + (Lbyte-$40)
                   else
                    code:=code + (Lbyte-$62);
                 end
                 else
                 if (HByte>=$c6) and (HByte<=$c8) and (LByte>=$a1) and (LByte<=$FE) then
                 begin
                   code:= $f672 + (157 * (Hbyte-$c6)) ;
                   if (Lbyte<$80) then
                    code:=code + (Lbyte-$40)
                   else
                    code:=code + (Lbyte-$62);
                 end
                 else
                 if (HByte>=$fa) and (HByte<=$fe) and (LByte>=$40) and (LByte<=$FE) then
                 begin
                   code:= $e000 + (157 * (Hbyte-$fa)) ;
                   if (Lbyte<$80) then
                    code:=code + (Lbyte-$40)
                   else
                    code:=code + (Lbyte-$62);
                 end;
            end;
          949:
            begin
                 if (HByte=$c9) and (LByte>=$a1) and (LByte<=$fe) then
                 begin
                      code:= $e000 + (94 * (HByte-$c9)) + (LByte-$a1) ;
                 end
                 else
                 if (HByte=$fe) and (LByte>=$a1) and (LByte<=$fe) then
                 begin
                      code:= $e05e + (94 * (HByte-$fe)) + (LByte-$a1) ;
                 end
                 else
                 if code1=$ff then
                 begin
                      code:= $f8f7;
                 end;
            end;
          932:
            begin
                 if (HByte>=$f0) and (HByte<=$f9) and (LByte>=$40) and (LByte<=$fc) then
                 begin
                      code:= $e000 + (188 * (HByte-$f0)) + (LByte-$40) ;
                      if LByte>$7f then
                         code:=code-1;
                 end
                 else
                 begin
                   case code1 of
                     $00a0:code:=$f8f0;
                     $00fd:code:=$f8f1;
                     $00fe:code:=$f8f2;
                     $00ff:code:=$f8f3;
                   end;

                 end;
            end
          else
            code := 0;
        end;
      end;
      if code>0 then
      begin
        l:=UnicodeToUTF8Inline(code,Dest);
        inc(Dest,l);
      end
      else
      case ConvertEncodingErrorMode of
        ceemSkip:
          begin end;
        ceemException:
          raise EConvertError.Create('Cannot convert DBCS code page to UTF8');
        ceemReplace:
          begin
            Dest^:='?';
            Inc(Dest);
          end;
        ceemReturnEmpty:
          Exit('');
      end;
    end;
  until false;
  SetLength(Result, {%H-}PtrUInt(Dest) - PtrUInt(Result));
  SetCodePage(RawByteString(Result), CP_UTF8, False);
end;

function CP936ToUTF8(const s: string): string;
begin
  Result := DBCSToUTF8(s, Uni936C, CP936CC,936);
end;

function CP950ToUTF8(const s: string): string;
begin
  Result := DBCSToUTF8(s, Uni950C, CP950CC,950);
end;

function CP949ToUTF8(const s: string): string;
begin
  Result := DBCSToUTF8(s, Uni949C, CP949CC,949);
end;

function CP932ToUTF8(const s: string): string;
begin
  Result := DBCSToUTF8(s, Uni932C, CP932CC,932);
end;

{$IfNDef UseSystemCPConv}
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
{$endif}

procedure InternalUTF8ToDBCS(const s: string; TargetCodePage: TSystemCodePage;
  SetTargetCodePage: boolean;
  {$IfNDef UseSystemCPConv}const UTF8CharConvFunc: TUnicodeToCharID;{$endif}
  out TheResult: RawByteString); inline;
begin
  {$ifdef UseSystemCPConv}
  TheResult:=s;
  SetCodePage(TheResult, TargetCodePage, True);
  if not SetTargetCodePage then
    SetCodePage(TheResult, CP_ACP, False);
  {$else}
  TheResult:=UTF8ToDBCS(s,UTF8CharConvFunc);
  if SetTargetCodePage then
    SetCodePage(TheResult, TargetCodePage, False);
  {$endif}
end;

function UTF8ToCP932(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  InternalUTF8ToDBCS(s,932,SetTargetCodePage{$IfNDef UseSystemCPConv},@UnicodeToCP932{$endif},Result);
end;

function UTF8ToCP936(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  InternalUTF8ToDBCS(s,936,SetTargetCodePage{$IfNDef UseSystemCPConv},@UnicodeToCP936{$endif},Result);
end;

function UTF8ToCP949(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  InternalUTF8ToDBCS(s,949,SetTargetCodePage{$IfNDef UseSystemCPConv},@UnicodeToCP949{$endif},Result);
end;

function UTF8ToCP950(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  InternalUTF8ToDBCS(s,950,SetTargetCodePage{$IfNDef UseSystemCPConv},@UnicodeToCP950{$endif},Result);
end;

function UTF8ToDBCS(const s: string; const UTF8CharConvFunc: TUnicodeToCharID): string;
var
  len, i, CharLen: integer;
  Src, Dest: PChar;
  c: char;
  Unicode: longword;
begin
  if s = '' then exit('');
  len := length(s);
  SetLength(Result, len); // DBCS needs at most space as UTF-8
  Src  := PChar(s);
  Dest := PChar(Result);
  repeat
    c := Src^;
    if c < #128 then
    begin
      if (c=#0) and (Src-PChar(s)>=len) then break;
      Dest^ := c;
      Inc(Dest);
      Inc(Src);
    end
    else
    begin
      Unicode := UTF8CodepointToUnicode(Src, CharLen);
      Inc(Src, CharLen);
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
      end
      else
      case ConvertEncodingErrorMode of
        ceemSkip:
          begin end;
        ceemException:
          raise EConvertError.Create('Cannot convert UTF8 to DBCS code page');
        ceemReplace:
          begin
            Dest^ := '?';
            Inc(Dest);
          end;
        ceemReturnEmpty:
          Exit('');
      end;
    end;
  until false;
  //SetLength(Result, Dest - PChar(Result));
  SetLength(Result, {%H-}PtrUInt(Dest) - PtrUInt(Result));
end;
