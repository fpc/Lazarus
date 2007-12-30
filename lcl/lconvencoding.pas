{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit LConvEncoding;

{$mode objfpc}{$H+}
//As iconv is Linux command, there is no sense in Windows
{$IFDEF MSWindows}
{$DEFINE WINDOWS}
{$ENDIF}
{$IFDEF WINDOWS}
{$WARNING Windows/Wine/ReactOS locale conversion is not fully supported yet. Sorry.}
{$ENDIF}

interface

uses
  SysUtils, Classes, dos, LCLProc
  {$IFDEF UNIX},unix{$ENDIF};

const
  EncodingUTF8 = 'utf8';

function GuessEncoding(const s: string): string;

function ConvertEncoding(const s, FromEncoding, ToEncoding: string): string;

function GetSystemEncoding: string;


implementation


var EncodingValid: boolean=false;
    SystemEncoding: string='ANSI';

function GetSystemEncoding: string;
var Lang: string;
    i: integer;
    s: string;
begin
  if EncodingValid then begin
    Result:=SystemEncoding;
    exit;
  end;

  Result:='ANSI';
  lang := GetEnv('LC_ALL');
  if Length(lang) = 0 then
  begin
    lang := GetEnv('LC_MESSAGES');
    if Length(lang) = 0 then
    begin
      lang := GetEnv('LANG');
    end;
  end;
  i:=pos('.',Lang);
  if (i>0) and (i<=length(Lang)) then
    Result:=copy(Lang,i+1,length(Lang)-i);
  //Check parameters
  for i:=1 to ParamCount do
  begin
    s:=ParamStr(i);
    if s='--charset=' then Result:=copy(s,pos(#61,s),length(s));
  end;
  SystemEncoding:=Result;
  EncodingValid:=true;
end;

function Utf2Cp1251(s:string):string;
var i:integer;
    Skip,DSkip:boolean;
begin
  //TODO Complete SystemEncoding conversion
  Skip:=false;DSkip:=false;Result:='';
  for i:=1 to length(s) do
  begin
    if DSkip then begin Skip:=true;DSkip:=false;continue;end;
    if Skip then begin Skip:=false;Continue;end;
    if s[i]<#127 then begin Result:=Result+s[i];continue; end;
    if i=length(s) then break;//Do not translate 'strange' symbol
    if (s[i]=chr($D0)) and (s[i+1]>=chr($90))and (s[i+1]<chr($C0)) then begin
      Result:=Result+chr(ord(s[i+1])-$90+192);Skip:=true;continue;
    end;
    if (s[i]=chr($D1)) and (s[i+1]>=chr($80))and (s[i+1]<chr($90)) then begin
      Result:=Result+chr(ord(s[i+1])-$80+240);Skip:=true;continue;
    end;
    if (s[i]=chr($D0)) and (s[i+1]=chr($81)) then begin
      Result:=Result+#168;Skip:=true;continue;
    end;
    if (s[i]=chr($D1)) and (s[i+1]=chr($91)) then begin
      Result:=Result+#184;Skip:=true;continue;
    end;
    Result:=Result+s[i];
  end;
end;

function Cp1251toUTF(s:string):string;
var i:integer;
begin
  //TODO Complete SystemEncoding conversion
  Result:='';
  for i:=1 to length(s) do
  begin
    case s[i] of
      #0..#127:Result:=Result+s[i];
      #192..#239:Result:=Result+chr($D0)+chr(ord(s[i])-192+$90);
      #240..#255:Result:=Result+chr($D1)+chr(ord(s[i])-240+$80);
      #168:Result:=Result+chr($D0)+chr($81);
      #184:Result:=Result+chr($D1)+chr($91);
    end;
  end;
end;

function cp1251ToKoi8r(s:string):string;
var i:integer;
begin
  Result:='';
  for i:=1 to length(s) do
  begin
    if s[i]<=#127 then Result:=Result+s[i] else
    case s[i] of
      #192:Result:=Result+#225;
      #193:Result:=Result+#226;
      #194:Result:=Result+#247;
      #195:Result:=Result+#231;
      #196:Result:=Result+#228;
      #197:Result:=Result+#229;
      #168:Result:=Result+#179;
      #198:Result:=Result+#246;
      #199:Result:=Result+#250;
      #200:Result:=Result+#233;
      #201:Result:=Result+#234;
      #202:Result:=Result+#235;
      #203:Result:=Result+#236;
      #204:Result:=Result+#237;
      #205:Result:=Result+#238;
      #206:Result:=Result+#239;
      #207:Result:=Result+#240;
      #208:Result:=Result+#242;
      #209:Result:=Result+#243;
      #210:Result:=Result+#244;
      #211:Result:=Result+#245;
      #212:Result:=Result+#230;
      #213:Result:=Result+#232;
      #214:Result:=Result+#227;
      #215:Result:=Result+#254;
      #216:Result:=Result+#251;
      #217:Result:=Result+#253;
      #218:Result:=Result+#255;
      #219:Result:=Result+#249;
      #220:Result:=Result+#248;
      #221:Result:=Result+#252;
      #222:Result:=Result+#224;
      #223:Result:=Result+#241;
      #224:Result:=Result+#193;
      #225:Result:=Result+#194;
      #226:Result:=Result+#215;
      #227:Result:=Result+#199;
      #228:Result:=Result+#196;
      #229:Result:=Result+#197;
      #184:Result:=Result+#163;
      #230:Result:=Result+#214;
      #231:Result:=Result+#218;
      #232:Result:=Result+#201;
      #233:Result:=Result+#202;
      #234:Result:=Result+#203;
      #235:Result:=Result+#204;
      #236:Result:=Result+#205;
      #237:Result:=Result+#206;
      #238:Result:=Result+#207;
      #239:Result:=Result+#208;
      #240:Result:=Result+#210;
      #241:Result:=Result+#211;
      #242:Result:=Result+#212;
      #243:Result:=Result+#213;
      #244:Result:=Result+#198;
      #245:Result:=Result+#200;
      #246:Result:=Result+#195;
      #247:Result:=Result+#222;
      #248:Result:=Result+#219;
      #249:Result:=Result+#221;
      #250:Result:=Result+#223;
      #251:Result:=Result+#217;
      #252:Result:=Result+#216;
      #253:Result:=Result+#220;
      #254:Result:=Result+#192;
      #255:Result:=Result+#209;
     else Result:=s[i];
    end;
  end;
end;

function Koi8rToCP1251(s:string):string;
var i:integer;
begin
  Result:='';
  for i:=1 to length(s) do
  begin
    if s[i]<=#127 then Result:=Result+s[i] else
    case s[i] of
      #225:Result:=Result+#192;
      #226:Result:=Result+#193;
      #247:Result:=Result+#194;
      #231:Result:=Result+#195;
      #228:Result:=Result+#196;
      #229:Result:=Result+#197;
      #179:Result:=Result+#168;
      #246:Result:=Result+#198;
      #250:Result:=Result+#199;
      #233:Result:=Result+#200;
      #234:Result:=Result+#201;
      #235:Result:=Result+#202;
      #236:Result:=Result+#203;
      #237:Result:=Result+#204;
      #238:Result:=Result+#205;
      #239:Result:=Result+#206;
      #240:Result:=Result+#207;
      #242:Result:=Result+#208;
      #243:Result:=Result+#209;
      #244:Result:=Result+#210;
      #245:Result:=Result+#211;
      #230:Result:=Result+#212;
      #232:Result:=Result+#213;
      #227:Result:=Result+#214;
      #254:Result:=Result+#215;
      #251:Result:=Result+#216;
      #253:Result:=Result+#217;
      #255:Result:=Result+#218;
      #249:Result:=Result+#219;
      #248:Result:=Result+#220;
      #252:Result:=Result+#221;
      #224:Result:=Result+#222;
      #241:Result:=Result+#223;
      #193:Result:=Result+#224;
      #194:Result:=Result+#225;
      #215:Result:=Result+#226;
      #199:Result:=Result+#227;
      #196:Result:=Result+#228;
      #197:Result:=Result+#229;
      #163:Result:=Result+#184;
      #214:Result:=Result+#230;
      #218:Result:=Result+#231;
      #201:Result:=Result+#232;
      #202:Result:=Result+#233;
      #203:Result:=Result+#234;
      #204:Result:=Result+#235;
      #205:Result:=Result+#236;
      #206:Result:=Result+#237;
      #207:Result:=Result+#238;
      #208:Result:=Result+#239;
      #210:Result:=Result+#240;
      #211:Result:=Result+#241;
      #212:Result:=Result+#242;
      #213:Result:=Result+#243;
      #198:Result:=Result+#244;
      #200:Result:=Result+#245;
      #195:Result:=Result+#246;
      #222:Result:=Result+#247;
      #219:Result:=Result+#248;
      #221:Result:=Result+#249;
      #223:Result:=Result+#250;
      #217:Result:=Result+#251;
      #216:Result:=Result+#252;
      #220:Result:=Result+#253;
      #192:Result:=Result+#254;
      #209:Result:=Result+#255;
     else Result:=s[i];
    end;
  end;
end;

function GuessEncoding(const s: string): string;
var
  l: Integer;
  p: Integer;
  EndPos: LongInt;
  i: LongInt;
  
  function CompareI(p1, p2: PChar; Count: integer): boolean;
  var
    i: Integer;
    Chr1: Byte;
    Chr2: Byte;
  begin
    for i:=1 to Count do begin
      Chr1 := byte(p1^);
      Chr2 := byte(p2^);
      if Chr1<>Chr2 then begin
        if Chr1 in [97..122] then
          dec(Chr1,32);
        if Chr2 in [97..122] then
          dec(Chr2,32);
        if Chr1<>Chr2 then exit(false);
      end;
      inc(p1);
      inc(p2);
    end;
    Result:=true;
  end;
  
begin
  l:=length(s);
  if l=0 then begin
    Result:='';
    exit;
  end;
  
  // try BOM
  if CompareI(@s[1],#$EF#$BB#$BF,3) then begin
    Result:=EncodingUTF8;
    exit;
  end;
  
  // try {%encoding eee}
  if CompareI(@s[1],'{%encoding ',11) then begin
    p:=12;
    while (p<=l) and (s[p] in [' ',#9]) do inc(p);
    EndPos:=p;
    while (EndPos<=l) and (not (s[EndPos] in ['}',' ',#9])) do inc(EndPos);
    Result:=copy(s,p,EndPos-p);
    exit;
  end;
  
  // try UTF-8 (this includes ASCII)
  p:=1;
  while (p<=l) do begin
    if ord(s[p])<128 then begin
      // ASCII
      inc(p);
    end else begin
      i:=UTF8CharacterStrictLength(@s[p]);
      if i=0 then break;
      inc(p);
    end;
  end;
  if p>l then begin
    Result:=EncodingUTF8;
    exit;
  end;
  
  // use system encoding
  Result:=GetSystemEncoding;
end;

function ConvertEncoding(const s, FromEncoding, ToEncoding: string): string;
var AFrom,ATo:string;
    SL:TStringList;
    FN1,FN2:string;
begin
  Result:=s;
  AFrom:=LowerCase(FromEncoding);
  ATo:=LowerCase(ToEncoding);
  if AFrom=ATo then exit;
  if ATo='koi8r' then ATo:='koi8-r';
  if AFrom='koi8r' then AFrom:='koi8-r';
  if (AFrom='utf8') or (AFrom='utf-8') then
  begin
    if ATo='cp1251' then begin Result:=utf2cp1251(s);exit;end;
    if ATo='koi8-r' then begin Result:=cp1251ToKoi8r(utf2cp1251(s));exit;end;
  end;
  if (ATo='utf8') or (ATo='utf-8') then
  begin
    if AFrom='cp1251' then begin Result:=Cp1251toUTF(s);exit;end;
    if AFrom='koi8-r' then begin Result:=Cp1251toUTF(Koi8rToCP1251(s));exit;end;
  end;
  //Stupid code. Works anyway, but extra-slow
  {$ifdef Unix}
  DebugLn(['CPConvert NOTE: using slow iconv workaround to convert from ',AFrom,' to ',ATo]);
  SL:=TStringList.Create;
  SL.Text:=s;
  FN1:=GetTempFileName;
  SL.SaveToFile(FN1);
  FN2:=GetTempFileName;
  fpSystem('iconv -f '+FromEncoding+' -t '+ToEncoding+#60+FN1+' >'+FN2);
  SL.LoadFromFile(FN2);
  if SL.Text<>'' then
    Result:=SL.Text
  else
    Result:=s;
  DeleteFile(FN1);
  DeleteFile(FN2);
  {$endif}
end;

end.
