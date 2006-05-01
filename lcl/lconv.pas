unit LConv;
{This unit is to be inserted into LCL}
{$mode objfpc}{$H+}
//As iconv is Linux command, there is no sense in Windows
{$IFDEF MSWindows}
{$DEFINE WINDOWS}
{$ENDIF}
{$IFDEF WINDOWS}
{$WARNINI Windows/Wine/ReactOS locale conversion is not fully supported yet. Sorry.}
{$ENDIF}
{$IFDEF UNIX}
 {$IFNDEF NOLIBC}
 {$DEFINE UNIXLibc}
 {$ENDIF}
{$ENDIF}
interface
uses SysUtils,classes{$IFDEF UNIXLibc},libc{$ENDIF}{$IFDEF UNIX},unix{$ENDIF};
function CPConvert(const s,from,toC:string):string;
function GetDefaultCodepage:string;
implementation
var GotCodepage:boolean=false;
    Codepage:string='ANSI';
function GetDefaultCodepage:string;
var Lang:string;
    i:integer;
    s:string;
begin
  if GotCodepage then begin Result:=Codepage;exit;end;
  {$ifndef UNIXLibc}
  Result:='ANSI';
  Lang:=GetEnvironmentVariable('LANG');
  i:=pos('.',Lang);
  if (i>0)and(i<=length(Lang)) then Result:=copy(Lang,i+1,length(Lang)-i);
  {$ELSE}
  Result:=nl_langinfo(CODESET);
  {$ENDIF}
  //Check parameters
  for i:=1 to ParamCount do
  begin
    s:=ParamStr(i);
    if s='--charset=' then Result:=copy(s,pos('=',s),length(s));
  end;
  Codepage:=Result;
  GotCodepage:=true;
end;

function Utf2Cp1251(s:string):string;
var i:integer;
    Skip,DSkip:boolean;
begin
  //TODO Complete codepage conversion
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

function cp1251ToKoi8r(s:string):string;
var i:integer;
begin
  Result:='';
  for i:=1 to length(s) do
  begin
    if s[i]<=#127 then Result:=Result+s[i] else
    case s[i] of
      'À':Result:=Result+'á';
      'Á':Result:=Result+'â';
      'Â':Result:=Result+'÷';
      'Ã':Result:=Result+'ç';
      'Ä':Result:=Result+'ä';
      'Å':Result:=Result+'å';
      '¨':Result:=Result+'³';
      'Æ':Result:=Result+'ö';
      'Ç':Result:=Result+'ú';
      'È':Result:=Result+'é';
      'É':Result:=Result+'ê';
      'Ê':Result:=Result+'ë';
      'Ë':Result:=Result+'ì';
      'Ì':Result:=Result+'í';
      'Í':Result:=Result+'î';
      'Î':Result:=Result+'ï';
      'Ï':Result:=Result+'ð';
      'Ð':Result:=Result+'ò';
      'Ñ':Result:=Result+'ó';
      'Ò':Result:=Result+'ô';
      'Ó':Result:=Result+'õ';
      'Ô':Result:=Result+'æ';
      'Õ':Result:=Result+'è';
      'Ö':Result:=Result+'ã';
      '×':Result:=Result+'þ';
      'Ø':Result:=Result+'û';
      'Ù':Result:=Result+'ý';
      'Ú':Result:=Result+'ÿ';
      'Û':Result:=Result+'ù';
      'Ü':Result:=Result+'ø';
      'Ý':Result:=Result+'ü';
      'Þ':Result:=Result+'à';
      'ß':Result:=Result+'ñ';
      'à':Result:=Result+'Á';
      'á':Result:=Result+'Â';
      'â':Result:=Result+'×';
      'ã':Result:=Result+'Ç';
      'ä':Result:=Result+'Ä';
      'å':Result:=Result+'Å';
      '¸':Result:=Result+'£';
      'æ':Result:=Result+'Ö';
      'ç':Result:=Result+'Ú';
      'è':Result:=Result+'É';
      'é':Result:=Result+'Ê';
      'ê':Result:=Result+'Ë';
      'ë':Result:=Result+'Ì';
      'ì':Result:=Result+'Í';
      'í':Result:=Result+'Î';
      'î':Result:=Result+'Ï';
      'ï':Result:=Result+'Ð';
      'ð':Result:=Result+'Ò';
      'ñ':Result:=Result+'Ó';
      'ò':Result:=Result+'Ô';
      'ó':Result:=Result+'Õ';
      'ô':Result:=Result+'Æ';
      'õ':Result:=Result+'È';
      'ö':Result:=Result+'Ã';
      '÷':Result:=Result+'Þ';
      'ø':Result:=Result+'Û';
      'ù':Result:=Result+'Ý';
      'ú':Result:=Result+'ß';
      'û':Result:=Result+'Ù';
      'ü':Result:=Result+'Ø';
      'ý':Result:=Result+'Ü';
      'þ':Result:=Result+'À';
      'ÿ':Result:=Result+'Ñ';
     else Result:=s[i];
    end;
  end;
end;

function CPConvert(const s,from,toC:string):string;
var AFrom,ATo:string;
    SL:TStringList;
    FN1,FN2:string;
begin
  Result:=s;
  AFrom:=LowerCase(from);
  ATo:=LowerCase(toC);
  if (AFrom='utf8') or (AFrom='utf-8') then
  begin
    if ATo='cp1251' then begin Result:=utf2cp1251(s);exit;end;
    if ATo='koi8-r' then begin Result:=cp1251ToKoi8r(utf2cp1251(s));exit;end;
  end;
//Stupid code. Works anyway, but extra-slow
 {$ifdef Unix}
  SL:=TStringList.Create;
  SL.Text:=s;
  FN1:=GetTempFileName;
  SL.SaveToFile(FN1);
  FN2:=GetTempFileName;
  Shell('iconv -f '+from+' -t '+toC+'<'+FN1+' >'+FN2);
  SL.LoadFromFile(FN2);
  if SL.Text<>'' then  Result:=SL.Text else Result:=s;
  DeleteFile(FN1);DeleteFile(FN2);
{$endif}
end;
end.