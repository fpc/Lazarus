{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU Library General Public License          *
 *   as published by  the Free Software Foundation; either version 2 of    *
 *   the License, or  (at your option) any later version.                  *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU Library General Public License is available on the  *
 *   Web at <http://www.gnu.org/copyleft/lgpl.html>. You can also          *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Olivier GUILBAUD

  Abstract:
    This code initialze all FPC Date and Numbers formats consts
    with the localized format.

    The KDE configuration it's suported (Default config and User config)
    I have tested this unit with Linux Mandrake 10.0.
    
    For the Euro Monay symbol, I have overriten the result because the config
    file it's utf8 coded.
    
  History
   08/10/2004 OG - Create
------------------------------------------------------------------------------}
unit SysUtilsAdds;

{$mode objfpc}{$H+}

interface

{$IFDEF LINUX}
uses
  Classes, SysUtils
  {$IFDEF UseLibC}
  ,LibC
  {$ENDIF}
  ;
{$ENDIF}

implementation

{$IFDEF LINUX}
procedure InitInternationalFormats;
Var i      : Integer;
    St     : string;
    Lg     : String;
    LstKde : TStringList;
    U8S    : UTF8String;

   Function ConvertLinuxDateFormatToFp(Fmt,Dflt : String):string;
   begin
     try
       {$IFDEF UseLibC}
       Fmt:=StringReplace(Fmt,'%x',nl_langinfo(D_FMT),[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%X',nl_langinfo(T_FMT),[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%r',nl_langinfo(T_FMT_AMPM),[rfReplaceAll]);
       {$ENDIF}

       Fmt:=StringReplace(Fmt,'%a','ddd',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%A','dddd',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%b','mmm',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%B','mmmm',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%c','c',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%d','dd',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%D','mm/dd/yy',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%e','d',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%F','yyyy-mm-dd',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%g','yy',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%G','yyyy',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%h','mm',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%H','hh',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%h','hh',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%k','h',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%l','h',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%m','mm',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%M','nn',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%p','ampm',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%R','hh:nn',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%S','ss',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%t',#9,[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%T','hh:nn:ss',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%y','yy',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%Y','YYYY',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%%','%',[rfReplaceAll]);

       //No correspond values
       Fmt:=StringReplace(Fmt,'%n','',[rfReplaceAll]); //New line char
       Fmt:=StringReplace(Fmt,'%C','',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%E','',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%j','',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%o','',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%s','',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%u','',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%U','',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%V','',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%W','',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%w','',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%+','',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%z','',[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%Z','',[rfReplaceAll]);

     finally
       if Trim(fmt)<>'' then
          Result:=Fmt
       else
         Result:=Dflt;
     end;
   end;

   procedure ReadKdeFileConfig(aFileName,aSection : String;  alst : TStringList);
   Var f      : Text;
       Stc,S  : String;
       Flag   : Boolean;
   begin
     if FileExists(aFileName) then
     begin
       AssignFile(f,aFileName);
       Reset(f);
       try
         Flag:=False;
         While not EOF(F) do
         begin
           ReadLn(f,Stc);

           if ((Trim(Stc)='') or (Pos('[',Stc)=1)) and Flag then
             Break;

           if AnsiSameText(Stc,aSection) then
             Flag:=True;

           if Flag then
           begin
             S:=UpperCase(Trim(Copy(Stc,1,Pos('=',Stc)-1)));     //Name
             System.Delete(Stc,1,Pos('=',Stc)); //Value

             //if code is Euro symbol (UTF8)
             if Stc=#226#130#172 then
               Stc:='¤';

             aLst.Values[S]:=Stc; //if name exists, the value is override
           end;
         end;
       finally
         CloseFile(F);
       end;
     end;
   end;

   function ReadKdeConfig(Ident,defaultValue : string; DateFmt : Boolean=False):string;
   begin
     Result:=LstKde.Values[UpperCase(Ident)];
     if Result='' then
       Result:=LstKde.Values[UpperCase(Format('%s[%s]',[Ident,Lg]))];


     if (Result<>'') and DateFmt then
       Result:=ConvertLinuxDateFormatToFp(Result,'');

     if Result='' then
       Result:=DefaultValue;
   end;

begin

  {$IFDEF UseLibC}
  //Months
  for i:=1 to 12 do
  begin
    ShortMonthNames[i]:=nl_langinfo((ABMON_1-1)+i);
    LongMonthNames[i]:=nl_langinfo((MON_1-1)+i);
  end;

  //Days
  for i:=1 to 7 do
  begin
    ShortDayNames[i]:=nl_langinfo((ABDAY_1-1)+i);
    LongDayNames[i]:=nl_langinfo((DAY_1-1)+i);
  end;

  //PM/AM
  TimeAMString:=nl_langinfo(AM_STR);
  TimePMString:=nl_langinfo(PM_STR);

  //Date and time format
  ShortDateFormat:=ConvertLinuxDateFormatToFp(nl_langinfo(D_FMT),ShortDateFormat);
  LongDateFormat :=ConvertLinuxDateFormatToFp(nl_langinfo(D_T_FMT),LongDateFormat);
  ShortTimeFormat:=ConvertLinuxDateFormatToFp(nl_langinfo(T_FMT),ShortTimeFormat);
  LongTimeFormat :=ConvertLinuxDateFormatToFp(nl_langinfo(T_FMT_AMPM),LongTimeFormat);

  CurrencyString:=nl_langinfo(__CURRENCY_SYMBOL);
  St:=nl_langinfo(THOUSEP);
  if Length(St)>0 then
    ThousandSeparator:=St[1];
  St:=nl_langinfo(DECIMAL_POINT);
  if Length(St)>0 then
    DecimalSeparator:=St[1];

  //Date séparator
  for i:=1 to Length(ShortDateFormat) do
  begin
    if ShortDateFormat[i] in ['-','.','/'] then
    begin
      DateSeparator:=ShortDateFormat[i];
      Break;
    end;
  end;
  ShortDateFormat:=StringReplace(ShortDateFormat,DateSeparator,'/',[rfReplaceAll]);
  {$ENDIF}

  //KDE config
  if DirectoryExists(ExpandFileName('~/.kde/share/config')) then
  begin

    Lg:=Copy(GetEnvironmentVariable('LANG'),1,2); //Langue
    LstKde:=TStringList.Create;
    try
      ReadKdeFileConfig(Format('/usr/share/locale/l10n/%s/entry.desktop',[Lg]),'[KCM Locale]',LstKde);
      ReadKdeFileConfig(ExpandFileName('~/.kde/share/config/kdeglobals'),'[Locale]',LstKde);

      St:=ReadKdeConfig('DecimalSymbol',DecimalSeparator);
      DecimalSeparator:=St[1];
      CurrencyString:=ReadKdeConfig('CurrencySymbol',CurrencyString);
      ThousandSeparator:=ReadKdeConfig('ThousandsSeparator',ThousandSeparator)[1];
      LongTimeFormat:=ReadKdeConfig('TimeFormat',LongTimeFormat,True);
      LongDateFormat:=ReadKdeConfig('DateFormat',LongDateFormat,True);
      ShortDateFormat:=ReadKdeConfig('DateFormatShort',ShortDateFormat,True);
      ShortTimeFormat:=ReadKdeConfig('TimeFormatShort',ShortTimeFormat,True);

      //Date séparator
      for i:=1 to Length(ShortDateFormat) do
      begin
        if ShortDateFormat[i] in ['-','.','/'] then
        begin
          DateSeparator:=ShortDateFormat[i];
          Break;
        end;
      end;
      ShortDateFormat:=StringReplace(ShortDateFormat,DateSeparator,'/',[rfReplaceAll]);

    finally
      LstKde.Free;
    end;
  end;
end;


INITIALIZATION
  InitInternationalFormats;
{$ENDIF}
end.

