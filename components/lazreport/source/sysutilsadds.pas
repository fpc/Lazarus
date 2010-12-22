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

{$IFDEF UNIX}
  {$IF (FPC_VERSION > 2) or
       ((FPC_VERSION = 2)
         and ((FPC_RELEASE > 2) or
              ((FPC_RELEASE = 2) and (FPC_PATCH >= 1))))}
    {$DEFINE USECLOCALE}
  {$ENDIF}
{$ENDIF}

{$IFDEF USECLOCALE}
uses
  Classes, SysUtils, FileUtil,
  {$info if compiler can't find clocale unit, you probably are using old 2.2.1 or 2.3.1 version, please upgrade}
  CLocale;
{$ENDIF}

implementation

// only mess around config files if CLocale unit was not used
// trying to do anything desktop specific is prone to fail
// if conditions change on new versions.
{$IFNDEF USECLOCALE}
procedure InitInternationalFormats;
Var i      : Integer;
    St     : string;
    Lg     : String;
    LstKde : TStringList;
    DFMT   : string;
    TFMT   : string;
    TFMTAP : string;

   Function ConvertLinuxDateFormatToFp(Fmt,Dflt : String):string;
   begin
     try
       Fmt:=StringReplace(Fmt,'%x',DFMT,[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%X',TFMT,[rfReplaceAll]);
       Fmt:=StringReplace(Fmt,'%r',TFMTAP,[rfReplaceAll]);
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
     if FileExistsUTF8(aFileName) then
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

  //GNOME: GNOME_DESKTOP_SESSION_ID
  //KDE config
  if (GetEnvironmentVariable('KDE_FULL_SESSION')<>'') and
     (DirectoryExistsUTF8(ExpandFileNameUTF8('~/.kde/share/config'))) then
  begin

    DFMT := ShortDateFormat; //nl_langinfo(D_FMT)
    TFMT := ShortTimeFormat; //nl_langinfo(T_FMT)
    TFMTAP := LongTimeFormat; //nl_langinfo(T_FMT_AMPM)

    Lg:=Copy(GetEnvironmentVariableUTF8('LANG'),1,2); //Langue
    LstKde:=TStringList.Create;
    try
      ReadKdeFileConfig(Format('/usr/share/locale/l10n/%s/entry.desktop',[Lg]),'[KCM Locale]',LstKde);
      ReadKdeFileConfig(ExpandFileNameUTF8('~/.kde/share/config/kdeglobals'),'[Locale]',LstKde);

      St:=ReadKdeConfig('DecimalSymbol',DecimalSeparator);
      DecimalSeparator:=St[1];
      CurrencyString:=ReadKdeConfig('CurrencySymbol',CurrencyString);
      ThousandSeparator:=ReadKdeConfig('ThousandsSeparator',ThousandSeparator)[1];
      LongTimeFormat:=ReadKdeConfig('TimeFormat',LongTimeFormat,True);
      LongDateFormat:=ReadKdeConfig('DateFormat',LongDateFormat,True);
      ShortDateFormat:=ReadKdeConfig('DateFormatShort',ShortDateFormat,True);
      ShortTimeFormat:=ReadKdeConfig('TimeFormatShort',ShortTimeFormat,True);

      //Date separator
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

