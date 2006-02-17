unit DefaultTranslator;
{ Copyright (C) 2004 V.I.Volchenko and Lazarus Developers Team

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{This unit is needed for using translated form strings made by Lazarus IDE.
It seeks for localized .mo file in some common places. If you want to find
.mo file anywhere else, don't use this unit but initialize LRSMoFile variable
from LResources in your project by yourself. If you need standard translation,
just use this unit in your project.
String translation works quite stable, but noone can guarantee some by-effects,
so all these stuff needs definition TRANSLATESTRING. Anyway, it is incompartible
with old fpc versions (1.0.x, 1.9.x up to 1.9.5 except of late December 2004
vers). Current FPC contains all hooks required, so use fpc-1.9.6+, then rebuild
lazarus with -dTRANSLATESTRING. If don't, this unit (and any such translation)
will translate resourcestrings only.
Another reason for including this unit may be using localized LCL stuff. This
unit localizes LCL too, if it finds lcl.xxx.mo at that place, where main
localization found.
}
{$mode objfpc}{$H+}

{$IF defined(VER2_0_2) and defined(win32)}
// FPC <= 2.0.2 compatibility code
// WINDOWS define was added after FPC 2.0.2
  {$define WINDOWS}
{$endif}

interface

uses
  Classes, SysUtils, LResources, GetText, Controls, typinfo
  {$IFDEF WINDOWS},Windows{$ENDIF};
{$IFDEF TRANSLATESTRING}
type
 TDefaultTranslator=class(TAbstractTranslator)
 private
  FMOFile:TMOFile;
 public
  constructor Create(MOFileName:string);
  destructor Destroy;override;
  procedure TranslateStringProperty(Sender:TObject; const Instance: TPersistent; PropInfo: PPropInfo; var Content:string);override;
 end;
{$ENDIF}
implementation
uses Menus;

function FindLocaleFileName:string;
var LANG,lng:string;
  i: Integer;
  {$IFDEF WINDOWS}
   Buffer:array[1..4]of char;
  {$ENDIF}
begin
 //Win32 user may decide to override locale with LANG variable.
 LANG:=SysUtils.GetEnvironmentVariable('LANG');
 if LANG='' then begin
   for i:=1 to Paramcount-1 do
    if (paramstr(i)='--LANG') or
     (paramstr(i)='-l') or
     (paramstr(i)='--lang') then LANG:=ParamStr(i+1);
 end;
 {$IFDEF WINDOWS}
 //Modified code from lazconf.inc
 if LANG='' then
 begin
  if GetLocaleInfo(GetUserDefaultLCID, LOCALE_SABBREVLANGNAME, @Buffer, 4)<>0 then
    Lng := lowercase(copy(Buffer,1,2));
  if GetLocaleInfo(GetUserDefaultLCID, LOCALE_SABBREVCTRYNAME, @Buffer, 4)<>0 then
    LANG := Lng+'_'+copy(Buffer,1,2);
 end;
 {$ENDIF}

 if LANG<>'' then begin
  //paramstr(0) is said not to work properly in linux, but I've tested it
  Result:=ExtractFilePath(paramstr(0))+DirectorySeparator+LANG+
    DirectorySeparator+ChangeFileExt(ExtractFileName(paramstr(0)),'.mo');
  if FileExists(Result) then exit;

  Result:=ExtractFilePath(paramstr(0))+DirectorySeparator+'languages'+DirectorySeparator+LANG+
    DirectorySeparator+ChangeFileExt(ExtractFileName(paramstr(0)),'.mo');
  if FileExists(Result) then exit;

  Result:=ExtractFilePath(paramstr(0))+DirectorySeparator+'locale'+DirectorySeparator
    +LANG+DirectorySeparator+ChangeFileExt(ExtractFileName(paramstr(0)),'.mo');
  if FileExists(Result) then exit;

  Result:=ExtractFilePath(paramstr(0))+DirectorySeparator+'locale'+DirectorySeparator
    +LANG+DirectorySeparator+'LC_MESSAGES'+DirectorySeparator+
    ChangeFileExt(ExtractFileName(paramstr(0)),'.mo');
  if FileExists(Result) then exit;

  {$IFDEF UNIX}
  //In unix-like systems we can try to search for global locale
  Result:='/usr/share/locale/'+LANG+'/LC_MESSAGES/'
   +ChangeFileExt(ExtractFileName(paramstr(0)),'.mo');
  if FileExists(Result) then exit;
  {$ENDIF}
  //Let us search for reducted files
  lng:=copy(LANG,1,2);
  //At first, check all was checked
  Result:=ExtractFilePath(paramstr(0))+DirectorySeparator+lng+
    DirectorySeparator+ChangeFileExt(ExtractFileName(paramstr(0)),'.mo');
  if FileExists(Result) then exit;

  Result:=ExtractFilePath(paramstr(0))+DirectorySeparator+'languages'+DirectorySeparator+lng+
    DirectorySeparator+ChangeFileExt(ExtractFileName(paramstr(0)),'.mo');
  if FileExists(Result) then exit;

  Result:=ExtractFilePath(paramstr(0))+DirectorySeparator+'locale'+DirectorySeparator
    +lng+DirectorySeparator+ChangeFileExt(ExtractFileName(paramstr(0)),'.mo');
  if FileExists(Result) then exit;

  Result:=ExtractFilePath(paramstr(0))+DirectorySeparator+'locale'+DirectorySeparator
    +LANG+DirectorySeparator+'LC_MESSAGES'+DirectorySeparator+
    ChangeFileExt(ExtractFileName(paramstr(0)),'.mo');
  if FileExists(Result) then exit;

  //Full language in file name - this will be default for the project
  //We need more carefull handling, as it MAY result in incorrect filename
  try
    Result:=ExtractFilePath(paramstr(0))+ChangeFileExt(ExtractFileName(paramstr(0)),'.'+LANG)+'.mo';
    if FileExists(Result) then exit;
   //Common location (like in Lazarus)
    Result:=ExtractFilePath(paramstr(0))+DirectorySeparator+'locale'+DirectorySeparator+ChangeFileExt(ExtractFileName(paramstr(0)),'.'+LANG)+'.mo';
    if FileExists(Result) then exit;

    Result:=ExtractFilePath(paramstr(0))+DirectorySeparator+'languages'+DirectorySeparator+ChangeFileExt(ExtractFileName(paramstr(0)),'.'+LANG)+'.mo';
    if FileExists(Result) then exit;
  except
    Result:='';//Or do something else (useless)
  end;
  {$IFDEF UNIX}
  Result:='/usr/share/locale/'+lng+'/LC_MESSAGES/'
   +ChangeFileExt(ExtractFileName(paramstr(0)),'.mo');
  if FileExists(Result) then exit;
  {$ENDIF}
  Result:=ExtractFilePath(paramstr(0))+ChangeFileExt(ExtractFileName(paramstr(0)),'.'+lng)+'.mo';
  if FileExists(Result) then exit;

  Result:=ExtractFilePath(paramstr(0))+DirectorySeparator+'locale'+DirectorySeparator+ChangeFileExt(ExtractFileName(paramstr(0)),'.'+lng)+'.mo';
  if FileExists(Result) then exit;

  Result:=ExtractFilePath(paramstr(0))+DirectorySeparator+'languages'+DirectorySeparator+ChangeFileExt(ExtractFileName(paramstr(0)),'.'+lng)+'.mo';
  if FileExists(Result) then exit;
 end;
 Result:=ChangeFileExt(paramstr(0),'.mo');
 if FileExists(Result) then exit;

 Result:='';
end;
var lcfn:string;
{$IFNDEF TRANSLATESTRING}
{$WARNING TranslateString is not enabled. Nothing to translate}
{$ELSE}

{ TDefaultTranslator }

constructor TDefaultTranslator.Create(MOFileName: string);
begin
  inherited Create;
  FMOFile:=TMOFile.Create(MOFileName);
end;

destructor TDefaultTranslator.Destroy;
begin
  FMOFile.Free;
//If someone will use this class incorrectly, it can be destroyed
//before Reader destroying. It is a very bad thing, but in THIS situation
//in this case is impossible. May be, in future we can overcome this difficulty
  inherited Destroy;
end;

procedure TDefaultTranslator.TranslateStringProperty(Sender: TObject;
  const Instance: TPersistent; PropInfo: PPropInfo; var Content: string);
var
  s: String;
  s1: String;
begin
  if not Assigned(FMOFile) then exit;
  if not Assigned(PropInfo) then exit;
{DO we really need this?}
  if Instance is TComponent then
   if csDesigning in (Instance as TComponent).ComponentState then exit;
{End DO :)}
  if (AnsiUpperCase(PropInfo^.PropType^.Name)<>'TTRANSLATESTRING')and
   not (Instance is TMenuItem)
   then exit;
  s:=AnsiUpperCase(Instance.ClassName+'.'+PropInfo^.Name)+'=';
  s1:=s+Content;
  s1:=FMOFile.Translate(s1);
  if (copy(s1,1,length(s))=s)and(s1<>s+Content) then
  begin
    Content:=copy(s1,length(s)+1,length(s1)-length(s));
    exit;
  end;
  s:=AnsiUpperCase(PropInfo^.Name)+'=';
  s1:=s+Content;
  s1:=FMOFile.Translate(s1);
  if (copy(s1,1,length(s))=s)and(s1<>s+Content) then
  begin
    Content:=copy(s1,length(s)+1,length(s1)-length(s));
    exit;
  end;
  s1:=FMOFile.Translate(Content);
  if s1<>'' then Content:=s1;
  //TODO:another types of translation
end;
{$ENDIF}
var Dot1:integer;
    LCLPath:string;
initialization
//It is safe to place code here as no form is initialized before unit
//initialization made
//We are to search for all
  try
    lcfn:=FindLocaleFileName;
  except
    lcfn:='';
  end;

  if lcfn<>'' then
  begin
    TranslateResourceStrings(lcfn);
    LCLPath:=ExtractFileName(lcfn);
    Dot1:=pos('.',LCLPath);
    if Dot1>1 then
    begin
      Delete(LCLPath,1,Dot1-1);
      LCLPath:=ExtractFilePath(lcfn)+'lcl'+LCLPath;
      if FileExists(LCLPath) then
        TranslateResourceStrings(LCLPath);
    end;
    {$IFDEF TRANSLATESTRING}
    LRSTranslator:=TDefaultTranslator.Create(lcfn);
    {$ENDIF}
  end;
finalization
end.
{
No revision (not in LOG)
 2004/10/09 Sent as is to Lazarus Team - VVI
}

