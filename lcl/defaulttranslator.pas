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
just use this unit in your project and enable i18n in project options.

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
  Classes, SysUtils, LResources, GetText, Controls, typinfo, FileUtil
  {$IFDEF WINDOWS},Windows{$ENDIF};

type
 TDefaultTranslator=class(TAbstractTranslator)
 private
  FMOFile:TMOFile;
 public
  constructor Create(MOFileName:string);
  destructor Destroy;override;
  procedure TranslateStringProperty(Sender:TObject; const Instance: TPersistent; PropInfo: PPropInfo; var Content:string);override;
 end;

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
 LANG:=GetEnvironmentVariableUTF8('LANG');
 if LANG='' then begin
   for i:=1 to Paramcount-1 do
    if (ParamStrUTF8(i)='--LANG') or
     (ParamStrUTF8(i)='-l') or
     (ParamStrUTF8(i)='--lang') then LANG:=ParamStrUTF8(i+1);
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
  //ParamStrUTF8(0) is said not to work properly in linux, but I've tested it
  Result:=ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+LANG+
    DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
  if FileExistsUTF8(Result) then exit;

  Result:=ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+'languages'+DirectorySeparator+LANG+
    DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
  if FileExistsUTF8(Result) then exit;

  Result:=ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+'locale'+DirectorySeparator
    +LANG+DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
  if FileExistsUTF8(Result) then exit;

  Result:=ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+'locale'+DirectorySeparator
    +LANG+DirectorySeparator+'LC_MESSAGES'+DirectorySeparator+
    ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
  if FileExistsUTF8(Result) then exit;

  {$IFDEF UNIX}
  //In unix-like systems we can try to search for global locale
  Result:='/usr/share/locale/'+LANG+'/LC_MESSAGES/'
   +ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
  if FileExistsUTF8(Result) then exit;
  {$ENDIF}
  //Let us search for reducted files
  lng:=copy(LANG,1,2);
  //At first, check all was checked
  Result:=ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+lng+
    DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
  if FileExistsUTF8(Result) then exit;

  Result:=ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+'languages'+DirectorySeparator+lng+
    DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
  if FileExistsUTF8(Result) then exit;

  Result:=ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+'locale'+DirectorySeparator
    +lng+DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
  if FileExistsUTF8(Result) then exit;

  Result:=ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+'locale'+DirectorySeparator
    +LANG+DirectorySeparator+'LC_MESSAGES'+DirectorySeparator+
    ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
  if FileExistsUTF8(Result) then exit;

  //Full language in file name - this will be default for the project
  //We need more carefull handling, as it MAY result in incorrect filename
  try
    Result:=ExtractFilePath(ParamStrUTF8(0))+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.'+LANG)+'.mo';
    if FileExistsUTF8(Result) then exit;
   //Common location (like in Lazarus)
    Result:=ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+'locale'+DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.'+LANG)+'.mo';
    if FileExistsUTF8(Result) then exit;

    Result:=ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+'languages'+DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.'+LANG)+'.mo';
    if FileExistsUTF8(Result) then exit;
  except
    Result:='';//Or do something else (useless)
  end;
  {$IFDEF UNIX}
  Result:='/usr/share/locale/'+lng+'/LC_MESSAGES/'
   +ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
  if FileExistsUTF8(Result) then exit;
  {$ENDIF}
  Result:=ExtractFilePath(ParamStrUTF8(0))+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.'+lng)+'.mo';
  if FileExistsUTF8(Result) then exit;

  Result:=ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+'locale'+DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.'+lng)+'.mo';
  if FileExistsUTF8(Result) then exit;

  Result:=ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+'languages'+DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.'+lng)+'.mo';
  if FileExistsUTF8(Result) then exit;
 end;
 Result:=ChangeFileExt(ParamStrUTF8(0),'.mo');
 if FileExistsUTF8(Result) then exit;

 Result:='';
end;
var lcfn:string;

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
begin
  if not Assigned(FMOFile) then exit;
  if not Assigned(PropInfo) then exit;
{DO we really need this?}
  if Instance is TComponent then
   if csDesigning in (Instance as TComponent).ComponentState then exit;
{End DO :)}
  if (AnsiUpperCase(PropInfo^.PropType^.Name)<>'TTRANSLATESTRING') then exit;
  s:=FMOFile.Translate(Content);
  if s<>'' then Content:=s;
end;

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
      if FileExistsUTF8(LCLPath) then
        TranslateResourceStrings(LCLPath);
    end;

    LRSTranslator:=TDefaultTranslator.Create(lcfn);

  end;
finalization
end.
{
No revision (not in LOG)
 2004/10/09 Sent as is to Lazarus Team - VVI
}

