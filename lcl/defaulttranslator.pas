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
  Classes, SysUtils, LResources, GetText, Controls, typinfo, FileUtil, LCLProc
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
var
  Lang, T:string;
  i: Integer;

  function GetLocaleFileName(const LangID: String): String;
  var
    LangShortID: String;
  begin
    if LangID<>'' then
    begin
      //ParamStrUTF8(0) is said not to work properly in linux, but I've tested it
      Result:=ExtractFilePath(ParamStrUTF8(0))+LangID+
        DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
      if FileExistsUTF8(Result) then exit;

      Result:=ExtractFilePath(ParamStrUTF8(0))+'languages'+DirectorySeparator+LangID+
        DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
      if FileExistsUTF8(Result) then exit;

      Result:=ExtractFilePath(ParamStrUTF8(0))+'locale'+DirectorySeparator
        +LangID+DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
      if FileExistsUTF8(Result) then exit;

      Result:=ExtractFilePath(ParamStrUTF8(0))+'locale'+DirectorySeparator
        +LangID+DirectorySeparator+'LC_MESSAGES'+DirectorySeparator+
        ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
      if FileExistsUTF8(Result) then exit;

      {$IFDEF UNIX}
      //In unix-like systems we can try to search for global locale
      Result:='/usr/share/locale/'+LangID+'/LC_MESSAGES/'
       +ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
      if FileExistsUTF8(Result) then exit;
      {$ENDIF}
      //Let us search for reducted files
      LangShortID:=copy(LangID,1,2);
      //At first, check all was checked
      Result:=ExtractFilePath(ParamStrUTF8(0))+LangShortID+
        DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
      if FileExistsUTF8(Result) then exit;

      Result:=ExtractFilePath(ParamStrUTF8(0))+'languages'+DirectorySeparator+LangShortID+
        DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
      if FileExistsUTF8(Result) then exit;

      Result:=ExtractFilePath(ParamStrUTF8(0))+'locale'+DirectorySeparator
        +LangShortID+DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
      if FileExistsUTF8(Result) then exit;

      Result:=ExtractFilePath(ParamStrUTF8(0))+'locale'+DirectorySeparator
        +LangID+DirectorySeparator+'LC_MESSAGES'+DirectorySeparator+
        ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
      if FileExistsUTF8(Result) then exit;

      //Full language in file name - this will be default for the project
      //We need more carefull handling, as it MAY result in incorrect filename
      try
        Result:=ExtractFilePath(ParamStrUTF8(0))+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.'+LangID)+'.mo';
        if FileExistsUTF8(Result) then exit;
       //Common location (like in Lazarus)
        Result:=ExtractFilePath(ParamStrUTF8(0))+'locale'+DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.'+LangID)+'.mo';
        if FileExistsUTF8(Result) then exit;

        Result:=ExtractFilePath(ParamStrUTF8(0))+'languages'+DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.'+LangID)+'.mo';
        if FileExistsUTF8(Result) then exit;
      except
        Result:='';//Or do something else (useless)
      end;
      {$IFDEF UNIX}
      Result:='/usr/share/locale/'+LangShortID+'/LC_MESSAGES/'
       +ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.mo');
      if FileExistsUTF8(Result) then exit;
      {$ENDIF}
      Result:=ExtractFilePath(ParamStrUTF8(0))+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.'+LangShortID)+'.mo';
      if FileExistsUTF8(Result) then exit;

      Result:=ExtractFilePath(ParamStrUTF8(0))+'locale'+DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.'+LangShortID)+'.mo';
      if FileExistsUTF8(Result) then exit;

      Result:=ExtractFilePath(ParamStrUTF8(0))+'languages'+DirectorySeparator+ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'.'+LangShortID)+'.mo';
      if FileExistsUTF8(Result) then exit;
    end;

    Result := '';
  end;

begin
 Result := '';
 //Win32 user may decide to override locale with LANG variable.
 Lang:=GetEnvironmentVariableUTF8('LANG');
 if Lang='' then begin
   for i:=1 to Paramcount-1 do
    if (ParamStrUTF8(i)='--LANG') or
     (ParamStrUTF8(i)='-l') or
     (ParamStrUTF8(i)='--lang') then Lang:=ParamStrUTF8(i+1);
 end;
 if Lang='' then
   LCLGetLanguageIDs(Lang, T);

 Result := GetLocaleFileName(Lang);
 if Result <> '' then Exit;

 Result:=ChangeFileExt(ParamStrUTF8(0),'.mo');
 if FileExistsUTF8(Result) then exit;

 Result:='';
end;
var lcfn:string;

{ TDefaultTranslator }

constructor TDefaultTranslator.Create(MOFileName: string);
begin
  inherited Create;
  FMOFile:=TMOFile.Create(UTF8ToSys(MOFileName));
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
  Section: String;
  Component: TComponent;
begin
  if not Assigned(FMOFile) then exit;
  if not Assigned(PropInfo) then exit;
  // do not translate at design time
  if Instance is TComponent then
    if csDesigning in (Instance as TComponent).ComponentState then exit;

  if (UpperCase(PropInfo^.PropType^.Name)<>'TTRANSLATESTRING') then exit;
  s:=FMOFile.Translate(Content);
  if s = '' then
  begin
    Component := Instance as TComponent;
    if Component.Owner<>nil then
      Section := UpperCase(Component.Owner.Name) + '.';
    Section := 'T' + Section + UpperCase(Component.Name) + '.' +
      UpperCase(PropInfo^.Name);
    s := FMoFile.Translate(Section + #4 + Content);
  end;
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
    DebugLn(lcfn);
  except
    lcfn:='';
  end;

  if lcfn<>'' then
  begin
    TranslateResourceStrings(UTF8ToSys(lcfn));
    LCLPath:=ExtractFileName(lcfn);
    Dot1:=pos('.',LCLPath);
    if Dot1>1 then
    begin
      Delete(LCLPath,1,Dot1-1);
      LCLPath:=ExtractFilePath(lcfn)+'lcl'+LCLPath;
      if FileExistsUTF8(LCLPath) then
        TranslateResourceStrings(UTF8ToSys(LCLPath));
    end;

    LRSTranslator:=TDefaultTranslator.Create(lcfn);

  end;
finalization
  LRSTranslator.Free;
end.
{
No revision (not in LOG)
 2004/10/09 Sent as is to Lazarus Team - VVI
}

