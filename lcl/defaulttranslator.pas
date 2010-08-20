unit DefaultTranslator;

{ Copyright (C) 2004-2010 V.I.Volchenko and Lazarus Developers Team

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
It searches for translated .po/.mo files in some common places. If you need
to have .po/.mo files anywhere else, don't use this unit but initialize
LRSMoFile variable from LResources in your project by yourself.
If you need standard translation, just use this unit in your project and enable
i18n in project options.

Another reason for including this unit may be using translated LCL messages.
This unit localizes LCL too, if it finds lclstrconsts.xx.po/lclstrconsts.xx.mo
in directory where your program translation files are placed.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, GetText, Controls, typinfo, FileUtil, LCLProc,
  Translations;

type
  TDefaultTranslator = class(TAbstractTranslator)
  private
    FMOFile: TMOFile;
  public
    constructor Create(MOFileName: string);
    destructor Destroy; override;
    procedure TranslateStringProperty(Sender: TObject; const Instance: TPersistent;
      PropInfo: PPropInfo; var Content: string); override;
  end;

  TPOTranslator = class(TAbstractTranslator)
  private
    FPOFile: TPOFile;
  public
    constructor Create(POFileName: string);
    destructor Destroy; override;
    procedure TranslateStringProperty(Sender: TObject; const Instance: TPersistent;
      PropInfo: PPropInfo; var Content: string); override;
  end;

implementation

uses
  Menus;

type
  TPersistentAccess = class(TPersistent);

function FindLocaleFileName(LCExt: string): string;
var
  Lang, T: string;
  i: integer;

  function GetLocaleFileName(const LangID, LCExt: string): string;
  var
    LangShortID: string;
  begin
    if LangID <> '' then
    begin
      //ParamStrUTF8(0) is said not to work properly in linux, but I've tested it
      Result := ExtractFilePath(ParamStrUTF8(0)) + LangID +
        DirectorySeparator + ChangeFileExt(ExtractFileName(ParamStrUTF8(0)), LCExt);
      if FileExistsUTF8(Result) then
        exit;

      Result := ExtractFilePath(ParamStrUTF8(0)) + 'languages' + DirectorySeparator + LangID +
        DirectorySeparator + ChangeFileExt(ExtractFileName(ParamStrUTF8(0)), LCExt);
      if FileExistsUTF8(Result) then
        exit;

      Result := ExtractFilePath(ParamStrUTF8(0)) + 'locale' + DirectorySeparator
        + LangID + DirectorySeparator + ChangeFileExt(ExtractFileName(ParamStrUTF8(0)), LCExt);
      if FileExistsUTF8(Result) then
        exit;

      Result := ExtractFilePath(ParamStrUTF8(0)) + 'locale' + DirectorySeparator
        + LangID + DirectorySeparator + 'LC_MESSAGES' + DirectorySeparator +
        ChangeFileExt(ExtractFileName(ParamStrUTF8(0)), LCExt);
      if FileExistsUTF8(Result) then
        exit;

      {$IFDEF UNIX}
      //In unix-like systems we can try to search for global locale
      Result := '/usr/share/locale/' + LangID + '/LC_MESSAGES/' +
        ChangeFileExt(ExtractFileName(ParamStrUTF8(0)), LCExt);
      if FileExistsUTF8(Result) then
        exit;
      {$ENDIF}
      //Let us search for reducted files
      LangShortID := copy(LangID, 1, 2);
      //At first, check all was checked
      Result := ExtractFilePath(ParamStrUTF8(0)) + LangShortID +
        DirectorySeparator + ChangeFileExt(ExtractFileName(ParamStrUTF8(0)), LCExt);
      if FileExistsUTF8(Result) then
        exit;

      Result := ExtractFilePath(ParamStrUTF8(0)) + 'languages' + DirectorySeparator +
        LangShortID + DirectorySeparator + ChangeFileExt(
        ExtractFileName(ParamStrUTF8(0)), LCExt);
      if FileExistsUTF8(Result) then
        exit;

      Result := ExtractFilePath(ParamStrUTF8(0)) + 'locale' + DirectorySeparator
        + LangShortID + DirectorySeparator + ChangeFileExt(
        ExtractFileName(ParamStrUTF8(0)), LCExt);
      if FileExistsUTF8(Result) then
        exit;

      Result := ExtractFilePath(ParamStrUTF8(0)) + 'locale' + DirectorySeparator
        + LangShortID + DirectorySeparator + 'LC_MESSAGES' + DirectorySeparator +
        ChangeFileExt(ExtractFileName(ParamStrUTF8(0)), LCExt);
      if FileExistsUTF8(Result) then
        exit;

      //Full language in file name - this will be default for the project
      //We need more careful handling, as it MAY result in incorrect filename
      try
        Result := ExtractFilePath(ParamStrUTF8(0)) + ChangeFileExt(ExtractFileName(ParamStrUTF8(0)), '.' + LangID) + LCExt;
        if FileExistsUTF8(Result) then
          exit;
        //Common location (like in Lazarus)
        Result := ExtractFilePath(ParamStrUTF8(0)) + 'locale' + DirectorySeparator +
          ChangeFileExt(ExtractFileName(ParamStrUTF8(0)), '.' + LangID) + LCExt;
        if FileExistsUTF8(Result) then
          exit;

        Result := ExtractFilePath(ParamStrUTF8(0)) + 'languages' +
          DirectorySeparator + ChangeFileExt(ExtractFileName(ParamStrUTF8(0)), '.' + LangID) + LCExt;
        if FileExistsUTF8(Result) then
          exit;
      except
        Result := '';//Or do something else (useless)
      end;
      {$IFDEF UNIX}
      Result := '/usr/share/locale/' + LangShortID + '/LC_MESSAGES/' +
        ChangeFileExt(ExtractFileName(ParamStrUTF8(0)), LCExt);
      if FileExistsUTF8(Result) then
        exit;
      {$ENDIF}
      Result := ExtractFilePath(ParamStrUTF8(0)) + ChangeFileExt(
        ExtractFileName(ParamStrUTF8(0)), '.' + LangShortID) + LCExt;
      if FileExistsUTF8(Result) then
        exit;

      Result := ExtractFilePath(ParamStrUTF8(0)) + 'locale' + DirectorySeparator +
        ChangeFileExt(ExtractFileName(ParamStrUTF8(0)), '.' + LangShortID) + LCExt;
      if FileExistsUTF8(Result) then
        exit;

      Result := ExtractFilePath(ParamStrUTF8(0)) + 'languages' + DirectorySeparator +
        ChangeFileExt(ExtractFileName(ParamStrUTF8(0)), '.' + LangShortID) + LCExt;
      if FileExistsUTF8(Result) then
        exit;
    end;

    Result := '';
  end;

begin
  Result := '';
  Lang := '';

  for i := 1 to Paramcount - 1 do
    if (ParamStrUTF8(i) = '--LANG') or (ParamStrUTF8(i) = '-l') or
      (ParamStrUTF8(i) = '--lang') then
      Lang := ParamStrUTF8(i + 1);

  //Win32 user may decide to override locale with LANG variable.
  if Lang = '' then
    Lang := GetEnvironmentVariableUTF8('LANG');

  if Lang = '' then
    LCLGetLanguageIDs(Lang, T);

  Result := GetLocaleFileName(Lang, LCExt);
  if Result <> '' then
    exit;

  Result := ChangeFileExt(ParamStrUTF8(0), LCExt);
  if FileExistsUTF8(Result) then
    exit;

  Result := '';
end;

var
  lcfn: string;

{ TDefaultTranslator }

constructor TDefaultTranslator.Create(MOFileName: string);
begin
  inherited Create;
  FMOFile := TMOFile.Create(UTF8ToSys(MOFileName));
end;

destructor TDefaultTranslator.Destroy;
begin
  FMOFile.Free;
  //If someone will use this class incorrectly, it can be destroyed
  //before Reader destroying. It is a very bad thing, but in THIS situation
  //in this case is impossible. Maybe, in future we can overcome this difficulty
  inherited Destroy;
end;

procedure TDefaultTranslator.TranslateStringProperty(Sender: TObject;
  const Instance: TPersistent; PropInfo: PPropInfo; var Content: string);
var
  s: string;
  Section: string;
  Tmp: TPersistent;
  Component: TComponent;
begin
  if not Assigned(FMOFile) then
    exit;
  if not Assigned(PropInfo) then
    exit;
  if (UpperCase(PropInfo^.PropType^.Name) <> 'TTRANSLATESTRING') then
    exit;
  // do not translate at design time
  // get the component
  Tmp := Instance;
  while Assigned(Tmp) and not (Tmp is TComponent) do
    Tmp := TPersistentAccess(Tmp).GetOwner;
  if not Assigned(Tmp) then
    exit;
  Component := Tmp as TComponent;
  if (csDesigning in Component.ComponentState) then
    exit;

  if not (Sender is TReader) then
    exit;
  if Component = TReader(Sender).Root then
    Section := Component.ClassName
  else
    if Component.Owner = TReader(Sender).Root then
      Section := Component.Owner.ClassName
    else
      exit;
  Section := UpperCase(Section + '.' + Instance.GetNamePath + '.' + PropInfo^.Name);
  s := FMoFile.Translate(Section + #4 + Content);

  if s = '' then
    s := FMOFile.Translate(Content);

  if s <> '' then
    Content := s;
end;

{ TPOTranslator }

constructor TPOTranslator.Create(POFileName: string);
begin
  inherited Create;
  FPOFile := TPOFile.Create(UTF8ToSys(POFileName));
end;

destructor TPOTranslator.Destroy;
begin
  FPOFile.Free;
  //If someone will use this class incorrectly, it can be destroyed
  //before Reader destroying. It is a very bad thing, but in THIS situation
  //in this case is impossible. May be, in future we can overcome this difficulty
  inherited Destroy;
end;

procedure TPOTranslator.TranslateStringProperty(Sender: TObject;
  const Instance: TPersistent; PropInfo: PPropInfo; var Content: string);
var
  s: string;
  Section: string;
  Tmp: TPersistent;
  Component: TComponent;
begin
  if not Assigned(FPOFile) then
    exit;
  if not Assigned(PropInfo) then
    exit;
  if (UpperCase(PropInfo^.PropType^.Name) <> 'TTRANSLATESTRING') then
    exit;
  // do not translate at design time
  // get the component
  Tmp := Instance;
  while Assigned(Tmp) and not (Tmp is TComponent) do
    Tmp := TPersistentAccess(Tmp).GetOwner;
  if not Assigned(Tmp) then
    exit;
  Component := Tmp as TComponent;
  if (csDesigning in Component.ComponentState) then
    exit;

  if not (Sender is TReader) then
    exit;
  if Component = TReader(Sender).Root then
    Section := Component.ClassName
    else
      if Component.Owner = TReader(Sender).Root then
        Section := Component.Owner.ClassName
      else
        exit;
  Section := UpperCase(Section + '.' + Instance.GetNamePath + '.' + PropInfo^.Name);
  s := FPOFile.Translate(Section, Content);

  if s <> '' then
    Content := s;
end;

var
  Dot1: integer;
  LCLPath: string;
  LocalTranslator: TAbstractTranslator;

initialization
  //It is safe to place code here as no form is initialized before unit
  //initialization made

  LocalTranslator := nil;
  // search first po translation resources
  try
     lcfn := FindLocaleFileName('.po');
     if lcfn <> '' then
     begin
       Translations.TranslateResourceStrings(lcfn);
       LCLPath := ExtractFileName(lcfn);
       Dot1 := pos('.', LCLPath);
       if Dot1 > 1 then
       begin
         Delete(LCLPath, 1, Dot1 - 1);
         LCLPath := ExtractFilePath(lcfn) + 'lclstrconsts' + LCLPath;
         Translations.TranslateUnitResourceStrings('LCLStrConsts', LCLPath);
       end;
       LocalTranslator := TPOTranslator.Create(lcfn);
     end;
   except
     lcfn := '';
   end;

  if lcfn='' then
  begin
    // try now with MO traslation resources
    try
      lcfn := FindLocaleFileName('.mo');
      if lcfn <> '' then
      begin
        GetText.TranslateResourceStrings(UTF8ToSys(lcfn));
        LCLPath := ExtractFileName(lcfn);
        Dot1 := pos('.', LCLPath);
        if Dot1 > 1 then
        begin
          Delete(LCLPath, 1, Dot1 - 1);
          LCLPath := ExtractFilePath(lcfn) + 'lclstrconsts' + LCLPath;
          if FileExistsUTF8(LCLPath) then
            GetText.TranslateResourceStrings(UTF8ToSys(LCLPath));
        end;
        LocalTranslator := TDefaultTranslator.Create(lcfn);
      end;
    except
      lcfn := '';
    end;
  end;

  if LocalTranslator<>nil then
    LRSTranslator := LocalTranslator;

finalization
  LocalTranslator.Free;

end.
