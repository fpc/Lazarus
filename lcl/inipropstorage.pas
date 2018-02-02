{  $Id$  }
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit IniPropStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  // LazUtils
  LazUtf8,
  // LCL
  Forms;

type
  { TCustomIniPropStorage }

  TIniFileClass = class of TCustomIniFile;
  
  TCustomIniPropStorage = class(TFormPropertyStorage)
  private
    FCount : Integer;
    FReadOnly : Boolean;
    FIniFile: TCustomIniFile;
    FIniFileName: string;
    FIniSection: string;
  protected
    function IniFileClass: TIniFileClass; virtual;
    function GetIniFileName: string; virtual;
    function RootSection: string; override;
    property IniFile: TCustomIniFile read FIniFile;
  public
    procedure StorageNeeded(ReadOnly: Boolean); override;
    procedure FreeStorage; override;
    function  DoReadString(const Section, Ident, default: string): string; override;
    procedure DoWriteString(const Section, Ident, Value: string); override;
    procedure DoEraseSections(const ARootSection : string);override;
  public
    property IniFileName: string read FIniFileName write FIniFileName;
    property IniSection: string read FIniSection write FIniSection;
  end;
  
  { TIniPropStorage }
  
  TIniPropStorage = class(TCustomIniPropStorage)
  published
    Property StoredValues;
    property IniFileName;
    property IniSection;
    property Active;
    property OnSavingProperties;
    property OnSaveProperties;
    property OnRestoringProperties;
    property OnRestoreProperties;
  end;


procedure Register;


implementation


procedure Register;
begin
  RegisterComponents('Misc',[TIniPropStorage]);
end;

{ TCustomIniPropStorage }

function TCustomIniPropStorage.IniFileClass: TIniFileClass;
begin
  Result:=TIniFile;
end;

procedure TCustomIniPropStorage.StorageNeeded(ReadOnly: Boolean);
begin
  If (FIniFile=Nil) or (ReadOnly<>FReadOnly) then
    begin
    If (FiniFile<>Nil) then
      begin
      // Force free.
      FCount:=0;
      FreeStorage;
      end;
    FReadOnly:=ReadOnly;
    if not (csDesigning in ComponentState) then
      FInifile:=IniFileClass.Create(GetIniFileName{$IF FPC_FULLVERSION>=30101}, TEncoding.UTF8{$ENDIF});
    end;
  Inc(FCount);
end;

procedure TCustomIniPropStorage.FreeStorage;
begin
  Dec(FCount);
  If FCount<=0 then
    begin
    FCount:=0;
    FreeAndNil(FIniFile);
    end;
end;

function TCustomIniPropStorage.GetIniFileName: string;
begin
  If (FIniFileName<>'') then
    Result:=FIniFileName
  else if csDesigning in ComponentState then
    raise Exception.Create('TCustomIniPropStorage.GetIniFileName: missing Filename')
  else
{$ifdef unix}
    Result:=IncludeTrailingPathDelimiter(GetEnvironmentVariableUTF8('HOME'))
            +'.'+ExtractFileName(Application.ExeName);

{$else}
    Result:=ChangeFileExt(Application.ExeName,'.ini');
{$endif}
end;

function TCustomIniPropStorage.RootSection: String;
begin
  if (FIniSection='') then
    Result:=inherited RootSection
  else
    Result:=FIniSection;
end;

function TCustomIniPropStorage.DoReadString(const Section, Ident, Default: string): string;
begin
  Result:=FIniFile.ReadString(Section, Ident, Default);
end;

procedure TCustomIniPropStorage.DoWriteString(const Section, Ident, Value: string);
begin
  FIniFile.WriteString(Section, Ident, Value);
end;

procedure TCustomIniPropStorage.DoEraseSections(const ARootSection: String);

var
  Lines: TStrings;
  I: Integer;
begin
  Lines := TStringList.Create;
  try
    FInifile.ReadSections(Lines);
    for I := 0 to Lines.Count - 1 do begin
      if SameText(Lines[I],ARootSection) or
         SameText(Copy(Lines[i],1,Length(ARootSection)+1), ARootSection+'.') then
        FInifile.EraseSection(Lines[I]);
    end;
  finally
    Lines.Free;
  end;
end;

end.
