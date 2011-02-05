{
 /***************************************************************************
                        w32versioninfo.pas  -  Lazarus IDE unit
                        ---------------------------------------
                   TVersionInfo is responsible for the inclusion of the
                   version information in windows executables.


                   Initial Revision  : Sun Feb 20 12:00:00 CST 2006


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit W32VersionInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, LCLProc, Controls, Forms, FileUtil,
  CodeToolManager, LazConf, Laz_XMLCfg,
  {$IFDEF NewXMLCfg}
  laz2_DOM,
  {$ELSE}
  Laz_DOM,
  {$ENDIF}
  IDEProcs, ProjectIntf,
  ProjectResourcesIntf, resource, versionresource, versiontypes;

type

  { TProjectVersionStringTable }

  TProjectVersionStringTable = class(TVersionStringTable)
  private
    FOnModified: TNotifyEvent;
    function GetValues(Key: string): string;
    procedure SetValues(Key: string; const AValue: string);
  protected
    procedure DoModified;
    function KeyToIndex(const aKey: String): Integer;
  public
    constructor Create(const aName: string); reintroduce;
    procedure Add(const aKey, aValue: string); reintroduce;
    procedure AddDefault;
    function Equals(aTable: TProjectVersionStringTable): boolean; reintroduce;
    procedure Assign(aTable: TProjectVersionStringTable); virtual;
    procedure AddRequired;
    procedure Clear; reintroduce;
    procedure Delete(const aIndex: integer); overload; reintroduce;
    procedure Delete(const aKey: string); overload; reintroduce;
    function IsRequired(const aKey: string): Boolean;
    property Values[Key: string]: string read GetValues write SetValues; default;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
  end;

  { TProjectVersionInfo }

  TProjectVersionInfo = class(TAbstractProjectResource)
  private
    FAutoIncrementBuild: boolean;
    FHexCharSet: string;
    FHexLang: string;
    FStringTable: TProjectVersionStringTable;
    FUseVersionInfo: boolean;
    FVersion: TFileProductVersion;
    function GetCharSets: TStringList;
    function GetHexCharSets: TStringList;
    function GetHexLanguages: TStringList;
    function GetLanguages: TStringList;
    function GetVersion(AIndex: integer): integer;
    procedure SetAutoIncrementBuild(const AValue: boolean);
    procedure SetHexCharSet(const AValue: string);
    procedure SetHexLang(const AValue: string);
    procedure SetUseVersionInfo(const AValue: boolean);
    procedure SetVersion(AIndex: integer; const AValue: integer);
    function ExtractProductVersion: TFileProductVersion;
    function BuildFileVersionString: String;
    procedure DoModified(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure DoAfterBuild(AResources: TAbstractProjectResources;
      AReason: TCompileReason; SaveToTestDir: boolean); override;
    function UpdateResources(AResources: TAbstractProjectResources;
      const MainFilename: string): boolean; override;
    procedure WriteToProjectFile(AConfig: {TXMLConfig}TObject; Path: string); override;
    procedure ReadFromProjectFile(AConfig: {TXMLConfig}TObject; Path: string); override;

    property UseVersionInfo: boolean read FUseVersionInfo write SetUseVersionInfo;
    property AutoIncrementBuild: boolean read FAutoIncrementBuild write SetAutoIncrementBuild;

    property MajorVersionNr: integer index 0 read GetVersion write SetVersion;
    property MinorVersionNr: integer index 1 read GetVersion write SetVersion;
    property RevisionNr: integer index 2 read GetVersion write SetVersion;
    property BuildNr: integer index 3 read GetVersion write SetVersion;

    property HexLang: string read FHexLang write SetHexLang;
    property HexCharSet: string read FHexCharSet write SetHexCharSet;

    // string info
    property StringTable: TProjectVersionStringTable read FStringTable;
  end;

function MSLanguageToHex(const s: string): string;
function MSHexToLanguage(const s: string): string;
function MSCharacterSetToHex(const s: string): string;
function MSHexToCharacterSet(const s: string): string;

function MSLanguages: TStringList;
function MSHexLanguages: TStringList;
function MSCharacterSets: TStringList;
function MSHexCharacterSets: TStringList;

const
  DefaultLanguage = '0409'; // U.S. English
  DefaultCharSet = '04E4'; // Multilingual

implementation

var
  // languages
  fLanguages: TStringList = nil;
  fHexLanguages: TStringList = nil;

  // character sets
  fCharSets: TStringList = nil;
  fHexCharSets: TStringList = nil;

procedure CreateCharSets;
begin
  if fCharSets <> nil then
    exit;
  fCharSets := TStringList.Create;
  fHexCharSets := TStringList.Create;

  fCharSets.Add('7-bit ASCII');
  fHexCharSets.Add('0000');
  fCharSets.Add('Japan (Shift - JIS X-0208)');
  fHexCharSets.Add('03A4');
  fCharSets.Add('Korea (Shift - KSC 5601)');
  fHexCharSets.Add('03B5');
  fCharSets.Add('Taiwan (Big5)');
  fHexCharSets.Add('03B6');
  fCharSets.Add('Unicode');
  fHexCharSets.Add('04B0');
  fCharSets.Add('Latin-2 (Eastern European)');
  fHexCharSets.Add('04E2');
  fCharSets.Add('Cyrillic');
  fHexCharSets.Add('04E3');
  fCharSets.Add('Multilingual');
  fHexCharSets.Add('04E4');
  fCharSets.Add('Greek');
  fHexCharSets.Add('04E5');
  fCharSets.Add('Turkish');
  fHexCharSets.Add('04E6');
  fCharSets.Add('Hebrew');
  fHexCharSets.Add('04E7');
  fCharSets.Add('Arabic');
  fHexCharSets.Add('04E8');
end;

procedure CreateLanguages;
begin
  if fLanguages <> nil then
    exit;
  fLanguages := TStringList.Create;
  fHexLanguages := TStringList.Create;
  fLanguages.Add('Arabic');
  fHexLanguages.Add('0401');
  fLanguages.Add('Bulgarian');
  fHexLanguages.Add('0402');
  fLanguages.Add('Catalan');
  fHexLanguages.Add('0403');
  fLanguages.Add('Traditional Chinese');
  fHexLanguages.Add('0404');
  fLanguages.Add('Czech');
  fHexLanguages.Add('0405');
  fLanguages.Add('Danish');
  fHexLanguages.Add('0406');
  fLanguages.Add('German');
  fHexLanguages.Add('0407');
  fLanguages.Add('Greek');
  fHexLanguages.Add('0408');
  fLanguages.Add('U.S. English');
  fHexLanguages.Add('0409');
  fLanguages.Add('Castillian Spanish');
  fHexLanguages.Add('040A');
  fLanguages.Add('Finnish');
  fHexLanguages.Add('040B');
  fLanguages.Add('French');
  fHexLanguages.Add('040C');
  fLanguages.Add('Hebrew');
  fHexLanguages.Add('040D');
  fLanguages.Add('Hungarian');
  fHexLanguages.Add('040E');
  fLanguages.Add('Icelandic');
  fHexLanguages.Add('040F');
  fLanguages.Add('Italian');
  fHexLanguages.Add('0410');
  fLanguages.Add('Japanese');
  fHexLanguages.Add('0411');
  fLanguages.Add('Korean');
  fHexLanguages.Add('0412');
  fLanguages.Add('Dutch');
  fHexLanguages.Add('0413');
  fLanguages.Add('Norwegian - Bokmal');
  fHexLanguages.Add('0414');
  fLanguages.Add('Swiss Italian');
  fHexLanguages.Add('0810');
  fLanguages.Add('Belgian Dutch');
  fHexLanguages.Add('0813');
  fLanguages.Add('Norwegian - Nynorsk');
  fHexLanguages.Add('0814');
  fLanguages.Add('Polish');
  fHexLanguages.Add('0415');
  fLanguages.Add('Portugese (Brazil)');
  fHexLanguages.Add('0416');
  fLanguages.Add('Rhaeto-Romantic');
  fHexLanguages.Add('0417');
  fLanguages.Add('Romanian');
  fHexLanguages.Add('0418');
  fLanguages.Add('Russian');
  fHexLanguages.Add('0419');
  fLanguages.Add('Croato-Serbian (Latin)');
  fHexLanguages.Add('041A');
  fLanguages.Add('Slovak');
  fHexLanguages.Add('041B');
  fLanguages.Add('Albanian');
  fHexLanguages.Add('041C');
  fLanguages.Add('Swedish');
  fHexLanguages.Add('041D');
  fLanguages.Add('Thai');
  fHexLanguages.Add('041E');
  fLanguages.Add('Turkish');
  fHexLanguages.Add('041F');
  fLanguages.Add('Urdu');
  fHexLanguages.Add('0420');
  fLanguages.Add('Bahasa');
  fHexLanguages.Add('0421');
  fLanguages.Add('Simplified Chinese');
  fHexLanguages.Add('0804');
  fLanguages.Add('Swiss German');
  fHexLanguages.Add('0807');
  fLanguages.Add('U.K. English');
  fHexLanguages.Add('0809');
  fLanguages.Add('Mexican Spanish');
  fHexLanguages.Add('080A');
  fLanguages.Add('Belgian French');
  fHexLanguages.Add('080C');
  fLanguages.Add('Canadian French');
  fHexLanguages.Add('0C0C');
  fLanguages.Add('Swiss French');
  fHexLanguages.Add('100C');
  fLanguages.Add('Portugese (Portugal)');
  fHexLanguages.Add('0816');
  fLanguages.Add('Sebro-Croatian (Cyrillic)');
  fHexLanguages.Add('081A');
end;

function MSLanguageToHex(const s: string): string;
var
  i: longint;
begin
  i := MSLanguages.IndexOf(s);
  if i >= 0 then
    Result := fHexLanguages[i]
  else
    Result := '';
end;

function MSHexToLanguage(const s: string): string;
var
  i: longint;
begin
  i := MSHexLanguages.IndexOf(s);
  if i >= 0 then
    Result := fLanguages[i]
  else
    Result := '';
end;

function MSCharacterSetToHex(const s: string): string;
var
  i: longint;
begin
  i := MSCharacterSets.IndexOf(s);
  if i >= 0 then
    Result := fHexCharSets[i]
  else
    Result := '';
end;

function MSHexToCharacterSet(const s: string): string;
var
  i: longint;
begin
  i := MSHexCharacterSets.IndexOf(s);
  if i >= 0 then
    Result := fCharSets[i]
  else
    Result := '';
end;

function MSLanguages: TStringList;
begin
  CreateLanguages;
  Result := fLanguages;
end;

function MSHexLanguages: TStringList;
begin
  CreateLanguages;
  Result := fHexLanguages;
end;

function MSCharacterSets: TStringList;
begin
  CreateCharSets;
  Result := fCharSets;
end;

function MSHexCharacterSets: TStringList;
begin
  CreateCharSets;
  Result := fHexCharSets;
end;

{ VersionInfo }

function TProjectVersionInfo.UpdateResources(AResources: TAbstractProjectResources;
  const MainFilename: string): boolean;
var
  ARes: TVersionResource;
  st: TVersionStringTable;
  ti: TVerTranslationInfo;
  lang: string;
  charset: string;
  i: integer;
begin
  Result := True;
  if UseVersionInfo then
  begin
    // project indicates to use the versioninfo
    ARes := TVersionResource.Create(nil, nil);
    //it's always RT_VERSION and 1 respectively
    ARes.FixedInfo.FileVersion := FVersion;
    ARes.FixedInfo.ProductVersion := ExtractProductVersion;

    lang := HexLang;
    if lang = '' then
      lang := DefaultLanguage;
    charset := HexCharSet;
    if charset = '' then
      charset := DefaultCharSet;

    // set FileVersion from version numbers
    FStringTable['FileVersion'] := BuildFileVersionString;

    st := TVersionStringTable.Create(lang + charset);
    for i := 0 to FStringTable.Count - 1 do
      st.Add(Utf8ToAnsi(FStringTable.Keys[i]), Utf8ToAnsi(FStringTable.ValuesByIndex[i]));
    ARes.StringFileInfo.Add(st);

    ti.language := StrToInt('$' + lang);
    ti.codepage := StrToInt('$' + charset);
    ARes.VarFileInfo.Add(ti);
    AResources.AddSystemResource(ARes);
  end;
end;

procedure TProjectVersionInfo.WriteToProjectFile(AConfig: TObject; Path: string);
var
  i: integer;
  Key: string;
  DefaultValue: String;
begin
  with TXMLConfig(AConfig) do
  begin
    SetDeleteValue(Path + 'VersionInfo/UseVersionInfo/Value', UseVersionInfo, False);
    SetDeleteValue(Path + 'VersionInfo/AutoIncrementBuild/Value',
      AutoIncrementBuild, False);
    SetDeleteValue(Path + 'VersionInfo/MajorVersionNr/Value', MajorVersionNr, 0);
    SetDeleteValue(Path + 'VersionInfo/MinorVersionNr/Value', MinorVersionNr, 0);
    SetDeleteValue(Path + 'VersionInfo/RevisionNr/Value', RevisionNr, 0);
    SetDeleteValue(Path + 'VersionInfo/BuildNr/Value', BuildNr, 0);
    SetDeleteValue(Path + 'VersionInfo/Language/Value', HexLang, DefaultLanguage);
    SetDeleteValue(Path + 'VersionInfo/CharSet/Value', HexCharSet, DefaultCharset);

    // write string info
    DeletePath(Path + 'VersionInfo/StringTable');
    for i := 0 to StringTable.Count - 1 do begin
      Key:=StringTable.Keys[i];
      if Key='FileVersion' then continue; // FileVersion is created automatically
      DefaultValue:='';
      if (Key='ProductVersion') then
        DefaultValue:=BuildFileVersionString;
      SetDeleteValue(Path + 'VersionInfo/StringTable/' + StringTable.Keys[i],
        StringTable.ValuesByIndex[i],DefaultValue);
    end;
  end;
end;

procedure TProjectVersionInfo.ReadFromProjectFile(AConfig: TObject; Path: string);
var
  i: integer;
  Node: TDomNode;
begin
  with TXMLConfig(AConfig) do
  begin
    UseVersionInfo := GetValue(Path + 'VersionInfo/UseVersionInfo/Value', False);
    AutoIncrementBuild := GetValue(Path + 'VersionInfo/AutoIncrementBuild/Value', False);

    MajorVersionNr := GetValue(Path + 'VersionInfo/CurrentVersionNr/Value',
      GetValue(Path + 'VersionInfo/MajorVersionNr/Value', 0));
    MinorVersionNr := GetValue(Path + 'VersionInfo/CurrentMajorRevNr/Value',
      GetValue(Path + 'VersionInfo/MinorVersionNr/Value', 0));
    RevisionNr := GetValue(Path + 'VersionInfo/CurrentMinorRevNr/Value',
      GetValue(Path + 'VersionInfo/RevisionNr/Value', 0));
    BuildNr := GetValue(Path + 'VersionInfo/CurrentBuildNr/Value',
      GetValue(Path + 'VersionInfo/BuildNr/Value', 0));

    HexLang := GetValue(Path + 'VersionInfo/Language/Value', DefaultLanguage);
    HexCharSet := GetValue(Path + 'VersionInfo/CharSet/Value', DefaultCharset);

    // read string info
    Node := FindNode(Path + 'VersionInfo/StringTable', False);
    if Assigned(Node) then
    begin
      StringTable.Clear;
      for i := 0 to Node.Attributes.{$IFDEF NewXMLCfg}Length{$ELSE}Count{$ENDIF} - 1 do
        StringTable[Node.Attributes[i].NodeName] := Node.Attributes[i].NodeValue;
      StringTable.AddRequired;
    end
    else
    begin
      // read old info
      StringTable['Comments'] := GetValue(Path + 'VersionInfo/Comments/Value', '');
      StringTable['CompanyName'] := GetValue(Path + 'VersionInfo/CompanyName/Value', '');
      StringTable['FileDescription'] := GetValue(Path + 'VersionInfo/FileDescription/Value', '');
      StringTable['FileVersion'] := BuildFileVersionString;
      StringTable['InternalName'] := GetValue(Path + 'VersionInfo/InternalName/Value', '');
      StringTable['LegalCopyright'] := GetValue(Path + 'VersionInfo/LegalCopyright/Value', '');
      StringTable['LegalTrademarks'] := GetValue(Path + 'VersionInfo/LegalTrademarks/Value', '');
      StringTable['OriginalFilename'] := GetValue(Path + 'VersionInfo/OriginalFilename/Value', '');
      StringTable['ProductName'] := GetValue(Path + 'VersionInfo/ProductName/Value', '');
      StringTable['ProductVersion'] := GetValue(Path + 'VersionInfo/ProjectVersion/Value', BuildFileVersionString);
    end;
  end;
end;

function TProjectVersionInfo.GetCharSets: TStringList;
begin
  CreateCharSets;
  Result := fHexCharSets;
end;

function TProjectVersionInfo.GetHexCharSets: TStringList;
begin
  CreateCharSets;
  Result := fHexCharSets;
end;

function TProjectVersionInfo.GetHexLanguages: TStringList;
begin
  CreateLanguages;
  Result := fHexLanguages;
end;

function TProjectVersionInfo.GetLanguages: TStringList;
begin
  CreateLanguages;
  Result := fLanguages;
end;

function TProjectVersionInfo.GetVersion(AIndex: integer): integer;
begin
  Result := FVersion[AIndex];
end;

procedure TProjectVersionInfo.SetAutoIncrementBuild(const AValue: boolean);
begin
  if FAutoIncrementBuild = AValue then
    exit;
  FAutoIncrementBuild := AValue;
  Modified := True;
end;

procedure TProjectVersionInfo.SetHexCharSet(const AValue: string);
begin
  if FHexCharSet = AValue then
    exit;
  FHexCharSet := AValue;
  Modified := True;
end;

procedure TProjectVersionInfo.SetHexLang(const AValue: string);
begin
  if FHexLang = AValue then
    exit;
  FHexLang := AValue;
  Modified := True;
end;

procedure TProjectVersionInfo.SetUseVersionInfo(const AValue: boolean);
begin
  if FUseVersionInfo = AValue then
    exit;
  FUseVersionInfo := AValue;
  Modified := True;
end;

procedure TProjectVersionInfo.SetVersion(AIndex: integer; const AValue: integer);
begin
  if FVersion[AIndex] = AValue then
    Exit;
  FVersion[AIndex] := AValue;
  Modified := True;
end;

function TProjectVersionInfo.ExtractProductVersion: TFileProductVersion;
var
  S, Part: string;
  i, p: integer;
begin
  S := StringTable['ProductVersion'];
  for i := 0 to 3 do
  begin
    p := Pos('.', S);
    if p >= 1 then
    begin
      Part := Copy(S, 1, p - 1);
      Delete(S, 1, P);
    end
    else
    begin
      Part := S;
      S := '';
    end;
    Result[i] := StrToIntDef(Part, 0);
  end;
end;

function TProjectVersionInfo.BuildFileVersionString: String;
begin
  Result := Format('%d.%d.%d.%d', [MajorVersionNr, MinorVersionNr, RevisionNr, BuildNr]);
end;

procedure TProjectVersionInfo.DoModified(Sender: TObject);
begin
  Modified := True;
end;

constructor TProjectVersionInfo.Create;
begin
  inherited Create;
  FStringTable := TProjectVersionStringTable.Create('00000000');
  FStringTable.OnModified := @DoModified;
  HexLang:=DefaultLanguage;
  HexCharSet:=DefaultCharSet;
end;

destructor TProjectVersionInfo.Destroy;
begin
  FStringTable.Free;
  inherited Destroy;
end;

procedure TProjectVersionInfo.DoAfterBuild(AResources: TAbstractProjectResources;
  AReason: TCompileReason; SaveToTestDir: boolean);
begin
  if (AReason = crBuild) and AutoIncrementBuild then // project indicate to use autoincrementbuild
    BuildNr := BuildNr + 1;
end;

{ TProjectVersionStringTable }

function TProjectVersionStringTable.GetValues(Key: string): string;
var
  idx: Integer;
begin
  idx := KeyToIndex(Key);
  if idx = -1 then
    Result := ''
  else
    Result := ValuesByIndex[idx];
end;

procedure TProjectVersionStringTable.SetValues(Key: string; const AValue: string);
var
  idx: Integer;
begin
  idx := KeyToIndex(Key);
  if idx = -1 then
    Add(Key, AValue)
  else
    ValuesByIndex[idx] := AValue;
  DoModified;
end;

procedure TProjectVersionStringTable.AddDefault;
begin
  Add('Comments', '');
  Add('CompanyName', '');
  Add('FileDescription', '');
  Add('FileVersion', '');
  Add('InternalName', '');
  Add('LegalCopyright', '');
  Add('LegalTrademarks', '');
  Add('OriginalFilename', '');
  // - PrivateBuild
  Add('ProductName', '');
  Add('ProductVersion', '');
  // - SpecialBuild
end;

function TProjectVersionStringTable.Equals(aTable: TProjectVersionStringTable
  ): boolean;
var
  i: Integer;
begin
  Result:=true;
  for i:=0 to Count-1 do begin
    if aTable.Values[Keys[i]]<>ValuesByIndex[i] then begin
      debugln(['TProjectVersionStringTable.Equals differ Key=',Keys[i],' my=',ValuesByIndex[i],' other=',aTable.Values[Keys[i]]]);
      Result:=false;
    end;
  end;
  for i:=0 to aTable.Count-1 do begin
    if Values[aTable.Keys[i]]<>aTable.ValuesByIndex[i] then begin
      debugln(['TProjectVersionStringTable.Equals differ Key=',aTable.Keys[i],' my=',Values[aTable.Keys[i]],' other=',aTable.ValuesByIndex[i]]);
      Result:=false;
    end;
  end;
end;

procedure TProjectVersionStringTable.Assign(aTable: TProjectVersionStringTable);
var
  i: Integer;
begin
  if Equals(aTable) then exit;
  Clear;
  for i := 0 to aTable.Count - 1 do
    inherited Add(aTable.Keys[i],aTable.ValuesByIndex[i]);
  DoModified;
end;

function TProjectVersionStringTable.KeyToIndex(const aKey: String): Integer;
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    if Keys[i] = aKey then
      exit(i);
  Result := -1;
end;

procedure TProjectVersionStringTable.DoModified;
begin
  if Assigned(OnModified) then
    OnModified(Self);
end;

constructor TProjectVersionStringTable.Create(const aName: string);
begin
  inherited Create(aName);
  AddDefault;
end;

procedure TProjectVersionStringTable.Add(const aKey, aValue: string);
begin
  inherited Add(aKey, aValue);
  DoModified;
end;

procedure TProjectVersionStringTable.AddRequired;
const
  RequiredFields: array[0..6] of String = (
    'CompanyName',
    'FileDescription',
    'FileVersion',
    'InternalName',
    'OriginalFilename',
    'ProductName',
    'ProductVersion'
  );
var
  i: Integer;
begin
  for i := Low(RequiredFields) to High(RequiredFields) do
    if KeyToIndex(RequiredFields[i]) = -1 then
      Add(RequiredFields[i], '');
end;

procedure TProjectVersionStringTable.Clear;
begin
  if Count > 0 then
  begin
    inherited Clear;
    DoModified;
  end;
end;

procedure TProjectVersionStringTable.Delete(const aIndex: integer);
begin
  if not IsRequired(Keys[aIndex]) then
  begin
    inherited Delete(aIndex);
    DoModified;
  end;
end;

procedure TProjectVersionStringTable.Delete(const aKey: string);
begin
  if not IsRequired(aKey) then
  begin
    inherited Delete(aKey);
    DoModified;
  end;
end;

function TProjectVersionStringTable.IsRequired(const aKey: string): Boolean;
begin
  Result :=
    (aKey = 'CompanyName') or
    (aKey = 'FileDescription') or
    (aKey = 'FileVersion') or
    (aKey = 'InternalName') or
    (aKey = 'OriginalFilename') or
    (aKey = 'ProductName') or
    (aKey = 'ProductVersion');
end;

initialization
  RegisterProjectResource(TProjectVersionInfo);

finalization
  FreeAndNil(fHexCharSets);
  FreeAndNil(fHexLanguages);
  FreeAndNil(fLanguages);
  FreeAndNil(fCharSets);

end.

