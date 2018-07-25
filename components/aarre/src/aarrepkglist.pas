{ Read/Write Aarre packagelist.gz files

  Copyright (C) 2012 Mattias Gaertner  mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit AarrePkgList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zstream, Laz2_XMLCfg, LazUTF8, LConvEncoding,
  LazFileUtils, LazStringUtils, LazUtilities;

type
  TAPackageType = (
    aptRunTime,         // RunTime packages can't register anything in the IDE.
                        // They can be used by designtime packages.
    aptDesignTime,      // DesignTime packages can register anything in the IDE
                        // and should not be compiled into projects.
                        // The IDE calls the 'register' procedures of each unit.
    aptRunAndDesignTime,// RunAndDesignTime packages can do anything.
    aptRunTimeOnly      // as lptRunTime, but they can not be used in the IDE
    );
const
  DefaultPackageType = aptRunTime;
  AarrePkgListFileVersion = 1;
type

  { TAPkgVersion }

  TAPkgVersion = class
  private
    FBuild: integer;
    FMajor: integer;
    FMinor: integer;
    FRelease: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(XML: TXMLConfig; Path: string); virtual;
    procedure Save(XML: TXMLConfig; Path: string); virtual;
    function AsString: string;
    property Major: integer read FMajor write FMajor;
    property Minor: integer read FMinor write FMinor;
    property Release: integer read FRelease write FRelease;
    property Build: integer read FBuild write FBuild;
  end;

  { TAPkgDependency }

  TAPkgDependency = class
  private
    FDefaultFilename: string;
    FMaxVersion: TAPkgVersion;
    FMaxVersionValid: boolean;
    FMinVersion: TAPkgVersion;
    FMinVersionValid: boolean;
    FName: string;
    FPreferDefaultFilename: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(XML: TXMLConfig; Path: string); virtual;
    procedure Save(XML: TXMLConfig; Path: string); virtual;
    property Name: string read FName write FName;
    property MinVersion: TAPkgVersion read FMinVersion;
    property MinVersionValid: boolean read FMinVersionValid write FMinVersionValid;
    property MaxVersion: TAPkgVersion read FMaxVersion;
    property MaxVersionValid: boolean read FMaxVersionValid write FMaxVersionValid;
    property DefaultFilename: string read FDefaultFilename write FDefaultFilename;
    property PreferDefaultFilename: boolean read FPreferDefaultFilename write FPreferDefaultFilename;
  end;

  { TAPkgDependencies }

  TAPkgDependencies = class
  private
    FItems: TFPList;
    function GetItems(Index: integer): TAPkgDependency;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(XML: TXMLConfig; Path: string); virtual;
    procedure Save(XML: TXMLConfig; Path: string); virtual;
    procedure Add(Item: TAPkgDependency);
    procedure Insert(Index: integer; Item: TAPkgDependency);
    procedure Delete(Index: integer);
    function Count: integer;
    property Items[Index: integer]: TAPkgDependency read GetItems;
  end;

  { TAarrePkgListItem }

  TAarrePkgListItem = class
  private
    FAuthor: String;
    FDescription: String;
    FIncPath: string;
    FLicense: String;
    FLPKFilename: string;
    FName: string;
    FPackageType: TAPackageType;
    FDependencies: TAPkgDependencies;
    FSourcesPath: string;
    FUnitPath: string;
    FVersion: TAPkgVersion;
    procedure SetLPKFilename(AValue: string);
    procedure SetName(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(XML: TXMLConfig; Path: string); virtual;
    procedure Save(XML: TXMLConfig; Path: string); virtual;
    procedure LoadLPK(LPKFilename: string);
    procedure UpdateSourcesPath;

    // properties saved to packages.gz
    property Name: string read FName write SetName;
    property Version: TAPkgVersion read FVersion;
    property PackageType: TAPackageType read FPackageType write FPackageType;
    property Author: String read FAuthor write FAuthor;
    property Description: String read FDescription write FDescription;
    property License: String read FLicense write FLicense;

    property LPKFilename: string read FLPKFilename write SetLPKFilename;
    property Dependencies: TAPkgDependencies read FDependencies;

    // properties not saved in packages.gz
    property UnitPath: string read FUnitPath write FUnitPath;
    property IncPath: string read FIncPath write FIncPath;
    property SourcesPath: string read FSourcesPath write FSourcesPath;
  end;

  { TAarrePkgList }

  TAarrePkgList = class
  private
    fItems: TFPList;
    function GetItems(Index: integer): TAarrePkgListItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(XML: TXMLConfig; Path: string); virtual;
    procedure Save(XML: TXMLConfig; Path: string); virtual;
    procedure SaveToStream(s: TStream);
    procedure LoadFromStream(s: TStream);
    procedure SaveToFile(const aFilename: string);
    function AsString: string;
    procedure Add(Item: TAarrePkgListItem);
    procedure Insert(Index: integer; Item: TAarrePkgListItem);
    procedure Remove(Item: TAarrePkgListItem);
    procedure Delete(Index: integer);
    function IndexOf(Item: TAarrePkgListItem): integer;
    function Count: integer;
    property Items[Index: integer]: TAarrePkgListItem read GetItems; default;
  end;

const
  APackageTypeIdents: array[TAPackageType] of string = (
    'RunTime', 'DesignTime', 'RunAndDesignTime', 'RunTimeOnly');

function APackageTypeIdentToType(const s: string): TAPackageType;
function FixUTF8(const s: string): string;

implementation

function APackageTypeIdentToType(const s: string): TAPackageType;
begin
  for Result:=Low(TAPackageType) to High(TAPackageType) do
    if SysUtils.CompareText(s,APackageTypeIdents[Result])=0 then exit;
  Result:=aptRunTime;
end;

function FixUTF8(const s: string): string;
var
  i: PtrInt;
begin
  Result:=s;
  if Result='' then exit;
  i:=FindInvalidUTF8Codepoint(PChar(Result),length(Result));
  if i<0 then exit;
  Result:=ISO_8859_1ToUTF8(Result);
end;

{ TAPkgDependencies }

function TAPkgDependencies.GetItems(Index: integer): TAPkgDependency;
begin
  Result:=TAPkgDependency(FItems[Index]);
end;

constructor TAPkgDependencies.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TAPkgDependencies.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TAPkgDependencies.Clear;
var
  i: Integer;
begin
  for i:=Count-1 downto 0 do
    Delete(i);
end;

procedure TAPkgDependencies.Load(XML: TXMLConfig; Path: string);
var
  NewCount: Integer;
  i: Integer;
  Dep: TAPkgDependency;
begin
  Clear;
  NewCount:=XML.GetValue(Path+'Count',0);
  for i:=0 to NewCount-1 do begin
    Dep:=TAPkgDependency.Create;
    Dep.Load(XML,Path+'Item'+IntToStr(i+1)+'/');
    if Dep.Name='' then
      Dep.Free
    else
      FItems.Add(Dep);
  end;
end;

procedure TAPkgDependencies.Save(XML: TXMLConfig; Path: string);
var
  i: Integer;
begin
  XML.SetDeleteValue(Path+'Count',Count,0);
  for i:=0 to Count-1 do
    Items[i].Save(XML,Path+'Item'+IntToStr(i+1)+'/');
end;

procedure TAPkgDependencies.Add(Item: TAPkgDependency);
begin
  Insert(Count,Item);
end;

procedure TAPkgDependencies.Insert(Index: integer; Item: TAPkgDependency);
begin
  FItems.Insert(Index,Item);
end;

procedure TAPkgDependencies.Delete(Index: integer);
begin
  TObject(FItems[Index]).Free;
  FItems.Delete(Index);
end;

function TAPkgDependencies.Count: integer;
begin
  Result:=FItems.Count;
end;

{ TAPkgVersion }

constructor TAPkgVersion.Create;
begin

end;

destructor TAPkgVersion.Destroy;
begin
  inherited Destroy;
end;

procedure TAPkgVersion.Load(XML: TXMLConfig; Path: string);
begin
  Major:=XML.GetValue(Path+'Major',0);
  Minor:=XML.GetValue(Path+'Minor',0);
  Release:=XML.GetValue(Path+'Release',0);
  Build:=XML.GetValue(Path+'Build',0);
end;

procedure TAPkgVersion.Save(XML: TXMLConfig; Path: string);
begin
  XML.SetDeleteValue(Path+'Major',Major,0);
  XML.SetDeleteValue(Path+'Minor',Minor,0);
  XML.SetDeleteValue(Path+'Release',Release,0);
  XML.SetDeleteValue(Path+'Build',Build,0);
end;

function TAPkgVersion.AsString: string;
begin
  Result:=IntToStr(Major)+'.'+IntToStr(Minor)+'.'+IntToStr(Release)+'.'+IntToStr(Build);
end;

{ TAarrePkgListItem }

procedure TAarrePkgListItem.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TAarrePkgListItem.SetLPKFilename(AValue: string);
begin
  if FLPKFilename=AValue then Exit;
  FLPKFilename:=AValue;
end;

constructor TAarrePkgListItem.Create;
begin
  FVersion:=TAPkgVersion.Create;
  FDependencies:=TAPkgDependencies.Create;
end;

destructor TAarrePkgListItem.Destroy;
begin
  Clear;
  FreeAndNil(FDependencies);
  FreeAndNil(FVersion);
  inherited Destroy;
end;

procedure TAarrePkgListItem.Clear;
begin
  FDependencies.Clear;
end;

procedure TAarrePkgListItem.Load(XML: TXMLConfig; Path: string);
begin
  Name:=XML.GetValue(Path+'Name','');
  Version.Load(XML,Path+'Version/');
  PackageType:=APackageTypeIdentToType(xml.GetValue(Path+'Type',
                                       APackageTypeIdents[DefaultPackageType]));
  Author:=XML.GetValue(Path+'Author','');
  Description:=XML.GetValue(Path+'Description/Value','');
  License:=XML.GetValue(Path+'License/Value','');
  LPKFilename:=XML.GetValue(Path+'LPKFilename/Value','');
  Dependencies.Load(XML,Path+'Dependencies/');
end;

procedure TAarrePkgListItem.Save(XML: TXMLConfig; Path: string);
begin
  XML.SetDeleteValue(Path+'Name',Name,'');
  Version.Save(XML,Path+'Version/');
  XML.SetDeleteValue(Path+'Type',APackageTypeIdents[PackageType],
                     APackageTypeIdents[DefaultPackageType]);
  XML.SetDeleteValue(Path+'Author',Author,'');
  XML.SetDeleteValue(Path+'Description/Value',Description,'');
  XML.SetDeleteValue(Path+'License/Value',License,'');
  XML.SetDeleteValue(Path+'LPKFilename/Value',LPKFilename,'');
  Dependencies.Save(XML,Path+'Dependencies/');
end;

procedure TAarrePkgListItem.LoadLPK(LPKFilename: string);
var
  xml: TXMLConfig;
  Path: String;
  FileVersion: Integer;
  i: Integer;
  NewCount: Integer;
  PkgDependency: TAPkgDependency;
begin
  xml:=TXMLConfig.Create(LPKFilename);
  try
    Path:='Package/';
    FileVersion:=xml.GetValue(Path+'Version',0);
    if FileVersion=0 then
      raise Exception.Create('no file version');
    Name:=xml.GetValue(Path+'Name/Value','');
    if not IsValidIdent(Name) then
      raise Exception.Create('invalid name ');
    PackageType:=APackageTypeIdentToType(xml.GetValue(Path+'Type/Value',
                                            APackageTypeIdents[DefaultPackageType]));
    Author:=FixUTF8(xml.GetValue(Path+'Author/Value',''));
    Description:=FixUTF8(xml.GetValue(Path+'Description/Value',''));
    License:=FixUTF8(xml.GetValue(Path+'License/Value',''));
    Version.Load(xml,Path+'Version/');

    NewCount:=xml.GetValue(Path+'RequiredPkgs/Count',0);
    Dependencies.Clear;
    for i:=0 to NewCount-1 do begin
      PkgDependency:=TAPkgDependency.Create;
      PkgDependency.Load(xml,Path+'RequiredPkgs/Item'+IntToStr(i+1)+'/');
      Dependencies.Add(PkgDependency);
    end;

    UnitPath:=xml.GetValue(Path+'CompilerOptions/SearchPaths/UnitPath/Value','');
    IncPath:=xml.GetValue(Path+'CompilerOptions/SearchPaths/IncludePath/Value','');
  finally
    xml.Free;
  end;
end;

procedure TAarrePkgListItem.UpdateSourcesPath;
var
  Path: String;
  p: Integer;
  CurPath: String;
  MacroPos: Integer;
  NewPath: String;
begin
  Path:=UnitPath+';'+IncPath;
  NewPath:='';
  p:=1;
  while p<=length(Path) do begin
    CurPath:=TrimFilename(GetForcedPathDelims(GetNextDelimitedItem(Path,';',p)));
    if CurPath='' then continue;
    MacroPos:=1;
    while (MacroPos<length(CurPath)) do begin
      if CurPath[MacroPos]='$' then begin
        if CurPath[MacroPos+1]='$' then
          // $$ not a macro
          inc(MacroPos,2)
        else begin
          // macro found, e.g. lib/prefix$(TargetOS)
          // => keep only path without macros, e.g. lib
          while (MacroPos>1) and (CurPath[MacroPos]<>PathDelim) do
            dec(MacroPos);
          Delete(CurPath,MacroPos,length(CurPath));
        end;
      end;
      inc(MacroPos);
    end;
    if FindPathInSearchPath(PChar(CurPath),length(CurPath),
      PChar(NewPath),length(NewPath))=nil
    then;
      NewPath+=';'+CurPath;
  end;
end;

{ TAarrePkgList }

function TAarrePkgList.GetItems(Index: integer): TAarrePkgListItem;
begin
  Result:=TAarrePkgListItem(fItems[Index]);
end;

constructor TAarrePkgList.Create;
begin
  fItems:=TFPList.Create;
end;

destructor TAarrePkgList.Destroy;
begin
  Clear;
  FreeAndNil(fItems);
  inherited Destroy;
end;

procedure TAarrePkgList.Clear;
var
  i: Integer;
begin
  for i:=Count-1 downto 0 do
    Delete(i);
end;

procedure TAarrePkgList.Load(XML: TXMLConfig; Path: string);
var
  {%H-}FileVersion: Integer;
  NewCount: Integer;
  i: Integer;
  Item: TAarrePkgListItem;
begin
  Clear;
  FileVersion:=XML.GetValue(Path+'Version',0);
  NewCount:=XML.GetValue(Path+'Items/Count',0);
  for i:=0 to NewCount-1 do begin
    Item:=TAarrePkgListItem.Create;
    Item.Load(XML,Path+'Items/Item'+IntToStr(i+1)+'/');
    if Item.Name='' then
      Item.Free
    else
      Add(Item);
  end;
end;

procedure TAarrePkgList.Save(XML: TXMLConfig; Path: string);
var
  i: Integer;
begin
  XML.SetValue(Path+'Version',AarrePkgListFileVersion);
  XML.SetDeleteValue(Path+'Items/Count',Count,0);
  for i:=0 to Count-1 do begin
    Items[i].Save(XML,Path+'Items/Item'+IntToStr(i+1)+'/');
  end;
end;

procedure TAarrePkgList.SaveToStream(s: TStream);
var
  xml: TXMLConfig;
begin
  xml:=TXMLConfig.Create(nil);
  try
    Save(XML,'');
    xml.WriteToStream(s);
  finally
    xml.Free;
  end;
end;

procedure TAarrePkgList.LoadFromStream(s: TStream);
var
  xml: TXMLConfig;
begin
  xml:=TXMLConfig.Create(nil);
  try
    xml.ReadFromStream(s);
    Load(XML,'');
  finally
    xml.Free;
  end;
end;

procedure TAarrePkgList.SaveToFile(const aFilename: string);
var
  fs: TGZFileStream;
  ms: TMemoryStream;
  Cnt: LongInt;
begin
  fs:=TGZFileStream.create(aFilename,gzopenwrite);
  try
    ms:=TMemoryStream.Create;
    try
      SaveToStream(ms);
      Cnt:=fs.write(ms.Memory^,ms.Size);
      if Cnt<ms.Size then
        raise Exception.Create('write error "'+aFilename+'"');
    finally
      ms.Free;
    end;
  finally
    fs.Free;
  end;
end;

function TAarrePkgList.AsString: string;
var
  ms: TMemoryStream;
begin
  Result:='';
  ms:=TMemoryStream.Create;
  try
    SaveToStream(ms);
    SetLength(Result,ms.Size);
    if Result<>'' then
      Move(ms.Memory^,Result[1],length(Result));
  finally
    ms.Free;
  end;
end;

procedure TAarrePkgList.Add(Item: TAarrePkgListItem);
begin
  if Item=nil then
    raise Exception.Create('Item=nil');
  Insert(Count,Item);
end;

procedure TAarrePkgList.Insert(Index: integer; Item: TAarrePkgListItem);
begin
  fItems.Insert(Index,Item);
end;

procedure TAarrePkgList.Remove(Item: TAarrePkgListItem);
begin
  fItems.Remove(Item);
end;

procedure TAarrePkgList.Delete(Index: integer);
begin
  TObject(fItems[Index]).Free;
  fItems.Delete(Index);
end;

function TAarrePkgList.IndexOf(Item: TAarrePkgListItem): integer;
begin
  Result:=fItems.IndexOf(Item);
end;

function TAarrePkgList.Count: integer;
begin
  Result:=fItems.Count;
end;

{ TAPkgDependency }

constructor TAPkgDependency.Create;
begin
  FMinVersion:=TAPkgVersion.Create;
  FMaxVersion:=TAPkgVersion.Create;
end;

destructor TAPkgDependency.Destroy;
begin
  FreeAndNil(FMinVersion);
  FreeAndNil(FMaxVersion);
  inherited Destroy;
end;

procedure TAPkgDependency.Load(XML: TXMLConfig; Path: string);
begin
  Name:=XML.GetValue(Path+'PackageName/Value','');
  MaxVersion.Load(XML,Path+'MaxVersion/');
  MaxVersionValid:=XML.GetValue(Path+'MaxVersion/Valid',false);
  MinVersion.Load(XML,Path+'MinVersion/');
  MinVersionValid:=XML.GetValue(Path+'MinVersion/Valid',false);
  DefaultFilename:=XML.GetValue(Path+'DefaultFilename/Value','');
  PreferDefaultFilename:=XML.GetValue(Path+'DefaultFilename/Prefer',false);
end;

procedure TAPkgDependency.Save(XML: TXMLConfig; Path: string);
begin
  XML.SetDeleteValue(Path+'PackageName/Value',Name,'');
  MaxVersion.Save(XML,Path+'MaxVersion/');
  XML.SetDeleteValue(Path+'MaxVersion/Valid',MaxVersionValid,false);
  MinVersion.Save(XML,Path+'MinVersion/');
  XML.SetDeleteValue(Path+'MinVersion/Valid',MinVersionValid,false);
  XML.SetDeleteValue(Path+'DefaultFilename/Value',FDefaultFilename,'');
  XML.SetDeleteValue(Path+'DefaultFilename/Prefer',FPreferDefaultFilename,false);
end;

end.

