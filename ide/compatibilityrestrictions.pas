{ /***************************************************************************
              CompatibilityRestrictions.pas  -  Lazarus IDE unit
              --------------------------------------------------

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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Abstract:
    Compatiblity restrictions utilities

}
unit CompatibilityRestrictions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, LCLProc, LCLPlatformDef,
  // LazUtils
  Laz2_DOM, Laz2_XMLRead, Laz2_XMLWrite, StringHashList,
  // IdeIntf
  OIFavoriteProperties, PackageIntf, ComponentReg,
  // IDE
  PackageSystem, PackageDefs;

type
  TReadRestrictedEvent = procedure (const RestrictedName, WidgetSetName: String) of object;
  TReadRestrictedContentEvent = procedure (const Short, Description: String) of object;

  PRestriction = ^TRestriction;
  TRestriction = record
    Name: String;
    Short: String;
    Description: String;
    WidgetSet: TLCLPlatform;
  end;
  
  { TClassHashList }

  TClassHashList = class
  private
    FHashList: TStringHashList;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Add(const AClass: TPersistentClass);
    procedure AddComponent(const AClass: TComponentClass);
    function Find(const AClassName: String): TPersistentClass;
  end;
  
  TRestrictedList = array of TRestriction;

  { TRestrictedManager }

  TRestrictedManager = class
  private
    FRestrictedProperties: TOIRestrictedProperties;
    FRestrictedList: TRestrictedList;
    FRestrictedFiles: TStringList;
    FClassList: TClassHashList;
    procedure AddPackage(APackage: TLazPackageID);
    procedure AddRestricted(const RestrictedName, WidgetSetName: String);
    procedure AddRestrictedContent(const Short, Description: String);
    procedure AddRestrictedProperty(const RestrictedName, WidgetSetName: String);
    procedure GatherRestrictedFiles;
    procedure ReadRestrictions(const Filename: String;
      OnReadRestricted: TReadRestrictedEvent;
      OnReadRestrictedContent: TReadRestrictedContentEvent);
  public
    constructor Create;
    destructor Destroy; override;
    
    function GetRestrictedProperties: TOIRestrictedProperties;
    function GetRestrictedList: TRestrictedList;
  end;

  
function GetRestrictedProperties: TOIRestrictedProperties;
function GetRestrictedList: TRestrictedList;
  
implementation

var
  RestrictedManager: TRestrictedManager = nil;
  
{ TClassHashList }

constructor TClassHashList.Create;
begin
  inherited;
  
  FHashList := TStringHashList.Create(False);
end;

destructor TClassHashList.Destroy;
begin
  FHashList.Free;
  
  inherited;
end;

procedure TClassHashList.Add(const AClass: TPersistentClass);
var
  C: TClass;
begin
  C := AClass;
  while (C <> nil) and (FHashList.Find(C.ClassName) < 0) do
  begin
    FHashList.Add(C.ClassName, Pointer(C));
    if (C = TPersistent) then Break;
    C := C.ClassParent;
  end;
end;

procedure TClassHashList.AddComponent(const AClass: TComponentClass);
begin
  Add(AClass);
end;

function TClassHashList.Find(const AClassName: String): TPersistentClass;
begin
  Result := TPersistentClass(FHashList.Data[AClassName]);
end;


function GetRestrictedProperties: TOIRestrictedProperties;
begin
  if RestrictedManager = nil then
    RestrictedManager := TRestrictedManager.Create;
  Result := RestrictedManager.GetRestrictedProperties;
end;

function GetRestrictedList: TRestrictedList;
begin
  if RestrictedManager = nil then
    RestrictedManager := TRestrictedManager.Create;
  Result := RestrictedManager.GetRestrictedList;
end;

{ TRestrictedManager }

function TRestrictedManager.GetRestrictedProperties: TOIRestrictedProperties;
var
  I: Integer;
begin
  Result := nil;
  FreeAndNil(FRestrictedProperties);
  FRestrictedProperties := TOIRestrictedProperties.Create;


  FClassList := TClassHashList.Create;
  try
    IDEComponentPalette.IterateRegisteredClasses(@(FClassList.AddComponent));
    FClassList.Add(TForm);
    FClassList.Add(TDataModule);
  
    for I := 0 to FRestrictedFiles.Count - 1 do
      ReadRestrictions(FRestrictedFiles[I], @AddRestrictedProperty, nil);
    
    Result := FRestrictedProperties;
  finally
    FreeAndNil(FClassList);
  end;
end;

function TRestrictedManager.GetRestrictedList: TRestrictedList;
var
  I: Integer;
begin
  SetLength(FRestrictedList, 0);

  for I := 0 to FRestrictedFiles.Count - 1 do
    ReadRestrictions(FRestrictedFiles[I], @AddRestricted, @AddRestrictedContent);
    
  Result := FRestrictedList;
end;

procedure TRestrictedManager.AddPackage(APackage: TLazPackageID);
var
  ALazPackage: TLazPackage;
  I: Integer;
begin
  if APackage = nil then Exit;
  ALazPackage := PackageGraph.FindPackageWithID(APackage);
  if ALazPackage = nil then Exit;
  
  for I := 0 to ALazPackage.FileCount - 1 do
    if ALazPackage.Files[I].FileType = pftIssues then
      FRestrictedFiles.Add(ALazPackage.Files[I].GetFullFilename);
end;

procedure TRestrictedManager.AddRestricted(const RestrictedName, WidgetSetName: String);
begin
  SetLength(FRestrictedList, Succ(Length(FRestrictedList)));
  FRestrictedList[High(FRestrictedList)].Name := RestrictedName;
  FRestrictedList[High(FRestrictedList)].WidgetSet := DirNameToLCLPlatform(WidgetSetName);
  FRestrictedList[High(FRestrictedList)].Short := '';
  FRestrictedList[High(FRestrictedList)].Description := '';
end;

procedure TRestrictedManager.AddRestrictedContent(const Short, Description: String);
begin
  if Length(FRestrictedList) = 0 then Exit;
  FRestrictedList[High(FRestrictedList)].Short := Short;
  FRestrictedList[High(FRestrictedList)].Description := Description;
end;

procedure TRestrictedManager.AddRestrictedProperty(const RestrictedName, WidgetSetName: String);
var
  Issue: TOIRestrictedProperty;
  AClass: TPersistentClass;
  AProperty: String;
  P: Integer;
  Platform: TLCLPlatform;
begin
  if RestrictedName = '' then Exit;

  P := Pos('.', RestrictedName);
  if P = 0 then
  begin
    AClass := FClassList.Find(RestrictedName);
    AProperty := '';
  end
  else
  begin
    AClass := FClassList.Find(Copy(RestrictedName, 0, P - 1));
    AProperty := Copy(RestrictedName, P + 1, MaxInt);
  end;

  Platform:=DirNameToLCLPlatform(WidgetSetName);
  if AClass = nil then
  begin
    // add as generic widgetset issue
    //debugln('TRestrictedManager.AddRestrictedProperty ',RestrictedName,' ',WidgetSetName);
    inc(FRestrictedProperties.WidgetSetRestrictions[Platform]);
    Exit;
  end;
  
  Issue := TOIRestrictedProperty.Create(AClass, AProperty, True);
  Issue.WidgetSets := [Platform];
  FRestrictedProperties.Add(Issue);
end;

procedure TRestrictedManager.GatherRestrictedFiles;
begin
  FRestrictedFiles.Clear;
  PackageGraph.IteratePackages([fpfSearchInInstalledPckgs], @AddPackage);
end;

procedure TRestrictedManager.ReadRestrictions(const Filename: String;
  OnReadRestricted: TReadRestrictedEvent;
  OnReadRestrictedContent: TReadRestrictedContentEvent);
var
  IssueFile: TXMLDocument;
  R, N: TDOMNode;
  
  function ReadContent(ANode: TDOMNode): String;
  var
    S: TStringStream;
    N: TDOMNode;
  begin
    Result := '';
    S := TStringStream.Create('');
    try
      N := ANode.FirstChild;
      while N <> nil do
      begin
        WriteXML(N, S);
        N := N.NextSibling;
      end;
      
      Result := S.DataString;
    finally
      S.Free;
    end;
  end;
  
  procedure ParseWidgetSet(ANode: TDOMNode);
  var
    WidgetSetName, IssueName, Short, Description: String;
    IssueNode, AttrNode, IssueContentNode: TDOMNode;
  begin
    AttrNode := ANode.Attributes.GetNamedItem('name');
    if AttrNode <> nil then WidgetSetName := AttrNode.NodeValue
    else WidgetSetName := 'win32';
    
    IssueNode := ANode.FirstChild;
    while IssueNode <> nil do
    begin
      if IssueNode.NodeName = 'issue' then
      begin
        AttrNode := IssueNode.Attributes.GetNamedItem('name');
        if AttrNode <> nil then IssueName := AttrNode.NodeValue
        else IssueName := 'win32';
        
        if Assigned(OnReadRestricted) then
          OnReadRestricted(IssueName, WidgetSetName);
        if Assigned(OnReadRestrictedContent) then
        begin
          Short := '';
          Description := '';
          
          IssueContentNode := IssueNode.FirstChild;
          while IssueContentNode <> nil do
          begin
            if IssueContentNode.NodeName = 'short' then
              Short := ReadContent(IssueContentNode)
            else
              if IssueContentNode.NodeName = 'descr' then
                Description := ReadContent(IssueContentNode);

            IssueContentNode := IssueContentNode.NextSibling;
          end;
          
          OnReadRestrictedContent(Short, Description);
        end;
      end;
      IssueNode := IssueNode.NextSibling;
    end;
  end;
  
begin
  try
    ReadXMLFile(IssueFile, Filename);
  except
     on E: Exception do
       DebugLn('TIssueManager.ReadFileIssues failed: ' + E.Message);
  end;
  
  try
    if IssueFile = nil then Exit;

    R := IssueFile.FindNode('package');
    if R = nil then Exit;

    N := R.FirstChild;
    while N <> nil do
    begin
      if N.NodeName = 'widgetset' then
        ParseWidgetSet(N);

      N := N.NextSibling;
    end;
  finally
    IssueFile.Free;
  end;
end;

constructor TRestrictedManager.Create;
begin
  inherited;
  
  FRestrictedFiles := TStringList.Create;
  FRestrictedProperties := nil;
  
  GatherRestrictedFiles;
end;

destructor TRestrictedManager.Destroy;
begin
  FreeAndNil(FRestrictedFiles);
  FreeAndNil(FRestrictedProperties);
  
  inherited Destroy;
end;


finalization

  FreeAndNil(RestrictedManager);


end.
