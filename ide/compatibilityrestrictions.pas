{ /***************************************************************************
                  CompatibilityIssues.pas  -  Lazarus IDE unit
                  --------------------------------------------

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

  Abstract:
    Compatiblity restrictions utilities

}
unit CompatibilityRestrictions;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, SysUtils, InterfaceBase, ObjectInspector, PackageSystem, PackageDefs,
  ComponentReg, Laz_XMLRead, Laz_XMLWrite, Laz_DOM, LazConf, LCLProc, StringHashList;

type
  TReadIssueEvent = procedure (const IssueName, WidgetSetName: String) of object;
  TReadIssueContentEvent = procedure (const Short, Description: String) of object;

  PIssue = ^TIssue;
  TIssue = record
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
  
  TIssueList = array of TIssue;

  { TIssueManager }

  TIssueManager = class
  private
    FIssueProperties: TOIRestrictedProperties;
    FIssueList: TIssueList;
    FIssueFiles: TStringList;
    FClassList: TClassHashList;
    procedure AddPackage(APackage: TLazPackageID);
    procedure AddIssue(const IssueName, WidgetSetName: String);
    procedure AddIssueContent(const Short, Description: String);
    procedure AddIssueProperty(const IssueName, WidgetSetName: String);
    procedure GatherIssueFiles;
    procedure ReadFileIssues(const Filename: String; OnReadIssue: TReadIssueEvent;
      OnReadIssueContent: TReadIssueContentEvent);
  public
    constructor Create;
    destructor Destroy; override;
    
    function GetIssueProperties: TOIRestrictedProperties;
    function GetIssueList: TIssueList;
  end;

  
function GetIssueProperties: TOIRestrictedProperties;
function GetIssueList: TIssueList;
  
implementation

var
  IssueManager: TIssueManager = nil;
  
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


function GetIssueProperties: TOIRestrictedProperties;
begin
  if IssueManager = nil then IssueManager := TIssueManager.Create;
  Result := IssueManager.GetIssueProperties;
end;

function GetIssueList: TIssueList;
begin
  if IssueManager = nil then IssueManager := TIssueManager.Create;
  Result := IssueManager.GetIssueList;
end;

{ TIssueManager }

function TIssueManager.GetIssueProperties: TOIRestrictedProperties;
var
  I: Integer;
begin
  Result := nil;
  FreeAndNil(FIssueProperties);
  FIssueProperties := TOIRestrictedProperties.Create;


  FClassList := TClassHashList.Create;
  try
    IDEComponentPalette.IterateRegisteredClasses(@(FClassList.AddComponent));
    FClassList.Add(TForm);
    FClassList.Add(TDataModule);
  
    for I := 0 to FIssueFiles.Count - 1 do
      ReadFileIssues(FIssueFiles[I], @AddIssueProperty, nil);
    
    Result := FIssueProperties;
  finally
    FreeAndNil(FClassList);
  end;
end;

function TIssueManager.GetIssueList: TIssueList;
var
  I: Integer;
begin
  SetLength(FIssueList, 0);

  for I := 0 to FIssueFiles.Count - 1 do
    ReadFileIssues(FIssueFiles[I], @AddIssue, @AddIssueContent);
    
  Result := FIssueList;
end;

procedure TIssueManager.AddPackage(APackage: TLazPackageID);
var
  ALazPackage: TLazPackage;
  I: Integer;
begin
  if APackage = nil then Exit;
  ALazPackage := PackageGraph.FindPackageWithID(APackage);
  if ALazPackage = nil then Exit;
  
  for I := 0 to ALazPackage.FileCount - 1 do
    if ALazPackage.Files[I].FileType = pftIssues then
      FIssueFiles.Add(ALazPackage.Files[I].GetFullFilename);
end;

procedure TIssueManager.AddIssue(const IssueName, WidgetSetName: String);
begin
  SetLength(FIssueList, Succ(Length(FIssueList)));
  FIssueList[High(FIssueList)].Name := IssueName;
  FIssueList[High(FIssueList)].WidgetSet := DirNameToLCLPlatform(WidgetSetName);
  FIssueList[High(FIssueList)].Short := '';
  FIssueList[High(FIssueList)].Description := '';
end;

procedure TIssueManager.AddIssueContent(const Short, Description: String);
begin
  if Length(FIssueList) = 0 then Exit;
  FIssueList[High(FIssueList)].Short := Short;
  FIssueList[High(FIssueList)].Description := Description;
end;

procedure TIssueManager.AddIssueProperty(const IssueName, WidgetSetName: String);
var
  Issue: TOIRestrictedProperty;
  AClass: TPersistentClass;
  AProperty: String;
  P: Integer;
begin
  //DebugLn('TIssueManager.AddIssue ', IssueName, ' ', WidgetSetName);
  if IssueName = '' then Exit;

  P := Pos('.', IssueName);
  if P = 0 then
  begin
    AClass := FClassList.Find(IssueName);
    AProperty := '';
  end
  else
  begin
    AClass := FClassList.Find(Copy(IssueName, 0, P - 1));
    AProperty := Copy(IssueName, P + 1, MaxInt);
  end;
  
  if AClass = nil then
  begin
    // add as generic widgetset issue
    Inc(FIssueProperties.WidgetSetRestrictions[DirNameToLCLPlatform(WidgetSetName)]);
    Exit;
  end;
  
  Issue := TOIRestrictedProperty.Create(AClass, AProperty, True);
  Issue.WidgetSets := [DirNameToLCLPlatform(WidgetSetName)];
  FIssueProperties.Add(Issue);
  //DebugLn('TIssueManager.AddIssue True');
end;

procedure TIssueManager.GatherIssueFiles;
begin
  FIssueFiles.Clear;
  PackageGraph.IteratePackages([fpfSearchInInstalledPckgs], @AddPackage);
end;

procedure TIssueManager.ReadFileIssues(const Filename: String; OnReadIssue: TReadIssueEvent;
  OnReadIssueContent: TReadIssueContentEvent);
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
        
        if Assigned(OnReadIssue) then OnReadIssue(IssueName, WidgetSetName);
        if Assigned(OnReadIssueContent) then
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
          
          OnReadIssueContent(Short, Description);
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

constructor TIssueManager.Create;
begin
  inherited;
  
  FIssueFiles := TStringList.Create;
  FIssueProperties := nil;
  
  GatherIssueFiles;
end;

destructor TIssueManager.Destroy;
begin
  FreeAndNil(FIssueFiles);
  FreeAndNil(FIssueProperties);
  
  inherited Destroy;
end;


finalization

  FreeAndNil(IssueManager);


end.
