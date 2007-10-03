{
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

  Author: Tom Gregorovic
}
unit FPDocFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, DOM, XMLWrite, XMLRead;
  
type
  { TFPDocNode }

  TFPDocNode = class
  private
    FName: String;
    FNode: TDOMNode;
    function GetNodeValue(const AName: String): String;
    function GetDescription: String;
    function GetShort: String;
    procedure SetNodeValue(const AName, AValue: String);
    procedure SetDescription(const AValue: String);
    procedure SetShort(const AValue: String);
  public
    constructor Create(ANode: TDOMNode);
    procedure Assign(ASource: TFPDocNode);
  public
    property Name: String read FName;
    property Description: String read GetDescription write SetDescription;
    property Short: String read GetShort write SetShort;
  end;

  { TFPDocElement }

  TFPDocElement = class(TFPDocNode)
    function GetEmpty: Boolean;
  private
    function GetErrors: String;
    function GetSeaAlso: String;
    procedure SetErrors(const AValue: String);
    procedure SetSeaAlso(const AValue: String);
  public
    procedure Assign(ASource: TFPDocElement);
  public
    property Errors: String read GetErrors write SetErrors;
    property SeaAlso: String read GetSeaAlso write SetSeaAlso;
    property Empty: Boolean read GetEmpty;
  end;
  
  { TFPDocModule }

  TFPDocModule = class(TFPDocNode)
  private
    FElements: TObjectList;
    FNames: TStringList;
    function GetCount: Integer;
    function GetElement(Index: Integer): TFPDocElement;
    function GetElementByName(const Index: String): TFPDocElement;
  public
    constructor Create(ANode: TDOMNode);
    destructor Destroy; override;
    procedure ParseElements;
    procedure Add(const AElement: TFPDocElement);
  public
    property Elements[Index: Integer]: TFPDocElement read GetElement;
    property ElementsByName[const Index: String]: TFPDocElement read GetElementByName;
    property Count: Integer read GetCount;
    property Names: TStringList read FNames;
  end;
  
  { TFPDocPackage }

  TFPDocPackage = class(TFPDocNode)
  private
    FModules: TObjectList;
    FNames: TStringList;
    function GetCount: Integer;
    function GetModule(Index: Integer): TFPDocModule;
    function GetModuleByName(const Index: String): TFPDocModule;
  public
    constructor Create(ANode: TDOMNode);
    destructor Destroy; override;
    procedure ParseModules;
  public
    property Modules[Index: Integer]: TFPDocModule read GetModule;
    property ModulesByName[const Index: String]: TFPDocModule read GetModuleByName;
    property Count: Integer read GetCount;
    property Names: TStringList read FNames;
  end;
  
  TMoveElementEvent = procedure (const SrcPackage: TFPDocPackage;
    const SrcModule: TFPDocModule; const Src: TFPDocElement;
    const DestList: TStrings; var Dest: String) of object;
  
  { TFPDocFile }

  TFPDocFile = class
  private
    FDocument: TXMLDocument;
    FPackages: TObjectList;
    FNames: TStringList;
    function GetCount: Integer;
    function GetPackage(Index: Integer): TFPDocPackage;
    function GetPackageByName(const Index: String): TFPDocPackage;
  public
    constructor Create(const FileName: String);
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure ParsePackages;
    procedure SaveToFile(const FileName: String);
    procedure AssignToSkeleton(const SkeletonFile: TFPDocFile;
      OnMoveElement: TMoveElementEvent);
  public
    property Packages[Index: Integer]: TFPDocPackage read GetPackage;
    property PackagesByName[const Index: String]: TFPDocPackage read GetPackageByName;
    property Count: Integer read GetCount;
    property Names: TStringList read FNames;
  end;

implementation

uses LCLProc;

{ TFPDocNode }

function TFPDocNode.GetNodeValue(const AName: String): String;
var
  N: TDOMNode;
  S: TStringStream;
begin
  Result := '';
  N := FNode.FindNode(AName);
  if N = nil then Exit;
  if N.FirstChild = nil then Exit;
  
  S := TStringStream.Create('');
  try
    WriteXML(N.FirstChild, S);
    Result := S.DataString;
  finally
    S.Free;
  end;
end;

function TFPDocNode.GetDescription: String;
begin
  Result := GetNodeValue('descr');
end;

function TFPDocNode.GetShort: String;
begin
  Result := GetNodeValue('short');
end;

procedure TFPDocNode.SetNodeValue(const AName, AValue: String);
var
  N: TDOMNode;
  S: TStringStream;
begin
  N := FNode.FindNode(AName);

  if N = nil then
  begin
    if AValue = '' then Exit;
    N := FNode.OwnerDocument.CreateElement(AName);
    FNode.AppendChild(N);
  end;

  while N.FirstChild <> nil do N.RemoveChild(N.FirstChild);

  S := TStringStream.Create(AValue);
  try
    ReadXMLFragment(N, S);
  finally
    S.Free;
  end;
end;

procedure TFPDocNode.SetDescription(const AValue: String);
begin
  SetNodeValue('descr', AValue);
end;

procedure TFPDocNode.SetShort(const AValue: String);
begin
  SetNodeValue('short', AValue);
end;

constructor TFPDocNode.Create(ANode: TDOMNode);
begin
  FNode := ANode;
  FName := FNode.Attributes.GetNamedItem('name').NodeValue;
end;

procedure TFPDocNode.Assign(ASource: TFPDocNode);
begin
  Description := ASource.Description;
  Short := ASource.Short;
end;

{ TFPDocElement }

function TFPDocElement.GetEmpty: Boolean;
begin
  Result := (Description = '') and (Short = '') and (Errors = '') and
    (SeaAlso = '');
end;

function TFPDocElement.GetErrors: String;
begin
  Result := GetNodeValue('errors');
end;

function TFPDocElement.GetSeaAlso: String;
begin
  Result := GetNodeValue('seaalso');
end;

procedure TFPDocElement.SetErrors(const AValue: String);
begin
  SetNodeValue('errors', AValue);
end;

procedure TFPDocElement.SetSeaAlso(const AValue: String);
begin
  SetNodeValue('seaalso', AValue);
end;


procedure TFPDocElement.Assign(ASource: TFPDocElement);
begin
  inherited Assign(ASource);

  Errors := ASource.Errors;
  SeaAlso := ASource.SeaAlso;
end;

{ TFPDocModule }

function TFPDocModule.GetCount: Integer;
begin
  Result := FElements.Count;
end;

function TFPDocModule.GetElement(Index: Integer): TFPDocElement;
begin
  Result := FElements[Index] as TFPDocElement;
end;

function TFPDocModule.GetElementByName(const Index: String): TFPDocElement;
var
  I: Integer;
begin
  I := FNames.IndexOf(Index);
  if I = -1 then
    Result := nil
  else
    Result := FNames.Objects[I] as TFPDocElement;
end;

constructor TFPDocModule.Create(ANode: TDOMNode);
begin
  inherited;
  
  FElements := TObjectList.Create(True);
  FNames := TStringList.Create;
  FNames.Sorted := True;
  
  ParseElements;
end;

destructor TFPDocModule.Destroy;
begin
  FNames.Free;
  FElements.Free;
  
  inherited Destroy;
end;

procedure TFPDocModule.ParseElements;
var
  I: TDOMNode;
  E: TFPDocElement;
begin
  FElements.Clear;
  FNames.Clear;

  I := FNode.FirstChild;
  while I <> nil do
  begin
    if I.NodeName = 'element' then
    begin
      E := TFPDocElement.Create(I);
      FElements.Add(E);
      FNames.AddObject(E.Name, E);
    end;

    I := I.NextSibling;
  end;
end;

procedure TFPDocModule.Add(const AElement: TFPDocElement);
var
  E: TFPDocElement;
  N: TDOMElement;
begin
  E := ElementsByName[AElement.Name];
  
  if E = nil then
  begin
    N := FNode.OwnerDocument.CreateElement('element');
    N.AttribStrings['name'] := AElement.Name;
    
    E := TFPDocElement.Create(FNode.AppendChild(N));
    FElements.Add(E);
    FNames.AddObject(E.Name, E);
  end;
  
  E.Assign(AElement);
end;

{ TFPDocPackage }

function TFPDocPackage.GetCount: Integer;
begin
  Result := FModules.Count;
end;

function TFPDocPackage.GetModule(Index: Integer): TFPDocModule;
begin
  Result := FModules[Index] as TFPDocModule;
end;

function TFPDocPackage.GetModuleByName(const Index: String): TFPDocModule;
var
  I: Integer;
begin
  I := FNames.IndexOf(Index);
  if I = -1 then
    Result := nil
  else
    Result := FNames.Objects[I] as TFPDocModule;
end;

constructor TFPDocPackage.Create(ANode: TDOMNode);
begin
  inherited;
  
  FModules := TObjectList.Create(True);
  FNames := TStringList.Create;
  FNames.Sorted := True;
  
  ParseModules;
end;

destructor TFPDocPackage.Destroy;
begin
  FNames.Free;
  FModules.Free;

  inherited Destroy;
end;

procedure TFPDocPackage.ParseModules;
var
  I: TDOMNode;
  M: TFPDocModule;
begin
  FModules.Clear;

  I := FNode.FirstChild;
  while I <> nil do
  begin
    if I.NodeName = 'module' then
    begin
      M := TFPDocModule.Create(I);
      FModules.Add(M);
      FNames.AddObject(M.Name, M);
    end;

    I := I.NextSibling;
  end;
end;

{ TFPDocFile }

function TFPDocFile.GetCount: Integer;
begin
  Result := FPackages.Count;
end;

function TFPDocFile.GetPackage(Index: Integer): TFPDocPackage;
begin
  Result := FPackages[Index] as TFPDocPackage;
end;

function TFPDocFile.GetPackageByName(const Index: String): TFPDocPackage;
var
  I: Integer;
begin
  I := FNames.IndexOf(Index);
  if I = -1 then
    Result := nil
  else
    Result := FNames.Objects[I] as TFPDocPackage;
end;

constructor TFPDocFile.Create(const FileName: String);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmOpenRead);
  try
    Create(F);
  finally
    F.Free;
  end;
end;

constructor TFPDocFile.Create(Stream: TStream);
begin
  ReadXMLFile(FDocument, Stream);

  FPackages := TObjectList.Create(True);
  FNames := TStringList.Create;
  FNames.Sorted := True;

  ParsePackages;
end;

destructor TFPDocFile.Destroy;
begin
  FNames.Free;
  FPackages.Free;
  FDocument.Free;

  inherited Destroy;
end;

procedure TFPDocFile.ParsePackages;
var
  I, R: TDOMNode;
  P: TFPDocPackage;
begin
  FPackages.Clear;
  
  R := FDocument.FindNode('fpdoc-descriptions');
  if R = nil then
    raise Exception.Create('Invalid FPDoc file!');
  
  I := R.FirstChild;
  while I <> nil do
  begin
    if I.NodeName = 'package' then
    begin
      P := TFPDocPackage.Create(I);
      FPackages.Add(P);
      FNames.AddObject(P.Name, P);
    end;
    
    I := I.NextSibling;
  end;
end;

procedure TFPDocFile.SaveToFile(const FileName: String);
begin
  WriteXMLFile(FDocument, FileName);
end;

procedure TFPDocFile.AssignToSkeleton(const SkeletonFile: TFPDocFile;
  OnMoveElement: TMoveElementEvent);
var
  I, J, K: Integer;
  P1, P2: TFPDocPackage;
  M1, M2: TFPDocModule;
  E1, E2: TFPDocElement;
  DestList: TStringList;
  Dest: String;
begin
  for I := 0 to Count - 1 do
  begin
    P1 := Packages[I];
    P2 := SkeletonFile.PackagesByName[P1.Name];
    P2.Assign(P1);
    
    for J := 0 to P1.Count - 1 do
    begin
      M1 := P1.Modules[J];
      M2 := P2.ModulesByName[M1.Name];
      M2.Assign(M1);
      
      DestList := TStringList.Create;
      try
        for K := 0 to M2.Count - 1 do
        begin
          E2 := M2.Elements[K];
          if M1.ElementsByName[E2.Name] = nil then DestList.Add(E2.Name);
        end;
        
        for K := 0 to M1.Count - 1 do
        begin
          E1 := M1.Elements[K];
          if E1.Empty then Continue;

          E2 := M2.ElementsByName[E1.Name];

          if E2 = nil then
          begin
            Dest := '';
            if Assigned(OnMoveElement) then
              OnMoveElement(P1, M1, E1, DestList, Dest);

            E2 := M2.ElementsByName[Dest];
            if E2 <> nil then E2.Assign(E1);
          end
          else
            E2.Assign(E1);
        end;
      finally
        DestList.Free;
      end;
    end;
  end;
end;


end.

