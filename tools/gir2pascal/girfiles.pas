{
girfiles.pas
Copyright (C) 2011  Andrew Haines andrewd207@aol.com

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}
unit girFiles;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, DOM, girNameSpaces, girParser, CommandLineOptions;

type

  { TgirFile }

  TgirFile = class(IgirParser)
  private
    FNameSpaces: TgirNamespaces;
    FOnNeedGirFile: TgirNeedGirFileEvent;
    FOwner: TObject;
    FCmdOptions: TCommandLineOptions;
    procedure ParseNode(ANode: TDomNode);
    procedure SetOnNeedGirFile(AValue: TgirNeedGirFileEvent);
    procedure SetOwner(const AValue: TObject);
    procedure ParseIncludeNode(ANode: TDomNode; AIncludes: TList);
    procedure CheckVersionLimits(const ANameSpace: TgirNamespace);
    function CheckVersionOptions(const ANamespace: String; var AMajor, AMinor: Integer): Boolean;
  public
    constructor Create(AOwner: TObject; AOptions: TCommandLineOptions);
    destructor Destroy; override;
    procedure ParseXMLDocument(AXML: TXMLDocument);
    property NameSpaces: TgirNamespaces read FNameSpaces;
    property Owner: TObject read FOwner write SetOwner; // TGirConsoleConverter
    property OnNeedGirFile: TgirNeedGirFileEvent read FOnNeedGirFile write SetOnNeedGirFile;

  end;

implementation
uses girErrors, girTokens;

{ TgirFile }


{ TgirFile }

procedure TgirFile.ParseNode(ANode: TDomNode);
var
  Node: TDomNode;
  NS: TgirNamespace;
  Includes: TList;
begin
  if ANode.NodeName <> 'repository' then
    girError(geError, 'Not a Valid Document Type!');

  Node := Anode.FirstChild;
  Ns := nil;
  Includes := TList.Create;

  while Node <> nil do begin
    case GirTokenNameToToken(Node.NodeName) of
      gtInclude: ParseIncludeNode(Node, Includes);
      gtNameSpace:
        begin
          NS := TgirNamespace.CreateFromRepositoryNode(NameSpaces, ANode, Includes);
          girError(geDebug, 'Adding Namespace '+NS.NameSpace+' to NameSpaces');
          FNameSpaces.Add(NS);
          girError(geDebug, 'Added Namespace '+NS.NameSpace);
          CheckVersionLimits(NS);
          NS.ParseNode(Node);
        end;
      gtPackage, gtCInclude: ;// ignore for now
    else
      girError(geDebug, 'Unknown Node Type for Reposiotory: '+ node.NodeName);
    end;
    Node := Node.NextSibling;
  end;



  {ANode := ANode.FindNode('namespace');
  if ANode = nil then
    girError(geError, 'namespace node not found')
  else
  begin

  end;}
end;

procedure TgirFile.SetOnNeedGirFile(AValue: TgirNeedGirFileEvent);
begin
  FNameSpaces.OnNeedGirFile:=AValue;
  if FOnNeedGirFile=AValue then Exit;
  FOnNeedGirFile:=AValue;
end;

procedure TgirFile.SetOwner(const AValue: TObject);
begin
  if FOwner=AValue then exit;
  FOwner:=AValue;
end;

procedure TgirFile.ParseIncludeNode(ANode: TDomNode; AIncludes: TList);
var
  NS: TgirNamespace;
  NSName, NSVersion: String;
begin
  NSName := TDOMElement(ANode).GetAttribute('name');
  NSVersion := TDOMElement(ANode).GetAttribute('version');
  NS := FNameSpaces.FindNameSpace(NSName, NSVersion);
  if NS <> nil then
  begin
    AIncludes.Add(NS);
  end;
end;

procedure TgirFile.CheckVersionLimits(const ANameSpace: TgirNamespace);


  function SplitVersion(AVersionStr: String; out AVersion: TGirVersion): Boolean;
  begin
    try
      AVersion := girVersion(AVersionStr);
      Result := True;
    except
      Result := False;
    end;
  end;


  function SplitNameSpaceVersionCheck(AOptionName: String; var AVersion: TGirVersion): Boolean;
  var
    i: Integer;
  begin
    if FCmdOptions.HasOption(AOptionName) then
    with FCmdOptions.OptionValues(AOptionName) do
    begin
      for i := 0 to Count-1 do
      begin
        if Lowercase(ANameSpace.NameSpace)+'-' = Lowercase(Copy(Strings[i], 1, Length(ANameSpace.NameSpace)+1)) then
        begin
          Result := SplitVersion(Copy(Strings[i], Length(ANameSpace.NameSpace)+2, MaxInt), AVersion);
          break;
        end;
      end;

    end;
  end;

var
  lVersion: TGirVersion;
begin
  if SplitNameSpaceVersionCheck('max-version', lVersion) then
    ANameSpace.MaxSymbolVersion := lVersion
  else
    ANameSpace.MaxSymbolVersion := girVersion(MaxInt, MaxInt);


  if SplitNameSpaceVersionCheck('keep-deprecated-version', lVersion) then
    ANameSpace.DeprecatedVersion := lVersion
  else
    ANameSpace.DeprecatedVersion := girVersion(MaxInt, MaxInt);


end;

function TgirFile.CheckVersionOptions(const ANamespace: String; var AMajor, AMinor: Integer): Boolean;
begin
  Result := False;

end;


constructor TgirFile.Create(AOwner: TObject; AOptions: TCommandLineOptions);
begin
 Owner := AOwner;
 FCmdOptions := AOptions;
 FNameSpaces := TgirNamespaces.Create(Self);
end;

destructor TgirFile.Destroy;
begin
  FNameSpaces.Free;
  inherited Destroy;
end;

procedure TgirFile.ParseXMLDocument(AXML: TXMLDocument);
begin
  Self.ParseNode(AXML.DocumentElement);
end;



end.

