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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Tom Gregorovic
}
unit FPDocFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, FileUtil, DOM, XMLWrite, XMLRead,
  LazUtf8;
  
type
  TFPDocInfo = record
    Packages: Integer;
    Modules: Integer;
    Topics: Integer;
    Elements: Integer;
    ElementsNonEmpty: Integer;
    Shorts: Integer;
    Descriptions: Integer;
    Errors: Integer;
    SeeAlsos: Integer;
    Examples: Integer;
  end;

const
  EmptyFPDocInfo: TFPDocInfo = (
    Packages: 0;
    Modules: 0;
    Topics: 0;
    Elements: 0;
    ElementsNonEmpty: 0;
    Shorts: 0;
    Descriptions: 0;
    Errors: 0;
    SeeAlsos: 0;
    Examples: 0;
    );
  
type
  TUniqueName = record
    Name: String;
    Indexes: Array of Integer;
    NonEmptyCount: Integer;
  end;

  TUniqueNameArray = Array of TUniqueName;
  
  TFPDocNode = class;

  { TFPDocItem }

  TFPDocItem = class
  private
    FParent: TFPDocItem;
    FModified: Boolean;
  public
    constructor Create(const AParent: TFPDocItem);
    function GetInfo: TFPDocInfo; virtual; abstract;
    
    procedure Modify; virtual;
    procedure Reset; virtual;
    function AddNode(const AName: String): TFPDocNode; virtual; abstract;
    function InsertNode(Index: Integer; const AName: String): TFPDocNode; virtual; abstract;
    procedure DeleteNode(ANode: TFPDocNode); virtual; abstract;
    procedure RenameNode(ANode: TFPDocNode; const AName: String); virtual; abstract;
  public
    property Modified: Boolean read FModified write FModified;
    property Parent: TFPDocItem read FParent;
  end;

  { TFPDocNode }

  TFPDocNodeClass = class of TFPDocNode;
  TFPDocNode = class(TFPDocItem)
  private
    FName: String;
    FDOMNode: TDOMNode;
    function GetDOMNodeValue(const AName: String): String;
    function GetDescription: String;
    function GetShort: String;
    procedure SetDOMNodeValue(const AName, AValue: String);
    procedure SetDescription(const AValue: String);
    procedure SetShort(const AValue: String);
  protected
    function InsertDOMNode(const AName: String; const ADOMNode: TDOMNode): TDOMNode; virtual;
    function GetEmpty: Boolean; virtual;
  public
    constructor Create(const AParent: TFPDocItem; const ADOMNode: TDOMNode);
    procedure Assign(ASource: TFPDocNode);
    function GetInfo: TFPDocInfo; override;
    
    procedure Rename(const S: String);
    procedure Delete;
  public
    property Name: String read FName;
    property Description: String read GetDescription write SetDescription;
    property Short: String read GetShort write SetShort;
    property Empty: Boolean read GetEmpty;
  end;

  { TFPDocElement }

  TFPDocElement = class(TFPDocNode)
  private
    FExamples: TList;
    function GetErrors: String;
    function GetExample(Index: Integer): String;
    function GetExamplesCount: Integer;
    function GetSeeAlso: String;
    procedure SetErrors(const AValue: String);
    procedure SetExample(Index: Integer; const AValue: String);
    procedure SetSeeAlso(const AValue: String);
  protected
    function GetEmpty: Boolean; override;
  public
    constructor Create(const AParent: TFPDocItem; const ADOMNode: TDOMNode);
    destructor Destroy; override;

    procedure Assign(ASource: TFPDocElement);
    function GetInfo: TFPDocInfo; override;
    
    procedure ParseExamples; virtual;
    
    function AddExample(const AFileName: String): Integer; virtual;
    function InsertExample(Index: Integer; const AFileName: String): Integer; virtual;
    procedure DeleteExample(Index: Integer); virtual;

    function AddNode(const AName: String): TFPDocNode; override;
    procedure DeleteNode(ANode: TFPDocNode); override;
    function InsertNode(Index: Integer; const AName: String): TFPDocNode;
      override;
    procedure RenameNode(ANode: TFPDocNode; const AName: String); override;
  public
    property Errors: String read GetErrors write SetErrors;
    property SeeAlso: String read GetSeeAlso write SetSeeAlso;
    property Examples[Index: Integer]: String read GetExample write SetExample;
    property ExamplesCount: Integer read GetExamplesCount;
  end;
  
  { TFPDocTopic }

  TFPDocTopic = class(TFPDocNode)
  private
    FTopics: TObjectList;
    function GetTopicsCount: Integer;
    function GetTopic(Index: Integer): TFPDocTopic;
  public
    constructor Create(const AParent: TFPDocItem; const ADOMNode: TDOMNode);
    destructor Destroy; override;

    procedure ParseTopics; virtual;
    procedure AddTopic(const ATopic: TFPDocTopic);
    procedure Assign(ASource: TFPDocTopic);
    function GetInfo: TFPDocInfo; override;
    
    procedure Reset; override;
    
    function AddTopic(const AName: String): TFPDocTopic; virtual;
    function InsertTopic(Index: Integer; const AName: String): TFPDocTopic; virtual;
    procedure DeleteNode(ANode: TFPDocNode); override;
    procedure RenameNode(ANode: TFPDocNode; const AName: String); override;
  public
    function AddNode(const AName: String): TFPDocNode; override;
    function InsertNode(Index: Integer; const AName: String): TFPDocNode;
      override;
    property Topics[Index: Integer]: TFPDocTopic read GetTopic;
    property TopicsCount: Integer read GetTopicsCount;
  end;
  
  { TFPDocNodeWithList }

  TFPDocNodeWithList = class(TFPDocTopic)
  private
    FNodes: TObjectList;
    FNames: TStringList;
    function GetCount: Integer;
    function GetNode(Index: Integer): TFPDocNode;
    function GetNodeByName(const Index: String): TFPDocNode;
  public
    constructor Create(const AParent: TFPDocItem; const ADOMNode: TDOMNode);
    destructor Destroy; override;
    procedure ParseNodes; virtual; abstract;
    
    procedure Reset; override;
    function GetUniqueNames: TUniqueNameArray;
        
    function AddNode(const AName: String): TFPDocNode; override;
    procedure DeleteNode(ANode: TFPDocNode); override;
    procedure RenameNode(ANode: TFPDocNode; const AName: String); override;
  public
    property Nodes[Index: Integer]: TFPDocNode read GetNode;
    property NodesByName[const Index: String]: TFPDocNode read GetNodeByName;
    property Count: Integer read GetCount;
    property Names: TStringList read FNames;
  end;
  
  { TFPDocModule }

  TFPDocModule = class(TFPDocNodeWithList)
  private
    function GetElement(Index: Integer): TFPDocElement;
    function GetElementByName(const Index: String): TFPDocElement;
  public
    function GetInfo: TFPDocInfo; override;
    procedure ParseNodes; override;
    
    function InsertNode(Index: Integer; const AName: String): TFPDocNode; override;
  public
    property Elements[Index: Integer]: TFPDocElement read GetElement;
    property ElementsByName[const Index: String]: TFPDocElement read GetElementByName;
  end;
  
  { TFPDocPackage }

  TFPDocPackage = class(TFPDocNodeWithList)
  private
    function GetModule(Index: Integer): TFPDocModule;
    function GetModuleByName(const Index: String): TFPDocModule;
  public
    function GetInfo: TFPDocInfo; override;
    procedure ParseNodes; override;
    
    function InsertNode(Index: Integer; const AName: String): TFPDocNode; override;
  public
    property Modules[Index: Integer]: TFPDocModule read GetModule;
    property ModulesByName[const Index: String]: TFPDocModule read GetModuleByName;
  end;
  
  TMoveElementEvent = procedure (const SrcPackage: TFPDocPackage;
    const SrcModule: TFPDocModule; const Src: TFPDocElement;
    const DestList: TStrings; var Dest: Integer) of object;
  
  { TFPDocFile }

  TFPDocFile = class(TFPDocNodeWithList)
  private
    FDocument: TXMLDocument;
    function GetPackage(Index: Integer): TFPDocPackage;
    function GetPackageByName(const Index: String): TFPDocPackage;
  public
    constructor Create(const FileName: String);
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    
    procedure ParseNodes; override;
    
    procedure SaveToFile(const FileName: String);
    procedure AssignToSkeleton(const SkeletonFile: TFPDocFile;
      OnMoveElement: TMoveElementEvent);
    function GetInfo: TFPDocInfo; override;
    
    function InsertNode(Index: Integer; const AName: String): TFPDocNode; override;
  public
    property Packages[Index: Integer]: TFPDocPackage read GetPackage;
    property PackagesByName[const Index: String]: TFPDocPackage read GetPackageByName;
  end;
  
const
  SFPDocHeader  = 'fpdoc-descriptions';
  
  SFPDocPackage = 'package';
  SFPDocModule  = 'module';
  SFPDocTopic   = 'topic';
  SFPDocElement = 'element';

  SFPDocName    = 'name';
  SFPDocFile    = 'file';
  
  SFPDocDescr   = 'descr';
  SFPDocShort   = 'short';
  SFPDocErrors  = 'errors';
  SFPDocSeeAlso = 'seealso';
  SFPDocExample = 'example';
  
  function DbgS(const AInfo: TFPDocInfo): String; overload;

implementation

uses LCLProc;

function DbgS(const AInfo: TFPDocInfo): String;
begin
  Result :=
    SFPDocPackage + 's: ' + IntToStr(AInfo.Packages) + LineEnding +
    SFPDocModule + 's: ' + IntToStr(AInfo.Modules) + LineEnding +
    SFPDocTopic + 's: ' + IntToStr(AInfo.Topics) + LineEnding +
    SFPDocElement + 's: ' + IntToStr(AInfo.Elements) + LineEnding +
    SFPDocElement + 's NonEmpty: ' + IntToStr(AInfo.ElementsNonEmpty) + LineEnding +
    SFPDocShort + 's: ' + IntToStr(AInfo.Shorts) + LineEnding +
    SFPDocDescr + 's: ' + IntToStr(AInfo.Descriptions) + LineEnding +
    SFPDocErrors + 's: ' + IntToStr(AInfo.Errors) + LineEnding +
    SFPDocSeeAlso + 's: ' + IntToStr(AInfo.SeeAlsos) + LineEnding +
    SFPDocExample + 's: ' + IntToStr(AInfo.Examples);
end;

{ TFPDocItem }

constructor TFPDocItem.Create(const AParent: TFPDocItem);
begin
  FParent := AParent;
  FModified := False;
end;

procedure TFPDocItem.Modify;
begin
  FModified := True;
  if FParent <> nil then FParent.Modify;
end;

procedure TFPDocItem.Reset;
begin
  FModified := False;
end;

{ TFPDocNode }

function TFPDocNode.GetDOMNodeValue(const AName: String): String;
var
  N: TDOMNode;
  S: TStringStream;
  D: TDOMNode;
begin
  Result := '';
  N := FDOMNode.FindNode(AName);
  if N = nil then Exit;
  if N.FirstChild = nil then Exit;
  
  S := TStringStream.Create('');
  try
    D := N.FirstChild;
    while D <> nil do
    begin
      WriteXML(D, S);
      D := D.NextSibling;
    end;
    
    Result := S.DataString;
  finally
    S.Free;
  end;
end;

function TFPDocNode.GetDescription: String;
begin
  Result := GetDOMNodeValue(SFPDocDescr);
end;

function TFPDocNode.GetShort: String;
begin
  Result := GetDOMNodeValue(SFPDocShort);
end;

procedure TFPDocNode.SetDOMNodeValue(const AName, AValue: String);
var
  N: TDOMNode;
  S: TStream;
begin
  //DebugLn(FName, ' ', AName);
  N := FDOMNode.FindNode(AName);

  if N = nil then
  begin
    if AValue = '' then Exit;
    N := FDOMNode.OwnerDocument.CreateElement(AName);
    N := InsertDOMNode(AName, N);
    //DebugLn('New');
  end
  else
    while N.LastChild <> nil do N.RemoveChild(N.LastChild);

  if AValue = '' then Exit;

  S := TStringStream.Create(AValue);
  try
    ReadXMLFragment(N, S);
  finally
    S.Free;
  end;

  Modify;

end;

procedure TFPDocNode.SetDescription(const AValue: String);
begin
  SetDOMNodeValue(SFPDocDescr, AValue);
end;

procedure TFPDocNode.SetShort(const AValue: String);
begin
  SetDOMNodeValue(SFPDocShort, AValue);
end;

function TFPDocNode.InsertDOMNode(const AName: String; const ADOMNode: TDOMNode): TDOMNode;
begin
  if not FDOMNode.HasChildNodes then Result := FDOMNode.AppendChild(ADOMNode)
  else
  begin
    if AName = SFPDocShort then Result := FDOMNode.InsertBefore(ADOMNode, FDOMNode.FirstChild);
    if AName = SFPDocDescr then
    begin
      Result := FDOMNode.FindNode(SFPDocShort);
      if Result = nil then Result := FDOMNode.InsertBefore(ADOMNode, FDOMNode.FirstChild)
      else Result := FDOMNode.InsertBefore(ADOMNode, Result.NextSibling);
    end
    else
      if AName = SFPDocErrors then
      begin
        Result := FDOMNode.FindNode(SFPDocDescr);
        if Result = nil then Result := FDOMNode.FindNode(SFPDocShort);
        if Result = nil then Result := FDOMNode.InsertBefore(ADOMNode, FDOMNode.FirstChild)
        else Result := FDOMNode.InsertBefore(ADOMNode, Result.NextSibling);
      end
      else
        if AName = SFPDocSeeAlso then
        begin
          Result := FDOMNode.FindNode(SFPDocErrors);
          if Result = nil then Result := FDOMNode.FindNode(SFPDocDescr);
          if Result = nil then Result := FDOMNode.FindNode(SFPDocShort);
          if Result = nil then Result := FDOMNode.InsertBefore(ADOMNode, FDOMNode.FirstChild)
          else Result := FDOMNode.InsertBefore(ADOMNode, Result.NextSibling);
        end;
  end;
end;

function TFPDocNode.GetEmpty: Boolean;
begin
  Result := (Description = '') and (Short = '');
end;

constructor TFPDocNode.Create(const AParent: TFPDocItem; const ADOMNode: TDOMNode);
begin
  inherited Create(AParent);

  FDOMNode := ADOMNode;
  
  FName := (FDOMNode as TDOMElement).GetAttribute(SFPDocName);
end;

procedure TFPDocNode.Assign(ASource: TFPDocNode);
begin
  Short := ASource.Short;
  Description := ASource.Description;
end;

function TFPDocNode.GetInfo: TFPDocInfo;
begin
  Result.Packages := 0;
  Result.Modules := 0;
  Result.Topics := 0;
  Result.Elements := 0;
  Result.ElementsNonEmpty := 0;
  if Short <> '' then Result.Shorts := 1
  else Result.Shorts := 0;
  if Description <> '' then Result.Descriptions := 1
  else Result.Descriptions := 0;
  Result.Errors := 0;
  Result.SeeAlsos := 0;
  Result.Examples := 0;
end;

procedure TFPDocNode.Rename(const S: String);
begin
  if S <> Name then
    Parent.RenameNode(Self, S);
end;

procedure TFPDocNode.Delete;
begin
  Parent.DeleteNode(Self);
end;

{ TFPDocElement }

function TFPDocElement.GetEmpty: Boolean;
begin
  Result := (Description = '') and (Short = '') and (Errors = '') and
    (SeeAlso = '') and (ExamplesCount = 0);
end;

function TFPDocElement.GetErrors: String;
begin
  Result := GetDOMNodeValue(SFPDocErrors);
end;

function TFPDocElement.GetExample(Index: Integer): String;
begin
  Result := TDOMElement(FExamples[Index]).GetAttribute(SFPDocFile);
end;

function TFPDocElement.GetExamplesCount: Integer;
begin
  Result := FExamples.Count;
end;

function TFPDocElement.GetSeeAlso: String;
begin
  Result := GetDOMNodeValue(SFPDocSeeAlso);
end;

procedure TFPDocElement.SetErrors(const AValue: String);
begin
  SetDOMNodeValue(SFPDocErrors, AValue);
end;

procedure TFPDocElement.SetExample(Index: Integer; const AValue: String);
begin
  TDOMElement(FExamples[Index]).SetAttribute(SFPDocFile, AValue);
end;

procedure TFPDocElement.SetSeeAlso(const AValue: String);
begin
  SetDOMNodeValue(SFPDocSeeAlso, AValue);
end;

constructor TFPDocElement.Create(const AParent: TFPDocItem;
  const ADOMNode: TDOMNode);
begin
  inherited;
  
  FExamples := TList.Create;
  
  ParseExamples;
end;

destructor TFPDocElement.Destroy;
begin
  FExamples.Free;

  inherited Destroy;
end;


procedure TFPDocElement.Assign(ASource: TFPDocElement);
var
  I: Integer;
begin
  inherited Assign(ASource);

  Errors := ASource.Errors;
  SeeAlso := ASource.SeeAlso;
  
  FExamples.Clear;
  DebugLn('Assign Examples: ' + ASource.Name + ' ' + DbgS(ASource.ExamplesCount));
  for I := 0 to ASource.ExamplesCount - 1 do AddExample(ASource.Examples[I]);
end;

function TFPDocElement.GetInfo: TFPDocInfo;
begin
  Result := inherited GetInfo;
  
  Result.Elements := 1;
  if Empty then Result.ElementsNonEmpty := 0
  else Result.ElementsNonEmpty := 1;
  if Errors <> '' then Result.Errors := 1
  else Result.Errors := 0;
  if SeeAlso <> '' then Result.SeeAlsos := 1
  else Result.SeeAlsos := 0;
  Result.Examples := ExamplesCount;
end;

procedure TFPDocElement.ParseExamples;
var
  I: TDOMNode;
begin
  FExamples.Clear;

  I := FDOMNode.FirstChild;
  while I <> nil do
  begin
    if I.NodeName = SFPDocExample then
    begin
      FExamples.Add(Pointer(I));
    end;

    I := I.NextSibling;
  end;
end;

function TFPDocElement.AddExample(const AFileName: String): Integer;
begin
  Result := InsertExample(ExamplesCount, AFileName);
end;

function TFPDocElement.InsertExample(Index: Integer; const AFileName: String): Integer;
var
  E: TDOMElement;
  N: TDOMNode;
begin
  if Index < 0 then Index := 0;
  if Index > ExamplesCount then Index := ExamplesCount;
  E := FDOMNode.OwnerDocument.CreateElement(SFPDocExample);
  E.SetAttribute(SFPDocFile, AFileName);
  
  if Index >= ExamplesCount then
    N := FDOMNode.AppendChild(E)
  else
    N := FDOMNode.InsertBefore(E, TDOMNode(FExamples[Index]));
    
  FExamples.Insert(Index, Pointer(N));
  Result := Index;
end;

procedure TFPDocElement.DeleteExample(Index: Integer);
begin
  FDOMNode.RemoveChild(TDOMNode(FExamples[Index]));
  FExamples.Delete(Index);
end;

function TFPDocElement.AddNode(const AName: String): TFPDocNode;
begin
  raise Exception.Create('');
  Result:=nil;
end;

procedure TFPDocElement.DeleteNode(ANode: TFPDocNode);
begin
  raise Exception.Create('');
end;

function TFPDocElement.InsertNode(Index: Integer; const AName: String
  ): TFPDocNode;
begin
  raise Exception.Create('');
  Result:=nil;
end;

procedure TFPDocElement.RenameNode(ANode: TFPDocNode; const AName: String);
begin
  raise Exception.Create('');
end;

{ TFPDocTopic }

function TFPDocTopic.GetTopicsCount: Integer;
begin
  Result := FTopics.Count;
end;

function TFPDocTopic.GetTopic(Index: Integer): TFPDocTopic;
begin
  Result := FTopics[Index] as TFPDocTopic;
end;

constructor TFPDocTopic.Create(const AParent: TFPDocItem; const ADOMNode: TDOMNode);
begin
  inherited;
  
  FTopics := TObjectList.Create(True);
  //DebugLn(Name);
  
  ParseTopics;
end;

destructor TFPDocTopic.Destroy;
begin
  FTopics.Free;
  
  inherited Destroy;
end;

procedure TFPDocTopic.ParseTopics;
var
  I: TDOMNode;
  T: TFPDocTopic;
begin
  FTopics.Clear;

  I := FDOMNode.FirstChild;
  while I <> nil do
  begin
    if I.NodeName = SFPDocTopic then
    begin
      T := TFPDocTopic.Create(Self, I);
      FTopics.Add(T);
    end;

    I := I.NextSibling;
  end;
end;

procedure TFPDocTopic.AddTopic(const ATopic: TFPDocTopic);
begin
  AddTopic(ATopic.Name).Assign(ATopic);
end;

procedure TFPDocTopic.Assign(ASource: TFPDocTopic);
var
  I: Integer;
begin
  inherited Assign(ASource);
  
  for I := 0 to ASource.TopicsCount - 1 do
    AddTopic(ASource.Topics[I]);
end;

function TFPDocTopic.GetInfo: TFPDocInfo;
var
  I: Integer;
  Info: TFPDocInfo;
begin
  Result := inherited GetInfo;
  
  if ClassType <> TFPDocTopic then Result.Topics := 0
  else Result.Topics := 1;
  
  
  for I := 0 to TopicsCount - 1 do
  begin
    Info := Topics[I].GetInfo;
    
    Result.Topics := Result.Topics + Info.Topics;
    Result.Shorts := Result.Shorts + Info.Shorts;
    Result.Descriptions := Result.Descriptions + Info.Descriptions;
  end;
end;

procedure TFPDocTopic.Reset;
var
  I: Integer;
begin
  inherited;
  
  for I := 0 to TopicsCount - 1 do Topics[I].Reset;
end;

function TFPDocTopic.AddTopic(const AName: String): TFPDocTopic;
begin
  Result := InsertTopic(TopicsCount, AName);
end;

function TFPDocTopic.InsertTopic(Index: Integer; const AName: String): TFPDocTopic;
var
  N: TDOMElement;
begin
  N := FDOMNode.OwnerDocument.CreateElement(SFPDocTopic);
  N.AttribStrings[SFPDocName] := AName;
  
  if Index < 0 then Index := 0;
  if Index > TopicsCount then Index := TopicsCount;
  if Index < TopicsCount then
    Result := TFPDocTopic.Create(Self, FDOMNode.InsertBefore(N, Topics[Index].FDOMNode))
  else
    Result := TFPDocTopic.Create(Self, FDOMNode.AppendChild(N));
    
  FTopics.Insert(Index, Result);
  Result.Modify;
end;

procedure TFPDocTopic.DeleteNode(ANode: TFPDocNode);
begin
  FDOMNode.RemoveChild(ANode.FDOMNode);
  FTopics.Remove(ANode);
  Modify;
end;

procedure TFPDocTopic.RenameNode(ANode: TFPDocNode; const AName: String);
begin
  (ANode.FDOMNode as TDOMElement).SetAttribute(SFPDocName, AName);
  ANode.FName := AName;
  ANode.Modify;
end;

function TFPDocTopic.AddNode(const AName: String): TFPDocNode;
begin
  raise Exception.Create('');
  Result:=nil;
end;

function TFPDocTopic.InsertNode(Index: Integer; const AName: String
  ): TFPDocNode;
begin
  raise Exception.Create('');
  Result:=nil;
end;

{ TFPDocNodeWithList }

function TFPDocNodeWithList.GetCount: Integer;
begin
  Result := FNodes.Count;
end;

function TFPDocNodeWithList.GetNode(Index: Integer): TFPDocNode;
begin
  Result := FNodes[Index] as TFPDocNode;
end;

function TFPDocNodeWithList.GetNodeByName(const Index: String): TFPDocNode;
var
  I: Integer;
begin
  I := FNames.IndexOf(Index);
  if I = -1 then
    Result := nil
  else
    Result := FNames.Objects[I] as TFPDocNode;
end;

constructor TFPDocNodeWithList.Create(const AParent: TFPDocItem; const ADOMNode: TDOMNode);
begin
  inherited;
  
  FNodes := TObjectList.Create(True);
  FNames := TStringList.Create;
  FNames.Sorted := True;

  ParseNodes;
end;

destructor TFPDocNodeWithList.Destroy;
begin
  FNames.Free;
  FNodes.Free;

  inherited Destroy;
end;

procedure TFPDocNodeWithList.Reset;
var
  I: Integer;
begin
  inherited;
  
  for I := 0 to Count - 1 do Nodes[I].Reset;
end;

function TFPDocNodeWithList.GetUniqueNames: TUniqueNameArray;
var
  I, J: Integer;
  Found: Boolean;
begin
  SetLength(Result, 0);

  for I := 0 to Count - 1 do
  begin
    Found := False;
    for J := 0 to High(Result) do
    begin
      if Result[J].Name = Nodes[I].Name then
      begin
        Found := True;
        
        SetLength(Result[J].Indexes, Length(Result[J].Indexes) + 1);
        Result[J].Indexes[High(Result[J].Indexes)] := I;
        if not Nodes[I].Empty then Inc(Result[High(Result)].NonEmptyCount);
        
        Break;
      end;
    end;

    if not Found then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)].Name := Nodes[I].Name;
      SetLength(Result[High(Result)].Indexes, 1);
      Result[High(Result)].Indexes[0] := I;
      if Nodes[I].Empty then Result[High(Result)].NonEmptyCount := 0
      else Result[High(Result)].NonEmptyCount := 1;
    end;
  end;
end;

function TFPDocNodeWithList.AddNode(const AName: String): TFPDocNode;
begin
  Result := InsertNode(Count, AName);
end;

procedure TFPDocNodeWithList.DeleteNode(ANode: TFPDocNode);
begin
  if ANode is TFPDocTopic then inherited
  else
  begin
    FDOMNode.RemoveChild(ANode.FDOMNode);
    FNames.Delete(FNames.IndexOfObject(ANode));
    FNodes.Remove(ANode);
    Modify;
  end;
end;

procedure TFPDocNodeWithList.RenameNode(ANode: TFPDocNode; const AName: String);
begin
  if not (ANode is TFPDocTopic) then
    FNames.Strings[FNames.IndexOfObject(ANode)] := AName;
    
  inherited;
end;

{ TFPDocModule }

function TFPDocModule.GetElement(Index: Integer): TFPDocElement;
begin
  Result := FNodes[Index] as TFPDocElement;
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

function TFPDocModule.GetInfo: TFPDocInfo;
var
  I: Integer;
  Info: TFPDocInfo;
begin
  Result := inherited GetInfo;

  Result.Modules := 1;

  for I := 0 to Count - 1 do
  begin
    Info := Elements[I].GetInfo;

    Result.Elements := Result.Elements + Info.Elements;
    Result.ElementsNonEmpty := Result.ElementsNonEmpty + Info.ElementsNonEmpty;
    Result.Shorts := Result.Shorts + Info.Shorts;
    Result.Descriptions := Result.Descriptions + Info.Descriptions;
    Result.Errors := Result.Errors + Info.Errors;
    Result.SeeAlsos := Result.SeeAlsos + Info.SeeAlsos;
    Result.Examples := Result.Examples + Info.Examples;
  end;
end;

procedure TFPDocModule.ParseNodes;
var
  I: TDOMNode;
  N: TFPDocElement;
begin
  FNodes.Clear;
  FNames.Clear;

  I := FDOMNode.FirstChild;
  while I <> nil do
  begin
    if I.NodeName = SFPDocElement then
    begin
      N := TFPDocElement.Create(Self, I);
      FNodes.Add(N);
      FNames.AddObject(N.Name, N);
    end;

    I := I.NextSibling;
  end;
end;

function TFPDocModule.InsertNode(Index: Integer; const AName: String): TFPDocNode;
var
  N: TDOMElement;
begin
  N := FDOMNode.OwnerDocument.CreateElement(SFPDocElement);
  N.AttribStrings[SFPDocName] := AName;

  if Index < 0 then Index := 0;
  if Index > Count then Index := Count;
  if Index < Count then
    Result := TFPDocElement.Create(Self, FDOMNode.InsertBefore(N, Elements[Index].FDOMNode))
  else
    if TopicsCount = 0 then
      Result := TFPDocElement.Create(Self, FDOMNode.AppendChild(N))
    else
      Result := TFPDocElement.Create(Self, FDOMNode.InsertBefore(N, Topics[0].FDOMNode));

  FNodes.Insert(Index, Result);
  FNames.AddObject(AName, Result);
  Result.Modify;
end;


{ TFPDocPackage }

function TFPDocPackage.GetModule(Index: Integer): TFPDocModule;
begin
  Result := FNodes[Index] as TFPDocModule;
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

function TFPDocPackage.GetInfo: TFPDocInfo;
var
  I: Integer;
  Info: TFPDocInfo;
begin
  Result := inherited GetInfo;

  Result.Packages := 1;

  for I := 0 to Count - 1 do
  begin
    Info := Modules[I].GetInfo;

    Result.Modules := Result.Modules + Info.Modules;
    Result.Elements := Result.Elements + Info.Elements;
    Result.ElementsNonEmpty := Result.ElementsNonEmpty + Info.ElementsNonEmpty;
    Result.Shorts := Result.Shorts + Info.Shorts;
    Result.Descriptions := Result.Descriptions + Info.Descriptions;
    Result.Errors := Result.Errors + Info.Errors;
    Result.SeeAlsos := Result.SeeAlsos + Info.SeeAlsos;
    Result.Examples := Result.Examples + Info.Examples;
  end;
end;

procedure TFPDocPackage.ParseNodes;
var
  I: TDOMNode;
  N: TFPDocModule;
begin
  FNodes.Clear;
  FNames.Clear;

  I := FDOMNode.FirstChild;
  while I <> nil do
  begin
    if I.NodeName = SFPDocModule then
    begin
      N := TFPDocModule.Create(Self, I);
      FNodes.Add(N);
      FNames.AddObject(N.Name, N);
    end;

    I := I.NextSibling;
  end;
end;

function TFPDocPackage.InsertNode(Index: Integer; const AName: String
  ): TFPDocNode;
var
  N: TDOMElement;
begin
  N := FDOMNode.OwnerDocument.CreateElement(SFPDocModule);
  N.AttribStrings[SFPDocName] := AName;

  if Index < 0 then Index := 0;
  if Index > Count then Index := Count;
  if Index < Count then
    Result := TFPDocModule.Create(Self, FDOMNode.InsertBefore(N, Modules[Index].FDOMNode))
  else
    if TopicsCount = 0 then
      Result := TFPDocModule.Create(Self, FDOMNode.AppendChild(N))
    else
      Result := TFPDocModule.Create(Self, FDOMNode.InsertBefore(N, Topics[0].FDOMNode));

  FNodes.Insert(Index, Result);
  FNames.AddObject(AName, Result);
  Result.Modify;
end;

{ TFPDocFile }

function TFPDocFile.GetPackage(Index: Integer): TFPDocPackage;
begin
  Result := Nodes[Index] as TFPDocPackage;
end;

function TFPDocFile.GetPackageByName(const Index: String): TFPDocPackage;
begin
  Result := NodesByName[Index] as TFPDocPackage;
end;

constructor TFPDocFile.Create(const FileName: String);
var
  F: TFileStream;
begin
  F := TFileStream.Create(UTF8ToSys(FileName), fmOpenRead);
  try
    Create(F);
  finally
    F.Free;
  end;
end;

constructor TFPDocFile.Create(Stream: TStream);
var
  R: TDOMNode;
begin
  ReadXMLFile(FDocument, Stream);

  R := FDocument.FindNode(SFPDocHeader);
  if R = nil then
    raise Exception.Create('Invalid FPDoc file!');
    
  inherited Create(nil, R);
end;

destructor TFPDocFile.Destroy;
begin
  FDocument.Free;

  inherited Destroy;
end;

procedure TFPDocFile.ParseNodes;
var
  I: TDOMNode;
  P: TFPDocPackage;
begin
  FNodes.Clear;
  FNames.Clear;
  
  I := FDOMNode.FirstChild;
  while I <> nil do
  begin
    if I.NodeName = SFPDocPackage then
    begin
      P := TFPDocPackage.Create(Self, I);
      FNodes.Add(P);
      FNames.AddObject(P.Name, P);
    end;
    
    I := I.NextSibling;
  end;
end;

procedure TFPDocFile.SaveToFile(const FileName: String);
begin
  WriteXMLFile(FDocument, FileName);
  Reset;
end;

procedure TFPDocFile.AssignToSkeleton(const SkeletonFile: TFPDocFile;
  OnMoveElement: TMoveElementEvent);
var
  I, J, K, L, M: Integer;
  P1, P2: TFPDocPackage;
  M1, M2: TFPDocModule;
  E1, E2: TFPDocElement;
  Elements1, Elements2: TUniqueNameArray;
  DestList: TStringList;
  
  procedure AskMove;
  var
    N: Integer;
  begin
    N := M;
    OnMoveElement(P1, M1, E1, DestList, N);
    if N <> -1 then
    begin
      M2.Elements[N].Assign(E1);
      if not M2.Elements[N].Empty then
        DestList.Objects[N] := TObject(1);
    end;
  end;
  
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
          DestList.Add(E2.Name);
        end;
        
        Elements1 := M1.GetUniqueNames;
        Elements2 := M2.GetUniqueNames;
        
        for K := 0 to High(Elements1) do
        begin
          if Elements1[K].NonEmptyCount = 0 then Continue;

          M := -1;
          for L := 0 to High(Elements2) do
          begin
            if Elements1[K].Name = Elements2[L].Name then
            begin
              M := L;
            end;
          end;

          for L := 0 to High(Elements1[K].Indexes) do
          begin
            E1 := M1.Elements[Elements1[K].Indexes[L]];
            if E1.Empty then Continue;
            
            if M = -1 then AskMove
            else
              if L > High(Elements2[M].Indexes) then AskMove
              else
              begin
                E2 := M2.Elements[Elements2[M].Indexes[L]];
                if not E2.Empty then AskMove
                else
                begin
                  E2.Assign(E1);
                  
                  if not E2.Empty then
                    DestList.Objects[Elements2[M].Indexes[L]] := TObject(1);
                end;
              end;
          end;
        end;
      finally
        DestList.Free;
      end;
    end;
  end;
end;

function TFPDocFile.GetInfo: TFPDocInfo;
var
  I: Integer;
  Info: TFPDocInfo;
begin
  Result := EmptyFPDocInfo;

  for I := 0 to Count - 1 do
  begin
    Info := Packages[I].GetInfo;

    Result.Packages := Result.Packages + Info.Packages;
    Result.Modules := Result.Modules + Info.Modules;
    Result.Elements := Result.Elements + Info.Elements;
    Result.ElementsNonEmpty := Result.ElementsNonEmpty + Info.ElementsNonEmpty;
    Result.Shorts := Result.Shorts + Info.Shorts;
    Result.Descriptions := Result.Descriptions + Info.Descriptions;
    Result.Errors := Result.Errors + Info.Errors;
    Result.SeeAlsos := Result.SeeAlsos + Info.SeeAlsos;
    Result.Examples := Result.Examples + Info.Examples;
  end;
end;

function TFPDocFile.InsertNode(Index: Integer; const AName: String): TFPDocNode;
var
  N: TDOMElement;
begin
  N := FDocument.CreateElement(SFPDocPackage);
  N.AttribStrings[SFPDocName] := AName;

  if Index < 0 then Index := 0;
  if Index > Count then Index := Count;
  if Index < Count then
    Result := TFPDocPackage.Create(Self, FDocument.InsertBefore(N, Packages[Index].FDOMNode))
  else
    Result := TFPDocPackage.Create(Self, FDocument.AppendChild(N));

  FNodes.Insert(Index, Result);
  FNames.AddObject(AName, Result);
  Result.Modify;
end;



end.

