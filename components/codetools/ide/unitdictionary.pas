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

  Author: Mattias Gaertner

  Abstract:
    Quick lookup database for identifiers in units.
}
unit unitdictionary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AVL_Tree, BasicCodeTools, FileProcs, CodeToolsStructs,
  FindDeclarationCache, CodeToolManager, CodeCache;

const
  UDFileVersion = 1;
type
  TUDIdentifier = class;
  TUDUnit = class;
  TUnitDictionary = class;

  { TUDItem }

  TUDItem = class
  public
    Name: string;
  end;

  { TUDFileItem }

  TUDFileItem = class(TUDItem)
  public
    Filename: string;
    constructor Create(const aName, aFilename: string);
  end;

  { TUDUnitGroup }

  TUDUnitGroup = class(TUDFileItem)
  public
    Dictionary: TUnitDictionary;
    Units: TAVLTree; // tree of TIDUnit sorted with CompareIDItems
    constructor Create(const aName, aFilename: string);
    destructor Destroy; override;
    function AddUnit(NewUnit: TUDUnit): TUDUnit; overload;
    function AddUnit(const aName, aFilename: string): TUDUnit; overload;
    procedure RemoveUnit(TheUnit: TUDUnit);
  end;

  { TUDUnit }

  TUDUnit = class(TUDFileItem)
  public
    FileAge: longint;
    ToolStamp: integer;
    FirstIdentifier, LastIdentifier: TUDIdentifier;
    UnitGroups: TAVLTree; // tree of TUDUnitGroup sorted with CompareIDItems
    constructor Create(const aName, aFilename: string);
    destructor Destroy; override;
    function AddIdentifier(Item: TUDIdentifier): TUDIdentifier;
    procedure ClearIdentifiers;
    function IsInGroup(Group: TUDUnitGroup): boolean;
    function GetDictionary: TUnitDictionary;
    function HasIdentifier(Item: TUDIdentifier): boolean;
  end;

  { TUDIdentifier }

  TUDIdentifier = class(TUDItem)
  public
    DUnit: TUDUnit;
    NextInUnit: TUDIdentifier;
    constructor Create(const aName: string); overload;
    constructor Create(aName: PChar); overload;
  end;

  { TUnitDictionary }

  TUnitDictionary = class
  private
    FDefaultGroup: TUDUnitGroup;
    FIdentifiers: TAVLTree; // tree of TUDIdentifier sorted with CompareIDItems
    FUnitsByName: TAVLTree; // tree of TUDUnit sorted with CompareIDItems
    FUnitsByFilename: TAVLTree; // tree of TUDUnit sorted with CompareIDFileItems
    FUnitGroupsByName: TAVLTree; // tree of TUDUnitGroup sorted with CompareIDItems
    FUnitGroupsByFilename: TAVLTree; // tree of TUDUnitGroup sorted with CompareIDFileItems
    procedure RemoveIdentifier(Item: TUDIdentifier);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear(CreateDefaults: boolean = true);
    procedure ConsistencyCheck;
    procedure SaveToFile(Filename: string);
    procedure SaveToStream(aStream: TStream);

    // groups
    function AddUnitGroup(Group: TUDUnitGroup): TUDUnitGroup; overload;
    function AddUnitGroup(const aName, aFilename: string): TUDUnitGroup; overload;
    property DefaultGroup: TUDUnitGroup read FDefaultGroup;
    property UnitGroupsByName: TAVLTree read FUnitGroupsByName;
    property UnitGroupsByFilename: TAVLTree read FUnitGroupsByFilename;

    // units
    procedure ParseUnit(UnitFilename: string; Group: TUDUnitGroup = nil); overload;
    procedure ParseUnit(Code: TCodeBuffer; Group: TUDUnitGroup = nil); overload;
    procedure ParseUnit(Tool: TCodeTool; Group: TUDUnitGroup = nil); overload;
    function FindUnitWithFilename(const aFilename: string): TUDUnit;
    property UnitsByName: TAVLTree read FUnitsByName;
    property UnitsByFilename: TAVLTree read FUnitsByFilename;

    // identifiers
    property Identifiers: TAVLTree read FIdentifiers;
  end;

function CompareNameWithIDItem(NamePChar, Item: Pointer): integer;
function CompareIDItems(Item1, Item2: Pointer): integer;
function CompareFileNameWithIDFileItem(NameAnsiString, Item: Pointer): integer;
function CompareIDFileItems(Item1, Item2: Pointer): integer;

procedure IDCheckUnitNameAndFilename(const aName, aFilename: string);

implementation

function CompareNameWithIDItem(NamePChar, Item: Pointer): integer;
var
  i: TUDItem absolute Item;
begin
  Result:=CompareDottedIdentifiers(PChar(NamePChar),PChar(Pointer(i.Name)));
end;

function CompareIDItems(Item1, Item2: Pointer): integer;
var
  i1: TUDItem absolute Item1;
  i2: TUDItem absolute Item2;
begin
  Result:=CompareDottedIdentifiers(PChar(Pointer(i1.Name)),PChar(Pointer(i2.Name)));
end;

function CompareFileNameWithIDFileItem(NameAnsiString, Item: Pointer): integer;
var
  i: TUDFileItem absolute Item;
begin
  Result:=CompareFilenames(AnsiString(NameAnsiString),i.Filename);
end;

function CompareIDFileItems(Item1, Item2: Pointer): integer;
var
  i1: TUDFileItem absolute Item1;
  i2: TUDFileItem absolute Item2;
begin
  Result:=CompareFilenames(i1.Filename,i2.Filename);
end;

procedure IDCheckUnitNameAndFilename(const aName, aFilename: string);

  procedure InvalidName;
  begin
    raise Exception.Create('invalid UnitName="'+aName+'" Filename="'+aFilename+'"');
  end;

var
  ShortName: String;
begin
  ShortName:=ExtractFileNameOnly(aFilename);
  if CompareDottedIdentifiers(PChar(Pointer(aName)),PChar(Pointer(ShortName)))<>0
  then
    InvalidName;
end;

{ TUDIdentifier }

constructor TUDIdentifier.Create(const aName: string);
begin
  Name:=aName;
end;

constructor TUDIdentifier.Create(aName: PChar);
begin
  Name:=GetIdentifier(aName);
end;

constructor TUDUnit.Create(const aName, aFilename: string);
begin
  ToolStamp:=CTInvalidChangeStamp;
  IDCheckUnitNameAndFilename(aName,aFilename);
  inherited Create(aName,aFilename);
  UnitGroups:=TAVLTree.Create(@CompareIDItems);
end;

destructor TUDUnit.Destroy;
begin
  // the groups are freed by the TUnitDictionary
  FreeAndNil(UnitGroups);
  inherited Destroy;
end;

function TUDUnit.AddIdentifier(Item: TUDIdentifier): TUDIdentifier;
begin
  Result:=Item;
  Result.DUnit:=Self;
  if LastIdentifier<>nil then
    LastIdentifier.NextInUnit:=Result
  else
    FirstIdentifier:=Result;
  Result.NextInUnit:=nil;
  LastIdentifier:=Result;
end;

procedure TUDUnit.ClearIdentifiers;
var
  Item: TUDIdentifier;
  Dictionary: TUnitDictionary;
begin
  Dictionary:=GetDictionary;
  while FirstIdentifier<>nil do begin
    Item:=FirstIdentifier;
    FirstIdentifier:=Item.NextInUnit;
    Item.NextInUnit:=nil;
    Dictionary.RemoveIdentifier(Item);
    Item.Free;
  end;
  LastIdentifier:=nil;
end;

function TUDUnit.IsInGroup(Group: TUDUnitGroup): boolean;
begin
  Result:=UnitGroups.FindPointer(Group)<>nil;
end;

function TUDUnit.GetDictionary: TUnitDictionary;
begin
  Result:=TUDUnitGroup(UnitGroups.Root.Data).Dictionary;
end;

function TUDUnit.HasIdentifier(Item: TUDIdentifier): boolean;
var
  i: TUDIdentifier;
begin
  i:=FirstIdentifier;
  while i<>nil do begin
    if i=Item then exit(true);
    i:=i.NextInUnit;
  end;
  Result:=false;
end;

{ TUDUnitGroup }

constructor TUDUnitGroup.Create(const aName, aFilename: string);
begin
  IDCheckUnitNameAndFilename(aName,aFilename);
  inherited Create(aName,aFilename);
  Units:=TAVLTree.Create(@CompareIDItems);
end;

destructor TUDUnitGroup.Destroy;
begin
  // the units are freed by the TIdentifierDictionary
  FreeAndNil(Units);
  inherited Destroy;
end;

function TUDUnitGroup.AddUnit(NewUnit: TUDUnit): TUDUnit;
begin
  Result:=NewUnit;
  if Units.FindPointer(NewUnit)<>nil then exit;
  Units.Add(Result);
  Result.UnitGroups.Add(Self);
end;

function TUDUnitGroup.AddUnit(const aName, aFilename: string): TUDUnit;
begin
  Result:=AddUnit(TUDUnit.Create(aName,aFilename));
end;

procedure TUDUnitGroup.RemoveUnit(TheUnit: TUDUnit);
begin
  Units.RemovePointer(TheUnit);
  TheUnit.UnitGroups.RemovePointer(Self);
end;

{ TUDFileItem }

constructor TUDFileItem.Create(const aName, aFilename: string);
begin
  Name:=aName;
  Filename:=aFilename;
end;

{ TUnitDictionary }

procedure TUnitDictionary.RemoveIdentifier(Item: TUDIdentifier);
begin
  FIdentifiers.RemovePointer(Item);
end;

constructor TUnitDictionary.Create;
begin
  FIdentifiers:=TAVLTree.Create(@CompareIDItems);
  FUnitsByName:=TAVLTree.Create(@CompareIDItems);
  FUnitsByFilename:=TAVLTree.Create(@CompareIDFileItems);
  FUnitGroupsByName:=TAVLTree.Create(@CompareIDItems);
  FUnitGroupsByFilename:=TAVLTree.Create(@CompareIDFileItems);
  FDefaultGroup:=AddUnitGroup('','');
end;

destructor TUnitDictionary.Destroy;
begin
  Clear(false);
  FreeAndNil(FIdentifiers);
  FreeAndNil(FUnitsByName);
  FreeAndNil(FUnitsByFilename);
  FreeAndNil(FUnitGroupsByName);
  FreeAndNil(FUnitGroupsByFilename);
  inherited Destroy;
end;

procedure TUnitDictionary.Clear(CreateDefaults: boolean);
begin
  FDefaultGroup:=nil;
  FUnitGroupsByFilename.Clear;
  FUnitGroupsByName.FreeAndClear;
  FUnitsByFilename.Clear;
  FUnitsByName.FreeAndClear;
  FIdentifiers.FreeAndClear;
  if CreateDefaults then
    FDefaultGroup:=AddUnitGroup('','');
end;

procedure TUnitDictionary.ConsistencyCheck;

  procedure e(const Msg: string);
  begin
    raise Exception.Create('ERROR: TUnitDictionary.ConsistencyCheck '+Msg);
  end;

var
  AVLNode: TAVLTreeNode;
  CurUnit: TUDUnit;
  Group: TUDUnitGroup;
  Item: TUDIdentifier;
  SubAVLNode: TAVLTreeNode;
begin
  if DefaultGroup=nil then
    e('DefaultGroup=nil');

  // check FUnitsByName
  AVLNode:=FUnitsByName.FindLowest;
  while AVLNode<>nil do begin
    CurUnit:=TUDUnit(AVLNode.Data);
    if CurUnit.Name='' then
      e('unit without name');
    if CurUnit.Filename='' then
      e('unit '+CurUnit.Name+' without filename');
    if FUnitsByFilename.FindPointer(CurUnit)=nil then
      e('unit '+CurUnit.Name+' in FUnitsByName not in FUnitsByFilename');
    if CurUnit.UnitGroups.Count=0 then
      e('unit '+CurUnit.Name+' has not group');
    SubAVLNode:=CurUnit.UnitGroups.FindLowest;
    while SubAVLNode<>nil do begin
      Group:=TUDUnitGroup(SubAVLNode.Data);
      if Group.Units.FindPointer(CurUnit)=nil then
        e('unit '+CurUnit.Name+' not in group '+Group.Name);
      SubAVLNode:=CurUnit.UnitGroups.FindSuccessor(SubAVLNode);
    end;
    AVLNode:=FUnitsByName.FindSuccessor(AVLNode);
  end;

  // FUnitsByFilename
  AVLNode:=FUnitsByFilename.FindLowest;
  while AVLNode<>nil do begin
    CurUnit:=TUDUnit(AVLNode.Data);
    if FUnitsByName.FindPointer(CurUnit)=nil then
      e('unit '+CurUnit.Name+' in FUnitsByFilename not in FUnitsByName');
    AVLNode:=FUnitsByFilename.FindSuccessor(AVLNode);
  end;

  // check FUnitGroupsByName
  AVLNode:=FUnitGroupsByName.FindLowest;
  while AVLNode<>nil do begin
    Group:=TUDUnitGroup(AVLNode.Data);
    if (Group.Name='') and (Group<>DefaultGroup) then
      e('group without name');
    if (Group.Filename='') and (Group<>DefaultGroup) then
      e('group '+Group.Name+' without filename');
    if FUnitGroupsByFilename.FindPointer(Group)=nil then
      e('group '+Group.Name+' in FUnitGroupsByName not in FUnitGroupsByFilename');
    SubAVLNode:=Group.Units.FindLowest;
    while SubAVLNode<>nil do begin
      CurUnit:=TUDUnit(SubAVLNode.Data);
      if CurUnit.UnitGroups.FindPointer(Group)=nil then
        e('group '+Group.Name+' has not the unit '+CurUnit.Name);
      SubAVLNode:=Group.Units.FindSuccessor(SubAVLNode);
    end;
    AVLNode:=FUnitGroupsByName.FindSuccessor(AVLNode);
  end;

  // FUnitGroupsByFilename
  AVLNode:=FUnitGroupsByFilename.FindLowest;
  while AVLNode<>nil do begin
    Group:=TUDUnitGroup(AVLNode.Data);
    if FUnitGroupsByName.FindPointer(Group)=nil then
      e('group '+Group.Name+' in FUnitGroupsByFilename not in FUnitGroupsByName');
    AVLNode:=FUnitGroupsByFilename.FindSuccessor(AVLNode);
  end;

  // FIdentifiers
  AVLNode:=FIdentifiers.FindLowest;
  while AVLNode<>nil do begin
    Item:=TUDIdentifier(AVLNode.Data);
    if Item.Name='' then
      e('identifier without name');
    if Item.DUnit=nil then
      e('identifier '+Item.Name+' without unit');
    if not Item.DUnit.HasIdentifier(Item) then
      e('identifier '+Item.Name+' not in unit '+Item.DUnit.Name);
    AVLNode:=FIdentifiers.FindSuccessor(AVLNode);
  end;

end;

procedure TUnitDictionary.SaveToFile(Filename: string);
var
  ms: TMemoryStream;
begin
  ms:=TMemoryStream.Create;
  try
    SaveToStream(ms);
    ms.Position:=0;
    ms.SaveToFile(Filename);
  finally
    ms.Free;
  end;
end;

procedure TUnitDictionary.SaveToStream(aStream: TStream);

  procedure w(const s: string);
  begin
    if s='' then exit;
    aStream.Write(s[1],length(s));
  end;

  function GetBase32(i: integer): string;
  const
    l: shortstring = '0123456789ABCDEFGHIJKLMNOPQRSTUV';
  begin
    Result:='';
    while i>0 do begin
      Result:=Result+l[(i mod 32)+1];
      i:=i div 32;
    end;
  end;

var
  AVLNode: TAVLTreeNode;
  CurUnit: TUDUnit;
  Item: TUDIdentifier;
  Group: TUDUnitGroup;
  SubAVLNode: TAVLTreeNode;
  UnitID: TFilenameToStringTree;
  i: Integer;
begin
  // write format version
  w('UnitDirectory:');
  w(IntToStr(UDFileVersion));
  w(LineEnding);

  UnitID:=TFilenameToStringTree.Create(false);
  try
    // write units
    i:=0;
    AVLNode:=FUnitsByFilename.FindLowest;
    while AVLNode<>nil do begin
      CurUnit:=TUDUnit(AVLNode.Data);
      inc(i);
      UnitID.Add(CurUnit.Filename,GetBase32(i));
      AVLNode:=FUnitsByFilename.FindSuccessor(AVLNode);
    end;

    // write groups
    AVLNode:=FUnitGroupsByFilename.FindLowest;
    while AVLNode<>nil do begin
      Group:=TUDUnitGroup(AVLNode.Data);
      // write group name
      w(Group.Name);
      w(';');
      w(Group.Filename);
      w(LineEnding);
      // write IDs of units
      SubAVLNode:=Group.Units.FindLowest;
      while SubAVLNode<>nil do begin
        CurUnit:=TUDUnit(SubAVLNode.Data);
        w(UnitID[CurUnit.Filename]);
        w(LineEnding);
        SubAVLNode:=Group.Units.FindSuccessor(SubAVLNode);
      end;
      w(LineEnding); // empty line as end of group
      AVLNode:=FUnitGroupsByFilename.FindSuccessor(AVLNode);
    end;

    // write units
    AVLNode:=FUnitsByFilename.FindLowest;
    while AVLNode<>nil do begin
      CurUnit:=TUDUnit(AVLNode.Data);
      // write unit number ; unit name ; unit file name
      w(UnitID[CurUnit.Filename]);
      w(';');
      w(CurUnit.Name);
      w(';');
      w(CurUnit.Filename);
      w(LineEnding);
      // write identifiers
      Item:=CurUnit.FirstIdentifier;
      while Item<>nil do begin
        if Item.Name<>'' then begin
          w(Item.Name);
          w(LineEnding);
        end;
        Item:=Item.NextInUnit;
      end;
      w(LineEnding); // empty line as end of unit
      AVLNode:=FUnitsByFilename.FindSuccessor(AVLNode);
    end;


  finally
    UnitID.Free;
  end;
end;

function TUnitDictionary.AddUnitGroup(Group: TUDUnitGroup): TUDUnitGroup;
begin
  if Group.Dictionary<>nil then
    raise Exception.Create('TIdentifierDictionary.AddUnitGroup Group.Dictionary<>nil');
  Result:=Group;
  Result.Dictionary:=Self;
  FUnitGroupsByName.Add(Result);
  FUnitGroupsByFilename.Add(Result);
end;

function TUnitDictionary.AddUnitGroup(const aName, aFilename: string
  ): TUDUnitGroup;
begin
  Result:=AddUnitGroup(TUDUnitGroup.Create(aName,aFilename));
end;

procedure TUnitDictionary.ParseUnit(UnitFilename: string; Group: TUDUnitGroup);
var
  Code: TCodeBuffer;
begin
  UnitFilename:=TrimFilename(UnitFilename);
  if UnitFilename='' then exit;
  Code:=CodeToolBoss.LoadFile(UnitFilename,true,false);
  if Code=nil then
    raise Exception.Create('unable to load file '+UnitFilename);
  ParseUnit(Code,Group);
end;

procedure TUnitDictionary.ParseUnit(Code: TCodeBuffer; Group: TUDUnitGroup);
begin
  if Code=nil then exit;
  if not CodeToolBoss.InitCurCodeTool(Code) then
    raise Exception.Create('unable to init unit parser for file '+Code.Filename);
  ParseUnit(CodeToolBoss.CurCodeTool,Group);
end;

procedure TUnitDictionary.ParseUnit(Tool: TCodeTool; Group: TUDUnitGroup);
var
  SrcTree: TAVLTree;
  AVLNode: TAVLTreeNode;
  Item: PInterfaceIdentCacheEntry;
  UnitFilename: String;
  CurUnit: TUDUnit;
  NiceName: String;
  SrcName: String;
  IdentifierItem: TUDIdentifier;
begin
  if Tool=nil then exit;
  if Group=nil then
    Group:=DefaultGroup;
  // parse unit
  Tool.BuildInterfaceIdentifierCache(true);

  // get unit name from source
  UnitFilename:=Tool.MainFilename;
  NiceName:=ExtractFileNameOnly(UnitFilename);
  if (LowerCase(NiceName)=NiceName)
  or (UpperCase(NiceName)=NiceName) then begin
    SrcName:=Tool.GetSourceName(false);
    if CompareDottedIdentifiers(PChar(SrcName),PChar(NiceName))=0 then
      NiceName:=SrcName;
  end;

  // find/create unit
  CurUnit:=FindUnitWithFilename(UnitFilename);
  if CurUnit<>nil then begin
    // old unit
    if (Group<>DefaultGroup) then begin
      if CurUnit.IsInGroup(DefaultGroup) then begin
        // move from no group to some group
        DefaultGroup.RemoveUnit(CurUnit);
      end;
      Group.AddUnit(CurUnit);
    end;
    // update name
    if CurUnit.Name<>NiceName then
      CurUnit.Name:=NiceName;
    if CurUnit.ToolStamp=Tool.TreeChangeStep then begin
      // nothing changed since last parsing
      exit;
    end;
    CurUnit.ToolStamp:=Tool.TreeChangeStep;
  end else begin
    // new unit
    CurUnit:=Group.AddUnit(NiceName,UnitFilename);
    FUnitsByName.Add(CurUnit);
    FUnitsByFilename.Add(CurUnit);
  end;

  // rebuild list of identifiers
  CurUnit.ClearIdentifiers;
  SrcTree:=Tool.InterfaceIdentifierCache.Items;
  AVLNode:=SrcTree.FindLowest;
  while AVLNode<>nil do begin
    Item:=PInterfaceIdentCacheEntry(AVLNode.Data);
    if (Item^.Node<>nil) and (Item^.Identifier<>nil) then begin
      IdentifierItem:=TUDIdentifier.Create(Item^.Identifier);
      CurUnit.AddIdentifier(IdentifierItem);
      FIdentifiers.Add(IdentifierItem);
    end;
    AVLNode:=SrcTree.FindSuccessor(AVLNode);
  end;
end;

function TUnitDictionary.FindUnitWithFilename(const aFilename: string
  ): TUDUnit;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=FUnitsByFilename.FindKey(Pointer(aFilename),@CompareFileNameWithIDFileItem);
  if AVLNode<>nil then
    Result:=TUDUnit(AVLNode.Data)
  else
    Result:=nil;
end;

end.

