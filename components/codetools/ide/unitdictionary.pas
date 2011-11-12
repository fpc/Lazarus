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
  Classes, SysUtils, AVL_Tree, BasicCodeTools, FileProcs,
  FindDeclarationCache, CodeToolManager;

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
    FirstIdentifier, LastIdentifier: TUDIdentifier;
    UnitGroups: TAVLTree; // tree of TUDUnitGroup sorted with CompareIDItems
    constructor Create(const aName, aFilename: string);
    destructor Destroy; override;
    function AddIdentifier(Item: TUDIdentifier): TUDIdentifier;
    procedure ClearIdentifiers;
    function IsInGroup(Group: TUDUnitGroup): boolean;
    function GetDictionary: TUnitDictionary;
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
    function AddUnitGroup(Group: TUDUnitGroup): TUDUnitGroup; overload;
    function AddUnitGroup(const aName, aFilename: string): TUDUnitGroup; overload;
    property DefaultGroup: TUDUnitGroup read FDefaultGroup;
    procedure ParseUnit(Tool: TCodeTool; Group: TUDUnitGroup = nil);
    function FindUnitWithFilename(const aFilename: string): TUDUnit;
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
  Changed: Boolean;
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
  end else begin
    // new unit
    CurUnit:=Group.AddUnit(NiceName,UnitFilename);
    FUnitsByName.Add(CurUnit);
    FUnitsByFilename.Add(CurUnit);
  end;

  SrcTree:=Tool.InterfaceIdentifierCache.Items;
  // check if something changed
  AVLNode:=SrcTree.FindLowest;
  Changed:=false;
  IdentifierItem:=CurUnit.FirstIdentifier;
  while AVLNode<>nil do begin
    Item:=PInterfaceIdentCacheEntry(AVLNode.Data);
    if (Item^.Node<>nil) and (Item^.Identifier<>nil) then begin
      if (IdentifierItem=nil)
      or (CompareIdentifiers(Item^.Identifier,PChar(Pointer(IdentifierItem.Name)))<>0)
      then begin
        Changed:=true;
        break;
      end;
      IdentifierItem:=IdentifierItem.NextInUnit;
    end;
    AVLNode:=SrcTree.FindSuccessor(AVLNode);
  end;
  if IdentifierItem<>nil then
    Changed:=true; // the old list had more identifiers
  if Changed then begin
    // list of identifiers has changed => rebuild
    CurUnit.ClearIdentifiers;
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

