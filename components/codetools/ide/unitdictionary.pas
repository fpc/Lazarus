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
  Classes, SysUtils, AVL_Tree, BasicCodeTools, FileProcs;

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
  end;

  { TUDUnit }

  TUDUnit = class(TUDFileItem)
  public
    FileAge: longint;
    FirstIdentifier: TUDIdentifier;
    UnitGroups: TAVLTree; // tree of TUDUnitGroup sorted with CompareIDItems
    constructor Create(const aName, aFilename: string);
    destructor Destroy; override;
    function AddIdentifier(aName: PChar): TUDIdentifier;
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
    FIdentifiers: TAVLTree; // tree of TUDIdentifier sorted with CompareIDItems
    FUnitsByName: TAVLTree; // tree of TUDUnit sorted with CompareIDItems
    FUnitsByFilename: TAVLTree; // tree of TUDUnit sorted with CompareIDFileItems
    FUnitGroupsByName: TAVLTree; // tree of TUDUnitGroup sorted with CompareIDItems
    FUnitGroupsByFilename: TAVLTree; // tree of TUDUnitGroup sorted with CompareIDFileItems
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddUnitGroup(Group: TUDUnitGroup): TUDUnitGroup; overload;
    function AddUnitGroup(const aName, aFilename: string): TUDUnitGroup; overload;
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

function TUDUnit.AddIdentifier(aName: PChar): TUDIdentifier;
begin
  Result:=TUDIdentifier.Create(aName);
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
  Units.Add(Result);
end;

function TUDUnitGroup.AddUnit(const aName, aFilename: string): TUDUnit;
begin
  Result:=AddUnit(TUDUnit.Create(aName,aFilename));
end;

{ TUDFileItem }

constructor TUDFileItem.Create(const aName, aFilename: string);
begin
  Name:=aName;
  Filename:=aFilename;
end;

{ TUnitDictionary }

constructor TUnitDictionary.Create;
begin
  FIdentifiers:=TAVLTree.Create(@CompareIDItems);
  FUnitsByName:=TAVLTree.Create(@CompareIDItems);
  FUnitsByFilename:=TAVLTree.Create(@CompareIDFileItems);
  FUnitGroupsByName:=TAVLTree.Create(@CompareIDItems);
  FUnitGroupsByFilename:=TAVLTree.Create(@CompareIDFileItems);
end;

destructor TUnitDictionary.Destroy;
begin
  Clear;
  FreeAndNil(FIdentifiers);
  FreeAndNil(FUnitsByName);
  FreeAndNil(FUnitsByFilename);
  FreeAndNil(FUnitGroupsByName);
  FreeAndNil(FUnitGroupsByFilename);
  inherited Destroy;
end;

procedure TUnitDictionary.Clear;
begin
  FUnitGroupsByFilename.Clear;
  FUnitGroupsByName.FreeAndClear;
  FUnitsByFilename.Clear;
  FUnitsByName.FreeAndClear;
  FIdentifiers.FreeAndClear;
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

end.

