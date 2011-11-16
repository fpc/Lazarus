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
unit UnitDictionary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AVL_Tree, BasicCodeTools, FileProcs, CodeToolsStructs,
  FindDeclarationCache, CodeToolManager, CodeCache, zstream;

const
  UDFileVersion = 1;
  UDFileHeader = 'UnitDirectory:';
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
    FNoGroup: TUDUnitGroup;
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
    procedure SaveToFile(const Filename: string; Compress: boolean = false);
    procedure SaveToStream(aStream: TStream);
    procedure LoadFromFile(const Filename: string);
    procedure LoadFromStream(aStream: TMemoryStream);
    function Equals(Dictionary: TUnitDictionary): boolean; reintroduce;

    // groups
    function AddUnitGroup(Group: TUDUnitGroup): TUDUnitGroup; overload;
    function AddUnitGroup(const aName, aFilename: string): TUDUnitGroup; overload;
    property NoGroup: TUDUnitGroup read FNoGroup;
    property UnitGroupsByName: TAVLTree read FUnitGroupsByName;
    property UnitGroupsByFilename: TAVLTree read FUnitGroupsByFilename;
    function FindGroupWithFilename(const aFilename: string): TUDUnitGroup;

    // units
    function AddUnit(const aName, aFilename: string; Group: TUDUnitGroup = nil): TUDUnit; overload;
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
  FNoGroup:=AddUnitGroup('','');
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
  FNoGroup:=nil;
  FUnitGroupsByFilename.Clear;
  FUnitGroupsByName.FreeAndClear;
  FUnitsByFilename.Clear;
  FUnitsByName.FreeAndClear;
  FIdentifiers.FreeAndClear;
  if CreateDefaults then
    FNoGroup:=AddUnitGroup('','');
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
  if NoGroup=nil then
    e('DefaultGroup=nil');

  if UnitGroupsByFilename.Count<>UnitGroupsByName.Count then
    e('UnitGroupsByFilename.Count<>UnitGroupsByName.Count');
  if UnitsByFilename.Count<>UnitsByName.Count then
    e('UnitsByFilename.Count<>UnitsByName.Count');

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
    if (Group.Name='') and (Group<>NoGroup) then
      e('group without name');
    if (Group.Filename='') and (Group<>NoGroup) then
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

procedure TUnitDictionary.SaveToFile(const Filename: string; Compress: boolean);
var
  UncompressedMS: TMemoryStream;
  cs: Tcompressionstream;
  CompressedMS: TMemoryStream;
begin
  if Compress then begin
    UncompressedMS:=TMemoryStream.Create;
    CompressedMS:=TMemoryStream.Create;
    cs:=Tcompressionstream.create(cldefault,CompressedMS);
    try
      SaveToStream(UncompressedMS);
      UncompressedMS.Position:=0;
      cs.CopyFrom(UncompressedMS,UncompressedMS.Size);
      CompressedMS.Position:=0;
      CompressedMS.SaveToFile(Filename);
    finally
      cs.Free;
      UncompressedMS.Free;
      CompressedMS.Free;
    end;
  end else begin
    UncompressedMS:=TMemoryStream.Create;
    try
      SaveToStream(UncompressedMS);
      UncompressedMS.Position:=0;
      UncompressedMS.SaveToFile(Filename);
    finally
      UncompressedMS.Free;
    end;
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
  w(UDFileHeader);
  w(IntToStr(UDFileVersion));
  w(LineEnding);

  UnitID:=TFilenameToStringTree.Create(false);
  try
    // write units
    w('//BeginUnits'+LineEnding);
    AVLNode:=FUnitsByFilename.FindLowest;
    i:=0;
    while AVLNode<>nil do begin
      CurUnit:=TUDUnit(AVLNode.Data);
      inc(i);
      UnitID.Add(CurUnit.Filename,GetBase32(i));
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
    w('//EndUnits'+LineEnding);

    // write groups
    w('//BeginGroups'+LineEnding);
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
    w('//EndGroups'+LineEnding);
  finally
    UnitID.Free;
  end;
end;

procedure TUnitDictionary.LoadFromFile(const Filename: string);
var
  UncompressedMS: TMemoryStream;
begin
  UncompressedMS:=TMemoryStream.Create;
  try
    UncompressedMS.LoadFromFile(Filename);
    UncompressedMS.Position:=0;
    LoadFromStream(UncompressedMS);
  finally
    UncompressedMS.Free;
  end;
end;

procedure TUnitDictionary.LoadFromStream(aStream: TMemoryStream);
var
  Y: integer;
  LineStart: PChar;
  p: PChar;
  EndP: PChar;
  Version: Integer;
  IDToUnit: TStringToPointerTree;

  procedure E(Msg: string; Col: PtrInt = 0);
  var
    s: String;
  begin
    s:='Error in line '+IntToStr(Y);
    if Col=0 then
      Col:=p-LineStart+1;
    if Col>0 then
      s:=s+', column '+IntToStr(Col);
    s:=s+': '+Msg;
    raise Exception.Create(s);
  end;

  function ReadDecimal: integer;
  var
    s: PChar;
  begin
    Result:=0;
    s:=p;
    while (p<EndP) and (p^ in ['0'..'9']) do begin
      Result:=Result*10+ord(p^)-ord('0');
      inc(p);
    end;
    if s=p then
      e('number expected, but '+dbgstr(p^)+' found.');
  end;

  procedure ReadConstant(const Expected, ErrMsg: string);
  var
    i: Integer;
  begin
    i:=1;
    while (i<=length(Expected)) do begin
      if (p=EndP) or (p^<>Expected[i]) then
        e(ErrMsg);
      inc(p);
      inc(i);
    end;
  end;

  procedure ReadLineEnding;
  var
    c: Char;
  begin
    if (p=EndP) or (not (p^ in [#10,#13])) then
      e('line ending missing');
    c:=p^;
    inc(p);
    if (p<EndP) and (p^ in [#10,#13]) and (c<>p^) then
      inc(p);
    inc(y);
    LineStart:=p;
  end;

  function ReadFileFormat: integer;
  begin
    ReadConstant(UDFileHeader,'invalid file header');
    Result:=ReadDecimal;
    ReadLineEnding;
  end;

  procedure ReadUnits;
  var
    StartP: PChar;
    UnitID, CurUnitName, UnitFilename, Identifier: string;
    CurUnit: TUDUnit;
    Item: TUDIdentifier;
  begin
    ReadConstant('//BeginUnits','missing //BeginUnits header');
    ReadLineEnding;

    repeat
      // read unit id
      StartP:=p;
      while (p<EndP) and (p^ in ['0'..'9','A'..'Z']) do inc(p);
      if (StartP=p) or (p^<>';') then
        e('unit id expected');
      SetLength(UnitID,p-StartP);
      Move(StartP^,UnitID[1],length(UnitID));
      inc(p); // skip semicolon

      // read unit name
      StartP:=p;
      while (p<EndP) and (p^ in ['0'..'9','A'..'Z','a'..'z','_','.']) do inc(p);
      if (StartP=p) or (p^<>';') then
        e('unit name expected');
      SetLength(CurUnitName,p-StartP);
      Move(StartP^,CurUnitName[1],length(CurUnitName));
      inc(p); // skip semicolon

      // read file name
      StartP:=p;
      while (p<EndP) and (not (p^ in [#10,#13])) do inc(p);
      if (StartP=p) or (not (p^ in [#10,#13])) then
        e('file name expected');
      SetLength(UnitFilename,p-StartP);
      Move(StartP^,UnitFilename[1],length(UnitFilename));
      ReadLineEnding;

      CurUnit:=AddUnit(CurUnitName,UnitFilename);
      IDToUnit[UnitID]:=CurUnit;

      // read identifiers until empty line
      repeat
        StartP:=p;
        while (p<EndP) and (p^ in ['0'..'9','A'..'Z','a'..'z','_']) do inc(p);
        if (not (p^ in [#10,#13])) then
          e('identifier expected');
        if p=StartP then break;
        SetLength(Identifier,p-StartP);
        Move(StartP^,Identifier[1],length(Identifier));
        ReadLineEnding;
        Item:=TUDIdentifier.Create(Identifier);
        FIdentifiers.Add(Item);
        CurUnit.AddIdentifier(Item);
      until false;
      ReadLineEnding;

    until (p=EndP) or (p^='/');

    ReadConstant('//EndUnits','missing //EndUnits footer');
    ReadLineEnding;
  end;

  procedure ReadGroups;
  var
    GroupName, GroupFilename, UnitID: string;
    StartP: PChar;
    Group: TUDUnitGroup;
    CurUnit: TUDUnit;
  begin
    ReadConstant('//BeginGroups','missing //BeginGroups header');
    ReadLineEnding;

    repeat
      // read group name
      StartP:=p;
      while (p<EndP) and (p^ in ['0'..'9','A'..'Z','a'..'z','_','.']) do inc(p);
      if (p^<>';') then
        e('group name expected');
      SetLength(GroupName,p-StartP);
      if GroupName<>'' then
        Move(StartP^,GroupName[1],length(GroupName));
      inc(p); // skip semicolon

      // read file name
      StartP:=p;
      while (p<EndP) and (not (p^ in [#10,#13])) do inc(p);
      if (not (p^ in [#10,#13])) then
        e('file name expected');
      SetLength(GroupFilename,p-StartP);
      if GroupFilename<>'' then
        Move(StartP^,GroupFilename[1],length(GroupFilename));
      ReadLineEnding;

      Group:=FindGroupWithFilename(GroupFilename);
      if Group=nil then
        Group:=AddUnitGroup(GroupName,GroupFilename);

      // read units of group until empty line
      repeat
        StartP:=p;
        while (p<EndP) and (p^ in ['0'..'9','A'..'Z','a'..'z','_']) do inc(p);
        if (not (p^ in [#10,#13])) then
          e('unit identifier expected');
        if p=StartP then break;
        SetLength(UnitID,p-StartP);
        Move(StartP^,UnitID[1],length(UnitID));
        ReadLineEnding;

        CurUnit:=TUDUnit(IDToUnit[UnitID]);
        if CurUnit<>nil then begin
          if (Group<>NoGroup) and (CurUnit.IsInGroup(NoGroup)) then begin
            // move from no group to some group
            NoGroup.RemoveUnit(CurUnit);
          end;
          Group.AddUnit(CurUnit);
        end else begin
          debugln(['Warning: TUnitDictionary.LoadFromStream.ReadGroups unit id is not defined: ',UnitID]);
        end;
      until false;
      ReadLineEnding;

    until (p=EndP) or (p^='/');

    ReadConstant('//EndGroups','missing //EndGroups footer');
    ReadLineEnding;
  end;

begin
  Clear;
  if aStream.Size<=aStream.Position then
    raise Exception.Create('This is not a UnitDictionary. Header missing.');
  p:=PChar(aStream.Memory);
  EndP:=p+aStream.Size;
  LineStart:=p;
  Y:=1;
  Version:=ReadFileFormat;
  if Version<1 then
    E('invalid version '+IntToStr(Version));
  //debugln(['TUnitDictionary.LoadFromStream Version=',Version]);
  IDToUnit:=TStringToPointerTree.Create(true);
  try
    ReadUnits;
    ReadGroups;
  finally
    IDToUnit.Free;
  end;
end;

function TUnitDictionary.Equals(Dictionary: TUnitDictionary): boolean;
var
  Node1, Node2: TAVLTreeNode;
  Group1: TUDUnitGroup;
  Group2: TUDUnitGroup;
  Unit1: TUDUnit;
  Unit2: TUDUnit;
  Item1: TUDIdentifier;
  Item2: TUDIdentifier;
begin
  Result:=false;
  if Dictionary=nil then exit;
  if Dictionary=Self then exit(true);
  if UnitGroupsByFilename.Count<>Dictionary.UnitGroupsByFilename.Count then exit;
  if UnitGroupsByName.Count<>Dictionary.UnitGroupsByName.Count then exit;
  if UnitsByFilename.Count<>Dictionary.UnitsByFilename.Count then exit;
  if UnitsByName.Count<>Dictionary.UnitsByName.Count then exit;
  if Identifiers.Count<>Dictionary.Identifiers.Count then exit;

  Node1:=UnitGroupsByFilename.FindLowest;
  Node2:=Dictionary.UnitGroupsByFilename.FindLowest;
  while Node1<>nil do begin
    Group1:=TUDUnitGroup(Node1.Data);
    Group2:=TUDUnitGroup(Node2.Data);
    if Group1.Name<>Group2.Name then exit;
    if Group1.Filename<>Group2.Filename then exit;
    Node1:=UnitGroupsByFilename.FindSuccessor(Node1);
    Node2:=UnitGroupsByFilename.FindSuccessor(Node2);
  end;

  Node1:=UnitsByFilename.FindLowest;
  Node2:=Dictionary.UnitsByFilename.FindLowest;
  while Node1<>nil do begin
    Unit1:=TUDUnit(Node1.Data);
    Unit2:=TUDUnit(Node2.Data);
    if Unit1.Name<>Unit2.Name then exit;
    if Unit1.Filename<>Unit2.Filename then exit;

    Item1:=Unit1.FirstIdentifier;
    Item2:=Unit2.FirstIdentifier;
    while (Item1<>nil) and (Item2<>nil) do begin
      if Item1.Name<>Item2.Name then begin
        //debugln(['TUnitDictionary.Equals Item1.Name=',Item1.Name,'<>Item2.Name=',Item2.Name]);
        exit;
      end;
      Item1:=Item1.NextInUnit;
      Item2:=Item2.NextInUnit;
    end;
    if (Item1<>nil) then exit;
    if (Item2<>nil) then exit;
    Node1:=UnitGroupsByFilename.FindSuccessor(Node1);
    Node2:=UnitGroupsByFilename.FindSuccessor(Node2);
  end;

  Result:=true
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

function TUnitDictionary.FindGroupWithFilename(const aFilename: string
  ): TUDUnitGroup;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=FUnitGroupsByFilename.FindKey(Pointer(aFilename),@CompareFileNameWithIDFileItem);
  if AVLNode<>nil then
    Result:=TUDUnitGroup(AVLNode.Data)
  else
    Result:=nil;
end;

function TUnitDictionary.AddUnit(const aName, aFilename: string;
  Group: TUDUnitGroup): TUDUnit;
begin
  if Group=nil then
    Group:=NoGroup;
  Result:=FindUnitWithFilename(aFilename);
  if Result=nil then begin
    Result:=Group.AddUnit(aName,aFilename);
    FUnitsByFilename.Add(Result);
    FUnitsByName.Add(Result);
  end else
    Group.AddUnit(Result);
  if Group<>NoGroup then
    NoGroup.RemoveUnit(Result);
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
    Group:=NoGroup;
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
    if (Group<>NoGroup) then begin
      if CurUnit.IsInGroup(NoGroup) then begin
        // move from no group to some group
        NoGroup.RemoveUnit(CurUnit);
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

