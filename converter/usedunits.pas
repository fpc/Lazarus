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

  Author: Juha Manninen

  Abstract:
    Takes care of converting Uses section, adding, removing and replacing unit names.
    Part of Delphi converter.
}
unit UsedUnits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs,
  // IDE
  LazarusIDEStrConsts, IDEMsgIntf,
  // codetools
  CodeToolManager, StdCodeTools, CodeTree, CodeCache, CodeToolsStructs, AVL_Tree,
  KeywordFuncLists, SourceChanger,
  // Converter
  ConverterTypes, ConvCodeTool, ConvertSettings, ReplaceNamesUnit;

type

  TUsedUnitsTool = class;

  { TUsedUnits }

  TUsedUnits = class
  private
    fCTLink: TCodeToolLink;           // Link to codetools.
    fUsesSection: TUsesSection;       // Enum used by some codetools funcs.
    fExistingUnits: TStringList;      // List of units before conversion.
    fUnitsToAddForLCL: TStringList;   // List of new units for LCL (not for Delphi).
    fUnitsToRemove: TStringList;      // List of units to remove.
    // Units to rename. Map old unit name -> new unit name.
    fUnitsToRename: TStringToStringTree;
    fUnitsToComment: TStringList;     // List of units to be commented.
    fMissingUnits: TStringList;       // Units not found in search path.
    procedure ToBeRenamedOrRemoved(AOldName, ANewName: string);
    procedure FindReplacement(AUnitUpdater: TStringMapUpdater;
                              AMapToEdit: TStringToStringTree);
    function AddDelphiAndLCLSections: Boolean;
    function RemoveUnits: boolean;
    function RenameUnits: boolean;
    function AddUnits: boolean;
    function CommentOutUnits: boolean;
  protected
    // This is either the Interface or Implementation node.
    function ParentBlockNode: TCodeTreeNode; virtual; abstract;
    // Uses node in either Main or Implementation section.
    function UsesSectionNode: TCodeTreeNode; virtual; abstract;
  public
    constructor Create(ACTLink: TCodeToolLink);
    destructor Destroy; override;
    procedure CommentAutomatic(ACommentedUnits: TStringList);
  public
    property ExistingUnits: TStringList read fExistingUnits;
    property UnitsToAddForLCL: TStringList read fUnitsToAddForLCL;
    property MissingUnits: TStringList read fMissingUnits;
    property UnitsToRemove: TStringList read fUnitsToRemove;
    property UnitsToRename: TStringToStringTree read fUnitsToRename;
    property UnitsToComment: TStringList read fUnitsToComment;
  end;

  { TMainUsedUnits }

  TMainUsedUnits = class(TUsedUnits)
  private
  protected
    function ParentBlockNode: TCodeTreeNode; override;
    function UsesSectionNode: TCodeTreeNode; override;
  public
    constructor Create(ACTLink: TCodeToolLink);
    destructor Destroy; override;
  end;

  { TImplUsedUnits }

  TImplUsedUnits = class(TUsedUnits)
  private
  protected
    function ParentBlockNode: TCodeTreeNode; override;
    function UsesSectionNode: TCodeTreeNode; override;
  public
    constructor Create(ACTLink: TCodeToolLink);
    destructor Destroy; override;
  end;

  { TUsedUnitsTool }

  TUsedUnitsTool = class
  private
    fCTLink: TCodeToolLink;
    fMainUsedUnits: TUsedUnits;
    fImplUsedUnits: TUsedUnits;
    fFilename: string;
    function GetMissingUnitCount: integer;
    function GetMissingUnits: TModalResult;
  public
    constructor Create(ACTLink: TCodeToolLink; AFilename: string);
    destructor Destroy; override;
    function Prepare: TModalResult;
    function Convert: TModalResult;
    procedure MoveMissingToComment(AAllCommentedUnits: TStrings);
  public
    property MainUsedUnits: TUsedUnits read fMainUsedUnits;
    property ImplUsedUnits: TUsedUnits read fImplUsedUnits;
    property MissingUnitCount: integer read GetMissingUnitCount;
  end;


implementation

function Join(AList: TStringList): string;
// Used in AddDelphiAndLCLSections. Could be moved to a more generic place.
var
  i: Integer;
begin
  Result:='';
  for i:=0 to AList.Count-1 do begin
    if i<AList.Count-1 then
      Result:=Result+AList[i]+', '
    else
      Result:=Result+AList[i];
  end;
end;

{ TUsedUnits }

constructor TUsedUnits.Create(ACTLink: TCodeToolLink);
var
  UsesNode: TCodeTreeNode;
begin
  inherited Create;
  fCTLink:=ACTLink;
  fUnitsToAddForLCL:=TStringList.Create;
  fUnitsToRemove:=TStringList.Create;
  fUnitsToRename:=TStringToStringTree.Create(false);
  fUnitsToComment:=TStringList.Create;
  fMissingUnits:=TStringList.Create;
  // Get existing unit names from uses section
  UsesNode:=UsesSectionNode;
  if Assigned(UsesNode) then
    fExistingUnits:=TStringList(fCTLink.CodeTool.UsesSectionToUnitnames(UsesNode))
  else
    fExistingUnits:=TStringList.Create;
  fExistingUnits.CaseSensitive:=false;
  fExistingUnits.Sorted:=True;
end;

destructor TUsedUnits.Destroy;
begin
  fExistingUnits.Free;
  fMissingUnits.Free;
  fUnitsToComment.Free;
  fUnitsToRename.Free;
  fUnitsToRemove.Free;
  fUnitsToAddForLCL.Free;
  inherited Destroy;
end;

// function TUsedUnits.GetMissingUnits: TModalResult;  was here.

procedure TUsedUnits.ToBeRenamedOrRemoved(AOldName, ANewName: string);
// Replace a unit name with a new name or remove it if there is no new name.
begin
  if ANewName<>'' then begin
    fUnitsToRename[AOldName]:=ANewName;
    IDEMessagesWindow.AddMsg(Format(
      lisConvDelphiReplacedUnitSWithSInUsesSection, [AOldName, ANewName]), '', -1);
  end
  else begin
    fUnitsToRemove.Add(AOldName);
    IDEMessagesWindow.AddMsg(Format(
        lisConvDelphiRemovedUsedUnitSInUsesSection, [AOldName]), '', -1);
  end;
end;

procedure TUsedUnits.FindReplacement(AUnitUpdater: TStringMapUpdater;
                                     AMapToEdit: TStringToStringTree);
var
  i: integer;
  UnitN, s: string;
begin
  for i:=fMissingUnits.Count-1 downto 0 do begin
    UnitN:=fMissingUnits[i];
    if AUnitUpdater.FindReplacement(UnitN, s) then begin
      // Don't replace Windows unit with LCL units in a console application.
      if (LowerCase(UnitN)='windows') and fCTLink.IsConsoleApp then
        s:='';
      if Assigned(AMapToEdit) then
        AMapToEdit[UnitN]:=s                      // Add for interactive editing.
      else
        ToBeRenamedOrRemoved(UnitN, s);
      fMissingUnits.Delete(i);
    end;
  end;
end;

function TUsedUnits.AddDelphiAndLCLSections: Boolean;
var
  DelphiOnlyUnits: TStringList;  // Delphi specific units.
  LclOnlyUnits: TStringList;     // LCL specific units.

  function MoveToDelphi(AUnitName: string; ARenameForLcl: boolean): boolean;
  var
    UsesNode: TCodeTreeNode;
  begin
    Result:=True;
//    if fExistingUnits.Find(AUnitName, ind) then begin
      fCTLink.ResetMainScanner;
      fCTLink.CodeTool.BuildTree(fUsesSection=usMain);
      // Calls either FindMainUsesSection; or FindImplementationUsesSection;
      UsesNode:=UsesSectionNode;
      Assert(Assigned(UsesNode),
            'UsesNode should be assigned in AddDelphiAndLCLSections->MoveToDelphi');
      Result:=fCTLink.CodeTool.RemoveUnitFromUsesSection(UsesNode,
                                      UpperCaseStr(AUnitName), fCTLink.SrcCache);
      DelphiOnlyUnits.Add(AUnitName);
      if ARenameForLcl then
        LCLOnlyUnits.Add(fUnitsToRename[AUnitName]);
//    end;
  end;

var
  i, InsPos: Integer;
  s: string;
  EndChar: char;
  RenameList: TStringList;
  UsesNode: TCodeTreeNode;
  ParentBlock: TCodeTreeNode;
begin
  Result:=False;
  DelphiOnlyUnits:=TStringList.Create;
  LclOnlyUnits:=TStringList.Create;
  RenameList:=TStringList.Create;
  try
    // Don't remove the unit names but add to Delphi block instead.
    for i:=0 to fUnitsToRemove.Count-1 do
      if not MoveToDelphi(fUnitsToRemove[i], False) then Exit;
    // ... and don't comment the unit names either.
    for i:=0 to fUnitsToComment.Count-1 do
      if not MoveToDelphi(fUnitsToComment[i], False) then Exit;
    // Add replacement units to LCL block.
    fUnitsToRename.GetNames(RenameList);
    for i:=0 to RenameList.Count-1 do
      if not MoveToDelphi(RenameList[i], True) then Exit;
    // Additional units for LCL (like Interfaces).
    LCLOnlyUnits.AddStrings(fUnitsToAddForLCL);
    // Add LCL and Delphi sections for output.
    if (LclOnlyUnits.Count=0) and (DelphiOnlyUnits.Count=0) then Exit(True);
    fCTLink.ResetMainScanner;
    fCTLink.CodeTool.BuildTree(fUsesSection=usMain);
    UsesNode:=UsesSectionNode;
    if Assigned(UsesNode) then begin //uses section exists
      EndChar:=',';
      s:='';
      //TODO: check for special units
      fCTLink.CodeTool.MoveCursorToUsesStart(UsesNode);
      InsPos:=fCTLink.CodeTool.CurPos.StartPos;
    end
    else begin                        //uses section does not exist
      EndChar:=';';
      s:=LineEnding;
      // ParentBlock should never be Nil. UsesNode=Nil only for implementation section.
      ParentBlock:=ParentBlockNode;
      Assert(Assigned(ParentBlock),'ParentBlock should be assigned in AddDelphiAndLCLSections');
      if ParentBlock=Nil then Exit;
      // set insert position behind interface or implementation keyword
      // TODO: what about program?
      with fCTLink.CodeTool do begin
        MoveCursorToNodeStart(ParentBlock);
        ReadNextAtom;
        InsPos:=FindLineEndOrCodeAfterPosition(CurPos.EndPos,false);
      end;
    end;
    s:=s+'{$IFNDEF FPC}'+LineEnding;
    if DelphiOnlyUnits.Count>0 then begin
      if UsesNode=Nil then
        s:=s+'uses'+LineEnding;
      s:=s+'  '+Join(DelphiOnlyUnits)+EndChar+LineEnding;
    end;
    s:=s+'{$ELSE}'+LineEnding;
    if LclOnlyUnits.Count>0 then begin
      if UsesNode=Nil then
        s:=s+'uses'+LineEnding;
      s:=s+'  '+Join(LclOnlyUnits)+EndChar+LineEnding;
    end;
    s:=s+'{$ENDIF}';
    if Assigned(UsesNode) then
      s:=s+LineEnding+'  ';
    // Now add the generated lines.
    if not fCTLink.SrcCache.Replace(gtNewLine,gtNone,InsPos,InsPos,s) then exit;
    Result:=fCTLink.SrcCache.Apply;
  finally
    RenameList.Free;
    LclOnlyUnits.Free;
    DelphiOnlyUnits.Free;
  end;
end;

procedure TUsedUnits.CommentAutomatic(ACommentedUnits: TStringList);
// Comment automatically all missing units that are found in predefined list.
var
  i, x: Integer;
begin
  for i:=fMissingUnits.Count-1 downto 0 do begin
    if ACommentedUnits.Find(fMissingUnits[i], x) then begin
      fUnitsToComment.Add(fMissingUnits[i]);
      fMissingUnits.Delete(i);
    end;
  end;
end;

function TUsedUnits.RemoveUnits: boolean;
// Remove units
var
  i: Integer;
begin
  Result:=false;
  for i:=0 to fUnitsToRemove.Count-1 do begin
    fCTLink.ResetMainScanner;
    fCTLink.CodeTool.BuildTree(fUsesSection=usMain);
    if not fCTLink.CodeTool.RemoveUnitFromUsesSection(UsesSectionNode,
                         UpperCaseStr(fUnitsToRemove[i]), fCTLink.SrcCache) then
      exit;
    if not fCTLink.SrcCache.Apply then exit;
  end;
  //fUnitsToRemove.Clear;
  Result:=true;
end;

function TUsedUnits.RenameUnits: boolean;
// Rename units
begin
  Result:=false;
  if not fCTLink.CodeTool.ReplaceUsedUnits(fUnitsToRename, fCTLink.SrcCache) then
    exit;
  //fUnitsToRename.Clear;
  Result:=true;
end;

function TUsedUnits.AddUnits: boolean;
var
  i: Integer;
begin
  Result:=false;
  for i:=0 to fUnitsToAddForLCL.Count-1 do
    if not fCTLink.CodeTool.AddUnitToSpecificUsesSection(
                    fUsesSection, fUnitsToAddForLCL[i], '', fCTLink.SrcCache) then exit;
  Result:=true;
end;

function TUsedUnits.CommentOutUnits: boolean;
// Comment out missing units
begin
  Result:=false;
  if fUnitsToComment.Count>0 then
    if not fCTLink.CodeTool.CommentUnitsInUsesSections(fUnitsToComment,
                                                       fCTLink.SrcCache) then
      exit;
  Result:=true;
end;

{ TMainUsedUnits }

constructor TMainUsedUnits.Create(ACTLink: TCodeToolLink);
begin
  inherited Create(ACTLink);
  fUsesSection:=usMain;
end;

destructor TMainUsedUnits.Destroy;
begin
  inherited Destroy;
end;

function TMainUsedUnits.ParentBlockNode: TCodeTreeNode;
begin
  Result:=fCTLink.CodeTool.FindInterfaceNode;
end;

function TMainUsedUnits.UsesSectionNode: TCodeTreeNode;
begin
  Result:=fCTLink.CodeTool.FindMainUsesSection;
end;

{ TImplUsedUnits }

constructor TImplUsedUnits.Create(ACTLink: TCodeToolLink);
begin
  inherited Create(ACTLink);
  fUsesSection:=usImplementation;
end;

destructor TImplUsedUnits.Destroy;
begin
  inherited Destroy;
end;

function TImplUsedUnits.ParentBlockNode: TCodeTreeNode;
begin
  Result:=fCTLink.CodeTool.FindImplementationNode;
end;

function TImplUsedUnits.UsesSectionNode: TCodeTreeNode;
begin
  Result:=fCTLink.CodeTool.FindImplementationUsesSection;
end;

{ TUsedUnitsTool }

constructor TUsedUnitsTool.Create(ACTLink: TCodeToolLink; AFilename: string);
begin
  inherited Create;
  fCTLink:=ACTLink;
  fFilename:=AFilename;
  fCTLink.CodeTool.BuildTree(False);
  // These will read uses sections while creating.
  fMainUsedUnits:=TMainUsedUnits.Create(ACTLink);
  fImplUsedUnits:=TImplUsedUnits.Create(ACTLink);
end;

destructor TUsedUnitsTool.Destroy;
begin
  fImplUsedUnits.Free;
  fMainUsedUnits.Free;
  inherited Destroy;
end;

function TUsedUnitsTool.GetMissingUnits: TModalResult;
// Get missing unit by codetools.
// This can be moved to TUsedUnits if codetools is refactored.
var
  i: Integer;
  s: String;
  AllMissUnits: TStrings;
begin
  Result:=mrOk;
  AllMissUnits:=nil;    // Will be created by FindMissingUnits.
  try
    if not fCTLink.CodeTool.FindMissingUnits(AllMissUnits,False,True,fCTLink.SrcCache)
    then begin
      Result:=mrCancel;
      exit;
    end;
    if Assigned(AllMissUnits) then begin
      // Remove Windows specific units from the list if target is "Windows only",
      //  needed if work-platform is different from Windows (kind of a hack).
      if fCTLink.Settings.Target=ctLazarusWin then begin
        for i:=AllMissUnits.Count-1 downto 0 do begin
          s:=LowerCase(AllMissUnits[i]);
          if (s='windows') or (s='variants') or (s='shellapi') then
            AllMissUnits.Delete(i);
        end;
      end;
      // Split AllMissUnits into Main and Implementation
      for i:=0 to AllMissUnits.Count-1 do begin
        s:=AllMissUnits[i];
        if fMainUsedUnits.ExistingUnits.IndexOf(s)<>-1 then
          fMainUsedUnits.MissingUnits.Add(s);
        if fImplUsedUnits.ExistingUnits.IndexOf(s)<>-1 then
          fImplUsedUnits.MissingUnits.Add(s);
      end;
    end;
  finally
    AllMissUnits.Free;
  end;
end;

function TUsedUnitsTool.Prepare: TModalResult;
// Find missing units and mark some of them to be replaced later.
// More units can be marked for add, remove, rename and comment during conversion.
var
  UnitUpdater: TStringMapUpdater;
  MapToEdit: TStringToStringTree;
  Node: TAVLTreeNode;
  Item: PStringToStringTreeItem;
  UnitN, s: string;
  i: Integer;
begin
  Result:=mrOK;
  // Add unit 'Interfaces' if project uses 'Forms' and doesn't have 'Interfaces' yet.
  if fCTLink.IsMainFile then begin
    if ( fMainUsedUnits.fExistingUnits.Find('forms', i)
      or fImplUsedUnits.fExistingUnits.Find('forms', i) )
    and (not fMainUsedUnits.fExistingUnits.Find('interfaces', i) )
    and (not fImplUsedUnits.fExistingUnits.Find('interfaces', i) ) then
      fMainUsedUnits.fUnitsToAddForLCL.Add('Interfaces');
  end;
  UnitUpdater:=TStringMapUpdater.Create(fCTLink.Settings.ReplaceUnits);
  try
    MapToEdit:=Nil;
    if fCTLink.Settings.UnitsReplaceMode=rlInteractive then
      MapToEdit:=TStringToStringTree.Create(false);
    Result:=GetMissingUnits;
    if Result<>mrOK then exit;
    // Find replacements for missing units from settings.
    fMainUsedUnits.FindReplacement(UnitUpdater, MapToEdit);
    fImplUsedUnits.FindReplacement(UnitUpdater, MapToEdit);
    if Assigned(MapToEdit) and (MapToEdit.Tree.Count>0) then begin
      // Edit, then remove or replace units.
      Result:=EditMap(MapToEdit, Format(lisConvDelphiUnitsToReplaceIn,
                                        [ExtractFileName(fFilename)]));
      if Result<>mrOK then exit;
      // Iterate the map and rename / remove.
      Node:=MapToEdit.Tree.FindLowest;
      while Node<>nil do begin
        Item:=PStringToStringTreeItem(Node.Data);
        UnitN:=Item^.Name;
        s:=Item^.Value;
        if fMainUsedUnits.fExistingUnits.IndexOf(UnitN)<>-1 then
          fMainUsedUnits.ToBeRenamedOrRemoved(UnitN,s);
        if fImplUsedUnits.fExistingUnits.IndexOf(UnitN)<>-1 then
          fImplUsedUnits.ToBeRenamedOrRemoved(UnitN,s);
        Node:=MapToEdit.Tree.FindSuccessor(Node);
      end;
    end;
//  if not fCodeTool.FixUsedUnitCase(fSrcCache) then exit;
  finally
    MapToEdit.Free;      // May be Nil but who cares.
    UnitUpdater.Free;
  end;
end;

function TUsedUnitsTool.Convert: TModalResult;
// Add, remove, rename and comment out unit names that were marked earlier.
begin
  Result:=mrCancel;
  if fCTLink.Settings.Target=ctLazarus then begin
    // One way conversion -> remove and rename units.
    if not fMainUsedUnits.RemoveUnits then exit;    // Remove
    if not fImplUsedUnits.RemoveUnits then exit;
    if not fMainUsedUnits.RenameUnits then exit;    // Rename
    if not fImplUsedUnits.RenameUnits then exit;
  end;
  if fCTLink.Settings.Target in [ctLazarusDelphi, ctLazarusDelphiSameDfm] then begin
    // Support Delphi. Add IFDEF blocks for units.
    if not fMainUsedUnits.AddDelphiAndLCLSections then exit;
    if not fImplUsedUnits.AddDelphiAndLCLSections then exit;
  end
  else begin // [ctLazarus, ctLazarusWin] -> comment out units if needed.
    if not fMainUsedUnits.CommentOutUnits then exit;
    if not fImplUsedUnits.CommentOutUnits then exit;
    if not fMainUsedUnits.AddUnits then exit;       // Add the extra units.
    if not fImplUsedUnits.AddUnits then exit;
  end;
  Result:=mrOK;
end;

procedure TUsedUnitsTool.MoveMissingToComment(AAllCommentedUnits: TStrings);
begin
  // These units will be commented automatically in one project/package.
  if Assigned(AAllCommentedUnits) then begin
    AAllCommentedUnits.AddStrings(fMainUsedUnits.MissingUnits);
    AAllCommentedUnits.AddStrings(fImplUsedUnits.MissingUnits);
  end;
  // Move all to be commented.
  fMainUsedUnits.UnitsToComment.AddStrings(fMainUsedUnits.MissingUnits);
  fMainUsedUnits.MissingUnits.Clear;
  fImplUsedUnits.UnitsToComment.AddStrings(fImplUsedUnits.MissingUnits);
  fImplUsedUnits.MissingUnits.Clear;
end;

function TUsedUnitsTool.GetMissingUnitCount: integer;
begin
  Result:=fMainUsedUnits.MissingUnits.Count+fImplUsedUnits.MissingUnits.Count;
end;

end.

