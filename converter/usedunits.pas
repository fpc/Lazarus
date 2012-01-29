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
  LinkScanner, KeywordFuncLists, SourceChanger, CodeAtom, CodeToolsStrConsts,
  // Converter
  ConverterTypes, ConvCodeTool, ConvertSettings, ReplaceNamesUnit;

type

  TUsedUnitsTool = class;

  { TUsedUnits }

  TUsedUnits = class
  private
    fCTLink: TCodeToolLink;              // Link to codetools.
    fOwnerTool: TUsedUnitsTool;
    fUsesSection: TUsesSection;          // Enum used by some codetools funcs.
    fExistingUnits: TStringList;         // List of units before conversion.
    fUnitsToAdd: TStringList;            // List of new units to add.
    fUnitsToAddForLCL: TStringList;      // List of new units for LCL (not for Delphi).
    fUnitsToRemove: TStringList;         // List of units to remove.
    fUnitsToRename: TStringToStringTree; // Units to rename. Map old name -> new name.
    fUnitsToRenameKeys: TStringList;     // List of keys of the above map.
    fUnitsToRenameVals: TStringList;     // List of values of the above map.
    fUnitsToFixCase: TStringToStringTree;// Like rename but done for every target.
    fUnitsToComment: TStringList;        // List of units to be commented.
    fMissingUnits: TStringList;          // Units not found in search path.
    function FindMissingUnits(AUnitUpdater: TStringMapUpdater): boolean;
    procedure ToBeRenamedOrRemoved(AOldName, ANewName: string);
    procedure FindReplacement(AUnitUpdater: TStringMapUpdater;
                              AMapToEdit: TStringToStringTree);
    function AddDelphiAndLCLSections: Boolean;
    function RemoveUnits: boolean;
  protected
    // This is either the Interface or Implementation node.
    function ParentBlockNode: TCodeTreeNode; virtual; abstract;
    // Uses node in either Main or Implementation section.
    function UsesSectionNode: TCodeTreeNode; virtual; abstract;
  public
    constructor Create(ACTLink: TCodeToolLink; aOwnerTool: TUsedUnitsTool);
    destructor Destroy; override;
    procedure CommentAutomatic(ACommentedUnits: TStringList);
  public
    property UnitsToRemove: TStringList read fUnitsToRemove;
    property UnitsToRename: TStringToStringTree read fUnitsToRename;
    property UnitsToFixCase: TStringToStringTree read fUnitsToFixCase;
    property MissingUnits: TStringList read fMissingUnits;
  end;

  { TMainUsedUnits }

  TMainUsedUnits = class(TUsedUnits)
  private
  protected
    function ParentBlockNode: TCodeTreeNode; override;
    function UsesSectionNode: TCodeTreeNode; override;
  public
    constructor Create(ACTLink: TCodeToolLink; aOwnerTool: TUsedUnitsTool);
    destructor Destroy; override;
  end;

  { TImplUsedUnits }

  TImplUsedUnits = class(TUsedUnits)
  private
  protected
    function ParentBlockNode: TCodeTreeNode; override;
    function UsesSectionNode: TCodeTreeNode; override;
  public
    constructor Create(ACTLink: TCodeToolLink; aOwnerTool: TUsedUnitsTool);
    destructor Destroy; override;
  end;

  { TUsedUnitsTool }

  TUsedUnitsTool = class
  private
    fCTLink: TCodeToolLink;
    fFilename: string;
    fIsMainFile: Boolean;                 // Main project / package file.
    fIsConsoleApp: Boolean;
    fMainUsedUnits: TUsedUnits;
    fImplUsedUnits: TUsedUnits;
    fCheckPackageDependencyEvent: TCheckUnitEvent;
    function GetMissingUnitCount: integer;
  public
    constructor Create(ACTLink: TCodeToolLink; AFilename: string);
    destructor Destroy; override;
    function Prepare: TModalResult;
    function Convert: TModalResult;
    function Remove(AUnit: string): TModalResult;
    procedure MoveMissingToComment(AAllCommentedUnits: TStrings);
    procedure AddUnitIfNeeded(AUnitName: string);
    function AddThreadSupport: TModalResult;
  public
    property IsMainFile: Boolean read fIsMainFile write fIsMainFile;
    property IsConsoleApp: Boolean read fIsConsoleApp write fIsConsoleApp;
    property MainUsedUnits: TUsedUnits read fMainUsedUnits;
    property ImplUsedUnits: TUsedUnits read fImplUsedUnits;
    property MissingUnitCount: integer read GetMissingUnitCount;
    property CheckPackDepEvent: TCheckUnitEvent read fCheckPackageDependencyEvent
                                               write fCheckPackageDependencyEvent;
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

constructor TUsedUnits.Create(ACTLink: TCodeToolLink; aOwnerTool: TUsedUnitsTool);
var
  UsesNode: TCodeTreeNode;
begin
  inherited Create;
  fCTLink:=ACTLink;
  fOwnerTool:=aOwnerTool;
  fUnitsToAdd:=TStringList.Create;
  fUnitsToAddForLCL:=TStringList.Create;
  fUnitsToRemove:=TStringList.Create;
  fUnitsToRename:=TStringToStringTree.Create(true);
  fUnitsToRenameKeys:=TStringList.Create;
  fUnitsToRenameKeys.CaseSensitive:=false;
  fUnitsToRenameVals:=TStringList.Create;
  fUnitsToRenameVals.CaseSensitive:=false;
  fUnitsToFixCase:=TStringToStringTree.Create(true);
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
  fUnitsToFixCase.Free;
  fUnitsToRenameVals.Free;
  fUnitsToRenameKeys.Free;
  fUnitsToRename.Free;
  fUnitsToRemove.Free;
  fUnitsToAddForLCL.Free;
  fUnitsToAdd.Free;
  inherited Destroy;
end;

function TUsedUnits.FindMissingUnits(AUnitUpdater: TStringMapUpdater): boolean;
var
  UsesNode: TCodeTreeNode;
  InAtom, UnitNameAtom: TAtomPosition;
  OldUnitName, OldInFilename: String;
  NewUnitName, NewInFilename: String;
  AFilename, s, slo: String;
  x: Integer;
  OmitUnit: Boolean;
begin
  UsesNode:=UsesSectionNode;
  if UsesNode=nil then exit(true);
  with fCTLink do begin
    CodeTool.MoveCursorToUsesStart(UsesNode);
    repeat
      // read next unit name
      CodeTool.ReadNextUsedUnit(UnitNameAtom, InAtom);
      OldUnitName:=CodeTool.GetAtom(UnitNameAtom);
      if InAtom.StartPos>0 then
        OldInFilename:=copy(CodeTool.Src,InAtom.StartPos+1,
                            InAtom.EndPos-InAtom.StartPos-2)
      else
        OldInFilename:='';
      // find unit file
      NewUnitName:=OldUnitName;
      NewInFilename:=OldInFilename;
      AFilename:=CodeTool.FindUnitCaseInsensitive(NewUnitName,NewInFilename);
      s:=NewUnitName;
      if NewInFilename<>'' then
        s:=s+' in '''+NewInFilename+'''';
      if AFilename<>'' then begin                         // unit found
        OmitUnit:=Settings.OmitProjUnits.Find(NewUnitName, x);
        if (NewUnitName<>OldUnitName) and not OmitUnit then begin
          // Character case differs and it will not be replaced.
          fUnitsToFixCase[OldUnitName]:=NewUnitName;      // fix case
          IDEMessagesWindow.AddMsg(Format(lisConvDelphiFixedUnitCase,
                                          [OldUnitName, NewUnitName]), '', -1);
        end;
        // Report Windows specific units as missing if target is MultiPlatform,
        //  needed if work-platform is Windows (kind of a hack).
        slo:=LowerCase(NewUnitName);                        // 'variants' ?
        if (Settings.MultiPlatform and ((slo='windows') or (slo='shellapi'))) or OmitUnit then
          fMissingUnits.Add(s);
      end
      else begin
        // Omit Windows specific units from the list if target is "Windows only",
        //  needed if work-platform is different from Windows (kind of a hack).
        slo:=LowerCase(NewUnitName);
        if Settings.MultiPlatform or ((slo<>'windows') and (slo<>'shellapi')) then
          fMissingUnits.Add(s);
      end;
      if CodeTool.CurPos.Flag=cafComma then begin
        // read next unit name
        CodeTool.ReadNextAtom;
      end else if CodeTool.CurPos.Flag=cafSemicolon then begin
        break;
      end else
        CodeTool.RaiseExceptionFmt(ctsStrExpectedButAtomFound,[';',CodeTool.GetAtom]);
    until false;
  end;
  Result:=true;
end;

procedure TUsedUnits.ToBeRenamedOrRemoved(AOldName, ANewName: string);
// Replace a unit name with a new name or remove it if there is no new name.
var
  UnitInFileName: string;
begin
  if ANewName<>'' then begin
    fUnitsToRename[AOldName]:=ANewName;
    fUnitsToRenameKeys.Add(AOldName);
    fUnitsToRenameVals.Add(ANewName);
    IDEMessagesWindow.AddMsg(Format(lisConvDelphiReplacedUnitInUsesSection,
                                    [AOldName, ANewName]), '', -1);
    // If the unit is not found, open the package containing it.
    UnitInFileName:='';
    if fCTLink.CodeTool.FindUnitCaseInsensitive(ANewName,UnitInFileName) = '' then
      if Assigned(fOwnerTool.CheckPackDepEvent) then
        if not fOwnerTool.CheckPackDepEvent(ANewName) then
          ;
  end
  else begin
    fUnitsToRemove.Add(AOldName);
    IDEMessagesWindow.AddMsg(Format(lisConvDelphiRemovedUnitInUsesSection,
                                    [AOldName]), '', -1);
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
      if (LowerCase(UnitN)='windows') and fOwnerTool.IsConsoleApp then
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

  function MoveToDelphi(AUnitName: string): boolean;
  var
    UsesNode: TCodeTreeNode;
  begin
    Result:=True;
    with fCTLink do begin
      ResetMainScanner;
      if fUsesSection=usMain then
        CodeTool.BuildTree(lsrMainUsesSectionEnd)
      else
        CodeTool.BuildTree(lsrImplementationUsesSectionEnd);
      // Calls either FindMainUsesSection or FindImplementationUsesSection
      UsesNode:=UsesSectionNode;
      Assert(Assigned(UsesNode),
            'UsesNode should be assigned in AddDelphiAndLCLSections->MoveToDelphi');
      Result:=CodeTool.RemoveUnitFromUsesSection(UsesNode,UpperCaseStr(AUnitName),SrcCache);
    end;
    DelphiOnlyUnits.Add(AUnitName);
  end;

var
  i, InsPos: Integer;
  s: string;
  EndChar: char;
  UsesNode: TCodeTreeNode;
  ParentBlock: TCodeTreeNode;
begin
  Result:=False;
  DelphiOnlyUnits:=TStringList.Create;
  LclOnlyUnits:=TStringList.Create;
  try
    // Don't remove the unit names but add to Delphi block instead.
    for i:=0 to fUnitsToRemove.Count-1 do
      if not MoveToDelphi(fUnitsToRemove[i]) then Exit;
    // ... and don't comment the unit names either.
    for i:=0 to fUnitsToComment.Count-1 do
      if not MoveToDelphi(fUnitsToComment[i]) then Exit;
    // Add replacement units to LCL block.
    for i:=0 to fUnitsToRenameKeys.Count-1 do begin
      if not MoveToDelphi(fUnitsToRenameKeys[i]) then Exit;
      LCLOnlyUnits.Add(fUnitsToRename[fUnitsToRenameKeys[i]]);
    end;
    // Additional units for LCL (like Interfaces).
    LCLOnlyUnits.AddStrings(fUnitsToAddForLCL);
    // Add LCL and Delphi sections for output.
    if (LclOnlyUnits.Count=0) and (DelphiOnlyUnits.Count=0) then Exit(True);
    fCTLink.ResetMainScanner;
    if fUsesSection=usMain then
      fCTLink.CodeTool.BuildTree(lsrMainUsesSectionEnd)
    else
      fCTLink.CodeTool.BuildTree(lsrImplementationUsesSectionEnd);
    UsesNode:=UsesSectionNode;
    if Assigned(UsesNode) then begin      //uses section exists
      EndChar:=',';
      s:='';
      fCTLink.CodeTool.MoveCursorToUsesStart(UsesNode);
      InsPos:=fCTLink.CodeTool.CurPos.StartPos;
    end
    else begin                            //uses section does not exist
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
//    RenameList.Free;
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
    if fUsesSection=usMain then
      fCTLink.CodeTool.BuildTree(lsrMainUsesSectionEnd)
    else
      fCTLink.CodeTool.BuildTree(lsrImplementationUsesSectionEnd);
    if not fCTLink.CodeTool.RemoveUnitFromUsesSection(UsesSectionNode,
                         UpperCaseStr(fUnitsToRemove[i]), fCTLink.SrcCache) then
      exit;
    if not fCTLink.SrcCache.Apply then exit;
  end;
  fUnitsToRemove.Clear;
  Result:=true;
end;

{ TMainUsedUnits }

constructor TMainUsedUnits.Create(ACTLink: TCodeToolLink; aOwnerTool: TUsedUnitsTool);
begin
  inherited Create(ACTLink, aOwnerTool);
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

constructor TImplUsedUnits.Create(ACTLink: TCodeToolLink; aOwnerTool: TUsedUnitsTool);
begin
  inherited Create(ACTLink, aOwnerTool);
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
  fIsMainFile:=False;
  fIsConsoleApp:=False;
  fCTLink.CodeTool.BuildTree(lsrEnd);
  // These will read uses sections while creating.
  fMainUsedUnits:=TMainUsedUnits.Create(ACTLink, Self);
  fImplUsedUnits:=TImplUsedUnits.Create(ACTLink, Self);
end;

destructor TUsedUnitsTool.Destroy;
begin
  fImplUsedUnits.Free;
  fMainUsedUnits.Free;
  inherited Destroy;
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
  if fIsMainFile then begin
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
    fCTLink.CodeTool.BuildTree(lsrEnd);
    if not (fMainUsedUnits.FindMissingUnits(UnitUpdater) and
            fImplUsedUnits.FindMissingUnits(UnitUpdater)) then begin
      Result:=mrCancel;
      exit;
    end;
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
  finally
    MapToEdit.Free;      // May be Nil but who cares.
    UnitUpdater.Free;
  end;
end;

function TUsedUnitsTool.Convert: TModalResult;
// Add, remove, rename and comment out unit names that were marked earlier.
var
  i: Integer;
begin
  Result:=mrCancel;
  with fCTLink do begin
    // Fix case
    if not CodeTool.ReplaceUsedUnits(fMainUsedUnits.fUnitsToFixCase, SrcCache) then exit;
    if not CodeTool.ReplaceUsedUnits(fImplUsedUnits.fUnitsToFixCase, SrcCache) then exit;
    // Add more units.
    with fMainUsedUnits do begin
      for i:=0 to fUnitsToAdd.Count-1 do
        if not CodeTool.AddUnitToSpecificUsesSection(
                          fUsesSection, fUnitsToAdd[i], '', SrcCache) then exit;
    end;
    with fImplUsedUnits do begin
      for i:=0 to fUnitsToAdd.Count-1 do
        if not CodeTool.AddUnitToSpecificUsesSection(
                          fUsesSection, fUnitsToAdd[i], '', SrcCache) then exit;
    end;
    if fIsMainFile or (Settings.MultiPlatform and not Settings.SupportDelphi) then begin
      // One way conversion (or main file) -> remove and rename units.
      if not fMainUsedUnits.RemoveUnits then exit;    // Remove
      if not fImplUsedUnits.RemoveUnits then exit;
      // Rename
      if not CodeTool.ReplaceUsedUnits(fMainUsedUnits.fUnitsToRename, SrcCache) then exit;
      if not CodeTool.ReplaceUsedUnits(fImplUsedUnits.fUnitsToRename, SrcCache) then exit;
    end;
    if Settings.SupportDelphi then begin
      // Support Delphi. Add IFDEF blocks for units.
      if not fMainUsedUnits.AddDelphiAndLCLSections then exit;
      if not fImplUsedUnits.AddDelphiAndLCLSections then exit;
    end
    else begin // Lazarus only multi- or single-platform -> comment out units if needed.
      if not CodeTool.CommentUnitsInUsesSections(fMainUsedUnits.fUnitsToComment,
                                                 SrcCache) then exit;
      if not CodeTool.CommentUnitsInUsesSections(fImplUsedUnits.fUnitsToComment,
                                                 SrcCache) then exit;
      // Add more units meant for only LCL.
      with fMainUsedUnits do begin
        for i:=0 to fUnitsToAddForLCL.Count-1 do
          if not CodeTool.AddUnitToSpecificUsesSection(
                            fUsesSection, fUnitsToAddForLCL[i], '', SrcCache) then exit;
      end;
      with fImplUsedUnits do begin
        for i:=0 to fUnitsToAddForLCL.Count-1 do
          if not CodeTool.AddUnitToSpecificUsesSection(
                            fUsesSection, fUnitsToAddForLCL[i], '', SrcCache) then exit;
      end;
    end;
  end;
  Result:=mrOK;
end;

function TUsedUnitsTool.Remove(AUnit: string): TModalResult;
var
  x: Integer;
begin
  Result:=mrIgnore;
  if fMainUsedUnits.fExistingUnits.Find(AUnit, x) then begin
    fMainUsedUnits.UnitsToRemove.Add(AUnit);
    Result:=mrOK;
  end
  else if fImplUsedUnits.fExistingUnits.Find(AUnit, x) then begin
    fImplUsedUnits.UnitsToRemove.Add(AUnit);
    Result:=mrOK;
  end;
end;

procedure TUsedUnitsTool.MoveMissingToComment(AAllCommentedUnits: TStrings);
begin
  // These units will be commented automatically in one project/package.
  if Assigned(AAllCommentedUnits) then begin
    AAllCommentedUnits.AddStrings(fMainUsedUnits.fMissingUnits);
    AAllCommentedUnits.AddStrings(fImplUsedUnits.fMissingUnits);
  end;
  // Move all to be commented.
  fMainUsedUnits.fUnitsToComment.AddStrings(fMainUsedUnits.fMissingUnits);
  fMainUsedUnits.fMissingUnits.Clear;
  fImplUsedUnits.fUnitsToComment.AddStrings(fImplUsedUnits.fMissingUnits);
  fImplUsedUnits.fMissingUnits.Clear;
end;

procedure TUsedUnitsTool.AddUnitIfNeeded(AUnitName: string);
var
  i: Integer;
  UnitInFileName: String;
  RenameValFound: Boolean;
begin
  RenameValFound:=false;
  for i := 0 to fMainUsedUnits.fUnitsToRenameVals.Count-1 do
    if Pos(AUnitName, fMainUsedUnits.fUnitsToRenameVals[i]) > 0 then begin
      RenameValFound:=true;
      Break;
    end;
  if not RenameValFound then
    for i := 0 to fImplUsedUnits.fUnitsToRenameVals.Count-1 do
      if Pos(AUnitName, fImplUsedUnits.fUnitsToRenameVals[i]) > 0 then begin
        RenameValFound:=true;
        Break;
      end;
  if not ( fMainUsedUnits.fExistingUnits.Find(AUnitName, i) or
           fImplUsedUnits.fExistingUnits.Find(AUnitName, i) or
          (fMainUsedUnits.fUnitsToAdd.IndexOf(AUnitName) > -1) or RenameValFound)
  then begin
    fMainUsedUnits.fUnitsToAdd.Add(AUnitName);
    IDEMessagesWindow.AddMsg('Added unit '+AUnitName+ ' to uses section', '', -1);
    // If the unit is not found, open the package containing it.
    UnitInFileName:='';
    if fCTLink.CodeTool.FindUnitCaseInsensitive(AUnitName,UnitInFileName) = '' then
      if Assigned(fCheckPackageDependencyEvent) then
        if not fCheckPackageDependencyEvent(AUnitName) then
          ;
  end;
end;

function TUsedUnitsTool.AddThreadSupport: TModalResult;
// AddUnitToSpecificUsesSection would insert cthreads in the beginning automatically
// It doesn't work with {$IFDEF UNIX} directive -> use UsesInsertPolicy.
var
  i: Integer;
  OldPolicy: TUsesInsertPolicy;
begin
  Result:=mrCancel;
  if not ( fMainUsedUnits.fExistingUnits.Find('cthreads', i) or
           fImplUsedUnits.fExistingUnits.Find('cthreads', i) ) then
    with fCTLink, SrcCache.BeautifyCodeOptions do
    try
      OldPolicy:=UsesInsertPolicy;
      UsesInsertPolicy:=uipFirst;
      if not CodeTool.AddUnitToSpecificUsesSection(fMainUsedUnits.fUsesSection,
                           '{$IFDEF UNIX}cthreads{$ENDIF}', '', SrcCache) then exit;
    finally
      UsesInsertPolicy:=OldPolicy;
    end;
  Result:=mrOK;
end;

function TUsedUnitsTool.GetMissingUnitCount: integer;
begin
  Result:=fMainUsedUnits.fMissingUnits.Count+fImplUsedUnits.fMissingUnits.Count;
end;

end.

