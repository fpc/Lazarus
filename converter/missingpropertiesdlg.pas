{  $Id$  }
{
 /***************************************************************************
                            MissingPropertiesDlg.pas
                            ------------------------

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
}
unit MissingPropertiesDlg;

// Use RegisterPropertyToSkip
// RegisterPropertyToSkip(TControl, 'Ctl3D', 'VCL compatibility property', '');
// RegisterPropertyToSkip(TControl, 'ParentCtl3D', 'VCL compatibility property', '');
// mail thread from 08.12.2008 "DefaultButtonControlUseOnChange,
//  TButtonControl.UseOnChange (Mattias, you've added them long time ago)"

{$mode objfpc}{$H+}

interface

uses
  // FCL+LCL
  Classes, SysUtils, Math, LCLProc, Forms, Controls, //LResources,
  Graphics, Dialogs, Buttons, StdCtrls,
  // components
  SynHighlighterLFM, SynEdit, SynEditMiscClasses, LFMTrees,
  // codetools
  BasicCodeTools, CodeCache, CodeToolManager,
  // IDE
  IDEDialogs, ComponentReg, PackageIntf, IDEWindowIntf, //PropEdits,
  CustomFormEditor, LazarusIDEStrConsts, IDEProcs, OutputFilter, //IDEOptionDefs,
  EditorOptions, ExtCtrls, Grids, ConvertSettings; //JITForms, PropEditUtils,

type

  { TLfmChecker }

  TLFMChecker = class
  private
    fPascalBuffer: TCodeBuffer;
    fLFMBuffer: TCodeBuffer;
    fOnOutput: TOnAddFilteredLine;
    fRootMustBeClassInIntf: boolean;
    fObjectsMustExists: boolean;
    fLFMTree: TLFMTree;
    // References to controls in UI:
    fLFMSynEdit: TSynEdit;
    fErrorsListBox: TListBox;
    procedure WriteUnitError(Code: TCodeBuffer; X, Y: integer;
      const ErrorMessage: string);
    procedure WriteCodeToolsError;
    procedure WriteLFMErrors;
    function FixMissingComponentClasses: TModalResult;
    function CheckUnit: boolean;
    // Refactored and moved from dialog class:
    procedure LoadLFM;
    function RemoveAll: TModalResult;
    procedure FindNiceNodeBounds(LFMNode: TLFMTreeNode;
                                 var StartPos, EndPos: integer);
    function FindListBoxError: TLFMError;
    procedure JumpToError(LFMError: TLFMError);
    procedure AddReplacement(LFMChangeList: TList; StartPos, EndPos: integer;
                             const NewText: string);
    function ApplyReplacements(LFMChangeList: TList): boolean;
  protected
    function ShowRepairLFMWizard: TModalResult; virtual;
  public
    constructor Create(APascalBuffer, ALFMBuffer: TCodeBuffer;
                       const AOnOutput: TOnAddFilteredLine);
    destructor Destroy; override;
    function Repair: TModalResult;
    procedure FillErrorsListBox;
    function AutomaticFixIsPossible: boolean;
  public
    property PascalBuffer: TCodeBuffer read fPascalBuffer;
    property LFMBuffer: TCodeBuffer read fLFMBuffer;
    property OnOutput: TOnAddFilteredLine read fOnOutput;
    property RootMustBeClassInIntf: boolean read fRootMustBeClassInIntf
                                           write fRootMustBeClassInIntf;
    property ObjectsMustExists: boolean read fObjectsMustExists
                                       write fObjectsMustExists;
  end;

  { TLfmFixer }

  TLFMFixer = class(TLFMChecker)
  private
    // References to controls in UI:
    fPropReplaceGrid: TStringGrid;
  protected
    function ShowRepairLFMWizard: TModalResult; override;
  public
    constructor Create(APascalBuffer, ALFMBuffer: TCodeBuffer;
                       const AOnOutput: TOnAddFilteredLine);
    destructor Destroy; override;
    function Repair: TModalResult;
  end;


  { TFixLFMDialog }

  TFixLFMDialog = class(TForm)
    ReplaceAllButton: TBitBtn;
    CancelButton: TBitBtn;
    ErrorsGroupBox: TGroupBox;
    ErrorsListBox: TListBox;
    PropertyReplaceGroupBox: TGroupBox;
    NoteLabel: TLabel;
    LFMGroupBox: TGroupBox;
    LFMSynEdit: TSynEdit;
    BtnPanel: TPanel;
    RemoveAllButton: TBitBtn;
    Splitter1: TSplitter;
    PropertyReplaceGrid: TStringGrid;
    SynLFMSyn1: TSynLFMSyn;
    procedure ErrorsListBoxClick(Sender: TObject);
    procedure RemoveAllButtonClick(Sender: TObject);
    procedure ReplaceAllButtonClick(Sender: TObject);
    procedure LFMSynEditSpecialLineMarkup(Sender: TObject;
      Line: integer; var Special: boolean; AMarkup: TSynSelectedColor);
    procedure CheckLFMDialogCREATE(Sender: TObject);
  private
//    fLfmChecker: TLFMChecker;
    fLfmFixer: TLFMFixer;
    procedure SetupComponents;
  public
    constructor Create(AOwner: TComponent; ALfmFixer: TLFMFixer);
    destructor Destroy; override;
  end;


implementation

{$R *.lfm}

type
  TLFMChangeEntry = class
  public
    StartPos, EndPos: integer;
    NewText: string;
  end;

{ TLFMChecker }

constructor TLFMChecker.Create(APascalBuffer, ALFMBuffer: TCodeBuffer;
                             const AOnOutput: TOnAddFilteredLine);
begin
  fPascalBuffer:=APascalBuffer;
  fLFMBuffer:=ALFMBuffer;
  fOnOutput:=AOnOutput;
  fRootMustBeClassInIntf:=false;
  fObjectsMustExists:=false;
end;

destructor TLFMChecker.Destroy;
begin
  inherited Destroy;
end;

function TLFMChecker.ShowRepairLFMWizard: TModalResult;
//var
//  FixLFMDialog: TFixLFMDialog;
begin
  Result:=mrCancel;
{  FixLFMDialog:=TFixLFMDialog.Create(nil, self);
  try
    fLFMSynEdit:=FixLFMDialog.LFMSynEdit;
    fErrorsListBox:=FixLFMDialog.ErrorsListBox;
    fPropReplaceGrid:=FixLFMDialog.PropertyReplaceGrid;
    LoadLFM;
    Result:=FixLFMDialog.ShowModal;
  finally
    FixLFMDialog.Free;
  end; }
end;

procedure TLFMChecker.LoadLFM;
begin
  fLFMSynEdit.Lines.Text:=fLFMBuffer.Source;
  FillErrorsListBox;
end;

function TLFMChecker.Repair: TModalResult;
begin
  Result:=mrCancel;
  if not CheckUnit then exit;
  if CodeToolBoss.CheckLFM(fPascalBuffer,fLFMBuffer,fLFMTree,
                           fRootMustBeClassInIntf,fObjectsMustExists)
  then begin
    Result:=mrOk;
    exit;
  end;
  Result:=FixMissingComponentClasses;
  if Result in [mrAbort,mrOk] then exit;
  WriteLFMErrors;
  Result:=ShowRepairLFMWizard;
end;

procedure TLFMChecker.WriteUnitError(Code: TCodeBuffer; X, Y: integer;
  const ErrorMessage: string);
var
  Dir: String;
  Filename: String;
  Msg: String;
begin
  if not Assigned(fOnOutput) then exit;
  if Code=nil then
    Code:=fPascalBuffer;
  Dir:=ExtractFilePath(Code.Filename);
  Filename:=ExtractFilename(Code.Filename);
  Msg:=Filename
       +'('+IntToStr(Y)+','+IntToStr(X)+')'
       +' Error: '
       +ErrorMessage;
  fOnOutput(Msg,Dir,-1,nil);
end;

procedure TLFMChecker.WriteCodeToolsError;
begin
  WriteUnitError(CodeToolBoss.ErrorCode,CodeToolBoss.ErrorColumn,
    CodeToolBoss.ErrorLine,CodeToolBoss.ErrorMessage);
end;

procedure TLFMChecker.WriteLFMErrors;
var
  CurError: TLFMError;
  Dir: String;
  Msg: String;
  Filename: String;
begin
  if not Assigned(fOnOutput) then exit;
  CurError:=fLFMTree.FirstError;
  Dir:=ExtractFilePath(fLFMBuffer.Filename);
  Filename:=ExtractFilename(fLFMBuffer.Filename);
  while CurError<>nil do begin
    Msg:=Filename
         +'('+IntToStr(CurError.Caret.Y)+','+IntToStr(CurError.Caret.X)+')'
         +' Error: '
         +CurError.ErrorMessage;
    fOnOutput(Msg,Dir,-1,nil);
    CurError:=CurError.NextError;
  end;
end;

function TLFMChecker.FixMissingComponentClasses: TModalResult;
// returns true, if after adding units to uses section all errors are fixed
var
  CurError: TLFMError;
  MissingObjectTypes: TStringList;
  TypeName: String;
  RegComp: TRegisteredComponent;
  i: Integer;
begin
  Result:=mrCancel;
  MissingObjectTypes:=TStringList.Create;
  try
    // collect all missing object types
    CurError:=fLFMTree.FirstError;
    while CurError<>nil do begin
      if CurError.IsMissingObjectType then begin
        TypeName:=(CurError.Node as TLFMObjectNode).TypeName;
        if MissingObjectTypes.IndexOf(TypeName)<0 then
          MissingObjectTypes.Add(TypeName);
      end;
      CurError:=CurError.NextError;
    end;
    // Missing object types in unit.

    // keep all object types with a registered component class
    for i:=MissingObjectTypes.Count-1 downto 0 do begin
      RegComp:=IDEComponentPalette.FindComponent(MissingObjectTypes[i]);
      if (RegComp=nil) or (RegComp.GetUnitName='') then
        MissingObjectTypes.Delete(i);
    end;
    if MissingObjectTypes.Count=0 then exit;
    // Missing object types, but luckily found in IDE.

    // there are missing object types with registered component classes
    Result:=PackageEditingInterface.AddUnitDependenciesForComponentClasses(
         fPascalBuffer.Filename,MissingObjectTypes);
    if Result<>mrOk then exit;

    // check LFM again
    if CodeToolBoss.CheckLFM(fPascalBuffer,fLFMBuffer,fLFMTree,
                             fRootMustBeClassInIntf,fObjectsMustExists)
    then begin
      Result:=mrOk;
    end else begin
      Result:=mrCancel;
    end;
  finally
    MissingObjectTypes.Free;
  end;
end;

function TLFMChecker.CheckUnit: boolean;
var
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
  ErrorMsg: string;
  MissingUnits: TStrings;
  s: String;
begin
  Result:=false;
  // check syntax
  if not CodeToolBoss.CheckSyntax(fPascalBuffer,NewCode,NewX,NewY,NewTopLine,ErrorMsg)
  then begin
    WriteUnitError(NewCode,NewX,NewY,ErrorMsg);
    exit;
  end;
  // check used units
  MissingUnits:=nil;
  try
    if not CodeToolBoss.FindMissingUnits(fPascalBuffer,MissingUnits,false,false)
    then begin
      WriteCodeToolsError;
      exit;
    end;
    if (MissingUnits<>nil) and (MissingUnits.Count>0) then begin
      s:=StringListToText(MissingUnits,',');
      WriteUnitError(fPascalBuffer,1,1,'Units not found: '+s);
      exit;
    end;
  finally
    MissingUnits.Free;
  end;
  if NewTopLine=0 then ;
  Result:=true;
end;

function TLFMChecker.RemoveAll: TModalResult;
var
  CurError: TLFMError;
  DeleteNode: TLFMTreeNode;
  StartPos, EndPos: integer;
  Replacements: TList;
  i: integer;
begin
  Result:=mrNone;
  Replacements:=TList.Create;
  try
    // automatically delete each error location
    CurError:=fLFMTree.LastError;
    while CurError<>nil do begin
      DeleteNode:=CurError.FindContextNode;
      if (DeleteNode<>nil) and (DeleteNode.Parent<>nil) then begin
        FindNiceNodeBounds(DeleteNode,StartPos,EndPos);
        AddReplacement(Replacements,StartPos,EndPos,'');
      end;
      CurError:=CurError.PrevError;
    end;
    if ApplyReplacements(Replacements) then
      Result:=mrOk;
  finally
    for i := 0 to Replacements.Count - 1 do
      TObject(Replacements[i]).Free;
    Replacements.Free;
  end;
end;

procedure TLFMChecker.FindNiceNodeBounds(LFMNode: TLFMTreeNode;
  var StartPos, EndPos: integer);
var
  Src: String;
begin
  Src:=fLFMBuffer.Source;
  StartPos:=FindLineEndOrCodeInFrontOfPosition(Src,LFMNode.StartPos,1,false,true);
  EndPos:=FindLineEndOrCodeInFrontOfPosition(Src,LFMNode.EndPos,1,false,true);
  EndPos:=FindLineEndOrCodeAfterPosition(Src,EndPos,length(Src),false);
end;

function TLFMChecker.FindListBoxError: TLFMError;
var
  i: Integer;
begin
  Result:=nil;
  i:=fErrorsListBox.ItemIndex;
  if (i<0) or (i>=fErrorsListBox.Items.Count) then exit;
  Result:=fLFMTree.FirstError;
  while Result<>nil do begin
    if i=0 then exit;
    Result:=Result.NextError;
    dec(i);
  end;
end;

procedure TLFMChecker.JumpToError(LFMError: TLFMError);
begin
  if LFMError=nil then exit;
  fLFMSynEdit.CaretXY:=LFMError.Caret;
end;

procedure TLFMChecker.AddReplacement(LFMChangeList: TList;
  StartPos, EndPos: integer; const NewText: string);
var
  Entry: TLFMChangeEntry;
  NewEntry: TLFMChangeEntry;
  i: Integer;
begin
  if StartPos>EndPos then
    RaiseException('TCheckLFMDialog.AddReplaceMent StartPos>EndPos');

  // check for intersection
  for i:=0 to LFMChangeList.Count-1 do begin
    Entry:=TLFMChangeEntry(LFMChangeList[i]);
    if ((Entry.StartPos<EndPos) and (Entry.EndPos>StartPos)) then begin
      // New and Entry intersects
      if (Entry.NewText='') and (NewText='') then begin
        // both are deletes => combine
        StartPos:=Min(StartPos,Entry.StartPos);
        EndPos:=Max(EndPos,Entry.EndPos);
      end else begin
        // not allowed
        RaiseException('TCheckLFMDialog.AddReplaceMent invalid Intersection');
      end;
    end;
  end;

  // combine deletions
  if NewText='' then begin
    for i:=LFMChangeList.Count-1 downto 0 do begin
      Entry:=TLFMChangeEntry(LFMChangeList[i]);
      if ((Entry.StartPos<EndPos) and (Entry.EndPos>StartPos)) then begin
        // New and Entry intersects
        // -> remove Entry
        LFMChangeList.Delete(i);
        Entry.Free;
      end;
    end;
  end;

  // insert new entry
  NewEntry:=TLFMChangeEntry.Create;
  NewEntry.NewText:=NewText;
  NewEntry.StartPos:=StartPos;
  NewEntry.EndPos:=EndPos;
  if LFMChangeList.Count=0 then begin
    LFMChangeList.Add(NewEntry);
  end else begin
    for i:=0 to LFMChangeList.Count-1 do begin
      Entry:=TLFMChangeEntry(LFMChangeList[i]);
      if EndPos<=Entry.StartPos then begin
        // insert in front
        LFMChangeList.Insert(i,NewEntry);
        break;
      end else if i=LFMChangeList.Count-1 then begin
        // insert behind
        LFMChangeList.Add(NewEntry);
        break;
      end;
    end;
  end;
end;

function TLFMChecker.ApplyReplacements(LfmChangeList: TList): boolean;
var
  i: Integer;
  Entry: TLFMChangeEntry;
begin
  Result:=false;
  for i:=LfmChangeList.Count-1 downto 0 do begin
    Entry:=TLFMChangeEntry(LfmChangeList[i]);
//    DebugLn('TCheckLFMDialog.ApplyReplacements A ',IntToStr(i),' ',
//      IntToStr(Entry.StartPos),',',IntToStr(Entry.EndPos),
//      ' "',copy(fLFMBuffer.Source,Entry.StartPos,Entry.EndPos-Entry.StartPos),'" -> "',Entry.NewText,'"');
    fLFMBuffer.Replace(Entry.StartPos,Entry.EndPos-Entry.StartPos,Entry.NewText);
  end;
  //writeln(fLFMBuffer.Source);
  Result:=true;
end;

procedure TLFMChecker.FillErrorsListBox;
var
  CurError: TLFMError;
  Filename: String;
  Msg: String;
begin
  fErrorsListBox.Items.BeginUpdate;
  fErrorsListBox.Items.Clear;
  if fLFMTree<>nil then begin
    Filename:=ExtractFileName(fLFMBuffer.Filename);
    CurError:=fLFMTree.FirstError;
    while CurError<>nil do begin
      Msg:=Filename
           +'('+IntToStr(CurError.Caret.Y)+','+IntToStr(CurError.Caret.X)+')'
           +' Error: '
           +CurError.ErrorMessage;
      fErrorsListBox.Items.Add(Msg);
      CurError:=CurError.NextError;
    end;
  end;
  fErrorsListBox.Items.EndUpdate;
end;

function TLFMChecker.AutomaticFixIsPossible: boolean;
var
  CurError: TLFMError;
begin
  Result:=true;
  CurError:=fLFMTree.FirstError;
  while CurError<>nil do begin
    if CurError.ErrorType in [lfmeNoError,lfmeIdentifierNotFound,
      lfmeObjectNameMissing,lfmeObjectIncompatible,lfmePropertyNameMissing,
      lfmePropertyHasNoSubProperties,lfmeIdentifierNotPublished]
    then begin
      // these things can be fixed automatically
    end else begin
      // these not
      Result:=false;
      exit;
    end;
    CurError:=CurError.NextError;
  end;
end;


{ TLFMFixer }

constructor TLFMFixer.Create(APascalBuffer, ALFMBuffer: TCodeBuffer;
  const AOnOutput: TOnAddFilteredLine);
begin
  inherited Create(APascalBuffer, ALFMBuffer, AOnOutput);

end;

destructor TLFMFixer.Destroy;
begin
  inherited Destroy;
end;

function TLFMFixer.ShowRepairLFMWizard: TModalResult;
var
  FixLFMDialog: TFixLFMDialog;
begin
  Result:=mrCancel;
  FixLFMDialog:=TFixLFMDialog.Create(nil, self);
  try
    fLFMSynEdit:=FixLFMDialog.LFMSynEdit;
    fErrorsListBox:=FixLFMDialog.ErrorsListBox;
    fPropReplaceGrid:=FixLFMDialog.PropertyReplaceGrid;
    LoadLFM;
    Result:=FixLFMDialog.ShowModal;
  finally
    FixLFMDialog.Free;
  end;
end;

function TLFMFixer.Repair: TModalResult;
begin
  Result:=inherited Repair;
end;


{ TFixLFMDialog }

constructor TFixLFMDialog.Create(AOwner: TComponent; ALfmFixer: TLFMFixer);
begin
  inherited Create(AOwner);
  fLfmFixer:=ALfmFixer;
end;

destructor TFixLFMDialog.Destroy;
begin
  inherited Destroy;
end;

procedure TFixLFMDialog.ReplaceAllButtonClick(Sender: TObject);
begin
  ;
end;

procedure TFixLFMDialog.RemoveAllButtonClick(Sender: TObject);
begin
  ModalResult:=fLfmFixer.RemoveAll;
end;

procedure TFixLFMDialog.ErrorsListBoxClick(Sender: TObject);
begin
  fLfmFixer.JumpToError(fLfmFixer.FindListBoxError);
end;

procedure TFixLFMDialog.LFMSynEditSpecialLineMarkup(Sender: TObject;
  Line: integer; var Special: boolean; AMarkup: TSynSelectedColor);
var
  CurError: TLFMError;
begin
  CurError:=fLfmFixer.fLFMTree.FindErrorAtLine(Line);
  if CurError = nil then Exit;
  Special := True;
  EditorOpts.SetMarkupColor(SynLFMSyn1, ahaErrorLine, AMarkup);
end;

procedure TFixLFMDialog.CheckLFMDialogCREATE(Sender: TObject);
begin
  Caption:=lisFixLFMFile;
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,600,400);
  SetupComponents;
end;

procedure TFixLFMDialog.SetupComponents;
const // Will be moved to LazarusIDEStrConsts
  lisReplaceAllProperties = 'Replace all properties';
begin
  NoteLabel.Caption:=lisTheLFMLazarusFormFileContainsInvalidPropertiesThis;
  ErrorsGroupBox.Caption:=lisErrors;
  LFMGroupBox.Caption:=lisLFMFile;
  RemoveAllButton.Caption:=lisRemoveAllInvalidProperties;
  RemoveAllButton.LoadGlyphFromLazarusResource('laz_delete');
  ReplaceAllButton.Caption:=lisReplaceAllProperties;
  ReplaceAllButton.LoadGlyphFromLazarusResource('laz_refresh');
  EditorOpts.GetHighlighterSettings(SynLFMSyn1);
  EditorOpts.GetSynEditSettings(LFMSynEdit);
end;


end.

