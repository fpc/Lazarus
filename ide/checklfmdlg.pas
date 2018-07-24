{
 /***************************************************************************
                            checklfmdlg.pas
                            ---------------

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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit CheckLFMDlg;

{$mode objfpc}{$H+}

interface

uses
  // FCL+LCL
  Classes, SysUtils, Math, TypInfo, contnrs,
  LCLProc, LResources, Forms, Controls,
  Dialogs, Buttons, StdCtrls, ExtCtrls,
  // components
  SynHighlighterLFM, SynEdit, BasicCodeTools, CodeCache, CodeToolManager,
  SynEditMiscClasses, LFMTrees,
  // IDEIntf
  IDEExternToolIntf, PackageIntf, IDEWindowIntf, PropEdits, PropEditUtils,
  IDEMsgIntf, IDEDialogs, ComponentReg,
  // IDE
  CustomFormEditor, LazarusIDEStrConsts, IDEImagesIntf,
  IDEProcs, EditorOptions, SourceMarks, JITForms;

type

  { TLfmChecker }

  TLFMChecker = class
  private
    fShowMessages: boolean;
    procedure WriteUnitError(Code: TCodeBuffer; X, Y: integer; const ErrorMessage: string);
    procedure WriteCodeToolsError;
    function CheckUnit: boolean;
    function ShowRepairLFMWizard: TModalResult; // Show the interactive user interface.
  protected
    fPascalBuffer: TCodeBuffer;
    fLFMBuffer: TCodeBuffer;
    fLFMTree: TLFMTree;
    fRootMustBeClassInUnit: boolean;
    fRootMustBeClassInIntf: boolean;
    fObjectsMustExist: boolean;
    // References to controls in UI:
    fLFMSynEdit: TSynEdit;
    fErrorsListBox: TListBox;
    // Refactored and moved from dialog class:
    procedure LoadLFM;
    function RemoveAll: TModalResult;
    procedure FindNiceNodeBounds(LFMNode: TLFMTreeNode;
                                 out StartPos, EndPos: integer);
    function FindListBoxError: TLFMError;
    procedure WriteLFMErrors;
    function FindAndFixMissingComponentClasses: TModalResult;
    function FixMissingComponentClasses(aMissingTypes: TStringList): TModalResult; virtual;
    procedure FillErrorsListBox;
    procedure JumpToError(LFMError: TLFMError);
    procedure AddReplacement(LFMChangeList: TObjectList; StartPos, EndPos: integer;
                             const NewText: string);
    function ApplyReplacements(LFMChangeList: TList): boolean;
  public
    constructor Create(APascalBuffer, ALFMBuffer: TCodeBuffer);
    destructor Destroy; override;
    function Repair: TModalResult;
    function AutomaticFixIsPossible: boolean;
  public
    property PascalBuffer: TCodeBuffer read fPascalBuffer;
    property LFMBuffer: TCodeBuffer read fLFMBuffer;
    property ShowMessages: boolean read fShowMessages write fShowMessages;
    property RootMustBeClassInUnit: boolean read fRootMustBeClassInUnit
                                           write fRootMustBeClassInUnit;
    property RootMustBeClassInIntf: boolean read fRootMustBeClassInIntf
                                           write fRootMustBeClassInIntf;
    property ObjectsMustExist: boolean read fObjectsMustExist
                                       write fObjectsMustExist;
  end;

  { TCheckLFMDialog }

  TCheckLFMDialog = class(TForm)
    CancelButton: TBitBtn;
    ErrorsGroupBox: TGroupBox;
    ErrorsListBox: TListBox;
    NoteLabel: TLabel;
    LFMGroupBox: TGroupBox;
    LFMSynEdit: TSynEdit;
    BtnPanel: TPanel;
    RemoveAllButton: TBitBtn;
    SynLFMSyn1: TSynLFMSyn;
    procedure ErrorsListBoxClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure LFMSynEditSpecialLineMarkup(Sender: TObject; Line: integer;
      var Special: boolean; AMarkup: TSynSelectedColor);
    procedure RemoveAllButtonClick(Sender: TObject);
    procedure CheckLFMDialogCREATE(Sender: TObject);
  private
    fLfmChecker: TLFMChecker;
    procedure SetupComponents;
  public
    constructor Create(AOwner: TComponent; ALfmChecker: TLFMChecker); reintroduce;
    destructor Destroy; override;
  end;

// check and repair lfm files
function QuickCheckLFMBuffer({%H-}PascalBuffer, LFMBuffer: TCodeBuffer;
  out LFMType, LFMComponentName, LFMClassName: string;
  out LCLVersion: string;
  out MissingClasses: TStrings// e.g. MyFrame2:TMyFrame
  ): TModalResult;
// Now this is just a wrapper for designer/changeclassdialog. Could be moved there.
function RepairLFMBuffer(PascalBuffer, LFMBuffer: TCodeBuffer;
  RootMustBeClassInUnit, RootMustBeClassInIntf,
  ObjectsMustExist: boolean): TModalResult;
// dangling events
function RemoveDanglingEvents(RootComponent: TComponent;
  PascalBuffer: TCodeBuffer; OkOnCodeErrors: boolean;
  out ComponentModified: boolean): TModalResult;
procedure ClearDanglingEvents(ListOfPInstancePropInfo: TFPList);

implementation

{$R *.lfm}

type
  TLFMChangeEntry = class
  public
    StartPos, EndPos: integer;
    NewText: string;
  end;

function QuickCheckLFMBuffer(PascalBuffer, LFMBuffer: TCodeBuffer;
  out LFMType, LFMComponentName, LFMClassName: string;
  out LCLVersion: string; out MissingClasses: TStrings): TModalResult;
var
  LFMTree: TLFMTree;
  
  procedure FindLCLVersion;
  var
    LCLVersionNode: TLFMPropertyNode;
    LCLVersionValueNode: TLFMValueNode;
  begin
    // first search the version
    LCLVersionNode:=LFMTree.FindProperty('LCLVersion',LFMTree.Root);
    //DebugLn(['QuickCheckLFMBuffer LCLVersionNode=',LCLVersionNode<>nil]);
    if (LCLVersionNode<>nil) and (LCLVersionNode.FirstChild is TLFMValueNode) then
    begin
      LCLVersionValueNode:=TLFMValueNode(LCLVersionNode.FirstChild);
      //DebugLn(['QuickCheckLFMBuffer ',TLFMValueTypeNames[LCLVersionValueNode.ValueType]]);
      if LCLVersionValueNode.ValueType=lfmvString then begin
        LCLVersion:=LCLVersionValueNode.ReadString;
        //DebugLn(['QuickCheckLFMBuffer LCLVersion=',LCLVersion]);
      end;
    end;
  end;
  
  procedure FindMissingClass(ObjNode: TLFMObjectNode);
  var
    i: Integer;
    AClassName: String;
    RegComp: TRegisteredComponent;
  begin
    AClassName:=ObjNode.TypeName;
    // search in already missing classes
    if (MissingClasses<>nil) then begin
      for i:=0 to MissingClasses.Count-1 do
        if SysUtils.CompareText(AClassName,MissingClasses[i])=0 then
          exit;
    end;
    // ToDo: search only in used packages
    // search in designer base classes
    if BaseFormEditor1.FindDesignerBaseClassByName(AClassName,true)<>nil then
      exit;
    // search in registered classes
    RegComp:=IDEComponentPalette.FindComponent(ObjNode.TypeName);
    if (RegComp<>nil) and (RegComp.GetUnitName<>'') then exit;
    // search in global registered classes
    if GetClass(ObjNode.TypeName) <> nil then
      Exit;
    // class is missing
    DebugLn(['FindMissingClass ',ObjNode.Name,':',ObjNode.TypeName,' IsInherited=',ObjNode.IsInherited]);
    if MissingClasses=nil then
      MissingClasses:=TStringList.Create;
    MissingClasses.Add(AClassName);
  end;
  
  procedure FindMissingClasses;
  var
    Node: TLFMTreeNode;
    ObjNode: TLFMObjectNode absolute Node;
  begin
    Node := LFMTree.Root;
    if Node = nil then Exit;
    // skip root
    Node := Node.Next;
    // check all other
    while Node <> nil do
    begin
      if Node is TLFMObjectNode then
      begin
        FindMissingClass(ObjNode);
        Node := Node.Next(ObjNode.IsInline); // skip children if node is inline
      end
      else
        Node := Node.Next;
    end;
  end;
  
begin
  //DebugLn(['QuickCheckLFMBuffer LFMBuffer=',LFMBuffer.Filename]);
  LCLVersion:='';
  MissingClasses:=nil;

  // read header
  ReadLFMHeader(LFMBuffer.Source,LFMType,LFMComponentName,LFMClassName);

  // parse tree
  LFMTree:=DefaultLFMTrees.GetLFMTree(LFMBuffer,true);
  if not LFMTree.ParseIfNeeded then begin
    DebugLn(['QuickCheckLFMBuffer LFM error: ',LFMTree.FirstErrorAsString]);
    exit(mrCancel);
  end;
  
  //LFMTree.WriteDebugReport;
  FindLCLVersion;
  FindMissingClasses;
  
  Result:=mrOk;
end;

function RepairLFMBuffer(PascalBuffer, LFMBuffer: TCodeBuffer;
  RootMustBeClassInUnit, RootMustBeClassInIntf,
  ObjectsMustExist: boolean): TModalResult;
var
  LFMChecker: TLFMChecker;
begin
  LFMChecker:=TLFMChecker.Create(PascalBuffer,LFMBuffer);
  try
    LFMChecker.RootMustBeClassInUnit:=RootMustBeClassInUnit;
    LFMChecker.RootMustBeClassInIntf:=RootMustBeClassInIntf;
    LFMChecker.ObjectsMustExist:=ObjectsMustExist;
    Result:=LFMChecker.Repair;
  finally
    LFMChecker.Free;
  end;
end;

function RemoveDanglingEvents(RootComponent: TComponent;
  PascalBuffer: TCodeBuffer; OkOnCodeErrors: boolean; out
  ComponentModified: boolean): TModalResult;
var
  ListOfPInstancePropInfo: TFPList;
  p: PInstancePropInfo;
  i: Integer;
  CurMethod: TMethod;
  JitMethod: TJITMethod;
  LookupRoot: TPersistent;
  CurMethodName: String;
  s: String;
  MsgResult: TModalResult;
begin
  ComponentModified:=false;
  ListOfPInstancePropInfo:=nil;
  try
    // find all dangling events
    //debugln('RemoveDanglingEvents A ',PascalBuffer.Filename,' ',DbgSName(RootComponent));
    if not CodeToolBoss.FindDanglingComponentEvents(PascalBuffer,
      RootComponent.ClassName,RootComponent,false,true,ListOfPInstancePropInfo,
      @BaseFormEditor1.OnGetDanglingMethodName)
    then begin
      //debugln('RemoveDanglingEvents Errors in code');
      if OkOnCodeErrors then
        exit(mrOk)
      else
        exit(mrCancel);
    end;
    if ListOfPInstancePropInfo=nil then
      exit(mrOk);

    // show the user the list of dangling events
    //debugln('RemoveDanglingEvents Dangling Events: Count=',dbgs(ListOfPInstancePropInfo.Count));
    s:='';
    for i := 0 to ListOfPInstancePropInfo.Count-1 do
    begin
      p := PInstancePropInfo(ListOfPInstancePropInfo[i]);
      CurMethod := GetMethodProp(p^.Instance, p^.PropInfo);
      LookupRoot := GetLookupRootForComponent(TComponent(p^.Instance));
      if IsJITMethod(CurMethod) then
      begin
        JitMethod := TJITMethod(CurMethod.Data);
        if JitMethod.TheClass <> LookupRoot.ClassType then
          Continue;
      end;
      CurMethodName := GlobalDesignHook.GetMethodName(CurMethod, p^.Instance);
      s := s + DbgSName(p^.Instance) + ' ' + p^.PropInfo^.Name + '=' + CurMethodName + LineEnding;
    end;
    //debugln('RemoveDanglingEvents ',s);

    if s = '' then
      Exit(mrOk);

    MsgResult:=IDEQuestionDialog(lisMissingEvents,
      Format(lisTheFollowingMethodsUsedByAreNotInTheSourceRemoveTh, [DbgSName(
        RootComponent), LineEnding, PascalBuffer.Filename, LineEnding+LineEnding, s, LineEnding]),
      mtConfirmation, [mrYes, lisRemoveThem,
                       mrIgnore, lisKeepThemAndContinue,
                       mrAbort]);
     if MsgResult=mrYes then begin
       ClearDanglingEvents(ListOfPInstancePropInfo);
       ComponentModified:=true;
     end else if MsgResult=mrIgnore then
       exit(mrOk)
     else
       exit(mrAbort);
  finally
    FreeListOfPInstancePropInfo(ListOfPInstancePropInfo);
  end;
  Result:=mrOk;
end;

procedure ClearDanglingEvents(ListOfPInstancePropInfo: TFPList);
const
  EmptyMethod: TMethod = (code:nil; data:nil);
var
  i: Integer;
  p: PInstancePropInfo;
begin
  if ListOfPInstancePropInfo=nil then exit;
  for i:=0 to ListOfPInstancePropInfo.Count-1 do begin
    p:=PInstancePropInfo(ListOfPInstancePropInfo[i]);
    debugln('ClearDanglingEvents ',DbgSName(p^.Instance),' ',p^.PropInfo^.Name);
    SetMethodProp(p^.Instance,p^.PropInfo,EmptyMethod);
  end;
end;

{ TLFMChecker }

constructor TLFMChecker.Create(APascalBuffer, ALFMBuffer: TCodeBuffer);
begin
  fPascalBuffer:=APascalBuffer;
  fLFMBuffer:=ALFMBuffer;
  fRootMustBeClassInIntf:=false;
  fObjectsMustExist:=false;
end;

destructor TLFMChecker.Destroy;
begin
  inherited Destroy;
end;

function TLFMChecker.ShowRepairLFMWizard: TModalResult;
var
  CheckLFMDialog: TCheckLFMDialog;
begin
  Result:=mrCancel;
  CheckLFMDialog:=TCheckLFMDialog.Create(nil, self);
  try
    fLFMSynEdit:=CheckLFMDialog.LFMSynEdit;
    fErrorsListBox:=CheckLFMDialog.ErrorsListBox;
    LoadLFM;
    Result:=CheckLFMDialog.ShowModal;
  finally
    CheckLFMDialog.Free;
  end;
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
               fRootMustBeClassInUnit,fRootMustBeClassInIntf,fObjectsMustExist)
  then
    exit(mrOk);
  Result:=FindAndFixMissingComponentClasses;
  if Result=mrAbort then exit;
  // check LFM again
  if CodeToolBoss.CheckLFM(fPascalBuffer,fLFMBuffer,fLFMTree,
             fRootMustBeClassInUnit,fRootMustBeClassInIntf,fObjectsMustExist)
  then
    exit(mrOk);
  WriteLFMErrors;
  Result:=ShowRepairLFMWizard;
end;

procedure TLFMChecker.WriteUnitError(Code: TCodeBuffer; X, Y: integer;
  const ErrorMessage: string);
var
  Filename: String;
begin
  if (not ShowMessages) or (IDEMessagesWindow=nil) then exit;
  if Code=nil then
    Code:=fPascalBuffer;
  Filename:=ExtractFilename(Code.Filename);
  IDEMessagesWindow.AddCustomMessage(mluError,ErrorMessage,Filename,Y,X,'Codetools');
  Application.ProcessMessages;
end;

procedure TLFMChecker.WriteCodeToolsError;
begin
  WriteUnitError(CodeToolBoss.ErrorCode,CodeToolBoss.ErrorColumn,
    CodeToolBoss.ErrorLine,CodeToolBoss.ErrorMessage);
end;

procedure TLFMChecker.WriteLFMErrors;
var
  CurError: TLFMError;
  Filename: String;
begin
  if (not ShowMessages) or (IDEMessagesWindow=nil) then exit;
  CurError:=fLFMTree.FirstError;
  Filename:=ExtractFilename(fLFMBuffer.Filename);
  while CurError<>nil do begin
    IDEMessagesWindow.AddCustomMessage(mluError,CurError.ErrorMessage,
      Filename,CurError.Caret.Y,CurError.Caret.X);
    CurError:=CurError.NextError;
  end;
  Application.ProcessMessages;
end;

function TLFMChecker.FindAndFixMissingComponentClasses: TModalResult;
// returns true, if after adding units to uses section all errors are fixed
var
  CurError: TLFMError;
  MissingObjectTypes: TStringList;
  TypeName: String;
  RegComp: TRegisteredComponent;
  i: Integer;
begin
  Result:=mrOK;
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

    // keep missing object types only with a registered component class
    for i:=MissingObjectTypes.Count-1 downto 0 do begin
      RegComp:=IDEComponentPalette.FindComponent(MissingObjectTypes[i]);
      if (RegComp=nil) or (RegComp.GetUnitName='') then
        MissingObjectTypes.Delete(i);
    end;
    // Now the list contains only types that are found in IDE.
    if MissingObjectTypes.Count>0 then
      Result:=FixMissingComponentClasses(MissingObjectTypes); // Fix them.
  finally
    MissingObjectTypes.Free;
  end;
end;

function TLFMChecker.FixMissingComponentClasses(aMissingTypes: TStringList): TModalResult;
begin
  // add units for the missing object types with registered component classes
  Result:=PackageEditingInterface.AddUnitDependenciesForComponentClasses(
       fPascalBuffer.Filename, aMissingTypes);
end;

function TLFMChecker.CheckUnit: boolean;
var
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
  ErrorMsg: string;
  MissingUnits: TStrings;
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
      ErrorMsg:=StringListToText(MissingUnits,',');
      WriteUnitError(fPascalBuffer,1,1,'Units not found: '+ErrorMsg);
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
  Replacements: TObjectList;
begin
  Result:=mrNone;
  Replacements:=TObjectList.Create;
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
    Replacements.Free;
  end;
end;

procedure TLFMChecker.FindNiceNodeBounds(LFMNode: TLFMTreeNode;
  out StartPos, EndPos: integer);
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

procedure TLFMChecker.JumpToError(LFMError: TLFMError);
begin
  if LFMError=nil then exit;
  fLFMSynEdit.CaretXY:=LFMError.Caret;
end;

procedure TLFMChecker.AddReplacement(LFMChangeList: TObjectList;
  StartPos, EndPos: integer; const NewText: string);
var
  Entry: TLFMChangeEntry;
  NewEntry: TLFMChangeEntry;
  i: Integer;
begin
  if StartPos>EndPos then
    RaiseGDBException('TCheckLFMDialog.AddReplaceMent StartPos>EndPos');
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
        RaiseGDBException('TCheckLFMDialog.AddReplaceMent invalid Intersection');
      end;
    end;
  end;
  // combine deletions
  if NewText='' then begin
    for i:=LFMChangeList.Count-1 downto 0 do begin
      Entry:=TLFMChangeEntry(LFMChangeList[i]);
      if ((Entry.StartPos<EndPos) and (Entry.EndPos>StartPos)) then
        // New and Entry intersects -> remove Entry
        LFMChangeList.Delete(i);
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

function TLFMChecker.ApplyReplacements(LFMChangeList: TList): boolean;
var
  i: Integer;
  Entry: TLFMChangeEntry;
begin
  Result:=false;
  for i:=LfmChangeList.Count-1 downto 0 do begin
    Entry:=TLFMChangeEntry(LfmChangeList[i]);
    fLFMBuffer.Replace(Entry.StartPos,Entry.EndPos-Entry.StartPos,Entry.NewText);
  end;
  Result:=true;
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
      // these not: lfmeParseError, lfmeMissingRoot, lfmeEndNotFound
      Result:=false;
      exit;
    end;
    CurError:=CurError.NextError;
  end;
end;


{ TCheckLFMDialog }

constructor TCheckLFMDialog.Create(AOwner: TComponent; ALfmChecker: TLFMChecker);
begin
  inherited Create(AOwner);
  fLfmChecker:=ALfmChecker;
end;

destructor TCheckLFMDialog.Destroy;
begin
  inherited Destroy;
end;

procedure TCheckLFMDialog.CheckLFMDialogCREATE(Sender: TObject);
begin
  Caption:=lisFixLFMFile;
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,600,400);
  SetupComponents;
end;

procedure TCheckLFMDialog.RemoveAllButtonClick(Sender: TObject);
begin
  ModalResult:=fLfmChecker.RemoveAll;
end;

procedure TCheckLFMDialog.ErrorsListBoxClick(Sender: TObject);
begin
  fLfmChecker.JumpToError(fLfmChecker.FindListBoxError);
end;

procedure TCheckLFMDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TCheckLFMDialog.LFMSynEditSpecialLineMarkup(Sender: TObject;
  Line: integer; var Special: boolean; AMarkup: TSynSelectedColor);
var
  CurError: TLFMError;
begin
  CurError:=fLfmChecker.fLFMTree.FindErrorAtLine(Line);
  if CurError = nil then Exit;
  Special := True;
  EditorOpts.SetMarkupColor(SynLFMSyn1, ahaErrorLine, AMarkup);
end;

procedure TCheckLFMDialog.SetupComponents;
begin
  NoteLabel.Caption:=lisTheLFMLazarusFormFileContainsInvalidPropertiesThis;
  ErrorsGroupBox.Caption:=lisErrors;
  LFMGroupBox.Caption:=lisLFMFile;
  RemoveAllButton.Caption:=lisRemoveAllInvalidProperties;
  IDEImages.AssignImage(RemoveAllButton, 'laz_delete');
  CancelButton.Caption:=lisCancel;
  EditorOpts.GetHighlighterSettings(SynLFMSyn1);
  EditorOpts.GetSynEditSettings(LFMSynEdit);
end;


end.

