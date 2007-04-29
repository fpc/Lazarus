{  $Id$  }
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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit CheckLFMDlg;

{$mode objfpc}{$H+}

interface

uses
  // FCL+LCL
  Classes, SysUtils, Math, TypInfo, LCLProc, LResources, Forms, Controls,
  Graphics, Dialogs, Buttons, StdCtrls,
  // components
  SynHighlighterLFM, SynEdit, BasicCodeTools, CodeCache, CodeToolManager,
  LFMTrees,
  // IDE
  PropEdits, ComponentReg, PackageIntf, IDEWindowIntf,
  LazarusIDEStrConsts, OutputFilter, IDEProcs, IDEOptionDefs, EditorOptions;

type
  TCheckLFMDialog = class(TForm)
    CancelButton: TButton;
    ErrorsGroupBox: TGroupBox;
    ErrorsListBox: TListBox;
    NoteLabel: TLabel;
    LFMGroupBox: TGroupBox;
    LFMSynEdit: TSynEdit;
    RemoveAllButton: TButton;
    SynLFMSyn1: TSynLFMSyn;
    procedure ErrorsListBoxClick(Sender: TObject);
    procedure LFMSynEditSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure RemoveAllButtonClick(Sender: TObject);
    procedure CheckLFMDialogCREATE(Sender: TObject);
  private
    FLFMSource: TCodeBuffer;
    FLFMTree: TLFMTree;
    procedure SetLFMSource(const AValue: TCodeBuffer);
    procedure SetLFMTree(const AValue: TLFMTree);
    procedure SetupComponents;
    function FindListBoxError: TLFMError;
    procedure JumpToError(LFMError: TLFMError);
    procedure FindNiceNodeBounds(LFMNode: TLFMTreeNode;
                                 var StartPos, EndPos: integer);
    procedure AddReplacement(LFMChangeList: TList; StartPos, EndPos: integer;
                             const NewText: string);
    function ApplyReplacements(LFMChangeList: TList): boolean;
  public
    procedure LoadLFM;
    procedure FillErrorsListBox;
    function AutomaticFixIsPossible: boolean;
    property LFMTree: TLFMTree read FLFMTree write SetLFMTree;
    property LFMSource: TCodeBuffer read FLFMSource write SetLFMSource;
  end;
  
function CheckLFMBuffer(PascalBuffer, LFMBuffer: TCodeBuffer;
  const OnOutput: TOnAddFilteredLine;
  RootMustBeClassInIntf, ObjectsMustExists: boolean): TModalResult;
function CheckLFMText(PascalBuffer: TCodeBuffer; var LFMText: string;
  const OnOutput: TOnAddFilteredLine;
  RootMustBeClassInIntf, ObjectsMustExists: boolean): TModalResult;
function ShowRepairLFMWizard(LFMBuffer: TCodeBuffer;
  LFMTree: TLFMTree): TModalResult;

function RemoveDanglingEvents(RootComponent: TComponent;
  PascalBuffer: TCodeBuffer; OkOnCodeErrors: boolean;
  out ComponentModified: boolean): TModalResult;
procedure ClearDanglingEvents(ListOfPInstancePropInfo: TFPList);

implementation

type
  TLFMChangeEntry = class
  public
    StartPos, EndPos: integer;
    NewText: string;
  end;

function CheckLFMBuffer(PascalBuffer, LFMBuffer: TCodeBuffer;
  const OnOutput: TOnAddFilteredLine;
  RootMustBeClassInIntf, ObjectsMustExists: boolean): TModalResult;
var
  LFMTree: TLFMTree;
  
  procedure WriteUnitError(Code: TCodeBuffer; x, Y: integer;
    const ErrorMessage: string);
  var
    Dir: String;
    Filename: String;
    Msg: String;
  begin
    if not Assigned(OnOutput) then exit;
    if Code=nil then
      Code:=PascalBuffer;
    Dir:=ExtractFilePath(Code.Filename);
    Filename:=ExtractFilename(Code.Filename);
    Msg:=Filename
         +'('+IntToStr(Y)+','+IntToStr(X)+')'
         +' Error: '
         +ErrorMessage;
    debugln('WriteLFMErrors ',Msg);
    OnOutput(Msg,Dir,-1);
  end;
  
  procedure WriteCodeToolsError;
  begin
    WriteUnitError(CodeToolBoss.ErrorCode,CodeToolBoss.ErrorColumn,
      CodeToolBoss.ErrorLine,CodeToolBoss.ErrorMessage);
  end;

  procedure WriteLFMErrors;
  var
    CurError: TLFMError;
    Dir: String;
    Msg: String;
    Filename: String;
  begin
    if not Assigned(OnOutput) then exit;
    CurError:=LFMTree.FirstError;
    Dir:=ExtractFilePath(LFMBuffer.Filename);
    Filename:=ExtractFilename(LFMBuffer.Filename);
    while CurError<>nil do begin
      Msg:=Filename
           +'('+IntToStr(CurError.Caret.Y)+','+IntToStr(CurError.Caret.X)+')'
           +' Error: '
           +CurError.ErrorMessage;
      debugln('WriteLFMErrors ',Msg);
      OnOutput(Msg,Dir,-1);
      CurError:=CurError.NextError;
    end;
  end;
  
  function FixMissingComponentClasses: TModalResult;
  // returns true, if after adding units to uses section all errors are fixed
  var
    CurError: TLFMError;
    MissingObjectTypes: TStringList;
    TypeName: String;
    RegComp: TRegisteredComponent;
    i: Integer;
  begin
    DebugLn(['FixMissingComponentClasses ',LFMBuffer.Filename]);
    Result:=mrCancel;
    MissingObjectTypes:=TStringList.Create;
    try
      // collect all missing object types
      CurError:=LFMTree.FirstError;
      while CurError<>nil do begin
        if CurError.IsMissingObjectType then begin
          TypeName:=(CurError.Node as TLFMObjectNode).TypeName;
          if MissingObjectTypes.IndexOf(TypeName)<0 then
            MissingObjectTypes.Add(TypeName);
        end;
        CurError:=CurError.NextError;
      end;
      DebugLn(['FixMissingComponentClasses Missing object types in unit: ',MissingObjectTypes.Text]);
      
      // keep all object types with a registered component class
      for i:=MissingObjectTypes.Count-1 downto 0 do begin
        RegComp:=IDEComponentPalette.FindComponent(MissingObjectTypes[i]);
        if (RegComp=nil) or (RegComp.GetUnitName='') then
          MissingObjectTypes.Delete(i);
      end;
      if MissingObjectTypes.Count=0 then exit;
      DebugLn(['FixMissingComponentClasses Missing object types, but luckily found in IDE: ',MissingObjectTypes.Text]);

      // there are missing object types with registered component classes
      Result:=PackageEditingInterface.AddUnitDependenciesForComponentClasses(
           PascalBuffer.Filename,MissingObjectTypes);
      if Result<>mrOk then begin
        DebugLn(['FixMissingComponentClasses Failed to add dependencies for ',MissingObjectTypes.Text]);
        exit;
      end;

      // check LFM again
      LFMTree.Free;
      LFMTree:=nil;
      if CodeToolBoss.CheckLFM(PascalBuffer,LFMBuffer,LFMTree,
                                    RootMustBeClassInIntf,ObjectsMustExists)
      then begin
        DebugLn(['FixMissingComponentClasses Success: All found errors fixed']);
        Result:=mrOk;
      end else begin
        Result:=mrCancel;
      end;
    finally
      MissingObjectTypes.Free;
    end;
  end;
  
  function CheckUnit: boolean;
  var
    NewCode: TCodeBuffer;
    NewX, NewY, NewTopLine: integer;
    ErrorMsg: string;
    MissingUnits: TStrings;
    s: String;
  begin
    Result:=false;
    // check syntax
    DebugLn(['CheckUnit Checking syntax ...']);
    if not CodeToolBoss.CheckSyntax(PascalBuffer,NewCode,NewX,NewY,NewTopLine,
      ErrorMsg)
    then begin
      WriteUnitError(NewCode,NewX,NewY,ErrorMsg);
      exit;
    end;
    // check used units
    MissingUnits:=nil;
    try
      DebugLn(['CheckUnit Checking used units ...']);
      if not CodeToolBoss.FindMissingUnits(PascalBuffer,MissingUnits,false,
        false)
      then begin
        WriteCodeToolsError;
        exit;
      end;
      if (MissingUnits<>nil) and (MissingUnits.Count>0) then begin
        s:=StringListToText(MissingUnits,',');
        WriteUnitError(PascalBuffer,1,1,'Units not found: '+s);
        exit;
      end;
    finally
      MissingUnits.Free;
    end;
    if NewTopLine=0 then ;
    Result:=true;
  end;
  
begin
  Result:=mrCancel;
  
  if not CheckUnit then begin
    DebugLn(['CheckLFMBuffer failed parsing unit: ',PascalBuffer.Filename]);
    exit;
  end;

  LFMTree:=nil;
  try
    if CodeToolBoss.CheckLFM(PascalBuffer,LFMBuffer,LFMTree,
                             RootMustBeClassInIntf,ObjectsMustExists)
    then begin
      DebugLn(['CheckLFMBuffer no errors found']);
      Result:=mrOk;
      exit;
    end;
    Result:=FixMissingComponentClasses;
    if Result in [mrAbort,mrOk] then begin
      DebugLn(['CheckLFMBuffer all errors fixed']);
      exit;
    end;
    WriteLFMErrors;
    Result:=ShowRepairLFMWizard(LFMBuffer,LFMTree);
  finally
    LFMTree.Free;
  end;
end;

function CheckLFMText(PascalBuffer: TCodeBuffer; var LFMText: string;
  const OnOutput: TOnAddFilteredLine;
  RootMustBeClassInIntf, ObjectsMustExists: boolean): TModalResult;
var
  LFMBuf: TCodeBuffer;
begin
  Result:=mrCancel;
  LFMBuf:=CodeToolBoss.CreateTempFile('temp.lfm');
  try
    LFMBuf.Source:=LFMText;
    Result:=CheckLFMBuffer(PascalBuffer,LFMBuf,OnOutput,RootMustBeClassInIntf,
                           ObjectsMustExists);
    LFMText:=LFMBuf.Source;
  finally
    CodeToolBoss.ReleaseTempFile(LFMBuf);
  end;
end;

function ShowRepairLFMWizard(LFMBuffer: TCodeBuffer;
  LFMTree: TLFMTree): TModalResult;
var
  CheckLFMDialog: TCheckLFMDialog;
begin
  Result:=mrCancel;
  DebugLn(['ShowRepairLFMWizard START']);
  CheckLFMDialog:=TCheckLFMDialog.Create(nil);
  CheckLFMDialog.LFMTree:=LFMTree;
  CheckLFMDialog.LFMSource:=LFMBuffer;
  CheckLFMDialog.LoadLFM;
  Result:=CheckLFMDialog.ShowModal;
  DebugLn(['ShowRepairLFMWizard END']);
  CheckLFMDialog.Free;
end;

function RemoveDanglingEvents(RootComponent: TComponent;
  PascalBuffer: TCodeBuffer; OkOnCodeErrors: boolean; out
  ComponentModified: boolean): TModalResult;
var
  ListOfPInstancePropInfo: TFPList;
  p: PInstancePropInfo;
  i: Integer;
  CurMethod: TMethod;
  CurMethodName: String;
  PropName: String;
  s: String;
  MsgResult: TModalResult;
begin
  ListOfPInstancePropInfo:=nil;
  try
    // find all dangling events
    //debugln('RemoveDanglingEvents A ',PascalBuffer.Filename,' ',DbgSName(RootComponent));
    if not CodeToolBoss.FindDanglingComponentEvents(PascalBuffer,
      RootComponent.ClassName,RootComponent,false,true,ListOfPInstancePropInfo)
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
    for i:=0 to ListOfPInstancePropInfo.Count-1 do begin
      p:=PInstancePropInfo(ListOfPInstancePropInfo[i]);
      PropName:=p^.PropInfo^.Name;
      CurMethod:=GetMethodProp(p^.Instance,p^.PropInfo);
      CurMethodName:=GlobalDesignHook.GetMethodName(CurMethod,nil);
      s:=s+DbgSName(p^.Instance)+' '+PropName+'='+CurMethodName+#13;
    end;
    //debugln('RemoveDanglingEvents ',s);

    MsgResult:=QuestionDlg(lisMissingEvents,
      Format(lisTheFollowingMethodsUsedByAreNotInTheSourceRemoveTh, [DbgSName(
        RootComponent), #13, PascalBuffer.Filename, #13, #13, s, #13])
       ,mtConfirmation,
       [mrYes, lisRemoveThem, mrIgnore, lisKeepThemAndContinue, mrCancel],
         0);
     if MsgResult=mrYes then begin
       ClearDanglingEvents(ListOfPInstancePropInfo);
       ComponentModified:=true;
     end else if MsgResult=mrIgnore then
       exit(mrOk)
     else
       exit(mrCancel);
  finally
    FreeListOfPInstancePropInfo(ListOfPInstancePropInfo);
  end;
  Result:=mrOk;
end;

procedure ClearDanglingEvents(ListOfPInstancePropInfo: TFPList);
const
  EmtpyMethod: TMethod = (code:nil; data:nil);
var
  i: Integer;
  p: PInstancePropInfo;
begin
  if ListOfPInstancePropInfo=nil then exit;
  for i:=0 to ListOfPInstancePropInfo.Count-1 do begin
    p:=PInstancePropInfo(ListOfPInstancePropInfo[i]);
    debugln('ClearDanglingEvents ',DbgSName(p^.Instance),' ',p^.PropInfo^.Name);
    SetMethodProp(p^.Instance,p^.PropInfo,EmtpyMethod);
  end;
end;

{ TCheckLFMDialog }

procedure TCheckLFMDialog.RemoveAllButtonClick(Sender: TObject);
var
  CurError: TLFMError;
  DeleteNode: TLFMTreeNode;
  StartPos, EndPos: integer;
  Replacements: TList;
begin
  Replacements:=TList.Create;
  try
    // automatically delete each error location
    CurError:=LFMTree.LastError;
    while CurError<>nil do begin
      DeleteNode:=CurError.FindContextNode;
      if (DeleteNode<>nil) and (DeleteNode.Parent<>nil) then begin
        FindNiceNodeBounds(DeleteNode,StartPos,EndPos);
        AddReplacement(Replacements,StartPos,EndPos,'');
      end;
      CurError:=CurError.PrevError;
    end;

    if ApplyReplacements(Replacements) then
      ModalResult:=mrOk;
  finally
    Replacements.Free;
  end;
end;

procedure TCheckLFMDialog.ErrorsListBoxClick(Sender: TObject);
begin
  JumpToError(FindListBoxError);
end;

procedure TCheckLFMDialog.LFMSynEditSpecialLineColors(Sender: TObject;
  Line: integer; var Special: boolean; var FG, BG: TColor);
var
  CurError: TLFMError;
begin
  CurError:=LFMTree.FindErrorAtLine(Line);
  if CurError<>nil then begin
    EditorOpts.GetSpecialLineColors(SynLFMSyn1,ahaErrorLine,Special,FG,BG);
  end;
end;

procedure TCheckLFMDialog.CheckLFMDialogCREATE(Sender: TObject);
begin
  Caption:=lisFixLFMFile;
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,600,400);
  SetupComponents;
end;

procedure TCheckLFMDialog.SetLFMSource(const AValue: TCodeBuffer);
begin
  if FLFMSource=AValue then exit;
  FLFMSource:=AValue;
end;

procedure TCheckLFMDialog.SetLFMTree(const AValue: TLFMTree);
begin
  if FLFMTree=AValue then exit;
  FLFMTree:=AValue;
  RemoveAllButton.Enabled:=AutomaticFixIsPossible;
end;

procedure TCheckLFMDialog.SetupComponents;
begin
  NoteLabel.Caption:=lisTheLFMLazarusFormFileContainsInvalidPropertiesThis;
  CancelButton.Caption:=dlgCancel;
  ErrorsGroupBox.Caption:=lisErrors;
  LFMGroupBox.Caption:=lisLFMFile;
  RemoveAllButton.Caption:=lisRemoveAllInvalidProperties;
  
  EditorOpts.GetHighlighterSettings(SynLFMSyn1);
  EditorOpts.GetSynEditSettings(LFMSynEdit);
end;

function TCheckLFMDialog.FindListBoxError: TLFMError;
var
  i: Integer;
begin
  Result:=nil;
  i:=ErrorsListBox.ItemIndex;
  if (i<0) or (i>=ErrorsListBox.Items.Count) then exit;
  Result:=LFMTree.FirstError;
  while Result<>nil do begin
    if i=0 then exit;
    Result:=Result.NextError;
    dec(i);
  end;
end;

procedure TCheckLFMDialog.JumpToError(LFMError: TLFMError);
begin
  if LFMError=nil then exit;
  LFMSynEdit.CaretXY:=LFMError.Caret;
end;

procedure TCheckLFMDialog.FindNiceNodeBounds(LFMNode: TLFMTreeNode;
  var StartPos, EndPos: integer);
var
  Src: String;
begin
  Src:=LFMSource.Source;
  StartPos:=FindLineEndOrCodeInFrontOfPosition(Src,LFMNode.StartPos,1,false,true);
  EndPos:=FindLineEndOrCodeInFrontOfPosition(Src,LFMNode.EndPos,1,false,true);
  EndPos:=FindLineEndOrCodeAfterPosition(Src,EndPos,length(Src),false);
end;

procedure TCheckLFMDialog.AddReplacement(LFMChangeList: TList;
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
        debugln('TCheckLFMDialog.AddReplacement Combine Deletion: Old=',dbgs(Entry.StartPos),'-',dbgs(Entry.EndPos),' New=',dbgs(StartPos),'-',dbgs(EndPos));
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
        debugln('TCheckLFMDialog.AddReplacement Intersecting Deletion: Old=',dbgs(Entry.StartPos),'-',dbgs(Entry.EndPos),' New=',dbgs(StartPos),'-',dbgs(EndPos));
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

function TCheckLFMDialog.ApplyReplacements(LFMChangeList: TList): boolean;
var
  i: Integer;
  Entry: TLFMChangeEntry;
begin
  Result:=false;
  //writeln(LFMSource.Source);
  for i:=LFMChangeList.Count-1 downto 0 do begin
    Entry:=TLFMChangeEntry(LFMChangeList[i]);
    DebugLn('TCheckLFMDialog.ApplyReplacements A ',IntToStr(i),' ',
      IntToStr(Entry.StartPos),',',IntToStr(Entry.EndPos),
      ' "',copy(LFMSource.Source,Entry.StartPos,Entry.EndPos-Entry.StartPos),'" -> "',Entry.NewText,'"');
    LFMSource.Replace(Entry.StartPos,Entry.EndPos-Entry.StartPos,Entry.NewText);
  end;
  //writeln(LFMSource.Source);
  Result:=true;
end;

procedure TCheckLFMDialog.LoadLFM;
begin
  LFMSynEdit.Lines.Text:=LFMSource.Source;
  FillErrorsListBox;
end;

procedure TCheckLFMDialog.FillErrorsListBox;
var
  CurError: TLFMError;
  Filename: String;
  Msg: String;
begin
  ErrorsListBox.Items.BeginUpdate;
  ErrorsListBox.Items.Clear;
  if LFMTree<>nil then begin
    Filename:=ExtractFileName(LFMSource.Filename);
    CurError:=LFMTree.FirstError;
    while CurError<>nil do begin
      Msg:=Filename
           +'('+IntToStr(CurError.Caret.Y)+','+IntToStr(CurError.Caret.X)+')'
           +' Error: '
           +CurError.ErrorMessage;
      ErrorsListBox.Items.Add(Msg);
      CurError:=CurError.NextError;
    end;
  end;
  ErrorsListBox.Items.EndUpdate;
end;

function TCheckLFMDialog.AutomaticFixIsPossible: boolean;
var
  CurError: TLFMError;
begin
  Result:=true;
  CurError:=LFMTree.FirstError;
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

initialization
  {$I checklfmdlg.lrs}

end.

