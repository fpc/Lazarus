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
unit CheckerLFM;

{$mode objfpc}{$H+}

interface

uses
  // FCL
  Classes, SysUtils, TypInfo, Contnrs, System.UITypes,
  // LCL
  LResources, Forms,
  // LazUtils
  LazStringUtils, AvgLvlTree, LazLoggerBase,
  // CodeTools
  CodeCache, CodeToolManager, LFMTrees,
  // SynEdit
  SynEdit,
  // BuildIntf
  PackageIntf, ComponentReg,
  // IdeUtils
  CheckerLfmBase,
  // IDEIntf
  IDEExternToolIntf, IDEMsgIntf, IDEDialogs, PropEdits, PropEditUtils,
  // IDE
  LazarusIDEStrConsts, CustomFormEditor, JITForms;

type

  { TLfmChecker }

  TLFMChecker = class(TCheckerLFMBase)
  private
    fShowMessages: boolean;
    procedure WriteUnitError(Code: TCodeBuffer; X, Y: integer; const ErrorMessage: string);
    procedure WriteCodeToolsError;
    function CheckUnit: boolean;
    function ShowRepairLFMWizard: TModalResult; // Show the interactive user interface.
  protected
    procedure WriteLFMErrors;
    function FixMissingComponentClasses(aMissingTypes: TClassList): TModalResult; override;
  public
    //constructor Create(APascalBuffer, ALFMBuffer: TCodeBuffer);
    //destructor Destroy; override;
    function RemoveAll: TModalResult;
    function Repair: TModalResult;
    function AutomaticFixIsPossible: boolean;
  public
    property ShowMessages: boolean read fShowMessages write fShowMessages;
  end;

// check and repair lfm files
function QuickCheckLFMBuffer({%H-}PascalBuffer, LFMBuffer: TCodeBuffer;
  out LFMType, LFMComponentName, LFMClassName: string;
  out LCLVersion: string;
  out MissingClasses: TStrings;// e.g. MyFrame2:TMyFrame
  out AmbiguousClasses: TFPList
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

uses
  CheckLFMDlg;

function QuickCheckLFMBuffer(PascalBuffer, LFMBuffer: TCodeBuffer; out LFMType,
  LFMComponentName, LFMClassName: string; out LCLVersion: string; out
  MissingClasses: TStrings; out AmbiguousClasses: TFPList): TModalResult;
const
  ClassFound = 'found';
  ClassMissing = 'missing';
var
  LFMTree: TLFMTree;
  Classes: TStringToStringTree;
  
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
  // Add a missing or nested class to MissingClasses.
  // A nested class means a TFrame installed as a component.
  var
    AClassName, AnUnitName, AFullName: String;
    RegComp: TRegisteredComponent;
  begin
    AClassName:=ObjNode.TypeName;
    AnUnitName:=ObjNode.TypeUnitName;
    if AnUnitName<>'' then
      AFullName:=AnUnitName+'/'+AClassName
    else
      AFullName:=AClassName;
    if Classes[AFullName]<>'' then exit;

    // search in registered classes
    RegComp:=IDEComponentPalette.FindRegComponent(AFullName);
    {$IFDEF VerboseIDEAmbiguousClasses}
    debugln(['QuickCheckLFMBuffer.FindMissingClass AFullName="',AFullName,'" RegComp=',RegComp<>nil]);
    {$ENDIF}
    if (RegComp<>nil) and (RegComp.GetUnitName<>'')
    and not RegComp.ComponentClass.InheritsFrom(TCustomFrame) // not Nested TFrame
    then begin
      Classes[AFullName]:=ClassFound;
      if (AnUnitName='') and RegComp.HasAmbiguousClassName then
      begin
        if AmbiguousClasses=nil then
          AmbiguousClasses:=TFPList.Create;
        if AmbiguousClasses.IndexOf(RegComp)<0 then
          AmbiguousClasses.Add(RegComp);
      end;
      exit;
    end;
    // search in designer base classes
    if BaseFormEditor1.FindDesignerBaseClassByName(AFullName,true)<>nil then
    begin
      Classes[AFullName]:=ClassFound;
      exit;
    end;
    // search in global registered classes
    {$IF FPC_FULLVERSION>30300}
    if GetClass(AnUnitName,AClassName)<>nil then
    {$ELSE}
    if GetClass(AClassName)<>nil then
    {$ENDIF}
    begin
      Classes[AFullName]:=ClassFound;
      exit;
    end;
    // class is missing
    DebugLn(['QuickCheckLFMBuffer->FindMissingClass ',ObjNode.Name,':',AFullName,' IsInherited=',ObjNode.IsInherited]);
    if MissingClasses=nil then
      MissingClasses:=TStringList.Create;
    MissingClasses.Add(AFullName);
    Classes[AFullName]:=ClassMissing;
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
    Classes:=TStringToStringTree.Create(false);
    try
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
    finally
      Classes.Free;
    end;
  end;
  
begin
  //DebugLn(['QuickCheckLFMBuffer LFMBuffer=',LFMBuffer.Filename]);
  LCLVersion:='';
  MissingClasses:=nil;
  AmbiguousClasses:=nil;

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
{
constructor TLFMChecker.Create(APascalBuffer, ALFMBuffer: TCodeBuffer);
begin
  inherited Create(APascalBuffer, ALFMBuffer);
end;

destructor TLFMChecker.Destroy;
begin
  inherited Destroy;
end;
}
function TLFMChecker.ShowRepairLFMWizard: TModalResult;
var
  CheckLFMDialog: TCheckLFMDialog;
begin
  Result:=mrCancel;
  CheckLFMDialog:=TCheckLFMDialog.Create(nil, self);
  try
    fLFMSynEdit:=CheckLFMDialog.LFMSynEdit;
    fErrorsListBox:=CheckLFMDialog.ErrorsListBox;
    LoadFormFile;
    Result:=CheckLFMDialog.ShowModal;
  finally
    CheckLFMDialog.Free;
  end;
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

function TLFMChecker.FixMissingComponentClasses(aMissingTypes: TClassList): TModalResult;
begin
  // add units for the missing object types with registered component classes
  Result:=PackageEditingInterface.AddUnitDepsForCompClasses(fPascalBuffer.Filename,
                                                            aMissingTypes);
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

end.
