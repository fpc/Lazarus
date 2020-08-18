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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    A dialog showing the empty methods of the current class
    (at cursor in source editor).
    With the ability to remove them automatically.
}
unit EmptyMethodsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo,
  // LCL
  LCLProc, Forms, Controls, Dialogs, StdCtrls, ButtonPanel,
  // SynEdit
  SynEdit, SynHighlighterPas,
  // CodeTools
  CodeToolsStructs, CodeCache, CodeToolManager, PascalParserTool, CodeTree,
  // IdeIntf
  SrcEditorIntf, LazIDEIntf, PropEdits, IDEDialogs,
  // IDE
  CustomFormEditor, JitForms, Project, LazarusIDEStrConsts, EditorOptions;

type

  { TEmptyMethodsDialog }

  TEmptyMethodsDialog = class(TForm)
    AllButton: TButton;
    PublishedButton: TButton;
    ButtonPanel1: TButtonPanel;
    PrivateCheckBox: TCheckBox;
    ProtectedCheckBox: TCheckBox;
    PublicCheckBox: TCheckBox;
    PublishedCheckBox: TCheckBox;
    SectionsGroupBox: TGroupBox;
    MethodsGroupBox: TGroupBox;
    MethodsSynEdit: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    procedure AllButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PrivateCheckBoxChange(Sender: TObject);
    procedure PublishedButtonClick(Sender: TObject);
  private
    FCaret: TPoint;
    FCode: TCodeBuffer;
    function GetSections: TPascalClassSections;
    procedure SetCaret(const AValue: TPoint);
    procedure SetCode(const AValue: TCodeBuffer);
    procedure SetSections(const AValue: TPascalClassSections);
    procedure UpdateList;
  public
    property Sections: TPascalClassSections read GetSections write SetSections;
    property Code: TCodeBuffer read FCode write SetCode;
    property Caret: TPoint read FCaret write SetCaret;
  end;

function ShowEmptyMethodsDialog: TModalResult;

function RemoveEmptyMethodsInUnit(Code: TCodeBuffer; AClassName: string;
  X, Y: integer; Sections: TPascalClassSections): TModalResult;


implementation

{$R *.lfm}

function ShowEmptyMethodsDialog: TModalResult;
var
  EmptyMethodsDialog: TEmptyMethodsDialog;
  ErrMsg: String;
  SrcEdit: TSourceEditorInterface;
  Code: TCodeBuffer;
  Caret: TPoint;
  ListOfPCodeXYPosition: TFPList;
  AllEmpty: boolean;
begin
  Result:=mrCancel;
  ListOfPCodeXYPosition:=TFPList.Create;
  try
    // init codetools
    ErrMsg:=lisSAMIDEIsBusy;
    if not LazarusIDE.BeginCodeTools then exit;

    // get cursor position
    ErrMsg:=lisSAMCursorIsNotInAClassDeclaration;
    SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
    if SrcEdit=nil then exit;
    Code:=TCodeBuffer(SrcEdit.CodeToolsBuffer);
    if Code=nil then exit;
    Caret:=SrcEdit.CursorTextXY;
    ErrMsg:='';

    // check cursor is in a class
    if not CodeToolBoss.FindEmptyMethods(Code,'',Caret.X,Caret.Y,
      AllPascalClassSections,ListOfPCodeXYPosition,AllEmpty)
    then begin
      DebugLn(['ShowEmptyMethodsDialog CodeToolBoss.FindEmptyMethods failed']);
      if CodeToolBoss.ErrorMessage<>'' then begin
        ErrMsg:='';
        LazarusIDE.DoJumpToCodeToolBossError;
      end else begin
        IDEMessageDialog(lisEMDNoClass,
          Format(lisEMDNoClassAt, [Code.Filename, IntToStr(Caret.Y), IntToStr(
            Caret.X)]),
          mtError,[mbCancel]);
      end;
      exit;
    end;
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);

    EmptyMethodsDialog:=TEmptyMethodsDialog.Create(nil);
    try
      EmptyMethodsDialog.Code:=Code;
      EmptyMethodsDialog.Caret:=Caret;
      EmptyMethodsDialog.UpdateList;
      Result:=EmptyMethodsDialog.ShowModal;
    finally
      EmptyMethodsDialog.Free;
    end;
  finally
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
    if ErrMsg<>'' then begin
      IDEMessageDialog(lisCCOErrorCaption,
        Format(lisEMDUnableToShowEmptyMethodsOfTheCurrentClassBecause,
               [LineEnding, ErrMsg]), mtError, [mbCancel]);
    end;
  end;
end;

function GetInheritedMethod(APersistent: TPersistent; PropInfo: PPropInfo): TMethod;
var
  AncestorRoot, AncestorComponent: TComponent;
  AncestorMethod: TMethod;
  Comp: TComponent;
begin
  FillByte(Result{%H-}, SizeOf(Result), 0);
  if APersistent is TComponent then
  begin
    Comp := TComponent(APersistent);
    if csAncestor in Comp.ComponentState then
    begin
      // search for ancestor component
      if Assigned(Comp.Owner) then
      begin
        AncestorRoot := BaseFormEditor1.GetAncestorLookupRoot(Comp);
        if Assigned(AncestorRoot) then
          AncestorComponent := AncestorRoot.FindComponent(Comp.Name)
        else
          AncestorComponent := nil;
      end
      else
        AncestorComponent := BaseFormEditor1.GetAncestorInstance(Comp);

      if Assigned(AncestorComponent) then
      begin
        AncestorMethod := GetMethodProp(AncestorComponent, PropInfo);
        if IsJITMethod(AncestorMethod) then
          Result := AncestorMethod
      end;
    end;
  end;
end;

function RemoveEmptyMethodsInUnit(Code: TCodeBuffer; AClassName: string;
  X, Y: integer; Sections: TPascalClassSections): TModalResult;
var
  RemovedProcHeads: TStrings;
  PropChanged: boolean;

  function ExtractClassName: string;
  var
    ProcName: string;
    p: LongInt;
    i: Integer;
  begin
    Result:='';
    for i:=RemovedProcHeads.Count-1 downto 0 do
    begin
      ProcName:=RemovedProcHeads[i];
      p:=System.Pos('.',ProcName);
      if p<1 then
        RemovedProcHeads.Delete(i)
      else begin
        Result:=copy(ProcName,1,p-1);
        RemovedProcHeads[i]:=copy(ProcName,p+1,length(ProcName));
      end;
    end;
  end;

  procedure CheckEvents(APersistent: TPersistent);
  // Read properties and remove event handlers which were removed from source by Codetools.
  var
    TypeInfo: PTypeInfo;
    PropInfo: PPropInfo;
    PropList: PPropList;
    PropCount, ic, i: integer;
    AMethod: TMethod;
    AMethodName: String;
    Coll: TCollection;
  begin
    TypeInfo:=PTypeInfo(APersistent.ClassInfo);
    PropCount:=GetPropList(TypeInfo,PropList); // List of properties and their count
    try
      for ic:=0 to PropCount-1 do              // iterate properties
      begin
        PropInfo:=PropList^[ic];
        if PropInfo^.PropType^.Kind=tkMethod then
        begin
          AMethod:=GetMethodProp(APersistent,PropInfo);   // event
          AMethodName:=GlobalDesignHook.GetMethodName(AMethod,nil);
          if AMethodName<>'' then
          begin
            i:=RemovedProcHeads.Count-1;
            while (i>=0) and (CompareText(RemovedProcHeads[i],AMethodName)<>0) do
              dec(i);
            if i>=0 then
            begin
              //DebugLn([' CheckEvents Clearing Property=',PropInfo^.Name,' AMethodName=',AMethodName]);
              AMethod := GetInheritedMethod(APersistent, PropInfo);
              SetMethodProp(APersistent, PropInfo, AMethod);
              PropChanged:=true;
            end;
          end;
        end
        else if PropInfo^.PropType^.Kind=tkClass then
        begin
          Coll := TCollection(GetObjectProp(APersistent, PropInfo, TCollection));
          if Assigned(Coll) then
            for i := 0 to Coll.Count - 1 do // CollectionItem can have events.
              CheckEvents(Coll.Items[i]); // Recurse also because collections can be nested.
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;

var
  AllEmpty: boolean;
  AnUnitInfo: TUnitInfo;
  i: Integer;
  LookupRoot: TComponent;
  CurClassName: String;
begin
  Result:=mrCancel;
  RemovedProcHeads:=nil;
  try
    if not CodeToolBoss.RemoveEmptyMethods(Code,AClassName,X,Y,Sections,AllEmpty,
      [phpAddClassName,phpDoNotAddSemicolon,phpWithoutParamList,
       phpWithoutBrackets,phpWithoutClassKeyword,phpWithoutSemicolon],
      RemovedProcHeads)
    then begin
      DebugLn(['RemoveEmptyMethods failed']);
      exit;
    end;
    if (RemovedProcHeads<>nil) and (RemovedProcHeads.Count>0) then begin
      // RemovedProcHeads contains a list of classname.procname, remove classname from the list
      CurClassName:=ExtractClassName;
      if (CurClassName<>'') and (Project1<>nil) then
      begin
        AnUnitInfo:=Project1.UnitInfoWithFilename(Code.Filename);
        if AnUnitInfo<>nil then
        begin
          // fix events of designer components
          LookupRoot:=AnUnitInfo.Component;
          if (LookupRoot<>nil) and (CompareText(LookupRoot.ClassName,CurClassName)=0) then
          begin
            PropChanged:=false;
            CheckEvents(LookupRoot);
            for i:=0 to LookupRoot.ComponentCount-1 do
              CheckEvents(LookupRoot.Components[i]);
            // update objectinspector
            if PropChanged and (GlobalDesignHook.LookupRoot=LookupRoot) then
              GlobalDesignHook.RefreshPropertyValues;
          end;
        end;
      end;
    end;
  finally
    RemovedProcHeads.Free;
  end;
  Result:=mrOk;
end;

{ TEmptyMethodsDialog }

procedure TEmptyMethodsDialog.FormCreate(Sender: TObject);
begin
  Caption:=lisEMDEmptyMethods;
  SectionsGroupBox.Caption:=lisEMDSearchInTheseClassSections;
  PrivateCheckBox.Caption:=lisPrivate;
  ProtectedCheckBox.Caption:=lisProtected;
  PublicCheckBox.Caption:=lisEMDPublic;
  PublishedCheckBox.Caption:=lisEMDPublished;
  AllButton.Caption:=lisEMDAll;
  PublishedButton.Caption:=lisEMDOnlyPublished;
  MethodsGroupBox.Caption:=lisEMDFoundEmptyMethods;
  Sections:=AllPascalClassSections;
  
  ButtonPanel1.OKButton.Caption:=lisEMDRemoveMethods;
  ButtonPanel1.CancelButton.Caption:=lisCancel;

  EditorOpts.GetSynEditSettings(MethodsSynEdit);
end;

procedure TEmptyMethodsDialog.OKButtonClick(Sender: TObject);
begin
  if LazarusIDE.BeginCodeTools
  and (RemoveEmptyMethodsInUnit(Code,'',Caret.X,Caret.Y,Sections)=mrOk) then
    ModalResult:=mrOk;
end;

procedure TEmptyMethodsDialog.PrivateCheckBoxChange(Sender: TObject);
begin
  UpdateList;
end;

procedure TEmptyMethodsDialog.PublishedButtonClick(Sender: TObject);
begin
  Sections:=[pcsPublished];
end;

procedure TEmptyMethodsDialog.SetSections(const AValue: TPascalClassSections);
begin
  PrivateCheckBox.Checked:=pcsPrivate in AValue;
  ProtectedCheckBox.Checked:=pcsProtected in AValue;
  PublicCheckBox.Checked:=pcsPublic in AValue;
  PublishedCheckBox.Checked:=pcsPublished in AValue;
end;

procedure TEmptyMethodsDialog.SetCaret(const AValue: TPoint);
begin
  FCaret:=AValue;
end;

function TEmptyMethodsDialog.GetSections: TPascalClassSections;
begin
  Result:=[];
  if PrivateCheckBox.Checked then Include(Result,pcsPrivate);
  if ProtectedCheckBox.Checked then Include(Result,pcsProtected);
  if PublicCheckBox.Checked then Include(Result,pcsPublic);
  if PublishedCheckBox.Checked then Include(Result,pcsPublished);
end;

procedure TEmptyMethodsDialog.SetCode(const AValue: TCodeBuffer);
begin
  if FCode=AValue then exit;
  FCode:=AValue;
end;

procedure TEmptyMethodsDialog.UpdateList;
var
  CurSections: TPascalClassSections;
  ListOfPCodeXYPosition: TFPList;
  i: Integer;
  CodePos: TCodeXYPosition;
  Tool: TCodeTool;
  CleanPos: integer;
  Node: TCodeTreeNode;
  NodeText: String;
  AllEmpty: boolean;
  NewTxt: String;
begin
  if (Code=nil) or (Caret.X<1) or (Caret.Y<1) then begin
    MethodsSynEdit.Text:='';
    exit;
  end;

  CurSections:=Sections;
  ListOfPCodeXYPosition:=TFPList.Create;
  try
    if (not CodeToolBoss.FindEmptyMethods(Code,'',Caret.X,Caret.Y,
      CurSections,ListOfPCodeXYPosition,AllEmpty))
    or (not CodeToolBoss.Explore(Code,Tool,false))
    then begin
      MethodsSynEdit.Text:='CodeToolBoss.FindEmptyMethods failed'#10
        +CodeToolBoss.ErrorMessage;
      exit;
    end;

    NewTxt:='';
    for i:=0 to ListOfPCodeXYPosition.Count-1 do begin
      CodePos:=PCodeXYPosition(ListOfPCodeXYPosition[i])^;
      //DebugLn(['TEmptyMethodsDialog.UpdateList ',i,' ',DbgsCXY(CodePos)]);
      if Tool.CaretToCleanPos(CodePos,CleanPos)<>0 then begin
        DebugLn(['TEmptyMethodsDialog.UpdateList Tool.CaretToCleanPos failed']);
        continue;
      end;
      Node:=Tool.FindDeepestNodeAtPos(CleanPos,false);
      if Node=nil then begin
        DebugLn(['TEmptyMethodsDialog.UpdateList Tool.FindDeepestNodeAtPos failed']);
        continue;
      end;
      NodeText:=Tool.ExtractProcHead(Node,[phpWithStart,phpWithParameterNames,
        phpWithVarModifiers,phpWithDefaultValues,phpWithResultType,
        phpWithCallingSpecs,phpWithProcModifiers]);
      NewTxt:=NewTxt+NodeText+#10;
    end;
    MethodsSynEdit.Text:=NewTxt;
  finally
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
  end;
end;

procedure TEmptyMethodsDialog.AllButtonClick(Sender: TObject);
begin
  Sections:=AllPascalClassSections;
end;

end.

