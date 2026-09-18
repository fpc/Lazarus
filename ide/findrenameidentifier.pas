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
    Options dialog and methods for finding and renaming identifier references.
}
unit FindRenameIdentifier;

{$mode objfpc}{$H+}

interface

uses
  // RTL + FCL
  Classes, SysUtils, AVL_Tree,
  // LCL
  Forms, Controls, Dialogs, StdCtrls, ExtCtrls, ComCtrls, ButtonPanel, LclIntf, Graphics,
  // CodeTools
  CTUnitGraph, CodeTree, CodeAtom, LinkScanner, CustomCodeTool, CodeCache, BasicCodeTools,
  IdentCompletionTool, CodeToolManager,
  // LazUtils
  LazFileUtils, LazStringUtils, LazLoggerBase,
  // BuildIntf
  PackageIntf,
  // IdeIntf
  IdeIntfStrConsts, LazIDEIntf, IDEWindowIntf, SrcEditorIntf, IDEDialogs,
  // IdeConfig
  DialogProcs, MiscOptions,
  // IdeProject
  Project,
  // IDE
  LazarusIDEStrConsts, CodeToolsOptions;

type
  TFRIdentifierKind = (
    friDeclaration,
    friSourceName
    );
  TFRIdentifierKinds = set of TFRIdentifierKind;

  { TFindRenameIdentifierDialog }

  TFindRenameIdentifierDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    ScopeOverridesCheckBox: TCheckBox;
    ShowResultCheckBox: TCheckBox;
    CurrentGroupBox: TGroupBox;
    CurrentListBox: TListBox;
    ExtraFilesEdit: TEdit;
    ExtraFilesGroupBox: TGroupBox;
    NewEdit: TEdit;
    NewGroupBox: TGroupBox;
    RenameCheckBox: TCheckBox;
    ScopeCommentsCheckBox: TCheckBox;
    ScopeGroupBox: TGroupBox;
    ScopeRadioGroup: TRadioGroup;
    ScopeIncludeLFMs: TCheckBox;
    procedure FindOrRenameButtonClick(Sender: TObject);
    procedure FindRenameIdentifierDialogClose(Sender: TObject;
      var {%H-}CloseAction: TCloseAction);
    procedure FindRenameIdentifierDialogCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure RenameCheckBoxChange(Sender: TObject);
    procedure ValidateNewName(Sender: TObject);
  private
    FAllowRename: boolean;
    FIdentifierFilename: string;
    FIdentifierKind: TFRIdentifierKind;
    FIdentifierPosition: TPoint;
    FOldIdentifier: string;
    FNewIdentifier: string;
    FConflictUnitNames: TStringList; // already defined identifiers in scope
    FIsPrivate: boolean;
    FNode: TCodeTreeNode;
    FNodesDeletedChangeStep: integer;
    FTool: TCodeTool; // of FNode
    FFiles: TStringList;
    procedure SetAllowRename(AValue: boolean);
    procedure SetIsPrivate(AValue: boolean);
    procedure SetFiles(AFiles: TStringList);
    procedure UpdateRename;
    procedure GatherFiles;
    function NewIdentifierIsConflicted(var ErrMsg: string): boolean;
    function IsNodeInvalid(const Msg: string): boolean;
  public
    destructor Destroy; override;
    procedure LoadFromConfig;
    procedure SaveToConfig;
    procedure LoadFromOptions(Options: TFindRenameIdentifierOptions);
    procedure SaveToOptions(Options: TFindRenameIdentifierOptions);
    procedure SetIdentifier(const NewIdentifierFilename: string;
                            var NewIdentifierPosition: TPoint;
                            anIdentifierKind: TFRIdentifierKind);
    property IdentifierFilename: string read FIdentifierFilename;
    property IdentifierPosition: TPoint read FIdentifierPosition;
    property AllowRename: boolean read FAllowRename write SetAllowRename;
    property IsPrivate: boolean read FIsPrivate write SetIsPrivate;
    property IdentifierKind: TFRIdentifierKind read FIdentifierKind;
  end;

function ShowFindRenameIdentifierDialog(const Filename: string;
  var Position: TPoint;
  AllowRename: boolean; // allow user to disable/enable rename
  SetRenameActive: boolean; // check rename
  IdentifierKind: TFRIdentifierKind): TModalResult;


implementation

{$R *.lfm}

function ShowFindRenameIdentifierDialog(const Filename: string;
  var Position: TPoint; AllowRename: boolean; SetRenameActive: boolean;
  IdentifierKind: TFRIdentifierKind): TModalResult;
var
  FindRenameIdentifierDialog: TFindRenameIdentifierDialog;
begin
  FindRenameIdentifierDialog:=TFindRenameIdentifierDialog.Create(nil);
  try
    // LoadFromConfig executed at FindRenameIdentifierDialogCreate in OnCreate
    FindRenameIdentifierDialog.SetIdentifier(Filename,Position,IdentifierKind);
    FindRenameIdentifierDialog.AllowRename:=AllowRename;
    FindRenameIdentifierDialog.RenameCheckBox.Checked:=SetRenameActive and AllowRename;
    Result:=FindRenameIdentifierDialog.ShowModal;
  finally
    FindRenameIdentifierDialog.Free;
    FindRenameIdentifierDialog:=nil;
  end;
end;

{ TFindRenameIdentifierDialog }

procedure TFindRenameIdentifierDialog.FindRenameIdentifierDialogCreate(Sender: TObject);
begin
  IDEDialogLayoutList.ApplyLayout(Self,450,480);

  Caption:=lisFRIFindOrRenameIdentifier;
  CurrentGroupBox.Caption:=lisCodeToolsOptsIdentifier;
  ExtraFilesGroupBox.Caption:=lisFRIAdditionalFilesToSearchEGPathPasPath2Pp;
  ButtonPanel1.OKButton.Caption:=lisFRIFindReferences;
  ButtonPanel1.OKButton.ModalResult:=mrNone;
  ButtonPanel1.CancelButton.Caption:=lisCancel;
  NewGroupBox.Caption:=lisFRIRenaming;
  RenameCheckBox.Caption:=lisRename;
  ShowResultCheckBox.Caption:=lisRenameShowResult;
  ScopeCommentsCheckBox.Caption:=lisFRISearchInCommentsToo;
  ScopeOverridesCheckBox.Caption:=lisFindOverridesToo;
  ScopeIncludeLFMs.Caption:=lisIncludeLFMs;
  ScopeGroupBox.Caption:=lisFRISearch;
  ScopeRadioGroup.Caption:=dlgSearchScope;
  ScopeRadioGroup.Items[0]:=lisFRIinCurrentUnit;
  ScopeRadioGroup.Items[1]:=lisFRIinMainProject;
  ScopeRadioGroup.Items[2]:=lisFRIinProjectPackageOwningCurrentUnit;
  ScopeRadioGroup.Items[3]:=lisFRIinAllOpenPackagesAndProjects;
  FFiles:=nil;
  LoadFromConfig;
end;

procedure TFindRenameIdentifierDialog.FormShow(Sender: TObject);
begin
  if NewEdit.CanFocus then
  begin
    NewEdit.SelectAll;
    NewEdit.SetFocus;
  end;
end;

procedure TFindRenameIdentifierDialog.HelpButtonClick(Sender: TObject);
begin
  OpenUrl('https://wiki.freepascal.org/IDE_Window:_Find_or_Rename_identifier');
end;

procedure TFindRenameIdentifierDialog.RenameCheckBoxChange(Sender: TObject);
begin
  if RenameCheckBox.Checked then
    ValidateNewName(Sender)
  else begin
    FNewIdentifier:=FOldIdentifier;
    NewEdit.Text:=FNewIdentifier;
    UpdateRename;
  end;
end;

procedure TFindRenameIdentifierDialog.ValidateNewName(Sender: TObject);
var
  ok: boolean;
  Err, dotPart:string;
  i: integer;
begin
  if FOldIdentifier='' then exit;
  if IsNodeInvalid('TFindRenameIdentifierDialog.ValidateNewName') then exit;
  Err:='';
  FNewIdentifier:=NewEdit.Text;
  ok:=IsValidDottedIdent(FNewIdentifier);
  if not ok then begin
    if FNewIdentifier='' then
      Err:=lisIdentifierCannotBeEmpty
    else
      Err:= format(lisIdentifierIsInvalid,[FNewIdentifier]);
  end else if (IdentifierKind<>friSourceName) and (Pos('.',FNewIdentifier)>0) then
  begin
    ok:=false;
    Err:=Format(lisIdentifierCannotBeDotted,[FNewIdentifier]);
  end;

  if ok
      and (FNode.Desc=ctnBeginBlock) // 'Result', 'Self'
      and (CompareIdentifiers(PChar(FNewIdentifier),
                              PChar(FOldIdentifier))<>0) // only case change allowed
  then begin
    // Result inside function or Self inside a procedure/function in class or record
    ok:=false;
    Err:=Format(lisIdentifierIsReservedWord,[FOldIdentifier]);
  end;

  if ok and (FTool<>nil) then begin
    i:=1;
    while ok and (i<=length(FNewIdentifier)) do begin
      dotPart:='';
      while (i<=length(FNewIdentifier)) do begin
        dotPart:=dotPart+FNewIdentifier[i];
        inc(i);
        if i>length(FNewIdentifier)then
          break;
        if FNewIdentifier[i]='.' then begin
          inc(i);
          break;
        end;
      end;
      ok:=not FTool.StringIsKeyWord(dotPart);
    end;
    if not ok then begin
      Err:=Format(lisIdentifierIsReservedWord,[dotPart]);
      ok:=true;
    end;
  end;

  if (Err='')
      and (CompareDottedIdentifiers(PChar(FNewIdentifier),PChar(FOldIdentifier))<>0)
  then
    NewIdentifierIsConflicted(Err);

  ButtonPanel1.OKButton.Enabled:=ok;
  if Err='' then begin
    NewGroupBox.Caption:=lisFRIRenaming;
    NewGroupBox.Font.Style:=NewGroupBox.Font.Style-[fsBold];
  end
  else begin
    Err:=StringReplace(Err,'&','&&',[rfReplaceAll]);
    NewGroupBox.Caption:=lisFRIRenaming+' - '+ Err;
    NewGroupBox.Font.Style:=NewGroupBox.Font.Style+[fsBold];
  end;

  UpdateRename;
end;

procedure TFindRenameIdentifierDialog.UpdateRename;
begin
  RenameCheckBox.Enabled:=AllowRename;
  if not RenameCheckBox.Checked then begin
    ButtonPanel1.OKButton.Enabled:=true;
    NewGroupBox.Caption:=lisFRIRenaming;
    NewGroupBox.Font.Style:=NewGroupBox.Font.Style-[fsBold];
  end;
  NewEdit.Enabled:=RenameCheckBox.Checked and RenameCheckBox.Enabled;
  ShowResultCheckBox.Enabled:=RenameCheckBox.Checked and RenameCheckBox.Enabled;
  if NewEdit.Enabled then
    ButtonPanel1.OKButton.Caption:=lisFRIRenameAllReferences
  else
    ButtonPanel1.OKButton.Caption:=lisFRIFindReferences;
  if RenameCheckBox.Checked and (FConflictUnitNames=nil) then
    GatherFiles;
end;

procedure TFindRenameIdentifierDialog.SetAllowRename(AValue: boolean);
begin
  if FAllowRename=AValue then exit;
  FAllowRename:=AValue;
  UpdateRename;
end;

procedure TFindRenameIdentifierDialog.SetIsPrivate(AValue: boolean);
begin
  if FIsPrivate=AValue then exit;
  FIsPrivate:=AValue;
  ExtraFilesGroupBox.Enabled:=not IsPrivate;
  ScopeRadioGroup.Enabled:=not IsPrivate;
  ScopeRadioGroup.ItemIndex:=0;
end;

procedure TFindRenameIdentifierDialog.SetFiles(AFiles: TStringList);
begin
  if FFiles<>nil then exit; //already set
  if AFiles = nil then exit;
  if AFiles.Count = 0 then exit;
  if FFiles=nil then FFiles:=TStringList.Create;
  FFiles.Assign(AFiles);
end;

procedure TFindRenameIdentifierDialog.FindOrRenameButtonClick(Sender: TObject);
var
  ACodeBuffer:TCodeBuffer;
  X,Y: integer;
  ErrInfo: string;

  function GetContextEndCodePos(ANode:TCodeTreeNode; out X,Y:integer):boolean;
  var
    CodeTool: TCodeTool;
    CaretXY: TCodeXYPosition;
    aPos,aTop: integer;
  begin
    X:=0;
    Y:=0;
    Result:=false;
    CodeToolBoss.Explore(ACodeBuffer,CodeTool,true);
    if CodeTool<>nil then begin
      CodeTool.MoveCursorToCleanPos(ANode.EndPos);
      CodeTool.ReadNextAtom;

      CodeTool.ReadPriorAtom;
      if not (CodeTool.CurPos.Flag in [cafWord, cafEnd]) and (CodeTool.CurPos.StartPos>0) then
        CodeTool.ReadPriorAtom;
      if (CodeTool.CurPos.Flag=cafEnd) and (CodeTool.CurPos.StartPos>0) then
        CodeTool.ReadPriorAtom;

      aPos:=CodeTool.CurPos.StartPos;

      if CodeTool.CleanPosToCaretAndTopLine(aPos, CaretXY, aTop) then begin
        X:=CaretXY.X;
        Y:=CaretXY.Y;
        Result:=true;
      end;
    end;
  end;

  function FindConflict: boolean;
  var
    anItem: TIdentifierListItem;
  begin
    anItem:=CodeToolBoss.IdentifierList.FindIdentifier(PChar(FNewIdentifier));
    Result:=(anItem<>nil) and
      (CompareDottedIdentifiers(PChar(FOldIdentifier), PChar(FNewIdentifier))<>0);
    if Result then begin
      if iliTemplate in anItem.Flags then exit(False); //template name can be used
      if anItem.Node<>nil then begin
        ErrInfo:= Format(lisIdentifierIsAlreadyUsed2,[FNewIdentifier]);
      end else begin
        if anItem.ResultType='' then
          ErrInfo:= Format(lisIdentifierIsDeclaredCompilerProcedure,[FNewIdentifier])
        else
          ErrInfo:= Format(lisIdentifierIsDeclaredCompilerFunction,[FNewIdentifier]);
      end;
    end;
  end;

var
  Res:TModalResult;
  CTB_IdentComplIncludeKeywords: Boolean;
  CTB_CodeCompletionTemplateFileName, AmpIdentifier: string;
  CTB_IdentComplIncludeWords: TIdentComplIncludeWords;
  r: integer;
  tmpNode: TCodeTreeNode;
begin
  if IsNodeInvalid('TFindRenameIdentifierDialog.FindOrRenameButtonClick') then exit;

  if not RenameCheckBox.Checked then begin
    // find references
    ModalResult:=mrOK;
    exit;
  end;

  if CompareDottedIdentifiers(PChar(FNewIdentifier),PChar(FOldIdentifier))=0 then begin
    // change all references to same case
    ModalResult:=mrOk;
    exit;
  end;

  // rename -> check for conflict
  ModalResult:=mrNone;

  if IdentifierHasKeywords(FNewIdentifier,cmFPC,AmpIdentifier) then begin
    Res:=TaskDlg(lisInvalidPascalIdentifierCap,
        Format(lisTheNameContainsAPascalKeyword, [FNewIdentifier]), '',
        tdiWarning,[mbOk,mbCancel],mbOk,
          [lisChooseADifferentName2,
           Format(lisUseInstead, [StringReplace(AmpIdentifier,'&','&&',[rfReplaceAll])]),
           Format(lisUseAnyway, [StringReplace(FNewIdentifier,'&','&&',[rfReplaceAll])])], r);
    if Res<>mrOK then
      exit;
    case r of
    1:
      begin
        FNewIdentifier:=AmpIdentifier;
        NewEdit.Text:=FNewIdentifier;
      end;
    2: ;
    else exit;
    end;
  end;

  CTB_IdentComplIncludeKeywords:=CodeToolBoss.IdentComplIncludeKeywords;
  CodeToolBoss.IdentComplIncludeKeywords:=false;

  CTB_CodeCompletionTemplateFileName:=
    CodeToolsOptions.CodeToolsOpts.CodeCompletionTemplateFileName;
  CodeToolsOptions.CodeToolsOpts.CodeCompletionTemplateFileName:='';

  CTB_IdentComplIncludeWords:=CodeToolsOptions.CodeToolsOpts.IdentComplIncludeWords;
  CodeToolsOptions.CodeToolsOpts.IdentComplIncludeWords:=icwIncludeFromAllUnits;

  ErrInfo:='';
  try
    CodeToolBoss.IdentifierList.Clear;

    Res:=LoadCodeBuffer(ACodeBuffer,IdentifierFileName,[lbfCheckIfText],false);
    //try declaration context
    if Res<>mrOK then begin
      ModalResult:=mrCancel;
      exit;
    end;
    tmpNode:=FNode;
    while tmpNode<>nil do begin
      if (tmpNode.Parent<>nil) and (tmpNode.Parent.Desc in AllFindContextDescs)
      then begin
        if GetContextEndCodePos(tmpNode.Parent,X,Y) then begin
          CodeToolBoss.GatherIdentifiers(ACodeBuffer, X, Y);
          FindConflict; // ErrInfo is set inside the function
        end; // gathering identifiers may fail
        break;
      end;
      tmpNode:=tmpNode.Parent;
    end;
  finally
    CodeToolBoss.IdentComplIncludeKeywords:=
      CTB_IdentComplIncludeKeywords;
    CodeToolsOptions.CodeToolsOpts.CodeCompletionTemplateFileName:=
      CTB_CodeCompletionTemplateFileName;
    CodeToolsOptions.CodeToolsOpts.IdentComplIncludeWords:=
      CTB_IdentComplIncludeWords;
  end;
  if ErrInfo<>'' then begin
    if IDEMessageDialog(dlgMsgWinColorUrgentWarning, ErrInfo, mtWarning,
      [mbCancel, mbIgnore])<>mrIgnore then
    exit;
  end;
  ModalResult:=mrOk;
end;

procedure TFindRenameIdentifierDialog.FindRenameIdentifierDialogClose(
  Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveToConfig;
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TFindRenameIdentifierDialog.LoadFromConfig;
begin
  LoadFromOptions(MiscellaneousOptions.FindRenameIdentifierOptions);
end;

procedure TFindRenameIdentifierDialog.SaveToConfig;
begin
  SaveToOptions(MiscellaneousOptions.FindRenameIdentifierOptions);
end;

procedure TFindRenameIdentifierDialog.LoadFromOptions(
  Options: TFindRenameIdentifierOptions);
begin
  RenameCheckBox.Checked:=Options.Rename;
  ExtraFilesEdit.Text:=StringListToText(Options.ExtraFiles,';',true);
  NewEdit.Text:=Options.RenameTo;
  ShowResultCheckBox.Checked:=Options.RenameShowResult;
  ScopeCommentsCheckBox.Checked:=Options.SearchInComments;
  ScopeOverridesCheckBox.Checked:=Options.Overrides;

  ScopeIncludeLFMs.Checked:=Options.IncludeLFMs;

  case Options.Scope of
  frCurrentUnit: ScopeRadioGroup.ItemIndex:=0;
  frProject: ScopeRadioGroup.ItemIndex:=1;
  frOwnerProjectPackage: ScopeRadioGroup.ItemIndex:=2;
  else
    ScopeRadioGroup.ItemIndex:=3;
  end;
  UpdateRename;
end;

procedure TFindRenameIdentifierDialog.SaveToOptions(
  Options: TFindRenameIdentifierOptions);
begin
  Options.Rename:=RenameCheckBox.Checked;
  if ExtraFilesGroupBox.Enabled then
    SplitString(ExtraFilesEdit.Text,';',Options.ExtraFiles,true);
  Options.RenameTo:=NewEdit.Text;
  Options.RenameShowResult := ShowResultCheckBox.Checked;
  Options.SearchInComments:=ScopeCommentsCheckBox.Checked;
  Options.Overrides:=ScopeOverridesCheckBox.Checked;
  Options.IncludeLFMs:=ScopeIncludeLFMs.Checked;
  if IsPrivate then begin
    // when the identifier is private, the only scope allowed is 'current unit'
    // -> keep Options.Scope, so that next time renaming a non private identifier the scope is back
  end else begin
    if ScopeRadioGroup.Enabled then
      case ScopeRadioGroup.ItemIndex of
      0: Options.Scope:=frCurrentUnit;
      1: Options.Scope:=frProject;
      2: Options.Scope:=frOwnerProjectPackage;
      else Options.Scope:=frAllOpenProjectsAndPackages;
      end
    else
      Options.Scope:=frCurrentUnit;
  end;
end;

procedure TFindRenameIdentifierDialog.SetIdentifier(const NewIdentifierFilename: string;
  var NewIdentifierPosition: TPoint; anIdentifierKind: TFRIdentifierKind);
var
  s: String;
  ACodeBuffer, CurCode: TCodeBuffer;
  ListOfCodeBuffer: TFPList;
  i: Integer;
  CodeXY: TCodeXYPosition;
  CleanPos: integer;
  Node: TCodeTreeNode;
begin
  FIdentifierKind:=anIdentifierKind;
  FIdentifierFilename:=NewIdentifierFilename;
  FIdentifierPosition:=NewIdentifierPosition;
  FOldIdentifier:='';
  FNode:=nil;
  FTool:=nil;
  Node:=nil;
  //debugln(['TFindRenameIdentifierDialog.SetIdentifier ',FIdentifierFilename,' ',dbgs(FIdentifierPosition)]);
  CurrentListBox.Items.Clear;
  s:=IdentifierFilename
     +'('+IntToStr(IdentifierPosition.Y)+','+IntToStr(IdentifierPosition.X)+')';
  CurrentListBox.Items.Add(s);
  LoadCodeBuffer(ACodeBuffer,IdentifierFileName,[lbfCheckIfText],false);
  IsPrivate:=false;
  ScopeRadioGroup.Items[0]:=lisFRIinCurrentUnit;
  if ACodeBuffer=nil then begin
    CurrentGroupBox.Caption:='?file not found?';
    exit;
  end;

  // Check if this is an include file and list all files up to the unit/program
  CodeToolBoss.GetIncludeCodeChain(ACodeBuffer,true,ListOfCodeBuffer);
  if ListOfCodeBuffer<>nil then begin
    for i:=0 to ListOfCodeBuffer.Count-1 do begin
      CurCode:=TCodeBuffer(ListOfCodeBuffer[i]);
      if CurCode=ACodeBuffer then break;
      CurrentListBox.Items.Insert(0,CurCode.Filename);
    end;
    ListOfCodeBuffer.Free;
  end;

  // check if in implementation or private section
  if CodeToolBoss.Explore(ACodeBuffer,FTool,false) then begin
    CodeXY:=CodeXYPosition(NewIdentifierPosition.X,NewIdentifierPosition.Y,ACodeBuffer);
    if FTool.CaretToCleanPos(CodeXY,CleanPos)=0 then begin
      Node:=FTool.BuildSubTreeAndFindDeepestNodeAtPos(CleanPos,false);
      if (Node=nil)
      or Node.HasParentOfType(ctnImplementation)
      or Node.HasParentOfType(ctnClassPrivate) then
        IsPrivate:=true;
    end;
  end;
  if FTool=nil then exit;
  if FTool.TrueSelf then
    ScopeRadioGroup.Items[0]:=lisFRIinCurrentMethod else
  if FTool.TruePredefinedResult then
    ScopeRadioGroup.Items[0]:=lisFRIinLocalFunction;

  ScopeOverridesCheckBox.Visible:=(Node<>nil) and (Node.Desc=ctnProcedureHead)
      and (FTool.ProcNodeHasSpecifier(Node,psVirtual) or FTool.ProcNodeHasSpecifier(Node,psOverride));

  if CodeToolBoss.GetIdentifierAt(ACodeBuffer,
    NewIdentifierPosition.X,NewIdentifierPosition.Y,FOldIdentifier,FNode) then
  begin
    CurrentGroupBox.Caption:=Format(lisFRIIdentifier,[StringReplace(FOldIdentifier,'&','&&',[rfReplaceAll])]);
    if (FNode<>nil) then begin
      if (FNode.Desc=ctnBeginBlock) then // 'Result', 'Self'
        IsPrivate:=true;
    end;
  end else
    FOldIdentifier:='';
  FNodesDeletedChangeStep:=FTool.NodesDeletedChangeStep;
  NewEdit.Text:=FOldIdentifier;
end;

procedure TFindRenameIdentifierDialog.GatherFiles;
var
  StartSrcEdit: TSourceEditorInterface;
  DeclCode, StartSrcCode: TCodeBuffer;
  DeclX, DeclY, DeclTopLine, i: integer;
  LogCaretXY: TPoint;
  OwnerList: TFPList;
  ExtraFiles: TStrings;
  Files: TStringList;
  CurUnitname: string;
  Graph: TUsesGraph;
  Node: TAVLTreeNode;
  UGUnit: TUGUnit;
  UnitInfo, ProjFileInfo:TUnitInfo;
  Completed: boolean;
  ExternalProjectName, InternalProjectName, ProjMainFilename: string;
begin
  if FConflictUnitNames<>nil then exit;
  if not LazarusIDE.BeginCodeTools then exit;
  if Project1=nil then exit;
  if not AllowRename then exit;

  StartSrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  StartSrcCode:=TCodeBuffer(StartSrcEdit.CodeToolsBuffer);
  //StartTopLine:=StartSrcEdit.TopLine;

  // find the main declaration
  LogCaretXY:=StartSrcEdit.CursorTextXY;
  if not CodeToolBoss.FindMainDeclaration(StartSrcCode,
    LogCaretXY.X,LogCaretXY.Y,
    DeclCode,DeclX,DeclY,DeclTopLine) then
  begin
    LazarusIDE.DoJumpToCodeToolBossError;
    exit;
  end;

  if IsNodeInvalid('TFindRenameIdentifierDialog.GatherFiles') then exit;

  OwnerList:=nil;
  Files:=nil;
  try
    FConflictUnitNames:=TStringList.Create;
    Files:=TStringList.Create;

    ProjFileInfo:=Project1.MainUnitInfo;
    if ProjFileInfo<>nil then begin
      ProjMainFilename:=ProjFileInfo.Filename;
      InternalProjectName:=ProjFileInfo.Unit_Name;
      ExternalProjectName:=ExtractFileNameOnly(ProjMainFilename);
      if ExternalProjectName<>'' then begin
        // units cannot have filename matching project file name - only warnings/problems,
        // projects source names can be changed to match its file names,
        // other identifiers can be renamed to project file name - if this differs from
        // project source name.
        if (FNode<>nil)
            and (FNode.Desc in [ctnUseUnit,ctnUseUnitNamespace,ctnUseUnitClearName,ctnUnit])
            and (CompareDottedIdentifiers(PChar(ExternalProjectName),
                                          PChar(InternalProjectName))<>0)
        then
          FConflictUnitNames.Add(ExternalProjectName);
      end;
    end;

    OwnerList:=TFPList.Create;
    OwnerList.Add(Project1);

    // get source files of packages and projects
    ExtraFiles:=PackageEditingInterface.GetSourceFilesOfOwners(OwnerList);
    if ExtraFiles<>nil then
    begin
      // parse all used units
      Graph:=CodeToolBoss.CreateUsesGraph;
      try
        for i:=0 to ExtraFiles.Count-1 do
          Graph.AddStartUnit(ExtraFiles[i]);
        Graph.AddTargetUnit(DeclCode.Filename);
        Graph.Parse(true,Completed);
        Node:=Graph.FilesTree.FindLowest;
        while Node<>nil do begin
          UGUnit:=TUGUnit(Node.Data);
          Files.Add(UGUnit.Filename);
          Node:=Node.Successor;
        end;
      finally
        ExtraFiles.Free;
        Graph.Free;
      end;
    end;
    for i:=0 to Files.Count-1 do begin //get project/unit name
      UnitInfo:=Project1.UnitWithFilename(Files[i]);
      if UnitInfo<>nil then
        CurUnitname:=UnitInfo.Unit_Name
      else
        CurUnitname:=ExtractFileNameOnly(Files[i]);
      FConflictUnitNames.Add(CurUnitname); //store for ValidateNewName
    end;
    SetFiles(Files);
  finally
    Files.Free;
    OwnerList.Free;
  end;
end;

function TFindRenameIdentifierDialog.NewIdentifierIsConflicted(var ErrMsg: string): boolean;
// checking if there are existing other identifiers conflited with the new
// will be executed when "Rename all References" button is clicked
var
  i: integer;
  CheckUnitName, CheckInFileName, aFilename: String;
begin
  Result:=false;
  ErrMsg:='';
  if not AllowRename then exit;
  if IsNodeInvalid('TFindRenameIdentifierDialog.NewIdentifierIsConflicted') then exit;

  if (IdentifierKind<>friSourceName) and (Pos('.',FNewIdentifier)>0) then
  begin
    ErrMsg:=Format(lisIdentifierCannotBeDotted,[FNewIdentifier]);
    exit(true);
  end;
  if FNewIdentifier='' then begin
    ErrMsg:=lisIdentifierCannotBeEmpty;
    exit(true);
  end;
  if FConflictUnitNames=nil then exit;
  i:=0;
  while (i<=FConflictUnitNames.Count-1) and
    (CompareDottedIdentifiers(PChar(FNewIdentifier),PChar(FConflictUnitNames[i]))<>0) do
    inc(i);
  Result:= i<=FConflictUnitNames.Count-1;

  if Result then begin
    ErrMsg:=Format(lisIdentifierIsAlreadyUsed,[FNewIdentifier]);
    exit;
  end;

  CheckUnitName:=FNewIdentifier;
  CheckInFileName:='';
  aFilename:=CodeToolBoss.DirectoryCachePool.FindUnitSourceInCompletePath(
    ExtractFilePath(IdentifierFilename),CheckUnitName,CheckInFileName,true);
  if aFilename<>'' then begin
    ErrMsg:='Matches unit "'+ExtractFileName(aFilename)+'"';
    exit(true);
  end;
end;

function TFindRenameIdentifierDialog.IsNodeInvalid(const Msg: string): boolean;
begin
  if FNode=nil then exit(true);
  if FTool.NodesDeletedChangeStep=FNodesDeletedChangeStep then exit(false);
  Result:=true;
  debugln([Msg,' nodes deleted New=',FTool.NodesDeletedChangeStep,' Old=',FNodesDeletedChangeStep]);
  FNode:=nil;
  ModalResult:=mrCancel;
end;

destructor TFindRenameIdentifierDialog.Destroy;
begin
  FreeAndNil(FConflictUnitNames);
  FreeAndNil(FFiles);
  inherited Destroy;
end;

end.

