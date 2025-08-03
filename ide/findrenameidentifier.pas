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
  Classes, SysUtils, AVL_Tree, Contnrs,
  // LCL
  LResources, Forms, Controls, Dialogs, StdCtrls, ExtCtrls, ComCtrls, ButtonPanel,
  LclIntf, Graphics,
  // CodeTools
  KeywordFuncLists, CTUnitGraph, LFMTrees, CodeTree, CodeAtom, LinkScanner,
  CustomCodeTool, CodeCache, FileProcs, BasicCodeTools, IdentCompletionTool, CodeToolManager,
  CodeToolsStructs, FindDeclarationTool, ChangeDeclarationTool,
  // LazUtils
  LazFileUtils, FileUtil, LazFileCache, laz2_DOM, LazStringUtils, AvgLvlTree, LazLoggerBase,
  // IdeIntf
  IdeIntfStrConsts, LazIDEIntf, IDEWindowIntf, SrcEditorIntf, PackageIntf, ProjectIntf,
  IDEDialogs,
  // IdeUtils
  DialogProcs, InputHistory,
  // LazConfig
  TransferMacros, IDEProcs, SearchPathProcs, EnvironmentOpts,
  // IDE
  LazarusIDEStrConsts, MiscOptions, CustomFormEditor, CodeToolsOptions, SearchResultView,
  CodeHelp, SourceFileManager, Project;

type

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
    FIdentifierPosition: TPoint;
    FIsSourceName: boolean;
    FOldIdentifier: string;
    FNewIdentifier: string;
    FConflictUnitNames: TStringList; // already defined identifiers in scope
    FIsPrivate: boolean;
    FNode: TCodeTreeNode;
    FNodesDeletedChangeStep: integer;
    FTool: TCodeTool; // of FNode
    FFiles: TStringList;
    procedure SetAllowRename(const AValue: boolean);
    procedure SetIsPrivate(const AValue: boolean);
    procedure SetFiles(const Files:TStringList);
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
                            var NewIdentifierPosition: TPoint; IsSrcName: boolean);

    property IdentifierFilename: string read FIdentifierFilename;
    property IdentifierPosition: TPoint read FIdentifierPosition;
    property AllowRename: boolean read FAllowRename write SetAllowRename;
    property IsPrivate: boolean read FIsPrivate write SetIsPrivate;
    property IsSourceName: boolean read FIsSourceName write FIsSourceName;
  end;

function ShowFindRenameIdentifierDialog(const Filename: string;
  var Position: TPoint;
  AllowRename: boolean; // allow user to disable/enable rename
  SetRenameActive: boolean; // check rename
  Options: TFindRenameIdentifierOptions;
  IsSourceName: boolean): TModalResult;
function DoFindRenameIdentifier(
  AllowRename: boolean; // allow user to disable/enable rename
  SetRenameActive: boolean; // check rename
  Options: TFindRenameIdentifierOptions): TModalResult;

function GatherIdentifierReferences(Files: TStringList;
  const DeclCodeXY: TCodeXYPosition;
  DeclTool: TCodeTool; // can be nil
  DeclNode: TCodeTreeNode; // can be nil
  SearchInComments: boolean;
  out ListOfSrcNameRefs: TObjectList; const Flags: TFindRefsFlags): boolean;
function ShowIdentifierReferences(
  DeclFilename: string;
  ListOfSrcNameRefs: TObjectList;
  LFMReferences: TCodeXYPositions;
  Identifier: string; RenameTo: string = ''): TModalResult;
procedure AddReferencesToResultView(Identifier: string;
  ListOfSrcNameRefs: TObjectList; LFMReferences: TCodeXYPositions;
  ClearItems: boolean; SearchPageIndex: integer; RenameTo: string = '');

function GatherFPDocReferencesForPascalFiles(PascalFiles: TStringList;
  DeclarationCode: TCodeBuffer; const DeclarationCaretXY: TPoint;
  var ListOfLazFPDocNode: TFPList): TModalResult;
function GatherReferencesInFPDocFile(
  const OldPackageName, OldModuleName, OldElementName: string;
  const FPDocFilename: string;
  var ListOfLazFPDocNode: TFPList): TModalResult;

function LCLEncodeAmps(const AmpString: string): string;

// identifier references in lfm
function GatherLFMsReferences(Files:TStringList;
  const Identifier: string;
  DeclTool: TCodeTool;
  DeclNode: TCodeTreeNode;
  var ListOfReferences: TCodeXYPositions;
  const Flags: TFindRefsFlags): TModalResult;
function DeclarationCanBeInLFM(DeclNode: TCodeTreeNode): boolean;

implementation

{$R *.lfm}

function ShowFindRenameIdentifierDialog(const Filename: string;
  var Position: TPoint; AllowRename: boolean; SetRenameActive: boolean;
  Options: TFindRenameIdentifierOptions; IsSourceName: boolean): TModalResult;
var
  FindRenameIdentifierDialog: TFindRenameIdentifierDialog;
begin
  FindRenameIdentifierDialog:=TFindRenameIdentifierDialog.Create(nil);
  try
    FindRenameIdentifierDialog.LoadFromConfig;
    FindRenameIdentifierDialog.SetIdentifier(Filename,Position,IsSourceName);
    FindRenameIdentifierDialog.AllowRename:=AllowRename;
    FindRenameIdentifierDialog.RenameCheckBox.Checked:=SetRenameActive and AllowRename;
    if Options<>nil then
      FindRenameIdentifierDialog.ShowResultCheckBox.Checked:=Options.RenameShowResult and AllowRename;
    Result:=FindRenameIdentifierDialog.ShowModal;
    if Result=mrOk then
      if Options<>nil then
        FindRenameIdentifierDialog.SaveToOptions(Options);
  finally
    FindRenameIdentifierDialog.Free;
    FindRenameIdentifierDialog:=nil;
  end;
end;

function GetDeclCodeNode(const DeclCodeXY: TCodeXYPosition; out DeclTool: TCodeTool;
  out DeclNode: TCodeTreeNode; out DeclCleanPos: integer): boolean;

  procedure Err(id: int64; Msg: string);
  begin
    Msg:='DoFindRenameIdentifier: '+Msg;
    debugln(['Error: ',DeclCodeXY.Code.Filename,'(',DeclCodeXY.Y,',',DeclCodeXY.X,') [',id,'] ',Msg]);
    CodeToolBoss.SetError(id,DeclCodeXY.Code,DeclCodeXY.Y,DeclCodeXY.X,Msg);
    LazarusIDE.DoJumpToCodeToolBossError;
  end;

begin
  Result:=false;
  DeclTool:=nil;
  DeclNode:=nil;
  if DeclCodeXY.Code=nil then exit;
  CodeToolBoss.Explore(DeclCodeXY.Code,DeclTool,false);
  if DeclTool=nil then begin
    debugln(['Error: (lazarus) [20250206142319] DoFindRenameIdentifier CodeToolBoss.Explore failed']);
    LazarusIDE.DoJumpToCodeToolBossError;
    exit;
  end;
  if DeclTool.CaretToCleanPos(DeclCodeXY,DeclCleanPos)<>0 then begin
    Err(20250206143746,'position not in Pascal');
    exit;
  end;
  DeclNode:=DeclTool.FindDeepestNodeAtPos(DeclCleanPos,false);
  if DeclNode=nil then begin
    Err(20250206143807,'no Pascal node');
    exit;
  end;
  if (DeclNode.Desc=ctnIdentifier)
      and (DeclNode.Parent.Desc in [ctnSrcName,ctnUseUnitClearName,ctnUseUnitNamespace]) then
    DeclNode:=DeclNode.Parent;

  Result:=true;
end;

function RenameIdentifier_UnitFile(const OldFileName, NewFilename, NewUnitName: string;
  ListOfSrcNameRefs: TObjectList; out OldRefs: TSrcNameRefs; UpdateOldRefs: boolean): TModalResult;
var
  anUnitInfo: TUnitInfo;
  LFMCode, LRSCode, OldCode, NewCode: TCodeBuffer;
  i: Integer;
  Refs: TSrcNameRefs;
  Tool: TCodeTool;
begin
  Result:=mrOk;
  OldRefs:=nil;
  if (CompareFilenames(OldFileName,NewFileName)=0)
      and (ExtractFileName(OldFileName)=ExtractFilename(NewFilename)) then exit;
  anUnitInfo:=nil;
  if Assigned(Project1) then
    anUnitInfo:=Project1.UnitInfoWithFilename(OldFileName);
  if anUnitInfo=nil then begin
    debugln(['Error: RenameIdentifier_UnitFile missing unitinfo "',OldFileName,'"']);
    exit(mrCancel);
  end;

  OldCode:=anUnitInfo.Source;
  if ListOfSrcNameRefs<>nil then begin
    for i:=0 to ListOfSrcNameRefs.Count-1 do begin
      Refs:=TSrcNameRefs(ListOfSrcNameRefs[i]);
      if Refs.Tool.Scanner.MainCode=OldCode then begin
        OldRefs:=Refs;
        ListOfSrcNameRefs.OwnsObjects:=false;
        ListOfSrcNameRefs.Delete(i);
        ListOfSrcNameRefs.OwnsObjects:=true;
        break;
      end;
    end;
  end;

  LFMCode:=nil;
  LRSCode:=nil;
  Result:=RenameUnit(anUnitInfo,NewFilename,NewUnitName,LFMCode,LRSCode,true);

  if UpdateOldRefs and (OldRefs<>nil) then begin
    NewCode:=anUnitInfo.Source;
    CodeToolBoss.Explore(NewCode,Tool,true);
    OldRefs.Tool:=Tool;
    OldRefs.NewLocalSrcName:=NewUnitName;
    ReplaceCodeInTreeOfPCodeXYPosition(OldRefs.TreeOfPCodeXYPosition,OldCode,NewCode);
  end;
end;

function LCLEncodeAmps(const AmpString: string): string;
var i, len: integer;
begin
  Result:='';
  i:=1;
  len:= length(AmpString);
  while i<=len do begin
    if AmpString[i]='&' then begin
      Result:= Result + '&&';
      inc(i);
    end;
    if (i<=len) and (isIdentStartChar[AmpString[i]]) then
      while (i<=len) and isIdentChar[AmpString[i]] do begin
        Result:= Result + AmpString[i];
        inc(i);
      end;
    if (i>len) or (AmpString[i]<>'.') then break;
    Result:= Result + AmpString[i];
    inc(i);
  end;
end;

procedure CheckDeclOfDesigner(const Identifier: string; DeclTool: TCodeTool; DeclNode: TCodeTreeNode);
var
  UnitInfo: TUnitInfo;
  aComponent: TComponent;
begin
  UnitInfo:=Project1.UnitInfoWithFilename(DeclTool.MainFilename);
  if UnitInfo=nil then exit;
  aComponent:=UnitInfo.Component;
  if aComponent=nil then exit;

  if DeclNode.Desc=ctnVarDefinition then begin
    if (DeclNode.Parent.Desc in [ctnImplementation,ctnProgram])
        and SameText(Identifier,aComponent.Name) then
    begin
      // renaming a designer root component
      // todo
    end;
  end;
end;

function GatherLFMsReferences(Files: TStringList; const Identifier: string; DeclTool: TCodeTool;
  DeclNode: TCodeTreeNode; var ListOfReferences: TCodeXYPositions; const Flags: TFindRefsFlags
  ): TModalResult;
var
  i: integer;
  LFMBuffer, Code: TCodeBuffer;
  UnitInfo: TUnitInfo;
  LFMFilename, Filename, DeclFilename: String;
  aCache: CodeToolsStructs.TPointerToPointerTree;
begin
  Result:=mrOk;
  ListOfReferences:=nil;
  if Files=nil then exit;
  if Identifier='' then exit;
  if DeclNode=nil then exit;
  if not (frfIncludingLFM in Flags) then exit;
  DeclFilename:=DeclTool.MainFilename;
  if not FilenameIsPascalUnit(DeclTool.MainFilename) then exit;

  if not DeclarationCanBeInLFM(DeclNode) then exit;

  {$IFNDEF EnableFindLFMRefs}
  exit;
  {$ENDIF}

  debugln(['GatherLFMsReferences Files.Count=',Files.Count]);
  // Note: this only supports lfm, not other form formats like dfm or fmx

  aCache:=CodeToolsStructs.TPointerToPointerTree.Create;
  try
    if frfRename in Flags then
      CheckDeclOfDesigner(Identifier,DeclTool,DeclNode);

    // search in other lfm
    for i:=0 to Files.Count-1 do begin
      UnitInfo:=Project1.UnitInfoWithFilename(Files[i]);
      if UnitInfo=nil then
        continue;

      Filename:=UnitInfo.Filename;
      if not FilenameIsAbsolute(Filename) then continue;
      if not FilenameIsPascalSource(Filename) then continue;
      LFMFilename:=ChangeFileExt(Filename,'.lfm');
      if not FileExistsCached(LFMFilename) then
        continue;

      // check if DeclTool in unit path
      if not CodeToolBoss.IsUnitInUnitPath(Filename,DeclFilename,aCache) then
        continue;

      // load lfm source
      LFMBuffer:=CodeToolBoss.LoadFile(LFMFilename,true,false);
      if LFMBuffer=nil then continue;

      // check if identifier exists in lfm
      if Pos(LowerCase(Identifier),LowerCase(LFMBuffer.Source))<1 then
        continue;

      // parse lfm
      Code:=CodeToolBoss.LoadFile(Filename,true,false);
      if Code=nil then continue;

      CodeToolBoss.GatherReferencesInLFM(Code, LFMBuffer, Identifier,
        DeclTool, DeclNode, ListOfReferences, Flags);
    end;
  finally
    aCache.Free;
  end;
  Result:= mrOK;
end;

function DeclarationCanBeInLFM(DeclNode: TCodeTreeNode): boolean;
begin
  Result:=false;
  if DeclNode.HasParentOfType(ctnImplementation) then
    exit; // cant be referenced in lfm
  if DeclNode.Desc=ctnProcedureHead then
    DeclNode:=DeclNode.Parent;
  case DeclNode.Desc of
  ctnProperty:
    ; // even private properties can later be made published -> must be searched
  ctnVarDefinition:
    if not (DeclNode.Parent.Desc in AllClassBaseSections) then
      exit; // not a field, e.g. a parameter or local var
  ctnProcedure:
    if not (DeclNode.Parent.Desc in AllClassBaseSections) then
      exit; // not a method
  ctnTypeDefinition: ;
  ctnEnumIdentifier: ;
  else
    exit;
  end;
  Result:=true;
end;

function DoFindRenameIdentifier(AllowRename: boolean; SetRenameActive: boolean;
  Options: TFindRenameIdentifierOptions): TModalResult;
var
  DeclCleanPos: integer;
  DeclTool: TCodeTool;
  DeclNode: TCodeTreeNode;
  DeclCodeXY: TCodeXYPosition;

  procedure Err(id: int64; Msg: string);
  begin
    Msg:='DoFindRenameIdentifier: '+Msg;
    debugln(['Error: ',DeclCodeXY.Code.Filename,'(',DeclCodeXY.Y,',',DeclCodeXY.X,') [',id,'] ',Msg]);
    CodeToolBoss.SetError(id,DeclCodeXY.Code,DeclCodeXY.Y,DeclCodeXY.X,Msg);
    LazarusIDE.DoJumpToCodeToolBossError;
  end;

  function UpdateCodeNode: boolean;
  begin
    Result:=GetDeclCodeNode(DeclCodeXY,DeclTool,DeclNode,DeclCleanPos);
  end;

  function CheckUsesNode: boolean;
  var
    InFilename, aUnitName, Dir, Filename: string;
    NewCode: TCodeBuffer;
    NewTool: TCodeTool;
  begin
    case DeclNode.Desc of
    ctnUseUnit: ;
    ctnUseUnitNamespace,ctnUseUnitClearName:
      DeclNode:=DeclNode.Parent;
    else
      exit(true);
    end;
    // renaming a uses -> rename the unit
    // find unit
    Result:=false;
    aUnitName:=RemoveAmpersands(DeclTool.ExtractUsedUnitName(DeclNode,@InFilename));
    if aUnitName='' then begin
      Err(20250206143851,'ExtractUsedUnitName failed');
      exit;
    end;
    Dir:=ExtractFilePath(DeclTool.MainFilename);
    Filename:=CodeToolBoss.DirectoryCachePool.FindUnitSourceInCompletePath(Dir,aUnitName,InFilename);
    if Filename='' then begin
      Err(20250206143916,'unit "'+aUnitName+'" not found');
      exit;
    end;
    // load unit
    NewCode:=CodeToolBoss.LoadFile(Filename,true,false);
    if NewCode=nil then begin
      Err(20250206143931,'unable to load file "'+Filename+'"');
      exit;
    end;
    // parse
    if not CodeToolBoss.Explore(NewCode,NewTool,true) then begin
      debugln(['Error: (lazarus) [20250206142339] DoFindRenameIdentifier CodeToolBoss.Explore failed']);
      LazarusIDE.DoJumpToCodeToolBossError;
      exit;
    end;
    DeclTool:=NewTool;
    DeclCodeXY.Code:=NewCode;
    DeclCodeXY.X:=1;
    DeclCodeXY.Y:=1;
    DeclNode:=DeclTool.GetSourceNameNode;
    if DeclNode=nil then begin
      Err(20250206144454,'failed to find unit name');
      exit;
    end;
    DeclTool.CleanPosToCaret(DeclNode.StartPos,DeclCodeXY);
    Result:=true;
  end;

  function AddExtraFiles(Files: TStrings): boolean;
  // TODO: replace Files: TStringsList with a AVL tree
  var
    i: Integer;
    CurFileMask: string;
    FileInfo: TSearchRec;
    CurDirectory: String;
    CurFilename: String;
    OnlyPascalSources: Boolean;
    SPMaskType: TSPMaskType;
    FilesTree: TFilenameToStringTree;
    FTItem: PStringToStringItem;
  begin
    Result:=false;
    if (Options.ExtraFiles<>nil) then begin
      for i:=0 to Options.ExtraFiles.Count-1 do begin
        CurFileMask:=Options.ExtraFiles[i];
        if not GlobalMacroList.SubstituteStr(CurFileMask) then begin
          Err(20250206153855,'invalid file mask "'+CurFileMask+'"');
          exit;
        end;
        CurFileMask:=ChompPathDelim(CurFileMask);
        if not FilenameIsAbsolute(CurFileMask) then begin
          if LazarusIDE.ActiveProject.IsVirtual then continue;
          CurFileMask:=AppendPathDelim(LazarusIDE.ActiveProject.Directory+CurFileMask);
        end;
        CurFileMask:=TrimFilename(CurFileMask);
        SPMaskType:=GetSPMaskType(CurFileMask);
        if SPMaskType<>TSPMaskType.None then
        begin
          FilesTree:=TFilenameToStringTree.Create(false);
          try
            CollectFilesInSearchPath(CurFileMask,FilesTree);
            for FTItem in FilesTree do
            begin
              if not FilenameIsPascalSource(FTItem^.Name) then
                continue;
              if FileIsText(FTItem^.Name) then
                Files.Add(FTItem^.Name);
            end;
          finally
            FilesTree.Free;
          end;
          continue;
        end;

        OnlyPascalSources:=false;
        if DirPathExistsCached(CurFileMask) then begin
          // a whole directory
          OnlyPascalSources:=true;
          CurFileMask:=AppendPathDelim(CurFileMask)+AllFilesMask;
        end else if FileExistsCached(CurFileMask) then begin
          // single file
          Files.Add(CurFileMask);
          continue;
        end else begin
          // a mask
        end;
        if FindFirstUTF8(CurFileMask,faAnyFile,FileInfo)=0
        then begin
          CurDirectory:=AppendPathDelim(ExtractFilePath(CurFileMask));
          repeat
            // check if special file
            if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
            then
              continue;
            if OnlyPascalSources and not FilenameIsPascalSource(FileInfo.Name)
            then
              continue;
            CurFilename:=CurDirectory+FileInfo.Name;
            //debugln(['AddExtraFiles ',CurFilename]);
            if FileIsText(CurFilename) then
              Files.Add(CurFilename);
          until FindNextUTF8(FileInfo)<>0;
        end;
        FindCloseUTF8(FileInfo);
      end;
    end;
    Result:=true;
  end;

var
  StartSrcEdit: TSourceEditorInterface;
  StartSrcCode, LastCode, Code: TCodeBuffer;
  DeclTopLine, StartTopLine, i, j: integer;
  StartCaretXY, DeclXY: TPoint;
  OwnerList, ListOfLazFPDocNode: TFPList;
  ExtraFiles: TStrings;
  Files: TStringList;
  PascalReferences: TObjectList; // list of TSrcNameRefs
  LFMReferences: TCodeXYPositions;
  OldChange, Completed, MovingFile, RenamingFile, IsConflicted, DoLowercase, Confirm,
    NewFileCreated: Boolean;
  Graph: TUsesGraph;
  AVLNode: TAVLTreeNode;
  UGUnit: TUGUnit;
  Identifier, NewFilename, OldFileName, PasFilename, s: string;
  FindRefFlags: TFindRefsFlags;
  TreeOfPCodeXYPosition, LFMTreeOfPCodeXYPosition: TAVLTree;
  Refs, OldRefs: TSrcNameRefs;
  AUnitInfo: TUnitInfo;
begin
  Result:=mrCancel;
  if not LazarusIDE.BeginCodeTools then exit(mrCancel);
  StartSrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  StartSrcCode:=TCodeBuffer(StartSrcEdit.CodeToolsBuffer);
  StartTopLine:=StartSrcEdit.TopLine;
  RenamingFile:=False;

  // find the main declaration
  StartCaretXY:=StartSrcEdit.CursorTextXY;
  if not CodeToolBoss.FindMainDeclaration(StartSrcCode,
    StartCaretXY.X,StartCaretXY.Y,
    DeclCodeXY.Code,DeclCodeXY.X,DeclCodeXY.Y,DeclTopLine) then
  begin
    LazarusIDE.DoJumpToCodeToolBossError;
    exit(mrCancel);
  end;
  DeclTool:=nil;
  if not UpdateCodeNode then exit;
  if not CheckUsesNode then exit;

  DeclXY:=Point(DeclCodeXY.X,DeclCodeXY.Y);
  Result:=LazarusIDE.DoOpenFileAndJumpToPos(DeclCodeXY.Code.Filename, DeclXY,
    DeclTopLine,-1,-1,[ofOnlyIfExists,ofRegularFile,ofDoNotLoadResource]);
  if Result<>mrOk then
    exit;

  CodeToolBoss.GetIdentifierAt(DeclCodeXY.Code,DeclCodeXY.X,DeclCodeXY.Y,Identifier);
  Files:=nil;
  OwnerList:=nil;
  PascalReferences:=nil;
  LFMReferences:=nil;
  ListOfLazFPDocNode:=nil;
  NewFilename:='';
  NewFileCreated:=false;
  OldRefs:=nil;
  try
    // let user choose the search scope
    Result:=ShowFindRenameIdentifierDialog(DeclCodeXY.Code.Filename,DeclXY,
      AllowRename,SetRenameActive,nil,DeclNode.Desc=ctnSrcName);
    if Result<>mrOk then begin
      debugln('Error: (lazarus) DoFindRenameIdentifier failed: user cancelled dialog');
      exit;
    end;

    Options:=MiscellaneousOptions.FindRenameIdentifierOptions;
    if Options.Rename then begin
      OldFileName:=DeclCodeXY.Code.Filename;
      if DeclNode.Desc=ctnSrcName then
      begin
        // rename unit/program
        DoLowercase:=false;
        if Options.RenameTo<>lowercase(Options.RenameTo) then begin
          // new identifier is not lowercase
          case EnvironmentOptions.CharcaseFileAction of
          ccfaAsk:
            begin
              Confirm:=true;
              s:=ExtractFileName(OldFileName);
              if (s=lowercase(s)) and (Identifier<>lowercase(Identifier)) then begin
                // old unitname was mixed case and old file was lowercase -> keep policy, no need to ask
                Confirm:=false;
              end;
              if Confirm then begin
                s:=RemoveAmpersands(Options.RenameTo)+ExtractFileExt(OldFileName);
                Result:=IDEQuestionDialog(lisFileNotLowercase,
                  Format(lisTheUnitIsNotLowercaseTheFreePascalCompiler,
                         [s, LineEnding, LineEnding+LineEnding]),
                  mtConfirmation,[mrYes,mrNo,mrCancel],'');
                case Result of
                mrYes: DoLowercase:=true;
                mrNo: ;
                else
                  exit(mrCancel);
                end;
              end;
            end;
          ccfaAutoRename:
            // always lower case
            DoLowercase:=true;
          else
            // use mixed case for filename
          end;
        end;
        if DoLowercase then
          NewFilename:=ExtractFilePath(OldFileName)+
            lowercase(RemoveAmpersands(Options.RenameTo)+ExtractFileExt(OldFileName))
        else
          NewFilename:=ExtractFilePath(OldFileName)+
            RemoveAmpersands(Options.RenameTo)+
            ExtractFileExt(OldFileName);

        // Check if new file already exists (change in case is silently done)
        MovingFile:=CompareFilenames(ExtractFilePath(OldFileName),ExtractFilePath(NewFilename))<>0;
        RenamingFile:=MovingFile or (ExtractFileName(NewFilename)<>ExtractFileName(OldFileName));
        if (MovingFile or not SameText(ExtractFileName(NewFilename),ExtractFileName(OldFileName)))
            and CodeToolBoss.DirectoryCachePool.FileExists(NewFilename,ctsfcAllCase)
        then begin
          IDEMessageDialog(lisRenamingAborted,
            Format(lisFileAlreadyExists,[FindDiskFilename(NewFilename)]),
            mtError,[mbOK]);
          exit(mrCancel);
        end;
      end;
    end;

    if not UpdateCodeNode then exit(mrCancel);

    // create the file list
    Files:=TStringList.Create;
    Files.Add(DeclCodeXY.Code.Filename);
    if CompareFilenames(DeclCodeXY.Code.Filename,StartSrcCode.Filename)<>0 then
      Files.Add(StartSrcCode.Filename);

    // add packages, projects
    case Options.Scope of
    frProject:
      begin
        OwnerList:=TFPList.Create;
        OwnerList.Add(LazarusIDE.ActiveProject);
      end;
    frOwnerProjectPackage,frAllOpenProjectsAndPackages:
      begin
        OwnerList:=PackageEditingInterface.GetOwnersOfUnit(StartSrcCode.Filename);
        if (OwnerList<>nil) and (OwnerList.Count=0) then
          FreeAndNil(OwnerList);
        if (OwnerList=nil) then
          OwnerList:=PackageEditingInterface.GetPossibleOwnersOfUnit(
            StartSrcCode.Filename,[piosfExcludeOwned,piosfIncludeSourceDirectories]);
        if (OwnerList<>nil) and (OwnerList.Count=0) then
          FreeAndNil(OwnerList);
        if (OwnerList<>nil) then begin
          if Options.Scope=frAllOpenProjectsAndPackages then begin
            PackageEditingInterface.ExtendOwnerListWithUsedByOwners(OwnerList);
            ReverseList(OwnerList);
          end;
        end else begin
          // unknown unit -> search everywhere
          OwnerList:=TFPList.Create;
          OwnerList.Add(LazarusIDE.ActiveProject);
          for i:=0 to PackageEditingInterface.GetPackageCount-1 do
            OwnerList.Add(PackageEditingInterface.GetPackages(i));
          ReverseList(OwnerList);
        end;
      end;
    end;

    // get source files of packages and projects
    if OwnerList<>nil then begin
      // start in all listed files of the package(s)
      ExtraFiles:=PackageEditingInterface.GetSourceFilesOfOwners(OwnerList);
      if ExtraFiles<>nil then
      begin
        // parse all used units
        Graph:=CodeToolBoss.CreateUsesGraph;
        try
          for i:=0 to ExtraFiles.Count-1 do
            Graph.AddStartUnit(ExtraFiles[i]);
          Graph.AddTargetUnit(DeclCodeXY.Code.Filename);
          Graph.Parse(true,Completed);
          AVLNode:=Graph.FilesTree.FindLowest;
          while AVLNode<>nil do begin
            UGUnit:=TUGUnit(AVLNode.Data);
            Files.Add(UGUnit.Filename);
            AVLNode:=AVLNode.Successor;
          end;
        finally
          ExtraFiles.Free;
          Graph.Free;
        end;
      end;
    end;

    //debugln(['DoFindRenameIdentifier ',Files.Text]);

    // add user defined extra files
    if not AddExtraFiles(Files) then
      exit(mrCancel);

    // search pascal source references
    FindRefFlags:=[];
    if Options.Rename then
      Include(FindRefFlags,frfRename);
    if Options.Overrides then
      Include(FindRefFlags,frfMethodOverrides);
    if not GatherIdentifierReferences(Files,DeclCodeXY,DeclTool,DeclNode,
      Options.SearchInComments,PascalReferences,FindRefFlags) then
    begin
      debugln('Error: 20250206162727 DoFindRenameIdentifier GatherIdentifierReferences failed');
      exit(mrCancel);
    end;

    // search references in lfm files
    if Options.IncludeLFMs then
      Include(FindRefFlags,frfIncludingLFM);

    if (frfIncludingLFM in FindRefFlags)
        and (GatherLFMsReferences(Files, Identifier, DeclTool, DeclNode,
          LFMReferences, FindRefFlags)<>mrOk) then
    begin
      debugln('Error: 20250506120810 DoFindRenameIdentifier GatherLFMsReferences failed');
      exit(mrCancel);
    end;

    {$IFDEF EnableFPDocRename}
    // search fpdoc references
    Result:=GatherFPDocReferencesForPascalFiles(Files,DeclarationUnitInfo.Source,
                                  DeclarationCaretXY,ListOfLazFPDocNode);
    if Result<>mrOk then begin
      debugln('Error: (lazarus) DoFindRenameIdentifier GatherFPDocReferences failed');
      exit;
    end;
    {$ENDIF}

    // ToDo: search i18n references
    // ToDo: search fpdoc references

    if Options.Rename then begin

      if RenamingFile then begin
        // rename file, and associated lfm, res, etc,
        // keeping source editor and session data
        // rename source name in this file
        // -> extract the references (OldRefs) for this file to show them later
        Result:=RenameIdentifier_UnitFile(OldFileName,
                NewFilename, Options.RenameTo, PascalReferences, OldRefs,
                Options.RenameShowResult);
        if Result<>mrOk then
          exit(mrCancel);

        DeclCodeXY.Code:=CodeToolBoss.LoadFile(NewFilename,false,false);
        NewFileCreated:=true;
      end;

      // rename identifier
      OldChange:=LazarusIDE.OpenEditorsOnCodeToolChange;
      LazarusIDE.OpenEditorsOnCodeToolChange:=true;
      try
        // todo: check for conflicts and show user list
        IsConflicted:=false;
        Result:=mrOk;
        if DeclNode.Desc=ctnSrcName then begin
          if not CodeToolBoss.RenameSourceNameReferences(OldFileName,NewFilename,
              Options.RenameTo,PascalReferences) then
            Result:=mrCancel;
        end else begin
          if (PascalReferences<>nil) and (PascalReferences.Count>0) then begin
            Refs:=TSrcNameRefs(PascalReferences[0]);
            TreeOfPCodeXYPosition:=Refs.TreeOfPCodeXYPosition;
            if not CodeToolBoss.RenameIdentifier(TreeOfPCodeXYPosition,
                Identifier, Options.RenameTo, DeclCodeXY.Code, @DeclXY) then
              Result:=mrCancel;
          end;
          LFMTreeOfPCodeXYPosition:=nil;
          if (LFMReferences<>nil) and (LFMReferences.Count>0) then begin
            try
              LFMTreeOfPCodeXYPosition:=CreateTreeOfPCodeXYPosition;
              for i:=0 to LFMReferences.Count-1 do
                LFMTreeOfPCodeXYPosition.Add(LFMReferences.Items[i]);

              if not CodeToolBoss.RenameIdentifierInLFMs(LFMTreeOfPCodeXYPosition,
                Identifier, Options.RenameTo) then begin
                // error occured, show something
                Result:=mrCancel;
              end;
            finally
              LFMTreeOfPCodeXYPosition.Free;
            end;
          end;
        end;

        if Result<>mrOk then begin
          if IsConflicted then
            IDEMessageDialog(lisRenamingConflict,
              Format(lisIdentifierIsAlreadyUsed2,[Options.RenameTo]),
              mtError,[mbOK])
          else
            LazarusIDE.DoJumpToCodeToolBossError;
            debugln('Error: (lazarus) DoFindRenameIdentifier unable to commit');
          exit(mrCancel);
        end;

        // ToDo: rename fpdoc references

        // hack designers
        if LFMReferences<>nil then begin
          LastCode:=nil;
          for i:=0 to LFMReferences.Count-1 do begin
            Code:=LFMReferences.Items[i]^.Code;
            if (Code<>LastCode) then begin
              LastCode:=Code;
              // hack LastCode related designers
              AUnitInfo:=nil;
              for j:=low(PascalSourceExt) to high(PascalSourceExt) do begin
                PasFilename:=ExtractFileNameWithoutExt(Code.Filename)+PascalSourceExt[j];
                AUnitInfo:=Project1.UnitInfoWithFilename(PasFilename);
                if AUnitInfo<>nil then
                  break;
              end;

              if AUnitInfo=nil then
                continue;

              if AUnitInfo.EditorInfoCount>1 then
                for j:= AUnitInfo.EditorInfoCount-1 downto 1 do
                  CloseEditorFile(AUnitInfo.EditorInfo[j].EditorComponent,[cfQuiet,
                    cfCloseDependencies]);

              //force dumb saving
              ReloadUnitComponent(AUnitInfo);

            end;
          end;
        end;
      finally
        LazarusIDE.OpenEditorsOnCodeToolChange:=OldChange;
      end;

      if Options.RenameShowResult then begin
        if OldRefs<>nil then begin
          // re-add the references
          //debugln(['DoFindRenameIdentifier NewRefs: MainFilename="',OldRefs.Tool.MainFilename,'" NewName="',OldRefs.NewLocalSrcName,'"']);
          PascalReferences.Insert(0,OldRefs);
          OldRefs:=nil;
        end;
        Result:=ShowIdentifierReferences(DeclCodeXY.Code.Filename,
          PascalReferences,LFMReferences,Identifier,Options.RenameTo);
      end;

    end else begin //no renaming, only references - always shown
      Result:=ShowIdentifierReferences(DeclCodeXY.Code.Filename,
        PascalReferences,LFMReferences,Identifier);
    end;

  finally
    OldRefs.Free;
    Files.Free;
    OwnerList.Free;
    PascalReferences.Free;
    LFMReferences.Free;
    FreeListObjects(ListOfLazFPDocNode,true);

    if RenamingFile and NewFileCreated then
      // source renamed -> jump to new file
      Result:=LazarusIDE.DoOpenFileAndJumpToPos(NewFilename, DeclXY,
        StartTopLine,-1,-1,[ofOnlyIfExists,ofRegularFile,ofDoNotLoadResource])
    else
      // jump back to where user started
      Result:=LazarusIDE.DoOpenFileAndJumpToPos(StartSrcCode.Filename, StartCaretXY,
        StartTopLine,-1,-1,[ofOnlyIfExists,ofRegularFile,ofDoNotLoadResource]);
  end;
end;

function GatherIdentifierReferences(Files: TStringList; const DeclCodeXY: TCodeXYPosition;
  DeclTool: TCodeTool; DeclNode: TCodeTreeNode; SearchInComments: boolean; out
  ListOfSrcNameRefs: TObjectList; const Flags: TFindRefsFlags): boolean;
var
  i, DeclCleanPos: Integer;
  LoadResult: TModalResult;
  Code: TCodeBuffer;
  ListOfPCodeXYPosition: TFPList;
  Cache: TFindIdentifierReferenceCache;
  TreeOfPCodeXYPosition: TAVLTree;
  Refs: TSrcNameRefs;
  Filename: String;
begin
  Result:=false;
  ListOfSrcNameRefs:=nil;
  ListOfPCodeXYPosition:=nil;
  TreeOfPCodeXYPosition:=nil;
  Cache:=nil;
  try
    CleanUpFileList(Files);
    for i:=Files.Count-1 downto 0 do begin
      Filename:=Files[i];
      if FilenameIsAbsolute(Filename) and not FileExistsCached(Filename) then
        Files.Delete(i);
    end;

    if DeclNode=nil then begin
      if not GetDeclCodeNode(DeclCodeXY,DeclTool,DeclNode,DeclCleanPos) then
        exit;
    end;

    if DeclNode.Desc=ctnSrcName then begin
      // search source name references
      if not CodeToolBoss.FindSourceNameReferences(DeclCodeXY.Code.Filename,Files,
        not SearchInComments,ListOfSrcNameRefs) then
      begin
        debugln('GatherIdentifierReferences CodeToolBoss.FindSourceNameReferences failed');
        if CodeToolBoss.ErrorMessage='' then
          CodeToolBoss.SetError(20250206162241,DeclCodeXY.Code,DeclCodeXY.Y,DeclCodeXY.X,'CodeToolBoss.FindSourceNameReferences failed');
        LazarusIDE.DoJumpToCodeToolBossError;
        exit;
      end;
    end else begin
      // search identifier in every file
      for i:=0 to Files.Count-1 do begin
        //debugln(['GatherIdentifierReferences ',Files[i]]);
        LoadResult:=
            LoadCodeBuffer(Code,Files[i],[lbfCheckIfText,lbfUpdateFromDisk,lbfIgnoreMissing],true);
        if LoadResult=mrAbort then begin
          debugln('GatherIdentifierReferences unable to load "',Files[i],'"');
          exit;
        end;
        if LoadResult<>mrOk then continue;

        // search references
        CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
        if not CodeToolBoss.FindReferences(
          DeclCodeXY.Code,DeclCodeXY.X,DeclCodeXY.Y,
          Code, not SearchInComments, ListOfPCodeXYPosition, Cache, Flags) then
        begin
          debugln('GatherIdentifierReferences CodeToolBoss.FindReferences failed in "',Code.Filename,'"');
          if CodeToolBoss.ErrorMessage='' then
            CodeToolBoss.SetError(20250206161149,Code,1,1,'CodeToolBoss.FindReferences failed');
          LazarusIDE.DoJumpToCodeToolBossError;
          exit;
        end;
        //debugln('GatherIdentifierReferences FindReferences in "',Code.Filename,'" ',dbgs(ListOfPCodeXYPosition<>nil));

        // add to tree
        if ListOfPCodeXYPosition<>nil then begin
          if TreeOfPCodeXYPosition=nil then
            TreeOfPCodeXYPosition:=CodeToolBoss.CreateTreeOfPCodeXYPosition;
          CodeToolBoss.AddListToTreeOfPCodeXYPosition(ListOfPCodeXYPosition,
                                                TreeOfPCodeXYPosition,true,false);
        end;
      end;
      if TreeOfPCodeXYPosition<>nil then begin
        ListOfSrcNameRefs:=TObjectList.Create(true);
        Refs:=TSrcNameRefs.Create;
        Refs.TreeOfPCodeXYPosition:=TreeOfPCodeXYPosition;
        TreeOfPCodeXYPosition:=nil;
        if ListOfSrcNameRefs=nil then
          ListOfSrcNameRefs:=TObjectList.Create(true);
        ListOfSrcNameRefs.Add(Refs);
      end;
    end;

    Result:=true;
  finally
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
    CodeToolBoss.FreeTreeOfPCodeXYPosition(TreeOfPCodeXYPosition);
    Cache.Free;
  end;
end;

function ShowIdentifierReferences(DeclFilename: string;
  ListOfSrcNameRefs: TObjectList; LFMReferences: TCodeXYPositions;
  Identifier: string; RenameTo: string): TModalResult;
var
  OldSearchPageIndex: TTabSheet;
  SearchPageIndex: TTabSheet;
  lOptions: TLazFindInFileSearchOptions;
begin
  if (ListOfSrcNameRefs=nil) or (ListOfSrcNameRefs.Count=0) then exit(mrOk);

  Result:=mrCancel;
  LazarusIDE.DoShowSearchResultsView(iwgfShow);
  SearchPageIndex:=nil;
  try
    // create a search result page
    //debugln(['ShowIdentifierReferences ',DbgSName(SearchResultsView)]);
    if RenameTo = '' then
      lOptions := []
    else
      lOptions := [fifReplace];

    SearchPageIndex:=SearchResultsView.AddSearch(
      Identifier,
      RenameTo,
      ExtractFilePath(DeclFilename),
      '*.pas;*.pp;*.p;*.inc',
      lOptions);
    if SearchPageIndex = nil then exit;

    // list results
    SearchResultsView.BeginUpdate(SearchPageIndex.PageIndex);
    AddReferencesToResultView(Identifier,ListOfSrcNameRefs,LFMReferences,true,
      SearchPageIndex.PageIndex, RenameTo);


    OldSearchPageIndex:=SearchPageIndex;
    SearchPageIndex:=nil;
    Identifier:=LCLEncodeAmps(Identifier);
    SearchResultsView.EndUpdate(OldSearchPageIndex.PageIndex, 'Ref: '+Identifier);
    IDEWindowCreators.ShowForm(SearchResultsView,true);

  finally
    if SearchPageIndex <> nil then
      SearchResultsView.EndUpdate(SearchPageIndex.PageIndex, 'Ref: '+Identifier);
  end;
  Result:=mrOK;
end;

procedure AddReferencesToResultView(Identifier: string;
  ListOfSrcNameRefs: TObjectList; LFMReferences: TCodeXYPositions;
  ClearItems: boolean; SearchPageIndex: integer; RenameTo: string = '');
var
  CodePos: PCodeXYPosition;
  CurLine, TrimmedLine, CurIdentifier: String;
  TrimCnt: Integer;
  ANode: TAVLTreeNode;
  CaretXY: TCodeXYPosition;
  i, Len: integer;
  CleanPos: integer;
  EndPos: integer;
  CodeTool: TCodeTool;
  Refs: TSrcNameRefs;
  Tree: TAVLTree;
begin
  SearchResultsView.BeginUpdate(SearchPageIndex);
  if ClearItems then
    SearchResultsView.Items[SearchPageIndex].Clear;
  if ListOfSrcNameRefs<>nil then begin
    for i:=0 to ListOfSrcNameRefs.Count-1 do begin
      Refs:=TSrcNameRefs(ListOfSrcNameRefs[i]);
      Tree:=Refs.TreeOfPCodeXYPosition;
      if Tree=nil then continue;

      CurIdentifier:=Refs.NewLocalSrcName;
      if CurIdentifier='' then
        CurIdentifier:=RenameTo;
      if CurIdentifier='' then
        CurIdentifier:=Identifier;

      ANode:=Tree.FindHighest;
      while ANode<>nil do begin
        CodePos:=PCodeXYPosition(ANode.Data);
        ANode:=Tree.FindPrecessor(ANode);

        CurLine:=TrimRight(CodePos^.Code.GetLine(CodePos^.Y-1,false));
        TrimmedLine:=Trim(CurLine);
        TrimCnt:=length(CurLine)-length(TrimmedLine);
        //debugln('ShowReferences x=',dbgs(CodePos^.x),' y=',dbgs(CodePos^.y),' ',CurLine);
        Len:=length(CurIdentifier);
        if Pos('.',CurIdentifier)>0 then begin
          CodeToolBoss.Explore(CodePos^.Code,CodeTool,true);
          CaretXY.X:=CodePos^.X;
          CaretXY.Y:=CodePos^.Y;
          CaretXY.Code:=CodePos^.Code;
          if CodeTool.CaretToCleanPos(CaretXY,CleanPos)<>0 then
            continue;
          CodeTool.ExtractIdentifierWithPointsOutEndPos(CleanPos,EndPos,
            length(Identifier));
          Len:=EndPos-CleanPos;
        end;
        SearchResultsView.AddMatch(SearchPageIndex,
                                   CodePos^.Code.Filename,
                                   Point(CodePos^.X,CodePos^.Y),
                                   Point(CodePos^.X+Len,CodePos^.Y),
                                   TrimmedLine,
                                   CodePos^.X-TrimCnt, Len);
      end;
    end;
  end;

  if (LFMReferences<>nil) and (LFMReferences.Count>0) then begin
    if RenameTo<>'' then
      Len:=Length(RenameTo)
    else
      Len:=Length(Identifier);

    Tree:=CreateTreeOfPCodeXYPosition;

    for i:=0 to LFMReferences.Count-1 do
      Tree.Add(LFMReferences.Items[i]);

    ANode:=Tree.FindHighest;
    while ANode<>nil do begin
      CodePos:=PCodeXYPosition(ANode.Data);
      ANode:=Tree.FindPrecessor(ANode);

      CurLine:=TrimRight(CodePos^.Code.GetLine(CodePos^.Y-1,false));
      TrimmedLine:=Trim(CurLine);
      TrimCnt:=length(CurLine)-length(TrimmedLine);

      SearchResultsView.AddMatch(SearchPageIndex,
                                 CodePos^.Code.Filename,
                                 Point(CodePos^.X,CodePos^.Y),
                                 Point(CodePos^.X+Len,CodePos^.Y),
                                 TrimmedLine,
                                 CodePos^.X-TrimCnt, Len);
    end;
    Tree.Free;
  end;
  SearchResultsView.EndUpdate(SearchPageIndex);
end;

function GatherFPDocReferencesForPascalFiles(PascalFiles: TStringList;
  DeclarationCode: TCodeBuffer; const DeclarationCaretXY: TPoint;
  var ListOfLazFPDocNode: TFPList): TModalResult;
var
  PascalFilenames, FPDocFilenames: TFilenameToStringTree;
  CacheWasUsed: boolean;
  Chain: TCodeHelpElementChain;
  CHResult: TCodeHelpParseResult;
  CHElement: TCodeHelpElement;
  FPDocFilename: String;
  S2SItem: PStringToStringItem;
begin
  Result:=mrCancel;
  PascalFilenames:=nil;
  FPDocFilenames:=nil;
  try
    // gather FPDoc files
    CleanUpFileList(PascalFiles);

    PascalFilenames:=TFilenameToStringTree.Create(false);
    PascalFilenames.AddNames(PascalFiles);
    CodeHelpBoss.GetFPDocFilenamesForSources(PascalFilenames,true,FPDocFilenames);
    if FPDocFilenames=nil then begin
      DebugLn(['GatherFPDocReferences no fpdoc files found']);
      exit(mrOk);
    end;

    // get codehelp element
    CHResult:=CodeHelpBoss.GetElementChain(DeclarationCode,
             DeclarationCaretXY.X,DeclarationCaretXY.Y,true,Chain,CacheWasUsed);
    if CHResult<>chprSuccess then begin
      DebugLn(['GatherFPDocReferences CodeHelpBoss.GetElementChain failed']);
      exit;
    end;
    CHElement:=Chain[0];
    DebugLn(['GatherFPDocReferences OwnerName=',CHElement.ElementOwnerName,' FPDocPkg=',CHElement.ElementFPDocPackageName,' Name=',CHElement.ElementName]);

    // search FPDoc files
    for S2SItem in FPDocFilenames do begin
      FPDocFilename:=S2SItem^.Name;
      Result:=GatherReferencesInFPDocFile(
                CHElement.ElementFPDocPackageName,CHElement.ElementUnitName,
                CHElement.ElementName,
                FPDocFilename,ListOfLazFPDocNode);
      if Result<>mrOk then exit;
    end;

    Result:=mrOk;
  finally
    PascalFilenames.Free;
    FPDocFilenames.Free;
    if Result<>mrOk then begin
      FreeListObjects(ListOfLazFPDocNode,true);
      ListOfLazFPDocNode:=nil;
    end;
  end;
end;

function GatherReferencesInFPDocFile(
  const OldPackageName, OldModuleName, OldElementName: string;
  const FPDocFilename: string;
  var ListOfLazFPDocNode: TFPList
  ): TModalResult;
var
  DocFile: TLazFPDocFile;
  IsSamePackage: Boolean;
  IsSameModule: Boolean;// = same unit

  procedure CheckLink(Node: TDOMNode; Link: string);
  var
    p: LongInt;
    PackageName: String;
  begin
    if Link='' then exit;
    if Link[1]='#' then begin
      p:=System.Pos('.',Link);
      if p<1 then exit;
      PackageName:=copy(Link,2,p-2);
      if SysUtils.CompareText(PackageName,OldPackageName)<>0 then exit;
      delete(Link,1,p);
    end;
    if (SysUtils.CompareText(Link,OldElementName)=0)
    or (SysUtils.CompareText(Link,OldModuleName+'.'+OldElementName)=0) then
    begin
      DebugLn(['CheckLink Found: ',Link]);
      if ListOfLazFPDocNode=nil then
        ListOfLazFPDocNode:=TFPList.Create;
      ListOfLazFPDocNode.Add(TLazFPDocNode.Create(DocFile,Node));
    end;
  end;

  procedure SearchLinksInChildNodes(Node: TDomNode);
  // search recursively for links
  begin
    Node:=Node.FirstChild;
    while Node<>nil do begin
      if (Node.NodeName='link')
      and (Node is TDomElement) then begin
        CheckLink(Node,TDomElement(Node).GetAttribute('id'));
      end;
      SearchLinksInChildNodes(Node);
      Node:=Node.NextSibling;
    end;
  end;

var
  CHResult: TCodeHelpParseResult;
  CacheWasUsed: boolean;
  Node: TDOMNode;
begin
  Result:=mrCancel;
  DebugLn(['GatherFPDocReferences ',
    ' OldPackageName=',OldPackageName,
    ' OldModuleName=',OldModuleName,' OldElementName=',OldElementName,
    ' FPDocFilename=',FPDocFilename]);

  CHResult:=CodeHelpBoss.LoadFPDocFile(FPDocFilename,[chofUpdateFromDisk],
                                       DocFile,CacheWasUsed);
  if CHResult<>chprSuccess then begin
    DebugLn(['GatherReferencesInFPDocFile CodeHelpBoss.LoadFPDocFile failed File=',FPDocFilename]);
    exit(mrCancel);
  end;

  // search in Doc nodes
  IsSamePackage:=SysUtils.CompareText(DocFile.GetPackageName,OldPackageName)=0;
  IsSameModule:=SysUtils.CompareText(DocFile.GetModuleName,OldModuleName)=0;
  DebugLn(['GatherReferencesInFPDocFile ',DocFile.GetPackageName,'=',OldPackageName,' ',DocFile.GetModuleName,'=',OldModuleName]);
  Node:=DocFile.GetFirstElement;
  while Node<>nil do begin
    if Node is TDomElement then begin
      if (SysUtils.CompareText(TDomElement(Node).GetAttribute('name'),OldElementName)=0)
      and IsSamePackage and IsSameModule
      then begin
        // this is the element itself
        DebugLn(['GatherReferencesInFPDocFile Element itself found: ',Node.NodeName,' ',Node.NodeValue]);
        if ListOfLazFPDocNode=nil then
          ListOfLazFPDocNode:=TFPList.Create;
        ListOfLazFPDocNode.Add(TLazFPDocNode.Create(DocFile,Node));
      end;
      CheckLink(Node,TDomElement(Node).GetAttribute('link'));
      SearchLinksInChildNodes(Node);
    end;
    Node:=Node.NextSibling;
  end;

  Result:=mrOk;
end;

{ TFindRenameIdentifierDialog }

procedure TFindRenameIdentifierDialog.FindRenameIdentifierDialogCreate(
  Sender: TObject);
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
  end else if not IsSourceName and (Pos('.',FNewIdentifier)>0) then
  begin
    ok:=false;
    Err:=Format(lisIdentifierCannotBeDotted,[FNewIdentifier]);
  end;

  if ok
      and (FNode.Desc=ctnBeginBlock) // 'Result'
      and (CompareIdentifiers(PChar(FNewIdentifier),
                              PChar(FOldIdentifier))<>0) // only case change allowed
  then begin
    // Result inside function
    ok:=false;
    Err:=Format(lisIdentifierIsReservedWord,['Result']);
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

procedure TFindRenameIdentifierDialog.SetAllowRename(const AValue: boolean);
begin
  if FAllowRename=AValue then exit;
  FAllowRename:=AValue;
  UpdateRename;
end;

procedure TFindRenameIdentifierDialog.SetIsPrivate(const AValue: boolean);
begin
  if FIsPrivate=AValue then exit;
  FIsPrivate:=AValue;
  ExtraFilesGroupBox.Enabled:=not IsPrivate;
  ScopeRadioGroup.Enabled:=not IsPrivate;
  ScopeRadioGroup.ItemIndex:=0;
end;

procedure TFindRenameIdentifierDialog.SetFiles(const Files: TStringList);
begin
  if FFiles<>nil then exit; //already set
  if Files = nil then exit;
  if Files.Count = 0 then exit;
  if FFiles=nil then FFiles:=TStringList.Create;
  FFiles.Assign(Files);
end;

procedure TFindRenameIdentifierDialog.FindOrRenameButtonClick(Sender: TObject);
var
  ACodeBuffer:TCodeBuffer;
  X,Y: integer;
  ContextPos: integer;
  ErrInfo: string;

  function GetCodePos(var aPos: integer; out X,Y:integer;
    pushBack: boolean = true):boolean;
  var
    CodeTool: TCodeTool;
    CaretXY: TCodeXYPosition;
    aTop: integer;
  begin
    X:=0;
    Y:=0;
    Result:=false;
    CodeToolBoss.Explore(ACodeBuffer,CodeTool,true);
    if CodeTool<>nil then begin
      CodeTool.MoveCursorToCleanPos(aPos);
      CodeTool.ReadNextAtom;
      if pushBack then begin
        CodeTool.ReadPriorAtom;
        if not (CodeTool.CurPos.Flag in [cafWord, cafEnd]) and (CodeTool.CurPos.StartPos>0) then
          CodeTool.ReadPriorAtom;
        if (CodeTool.CurPos.Flag=cafEnd) and (CodeTool.CurPos.StartPos>0) then
          CodeTool.ReadPriorAtom;
        CodeTool.ReadNextAtom;
      end;
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
      if anItem.Node<>nil then begin
        ContextPos:=anItem.Node.StartPos;
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
        ContextPos:=tmpNode.Parent.EndPos;//can point at the end of "end;"
        if GetCodePos(ContextPos,X,Y) then
          CodeToolBoss.GatherIdentifiers(ACodeBuffer, X, Y);
        FindConflict; //ErrInfo is set inside the function
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
  {$IFDEF EnableFindLFMRefs}
  ScopeIncludeLFMs.Checked:=Options.IncludeLFMs;
  {$ELSE}
  ScopeIncludeLFMs.Checked:=false;
  ScopeIncludeLFMs.Visible:=false;
  {$ENDIF}
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
  var NewIdentifierPosition: TPoint; IsSrcName: boolean);
var
  s: String;
  ACodeBuffer, CurCode: TCodeBuffer;
  ListOfCodeBuffer: TFPList;
  i: Integer;
  CodeXY: TCodeXYPosition;
  CleanPos: integer;
  Node: TCodeTreeNode;
begin
  IsSourceName:=IsSrcName;
  FIdentifierFilename:=NewIdentifierFilename;
  FIdentifierPosition:=NewIdentifierPosition;
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
  FOldIdentifier:='';
  if ACodeBuffer=nil then begin
    CurrentGroupBox.Caption:='?file not found?';
    exit;
  end;

  // Check if this is an include file and list files this unit/program
  CodeToolBoss.GetIncludeCodeChain(ACodeBuffer,true,ListOfCodeBuffer);
  if ListOfCodeBuffer<>nil then begin
    for i:=0 to ListOfCodeBuffer.Count-1 do begin
      CurCode:=TCodeBuffer(ListOfCodeBuffer[i]);
      if CurCode=ACodeBuffer then break;
      s:=CurCode.Filename;
      CurrentListBox.Items.Insert(0,s);
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
  ScopeOverridesCheckBox.Visible:=(Node<>nil) and (Node.Desc=ctnProcedureHead)
      and (FTool.ProcNodeHasSpecifier(Node,psVirtual) or FTool.ProcNodeHasSpecifier(Node,psOverride));

  if CodeToolBoss.GetIdentifierAt(ACodeBuffer,
    NewIdentifierPosition.X,NewIdentifierPosition.Y,FOldIdentifier,FNode) then
  begin
    CurrentGroupBox.Caption:= Format(lisFRIIdentifier,[''])+LCLEncodeAmps(FOldIdentifier);
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
      UnitInfo:=Project1.UnitInfoWithFilename(Files[i]);
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

  if not IsSourceName and (Pos('.',FNewIdentifier)>0) then
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


