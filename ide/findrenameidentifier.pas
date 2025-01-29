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
  IdentCompletionTool, KeywordFuncLists, CTUnitGraph, CodeTree, CodeAtom, LinkScanner,
  CodeCache, CodeToolManager, BasicCodeTools,
  // LazUtils
  LazFileUtils, LazFileCache, laz2_DOM, LazStringUtils, AvgLvlTree, LazLoggerBase,
  // IdeIntf
  IdeIntfStrConsts, LazIDEIntf, IDEWindowIntf, SrcEditorIntf, PackageIntf,
  IDEDialogs, InputHistory,
  // IdeUtils
  DialogProcs,
  // LazConfig
  TransferMacros, IDEProcs, SearchPathProcs,
  // IDE
  LazarusIDEStrConsts, MiscOptions, CodeToolsOptions, SearchResultView, CodeHelp, CustomCodeTool,
  FindDeclarationTool, SourceFileManager, Project;

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
    FOldIdentifier: string;
    FNewIdentifier: string;
    FForbidden: TStringList;
    FIsPrivate: boolean;
    FNode: TCodeTreeNode;
    FTool: TCustomCodeTool;
    FFiles: TStringList;
    procedure SetAllowRename(const AValue: boolean);
    procedure SetIsPrivate(const AValue: boolean);
    procedure SetFiles(const Files:TStringList);
    procedure UpdateRename;
    procedure GatherProhibited;
    function NewIdentifierIsConflicted(var ErrMsg: string): boolean;
  public
    procedure LoadFromConfig;
    procedure SaveToConfig;
    procedure LoadFromOptions(Options: TFindRenameIdentifierOptions);
    procedure SaveToOptions(Options: TFindRenameIdentifierOptions);
    procedure SetIdentifier(const NewIdentifierFilename: string;
                            var NewIdentifierPosition: TPoint);

    property IdentifierFilename: string read FIdentifierFilename;
    property IdentifierPosition: TPoint read FIdentifierPosition;
    property AllowRename: boolean read FAllowRename write SetAllowRename;
    property IsPrivate: boolean read FIsPrivate write SetIsPrivate;
  end;

function ShowFindRenameIdentifierDialog(const Filename: string;
  var Position: TPoint;
  AllowRename: boolean; // allow user to disable/enable rename
  SetRenameActive: boolean; // check rename
  Options: TFindRenameIdentifierOptions): TModalResult;
function DoFindRenameIdentifier(
  AllowRename: boolean; // allow user to disable/enable rename
  SetRenameActive: boolean; // check rename
  Options: TFindRenameIdentifierOptions): TModalResult;
function GatherIdentifierReferences(Files: TStringList;
  DeclarationCode: TCodeBuffer; const DeclarationCaretXY: TPoint;
  SearchInComments: boolean;
  var TreeOfPCodeXYPosition: TAVLTree; const Flags: TFindRefsFlags): TModalResult;
function ShowIdentifierReferences(
  DeclarationCode: TCodeBuffer; const DeclarationCaretXY: TPoint;
  TreeOfPCodeXYPosition: TAVLTree;
  Identifier: string;
  RenameTo: string = '';
  InsertedCommentsAllowed: boolean=false): TModalResult;
procedure AddReferencesToResultView(DeclarationCode: TCodeBuffer;
  const DeclarationCaretXY: TPoint;
  TreeOfPCodeXYPosition: TAVLTree; ClearItems: boolean; SearchPageIndex: integer;
  InsertedCommentsAllowed: boolean=false);

function GatherFPDocReferencesForPascalFiles(PascalFiles: TStringList;
  DeclarationCode: TCodeBuffer; const DeclarationCaretXY: TPoint;
  var ListOfLazFPDocNode: TFPList): TModalResult;
function GatherReferencesInFPDocFile(
  const OldPackageName, OldModuleName, OldElementName: string;
  const FPDocFilename: string;
  var ListOfLazFPDocNode: TFPList): TModalResult;
function RenameUnitFromFileName(OldFileName: string; NewUnitName: string):
  TModalResult;
function ShowSaveProject(NewProjectName: string): TModalResult;
function EnforceAmp(AmpString: string): string;

implementation

{$R *.lfm}

function ShowFindRenameIdentifierDialog(const Filename: string;
  var Position: TPoint; AllowRename: boolean; SetRenameActive: boolean;
  Options: TFindRenameIdentifierOptions): TModalResult;
var
  FindRenameIdentifierDialog: TFindRenameIdentifierDialog;
begin
  FindRenameIdentifierDialog:=TFindRenameIdentifierDialog.Create(nil);
  try
    FindRenameIdentifierDialog.LoadFromConfig;
    FindRenameIdentifierDialog.SetIdentifier(Filename,Position);
    FindRenameIdentifierDialog.AllowRename:=AllowRename;
    FindRenameIdentifierDialog.RenameCheckBox.Checked:=SetRenameActive and AllowRename;
    if Options<>nil then
      FindRenameIdentifierDialog.ShowResultCheckBox.Checked:=Options.RenameShowResult and AllowRename;
    Result:=FindRenameIdentifierDialog.ShowModal;
    if Result=mrOk then
      if Options<>nil then
        FindRenameIdentifierDialog.SaveToOptions(Options);
  finally
    FindRenameIdentifierDialog.FForbidden.Free;
    FindRenameIdentifierDialog.FFiles.Free;
    FindRenameIdentifierDialog.Free;
    FindRenameIdentifierDialog:=nil;
  end;
end;

function DoFindRenameIdentifier(AllowRename: boolean; SetRenameActive: boolean;
  Options: TFindRenameIdentifierOptions): TModalResult;

  // TODO: replace Files: TStringsList with a AVL tree

  function AddExtraFiles(Files: TStrings): TModalResult;
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
    Result:=mrCancel;
    if (Options.ExtraFiles<>nil) then begin
      for i:=0 to Options.ExtraFiles.Count-1 do begin
        CurFileMask:=Options.ExtraFiles[i];
        if not GlobalMacroList.SubstituteStr(CurFileMask) then exit;
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
    Result:=mrOk;
  end;

var
  StartSrcEdit: TSourceEditorInterface;
  DeclCode, StartSrcCode: TCodeBuffer;
  DeclX, DeclY, DeclTopLine, StartTopLine, i: integer;
  LogCaretXY, DeclarationCaretXY: TPoint;
  OwnerList: TFPList;
  ExtraFiles: TStrings;
  Files: TStringList;
  Identifier: string;
  PascalReferences: TAVLTree;
  ListOfLazFPDocNode: TFPList;
  CurUnitname: String;
  OldChange, Completed, RewriteNeeded: Boolean;
  Graph: TUsesGraph;
  Node: TAVLTreeNode;
  UGUnit: TUGUnit;
  TargetFilename, OrigFileName, ChangedFileType, srcNamed, lfmString: string;
  SkipRenamingFile, isConflicted: Boolean;
  FindRefFlags: TFindRefsFlags;
begin
  Result:=mrCancel;
  if not LazarusIDE.BeginCodeTools then exit(mrCancel);
  StartSrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  StartSrcCode:=TCodeBuffer(StartSrcEdit.CodeToolsBuffer);
  StartTopLine:=StartSrcEdit.TopLine;
  RewriteNeeded:=False;
  ChangedFileType:='';

  // find the main declaration
  LogCaretXY:=StartSrcEdit.CursorTextXY;
  if not CodeToolBoss.FindMainDeclaration(StartSrcCode,
    LogCaretXY.X,LogCaretXY.Y,
    DeclCode,DeclX,DeclY,DeclTopLine) then
  begin
    LazarusIDE.DoJumpToCodeToolBossError;
    exit(mrCancel);
  end;
  DeclarationCaretXY:=Point(DeclX,DeclY);
  Result:=LazarusIDE.DoOpenFileAndJumpToPos(DeclCode.Filename, DeclarationCaretXY,
    DeclTopLine,-1,-1,[ofOnlyIfExists,ofRegularFile,ofDoNotLoadResource]);
  if Result<>mrOk then
    exit;

  CodeToolBoss.GetIdentifierAt(DeclCode,DeclarationCaretXY.X,DeclarationCaretXY.Y,Identifier);
  CurUnitname:= ExtractFileNameOnly(DeclCode.Filename);
  Files:=nil;
  OwnerList:=nil;
  PascalReferences:=nil;
  ListOfLazFPDocNode:=nil;
  try
    // let user choose the search scope
    Result:=ShowFindRenameIdentifierDialog(DeclCode.Filename,DeclarationCaretXY,
      AllowRename,SetRenameActive,nil);
    if Result<>mrOk then begin
      debugln('Error: (lazarus) DoFindRenameIdentifier failed: user cancelled dialog');
      exit;
    end;

    // create the file list
    Files:=TStringList.Create;
    Files.Add(DeclCode.Filename);
    if CompareFilenames(DeclCode.Filename,StartSrcCode.Filename)<>0 then
      Files.Add(StartSrcCode.Filename);

    Options:=MiscellaneousOptions.FindRenameIdentifierOptions;

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
    end;

    //debugln(['DoFindRenameIdentifier ',Files.Text]);

    // add user defined extra files
    Result:=AddExtraFiles(Files);
    if Result<>mrOk then begin
      debugln('Error: (lazarus) DoFindRenameIdentifier unable to add user defined extra files');
      exit;
    end;

    // search pascal source references
    FindRefFlags:=[];
    if Options.Overrides then
      Include(FindRefFlags,frfMethodOverrides);
    Result:=GatherIdentifierReferences(Files,DeclCode,
      DeclarationCaretXY,Options.SearchInComments,PascalReferences,FindRefFlags);
    if CodeToolBoss.ErrorMessage<>'' then
      LazarusIDE.DoJumpToCodeToolBossError;
    if Result<>mrOk then begin
      debugln('Error: (lazarus) DoFindRenameIdentifier GatherIdentifierReferences failed');
      exit;
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
    // ToDo: search designer references

    if Options.Rename then begin

      SkipRenamingFile:= true;
      if (CompareDottedIdentifiers(PChar(Identifier),PChar(CurUnitName))=0) then
      begin //are project/units names involved?
        TargetFilename:=ExtractFilePath(DeclCode.Filename)+
          LowerCase(StringReplace(Options.RenameTo,'&','',[rfReplaceAll]))+
          ExtractFileExt(DeclCode.Filename);
        OrigFileName:=DeclCode.Filename;
        SkipRenamingFile:= CompareFileNames(ExtractFilenameOnly(TargetFilename),
          ExtractFilenameOnly(DeclCode.Filename))=0;
        if not SkipRenamingFile and FileExists(TargetFilename) then begin
          IDEMessageDialog(lisRenamingAborted,
            Format(lisFileAlreadyExists,[TargetFilename]),
            mtError,[mbOK]);
          exit(mrCancel);
        end;
        ChangedFileType:= CodeToolBoss.GetSourceType(DeclCode,False);
        if ChangedFileType='' then SkipRenamingFile:= true else begin
           case ChangedFileType[2] of
           {P}'R':srcNamed:=dlgFoldPasProgram;
           {L}'I':srcNamed:=lisPckOptsLibrary;
           {P}'A':srcNamed:=lisPackage;
           else
             {U} srcNamed:=dlgFoldPasUnit;
           end;
        end;
        if not SkipRenamingFile and (IDEMessageDialog(srkmecRenameIdentifier,
             Format(lisTheIdentifierIsAUnitProceedAnyway,
             [srcNamed,LineEnding,LineEnding]),
             mtInformation,[mbCancel, mbOK],'') <> mrOK) then
          exit(mrCancel);
        RewriteNeeded:=True;
      end;

      // rename identifier
      OldChange:=LazarusIDE.OpenEditorsOnCodeToolChange;
      LazarusIDE.OpenEditorsOnCodeToolChange:=true;
      try
        isConflicted:=false; // todo: check for conflicts and show user list
        if not CodeToolBoss.RenameIdentifier(PascalReferences,
          Identifier, Options.RenameTo, DeclCode, @DeclarationCaretXY)
        then begin
          if isConflicted then
            IDEMessageDialog(lisRenamingConflict,
              Format(lisIdentifierWasAlreadyUsed,[Options.RenameTo]),
              mtError,[mbOK])
          else
            LazarusIDE.DoJumpToCodeToolBossError;
            debugln('Error: (lazarus) DoFindRenameIdentifier unable to commit');
          exit(mrCancel);
        end else begin
          // ToDo: search lfm source references
        end;
      finally
        LazarusIDE.OpenEditorsOnCodeToolChange:=OldChange;
      end;
      if Options.RenameShowResult and not RewriteNeeded  then
        Result:=ShowIdentifierReferences(DeclCode,
          DeclarationCaretXY,PascalReferences,Identifier,Options.RenameTo);
    end else begin //no renaming, only references - always shown
      Result:=ShowIdentifierReferences(DeclCode,
        DeclarationCaretXY,PascalReferences,Identifier,'',true);
    end;

    if RewriteNeeded then begin
      if not SkipRenamingFile then begin
        if ChangedFileType[1]='U' then //UNIT
          Result:= RenameUnitFromFileName(OrigFileName, Options.RenameTo)
        else //PROGRAM, LIBRARY, PACKAGE
          Result:= ShowSaveProject(Options.RenameTo);
      end else
        Result:= mrOK;
      if Result=mrOK then
      if Options.RenameShowResult then begin
        i:=0;
        while (i<=Files.Count-1) do begin
          if (CompareFileNames(PChar(Files[i]), PChar(OrigFileName))=0) then begin
            Files.Delete(i);
            Files.Add(TargetFilename);
            break;
          end;
          Inc(i);
        end;
        DeclCode:=CodeToolBoss.LoadFile(TargetFilename,false,false);
        //cursor pos for declaration is the same as before renaming
        CodeToolBoss.FreeTreeOfPCodeXYPosition(PascalReferences);
        Result:=GatherIdentifierReferences(Files,DeclCode,
          DeclarationCaretXY,Options.SearchInComments,PascalReferences,FindRefFlags);
        if CodeToolBoss.ErrorMessage<>'' then
          LazarusIDE.DoJumpToCodeToolBossError;
        if Result<>mrOk then begin
          debugln('Error: (lazarus) DoFindRenameIdentifier GatherIdentifierReferences failed');
          exit;
        end;

        Result:=ShowIdentifierReferences(DeclCode,
          DeclarationCaretXY,PascalReferences,Identifier,Options.RenameTo);
      end;
    end;
  finally
    Files.Free;
    OwnerList.Free;
    CodeToolBoss.FreeTreeOfPCodeXYPosition(PascalReferences);
    FreeListObjects(ListOfLazFPDocNode,true);

    // jump back in source editor - depens on the need to rename Files
    if RewriteNeeded and not SkipRenamingFile and (Result=mrOK) then
      Result:=LazarusIDE.DoOpenFileAndJumpToPos(TargetFilename, LogCaretXY,
        StartTopLine,-1,-1,[ofOnlyIfExists,ofRegularFile,ofDoNotLoadResource])
    else
      Result:=LazarusIDE.DoOpenFileAndJumpToPos(StartSrcCode.Filename, LogCaretXY,
        StartTopLine,-1,-1,[ofOnlyIfExists,ofRegularFile,ofDoNotLoadResource]);
  end;
end;

function GatherIdentifierReferences(Files: TStringList; DeclarationCode: TCodeBuffer;
  const DeclarationCaretXY: TPoint; SearchInComments: boolean; var TreeOfPCodeXYPosition: TAVLTree;
  const Flags: TFindRefsFlags): TModalResult;
var
  i: Integer;
  LoadResult: TModalResult;
  Code: TCodeBuffer;
  ListOfPCodeXYPosition: TFPList;
  Cache: TFindIdentifierReferenceCache;
begin
  Result:=mrCancel;
  ListOfPCodeXYPosition:=nil;
  TreeOfPCodeXYPosition.Free;
  TreeOfPCodeXYPosition:=nil;
  Cache:=nil;
  try
    CleanUpFileList(Files);

    // search in every file
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
        DeclarationCode,DeclarationCaretXY.X,DeclarationCaretXY.Y,
        Code, not SearchInComments, ListOfPCodeXYPosition, Cache, Flags) then
      begin
        debugln('GatherIdentifierReferences unable to FindReferences in "',Code.Filename,'"');
        Result:=mrAbort;
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

    Result:=mrOk;
  finally
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
    if Result<>mrOk then
      CodeToolBoss.FreeTreeOfPCodeXYPosition(TreeOfPCodeXYPosition);
    Cache.Free;
  end;
end;

function ShowIdentifierReferences(
  DeclarationCode: TCodeBuffer; const DeclarationCaretXY: TPoint;
  TreeOfPCodeXYPosition: TAVLTree;
  Identifier: string;
  RenameTo: string;
  InsertedCommentsAllowed: boolean=false): TModalResult;
var
  OldSearchPageIndex: TTabSheet;
  SearchPageIndex: TTabSheet;
  lOptions: TLazFindInFileSearchOptions;
begin
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
      ExtractFilePath(DeclarationCode.Filename),
      '*.pas;*.pp;*.p;*.inc',
      lOptions);
    if SearchPageIndex = nil then exit;

    // list results
    SearchResultsView.BeginUpdate(SearchPageIndex.PageIndex);
    AddReferencesToResultView(DeclarationCode,DeclarationCaretXY,
                   TreeOfPCodeXYPosition,true,SearchPageIndex.PageIndex,
                   InsertedCommentsAllowed and
                   IsDottedIdentifier(Identifier));
    OldSearchPageIndex:=SearchPageIndex;
    SearchPageIndex:=nil;
    Identifier:=EnforceAmp(Identifier);
    SearchResultsView.EndUpdate(OldSearchPageIndex.PageIndex, 'Ref: '+Identifier);
    IDEWindowCreators.ShowForm(SearchResultsView,true);

  finally
    if SearchPageIndex <> nil then
      SearchResultsView.EndUpdate(SearchPageIndex.PageIndex, 'Ref: '+Identifier);
  end;
  Result:=mrOK;
end;

procedure AddReferencesToResultView(DeclarationCode: TCodeBuffer;
  const DeclarationCaretXY: TPoint; TreeOfPCodeXYPosition: TAVLTree;
  ClearItems: boolean; SearchPageIndex: integer;
  InsertedCommentsAllowed: boolean=false);
var
  Identifier: string;
  CodePos: PCodeXYPosition;
  CurLine: String;
  TrimmedLine: String;
  TrimCnt: Integer;
  ANode: TAVLTreeNode;
  Xout,Yout: integer;
  CaretXY: TCodeXYPosition;
  AmdLen: integer;
  CleanPos: integer;
  AmdEndPos: integer;
  CodeTool: TCodeTool;
begin
  Xout:=DeclarationCaretXY.X;
  Yout:=DeclarationCaretXY.Y;
  CodeToolBoss.GetIdentifierAt(DeclarationCode,Xout,Yout,Identifier);

  SearchResultsView.BeginUpdate(SearchPageIndex);
  if ClearItems then
    SearchResultsView.Items[SearchPageIndex].Clear;
  if (TreeOfPCodeXYPosition<>nil) then begin
    ANode:=TreeOfPCodeXYPosition.FindHighest;
    while ANode<>nil do begin
      CodePos:=PCodeXYPosition(ANode.Data);
      CurLine:=TrimRight(CodePos^.Code.GetLine(CodePos^.Y-1,false));
      TrimmedLine:=Trim(CurLine);
      TrimCnt:=length(CurLine)-length(TrimmedLine);
      //debugln('ShowReferences x=',dbgs(CodePos^.x),' y=',dbgs(CodePos^.y),' ',CurLine);
      AmdLen:=0;
      if InsertedCommentsAllowed  then begin
        CodeToolBoss.Explore(CodePos^.Code,CodeTool,true);
        if CodeTool<>nil then begin
          CaretXY.X:=CodePos^.X;
          CaretXY.Y:=CodePos^.Y;
          CaretXY.Code:=CodePos^.Code;
          CodeTool.CaretToCleanPos(CaretXY,CleanPos);
          CodeTool.ExtractIdentifierWithPointsOutEndPos(CleanPos,AmdEndPos,
            length(Identifier));
          AmdLen:=AmdEndPos-CleanPos-length(Identifier);
        end;
      end;
      SearchResultsView.AddMatch(SearchPageIndex,
                                 CodePos^.Code.Filename,
                                 Point(CodePos^.X,CodePos^.Y),
                                 Point(CodePos^.X+length(Identifier)+AmdLen,CodePos^.Y),
                                 TrimmedLine,
                                 CodePos^.X-TrimCnt, length(Identifier)+AmdLen);//+dotted
      ANode:=TreeOfPCodeXYPosition.FindPrecessor(ANode);
    end;
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
  OpenUrl('http://wiki.freepascal.org/IDE_Window:_Find_or_Rename_identifier');
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
var  isOK:boolean;
    errInfo:string;
    dotPart:string;
    i:integer;
begin
  if FOldIdentifier='' then exit;
  errInfo:='';
  FNewIdentifier:=NewEdit.Text;
  if (FNode<>nil)
  and (FNode.Desc in [ctnProgram..ctnUnit,ctnUseUnitNamespace,ctnUseUnitClearName]) then
    isOK:=IsValidDottedIdent(FNewIdentifier) //can be dotted
  else
    isOK:=IsValidDottedIdent(FNewIdentifier,false);//not dotted for sure

  if not isOK then begin
    if FNewIdentifier='' then
      errInfo:=lisIdentifierCannotBeEmpty
    else
      errInfo:= format(lisIdentifierIsInvalid,[FNewIdentifier]);
  end;

  if isOK and (FTool<>nil) then begin
    i:=1;
    while isOK and (i<=length(FNewIdentifier)) do begin
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
      isOK:=not FTool.IsStringKeyWord(dotPart);
    end;
    if not isOK then
      errInfo:=Format(lisIdentifierIsReservedWord,[dotPart]);
  end;

  if isOK
  and (CompareDottedIdentifiers(PChar(FNewIdentifier),PChar(FOldIdentifier))<>0)
  then
    isOK:=not NewIdentifierIsConflicted(errInfo);

  errInfo:=StringReplace(errInfo,'&','&&',[rfReplaceAll]);
  ButtonPanel1.OKButton.Enabled:=isOK;
  if isOK then begin
    NewGroupBox.Caption:=lisFRIRenaming;
    NewGroupBox.Font.Style:=NewGroupBox.Font.Style-[fsBold];
  end
  else begin
    NewGroupBox.Caption:=lisFRIRenaming+' - '+ errInfo;
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
  if RenameCheckBox.Checked and (self.FForbidden=nil) then
    GatherProhibited;
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
  Res:TModalResult;
  ACodeBuffer:TCodeBuffer;
  anItem: TIdentifierListItem;
  tmpNode: TCodeTreeNode;
  X,Y: integer;
  ErrInfo: string;
  isOK: boolean;
  CTB_IdentComplIncludeKeywords: Boolean;
  CTB_CodeCompletionTemplateFileName: string;
  CTB_IdentComplIncludeWords: TIdentComplIncludeWords;
  ContextPos: integer;

  function GetCodePos(var aPos: integer; out X,Y:integer;
    pushBack: boolean = true):boolean;
  var
    CodeTool: TCodeTool;
    CaretXY: TCodeXYPosition;
    aTop, amdPos: integer;
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
    aNode: TCodeTreeNode;
  begin
    anItem:=CodeToolBoss.IdentifierList.FindIdentifier(PChar(FNewIdentifier));
    Result:=(anItem<>nil) and
      (CompareDottedIdentifiers(PChar(FOldIdentifier), PChar(FNewIdentifier))<>0);
    if Result then begin
      if anItem.Node<>nil then begin
        ContextPos:=anItem.Node.StartPos;
        ErrInfo:= Format(lisIdentifierWasAlreadyUsed,[FNewIdentifier]);
      end else begin
        if anItem.ResultType='' then
          ErrInfo:= Format(lisIdentifierIsDeclaredCompilerProcedure,[FNewIdentifier])
        else
          ErrInfo:= Format(lisIdentifierIsDeclaredCompilerFunction,[FNewIdentifier]);
      end;
    end;
  end;
begin
  if RenameCheckBox.Checked then
    ModalResult:=mrNone
  else begin
    ModalResult:=mrOK;
    exit;
  end;

  CTB_IdentComplIncludeKeywords:=CodeToolBoss.IdentComplIncludeKeywords;
  CodeToolBoss.IdentComplIncludeKeywords:=false;

  CTB_CodeCompletionTemplateFileName:=
    CodeToolsOptions.CodeToolsOpts.CodeCompletionTemplateFileName;
  CodeToolsOptions.CodeToolsOpts.CodeCompletionTemplateFileName:='';

  CTB_IdentComplIncludeWords:=CodeToolsOptions.CodeToolsOpts.IdentComplIncludeWords;
  CodeToolsOptions.CodeToolsOpts.IdentComplIncludeWords:=icwIncludeFromAllUnits;

  try
    anItem:=nil;
    isOK:=true;
    CodeToolBoss.IdentifierList.Clear;

    Res:= LoadCodeBuffer(ACodeBuffer,IdentifierFileName,[lbfCheckIfText],false);
    //try declaration context
    if Res=mrOK then begin
      tmpNode:=FNode;
      while isOK and (tmpNode<>nil) do begin
        if (tmpNode.Parent<>nil) and (tmpNode.Parent.Desc in AllFindContextDescs)
        then begin
          ContextPos:=tmpNode.Parent.EndPos;//can point at the end of "end;"
          if GetCodePos(ContextPos,X,Y) then
            CodeToolBoss.GatherIdentifiers(ACodeBuffer, X, Y);
            isOK:=not FindConflict; //ErrInfo is set inside the function
          end;
        tmpNode:=tmpNode.Parent;
      end;
    end;
  if isOK then
    ModalResult:=mrOk;
  finally
    CodeToolBoss.IdentComplIncludeKeywords:=
      CTB_IdentComplIncludeKeywords;
    CodeToolsOptions.CodeToolsOpts.CodeCompletionTemplateFileName:=
      CTB_CodeCompletionTemplateFileName;
    CodeToolsOptions.CodeToolsOpts.IdentComplIncludeWords:=
      CTB_IdentComplIncludeWords;

    if RenameCheckBox.Checked then begin
      ButtonPanel1.OKButton.Enabled:=isOK;
      if isOK then begin
        NewGroupBox.Caption:=lisFRIRenaming;
        NewGroupBox.Font.Style:=NewGroupBox.Font.Style-[fsBold];
      end else begin
        ErrInfo:=StringReplace(ErrInfo,'&','&&',[rfReplaceAll]);
        NewGroupBox.Caption:=lisFRIRenaming+' - '+ ErrInfo;
        NewGroupBox.Font.Style:=NewGroupBox.Font.Style+[fsBold];
      end;
    end;
  end;
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
  if ScopeRadioGroup.Enabled then
    case ScopeRadioGroup.ItemIndex of
    0: Options.Scope:=frCurrentUnit;
    1: Options.Scope:=frProject;
    2: Options.Scope:=frOwnerProjectPackage;
    else Options.Scope:=frAllOpenProjectsAndPackages;
    end;
end;

procedure TFindRenameIdentifierDialog.SetIdentifier(
  const NewIdentifierFilename: string; var NewIdentifierPosition: TPoint);
var
  s: String;
  ACodeBuffer: TCodeBuffer;
  ListOfCodeBuffer: TFPList;
  i: Integer;
  CurCode: TCodeBuffer;
  Tool: TCodeTool;
  CodeXY: TCodeXYPosition;
  CleanPos: integer;
  Node: TCodeTreeNode;
begin
  FIdentifierFilename:=NewIdentifierFilename;
  FIdentifierPosition:=NewIdentifierPosition;
  FNode:=nil;
  //debugln(['TFindRenameIdentifierDialog.SetIdentifier ',FIdentifierFilename,' ',dbgs(FIdentifierPosition)]);
  CurrentListBox.Items.Clear;
  s:=IdentifierFilename
     +'('+IntToStr(IdentifierPosition.Y)+','+IntToStr(IdentifierPosition.X)+')';
  CurrentListBox.Items.Add(s);
  LoadCodeBuffer(ACodeBuffer,IdentifierFileName,[lbfCheckIfText],false);
  if ACodeBuffer<>nil then begin
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
    if CodeToolBoss.GetIdentifierAt(ACodeBuffer,
      NewIdentifierPosition.X,NewIdentifierPosition.Y,FOldIdentifier,FNode) then
    begin
      CurrentGroupBox.Caption:= Format(lisFRIIdentifier,[''])+EnforceAmp(FOldIdentifier);
      NewEdit.Text:=FOldIdentifier;
    end else
      FOldIdentifier:='';
    // check if in implementation or private section
    if CodeToolBoss.Explore(ACodeBuffer,Tool,false) then begin
      CodeXY:=CodeXYPosition(NewIdentifierPosition.X,NewIdentifierPosition.Y,ACodeBuffer);
      if Tool.CaretToCleanPos(CodeXY,CleanPos)=0 then begin
        Node:=Tool.BuildSubTreeAndFindDeepestNodeAtPos(CleanPos,false);
        if (Node=nil)
        or Node.HasParentOfType(ctnImplementation)
        or Node.HasParentOfType(ctnClassPrivate) then
          IsPrivate:=true;
      end;
    end;
  end;
end;
procedure TFindRenameIdentifierDialog.GatherProhibited;
var
  StartSrcEdit: TSourceEditorInterface;
  DeclCode, StartSrcCode: TCodeBuffer;
  DeclX, DeclY, DeclTopLine, i: integer;
  LogCaretXY, DeclarationCaretXY: TPoint;
  OwnerList: TFPList;
  ExtraFiles: TStrings;
  Files: TStringList;
  CurUnitname: string;
  Graph: TUsesGraph;
  Node: TAVLTreeNode;
  UGUnit: TUGUnit;
  UnitInfo:TUnitInfo;
  Completed: boolean;
  ExternalProjectName, InternalProjectName: string;
begin
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

  try
    FTool:=CodeToolBoss.FindCodeToolForSource(DeclCode);
    FForbidden:=TStringList.Create;
    Files:=TStringList.Create;

    InternalProjectName:=
      Project1.ProjectUnitWithFilename(Project1.MainFilename).Unit_Name;
    ExternalProjectName:=ExtractFileNameOnly(Project1.MainFilename);
    if ExternalProjectName<>'' then begin
      // units cannot have filename matching project file name - only warnings/problems,
      // projects source names can be changed to match its file names,
      // other identifiers can be renamed to project file name - if this differs from
      // project source name.
      if (FNode<>nil) and
        (FNode.Desc in [ctnUseUnit,ctnUseUnitNamespace,ctnUseUnitClearName,ctnUnit])
        and (CompareDottedIdentifiers(PChar(ExternalProjectName),
        Pchar(InternalProjectName))<>0) then
          FForbidden.Add(ExternalProjectName);
    end;

    OwnerList:=TFPList.Create;
    OwnerList.Add(LazarusIDE.ActiveProject);

    // get source files of packages and projects
    if OwnerList<>nil then begin
      // start in all listed Files of the package(s)
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
    end;
    for i:=0 to Files.Count-1 do begin //get project/unit name
      UnitInfo:=Project1.UnitInfoWithFilename(Files[i]);
      if UnitInfo<>nil then begin
        CurUnitname:=UnitInfo.Unit_Name;
        FForbidden.Add(CurUnitname); //store for ValidateNewName
      end;
    end;
    SetFiles(Files);
  finally
    Files.Free;
    OwnerList.Free;
  end;
end;

function TFindRenameIdentifierDialog.NewIdentifierIsConflicted(var ErrMsg: string): boolean;
var
  i: integer;
  anItem: TIdentifierListItem;
begin
  Result:=false;
  ErrMsg:='';
  if not AllowRename then exit;
  if not (FNode.Desc in [ctnProgram..ctnUnit,ctnUseUnit,
      ctnUseUnitNamespace,ctnUseUnitClearName]) and
    (Pos('.',FNewIdentifier)>0) then
  begin
    ErrMsg:=Format(lisIdentifierCannotBeDotted,[FNewIdentifier]);
    exit(true);
  end;
  if FForbidden=nil then exit;
  if FNewIdentifier='' then begin
    ErrMsg:=lisIdentifierCannotBeEmpty;
    exit(true);
  end;
  i:=0;
  while (i<=FForbidden.Count-1) and
    (CompareDottedIdentifiers(PChar(FNewIdentifier),PChar(FForbidden[i]))<>0) do
    inc(i);
  Result:= i<=FForbidden.Count-1;

  if Result then begin
    ErrMsg:=Format(lisIdentifierWasAlreadyUsed,[FNewIdentifier]);
    exit;
  end;
  // checking if there are existing other identifiers conflited with the new
  // will be executed when "Rename all References" button is clicked
end;


function RenameUnitFromFileName(OldFileName: string; NewUnitName: string):
  TModalResult;
var
  AUnitInfoOld, UnitInfo : TUnitInfo;
  NewFileName: string;
  i:integer;
begin
  Result:=mrCancel;
  if not Assigned(Project1) then Exit;
  AUnitInfoOld:= Project1.UnitInfoWithFilename(OldFileName);
  if AUnitInfoOld=nil then Exit;
  AUnitInfoOld.ReadUnitSource(False,False);
  //OldUnitName:=AUnitInfoOld.Unit_Name;
  NewFileName:= ExtractFilePath(OldFileName)+
    lowerCase(NewUnitName + ExtractFileExt(OldFilename));
  if CompareFileNames(AUnitInfoOld.Filename,NewFileName)=0 then Exit;
  AUnitInfoOld.ClearModifieds;
  AUnitInfoOld.Unit_Name:=NewUnitName;
  Result:=SaveEditorFile(OldFileName,[sfProjectSaving,sfSaveAs,sfSkipReferences]);
  if Result = mrOK then
    DeleteFile(OldFileName);
end;

function ShowSaveProject(NewProjectName: string): TModalResult;
var
  AUnitInfoOld: TUnitInfo;
  Flags:TSaveFlags;
begin
  Result:=mrCancel;
  if not Assigned(Project1) then Exit;
  AUnitInfoOld:=Project1.MainUnitInfo;
  if AUnitInfoOld=nil then Exit;
  if Project1.MainUnitInfo.Unit_Name = NewProjectName then
    Flags:=[sfProjectSaving, sfSkipReferences]
  else begin
    Project1.MainUnitInfo.Unit_Name:= NewProjectName;
    Flags:=[sfSaveAs, sfProjectSaving, sfSkipReferences];
  end;
  Result:=SaveProject(Flags);
end;

function EnforceAmp(AmpString: string): string;
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
end.


