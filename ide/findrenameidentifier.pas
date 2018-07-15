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
  Classes, SysUtils, Laz_AVL_Tree,
  // LCL
  LCLProc, Forms, Controls, Dialogs, StdCtrls, ExtCtrls, ComCtrls, ButtonPanel,
  LclIntf,
  // CodeTools
  FileProcs, CTUnitGraph, CodeTree, CodeCache, CodeToolManager, BasicCodeTools,
  // LazUtils
  LazFileUtils, LazFileCache, laz2_DOM, AvgLvlTree,
  // IDE
  LazarusIDEStrConsts, IDEProcs, IDEWindowIntf, MiscOptions, DialogProcs,
  LazIDEIntf, IDEDialogs, SrcEditorIntf, PackageIntf, InputHistory,
  SearchResultView, CodeHelp, TransferMacros;

type

  { TFindRenameIdentifierDialog }

  TFindRenameIdentifierDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
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
  private
    FAllowRename: boolean;
    FIdentifierFilename: string;
    FIdentifierPosition: TPoint;
    FIsPrivate: boolean;
    procedure SetAllowRename(const AValue: boolean);
    procedure SetIsPrivate(const AValue: boolean);
    procedure UpdateRename;
  public
    procedure LoadFromConfig;
    procedure SaveToConfig;
    procedure LoadFromOptions(Options: TFindRenameIdentifierOptions);
    procedure SaveToOptions(Options: TFindRenameIdentifierOptions);
    procedure SetIdentifier(const NewIdentifierFilename: string;
                            const NewIdentifierPosition: TPoint);
    property IdentifierFilename: string read FIdentifierFilename;
    property IdentifierPosition: TPoint read FIdentifierPosition;
    property AllowRename: boolean read FAllowRename write SetAllowRename;
    property IsPrivate: boolean read FIsPrivate write SetIsPrivate;
  end;

procedure CleanUpFileList(Files: TStringList);

function ShowFindRenameIdentifierDialog(const Filename: string;
  const Position: TPoint;
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
  var TreeOfPCodeXYPosition: TAVLTree): TModalResult;
function GatherUnitReferences(Files: TStringList;
  UnitCode: TCodeBuffer; SearchInComments, IgnoreErrors, IgnoreMissingFiles: boolean;
  var TreeOfPCodeXYPosition: TAVLTree): TModalResult;
function ShowIdentifierReferences(
  DeclarationCode: TCodeBuffer; const DeclarationCaretXY: TPoint;
  TreeOfPCodeXYPosition: TAVLTree): TModalResult;
procedure AddReferencesToResultView(DeclarationCode: TCodeBuffer;
  const DeclarationCaretXY: TPoint;
  TreeOfPCodeXYPosition: TAVLTree; ClearItems: boolean; SearchPageIndex: integer);

function GatherFPDocReferencesForPascalFiles(PascalFiles: TStringList;
  DeclarationCode: TCodeBuffer; const DeclarationCaretXY: TPoint;
  var ListOfLazFPDocNode: TFPList): TModalResult;
function GatherReferencesInFPDocFile(
  const OldPackageName, OldModuleName, OldElementName: string;
  const FPDocFilename: string;
  var ListOfLazFPDocNode: TFPList): TModalResult;
  

implementation

{$R *.lfm}

procedure CleanUpFileList(Files: TStringList);
var
  i: Integer;
begin
  // sort files
  Files.Sort;
  // remove doubles
  i:=0;
  while i<=Files.Count-2 do begin
    while (i<=Files.Count-2) and (CompareFilenames(Files[i],Files[i+1])=0) do
      Files.Delete(i+1);
    inc(i);
  end;
  // remove non files
  for i:=Files.Count-1 downto 0 do
    if ExtractFilename(Files[i])='' then begin
      debugln(['Note: (lazarus) [FindRenameIdentifier.CleanUpFileList] invalid file "',Files[i],'"']);
      Files.Delete(i);
    end;
end;

function ShowFindRenameIdentifierDialog(const Filename: string;
  const Position: TPoint; AllowRename: boolean; SetRenameActive: boolean;
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
    FindRenameIdentifierDialog.Free;
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
  OldChange, Completed: Boolean;
  Graph: TUsesGraph;
  Node: TAVLTreeNode;
  UGUnit: TUGUnit;
begin
  Result:=mrCancel;
  if not LazarusIDE.BeginCodeTools then exit(mrCancel);

  StartSrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  StartSrcCode:=TCodeBuffer(StartSrcEdit.CodeToolsBuffer);
  StartTopLine:=StartSrcEdit.TopLine;

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
  CurUnitname:=ExtractFileNameOnly(DeclCode.Filename);

  //debugln('TMainIDE.DoFindRenameIdentifier A DeclarationCaretXY=x=',dbgs(DeclarationCaretXY.X),' y=',dbgs(DeclarationCaretXY.Y));

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
      Files.Add(DeclCode.Filename);

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
    Result:=GatherIdentifierReferences(Files,DeclCode,
      DeclarationCaretXY,Options.SearchInComments,PascalReferences);
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

    // ToDo: search lfm source references
    // ToDo: search i18n references
    // ToDo: designer references

    // rename identifier
    if Options.Rename then begin
      if CompareIdentifiers(PChar(Identifier),PChar(CurUnitName))=0 then
      begin
        IDEMessageDialog(srkmecRenameIdentifier,
          lisTheIdentifierIsAUnitPleaseUseTheFileSaveAsFunction,
          mtInformation,[mbCancel],'');
        exit(mrCancel);
      end;
      OldChange:=LazarusIDE.OpenEditorsOnCodeToolChange;
      LazarusIDE.OpenEditorsOnCodeToolChange:=true;
      try
        if not CodeToolBoss.RenameIdentifier(PascalReferences,
          Identifier,Options.RenameTo, DeclCode, @DeclarationCaretXY)
        then begin
          LazarusIDE.DoJumpToCodeToolBossError;
          debugln('Error: (lazarus) DoFindRenameIdentifier unable to commit');
          Result:=mrCancel;
          exit;
        end;
      finally
        LazarusIDE.OpenEditorsOnCodeToolChange:=OldChange;
      end;
      if Options.RenameShowResult then
        Result:=ShowIdentifierReferences(DeclCode,
          DeclarationCaretXY,PascalReferences);
    end;

    // show result
    Result:=mrOk;
    if (not Options.Rename) or (not SetRenameActive) then begin
      Result:=ShowIdentifierReferences(DeclCode,
        DeclarationCaretXY,PascalReferences);
      if Result<>mrOk then exit;
    end;

  finally
    Files.Free;
    OwnerList.Free;
    CodeToolBoss.FreeTreeOfPCodeXYPosition(PascalReferences);
    FreeListObjects(ListOfLazFPDocNode,true);

    // jump back in source editor
    Result:=LazarusIDE.DoOpenFileAndJumpToPos(StartSrcCode.Filename, LogCaretXY,
      StartTopLine,-1,-1,[ofOnlyIfExists,ofRegularFile,ofDoNotLoadResource]);
  end;
end;

function GatherIdentifierReferences(Files: TStringList;
  DeclarationCode: TCodeBuffer; const DeclarationCaretXY: TPoint;
  SearchInComments: boolean;
  var TreeOfPCodeXYPosition: TAVLTree): TModalResult;
var
  i: Integer;
  LoadResult: TModalResult;
  Code: TCodeBuffer;
  ListOfPCodeXYPosition: TFPList;
  Cache: TFindIdentifierReferenceCache;
begin
  Result:=mrCancel;
  ListOfPCodeXYPosition:=nil;
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
        Code, not SearchInComments, ListOfPCodeXYPosition, Cache) then
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

function GatherUnitReferences(Files: TStringList; UnitCode: TCodeBuffer;
  SearchInComments, IgnoreErrors, IgnoreMissingFiles: boolean;
  var TreeOfPCodeXYPosition: TAVLTree): TModalResult;
var
  ListOfPCodeXYPosition: TFPList;
  LoadResult: TModalResult;
  Code: TCodeBuffer;
  i: Integer;
begin
  Result:=mrCancel;
  ListOfPCodeXYPosition:=nil;
  TreeOfPCodeXYPosition:=nil;
  try
    CleanUpFileList(Files);

    Result:=mrOk;
    // search in every file
    for i:=0 to Files.Count-1 do begin
      if CompareFilenames(Files[i],UnitCode.Filename)=0 then continue;
      if IgnoreMissingFiles then
      begin
        if FilenameIsAbsolute(Files[i]) then
        begin
          if not FileExistsCached(Files[i]) then continue;
        end else begin
          Code:=CodeToolBoss.LoadFile(Files[i],false,false);
          if (Code=nil) then continue;
        end;
      end;
      LoadResult:=
          LoadCodeBuffer(Code,Files[i],[lbfCheckIfText,lbfUpdateFromDisk],true);
      if LoadResult=mrAbort then begin
        debugln('GatherUnitReferences unable to load "',Files[i],'"');
        if IgnoreErrors then
          continue;
        Result:=mrCancel;
        exit;
      end;
      if LoadResult<>mrOk then continue;

      // search references
      CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
      if not CodeToolBoss.FindUnitReferences(
        UnitCode, Code, not SearchInComments, ListOfPCodeXYPosition) then
      begin
        debugln('GatherUnitReferences unable to FindUnitReferences in "',Code.Filename,'"');
        if IgnoreErrors then
          continue;
        Result:=mrCancel;
        exit;
      end;
      //debugln('GatherUnitReferences FindUnitReferences in "',Code.Filename,'" ',dbgs(ListOfPCodeXYPosition<>nil));

      // add to tree
      if ListOfPCodeXYPosition<>nil then begin
        if TreeOfPCodeXYPosition=nil then
          TreeOfPCodeXYPosition:=CodeToolBoss.CreateTreeOfPCodeXYPosition;
        CodeToolBoss.AddListToTreeOfPCodeXYPosition(ListOfPCodeXYPosition,
                                              TreeOfPCodeXYPosition,true,false);
      end;
    end;
  finally
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
  end;
end;

function ShowIdentifierReferences(
  DeclarationCode: TCodeBuffer; const DeclarationCaretXY: TPoint;
  TreeOfPCodeXYPosition: TAVLTree): TModalResult;
var
  Identifier: string;
  OldSearchPageIndex: TTabSheet;
  SearchPageIndex: TTabSheet;
begin
  Result:=mrCancel;
  LazarusIDE.DoShowSearchResultsView(iwgfShow);
  SearchPageIndex:=nil;
  try
    // show result
    CodeToolBoss.GetIdentifierAt(DeclarationCode,
      DeclarationCaretXY.X,DeclarationCaretXY.Y,Identifier);
    // create a search result page
    //debugln(['ShowIdentifierReferences ',DbgSName(SearchResultsView)]);
    SearchPageIndex:=SearchResultsView.AddSearch(
      'Ref: '+Identifier,
      Identifier,
      '',
      ExtractFilePath(DeclarationCode.Filename),
      '*.pas;*.pp;*.p;*.inc',
      [fifWholeWord,fifSearchDirectories]);
    if SearchPageIndex = nil then exit;

    // list results
    SearchResultsView.BeginUpdate(SearchPageIndex.PageIndex);
    AddReferencesToResultView(DeclarationCode,DeclarationCaretXY,
                   TreeOfPCodeXYPosition,true,SearchPageIndex.PageIndex);
    OldSearchPageIndex:=SearchPageIndex;
    SearchPageIndex:=nil;
    SearchResultsView.EndUpdate(OldSearchPageIndex.PageIndex);
    IDEWindowCreators.ShowForm(SearchResultsView,true);
  finally
    if SearchPageIndex <> nil then
      SearchResultsView.EndUpdate(SearchPageIndex.PageIndex);
  end;
end;

procedure AddReferencesToResultView(DeclarationCode: TCodeBuffer;
  const DeclarationCaretXY: TPoint; TreeOfPCodeXYPosition: TAVLTree;
  ClearItems: boolean; SearchPageIndex: integer);
var
  Identifier: string;
  CodePos: PCodeXYPosition;
  CurLine: String;
  TrimmedLine: String;
  TrimCnt: Integer;
  ANode: TAVLTreeNode;
begin
  CodeToolBoss.GetIdentifierAt(DeclarationCode,
    DeclarationCaretXY.X,DeclarationCaretXY.Y,Identifier);

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
      SearchResultsView.AddMatch(SearchPageIndex,
                                 CodePos^.Code.Filename,
                                 Point(CodePos^.X,CodePos^.Y),
                                 Point(CodePos^.X+length(Identifier),CodePos^.Y),
                                 TrimmedLine,
                                 CodePos^.X-TrimCnt, length(Identifier));
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
  ScopeGroupBox.Caption:=lisFRISearch;
  ScopeRadioGroup.Caption:=dlgSearchScope;
  ScopeRadioGroup.Items[0]:=lisFRIinCurrentUnit;
  ScopeRadioGroup.Items[1]:=lisFRIinMainProject;
  ScopeRadioGroup.Items[2]:=lisFRIinProjectPackageOwningCurrentUnit;
  ScopeRadioGroup.Items[3]:=lisFRIinAllOpenPackagesAndProjects;

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
  UpdateRename;
end;

procedure TFindRenameIdentifierDialog.UpdateRename;
begin
  RenameCheckBox.Enabled:=AllowRename;
  NewEdit.Enabled:=RenameCheckBox.Checked and RenameCheckBox.Enabled;
  ShowResultCheckBox.Enabled:=RenameCheckBox.Checked and RenameCheckBox.Enabled;
  if NewEdit.Enabled then
    ButtonPanel1.OKButton.Caption:=lisFRIRenameAllReferences
  else
    ButtonPanel1.OKButton.Caption:=lisFRIFindReferences;
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

procedure TFindRenameIdentifierDialog.FindOrRenameButtonClick(Sender: TObject);
var
  NewIdentifier: String;
begin
  NewIdentifier:=NewEdit.Text;
  if not IsValidIdent(NewIdentifier) then begin
    IDEMessageDialog(lisFRIInvalidIdentifier,
      Format(lisSVUOisNotAValidIdentifier, [NewIdentifier]), mtError, [mbCancel]);
    ModalResult:=mrNone;
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
  if ScopeRadioGroup.Enabled then
    case ScopeRadioGroup.ItemIndex of
    0: Options.Scope:=frCurrentUnit;
    1: Options.Scope:=frProject;
    2: Options.Scope:=frOwnerProjectPackage;
    else Options.Scope:=frAllOpenProjectsAndPackages;
    end;
end;

procedure TFindRenameIdentifierDialog.SetIdentifier(
  const NewIdentifierFilename: string; const NewIdentifierPosition: TPoint);
var
  s: String;
  ACodeBuffer: TCodeBuffer;
  ListOfCodeBuffer: TFPList;
  i: Integer;
  CurCode: TCodeBuffer;
  NewIdentifier: String;
  Tool: TCodeTool;
  CodeXY: TCodeXYPosition;
  CleanPos: integer;
  Node: TCodeTreeNode;
begin
  FIdentifierFilename:=NewIdentifierFilename;
  FIdentifierPosition:=NewIdentifierPosition;
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
      NewIdentifierPosition.X,NewIdentifierPosition.Y,NewIdentifier) then
    begin
      CurrentGroupBox.Caption:=Format(lisFRIIdentifier, [NewIdentifier]);
      NewEdit.Text:=NewIdentifier;
    end;
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

end.


