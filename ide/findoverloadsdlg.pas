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

  Author: Mattias Gaertner

  Abstract:
    Find all alternative declarations of an identifier.
}
unit FindOverloadsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc,FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Buttons, ButtonPanel, ComCtrls, AvgLvlTree,
  // codetools
  FindDeclarationTool, PascalParserTool, CodeTree, CodeCache, CodeAtom,
  CodeToolManager, CodeGraph, FindOverloads,
  // IDE
  LazIDEIntf, ProjectIntf, SrcEditorIntf, IDEProcs;

type

  { TFOWFile }

  TFOWFile = class
  private
    FCode: TCodeBuffer;
    FFilename: string;
    FOnlyInterface: boolean;
    FScanned: boolean;
    procedure SetCode(const AValue: TCodeBuffer);
    procedure SetScanned(const AValue: boolean);
  public
    constructor Create(const TheFilename: string);
    destructor Destroy; override;
    property Filename: string read FFilename;
    property OnlyInterface: boolean read FOnlyInterface write FOnlyInterface;
    property Scanned: boolean read FScanned write SetScanned;
    property Code: TCodeBuffer read FCode write SetCode;
  end;

  TFindOverloadsScope = (
    fosProject,
    fosPackages,
    fosOtherSources
    );
  TFindOverloadsScopes = set of TFindOverloadsScope;

  TFOWStage = (
    fowsStart,
    fowsFinished
    );

  { TFindOverloadsWorker }

  TFindOverloadsWorker = class
  private
    FFiles: TAvgLvlTree;
    FScanFiles: TAvgLvlTree;
    FStagePosition: integer;
    FStagePosMax: integer;
    FStageTitle: string;
    procedure CollectProjectFiles;
    procedure CollectPackageFiles;
    procedure CollectOtherSourceFiles;
    procedure ScanSomeFiles;
    procedure ScanFile(AFile: TFOWFile);
    procedure CollectStartSource;
  public
    StartSourceScanned: boolean;
    Scopes: TFindOverloadsScopes;
    CompletedScopes: TFindOverloadsScopes;
    Graph: TDeclarationOverloadsGraph;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Work;
    function Done: boolean;
    procedure StopSearching;
    function AddFileToScan(const Filename: string;
                           CheckExtension: boolean = true): TFOWFile;
    function FindFile(const Filename: string): TFOWFile;
    property Files: TAvgLvlTree read FFiles; // tree of TFindOverloadsWorkerFile
    property ScanFiles: TAvgLvlTree read FScanFiles;// tree of TFindOverloadsWorkerFile
    property StageTitle: string read FStageTitle write FStageTitle;
    property StagePosition: integer read FStagePosition write FStagePosition;
    property StagePosMax: integer read FStagePosMax write FStagePosMax;
  end;

  { TFindOverloadsDialog }

  TFindOverloadsDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    SearchAllCheckBox: TCheckBox;
    CurGroupBox: TGroupBox;
    ResultsProgressBar: TProgressBar;
    ResultsGroupBox: TGroupBox;
    CurTreeView: TTreeView;
    ResultsTreeView: TTreeView;
    procedure ButtonPanel1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  private
    FIdleConnected: boolean;
    fCurTreeViewComplete: boolean;
    fWorker: TFindOverloadsWorker;
    procedure SetIdleConnected(const AValue: boolean);
    procedure UpdateProgress;
    procedure StopWorking;
    procedure UpdateCurTreeView;
  public
    property Worker: TFindOverloadsWorker read fWorker;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;

function ShowFindOverloadsDialog: TModalResult;
function ShowFindOverloadsDialog(Code: TCodeBuffer; X, Y: integer): TModalResult;

function CompareFOWFiles(File1, File2: TFOWFile): integer;
function CompareFilenameWithFOWFile(FilenameAnsiString, FOWFile: Pointer): integer;

implementation

{$R *.lfm}

function ShowFindOverloadsDialog: TModalResult;
var
  SrcEdit: TSourceEditorInterface;
  Code: TCodeBuffer;
  XY: TPoint;
begin
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then
    exit(mrCancel);
  Code:=TCodeBuffer(SrcEdit.CodeToolsBuffer);
  XY:=SrcEdit.CursorTextXY;
  Result:=ShowFindOverloadsDialog(Code,XY.X,XY.Y);
end;

function ShowFindOverloadsDialog(Code: TCodeBuffer; X, Y: integer
  ): TModalResult;
var
  FindOverloadsDialog: TFindOverloadsDialog;
  Graph: TDeclarationOverloadsGraph;
begin
  if not LazarusIDE.BeginCodeTools then exit;
  Graph:=nil;
  FindOverloadsDialog:=nil;
  CodeToolBoss.ActivateWriteLock;
  try
    if not CodeToolBoss.GatherOverloads(Code,X,Y,Graph) then begin
      LazarusIDE.DoJumpToCodeToolBossError;
      exit(mrCancel);
    end;
    //DebugLn(['ShowFindOverloadsDialog ',Graph.StartCode.Filename,' ',Graph.StartX,',',Graph.StartY]);
    FindOverloadsDialog:=TFindOverloadsDialog.Create(nil);
    FindOverloadsDialog.Worker.Graph:=Graph;
    Result:=FindOverloadsDialog.ShowModal;
  finally
    CodeToolBoss.DeactivateWriteLock;
    FindOverloadsDialog.Free;
    Graph.Free;
  end;
end;

function CompareFOWFiles(File1, File2: TFOWFile): integer;
begin
  Result:=CompareFilenames(File1.Filename,File2.Filename);
end;

function CompareFilenameWithFOWFile(FilenameAnsiString, FOWFile: Pointer
  ): integer;
begin
  Result:=CompareFilenames(ansistring(FilenameAnsiString),TFOWFile(FOWFile).Filename);
end;

{ TFindOverloadsDialog }

procedure TFindOverloadsDialog.FormCreate(Sender: TObject);
begin
  Caption:='Find overloads';
  CurGroupBox.Caption:='Current identifier';
  ResultsGroupBox.Caption:='Overloads';
  SearchAllCheckBox.Caption:='Search in other sources too';
  SearchAllCheckBox.ShowHint:=true;
  SearchAllCheckBox.Hint:='Enable this to search in system sources too. For example the RTL and FCL sources. This can take some minutes on slow machines.';

  ButtonPanel1.CancelButton.OnClick:=@ButtonPanel1Click;

  fWorker:=TFindOverloadsWorker.Create;
  IdleConnected:=true;
  UpdateProgress;
end;

procedure TFindOverloadsDialog.ButtonPanel1Click(Sender: TObject);
begin
  StopWorking;
end;

procedure TFindOverloadsDialog.FormDestroy(Sender: TObject);
begin
  IdleConnected:=false;
  FreeAndNil(fWorker);
end;

procedure TFindOverloadsDialog.OnIdle(Sender: TObject; var Done: Boolean);
begin
  fWorker.Work;
  Done:=fWorker.Done;
  if Done then
    IdleConnected:=false;
  UpdateProgress;
  if not fCurTreeViewComplete then
    UpdateCurTreeView;
end;

procedure TFindOverloadsDialog.SetIdleConnected(const AValue: boolean);
begin
  if FIdleConnected=AValue then exit;
  FIdleConnected:=AValue;
  if FIdleConnected then begin
    ButtonPanel1.CancelButton.Enabled:=true;
    ButtonPanel1.CloseButton.Enabled:=false;
    Application.AddOnIdleHandler(@OnIdle)
  end else begin
    ButtonPanel1.CancelButton.Enabled:=false;
    ButtonPanel1.CloseButton.Enabled:=true;
    Application.RemoveOnIdleHandler(@OnIdle);
  end;
end;

procedure TFindOverloadsDialog.UpdateProgress;
begin
  if Worker.Done then
    ResultsProgressBar.Visible:=false
  else begin
    ResultsProgressBar.Max:=Worker.StagePosMax;
    ResultsProgressBar.Position:=Worker.StagePosition;
    ResultsProgressBar.Visible:=true;
  end;
end;

procedure TFindOverloadsDialog.StopWorking;
begin
  IdleConnected:=false;
  Worker.StopSearching;
end;

procedure TFindOverloadsDialog.UpdateCurTreeView;
var
  s: String;
  Node: TCodeTreeNode;
  Tool: TFindDeclarationTool;
  ParentNode: TCodeTreeNode;
begin
  fCurTreeViewComplete:=true;
  CurTreeView.BeginUpdate;
  CurTreeView.Items.Clear;
  Node:=Worker.Graph.StartCodeNode;
  Tool:=Worker.Graph.StartTool;
  if Node<>nil then begin
    DebugLn(['TFindOverloadsDialog.UpdateCurTreeView ',Node.DescAsString,' ',dbgstr(copy(Tool.Src,Node.StartPos,20))]);
    // unit name
    s:=Tool.GetSourceName(false)+': ';
    // keyword
    case Node.Desc of
    ctnEnumIdentifier: s:=s+'enum';
    ctnVarDefinition: s:=s+'var';
    ctnConstDefinition: s:=s+'const';
    ctnTypeDefinition: s:=s+'type';
    ctnGenericType: s:=s+'generic';
    ctnProperty: s:=s+'property';
    ctnProcedure: s:=s+'procedure';
    ctnUseUnit: s:=s+'uses';
    ctnUnit: s:=s+'unit';
    ctnProgram: s:=s+'program';
    ctnPackage: s:=s+'package';
    ctnLibrary: s:=s+'library';
    end;
    s:=s+' ';
    // context
    if Node.Desc<>ctnEnumIdentifier then
    begin
      ParentNode:=Node.Parent;
      while ParentNode<>nil do begin
        case ParentNode.Desc of
        ctnTypeDefinition,ctnGenericType:
          s:=s+Tool.ExtractDefinitionName(Node)+'.';
        end;
        ParentNode:=ParentNode.Parent;
      end;
    end;
    // name
    case Node.Desc of
    ctnEnumIdentifier, ctnTypeDefinition, ctnConstDefinition, ctnVarDefinition,
    ctnGenericType:
      s:=s+Tool.ExtractDefinitionName(Node);
    ctnProperty:
      s:=s+Tool.ExtractPropName(Node,false);
    ctnProcedure:
      s:=s+Tool.ExtractProcName(Node,[phpWithoutClassName,phpCommentsToSpace]);
    ctnUseUnit:
      s:=s+Tool.ExtractNode(Node,[phpCommentsToSpace]);
    ctnUnit,ctnProgram,ctnPackage,ctnLibrary:
      s:=s+Tool.GetSourceName(false);
    end;
    // add node
    CurTreeView.Items.Add(nil,s);
  end;
  CurTreeView.EndUpdate;
end;

{ TFOWFile }

procedure TFOWFile.SetCode(const AValue: TCodeBuffer);
begin
  if FCode=AValue then exit;
  FCode:=AValue;
end;

procedure TFOWFile.SetScanned(const AValue: boolean);
begin
  if FScanned=AValue then exit;
  FScanned:=AValue;
end;

constructor TFOWFile.Create(const TheFilename: string);
begin
  FFilename:=TheFilename;
  FOnlyInterface:=true;
end;

destructor TFOWFile.Destroy;
begin
  inherited Destroy;
end;

{ TFindOverloadsWorker }

procedure TFindOverloadsWorker.CollectProjectFiles;
var
  AProject: TLazProject;
  i: Integer;
  ProjFile: TLazProjectFile;
begin
  AProject:=LazarusIDE.ActiveProject;
  if AProject<>nil then begin
    for i:=0 to AProject.FileCount-1 do begin
      ProjFile:=AProject.Files[i];
      if ProjFile.IsPartOfProject then
        AddFileToScan(ProjFile.Filename);
    end;
  end;
  Include(CompletedScopes,fosProject);
end;

procedure TFindOverloadsWorker.CollectPackageFiles;
begin

  Include(CompletedScopes,fosPackages);
end;

procedure TFindOverloadsWorker.CollectOtherSourceFiles;
begin

  Include(CompletedScopes,fosOtherSources);
end;

procedure TFindOverloadsWorker.ScanSomeFiles;
const
  MaxScanTime = 0.3/86400; // 0.3 seconds
var
  StartTime: TDateTime;
  CurFile: TFOWFile;
begin
  StartTime:=Now;
  while FScanFiles.Count>0 do begin
    CurFile:=TFOWFile(FScanFiles.FindLowest.Data);
    ScanFile(CurFile);
    if Now-StartTime>=MaxScanTime then
      break;
  end;
end;

procedure TFindOverloadsWorker.ScanFile(AFile: TFOWFile);
var
  Tool: TCodeTool;
  Filename: String;
  MainFile: TFOWFile;
  Code: TCodeBuffer;
begin
  FScanFiles.Remove(AFile);
  if AFile.Scanned then exit;
  AFile.Scanned:=true;
  //DebugLn(['TFindOverloadsWorker.ScanFile File=',AFile.Filename]);
  // get codetool
  Filename:=TrimFilename(AFile.Filename);
  Code:=CodeToolBoss.LoadFile(Filename,true,false);
  if Code=nil then begin
    DebugLn(['TFindOverloadsWorker.ScanFile file not readable: ',Filename]);
    exit;
  end;
  Tool:=TCodeTool(CodeToolBoss.GetCodeToolForSource(Code,true,false));
  if Tool=nil then begin
    DebugLn(['TFindOverloadsWorker.ScanFile file not in a unit: ',Filename]);
    exit;
  end;
  // check if AFile is an include file
  Filename:=Tool.MainFilename;
  MainFile:=FindFile(Filename);
  // get unit
  if MainFile=nil then begin
    MainFile:=TFOWFile.Create(Filename);
    FFiles.Add(MainFile);
  end;
  if (MainFile<>AFile) and MainFile.Scanned then begin
    //DebugLn(['TFindOverloadsWorker.ScanFile already scanned: ',Filename]);
    exit;
  end;
  // scan unit
  FScanFiles.Remove(MainFile);
  MainFile.Scanned:=true;
  if not AFile.OnlyInterface then
    MainFile.OnlyInterface:=false;
  //DebugLn(['TFindOverloadsWorker.ScanFile scanning: ',Tool.MainFilename]);
  Graph.ScanToolForIdentifier(Tool,MainFile.OnlyInterface);
end;

procedure TFindOverloadsWorker.CollectStartSource;
var
  Filename: String;
  aFile: TFOWFile;
begin
  Filename:=Graph.StartCode.Filename;
  aFile:=FindFile(Filename);
  if aFile=nil then begin
    aFile:=TFOWFile.Create(Filename);
    aFile.OnlyInterface:=false;
    FFiles.Add(aFile);
    FScanFiles.Add(aFile);
  end;
end;

function TFindOverloadsWorker.AddFileToScan(const Filename: string;
  CheckExtension: boolean): TFOWFile;
begin
  if CheckExtension and (not FilenameIsPascalSource(Filename)) then
    exit;
  Result:=FindFile(Filename);
  if Result<>nil then exit;
  Result:=TFOWFile.Create(Filename);
  FFiles.Add(Result);
  FScanFiles.Add(Result);
end;

function TFindOverloadsWorker.FindFile(const Filename: string): TFOWFile;
var
  AVLNode: TAvgLvlTreeNode;
begin
  AVLNode:=FFiles.FindKey(Pointer(Filename),@CompareFilenameWithFOWFile);
  if AVLNode<>nil then
    Result:=TFOWFile(AVLNode.Data)
  else
    Result:=nil;
end;

constructor TFindOverloadsWorker.Create;
begin
  Scopes:=[fosProject,fosPackages];
  FFiles:=TAvgLvlTree.Create(TListSortCompare(@CompareFOWFiles));
  FScanFiles:=TAvgLvlTree.Create(TListSortCompare(@CompareFOWFiles));
  FStagePosMax:=100;
end;

destructor TFindOverloadsWorker.Destroy;
begin
  Clear;
  FreeAndNil(FFiles);
  FreeAndNil(FScanFiles);
  inherited Destroy;
end;

procedure TFindOverloadsWorker.Clear;
begin
  FFiles.FreeAndClear;
  FScanFiles.Clear;
  FStageTitle:='Finished';
  FStagePosition:=0;
  FStagePosMax:=100;
  StartSourceScanned:=false;
end;

procedure TFindOverloadsWorker.Work;
begin
  DebugLn(['TFindOverloadsWorker.Work START']);
  if FScanFiles.Count>0 then begin
    // scan files
    ScanSomeFiles;
  end
  else if not StartSourceScanned then
  begin
    StageTitle:='Scanning start source ...';
    StagePosition:=10;
    StartSourceScanned:=true;
    CollectStartSource;
  end
  else if (fosProject in Scopes) and not (fosProject in CompletedScopes) then
  begin
    // collect project files
    StageTitle:='Scanning project ...';
    StagePosition:=20;
    CollectProjectFiles;
  end
  else if (fosPackages in Scopes) and not (fosPackages in CompletedScopes) then
  begin
    // collect package files
    StageTitle:='Scanning packages ...';
    StagePosition:=40;
    CollectPackageFiles;
  end
  else if (fosOtherSources in Scopes) and not (fosOtherSources in CompletedScopes)
  then begin
    // collect other sources
    StageTitle:='Scanning other sources ...';
    StagePosition:=60;
    CollectOtherSourceFiles;
  end else begin
    StageTitle:='Finished';
    StagePosition:=StagePosMax;
    Graph.ComputeShortestPaths;
  end;
  DebugLn(['TFindOverloadsWorker.Work END ',StageTitle,' ',StagePosition,'/',StagePosMax]);
end;

function TFindOverloadsWorker.Done: boolean;
begin
  Result:=(Scopes-CompletedScopes=[]) and (FScanFiles.Count=0);
end;

procedure TFindOverloadsWorker.StopSearching;
begin
  CompletedScopes:=Scopes;
  FScanFiles.Clear;
  Graph.ComputeShortestPaths;
end;

end.

