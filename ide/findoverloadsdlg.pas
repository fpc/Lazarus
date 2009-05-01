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
  Classes, SysUtils, LCLProc,FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Buttons, ButtonPanel, ComCtrls, AvgLvlTree,
  // codetools
  CodeTree, CodeCache, LazIDEIntf, ProjectIntf,
  // IDE
  SrcEditorIntf, IDEProcs;

type
  TFOWNode = class
  public
    Identifier: string;
    Desc: TCodeTreeNodeDesc;
    Code: TCodeBuffer;
    Position: integer;
  end;

  { TFOWFile }

  TFOWFile = class
  private
    FCode: TCodeBuffer;
    FFilename: string;
    FScanned: boolean;
    procedure SetCode(const AValue: TCodeBuffer);
    procedure SetScanned(const AValue: boolean);
  public
    constructor Create(const TheFilename: string);
    destructor Destroy; override;
    property Filename: string read FFilename;
    property Scanned: boolean read FScanned write SetScanned;
    property Code: TCodeBuffer read FCode write SetCode;
  end;

  TFindOverloadScope = (
    fosProject,
    fosPackages,
    fosOtherSources
    );
  TFindOverloadScopes = set of TFindOverloadScope;

  TFOWStage = (
    fowsStart,
    fowsFinished
    );

  { TFindOverloadWorker }

  TFindOverloadWorker = class
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
  public
    Scopes: TFindOverloadScopes;
    CompletedScopes: TFindOverloadScopes;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Work;
    function Done: boolean;
    procedure StopSearching;
    function AddFileToScan(const Filename: string;
                           CheckExtension: boolean = true): TFOWFile;
    function FindFile(const Filename: string): TFOWFile;
    property Files: TAvgLvlTree read FFiles; // tree of TFindOverloadWorkerFile
    property ScanFiles: TAvgLvlTree read FScanFiles;// tree of TFindOverloadWorkerFile
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
    fWorker: TFindOverloadWorker;
    procedure SetIdleConnected(const AValue: boolean);
    procedure UpdateProgress;
    procedure StopWorking;
  public
    property Worker: TFindOverloadWorker read fWorker;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;

function ShowFindOverloadsDialog: TModalResult;
function ShowFindOverloadsDialog(Code: TCodeBuffer; X, Y: integer): TModalResult;

function CompareFOWFiles(File1, File2: TFOWFile): integer;
function CompareFilenameWithFOWFile(FilenameAnsiString, FOWFile: Pointer): integer;

implementation

function ShowFindOverloadsDialog: TModalResult;
var
  SrcEdit: TSourceEditorInterface;
  Code: TCodeBuffer;
  XY: TPoint;
begin
  SrcEdit:=SourceEditorWindow.ActiveEditor;
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
begin
  FindOverloadsDialog:=TFindOverloadsDialog.Create(nil);
  try
    Result:=FindOverloadsDialog.ShowModal;
  finally
    FindOverloadsDialog.Free;
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

  fWorker:=TFindOverloadWorker.Create;
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
end;

destructor TFOWFile.Destroy;
begin
  inherited Destroy;
end;

{ TFindOverloadWorker }

procedure TFindOverloadWorker.CollectProjectFiles;
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

procedure TFindOverloadWorker.CollectPackageFiles;
begin

  Include(CompletedScopes,fosPackages);
end;

procedure TFindOverloadWorker.CollectOtherSourceFiles;
begin

  Include(CompletedScopes,fosOtherSources);
end;

procedure TFindOverloadWorker.ScanSomeFiles;
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

procedure TFindOverloadWorker.ScanFile(AFile: TFOWFile);
begin
  FScanFiles.Remove(AFile);
  if AFile.Scanned then exit;
  AFile.Scanned:=true;
  DebugLn(['TFindOverloadWorker.ScanFile ',AFile.Filename]);
end;

function TFindOverloadWorker.AddFileToScan(const Filename: string;
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

function TFindOverloadWorker.FindFile(const Filename: string): TFOWFile;
var
  AVLNode: TAvgLvlTreeNode;
begin
  AVLNode:=FFiles.FindKey(Pointer(Filename),@CompareFilenameWithFOWFile);
  if AVLNode<>nil then
    Result:=TFOWFile(AVLNode.Data)
  else
    Result:=nil;
end;

constructor TFindOverloadWorker.Create;
begin
  Scopes:=[fosProject,fosPackages];
  FFiles:=TAvgLvlTree.Create(TListSortCompare(@CompareFOWFiles));
  FScanFiles:=TAvgLvlTree.Create(TListSortCompare(@CompareFOWFiles));
  FStagePosMax:=100;
end;

destructor TFindOverloadWorker.Destroy;
begin
  Clear;
  FreeAndNil(FFiles);
  FreeAndNil(FScanFiles);
  inherited Destroy;
end;

procedure TFindOverloadWorker.Clear;
begin
  FFiles.FreeAndClear;
  FScanFiles.Clear;
  FStageTitle:='Finished';
  FStagePosition:=0;
  FStagePosMax:=100;
end;

procedure TFindOverloadWorker.Work;
begin
  DebugLn(['TFindOverloadWorker.Work START']);
  if FScanFiles.Count>0 then begin
    // scan files
    ScanSomeFiles;
  end
  else if (fosProject in Scopes) and not (fosProject in CompletedScopes) then
  begin
    // collect project files
    StageTitle:='Scanning project ...';
    StagePosition:=1;
    CollectProjectFiles;
  end
  else if (fosPackages in Scopes) and not (fosPackages in CompletedScopes) then
  begin
    // collect package files
    StageTitle:='Scanning packages ...';
    StagePosition:=10;
    CollectPackageFiles;
  end
  else if (fosOtherSources in Scopes) and not (fosOtherSources in CompletedScopes)
  then begin
    // collect other sources
    StageTitle:='Scanning other sources ...';
    StagePosition:=30;
    CollectOtherSourceFiles;
  end else begin
    StageTitle:='Finished';
    StagePosition:=StagePosMax;
  end;
  DebugLn(['TFindOverloadWorker.Work END ',StageTitle,' ',StagePosition,'/',StagePosMax]);
end;

function TFindOverloadWorker.Done: boolean;
begin
  Result:=(Scopes-CompletedScopes=[]) and (FScanFiles.Count=0);
end;

procedure TFindOverloadWorker.StopSearching;
begin
  CompletedScopes:=Scopes;
  FScanFiles.Clear;
end;

initialization
  {$I findoverloadsdlg.lrs}

end.

