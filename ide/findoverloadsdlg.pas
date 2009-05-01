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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ButtonPanel, ComCtrls, AvgLvlTree,
  // codetools
  CodeTree, CodeCache,
  // IDE
  SrcEditorIntf
  ;

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
    fosFPC
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
  public
    Scopes: TFindOverloadScopes;
    CompletedScopes: TFindOverloadScopes;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Work;
    function Done: boolean;
    property Files: TAvgLvlTree read FFiles; // tree of TFindOverloadWorkerFile
    property ScanFiles: TAvgLvlTree read FScanFiles;// tree of TFindOverloadWorkerFile
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
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

function ShowFindOverloadsDialog: TModalResult;
function ShowFindOverloadsDialog(Code: TCodeBuffer; X, Y: integer): TModalResult;

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

{ TFindOverloadsDialog }

procedure TFindOverloadsDialog.FormCreate(Sender: TObject);
begin
  Caption:='Find overloads';
  CurGroupBox.Caption:='Current identifier';
  ResultsGroupBox.Caption:='Overloads';
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

constructor TFindOverloadWorker.Create;
begin

end;

destructor TFindOverloadWorker.Destroy;
begin
  inherited Destroy;
end;

procedure TFindOverloadWorker.Clear;
begin

end;

procedure TFindOverloadWorker.Work;
begin

end;

function TFindOverloadWorker.Done: boolean;
begin
  Result:=true;
end;

initialization
  {$I findoverloadsdlg.lrs}

end.

