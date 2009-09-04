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
}
unit BuildModesEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Grids,
  CompilerOptions;

type

  { TBuildModeGridRow }

  TBuildModeGridRow = class
  private
    FFlag: TBuildModeFlag;
    FMode: TBuildMode;
  public
    constructor Create(aMode: TBuildMode; aFlag: TBuildModeFlag);
    destructor Destroy; override;
    property Mode: TBuildMode read FMode;
    property Flag: TBuildModeFlag read FFlag;
  end;

  { TBuildModesGrid }

  TBuildModesGrid = class(TStringGrid)
  private
    FGraph: TBuildModeGraph;
    FModeRows: array of TBuildModeGridRow;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Graph: TBuildModeGraph read FGraph;
    procedure RebuildGrid; // call this after Graph changed
  end;

  TBuildModesEditorFrame = class(TFrame)
  private
  public
  end;

implementation

{ TBuildModesGrid }

constructor TBuildModesGrid.Create(TheOwner: TComponent);
begin
  fGraph:=TBuildModeGraph.Create;
  inherited Create(TheOwner);
end;

destructor TBuildModesGrid.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FGraph);
end;

procedure TBuildModesGrid.RebuildGrid;
var
  i: Integer;
  Cnt: Integer;
begin
  for i:=0 to Length(FModeRows)-1 do FModeRows[i].Free;
  Cnt:=0;
  for i:=0 to FGraph.ModeCount-1 do
    ;
  SetLength(FModeRows,Cnt);
end;

{ TBuildModeGridRow }

constructor TBuildModeGridRow.Create(aMode: TBuildMode; aFlag: TBuildModeFlag);
begin
  FMode:=aMode;
  FFlag:=aFlag;
end;

destructor TBuildModeGridRow.Destroy;
begin
  inherited Destroy;
end;

initialization
  {$I buildmodeseditor.lrs}

end.

