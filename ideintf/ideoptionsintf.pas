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
unit IDEOptionsIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type
  TAbstractIDEOptions = class(TPersistent)
  end;
  TAbstractIDEOptionsClass = class of TAbstractIDEOptions;

  TOnLoadIDEOptions = procedure(Sender: TObject; AOptions: TAbstractIDEOptions) of object;
  TOnSaveIDEOptions = procedure(Sender: TObject; AOptions: TAbstractIDEOptions) of object;

  { TAbstractIDEOptionsEditor }

  TAbstractIDEOptionsEditor = class(TFrame)
  private
    FOnLoadIDEOptions: TOnLoadIDEOptions;
    FOnSaveIDEOptions: TOnSaveIDEOptions;
  public
    function Check: Boolean; virtual;
    function GetTitle: String; virtual; abstract;
    procedure Setup; virtual; abstract;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); virtual; abstract;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); virtual; abstract;

    property OnLoadIDEOptions: TOnLoadIDEOptions read FOnLoadIDEOptions write FOnLoadIDEOptions;
    property OnSaveIDEOptions: TOnSaveIDEOptions read FOnSaveIDEOptions write FOnSaveIDEOptions;
  end;

  TAbstractIDEOptionsEditorClass = class of TAbstractIDEOptionsEditor;

  { TIDEOptionsEditorList }

  TIDEOptionsEditorList = class(TList)
  private
    function GetItem(AIndex: Integer): TAbstractIDEOptionsEditorClass;
    procedure SetItem(AIndex: Integer; const AValue: TAbstractIDEOptionsEditorClass);
  public
    property Items[AIndex: Integer]: TAbstractIDEOptionsEditorClass read GetItem write SetItem; default;
  end;

procedure RegisterIDEOptionsGroup(AGroupIndex: Integer; ATitle: String);
procedure RegisterIDEOptionsEditor(AGroupIndex: Integer; AEditor: TAbstractIDEOptionsEditorClass; AOptionsClass: TAbstractIDEOptionsClass; AIndex: Integer);

function IDEEditors: TIDEOptionsEditorList;

const
  // options groups
  GroupEnvironment = 100;
    EnvOptionsFiles   = 100;
    EnvOptionsDesktop = 200;
    EnvOptionsWindow  = 300;
    EnvOptionsFormEd  = 400;
    EnvOptionsOI      = 500;
    EnvOptionsBackup  = 600; 
    EnvOptionsNaming  = 700; 
    EnvOptionsFpDoc   = 800;

implementation
var
  FIDEEditors: TIDEOptionsEditorList;

function IDEEditors: TIDEOptionsEditorList;
begin
  if FIDEEditors = nil then
    FIDEEditors := TIDEOptionsEditorList.Create;
  Result := FIDEEditors;
end;

procedure RegisterIDEOptionsGroup(AGroupIndex: Integer; ATitle: String);
begin
  // TODO:
end;

procedure RegisterIDEOptionsEditor(AGroupIndex: Integer; AEditor: TAbstractIDEOptionsEditorClass; AOptionsClass: TAbstractIDEOptionsClass; AIndex: Integer);
begin
  // TODO:
  if IDEEditors.IndexOf(AEditor) = -1 then
    IDEEditors.Add(AEditor);
end;

{ TAbstractIDEOptionsEditor }

function TAbstractIDEOptionsEditor.Check: Boolean;
begin
  Result := True;
end;

{ TIDEOptionsEditorList }

function TIDEOptionsEditorList.GetItem(AIndex: Integer): TAbstractIDEOptionsEditorClass;
begin
  Result := TAbstractIDEOptionsEditorClass(inherited Get(AIndex));
end;

procedure TIDEOptionsEditorList.SetItem(AIndex: Integer; const AValue: TAbstractIDEOptionsEditorClass);
begin
  inherited Put(AIndex, AValue);
end;

initialization
  FIDEEditors := nil;

finalization
  FIDEEditors.Free;
  FIDEEditors := nil;
end.
