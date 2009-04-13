{               ----------------------------------------------
                     inspectdlg.pas  -  Inspect Dialog
                ----------------------------------------------

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
unit InspectDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, ComCtrls, ObjectInspector, PropEdits, Debugger, DebuggerDlg,
  LazarusIDEStrConsts, IDEWindowIntf;

type

  { TIDEInspectDlg }

  TIDEInspectDlg = class(TDebuggerDlg)
    PageControl: TPageControl;
    StatusBar1: TStatusBar;
    DataPage: TTabSheet;
    PropertiesPage: TTabSheet;
    MethodsPage: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FDataGridHook,
    FPropertiesGridHook,
    FMethodsGridHook: TPropertyEditorHook;
    FDataGrid,
    FPropertiesGrid,
    FMethodsGrid: TOIPropertyGrid;

    procedure Localize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(ADebugger: TDebugger; AData, AProperties, AMethods: TStrings);
  end; 

implementation

{ TIDEInspectDlg }

procedure TIDEInspectDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TIDEInspectDlg.Localize;
begin
  Caption := lisInspectDialog;
  DataPage.Caption := lisInspectData;
  PropertiesPage.Caption := lisInspectProperties;
  MethodsPage.Caption := lisInspectMethods;
end;

constructor TIDEInspectDlg.Create(AOwner: TComponent);

  function NewGrid(AName: String; AParent: TWinControl; AHook: TPropertyEditorHook): TOIPropertyGrid;
  begin
    Result := TOIPropertyGrid.Create(Self);
    with Result do
    begin
      Name := AName;
      Parent := AParent;
      Visible := True;
      Align := alClient;
    end;
  end;

begin
  inherited Create(AOwner);
  FDataGridHook := TPropertyEditorHook.Create;
  FDataGrid := NewGrid('DataGrid', DataPage, FDataGridHook);

  FPropertiesGridHook := TPropertyEditorHook.Create;
  FPropertiesGrid := NewGrid('PropertiesGrid', PropertiesPage, FPropertiesGridHook);

  FMethodsGridHook := TPropertyEditorHook.Create;
  FMethodsGrid := NewGrid('MethodsGrid', MethodsPage, FMethodsGridHook);

  Localize;
  IDEDialogLayoutList.ApplyLayout(Self, 260, 400);
end;

destructor TIDEInspectDlg.Destroy;
begin
  FreeAndNil(FDataGridHook);
  FreeAndNil(FPropertiesGridHook);
  FreeAndNil(FMethodsGridHook);
  inherited Destroy;
end;

procedure TIDEInspectDlg.Execute(ADebugger: TDebugger; AData, AProperties, AMethods: TStrings);
begin
  FDataGrid.BuildPropertyList;
end;

initialization
  {$I inspectdlg.lrs}

end.

