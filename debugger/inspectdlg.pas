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
  Classes, SysUtils, TypInfo, FileUtil, Forms, Controls, Graphics,
  Dialogs, ComCtrls, ObjectInspector, PropEdits, Debugger, DebuggerDlg, BaseDebugManager,
  LazarusIDEStrConsts, IDEWindowIntf, LCLProc, LCLType, Grids, StdCtrls;

type

  { TOIDBGGrid }

  TOIDBGGrid=class(TOIPropertyGrid)
  private
  protected
    procedure BuildPropertyList(OnlyIfNeeded: boolean=false);
  public
  end;

  { TIDEInspectDlg }

  TIDEInspectDlg = class(TDebuggerDlg)
    EditInspected: TEdit;
    PageControl: TPageControl;
    StatusBar1: TStatusBar;
    DataPage: TTabSheet;
    PropertiesPage: TTabSheet;
    MethodsPage: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FDataGridHook,
    FPropertiesGridHook,
    FMethodsGridHook: TPropertyEditorHook;
    FDataGrid,
    FPropertiesGrid,
    FMethodsGrid: TOIDBGGrid;
    FExpression: ansistring;
    FHumanReadable: ansistring;
    FDBGInfo: TDBGType;
    FGridData: TStringGrid;
    FGridMethods: TStringGrid;
    procedure Localize;
    procedure InspectClass;
    procedure InspectRecord;
    procedure InspectVariant;
    procedure InspectSimple;
    procedure InspectPointer;
    procedure GridDataSetup;
    procedure GridMethodsSetup;
    procedure ShowDataFields;
    procedure ShowMethodsFields;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(const AExpression: ansistring);
  end;

implementation

{$R *.lfm}

{ TIDEInspectDlg }

procedure TIDEInspectDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TIDEInspectDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TIDEInspectDlg.Localize;
begin
  Caption := lisInspectDialog;
  DataPage.Caption := lisInspectData;
  PropertiesPage.Caption := lisInspectProperties;
  MethodsPage.Caption := lisInspectMethods;
end;

procedure TIDEInspectDlg.InspectClass;
begin
  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=true;

  if not Assigned(FDBGInfo) then exit;
  if not Assigned(FDBGInfo.Fields) then exit;
  EditInspected.Text:=FExpression+' : Class '+FDBGInfo.TypeName+' inherits from '+FDBGInfo.Ancestor;
  GridDataSetup;
  ShowDataFields;
  FGridData.AutoSizeColumn(1);
  FGridData.AutoSizeColumn(2);
  GridMethodsSetup;
  ShowMethodsFields;
  FGridMethods.AutoSizeColumn(1);
  FGridMethods.AutoSizeColumn(3);
end;

procedure TIDEInspectDlg.InspectVariant;
begin
  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;
  if not Assigned(FDBGInfo) then exit;
  EditInspected.Text:=FExpression+' : Variant';
  GridDataSetup;
  FGridData.Cells[0,1]:=FExpression;
  FGridData.Cells[1,1]:='Variant';
  FGridData.Cells[2,1]:=FDBGInfo.Value.AsString;
  FGridData.AutoSizeColumn(1);
end;

procedure TIDEInspectDlg.InspectRecord;
begin
  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;

  if not Assigned(FDBGInfo) then exit;
  if not Assigned(FDBGInfo.Fields) then exit;
  EditInspected.Text:=FExpression+' : '+FDBGInfo.TypeName;
  GridDataSetup;
  ShowDataFields;
  FGridData.AutoSizeColumn(2);
end;

procedure TIDEInspectDlg.InspectSimple;
begin
  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;
  if not Assigned(FDBGInfo) then exit;
  EditInspected.Text:=FExpression+' : '+FDBGInfo.TypeName + ' = ' + FDBGInfo.Value.AsString;
  GridDataSetup;
  FGridData.Cells[0,1]:=FExpression;
  FGridData.Cells[1,1]:=FDBGInfo.TypeName;
  FGridData.Cells[2,1]:=FDBGInfo.Value.AsString;
  FGridData.AutoSizeColumn(2);
end;

procedure TIDEInspectDlg.InspectPointer;
begin
  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;
  if not Assigned(FDBGInfo) then exit;
  EditInspected.Text:=FExpression+' : '+FDBGInfo.TypeName + ' = ' + FDBGInfo.Value.AsString;
  GridDataSetup;
  FGridData.Cells[0,1]:=FExpression;
  if (FDBGInfo.TypeName <> '') and (FDBGInfo.TypeName[1] = '^')
  then FGridData.Cells[1,1]:='Pointer to '+copy(FDBGInfo.TypeName, 2, length(FDBGInfo.TypeName))
  else FGridData.Cells[1,1]:=FDBGInfo.TypeName;
  FGridData.Cells[2,1]:=format('$%x',[PtrUInt(FDBGInfo.Value.AsPointer)]);
  FGridData.AutoSizeColumn(2);
end;

procedure TIDEInspectDlg.GridDataSetup;
begin
  with FGridData do begin
    Clear;
    BorderStyle:=bsNone;
    BorderWidth:=0;
    DefaultColWidth:=100;
    Options:=[goColSizing,goDblClickAutoSize,goDrawFocusSelected,
                        goVertLine,goHorzLine,goFixedHorzLine,goSmoothScroll,
                        goTabs,goScrollKeepVisible,goRowSelect];
    Align:=alClient;
    TitleFont.Style:=[fsBold];
    ExtendedSelect:=false;
    RowCount:=2;
    FixedRows:=1;
    FixedCols:=0;
    ColCount:=3;
    Cols[0].Text:='Name';
    Cols[1].Text:='Type';
    Cols[2].Text:='Value';
    Color:=clBtnFace;
  end;
end;

procedure TIDEInspectDlg.GridMethodsSetup;
begin
  with FGridMethods do begin
    Clear;
    BorderStyle:=bsNone;
    BorderWidth:=0;
    DefaultColWidth:=100;
    Options:=[goColSizing,goDblClickAutoSize,goDrawFocusSelected,
                        goVertLine,goHorzLine,goFixedHorzLine,goSmoothScroll,
                        goTabs,goScrollKeepVisible,goRowSelect];
    Align:=alClient;
    TitleFont.Style:=[fsBold];
    ExtendedSelect:=false;
    RowCount:=2;
    FixedRows:=1;
    FixedCols:=0;
    ColCount:=4;
    Cols[0].Text:='Name';
    Cols[1].Text:='Type';
    Cols[2].Text:='Returns';
    Cols[3].Text:='Address';
    Color:=clBtnFace;
  end;
end;

procedure TIDEInspectDlg.ShowDataFields;
var
  j,k: SizeInt;
begin
  k:=0;
  for j := 0 to FDBGInfo.Fields.Count-1 do begin
    case FDBGInfo.Fields[j].DBGType.Kind of
      skSimple,skRecord,skPointer: inc(k);
    end;
  end;
  k:=k+1;
  if k<2 Then k:=2;
  FGridData.RowCount:=k;
  k:=0;
  for j := 0 to FDBGInfo.Fields.Count-1 do begin
    case FDBGInfo.Fields[j].DBGType.Kind of
      skSimple:
        begin
          inc(k);
          FGridData.Cells[0,k]:=FDBGInfo.Fields[j].Name;
          FGridData.Cells[1,k]:=FDBGInfo.Fields[j].DBGType.TypeName;
          if FDBGInfo.Fields[j].DBGType.Value.AsString='$0' then begin
            if FDBGInfo.Fields[j].DBGType.TypeName='ANSISTRING' then begin
              FGridData.Cells[2,k]:='''''';
            end else begin
              FGridData.Cells[2,k]:='nil';
            end;
          end else begin
            FGridData.Cells[2,k]:=FDBGInfo.Fields[j].DBGType.Value.AsString;
          end;
        end;
      skRecord:
        begin
          inc(k);
          FGridData.Cells[0,k]:=FDBGInfo.Fields[j].Name;
          FGridData.Cells[1,k]:='Record '+FDBGInfo.Fields[j].DBGType.TypeName;
          FGridData.Cells[2,k]:=FDBGInfo.Fields[j].DBGType.Value.AsString;
        end;
      skVariant:
        begin
          inc(k);
          FGridData.Cells[0,k]:=FDBGInfo.Fields[j].Name;
          FGridData.Cells[1,k]:='Variant';
          FGridData.Cells[2,k]:=FDBGInfo.Fields[j].DBGType.Value.AsString;
        end;
      skProcedure:
        begin
        end;
      skFunction:
        begin
        end;
       skPointer:
        begin
          inc(k);
          FGridData.Cells[0,k]:=FDBGInfo.Fields[j].Name;
          FGridData.Cells[1,k]:='Pointer '+FDBGInfo.Fields[j].DBGType.TypeName;
          FGridData.Cells[2,k]:=FDBGInfo.Fields[j].DBGType.Value.AsString;
        end;
      else
        raise Exception.Create('Inspect: Unknown type in record ->'+inttostr(ord(FDBGInfo.Fields[j].DBGType.Kind)));
    end;
  end;
end;

procedure TIDEInspectDlg.ShowMethodsFields;
var
  j,k: SizeInt;
begin
  k:=0;
  for j := 0 to FDBGInfo.Fields.Count-1 do begin
    case FDBGInfo.Fields[j].DBGType.Kind of
      skProcedure,skFunction: inc(k);
    end;
  end;
  k:=k+1;
  if k<2 Then k:=2;
  FGridMethods.RowCount:=k;
  k:=0;
  for j := 0 to FDBGInfo.Fields.Count-1 do begin
    case FDBGInfo.Fields[j].DBGType.Kind of
      skProcedure:
        begin
          inc(k);
          FGridMethods.Cells[0,k]:=FDBGInfo.Fields[j].Name;
          if ffDestructor in FDBGInfo.Fields[j].Flags then begin
            FGridMethods.Cells[1,k]:='Destructor';
          end else begin
            FGridMethods.Cells[1,k]:='Procedure';
          end;
          FGridMethods.Cells[2,k]:='';
          FGridMethods.Cells[3,k]:='???';
        end;
      skFunction:
        begin
          inc(k);
          FGridMethods.Cells[0,k]:=FDBGInfo.Fields[j].Name;
          if ffConstructor in FDBGInfo.Fields[j].Flags then begin
            FGridMethods.Cells[1,k]:='Constructor';
          end else begin
            FGridMethods.Cells[1,k]:='Function';
          end;
          if Assigned(FDBGInfo.Fields[j].DBGType.Result) then begin
            FGridMethods.Cells[2,k]:=FDBGInfo.Fields[j].DBGType.Result.TypeName;
          end else begin
            FGridMethods.Cells[2,k]:='';
          end;
          FGridMethods.Cells[3,k]:='???';
        end;
    end;
  end;
end;

constructor TIDEInspectDlg.Create(AOwner: TComponent);

  function NewGrid(AName: String; AParent: TWinControl; AHook: TPropertyEditorHook): TOIDBGGrid;
  begin
    Result := TOIDBGGrid.Create(Self);
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

  FGridData:=TStringGrid.Create(DataPage);
  DataPage.InsertControl(FGridData);
  GridDataSetup;
  FGridMethods:=TStringGrid.Create(MethodsPage);
  MethodsPage.InsertControl(FGridMethods);
  GridMethodsSetup;
end;

destructor TIDEInspectDlg.Destroy;
begin
  FreeAndNil(FDBGInfo);
  FreeAndNil(FDataGridHook);
  FreeAndNil(FPropertiesGridHook);
  FreeAndNil(FMethodsGridHook);
  inherited Destroy;
end;

procedure TIDEInspectDlg.Execute(const AExpression: ansistring);
begin
  FExpression:='';
  FreeAndNil(FDBGInfo);
  if not DebugBoss.Evaluate(AExpression,FHumanReadable,FDBGInfo) or not assigned(FDBGInfo) then
  begin
    FreeAndNil(FDBGInfo);
    Exit;
  end;
  FExpression:=AExpression;
  case FDBGInfo.Kind of
    skClass: InspectClass();
    skRecord: InspectRecord();
    skVariant: InspectVariant();
  //  skEnum: ;
  //  skSet: ;
  //  skProcedure: ;
  //  skFunction: ;
    skSimple: InspectSimple();
    skPointer: InspectPointer();
  //  skDecomposable: ;
  end;
end;

{ TOIDBGGrid }

procedure TOIDBGGrid.BuildPropertyList(OnlyIfNeeded: boolean);
begin

end;

end.

