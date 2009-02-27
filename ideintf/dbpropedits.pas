{ Copyright (C) 2004

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Lagunov Aleksey

  Abstract:
    Property Editors for Database components of FCL and LCL.
}
unit DBPropEdits;

{$mode objfpc}{$H+}

interface

uses
  Classes, ObjInspStrConsts, PropEdits, Componenteditors, TypInfo, DB, SysUtils,
  DbCtrls, DBGrids;

type
  TFieldProperty = class(TStringPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure FillValues(const Values: TStringList); virtual;
  end;

  { TLookupFieldProperty }

  TLookupFieldProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

  TDBGridFieldProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

  { TDBGridComponentEditor }

  TDBGridComponentEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

{ TFieldProperty }

function TFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList, paSortList, paMultiSelect];
end;

procedure TFieldProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    FillValues(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

procedure TFieldProperty.FillValues(const Values: TStringList);
var
  DataSource: TDataSource;
begin
  DataSource := GetObjectProp(GetComponent(0), 'DataSource') as TDataSource;
  if (DataSource is TDataSource) and Assigned(DataSource.DataSet) then
    DataSource.DataSet.GetFieldNames(Values);
end;

{ TDBGridFieldProperty }

procedure TDBGridFieldProperty.FillValues(const Values: TStringList);
var
  Column: TColumn;
  Grid: TdbGrid;
  DataSource: TDataSource;
begin
  Column:=TColumn(GetComponent(0));
  if not (Column is TColumn) then exit;
  Grid:=TdbGrid(Column.Grid);
  if not (Grid is TdbGrid) then exit;
  DataSource := Grid.DataSource;
  if Assigned(DataSource) and Assigned(DataSource.DataSet) then
    DataSource.DataSet.GetFieldNames(Values);
end;

{ TDBGridComponentEditor }

function TDBGridComponentEditor.GetVerbCount: Integer;
begin
  Result:= 1;
end;

function TDBGridComponentEditor.GetVerb(Index: Integer): string;
begin
  Result:= sccsLvColEdt;
end;

procedure TDBGridComponentEditor.ExecuteVerb(Index: Integer);
var
  Hook: TPropertyEditorHook;
  DBGrid: TDBGrid;
begin
  DBGrid := GetComponent as TDBGrid;
  GetHook(Hook);
  EditCollection(DBGrid, DBGrid.Columns, 'Columns');
  if Assigned(Hook) then Hook.Modified(Self);
end;

{ TLookupFieldProperty }

procedure TLookupFieldProperty.FillValues(const Values: TStringList);
var
  DataSource: TDataSource;
begin
  DataSource := GetObjectProp(GetComponent(0), 'ListSource') as TDataSource;
  if (DataSource is TDataSource) and Assigned(DataSource.DataSet) then
    DataSource.DataSet.GetFieldNames(Values);
end;

initialization
  RegisterPropertyEditor(TypeInfo(string), TComponent, 'DataField', TFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBLookupListBox, 'KeyField', TLookupFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBLookupListBox, 'ListField', TLookupFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBLookupComboBox, 'KeyField', TLookupFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBLookupComboBox, 'ListField', TLookupFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TColumn, 'FieldName', TDBGridFieldProperty);
  RegisterComponentEditor(TDBGrid,TDBGridComponentEditor);

end.

