{ Copyright (C) 2004

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  Classes, PropEdits, Componenteditors, TypInfo;

type
  TFieldProperty = class(TStringPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure FillValues(const Values: TStringList); virtual;
  end;

  TDBGridFieldProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

implementation

uses DB, SysUtils, DBGrids;

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

initialization
  RegisterPropertyEditor(TypeInfo(string), TComponent, 'DataField', TFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TColumn, 'FieldName', TDBGridFieldProperty);

end.

