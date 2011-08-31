{
 /***************************************************************************
                               TASeriesEditor.pas
                               ----------------
              Component Library Standard Graph Design-time Editors


 ***************************************************************************/

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

Author: Alexander Klenin

}
unit TASeriesEditor;

{$H+}

interface

  procedure Register;

resourcestring
  sesSeriesEditorTitle = 'Edit series';

implementation

uses
  Classes, ComponentEditors, Forms, Menus, PropEdits, SysUtils,
  TAGraph, TASubcomponentsEditor;

type
  { TSeriesComponentEditor }

  TSeriesComponentEditor = class(TSubComponentListEditor)
  protected
    function MakeEditorForm: TForm; override;
  public
    function GetVerb(Index: Integer): string; override;
  end;

  { TSeriesPropertyEditor }

  TSeriesPropertyEditor = class(TComponentListPropertyEditor)
  protected
    function GetChildrenCount: Integer; override;
    function MakeEditorForm: TForm; override;
  end;

  { TSeriesEditorForm }

  TSeriesEditorForm = class(TComponentListEditorForm)
  protected
    procedure AddSubcomponent(AParent, AChild: TComponent); override;
    procedure BuildCaption; override;
    function ChildClass: TComponentClass; override;
    procedure EnumerateSubcomponentClasses; override;
    function GetChildrenList: TFPList; override;
    function MakeSubcomponent(
      AOwner: TComponent; ATag: Integer): TComponent; override;
  end;

procedure Register;
begin
  RegisterPropertyEditor(
    TypeInfo(TChartSeriesList), TChart, 'Series', TSeriesPropertyEditor);
  RegisterComponentEditor(TChart, TSeriesComponentEditor);
end;

{ TSeriesComponentEditor }

function TSeriesComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := sesSeriesEditorTitle;
    else Result := '';
  end;
end;

function TSeriesComponentEditor.MakeEditorForm: TForm;
begin
  Result := TSeriesEditorForm.Create(Application, GetComponent, Self, nil);
end;

{ TSeriesPropertyEditor }

function TSeriesPropertyEditor.GetChildrenCount: Integer;
begin
  Result := (GetObjectValue as TChartSeriesList).Count;
end;

function TSeriesPropertyEditor.MakeEditorForm: TForm;
begin
  with TSeriesEditorForm do
    Result := Create(Application, GetComponent(0) as TComponent, nil, Self);
end;

{ TSeriesEditorForm }

procedure TSeriesEditorForm.AddSubcomponent(AParent, AChild: TComponent);
begin
  (AParent as TChart).AddSeries(AChild as TBasicChartSeries);
end;

procedure TSeriesEditorForm.BuildCaption;
begin
  Caption := sesSeriesEditorTitle + ' - ' + Parent.Name;
end;

function TSeriesEditorForm.ChildClass: TComponentClass;
begin
  Result := TBasicChartSeries;
end;

procedure TSeriesEditorForm.EnumerateSubcomponentClasses;
var
  i: Integer;
begin
  for i := 0 to SeriesClassRegistry.Count - 1 do
    AddSubcomponentClass(SeriesClassRegistry[i], i);
end;

function TSeriesEditorForm.GetChildrenList: TFPList;
begin
  Result := (Parent as TChart).Series.List;
end;

function TSeriesEditorForm.MakeSubcomponent(
  AOwner: TComponent; ATag: Integer): TComponent;
begin
  Result := TSeriesClass(SeriesClassRegistry.Objects[ATag]).Create(AOwner);
end;

end.

