{

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

Authors: Alexander Klenin

}

unit TAToolEditors;

{$H+}

interface

procedure Register;

resourcestring
  tasToolsEditorTitle = 'Edit tools';

implementation

uses
  Classes, ComponentEditors, Forms, PropEdits, SysUtils,
  TATools, TASubcomponentsEditor;

type
  { TToolsComponentEditor }

  TToolsComponentEditor = class(TSubComponentListEditor)
  protected
    function MakeEditorForm: TForm; override;
  public
    function GetVerb(Index: Integer): string; override;
  end;

  { TToolsPropertyEditor }

  TToolsPropertyEditor = class(TComponentListPropertyEditor)
  protected
    function GetChildrenCount: Integer; override;
    function MakeEditorForm: TForm; override;
  end;

  { TToolsEditorForm }

  TToolsEditorForm = class(TComponentListEditorForm)
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
    TypeInfo(TChartTools), TChartToolset, 'Tools', TToolsPropertyEditor);
  RegisterComponentEditor(TChartToolset, TToolsComponentEditor);
  RegisterPropertyEditor(
    TypeInfo(Boolean), TZoomDragTool, 'Proportional', THiddenPropertyEditor);
end;

{ TToolsComponentEditor }

function TToolsComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := tasToolsEditorTitle
  else
    Result := '';
end;

function TToolsComponentEditor.MakeEditorForm: TForm;
begin
  Result := TToolsEditorForm.Create(Application, GetComponent, Self, nil);
end;

{ TToolsPropertyEditor }

function TToolsPropertyEditor.GetChildrenCount: Integer;
begin
  Result := (GetObjectValue as TChartTools).Count;
end;

function TToolsPropertyEditor.MakeEditorForm: TForm;
begin
  with TToolsEditorForm do
    Result := Create(Application, GetComponent(0) as TComponent, nil, Self);
end;

{ TToolsEditorForm }

procedure TToolsEditorForm.AddSubcomponent(AParent, AChild: TComponent);
begin
  (AChild as TChartTool).Toolset := (AParent as TChartToolset);
end;

procedure TToolsEditorForm.BuildCaption;
begin
  Caption := tasToolsEditorTitle + ' - ' + Parent.Name;
end;

function TToolsEditorForm.ChildClass: TComponentClass;
begin
  Result := TChartTool;
end;

procedure TToolsEditorForm.EnumerateSubcomponentClasses;
var
  i: Integer;
begin
  for i := 0 to ToolsClassRegistry.Count - 1 do
    AddSubcomponentClass(ToolsClassRegistry[i], i);
end;

function TToolsEditorForm.GetChildrenList: TFPList;
begin
  Result := (Parent as TChartToolset).Tools;
end;

function TToolsEditorForm.MakeSubcomponent(
  AOwner: TComponent; ATag: Integer): TComponent;
begin
  Result := TChartToolClass(ToolsClassRegistry.Objects[ATag]).Create(AOwner);
end;

end.

