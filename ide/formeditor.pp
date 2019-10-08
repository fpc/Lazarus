{
 /***************************************************************************
                               FormEditor.pp
                             -------------------

 ***************************************************************************/

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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit FormEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLProc, Controls, Forms, Buttons, SysUtils, Graphics,
  // register IDE base packages
  LazarusPackageIntf, RegisterLCL, PkgRegisterBase, allsynedit, LazControls,
  // IDE
  ObjectInspector, Designer, IDECommands, FormEditingIntf, CustomFormEditor;

type

  { TFormEditor }

  TFormEditor = class(TCustomFormEditor)
  protected
    procedure SetObj_Inspector(AnObjectInspector: TObjectInspectorDlg); override;
  public
    procedure PaintAllDesignerItems;
    procedure CheckDesignerPositions;
    function GetDesignerMediatorByComponent(AComponent: TComponent
            ): TDesignerMediator; override;

    function ComponentUsesRTTIForMethods(Component: TComponent): boolean;
  end;

var
  FormEditor1: TFormEditor = nil;
  
procedure CreateFormEditor;
procedure FreeFormEditor;


implementation

procedure CreateFormEditor;
begin
  if FormEditor1=nil then begin
    FormEditor1 := TFormEditor.Create;
    BaseFormEditor1 := FormEditor1;
    FormEditingHook := FormEditor1;
    IDECmdScopeDesignerOnly.AddWindowClass(TDesignerIDECommandForm);
  end;
end;

procedure FreeFormEditor;
begin
  if FormEditor1=nil then exit;
  DebugLn(['FreeFormEditor: FormEditor1=', FormEditor1]);
  FormEditingHook:=nil;
  FormEditor1.Free;
  FormEditor1:=nil;
  BaseFormEditor1 := nil;
end;

procedure TFormEditor.SetObj_Inspector(AnObjectInspector: TObjectInspectorDlg);
begin
  if AnObjectInspector=Obj_Inspector then exit;
  inherited SetObj_Inspector(AnObjectInspector);
end;

procedure TFormEditor.PaintAllDesignerItems;
var
  i: Integer;
  ADesigner: TDesigner;
  AForm: TCustomForm;
begin
  for i:=0 to JITFormList.Count-1 do begin
    ADesigner:=TDesigner(JITFormList[i].Designer);
    if ADesigner<>nil then ADesigner.DrawDesignerItems(true);
  end;
  for i:=0 to JITNonFormList.Count-1 do begin
    AForm:=GetDesignerForm(JITNonFormList[i]);
    if AForm=nil then continue;
    ADesigner:=TDesigner(AForm.Designer);
    if ADesigner<>nil then ADesigner.DrawDesignerItems(true);
  end;
end;

procedure TFormEditor.CheckDesignerPositions;
var
  i: Integer;
  ADesigner: TDesigner;
  AForm: TCustomForm;
begin
  for i:=0 to JITFormList.Count-1 do begin
    ADesigner:=TDesigner(JITFormList[i].Designer);
    if ADesigner<>nil then ADesigner.CheckFormBounds;
  end;
  for i:=0 to JITNonFormList.Count-1 do begin
    AForm:=GetDesignerForm(JITNonFormList[i]);
    if AForm=nil then continue;
    ADesigner:=TDesigner(AForm.Designer);
    if ADesigner<>nil then ADesigner.CheckFormBounds;
  end;
end;

function TFormEditor.GetDesignerMediatorByComponent(AComponent: TComponent
  ): TDesignerMediator;
var
  ADesigner: TIDesigner;
begin
  ADesigner:=GetDesignerByComponent(AComponent);
  if ADesigner is TDesigner then
    Result:=TDesigner(ADesigner).Mediator
  else
    Result:=nil;
end;

function TFormEditor.ComponentUsesRTTIForMethods(Component: TComponent): boolean;
var
  Mediator: TDesignerMediator;
begin
  Mediator:=GetDesignerMediatorByComponent(Component);
  Result:=(Mediator<>nil) and (Mediator.UseRTTIForMethods(Component));
end;

initialization
  RegisterLCLBase;

end.
