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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit FormEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, CustomFormEditor, Controls, Forms, Buttons, SysUtils, Graphics,
  ObjectInspector, Designer;

type
  TFormEditor = class(TCustomFormEditor)
  protected
    procedure SetObj_Inspector(AnObjectInspector: TObjectInspector); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PaintAllDesignerItems;
  end;

var
  FormEditor1 : TFormEditor;


implementation


procedure TFormEditor.SetObj_Inspector(AnObjectInspector: TObjectInspector);
begin
  if AnObjectInspector=Obj_Inspector then exit;
  
  inherited SetObj_Inspector(AnObjectInspector);
  
end;

constructor TFormEditor.Create;
Begin
  inherited Create;
end;

destructor TFormEditor.destroy;
Begin
  inherited Destroy;
end;

procedure TFormEditor.PaintAllDesignerItems;
var
  i: Integer;
  ADesigner: TDesigner;
  AForm: TCustomForm;
begin
  for i:=0 to JITFormList.Count-1 do begin
    ADesigner:=TDesigner(JITFormList[i].Designer);
    if ADesigner<>nil then begin
      ADesigner.DrawDesignerItems(true);
    end;
  end;
  for i:=0 to JITDataModuleList.Count-1 do begin
    AForm:=GetDesignerForm(JITDataModuleList[i]);
    if AForm=nil then continue;
    ADesigner:=TDesigner(AForm.Designer);
    if ADesigner<>nil then begin
      ADesigner.DrawDesignerItems(true);
    end;
  end;
end;

end.
