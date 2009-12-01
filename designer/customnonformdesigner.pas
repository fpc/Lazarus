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

  Author: Mattias Gaertner

  Abstract:
    TCustomNonFormDesignerForm is a base designer form for non form components (TDataModule, TFrame).
}
unit CustomNonFormDesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, LCLProc, Graphics, GraphType, Forms, Controls,
  IDEProcs;
  
type

  { TCustomNonFormDesignerForm }

  TCustomNonFormDesignerForm = class(TForm)
  private
    FLookupRoot: TComponent;
    FOnLoadBounds: TNotifyEvent;
    FOnSaveBounds: TNotifyEvent;
  protected
    procedure SetLookupRoot(const AValue: TComponent); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
         override;
  public
    procedure DoLoadBounds; virtual;
    procedure DoSaveBounds; virtual;
  public
    property LookupRoot: TComponent read FLookupRoot write SetLookupRoot;
    property OnLoadBounds: TNotifyEvent read FOnLoadBounds write FOnLoadBounds;
    property OnSaveBounds: TNotifyEvent read FOnSaveBounds write FOnSaveBounds;
  end;
  
  
function CompareNonFormDesignerForms(Data1, Data2: Pointer): integer;
function CompareLookupRootAndNonFormDesignerForm(Key, Data: Pointer): integer;

implementation


function CompareNonFormDesignerForms(Data1, Data2: Pointer): integer;
var
  Form1: TCustomNonFormDesignerForm;
  Form2: TCustomNonFormDesignerForm;
begin
  Form1 := TCustomNonFormDesignerForm(Data1);
  Form2 := TCustomNonFormDesignerForm(Data2);
  Result := PtrInt(Form1.LookupRoot) - PtrInt(Form2.LookupRoot);
end;

function CompareLookupRootAndNonFormDesignerForm(Key, Data: Pointer): integer;
var
  LookupRoot: TComponent;
  Form: TCustomNonFormDesignerForm;
begin
  LookupRoot := TComponent(Key);
  Form := TCustomNonFormDesignerForm(Data);
  Result := PtrInt(LookupRoot) - PtrInt(Form.LookupRoot);
end;

{ TCustomNonFormDesignerForm }

procedure TCustomNonFormDesignerForm.SetLookupRoot(const AValue: TComponent);
begin
  if FLookupRoot = AValue then 
    Exit;
  if FLookupRoot<>nil then
    FLookupRoot.RemoveFreeNotification(Self);
  DoSaveBounds;
  FLookupRoot := AValue;
  if FLookupRoot <> nil then begin
    FLookupRoot.FreeNotification(Self);
    Caption := FLookupRoot.Name;
  end;
  DoLoadBounds;
end;

procedure TCustomNonFormDesignerForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    if AComponent=FLookupRoot then FLookupRoot:=nil;
  end;
end;

procedure TCustomNonFormDesignerForm.DoLoadBounds;
begin
  if Assigned(OnLoadBounds) then 
    OnLoadBounds(Self);
end;

procedure TCustomNonFormDesignerForm.DoSaveBounds;
begin
  if Assigned(OnSaveBounds) then 
    OnSaveBounds(Self);
end;

end.

