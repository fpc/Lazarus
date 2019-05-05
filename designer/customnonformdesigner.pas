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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
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
  Classes, SysUtils,
  FormEditingIntf;
  
type

  { TCustomNonFormDesignerForm }

  TCustomNonFormDesignerForm = class(TInterfacedObject, INonFormDesigner)
  private
    FNonFormProxyDesignerForm: TNonFormProxyDesignerForm;
    FOnLoadBounds: TNotifyEvent;
    FOnSaveBounds: TNotifyEvent;
  protected
    function GetLookupRoot: TComponent; virtual;
    procedure SetLookupRoot(const AValue: TComponent); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); virtual;
  public
    procedure Create; virtual; overload;
    constructor Create(ANonFormProxyDesignerForm: TNonFormProxyDesignerForm); virtual; overload;
    destructor Destroy; override;
    procedure DoLoadBounds; virtual;
    procedure DoSaveBounds; virtual;
    procedure SetBounds({%H-}ALeft, {%H-}ATop, {%H-}AWidth, {%H-}AHeight: integer); virtual;
    procedure Paint; virtual;
  public
    property LookupRoot: TComponent read GetLookupRoot write SetLookupRoot;
    property NonFormProxyDesignerForm: TNonFormProxyDesignerForm read FNonFormProxyDesignerForm;
    property OnLoadBounds: TNotifyEvent read FOnLoadBounds write FOnLoadBounds;
    property OnSaveBounds: TNotifyEvent read FOnSaveBounds write FOnSaveBounds;
  end;
  
  
function CompareNonFormDesignerForms(Data1, Data2: Pointer): integer;
function CompareLookupRootAndNonFormDesignerForm(Key, Data: Pointer): integer;

implementation


function CompareNonFormDesignerForms(Data1, Data2: Pointer): integer;
var
  Form1: INonFormDesigner;
  Form2: INonFormDesigner;
begin
  Form1 := TNonFormProxyDesignerForm(Data1) as INonFormDesigner;
  Form2 := TNonFormProxyDesignerForm(Data2) as INonFormDesigner;
  {$PUSH}{$Q-} // Overflow is allowed to occur
  Result := Integer(PtrInt(Form1.LookupRoot) - PtrInt(Form2.LookupRoot));
  {$POP}
end;

function CompareLookupRootAndNonFormDesignerForm(Key, Data: Pointer): integer;
var
  LookupRoot: TComponent;
  Form: INonFormDesigner;
begin
  LookupRoot := TComponent(Key);
  Form := TNonFormProxyDesignerForm(Data) as INonFormDesigner;
  {$PUSH}{$Q-} // Overflow is allowed to occur
  Result := Integer(PtrInt(LookupRoot) - PtrInt(Form.LookupRoot));
  {$POP}
end;

{ TCustomNonFormDesignerForm }

function TCustomNonFormDesignerForm.GetLookupRoot: TComponent;
begin
  Result := FNonFormProxyDesignerForm.LookupRoot;
end;

procedure TCustomNonFormDesignerForm.SetLookupRoot(const AValue: TComponent);
begin
  if FNonFormProxyDesignerForm.LookupRoot = AValue then
    Exit;
  if FNonFormProxyDesignerForm.LookupRoot<>nil then
    FNonFormProxyDesignerForm.LookupRoot.RemoveFreeNotification(FNonFormProxyDesignerForm);
  DoSaveBounds;
  FNonFormProxyDesignerForm.LookupRoot := AValue;
  if FNonFormProxyDesignerForm.LookupRoot <> nil then begin
    FNonFormProxyDesignerForm.LookupRoot.FreeNotification(FNonFormProxyDesignerForm);
    FNonFormProxyDesignerForm.Caption := FNonFormProxyDesignerForm.LookupRoot.Name;
  end;
  DoLoadBounds;
end;

procedure TCustomNonFormDesignerForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation=opRemove then begin
    if AComponent=FNonFormProxyDesignerForm.LookupRoot then FNonFormProxyDesignerForm.LookupRoot:=nil;
  end;
end;

constructor TCustomNonFormDesignerForm.Create(
  ANonFormProxyDesignerForm: TNonFormProxyDesignerForm);
begin
  FNonFormProxyDesignerForm := ANonFormProxyDesignerForm;
end;

destructor TCustomNonFormDesignerForm.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomNonFormDesignerForm.Create;
begin
  inherited Create;
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

procedure TCustomNonFormDesignerForm.SetBounds(ALeft, ATop, AWidth,
  AHeight: integer);
begin
end;

procedure TCustomNonFormDesignerForm.Paint;
begin

end;

end.

