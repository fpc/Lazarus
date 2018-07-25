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
    TFrameDesignerForm is a designer form to design TFrame.
}
unit FrameDesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  // LCL
  LCLProc, Graphics, GraphType, Forms, Controls,
  // IdeIntf
  FormEditingIntf,
  // IDE
  DesignerProcs, CustomNonFormDesigner;

type

  { TFrameDesignerForm }

  TFrameDesignerForm = class(TCustomNonFormDesignerForm, INonFormDesigner, IFrameDesigner)
  protected
    FChangingBounds: Boolean;
    FResizing: Boolean;
    procedure SetLookupRoot(const AValue: TComponent); override;
    procedure OnControlChangeBounds(Sender: TObject);
    procedure OnControlResize(Sender: TObject);
  public
    procedure Create; override;
    constructor Create(ANonFormProxyDesignerForm: TNonFormProxyDesignerForm); override;
    destructor Destroy; override;
    procedure DoLoadBounds; override;
    procedure DoSaveBounds; override;
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
  end;
  
  
implementation

{ TFrameDesignerForm }

destructor TFrameDesignerForm.Destroy;
begin
  if LookupRoot is TControl then
    TControl(LookupRoot).RemoveAllHandlersOfObject(Self);
  inherited Destroy;
end;

procedure TFrameDesignerForm.SetLookupRoot(const AValue: TComponent);
var
  AControl: TControl;
begin
  if AValue = LookupRoot then Exit;
  if LookupRoot is TControl then
    TControl(LookupRoot).RemoveAllHandlersOfObject(Self);
  if (AValue is TControl) then
  begin
    AControl := TControl(AValue);
    AControl.Parent := NonFormProxyDesignerForm;
    AControl.AddHandlerOnChangeBounds(@OnControlChangeBounds, True);
    AControl.AddHandlerOnResize(@OnControlResize, True);
  end;
  inherited SetLookupRoot(AValue);
end;

procedure TFrameDesignerForm.OnControlChangeBounds(Sender: TObject);
var
  AControl: TControl;
  a: TAnchorKind;
begin
  if FChangingBounds then Exit;
  AControl := TControl(LookupRoot);
  FChangingBounds := True;
  try
    // reset anchors
    for a := Low(TAnchorKind) to High(TAnchorKind) do
      AControl.AnchorSide[a].Control := nil;
    // reset left, top but save width and height
    NonFormProxyDesignerForm.SetLookupRootBounds(0, 0, AControl.Width, AControl.Height);
  finally
    FChangingBounds := False;
  end;
end;

procedure TFrameDesignerForm.OnControlResize(Sender: TObject);
var
  AControl: TControl;
begin
  if FResizing then Exit;
  AControl := TControl(LookupRoot);
  FResizing := True;
  try
    // update form bounds
    with NonFormProxyDesignerForm do
    SetPublishedBounds(Left, Top, AControl.Width, AControl.Height);
  finally
    FResizing := False;
  end;
end;

procedure TFrameDesignerForm.Create;
begin
  inherited Create;
  // workaround problem with assumption that Width = ClientWidth, Height = ClientHeight
  NonFormProxyDesignerForm.AutoScroll := False;
end;

constructor TFrameDesignerForm.Create(
  ANonFormProxyDesignerForm: TNonFormProxyDesignerForm);
begin
  inherited Create(ANonFormProxyDesignerForm);
  NonFormProxyDesignerForm.Position := poDesigned;
end;

procedure TFrameDesignerForm.DoLoadBounds;

  procedure SetNewBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
  begin
    with NonFormProxyDesignerForm do
    begin
      if NewWidth <= 0 then NewWidth := Width;
      if NewHeight <= 0 then NewHeight := Height;

      if DockedDesigner then
      begin
        NewLeft:=Max(0,NewLeft);
        NewTop:=Max(0,NewTop);
      end
      else
      begin
        NewWidth := Max(20, Min(NewWidth, Screen.Width - 50));
        NewHeight := Max(20, Min(NewHeight, Screen.Height - 50));
      end;
      SetPublishedBounds(NewLeft, NewTop, NewWidth, NewHeight);
    end;
  end;

var
  CurControl: TControl;
  NewLeft: SmallInt;
  NewTop: SmallInt;
begin
  inherited;

  if LookupRoot is TControl then
  begin
    CurControl := TControl(LookupRoot);
    // restore designer position
    DesignInfoToLeftTop(LookupRoot.DesignInfo, NewLeft, NewTop);
    // resize designer form
    SetNewBounds(NewLeft,NewTop,CurControl.Width,CurControl.Height);
    //DebugLn(['TFrameDesignerForm.DoLoadBounds ',NewLeft,',',NewTop]);
  end
  else 
  if LookupRoot <> nil then 
    DebugLn(['Unsupported component type in TFrameDesignerForm.DoLoadBounds: ', LookupRoot.ClassName])
end;

procedure TFrameDesignerForm.DoSaveBounds;
begin
  if LookupRoot is TControl then
  with NonFormProxyDesignerForm do
  begin
    // store designer position
    LookupRoot.DesignInfo := LeftTopToDesignInfo(Left, Top);
    // always fill the whole designer form
    SetLookupRootBounds(0, 0, Width, Height);
    //DebugLn(['TFrameDesignerForm.DoSaveBounds ',Left,',',Top,' ',LazLongRec(LookupRoot.DesignInfo).Lo,',',LazLongRec(LookupRoot.DesignInfo).hi]);
  end
  else
  if LookupRoot <> nil then
    DebugLn(['Unsupported component type in TFrameDesignerForm.DoSaveBounds: ', LookupRoot.ClassName]);
  inherited;  
end;

procedure TFrameDesignerForm.SetBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  // auto apply width and height
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);

  with NonFormProxyDesignerForm do
    SetLookupRootBounds(0, 0, Width, Height);
end;

end.

