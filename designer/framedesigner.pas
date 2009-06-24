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
    TFrameDesignerForm is a designer form to design TFrame.
}
unit FrameDesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, LCLProc, Graphics, GraphType, Forms, Controls,
  IDEProcs, DesignerProcs, CustomNonFormDesigner;
  
type

  { TFrameDesignerForm }

  TFrameDesignerForm = class(TCustomNonFormDesignerForm)
  protected
    FChangingBounds: Boolean;
    procedure SetLookupRoot(const AValue: TComponent); override;
    procedure OnControlChangeBounds(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoLoadBounds; override;
    procedure DoSaveBounds; override;
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
  end;
  
  
implementation

{ TFrameDesignerForm }

constructor TFrameDesignerForm.Create(AOwner: TComponent);
begin
  Position := poDesigned; 
  inherited Create(AOwner);
  // workaround problem with out assumption that Width = ClientWidth, Height = ClientHeight
  AutoScroll := False; 
end;

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
  if AValue=LookupRoot then exit;
  if LookupRoot is TControl then
    TControl(LookupRoot).RemoveAllHandlersOfObject(Self);
  if (AValue is TControl) then begin
    AControl:=TControl(AValue);
    AControl.Parent := Self;
    AControl.AddHandlerOnChangeBounds(@OnControlChangeBounds,true);
  end;
  inherited SetLookupRoot(AValue);
end;

procedure TFrameDesignerForm.OnControlChangeBounds(Sender: TObject);
var
  AControl: TControl;
  a: TAnchorKind;
begin
  if FChangingBounds then exit;
  AControl:=TControl(LookupRoot);
  FChangingBounds:=true;
  try
    // reset anchors
    for a:=Low(TAnchorKind) to High(TAnchorKind) do
      AControl.AnchorSide[a].Control:=nil;
    // reset bounds
    AControl.SetBounds(0, 0, Width, Height);
  finally
    FChangingBounds:=false;
  end;
end;

procedure TFrameDesignerForm.DoLoadBounds;

  procedure SetNewBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
  begin
    if NewWidth <= 0 then NewWidth := Width;
    if NewHeight <= 0 then NewHeight := Height;

    NewWidth := Max(20, Min(NewWidth, Screen.Width - 50));
    NewHeight := Max(20, Min(NewHeight, Screen.Height - 50));
    SetBounds(NewLeft, NewTop, Max(20, NewWidth), Max(NewHeight, 20));
  end;

var
  CurControl: TControl;
  NewLeft: integer;
  NewTop: integer;
begin
  inherited;

  if LookupRoot is TControl then
  begin
    CurControl := TControl(LookupRoot);
    // restore designer position
    NewLeft:=LongRec(LookupRoot.DesignInfo).Lo;
    NewTop:=LongRec(LookupRoot.DesignInfo).Hi;
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
  if LookupRoot is TControl then begin
    // store designer position
    LookupRoot.DesignInfo := DesignInfoFrom(Left, Top);
    // always fill the whole designer form
    TControl(LookupRoot).SetBounds(0, 0, Width, Height);
    //DebugLn(['TFrameDesignerForm.DoSaveBounds ',Left,',',Top,' ',LongRec(LookupRoot.DesignInfo).Lo,',',LongRec(LookupRoot.DesignInfo).hi]);
  end else
  if LookupRoot <> nil then
    DebugLn(['Unsupported component type in TFrameDesignerForm.DoSaveBounds: ', LookupRoot.ClassName]);
  inherited;  
end;

procedure TFrameDesignerForm.SetBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  // auto apply width and height
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);
  if (LookupRoot is TControl) then
    TControl(LookupRoot).SetBounds(0, 0, Width, Height);
end;

end.

