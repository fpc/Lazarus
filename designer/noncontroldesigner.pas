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
    TNonControlForm is a designer form to design non TControl components like
    TDataModule.
}
unit NonControlDesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, LCLProc, Graphics, GraphType, Forms, Controls,
  IDEProcs, DesignerProcs, FormEditingIntf, CustomNonFormDesigner;
  
type

  { TNonControlDesignerForm }

  TNonControlDesignerForm = class(TCustomNonFormDesignerForm, INonFormDesigner, INonControlDesigner)
  private
    FFrameWidth: integer;
    function GetMediator: TDesignerMediator;
    procedure SetMediator(AValue: TDesignerMediator);
  protected
    procedure SetFrameWidth(const AValue: integer); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
          override;
  public
    procedure Create; override; overload;
    destructor Destroy; override;
    procedure Paint; override;
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
    procedure DoLoadBounds; override;
    procedure DoSaveBounds; override;
  public
    property FrameWidth: integer read FFrameWidth write SetFrameWidth;
    property Mediator: TDesignerMediator read GetMediator write SetMediator;
  end;
  
  
implementation


{ TNonControlDesignerForm }

function TNonControlDesignerForm.GetMediator: TDesignerMediator;
begin
  Result := TNonControlProxyDesignerForm(NonFormProxyDesignerForm).Mediator;
end;

procedure TNonControlDesignerForm.SetMediator(AValue: TDesignerMediator);
begin
  with TNonControlProxyDesignerForm(NonFormProxyDesignerForm) do
  begin
    if Mediator=AValue then exit;
    if Mediator<>nil then begin
      Mediator.LCLForm:=nil;
      Mediator.RemoveFreeNotification(NonFormProxyDesignerForm);
    end;
    Mediator:=AValue;
    if Mediator<>nil then begin
      Mediator.LCLForm:=NonFormProxyDesignerForm;
      Mediator.FreeNotification(NonFormProxyDesignerForm);
      DoLoadBounds;
    end;
  end;
end;

procedure TNonControlDesignerForm.SetFrameWidth(const AValue: integer);
begin
  if FFrameWidth = AValue then 
    Exit;
  FFrameWidth := AValue;
  NonFormProxyDesignerForm.Invalidate;
end;

procedure TNonControlDesignerForm.SetBounds(aLeft, aTop, aWidth,
  aHeight: integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if Mediator<>nil then
    Mediator.SetFormBounds(LookupRoot,NonFormProxyDesignerForm.BoundsRect,NonFormProxyDesignerForm.ClientRect);
end;

procedure TNonControlDesignerForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    if Mediator=AComponent then Mediator:=nil;
  end;
end;

procedure TNonControlDesignerForm.Create;
begin
  inherited;
  FFrameWidth := 1;
  NonFormProxyDesignerForm.ControlStyle := NonFormProxyDesignerForm.ControlStyle - [csAcceptsControls];
end;

destructor TNonControlDesignerForm.Destroy;
var
  tmp: TDesignerMediator;
begin
  try
    tmp := Mediator;
    Mediator := nil;
    tmp.Free;
  except
    on E: Exception do begin
      debugln(['TNonControlDesignerForm.Destroy freeing mediator failed: ',E.Message]);
    end;
  end;
  inherited Destroy;
end;

procedure TNonControlDesignerForm.Paint;
var
  ARect: TRect;
begin
  inherited Paint;
  with NonFormProxyDesignerForm do
  with Canvas do begin
    if LookupRoot is TDataModule then
    begin
      Brush.Color:=clWhite;
      ARect:=Rect(FrameWidth,FrameWidth,
          ClientWidth-FrameWidth,
          ClientHeight-FrameWidth);
      FillRect(ARect);
      ARect:=Rect(0,0,ClientWidth+1,ClientHeight+1);
      Pen.Color:=clBlack;
      Frame3d(ARect, FrameWidth, bvLowered);
    end;
    if (Mediator<>nil) and (LookupRoot<>nil) then
      Mediator.Paint;
  end;
end;

procedure TNonControlDesignerForm.DoLoadBounds;

  procedure SetNewBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
  begin
    with NonFormProxyDesignerForm do
    begin
      if NewWidth<=0 then NewWidth:=Width;
      if NewHeight<=0 then NewHeight:=Height;

      if DockedDesigner then
      begin
        NewLeft:=Max(0,NewLeft);
        NewTop:=Max(0,NewTop);
        SetPublishedBounds(NewLeft,NewTop,Max(0,NewWidth),Max(NewHeight,0));
      end
      else
      begin
        NewWidth:=Max(20,Min(NewWidth,Screen.Width-50));
        NewHeight:=Max(20,Min(NewHeight,Screen.Height-50));
        NewLeft:=Max(0,Min(NewLeft,Screen.Width-NewWidth-50));
        NewTop:=Max(0,Min(NewTop,Screen.Height-NewHeight-50));
        SetPublishedBounds(NewLeft,NewTop,Max(20,NewWidth),Max(NewHeight,20));
      end;
      //debugln('TNonControlDesignerForm.DoLoadBounds (TDataModule) ',dbgsName(LookupRoot),' ',dbgs(NewLeft),',',dbgs(NewTop),',',dbgs(NewWidth),',',dbgs(NewHeight));
    end;
  end;

var
  CurDataModule: TDataModule;
  NewLeft, NewTop: integer;
  NewWidth, NewHeight: Integer;
  NewBounds, NewClientRect: TRect;
begin
  inherited DoLoadBounds;
  if LookupRoot=nil then exit;

  if LookupRoot is TDataModule then 
  begin
    CurDataModule := TDataModule(LookupRoot);
    NewLeft := CurDataModule.DesignOffset.X;
    NewTop := CurDataModule.DesignOffset.Y;
    NewWidth := CurDataModule.DesignSize.X;
    NewHeight := CurDataModule.DesignSize.Y;
    
    SetNewBounds(NewLeft, NewTop, NewWidth, NewHeight);
  end else with NonFormProxyDesignerForm do begin
    if Mediator<>nil then begin
      Mediator.GetFormBounds(LookupRoot,NewBounds,NewClientRect);
      NewLeft:=NewBounds.Left;
      NewTop:=NewBounds.Top;
      NewWidth:=NewBounds.Right-NewBounds.Left;
      NewHeight:=NewBounds.Bottom-NewBounds.Top;
      if (NewClientRect.Left<>NewClientRect.Right)
      or (NewClientRect.Top<>NewClientRect.Bottom) then begin
        // use the clientrect (the Width, Height depends on window theme)
        NewWidth:=NewClientRect.Right-NewClientRect.Left+Width-ClientWidth;
        NewHeight:=NewClientRect.Bottom-NewClientRect.Top+Height-ClientHeight;
      end;
    end else begin
      GetComponentLeftTopOrDesignInfo(LookupRoot,NewLeft,NewTop);
      NewWidth:=Width;
      NewHeight:=Height;
    end;
    SetNewBounds(NewLeft, NewTop, NewWidth, NewHeight);
  end;
end;

procedure TNonControlDesignerForm.DoSaveBounds;
var
  LBoundsRect: TRect;
  LClientRect: TRect;
begin
  if LookupRoot is TDataModule then begin
    with NonFormProxyDesignerForm, TDataModule(LookupRoot) do begin
      DesignOffset:=Point(Left,Top);
      DesignSize:=Point(Width,Height);
      //debugln('TNonControlDesignerForm.DoSaveBounds (TDataModule) ',dbgsName(LookupRoot),' ',dbgs(DesignOffset.X),',',dbgs(DesignOffset.Y));
    end;
  end else if LookupRoot<>nil then with NonFormProxyDesignerForm do begin
    //debugln(['TNonControlDesignerForm.DoSaveBounds ',dbgsName(LookupRoot),' ',dbgs(Left),',',dbgs(Top),' ',DbgSName(Mediator)]);
    if Mediator<>nil then begin
      LBoundsRect := Rect(Left, Top, Left + Width, Top + Height);
      LClientRect := Rect(0, 0, Width, Height);

      Mediator.SetFormBounds(LookupRoot, LBoundsRect, LClientRect);
    end else begin
      SetComponentLeftTopOrDesignInfo(LookupRoot,Left,Top);
    end;
  end;
  inherited DoSaveBounds;
end;

end.

