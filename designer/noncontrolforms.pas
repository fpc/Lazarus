{  $Id$  }
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
unit NonControlForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, LCLProc, Graphics, GraphType, Forms, Controls,
  IDEProcs;
  
type

  { TNonFormDesignerForm }

  TNonFormDesignerForm = class(TForm)
  private
    FFrameWidth: integer;
    FLookupRoot: TComponent;
    FOnLoadBounds: TNotifyEvent;
    FOnSaveBounds: TNotifyEvent;
  protected
    procedure SetFrameWidth(const AValue: integer); virtual;
    procedure SetLookupRoot(const AValue: TComponent); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure DoLoadBounds; virtual;
    procedure DoSaveBounds; virtual;
  public
    property LookupRoot: TComponent read FLookupRoot write SetLookupRoot;
    property FrameWidth: integer read FFrameWidth write SetFrameWidth;
    property OnLoadBounds: TNotifyEvent read FOnLoadBounds write FOnLoadBounds;
    property OnSaveBounds: TNotifyEvent read FOnSaveBounds write FOnSaveBounds;
  end;
  
  
function CompareNonControlForms(Data1, Data2: Pointer): integer;
function CompareLookupRootAndNonControlForm(Key, Data: Pointer): integer;

implementation


function CompareNonControlForms(Data1, Data2: Pointer): integer;
var
  Form1: TNonFormDesignerForm;
  Form2: TNonFormDesignerForm;
begin
  Form1:=TNonFormDesignerForm(Data1);
  Form2:=TNonFormDesignerForm(Data2);
  Result:=integer(Form1.LookupRoot)-integer(Form2.LookupRoot);
end;

function CompareLookupRootAndNonControlForm(Key, Data: Pointer): integer;
var
  LookupRoot: TComponent;
  Form: TNonFormDesignerForm;
begin
  LookupRoot:=TComponent(Key);
  Form:=TNonFormDesignerForm(Data);
  Result:=integer(LookupRoot)-integer(Form.LookupRoot);
end;

{ TNonFormDesignerForm }

procedure TNonFormDesignerForm.SetLookupRoot(const AValue: TComponent);
begin
  if FLookupRoot=AValue then exit;
  DoSaveBounds;
  FLookupRoot:=AValue;
  if FLookupRoot<>nil then begin
    Caption:=FLookupRoot.Name;
  end;
  DoLoadBounds;
end;

procedure TNonFormDesignerForm.SetFrameWidth(const AValue: integer);
begin
  if FFrameWidth=AValue then exit;
  FFrameWidth:=AValue;
  Invalidate;
end;

constructor TNonFormDesignerForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FFrameWidth:=1;
  ControlStyle:=ControlStyle-[csAcceptsControls];
end;

destructor TNonFormDesignerForm.Destroy;
begin
  inherited Destroy;
end;

procedure TNonFormDesignerForm.Paint;
var
  ARect: TRect;
begin
  inherited Paint;
  with Canvas do begin
    Brush.Color:=clWhite;
    ARect:=Rect(FrameWidth,FrameWidth,
                                ClientWidth-FrameWidth,ClientHeight-FrameWidth);
    FillRect(ARect);
    ARect:=Rect(0,0,ClientWidth+1,ClientHeight+1);
    Pen.Color:=clBlack;
    Frame3d(ARect, FrameWidth, bvLowered);
  end;
end;

procedure TNonFormDesignerForm.DoLoadBounds;

  procedure SetNewBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
  begin
    if NewWidth<=0 then NewWidth:=Width;
    if NewHeight<=0 then NewHeight:=Height;

    NewWidth:=Max(20,Min(NewWidth,Screen.Width-50));
    NewHeight:=Max(20,Min(NewHeight,Screen.Height-50));
    NewLeft:=Max(0,Min(NewLeft,Screen.Width-NewWidth-50));
    NewTop:=Max(0,Min(NewTop,Screen.Height-NewHeight-50));

    //debugln('TNonFormDesignerForm.DoLoadBounds (TDataModule) ',dbgsName(LookupRoot),' ',dbgs(NewLeft),',',dbgs(NewTop),',',dbgs(NewWidth),',',dbgs(NewHeight));
    SetBounds(NewLeft,NewTop,Max(20,NewWidth),Max(NewHeight,20));
  end;

var
  CurDataModule: TDataModule;
  NewLeft: Integer;
  NewTop: Integer;
  NewWidth: Integer;
  NewHeight: Integer;
begin
  if Assigned(OnLoadBounds) then OnLoadBounds(Self);
  if LookupRoot is TDataModule then begin
    CurDataModule:=TDataModule(LookupRoot);
    NewLeft:=CurDataModule.DesignOffset.X;
    NewTop:=CurDataModule.DesignOffset.Y;
    NewWidth:=CurDataModule.DesignSize.X;
    NewHeight:=CurDataModule.DesignSize.Y;
    
    SetNewBounds(NewLeft,NewTop,NewWidth,NewHeight);
  end else if LookupRoot<>nil then begin
    NewLeft:=LongRec(LookupRoot.DesignInfo).Lo;
    NewTop:=LongRec(LookupRoot.DesignInfo).Hi;
    SetNewBounds(NewLeft,NewTop,Width,Height);
  end;
end;

procedure TNonFormDesignerForm.DoSaveBounds;
begin
  if LookupRoot is TDataModule then begin
    with TDataModule(LookupRoot) do begin
      DesignOffset:=Point(Left,Top);
      DesignSize:=Point(Width,Height);
      //debugln('TNonFormDesignerForm.DoSaveBounds (TDataModule) ',dbgsName(LookupRoot),' ',dbgs(DesignOffset.X),',',dbgs(DesignOffset.Y));
    end;
  end else if LookupRoot<>nil then begin
    //debugln('TNonFormDesignerForm.DoSaveBounds ',dbgsName(LookupRoot),' ',dbgs(Left),',',dbgs(Top));
    LongRec(LookupRoot.DesignInfo).Lo:=Left;
    LongRec(LookupRoot.DesignInfo).Hi:=Top;
  end;
  if Assigned(OnSaveBounds) then OnSaveBounds(Self);
end;

end.

