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
  Classes, SysUtils, Graphics, GraphType, Forms, Controls, IDEProcs;
  
type

  { TNonControlForm }

  TNonControlForm = class(TForm)
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
  
  
  { TDataModuleForm }
  
  TDataModuleForm = class(TNonControlForm)
  private
    function GetDataModule: TDataModule;
    procedure SetDataModule(const AValue: TDataModule);
  protected
    procedure SetLookupRoot(const AValue: TComponent); override;
  public
    procedure DoLoadBounds; override;
    procedure DoSaveBounds; override;
  public
    property DataModule: TDataModule read GetDataModule write SetDataModule;
  end;
  
function CompareNonControlForms(Data1, Data2: Pointer): integer;
function CompareLookupRootAndNonControlForm(Key, Data: Pointer): integer;

implementation


function CompareNonControlForms(Data1, Data2: Pointer): integer;
var
  Form1: TNonControlForm;
  Form2: TNonControlForm;
begin
  Form1:=TNonControlForm(Data1);
  Form2:=TNonControlForm(Data2);
  Result:=integer(Form1.LookupRoot)-integer(Form2.LookupRoot);
end;

function CompareLookupRootAndNonControlForm(Key, Data: Pointer): integer;
var
  LookupRoot: TComponent;
  Form: TNonControlForm;
begin
  LookupRoot:=TComponent(Key);
  Form:=TNonControlForm(Data);
  Result:=integer(LookupRoot)-integer(Form.LookupRoot);
end;

{ TNonControlForm }

procedure TNonControlForm.SetLookupRoot(const AValue: TComponent);
begin
  if FLookupRoot=AValue then exit;
  DoSaveBounds;
  FLookupRoot:=AValue;
  DoLoadBounds;
end;

procedure TNonControlForm.SetFrameWidth(const AValue: integer);
begin
  if FFrameWidth=AValue then exit;
  FFrameWidth:=AValue;
  Invalidate;
end;

constructor TNonControlForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FFrameWidth:=1;
  ControlStyle:=ControlStyle-[csAcceptsControls];
end;

destructor TNonControlForm.Destroy;
begin
  inherited Destroy;
end;

procedure TNonControlForm.Paint;
var
  ARect: TRect;
begin
  inherited Paint;
  ARect:=Rect(FrameWidth,FrameWidth,
                                ClientWidth-FrameWidth,ClientHeight-FrameWidth);
  with Canvas do begin
    Brush.Color:=clWhite;
    FillRect(ARect);
    Frame3d(ARect, FrameWidth, bvLowered);
  end;
end;

procedure TNonControlForm.DoLoadBounds;
begin
  if Assigned(OnLoadBounds) then OnLoadBounds(Self);
end;

procedure TNonControlForm.DoSaveBounds;
begin
  if Assigned(OnSaveBounds) then OnSaveBounds(Self);
end;

{ TDataModuleForm }

procedure TDataModuleForm.SetDataModule(const AValue: TDataModule);
begin
  LookupRoot:=AValue;
end;

function TDataModuleForm.GetDataModule: TDataModule;
begin
  Result:=TDataModule(LookupRoot);
end;

procedure TDataModuleForm.SetLookupRoot(const AValue: TComponent);
begin
  if AValue=LookupRoot then exit;
  if (AValue<>nil) and (not (AValue is TDataModule)) then
    RaiseException('TDataModuleForm.SetLookupRoot AValue.ClassName='
                   +AValue.ClassName);
  inherited SetLookupRoot(AValue);
end;

procedure TDataModuleForm.DoLoadBounds;
var
  CurDataModule: TDataModule;
  NewLeft: Integer;
  NewTop: Integer;
  NewWidth: Integer;
  NewHeight: Integer;
begin
  inherited DoLoadBounds;
  CurDataModule:=DataModule;
  if CurDataModule<>nil then begin
    NewLeft:=CurDataModule.DesignOffset.X;
    NewTop:=CurDataModule.DesignOffset.Y;
    NewWidth:=CurDataModule.DesignSize.X;
    NewHeight:=CurDataModule.DesignSize.Y;
    SetBounds(NewLeft,NewTop,NewWidth,NewHeight);
  end;
end;

procedure TDataModuleForm.DoSaveBounds;
var
  CurDataModule: TDataModule;
begin
  CurDataModule:=DataModule;
  if CurDataModule<>nil then begin
    CurDataModule.DesignOffset.X:=Left;
    CurDataModule.DesignOffset.Y:=Top;
    CurDataModule.DesignSize.X:=Width;
    CurDataModule.DesignSize.Y:=Height;
  end;
  inherited DoSaveBounds;
end;

end.

