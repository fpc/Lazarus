{/***************************************************************************
                             DesignerProcs.pas
                             -----------------

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

  Author: Mattias Gaertner

}
unit DesignerProcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, LCLProc, LCLIntf, LCLType, Forms, Controls,
  Graphics, FormEditingIntf;

type
  TDesignerDCFlag = (ddcDCOriginValid, ddcFormOriginValid,
    ddcFormClientOriginValid, ddcSizeValid);
  TDesignerDCFlags = set of TDesignerDCFlag;

  TDesignerDeviceContext = class
  private
    FCanvas: TCanvas;
    FDC: HDC;
    FDCOrigin: TPoint;   // DC origin on desktop
    FFlags: TDesignerDCFlags;
    FFormClientOrigin: TPoint; // Form client origin on desktop
    FFormOrigin: TPoint; // DC origin relative to designer Form
    FSavedDC: HDC;
    FDcSize: TPoint;
    FForm: TCustomForm;
    function GetDCOrigin: TPoint;
    function GetDCSize: TPoint;
    function GetFormClientOrigin: TPoint;
    function GetFormOrigin: TPoint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetDC(AForm: TCustomForm; aDC: HDC);
    procedure Clear;
    procedure Save;
    procedure Restore;
    function RectVisible(ALeft, ATop, ARight, ABottom: integer): boolean;
    property Canvas: TCanvas read FCanvas;
    property DC: HDC read FDC;
    property Form: TCustomForm read FForm;
    property FormOrigin: TPoint
      read GetFormOrigin;// DC origin relative to designer Form
    property DCOrigin: TPoint read GetDCOrigin; // DC origin on Desktop
    property FormClientOrigin: TPoint
      read GetFormClientOrigin;// Form Client Origin on desktop
    property DCSize: TPoint read GetDCSize;
  end;

const
  NonVisualCompIconWidth = ComponentPaletteImageWidth;
  NonVisualCompBorder = 2;
  NonVisualCompWidth = NonVisualCompIconWidth+2*NonVisualCompBorder;


type
  TOnComponentIsInvisible = procedure(AComponent: TComponent;
                                      var Invisible: boolean) of object;
var
  OnComponentIsInvisible: TOnComponentIsInvisible;

function GetParentLevel(AControl: TControl): integer;
function ControlIsInDesignerVisible(AControl: TControl): boolean;
function ComponentIsInvisible(AComponent: TComponent): boolean;
function ComponentIsNonVisual(AComponent: TComponent): boolean;
function ComponentBoundsDesignable(AComponent: TComponent): boolean;

function GetParentFormRelativeTopLeft(Component: TComponent): TPoint;
function GetParentFormRelativeBounds(Component: TComponent): TRect;
function GetParentFormRelativeClientOrigin(Component: TComponent): TPoint;
function GetParentFormRelativeParentClientOrigin(Component: TComponent): TPoint;

function GetFormRelativeMousePosition(Form: TCustomForm): TPoint;

procedure GetComponentBounds(AComponent: TComponent;
  var Left, Top, Width, Height: integer);
function GetComponentLeft(AComponent: TComponent): integer;
function GetComponentTop(AComponent: TComponent): integer;
function GetComponentWidth(AComponent: TComponent): integer;
function GetComponentHeight(AComponent: TComponent): integer;

procedure InvalidateDesignerRect(aHandle: HWND; ARect: pRect);


implementation


function GetParentFormRelativeTopLeft(Component: TComponent): TPoint;
var
  FormOrigin: TPoint;
  ParentForm: TCustomForm;
  Parent: TWinControl;
begin
  if Component is TControl then begin
    ParentForm:=GetParentForm(TControl(Component));
    Parent:=TControl(Component).Parent;
    if (Parent=nil) or (ParentForm=nil) then begin
      Result:=Point(0,0);
    end else begin
      Result:=Parent.ClientOrigin;
      FormOrigin:=ParentForm.ClientOrigin;
      //DebugLn(['GetParentFormRelativeTopLeft Component=',dbgsName(Component),' Parent=',dbgsName(Parent),' ',dbgs(Result),' ParentForm=',dbgsName(ParentForm),' ',dbgs(FormOrigin)]);
      Result.X:=Result.X-FormOrigin.X+TControl(Component).Left;
      Result.Y:=Result.Y-FormOrigin.Y+TControl(Component).Top;
    end;
  end else begin
    Result.X:=LongRec(Component.DesignInfo).Lo;
    Result.Y:=LongRec(Component.DesignInfo).Hi;
  end;
end;

function GetParentFormRelativeBounds(Component: TComponent): TRect;
var CTopLeft: TPoint;
begin
  CTopLeft:=GetParentFormRelativeTopLeft(Component);
  Result.Left:=CTopLeft.X;
  Result.Top:=CTopLeft.Y;
  Result.Right:=Result.Left+GetComponentWidth(Component);
  Result.Bottom:=Result.Top+GetComponentHeight(Component);
end;

function GetParentFormRelativeClientOrigin(Component: TComponent): TPoint;
var
  FormOrigin: TPoint;
  ParentForm: TCustomForm;
begin
  if Component is TControl then begin
    ParentForm:=GetParentForm(TControl(Component));
    if ParentForm=nil then begin
      Result:=Point(0,0);
    end else begin
      Result:=TControl(Component).ClientOrigin;
      FormOrigin:=ParentForm.ClientOrigin;
      Result.X:=Result.X-FormOrigin.X;
      Result.Y:=Result.Y-FormOrigin.Y;
    end;
  end else begin
    Result.X:=LongRec(Component.DesignInfo).Lo;
    Result.Y:=LongRec(Component.DesignInfo).Hi;
  end;
end;

function GetParentFormRelativeParentClientOrigin(Component: TComponent): TPoint;
var
  FormOrigin, ParentOrigin: TPoint;
  ParentForm: TCustomForm;
  Parent: TWinControl;
begin
  if Component is TControl then begin
    ParentForm:=GetParentForm(TControl(Component));
    Parent:=TControl(Component).Parent;
    if (Parent=nil) or (ParentForm=nil) then begin
      Result:=Point(0,0);
    end else begin
      ParentOrigin:=Parent.ClientOrigin;
      FormOrigin:=ParentForm.ClientOrigin;
      Result.X:=ParentOrigin.X-FormOrigin.X;
      Result.Y:=ParentOrigin.Y-FormOrigin.Y;
    end;
  end else begin
    Result:=Point(0,0);
  end;
end;

function GetFormRelativeMousePosition(Form: TCustomForm): TPoint;
var
  FormClientOrigin: TPoint;
begin
  Result.X:=0;
  Result.Y:=0;
  GetCursorPos(Result);
  FormClientOrigin:=Form.ClientOrigin;
  dec(Result.X,FormClientOrigin.X);
  dec(Result.Y,FormClientOrigin.Y);
end;

procedure GetComponentBounds(AComponent: TComponent;
  var Left, Top, Width, Height: integer);
begin
  if AComponent is TControl then begin
    Left:=TControl(AComponent).Left;
    Top:=TControl(AComponent).Top;
    Width:=TControl(AComponent).Width;
    Height:=TControl(AComponent).Height;
  end else begin
    Left:=LongRec(AComponent.DesignInfo).Lo;
    Top:=LongRec(AComponent.DesignInfo).Hi;
    Width:=NonVisualCompWidth;
    Height:=Width;
  end;
end;

function GetComponentLeft(AComponent: TComponent): integer;
begin
  if AComponent is TControl then begin
    Result:=TControl(AComponent).Left;
  end else begin
    Result:=LongRec(AComponent.DesignInfo).Lo;
  end;
end;

function GetComponentTop(AComponent: TComponent): integer;
begin
  if AComponent is TControl then begin
    Result:=TControl(AComponent).Top;
  end else begin
    Result:=LongRec(AComponent.DesignInfo).Hi;
  end;
end;

function GetComponentWidth(AComponent: TComponent): integer;
begin
  if AComponent is TControl then begin
    Result:=TControl(AComponent).Width;
  end else begin
    Result:=NonVisualCompWidth;
  end;
end;

function GetComponentHeight(AComponent: TComponent): integer;
begin
  if AComponent is TControl then begin
    Result:=TControl(AComponent).Height;
  end else begin
    Result:=NonVisualCompWidth;
  end;
end;

procedure InvalidateDesignerRect(aHandle: HWND; ARect: pRect);
const
  ExtraInvalidateFrame = 3;
var
  InvRect: TRect;
begin
  InvRect:=ARect^;
  dec(InvRect.Left,ExtraInvalidateFrame);
  dec(InvRect.Top,ExtraInvalidateFrame);
  inc(InvRect.Right,ExtraInvalidateFrame);
  inc(InvRect.Bottom,ExtraInvalidateFrame);
  InvalidateRect(aHandle,@InvRect,false);
end;

function GetParentLevel(AControl: TControl): integer;
begin
  Result:=0;
  while AControl<>nil do begin
    inc(Result);
    AControl:=AControl.Parent;
  end;
end;

function ControlIsInDesignerVisible(AControl: TControl): boolean;
begin
  Result:=true;
  while AControl<>nil do begin
    if csNoDesignVisible in AControl.ControlStyle then begin
      Result:=false;
      exit;
    end;
    AControl:=AControl.Parent;
  end;
end;

function ComponentIsInvisible(AComponent: TComponent): boolean;
begin
  Result:=false;
  if Assigned(OnComponentIsInvisible) then
    OnComponentIsInvisible(AComponent,Result);
end;

function ComponentIsNonVisual(AComponent: TComponent): boolean;
begin
  Result:=(AComponent<>nil)
          and (not (AComponent is TControl))
          and (not ComponentIsInvisible(AComponent));
end;

function ComponentBoundsDesignable(AComponent: TComponent): boolean;
begin
  Result:=(not ComponentIsInvisible(AComponent));
  if Result and (AComponent is TControl) then begin
    if [csDesignFixedBounds,csNoDesignVisible]*TControl(AComponent).ControlStyle
      <>[]
    then
      Result:=false;
  end;
end;

{ TDesignerDeviceContext }

function TDesignerDeviceContext.GetDCOrigin: TPoint;
// returns the DC origin in screen coordinates
var
  CurFormClientOrigin: TPoint;
  CurFormOrigin: TPoint;
begin
  if not (ddcDCOriginValid in FFlags) then begin
    CurFormClientOrigin:=FormClientOrigin;
    CurFormOrigin:=FormOrigin;
    FDCOrigin.X:=CurFormOrigin.X-CurFormClientOrigin.X;
    FDCOrigin.Y:=CurFormOrigin.Y-CurFormClientOrigin.Y;
    Include(FFlags,ddcDCOriginValid);
  end;
  Result:=FDCOrigin;
end;

function TDesignerDeviceContext.GetDCSize: TPoint;
// returns the DC size
begin
  if not (ddcSizeValid in FFlags) then begin
    GetDeviceSize(FDC,FDCSize);
    Include(FFlags,ddcSizeValid);
  end;
  Result:=FDCSize;
end;

function TDesignerDeviceContext.GetFormClientOrigin: TPoint;
// returns the Form Client Origin on desktop
begin
  if not (ddcFormClientOriginValid in FFlags) then begin
    FFormClientOrigin:=FForm.ClientOrigin;
    Include(FFlags,ddcFormClientOriginValid);
  end;
  Result:=FFormClientOrigin;
end;

function TDesignerDeviceContext.GetFormOrigin: TPoint;
// returns the DC origin relative to the form client origin
// For example: The DC of the client area of the form itself will return 0,0
begin
  if not (ddcFormOriginValid in FFlags) then begin
    GetDCOriginRelativeToWindow(FDC,FForm.Handle,FFormOrigin);
    Include(FFlags,ddcFormOriginValid);
  end;
  Result:=FFormOrigin;
end;

constructor TDesignerDeviceContext.Create;
begin
  inherited Create;
  FCanvas:=TCanvas.Create;
end;

destructor TDesignerDeviceContext.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TDesignerDeviceContext.SetDC(AForm: TCustomForm; aDC: HDC);
begin
  Clear;
  FDC:=aDC;
  FForm:=AForm;
end;

procedure TDesignerDeviceContext.Clear;
begin
  Restore;
  FDC:=0;
  FFlags:=FFlags-[ddcFormOriginValid,ddcFormClientOriginValid,ddcDCOriginValid,
                  ddcSizeValid];
end;

procedure TDesignerDeviceContext.Save;
begin
  if FSavedDC=0 then begin
    FSavedDC:=SaveDC(DC);
    FCanvas.Handle:=FDC;
  end;
end;

procedure TDesignerDeviceContext.Restore;
begin
  if FSavedDC<>0 then begin
    FCanvas.Handle:=0;
    RestoreDC(DC,FSavedDC);
    FSavedDC:=0;
  end;
end;

function TDesignerDeviceContext.RectVisible(ALeft, ATop, ARight,
  ABottom: integer): boolean;
// coordinates must be relative to DC origin
var
  ASize: TPoint;
begin
  if (ARight<0) or (ABottom<0) then
    Result:=false
  else begin
    ASize:=DCSize;
    if (ALeft>=ASize.X) or (ATop>=ASize.Y) then
      Result:=false
    else
      Result:=true;
  end;
end;

initialization
  OnGetDesignerForm:=nil;

end.

