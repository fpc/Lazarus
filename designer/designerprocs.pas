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
  Classes, SysUtils, LCLLinux, Forms, Controls, LCLType, Graphics;

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
  NonVisualCompIconWidth = 23;
  NonVisualCompBorder = 2;
  NonVisualCompWidth = NonVisualCompIconWidth+2*NonVisualCompBorder;

function GetParentFormRelativeTopLeft(Component: TComponent): TPoint;
function GetParentFormRelativeBounds(Component: TComponent): TRect;
function GetParentFormRelativeClientOrigin(Component: TComponent): TPoint;
function GetParentFormRelativeParentClientOrigin(Component: TComponent): TPoint;

function GetFormRelativeMousePosition(Form: TCustomForm): TPoint;

function ComponentIsTopLvl(AComponent: TComponent): boolean;
procedure GetComponentBounds(AComponent: TComponent;
  var Left, Top, Width, Height: integer);
function GetComponentLeft(AComponent: TComponent): integer;
function GetComponentTop(AComponent: TComponent): integer;
function GetComponentWidth(AComponent: TComponent): integer;
function GetComponentHeight(AComponent: TComponent): integer;

function GetParentLevel(AControl: TControl): integer;

function ControlIsDesignerVisible(AControl: TControl): boolean;

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
  GetCaretPos(Result);
  FormClientOrigin:=Form.ClientOrigin;
  dec(Result.X,FormClientOrigin.X);
  dec(Result.Y,FormClientOrigin.Y);
end;

function ComponentIsTopLvl(AComponent: TComponent): boolean;
begin
  Result:=(AComponent<>nil) and (AComponent is TControl)
     and (TControl(AComponent).Parent=nil);
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

function GetParentLevel(AControl: TControl): integer;
begin
  Result:=0;
  while AControl<>nil do begin
    inc(Result);
    AControl:=AControl.Parent;
  end;
end;

function ControlIsDesignerVisible(AControl: TControl): boolean;
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


{ TDesignerDeviceContext }

function TDesignerDeviceContext.GetDCOrigin: TPoint;
begin
  if not (ddcDCOriginValid in FFlags) then begin
    GetWindowOrgEx(FDC,FDCOrigin);
    Include(FFlags,ddcDCOriginValid);
  end;
  Result:=FDCOrigin;
end;

function TDesignerDeviceContext.GetDCSize: TPoint;
begin
  if not (ddcSizeValid in FFlags) then begin
    GetDeviceSize(FDC,FDCSize);
    Include(FFlags,ddcSizeValid);
  end;
  Result:=FDCSize;
end;

function TDesignerDeviceContext.GetFormClientOrigin: TPoint;
begin
  if not (ddcFormClientOriginValid in FFlags) then begin
    FFormClientOrigin:=FForm.ClientOrigin;
    Include(FFlags,ddcFormClientOriginValid);
  end;
  Result:=FFormClientOrigin;
end;

function TDesignerDeviceContext.GetFormOrigin: TPoint;
var
  FormClientOrig, DCOrig: TPoint;
begin
  if not (ddcFormOriginValid in FFlags) then begin
    FormClientOrig:=FormClientOrigin;
    DCOrig:=DCOrigin;
    FFormOrigin.X:=DCOrig.X-FormClientOrig.X;
    FFormOrigin.Y:=DCOrig.Y-FormClientOrig.Y;
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
    RestoreDC(DC,FSavedDC);
    FSavedDC:=0;
    FCanvas.Handle:=0;
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

end.

