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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

}
unit DesignerProcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, typinfo,
  // LazUtils
  LazLoggerBase,
  // LCL
  LCLIntf, LCLType, Forms, Controls, Graphics,
  // IdeIntf
  FormEditingIntf;

type
  TDesignerDCFlag = (
    ddcDCOriginValid,         // please comment
    ddcFormOriginValid,       //
    ddcFormClientOriginValid, //
    ddcSizeValid              //
  );
  TDesignerDCFlags = set of TDesignerDCFlag;

  { TDesignerDeviceContext }

  TDesignerDeviceContext = class
  private
    FCanvas: TCanvas;
    FDC: HDC;
    FDCControl: TControl;
    FDCOrigin: TPoint;   // DC origin on desktop
    FFlags: TDesignerDCFlags;
    FFormClientOrigin: TPoint; // Form client origin on desktop
    FFormOrigin: TPoint; // DC origin relative to designer Form
    FDcSize: TPoint;
    FForm: TCustomForm;
    FSavedDC: HDC;
    FPaintCount: integer;
    function GetDCOrigin: TPoint;
    function GetDCSize: TPoint;
    function GetFormClientOrigin: TPoint;
    function GetFormOrigin: TPoint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetDC(AForm: TCustomForm; ADCControl: TControl; ADC: HDC);
    procedure Clear;
    procedure BeginPainting;
    procedure EndPainting;
    function RectVisible(ALeft, ATop, ARight, ABottom: integer): boolean;
    property Canvas: TCanvas read FCanvas;
    property DC: HDC read FDC;
    property Form: TCustomForm read FForm;
    property FormOrigin: TPoint read GetFormOrigin;// DC origin relative to designer Form
    property DCOrigin: TPoint read GetDCOrigin; // DC origin on Desktop
    property FormClientOrigin: TPoint read GetFormClientOrigin;// Form Client Origin on desktop
    property DCSize: TPoint read GetDCSize;
  end;

const
  NonVisualCompBorder = 2;


type
  TOnComponentIsInvisible = procedure(AComponent: TComponent;
                                      var Invisible: boolean) of object;
var
  OnComponentIsInvisible: TOnComponentIsInvisible;

function NonVisualCompWidth: integer;
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
  out Left, Top, Width, Height: integer);
function GetComponentLeft(AComponent: TComponent): integer;
function GetComponentTop(AComponent: TComponent): integer;
function GetComponentWidth(AComponent: TComponent): integer;
function GetComponentHeight(AComponent: TComponent): integer;

procedure InvalidateDesignerRect(aHandle: HWND; ARect: pRect);

procedure WriteComponentStates(aComponent: TComponent; Recursive: boolean;
  const Prefix: string = '');

procedure ScaleNonVisual(const aParent: TComponent;
  const AFromPPI, AToPPI: Integer);

implementation


function GetParentFormRelativeTopLeft(Component: TComponent): TPoint;
var
  FormOrigin: TPoint;
  ParentForm: TCustomForm;
  Parent: TWinControl;
  p: TPoint;
begin
  if Component is TControl then
  begin
    ParentForm := GetDesignerForm(TControl(Component));
    Parent := TControl(Component).Parent;
    if (Parent = nil) or (ParentForm = nil) then
    begin
      Result := Point(0, 0);
    end else
    begin
      Result := Parent.ClientOrigin;
      FormOrigin := ParentForm.ClientOrigin;
      //DebugLn(['GetParentFormRelativeTopLeft Component=',dbgsName(Component),' Parent=',dbgsName(Parent),' ',dbgs(Result),' ParentForm=',dbgsName(ParentForm),' ',dbgs(FormOrigin)]);
      Result.X := Result.X - FormOrigin.X + TControl(Component).Left;
      Result.Y := Result.Y - FormOrigin.Y + TControl(Component).Top;
    end;
  end else
  begin
    Result.X := LeftFromDesignInfo(Component.DesignInfo);
    Result.Y := TopFromDesignInfo(Component.DesignInfo);
    if Component.Owner is TWinControl then
    begin
      Parent:=TWinControl(Component.Owner);
      ParentForm := GetDesignerForm(Parent);
      if (ParentForm<>nil) and (ParentForm<>Parent) then
      begin
        p:=Parent.ClientOrigin;
        FormOrigin := ParentForm.ClientOrigin;
        inc(Result.X,p.X-FormOrigin.X);
        inc(Result.Y,p.Y-FormOrigin.Y);
      end;
    end;
  end;
end;

function GetParentFormRelativeBounds(Component: TComponent): TRect;
var
  CTopLeft: TPoint;
begin
  CTopLeft := GetParentFormRelativeTopLeft(Component);
  Result.Left := CTopLeft.X;
  Result.Top := CTopLeft.Y;
  Result.Right := Result.Left + GetComponentWidth(Component);
  Result.Bottom := Result.Top + GetComponentHeight(Component);
end;

function GetParentFormRelativeClientOrigin(Component: TComponent): TPoint;
var
  FormOrigin: TPoint;
  ParentForm: TCustomForm;
begin
  if Component is TControl then
  begin
    ParentForm := GetDesignerForm(TControl(Component));
    if ParentForm = nil then
      Result := Point(0, 0)
    else
    begin
      Result := TControl(Component).ClientOrigin;
      FormOrigin := ParentForm.ClientOrigin;
      Result.X := Result.X - FormOrigin.X;
      Result.Y := Result.Y - FormOrigin.Y;
    end;
  end else
  begin
    Result.X := LeftFromDesignInfo(Component.DesignInfo);
    Result.Y := TopFromDesignInfo(Component.DesignInfo);
  end;
end;

function GetParentFormRelativeParentClientOrigin(Component: TComponent): TPoint;
var
  FormOrigin, ParentOrigin: TPoint;
  ParentForm: TCustomForm;
  Parent: TWinControl;
begin
  if Component is TControl then
  begin
    ParentForm := GetDesignerForm(TControl(Component));
    Parent := TControl(Component).Parent;
    if (Parent = nil) or (ParentForm = nil) then
      Result := Point(0, 0)
    else
    begin
      ParentOrigin := Parent.ClientOrigin;
      FormOrigin := ParentForm.ClientOrigin;
      Result.X := ParentOrigin.X - FormOrigin.X;
      Result.Y := ParentOrigin.Y - FormOrigin.Y;
    end;
  end
  else
  begin
    Result := Point(0, 0);
    ParentForm := GetDesignerForm(Component);
    if ParentForm = nil then
      Exit;
    if (Component <> nil) and (Component.Owner <> ParentForm) then
    begin
      Component := Component.Owner;
      if (csInline in Component.ComponentState) and (Component is TControl) then
      begin
        with ParentForm.ScreenToClient(TControl(Component).ClientToScreen(Point(0, 0))) do
        begin
          inc(Result.X, X);
          inc(Result.Y, Y);
        end;
      end;
    end;
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

procedure GetComponentBounds(AComponent: TComponent; out Left, Top, Width,
  Height: integer);
begin
  if AComponent is TControl then 
  begin
    Left := TControl(AComponent).Left;
    Top := TControl(AComponent).Top;
    Width := TControl(AComponent).Width;
    Height := TControl(AComponent).Height;
  end else 
  begin
    Left := LeftFromDesignInfo(AComponent.DesignInfo);
    Top := TopFromDesignInfo(AComponent.DesignInfo);
    Width := NonVisualCompWidth;
    Height := Width;
  end;
end;

function GetComponentLeft(AComponent: TComponent): integer;
begin
  if AComponent is TControl then
    Result := TControl(AComponent).Left
  else
    Result := LeftFromDesignInfo(AComponent.DesignInfo);
end;

function GetComponentTop(AComponent: TComponent): integer;
begin
  if AComponent is TControl then
    Result := TControl(AComponent).Top
  else
    Result := TopFromDesignInfo(AComponent.DesignInfo);
end;

function GetComponentWidth(AComponent: TComponent): integer;
begin
  if AComponent is TControl then
    Result := TControl(AComponent).Width
  else
    Result := NonVisualCompWidth;
end;

function GetComponentHeight(AComponent: TComponent): integer;
begin
  if AComponent is TControl then
    Result := TControl(AComponent).Height
  else
    Result := NonVisualCompWidth;
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

procedure WriteComponentStates(aComponent: TComponent; Recursive: boolean;
  const Prefix: string);
var
  i: Integer;
begin
  if aComponent=nil then exit;
  debugln([Prefix,DbgSName(aComponent),' ',dbgs(aComponent.ComponentState)]);
  if Recursive then begin
    for i:=0 to aComponent.ComponentCount-1 do
      WriteComponentStates(aComponent.Components[i],true,Prefix+'  ');
  end;
end;

procedure ScaleNonVisual(const aParent: TComponent; const AFromPPI,
  AToPPI: Integer);
var
  I: Integer;
  Comp: TComponent;
  DsgnInfo: LongInt;
begin
  for I := 0 to aParent.ComponentCount-1 do
  begin
    Comp := aParent.Components[I];
    DsgnInfo := Comp.DesignInfo;
    LongRec(DsgnInfo).Lo:=MulDiv(LongRec(DsgnInfo).Lo, AToPPI, AFromPPI);
    LongRec(DsgnInfo).Hi:=MulDiv(LongRec(DsgnInfo).Hi, AToPPI, AFromPPI);
    Comp.DesignInfo := DsgnInfo;
  end;
end;

function NonVisualCompWidth: integer;
begin
  if Application.Scaled then
    Result := MulDiv(ComponentPaletteImageWidth, Screen.PixelsPerInch, 96) + 2 * NonVisualCompBorder
  else
    Result := ComponentPaletteImageWidth + 2 * NonVisualCompBorder
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
  if not (ddcDCOriginValid in FFlags) then
  begin
    CurFormClientOrigin := FormClientOrigin;
    CurFormOrigin := FormOrigin;
    FDCOrigin.X := CurFormOrigin.X - CurFormClientOrigin.X;
    FDCOrigin.Y := CurFormOrigin.Y - CurFormClientOrigin.Y;
    Include(FFlags, ddcDCOriginValid);
  end;
  Result:=FDCOrigin;
end;

function TDesignerDeviceContext.GetDCSize: TPoint;
// returns the DC size
begin
  if not (ddcSizeValid in FFlags) then 
  begin
    GetDeviceSize(DC, FDCSize);
    Include(FFlags, ddcSizeValid);
  end;
  Result := FDCSize;
end;

function TDesignerDeviceContext.GetFormClientOrigin: TPoint;
// returns the Form Client Origin on desktop
begin
  if not (ddcFormClientOriginValid in FFlags) then
  begin
    FFormClientOrigin := FForm.ClientOrigin;
    Include(FFlags, ddcFormClientOriginValid);
  end;
  Result := FFormClientOrigin;
end;

function TDesignerDeviceContext.GetFormOrigin: TPoint;
// returns the DC origin relative to the form client origin
// For example: The DC of the client area of the form itself will return 0,0
var
  AControlOrigin: TPoint;
begin
  if not (ddcFormOriginValid in FFlags) then 
  begin
    if not GetDCOriginRelativeToWindow(DC, FForm.Handle, FFormOrigin) then
    begin
      // For some reason we cannot retrieve the DC origin. It can happen for example
      // when DC is not a control DC but a double buffer DC. Lets use another trick
      if FDCControl <> nil then
      begin
        if FDCControl.Parent <> nil then
          AControlOrigin := FDCControl.Parent.ClientToScreen(FDCControl.BoundsRect.TopLeft)
        else
          AControlOrigin := FDCControl.ClientToScreen(Point(0, 0));
        FFormOrigin := FForm.ClientToScreen(Point(0, 0));
        FFormOrigin.X := AControlOrigin.X - FFormOrigin.X;
        FFormOrigin.Y := AControlOrigin.Y - FFormOrigin.Y;
      end
      else
        FFormOrigin := Point(0, 0);
      if GetWindowOrgEx(DC, @AControlOrigin) <> 0 then
      begin
        Dec(FFormOrigin.X, AControlOrigin.X);
        Dec(FFormOrigin.Y, AControlOrigin.Y);
      end;
    end;
    Include(FFlags, ddcFormOriginValid);
    // DebugLn(['New origin: ', FFormOrigin.X, ':', FFormOrigin.Y]);
  end;
  Result := FFormOrigin;
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

procedure TDesignerDeviceContext.SetDC(AForm: TCustomForm;
  ADCControl: TControl; ADC: HDC);
begin
  Clear;
  FDC := ADC;
  FDCControl := ADCControl;
  FForm := AForm;
end;

procedure TDesignerDeviceContext.Clear;
begin
  if (FSavedDC<>0) or (FPaintCount>0) then
    RaiseGDBException('');
  FDC := 0;
  FFlags := FFlags - [ddcFormOriginValid, ddcFormClientOriginValid, ddcDCOriginValid, ddcSizeValid];
end;

procedure TDesignerDeviceContext.BeginPainting;
begin
  if FSavedDC = 0 then
  begin
    FSavedDC := SaveDC(DC);
    FCanvas.Handle := DC;
  end;
  inc(FPaintCount);
  //DebugLn(['TDesignerDeviceContext.BeginPainting ',FPaintCount]);
end;

procedure TDesignerDeviceContext.EndPainting;
begin
  //DebugLn(['TDesignerDeviceContext.EndPainting ',FPaintCount]);
  dec(FPaintCount);
  if (FPaintCount=0) and (FSavedDC <> 0) then
  begin
    FCanvas.Handle := 0;
    RestoreDC(DC, FSavedDC);
    FSavedDC := 0;
  end;
end;

function TDesignerDeviceContext.RectVisible(ALeft, ATop, ARight,
  ABottom: integer): boolean;
// coordinates must be relative to DC origin
var
  ASize: TPoint;
begin
  if (ARight < 0) or (ABottom < 0) then
    Result := False
  else begin
    ASize := DCSize;
    if (ALeft >= ASize.X) or (ATop >= ASize.Y) then
      Result := False
    else
      Result := True;
  end;
end;

initialization
  OnGetDesignerForm:=nil;

end.

