{
 *****************************************************************************
 *                                Gtk3WSForms.pp                                 *
 *                                ----------                                 * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk3WSForms;
{$i gtk3defines.inc}
{$mode objfpc}{$H+}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Classes, Graphics, Controls, Forms, LCLType, LCLProc,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, Gtk3WSControls, WSFactory, WSForms, WSProc,
  LazGtk3, LazGdk3, LazGLib2, gtk3widgets, gtk3int, gtk3objects;

type
  { TWSScrollingWinControl }

  TGtk3WSScrollingWinControlClass = class of TWSScrollingWinControl;

  { TGtk3WSScrollingWinControl }

  TGtk3WSScrollingWinControl = class(TWSScrollingWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TWSScrollBox }

  TGtk3WSScrollBox = class(TGtk3WSScrollingWinControl)
  published
  end;

  { TWSCustomFrame }

  TGtk3WSCustomFrame = class(TGtk3WSScrollingWinControl)
  published
  end;

  { TWSFrame }

  TGtk3WSFrame = class(TGtk3WSCustomFrame)
  published
  end;

  { TWSCustomForm }

  { TGtk3WSCustomForm }

  TGtk3WSCustomForm = class(TWSCustomForm)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;

    class procedure CloseModal(const ACustomForm: TCustomForm); override;
    class procedure SetAllowDropFiles(const AForm: TCustomForm; AValue: Boolean); override;
    class procedure SetAlphaBlend(const ACustomForm: TCustomForm; const AlphaBlend: Boolean;
      const Alpha: Byte); override;
    class procedure SetBorderIcons(const AForm: TCustomForm;
        const ABorderIcons: TBorderIcons); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetFormStyle(const AForm: TCustomform; const AFormStyle, AOldFormStyle: TFormStyle); override;
    class procedure SetIcon(const AForm: TCustomForm; const Small, Big: HICON); override;
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
    class procedure SetRealPopupParent(const ACustomForm: TCustomForm;
      const APopupParent: TCustomForm); override;
    class procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); override;
    class procedure SetZPosition(const AWinControl: TWinControl; const APosition: TWSZPosition); override;
    class function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;

    {mdi support}
    class function ActiveMDIChild(const AForm: TCustomForm): TCustomForm; override;
    class function Cascade(const AForm: TCustomForm): Boolean; override;
    class function GetClientHandle(const AForm: TCustomForm): HWND; override;
    class function GetMDIChildren(const AForm: TCustomForm; AIndex: Integer): TCustomForm; override;
    class function Next(const AForm: TCustomForm): Boolean; override;
    class function Previous(const AForm: TCustomForm): Boolean; override;
    class function Tile(const AForm: TCustomForm): Boolean; override;
    class function MDIChildCount(const AForm: TCustomForm): Integer; override;
  end;
  TGtk3WSCustomFormClass = class of TGtk3WSCustomForm;

  { TWSForm }

  TGtk3WSForm = class(TGtk3WSCustomForm)
  published
  end;

  { TWSHintWindow }

  { TGtk3WSHintWindow }

  TGtk3WSHintWindow = class(TGtk3WSCustomForm)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TWSScreen }

  TGtk3WSScreen = class(TWSLCLComponent)
  published
  end;

  { TWSApplicationProperties }

  TGtk3WSApplicationProperties = class(TWSLCLComponent)
  published
  end;

implementation
uses SysUtils, gtk3procs;


{ TGtk3WSScrollingWinControl }

class function TGtk3WSScrollingWinControl.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TGtk3ScrollingWinControl.Create(AWinControl, AParams));
end;

{ TGtk3WSCustomForm }

class function TGtk3WSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  AWindow: TGtk3Window;
  AWinPtr: PGtkWindow;
  ARect: TGdkRectangle;
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.CreateHandle');
  {$ENDIF}
  AWindow := TGtk3Window.Create(AWinControl, AParams);

  AWinPtr := PGtkWindow(AWindow.Widget);
  AWindow.Title := AWinControl.Caption;

  AWinPtr^.set_resizable(True);
  AWinPtr^.set_has_resize_grip(False);

  with ARect do
  begin
    x := AWinControl.Left;
    y := AWinControl.Top;
    width := AWinControl.Width;
    height := AWinControl.Height;
  end;
  AWinPtr^.set_allocation(@ARect);
  Gtk3WidgetSet.AddWindow(AWinPtr);

  Result := TLCLIntfHandle(AWindow);

  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.CreateHandle handle ',dbgs(Result));
  {$ENDIF}
end;

class procedure TGtk3WSCustomForm.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
var
  AWidget: PGtkWidget;
  ARect: TGdkRectangle;
  Geometry: TGdkGeometry;
  AHints: TGdkWindowHints;
  AFixedWidthHeight: Boolean;
  AForm: TCustomForm;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBounds') then
    Exit;
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.SetBounds ',dbgsName(AWinControl),Format(' ALeft %d ATop %d AWidth %d AHeight %d',[ALeft, ATop, AWidth, AHeight]));
  {$ENDIF}
  AWidget := TGtk3Widget(AWinControl.Handle).Widget;
  AForm := TCustomForm(AWinControl);
  TGtk3Widget(AWinControl.Handle).BeginUpdate;
  ARect.x := ALeft;
  ARect.y := ATop;
  ARect.width := AWidth;
  ARect.Height := AHeight;
  try
    AWidget^.size_allocate(@ARect);
    if not (csDesigning in AForm.ComponentState) and (AForm.Parent = nil) and (AForm.ParentWindow = 0) then
    begin
      AFixedWidthHeight := AForm.BorderStyle in [bsDialog, bsSingle, bsToolWindow];
      with Geometry do
      begin
        if not AFixedWidthHeight and (AForm.Constraints.MinWidth > 0) then
          min_width := AForm.Constraints.MinWidth
        else
          min_width := AForm.Width;
        if not AFixedWidthHeight and (AForm.Constraints.MaxWidth > 0) then
          max_width := AForm.Constraints.MaxWidth
        else
        max_width := AForm.Width;
        if not AFixedWidthHeight and (AForm.Constraints.MinHeight > 0) then
          min_height := AForm.Constraints.MinHeight
        else
          min_height := AForm.Height;
        if not AFixedWidthHeight and (AForm.Constraints.MaxHeight > 0) then
          max_height := AForm.Constraints.MaxHeight
        else
          max_height := AForm.Height;

        base_width := AForm.Width;
        base_height := AForm.Height;
        width_inc := 1;
        height_inc := 1;
        min_aspect := 0;
        max_aspect := 1;
        win_gravity := PGtkWindow(AWidget)^.get_gravity;
      end;

      if AFixedWidthHeight then
        PGtkWindow(AWidget)^.set_geometry_hints(nil, @Geometry,
          GDK_HINT_POS or GDK_HINT_MIN_SIZE or GDK_HINT_MAX_SIZE)
      else
      begin
        if AForm.BorderStyle <> bsNone then
        begin
          AHints := GDK_HINT_POS or GDK_HINT_BASE_SIZE;
          if (AForm.Constraints.MinHeight > 0) or (AForm.Constraints.MinWidth > 0) then
            AHints := AHints or GDK_HINT_MIN_SIZE;
          if (AForm.Constraints.MaxHeight > 0) or (AForm.Constraints.MaxWidth > 0) then
            AHints := AHints or GDK_HINT_MAX_SIZE;

          PGtkWindow(AWidget)^.set_geometry_hints(nil, @Geometry, AHints);
        end;
      end;
    end;
    PGtkWindow(AWidget)^.resize(AWidth, AHeight);
    PGtkWindow(AWidget)^.move(ALeft, ATop);
  finally
    TGtk3Widget(AWinControl.Handle).EndUpdate;
  end;
end;

class procedure TGtk3WSCustomForm.ShowHide(const AWinControl: TWinControl);
var
  AMask: TGdkEventMask;
  AForm: TCustomForm;
  AWindow: PGtkWindow;
  i: Integer;
  AGeom: TGdkGeometry;
  AGeomMask: TGdkWindowHints;
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.ShowHide handleAllocated=',dbgs(AWinControl.HandleAllocated));
  {$ENDIF}
  if not WSCheckHandleAllocated(AWinControl, 'ShowHide') then
    Exit;
  AForm := TCustomForm(AWinControl);
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.ShowHide visible=',dbgs(AWinControl.HandleObjectShouldBeVisible));
  {$ENDIF}
  AWindow := PGtkWindow(TGtk3Widget(AForm.Handle).Widget);

  if (fsModal in AForm.FormState) and AForm.HandleObjectShouldBeVisible then
  begin
    AWindow^.set_type_hint(GDK_WINDOW_TYPE_HINT_DIALOG);
    AWindow^.set_modal(True);
  end;
  TGtk3Widget(AWinControl.Handle).Visible := AWinControl.HandleObjectShouldBeVisible;
  if TGtk3Widget(AWinControl.Handle).Visible then
  begin
    if (fsModal in AForm.FormState) and (Application.ModalLevel > 0) then
    begin
      // DebugLn('TGtk3WSCustomForm.ShowHide ModalLevel=',dbgs(Application.ModalLevel),' Self=',dbgsName(AForm));
      if Application.ModalLevel > 1 then
      begin
        for i := 0 to Screen.CustomFormZOrderCount - 1 do
        begin
          // DebugLn('CustomFormZOrder[',dbgs(i),'].',dbgsName(Screen.CustomFormsZOrdered[i]),' modal=',
          //  dbgs(fsModal in Screen.CustomFormsZOrdered[i].FormState));
          if (Screen.CustomFormsZOrdered[i] <> AForm) and
            (fsModal in Screen.CustomFormsZOrdered[i].FormState) and
            Screen.CustomFormsZOrdered[i].HandleAllocated then
          begin
            // DebugLn('TGtk3WSCustomForm.ShowHide setTransient for ',dbgsName(Screen.CustomFormsZOrdered[i]));
            AWindow^.set_transient_for(PGtkWindow(TGtk3Window(Screen.CustomFormsZOrdered[i].Handle).Widget));
            break;
          end;
        end;
      end;
    end;
    AWindow^.show_all;
    AMask := AWindow^.window^.get_events;
    AWindow^.window^.set_events(GDK_ALL_EVENTS_MASK {AMask or GDK_POINTER_MOTION_MASK or GDK_POINTER_MOTION_HINT_MASK});
  end else
  begin
    if fsModal in AForm.FormState then
    begin
      if AWindow^.transient_for <> nil then
      begin
        // DebugLn('TGtk3WSCustomForm.ShowHide removetransientsient for ',dbgsName(AForm));
        AWindow^.set_transient_for(nil);
      end;
    end;
  end;
end;

class procedure TGtk3WSCustomForm.CloseModal(const ACustomForm: TCustomForm);
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.CloseModal');
  {$ENDIF}
end;

class procedure TGtk3WSCustomForm.SetAllowDropFiles(const AForm: TCustomForm;
  AValue: Boolean);
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.SetAllowDropFiles');
  {$ENDIF}
end;

class procedure TGtk3WSCustomForm.SetBorderIcons(const AForm: TCustomForm;
        const ABorderIcons: TBorderIcons);
begin
  if not WSCheckHandleAllocated(AForm, 'SetBorderIcons') then
    Exit;
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.SetBorderIcons');
  {$ENDIF}
end;

class procedure TGtk3WSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
begin
  if not WSCheckHandleAllocated(AForm, 'SetFormBorderStyle') then
    Exit;
  // will be done in interface override
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.SetFormBorderStyle');
  {$ENDIF}
end;

class procedure TGtk3WSCustomForm.SetFormStyle(const AForm: TCustomform;
  const AFormStyle, AOldFormStyle: TFormStyle);
begin
  if not WSCheckHandleAllocated(AForm, 'SetFormStyle') then
    Exit;
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.SetFormStyle');
  {$ENDIF}
end;
    
class procedure TGtk3WSCustomForm.SetIcon(const AForm: TCustomForm; const Small, Big: HICON);
begin
  if not WSCheckHandleAllocated(AForm, 'SetIcon') then
    Exit;
  if Big = 0 then
    TGtk3Window(AForm.Handle).Icon := Gtk3WidgetSet.AppIcon
  else
    TGtk3Window(AForm.Handle).Icon := TGtk3Image(Big).Handle;
end;

class procedure TGtk3WSCustomForm.SetShowInTaskbar(const AForm: TCustomForm;
  const AValue: TShowInTaskbar);
var
  AWindow: TGtk3Window;
  Enable: boolean;
begin
  if not WSCheckHandleAllocated(AForm, 'SetShowInTaskbar') then
    Exit;
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.SetShowInTaskbar');
  {$ENDIF}
  if (AForm.Parent <> nil) or
     (AForm.ParentWindow <> 0) or
     not (AForm.HandleAllocated) then Exit;
  AWindow := TGtk3Window(AForm.Handle);
  if not Gtk3IsGdkWindow(AWindow.Widget^.window) then
    exit;
  Enable := AValue <> stNever;
  if (not Enable) and AWindow.SkipTaskBarHint then
    AWindow.SkipTaskBarHint := False;
  AWindow.SkipTaskBarHint := not Enable;
end;

class procedure TGtk3WSCustomForm.SetZPosition(const AWinControl: TWinControl; const APosition: TWSZPosition);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetZPosition') then
    Exit;
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.SetZPosition');
  {$ENDIF}
end;

class function TGtk3WSCustomForm.GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor;
const
  DefColors: array[TDefaultColorType] of TColor = (
 { dctBrush } clForm,
 { dctFont  } clBtnText
  );
begin
  Result := DefColors[ADefaultColorType];
end;

class procedure TGtk3WSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
  if not WSCheckHandleAllocated(ACustomForm, 'ShowModal') then
    Exit;
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.ShowModal ... we are using ShowHide.');
  {$ENDIF}
end;

class procedure TGtk3WSCustomForm.SetRealPopupParent(
  const ACustomForm: TCustomForm; const APopupParent: TCustomForm);
begin
  if not WSCheckHandleAllocated(ACustomForm, 'SetRealPopupParent') then
    Exit;
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.SetRealPopupParent');
  {$ENDIF}
end;

class procedure TGtk3WSCustomForm.SetAlphaBlend(const ACustomForm: TCustomForm;
  const AlphaBlend: Boolean; const Alpha: Byte);
begin
  if not WSCheckHandleAllocated(ACustomForm, 'SetAlphaBlend') then
    Exit;
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.SetAlphaBlend');
  {$ENDIF}
end;

{ mdi support }

class function TGtk3WSCustomForm.ActiveMDIChild(const AForm: TCustomForm
  ): TCustomForm;
begin
  Result := nil;
end;

class function TGtk3WSCustomForm.Cascade(const AForm: TCustomForm): Boolean;
begin
  Result := False;
end;

class function TGtk3WSCustomForm.GetClientHandle(const AForm: TCustomForm): HWND;
begin
  Result := 0;
end;

class function TGtk3WSCustomForm.GetMDIChildren(const AForm: TCustomForm;
  AIndex: Integer): TCustomForm;
begin
  Result := nil;
end;

class function TGtk3WSCustomForm.MDIChildCount(const AForm: TCustomForm): Integer;
begin
  Result := 0;
end;

class function TGtk3WSCustomForm.Next(const AForm: TCustomForm): Boolean;
begin
  Result := False;
end;

class function TGtk3WSCustomForm.Previous(const AForm: TCustomForm): Boolean;
begin
  Result := False;
end;

class function TGtk3WSCustomForm.Tile(const AForm: TCustomForm): Boolean;
begin
  Result := False;
end;

{ TGtk3WSHintWindow }

class function TGtk3WSHintWindow.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TGtk3HintWindow.Create(AWinControl, AParams));
end;

end.
