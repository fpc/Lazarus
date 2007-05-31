{ $Id$}
{
 *****************************************************************************
 *                              QtWSComCtrls.pp                              * 
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit QtWSComCtrls;

{$mode delphi}{$H+}

interface

uses
  // Bindings
  qt4, qtwidgets,
  // LCL
  Classes, ComCtrls, Controls, LCLType, Graphics,
  // Widgetset
  WSProc, WSComCtrls, WSLCLClasses;

type

  { TQtWSStatusBar }

  TQtWSStatusBar = class(TWSStatusBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure Update(const AStatusBar: TStatusBar); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
     var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
  end;

  { TQtWSTabSheet }

  TQtWSTabSheet = class(TWSTabSheet)
  private
  protected
  public
  end;

  { TQtWSPageControl }

  TQtWSPageControl = class(TWSPageControl)
  private
  protected
  public
  end;

  { TQtWSCustomListView }

  TQtWSCustomListView = class(TWSCustomListView)
  private
  protected
  public
  end;

  { TQtWSListView }

  TQtWSListView = class(TWSListView)
  private
  protected
  public
  end;

  { TQtWSProgressBar }

  TQtWSProgressBar = class(TWSProgressBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
  end;

  { TQtWSCustomUpDown }

  TQtWSCustomUpDown = class(TWSCustomUpDown)
  private
  protected
  public
  end;

  { TQtWSUpDown }

  TQtWSUpDown = class(TWSUpDown)
  private
  protected
  public
  end;

  { TQtWSToolButton }

  TQtWSToolButton = class(TWSToolButton)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
  end;

  { TQtWSToolBar }

  TQtWSToolBar = class(TWSToolBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
  end;

  { TQtWSTrackBar }

  TQtWSTrackBar = class(TWSTrackBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure ApplyChanges(const ATrackBar: TCustomTrackBar); override;
    class function  GetPosition(const ATrackBar: TCustomTrackBar): integer; override;
    class procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer); override;
  end;

  { TQtWSCustomTreeView }

  TQtWSCustomTreeView = class(TWSCustomTreeView)
  private
  protected
  public
  end;

  { TQtWSTreeView }

  TQtWSTreeView = class(TWSTreeView)
  private
  protected
  public
  end;


implementation


{ TQtWSToolButton }

class function TQtWSToolButton.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
   QtToolButton: TQtToolButton;
   Hook: QToolButton_hookH;
   Method: TMethod;
begin
  QtToolButton := TQtToolButton.Create(AWinControl, AParams);

  Hook := QToolButton_hook_create(QtToolButton.Widget);
  TEventFilterMethod(Method) := QtToolButton.EventFilter;
  QObject_hook_hook_events(Hook, Method);
  
  Result := THandle(QtToolButton);
end;

{------------------------------------------------------------------------------
  Method: TQtWSToolButton.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSToolButton.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtToolButton(AWinControl.Handle).Free;
  
  AWinControl.Handle := 0;
end;

{------------------------------------------------------------------------------
  Method: TQtWSToolButton.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSToolButton.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  Str: WideString;
begin
  TQtToolButton(AWinControl.Handle).Text(@Str);

  AText := UTF8Encode(Str);

  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TQtWSToolButton.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSToolButton.SetText(const AWinControl: TWinControl; const AText: String);
var
  Str: WideString;
begin
  Str := UTF8Decode(AText);

  TQtToolButton(AWinControl.Handle).SetText(@Str);
end;

{------------------------------------------------------------------------------
  Method: TQtWSToolButton.SetColor
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSToolButton.SetColor(const AWinControl: TWinControl);
var
  QColor: TQColor;
  Color: TColor;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetColor')
  then Exit;

  if AWinControl.Color = CLR_INVALID then exit;

  // Get the color numeric value (system colors are mapped to numeric colors depending on the widget style)
  Color:=ColorToRGB(AWinControl.Color);

  // Fill QColor
  QColor_setRgb(@QColor,Red(Color),Green(Color),Blue(Color));

  // Set color of the widget to QColor
  TQtAbstractButton(AWinControl.Handle).SetColor(@QColor);
end;

{ TQtWSToolBar }

class function TQtWSToolBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
   QtToolBar: TQtToolBar;
   Hook: QToolBar_hookH;
   Method: TMethod;
begin
  QtToolBar := TQtToolBar.Create(AWinControl, AParams);

  Hook := QToolBar_hook_create(QtToolBar.Widget);
  TEventFilterMethod(Method) := QtToolBar.EventFilter;
  QObject_hook_hook_events(Hook, Method);
     
  Result := THandle(QtToolBar);
end;

{------------------------------------------------------------------------------
  Method: TQtWSToolBar.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSToolBar.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtToolBar(AWinControl.Handle).Free;
  AWinControl.Handle := 0;
end;


{------------------------------------------------------------------------------
  Method: TQtWSToolBar.SetColor
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSToolBar.SetColor(const AWinControl: TWinControl);
var
  QColor: TQColor;
  Color: TColor;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetColor')
  then Exit;

  if AWinControl.Color = CLR_INVALID then exit;

  // Get the color numeric value (system colors are mapped to numeric colors depending on the widget style)
  Color:=ColorToRGB(AWinControl.Color);

  // Fill QColor
  QColor_setRgb(@QColor,Red(Color),Green(Color),Blue(Color));

  // Set color of the widget to QColor
  TQtToolBar(AWinControl.Handle).SetColor(@QColor);
end;

{ TQtWSTrackBar }

class function TQtWSTrackBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  QtTrackBar: TQtTrackBar;
  Method: TMethod;
  Hook : QSlider_hookH;
begin
  QtTrackBar := TQtTrackBar.Create(AWinControl, AParams);

  Hook := QSlider_hook_create(QtTrackBar.Widget);
  TEventFilterMethod(Method) := QtTrackBar.EventFilter;
  QObject_hook_hook_events(Hook, Method);

  QAbstractSlider_rangeChanged_Event(Method) := QtTrackbar.SlotRangeChanged;
  QAbstractSlider_hook_hook_rangeChanged(QAbstractSlider_hook_create(QtTrackBar.Widget), Method);

  QAbstractSlider_sliderMoved_Event(Method) := QtTrackBar.SlotSliderMoved;
  QAbstractSlider_hook_hook_sliderMoved(QAbstractSlider_hook_create(QtTrackBar.Widget), Method);

  QAbstractSlider_sliderPressed_Event(Method) := QtTrackBar.SlotSliderPressed;
  QAbstractSlider_hook_hook_sliderPressed(QAbstractSlider_hook_create(QtTrackBar.Widget), Method);

  QAbstractSlider_sliderReleased_Event(Method) := QtTrackBar.SlotSliderReleased;
  QAbstractSlider_hook_hook_sliderReleased(QAbstractSlider_hook_create(QtTrackBar.Widget), Method);

  QAbstractSlider_valueChanged_Event(Method) := QtTrackBar.SlotValueChanged;
  QAbstractSlider_hook_hook_valueChanged(QAbstractSlider_hook_create(QtTrackBar.Widget), Method);

  Result := THandle(QtTrackBar);
end;

class procedure TQtWSTrackBar.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtTrackBar(AWinControl.Handle).Free;
  AWinControl.Handle := 0;
end;

class procedure TQtWSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
var
  QtTrackBar: TQtTrackBar;
begin

  QtTrackBar := TQtTrackBar(ATrackBar.Handle);

  QtTrackBar.setRange(ATrackBar.Min, ATrackBar.Max);

  case ATrackBar.TickMarks of
    tmBoth:QtTrackBar.SetTickPosition(QSliderTicksBothSides);
    tmTopLeft:QtTrackBar.SetTickPosition(QSliderTicksAbove);
    tmBottomRight:QtTrackBar.SetTickPosition(QSliderTicksBelow);
  end;

  if QAbstractSlider_pageStep(QAbstractSliderH(QtTrackBar.Widget)) <> ATrackBar.PageSize then
    QtTrackBar.setPageStep(ATrackBar.PageSize);
  if QSlider_tickInterval(QSliderH(QtTrackBar.Widget)) <> ATrackBar.Frequency then
    QtTrackBar.setTickInterval(ATrackBar.Frequency);
  if QAbstractSlider_value(QAbstractSliderH(QtTrackBar.Widget)) <> ATrackBar.Position then
    QtTrackBar.setSliderPosition(ATrackBar.Position);

  case ATrackBar.Orientation of
    trVertical:
    begin
      QtTrackBar.Hide;
      QtTrackBar.setOrientation(QtVertical);
      QtTrackBar.setInvertedAppereance(False);
      QtTrackBar.setInvertedControls(False);
      QtTrackBar.Show;
    end;
    else {trHorizontal}
    begin
      QtTrackBar.Hide;
      QtTrackBar.setOrientation(QtHorizontal);
      QtTrackBar.setInvertedAppereance(False);
      QtTrackBar.setInvertedControls(False);
      QtTrackBar.Show;
    end;
  end;
end;

class function  TQtWSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar): integer;
var
  QtTrackBar: TQtTrackBar;
begin
  QtTrackBar := TQtTrackBar(ATrackBar.Handle);
  Result := QAbstractSlider_value(QAbstractSliderH(QtTrackBar.Widget));
end;

class procedure TQtWSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer);
var
  QtTrackBar: TQtTrackBar;
begin
  QtTrackBar := TQtTrackBar(ATrackBar.Handle);
  QtTrackBar.setValue(NewPosition);
end;

{ TQtWSProgressBar }

class function TQtWSProgressBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  QtProgressBar: TQtProgressBar;
{  Method: TMethod;
  Hook : QObject_hookH;}
begin
  QtProgressBar := TQtProgressBar.Create(AWinControl, AParams);

  // Various Events

{  Hook := QObject_hook_create(QtWidget.Widget);

  TEventFilterMethod(Method) := QtWidget.EventFilter;

  QObject_hook_hook_events(Hook, Method);}

  Result := THandle(QtProgressBar);
end;

class procedure TQtWSProgressBar.DestroyHandle(const AWinControl: TWinControl);
begin
//  TQtWidget(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

class procedure TQtWSProgressBar.ApplyChanges(const AProgressBar: TCustomProgressBar);
var
  QtProgressBar: TQtProgressBar;
begin
  QtProgressBar := TQtProgressBar(AProgressBar.Handle);

//  if AProgressBar.Smooth then
//    gtk_progress_bar_set_bar_style (GTK_PROGRESS_BAR(Pointer(Pointer(wHandle))),
//                                         GTK_PROGRESS_CONTINUOUS)
//   else gtk_progress_bar_set_bar_style (GTK_PROGRESS_BAR(Pointer(Pointer(wHandle))),
//                                         GTK_PROGRESS_DISCRETE);

  case AProgressBar.Orientation of
    pbVertical:
    begin
      QtProgressBar.setOrientation(QtVertical);
      QtProgressBar.setInvertedAppearance(False);
    end;
    pbRightToLeft:
    begin
      QtProgressBar.setOrientation(QtHorizontal);
      QtProgressBar.setInvertedAppearance(True);
    end;
    pbTopDown:
    begin
      QtProgressBar.setOrientation(QtVertical);
      QtProgressBar.setInvertedAppearance(True);
    end;
  else { pbHorizontal is default }
    QtProgressBar.setOrientation(QtHorizontal);
    QtProgressBar.setInvertedAppearance(False);
  end;

  if AProgressBar.BarShowText then
  begin
//       gtk_progress_set_format_string (GTK_PROGRESS(Pointer(Pointer(wHandle))),
//                                       '%v from [%l-%u] (=%p%%)');

    QtProgressBar.setTextVisible(True);
  end
  else QtProgressBar.setTextVisible(False);

  // The position, minumum and maximum values
  QtProgressBar.setValue(AProgressBar.Position);

  QtProgressBar.setRange(AProgressBar.Min, AProgressBar.Max);
end;

class procedure TQtWSProgressBar.SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  TQtProgressBar(AProgressBar.Handle).setValue(NewPosition);
end;

{ TQtWSStatusBar }

class function TQtWSStatusBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  QtStatusBar: TQtStatusBar;
  Method: TMethod;
  Hook : QStatusBar_hookH;
begin
  QtStatusBar := TQtStatusBar.Create(AWinControl, AParams);

  // Various Events

  Hook := QStatusBar_hook_create(QtStatusBar.Widget);

  TEventFilterMethod(Method) := QtStatusBar.EventFilter;

  QObject_hook_hook_events(Hook, Method);

  // Return handle

  Result := THandle(QtStatusBar);
end;

class procedure TQtWSStatusBar.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtWidget(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

class procedure TQtWSStatusBar.PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer);
begin

end;

class procedure TQtWSStatusBar.SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer);
var
  Text: Widestring;
begin
  Text := UTF8Decode(AStatusBar.SimpleText);
  TQtStatusBar(AStatusBar.Handle).showMessage(@Text);
end;

class procedure TQtWSStatusBar.Update(const AStatusBar: TStatusBar);
begin

end;

class procedure TQtWSStatusBar.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin

end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TStatusBar, TQtWSStatusBar);
//  RegisterWSComponent(TCustomTabSheet, TQtWSTabSheet);
//  RegisterWSComponent(TCustomPageControl, TQtWSPageControl);
//  RegisterWSComponent(TCustomListView, TQtWSCustomListView);
//  RegisterWSComponent(TCustomListView, TQtWSListView);
  RegisterWSComponent(TCustomProgressBar, TQtWSProgressBar);
//  RegisterWSComponent(TCustomUpDown, TQtWSCustomUpDown);
//  RegisterWSComponent(TCustomUpDown, TQtWSUpDown);
  RegisterWSComponent(TToolButton, TQtWSToolButton);
  RegisterWSComponent(TToolBar, TQtWSToolBar);
//  RegisterWSComponent(TCustomToolButton, TQtWSToolButton);
//  RegisterWSComponent(TCustomToolBar, TQtWSToolBar);
  RegisterWSComponent(TCustomTrackBar, TQtWSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TQtWSCustomTreeView);
//  RegisterWSComponent(TCustomTreeView, TQtWSTreeView);
////////////////////////////////////////////////////
end.
