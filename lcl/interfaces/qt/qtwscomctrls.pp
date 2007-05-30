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
  Classes, ComCtrls, Controls, LCLType,
  // Widgetset
  WSComCtrls, WSLCLClasses;

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
  end;

  { TQtWSToolBar }

  TQtWSToolBar = class(TWSToolBar)
  private
  protected
  public
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
//  RegisterWSComponent(TCustomToolButton, TQtWSToolButton);
//  RegisterWSComponent(TCustomToolBar, TQtWSToolBar);
//  RegisterWSComponent(TCustomToolButton, TQtWSToolButton);
//  RegisterWSComponent(TCustomToolBar, TQtWSToolBar);
  RegisterWSComponent(TCustomTrackBar, TQtWSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TQtWSCustomTreeView);
//  RegisterWSComponent(TCustomTreeView, TQtWSTreeView);
////////////////////////////////////////////////////
end.
