{ $Id$}
{
 *****************************************************************************
 *                              QtWSExtCtrls.pp                              * 
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
unit QtWSExtCtrls;

{$mode delphi}{$H+}

interface

uses
  // Bindings
{$ifdef USE_QT_4_3}
  qt43,
{$else}
  qt4,
{$endif}
  qtwidgets, qtobjects,
  // LCL
  LMessages, LCLMessageGlue,
  SysUtils, Classes, Controls, Graphics, Forms, StdCtrls, ExtCtrls, LCLType,
  // Widgetset
  WSExtCtrls, WSLCLClasses;

type

  { TQtWSCustomPage }

  TQtWSCustomPage = class(TWSCustomPage)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
//    class procedure UpdateProperties(const ACustomPage: TCustomPage); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
  end;

  { TQtWSCustomNotebook }

  TQtWSCustomNotebook = class(TWSCustomNotebook)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure AddAllNBPages(const ANotebook: TCustomNotebook);
{    class procedure AdjustSizeNotebookPages(const ANotebook: TCustomNotebook);}
    class procedure AddPage(const ANotebook: TCustomNotebook;
      const AChild: TCustomPage; const AIndex: integer); override;
{    class procedure MovePage(const ANotebook: TCustomNotebook;
      const AChild: TCustomPage; const NewIndex: integer); override;
    class procedure RemoveAllNBPages(const ANotebook: TCustomNotebook);
    class procedure RemovePage(const ANotebook: TCustomNotebook;
      const AIndex: integer); override;

    class function GetPageRealIndex(const ANotebook: TCustomNotebook; AIndex: Integer): Integer; override;
    class function GetTabIndexAtPos(const ANotebook: TCustomNotebook; const AClientPos: TPoint): integer; override;}
    class procedure SetPageIndex(const ANotebook: TCustomNotebook; const AIndex: integer); override;
    class procedure SetTabPosition(const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition); override;
    {class procedure ShowTabs(const ANotebook: TCustomNotebook; AShowTabs: boolean); override;}
  end;

  { TQtWSPage }

  TQtWSPage = class(TWSPage)
  private
  protected
  public
  end;

  { TQtWSNotebook }

  TQtWSNotebook = class(TWSNotebook)
  private
  protected
  public
  end;

  { TQtWSShape }

  TQtWSShape = class(TWSShape)
  private
  protected
  public
  end;

  { TQtWSCustomSplitter }

  TQtWSCustomSplitter = class(TWSCustomSplitter)
  private
  protected
  public
  end;

  { TQtWSSplitter }

  TQtWSSplitter = class(TWSSplitter)
  private
  protected
  public
  end;

  { TQtWSPaintBox }

  TQtWSPaintBox = class(TWSPaintBox)
  private
  protected
  public
  end;

  { TQtWSCustomImage }

  TQtWSCustomImage = class(TWSCustomImage)
  private
  protected
  public
  end;

  { TQtWSImage }

  TQtWSImage = class(TWSImage)
  private
  protected
  public
  end;

  { TQtWSBevel }

  TQtWSBevel = class(TWSBevel)
  private
  protected
  public
  end;

  { TQtWSCustomRadioGroup }

  TQtWSCustomRadioGroup = class(TWSCustomRadioGroup)
  private
  protected
  public
    class procedure ConstraintsChange(const AWinControl: TWinControl); override;
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
  end;

  { TQtWSRadioGroup }

  TQtWSRadioGroup = class(TWSRadioGroup)
  private
  protected
  public
  end;

  { TQtWSCustomCheckGroup }

  TQtWSCustomCheckGroup = class(TWSCustomCheckGroup)
  private
  protected
  public
    class procedure ConstraintsChange(const AWinControl: TWinControl); override;
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
  end;

  { TQtWSCheckGroup }

  TQtWSCheckGroup = class(TWSCheckGroup)
  private
  protected
  public
  end;

  { TQtWSCustomLabeledEdit }

  TQtWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  private
  protected
  public
  end;

  { TQtWSLabeledEdit }

  TQtWSLabeledEdit = class(TWSLabeledEdit)
  private
  protected
  public
  end;

  { TQtWSCustomPanel }

  TQtWSCustomPanel = class(TWSCustomPanel)
  private
  protected
  public
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
  end;

  { TQtWSPanel }

  TQtWSPanel = class(TWSPanel)
  private
  protected
  public
  end;


implementation

const
  QTabWidgetTabPositionMap: array[TTabPosition] of QTabWidgetTabPosition =
  (
{ tpTop    } QTabWidgetNorth,
{ tpBottom } QTabWidgetSouth,
{ tpLeft   } QTabWidgetWest,
{ tpRight  } QTabWidgetEast
  );

{ TQtWSCustomPanel }

{------------------------------------------------------------------------------
  Method: TQtWSCustomPanel.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TQtWSCustomPanel.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  QtFrame: TQtFrame;
begin
  QtFrame := TQtFrame.Create(AWinControl, AParams);
  QtFrame.AttachEvents;

  // Set´s initial properties

  QtFrame.setFrameShape(QFrameWinPanel);
  
  QtFrame.setFrameShadow(QFrameRaised);
  
  // Return the Handle

  Result := THandle(QtFrame);
end;

{ TQtWSCustomPage }

{------------------------------------------------------------------------------
  Method: TQtWSCustomPage.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TQtWSCustomPage.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  QtWidget: TQtWidget;
begin
  {$ifdef VerboseQt}
    WriteLn('Trace:> [TQtWSCustomPage.CreateHandle]');
  {$endif}

  QtWidget := TQtWidget.CreatePage(AWinControl, AParams);
  QtWidget.AttachEvents;

  // Returns the Handle
  Result := THandle(QtWidget);

  {$ifdef VerboseQt}
    WriteLn('Trace:< [TQtWSCustomPage.CreateHandle] Result: ', IntToStr(Result));
  {$endif}
end;

class procedure TQtWSCustomPage.SetText(const AWinControl: TWinControl; const AText: string);
var
  ANoteBook: TCustomNoteBook;
  Index: Integer;
  AWsText: WideString;
begin
  if (AWinControl is TCustomPage) and
     (AWinControl.Parent <> nil) and
     (AWinControl.Parent is TCustomNotebook) then
  begin
    ANoteBook := TCustomNotebook(AWinControl.Parent);
    Index := ANoteBook.IndexOf(TCustomPage(AWinControl));
    AWsText := UTF8Decode(AText);
    TQtTabWidget(ANoteBook.Handle).setTabText(Index, @AWsText);
  end;
end;

{ TQtWSCustomNotebook }

{------------------------------------------------------------------------------
  Method: TQtWSCustomNotebook.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TQtWSCustomNotebook.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  QtTabWidget: TQtTabWidget;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtWSCustomNotebook.CreateHandle');
  {$endif}

  QtTabWidget := TQtTabWidget.Create(AWinControl, AParams);
  QtTabWidget.SetTabPosition(QTabWidgetTabPositionMap[TCustomNoteBook(AWinControl).TabPosition]);
  QtTabWidget.AttachEvents;

  // Returns the Handle

  Result := THandle(QtTabWidget);
end;

class procedure TQtWSCustomNotebook.AddAllNBPages(const ANotebook: TCustomNotebook);
begin

end;

class procedure TQtWSCustomNotebook.AddPage(const ANotebook: TCustomNotebook;
  const AChild: TCustomPage; const AIndex: integer);
var
  Str: WideString;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtWSCustomNotebook.AddPage');
  {$endif}

  Str := UTF8Decode(AChild.Caption);

  TQtTabWidget(ANotebook.Handle).insertTab(AIndex, TQtWidget(AChild.Handle).Widget, @Str);
end;

class procedure TQtWSCustomNotebook.SetPageIndex(
  const ANotebook: TCustomNotebook; const AIndex: integer);
var
  TabWidget: TQtTabWidget;
  OldIndex: Integer;
begin
  TabWidget := TQtTabWidget(ANotebook.Handle);

  // copy pasted from win32wsextctrls with corrections

  OldIndex := TabWidget.getCurrentIndex;
  TabWidget.setCurrentIndex(AIndex);
  if not (csDestroying in ANotebook.ComponentState) then
  begin
    if (OldIndex >= 0) and (OldIndex <> AIndex) and
       (OldIndex < ANotebook.PageCount) and
       (ANotebook.CustomPage(OldIndex).HandleAllocated) then
      TQtWidget(ANotebook.CustomPage(OldIndex).Handle).setVisible(False);
  end;
end;

class procedure TQtWSCustomNotebook.SetTabPosition(
  const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition);
begin
  TQtTabWidget(ANotebook.Handle).SetTabPosition(QTabWidgetTabPositionMap[ATabPosition]);
end;

{ TQtWSCustomRadioGroup }

{------------------------------------------------------------------------------
  Method: TQtWSCustomRadioGroup.ContstraintsChange
  Params:  None
  Returns: Nothing

 ------------------------------------------------------------------------------}
class procedure TQtWSCustomRadioGroup.ConstraintsChange(const AWinControl: TWinControl);
var
  R: TRect;
  QtWidget: TQtWidget;
  i: Integer;
  w: QWidgetH;
begin

  QtWidget := TQtWidget(AWinControl.Handle);
  QWidget_contentsRect(QtWidget.Widget, @R);

  if (R.Left<>0) or (R.Top<>0) then
  begin
    w := QtWidget.GetContainerWidget;
    QWidget_contentsRect(QtWidget.Widget, @R);
    {TODO: we should recalculate sizes if we have items.Count
     each button have 23px height by default, don't forget to include
     HiddenRadioButton size ! }
    if (w <> QtWidget.Widget) and (R.Top > 0) then
    begin
      if TCustomRadioGroup(AWinControl).Items.Count > 0 then
      begin
        R.Top := R.Top + 1;
        R.Top := (R.Top div 4) + TCustomRadioGroup(AWinControl).Columns;
        R.Bottom := R.Bottom + R.Top;
      end else
      begin
        R.Top := R.Top + 1;
        R.Top := (R.Top div (TCustomRadioGroup(AWinControl).Columns));
      end;
      QWidget_setGeometry(w, @R);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomRadioGroup.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}

class function TQtWSCustomRadioGroup.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtGroupBox: TQtGroupBox;
  Str: WideString;
begin
  QtGroupBox := TQtGroupBox.Create(AWinControl, AParams);

  Str := UTF8Decode(AWinControl.Caption);
  QGroupBox_setTitle(QGroupBoxH(QtGroupBox.Widget), @Str);

  QtGroupBox.AttachEvents;

  Result := THandle(QtGroupBox);
end;

class procedure TQtWSCustomRadioGroup.ShowHide(const AWinControl: TWinControl);
var
  i: Integer;
  ATextWidth: Integer;
  ATextHeight: Integer;
  FM: QFontMetricsH;
  Str: WideString;
begin
  inherited ShowHide(AWinControl);
  {without this we have invisible radio buttons}
  {only this guarantee that our items are visible anytime ....}
  for i := 0 to TRadioGroup(AWinControl).ComponentCount - 1 do
  begin
    if TRadioButton(AWinControl.Components[i]).Height = 0 then
    begin
      FM := QFontMetrics_create(QWidget_font(TQtRadioButton(TRadioButton(AWinControl.Components[i]).Handle).Widget));
      try
        Str := UTF8Encode(TRadioButton(AWinControl.Components[i]).Caption);
        ATextWidth := QFontMetrics_width(FM, @Str, Length(Str));
        ATextHeight := QFontMetrics_height(FM);
      finally
        QFontMetrics_destroy(FM);
      end;
      
      { now, textwidth + default width of checkbox, default height
        qt doesn't align well control with text size < 100}
      if ATextWidth < 100 then
        ATextWidth := 100;
        
      TRadioButton(AWinControl.Components[i]).SetBounds(0, 0, ATextWidth + ATextHeight + 1, ATextHeight);
    end;
  end;
end;
 
{ TQtWSCustomCheckGroup }

{------------------------------------------------------------------------------
  Method: TQtWSCustomCheckGroup.ContstraintsChange
  Params:  None
  Returns: Nothing

 ------------------------------------------------------------------------------}
class procedure TQtWSCustomCheckGroup.ConstraintsChange(const AWinControl: TWinControl);
var
  R: TRect;
  QtWidget: TQtWidget;
  i: Integer;
  w: QWidgetH;
begin

  QtWidget := TQtWidget(AWinControl.Handle);
  QWidget_contentsRect(QtWidget.Widget, @R);

  if (R.Left<>0) or (R.Top<>0) then
  begin
    w := QtWidget.GetContainerWidget;
    QWidget_contentsRect(QtWidget.Widget, @R);

    if (w <> QtWidget.Widget) and (R.Top > 0) then
    begin
			if TCustomCheckGroup(AWinControl).Items.Count > 0
   		then
		    R.Bottom := R.Bottom - R.Top
      else
        R.Top := (R.Top div 2) - 2;

      QWidget_setGeometry(w, @R);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomCheckGroup.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TQtWSCustomCheckGroup.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtGroupBox: TQtGroupBox;
  Str: WideString;
begin
  QtGroupBox := TQtGroupBox.Create(AWinControl, AParams);
  
  Str := UTF8Decode(AWinControl.Caption);
  QGroupBox_setTitle(QGroupBoxH(QtGroupBox.Widget), @Str);

  QtGroupBox.AttachEvents;

  Result := THandle(QtGroupBox);
end;

class procedure TQtWSCustomCheckGroup.ShowHide(const AWinControl: TWinControl);
var
  i: Integer;
  ATextWidth: Integer;
  ATextHeight: Integer;
  FM: QFontMetricsH;
  Str: WideString;
begin
  {without this checkboxes are invisible}
  inherited ShowHide(AWinControl);
  
  {only this guarantee that our items are visible anytime ....}
  for i := 0 to TCheckGroup(AWinControl).ComponentCount - 1 do
  begin
    if TCheckBox(AWinControl.Components[i]).Height = 0 then
    begin
      FM := QFontMetrics_create(QWidget_font(TQtCheckBox(TCheckBox(AWinControl.Components[i]).Handle).Widget));
      try
        Str := UTF8Encode(TCheckBox(AWinControl.Components[i]).Caption);
        ATextWidth := QFontMetrics_width(FM, @Str, Length(Str));
        ATextHeight := QFontMetrics_height(FM);
      finally
        QFontMetrics_destroy(FM);
      end;
      
      { now, textwidth + default width of checkbox, default height
        qt doesn't align well control with text size < 100}
      if ATextWidth < 100 then
        ATextWidth := 100;
        
      TCheckBox(AWinControl.Components[i]).SetBounds(0, 0, ATextWidth + ATextHeight + 1, ATextHeight);
    end;
  end;
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomPage, TQtWSCustomPage);
  RegisterWSComponent(TCustomNotebook, TQtWSCustomNotebook);
//  RegisterWSComponent(TPage, TQtWSPage);
//  RegisterWSComponent(TNotebook, TQtWSNotebook);
//  RegisterWSComponent(TShape, TQtWSShape);
//  RegisterWSComponent(TCustomSplitter, TQtWSCustomSplitter);
//  RegisterWSComponent(TSplitter, TQtWSSplitter);
//  RegisterWSComponent(TPaintBox, TQtWSPaintBox);
//  RegisterWSComponent(TCustomImage, TQtWSCustomImage);
//  RegisterWSComponent(TImage, TQtWSImage);
//  RegisterWSComponent(TBevel, TQtWSBevel);
  RegisterWSComponent(TCustomRadioGroup, TQtWSCustomRadioGroup);
//  RegisterWSComponent(TRadioGroup, TQtWSRadioGroup);
  RegisterWSComponent(TCustomCheckGroup, TQtWSCustomCheckGroup);
//  RegisterWSComponent(TCheckGroup, TQtWSCheckGroup);
//  RegisterWSComponent(TCustomLabeledEdit, TQtWSCustomLabeledEdit);
//  RegisterWSComponent(TLabeledEdit, TQtWSLabeledEdit);
  RegisterWSComponent(TCustomPanel, TQtWSCustomPanel);
//  RegisterWSComponent(TPanel, TQtWSPanel);
////////////////////////////////////////////////////
end.
