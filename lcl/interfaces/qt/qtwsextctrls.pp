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
  SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, LCLType,
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
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
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
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
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
    class function GetTabIndexAtPos(const ANotebook: TCustomNotebook; const AClientPos: TPoint): integer; override;
    class procedure SetPageIndex(const ANotebook: TCustomNotebook; const AIndex: integer); override;}
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
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
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
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
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
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
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
  Method: TMethod;
  Hook : QObject_hookH;
begin
  QtFrame := TQtFrame.Create(AWinControl, AParams);

  // Various Events

  Hook := QObject_hook_create(QtFrame.Widget);

  TEventFilterMethod(Method) := QtFrame.EventFilter;

  QObject_hook_hook_events(Hook, Method);

  // Set´s initial properties

  QtFrame.setFrameShape(QFrameWinPanel);
  
  QtFrame.setFrameShadow(QFrameRaised);
  
  // Return the Handle

  Result := THandle(QtFrame);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomGroupBox.DestroyHandle
  Params:  None
  Returns: Nothing

  Releases allocated memory and resources
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomPanel.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtFrame(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
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
  Method: TMethod;
  Hook : QTabBar_hookH;
begin
  {$ifdef VerboseQt}
    WriteLn('Trace:> [TQtWSCustomPage.CreateHandle]');
  {$endif}

  QtWidget := TQtWidget.CreatePage(AWinControl, AParams);

  // Various Events

  Hook := QTabBar_hook_create(QtWidget.Widget);

  TEventFilterMethod(Method) := QtWidget.EventFilter;

  QObject_hook_hook_events(Hook, Method);

  // Returns the Handle

  Result := THandle(QtWidget);

  {$ifdef VerboseQt}
    WriteLn('Trace:< [TQtWSCustomPage.CreateHandle] Result: ', IntToStr(Result));
  {$endif}
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomPage.DestroyHandle
  Params:  None
  Returns: Nothing

  Releases allocated memory and resources
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomPage.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtWidget(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

class procedure TQtWSCustomPage.SetText(const AWinControl: TWinControl;
  const AText: string);
begin
  inherited SetText(AWinControl, AText);
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
  Method: TMethod;
  Hook : QTabWidget_hookH;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtWSCustomNotebook.CreateHandle');
  {$endif}

  QtTabWidget := TQtTabWidget.Create(AWinControl, AParams);
  QtTabWidget.SetTabPosition(QTabWidgetTabPositionMap[TCustomNoteBook(AWinControl).TabPosition]);

  // Various Events

  Hook := QTabWidget_hook_create(QtTabWidget.Widget);


  TEventFilterMethod(Method) := QtTabWidget.EventFilter;

  QObject_hook_hook_events(Hook, Method);
  QTabWidget_currentChanged_Event(Method) := QtTabWidget.SignalCurrentChanged;
  QTabWidget_hook_hook_currentChanged(QTabWidget_hook_create(QtTabWidget.Widget), Method);

  // Returns the Handle

  Result := THandle(QtTabWidget);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomNotebook.DestroyHandle
  Params:  None
  Returns: Nothing

  Releases allocated memory and resources
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomNotebook.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtTabWidget(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
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

class procedure TQtWSCustomNotebook.SetTabPosition(
  const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition);
begin
  TQtTabWidget(ANotebook.Handle).SetTabPosition(QTabWidgetTabPositionMap[ATabPosition]);
end;

{ TQtWSCustomRadioGroup }

{------------------------------------------------------------------------------
  Method: TQtWSCustomRadioGroup.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}

class function TQtWSCustomRadioGroup.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtGroupBox: TQtButtonGroupBox;
  Str: WideString;
  Method: TMethod;
  Hook : QGroupBox_hookH;
begin

  QtGroupBox := TQtButtonGroupBox.Create(AWinControl, AParams);

  QtGroupBox.VBoxLayout := QVBoxLayout_create;
  QWidget_setLayout(QtGroupBox.Widget, QtGroupBox.VBoxLayout);
  QtGroupBox.ButtonGroup := TQtButtonGroup.Create(QObjectH(QtGroupBox.VBoxLayout));

  Hook := QGroupBox_hook_create(QtGroupBox.Widget);
  TEventFilterMethod(Method) := QtGroupBox.EventFilter;
  QObject_hook_hook_events(Hook, Method);

  Str := UTF8Decode(AWinControl.Caption);
  QGroupBox_setTitle(QGroupBoxH(QtGroupBox.Widget), @Str);

  Result := THandle(QtGroupBox);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomRadioGroup.DestroyHandle
  Params:  None
  Returns: Nothing

  Releases allocated memory and resources
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomRadioGroup.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtGroupBox(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;


class procedure TQtWSCustomRadioGroup.ShowHide(const AWinControl: TWinControl);
begin
  inherited ShowHide(AWinControl);
  {without this we have invisible radio buttons}
end;
 
{ TQtWSCustomCheckGroup }

{------------------------------------------------------------------------------
  Method: TQtWSCustomCheckGroup.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}

class function TQtWSCustomCheckGroup.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtGroupBox: TQtButtonGroupBox;
  Str: WideString;
  Method: TMethod;
  Hook : QGroupBox_hookH;
begin

  QtGroupBox := TQtButtonGroupBox.Create(AWinControl, AParams);
  QtGroupBox.VBoxLayout := QVBoxLayout_create;
  QWidget_setLayout(QtGroupBox.Widget, QtGroupBox.VBoxLayout);
  QtGroupBox.ButtonGroup := TQtButtonGroup.Create(QObjectH(QtGroupBox.VBoxLayout));

  Hook := QGroupBox_hook_create(QtGroupBox.Widget);
  TEventFilterMethod(Method) := QtGroupBox.EventFilter;
  QObject_hook_hook_events(Hook, Method);

  Str := UTF8Decode(AWinControl.Caption);
  QGroupBox_setTitle(QGroupBoxH(QtGroupBox.Widget), @Str);

  Result := THandle(QtGroupBox);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomCheckGroup.DestroyHandle
  Params:  None
  Returns: Nothing

  Releases allocated memory and resources
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomCheckGroup.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtGroupBox(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;


class procedure TQtWSCustomCheckGroup.ShowHide(const AWinControl: TWinControl);
begin
  inherited ShowHide(AWinControl);
  {without this checkboxes are invisible}
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
