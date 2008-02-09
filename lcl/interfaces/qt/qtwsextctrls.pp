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

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Bindings
  qt4,
  qtwidgets, qtobjects, qtproc,
  // LCL
  LMessages, LCLMessageGlue,
  SysUtils, Classes, Controls, Graphics, Forms, StdCtrls, ExtCtrls, LCLType,
  ImgList,
  // Widgetset
  WSExtCtrls, WSLCLClasses;

type

  { TQtWSCustomPage }

  TQtWSCustomPage = class(TWSCustomPage)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure UpdateProperties(const ACustomPage: TCustomPage); override;
  end;

  { TQtWSCustomNotebook }

  TQtWSCustomNotebook = class(TWSCustomNotebook)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;

    class procedure AddPage(const ANotebook: TCustomNotebook;
      const AChild: TCustomPage; const AIndex: integer); override;
    class procedure MovePage(const ANotebook: TCustomNotebook;
      const AChild: TCustomPage; const NewIndex: integer); override;
    class procedure RemovePage(const ANotebook: TCustomNotebook;
      const AIndex: integer); override;

    class function GetTabIndexAtPos(const ANotebook: TCustomNotebook; const AClientPos: TPoint): integer; override;
    class procedure SetPageIndex(const ANotebook: TCustomNotebook; const AIndex: integer); override;
    class procedure SetTabCaption(const ANotebook: TCustomNotebook; const AChild: TCustomPage; const AText: string); override;
    class procedure SetTabPosition(const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition); override;
    class procedure ShowTabs(const ANotebook: TCustomNotebook; AShowTabs: boolean); override;
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
          const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TQtWSPanel }

  TQtWSPanel = class(TWSPanel)
  private
  protected
  public
  end;

  { TQtWSCustomTrayIcon }

  TQtWSCustomTrayIcon = class(TWSCustomTrayIcon)
  public
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
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
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtFrame: TQtFrame;
begin
  QtFrame := TQtFrame.Create(AWinControl, AParams);
  QtFrame.AttachEvents;

  // Set's initial properties

  QtFrame.setFrameShape(QFrameNoFrame);
  
  // Return the Handle

  Result := TLCLIntfHandle(QtFrame);
end;

{ TQtWSCustomPage }

{------------------------------------------------------------------------------
  Method: TQtWSCustomPage.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TQtWSCustomPage.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtPage: TQtPage;
begin
  {$ifdef VerboseQt}
    WriteLn('Trace:> [TQtWSCustomPage.CreateHandle]');
  {$endif}

  QtPage := TQtPage.Create(AWinControl, AParams);
  QtPage.AttachEvents;

  // Returns the Handle
  Result := TLCLIntfHandle(QtPage);

  {$ifdef VerboseQt}
    WriteLn('Trace:< [TQtWSCustomPage.CreateHandle] Result: ', IntToStr(Result));
  {$endif}
end;

class procedure TQtWSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
var
  ImageList: TCustomImageList;
  Bmp: TBitmap;
begin
  ImageList := TCustomNoteBook(ACustomPage.Parent).Images;

  if Assigned(ImageList) and (ACustomPage.ImageIndex >= 0) and
     (ACustomPage.ImageIndex < ImageList.Count) then
  begin
    Bmp := TBitmap.Create;
    try
      ImageList.GetBitmap(ACustomPage.ImageIndex, Bmp);
      TQtPage(ACustomPage.Handle).setIcon(TQtImage(Bmp.Handle).AsIcon);
    finally
      Bmp.Free;
    end;
  end;
end;

{ TQtWSCustomNotebook }

{------------------------------------------------------------------------------
  Method: TQtWSCustomNotebook.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TQtWSCustomNotebook.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
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

  Result := TLCLIntfHandle(QtTabWidget);
end;

class procedure TQtWSCustomNotebook.AddPage(const ANotebook: TCustomNotebook;
  const AChild: TCustomPage; const AIndex: integer);
begin
  {$ifdef VerboseQt}
    WriteLn('TQtWSCustomNotebook.AddPage');
  {$endif}
  TQtTabWidget(ANotebook.Handle).insertTab(AIndex, TQtPage(AChild.Handle).Widget,
    GetUtf8String(AChild.Caption));
  TQtWsCustomPage.UpdateProperties(AChild);
end;

class procedure TQtWSCustomNotebook.MovePage(const ANotebook: TCustomNotebook;
  const AChild: TCustomPage; const NewIndex: integer);
var
  TabWidget: TQtTabWidget;
  AIndex: Integer;
  Page: TQtPage;
begin
  Page := TQtPage(AChild.Handle);
  TabWidget := TQtTabWidget(ANotebook.Handle);
  AIndex := ANoteBook.IndexOf(AChild);
  TabWidget.setUpdatesEnabled(false);
  TabWidget.removeTab(AIndex);
  TabWidget.insertTab(NewIndex, Page.Widget, Page.getIcon, Page.getText);
  TabWidget.setUpdatesEnabled(true);
end;

class procedure TQtWSCustomNotebook.RemovePage(const ANotebook: TCustomNotebook;
  const AIndex: integer);
var
  TabWidget: TQtTabWidget;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtWSCustomNotebook.RemovePage');
  {$endif}
  TabWidget := TQtTabWidget(ANotebook.Handle);
  TabWidget.removeTab(AIndex);
end;

class function TQtWSCustomNotebook.GetTabIndexAtPos(
  const ANotebook: TCustomNotebook; const AClientPos: TPoint): integer;
var
  TabWidget: TQtTabWidget;
  APoint: TQtPoint;
begin
  TabWidget := TQtTabWidget(ANotebook.Handle);
  if TabWidget.TabBar <> nil then
  begin
    APoint := QtPoint(AClientPos.x, AClientPos.y);

    Result := QTabBar_tabAt(TabWidget.TabBar, @APoint);
  end
  else
    Result := -1;
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

class procedure TQtWSCustomNotebook.SetTabCaption(
  const ANotebook: TCustomNotebook; const AChild: TCustomPage;
  const AText: string);
begin
  TQtTabWidget(ANotebook.Handle).setTabText(ANoteBook.IndexOf(AChild), GetUtf8String(AText));
end;

class procedure TQtWSCustomNotebook.SetTabPosition(
  const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition);
begin
  TQtTabWidget(ANotebook.Handle).SetTabPosition(QTabWidgetTabPositionMap[ATabPosition]);
end;

class procedure TQtWSCustomNotebook.ShowTabs(const ANotebook: TCustomNotebook;
  AShowTabs: boolean);
var
  TabWidget: TQtTabWidget;
begin
  TabWidget := TQtTabWidget(ANotebook.Handle);
  if TabWidget.TabBar <> nil then
    QWidget_setVisible(TabWidget.TabBar, AShowTabs);
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
  QtGroupBox: TQtGroupBox;
  Str: WideString;
begin
  QtGroupBox := TQtGroupBox.Create(AWinControl, AParams);

  Str := GetUtf8String(AWinControl.Caption);
  QGroupBox_setTitle(QGroupBoxH(QtGroupBox.Widget), @Str);

  QtGroupBox.AttachEvents;

  Result := TLCLIntfHandle(QtGroupBox);
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
  QtGroupBox: TQtGroupBox;
  Str: WideString;
begin
  QtGroupBox := TQtGroupBox.Create(AWinControl, AParams);
  
  Str := GetUtf8String(AWinControl.Caption);
  QGroupBox_setTitle(QGroupBoxH(QtGroupBox.Widget), @Str);

  QtGroupBox.AttachEvents;

  Result := TLCLIntfHandle(QtGroupBox);
end;

{ TQtWSCustomTrayIcon }

class function TQtWSCustomTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  SystemTrayIcon: TQtSystemTrayIcon;
begin
  Result := False;

  SystemTrayIcon := TQtSystemTrayIcon(ATrayIcon.Handle);

  SystemTrayIcon.Hide;

  QIcon_destroy(SystemTrayIcon.IconHandle);
  
  SystemTrayIcon.IconHandle := nil;

  SystemTrayIcon.Free;

  ATrayIcon.Handle := 0;

  Result := True;
end;

class function TQtWSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  Text: WideString;
  SystemTrayIcon: TQtSystemTrayIcon;
begin
  Result := False;

  SystemTrayIcon.IconHandle := TQtSystemTrayIcon.TIconToQIconH(ATrayIcon.Icon);

  SystemTrayIcon := TQtSystemTrayIcon.Create(SystemTrayIcon.IconHandle);

  ATrayIcon.Handle := PtrInt(SystemTrayIcon);

  Text := UTF8Decode(ATrayIcon.Hint);
  SystemTrayIcon.setToolTip(Text);

  if Assigned(ATrayIcon.PopUpMenu) then
   if TQtMenu(ATrayIcon.PopUpMenu.Handle).Widget <> nil then
    SystemTrayIcon.setContextMenu(QMenuH(TQtMenu(ATrayIcon.PopUpMenu.Handle).Widget));

  SystemTrayIcon.show;

  Result := True;
end;

{*******************************************************************
*  TQtWSCustomTrayIcon.InternalUpdate ()
*
*  DESCRIPTION:    Makes modifications to the Icon while running
*                  i.e. without hiding it and showing again
*******************************************************************}
class procedure TQtWSCustomTrayIcon.InternalUpdate(const ATrayIcon: TCustomTrayIcon);
var
  SystemTrayIcon: TQtSystemTrayIcon;
begin
  if (ATrayIcon.Handle = 0) then Exit;

  SystemTrayIcon := TQtSystemTrayIcon(ATrayIcon.Handle);

  { PopUpMenu }
  if Assigned(ATrayIcon.PopUpMenu) then
   if TQtMenu(ATrayIcon.PopUpMenu.Handle).Widget <> nil then
    SystemTrayIcon.setContextMenu(QMenuH(TQtMenu(ATrayIcon.PopUpMenu.Handle).Widget));
end;

class function TQtWSCustomTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint;
begin
  Result := Point(0, 0);
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
  RegisterWSComponent(TCustomTrayIcon, TQtWSCustomTrayIcon);
////////////////////////////////////////////////////
end.
