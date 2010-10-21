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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    * 
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
  qtwidgets, qtobjects, qtproc, QtWSControls,
  // LCL
  LMessages, LCLMessageGlue, LCLProc,
  SysUtils, Classes, Controls, Graphics, Forms, StdCtrls, ExtCtrls, LCLType,
  ImgList,
  // Widgetset
  WSExtCtrls, WSProc, WSLCLClasses;

type

  { TQtWSCustomPage }

  TQtWSCustomPage = class(TWSCustomPage)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure UpdateProperties(const ACustomPage: TCustomPage); override;
  end;

  { TQtWSCustomNotebook }

  TQtWSCustomNotebook = class(TWSCustomNotebook)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;

    class procedure AddPage(const ANotebook: TCustomNotebook;
      const AChild: TCustomPage; const AIndex: integer); override;
    class procedure MovePage(const ANotebook: TCustomNotebook;
      const AChild: TCustomPage; const NewIndex: integer); override;
    class procedure RemovePage(const ANotebook: TCustomNotebook;
      const AIndex: integer); override;

    class function GetCapabilities: TNoteBookCapabilities; override;
    class function GetDesignInteractive(const AWinControl: TWinControl; AClientPos: TPoint): Boolean; override;
    class function GetTabIndexAtPos(const ANotebook: TCustomNotebook; const AClientPos: TPoint): integer; override;
    class function GetTabRect(const ANotebook: TCustomNotebook; const AIndex: Integer): TRect; override;
    class procedure SetPageIndex(const ANotebook: TCustomNotebook; const AIndex: integer); override;
    class procedure SetTabCaption(const ANotebook: TCustomNotebook; const AChild: TCustomPage; const AText: string); override;
    class procedure SetTabPosition(const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition); override;
    class procedure ShowTabs(const ANotebook: TCustomNotebook; AShowTabs: boolean); override;
    class procedure UpdateProperties(const ANotebook: TCustomNotebook); override;
  end;

  { TQtWSPage }

  TQtWSPage = class(TWSPage)
  published
  end;

  { TQtWSNotebook }

  TQtWSNotebook = class(TWSNotebook)
  published
  end;

  { TQtWSShape }

  TQtWSShape = class(TWSShape)
  published
  end;

  { TQtWSCustomSplitter }

  TQtWSCustomSplitter = class(TWSCustomSplitter)
  published
  end;

  { TQtWSSplitter }

  TQtWSSplitter = class(TWSSplitter)
  published
  end;

  { TQtWSPaintBox }

  TQtWSPaintBox = class(TWSPaintBox)
  published
  end;

  { TQtWSCustomImage }

  TQtWSCustomImage = class(TWSCustomImage)
  published
  end;

  { TQtWSImage }

  TQtWSImage = class(TWSImage)
  published
  end;

  { TQtWSBevel }

  TQtWSBevel = class(TWSBevel)
  published
  end;

  { TQtWSCustomRadioGroup }

  TQtWSCustomRadioGroup = class(TWSCustomRadioGroup)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TQtWSRadioGroup }

  TQtWSRadioGroup = class(TWSRadioGroup)
  published
  end;

  { TQtWSCustomCheckGroup }

  TQtWSCustomCheckGroup = class(TWSCustomCheckGroup)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TQtWSCheckGroup }

  TQtWSCheckGroup = class(TWSCheckGroup)
  published
  end;

  { TQtWSCustomLabeledEdit }

  TQtWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  published
  end;

  { TQtWSLabeledEdit }

  TQtWSLabeledEdit = class(TWSLabeledEdit)
  published
  end;

  { TQtWSCustomPanel }

  TQtWSCustomPanel = class(TWSCustomPanel)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TQtWSPanel }

  TQtWSPanel = class(TWSPanel)
  published
  end;

  { TQtWSCustomTrayIcon }

  TQtWSCustomTrayIcon = class(TWSCustomTrayIcon)
  published
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean; override;
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
  QtFrame.setFrameShape(TBorderStyleToQtFrameShapeMap[TCustomPanel(AWinControl).BorderStyle]);
  
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
  ImageIndex: Integer;
  Bmp: TBitmap;
begin
  ImageList := TCustomNoteBook(ACustomPage.Parent).Images;

  if Assigned(ImageList) then
  begin
    ImageIndex := TCustomNoteBook(ACustomPage.Parent).GetImageIndex(ACustomPage.PageIndex);
    if (ImageIndex >= 0) and (ImageIndex < ImageList.Count) then
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
  QtTabWidget.setTabPosition(QTabWidgetTabPositionMap[TCustomNoteBook(AWinControl).TabPosition]);
  QtTabWidget.setTabsClosable(nboShowCloseButtons in TCustomNotebook(AWinControl).Options);
  QtTabWidget.AttachEvents;

  // Returns the Handle

  Result := TLCLIntfHandle(QtTabWidget);
end;

class procedure TQtWSCustomNotebook.AddPage(const ANotebook: TCustomNotebook;
  const AChild: TCustomPage; const AIndex: integer);
var
  QtTabWidget: TQtTabWidget;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtWSCustomNotebook.AddPage');
  {$endif}
  QtTabWidget := TQtTabWidget(ANotebook.Handle);
  QtTabWidget.setUpdatesEnabled(False);
  QtTabWidget.insertTab(AIndex, TQtPage(AChild.Handle).Widget,
    GetUtf8String(AChild.Caption));
  QtTabWidget.setUpdatesEnabled(True);
  TQtPage(AChild.Handle).ChildOfComplexWidget := ccwTabWidget;
  TQtWsCustomPage.UpdateProperties(AChild);
end;

class procedure TQtWSCustomNotebook.MovePage(const ANotebook: TCustomNotebook;
  const AChild: TCustomPage; const NewIndex: integer);
var
  TabWidget: TQtTabWidget;
  Index: Integer;
  Page: TQtPage;
begin
  Page := TQtPage(AChild.Handle);
  TabWidget := TQtTabWidget(ANotebook.Handle);
  Index := AChild.PageIndex;
  if Index < 0 then
    Index := ANoteBook.IndexOf(AChild);

  TabWidget.BeginUpdate;
  TabWidget.setUpdatesEnabled(false);
  TabWidget.removeTab(Index);
  TabWidget.insertTab(NewIndex, Page.Widget, Page.getIcon, Page.getText);
  TabWidget.setUpdatesEnabled(true);
  if TabWidget.getCurrentIndex <> NewIndex then
    TabWidget.setCurrentWidget(Page);
  TabWidget.EndUpdate;
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
  TabWidget.setUpdatesEnabled(false);
  TabWidget.removeTab(AIndex);
  TabWidget.setUpdatesEnabled(true);
end;

class function TQtWSCustomNotebook.GetCapabilities: TNoteBookCapabilities;
begin
  Result := [nbcShowCloseButtons];
end;

class function TQtWSCustomNotebook.GetDesignInteractive(
  const AWinControl: TWinControl; AClientPos: TPoint): Boolean;
var
  TabWidget: TQtTabWidget;
  TabBar: TQtTabBar;
  TabIndex: Integer;
  p: TQtPoint;
begin
  Result := False;
  if not WSCheckHandleAllocated(AWinControl, 'GetDesignInteractive') then
    Exit;
  TabWidget := TQtTabWidget(AWinControl.Handle);
  TabBar := TabWidget.TabBar;
  p := QtPoint(AClientPos.x, AClientPos.y);
  TabIndex := QTabBar_tabAt(QTabBarH(TabBar.Widget), @p);
  Result := (TabIndex >= 0) and (TabWidget.getCurrentIndex <> TabIndex);
end;

class function TQtWSCustomNotebook.GetTabIndexAtPos(
  const ANotebook: TCustomNotebook; const AClientPos: TPoint): integer;
var
  TabWidget: TQtTabWidget;
  NewPos: TPoint;
  R: TRect;
begin
  TabWidget := TQtTabWidget(ANotebook.Handle);
  NewPos := AClientPos;
  R := TabWidget.TabBar.getGeometry;
  case ANoteBook.TabPosition of
    tpTop: if NewPos.Y < 0 then NewPos.Y := R.Bottom + NewPos.Y;
    tpLeft: if NewPos.X < 0 then NewPos.X := R.Left + NewPos.X;
    tpRight: NewPos.X := R.Right - NewPos.X;
    tpBottom: NewPos.Y := R.Bottom - NewPos.Y;
  end;
  Result := TabWidget.tabAt(NewPos);
end;

class function TQtWSCustomNotebook.GetTabRect(const ANotebook: TCustomNotebook;
  const AIndex: Integer): TRect;
var
  TabWidget: TQtTabWidget;
begin
  Result := Rect(-1, -1, -1, -1);
  if not WSCheckHandleAllocated(ANotebook, 'GetTabRect') then
    Exit;
  TabWidget := TQtTabWidget(ANotebook.Handle);
  Result := TabWidget.TabBar.GetTabRect(AIndex);
  case ANoteBook.TabPosition of
    tpTop: OffsetRect(Result, 0, -Result.Bottom);
    tpLeft: OffsetRect(Result, -Result.Right, 0);
    tpRight: OffsetRect(Result, Result.Left, 0);
    tpBottom: OffsetRect(Result, Result.Top, 0);
  end;
end;

class procedure TQtWSCustomNotebook.SetPageIndex(
  const ANotebook: TCustomNotebook; const AIndex: integer);
var
  TabWidget: TQtTabWidget;
begin
  if not WSCheckHandleAllocated(ANotebook, 'SetPageIndex') then
    Exit;
  TabWidget := TQtTabWidget(ANotebook.Handle);
  TabWidget.setCurrentIndex(AIndex);
end;

class procedure TQtWSCustomNotebook.SetTabCaption(
  const ANotebook: TCustomNotebook; const AChild: TCustomPage;
  const AText: string);
var
  Index: Integer;
begin
  Index := AChild.PageIndex;
  if Index < 0 then
    Index := ANotebook.IndexOf(AChild);
  TQtTabWidget(ANotebook.Handle).setTabText(Index, GetUtf8String(AText));
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
    TabWidget.ShowTabs := AShowTabs;
end;

class procedure TQtWSCustomNotebook.UpdateProperties(const ANotebook: TCustomNotebook);
begin
  TQtTabWidget(ANotebook.Handle).setTabsClosable(nboShowCloseButtons in ANotebook.Options);
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
  QtGroupBox.GroupBoxType := tgbtRadioGroup;

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
  QtGroupBox.GroupBoxType := tgbtCheckGroup;

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

  SystemTrayIcon.Free;

  ATrayIcon.Handle := 0;

  Result := True;
end;

class function TQtWSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  Text: WideString;
  SystemTrayIcon: TQtSystemTrayIcon;
  IconH: QIconH;
begin
  Result := False;

  if ATrayIcon.Icon.Handle = 0 then
    IconH := nil
  else
    IconH := TQtIcon(ATrayIcon.Icon.Handle).Handle;
    
  SystemTrayIcon := TQtSystemTrayIcon.Create(IconH);
  SystemTrayIcon.FTrayIcon := ATrayIcon;

  ATrayIcon.Handle := HWND(SystemTrayIcon);

  Text := UTF8ToUTF16(ATrayIcon.Hint);
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

class function TQtWSCustomTrayIcon.ShowBalloonHint(
  const ATrayIcon: TCustomTrayIcon): Boolean;
var
  QtTrayIcon: TQtSystemTrayIcon;
begin
  Result := False;
  if (ATrayIcon.Handle = 0) then Exit;
  QtTrayIcon := TQtSystemTrayIcon(ATrayIcon.Handle);

  QtTrayIcon.showBaloonHint(ATrayIcon.BalloonTitle, ATrayIcon.BalloonHint,
    QSystemTrayIconMessageIcon(Ord(ATrayIcon.BalloonFlags)),
    ATrayIcon.BalloonTimeout);

  Result := True;
end;

class function TQtWSCustomTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint;
begin
  Result := Point(0, 0);
end;

end.
