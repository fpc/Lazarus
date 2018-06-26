{
 *****************************************************************************
 *                            WinCEWSComCtrls.pp                             *
 *                            ------------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.LCL, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WinCEWSComCtrls;

{$mode objfpc}{$H+}
interface

uses
  // RTL, FCL
  Windows, Classes, SysUtils, WinExt, Forms,
  commctrl,
  // Compatibility
  {$ifdef Win32}win32compat,{$endif}
  // LCL
  ComCtrls, LCLType, Controls, Graphics,
  ImgList, StdCtrls, Math, LCLIntf, ExtCtrls,
  LazUTF8, InterfaceBase, LMessages, LCLMessageGlue,
  // widgetset
  WSComCtrls, WSLCLClasses, WSProc, WsControls,
  WinCEInt, WinCEProc, WinCEWSControls, WinCEExtra;

type
  { TWinCEWSCustomPage }

  TWinCEWSCustomPage = class(TWSCustomPage)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure UpdateProperties(const ACustomPage: TCustomPage); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
  end;

  { TWinCEWSCustomNotebook }

  TWinCEWSCustomNotebook = class(TWSCustomTabControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure AddAllNBPages(const ATabControl: TCustomTabControl);
    class procedure AdjustSizeNotebookPages(const ATabControl: TCustomTabControl);
    class procedure AddPage(const ATabControl: TCustomTabControl;
      const AChild: TCustomPage; const AIndex: integer); override;
    class procedure MovePage(const ATabControl: TCustomTabControl;
      const AChild: TCustomPage; const NewIndex: integer); override;
    class procedure RemoveAllNBPages(const ATabControl: TCustomTabControl);
    class procedure RemovePage(const ATabControl: TCustomTabControl;
      const AIndex: integer); override;

    class function GetTabIndexAtPos(const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer; override;
    class function GetTabRect(const ATabControl: TCustomTabControl; const AIndex: Integer): TRect; override;
    class function GetCapabilities: TCTabControlCapabilities;override;
    class function GetDesignInteractive(const AWinControl: TWinControl; AClientPos: TPoint): Boolean; override;
    class procedure SetImageList(const ATabControl: TCustomTabControl; const AImageList: TCustomImageList);
    class procedure SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer); override;
    class procedure SetTabPosition(const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition); override;
    class procedure ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean); override;
    class procedure UpdateProperties(const ATabControl: TCustomTabControl); override;
  end;

  { TWinCEWSStatusBar }

  TWinCEWSStatusBar = class(TWSStatusBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure Update(const AStatusBar: TStatusBar); override;
    class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, 
      AWidth, AHeight: integer); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
  end;

  { TWinCEWSTabSheet }

  TWinCEWSTabSheet = class(TWSTabSheet)
  private
  protected
  public
  end;

  { TWinCEWSPageControl }

  TWinCEWSPageControl = class(TWSPageControl)
  private
  protected
  public
  end;

  { TWinCEWSCustomListView }

  TWinCEWSCustomListView = class(TWSCustomListView)
  private
    class function  GetHeader(const AHandle: THandle): THandle;
    class procedure PositionHeader(const AHandle: THandle);
    class procedure UpdateStyle(const AHandle: THandle; const AMask, AStyle: Integer);
    class procedure UpdateExStyle(const AHandle: THandle; const AMask, AStyle: Integer);
    class procedure LVItemAssign(const ALV: TCustomListView; AItem: TListItem; const AIndex: Integer);
  published
    // columns
    class procedure ColumnDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn): Integer; override;
    class procedure ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn); override;
    class procedure ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); override;
    class procedure ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment); override;
    class procedure ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean); override;
    class procedure ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ACaption: String); override;
    class procedure ColumnSetImage(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer); override;
    class procedure ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer); override;
    class procedure ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer); override;
    class procedure ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer); override;
    class procedure ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean); override;

    // items
    class procedure ItemDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ItemDisplayRect(const ALV: TCustomListView; const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect; override;
    class procedure ItemExchange(const ALV: TCustomListView; AItem: TListItem; const AIndex1, AIndex2: Integer); override;
    class procedure ItemMove(const ALV: TCustomListView; AItem: TListItem; const AFromIndex, AToIndex: Integer); override;
    class function  ItemGetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem): Boolean; override;
    class function  ItemGetPosition(const ALV: TCustomListView; const AIndex: Integer): TPoint; override;
    class function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; override; // returns True if supported
    class procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); override;
    class procedure ItemSetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean); override;
    class procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer); override;
    class function  ItemSetPosition(const ALV: TCustomListView; const AIndex: Integer; const ANewPosition: TPoint): Boolean; override;
    class procedure ItemSetStateImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AStateImageIndex: Integer); override;
    class procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); override;
    class procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String); override;
    class procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean); override;
  
    // lv
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;

    class procedure BeginUpdate(const ALV: TCustomListView); override;
    class procedure EndUpdate(const ALV: TCustomListView); override;

    class function GetBoundingRect(const ALV: TCustomListView): TRect; override;
    class function GetDropTarget(const ALV: TCustomListView): Integer; override;
    class function GetFocused(const ALV: TCustomListView): Integer; override;
    class function GetHitTestInfoAt( const ALV: TCustomListView; X, Y: Integer ) : THitTests; override;
    class function GetHoverTime(const ALV: TCustomListView): Integer; override;
    class function GetItemAt(const ALV: TCustomListView; x,y: Integer): Integer; override;
    class function GetSelCount(const ALV: TCustomListView): Integer; override;
    class function GetSelection(const ALV: TCustomListView): Integer; override;
    class function GetTopItem(const ALV: TCustomListView): Integer; override;
    class function GetViewOrigin(const ALV: TCustomListView): TPoint; override;
    class function GetVisibleRowCount(const ALV: TCustomListView): Integer; override;

    class procedure SetAllocBy(const ALV: TCustomListView; const AValue: Integer); override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetDefaultItemHeight(const ALV: TCustomListView; const AValue: Integer); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetHotTrackStyles(const ALV: TCustomListView; const AValue: TListHotTrackStyles); override;
    class procedure SetHoverTime(const ALV: TCustomListView; const AValue: Integer); override;
    class procedure SetIconArrangement(const ALV: TCustomListView; const AValue: TIconArrangement); override;
    class procedure SetImageList(const ALV: TCustomListView; const AList: TListViewImageList; const AValue: TCustomImageListResolution); override;
    class procedure SetItemsCount(const ALV: TCustomListView; const AValue: Integer); override;
    class procedure SetOwnerData(const ALV: TCustomListView; const AValue: Boolean); override;
    class procedure SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean); override;
    class procedure SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties); override;
    class procedure SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle); override;
    class procedure SetSort(const ALV: TCustomListView; const AType: TSortType; const AColumn: Integer;
      const ASortDirection: TSortDirection); override;
    class procedure SetViewOrigin(const ALV: TCustomListView; const AValue: TPoint); override;
    class procedure SetViewStyle(const ALV: TCustomListView; const Avalue: TViewStyle); override;
  end;

  { TWinCEWSListView }

  TWinCEWSListView = class(TWSListView)
  private
  protected
  public
  end;

  { TWinCEWSProgressBar }

  TWinCEWSProgressBar = class(TWSProgressBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
  end;

  { TWinCEWSCustomUpDown }

  TWinCEWSCustomUpDown = class(TWSCustomUpDown)
  private
  protected
  public
  end;

  { TWinCEWSUpDown }

  TWinCEWSUpDown = class(TWSUpDown)
  private
  protected
  public
  end;

  { TWinCEWSToolButton }

  TWinCEWSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TWinCEWSToolBar }

  TWinCEWSToolBar = class(TWSToolBar)
  private
  protected
  public
  end;

  { TWinCEWSTrackBar }

  TWinCEWSTrackBar = class(TWSTrackBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure ApplyChanges(const ATrackBar: TCustomTrackBar); override;
    class function  GetPosition(const ATrackBar: TCustomTrackBar): integer; override;
    class procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer); override;
  end;

  { TWinCEWSCustomTreeView }

  TWinCEWSCustomTreeView = class(TWSCustomTreeView)
  private
  protected
  public
  end;

  { TWinCEWSTreeView }

  TWinCEWSTreeView = class(TWSTreeView)
  private
  protected
  public
  end;


procedure NotebookFocusNewControl(const ATabControl: TCustomTabControl; NewIndex: integer);
function NotebookPageRealToLCLIndex(const ATabControl: TCustomTabControl; AIndex: integer): integer;

implementation


{$I wincepagecontrol.inc}
{$I wincewscustomlistview.inc }


{ --- Helper routines for TWinCEWSStatusBar --- }

{------------------------------------------------------------------------------
  Method: UpdateStatusBarPanel
  Params: StatusPanel - StatusPanel which needs to be update
  Returns: Nothing

  Called by StatusBarPanelUpdate and StatusBarSetText
  Everything is updated except the panel width
 ------------------------------------------------------------------------------}
procedure UpdateStatusBarPanel(const StatusPanel: TStatusPanel);
var
  BevelType: integer;
  Text: string;
begin
  Text := StatusPanel.Text;
  case StatusPanel.Alignment of
    taCenter: Text := #9 + Text;
    taRightJustify: Text := #9#9 + Text;
  end;
  case StatusPanel.Bevel of
    pbNone: BevelType := Windows.SBT_NOBORDERS;
    pbLowered: BevelType := 0;
    pbRaised: BevelType := Windows.SBT_POPOUT;
  end;
  Windows.SendMessageW(StatusPanel.StatusBar.Handle, SB_SETTEXT,
   StatusPanel.Index or BevelType, LPARAM(PWideChar(UTF8Decode(Text))));
end;

procedure UpdateStatusBarPanelWidths(const StatusBar: TStatusBar);
var
  Rights: PInteger;
  PanelIndex: integer;
  CurrentRight: integer;
begin
  if StatusBar.Panels.Count=0 then begin
    // SETPARTS 0,0 does not work :S
    Windows.SendMessageW(StatusBar.Handle, SB_SIMPLE, 1, 0);
    Windows.SendMessageW(StatusBar.Handle, SB_SETTEXT, 255, WPARAM(PWideChar('')));
    exit;
  end;
  Getmem(Rights, StatusBar.Panels.Count * sizeof(integer));
  try
    CurrentRight := 0;
    for PanelIndex := 0 to StatusBar.Panels.Count-2 do begin
      CurrentRight := CurrentRight + StatusBar.Panels[PanelIndex].Width;
      Rights[PanelIndex] := CurrentRight;
    end;
    Rights[StatusBar.Panels.Count-1] := -1; //Last extends to end;
    Windows.SendMessage(StatusBar.Handle, SB_SETPARTS, StatusBar.Panels.Count, LPARAM(Rights));
  finally
    Freemem(Rights);
  end;
end;

{ TWinCEWSStatusBar }


class function TWinCEWSStatusBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
  init : TINITCOMMONCONTROLSEX;
begin
  init.dwSize := Sizeof(TINITCOMMONCONTROLSEX);
  init.dwICC := ICC_BAR_CLASSES;
  InitCommonControlsEx(@init);
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);

  // customization of Params
  with Params do
  begin
    pClassName := STATUSCLASSNAME;
    WindowTitle := StrCaption;
    Left := LongInt(CW_USEDEFAULT);
    Top := LongInt(CW_USEDEFAULT);
    Width := LongInt(CW_USEDEFAULT);
    Height := LongInt(CW_USEDEFAULT);
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // need to set handle for Update method
  AWinControl.Handle := Params.Window;
  Update(TStatusBar(AWinControl));
  Result := Params.Window;
end;

class procedure TWinCEWSStatusBar.PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer);
begin
  UpdateStatusBarPanelWidths(AStatusBar);
  UpdateStatusBarPanel(AStatusBar.Panels[PanelIndex]);
end;

class procedure TWinCEWSStatusBar.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: integer);
begin
  // statusbars do their own resizing, post a size message to it's queue
  Windows.PostMessage(AWinControl.Handle, WM_SIZE, 0, 0);
end;

class procedure TWinCEWSStatusBar.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
var
  R: TRect;
begin
  // statusbars cannot be resized by the LCL, so actual size is preferred size
  if Windows.GetWindowRect(AWinControl.Handle, @R) then begin
    PreferredHeight:= R.Bottom - R.Top;
    PreferredWidth:= R.Right - R.Left;
  end;
end;

class procedure TWinCEWSStatusBar.SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer);
begin
  if AStatusBar.SimplePanel then
  begin
    Windows.SendMessageW(AStatusBar.Handle, SB_SETTEXT, 255,
      LPARAM(PWideChar(UTF8Decode(AStatusBar.SimpleText))));
  end
  else
    UpdateStatusBarPanel(AStatusBar.Panels[PanelIndex]);
end;

class procedure TWinCEWSStatusBar.SetText(const AWinControl: TWinControl;
  const AText: string);
begin
  // inhibit. StatusBars do not have a caption, simpletext is set by SetPanelText
end;

class procedure TWinCEWSStatusBar.Update(const AStatusBar: TStatusBar);
var
  PanelIndex: integer;
begin
  Windows.SendMessage(AStatusBar.Handle, SB_SIMPLE, WPARAM(AStatusBar.SimplePanel), 0);
  if AStatusBar.SimplePanel then
    SetPanelText(AStatusBar, 0)
  else begin
    UpdateStatusBarPanelWidths(AStatusBar);
    for PanelIndex := 0 to AStatusBar.Panels.Count-1 do
      UpdateStatusBarPanel(AStatusBar.Panels[PanelIndex]);
  end;
end;

{ TWinCEWSProgressBar }

class function TWinCEWSProgressBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
  init : TINITCOMMONCONTROLSEX;
begin
  init.dwSize := Sizeof(TINITCOMMONCONTROLSEX);
  init.dwICC := ICC_PROGRESS_CLASS;
  InitCommonControlsEx(@init);
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    with TCustomProgressBar(AWinControl) do
    begin
      if Smooth then
        Flags := Flags or PBS_SMOOTH;
      if (Orientation = pbVertical) or (Orientation = pbTopDown) then
        Flags := Flags or PBS_VERTICAL;
    end;
    pClassName := PROGRESS_CLASS;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

class procedure TWinCEWSProgressBar.ApplyChanges(
  const AProgressBar: TCustomProgressBar);
begin
  with AProgressBar do
  begin
    { smooth and vertical need window recreation }
    if ((GetWindowLong(Handle, GWL_STYLE) and PBS_SMOOTH  ) <>
         Integer(Smooth) * PBS_SMOOTH) or
       ((GetWindowLong(Handle, GWL_STYLE) and PBS_VERTICAL) <>
         Integer((Orientation = pbVertical) or (Orientation = pbTopDown)) * PBS_VERTICAL) then
      RecreateWnd(AProgressBar);

    SendMessage(Handle, PBM_SETRANGE32, Min, Max);
    SendMessage(Handle, PBM_SETPOS, Position, 0);

{ TODO: Implementable?
    If BarShowText Then
    Begin
      SetWindowText(Handle, StrToPChar((Sender As TControl).Caption));
    End
    Else
      SetWindowText(Handle, Nil);
}
  end;
end;

class procedure TWinCEWSProgressBar.SetPosition(
  const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  Windows.SendMessage(AProgressBar.Handle, PBM_SETPOS, Windows.WPARAM(NewPosition), 0);
end;

{ TWinCEWSTrackBar }

class function TWinCEWSTrackBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
  init : TINITCOMMONCONTROLSEX;
begin
  init.dwSize := Sizeof(TINITCOMMONCONTROLSEX);
  init.dwICC := ICC_BAR_CLASSES;
  InitCommonControlsEx(@init);
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := TRACKBAR_CLASS;
    WindowTitle := StrCaption;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

class procedure TWinCEWSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
var
  wHandle: HWND;
  NewStyle: integer;
const
  StyleMask = TBS_AUTOTICKS or TBS_NOTICKS or TBS_VERT or TBS_TOP or TBS_BOTH;
  TickStyleStyle : array[TTickStyle] of integer =
    (TBS_NOTICKS, TBS_AUTOTICKS, 0);
  OrientationStyle : array[TTrackBarOrientation] of integer =
    (TBS_HORZ, TBS_VERT);
  TickMarksStyle : array[TTickMark] of integer =
    (TBS_BOTTOM, TBS_TOP, TBS_BOTH);
begin
  with ATrackBar do
  begin
    { cache handle }
    wHandle := Handle;
    NewStyle := TickStyleStyle[TickStyle] or OrientationStyle[Orientation] or
                TickMarksStyle[TickMarks];
    UpdateWindowStyle(wHandle, NewStyle, StyleMask);
    Windows.SendMessage(wHandle, TBM_SETRANGEMAX, Windows.WPARAM(true), Max);
    Windows.SendMessage(wHandle, TBM_SETRANGEMIN, Windows.WPARAM(true), Min);
    Windows.SendMessage(wHandle, TBM_SETPOS, Windows.WPARAM(true), Position);
    Windows.SendMessage(wHandle, TBM_SETLINESIZE, 0, LineSize);
    Windows.SendMessage(wHandle, TBM_SETPAGESIZE, 0, PageSize);
    Windows.SendMessage(wHandle, TBM_SETTICFREQ, Frequency, 0);
  end;
end;

class function  TWinCEWSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar): integer;
begin
  Result := SendMessage(ATrackBar.Handle, TBM_GETPOS, 0, 0)
end;

class procedure TWinCEWSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer);
begin
  Windows.SendMessage(ATrackBar.Handle, TBM_SETPOS, Windows.WPARAM(true), Windows.LPARAM(NewPosition));
end;

end.
