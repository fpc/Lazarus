{ $Id$}
{
 *****************************************************************************
 *                            Win32WSStdCtrls.pp                             *
 *                            ------------------                             *
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
unit Win32WSStdCtrls;

{$mode objfpc}{$H+}
{$I win32defines.inc}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Classes, StdCtrls, Controls, Graphics, Forms, SysUtils,
  Themes,
////////////////////////////////////////////////////
  WSControls, WSStdCtrls, WSLCLClasses, WSProc, Windows, LCLType, InterfaceBase,
  Win32Int, Win32Proc, Win32WSControls, Win32Extra;

type

  { TWin32WSScrollBar }

  TWin32WSScrollBar = class(TWSScrollBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure SetParams(const AScrollBar: TCustomScrollBar); override;
  end;

  { TWin32WSCustomGroupBox }

  TWin32WSCustomGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); override;
    class procedure SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign,
      UseRightToLeftReading, UseRightToLeftScrollBar : Boolean); override;
  end;

  { TWin32WSGroupBox }

  TWin32WSGroupBox = class(TWSGroupBox)
  private
  protected
  public
  end;

  { TWin32WSCustomComboBox }

  TWin32WSCustomComboBox = class(TWSCustomComboBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
      var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    class function  GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetText(const AWinControl: TWinControl; var AText: string): boolean; override;

    class procedure SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox;
      NewTraverseList: boolean); override;
    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetReadOnly(const ACustomComboBox: TCustomComboBox; NewReadOnly: boolean); override;
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;

    class function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
    class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); override;

    class function GetItemHeight(const ACustomComboBox: TCustomComboBox): Integer; override;
    class procedure SetItemHeight(const ACustomComboBox: TCustomComboBox; const AItemHeight: Integer); override;
  end;

  { TWin32WSComboBox }

  TWin32WSComboBox = class(TWSComboBox)
  private
  protected
  public
  end;

  { TWin32WSCustomListBox }

  TWin32WSCustomListBox = class(TWSCustomListBox)
  private
  protected
  public
    class procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); override;
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function GetIndexAtY(const ACustomListBox: TCustomListBox; y: integer): integer; override;
    class function GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class function GetItemRect(const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean; override;
    class function GetSelCount(const ACustomListBox: TCustomListBox): integer; override;
    class function GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;
    class function GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class function GetTopIndex(const ACustomListBox: TCustomListBox): integer; override;

    class procedure SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean); override;
    class procedure SetBorder(const ACustomListBox: TCustomListBox); override;
    class procedure SetColumnCount(const ACustomListBox: TCustomListBox; ACount: Integer); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect,
      AMultiSelect: boolean); override;
    class procedure SetStyle(const ACustomListBox: TCustomListBox); override;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;
    class procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); override;
  end;

  { TWin32WSListBox }

  TWin32WSListBox = class(TWSListBox)
  private
  protected
  public
  end;

  { TWin32WSCustomEdit }

  TWin32WSCustomEdit = class(TWSCustomEdit)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function GetCanUndo(const ACustomEdit: TCustomEdit): Boolean; override;
    class function GetCaretPos(const ACustomEdit: TCustomEdit): TPoint; override;
    class function GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class function GetMaxLength(const ACustomEdit: TCustomEdit): integer; {override;}
    class function GetText(const AWinControl: TWinControl; var AText: string): boolean; override;

    class procedure SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint); override;
    class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;

    class procedure Undo(const ACustomEdit: TCustomEdit); override;
  end;

  { TWin32WSCustomMemo }

  TWin32WSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); override;

    class function  GetCaretPos(const ACustomEdit: TCustomEdit): TPoint; override;
    class function  GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;

    class procedure SetAlignment(const ACustomMemo: TCustomMemo; const AAlignment: TAlignment); override;
    class procedure SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint); override;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); override;
  end;

  { TWin32WSEdit }

  TWin32WSEdit = class(TWSEdit)
  private
  protected
  public
  end;

  { TWin32WSMemo }

  TWin32WSMemo = class(TWSMemo)
  private
  protected
  public
  end;

  { TWin32WSCustomStaticText }

  TWin32WSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment); override;
    class procedure SetStaticBorderStyle(const ACustomStaticText: TCustomStaticText; const NewBorderStyle: TStaticBorderStyle); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TWin32WSStaticText }

  TWin32WSStaticText = class(TWSStaticText)
  private
  protected
  public
  end;

  { TWin32WSButtonControl }

  TWin32WSButtonControl = class(TWSButtonControl)
  private
  protected
  public
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
  end;

  { TWin32WSButton }

  TWin32WSButton = class(TWSButton)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
    class procedure SetShortCut(const AButton: TCustomButton; const OldKey, NewKey: word); override;
  end;

  { TWin32WSCustomCheckBox }

  TWin32WSCustomCheckBox = class(TWSCustomCheckBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox;
          const OldShortCut, NewShortCut: TShortCut); override;
    class procedure SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign,
      UseRightToLeftReading, UseRightToLeftScrollBar : Boolean); override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;
  end;

  { TWin32WSCheckBox }

  TWin32WSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TWin32WSToggleBox }

  TWin32WSToggleBox = class(TWSToggleBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
  end;

  { TWin32WSRadioButton }

  TWin32WSRadioButton = class(TWSRadioButton)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
  end;

{ useful helper functions }

function  EditGetSelStart(WinHandle: HWND): integer;
function  EditGetSelLength(WinHandle: HWND): integer;
procedure EditSetSelStart(WinHandle: HWND; NewStart: integer);
procedure EditSetSelLength(WinHandle: HWND; NewLength: integer);

{$DEFINE MEMOHEADER}
{$I win32memostrings.inc}
{$UNDEF MEMOHEADER}

implementation
  
const
  AlignmentMap: array[TAlignment] of DWORD =
  (
{taLeftJustify } ES_LEFT,
{taRightJustify} ES_RIGHT,
{taCenter      } ES_CENTER
  );

  AlignmentToStaticTextFlags: array[TAlignment] of dword =
  (
    SS_LEFT,
    SS_RIGHT,
    SS_CENTER
  );
  BorderToStaticTextFlags: array[TStaticBorderStyle] of dword =
  (
    0,
    WS_BORDER, // generic border
    SS_SUNKEN  // the only one special border for text static controls
  );
  AccelCharToStaticTextFlags: array[Boolean] of LONG =
  (
    SS_NOPREFIX,
    0
  );

{$I win32memostrings.inc}

{------------------------------------------------------------------------------
 Function: ComboBoxWindowProc
 Params: Window - The window that receives a message
         Msg    - The message received
         WParam - Word parameter
         LParam - Long-integer parameter
  Returns: 0 if Msg is handled; non-zero long-integer result otherwise

  Handles the messages sent to a combobox control by Windows or other
  applications
 ------------------------------------------------------------------------------}
function ComboBoxWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  Info: TComboboxInfo;
begin
  // darn MS: if combobox has edit control, and combobox receives focus, it
  // passes it on to the edit, so it will send a WM_KILLFOCUS; inhibit
  // also don't pass WM_SETFOCUS to the lcl,
  // it will get one from the edit control

  Info.cbSize := SizeOf(Info);
  Win32Extra.GetComboBoxInfo(Window, @Info);
  if ((Msg = WM_KILLFOCUS) or (Msg = WM_SETFOCUS)) and
     ((HWND(WParam) = Info.hwndItem) or (HWND(WParam) = Info.hwndList)) then
  begin
    // continue normal processing, don't send to lcl
    Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
  end else
  begin
    // normal processing
    Result := WindowProc(Window, Msg, WParam, LParam);
  end;
end;

{ TWin32WSScrollBar }

class function TWin32WSScrollBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    case TScrollBar(AWinControl).Kind of
      sbHorizontal:
        Flags := Flags or SBS_HORZ;
      sbVertical:
        Flags := Flags or SBS_VERT;
    end;
    pClassName := 'SCROLLBAR';
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

class procedure TWin32WSScrollBar.SetParams(const AScrollBar: TCustomScrollBar);
var
  ScrollInfo: TScrollInfo;
  AMax: Integer;
begin
  with AScrollBar do
  begin
    AMax := Max + PageSize - 1;
    if AMax < Min then AMax := Min;
    if AMax < Max then AMax := Max;
  
    ScrollInfo.cbSize := SizeOf(TScrollInfo);
    ScrollInfo.fMask := SIF_POS or SIF_Range or SIF_PAGE;
    ScrollInfo.nMin := Min;
    ScrollInfo.nMax := AMax;
    ScrollInfo.nPage := PageSize;
    ScrollInfo.nPos := Position;
    
    SendMessage(Handle, SBM_SETSCROLLINFO, WParam(True), LParam(@ScrollInfo));
    case Kind of
      sbHorizontal:
        SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or SBS_HORZ);
      sbVertical:
        SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or SBS_VERT);
    end;
    Assert(False, 'Trace:TODO: [TWin32WSScrollBar.SetParams] Set up step and page increments for csScrollBar');
  end;
end;

{ TWin32WSCustomGroupBox }

function GroupBoxPanelWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
begin
  // handle paint messages for theming
  case Msg of
    WM_ERASEBKGND, WM_NCPAINT, WM_PAINT, WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
    begin
      Result := WindowProc(Window, Msg, WParam, LParam);
    end;
  else
    Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
  end;
end;

class function TWin32WSCustomGroupBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    if ThemeServices.ThemesEnabled and (AWinControl.Parent <> nil) and
      (AWinControl.Parent is TCustomGroupBox) then
    begin
      // the parent of this groupbox is another groupbox: there is a bug in
      // drawing the caption in that case, the caption of the child groupbox
      // is drawn in system font, make an intermediate "ParentPanel", then
      // the bug is hidden. Use 'ParentPanel' property of groupbox window
      // to determine reference to this parent panel
      // do not use 'ParentPanel' property for other controls!
      Buddy := CreateWindowEx(0, @ClsName[0], nil, WS_CHILD or WS_CLIPCHILDREN or
        WS_CLIPSIBLINGS or (Flags and WS_VISIBLE),
        Left, Top, Width, Height, Parent, 0, HInstance, nil);
      Left := 0;
      Top := 0;
      Flags := Flags or WS_VISIBLE;
      // set P(aint)WinControl, for paint message to retrieve information
      // about wincontrol (hack)
      // allocate windowinfo record ourselves, we do not call WindowInitBuddy
      BuddyWindowInfo := AllocWindowInfo(Buddy);
      BuddyWindowInfo^.PWinControl := AWinControl;
      if GetWindowInfo(Parent)^.needParentPaint then
        BuddyWindowInfo^.needParentPaint := true;
      Parent := Buddy;
    end;
    pClassName := @ButtonClsName[0];
    WindowTitle := StrCaption;
    Flags := Flags Or BS_GROUPBOX;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // handle winxp panel hack
  with Params do
  begin
    if Buddy <> 0 then
    begin
      WindowInfo^.ParentPanel := Buddy;
      // no need to subclass this parentpanel
      Buddy := 0;
    end;
  end;
  // if themed but does not have tabpage as parent
  // remember we are a groupbox in need of erasebackground hack
  if ThemeServices.ThemesEnabled and not Params.WindowInfo^.needParentPaint then
    Params.WindowInfo^.isGroupBox := true;
  AWinControl.InvalidateClientRectCache(true);
  Result := Params.Window;
end;

class procedure TWin32WSCustomGroupBox.AdaptBounds(const AWinControl: TWinControl;
  var Left, Top, Width, Height: integer; var SuppressMove: boolean);
var
  WinHandle, BuddyHandle: HWND;
begin
  WinHandle := AWinControl.Handle;
  // check if we have a ``container'', if so, move that
  BuddyHandle := GetWindowInfo(WinHandle)^.ParentPanel;
  if BuddyHandle <> 0 then
  begin
    MoveWindow(BuddyHandle, Left, Top, Width, Height, false);
    Left := 0;
    Top := 0;
  end;
end;

class procedure TWin32WSCustomGroupBox.SetBiDiMode(
  const AWinControl: TWinControl; UseRightToLeftAlign,
  UseRightToLeftReading, UseRightToLeftScrollBar : Boolean);
begin
  RecreateWnd(AWinControl);
end;

{ TWin32WSCustomListBox }

class procedure TWin32WSCustomListBox.AdaptBounds(
  const AWinControl: TWinControl; var Left, Top, Width, Height: integer;
  var SuppressMove: boolean);
var
  ColCount: Integer;
begin
  ColCount := TCustomListBox(AWinControl).Columns;
  if ColCount > 1 then
    SendMessage(AWinControl.Handle, LB_SETCOLUMNWIDTH, Max(1, Width div ColCount), 0);
end;

class function TWin32WSCustomListBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    with TCustomListBox(AWinControl) do
    begin
      if Sorted then
        Flags := Flags or LBS_SORT;
      if MultiSelect then
        if ExtendedSelect then
          Flags := Flags or LBS_EXTENDEDSEL
        else
          Flags := Flags or LBS_MULTIPLESEL;
      if Columns > 1 then
        Flags := Flags or LBS_MULTICOLUMN;
      if AWinControl.FCompStyle = csCheckListBox then
        Flags := Flags or LBS_OWNERDRAWFIXED
      else case Style of
        lbOwnerDrawFixed: Flags := Flags or LBS_OWNERDRAWFIXED;
        lbOwnerDrawVariable: Flags := Flags or LBS_OWNERDRAWVARIABLE;
      end;
      if BorderStyle=bsSingle then
        FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
    end;
    pClassName := 'LISTBOX';
    Flags := Flags or (WS_VSCROLL or LBS_NOINTEGRALHEIGHT or LBS_HASSTRINGS or
                       LBS_NOTIFY);
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // listbox is not a transparent control -> no need for parentpainting
  Params.WindowInfo^.needParentPaint := false;
  Result := Params.Window;
end;

class function TWin32WSCustomListBox.GetIndexAtY(
  const ACustomListBox: TCustomListBox; y: integer): integer;
begin
  Result := Windows.SendMessage(ACustomListBox.Handle, LB_ITEMFROMPOINT, 0, MakeLParam(0,y));
  if hi(Result)=0 then
    Result := lo(Result)
  else
    Result := -1;
end;

class function TWin32WSCustomListBox.GetItemIndex(const ACustomListBox: TCustomListBox): integer;
begin
  if ACustomListBox.MultiSelect then
    // Return focused item for multiselect listbox
    Result := SendMessage(ACustomListBox.Handle, LB_GETCARETINDEX, 0, 0)
  else
    // LB_GETCURSEL is only for single select listbox
    Result := SendMessage(ACustomListBox.Handle, LB_GETCURSEL, 0, 0);
  if Result = LB_ERR then
  begin
    Assert(false, 'Trace:[TWin32WSCustomListBox.GetItemIndex] could not retrieve itemindex, try selecting an item first');
    Result := -1;
  end;
end;

class function TWin32WSCustomListBox.GetItemRect(
  const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect
  ): boolean;
begin
  Result := Windows.SendMessage(ACustomListBox.Handle, LB_GETITEMRECT, Index,
    LPARAM(@ARect)) <> LB_ERR;
end;

class function TWin32WSCustomListBox.GetSelCount(const ACustomListBox: TCustomListBox): integer;
begin
  // GetSelCount only works for multiple-selection listboxes
  if ACustomListBox.MultiSelect then
    Result := Windows.SendMessage(ACustomListBox.Handle, LB_GETSELCOUNT, 0, 0)
  else begin
    if Windows.SendMessage(ACustomListBox.Handle, LB_GETCURSEL, 0, 0) = LB_ERR then
      Result := 0
    else
      Result := 1;
  end;
end;

class function TWin32WSCustomListBox.GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean;
var
  WindowInfo: PWindowInfo;
  winHandle: HWND;
begin
  winHandle := ACustomListBox.Handle;
  WindowInfo := GetWindowInfo(winHandle);
  // if we're handling a WM_DRAWITEM, then LB_GETSEL is not reliable, check stored info
  if (WindowInfo^.DrawItemIndex <> -1) and (WindowInfo^.DrawItemIndex = AIndex) then
    Result := WindowInfo^.DrawItemSelected
  else
    Result := Windows.SendMessage(winHandle, LB_GETSEL, Windows.WParam(AIndex), 0) > 0;
end;

class function TWin32WSCustomListBox.GetStrings(const ACustomListBox: TCustomListBox): TStrings;
var
  Handle: HWND;
begin
  Handle := ACustomListBox.Handle;
  Result := TWin32ListStringList.Create(Handle, ACustomListBox);
  GetWindowInfo(Handle)^.List := Result;
end;

class function TWin32WSCustomListBox.GetTopIndex(const ACustomListBox: TCustomListBox): integer;
begin
  Result:=Windows.SendMessage(ACustomListBox.Handle, LB_GETTOPINDEX, 0, 0);
end;

class procedure TWin32WSCustomListBox.SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean);
begin
  if ACustomListBox.MultiSelect then
    Windows.SendMessage(ACustomListBox.Handle, LB_SETSEL,
      Windows.WParam(ASelected), Windows.LParam(AIndex))
  else
  if ASelected then
    SetItemIndex(ACustomListBox, AIndex)
  else
    SetItemIndex(ACustomListBox, -1);
end;

class procedure TWin32WSCustomListBox.SetBorder(const ACustomListBox: TCustomListBox);
var
  Handle: HWND;
  StyleEx: PtrInt;
begin
  Handle := ACustomListBox.Handle;
  StyleEx := GetWindowLong(Handle, GWL_EXSTYLE);
  if ACustomListBox.BorderStyle = TBorderStyle(bsSingle) Then
    StyleEx := StyleEx or WS_EX_CLIENTEDGE
  else
    StyleEx := StyleEx and not WS_EX_CLIENTEDGE;
  SetWindowLong(Handle, GWL_EXSTYLE, StyleEx);
end;

class procedure TWin32WSCustomListBox.SetColumnCount(const ACustomListBox: TCustomListBox;
  ACount: Integer);
begin
  // The listbox styles can't be updated, so recreate the listbox
  RecreateWnd(ACustomListBox);
end;

class procedure TWin32WSCustomListBox.SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer);
var
  Handle: HWND;
begin
  Handle := ACustomListBox.Handle;
  if ACustomListBox.MultiSelect then
  begin
    // deselect all items first
    Windows.SendMessage(Handle, LB_SETSEL, Windows.WParam(false), -1);
    if AIndex >= 0 then
      Windows.SendMessage(Handle, LB_SETSEL, Windows.WParam(true), Windows.LParam(AIndex));
  end else
    Windows.SendMessage(Handle, LB_SETCURSEL, Windows.WParam(AIndex), 0);
end;

class procedure TWin32WSCustomListBox.SetSelectionMode(const ACustomListBox: TCustomListBox;
  const AExtendedSelect, AMultiSelect: boolean);
begin
  RecreateWnd(ACustomListBox);
end;

class procedure TWin32WSCustomListBox.SetStyle(const ACustomListBox: TCustomListBox);
begin
  // The listbox styles can't be updated, so recreate the listbox
  RecreateWnd(ACustomListBox);
end;

class procedure TWin32WSCustomListBox.SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean);
begin
  TWin32ListStringList(AList).Sorted := ASorted;
end;

class procedure TWin32WSCustomListBox.SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer);
begin
  Windows.SendMessage(ACustomListBox.Handle, LB_SETTOPINDEX, NewTopIndex, 0);
end;

{ TWin32WSCustomComboBox }

const
  ComboBoxStylesMask = CBS_DROPDOWN or CBS_DROPDOWN or CBS_DROPDOWNLIST or
    CBS_OWNERDRAWFIXED or CBS_OWNERDRAWVARIABLE;

function CalcComboBoxWinFlags(AComboBox: TCustomComboBox): dword;
const
  ComboBoxStyles: array[TComboBoxStyle] of dword = (
    CBS_DROPDOWN, CBS_SIMPLE, CBS_DROPDOWNLIST,
    CBS_OWNERDRAWFIXED, CBS_OWNERDRAWVARIABLE);
  ComboBoxReadOnlyStyles: array[boolean] of dword = (
    CBS_DROPDOWN, CBS_DROPDOWNLIST);
begin
  Result := ComboBoxStyles[AComboBox.Style];
  if AComboBox.Style in [csOwnerDrawFixed, csOwnerDrawVariable] then
    Result := Result or ComboBoxReadOnlyStyles[AComboBox.ReadOnly];
end;

class function TWin32WSCustomComboBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
  Info: TComboboxInfo;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    Flags := Flags or CalcComboBoxWinFlags(TCustomComboBox(AWinControl));
    if TComboBox(AWinControl).Sorted Then
      Flags:= Flags or CBS_SORT;
    pClassName := ComboboxClsName;
    Flags := Flags or (WS_VSCROLL or CBS_AUTOHSCROLL or CBS_HASSTRINGS);
    SubClassWndProc := @ComboBoxWindowProc;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // combobox is not a transparent control -> no need for parentpainting
  Params.WindowInfo^.needParentPaint := false;

  Info.cbSize:= SizeOf(Info);
  Win32Extra.GetComboBoxInfo(Params.Window, @Info);

  // get edit window within
  with Params do
  begin
    // win32 bug? sometimes, if combo should not have edit (apropriate style), hwndItem = hwndCombo
    if Info.hwndItem <> Info.hwndCombo then
      Buddy := Info.hwndItem
    else
      Buddy := 0;
    // If the style is CBS_DROPDOWNLIST, Info.hwndItem is null,
    // because the combobox has no edit in that case.
    if Buddy <> HWND(nil) then
    begin
      SubClassWndProc := @WindowProc;
      WindowCreateInitBuddy(AWinControl, Params);
      BuddyWindowInfo^.isChildEdit := true;
      BuddyWindowInfo^.isComboEdit := true;
    end
    else
      BuddyWindowInfo:=nil;
  end;
  Result := Params.Window;
end;

class procedure TWin32WSCustomComboBox.AdaptBounds(const AWinControl: TWinControl;
  var Left, Top, Width, Height: integer; var SuppressMove: boolean);
var
  WinHandle: HWND;
  StringList: TWin32ComboBoxStringList;
begin
  WinHandle := AWinControl.Handle;
  if TCustomComboBox(AWinControl).Style <> csSimple then
  begin
    StringList := TWin32ComboBoxStringList(GetWindowInfo(WinHandle)^.List);
    if StringList <> nil then
      Height := StringList.ComboHeight;
  end;
end;

class procedure TWin32WSCustomComboBox.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  PreferredHeight := 0;
  if (AWinControl.HandleAllocated) and (TCustomComboBox(AWinControl).Style <> csSimple) then
    PreferredHeight := AWinControl.Height;
end;

class function TWin32WSCustomComboBox.GetSelStart(const ACustomComboBox: TCustomComboBox): integer;
begin
  SendMessage(ACustomComboBox.Handle, CB_GETEDITSEL, Windows.WPARAM(@Result), Windows.LPARAM(nil));
end;

class function TWin32WSCustomComboBox.GetSelLength(const ACustomComboBox: TCustomComboBox): integer;
var
  startPos, endPos: dword;
begin
  SendMessage(ACustomComboBox.Handle, CB_GETEDITSEL, Windows.WPARAM(@startPos), Windows.LPARAM(@endPos));
  Result := endPos - startPos;
end;

class procedure TWin32WSCustomComboBox.SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle);
var
  CurrentStyle: dword;
begin
  CurrentStyle := GetWindowLong(ACustomComboBox.Handle, GWL_STYLE);
  if (CurrentStyle and ComboBoxStylesMask) =
        CalcComboBoxWinFlags(ACustomComboBox) then
    exit;

  RecreateWnd(ACustomComboBox);
end;

class function TWin32WSCustomComboBox.GetItemIndex(const ACustomComboBox: TCustomComboBox): integer;
begin
  Result := SendMessage(ACustomComboBox.Handle, CB_GETCURSEL, 0, 0);
  if Result = LB_ERR Then
  Begin
    Assert(False, 'Trace:[TWin32WidgetSet.IntSendMessage3] Could not retrieve item index '+
        'via LM_GETITEMINDEX; try selecting an item first');
    Result := -1;
  End;
end;

class function TWin32WSCustomComboBox.GetMaxLength(const ACustomComboBox: TCustomComboBox): integer;
begin
  Result := GetWindowInfo(ACustomComboBox.Handle)^.MaxLength;
end;

class function TWin32WSCustomComboBox.GetText(const AWinControl: TWinControl; var AText: string): boolean;
begin
  Result := AWinControl.HandleAllocated;
  if not Result then
    exit;
  AText := GetControlText(AWinControl.Handle);
end;

class procedure TWin32WSCustomComboBox.SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox;
  NewTraverseList: boolean);
begin
  // TODO: implement me?
end;

class procedure TWin32WSCustomComboBox.SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer);
begin
  SendMessage(ACustomComboBox.Handle, CB_SETEDITSEL, 0, MakeLParam(NewStart, NewStart));
end;

class procedure TWin32WSCustomComboBox.SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer);
var
  startpos, endpos: integer;
  winhandle: HWND;
begin
  winhandle := ACustomComboBox.Handle;
  SendMessage(winhandle, CB_GETEDITSEL, Windows.WParam(@startpos), Windows.LParam(@endpos));
  endpos := startpos + NewLength;
  SendMessage(winhandle, CB_SETEDITSEL, 0, MakeLParam(startpos, endpos));
end;

class procedure TWin32WSCustomComboBox.SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer);
begin
  SendMessage(ACustomComboBox.Handle, CB_SETCURSEL, Windows.WParam(NewIndex), 0);
end;

class procedure TWin32WSCustomComboBox.SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer);
var
  winhandle: HWND;
begin
  winhandle := ACustomComboBox.Handle;
  SendMessage(winhandle, CB_LIMITTEXT, NewLength, 0);
  GetWindowInfo(winhandle)^.MaxLength := NewLength;
end;

class procedure TWin32WSCustomComboBox.SetReadOnly(const ACustomComboBox: TCustomComboBox;
  NewReadOnly: boolean);
begin
  RecreateWnd(ACustomComboBox);
end;

class procedure TWin32WSCustomComboBox.SetText(const AWinControl: TWinControl; const AText: string);
var
  Handle: HWND;
  {$ifdef WindowsUnicodeSupport}
  AnsiBuffer: ansistring;
  WideBuffer: widestring;
  {$endif}
begin
  Assert(False, Format('Trace:TWin32WSCustomComboBox.SetText --> %S', [AText]));
  Handle := AWinControl.Handle;
  {$ifdef WindowsUnicodeSupport}
  if UnicodeEnabledOS then
  begin
    WideBuffer := UTF8Decode(AText);
    
    if TCustomComboBox(AWinControl).ReadOnly then
      Windows.SendMessageW(Handle, CB_SELECTSTRING, -1, LPARAM(PWideChar(WideBuffer)))
    else
      Windows.SendMessageW(Handle, WM_SETTEXT, 0, LPARAM(PWideChar(WideBuffer)));
  end
  else
  begin
    AnsiBuffer := UTF8ToAnsi(AText);
    
    if TCustomComboBox(AWinControl).ReadOnly then
      Windows.SendMessage(Handle, CB_SELECTSTRING, -1, LPARAM(PChar(AnsiBuffer)))
    else
      Windows.SendMessage(Handle, WM_SETTEXT, 0, LPARAM(PChar(AnsiBuffer)));
  end;
  {$else}
  if TCustomComboBox(AWinControl).ReadOnly then
    Windows.SendMessage(Handle, CB_SELECTSTRING, -1, LPARAM(PChar(AText)))
  else
    Windows.SendMessage(Handle, WM_SETTEXT, 0, LPARAM(PChar(AText)));
  {$endif}
end;

class function TWin32WSCustomComboBox.GetItems(const ACustomComboBox: TCustomComboBox): TStrings;
var
  winhandle: HWND;
begin
  winhandle := ACustomComboBox.Handle;
  Result := TWin32ComboBoxStringList.Create(winhandle, ACustomComboBox);
  GetWindowInfo(winhandle)^.List := Result;
end;

class procedure TWin32WSCustomComboBox.Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean);
begin
  TWin32ListStringList(AList).Sorted := IsSorted;
end;

class function TWin32WSCustomComboBox.GetItemHeight(const ACustomComboBox: TCustomComboBox): Integer;
begin
  if not WSCheckHandleAllocated(ACustomComboBox, 'GetItemHeight') then 
    Result := 0
  else
    Result := SendMessage(ACustomComboBox.Handle, CB_GETITEMHEIGHT, 0, 0);
end;

class procedure TWin32WSCustomComboBox.SetItemHeight(const ACustomComboBox: TCustomComboBox; const AItemHeight: Integer);
begin
  if not WSCheckHandleAllocated(ACustomComboBox, 'SetItemHeight') then 
    Exit;
  // size requests are done through WM_MeasureItem
  // SendMessage(ACustomComboBox.Handle, CB_SETITEMHEIGHT, AItemHeight, -1);
  // SendMessage(ACustomComboBox.Handle, CB_SETITEMHEIGHT, AItemHeight, 0);
  RecreateWnd(ACustomComboBox);
end;
{ TWin32WSCustomEdit helper functions }

function EditGetSelStart(WinHandle: HWND): integer;
begin
  Windows.SendMessage(WinHandle, EM_GETSEL, Windows.WPARAM(@Result), 0);
end;

function EditGetSelLength(WinHandle: HWND): integer;
var
  startpos, endpos: integer;
begin
  Windows.SendMessage(WinHandle, EM_GETSEL, Windows.WPARAM(@startpos), Windows.LPARAM(@endpos));
  Result := endpos - startpos;
end;

procedure EditSetSelStart(WinHandle: HWND; NewStart: integer);
begin
  Windows.SendMessage(WinHandle, EM_SETSEL, Windows.WParam(NewStart), Windows.LParam(NewStart));
  // scroll caret into view
  Windows.SendMessage(WinHandle, EM_SCROLLCARET, 0, 0);
end;

procedure EditSetSelLength(WinHandle: HWND; NewLength: integer);
var
  startpos, endpos: integer;
begin
  Windows.SendMessage(WinHandle, EM_GETSEL, Windows.WParam(@startpos), Windows.LParam(@endpos));
  endpos := startpos + NewLength;
  Windows.SendMessage(WinHandle, EM_SETSEL, Windows.WParam(startpos), Windows.LParam(endpos));
end;

{ TWin32WSCustomEdit }

class function TWin32WSCustomEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    if (AWinControl is TCustomEdit) then
      if TCustomEdit(AWinControl).BorderStyle=bsSingle then
        FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
    pClassName := @EditClsName[0];
    WindowTitle := StrCaption;
    Flags := Flags or ES_AUTOHSCROLL;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // edit is not a transparent control -> no need for parentpainting
  Params.WindowInfo^.needParentPaint := false;
  Result := Params.Window;
end;

class function TWin32WSCustomEdit.GetCanUndo(const ACustomEdit: TCustomEdit
  ): Boolean;
begin
  Result := False;
  if not WSCheckHandleAllocated(ACustomEdit, 'GetCanUndo') then
    Exit;
  Result := Windows.SendMessage(ACustomEdit.Handle, EM_CANUNDO, 0, 0) <> 0;
end;

class function TWin32WSCustomEdit.GetCaretPos(const ACustomEdit: TCustomEdit): TPoint;
var
  BufferX: Longword;
begin
  // EM_GETSEL expects a pointer to 32-bits buffer in lParam
  Windows.SendMessageW(ACustomEdit.Handle, EM_GETSEL, 0, PtrInt(@BufferX));
  Result.X := BufferX;
  Result.Y := 0;
end;

class function TWin32WSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  Result := EditGetSelStart(ACustomEdit.Handle);
end;

class function TWin32WSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  Result := EditGetSelLength(ACustomEdit.Handle);
end;

class function TWin32WSCustomEdit.GetMaxLength(const ACustomEdit: TCustomEdit): integer;
begin
  Result := GetWindowInfo(ACustomEdit.Handle)^.MaxLength;
end;

class function TWin32WSCustomEdit.GetText(const AWinControl: TWinControl; var AText: string): boolean;
begin
  Result := AWinControl.HandleAllocated;
  if not Result then
    exit;
  AText := GetControlText(AWinControl.Handle);
end;

class procedure TWin32WSCustomEdit.SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint);
begin
  Windows.SendMessageW(ACustomEdit.Handle, EM_SETSEL, NewPos.X, NewPos.X);
end;

class procedure TWin32WSCustomEdit.SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase);
const
  EditStyles: array[TEditCharCase] of integer = (0, ES_UPPERCASE, ES_LOWERCASE);
  EditStyleMask = ES_UPPERCASE or ES_LOWERCASE;
begin
  UpdateWindowStyle(ACustomEdit.Handle, EditStyles[NewCase], EditStyleMask);
end;

class procedure TWin32WSCustomEdit.SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode);
begin
  // nothing to do, SetPasswordChar will do the work
end;

class procedure TWin32WSCustomEdit.SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer);
var
  winhandle: HWND;
begin
  winhandle := ACustomEdit.Handle;
  SendMessage(winhandle, EM_LIMITTEXT, NewLength, 0);
  GetWindowInfo(winhandle)^.MaxLength := NewLength;
end;

class procedure TWin32WSCustomEdit.SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char);
begin
  SendMessage(ACustomEdit.Handle, EM_SETPASSWORDCHAR, WParam(NewChar), 0);
end;

class procedure TWin32WSCustomEdit.SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
begin
  Windows.SendMessage(ACustomEdit.Handle, EM_SETREADONLY, Windows.WPARAM(NewReadOnly), 0);
end;

class procedure TWin32WSCustomEdit.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);
begin
  EditSetSelStart(ACustomEdit.Handle, NewStart);
end;

class procedure TWin32WSCustomEdit.SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer);
begin
  EditSetSelLength(ACustomEdit.Handle, NewLength);
end;

class procedure TWin32WSCustomEdit.Undo(const ACustomEdit: TCustomEdit);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'Undo') then
    Exit;
  SendMessage(ACustomEdit.Handle, EM_UNDO, 0, 0)
end;

{ TWin32WSCustomMemo }

class function TWin32WSCustomMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    Flags := Flags or ES_AUTOVSCROLL or ES_MULTILINE or ES_WANTRETURN;
    if TCustomMemo(AWinControl).ReadOnly then
      Flags := Flags or ES_READONLY;
    Flags := Flags or AlignmentMap[TCustomMemo(AWinControl).Alignment];
    case TCustomMemo(AWinControl).ScrollBars of
      ssHorizontal, ssAutoHorizontal:
        Flags := Flags or WS_HSCROLL;
      ssVertical, ssAutoVertical:
        Flags := Flags or WS_VSCROLL;
      ssBoth, ssAutoBoth:
        Flags := Flags or WS_HSCROLL or WS_VSCROLL;
    end;
    if TCustomMemo(AWinControl).WordWrap then
      Flags := Flags and not WS_HSCROLL
    else
      Flags := Flags or ES_AUTOHSCROLL;
    FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
    pClassName := @EditClsName[0];
    WindowTitle := StrCaption;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // memo is not a transparent control -> no need for parentpainting
  Params.WindowInfo^.needParentPaint := false;
  Result := Params.Window;
end;

class function TWin32WSCustomMemo.GetStrings(const ACustomMemo: TCustomMemo): TStrings;
begin
  Result := TWin32MemoStrings.Create(ACustomMemo.Handle, ACustomMemo)
end;

class procedure TWin32WSCustomMemo.AppendText(const ACustomMemo: TCustomMemo; const AText: string);
var
  S: string;
begin
  if Length(AText) > 0 then
  begin
    GetText(ACustomMemo, S);
    S := S + AText;
    SetText(ACustomMemo, S);
  end;
end;

{
  The index of the first line is zero
  
  The index of the caret before the first char is zero
  
  If there is a selection, the caret is considered to be right after
  the last selected char, being that "last" here means the right-most char.
}
class function TWin32WSCustomMemo.GetCaretPos(const ACustomEdit: TCustomEdit): TPoint;
var
  BufferX: Longword;
begin
  { X position calculation }
  
  { EM_GETSET returns the char index of the caret, but this index
    doesn't go back to zero in new lines, so we need to subtract
    the char index from the line

    EM_GETSEL expects a pointer to 32-bits buffer in lParam
  }
  Windows.SendMessageW(ACustomEdit.Handle, EM_GETSEL, 0, PtrInt(@BufferX));
  { EM_LINEINDEX returns the char index of a given line
    wParam = -1 indicates the line of the caret
  }
  Result.X := BufferX - Windows.SendMessageW(ACustomEdit.Handle, EM_LINEINDEX, -1, 0);

  { Y position calculation }

  { EM_LINEFROMCHAR returns the number of the line of a given
    char index.
  }
  Result.Y := Windows.SendMessageW(ACustomEdit.Handle, EM_LINEFROMCHAR, BufferX, 0);
end;

class procedure TWin32WSCustomMemo.SetAlignment(const ACustomMemo: TCustomMemo;
  const AAlignment: TAlignment);
begin
  // SetWidowLong is not working here
  RecreateWnd(ACustomMemo);
end;

class procedure TWin32WSCustomMemo.SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint);
var
  CharIndex: Longword;
begin
  { EM_LINEINDEX returns the char index of a given line }
  CharIndex := Windows.SendMessageW(ACustomEdit.Handle, EM_LINEINDEX, NewPos.Y, 0) + NewPos.X;
  { EM_SETSEL expects the character position in char index, which
    doesn't go back to zero in new lines
  }
  Windows.SendMessageW(ACustomEdit.Handle, EM_SETSEL, CharIndex, CharIndex);
end;

class procedure TWin32WSCustomMemo.SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle);
begin
  // TODO: check if can be done without recreation
  RecreateWnd(ACustomMemo);
end;

class procedure TWin32WSCustomMemo.SetText(const AWinControl: TWinControl; const AText: string);
begin
  {$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS then
      SendMessageW(AWinControl.Handle, WM_SETTEXT, 0, LPARAM(PWideChar(Utf8Decode(AText))))
    else
      SendMessage(AWinControl.Handle, WM_SETTEXT, 0, LPARAM(PChar(Utf8ToAnsi(AText))));
  {$else}
    SendMessage(AWinControl.Handle, WM_SETTEXT, 0, LPARAM(PChar(AText)));
  {$endif}
end;

class procedure TWin32WSCustomMemo.SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean);
begin
  // TODO: check if can be done without recreation
  RecreateWnd(ACustomMemo);
end;

{ TWin32WSCustomStaticText }

function CalcStaticTextFlags(
   const AAlignment: TAlignment;
   const ABorder: TStaticBorderStyle;
   const AShowAccelChar: Boolean): dword;
begin
  Result :=
   AlignmentToStaticTextFlags[AAlignment] or
   BorderToStaticTextFlags[ABorder] or
   AccelCharToStaticTextFlags[AShowAccelChar];
end;

class function TWin32WSCustomStaticText.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := 'STATIC';
    WindowTitle := StrCaption;
    // if control style have SS_NOTIFY then HTCLIENT otherwise HTTRANSPARENT =>
    // so it will not understand mouse if there is no SS_NOTIFY
    Flags := Flags or SS_NOTIFY or
      CalcStaticTextFlags(TCustomStaticText(AWinControl).Alignment,
       TCustomStaticText(AWinControl).BorderStyle, TCustomStaticText(AWinControl).ShowAccelChar);
    if (TCustomStaticText(AWinControl).BorderStyle = sbsSingle) and ThemeServices.ThemesEnabled then
    begin
      Flags := Flags and not WS_BORDER; // under XP WS_BORDER is not themed and there are some problems with redraw
      FlagsEx := FlagsEx or WS_EX_CLIENTEDGE; // this is themed-border
    end;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

class procedure TWin32WSCustomStaticText.SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment);
begin
  if not WSCheckHandleAllocated(ACustomStaticText, 'SetAlignment') then
    exit;
  // can not apply on the fly: needs window recreate
  RecreateWnd(ACustomStaticText);
end;

class procedure TWin32WSCustomStaticText.SetStaticBorderStyle(
  const ACustomStaticText: TCustomStaticText;
  const NewBorderStyle: TStaticBorderStyle);
begin
  if not WSCheckHandleAllocated(ACustomStaticText, 'SetStaticBorderStyle') then
    exit;
  // can not apply on the fly: needs window recreate
  RecreateWnd(ACustomStaticText);
end;

class procedure TWin32WSCustomStaticText.SetText(
  const AWinControl: TWinControl; const AText: String);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetText') then
    exit;

  // maybe we need TWSCustomStaticText.SetShowAccelChar ?
  
  if (GetWindowLong(AWinControl.Handle, GWL_STYLE) and SS_NOPREFIX) <>
     AccelCharToStaticTextFlags[TCustomStaticText(AWinControl).ShowAccelChar] then
    RecreateWnd(AWinControl);
    
  TWSWinControlClass(ClassParent).SetText(AWinControl, AText);
end;

{ TWin32WSButtonControl }

class procedure TWin32WSButtonControl.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  if MeasureText(AWinControl, AWinControl.Caption, PreferredWidth, PreferredHeight) then
  begin
    Inc(PreferredWidth, 20);
    Inc(PreferredHeight, 4);
    if WithThemeSpace then begin
      Inc(PreferredWidth, 6);
      Inc(PreferredHeight, 6);
    end;
  end;
end;

{ TWin32WSButton }

class function TWin32WSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    if TCustomButton(AWinControl).Default Then
      Flags := Flags or BS_DEFPUSHBUTTON
    else
      Flags := Flags or BS_PUSHBUTTON;
    pClassName := @ButtonClsName[0];
    WindowTitle := StrCaption;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

class procedure TWin32WSButton.SetDefault(const AButton: TCustomButton; ADefault: Boolean);
var
  WindowStyle: dword;
begin
  if not WSCheckHandleAllocated(AButton, 'SetDefault') then Exit;

  WindowStyle := GetWindowLong(AButton.Handle, GWL_STYLE) and not (BS_DEFPUSHBUTTON or BS_PUSHBUTTON);
  if ADefault then
    WindowStyle := WindowStyle or BS_DEFPUSHBUTTON
  else
    WindowStyle := WindowStyle or BS_PUSHBUTTON;
  Windows.SendMessage(AButton.Handle, BM_SETSTYLE, WindowStyle, 1);
end;

class procedure TWin32WSButton.SetShortCut(const AButton: TCustomButton; const OldKey, NewKey: word);
begin
  if not WSCheckHandleAllocated(AButton, 'SetShortcut') then Exit;
  // TODO: implement me!
end;

{ TWin32WSCustomCheckBox }

class function TWin32WSCustomCheckBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ButtonClsName[0];
    WindowTitle := StrCaption;
    if TCustomCheckBox(AWinControl).AllowGrayed then
      Flags := Flags Or BS_AUTO3STATE
    else
      Flags := Flags Or BS_AUTOCHECKBOX;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

class procedure TWin32WSCustomCheckBox.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
var
  iconHeight: integer;
begin
  if MeasureText(AWinControl, AWinControl.Caption, PreferredWidth, PreferredHeight) then
  begin
    // 7 pixels spacing between checkbox and text
    Inc(PreferredWidth, GetSystemMetrics(SM_CXMENUCHECK) + 7);
    iconHeight := GetSystemMetrics(SM_CYMENUCHECK);
    if iconHeight > PreferredHeight then
      PreferredHeight := iconHeight;
    if WithThemeSpace then begin
      Inc(PreferredWidth, 6);
      Inc(PreferredHeight, 6);
    end;
  end;
end;

class function TWin32WSCustomCheckBox.RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
begin
  case SendMessage(ACustomCheckBox.Handle, BM_GETCHECK, 0, 0) of
    BST_CHECKED:       Result := cbChecked;
    BST_INDETERMINATE: Result := cbGrayed;
  else
    {BST_UNCHECKED:}   Result := cbUnChecked;
  end;
end;

class procedure TWin32WSCustomCheckBox.SetShortCut(const ACustomCheckBox: TCustomCheckBox;
  const OldShortCut, NewShortCut: TShortCut);
begin
  // TODO: implement me!
end;

class procedure TWin32WSCustomCheckBox.SetBiDiMode(
  const AWinControl: TWinControl; UseRightToLeftAlign,
  UseRightToLeftReading, UseRightToLeftScrollBar : Boolean);
begin
//  UpdateStdBiDiModeFlags(AWinControl); not worked
  RecreateWnd(AWinControl);
end;

class procedure TWin32WSCustomCheckBox.SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
var
  Flags: WPARAM;
begin
  case NewState of
    cbChecked: Flags := Windows.WParam(BST_CHECKED);
    cbUnchecked: Flags := Windows.WParam(BST_UNCHECKED);
  else
    Flags := Windows.WParam(BST_INDETERMINATE);
  end;
  Windows.SendMessage(ACustomCheckBox.Handle, BM_SETCHECK, Flags, 0);
end;

{ TWin32WSToggleBox }

class function TWin32WSToggleBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ButtonClsName[0];
    WindowTitle := StrCaption;
    Flags := Flags or BS_AUTOCHECKBOX or BS_PUSHLIKE;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;


{ TWin32WSRadioButton }

class function TWin32WSRadioButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ButtonClsName[0];
    WindowTitle := StrCaption;
    // BS_AUTORADIOBUTTON may hang the application,
    // if the radiobuttons are not consecutive controls.
    Flags := Flags Or BS_RADIOBUTTON;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;



initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TScrollBar, TWin32WSScrollBar);
  RegisterWSComponent(TCustomGroupBox, TWin32WSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TWin32WSGroupBox);
  RegisterWSComponent(TCustomComboBox, TWin32WSCustomComboBox);
//  RegisterWSComponent(TComboBox, TWin32WSComboBox);
  RegisterWSComponent(TCustomListBox, TWin32WSCustomListBox);
//  RegisterWSComponent(TListBox, TWin32WSListBox);
  RegisterWSComponent(TCustomEdit, TWin32WSCustomEdit);
  RegisterWSComponent(TCustomMemo, TWin32WSCustomMemo);
//  RegisterWSComponent(TEdit, TWin32WSEdit);
//  RegisterWSComponent(TMemo, TWin32WSMemo);
  RegisterWSComponent(TButtonControl, TWin32WSButtonControl);
  RegisterWSComponent(TCustomButton, TWin32WSButton);
  RegisterWSComponent(TCustomCheckBox, TWin32WSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TWin32WSCheckBox);
//  RegisterWSComponent(TCheckBox, TWin32WSCheckBox);
  RegisterWSComponent(TToggleBox, TWin32WSToggleBox);
  RegisterWSComponent(TRadioButton, TWin32WSRadioButton);
  RegisterWSComponent(TCustomStaticText, TWin32WSCustomStaticText);
//  RegisterWSComponent(TStaticText, TWin32WSStaticText);
////////////////////////////////////////////////////
end.
