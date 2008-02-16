{ $Id: WinCEwsstdctrls.pp 8805 2006-02-23 09:40:21Z vincents $}
{
 *****************************************************************************
 *                            WinCEWSStdCtrls.pp                             *
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
unit WinCEWSStdCtrls;

{$mode objfpc}{$H+}

interface

uses
  // Libs
  Windows,
  // Compatibility
  {$ifdef Win32}win32compat,{$endif}
  // RTL, FCL, LCL
  SysUtils, LCLType, Classes, StdCtrls, Controls, Graphics, Forms, WinCEProc,
  InterfaceBase,
  // Widgetset
  WSStdCtrls, WSLCLClasses, WinCEInt, WinCEWSControls, WinCEExtra;

type

  { TWinCEWSScrollBar }

  TWinCEWSScrollBar = class(TWSScrollBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure SetParams(const AScrollBar: TCustomScrollBar); override;
  end;

  { TWinCEWSCustomGroupBox }

  TWinCEWSCustomGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); override;
  end;

  { TWinCEWSGroupBox }

  TWinCEWSGroupBox = class(TWSGroupBox)
  private
  protected
  public
  end;

  { TWinCEWSCustomComboBox }

  TWinCEWSCustomComboBox = class(TWSCustomComboBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); override;
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
  end;

  { TWinCEWSComboBox }

  TWinCEWSComboBox = class(TWSComboBox)
  private
  protected
  public
  end;

  { TWinCEWSCustomListBox }

  TWinCEWSCustomListBox = class(TWSCustomListBox)
  private
  protected
  public
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
    
  { TWinCEWSListBox }

  TWinCEWSListBox = class(TWSListBox)
  private
  protected
  public
  end;

  { TWinCEWSCustomEdit }

  TWinCEWSCustomEdit = class(TWSCustomEdit)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetMaxLength(const ACustomEdit: TCustomEdit): integer; {override;}
    class function  GetText(const AWinControl: TWinControl; var AText: string): boolean; override;

    class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
  end;

  { TWinCEWSCustomMemo }

  TWinCEWSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function  GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;
    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); override;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); override;
  end;

  { TWinCEWSEdit }

  TWinCEWSEdit = class(TWSEdit)
  private
  protected
  public
  end;

  { TWinCEWSMemo }

  TWinCEWSMemo = class(TWSMemo)
  private
  protected
  public
  end;

  { TWinCEWSCustomStaticText }

  TWinCEWSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment); override;
  end;

  { TWinCEWSStaticText }

  TWinCEWSStaticText = class(TWSStaticText)
  private
  protected
  public
  end;

  { TWinCEWSButtonControl }

  TWinCEWSButtonControl = class(TWSButtonControl)
  private
  protected
  public
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
  end;

  { TWinCEWSButton }

  TWinCEWSButton = class(TWSButton)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
//    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
//    class procedure SetShortcut(const AButton: TCustomButton; const OldShortcut, NewShortcut: TShortcut); override;
//    class procedure GetPreferredSize(const AWinControl: TWinControl;
//                        var PreferredWidth, PreferredHeight: integer); override;
  end;
  { TWinCEWSCustomCheckBox }

  TWinCEWSCustomCheckBox = class(TWSCustomCheckBox)
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
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;
  end;

  { TWinCEWSCheckBox }

  TWinCEWSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TWinCEWSToggleBox }

  TWinCEWSToggleBox = class(TWSToggleBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
  end;

  { TWinCEWSRadioButton }

  TWinCEWSRadioButton = class(TWSRadioButton)
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
{$I wincememostrings.inc}
{$UNDEF MEMOHEADER}

implementation


{$I wincememostrings.inc}


{ TWinCEWSScrollBar }

class function TWinCEWSScrollBar.CreateHandle(const AWinControl: TWinControl;
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
    pClassName := @ScrollBarClsName;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

class procedure TWinCEWSScrollBar.SetParams(const AScrollBar: TCustomScrollBar);
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
    Assert(False, 'Trace:TODO: [TWinCEWSScrollBar.SetParams] Set up step and page increments for csScrollBar');
  end;
end;

{ TWinCEWSCustomGroupBox }

//roozbeh:there are still some issues with group box!

function GroupBoxPanelWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
begin
  // handle paint messages for theming
  case Msg of
    WM_ERASEBKGND, WM_NCPAINT, WM_PAINT, WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
    begin
    Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
//      Result := WindowProc(Window, Msg, WParam, LParam);
    end;
  else
      Result := WindowProc(Window, Msg, WParam, LParam);
//    Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
  end;
end;

class function TWinCEWSCustomGroupBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ButtonClsName;
    WindowTitle := StrCaption;
    Flags := Flags Or BS_GROUPBOX;
  end;
  // create window
  Params.SubClassWndProc := @GroupBoxPanelWindowProc;
  FinishCreateWindow(AWinControl, Params, false);
  // handle winxp panel hack
  // if themed but does not have tabpage as parent
  // remember we are a groupbox in need of erasebackground hack
//  if TWinCEWidgetSet(WidgetSet).ThemesActive
//      and not Params.WindowInfo^.hasTabParent then
//  Params.WindowInfo^.isGroupBox := true;
  AWinControl.InvalidateClientRectCache(true);
  Result := Params.Window;
end;

class procedure TWinCEWSCustomGroupBox.AdaptBounds(const AWinControl: TWinControl;
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


{ TWinCEWSCustomListBox }

class function TWinCEWSCustomListBox.CreateHandle(const AWinControl: TWinControl;
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
    pClassName := @ListBoxClsName;
    Flags := Flags or (WS_VSCROLL or LBS_NOINTEGRALHEIGHT or LBS_HASSTRINGS or
                       LBS_NOTIFY);
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // listbox is not a transparent control -> no need for parentpainting
  Params.WindowInfo^.needParentPaint := false;
  Result := Params.Window;
end;

class function TWinCEWSCustomListBox.GetIndexAtY(
  const ACustomListBox: TCustomListBox; y: integer): integer;
begin
  Result := Windows.SendMessage(ACustomListBox.Handle, LB_ITEMFROMPOINT, 0, MakeLParam(0,y));
  if hi(Result)=0 then
    Result := lo(Result)
  else
    Result := -1;
end;

class function  TWinCEWSCustomListBox.GetItemIndex(const ACustomListBox: TCustomListBox): integer;
begin
  if ACustomListBox.MultiSelect then
    // Return focused item for multiselect listbox
    Result := SendMessage(ACustomListBox.Handle, LB_GETCARETINDEX, 0, 0)
  else
    // LB_GETCURSEL is only for single select listbox
    Result := SendMessage(ACustomListBox.Handle, LB_GETCURSEL, 0, 0);
  if Result = LB_ERR then
  begin
    Assert(false, 'Trace:[TWinCEWSCustomListBox.GetItemIndex] could not retrieve itemindex, try selecting an item first');
    Result := -1;
  end;
end;

class function TWinCEWSCustomListBox.GetItemRect(
  const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect
  ): boolean;
begin
  Result := Windows.SendMessage(ACustomListBox.Handle, LB_GETITEMRECT, Index,
    LPARAM(@ARect)) <> LB_ERR;
end;

class function  TWinCEWSCustomListBox.GetSelCount(const ACustomListBox: TCustomListBox): integer;
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

class function  TWinCEWSCustomListBox.GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean;
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

class function  TWinCEWSCustomListBox.GetStrings(const ACustomListBox: TCustomListBox): TStrings;
var
  Handle: HWND;
begin
  Handle := ACustomListBox.Handle;
  Result := TWinCEListStringList.Create(Handle, ACustomListBox);
  GetWindowInfo(Handle)^.List := Result;
end;

class function  TWinCEWSCustomListBox.GetTopIndex(const ACustomListBox: TCustomListBox): integer;
begin
  Result:=Windows.SendMessage(ACustomListBox.Handle, LB_GETTOPINDEX, 0, 0);
end;

class procedure TWinCEWSCustomListBox.SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean);
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

class procedure TWinCEWSCustomListBox.SetBorder(const ACustomListBox: TCustomListBox);
var
  Handle: HWND;
  StyleEx: dword;
begin
  Handle := ACustomListBox.Handle;
  StyleEx := GetWindowLong(Handle, GWL_EXSTYLE);
  if ACustomListBox.BorderStyle = TBorderStyle(bsSingle) Then
    StyleEx := StyleEx or WS_EX_CLIENTEDGE
  else
    StyleEx := StyleEx and not WS_EX_CLIENTEDGE;
  SetWindowLong(Handle, GWL_EXSTYLE, StyleEx);
end;

class procedure TWinCEWSCustomListBox.SetColumnCount(const ACustomListBox: TCustomListBox;
  ACount: Integer);
begin
  // The listbox styles can't be updated, so recreate the listbox
  RecreateWnd(ACustomListBox);
end;

class procedure TWinCEWSCustomListBox.SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer);
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

class procedure TWinCEWSCustomListBox.SetSelectionMode(const ACustomListBox: TCustomListBox;
  const AExtendedSelect, AMultiSelect: boolean);
begin
  RecreateWnd(ACustomListBox);
end;

class procedure TWinCEWSCustomListBox.SetStyle(const ACustomListBox: TCustomListBox);
begin
  // The listbox styles can't be updated, so recreate the listbox
  RecreateWnd(ACustomListBox);
end;

class procedure TWinCEWSCustomListBox.SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean);
begin
  TWinCEListStringList(AList).Sorted := ASorted;
end;

class procedure TWinCEWSCustomListBox.SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer);
begin
  Windows.SendMessage(ACustomListBox.Handle, LB_SETTOPINDEX, NewTopIndex, 0);
end;

{ TWinCEWSCustomComboBox }

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

class function TWinCEWSCustomComboBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    Flags := Flags or CalcComboBoxWinFlags(TCustomComboBox(AWinControl));
    If TComboBox(AWinControl).Sorted Then
      Flags:= Flags or CBS_SORT;
    pClassName := @ComboboxClsName;
    Flags := Flags or (WS_VSCROLL or CBS_AUTOHSCROLL or CBS_HASSTRINGS);
    SubClassWndProc := @ComboBoxWindowProc;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // combobox is not a transparent control -> no need for parentpainting
  Params.WindowInfo^.needParentPaint := false;

  // get edit window within
  with Params do
  begin
    Buddy := GetTopWindow(Window);
    // If the style is CBS_DROPDOWNLIST, GetTopWindow returns null,
    // because the combobox has no edit in that case.
    if Buddy<>HWND(nil) then begin
      SubClassWndProc := @WindowProc;
      WindowCreateInitBuddy(AWinControl, Params);
      BuddyWindowInfo^.isChildEdit := true;
      BuddyWindowInfo^.isComboEdit := true;
    end else BuddyWindowInfo:=nil;
  end;
  Result := Params.Window;
end;

class procedure TWinCEWSCustomComboBox.AdaptBounds(const AWinControl: TWinControl;
  var Left, Top, Width, Height: integer; var SuppressMove: boolean);
var
  WinHandle: HWND;
  StringList: TWinCEComboBoxStringList;
begin
  WinHandle := AWinControl.Handle;
  StringList := TWinCEComboBoxStringList(GetWindowInfo(WinHandle)^.List);
  if StringList <> nil then
    Height := StringList.ComboHeight;
end;

class function  TWinCEWSCustomComboBox.GetSelStart(const ACustomComboBox: TCustomComboBox): integer;
begin
  SendMessage(ACustomComboBox.Handle, CB_GETEDITSEL, Windows.WPARAM(@Result), Windows.LPARAM(nil));
end;

class function  TWinCEWSCustomComboBox.GetSelLength(const ACustomComboBox: TCustomComboBox): integer;
var
  startPos, endPos: dword;
begin
  SendMessage(ACustomComboBox.Handle, CB_GETEDITSEL, Windows.WPARAM(@startPos), Windows.LPARAM(@endPos));
  Result := endPos - startPos;
end;

class procedure TWinCEWSCustomComboBox.SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle);
var
  CurrentStyle: dword;
begin
  CurrentStyle := GetWindowLong(ACustomComboBox.Handle, GWL_STYLE);
  if (CurrentStyle and ComboBoxStylesMask) =
        CalcComboBoxWinFlags(ACustomComboBox) then
    exit;

  RecreateWnd(ACustomComboBox);
end;

class function  TWinCEWSCustomComboBox.GetItemIndex(const ACustomComboBox: TCustomComboBox): integer;
begin
  Result := SendMessage(ACustomComboBox.Handle, CB_GETCURSEL, 0, 0);
  if Result = LB_ERR Then
  Begin
    Assert(False, 'Trace:[TWinCEWidgetSet.IntSendMessage3] Could not retrieve item index '+
        'via LM_GETITEMINDEX; try selecting an item first');
    Result := -1;
  End;
end;

class function  TWinCEWSCustomComboBox.GetMaxLength(const ACustomComboBox: TCustomComboBox): integer;
begin
  Result := GetWindowInfo(ACustomComboBox.Handle)^.MaxLength;
end;

class function  TWinCEWSCustomComboBox.GetText(const AWinControl: TWinControl; var AText: string): boolean;
begin
  Result := AWinControl.HandleAllocated;
  if not Result then
    exit;
  AText := GetControlText(AWinControl.Handle);
end;

class procedure TWinCEWSCustomComboBox.SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox;
  NewTraverseList: boolean);
begin
  // TODO: implement me?
end;

class procedure TWinCEWSCustomComboBox.SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer);
begin
  SendMessage(ACustomComboBox.Handle, CB_SETEDITSEL, 0, MakeLParam(NewStart, NewStart));
end;

class procedure TWinCEWSCustomComboBox.SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer);
var
  startpos, endpos: integer;
  winhandle: HWND;
begin
  winhandle := ACustomComboBox.Handle;
  SendMessage(winhandle, CB_GETEDITSEL, Windows.WParam(@startpos), Windows.LParam(@endpos));
  endpos := startpos + NewLength;
  SendMessage(winhandle, CB_SETEDITSEL, 0, MakeLParam(startpos, endpos));
end;

class procedure TWinCEWSCustomComboBox.SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer);
begin
  SendMessage(ACustomComboBox.Handle, CB_SETCURSEL, Windows.WParam(NewIndex), 0);
end;

class procedure TWinCEWSCustomComboBox.SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer);
var
  winhandle: HWND;
begin
  winhandle := ACustomComboBox.Handle;
  SendMessage(winhandle, CB_LIMITTEXT, NewLength, 0);
  GetWindowInfo(winhandle)^.MaxLength := NewLength;
end;

class procedure TWinCEWSCustomComboBox.SetReadOnly(const ACustomComboBox: TCustomComboBox;
  NewReadOnly: boolean);
begin
  RecreateWnd(ACustomComboBox);
end;

class procedure TWinCEWSCustomComboBox.SetText(const AWinControl: TWinControl; const AText: string);
var
  Handle: HWND;
  pwAText: widestring;
begin
  Assert(False, Format('Trace:TWinCEWSCustomComboBox.SetText --> %S', [AText]));
  Handle := AWinControl.Handle;
  pwAText := UTF8Decode(AText);
  if TCustomComboBox(AWinControl).ReadOnly then
    Windows.SendMessageW(Handle, CB_SELECTSTRING, -1, LPARAM(PWideChar(pwAText)))
  else
    Windows.SendMessageW(Handle, WM_SETTEXT, 0, LPARAM(PWideChar(pwAText)));
end;

class function  TWinCEWSCustomComboBox.GetItems(const ACustomComboBox: TCustomComboBox): TStrings;
var
  winhandle: HWND;
begin
  winhandle := ACustomComboBox.Handle;
  Result := TWinCEComboBoxStringList.Create(winhandle, ACustomComboBox);
  GetWindowInfo(winhandle)^.List := Result;
end;

class procedure TWinCEWSCustomComboBox.Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean);
begin
  TWinCEListStringList(AList).Sorted := IsSorted;
end;



{ TWinCEWSCustomEdit helper functions }

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

{ TWinCEWSCustomEdit }

class function TWinCEWSCustomEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
    pClassName := @EditClsName;
    WindowTitle := StrCaption;
    Flags := Flags or ES_AUTOHSCROLL;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // edit is not a transparent control -> no need for parentpainting
  Params.WindowInfo^.needParentPaint := false;
  Result := Params.Window;
end;

class function  TWinCEWSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  Result := EditGetSelStart(ACustomEdit.Handle);
end;

class function  TWinCEWSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  Result := EditGetSelLength(ACustomEdit.Handle);
end;

class function  TWinCEWSCustomEdit.GetMaxLength(const ACustomEdit: TCustomEdit): integer;
begin
  Result := GetWindowInfo(ACustomEdit.Handle)^.MaxLength;
end;

class function  TWinCEWSCustomEdit.GetText(const AWinControl: TWinControl; var AText: string): boolean;
begin
  Result := AWinControl.HandleAllocated;
  if not Result then
    exit;
  AText := GetControlText(AWinControl.Handle);
end;

class procedure TWinCEWSCustomEdit.SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase);
const
  EditStyles: array[TEditCharCase] of integer = (0, ES_UPPERCASE, ES_LOWERCASE);
  EditStyleMask = ES_UPPERCASE or ES_LOWERCASE;
begin
  UpdateWindowStyle(ACustomEdit.Handle, EditStyles[NewCase], EditStyleMask);
end;

class procedure TWinCEWSCustomEdit.SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode);
begin
  // nothing to do, SetPasswordChar will do the work
end;

class procedure TWinCEWSCustomEdit.SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer);
var
  winhandle: HWND;
begin
  winhandle := ACustomEdit.Handle;
  SendMessage(winhandle, EM_LIMITTEXT, NewLength, 0);
  GetWindowInfo(winhandle)^.MaxLength := NewLength;
end;

class procedure TWinCEWSCustomEdit.SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char);
begin
  SendMessage(ACustomEdit.Handle, EM_SETPASSWORDCHAR, WParam(NewChar), 0);
end;

class procedure TWinCEWSCustomEdit.SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
begin
  Windows.SendMessage(ACustomEdit.Handle, EM_SETREADONLY, Windows.WPARAM(NewReadOnly), 0);
end;

class procedure TWinCEWSCustomEdit.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);
begin
  EditSetSelStart(ACustomEdit.Handle, NewStart);
end;

class procedure TWinCEWSCustomEdit.SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer);
begin
  EditSetSelLength(ACustomEdit.Handle, NewLength);
end;

{ TWinCEWSCustomMemo }

class function TWinCEWSCustomMemo.CreateHandle(const AWinControl: TWinControl;
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
    pClassName := @EditClsName;
    WindowTitle := StrCaption;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // memo is not a transparent control -> no need for parentpainting
  Params.WindowInfo^.needParentPaint := false;
  Result := Params.Window;
end;

class function TWinCEWSCustomMemo.GetStrings(const ACustomMemo: TCustomMemo
  ): TStrings;
begin
  Result:=TWinCEMemoStrings.Create(ACustomMemo.Handle, ACustomMemo)
end;

class procedure TWinCEWSCustomMemo.AppendText(const ACustomMemo: TCustomMemo; const AText: string);
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

class procedure TWinCEWSCustomMemo.SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle);
begin
  // TODO: check if can be done without recreation
  RecreateWnd(ACustomMemo);
end;

class procedure TWinCEWSCustomMemo.SetText(const AWinControl: TWinControl; const AText: string);
begin
  SendMessageW(AWinControl.Handle, WM_SETTEXT, 0, LPARAM(PWideChar(UTF8Decode(AText))));
end;

class procedure TWinCEWSCustomMemo.SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean);
begin
  // TODO: check if can be done without recreation
  RecreateWnd(ACustomMemo);
end;

{ TWinCEWSCustomStaticText }
const
  AlignmentToStaticTextFlags: array[TAlignment] of dword = (SS_LEFT, SS_RIGHT, SS_CENTER);

function CalcStaticTextFlags(const Alignment: TAlignment): dword;
begin
  Result := AlignmentToStaticTextFlags[Alignment];
end;

class function TWinCEWSCustomStaticText.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  {$ifdef VerboseWinCE}
  WriteLn('TWinCEWSCustomStaticText.CreateHandle');
  {$endif}

  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  
  // customization of Params
  with Params do
  begin
    pClassName := @LabelClsName;
    WindowTitle := StrCaption;
    Flags := WS_CHILD or WS_VISIBLE or WS_TABSTOP or SS_LEFT;//Flags or CalcStaticTextFlags(TCustomStaticText(AWinControl).Alignment);//is ws_child included?
  end;

  // create window
  FinishCreateWindow(AWinControl, Params, false);
  FreeMem(Params.WindowTitle);
  Result := Params.Window;
end;

class procedure TWinCEWSCustomStaticText.SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment);
begin
  // can not apply on the fly: needs window recreate
  RecreateWnd(ACustomStaticText);
end;

{ TWinCEWSButtonControl }

class procedure TWinCEWSButtonControl.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  if MeasureText(AWinControl, AWinControl.Caption, PreferredWidth, PreferredHeight) then
  begin
    Inc(PreferredWidth, 20);
    Inc(PreferredHeight, 12);
  end;
end;


{ TWinCEWSButton }

{------------------------------------------------------------------------------
  Function: TWinCEWSButton.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TWinCEWSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Params: TCreateWindowExParams;
  str : array[0..255] of WideChar;
begin
  {$ifdef VerboseWinCE}
  WriteLn('TWinCEWSButton.CreateHandle');
  {$endif}

  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);

  // customization of Params
  with Params do
  begin
   // if TCustomButton(AWinControl).Default Then
   //   Flags := Flags or BS_DEFPUSHBUTTON
   // else
   //   Flags := Flags or BS_PUSHBUTTON;
    Flags := WS_CHILD or WS_VISIBLE;
    pClassName := @ButtonClsName;
    WindowTitle := StrCaption;
    Left := AWinControl.Left;
    Top := AWinControl.Top;
    Width := AWinControl.Width;
    Height := AWinControl.Height;
    Parent := AWinControl.Parent.Handle;
    MenuHandle := 0;
  end;

  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;

  {$ifdef VerboseWinCE}
  WriteLn('End Create Button. Handle = ' + IntToStr(Result) +
   ' Left ' + IntToStr(AWinControl.Left) +
   ' Top ' + IntToStr(AWinControl.Top) +
   ' Width ' + IntToStr(AWinControl.Width) +
   ' Height ' + IntToStr(AWinControl.Height) +
   ' ParentHandle ' + IntToStr(AWinControl.Parent.Handle));
  {$endif}
end;

{ TWinCEWSCustomCheckBox }

class function TWinCEWSCustomCheckBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  {$ifdef VerboseWinCE}
  WriteLn('TWinCEWSCustomCheckBox.CreateHandle');
  {$endif}
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ButtonClsName;
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

class procedure TWinCEWSCustomCheckBox.GetPreferredSize(const AWinControl: TWinControl;
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

    // All TCustomCheckBox descendents were consistently too small
    // on autosize, so an extra spacing is added it to fix that
    Inc(PreferredWidth, 10);
  end;
end;

class function  TWinCEWSCustomCheckBox.RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
begin
  case SendMessage(ACustomCheckBox.Handle, BM_GETCHECK, 0, 0) of
    BST_CHECKED:       Result := cbChecked;
    BST_INDETERMINATE: Result := cbGrayed;
  else
    {BST_UNCHECKED:}   Result := cbUnChecked;
  end;
end;

class procedure TWinCEWSCustomCheckBox.SetShortCut(const ACustomCheckBox: TCustomCheckBox;
  const OldShortCut, NewShortCut: TShortCut);
begin
  // TODO: implement me!
end;

class procedure TWinCEWSCustomCheckBox.SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
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

{ TWinCEWSToggleBox }

class function TWinCEWSToggleBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ButtonClsName;
    WindowTitle := StrCaption;
    Flags := Flags or BS_AUTOCHECKBOX or BS_PUSHLIKE;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

{ TWinCEWSRadioButton }

class function TWinCEWSRadioButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  {$ifdef VerboseWinCE}
  WriteLn('TWinCEWSRadioButton.CreateHandle');
  {$endif}

  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ButtonClsName;
    WindowTitle := StrCaption;
    // BS_AUTORADIOBUTTON may hang the application,
    // if the radiobuttons are not consecutive controls.//roozbeh:is it so in wince?
    Flags := Flags or BS_RADIOBUTTON;
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
  RegisterWSComponent(TScrollBar, TWinCEWSScrollBar);
  RegisterWSComponent(TCustomGroupBox, TWinCEWSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TWinCEWSGroupBox);
  RegisterWSComponent(TCustomComboBox, TWinCEWSCustomComboBox);
//  RegisterWSComponent(TComboBox, TWinCEWSComboBox);
  RegisterWSComponent(TCustomListBox, TWinCEWSCustomListBox);
//  RegisterWSComponent(TListBox, TWinCEWSListBox);
  RegisterWSComponent(TCustomEdit, TWinCEWSCustomEdit);
  RegisterWSComponent(TCustomMemo, TWinCEWSCustomMemo);
//  RegisterWSComponent(TEdit, TWinCEWSEdit);
//  RegisterWSComponent(TMemo, TWinCEWSMemo);
//  RegisterWSComponent(TButtonControl, TWinCEWSButtonControl);
  RegisterWSComponent(TCustomButton, TWinCEWSButton);
  RegisterWSComponent(TCustomCheckBox, TWinCEWSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TWinCEWSCheckBox);
  RegisterWSComponent(TToggleBox, TWinCEWSToggleBox);
  RegisterWSComponent(TRadioButton, TWinCEWSRadioButton);
  RegisterWSComponent(TCustomStaticText, TWinCEWSCustomStaticText);
//  RegisterWSComponent(TStaticText, TWinCEWSStaticText);
////////////////////////////////////////////////////
end.
