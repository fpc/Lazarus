{ $Id: WinCEwsstdctrls.pp 8805 2006-02-23 09:40:21Z vincents $}
{
 *****************************************************************************
 *                            WinCEWSStdCtrls.pp                             *
 *                            ------------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WinCEWSStdCtrls;

{$mode objfpc}{$H+}

interface

uses
  // Libs
  Windows,
  {$ifndef win32}oleauto,{$endif}
  // Compatibility
  {$ifdef Win32}win32compat,{$endif}
  // RTL, FCL, LCL
  SysUtils, LCLType, Classes, StdCtrls, Controls, Graphics, Forms, WinCEProc,
  InterfaceBase, LMessages, LCLMessageGlue, LazUTF8, LazUtf8Classes,
  // Widgetset
  WSControls, WSStdCtrls, WSLCLClasses, WinCEInt, WinCEWSControls, WinCEExtra,
  WSProc;

type

  { TWinCEWSScrollBar }

  TWinCEWSScrollBar = class(TWSScrollBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure SetParams(const AScrollBar: TCustomScrollBar); override;
  end;

  { TWinCEWSCustomGroupBox }

  TWinCEWSCustomGroupBox = class(TWSCustomGroupBox)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
  end;

  { TWinCEWSGroupBox }

  TWinCEWSGroupBox = class(TWSGroupBox)
  private
  protected
  public
  end;

  { TWinCEWSCustomComboBox }

  TWinCEWSCustomComboBox = class(TWSCustomComboBox)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); override;
    class function GetDroppedDown(const ACustomComboBox: TCustomComboBox): Boolean; override;
    class function GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetText(const AWinControl: TWinControl; var AText: string): boolean; override;

    class procedure SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox; 
      NewTraverseList: boolean); override;
    class procedure SetDroppedDown(const ACustomComboBox: TCustomComboBox;
       ADroppedDown: Boolean); override;
    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
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
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function GetIndexAtXY(const ACustomListBox: TCustomListBox; X, Y: integer): integer; override;
    class function GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class function GetItemRect(const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean; override;
    class function GetScrollWidth(const ACustomListBox: TCustomListBox): Integer; override;
    class function GetSelCount(const ACustomListBox: TCustomListBox): integer; override;
    class function GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;
    class function GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class function GetTopIndex(const ACustomListBox: TCustomListBox): integer; override;

    class procedure SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean); override;
    class procedure SetBorder(const ACustomListBox: TCustomListBox); override;
    class procedure SetColumnCount(const ACustomListBox: TCustomListBox; ACount: Integer); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class procedure SetScrollWidth(const ACustomListBox: TCustomListBox; const AScrollWidth: Integer); override;
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
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function GetCanUndo(const ACustomEdit: TCustomEdit): Boolean; override;
    class function GetCaretPos(const ACustomEdit: TCustomEdit): TPoint; override;
    class function GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class function GetMaxLength(const ACustomEdit: TCustomEdit): integer; {override;}
    class function GetText(const AWinControl: TWinControl; var AText: string): boolean; override;

    class procedure SetAlignment(const ACustomEdit: TCustomEdit; const AAlignment: TAlignment); override;
    class procedure SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint); override;
    class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetHideSelection(const ACustomEdit: TCustomEdit; NewHideSelection: Boolean); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetNumbersOnly(const ACustomEdit: TCustomEdit; NewNumbersOnly: Boolean); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;

    class procedure Cut(const ACustomEdit: TCustomEdit); override;
    class procedure Copy(const ACustomEdit: TCustomEdit); override;
    class procedure Paste(const ACustomEdit: TCustomEdit); override;
    class procedure Undo(const ACustomEdit: TCustomEdit); override;

    class procedure GetPreferredSize(const AWinControl: TWinControl;
      var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: Boolean); override;
  end;

  { TWinCEWSCustomMemo }

  TWinCEWSCustomMemo = class(TWSCustomMemo)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); override;

    class function  GetCaretPos(const ACustomEdit: TCustomEdit): TPoint; override;
    class function  GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;

    class procedure SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint); override;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); override;
    class procedure ScrollBy(const AWinControl: TWinControl; DeltaX, DeltaY: integer); override;
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
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
    class procedure SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment); override;
    class procedure SetStaticBorderStyle(const ACustomStaticText: TCustomStaticText; const NewBorderStyle: TStaticBorderStyle); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TWinCEWSStaticText }

  TWinCEWSStaticText = class(TWSStaticText)
  private
  protected
  public
  end;

  { TWinCEWSButtonControl }

  TWinCEWSButtonControl = class(TWSButtonControl)
  published
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
  end;

  { TWinCEWSButton }

  TWinCEWSButton = class(TWSButton)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
//    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
//    class procedure SetShortcut(const AButton: TCustomButton; const ShortCutK1, ShortCutK2: TShortcut); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
      var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: Boolean); override;
  end;

  { TWinCEWSCustomCheckBox }

  TWinCEWSCustomCheckBox = class(TWSCustomCheckBox)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox; const ShortCutK1, ShortCutK2: TShortCut); override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;
    class procedure SetAlignment(const ACustomCheckBox: TCustomCheckBox; const NewAlignment: TLeftRight); override;
  end;

  { TWinCEWSCheckBox }

  TWinCEWSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TWinCEWSToggleBox }

  TWinCEWSToggleBox = class(TWSToggleBox)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
  end;

  { TWinCEWSRadioButton }

  TWinCEWSRadioButton = class(TWSRadioButton)
  published
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

const
  AlignmentToEditFlags: array[TAlignment] of DWord =
  (
{ taLeftJustify  } ES_LEFT,
{ taRightJustify } ES_RIGHT,
{ taCenter       } ES_CENTER
  );

  AlignmentToStaticTextFlags: array[TAlignment] of DWord =
  (
{ taLeftJustify  } SS_LEFT,
{ taRightJustify } SS_RIGHT,
{ taCenter       } SS_CENTER
  );

  BorderToStaticTextFlags: array[TStaticBorderStyle] of DWord =
  (
    0,
    WS_BORDER, // generic border
    WS_BORDER  // SS_SUNKEN is not supported
  );

  AccelCharToStaticTextFlags: array[Boolean] of LONG =
  (
    SS_NOPREFIX,
    0
  );

{$I wincememostrings.inc}


function ScrollBarWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; {$ifdef win32}stdcall;{$else}cdecl;{$endif}
begin
  case Msg of
    WM_PAINT,
    WM_PRINTCLIENT,
    WM_ERASEBKGND:
      begin
        Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
        Exit;
      end;
  end;
  Result := WindowProc(Window, Msg, WParam, LParam);
end;

{ TWinCEWSScrollBar }

class function TWinCEWSScrollBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ScrollBarClsName;
    SubClassWndProc := @ScrollBarWindowProc;
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
    AMax := Max - 1;
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
    //DebugLn('Trace:TODO: [TWinCEWSScrollBar.SetParams] Set up step and page increments for csScrollBar');
  end;
end;

{ TWinCEWSCustomGroupBox }

// Don't choose too much which messages to send to WindowProc or else
// events on controls inside the panel will stop working, see bug 16530
function GroupBoxPanelWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; {$ifdef win32}stdcall{$else}cdecl{$endif};
begin
  Result := WindowProc(Window, Msg, WParam, LParam);
end;

class function TWinCEWSCustomGroupBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    SubClassWndProc := @GroupBoxPanelWindowProc;
    pClassName := @ButtonClsName;
    WindowTitle := StrCaption;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

{ TWinCEWSCustomListBox }

class function TWinCEWSCustomListBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
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

      if (AWinControl.FCompStyle = csCheckListBox) and (Style = lbStandard) then
        Flags := Flags or LBS_OWNERDRAWFIXED
      else
        case Style of
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

class function TWinCEWSCustomListBox.GetIndexAtXY(
  const ACustomListBox: TCustomListBox; X, Y: integer): integer;
begin
  Result := Windows.SendMessage(ACustomListBox.Handle, LB_ITEMFROMPOINT, 0, MakeLParam(X,Y));
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
    //DebugLn('Trace:[TWinCEWSCustomListBox.GetItemIndex] could not retrieve itemindex, try selecting an item first');
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

class function TWinCEWSCustomListBox.GetScrollWidth(const ACustomListBox: TCustomListBox): Integer;
begin
  Result := Windows.SendMessage(ACustomListBox.Handle, LB_GETHORIZONTALEXTENT, 0, 0);
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

class procedure TWinCEWSCustomListBox.SetScrollWidth(
  const ACustomListBox: TCustomListBox; const AScrollWidth: Integer);
begin
  Windows.SendMessage(ACustomListBox.Handle, LB_SETHORIZONTALEXTENT, AScrollWidth, 0);
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

{
    Obs:
    CBS_SIMPLE, CBS_OWNERDRAWFIXED and CBS_OWNERDRAWVARIABLE
    are unsupported in Windows CE
}
const
  ComboBoxStylesMask = CBS_DROPDOWN or CBS_DROPDOWN or CBS_DROPDOWNLIST
    { or CBS_OWNERDRAWFIXED or CBS_OWNERDRAWVARIABLE};

function CalcComboBoxWinFlags(AComboBox: TCustomComboBox): dword;
const
  ComboBoxStyles: array[TComboBoxStyle] of dword = (
    CBS_DROPDOWN,
    0 {CBS_SIMPLE},
    CBS_DROPDOWNLIST,
    0 or CBS_DROPDOWNLIST {CBS_OWNERDRAWFIXED},
    0 or CBS_DROPDOWNLIST {CBS_OWNERDRAWVARIABLE},
    0 or CBS_DROPDOWN {CBS_OWNERDRAWFIXED},
    0 or CBS_DROPDOWN {CBS_OWNERDRAWVARIABLE}
    );
begin
  Result := ComboBoxStyles[AComboBox.Style];
end;

class function TWinCEWSCustomComboBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    // The following styles are suposed to be unsupported:
    // CBS_SIMPLE or CBS_OWNERDRAWFIXED or CBS_OWNERDRAWVARIABLE
    // But they work anyway, at least on the WM 6 Emulator
    // So don't remove them or else you will cause this bug:
    // http://bugs.freepascal.org/view.php?id=16627
    pClassName := @ComboboxClsName;
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

class function TWinCEWSCustomComboBox.GetDroppedDown(
  const ACustomComboBox: TCustomComboBox): Boolean;
begin
  if not WSCheckHandleAllocated(ACustomComboBox, 'TWinCEWSCustomComboBox.GetDroppedDown') then
    Exit(False);
  Result := LongBool(SendMessage(ACustomComboBox.Handle, CB_GETDROPPEDSTATE, 0, 0));
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
    //DebugLn('Trace:[TWinCEWidgetSet.IntSendMessage3] Could not retrieve item index '+
    //    'via LM_GETITEMINDEX; try selecting an item first');
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

class procedure TWinCEWSCustomComboBox.SetDroppedDown(
  const ACustomComboBox: TCustomComboBox; ADroppedDown: Boolean);
begin
  if WSCheckHandleAllocated(ACustomComboBox, 'TWin32WSCustomComboBox.SetDroppedDown') then
    SendMessage(ACustomComboBox.Handle, CB_SHOWDROPDOWN, WPARAM(ADroppedDown), 0);
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

class procedure TWinCEWSCustomComboBox.SetText(const AWinControl: TWinControl; const AText: string);
var
  Handle: HWND;
  pwAText: widestring;
begin
  //DebugLn(Format('Trace:TWinCEWSCustomComboBox.SetText --> %S', [AText]));
  Handle := AWinControl.Handle;
  pwAText := UTF8Decode(AText);

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
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @EditClsName;
    WindowTitle := StrCaption;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // edit is not a transparent control -> no need for parentpainting
  Params.WindowInfo^.needParentPaint := false;
  Result := Params.Window;
end;

class function TWinCEWSCustomEdit.GetCanUndo(const ACustomEdit: TCustomEdit
  ): Boolean;
begin
  Result := False;
  if not WSCheckHandleAllocated(ACustomEdit, 'GetCanUndo') then
    Exit;
  Result := Windows.SendMessage(ACustomEdit.Handle, EM_CANUNDO, 0, 0) <> 0;
end;

class function TWinCEWSCustomEdit.GetCaretPos(const ACustomEdit: TCustomEdit): TPoint;
var
  BufferX: Longword;
begin
  // EM_GETSEL expects a pointer to 32-bits buffer in lParam
  Windows.SendMessage(ACustomEdit.Handle, EM_GETSEL, 0, PtrInt(@BufferX));
  Result.X := BufferX;
  Result.Y := 0;
end;

class function TWinCEWSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  Result := EditGetSelStart(ACustomEdit.Handle);
end;

class function TWinCEWSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  Result := EditGetSelLength(ACustomEdit.Handle);
end;

class function TWinCEWSCustomEdit.GetMaxLength(const ACustomEdit: TCustomEdit): integer;
begin
  Result := GetWindowInfo(ACustomEdit.Handle)^.MaxLength;
end;

class function TWinCEWSCustomEdit.GetText(const AWinControl: TWinControl; var AText: string): boolean;
begin
  Result := AWinControl.HandleAllocated;
  if not Result then
    exit;
  AText := GetControlText(AWinControl.Handle);
end;

class procedure TWinCEWSCustomEdit.SetAlignment(const ACustomEdit: TCustomEdit;
  const AAlignment: TAlignment);
var
  CurrentStyle: DWord;
begin
  CurrentStyle := GetWindowLong(ACustomEdit.Handle, GWL_STYLE);
  if (CurrentStyle and 3) = AlignmentToEditFlags[AAlignment] then
    Exit;
  RecreateWnd(ACustomEdit);
end;

class procedure TWinCEWSCustomEdit.SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint);
begin
  Windows.SendMessage(ACustomEdit.Handle, EM_SETSEL, NewPos.X, NewPos.X);
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

class procedure TWinCEWSCustomEdit.SetHideSelection(const ACustomEdit: TCustomEdit; NewHideSelection: Boolean);
var
  CurrentStyle: DWord;
begin
  CurrentStyle := GetWindowLong(ACustomEdit.Handle, GWL_STYLE);
  if (CurrentStyle and ES_NOHIDESEL = 0) = NewHideSelection  then
    Exit;
  RecreateWnd(ACustomEdit);
end;

class procedure TWinCEWSCustomEdit.SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer);
var
  winhandle: HWND;
begin
  winhandle := ACustomEdit.Handle;
  SendMessage(winhandle, EM_LIMITTEXT, NewLength, 0);
  GetWindowInfo(winhandle)^.MaxLength := NewLength;
end;

class procedure TWinCEWSCustomEdit.SetNumbersOnly(const ACustomEdit: TCustomEdit; NewNumbersOnly: Boolean);
const
  EditStyles: array[Boolean] of integer = (0, ES_NUMBER);
  EditStyleMask = ES_NUMBER;
begin
  UpdateWindowStyle(ACustomEdit.Handle, EditStyles[NewNumbersOnly], EditStyleMask);
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

class procedure TWinCEWSCustomEdit.Cut(const ACustomEdit: TCustomEdit);
begin
  SendMessage(ACustomEdit.Handle, WM_CUT, 0, 0)
end;

class procedure TWinCEWSCustomEdit.Copy(const ACustomEdit: TCustomEdit);
begin
  SendMessage(ACustomEdit.Handle, WM_COPY, 0, 0)
end;

class procedure TWinCEWSCustomEdit.Paste(const ACustomEdit: TCustomEdit);
begin
  SendMessage(ACustomEdit.Handle, WM_PASTE, 0, 0)
end;

class procedure TWinCEWSCustomEdit.Undo(const ACustomEdit: TCustomEdit);
begin
  SendMessage(ACustomEdit.Handle, EM_UNDO, 0, 0)
end;

class procedure TWinCEWSCustomEdit.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  if MeasureText(AWinControl, AWinControl.Caption, PreferredWidth, PreferredHeight) then
  begin
    Inc(PreferredWidth, 5);
    Inc(PreferredHeight, 5);
  end;
end;

{ TWinCEWSCustomMemo }

class function TWinCEWSCustomMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
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

class procedure TWinCEWSCustomMemo.SetCaretPos(const ACustomEdit: TCustomEdit;
  const NewPos: TPoint);
var
  CharIndex: Longword;
begin
  { EM_LINEINDEX returns the char index of a given line }
  CharIndex := Windows.SendMessage(ACustomEdit.Handle, EM_LINEINDEX, NewPos.Y, 0) + NewPos.X;
  { EM_SETSEL expects the character position in char index, which
    doesn't go back to zero in new lines
  }
  Windows.SendMessage(ACustomEdit.Handle, EM_SETSEL, CharIndex, CharIndex);
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

class function TWinCEWSCustomMemo.GetCaretPos(const ACustomEdit: TCustomEdit): TPoint;
var
  BufferX: Longword;
begin
  { X position calculation }

  { EM_GETSEL returns the char index of the caret, but this index
    doesn't go back to zero in new lines, so we need to subtract
    the char index from the line

    EM_GETSEL expects a pointer to 32-bits buffer in lParam
  }
  Windows.SendMessage(ACustomEdit.Handle, EM_GETSEL, 0, PtrInt(@BufferX));
  { EM_LINEINDEX returns the char index of a given line
    wParam = -1 indicates the line of the caret
  }
  Result.X := BufferX - Windows.SendMessage(ACustomEdit.Handle, EM_LINEINDEX, WPARAM(-1), 0);

  { Y position calculation }

  { EM_LINEFROMCHAR returns the number of the line of a given
    char index.
  }
  Result.Y := Windows.SendMessage(ACustomEdit.Handle, EM_LINEFROMCHAR, BufferX, 0);
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

class procedure TWinCEWSCustomMemo.ScrollBy(const AWinControl: TWinControl;
  DeltaX, DeltaY: integer);
begin
  SendMessage(AWinControl.Handle, EM_LINESCROLL, -DeltaX, -DeltaY);
end;

{ TWinCEWSCustomStaticText }

function CalcStaticTextFlags(
   const AAlignment: TAlignment;
   const ABorder: TStaticBorderStyle;
   const AShowAccelChar: Boolean): dword;
begin
  Result :=
   AlignmentToStaticTextFlags[AAlignment] or
   BorderToStaticTextFlags[ABorder] or
   DWORD(AccelCharToStaticTextFlags[AShowAccelChar]);
end;

class function TWinCEWSCustomStaticText.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @LabelClsName;
    WindowTitle := StrCaption;
    Flags := Flags or SS_NOTIFY or
      CalcStaticTextFlags(TCustomStaticText(AWinControl).Alignment,
       TCustomStaticText(AWinControl).BorderStyle,
       TCustomStaticText(AWinControl).ShowAccelChar);
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

class procedure TWinCEWSCustomStaticText.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  if MeasureText(AWinControl, AWinControl.Caption, PreferredWidth, PreferredHeight) then
  begin
    Inc(PreferredHeight);
    if TCustomStaticText(AWinControl).BorderStyle <> sbsNone then
    begin
      Inc(PreferredWidth, 2);
      Inc(PreferredHeight, 2);
    end;
  end;
end;

class procedure TWinCEWSCustomStaticText.SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment);
begin
  if not WSCheckHandleAllocated(ACustomStaticText, 'SetAlignment') then
    exit;
  // can not apply on the fly: needs window recreate
  RecreateWnd(ACustomStaticText);
end;

class procedure TWinCEWSCustomStaticText.SetStaticBorderStyle(
  const ACustomStaticText: TCustomStaticText;
  const NewBorderStyle: TStaticBorderStyle);
begin
  if not WSCheckHandleAllocated(ACustomStaticText, 'SetStaticBorderStyle') then
    exit;
  // can not apply on the fly: needs window recreate
  RecreateWnd(ACustomStaticText);
end;

class procedure TWinCEWSCustomStaticText.SetText(const AWinControl: TWinControl;
  const AText: string);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetText') then
    exit;

  // maybe we need TWSCustomStaticText.SetShowAccelChar ?

  if (GetWindowLong(AWinControl.Handle, GWL_STYLE) and SS_NOPREFIX) <>
     AccelCharToStaticTextFlags[TCustomStaticText(AWinControl).ShowAccelChar] then
    RecreateWnd(AWinControl);

  TWSWinControlClass(ClassParent).SetText(AWinControl, AText);
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
  {$ifdef WinCEDebugHiRes}
  DebugLn(Format('[TWinCEWSButtonControl.GetPreferredSize] CX %d CY %d',
    [PreferredWidth, PreferredHeight]));
  {$endif}
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
  PreferredWidth: integer;
  PreferredHeight: integer;
begin
  {$ifdef VerboseWinCE}
  DebugLn('TWinCEWSButton.CreateHandle');
  {$endif}

  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);

  // customization of Params
  with Params do
  begin
    Flags := WS_CHILD or WS_VISIBLE;
    pClassName := @ButtonClsName;
    WindowTitle := StrCaption;
    MenuHandle := 0;
  end;

  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;

  {$ifdef VerboseWinCE}
  DebugLn('End Create Button. Handle = ' + IntToStr(Result) +
   ' Left ' + IntToStr(AWinControl.Left) +
   ' Top ' + IntToStr(AWinControl.Top) +
   ' Width ' + IntToStr(AWinControl.Width) +
   ' Height ' + IntToStr(AWinControl.Height) +
   ' ParentHandle ' + IntToStr(AWinControl.Parent.Handle));
  {$endif}
end;

class procedure TWinCEWSButton.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  TWinCEWSButtonControl.GetPreferredSize(AWinControl, PreferredWidth, PreferredHeight,
    WithThemeSpace);
end;

{ TWinCEWSCustomCheckBox }

class function TWinCEWSCustomCheckBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  {$ifdef VerboseWinCE}
  DebugLn('TWinCEWSCustomCheckBox.CreateHandle');
  {$endif}
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ButtonClsName;
    WindowTitle := StrCaption;
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
    if WithThemeSpace then
    begin
      Inc(PreferredWidth, 6);
      Inc(PreferredHeight, 6);
    end;

    // All TCustomCheckBox descendents were consistently too small
    // on autosize, so an extra spacing is added it to fix that
    Inc(PreferredWidth, 10);

    // In Hi-res aware software the checkbox width needs to be even larger
    if IsHiResMode() then
      Inc(PreferredWidth, 20);
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
  const ShortCutK1, ShortCutK2: TShortCut);
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
  //Pass SKIP_LMCHANGE through lParam to avoid the OnChange event be fired
  Windows.SendMessage(ACustomCheckBox.Handle, BM_SETCHECK, Flags, SKIP_LMCHANGE);
end;

class procedure TWinCEWSCustomCheckBox.SetAlignment(
  const ACustomCheckBox: TCustomCheckBox; const NewAlignment: TLeftRight);
begin
  RecreateWnd(ACustomCheckBox);
end;


{ TWinCEWSToggleBox }

class function TWinCEWSToggleBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ButtonClsName;
    WindowTitle := StrCaption;
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
  DebugLn('TWinCEWSRadioButton.CreateHandle');
  {$endif}

  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ButtonClsName;
    WindowTitle := StrCaption;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

end.
