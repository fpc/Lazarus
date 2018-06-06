{ $Id$}
{
 *****************************************************************************
 *                            Win32WSStdCtrls.pp                             *
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
  Classes, SysUtils, CommCtrl,
  StdCtrls, Controls, Graphics, Forms, Themes,
////////////////////////////////////////////////////
  WSControls, WSStdCtrls, WSLCLClasses, WSProc, Windows, LCLIntf, LCLType,
  LazUTF8, LazUtf8Classes, InterfaceBase, LMessages, LCLMessageGlue, TextStrings,
  Win32Int, Win32Proc, Win32WSControls, Win32Extra, Win32Themes;

type

  { TWin32WSScrollBar }

  TWin32WSScrollBar = class(TWSScrollBar)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function GetDoubleBuffered(const AWinControl: TWinControl): Boolean; override;
    class procedure SetParams(const AScrollBar: TCustomScrollBar); override;
  end;

  { TWin32WSCustomGroupBox }

  TWin32WSCustomGroupBox = class(TWSCustomGroupBox)
  published
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign,
      UseRightToLeftReading, UseRightToLeftScrollBar : Boolean); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
  end;

  { TWin32WSGroupBox }

  TWin32WSGroupBox = class(TWSGroupBox)
  published
  end;

  { TWin32WSCustomComboBox }

  TWin32WSCustomComboBox = class(TWSCustomComboBox)
  private
    class function GetStringList(const ACustomComboBox: TCustomComboBox): TWin32ComboBoxStringList;
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); override;
    class function GetDoubleBuffered(const AWinControl: TWinControl): Boolean; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
      var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    class function GetDroppedDown(const ACustomComboBox: TCustomComboBox): Boolean; override;
    class function GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetText(const AWinControl: TWinControl; var AText: string): boolean; override;

    class procedure SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox;
      NewTraverseList: boolean); override;
    class procedure SetDropDownCount(const ACustomComboBox: TCustomComboBox; NewCount: Integer); override;
    class procedure SetDroppedDown(const ACustomComboBox: TCustomComboBox;
       ADroppedDown: Boolean); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); override;

    class function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
    class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); override;

    class function GetItemHeight(const ACustomComboBox: TCustomComboBox): Integer; override;
    class procedure SetItemHeight(const ACustomComboBox: TCustomComboBox; const AItemHeight: Integer); override;
  end;

  { TWin32WSComboBox }

  TWin32WSComboBox = class(TWSComboBox)
  published
  end;

  { TWin32WSCustomListBox }

  TWin32WSCustomListBox = class(TWSCustomListBox)
  published
    class procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); override;
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DragStart(const ACustomListBox: TCustomListBox); override;

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

  { TWin32WSListBox }

  TWin32WSListBox = class(TWSListBox)
  published
  end;

  { TWin32WSCustomEdit }

  TWin32WSCustomEdit = class(TWSCustomEdit)
  private
    class procedure ApplyMargins(const AWinControl: TWinControl);
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function GetCanUndo(const ACustomEdit: TCustomEdit): Boolean; override;
    class function GetCaretPos(const ACustomEdit: TCustomEdit): TPoint; override;
    class function GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class function GetMaxLength(const ACustomEdit: TCustomEdit): integer; {override;}
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
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
    class procedure SetSelText(const ACustomEdit: TCustomEdit; const NewSelText: string); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetTextHint(const ACustomEdit: TCustomEdit; const ATextHint: string); override;

    class procedure Cut(const ACustomEdit: TCustomEdit); override;
    class procedure Copy(const ACustomEdit: TCustomEdit); override;
    class procedure Paste(const ACustomEdit: TCustomEdit); override;
    class procedure Undo(const ACustomEdit: TCustomEdit); override;
  end;

  { TWin32WSCustomMemo }

  TWin32WSCustomMemo = class(TWSCustomMemo)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); override;

    class function  GetCaretPos(const ACustomEdit: TCustomEdit): TPoint; override;
    class function  GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;

    class procedure SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint); override;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); override;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); override;
    class procedure ScrollBy(const AWinControl: TWinControl; DeltaX, DeltaY: integer); override;
    class procedure SetSelText(const ACustomEdit: TCustomEdit; const NewSelText: string); override;
  end;

  { TWin32WSEdit }

  TWin32WSEdit = class(TWSEdit)
  published
  end;

  { TWin32WSMemo }

  TWin32WSMemo = class(TWSMemo)
  published
  end;

  { TWin32WSCustomStaticText }

  TWin32WSCustomStaticText = class(TWSCustomStaticText)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
    class procedure SetBiDiMode(const AWinControl: TWinControl;
       UseRightToLeftAlign, UseRightToLeftReading,
       UseRightToLeftScrollBar: Boolean); override;
    class procedure SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment); override;
    class procedure SetStaticBorderStyle(const ACustomStaticText: TCustomStaticText; const NewBorderStyle: TStaticBorderStyle); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TWin32WSStaticText }

  TWin32WSStaticText = class(TWSStaticText)
  published
  end;

  { TWin32WSButtonControl }

  TWin32WSButtonControl = class(TWSButtonControl)
  published
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
  end;

  { TWin32WSButton }

  TWin32WSButton = class(TWSButton)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function GetDoubleBuffered(const AWinControl: TWinControl): Boolean; override;
    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
    class procedure SetShortCut(const AButton: TCustomButton; const ShortCutK1, ShortCutK2: TShortCut); override;
  end;

  { TWin32WSCustomCheckBox }

  TWin32WSCustomCheckBox = class(TWSCustomCheckBox)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function GetDoubleBuffered(const AWinControl: TWinControl): Boolean; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
    class function RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox; const ShortCutK1, ShortCutK2: TShortCut); override;
    class procedure SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign,
      UseRightToLeftReading, UseRightToLeftScrollBar : Boolean); override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;
    class procedure SetAlignment(const ACustomCheckBox: TCustomCheckBox; const NewAlignment: TLeftRight); override;
  end;

  { TWin32WSCheckBox }

  TWin32WSCheckBox = class(TWSCheckBox)
  published
  end;

  { TWin32WSToggleBox }

  TWin32WSToggleBox = class(TWSToggleBox)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
  end;

  { TWin32WSRadioButton }

  TWin32WSRadioButton = class(TWSRadioButton)
  published
    class function CreateHandle(const AWinControl: TWinControl;
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

function ListBoxWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;

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
    SS_SUNKEN  // the only one special border for text static controls
  );

  AccelCharToStaticTextFlags: array[Boolean] of LONG =
  (
    SS_NOPREFIX,
    0
  );

{$I win32memostrings.inc}

type
  TWinControlAccess = class(TWinControl);

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
  WindowInfo: PWin32WindowInfo;
  NCCreateParams: PNCCreateParams;
  LMessage: TLMessage;
begin
  // darn MS: if combobox has edit control, and combobox receives focus, it
  // passes it on to the edit, so it will send a WM_KILLFOCUS; inhibit
  // also don't pass WM_SETFOCUS to the lcl,
  // it will get one from the edit control
  case Msg of
    WM_NCCREATE:
      begin
        NCCreateParams := PCREATESTRUCT(lParam)^.lpCreateParams;
        if Assigned(NCCreateParams) then
        begin
          WindowInfo := AllocWindowInfo(Window);
          WindowInfo^.WinControl := NCCreateParams^.WinControl;
          WindowInfo^.WinControl.Handle := Window;
          WindowInfo^.DefWndProc := NCCreateParams^.DefWndProc;
          WindowInfo^.needParentPaint := False;
          SetWindowLong(Window, GWL_ID, PtrInt(NCCreateParams^.WinControl));
          NCCreateParams^.Handled := True;
        end;
      end;
    WM_KILLFOCUS, WM_SETFOCUS:
      begin
        Info.cbSize := SizeOf(Info);
        Win32Extra.GetComboBoxInfo(Window, @Info);
        if (HWND(WParam) = Info.hwndItem) or (HWND(WParam) = Info.hwndList) then
        begin
          // continue normal processing, don't send to lcl
          Exit(CallDefaultWindowProc(Window, Msg, WParam, LParam));
        end;
      end;
    WM_PAINT,
    WM_ERASEBKGND:
      begin
        WindowInfo := GetWin32WindowInfo(Window);
        if not TWSWinControlClass(WindowInfo^.WinControl.WidgetSetClass).GetDoubleBuffered(WindowInfo^.WinControl) then
        begin
          LMessage.msg := Msg;
          LMessage.wParam := WParam;
          LMessage.lParam := LParam;
          LMessage.Result := 0;
          Exit(DeliverMessage(WindowInfo^.WinControl, LMessage));
        end
        else
          Exit(WindowProc(Window, Msg, WParam, LParam));
      end;
    WM_PRINTCLIENT:
      Exit(CallDefaultWindowProc(Window, Msg, WParam, LParam));
    WM_MEASUREITEM:
      begin
        WindowInfo := GetWin32WindowInfo(Window);
        LMessage.Msg := LM_MEASUREITEM;
        LMessage.LParam := LParam;
        LMessage.WParam := WParam;
        LMessage.Result := 0;
        Exit(DeliverMessage(WindowInfo^.WinControl, LMessage));
      end;
  end;
  // normal processing
  Result := WindowProc(Window, Msg, WParam, LParam);
end;

function ScrollBarWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  LMessage: TLMessage;
  Control: TWinControl;
begin
  case Msg of
    WM_PRINTCLIENT: Exit(CallDefaultWindowProc(Window, Msg, WParam, LParam));
    WM_PAINT,
    WM_ERASEBKGND:
      begin
        Control := GetWin32WindowInfo(Window)^.WinControl;
        if not TWSWinControlClass(Control.WidgetSetClass).GetDoubleBuffered(Control) then
        begin
          LMessage.msg := Msg;
          LMessage.wParam := WParam;
          LMessage.lParam := LParam;
          LMessage.Result := 0;
          Result := DeliverMessage(Control, LMessage);
          Exit;
        end;
      end;
  end;
  Result := WindowProc(Window, Msg, WParam, LParam);
end;

{ TWin32WSScrollBar }

class function TWin32WSScrollBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := 'SCROLLBAR';
    SubClassWndProc := @ScrollBarWindowProc;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

class function TWin32WSScrollBar.GetDoubleBuffered(
  const AWinControl: TWinControl): Boolean;
begin
  Result := GetWin32NativeDoubleBuffered(AWinControl); // double buffered scrollbar flickers on mouse-in/mouse-out on Windows 10
end;

class procedure TWin32WSScrollBar.SetParams(const AScrollBar: TCustomScrollBar);
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
  end;
end;

{ TWin32WSCustomGroupBox }

function GroupBoxWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  Info: PWin32WindowInfo;
  DC: HDC;
  Flags: Cardinal;
  ARect: TRect;
  WideBuffer: WideString;
  GroupBox: TCustomGroupBox;
begin
  // move groupbox specific code here
  case Msg of
    WM_NCHITTEST:
      begin
        Result := HTCLIENT;
        Exit;
      end;
    WM_ENABLE:
      begin
        Result := WindowProc(Window, Msg, WParam, LParam);
        // if it is groupbox and themed app then invalidate it on enable change
        // to redraw graphic controls on it (issue 0007877)
        if ThemeServices.ThemesAvailable then
          InvalidateRect(Window, nil, True);
        Exit;
      end;
    WM_PAINT:
      begin
        Result := WindowProc(Window, Msg, WParam, LParam);
        // bug in comctrl32.dll see Mantis #27491, we have to paint a grayed
        // caption if groupbox is disabled
        if ThemeServices.ThemesEnabled then
        begin
          Info := GetWin32WindowInfo(Window);
          if Assigned(Info) and (Info^.WinControl is TCustomGroupBox)
          and not TCustomGroupBox(Info^.WinControl).Enabled then
          begin
            GroupBox := TCustomGroupBox(Info^.WinControl);
            DC := Windows.GetDC(Window);
            SetBkMode(DC, TRANSPARENT);
            SetTextColor(DC, GetSysColor(COLOR_GRAYTEXT));
            SelectObject(DC, GroupBox.Font.Reference.Handle);
            Flags := 0;
            ARect := Classes.Rect(0, 0, 0, 0);
            WideBuffer := UTF8ToUTF16(TCustomGroupBox(Info^.WinControl).Caption);
            DrawTextW(DC, PWideChar(WideBuffer), Length(WideBuffer), ARect, Flags or DT_CALCRECT);
            if GroupBox.BiDiMode = bdRightToLeft then
            begin
              Flags := Flags or DT_RIGHT;
              OffsetRect(ARect, GroupBox.Width - ARect.Right - 7, 0);
            end
            else
              OffsetRect(ARect, 9, 0);
            DrawTextW(DC, PWideChar(WideBuffer), Length(WideBuffer), ARect, Flags);
            ReleaseDC(Window, DC);
          end;
        end;
        Exit;
      end;
  end;
  Result := WindowProc(Window, Msg, WParam, LParam);
end;

function GroupBoxParentMsgHandler(const AWinControl: TWinControl; Window: HWnd;
      Msg: UInt; WParam: Windows.WParam; LParam: Windows.LParam;
      var MsgResult: Windows.LResult; var WinProcess: Boolean): Boolean;
var
  Info: PWin32WindowInfo;
begin
  Result := False;
  case Msg of
    WM_CTLCOLORSTATIC:
    begin
      Info := GetWin32WindowInfo(HWND(LParam));
      Result := Assigned(Info) and ThemeServices.ThemesEnabled and (Info^.WinControl.Color = AWinControl.Color);
      if Result then
      begin
        ThemeServices.DrawParentBackground(HWND(LParam), HDC(WParam), nil, False);
        MsgResult := GetStockObject(HOLLOW_BRUSH);
        WinProcess := False;
        SetBkMode(HDC(WParam), TRANSPARENT);
      end;
    end;
  end;
end;

class function TWin32WSCustomGroupBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    SubClassWndProc := @GroupBoxWindowProc;
    pClassName := @ButtonClsName[0];
    WindowTitle := StrCaption;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, False);
  Result := Params.Window;
  Params.WindowInfo^.ParentMsgHandler := @GroupBoxParentMsgHandler;
end;

class procedure TWin32WSCustomGroupBox.SetBiDiMode(
  const AWinControl: TWinControl; UseRightToLeftAlign,
  UseRightToLeftReading, UseRightToLeftScrollBar : Boolean);
begin
  RecreateWnd(AWinControl);
end;

class procedure TWin32WSCustomGroupBox.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  I: Integer;
begin
  TWin32WSWinControl.SetFont(AWinControl, AFont);

  TWinControlAccess(AWinControl).InvalidateBoundsRealized;
  for I := 0 to AWinControl.ControlCount-1 do
    if AWinControl.Controls[I] is TWinControl then
      TWinControlAccess(AWinControl.Controls[I]).InvalidateBoundsRealized;
  TWinControlAccess(AWinControl).RealizeBoundsRecursive;
end;

class procedure TWin32WSCustomGroupBox.SetText(const AWinControl: TWinControl;
  const AText: string);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetText') then Exit;
  TWin32WSWinControl.SetText(AWinControl, AText);
  AWinControl.Invalidate;
end;

class procedure TWin32WSCustomGroupBox.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  if MeasureText(AWinControl, AWinControl.Caption, PreferredWidth,
                 PreferredHeight) then begin
    PreferredWidth += 19;
    PreferredHeight += 4;
  end;
end;

{ TWin32WSCustomListBox }

function ListBoxWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  WindowInfo: PWin32WindowInfo;
  NCCreateParams: PNCCreateParams;
  LMessage: TLMessage;
  Count: LResult;
  Top: Integer;
  ARect: TRect;
begin
  case Msg of
    WM_NCCREATE:
      begin
        NCCreateParams := PCREATESTRUCT(lParam)^.lpCreateParams;
        if Assigned(NCCreateParams) then
        begin
          WindowInfo := AllocWindowInfo(Window);
          WindowInfo^.WinControl := NCCreateParams^.WinControl;
          WindowInfo^.WinControl.Handle := Window;
          WindowInfo^.DefWndProc := NCCreateParams^.DefWndProc;
          // listbox is not a transparent control -> no need for parentpainting
          WindowInfo^.needParentPaint := False;
          SetWindowLong(Window, GWL_ID, PtrInt(NCCreateParams^.WinControl));
          NCCreateParams^.Handled := True;
        end;
      end;
    WM_MEASUREITEM:
      begin
        WindowInfo := GetWin32WindowInfo(Window);
        LMessage.Msg := LM_MEASUREITEM;
        LMessage.LParam := LParam;
        LMessage.WParam := WParam;
        LMessage.Result := 0;
        Exit(DeliverMessage(WindowInfo^.WinControl, LMessage));
      end;
    WM_ERASEBKGND:
      begin
        WindowInfo := GetWin32WindowInfo(Window);
        if ((WindowsVersion <= wvServer2003) or not ThemeServices.ThemesEnabled) then
        begin
          if Assigned(WindowInfo^.WinControl) and not
             (TCustomListbox(WindowInfo^.WinControl).Style in [lbOwnerDrawFixed, lbOwnerDrawVariable])
          then begin
            // Standard behavior for XP/WinServer2003, no themes, no OwnerDraw
            Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
            exit;
          end
        end;
        // Avoid unnecessary background paints to avoid flickering of the listbox
        Count := SendMessage(Window, LB_GETCOUNT, 0, 0);
        if Assigned(WindowInfo^.WinControl) and
          (TCustomListBox(WindowInfo^.WinControl).Columns < 2) and
          (Count <> LB_ERR) and (SendMessage(Window, LB_GETITEMRECT, Count - 1, Windows.LParam(@ARect)) <> LB_ERR) then
        begin
          Top := ARect.Bottom;
          Windows.GetClientRect(Window, ARect);
          ARect.Top := Top;
          if not IsRectEmpty(ARect) then
            Windows.FillRect(HDC(WParam), ARect, WindowInfo^.WinControl.Brush.Reference.Handle);
          Result := 1;
        end
        else
          Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
        Exit;
      end;
  end;
  // normal processing
  Result := WindowProc(Window, Msg, WParam, LParam);
end;

class procedure TWin32WSCustomListBox.AdaptBounds(
  const AWinControl: TWinControl; var Left, Top, Width, Height: integer;
  var SuppressMove: boolean);
var
  ColCount: Integer;
  DW: Integer;
  ARect: TRect;
begin
  ColCount := TCustomListBox(AWinControl).Columns;
  if ColCount > 1 then
  begin
    // Listbox has a border and Width argument is a window rect =>
    // Decrease it by border width
    Windows.GetClientRect(AWinControl.Handle, ARect);
    DW := ARect.Right - ARect.Left;
    Windows.GetWindowRect(AWinControl.Handle, ARect);
    DW := ARect.Right - ARect.Left - DW;
    SendMessage(AWinControl.Handle, LB_SETCOLUMNWIDTH, Max(1, (Width - DW) div ColCount), 0);
  end;
end;

class function TWin32WSCustomListBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  with Params do
  begin
    pClassName := ListBoxClsName;
    pSubClassName := LCLListboxClsName;
    SubClassWndProc := @ListBoxWindowProc;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, False, True);
  Result := Params.Window;
end;

class procedure TWin32WSCustomListBox.DragStart(const ACustomListBox: TCustomListBox);
var
  P: TPoint;
begin
  if csLButtonDown in ACustomListBox.ControlState then
  begin
    // if drag is called by mouse down then we need to complete it with mouse up
    // since in other case we will not get the change event called
    GetCursorPos(P);
    P := ACustomListBox.ScreenToClient(P);
    CallDefaultWindowProc(ACustomListBox.Handle, WM_LBUTTONUP, 0, MAKELPARAM(P.X, P.Y));
  end;
end;

class function TWin32WSCustomListBox.GetIndexAtXY(
  const ACustomListBox: TCustomListBox; X, Y: integer): integer;
begin
  Result := Windows.SendMessage(ACustomListBox.Handle, LB_ITEMFROMPOINT, 0, MakeLParam(X,Y));
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
end;

class function TWin32WSCustomListBox.GetItemRect(
  const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect
  ): boolean;
var
  Handle: HWND;
begin
  Handle := ACustomListBox.Handle;
  // The check for GetProp is required because of some division error which happens
  // if call LB_GETITEMRECT on window initialization
  Result := Assigned(GetProp(Handle, 'WinControl')) and (Windows.SendMessage(Handle, LB_GETITEMRECT, Index, LPARAM(@ARect)) <> LB_ERR);
end;

class function TWin32WSCustomListBox.GetScrollWidth(const ACustomListBox: TCustomListBox): Integer;
begin
  Result := Windows.SendMessage(ACustomListBox.Handle, LB_GETHORIZONTALEXTENT, 0, 0);
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
  WindowInfo: PWin32WindowInfo;
  winHandle: HWND;
begin
  winHandle := ACustomListBox.Handle;
  WindowInfo := GetWin32WindowInfo(winHandle);
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
  GetWin32WindowInfo(Handle)^.List := Result;
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
    begin
      Windows.SendMessage(Handle, LB_SETSEL, Windows.WParam(true), Windows.LParam(AIndex));
    end;
    Windows.SendMessage(Handle, LB_SETCARETINDEX, Windows.WParam(AIndex), 0);
  end else
    Windows.SendMessage(Handle, LB_SETCURSEL, Windows.WParam(AIndex), 0);
end;

class procedure TWin32WSCustomListBox.SetScrollWidth(
  const ACustomListBox: TCustomListBox; const AScrollWidth: Integer);
begin
  Windows.SendMessage(ACustomListBox.Handle, LB_SETHORIZONTALEXTENT, AScrollWidth, 0);
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

class function TWin32WSCustomComboBox.GetStringList(
  const ACustomComboBox: TCustomComboBox): TWin32ComboBoxStringList;
begin
  Result := nil;
  if ACustomComboBox.Style <> csSimple then
    Result := TWin32ComboBoxStringList(GetWin32WindowInfo(ACustomComboBox.Handle)^.List);
end;

class function TWin32WSCustomComboBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
  Info: TComboboxInfo;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := ComboboxClsName;
    pSubClassName := LCLComboboxClsName;
    SubClassWndProc := @ComboBoxWindowProc;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, False, True);

  Info.cbSize := SizeOf(Info);
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

class function TWin32WSCustomComboBox.GetDoubleBuffered(
  const AWinControl: TWinControl): Boolean;
begin
  Result := False; // force DoubleBuffered False, see #33831
end;

class procedure TWin32WSCustomComboBox.AdaptBounds(const AWinControl: TWinControl;
  var Left, Top, Width, Height: integer; var SuppressMove: boolean);
var
  StringList: TWin32ComboBoxStringList;
begin
  if TCustomComboBox(AWinControl).Style = csSimple then Exit;
  StringList := GetStringList(TCustomComboBox(AWinControl));
  if Assigned(StringList) then
    Height := StringList.ComboHeight;
end;

class procedure TWin32WSCustomComboBox.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  PreferredHeight := 0;
  if (AWinControl.HandleAllocated) and (TCustomComboBox(AWinControl).Style <> csSimple) then
    PreferredHeight := AWinControl.Height;
end;

class function TWin32WSCustomComboBox.GetDroppedDown(
  const ACustomComboBox: TCustomComboBox): Boolean;
begin
  if not WSCheckHandleAllocated(ACustomComboBox, 'TWin32WSCustomComboBox.GetDroppedDown') then
    Exit(False);
  Result := LongBool(SendMessage(ACustomComboBox.Handle, CB_GETDROPPEDSTATE, 0, 0));
end;

class function TWin32WSCustomComboBox.GetSelStart(const ACustomComboBox: TCustomComboBox): integer;
begin
  if not WSCheckHandleAllocated(ACustomComboBox, 'TWin32WSCustomComboBox.GetSelStart') then
    Exit(-1);
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
begin
  RecreateWnd(ACustomComboBox);
end;

class function TWin32WSCustomComboBox.GetItemIndex(const ACustomComboBox: TCustomComboBox): integer;
begin
  Result := SendMessage(ACustomComboBox.Handle, CB_GETCURSEL, 0, 0);
end;

class function TWin32WSCustomComboBox.GetMaxLength(const ACustomComboBox: TCustomComboBox): integer;
begin
  Result := GetWin32WindowInfo(ACustomComboBox.Handle)^.MaxLength;
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

class procedure TWin32WSCustomComboBox.SetDropDownCount(
  const ACustomComboBox: TCustomComboBox; NewCount: Integer);
var
  StringList: TWin32ComboBoxStringList;
begin
  if (WindowsVersion >= wvVista) and ThemeServices.ThemesEnabled then
  begin
    //Use CB_SETMINVISIBLE in vista or above
    SendMessage(ACustomComboBox.Handle, CB_SETMINVISIBLE, NewCount, 0);
  end
  else
  begin
    StringList := GetStringList(ACustomComboBox);
    if StringList <> nil then
      StringList.DropDownCount := NewCount;
  end;
end;

class procedure TWin32WSCustomComboBox.SetDroppedDown(
  const ACustomComboBox: TCustomComboBox; ADroppedDown: Boolean);
var
  aSelStart, aSelLength: Integer;
  aText: string;
  Editable: Boolean;
  OldItemIndex: Integer;
begin
  if WSCheckHandleAllocated(ACustomComboBox, 'TWin32WSCustomComboBox.SetDroppedDown') then
  begin
    Editable := (ACustomComboBox.Style in [csDropDown, csOwnerDrawEditableFixed, csOwnerDrawEditableVariable]);
    if Editable then
    begin
      if not GetText(ACustomComboBox, aText) then
        aText := ACustomComboBox.Text;
      aSelStart := GetSelStart(ACustomComboBox);
      aSelLength := GetSelLength(ACustomComboBox);
    end;

    OldItemIndex := GetItemIndex(ACustomComboBox);
    SendMessage(ACustomComboBox.Handle, CB_SHOWDROPDOWN, WPARAM(ADroppedDown), 0);
    SetItemIndex(ACustomComboBox, OldItemIndex);

    if Editable then
    begin
      SetText(ACustomComboBox, aText);
      SetSelStart(ACustomComboBox, aSelStart);
      SetSelLength(ACustomComboBox, aSelLength);
    end;
  end;
end;

class procedure TWin32WSCustomComboBox.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
begin
  TWin32WSWinControl.SetFont(AWinControl, AFont);
  GetControlConstraints(AWinControl.Constraints);
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
  GetWin32WindowInfo(winhandle)^.MaxLength := NewLength;
end;

class function TWin32WSCustomComboBox.GetItems(const ACustomComboBox: TCustomComboBox): TStrings;
var
  winhandle: HWND;
begin
  winhandle := ACustomComboBox.Handle;
  Result := TWin32ComboBoxStringList.Create(winhandle, ACustomComboBox);
  GetWin32WindowInfo(winhandle)^.List := Result;
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
  Windows.SendMessageW(WinHandle, EM_GETSEL, Windows.WPARAM(@Result), 0);
end;

function EditGetSelLength(WinHandle: HWND): integer;
var
  startpos, endpos: integer;
begin
  Windows.SendMessageW(WinHandle, EM_GETSEL, Windows.WPARAM(@startpos), Windows.LPARAM(@endpos));
  Result := endpos - startpos;
end;

procedure EditSetSelStart(WinHandle: HWND; NewStart: integer);
begin
  Windows.SendMessageW(WinHandle, EM_SETSEL, Windows.WParam(NewStart), Windows.LParam(NewStart));
  // scroll caret into view
  Windows.SendMessageW(WinHandle, EM_SCROLLCARET, 0, 0);
end;

procedure EditSetSelLength(WinHandle: HWND; NewLength: integer);
var
  startpos, endpos: integer;
begin
 Windows.SendMessageW(WinHandle, EM_GETSEL, Windows.WParam(@startpos), Windows.LParam(@endpos));
 endpos := startpos + NewLength;
 Windows.SendMessageW(WinHandle, EM_SETSEL, Windows.WParam(startpos), Windows.LParam(endpos));
end;

{ TWin32WSCustomEdit }

class function TWin32WSCustomEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @EditClsName[0];
    WindowTitle := StrCaption;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // edit is not a transparent control -> no need for parentpainting
  Params.WindowInfo^.needParentPaint := false;
  Result := Params.Window;

  ApplyMargins(AWinControl);
end;

class function TWin32WSCustomEdit.GetCanUndo(const ACustomEdit: TCustomEdit): Boolean;
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
  Result := GetWin32WindowInfo(ACustomEdit.Handle)^.MaxLength;
end;

class procedure TWin32WSCustomEdit.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  if MeasureText(AWinControl, 'Fj', PreferredWidth, PreferredHeight) then
  begin
    PreferredWidth := 0;
    if TCustomEdit(AWinControl).BorderStyle <> bsNone then
      Inc(PreferredHeight, 8);
  end;
end;

class function TWin32WSCustomEdit.GetText(const AWinControl: TWinControl; var AText: string): boolean;
begin
  Result := AWinControl.HandleAllocated;
  if not Result then
    exit;
  AText := GetControlText(AWinControl.Handle);
end;

class procedure TWin32WSCustomEdit.SetAlignment(const ACustomEdit: TCustomEdit;
  const AAlignment: TAlignment);
var
  CurrentStyle: DWord;
begin
  CurrentStyle := GetWindowLong(ACustomEdit.Handle, GWL_STYLE);
  if (CurrentStyle and 3) = AlignmentToEditFlags[AAlignment] then
    Exit;
  RecreateWnd(ACustomEdit);
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

class procedure TWin32WSCustomEdit.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetFont') then Exit;
  TWin32WSWinControl.SetFont(AWinControl, AFont);

  ApplyMargins(AWinControl);
end;

class procedure TWin32WSCustomEdit.SetHideSelection(const ACustomEdit: TCustomEdit; NewHideSelection: Boolean);
var
  CurrentStyle: DWord;
begin
  CurrentStyle := GetWindowLong(ACustomEdit.Handle, GWL_STYLE);
  if (CurrentStyle and ES_NOHIDESEL = 0) = NewHideSelection  then
    Exit;
  RecreateWnd(ACustomEdit);
end;

class procedure TWin32WSCustomEdit.SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer);
var
  winhandle: HWND;
begin
  winhandle := ACustomEdit.Handle;
  SendMessage(winhandle, EM_LIMITTEXT, NewLength, 0);
  GetWin32WindowInfo(winhandle)^.MaxLength := NewLength;
end;

class procedure TWin32WSCustomEdit.SetNumbersOnly(const ACustomEdit: TCustomEdit; NewNumbersOnly: Boolean);
const
  EditStyles: array[Boolean] of integer = (0, ES_NUMBER);
  EditStyleMask = ES_NUMBER;
begin
  UpdateWindowStyle(ACustomEdit.Handle, EditStyles[NewNumbersOnly], EditStyleMask);
end;

class procedure TWin32WSCustomEdit.SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char);
begin
  SendMessage(ACustomEdit.Handle, EM_SETPASSWORDCHAR, WParam(NewChar), 0);
  //it does not propagate immediately to the control otherwise...
  ACustomEdit.Invalidate;
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

class procedure TWin32WSCustomEdit.SetSelText(const ACustomEdit: TCustomEdit;
  const NewSelText: string);
begin
  SendMessageW(ACustomEdit.Handle, EM_REPLACESEL, WPARAM(1), LPARAM(PWideChar(UTF8ToUTF16(NewSelText))));
end;

class procedure TWin32WSCustomEdit.SetText(const AWinControl: TWinControl;
  const AText: string);
var
  ACustomEdit: TCustomEdit absolute AWinControl;
begin
  if (ACustomEdit.MaxLength > 0) and (UTF8Length(AText) > ACustomEdit.MaxLength) then
    TWin32WSWinControl.SetText(ACustomEdit, UTF8Copy(AText, 1, ACustomEdit.MaxLength))
  else
    TWin32WSWinControl.SetText(ACustomEdit, AText);
end;

class procedure TWin32WSCustomEdit.SetTextHint(const ACustomEdit: TCustomEdit;
  const ATextHint: string);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetTextHint') then Exit;
  SendMessage(ACustomEdit.Handle, EM_SETCUEBANNER, 1, {%H-}LParam(PWideChar(UTF8ToUTF16(ATextHint))));
end;

class procedure TWin32WSCustomEdit.Cut(const ACustomEdit: TCustomEdit);
begin
  SendMessage(ACustomEdit.Handle, WM_CUT, 0, 0)
end;

class procedure TWin32WSCustomEdit.ApplyMargins(const AWinControl: TWinControl);
begin
  if (WindowsVersion >= wv2000) and AWinControl.HandleAllocated then
    SendMessage(AWinControl.Handle, EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN, 0);
end;

class procedure TWin32WSCustomEdit.Copy(const ACustomEdit: TCustomEdit);
begin
  SendMessage(ACustomEdit.Handle, WM_COPY, 0, 0)
end;

class procedure TWin32WSCustomEdit.Paste(const ACustomEdit: TCustomEdit);
begin
  SendMessage(ACustomEdit.Handle, WM_PASTE, 0, 0)
end;

class procedure TWin32WSCustomEdit.Undo(const ACustomEdit: TCustomEdit);
begin
  SendMessage(ACustomEdit.Handle, EM_UNDO, 0, 0)
end;

{ TWin32WSCustomMemo }

function MemoWndProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  Control: TWinControl;
  LMessage: TLMessage;
begin
  case Msg of
    // prevent flickering, Mantis #16140
    WM_ERASEBKGND:
      begin
        Control := GetWin32WindowInfo(Window)^.WinControl;
        LMessage.msg := Msg;
        LMessage.wParam := WParam;
        LMessage.lParam := LParam;
        LMessage.Result := 0;
        Result := DeliverMessage(Control, LMessage);
      end;
    else
      Result := WindowProc(Window, Msg, WParam, LParam);
  end;
end;

class function TWin32WSCustomMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @EditClsName[0];
    SubClassWndProc := @MemoWndProc;
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

  { EM_GETSEL returns the char index of the caret, but this index
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

class procedure TWin32WSCustomMemo.SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint);
var
  CharIndex: LRESULT;
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

class procedure TWin32WSCustomMemo.SetSelText(const ACustomEdit: TCustomEdit;
  const NewSelText: string);
begin
  TWin32WSCustomEdit.SetSelText(ACustomEdit, NewSelText);
end;

class procedure TWin32WSCustomMemo.SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean);
begin
  // TODO: check if can be done without recreation
  RecreateWnd(ACustomMemo);
end;

class procedure TWin32WSCustomMemo.ScrollBy(const AWinControl: TWinControl;
  DeltaX, DeltaY: integer);
begin
  SendMessage(AWinControl.Handle, EM_LINESCROLL, -DeltaX, -DeltaY);
end;

{ TWin32WSCustomStaticText }

function StaticTextWndProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  WindowInfo: PWin32WindowInfo;
  StaticText: TCustomStaticText;
  DC: HDC;
  Flags: Cardinal;
  ARect: TRect;
  WideBuffer: WideString;
begin
  // move static text specific code here
  case Msg of
    WM_NCPAINT:
    begin
      WindowInfo := GetWin32WindowInfo(Window);
      if Assigned(WindowInfo) and
         TWin32ThemeServices(ThemeServices).ThemesEnabled and
         (GetWindowLong(Window, GWL_EXSTYLE) and WS_EX_CLIENTEDGE <> 0) then
      begin
        TWin32ThemeServices(ThemeServices).PaintBorder(WindowInfo^.WinControl, True);
        Result := 0;
        Exit;
      end;
    end;
    WM_PAINT:
      begin
        WindowInfo := GetWin32WindowInfo(Window);
        if ThemeServices.ThemesEnabled and Assigned(WindowInfo) and (WindowInfo^.WinControl is TCustomStaticText)
        and not TCustomStaticText(WindowInfo^.WinControl).Enabled then
        begin
          Result := WindowProc(Window, Msg, WParam, LParam);
          StaticText := TCustomStaticText(WindowInfo^.WinControl);
          DC := GetDC(Window);
          SetBkColor(DC, GetSysColor(COLOR_BTNFACE));
          SetTextColor(DC, GetSysColor(COLOR_GRAYTEXT));
          SelectObject(DC, StaticText.Font.Reference.Handle);
          Flags := 0;
          ARect := Classes.Rect(0, 0, 0, 0);
          WideBuffer := UTF8ToUTF16(TCustomGroupBox(WindowInfo^.WinControl).Caption);
          DrawTextW(DC, PWideChar(WideBuffer), Length(WideBuffer), ARect, Flags or DT_CALCRECT);
          if StaticText.BiDiMode = bdRightToLeft then
          begin
            Flags := Flags or DT_RIGHT;
            OffsetRect(ARect, StaticText.ClientWidth - ARect.Right, 0);
          end
          else
            OffsetRect(ARect, 0, 0);
          DrawTextW(DC, PWideChar(WideBuffer), Length(WideBuffer), ARect, Flags);
          ReleaseDC(Window, DC);
          Exit;
        end;
      end;
  end;
  Result := WindowProc(Window, Msg, WParam, LParam);
end;

function StaticTextParentMsgHandler(const AWinControl: TWinControl; Window: HWnd;
      Msg: UInt; WParam: Windows.WParam; LParam: Windows.LParam;
      var MsgResult: Windows.LResult; var WinProcess: Boolean): Boolean;
var
  Info: PWin32WindowInfo;
  TextColor: TColor;
begin
  Result := False;
  case Msg of
    WM_CTLCOLORSTATIC:
    begin
      Info := GetWin32WindowInfo(HWND(LParam));
      Result := Assigned(Info) and ThemeServices.ThemesEnabled and TCustomStaticText(Info^.WinControl).Transparent;
      if Result then
      begin
        ThemeServices.DrawParentBackground(HWND(LParam), HDC(WParam), nil, False);
        MsgResult := Windows.GetStockObject(HOLLOW_BRUSH);
        WinProcess := False;
        Windows.SetBkMode(HDC(WParam), TRANSPARENT);
        TextColor := Info^.WinControl.Font.Color;
        if TextColor = clDefault then
          TextColor := Info^.WinControl.GetDefaultColor(dctFont);
        Windows.SetTextColor(HDC(WParam), ColorToRGB(TextColor));
      end;
    end;
  end;
end;

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

class function TWin32WSCustomStaticText.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
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
    SubClassWndProc := @StaticTextWndProc;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
  Params.WindowInfo^.ParentMsgHandler := @StaticTextParentMsgHandler;
end;

class procedure TWin32WSCustomStaticText.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  if MeasureText(AWinControl, AWinControl.Caption, PreferredWidth, PreferredHeight) then
  begin
    Inc(PreferredHeight);
    if TCustomStaticText(AWinControl).BorderStyle <> sbsNone then
    begin
      if ThemeServices.ThemesEnabled and (TCustomStaticText(AWinControl).BorderStyle = sbsSingle) then
      begin
        inc(PreferredWidth, 4);
        inc(PreferredHeight, 4);
      end
      else
      begin
        inc(PreferredWidth, 2);
        inc(PreferredHeight, 2);
      end;
    end;
  end;
end;

class procedure TWin32WSCustomStaticText.SetBiDiMode(
  const AWinControl: TWinControl; UseRightToLeftAlign, UseRightToLeftReading,
  UseRightToLeftScrollBar: Boolean);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBiDiMode') then
    exit;
  RecreateWnd(AWinControl);//to adjust the update the Alignment
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
    if WithThemeSpace then
    begin
      Inc(PreferredWidth, 6);
      Inc(PreferredHeight, 6);
    end;
  end;
end;

{ TWin32WSButton }

function ButtonWndProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  Control: TWinControl;
  LMessage: TLMessage;
begin
  case Msg of
    WM_PAINT,
    WM_ERASEBKGND:
      begin
        Control := GetWin32WindowInfo(Window)^.WinControl;
        if not TWSWinControlClass(Control.WidgetSetClass).GetDoubleBuffered(Control) then
        begin
          LMessage.msg := Msg;
          LMessage.wParam := WParam;
          LMessage.lParam := LParam;
          LMessage.Result := 0;
          Result := DeliverMessage(Control, LMessage);
        end
        else
          Result := WindowProc(Window, Msg, WParam, LParam);
      end;
    WM_PRINTCLIENT:
      Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
    else
      Result := WindowProc(Window, Msg, WParam, LParam);
  end;
end;

class function TWin32WSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ButtonClsName[0];
    SubClassWndProc := @ButtonWndProc;
    WindowTitle := StrCaption;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

class function TWin32WSButton.GetDoubleBuffered(
  const AWinControl: TWinControl): Boolean;
begin
  Result := GetWin32NativeDoubleBuffered(AWinControl);
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

class procedure TWin32WSButton.SetShortCut(const AButton: TCustomButton;
  const ShortCutK1, ShortCutK2: TShortCut);
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
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ButtonClsName[0];
    SubClassWndProc := @ButtonWndProc;
    WindowTitle := StrCaption;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

class function TWin32WSCustomCheckBox.GetDoubleBuffered(
  const AWinControl: TWinControl): Boolean;
begin
  Result := GetWin32NativeDoubleBuffered(AWinControl);
end;

class procedure TWin32WSCustomCheckBox.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
var
  iconHeight: integer;
begin
  if MeasureText(AWinControl, AWinControl.Caption, PreferredWidth, PreferredHeight) then
  begin
    Inc(PreferredWidth, GetSystemMetrics(SM_CXMENUCHECK));
    // pixels spacing between checkbox and text
    if ThemeServices.ThemesEnabled then
      Inc(PreferredWidth, 4)
    else
      Inc(PreferredWidth, 6);
    iconHeight := GetSystemMetrics(SM_CYMENUCHECK);
    if iconHeight > PreferredHeight then
      PreferredHeight := iconHeight;
    if WithThemeSpace then
    begin
      Inc(PreferredWidth, 1);
      Inc(PreferredHeight, 4);
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
  const ShortCutK1, ShortCutK2: TShortCut);
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
  //Pass SKIP_LMCHANGE through lParam to avoid the OnChange event be fired
  Windows.SendMessage(ACustomCheckBox.Handle, BM_SETCHECK, Flags, SKIP_LMCHANGE);
end;

class procedure TWin32WSCustomCheckBox.SetAlignment(
  const ACustomCheckBox: TCustomCheckBox; const NewAlignment: TLeftRight);
begin
  RecreateWnd(ACustomCheckBox);
end;

{ TWin32WSToggleBox }

class function TWin32WSToggleBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ButtonClsName[0];
    WindowTitle := StrCaption;
    Flags:= Flags or BS_MULTILINE;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;


{ TWin32WSRadioButton }

class function TWin32WSRadioButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
const
  BM_SETDONTCLICK = $00F8;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ButtonClsName[0];
    SubClassWndProc := @ButtonWndProc;
    WindowTitle := StrCaption;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
  // don't generate a BM_CLICK on focus
  SendMessage(Result, BM_SETDONTCLICK, 1, 0);
end;

end.
