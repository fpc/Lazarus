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
 *  See the file COPYING.LCL, included in this distribution,                 *
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

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Classes, StdCtrls, Controls, Graphics,
////////////////////////////////////////////////////
  WSStdCtrls, WSLCLClasses, Windows, Win32Int, Win32Proc, InterfaceBase, LCLType;

type

  { TWin32WSScrollBar }

  TWin32WSScrollBar = class(TWSScrollBar)
  private
  protected
  public
  end;

  { TWin32WSCustomGroupBox }

  TWin32WSCustomGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
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
    class function  GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;

    class procedure SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox; 
      NewTraverseList: boolean); override;
    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); override;
    
    class function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
    class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); override;
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
    class function  GetSelCount(const ACustomListBox: TCustomListBox): integer; override;
    class function  GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;
    class function  GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class function  GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class procedure SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean); override;
    class procedure SetBorder(const ACustomListBox: TCustomListBox); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect,
      AMultiSelect: boolean); override;
    class procedure SetStyle(const ACustomListBox: TCustomListBox); override;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;
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
    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetMaxLength(const ACustomEdit: TCustomEdit): integer; {override;}

    class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
  end;

  { TWin32WSCustomMemo }

  TWin32WSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
    class procedure AppendText(const ACustomMemo: TCustomMemo; AText: string); override;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); override;
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

  { TWin32WSCustomLabel }

  TWin32WSCustomLabel = class(TWSCustomLabel)
  private
  protected
  public
    class procedure SetAlignment(const ACustomLabel: TCustomLabel; const NewAlignment: TAlignment); override;
    class procedure SetLayout(const ACustomLabel: TCustomLabel; const NewLayout: TTextLayout); override;
    class procedure SetWordWrap(const ACustomLabel: TCustomLabel; const NewWordWrap: boolean); override;
  end;

  { TWin32WSLabel }

  TWin32WSLabel = class(TWSLabel)
  private
  protected
  public
  end;

  { TWin32WSButtonControl }

  TWin32WSButtonControl = class(TWSButtonControl)
  private
  protected
  public
  end;

  { TWin32WSCustomCheckBox }

  TWin32WSCustomCheckBox = class(TWSCustomCheckBox)
  private
  protected
  public
    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox;
          const OldShortCut, NewShortCut: TShortCut); override;
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
  end;

  { TWin32WSRadioButton }

  TWin32WSRadioButton = class(TWSRadioButton)
  private
  protected
  public
  end;

  { TWin32WSCustomStaticText }

  TWin32WSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  public
  end;

  { TWin32WSStaticText }

  TWin32WSStaticText = class(TWSStaticText)
  private
  protected
  public
  end;

{ useful helper functions }

function  EditGetSelStart(WinHandle: HWND): integer;
function  EditGetSelLength(WinHandle: HWND): integer;
procedure EditSetSelStart(WinHandle: HWND; NewStart: integer);
procedure EditSetSelLength(WinHandle: HWND; NewLength: integer);

implementation

{ TWin32WSCustomListBox }

function  TWin32WSCustomListBox.GetItemIndex(const ACustomListBox: TCustomListBox): integer;
var
  Handle: HWND;
begin
  Handle := ACustomListBox.Handle;
  case ACustomListBox.FCompStyle of
    csListBox, csCListBox:
    begin
      Result := SendMessage(Handle, LB_GETCURSEL, 0, 0);
      if Result = LB_ERR then
      begin
        Assert(False, 'Trace:[TWin32WidgetSet.IntSendMessage3] Could not retrieve item index via LM_GETITEMINDEX; try selecting an item first');
        Result := -1;
      end;
    end;
    csNotebook:
    begin
      Result := SendMessage(Handle, TCM_GETCURSEL, 0, 0);
    end;
  end;
end;

function  TWin32WSCustomListBox.GetSelCount(const ACustomListBox: TCustomListBox): integer;
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

function  TWin32WSCustomListBox.GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean;
begin
  Result := Windows.SendMessage(ACustomListBox.Handle, LB_GETSEL, Windows.WParam(AIndex), 0) > 0;
end;

function  TWin32WSCustomListBox.GetStrings(const ACustomListBox: TCustomListBox): TStrings;
var
  Handle: HWND;
begin
  Handle := ACustomListBox.Handle;
  if ACustomListBox.fCompStyle = csCListBox then 
    Result := TWin32CListStringList.Create(Handle, ACustomListBox)
  else
  if ACustomListBox.fCompStyle = csCheckListBox then 
    Result := TWin32CheckListBoxStrings.Create(Handle, ACustomListBox)
  else
    Result := TWin32ListStringList.Create(Handle, ACustomListBox);
  Windows.SetProp(Handle, 'List', dword(Result));
end;

procedure TWin32WSCustomListBox.SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean);
begin
  if ACustomListBox.FCompStyle = csListBox then
    Windows.SendMessage(ACustomListBox.Handle, LB_SELITEMRANGE, Windows.WParam(ASelected), Windows.LParam(MakeLParam(AIndex, AIndex)))
end;

procedure TWin32WSCustomListBox.SetBorder(const ACustomListBox: TCustomListBox);
var
  Handle: HWND;
begin
  if ACustomListBox.FCompStyle in [csListBox, csCListBox] then
  begin
    Handle := ACustomListBox.Handle;
    if ACustomListBox.BorderStyle = TBorderStyle(bsSingle) Then
      SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_CLIENTEDGE)
    else
      SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) and not WS_EX_CLIENTEDGE);
  end;
end;

procedure TWin32WSCustomListBox.SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer);
var
  Handle: HWND;
begin
  Handle := ACustomListBox.Handle;
  case ACustomListBox.FCompStyle of
    csListBox, csCListBox:
    begin
      if ACustomListBox.MultiSelect then
        Windows.SendMessage(Handle, LB_SETSEL, Windows.WPARAM(true), Windows.LParam(AIndex))
      else
        Windows.SendMessage(Handle, LB_SETCURSEL, Windows.WParam(AIndex), 0);
    end;
  end;
end;

procedure TWin32WSCustomListBox.SetSelectionMode(const ACustomListBox: TCustomListBox;
  const AExtendedSelect, AMultiSelect: boolean);
begin
  TWin32WidgetSet(InterfaceObject).RecreateWnd(ACustomListBox);
end;

procedure TWin32WSCustomListBox.SetStyle(const ACustomListBox: TCustomListBox);
begin
  // The listbox styles can't be updated, so recreate the listbox
  TWin32WidgetSet(InterfaceObject).RecreateWnd(ACustomListBox);
end;

procedure TWin32WSCustomListBox.SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean);
begin
  case ACustomListBox.FCompStyle of
    csListBox:
      TWin32ListStringList(AList).Sorted := ASorted;
    csCListBox:
      TWin32CListStringList(AList).Sorted := ASorted;
  end;
end;

{ TWin32WSCustomComboBox }

function  TWin32WSCustomComboBox.GetSelStart(const ACustomComboBox: TCustomComboBox): integer;
begin
  Result := Low(SendMessage(ACustomComboBox.Handle, CB_GETEDITSEL, Windows.WPARAM(nil), Windows.LPARAM(nil)));
end;

function  TWin32WSCustomComboBox.GetSelLength(const ACustomComboBox: TCustomComboBox): integer;
begin
  Result := SendMessage(ACustomComboBox.Handle, CB_GETEDITSEL, Windows.WPARAM(nil), Windows.LPARAM(nil));
end;

procedure TWin32WSCustomComboBox.SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle);
begin
  // TODO: implement me
end;

function  TWin32WSCustomComboBox.GetItemIndex(const ACustomComboBox: TCustomComboBox): integer;
begin
  Result := SendMessage(ACustomComboBox.Handle, CB_GETCURSEL, 0, 0);
  if Result = LB_ERR Then
  Begin
    Assert(False, 'Trace:[TWin32WidgetSet.IntSendMessage3] Could not retrieve item index '+
        'via LM_GETITEMINDEX; try selecting an item first');
    Result := -1;
  End;
end;

function  TWin32WSCustomComboBox.GetMaxLength(const ACustomComboBox: TCustomComboBox): integer;
begin
  Result := integer(GetProp(ACustomComboBox.Handle, 'MAXLENGTH'));
end;

procedure TWin32WSCustomComboBox.SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox; 
  NewTraverseList: boolean);
begin
  // TODO: implement me?
end;

procedure TWin32WSCustomComboBox.SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer);
begin
  SendMessage(ACustomComboBox.Handle, CB_SETEDITSEL, 0, MakeLParam(NewStart, NewStart));
end;

procedure TWin32WSCustomComboBox.SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer);
var
  startpos, endpos: integer;
  winhandle: HWND;
begin
  winhandle := ACustomComboBox.Handle;
  SendMessage(winhandle, CB_GETEDITSEL, Windows.WParam(@startpos), Windows.LParam(@endpos));
  endpos := startpos + NewLength;
  SendMessage(winhandle, CB_SETEDITSEL, 0, MakeLParam(startpos, endpos));
end;

procedure TWin32WSCustomComboBox.SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer);
begin
  SendMessage(ACustomComboBox.Handle, CB_SETCURSEL, Windows.WParam(NewIndex), 0);
end;

procedure TWin32WSCustomComboBox.SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer);
var
  winhandle: HWND;
begin
  winhandle := ACustomComboBox.Handle;
  SendMessage(winhandle, CB_LIMITTEXT, NewLength, 0);
  SetProp(winhandle, 'MAXLENGTH', NewLength);
end;

function  TWin32WSCustomComboBox.GetItems(const ACustomComboBox: TCustomComboBox): TStrings;
var
  winhandle: HWND;
begin
  winhandle := ACustomComboBox.Handle;
  Result := TWin32ListStringList.Create(winhandle, ACustomComboBox);
  SetProp(winhandle, 'List', dword(Result));
end;

procedure TWin32WSCustomComboBox.Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean);
begin
  TWin32ListStringList(AList).Sorted := IsSorted;
end;

{ TWin32WSCustomEdit helper functions }

function EditGetSelStart(WinHandle: HWND): integer;
begin
  SendMessage(WinHandle, EM_GETSEL, Windows.WPARAM(@Result), 0);
end;

function EditGetSelLength(WinHandle: HWND): integer;
var
  startpos, endpos: integer;
begin
  SendMessage(WinHandle, EM_GETSEL, Windows.WPARAM(@startpos), Windows.LPARAM(@endpos));
  Result := endpos - startpos;
end;

procedure EditSetSelStart(WinHandle: HWND; NewStart: integer);
begin
  SendMessage(WinHandle, EM_SETSEL, Windows.WParam(NewStart), Windows.LParam(NewStart));
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

function  TWin32WSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  Result := EditGetSelStart(ACustomEdit.Handle);
end;

function  TWin32WSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  Result := EditGetSelLength(ACustomEdit.Handle);
end;

function  TWin32WSCustomEdit.GetMaxLength(const ACustomEdit: TCustomEdit): integer;
begin
  Result := integer(GetProp(ACustomEdit.Handle, 'MAXLENGTH'));
end;

procedure TWin32WSCustomEdit.SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase);
const
  EditStyles: array[TEditCharCase] of integer = (0, ES_UPPERCASE, ES_LOWERCASE);
  EditStyleMask = ES_UPPERCASE or ES_LOWERCASE;
begin
  UpdateWindowStyle(ACustomEdit.Handle, EditStyles[NewCase], EditStyleMask);
end;

procedure TWin32WSCustomEdit.SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode);
begin
end;

procedure TWin32WSCustomEdit.SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer);
var
  winhandle: HWND;
begin
  winhandle := ACustomEdit.Handle;
  SendMessage(winhandle, EM_LIMITTEXT, NewLength, 0);
  SetProp(winhandle, 'MAXLENGTH', NewLength);
end;

procedure TWin32WSCustomEdit.SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char);
begin
  SendMessage(ACustomEdit.Handle, EM_SETPASSWORDCHAR, WParam(NewChar), 0);
end;

procedure TWin32WSCustomEdit.SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
begin
  Windows.SendMessage(ACustomEdit.Handle, EM_SETREADONLY, Windows.WPARAM(NewReadOnly), 0);
end;

procedure TWin32WSCustomEdit.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);
begin
  EditSetSelStart(ACustomEdit.Handle, NewStart);
end;

procedure TWin32WSCustomEdit.SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer);
begin
  EditSetSelLength(ACustomEdit.Handle, NewLength);
end;

{ TWin32WSCustomMemo }

procedure TWin32WSCustomMemo.AppendText(const ACustomMemo: TCustomMemo; AText: string);
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

procedure TWin32WSCustomMemo.SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle);
begin
  // TODO: check if can be done without recreation
  TWin32WidgetSet(InterfaceObject).RecreateWnd(ACustomMemo);
end;

procedure TWin32WSCustomMemo.SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean);
begin
  // TODO: check if can be done without recreation
  TWin32WidgetSet(InterfaceObject).RecreateWnd(ACustomMemo);
end;

{ TWin32WSCustomLabel }

procedure TWin32WSCustomLabel.SetAlignment(const ACustomLabel: TCustomLabel; const NewAlignment: TAlignment);
var
  Style: dword;
begin
  if ACustomLabel.WordWrap then
  begin
    case NewAlignment of
      taLeftJustify:
        Style := SS_LEFT;
      taCenter:
        Style := SS_CENTER;
      taRightJustify:
        Style := SS_RIGHT;
    else
      Style := SS_LEFT; // default, shouldn't happen
    end;
  end else begin
    Style := SS_LEFTNOWORDWRAP;
  end;
  UpdateWindowStyle(ACustomLabel.Handle, Style, SS_LEFT or SS_CENTER or SS_RIGHT or SS_LEFTNOWORDWRAP);
end;

procedure TWin32WSCustomLabel.SetLayout(const ACustomLabel: TCustomLabel; const NewLayout: TTextLayout);
var
  Style: dword;
begin
  case NewLayout of
    tlTop:
      Style := BS_TOP;
    tlCenter:
      Style := BS_VCENTER;
  else
    {tlBottom:}
    Style := BS_BOTTOM;
  end;
  UpdateWindowStyle(ACustomLabel.Handle, Style, BS_TOP or BS_VCENTER or BS_BOTTOM);
end;

procedure TWin32WSCustomLabel.SetWordWrap(const ACustomLabel: TCustomLabel; const NewWordWrap: boolean);
begin
  SetAlignment(ACustomLabel, ACustomLabel.Alignment);
end;

{ TWin32WSCustomCheckBox }

function  TWin32WSCustomCheckBox.RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
begin
  case SendMessage(ACustomCheckBox.Handle, BM_GETCHECK, 0, 0) of
    BST_CHECKED:       Result := cbChecked;
    BST_INDETERMINATE: Result := cbGrayed;
  else
    {BST_UNCHECKED:}   Result := cbUnChecked;
  end;
end;

procedure TWin32WSCustomCheckBox.SetShortCut(const ACustomCheckBox: TCustomCheckBox;
  const OldShortCut, NewShortCut: TShortCut);
begin
  // TODO: implement me!
end;

procedure TWin32WSCustomCheckBox.SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
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

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollBar, TWin32WSScrollBar);
//  RegisterWSComponent(TCustomGroupBox, TWin32WSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TWin32WSGroupBox);
  RegisterWSComponent(TCustomComboBox, TWin32WSCustomComboBox);
//  RegisterWSComponent(TComboBox, TWin32WSComboBox);
  RegisterWSComponent(TCustomListBox, TWin32WSCustomListBox);
//  RegisterWSComponent(TListBox, TWin32WSListBox);
  RegisterWSComponent(TCustomEdit, TWin32WSCustomEdit);
  RegisterWSComponent(TCustomMemo, TWin32WSCustomMemo);
//  RegisterWSComponent(TEdit, TWin32WSEdit);
//  RegisterWSComponent(TMemo, TWin32WSMemo);
  RegisterWSComponent(TCustomLabel, TWin32WSCustomLabel);
//  RegisterWSComponent(TLabel, TWin32WSLabel);
//  RegisterWSComponent(TButtonControl, TWin32WSButtonControl);
  RegisterWSComponent(TCustomCheckBox, TWin32WSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TWin32WSCheckBox);
//  RegisterWSComponent(TCheckBox, TWin32WSCheckBox);
//  RegisterWSComponent(TToggleBox, TWin32WSToggleBox);
//  RegisterWSComponent(TRadioButton, TWin32WSRadioButton);
//  RegisterWSComponent(TCustomStaticText, TWin32WSCustomStaticText);
//  RegisterWSComponent(TStaticText, TWin32WSStaticText);
////////////////////////////////////////////////////
end.
