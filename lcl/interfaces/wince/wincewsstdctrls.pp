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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  // LCL
  SysUtils, LCLType, Classes, StdCtrls, Controls, Graphics, Forms, WinCEProc ,
  InterfaceBase,
  // Widgetset
  WSStdCtrls, WSLCLClasses, WinCEInt, WinCEWSControls;

type

  { TWinCEWSScrollBar }

  TWinCEWSScrollBar = class(TWSScrollBar)
  private
  protected
  public
{    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure SetParams(const AScrollBar: TCustomScrollBar); override;}
  end;

  { TWinCEWSCustomGroupBox }

  TWinCEWSCustomGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
{    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); override;}
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
{    class function  CreateHandle(const AWinControl: TWinControl;
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
    class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); override;}
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
{    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function  GetSelCount(const ACustomListBox: TCustomListBox): integer; override;
    class function  GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;
    class function  GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class function  GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class function  GetTopIndex(const ACustomListBox: TCustomListBox): integer; override;
    class procedure SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean); override;
    class procedure SetBorder(const ACustomListBox: TCustomListBox); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect,
      AMultiSelect: boolean); override;
    class procedure SetStyle(const ACustomListBox: TCustomListBox); override;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;
    class procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); override;}
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
{    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetMaxLength(const ACustomEdit: TCustomEdit): integer; override;}
    class function  GetText(const AWinControl: TWinControl; var AText: string): boolean; override;

{    class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;}
  end;

  { TWinCEWSCustomMemo }

  TWinCEWSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
{    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function  GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;
    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); override;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); override;}
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
{    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer); override;}
  end;

  { TWinCEWSCustomCheckBox }

  TWinCEWSCustomCheckBox = class(TWSCustomCheckBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
{    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer); override;
    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox;
          const OldShortCut, NewShortCut: TShortCut); override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;}
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
{    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;}
  end;

  { TWinCEWSRadioButton }

  TWinCEWSRadioButton = class(TWSRadioButton)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
  end;

implementation

{ TWinCEWSCustomEdit }

function TWinCEWSCustomEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  hwnd: THandle;
  Str: array[0..255] of WideChar;
begin
  {$ifdef VerboseWinCE}
  WriteLn('TWinCEWSCustomEdit.CreateHandle');
  {$endif}

  MultiByteToWideChar(CP_ACP, 0, PChar(AWinControl.Caption), -1, @Str, 256);

  Result := CreateWindow(
    @EditClsName,               // Name of the registered class
    @Str,                       // Title of the window
    WS_CHILD or WS_VISIBLE,     // Style of the window
    AWinControl.Left,           // x-position (at beginning)
    AWinControl.Top,            // y-position (at beginning)
    AWinControl.Width,          // window width
    AWinControl.Height,         // window height
    AWinControl.Parent.Handle,  // handle to parent or owner window
    0,                          // handle to menu
    System.hInstance,           // handle to application instance
    nil);                       // pointer to window-creation data

  if (hwnd = 0) then WriteLn('CreateWindow failed');

  Result := hwnd;
end;

function  TWinCEWSCustomEdit.GetText(const AWinControl: TWinControl; var AText: string): boolean;
var
  TextLen: dword;
  Str: array of WideChar;
  Buffer: array[0..255] of Char;
begin
  Result := AWinControl.HandleAllocated;
  
  if not Result then Exit;
  
  TextLen := GetWindowTextLength(AWinControl.Handle);
  SetLength(Str, TextLen);
  GetWindowText(AWinControl.Handle, @Str, TextLen + 1);
  
  WideCharToMultiByte(CP_ACP, 0, @Str, TextLen, @Buffer, 256, nil, nil);
  
  AText := string(PChar(Buffer));
end;

{ TWinCEWSCustomCheckBox }

function TWinCEWSCustomCheckBox.CreateHandle(const AWinControl: TWinControl;
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
    pClassName := 'BUTTON';
    WindowTitle := CreatePWideCharFromString(AWinControl.Caption);
    if TCustomCheckBox(AWinControl).AllowGrayed then
      Flags := Flags Or BS_AUTO3STATE
    else
      Flags := Flags Or BS_AUTOCHECKBOX;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;

{
  MultiByteToWideChar(CP_ACP, 0, PChar(AWinControl.Caption), -1, @Str, 256);

  Result := CreateWindow(
    @ButtonClsName,             // Name of the registered class
    @Str,                       // Title of the window
    WS_CHILD or WS_VISIBLE or WS_TABSTOP or BS_AUTOCHECKBOX or BS_LEFT,  // Style of the window
    AWinControl.Left,           // x-position (at beginning)
    AWinControl.Top,            // y-position (at beginning)
    AWinControl.Width,          // window width
    AWinControl.Height,         // window height
    AWinControl.Parent.Handle,  // handle to parent or owner window
    0,                          // handle to menu
    System.hInstance,           // handle to application instance
    nil);                       // pointer to window-creation data

  if (hwnd = 0) then WriteLn('CreateWindow failed');

  Result := hwnd;}
end;

{ TWinCEWSRadioButton }

function TWinCEWSRadioButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
  hwnd: THandle;
  Str: array[0..255] of WideChar;

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
    WindowTitle := CreatePWideCharFromString(AWinControl.Caption);
    // BS_AUTORADIOBUTTON may hang the application,
    // if the radiobuttons are not consecutive controls.//roozbeh:is it so in wince?
    Flags := Flags or BS_AUTORADIOBUTTON;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

{ TWinCEWSCustomStaticText }
const
  AlignmentToStaticTextFlags: array[TAlignment] of dword = (SS_LEFT, SS_RIGHT, SS_CENTER);

function CalcStaticTextFlags(const Alignment: TAlignment): dword;
begin
  Result := AlignmentToStaticTextFlags[Alignment];
end;

function TWinCEWSCustomStaticText.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  hwnd: THandle;
  Str: array[0..255] of WideChar;
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
    WindowTitle := CreatePWideCharFromString(AWinControl.Caption);//roozbeh..we already have this in strcaptiob..whats the diffrence?
    Flags := Flags or CalcStaticTextFlags(TCustomStaticText(AWinControl).Alignment);//is ws_child included?
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;

end;

procedure TWinCEWSCustomStaticText.SetAlignment(
  const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment);
begin
  inherited SetAlignment(ACustomStaticText, NewAlignment);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollBar, TWinCEWSScrollBar);
//  RegisterWSComponent(TCustomGroupBox, TWinCEWSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TWinCEWSGroupBox);
//  RegisterWSComponent(TCustomComboBox, TWinCEWSCustomComboBox);
//  RegisterWSComponent(TComboBox, TWinCEWSComboBox);
//  RegisterWSComponent(TCustomListBox, TWinCEWSCustomListBox);
//  RegisterWSComponent(TListBox, TWinCEWSListBox);
  RegisterWSComponent(TCustomEdit, TWinCEWSCustomEdit);
//  RegisterWSComponent(TCustomMemo, TWinCEWSCustomMemo);
//  RegisterWSComponent(TEdit, TWinCEWSEdit);
//  RegisterWSComponent(TMemo, TWinCEWSMemo);
//  RegisterWSComponent(TButtonControl, TWinCEWSButtonControl);
  RegisterWSComponent(TCustomCheckBox, TWinCEWSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TWinCEWSCheckBox);
//  RegisterWSComponent(TToggleBox, TWinCEWSToggleBox);
  RegisterWSComponent(TRadioButton, TWinCEWSRadioButton);
//  RegisterWSComponent(TCustomStaticText, TWinCEWSCustomStaticText);
  RegisterWSComponent(TStaticText, TWinCEWSStaticText);
////////////////////////////////////////////////////
end.
