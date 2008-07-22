{ $Id: carbonwsstdctrls.pp 15309 2008-06-04 22:12:59Z vincents $}
{
 *****************************************************************************
 *                              CocoaWSStdCtrls.pp                           *
 *                              ---------------                              *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CocoaWSStdCtrls;

{$mode objfpc}{$H+}

interface

uses
  // Libs
{$ifdef ver2_2_0}
  FPCMacOSAll,
{$else}
  MacOSAll,
{$endif}
  // Cocoa
  foundation, appkit,
  // LCL
  Controls, StdCtrls, Graphics, LCLType, LMessages, LCLProc, Classes,
  // Widgetset
  WSStdCtrls, WSLCLClasses, WSControls, WSProc,
  // LCL Cocoa
  CocoaPrivate;

type

  { TCocoaWSScrollBar }

  TCocoaWSScrollBar = class(TWSScrollBar)
  private
  protected
  public
//    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
//    class procedure SetParams(const AScrollBar: TCustomScrollBar); override;
  end;

  { TCocoaWSCustomGroupBox }

  TCocoaWSCustomGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
//    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCocoaWSGroupBox }

  TCocoaWSGroupBox = class(TWSGroupBox)
  private
  protected
  public
  end;

  { TCocoaWSCustomComboBox }

  TCocoaWSCustomComboBox = class(TWSCustomComboBox)
  private
  protected
  public
{    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;

    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); override;
    class procedure SetReadOnly(const ACustomComboBox: TCustomComboBox; NewReadOnly: boolean); override;

    class function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
    class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); override;}
  end;

  { TCocoaWSComboBox }

  TCocoaWSComboBox = class(TWSComboBox)
  private
  protected
  public
  end;

  { TCocoaWSCustomListBox }

  TCocoaWSCustomListBox = class(TWSCustomListBox)
  private
  protected
  public
{    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetIndexAtY(const ACustomListBox: TCustomListBox; y: integer): integer; override;
    class function GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class function GetItemRect(const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean; override;
    class function GetSelCount(const ACustomListBox: TCustomListBox): integer; override;
    class function GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;
    class function GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class function GetTopIndex(const ACustomListBox: TCustomListBox): integer; override;

    class procedure SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean); override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
    //class procedure SetBorder(const ACustomListBox: TCustomListBox); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect, AMultiSelect: boolean); override;
    class procedure SetStyle(const ACustomListBox: TCustomListBox); override;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;
    class procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); override;}
  end;

  { TCocoaWSListBox }

  TCocoaWSListBox = class(TWSListBox)
  private
  protected
  public
  end;

  { TCocoaWSCustomEdit }

  TCocoaWSCustomEdit = class(TWSCustomEdit)
  private
  protected
  public
{    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;

    class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;}
  end;
  
  { TCocoaWSCustomMemo }

  TCocoaWSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
{    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;

    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); override;
    class procedure SetAlignment(const ACustomMemo: TCustomMemo; const AAlignment: TAlignment); override;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); override;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); override;}
  end;

  { TCocoaWSEdit }

  TCocoaWSEdit = class(TWSEdit)
  private
  protected
  public
  end;

  { TCocoaWSMemo }

  TCocoaWSMemo = class(TWSMemo)
  private
  protected
  public
  end;

  { TCocoaWSCustomLabel }

  {TCocoaWSCustomLabel = class(TWSCustomLabel)
  private
  protected
  public
  end;}

  { TCocoaWSLabel }

  {TCocoaWSLabel = class(TWSLabel)
  private
  protected
  public
  end;}

  { TCocoaWSButtonControl }

  TCocoaWSButtonControl = class(TWSButtonControl)
  private
  protected
  public
  end;

  { TCocoaWSButton }

  TCocoaWSButton = class(TWSButton)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
  end;

  { TCocoaWSCustomCheckBox }

  TCocoaWSCustomCheckBox = class(TWSCustomCheckBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;
  end;

  { TCocoaWSCheckBox }

  TCocoaWSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TCocoaWSToggleBox }

  TCocoaWSToggleBox = class(TWSToggleBox)
  private
  protected
  public
//    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCocoaWSRadioButton }

  TCocoaWSRadioButton = class(TWSRadioButton)
  private
  protected
  public
//    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCocoaWSCustomStaticText }

  TCocoaWSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  public
//    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
//    class procedure SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment); override;
  end;

  { TCocoaWSStaticText }

  TCocoaWSStaticText = class(TWSStaticText)
  private
  protected
  public
  end;
  

implementation

{ TCocoaWSButton }

{------------------------------------------------------------------------------
  Method:  TCocoaWSButton.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Cocoa interface

  Creates new button control in Cocoa interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCocoaWSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  CocoaButton: TCocoaButton;
begin
  CocoaButton := TCocoaButton.Create(AWinControl, AParams);

  Result := TLCLIntfHandle(CocoaButton);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSButton.SetDefault
  Params:  AButton  - LCL button control
           ADefault

  Sets button default indication in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSButton.SetDefault(const AButton: TCustomButton;
  ADefault: Boolean);
begin
//  if not CheckHandle(AButton, Self, 'SetDefault') then Exit;

//  TCocoaCustomButton(AButton.Handle).SetDefault(ADefault);
end;

{ TCocoaWSCustomCheckBox }

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomCheckBox.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Cocoa interface

  Creates new check box in Cocoa interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCocoaWSCustomCheckBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  CocoaButton: TCocoaButton;
begin
  CocoaButton := TCocoaButton.Create(AWinControl, AParams);
  CocoaButton.Button.setButtonType(NSSwitchButton);

  Result := TLCLIntfHandle(CocoaButton);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomCheckBox.RetrieveState
  Params:  ACustomCheckBox - LCL custom check box
  Returns: State of check box

  Retrieves the state of check box in Cocoa interface
 ------------------------------------------------------------------------------}
class function TCocoaWSCustomCheckBox.RetrieveState(
  const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
begin
//  Result := cbUnchecked;
  
//  if not CheckHandle(ACustomCheckBox, Self, 'RetrieveState') then Exit;

//  Result := TCocoaCustomCheckBox(ACustomCheckBox.Handle).RetrieveState;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomCheckBox.SetState
  Params:  ACustomCheckBox - LCL custom check box
           NewState        - New state of check box

  Sets the new state of check box in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSCustomCheckBox.SetState(
  const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
begin
//  if not CheckHandle(ACustomCheckBox, Self, 'SetState') then Exit;

//  TCocoaCustomCheckBox(ACustomCheckBox.Handle).SetState(NewState);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollBar, TCocoaWSScrollBar);
//  RegisterWSComponent(TCustomGroupBox, TCocoaWSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TCocoaWSGroupBox);
//  RegisterWSComponent(TCustomComboBox, TCocoaWSCustomComboBox);
//  RegisterWSComponent(TComboBox, TCocoaWSComboBox);
//  RegisterWSComponent(TCustomListBox, TCocoaWSCustomListBox);
//  RegisterWSComponent(TListBox, TCocoaWSListBox);
//  RegisterWSComponent(TCustomEdit, TCocoaWSCustomEdit);
//  RegisterWSComponent(TCustomMemo, TCocoaWSCustomMemo);
//  RegisterWSComponent(TEdit, TCocoaWSEdit);
//  RegisterWSComponent(TMemo, TCocoaWSMemo);
//  RegisterWSComponent(TCustomLabel, TCocoaWSCustomLabel);
//  RegisterWSComponent(TLabel, TCocoaWSLabel);
//  RegisterWSComponent(TButtonControl, TCocoaWSButtonControl);
  RegisterWSComponent(TCustomButton, TCocoaWSButton);
  RegisterWSComponent(TCustomCheckBox, TCocoaWSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TCocoaWSCheckBox);
//  RegisterWSComponent(TToggleBox, TCocoaWSToggleBox);
//  RegisterWSComponent(TRadioButton, TCocoaWSRadioButton);
//  RegisterWSComponent(TCustomStaticText, TCocoaWSCustomStaticText);
//  RegisterWSComponent(TStaticText, TCocoaWSStaticText);
////////////////////////////////////////////////////
end.

