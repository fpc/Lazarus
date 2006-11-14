{ $Id$}
{
 *****************************************************************************
 *                              QtWSStdCtrls.pp                              * 
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
unit QtWSStdCtrls;

{$mode delphi}{$H+}

interface

uses
  // Bindings
  qt4, qtprivate, qtwidgets,
  // LCL
  Classes, StdCtrls, Controls, Graphics, Forms, SysUtils, InterfaceBase,
  // Widgetset
  WSStdCtrls, WSLCLClasses, LCLType;

type

  { TQtWSScrollBar }

  TQtWSScrollBar = class(TWSScrollBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure SetParams(const AScrollBar: TCustomScrollBar); override;
  end;

  { TQtWSCustomGroupBox }

  TQtWSCustomGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

  { TQtWSGroupBox }

  TQtWSGroupBox = class(TWSGroupBox)
  private
  protected
  public
  end;

  { TQtWSCustomComboBox }

  TQtWSCustomComboBox = class(TWSCustomComboBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  public
{    class function  GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;}
    class function  GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
{    class function  GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;

    class procedure SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox;
      NewTraverseList: boolean); virtual;
    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;}
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
{    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); override;
    class procedure SetReadOnly(const ACustomComboBox: TCustomComboBox; NewReadOnly: boolean); override;}

    class function GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
//    class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); override;
  end;

  { TQtWSComboBox }

  TQtWSComboBox = class(TWSComboBox)
  private
  protected
  public
  end;

  { TQtWSCustomListBox }

  TQtWSCustomListBox = class(TWSCustomListBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
     const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  public
    class function  GetSelCount(const ACustomListBox: TCustomListBox): integer; override;
    class function  GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;
    class function  GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class function  GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class function  GetTopIndex(const ACustomListBox: TCustomListBox): integer; override;

    class procedure SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean); override;
    class procedure SetBorder(const ACustomListBox: TCustomListBox); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect, AMultiSelect: boolean); override;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;
    class procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); override;
//    class procedure SetColor(const AWinControl: TWinControl); override;
  end;

  { TQtWSListBox }

  TQtWSListBox = class(TWSListBox)
  private
  protected
  public
  end;

  { TQtWSCustomEdit }

  TQtWSCustomEdit = class(TWSCustomEdit)
  private
  protected
  public
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  public
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
{    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;

    class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;

    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer); override;}
  end;

  { TQtWSCustomMemo }

  TQtWSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  public
    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); override;
    class function  GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;
//    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); virtual;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); override;
  public
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
{    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;

    class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
}    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
{    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;

    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer); override;}
  end;

  { TQtWSEdit }

  TQtWSEdit = class(TWSEdit)
  private
  protected
  public
  end;

  { TQtWSMemo }

  TQtWSMemo = class(TWSMemo)
  private
  protected
  public
  end;

  { TQtWSButtonControl }

  TQtWSButtonControl = class(TWSButtonControl)
  private
  protected
  public
  end;

  { TQtWSCustomCheckBox }

  TQtWSCustomCheckBox = class(TWSCustomCheckBox)
  private
  protected
  public
    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox;
      const OldShortCut, NewShortCut: TShortCut); override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;
  public
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;

    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;

    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

  { TQtWSCheckBox }

  TQtWSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TQtWSToggleBox }

  TQtWSToggleBox = class(TWSToggleBox)
  private
  protected
  public
  end;

  { TQtWSRadioButton }

  TQtWSRadioButton = class(TWSRadioButton)
  private
  protected
  public
    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox;
      const OldShortCut, NewShortCut: TShortCut); override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;
  public
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;

    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;

    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

  { TQtWSCustomStaticText }

  TQtWSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment); override;

    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;

    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TQtWSStaticText }

  TQtWSStaticText = class(TWSStaticText)
  private
  protected
  public
  end;


implementation

uses LMessages;

{ TQtWSScrollBar }

{------------------------------------------------------------------------------
  Method: TQtWSCustomScrollBar.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSScrollBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  QtScrollBar: TQtScrollBar;
  Method: TMethod;
  Hook : QScrollBar_hookH;
begin
  QtScrollBar := TQtScrollBar.Create(AWinControl, AParams);

  case TScrollBar(AWinControl).Kind of
    sbHorizontal:
    begin
      QtScrollBar.SetOrientation(QtHorizontal);
      QtScrollBar.setInvertedAppereance(False);
      QTScrollBar.setInvertedControls(False);
    end;
    sbVertical:
    begin
      QtScrollBar.SetOrientation(QtVertical);
      QtScrollBar.setInvertedAppereance(False);
      QTScrollBar.setInvertedControls(False);
    end;
  end;

  QtScrollbar.setWidth(TScrollBar(AWinControl).Width);
  QtScrollbar.setHeight(TScrollBar(AWinControl).Height);
  QtScrollbar.setValue(TScrollBar(AWinControl).Position);
  QtScrollBar.setPageStep(TScrollBar(AWinControl).PageSize);

  // Various Events
//  Hook := QScrollBar_hook_create(QtScrollBar.Widget);
  // TEventFilterMethod(Method) := QtScrollBar.EventFilter;
  
  Result := THandle(QtScrollbar);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomScrollBar.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSScrollBar.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtScrollBar(AWinControl.Handle).Free;
end;


{------------------------------------------------------------------------------
  Method: TQtWSCustomScrollBar.SetParams
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSScrollBar.SetParams(const AScrollBar: TCustomScrollBar);
var
  QtScrollBar: TQtScrollBar;
begin
  QtScrollBar := TQtScrollBar(AScrollBar.Handle);	
// TODO: Check HeightForWidth() set here or we have lazarus bug ?
// AScrollBar.Width;

  QtScrollBar.setValue(AScrollBar.Position);
  QtScrollBar.setPageStep(AScrollBar.PageSize);
  QtScrollBar.setRange(AScrollBar.Min, AScrollBar.Max);
     
  QtScrollbar.setWidth(AscrollBar.Width);
  QtScrollbar.setHeight(AscrollBar.Height);

  case AScrollBar.Kind of
    sbHorizontal:
    begin
      QTScrollBar.SetOrientation(QtHorizontal);
      QTScrollBar.setInvertedAppereance(False);
      QTScrollBar.setInvertedControls(False);
    end;
    sbVertical:
    begin
      QTScrollBar.SetOrientation(QtVertical);
      QTScrollBar.setInvertedAppereance(False);
      QTScrollBar.setInvertedControls(False);
    end;
  end;
end;


{ TQtWSCustomListBox }

{------------------------------------------------------------------------------
  Method: TQtWSCustomListBox.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomListBox.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  QtListWidget: TQtListWidget;
  Method: TMethod;
  Hook : QListWidget_hookH;
begin
  QtListWidget := TQtListWidGet.Create(AWinControl, AParams);
  
  // Various Events

  Hook := QListWidget_hook_create(QtListWidget.Widget);

  TEventFilterMethod(Method) := QtListWidget.EventFilter;

  QObject_hook_hook_events(Hook, Method);

  // OnSelectionChange event

  QListWidget_currentItemChanged_Event(Method) := QtListWidget.SlotSelectionChange;

  QListWidget_hook_hook_currentItemChanged(Hook, Method);

  Result := THandle(QtListWidget);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListBox.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListBox.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtListWidget(AWinControl.Handle).Free;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListBox.GetSelCount
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomListBox.GetSelCount(const ACustomListBox: TCustomListBox): integer;
begin
  Result := 0;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListBox.GetSelected
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomListBox.GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListBox.GetStrings
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomListBox.GetStrings(const ACustomListBox: TCustomListBox): TStrings;
var
  ListWidgetH: QListWidgetH;
begin
  ListWidgetH := QListWidgetH((TQtWidget(ACustomListBox.Handle).Widget));
  Result := TQtListStrings.Create(ListWidgetH, ACustomListBox);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListBox.GetItemIndex
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomListBox.GetItemIndex(const ACustomListBox: TCustomListBox): integer;
begin
  Result := TQtListWidget(ACustomListBox.Handle).currentRow;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListBox.GetTopIndex
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomListBox.GetTopIndex(const ACustomListBox: TCustomListBox): integer;
begin
  Result := 0;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListBox.SelectItem
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListBox.SelectItem(const ACustomListBox: TCustomListBox;
  AIndex: integer; ASelected: boolean);
begin

end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListBox.SetBorder
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListBox.SetBorder(const ACustomListBox: TCustomListBox);
begin

end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListBox.SetItemIndex
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListBox.SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer);
begin
  TQtListWidget(ACustomListBox.Handle).setCurrentRow(AIndex);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListBox.SetSelectionMode
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListBox.SetSelectionMode(
  const ACustomListBox: TCustomListBox; const AExtendedSelect, AMultiSelect: boolean);
begin

end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListBox.SetSorted
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListBox.SetSorted(const ACustomListBox: TCustomListBox;
  AList: TStrings; ASorted: boolean);
begin

end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListBox.SetTopIndex
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListBox.SetTopIndex(const ACustomListBox: TCustomListBox;
  const NewTopIndex: integer);
begin

end;

{ TQtWSCustomMemo }

{------------------------------------------------------------------------------
  Method: TQtWSCustomMemo.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  QtTextEdit: TQtTextEdit;
begin
  QtTextEdit := TQtTextEdit.Create(AWinControl, AParams);
  Result := THandle(QtTextEdit);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomMemo.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomMemo.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtTextEdit(AWinControl.Handle).Free;
end;


{------------------------------------------------------------------------------
  Method: TQtWSCustomMemo.AppendText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomMemo.AppendText(const ACustomMemo: TCustomMemo; const AText: string);
var
  Astr: WideString;
begin
  if Length(AText) = 0 then
    exit;
  Astr := UTF8Decode(AText);
  //QTextEdit_append(QTextEditH(ACustomMemo.Handle),@Astr);
  QTextEdit_append(QTextEditH(TQtWidget(ACustomMemo.Handle).Widget),@Astr);

end;


{------------------------------------------------------------------------------
  Method: TQtWSCustomMemo.GetStrings
  Params:  None
  Returns: Memo Contents as TStrings
 ------------------------------------------------------------------------------}
class function TQtWSCustomMemo.GetStrings(const ACustomMemo: TCustomMemo): TStrings;
var
  TextEditH: QTextEditH;
begin
  TextEditH := QTextEditH((TQtWidget(ACustomMemo.Handle).Widget));  // set to proper type
  Result := TQtMemoStrings.Create(TextEditH,ACustomMemo);
end;


{------------------------------------------------------------------------------
  Method: TQtWSCustomMemo.SetWordWrap
  Params:  NewWordWrap boolean
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomMemo.SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean);
var
  TextEditH: QTextEditH;
begin
  TextEditH := QTextEditH((TQtWidget(ACustomMemo.Handle).Widget));  // set to proper type
  if NewWordWrap then
     QTextEdit_setLineWrapMode(TextEditH,QTextEditWidgetWidth)
  else
     QTextEdit_setLineWrapMode(TextEditH,QTextEditNoWrap);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomMemo.GetText
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomMemo.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  Str: WideString;
begin
  QTextEdit_toPlainText(QTextEditH(TQtWidget(AWinControl.Handle).Widget), @Str);

  AText := UTF8Encode(Str);

  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomMemo.SetText
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomMemo.SetText(const AWinControl: TWinControl; const AText: string);
var
  AString: WideString;
begin
  AString := UTF8Decode(AText);

  QTextEdit_append(QTextEditH(TQtWidget(AWinControl.Handle).Widget), @AString);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomMemo.SetReadOnly
  Params:  NewReadOnly boolean
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomMemo.SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
var
  TextEditH: QTextEditH;
begin
  TextEditH := QTextEditH((TQtWidget(ACustomEdit.Handle).Widget));  // set to proper type
  QTextEdit_setReadOnly(TextEditH,NewReadOnly);
end;

{ TQtWSCustomEdit }

{------------------------------------------------------------------------------
  Method: TQtWSCustomEdit.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  QtLineEdit: TQtLineEdit;
begin
  QtLineEdit := TQtLineEdit.Create(AWinControl, AParams);

  Result := THandle(QtLineEdit);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomEdit.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomEdit.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtLineEdit(AWinControl.Handle).Free;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomEdit.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomEdit.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  Str: WideString;
begin
  QLineEdit_text(QLineEditH(TQtWidget(AWinControl.Handle).Widget), @Str);

  AText := UTF8Encode(Str);

  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomEdit.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomEdit.SetText(const AWinControl: TWinControl; const AText: string);
var
  AString: WideString;
begin
  AString := UTF8Decode(AText);

  QLineEdit_setText(QLineEditH(TQtWidget(AWinControl.Handle).Widget), @AString);
end;

{ TQtWSStaticText }

{------------------------------------------------------------------------------
  Method: TQtWSCustomStaticText.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomStaticText.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtStaticText: TQtStaticText;
begin
  QtStaticText := TQtStaticText.Create(AWinControl, AParams);

//  SetSlots(QtStaticText);

  // Returns the Handle

  Result := THandle(QtStaticText);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomStaticText.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomStaticText.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtStaticText(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomStaticText.SetAlignment
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomStaticText.SetAlignment(
  const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment);
begin
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomStaticText.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomStaticText.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  Str: WideString;
begin
  TQtStaticText(AWinControl.Handle).Text(@Str);

  AText := UTF8Encode(Str);

  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomStaticText.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomStaticText.SetText(const AWinControl: TWinControl; const AText: String);
var
  Str: WideString;
begin
  Str := UTF8Decode(AText);

  TQtStaticText(AWinControl.Handle).SetText(@Str);
end;

{ TQtWSCustomCheckBox }

{------------------------------------------------------------------------------
  Method: TQtWSCustomEdit.RetrieveState
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomCheckBox.RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
begin
  case TQtCheckBox(ACustomCheckBox.Handle).CheckState of
   QtPartiallyChecked: Result := cbGrayed;
   QtChecked: Result := cbChecked;
  else
    Result := cbUnchecked;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomEdit.SetShortCut
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomCheckBox.SetShortCut(const ACustomCheckBox: TCustomCheckBox;
  const OldShortCut, NewShortCut: TShortCut);
begin
  inherited SetShortCut(ACustomCheckBox, OldShortCut, NewShortCut);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomEdit.SetState
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomCheckBox.SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
begin
  case NewState of
   cbGrayed: TQtCheckBox(ACustomCheckBox.Handle).setCheckState(QtPartiallyChecked);
   cbChecked: TQtCheckBox(ACustomCheckBox.Handle).setCheckState(QtChecked);
  else
    TQtCheckBox(ACustomCheckBox.Handle).setCheckState(QtUnchecked);
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomEdit.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomCheckBox.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  Str: WideString;
begin
  TQtAbstractButton(AWinControl.Handle).Text(@Str);

  AText := UTF8Encode(Str);

  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomEdit.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomCheckBox.SetText(const AWinControl: TWinControl; const AText: String);
var
  Str: WideString;
begin
  Str := UTF8Decode(AText);

  TQtAbstractButton(AWinControl.Handle).SetText(@Str);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomCheckBox.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TQtWSCustomCheckBox.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtCheckBox: TQtCheckBox;
begin
  QtCheckBox := TQtCheckBox.Create(AWinControl, AParams);

  // Focus

  QWidget_setFocusPolicy(QtCheckBox.Widget, QtStrongFocus);
  
  // Returns the Handle

  Result := THandle(QtCheckBox);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomCheckBox.DestroyHandle
  Params:  None
  Returns: Nothing

  Releases allocated memory and resources
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomCheckBox.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtCheckBox(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

{ TQtWSRadioButton }

{------------------------------------------------------------------------------
  Method: TQtWSRadioButton.RetrieveState
  Params:  None
  Returns: The state of the control
 ------------------------------------------------------------------------------}
class function TQtWSRadioButton.RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
begin
  if TQtAbstractButton(ACustomCheckBox.Handle).isChecked then Result := cbChecked
  else Result := cbUnchecked;
end;

{------------------------------------------------------------------------------
  Method: TQtWSRadioButton.SetShortCut
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSRadioButton.SetShortCut(const ACustomCheckBox: TCustomCheckBox;
  const OldShortCut, NewShortCut: TShortCut);
begin
  inherited SetShortCut(ACustomCheckBox, OldShortCut, NewShortCut);
end;

{------------------------------------------------------------------------------
  Method: TQtWSRadioButton.SetState
  Params:  None
  Returns: Nothing

  Sets the state of the control
 ------------------------------------------------------------------------------}
class procedure TQtWSRadioButton.SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
begin
  case NewState of
   cbUnchecked: TQtAbstractButton(ACustomCheckBox.Handle).setChecked(False);
   cbChecked: TQtAbstractButton(ACustomCheckBox.Handle).setChecked(true);
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSRadioButton.GetText
  Params:  None
  Returns: The text of the control
 ------------------------------------------------------------------------------}
class function TQtWSRadioButton.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  Str: WideString;
begin
  TQtAbstractButton(AWinControl.Handle).Text(@Str);

  AText := UTF8Encode(Str);

  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TQtWSRadioButton.SetText
  Params:  None
  Returns: Nothing

  Changes the text of the control
 ------------------------------------------------------------------------------}
class procedure TQtWSRadioButton.SetText(const AWinControl: TWinControl; const AText: String);
var
  Str: WideString;
begin
  Str := UTF8Decode(AText);

  TQtAbstractButton(AWinControl.Handle).SetText(@Str);
end;

{------------------------------------------------------------------------------
  Method: TQtWSRadioButton.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TQtWSRadioButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtRadioButton: TQtRadioButton;
{  Method: TMethod;
  Hook : QObject_hookH;}
begin
  QtRadioButton := TQtRadioButton.Create(AWinControl, AParams);

  // Various Events

{  Hook := QObject_hook_create(QtRadioButton.Widget);

  TEventFilterMethod(Method) := QtRadioButton.EventFilter;

  QObject_hook_hook_events(Hook, Method);}
  
  // Focus
  
  QWidget_setFocusPolicy(QtRadioButton.Widget, QtStrongFocus);
  
  // Returns the Handle

  Result := THandle(QtRadioButton);
end;

{------------------------------------------------------------------------------
  Method: TQtWSRadioButton.DestroyHandle
  Params:  None
  Returns: Nothing

  Releases allocated memory and resources
 ------------------------------------------------------------------------------}
class procedure TQtWSRadioButton.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtRadioButton(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

{ TQtWSCustomGroupBox }

{------------------------------------------------------------------------------
  Method: TQtWSCustomGroupBox.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TQtWSCustomGroupBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtGroupBox: TQtGroupBox;
  Str: WideString;
begin
  QtGroupBox := TQtGroupBox.Create(AWinControl, AParams);

// If SetSlots is uncommented, then TRadioGroup stops working
// This needs further investigation
//  SetSlots(QtButtonGroup);

  Str := UTF8Decode(AWinControl.Caption);
  QGroupBox_setTitle(QGroupBoxH(QtGroupBox.Widget), @Str);

  // Returns the Handle

  Result := THandle(QtGroupBox);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomGroupBox.DestroyHandle
  Params:  None
  Returns: Nothing

  Releases allocated memory and resources
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomGroupBox.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtGroupBox(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

{ TQtWSCustomComboBox }

{------------------------------------------------------------------------------
  Method: TQtWSCustomComboBox.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TQtWSCustomComboBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtComboBox: TQtComboBox;
  Method: TMethod;
  Hook : QObject_hookH;
begin
  QtComboBox := TQtComboBox.Create(AWinControl, AParams);

  // Various Events

  Hook := QObject_hook_create(QtComboBox.Widget);

  TEventFilterMethod(Method) := QtComboBox.EventFilter;

  QObject_hook_hook_events(Hook, Method);

  // OnChange event

  QComboBox_editTextChanged_Event(Method) := QtComboBox.SlotChange;

  QComboBox_hook_hook_editTextChanged(
   QComboBox_hook_create(QtComboBox.Widget), Method);

  // OnSelect event

  QComboBox_currentIndexChanged_Event(Method) := QtComboBox.SlotSelect;

  QComboBox_hook_hook_currentIndexChanged(
   QComboBox_hook_create(QtComboBox.Widget), Method);

  Result := THandle(QtComboBox);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomComboBox.DestroyHandle
  Params:  None
  Returns: Nothing

  Releases allocated memory and resources
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomComboBox.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtComboBox(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomComboBox.GetItemIndex
  Params:  None
  Returns: The state of the control
 ------------------------------------------------------------------------------}
class function TQtWSCustomComboBox.GetItemIndex(
  const ACustomComboBox: TCustomComboBox): integer;
begin
  Result := TQtComboBox(ACustomComboBox.Handle).currentIndex;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomComboBox.SetItemIndex
  Params:  None
  Returns: The state of the control
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomComboBox.SetItemIndex(
  const ACustomComboBox: TCustomComboBox; NewIndex: integer);
begin
  TQtComboBox(ACustomComboBox.Handle).setCurrentIndex(NewIndex);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomComboBox.GetItems
  Params:  None
  Returns: The state of the control
 ------------------------------------------------------------------------------}
class function TQtWSCustomComboBox.GetItems(const ACustomComboBox: TCustomComboBox): TStrings;
var
  ComboBoxH: QComboBoxH;
begin
  ComboBoxH := QComboBoxH((TQtWidget(ACustomComboBox.Handle).Widget));
  Result := TQtComboStrings.Create(ComboBoxH, ACustomComboBox);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TScrollBar, TQtWSScrollBar);
  RegisterWSComponent(TCustomGroupBox, TQtWSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TQtWSGroupBox);
  RegisterWSComponent(TCustomComboBox, TQtWSCustomComboBox);
//  RegisterWSComponent(TComboBox, TQtWSComboBox);
  RegisterWSComponent(TCustomListBox, TQtWSCustomListBox);
//  RegisterWSComponent(TListBox, TQtWSListBox);
  RegisterWSComponent(TCustomEdit, TQtWSCustomEdit);
  RegisterWSComponent(TCustomMemo, TQtWSCustomMemo);
//  RegisterWSComponent(TEdit, TQtWSEdit);
//  RegisterWSComponent(TMemo, TQtWSMemo);
//  RegisterWSComponent(TCustomLabel, TQtWSCustomLabel);
//  RegisterWSComponent(TLabel, TQtWSLabel);
//  RegisterWSComponent(TButtonControl, TQtWSButtonControl);
  RegisterWSComponent(TCustomCheckBox, TQtWSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TQtWSCheckBox);
//  RegisterWSComponent(TCheckBox, TQtWSCheckBox);
//  RegisterWSComponent(TToggleBox, TQtWSToggleBox);
  RegisterWSComponent(TRadioButton, TQtWSRadioButton);
  RegisterWSComponent(TCustomStaticText, TQtWSCustomStaticText);
//  RegisterWSComponent(TStaticText, TQtWSStaticText);
////////////////////////////////////////////////////
end.
