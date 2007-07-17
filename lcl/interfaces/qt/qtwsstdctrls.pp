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
{$ifdef USE_QT_4_3}
  qt43,
{$else}
  qt4,
{$endif}
  qtprivate, qtwidgets,
  // LCL
  Classes, StdCtrls, Controls, Graphics, Forms, SysUtils, InterfaceBase, LCLType, LCLIntf, LCLProc,
  // Widgetset
  WSProc, WSStdCtrls, WSLCLClasses;

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
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); override;}

    class function GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetReadOnly(const ACustomComboBox: TCustomComboBox; NewReadOnly: boolean); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
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
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
{    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;

    class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;

    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer); override;}
    class procedure SetColor(const AWinControl: TWinControl); override;
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
    class procedure SetAlignment(const ACustomMemo: TCustomMemo; const AAlignment: TAlignment); override;
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

  { TQtWSButton }

  TQtWSButton = class(TWSButton)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
//    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
//    class procedure SetShortcut(const AButton: TCustomButton; const OldShortcut, NewShortcut: TShortcut); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
//    class procedure GetPreferredSize(const AWinControl: TWinControl;
//                        var PreferredWidth, PreferredHeight: integer); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
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
begin
  QtScrollBar := TQtScrollBar.Create(AWinControl, AParams);

  case TScrollBar(AWinControl).Kind of
    sbHorizontal:
    begin
      QtScrollBar.SetOrientation(QtHorizontal);
      QtScrollBar.setInvertedAppereance(False);
      QTScrollBar.setInvertedControls(False);
      TScrollBar(AWinControl).Height := TScrollBar(AWinControl).Height;
    end;
    sbVertical:
    begin
      QtScrollBar.SetOrientation(QtVertical);
      QtScrollBar.setInvertedAppereance(False);
      QTScrollBar.setInvertedControls(True);
      TScrollBar(AWinControl).Width := TScrollBar(AWinControl).Width;
    end;
  end;
  
  QWidget_setGeometry(QtScrollbar.Widget,TScrollBar(AWinControl).Left,TScrollBar(AWinControl).Top,TScrollBar(AWinControl).Width,TScrollBar(AWinControl).Height);
  QtScrollBar.setRange(TScrollBar(AWinControl).Min, TScrollBar(AWinControl).Max);
  QtScrollbar.setValue(TScrollBar(AWinControl).Position);
  QtScrollBar.setPageStep(TScrollBar(AWinControl).PageSize);
  QtScrollBar.AttachEvents;

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
  AWinControl.Handle := 0;
end;


{------------------------------------------------------------------------------
  Method: TQtWSCustomScrollBar.SetParams
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSScrollBar.SetParams(const AScrollBar: TCustomScrollBar);
var
  QtScrollBar: TQtScrollBar;
  RA,RB: TRect;
  IsSameGeometry: Boolean;
begin
  QtScrollBar := TQtScrollBar(AScrollBar.Handle);	

  QtScrollBar.setValue(AScrollBar.Position);
  QtScrollBar.setPageStep(AScrollBar.PageSize);
  QtScrollBar.setRange(AScrollBar.Min, AScrollBar.Max);
  
  RA := QtScrollBar.LCLObject.ClientRect;
  RB := AScrollBar.ClientRect;
  
  IsSameGeometry := (RA.Left = RB.Left) and (RA.Top = RB.Top) and (RA.Right = RB.Right) and (RA.Bottom = RB.Bottom);

  if not IsSameGeometry then
    QWidget_setGeometry(QtScrollbar.Widget,AScrollBar.Left,AScrollBar.Top,AScrollBar.Width,AScrollBar.Height);
    {don't update geometry each time}

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
      QTScrollBar.setInvertedControls(True);
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
begin
  QtListWidget := TQtListWidGet.Create(AWinControl, AParams);
  
  QtListWidget.AttachEvents;
  
  // QListWidget_itemClicked_Event(Method;
  
  // create our FList helper
  QtListWidget.FList := TQtListStrings.Create(QListWidgetH(QtListWidget.Widget), TCustomListBox(AWinControl));


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
  AWinControl.Handle := 0;
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
  if not Assigned(TQtListWidget(ACustomListBox.Handle).FList) then
  begin
    ListWidgetH := QListWidgetH(TQtListWidget(ACustomListBox.Handle).Widget);
    TQtListWidget(ACustomListBox.Handle).FList := TQtListStrings.Create(ListWidgetH, ACustomListBox);
  end;
  Result := TQtListWidget(ACustomListBox.Handle).FList;
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
  QtTextEdit.AttachEvents;
  
  // create our FList helper
  QtTextEdit.FList := TQtMemoStrings.Create(QTextEditH(QtTextEdit.Widget), TCustomMemo(AWinControl));
  
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
  AWinControl.Handle := 0;
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
  QTextEdit_append(QTextEditH(TQtWidget(ACustomMemo.Handle).Widget),@Astr);
end;

class procedure TQtWSCustomMemo.SetAlignment(const ACustomMemo: TCustomMemo;
  const AAlignment: TAlignment);
begin
  TQtTextEdit(ACustomMemo.Handle).SetAlignment(ACustomMemo.Alignment);
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
  if not Assigned(TQtTextEdit(ACustomMemo.Handle).FList) then
  begin
    TextEditH := QTextEditH((TQtTextEdit(ACustomMemo.Handle).Widget));  // set to proper type
    TQtTextEdit(ACustomMemo.Handle).FList := TQtMemoStrings.Create(TextEditH,ACustomMemo);
  end;
  
  Result := TQtTextEdit(ACustomMemo.Handle).FList;
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
	QTextEdit_setPlainText(QTextEditH(TQtWidget(AWinControl.Handle).Widget), @AString);
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
  QtLineEdit.AttachEvents;

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
  AWinControl.Handle := 0;
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
  Method: TQtWSCustomEdit.SetEchoMode
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomEdit.SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode);
begin
  QLineEdit_setEchoMode(QLineEditH(TQtLineEdit(ACustomEdit.Handle).Widget),QLineEditEchoMode(Ord(NewMode)));
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomEdit.SetMaxLength
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomEdit.SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer);
begin
  if NewLength >= 0 then {qt doesn't accept -1 !}
  QLineEdit_setMaxLength(QLineEditH(TQtLineEdit(ACustomEdit.Handle).Widget),NewLength);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomEdit.SetReadOnly
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomEdit.SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
begin
  QLineEdit_setReadOnly(QLineEditH(TQtLineEdit(ACustomEdit.Handle).Widget), NewReadOnly);
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

{------------------------------------------------------------------------------
  Method: TQtWSCustomEdit.SetColor
  Params:  AWinControl     - the calling object

  Returns: Nothing

  Sets the color of the widget.
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomEdit.SetColor(const AWinControl: TWinControl);
var
  QColor: TQColor;
  Color: TColor;
begin
  if AWinControl = nil then exit;

  if not AWinControl.HandleAllocated then exit;

  if AWinControl.Color = CLR_INVALID then exit;

  // Get the color numeric value (system colors are mapped to numeric colors depending on the widget style)
  Color:=ColorToRGB(AWinControl.Color);

  // Fill QColor
  QColor_setRgb(QColorH(@QColor),Red(Color),Green(Color),Blue(Color));

  // Set color of the widget to QColor
  TQtLineEdit(AWinControl.Handle).SetColor(@QColor);
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

  QtStaticText.AttachEvents;
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

  if (csDestroying in AWinControl.ComponentState)
  or (csFreeNotification in AWinControl.ComponentState)
  then
    exit;

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

{ TQtWSButton }

{------------------------------------------------------------------------------
  Function: TQtWSButton.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TQtWSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtPushButton: TQtPushButton;
begin
  QtPushButton := TQtPushButton.Create(AWinControl, AParams);
  QtPushButton.AttachEvents;

  // Focus
  QWidget_setFocusPolicy(QtPushButton.Widget, QtStrongFocus);

  // Returns the Handle
  Result := THandle(QtPushButton);
end;

{------------------------------------------------------------------------------
  Function: TQtWSButton.DestroyHandle
  Params:  None
  Returns: Nothing

  Releases allocated memory and resources
 ------------------------------------------------------------------------------}
class procedure TQtWSButton.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtPushButton(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

{------------------------------------------------------------------------------
  Function: TQtWSButton.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSButton.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  Str: WideString;
begin
  Result := False;

  if not WSCheckHandleAllocated(AWincontrol, 'GetText') then Exit;

  TQtAbstractButton(AWinControl.Handle).Text(@Str);

  AText := UTF8Encode(Str);

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TQtWSButton.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSButton.SetText(const AWinControl: TWinControl; const AText: String);
var
  Str: WideString;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetText') then Exit;

  Str := UTF8Decode(AText);

  TQtAbstractButton(AWinControl.Handle).SetText(@Str);
end;

{------------------------------------------------------------------------------
  Method: TQtWSButton.SetColor
  Params:  AWinControl     - the calling object

  Returns: Nothing

  Sets the color of the widget.
 ------------------------------------------------------------------------------}
class procedure TQtWSButton.SetColor(const AWinControl: TWinControl);
var
  QColor: TQColor;
  Color: TColor;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetColor') then Exit;

  if AWinControl.Color = CLR_INVALID then exit;

  // Get the color numeric value (system colors are mapped to numeric colors depending on the widget style)
  Color:=ColorToRGB(AWinControl.Color);

  // Fill QColor
  QColor_setRgb(@QColor,Red(Color),Green(Color),Blue(Color));

  // Set color of the widget to QColor
  TQtAbstractButton(AWinControl.Handle).SetColor(@QColor);
end;

{ TQtWSCustomCheckBox }

{------------------------------------------------------------------------------
  Method: TQtWSCustomCheckBox.RetrieveState
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
  Method: TQtWSCustomCheckBox.SetShortCut
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomCheckBox.SetShortCut(const ACustomCheckBox: TCustomCheckBox;
  const OldShortCut, NewShortCut: TShortCut);
begin
  inherited SetShortCut(ACustomCheckBox, OldShortCut, NewShortCut);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomCheckBox.SetState
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
  Method: TQtWSCustomCheckBox.GetText
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
  Method: TQtWSCustomCheckBox.SetText
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
  ATextWidth: Integer;
  FM: QFontMetricsH;
  Str: WideString;
begin
  QtCheckBox := TQtCheckBox.Create(AWinControl, AParams);
  QtCheckBox.AttachEvents;

  // Focus
  // QWidget_setFocusPolicy(QtCheckBox.Widget, QtStrongFocus);
  {we have a bug in LCL when parent is TCustomCheckGroup, it doesn't set sizes for items ?!? Width = 0 , Height = 0}
  // writeln('WW=',QWidget_width(QtCheckBox.Widget),' WH=',QWidget_height(QtCheckBox.Widget),' WCW=',AWinControl.Width,' WCH=',AWinControl.Height,' CAPTION=',TCustomCheckBox(AWinControl).Caption);

  {we must cheat TCustomCheckGroup here with some reasonable CheckBox size...}
  if AWinControl.Height = 0 then
  begin
      { we must calculate text size to get real checkbox size in TCustomCheckGroup }
      FM := QFontMetrics_create(QWidget_font(QtCheckBox.Widget));
      try
      Str := UTF8Encode(AWinControl.Caption);
      ATextWidth := QFontMetrics_width(FM, @Str, Length(Str));
      finally
      QFontMetrics_destroy(FM);
      end;
      { now, textwidth + default width of checkbox, default height
        qt doesn't align well control with text size < 100}
      if ATextWidth < 100 then
      ATextWidth := 100;
      
      AWinControl.SetInitialBounds(0, 0, ATextWidth + 22, 22);
  end;

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
  ATextWidth: Integer;
  FM: QFontMetricsH;
  Str: WideString;
begin
  QtRadioButton := TQtRadioButton.Create(AWinControl, AParams);
  QtRadioButton.AttachEvents;

  {we must cheat TCustomRadioGroup here with some reasonable RadioButton size...}
  if AWinControl.Height = 0 then
  begin
    { we must calculate text size to get real radiobutton size in TCustomRadioGroup }
    FM := QFontMetrics_create(QWidget_font(QtRadioButton.Widget));
    try
    Str := UTF8Encode(AWinControl.Caption);
    ATextWidth := QFontMetrics_width(FM, @Str, Length(Str));
    finally
    QFontMetrics_destroy(FM);
    end;
    { now, textwidth + default width of radiobutton (including space), default height
      qt doesn't well align control with textsize < 100 }
    if ATextWidth < 100 then
    ATextWidth := 100;
    AWinControl.SetInitialBounds(0, 0, ATextWidth + 22, 22);
  end;

  // Focus
  
  //QWidget_setFocusPolicy(QtRadioButton.Widget, QtStrongFocus);
  
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
  QtGroupBox.AttachEvents;

// If SetSlots is uncommented, then TRadioGroup stops working
// This needs further investigation --> Problem is with child controls sizes (zeljko@holobit.net)
// SetSlots(QtButtonGroup);

  Str := UTF8Decode(AWinControl.Caption);
  QGroupBox_setTitle(QGroupBoxH(QtGroupBox.Widget), @Str);
  
// LCL doesn't have such features ...
// QGroupBox_setCheckable(QGroupBoxH(QtGroupBox.Widget), True);

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
begin
  QtComboBox := TQtComboBox.Create(AWinControl, AParams);
  QtComboBox.AttachEvents;
  
  // create our FList helper

  QtComboBox.FList := TQtComboStrings.Create(QComboBoxH(QtComboBox.Widget), TCustomComboBox(AWinControl));
  
  // we must save itemindex, since GetItems() should delete it.
  QtComboBox.FSavedItemIndex := TCustomComboBox(AWinControl).ItemIndex;

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
  TQtComboBox(ACustomComboBox.Handle).setCurrentIndex(TQtComboBox(ACustomComboBox.Handle).FSavedItemIndex);
  TQtComboBox(ACustomComboBox.Handle).FSavedItemIndex := NewIndex;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomComboBox.GetItems
  Params:  None
  Returns: ComboBox items
 ------------------------------------------------------------------------------}
class function TQtWSCustomComboBox.GetItems(const ACustomComboBox: TCustomComboBox): TStrings;
var
  ComboBoxH: QComboBoxH;
begin
  if not Assigned(TQtComboBox(ACustomComboBox.Handle).FList) then
  begin
    ComboBoxH := QComboBoxH((TQtComboBox(ACustomComboBox.Handle).Widget));
    TQtComboBox(ACustomComboBox.Handle).FList := TQtComboStrings.Create(ComboBoxH, ACustomComboBox);
  end;
  {TODO: ask why GetItems() always clears the list, that's why FSavedItemIndex exists. }
  Result := TQtComboBox(ACustomComboBox.Handle).FList;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomComboBox.SetReadOnly
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomComboBox.SetReadOnly(const ACustomComboBox: TCustomComboBox; NewReadOnly: boolean);
var
  ComboBoxH: QComboBoxH;
begin
  ComboBoxH := QComboBoxH((TQtWidget(ACustomComboBox.Handle).Widget));
  QComboBox_setEditable(ComboBoxH, not NewReadOnly);
end;

class function TQtWSCustomComboBox.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  QtComboBox: TQtComboBox;
  Str: WideString;
begin
  Result := False;
  if AWinControl.Handle = 0 then exit;
  
  QtComboBox := TQtComboBox(AWinControl.Handle);
  
  QComboBox_currentText(QComboBoxH(QtComboBox.Widget), @Str);
  
  AText := UTF8Encode(Str);
  Result := True;
end;

class procedure TQtWSCustomComboBox.SetText(const AWinControl: TWinControl; const AText: string);
var
  QtComboBox: TQtComboBox;
  Str: WideString;
begin
  QtComboBox := TQtComboBox(AWinControl.Handle);
  Str := UTF8Decode(AText);
  QComboBox_setEditText(QComboBoxH(QtComboBox.Widget), @Str);
end;


{ TQtWSToggleBox }

{------------------------------------------------------------------------------
  Method: TQtWSToggleBox.RetrieveState
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSToggleBox.RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
begin
  if QAbstractButton_isDown(QAbstractButtonH(TQtPushButton(ACustomCheckBox.Handle).Widget))
  then
   Result := cbChecked
  else
   Result := cbUnChecked;
end;

{------------------------------------------------------------------------------
  Method: TQtWSToggleBox.SetShortCut
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSToggleBox.SetShortCut(const ACustomCheckBox: TCustomCheckBox;
  const OldShortCut, NewShortCut: TShortCut);
begin
  inherited SetShortCut(ACustomCheckBox, OldShortCut, NewShortCut);
end;

{------------------------------------------------------------------------------
  Method: TQtWSToggleBox.SetState
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSToggleBox.SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
var
  AState: TCheckBoxState;
begin
  if QAbstractButton_isDown(QAbstractButtonH(TQtPushButton(ACustomCheckBox.Handle).Widget))
  then
   AState := cbChecked
  else
   AState := cbUnChecked;

  if AState <> NewState then
    QAbstractButton_toggle(QAbstractButtonH(TQtPushButton(ACustomCheckBox.Handle).Widget));
end;

{------------------------------------------------------------------------------
  Method: TQtWSToggleBox.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSToggleBox.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  Str: WideString;
begin
  TQtAbstractButton(AWinControl.Handle).Text(@Str);

  AText := UTF8Encode(Str);

  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TQtWSToggleBox.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSToggleBox.SetText(const AWinControl: TWinControl; const AText: String);
var
  Str: WideString;
begin
  Str := UTF8Decode(AText);

  TQtAbstractButton(AWinControl.Handle).SetText(@Str);
end;

{------------------------------------------------------------------------------
  Method: TQtWSToggleBox.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TQtWSToggleBox.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtToggleBox: TQtPushButton;
begin

  QtToggleBox := TQtPushButton.Create(AWinControl, AParams);
  
  QAbstractButton_setCheckable(QAbstractButtonH(QtToggleBox.Widget), True);
  QtToggleBox.AttachEvents;
  
  QWidget_setFocusPolicy(QtToggleBox.Widget, QtTabFocus or QtClickFocus);

  Result := THandle(QtToggleBox);
end;

{------------------------------------------------------------------------------
  Method: TQtWSToggleBox.DestroyHandle
  Params:  None
  Returns: Nothing

  Releases allocated memory and resources
 ------------------------------------------------------------------------------}
class procedure TQtWSToggleBox.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtPushButton(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
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
  RegisterWSComponent(TCustomButton, TQtWSButton);
  RegisterWSComponent(TCustomCheckBox, TQtWSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TQtWSCheckBox);
//  RegisterWSComponent(TCheckBox, TQtWSCheckBox);
  RegisterWSComponent(TToggleBox, TQtWSToggleBox);
  RegisterWSComponent(TRadioButton, TQtWSRadioButton);
  RegisterWSComponent(TCustomStaticText, TQtWSCustomStaticText);
//  RegisterWSComponent(TStaticText, TQtWSStaticText);
////////////////////////////////////////////////////
end.
