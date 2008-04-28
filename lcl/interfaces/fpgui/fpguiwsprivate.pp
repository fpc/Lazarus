{ $Id: FPGUIwsprivate.pp 10697 2007-02-27 23:17:33Z marc $ }
{
                 ------------------------------------------
                 FPGUIwsprivate.pp  -  FPGUI internal classes
                 ------------------------------------------

 @created(Thu Feb 1st WET 2007)
 @lastmod($Date: 2007-02-27 18:17:33 -0500 (Tue, 27 Feb 2007) $)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains the private class hierarchy for the fpgui implemetation
 This hierarchy reflects (more or less) the  widget hierarchy

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

unit fpguiwsprivate;

{$mode delphi}

interface

uses
  // LCL
  LCLType, LMessages, LCLProc, Controls, Classes, SysUtils, Forms,
  LCLIntf,
  // widgetset
  WSControls, WSLCLClasses, WSProc,
  // interface
  gfx_widget, gui_form, gui_button, gui_combobox, gui_dialogs,
  gui_edit, gui_checkbox, gui_radiobutton, gui_tab;


type

  IContainer = interface(IInterface)
    procedure AddChild(AWidget: TfpgWidget);
    procedure RemoveChild(AWidget: TfpgWidget);
  end;

  ISimpleText = interface(IInterface)
    procedure SetText(const AText: String);
    function GetText: String;
  end;

  { TFPGUIPrivate }

  TFPGUIPrivate = class(TInterfacedObject)
    function _AddRef : longint;stdcall;
    function _Release : longint;stdcall;
  end;

  { TFPGUIPrivateWidget }
  { Private class for widgets }

  TFPGUIPrivateWidget = class(TFPGUIPrivate)
  private
    FWidget: TfpgWidget;
    FLCLObject: TWinControl;
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);
  protected
  public
    { Constructors / Destructors }
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); virtual;
    destructor Destroy; override;
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); virtual; abstract;
    procedure SetEvents; virtual;
    procedure SetSize(AWidth, AHeight: LongInt); virtual;
    procedure SetPosition(AX, AY: Integer); virtual;
  public
    { Properties }
    property LCLObject: TWinControl read FLCLObject;
    property Visible: Boolean read GetVisible write SetVisible;
    property Widget: TfpgWidget read FWidget write FWidget;
  end;


  { TFPGUIPrivateContainer }
  { Private class for containers }

  TFPGUIPrivateContainer = class(TFPGUIPrivateWidget, IContainer)
  private
  protected
  public
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
  // IContainer
    procedure AddChild(AWidget: TfpgWidget);
    procedure RemoveChild(AWidget: TfpgWidget);
  end;


  { TFPGUIPrivateBin }
  { Private class for bins }

  TFPGUIPrivateBin = class(TFPGUIPrivateContainer)
  private
  protected
  public
  end;


  { TFPGUIPrivateWindow }
  { Private class for windows }

  TFPGUIPrivateWindow = class(TFPGUIPrivateBin, ISimpleText)
  private
    { Event Handlers }
    procedure PaintHandler(Sender: TObject{; const ARect: TfpgRect});
  protected
  public
    { Constructors / Destructors }
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure SetEvents; override;
    procedure SetSize(AWidth, AHeight: LongInt); override;
    procedure SetPosition(AX, AY: Integer); override;
  public
    { Other methods }
    function Form: TfpgForm;
    { ISimpleText }
    procedure SetText(const AText: String);
    function GetText: String;
  end;


  { TFPGUIPrivateDialog }
  { Private class for dialogs }

  TFPGUIPrivateDialog = class(TfpgBaseDialog)
  private
  protected
  public
  end;


  { TFPGUIPrivateButton }
  { Private class for buttons }

  TFPGUIPrivateButton = class(TFPGUIPrivateWidget, ISimpleText)
  private
    procedure Clicked(Sender: TObject);
  protected
  public
    { Constructors / Destructors }
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); override;
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure SetEvents; override;
  public
    { Other methods }
    function Button: TfpgButton;
    { ISimpleText }
    procedure SetText(const AText: String);
    function GetText: String;
  end;

  { TFPGUIPrivateComboBox }

  TFPGUIPrivateComboBox = class(TFPGUIPrivateWidget)
  private
  protected
  public
    { Constructors / Destructors }
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); override;
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    { Other methods }
    function ComboBox: TfpgComboBox;
  end;


  { TFPGUIPrivateEdit }

  TFPGUIPrivateEdit = class(TFPGUIPrivateWidget, ISimpleText)
  private
  protected
  public
    { Constructors / Destructors }
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); override;
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    { Other methods }
    function Edit: TfpgEdit;
    { ISimpleText }
    procedure SetText(const AText: String);
    function GetText: String;
  end;

  { TFPGUIPrivateCheckBox }

  TFPGUIPrivateCheckBox = class(TFPGUIPrivateWidget, ISimpleText)
  private
  protected
  public
    { Constructors / Destructors }
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); override;
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    { Other methods }
    function CheckBox: TfpgCheckBox;
    { ISimpleText }
    procedure SetText(const AText: String);
    function GetText: String;
  end;

  { TFPGUIPrivateRadioButton }

  TFPGUIPrivateRadioButton = class(TFPGUIPrivateWidget, ISimpleText)
  private
  protected
  public
    { Constructors / Destructors }
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); override;
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    { Other methods }
    function RadioButton: TfpgRadioButton;
    { ISimpleText }
    procedure SetText(const AText: String);
    function GetText: String;
  end;

  { TFPGUIPrivatePageControl }

  TFPGUIPrivatePageControl = class(TFPGUIPrivateWidget)
  private
  protected
  public
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;


implementation

uses
  LCLMessageGlue, GfxBase;

{ TFPGUIPrivate }

function TFPGUIPrivate._AddRef: longint; stdcall;
begin
  Result := -1;
end;

function TFPGUIPrivate._Release: longint; stdcall;
begin
  Result := -1;
end;

{ TFPGUIPrivateWidget }

procedure TFPGUIPrivateWidget.SetVisible(const AValue: Boolean);
begin
  Widget.Visible := AValue;
end;

function TFPGUIPrivateWidget.GetVisible: Boolean;
begin
  Result := Widget.Visible;
end;

constructor TFPGUIPrivateWidget.Create(ALCLObject: TWinControl; const AParams: TCreateParams);
begin
  FLCLObject := ALCLObject;

  CreateWidget(AParams);
  
  SetEvents;
end;

destructor TFPGUIPrivateWidget.Destroy;
begin
  FreeAndNil(Widget);

  inherited Destroy;
end;

procedure TFPGUIPrivateWidget.SetEvents;
begin

end;

procedure TFPGUIPrivateWidget.SetSize(AWidth, AHeight: LongInt);
begin
  Widget.SetPosition(Widget.Left, Widget.Top, AWidth, AHeight);
end;

procedure TFPGUIPrivateWidget.SetPosition(AX, AY: Integer);
begin
  Widget.SetPosition(AX, AY, Widget.Width, Widget.Height);
end;

{ TFPGUIPrivateContainer }

constructor TFPGUIPrivateContainer.Create(ALCLObject: TWinControl;
  const AParams: TCreateParams);
begin
  inherited Create(ALCLObject, AParams);
end;

destructor TFPGUIPrivateContainer.Destroy;
begin
  inherited Destroy;
end;

procedure TFPGUIPrivateContainer.AddChild(AWidget: TfpgWidget);
begin
//  fFixed.AddWidget(AWidget, 0, 0);
end;

procedure TFPGUIPrivateContainer.RemoveChild(AWidget: TfpgWidget);
begin
//  fFixed.RemoveChild(AWidget);
end;

{ TFPGUIPrivateWindow }

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.Form
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TFPGUIPrivateWindow.Form: TfpgForm;
begin
  Result := TfpgForm(Widget);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.SetSize
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateWindow.SetSize(AWidth, AHeight: LongInt);
begin
  Form.SetPosition(Form.Top, Form.Left, AWidth, AHeight);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.SetPosition
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateWindow.SetPosition(AX, AY: Integer);
begin
  Form.SetPosition(AX, AY, Form.Width, Form.Height);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.PaintHandler

  Sends a LM_PAINT message to the LCL. This is for windowed controls only
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateWindow.PaintHandler(Sender: TObject);
var
  Msg: TLMPaint;
  AStruct: PPaintStruct;
begin
  {$ifdef VerboseFPGUIPrivate}
    WriteLn('TFPGUIPrivateWindow.PaintHandler');
  {$endif}
  
  if (LCLObject is TWinControl) then
  begin
    FillChar(Msg, SizeOf(Msg), #0);

    Msg.Msg := LM_PAINT;
    New(AStruct);
    FillChar(AStruct^, SizeOf(TPaintStruct), 0);
    Msg.PaintStruct := AStruct;
    Msg.DC := BeginPaint(THandle(Self), AStruct^);

//    Msg.PaintStruct^.rcPaint := PaintData.ClipRect^;
    Msg.PaintStruct^.hdc := Msg.DC;


    // send paint message
    try
      // Saving clip rect and clip region
      try
        LCLObject.WindowProc(TLMessage(Msg));
      finally
        EndPaint(THandle(Self), AStruct^);
        Dispose(AStruct);
      end;
    except
      Application.HandleException(nil);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TFPGUIPrivateWindow.Create(ALCLObject: TWinControl; const AParams: TCreateParams);
begin
  inherited Create(ALCLObject, AParams);

//  Form.InsertChild(fFixed);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.CreateWidget
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateWindow.CreateWidget(const AParams: TCreateParams);
begin
{$IFDEF VerboseFPGUIPrivate}
  WriteLn('[TFPGUIPrivateWindow.CreateWidget]');
{$ENDIF}
  Widget := TfpgForm.Create(nil);
  Form.SetPosition(AParams.X, AParams.Y, AParams.Width, AParams.Height);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.SetEvents
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateWindow.SetEvents;
begin
  inherited SetEvents;

  Form.OnPaint := PaintHandler;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TFPGUIPrivateWindow.Destroy;
begin
{$IFDEF VerboseFPGUIPrivate}
  WriteLn('[TFPGUIPrivateWindow.Destroy]');
{$ENDIF}

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateWindow.SetText(const AText: String);
begin
  Form.WindowTitle := AText;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TFPGUIPrivateWindow.GetText: String;
begin
  Result := Form.WindowTitle;
end;

{ TFPGUIPrivateButton }

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateButton.Clicked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateButton.Clicked(Sender: TObject);
begin
  LCLSendClickedMsg(LCLObject);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateButton.Button
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TFPGUIPrivateButton.Button: TfpgButton;
begin
  Result := TfpgButton(Widget);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateButton.CreateWidget
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateButton.CreateWidget(const AParams: TCreateParams);
var
  ParentContainer: TFPGUIPrivateContainer;
begin
{$IFDEF VerboseFPGUIPrivate}
  WriteLn('[TFPGUIPrivateButton.CreateWidget]');
{$ENDIF}

  ParentContainer := TFPGUIPrivateContainer(LCLObject.Parent.Handle);

  Widget := TfpgButton.Create(ParentContainer.Widget);
  Widget.SetPosition(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateButton.SetEvents
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateButton.SetEvents;
begin
  Button.OnClick := Clicked;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateButton.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TFPGUIPrivateButton.Create(ALCLObject: TWinControl; const AParams: TCreateParams);
begin
  inherited Create(ALCLObject, AParams);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateButton.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateButton.SetText(const AText: String);
begin
  Button.Text := AText;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateButton.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TFPGUIPrivateButton.GetText: String;
begin
  Result := Button.Text;
end;

{ TFPGUIPrivateComboBox }

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateComboBox.ComboBox
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TFPGUIPrivateComboBox.ComboBox: TfpgComboBox;
begin
  Result := TfpgComboBox(Widget);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateComboBox.CreateWidget
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateComboBox.CreateWidget(const AParams: TCreateParams);
var
  ParentContainer: TFPGUIPrivateContainer;
begin
  ParentContainer := TFPGUIPrivateContainer(LCLObject.Parent.Handle);

  Widget := TfpgComboBox.Create(ParentContainer.Widget);
  Widget.SetPosition(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateComboBox.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TFPGUIPrivateComboBox.Create(ALCLObject: TWinControl;
  const AParams: TCreateParams);
begin
  inherited Create(ALCLObject, AParams);
end;

{ TFPGUIPrivateEdit }

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateEdit.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TFPGUIPrivateEdit.Create(ALCLObject: TWinControl;
  const AParams: TCreateParams);
begin
  inherited Create(ALCLObject, AParams);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateEdit.CreateWidget
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateEdit.CreateWidget(const AParams: TCreateParams);
var
  ParentContainer: TFPGUIPrivateContainer;
begin
  ParentContainer := TFPGUIPrivateContainer(LCLObject.Parent.Handle);

  Widget := TfpgEdit.Create(ParentContainer.Widget);
  Widget.SetPosition(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateEdit.Edit
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TFPGUIPrivateEdit.Edit: TfpgEdit;
begin
  Result := TfpgEdit(Widget);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateEdit.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateEdit.SetText(const AText: String);
begin
  Edit.Text := AText;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateEdit.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TFPGUIPrivateEdit.GetText: String;
begin
  Result := Edit.Text;
end;

{ TFPGUIPrivateCheckBox }

function TFPGUIPrivateCheckBox.CheckBox: TfpgCheckBox;
begin
  Result := TfpgCheckBox(Widget);
end;

constructor TFPGUIPrivateCheckBox.Create(ALCLObject: TWinControl;
  const AParams: TCreateParams);
begin
  inherited Create(ALCLObject, AParams);
end;

procedure TFPGUIPrivateCheckBox.CreateWidget(const AParams: TCreateParams);
var
  ParentContainer: TFPGUIPrivateContainer;
begin
  ParentContainer := TFPGUIPrivateContainer(LCLObject.Parent.Handle);

  Widget := TfpgCheckBox.Create(ParentContainer.Widget);
  Widget.SetPosition(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
end;

procedure TFPGUIPrivateCheckBox.SetText(const AText: String);
begin
  CheckBox.Text := AText;
end;

function TFPGUIPrivateCheckBox.GetText: String;
begin
  Result := CheckBox.Text;
end;

{ TFPGUIPrivateRadioButton }

function TFPGUIPrivateRadioButton.RadioButton: TfpgRadioButton;
begin
  Result := TfpgRadioButton(Widget);
end;

constructor TFPGUIPrivateRadioButton.Create(ALCLObject: TWinControl;
  const AParams: TCreateParams);
begin
  inherited Create(ALCLObject, AParams);
end;

procedure TFPGUIPrivateRadioButton.CreateWidget(const AParams: TCreateParams);
var
  ParentContainer: TFPGUIPrivateContainer;
begin
  ParentContainer := TFPGUIPrivateContainer(LCLObject.Parent.Handle);

  Widget := TfpgRadioButton.Create(ParentContainer.Widget);
  Widget.SetPosition(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
end;

procedure TFPGUIPrivateRadioButton.SetText(const AText: String);
begin
  RadioButton.Text := AText;
end;

function TFPGUIPrivateRadioButton.GetText: String;
begin
  Result := RadioButton.Text;
end;

{ TFPGUIPrivateNotebook }

procedure TFPGUIPrivatePageControl.CreateWidget(const AParams: TCreateParams);
var
  ParentContainer: TFPGUIPrivateContainer;
begin
  ParentContainer := TFPGUIPrivateContainer(LCLObject.Parent.Handle);

  Widget := TfpgPageControl.Create(ParentContainer.Widget);
  Widget.SetPosition(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
end;

end.
  
