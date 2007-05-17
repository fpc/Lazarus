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

{$mode objfpc}{$H+}

interface

uses
  // LCL
  LCLType, LMessages, LCLProc, Controls, Classes, SysUtils, Forms,
  // widgetset
  WSControls, WSLCLClasses, WSProc,
  // interface
  fpgui, fpgfx;


type

  IContainer = interface(IInterface)
    procedure AddChild(AWidget: TFWidget);
    procedure RemoveChild(AWidget: TFWidget);
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
    FWidget: TFWidget;
    FLCLObject: TWinControl;
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);
  protected
  public
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); virtual;
    destructor Destroy; override;
    procedure CreateWidget(const AParams: TCreateParams); virtual; abstract;
    procedure DestroyWidget; virtual; abstract;
    procedure SetSize(AWidth, AHeight: LongInt); virtual;
    procedure SetPosition(AX, AY: Integer); virtual;

    property LCLObject: TWinControl read FLCLObject;
    property Visible: Boolean read GetVisible write SetVisible;
    property Widget: TFWidget read FWidget write FWidget;
  end;


  { TFPGUIPrivateContainer }
  { Private class for containers }

  TFPGUIPrivateContainer = class(TFPGUIPrivateWidget, IContainer)
  private
  protected
    fFixed: TFFixedLayout;
  public
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
  // IContainer
    procedure AddChild(AWidget: TFWidget);
    procedure RemoveChild(AWidget: TFWidget);
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
  protected
  public
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); override;
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    destructor Destroy; override;
    function Form: TFForm;
    procedure SetSize(AWidth, AHeight: LongInt); override;
    procedure SetPosition(AX, AY: Integer); override;
  // ISimpleText
    procedure SetText(const AText: String);
    function GetText: String;
  end;


  { TFPGUIPrivateDialog }
  { Private class for dialogs }

  TFPGUIPrivateDialog = class(TFWidget) // is there a dialog widget?
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
    function Button: TFButton;
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); override;
    procedure CreateWidget(const AParams: TCreateParams); override;
    // ISimpleText
    procedure SetText(const AText: String);
    function GetText: String;
  end;

  { TFPGUIPrivateComboBox }

  TFPGUIPrivateComboBox = class(TFPGUIPrivateWidget)
  private
  protected
  public
    function ComboBox: TFComboBox;
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;


  { TFPGUIPrivateEdit }

  TFPGUIPrivateEdit = class(TFPGUIPrivateWidget, ISimpleText)
  private
  protected
  public
    function Edit: TFEdit;
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); override;
    procedure CreateWidget(const AParams: TCreateParams); override;
    // ISimpleText
    procedure SetText(const AText: String);
    function GetText: String;
  end;

  { TFPGUIPrivateCheckBox }

  TFPGUIPrivateCheckBox = class(TFPGUIPrivateWidget, ISimpleText)
  private
  protected
  public
    function CheckBox: TFCheckBox;
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); override;
    procedure CreateWidget(const AParams: TCreateParams); override;
    // ISimpleText
    procedure SetText(const AText: String);
    function GetText: String;
  end;

  { TFPGUIPrivateRadioButton }

  TFPGUIPrivateRadioButton = class(TFPGUIPrivateWidget, ISimpleText)
  private
  protected
  public
    function RadioButton: TFRadioButton;
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); override;
    procedure CreateWidget(const AParams: TCreateParams); override;
    // ISimpleText
    procedure SetText(const AText: String);
    function GetText: String;
  end;

  {TFPGUIPrivateNotebook = class(TPrivateNotebook)
  private
  protected
  public
  end;}


implementation

uses LCLMessageGlue, GfxBase;

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
end;

destructor TFPGUIPrivateWidget.Destroy;
begin
  FreeAndNil(Widget);

  inherited Destroy;
end;

procedure TFPGUIPrivateWidget.SetSize(AWidth, AHeight: LongInt);
begin
  Widget.SetBounds(Widget.Left, Widget.Top, AWidth, AHeight);
end;

procedure TFPGUIPrivateWidget.SetPosition(AX, AY: Integer);
begin
  Widget.SetBounds(AX, AY, Widget.Width, Widget.Height);
end;

{ TFPGUIPrivateContainer }

constructor TFPGUIPrivateContainer.Create(ALCLObject: TWinControl;
  const AParams: TCreateParams);
begin
  inherited Create(ALCLObject, AParams);

  FFixed := TFFixedLayout.Create(Widget);
end;

destructor TFPGUIPrivateContainer.Destroy;
begin
  FFixed.Free;

  inherited Destroy;
end;

procedure TFPGUIPrivateContainer.AddChild(AWidget: TFWidget);
begin
  fFixed.AddWidget(AWidget, 0, 0);
end;

procedure TFPGUIPrivateContainer.RemoveChild(AWidget: TFWidget);
begin
//  fFixed.RemoveChild(AWidget);
end;

{ TFPGUIPrivateWindow }

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.Form
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TFPGUIPrivateWindow.Form: TFForm;
begin
  Result := TFForm(Widget);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.SetSize
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateWindow.SetSize(AWidth, AHeight: LongInt);
begin
  Form.Wnd.SetSize(Size(AWidth, AHeight));
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.SetPosition
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateWindow.SetPosition(AX, AY: Integer);
begin
  Form.Wnd.SetPosition(Point(AX, AY));
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TFPGUIPrivateWindow.Create(ALCLObject: TWinControl; const AParams: TCreateParams);
begin
  inherited Create(ALCLObject, AParams);

  Form.InsertChild(fFixed);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.CreateWidget
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateWindow.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TFForm.Create(nil);

  Form.Wnd.SetSize(Size(AParams.Width, AParams.Height));

  Form.Wnd.SetPosition(Point(AParams.X, AParams.Y));
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.DestroyWidget
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateWindow.DestroyWidget;
begin
  Form.Free;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TFPGUIPrivateWindow.Destroy;
begin
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateWindow.SetText(const AText: String);
begin
  Form.Wnd.Title := AText;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TFPGUIPrivateWindow.GetText: String;
begin
  Result := Form.Wnd.Title;
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
function TFPGUIPrivateButton.Button: TFButton;
begin
  Result := TFButton(Widget);
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
  ParentContainer := TFPGUIPrivateContainer(LCLObject.Parent.Handle);

  Widget := TFButton.Create(ParentContainer.Widget);

  ParentContainer.AddChild(Widget);

  Widget.SetBounds(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateButton.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TFPGUIPrivateButton.Create(ALCLObject: TWinControl; const AParams: TCreateParams);
begin
  inherited Create(ALCLObject, AParams);

  // Events
  Button.OnClick := @Clicked;
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
function TFPGUIPrivateComboBox.ComboBox: TFComboBox;
begin
  Result := TFComboBox(Widget);
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

  Widget := TFComboBox.Create(ParentContainer.Widget);

  ParentContainer.AddChild(Widget);

  Widget.SetBounds(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
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

  // Events
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateComboBox.DestroyWidget
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateComboBox.DestroyWidget;
begin
{
var
  ParentContainer: TFPGUIPrivateContainer;
begin
  ParentContainer := TFPGUIPrivateContainer(LCLObject.Parent.Handle);

  ParentContainer.RemoveChild(Widget);

  Widget.Free;}
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

  // Events
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

  Widget := TFEdit.Create(ParentContainer.Widget);

  ParentContainer.AddChild(Widget);

  Widget.SetBounds(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateEdit.Edit
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TFPGUIPrivateEdit.Edit: TFEdit;
begin
  Result := TFEdit(Widget);
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

function TFPGUIPrivateCheckBox.CheckBox: TFCheckBox;
begin
  Result := TFCheckBox(Widget);
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

  Widget := TFCheckBox.Create(ParentContainer.Widget);

  ParentContainer.AddChild(Widget);

  Widget.SetBounds(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
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

function TFPGUIPrivateRadioButton.RadioButton: TFRadioButton;
begin
  Result := TFRadioButton(Widget);
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

  Widget := TFRadioButton.Create(ParentContainer.Widget);

  ParentContainer.AddChild(Widget);

  Widget.SetBounds(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
end;

procedure TFPGUIPrivateRadioButton.SetText(const AText: String);
begin
  RadioButton.Text := AText;
end;

function TFPGUIPrivateRadioButton.GetText: String;
begin
  Result := RadioButton.Text;
end;

end.
  
