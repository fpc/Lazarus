{ $Id: FPGUIwsprivate.pp 10697 2007-02-27 23:17:33Z marc $ }
{
                 ------------------------------------------
                 FPGUIwsprivate.pp  -  FPGUI internal classes
                 ------------------------------------------

 @created(Thu Feb 1st WET 2007)
 @lastmod($Date: 2007-02-27 18:17:33 -0500 (Tue, 27 Feb 2007) $)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains the private classhierarchy for the fpgui implemetation
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
  //fFixed.RemoveChild(TFPGUIPrivateWidget(AControl.Handle).Widget);
  // !!
end;

{ TFPGUIPrivateWindow }

function TFPGUIPrivateWindow.Form: TFForm;
begin
  Result := TFForm(Widget);
end;

procedure TFPGUIPrivateWindow.SetSize(AWidth, AHeight: LongInt);
begin
  Form.Wnd.SetSize(Size(AWidth, AHeight));
end;

procedure TFPGUIPrivateWindow.SetPosition(AX, AY: Integer);
begin
  Form.Wnd.SetPosition(Point(AX, AY));
end;

constructor TFPGUIPrivateWindow.Create(ALCLObject: TWinControl; const AParams: TCreateParams);
begin
  inherited Create(ALCLObject, AParams);

  Form.InsertChild(fFixed);
end;

procedure TFPGUIPrivateWindow.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TFForm.Create(LCLObject);
  Form.Wnd.SetSize(Size(AParams.Width, AParams.Height));
  Form.Wnd.SetPosition(Point(AParams.X, AParams.Y));
end;

destructor TFPGUIPrivateWindow.Destroy;
begin
  inherited Destroy;
end;

procedure TFPGUIPrivateWindow.SetText(const AText: String);
begin
  Form.Wnd.Title := AText;
end;

function TFPGUIPrivateWindow.GetText: String;
begin
  Result := Form.Wnd.Title;
end;

{ TFPGUIPrivateButton }

procedure TFPGUIPrivateButton.Clicked(Sender: TObject);
begin
  LCLSendClickedMsg(TControl(LCLObject));
end;

function TFPGUIPrivateButton.Button: TFButton;
begin
  Result := TFButton(Widget);
end;

procedure TFPGUIPrivateButton.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TFButton.Create(TFPGUIPrivateWidget(LCLObject.Parent.Handle).Widget);
  
  TFPGUIPrivateContainer(LCLObject.Parent.Handle).AddChild(Widget);
  Widget.SetBounds(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
end;

constructor TFPGUIPrivateButton.Create(ALCLObject: TWinControl; const AParams: TCreateParams);
begin
  inherited Create(ALCLObject, AParams);
  
  // Events
  Button.OnClick := @Clicked;
end;

procedure TFPGUIPrivateButton.SetText(const AText: String);
begin
  Button.Text := AText;
end;

function TFPGUIPrivateButton.GetText: String;
begin
  Result := Button.Text;
end;

end.
  
