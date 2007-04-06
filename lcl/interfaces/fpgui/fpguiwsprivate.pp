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
    procedure AddChild(AControl: TWinControl);
    procedure RemoveChild(AControl: TWinControl);
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
    fWidget: TWidget;
    fLCLObject: TControl;
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);
  protected
  public
    constructor Create(ALCLObject: TControl; const AParams: TCreateParams); virtual;
    destructor Destroy; override;
    procedure CreateWidget(const AParams: TCreateParams); virtual; abstract;
    procedure SetSize(AWidth, AHeight: LongInt); virtual;
    procedure SetPosition(AX, AY: Integer); virtual;

    property LCLObject: TControl read fLCLObject;
    property Visible: Boolean read GetVisible write SetVisible;
    property Widget: TWidget read fWidget write fWidget;
  end;
  
  
  { TFPGUIPrivateContainer }
  { Private class for containers }

  TFPGUIPrivateContainer = class(TFPGUIPrivateWidget)
  private
  protected
  public
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

  TFPGUIPrivateWindow = class(TFPGUIPrivateBin, IContainer, ISimpleText)
  private
    fFixed: TFixedLayout;
  protected
  public
    constructor Create(ALCLObject: TControl; const AParams: TCreateParams); override;
    procedure CreateWidget(const AParams: TCreateParams); override;
    destructor Destroy; override;
    function Form: TForm;
    procedure SetSize(AWidth, AHeight: LongInt); override;
    procedure SetPosition(AX, AY: Integer); override;
  // IContainer
    procedure AddChild(AControl: TWinControl);
    procedure RemoveChild(AControl: TWinControl);
  // ISimpleText
    procedure SetText(const AText: String);
    function GetText: String;
  end;


  { TFPGUIPrivateDialog }
  { Private class for dialogs }

  TFPGUIPrivateDialog = class(fpgui.TWidget) // is there a dialog widget?
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
    function Button: TButton;
    constructor Create(ALCLObject: TControl; const AParams: TCreateParams); override;
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

{ TFPGUIPrivateWindow }

function TFPGUIPrivateWindow.Form: fpgui.TForm;
begin
  Result := fpgui.TForm(Widget);
end;

procedure TFPGUIPrivateWindow.SetSize(AWidth, AHeight: LongInt);
begin
  Form.Wnd.SetSize(Size(AWidth, AHeight));
end;

procedure TFPGUIPrivateWindow.SetPosition(AX, AY: Integer);
begin
  Form.Wnd.SetPosition(Point(AX, AY));
end;

procedure TFPGUIPrivateWindow.AddChild(AControl: TWinControl);
var
 AWidget: TWidget;
begin
  AWidget := TFPGUIPrivateWidget(AControl.Handle).Widget;
  fFixed.AddWidget(AWidget, AControl.Left, AControl.Top);
  AWidget.SetBounds(AControl.Left, AControl.Top, AControl.Width, AControl.Height);
end;

procedure TFPGUIPrivateWindow.RemoveChild(AControl: TWinControl);
begin
  //fFixed.RemoveChild(TFPGUIPrivateWidget(AControl.Handle).Widget);
  // !!
end;

constructor TFPGUIPrivateWindow.Create(ALCLObject: TControl; const AParams: TCreateParams);
begin
  inherited Create(ALCLObject, AParams);
  fFixed := TFixedLayout.Create(Widget);
  Form.InsertChild(fFixed);
end;

procedure TFPGUIPrivateWindow.CreateWidget(const AParams: TCreateParams);
begin
  Widget := fpgui.TForm.Create(LCLObject);
  Form.Wnd.SetSize(Size(AParams.Width, AParams.Height));
  Form.Wnd.SetPosition(Point(AParams.X, AParams.Y));
end;

destructor TFPGUIPrivateWindow.Destroy;
begin
  fFixed.Free;
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

function TFPGUIPrivateButton.Button: TButton;
begin
  Result := fpgui.TButton(Widget);
end;

procedure TFPGUIPrivateButton.CreateWidget(const AParams: TCreateParams);
begin
  Widget := fpgui.TButton.Create(LCLObject);
end;

constructor TFPGUIPrivateButton.Create(ALCLObject: TControl; const AParams: TCreateParams);
begin
  inherited Create(ALCLObject, AParams);
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

{ TFPGUIPrivateWidget }

procedure TFPGUIPrivateWidget.SetVisible(const AValue: Boolean);
begin
  Widget.Visible := AValue;
end;

function TFPGUIPrivateWidget.GetVisible: Boolean;
begin
  Result := Widget.Visible;
end;

constructor TFPGUIPrivateWidget.Create(ALCLObject: TControl; const AParams: TCreateParams);
begin
  fLCLObject := ALCLObject;
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

{ TFPGUIPrivate }

function TFPGUIPrivate._AddRef: longint; stdcall;
begin
  Result := -1;
end;

function TFPGUIPrivate._Release: longint; stdcall;
begin
  Result := -1;
end;

end.
  
