{ $Id$}
{
 *****************************************************************************
 *                              GtkWSButtons.pp                              * 
 *                              ---------------                              * 
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
unit GtkWSButtons;

{$mode objfpc}{$H+}

interface

uses
  // libs
  GLib, Gtk, 
  // LCL
  Buttons, Classes, LCLType, LMessages,
  // widgetset
  WSButtons, WSLCLClasses, 
  // interface
  GtkDef;

type

  { TGtkWSButton }

  TGtkWSButton = class(TWSButton)
  private
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  public
    class function CreateHandle(const AComponent: TComponent; const AParams: TCreateParams): THandle; override;
  end;

  { TGtkWSBitBtn }

  TGtkWSBitBtn = class(TWSBitBtn)
  private
  protected
  public
  end;

  { TGtkWSSpeedButton }

  TGtkWSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
  end;


implementation

uses
  GtkProc, GtkInt, GtkGlobals,
  GtkWSControls;

{ TGtkWSButton }

function GtkWSButton_Clicked(AWidget: PGtkWidget; AInfo: PWidgetInfo): GBoolean; cdecl;
var
  Msg: TLMessage;
begin
  Result := CallBackDefaultReturn;
  if AInfo^.ChangeLock > 0 then Exit;
  Msg.Msg := LM_CLICKED;
  Result := DeliverMessage(AInfo^.LCLObject, Msg) = 0;
end;


function TGtkWSButton.CreateHandle(const AComponent: TComponent; const AParams: TCreateParams): THandle; 
var
  Caption, Pattern: String;
  AccelKey: Char;
  Button: TButton;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  //TODO: support utf accelkey
  
  Button := AComponent as TButton;

  Caption := Button.Caption;
  LabelFromAmpersands(Caption, Pattern, AccelKey);
  Result := THandle(gtk_button_new_with_label(PChar(Caption)));
  if Result = 0 then Exit;

{$ifdef gtk1}
  gtk_label_set_pattern(PGtkLabel(PGtkButton(Result)^.Child), PChar(Pattern));
{$endif gtk1}
  Accelerate(Button, PGtkWidget(Result), Ord(AccelKey), 0, 'clicked');
  
  WidgetInfo := CreateWidgetInfo(Result, Button, AParams);
  WidgetInfo^.CoreWidget := PGtkWidget(Result);
  
  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(PGtkWidget(Result), @Allocation);

  SetCallbacks(PGtkWidget(Result), WidgetInfo);
end;

procedure TGtkWSButton.SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin        
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));

  SignalConnect(AGtkWidget, 'clicked', @GtkWSButton_Clicked, AWidgetInfo);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TButton, TGtkWSButton);
  RegisterWSComponent(TBitBtn, TGtkWSBitBtn); // register it to fallback to default
//  RegisterWSComponent(TSpeedButton, TGtkWSSpeedButton);
////////////////////////////////////////////////////
end.
