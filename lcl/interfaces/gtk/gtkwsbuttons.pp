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
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Buttons,
////////////////////////////////////////////////////
  Classes, LCLType, LMessages,
  WSButtons, WSLCLClasses;

type

  { TGtkWSButton }

  TGtkWSButton = class(TWSButton)
  private
  protected
  public
    class function CreateHandle(const AComponent: TComponent; 
                                const AParams: TCreateParams): THandle; override;
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
  Gtk, GtkProc, GtkDef, GtkInt;

{ TGtkWSButton }      

function TGtkWSButton.CreateHandle(const AComponent: TComponent; 
  const AParams: TCreateParams): THandle; 
var
  Caption, Pattern: String;
  AccelKey: Char;
  Button: TButton;
  WidgetInfo: PWinWidgetInfo;
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
  WidgetInfo^.ImplementationWidget := PGtkWidget(Result);
  
  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(PGtkWidget(Result), @Allocation);

  GtkWidgetSet.HookWincontrolSignals(PGTKObject(Result), Button);
  GtkWidgetSet.SetCallback(LM_CLICKED, PGTKObject(Result), Button);
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
