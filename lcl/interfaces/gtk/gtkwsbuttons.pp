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
  {$IFDEF GTK2}
  GLib2, Gtk2,
  {$ELSE}
  GLib, Gtk, 
  {$ENDIF}
  // LCL
  Buttons, Classes, LCLType, LMessages, Controls,
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
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TGtkWSBitBtn }

  TGtkWSBitBtn = class(TWSBitBtn)
  private
  protected
  public
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TGtkWSSpeedButton }

  TGtkWSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
  end;


implementation

uses
  SysUtils, 
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
  Button: TButton;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  //TODO: support utf accelkey
  
  Button := AComponent as TButton;

  Result := THandle(gtk_button_new_with_label('button'));
  if Result = 0 then Exit;

  WidgetInfo := CreateWidgetInfo(Result, Button, AParams);
  WidgetInfo^.CoreWidget := PGtkWidget(Result);
  
  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(PGtkWidget(Result), @Allocation);

  SetCallbacks(PGtkWidget(Result), WidgetInfo);
end;

function TGtkWSButton.GetText(const AWinControl: TWinControl; var AText: String): Boolean; 
begin             
  // The button text is static, so let the LCL fallback to FCaption
  Result := False;
end;

procedure TGtkWSButton.SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin        
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));

  SignalConnect(AGtkWidget, 'clicked', @GtkWSButton_Clicked, AWidgetInfo);
end;

procedure TGtkWSButton.SetText(const AWinControl: TWinControl; const AText: String); 
var
  BtnWidget: PGtkButton;
  LblWidget: PGtkLabel;
begin          
  if not AWinControl.HandleAllocated
  then begin
    Assert(False, Format('trace: [WARNING] SetText called without handle for %s(%s)', [AWinControl.Name, AWinControl.ClassName]));
    Exit;
  end;

  BtnWidget := PGtkButton(AWinControl.Handle);
  {$IFDEF GTK2}
  LblWidget := PGtkLabel(PGtkBin(BtnWidget)^.Child);
  {$ELSE}
  LblWidget := PGtkLabel(BtnWidget^.Child);
  {$ENDIF}

  if LblWidget = nil
  then begin
    Assert(False, Format('trace: [WARNING] Button %s(%s) has no label', [AWinControl.Name, AWinControl.ClassName]));
    LblWidget := PGtkLabel(gtk_label_new(''));
    gtk_container_add(PGtkContainer(BtnWidget), PGtkWidget(LblWidget));
  end;
  
  GtkWidgetSet.SetLabelCaption(LblWidget, AText, AWinControl, PGtkWidget(BtnWidget), 'clicked');   
end;

{ TGtkWSBitBtn }

function TGtkWSBitBtn.GetText(const AWinControl: TWinControl; var AText: String): Boolean; 
begin             
  // The button text is static, so let the LCL fallback to FCaption
  Result := False;
end;

procedure TGtkWSBitBtn.SetText(const AWinControl: TWinControl; const AText: String); 
var
  BtnWidget: PGtkButton;
  LblWidget: PGtkLabel;
begin          
  if not AWinControl.HandleAllocated
  then begin
    Assert(False, Format('trace: [WARNING] SetText called without handle for %s(%s)', [AWinControl.Name, AWinControl.ClassName]));
    Exit;
  end;

  BtnWidget := PGtkButton(AWinControl.Handle);
  LblWidget := PGtkLabel(gtk_object_get_data(PGtkObject(BtnWidget),'Label'));
  if LblWidget = nil then Exit;  

  GtkWidgetSet.SetLabelCaption(LblWidget, AText, AWinControl, PGtkWidget(BtnWidget), 'clicked');   
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
