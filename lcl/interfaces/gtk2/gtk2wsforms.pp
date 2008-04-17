{ $Id$}
{
 *****************************************************************************
 *                              Gtk2WSForms.pp                               * 
 *                              --------------                               * 
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
unit Gtk2WSForms;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  Forms,
////////////////////////////////////////////////////
  WSForms, WSLCLClasses, Forms, GTKProc;

type

  { TGtk2WSScrollingWinControl }

  TGtk2WSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
  end;

  { TGtk2WSScrollBox }

  TGtk2WSScrollBox = class(TWSScrollBox)
  private
  protected
  public
  end;

  { TGtk2WSCustomFrame }

  TGtk2WSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TGtk2WSFrame }

  TGtk2WSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TGtk2WSCustomForm }

  TGtk2WSCustomForm = class(TWSCustomForm)
  private
  protected
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); override;
  public
    class function GetDefaultClientRect(const AWinControl: TWinControl;
             const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect
             ): boolean; override;
    class procedure SetBorderIcons(const AForm: TCustomForm;
                                   const ABorderIcons: TBorderIcons); override;
  end;

  { TGtk2WSForm }

  TGtk2WSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TGtk2WSHintWindow }

  TGtk2WSHintWindow = class(TWSHintWindow)
  private
  protected
  public
  end;

  { TGtk2WSScreen }

  TGtk2WSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TGtk2WSApplicationProperties }

  TGtk2WSApplicationProperties = class(TWSApplicationProperties)
  private
  protected
  public
  end;


implementation

{ TGtk2WSCustomForm }

class procedure TGtk2WSCustomForm.SetCallbacks(const AWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AWidget), TComponent(AWidgetInfo^.LCLObject));
  if (TControl(AWidgetInfo^.LCLObject).Parent = nil) then
    with TGTKWidgetSet(Widgetset) do
    begin
      SetCallback(LM_CONFIGUREEVENT, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
      SetCallback(LM_CLOSEQUERY, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
      SetCallBack(LM_Activate, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
      SetCallback(LM_HSCROLL, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
      SetCallback(LM_VSCROLL, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
    end;

  g_signal_connect(PGtkObject(AWidgetInfo^.CoreWidget), 'window-state-event',
    gtk_signal_func(@GTKWindowStateEventCB), AWidgetInfo^.LCLObject);
end;

class function TGtk2WSCustomForm.GetDefaultClientRect(
  const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer;
  var aClientRect: TRect): boolean;
begin
  Result:=false;
  if AWinControl.HandleAllocated then begin

  end else begin
    FrameBorders:=GetStyleFormFrameBorders(TCustomForm(AWinControl).Menu<>nil);
    aClientRect:=Rect(0,0,
                 Max(0,aWidth-FrameBorders.Left-FrameBorders.Right),
                 Max(0,aHeight-FrameBorders.Top-FrameBorders.Bottom));
    Result:=true;
  end;
end;

class procedure TGtk2WSCustomForm.SetBorderIcons(const AForm: TCustomForm;
  const ABorderIcons: TBorderIcons);
begin
  gtk_window_get_modal();
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollingWinControl, TGtk2WSScrollingWinControl);
//  RegisterWSComponent(TScrollBox, TGtk2WSScrollBox);
//  RegisterWSComponent(TCustomFrame, TGtk2WSCustomFrame);
//  RegisterWSComponent(TFrame, TGtk2WSFrame);
  RegisterWSComponent(TCustomForm, TGtk2WSCustomForm);
//  RegisterWSComponent(TForm, TGtk2WSForm);
//  RegisterWSComponent(THintWindow, TGtk2WSHintWindow);
//  RegisterWSComponent(TScreen, TGtk2WSScreen);
//  RegisterWSComponent(TApplicationProperties, TGtk2WSApplicationProperties);
////////////////////////////////////////////////////
end.
