{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSControls.pp                             * 
 *                             -----------------                             * 
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
unit Gtk2WSControls;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Controls,
////////////////////////////////////////////////////
  Gtk2, Gdk2, Glib2, GtkGlobals,
  GtkWsControls,
  gtkProc, LCLType,
  WSControls, WSLCLClasses, WSProc;
  

type

  { TGtk2WSDragImageList }

  TGtk2WSDragImageList = class(TWSDragImageList)
  private
  protected
  public
  end;

  { TGtk2WSControl }

  TGtk2WSControl = class(TWSControl)
  private
  protected
  public
  end;

  { TGtk2WSWinControl }

  TGtk2WSWinControl = class(TGtkWSWinControl)
  private
  protected
  public
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
  end;

  { TGtk2WSGraphicControl }

  TGtk2WSGraphicControl = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TGtk2WSCustomControl }

  TGtk2WSCustomControl = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TGtk2WSImageList }

  TGtk2WSImageList = class(TWSImageList)
  private
  protected
  public
  end;


implementation

{ TGtk2WSWinControl }

class function TGtk2WSWinControl.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  TextBuf: PGtkTextBuffer;
  StartIter,
  EndIter: TGtkTextIter;
  CS: PChar;
  Handle: HWND;
begin
  Result := true;
  Handle := AWinControl.Handle;
  case AWinControl.fCompStyle of
   //csComboBox:
   //  begin
   //    AText := StrPas(gtk_entry_get_text(PGtkEntry(PGtkCombo(Handle)^.entry)));
   //  end;

   //csEdit, csSpinEdit:
   //    AText:= StrPas(gtk_entry_get_text(PgtkEntry(Handle)));

   csMemo    : begin
                  TextBuf := gtk_text_view_get_buffer(PGtkTextView(GetWidgetInfo(Pointer(Handle), True)^.CoreWidget));
                  gtk_text_buffer_get_start_iter(TextBuf, @StartIter);
                  gtk_text_buffer_get_end_iter(TextBuf, @EndIter);
                  CS := gtk_text_buffer_get_text(TextBuf, @StartIter, @EndIter, False);
                  AText := StrPas(CS);
                  g_free(CS);
               end;
  else
    Result := TGtkWSWinControl{(ClassParent)}.GetText(AWinControl, AText);
  end;
end;

class procedure TGtk2WSWinControl.SetText(const AWinControl: TWinControl;
  const AText: string);
var
P : Pointer;
TextBuf: PGtkTextBuffer;
StartIter: TGtkTextIter;
pLabel: pchar;
begin
  P := Pointer(AWinControl.Handle);
  
  pLabel := pchar(AText);
  
  case AWinControl.fCompStyle of
    csMemo        : begin
                    TextBuf := gtk_text_view_get_buffer(PGtkTextView(GetWidgetInfo(P, True)^.CoreWidget));
                    gtk_text_buffer_set_text(TextBuf, plabel, -1);
                    gtk_text_buffer_get_start_iter(TextBuf, @StartIter);
                    gtk_text_buffer_place_cursor(TextBuf, @StartIter);
                    //debugln('TGtkWSWinControl.SetText A ',dbgs(gtk_text_get_length(PGtkText(P))),' AText="',AText,'"');
                    //gtk_text_freeze(PGtkText(P));
                    //gtk_text_set_point(PGtkText(P), 0);
                    //gtk_text_forward_delete(PGtkText(P), gtk_text_get_length(PGtkText(P)));
                    //gtk_text_insert(PGtkText(P), nil, nil, nil, pLabel, -1);
                    //gtk_text_thaw(PGtkText(P));
                    //debugln('TGtkWSWinControl.SetText B ',dbgs(gtk_text_get_length(PGtkText(P))));
                  end;
  else
    TGtkWSWinControl{(ClassParent)}.SetText(AWinControl, AText);
  end;
end;

class procedure TGtk2WSWinControl.SetBorderStyle(
  const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
var
  Widget: PGtkWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBorderStyle')
  then Exit;
  
  Widget := PGtkWidget(AWinControl.Handle);
  if GTK_IS_SCROLLED_WINDOW(Widget) then
    gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(Widget), BorderStyleShadowMap[ABorderStyle])
  else
    TGtkWSWinControl{(ClassParent)}.SetBorderStyle(AWinControl, ABorderStyle);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TDragImageList, TGtk2WSDragImageList);
//  RegisterWSComponent(TControl, TGtk2WSControl);
  RegisterWSComponent(TWinControl, TGtk2WSWinControl);
//  RegisterWSComponent(TGraphicControl, TGtk2WSGraphicControl);
//  RegisterWSComponent(TCustomControl, TGtk2WSCustomControl);
//  RegisterWSComponent(TImageList, TGtk2WSImageList);
////////////////////////////////////////////////////
end.
