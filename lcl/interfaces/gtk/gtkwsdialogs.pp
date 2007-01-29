{ $Id$}
{
 *****************************************************************************
 *                              GtkWSDialogs.pp                              * 
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
unit GtkWSDialogs;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef gtk2}
  Gtk2, Glib2, gdk2,
  {$else}
  Gtk, gdk, Glib,
  {$endif}
  SysUtils, Classes, Controls, LMessages, InterfaceBase, graphics,
  LCLType, Dialogs, WSDialogs, WSLCLClasses,
  gtkint, gtkproc, gtkwscontrols, gtkExtra;

type

  { TGtkWSCommonDialog }

  TGtkWSCommonDialog = class(TWSCommonDialog)
  private
  protected
  public
    class function  CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
  end;

  { TGtkWSFileDialog }

  TGtkWSFileDialog = class(TWSFileDialog)
  private
  protected
  public
  end;

  { TGtkWSOpenDialog }

  TGtkWSOpenDialog = class(TWSOpenDialog)
  private
  protected
  public
  end;

  { TGtkWSSaveDialog }

  TGtkWSSaveDialog = class(TWSSaveDialog)
  private
  protected
  public
  end;

  { TGtkWSSelectDirectoryDialog }

  TGtkWSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  private
  protected
  public
  end;

  { TGtkWSColorDialog }

  TGtkWSColorDialog = class(TWSColorDialog)
  private
  protected
  public
  end;

  { TGtkWSColorButton }

  TGtkWSColorButton = class(TWSColorButton)
  private
  protected
  public
  end;

  { TGtkWSFontDialog }

  TGtkWSFontDialog = class(TWSFontDialog)
  private
  protected
  public
  end;


implementation
    
{------------------------------------------------------------------------------
  Method: SetColorDialogColor
  Params:  ColorSelection : a gtk color selection dialog;
           Color          : the color to select
  Returns: nothing

  Set the color of the color selection dialog
 ------------------------------------------------------------------------------}
procedure SetColorDialogColor(ColorSelection: PGtkColorSelection;
  Color: TColor);
var
  SelectionColor: TGDKColor;
  colorSel : PGTKCOLORSELECTION;
begin
  {$IFDEF VerboseColorDialog}
  DebugLn('TGtkWidgetSet.SetColorDialogColor Start Color=',DbgS(Color));
  {$ENDIF}
  Color:=ColorToRGB(Color);
  {$IFDEF VerboseColorDialog}
  DebugLn('TGtkWidgetSet.SetColorDialogColor Converted Color=',DbgS(Color));
  {$ENDIF}
  SelectionColor.Pixel := 0;
  SelectionColor.Red :=  Red(Color) shl 8;
  SelectionColor.Green:= Green(Color) shl 8;
  SelectionColor.Blue:= Blue(Color) shl 8;
  colorSel := PGTKCOLORSELECTION((PGTKCOLORSELECTIONDIALOG(ColorSelection))^.colorsel);
  gtk_color_selection_set_current_color(colorSel,@SelectionColor);
end;

class procedure TGtkWSCommonDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  GtkWindow: PGtkWindow;
  {$IFDEF Gtk1}
  {$IFDEF UseXinerama}
  Requisition: TGtkRequisition;
  {$ENDIF}
  {$ENDIF}
begin
  ReleaseMouseCapture;
  GtkWindow:=PGtkWindow(ACommonDialog.Handle);
  gtk_window_set_title(GtkWindow,PChar(ACommonDialog.Title));
  if ACommonDialog is TColorDialog then
    SetColorDialogColor(PGtkColorSelection(GtkWindow),
                        TColorDialog(ACommonDialog).Color);
  {$IFDEF Gtk1}
  {$IFDEF UseXinerama}
  if GetFirstScreen then begin
    { Fix multi screen problems, at least partially by forcing dialog to centre of first screen }
    gtk_widget_size_request(PGtkWidget(GtkWindow), @Requisition);
    gtk_widget_set_uposition(PGtkWidget(GtkWindow), (FirstScreen.x - Requisition.width) div 2,
                                                    (FirstScreen.y - Requisition.height) div 2);
  end else
  {$ENDIF}
  {$ENDIF}
  gtk_window_set_position(GtkWindow, GTK_WIN_POS_CENTER);
  GtkWindowShowModal(GtkWindow);
end;

class function  TGtkWSCommonDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
begin
  { TODO: cleanup }
  Result := TGtkWidgetSet(WidgetSet).CreateComponent(ACommonDialog);
end;

class procedure TGtkWSCommonDialog.DestroyHandle(const ACommonDialog: TCommonDialog);
begin
  { TODO: cleanup }
  TGtkWidgetSet(WidgetSet).DestroyLCLComponent(ACommonDialog);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCommonDialog, TGtkWSCommonDialog);
//  RegisterWSComponent(TFileDialog, TGtkWSFileDialog);
//  RegisterWSComponent(TOpenDialog, TGtkWSOpenDialog);
//  RegisterWSComponent(TSaveDialog, TGtkWSSaveDialog);
//  RegisterWSComponent(TSelectDirectoryDialog, TGtkWSSelectDirectoryDialog);
//  RegisterWSComponent(TColorDialog, TGtkWSColorDialog);
//  RegisterWSComponent(TColorButton, TGtkWSColorButton);
//  RegisterWSComponent(TFontDialog, TGtkWSFontDialog);
////////////////////////////////////////////////////
end.
