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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  {$IFDEF GTK2} Gtk2, Glib2, gdk2, {$ELSE} Gtk, gdk, Glib, {$ENDIF}
  SysUtils, Classes, Controls, LMessages, InterfaceBase, graphics,
  Dialogs, WSDialogs, WSLCLClasses, gtkint, gtkproc, gtkwscontrols;

type

  { TGtkWSCommonDialog }

  TGtkWSCommonDialog = class(TWSCommonDialog)
  private
  protected
  public
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
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
  DebugLn('TGtkWidgetSet.SetColorDialogColor Start Color=',HexStr(Cardinal(Color),8));
  {$ENDIF}
  Color:=ColorToRGB(Color);
  {$IFDEF VerboseColorDialog}
  DebugLn('TGtkWidgetSet.SetColorDialogColor Converted Color=',HexStr(Cardinal(Color),8));
  {$ENDIF}
  SelectionColor.Pixel := 0;
  SelectionColor.Red :=  Red(Color) shl 8;
  SelectionColor.Green:= Green(Color) shl 8;
  SelectionColor.Blue:= Blue(Color) shl 8;
  colorSel := PGTKCOLORSELECTION((PGTKCOLORSELECTIONDIALOG(ColorSelection))^.colorsel);
  gtk_color_selection_set_current_color(colorSel,@SelectionColor);
end;

procedure TGtkWSCommonDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  GtkWindow: PGtkWindow;
begin    
  ReleaseMouseCapture;
  GtkWindow:=PGtkWindow(ACommonDialog.Handle);
  gtk_window_set_title(GtkWindow,PChar(ACommonDialog.Title));
  if ACommonDialog is TColorDialog then
    SetColorDialogColor(PGtkColorSelection(GtkWindow),
                        TColorDialog(ACommonDialog).Color);
  gtk_window_set_position(GtkWindow, GTK_WIN_POS_CENTER);
  GtkWindowShowModal(GtkWindow);
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
