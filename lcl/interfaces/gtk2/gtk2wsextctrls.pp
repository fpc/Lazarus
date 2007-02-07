{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSExtCtrls.pp                             * 
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
unit Gtk2WSExtCtrls;

{$mode objfpc}{$H+}

interface

uses
// libs
  GLib2, Gtk2, Gdk2, Gtk2Int, gtkProc, GtkDef,
  // LCL
  ExtCtrls, Classes, Controls, LCLType,
  // widgetset
  WSExtCtrls, WSLCLClasses, WSProc,
  GtkWSExtCtrls, gtk2private;

type

  { TGtk2WSCustomPage }

  {TGtk2WSCustomPage = class(TWSCustomPage)
  private
  protected
  public
  end;}

  { TGtk2WSCustomNotebook }
  
  TGtk2WSCustomNotebook = class(TGtkWSCustomNotebook)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
  end;

  { TGtk2WSPage }

  TGtk2WSPage = class(TWSPage)
  private
  protected
  public
  end;

  { TGtk2WSNotebook }

  TGtk2WSNotebook = class(TWSNotebook)
  private
  protected
  public
  end;

  { TGtk2WSShape }

  TGtk2WSShape = class(TWSShape)
  private
  protected
  public
  end;

  { TGtk2WSCustomSplitter }

  TGtk2WSCustomSplitter = class(TWSCustomSplitter)
  private
  protected
  public
  end;

  { TGtk2WSSplitter }

  TGtk2WSSplitter = class(TWSSplitter)
  private
  protected
  public
  end;

  { TGtk2WSPaintBox }

  TGtk2WSPaintBox = class(TWSPaintBox)
  private
  protected
  public
  end;

  { TGtk2WSCustomImage }

  TGtk2WSCustomImage = class(TWSCustomImage)
  private
  protected
  public
  end;

  { TGtk2WSImage }

  TGtk2WSImage = class(TWSImage)
  private
  protected
  public
  end;

  { TGtk2WSBevel }

  TGtk2WSBevel = class(TWSBevel)
  private
  protected
  public
  end;

  { TGtk2WSCustomRadioGroup }

  TGtk2WSCustomRadioGroup = class(TWSCustomRadioGroup)
  private
  protected
  public
  end;

  { TGtk2WSRadioGroup }

  TGtk2WSRadioGroup = class(TWSRadioGroup)
  private
  protected
  public
  end;

  { TGtk2WSCustomCheckGroup }

  TGtk2WSCustomCheckGroup = class(TWSCustomCheckGroup)
  private
  protected
  public
  end;

  { TGtk2WSCheckGroup }

  TGtk2WSCheckGroup = class(TGtkWSCheckGroup)
  private
  protected
  public
  end;

  { TGtk2WSBoundLabel }

  {TGtk2WSBoundLabel = class(TWSBoundLabel)
  private
  protected
  public
  end;}

  { TGtk2WSCustomLabeledEdit }

  TGtk2WSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  private
  protected
  public
  end;

  { TGtk2WSLabeledEdit }

  TGtk2WSLabeledEdit = class(TGtkWSLabeledEdit)
  private
  protected
  public
  end;

  { TGtk2WSCustomPanel }

  TGtk2WSCustomPanel = class(TGtkWSCustomPanel)
  private
  protected
  public
  end;

  { TGtk2WSPanel }

  TGtk2WSPanel = class(TGtkWSPanel)
  private
  protected
  public
  end;


implementation

uses interfacebase;

type
  GtkNotebookPressEventProc = function (widget:PGtkWidget; event:PGdkEventButton):gboolean; cdecl;
  
var
  OldNoteBookButtonPress: GtkNotebookPressEventProc = nil;

// this was created as a workaround of a tnotebook eating rightclick of custom controls
function Notebook_Button_Press(widget:PGtkWidget; event:PGdkEventButton):gboolean; cdecl;
begin
  Result := True;
  if gtk_get_event_widget(PGdkEvent(event)) <> widget then exit;
  if OldNoteBookButtonPress = nil then exit;
  Result := OldNoteBookButtonPress(widget, event);
end;

procedure HookNoteBookClass;
var
WidgetClass: PGtkWidgetClass;
begin
  WidgetClass := GTK_WIDGET_CLASS(gtk_type_class(gtk_notebook_get_type));

  OldNoteBookButtonPress := GtkNotebookPressEventProc(WidgetClass^.button_press_event);
  WidgetClass^.button_press_event := @Notebook_Button_Press;
end;

{ TGtk2WSCustomNotebook }

class function TGtk2WSCustomNotebook.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  P: PGtkNoteBook;
begin
  if OldNoteBookButtonPress = nil then HookNoteBookClass;
  P := PGtknotebook(WidgetSet.CreateComponent(AWinControl));
  Result := HWND(P);
end;


initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomPage, TGtk2WSCustomPage);
  RegisterWSComponent(TCustomNotebook, TGtk2WSCustomNotebook, TGtk2PrivateNotebook);
//  RegisterWSComponent(TPage, TGtk2WSPage);
//  RegisterWSComponent(TNotebook, TGtk2WSNotebook);
//  RegisterWSComponent(TShape, TGtk2WSShape);
//  RegisterWSComponent(TCustomSplitter, TGtk2WSCustomSplitter);
//  RegisterWSComponent(TSplitter, TGtk2WSSplitter);
//  RegisterWSComponent(TPaintBox, TGtk2WSPaintBox);
//  RegisterWSComponent(TCustomImage, TGtk2WSCustomImage);
//  RegisterWSComponent(TImage, TGtk2WSImage);
//  RegisterWSComponent(TBevel, TGtk2WSBevel);
//  RegisterWSComponent(TCustomRadioGroup, TGtk2WSCustomRadioGroup);
//  RegisterWSComponent(TRadioGroup, TGtk2WSRadioGroup);
//  RegisterWSComponent(TCustomCheckGroup, TGtk2WSCustomCheckGroup);
//  RegisterWSComponent(TCheckGroup, TGtk2WSCheckGroup);
//  RegisterWSComponent(TBoundLabel, TGtk2WSBoundLabel);
//  RegisterWSComponent(TCustomLabeledEdit, TGtk2WSCustomLabeledEdit);
//  RegisterWSComponent(TLabeledEdit, TGtk2WSLabeledEdit);
//  RegisterWSComponent(TCustomPanel, TGtk2WSCustomPanel);
//  RegisterWSComponent(TPanel, TGtk2WSPanel);
////////////////////////////////////////////////////
end.
