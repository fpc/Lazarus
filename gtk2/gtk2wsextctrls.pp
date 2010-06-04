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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit Gtk2WSExtCtrls;

{$I gtk2defines.inc}
//{$define UseStatusIcon} // can be used only with fpc r13008


{$mode objfpc}{$H+}

interface

uses
  // libs
  Math, GLib2, Gtk2, Gdk2, Gdk2Pixbuf, Gtk2Int, Gtk2Def, {$ifdef UseStatusIcon}Gtk2Ext, {$endif}
  // LCL
  LCLProc, ExtCtrls, Classes, Controls, SysUtils, LCLType,
  // widgetset
  WSExtCtrls, WSLCLClasses, WSProc,
  GtkWSExtCtrls, gtk2WSPrivate, Gtk2Proc;

type

  { TGtk2WSCustomPage }

  TGtk2WSCustomPage = class(TGtkWSCustomPage)
  published
    class function GetDefaultClientRect(const AWinControl: TWinControl;
             const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect
             ): boolean; override;
  end;

  { TGtk2WSCustomNotebook }
  
  TGtk2WSCustomNotebook = class(TGtkWSCustomNotebook)
  published
    class function CreateHandle(const AWinControl: TWinControl;
                                const AParams: TCreateParams): HWND; override;
    class function GetDefaultClientRect(const AWinControl: TWinControl;
             const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect
             ): boolean; override;
  end;

  { TGtk2WSPage }

  TGtk2WSPage = class(TWSPage)
  published
  end;

  { TGtk2WSNotebook }

  TGtk2WSNotebook = class(TWSNotebook)
  published
  end;

  { TGtk2WSShape }

  TGtk2WSShape = class(TWSShape)
  published
  end;

  { TGtk2WSCustomSplitter }

  TGtk2WSCustomSplitter = class(TWSCustomSplitter)
  published
  end;

  { TGtk2WSSplitter }

  TGtk2WSSplitter = class(TWSSplitter)
  published
  end;

  { TGtk2WSPaintBox }

  TGtk2WSPaintBox = class(TWSPaintBox)
  published
  end;

  { TGtk2WSCustomImage }

  TGtk2WSCustomImage = class(TWSCustomImage)
  published
  end;

  { TGtk2WSImage }

  TGtk2WSImage = class(TWSImage)
  published
  end;

  { TGtk2WSBevel }

  TGtk2WSBevel = class(TWSBevel)
  published
  end;

  { TGtk2WSCustomRadioGroup }

  TGtk2WSCustomRadioGroup = class(TWSCustomRadioGroup)
  published
  end;

  { TGtk2WSRadioGroup }

  TGtk2WSRadioGroup = class(TWSRadioGroup)
  published
  end;

  { TGtk2WSCustomCheckGroup }

  TGtk2WSCustomCheckGroup = class(TWSCustomCheckGroup)
  published
  end;

  { TGtk2WSCheckGroup }

  TGtk2WSCheckGroup = class(TGtkWSCheckGroup)
  published
  end;

  { TGtk2WSBoundLabel }

  {TGtk2WSBoundLabel = class(TWSBoundLabel)
  private
  protected
  public
  end;}

  { TGtk2WSCustomLabeledEdit }

  TGtk2WSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  published
  end;

  { TGtk2WSLabeledEdit }

  TGtk2WSLabeledEdit = class(TGtkWSLabeledEdit)
  published
  end;

  { TGtk2WSCustomPanel }

  TGtk2WSCustomPanel = class(TGtkWSCustomPanel)
  published
  end;

  { TGtk2WSPanel }

  TGtk2WSPanel = class(TGtkWSPanel)
  published
  end;

  { TGtk2WSCustomTrayIcon }

  TGtk2WSCustomTrayIcon = class(TWSCustomTrayIcon)
  published
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
  end;

implementation

uses
{$ifdef HasX}
  x, xlib, xutil,
{$endif}
//  gtk2, gdk2, glib2, gtk2def, gtk2proc,
{$ifdef HasGdk2X}
  gdk2x,
{$endif}
  interfacebase;

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
  if OldNoteBookButtonPress = nil then
    HookNoteBookClass;
  //DebugLn(['TGtk2WSCustomNotebook.CreateHandle ',DbgSName(AWinControl)]);
  P := PGtkNoteBook(TGtkWSCustomNotebook.CreateHandle(AWinControl, AParams));
  Result := HWND(PtrUInt(P));
end;

class function TGtk2WSCustomNotebook.GetDefaultClientRect(
  const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer;
  var aClientRect: TRect): boolean;
var
  FrameBorders: TRect;
begin
  Result:=false;
  //DebugLn(['TGtk2WSCustomNotebook.GetDefaultClientRect ',DbgSName(AWinControl),' ',aWidth,'x',aHeight]);
  if AWinControl.HandleAllocated then begin

  end else begin
    FrameBorders:=GetStyleNotebookFrameBorders;
    aClientRect:=Rect(0,0,
                 Max(0,aWidth-FrameBorders.Left-FrameBorders.Right),
                 Max(0,aHeight-FrameBorders.Top-FrameBorders.Bottom));
    Result:=true;
  end;
  {$IFDEF VerboseSizeMsg}
  if Result then DebugLn(['TGtk2WSCustomNotebook.GetDefaultClientRect END FrameBorders=',dbgs(FrameBorders),' aClientRect=',dbgs(aClientRect)]);
  {$ENDIF}
end;


{ TGtk2WSCustomPage }

class function TGtk2WSCustomPage.GetDefaultClientRect(
  const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer;
  var aClientRect: TRect): boolean;
begin
  Result:=false;
  if AWinControl.Parent=nil then exit;
  if AWinControl.HandleAllocated and AWinControl.Parent.HandleAllocated
  and (PGtkWidget(AWinControl.Handle)^.parent<>nil) then
  begin

  end else begin
    Result:=true;
    aClientRect:=AWinControl.Parent.ClientRect;
    //DebugLn(['TGtk2WSCustomPage.GetDefaultClientRect ',DbgSName(AWinControl),' Parent=',DbgSName(AWinControl.Parent),' ParentBounds=',dbgs(AWinControl.Parent.BoundsRect),' ParentClient=',dbgs(AWinControl.Parent.ClientRect)]);
  end;
  {$IFDEF VerboseSizeMsg}
  if Result then DebugLn(['TGtk2WSCustomPage.GetDefaultClientRect ',DbgSName(AWinControl),' aClientRect=',dbgs(aClientRect)]);
  {$ENDIF}
end;

{$include gtk2trayicon.inc}

end.
