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

{$I ../gtk/gtkdefines.inc}

{$mode objfpc}{$H+}

interface

uses
  // libs
  Math, GLib2, Gtk2, Gdk2, Gtk2Int, gtkProc, GtkDef,
  // LCL
  LCLProc, ExtCtrls, Classes, Controls, SysUtils, LCLType,
  // widgetset
  WSExtCtrls, WSLCLClasses, WSProc,
  GtkWSExtCtrls, gtk2WSPrivate;

type

  { TGtk2WSCustomPage }

  TGtk2WSCustomPage = class(TGtkWSCustomPage)
  private
  protected
  public
    class function GetDefaultClientRect(const AWinControl: TWinControl;
             const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect
             ): boolean; override;
  end;

  { TGtk2WSCustomNotebook }
  
  TGtk2WSCustomNotebook = class(TGtkWSCustomNotebook)
  private
  protected
  public
    class function CreateHandle(const AWinControl: TWinControl;
                                const AParams: TCreateParams): HWND; override;
    class function GetDefaultClientRect(const AWinControl: TWinControl;
             const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect
             ): boolean; override;
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

  { TGtk2WSCustomTrayIcon }

  TGtk2WSCustomTrayIcon = class(TWSCustomTrayIcon)
  public
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
//  gtk2, gdk2, glib2, gtkdef, gtkproc,
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

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomPage, TGtk2WSCustomPage);
  RegisterWSComponent(TCustomNotebook, TGtk2WSCustomNotebook, TGtk2PrivateNotebook);
  RegisterWSComponent(TCustomPage, TGtk2WSCustomPage);
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
  RegisterWSComponent(TCustomTrayIcon, TGtk2WSCustomTrayIcon);
////////////////////////////////////////////////////
end.
