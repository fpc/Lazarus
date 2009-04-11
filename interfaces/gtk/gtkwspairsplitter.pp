{ $Id$}
{
 *****************************************************************************
 *                           GtkWSPairSplitter.pp                            * 
 *                           --------------------                            * 
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
unit GtkWSPairSplitter;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF GTK2}
  Gtk2, //Glib2, Gdk2,
  {$ELSE}
  Gtk, //Glib, Gdk,
  {$ENDIF}
  GtkWSPrivate, GtkInt, GtkDef, GtkProc, GtkWSControls,
  Classes, Controls, LCLType, PairSplitter,
  WSPairSplitter, WSLCLClasses, WSProc;

type

  { TGtkWSPairSplitterSide }
  TGtkWSPairSplitterSide = class(TWSPairSplitterSide)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;
  
  { TGtkWSCustomPairSplitter }

  TGtkWSCustomPairSplitter = class(TWSCustomPairSplitter)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class function AddSide(ASplitter: TCustomPairSplitter; ASide: TPairSplitterSide; Side: integer): Boolean; override;
    class function SetPosition(ASplitter: TCustomPairSplitter; var NewPosition: integer): Boolean; override;
    // special cursor handling
    class function GetSplitterCursor(ASplitter: TCustomPairSplitter; var ACursor: TCursor): Boolean; override;
    class function SetSplitterCursor(ASplitter: TCustomPairSplitter; ACursor: TCursor): Boolean; override;
  end;

implementation

{ TGtkWSCustomPairSplitter }

class procedure TGtkWSCustomPairSplitter.SetCallbacks(
  const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtkWSCustomPairSplitter.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  // create the paned
  if TCustomPairSplitter(AWinControl).SplitterType = pstHorizontal then
    Widget := gtk_hpaned_new
  else
    Widget := gtk_vpaned_new;

  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  Result := TLCLIntfHandle(PtrUInt(Widget));

  WidgetInfo := CreateWidgetInfo(Widget, AWinControl, AParams);

  Set_RC_Name(AWinControl, Widget);
  SetCallBacks(Widget, WidgetInfo);
end;

class function TGtkWSCustomPairSplitter.AddSide(ASplitter: TCustomPairSplitter;
  ASide: TPairSplitterSide; Side: integer): Boolean;
begin
  Result := False;
  
  if not (WSCheckHandleAllocated(ASplitter, 'AddSide - splitter') and
          WSCheckHandleAllocated(ASide, 'AddSide - side'))
  then Exit;

  if (Side<0) or (Side>1) then exit;
  
  if Side = 0 then
    gtk_paned_add1(PGtkPaned(ASplitter.Handle),PGtkWidget(ASide.Handle))
  else
    gtk_paned_add2(PGtkPaned(ASPlitter.Handle),PGtkWidget(ASide.Handle));
    
  Result := True;
end;

class function TGtkWSCustomPairSplitter.SetPosition(
  ASplitter: TCustomPairSplitter; var NewPosition: integer): Boolean;
begin
  Result := False;
  if not WSCheckHandleAllocated(ASplitter, 'SetPosition')
  then Exit;
  if NewPosition>=0 then
    gtk_paned_set_position(PGtkPaned(ASplitter.Handle), NewPosition);
  NewPosition := PGtkPaned(ASplitter.Handle)^.child1_size;
  Result := True;
end;

class function TGtkWSCustomPairSplitter.GetSplitterCursor(
  ASplitter: TCustomPairSplitter; var ACursor: TCursor): Boolean;
begin
  Result := False;
end;

class function TGtkWSCustomPairSplitter.SetSplitterCursor(
  ASplitter: TCustomPairSplitter; ACursor: TCursor): Boolean;
begin
  Result := False;
end;

{ TGtkWSPairSplitterSide }

class procedure TGtkWSPairSplitterSide.SetCallbacks(
  const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtkWSPairSplitterSide.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  Widget := GtkWidgetset.CreateSimpleClientAreaWidget(AWinControl, True);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  Result := TLCLIntfHandle(PtrUInt(Widget));

  WidgetInfo := GetWidgetInfo(Widget);
  WidgetInfo^.LCLObject := AWinControl;
  WidgetInfo^.Style := AParams.Style;
  WidgetInfo^.ExStyle := AParams.ExStyle;
  WidgetInfo^.WndProc := PtrUInt(AParams.WindowClass.lpfnWndProc);

  Set_RC_Name(AWinControl, Widget);
  SetCallBacks(Widget, WidgetInfo);
end;

initialization

end.
