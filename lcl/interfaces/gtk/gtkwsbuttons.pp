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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  GLib2, Gtk2, Gtk2Private,
  {$ELSE}
  GLib, Gtk, Gtk1Private,
  {$ENDIF}
  // LCL
  Classes, LCLProc, LCLType, LMessages, Controls, Graphics, Buttons,
  // widgetset
  WSButtons, WSLCLClasses, WSProc,
  // interface
  GtkDef;

type
  PBitBtnWidgetInfo = ^TBitBtnWidgetInfo;
  TBitBtnWidgetInfo = record
    LabelWidget: Pointer;
    ImageWidget: Pointer;
    SpaceWidget: Pointer; 
    AlignWidget: Pointer; 
    TableWidget: Pointer; 
  end;

  { TGtkWSButton }

  TGtkWSButton = class(TWSButton)
  private
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ActiveDefaultButtonChanged(const AButton: TCustomButton); override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetShortcut(const AButton: TCustomButton; const OldShortcut, NewShortcut: TShortcut); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont : tFont); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
  end;

  { TGtkWSBitBtn }

  TGtkWSBitBtn = class(TWSBitBtn)
  private
  protected
    class procedure UpdateLayout(const AInfo: PBitBtnWidgetInfo; const ALayout: TButtonLayout; const AMargin: Integer);
    class procedure UpdateMargin(const AInfo: PBitBtnWidgetInfo; const ALayout: TButtonLayout; const AMargin: Integer);
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TBitmap); override;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); override;
    class procedure SetMargin(const ABitBtn: TCustomBitBtn; const AValue: Integer); override;
    class procedure SetSpacing(const ABitBtn: TCustomBitBtn; const AValue: Integer); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont : tFont); override;

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


class function TGtkWSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Button: TCustomButton;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  Button := AWinControl as TCustomButton;

  Result := TLCLIntfHandle(gtk_button_new_with_label('button'));
  if Result = 0 then Exit;
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Pointer(Result),'button');
  {$ENDIF}

  WidgetInfo := CreateWidgetInfo(Pointer(Result), Button, AParams);
  
  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(PGtkWidget(Result), @Allocation);

  SetCallbacks(PGtkWidget(Result), WidgetInfo);
end;

class procedure TGtkWSButton.ActiveDefaultButtonChanged(const AButton: TCustomButton);
begin
  if (AButton.Active)
  and (GTK_WIDGET_CAN_DEFAULT(pgtkwidget(AButton.Handle))) then
    //gtk_widget_grab_default(pgtkwidget(handle))
  else begin
    {DebugLn('LM_BTNDEFAULT_CHANGED ',TCustomButton(Sender).Name,':',Sender.ClassName,' widget can not grab default ',
      ' visible=',GTK_WIDGET_VISIBLE(PGtkWidget(Handle)),
      ' realized=',GTK_WIDGET_REALIZED(PGtkWidget(Handle)),
      ' mapped=',GTK_WIDGET_MAPPED(PGtkWidget(Handle)),
      '');}
    //  gtk_widget_Draw_Default(pgtkwidget(Handle));  //this isn't right but I'm not sure what to call
  end;
end;
    
class function TGtkWSButton.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
begin             
  // The button text is static, so let the LCL fallback to FCaption
  Result := False;
end;

class procedure TGtkWSButton.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin        
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));

  SignalConnect(AGtkWidget, 'clicked', @GtkWSButton_Clicked, AWidgetInfo);
end;

class procedure TGtkWSButton.SetShortcut(const AButton: TCustomButton; const OldShortcut, NewShortcut: TShortcut);
begin
  if not WSCheckHandleAllocated(AButton, 'SetShortcut')
  then Exit;

  // ToDo: use accelerator group of Form
  Accelerate(AButton, PGtkWidget(AButton.Handle), NewShortcut, 'clicked');
end;

class procedure TGtkWSButton.SetText(const AWinControl: TWinControl; const AText: String);
var
  BtnWidget: PGtkButton;
  LblWidget: PGtkLabel;
begin          
  if not WSCheckHandleAllocated(AWincontrol, 'SetText')
  then Exit;

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
  
  GtkWidgetSet.SetLabelCaption(LblWidget, AText, AWinControl,
                               PGtkWidget(BtnWidget), 'clicked');
end;

class procedure TGtkWSButton.SetColor(const AWinControl: TWinControl);
var
  Widget: PGTKWidget;
begin
  Widget:= PGtkWidget(AWinControl.Handle);
  GtkWidgetSet.SetWidgetColor(Widget, clNone, AWinControl.color,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
end;

class procedure TGtkWSButton.SetFont(const AWinControl: TWinControl;
  const AFont : TFont);
var
  Widget: PGTKWidget;
  LblWidget: PGtkWidget;
begin
  if not AWinControl.HandleAllocated then exit;
  if AFont.IsDefault then exit;

  Widget:= PGtkWidget(AWinControl.Handle);
  LblWidget := (pGtkBin(Widget)^.Child);

  if LblWidget<>nil then begin
    GtkWidgetSet.SetWidgetColor(LblWidget, AWinControl.font.color, clNone,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
    GtkWidgetSet.SetWidgetFont(LblWidget, AFont);
  end;
end;

class procedure TGtkWSButton.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  GetGTKDefaultWidgetSize(AWinControl,PreferredWidth,PreferredHeight,
                          WithThemeSpace);
  //debugln('TGtkWSButton.GetPreferredSize ',DbgSName(AWinControl),' PreferredWidth=',dbgs(PreferredWidth),' PreferredHeight=',dbgs(PreferredHeight));
end;

{ TGtkWSBitBtn }

{
 The interiour of TBitBtn is created with a 4X4 table
 Depending in how the image and label are aligned, only a 
 columns or rows are used (like a 4x1 or 1x4 table). 
 This way the table doesn't have to be recreated on changes.
 So there are 4 positions 0, 1, 2, 3.
 Positions 1 and 2 are used for the label and image.
 Since this is always the case, spacing can be implemented
 by setting the spacing of row/col 1
 To get a margin, a gtkInvisible is needed for bottom and 
 right, so the invisible is always in position 3. 
}
class function TGtkWSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  BitBtn: TCustomBitBtn;
  WidgetInfo: PWidgetInfo;
  BitBtnInfo: PBitBtnWidgetInfo;
  Allocation: TGTKAllocation;
begin
  BitBtn := AWinControl as TCustomBitBtn;

  Result := TLCLIntfHandle(gtk_button_new);
  if Result = 0 then Exit;
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Pointer(Result),dbgsName(AWinControl));
  {$ENDIF}

  WidgetInfo := CreateWidgetInfo(Pointer(Result), BitBtn, AParams);

  New(BitBtnInfo);
  FillChar(BitBtnInfo^, SizeOf(BitBtnInfo^), 0);
  WidgetInfo^.UserData := BitBtnInfo;
  WidgetInfo^.DataOwner := True;
  
  
  BitBtnInfo^.AlignWidget := gtk_alignment_new(0.5, 0.5, 0, 0);
  gtk_container_add(Pointer(Result), BitBtnInfo^.AlignWidget);

  BitBtnInfo^.TableWidget := gtk_table_new(4, 4, False);
  gtk_container_add(BitBtnInfo^.AlignWidget, BitBtnInfo^.TableWidget);
  
  BitBtnInfo^.LabelWidget := gtk_label_new('bitbtn');
  gtk_table_attach(BitBtnInfo^.TableWidget, BitBtnInfo^.LabelWidget,
                   2, 3, 0, 4, 0, 0, 0, 0);

  BitBtnInfo^.SpaceWidget := nil;
  BitBtnInfo^.ImageWidget := nil;

  gtk_widget_show(BitBtnInfo^.AlignWidget);
  gtk_widget_show(BitBtnInfo^.TableWidget);
  gtk_widget_show(BitBtnInfo^.LabelWidget);

  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(PGtkWidget(Result), @Allocation);

  TGtkWSButton.SetCallbacks(PGtkWidget(Result), WidgetInfo);
end;

class procedure TGtkWSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn;
  const AValue: TBitmap);
var
  WidgetInfo: PWidgetInfo;
  BitBtnInfo: PBitBtnWidgetInfo;
  GDIObject: PGDIObject;
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetGlyph')
  then Exit;
  
  WidgetInfo := GetWidgetInfo(Pointer(ABitBtn.Handle));
  BitBtnInfo := WidgetInfo^.UserData;                       
  
  // check if an image is needed
  if (AValue.Handle = 0)
  or (AValue.Width = 0) 
  or (AValue.Height = 0)
  then begin                                  
    if BitBtnInfo^.ImageWidget <> nil
    then begin
      gtk_container_remove(BitBtnInfo^.TableWidget, BitBtnInfo^.ImageWidget);
      BitBtnInfo^.ImageWidget := nil;
    end;
    Exit;
  end;
  
  GDIObject := PgdiObject(AValue.Handle);
  // check for image
  if BitBtnInfo^.ImageWidget = nil
  then begin
    BitBtnInfo^.ImageWidget :=
     gtk_pixmap_new(GDIObject^.GDIPixmapObject, GDIObject^.GDIBitmapMaskObject);
    gtk_widget_show(BitBtnInfo^.ImageWidget);
    UpdateLayout(BitBtnInfo, ABitBtn.Layout, ABitBtn.Margin);
  end
  else begin
    gtk_pixmap_set(BitBtnInfo^.ImageWidget, GDIObject^.GDIPixmapObject,
                   GDIObject^.GDIBitmapMaskObject);
  end;
end;

class procedure TGtkWSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
var
  WidgetInfo: PWidgetInfo;
  BitBtnInfo: PBitBtnWidgetInfo;
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetLayout')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ABitBtn.Handle));
  BitBtnInfo := WidgetInfo^.UserData;                       
  UpdateLayout(BitBtnInfo, AValue, ABitBtn.Margin);
end;

class procedure TGtkWSBitBtn.SetMargin(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
var
  WidgetInfo: PWidgetInfo;
  BitBtnInfo: PBitBtnWidgetInfo;
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetMargin')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ABitBtn.Handle));
  BitBtnInfo := WidgetInfo^.UserData;                       
  UpdateMargin(BitBtnInfo, ABitBtn.Layout, AValue);
end;

class procedure TGtkWSBitBtn.SetSpacing(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
var
  WidgetInfo: PWidgetInfo;
  BitBtnInfo: PBitBtnWidgetInfo;
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetSpacing')
  then Exit;
  
  WidgetInfo := GetWidgetInfo(Pointer(ABitBtn.Handle));
  BitBtnInfo := WidgetInfo^.UserData;                       
  gtk_table_set_col_spacing(BitBtnInfo^.TableWidget, 1, AValue);
  gtk_table_set_row_spacing(BitBtnInfo^.TableWidget, 1, AValue);
end;

class procedure TGtkWSBitBtn.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  WidgetInfo: PWidgetInfo;
  BitBtnInfo: PBitBtnWidgetInfo;
begin          
  if not WSCheckHandleAllocated(AWincontrol, 'SetText')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(AWinControl.Handle));
  BitBtnInfo := WidgetInfo^.UserData;                       
  if BitBtnInfo^.LabelWidget = nil then Exit;  

  //debugln('TGtkWSBitBtn.SetText ',DbgStr(AText));
  GtkWidgetSet.SetLabelCaption(BitBtnInfo^.LabelWidget, AText, AWinControl,
                               WidgetInfo^.CoreWidget, 'clicked');
end;

class procedure TGtkWSBitBtn.SetColor(const AWinControl: TWinControl);
var
  Widget: PGTKWidget;
begin
  if not AWinControl.HandleAllocated then exit;
  Widget:= PGtkWidget(AWinControl.Handle);
  GtkWidgetSet.SetWidgetColor(Widget, clNone, AWinControl.color,
     [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
end;

class procedure TGtkWSBitBtn.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  WidgetInfo: PWidgetInfo;
  BitBtnInfo: PBitBtnWidgetInfo;
  Widget: PGTKWidget;
begin
  if not AWinControl.HandleAllocated then exit;
  if AFont.IsDefault then exit;
  
  Widget:= PGtkWidget(AWinControl.Handle);
  WidgetInfo := GetWidgetInfo(Widget);
  BitBtnInfo := WidgetInfo^.UserData;

  if (BitBtnInfo=nil) or (BitBtnInfo^.LabelWidget = nil) then Exit;
  GtkWidgetSet.SetWidgetColor(BitBtnInfo^.LabelWidget, AWinControl.font.color,
    clNone,
    [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
  GtkWidgetSet.SetWidgetFont(BitBtnInfo^.LabelWidget, AFont);
end;

class procedure TGtkWSBitBtn.UpdateLayout(const AInfo: PBitBtnWidgetInfo;
  const ALayout: TButtonLayout; const AMargin: Integer);
begin
  if (AInfo^.ImageWidget = nil)
  and (AMargin < 0) 
  then exit; // nothing to do
  
  // add references and remove it from the table
  gtk_object_ref(AInfo^.LabelWidget);
  gtk_container_remove(AInfo^.TableWidget, AInfo^.LabelWidget);
  if AInfo^.ImageWidget <> nil
  then begin
    gtk_object_ref(AInfo^.ImageWidget);                          
    if PGtkWidget(AInfo^.ImageWidget)^.Parent <> nil
    then gtk_container_remove(AInfo^.TableWidget, AInfo^.ImageWidget);
  end;
  if AInfo^.SpaceWidget <> nil
  then begin
    gtk_object_ref(AInfo^.SpaceWidget);
    if PGtkWidget(AInfo^.SpaceWidget)^.Parent <> nil
    then gtk_container_remove(AInfo^.TableWidget, AInfo^.SpaceWidget);
  end;
  
  case ALayout of 
    blGlyphLeft: begin
      if AInfo^.ImageWidget <> nil
      then gtk_table_attach(AInfo^.TableWidget, AInfo^.ImageWidget,
                            1, 2, 1, 3, 0, 0, 0, 0);
      gtk_table_attach(AInfo^.TableWidget, AInfo^.LabelWidget,
                       2, 3, 1, 3, 0, 0, 0, 0);
    end;
    blGlyphRight: begin
      gtk_table_attach(AInfo^.TableWidget, AInfo^.LabelWidget,
                       1, 2, 1, 3, 0, 0, 0, 0);
      if AInfo^.ImageWidget <> nil
      then gtk_table_attach(AInfo^.TableWidget, AInfo^.ImageWidget,
                            2, 3, 1, 3, 0, 0, 0, 0);
      if AInfo^.SpaceWidget <> nil
      then gtk_table_attach(AInfo^.TableWidget, AInfo^.SpaceWidget,
                            3, 4, 1, 3, 0, 0, 0, 0);
    end;
    blGlyphTop: begin
      if AInfo^.ImageWidget <> nil
      then gtk_table_attach(AInfo^.TableWidget, AInfo^.ImageWidget,
                            1, 3, 1, 2, 0, 0, 0, 0);
      gtk_table_attach(AInfo^.TableWidget, AInfo^.LabelWidget,
                       1, 3, 2, 3, 0, 0, 0, 0);
    end; 
    blGlyphBottom: begin
      gtk_table_attach(AInfo^.TableWidget, AInfo^.LabelWidget,
                       1, 3, 1, 2, 0, 0, 0, 0);
      if AInfo^.ImageWidget <> nil
      then gtk_table_attach(AInfo^.TableWidget, AInfo^.ImageWidget,
                            1, 3, 2, 3, 0, 0, 0, 0);
      if AInfo^.SpaceWidget <> nil
      then gtk_table_attach(AInfo^.TableWidget, AInfo^.SpaceWidget,
                            1, 3, 3, 4, 0, 0, 0, 0);
    end;
  end;
  
  // remove temp reference
  if AInfo^.SpaceWidget <> nil
  then gtk_object_unref(AInfo^.SpaceWidget);
  if AInfo^.ImageWidget <> nil
  then gtk_object_unref(AInfo^.ImageWidget);
  gtk_object_unref(AInfo^.LabelWidget);
  
  if AMargin >= 0 
  then UpdateMargin(AInfo, ALayout, AMargin)
end;

class procedure TGtkWSBitBtn.UpdateMargin(const AInfo: PBitBtnWidgetInfo;
  const ALayout: TButtonLayout; const AMargin: Integer);
begin
  if AMargin < 0 
  then begin
    if AInfo^.SpaceWidget <> nil
    then begin
      gtk_container_remove(AInfo^.TableWidget, AInfo^.SpaceWidget);
      AInfo^.SpaceWidget := nil;

      gtk_alignment_set(AInfo^.AlignWidget, 0.5, 0.5, 0, 0);
      
      case ALayout of 
        blGlyphLeft:   gtk_table_set_col_spacing(AInfo^.TableWidget, 0, 0);
        blGlyphRight:  gtk_table_set_col_spacing(AInfo^.TableWidget, 2, 0);
        blGlyphTop:    gtk_table_set_row_spacing(AInfo^.TableWidget, 0, 0);
        blGlyphBottom: gtk_table_set_row_spacing(AInfo^.TableWidget, 2, 0);
      end;
    end;
  end
  else begin
    if (AInfo^.SpaceWidget = nil)
    and (ALayout in [blGlyphRight, blGlyphBottom])
    then begin
      AInfo^.SpaceWidget := gtk_invisible_new;
      UpdateLayout(AInfo, ALayout, AMargin);
    end
    else begin
      case ALayout of 
        blGlyphLeft: begin
          gtk_alignment_set(AInfo^.AlignWidget, 0, 0.5, 0, 0);
          gtk_table_set_col_spacing(AInfo^.TableWidget, 0, AMargin);
          gtk_table_set_col_spacing(AInfo^.TableWidget, 2, 0);
          gtk_table_set_row_spacing(AInfo^.TableWidget, 0, 0);
          gtk_table_set_row_spacing(AInfo^.TableWidget, 2, 0);
        end;
        blGlyphRight: begin
          gtk_alignment_set(AInfo^.AlignWidget, 1, 0.5, 0, 0);
          gtk_table_set_col_spacing(AInfo^.TableWidget, 0, 0);
          gtk_table_set_col_spacing(AInfo^.TableWidget, 2, AMargin);
          gtk_table_set_row_spacing(AInfo^.TableWidget, 0, 0);
          gtk_table_set_row_spacing(AInfo^.TableWidget, 2, 0);
        end;
        blGlyphTop: begin
          gtk_alignment_set(AInfo^.AlignWidget, 0.5, 0, 0, 0);
          gtk_table_set_col_spacing(AInfo^.TableWidget, 0, 0);
          gtk_table_set_col_spacing(AInfo^.TableWidget, 2, 0);
          gtk_table_set_row_spacing(AInfo^.TableWidget, 0, AMargin);
          gtk_table_set_row_spacing(AInfo^.TableWidget, 2, 0);
        end;
        blGlyphBottom: begin
          gtk_alignment_set(AInfo^.AlignWidget, 0.5, 1, 0, 0);
          gtk_table_set_col_spacing(AInfo^.TableWidget, 0, 0);
          gtk_table_set_col_spacing(AInfo^.TableWidget, 2, 0);
          gtk_table_set_row_spacing(AInfo^.TableWidget, 0, 0);
          gtk_table_set_row_spacing(AInfo^.TableWidget, 2, AMargin);
        end;
      end;
    end;
  end;
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////                 
{$ifdef gtk1}
  RegisterWSComponent(TCustomButton, TGtkWSButton, TGtk1PrivateButton);
  RegisterWSComponent(TCustomBitBtn, TGtkWSBitBtn, TGtk1PrivateButton); // register it to fallback to default
{$else}
  RegisterWSComponent(TCustomButton, TGtkWSButton, TGtk2PrivateButton);
  RegisterWSComponent(TCustomBitBtn, TGtkWSBitBtn, TGtk2PrivateButton); // register it to fallback to default
{$endif}  
//  RegisterWSComponent(TCustomSpeedButton, TGtkWSSpeedButton);
////////////////////////////////////////////////////
end.
