{
 *****************************************************************************
 *                             Gtk2WSButtons.pp                              * 
 *                             ----------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk2WSButtons;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  glib2, gtk2, gdk2, gdk2pixbuf,
  // LCL
////////////////////////////////////////////////////
  LCLType, Controls, Buttons, Graphics, GraphType, ImgList, Classes,
  {$IFDEF DebugLCLComponents}
  LazLoggerBase,
  {$ENDIF}
////////////////////////////////////////////////////
  WSButtons, WSLCLClasses, WSProc,
  Gtk2Def;

type
  PBitBtnWidgetInfo = ^TBitBtnWidgetInfo;
  TBitBtnWidgetInfo = record
    ImageWidget: Pointer;
    LabelWidget: Pointer;
    SetLayoutCalled: boolean;
  end;

  { TGtk2WSBitBtn }

  TGtk2WSBitBtn = class(TWSBitBtn)
  private
    class procedure BuildWidget(ABitBtn: TCustomBitBtn; MainWidget: PGtkWidget;
      ABitBtnInfo: PBitBtnWidgetInfo; const ACaption: String);
    class procedure UnparentWidget(Widget: PGtkWidget);
    class procedure UpdateImageWidget(ImageWidget: PGtkImage; Bitmap: TBitmap);
    class procedure UpdateLabelFont(LabelWidget: PGtkWidget; Font: TFont);
  protected
    class function UpdateGlyph(const ABitBtn: TCustomBitBtn; BitBtnInfo: PBitBtnWidgetInfo;
      const AValue: TButtonGlyph; const AButtonState: TButtonState): Boolean;
    class procedure UpdateMargin(const ABitBtn: TCustomBitBtn; const AAlignWidget: PGtkAlignment; const AMargin: Integer);
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); override;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const {%H-}AValue: TButtonLayout); override;
    class procedure SetMargin(const ABitBtn: TCustomBitBtn; const AValue: Integer); override;
    class procedure SetSpacing(const ABitBtn: TCustomBitBtn; const AValue: Integer); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
  end;
  TGtk2WSBitBtnClass = class of TGtk2WSBitBtn;


  { TGtk2WSSpeedButton }

  TGtk2WSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
  end;


implementation

uses
  Gtk2Proc, Gtk2Int, Gtk2WSStdCtrls;

const
  GtkStateToButtonState: array[GTK_STATE_NORMAL..GTK_STATE_INSENSITIVE] of TButtonState =
  (
{GTK_STATE_NORMAL     } bsUp,
{GTK_STATE_ACTIVE     } bsDown,
{GTK_STATE_PRELIGHT   } bsHot,
{GTK_STATE_SELECTED   } bsDown,
{GTK_STATE_INSENSITIVE} bsDisabled
  );
  
type
  TCustomBitBtnAccess = class(TCustomBitBtn)
  end;

procedure GtkWSBitBtn_StateChanged(AWidget: PGtkWidget; {%H-}AState: TGtkStateType; AInfo: PWidgetInfo); cdecl;
var
  BitBtn: TCustomBitBtnAccess;
begin
  //WriteLn(Astate, ' :: ', GTK_WIDGET_STATE(AWidget));
  BitBtn := TCustomBitBtnAccess(AInfo^.LCLObject);
  TGtk2WSBitBtn.UpdateGlyph(BitBtn, PBitBtnWidgetInfo(AInfo^.UserData),
    BitBtn.FButtonGlyph, GtkStateToButtonState[GTK_WIDGET_STATE(AWidget)]);
end;

{ TGtk2WSBitBtn }

class procedure TGtk2WSBitBtn.BuildWidget(ABitBtn: TCustomBitBtn; MainWidget: PGtkWidget; ABitBtnInfo: PBitBtnWidgetInfo; const ACaption: String);
var
  AlignWidget: PGtkWidget;
  LabelWidget: PGtkWidget;
  ImageWidget: PGtkWidget;
  ContentWidget: PGtkWidget;
begin
  ImageWidget := ABitBtnInfo^.ImageWidget;
  // keep a temporary reference to avoid the destruction and remove ImageWidget
  if ImageWidget <> nil then
  begin
    g_object_ref(ImageWidget);
    UnparentWidget(ImageWidget);
  end;
  // clear the widget (will destroy the children)
  ContentWidget := gtk_bin_get_child(PGtkBin(MainWidget));
  if ContentWidget <> nil then
    gtk_container_remove(PGtkContainer(MainWidget), ContentWidget);
  ContentWidget := nil;
  // setup label
  LabelWidget := nil;
  if ACaption <> '' then
  begin
    LabelWidget := gtk_label_new(nil);
    GTK2WidgetSet.SetLabelCaption(PGtkLabel(LabelWidget), ACaption);
    UpdateLabelFont(LabelWidget, ABitBtn.Font);
  end;
  // button with image and label
  if (ImageWidget <> nil) and (LabelWidget <> nil) then
  begin
    if (ABitBtn.Layout in [blGlyphLeft, blGlyphRight]) then
      ContentWidget := gtk_hbox_new(False, ABitBtn.Spacing)
    else
      ContentWidget := gtk_vbox_new(False, ABitBtn.Spacing);

    if (ABitBtn.Layout in [blGlyphLeft, blGlyphTop]) then
      gtk_box_pack_start(PGtkBox(ContentWidget), ImageWidget, True, True, 0)
    else
      gtk_box_pack_end(PGtkBox(ContentWidget), ImageWidget, True, True, 0);

    if (ABitBtn.Layout in [blGlyphRight, blGlyphBottom]) then
      gtk_box_pack_start(PGtkBox(ContentWidget), LabelWidget, True, True, 0)
    else
      gtk_box_pack_end(PGtkBox(ContentWidget), LabelWidget, True, True, 0);
  end
  else
  begin
    // only image or label (or none)
    if ImageWidget <> nil then
      ContentWidget := ImageWidget
    else if LabelWidget <> nil then
      ContentWidget := LabelWidget;
  end;
  // setup align and build the widget
  AlignWidget := gtk_alignment_new(0, 0, 0, 0);
  UpdateMargin(ABitBtn, PGtkAlignment(AlignWidget), ABitBtn.Margin);
  gtk_container_add(PGtkContainer(MainWidget), AlignWidget);
  if ContentWidget <> nil then
    gtk_container_add(PGtkContainer(AlignWidget), ContentWidget);
  gtk_widget_show_all(AlignWidget);
  // Release the temporary reference
  if ImageWidget <> nil then
    g_object_unref(ImageWidget);

  ABitBtnInfo^.LabelWidget := LabelWidget;
end;

class procedure TGtk2WSBitBtn.UnparentWidget(Widget: PGtkWidget);
var
  ParentWidget: PGtkWidget;
begin
  ParentWidget := gtk_widget_get_parent(Widget);
  if ParentWidget <> nil then
    gtk_container_remove(PGtkContainer(ParentWidget), Widget);
end;

class function TGtk2WSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  BitBtn: TCustomBitBtn absolute AWinControl;
  WidgetInfo: PWidgetInfo;
  BitBtnInfo: PBitBtnWidgetInfo;
  Allocation: TGTKAllocation;
begin
  Result := TLCLIntfHandle({%H-}PtrUInt(gtk_button_new));
  if Result = 0 then Exit;
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Pointer(Result),DbgSName(AWinControl));
  {$ENDIF}

  WidgetInfo := CreateWidgetInfo({%H-}Pointer(Result), BitBtn, AParams);

  New(BitBtnInfo);
  FillChar(BitBtnInfo^, SizeOf(BitBtnInfo^), 0);
  WidgetInfo^.UserData := BitBtnInfo;
  WidgetInfo^.DataOwner := True;

  gtk_widget_show({%H-}PGtkWidget(Result));

  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate({%H-}PGtkWidget(Result), @Allocation);

  Set_RC_Name(AWinControl, {%H-}PGtkWidget(Result));
  if not AWinControl.HandleObjectShouldBeVisible and not (csDesigning in AWinControl.ComponentState) then
    gtk_widget_hide({%H-}PGtkWidget(Result));
  gtk_widget_set_sensitive({%H-}PGtkWidget(Result), AParams.Style and WS_DISABLED = 0);
  SetCallbacks({%H-}PGtkWidget(Result), WidgetInfo);
end;

class procedure TGtk2WSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonGlyph);
var
  MainWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  BitBtnInfo: PBitBtnWidgetInfo;
  BuildNeeded: Boolean;
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetGlyph') then
    Exit;
  MainWidget := {%H-}PGtkWidget(ABitBtn.Handle);
  WidgetInfo := GetWidgetInfo(MainWidget);
  BitBtnInfo := WidgetInfo^.UserData;
  BuildNeeded := UpdateGlyph(ABitBtn, BitBtnInfo, AValue, GtkStateToButtonState[GTK_WIDGET_STATE(MainWidget)]);
  if BuildNeeded and BitBtnInfo^.SetLayoutCalled then
    BuildWidget(ABitBtn, MainWidget, BitBtnInfo, ABitBtn.Caption);
end;

class procedure TGtk2WSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
var
  MainWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  BitBtnInfo: PBitBtnWidgetInfo;
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetLayout') then
    Exit;
  MainWidget := {%H-}Pointer(ABitBtn.Handle);
  WidgetInfo := GetWidgetInfo(MainWidget);
  BitBtnInfo := WidgetInfo^.UserData;
  BitBtnInfo^.SetLayoutCalled:=true;
  BuildWidget(ABitBtn, MainWidget, BitBtnInfo, ABitBtn.Caption);
end;

class procedure TGtk2WSBitBtn.SetMargin(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
var
  MainWidget: PGtkWidget;
  AlignWidget: PGtkAlignment;
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetMargin') then
    Exit;
  MainWidget := {%H-}PGtkWidget(ABitBtn.Handle);
  AlignWidget := PGtkAlignment(gtk_bin_get_child(PGtkBin(MainWidget)));
  if GTK_IS_ALIGNMENT(AlignWidget) then
    UpdateMargin(ABitBtn, AlignWidget, AValue);
end;

class procedure TGtk2WSBitBtn.SetSpacing(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
var
  MainWidget: PGtkWidget;
  ChildWidget: PGtkWidget;
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetSpacing') then
    Exit;
  MainWidget := {%H-}Pointer(ABitBtn.Handle);
  ChildWidget := gtk_bin_get_child(PGtkBin(MainWidget));
  if GTK_IS_ALIGNMENT(ChildWidget) then
  begin
    ChildWidget := gtk_bin_get_child(PGtkBin(ChildWidget));
    if GTK_IS_BOX(ChildWidget) then
      gtk_box_set_spacing(PGtkBox(ChildWidget), AValue);
  end;
end;

class procedure TGtk2WSBitBtn.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  MainWidget: PGtkWidget;
  LabelWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  BitBtnInfo: PBitBtnWidgetInfo;
  BuildNeeded: Boolean;
begin
  MainWidget := {%H-}Pointer(AWinControl.Handle);
  WidgetInfo := GetWidgetInfo(MainWidget);
  BitBtnInfo := WidgetInfo^.UserData;
  if (not BitBtnInfo^.SetLayoutCalled)
  or not WSCheckHandleAllocated(AWincontrol, 'SetText') then
    Exit;
  LabelWidget := BitBtnInfo^.LabelWidget;
  BuildNeeded := (LabelWidget = nil) xor (AText = '');
  if BuildNeeded then
    BuildWidget(TBitBtn(AWinControl), MainWidget, BitBtnInfo, AText)
  else
  begin
    if LabelWidget <> nil then
      Gtk2WidgetSet.SetLabelCaption(PGtkLabel(LabelWidget), AText);
  end;
end;

class procedure TGtk2WSBitBtn.SetColor(const AWinControl: TWinControl);
var
  Widget: PGTKWidget;
begin
  if not AWinControl.HandleAllocated then exit;
  Widget:= {%H-}PGtkWidget(AWinControl.Handle);
  Gtk2WidgetSet.SetWidgetColor(Widget, clNone, AWinControl.color,
     [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
end;

class procedure TGtk2WSBitBtn.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  WidgetInfo: PWidgetInfo;
  BitBtnInfo: PBitBtnWidgetInfo;
  LabelWidget: PGTKWidget;
begin
  if not AWinControl.HandleAllocated then exit;

  WidgetInfo := GetWidgetInfo({%H-}PGtkWidget(AWinControl.Handle));
  BitBtnInfo := WidgetInfo^.UserData;
  LabelWidget := BitBtnInfo^.LabelWidget;
  if LabelWidget <> nil then
    UpdateLabelFont(LabelWidget, AFont);
end;

{
  UpdateGlyph: update the bitbtn glyph and returns if the structure changed
}

class function TGtk2WSBitBtn.UpdateGlyph(const ABitBtn: TCustomBitBtn; BitBtnInfo: PBitBtnWidgetInfo;
  const AValue: TButtonGlyph; const AButtonState: TButtonState): Boolean;
var
  ShowGlyph: Boolean;
  ImageWidget: PGtkWidget;
  AGlyph: TBitmap;
  AIndex: Integer;
  AEffect: TGraphicsDrawEffect;
  AImageRes: TScaledImageListResolution;
begin
  ShowGlyph := ABitBtn.CanShowGlyph(True);
  if ShowGlyph then
  begin
    ImageWidget := BitBtnInfo^.ImageWidget;
    AGlyph := TBitmap.Create;
    AValue.GetImageIndexAndEffect(AButtonState, ABitBtn.Font.PixelsPerInch,
      ABitBtn.GetCanvasScaleFactor, AImageRes, AIndex, AEffect);
    if (AIndex <> -1) and AImageRes.Valid then
      AImageRes.GetBitmap(AIndex, AGlyph, AEffect);
    ShowGlyph := not AGlyph.Empty;
    if ShowGlyph then
    begin
      if ImageWidget = nil then
        ImageWidget := gtk_image_new;
      UpdateImageWidget(PGtkImage(ImageWidget), AGlyph);
    end;
    AGlyph.Destroy;
  end
  else
    ImageWidget := nil;
  // Return true if the image was removed or added
  Result := ImageWidget <> BitBtnInfo^.ImageWidget;
  if Result then
  begin
    // BitBtnInfo^.ImageWidget <> nil -> remove from parent
    if not ShowGlyph then
      UnparentWidget(BitBtnInfo^.ImageWidget);
    BitBtnInfo^.ImageWidget := ImageWidget;
  end;
end;

class procedure TGtk2WSBitBtn.UpdateMargin(const ABitBtn: TCustomBitBtn;
  const AAlignWidget: PGtkAlignment; const AMargin: Integer);
begin
  if AMargin < 0 then
    gtk_alignment_set (AAlignWidget, 0.5, 0.5, 0.0, 0.0)
  else
  begin
    case ABitBtn.Layout of
      blGlyphLeft:
        begin
          gtk_alignment_set(AAlignWidget, 0, 0.5, 0, 0);
          gtk_alignment_set_padding(AAlignWidget, 0, 0, AMargin, 0);
        end;
      blGlyphRight:
        begin
          gtk_alignment_set(AAlignWidget, 1, 0.5, 0, 0);
          gtk_alignment_set_padding(AAlignWidget, 0, 0, 0, AMargin);
        end;
      blGlyphTop:
        begin
          gtk_alignment_set(AAlignWidget, 0.5, 0, 0, 0);
          gtk_alignment_set_padding(AAlignWidget, AMargin, 0, 0, 0);
        end;
      blGlyphBottom:
        begin
          gtk_alignment_set(AAlignWidget, 0.5, 1, 0, 0);
          gtk_alignment_set_padding(AAlignWidget, 0, AMargin, 0, 0);
        end;
    end;
  end;
end;

class procedure TGtk2WSBitBtn.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSButton.SetCallbacks(AGtkWidget, AWidgetInfo);

  SignalConnect(AGtkWidget, 'state-changed', @GtkWSBitBtn_StateChanged, AWidgetInfo);
end;

class procedure TGtk2WSBitBtn.UpdateImageWidget(ImageWidget: PGtkImage; Bitmap: TBitmap);
var
  GDIObject: PGDIObject;
  Pixbuf: PGdkPixbuf;
  Mask: PGdkBitmap;
begin
  GDIObject := {%H-}PGDIObject(Bitmap.Handle);
  Mask := nil;
  Pixbuf := nil;
  if GDIObject^.GDIBitmapType = gbPixbuf then
    Pixbuf := GDIObject^.GDIPixbufObject
  else
    Mask := CreateGdkMaskBitmap(Bitmap.Handle, Bitmap.MaskHandle);

  if Pixbuf <> nil then
    gtk_image_set_from_pixbuf(ImageWidget, Pixbuf)
  else
    gtk_image_set_from_pixmap(ImageWidget, GDIObject^.GDIPixmapObject.Image, Mask);

  if Mask <> nil then
    g_object_unref(Mask);
end;

class procedure TGtk2WSBitBtn.UpdateLabelFont(LabelWidget: PGtkWidget; Font: TFont);
begin
  Gtk2WidgetSet.SetWidgetColor(LabelWidget, Font.Color, clNone,
    [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
  Gtk2WidgetSet.SetWidgetFont(LabelWidget, Font);
end;


end.
