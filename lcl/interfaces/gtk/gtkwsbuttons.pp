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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  GLib2, Gtk2,
  {$ELSE}
  GLib, Gtk, 
  {$ENDIF}
  // LCL
  Buttons, Classes, LCLType, LMessages, Controls, Graphics,
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
    class function CreateHandle(const AComponent: TComponent; const AParams: TCreateParams): THandle; override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TGtkWSBitBtn }

  TGtkWSBitBtn = class(TWSBitBtn)
  private
  protected
    class procedure UpdateLayout(const AInfo: PBitBtnWidgetInfo; const ALayout: TButtonLayout; const AMargin: Integer);
    class procedure UpdateMargin(const AInfo: PBitBtnWidgetInfo; const ALayout: TButtonLayout; const AMargin: Integer);
  public
    class function CreateHandle(const AComponent: TComponent; const AParams: TCreateParams): THandle; override;
    class procedure SetGlyph(const ABitBtn: TBitBtn; const AValue: TBitmap); override;
    class procedure SetLayout(const ABitBtn: TBitBtn; const AValue: TButtonLayout); override;
    class procedure SetMargin(const ABitBtn: TBitBtn; const AValue: Integer); override;
    class procedure SetSpacing(const ABitBtn: TBitBtn; const AValue: Integer); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
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


function TGtkWSButton.CreateHandle(const AComponent: TComponent; const AParams: TCreateParams): THandle; 
var
  Button: TButton;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  Button := AComponent as TButton;

  Result := THandle(gtk_button_new_with_label('button'));
  if Result = 0 then Exit;

  WidgetInfo := CreateWidgetInfo(Pointer(Result), Button, AParams);
  
  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(PGtkWidget(Result), @Allocation);

  SetCallbacks(PGtkWidget(Result), WidgetInfo);
end;

function TGtkWSButton.GetText(const AWinControl: TWinControl; var AText: String): Boolean; 
begin             
  // The button text is static, so let the LCL fallback to FCaption
  Result := False;
end;

procedure TGtkWSButton.SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin        
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));

  SignalConnect(AGtkWidget, 'clicked', @GtkWSButton_Clicked, AWidgetInfo);
end;

procedure TGtkWSButton.SetText(const AWinControl: TWinControl; const AText: String); 
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
  
  GtkWidgetSet.SetLabelCaption(LblWidget, AText, AWinControl, PGtkWidget(BtnWidget), 'clicked');   
end;

{ TGtkWSBitBtn }

{
 The interiour of TBitBtn is created with a 4X4 table
 Depending in how the image and label are aligned, only a 
 columns or rows are used (like a 4x1 or 1x4 table). 
 This wat the table doesn't have to be recreated on changes.
 So there are 4 positions 0, 1, 2, 3.
 Positions 1 and 2 are used for the label and image.
 Since this is always the case, spacing can be implenented
 by setting the spacing of row/col 1
 To get a margin, a gtkInvisible is needed for bottom and 
 right, so the invisible is always in position 3. 
} 

function TGtkWSBitBtn.CreateHandle(const AComponent: TComponent; const AParams: TCreateParams): THandle; 
var
  BitBtn: TBitBtn;
  WidgetInfo: PWidgetInfo;
  BitBtnInfo: PBitBtnWidgetInfo;
  Allocation: TGTKAllocation;
begin
  BitBtn := AComponent as TBitBtn;

  Result := THandle(gtk_button_new);
  if Result = 0 then Exit;

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
  gtk_table_attach(BitBtnInfo^.TableWidget, BitBtnInfo^.LabelWidget, 2, 3, 0, 4, 0, 0, 0, 0);

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

procedure TGtkWSBitBtn.SetGlyph(const ABitBtn: TBitBtn; const AValue: TBitmap); 
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
    BitBtnInfo^.ImageWidget := gtk_pixmap_new(GDIObject^.GDIPixmapObject, GDIObject^.GDIBitmapMaskObject);
    gtk_widget_show(BitBtnInfo^.ImageWidget);
    UpdateLayout(BitBtnInfo, ABitBtn.Layout, ABitBtn.Margin);
  end
  else begin
    gtk_pixmap_set(BitBtnInfo^.ImageWidget, GDIObject^.GDIPixmapObject, GDIObject^.GDIBitmapMaskObject);
  end;
end;

procedure TGtkWSBitBtn.SetLayout(const ABitBtn: TBitBtn; const AValue: TButtonLayout); 
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

procedure TGtkWSBitBtn.SetMargin(const ABitBtn: TBitBtn; const AValue: Integer); 
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

procedure TGtkWSBitBtn.SetSpacing(const ABitBtn: TBitBtn; const AValue: Integer); 
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

procedure TGtkWSBitBtn.SetText(const AWinControl: TWinControl; const AText: String); 
var
  WidgetInfo: PWidgetInfo;
  BitBtnInfo: PBitBtnWidgetInfo;
begin          
  if not WSCheckHandleAllocated(AWincontrol, 'SetText')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(AWinControl.Handle));
  BitBtnInfo := WidgetInfo^.UserData;                       
  if BitBtnInfo^.LabelWidget = nil then Exit;  

  GtkWidgetSet.SetLabelCaption(BitBtnInfo^.LabelWidget, AText, AWinControl, WidgetInfo^.CoreWidget, 'clicked');   
end;

procedure TGtkWSBitBtn.UpdateLayout(const AInfo: PBitBtnWidgetInfo; const ALayout: TButtonLayout; const AMargin: Integer);
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
      then gtk_table_attach(AInfo^.TableWidget, AInfo^.ImageWidget, 1, 2, 1, 3, 0, 0, 0, 0);
      gtk_table_attach(AInfo^.TableWidget, AInfo^.LabelWidget, 2, 3, 1, 3, 0, 0, 0, 0);
    end;
    blGlyphRight: begin
      gtk_table_attach(AInfo^.TableWidget, AInfo^.LabelWidget, 1, 2, 1, 3, 0, 0, 0, 0);
      if AInfo^.ImageWidget <> nil
      then gtk_table_attach(AInfo^.TableWidget, AInfo^.ImageWidget, 2, 3, 1, 3, 0, 0, 0, 0);
      if AInfo^.SpaceWidget <> nil
      then gtk_table_attach(AInfo^.TableWidget, AInfo^.SpaceWidget, 3, 4, 1, 3, 0, 0, 0, 0);
    end;
    blGlyphTop: begin
      if AInfo^.ImageWidget <> nil
      then gtk_table_attach(AInfo^.TableWidget, AInfo^.ImageWidget, 1, 3, 1, 2, 0, 0, 0, 0);
      gtk_table_attach(AInfo^.TableWidget, AInfo^.LabelWidget, 1, 3, 2, 3, 0, 0, 0, 0);
    end; 
    blGlyphBottom: begin
      gtk_table_attach(AInfo^.TableWidget, AInfo^.LabelWidget, 1, 3, 1, 2, 0, 0, 0, 0);
      if AInfo^.ImageWidget <> nil
      then gtk_table_attach(AInfo^.TableWidget, AInfo^.ImageWidget, 1, 3, 2, 3, 0, 0, 0, 0);
      if AInfo^.SpaceWidget <> nil
      then gtk_table_attach(AInfo^.TableWidget, AInfo^.SpaceWidget, 1, 3, 3, 4, 0, 0, 0, 0);
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

procedure TGtkWSBitBtn.UpdateMargin(const AInfo: PBitBtnWidgetInfo; const ALayout: TButtonLayout; const AMargin: Integer);
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
  RegisterWSComponent(TButton, TGtkWSButton);
  RegisterWSComponent(TBitBtn, TGtkWSBitBtn); // register it to fallback to default
//  RegisterWSComponent(TSpeedButton, TGtkWSSpeedButton);
////////////////////////////////////////////////////
end.
