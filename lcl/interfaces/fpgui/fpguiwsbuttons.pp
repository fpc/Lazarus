{ $Id: FpGuiwsbuttons.pp 5682 2004-07-15 10:43:39Z mattias $}
{
 *****************************************************************************
 *                              FpGuiWSButtons.pp                               * 
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.LCL, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit FpGuiWSButtons;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  fpguiwsprivate, fpg_main, fpg_button,
  // RTL
  SysUtils,
  // LCL
  Buttons, LCLType, Controls, Graphics, GraphType, ImgList,
  // Widgetset
  WSButtons, WSLCLClasses;

type

  { TFpGuiWSBitBtn }

  TFpGuiWSBitBtn = class(TWSBitBtn)
  private
  protected
    FButImageName: string;
  public
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); override;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout);
      override;
  end;

  { TFpGuiWSSpeedButton }

  TFpGuiWSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
  published
  end;


implementation

{ TFpGuiWSBitBtn }

class function TFpGuiWSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TFPGUIPrivateButton.Create(AWinControl, AParams));
end;

class procedure TFpGuiWSBitBtn.DestroyHandle(const AWinControl: TWinControl);
begin
  TFPGUIPrivateButton(AWinControl.Handle).Free;
  AWinControl.Handle := 0;
end;

class procedure TFpGuiWSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonGlyph);
var
  lButton: TFPGUIPrivateButton;
  lBitmap: TBitmap=nil;
  fpgImage: TfpgImage;
  lButImageName: string;
  lButtonState: TButtonState;
  lIndex: integer;
  lEffect: TGraphicsDrawEffect;
  lImageRes: TScaledImageListResolution;
begin
  if not Assigned(AValue.Images) then exit;
  lButton := TFPGUIPrivateButton(ABitBtn.Handle);
  lButtonState := bsUp;

  AValue.GetImageIndexAndEffect(lButtonState, ABitBtn.Font.PixelsPerInch,
        ABitBtn.GetCanvasScaleFactor, lImageRes, lIndex, lEffect);

  lBitmap:=TBitmap.Create;
  AValue.Images.GetBitmap(lIndex,lBitmap);
  fpgImage:=TfpgImage.Create;

  fpgImage.AllocateImage(32,lBitmap.Width,lBitmap.Height);
  move(lBitmap.RawImage.Data^,fpgImage.ImageData^,fpgImage.ImageDataSize);

  fpgImage.CreateMaskFromSample(0,0);
  fpgImage.UpdateImage;
  lBitmap.Free;
  if lButton.Button.ImageName<>'' then begin
    fpgImages.DeleteImage(lButton.Button.ImageName,true);
  end;
  lButImageName:='lcl.bitbtn.'+IntToStr(PtrInt(ABitBtn.Handle));
  fpgImages.AddImage(lButImageName,fpgImage);
  lButton.Button.ImageName:=lButImageName;
  lButton.Button.ShowImage:=true;
end;

class procedure TFpGuiWSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
var
  lButton: TFPGUIPrivateButton;
begin
  lButton := TFPGUIPrivateButton(ABitBtn.Handle);
  case AValue of
    blGlyphLeft: lButton.Button.ImageLayout:=ilImageLeft;
    blGlyphRight: lButton.Button.ImageLayout:=ilImageRight;
    blGlyphTop: lButton.Button.ImageLayout:=ilImageTop;
    blGlyphBottom: lButton.Button.ImageLayout:=ilImageBottom;
  end;
end;



initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomBitBtn, TFpGuiWSBitBtn);
//  RegisterWSComponent(TCustomSpeedButton, TFpGuiWSSpeedButton);
////////////////////////////////////////////////////
end.
