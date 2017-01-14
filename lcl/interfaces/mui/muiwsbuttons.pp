{
 *****************************************************************************
 *                              MUIWSButtons.pp                          *
 *                              --------------                               *
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
unit MUIWSButtons;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  // LCL
  Buttons, LCLType, Controls,
  MuiBaseUnit, MuiStdCtrls,
  // Widgetset
  muidrawing, tagsparamshelper,
  WSButtons, WSLCLClasses;

type

  { TMUIWSBitBtn }

  TMUIWSBitBtn = class(TWSBitBtn)
  private
  protected
  public
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); override;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); override;
    class procedure SetMargin(const ABitBtn: TCustomBitBtn; const AValue: Integer); override;
    class procedure SetSpacing(const ABitBtn: TCustomBitBtn; const AValue: Integer); override;
  end;

  { TMUIWSSpeedButton }

  TMUIWSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
  published
    {class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); override;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); override;
    class procedure SetMargin(const ABitBtn: TCustomBitBtn; const AValue: Integer); override;
    class procedure SetSpacing(const ABitBtn: TCustomBitBtn; const AValue: Integer); override;}
  end;


implementation

uses
  dos, mui;


class function TMUIWSBitBtn.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  MuiButton: TMuiBitBtn;
  Tags: TATagList;
begin
  Tags.AddTags([
      MUIA_InputMode, MUIV_InputMode_RelVerify,
      MUIA_ShowSelState, 1,
      MUIA_Frame, MUIV_Frame_Button,
      MUIA_Background, MUII_ButtonBack
      ]);
  MuiButton := TMuiBitBtn.Create(LCLGroupClass, Tags);
  MuiButton.MUIDrawing := True;
  //Create([PChar(AParams.Caption)]);
  With MuiButton do
  begin
    Left := AParams.X;
    Top := AParams.Y;
    Width := AParams.Width;
    Height := AParams.Height;
    PasObject := AWinControl;
  end;

  if AWinControl.Parent <> NIL then
  begin
    MuiButton.Parent := TMuiObject(AWinControl.Parent.Handle);
  end;
  //
  Result := TLCLIntfHandle(MuiButton);
end;

class procedure TMUIWSBitBtn.GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin

end;

class procedure TMUIWSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph);
var
  MuiButton: TMuiBitBtn;
begin
  MuiButton := TMuiBitBtn(ABitBtn.Handle);
  if Assigned(MuiButton) then
  begin
    MUIButton.FBitmap := TMUIBitmap(AValue.Glyph.Handle);
  end;

end;

class procedure TMUIWSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout);
var
  MuiButton: TMuiBitBtn;
begin
  MuiButton := TMuiBitBtn(ABitBtn.Handle);
  if Assigned(MuiButton) then
  begin
    MUIButton.FLayout := AValue;
  end;
end;

class procedure TMUIWSBitBtn.SetMargin(const ABitBtn: TCustomBitBtn; const AValue: Integer);
var
  MuiButton: TMuiBitBtn;
begin
  MuiButton := TMuiBitBtn(ABitBtn.Handle);
  if Assigned(MuiButton) then
  begin
    MUIButton.FMargin := AValue;
  end;
end;

class procedure TMUIWSBitBtn.SetSpacing(const ABitBtn: TCustomBitBtn; const AValue: Integer);
var
  MuiButton: TMuiBitBtn;
begin
  MuiButton := TMuiBitBtn(ABitBtn.Handle);
  if Assigned(MuiButton) then
  begin
    MUIButton.FSpacing := AValue;
  end;
end;

(*

class function TMUIWSSpeedButton.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  MuiButton: TMuiBitBtn;
  TagList: TTagsList;
begin
  AddTags(TagList, [
      LongInt(MUIA_InputMode), MUIV_InputMode_RelVerify,
      LongInt(MUIA_ShowSelState), 1,
      LongInt(MUIA_Frame), MUIV_Frame_Button,
      LongInt(MUIA_Background), MUII_ButtonBack]);
  MuiButton := TMuiBitBtn.Create(LCLGroupClass, GetTagPtr(TagList));
  MuiButton.MUIDrawing := True;
  //Create([PChar(AParams.Caption)]);
  With MuiButton do
  begin
    Left := AParams.X;
    Top := AParams.Y;
    Width := AParams.Width;
    Height := AParams.Height;
    PasObject := AWinControl;
  end;

  if AWinControl.Parent <> NIL then
  begin
    MuiButton.Parent := TMuiObject(AWinControl.Parent.Handle);
  end;
  //
  Result := TLCLIntfHandle(MuiButton);
end;

class procedure TMUIWSSpeedButton.GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin

end;

class procedure TMUIWSSpeedButton.SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph);
var
  MuiButton: TMuiBitBtn;
  Bit: TMUIBitmap;
begin
  MuiButton := TMuiBitBtn(ABitBtn.Handle);
  if Assigned(MuiButton) then
  begin
    MUIButton.FBitmap := TMUIBitmap(AValue.Glyph.Handle);
  end;

end;

class procedure TMUIWSSpeedButton.SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout);
var
  MuiButton: TMuiBitBtn;
begin
  MuiButton := TMuiBitBtn(ABitBtn.Handle);
  if Assigned(MuiButton) then
  begin
    MUIButton.FLayout := AValue;
  end;
end;

class procedure TMUIWSSpeedButton.SetMargin(const ABitBtn: TCustomBitBtn; const AValue: Integer);
var
  MuiButton: TMuiBitBtn;
begin
  MuiButton := TMuiBitBtn(ABitBtn.Handle);
  if Assigned(MuiButton) then
  begin
    MUIButton.FMargin := AValue;
  end;
end;

class procedure TMUIWSSpeedButton.SetSpacing(const ABitBtn: TCustomBitBtn; const AValue: Integer);
var
  MuiButton: TMuiBitBtn;
begin
  MuiButton := TMuiBitBtn(ABitBtn.Handle);
  if Assigned(MuiButton) then
  begin
    MUIButton.FSpacing := AValue;
  end;
end; *)


initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomSpeedButton, TMUIWSSpeedButton);
////////////////////////////////////////////////////
end.
