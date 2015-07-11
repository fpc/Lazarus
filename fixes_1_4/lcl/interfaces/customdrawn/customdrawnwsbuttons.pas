{
 *****************************************************************************
 *                              QtWSButtons.pp                               * 
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit customdrawnwsbuttons;

{$mode objfpc}{$H+}

interface

{$I customdrawndefines.inc}

uses
  // RTL
  SysUtils, Types,
  // LCL
  Controls, LCLType, Forms, InterfaceBase, Buttons, Graphics, GraphType,
  customdrawncontrols, StdCtrls,
  // Widgetset
  WSProc, WSButtons, WSLCLClasses, CustomDrawnWsControls, customdrawnproc,
  customdrawnprivate;

type

  { TCDWSBitBtn }

  TCDWSBitBtn = class(TWSBitBtn)
  public
    class procedure InjectCDControl(const AWinControl: TWinControl; var ACDControlField: TCDControl);
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); override;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); override;
  end;

  { TCDWSSpeedButton }

  TCDWSSpeedButton = class(TWSSpeedButton)
  published
  end;


implementation

{ TCDWSBitBtn }

class procedure TCDWSBitBtn.InjectCDControl(const AWinControl: TWinControl;
  var ACDControlField: TCDControl);
begin
  TCDIntfButton(ACDControlField).LCLControl := TCustomButton(AWinControl);
  ACDControlField.Caption := AWinControl.Caption;
  ACDControlField.Parent := AWinControl;
  ACDControlField.Align := alClient;
end;

class function TCDWSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  lCDWinControl: TCDWinControl;
begin
  Result := TCDWSWinControl.CreateHandle(AWinControl, AParams);
  lCDWinControl := TCDWinControl(Result);
  lCDWinControl.CDControl := TCDIntfButton.Create(AWinControl);
end;

class procedure TCDWSBitBtn.DestroyHandle(const AWinControl: TWinControl);
var
  lCDWinControl: TCDWinControl;
begin
  lCDWinControl := TCDWinControl(AWinControl.Handle);
  lCDWinControl.CDControl.Free;
  lCDWinControl.Free;
end;

class procedure TCDWSBitBtn.ShowHide(const AWinControl: TWinControl);
var
  lCDWinControl: TCDWinControl;
begin
  lCDWinControl := TCDWinControl(AWinControl.Handle);

  TCDWSWinControl.ShowHide(AWinControl);

  if not lCDWinControl.CDControlInjected then
  begin
    InjectCDControl(AWinControl, lCDWinControl.CDControl);
    lCDWinControl.CDControlInjected := True;
  end;
end;

{------------------------------------------------------------------------------
  Function: TCDWSBitBtn.SetGlyph
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph);
var
  lCDWinControl: TCDWinControl;
  lCDControl: TCDIntfButton;
begin
  //  if not WSCheckHandleAllocated(ABitBtn, 'SetGlyph') then
  //    Exit;

  lCDWinControl := TCDWinControl(ABitBtn.Handle);
  lCDControl := TCDIntfButton(lCDWinControl.CDControl);

  lCDControl.Glyph.Assign(AValue.Glyph);
end;

class procedure TCDWSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
begin
{  if not WSCheckHandleAllocated(ABitBtn, 'SetLayout') then
    Exit;
  TQtBitBtn(ABitBtn.Handle).GlyphLayout := Ord(ABitBtn.Layout);
  if TQtBitBtn(ABitBtn.Handle).getVisible then
    TQtBitBtn(ABitBtn.Handle).Update(nil);}
end;

end.
