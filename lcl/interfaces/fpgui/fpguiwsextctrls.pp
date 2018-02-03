{ $Id: FpGuiwsextctrls.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                              FpGuiWSExtCtrls.pp                              * 
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.LCL, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit FpGuiWSExtCtrls;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  fpguiwsprivate,
  // LCL
  Classes,
  ExtCtrls, Controls, LCLType,
  // Widgetset
  WSExtCtrls, WSLCLClasses;

type

  { TFpGuiWSPage }

  TFpGuiWSPage = class(TWSPage)
  private
  protected
  public
  end;

  { TFpGuiWSNotebook }

  TFpGuiWSNotebook = class(TWSNotebook)
  private
  protected
  public
  end;

  { TFpGuiWSShape }

  TFpGuiWSShape = class(TWSShape)
  private
  protected
  public
  end;

  { TFpGuiWSCustomSplitter }

  TFpGuiWSCustomSplitter = class(TWSCustomSplitter)
  private
  protected
  public
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

  { TFpGuiWSSplitter }

  TFpGuiWSSplitter = class(TWSSplitter)
  private
  protected
  public
  end;

  { TFpGuiWSPaintBox }

  TFpGuiWSPaintBox = class(TWSPaintBox)
  private
  protected
  public
  end;

  { TFpGuiWSCustomImage }

  TFpGuiWSCustomImage = class(TWSCustomImage)
  private
  protected
  public
  end;

  { TFpGuiWSImage }

  TFpGuiWSImage = class(TWSImage)
  private
  protected
  public
  end;

  { TFpGuiWSBevel }

  TFpGuiWSBevel = class(TWSBevel)
  private
  protected
  public
  end;

  { TFpGuiWSCustomRadioGroup }

  TFpGuiWSCustomRadioGroup = class(TWSCustomRadioGroup)
  private
  protected
  public
  end;

  { TFpGuiWSRadioGroup }

  TFpGuiWSRadioGroup = class(TWSRadioGroup)
  private
  protected
  public
  end;

  { TFpGuiWSCustomCheckGroup }

  TFpGuiWSCustomCheckGroup = class(TWSCustomCheckGroup)
  private
  protected
  public
  end;

  { TFpGuiWSCheckGroup }

  TFpGuiWSCheckGroup = class(TWSCheckGroup)
  private
  protected
  public
  end;

  { TFpGuiWSCustomLabeledEdit }

  TFpGuiWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  private
  protected
  public
  end;

  { TFpGuiWSLabeledEdit }

  TFpGuiWSLabeledEdit = class(TWSLabeledEdit)
  private
  protected
  public
  end;

  { TFpGuiWSCustomPanel }

  TFpGuiWSCustomPanel = class(TWSCustomPanel)
  private
  protected
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

  { TFpGuiWSPanel }

  TFpGuiWSPanel = class(TWSPanel)
  private
  protected
  public
  end;

  { TFpGuiWSCustomTrayIcon }

  TFpGuiWSCustomTrayIcon = class(TWSCustomTrayIcon)
  published
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
  end;

implementation

uses
  fpg_panel;

{ TFpGuiWSCustomSplitter }

class function TFpGuiWSCustomSplitter.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TFPGUIPrivateSplitter.Create(AWinControl, AParams));
end;

class procedure TFpGuiWSCustomSplitter.DestroyHandle(
  const AWinControl: TWinControl);
begin
  inherited DestroyHandle(AWinControl);
end;

{ TFpGuiWSCustomTrayIcon }

class function TFpGuiWSCustomTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  Result:=inherited Hide(ATrayIcon);
end;

class function TFpGuiWSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  Result:=inherited Show(ATrayIcon);
end;

class procedure TFpGuiWSCustomTrayIcon.InternalUpdate(
  const ATrayIcon: TCustomTrayIcon);
begin
  inherited InternalUpdate(ATrayIcon);
end;

class function TFpGuiWSCustomTrayIcon.ShowBalloonHint(
  const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  Result:=inherited ShowBalloonHint(ATrayIcon);
end;

class function TFpGuiWSCustomTrayIcon.GetPosition(
  const ATrayIcon: TCustomTrayIcon): TPoint;
begin
  Result:=inherited GetPosition(ATrayIcon);
end;

{ TFpGuiWSCustomPanel }

class function TFpGuiWSCustomPanel.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  lPanel: TfpgPanel;
begin
  Result := TLCLIntfHandle(TFPGUIPrivateCustomPanel.Create(AWinControl, AParams));

  lPanel := TFPGUIPrivateCustomPanel(Result).Panel;
  lPanel.Style := bsFlat;

  (*
  case TCustomPanel(AWinControl).BevelOuter of
    bvLowered:  begin
      lPanel.Style := bsLowered;
    end;
    bvRaised:   begin
      lPanel.Style := bsRaised;
    end;
    bvNone,
    bvSpace:    lPanel.Style := bsFlat;
  end;
  *)
end;

class procedure TFpGuiWSCustomPanel.DestroyHandle(const AWinControl: TWinControl);
begin
  TFPGUIPrivateCustomPanel(AWinControl.Handle).Free;
  AWinControl.Handle := 0;
end;

end.
