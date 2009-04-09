{ $Id: FpGuiwsextctrls.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                              FpGuiWSExtCtrls.pp                              * 
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

  { TFpGuiWSCustomPage }

  TFpGuiWSCustomPage = class(TWSCustomPage)
  private
  protected
  public
  end;

  { TFpGuiWSCustomNotebook }

  TFpGuiWSCustomNotebook = class(TWSCustomNotebook)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

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
  public
  end;

  { TFpGuiWSPanel }

  TFpGuiWSPanel = class(TWSPanel)
  private
  protected
  public
  end;

  { TFpGuiWSCustomTrayIcon }

  TFpGuiWSCustomTrayIcon = class(TWSCustomTrayIcon)
  public
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
  end;

implementation

{ TFpGuiWSCustomNotebook }

class function TFpGuiWSCustomNotebook.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
begin
  Result := TLCLIntfHandle(TFPGUIPrivatePageControl.Create(AWinControl, AParams));
end;

class procedure TFpGuiWSCustomNotebook.DestroyHandle(
  const AWinControl: TWinControl);
begin
  TFPGUIPrivatePageControl(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
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

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomPage, TFpGuiWSCustomPage);
  RegisterWSComponent(TCustomNotebook, TFpGuiWSCustomNotebook);
//  RegisterWSComponent(TPage, TFpGuiWSPage);
//  RegisterWSComponent(TNotebook, TFpGuiWSNotebook);
//  RegisterWSComponent(TShape, TFpGuiWSShape);
//  RegisterWSComponent(TCustomSplitter, TFpGuiWSCustomSplitter);
//  RegisterWSComponent(TSplitter, TFpGuiWSSplitter);
//  RegisterWSComponent(TPaintBox, TFpGuiWSPaintBox);
//  RegisterWSComponent(TCustomImage, TFpGuiWSCustomImage);
//  RegisterWSComponent(TImage, TFpGuiWSImage);
//  RegisterWSComponent(TBevel, TFpGuiWSBevel);
//  RegisterWSComponent(TCustomRadioGroup, TFpGuiWSCustomRadioGroup);
//  RegisterWSComponent(TRadioGroup, TFpGuiWSRadioGroup);
//  RegisterWSComponent(TCustomCheckGroup, TFpGuiWSCustomCheckGroup);
//  RegisterWSComponent(TCheckGroup, TFpGuiWSCheckGroup);
//  RegisterWSComponent(TBoundLabel, TFpGuiWSBoundLabel);
//  RegisterWSComponent(TCustomLabeledEdit, TFpGuiWSCustomLabeledEdit);
//  RegisterWSComponent(TLabeledEdit, TFpGuiWSLabeledEdit);
//  RegisterWSComponent(TCustomPanel, TFpGuiWSCustomPanel);
//  RegisterWSComponent(TPanel, TFpGuiWSPanel);
////////////////////////////////////////////////////
end.
