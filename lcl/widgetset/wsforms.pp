{ $Id$}
{
 *****************************************************************************
 *                                WSForms.pp                                 * 
 *                                ----------                                 * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSForms;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Graphics, Controls, Forms, LCLType,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, WSFactory;

type
  { TWSScrollingWinControl }

  TWSScrollingWinControlClass = class of TWSScrollingWinControl;
  TWSScrollingWinControl = class(TWSWinControl_CallWS)
  private class var
    FWSScrollingWinControl_Impl: TWSScrollingWinControlClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
    // procedure ScrollBy is moved to TWSWinControl.
  end;

  { TWSScrollBox }

  TWSScrollBox = class(TWSScrollingWinControl)
  published
  end;

  { TWSCustomFrame }

  TWSCustomFrame = class(TWSScrollingWinControl)
  published
  end;

  { TWSFrame }

  TWSFrame = class(TWSCustomFrame)
  published
  end;

  { TWSCustomForm }

  TWSCustomFormClass = class of TWSCustomForm;
  TWSCustomForm = class(TWSScrollingWinControl)
  private class var
    FWSCustomForm_Impl: TWSCustomFormClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
    class procedure CloseModal(const ACustomForm: TCustomForm); virtual;
    class procedure SetAllowDropFiles(const AForm: TCustomForm; AValue: Boolean); virtual;
    class procedure SetAlphaBlend(const ACustomForm: TCustomForm; const AlphaBlend: Boolean;
      const Alpha: Byte); virtual;
    class procedure SetBorderIcons(const AForm: TCustomForm;
        const ABorderIcons: TBorderIcons); virtual;
    class procedure SetFormBorderStyle(const AForm: TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle); virtual;
    class procedure SetFormStyle(const AForm: TCustomform; const AFormStyle, AOldFormStyle: TFormStyle); virtual;
    class procedure SetIcon(const AForm: TCustomForm; const Small, Big: HICON); virtual;
    class procedure ShowModal(const ACustomForm: TCustomForm); virtual;
    class procedure SetModalResult(const ACustomForm: TCustomForm; ANewValue: TModalResult); virtual;
    class procedure SetRealPopupParent(const ACustomForm: TCustomForm;
      const APopupParent: TCustomForm); virtual;
    class procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); virtual;
    class procedure SetZPosition(const AWinControl: TWinControl; const APosition: TWSZPosition); virtual;
    class function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;

    {mdi support}
    class function ActiveMDIChild(const AForm: TCustomForm): TCustomForm; virtual;
    class function Cascade(const AForm: TCustomForm): Boolean; virtual;
    class function GetClientHandle(const AForm: TCustomForm): HWND; virtual;
    class function GetMDIChildren(const AForm: TCustomForm; AIndex: Integer): TCustomForm; virtual;
    class function Next(const AForm: TCustomForm): Boolean; virtual;
    class function Previous(const AForm: TCustomForm): Boolean; virtual;
    class function Tile(const AForm: TCustomForm): Boolean; virtual;
    class function MDIChildCount(const AForm: TCustomForm): Integer; virtual;
  end;

  { TWSCustomForm_CallWS }

  TWSCustomForm_CallWS = class(TWSCustomForm)
  public
    class procedure CloseModal(const ACustomForm: TCustomForm); override;
    class procedure SetAllowDropFiles(const AForm: TCustomForm; AValue: Boolean); override;
    class procedure SetAlphaBlend(const ACustomForm: TCustomForm; const AlphaBlend: Boolean;
      const Alpha: Byte); override;
    class procedure SetBorderIcons(const AForm: TCustomForm;
        const ABorderIcons: TBorderIcons); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetFormStyle(const AForm: TCustomform; const AFormStyle, AOldFormStyle: TFormStyle); override;
    class procedure SetIcon(const AForm: TCustomForm; const Small, Big: HICON); override;
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
    class procedure SetModalResult(const ACustomForm: TCustomForm; ANewValue: TModalResult); override;
    class procedure SetRealPopupParent(const ACustomForm: TCustomForm;
      const APopupParent: TCustomForm); override;
    class procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); override;
    class procedure SetZPosition(const AWinControl: TWinControl; const APosition: TWSZPosition); override;
    //class function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;

    {mdi support}
    {class function ActiveMDIChild(const AForm: TCustomForm): TCustomForm; virtual;
    class function Cascade(const AForm: TCustomForm): Boolean; virtual;
    class function GetClientHandle(const AForm: TCustomForm): HWND; virtual;
    class function GetMDIChildren(const AForm: TCustomForm; AIndex: Integer): TCustomForm; virtual;
    class function Next(const AForm: TCustomForm): Boolean; virtual;
    class function Previous(const AForm: TCustomForm): Boolean; virtual;
    class function Tile(const AForm: TCustomForm): Boolean; virtual;
    class function MDIChildCount(const AForm: TCustomForm): Integer; virtual;}
  end;

  { TWSForm }

  TWSFormClass = class of TWSForm;
  TWSForm = class(TWSCustomForm_CallWS)
  private class var
    FWSForm_Impl: TWSFormClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
  end;

  { TWSHintWindow }

  TWSHintWindowClass = class of TWSHintWindow;
  TWSHintWindow = class(TWSCustomForm_CallWS)
  private class var
    FWSHintWindow_Impl: TWSHintWindowClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
  end;

  { TWSScreen }

  TWSScreen = class(TWSLCLComponent)
  published
  end;

  { TWSApplicationProperties }

  TWSApplicationProperties = class(TWSLCLComponent)
  published
  end;

  { WidgetSetRegistration }

  procedure RegisterScrollingWinControl;
  procedure RegisterScrollBox;
  procedure RegisterCustomFrame;
  procedure RegisterCustomForm;
  procedure RegisterHintWindow;

implementation

{ TWSScrollingWinControl }

class function TWSScrollingWinControl.GetImplementation: TWSObjectClass;
begin
  Result:= FWSScrollingWinControl_Impl;
end;

class procedure TWSScrollingWinControl.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSScrollingWinControl_Impl := TWSScrollingWinControlClass(AImpl);
end;

{ TWSCustomForm }

class function TWSCustomForm.GetImplementation: TWSObjectClass;
begin
  Result:= FWSCustomForm_Impl;
end;

class procedure TWSCustomForm.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSCustomForm_Impl := TWSCustomFormClass(AImpl);
end;

class procedure TWSCustomForm.CloseModal(const ACustomForm: TCustomForm);
begin
end;

class procedure TWSCustomForm.SetAllowDropFiles(const AForm: TCustomForm;
  AValue: Boolean);
begin
end;

class procedure TWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
        const ABorderIcons: TBorderIcons);
begin
end;

class procedure TWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
begin
  // will be done in interface override
end;

class procedure TWSCustomForm.SetFormStyle(const AForm: TCustomform;
  const AFormStyle, AOldFormStyle: TFormStyle);
begin
end;
    
class procedure TWSCustomForm.SetIcon(const AForm: TCustomForm; const Small, Big: HICON);
begin
end;

class procedure TWSCustomForm.SetShowInTaskbar(const AForm: TCustomForm;
  const AValue: TShowInTaskbar);
begin
end;

class procedure TWSCustomForm.SetZPosition(const AWinControl: TWinControl; const APosition: TWSZPosition);
begin
end;

class function TWSCustomForm.GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor;
const
  DefColors: array[TDefaultColorType] of TColor = (
 { dctBrush } clForm,
 { dctFont  } clBtnText
  );
begin
  Result := DefColors[ADefaultColorType];
end;

class procedure TWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
end;

// This needs implementing only if the TWSCustomForm.ShowModal implementation
// is fully blocking (which it shouldn't be ideally)
class procedure TWSCustomForm.SetModalResult(const ACustomForm: TCustomForm;
  ANewValue: TModalResult);
begin
end;

class procedure TWSCustomForm.SetRealPopupParent(
  const ACustomForm: TCustomForm; const APopupParent: TCustomForm);
begin
end;

class procedure TWSCustomForm.SetAlphaBlend(const ACustomForm: TCustomForm;
  const AlphaBlend: Boolean; const Alpha: Byte);
begin
end;

{ mdi support }

class function TWSCustomForm.ActiveMDIChild(const AForm: TCustomForm
  ): TCustomForm;
begin
  Result := nil;
end;

class function TWSCustomForm.Cascade(const AForm: TCustomForm): Boolean;
begin
  Result := False;
end;

class function TWSCustomForm.GetClientHandle(const AForm: TCustomForm): HWND;
begin
  Result := 0;
end;

class function TWSCustomForm.GetMDIChildren(const AForm: TCustomForm;
  AIndex: Integer): TCustomForm;
begin
  Result := nil;
end;

class function TWSCustomForm.MDIChildCount(const AForm: TCustomForm): Integer;
begin
  Result := 0;
end;

class function TWSCustomForm.Next(const AForm: TCustomForm): Boolean;
begin
  Result := False;
end;

class function TWSCustomForm.Previous(const AForm: TCustomForm): Boolean;
begin
  Result := False;
end;

class function TWSCustomForm.Tile(const AForm: TCustomForm): Boolean;
begin
  Result := False;
end;

{ TWSCustomForm_CallWS }

class procedure TWSCustomForm_CallWS.CloseModal(const ACustomForm: TCustomForm);
begin
  FWSCustomForm_Impl.CloseModal(ACustomForm);
end;

class procedure TWSCustomForm_CallWS.SetAllowDropFiles(
  const AForm: TCustomForm; AValue: Boolean);
begin
  FWSCustomForm_Impl.SetAllowDropFiles(AForm, AValue);
end;

class procedure TWSCustomForm_CallWS.SetAlphaBlend(
  const ACustomForm: TCustomForm; const AlphaBlend: Boolean; const Alpha: Byte);
begin
  FWSCustomForm_Impl.SetAlphaBlend(ACustomForm, AlphaBlend, Alpha);
end;

class procedure TWSCustomForm_CallWS.SetBorderIcons(const AForm: TCustomForm;
  const ABorderIcons: TBorderIcons);
begin
  FWSCustomForm_Impl.SetBorderIcons(AForm, ABorderIcons);
end;

class procedure TWSCustomForm_CallWS.SetFormBorderStyle(
  const AForm: TCustomForm; const AFormBorderStyle: TFormBorderStyle);
begin
  FWSCustomForm_Impl.SetFormBorderStyle(AForm, AFormBorderStyle);
end;

class procedure TWSCustomForm_CallWS.SetFormStyle(const AForm: TCustomform;
  const AFormStyle, AOldFormStyle: TFormStyle);
begin
  FWSCustomForm_Impl.SetFormStyle(AForm, AFormStyle, AOldFormStyle);
end;

class procedure TWSCustomForm_CallWS.SetIcon(const AForm: TCustomForm;
  const Small, Big: HICON);
begin
  FWSCustomForm_Impl.SetIcon(AForm, Small, Big);
end;

class procedure TWSCustomForm_CallWS.ShowModal(const ACustomForm: TCustomForm);
begin
  FWSCustomForm_Impl.ShowModal(ACustomForm);
end;

class procedure TWSCustomForm_CallWS.SetModalResult(
  const ACustomForm: TCustomForm; ANewValue: TModalResult);
begin
  FWSCustomForm_Impl.SetModalResult(ACustomForm, ANewValue);
end;

class procedure TWSCustomForm_CallWS.SetRealPopupParent(
  const ACustomForm: TCustomForm; const APopupParent: TCustomForm);
begin
  FWSCustomForm_Impl.SetRealPopupParent(ACustomForm, APopupParent);
end;

class procedure TWSCustomForm_CallWS.SetShowInTaskbar(const AForm: TCustomForm;
  const AValue: TShowInTaskbar);
begin
  FWSCustomForm_Impl.SetShowInTaskbar(AForm, AValue);
end;

class procedure TWSCustomForm_CallWS.SetZPosition(
  const AWinControl: TWinControl; const APosition: TWSZPosition);
begin
  FWSCustomForm_Impl.SetZPosition(AWinControl, APosition);
end;

{ TWSForm }

class function TWSForm.GetImplementation: TWSObjectClass;
begin
  Result:= FWSForm_Impl;
end;

class procedure TWSForm.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSForm_Impl := TWSFormClass(AImpl);
end;

{ TWSHintWindow }

class function TWSHintWindow.GetImplementation: TWSObjectClass;
begin
  Result:= FWSHintWindow_Impl;
end;

class procedure TWSHintWindow.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSHintWindow_Impl := TWSHintWindowClass(AImpl);
end;

{ WidgetSetRegistration }

procedure RegisterScrollingWinControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterScrollingWinControl;
//  if not WSRegisterScrollingWinControl then
//    RegisterWSComponent(TScrollingWinControl, TWSScrollingWinControl);
  Done := True;
end;

procedure RegisterScrollBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterScrollBox;
//  if not WSRegisterScrollBox then
//    RegisterWSComponent(TScrollBox, TWSScrollBox);
  Done := True;
end;

procedure RegisterCustomFrame;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomFrame;
//  if not WSRegisterCustomFrame then
//    RegisterWSComponent(TCustomFrame, TWSCustomFrame);
  Done := True;
end;

procedure RegisterCustomForm;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomForm;
//  if not WSRegisterCustomForm then
//    RegisterWSComponent(TCustomForm, TWSCustomForm);
  Done := True;
end;

procedure RegisterHintWindow;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterHintWindow;
//  if not WSRegisterHintWindow then
//    RegisterWSComponent(THintWindow, TWSHintWindow);
  Done := True;
end;

end.
