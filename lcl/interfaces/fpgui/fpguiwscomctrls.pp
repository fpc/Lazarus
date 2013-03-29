{ $Id: FpGuiwscomctrls.pp 5687 2004-07-16 21:49:00Z mattias $}
{
 *****************************************************************************
 *                              FpGuiWSComCtrls.pp                              * 
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
unit FpGuiWSComCtrls;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  fpguiwsprivate,
  // LCL
  Classes,
  ComCtrls, Controls, LCLType,
  // Widgetset
  WSComCtrls, WSLCLClasses;

type
  { TFpGuiWSCustomPage }

  TFpGuiWSCustomPage = class(TWSCustomPage)
  private
  protected
  public
  end;

  { TFpGuiWSCustomNotebook }

  TFpGuiWSCustomNotebook = class(TWSCustomTabControl)
  private
  protected
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

  { TFpGuiWSStatusBar }

  TFpGuiWSStatusBar = class(TWSStatusBar)
  private
  protected
  public
  end;

  { TFpGuiWSTabSheet }

  TFpGuiWSTabSheet = class(TWSTabSheet)
  private
  protected
  public
  end;

  { TFpGuiWSPageControl }

  TFpGuiWSPageControl = class(TWSPageControl)
  private
  protected
  public
  end;

  { TFpGuiWSCustomListView }

  TFpGuiWSCustomListView = class(TWSCustomListView)
  private
  protected
  public
  end;

  { TFpGuiWSListView }

  TFpGuiWSListView = class(TWSListView)
  private
  protected
  public
  end;

  { TFpGuiWSProgressBar }

  TFpGuiWSProgressBar = class(TWSProgressBar)
  private
  protected
  public
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
//    class procedure SetStyle(const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle); override;
  end;

  { TFpGuiWSCustomUpDown }

  TFpGuiWSCustomUpDown = class(TWSCustomUpDown)
  private
  protected
  public
  end;

  { TFpGuiWSUpDown }

  TFpGuiWSUpDown = class(TWSUpDown)
  private
  protected
  public
  end;

  { TFpGuiWSToolButton }

  TFpGuiWSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TFpGuiWSToolBar }

  TFpGuiWSToolBar = class(TWSToolBar)
  private
  protected
  public
  end;

  { TFpGuiWSTrackBar }

  TFpGuiWSTrackBar = class(TWSTrackBar)
  private
  protected
  public
  end;

  { TFpGuiWSCustomTreeView }

  TFpGuiWSCustomTreeView = class(TWSCustomTreeView)
  private
  protected
  public
  end;

  { TFpGuiWSTreeView }

  TFpGuiWSTreeView = class(TWSTreeView)
  private
  protected
  public
  end;


implementation

uses
  fpg_progressbar;

{ TFpGuiWSProgressBar }

class function TFpGuiWSProgressBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TFPGUIPrivateProgressBar.Create(AWinControl, AParams));
end;

class procedure TFpGuiWSProgressBar.DestroyHandle(const AWinControl: TWinControl);
begin
  TFPGUIPrivateProgressBar(AWinControl.Handle).Free;
  AWinControl.Handle := 0;
end;

class procedure TFpGuiWSProgressBar.ApplyChanges(
  const AProgressBar: TCustomProgressBar);
begin
  SetPosition(AProgressBar, AProgressBar.Position);
end;

class procedure TFpGuiWSProgressBar.SetPosition(
  const AProgressBar: TCustomProgressBar; const NewPosition: integer);
var
  lProgressBar: TfpgProgressBar;
begin
  lProgressBar := TFPGUIPrivateProgressBar(AProgressBar.Handle).ProgressBar;
  lProgressBar.Position := NewPosition;
end;

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


end.

