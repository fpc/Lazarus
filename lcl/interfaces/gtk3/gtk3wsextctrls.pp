{
 *****************************************************************************
 *                             Gtk3WSExtCtrls.pp                             *
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk3WSExtCtrls;
{$mode objfpc}{$H+}
{$I gtk3defines.inc}

interface

uses
  // LCL
  LCLProc, ExtCtrls, Classes, Controls, SysUtils, types, Graphics, LCLType,
  // widgetset
  WSExtCtrls, WSLCLClasses,
  Gtk3WSControls;

type

  { TGtk3WSPage }

  TGtk3WSPage = class(TWSPage)
  published
  end;

  { TGtk3WSNotebook }

  TGtk3WSNotebook = class(TWSNotebook)
  published
  end;

  { TGtk3WSShape }

  TGtk3WSShape = class(TWSShape)
  published
  end;

  { TGtk3WSCustomSplitter }

  TGtk3WSCustomSplitter = class(TWSCustomSplitter)
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TGtk3WSSplitter }

  TGtk3WSSplitter = class(TWSSplitter)
  published
  end;

  { TGtk3WSPaintBox }

  TGtk3WSPaintBox = class(TWSPaintBox)
  published
  end;

  { TGtk3WSCustomImage }

  TGtk3WSCustomImage = class(TWSCustomImage)
  published
  end;

  { TGtk3WSImage }

  TGtk3WSImage = class(TWSImage)
  published
  end;

  { TGtk3WSBevel }

  TGtk3WSBevel = class(TWSBevel)
  published
  end;

  { TGtk3WSCustomRadioGroup }

  TGtk3WSCustomRadioGroup = class(TWSCustomRadioGroup)
  published
  end;

  { TGtk3WSRadioGroup }

  TGtk3WSRadioGroup = class(TWSRadioGroup)
  published
  end;

  { TGtk3WSCustomCheckGroup }

  TGtk3WSCustomCheckGroup = class(TWSCustomCheckGroup)
  published
  end;

  { TGtk3WSCheckGroup }

  TGtk3WSCheckGroup = class(TWSCheckGroup)
  published
  end;

  { TGtk3WSCustomLabeledEdit }

  TGtk3WSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  published
  end;

  { TGtk3WSLabeledEdit }

  TGtk3WSLabeledEdit = class(TWSLabeledEdit)
  published
  end;

  { TGtk3WSCustomPanel }

  TGtk3WSCustomPanel = class(TWSCustomPanel)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
  end;

  { TGtk3WSPanel }

  TGtk3WSPanel = class(TWSPanel)
  published
  end;

  { TGtk3WSCustomTrayIcon }

  TGtk3WSCustomTrayIcon = class(TWSCustomTrayIcon)
  published
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
  end;

implementation

uses
  gtk3widgets;

{ TGtk3WSCustomSplitter }

class function TGtk3WSCustomSplitter.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  ASplitter: TGtk3Splitter;
begin
  ASplitter := TGtk3Splitter.Create(AWinControl, AParams);
  Result := TLCLIntfHandle(ASplitter);
end;

{ TGtk3WSCustomPanel }

class function TGtk3WSCustomPanel.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  APanel: TGtk3Panel;
begin
  APanel := TGtk3Panel.Create(AWinControl, AParams);

  APanel.BorderStyle := TCustomControl(AWinControl).BorderStyle;
  APanel.BevelInner := TCustomPanel(AWinControl).BevelInner;
  APanel.BevelOuter := TCustomPanel(AWinControl).BevelOuter;
  APanel.Text := AWinControl.Caption;

  Result := TLCLIntfHandle(APanel);
end;

class procedure TGtk3WSCustomPanel.SetBorderStyle(
  const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
var
  APanel: TGtk3Panel;
begin
  APanel := TGtk3Panel(AWinControl.Handle);
  APanel.BorderStyle := ABorderStyle;
end;

{ TGtk3WSCustomTrayIcon }

class function TGtk3WSCustomTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon
  ): Boolean;
begin
  Result:=inherited Hide(ATrayIcon);
end;

class function TGtk3WSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon
  ): Boolean;
begin
  Result:=inherited Show(ATrayIcon);
end;

class procedure TGtk3WSCustomTrayIcon.InternalUpdate(
  const ATrayIcon: TCustomTrayIcon);
begin
  inherited InternalUpdate(ATrayIcon);
end;

class function TGtk3WSCustomTrayIcon.GetPosition(
  const ATrayIcon: TCustomTrayIcon): TPoint;
begin
  Result:=inherited GetPosition(ATrayIcon);
end;

end.
