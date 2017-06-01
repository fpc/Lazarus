{
 *****************************************************************************
 *                               WSDialogs.pp                                * 
 *                               ------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSDialogs;

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
  LCLType, Dialogs,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, WSFactory;

type
  { TWSCommonDialog }

  TWSCommonDialogClass = class of TWSCommonDialog;
  TWSCommonDialog = class(TWSLCLComponent)
  public class var
    FWSCommonDialog_WSClass: TWSCommonDialogClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
    class function  CreateHandle(const ACommonDialog: TCommonDialog): THandle; virtual;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); virtual;
    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); virtual;
    class function QueryWSEventCapabilities(const ACommonDialog: TCommonDialog): TCDWSEventCapabilities; virtual;
  end;

  TWSCommonDialog_CallWS = class(TWSCommonDialog)
  published
    class function  CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
    class function QueryWSEventCapabilities(const ACommonDialog: TCommonDialog): TCDWSEventCapabilities; override;
  end;

  { TWSFileDialog }

  TWSFileDialogClass = class of TWSFileDialog;
  TWSFileDialog = class(TWSCommonDialog_CallWS)
  public class var
    FWSFileDialog_WSClass: TWSFileDialogClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
  end;

  { TWSOpenDialog }

  TWSOpenDialogClass = class of TWSOpenDialog;
  TWSOpenDialog = class(TWSFileDialog)
  public class var
    FWSOpenDialog_WSClass: TWSOpenDialogClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
  end;

  { TWSSaveDialog }

  TWSSaveDialogClass = class of TWSSaveDialog;
  TWSSaveDialog = class(TWSOpenDialog)
  public class var
    FWSSaveDialog_WSClass: TWSSaveDialogClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
  end;

  { TWSSelectDirectoryDialog }

  TWSSelectDirectoryDialogClass = class of TWSSelectDirectoryDialog;
  TWSSelectDirectoryDialog = class(TWSOpenDialog)
  public class var
    FWSSelectDirectoryDialog_WSClass: TWSSelectDirectoryDialogClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
  end;

  { TWSColorDialog }

  TWSColorDialogClass = class of TWSColorDialog;
  TWSColorDialog = class(TWSCommonDialog_CallWS)
  public class var
    FWSColorDialog_WSClass: TWSColorDialogClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
  end;

  { TWSColorButton }

  TWSColorButton = class(TWSGraphicControl)
  published
  end;

  { TWSFontDialog }

  TWSFontDialogClass = class of TWSFontDialog;
  TWSFontDialog = class(TWSCommonDialog_CallWS)
  public class var
    FWSFontDialog_WSClass: TWSFontDialogClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
  end;

  { WidgetSetRegistration }

  procedure RegisterCommonDialog;
  procedure RegisterFileDialog;
  procedure RegisterOpenDialog;
  procedure RegisterSaveDialog;
  procedure RegisterSelectDirectoryDialog;
  procedure RegisterColorDialog;
  procedure RegisterColorButton;
  procedure RegisterFontDialog;

implementation

uses
  LResources;

class function TWSCommonDialog.GetImplementation: TWSObjectClass;
begin
  Result := FWSCommonDialog_WSClass;
end;

class procedure TWSCommonDialog.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSCommonDialog_WSClass := TWSCommonDialogClass(AImpl);
end;

class function  TWSCommonDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
begin
  Result := 0;
end;

class procedure TWSCommonDialog.DestroyHandle(const ACommonDialog: TCommonDialog);
begin
end;

class function TWSCommonDialog.QueryWSEventCapabilities(
  const ACommonDialog: TCommonDialog): TCDWSEventCapabilities;
begin
  Result := [];
end;

class procedure TWSCommonDialog.ShowModal(const ACommonDialog: TCommonDialog);
begin
end;

{ TWSCommonDialog_CallWS }

class function TWSCommonDialog_CallWS.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
begin
  Result := FWSCommonDialog_WSClass.CreateHandle(ACommonDialog);
end;

class procedure TWSCommonDialog_CallWS.ShowModal(const ACommonDialog: TCommonDialog);
begin
  FWSCommonDialog_WSClass.ShowModal(ACommonDialog);
end;

class procedure TWSCommonDialog_CallWS.DestroyHandle(const ACommonDialog: TCommonDialog);
begin
  FWSCommonDialog_WSClass.DestroyHandle(ACommonDialog);
end;

class function TWSCommonDialog_CallWS.QueryWSEventCapabilities(
  const ACommonDialog: TCommonDialog): TCDWSEventCapabilities;
begin
  Result := FWSCommonDialog_WSClass.QueryWSEventCapabilities(ACommonDialog);
end;

{ TWSFileDialog }

class function TWSFileDialog.GetImplementation: TWSObjectClass;
begin
  Result:= FWSFileDialog_WSClass;
end;

class procedure TWSFileDialog.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSFileDialog_WSClass := TWSFileDialogClass(AImpl);
end;

{ TWSOpenDialog }

class function TWSOpenDialog.GetImplementation: TWSObjectClass;
begin
  Result:= FWSOpenDialog_WSClass;
end;

class procedure TWSOpenDialog.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSOpenDialog_WSClass := TWSOpenDialogClass(AImpl);
end;

{ TWSSaveDialog }

class function TWSSaveDialog.GetImplementation: TWSObjectClass;
begin
  Result:= FWSSaveDialog_WSClass;
end;

class procedure TWSSaveDialog.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSSaveDialog_WSClass := TWSSaveDialogClass(AImpl);
end;

{ TWSSelectDirectoryDialog }

class function TWSSelectDirectoryDialog.GetImplementation: TWSObjectClass;
begin
  Result:= FWSSelectDirectoryDialog_WSClass;
end;

class procedure TWSSelectDirectoryDialog.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSSelectDirectoryDialog_WSClass := TWSSelectDirectoryDialogClass(AImpl);
end;

{ TWSColorDialog }

class function TWSColorDialog.GetImplementation: TWSObjectClass;
begin
  Result:= FWSColorDialog_WSClass;
end;

class procedure TWSColorDialog.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSColorDialog_WSClass := TWSColorDialogClass(AImpl);
end;

{ TWSFontDialog }

class function TWSFontDialog.GetImplementation: TWSObjectClass;
begin
  Result:= FWSFontDialog_WSClass;
end;

class procedure TWSFontDialog.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSFontDialog_WSClass := TWSFontDialogClass(AImpl);
end;

{ WidgetSetRegistration }

procedure RegisterCommonDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterCommonDialog then
    RegisterWSComponent(TCommonDialog, TWSCommonDialog);
  RegisterPropertyToSkip(TCommonDialog, 'Ctl3D', 'VCL compatibility property', '');
  Done := True;
end;

procedure RegisterFileDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterFileDialog;
//  if not WSRegisterFileDialog then
//    RegisterWSComponent(TFileDialog, TWSFileDialog);
  Done := True;
end;

procedure RegisterOpenDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterOpenDialog;
//  if not WSRegisterOpenDialog then
//    RegisterWSComponent(TOpenDialog, TWSOpenDialog);
  Done := True;
end;

procedure RegisterSaveDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterSaveDialog;
//  if not WSRegisterSaveDialog then
//    RegisterWSComponent(TSaveDialog, TWSSaveDialog);
  Done := True;
end;

procedure RegisterSelectDirectoryDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterSelectDirectoryDialog;
//  if not WSRegisterSelectDirectoryDialog then
//    RegisterWSComponent(TSelectDirectoryDialog, TWSSelectDirectoryDialog);
  Done := True;
end;

procedure RegisterColorDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterColorDialog;
//  if not WSRegisterColorDialog then
//    RegisterWSComponent(TColorDialog, TWSColorDialog);
  Done := True;
end;

procedure RegisterColorButton;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterColorButton;
//  if not WSRegisterColorButton then
//    RegisterWSComponent(TColorButton, TWSColorButton);
  Done := True;
end;

procedure RegisterFontDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterFontDialog;
//  if not WSRegisterFontDialog then
//    RegisterWSComponent(TFontDialog, TWSFontDialog);
  Done := True;
end;

end.
