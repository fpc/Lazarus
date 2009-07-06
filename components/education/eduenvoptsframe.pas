{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the EducationLaz package                            *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Dialog to setup the education package.
}
unit EduEnvOptsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls,
  LazConfigStorage, IDEOptionsIntf, EduOptions;

const
  EnvOptionsEducation = 2000;

type

  { TEduGeneralOptions }

  TEduGeneralOptions = class(TEduOptionsNode)
  private
    FEnabled: boolean;
    procedure SetEnabled(const AValue: boolean);
  public
    destructor Destroy; override;
    function Load(Config: TConfigStorage): TModalResult; override;
    function Save(Config: TConfigStorage): TModalResult; override;
    property Enabled: boolean read FEnabled write SetEnabled;
  end;

  { TEduEnvFrame }

  TEduEnvFrame = class(TAbstractIDEOptionsEditor)
    EnableCheckBox: TCheckBox;
  private
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

var
  EduEnvFrame: TEduEnvFrame;
  EduGeneralOptions: TEduGeneralOptions = nil;

procedure Register;

implementation

procedure Register;
begin
  RegisterIDEOptionsGroup(EduOptionID,'Education');
  RegisterIDEOptionsEditor(EduOptionID,TEduEnvFrame,EduOptionGeneralID);
  EduGeneralOptions:=TEduGeneralOptions.Create;
  EducationOptions.Root.Add(EduGeneralOptions);
end;

{ TEduEnvFrame }

function TEduEnvFrame.GetTitle: String;
begin
  Result:='General';
end;

procedure TEduEnvFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  EnableCheckBox.Caption:='Enable education settings';
end;

procedure TEduEnvFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  debugln(['TEduEnvFrame.ReadSettings ',EduGeneralOptions.Enabled]);
  EnableCheckBox.Checked:=EduGeneralOptions.Enabled;
end;

procedure TEduEnvFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  debugln(['TEduEnvFrame.WriteSettings ',EnableCheckBox.Checked]);
  EduGeneralOptions.Enabled:=EnableCheckBox.Checked;
end;

class function TEduEnvFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEduOptions;
end;

{ TEduGeneralOptions }

procedure TEduGeneralOptions.SetEnabled(const AValue: boolean);
begin
  if FEnabled=AValue then exit;
  FEnabled:=AValue;
  Changed;
end;

destructor TEduGeneralOptions.Destroy;
begin
  if EduGeneralOptions=Self then EduGeneralOptions:=nil;
  inherited Destroy;
end;

function TEduGeneralOptions.Load(Config: TConfigStorage): TModalResult;
begin
  Result:=inherited Load(Config);
  FEnabled:=Config.GetValue('Enabled',true);
end;

function TEduGeneralOptions.Save(Config: TConfigStorage): TModalResult;
begin
  Result:=inherited Save(Config);
  Config.SetDeleteValue('Enabled',Enabled,true);
end;

initialization
  {$I eduenvoptsframe.lrs}

end.

