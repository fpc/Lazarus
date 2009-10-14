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
    constructor Create;
    destructor Destroy; override;
    function Load(Config: TConfigStorage): TModalResult; override;
    function Save(Config: TConfigStorage): TModalResult; override;
    property Enabled: boolean read FEnabled write SetEnabled default true;
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
  RegisterIDEOptionsGroup(EduOptionID,TEduOptions);
  RegisterIDEOptionsEditor(EduOptionID,TEduEnvFrame,EduOptionGeneralID);
  EduGeneralOptions:=TEduGeneralOptions.Create;
  EducationOptions.Root.Add(EduGeneralOptions);

  // load options
  EducationOptions.Load;
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
  EnableCheckBox.Checked:=EduGeneralOptions.Enabled;
end;

procedure TEduEnvFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  EduGeneralOptions.Enabled:=EnableCheckBox.Checked;
  if EducationOptions.Save<>mrOk then
    DebugLn(['TEduEnvFrame.WriteSettings Failed']);
end;

class function TEduEnvFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := nil;
end;

{ TEduGeneralOptions }

procedure TEduGeneralOptions.SetEnabled(const AValue: boolean);
begin
  if FEnabled=AValue then exit;
  FEnabled:=AValue;
  Changed;
end;

constructor TEduGeneralOptions.Create;
begin
  inherited Create;
  Name:='General';
  FEnabled:=true;
end;

destructor TEduGeneralOptions.Destroy;
begin
  if EduGeneralOptions=Self then EduGeneralOptions:=nil;
  inherited Destroy;
end;

function TEduGeneralOptions.Load(Config: TConfigStorage): TModalResult;
begin
  FEnabled:=Config.GetValue('Enabled',True);
  Result:=inherited Load(Config);
end;

function TEduGeneralOptions.Save(Config: TConfigStorage): TModalResult;
begin
  Config.SetDeleteValue('Enabled',Enabled,true);
  Result:=inherited Save(Config);
end;

initialization
  {$I eduenvoptsframe.lrs}

end.

