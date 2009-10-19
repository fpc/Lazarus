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
    Frame to setup the education package.
}
unit EduEnvOptsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, AvgLvlTree,
  FormEditingIntf, LazConfigStorage, IDEOptionsIntf, ComponentReg,
  IDEImagesIntf, LazIDEIntf,
  EduOptions, EduCompPalette;

const
  EnvOptionsEducation = 2000;

type

  { TEduGeneralOptions }

  TEduGeneralOptions = class(TEduOptionsNode)
  public
    constructor Create;
    destructor Destroy; override;
    function Load(Config: TConfigStorage): TModalResult; override;
    function Save(Config: TConfigStorage): TModalResult; override;
  end;

  { TEduEnvFrame }

  TEduEnvFrame = class(TAbstractIDEOptionsEditor)
    EnableCheckBox: TCheckBox;
    procedure FrameClick(Sender: TObject);
  private
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
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
  EduGeneralOptions:=TEduGeneralOptions.Create;
  EducationOptions.Root.Add(EduGeneralOptions);
  RegisterIDEOptionsGroup(EduOptionID,TEduOptions);
  RegisterIDEOptionsEditor(EduOptionID,TEduEnvFrame,EduOptionGeneralID);

  LazarusIDE.AddHandlerOnProjectOpened(@EducationOptions.OnProjectOpened);
end;

{ TEduEnvFrame }

procedure TEduEnvFrame.FrameClick(Sender: TObject);
begin

end;

constructor TEduEnvFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

end;

destructor TEduEnvFrame.Destroy;
begin
  inherited Destroy;
end;

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
  EnableCheckBox.Checked:=EducationOptions.Enabled;
end;

procedure TEduEnvFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  EducationOptions.Enabled:=EnableCheckBox.Checked;

  if EducationOptions.Save<>mrOk then
    DebugLn(['TEduEnvFrame.WriteSettings Failed']);
  EducationOptions.Apply;
end;

class function TEduEnvFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := nil;
end;

{ TEduGeneralOptions }

constructor TEduGeneralOptions.Create;
begin
  inherited Create;
  Name:='General';
end;

destructor TEduGeneralOptions.Destroy;
begin
  if EduGeneralOptions=Self then EduGeneralOptions:=nil;
  inherited Destroy;
end;

function TEduGeneralOptions.Load(Config: TConfigStorage): TModalResult;
begin
  Result:=inherited Load(Config);
end;

function TEduGeneralOptions.Save(Config: TConfigStorage): TModalResult;
begin
  Result:=inherited Save(Config);
end;

initialization
  {$I eduenvoptsframe.lrs}

end.

