{
 *****************************************************************************
  This file is part of the EducationLaz package

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Frame to setup the education package.
}
unit EduEnvOptsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazConfigStorage,
  // LCL
  LCLProc, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, LazIDEIntf,
  // Education
  EduOptions;

const
  EnvOptionsEducation = 2000;

type

  { TEduGeneralOptions }

  TEduGeneralOptions = class(TEduOptionsNode)
  public
    constructor Create; override;
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
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
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
  EduOptionID:=RegisterIDEOptionsGroup(EduOptionID,TEduOptions)^.Index;
  EduOptionGeneralID:=RegisterIDEOptionsEditor(EduOptionID,TEduEnvFrame,
                                                     EduOptionGeneralID)^.Index;

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
  Result:=ersEduEnvOptsFrameTitle;
end;

procedure TEduEnvFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  EnableCheckBox.Caption:=ersEnableEduCheckBoxCaption;
end;

procedure TEduEnvFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions=EducationOptions then begin
    EnableCheckBox.Checked:=EducationOptions.Enabled;
  end;
end;

procedure TEduEnvFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions=EducationOptions then begin
    EducationOptions.Enabled:=EnableCheckBox.Checked;
  end;
end;

class function TEduEnvFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := EducationIDEOptionsClass;
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

{$R *.lfm}

end.
