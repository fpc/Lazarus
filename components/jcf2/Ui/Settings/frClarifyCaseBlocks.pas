{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frClarifyCaseBlocks.pas, released June 2004.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele.

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

unit frClarifyCaseBlocks;

{$I JcfGlobal.inc}

interface

uses
  Classes, StdCtrls, ExtCtrls,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TfClarifyCaseBlocks }

  TfClarifyCaseBlocks = class(TAbstractIDEOptionsEditor)
    rgLabelBegin: TRadioGroup;
    rgLabel: TRadioGroup;
    Label1: TLabel;
    rgCaseLabel: TRadioGroup;
    rgElseCase: TRadioGroup;
    rgCaseBegin: TRadioGroup;
    rgCaseElseBegin: TRadioGroup;
    procedure FrameResize(Sender:TObject);
  public
    constructor Create(AOwner: TComponent); override;

    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

uses
  JcfSettings, SettingsTypes, jcfuiconsts;

procedure TfClarifyCaseBlocks.FrameResize(Sender:TObject);
begin
  rgLabel.Width := (Width-18) div 2;
end;

constructor TfClarifyCaseBlocks.Create(AOwner: TComponent);
begin
  inherited;
  //fiHelpContext := HELP_CLARIFY_BLOCKS;
end;

function TfClarifyCaseBlocks.GetTitle: String;
begin
  Result := lisCaseBlocksCaseBlocks;
end;

procedure TfClarifyCaseBlocks.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  Label1.Caption := lisCaseBlocksUseANewLineInCaseBlocksAt;
  rgLabelBegin.Caption := lisCaseBlocksLabelWithBegin;
  rgLabelBegin.Items[0] := lisCaseBlocksAlways;
  rgLabelBegin.Items[1] := lisCaseBlocksLeaveAsIs;
  rgLabelBegin.Items[2] := lisCaseBlocksNever;

  rgLabel.Caption := lisCaseBlocksLabelWithoutBegin;
  rgLabel.Items[0] := lisCaseBlocksAlways;
  rgLabel.Items[1] := lisCaseBlocksLeaveAsIs;
  rgLabel.Items[2] := lisCaseBlocksNever;

  rgCaseBegin.Caption := lisCaseBlocksCaseWithBegin;
  rgCaseBegin.Items[0] := lisCaseBlocksAlways;
  rgCaseBegin.Items[1] := lisCaseBlocksLeaveAsIs;
  rgCaseBegin.Items[2] := lisCaseBlocksNever;

  rgCaseLabel.Caption := lisCaseBlocksCaseWithoutBegin;
  rgCaseLabel.Items[0] := lisCaseBlocksAlways;
  rgCaseLabel.Items[1] := lisCaseBlocksLeaveAsIs;
  rgCaseLabel.Items[2] := lisCaseBlocksNever;

  rgCaseElseBegin.Caption := lisCaseBlocksElseCaseWithBegin;
  rgCaseElseBegin.Items[0] := lisCaseBlocksAlways;
  rgCaseElseBegin.Items[1] := lisCaseBlocksLeaveAsIs;
  rgCaseElseBegin.Items[2] := lisCaseBlocksNever;

  rgElseCase.Caption := lisCaseBlocksElseCaseWithoutBegin;
  rgElseCase.Items[0] := lisCaseBlocksAlways;
  rgElseCase.Items[1] := lisCaseBlocksLeaveAsIs;
  rgElseCase.Items[2] := lisCaseBlocksNever;
end;


{-------------------------------------------------------------------------------
  worker procs }

procedure TfClarifyCaseBlocks.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Returns do
  begin
    { block styles }
    rgLabelBegin.ItemIndex := Ord(LabelBeginStyle);
    rgLabel.ItemIndex      := Ord(LabelStyle);

    rgCaseLabel.ItemIndex  := Ord(CaseLabelStyle);
    rgCaseBegin.ItemIndex  := Ord(CaseBeginStyle);
    rgElseCase.ItemIndex := Ord(CaseElseStyle);
    rgCaseElseBegin.ItemIndex := Ord(CaseElseBeginStyle);
  end;
end;

procedure TfClarifyCaseBlocks.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Returns do
  begin
    { block styles }
    LabelBeginStyle := TTriOptionStyle(rgLabelBegin.ItemIndex);
    LabelStyle      := TTriOptionStyle(rgLabel.ItemIndex);

    CaseLabelStyle := TTriOptionStyle(rgCaseLabel.ItemIndex);
    CaseBeginStyle := TTriOptionStyle(rgCaseBegin.ItemIndex);
    CaseElseStyle  := TTriOptionStyle(rgElseCase.ItemIndex);
    CaseElseBeginStyle  := TTriOptionStyle(rgCaseElseBegin.ItemIndex);
  end;
end;

class function TfClarifyCaseBlocks.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TFormattingSettings;
end;

{-------------------------------------------------------------------------------
  event handlers }

initialization
  RegisterIDEOptionsEditor(JCFOptionsGroup, TfClarifyCaseBlocks, JCFOptionCaseBlocks, JCFOptionLongLines);
end.
