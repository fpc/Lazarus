{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frClarify.pas, released April 2000.
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

unit frClarifyLongLineBreaker;

{$I JcfGlobal.inc}

interface

uses
  Classes, StdCtrls, ExtCtrls, Spin,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TfClarifyLongLineBreaker }

  TfClarifyLongLineBreaker = class(TAbstractIDEOptionsEditor)
    edtMaxLineLength: TSpinEdit;
    Label3: TLabel;
    rgRebreakLongLines: TRadioGroup;
    procedure cbRebreakLinesClick(Sender: TObject);
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
  JcfSettings, SetReturns, jcfuiconsts;


constructor TfClarifyLongLineBreaker.Create(AOwner: TComponent);
begin
  inherited;
  //fiHelpContext := HELP_CLARIFY_LONG_LINES;
end;

function TfClarifyLongLineBreaker.GetTitle: String;
begin
  Result := lisLBLineBreaking;
end;

procedure TfClarifyLongLineBreaker.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  Label3.Caption := lisLBMaxLineLength;
  rgRebreakLongLines.Caption := lisLBBreakLinesThatAreLongerThanMaxLineLength;
  rgRebreakLongLines.Items[0] := lisLBNever;
  rgRebreakLongLines.Items[1] := lisLBSometimesIfAGoodPlaceToBreakIsFound;
  rgRebreakLongLines.Items[2] :=
    lisLBUsuallyUnlessThereIsNoAcceptablePlaceToBreak;
end;

{-------------------------------------------------------------------------------
  worker procs }

procedure TfClarifyLongLineBreaker.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Returns do
  begin
    { line breaking }
    edtMaxLineLength.Value := MaxLineLength;
    rgRebreakLongLines.ItemIndex := Ord(RebreakLines);
  end;
end;

procedure TfClarifyLongLineBreaker.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Returns do
  begin
    { line breaking }
    MaxLineLength := edtMaxLineLength.Value;
    RebreakLines  := TWhenToRebreakLines(rgRebreakLongLines.ItemIndex);
  end;
end;

class function TfClarifyLongLineBreaker.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TFormattingSettings;
end;

{-------------------------------------------------------------------------------
  event handlers }

procedure TfClarifyLongLineBreaker.cbRebreakLinesClick(Sender: TObject);
begin
  edtMaxLineLength.Enabled := (rgRebreakLongLines.ItemIndex > 0);
end;

initialization
  RegisterIDEOptionsEditor(JCFOptionsGroup, TfClarifyLongLineBreaker, JCFOptionLongLines, JCFOptionClarify);
end.
