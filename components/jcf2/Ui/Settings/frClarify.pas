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

unit frClarify;

{$I JcfGlobal.inc}

interface

uses
  Classes,
  StdCtrls, ExtCtrls,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TfClarify }

  TfClarify = class(TAbstractIDEOptionsEditor)
    rgRunOnceOffs: TRadioGroup;
    mFileExtensions: TMemo;
    Label1: TLabel;
  public
    constructor Create(AOwner: TComponent); override;

    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    class function DefaultCollapseChildNodes: Boolean; override;
  end;

implementation

{$R *.lfm}

uses
  JcfSettings, SetClarify, jcfuiconsts;

constructor TfClarify.Create(AOwner: TComponent);
begin
  inherited;
  //fiHelpContext := HELP_CLARIFY;
end;

function TfClarify.GetTitle: String;
begin
  Result := lisClarifyClarify;
end;

procedure TfClarify.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  Label1.Caption := lisClarifyFileExtensionsToFormat;
  rgRunOnceOffs.Caption := lisClarifyRunOnceOffs;
  rgRunOnceOffs.Items[0] := lisClarifyDoNotRun;
  rgRunOnceOffs.Items[1] := lisClarifyDoRun;
  rgRunOnceOffs.Items[2] := lisClarifyRunOnlyThese;
end;


{-------------------------------------------------------------------------------
  worker procs }

procedure TfClarify.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Clarify do
  begin
    rgRunOnceOffs.ItemIndex := Ord(OnceOffs);

    mFileExtensions.Lines.Assign(FileExtensions);
  end;
end;

procedure TfClarify.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Clarify do
  begin
    OnceOffs := TOnceOffsOption(rgRunOnceOffs.ItemIndex);

    FileExtensions.Assign(mFileExtensions.Lines);
    FileExtensions.Sort;
  end;
end;

class function TfClarify.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TFormattingSettings;
end;

class function TfClarify.DefaultCollapseChildNodes: Boolean;
begin
  Result := True;
end;

{-------------------------------------------------------------------------------
  event handlers }
initialization
  RegisterIDEOptionsEditor(JCFOptionsGroup, TfClarify, JCFOptionClarify);
end.
