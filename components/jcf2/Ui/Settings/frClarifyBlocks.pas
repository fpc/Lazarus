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

unit frClarifyBlocks;

{$I JcfGlobal.inc}

interface

uses
  Classes, StdCtrls, ExtCtrls,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TfClarifyBlocks }

  TfClarifyBlocks = class(TAbstractIDEOptionsEditor)
    rgBlockBegin: TRadioGroup;
    rgBlock: TRadioGroup;
    rgEndElse: TRadioGroup;
    Label1: TLabel;
    rgElseIf: TRadioGroup;
    rgElseBegin: TRadioGroup;
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

procedure TfClarifyBlocks.FrameResize(Sender:TObject);
begin
  rgBlock.Width := (Width-18) div 2;
end;

constructor TfClarifyBlocks.Create(AOwner: TComponent);
begin
  inherited;
  //fiHelpContext := HELP_CLARIFY_BLOCKS;
end;

function TfClarifyBlocks.GetTitle: String;
begin
  Result := lisBlocksBlocks;
end;

procedure TfClarifyBlocks.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  Label1.Caption := lisBlocksUseANewLineInBlocksAt;
  rgBlockBegin.Caption := lisBlocksBlockWithBegin;
  rgBlockBegin.Items[0] := lisCaseBlocksAlways;
  rgBlockBegin.Items[1] := lisCaseBlocksLeaveAsIs;
  rgBlockBegin.Items[2] := lisCaseBlocksNever;

  rgBlock.Caption := lisBlocksBlockWithoutBegin;
  rgBlock.Items[0] := lisCaseBlocksAlways;
  rgBlock.Items[1] := lisCaseBlocksLeaveAsIs;
  rgBlock.Items[2] := lisCaseBlocksNever;

  rgElseIf.Caption := lisBlocksBetweenElseAndIf;
  rgElseIf.Items[0] := lisCaseBlocksAlways;
  rgElseIf.Items[1] := lisCaseBlocksLeaveAsIs;
  rgElseIf.Items[2] := lisCaseBlocksNever;

  rgEndElse.Caption := lisBlocksBetweenEndAndElse;
  rgEndElse.Items[0] := lisCaseBlocksAlways;
  rgEndElse.Items[1] := lisCaseBlocksLeaveAsIs;
  rgEndElse.Items[2] := lisCaseBlocksNever;

  rgElseBegin.Caption := lisBlocksElseBegin;
  rgElseBegin.Items[0] := lisCaseBlocksAlways;
  rgElseBegin.Items[1] := lisCaseBlocksLeaveAsIs;
  rgElseBegin.Items[2] := lisCaseBlocksNever;
end;

{-------------------------------------------------------------------------------
  worker procs }

procedure TfClarifyBlocks.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Returns do
  begin
    { block styles }
    rgBlockBegin.ItemIndex := Ord(BlockBeginStyle);
    rgBlock.ItemIndex      := Ord(BlockStyle);

    rgEndElse.ItemIndex    := Ord(EndElseStyle);
    rgElseIf.ItemIndex     := Ord(ElseIfStyle);
    rgElseBegin.ItemIndex := Ord(ElseBeginStyle);
  end;
end;

procedure TfClarifyBlocks.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Returns do
  begin
    { block styles }
    BlockBeginStyle := TTriOptionStyle(rgBlockBegin.ItemIndex);
    BlockStyle      := TTriOptionStyle(rgBlock.ItemIndex);

    EndElseStyle    := TTriOptionStyle(rgEndElse.ItemIndex);
    ElseIfStyle     := TTriOptionStyle(rgElseIf.ItemIndex);
    ElseBeginStyle  := TTriOptionStyle(rgElseBegin.ItemIndex);
  end;
end;

class function TfClarifyBlocks.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TFormattingSettings;
end;

{-------------------------------------------------------------------------------
  event handlers }

initialization
  RegisterIDEOptionsEditor(JCFOptionsGroup, TfClarifyBlocks, JCFOptionBlocks, JCFOptionLongLines);
end.
