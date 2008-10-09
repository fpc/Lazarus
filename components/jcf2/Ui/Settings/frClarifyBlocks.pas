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
  { delphi }
  Classes, Controls, Forms,
  StdCtrls, ExtCtrls,
  { local}
  frmBaseSettingsFrame;

type
  TfClarifyBlocks = class(TfrSettingsFrame)
    rgBlockBegin: TRadioGroup;
    rgBlock: TRadioGroup;
    rgEndElse: TRadioGroup;
    Label1: TLabel;
    rgElseIf: TRadioGroup;
    rgElseBegin: TRadioGroup;
  private

  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;

  end;

implementation

uses JcfSettings, SettingsTypes, JcfHelp;

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

constructor TfClarifyBlocks.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY_BLOCKS;
end;

{-------------------------------------------------------------------------------
  worker procs }

procedure TfClarifyBlocks.Read;
begin
  with FormatSettings.Returns do
  begin
    { block styles }
    rgBlockBegin.ItemIndex := Ord(BlockBeginStyle);
    rgBlock.ItemIndex      := Ord(BlockStyle);

    rgEndElse.ItemIndex    := Ord(EndElseStyle);
    rgElseIf.ItemIndex     := Ord(ElseIfStyle);
    rgElseBegin.ItemIndex := Ord(ElseBeginStyle);
  end;
end;

procedure TfClarifyBlocks.Write;
begin
  with FormatSettings.Returns do
  begin
    { block styles }
    BlockBeginStyle := TTriOptionStyle(rgBlockBegin.ItemIndex);
    BlockStyle      := TTriOptionStyle(rgBlock.ItemIndex);

    EndElseStyle    := TTriOptionStyle(rgEndElse.ItemIndex);
    ElseIfStyle     := TTriOptionStyle(rgElseIf.ItemIndex);
    ElseBeginStyle  := TTriOptionStyle(rgElseBegin.ItemIndex);
  end;
end;

{-------------------------------------------------------------------------------
  event handlers }

end.
