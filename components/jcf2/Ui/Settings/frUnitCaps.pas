unit frUnitCaps;
{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frUnitCaps.pas
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

{$I JcfGlobal.inc}

interface

uses
  Classes, Controls, Forms,
  { local}frmBaseSettingsFrame, StdCtrls, JvMemo, JvExStdCtrls;

type
  TfrUnitNameCaps = class(TfrSettingsFrame)
    mWords: TJvMemo;
    cbEnableAnyWords: TCheckBox;
    Label1: TLabel;
    procedure FrameResize(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;
  end;

implementation

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

uses JcfSettings;

constructor TfrUnitNameCaps.Create(AOwner: TComponent);
begin
  inherited;

end;

procedure TfrUnitNameCaps.Read;
begin
  inherited;
  with FormatSettings.UnitNameCaps do
  begin
    cbEnableAnyWords.Checked := Enabled;
    mWords.Lines.Assign(Words);
  end;

end;

procedure TfrUnitNameCaps.Write;
begin
  inherited;
  with FormatSettings.UnitNameCaps do
  begin
    Enabled := cbEnableAnyWords.Checked;
    Words.Assign(mWords.Lines);
  end;

end;

procedure TfrUnitNameCaps.FrameResize(Sender: TObject);
begin
  mWords.Height := ClientHeight -
    (cbEnableAnyWords.Top + cbEnableAnyWords.Height + GUI_PAD);
end;

end.
