unit frCompilerDirectReturns;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frCompilerDirectReturns.pas, released April 2005.
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
  Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TfCompilerDirectReturns }

  TfCompilerDirectReturns = class(TAbstractIDEOptionsEditor)
    Label1: TLabel;
    rgBeforeUses: TRadioGroup;
    rgBeforeStatements: TRadioGroup;
    rgBeforeGeneral: TRadioGroup;
    rgAfterGeneral: TRadioGroup;
    rgAfterStatements: TRadioGroup;
    rgAfterUses: TRadioGroup;
    Label2: TLabel;
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

procedure TfCompilerDirectReturns.FrameResize(Sender:TObject);
begin
  rgBeforeGeneral.Width := (Width-24) div 3;
  rgBeforeStatements.Width := rgBeforeGeneral.Width;
  rgAfterGeneral.Width := rgBeforeGeneral.Width;
  rgAfterStatements.Width := rgBeforeGeneral.Width;
end;

constructor TfCompilerDirectReturns.Create(AOwner: TComponent);
begin
  inherited;
  //  fiHelpContext := ?? ;
end;

function TfCompilerDirectReturns.GetTitle: String;
begin
  Result := lisCDCompilerDirectives;
end;

procedure TfCompilerDirectReturns.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  Label1.Caption := lisCDUseANewLineBeforeCompilerDirectives;
  rgBeforeUses.Caption := lisCDUsesClause;
  rgBeforeUses.Items[0] := lisCaseBlocksAlways;
  rgBeforeUses.Items[1] := lisCaseBlocksLeaveAsIs;
  rgBeforeUses.Items[2] := lisCaseBlocksNever;

  rgBeforeStatements.Caption := lisCDStatements;
  rgBeforeStatements.Items[0] := lisCaseBlocksAlways;
  rgBeforeStatements.Items[1] := lisCaseBlocksLeaveAsIs;
  rgBeforeStatements.Items[2] := lisCaseBlocksNever;

  rgBeforeGeneral.Caption := lisCDOtherPlaces;
  rgBeforeGeneral.Items[0] := lisCaseBlocksAlways;
  rgBeforeGeneral.Items[1] := lisCaseBlocksLeaveAsIs;
  rgBeforeGeneral.Items[2] := lisCaseBlocksNever;

  Label2.Caption := lisCDUseANewLineAfterCompilerDirectives;
  rgAfterUses.Caption := lisCDUsesClause;
  rgAfterUses.Items[0] := lisCaseBlocksAlways;
  rgAfterUses.Items[1] := lisCaseBlocksLeaveAsIs;
  rgAfterUses.Items[2] := lisCaseBlocksNever;

  rgAfterStatements.Caption := lisCDStatements;
  rgAfterStatements.Items[0] := lisCaseBlocksAlways;
  rgAfterStatements.Items[1] := lisCaseBlocksLeaveAsIs;
  rgAfterStatements.Items[2] := lisCaseBlocksNever;

  rgAfterGeneral.Caption := lisCDOtherPlaces;
  rgAfterGeneral.Items[0] := lisCaseBlocksAlways;
  rgAfterGeneral.Items[1] := lisCaseBlocksLeaveAsIs;
  rgAfterGeneral.Items[2] := lisCaseBlocksNever;
end;

procedure TfCompilerDirectReturns.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Returns do
  begin
    rgBeforeUses.ItemIndex := Ord(BeforeCompilerDirectUses);
    rgBeforeStatements.ItemIndex := Ord(BeforeCompilerDirectStatements);
    rgBeforeGeneral.ItemIndex := Ord(BeforeCompilerDirectGeneral);

    rgAfterUses.ItemIndex := Ord(AfterCompilerDirectUses);
    rgAfterStatements.ItemIndex := Ord(AfterCompilerDirectStatements);
    rgAfterGeneral.ItemIndex := Ord(AfterCompilerDirectGeneral);
  end;
end;

procedure TfCompilerDirectReturns.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Returns do
  begin
    BeforeCompilerDirectUses := TTriOptionStyle(rgBeforeUses.ItemIndex);
    BeforeCompilerDirectStatements := TTriOptionStyle(rgBeforeStatements.ItemIndex);
    BeforeCompilerDirectGeneral := TTriOptionStyle(rgBeforeGeneral.ItemIndex);

    AfterCompilerDirectUses := TTriOptionStyle(rgAfterUses.ItemIndex);
    AfterCompilerDirectStatements := TTriOptionStyle(rgAfterStatements.ItemIndex);
    AfterCompilerDirectGeneral := TTriOptionStyle(rgAfterGeneral.ItemIndex);

  end;
end;

class function TfCompilerDirectReturns.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TFormattingSettings;
end;

initialization
  RegisterIDEOptionsEditor(JCFOptionsGroup, TfCompilerDirectReturns, JCFOptionCompilerDirectives, JCFOptionLongLines);
end.
