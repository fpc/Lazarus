unit frUses;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frUses, released May 2003.
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
  Classes, StdCtrls,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TfUses }

  TfUses = class(TAbstractIDEOptionsEditor)
    cbRemoveEnabled: TCheckBox;
    cbInsertInterface: TCheckBox;
    cbInsertImplementation: TCheckBox;
    cbFindReplace: TCheckBox;
    mRemove: TMemo;
    mInsertInterface: TMemo;
    mFind: TMemo;
    mInsertImplementation: TMemo;
    mReplace: TMemo;
    procedure cbInsertInterfaceClick(Sender: TObject);
    procedure cbInsertImplementationClick(Sender: TObject);
    procedure cbRemoveEnabledClick(Sender: TObject);
    procedure cbFindReplaceClick(Sender: TObject);
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
  JcfSettings, jcfuiconsts;

constructor TfUses.Create(AOwner: TComponent);
begin
  inherited;
  //fiHelpContext := HELP_CLARIFY_FIND_AND_REPLACE_USES;
end;

function TfUses.GetTitle: String;
begin
  Result := lisUsesUses;
end;

procedure TfUses.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  cbRemoveEnabled.Caption := lisUsesRemove;
  cbInsertInterface.Caption := lisUsesInsertIntoInterface;
  cbInsertImplementation.Caption := lisUsesInsertIntoImplementation;
  cbFindReplace.Caption := lisUsesReplace;
end;


procedure TfUses.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.UsesClause do
  begin
    cbRemoveEnabled.Checked   := RemoveEnabled;
    cbInsertInterface.Checked := InsertInterfaceEnabled;
    cbInsertImplementation.Checked := InsertImplementationEnabled;
    cbFindReplace.Checked     := FindReplaceEnabled;

    mRemove.Lines.Assign(Remove);
    mInsertInterface.Lines.Assign(InsertInterface);
    mInsertImplementation.Lines.Assign(InsertImplementation);
    mFind.Lines.Assign(Find);
    mReplace.Lines.Assign(Replace);
  end;

  cbInsertInterfaceClick(nil);
  cbInsertImplementationClick(nil);
  cbRemoveEnabledClick(nil);
  cbFindReplaceClick(nil);
end;

procedure TfUses.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.UsesClause do
  begin
    RemoveEnabled      := cbRemoveEnabled.Checked;
    InsertInterfaceEnabled := cbInsertInterface.Checked;
    InsertImplementationEnabled := cbInsertImplementation.Checked;
    FindReplaceEnabled := cbFindReplace.Checked;

    Remove.Assign(mRemove.Lines);
    InsertInterface.Assign(mInsertInterface.Lines);
    InsertImplementation.Assign(mInsertImplementation.Lines);
    Find.Assign(mFind.Lines);
    Replace.Assign(mReplace.Lines);
  end;

end;

class function TfUses.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TFormattingSettings;
end;

procedure TfUses.cbInsertInterfaceClick(Sender: TObject);
begin
  mInsertInterface.Enabled := cbInsertInterface.Checked;
end;

procedure TfUses.cbInsertImplementationClick(Sender: TObject);
begin
  mInsertImplementation.Enabled := cbInsertImplementation.Checked;
end;

procedure TfUses.cbRemoveEnabledClick(Sender: TObject);
begin
  mRemove.Enabled := cbRemoveEnabled.Checked;
end;

procedure TfUses.cbFindReplaceClick(Sender: TObject);
begin
  mFind.Enabled    := cbFindReplace.Checked;
  mReplace.Enabled := cbFindReplace.Checked;
end;

procedure TfUses.FrameResize(Sender:TObject);
begin
  mInsertImplementation.Width := (Width-24) div 2;
end;

initialization
  RegisterIDEOptionsEditor(JCFOptionsGroup, TfUses, JCFOptionUses, JCFOptionFindAndReplace);
end.
