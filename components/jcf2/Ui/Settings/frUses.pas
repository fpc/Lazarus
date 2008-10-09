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
  { delphi }
  Classes, Controls, Forms,
  StdCtrls,
  { local }
  frmBaseSettingsFrame;

type
  TfUses = class(TfrSettingsFrame)
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

uses
  { local }JcfHelp, JcfSettings;

constructor TfUses.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY_FIND_AND_REPLACE_USES;
end;


procedure TfUses.Read;
begin
  with FormatSettings.UsesClause do
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

procedure TfUses.Write;
begin
  with FormatSettings.UsesClause do
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

end.
