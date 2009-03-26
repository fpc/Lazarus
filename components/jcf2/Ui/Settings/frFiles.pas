{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frLog.pas, released April 2000.
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

unit frFiles;

{$I JcfGlobal.inc}

interface

uses
  { delphi }
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, Graphics,
  { local }
  frmBaseSettingsFrame, JvMemo, JvExStdCtrls;

type
  TfFiles = class(TfrSettingsFrame)
    lblStatus: TLabel;
    lblDate: TLabel;
    lblVersion: TLabel;
    lblDescription: TLabel;
    mDescription: TJvMemo;
    lblFormatFileName: TLabel;
    procedure FrameResize(Sender: TObject);
  private


  public

    procedure Read; override;
    procedure Write; override;
  end;

implementation

uses
  { jcl }
  JclFileUtils,
  { local }
  JcfRegistrySettings, JcfSettings;

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

procedure TfFiles.Read;
var
  lcSet: TJCFRegistrySettings;
begin
  { from the registry, about the file }
  lcSet := GetRegSettings;
  //lblFormatFileName.Caption := 'Format file is ' + lcSet.FormatConfigFileName;
  lblFormatFileName.Caption := PathCompactPath(lblFormatFileName.Canvas.Handle, 'Format file is ' + lcSet.FormatConfigFileName, 450, cpCenter);

  if not FileExists(lcSet.FormatConfigFileName) then
  begin
    lblStatus.Caption := 'File not found';
  end
  else
  begin
    if FileIsReadOnly(lcSet.FormatConfigFileName) then
    begin
      lblStatus.Caption     := 'File is read only';
      mDescription.ReadOnly := True;
      mDescription.ParentColor := True;
    end
    else
    begin
      lblStatus.Caption     := 'File is writable';
      mDescription.ReadOnly := False;
    end;

    { from the file, about itself}
    lblDate.Caption    := 'Date file written: ' +
      FormatDateTime(ShortDateFormat + ' ' + ShortTimeFormat,
      FormatSettings.WriteDateTime);
    lblVersion.Caption := 'Version that wrote this file: ' + FormatSettings.WriteVersion;
    mDescription.Text  := FormatSettings.Description;

  end;
end;

procedure TfFiles.Write;
begin
  FormatSettings.Description := mDescription.Text;
end;

procedure TfFiles.FrameResize(Sender: TObject);
const
  SPACING = 8;
begin
  inherited;

  lblFormatFileName.Left  := SPACING;
  lblFormatFileName.Width := ClientWidth - (lblFormatFileName.Left + SPACING);

  // file name is varaible height due to wrap. Rest go below
  lblStatus.Left := SPACING;
  lblStatus.Top  := lblFormatFileName.Top + lblFormatFileName.Height + SPACING;

  lblDate.Left := SPACING;
  lblDate.Top  := lblStatus.Top + lblStatus.Height + SPACING;

  lblVersion.Left := SPACING;
  lblVersion.Top  := lblDate.Top + lblDate.Height + SPACING;

  lblDescription.Left := SPACING;
  lblDescription.Top  := lblVersion.Top + lblVersion.Height + SPACING;

  mDescription.Left   := SPACING;
  mDescription.Top    := lblDescription.Top + lblDescription.Height + SPACING;
  mDescription.Height := CLientHeight - (mDescription.Top + SPACING);

  mDescription.Left  := SPACING;
  mDescription.Width := ClientWidth - (mDescription.Left + SPACING);

end;

end.
