{ Copyright (C) 2008 Darius Blaszijk

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit SVNDiffForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, Process, Buttons, LCLProc;

type

  { TSVNDiffFrm }

  TSVNDiffFrm = class(TForm)
    SaveButton: TBitBtn;
    ButtonPanel: TButtonPanel;
    SaveDialog: TSaveDialog;
    SVNDiffMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    { private declarations }
    FSwitches: string;
    FRepoPath: string;
  public
    { public declarations }
    procedure Execute(Data: PtrInt);
  end;

procedure ShowSVNDiffFrm(ASwitches, ARepoPath: string);

implementation

uses
  SVNClasses;

procedure ShowSVNDiffFrm(ASwitches, ARepoPath: string);
var
  SVNDiffFrm: TSVNDiffFrm;
begin
  SVNDiffFrm := TSVNDiffFrm.Create(nil);

  SVNDiffFrm.FRepoPath:=ARepoPath;
  SVNDiffFrm.FSwitches:=ASwitches;
  SVNDiffFrm.ShowModal;

  SVNDiffFrm.Free;
end;

{ TSVNDiffFrm }

procedure TSVNDiffFrm.FormShow(Sender: TObject);
begin
  Caption := Format(rsLazarusSVNDiff, [FRepoPath]);
  Application.QueueAsyncCall(@Execute, 0);
end;

procedure TSVNDiffFrm.FormCreate(Sender: TObject);
begin
  SaveButton.Caption:=rsSave;
end;

procedure TSVNDiffFrm.SaveButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    SVNDiffMemo.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TSVNDiffFrm.Execute(Data: PtrInt);
var
  AProcess: TProcess;
  BytesRead: LongInt;
  n: LongInt;
  M: TMemoryStream;
begin
  AProcess := TProcess.Create(nil);
  AProcess.CommandLine := SVNExecutable + ' diff ' + FSwitches + ' ' + FRepoPath + ' --non-interactive';
  debugln('TSVNDiffFrm.Execute commandline=', AProcess.CommandLine);
  AProcess.Options := AProcess.Options + [poUsePipes, poStdErrToOutput];
  AProcess.ShowWindow := swoHIDE;
  AProcess.Execute;

  M := TMemoryStream.Create;
  BytesRead := 0;

  while AProcess.Running do
  begin
    // make sure we have room
    M.SetSize(BytesRead + READ_BYTES);

    // try reading it
    n := AProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0
    then begin
      Inc(BytesRead, n);
    end
    else begin
      // no data, wait 100 ms
      Sleep(100);
    end;
  end;
  // read last part
  repeat
    // make sure we have room
    M.SetSize(BytesRead + READ_BYTES);
    // try reading it
    n := AProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0
    then begin
      Inc(BytesRead, n);
    end;
  until n <= 0;
  M.SetSize(BytesRead);

  SVNDiffMemo.Lines.LoadFromStream(M);
  SVNDiffMemo.Lines.Text := ReplaceLineEndings(SVNDiffMemo.Lines.Text, LineEnding);

  AProcess.Free;
end;

initialization
  {$I svndiffform.lrs}

end.

