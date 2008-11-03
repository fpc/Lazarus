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
    FRepositoryPath: string;
    { private declarations }
    FSwitches: string;
  public
    { public declarations }
    procedure Execute(Data: PtrInt);
    property RepositoryPath: string read FRepositoryPath write FrepositoryPath;
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

  SVNDiffFrm.RepositoryPath:=ARepoPath;
  SVNDiffFrm.FSwitches:=ASwitches;
  SVNDiffFrm.ShowModal;

  SVNDiffFrm.Free;
end;

{ TSVNDiffFrm }

procedure TSVNDiffFrm.FormShow(Sender: TObject);
begin
  Caption := Format(rsLazarusSVNDiff, [RepositoryPath]);
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
begin
  CmdLineToMemo(SVNExecutable + ' diff ' + FSwitches + ' ' + RepositoryPath + ' --non-interactive',
                SVNDiffMemo);
end;

initialization
  {$I svndiffform.lrs}

end.

