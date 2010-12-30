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
  Classes, SysUtils, FileUtil, LResources, Forms, Dialogs,
  ButtonPanel, StdCtrls, Buttons, LazIDEIntf;

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
    destructor Destroy; override;
  private
    FFileList: TStringList;
    FSwitches: string;
    FRepoBaseDir: string;
  public
    procedure Execute(Data: PtrInt);
    {list of filenames with absolute path}
    property FileList: TStringList read FFileList write FFileList;
    {switches for the diff command}
    property Switches: string read FSwitches write FSwitches;
  end;

procedure ShowSVNDiffFrm(ASwitches, AFileName: string);
procedure ShowSVNDiffFrm(ASwitches: string; AFileList: TStringList); overload;

implementation

{$R *.lfm}

uses
  SVNClasses;

procedure ShowSVNDiffFrm(ASwitches, AFileName: string);
var
  List: TStringList;
begin
  List := TStringList.Create;
  List.Append(AFileName);
  ShowSVNDiffFrm(ASwitches, List);
end;

procedure ShowSVNDiffFrm(ASwitches: string; AFileList: TStringList);
var
  SVNDiffFrm: TSVNDiffFrm;
begin
  SVNDiffFrm := TSVNDiffFrm.Create(nil);

  SVNDiffFrm.FileList:=AFileList;
  SVNDiffFrm.Switches:=ASwitches;
  SVNDiffFrm.ShowModal;

  SVNDiffFrm.Free;
end;

{ TSVNDiffFrm }

procedure TSVNDiffFrm.FormShow(Sender: TObject);
var
  CaptionName: string;
begin
  FRepoBaseDir := LazarusIDE.ActiveProject.CustomSessionData.Values[SVN_REPOSITORY];
  if FileList.Count = 1 then
    CaptionName := CreateRelativePath(FileList.Strings[0], FRepoBaseDir, false)
  else
    CaptionName := FRepoBaseDir;
  Caption := Format(rsLazarusSVNDiff, [CaptionName]);
  Application.QueueAsyncCall(@Execute, 0);
end;

procedure TSVNDiffFrm.FormCreate(Sender: TObject);
begin
  SaveButton.Enabled := False;
  SaveButton.Caption := rsSave;
end;

procedure TSVNDiffFrm.SaveButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    SVNDiffMemo.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TSVNDiffFrm.Execute(Data: PtrInt);
var
  i: Integer;
  FileNames: String; // all filenames concatenated for the command line
begin
  FileNames := '';
  for i := 0 to FileList.Count - 1 do begin
    if FileExists(FileList.Strings[i]) then
      FileNames += ' "' + CreateRelativePath(FileList.Strings[i], FRepoBaseDir, False) + '"'
    else
      FileNames += ' "' + FileList.Strings[i] + '"' // might be a http:// url

  end;

  // in the previous step we made the filenames relative because we don't
  // want absolute paths in our diff files. Now we must make sure we execute
  // the svn diff command from within the repository base directory.
  chdir(FRepoBaseDir);
  CmdLineToMemo(SVNExecutable + ' diff ' + FSwitches + FileNames + ' --non-interactive',
                SVNDiffMemo);
  SaveButton.Enabled := True;
end;

destructor TSVNDiffFrm.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
end;

end.

