{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

 Author: Mattias Gaertner

 Abstract:
   IDE dialog showing stats what is modified.
}
unit IDEModifiedInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, Project, SourceEditor;

type

  { TIDEModifiedInfoDialog }

  TIDEModifiedInfoDialog = class(TForm)
    ProjectMemo: TMemo;
    PageControl1: TPageControl;
    ProjectTabSheet: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdateProjectMemo;
    procedure GatherProject(AProject: TProject; sl: TStrings);
  public
  end;

function ShowIDEModifiedInfo: TModalResult;

implementation

function ShowIDEModifiedInfo: TModalResult;
var
  Dlg: TIDEModifiedInfoDialog;
begin
  Dlg:=TIDEModifiedInfoDialog.Create(nil);
  try
    Result:=Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

{$R *.lfm}

{ TIDEModifiedInfoDialog }

procedure TIDEModifiedInfoDialog.FormCreate(Sender: TObject);
begin
  Caption:='Modified items in IDE';

  UpdateProjectMemo;
  PageControl1.PageIndex:=0;
end;

procedure TIDEModifiedInfoDialog.UpdateProjectMemo;
var
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    GatherProject(Project1,sl);

    ProjectMemo.Lines.Assign(sl);
  finally
    sl.Free;
  end;
end;

procedure TIDEModifiedInfoDialog.GatherProject(AProject: TProject; sl: TStrings
  );
var
  aFile: TUnitInfo;
  HeaderWritten: Boolean;
  s: String;
begin
  // summary
  if AProject.Modified then
    sl.Add('Project.Modified');
  if AProject.SessionModified then
    sl.Add('Project.SessionModified');
  if Project1.SomethingModified(true,false) then
    sl.Add('Project.SomethingModified Data');
  if Project1.SomethingModified(false,true) then
    sl.Add('Project.SomethingModified Session');
  if SourceEditorManager.SomethingModified(false) then
    sl.Add('SourceEditorManager.SomethingModified');
  if AProject.BuildModes.IsModified(false) then
    sl.Add('Project.BuildModes.IsModified data');
  if AProject.BuildModes.IsModified(true) then
    sl.Add('Project.BuildModes.IsModified session');

  // details
  HeaderWritten:=false;
  aFile:=AProject.FirstPartOfProject;
  while aFile<>nil do begin
    if aFile.Modified or aFile.SessionModified
    or ((aFile.Source<>nil) and aFile.Source.Modified)
    then begin
      if not HeaderWritten then begin
        sl.Add('');
        sl.Add('Project units:');
        s:=aFile.GetShortFilename(true);
        if aFile.Modified then
          s:=s+' Modified';
        if aFile.SessionModified then
          s:=s+' SessionModified';
        if (aFile.Source<>nil) and (aFile.Source.Modified) then
          s:=s+' Source.Modified';
        sl.Add(s);
      end;
    end;
    aFile:=aFile.NextPartOfProject;
  end;
end;

end.

