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
    An IDE window for context sensitive codetools.
}
unit CodyFrm;

{$mode objfpc}{$H+}

{$R *.lfm}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  // codetools
  FileProcs, CodeToolManager, SourceLog, CodeCache,
  // IDEIntf
  LazIDEIntf, SrcEditorIntf, IDEDialogs,
  // cody
  CodyStrConsts;

type

  { TCody }

  TCody = class
  public
    procedure DecodeLoaded(Sender: TSourceLog; const Filename: string;
                           var Source, DiskEncoding, MemEncoding: string);
  end;

  TCodyWindow = class(TForm)
  private
  public
  end;

var
  Cody: TCody;
  CodyWindow: TCodyWindow;

procedure RemoveWithBlockCmd(Sender: TObject);
procedure InsertFileAtCursor(Sender: TObject);

implementation

procedure RemoveWithBlockCmd(Sender: TObject);

  procedure ErrorNotInWithVar;
  begin
    IDEMessageDialog(crsCWError,
      crsCWPleasePlaceTheCursorOfTheSourceEditorOnAWithVariab,
      mtError,[mbCancel]);
  end;

var
  SrcEdit: TSourceEditorInterface;
begin
  // commit changes form source editor to codetools
  if not LazarusIDE.BeginCodeTools then exit;
  // check context at cursor
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then begin
    ErrorNotInWithVar;
    exit;
  end;
  if not CodeToolBoss.RemoveWithBlock(SrcEdit.CodeToolsBuffer as TCodeBuffer,
    SrcEdit.CursorTextXY.X,SrcEdit.CursorTextXY.Y)
  then begin
    // syntax error or not in a class
    if CodeToolBoss.ErrorMessage<>'' then
      LazarusIDE.DoJumpToCodeToolBossError
    else
      ErrorNotInWithVar;
    exit;
  end;
end;

procedure InsertFileAtCursor(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  Filter: String;
  Filename: String;
  Code: TCodeBuffer;
  SrcEdit: TSourceEditorInterface;
begin
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then exit;

  OpenDialog:=TOpenDialog.Create(nil);
  Code:=nil;
  try
    InitIDEFileDialog(OpenDialog);
    OpenDialog.Title:='Select file to insert at cursor';
    OpenDialog.Options:=OpenDialog.Options+[ofFileMustExist];
    Filter:='Pascal' + ' (*.pas;*.pp)|*.pas;*.pp';
    Filter:=Filter+'|'+'All files' + ' (' + GetAllFilesMask + ')|' + GetAllFilesMask;
    OpenDialog.Filter:=Filter;
    if not OpenDialog.Execute then exit;
    Filename:=OpenDialog.FileName;
    if not FileIsText(Filename) then begin
      if IDEMessageDialog('Warning','The file seems to be a binary. Proceed?',
        mtConfirmation,[mbOk,mbCancel])<>mrOK then exit;
    end;
    Code:=TCodeBuffer.Create;
    Code.Filename:=Filename;
    Code.OnDecodeLoaded:=@Cody.DecodeLoaded;
    if not Code.LoadFromFile(Filename) then begin
      IDEMessageDialog('Error','Unable to load file "'+Filename+'"'#13
      +Code.LastError,
        mtError,[mbCancel]);
      exit;
    end;

    SrcEdit.Selection:=Code.Source;
  finally
    OpenDialog.Free;
    Code.Free;
  end;
end;

{ TCody }

procedure TCody.DecodeLoaded(Sender: TSourceLog; const Filename: string;
  var Source, DiskEncoding, MemEncoding: string);
begin
  //debugln(['TCody.DecodeLoaded ',Filename]);
  if (Sender is TCodeBuffer)
  and Assigned(CodeToolBoss.SourceCache.OnDecodeLoaded) then
    CodeToolBoss.SourceCache.OnDecodeLoaded(TCodeBuffer(Sender),Filename,
      Source,DiskEncoding,MemEncoding);
end;

initialization
  Cody:=TCody.Create;
finalization
  FreeAndNil(Cody);

end.

