{  $Id$  }
{
 /***************************************************************************
                            checklfmdlg.pas
                            ---------------

 ***************************************************************************/

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
}
unit CheckLFMDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CodeCache,
  CodeToolManager, LFMTrees, OutputFilter, IDEOptionDefs;

type
  TCheckLFMDialog = class(TForm)
    procedure CheckLFMDialogCREATE(Sender: TObject);
  private
    procedure SetupComponents;
  public
  end;
  
function CheckLFMBuffer(PascalBuffer, LFMBuffer: TCodeBuffer;
  const OnOutput: TOnOutputString): boolean;

implementation

function CheckLFMBuffer(PascalBuffer, LFMBuffer: TCodeBuffer;
  const OnOutput: TOnOutputString): boolean;
var
  LFMTree: TLFMTree;
  
  procedure WriteLFMErrors;
  var
    CurError: TLFMError;
    Dir: String;
    Msg: String;
    Filename: String;
  begin
    if not Assigned(OnOutput) then exit;
    CurError:=LFMTree.FirstError;
    Dir:=ExtractFilePath(LFMBuffer.Filename);
    Filename:=ExtractFilename(LFMBuffer.Filename);
    while CurError<>nil do begin
      Msg:=Filename
           +'('+IntToStr(CurError.Caret.Y)+','+IntToStr(CurError.Caret.X)+')'
           +' Error: '
           +CurError.ErrorMessage;
      writeln('WriteLFMErrors ',Msg);
      OnOutput(Msg,Dir);
      CurError:=CurError.NextError;
    end;
  end;
  
begin
  Result:=CodeToolBoss.CheckLFM(PascalBuffer,LFMBuffer,LFMTree);
  try
    if Result then exit;
    WriteLFMErrors;
    // ToDo: open wizard for automatic repair
  finally
    LFMTree.Free;
  end;
end;

{ TCheckLFMDialog }

procedure TCheckLFMDialog.CheckLFMDialogCREATE(Sender: TObject);
begin
  Caption:='Fix LFM file';
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,600,400);
  SetupComponents;
end;

procedure TCheckLFMDialog.SetupComponents;
begin

end;

initialization
  {$I checklfmdlg.lrs}

end.

