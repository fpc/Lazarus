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

  Author: Michael Van Canneyt
}
unit FrmExample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, EditBtn, StdCtrls, ButtonPanel;

type

  { TExampleForm }

  TExampleForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    EFileName: TFileNameEdit;
    LEFileName: TLabel;
    procedure ExampleFormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    function GetExampleDir: String;
    function GetExampleName: String;
    procedure SetExampleDir(const AValue: String);
    procedure SetExampleName(const AValue: String);
    function CheckFilePath(const AValue: String): boolean;
    { private declarations }
  public
    { public declarations }
    Property ExampleName : String Read GetExampleName Write SetExampleName;
    Property ExampleDir: String read GetExampleDir write SetExampleDir;
  end; 

var
  ExampleForm: TExampleForm;

implementation

{$R *.lfm}

{ TExampleForm }

procedure TExampleForm.ExampleFormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  S: String;
begin
  if Sender = nil then exit;
  if ModalResult = mrOk then begin
    S := ExtractFilePath(EFilename.Text);
    CanClose := CheckFilePath(S);
  end;
end;

function TExampleForm.GetExampleDir: String;
begin
  Result := EFileName.InitialDir;
end;

function TExampleForm.GetExampleName: String;
begin
  Result:=EFilename.Text; //EFileName.FileName;
  if ExampleDir<>'' then
    Result := ExtractRelativePath(ExampleDir, Result);
end;

procedure TExampleForm.SetExampleDir(const AValue: String);
begin
  EFileName.InitialDir := AValue;
end;

procedure TExampleForm.SetExampleName(const AValue: String);
begin
  EFileName.FileName:=AValue;
end;

function TExampleForm.CheckFilePath(const AValue: String): boolean;
begin
  Result := True;
  
  // Check Empty Path and filename
  if (Length(ExtractFilePath(AValue))=0) and FileExistsUTF8(ExampleDir+AValue) then
    exit;
  
  // Check partial file path within ExampleDir
  if FileExistsUTF8(ExampleDir + AValue) then
    exit;
  
  // it might be a full path
  if FileExistsUTF8(AValue) and (Pos(ExampleDir, AValue)<>0) then
    exit;

  Result := false;
  ShowMessage('Invalid file or path');
end;

end.

