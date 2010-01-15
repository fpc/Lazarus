{ $Id$ }
{                       ------------------------------------------  
                        debugoutputform.pp  -  Shows target output 
                        ------------------------------------------ 
 
 @created(Wed Feb 25st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

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
unit DebugOutputForm;

{$mode objfpc}
{$H+}

interface

uses
  Classes, Graphics, Controls, Forms, Dialogs, Clipbrd,
  Buttons, StdCtrls, Menus, DebuggerDlg;

type

  { TDbgOutputForm }

  TDbgOutputForm = class(TDebuggerDlg)
    popCopyAll: TMenuItem;
    txtOutput: TMemo;
    mnuPopup: TPopupMenu;
    popClear: TMenuItem;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure popClearClick(Sender: TObject);
    procedure popCopyAllClick(Sender: TObject);
  private
  protected
    procedure Loaded; override;
  public
    procedure AddText(const AText: String);
    procedure Clear;
    procedure SetLogText(Lines: TStrings);
    procedure GetLogText(Lines: TStrings);
  end;

implementation

{$R *.lfm}

uses 
  LazarusIDEStrConsts;

procedure TDbgOutputForm.AddText(const AText: String);
begin
  txtOutput.Lines.Add(AText);
end;

procedure TDbgOutputForm.Clear;
begin             
  txtOutput.Lines.Clear; 
end;

procedure TDbgOutputForm.SetLogText(Lines: TStrings);
begin
  txtOutput.Lines.Assign(Lines);
end;

procedure TDbgOutputForm.GetLogText(Lines: TStrings);
begin
  Lines.Assign(txtOutput.Lines);
end;

procedure TDbgOutputForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TDbgOutputForm.FormCreate(Sender: TObject);
begin
  txtOutput.Lines.Clear;
  Caption:= lisMenuViewDebugOutput;

  popClear.Caption:=lisUIDClear;
  popCopyAll.Caption:=lisCopyAllOutputClipboard;
end;

procedure TDbgOutputForm.Loaded;
begin
  inherited Loaded;
  
  // Not yet through resources
  txtOutput.Scrollbars := ssBoth;  
end;

procedure TDbgOutputForm.popClearClick(Sender: TObject);
begin
  Clear;
end;

procedure TDbgOutputForm.popCopyAllClick(Sender: TObject);
begin
  Clipboard.AsText := txtOutput.Text;
end;

end.
