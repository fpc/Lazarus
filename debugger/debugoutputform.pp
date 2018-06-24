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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit DebugOutputForm;

(* DBG_WITH_DEBUGGER_DEBUG:
    This enables direct access to the debugger.
   WARNING:
    - This bypasses some of the internals of the debugger.
    - It does intentionally no check or validation
    - Using this feature without full knowledge of all internals of the debugger,
      can *HANG* or *CRASH* the debugger or the entire IDE.
*)

{$mode objfpc}
{$H+}

interface

uses
  Classes, Controls, Forms, Clipbrd,
  IDEWindowIntf,
  BaseDebugManager, StdCtrls, Menus, ExtCtrls, DebuggerDlg
  {$IFDEF DBG_WITH_DEBUGGER_DEBUG}
  , CmdLineDebugger
  {$ENDIF}
  ;

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
    {$IFDEF DBG_WITH_DEBUGGER_DEBUG}
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    {$ENDIF}
  protected
    procedure Loaded; override;
  public
    {$IFDEF DBG_WITH_DEBUGGER_DEBUG}
    constructor Create(TheOwner: TComponent); override;
    {$ENDIF}
    procedure AddText(const AText: String);
    procedure Clear;
    procedure SetLogText(Lines: TStrings);
    procedure GetLogText(Lines: TStrings);
  end;

implementation

{$R *.lfm}

uses 
  LazarusIDEStrConsts;

var
  DbgOutputDlgWindowCreator: TIDEWindowCreator;

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


{$IFDEF DBG_WITH_DEBUGGER_DEBUG}
constructor TDbgOutputForm.Create(TheOwner: TComponent);
var
  p: TPanel;
  b: TButton;
begin
  inherited;
  p := TPanel.Create(Self);
  p.Parent := Self;
  p.Align := alBottom;
  p.AutoSize := True;
  p.Caption := '';
  Edit1 := TEdit.Create(Self);
  Edit1.Parent := p;
  Edit1.Align := alClient;
  b := TButton.Create(Self);
  b.Parent := p;
  b.Align := alRight;
  b.OnClick := @Button1Click;
  b.Caption := 'Execute';
  b.AutoSize := True;
end;
{$ENDIF}

procedure TDbgOutputForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

{$IFDEF DBG_WITH_DEBUGGER_DEBUG}
procedure TDbgOutputForm.Button1Click(Sender: TObject);
begin
  DebugBoss.Debugger.TestCmd(Edit1.Text);
end;
{$ENDIF}

procedure TDbgOutputForm.FormCreate(Sender: TObject);
begin
  txtOutput.Lines.Clear;
  Caption:= lisMenuViewDebugOutput;

  popClear.Caption:=lisClear;
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

initialization

  DbgOutputDlgWindowCreator := IDEWindowCreators.Add(DebugDialogNames[ddtOutput]);
  DbgOutputDlgWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  DbgOutputDlgWindowCreator.CreateSimpleLayout;

end.
