{ $Id$ }
{               ----------------------------------------------  
                 callstackdlg.pp  -  Overview of the callstack 
                ---------------------------------------------- 
 
 @created(Sun Apr 28th WET 2002)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

 This unit contains the Call Stack debugger dialog.
 
 
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
unit CallStackDlg;

{$mode objfpc}{$H+}

interface

uses
  LResources, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Debugger, DebuggerDlg;

type
  TCallStackDlg = class(TDebuggerDlg)
    lvCallStack: TListView;
    procedure lvCallStackDBLCLICK(Sender: TObject);
  private  
    procedure CallStackChanged(Sender: TObject);
  protected
    procedure SetDebugger(const ADebugger: TDebugger); override;
  public
  published         
    // publish some properties until fpcbug #1888 is fixed
    property Top;
    property Left;
    property Width; 
    property Height; 
    property Caption;
  end;


implementation

{ TCallStackDlg }

procedure TCallStackDlg.lvCallStackDBLCLICK(Sender: TObject);
var
  CurItem: TListItem;
  Filename: String;
  Line: Integer;
begin
  CurItem:=lvCallStack.Selected;
  if CurItem=nil then exit;
  Filename:=CurItem.Caption;
  if DoGetFullDebugFilename(Filename,true)<>mrOk then exit;
  Line:=StrToIntDef(CurItem.SubItems[0],0);
  DoJumpToCodePos(Filename,Line,0);
end;

procedure TCallStackDlg.CallStackChanged(Sender: TObject);
var
  n, m: Integer;                               
  Item: TListItem;
  S: String;   
  Entry: TDBGCallStackEntry;
begin       
  if Debugger=nil then begin
    lvCallStack.Items.Clear;
    exit;
  end;

  // Reuse entries, so add and remove only                    
  // Remove unneded
  for n := lvCallStack.Items.Count - 1 downto Debugger.CallStack.Count do
    lvCallStack.Items.Delete(n);

  // Add needed
  for n := lvCallStack.Items.Count to Debugger.CallStack.Count - 1 do
  begin
    Item := lvCallStack.Items.Add;
    Item.SubItems.Add('');
    Item.SubItems.Add('');
  end;

  for n := 0 to lvCallStack.Items.Count - 1 do  
  begin
    Item := lvCallStack.Items[n];
    Entry := Debugger.CallStack.Entries[n];
    Item.Caption := Entry.Source;
    Item.SubItems[0] := IntToStr(Entry.Line);
    S := '';
    for m := 0 to Entry.ArgumentCount - 1 do
    begin
      if S <> '' 
      then S := S + ', ';
      S := S + Entry.ArgumentValues[m];
    end;                               
    if S <> ''
    then S := '(' + S + ')';
    Item.SubItems[1] := Entry.FunctionName + S;
  end;                                 
end;

procedure TCallStackDlg.SetDebugger(const ADebugger: TDebugger); 
begin
  if ADebugger <> Debugger
  then begin
    if Debugger <> nil
    then begin
      Debugger.CallStack.OnChange := nil;
    end;
    inherited;
    if Debugger <> nil
    then begin
      Debugger.CallStack.OnChange := @CallStackChanged;
      CallStackChanged(Debugger.CallStack);
    end;
  end
  else inherited;
end;

initialization
  {$I callstackdlg.lrs}

end.

{ =============================================================================
  $Log$
  Revision 1.3  2003/05/29 23:14:17  mattias
  implemented jump to code on double click for breakpoints and callstack dlg

  Revision 1.2  2002/05/10 06:57:47  lazarus
  MG: updated licenses

  Revision 1.1  2002/04/30 15:57:39  lazarus
  MWE:
    + Added callstack object and dialog
    + Added checks to see if debugger = nil
    + Added dbgutils

}
