{ $Id$ }
{               ----------------------------------------------  
                 callstackdlg.pp  -  Overview of the callstack 
                ---------------------------------------------- 
 
 @created(Sun Apr 28th WET 2002)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

 This unit contains the Call Stack debugger dialog.
 
 
/*************************************************************************** 
 *                                                                         * 
 *   This program is free software; you can redistribute it and/or modify  * 
 *   it under the terms of the GNU General Public License as published by  * 
 *   the Free Software Foundation; either version 2 of the License, or     * 
 *   (at your option) any later version.                                   * 
 *                                                                         * 
 ***************************************************************************/ 
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

procedure TCallStackDlg.CallStackChanged(Sender: TObject);
var
  n, m: Integer;                               
  Item: TListItem;
  S: String;   
  Entry: TDBGCallStackEntry;
begin       
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
  Revision 1.1  2002/04/30 15:57:39  lazarus
  MWE:
    + Added callstack object and dialog
    + Added checks to see if debugger = nil
    + Added dbgutils

}