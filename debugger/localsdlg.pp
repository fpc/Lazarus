{ $Id$ }
{               ----------------------------------------------  
                 localsdlg.pp  -  Overview of local variables 
                ---------------------------------------------- 
 
 @created(Thu Mar 14st WET 2002)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

 This unit contains the Locals debugger dialog.
 
 
/*************************************************************************** 
 *                                                                         * 
 *   This program is free software; you can redistribute it and/or modify  * 
 *   it under the terms of the GNU General Public License as published by  * 
 *   the Free Software Foundation; either version 2 of the License, or     * 
 *   (at your option) any later version.                                   * 
 *                                                                         * 
 ***************************************************************************/ 
} 
unit localsdlg;

{$mode objfpc}{$H+}

interface

uses
  LResources, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Debugger, DebuggerDlg;

type
  TLocalsDlg = class(TDebuggerDlg)
    lvLocals: TListView;
  private  
    procedure LocalsChanged(Sender: TObject);
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

{ TLocalsDlg }

procedure TLocalsDlg.LocalsChanged(Sender: TObject);
var
  n, idx: Integer;                               
  List: TStringList;
  Item: TListItem;
  S: String;
begin                                        
  List := TStringList.Create;
  //Get existing items
  for n := 0 to lvLocals.Items.Count - 1 do  
  begin
    Item := lvLocals.Items[n];
    S := Item.Caption;
    S := UpperCase(S);
    List.AddObject(S, Item);
  end;                                 
  
  // add/update entries
  for n := 0 to Debugger.Locals.Count - 1 do
  begin
    idx := List.IndexOf(Uppercase(Debugger.Locals.Names[n]));
    if idx = -1 
    then begin
      // New entry
      Item := lvLocals.Items.Add;
      Item.Caption := Debugger.Locals.Names[n];
      Item.SubItems.Add(Debugger.Locals.Values[n]);
    end
    else begin
      // Existing entry
      Item := TListItem(List.Objects[idx]);
      Item.SubItems[0] := Debugger.Locals.Values[n];
      List.Delete(idx);
    end;
  end;
  
  // remove obsolete entries
  for n := 0 to List.Count - 1 do 
    lvLocals.Items.Delete(TListItem(List.Objects[n]).Index);
    
  List.Free;
end;

procedure TLocalsDlg.SetDebugger(const ADebugger: TDebugger); 
begin
  if ADebugger <> Debugger
  then begin
    if Debugger <> nil
    then begin
      Debugger.Locals.OnChange := nil;
    end;
    inherited;
    if Debugger <> nil
    then begin
      Debugger.Locals.OnChange := @LocalsChanged;
      LocalsChanged(Debugger.Locals);
    end;
  end
  else inherited;
end;

initialization
  {$I localsdlg.lrs}

end.

{ =============================================================================
  $Log$
  Revision 1.2  2002/04/24 20:42:29  lazarus
  MWE:
    + Added watches
    * Updated watches and watchproperty dialog to load as resource
    = renamed debugger resource files from *.lrc to *.lrs
    * Temporary fixed language problems on GDB (bug #508)
    * Made Debugmanager dialog handling more generic

  Revision 1.1  2002/03/23 15:54:30  lazarus
  MWE:
    + Added locals dialog
    * Modified breakpoints dialog (load as resource)
    + Added generic debuggerdlg class
    = Reorganized main.pp, all debbugger relater routines are moved
      to include/ide_debugger.inc

}