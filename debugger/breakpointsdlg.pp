{ $Id$ }
{               ----------------------------------------------
                 breakpointsdlg.pp  -  Overview of breeakponts
                ----------------------------------------------

 @created(Fri Dec 14st WET 2001)
 @lastmod($Date$)
 @author(Shane Miller)
 @author(Marc Weustink <marc@@dommelstein.net>)

 This unit contains the Breakpoint dialog.


/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}

unit breakpointsdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources, StdCtrls,
  Buttons, Extctrls, Menus, ComCtrls, Debugger, DebuggerDlg;

type
  TBreakPointsDlg = class(TDebuggerDlg)
    lvBreakPoints: TListView;
    procedure lvBreakPointsClick(Sender: TObject);
    mnuPopup: TPopupMenu;
    popAdd: TMenuItem;
    popAddSourceBP: TMenuItem;
    procedure popAddSourceBPClick(Sender: TObject);
    popDeleteAll: TMenuItem;
    procedure popDeleteAllClick(Sender: TObject);
    popDisableAll: TMenuItem;
    procedure popDisableAllClick(Sender: TObject);
    popEnableAll: TMenuItem;
    procedure popEnableAllClick(Sender: TObject);
    mnuPopSelected: TPopupMenu;
    popEnabled: TMenuItem;
    procedure popEnabledClick(Sender: TObject);
    popDelete: TMenuItem;
    procedure popDeleteClick(Sender: TObject);
    popProperties: TMenuItem;
    procedure popPropertiesClick(Sender: TObject);
  private
    procedure BreakPointAdd(const ASender: TDBGBreakPoints; const ABreakpoint: TDBGBreakPoint);
    procedure BreakPointUpdate(const ASender: TDBGBreakPoints; const ABreakpoint: TDBGBreakPoint);
    procedure BreakPointRemove(const ASender: TDBGBreakPoints; const ABreakpoint: TDBGBreakPoint);

    procedure UpdateItem(const AItem: TListItem; const ABreakpoint: TDBGBreakPoint);
  protected
    procedure Loaded; override;
    procedure SetDebugger(const ADebugger: TDebugger); override;
  public
  published
    property Dummy: Boolean; // insert some dummies until fpcbug #1888 is fixed
  end;


implementation

procedure TBreakPointsDlg.BreakPointAdd(const ASender: TDBGBreakPoints; const ABreakpoint: TDBGBreakPoint);
var
  Item: TListItem;
  n: Integer;
begin
  Item := lvBreakPoints.Items.FindData(ABreakpoint);
  if Item = nil
  then begin
    Item := lvBreakPoints.Items.Add;
    Item.Data := ABreakPoint;
    for n := 0 to 5 do
      Item.SubItems.Add('');
  end;

  UpdateItem(Item, ABreakPoint);
end;

procedure TBreakPointsDlg.BreakPointUpdate(const ASender: TDBGBreakPoints; const ABreakpoint: TDBGBreakPoint);
var
  Item: TListItem;
begin
  if ABreakpoint = nil then Exit;

  Item := lvBreakPoints.Items.FindData(ABreakpoint);
  if Item = nil
  then BreakPointAdd(ASender, ABreakPoint)
  else UpdateItem(Item, ABreakPoint);
end;

procedure TBreakPointsDlg.BreakPointRemove(const ASender: TDBGBreakPoints; const ABreakpoint: TDBGBreakPoint);
begin
  lvBreakPoints.Items.FindData(ABreakpoint).Free;
end;

procedure TBreakPointsDlg.Loaded;
begin
  inherited Loaded;
  
  // Not yet through resources
  mnuPopUp.Items.Add(popAdd);
  popAdd.Add(popAddSourceBP);
  mnuPopUp.Items.Add(popDeleteAll);
  mnuPopUp.Items.Add(popDisableAll);
  mnuPopUp.Items.Add(popEnableAll);

  mnuPopSelected.Items.Add(popEnabled);
  mnuPopSelected.Items.Add(popDelete);
  mnuPopSelected.Items.Add(popProperties);
end;

procedure TBreakPointsDlg.lvBreakPointsClick(Sender: TObject);
var
  Item: TListItem;
begin
  Item := lvBreakPoints.Selected;
  if Item = nil 
  then lvBreakPoints.PopupMenu := mnuPopup
  else lvBreakPoints.PopupMenu := mnuPopSelected;
end;

procedure TBreakPointsDlg.popAddSourceBPClick(Sender: TObject);
begin
end;

procedure TBreakPointsDlg.popDeleteAllClick(Sender: TObject);
var
  n: Integer;
begin                                    
  for n := lvBreakPoints.Items.Count - 1 downto 0 do
    TDBGBreakPoint(lvBreakPoints.Items[n].Data).Free;
end;

procedure TBreakPointsDlg.popDeleteClick(Sender: TObject);
begin
end;

procedure TBreakPointsDlg.popDisableAllClick(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
begin
  for n := 0 to lvBreakPoints.Items.Count - 1 do
  begin
    Item := lvBreakPoints.Items[n];
    if Item.Data <> nil
    then TDBGBreakPoint(Item.Data).Enabled := False;
  end;
end;

procedure TBreakPointsDlg.popEnableAllClick(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
begin
  for n := 0 to lvBreakPoints.Items.Count - 1 do
  begin
    Item := lvBreakPoints.Items[n];
    if Item.Data <> nil
    then TDBGBreakPoint(Item.Data).Enabled := True;
  end;
end;

procedure TBreakPointsDlg.popEnabledClick(Sender: TObject);
begin
end;

procedure TBreakPointsDlg.popPropertiesClick(Sender: TObject);
begin     
end;

procedure TBreakPointsDlg.SetDebugger(const ADebugger: TDebugger);
begin
  if ADebugger <> Debugger
  then begin
    if Debugger <> nil
    then begin
      Debugger.Breakpoints.OnAdd := nil;
      Debugger.Breakpoints.OnUpdate := nil;
      Debugger.Breakpoints.OnRemove := nil;
    end;
    inherited;
    if Debugger <> nil
    then begin
      Debugger.Breakpoints.OnAdd := @BreakPointAdd;
      Debugger.Breakpoints.OnUpdate := @BreakPointUpdate;
      Debugger.Breakpoints.OnRemove := @BreakPointRemove;
    end;
  end
  else inherited;
end;

procedure TBreakPointsDlg.UpdateItem(const AItem: TListItem; const ABreakpoint: TDBGBreakPoint);
const
  DEBUG_ACTION: array[TDBGBreakPointAction] of string = ('Break', 'Enable Group', 'Disable Group');
  //                 enabled  valid
  DEBUG_STATE: array[Boolean, Boolean] of String = (('?', ''), ('!', '*'));
var
  Action: TDBGBreakPointAction;
  S: String;
begin
// Filename/Address
// Line/Length
// Condition
// Action
// Pass Count
// Group

  AItem.Caption := DEBUG_STATE[ABreakpoint.Enabled, ABreakpoint.Valid];
  AItem.SubItems[0] := ABreakpoint.Source;
  if ABreakpoint.Line > 0
  then AItem.SubItems[1] := IntToStr(ABreakpoint.Line)
  else AItem.SubItems[1] := '';
  AItem.SubItems[2] := ABreakpoint.Expression;
  S := '';
  for Action := Low(Action) to High(Action) do
    if Action in ABreakpoint.Actions
    then begin
      if S <> '' then s := S + ', ';
      S := S + DEBUG_ACTION[Action]
    end;
  AItem.SubItems[3]  := S;
  AItem.SubItems[4] := IntToStr(ABreakpoint.HitCount);
  if ABreakpoint.Group = nil
  then AItem.SubItems[5] := ''
  else AItem.SubItems[5] := ABreakpoint.Group.Name;
end;


initialization
  {$I breakpointsdlg.lrc}

end.

{ =============================================================================
  $Log$
  Revision 1.2  2002/03/23 15:54:30  lazarus
  MWE:
    + Added locals dialog
    * Modified breakpoints dialog (load as resource)
    + Added generic debuggerdlg class
    = Reorganized main.pp, all debbugger relater routines are moved
      to include/ide_debugger.inc

  Revision 1.1  2002/03/12 23:55:36  lazarus
  MWE:
    * More delphi compatibility added/updated to TListView
    * Introduced TDebugger.locals
    * Moved breakpoints dialog to debugger dir
    * Changed breakpoints dialog to read from resource

}