{ $Id$ }
{               ----------------------------------------------
                 breakpointsdlg.pp  -  Overview of breeakponts
                ----------------------------------------------

 @created(Fri Dec 14st WET 2001)
 @lastmod($Date$)
 @author(Shane Miller)
 @author(Marc Weustink <marc@@dommelstein.net>)

 This unit contains the Breakpoint dialog.


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

unit BreakPointsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources, StdCtrls,
  Buttons, Menus, ComCtrls, IDEProcs, Debugger, DebuggerDlg;

type
  TBreakPointsDlgState = (
    bpdsItemsNeedUpdate
    );
  TBreakPointsDlgStates = set of TBreakPointsDlgState;

  TBreakPointsDlg = class(TDebuggerDlg)
    lvBreakPoints: TListView;
    N0: TMenuItem;
    popShow: TMenuItem;
    mnuPopup: TPopupMenu;
    popAdd: TMenuItem;
    popAddSourceBP: TMenuItem;
    N1: TMenuItem; //--------------
    popProperties: TMenuItem;
    popEnabled: TMenuItem;
    popDelete: TMenuItem;
    N2: TMenuItem; //--------------
    popDisableAll: TMenuItem;
    popEnableAll: TMenuItem;
    popDeleteAll: TMenuItem;
    N3: TMenuItem; //--------------
    popDisableAllSameSource: TMenuItem;
    popEnableAllSameSource: TMenuItem;
    popDeleteAllSameSource: TMenuItem;
    procedure BreakpointsDlgCREATE(Sender: TObject);
    procedure lvBreakPointsClick(Sender: TObject);
    procedure lvBreakPointsDBLCLICK(Sender: TObject);
    procedure lvBreakPointsSelectItem(Sender: TObject; AItem: TListItem;
      Selected: Boolean);
    procedure mnuPopupPopup(Sender: TObject);
    procedure popAddSourceBPClick(Sender: TObject);
    procedure popDeleteAllSameSourceCLICK(Sender: TObject);
    procedure popDisableAllSameSourceCLICK(Sender: TObject);
    procedure popEnableAllSameSourceCLICK(Sender: TObject);
    procedure popPropertiesClick(Sender: TObject);
    procedure popEnabledClick(Sender: TObject);
    procedure popDeleteClick(Sender: TObject);
    procedure popDisableAllClick(Sender: TObject);
    procedure popEnableAllClick(Sender: TObject);
    procedure popDeleteAllClick(Sender: TObject);
    procedure popShowClick(Sender: TObject);
  private
    FBaseDirectory: string;
    FBreakPoints: TIDEBreakPoints;
    FBreakpointsNotification: TIDEBreakPointsNotification;
    FStates: TBreakPointsDlgStates;
    procedure BreakPointAdd(const ASender: TIDEBreakPoints;
                            const ABreakpoint: TIDEBreakPoint);
    procedure BreakPointUpdate(const ASender: TIDEBreakPoints;
                               const ABreakpoint: TIDEBreakPoint);
    procedure BreakPointRemove(const ASender: TIDEBreakPoints;
                               const ABreakpoint: TIDEBreakPoint);
    procedure SetBaseDirectory(const AValue: string);

    procedure SetBreakPoints(const AValue: TIDEBreakPoints);

    procedure UpdateItem(const AnItem: TListItem;
                         const ABreakpoint: TIDEBreakPoint);
    procedure UpdateAll;
  protected
    procedure DoEndUpdate; override;
    procedure JumpToCurrentBreakPoint; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property BaseDirectory: string read FBaseDirectory write SetBaseDirectory;
    property BreakPoints: TIDEBreakPoints read FBreakPoints write SetBreakPoints;
  end;
  
function GetBreakPointStateDescription(ABreakpoint: TBaseBreakpoint): string;
function GetBreakPointActionsDescription(ABreakpoint: TBaseBreakpoint): string;


implementation

function GetBreakPointStateDescription(ABreakpoint: TBaseBreakpoint): string;
const
  //                 enabled  valid
  DEBUG_STATE: array[Boolean, TValidState] of ShortString = (
                {vsUnknown,     vsValid,   vsInvalid}
    {Disabled} ('? (Off)','Disabled','Invalid (Off)'),
    {Endabled} ('? (On)', 'Enabled', 'Invalid (On)'));
begin
  Result:=DEBUG_STATE[ABreakpoint.Enabled,ABreakpoint.Valid];
end;

function GetBreakPointActionsDescription(ABreakpoint: TBaseBreakpoint): string;
const
  DEBUG_ACTION: array[TIDEBreakPointAction] of ShortString =
    ('Break', 'Enable Group', 'Disable Group');

var
  CurBreakPoint: TIDEBreakPoint;
  Action: TIDEBreakPointAction;
begin
  Result := '';
  if ABreakpoint is TIDEBreakPoint then begin
    CurBreakPoint:=TIDEBreakPoint(ABreakpoint);
    for Action := Low(TIDEBreakPointAction) to High(TIDEBreakPointAction) do
      if Action in CurBreakpoint.Actions
      then begin
        if Result <> '' then Result := Result + ', ';
        Result := Result + DEBUG_ACTION[Action]
      end;
  end;
end;

procedure TBreakPointsDlg.BreakPointAdd(const ASender: TIDEBreakPoints;
  const ABreakpoint: TIDEBreakPoint);
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

procedure TBreakPointsDlg.BreakPointUpdate(const ASender: TIDEBreakPoints;
  const ABreakpoint: TIDEBreakPoint);
var
  Item: TListItem;
begin
  if ABreakpoint = nil then Exit;

  Item := lvBreakPoints.Items.FindData(ABreakpoint);
  if Item = nil
  then BreakPointAdd(ASender, ABreakPoint)
  else begin
    if UpdateCount>0 then begin
      Include(FStates,bpdsItemsNeedUpdate);
      exit;
    end;
    UpdateItem(Item, ABreakPoint);
  end;
end;

procedure TBreakPointsDlg.BreakPointRemove(const ASender: TIDEBreakPoints;
  const ABreakpoint: TIDEBreakPoint);
begin
  lvBreakPoints.Items.FindData(ABreakpoint).Free;
end;

procedure TBreakPointsDlg.SetBaseDirectory(const AValue: string);
begin
  if FBaseDirectory=AValue then exit;
  FBaseDirectory:=AValue;
  UpdateAll;
end;

procedure TBreakPointsDlg.SetBreakPoints(const AValue: TIDEBreakPoints);
var
  i: Integer;
begin
  if FBreakPoints = AValue then Exit;

  BeginUpdate;
  try
    lvBreakPoints.Items.Clear;
    if FBreakPoints <> nil
    then begin
      FBreakPoints.RemoveNotification(FBreakpointsNotification);
    end;

    FBreakPoints:=AValue;

    if FBreakPoints <> nil
    then begin
      FBreakPoints.AddNotification(FBreakpointsNotification);

      for i:=0 to FBreakPoints.Count-1 do
        BreakPointUpdate(FBreakPoints, FBreakPoints.Items[i]);
    end;
  finally
    EndUpdate;
  end;
end;

constructor TBreakPointsDlg.Create(AOwner: TComponent);
begin
  inherited;
  Name:='BreakPointsDlg';
  FBreakpointsNotification := TIDEBreakPointsNotification.Create;
  FBreakpointsNotification.AddReference;
  FBreakpointsNotification.OnAdd := @BreakPointAdd;
  FBreakpointsNotification.OnUpdate := @BreakPointUpdate;
  FBreakpointsNotification.OnRemove := @BreakPointRemove;
end;       

destructor TBreakPointsDlg.Destroy;
begin
  SetBreakPoints(nil);
  FBreakpointsNotification.OnAdd := nil;
  FBreakpointsNotification.OnUpdate := nil;
  FBreakpointsNotification.OnRemove := nil;
  FBreakpointsNotification.ReleaseReference;
  inherited;
end;

procedure TBreakPointsDlg.JumpToCurrentBreakPoint;
var
  CurItem: TListItem;
  CurBreakPoint: TIDEBreakPoint;
begin
  CurItem:=lvBreakPoints.Selected;
  if CurItem=nil then exit;
  CurBreakPoint:=TIDEBreakPoint(CurItem.Data);
  DoJumpToCodePos(CurBreakPoint.Source,CurBreakPoint.Line,0);
end;

procedure TBreakPointsDlg.lvBreakPointsClick(Sender: TObject);
begin
end;

procedure TBreakPointsDlg.BreakpointsDlgCREATE(Sender: TObject);
begin
  lvBreakPoints.Align:=alClient;
end;

procedure TBreakPointsDlg.lvBreakPointsDBLCLICK(Sender: TObject);
begin
  JumpToCurrentBreakPoint;
end;

procedure TBreakPointsDlg.lvBreakPointsSelectItem(Sender: TObject;
  AItem: TListItem; Selected: Boolean);
begin
end;

procedure TBreakPointsDlg.mnuPopupPopup(Sender: TObject);
var
  Enable: Boolean;
  CurBreakPoint: TIDEBreakPoint;
begin
  Enable := lvBreakPoints.Selected <> nil;
  if Enable then
    CurBreakPoint:=TIDEBreakPoint(lvBreakPoints.Selected.Data)
  else
    CurBreakPoint:=nil;
  popProperties.Enabled := Enable;
  popEnabled.Enabled := Enable;
  if CurBreakPoint<>nil then
    popEnabled.Checked := CurBreakPoint.Enabled
  else
    popEnabled.Checked := false;
  popDelete.Enabled := Enable;
  
  // 'All in same source' menuitems
  popDisableAllSameSource.Enabled := Enable;
  popDeleteAllSameSource.Enabled := Enable;
  popEnableAllSameSource.Enabled := Enable;

  // 'All' menuitems
  Enable := lvBreakPoints.Items.Count>0;
  popDisableAll.Enabled := Enable;
  popDeleteAll.Enabled := Enable;
  popEnableAll.Enabled := Enable;
end;

procedure TBreakPointsDlg.popAddSourceBPClick(Sender: TObject);
begin
end;

procedure TBreakPointsDlg.popDeleteAllSameSourceCLICK(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
  CurItem: TListItem;
  CurBreakPoint: TIDEBreakPoint;
  Filename: String;
begin
  CurItem:=lvBreakPoints.Selected;
  if (CurItem=nil) then exit;
  Filename:=TIDEBreakpoint(CurItem.Data).Source;
  if MessageDlg('Delete all breakpoints?',
    'Delete all breakpoints in file "'+Filename+'"?',
    mtConfirmation,[mbYes,mbCancel],0)<>mrYes
  then exit;
  for n := lvBreakPoints.Items.Count - 1 downto 0 do
  begin
    Item := lvBreakPoints.Items[n];
    CurBreakPoint:=TIDEBreakPoint(Item.Data);
    if CompareFilenames(CurBreakPoint.Source,Filename)=0
    then CurBreakPoint.Free;
  end;
end;

procedure TBreakPointsDlg.popDisableAllSameSourceCLICK(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
  CurItem: TListItem;
  CurBreakPoint: TIDEBreakPoint;
  Filename: String;
begin
  CurItem:=lvBreakPoints.Selected;
  if (CurItem=nil) then exit;
  Filename:=TIDEBreakpoint(CurItem.Data).Source;
  for n := 0 to lvBreakPoints.Items.Count - 1 do
  begin
    Item := lvBreakPoints.Items[n];
    CurBreakPoint:=TIDEBreakPoint(Item.Data);
    if CompareFilenames(CurBreakPoint.Source,Filename)=0
    then CurBreakPoint.Enabled := False;
  end;
end;

procedure TBreakPointsDlg.popEnableAllSameSourceCLICK(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
  CurItem: TListItem;
  CurBreakPoint: TIDEBreakPoint;
  Filename: String;
begin
  CurItem:=lvBreakPoints.Selected;
  if (CurItem=nil) then exit;
  Filename:=TIDEBreakpoint(CurItem.Data).Source;
  for n := 0 to lvBreakPoints.Items.Count - 1 do
  begin
    Item := lvBreakPoints.Items[n];
    CurBreakPoint:=TIDEBreakPoint(Item.Data);
    if CompareFilenames(CurBreakPoint.Source,Filename)=0
    then CurBreakPoint.Enabled := True;
  end;
end;

procedure TBreakPointsDlg.popDeleteAllClick(Sender: TObject);
var
  n: Integer;
begin                                    
  if MessageDlg('Delete all breakpoints?',
    'Delete all breakpoints?',
    mtConfirmation,[mbYes,mbCancel],0)<>mrYes
  then exit;
  for n := lvBreakPoints.Items.Count - 1 downto 0 do
    TIDEBreakPoint(lvBreakPoints.Items[n].Data).Free;
end;

procedure TBreakPointsDlg.popShowClick(Sender: TObject);
begin
  JumpToCurrentBreakPoint;
end;

procedure TBreakPointsDlg.popDeleteClick(Sender: TObject);
var
  CurItem: TListItem;
  CurBreakPoint: TIDEBreakPoint;
begin
  CurItem:=lvBreakPoints.Selected;
  if CurItem=nil then exit;
  CurBreakPoint:=TIDEBreakPoint(CurItem.Data);
  if MessageDlg('Delete breakpoint?',
    'Delete breakpoint at'#13
    +'"'+CurBreakPoint.Source+'" line '+IntToStr(CurBreakPoint.Line)+'?',
    mtConfirmation,[mbYes,mbCancel],0)<>mrYes
  then exit;
  CurBreakPoint.Free;
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
    then TIDEBreakPoint(Item.Data).Enabled := False;
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
    then TIDEBreakPoint(Item.Data).Enabled := True;
  end;
end;

procedure TBreakPointsDlg.popEnabledClick(Sender: TObject);
var
  CurItem: TListItem;
begin
  CurItem:=lvBreakPoints.Selected;
  if (CurItem=nil) then exit;
  TIDEBreakPoint(CurItem.Data).Enabled:=not TIDEBreakPoint(CurItem.Data).Enabled;
end;

procedure TBreakPointsDlg.popPropertiesClick(Sender: TObject);
begin     
end;

procedure TBreakPointsDlg.DoEndUpdate;
begin
  inherited DoEndUpdate;
  if bpdsItemsNeedUpdate in FStates then UpdateAll;
end;

procedure TBreakPointsDlg.UpdateItem(const AnItem: TListItem;
  const ABreakpoint: TIDEBreakPoint);
var
  Filename: String;
begin
  // Filename/Address
  // Line/Length
  // Condition
  // Action
  // Pass Count
  // Group

  // state
  AnItem.Caption := GetBreakPointStateDescription(ABreakpoint);
  
  // filename
  Filename:=ABreakpoint.Source;
  if BaseDirectory<>'' then
    Filename:=CreateRelativePath(Filename,BaseDirectory);
  AnItem.SubItems[0] := Filename;
  
  // line
  if ABreakpoint.Line > 0
  then AnItem.SubItems[1] := IntToStr(ABreakpoint.SourceLine)
  else AnItem.SubItems[1] := '';
  
  // expression
  AnItem.SubItems[2] := ABreakpoint.Expression;
  
  // actions
  AnItem.SubItems[3]  := GetBreakPointActionsDescription(ABreakpoint);
  
  // hitcount
  AnItem.SubItems[4] := IntToStr(ABreakpoint.HitCount);
  
  // group
  if ABreakpoint.Group = nil
  then AnItem.SubItems[5] := ''
  else AnItem.SubItems[5] := ABreakpoint.Group.Name;
end;

procedure TBreakPointsDlg.UpdateAll;
var
  i: Integer;
  CurItem: TListItem;
begin
  if UpdateCount>0 then begin
    Include(FStates,bpdsItemsNeedUpdate);
    exit;
  end;
  Exclude(FStates,bpdsItemsNeedUpdate);
  for i:=0 to lvBreakPoints.Items.Count-1 do begin
    CurItem:=lvBreakPoints.Items[i];
    UpdateItem(CurItem,TIDEBreakPoint(CurItem.Data));
  end;
end;


initialization
  {$I breakpointsdlg.lrs}

end.

{ =============================================================================
  $Log$
  Revision 1.23  2005/02/18 16:59:39  mattias
  fixed note about jpeg bug

  Revision 1.22  2004/08/26 23:50:05  marc
  * Restructured debugger view classes
  * Fixed help

  Revision 1.21  2004/05/02 12:01:15  mattias
  removed unneeded units in uses sections

  Revision 1.20  2003/06/04 13:34:58  mattias
  implemented breakpoints hints for source editor

  Revision 1.19  2003/06/03 11:20:12  mattias
  implemented enable/disable/delete breakpoints in same source

  Revision 1.18  2003/06/03 08:02:33  mattias
  implemented showing source lines in breakpoints dialog

  Revision 1.17  2003/06/03 01:35:39  marc
  MWE: = Splitted TDBGBreakpoint into TBaseBreakPoint, TIDEBreakpoint and
         TDBGBreakPoint

  Revision 1.16  2003/05/29 23:14:17  mattias
  implemented jump to code on double click for breakpoints and callstack dlg

  Revision 1.15  2003/05/28 15:56:19  mattias
  implemented sourcemarks

  Revision 1.14  2003/05/28 08:46:24  mattias
  break;points dialog now gets the items without debugger

  Revision 1.13  2003/05/27 20:58:12  mattias
  implemented enable and deleting breakpoint in breakpoint dlg

  Revision 1.12  2003/05/27 15:04:00  mattias
  small fixes for debugger without file

  Revision 1.11  2003/05/23 14:12:51  mattias
  implemented restoring breakpoints

  Revision 1.10  2003/05/22 23:08:19  marc
  MWE: = Moved and renamed debuggerforms so that they can be
         modified by the ide
       + Added some parsing to evaluate complex expressions
         not understood by the debugger

  Revision 1.9  2003/05/18 10:42:57  mattias
  implemented deleting empty submenus

  Revision 1.8  2003/02/28 19:10:25  mattias
  added new ... dialog

  Revision 1.7  2002/05/30 22:45:57  lazarus
  MWE:
    - Removed menucreation from loaded since streaming works

  Revision 1.6  2002/05/10 06:57:47  lazarus
  MG: updated licenses

  Revision 1.5  2002/04/24 20:42:29  lazarus
  MWE:
    + Added watches
    * Updated watches and watchproperty dialog to load as resource
    = renamed debugger resource files from *.lrc to *.lrs
    * Temporary fixed language problems on GDB (bug #508)
    * Made Debugmanager dialog handling more generic

  Revision 1.4  2002/03/27 00:31:02  lazarus
  MWE:
    * activated selection dependent popup

  Revision 1.3  2002/03/25 22:38:29  lazarus
  MWE:
    + Added invalidBreakpoint image
    * Reorganized uniteditor so that breakpoints can be added erternal
    * moved breakpoints events to notification object

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
