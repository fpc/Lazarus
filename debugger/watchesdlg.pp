{ $Id$ }
{               ----------------------------------------------
                 watchesdlg.pp  -  Overview of watches
                ----------------------------------------------

 @created(Fri Dec 14st WET 2001)
 @lastmod($Date$)
 @author(Shane Miller)
 @author(Marc Weustink <marc@@dommelstein.net>)

 This unit contains the watches dialog.


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

unit WatchesDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources, StdCtrls,
  Buttons, Menus, ComCtrls, Debugger, DebuggerDlg, BaseDebugManager, LCLtype;

type

{ TWatchesDlg }

TWatchesDlg = class(TDebuggerDlg)
    lvWatches: TListView;
    mnuPopup: TPopupMenu;
    popAdd: TMenuItem;
    N1: TMenuItem; //--------------
    popProperties: TMenuItem;
    popEnabled: TMenuItem;
    popDelete: TMenuItem;
    N2: TMenuItem; //--------------
    popDisableAll: TMenuItem;
    popEnableAll: TMenuItem;
    popDeleteAll: TMenuItem;
    procedure lvWatchesDblClick(Sender: TObject);
    procedure lvWatchesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvWatchesMouseDown(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lvWatchesSelectItem(Sender: TObject; AItem: TListItem; Selected: Boolean);
    procedure popAddClick(Sender: TObject);
    procedure popPropertiesClick(Sender: TObject);
    procedure popEnabledClick(Sender: TObject);
    procedure popDeleteClick(Sender: TObject);
    procedure popDisableAllClick(Sender: TObject);
    procedure popEnableAllClick(Sender: TObject);
    procedure popDeleteAllClick(Sender: TObject);
  private
    FWatches: TIDEWatches;
    FWatchesNotification: TIDEWatchesNotification;
    function GetSelected: TIDEWatch;
    procedure SetWatches(const AValue: TIDEWatches);
    procedure WatchAdd(const ASender: TIDEWatches; const AWatch: TIDEWatch);
    procedure WatchUpdate(const ASender: TIDEWatches; const AWatch: TIDEWatch);
    procedure WatchRemove(const ASender: TIDEWatches; const AWatch: TIDEWatch);

    procedure UpdateItem(const AItem: TListItem; const AWatch: TIDEWatch);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Watches: TIDEWatches read FWatches write SetWatches;
  end;


implementation


{ TWatchesDlg }

constructor TWatchesDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  Name:='WatchesDlg';
  FWatchesNotification := TIDEWatchesNotification.Create;
  FWatchesNotification.AddReference;
  FWatchesNotification.OnAdd := @WatchAdd;
  FWatchesNotification.OnUpdate := @WatchUpdate;
  FWatchesNotification.OnRemove := @WatchRemove;
  
{$IFDEF WIN32}
  {$NOTE TODO repair TListView column widths and remove this hack}
  lvWatches.Column[0].Width := 100;
  lvWatches.Column[1].Width := 200;
{$ENDIF WIN32}
end;

destructor TWatchesDlg.Destroy;
begin
  SetWatches(nil);
  FWatchesNotification.OnAdd := nil;
  FWatchesNotification.OnUpdate := nil;
  FWatchesNotification.OnRemove := nil;
  FWatchesNotification.ReleaseReference;
  inherited;
end;
          
function TWatchesDlg.GetSelected: TIDEWatch;
var
  Item: TListItem;
begin
  Item := lvWatches.Selected;
  if Item = nil
  then Result := nil
  else Result := TIDEWatch(Item.Data);
end;

procedure TWatchesDlg.SetWatches(const AValue: TIDEWatches);
var
  i: Integer;
begin
  if FWatches = AValue then Exit;

  BeginUpdate;
  try
    lvWatches.Items.Clear;

    if FWatches <> nil
    then begin
      FWatches.RemoveNotification(FWatchesNotification);
    end;

    FWatches:=AValue;

    if FWatches <> nil
    then begin
      FWatches.AddNotification(FWatchesNotification);

      for i:=0 to FWatches.Count-1 do
        WatchUpdate(FWatches, FWatches.Items[i]);
    end;
    
  finally
    EndUpdate;
  end;
end;

procedure TWatchesDlg.lvWatchesSelectItem(Sender: TObject; AItem: TListItem; Selected: Boolean);
var
  Enable: Boolean;
  Watch: TIDEWatch;
begin
  Watch := GetSelected;
  Enable := Watch <> nil;
  popProperties.Enabled := Enable;
  popEnabled.Enabled := Enable;
  popDelete.Enabled := Enable;   
  popEnabled.Checked := Enable and Watch.Enabled;
end;

procedure TWatchesDlg.lvWatchesMouseDown(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  {$IFDEF WIN32}
  {$NOTE TODO repair TListView.PopupMenu and remove this hack}
  if Button in [mbRight] then mnuPopup.PopUp(X + lvWatches.Left + Left, Y + lvWatches.Top + Top);
  {$ENDIF}
end;

procedure TWatchesDlg.lvWatchesDblClick(Sender: TObject);
begin
  if lvWatches.SelCount >= 0 then
    popPropertiesClick(Sender)
  else
    popAddClick(Sender);
end;

procedure TWatchesDlg.lvWatchesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    //delete key pressed: delete selected item
    VK_DELETE: popDeleteClick(Sender);

    //insert key pressed: add new item
    VK_INSERT: popAddClick(Sender);
  end;
end;

procedure TWatchesDlg.popAddClick(Sender: TObject);
begin
  DebugBoss.ShowWatchProperties(nil);
end;

procedure TWatchesDlg.popDeleteAllClick(Sender: TObject);
var
  n: Integer;
begin                                    
  for n := lvWatches.Items.Count - 1 downto 0 do
    TIDEWatch(lvWatches.Items[n].Data).Free;
end;

procedure TWatchesDlg.popDeleteClick(Sender: TObject);
begin
  GetSelected.Free;
end;

procedure TWatchesDlg.popDisableAllClick(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
begin
  for n := 0 to lvWatches.Items.Count - 1 do
  begin
    Item := lvWatches.Items[n];
    if Item.Data <> nil
    then TIDEWatch(Item.Data).Enabled := False;
  end;
end;

procedure TWatchesDlg.popEnableAllClick(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
begin
  for n := 0 to lvWatches.Items.Count - 1 do
  begin
    Item := lvWatches.Items[n];
    if Item.Data <> nil
    then TIDEWatch(Item.Data).Enabled := True;
  end;
end;

procedure TWatchesDlg.popEnabledClick(Sender: TObject);
var
  Watch: TIDEWatch;
begin
  Watch := GetSelected;
  if Watch = nil then Exit;
  popEnabled.Checked := not popEnabled.Checked;
  Watch.Enabled := popEnabled.Checked;
end;

procedure TWatchesDlg.popPropertiesClick(Sender: TObject);
begin
  DebugBoss.ShowWatchProperties(GetSelected);
end;

procedure TWatchesDlg.UpdateItem(const AItem: TListItem; const AWatch: TIDEWatch);
begin
// Expression
// Result
  AItem.Caption := AWatch.Expression;
  AItem.SubItems[0] := AWatch.Value;
end;

procedure TWatchesDlg.WatchAdd(const ASender: TIDEWatches; const AWatch: TIDEWatch);
var
  Item: TListItem;
begin
  Item := lvWatches.Items.FindData(AWatch);
  if Item = nil
  then begin
    Item := lvWatches.Items.Add;
    Item.Data := AWatch;
    Item.SubItems.Add('');
  end;

  UpdateItem(Item, AWatch);
end;

procedure TWatchesDlg.WatchUpdate(const ASender: TIDEWatches; const AWatch: TIDEWatch);
var
  Item: TListItem;
begin
  if AWatch = nil then Exit;

  Item := lvWatches.Items.FindData(AWatch);
  if Item = nil
  then WatchAdd(ASender, AWatch)
  else UpdateItem(Item, AWatch);
end;

procedure TWatchesDlg.WatchRemove(const ASender: TIDEWatches; const AWatch: TIDEWatch);
begin
  lvWatches.Items.FindData(AWatch).Free;
end;

initialization
  {$I watchesdlg.lrs}

end.

{
  $Log$
  Revision 1.11  2005/07/17 01:05:04  mattias
  started basic docking anchoring and made Contributors.txt ASCII to show everywhere

  Revision 1.10  2005/07/15 17:59:47  mattias
  improved watches: popup on rightclick, listview widths manually override under win32, double click shows adds/change  from Darius Blaszijk

  Revision 1.9  2004/11/23 00:54:55  marc
  + Added Evaluate/Modify dialog

  Revision 1.8  2004/08/26 23:50:05  marc
  * Restructured debugger view classes
  * Fixed help

  Revision 1.7  2004/05/02 12:01:15  mattias
  removed unneeded units in uses sections

  Revision 1.6  2003/05/28 09:00:35  mattias
  watches dialog now without DoInitDebugger

  Revision 1.5  2003/05/18 10:42:58  mattias
  implemented deleting empty submenus

  Revision 1.4  2002/05/30 22:45:57  lazarus
  MWE:
    - Removed menucreation from loaded since streaming works

  Revision 1.3  2002/05/30 21:53:56  lazarus
  MG: fixed form streaming of not direct TForm descendents

  Revision 1.2  2002/05/10 06:57:48  lazarus
  MG: updated licenses

  Revision 1.1  2002/04/24 20:42:29  lazarus
  MWE:
    + Added watches
    * Updated watches and watchproperty dialog to load as resource
    = renamed debugger resource files from *.lrc to *.lrs
    * Temporary fixed language problems on GDB (bug #508)
    * Made Debugmanager dialog handling more generic


}
