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
  Buttons, Extctrls, Menus, ComCtrls, Debugger, DebuggerDlg, WatchPropertyDlg;

type
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
    procedure lvWatchesClick(Sender: TObject);
    procedure lvWatchesSelectItem(Sender: TObject; AItem: TListItem; Selected: Boolean);
    procedure popAddClick(Sender: TObject);
    procedure popPropertiesClick(Sender: TObject);
    procedure popEnabledClick(Sender: TObject);
    procedure popDeleteClick(Sender: TObject);
    procedure popDisableAllClick(Sender: TObject);
    procedure popEnableAllClick(Sender: TObject);
    procedure popDeleteAllClick(Sender: TObject);
  private 
    FWatchesNotification: TDBGWatchesNotification;
    function GetSelected: TDBGWatch;
    procedure WatchAdd(const ASender: TDBGWatches; const AWatch: TDBGWatch);
    procedure WatchUpdate(const ASender: TDBGWatches; const AWatch: TDBGWatch);
    procedure WatchRemove(const ASender: TDBGWatches; const AWatch: TDBGWatch);

    procedure UpdateItem(const AItem: TListItem; const AWatch: TDBGWatch);
  protected
    procedure SetDebugger(const ADebugger: TDebugger); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


implementation


{ TWatchesDlg }

constructor TWatchesDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name:='WatchesDlg';
  FWatchesNotification := TDBGWatchesNotification.Create;
  FWatchesNotification.AddReference;
  FWatchesNotification.OnAdd := @WatchAdd;
  FWatchesNotification.OnUpdate := @WatchUpdate;
  FWatchesNotification.OnRemove := @WatchRemove;
end;       

destructor TWatchesDlg.Destroy;
begin
  SetDebugger(nil);
  FWatchesNotification.OnAdd := nil;
  FWatchesNotification.OnUpdate := nil;
  FWatchesNotification.OnRemove := nil;
  FWatchesNotification.ReleaseReference;
  inherited;
end;
          
function TWatchesDlg.GetSelected: TDBGWatch;
var
  Item: TListItem;
begin
  Item := lvWatches.Selected;
  if Item = nil
  then Result := nil
  else Result := TDBGWatch(Item.Data);
end;

procedure TWatchesDlg.lvWatchesClick(Sender: TObject);
begin
end;

procedure TWatchesDlg.lvWatchesSelectItem(Sender: TObject; AItem: TListItem; Selected: Boolean);
var
  Enable: Boolean;
  Watch: TDBGWatch;
begin
  Watch := GetSelected;
  Enable := Watch <> nil;
  popProperties.Enabled := Enable;
  popEnabled.Enabled := Enable;
  popDelete.Enabled := Enable;   
  popEnabled.Checked := Enable and Watch.Enabled;
end;

procedure TWatchesDlg.popAddClick(Sender: TObject);
begin
  with TWatchPropertyDlg.Create(Self, nil, Debugger) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TWatchesDlg.popDeleteAllClick(Sender: TObject);
var
  n: Integer;
begin                                    
  for n := lvWatches.Items.Count - 1 downto 0 do
    TDBGWatch(lvWatches.Items[n].Data).Free;
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
    then TDBGWatch(Item.Data).Enabled := False;
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
    then TDBGWatch(Item.Data).Enabled := True;
  end;
end;

procedure TWatchesDlg.popEnabledClick(Sender: TObject);
var
  Watch: TDBGWatch;
begin
  Watch := GetSelected;
  if Watch = nil then Exit;
  popEnabled.Checked := not popEnabled.Checked;
  Watch.Enabled := popEnabled.Checked;
end;

procedure TWatchesDlg.popPropertiesClick(Sender: TObject);
begin
  with TWatchPropertyDlg.Create(Self, GetSelected, Debugger) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TWatchesDlg.UpdateItem(const AItem: TListItem; const AWatch: TDBGWatch);
begin
// Expression
// Result
  AItem.Caption := AWatch.Expression;
  AItem.SubItems[0] := AWatch.Value;
end;

procedure TWatchesDlg.SetDebugger(const ADebugger: TDebugger);
begin
  if ADebugger <> Debugger
  then begin
    if Debugger <> nil
    then begin
      Debugger.Watches.RemoveNotification(FWatchesNotification);
    end;
    inherited;
    if Debugger <> nil
    then begin
      Debugger.Watches.AddNotification(FWatchesNotification);
    end;
  end
  else inherited;
end;

procedure TWatchesDlg.WatchAdd(const ASender: TDBGWatches; const AWatch: TDBGWatch);
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

procedure TWatchesDlg.WatchUpdate(const ASender: TDBGWatches; const AWatch: TDBGWatch);
var
  Item: TListItem;
begin
  if AWatch = nil then Exit;

  Item := lvWatches.Items.FindData(AWatch);
  if Item = nil
  then WatchAdd(ASender, AWatch)
  else UpdateItem(Item, AWatch);
end;

procedure TWatchesDlg.WatchRemove(const ASender: TDBGWatches; const AWatch: TDBGWatch);
begin
  lvWatches.Items.FindData(AWatch).Free;
end;

initialization
  {$I watchesdlg.lrs}

end.

{ =============================================================================
  $Log$
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
