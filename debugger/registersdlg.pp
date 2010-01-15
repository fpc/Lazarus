{ $Id$ }
{               ----------------------------------------------  
                 registersdlg.pp  -  Overview of registers 
                ---------------------------------------------- 
 
 @created(Sun Nov 16th WET 2008)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

 This unit contains the registers debugger dialog.
 
 
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
unit RegistersDlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Debugger, DebuggerDlg;

type

  { TRegistersDlg }

  TRegistersDlg = class(TDebuggerDlg)
    ImageList1: TImageList;
    lvRegisters: TListView;
  private
    FRegisters: TIDERegisters;
    FRegistersNotification: TIDERegistersNotification;
    procedure RegistersChanged(Sender: TObject);
    procedure SetRegisters(const AValue: TIDERegisters);
  protected
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Registers: TIDERegisters read FRegisters write SetRegisters;
  end;


implementation

{$R *.lfm}

uses
  LazarusIDEStrConsts;
  
{ TRegistersDlg }

constructor TRegistersDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRegistersNotification := TIDERegistersNotification.Create;
  FRegistersNotification.AddReference;
  FRegistersNotification.OnChange := @RegistersChanged;
  Caption:= lisRegisters;
  lvRegisters.Columns[0].Caption:= lisRegistersDlgName;
  lvRegisters.Columns[1].Caption:= lisRegistersDlgValue;
end;

destructor TRegistersDlg.Destroy;
begin
  SetRegisters(nil);
  FRegistersNotification.OnChange := nil;
  FRegistersNotification.ReleaseReference;
  inherited Destroy;
end;

procedure TRegistersDlg.RegistersChanged(Sender: TObject);
var
  n, idx: Integer;                               
  List: TStringList;
  Item: TListItem;
  S: String;
begin                                        
  List := TStringList.Create;
  try
    BeginUpdate;
    try
      if FRegisters = nil
      then begin
        lvRegisters.Items.Clear;
        Exit;
      end;
    
      //Get existing items
      for n := 0 to lvRegisters.Items.Count - 1 do
      begin
        Item := lvRegisters.Items[n];
        S := Item.Caption;
        S := UpperCase(S);
        List.AddObject(S, Item);
      end;

      // add/update entries
      for n := 0 to FRegisters.Count - 1 do
      begin
        idx := List.IndexOf(Uppercase(FRegisters.Names[n]));
        if idx = -1
        then begin
          // New entry
          Item := lvRegisters.Items.Add;
          Item.Caption := FRegisters.Names[n];
          Item.SubItems.Add(FRegisters.Values[n]);
        end
        else begin
          // Existing entry
          Item := TListItem(List.Objects[idx]);
          Item.SubItems[0] := FRegisters.Values[n];
          List.Delete(idx);
        end;
        if FRegisters.Modified[n]
        then Item.ImageIndex := 0
        else Item.ImageIndex := -1;
      end;

      // remove obsolete entries
      for n := 0 to List.Count - 1 do
        lvRegisters.Items.Delete(TListItem(List.Objects[n]).Index);

    finally
      EndUpdate;
    end;
  finally
    List.Free;
  end;
end;

procedure TRegistersDlg.SetRegisters(const AValue: TIDERegisters);
begin
  if FRegisters = AValue then Exit;

  BeginUpdate;
  try
    if FRegisters <> nil
    then begin
      FRegisters.RemoveNotification(FRegistersNotification);
    end;

    FRegisters := AValue;

    if FRegisters <> nil
    then begin
      FRegisters.AddNotification(FRegistersNotification);
    end;
    
    RegistersChanged(FRegisters);
  finally
    EndUpdate;
  end;
end;

procedure TRegistersDlg.DoBeginUpdate;
begin
  lvRegisters.BeginUpdate;
end;

procedure TRegistersDlg.DoEndUpdate;
begin
  lvRegisters.EndUpdate;
end;

end.

