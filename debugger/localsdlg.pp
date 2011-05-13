{ $Id$ }
{               ----------------------------------------------  
                 localsdlg.pp  -  Overview of local variables 
                ---------------------------------------------- 
 
 @created(Thu Mar 14st WET 2002)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

 This unit contains the Locals debugger dialog.
 
 
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
unit LocalsDlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Debugger, DebuggerDlg;

type

  { TLocalsDlg }

  TLocalsDlg = class(TDebuggerDlg)
    lvLocals: TListView;
  private
    FCallStackMonitor: TCallStackMonitor;
    FLocalsMonitor: TLocalsMonitor;
    FLocalsNotification: TLocalsNotification;
    FThreadsMonitor: TThreadsMonitor;
    FThreadsNotification: TThreadsNotification;
    FCallstackNotification: TCallStackNotification;
    procedure ContextChanged(Sender: TObject);
    procedure LocalsChanged(Sender: TObject);
    procedure SetCallStackMonitor(const AValue: TCallStackMonitor);
    procedure SetLocals(const AValue: TLocalsMonitor);
    procedure SetThreadsMonitor(const AValue: TThreadsMonitor);
    function  GetThreadId: Integer;
    function GetStackframe: Integer;
  protected
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property LocalsMonitor: TLocalsMonitor read FLocalsMonitor write SetLocals;
    property ThreadsMonitor: TThreadsMonitor read FThreadsMonitor write SetThreadsMonitor;
    property CallStackMonitor: TCallStackMonitor read FCallStackMonitor write SetCallStackMonitor;
  end;


implementation

{$R *.lfm}

uses
  LazarusIDEStrConsts;
  
{ TLocalsDlg }

constructor TLocalsDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLocalsNotification := TLocalsNotification.Create;
  FLocalsNotification.AddReference;
  FLocalsNotification.OnChange := @LocalsChanged;

  FThreadsNotification := TThreadsNotification.Create;
  FThreadsNotification.AddReference;
  FThreadsNotification.OnCurrent := @ContextChanged;

  FCallstackNotification := TCallStackNotification.Create;
  FCallstackNotification.AddReference;
  FCallstackNotification.OnCurrent   := @ContextChanged;

  Caption:= lisLocals;
  lvLocals.Columns[0].Caption:= lisLocalsDlgName;
  lvLocals.Columns[1].Caption:= lisLocalsDlgValue;
end;

destructor TLocalsDlg.Destroy;
begin
  SetLocals(nil);
  FLocalsNotification.OnChange := nil;
  FLocalsNotification.ReleaseReference;
  FThreadsNotification.OnCurrent := nil;
  FThreadsNotification.ReleaseReference;
  FCallstackNotification.OnCurrent := nil;
  FCallstackNotification.ReleaseReference;
  inherited Destroy;
end;

procedure TLocalsDlg.ContextChanged(Sender: TObject);
begin
  LocalsChanged(nil);
end;

procedure TLocalsDlg.LocalsChanged(Sender: TObject);
var
  n, idx: Integer;                               
  List: TStringList;
  Item: TListItem;
  S: String;
  Locals: TLocals;
begin                                        
  if (FThreadsMonitor = nil) or (FCallStackMonitor = nil) then begin
    lvLocals.Items.Clear;
    exit;
  end;
  if GetStackframe < 0 then begin // TODO need dedicated validity property
    lvLocals.Items.Clear;
    exit;
  end;

  List := TStringList.Create;
  try
    BeginUpdate;
    try
      if FLocalsMonitor <> nil
      then Locals := LocalsMonitor.CurrentLocalsList[GetThreadId, GetStackframe]
      else Locals := nil;
      if Locals = nil
      then begin
        lvLocals.Items.Clear;
        Exit;
      end;

      //Get existing items
      for n := 0 to lvLocals.Items.Count - 1 do
      begin
        Item := lvLocals.Items[n];
        S := Item.Caption;
        S := UpperCase(S);
        List.AddObject(S, Item);
      end;

      // add/update entries
      for n := 0 to Locals.Count - 1 do
      begin
        idx := List.IndexOf(Uppercase(Locals.Names[n]));
        if idx = -1
        then begin
          // New entry
          Item := lvLocals.Items.Add;
          Item.Caption := Locals.Names[n];
          Item.SubItems.Add(Locals.Values[n]);
        end
        else begin
          // Existing entry
          Item := TListItem(List.Objects[idx]);
          Item.SubItems[0] := Locals.Values[n];
          List.Delete(idx);
        end;
      end;

      // remove obsolete entries
      for n := 0 to List.Count - 1 do
        lvLocals.Items.Delete(TListItem(List.Objects[n]).Index);

    finally
      EndUpdate;
    end;
  finally
    List.Free;
  end;
end;

procedure TLocalsDlg.SetCallStackMonitor(const AValue: TCallStackMonitor);
begin
  if FCallStackMonitor = AValue then exit;
  BeginUpdate;
  try
    if FCallStackMonitor <> nil
    then FCallStackMonitor.RemoveNotification(FCallstackNotification);

    FCallStackMonitor := AValue;

    if FCallStackMonitor <> nil
    then FCallStackMonitor.AddNotification(FCallstackNotification);

    LocalsChanged(nil);
  finally
    EndUpdate;
  end;
end;

procedure TLocalsDlg.SetLocals(const AValue: TLocalsMonitor);
begin
  if FLocalsMonitor = AValue then Exit;

  BeginUpdate;
  try
    if FLocalsMonitor <> nil
    then begin
      FLocalsMonitor.RemoveNotification(FLocalsNotification);
    end;

    FLocalsMonitor := AValue;

    if FLocalsMonitor <> nil
    then begin
      FLocalsMonitor.AddNotification(FLocalsNotification);
    end;
    
    LocalsChanged(FLocalsMonitor);
  finally
    EndUpdate;
  end;
end;

procedure TLocalsDlg.SetThreadsMonitor(const AValue: TThreadsMonitor);
begin
  if FThreadsMonitor = AValue then exit;
  BeginUpdate;
  try
    if FThreadsMonitor <> nil
    then FThreadsMonitor.RemoveNotification(FThreadsNotification);

    FThreadsMonitor := AValue;

    if FThreadsMonitor <> nil
    then FThreadsMonitor.AddNotification(FThreadsNotification);

    LocalsChanged(nil);
  finally
    EndUpdate;
  end;
end;

function TLocalsDlg.GetThreadId: Integer;
begin
  Result := -1;
  if (FThreadsMonitor = nil) then exit;
  Result := FThreadsMonitor.CurrentThreads.CurrentThreadId;
end;

function TLocalsDlg.GetStackframe: Integer;
begin
  Result := -1;
  if (FCallStackMonitor = nil) then exit;
  Result := FCallStackMonitor.CurrentCallStackList.EntriesForThreads[GetThreadId].CurrentIndex;
end;

procedure TLocalsDlg.DoBeginUpdate;
begin
  lvLocals.BeginUpdate;
end;

procedure TLocalsDlg.DoEndUpdate;
begin
  lvLocals.EndUpdate;
end;

end.

