{ $Id$ }
{                       ------------------------------------------
                        debugeventsform.pp  -  Shows target output
                        ------------------------------------------

 @created(Wed Mar 1st 2010)
 @lastmod($Date$)
 @author Lazarus Project

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
unit DebugEventsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, ComCtrls,
  Debugger, DebuggerDlg, LazarusIDEStrConsts, EnvironmentOpts;

type
  TDBGEventCategories = set of TDBGEventCategory;

  { TDbgEventsForm }

  TDbgEventsForm = class(TDebuggerDlg)
    ckgFilter: TCheckGroup;
    imlMain: TImageList;
    lstFilteredEvents: TListView;
    procedure ckgFilterItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FEvents: TStringList;
    FFilter: TDBGEventCategories;
    procedure UpdateFilteredList;
  public
    procedure SetEvents(const AEvents: TStrings; const AFilter: TDBGEventCategories);
    procedure GetEvents(const AResultEvents: TStrings; var AResultFilter: TDBGEventCategories);
    procedure Clear;
    procedure AddEvent(const ACategory: TDBGEventCategory; const AText: String);
  end; 

implementation

{$R *.lfm}

{ TDbgEventsForm }

procedure TDbgEventsForm.FormCreate(Sender: TObject);
var
  i: TDBGEventCategory;
begin
  Caption := lisMenuViewDebugEvents;
  FEvents := TStringList.Create;
  ckgFilter.Items.Clear;
  for i := Low(TDBGEventCategory) to High(TDBGEventCategory) do
    case i of
      ecBreakpoint: ckgFilter.Items.Add(lisDebugOptionsFrmBreakpoint);
      ecProcess: ckgFilter.Items.Add(lisDebugOptionsFrmProcess);
      ecThread: ckgFilter.Items.Add(lisDebugOptionsFrmThread);
      ecModule: ckgFilter.Items.Add(lisDebugOptionsFrmModule);
      ecOutput: ckgFilter.Items.Add(lisDebugOptionsFrmOutput);
      ecWindow: ckgFilter.Items.Add(lisDebugOptionsFrmWindow);
      ecDebugger: ckgFilter.Items.Add(lisDebugOptionsFrmDebugger);
    end;
end;

procedure TDbgEventsForm.ckgFilterItemClick(Sender: TObject; Index: integer);
begin
  if ckgFilter.Checked[Index] then
    Include(FFilter, TDBGEventCategory(Index))
  else
    Exclude(FFilter, TDBGEventCategory(Index));
  UpdateFilteredList;
end;
procedure TDbgEventsForm.FormDestroy(Sender: TObject);
begin
  FEvents.Free;
end;

procedure TDbgEventsForm.UpdateFilteredList;
var
  i: Integer;
  Item: TListItem;
  CategoryImages: array [TDBGEventCategory] of Integer;
begin
  CategoryImages[ecBreakpoint] := 0;
  CategoryImages[ecProcess] := 1;
  CategoryImages[ecThread] := 2;
  CategoryImages[ecModule] := 3;
  CategoryImages[ecOutput] := 4;
  CategoryImages[ecWindow] := 5;
  CategoryImages[ecDebugger] := 6;

  lstFilteredEvents.BeginUpdate;
  try
    lstFilteredEvents.Clear;
    for i := 0 to FEvents.Count -1 do
      if TDBGEventCategory(FEvents.Objects[i]) in FFilter then
    begin
      Item := lstFilteredEvents.Items.Add;
      Item.Caption := FEvents[i];
      Item.ImageIndex := CategoryImages[TDBGEventCategory(FEvents.Objects[i])];
    end;
  finally
    lstFilteredEvents.EndUpdate;
  end;
  // To be a smarter and restore the active Item, we would have to keep a link
  //between the lstFilteredEvents item and FEvents index, and account items
  //removed from FEvents because of log limit.
  // Also, TopItem and GetItemAt(0,0) both return nil in gtk2.
  if lstFilteredEvents.Items.Count <> 0 then
    lstFilteredEvents.Items[lstFilteredEvents.Items.Count -1].MakeVisible(False);
end;

procedure TDbgEventsForm.SetEvents(const AEvents: TStrings;
  const AFilter: TDBGEventCategories);
var
  i: TDBGEventCategory;
begin
  if AEvents <> nil then
    FEvents.Assign(AEvents)
  else
    FEvents.Clear;
  FFilter := AFilter;
  for i := Low(TDBGEventCategory) to High(TDBGEventCategory) do
    ckgFilter.Checked[Ord(i)] := i in FFilter;
  UpdateFilteredList;
end;

procedure TDbgEventsForm.GetEvents(const AResultEvents: TStrings;
  var AResultFilter: TDBGEventCategories);
begin
  AResultEvents.Assign(FEvents);
  AResultFilter := FFilter;
end;

procedure TDbgEventsForm.Clear;
begin
  FEvents.Clear;
  lstFilteredEvents.Clear;
end;

procedure TDbgEventsForm.AddEvent(const ACategory: TDBGEventCategory;
  const AText: String);
var
  Item: TListItem;
begin
  if EnvironmentOptions.DebuggerEventLogCheckLineLimit then
  begin
    lstFilteredEvents.BeginUpdate;
    try
      while lstFilteredEvents.Items.Count >= EnvironmentOptions.DebuggerEventLogLineLimit do
        lstFilteredEvents.Items.Delete(0);
    finally
      lstFilteredEvents.EndUpdate;
    end;
  end;
  FEvents.AddObject(AText, TObject(ACategory));
  if ACategory in FFilter then
  begin
    Item := lstFilteredEvents.Items.Add;
    Item.Caption := AText;
    Item.ImageIndex := Ord(ACategory);
    Item.MakeVisible(False);
  end;
end;

end.

