{ ListViewFilterEdit

  Copyright (C) 2012 Lazarus team

  This library is free software; you can redistribute it and/or modify it
  under the same terms as the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.

}
unit ListViewFilterEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, LResources, Graphics, Controls, ComCtrls,
  LCLProc, LCLType, EditBtn, FileUtil, LazUTF8, fgl;

type

  //TImageIndexEvent = function (Str: String; Data: TObject;
  //                             var IsEnabled: Boolean): Integer of object;
  TStringArray = array of string;
  TListViewDataList = specialize TFPGList<TStringArray>;

  { TListViewFilterEdit }

  TListViewFilterEdit = class(TCustomControlFilterEdit)
  private
    // A control showing the (filtered) data.
    fFilteredListview: TCustomListView;
    // All fields / Just the first field, caption.
    fByAllFields: Boolean;
    // Store/restore the old selections here.
    fSelectionList: TStringList;
    // Data supplied by caller through Items property.
    fOriginalData: TListViewDataList;
    // Data sorted for viewing.
    fFilteredData: TListViewDataList;
    function MatchesFilter(aData: TStringArray): Boolean;
    procedure SetFilteredListview(const AValue: TCustomListView);
  protected
    procedure MoveNext; override;
    procedure MovePrev; override;
    function ReturnKeyHandled: Boolean; override;
    procedure SortAndFilter; override;
    procedure ApplyFilterCore; override;
    function GetDefaultGlyph: TBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StoreSelection; override;
    procedure RestoreSelection; override;
  public
    property SelectionList: TStringList read fSelectionList;
    property Items: TListViewDataList read fOriginalData;
  published
    property FilteredListview: TCustomListView read fFilteredListview write SetFilteredListview;
    property ByAllFields: Boolean read fByAllFields write fByAllFields;
  end;

var
  ListFilterGlyph: TBitmap;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('LazControls',[TListViewFilterEdit]);
end;

{ TListViewFilterEdit }

constructor TListViewFilterEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fOriginalData:=TListViewDataList.Create;
  fSelectionList:=TStringList.Create;
  fFilteredData:=TListViewDataList.Create;
end;

destructor TListViewFilterEdit.Destroy;
begin
  fFilteredData.Free;
  fSelectionList.Free;
  fOriginalData.Free;
  inherited Destroy;
end;

function TListViewFilterEdit.GetDefaultGlyph: TBitmap;
begin
  Result := ListFilterGlyph;
end;

function ListItem2Data(AItem: TListItem): TStringArray;
var
  i: Integer;
begin
  SetLength(Result, AItem.SubItems.Count+1);
  Result[0] := AItem.Caption;
  for i := 0 to AItem.SubItems.Count-1 do
    Result[i+1] := AItem.SubItems[i];
end;

procedure Data2ListItem(AData: TStringArray; AItem: TListItem);
var
  i: Integer;
begin
  AItem.Caption := AData[0];
  for i := 1 to Length(AData)-1 do
    AItem.SubItems.Add(AData[i]);
end;

procedure TListViewFilterEdit.SetFilteredListview(const AValue: TCustomListView);
var
  i: Integer;
begin
  if fFilteredListview = AValue then Exit;
  fFilteredListview:=AValue;
  if Assigned(fFilteredListview) then
    for i := 0 to fFilteredListview.Items.Count-1 do
      fOriginalData.Add(ListItem2Data(fFilteredListview.Items[i]));
end;

function TListViewFilterEdit.MatchesFilter(aData: TStringArray): Boolean;
var
  i, EndInd: Integer;
begin
  if Filter='' then
    Exit(True);
  if fByAllFields then
    EndInd := Pred(Length(aData))
  else
    EndInd := 0;
  for i := 0 to EndInd do begin
    Result := Pos(Filter,UTF8LowerCase(aData[i]))>0;
    if Result then
      Exit;
  end;
  Result := False;
end;

procedure TListViewFilterEdit.SortAndFilter;
// Copy data from fOriginalData to fSortedData in sorted order
var
  Origi: Integer;
  Data: TStringArray;
begin
  fFilteredData.Clear;
  for Origi:=0 to fOriginalData.Count-1 do begin
    Data:=fOriginalData[Origi];
    if MatchesFilter(Data) then
      fFilteredData.Add(Data);
  end;
end;

procedure TListViewFilterEdit.ApplyFilterCore;
var
  i: Integer;
begin
  if fFilteredListview = nil then
    exit;
  fFilteredListview.Clear;
  fFilteredListview.Items.BeginUpdate;
  for i:=0 to fFilteredData.Count-1 do
    Data2ListItem(fFilteredData[i], fFilteredListview.Items.Add);
  fFilteredListview.AlphaSort;         // Always sorted.
  fFilteredListview.Items.EndUpdate;
end;

procedure TListViewFilterEdit.StoreSelection;
var
  i: Integer;
begin
  if fFilteredListview = nil then
    exit;
  fSelectionList.Clear;
  if fFilteredListview.SelCount > 0 then
    for i := 0 to fFilteredListview.Items.Count-1 do
      if fFilteredListview.Items[i].Selected then
        fSelectionList.Add(fFilteredListview.Items[i].Caption);
end;

procedure TListViewFilterEdit.RestoreSelection;
var
  i: Integer;
begin
  if fSelectionList.Count > 0 then
    for i := 0 to fFilteredListview.Items.Count-1 do
      if fSelectionList.IndexOf(fFilteredListview.Items[i].Caption) > -1 then
        fFilteredListview.Items[i].Selected:=True;
end;

procedure TListViewFilterEdit.MoveNext;
var
  i: Integer;
begin
  i := fFilteredListview.ItemIndex + 1;
  if fFilteredListview.Items.Count > 0 then begin
    if i < fFilteredListview.Items.Count then
      fFilteredListview.ItemIndex := i
    else
      fFilteredListview.ItemIndex := 0;
  end;
end;

procedure TListViewFilterEdit.MovePrev;
var
  i: Integer;
begin
  i := fFilteredListview.ItemIndex - 1;
  if fFilteredListview.Items.Count > 0 then begin
    if i >= 0 then
      fFilteredListview.ItemIndex := i
    else
      fFilteredListview.ItemIndex := fFilteredListview.Items.Count-1;
  end;
end;

function TListViewFilterEdit.ReturnKeyHandled: Boolean;
// Retuns true if the Return press was forwarded to the ListView
var
  Key: Char;
begin
  if fFilteredListview = nil then
    exit(false);
  Key:=Char(VK_RETURN);
  Result:=Assigned(fFilteredListview.OnKeyPress);
  if Result then
    fFilteredListview.OnKeyPress(fFilteredListview, Key);
end;

end.

