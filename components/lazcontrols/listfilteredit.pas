unit ListFilterEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, LResources, Graphics, Controls, StdCtrls, ComCtrls,
  LCLProc, EditBtn, FileUtil;

type

  TImageIndexEvent = function (Str: String; Data: TObject;
                               var IsEnabled: Boolean): Integer of object;

  { TListFilterEdit }

  TListFilterEdit = class(TCustomControlFilterEdit)
  private
    // A control showing the (filtered) data. These are exclusive, only one is used.
    fFilteredListbox: TListbox;
    fSelectionList: TStringList;       // or store/restore the old selections here.
    // Data supplied by caller through Data property.
    fOriginalData: TStringList;
    // Data sorted for viewing.
    fSortedData: TStringList;
    function CompareFNs(AFilename1,AFilename2: string): integer;
    procedure SetFilteredListbox(const AValue: TListBox);
  protected
    procedure MoveNext; override;
    procedure MovePrev; override;
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
    property Data: TStringList read fOriginalData;
  published
    property FilteredListbox: TListBox read fFilteredListbox write SetFilteredListbox;
  end;

var
  ListFilterGlyph: TBitmap;

procedure Register;

implementation

procedure Register;
begin
  {$I listfilteredit_icon.lrs}
  RegisterComponents('LazControls',[TListFilterEdit]);
end;

{ TListBoxFilterEdit }

constructor TListFilterEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fOriginalData:=TStringList.Create;
  fSelectionList:=TStringList.Create;
  fSortedData:=TStringList.Create;
end;

destructor TListFilterEdit.Destroy;
begin
  fSortedData.Free;
  fSelectionList.Free;
  fOriginalData.Free;
  inherited Destroy;
end;

function TListFilterEdit.GetDefaultGlyph: TBitmap;
begin
  Result := ListFilterGlyph;
end;

procedure TListFilterEdit.SetFilteredListbox(const AValue: TListBox);
begin
  if fFilteredListbox = AValue then Exit;
  fFilteredListbox:=AValue;
  if AValue = nil then Exit;
  fOriginalData.Assign(fFilteredListbox.Items);
end;

procedure TListFilterEdit.ApplyFilterCore;
var
  i: Integer;
  FileN: string;
begin
  fFilteredListbox.Clear;
  fFilteredListbox.Items.BeginUpdate;
  for i:=0 to fSortedData.Count-1 do begin
    FileN:=fSortedData[i];
    fFilteredListbox.Items.AddObject(FileN, fSortedData.Objects[i]);
    if Assigned(fSelectedPart) then
      fFilteredListbox.Selected[i]:=fSelectedPart=fSortedData.Objects[i];
  end;
  fFilteredListbox.Items.EndUpdate;
  DebugLn('TListFilterEdit.ApplyFilterCore 1, ItemIndex = ' + IntToStr(fFilteredListbox.ItemIndex));
  fFilteredListbox.ItemIndex:=-1;
  DebugLn('TListFilterEdit.ApplyFilterCore 2 after setting "ItemIndex:=-1;", ItemIndex = ' + IntToStr(fFilteredListbox.ItemIndex));
end;

function TListFilterEdit.CompareFNs(AFilename1,AFilename2: string): integer;
begin
  if SortData then
    Result:=CompareFilenames(AFilename1, AFilename2)
  else
    Result:=0;
end;

procedure TListFilterEdit.SortAndFilter;
// Copy data from fOriginalData to fSortedData in sorted order
var
  Origi, i: Integer;
  FileN: string;
begin
  fSortedData.Clear;
  for Origi:=0 to fOriginalData.Count-1 do begin
    FileN:=fOriginalData[Origi];
    if (Filter='') or (Pos(Filter,lowercase(FileN))>0) then begin
      i:=fSortedData.Count-1;
      while i>=0 do begin
        if CompareFNs(FileN,fSortedData[i])>=0 then break;
        dec(i);
      end;
      fSortedData.InsertObject(i+1,FileN, fOriginalData.Objects[Origi]);
    end;
  end;
end;

procedure TListFilterEdit.StoreSelection;
var
  i: Integer;
begin
  fSelectionList.Clear;
  for i := 0 to fFilteredListbox.Count-1 do begin
    if fFilteredListbox.Selected[i] then
      fSelectionList.Add(fFilteredListbox.Items[i]);
  end;
end;

procedure TListFilterEdit.RestoreSelection;
var
  i: Integer;
begin
  for i := 0 to fFilteredListbox.Count-1 do begin
    if fSelectionList.IndexOf(fFilteredListbox.Items[i])>0 then
      fFilteredListbox.Selected[i]:=True;
  end;
end;

procedure TListFilterEdit.MoveNext;
var
  i: Integer;
begin
  i := fFilteredListbox.ItemIndex + 1;
  if i < fFilteredListbox.Count then fFilteredListbox.ItemIndex := i;
end;

procedure TListFilterEdit.MovePrev;
var
  i: Integer;
begin
  i := fFilteredListbox.ItemIndex - 1;
  if i >= 0 then fFilteredListbox.ItemIndex := i;
end;

end.

