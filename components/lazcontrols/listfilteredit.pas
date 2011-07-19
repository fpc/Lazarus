unit ListFilterEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, LResources, LCLType, Graphics,
  Controls, StdCtrls, ComCtrls, EditBtn, FileUtil, AvgLvlTree, ImgList;

type

  TImageIndexEvent = function (Str: String; Data: TObject;
                               var IsEnabled: Boolean): Integer of object;

  { TListFilterEdit }

  TListFilterEdit = class(TCustomControlFilterEdit)
    procedure ListBoxDrawItem(Control: TWinControl;
      Index: Integer; ARect: TRect; State: TOwnerDrawState);
  private
    // A control showing the (filtered) data. These are exclusive, only one is used.
    fFilteredListbox: TListbox;
    fImageIndexDirectory: integer;     // Needed if directory structure is shown.
    fImages4Listbox: TCustomImageList; // Listbox does not have ImageList of its own.
    fSelectionList: TStringList;    // or store/restore the old selections here.
    // Data supplied by caller through Data property.
    // Objects property is passed to OnGetImageIndex.
    fOriginalData: TStringList;
    // Data sorted for viewing.
    fOnGetImageIndex: TImageIndexEvent;
    fImgIndex: Integer;
    fSortedData: TStringList;
    function CompareFNs(AFilename1,AFilename2: string): integer;
    procedure SetFilteredListbox(const AValue: TListBox);
  protected
    procedure MoveNext; override;
    procedure MovePrev; override;
    procedure SortAndFilter; override;
    procedure ApplyFilterCore; override;
    function GetDefaultGlyph: TBitmap; override;
    function GetDefaultGlyphName: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StoreSelection; override;
    procedure RestoreSelection; override;
  public
    property ImageIndexDirectory: integer read fImageIndexDirectory write fImageIndexDirectory;
    property SelectionList: TStringList read fSelectionList;
    property Data: TStringList read fOriginalData;
  published
    property FilteredListbox: TListBox read fFilteredListbox write SetFilteredListbox;
    property Images4Listbox: TCustomImageList read fImages4Listbox write fImages4Listbox; deprecated 'use OnDrawItem handler in FilteredListbox';
    property OnGetImageIndex: TImageIndexEvent read fOnGetImageIndex write fOnGetImageIndex; deprecated 'use OnDrawItem handler in FilteredListbox';
  end;

var
  ListFilterGlyph: TBitmap;

const
  ResBtnListFilter = 'btnfiltercancel';

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
  fImageIndexDirectory := -1;
  fImgIndex:=-1;
end;

destructor TListFilterEdit.Destroy;
begin
  fSortedData.Free;
  fSelectionList.Free;
  fOriginalData.Free;
  inherited Destroy;
end;

procedure TListFilterEdit.ListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  ImgIndex: Integer;
  ena: Boolean;
begin
  if Index < 0 then Exit;
  ena:=True;
  ImgIndex:=-1;
  if Assigned(fImages4Listbox) and Assigned(OnGetImageIndex) then begin
    ImgIndex:=OnGetImageIndex(fFilteredListbox.Items[Index],
                              fFilteredListbox.Items.Objects[Index], ena);
  end;
  fFilteredListbox.Canvas.FillRect(ARect);
  if ImgIndex<>-1 then
  begin
    if not (ena or (odSelected in State)) then
      fFilteredListbox.Canvas.Font.Color := clGreen;
    fImages4Listbox.Draw(fFilteredListbox.Canvas, 1, ARect.Top, ImgIndex, ena);
  end;
  fFilteredListbox.Canvas.TextRect(ARect, ARect.Left + 20,ARect.Top,
                                fFilteredListbox.Items[Index]);
end;

function TListFilterEdit.GetDefaultGlyph: TBitmap;
begin
  Result := ListFilterGlyph;
end;

function TListFilterEdit.GetDefaultGlyphName: String;
begin
  Result := ResBtnListFilter;
end;

procedure TListFilterEdit.SetFilteredListbox(const AValue: TListBox);
begin
  if fFilteredListbox = AValue then Exit;
  if Assigned(fFilteredListbox)
    and (fFilteredListbox.OnDrawItem = @ListBoxDrawItem) then
      fFilteredListbox.OnDrawItem := nil;
  fFilteredListbox:=AValue;
  if AValue = nil then Exit;
  if fFilteredListbox.Style<>lbStandard then begin
    // if Listbox.Style<>Standard then it is OwnerDraw,
    // and OnDrawItem should be defined, so the following code is useless
    if not (csDesigning in fFilteredListbox.ComponentState)
      and not Assigned(fFilteredListbox.OnDrawItem) then
        // AP: I propose not to use ListBoxDrawItem at all, DrawItem handler of FilteredListbox must be used
        fFilteredListbox.OnDrawItem:=@ListBoxDrawItem;
  end;
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

