{
 *****************************************************************************
  This file is part of a Lazarus Package, IconFinder.

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.
 *****************************************************************************

 Icon thumbnails.
}

unit IconThumbNails;

{$mode ObjFPC}{$H+}
{$define OVERLAY_ICONS}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, fgl, FPImage, StrUtils, LazLoggerBase,
  laz2_dom, laz2_xmlread, laz2_xmlwrite,
  FileUtil, LazFileUtils, Graphics, Controls, Dialogs, Menus, Forms,
  IconFinderStrConsts, BasicThumbnails;

type
  TIconItem = class;
  TIconThumbnailViewer = class;

  TIconStyle = (isAnyStyle, isClassic, isFlat, isOutline, isOutline2);

  TIconThumbnail = class(TBasicThumbnail)
  private
    FItem: TIconItem;
  public
    constructor Create(AItem: TIconItem); reintroduce;
    procedure DrawToCanvas(ACanvas: TCanvas; ARect: TRect); override;
    property Item: TIconItem read FItem;
  end;

  TIconItem = class(TObject)
  private
    FFileName: String;   // including path
    FWidth: Integer;
    FHeight: Integer;
    FStyle: TIconStyle;
    FKeywords: TStrings;
    FPicture: TPicture;
    FHidden: Boolean;
    FViewer: TIconThumbnailViewer;
    procedure SetStyleAsString(AValue: String);
  protected
    function GetDirectory: String;
    function GetKeywordCount: Integer;
    function GetKeywords(AIndex: Integer): String;
    function GetKeywordsAsString: String;
    function GetName: String;
    function GetNameBase: String;
    function GetPicture: TPicture;
    function GetSizeAsString: String;
    function GetStyleAsString: String;
  public
    constructor Create(AFileName, AKeywords: String; AStyle: TIconStyle; AWidth, AHeight: Integer);
    destructor Destroy; override;
    procedure CopyMetadataFrom(AIcon: TIconItem);
    procedure ExportKeywordsToStrings(AList: TStrings);
    function HasKeyword(AKeyword: String): Boolean;
    function HasKeywordPart(AKeywordPart: String): Boolean;
    procedure ReleasePicture;
    procedure SetKeywordsFromStrings(AList: TStrings);

    property Directory: String read GetDirectory;
    property FileName: String read FFileName;
    property Height: Integer read FHeight;
    property Hidden: Boolean read FHidden write FHidden;
    property KeywordCount: Integer read GetKeywordCount;
    property Keywords[AIndex: Integer]: String read GetKeywords;
    property KeywordsAsString: String read GetKeywordsAsString;
    property Name: String read GetName;
    property NameBase: String read GetNameBase;
    property Picture: TPicture read GetPicture;
    property SizeAsString: String read GetSizeAsString;
    property Style: TIconStyle read FStyle write FStyle;
    property StyleAsString: String read GetStyleAsString write SetStyleAsString;
    property Width: Integer read FWidth;
  end;

  TIconList = class(specialize TFPGObjectList<TIconItem>)
  public
    function IndexOfFileName(AFileName: String): Integer;
  end;

  TIconFolderItem = class
    FolderName: String;
    Hidden: Boolean;
    Dirty: Boolean;
  end;

  TIconFolderList = class(specialize TFPGObjectlist<TIconFolderItem>)
  public
    function AddFolder(AFolderName: String; IsHidden: Boolean): Integer;
    procedure Hide(AFolder: String);
    procedure Hide(AIndex: Integer);
    function IndexOf(AFolder: String): Integer;
    function IsHidden(AFolder: string): Boolean;
    function IsHidden(AIndex: Integer): Boolean;
    procedure Show(AFolder: string);
    procedure Show(AIndex: Integer);
    procedure Toggle(AFolder: String);
    procedure Toggle(AIndex: Integer);
  end;

  TIconThumbnailViewer = class(TBasicThumbnailViewer)
  private
    FIconFolders: TIconFolderList;
    FIconList: TIconList;
    FSizes: TStrings;
    FFilterByIconSize: String;
    FFilterByIconStyle: TIconStyle;
    FFilterByIconWidth: Integer;
    FFilterByIconHeight: Integer;
    FFilterByIconKeywords: String;
    FLargestIconWidth: Integer;
    FLargestIconHeight: Integer;
    FAutoThumbnailSize: Boolean;
    FFilterLock: Integer;
    FOnFilter: TNotifyEvent;
    function GetIconCount: Integer;
    procedure IconFolderClicked(Sender: TObject);
    procedure SetFilterByIconKeywords(AValue: String);
    procedure SetFilterByIconSize(AValue: String);
    procedure SetFilterByIconStyle(AValue: TIconStyle);

  protected
    FSelectedIcon: TIconItem;
    {$ifdef OVERLAY_ICONS}
    FOverlayIcons: array[0..2] of TGraphic;
    procedure DrawThumbnail(AThumbnail: TBasicThumbnail; ARect: TRect); override;
    {$endif}
    function AcceptIcon(AIcon: TIconItem): Boolean; virtual;
    function AcceptKeywords(AIcon: TIconItem): Boolean;
    function AddIcon(AFileName, AKeywords: String; AStyle: TIconStyle; AWidth, AHeight: Integer): TIconItem;
    procedure DeleteIconFolder(AFolder: String);
    procedure FilterIcons;
    function MetadataDirty: Boolean;
    procedure ReadIconFolder(AFolder: String);
    procedure ReadIcons(AFolder: String; AHidden: Boolean);
    procedure ReadMetadataFile(AFileName: String; AHidden: Boolean);
    procedure SetSelectedIndex(AValue: Integer); override;
    function ThumbnailMarked(AThumbnail: TBasicThumbnail): Boolean; override;
    procedure ThumbnailOutside(AThumbnail: TBasicThumbnail); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddIconFolder(AFolder: String; Hidden: Boolean = false);
    procedure Clear; override;
    procedure CopyMetadataToNameBase(AIcon: TIconItem);
    procedure DeleteIcon(AIcon: TIconItem);
    function FilterLocked: Boolean;
    function FindIconSize(AIcon: TIconItem; AWidth, AHeight: Integer): TIconItem;
    function FindLargestIcon(AIcon: TIconItem): TIconItem;
    procedure GetIconSizesAsStrings(AList: TStrings);
    procedure GetKeywordsAsStrings(AList: TStrings);
    function IndexOfThumbnail(AIcon: TIconItem): Integer;
    procedure LockFilter;
    procedure PopulateIconFoldersMenu(AMenu: TMenu);
    procedure ReadIconFolders(AList: TStrings);
    function SelectIconInFile(AFileName: String): Boolean;
    procedure UnlockFilter;
    procedure UpdateIconFolders;
    procedure WriteIconFolders(AList: TStrings);
    procedure WriteMetadataFiles;

    property AutoThumbnailSize: Boolean read FAutoThumbnailSize write FAutoThumbnailSize default true;
    property FilterByIconKeywords: string read FFilterByIconKeywords write SetFilterByIconKeywords;
    property FilterByIconSize: string read FFilterByIconSize write SetFilterByIconSize;
    property FilterByIconStyle: TIconStyle read FFilterByIconStyle write SetFilterByIconStyle;
    property IconCount: Integer read GetIconCount;
    property IconFolders: TIconFolderList read FIconFolders;
    property LargestIconWidth: Integer read FLargestIconWidth;
    property LargestIconHeight: Integer read FLargestIconHeight;
    property SelectedIcon: TIconItem read FSelectedIcon;
    property OnFilter: TNotifyEvent read FOnFilter write FOnFilter;

  end;

function IconStyleToStr(AStyle: TIconStyle): String;
procedure IconStylesToStrings(AList: TStrings);
function StrToIconStyle(AText: String): TIconStyle;

const
  IMAGES_MASK = '*.png;*.bmp';


implementation

{$ifdef OVERLAY_ICONS}
 {$R overlay.res}
{$endif}

const
  ICON_MARGIN = 8;  // or, more precisely: double of margin
  METADATA_FILENAME = 'metadata.xml';
  ICONSTYLE_NAMES: Array[TIconStyle] of String = (
    '(any style)', 'classic', 'flat', 'outline', 'outline 2-color'
  );

function IconStyleToStr(AStyle: TIconStyle): String;
begin
  if AStyle = isAnyStyle then
    Result := ''
  else
    Result := ICONSTYLE_NAMES[AStyle];
end;

procedure IconStylesToStrings(AList: TStrings);
var
  style: TIconStyle;
begin
  AList.Clear;
  for style in TIconStyle do
    if style = isAnyStyle then
      AList.Add(RSMetadata_AnyStyle)
    else
      AList.Add(ICONSTYLE_NAMES[style]);
end;

function StrToIconStyle(AText: String): TIconStyle;
begin
  case lowercase(AText) of
    '':
      Result := isAnyStyle;
    'classic':
      Result := isClassic;
    'flat':
      Result := isFlat;
    'outline':
      Result := isOutline;
    'outline2', 'outline 2-color':
      Result := isOutline2;
    else
      raise Exception.Create('[StrToIconStyle] Unknown icon style');
  end;
end;


{ TIconThumbnail }

constructor TIconThumbnail.Create(AItem: TIconItem);
begin
  inherited Create;
  FItem := AItem;  // Do not destroy this item!
end;

procedure TIconThumbnail.DrawToCanvas(ACanvas: TCanvas; ARect: TRect);
var
  pic: TPicture;
  x, y: Integer;
begin
  pic := FItem.Picture;
  if pic <> nil then
  begin
    x := (Viewer.ThumbnailWidth - pic.Width) div 2;
    y := (Viewer.ThumbnailHeight - pic.Height) div 2;
    ACanvas.Draw(ARect.Left + x, ARect.Top + y, pic.Bitmap);
  end;
end;


{ TIconItem }

constructor TIconItem.Create(AFileName, AKeywords: String; AStyle: TIconStyle;
  AWidth, AHeight: Integer);
var
  i: Integer;
begin
  inherited Create;
  FFileName := AFileName;
  FStyle := AStyle;
  FWidth := AWidth;
  FHeight := AHeight;
  FKeywords := TStringList.Create;
  TStringList(FKeywords).Sorted := true;
  TStringList(FKeywords).CaseSensitive := false;
  TStringList(FKeywords).Duplicates := dupIgnore;
  FKeywords.Delimiter := ';';
  FKeywords.StrictDelimiter := true;
  FKeywords.DelimitedText := AKeywords;
  for i := FKeywords.Count-1 downto 0 do  // No empty keywords
    if FKeywords[i] = '' then FKeywords.Delete(i);
end;

destructor TIconItem.Destroy;
begin
  FKeywords.Free;
  FreeAndNil(FPicture);
  inherited;
end;

procedure TIconItem.CopyMetadataFrom(AIcon: TIconItem);
var
  folder: String;
  idx: Integer;
begin
  FKeywords.Assign(AIcon.FKeywords);
  FStyle := AIcon.FStyle;

  // Mark the folder of the icon as "dirty" so that it can be re-written.
  folder := AIcon.GetDirectory;
  idx := FViewer.IconFolders.IndexOf(folder);
  if idx > -1 then
    FViewer.IconFolders[idx].Dirty := true;
end;

function TIconItem.GetDirectory: String;
begin
  Result := ExtractFilePath(FFileName);
end;

function TIconItem.GetKeywordCount: Integer;
begin
  Result := FKeywords.Count;
end;

function TIconItem.GetKeywords(AIndex: Integer): String;
begin
  Result := FKeywords[AIndex];
end;

function TIconItem.GetKeywordsAsString: String;
var
  i: Integer;
begin
  if FKeywords.Count = 0 then
    Result := ''
  else
  begin
    Result := FKeywords[0];
    for i := 1 to FKeywords.Count-1 do
      Result := Result + ';' + FKeywords[i];
  end;
end;

function TIconItem.GetName: String;
begin
  Result := ChangeFileExt(ExtractFileName(FFileName), '');
end;

function TIconItem.GetNameBase: String;
var
  p, n: Integer;
  suffix: String;
begin
  Result := GetName;
  p := RPos('_', Result);
  if p > 0 then
  begin
    suffix := Copy(Result, p+1);
    if TryStrToInt(suffix, n) then
      Result := Copy(Result, 1, p-1);
  end;
end;

function TIconItem.GetPicture: TPicture;
begin
  if FPicture = nil then
  begin
    FPicture := TPicture.Create;
    FPicture.LoadFromFile(FFileName);
  end;
  Result := FPicture;
end;

function TIconItem.GetSizeAsString: String;
begin
  Result := Format('%d x %d', [FWidth, FHeight]);
end;

function TIconItem.GetStyleAsString: String;
begin
  Result := ICONSTYLE_NAMES[FStyle];
end;

function TIconItem.HasKeyword(AKeyword: String): Boolean;
begin
  Result := FKeywords.IndexOf(AKeyword) <> -1;
end;

function TIconItem.HasKeywordPart(AKeywordPart: String): Boolean;
var
  i: Integer;
begin
  Result := true;
  AKeywordPart := Lowercase(AKeywordPart);
  for i := 0 to FKeywords.Count-1 do
    if pos(AKeywordPart, Lowercase(FKeywords[i])) = 1 then
      exit;
  Result := false;
end;

procedure TIconItem.ExportKeywordsToStrings(AList: TStrings);
begin
  Assert(AList <> nil);
  AList.Assign(FKeywords);
end;

procedure TIconItem.ReleasePicture;
begin
  FreeAndNil(FPicture);
end;

procedure TIconItem.SetKeywordsFromStrings(AList: TStrings);
var
  i, j: Integer;
begin
  FKeywords.BeginUpdate;
  try
    FKeywords.Clear;
    for i := 0 to AList.Count-1 do
    begin
      j := FKeywords.IndexOf(AList[i]);
      if j = -1 then
        FKeywords.Add(AList[i]);
    end;
  finally
    FKeywords.EndUpdate;
  end;
end;

procedure TIconItem.SetStyleAsString(AValue: String);
begin
  FStyle := StrToIconStyle(AValue);
end;


{ TIconList }

function TIconList.IndexOfFileName(AFileName: String): Integer;
var
  i: Integer;
  item: TIconItem;
begin
  for i := 0 to Count-1 do
  begin
    item := Items[i];
    if item.FileName = AFileName then
    begin
      Result := i;
      exit;
    end;
  end;
  Result := -1;
end;


{ TIconFolderList

  A list with the folder names stored in TIconFolderItem instances.
  Besides the folder names, each item contains a flag to hide the icons of
  that folder, as well as a Dirty flag to indicate that there are icons in
  that folder with modified metadata and that the folder's metadata need
  re-saving.

  Each folder name is stored with correct and trailing path delimiters.
}
function TIconFolderList.AddFolder(AFolderName: String; IsHidden: Boolean): Integer;
var
  item: TIconFolderItem;
begin
  item := TIconFolderItem.Create;
  item.FolderName := AppendPathDelim(SwitchPathDelims(AFolderName, true));
  item.Hidden := IsHidden;
  item.Dirty := false;
  Result := Add(item);
end;

procedure TIconFolderList.Hide(AFolder: String);
var
  idx: Integer;
begin
  idx := IndexOf(AFolder);
  if idx > -1 then
    Hide(idx);
end;

procedure TIconFolderList.Hide(AIndex: Integer);
begin
  Items[AIndex].Hidden := true;
end;

function TIconFolderList.IndexOf(AFolder: String): Integer;
var
  i: Integer;
begin
  AFolder := SwitchPathDelims(AppendPathDelim(AFolder), true);
  for i := 0 to Count-1 do
  begin
    if Items[i].FolderName = AFolder then
    begin
      Result := i;
      exit;
    end;
  end;
  Result := -1;
end;

function TIconFolderList.IsHidden(AFolder: string): Boolean;
var
  idx: Integer;
begin
  idx := IndexOf(AFolder);
  if idx > -1 then
    Result := Items[idx].Hidden
  else
    Result := false;
end;

function TIconFolderList.IsHidden(AIndex: Integer): Boolean;
begin
  Result := Items[AIndex].Hidden;
end;

procedure TIconFolderList.Show(AFolder: string);
var
  idx: Integer;
begin
  idx := IndexOf(AFolder);
  if idx > -1 then
    Show(idx);
end;

procedure TIconFolderList.Show(AIndex: Integer);
begin
  Items[AIndex].Hidden := false;
end;

procedure TIconFolderList.Toggle(AFolder: String);
var
  idx: Integer;
begin
  idx := IndexOf(AFolder);
  if idx > -1 then
  begin
    Toggle(idx);
  end;
end;

procedure TIconFolderList.Toggle(AIndex: Integer);
begin
  Items[AIndex].Hidden := not Items[AIndex].Hidden;
end;


{ TIconThumbnailViewer }

constructor TIconThumbnailViewer.Create(AOwner: TComponent);
begin
  inherited;
  FIconFolders := TIconFolderList.Create;
  FIconList := TIconList.Create;
  FSizes := TStringList.Create;
  TStringList(FSizes).Sorted := true;
  FAutoThumbnailSize := true;
  {$ifdef OVERLAY_ICONS}
  FOverlayIcons[0] := TPortableNetworkGraphic.Create;
  FOverlayIcons[1] := TPortableNetworkGraphic.Create;
  FOverlayIcons[2] := TPortableNetworkGraphic.Create;
  FOverlayIcons[0].LoadFromResourceName(HINSTANCE, 'ovl_H_8');
  FOverlayIcons[1].LoadFromResourceName(HINSTANCE, 'ovl_H_12');
  FOverlayIcons[2].LoadFromResourceName(HINSTANCE, 'ovl_H_16');
  {$endif}
end;

destructor TIconThumbnailViewer.Destroy;
var
  res: Integer;
begin
  {$ifdef OVERLAY_ICONS}
  FOverlayIcons[2].Free;
  FOverlayIcons[1].Free;
  FOverlayIcons[0].Free;
  {$endif}
  if MetadataDirty then
  begin
    res := MessageDlg(RSIconViewer_AskSaveMetadata, mtConfirmation, [mbYes, mbNo], 0);
    if res = mrYes then
      WriteMetadataFiles;
  end;
  FSizes.Free;
  FIconList.Free;
  FIconFolders.Free;
  inherited;
end;

{ Main filtering method: Compares the icon properties with the filter conditions
  and returns true when the icon can be displayed as a thumbnail. }
function TIconThumbnailViewer.AcceptIcon(AIcon: TIconItem): Boolean;
begin
  Result := false;
  if AIcon.Hidden then
    exit;
  if (FFilterByIconSize <> '') and ((AIcon.Width <> FFilterByIconWidth) or (AIcon.Height <> FFilterByIconHeight)) then
    exit;
  if (FFilterByIconStyle <> isAnyStyle) and (AIcon.Style <> FFilterByIconStyle) then
    exit;
  if (FFilterByIconKeywords <> '') and not AcceptKeywords(AIcon) then
    exit;
  Result := true;
  if Result then
  begin
    if FLargestIconWidth < AIcon.Width then FLargestIconWidth := AIcon.Width;
    if FLargestIconHeight < AIcon.Height then FLargestIconHeight := AIcon.Height;
  end;
end;

{ Implements a simple parser for logical expressions between keywords, e.g.
     keyword1 AND keyword2
     keyword1 OR NOT keyword2 AND keyword3 AND NOT keyword4
     keyword1 keyword2  -- defaults to AND
  No brackets allowed! }
function TIconThumbnailViewer.AcceptKeywords(AIcon: TIconItem): Boolean;
type
  TOperationKind = (opkAND, opkOR, opkNOT);
  TOperation = record
    Kind: TOperationKind;
    Left, Right: Boolean;
    Complete: Boolean;
  end;
const
  DEFAULT_OPERATION = opkAND;
var
  operation: TOperation;
  i: Integer;
  parts: TStringArray;
begin
  Result := True;
  parts := FFilterByIconKeywords.Split(' ');

  operation := Default(TOperation);
  operation.Kind := DEFAULT_OPERATION;
  operation.Left := true;

  i := 0;
  while i < Length(parts) do
  begin
    case Uppercase(parts[i]) of
      'AND': operation.Kind := opkAND;
      'OR': operation.Kind := opkOR;
      'NOT':
        begin
          inc(i);
          operation.Right := not AIcon.HasKeywordPart(parts[i]);
          operation.Complete := true;
        end;
      else
        operation.Right := AIcon.HasKeywordPart(parts[i]);
        operation.Complete := true;
    end;
    if operation.Complete then
    begin
      case operation.Kind of
        opkAND: operation.Left := operation.Left and operation.Right;
        opkOR:  operation.Left := operation.Left or operation.Right;
      end;
      operation.Complete := false;
      operation.Kind := DEFAULT_OPERATION;
    end;
    inc(i);
  end;
  Result := operation.Left;
end;

{ Adds the associated icon to the unfiltered icon list. }
function TIconThumbnailViewer.AddIcon(AFileName, AKeywords: String; AStyle: TIconStyle;
  AWidth, AHeight: Integer): TIconItem;
var
  sizeStr: String;
  idx: Integer;
begin
  idx := FIconList.IndexOfFileName(AFileName);
  if idx = -1 then
  begin
    Result := TIconItem.Create(AFileName, AKeywords, AStyle, AWidth, AHeight);
    Result.FViewer := Self;
    FIconList.Add(Result);
  end else
    Result := FIconList[idx];

  sizeStr := Format('%d x %d', [AWidth, AHeight]);
  if FSizes.IndexOf(sizeStr) = -1 then
    FSizes.Add(sizestr);
end;

procedure TIconThumbnailViewer.AddIconFolder(AFolder: String; Hidden: Boolean = false);
begin
  AFolder := AppendPathDelim(SwitchPathDelims(AFolder, true));

  if FIconFolders.IndexOf(AFolder) > -1 then   // Avoid duplicates
    DeleteIconFolder(AFolder);

  FIconFolders.AddFolder(AFolder, Hidden);
  ReadIconFolder(AFolder);

  FilterIcons;
  SelectedIndex := -1;
end;

procedure TIconThumbnailViewer.Clear;
begin
  inherited;
  FLargestIconWidth := 0;
  FLargestIconHeight := 0;
  FSizes.Clear;
  FIconList.Clear;
  FIconFolders.Clear;
end;

{ Copies the metadata from the given icon to all other icons sharing the
  same name base (= part in name before the last '_') and directory. }
procedure TIconThumbnailViewer.CopyMetadataToNameBase(AIcon: TIconItem);
var
  i: Integer;
  item: TIconItem;
  itemDir, iconDir: String;
  iconNameBase: String;
begin
  iconNameBase := AIcon.NameBase;
  iconDir := AIcon.Directory;
  for i := 0 to FIconList.Count-1 do
  begin
    item := FIconList[i];
    itemDir := FIconList[i].Directory;
    if SameText(iconNameBase, item.NameBase) and SameText(iconDir, itemDir) and (item <> AIcon) then
      item.CopyMetadataFrom(AIcon);
  end;
end;

procedure TIconThumbnailViewer.DeleteIcon(AIcon: TIconItem);
var
  selIdx: Integer = -1;
  iconIdx: Integer;
begin
  if AIcon = SelectedIcon then
    selIdx := SelectedIndex;

  iconIdx := FIconList.IndexOf(AIcon);
  if iconIdx <> -1 then
  begin
    FIconList.Delete(iconIdx);
    LockFilter;
    try
      if (selIdx <> -1) then
      begin
        if selIdx >= ThumbnailCount then selIdx := ThumbnailCount-1;
        SelectedIndex := selIdx;
      end;
    finally
      UnlockFilter;
    end;
  end;
end;

procedure TIconThumbnailViewer.DeleteIconFolder(AFolder: String);
var
  i: Integer;
  folder: String;
begin
  AFolder := AppendPathDelim(SwitchPathDelims(AFolder, true));

  for i := FIconFolders.Count-1 downto 0 do
    if FIconFolders[i].FolderName = AFolder then
      FIconFolders.Delete(i);

  for i := FIconList.Count-1 downto 0 do
  begin
    folder := ExtractFilePath(FIconList[i].FileName);
    if folder = AFolder then
      FIconList.Delete(i);
  end;
end;

{$ifdef OVERLAY_ICONS}
procedure TIconThumbnailViewer.DrawThumbnail(AThumbnail: TBasicThumbnail; ARect: TRect);
var
  ovl: TGraphic;
  ppi: Integer;
  item: TIconItem;
begin
  inherited;

  item := TIconThumbnail(AThumbnail).Item;
  if (item.KeywordCount = 0) or (item.Style = isAnyStyle) then
  begin
    ppi := Font.PixelsPerInch;
    if ppi < 120 then
      ovl := FOverlayIcons[0]
    else if ppi < 168 then
      ovl := FOverlayIcons[1]
    else
      ovl := FOverlayIcons[2];
    Canvas.Draw(ARect.Left+1, ARect.Top+1, ovl);
  end;
end;
{$endif}

procedure TIconThumbnailViewer.FilterIcons;
var
  i: Integer;
  item: TIconItem;
  oldNameBase: String = '';
begin
  if FilterLocked then
    exit;

  if SelectedIcon <> nil then
    oldNameBase := SelectedIcon.NameBase;

  ThumbnailList.Clear;

  for i := 0 to FIconList.Count-1 do
  begin
    item := FIconList[i];
    if AcceptIcon(item) then
      Add(TIconThumbnail.Create(item));
  end;

  if FAutoThumbnailSize then
  begin
    if FFilterByIconSize = '' then
    begin
      FThumbnailWidth := FLargestIconWidth + ICON_MARGIN;
      FThumbnailHeight := FLargestIconHeight + ICON_MARGIN;
    end else
    begin
      FThumbnailWidth := FFilterByIconWidth + ICON_MARGIN;
      FThumbnailHeight := FFilterByIconHeight + ICON_MARGIN;
    end;
  end;

  LayoutThumbnails;

  if oldNameBase <> '' then
    for i := 0 to ThumbnailCount-1 do
      if (TIconThumbnail(Thumbnail[i]).Item.NameBase = oldNameBase) then
      begin
        SelectedIndex := i;
        exit;
      end;

  SelectedIndex := -1;

  if Assigned(FOnFilter) then
    FOnFilter(self);
end;

function TIconThumbnailViewer.FilterLocked: Boolean;
begin
  Result := FFilterLock <> 0;
end;

{ Finds the icon list entry for the specified item which has the same
  namebase, but the specified size.
  When AWidth = -1 and AHeight = -1 then the file without appendix is used. }
function TIconThumbnailViewer.FindIconSize(AIcon: TIconItem; AWidth, AHeight: Integer): TIconItem;
var
  i: Integer;
  iconNameBase: String;
  item: TIconItem;
  itemNameBase: String;
begin
  iconNameBase := AIcon.NameBase;
  for i := 0 to FIconList.Count-1 do
  begin
    item := FIconList[i];
    itemNameBase := item.NameBase;
    if SameText(itemNameBase, iconNameBase) then
    begin
      // Items with appendix
      if Length(item.Name) <> Length(itemNameBase) then
      begin
        if (item.Width = AWidth) and (item.Height = AHeight) then
        begin
          Result := item;
          exit;
        end;
      end else
      begin
        // Item without appendix
        Result := item;
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TIconThumbnailViewer.FindLargestIcon(AIcon: TIconItem): TIconItem;
var
  i: Integer;
  iconNameBase: String;
  item: TIconItem;
  itemNameBase: String;
  w: Integer;
begin
  Result := nil;
  iconNameBase := AIcon.NameBase;
  w := 0;
  for i := 0 to FIconList.Count-1 do
  begin
    item := FIconList[i];
    itemNameBase := item.NameBase;
    if SameText(itemNameBase, iconNameBase) then
    begin
      if item.Width > w then  // considering only width
      begin
        w := item.Width;
        Result := item;
      end;
    end;
  end;
end;

{ Returns the number of unfiltered icons loaded. }
function TIconThumbnailViewer.GetIconCount: Integer;
begin
  Result := FIconList.Count;
end;

{ Returns a sorted list with all available icon sizes, formatted as "width x height" }
procedure TIconThumbnailViewer.GetIconSizesAsStrings(AList: TStrings);
begin
  AList.Assign(FSizes);
end;

procedure TIconThumbnailViewer.GetKeywordsAsStrings(AList: TStrings);
var
  i, j, k: Integer;
  keyword: String;
  item: TIconItem;
  list: TStringList;
begin
  list := TStringList.Create;
  try
    list.CaseSensitive := false;
    list.Sorted := true;
    for i := 0 to FIconList.Count-1 do
    begin
      item := FIconList[i];
      for j := 0 to item.KeywordCount-1 do
      begin
        keyword := item.Keywords[j];
        if not list.Find(keyword, k) then
          list.Add(keyword);
      end;
    end;
    AList.Assign(list);
  finally
    list.Free;
  end;
end;

{ OnClick handler for the menu items created by PopulateIconFoldersMenu to
  show/hide the icons of the clicked folder.
  The FIconFolders index of the folder is stored in the Tag of the menu item.
  Special tags: -1 --> show all folders, -2 --> hide all folders. }
procedure TIconThumbnailViewer.IconFolderClicked(Sender: TObject);
var
  i, idx: Integer;
begin
  if TMenuItem(Sender).Tag < 0 then
  begin
    for i := 0 to FIconFolders.Count -1 do
    begin
      case TMenuItem(Sender).Tag of
        -1: // "Show all"
            TIconFolderList(FIconFolders).Show(i);
        -2: // "Hide all"
            TIconFolderList(FIconFolders).Hide(i);
        else
            exit;
      end;
    end;
  end else
  begin
    idx := TMenuItem(Sender).Tag;
    TIconFolderList(FIconFolders).Toggle(idx);
  end;

//  LockFilter;
//  try
    UpdateIconFolders;
//  finally
//    UnlockFilter;
//  end;
end;

function TIconThumbnailViewer.IndexOfThumbnail(AIcon: TIconItem): Integer;
var
  i: Integer;
begin
  Result := -1;
  if AIcon = nil then
    exit;

  for i := 0 to ThumbnailCount-1 do
    if TIconThumbnail(Thumbnail[i]).Item = AIcon then
    begin
      Result := i;
      exit;
    end;
end;

procedure TIconThumbnailViewer.LockFilter;
begin
  inc(FFilterLock);
end;

{ Returns true if at least one of the icon folders has been marked as "dirty"
  after changing its metadata.
  Called when the IconViewer is destroyed. Is evaluated for re-writing the metadata. }
function TIconThumbnailViewer.MetadataDirty: Boolean;
var
  i: Integer;
begin
  for i := 0 to FIconFolders.Count-1 do
    if FIconFolders[i].Dirty then
    begin
      Result := true;
      exit;
    end;
  Result := false;
end;

{ Populates the given menu with the names of all folders from which icons have
  been loaded. Hidden folders (having a non-nil Objects property in the
  IconFolder list) are not checked in the menu. }
procedure TIconThumbnailViewer.PopulateIconFoldersMenu(AMenu: TMenu);
var
  i: Integer;
  menuitem: TMenuItem;
begin
  AMenu.Items.Clear;

  menuItem := TMenuItem.Create(AMenu);
  menuItem.Caption := RSIconViewer_ShowAll;
  menuItem.Tag := -1;
  menuItem.OnClick := @IconFolderClicked;
  AMenu.Items.Add(menuItem);

  menuItem := TMenuItem.Create(AMenu);
  menuItem.Caption := RSIconViewer_HideAll;
  menuItem.Tag := -2;
  menuItem.OnClick := @IconFolderClicked;
  AMenu.Items.Add(menuItem);

  menuItem := TMenuItem.Create(AMenu);
  menuItem.Caption := '-';
  AMenu.Items.Add(menuItem);

  for i := 0 to FIconFolders.Count-1 do
  begin
    menuItem := TMenuItem.Create(AMenu);
    menuItem.Checked := not FIconFolders[i].Hidden;
    menuItem.Caption := FIconFolders[i].FolderName;
    menuItem.AutoCheck := true;
    menuItem.Tag := i;
    menuItem.OnClick := @IconFolderClicked;
    AMenu.Items.Add(menuItem);
  end;
end;

{ Reads the icons found in the specified folder. }
procedure TIconThumbnailViewer.ReadIconFolder(AFolder: String);
var
  isHidden: Boolean;
begin
  if AFolder = '' then
    exit;
  AFolder := AppendPathDelim(SwitchPathDelims(AFolder, true));
  isHidden := TIconFolderList(FIconFolders).IsHidden(AFolder);
  if (not DirectoryExists(AFolder)) or isHidden then
    exit;

  if FileExists(AFolder + METADATA_FILENAME) then
    ReadMetadataFile(AFolder + METADATA_FILENAME, isHidden)
  else
    ReadIcons(AFolder, isHidden);
end;

{ Reads the icons found in the folders of the given list.
  List items with a non-nil Objects property are marked as being hidden.
  Their names are stored but their icons are not displayed. }
procedure TIconThumbnailViewer.ReadIconFolders(AList: TStrings);
var
  i: Integer;
  selectedIconFileName: String = '';
  folder: String;
  isHidden: Boolean;
begin
  if SelectedIcon <> nil then
    selectedIconFileName := SelectedIcon.FileName;;
  SelectedIndex := -1;  // this sets FSelectedIcon to nil.
  FIconFolders.Clear;
  FIconList.Clear;
  for i := 0 to AList.Count-1 do
  begin
    folder := AList[i];
    isHidden := AList.Objects[i] <> nil;
    if FIconFolders.IndexOf(folder) > -1 then    // Avoid duplicates
      DeleteIconFolder(folder);
    TIconFolderList(FIconFolders).AddFolder(folder, isHidden);
    ReadIconFolder(AList[i]);
  end;
  FilterIcons;
  SelectIconInFile(selectedIconFileName);
end;

{ Looks for image files (*.png, *.bmp) in the given folder and adds them to
  the viewer.
  When AHidden is true all icons are marked as hidden, i.e. are not displayed. }
procedure TIconThumbnailViewer.ReadIcons(AFolder: String; AHidden: Boolean);
var
  files: TStrings;
  reader: TFPCustomImageReaderClass;
  stream: TStream;
  i, w, h: Integer;
begin
  files := TStringList.Create;
  try
    FindAllFiles(files, AFolder, IMAGES_MASK, false);
    for i := 0 to files.Count-1 do
    begin
      stream := TFileStream.Create(files[i], fmOpenRead or fmShareDenyNone);
      try
        reader := TFPCustomImage.FindReaderFromStream(stream);
        if reader <> nil then
        begin
          stream.Position := 0;
          with reader.ImageSize(stream) do
          begin
            w := X;
            h := Y;
          end;
          AddIcon(files[i], '', isAnyStyle, w, h).Hidden := AHidden;
        end;
      finally
        stream.Free;
      end;
    end;
  finally
    files.Free;
  end;
end;

procedure TIconThumbnailViewer.ReadMetadataFile(AFileName: String; AHidden: Boolean);
var
  doc: TXMLDocument = nil;
  iconsNode, iconNode: TDOMNode;
  keywordsNode, keywordNode: TDOMNode;
  folder, fn: String;
  i: Integer;
  w, h: Integer;
  style: TIconStyle;
  s: String;
  keywords: String;
  files: TStringList;
  stream: TStream;
  reader: TFPCustomImageReaderClass;
begin
  folder := ExtractFilePath(AFileName);
  files := TStringList.Create;
  try
    files.Sorted := true;
    FindAllFiles(files, folder, IMAGES_MASK, false);
    ReadXMLFile(doc, AFileName);
    iconsNode := doc.DocumentElement.FindNode('icons');
    iconNode := iconsNode.FindNode('icon');
    while iconNode <> nil do begin
      fn := '';
      style := isAnystyle;
      if iconNode.HasAttributes then
        for i := 0 to iconNode.Attributes.Length-1 do
        begin
          s := iconNode.Attributes[i].NodeValue;
          case iconNode.Attributes[i].NodeName of
            'filename': fn := s;
            'width': w := StrToIntDef(s, 0);
            'height': h := StrToIntDef(s, 0);
            'style': style := StrToIconStyle(s);
          end;
        end;
      keywords := '';
      keywordsNode := iconNode.FindNode('keywords');
      if keywordsNode <> nil then
      begin
        keywordNode := keywordsNode.FindNode('keyword');
        while keywordNode <> nil do
        begin
          s := keywordNode.TextContent;
          keywords := keywords + ';' + s;
          keywordNode := keywordNode.NextSibling;
        end;
      end;
      if keywords <> '' then
        System.Delete(keywords, 1, 1);

      if (fn <> '') then
      begin
        fn := folder + fn;
        if FileExists(fn) then   // ignore metadata entries for which the icon files do not exist.
          AddIcon(fn, keywords, style, w, h).Hidden := AHidden;

        // Delete the processed filename from the files list
        i := files.IndexOf(fn);
        if i > -1 then files.Delete(i);
      end;

      iconNode := iconNode.NextSibling;
    end;

    // Every image which exists in the metadata file has been deleted from
    // the files list. The entries which are left identify new files. Add them
    // to the metafile
    for i := 0 to files.Count-1 do
    begin
      fn := files[i];
      stream := TFileStream.Create(files[i], fmOpenRead or fmShareDenyNone);
      try
        reader := TFPCustomImage.FindReaderFromStream(stream);
        if reader <> nil then
        begin
          stream.Position := 0;
          with reader.ImageSize(stream) do
          begin
            w := X;
            h := Y;
          end;
          AddIcon(files[i], '', isAnyStyle, w, h).Hidden := AHidden;
        end;
      finally
        stream.Free;
      end;
    end;

  finally
    doc.Free;
    files.Free;
  end;
end;

procedure TIconThumbnailViewer.SetFilterByIconKeywords(AValue: String);
begin
  if FFilterByIconKeywords <> AValue then
  begin
    FFilterByIconKeywords := AValue;
    if not FilterLocked then
    begin
      FilterIcons;
      Invalidate;
    end;
  end;
end;

procedure TIconThumbnailViewer.SetFilterByIconSize(AValue: string);
var
  sa: TStringArray;
begin
  if FFilterByIconSize <> AValue then
  begin
    FFilterByIconSize := AValue;
    if AValue = '' then
    begin
      FFilterByIconWidth := -1;
      FFilterByIconHeight := -1;
    end else
    begin
      sa := AValue.Split('x');
      FFilterByIconWidth := StrToInt(Trim(sa[0]));
      FFilterByIconHeight := StrToInt(Trim(sa[1]));
    end;
    if not FilterLocked then
    begin
      FilterIcons;
      Invalidate;
    end;
  end;
end;

procedure TIconThumbnailViewer.SetFilterByIconStyle(AValue: TIconStyle);
begin
  if FFilterByIconStyle <> AValue then
  begin
    FFilterByIconStyle := AValue;
    if not FilterLocked then
    begin
      FilterIcons;
      Invalidate;
    end;
  end;
end;

{ Selects, among all visible thumbnails, the icon which has the given filename.}
function TIconThumbnailViewer.SelectIconInFile(AFileName: String): Boolean;
var
  i, idx: Integer;
  folder: String;
begin
  Result := false;
  if AFileName = '' then
  begin
    SelectedIndex := -1;
    exit;
  end;

  // Make sure that folder of file is not hidden
  folder := ExtractFilePath(AFileName);
  if TIconFolderList(FIconFolders).IsHidden(folder) then
  begin
    SelectedIndex := -1;
    exit;
  end;

  // Find the index of the icon with the given filename among all thumbnails.
  idx := -1;
  for i := 0 to ThumbnailCount-1 do
  begin
    if (Thumbnail[i] = nil) or (TIconThumbnail(Thumbnail[i]).Item = nil) then
      Continue;
    if TIconThumbnail(Thumbnail[i]).Item.FileName = AFileName then
    begin
      Result := true;
      idx := i;
      break;
    end;
  end;

  SelectedIndex := idx;
end;

procedure TIconThumbnailViewer.SetSelectedIndex(AValue: Integer);
var
  thumb: TIconThumbnail;
begin
  if AValue = SelectedIndex then
    exit;

  if (AValue > -1) and (AValue < ThumbnailCount) then
  begin
    thumb := Thumbnail[AValue] as TIconThumbnail;
    FSelectedIcon := thumb.Item;
  end else
    FSelectedIcon := nil;

  inherited;
end;

{ This is for emphasizing icons in the viewer which do not yet have keywords
  or have an unspecified style. }
function TIconThumbnailViewer.ThumbnailMarked(AThumbnail: TBasicThumbnail): Boolean;
var
  item: TIconItem;
begin
  item := TIconThumbnail(AThumbnail).Item;
  Result := (item.KeywordCount = 0) or (item.Style = isAnyStyle);
end;

{ The specified thumbnail is outside the drawing area. In this implementation
  of the icon viewer we release the associated picture to avoid running out
  of memory, in particular in the 32-bit IDE. }
procedure TIconThumbnailViewer.ThumbnailOutside(AThumbnail: TBasicThumbnail);
var
  item: TIconItem;
begin
  item := TIconThumbnail(AThumbnail).Item;
  if item <> nil then item.ReleasePicture;
end;

procedure TIconThumbnailViewer.UnlockFilter;
begin
  dec(FFilterLock);
  if FFilterLock = 0 then
  begin
    FilterIcons;
    Invalidate;
  end;
end;

{ Folders and all their icons can be hidden by setting the Hidden flag of the
  folder record to true.
  This procedure iterates over all icons and sets their Hidden flag when
  their folder is hidden. }
procedure TIconThumbnailViewer.UpdateIconFolders;
var
  i, j: Integer;
  hiddenFolders: TStringList;
  folder: String;
  item: TIconItem;
begin
  hiddenFolders := TStringList.Create;
  try
    // Collect all hidden folders...
    hiddenFolders.Sorted := true;
    for i := 0 to FIconFolders.count-1 do
    begin
      folder := FIconFolders[i].FolderName;
      if FIconFolders[i].Hidden then
        hiddenFolders.Add(AppendPathDelim(folder));
    end;

    // ... find the icons in the hidden folders and set their Hidden flag.
    for i := 0 to FIconList.Count-1 do
    begin
      item := FIconList[i];
      folder := item.Directory;
      item.Hidden := hiddenfolders.Find(folder, j);
    end;

    FilterIcons;
    Invalidate;
  finally
    hiddenFolders.Free;
  end;
end;

{ Copies the names of the stored icon folders to the given list. Hidden folders
  are marked by putting a non-nil value in the Objects of the output list. }
procedure TIconThumbnailViewer.WriteIconFolders(AList: TStrings);
var
  i: Integer;
  folder: String;
  isHidden: Boolean;
begin
  for i := 0 to FIconFolders.Count-1 do
  begin
    folder := FIconFolders[i].FolderName;
    isHidden := TIconFolderList(FIconFolders).IsHidden(i);
    if isHidden then
      AList.AddObject(folder, TObject(PtrUInt(1)))
    else
      AList.Add(folder);
  end;
end;

procedure TIconThumbnailViewer.WriteMetadataFiles;
var
  folder, filename: String;
  doc: TXMLDocument;
  root, iconsNode, iconNode, keywordsNode, keywordNode: TDOMNode;
  i, j, k: Integer;
  item: TIconItem;
begin
  Screen.Cursor := crHourglass;
  try
    Application.ProcessMessages;
    for i := 0 to FIconFolders.Count-1 do
    begin
      folder := AppendPathDelim(FIconFolders[i].FolderName);

      // Write only unmodified metadata
      if not FIconFolders[i].Dirty then
        Continue;

      doc := TXMLDocument.Create;
      try
        root := doc.CreateElement('metadata');
        doc.AppendChild(root);
        iconsNode := doc.CreateElement('icons');
        root.AppendChild(iconsNode);
        for j := 0 to FIconList.Count-1 do
        begin
          item := FIconList[j];
          filename := ExtractFileName(item.FileName);
          if ExtractFilePath(item.FileName) = folder then
          begin
            iconNode := doc.CreateElement('icon');
            iconsNode.AppendChild(iconNode);
            TDOMElement(iconNode).SetAttribute('filename', filename);
            TDOMElement(iconNode).SetAttribute('width', IntToStr(item.Width));
            TDOMElement(iconNode).SetAttribute('height', IntToStr(item.Height));
            if item.Style <> isAnyStyle then
              TDOMElement(iconNode).SetAttribute('style', item.StyleAsString);
            if item.KeywordCount > 0 then
            begin
              keywordsNode := doc.CreateElement('keywords');
              iconNode.AppendChild(keywordsNode);
              for k := 0 to item.KeywordCount-1 do
              begin
                keywordNode := doc.CreateElement('keyword');
                keywordsNode.AppendChild(keywordNode);
                keywordNode.AppendChild(doc.CreateTextNode(item.Keywords[k]));
              end;
            end;
          end;
        end;
        if FileExists(folder + METADATA_FILENAME) then
          RenameFile(folder + METADATA_FILENAME, folder + METADATA_FILENAME + '.bak');

        WriteXMLFile(doc, folder + METADATA_FILENAME);
        FIconFolders[i].Dirty := false;
      finally
        doc.Free;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.

