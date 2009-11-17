{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditMiscClasses.pas, released 2000-04-07.
The Original Code is based on the mwSupportClasses.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Hieke.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditMiscClasses;

{$I synedit.inc}

interface

uses
  {$IFDEF SYN_LAZARUS}
  LCLIntf, LCLType, LCLProc,
  {$ELSE}
  Windows,
  {$ENDIF}
  Classes, Graphics, Controls, SysUtils, Clipbrd,
  SynEditMiscProcs, SynEditTypes, SynEditTextBase, SynEditPointClasses;

type

  { TSynWordBreaker }

  TSynWordBreaker = class
  private
    FIdentChars: TSynIdentChars;
    FWhiteChars: TSynIdentChars;
    FWordBreakChars: TSynIdentChars;
    FWordChars: TSynIdentChars;
    procedure SetIdentChars(const AValue: TSynIdentChars);
    procedure SetWhiteChars(const AValue: TSynIdentChars);
    procedure SetWordBreakChars(const AValue: TSynIdentChars);
  public
    constructor Create;
    procedure Reset;

    // aX is the position between the chars (as in CaretX)
    function IsInWord     (aLine: String; aX: Integer
                           ): Boolean;
    function IsAtWordStart(aLine: String; aX: Integer): Boolean;
    function IsAtWordEnd  (aLine: String; aX: Integer): Boolean;
    function NextWordStart(aLine: String; aX: Integer;
                           aIncludeCurrent: Boolean = False): Integer;
    function NextWordEnd  (aLine: String; aX: Integer;
                           aIncludeCurrent: Boolean = False): Integer;
    function PrevWordStart(aLine: String; aX: Integer;
                           aIncludeCurrent: Boolean = False): Integer;
    function PrevWordEnd  (aLine: String; aX: Integer;
                           aIncludeCurrent: Boolean = False): Integer;

    function NextBoundary (aLine: String; aX: Integer): Integer;

    property IdentChars: TSynIdentChars read FIdentChars write SetIdentChars;
    property WordChars: TSynIdentChars read FWordChars;
    property WordBreakChars: TSynIdentChars read FWordBreakChars write SetWordBreakChars;
    property WhiteChars: TSynIdentChars read FWhiteChars write SetWhiteChars;
  end;

  { TSynEditBase }

  TSynEditBase = class(TCustomControl)
  protected
    FWordBreaker: TSynWordBreaker;
    FBlockSelection: TSynEditSelection;
    FIsUndoing, FIsRedoing: Boolean;
    function GetMarkupMgr: TObject; virtual; abstract;
    function GetLines: TStrings; virtual; abstract;
    function GetCaretObj: TSynEditCaret; virtual; abstract;
    procedure SetLines(Value: TStrings); virtual; abstract;
    function GetViewedTextBuffer: TSynEditStrings; virtual; abstract;
    function GetTextBuffer: TSynEditStrings; virtual; abstract;

    property MarkupMgr: TObject read GetMarkupMgr;
    property ViewedTextBuffer: TSynEditStrings read GetViewedTextBuffer;        // As viewed internally (with uncommited spaces / TODO: expanded tabs, folds). This may change, use with care
    property TextBuffer: TSynEditStrings read GetTextBuffer;                    // No uncommited (trailing/trimmable) spaces
    property WordBreaker: TSynWordBreaker read FWordBreaker;
  public
    property Lines: TStrings read GetLines write SetLines;
  end;

  { TSynEditFriend }

  TSynEditFriend = class(TComponent)
  private
    FFriendEdit: TSynEditBase;
    function GetCaretObj: TSynEditCaret;
    function GetIsRedoing: Boolean;
    function GetIsUndoing: Boolean;
    function GetMarkupMgr: TObject;
    function GetSelectionObj: TSynEditSelection;
    function GetViewedTextBuffer: TSynEditStrings;
    function GetWordBreaker: TSynWordBreaker;
  protected
    property FriendEdit: TSynEditBase read FFriendEdit write FFriendEdit;
    property ViewedTextBuffer: TSynEditStrings read GetViewedTextBuffer;        // As viewed internally (with uncommited spaces / TODO: expanded tabs, folds). This may change, use with care
    property CaretObj: TSynEditCaret read GetCaretObj;
    property SelectionObj: TSynEditSelection read GetSelectionObj;
    property MarkupMgr: TObject read GetMarkupMgr;
    property IsUndoing: Boolean read GetIsUndoing;
    property IsRedoing: Boolean read GetIsRedoing;
    property WordBreaker: TSynWordBreaker read GetWordBreaker;
  end;


  TSynObjectListItem = class;

  { TSynObjectList }

  TSynObjectList = class(TComponent)
  private
    FList: TList;
    FOnChange: TNotifyEvent;
    FOwner: TComponent;
    function GetBasePart(Index: Integer): TSynObjectListItem;
    procedure PutBasePart(Index: Integer; const AValue: TSynObjectListItem);
  protected
    function GetChildOwner: TComponent; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure RegisterItem(AnItem: TSynObjectListItem); virtual;
    procedure DoChange(Sender: TObject); virtual;
    property List: TList read FList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Assign(Source: TPersistent); override;
    Function  Add(AnItem: TSynObjectListItem): Integer;
    Procedure Delete(Index: Integer);
    Procedure Clear;
    Function  Count: Integer;
    Function  IndexOf(AnItem: TSynObjectListItem): Integer;
    Procedure Move(AOld, ANew: Integer);
    property Owner: TComponent read FOwner;
    property BaseItems[Index: Integer]: TSynObjectListItem
      read GetBasePart write PutBasePart; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TSynObjectListItem }

  TSynObjectListItem = class(TComponent)
  private
    FOwner: TSynObjectList;
    function GetIndex: Integer;
    procedure SetIndex(const AValue: Integer);
  protected
    function GetDisplayName: String; virtual;
    property Owner: TSynObjectList read FOwner;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property Index: Integer read GetIndex write SetIndex;
    property DisplayName: String read GetDisplayName;
    function GetParentComponent: TComponent; override; // for child order in stream reading
  end;

  TSynObjectListItemClass = class of TSynObjectListItem;

  { TSynSelectedColor }

  TSynSelectedColor = class(TPersistent)
  private
    FBG: TColor;
    FFG: TColor;
    FFrameColor: TColor;
    FStyle: TFontStyles;
    // StyleMask = 1 => Copy Style Bits
    // StyleMask = 0 => Invert where Style Bit = 1
    FStyleMask: TFontStyles;
    FOnChange: TNotifyEvent;
    // 0 or -1 start/end before/after line // 1 first char
    FStartX, FEndX: Integer;
    procedure SetBG(Value: TColor);
    procedure SetFG(Value: TColor);
    procedure SetFrameColor(const AValue: TColor);
    procedure SetStyle(const AValue : TFontStyles);
    procedure SetStyleMask(const AValue : TFontStyles);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(aSource: TPersistent); override;
    procedure Clear;
    function IsEnabled: boolean;
    function GetModifiedStyle(aStyle : TFontStyles): TFontStyles;
    procedure ModifyColors(var AForeground, ABackground, AFrameColor: TColor; var AStyle: TFontStyles);
    property StartX: Integer read FStartX write FStartX;
    property EndX: Integer read FEndX write FEndX;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  published
    property Background: TColor read FBG write SetBG default clHighLight;
    property Foreground: TColor read FFG write SetFG default clHighLightText;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clNone;
    property Style: TFontStyles read FStyle write SetStyle default [];
    property StyleMask: TFontStyles read fStyleMask write SetStyleMask default [];
  end;

  { TSynBookMarkOpt }

  TSynBookMarkOpt = class(TPersistent)
  private
    fBookmarkImages: TImageList;
    fDrawBookmarksFirst: boolean;                                               //mh 2000-10-12
    fEnableKeys: Boolean;
    fGlyphsVisible: Boolean;
    fLeftMargin: Integer;
    fOwner: TComponent;
    fXoffset: integer;
    fOnChange: TNotifyEvent;
    procedure SetBookmarkImages(const Value: TImageList);
    procedure SetDrawBookmarksFirst(Value: boolean);                            //mh 2000-10-12
    procedure SetGlyphsVisible(Value: Boolean);
    procedure SetLeftMargin(Value: Integer);
    procedure SetXOffset(Value: integer);
  public
    constructor Create(AOwner: TComponent);
  published
    property BookmarkImages: TImageList
      read fBookmarkImages write SetBookmarkImages;
    property DrawBookmarksFirst: boolean read fDrawBookmarksFirst               //mh 2000-10-12
      write SetDrawBookmarksFirst default True;
    property EnableKeys: Boolean
      read fEnableKeys write fEnableKeys default True;
    property GlyphsVisible: Boolean
      read fGlyphsVisible write SetGlyphsVisible default True;
    property LeftMargin: Integer read fLeftMargin write SetLeftMargin default 2;
    property Xoffset: integer read fXoffset write SetXOffset default 12;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  { TSynInternalImage }

  TSynInternalImage = class(TObject)
  public
    constructor Create(const AName: string; Count: integer);
    destructor Destroy; override;
    procedure DrawMark(ACanvas: TCanvas; Number, X, Y, LineHeight: integer);
    {$IFNDEF SYN_LAZARUS}
    procedure DrawMarkTransparent(ACanvas: TCanvas; Number, X, Y,
      LineHeight: integer; TransparentColor: TColor);
    {$ENDIF}
  end;


  { TSynEditSearchCustom }

  TSynEditSearchCustom = class(TComponent)
  protected
    function GetPattern: string; virtual; abstract;
    procedure SetPattern(const Value: string); virtual; abstract;
    function GetLength(aIndex: integer): integer; virtual; abstract;
    function GetResult(aIndex: integer): integer; virtual; abstract;
    function GetResultCount: integer; virtual; abstract;
    procedure SetOptions(const Value: TSynSearchOptions); virtual; abstract;
  public
    function FindAll(const NewText: string): integer; virtual; abstract;
    property Pattern: string read GetPattern write SetPattern;
    property ResultCount: integer read GetResultCount;
    property Results[aIndex: integer]: integer read GetResult;
    property Lengths[aIndex: integer]: integer read GetLength;
    property Options: TSynSearchOptions write SetOptions;
  end;

  TSynClipboardStreamTag = type word;

  { TSynClipboardStream }

  TSynClipboardStream = class
  private
    FMemStream: TMemoryStream;
    FText: String;
    FTextP: PChar;
    FIsPlainText: Boolean;

    function GetMemory: Pointer;
    function GetSize: LongInt;
    function GetSelectionMode: TSynSelectionMode;
    procedure SetSelectionMode(const AValue: TSynSelectionMode);
    procedure SetInternalText(const AValue: String);
    procedure SetText(const AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    class function ClipboardFormatId: TClipboardFormat;

    function CanReadFromClipboard(AClipboard: TClipboard): Boolean;
    function ReadFromClipboard(AClipboard: TClipboard): Boolean;
    function WriteToClipboard(AClipboard: TClipboard): Boolean;

    procedure Clear;

    function HasTag(ATag: TSynClipboardStreamTag): Boolean;
    function GetTagPointer(ATag: TSynClipboardStreamTag): Pointer;
    function GetTagLen(ATag: TSynClipboardStreamTag): Integer;
    // No check for duplicates
    Procedure AddTag(ATag: TSynClipboardStreamTag; Location: Pointer; Len: Integer);
    property IsPlainText: Boolean read FIsPlainText;

    // Currently Each method (or each method of a pair) must be assigned only ONCE
    property TextP: PChar read FTextP;
    property Text: String write SetText;
    property InternalText: String write SetInternalText;

    property SelectionMode: TSynSelectionMode read GetSelectionMode write SetSelectionMode;

    property Memory: Pointer read GetMemory;
    property Size: LongInt read GetSize;
  end;

const
  synClipTagText = TSynClipboardStreamTag(1);
  synClipTagExtText = TSynClipboardStreamTag(2);
  synClipTagMode = TSynClipboardStreamTag(3);
  synClipTagFold = TSynClipboardStreamTag(4);


implementation

{ TSynEditFriend }

function TSynEditFriend.GetViewedTextBuffer: TSynEditStrings;
begin
  Result := FFriendEdit.ViewedTextBuffer;
end;

function TSynEditFriend.GetWordBreaker: TSynWordBreaker;
begin
  Result := FFriendEdit.WordBreaker;
end;

function TSynEditFriend.GetMarkupMgr: TObject;
begin
  Result := FFriendEdit.MarkupMgr;
end;

function TSynEditFriend.GetSelectionObj: TSynEditSelection;
begin
  Result := FFriendEdit.FBlockSelection;
end;

function TSynEditFriend.GetIsRedoing: Boolean;
begin
  Result := FFriendEdit.FIsRedoing;
end;

function TSynEditFriend.GetCaretObj: TSynEditCaret;
begin
  Result := FFriendEdit.GetCaretObj;
end;

function TSynEditFriend.GetIsUndoing: Boolean;
begin
  Result := FFriendEdit.FIsUndoing;
end;

{ TSynSelectedColor }

constructor TSynSelectedColor.Create;
begin
  inherited Create;
  fBG := clHighLight;
  fFG := clHighLightText;
  FFrameColor:= clNone;
end;

function TSynSelectedColor.GetModifiedStyle(aStyle : TFontStyles) : TFontStyles;
begin
  Result := fsXor(aStyle, FStyle * fsNot(FStyleMask)) // Invert Styles
            + (FStyle*FStyleMask)                     // Set Styles
            - (fsNot(FStyle)*FStyleMask);             // Remove Styles
end;

procedure TSynSelectedColor.ModifyColors(var AForeground, ABackground, AFrameColor: TColor; var AStyle: TFontStyles);
begin
  if Foreground <> clNone then AForeground := Foreground;
  if Background <> clNone then ABackground := Background;
  if FrameColor <> clNone then AFrameColor := FrameColor;
  AStyle := GetModifiedStyle(AStyle);
end;

procedure TSynSelectedColor.SetBG(Value: TColor);
begin
  if (FBG <> Value) then
  begin
    FBG := Value;
    DoChange;
  end;
end;

procedure TSynSelectedColor.SetFG(Value: TColor);
begin
  if (FFG <> Value) then
  begin
    FFG := Value;
    DoChange;
  end;
end;

procedure TSynSelectedColor.SetFrameColor(const AValue: TColor);
begin
  if FFrameColor <> AValue then
  begin
    FFrameColor := AValue;
    DoChange;
  end;
end;

procedure TSynSelectedColor.SetStyle(const AValue : TFontStyles);
begin
  if (FStyle <> AValue) then
  begin
    FStyle := AValue;
    DoChange;
  end;
end;

procedure TSynSelectedColor.SetStyleMask(const AValue : TFontStyles);
begin
  if (FStyleMask <> AValue) then
  begin
    FStyleMask := AValue;
    DoChange;
  end;
end;

procedure TSynSelectedColor.DoChange;
begin
  if Assigned(FOnChange) then
    OnChange(Self);
end;

procedure TSynSelectedColor.Assign(aSource : TPersistent);
var
  Source : TSynSelectedColor;
begin
  if Assigned(aSource) and (aSource is TSynSelectedColor) then
  begin
    Source := TSynSelectedColor(aSource);
    FBG := Source.FBG;
    FFG := Source.FFG;
    FFrameColor := Source.FFrameColor;
    FStyle := Source.FStyle;
    FStyleMask := Source.FStyleMask;
    FStartX := Source.FStartX;
    FEndX   := Source.FEndX;
    DoChange; {TODO: only if really changed}
  end;
end;

procedure TSynSelectedColor.Clear;
begin
  FBG := clNone;
  FFG := clNone;
  FFrameColor := clNone;
  FStyle := [];
  FStyleMask := [];
  FStartX := -1;
  FEndX := -1;
end;

function TSynSelectedColor.IsEnabled: boolean;
begin
  Result := (FBG <> clNone) or (FFG <> clNone) or (FFrameColor <> clNone) or
            (FStyle <> []) or (FStyleMask <> []);
end;

{ TSynBookMarkOpt }

constructor TSynBookMarkOpt.Create(AOwner: TComponent);
begin
  inherited Create;
  fDrawBookmarksFirst := TRUE;                                                  //mh 2000-10-12
  fEnableKeys := True;
  fGlyphsVisible := True;
  fLeftMargin := 2;
  fOwner := AOwner;
  fXOffset := 12;
end;

procedure TSynBookMarkOpt.SetBookmarkImages(const Value: TImageList);
begin
  if fBookmarkImages <> Value then begin
    fBookmarkImages := Value;
    if Assigned(fBookmarkImages) then fBookmarkImages.FreeNotification(fOwner);
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

{begin}                                                                         //mh 2000-10-12
procedure TSynBookMarkOpt.SetDrawBookmarksFirst(Value: boolean);
begin
  if Value <> fDrawBookmarksFirst then begin
    fDrawBookmarksFirst := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;
{end}                                                                           //mh 2000-10-12

procedure TSynBookMarkOpt.SetGlyphsVisible(Value: Boolean);
begin
  if fGlyphsVisible <> Value then begin
    fGlyphsVisible := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynBookMarkOpt.SetLeftMargin(Value: Integer);
begin
  if fLeftMargin <> Value then begin
    fLeftMargin := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynBookMarkOpt.SetXOffset(Value: integer);
begin
  if fXOffset <> Value then begin
    fXOffset := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

var
  InternalImages: TBitmap;
  InternalImagesUsers: integer;
  IIWidth, IIHeight: integer;
  IICount: integer;

constructor TSynInternalImage.Create(const AName: string; Count: integer);
begin
  inherited Create;
  Inc(InternalImagesUsers);
  if InternalImagesUsers = 1 then begin
    InternalImages := TBitmap.Create;
    InternalImages.LoadFromResourceName(HInstance, AName);
    IIWidth := (InternalImages.Width + Count shr 1) div Count;
    IIHeight := InternalImages.Height;
    IICount := Count;
  end;
end;

destructor TSynInternalImage.Destroy;
begin
  Dec(InternalImagesUsers);
  if InternalImagesUsers = 0 then begin
    InternalImages.Free;
    InternalImages := nil;
  end;
  inherited Destroy;
end;

procedure TSynInternalImage.DrawMark(ACanvas: TCanvas;
  Number, X, Y, LineHeight: integer);
var
  rcSrc, rcDest: TRect;
begin
  if (Number >= 0) and (Number < IICount) then
  begin
    if LineHeight >= IIHeight then begin
      rcSrc := Rect(Number * IIWidth, 0, (Number + 1) * IIWidth, IIHeight);
      Inc(Y, (LineHeight - IIHeight) div 2);
      rcDest := Rect(X, Y, X + IIWidth, Y + IIHeight);
    end else begin
      rcDest := Rect(X, Y, X + IIWidth, Y + LineHeight);
      Y := (IIHeight - LineHeight) div 2;
      rcSrc := Rect(Number * IIWidth, Y, (Number + 1) * IIWidth, Y + LineHeight);
    end;
    ACanvas.CopyRect(rcDest, InternalImages.Canvas, rcSrc);
  end;
end;

{$IFNDEF SYN_LAZARUS}
procedure TSynInternalImage.DrawMarkTransparent(ACanvas: TCanvas; Number, X, Y,
  LineHeight: integer; TransparentColor: TColor);
var
  rcSrc, rcDest: TRect;
begin
  if (Number >= 0) and (Number < IICount) then
  begin
    if LineHeight >= IIHeight then begin
      rcSrc := Rect(Number * IIWidth, 0, (Number + 1) * IIWidth, IIHeight);
      Inc(Y, (LineHeight - IIHeight) div 2);
      rcDest := Rect(X, Y, X + IIWidth, Y + IIHeight);
    end else begin
      rcDest := Rect(X, Y, X + IIWidth, Y + LineHeight);
      Y := (IIHeight - LineHeight) div 2;
      rcSrc := Rect(Number * IIWidth, Y, (Number + 1) * IIWidth, Y + LineHeight);
    end;
    ACanvas.BrushCopy(rcDest, InternalImages, rcSrc, TransparentColor);
  end;
end;
{$ENDIF}

{ TSynObjectList }

constructor TSynObjectList.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  SetAncestor(True);
  SetInline(True);
  FList := TList.Create;
  FOwner := AOwner;
end;

destructor TSynObjectList.Destroy;
begin
  inherited Destroy;
  Clear;
  FreeAndNil(FList);
end;

procedure TSynObjectList.Assign(Source: TPersistent);
begin
  FList.Assign(TSynObjectList(Source).FList);
  DoChange(self);
end;

function TSynObjectList.GetChildOwner: TComponent;
begin
  Result := self;
end;

procedure TSynObjectList.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  if Root = self then
    for i:= 0 to Count -1 do
      Proc(BaseItems[i]);
end;

procedure TSynObjectList.SetChildOrder(Child: TComponent; Order: Integer);
begin
  (Child as TSynObjectListItem).Index := Order;
  DoChange(self);;
end;

procedure TSynObjectList.RegisterItem(AnItem: TSynObjectListItem);
begin
  Add(AnItem);
end;

function TSynObjectList.GetBasePart(Index: Integer): TSynObjectListItem;
begin
  Result := TSynObjectListItem(FList[Index]);
end;

procedure TSynObjectList.PutBasePart(Index: Integer; const AValue: TSynObjectListItem);
begin
  FList[Index] := Pointer(AValue);
  DoChange(self);
end;

procedure TSynObjectList.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TSynObjectList.Add(AnItem: TSynObjectListItem): Integer;
begin
  Result := FList.Add(Pointer(AnItem));
  DoChange(self);
end;

procedure TSynObjectList.Delete(Index: Integer);
begin
  FList.Delete(Index);
  DoChange(self);
end;

procedure TSynObjectList.Clear;
begin
  while FList.Count > 0 do
    BaseItems[0].Free;
  FList.Clear;
  DoChange(self);
end;

function TSynObjectList.Count: Integer;
begin
  Result := FList.Count;
end;

function TSynObjectList.IndexOf(AnItem: TSynObjectListItem): Integer;
begin
  Result := Flist.IndexOf(Pointer(AnItem));
end;

procedure TSynObjectList.Move(AOld, ANew: Integer);
begin
  FList.Move(AOld, ANew);
  DoChange(self);;
end;

{ TSynObjectListItem }

function TSynObjectListItem.GetIndex: Integer;
begin
  Result := Owner.IndexOf(self);
end;

function TSynObjectListItem.GetDisplayName: String;
begin
  Result := Name + ' (' + ClassName + ')';
end;

procedure TSynObjectListItem.SetIndex(const AValue: Integer);
begin
  Owner.Move(GetIndex, AValue);
end;

constructor TSynObjectListItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetAncestor(True);
  FOwner := AOwner as TSynObjectList;
  FOwner.RegisterItem(self);
end;

destructor TSynObjectListItem.Destroy;
begin
  inherited Destroy;
  FOwner.Delete(FOwner.IndexOf(self));
end;

function TSynObjectListItem.GetParentComponent: TComponent;
begin
  Result := FOwner;
end;

{ TSynClipboardStream }

function TSynClipboardStream.GetMemory: Pointer;
begin
  Result := FMemStream.Memory;
end;

function TSynClipboardStream.GetSize: LongInt;
begin
  Result := FMemStream.Size;
end;

procedure TSynClipboardStream.SetInternalText(const AValue: String);
begin
  FIsPlainText := False;
  // Text, if we don't need CF_TEXT // Must include a zero byte
  AddTag(synClipTagText, @AValue[1], length(AValue) + 1);
end;

function TSynClipboardStream.GetSelectionMode: TSynSelectionMode;
var
  PasteMode: ^TSynSelectionMode;
begin
  PasteMode := GetTagPointer(synClipTagMode);
  if PasteMode = nil then
    Result := smNormal
  else
    Result := PasteMode^;
end;

procedure TSynClipboardStream.SetSelectionMode(const AValue: TSynSelectionMode);
begin
  AddTag(synClipTagMode, @AValue, SizeOf(TSynSelectionMode));
end;

procedure TSynClipboardStream.SetText(const AValue: String);
var
  SLen: Integer;
begin
  FIsPlainText := True;
  FText := AValue;
  SLen := length(FText);
  AddTag(synClipTagExtText, @SLen, SizeOf(Integer));
end;

constructor TSynClipboardStream.Create;
begin
  FMemStream := TMemoryStream.Create;
end;

destructor TSynClipboardStream.Destroy;
begin
  FreeAndNil(FMemStream);
  inherited Destroy;
end;

class function TSynClipboardStream.ClipboardFormatId: TClipboardFormat;
const
  SYNEDIT_CLIPBOARD_FORMAT_TAGGED = 'Application/X-Laz-SynEdit-Tagged';
  Format: UINT = 0;
begin
  if Format = 0 then
    Format := ClipboardRegisterFormat(SYNEDIT_CLIPBOARD_FORMAT_TAGGED);
  Result := Format;
end;

function TSynClipboardStream.CanReadFromClipboard(AClipboard: TClipboard): Boolean;
begin
  Result := AClipboard.HasFormat(ClipboardFormatId);
end;

function TSynClipboardStream.ReadFromClipboard(AClipboard: TClipboard): Boolean;
var
  ip: PInteger;
  len: LongInt;
begin
  Clear;
  Result := AClipboard.GetFormat(ClipboardFormatId, FMemStream);
  // Check for embedded text
  FTextP := GetTagPointer(synClipTagText);
  if FTextP <> nil then begin
    len := GetTagLen(synClipTagText);
    if len > 0 then
      (FTextP + len - 1)^ := #0
    else
      FTextP := nil;
  end;
  // Normal text
  if (FTextP = nil) and AClipboard.HasFormat(CF_TEXT) then begin
    FText:= AClipboard.AsText;
    if FText <> '' then begin
      FTextP := @FText[1];
      ip := GetTagPointer(synClipTagExtText);
      if (length(FText) = 0) or (ip = nil) or (length(FText) <> ip^) then
        FIsPlainText := True;
    end;
  end;
end;

function TSynClipboardStream.WriteToClipboard(AClipboard: TClipboard): Boolean;
begin
  if FIsPlainText and (FText <> '') then begin
    Clipboard.AsText:= FText;
    if not Clipboard.HasFormat(CF_TEXT) then
      raise ESynEditError.Create('Clipboard copy operation failed: HasFormat');
  end;
  Result := AClipboard.AddFormat(ClipboardFormatId, FMemStream.Memory^, FMemStream.Size);
end;

procedure TSynClipboardStream.Clear;
begin
  FMemStream.Clear;
  FIsPlainText := False;
end;

function TSynClipboardStream.HasTag(ATag: TSynClipboardStreamTag): Boolean;
begin
  Result := GetTagPointer(ATag) <> nil;
end;

function TSynClipboardStream.GetTagPointer(ATag: TSynClipboardStreamTag): Pointer;
var
  ctag, mend: Pointer;
begin
  Result :=  nil;
  if FIsPlainText then
    exit;
  ctag := FMemStream.Memory;
  mend := ctag + FMemStream.Size;
  while (result = nil) and
        (ctag + SizeOf(TSynClipboardStreamTag) + SizeOf(Integer) <= mend) do
  begin
     if TSynClipboardStreamTag(ctag^) = ATag then begin
      Result := ctag + SizeOf(TSynClipboardStreamTag) + SizeOf(Integer)
    end else begin
      inc(ctag, SizeOf(TSynClipboardStreamTag));
      inc(ctag, PInteger(ctag)^);
      inc(ctag, SizeOf(Integer));
    end;
  end;
  if (Result <> nil) and
     (ctag + Integer((ctag + SizeOf(TSynClipboardStreamTag))^) > mend) then
  begin
    Result := nil;
    raise ESynEditError.Create('Clipboard read operation failed, data corrupt');
  end;
end;

function TSynClipboardStream.GetTagLen(ATag: TSynClipboardStreamTag): Integer;
var
  p: PInteger;
begin
  Result := 0;
  p := GetTagPointer(ATag);
  if p = nil then
    exit;
  dec(p, 1);
  Result := p^;
end;

procedure TSynClipboardStream.AddTag(ATag: TSynClipboardStreamTag; Location: Pointer;
  Len: Integer);
var
  msize: Int64;
  mpos: Pointer;
begin
  msize := FMemStream.Size;
  FMemStream.Size := msize + Len + SizeOf(TSynClipboardStreamTag) + SizeOf(Integer);
  mpos := FMemStream.Memory + msize;
  TSynClipboardStreamTag(mpos^) := ATag;
  inc(mpos, SizeOf(TSynClipboardStreamTag));
  Integer(mpos^) := Len;
  inc(mpos, SizeOf(Integer));
  System.Move(Location^, mpos^, Len);
end;

{ TSynWordBreaker }

procedure TSynWordBreaker.SetIdentChars(const AValue: TSynIdentChars);
begin
  if FIdentChars = AValue then exit;
  FIdentChars := AValue;
end;

procedure TSynWordBreaker.SetWhiteChars(const AValue: TSynIdentChars);
begin
  if FWhiteChars = AValue then exit;
  FWhiteChars := AValue;
  FWordChars := [#1..#255] - (FWordBreakChars + FWhiteChars);
end;

procedure TSynWordBreaker.SetWordBreakChars(const AValue: TSynIdentChars);
begin
  if FWordBreakChars = AValue then exit;
  FWordBreakChars := AValue;
  FWordChars := [#1..#255] - (FWordBreakChars + FWhiteChars);
end;

constructor TSynWordBreaker.Create;
begin
  inherited;
  Reset;
end;

procedure TSynWordBreaker.Reset;
begin
  FWhiteChars     := TSynWhiteChars;
  FWordBreakChars := TSynWordBreakChars;
  FIdentChars     := TSynValidStringChars - TSynSpecialChars;
  FWordChars      := [#1..#255] - (FWordBreakChars + FWhiteChars);
end;

function TSynWordBreaker.IsInWord(aLine: String; aX: Integer): Boolean;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX < 1) or (aX > len + 1) then exit(False);
  Result := ((ax <= len) and (aLine[aX] in FWordChars)) or
            ((aX > 1) and (aLine[aX - 1] in FWordChars));
end;

function TSynWordBreaker.IsAtWordStart(aLine: String; aX: Integer): Boolean;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX < 1) or (aX > len) then exit(False);
  Result := (aLine[aX] in FWordChars) and
            ((aX = 1) or not (aLine[aX - 1] in FWordChars));
end;

function TSynWordBreaker.IsAtWordEnd(aLine: String; aX: Integer): Boolean;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX < 1) or (aX > len + 1) then exit(False);
  Result := ((ax = len + 1) or not(aLine[aX] in FWordChars)) and
            (aLine[aX - 1] in FWordChars);
end;

function TSynWordBreaker.NextWordStart(aLine: String; aX: Integer;
  aIncludeCurrent: Boolean): Integer;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX < 1) or (aX > len + 1) then exit(-1);
  if not aIncludeCurrent then
    inc(aX);
  if (aX > 1) and (aLine[aX - 1] in FWordChars) then
    while (aX <= len) and (aLine[aX] in FWordChars) do Inc(ax);
  while (aX <= len) and not(aLine[aX] in FWordChars) do Inc(ax);
  if aX > len then
    exit(-1);
  Result := aX;
end;

function TSynWordBreaker.NextWordEnd(aLine: String; aX: Integer;
  aIncludeCurrent: Boolean): Integer;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX < 1) or (aX > len + 1) then exit(-1);
  if not aIncludeCurrent then
    inc(aX);
  if (aX = 1) or not(aLine[aX - 1] in FWordChars) then
    while (aX <= len) and not(aLine[aX] in FWordChars) do Inc(ax);
  while (aX <= len) and (aLine[aX] in FWordChars) do Inc(ax);
  Result := aX;
end;

function TSynWordBreaker.PrevWordStart(aLine: String; aX: Integer;
  aIncludeCurrent: Boolean): Integer;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX < 1) or (aX > len + 1) then exit(-1);
  if ax > len then aX := len;
  if not aIncludeCurrent then
    dec(aX);
  while (aX >= 1) and not(aLine[aX] in FWordChars) do Dec(ax);
  if aX = 0 then
    exit(-1);
  while (aX >= 1) and (aLine[aX] in FWordChars) do Dec(ax);
  Result := aX  + 1;
end;

function TSynWordBreaker.PrevWordEnd(aLine: String; aX: Integer;
  aIncludeCurrent: Boolean): Integer;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX < 1) or (aX > len + 1) then exit(-1);
  if ax > len then aX := len;
  if not aIncludeCurrent then
    dec(aX);
  while (aX >= 1) and (aLine[aX] in FWordChars) do Dec(ax);
  while (aX >= 1) and not(aLine[aX] in FWordChars) do Dec(ax);
  if aX = 0 then
    exit(-1);
  Result := aX + 1;
end;

function TSynWordBreaker.NextBoundary(aLine: String; aX: Integer): Integer;
var
  len: Integer;
begin
  len := Length(aLine);
  if (aX < 1) or (ax > len) then exit(-1);

  if (aLine[aX] in FWordChars) then
    while (aX <= len) and (aLine[aX] in FWordChars) do Inc(ax)
  else
  if (aLine[aX] in FWordBreakChars) then
    while (aX <= len) and (aLine[aX] in FWordBreakChars) do Inc(ax)
  else
    while (aX <= len) and (aLine[aX] in FWhiteChars) do Inc(ax);
  Result := aX;
end;

end.

