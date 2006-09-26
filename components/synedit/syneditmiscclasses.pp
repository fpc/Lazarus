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
  LCLIntf, LCLType,
  {$ELSE}
  Windows,
  {$ENDIF}
  Classes, Graphics, Controls, SysUtils, SynEditTypes;

type

  { TSynSelectedColor }

  TSynSelectedColor = class(TPersistent)
  private
    fBG: TColor;
    fFG: TColor;
    fOnChange: TNotifyEvent;
    procedure SetBG(Value: TColor);
    procedure SetFG(Value: TColor);
  public
    constructor Create;
  published
    property Background: TColor read fBG write SetBG default clHighLight;
    property Foreground: TColor read fFG write SetFG default clHighLightText;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  { TSynGutter }

  TSynGutter = class(TPersistent)
  private
    {$IFDEF SYN_LAZARUS}
    FCodeFoldingWidth: integer;
    {$ENDIF}
    fColor: TColor;
    fWidth: integer;
    fShowLineNumbers: boolean;
    fShowCodeFolding: boolean;
    fDigitCount: integer;
    fLeadingZeros: boolean;
    fZeroStart: boolean;
    fLeftOffset: integer;
    fRightOffset: integer;
    fOnChange: TNotifyEvent;
    fCursor: TCursor;
    fVisible: boolean;
    fUseFontStyle: boolean;
    fAutoSize: boolean;
    fAutoSizeDigitCount: integer;
    procedure SetAutoSize(const Value: boolean);
    {$IFDEF SYN_LAZARUS}
    procedure SetCodeFoldingWidth(const AValue: integer);
    {$ENDIF}
    procedure SetColor(const Value: TColor);
    procedure SetDigitCount(Value: integer);
    procedure SetLeadingZeros(const Value: boolean);
    procedure SetLeftOffset(Value: integer);
    procedure SetRightOffset(Value: integer);
    procedure SetShowLineNumbers(const Value: boolean);
    procedure SetShowCodeFolding(const Value: boolean);
    procedure SetUseFontStyle(Value: boolean);
    procedure SetVisible(Value: boolean);
    procedure SetWidth(Value: integer);
    procedure SetZeroStart(const Value: boolean);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure AutoSizeDigitCount(LinesCount: integer);
    function FormatLineNumber(Line: integer): string;
    function RealGutterWidth(CharWidth: integer): integer;
  published
    property AutoSize: boolean read fAutoSize write SetAutoSize default FALSE;
    property Color: TColor read fColor write SetColor default clBtnFace;
    property Cursor: TCursor read fCursor write fCursor default crDefault;
    property DigitCount: integer read fDigitCount write SetDigitCount
      default 4;
    property LeadingZeros: boolean read fLeadingZeros write SetLeadingZeros
      default FALSE;
    property LeftOffset: integer read fLeftOffset write SetLeftOffset
      default 16;
    property RightOffset: integer read fRightOffset write SetRightOffset
      default 2;
    property ShowLineNumbers: boolean read fShowLineNumbers
      write SetShowLineNumbers default FALSE;
    property ShowCodeFolding: boolean read fShowCodeFolding
      write SetShowCodeFolding default FALSE;
    property UseFontStyle: boolean read fUseFontStyle write SetUseFontStyle
      default FALSE;
    property Visible: boolean read fVisible write SetVisible default TRUE;
    property Width: integer read fWidth write SetWidth default 30;
    property ZeroStart: boolean read fZeroStart write SetZeroStart default FALSE;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    {$IFDEF SYN_LAZARUS}
    property CodeFoldingWidth: integer read FCodeFoldingWidth write SetCodeFoldingWidth;
    {$ENDIF}
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

  { TSynMethodChain }

  ESynMethodChain = class(Exception);
  TSynExceptionEvent = procedure (Sender: TObject; E: Exception;
    var DoContinue: Boolean) of object;

  TSynMethodChain = class
  private
    FNotifyProcs: TList;
    FExceptionHandler: TSynExceptionEvent;
  protected
    procedure DoFire(AEvent: TMethod); virtual; abstract;
    function DoHandleException(E: Exception): Boolean; virtual;
    property ExceptionHandler: TSynExceptionEvent read FExceptionHandler
      write FExceptionHandler;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AEvent: TMethod);
    procedure Remove(AEvent: TMethod);
    procedure Fire;
  end;

  { TSynNotifyEventChain }

  TSynNotifyEventChain = class(TSynMethodChain)
  private
    FSender: TObject;
  protected
    procedure DoFire(AEvent: TMethod); override;
  public
    constructor CreateEx(ASender: TObject);
    procedure Add(AEvent: TNotifyEvent);
    procedure Remove(AEvent: TNotifyEvent);
    property ExceptionHandler;
    property Sender: TObject read FSender write FSender;
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


implementation

uses
  SynEditMiscProcs;

{ TSynSelectedColor }

constructor TSynSelectedColor.Create;
begin
  inherited Create;
  fBG := clHighLight;
  fFG := clHighLightText;
end;

procedure TSynSelectedColor.SetBG(Value: TColor);
begin
  if (fBG <> Value) then begin
    fBG := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynSelectedColor.SetFG(Value: TColor);
begin
  if (fFG <> Value) then begin
    fFG := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

{ TSynGutter }

constructor TSynGutter.Create;
begin
  inherited Create;
  fColor := clBtnFace;
  fVisible := TRUE;
  fWidth := 30;
  fLeftOffset := 16;
  fDigitCount := 4;
  fAutoSizeDigitCount := fDigitCount;
  fRightOffset := 2;
  CodeFoldingWidth := 14;
end;

procedure TSynGutter.Assign(Source: TPersistent);
var
  Src: TSynGutter;
begin
  if Assigned(Source) and (Source is TSynGutter) then begin
    Src := TSynGutter(Source);
    fColor := Src.fColor;
    fVisible := Src.fVisible;
    fWidth := Src.fWidth;
    fShowLineNumbers := Src.fShowLineNumbers;
    fShowCodeFolding := Src.fShowCodeFolding;
    fLeadingZeros := Src.fLeadingZeros;
    fZeroStart := Src.fZeroStart;
    fLeftOffset := Src.fLeftOffset;
    fDigitCount := Src.fDigitCount;
    fRightOffset := Src.fRightOffset;
    fAutoSize := Src.fAutoSize;
    fAutoSizeDigitCount := Src.fAutoSizeDigitCount;
    if Assigned(fOnChange) then fOnChange(Self);
  end else
    inherited;
end;

procedure TSynGutter.AutoSizeDigitCount(LinesCount: integer);
var
  nDigits: integer;
begin
  if fVisible and fAutoSize and fShowLineNumbers then begin            
    if fZeroStart then Dec(LinesCount);
    nDigits := Max(Length(IntToStr(LinesCount)), fDigitCount);
    if fAutoSizeDigitCount <> nDigits then begin
      fAutoSizeDigitCount := nDigits;
      if Assigned(fOnChange) then fOnChange(Self);
    end;
  end else
    fAutoSizeDigitCount := fDigitCount;
end;

function TSynGutter.FormatLineNumber(Line: integer): string;
var
  i: integer;
begin
  Result := '';
  if fZeroStart then Dec(Line);
  Str(Line : fAutoSizeDigitCount, Result);
  if fLeadingZeros then
    for i := 1 to fAutoSizeDigitCount - 1 do begin
      if (Result[i] <> ' ') then break;
      Result[i] := '0';
    end;
end;

function TSynGutter.RealGutterWidth(CharWidth: integer): integer;
begin
  if not fVisible then
  begin
    Result := 0;
    Exit;
  end;
  
  if fShowLineNumbers then
    Result := fLeftOffset + fRightOffset + fAutoSizeDigitCount * CharWidth + 2
  else
    Result := fWidth;

  if fShowCodeFolding then
    Result := Result + CodeFoldingWidth;
end;

procedure TSynGutter.SetAutoSize(const Value: boolean);
begin
  if fAutoSize <> Value then begin
    fAutoSize := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

{$IFDEF SYN_LAZARUS}
procedure TSynGutter.SetCodeFoldingWidth(const AValue: integer);
begin
  if FCodeFoldingWidth=AValue then exit;
  FCodeFoldingWidth:=AValue;
  if Assigned(fOnChange) then fOnChange(Self);
end;
{$ENDIF}

procedure TSynGutter.SetColor(const Value: TColor);
begin
  if fColor <> Value then begin
    fColor := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetDigitCount(Value: integer);
begin
  Value := MinMax(Value, 2, 12);
  if fDigitCount <> Value then begin
    fDigitCount := Value;
    fAutoSizeDigitCount := fDigitCount;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetLeadingZeros(const Value: boolean);
begin
  if fLeadingZeros <> Value then begin
    fLeadingZeros := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetLeftOffset(Value: integer);
begin
  Value := Max(0, Value);
  if fLeftOffset <> Value then begin
    fLeftOffset := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetRightOffset(Value: integer);
begin
  Value := Max(0, Value);
  if fRightOffset <> Value then begin
    fRightOffset := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetShowLineNumbers(const Value: boolean);
begin
  if fShowLineNumbers <> Value then begin
    fShowLineNumbers := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetShowCodeFolding(const Value: boolean);
begin
  if fShowCodeFolding <> Value then begin
    fShowCodeFolding := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetUseFontStyle(Value: boolean);
begin
  if fUseFontStyle <> Value then begin
    fUseFontStyle := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetVisible(Value: boolean);
begin
  if fVisible <> Value then begin
    fVisible := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetWidth(Value: integer);
begin
  Value := Max(0, Value);
  if fWidth <> Value then begin
    fWidth := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetZeroStart(const Value: boolean);
begin
  if fZeroStart <> Value then begin
    fZeroStart := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
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

{ TSynMethodChain }

procedure TSynMethodChain.Add(AEvent: TMethod);
begin
  if not Assigned(@AEvent) then
    raise ESynMethodChain.CreateFmt(
      '%s.Entry : the parameter `AEvent'' must be specified.', [ClassName]);

  with FNotifyProcs, AEvent do
  begin
    Add(Code);
    Add(Data);
  end
end;

constructor TSynMethodChain.Create;
begin
  inherited;
  FNotifyProcs := TList.Create;
end;

destructor TSynMethodChain.Destroy;
begin
  FNotifyProcs.Free;
  inherited;
end;

function TSynMethodChain.DoHandleException(E: Exception): Boolean;
begin
  if not Assigned(FExceptionHandler) then
    raise E
  else
    try
      Result := True;
      FExceptionHandler(Self, E, Result);
    except
      raise ESynMethodChain.CreateFmt(
        '%s.DoHandleException : MUST NOT occur any kind of exception in '+
        'ExceptionHandler', [ClassName]);
    end;
end;

procedure TSynMethodChain.Fire;
var
  AMethod: TMethod;
  i: Integer;
begin
  i := 0;
  with FNotifyProcs, AMethod do
    while i < Count do
      try
        repeat
          Code := Items[i];
          Inc(i);
          Data := Items[i];
          Inc(i);

          DoFire(AMethod)
        until i >= Count;
      except
        on E: Exception do
          if not DoHandleException(E) then
            i := MaxInt;
      end;
end;

procedure TSynMethodChain.Remove(AEvent: TMethod);
var
  i: Integer;
begin
  if not Assigned(@AEvent) then
    raise ESynMethodChain.CreateFmt(
      '%s.Remove: the parameter `AEvent'' must be specified.', [ClassName]);

  with FNotifyProcs, AEvent do
  begin
    i := Count - 1;
    while i > 0 do
      if Items[i] <> Data then
        Dec(i, 2)
      else
      begin
        Dec(i);
        if Items[i] = Code then
        begin
          Delete(i);
          Delete(i);
        end;
        Dec(i);
      end;
  end;
end;

{ TSynNotifyEventChain }

procedure TSynNotifyEventChain.Add(AEvent: TNotifyEvent);
begin
  inherited Add(TMethod(AEvent));
end;

constructor TSynNotifyEventChain.CreateEx(ASender: TObject);
begin
  inherited Create;
  FSender := ASender;
end;

procedure TSynNotifyEventChain.DoFire(AEvent: TMethod);
begin
  TNotifyEvent(AEvent)(FSender);
end;

procedure TSynNotifyEventChain.Remove(AEvent: TNotifyEvent);
begin
  inherited Remove(TMethod(AEvent));
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

end.

