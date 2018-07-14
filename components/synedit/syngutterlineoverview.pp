{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

-------------------------------------------------------------------------------}

unit SynGutterLineOverview;

{$I synedit.inc}

interface

uses
  Classes, Graphics, Controls, LCLProc, LCLType, LCLIntf, Forms, LMessages,
  FPCanvas, sysutils, math, SynGutterBase, SynEditTypes, LazSynEditText,
  SynEditTextBuffer, SynEditMarks, SynEditMiscClasses, SynEditFoldedView,
  SynEditMouseCmds;

type
  TSynGutterLineOverview = class;
  TSynGutterLineOverviewProviderList = class;
  TSynGutterLOvLineMarks = class;

  { TSynGutterLOvMark }

  TSynGutterLOvMark = class
  private
    FColor: TColor;
    FColumn: Integer;
    FLine: Integer;
    FLineMarks: TSynGutterLOvLineMarks;
    FOnChange: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FPriority: Integer;
  protected
    FData: Pointer;
  protected
    // for TSynGutterLOvLineMarks
    function CompareByPrior(Other: TSynGutterLOvMark): Integer;
    function CompareByLine(Other: TSynGutterLOvMark): Integer;
    procedure DoChange;
    property LineMarks: TSynGutterLOvLineMarks read FLineMarks write FLineMarks;
  public
    destructor Destroy; override;
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
    property Color: TColor read FColor;
    property Priority: Integer read FPriority;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

  { TSynGutterLOvMarkList }

  TSynGutterLOvMarkList = class(TFPList)
  private
    FOwnMarks: Boolean;
    FLockCount: Integer;
    FNeedSort: Boolean;
    function GetMark(Index: Integer): TSynGutterLOvMark;
    procedure PutMark(Index: Integer; const AValue: TSynGutterLOvMark);
  protected
    procedure ReSort; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
    procedure Add(AValue: TSynGutterLOvMark); virtual;
    property Items[Index: Integer]: TSynGutterLOvMark read GetMark write PutMark; default;
  end;

  { TSynGutterLOvLineMarks
    List of TSynGutterLOvMark on a given Line (PixelLine)
  }

  TSynGutterLOvLineMarks = class(TSynGutterLOvMarkList)
  private
    FPixLine: Integer;
  protected
    procedure ReSort; override; // by prior
    procedure ReSortByLine;
    function Compare(Other: TSynGutterLOvLineMarks): Integer;
  public
    constructor Create;
    procedure Paint(Canvas: TCanvas; AClip: TRect; TopOffset: integer; AnItemHeight: Integer);
    procedure Add(AValue: TSynGutterLOvMark); override;
    property PixLine: Integer read FPixLine;
  end;

  { TSynGutterLOvLineMarksList
    List of (Pixel-)Lines with Marks
  }

  TSynGutterLOvLineMarksList = class(TFPList)
  private
    FItemHeight: Integer;
    FPixelHeight: Integer;
    FPixelPerLine: Integer;
    FTextLineCount: Integer;
    function GetLineMarks(Index: Integer): TSynGutterLOvLineMarks;
    procedure PutLineMarks(Index: Integer; const AValue: TSynGutterLOvLineMarks);
    function ItemForLine(ALine: Integer; CreateIfNotExists: Boolean = False): TSynGutterLOvLineMarks;
    procedure SetItemHeight(const AValue: Integer);
    procedure SetPixelHeight(const AValue: Integer);
    procedure SetTextLineCount(const AValue: Integer);
  protected
    function IndexForLine(ALine: Integer; PreviousIfNotExist: Boolean = False;
      UseItemHeight: Boolean = False): Integer;
    procedure ReSort;
    procedure MarkChanged(Sender: TObject);
    procedure MarkDestroying(Sender: TObject);
    function TextLineToPixLine(ATxtLine: Integer): Integer;
    procedure ReBuild(AFromIndex: Integer = -1);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddMark(AMark: TSynGutterLOvMark);
    property Items[Index: Integer]: TSynGutterLOvLineMarks
      read GetLineMarks write PutLineMarks; default;
    property PixelHeight: Integer read FPixelHeight write SetPixelHeight;
    property TextLineCount: Integer read FTextLineCount write SetTextLineCount;
    property ItemHeight: Integer read FItemHeight write SetItemHeight;
  end;

  { TSynGutterLineOverviewProvider }

  TSynGutterLineOverviewProvider = class(TSynObjectListItem)
  private
    FColor: TColor;
    FHeight: Integer;
    FGutterPart: TSynGutterLineOverview;
    FPriority: Integer;
    FRGBColor: TColor;
    function  GetList: TSynGutterLineOverviewProviderList;
    procedure SetColor(const AValue: TColor);
    procedure SetHeight(const AValue: Integer);
    procedure SetPriority(const AValue: Integer);
  protected
    function  Compare(Other: TSynObjectListItem): Integer; override;
    procedure DoChange(Sender: TObject);

    procedure InvalidateTextLines(AFromLine, AToLine: Integer);
    procedure InvalidatePixelLines(AFromLine, AToLine: Integer);
    function  TextLineToPixel(ALine: Integer): Integer;
    function  TextLineToPixelEnd(ALine: Integer): Integer;
    function  PixelLineToText(ALineIdx: Integer): Integer;
    procedure ReCalc; virtual;                                                  // Does not invalidate

    function  SynEdit: TSynEditBase;
    property  Owner: TSynGutterLineOverviewProviderList read GetList; //the list
    property  GutterPart: TSynGutterLineOverview read FGutterPart;
    property  RGBColor: TColor read FRGBColor;

    procedure Paint(Canvas: TCanvas; AClip: TRect; TopOffset: integer); virtual;
  private
    FMarkList: TSynGutterLOvMarkList;
    function  GetMarks(AnIndex: Integer): TSynGutterLOvMark; virtual;
  protected
    function  MarksCount: Integer;
    property  Marks[AnIndex: Integer]: TSynGutterLOvMark read GetMarks;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property Height: Integer read FHeight write SetHeight;
  published
    property Priority: Integer read FPriority write SetPriority;
    property Color: TColor read FColor write SetColor;
  end;

  { TSynGutterLineOverviewProviderList }

  TSynGutterLineOverviewProviderList = class(TSynObjectList)
  private
    function GetGutterPart: TSynGutterLineOverview;
    function GetProviders(AIndex: Integer): TSynGutterLineOverviewProvider;
  public
    constructor Create(AOwner: TComponent); override;
    property Owner: TSynGutterLineOverview read GetGutterPart;
    property Providers[AIndex: Integer]: TSynGutterLineOverviewProvider
             read GetProviders; default;
  end;

  { TSynGutterLOvProviderCurrentPage }

  TSynGutterLOvProviderCurrentPage = class(TSynGutterLineOverviewProvider)
  private
    FCurTopLine, FCurBottomLine: Integer;
    FFoldedTextBuffer: TSynEditFoldedView;
    FPixelTopLine, FPixelBottomLine: Integer;
    procedure SetFoldedTextBuffer(AValue: TSynEditFoldedView);
  protected
    procedure SynStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
    procedure FoldChanged(aLine: Integer);

    procedure Paint(Canvas: TCanvas; AClip: TRect; TopOffset: integer); override;
    procedure ReCalc; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property FoldedTextBuffer: TSynEditFoldedView read FFoldedTextBuffer write SetFoldedTextBuffer;
  end;

  { TSynGutterLOvProviderModifiedLines }

  TSynGutterLOvProviderModifiedLines = class(TSynGutterLineOverviewProvider)
  private
    FColorSaved, FRGBColorSaved: TColor;
    FPixLineStates: Array of TSynEditStringFlags;
    FFirstTextLineChanged, FLastTextLineChanged: Integer;
    procedure SetColorSaved(const AValue: TColor);
  protected
    procedure Paint(Canvas: TCanvas; AClip: TRect; TopOffset: integer); override;
    procedure ReScan;
    procedure ReCalc; override;

    procedure BufferChanged(Sender: TObject);
    procedure LineModified(Sender: TSynEditStrings; aIndex, aNewCount, aOldCount: Integer);
    procedure SynStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property ColorSaved: TColor read FColorSaved write SetColorSaved;
  end;

  { TSynGutterLOvProviderBookmarks }
  TSynGutterLOvProviderBookmarks = class;

  TSynGutterColorforMarkEvent = procedure(ASender: TSynGutterLOvProviderBookmarks;
                                          AMark: TSynEditMark;
                                          var AColor: TColor;
                                          var APriority: Integer) of object;

  TSynGutterLOvProviderBookmarks = class(TSynGutterLineOverviewProvider)
  private
    FOnColorForMark: TSynGutterColorforMarkEvent;
  protected
    procedure AdjustColorForMark(AMark: TSynEditMark; var AColor: TColor; var APriority: Integer); virtual;
    procedure DoMarkChange(Sender: TSynEditMark; Changes: TSynEditMarkChangeReasons);
    function CreateGutterMark(ASynMark: TSynEditMark): TSynGutterLOvMark;
    function IndexOfSynMark(ASynMark: TSynEditMark): Integer;
    procedure BufferChanging(Sender: TObject);
    procedure BufferChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property OnColorForMark: TSynGutterColorforMarkEvent read FOnColorForMark write FOnColorForMark;
  end;

  TSynGutterLOvProviderCustom = class(TSynGutterLineOverviewProvider)
  end;

  { TSynChildWinControl
    Allow individual invalidates, for less painting
  }

  TSynChildWinControl = class(TCustomControl)
  protected
    procedure WMNCHitTest(var Message: TLMessage); message LM_NCHITTEST;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TSynGutterLineOverview }
  TSynGutterLOvStateFlag = (losLineCountChanged, losResized, losASyncScheduled, losWaitForPaint);
  TSynGutterLOvStateFlags = set of TSynGutterLOvStateFlag;

  TSynGutterLineOverview = class(TSynGutterPartBase)
  private
    FProviders: TSynGutterLineOverviewProviderList;
    FWinControl: TSynChildWinControl;
    FLineMarks: TSynGutterLOvLineMarksList;
    FMouseActionsForMarks: TSynEditMouseInternalActions;
    FState: TSynGutterLOvStateFlags;
    FPpiPenWidth: Integer;
    function GetMarkHeight: Integer;
    function GetMouseActionsForMarks: TSynEditMouseActions;
    procedure SetMarkHeight(const AValue: Integer);
    procedure ScheduleASync(AStates: TSynGutterLOvStateFlags);
    procedure ExecASync(Data: PtrInt);
    procedure SetMouseActionsForMarks(AValue: TSynEditMouseActions);
  protected
    function  PreferedWidth: Integer; override;
    procedure Init; override;
    procedure LineCountChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
    procedure BufferChanged(Sender: TObject);
    procedure SetVisible(const AValue : boolean); override;
    procedure GutterVisibilityChanged; override;
    procedure DoChange(Sender: TObject); override;
  protected
    procedure InvalidateTextLines(AFromLine, AToLine: Integer);
    procedure InvalidatePixelLines(AFromLine, AToLine: Integer);
    function  PixelLineToText(ALineIdx: Integer): Integer;
    function  TextLineToPixel(ALine: Integer): Integer;
    function  TextLineToPixelEnd(ALine: Integer): Integer;
    procedure DoResize(Sender: TObject); override;
    Procedure PaintWinControl(Sender: TObject);
    //function CreateMouseActions: TSynEditMouseInternalActions; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer); override;
    procedure AddMark(AMark: TSynGutterLOvMark);
    procedure ScalePPI(const AScaleFactor: Double); override;

    function MaybeHandleMouseAction(var AnInfo: TSynEditMouseActionInfo;
      HandleActionProc: TSynEditMouseActionHandler): Boolean; override;
    function DoHandleMouseAction(AnAction: TSynEditMouseAction;
                                 var AnInfo: TSynEditMouseActionInfo): Boolean; override;

    property Providers: TSynGutterLineOverviewProviderList read FProviders;
  published
    property MarkHeight: Integer read GetMarkHeight write SetMarkHeight;
    property MarkupInfo;
    property MouseActionsForMarks: TSynEditMouseActions
      read GetMouseActionsForMarks write SetMouseActionsForMarks;
  end;

implementation
uses
  SynEdit;

{ TSynGutterLOvMark }

function TSynGutterLOvMark.CompareByPrior(Other: TSynGutterLOvMark): Integer;
begin
  Result := Priority - Other.Priority;
  if Result <> 0 then exit;
  Result := Line - Other.Line;
  if Result <> 0 then exit;
  Result := Column - Other.Column;
  if Result <> 0 then exit;
  {$PUSH}{$Q-} // Overflow is allowed to occur
  Result := Integer(PtrUint(self) - PtrUInt(Other));
  {$POP}
end;

function TSynGutterLOvMark.CompareByLine(Other: TSynGutterLOvMark): Integer;
begin
  Result := Line - Other.Line;
  if Result <> 0 then exit;
  Result := Column - Other.Column;
  if Result <> 0 then exit;
  Result := Priority - Other.Priority;
  if Result <> 0 then exit;
  {$PUSH}{$Q-} // Overflow is allowed to occur
  Result := Integer(PtrUint(self) - PtrUInt(Other));
  {$POP}
end;

procedure TSynGutterLOvMark.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

destructor TSynGutterLOvMark.Destroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
  inherited Destroy;
end;

{ TSynGutterLOvMarkList }

function TSynGutterLOvMarkList.GetMark(Index: Integer): TSynGutterLOvMark;
begin
  Result := TSynGutterLOvMark(Get(Index));
end;

procedure TSynGutterLOvMarkList.PutMark(Index: Integer; const AValue: TSynGutterLOvMark);
begin
  Put(Index, AValue);
  ReSort;
end;

procedure TSynGutterLOvMarkList.Add(AValue: TSynGutterLOvMark);
begin
  inherited Add(AValue);
  ReSort;
end;

procedure TSynGutterLOvMarkList.ReSort;
begin
  // nothing
end;

constructor TSynGutterLOvMarkList.Create;
begin
  inherited;
  FOwnMarks := True;
end;

destructor TSynGutterLOvMarkList.Destroy;
begin
  if FOwnMarks then begin
    while Count > 0 do begin
      Items[0].Destroy;
      Delete(0);
    end;
  end;
  inherited Destroy;
end;

procedure TSynGutterLOvMarkList.Lock;
begin
  inc(FLockCount);
end;

procedure TSynGutterLOvMarkList.UnLock;
begin
  dec(FLockCount);
  if (FLockCount = 0) and FNeedSort then
    ReSort;
end;

{ TSynGutterLOvLineMarks }

function SynGutterLOvProviderLineMarksSort(Item1, Item2: Pointer): Integer;
begin
  Result := TSynGutterLOvMark(Item1).CompareByPrior(TSynGutterLOvMark(Item2));
end;

procedure TSynGutterLOvLineMarks.ReSort;
begin
  FNeedSort := FLockCount > 0;
  if FLockCount = 0 then
    Sort(@SynGutterLOvProviderLineMarksSort);
end;

function SynGutterLOvProviderLineMarksSortByLine(Item1, Item2: Pointer): Integer;
begin
  Result := TSynGutterLOvMark(Item1).CompareByLine(TSynGutterLOvMark(Item2));
end;

procedure TSynGutterLOvLineMarks.ReSortByLine;
begin
  if FLockCount > 0 then
    FNeedSort := True;
  Sort(@SynGutterLOvProviderLineMarksSortByLine);
end;

function TSynGutterLOvLineMarks.Compare(Other: TSynGutterLOvLineMarks): Integer;
begin
  Result := PixLine - Other.PixLine;
  if Result <> 0 then exit;
  {$PUSH}{$Q-} // Overflow is allowed to occur
  Result := PtrUint(self) - PtrUInt(Other);
  {$POP}
end;

constructor TSynGutterLOvLineMarks.Create;
begin
  inherited;
  FOwnMarks := False;
end;

procedure TSynGutterLOvLineMarks.Paint(Canvas: TCanvas; AClip: TRect; TopOffset: integer;
  AnItemHeight: Integer);
var
  bs: TFPBrushStyle;
begin
  if (FPixLine + AnItemHeight < AClip.Top - TopOffset) or
     (FPixLine > AClip.Bottom - TopOffset)
  then
    exit;

  AClip.Top    :=  FPixLine;
  AClip.Bottom := Max(FPixLine+1, FPixLine + AnItemHeight - 1);
  inc(AClip.Left);
  dec(AClip.Right);

  Canvas.Pen.Color := Items[0].Color;
  bs := Canvas.Brush.Style;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(AClip);
  Canvas.Brush.Style := bs;
end;

procedure TSynGutterLOvLineMarks.Add(AValue: TSynGutterLOvMark);
begin
  AValue.LineMarks := Self;
  inherited Add(AValue);
end;

{ TSynGutterLOvLineMarksList }

function TSynGutterLOvLineMarksList.GetLineMarks(Index: Integer): TSynGutterLOvLineMarks;
begin
  Result := TSynGutterLOvLineMarks(inherited Items[Index]);
end;

procedure TSynGutterLOvLineMarksList.PutLineMarks(Index: Integer;
  const AValue: TSynGutterLOvLineMarks);
begin
  inherited Items[Index] := AValue;
  Resort;
end;

function SynGutterLOvProviderLineMarksListSort(Item1, Item2: Pointer): Integer;
begin
  Result := TSynGutterLOvLineMarks(Item1).Compare(TSynGutterLOvLineMarks(Item2));
end;

procedure TSynGutterLOvLineMarksList.ReSort;
begin
  Sort(@SynGutterLOvProviderLineMarksListSort);
end;

procedure TSynGutterLOvLineMarksList.MarkChanged(Sender: TObject);
var
  LMark: TSynGutterLOvLineMarks;
  j: Integer;
begin
  LMark := TSynGutterLOvMark(Sender).LineMarks;
  j := TextLineToPixLine(TSynGutterLOvMark(Sender).Line);
  if (j >= LMark.PixLine) and (j < LMark.PixLine + ItemHeight) then begin
    LMark.ReSort;
    exit;
  end;
  LMark.Remove(Sender);
  if LMark.Count = 0 then begin
    Remove(LMark);
    LMark.Free;
  end;
  AddMark(TSynGutterLOvMark(Sender));
end;

procedure TSynGutterLOvLineMarksList.MarkDestroying(Sender: TObject);
var
  LMark: TSynGutterLOvLineMarks;
begin
  LMark := TSynGutterLOvMark(Sender).LineMarks;
  LMark.Remove(Sender);
  if LMark.Count = 0 then begin
    Remove(LMark);
    LMark.Free;
  end;
end;

function TSynGutterLOvLineMarksList.TextLineToPixLine(ATxtLine: Integer): Integer;
begin
  if PixelHeight < 1 then exit(0);

  Result := (Int64(ATxtLine) - 1) * Int64(PixelHeight) div TextLineCount;

  If FPixelPerLine * 2 < ItemHeight then
    dec(Result)
  else
  if FPixelPerLine > ItemHeight + 2 then
    inc(Result);

  if Result + ItemHeight > PixelHeight then Result := PixelHeight - ItemHeight;
  if Result < 0 then Result := 0;
end;

procedure TSynGutterLOvLineMarksList.ReBuild(AFromIndex: Integer = -1);

  procedure PushItemsDown(ASrc, ADest: TSynGutterLOvLineMarks);
  var
    i, j: Integer;
  begin
    i := ASrc.Count -1;
    while i >= 0 do begin
       j := TextLineToPixLine(ASrc[i].Line);
       if j >= ASrc.PixLine +ItemHeight then begin
         if (ADest.Count = 0) or (ADest.PixLine > j) then
           ADest.FPixLine := j;
         ADest.Add(ASrc[i]);
         ASrc.Delete(i);
       end;
       dec(i);
    end;
  end;

  procedure PullItemsUp(ADest, ASrc: TSynGutterLOvLineMarks);
  var
    i, j, NewLine: Integer;
  begin
    i := ASrc.Count -1;
    NewLine := MaxInt;
    while i >= 0 do begin
       j := TextLineToPixLine(ASrc[i].Line);
       if j < ADest.PixLine + ItemHeight then begin
         ADest.Add(ASrc[i]);
         ASrc.Delete(i);
       end
       else
         if j < NewLine then NewLine := j;
       dec(i);
    end;
    ASrc.FPixLine := NewLine;
  end;

var
  i, j, NewIdx: Integer;
  CurItem, NextItem, TmpItem: TSynGutterLOvLineMarks;
begin
  if Count = 0 then exit;

  if AFromIndex < 0 then begin
    NewIdx := -1;
    for i := 0 to Items[0].Count -1 do begin
      j := TextLineToPixLine(Items[0][i].Line);
      if (i = 0) or (j < NewIdx) then
        NewIdx := j;
    end;
    Items[0].FPixLine := NewIdx;
    AFromIndex := 0;
  end;

  TmpItem := TSynGutterLOvLineMarks.Create;
  TmpItem.Lock;
  NextItem := Items[AFromIndex];
  NextItem.Lock;
  while NextItem <> nil do begin
    if (TmpItem.Count > 0) and (TmpItem.PixLine < NextItem.PixLine) then begin
      Insert(AFromIndex, TmpItem);
      CurItem := TmpItem;
      TmpItem := TSynGutterLOvLineMarks.Create;
      TmpItem.Lock;
    end
    else
      CurItem := NextItem;

    inc(AFromIndex);
    if AFromIndex < Count then begin
      NextItem := Items[AFromIndex];
      NextItem.Lock;
      if NextItem.PixLine < CurItem.PixLine + ItemHeight then
        NextItem.FPixLine := CurItem.PixLine + ItemHeight;
    end
    else
      NextItem := nil;

    PushItemsDown(CurItem, TmpItem);
    PullItemsUp(CurItem, TmpItem);

    while NextItem <> nil do begin
      PullItemsUp(CurItem, NextItem);
      if NextItem.Count > 0 then
        break;
      Delete(AFromIndex);
      NextItem.Free;
      if AFromIndex < Count then begin
        NextItem := Items[AFromIndex];
        NextItem.Lock;
      end
      else
        NextItem := nil;
    end;

    if (TmpItem.Count > 0) and (NextItem = nil) then begin
      Insert(AFromIndex, TmpItem);
      NextItem := TmpItem;
      TmpItem := TSynGutterLOvLineMarks.Create;
      TmpItem.Lock;
    end;


    CurItem.UnLock;
    if CurItem.Count = 0 then begin
      Delete(AFromIndex - 1);
      dec(AFromIndex);
      CurItem.Free;
    end;
  end;
  TmpItem.Free;
end;

constructor TSynGutterLOvLineMarksList.Create;
begin
  FTextLineCount := 1;
  FItemHeight := 4;
  inherited;
end;

destructor TSynGutterLOvLineMarksList.Destroy;
begin
  while Count > 0 do begin
    Items[0].Destroy;
    Delete(0);
  end;
  inherited Destroy;
end;

procedure TSynGutterLOvLineMarksList.AddMark(AMark: TSynGutterLOvMark);
var
  i, PixLine: Integer;
  LMarks: TSynGutterLOvLineMarks;
begin
  AMark.OnChange := @MarkChanged;
  AMark.OnDestroy := @MarkDestroying;
  PixLine := TextLineToPixLine(AMark.Line);

  i := IndexForLine(PixLine, True);
  if i >= 0 then begin
    LMarks := Items[i];
    if (PixLine >= LMarks.PixLine) and (PixLine < LMarks.PixLine + ItemHeight) then begin
      LMarks.Add(AMark);
      // sendinvalidate
      exit;
    end;
  end;

  inc(i);
  LMarks := TSynGutterLOvLineMarks.Create;
  LMarks.FPixLine := PixLine;
  Insert(i, LMarks);

  LMarks.Add(AMark);
  // sendinvalidate

  if (i < Count - 1) and (Items[i+1].PixLine < LMarks.PixLine + ItemHeight) then
    ReBuild(i);
end;

function TSynGutterLOvLineMarksList.IndexForLine(ALine: Integer;
  PreviousIfNotExist: Boolean; UseItemHeight: Boolean): Integer;
var
  l, h, m: Integer;
begin
  l := 0;
  h := Count - 1;
  if h < 0 then
    exit(-1);

  while h > l do begin
    m := (h+l) div 2;
    if Items[m].PixLine <= ALine then
      l := m + 1
    else
      h := m;
  end;
  Result := h;

  if Items[Result].PixLine > ALine then begin
    dec(Result);
    if Result < 0 then exit;
  end;
  Assert(Items[Result].PixLine <= ALine);

  if UseItemHeight and (Items[Result].PixLine + ItemHeight > ALine) then
    exit;

  if (Items[Result].PixLine = ALine) or (PreviousIfNotExist) then
    exit;

  Result := -1;
end;

function TSynGutterLOvLineMarksList.ItemForLine(ALine: Integer;
  CreateIfNotExists: Boolean): TSynGutterLOvLineMarks;
var
  i: Integer;
  LMarks: TSynGutterLOvLineMarks;
begin
  i := IndexForLine(ALine, CreateIfNotExists);
  if CreateIfNotExists and ( (i < 0) or (Items[i].PixLine <> ALine) ) then begin
    inc(i);
    LMarks := TSynGutterLOvLineMarks.Create;
    LMarks.FPixLine := i;
    Insert(i, LMarks);
  end;
  if  i >= 0 then
    Result := Items[i]
  else
    Result := nil;
end;

procedure TSynGutterLOvLineMarksList.SetItemHeight(const AValue: Integer);
begin
  if FItemHeight = AValue then exit;
  FItemHeight := AValue;
  ReBuild;
end;

procedure TSynGutterLOvLineMarksList.SetPixelHeight(const AValue: Integer);
begin
  if FPixelHeight = AValue then exit;
  FPixelHeight := AValue;
  FPixelPerLine := FPixelHeight div TextLineCount;
  ReBuild;
end;

procedure TSynGutterLOvLineMarksList.SetTextLineCount(const AValue: Integer);
begin
  if FTextLineCount = AValue then exit;
  FTextLineCount := Max(1, AValue);
  ReBuild;
end;

{ TSynGutterLineOverviewProvider }

constructor TSynGutterLineOverviewProvider.Create(AOwner: TComponent);
begin
  inherited;
  FGutterPart := Owner.Owner;
  FColor := clLtGray;
  FriendEdit := SynEdit;
  FMarkList := TSynGutterLOvMarkList.Create;
end;

destructor TSynGutterLineOverviewProvider.Destroy;
begin
  FreeAndNil(FMarkList);
  inherited Destroy;
end;

function TSynGutterLineOverviewProvider.GetList: TSynGutterLineOverviewProviderList;
begin
  Result := TSynGutterLineOverviewProviderList(inherited Owner);
end;

function TSynGutterLineOverviewProvider.GetMarks(AnIndex: Integer): TSynGutterLOvMark;
begin
  Result := FMarkList[Index];
end;

procedure TSynGutterLineOverviewProvider.SetColor(const AValue: TColor);
begin
  if FColor = AValue then exit;
  FColor := AValue;
  FRGBColor := TColor(ColorToRGB(AValue));
  DoChange(Self);
end;

procedure TSynGutterLineOverviewProvider.SetHeight(const AValue: Integer);
begin
  if FHeight = AValue then exit;
  FHeight := AValue;
  ReCalc;
end;

procedure TSynGutterLineOverviewProvider.SetPriority(const AValue: Integer);
begin
  if FPriority = AValue then exit;
  FPriority := AValue;
  Owner.Sort;
end;

function TSynGutterLineOverviewProvider.SynEdit: TSynEditBase;
begin
  Result := FGutterPart.SynEdit;
end;

function TSynGutterLineOverviewProvider.Compare(Other: TSynObjectListItem): Integer;
begin
  Result := Priority - TSynGutterLineOverviewProvider(Other).Priority;
  if Result = 0 then
    Result := inherited Compare(Other);
end;

procedure TSynGutterLineOverviewProvider.DoChange(Sender: TObject);
begin
  FGutterPart.DoChange(Sender);
end;

procedure TSynGutterLineOverviewProvider.InvalidateTextLines(AFromLine, AToLine: Integer);
begin
  FGutterPart.InvalidateTextLines(AFromLine, AToLine);
end;

procedure TSynGutterLineOverviewProvider.InvalidatePixelLines(AFromLine, AToLine: Integer);
begin
  FGutterPart.InvalidatePixelLines(AFromLine, AToLine);
end;

function TSynGutterLineOverviewProvider.TextLineToPixel(ALine: Integer): Integer;
var
  c: Integer;
begin
  if ALine < 0 then exit(-1);
  c := Max(1, TextBuffer.Count);
  if c = 0 then
    Result := -1
  else
    Result := (Int64(ALine - 1) * Int64(Height)) div c;
end;

function TSynGutterLineOverviewProvider.TextLineToPixelEnd(ALine: Integer): Integer;
var
  c, n: Integer;
begin
  if ALine < 0 then exit(-1);
  c := Max(1, TextBuffer.Count);
  if c = 0 then
    Result := -1
  else begin
    Result := (Int64(ALine - 1) * Int64(Height)) div c;
    n      := (Int64(ALine)     * Int64(Height)) div c - 1; // next line - 1 pix
    if n > Result then
      Result := n;
  end;
end;

function TSynGutterLineOverviewProvider.PixelLineToText(ALineIdx: Integer): Integer;
var
  c: Int64;
begin
  c := Max(1, TextBuffer.Count);
  if c = 0 then
    Result := -1
  else begin
    Result := (Int64(ALineIdx) * c) div Height + 1;
    if (Int64(Result - 1) * Int64(Height)) div c <> ALineIdx then
      inc(Result);
  end;
end;

procedure TSynGutterLineOverviewProvider.ReCalc;
begin
  // nothing
end;

procedure TSynGutterLineOverviewProvider.Paint(Canvas: TCanvas; AClip: TRect;
  TopOffset: integer);
begin
  // nothing
end;

function TSynGutterLineOverviewProvider.MarksCount: Integer;
begin
  Result := FMarkList.Count;
end;

{ TSynGutterLineOverviewProviderList }

function TSynGutterLineOverviewProviderList.GetGutterPart: TSynGutterLineOverview;
begin
  Result := TSynGutterLineOverview(inherited Owner);
end;

function TSynGutterLineOverviewProviderList.GetProviders(AIndex: Integer): TSynGutterLineOverviewProvider;
begin
  Result := TSynGutterLineOverviewProvider(BaseItems[AIndex]);
end;

constructor TSynGutterLineOverviewProviderList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Sorted := True;
end;

{ TSynGutterLOvProviderCurrentPage }

procedure TSynGutterLOvProviderCurrentPage.FoldChanged(aLine: Integer);
begin
  SynStatusChanged(nil, []);
end;

procedure TSynGutterLOvProviderCurrentPage.SetFoldedTextBuffer(AValue: TSynEditFoldedView);
begin
  if FFoldedTextBuffer <> nil then
    FFoldedTextBuffer.RemoveFoldChangedHandler(@FoldChanged);

  if FFoldedTextBuffer = AValue then Exit;
  FFoldedTextBuffer := AValue;

  if FFoldedTextBuffer <> nil then
    FFoldedTextBuffer.AddFoldChangedHandler(@FoldChanged);
end;

procedure TSynGutterLOvProviderCurrentPage.SynStatusChanged(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  InvalidatePixelLines(FPixelTopLine, FPixelBottomLine);
  FCurTopLine := TCustomSynEdit(SynEdit).TopLine;
  FCurBottomLine := TCustomSynEdit(SynEdit).ScreenRowToRow(TCustomSynEdit(SynEdit).LinesInWindow);
  ReCalc;
  InvalidatePixelLines(FPixelTopLine, FPixelBottomLine);
end;

procedure TSynGutterLOvProviderCurrentPage.ReCalc;
begin
  FPixelTopLine    := TextLineToPixel(FCurTopLine);
  FPixelBottomLine := TextLineToPixelEnd(FCurBottomLine - 1);
end;

procedure TSynGutterLOvProviderCurrentPage.Paint(Canvas: TCanvas; AClip: TRect;
  TopOffset: integer);
begin
  if (FPixelBottomLine < AClip.Top - TopOffset) or
     (FPixelTopLine > AClip.Bottom - TopOffset)
  then
    exit;

  AClip.Top    := Max(AClip.Top, FPixelTopLine+TopOffset);
  AClip.Bottom := Min(AClip.Bottom, FPixelBottomLine+TopOffset);
  Canvas.Brush.Color := FRGBColor;
  Canvas.FillRect(AClip);
end;

constructor TSynGutterLOvProviderCurrentPage.Create(AOwner: TComponent);
begin
  inherited;
  FColor := 0;
  Color := $C0C0C0;
  TCustomSynEdit(SynEdit).RegisterStatusChangedHandler(@SynStatusChanged,
                                                 [scTopLine, scLinesInWindow]);
end;

destructor TSynGutterLOvProviderCurrentPage.Destroy;
begin
  TCustomSynEdit(SynEdit).UnRegisterStatusChangedHandler(@SynStatusChanged);
  inherited;
end;

{ TSynGutterLOvProviderModifiedLines }

procedure TSynGutterLOvProviderModifiedLines.SetColorSaved(const AValue: TColor);
begin
  if FColorSaved = AValue then exit;
  FColorSaved := AValue;
  FRGBColorSaved := TColor(ColorToRGB(AValue));
  DoChange(Self);
end;

procedure TSynGutterLOvProviderModifiedLines.Paint(Canvas: TCanvas; AClip: TRect;
  TopOffset: integer);
var
  i, i2, imax: Integer;
  State: TSynEditStringFlags;
begin
  if FFirstTextLineChanged > 0 then ReScan;

  AClip.Right := AClip.Left + Round((AClip.Right - AClip.Left) / 3);
  i := AClip.Top - TopOffset;
  imax := AClip.Bottom - TopOffset;
  if imax > high(FPixLineStates) then imax := high(FPixLineStates);
  while i <= imax do begin
    i2 := i+1;
    State := FPixLineStates[i];
    while (i2 <= imax) and (FPixLineStates[i2] = State) do
      inc(i2);

    if State <> [] then begin
      AClip.Top    := i;
      AClip.Bottom := i2;
      if sfSaved in State then
        Canvas.Brush.Color := FRGBColorSaved
      else
        Canvas.Brush.Color := FRGBColor;
      Canvas.FillRect(AClip);
    end;

    i := i2;
  end;
end;

procedure TSynGutterLOvProviderModifiedLines.ReScan;
var
  i, PixLine, PixLineEnd, CurPixLineEnd, TxtIndex, TxtIndexEnd: Integer;
  NewState: TSynEditStringFlags;
begin
  SetLength(FPixLineStates, Height);

  PixLine := TextLineToPixel(FFirstTextLineChanged);
  if PixLine < 0 then PixLine := 0;

  if FLastTextLineChanged = 0 then
    PixLineEnd := TextLineToPixelEnd(TextBuffer.Count)
  else
    PixLineEnd := TextLineToPixelEnd(FLastTextLineChanged);
  if PixLineEnd < PixLine then PixLineEnd := PixLine;
  if PixLineEnd >= Height then PixLineEnd := Height - 1;

  while PixLine <= PixLineEnd do begin
    NewState := [];
    TxtIndex := PixelLineToText(PixLine) - 1;
    CurPixLineEnd := TextLineToPixelEnd(TxtIndex + 1);
    TxtIndexEnd := PixelLineToText(CurPixLineEnd + 1) - 1;

    while TxtIndex < TxtIndexEnd do begin
      NewState := NewState + TSynEditStringList(TextBuffer).Flags[TxtIndex];
      inc(TxtIndex);
    end;

    for i := PixLine to CurPixLineEnd do
      FPixLineStates[i] := NewState;
    PixLine := CurPixLineEnd + 1;
  end;

  FFirstTextLineChanged := -1;;
  FLastTextLineChanged := -1;;
end;

procedure TSynGutterLOvProviderModifiedLines.ReCalc;
begin
  FFirstTextLineChanged := 1;
  FLastTextLineChanged := 0;
end;

procedure TSynGutterLOvProviderModifiedLines.BufferChanged(Sender: TObject);
begin
  TSynEditStringList(Sender).RemoveHanlders(self);
  TSynEditStringList(TextBuffer).AddModifiedHandler(senrLinesModified, @LineModified);
  TSynEditStringList(TextBuffer).AddNotifyHandler(senrTextBufferChanged, @BufferChanged);
end;

procedure TSynGutterLOvProviderModifiedLines.LineModified(Sender: TSynEditStrings; aIndex,
  aNewCount, aOldCount: Integer);
begin
  if (FFirstTextLineChanged < 0) or (AIndex + 1 < FFirstTextLineChanged) then
    FFirstTextLineChanged := AIndex + 1;

  if aOldCount = aNewCount then
    aIndex := aIndex + aOldCount
  else
    aIndex := TextBuffer.Count;
  if (FLastTextLineChanged <> 0) and (AIndex + 1 > FLastTextLineChanged) then
    FLastTextLineChanged := AIndex + 1;

  InvalidateTextLines(FFirstTextLineChanged, FLastTextLineChanged);
end;

procedure TSynGutterLOvProviderModifiedLines.SynStatusChanged(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if (scModified in Changes) and not TCustomSynEdit(SynEdit).Modified then begin;
    FFirstTextLineChanged := 1;
    FLastTextLineChanged := 0; // open end
    InvalidateTextLines(FFirstTextLineChanged, TextBuffer.Count);
  end;
end;

constructor TSynGutterLOvProviderModifiedLines.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TSynEditStringList(TextBuffer).AddModifiedHandler(senrLinesModified, @LineModified);
  TSynEditStringList(TextBuffer).AddNotifyHandler(senrTextBufferChanged, @BufferChanged);
  TCustomSynEdit(SynEdit).RegisterStatusChangedHandler(@SynStatusChanged, [scModified]);
  FFirstTextLineChanged := -1;
  FLastTextLineChanged := -1;
  Color := clYellow;
  ColorSaved := clGreen;
end;

destructor TSynGutterLOvProviderModifiedLines.Destroy;
begin
  TSynEditStringList(TextBuffer).RemoveHanlders(self);
  TCustomSynEdit(SynEdit).UnRegisterStatusChangedHandler(@SynStatusChanged);
  inherited Destroy;
end;

{ TSynGutterLOvProviderBookmarks }

procedure TSynGutterLOvProviderBookmarks.DoMarkChange(Sender: TSynEditMark;
  Changes: TSynEditMarkChangeReasons);
var
  i: Integer;
  c: TColor;
  p: Integer;
begin
  if (smcrAdded in Changes) or
     ( (smcrVisible in Changes) and Sender.Visible )
  then begin
    if Sender.Visible then
      CreateGutterMark(Sender);
    InvalidateTextLines(0, TextBuffer.Count); // Todo
  end
  else
  if (smcrRemoved in Changes) or
     ( (smcrVisible in Changes) and not Sender.Visible )
  then begin
    i := IndexOfSynMark(Sender);
    if i >= 0 then begin
      FMarkList[i].Destroy;
      FMarkList.Delete(i);
    end;
    InvalidateTextLines(0, TextBuffer.Count); // Todo
  end
  else
  if (smcrLine in Changes) then begin
    i := IndexOfSynMark(Sender);
    if i >= 0 then begin
      FMarkList[i].FLine := Sender.Line;
      FMarkList[i].DoChange;
    end;
    FMarkList.ReSort;
    InvalidateTextLines(0, TextBuffer.Count); // Todo
  end
  else
  begin
    i := IndexOfSynMark(Sender);
    if i >= 0 then begin
      c := Color;
      p := Priority;
      AdjustColorForMark(Sender, c, p);
      FMarkList[i].FPriority := p;
      FMarkList[i].FColor    := c;
      FMarkList[i].DoChange;
    end;
    InvalidateTextLines(0, TextBuffer.Count); // Todo
  end
end;

procedure TSynGutterLOvProviderBookmarks.AdjustColorForMark(AMark: TSynEditMark;
  var AColor: TColor; var APriority: Integer);
begin
  if assigned(FOnColorForMark) then
    FOnColorForMark(Self, AMark, AColor, APriority);
end;

function TSynGutterLOvProviderBookmarks.CreateGutterMark(ASynMark: TSynEditMark): TSynGutterLOvMark;
var
  c: TColor;
  p: Integer;
begin
  Result := TSynGutterLOvMark.Create;
  Result.FData     := ASynMark;
  Result.FLine     := ASynMark.Line;
  Result.FColumn   := ASynMark.Column;
  c := Color;
  p := Priority;
  AdjustColorForMark(ASynMark, c, p);
  Result.FPriority := p;
  Result.FColor    := c;
  FMarkList.Add(Result);
  GutterPart.AddMark(Result);
end;

function TSynGutterLOvProviderBookmarks.IndexOfSynMark(ASynMark: TSynEditMark): Integer;
begin
  Result := FMarkList.Count - 1;
  while  Result >= 0 do begin;
    if FMarkList[Result].FData = Pointer(ASynMark) then exit;
    dec(Result);

  end;
end;

procedure TSynGutterLOvProviderBookmarks.BufferChanging(Sender: TObject);
begin
  TCustomSynEdit(SynEdit).Marks.UnRegisterChangeHandler(@DoMarkChange);
end;

procedure TSynGutterLOvProviderBookmarks.BufferChanged(Sender: TObject);
var
  i: Integer;
begin
  TSynEditStringList(Sender).RemoveHanlders(self);
  TSynEditStringList(TextBuffer).AddNotifyHandler(senrTextBufferChanging, @BufferChanging);
  TSynEditStringList(TextBuffer).AddNotifyHandler(senrTextBufferChanged, @BufferChanged);
  TCustomSynEdit(SynEdit).Marks.RegisterChangeHandler(@DoMarkChange,
    [smcrAdded, smcrRemoved, smcrLine, smcrVisible, smcrChanged]);

  while FMarkList.Count > 0 do begin
    FMarkList[0].Destroy;
    FMarkList.Delete(0);
  end;

  for i := 0 to TCustomSynEdit(SynEdit).Marks.Count - 1 do
    if TCustomSynEdit(SynEdit).Marks[i].Visible then
      CreateGutterMark(TCustomSynEdit(SynEdit).Marks[i]);
end;

constructor TSynGutterLOvProviderBookmarks.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  Color := clBlue;
  TSynEditStringList(TextBuffer).AddNotifyHandler(senrTextBufferChanging, @BufferChanging);
  TSynEditStringList(TextBuffer).AddNotifyHandler(senrTextBufferChanged, @BufferChanged);

  TCustomSynEdit(SynEdit).Marks.RegisterChangeHandler(@DoMarkChange,
    [smcrAdded, smcrRemoved, smcrLine, smcrVisible, smcrChanged]);

  for i := 0 to TCustomSynEdit(SynEdit).Marks.Count - 1 do
    if TCustomSynEdit(SynEdit).Marks[i].Visible then
      CreateGutterMark(TCustomSynEdit(SynEdit).Marks[i]);
end;

destructor TSynGutterLOvProviderBookmarks.Destroy;
begin
  TSynEditStringList(TextBuffer).RemoveHanlders(self);
  TCustomSynEdit(SynEdit).Marks.UnRegisterChangeHandler(@DoMarkChange);
  inherited Destroy;
end;

{ TSynChildWinControl }

procedure TSynChildWinControl.WMNCHitTest(var Message: TLMessage);
begin
  Message.Result := HTTRANSPARENT;
end;

constructor TSynChildWinControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
end;

{ TSynGutterLineOverview }

procedure TSynGutterLineOverview.Init;
begin
  inherited Init;
  TSynEditStringList(TextBuffer).AddChangeHandler(senrLineCount, @LineCountChanged);
  TSynEditStringList(TextBuffer).AddNotifyHandler(senrTextBufferChanged, @BufferChanged);
  FWinControl := TSynChildWinControl.Create(Self);
  FWinControl.Parent := SynEdit;
  FWinControl.OnPaint := @PaintWinControl;

  FLineMarks := TSynGutterLOvLineMarksList.Create;
  FProviders := TSynGutterLineOverviewProviderList.Create(Self);
  MarkupInfo.Background := $E0E0E0;

  DoResize(Self);
  LineCountchanged(nil, 0, 0);
end;

destructor TSynGutterLineOverview.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  TSynEditStringList(TextBuffer).RemoveHanlders(self);
  FreeAndNil(FProviders);
  FreeAndNil(FWinControl);
  FreeAndNil(FLineMarks);
  FreeAndNil(FMouseActionsForMarks);
  inherited Destroy;
end;

procedure TSynGutterLineOverview.LineCountChanged(Sender: TSynEditStrings; AIndex,
  ACount: Integer);
begin
  FLineMarks.TextLineCount := TextBuffer.Count;
  if (not SynEdit.HandleAllocated) or (not Self.Visible) then exit;
  ScheduleASync([losLineCountChanged]);
end;

procedure TSynGutterLineOverview.BufferChanged(Sender: TObject);
begin
  TSynEditStringList(Sender).RemoveHanlders(self);
  TSynEditStringList(TextBuffer).AddChangeHandler(senrLineCount, @LineCountChanged);
  TSynEditStringList(TextBuffer).AddNotifyHandler(senrTextBufferChanged, @BufferChanged);
  LineCountChanged(nil, 0, 0);
end;

procedure TSynGutterLineOverview.SetVisible(const AValue: boolean);
begin
  inherited SetVisible(AValue);
  FWinControl.Visible := Visible and Gutter.Visible;
  if FWinControl.Visible then
    ScheduleASync([losResized, losLineCountChanged]);
end;

procedure TSynGutterLineOverview.GutterVisibilityChanged;
begin
  inherited GutterVisibilityChanged;
  FWinControl.Visible := Visible and Gutter.Visible;
  if FWinControl.Visible then
    ScheduleASync([losResized, losLineCountChanged]);
end;

procedure TSynGutterLineOverview.DoChange(Sender: TObject);
begin
  inherited;
  FWinControl.Invalidate;
end;

procedure TSynGutterLineOverview.InvalidateTextLines(AFromLine, AToLine: Integer);
begin
  InvalidatePixelLines(TextLineToPixel(AFromLine), TextLineToPixelEnd(AToLine));
end;

procedure TSynGutterLineOverview.InvalidatePixelLines(AFromLine, AToLine: Integer);
var
  r: TRect;
begin
  if not SynEdit.HandleAllocated then exit;
  r := Rect(0, Top, Width, Top + Height);
  r.Top := AFromLine;
  r.Bottom := AToLine + 1;
  InvalidateRect(FWinControl.Handle, @r, False);
end;

function TSynGutterLineOverview.PixelLineToText(ALineIdx: Integer): Integer;
var
  c: Integer;
begin
  if ALineIdx < 0 then exit(-1);
  c := TextBuffer.Count;
  if c = 0 then
    Result := -1
  else
    Result := Min(Int64(ALineIdx) * Int64(c) div Height, c-1);
end;

function TSynGutterLineOverview.TextLineToPixel(ALine: Integer): Integer;
var
  c: Integer;
begin
  if ALine < 0 then exit(-1);
  c := Max(1, TextBuffer.Count);
  if c = 0 then
    Result := -1
  else
    Result := Int64(ALine - 1) * Int64(Height) div c;
end;

function TSynGutterLineOverview.TextLineToPixelEnd(ALine: Integer): Integer;
var
  c, n: Integer;
begin
  if ALine < 0 then exit(-1);
  c := Max(1, TextBuffer.Count);
  if c = 0 then
    Result := -1
  else begin
    Result := (Int64(ALine - 1) * Int64(Height)) div c;
    n      := (Int64(ALine)     * Int64(Height)) div c - 1; // next line - 1 pix
    if n > Result then
      Result := n;
  end;
end;

procedure TSynGutterLineOverview.DoResize(Sender: TObject);
var
  i: Integer;
begin
  inherited DoResize(Sender);
  if (not SynEdit.HandleAllocated) or (not Self.Visible) then exit;
  FWinControl.BoundsRect := Bounds(Left,Top,Width,Height);

  {$IFDEF DARWIN}
  FLineMarks.PixelHeight := Height;
  for i := 0 to FProviders.Count - 1 do
    FProviders[i].Height := Height;
  FWinControl.Invalidate;
  {$ELSE}
  ScheduleASync([losResized]); // May only be executed after mouse up
  if not (losWaitForPaint in FState) then begin
    FLineMarks.PixelHeight := Height;
    for i := 0 to FProviders.Count - 1 do
      FProviders[i].Height := Height;
    FWinControl.Invalidate;
    FState := FState + [losWaitForPaint];
  end;
  {$ENDIF}
end;

procedure TSynGutterLineOverview.PaintWinControl(Sender: TObject);
var
  i: Integer;
  AClip: TRect;
begin
  FState := FState - [losWaitForPaint];
  if not Visible then exit;
  AClip := FWinControl.Canvas.ClipRect;
  AClip.Left := 0;
  AClip.Right := Width;
  FWinControl.Canvas.Brush.Color := MarkupInfo.Background;
  FWinControl.Canvas.FillRect(AClip);
  FWinControl.Canvas.Pen.Width := FPpiPenWidth;
  FWinControl.Canvas.Pen.JoinStyle := pjsMiter;

  for i := 0 to Providers.Count - 1 do
    Providers[i].Paint(FWinControl.Canvas, AClip, 0);

  for i := 0 to FLineMarks.Count - 1 do
    FLineMarks[i].Paint(FWinControl.Canvas, AClip, 0, MarkHeight);
end;

constructor TSynGutterLineOverview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPpiPenWidth := 1;
  FMouseActionsForMarks := TSynEditMouseInternalActions.Create(Self);
end;

function TSynGutterLineOverview.GetMarkHeight: Integer;
begin
  Result := FLineMarks.ItemHeight;
end;

function TSynGutterLineOverview.GetMouseActionsForMarks: TSynEditMouseActions;
begin
  Result := FMouseActionsForMarks.UserActions;
end;

procedure TSynGutterLineOverview.SetMarkHeight(const AValue: Integer);
begin
  FLineMarks.ItemHeight := AValue;
end;

procedure TSynGutterLineOverview.ScheduleASync(AStates: TSynGutterLOvStateFlags);
begin
  if not (losASyncScheduled in FState) then
    Application.QueueAsyncCall(@ExecASync, 0);
  FState := FState + [losASyncScheduled] + AStates;
end;

procedure TSynGutterLineOverview.ExecASync(Data: PtrInt);
var
  i: Integer;
begin
  if losLineCountChanged in FState then begin
    for i := 0 to FProviders.Count - 1 do
      FProviders[i].ReCalc;
    FLineMarks.ReBuild;
  end;

  if losResized in FState then begin
    FLineMarks.PixelHeight := Height;

    for i := 0 to FProviders.Count - 1 do
      FProviders[i].Height := Height;
  end;

  FWinControl.Invalidate;
  FState := FState - [losASyncScheduled, losResized, losLineCountChanged];
end;

procedure TSynGutterLineOverview.SetMouseActionsForMarks(
  AValue: TSynEditMouseActions);
begin
  FMouseActionsForMarks.UserActions := AValue;
end;

function TSynGutterLineOverview.PreferedWidth: Integer;
begin
  Result := 10;
end;

procedure TSynGutterLineOverview.ScalePPI(const AScaleFactor: Double);
begin
  FLineMarks.ItemHeight := Round(FLineMarks.ItemHeight*AScaleFactor);
  FPpiPenWidth := Max(1, Scale96ToFont(1));
  inherited ScalePPI(AScaleFactor);
end;

procedure TSynGutterLineOverview.Assign(Source : TPersistent);
begin
  if Source is TSynGutterLineOverview then
  begin
    inherited;
    // Todo: assign providerlist?
  end;
  inherited;
end;

procedure TSynGutterLineOverview.Paint(Canvas : TCanvas; AClip : TRect; FirstLine, LastLine : integer);
begin
  // do nothing
end;

procedure TSynGutterLineOverview.AddMark(AMark: TSynGutterLOvMark);
begin
  FLineMarks.AddMark(AMark);
end;

function TSynGutterLineOverview.MaybeHandleMouseAction(
  var AnInfo: TSynEditMouseActionInfo;
  HandleActionProc: TSynEditMouseActionHandler): Boolean;
begin
  Result := False;
  if FLineMarks.IndexForLine(AnInfo.MouseY, False, True) >= 0 then
    Result := HandleActionProc(FMouseActionsForMarks.GetActionsForOptions(TCustomSynEdit(SynEdit).MouseOptions), AnInfo);

  if not Result then
    Result := inherited MaybeHandleMouseAction(AnInfo, HandleActionProc);
end;

function TSynGutterLineOverview.DoHandleMouseAction(
  AnAction: TSynEditMouseAction; var AnInfo: TSynEditMouseActionInfo): Boolean;
var
  i, TextLine: Integer;
begin
  Result := False;
  if AnAction = nil then exit;

  case AnAction.Command of
    emcOverViewGutterScrollTo: begin
      TextLine := PixelLineToText(AnInfo.MouseY);

      AnInfo.NewCaret.BytePos := 1;
      Result := True;
    end;
    emcOverViewGutterGotoMark: begin
      i := FLineMarks.IndexForLine(AnInfo.MouseY, False, True);
      if (i < 0) or (FLineMarks.Items[i].Count = 0) then
        exit;
      TextLine := FLineMarks.Items[i].Items[0].Line;
      AnInfo.NewCaret.BytePos := Max(FLineMarks.Items[i].Items[0].Column, 1);
      Result := True;
    end;
  end;

  if Result then begin
    if (TextLine < TCustomSynEdit(SynEdit).TopLine) or
       (TextLine > TCustomSynEdit(SynEdit).TopLine + TCustomSynEdit(SynEdit).LinesInWindow)
    then
      TCustomSynEdit(SynEdit).TopLine := Max(1, TextLine - TCustomSynEdit(SynEdit).LinesInWindow div 2);

    AnInfo.NewCaret.LinePos := TextLine;
  end;
end;

end.

