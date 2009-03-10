unit SynGutterMarks;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, Graphics, LCLType, LCLIntf, SynGutterBase,
  SynEditMiscClasses, SynEditMiscProcs, SynEditFoldedView, SynEditMarks;

type

  { TSynGutterMarks }

  TSynGutterMarks = class(TSynGutterPartBase)
  private
    FFoldView: TSynEditFoldedView;
    FBookMarkOpt: TSynBookMarkOpt;
    FInternalImage: TSynInternalImage;
  protected
    procedure DoChange(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
      override;
    function RealGutterWidth(CharWidth: integer): integer;  override;
  end;

implementation
uses
  SynEdit;


{ TSynGutterMarks }

procedure TSynGutterMarks.DoChange(Sender: TObject);
begin
  if AutoSize then
    FWidth := 22;
  inherited DoChange(Sender);
end;

constructor TSynGutterMarks.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFoldView := Gutter.FoldView;
  FBookMarkOpt := TSynEdit(SynEdit).BookMarkOptions;
  FInternalImage := nil;

  FWidth := 22;
end;

destructor TSynGutterMarks.Destroy;
begin
  FreeAndNil(fInternalImage);
  inherited Destroy;
end;

function TSynGutterMarks.RealGutterWidth(CharWidth : integer) : integer;
begin
  If Visible then
    Result := Width
  else
    Result := 0;
end;

function DoMarksCompareBookmarksFirst(Item1, Item2: Pointer): Integer;
var
  Mark1: TSynEditMark absolute Item1;
  Mark2: TSynEditMark absolute Item2;
begin
  Result := 0;
  if Mark1 = Mark2 then Exit;

  if Mark1.IsBookmark then
    Result := -1
  else
  if Mark2.IsBookmark then
    Result := 1
  else
  if Mark1.Priority < Mark2.Priority then
    Result := 1
  else
  if Mark1.Priority > Mark2.Priority then
    Result := -1;
end;

function DoMarksCompareBookmarksLast(Item1, Item2: Pointer): Integer;
var
  Mark1: TSynEditMark absolute Item1;
  Mark2: TSynEditMark absolute Item2;
begin
  Result := 0;
  if Mark1 = Mark2 then Exit;

  if Mark1.IsBookmark then
    Result := 1
  else
  if Mark2.IsBookmark then
    Result := -1
  else
  if Mark1.Priority < Mark2.Priority then
    Result := 1
  else
  if Mark1.Priority > Mark2.Priority then
    Result := -1;
end;

procedure TSynGutterMarks.Paint(Canvas : TCanvas; AClip : TRect; FirstLine, LastLine : integer);
var
  i: integer;
  bHasOtherMarks: boolean;
  aGutterOffs: PIntArray;
  dc: HDC;
  LineHeight: Integer;
  LineMarks: TList;

  procedure DrawMark(CurMark: TSynEditMark);
  var
    iLine: integer;
    itop : Longint;
  begin
    iTop := 0;
    if (CurMark.Line<1) or (CurMark.Line > SynEdit.Lines.Count) then exit;
    if FFoldView.FoldedAtTextIndex[CurMark.Line-1] then exit;
    iLine := FFoldView.TextIndexToScreenLine(CurMark.Line-1);

    if Assigned(fBookMarkOpt.BookmarkImages) and not CurMark.InternalImage
    then begin
      if (CurMark.ImageIndex <= FBookMarkOpt.BookmarkImages.Count) then begin
        if CurMark.IsBookmark = FBookMarkOpt.DrawBookmarksFirst then
          aGutterOffs^[iLine] := AClip.Left
        else if aGutterOffs^[iLine] = 0 then
          aGutterOffs^[iLine] := fBookMarkOpt.BookmarkImages.Width + AClip.Left;
        if LineHeight > fBookMarkOpt.BookmarkImages.Height then
          iTop := (LineHeight - fBookMarkOpt.BookmarkImages.Height) div 2;
        with fBookMarkOpt do
          BookmarkImages.Draw(Canvas, LeftMargin + aGutterOffs^[iLine],
                           iTop + iLine * LineHeight, CurMark.ImageIndex, true);

        Inc(aGutterOffs^[iLine], fBookMarkOpt.BookmarkImages.Width);
      end;
    end else
    begin
      if CurMark.ImageIndex in [0..9] then begin
        if not Assigned(fInternalImage) then begin
          fInternalImage := TSynInternalImage.Create('SynEditInternalImages',10);
        end;
        if aGutterOffs^[iLine] = 0 then
          aGutterOffs^[iLine] := AClip.Left;
        fInternalImage.DrawMark(Canvas, CurMark.ImageIndex,
            fBookMarkOpt.LeftMargin + aGutterOffs^[iLine], iLine * LineHeight,
            LineHeight);
        Inc(aGutterOffs^[iLine], fBookMarkOpt.Xoffset);
      end;
    end;
  end;

begin
  if not Visible then exit;
  LineHeight := TSynEdit(SynEdit).LineHeight;
  if MarkupInfo.Background <> clNone then
    Canvas.Brush.Color := MarkupInfo.Background
  else
    Canvas.Brush.Color := Gutter.Color;
  dc := Canvas.Handle;
  {$IFDEF SYN_LAZARUS}
  LCLIntf.SetBkColor(dc,Canvas.Brush.Color);
  {$ENDIF}


  // now the gutter marks
  if FBookMarkOpt.GlyphsVisible and (TSynEdit(SynEdit).Marks.Count > 0) and (LastLine >= FirstLine) then
  begin
    aGutterOffs := AllocMem((LastLine+1{$IFNDEF SYN_LAZARUS}-TopLine{$ENDIF}) * SizeOf(integer));
    try
      LineMarks := TList.Create;
      try
        for i := 0 to TSynEdit(SynEdit).Marks.Count - 1 do with TSynEdit(SynEdit).Marks[i] do
          if Visible and (Line >= FFoldView.TextIndex[FirstLine] + 1) and (Line <= FFoldView.TextIndex[LastLine] + 1) then
            LineMarks.Add(TSynEdit(SynEdit).Marks[i]);
        if fBookMarkOpt.DrawBookmarksFirst then
          LineMarks.Sort(@DoMarksCompareBookmarksFirst)
        else
          LineMarks.Sort(@DoMarksCompareBookmarksLast);
        for i := 0 to LineMarks.Count - 1 do
          DrawMark(TSynEditMark(LineMarks[i]));
      finally
        LineMarks.Free;
      end;
    finally
      FreeMem(aGutterOffs);
    end;
  end;
end;

end.

