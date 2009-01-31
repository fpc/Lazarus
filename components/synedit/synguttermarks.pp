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

procedure TSynGutterMarks.Paint(Canvas : TCanvas; AClip : TRect; FirstLine, LastLine : integer);
var
  i: integer;
  bHasOtherMarks: boolean;
  aGutterOffs: PIntArray;
  dc: HDC;
  LineHeight: Integer;

  procedure DrawMark(iMark: integer);
  var
    iLine: integer;
    itop : Longint;
    CurMark: TSynEditMark;
  begin
    iTop := 0;
    CurMark := TSynEdit(SynEdit).Marks[iMark];
    if (CurMark.Line<1) or (CurMark.Line>TSynEdit(SynEdit).Lines.Count) then exit;
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
                           iTop + iLine * LineHeight, CurMark.ImageIndex,true);

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
  if FBookMarkOpt.GlyphsVisible and (TSynEdit(SynEdit).Marks.Count > 0)
    and (LastLine >= FirstLine)
  then begin
    aGutterOffs := AllocMem((LastLine+1{$IFNDEF SYN_LAZARUS}-TopLine{$ENDIF}) * SizeOf(integer));
    try
      // Instead of making a two pass loop we look while drawing the bookmarks
      // whether there is any other mark to be drawn
      bHasOtherMarks := FALSE;
      for i := 0 to TSynEdit(SynEdit).Marks.Count - 1 do with TSynEdit(SynEdit).Marks[i] do
        {$IFDEF SYN_LAZARUS}
        if Visible and (Line >= FFoldView.TextIndex[FirstLine]+1) and (Line <= FFoldView.TextIndex[LastLine]+1) then
        {$ELSE}
        if Visible and (Line >= FirstLine) and (Line <= LastLine) then
        {$ENDIF}
        begin
          if IsBookmark <> fBookMarkOpt.DrawBookmarksFirst then              //mh 2000-10-12
            bHasOtherMarks := TRUE
          else
            DrawMark(i);
        end;
      if bHasOtherMarks then
        for i := 0 to TSynEdit(SynEdit).Marks.Count - 1 do with TSynEdit(SynEdit).Marks[i] do
        begin
          if Visible and (IsBookmark <> FBookMarkOpt.DrawBookmarksFirst)     //mh 2000-10-12
            {$IFDEF SYN_LAZARUS}
            and (Line >= FFoldView.TextIndex[FirstLine]+1) and (Line <= FFoldView.TextIndex[LastLine]+1)
            {$ELSE}
            and (Line >= FirstLine) and (Line <= LastLine)
            {$ENDIF}
          then
            DrawMark(i);
        end;
    finally
      FreeMem(aGutterOffs);
    end;
  end;
end;

end.

