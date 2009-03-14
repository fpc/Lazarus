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
    FDebugMarksImageIndex: Integer;
    FFoldView: TSynEditFoldedView;
    FBookMarkOpt: TSynBookMarkOpt;
    FInternalImage: TSynInternalImage;
  protected
    procedure DoChange(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer); override;
    function RealGutterWidth(CharWidth: integer): integer;  override;
    property DebugMarksImageIndex: Integer read FDebugMarksImageIndex write FDebugMarksImageIndex;
  end;

implementation
uses
  SynEdit;


{ TSynGutterMarks }

procedure TSynGutterMarks.DoChange(Sender: TObject);
begin
  if AutoSize then
    FWidth := RealGutterWidth(0);
  inherited DoChange(Sender);
end;

constructor TSynGutterMarks.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFoldView := Gutter.FoldView;
  FBookMarkOpt := TSynEdit(SynEdit).BookMarkOptions;
  FInternalImage := nil;
  FDebugMarksImageIndex := -1;

  FWidth := 23;
end;

destructor TSynGutterMarks.Destroy;
begin
  FreeAndNil(FInternalImage);
  inherited Destroy;
end;

function TSynGutterMarks.RealGutterWidth(CharWidth: integer) : integer;
begin
  if Visible then
    Result := 22 + FBookMarkOpt.LeftMargin
  else
    Result := 0;
end;

procedure TSynGutterMarks.Paint(Canvas : TCanvas; AClip : TRect; FirstLine, LastLine : integer);
var
  i, j, iLine: integer;
  aGutterOffs: PIntArray;
  dc: HDC;
  LineHeight: Integer;
  Marks: TSynEditMarks;
  HasAnyMark: Boolean;

  procedure DrawMark(CurMark: TSynEditMark);
  var
    iLine: integer;
    itop : Longint;
  begin
    iTop := 0;
    if (CurMark.Line<1) or (CurMark.Line > SynEdit.Lines.Count) then exit;
    if FFoldView.FoldedAtTextIndex[CurMark.Line-1] then exit;
    iLine := FFoldView.TextIndexToScreenLine(CurMark.Line-1);
    if iLine < 0 then Exit;

    if Assigned(FBookMarkOpt.BookmarkImages) and not CurMark.InternalImage then
    begin
      if (CurMark.ImageIndex <= FBookMarkOpt.BookmarkImages.Count) and
         (CurMark.ImageIndex >= 0) then
      begin
        if CurMark.IsBookmark = FBookMarkOpt.DrawBookmarksFirst then
          aGutterOffs^[iLine] := AClip.Left
        else
        if aGutterOffs^[iLine] = 0 then
          aGutterOffs^[iLine] := FBookMarkOpt.BookmarkImages.Width + AClip.Left;
        if LineHeight > FBookMarkOpt.BookmarkImages.Height then
          iTop := (LineHeight - FBookMarkOpt.BookmarkImages.Height) div 2;
        with FBookMarkOpt do
          BookmarkImages.Draw(Canvas, LeftMargin + aGutterOffs^[iLine],
                              iTop + iLine * LineHeight, CurMark.ImageIndex, True);

        Inc(aGutterOffs^[iLine], FBookMarkOpt.BookmarkImages.Width);
      end;
    end
    else
    begin
      if CurMark.ImageIndex in [0..9] then
      begin
        if not Assigned(FInternalImage) then
          FInternalImage := TSynInternalImage.Create('SynEditInternalImages',10);
        if aGutterOffs^[iLine] = 0 then
          aGutterOffs^[iLine] := AClip.Left;
        FInternalImage.DrawMark(Canvas, CurMark.ImageIndex,
            FBookMarkOpt.LeftMargin + aGutterOffs^[iLine], iLine * LineHeight,
            LineHeight);
        Inc(aGutterOffs^[iLine], FBookMarkOpt.Xoffset);
      end;
    end;
  end;

  procedure DrawDebugMark(Line: Integer);
  var
    itop : Longint;
  begin
    iTop := 0;
    if Line < 0 then Exit;
    if Assigned(FBookMarkOpt.BookmarkImages) and
       (DebugMarksImageIndex <= FBookMarkOpt.BookmarkImages.Count) and
       (DebugMarksImageIndex >= 0) then
    begin
      if not FBookMarkOpt.DrawBookmarksFirst then
        aGutterOffs^[Line] := AClip.Left
      else
      if aGutterOffs^[Line] = 0 then
        aGutterOffs^[Line] := FBookMarkOpt.BookmarkImages.Width + AClip.Left;
      if LineHeight > FBookMarkOpt.BookmarkImages.Height then
        iTop := (LineHeight - FBookMarkOpt.BookmarkImages.Height) div 2;
      with FBookMarkOpt do
        BookmarkImages.Draw(Canvas, LeftMargin + aGutterOffs^[Line],
                            iTop + Line * LineHeight, DebugMarksImageIndex, True);

      Inc(aGutterOffs^[Line], FBookMarkOpt.BookmarkImages.Width);
    end
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
  if FBookMarkOpt.GlyphsVisible and (LastLine >= FirstLine) then
  begin
    aGutterOffs := AllocMem((LastLine + 1) * SizeOf(integer));
    try
      for i := FirstLine to LastLine do
      begin
        iLine := FFoldView.TextIndex[i] + 1;
        TSynEdit(SynEdit).Marks.GetMarksForLine(iLine, Marks);
        HasAnyMark := False;
        for j := Low(Marks) to High(Marks) do
        begin
          if Marks[j] = nil then
            break;
          if not Marks[j].Visible then
            continue;
          DrawMark(Marks[j]);
          HasAnyMark := HasAnyMark or not Marks[j].IsBookmark;
        end;
        if not HasAnyMark and TSynEdit(SynEdit).HasDebugMark(iLine) then
          DrawDebugMark(i);
      end;
    finally
      FreeMem(aGutterOffs);
    end;
  end;
end;

end.

