unit SynColorAttribEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, StdCtrls, sysutils, ExtCtrls, Graphics, GraphUtil,
  ColorBox, LCLProc, LCLType, LCLIntf, Dialogs, Menus, Forms, Spin, SynEdit,
  SynGutterCodeFolding, SynEditTypes,
  SynEditMouseCmds, SynEditHighlighter, SynTextDrawer,
  EditorOptions, IDEOptionsIntf,
  editor_general_options, IDEImagesIntf, LazarusIDEStrConsts, IDEProcs, typinfo,
  LazConf, SourceMarks, types, math, FPCanvas;

type

  { TSynColorAttrEditor }

  TSynColorAttrEditor = class(TFrame)
    BackPriorLabel: TLabel;
    BackPriorSpin: TSpinEdit;
    BackGroundColorBox: TColorBox;
    BackGroundLabel: TLabel;
    ColumnPosBevel: TPanel;
    ForePriorLabel: TLabel;
    ForePriorSpin: TSpinEdit;
    FramePriorSpin: TSpinEdit;
    FramePriorLabel: TLabel;
    FrameStyleBox: TComboBox;
    FrameEdgesBox: TComboBox;
    FrameColorBox: TColorBox;
    BackGroundUseDefaultCheckBox: TCheckBox;
    FrameColorUseDefaultCheckBox: TCheckBox;
    ForegroundColorBox: TColorBox;
    ForeAlphaLabel: TLabel;
    BackAlphaLabel: TLabel;
    FrameAlphaLabel: TLabel;
    pnlUnderline: TPanel;
    pnlBold: TPanel;
    pnlItalic: TPanel;
    ForeAlphaSpin: TSpinEdit;
    BackAlphaSpin: TSpinEdit;
    FrameAlphaSpin: TSpinEdit;
    TextBoldCheckBox: TCheckBox;
    TextBoldRadioInvert: TRadioButton;
    TextBoldRadioOff: TRadioButton;
    TextBoldRadioOn: TRadioButton;
    TextBoldRadioPanel: TPanel;
    TextItalicCheckBox: TCheckBox;
    TextItalicRadioInvert: TRadioButton;
    TextItalicRadioOff: TRadioButton;
    TextItalicRadioOn: TRadioButton;
    TextItalicRadioPanel: TPanel;
    TextUnderlineCheckBox: TCheckBox;
    TextUnderlineRadioInvert: TRadioButton;
    TextUnderlineRadioOff: TRadioButton;
    TextUnderlineRadioOn: TRadioButton;
    TextUnderlineRadioPanel: TPanel;
    ForeGroundLabel: TLabel;
    ForeGroundUseDefaultCheckBox: TCheckBox;
    procedure ForeAlphaSpinChange(Sender: TObject);
    procedure ForeAlphaSpinEnter(Sender: TObject);
    procedure ForegroundColorBoxChange(Sender: TObject);
    procedure ForegroundColorBoxGetColors(Sender: TCustomColorBox; Items: TStrings);
    procedure ForePriorSpinChange(Sender: TObject);
    procedure FrameEdgesBoxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      {%H-}State: TOwnerDrawState);
    procedure FrameStyleBoxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      {%H-}State: TOwnerDrawState);
    procedure GeneralCheckBoxOnChange(Sender: TObject);
    procedure pnlElementAttributesResize(Sender: TObject);
    procedure TextStyleRadioOnChange(Sender: TObject);
  private
    { private declarations }
    FCurHighlightElement: TColorSchemeAttribute;
    FCurrentColorScheme: TColorSchemeLanguage;
    FOnChanged: TNotifyEvent;
    FShowPrior: Boolean;
    UpdatingColor: Boolean;

    procedure SetCurHighlightElement(AValue: TColorSchemeAttribute);
    procedure DoChanged;
    procedure SetShowPrior(AValue: Boolean);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure Setup;
    procedure UpdateAll;
    // CurrentColorScheme must be set before CurHighlightElement
    property CurHighlightElement: TColorSchemeAttribute read FCurHighlightElement write SetCurHighlightElement;
    property CurrentColorScheme: TColorSchemeLanguage read FCurrentColorScheme write FCurrentColorScheme;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property ShowPrior: Boolean read FShowPrior write SetShowPrior;
  end;

implementation

{$R *.lfm}

function DefaultToNone(AColor: TColor): TColor;
begin
  if AColor = clDefault then
    Result := clNone
  else
    Result := AColor;
end;

function NoneToDefault(AColor: TColor): TColor;
begin
  if AColor = clNone then
    Result := clDefault
  else
    Result := AColor;
end;

{ TSynColorAttrEditor }

procedure TSynColorAttrEditor.ForegroundColorBoxChange(Sender: TObject);
begin
  if (FCurHighlightElement = nil) or UpdatingColor then
    exit;
  UpdatingColor := True;

  if Sender = ForegroundColorBox then
  begin
    FCurHighlightElement.Foreground := DefaultToNone(ForeGroundColorBox.Selected);
    ForeGroundUseDefaultCheckBox.Checked := ForeGroundColorBox.Selected <> clDefault;
  end;
  if Sender = BackGroundColorBox then
  begin
    FCurHighlightElement.Background := DefaultToNone(BackGroundColorBox.Selected);
    BackGroundUseDefaultCheckBox.Checked := BackGroundColorBox.Selected <> clDefault;
  end;
  if Sender = FrameColorBox then
  begin
    FCurHighlightElement.FrameColor := DefaultToNone(FrameColorBox.Selected);
    FrameColorUseDefaultCheckBox.Checked := FrameColorBox.Selected <> clDefault;
    FrameEdgesBox.Enabled := FrameColorBox.Selected <> clDefault;
    FrameStyleBox.Enabled := FrameColorBox.Selected <> clDefault;
  end;
  if Sender = FrameEdgesBox then
  begin
    FCurHighlightElement.FrameEdges := TSynFrameEdges(FrameEdgesBox.ItemIndex);
  end;
  if Sender = FrameStyleBox then
  begin
    FCurHighlightElement.FrameStyle := TSynLineStyle(FrameStyleBox.ItemIndex);
  end;

  UpdatingColor := False;
  DoChanged;
end;

procedure TSynColorAttrEditor.ForeAlphaSpinChange(Sender: TObject);
var
  v: Integer;
begin
  if UpdatingColor then
    exit;

  UpdatingColor := True;
  v := TSpinEdit(Sender).Value;
  if (v = 256) and (Caption <> dlgEdOff) then TSpinEdit(Sender).Caption := dlgEdOff;
  UpdatingColor := False;

  if (FCurHighlightElement = nil) then
    exit;

  if v = 256 then v := 0;

  if Sender = ForeAlphaSpin then
    FCurHighlightElement.ForeAlpha := v;
  if Sender = BackAlphaSpin then
    FCurHighlightElement.BackAlpha := v;
  if Sender = FrameAlphaSpin then
    FCurHighlightElement.FrameAlpha := v;

  DoChanged;
end;

procedure TSynColorAttrEditor.ForeAlphaSpinEnter(Sender: TObject);
begin
  UpdatingColor := True;
  If TSpinEdit(Sender).Value = 256 then
    TSpinEdit(Sender).Caption := '256';
  UpdatingColor := False;
end;

procedure TSynColorAttrEditor.ForegroundColorBoxGetColors(Sender: TCustomColorBox; Items: TStrings);
var
  i: longint;
begin
  i := Items.IndexOfObject(TObject(PtrInt(clDefault)));
  if i >= 0 then begin
    Items[i] := dlgColorNotModified;
    Items.Move(i, 1);
  end;
end;

procedure TSynColorAttrEditor.ForePriorSpinChange(Sender: TObject);
var
  v: Integer;
begin
  if (FCurHighlightElement = nil) then
    exit;

  v := TSpinEdit(Sender).Value;

  if Sender = ForePriorSpin then
    FCurHighlightElement.ForePriority := v;
  if Sender = BackPriorSpin then
    FCurHighlightElement.BackPriority := v;
  if Sender = FramePriorLabel then
    FCurHighlightElement.FramePriority := v;

  DoChanged;
end;

procedure TSynColorAttrEditor.FrameEdgesBoxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
var
  r: TRect;
  PCol: Integer;
begin
  if Index  < 0 then exit;;

  r.top := ARect.top + 3;
  r.bottom := ARect.bottom - 3;
  r.left := ARect.left + 5;
  r.right := ARect.Right - 5;

  with TCustomComboBox(Control).Canvas do
  begin
    FillRect(ARect);
    Pen.Width := 1;
    PCol := pen.Color;
    Pen.Color := clGray;
    Pen.Style := psDot;
    Pen.EndCap := pecFlat;
    Rectangle(r);
    Pen.Width := 2;
    pen.Color := PCol;
    Pen.Style := psSolid;
    case Index of
      ord(sfeAround): Rectangle(r);
      ord(sfeBottom): begin
          MoveTo(r.Left, r.Bottom);
          LineTo(r.Right-1, r.Bottom);
        end;
      ord(sfeLeft): begin
          MoveTo(r.Left, r.Top);
          LineTo(r.Left, r.Bottom-1);
        end;
    end;
  end;
end;

procedure TSynColorAttrEditor.FrameStyleBoxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
var
  p: TPoint;
begin
  if Index  < 0 then exit;;

  with TCustomComboBox(Control).Canvas do
  begin
    FillRect(ARect);
    Pen.Width := 2;
    pen.EndCap := pecFlat;
    case Index of
      0: Pen.Style := psSolid;
      1: Pen.Style := psDash;
      2: Pen.Style := psDot;
      3: Pen.Style := psSolid;
    end;
    if Index = 3 then begin
      MoveToEx(Handle, ARect.Left + 5, (ARect.Top + ARect.Bottom) div 2 - 2, @p);
      WaveTo(Handle, ARect.Right - 5, (ARect.Top + ARect.Bottom) div 2 - 2, 4);
    end else begin
      MoveTo(ARect.Left + 5, (ARect.Top + ARect.Bottom) div 2);
      LineTo(ARect.Right - 5, (ARect.Top + ARect.Bottom) div 2);
    end;
  end;
end;

procedure TSynColorAttrEditor.GeneralCheckBoxOnChange(Sender: TObject);
var
  TheColorBox: TColorBox;
begin
  if FCurHighlightElement = nil then
    exit;

  if UpdatingColor = False then begin
    UpdatingColor := True;

    TheColorBox := nil;
    if Sender = ForeGroundUseDefaultCheckBox then TheColorBox := ForegroundColorBox;
    if Sender = BackGroundUseDefaultCheckBox then TheColorBox := BackGroundColorBox;
    if Sender = FrameColorUseDefaultCheckBox then TheColorBox := FrameColorBox;
    if Assigned(TheColorBox) then begin
      if TCheckBox(Sender).Checked then begin
        TheColorBox.Selected := TheColorBox.Tag;
      end
      else begin
        TheColorBox.Tag := TheColorBox.Selected;
        TheColorBox.Selected := clDefault;
      end;

      if (Sender = ForeGroundUseDefaultCheckBox) and
         (DefaultToNone(ForegroundColorBox.Selected) <> FCurHighlightElement.Foreground)
      then begin
        FCurHighlightElement.Foreground := DefaultToNone(ForegroundColorBox.Selected);
        DoChanged;
      end;
      if (Sender = BackGroundUseDefaultCheckBox) and
         (DefaultToNone(BackGroundColorBox.Selected) <> FCurHighlightElement.Background)
      then begin
        FCurHighlightElement.Background := DefaultToNone(BackGroundColorBox.Selected);
        DoChanged;
      end;
      if (Sender = FrameColorUseDefaultCheckBox) and
         (DefaultToNone(FrameColorBox.Selected) <> FCurHighlightElement.FrameColor)
      then begin
        FCurHighlightElement.FrameColor := DefaultToNone(FrameColorBox.Selected);
        FrameEdgesBox.Enabled := TCheckBox(Sender).Checked;
        FrameStyleBox.Enabled := TCheckBox(Sender).Checked;
        DoChanged;
      end;
    end;

    UpdatingColor := False;
  end;

  if Sender = TextBoldCheckBox then begin
    if hafStyleMask in FCurHighlightElement.Features then
      TextStyleRadioOnChange(Sender)
    else
    if TextBoldCheckBox.Checked xor (fsBold in FCurHighlightElement.Style) then
    begin
      if TextBoldCheckBox.Checked then
        FCurHighlightElement.Style := FCurHighlightElement.Style + [fsBold]
      else
        FCurHighlightElement.Style := FCurHighlightElement.Style - [fsBold];
      DoChanged;
    end;
  end;

  if Sender = TextItalicCheckBox then begin
    if hafStyleMask in FCurHighlightElement.Features then
      TextStyleRadioOnChange(Sender)
    else
    if TextItalicCheckBox.Checked xor (fsItalic in FCurHighlightElement.Style) then
    begin
      if TextItalicCheckBox.Checked then
        FCurHighlightElement.Style := FCurHighlightElement.Style + [fsItalic]
      else
        FCurHighlightElement.Style := FCurHighlightElement.Style - [fsItalic];
      DoChanged;
    end;
  end;

  if Sender = TextUnderlineCheckBox then begin
    if hafStyleMask in FCurHighlightElement.Features then
      TextStyleRadioOnChange(Sender)
    else
    if TextUnderlineCheckBox.Checked xor (fsUnderline in FCurHighlightElement.Style) then
    begin
      if TextUnderlineCheckBox.Checked then
        FCurHighlightElement.Style := FCurHighlightElement.Style + [fsUnderline]
      else
        FCurHighlightElement.Style := FCurHighlightElement.Style - [fsUnderline];
      DoChanged;
    end;
  end;
end;

procedure TSynColorAttrEditor.pnlElementAttributesResize(Sender: TObject);
var
  MinAnchor: TControl;
  MinWidth: Integer;

  procedure CheckControl(Other: TControl);
  var w,h: Integer;
  begin
    if not Other.Visible then exit;
    h:=0;
    w:=0;
    Other.GetPreferredSize(w,h);
    if w <= MinWidth then exit;
    MinAnchor := Other;
    MinWidth := w;
  end;
begin
  MinWidth := -1;
  MinAnchor := ForeGroundLabel;
  CheckControl(ForeGroundLabel);
  CheckControl(BackGroundLabel);
  CheckControl(ForeGroundUseDefaultCheckBox);
  CheckControl(BackGroundUseDefaultCheckBox);
  CheckControl(FrameColorUseDefaultCheckBox);

  ColumnPosBevel.AnchorSide[akLeft].Control := MinAnchor;
  Constraints.MinHeight := pnlItalic.Top + pnlItalic.Height;
  Constraints.MinWidth := BackPriorSpin.Left + BackPriorSpin.Width;
end;

procedure TSynColorAttrEditor.TextStyleRadioOnChange(Sender: TObject);
  procedure CalcNewStyle(CheckBox: TCheckBox; RadioOn, RadioOff,
                         RadioInvert: TRadioButton; fs : TFontStyle;
                         Panel: TPanel);
  begin
    if CheckBox.Checked then
    begin
      Panel.Enabled := True;
      if RadioInvert.Checked then
      begin
        FCurHighlightElement.Style     := FCurHighlightElement.Style + [fs];
        FCurHighlightElement.StyleMask := FCurHighlightElement.StyleMask - [fs];
      end
      else
      if RadioOn.Checked then
      begin
        FCurHighlightElement.Style     := FCurHighlightElement.Style + [fs];
        FCurHighlightElement.StyleMask := FCurHighlightElement.StyleMask + [fs];
      end
      else
      if RadioOff.Checked then
      begin
        FCurHighlightElement.Style     := FCurHighlightElement.Style - [fs];
        FCurHighlightElement.StyleMask := FCurHighlightElement.StyleMask + [fs];
      end
    end
    else
    begin
      Panel.Enabled := False;
      FCurHighlightElement.Style     := FCurHighlightElement.Style - [fs];
      FCurHighlightElement.StyleMask := FCurHighlightElement.StyleMask - [fs];
    end;
  end;
begin
  if FCurHighlightElement = nil then
    exit;
  if UpdatingColor or not (hafStyleMask in FCurHighlightElement.Features) then
    Exit;

  if (Sender = TextBoldCheckBox) or
     (Sender = TextBoldRadioOn) or
     (Sender = TextBoldRadioOff) or
     (Sender = TextBoldRadioInvert) then
    CalcNewStyle(TextBoldCheckBox, TextBoldRadioOn, TextBoldRadioOff,
                    TextBoldRadioInvert, fsBold, TextBoldRadioPanel);

  if (Sender = TextItalicCheckBox) or
     (Sender = TextItalicRadioOn) or
     (Sender = TextItalicRadioOff) or
     (Sender = TextItalicRadioInvert) then
    CalcNewStyle(TextItalicCheckBox, TextItalicRadioOn, TextItalicRadioOff,
                    TextItalicRadioInvert, fsItalic, TextItalicRadioPanel);

  if (Sender = TextUnderlineCheckBox) or
     (Sender = TextUnderlineRadioOn) or
     (Sender = TextUnderlineRadioOff) or
     (Sender = TextUnderlineRadioInvert) then
    CalcNewStyle(TextUnderlineCheckBox, TextUnderlineRadioOn, TextUnderlineRadioOff,
                    TextUnderlineRadioInvert, fsUnderline, TextUnderlineRadioPanel);


  DoChanged;
end;

procedure TSynColorAttrEditor.UpdateAll;
begin
  if (FCurHighlightElement = nil) or UpdatingColor then
    Exit;
  UpdatingColor := True;
  DisableAlign;
  try
    // Adjust color captions
    ForeGroundUseDefaultCheckBox.Caption := dlgForecolor;
    FrameColorUseDefaultCheckBox.Caption := dlgFrameColor;
    if FCurrentColorScheme <> nil then begin
      if (FCurrentColorScheme.AttributeByEnum[ahaModifiedLine] <> nil) and
         (FCurHighlightElement.StoredName = FCurrentColorScheme.AttributeByEnum[ahaModifiedLine].StoredName)
      then begin
        ForeGroundUseDefaultCheckBox.Caption := dlgSavedLineColor;
        FrameColorUseDefaultCheckBox.Caption := dlgUnsavedLineColor;
      end else
      if (FCurrentColorScheme.AttributeByEnum[ahaCodeFoldingTree] <> nil) and
         (FCurHighlightElement.StoredName = FCurrentColorScheme.AttributeByEnum[ahaCodeFoldingTree].StoredName)
      then begin
        FrameColorUseDefaultCheckBox.Caption := dlgGutterCollapsedColor;
      end;
    end;

    if FCurHighlightElement.Group = agnDefault then begin
      ForegroundColorBox.Style := ForegroundColorBox.Style - [cbIncludeDefault];
      BackGroundColorBox.Style := BackGroundColorBox.Style - [cbIncludeDefault];
    end else begin
      ForegroundColorBox.Style := ForegroundColorBox.Style + [cbIncludeDefault];
      BackGroundColorBox.Style := BackGroundColorBox.Style + [cbIncludeDefault];
    end;

    // Forground
    ForeGroundLabel.Visible              := (hafForeColor in FCurHighlightElement.Features) and
                                            (FCurHighlightElement.Group = agnDefault);
    ForeGroundUseDefaultCheckBox.Visible := (hafForeColor in FCurHighlightElement.Features) and
                                            not(FCurHighlightElement.Group = agnDefault);
    ForegroundColorBox.Visible           := (hafForeColor in FCurHighlightElement.Features);

    ForegroundColorBox.Selected := NoneToDefault(FCurHighlightElement.Foreground);
    if ForegroundColorBox.Selected = clDefault then
      ForegroundColorBox.Tag := ForegroundColorBox.DefaultColorColor
    else
      ForegroundColorBox.Tag := ForegroundColorBox.Selected;
    ForeGroundUseDefaultCheckBox.Checked := ForegroundColorBox.Selected <> clDefault;

    ForeAlphaSpin.Visible  := ForegroundColorBox.Visible and
                             (hafAlpha in FCurHighlightElement.Features);
    ForeAlphaLabel.Visible := ForeAlphaSpin.Visible;
    if FCurHighlightElement.ForeAlpha = 0 then begin
      ForeAlphaSpin.Value    := 256; // Off
      Application.ProcessMessages;
      ForeAlphaSpin.Caption  := dlgEdOff;
    end
    else
      ForeAlphaSpin.Value    := FCurHighlightElement.ForeAlpha;

    ForePriorSpin.Visible  := ForegroundColorBox.Visible and FShowPrior and
                             (hafPrior in FCurHighlightElement.Features);
    ForePriorLabel.Visible := ForePriorSpin.Visible;
    ForePriorSpin.Value    := FCurHighlightElement.ForePriority;


    // BackGround
    BackGroundLabel.Visible              := (hafBackColor in FCurHighlightElement.Features) and
                                            (FCurHighlightElement.Group = agnDefault);
    BackGroundUseDefaultCheckBox.Visible := (hafBackColor in FCurHighlightElement.Features) and
                                            not(FCurHighlightElement.Group = agnDefault);
    BackGroundColorBox.Visible           := (hafBackColor in FCurHighlightElement.Features);

    BackGroundColorBox.Selected := NoneToDefault(FCurHighlightElement.Background);
    if BackGroundColorBox.Selected = clDefault then
      BackGroundColorBox.Tag := BackGroundColorBox.DefaultColorColor
    else
      BackGroundColorBox.Tag := BackGroundColorBox.Selected;
    BackGroundUseDefaultCheckBox.Checked := BackGroundColorBox.Selected <> clDefault;

    BackAlphaSpin.Visible := BackGroundColorBox.Visible and
                             (hafAlpha in FCurHighlightElement.Features);
    BackAlphaLabel.Visible := BackAlphaSpin.Visible;
    if FCurHighlightElement.BackAlpha = 0 then begin
      BackAlphaSpin.Value    := 256; // Off
      BackAlphaSpin.Caption  := dlgEdOff;
    end
    else
      BackAlphaSpin.Value    := FCurHighlightElement.BackAlpha;

    BackPriorSpin.Visible  := ForegroundColorBox.Visible and FShowPrior and
                             (hafPrior in FCurHighlightElement.Features);
    BackPriorLabel.Visible := BackPriorSpin.Visible;
    BackPriorSpin.Value    := FCurHighlightElement.BackPriority;

    // Frame
    FrameColorUseDefaultCheckBox.Visible := hafFrameColor in FCurHighlightElement.Features;
    FrameColorBox.Visible                := hafFrameColor in FCurHighlightElement.Features;
    FrameEdgesBox.Visible                := hafFrameEdges in FCurHighlightElement.Features;
    FrameStyleBox.Visible                := hafFrameStyle in FCurHighlightElement.Features;

    FrameColorBox.Selected := NoneToDefault(FCurHighlightElement.FrameColor);
    if FrameColorBox.Selected = clDefault then
      FrameColorBox.Tag := FrameColorBox.DefaultColorColor
    else
      FrameColorBox.Tag := FrameColorBox.Selected;
    FrameColorUseDefaultCheckBox.Checked := FrameColorBox.Selected <> clDefault;
    FrameEdgesBox.ItemIndex := integer(FCurHighlightElement.FrameEdges);
    FrameStyleBox.ItemIndex := integer(FCurHighlightElement.FrameStyle);
    FrameEdgesBox.Enabled := FrameColorUseDefaultCheckBox.Checked;
    FrameStyleBox.Enabled := FrameColorUseDefaultCheckBox.Checked;

    FrameAlphaSpin.Visible := FrameColorBox.Visible and
                             (hafAlpha in FCurHighlightElement.Features);
    FrameAlphaLabel.Visible := FrameAlphaSpin.Visible;
    if FCurHighlightElement.FrameAlpha = 0 then begin
      FrameAlphaSpin.Value    := 256; // Off
      FrameAlphaSpin.Caption  := dlgEdOff;
    end
    else
      FrameAlphaSpin.Value    := FCurHighlightElement.FrameAlpha;

    FramePriorSpin.Visible  := ForegroundColorBox.Visible and FShowPrior and
                             (hafPrior in FCurHighlightElement.Features);
    FramePriorLabel.Visible := FramePriorSpin.Visible;
    FramePriorSpin.Value    := FCurHighlightElement.FramePriority;

    // Styles
    TextBoldCheckBox.Visible      := hafStyle in FCurHighlightElement.Features;
    TextItalicCheckBox.Visible    := hafStyle in FCurHighlightElement.Features;
    TextUnderlineCheckBox.Visible := hafStyle in FCurHighlightElement.Features;

    TextBoldRadioPanel.Visible      := hafStyleMask in FCurHighlightElement.Features;
    TextItalicRadioPanel.Visible    := hafStyleMask in FCurHighlightElement.Features;
    TextUnderlineRadioPanel.Visible := hafStyleMask in FCurHighlightElement.Features;

    if hafStyleMask in FCurHighlightElement.Features then begin
      TextBoldCheckBox.Checked   := (fsBold in FCurHighlightElement.Style) or
                                    (fsBold in FCurHighlightElement.StyleMask);
      TextBoldRadioPanel.Enabled := TextBoldCheckBox.Checked;

      if not(fsBold in FCurHighlightElement.StyleMask) then
        TextBoldRadioInvert.Checked := True
      else
      if fsBold in FCurHighlightElement.Style then
        TextBoldRadioOn.Checked := True
      else
        TextBoldRadioOff.Checked := True;

      TextItalicCheckBox.Checked   := (fsItalic in FCurHighlightElement.Style) or
                                      (fsItalic in FCurHighlightElement.StyleMask);
      TextItalicRadioPanel.Enabled := TextItalicCheckBox.Checked;

      if not(fsItalic in FCurHighlightElement.StyleMask) then
        TextItalicRadioInvert.Checked := True
      else
      if fsItalic in FCurHighlightElement.Style then
        TextItalicRadioOn.Checked := True
      else
        TextItalicRadioOff.Checked := True;

      TextUnderlineCheckBox.Checked := (fsUnderline in FCurHighlightElement.Style) or
                                  (fsUnderline in FCurHighlightElement.StyleMask);
      TextUnderlineRadioPanel.Enabled := TextUnderlineCheckBox.Checked;

      if not(fsUnderline in FCurHighlightElement.StyleMask) then
        TextUnderlineRadioInvert.Checked := True
      else
      if fsUnderline in FCurHighlightElement.Style then
        TextUnderlineRadioOn.Checked := True
      else
        TextUnderlineRadioOff.Checked := True;
    end
    else
    begin
      TextBoldCheckBox.Checked      := fsBold in FCurHighlightElement.Style;
      TextItalicCheckBox.Checked    := fsItalic in FCurHighlightElement.Style;
      TextUnderlineCheckBox.Checked := fsUnderline in FCurHighlightElement.Style;
    end;

    UpdatingColor := False;
  finally
    EnableAlign;
  end;
  pnlElementAttributesResize(nil);
end;

procedure TSynColorAttrEditor.SetCurHighlightElement(AValue: TColorSchemeAttribute);
begin
  if FCurHighlightElement = AValue then Exit;
  FCurHighlightElement := AValue;
  UpdateAll;
end;

procedure TSynColorAttrEditor.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TSynColorAttrEditor.SetShowPrior(AValue: Boolean);
begin
  if FShowPrior = AValue then Exit;
  FShowPrior := AValue;
  UpdateAll;
end;

constructor TSynColorAttrEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FShowPrior := False;
end;

procedure TSynColorAttrEditor.Setup;
begin
  UpdatingColor := False;
  ColumnPosBevel.Height := 1;
  ForeGroundLabel.Caption := dlgForecolor;
  BackGroundLabel.Caption := dlgBackColor;
  ForeGroundUseDefaultCheckBox.Caption := dlgForecolor;
  BackGroundUseDefaultCheckBox.Caption := dlgBackColor;
  FrameColorUseDefaultCheckBox.Caption := dlgFrameColor;
  ForeAlphaLabel.Caption := lisAlpha;
  BackAlphaLabel.Caption := lisAlpha;
  FrameAlphaLabel.Caption := lisAlpha;
  ForePriorLabel.Caption := lisPriority;
  BackPriorLabel.Caption := lisPriority;
  FramePriorLabel.Caption := lisPriority;

  TextBoldCheckBox.Caption := dlgEdBold;
  TextBoldRadioOn.Caption := dlgEdOn;
  TextBoldRadioOff.Caption := dlgEdOff;
  TextBoldRadioInvert.Caption := dlgEdInvert;

  TextItalicCheckBox.Caption := dlgEdItal;
  TextItalicRadioOn.Caption := dlgEdOn;
  TextItalicRadioOff.Caption := dlgEdOff;
  TextItalicRadioInvert.Caption := dlgEdInvert;

  TextUnderlineCheckBox.Caption := dlgEdUnder;
  TextUnderlineRadioOn.Caption := dlgEdOn;
  TextUnderlineRadioOff.Caption := dlgEdOff;
  TextUnderlineRadioInvert.Caption := dlgEdInvert;

  Constraints.MinHeight := max(Constraints.MinHeight,
                               pnlUnderline.Top + pnlUnderline.Height +
                               Max(pnlUnderline.BorderSpacing.Around,
                                   pnlUnderline.BorderSpacing.Bottom)
                              );
end;

end.

