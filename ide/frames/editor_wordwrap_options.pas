unit editor_wordwrap_options;

{$mode ObjFPC}{$H+}

interface

uses
  // LCL
  Forms, Controls, StdCtrls, ExtCtrls, Spin, CheckLst,
  // IdeIntf
  IDEOptEditorIntf, IDEOptionsIntf, SynEditWrappedView, DividerBevel,
  // IDE
  EditorOptions, LazarusIDEStrConsts, EditorSyntaxHighlighterDef
  , Classes;

type

  { TEditorWordWrapOptionsFrame }

  TEditorWordWrapOptionsFrame = class(TAbstractIDEOptionsEditor)
    btnWordWrapAllHL: TButton;
    btnWordWrapNoneHL: TButton;
    cbEnableWordWrap: TCheckBox;
    cbIndentIsOffset: TCheckBox;
    cbHomeEndKey: TCheckBox;
    cbFixedWidth: TCheckBox;
    cblWordWrapHL: TCheckListBox;
    DividerBevelIndent: TDividerBevel;
    DividerBevelColumn: TDividerBevel;
    edMinWordWrapWidth: TSpinEdit;
    edMaxWordWrapWidth: TSpinEdit;
    edWordWrapIndentMax: TSpinEdit;
    edWordWrapIndentMaxRel: TSpinEdit;
    edWordWrapIndent: TSpinEdit;
    edWordWrapIndentMin: TSpinEdit;
    lblBevelColumn: TLabel;
    lblBevelCol2: TLabel;
    lblBevelIndent: TLabel;
    lblBevelIndent2: TLabel;
    lblFixedCol: TLabel;
    lblIndentOffs: TLabel;
    lblFixedCol2: TLabel;
    lblIndentOffs2: TLabel;
    lbMinWordWrapWidth: TLabel;
    lbMaxWordWrapWidth: TLabel;
    lbWordWrapIndentMin: TLabel;
    lbWordWrapIndent: TLabel;
    lbWordWrapIndentMax: TLabel;
    lbWordWrapIndentMaxRel: TLabel;
    Panel1: TPanel;
    rgCaretWrapPos: TRadioGroup;
    procedure btnWordWrapAllHLClick(Sender: TObject);
    procedure btnWordWrapNoneHLClick(Sender: TObject);
    procedure cbFixedWidthChange(Sender: TObject);
    procedure cbIndentIsOffsetChange(Sender: TObject);
  private

  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TEditorWordWrapOptionsFrame }

procedure TEditorWordWrapOptionsFrame.cbIndentIsOffsetChange(Sender: TObject);
begin
  if cbIndentIsOffset.Checked then begin
    edWordWrapIndent.MinValue := -100;
    edWordWrapIndentMin.Enabled := True;
    edWordWrapIndentMax.Enabled := True;
  end
  else begin
    edWordWrapIndent.MinValue := 0;
    edWordWrapIndentMin.Enabled := False;
    edWordWrapIndentMax.Enabled := False;
  end;
end;

procedure TEditorWordWrapOptionsFrame.cbFixedWidthChange(Sender: TObject);
begin
  edMaxWordWrapWidth.Enabled := not cbFixedWidth.Checked;
  case cbFixedWidth.Checked of
    True:    lbMinWordWrapWidth.Caption := dlgOptWordWrapFixedLineLength;
    False:   lbMinWordWrapWidth.Caption := dlgOptWordWrapMinimumLineLength;
  end;
end;

procedure TEditorWordWrapOptionsFrame.btnWordWrapAllHLClick(Sender: TObject);
begin
  cblWordWrapHL.CheckAll(cbChecked, False, False);
end;

procedure TEditorWordWrapOptionsFrame.btnWordWrapNoneHLClick(Sender: TObject);
begin
  cblWordWrapHL.CheckAll(cbUnchecked, False, False);
end;

function TEditorWordWrapOptionsFrame.GetTitle: String;
begin
  Result := dlgOptWordWrap;
end;

procedure TEditorWordWrapOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  i, index: Integer;
begin
  cbEnableWordWrap.Caption := dlgOptWordWrapUseWordwrap;
  btnWordWrapAllHL.Caption := dlgOptWordWrapAllHL;
  btnWordWrapNoneHL.Caption := dlgOptWordWrapNoneHL;
  rgCaretWrapPos.Caption := dlgOptWordWrapDisplayCaretAtWrapPositio;
  rgCaretWrapPos.Items.Add(dlgOptWordWrapEndOfLine);
  rgCaretWrapPos.Items.Add(dlgOptWordWrapStartOfNextLine);
  cbHomeEndKey.Caption := dlgOptWordWrapHomeEndKey;
  DividerBevelColumn.Caption := dlgOptWordWrapSectionColumn;
  lbMinWordWrapWidth.Caption := dlgOptWordWrapMinimumLineLength;
  lbMaxWordWrapWidth.Caption := dlgOptWordWrapMaximumLineLength;
  cbFixedWidth.Caption := dlgOptWordWrapCheckFixedLength;

  DividerBevelIndent.Caption := dlgOptWordWrapSectionIndent;
  lbWordWrapIndent.Caption       := dlgOptWordWrapIndent;
  cbIndentIsOffset.Caption       := dlgOptWordWrapIndentIsOffset;
  lbWordWrapIndentMin.Caption    := dlgOptWordWrapIndentMin;
  lbWordWrapIndentMax.Caption    := dlgOptWordWrapIndentMax;
  lbWordWrapIndentMaxRel.Caption := dlgOptWordWrapIndentMaxRel;

  cblWordWrapHL.Items.Clear;
  for i := IdeHighlighterNoneID to EditorOpts.HighlighterList.Count - 1 do
  begin
    if not (EditorOpts.HighlighterList.SharedSynInstances[i] is TNonSrcIDEHighlighter) then
    begin
      index := cblWordWrapHL.Items.AddObject(EditorOpts.HighlighterList.Captions[i], TObject(PtrUInt(i)));
      cblWordWrapHL.Checked[index] := True;
    end;
  end;
end;

procedure TEditorWordWrapOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  i, highlighterID: Integer;
begin
  cbEnableWordWrap.Checked := (AOptions as TEditorOptions).WordWrapEnabled;
  for i := 0 to cblWordWrapHL.Count - 1 do
  begin
    highlighterID := PtrUInt(Pointer(cblWordWrapHL.Items.Objects[i]));
    if (AOptions as TEditorOptions).WordWrapHLList.IndexOf(EditorOpts.HighlighterList.Names[highlighterID]) >= 0 then
      cblWordWrapHL.Checked[i] := False;
  end;
  case (AOptions as TEditorOptions).WordWrapCaretWrapPos of
    wcpEOL: rgCaretWrapPos.ItemIndex := 0;
    wcpBOL: rgCaretWrapPos.ItemIndex := 1;
  end;
  cbHomeEndKey.Checked     := (AOptions as TEditorOptions).WordWrapForceHomeEnd;

  cbFixedWidth.Checked     := (AOptions as TEditorOptions).WordWrapFixedWidth;
  edMinWordWrapWidth.Value := (AOptions as TEditorOptions).WordWrapMinWidth;
  edMaxWordWrapWidth.Value := (AOptions as TEditorOptions).WordWrapMaxWidth;

  edWordWrapIndent.Value       := (AOptions as TEditorOptions).WordWrapIndent;
  cbIndentIsOffset.Checked     := (AOptions as TEditorOptions).WordWrapIndentUseOffset;
  edWordWrapIndentMin.Value    := (AOptions as TEditorOptions).WordWrapIndentMin;
  edWordWrapIndentMax.Value    := (AOptions as TEditorOptions).WordWrapIndentMax;
  edWordWrapIndentMaxRel.Value := (AOptions as TEditorOptions).WordWrapIndentMaxRel;

  cbFixedWidthChange(nil);
end;

procedure TEditorWordWrapOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  i, highlighterID: Integer;
begin
  (AOptions as TEditorOptions).WordWrapEnabled := cbEnableWordWrap.Checked;
  (AOptions as TEditorOptions).WordWrapHLList.Clear;
  for i := 0 to cblWordWrapHL.Count - 1 do
  begin
    if not cblWordWrapHL.Checked[i] then
    begin
      highlighterID := PtrUInt(Pointer(cblWordWrapHL.Items.Objects[i]));
      (AOptions as TEditorOptions).WordWrapHLList.Add(EditorOpts.HighlighterList.Names[highlighterID]);
    end;
  end;
  case rgCaretWrapPos.ItemIndex of
    0: (AOptions as TEditorOptions).WordWrapCaretWrapPos := wcpEOL;
    1: (AOptions as TEditorOptions).WordWrapCaretWrapPos := wcpBOL;
  end;
  (AOptions as TEditorOptions).WordWrapForceHomeEnd := cbHomeEndKey.Checked;

  (AOptions as TEditorOptions).WordWrapFixedWidth := cbFixedWidth.Checked;
  (AOptions as TEditorOptions).WordWrapMinWidth := edMinWordWrapWidth.Value;
  (AOptions as TEditorOptions).WordWrapMaxWidth := edMaxWordWrapWidth.Value;

  (AOptions as TEditorOptions).WordWrapIndent          := edWordWrapIndent.Value;
  (AOptions as TEditorOptions).WordWrapIndentUseOffset := cbIndentIsOffset.Checked;
  (AOptions as TEditorOptions).WordWrapIndentMin       := edWordWrapIndentMin.Value;
  (AOptions as TEditorOptions).WordWrapIndentMax       := edWordWrapIndentMax.Value;
  (AOptions as TEditorOptions).WordWrapIndentMaxRel    := edWordWrapIndentMaxRel.Value;
end;

class function TEditorWordWrapOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorWordWrapOptionsFrame, EdtOptionsLineWrap);
end.

