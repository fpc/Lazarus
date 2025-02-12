unit editor_wordwrap_options;

{$mode ObjFPC}{$H+}

interface

uses
  // LCL
  Forms, Controls, StdCtrls, ExtCtrls, Spin,
  // IdeIntf
  IDEOptEditorIntf, IDEOptionsIntf, SynEditWrappedView,
  // IDE
  EditorOptions, LazarusIDEStrConsts
  , Classes;

type

  { TEditorWordWrapOptionsFrame }

  TEditorWordWrapOptionsFrame = class(TAbstractIDEOptionsEditor)
    cbEnableWordWrap: TCheckBox;
    cbIndentIsOffset: TCheckBox;
    edMinWordWrapWidth: TSpinEdit;
    edWordWrapIndentMax: TSpinEdit;
    edWordWrapIndentMaxRel: TSpinEdit;
    edWordWrapIndent: TSpinEdit;
    edWordWrapIndentMin: TSpinEdit;
    Label1: TLabel;
    lbMinWordWrapWidth: TLabel;
    lbWordWrapIndentMin: TLabel;
    lbWordWrapIndent: TLabel;
    lbWordWrapIndentMax: TLabel;
    lbWordWrapIndentMaxRel: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    rgCaretWrapPos: TRadioGroup;
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

function TEditorWordWrapOptionsFrame.GetTitle: String;
begin
  Result := dlgOptWordWrap;
end;

procedure TEditorWordWrapOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  cbEnableWordWrap.Caption := dlgOptWordWrapUseWordwrap;
  rgCaretWrapPos.Caption := dlgOptWordWrapDisplayCaretAtWrapPositio;
  rgCaretWrapPos.Items.Add(dlgOptWordWrapEndOfLine);
  rgCaretWrapPos.Items.Add(dlgOptWordWrapStartOfNextLine);
  lbMinWordWrapWidth.Caption := dlgOptWordWrapMinimumLineLength;

  lbWordWrapIndent.Caption       := dlgOptWordWrapIndent;
  cbEnableWordWrap.Caption       := dlgOptWordWrapIndentIsOffest;
  lbWordWrapIndentMin.Caption    := dlgOptWordWrapIndentMin;
  lbWordWrapIndentMax.Caption    := dlgOptWordWrapIndentMax;
  lbWordWrapIndentMaxRel.Caption := dlgOptWordWrapIndentMaxRel;
end;

procedure TEditorWordWrapOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  cbEnableWordWrap.Checked := (AOptions as TEditorOptions).WordWrapEnabled;
  case (AOptions as TEditorOptions).WordWrapCaretWrapPos of
    wcpEOL: rgCaretWrapPos.ItemIndex := 0;
    wcpBOL: rgCaretWrapPos.ItemIndex := 1;
  end;
  edMinWordWrapWidth.Value := (AOptions as TEditorOptions).WordWrapMinWidth;

  edWordWrapIndent.Value       := (AOptions as TEditorOptions).WordWrapIndent;
  cbIndentIsOffset.Checked     := (AOptions as TEditorOptions).WordWrapIndentUseOffset;
  edWordWrapIndentMin.Value    := (AOptions as TEditorOptions).WordWrapIndentMin;
  edWordWrapIndentMax.Value    := (AOptions as TEditorOptions).WordWrapIndentMax;
  edWordWrapIndentMaxRel.Value := (AOptions as TEditorOptions).WordWrapIndentMaxRel;
end;

procedure TEditorWordWrapOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  (AOptions as TEditorOptions).WordWrapEnabled := cbEnableWordWrap.Checked;
  case rgCaretWrapPos.ItemIndex of
    0: (AOptions as TEditorOptions).WordWrapCaretWrapPos := wcpEOL;
    1: (AOptions as TEditorOptions).WordWrapCaretWrapPos := wcpBOL;
  end;
  (AOptions as TEditorOptions).WordWrapMinWidth := edMinWordWrapWidth.Value;

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

