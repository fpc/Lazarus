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
  ;

type

  { TEditorWordWrapOptionsFrame }

  TEditorWordWrapOptionsFrame = class(TAbstractIDEOptionsEditor)
    cbEnableWordWrap: TCheckBox;
    lbWinWordWrapWidth: TLabel;
    rgCaretWrapPos: TRadioGroup;
    edWinWordWrapWidth: TSpinEdit;
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
  lbWinWordWrapWidth.Caption := dlgOptWordWrapMinimumLineLength;
end;

procedure TEditorWordWrapOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  cbEnableWordWrap.Checked := (AOptions as TEditorOptions).WordWrapEnabled;
  case (AOptions as TEditorOptions).WordWrapCaretWrapPos of
    wcpEOL: rgCaretWrapPos.ItemIndex := 0;
    wcpBOL: rgCaretWrapPos.ItemIndex := 1;
  end;
  edWinWordWrapWidth.Value := (AOptions as TEditorOptions).WordWrapMinWidth;
end;

procedure TEditorWordWrapOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  (AOptions as TEditorOptions).WordWrapEnabled := cbEnableWordWrap.Checked;
  case rgCaretWrapPos.ItemIndex of
    0: (AOptions as TEditorOptions).WordWrapCaretWrapPos := wcpEOL;
    1: (AOptions as TEditorOptions).WordWrapCaretWrapPos := wcpBOL;
  end;
  (AOptions as TEditorOptions).WordWrapMinWidth := edWinWordWrapWidth.Value;

end;

class function TEditorWordWrapOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorWordWrapOptionsFrame, EdtOptionsLineWrap);
end.

