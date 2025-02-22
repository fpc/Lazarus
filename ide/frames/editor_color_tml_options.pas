unit editor_color_tml_options;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, LazarusIDEStrConsts,
  EditorOptions, IDEOptEditorIntf, IDEOptionsIntf, LazFileUtils, FileUtil,
  SynTextMateSyn;

type

  { TTEditorColorOptionsTMLFrame }

  TTEditorColorOptionsTMLFrame = class(TAbstractIDEOptionsEditor)
    btnReload: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure btnReloadClick(Sender: TObject);
  private

  public
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
  end;

implementation

{$R *.lfm}

{ TTEditorColorOptionsTMLFrame }

procedure TTEditorColorOptionsTMLFrame.btnReloadClick(Sender: TObject);
var
  dir: String;
  FileList: TStringList;
  i: Integer;
  tmlHighlighter: TSynTextMateSyn;
begin
  Memo1.Clear;
  for i := 0 to HighlighterList.Count - 1 do begin

    if (HighlighterList.SharedInstances[i] is TSynTextMateSyn) and
       (TSynTextMateSyn(HighlighterList.SharedInstances[i]).TextMateGrammar.MissingIncludes <> '')
    then
      Memo1.Lines.Add(Format('"%2:s" (%3:s) - '+dlgColorsTmlMissingInclude, [LineEnding+'',
        TSynTextMateSyn(HighlighterList.SharedInstances[i]).TextMateGrammar.MissingIncludes,
        TSynTextMateSyn(HighlighterList.SharedInstances[i]).TextMateGrammar.LanguageName,
        TSynTextMateSyn(HighlighterList.SharedInstances[i]).TextMateGrammar.LanguageScopeName
        ]));
  end;
  if Memo1.Lines.Count > 1 then begin
    Memo1.Lines.Add('----------');
    Memo1.Lines.Add('');
  end;

  dir := AppendPathDelim(UserSchemeDirectory(False)) + 'tml';
  FileList := nil;
  try
    if DirectoryExistsUTF8(dir) then
      FileList := FindAllFiles(dir, '*.json', False);

    if (FileList = nil) or (FileList.Count = 0) then begin
      Memo1.Text := dlgColorsTmlNoFilesFound;
      exit;
    end;

    for i := 0 to FileList.Count - 1 do begin
      tmlHighlighter := TSynTextMateSyn.Create(nil);
      tmlHighlighter.LoadGrammar(FileList[i], '');

      Memo1.Lines.Add(tmlHighlighter.LanguageName);
      Memo1.Lines.Add('- '+dlgColorsTmlFromFile+' '+FileList[i]);

      if (tmlHighlighter.ParserError <> '') then begin
        Memo1.Lines.Add('- '+dlgColorsTmlError+' '+tmlHighlighter.ParserError);
      end
      else begin
        if (tmlHighlighter.TextMateGrammar.SampleText = '') then begin
          if (tmlHighlighter.TextMateGrammar.SampleTextFile = '') then
            Memo1.Lines.Add('- '+dlgColorsTmlNoSampleTxt)
          else
          if not FileExistsUTF8(TrimAndExpandFilename(tmlHighlighter.TextMateGrammar.SampleTextFile, dir)) then
            Memo1.Lines.Add('- '+Format(dlgColorsTmlBadSampleTxtFile, [LineEnding+'', tmlHighlighter.TextMateGrammar.SampleTextFile]));
        end;

        Memo1.Lines.Add('- '+dlgColorsTmlOk);
      end;

      Memo1.Lines.Add('');
      tmlHighlighter.Free;
    end;
  finally
    FileList.Free;
  end;
end;

class function TTEditorColorOptionsTMLFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

function TTEditorColorOptionsTMLFrame.GetTitle: String;
begin
  Result := dlgColorsTml;
end;

procedure TTEditorColorOptionsTMLFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
begin
  btnReload.Caption := dlgColorsTmlRefresh;
  Label1.Caption  := Format(dlgColorsTmlInfo, [LineEnding+'', AppendPathDelim(UserSchemeDirectory(False)) + 'tml']);
end;

procedure TTEditorColorOptionsTMLFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  btnReloadClick(nil);
end;

procedure TTEditorColorOptionsTMLFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin

end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TTEditorColorOptionsTMLFrame, EdtOptionsTMLColors, EdtOptionsColors);

end.

