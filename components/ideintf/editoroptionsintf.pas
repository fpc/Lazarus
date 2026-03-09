unit EditorOptionsIntf;

{$mode objfpc}{$H+}

interface

uses
  // LCL
  ComCtrls,
  // BuildIntf
  IDEOptionsIntf;

type
  TIDEEditorOptions = class(TAbstractIDEEnvironmentOptions)
  protected
    function GetTabPosition: TTabPosition; virtual; abstract;
  public
    // Syn is TSrcIDEHighlighter (TSynCustomHighlighter).
    procedure GetHighlighterObjSettings(Syn: TObject); virtual; abstract;
    // ASynEdit and SimilarEdit are TSynEdit.
    procedure GetSynEditorSettings(ASynEdit: TObject; SimilarEdit: TObject = nil); virtual; abstract;
    // read-only access to options needed by external packages.
    // feel free to extend when needed
    property TabPosition: TTabPosition read GetTabPosition;
  end;

var
  IDEEditorOptions: TIDEEditorOptions;

implementation

end.

