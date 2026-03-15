unit EditorOptionsIntf;

{$mode objfpc}{$H+}

interface

uses
  // LCL
  Controls, ComCtrls,
  // BuildIntf
  IDEOptionsIntf,
  // LazEdit
  LazEditHighlighter, LazEditTextAttributes;

type
  TIDEEditorOptions = class(TAbstractIDEEnvironmentOptions)
  protected
    function GetTabPosition: TTabPosition; virtual; abstract;
  public
    // Syn is TLazEditCustomHighlighter (TSynCustomHighlighter, TSrcIDEHighlighter).
    procedure GetHighlighterObjSettings(ASynHL: TLazEditCustomHighlighter); virtual; abstract; deprecated 'use GetHighlighterSettings / to be removed in Lazarus 5.99';
    procedure GetHighlighterSettings(ASynHL: TLazEditCustomHighlighter); virtual; abstract;
    // ASynEdit and SimilarEdit are TSynEdit.
    procedure GetSynEditorSettings(ASynEdit: TCustomControl;
                                SimilarEdit: TCustomControl = nil); virtual; abstract;
    procedure SetMarkupColor(ASynHL: TLazEditCustomHighlighter;
                       AddHilightAttrName: string;
                       aMarkup: TLazEditTextAttributeModifier); virtual; abstract;
  public
    // read-only access to options needed by external packages.
    // feel free to extend when needed
    property TabPosition: TTabPosition read GetTabPosition;
  end;

var
  IDEEditorOptions: TIDEEditorOptions;

implementation

end.

