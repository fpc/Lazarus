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
    function GetEditorFontName: string; virtual; abstract;
    procedure SetEditorFontName(const AValue: string); virtual; abstract;
    function GetEditorFontSize: Integer; virtual; abstract;
    procedure SetEditorFontSize(AValue: Integer); virtual; abstract;
  public
    // Syn is TLazEditCustomHighlighter (TLazEditCustomHighlighter, TSrcIDEHighlighter).
    procedure GetHighlighterObjSettings(ASynHL: TLazEditCustomHighlighter); virtual; abstract; deprecated 'use GetHighlighterSettings / to be removed in Lazarus 5.99';
    procedure GetHighlighterSettings(ASynHL: TLazEditCustomHighlighter); virtual; abstract;
    // ASynEdit and SimilarEdit are TSynEdit.
    procedure GetSynEditorSettings(ASynEdit: TCustomControl;
                                SimilarEdit: TCustomControl = nil); virtual; abstract;
    procedure SetMarkupColor(ASynHL: TLazEditCustomHighlighter;
                       AddHilightAttrName: string;
                       aMarkup: TLazEditTextAttributeModifier); virtual; abstract;
    // Name of the color scheme chosen for a language. LanguageName is the
    // LanguageName of its highlighter, e.g. IdeSyntaxHighlighters.SharedInstances[Id].LanguageName.
    // Call DoAfterWrite(False) after setting, to save and apply to all editors.
    function GetColorSchemeName(const LanguageName: string): string; virtual; abstract;
    procedure SetColorSchemeName(const LanguageName, SchemeName: string); virtual; abstract;
  public
    // read-only access to options needed by external packages.
    // feel free to extend when needed
    property TabPosition: TTabPosition read GetTabPosition;
    // Font of the source editor. Call DoAfterWrite(False) after setting,
    // to save and apply to all editors.
    property EditorFontName: string read GetEditorFontName write SetEditorFontName;
    property EditorFontSize: Integer read GetEditorFontSize write SetEditorFontSize;
  end;

var
  IDEEditorOptions: TIDEEditorOptions;

implementation

end.

