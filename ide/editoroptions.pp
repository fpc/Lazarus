{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Editor options container and editor options dialog.
    The editor options are stored in XML format in the
     ~/.lazarus/editoroptions.xml file.
    Currently only for TSynEdit.
}
unit EditorOptions;

{$mode objfpc}{$H+}

{$IFDEF Windows}
  {$IFnDEF WithoutWinIME}
    {$DEFINE WinIME}
  {$ENDIF}
{$ENDIF}

interface

uses
  // RTL, FCL
  Classes, SysUtils, typinfo, resource,
  // LCL
  Graphics, LCLProc, LResources, Forms, Dialogs, ComCtrls, LCLType,
  // LazUtils
  FileUtil, LazFileUtils, LazUTF8, LazClasses, LazUTF8Classes, Laz2_XMLCfg,
  LazStringUtils,
  // Synedit
  SynEdit, SynEditAutoComplete, SynEditKeyCmds, SynEditTypes,
  SynEditMiscClasses, SynBeautifier, SynEditTextTrimmer, SynEditMouseCmds,
  SynPluginTemplateEdit, SynPluginSyncroEdit,
  SynGutter, SynGutterBase, SynGutterCodeFolding, SynGutterLineNumber,
  SynGutterChanges, SynCompletion,
  SynEditMarkupBracket, SynEditMarkupHighAll, SynEditMarkupWordGroup,
  SynEditMarkupSpecialChar,
  SourceSynEditor,
  // SynEdit Highlighters
  SynEditHighlighter, SynEditHighlighterFoldBase, SynHighlighterCPP,
  SynHighlighterHTML, SynHighlighterJava, SynHighlighterLFM, SynHighlighterPas,
  SynHighlighterPerl, SynHighlighterPHP, SynHighlighterSQL, SynHighlighterCss,
  SynHighlighterPython, SynHighlighterUNIXShellScript, SynHighlighterXML,
  SynHighlighterJScript, SynHighlighterDiff, SynHighlighterBat,
  SynHighlighterIni, SynHighlighterPo, SynHighlighterPike, SynPluginMultiCaret,
  SynEditMarkupFoldColoring, SynEditMarkup,
  // codetools
  LinkScanner, CodeToolManager,
  // IDEIntf
  IDECommands, SrcEditorIntf, IDEOptionsIntf, IDEOptEditorIntf, IDEDialogs,
  EditorSyntaxHighlighterDef, MacroIntf,
  // IDE
  SourceMarks, LazarusIDEStrConsts, KeyMapping, LazConf;

const
  DefaultCompletionLongLineHintType = sclpExtendRightOnly;
  DefaultEditorDisableAntiAliasing = false;

type
  TPreviewPasSyn = TIDESynFreePasSyn;
  TSrcIDEHighlighter = TSynCustomHighlighter;
  TSynHighlightElement = TSynHighlighterAttributes;
  TCustomSynClass = class of TSrcIDEHighlighter;

  TLazSynPluginTemplateMultiCaret = class(TForm)     end;
  TLazSynPluginTemplateEditForm = class(TForm)     end;
  TLazSynPluginTemplateEditFormOff = class(TForm)  end;
  TLazSynPluginSyncroEditFormSel = class(TForm)    end;
  TLazSynPluginSyncroEditForm = class(TForm)       end;
  TLazSynPluginSyncroEditFormOff = class(TForm)    end;

  TColorSchemeAttributeFeature =
    ( hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior,
      hafStyle, hafStyleMask,
      hafFrameStyle, hafFrameEdges,
      hafMarkupFoldColor // for the MarkupFoldColor module
    );
  TColorSchemeAttributeFeatures = set of TColorSchemeAttributeFeature;

const
  SynEditPreviewIncludeOptions = [eoNoCaret, eoNoSelection];
  SynEditPreviewExcludeOptions = [eoDragDropEditing, eoDropFiles,
                                  eoScrollPastEof];
  SynEditPreviewIncludeOptions2 = [];
  SynEditPreviewExcludeOptions2 = [eoAlwaysVisibleCaret];

  DefaultCodeTemplatesFilename = 'lazarus.dci'; // in directory GetPrimaryConfigPath

  // Do not localize: those are used for the config XML
  ahaXmlNames: array[TAdditionalHilightAttribute] of String =
  (
    '',                    'Text block',                'Execution point',
    'Enabled breakpoint',  'Disabled breakpoint',       'Invalid breakpoint',
    'Unknown breakpoint',  'Error line',                'Incremental search match',
    'Highlight all',       'Brackets highlight',        'Mouse link',
    'Line number',         'Line highlight',            'Modified line',
    'Code folding tree',   'Highlight current word',    'Folded code',
    'Folded code Line',    'Hidden code Line',
    'Word-Brackets',       'TemplateEdit Current',      'TemplateEdit Sync',
    'TemplateEdit Cells',  'SyncronEdit Current Cells', 'SyncronEdit Syncron Cells',
    'SyncronEdit Other Cells', 'SyncronEdit Range',
    '', // scaGutterSeparator => uses RTTI only
    '', // ahaGutter
    '',  // ahaRightMargin
    '',  // ahaSpecialVisibleChars
    '',  // ahaTopInfoHint
    '', // ahaCaretColor
    '', '', '',  // ahaIfDefBlockInactive, ahaIfDefBlockActive, ahaIfDefBlockTmpActive
    '', '', '',  // ahaIfDefNodeInactive, ahaIfDefNodeActive, ahaIfDefNodeTmpActive
    '', '', '', '', // ahaIdentComplWindow, ahaIdentComplWindowBorder, ahaIdentComplWindowSelection, ahaIdentComplWindowHighlight
    '', '', '', '', '', '', '', '', '', '' // ahaOutlineLevel1Color..ahaOutlineLevel10Color
  );

  ahaGroupMap: array[TAdditionalHilightAttribute] of TAhaGroupName = (
    { ahaNone }                agnText,
    { ahaTextBlock }           agnText,
    { ahaExecutionPoint }      agnLine,
    { ahaEnabledBreakpoint }   agnLine,
    { ahaDisabledBreakpoint }  agnLine,
    { ahaInvalidBreakpoint }   agnLine,
    { ahaUnknownBreakpoint }   agnLine,
    { ahaErrorLine }           agnLine,
    { ahaIncrementalSearch }   agnText,
    { ahaHighlightAll }        agnText,
    { ahaBracketMatch }        agnText,
    { ahaMouseLink }           agnText,
    { ahaLineNumber }          agnGutter,
    { ahaLineHighlight }       agnLine,
    { ahaModifiedLine }        agnGutter,
    { ahaCodeFoldingTree }     agnGutter,
    { ahaHighlightWord }       agnText,
    { ahaFoldedCode }          agnGutter,
    { ahaFoldedCodeLine }      agnGutter,
    { ahaHiddenCodeLine }      agnGutter,
    { ahaWordGroup }           agnText,
    { ahaTemplateEditCur }     agnTemplateMode,
    { ahaTemplateEditSync }    agnTemplateMode,
    { ahaTemplateEditOther }   agnTemplateMode,
    { ahaSyncroEditCur }       agnSyncronMode,
    { ahaSyncroEditSync }      agnSyncronMode,
    { ahaSyncroEditOther }     agnSyncronMode,
    { ahaSyncroEditArea }      agnSyncronMode,
    { ahaGutterSeparator }     agnGutter,
    { ahaGutter }              agnGutter,
    { ahaRightMargin}          agnGutter,
    { ahaSpecialVisibleChars } agnText,
    { ahaTopInfoHint }         agnLine,
    { ahaCaretColor }          agnText,
    { ahaIfDefBlockInactive }  agnIfDef,
    { ahaIfDefBlockActive }    agnIfDef,
    { ahaIfDefBlockTmpActive } agnIfDef,
    { ahaIfDefNodeInactive }   agnIfDef,
    { ahaIfDefNodeActive }     agnIfDef,
    { ahaIfDefNodeTmpActive }  agnIfDef,
    { ahaIdentComplWindow }           agnIdentComplWindow,
    { ahaIdentComplWindowBorder }     agnIdentComplWindow,
    { ahaIdentComplWindowSelection }  agnIdentComplWindow,
    { ahaIdentComplWindowHighlight }  agnIdentComplWindow,
    { ahaOutlineLevel1Color }  agnOutlineColors,
    { ahaOutlineLevel2Color }  agnOutlineColors,
    { ahaOutlineLevel3Color }  agnOutlineColors,
    { ahaOutlineLevel4Color }  agnOutlineColors,
    { ahaOutlineLevel5Color }  agnOutlineColors,
    { ahaOutlineLevel6Color }  agnOutlineColors,
    { ahaOutlineLevel7Color }  agnOutlineColors,
    { ahaOutlineLevel8Color }  agnOutlineColors,
    { ahaOutlineLevel9Color }  agnOutlineColors,
    { ahaOutlineLevel10Color } agnOutlineColors

  );
  ahaSupportedFeatures: array[TAdditionalHilightAttribute] of TColorSchemeAttributeFeatures =
  (
    { ahaNone }               [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaTextBlock }          [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaExecutionPoint }     [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaEnabledBreakpoint }  [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaDisabledBreakpoint } [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaInvalidBreakpoint }  [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaUnknownBreakpoint }  [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaErrorLine }          [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIncrementalSearch }  [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaHighlightAll }       [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaBracketMatch }       [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaMouseLink }          [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaLineNumber }         [hafBackColor, hafForeColor, hafFrameColor, hafStyle],
    { ahaLineHighlight }      [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaModifiedLine }       [hafBackColor, hafForeColor, hafFrameColor],
    { ahaCodeFoldingTree }    [hafBackColor, hafForeColor, hafFrameColor],
    { ahaHighlightWord }      [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaFoldedCode }         [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaFoldedCodeLine }     [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaHiddenCodeLine }     [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaWordGroup }          [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaTemplateEditCur }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaTemplateEditSync }   [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaTemplateEditOther }  [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaSyncroEditCur }      [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaSyncroEditSync }     [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaSyncroEditOther }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaSyncroEditArea }     [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaGutterSeparator }    [hafBackColor, hafForeColor],
    { ahaGutter }             [hafBackColor],
    { ahaRightMargin}         [hafForeColor],
    { ahaSpecialVisibleChars }[hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaTopInfoHint }        [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaCaretColor }         [hafBackColor, hafForeColor],
    { ahaIfDefBlockInactive } [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIfDefBlockActive }   [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIfDefBlockTmpActive }[hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIfDefNodeInactive }  [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIfDefNodeActive }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIfDefNodeTmpActive } [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIdentComplWindow }   [hafBackColor, hafForeColor],
    { ahaIdentComplWindowBorder }    [hafForeColor],
    { ahaIdentComplWindowSelection } [hafBackColor, hafForeColor],
    { ahaIdentComplWindowHighlight } [hafForeColor],
    { ahaFoldLevel1Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel2Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel3Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel4Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel5Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel6Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel7Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel8Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel9Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel10Color }   [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor]
  );


var
  AdditionalHighlightAttributes: array[TAdditionalHilightAttribute] of String;
  AdditionalHighlightGroupNames: array[TAhaGroupName] of String;

type
  (* ***  ColorSchemes  *** *)

  { TQuickStringlist }

  TQuickStringlist=class(TStringlist)
    Function DoCompareText(const s1,s2 : string) : PtrInt; override;
  end;

  TColorScheme = class;
  TColorSchemeLanguage = class;

  { TColorSchemeAttribute }

  TColorSchemeAttribute = class(TSynHighlighterAttributesModifier)
  private
    FFeatures: TColorSchemeAttributeFeatures;
    FGroup: TAhaGroupName;
    FMarkupFoldLineAlpha: Byte;
    FMarkupFoldLineColor: TColor;
    FMarkupFoldLineStyle: TSynLineStyle;
    FOwner: TColorSchemeLanguage;
    FUseSchemeGlobals: Boolean;
    function GetIsUsingSchemeGlobals: Boolean;
    function OldAdditionalAttributeName(NewAha: String): string;
    procedure SetMarkupFoldLineAlpha(AValue: Byte);
    procedure SetMarkupFoldLineColor(AValue: TColor);
    procedure SetMarkupFoldLineStyle(AValue: TSynLineStyle);
  protected
    procedure Init; override;
  public
    constructor Create(ASchemeLang: TColorSchemeLanguage; attribName: PString; aStoredName: String = '');
    function IsEnabled: boolean; override;
    procedure ApplyTo(aDest: TSynHighlighterAttributes; aDefault: TColorSchemeAttribute = nil);
    procedure Assign(Src: TPersistent); override;
    function Equals(Other: TColorSchemeAttribute): Boolean; reintroduce;
    function GetStoredValuesForAttrib: TColorSchemeAttribute; // The IDE default colors from the resources
    function GetSchemeGlobal: TColorSchemeAttribute;
    procedure LoadFromXml(aXMLConfig: TRttiXMLConfig; aPath: String;
                          Defaults: TColorSchemeAttribute; Version: Integer);
    procedure LoadFromXmlV1(aXMLConfig: TRttiXMLConfig; aPath: String;
                            Defaults: TColorSchemeAttribute);
    procedure SaveToXml(aXMLConfig: TRttiXMLConfig; aPath: String;
                        Defaults: TColorSchemeAttribute);
    property Group: TAhaGroupName read FGroup write FGroup;
    property IsUsingSchemeGlobals: Boolean read GetIsUsingSchemeGlobals;
    property Features: TColorSchemeAttributeFeatures read FFeatures write FFeatures;
  published
    property UseSchemeGlobals: Boolean read FUseSchemeGlobals write FUseSchemeGlobals;
    // For markup fold color
    property MarkupFoldLineColor: TColor read FMarkupFoldLineColor write SetMarkupFoldLineColor default clNone; // clDefault will take Color[].Frame or Color[].Foreground
    property MarkupFoldLineStyle: TSynLineStyle read FMarkupFoldLineStyle write SetMarkupFoldLineStyle default slsSolid;
    property MarkupFoldLineAlpha: Byte read FMarkupFoldLineAlpha write SetMarkupFoldLineAlpha default 0;
  end;

  { TColorSchemeLanguage }

  TColorSchemeLanguage = class(TObject)
  private
    FDefaultAttribute: TColorSchemeAttribute;
    FAttributes: TQuickStringlist; // TColorSchemeAttribute
    FHighlighter: TSynCustomHighlighter;
    FLanguage: TLazSyntaxHighlighter;
    FOwner: TColorScheme;
    FLanguageName: String;
    FIsSchemeDefault: Boolean;
    FFormatVersion: integer;
    function GetAttribute(Index: String): TColorSchemeAttribute;
    function GetAttributeAtPos(Index: Integer): TColorSchemeAttribute;
    function GetAttributeByEnum(Index: TAdditionalHilightAttribute): TColorSchemeAttribute;
    function GetName: String;
    function AhaToStoredName(aha: TAdditionalHilightAttribute): String;
  public
    constructor Create(const AGroup: TColorScheme; const ALang: TLazSyntaxHighlighter;
                       IsSchemeDefault: Boolean = False);
    constructor CreateFromXml(const AGroup: TColorScheme; const ALang: TLazSyntaxHighlighter;
                              aXMLConfig: TRttiXMLConfig; aPath: String;
                              IsSchemeDefault: Boolean = False);
    destructor  Destroy; override;
    procedure Clear;
    procedure Assign(Src: TColorSchemeLanguage); reintroduce;
    function Equals(Other: TColorSchemeLanguage): Boolean; reintroduce;
    function GetStoredValuesForLanguage: TColorSchemeLanguage; // The IDE default colors from the resources
    function IndexOfAttr(AnAttr: TColorSchemeAttribute): Integer;
    procedure LoadFromXml(aXMLConfig: TRttiXMLConfig; aPath: String; Defaults: TColorSchemeLanguage;
              ColorVersion: Integer; aOldPath: String = '');
    procedure SaveToXml(aXMLConfig: TRttiXMLConfig; aPath: String; Defaults: TColorSchemeLanguage);
    procedure ApplyTo(ASynEdit: TSynEdit); // Write markup, etc
    procedure ApplyTo(AHLighter: TSynCustomHighlighter);
    function  AttributeCount: Integer;
    property  Name: String read GetName;
    property  Language: TLazSyntaxHighlighter read FLanguage;
    property  LanguageName: String read FLanguageName;
    property  Attribute[Index: String]: TColorSchemeAttribute read GetAttribute;
    property  AttributeByEnum[Index: TAdditionalHilightAttribute]: TColorSchemeAttribute
              read GetAttributeByEnum;
    property  AttributeAtPos[Index: Integer]: TColorSchemeAttribute read GetAttributeAtPos;
    property  DefaultAttribute: TColorSchemeAttribute read FDefaultAttribute;
    property  Highlighter: TSynCustomHighlighter read FHighlighter;
  end;

  { TColorScheme }

  TColorScheme = class(TObject)
  private
    FName: String;
    FColorSchemes: Array [TLazSyntaxHighlighter] of TColorSchemeLanguage;
    FDefaultColors: TColorSchemeLanguage;
    function GetColorScheme(Index: TLazSyntaxHighlighter): TColorSchemeLanguage;
    function GetColorSchemeBySynClass(Index: TClass): TColorSchemeLanguage;
  public
    constructor Create(AName: String);
    constructor CreateFromXml(aXMLConfig: TRttiXMLConfig; const AName, aPath: String);
    destructor  Destroy; override;
    procedure Assign(Src: TColorScheme); reintroduce;
    function GetStoredValuesForScheme: TColorScheme; // The IDE default colors from the resources
    procedure LoadFromXml(aXMLConfig: TRttiXMLConfig; aPath: String;
                          Defaults: TColorScheme; aOldPath: String = '');
    procedure SaveToXml(aXMLConfig: TRttiXMLConfig; aPath: String; Defaults: TColorScheme);
    property  Name: string read FName;
    property  DefaultColors: TColorSchemeLanguage read FDefaultColors;
    property  ColorScheme[Index: TLazSyntaxHighlighter]: TColorSchemeLanguage read GetColorScheme;
    property  ColorSchemeBySynClass[Index: TClass]: TColorSchemeLanguage read GetColorSchemeBySynClass;
  end;

  { TColorSchemeFactory }

  TColorSchemeFactory = class(TObject)
  private
    FMappings: TQuickStringlist; // TColorScheme
    function GetColorSchemeGroup(Index: String): TColorScheme;
    function GetColorSchemeGroupAtPos(Index: Integer): TColorScheme;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Clear;
    procedure Assign(Src: TColorSchemeFactory); reintroduce;
    procedure LoadFromXml(aXMLConfig: TRttiXMLConfig; aPath: String;
                          Defaults: TColorSchemeFactory; aOldPath: String = '');
    procedure SaveToXml(aXMLConfig: TRttiXMLConfig; aPath: String; Defaults: TColorSchemeFactory);
    procedure RegisterScheme(aXMLConfig: TRttiXMLConfig; AName, aPath: String);
    procedure GetRegisteredSchemes(AList: TStrings);
    property  ColorSchemeGroup[Index: String]: TColorScheme read GetColorSchemeGroup;
    property  ColorSchemeGroupAtPos[Index: Integer]: TColorScheme read GetColorSchemeGroupAtPos;
  end;

type

  TEditorOptionsDividerInfo = record
    Name: String;      // Name for display
    Xml: String;       // Name for XML
    BoolOpt: Boolean;  // Checkbox only
    MaxLevel: Integer;
  end;
  TEditorOptionsDividerInfoList = Array [0..999] of TEditorOptionsDividerInfo;
  PEditorOptionsDividerInfoList = ^TEditorOptionsDividerInfoList;

  TEditorOptionsDividerRecord = record
    Count: Integer;
    Info: PEditorOptionsDividerInfoList;
  end;

var

  (* When adding new entries, ensure that resourcestrings are re-assigned in InitLocale *)
  EditorOptionsDividerInfoPas: Array [0..8] of TEditorOptionsDividerInfo
  = (
      (Name: dlgDivPasUnitSectionName;  Xml: 'Sect';    BoolOpt: True;  MaxLevel: 1),
      (Name: dlgDivPasUsesName;         Xml: 'Uses';    BoolOpt: True;  MaxLevel: 0),
      (Name: dlgDivPasVarGlobalName;    Xml: 'GVar';    BoolOpt: True;  MaxLevel: 1),
      (Name: dlgDivPasVarLocalName;     Xml: 'LVar';    BoolOpt: False; MaxLevel: 0),
      (Name: dlgDivPasStructGlobalName; Xml: 'GStruct'; BoolOpt: False; MaxLevel: 1),
      (Name: dlgDivPasStructLocalName;  Xml: 'LStruct'; BoolOpt: False; MaxLevel: 0),
      (Name: dlgDivPasProcedureName;    Xml: 'Proc';    BoolOpt: False; MaxLevel: 1),
      (Name: dlgDivPasBeginEndName;     Xml: 'Begin';   BoolOpt: False; MaxLevel: 0),
      (Name: dlgDivPasTryName;          Xml: 'Try';     BoolOpt: False; MaxLevel: 0)
    );

const

  (* When adding new entries, ensure that resourcestrings are re-assigned in InitLocale *)
  EditorOptionsDividerDefaults: array[TLazSyntaxHighlighter] of
    TEditorOptionsDividerRecord =
    ( (Count: 0; Info: nil), // none
      (Count: 0; Info: nil), // text
      (Count: 9; Info: @EditorOptionsDividerInfoPas[0]), // Freepas
      (Count: 9; Info: @EditorOptionsDividerInfoPas[0]), // pas
      (Count: 0; Info: nil), // lfm
      (Count: 0; Info: nil), // xml
      (Count: 0; Info: nil), // html
      (Count: 0; Info: nil), // cpp
      (Count: 0; Info: nil), // perl
      (Count: 0; Info: nil), // java
      (Count: 0; Info: nil), // shell
      (Count: 0; Info: nil), // python
      (Count: 0; Info: nil), // php
      (Count: 0; Info: nil), // sql
      (Count: 0; Info: nil), // css
      (Count: 0; Info: nil), // jscript
      (Count: 0; Info: nil), // Diff
      (Count: 0; Info: nil), // Ini
      (Count: 0; Info: nil), // Bat
      (Count: 0; Info: nil), // PO
      (Count: 0; Info: nil)  // Pike
    );

type

  TEditorOptionsFoldInfo = record
    Name: String;      // Name for display
    Xml: String;       // Name for XML
    Index: Integer;    // FHighlighter.FoldConf[index]
    Enabled: Boolean;
  end;
  TEditorOptionsFoldInfoList = Array [0..999] of TEditorOptionsFoldInfo;
  PEditorOptionsFoldInfoList = ^TEditorOptionsFoldInfoList;

  TEditorOptionsFoldRecord = record
    Count: Integer;
    HasMarkup: Boolean;
    Info: PEditorOptionsFoldInfoList;
  end;

type

  { TSynEditMouseActionKeyCmdHelper }

  TSynEditMouseActionKeyCmdHelper = class(TSynEditMouseAction)
  private
    function GetOptionKeyCmd: TSynEditorCommand;
    procedure SetOptionKeyCmd(const AValue: TSynEditorCommand);
  published
    property Option: TSynEditorCommand read GetOptionKeyCmd write SetOptionKeyCmd;
  end;


const

  (* When adding new entries, ensure that resourcestrings are re-assigned in InitLocale *)
  EditorOptionsFoldInfoPas: Array [0..26] of TEditorOptionsFoldInfo
  = (
      (Name:  dlgFoldPasProcedure;     Xml:     'Procedure';
       Index: ord(cfbtProcedure);    Enabled: True),
      (Name:  dlgFoldLocalPasVarType;  Xml:     'LocalVarType';
       Index: ord(cfbtLocalVarType); Enabled: True),
      (Name:  dlgFoldPasProcBeginEnd;  Xml:     'ProcBeginEnd';
       Index: ord(cfbtTopBeginEnd);  Enabled: True),
      (Name:  dlgFoldPasBeginEnd;      Xml:     'BeginEnd';
       Index: ord(cfbtBeginEnd);     Enabled: True),
      (Name:  dlgFoldPasRepeat;        Xml:     'Repeat';
       Index: ord(cfbtRepeat);       Enabled: False),
      (Name:  dlgFoldPasCase;          Xml:     'Case';
       Index: ord(cfbtCase);         Enabled: False),
      (Name:  dlgFoldPasTry;           Xml:     'Try';
       Index: ord(cfbtTry);          Enabled: False),
      (Name:  dlgFoldPasExcept;        Xml:     'Except';
       Index: ord(cfbtExcept);       Enabled: False),
      (Name:  dlgFoldPasAsm;           Xml:     'Asm';
       Index: ord(cfbtAsm);          Enabled: True),

      (Name:  dlgFoldPasProgram;       Xml:     'Program';
       Index: ord(cfbtProgram);      Enabled: False),
      (Name:  dlgFoldPasUnit;          Xml:     'Unit';
       Index: ord(cfbtUnit);         Enabled: False),
      (Name:  dlgFoldPasUnitSection;   Xml:     'UnitSection';
       Index: ord(cfbtUnitSection);  Enabled: False),
      (Name:  dlgFoldPasUses;          Xml:     'Uses';
       Index: ord(cfbtUses);         Enabled: True),

      (Name:  dlgFoldPasVarType;       Xml:     'VarType';
       Index: ord(cfbtVarType);      Enabled: False),
      (Name:  dlgFoldPasClass;         Xml:     'Class';
       Index: ord(cfbtClass);        Enabled: True),
      (Name:  dlgFoldPasClassSection;  Xml:     'ClassSection';
       Index: ord(cfbtClassSection); Enabled: True),
      (Name:  dlgFoldPasRecord;        Xml:     'Record';
       Index: ord(cfbtRecord);       Enabled: True),

      (Name:  dlgFoldPasIfDef;         Xml:     'IfDef';
       Index: ord(cfbtIfDef);        Enabled: False),
      (Name:  dlgFoldPasUserRegion;    Xml:     'UserRegion';
       Index: ord(cfbtRegion);       Enabled: True),

      (Name:  dlgFoldPasAnsiComment;   Xml:     'AnsiComment';
       Index: ord(cfbtAnsiComment);  Enabled: True),
      (Name:  dlgFoldPasBorComment;    Xml:     'BorComment';
       Index: ord(cfbtBorCommand);   Enabled: True),
      (Name:  dlgFoldPasSlashComment;    Xml:     'SlashComment';
       Index: ord(cfbtSlashComment); Enabled: True),

      (Name:  dlgFoldPasNestedComment; Xml:     'NestedComment';
       Index: ord(cfbtNestedComment);Enabled: True),

      (Name:  dlgFoldPasIfThen; Xml:     'IfThen';
       Index: ord(cfbtIfThen); Enabled: False),
      (Name:  dlgFoldPasForDo; Xml:     'ForDo';
       Index: ord(cfbtForDo); Enabled: False),
      (Name:  dlgFoldPasWhileDo; Xml:     'WhileDo';
       Index: ord(cfbtWhileDo); Enabled: False),
      (Name:  dlgFoldPasWithDo; Xml:     'WithDo';
       Index: ord(cfbtWithDo); Enabled: False)
    );

  EditorOptionsFoldInfoLFM: Array [0..2] of TEditorOptionsFoldInfo
  = (
      ( Name:    dlgFoldLfmObject;
        Xml:    'Object';
        Index:   ord(cfbtLfmObject);
        Enabled: True
      ),
      ( Name:    dlgFoldLfmList;
        Xml:     'List';
        Index:   ord(cfbtLfmList);
        Enabled: True
      ),
      ( Name:    dlgFoldLfmItem;
        Xml:     'Item';
        Index:   ord(cfbtLfmItem);
        Enabled: True
      )
    );

  EditorOptionsFoldInfoXML: Array [0..4] of TEditorOptionsFoldInfo
  = (
      ( Name:    dlgFoldXmlNode;
        Xml:    'Node';
        Index:   ord(cfbtXmlNode);
        Enabled: True
      ),
      ( Name:    dlgFoldXmlComment;
        Xml:    'Comment';
        Index:   ord(cfbtXmlComment);
        Enabled: True
      ),
      ( Name:    dlgFoldXmlCData;
        Xml:    'CData';
        Index:   ord(cfbtXmlCData);
        Enabled: True
      ),
      ( Name:    dlgFoldXmlDocType;
        Xml:    'DocType';
        Index:   ord(cfbtXmlDocType);
        Enabled: True
      ),
      ( Name:    dlgFoldXmlProcess;
        Xml:    'ProcessInstr';
        Index:   ord(cfbtXmlProcess);
        Enabled: True
      )
    );

  EditorOptionsFoldInfoHTML: Array [0..2] of TEditorOptionsFoldInfo
  = (
      ( Name:    dlgFoldHtmlNode;
        Xml:    'Node';
        Index:   ord(cfbtHtmlNode);
        Enabled: True
      ),
      ( Name:    dlgFoldHtmlComment;
        Xml:    'Comment';
        Index:   ord(cfbtXmlComment);
        Enabled: True
      ),
      ( Name:    dlgFoldHtmlAsp;
        Xml:    'ASP';
        Index:   ord(cfbtHtmlAsp);
        Enabled: True
      )
    );

  EditorOptionsFoldInfoDiff: Array [0..2] of TEditorOptionsFoldInfo
  = (
      ( Name:    lisFile;
        Xml:    'File';
        Index:   ord(cfbtDiffFile);
        Enabled: True
      ),
      ( Name:    dlgFoldDiffChunk;
        Xml:    'Chunk';
        Index:   ord(cfbtDiffChunk);
        Enabled: True
      ),
      ( Name:    dlgFoldDiffChunkSect;
        Xml:    'ChunkSect';
        Index:   ord(cfbtDiffChunkSect);
        Enabled: True
      )
    );

  (* When adding new entries, ensure that resourcestrings are re-assigned in InitLocale *)
  EditorOptionsFoldDefaults: array[TLazSyntaxHighlighter] of
    TEditorOptionsFoldRecord =
    ( (Count:  0; HasMarkup: False; Info: nil), // none
      (Count:  0; HasMarkup: False; Info: nil), // text
      (Count: 27; HasMarkup: True; Info: @EditorOptionsFoldInfoPas[0]), // Freepas
      (Count: 27; HasMarkup: True; Info: @EditorOptionsFoldInfoPas[0]), // pas
      (Count:  3; HasMarkup: True; Info: @EditorOptionsFoldInfoLFM[0]), // lfm
      (Count:  5; HasMarkup: True; Info: @EditorOptionsFoldInfoXML[0]), // xml
      (Count:  3; HasMarkup: True; Info: @EditorOptionsFoldInfoHTML[0]), // html
      (Count:  0; HasMarkup: False; Info: nil), // cpp
      (Count:  0; HasMarkup: False; Info: nil), // perl
      (Count:  0; HasMarkup: False; Info: nil), // java
      (Count:  0; HasMarkup: False; Info: nil), // shell
      (Count:  0; HasMarkup: False; Info: nil), // python
      (Count:  0; HasMarkup: False; Info: nil), // php
      (Count:  0; HasMarkup: False; Info: nil), // sql
      (Count:  0; HasMarkup: False; Info: nil), // css
      (Count:  0; HasMarkup: False; Info: nil), // jscript
      (Count:  3; HasMarkup: False; Info: @EditorOptionsFoldInfoDiff[0]), // Diff
      (Count:  0; HasMarkup: False; Info: nil), // Bat
      (Count:  0; HasMarkup: False; Info: nil), // Ini
      (Count:  0; HasMarkup: False; Info: nil), // PO
      (Count:  0; HasMarkup: False; Info: nil)  // Pike
    );

const
  EditorOptsFormatVersion = 12;
  { * Changes in Version 6:
       - ColorSchemes now have a Global settings part.
         Language specific changes must save UseSchemeGlobals=False (Default is true)
         Since Version 5 did not have this setting, in Version 5 the default is false.
    * Changes in Version 7:
         DisableAntialiasing default true to false
    * Changes in Version 8:
         Replaced EditorFontHeight with EditorFontSize.
    * Changes in Version 9:
         Fix ahaMouseLink. It only used Foreground, and would underline with it too.
         It now uses all fields. Old entries will copy Foreground to Frame and
         set sfeBottom
    * Changes in Version 10:
         eoTabIndent was added to SynEditDefaultOptions
    * Changes in Version 11:
         Default for GutterLeft set to moglUpClickAndSelect (was moGLDownClick)
    * Changes in Version 12:
         Used in Colorscheme/Version
         Colors for MarkupFoldColor can now have gaps (before unset colors were filtered)
  }
  EditorMouseOptsFormatVersion = 1;
  { * Changes in Version 6:
      - MouseWheel is nov configurable
  }

  LazSyntaxHighlighterClasses: array[TLazSyntaxHighlighter] of
    TCustomSynClass =
    (nil, nil, TIDESynFreePasSyn, TIDESynPasSyn, TSynLFMSyn, TSynXMLSyn,
    TSynHTMLSyn, TSynCPPSyn, TSynPerlSyn, TSynJavaSyn, TSynUNIXShellScriptSyn,
    TSynPythonSyn, TSynPHPSyn, TSynSQLSyn,TSynCssSyn, TSynJScriptSyn, TSynDiffSyn,
    TSynBatSyn, TSynIniSyn, TSynPoSyn, TSynPikeSyn);


{ Comments }
const
  DefaultCommentTypes: array[TLazSyntaxHighlighter] of TCommentType = (
    comtNone,  // lshNone
    comtNone,  // lshText
    comtPascal,// lshFreePascal
    comtPascal,// lshDelphi
    comtDelphi,// lshLFM
    comtHtml,  // lshXML
    comtHtml,  // lshHTML
    comtCPP,   // lshCPP
    comtPerl,  // lshPerl
    comtCPP,   // lshJava
    comtPerl,  // lshBash
    comtPerl,  // lshPython
    comtHTML,  // lshPHP
    comtCPP,   // lshSQL
    comtCPP,   // lshCss
    comtCPP,   // lshJScript
    comtNone,  // Diff
    comtNone,  // Bat
    comtNone,  // Ini
    comtNone,  // po
    comtCPP    // lshPike
    );

const
  SynEditDefaultOptions = SYNEDIT_DEFAULT_OPTIONS - [eoShowScrollHint]
                                                  + [eoHalfPageScroll, eoTabIndent];
  SynEditDefaultOptions2 = SYNEDIT_DEFAULT_OPTIONS2;

  EditorOptionsMinimumFontSize = 5;

type
  { TEditOptLanguageInfo stores lazarus IDE additional information
    of a highlighter, such as samplesource, which sample lines are special
    lines, file extensions
    MappedAttributes is a list of the format "AttributName=PascalAttributName"
      This mapping attributes are used for default values. For example:
      The comment attribute of HTML is mapped to the comment attribute of
      pascal "Comment=Comment". If there is no mapping attribute for an
      attribute the default values are taken from an untouched highlighter.
      For example Symbol in HTML is not mapped and therefore has as default
      value fo style [fsBold] as defined in synhighlighterhtml.pp.
    }
  TEditOptLanguageInfo = class
  private
    MappedAttributes: TStringList; // map attributes to pascal
  protected
    procedure prepare(Syntax :  TLazSyntaxHighlighter);virtual;
  public
    SynClass: TCustomSynClass;
    TheType:  TLazSyntaxHighlighter;
    FileExtensions: String; // divided by semicolon, e.g. 'pas;pp;inc'
    DefaultFileExtensions: string;
    ColorScheme: String;
    SampleSource: String;
    AddAttrSampleLines: array[TAdditionalHilightAttribute] of Integer; // first line = 1
//    MappedAttributes: TStringList; // map attributes to pascal
    DefaultCommentType: TCommentType;
    CaretXY: TPoint;
    constructor Create;
    destructor Destroy; override;
    function GetDefaultFilextension: String;
    procedure SetBothFilextensions(const Extensions: string);
    function SampleLineToAddAttr(Line: Integer): TAdditionalHilightAttribute;
  end;

  { TEditOptLangCssInfo }

  TEditOptLangCssInfo = class(tEditOptLanguageInfo)
  protected
     procedure prepare(Syntax :  TLazSyntaxHighlighter);override;
     function getSampleSource:string;
     function getMappedAttributes: tStringList;
  end;

  { TEditOptLangList }

  TEditOptLangList = class(TList)
  private
    function GetInfos(Index: Integer): TEditOptLanguageInfo;
  public
    constructor Create;
    procedure Clear; override;
    destructor Destroy; override;
    function FindByName(const Name: String): Integer;
    function FindByClass(CustomSynClass: TCustomSynClass): Integer;
    function FindByHighlighter(Hilighter: TSynCustomHighlighter): Integer;
    function FindByType(AType: TLazSyntaxHighlighter): Integer;
    function GetDefaultFilextension(AType: TLazSyntaxHighlighter): String;
    function GetInfoByType(AType: TLazSyntaxHighlighter): TEditOptLanguageInfo;
    property Items[Index: Integer]: TEditOptLanguageInfo read GetInfos; default;
  end;

  TEditorOptions = class;
  TMouseOptGutterLeftType = (
    moGLDownClick,
    moglUpClickAndSelect,
    moglUpClickAndSelectRighHalf   // Changes and fold gutter (parts close to the text)
  );
  TMouseOptButtonActionOld = (
    mbaNone,
    mbaSelect, mbaSelectColumn, mbaSelectLine,
    //mbaSelectTokens,
    mbaSelectWords,
    //mbaSelectLines,
    mbaSelectSetWord, mbaSelectSetLineSmart, mbaSelectSetLineFull, mbaSelectSetPara,
    mbaPaste,
    mbaDeclarationJump,
    mbaDeclarationOrBlockJump,
    mbaAddHistoryPoint,
    mbaHistoryBack, mbaHistoryForw,
    mbaSetFreeBookmark,
    mbaZoomReset,
    mbaContextMenu,
    mbaContextMenuDebug,
    mbaContextMenuTab,

    mbaMultiCaretToggle,

    // Old values, needed to load old config
    moTCLNone, moTMIgnore,
    moTMPaste,
    moTMDeclarationJump, moTCLJump,
    moTCLJumpOrBlock
  );

  TMouseOptButtonAction = mbaNone..mbaMultiCaretToggle;

const
  MouseOptButtonActionOld: Array [moTCLNone..moTCLJumpOrBlock] of TMouseOptButtonActionOld = (
    mbaNone, mbaNone,
    mbaPaste,
    mbaDeclarationJump, mbaDeclarationJump,
    mbaDeclarationOrBlockJump
  );

type
  TMouseOptWheelAction = (
    mwaNone,
    mwaScroll, mwaScrollSingleLine,
    mwaScrollPage, mwaScrollPageLessOne, mwaScrollHalfPage,
    mwaScrollHoriz, mwaScrollHorizSingleLine,
    mwaScrollHorizPage, mwaScrollHorizPageLessOne, mwaScrollHorizHalfPage,
    mwaZoom
  );

  { TEditorMouseOptions }

  TEditorMouseOptions = class(TPersistent)
  private
    FGutterLeft: TMouseOptGutterLeftType;
    FTextDrag: Boolean;
    FTextRightMoveCaret: Boolean;
    FUserSchemes: TQuickStringlist;
  private
    FCustomSavedActions: Boolean;
    FGutterActionsChanges: TSynEditMouseActions;
    FMainActions, FSelActions, FTextActions: TSynEditMouseActions;
    FSelectOnLineNumbers: Boolean;
    FName: String;
    FGutterActions: TSynEditMouseActions;
    FGutterActionsFold, FGutterActionsFoldExp, FGutterActionsFoldCol: TSynEditMouseActions;
    FGutterActionsLines: TSynEditMouseActions;
    FGutterActionsOverView, FGutterActionsOverViewMarks: TSynEditMouseActions;
    FSelectedUserScheme: String;
    // left multi click
    FTextDoubleLeftClick: TMouseOptButtonAction;
    FTextTripleLeftClick: TMouseOptButtonAction;
    FTextQuadLeftClick: TMouseOptButtonAction;
    FTextShiftDoubleLeftClick: TMouseOptButtonAction;
    FTextAltDoubleLeftClick: TMouseOptButtonAction;
    FTextCtrlDoubleLeftClick: TMouseOptButtonAction;
    // left + modifier click
    FTextShiftLeftClick: TMouseOptButtonAction;
    FTextAltLeftClick: TMouseOptButtonAction;
    FTextCtrlLeftClick: TMouseOptButtonActionOld;
    FTextAltCtrlLeftClick: TMouseOptButtonAction;
    FTextShiftAltLeftClick: TMouseOptButtonAction;
    FTextShiftCtrlLeftClick: TMouseOptButtonAction;
    FTextShiftAltCtrlLeftClick: TMouseOptButtonAction;
    // middle click
    FTextMiddleClick: TMouseOptButtonActionOld;
    FTextAltMiddleClick: TMouseOptButtonAction;
    FTextCtrlMiddleClick: TMouseOptButtonAction;
    FTextAltCtrlMiddleClick: TMouseOptButtonAction;
    FTextShiftAltMiddleClick: TMouseOptButtonAction;
    FTextShiftAltCtrlMiddleClick: TMouseOptButtonAction;
    FTextShiftCtrlMiddleClick: TMouseOptButtonAction;
    FTextShiftMiddleClick: TMouseOptButtonAction;
    // right
    FTextAltCtrlRightClick: TMouseOptButtonAction;
    FTextAltRightClick: TMouseOptButtonAction;
    FTextCtrlRightClick: TMouseOptButtonAction;
    FTextRightClick: TMouseOptButtonAction;
    FTextShiftAltCtrlRightClick: TMouseOptButtonAction;
    FTextShiftAltRightClick: TMouseOptButtonAction;
    FTextShiftCtrlRightClick: TMouseOptButtonAction;
    FTextShiftRightClick: TMouseOptButtonAction;
    // extra-1 click
    FTextAltCtrlExtra1Click: TMouseOptButtonAction;
    FTextAltExtra1Click: TMouseOptButtonAction;
    FTextCtrlExtra1Click: TMouseOptButtonAction;
    FTextExtra1Click: TMouseOptButtonAction;
    FTextShiftAltCtrlExtra1Click: TMouseOptButtonAction;
    FTextShiftAltExtra1Click: TMouseOptButtonAction;
    FTextShiftCtrlExtra1Click: TMouseOptButtonAction;
    FTextShiftExtra1Click: TMouseOptButtonAction;
    // extra-2 click
    FTextAltCtrlExtra2Click: TMouseOptButtonAction;
    FTextAltExtra2Click: TMouseOptButtonAction;
    FTextCtrlExtra2Click: TMouseOptButtonAction;
    FTextExtra2Click: TMouseOptButtonAction;
    FTextShiftAltCtrlExtra2Click: TMouseOptButtonAction;
    FTextShiftAltExtra2Click: TMouseOptButtonAction;
    FTextShiftCtrlExtra2Click: TMouseOptButtonAction;
    FTextShiftExtra2Click: TMouseOptButtonAction;
    FVersion: Integer;
    // wheel
    FWheel: TMouseOptWheelAction;
    FAltWheel: TMouseOptWheelAction;
    FCtrlWheel: TMouseOptWheelAction;
    FShiftWheel: TMouseOptWheelAction;
    FShiftAltWheel: TMouseOptWheelAction;
    FShiftCtrlWheel: TMouseOptWheelAction;
    FAltCtrlWheel: TMouseOptWheelAction;
    FShiftAltCtrlWheel: TMouseOptWheelAction;

    procedure ClearUserSchemes;
    function GetUserSchemeNames(Index: Integer): String;
    function GetUserSchemes(Index: String): TEditorMouseOptions;
    function GetUserSchemesAtPos(Index: Integer): TEditorMouseOptions;
    function  GetSelectedUserSchemeIndex: Integer;
    procedure SetSelectedUserScheme(const AValue: String);
    procedure SetSelectedUserSchemeIndex(const AValue: Integer);
    procedure AssignActions(Src: TEditorMouseOptions);
    procedure SetTextCtrlLeftClick(AValue: TMouseOptButtonActionOld);
    procedure SetTextMiddleClick(AValue: TMouseOptButtonActionOld);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure ResetGutterToDefault;
    procedure ResetTextToDefault;
    procedure ResetToUserScheme;
    procedure AssignEx(Src: TEditorMouseOptions; WithUserSchemes: Boolean);
    procedure Assign(Src: TEditorMouseOptions); reintroduce;
    function  IsPresetEqualToMouseActions: Boolean;
    function  CalcCustomSavedActions: Boolean;
    procedure LoadFromXml(aXMLConfig: TRttiXMLConfig; aPath: String; aOldPath: String; FileVersion: Integer);
    procedure SaveToXml(aXMLConfig: TRttiXMLConfig; aPath: String);
    procedure ImportFromXml(aXMLConfig: TRttiXMLConfig; aPath: String);
    procedure ExportToXml(aXMLConfig: TRttiXMLConfig; aPath: String);
    procedure LoadUserSchemes;
    function  UserSchemeCount: Integer;
    function  IndexOfUserScheme(SchemeName: String): Integer;

    property Name: String read FName;
    property UserSchemes[Index: String]: TEditorMouseOptions read GetUserSchemes;
    property UserSchemesAtPos[Index: Integer]: TEditorMouseOptions read GetUserSchemesAtPos;
    property UserSchemeNames[Index: Integer]: String read GetUserSchemeNames;
    property SelectedUserSchemeIndex: Integer
             read GetSelectedUserSchemeIndex write SetSelectedUserSchemeIndex;

    property MainActions: TSynEditMouseActions read FMainActions;
    property SelActions: TSynEditMouseActions read FSelActions;
    property TextActions: TSynEditMouseActions read FTextActions;
    property GutterActions: TSynEditMouseActions read FGutterActions;
    property GutterActionsFold: TSynEditMouseActions read FGutterActionsFold;
    property GutterActionsFoldExp: TSynEditMouseActions read FGutterActionsFoldExp;
    property GutterActionsFoldCol: TSynEditMouseActions read FGutterActionsFoldCol;
    property GutterActionsLines: TSynEditMouseActions read FGutterActionsLines;
    property GutterActionsChanges: TSynEditMouseActions read FGutterActionsChanges;
    property GutterActionsOverView: TSynEditMouseActions read FGutterActionsOverView;
    property GutterActionsOverViewMarks: TSynEditMouseActions read FGutterActionsOverViewMarks;
  published
    property GutterLeft: TMouseOptGutterLeftType read FGutterLeft write FGutterLeft
             default moglUpClickAndSelect;
    property SelectOnLineNumbers: Boolean read FSelectOnLineNumbers write FSelectOnLineNumbers
             default True;
    property TextDrag: Boolean read FTextDrag write FTextDrag
             default True;
    property TextRightMoveCaret: Boolean read FTextRightMoveCaret  write FTextRightMoveCaret
             default False;
    // left multi click
    property TextDoubleLeftClick: TMouseOptButtonAction read FTextDoubleLeftClick write FTextDoubleLeftClick
             default mbaSelectSetWord;
    property TextTripleLeftClick: TMouseOptButtonAction read FTextTripleLeftClick write FTextTripleLeftClick
             default mbaSelectSetLineSmart;
    property TextQuadLeftClick: TMouseOptButtonAction read FTextQuadLeftClick write FTextQuadLeftClick
             default mbaSelectSetPara;
    property TextShiftDoubleLeftClick: TMouseOptButtonAction read FTextShiftDoubleLeftClick write FTextShiftDoubleLeftClick
             default mbaNone;
    property TextCtrlDoubleLeftClick: TMouseOptButtonAction read FTextCtrlDoubleLeftClick write FTextCtrlDoubleLeftClick
             default mbaNone;
    property TextAltDoubleLeftClick: TMouseOptButtonAction read FTextAltDoubleLeftClick write FTextAltDoubleLeftClick
             default mbaNone;
    // left + modifier click
    property TextShiftLeftClick: TMouseOptButtonAction read FTextShiftLeftClick write FTextShiftLeftClick
             default mbaNone;  // continue selection
    property TextCtrlLeftClick: TMouseOptButtonActionOld read FTextCtrlLeftClick write SetTextCtrlLeftClick
             default mbaDeclarationJump;
    property TextAltLeftClick: TMouseOptButtonAction read FTextAltLeftClick write FTextAltLeftClick
             default mbaSelectColumn;
    property TextShiftCtrlLeftClick: TMouseOptButtonAction read FTextShiftCtrlLeftClick write FTextShiftCtrlLeftClick
             default mbaMultiCaretToggle;  // continue selection
    property TextShiftAltLeftClick: TMouseOptButtonAction read FTextShiftAltLeftClick write FTextShiftAltLeftClick
             default mbaNone;  // continue selection
    property TextAltCtrlLeftClick: TMouseOptButtonAction read FTextAltCtrlLeftClick write FTextAltCtrlLeftClick
             default mbaNone;
    property TextShiftAltCtrlLeftClick: TMouseOptButtonAction read FTextShiftAltCtrlLeftClick write FTextShiftAltCtrlLeftClick
             default mbaNone;
    // middle click
    property TextMiddleClick: TMouseOptButtonActionOld read FTextMiddleClick write SetTextMiddleClick
             default mbaPaste;
    property TextShiftMiddleClick: TMouseOptButtonAction read FTextShiftMiddleClick write FTextShiftMiddleClick
             default mbaNone;
    property TextAltMiddleClick: TMouseOptButtonAction read FTextAltMiddleClick write FTextAltMiddleClick
             default mbaNone;
    property TextCtrlMiddleClick: TMouseOptButtonAction read FTextCtrlMiddleClick write FTextCtrlMiddleClick
             default mbaZoomReset;
    property TextShiftAltMiddleClick: TMouseOptButtonAction read FTextShiftAltMiddleClick write FTextShiftAltMiddleClick
             default mbaNone;
    property TextShiftCtrlMiddleClick: TMouseOptButtonAction read FTextShiftCtrlMiddleClick write FTextShiftCtrlMiddleClick
             default mbaNone;
    property TextAltCtrlMiddleClick: TMouseOptButtonAction read FTextAltCtrlMiddleClick write FTextAltCtrlMiddleClick
             default mbaNone;
    property TextShiftAltCtrlMiddleClick: TMouseOptButtonAction read FTextShiftAltCtrlMiddleClick write FTextShiftAltCtrlMiddleClick
             default mbaNone;
    // right click
    property TextRightClick: TMouseOptButtonAction read FTextRightClick write FTextRightClick
             default mbaContextMenu;
    property TextShiftRightClick: TMouseOptButtonAction read FTextShiftRightClick write FTextShiftRightClick
             default mbaNone;
    property TextAltRightClick: TMouseOptButtonAction read FTextAltRightClick write FTextAltRightClick
             default mbaNone;
    property TextCtrlRightClick: TMouseOptButtonAction read FTextCtrlRightClick write FTextCtrlRightClick
             default mbaContextMenuTab;
    property TextShiftAltRightClick: TMouseOptButtonAction read FTextShiftAltRightClick write FTextShiftAltRightClick
             default mbaNone;
    property TextShiftCtrlRightClick: TMouseOptButtonAction read FTextShiftCtrlRightClick write FTextShiftCtrlRightClick
             default mbaNone;
    property TextAltCtrlRightClick: TMouseOptButtonAction read FTextAltCtrlRightClick write FTextAltCtrlRightClick
             default mbaNone;
    property TextShiftAltCtrlRightClick: TMouseOptButtonAction read FTextShiftAltCtrlRightClick write FTextShiftAltCtrlRightClick
             default mbaNone;
    // extra-1 click
    property TextExtra1Click: TMouseOptButtonAction read FTextExtra1Click write FTextExtra1Click
             default mbaHistoryBack;
    property TextShiftExtra1Click: TMouseOptButtonAction read FTextShiftExtra1Click write FTextShiftExtra1Click
             default mbaNone;
    property TextAltExtra1Click: TMouseOptButtonAction read FTextAltExtra1Click write FTextAltExtra1Click
             default mbaNone;
    property TextCtrlExtra1Click: TMouseOptButtonAction read FTextCtrlExtra1Click write FTextCtrlExtra1Click
             default mbaNone;
    property TextShiftAltExtra1Click: TMouseOptButtonAction read FTextShiftAltExtra1Click write FTextShiftAltExtra1Click
             default mbaNone;
    property TextShiftCtrlExtra1Click: TMouseOptButtonAction read FTextShiftCtrlExtra1Click write FTextShiftCtrlExtra1Click
             default mbaNone;
    property TextAltCtrlExtra1Click: TMouseOptButtonAction read FTextAltCtrlExtra1Click write FTextAltCtrlExtra1Click
             default mbaNone;
    property TextShiftAltCtrlExtra1Click: TMouseOptButtonAction read FTextShiftAltCtrlExtra1Click write FTextShiftAltCtrlExtra1Click
             default mbaNone;
    // extra-2 click
    property TextExtra2Click: TMouseOptButtonAction read FTextExtra2Click write FTextExtra2Click
             default mbaHistoryForw;
    property TextShiftExtra2Click: TMouseOptButtonAction read FTextShiftExtra2Click write FTextShiftExtra2Click
             default mbaNone;
    property TextAltExtra2Click: TMouseOptButtonAction read FTextAltExtra2Click write FTextAltExtra2Click
             default mbaNone;
    property TextCtrlExtra2Click: TMouseOptButtonAction read FTextCtrlExtra2Click write FTextCtrlExtra2Click
             default mbaNone;
    property TextShiftAltExtra2Click: TMouseOptButtonAction read FTextShiftAltExtra2Click write FTextShiftAltExtra2Click
             default mbaNone;
    property TextShiftCtrlExtra2Click: TMouseOptButtonAction read FTextShiftCtrlExtra2Click write FTextShiftCtrlExtra2Click
             default mbaNone;
    property TextAltCtrlExtra2Click: TMouseOptButtonAction read FTextAltCtrlExtra2Click write FTextAltCtrlExtra2Click
             default mbaNone;
    property TextShiftAltCtrlExtra2Click: TMouseOptButtonAction read FTextShiftAltCtrlExtra2Click write FTextShiftAltCtrlExtra2Click
             default mbaNone;
    //
    property Wheel: TMouseOptWheelAction read FWheel write FWheel
             default mwaScroll;
    property CtrlWheel: TMouseOptWheelAction read FCtrlWheel write FCtrlWheel
             default mwaZoom;
    property AltWheel: TMouseOptWheelAction read FAltWheel write FAltWheel
             default mwaScrollPageLessOne;
    property ShiftWheel: TMouseOptWheelAction read FShiftWheel write FShiftWheel
             default mwaScrollSingleLine;
    property ShiftAltWheel: TMouseOptWheelAction read FShiftAltWheel write FShiftAltWheel
             default mwaNone;
    property ShiftCtrlWheel: TMouseOptWheelAction read FShiftCtrlWheel write FShiftCtrlWheel
             default mwaNone;
    property AltCtrlWheel: TMouseOptWheelAction read FAltCtrlWheel write FAltCtrlWheel
             default mwaNone;
    property ShiftAltCtrlWheel: TMouseOptWheelAction read FShiftAltCtrlWheel write FShiftAltCtrlWheel
             default mwaNone;

    // the flag below is set by CalcCustomSavedActions
    property CustomSavedActions: Boolean read FCustomSavedActions write FCustomSavedActions;
    property SelectedUserScheme: String read FSelectedUserScheme write SetSelectedUserScheme;
    property Version : Integer read FVersion write FVersion;
  end;

  { TEditorMouseOptionPresets }

  TEditorMouseOptionPresets = class
  private
    FPreset: TQuickStringlist;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TEditorOptionsEditAccessInViewState =
    (eoeaIgnoreInView,          // Find any editor
     eoeaInViewOnly,            // Only editors, with the jump-target in their current visible area
     eoeaInViewSoftCenterOnly   // Only editors, with the jump-target in their current visible soft center (exclude up to 5 lines top/bottom)
    );
  TEditorOptionsEditAccessLockedState =
    (eoeaIgnoreLock,     // Find any editor
     eoeaLockedOnly,     // Only use locked Editor (e.g for InView = eoeaInViewOnly)
     eoeaUnlockedOnly,   // Only use unlocked Editors (default)
     eoeaLockedFirst,    // Search locked Editors first (each group according to Order)
     eoeaLockedLast      // Search locked Editoes last
    );
  TEditorOptionsEditAccessOrder =
    (eoeaOrderByEditFocus,       // prefer the editors in the order they were last focused
     eoeaOrderByWindowFocus,     // prefer the editors in the order their window was last focused
     eoeaOrderByOldestEditFocus, // Reverse order by last focused
     eoeaOrderByOldestWindowFocus,
     eoeaOnlyCurrentEdit,        // search only the current-active editor (and only if it has the correct file)
     eoeaOnlyCurrentWindow,      // search only the current window (if it has an editor for the desired file)
     eoeaOrderByListPref         // follow global setting on the list
    );
  TEditorOptionsEditAccessOpenNew =
    (eoeaNoNewTab,                     // Do not open a new tab, if none found
     eoeaNewTabInExistingWindowOnly,   // Open a new tab in existing (last focus) window, if possible
     eoeaNewTabInNewWindowOnly,        // Open a new tab in new window
     eoeaNewTabInExistingOrNewWindow   // Open a new tab in existing or new window
    );
  TEditorOptionsEditAccessDefaultEntry = record
    SearchLocked: TEditorOptionsEditAccessLockedState;
    SearchInView: TEditorOptionsEditAccessInViewState;
    SearchOrder: TEditorOptionsEditAccessOrder;
    SearchOpenNew: TEditorOptionsEditAccessOpenNew;
    Enabled: Boolean;
    ID: String;
    Caption, Desc: String;
  end;
  TEditorOptionsEditAccessDefaults = Array [0..8] of TEditorOptionsEditAccessDefaultEntry;

const
  // captions and desc are set in TEditorOptions.Create
  EditorOptionsEditAccessDefaults: TEditorOptionsEditAccessDefaults =
  ( // Find locked - InView
    (SearchLocked: eoeaLockedOnly;        SearchInView:  eoeaInViewOnly;
     SearchOrder:  eoeaOrderByListPref;   SearchOpenNew: eoeaNoNewTab;
     Enabled:      True;                  ID:            'Locked_InView';
     Caption: '';                         Desc: '' ),
    // Find unlocked
    (SearchLocked: eoeaUnlockedOnly;      SearchInView:  eoeaInViewSoftCenterOnly;
     SearchOrder:  eoeaOrderByListPref;   SearchOpenNew: eoeaNoNewTab;
     Enabled:      False;                 ID:            'UnLocked_InSoftView';
     Caption: '';                         Desc: '' ),
    (SearchLocked: eoeaUnlockedOnly;      SearchInView:  eoeaIgnoreInView;
     SearchOrder:  eoeaOrderByListPref;   SearchOpenNew: eoeaNoNewTab;
     Enabled:      True;                  ID:            'UnLocked';
     Caption: '';                         Desc: '' ),
    // open new tab
    (SearchLocked: eoeaUnlockedOnly;      SearchInView:  eoeaIgnoreInView;
     SearchOrder:  eoeaOrderByListPref;   SearchOpenNew: eoeaNewTabInExistingWindowOnly;
     Enabled:      False;                 ID:            'UnLocked_OpenNewInOldWin';
     Caption: '';                         Desc: '' ),
    (SearchLocked: eoeaUnlockedOnly;      SearchInView:  eoeaIgnoreInView;
     SearchOrder:  eoeaOrderByListPref;   SearchOpenNew: eoeaNewTabInNewWindowOnly;
     Enabled:      False;                 ID:            'UnLocked_OpenNewInNewWin';
     Caption: '';                         Desc: '' ),
    // Ignore locks
    (SearchLocked: eoeaIgnoreLock;        SearchInView:  eoeaIgnoreInView;
     SearchOrder:  eoeaOrderByOldestEditFocus; SearchOpenNew: eoeaNoNewTab;
     Enabled:      False;                 ID:            'IgnLocked_OldEdit';
     Caption: '';                         Desc: '' ),
    (SearchLocked: eoeaIgnoreLock;        SearchInView:  eoeaIgnoreInView;
     SearchOrder:  eoeaOnlyCurrentEdit;   SearchOpenNew: eoeaNoNewTab;
     Enabled:      False;                 ID:            'IgnLocked_OnlyActEdit';
     Caption: '';                         Desc: '' ),
    (SearchLocked: eoeaIgnoreLock;        SearchInView:  eoeaIgnoreInView;
     SearchOrder:  eoeaOnlyCurrentWindow; SearchOpenNew: eoeaNoNewTab;
     Enabled:      False;                 ID:            'IgnLocked_OnlyActWin';
     Caption: '';                         Desc: '' ),
    // Fallback (must be last)
    (SearchLocked: eoeaUnlockedOnly;      SearchInView:  eoeaIgnoreInView;
     SearchOrder:  eoeaOrderByListPref;   SearchOpenNew: eoeaNewTabInExistingOrNewWindow;
     Enabled:      True;                  ID:            'UnLocked_OpenNewInAnyWin';
     Caption: '';                         Desc: '' )
  );
  EditorOptionsEditAccessUserDef: TEditorOptionsEditAccessDefaultEntry =
    (SearchLocked: eoeaUnlockedOnly;      SearchInView:  eoeaIgnoreInView;
     SearchOrder:  eoeaOrderByListPref;   SearchOpenNew: eoeaNoNewTab;
     Enabled:      True;                  ID:            '';
     Caption: '';                         Desc: '' );

type

  TEditorOptionsEditAccessOrderList = class;

  { TEditorOptionsEditAccessOrderEntry }

  TEditorOptionsEditAccessOrderEntry = class(TPersistent)
  private
    FId: String;
    FList: TEditorOptionsEditAccessOrderList;
    FCaption: String;
    FDesc: String;
    FEnabled: Boolean;
    FIsFallback: Boolean;
    FDefaults: TEditorOptionsEditAccessOrderEntry;
    FSearchInView: TEditorOptionsEditAccessInViewState;
    FSearchLocked: TEditorOptionsEditAccessLockedState;
    FSearchOpenNew: TEditorOptionsEditAccessOpenNew;
    FSearchOrder: TEditorOptionsEditAccessOrder;
    procedure AssignFrom(AValue: TEditorOptionsEditAccessDefaultEntry);
    procedure SetEnabled(const AValue: Boolean);
  public
    constructor Create(AList: TEditorOptionsEditAccessOrderList);
    destructor Destroy; override;
    procedure Assign(Src: TEditorOptionsEditAccessOrderEntry); reintroduce;
    procedure InitFrom(AValue: TEditorOptionsEditAccessDefaultEntry);
  public
    function RealSearchOrder: TEditorOptionsEditAccessOrder;
    property Defaults: TEditorOptionsEditAccessOrderEntry read FDefaults;
    property ID: String read FId write FId;
    property IsFallback: Boolean read FIsFallback;
    property Desc: String read FDesc write FDesc;
  //published
    property Caption: String
             read FCaption write FCaption;
  published
    property Enabled: Boolean
             read FEnabled write SetEnabled;
  public
    property SearchLocked: TEditorOptionsEditAccessLockedState
             read FSearchLocked write FSearchLocked;
    property SearchInView: TEditorOptionsEditAccessInViewState
             read FSearchInView write FSearchInView;
    property SearchOrder: TEditorOptionsEditAccessOrder
             read FSearchOrder write FSearchOrder;
    property SearchOpenNew: TEditorOptionsEditAccessOpenNew
             read FSearchOpenNew write FSearchOpenNew;
    //property IgnoreTopLineAdjustment;
  end;

  { TEditorOptionsEditAccessOrderList }

  TEditorOptionsEditAccessOrderList = class(TPersistent)
  private
    FList: TFPList;
    FSearchOrder: TEditorOptionsEditAccessOrder;
    function GetItems(Index: Integer): TEditorOptionsEditAccessOrderEntry;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure InitDefaults;
    procedure Assign(Src: TEditorOptionsEditAccessOrderList); reintroduce;
    procedure LoadFromXMLConfig(XMLConfig:TRttiXMLConfig; Path: String);
    procedure SaveToXMLConfig(XMLConfig:TRttiXMLConfig; Path: String);
    function Count: Integer;
    property Items[Index: Integer]: TEditorOptionsEditAccessOrderEntry
             read GetItems; default;
  published
    property SearchOrder: TEditorOptionsEditAccessOrder
             read FSearchOrder write FSearchOrder;
  end;

const
  EditorUserDefinedWordsKeyCatName = 'User defined word markup';
  MARKUP_USER_DEF_PRIOR = 3500;

var
  EditorUserDefinedWordsGlobalId: string = 'a';

type

  TEditorUserDefinedWordsList = class;

  { TEditorUserDefinedWords }

  TEditorUserDefinedWords = class(TSourceSynSearchTermList)
  private
    FGlobalList: Boolean;
    FGlobalTermsCache: TSynSearchTermDict;
    FId: String; // Used for TIDECommand.Name
    FKeyAddCase: Boolean;
    FKeyAddSelectBoundMaxLen: Integer;
    FKeyAddSelectSmart: Boolean;
    FKeyAddTermBounds: TSynSearchTermOptsBounds;
    FKeyAddWordBoundMaxLen: Integer;
    FList: TEditorUserDefinedWordsList;
    FColorAttr: TColorSchemeAttribute;
    FName: String;
    FAddTermCmd: TIDECommand;
    FRemoveTermCmd: TIDECommand;
    FToggleTermCmd: TIDECommand;
    procedure SetGlobalTermsCache(AValue: TSynSearchTermDict);
    procedure SetName(AValue: String);
    procedure UpdateIdeCommands;
    procedure ClearIdeCommands;
  protected
    property GlobalTermsCache: TSynSearchTermDict read FGlobalTermsCache write SetGlobalTermsCache;
  public
    constructor Create(AList: TEditorUserDefinedWordsList);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure LoadFromXMLConfig(XMLConfig:TRttiXMLConfig; Path: String);
    procedure SaveToXMLConfig(XMLConfig:TRttiXMLConfig; Path: String);

    property ColorAttr: TColorSchemeAttribute read FColorAttr;

    function HasKeyAssigned: Boolean;
    property AddTermCmd: TIDECommand read FAddTermCmd;
    property RemoveTermCmd: TIDECommand read FRemoveTermCmd;
    property ToggleTermCmd: TIDECommand read FToggleTermCmd;

    //property MatchWordBounds: TSynSearchTermOptsBounds read FMatchWordBounds write SetMatchWordBounds;
    //property MatchCase: Boolean read FMatchCase write SetMatchCase;
  published
    property Name: String read FName write SetName;
    property KeyAddTermBounds: TSynSearchTermOptsBounds read FKeyAddTermBounds write FKeyAddTermBounds;
    property KeyAddCase: Boolean read FKeyAddCase write FKeyAddCase;
    property KeyAddWordBoundMaxLen: Integer read FKeyAddWordBoundMaxLen write FKeyAddWordBoundMaxLen;
    property KeyAddSelectBoundMaxLen: Integer read FKeyAddSelectBoundMaxLen write FKeyAddSelectBoundMaxLen;
    property KeyAddSelectSmart: Boolean read FKeyAddSelectSmart write FKeyAddSelectSmart;
    property GlobalList: Boolean read FGlobalList write FGlobalList;
  end;

  { TEditorUserDefinedWordsList }

  TEditorUserDefinedWordsList = class(TPersistent)
  private
    FList: TList;
    FKeyCommandList: TIDECommands;
    FUseGlobalIDECommandList: Boolean;
    function GetKeyCommandList: TIDECommands;
    function GetLists(AIndex: Integer): TEditorUserDefinedWords;
    procedure SetLists(AIndex: Integer; AValue: TEditorUserDefinedWords);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Src: TEditorUserDefinedWordsList); reintroduce;

    procedure LoadFromXMLConfig(XMLConfig:TRttiXMLConfig; Path: String);
    procedure SaveToXMLConfig(XMLConfig:TRttiXMLConfig; Path: String);

    procedure Clear;
    function  Add(AList: TEditorUserDefinedWords): Integer;
    function  Add(AName: String): TEditorUserDefinedWords;
    function  IndexOf(AName: String): Integer;
    function  IndexOf(AList: TEditorUserDefinedWords): Integer;
    procedure Remove(AName: String; {%H-}FreeList: Boolean = True);
    procedure Remove(AList: TEditorUserDefinedWords; FreeList: Boolean = True);
    procedure Delete(AIndex: Integer);
    function  Count: Integer;
    property Lists[AIndex: Integer]: TEditorUserDefinedWords read GetLists write SetLists;
    property KeyCommandList: TIDECommands read GetKeyCommandList write FKeyCommandList;
    property UseGlobalIDECommandList: Boolean read FUseGlobalIDECommandList write FUseGlobalIDECommandList;
  end;

  { TEditorOptions - Editor Options object used to hold the editor options }

  TEditorOptions = class(TIDEEditorOptions)
  private
    FBlockTabIndent: Integer;
    FCompletionLongLineHintInMSec: Integer;
    FCompletionLongLineHintType: TSynCompletionLongHintType;
    FMultiCaretDefaultColumnSelectMode: TSynPluginMultiCaretDefaultMode;
    FMultiCaretDefaultMode: TSynPluginMultiCaretDefaultMode;
    FMultiCaretDeleteSkipLineBreak: Boolean;
    FPasExtendedKeywordsMode: Boolean;
    FHideSingleTabInWindow: Boolean;
    FPasStringKeywordMode: TSynPasStringMode;
    FTopInfoView: boolean;
    {$IFDEF WinIME}
    FUseMinimumIme: Boolean;
    {$ENDIF}
    xmlconfig: TRttiXMLConfig;

    // general options
    fFindTextAtCursor: Boolean;
    fShowTabCloseButtons: Boolean;
    FMultiLineTab: Boolean;
    fShowTabNumbers: Boolean;
    fUseTabHistory: Boolean;
    fTabPosition: TTabPosition;
    fSynEditOptions: TSynEditorOptions;
    fSynEditOptions2: TSynEditorOptions2;
    fUndoAfterSave: Boolean;
    fUseSyntaxHighlight: Boolean;
    FCopyWordAtCursorOnCopyNone: Boolean;
    FShowGutterHints: Boolean;
    fBlockIndent: Integer;
    fBlockIndentType: TSynBeautifierIndentType;
    FTrimSpaceType: TSynEditStringTrimmingType;
    fUndoLimit: Integer;
    fTabWidth:  Integer;
    FBracketHighlightStyle: TSynEditBracketHighlightStyle;
    FMultiCaretOnColumnSelect: Boolean;

    // Display options
    fVisibleRightMargin: Boolean;
    fVisibleGutter: Boolean;
    fShowLineNumbers: Boolean;
    fShowOnlyLineNumbersMultiplesOf: integer;
    FShowOverviewGutter: boolean;
    fGutterWidth: Integer;
    FGutterSeparatorIndex: Integer;
    fRightMargin: Integer;
    fEditorFont:  String;
    fEditorFontSize:   Integer;
    fExtraCharSpacing: Integer;
    fExtraLineSpacing: Integer;
    fDisableAntialiasing: Boolean;
    FDoNotWarnForFont: string;

    // Key Mappings options
    fKeyMappingScheme: String;
    fKeyMap: TKeyCommandRelationList;

    // Mouse Mappings options
    FUserMouseSettings: TEditorMouseOptions;
    FTempMouseSettings: TEditorMouseOptions;

    // Color options
    fHighlighterList: TEditOptLangList;
    FUserColorSchemeSettings: TColorSchemeFactory;
    FUserDefinedColors: TEditorUserDefinedWordsList;

    // Markup Current Word
    FMarkupCurWordTime: Integer;
    FMarkupCurWordFullLen: Integer;
    FMarkupCurWordNoKeyword: Boolean;
    FMarkupCurWordTrim: Boolean;
    FMarkupCurWordNoTimer: Boolean;

    // Code tools options (MG: these will move to an unit of their own)
    fAutoBlockCompletion: Boolean;
    fAutoCodeParameters: Boolean;
    fAutoDelayInMSec: Integer;
    FAutoRemoveEmptyMethods: Boolean;
    fAutoToolTipExprEval: Boolean;
    fAutoToolTipSymbTools: Boolean;
    FDbgHintAutoTypeCastClass: Boolean;
    fCodeTemplateFileNameRaw: String;
    fCTemplIndentToTokenStart: Boolean;
    fAutoDisplayFuncPrototypes: Boolean;

    // Code Folding
    FUseCodeFolding: Boolean;
    FUseMarkupWordBracket: Boolean;
    FUseMarkupOutline: Boolean;
    FReverseFoldPopUpOrder: Boolean;

    // Multi window
    FMultiWinEditAccessOrder: TEditorOptionsEditAccessOrderList;
    FCtrlMiddleTabClickClosesOthers: Boolean;
    FShowFileNameInCaption: Boolean;

    // Comment Continue
    FAnsiCommentContinueEnabled: Boolean;
    FAnsiCommentMatch: String;
    FAnsiCommentMatchMode: TSynCommentMatchMode;
    FAnsiCommentPrefix: String;
    FAnsiIndentMode: TSynCommentIndentFlags;
    FAnsiIndentAlignMax: integer;

    FCurlyCommentContinueEnabled: Boolean;
    FCurlyCommentMatch: String;
    FCurlyCommentMatchMode: TSynCommentMatchMode;
    FCurlyCommentPrefix: String;
    FCurlyIndentMode: TSynCommentIndentFlags;
    FCurlyIndentAlignMax: integer;

    FSlashCommentContinueEnabled: Boolean;
    FSlashCommentMatch: String;
    FSlashCommentMatchMode: TSynCommentMatchMode;
    FSlashCommentPrefix: String;
    FSlashIndentMode: TSynCommentIndentFlags;
    FSlashCommentExtend: TSynCommentExtendMode;
    FSlashIndentAlignMax: integer;

    FStringBreakAppend: String;
    FStringBreakEnabled: Boolean;
    FStringBreakPrefix: String;

    FDefaultValues: TEditorOptions;
    function GetCodeTemplateFileNameExpand:String;
  protected
    function GetTabPosition: TTabPosition; override;
  public
    class function GetGroupCaption:string; override;
    class function GetInstance: TAbstractIDEOptions; override;
    procedure DoAfterWrite(Restore: boolean); override;
  public
    constructor Create;
    constructor CreateDefaultOnly;
    destructor Destroy; override;
    procedure Init;
    procedure Load;
    procedure Save;
    procedure TranslateResourceStrings;
    function GetAdditionalAttributeName(aha:TAdditionalHilightAttribute): string;
    function GetSynEditOptionName(SynOption: TSynEditorOption): string;
    function GetSynBeautifierIndentName(IndentType: TSynBeautifierIndentType): string;
    function GetSynBeautifierIndentType(IndentName: String): TSynBeautifierIndentType;
    function GetTrimSpaceName(IndentType: TSynEditStringTrimmingType): string;
    function GetTrimSpaceType(IndentName: String): TSynEditStringTrimmingType;

    procedure AssignKeyMapTo(ASynEdit: TSynEdit; SimilarEdit: TSynEdit = nil); // Or copy fromSimilarEdit
    procedure GetHighlighterSettings(Syn: TSrcIDEHighlighter); // read highlight settings from config file
    procedure GetSynEditSettings(ASynEdit: TSynEdit; SimilarEdit: TSynEdit = nil); // read synedit settings from config file
    procedure GetSynEditPreviewSettings(APreviewEditor: TObject);
    procedure ApplyFontSettingsTo(ASynEdit: TSynEdit);

    function ExtensionToLazSyntaxHighlighter(Ext: String): TLazSyntaxHighlighter; override;
    function CreateSyn(LazSynHilighter: TLazSyntaxHighlighter): TSrcIDEHighlighter;
    function ReadColorScheme(const LanguageName: String): String;
    function ReadPascalColorScheme: String;
    procedure WriteColorScheme(const LanguageName, SynColorScheme: String);
    procedure ReadHighlighterSettings(Syn: TSrcIDEHighlighter;
                                      SynColorScheme: String);

    procedure ReadHighlighterFoldSettings(Syn: TSrcIDEHighlighter; ReadForOptions: Boolean = False);
    procedure ReadDefaultsForHighlighterFoldSettings(Syn: TSrcIDEHighlighter);
    procedure WriteHighlighterFoldSettings(Syn: TSrcIDEHighlighter);

    procedure ReadHighlighterDivDrawSettings(Syn: TSrcIDEHighlighter);
    procedure ReadDefaultsForHighlighterDivDrawSettings(Syn: TSrcIDEHighlighter);
    procedure WriteHighlighterDivDrawSettings(Syn: TSrcIDEHighlighter);

    procedure SetMarkupColor(Syn: TSrcIDEHighlighter;
                             AddHilightAttr: TAdditionalHilightAttribute;
                             aMarkup: TSynSelectedColor);
    procedure SetMarkupColors(aSynEd: TSynEdit);
  public
    // general options
    property SynEditOptions: TSynEditorOptions
      read fSynEditOptions write fSynEditOptions default SynEditDefaultOptions;
    property SynEditOptions2: TSynEditorOptions2
      read fSynEditOptions2 write fSynEditOptions2 default SynEditDefaultOptions2;
    property ShowTabCloseButtons: Boolean
      read fShowTabCloseButtons write fShowTabCloseButtons;
  published
    property MultiLineTab: Boolean
      read FMultiLineTab write FMultiLineTab default False;
  public
    property HideSingleTabInWindow: Boolean
      read FHideSingleTabInWindow write FHideSingleTabInWindow;
    property ShowTabNumbers: Boolean read fShowTabNumbers write fShowTabNumbers;
    property UndoAfterSave: Boolean read fUndoAfterSave
      write fUndoAfterSave default True;
    property FindTextAtCursor: Boolean
      read fFindTextAtCursor write fFindTextAtCursor default True;
    property UseSyntaxHighlight: Boolean
      read fUseSyntaxHighlight write fUseSyntaxHighlight default True;
    property CopyWordAtCursorOnCopyNone: Boolean
      read FCopyWordAtCursorOnCopyNone write FCopyWordAtCursorOnCopyNone;
    property ShowGutterHints: Boolean read FShowGutterHints
      write FShowGutterHints;
    property BlockIndent: Integer
      read fBlockIndent write fBlockIndent default 2;
    property BlockTabIndent: Integer
      read FBlockTabIndent write FBlockTabIndent default 0;
    property BlockIndentType: TSynBeautifierIndentType
      read fBlockIndentType write fBlockIndentType default sbitCopySpaceTab;
    property TrimSpaceType: TSynEditStringTrimmingType
      read FTrimSpaceType write FTrimSpaceType default settLeaveLine;
    property UndoLimit: Integer read fUndoLimit write fUndoLimit default 32767;
    property TabWidth: Integer read fTabWidth write fTabWidth default 8;
    property BracketHighlightStyle: TSynEditBracketHighlightStyle read FBracketHighlightStyle write FBracketHighlightStyle default sbhsBoth;

    // Display options
    property VisibleRightMargin: Boolean
      read fVisibleRightMargin write fVisibleRightMargin default True;
    property VisibleGutter: Boolean read fVisibleGutter
      write fVisibleGutter default True;
    property ShowLineNumbers: Boolean read fShowLineNumbers
      write fShowLineNumbers default False;
    property ShowOnlyLineNumbersMultiplesOf: integer read fShowOnlyLineNumbersMultiplesOf
      write fShowOnlyLineNumbersMultiplesOf;
    property GutterWidth: Integer
      read fGutterWidth write fGutterWidth default 30;
    property GutterSeparatorIndex: Integer read FGutterSeparatorIndex
      write FGutterSeparatorIndex default 3;
    property RightMargin: Integer
      read fRightMargin write fRightMargin default 80;
    property EditorFont: String read fEditorFont write fEditorFont;
    property EditorFontSize: Integer
      read fEditorFontSize write fEditorFontSize;
    property ExtraCharSpacing: Integer
      read fExtraCharSpacing write fExtraCharSpacing default 0;
    property ExtraLineSpacing: Integer
      read fExtraLineSpacing write fExtraLineSpacing default 1;
    property DisableAntialiasing: Boolean
      read fDisableAntialiasing write fDisableAntialiasing default DefaultEditorDisableAntiAliasing;
    property DoNotWarnForFont: string
      read FDoNotWarnForFont write FDoNotWarnForFont;

    // Key Mappings
    property KeyMappingScheme: String
      read fKeyMappingScheme write fKeyMappingScheme;
    property KeyMap: TKeyCommandRelationList read fKeyMap;

    // Mouse Mappings
    // Current saved config
    property UserMouseSettings: TEditorMouseOptions read FUserMouseSettings;
    // Used by the 2 Mouse-option pages, so they share data (un-saved)
    property TempMouseSettings: TEditorMouseOptions read FTempMouseSettings;

    // Color options
    property HighlighterList: TEditOptLangList read fHighlighterList;
    property UserColorSchemeGroup: TColorSchemeFactory read FUserColorSchemeSettings;
    property UserDefinedColors: TEditorUserDefinedWordsList read FUserDefinedColors;

    // Markup Current Word
    property MarkupCurWordTime: Integer
      read FMarkupCurWordTime write FMarkupCurWordTime default 1500;
    property MarkupCurWordFullLen: Integer
      read FMarkupCurWordFullLen write FMarkupCurWordFullLen default 3;
    property MarkupCurWordNoKeyword: Boolean
      read FMarkupCurWordNoKeyword write FMarkupCurWordNoKeyword default False;
    property MarkupCurWordTrim: Boolean
      read FMarkupCurWordTrim write FMarkupCurWordTrim default True;
    property MarkupCurWordNoTimer: Boolean
      read FMarkupCurWordNoTimer write FMarkupCurWordNoTimer default False;

    // Code Tools options
    property AutoBlockCompletion: Boolean
      read fAutoBlockCompletion write FAutoBlockCompletion default True;
    property AutoCodeParameters: Boolean
      read fAutoCodeParameters write fAutoCodeParameters default True;
    property AutoToolTipExprEval: Boolean
      read fAutoToolTipExprEval write fAutoToolTipExprEval default True; // debugger hints
    property AutoToolTipSymbTools: Boolean
      read fAutoToolTipSymbTools write fAutoToolTipSymbTools default True; // declaration hints
    property AutoDisplayFunctionPrototypes: Boolean
      read fAutoDisplayFuncPrototypes write fAutoDisplayFuncPrototypes default True;

  published
    property DbgHintAutoTypeCastClass: Boolean
      read FDbgHintAutoTypeCastClass write FDbgHintAutoTypeCastClass default True; // declaration hints
  public
    property AutoDelayInMSec: Integer read fAutoDelayInMSec
      write fAutoDelayInMSec default 1000;
    property CodeTemplateFileNameRaw: String
      read fCodeTemplateFileNameRaw write fCodeTemplateFileNameRaw;
    property CodeTemplateFileNameExpand:String
      read GetCodeTemplateFileNameExpand;
    property CodeTemplateIndentToTokenStart: Boolean
      read fCTemplIndentToTokenStart write fCTemplIndentToTokenStart;
    property AutoRemoveEmptyMethods: Boolean read FAutoRemoveEmptyMethods
      write FAutoRemoveEmptyMethods default False;
    property CompletionLongLineHintInMSec: Integer
             read FCompletionLongLineHintInMSec write FCompletionLongLineHintInMSec;
  published
    property CompletionLongLineHintType: TSynCompletionLongHintType
             read FCompletionLongLineHintType write FCompletionLongLineHintType
             default sclpExtendRightOnly;

  public
    // Code Folding
    property UseCodeFolding: Boolean
        read FUseCodeFolding write FUseCodeFolding default True;
    property UseMarkupWordBracket: Boolean
        read FUseMarkupWordBracket write FUseMarkupWordBracket default True;
    property UseMarkupOutline: Boolean
        read FUseMarkupOutline write FUseMarkupOutline default False;

    // Multi window
    property MultiWinEditAccessOrder: TEditorOptionsEditAccessOrderList
        read FMultiWinEditAccessOrder write FMultiWinEditAccessOrder;

  published { use RTTIConf}
    property TabPosition: TTabPosition
      read fTabPosition write fTabPosition default tpTop;
    // General - Misc
    {$IFDEF WinIME}
    property UseMinimumIme: Boolean read FUseMinimumIme write FUseMinimumIme default False;
    {$ENDIF}
    // Display
    property ShowOverviewGutter: boolean
      read FShowOverviewGutter write FShowOverviewGutter default True;
    property TopInfoView: boolean
      read FTopInfoView write FTopInfoView default True;
    // Code Folding
    property ReverseFoldPopUpOrder: Boolean
        read FReverseFoldPopUpOrder write FReverseFoldPopUpOrder default True;
    property UseTabHistory: Boolean read fUseTabHistory write fUseTabHistory;
    property MultiCaretOnColumnSelect: Boolean
      read FMultiCaretOnColumnSelect write FMultiCaretOnColumnSelect default True;
    property MultiCaretDefaultMode: TSynPluginMultiCaretDefaultMode
             read FMultiCaretDefaultMode write FMultiCaretDefaultMode default mcmMoveAllCarets;
    property MultiCaretDeleteSkipLineBreak: Boolean
             read FMultiCaretDeleteSkipLineBreak write FMultiCaretDeleteSkipLineBreak default False;
    property MultiCaretDefaultColumnSelectMode: TSynPluginMultiCaretDefaultMode
             read FMultiCaretDefaultColumnSelectMode write FMultiCaretDefaultColumnSelectMode default mcmCancelOnCaretMove;

    // Highlighter Pas
    property PasExtendedKeywordsMode: Boolean
             read FPasExtendedKeywordsMode write FPasExtendedKeywordsMode default False;
    property PasStringKeywordMode: TSynPasStringMode
             read FPasStringKeywordMode write FPasStringKeywordMode default spsmDefault;

    // Multi window
    property CtrlMiddleTabClickClosesOthers: Boolean
      read FCtrlMiddleTabClickClosesOthers write FCtrlMiddleTabClickClosesOthers default True;

    property ShowFileNameInCaption: Boolean
      read FShowFileNameInCaption write FShowFileNameInCaption default False;

    // Commend Continue
    property AnsiCommentContinueEnabled: Boolean
      read FAnsiCommentContinueEnabled write FAnsiCommentContinueEnabled;
    property AnsiCommentMatch: String
      read FAnsiCommentMatch write FAnsiCommentMatch;
    property AnsiCommentPrefix: String
      read FAnsiCommentPrefix write FAnsiCommentPrefix;
    property AnsiCommentMatchMode: TSynCommentMatchMode
      read FAnsiCommentMatchMode write FAnsiCommentMatchMode;
    property AnsiIndentMode: TSynCommentIndentFlags
      read FAnsiIndentMode write FAnsiIndentMode;
    property AnsiIndentAlignMax: integer
      read FAnsiIndentAlignMax write FAnsiIndentAlignMax;

    property CurlyCommentContinueEnabled: Boolean
      read FCurlyCommentContinueEnabled write FCurlyCommentContinueEnabled;
    property CurlyCommentMatch: String
      read FCurlyCommentMatch write FCurlyCommentMatch;
    property CurlyCommentPrefix: String
      read FCurlyCommentPrefix write FCurlyCommentPrefix;
    property CurlyCommentMatchMode: TSynCommentMatchMode
      read FCurlyCommentMatchMode write FCurlyCommentMatchMode;
    property CurlyIndentMode: TSynCommentIndentFlags
      read FCurlyIndentMode write FCurlyIndentMode;
    property CurlyIndentAlignMax: integer
      read FCurlyIndentAlignMax write FCurlyIndentAlignMax;

    property SlashCommentContinueEnabled: Boolean
      read FSlashCommentContinueEnabled write FSlashCommentContinueEnabled;
    property SlashCommentMatch: String
      read FSlashCommentMatch write FSlashCommentMatch;
    property SlashCommentPrefix: String
      read FSlashCommentPrefix write FSlashCommentPrefix;
    property SlashCommentMatchMode: TSynCommentMatchMode
      read FSlashCommentMatchMode write FSlashCommentMatchMode;
    property SlashIndentMode: TSynCommentIndentFlags
      read FSlashIndentMode write FSlashIndentMode;
    property SlashCommentExtend: TSynCommentExtendMode
      read FSlashCommentExtend write FSlashCommentExtend;
    property SlashIndentAlignMax: integer
      read FSlashIndentAlignMax write FSlashIndentAlignMax;

    property StringBreakEnabled: Boolean read FStringBreakEnabled write FStringBreakEnabled;
    property StringBreakAppend: String read FStringBreakAppend write FStringBreakAppend;
    property StringBreakPrefix: String read FStringBreakPrefix write FStringBreakPrefix;
  end;

var
  EditorOpts: TEditorOptions;

procedure RepairEditorFontSize(var FontSize: integer);

function BuildBorlandDCIFile(ACustomSynAutoComplete: TCustomSynAutoComplete): Boolean;
function ColorSchemeFactory: TColorSchemeFactory;
function UserSchemeDirectory(CreateIfNotExists: Boolean = False): String;
//function HighlighterListSingleton: TEditOptLangList;

procedure InitLocale;

implementation

{$R editoroptions.res}

const
  ValidAttribChars = ['a'..'z', 'A'..'Z', '_', '0'..'9'];

  // several language types can be redirected. For example there are FreePascal
  // and Delphi, but currently both are hilighted with the FreePascal
  // highlighter
  CompatibleLazSyntaxHilighter: array[TLazSyntaxHighlighter] of
    TLazSyntaxHighlighter = (
    lshNone,
    lshText,
    lshFreePascal,
    lshFreePascal,
    lshLFM,
    lshXML,
    lshHTML,
    lshCPP,
    lshPerl,
    lshJava,
    lshBash,
    lshPython,
    lshPHP,
    lshSQL,
    lshCSS,
    lshJScript,
    lshDiff,
    lshBat,
    lshIni,
    lshPo,
    lshPike
    );

var
  DefaultColorSchemeName: String;

function FontHeightToSize(Height: Integer): Integer;
var
  AFont: TFont;
begin
  AFont := TFont.Create;
  AFont.Height := Height;
  Result := AFont.Size;
  AFont.Free;
end;

{ TEditorUserDefinedWordsList }

function TEditorUserDefinedWordsList.GetLists(AIndex: Integer): TEditorUserDefinedWords;
begin
  Result := TEditorUserDefinedWords(FList[AINdex]);
end;

function TEditorUserDefinedWordsList.GetKeyCommandList: TIDECommands;
begin
  if FUseGlobalIDECommandList then
    Result := IDECommandList
  else
    Result := FKeyCommandList;
end;

procedure TEditorUserDefinedWordsList.SetLists(AIndex: Integer;
  AValue: TEditorUserDefinedWords);
begin
  FList[AINdex] := AValue;
end;

constructor TEditorUserDefinedWordsList.Create;
begin
  FList := TList.Create;
end;

destructor TEditorUserDefinedWordsList.Destroy;
begin
  inherited Destroy;
  Clear;
  FreeAndNil(FList);
end;

procedure TEditorUserDefinedWordsList.Assign(Src: TEditorUserDefinedWordsList);
var
  i: Integer;
begin
  Clear;
  for i := 0 to Src.Count - 1 do
    Add('').Assign(Src.Lists[i]);
end;

procedure TEditorUserDefinedWordsList.LoadFromXMLConfig(XMLConfig: TRttiXMLConfig;
  Path: String);
var
  c, i: Integer;
begin
  Clear;
  Path := Path + 'Entry/';
  c := XMLConfig.GetValue(Path + 'Count', 0);
  for i := 0 to c - 1 do
    Add('').LoadFromXMLConfig(XMLConfig, Path + 'E' + IntToStr(i) + '/');
end;

procedure TEditorUserDefinedWordsList.SaveToXMLConfig(XMLConfig: TRttiXMLConfig; Path: String);
var
  c, i: Integer;
begin
  Path := Path + 'Entry/';
  c := XMLConfig.GetValue(Path + 'Count', 0);
  XMLConfig.SetDeleteValue(Path + 'Count', Count, 0);
  for i := 0 to Count - 1 do
    Lists[i].SaveToXMLConfig(XMLConfig, Path + 'E' + IntToStr(i) + '/');
  for i := Count to c - 1 do
    XMLConfig.DeletePath(Path + 'E' + IntToStr(i));
end;

procedure TEditorUserDefinedWordsList.Clear;
begin
  while Count > 0 do
    Remove(Lists[0], True);
end;

function TEditorUserDefinedWordsList.Add(AList: TEditorUserDefinedWords): Integer;
begin
  Result := FList.Add(AList);
end;

function TEditorUserDefinedWordsList.Add(AName: String): TEditorUserDefinedWords;
begin
  Result := TEditorUserDefinedWords.Create(Self);
  Result.Name := AName;
  FList.Add(Result);
end;

function TEditorUserDefinedWordsList.IndexOf(AName: String): Integer;
begin
  Result := FList.Count - 1;
  while (Result >= 0) and (Lists[Result].Name <> AName) do
    dec(Result);
end;

function TEditorUserDefinedWordsList.IndexOf(AList: TEditorUserDefinedWords): Integer;
begin
  Result := FList.IndexOf(AList);
end;

procedure TEditorUserDefinedWordsList.Remove(AName: String; FreeList: Boolean);
var
  i: Integer;
begin
  i := IndexOf(AName);
  if i >= 0 then
    FList.Delete(i);
end;

procedure TEditorUserDefinedWordsList.Remove(AList: TEditorUserDefinedWords;
  FreeList: Boolean);
begin
  FList.Remove(AList);
  if FreeList then
    FreeAndNil(AList);
end;

procedure TEditorUserDefinedWordsList.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

function TEditorUserDefinedWordsList.Count: Integer;
begin
  Result := FList.Count;
end;

{ TEditorUserDefinedWords }

procedure TEditorUserDefinedWords.SetName(AValue: String);
begin
  if FName = AValue then Exit;
  FName := AValue;
  UpdateIdeCommands;
end;

procedure TEditorUserDefinedWords.SetGlobalTermsCache(AValue: TSynSearchTermDict);
begin
  if FGlobalTermsCache = AValue then Exit;

  if FGlobalTermsCache <> nil then
    FGlobalTermsCache.ReleaseReference;

  FGlobalTermsCache := AValue;

  if FGlobalTermsCache <> nil then
    FGlobalTermsCache.AddReference;
end;

procedure TEditorUserDefinedWords.UpdateIdeCommands;
var
  Keys: TKeyCommandRelationList;
  Cat: TIDECommandCategory;
begin
  if (FList = nil) or (FList.KeyCommandList = nil) or (FName = '') then
    exit;

  Keys := FList.KeyCommandList as TKeyCommandRelationList;
  Cat := nil;

  if FAddTermCmd = nil then
    FAddTermCmd := Keys.FindCommandByName('UserDefinedMarkup_Add_'+FId);
  if FAddTermCmd = nil then begin
    if Cat = nil then
      Cat := keys.FindCategoryByName(EditorUserDefinedWordsKeyCatName);
    FAddTermCmd := Keys.CreateCommand(
      Cat,
      'UserDefinedMarkup_Add_'+FId,
      Format(lisUserDefinedMarkupKeyAdd, [FName]),
      IDEShortCut(VK_UNKNOWN, [], VK_UNKNOWN, []),
      IDEShortCut(VK_UNKNOWN, [], VK_UNKNOWN, []),
      nil, nil
    );
    (FAddTermCmd as TKeyCommandRelation).SkipSaving := True;
  end
  else
    FAddTermCmd.LocalizedName := Format(lisUserDefinedMarkupKeyAdd, [FName]);

  if FRemoveTermCmd = nil then
    FRemoveTermCmd := Keys.FindCommandByName('UserDefinedMarkup_Remove_'+FId);
  if FRemoveTermCmd = nil then begin
    if Cat = nil then
      Cat := keys.FindCategoryByName(EditorUserDefinedWordsKeyCatName);
    FRemoveTermCmd := Keys.CreateCommand(
      Cat,
      'UserDefinedMarkup_Remove_'+FId,
      Format(lisUserDefinedMarkupKeyRemove, [FName]),
      IDEShortCut(VK_UNKNOWN, [], VK_UNKNOWN, []),
      IDEShortCut(VK_UNKNOWN, [], VK_UNKNOWN, []),
      nil, nil
    );
    (FRemoveTermCmd as TKeyCommandRelation).SkipSaving := True;
  end
  else
    FRemoveTermCmd.LocalizedName := Format(lisUserDefinedMarkupKeyRemove, [FName]);

  if FToggleTermCmd = nil then
    FToggleTermCmd := Keys.FindCommandByName('UserDefinedMarkup_Toggle_'+FId);
  if FToggleTermCmd = nil then begin
    if Cat = nil then
      Cat := keys.FindCategoryByName(EditorUserDefinedWordsKeyCatName);
    FToggleTermCmd := Keys.CreateCommand(
      Cat,
      'UserDefinedMarkup_Toggle_'+FId,
      Format(lisUserDefinedMarkupKeyToggle, [FName]),
      IDEShortCut(VK_UNKNOWN, [], VK_UNKNOWN, []),
      IDEShortCut(VK_UNKNOWN, [], VK_UNKNOWN, []),
      nil, nil
    );
    (FToggleTermCmd as TKeyCommandRelation).SkipSaving := True;
  end
  else
    FToggleTermCmd.LocalizedName := Format(lisUserDefinedMarkupKeyToggle, [FName]);
end;

procedure TEditorUserDefinedWords.ClearIdeCommands;
begin
  if (FList <> nil) and (FList.KeyCommandList <> nil) then begin
    if (FAddTermCmd <> nil) then
      (FList.KeyCommandList as TKeyCommandRelationList).RemoveCommand(FAddTermCmd);
    if (FRemoveTermCmd <> nil) then
      (FList.KeyCommandList as TKeyCommandRelationList).RemoveCommand(FRemoveTermCmd);
    if (FToggleTermCmd <> nil) then
      (FList.KeyCommandList as TKeyCommandRelationList).RemoveCommand(FToggleTermCmd);
  end;
  FreeAndNil(FAddTermCmd);
  FreeAndNil(FRemoveTermCmd);
  FreeAndNil(FToggleTermCmd);
end;

constructor TEditorUserDefinedWords.Create(AList: TEditorUserDefinedWordsList);
var
  i: Integer;
begin
  FList := AList;
  FId := EditorUserDefinedWordsGlobalId;
  i := 1;
  UniqueString(EditorUserDefinedWordsGlobalId);
  while i <= Length(EditorUserDefinedWordsGlobalId) do begin
    if EditorUserDefinedWordsGlobalId[i] < 'z' then begin
      inc(EditorUserDefinedWordsGlobalId[i]);
      break;
    end;
    inc(i);
  end;
  if i > Length(EditorUserDefinedWordsGlobalId) then
    EditorUserDefinedWordsGlobalId := EditorUserDefinedWordsGlobalId + 'a';

  inherited Create;
  FColorAttr := TColorSchemeAttribute.Create(nil, nil);
  FColorAttr.Features := [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior,hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask];
  FColorAttr.Group := agnText;
  FColorAttr.SetAllPriorities(MARKUP_USER_DEF_PRIOR);
  FKeyAddSelectSmart := True;
end;

destructor TEditorUserDefinedWords.Destroy;
begin
  ReleaseRefAndNil(FGlobalTermsCache);
  Clear;
  FreeAndNil(FColorAttr);
  ClearIdeCommands;
  inherited Destroy;
end;

procedure TEditorUserDefinedWords.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if not(Source is TEditorUserDefinedWords) then
    exit;
  ClearIdeCommands;

  FId                      := TEditorUserDefinedWords(Source).FId;
  FName                    := TEditorUserDefinedWords(Source).FName;
  FGlobalList              := TEditorUserDefinedWords(Source).FGlobalList;
  FKeyAddCase              := TEditorUserDefinedWords(Source).FKeyAddCase;
  FKeyAddSelectBoundMaxLen := TEditorUserDefinedWords(Source).FKeyAddSelectBoundMaxLen;
  FKeyAddSelectSmart       := TEditorUserDefinedWords(Source).FKeyAddSelectSmart;
  FKeyAddTermBounds        := TEditorUserDefinedWords(Source).FKeyAddTermBounds;
  FKeyAddWordBoundMaxLen   := TEditorUserDefinedWords(Source).FKeyAddWordBoundMaxLen;

  FColorAttr.Assign(TEditorUserDefinedWords(Source).FColorAttr);
  UpdateIdeCommands;
end;

procedure TEditorUserDefinedWords.LoadFromXMLConfig(XMLConfig: TRttiXMLConfig; Path: String);
  procedure Load(SubPath: string; out Key: TIDEShortCut);
  begin
    key.Key1   := XMLConfig.GetValue(SubPath+'Key1',VK_UNKNOWN);
    key.Shift1 := CfgStrToShiftState(XMLConfig.GetValue(SubPath+'Shift1',''));
    key.Key2   := XMLConfig.GetValue(SubPath+'Key2',VK_UNKNOWN);
    key.Shift2 := CfgStrToShiftState(XMLConfig.GetValue(SubPath+'Shift2',''));
  end;
var
  c, i: Integer;
  def: TEditorUserDefinedWords;
  ColorDef: TColorSchemeAttribute;
  DefEntry: TSynSearchTerm;
  SCut: TIDEShortCut;
  Keys: TKeyCommandRelationList;
begin
  Clear;
  def := TEditorUserDefinedWords.Create(nil);
  XMLConfig.ReadObject(Path + 'Main/', self, def);
  def.Free;

  ColorDef := TColorSchemeAttribute.Create(nil, nil);
  ColorDef.SetAllPriorities(MARKUP_USER_DEF_PRIOR);
  FColorAttr.StoredName := 'c1';
  FColorAttr.LoadFromXml(XMLConfig, Path + 'Color/', ColorDef, EditorOptsFormatVersion);
  ColorDef.Free;

  c := XMLConfig.GetValue(Path + 'Count', 0);
  Path := Path + 'Entry/';
  DefEntry := TSynSearchTerm.Create(nil);
  for i := 0 to c - 1 do
    XMLConfig.ReadObject(Path + 'Entry' + IntToStr(i) + '/', Add, DefEntry);
  DefEntry.Free;

  UpdateIdeCommands;

  if (FList <> nil) and (FList.KeyCommandList <> nil) then begin
    Keys := FList.KeyCommandList as TKeyCommandRelationList;

    if (FAddTermCmd <> nil) then begin
      Load(Path+'AddKeyA/', SCut);
      if Keys.Find(SCut, TSourceEditorWindowInterface) = nil then
        FAddTermCmd.ShortcutA := SCut;
      Load(Path+'AddKeyB/', SCut);
      if Keys.Find(SCut, TSourceEditorWindowInterface) = nil then
        FAddTermCmd.ShortcutB := SCut;
    end;

    if (FRemoveTermCmd <> nil) then begin
      Load(Path+'RemoveKeyA/', SCut);
      if Keys.Find(SCut, TSourceEditorWindowInterface) = nil then
        FRemoveTermCmd.ShortcutA := SCut;
      Load(Path+'RemoveKeyB/', SCut);
      if Keys.Find(SCut, TSourceEditorWindowInterface) = nil then
        FRemoveTermCmd.ShortcutB := SCut;
    end;

    if (FToggleTermCmd <> nil) then begin
      Load(Path+'ToggleKeyA/', SCut);
      if Keys.Find(SCut, TSourceEditorWindowInterface) = nil then
        FToggleTermCmd.ShortcutA := SCut;
      Load(Path+'ToggleKeyB/', SCut);
      if Keys.Find(SCut, TSourceEditorWindowInterface) = nil then
        FToggleTermCmd.ShortcutB := SCut;
    end;
  end;
end;

procedure TEditorUserDefinedWords.SaveToXMLConfig(XMLConfig: TRttiXMLConfig; Path: String);
  procedure ClearKey(const SubPath: string);
  begin
    XMLConfig.DeleteValue(SubPath+'Key1');
    XMLConfig.DeleteValue(SubPath+'Shift1');
    XMLConfig.DeleteValue(SubPath+'Key2');
    XMLConfig.DeleteValue(SubPath+'Shift2');
  end;
  procedure Store(const SubPath: string; Key: TIDEShortCut);
  var
    s: TShiftState;
  begin
    XMLConfig.SetDeleteValue(SubPath+'Key1', key.Key1, VK_UNKNOWN);
    if key.Key1=VK_UNKNOWN then
      s:=[]
    else
      s:=key.Shift1;
    XMLConfig.SetDeleteValue(SubPath+'Shift1',ShiftStateToCfgStr(s),ShiftStateToCfgStr([]));
    XMLConfig.SetDeleteValue(SubPath+'Key2',key.Key2,VK_UNKNOWN);
    if key.Key2=VK_UNKNOWN then
      s:=[]
    else
      s:=key.Shift2;
    XMLConfig.SetDeleteValue(SubPath+'Shift2',ShiftStateToCfgStr(s),ShiftStateToCfgStr([]));
  end;
var
  i, c: Integer;
  def: TEditorUserDefinedWords;
  ColorDef: TColorSchemeAttribute;
  DefEntry: TSynSearchTerm;
begin
  def := TEditorUserDefinedWords.Create(nil);
  XMLConfig.WriteObject(Path + 'Main/', Self, def);
  def.Free;

  ColorDef := TColorSchemeAttribute.Create(nil, nil);
  ColorDef.SetAllPriorities(MARKUP_USER_DEF_PRIOR);
  FColorAttr.StoredName := 'c1';
  FColorAttr.SaveToXml(XMLConfig, Path + 'Color/', ColorDef);
  ColorDef.Free;

  c := XMLConfig.GetValue(Path + 'Count', 0);
  XMLConfig.SetDeleteValue(Path + 'Count', Count, 0);
  Path := Path + 'Entry/';
  DefEntry := TSynSearchTerm.Create(nil);
  for i := 0 to Count - 1 do
    XMLConfig.WriteObject(Path + 'Entry' + IntToStr(i) + '/', Items[i], DefEntry);
  DefEntry.Free;
  for i := Count to c - 1 do
    XMLConfig.DeletePath(Path + 'Entry' + IntToStr(i));

  if (FAddTermCmd = nil) then begin
    ClearKey(Path + 'AddKeyA/');
    ClearKey(Path + 'AddKeyB/');
  end else begin
    Store(Path + 'AddKeyA/', FAddTermCmd.ShortcutA);
    Store(Path + 'AddKeyB/', FAddTermCmd.ShortcutB);
  end;

  if (FRemoveTermCmd = nil) then begin
    ClearKey(Path + 'RemoveKeyA/');
    ClearKey(Path + 'RemoveKeyB/');
  end else begin
    Store(Path + 'RemoveKeyA/', FRemoveTermCmd.ShortcutA);
    Store(Path + 'RemoveKeyB/', FRemoveTermCmd.ShortcutB);
  end;

  if (FToggleTermCmd = nil) then begin
    ClearKey(Path + 'ToggleKeyA/');
    ClearKey(Path + 'ToggleKeyB/');
  end else begin
    Store(Path + 'ToggleKeyA/', FToggleTermCmd.ShortcutA);
    Store(Path + 'ToggleKeyB/', FToggleTermCmd.ShortcutB);
  end;

end;

function TEditorUserDefinedWords.HasKeyAssigned: Boolean;
begin
  Result := (FAddTermCmd.ShortcutA.Key1 <> VK_UNKNOWN) or
            (FAddTermCmd.ShortcutB.Key1 <> VK_UNKNOWN) or
            (FRemoveTermCmd.ShortcutA.Key1 <> VK_UNKNOWN) or
            (FRemoveTermCmd.ShortcutB.Key1 <> VK_UNKNOWN) or
            (FToggleTermCmd.ShortcutA.Key1 <> VK_UNKNOWN) or
            (FToggleTermCmd.ShortcutB.Key1 <> VK_UNKNOWN);
end;

{ TSynEditMouseActionKeyCmdHelper }

function TSynEditMouseActionKeyCmdHelper.GetOptionKeyCmd: TSynEditorCommand;
begin
  Result := inherited Option;
end;

procedure TSynEditMouseActionKeyCmdHelper.SetOptionKeyCmd(
  const AValue: TSynEditorCommand);
begin
  inherited Option := AValue;
end;


procedure RepairEditorFontSize(var FontSize: integer);
begin
  if ((FontSize>=0) and (FontSize<=EditorOptionsMinimumFontSize))
  or ((FontSize<0) and (FontSize>=-EditorOptionsMinimumFontSize)) then
    FontSize := SynDefaultFontSize;
end;

const
  EditOptsConfFileName = 'editoroptions.xml';

function BuildBorlandDCIFile(
  ACustomSynAutoComplete: TCustomSynAutoComplete): Boolean;
  // returns if something has changed
var
  sl: TStringList;
  i, sp, ep: Integer;
  Token, Comment, Value: String;
  Attributes: TStrings;
begin
  Result := False;
  sl     := TStringList.Create;
  try
    for i := 0 to ACustomSynAutoComplete.Completions.Count - 1 do
    begin
      Token := ACustomSynAutoComplete.Completions[i];
      Comment := ACustomSynAutoComplete.CompletionComments[i];
      Value := ACustomSynAutoComplete.CompletionValues[i];
      sl.Add('[' + Token + ' | ' + Comment + ']');
      Attributes:=ACustomSynAutoComplete.CompletionAttributes[i];
      if (Attributes<>nil) and (Attributes.Count>0) then begin
        sl.Add(CodeTemplateAttributesStartMagic);
        sl.AddStrings(Attributes);
        sl.Add(CodeTemplateAttributesEndMagic);
      end;
      sp    := 1;
      ep    := 1;
      while ep <= length(Value) do
        if Value[ep] in [#10, #13] then
        begin
          sl.Add(copy(Value, sp, ep - sp));
          inc(ep);
          if (ep <= length(Value)) and (Value[ep] in [#10, #13]) and
            (Value[ep] <> Value[ep - 1]) then
            inc(ep);
          sp := ep;
        end
        else
          inc(ep);
      if (ep > sp) or ((Value <> '') and (Value[length(Value)] in [#10, #13])) then
        sl.Add(copy(Value, sp, ep - sp));
    end;
    if ACustomSynAutoComplete.AutoCompleteList.Equals(sl) = False then
    begin
      Result := True;
      ACustomSynAutoComplete.AutoCompleteList := sl;
    end;
  finally
    sl.Free;
  end;
end;

// The lazy-man color scheme factory
function ColorSchemeFactory: TColorSchemeFactory;
const
  Singleton: TColorSchemeFactory = nil;
var
  FileList: TStringList;
  i, j, c: Integer;
  XMLConfig: TRttiXMLConfig;
  n: String;

  procedure AddFromResource(AResName, ASchemeName: String);
  var
    FPResource: TFPResourceHandle;
    Stream: TLazarusResourceStream;
  begin
    FPResource := FindResource(HInstance, PChar(AResName), PChar(RT_RCDATA));
    if FPResource = 0 then exit;
    Stream := TLazarusResourceStream.CreateFromHandle(HInstance, FPResource);
    XMLConfig := TRttiXMLConfig.Create('');
    XMLConfig.ReadFromStream(Stream);
    Singleton.RegisterScheme(XMLConfig, ASchemeName, 'Lazarus/ColorSchemes/');
    FreeAndNil(XMLConfig);
    FreeAndNil(Stream);
  end;
begin
  if not Assigned(Singleton) then begin
    InitLocale;
    Singleton := TColorSchemeFactory.Create;
    // register all built-in color schemes

    AddFromResource('ColorSchemeDefault', 'Default');
    AddFromResource('ColorSchemeTwilight', 'Twilight');
    AddFromResource('ColorSchemePascalClassic', 'Pascal Classic');
    AddFromResource('ColorSchemeOcean', 'Ocean');
    AddFromResource('ColorSchemeDelphi', 'Delphi');
    DefaultColorSchemeName := 'Default';

    if DirectoryExistsUTF8(UserSchemeDirectory(False)) then begin
      FileList := FindAllFiles(UserSchemeDirectory(False), '*.xml', False);
      for i := 0 to FileList.Count - 1 do begin
        XMLConfig := nil;
        try
          XMLConfig := TRttiXMLConfig.Create(FileList[i]);
          c := XMLConfig.GetValue('Lazarus/ColorSchemes/Names/Count', 0);
          for j := 0 to c-1 do begin
            n := XMLConfig.GetValue('Lazarus/ColorSchemes/Names/Item'+IntToStr(j+1)+'/Value', '');
            if n <> '' then
              Singleton.RegisterScheme(XMLConfig, n, 'Lazarus/ColorSchemes/');
          end;
        except
          ShowMessage(Format(dlgUserSchemeError, [FileList[i]]));
        end;
        XMLConfig.Free;
      end;
      FileList.Free;
    end;
  end;
  Result := Singleton;
end;

function UserSchemeDirectory(CreateIfNotExists: Boolean): String;
begin
  Result := AppendPathDelim(GetPrimaryConfigPath) + 'userschemes';
  If CreateIfNotExists and (not DirectoryExistsUTF8(Result)) then
    CreateDirUTF8(Result);
end;

function HighlighterListSingleton: TEditOptLangList;
const
  Singleton: TEditOptLangList = nil;
begin
  if not Assigned(Singleton) then
    Singleton := TEditOptLangList.Create;
  Result := Singleton;
end;

procedure InitLocale;
const
  InitDone: Boolean = False;
begin
  if InitDone then exit;
  InitDone := true;
  EditorOptionsEditAccessDefaults[0].Caption := dlgEditAccessCaptionLockedInView;
  EditorOptionsEditAccessDefaults[0].Desc    := dlgEditAccessDescLockedInView;
  EditorOptionsEditAccessDefaults[1].Caption := dlgEditAccessCaptionUnLockedInSoftView;
  EditorOptionsEditAccessDefaults[1].Desc    := dlgEditAccessDescUnLockedInSoftView;
  EditorOptionsEditAccessDefaults[2].Caption := dlgEditAccessCaptionUnLocked;
  EditorOptionsEditAccessDefaults[2].Desc    := dlgEditAccessDescUnLocked;
  EditorOptionsEditAccessDefaults[3].Caption := dlgEditAccessCaptionUnLockedOpenNewInOldWin  ;
  EditorOptionsEditAccessDefaults[3].Desc    := dlgEditAccessDescUnLockedOpenNewInOldWin;
  EditorOptionsEditAccessDefaults[4].Caption := dlgEditAccessCaptionUnLockedOpenNewInNewWin;
  EditorOptionsEditAccessDefaults[4].Desc    := dlgEditAccessDescUnLockedOpenNewInNewWin;
  EditorOptionsEditAccessDefaults[5].Caption := dlgEditAccessCaptionIgnLockedOldEdit;
  EditorOptionsEditAccessDefaults[5].Desc    := dlgEditAccessDescIgnLockedOldEdit;
  EditorOptionsEditAccessDefaults[6].Caption := dlgEditAccessCaptionIgnLockedOnlyActEdit;
  EditorOptionsEditAccessDefaults[6].Desc    := dlgEditAccessDescIgnLockedOnlyActEdit;
  EditorOptionsEditAccessDefaults[7].Caption := dlgEditAccessCaptionIgnLockedOnlyActWin;
  EditorOptionsEditAccessDefaults[7].Desc    := dlgEditAccessDescIgnLockedOnlyActWin;
  EditorOptionsEditAccessDefaults[8].Caption := dlgEditAccessCaptionUnLockedOpenNewInAnyWin;
  EditorOptionsEditAccessDefaults[8].Desc    := dlgEditAccessDescUnLockedOpenNewInAnyWin;


  // update translation
  EditorOptionsFoldInfoPas[ 0].Name := dlgFoldPasProcedure;
  EditorOptionsFoldInfoPas[ 1].Name := dlgFoldLocalPasVarType;
  EditorOptionsFoldInfoPas[ 2].Name := dlgFoldPasProcBeginEnd;
  EditorOptionsFoldInfoPas[ 3].Name := dlgFoldPasBeginEnd;
  EditorOptionsFoldInfoPas[ 4].Name := dlgFoldPasRepeat;
  EditorOptionsFoldInfoPas[ 5].Name := dlgFoldPasCase;
  EditorOptionsFoldInfoPas[ 6].Name := dlgFoldPasTry;
  EditorOptionsFoldInfoPas[ 7].Name := dlgFoldPasExcept;
  EditorOptionsFoldInfoPas[ 8].Name := dlgFoldPasAsm;
  EditorOptionsFoldInfoPas[ 9].Name := dlgFoldPasProgram;
  EditorOptionsFoldInfoPas[10].Name := dlgFoldPasUnit;
  EditorOptionsFoldInfoPas[11].Name := dlgFoldPasUnitSection;
  EditorOptionsFoldInfoPas[12].Name := dlgFoldPasUses;
  EditorOptionsFoldInfoPas[13].Name := dlgFoldPasVarType;
  EditorOptionsFoldInfoPas[14].Name := dlgFoldPasClass;
  EditorOptionsFoldInfoPas[15].Name := dlgFoldPasClassSection;
  EditorOptionsFoldInfoPas[16].Name := dlgFoldPasRecord;
  EditorOptionsFoldInfoPas[17].Name := dlgFoldPasIfDef;
  EditorOptionsFoldInfoPas[18].Name := dlgFoldPasUserRegion;
  EditorOptionsFoldInfoPas[19].Name := dlgFoldPasAnsiComment;
  EditorOptionsFoldInfoPas[20].Name := dlgFoldPasBorComment;
  EditorOptionsFoldInfoPas[21].Name := dlgFoldPasSlashComment;
  EditorOptionsFoldInfoPas[22].Name := dlgFoldPasNestedComment;

  EditorOptionsFoldInfoHTML[0].Name := dlgFoldHtmlNode;
  EditorOptionsFoldInfoHTML[1].Name := dlgFoldHtmlComment;
  EditorOptionsFoldInfoHTML[2].Name := dlgFoldHtmlAsp;

  EditorOptionsFoldInfoLFM[0].Name := dlgFoldLfmObject;
  EditorOptionsFoldInfoLFM[1].Name := dlgFoldLfmList;
  EditorOptionsFoldInfoLFM[2].Name := dlgFoldLfmItem;

  EditorOptionsFoldInfoXML[0].Name := dlgFoldXmlNode;
  EditorOptionsFoldInfoXML[1].Name := dlgFoldXmlComment;
  EditorOptionsFoldInfoXML[2].Name := dlgFoldXmlCData;
  EditorOptionsFoldInfoXML[3].Name := dlgFoldXmlDocType;
  EditorOptionsFoldInfoXML[4].Name := dlgFoldXmlProcess;

  EditorOptionsFoldInfoDiff[0].Name := lisFile;
  EditorOptionsFoldInfoDiff[1].Name := dlgFoldDiffChunk;
  EditorOptionsFoldInfoDiff[2].Name := dlgFoldDiffChunkSect;

  EditorOptionsDividerInfoPas[0].Name:=dlgDivPasUnitSectionName;
  EditorOptionsDividerInfoPas[1].Name:=dlgDivPasUsesName;
  EditorOptionsDividerInfoPas[2].Name:=dlgDivPasVarGlobalName;
  EditorOptionsDividerInfoPas[3].Name:=dlgDivPasVarLocalName;
  EditorOptionsDividerInfoPas[4].Name:=dlgDivPasStructGlobalName;
  EditorOptionsDividerInfoPas[5].Name:=dlgDivPasStructLocalName;
  EditorOptionsDividerInfoPas[6].Name:=dlgDivPasProcedureName;
  EditorOptionsDividerInfoPas[7].Name:=dlgDivPasBeginEndName;
  EditorOptionsDividerInfoPas[8].Name:=dlgDivPasTryName;

  AdditionalHighlightAttributes[ahaNone]                := '';
  AdditionalHighlightAttributes[ahaTextBlock]           := dlgAddHiAttrTextBlock;
  AdditionalHighlightAttributes[ahaExecutionPoint]      := dlgAddHiAttrExecutionPoint;
  AdditionalHighlightAttributes[ahaEnabledBreakpoint]   := dlgAddHiAttrEnabledBreakpoint;
  AdditionalHighlightAttributes[ahaDisabledBreakpoint]  := dlgAddHiAttrDisabledBreakpoint;
  AdditionalHighlightAttributes[ahaInvalidBreakpoint]   := dlgAddHiAttrInvalidBreakpoint;
  AdditionalHighlightAttributes[ahaUnknownBreakpoint]   := dlgAddHiAttrUnknownBreakpoint;
  AdditionalHighlightAttributes[ahaErrorLine]           := dlgAddHiAttrErrorLine;
  AdditionalHighlightAttributes[ahaIncrementalSearch]   := dlgAddHiAttrIncrementalSearch;
  AdditionalHighlightAttributes[ahaHighlightAll]        := dlgAddHiAttrHighlightAll;
  AdditionalHighlightAttributes[ahaBracketMatch]        := dlgAddHiAttrBracketMatch;
  AdditionalHighlightAttributes[ahaMouseLink]           := dlgAddHiAttrMouseLink;
  AdditionalHighlightAttributes[ahaLineNumber]          := dlgAddHiAttrLineNumber;
  AdditionalHighlightAttributes[ahaLineHighlight]       := dlgAddHiAttrLineHighlight;
  AdditionalHighlightAttributes[ahaModifiedLine]        := dlgAddHiAttrModifiedLine;
  AdditionalHighlightAttributes[ahaCodeFoldingTree]     := dlgAddHiAttrCodeFoldingTree;
  AdditionalHighlightAttributes[ahaHighlightWord]       := dlgAddHiAttrHighlightWord;
  AdditionalHighlightAttributes[ahaFoldedCode]          := dlgAddHiAttrFoldedCode;
  AdditionalHighlightAttributes[ahaFoldedCodeLine]      := dlgAddHiAttrFoldedCodeLine;
  AdditionalHighlightAttributes[ahaHiddenCodeLine]      := dlgAddHiAttrHiddenCodeLine;
  AdditionalHighlightAttributes[ahaWordGroup]           := dlgAddHiAttrWordGroup;
  AdditionalHighlightAttributes[ahaTemplateEditCur]     := dlgAddHiAttrTemplateEditCur;
  AdditionalHighlightAttributes[ahaTemplateEditSync]    := dlgAddHiAttrTemplateEditSync;
  AdditionalHighlightAttributes[ahaTemplateEditOther]   := dlgAddHiAttrTemplateEditOther;
  AdditionalHighlightAttributes[ahaSyncroEditCur]       := dlgAddHiAttrSyncroEditCur;
  AdditionalHighlightAttributes[ahaSyncroEditSync]      := dlgAddHiAttrSyncroEditSync;
  AdditionalHighlightAttributes[ahaSyncroEditOther]     := dlgAddHiAttrSyncroEditOther;
  AdditionalHighlightAttributes[ahaSyncroEditArea]      := dlgAddHiAttrSyncroEditArea;
  AdditionalHighlightAttributes[ahaGutterSeparator]     := dlgAddHiAttrGutterSeparator;
  AdditionalHighlightAttributes[ahaGutter]              := dlgGutter;
  AdditionalHighlightAttributes[ahaRightMargin]         := dlgRightMargin;
  AdditionalHighlightAttributes[ahaSpecialVisibleChars] := dlgAddHiSpecialVisibleChars;
  AdditionalHighlightAttributes[ahaTopInfoHint]         := dlgTopInfoHint;
  AdditionalHighlightAttributes[ahaCaretColor]          := dlgCaretColor;
  AdditionalHighlightAttributes[ahaIfDefBlockInactive]  := dlgIfDefBlockInactive;
  AdditionalHighlightAttributes[ahaIfDefBlockActive]    := dlgIfDefBlockActive;
  AdditionalHighlightAttributes[ahaIfDefBlockTmpActive] := dlgIfDefBlockTmpActive;
  AdditionalHighlightAttributes[ahaIfDefNodeInactive]   := dlgIfDefNodeInactive;
  AdditionalHighlightAttributes[ahaIfDefNodeActive]     := dlgIfDefNodeActive;
  AdditionalHighlightAttributes[ahaIfDefNodeTmpActive]  := dlgIfDefNodeTmpActive;
  AdditionalHighlightGroupNames[agnIfDef]        := dlgAddHiAttrGroupIfDef;

  AdditionalHighlightAttributes[ahaIdentComplWindow]          := dlgAddHiAttrDefaultWindow;
  AdditionalHighlightAttributes[ahaIdentComplWindowBorder]    := dlgAddHiAttrWindowBorder;
  AdditionalHighlightAttributes[ahaIdentComplWindowSelection] := dlgBlockGroupOptions;
  AdditionalHighlightAttributes[ahaIdentComplWindowHighlight] := dlgAddHiAttrHighlightPrefix;
  AdditionalHighlightGroupNames[agnIdentComplWindow]          := dlgIdentifierCompletion;

  AdditionalHighlightAttributes[ahaOutlineLevel1Color]  := dlgAddHiAttrOutlineLevel1Color;
  AdditionalHighlightAttributes[ahaOutlineLevel2Color]  := dlgAddHiAttrOutlineLevel2Color;
  AdditionalHighlightAttributes[ahaOutlineLevel3Color]  := dlgAddHiAttrOutlineLevel3Color;
  AdditionalHighlightAttributes[ahaOutlineLevel4Color]  := dlgAddHiAttrOutlineLevel4Color;
  AdditionalHighlightAttributes[ahaOutlineLevel5Color]  := dlgAddHiAttrOutlineLevel5Color;
  AdditionalHighlightAttributes[ahaOutlineLevel6Color]  := dlgAddHiAttrOutlineLevel6Color;
  AdditionalHighlightAttributes[ahaOutlineLevel7Color]  := dlgAddHiAttrOutlineLevel7Color;
  AdditionalHighlightAttributes[ahaOutlineLevel8Color]  := dlgAddHiAttrOutlineLevel8Color;
  AdditionalHighlightAttributes[ahaOutlineLevel9Color]  := dlgAddHiAttrOutlineLevel9Color;
  AdditionalHighlightAttributes[ahaOutlineLevel10Color] := dlgAddHiAttrOutlineLevel10Color;
  AdditionalHighlightGroupNames[agnOutlineColors]  := dlgAddHiAttrGroupOutlineColors;

  AdditionalHighlightGroupNames[agnDefault]      := dlgAddHiAttrGroupDefault;
  AdditionalHighlightGroupNames[agnText]         := dlgAddHiAttrGroupText;
  AdditionalHighlightGroupNames[agnLine]         := dlgAddHiAttrGroupLine;
  AdditionalHighlightGroupNames[agnTemplateMode] := dlgAddHiAttrGroupTemplateEdit;
  AdditionalHighlightGroupNames[agnSyncronMode]  := dlgAddHiAttrGroupSyncroEdit;
  AdditionalHighlightGroupNames[agnGutter]       := dlgAddHiAttrGroupGutter;
end;

function StrToValidXMLName(const s: String): String;
var
  i: Integer;
begin
  Result := s;
  // replace invalid characters
  for i := 1 to length(Result) do
    if (not (Result[i] in ValidAttribChars)) then
      Result[i] := '_';
end;

{ TEditOptLanguageInfo }

constructor TEditOptLanguageInfo.Create;
begin
  inherited Create;
end;

destructor TEditOptLanguageInfo.Destroy;
begin
  MappedAttributes.Free;
  inherited Destroy;
end;

function TEditOptLanguageInfo.SampleLineToAddAttr(
  Line: Integer): TAdditionalHilightAttribute;
begin
  if Line < 1 then
    exit(ahaNone);
  for Result := Low(TAdditionalHilightAttribute)
    to High(TAdditionalHilightAttribute) do
    if (Result <> ahaNone) and (AddAttrSampleLines[Result] = Line) then
      exit;
  Result := ahaNone;
end;

function TEditOptLanguageInfo.GetDefaultFilextension: String;
var
  p: Integer;
begin
  // read the first file extension
  p := 1;
  while (p <= length(FileExtensions)) and (FileExtensions[p] <> ';') do
    inc(p);
  if p > 1 then
    Result := '.' + copy(FileExtensions, 1, p - 1)
  else
    Result := '';
end;

procedure TEditOptLanguageInfo.SetBothFilextensions(const Extensions: string);
begin
  FileExtensions:=Extensions;
  DefaultFileExtensions:=Extensions;
end;

procedure TEditOptLanguageInfo.prepare(Syntax: TLazSyntaxHighlighter);
begin
  TheType := Syntax;
  DefaultCommentType := DefaultCommentTypes[TheType];
  SynClass := LazSyntaxHighlighterClasses[TheType];
end;

{ TEditOptLangCssInfo }

procedure TEditOptLangCssInfo.prepare(Syntax: TLazSyntaxHighlighter);
begin
  inherited Prepare(syntax);
  SetBothFilextensions('css');
  SampleSource := getSampleSource;
  AddAttrSampleLines[ahaTextBlock] := 4;
  CaretXY := Point(1,1);
  MappedAttributes := getMappedAttributes;;
end;

function TEditOptLangCssInfo.getSampleSource: string;
begin
  result :=
      '.field :hover {'#10 +
      '   display:inline;'#10+
      '   border:10px;'#10+
      '   color: #555;'#10+
      '/* comment */'#10+
      '}'#10+#10;
end;

function TEditOptLangCssInfo.getMappedAttributes: tStringList;
begin
    result:=tStringList.create;
    with result do
    begin
      Add('Comment=Comment');
      Add('Selector=Reserved_word');
      Add('Identifier=Identifier');
      Add('Space=Space');
      Add('Symbol=Symbol');
      Add('Number=Number');
      Add('Key=Key');
      Add('String=String');
    end;
end;

{ TEditOptLangList }

function TEditOptLangList.GetInfos(Index: Integer): TEditOptLanguageInfo;
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.Create('TEditOptLangList.GetInfos Index '
      + IntToStr(Index) + ' out of bounds. Count=' + IntToStr(Count));
  Result := TEditOptLanguageInfo(inherited Items[Index]);
end;

procedure TEditOptLangList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  inherited Clear;
end;

constructor TEditOptLangList.Create;
var
  NewInfo: TEditOptLanguageInfo;
begin
  inherited Create;

  { create the meta information for each available highlighter.
    Please keep the pascal highlighter at the top. The rest can be ordered as you
    like.
  }

  // create info for pascal
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshFreePascal;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('pp;pas;inc;lpr;lrs;dpr;dpk;fpd');
    SampleSource :=
      '{ Comment }'#13 +
      '{$R- compiler directive}'#13 +
      'procedure TForm1.Button1Click(Sender: TObject);'#13 +
      'var  // Delphi Comment'#13 +
      '  Number, I, X: Integer;'#13 +
      'begin'#13 +
      '  Number := 12345 * (2 + 9) // << Matching Brackets ;'#13 +
      '  Caption := ''The number is '' + IntToStr(Number);'#13 +
      '  asm'#13 + '    MOV AX,1234h'#13 +
      '    MOV Number,AX'#13 +
      '  end;'#13 +
      '  {%region /fold}'#13 +
      '  {%endregion}'#13 +
      '  X := 10;'#13 +
      '  inc(X); {$R+} { Search Match, Text Block }'#13 +
      '  for I := 0 to Number do {$R-} { execution point }'#13 +
      '  begin'#13 +
      '    Inc(X, 2); {$R+} { Enabled breakpoint }'#13 +
      '    Dec(X, 3); {$R+} { Disabled breakpoint }'#13 +
      '    {$R-} // { Invalid breakpoint }'#13 +
      '    WriteLN(X); {$R-} { Unknown breakpoint }'#13 +
      '    X := X + 1.0; {$R-} { Error line }'#13 +
      '    case ModalResult of'#13+
      '      mrOK: inc(X);'#13+
      '      mrCancel, mrIgnore: dec(X);'#13+
      '    end;'#13+
      '    ListBox1.Items.Add(IntToStr(X));'#13 +
//{$IFDEF WithSynMarkupIfDef}
//      '    {$IFDEF Foo}' +
//      '      X := X + 1.0; {$R-} { Error line }'#13 +
//      '      {$DEFINE a}' +
//      '      case ModalResult of'#13+
//      '        mrOK: inc(X);'#13+
//      '        mrCancel, mrIgnore: dec(X);'#13+
//      '      end;'#13+
//      '    {$ELSE}' +
//      '      {%region teset}'#13 +
//      '      {%endregion}'#13 +
//      '      with self do'#13 +
//      '        X := 10;'#13 +
//      '    {$ENDIF}' +
//{$ENDIF}
      '  end;'#13 +
      'end;'#13 + #13;
    AddAttrSampleLines[ahaDisabledBreakpoint] := 20;
    AddAttrSampleLines[ahaEnabledBreakpoint] := 19;
    AddAttrSampleLines[ahaInvalidBreakpoint] := 21;
    AddAttrSampleLines[ahaUnknownBreakpoint] := 22;
    AddAttrSampleLines[ahaErrorLine] := 23;
    AddAttrSampleLines[ahaExecutionPoint] := 17;
    AddAttrSampleLines[ahaTextBlock] := 16;
    AddAttrSampleLines[ahaFoldedCode] := 13;
    CaretXY := Point(21, 7);
  end;
  Add(NewInfo);

  // create info for html
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshHTML;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('htm;html;xhtml');
    SampleSource :=
      '<html>'#13 + '<title>Lazarus Sample source for html</title>'#13 +
      '<body bgcolor=#ffffff background="bg.jpg">'#13 +
      '<!-- Comment -->'#13 + '<img src="lazarus.jpg">'#13 +
      '<p>'#13 + '  Some Text'#13 +
      '  Ampersands: &nbsp;F&nbsp;P&nbsp;C'#13 + '</p>'#13 +
      '<invalid_tag>'#13 + '<!-- Text Block -->'#13 +
      '</body>'#13 + '</html>'#13 + #13;
    AddAttrSampleLines[ahaTextBlock] := 11;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Space=Space');
    end;
    CaretXY := Point(1,1);
  end;
  Add(NewInfo);

  // create info for cpp
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshCPP;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('c;cc;cpp;h;hpp;hh');
    SampleSource :=
      '/* Comment */'#13 + '#include <stdio.h>'#13 +
      '#include <stdlib.h>'#13 + #13 +
      'static char line_buf[LINE_BUF];'#13 + #13 +
      'int main(int argc,char **argv){'#13 + '  FILE *file;'#13 +
      '  line_buf[0]=0;'#13 + '  printf("\n");'#13 +
      '  return 0;'#13 + '}'#13 + ''#13 + #13;
    AddAttrSampleLines[ahaTextBlock] := 11;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Assembler=Assembler');
      Add('Comment=Comment');
      Add('Preprocessor=Comment');
      Add('Identifier=Identifier');
      Add('Reserved_word=Reserved_word');
      Add('Number=Number');
      Add('Space=Space');
      Add('String=String');
      Add('Symbol=Symbol');
    end;
    CaretXY := Point(1,1);
  end;
  Add(NewInfo);

  // create info for XML
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshXML;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('xml;xsd;xsl;xslt;dtd;lpi;lps;lpk;wsdl;svg');
    SampleSource :=
      '<?xml version="1.0"?>'#13 + '<!DOCTYPE root ['#13 +
      '  ]>'#13 + '<!-- Comment -->'#13 + '<root version="&test;">'#13 +
      '  <![CDATA[ **CDATA section** ]]>'#13 + '</root>'#13 +
      '<!-- Text Block -->'#13 + ''#13 + #13;
    AddAttrSampleLines[ahaTextBlock] := 8;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Element=Reserved_word');
      Add('Comment=Comment');
      Add('Text=Identifier');
      Add('Space=Space');
      Add('Symbol=Symbol');
    end;
    CaretXY := Point(1,1);
  end;
  Add(NewInfo);

  // create info for LFM
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshLFM;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('lfm;dfm;xfm');
    SampleSource :=
      '{ Lazarus Form Definitions }'#13 + 'object TestForm: TTestForm'#13 +
      '  Left = 273'#13 + '  Top = 103'#13 +
      '  Caption = ''sample source'''#13 + 'end'#13 +
      '{ Text Block }'#13 + ''#13 + #13;
    AddAttrSampleLines[ahaTextBlock] := 7;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Element=Reserved_word');
      Add('Comment=Comment');
      Add('Identifier=Identifier');
      Add('Key=Reserved_word');
      Add('Number=Number');
      Add('Space=Space');
      Add('String=String');
      Add('Symbol=Symbol');
    end;
    CaretXY := Point(1,1);
  end;
  Add(NewInfo);

  // create info for Perl
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshPerl;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('pl;pm;cgi');
    SampleSource :=
      '#!/usr/bin/perl'#13 + '# Perl sample code'#13 +
      ''#13 + '$i = "10";'#13 + 'print "$ENV{PATH}\n";'#13 +
      '($i =~ /\d+/) || die "Error\n";'#13 + ''#13 +
      '# Text Block'#13 + ''#13 + #13;
    AddAttrSampleLines[ahaTextBlock] := 8;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Identifier=Identifier');
      Add('KeyAttri=Reserved_word');
      Add('NumberAttri=Number');
      Add('SpaceAttri=Space');
      Add('StringAttri=String');
      Add('Symbol=Symbol');
    end;
    CaretXY := Point(1,1);
  end;
  Add(NewInfo);

  // create info for Java
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshJava;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('java');
    SampleSource :=
      '/* Java syntax highlighting */'#13#10 +
      'import java.util.*;'#13#10 + #13#10 +
      '/** Example class */'#13#10 +
      'public class Sample {'#13#10 +
      '  public static void main(String[] args) {'#13#10 +
      '    int i = 0;'#13#10 +
      '    for(i = 0; i < 10; i++)'#13#10 +
      '      System.out.println("Hello world");'#13#10 +
      '  }'#13#10 + '}'#13#10 +
      '/* Text Block */'#13#10 + #13#10;
    AddAttrSampleLines[ahaTextBlock] := 12;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Documentation=Comment');
      Add('Identifier=Identifier');
      Add('Reserved_word=Reserved_word');
      Add('Number=Number');
      Add('Space=Space');
      Add('String=String');
      Add('Symbol=Symbol');
    end;
    CaretXY := Point(1,1);
  end;
  Add(NewInfo);

  // create info for Bash
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshBash;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('sh');
    SampleSource :=
      '#!/bin/bash'#13#13 +
      '# Bash syntax highlighting'#13#10 + 'set -x'#13#10 +
      'set -e'#13#10 +
      'Usage="Usage: $0 devel|stable"'#13#10 +
      'FPCVersion=$1'#13#10 +
      'for ver in devel stable; do'#13#10 +
      '  if [ "x$FPCVersion" = "x$ver" ]; then'#13#10 +
      '  fi'#13#10 + 'done'#13#10 +
      '# Text Block'#13#10 + #13#10;
    AddAttrSampleLines[ahaTextBlock] := 12;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Variable=Identifier');
      Add('Key=Reserved_word');
      Add('Number=Number');
      Add('Space=Space');
      Add('String=String');
      Add('Symbol=Symbol');
    end;
    CaretXY := Point(1,1);
  end;
  Add(NewInfo);

  // create info for Python
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshPython;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('py;pyw');
    SampleSource :=
      '# Python syntax highlighting'#13#10 +
      'import math'#13#10 + #13#10 +
      '""" Documentation """'#13#10 +
      'def DoSomething(Liste1,Liste2,param3=3):'#13#10 +
      '  for i in Liste1:'#13#10 +
      '    if i in Liste2:'#13#10 +
      '      Liste1.remove(i)'#13#10 +
      '/* Text Block */'#13#10 + #13#10;
    AddAttrSampleLines[ahaTextBlock] := 9;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Identifier=Identifier');
      Add('Documentation=Comment');
      Add('Reserved_word=Reserved_word');
      Add('Number=Number');
      Add('Space=Space');
      Add('String=String');
      Add('Symbol=Symbol');
    end;
    CaretXY := Point(1,1);
  end;
  Add(NewInfo);

  // create info for PHP
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshPHP;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('php;php3;php4');
    SampleSource :=
      '<?if ( ($HTTP_HOST == "www.lazarus.com") || ($HTTP_HOST == "lazarus.com") ){'#10 + '   HEADER("Location:http://www.lazarus.freepascal.org/\n\n");'#10
      + '};'#10 + '?>'#10 + #10;
    AddAttrSampleLines[ahaTextBlock] := 8;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Element=Reserved_word');
      Add('Comment=Comment');
      Add('Variable=Identifier');
      Add('Space=Space');
      Add('Symbol=Symbol');
      Add('Number=Number');
      Add('Key=Key');
      Add('String=String');
    end;
    CaretXY := Point(1,1);
  end;
  Add(NewInfo);

  // create info for SQL
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshSQL;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('sql');
    SampleSource :=
      '-- ansi sql sample source'#10 +
        'select name , region'#10 +
        'from cia'#10 +
        'where area < 2000'#10 +
        'and gdp > 5000000000'#10 + #10;
    AddAttrSampleLines[ahaTextBlock] := 4;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Element=Reserved_word');
      Add('Variable=Identifier');
      Add('Space=Space');
      Add('Symbol=Symbol');
      Add('Number=Number');
      Add('Key=Key');
      Add('String=String');
    end;
    CaretXY := Point(1,1);
  end;
  Add(NewInfo);

  // create info for CSS
  NewInfo := TEditOptLangCssInfo.Create;
  NewInfo.Prepare(lshCss);
  Add(NewInfo);



  // create info for JScript
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshJScript;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('js');
    SampleSource :=
      '/* JScript */'#13#10 +
      'var semafor={'#13#10 +
      '  semafor:0,'#13#10 +
      '  timer:null,'#13#10 +
      '  name:"Name",'#13#10 +
      '  clear: function(){'#13#10 +
      '    try{'#13#10 +
      '      this.semafor=0;'#13#10 +
      '      clearTimeout(this.timer);'#13#10 +
      '    }  catch (e)  { }'#13#10 +
      '  }'#13#10 +
      '};'#13#10 +

      #13#10 +
      '/* Text Block */'#13#10 + #13#10;
    AddAttrSampleLines[ahaTextBlock] := 2;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Documentation=Comment');
      Add('Identifier=Identifier');
      Add('Reserved_word=Reserved_word');
      Add('Number=Number');
      Add('Space=Space');
      Add('String=String');
      Add('Symbol=Symbol');
    end;
    CaretXY := Point(1,1);
  end;
  Add(NewInfo);

  // create info for Diff
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshDiff;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('diff');
    SampleSource :=
      '*** /a/file'#13#10 +
      '--- /b/file'#13#10 +
      '***************'#13#10 +
      '*** 2,5 ****'#13#10 +
      '--- 2,5 ----'#13#10 +
      '  context'#13#10 +
      '- removed'#13#10 +
      '! Changed'#13#10 +
      '+ added'#13#10 +
      '  context'#13#10;
    MappedAttributes := TStringList.Create;
    //with MappedAttributes do
    //begin
    //  Add('Unknown_word=Comment');
    //end;
    CaretXY := Point(1,6);
  end;
  Add(NewInfo);

  // create info for Bat
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshBat;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('bat');
    SampleSource :=
      'rem MS-DOS batch file'#13#10 +
      'rem'#13#10 +
      '@echo off'#13#10 +
      'cls'#13#10 +
      'echo The command line is: %1 %2 %3 %4 %5'#13#10 +
      'rem'#13#10 +
      'rem now wait for the user ...'#13#10 +
      'pause'#13#10 +
      'copy c:\*.pas d:\'#13#10 +
      'if errorlevel 1 echo Error in copy action!';
    MappedAttributes := TStringList.Create;
    //with MappedAttributes do
    //begin
    //  Add('Comment=Comment');
    //  Add('Identifier=Identifier');
    //  Add('Key=Key');
    //  Add('Number=Number');
    //  Add('Space=Space');
    //end;
    CaretXY := Point(1,3);
  end;
  Add(NewInfo);

  // create info for Diff
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshIni;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('ini');
    SampleSource :=
      '; Syntax highlighting'#13#10+
      '[Section]'#13#10+
      'Key=value'#13#10+
      'String="Arial"'#13#10+
      'Number=123456';
    MappedAttributes := TStringList.Create;
    //with MappedAttributes do
    //begin
    //  Add('Comment=Comment');
    //  Add('String=String');
    //  Add('Key=Key');
    //  Add('Number=Number');
    //  Add('Space=Space');
    //end;
    CaretXY := Point(1,1);
  end;
  Add(NewInfo);

  // create info for PO
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshPo;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('po');
    SampleSource :=
      '#: foo.bar'#13#10 +
      '#, fuzzy'#13#10 +
      '#| msgid "abc"'#13#10 +
      'msgid "abc"'#13#10 +
      'msgstr "123"'#13#10;
    //MappedAttributes := TStringList.Create;
    //with MappedAttributes do
    //begin
    //  Add('Comment=Comment');
    //  Add('Key=Key');
    //  Add('Identifier=Identifier');
    //  Add('Space=Space');
    //  Add('String=String');
    //end;
    CaretXY := Point(3,1);
  end;
  Add(NewInfo);

  // create info for Pike
  NewInfo := TEditOptLanguageInfo.Create;
  NewInfo.TheType := lshPike;
  NewInfo.DefaultCommentType := DefaultCommentTypes[NewInfo.TheType];
  NewInfo.SynClass := LazSyntaxHighlighterClasses[NewInfo.TheType];
  NewInfo.SetBothFilextensions('pike;pmod');
  NewInfo.SampleSource := TSynPikeSyn.Pike_GetSampleSource();
  with NewInfo do
  begin
    AddAttrSampleLines[ahaTextBlock] := 12;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Documentation=Comment');
      Add('Identifier=Identifier');
      Add('Reserved_word=Reserved_word');
      Add('Number=Number');
      Add('Space=Space');
      Add('String=String');
      Add('Symbol=Symbol');
    end;
    CaretXY := Point(1,1);
  end;
  Add(NewInfo);

end;

destructor TEditOptLangList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TEditOptLangList.FindByName(const Name: String): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (UTF8CompareText(
      Items[Result].SynClass.GetLanguageName, Name) <> 0) do
    dec(Result);
end;

function TEditOptLangList.FindByClass(
  CustomSynClass: TCustomSynClass): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (Items[Result].SynClass <> CustomSynClass) do
    dec(Result);
end;

function TEditOptLangList.FindByHighlighter(Hilighter:
  TSynCustomHighlighter): Integer;
begin
  if Hilighter <> Nil then
    Result := FindByClass(TCustomSynClass(Hilighter.ClassType))
  else
    Result := -1;
end;

function TEditOptLangList.FindByType(AType: TLazSyntaxHighlighter): Integer;
begin
  AType := CompatibleLazSyntaxHilighter[AType];
  Result := Count - 1;
  while (Result >= 0) and (Items[Result].TheType <> AType) do
    dec(Result);
end;

function TEditOptLangList.GetDefaultFilextension(
  AType: TLazSyntaxHighlighter): String;
var
  i: Integer;
begin
  i := FindByType(AType);
  if i >= 0 then
    Result := Items[i].GetDefaultFilextension
  else
    Result := '';
end;

function TEditOptLangList.GetInfoByType(AType: TLazSyntaxHighlighter
  ): TEditOptLanguageInfo;
var
  i: LongInt;
begin
  i:=FindByType(AType);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

{ TEditorMouseOptions }

procedure TEditorMouseOptions.ClearUserSchemes;
begin
  while FUserSchemes.Count > 0 do begin
    FUserSchemes.Objects[0].Free;
    FUserSchemes.Delete(0);
  end;
end;

function TEditorMouseOptions.GetUserSchemeNames(Index: Integer): String;
begin
  Result := TEditorMouseOptions(FUserSchemes.Objects[Index]).Name;
end;

function TEditorMouseOptions.GetUserSchemes(Index: String): TEditorMouseOptions;
var
  i: Integer;
begin
  i := IndexOfUserScheme(Index);
  if i >= 0 then
    Result := UserSchemesAtPos[i]
  else
    Result := nil;
end;

function TEditorMouseOptions.GetUserSchemesAtPos(Index: Integer): TEditorMouseOptions;
begin
  Result := TEditorMouseOptions(FUserSchemes.Objects[Index]);
end;

constructor TEditorMouseOptions.Create;
begin
  inherited Create;
  Reset;
  FMainActions          := TSynEditMouseActions.Create(nil);
  FSelActions           := TSynEditMouseActions.Create(nil);
  FTextActions          := TSynEditMouseActions.Create(nil);
  FGutterActions        := TSynEditMouseActions.Create(nil);
  FGutterActionsFold    := TSynEditMouseActions.Create(nil);
  FGutterActionsFoldExp := TSynEditMouseActions.Create(nil);
  FGutterActionsFoldCol := TSynEditMouseActions.Create(nil);
  FGutterActionsLines   := TSynEditMouseActions.Create(nil);
  FGutterActionsChanges := TSynEditMouseActions.Create(nil);
  FGutterActionsOverView:= TSynEditMouseActions.Create(nil);
  FGutterActionsOverViewMarks:= TSynEditMouseActions.Create(nil);
  FUserSchemes := TQuickStringlist.Create;
  FVersion := 0;
end;

destructor TEditorMouseOptions.Destroy;
begin
  ClearUserSchemes;
  FUserSchemes.Free;
  FMainActions.Free;
  FTextActions.Free;
  FSelActions.Free;
  FGutterActions.Free;
  FGutterActionsFold.Free;
  FGutterActionsFoldExp.Free;
  FGutterActionsFoldCol.Free;
  FGutterActionsLines.Free;
  FGutterActionsChanges.Free;
  FGutterActionsOverView.Free;
  FGutterActionsOverViewMarks.Free;
  inherited Destroy;
end;

procedure TEditorMouseOptions.Reset;
begin
  FCustomSavedActions  := False;
  FGutterLeft          := moglUpClickAndSelect;
  // left multi
  FTextDoubleLeftClick       := mbaSelectWords;
  FTextTripleLeftClick       := mbaSelectSetLineSmart;
  FTextQuadLeftClick         := mbaSelectSetPara;
  FTextShiftDoubleLeftClick  := mbaNone;
  FTextAltDoubleLeftClick    := mbaNone;
  FTextCtrlDoubleLeftClick   := mbaNone;
  // left
  FTextAltLeftClick          := mbaSelectColumn;
  FTextCtrlLeftClick         := mbaDeclarationJump;
  FTextAltCtrlLeftClick      := mbaNone;
  FTextShiftLeftClick        := mbaNone;
  FTextShiftAltLeftClick     := mbaNone;
  FTextShiftCtrlLeftClick    := mbaMultiCaretToggle;
  FTextShiftAltCtrlLeftClick := mbaNone;
  // middle
  FTextMiddleClick             := mbaPaste;
  FTextAltMiddleClick          := mbaNone;
  FTextCtrlMiddleClick         := mbaZoomReset;
  FTextShiftMiddleClick        := mbaNone;
  FTextAltCtrlMiddleClick      := mbaNone;
  FTextShiftAltMiddleClick     := mbaNone;
  FTextShiftAltCtrlMiddleClick := mbaNone;
  FTextShiftCtrlMiddleClick    := mbaNone;
  // wheel
  FWheel               := mwaScroll;
  FCtrlWheel           := mwaZoom;
  FAltWheel            := mwaScrollPageLessOne;
  FShiftWheel          := mwaScrollSingleLine;
  FAltCtrlWheel     := mwaNone;
  FShiftCtrlWheel       := mwaNone;
  FShiftAltWheel    := mwaNone;
  FShiftAltCtrlWheel    := mwaNone;
  // right
  FTextRightClick := mbaContextMenu;
  FTextAltCtrlRightClick := mbaNone;
  FTextAltRightClick := mbaNone;
  FTextCtrlRightClick := mbaContextMenuTab;
  FTextShiftAltCtrlRightClick := mbaNone;
  FTextShiftAltRightClick := mbaNone;
  FTextShiftCtrlRightClick := mbaNone;
  FTextShiftRightClick := mbaNone;
  // extra-1 click
  FTextExtra1Click := mbaHistoryBack;
  FTextAltCtrlExtra1Click := mbaNone;
  FTextAltExtra1Click := mbaNone;
  FTextCtrlExtra1Click := mbaNone;
  FTextShiftAltCtrlExtra1Click := mbaNone;
  FTextShiftAltExtra1Click := mbaNone;
  FTextShiftCtrlExtra1Click := mbaNone;
  FTextShiftExtra1Click := mbaNone;
  // extra-2 click
  FTextExtra2Click := mbaHistoryForw;
  FTextAltCtrlExtra2Click := mbaNone;
  FTextAltExtra2Click := mbaNone;
  FTextCtrlExtra2Click := mbaNone;
  FTextShiftAltCtrlExtra2Click := mbaNone;
  FTextShiftAltExtra2Click := mbaNone;
  FTextShiftCtrlExtra2Click := mbaNone;
  FTextShiftExtra2Click := mbaNone;


  FTextRightMoveCaret  := False;
  FTextDrag            := True;
  FSelectOnLineNumbers := True;
end;

procedure TEditorMouseOptions.ResetGutterToDefault;
  procedure AddStartSel(List: TSynEditMouseActions);
  begin
    with List do begin
      AddCommand(emcStartSelections,   True, mbXLeft, ccAny, cdDown, [],               [ssShift], emcoSelectionStart);
      AddCommand(emcStartSelections,   True, mbXLeft, ccAny, cdDown, [ssShift],        [ssShift], emcoSelectionContinue);
    end;
  end;
var
  CDir: TSynMAClickDir;
  R: TSynMAUpRestrictions;
begin
  FGutterActions.Clear;
  FGutterActionsFold.Clear;
  FGutterActionsFoldExp.Clear;
  FGutterActionsFoldCol.Clear;
  FGutterActionsLines.Clear;
  FGutterActionsChanges.Clear;
  FGutterActionsOverView.Clear;
  FGutterActionsOverViewMarks.Clear;
  //TMouseOptGutterLeftType = (moGLDownClick, moglUpClickAndSelect);

  with FGutterActions do begin
    AddCommand(emcContextMenu,         False, mbXRight,  ccSingle, cdUp, [], []);
  end;
  with FGutterActionsFold do begin
    AddCommand(emcCodeFoldContextMenu, False, mbXRight,  ccSingle, cdUp, [], []);
  end;


  CDir := cdDown;
  R := [];
  if FGutterLeft = moglUpClickAndSelect then begin
    CDir := cdUp;
    R := crRestrictAll;
    AddStartSel(FGutterActions);
  end;

  with FGutterActions do begin
    AddCommand(emcOnMainGutterClick,   False, mbXLeft,   ccAny,    CDir, R, [], []);  // breakpoint
  end;


  if FGutterLeft in [moglUpClickAndSelect, moglUpClickAndSelectRighHalf] then begin
    CDir := cdUp;
    R := crRestrictAll;
    AddStartSel(FGutterActionsChanges);
  end;

  with FGutterActionsChanges do begin
    if FGutterLeft = moGLDownClick then
      AddCommand(emcNone,   False, mbXLeft,   ccAny,    cdDown, [], []);
    AddCommand(emcNone,   False, mbXLeft,   ccAny,    cdUp, [], []);
  end;


  if FGutterLeft = moglUpClickAndSelectRighHalf then begin
    if not FSelectOnLineNumbers then
      AddStartSel(FGutterActionsLines);
    AddStartSel(FGutterActionsFold);
  end;


  if FSelectOnLineNumbers then begin
    with FGutterActionsLines do begin
      AddCommand(emcStartLineSelectionsNoneEmpty,   True, mbXLeft, ccAny, cdDown, [],               [ssShift], emcoSelectionStart);
      AddCommand(emcStartLineSelectionsNoneEmpty,   True, mbXLeft, ccAny, cdDown, [ssShift],        [ssShift], emcoSelectionContinue);
      AddCommand(emcNone,   False, mbXLeft,   ccAny,    cdUp, [], []);
    end;
  end;

  with FGutterActionsFold do begin
    AddCommand(emcNone,                False, mbXLeft,   ccAny,    CDir, R, [], []);
  end;
  with FGutterActionsFoldCol do begin
    AddCommand(emcCodeFoldCollaps,     False, mbXLeft,   ccAny,    CDir, R, [ssAlt],   [ssAlt, SYNEDIT_LINK_MODIFIER], emcoCodeFoldCollapsOne);
    AddCommand(emcCodeFoldExpand,      False, mbXLeft,   ccAny,    CDir, R, [SYNEDIT_LINK_MODIFIER],  [ssAlt, SYNEDIT_LINK_MODIFIER], emcoCodeFoldExpandAll);
    AddCommand(emcCodeFoldExpand,      False, mbXLeft,   ccAny,    CDir, R, [],        [],              emcoCodeFoldExpandOne);
    // TODO: why depend on FTextMiddleClick?
    if FTextMiddleClick <> mbaNone then
      AddCommand(emcCodeFoldCollaps,   False, mbXMiddle, ccAny,    CDir, R, [],       [],               emcoCodeFoldCollapsOne);
    // do not allow selection, over colapse/expand icons. Those may depend cursor pos (e.g. hide selected lines)
    if CDir = cdUp then
      AddCommand(emcNone,              False, mbXLeft,   ccAny,    cdDown, [], []);
  end;
  with FGutterActionsFoldExp do begin
    AddCommand(emcCodeFoldCollaps,     False, mbXLeft,   ccAny,    CDir, R, [],       [SYNEDIT_LINK_MODIFIER], emcoCodeFoldCollapsOne);
    AddCommand(emcCodeFoldCollaps,     False, mbXLeft,   ccAny,    CDir, R, [SYNEDIT_LINK_MODIFIER], [SYNEDIT_LINK_MODIFIER], emcoCodeFoldCollapsAll);
    // TODO: why depend on FTextMiddleClick?
    if FTextMiddleClick <> mbaNone then
      AddCommand(emcCodeFoldCollaps,   False, mbXMiddle, ccAny,    CDir, R, [],       [],       emcoCodeFoldCollapsOne);
    // do not allow selection, over colapse/expand icons. Those may depend cursor pos (e.g. hide selected lines)
    if CDir = cdUp then
      AddCommand(emcNone,              False, mbXLeft,   ccAny,    cdDown, [], []);
  end;

  with FGutterActionsOverViewMarks do begin
    R := R - [crLastDownPosSameLine];
    if R <> [] then
      R := R + [crAllowFallback];
    AddCommand(emcOverViewGutterGotoMark, True, mbXLeft, ccAny,  CDir, R, [], [ssShift, ssCtrl, ssAlt]);
  end;
  with FGutterActionsOverView do begin
    if R <> [] then
      R := R + [crLastDownPosSearchAll];
    AddCommand(emcOverViewGutterScrollTo, True, mbXLeft, ccAny,  CDir, R, [], [ssShift, ssCtrl, ssAlt]);
  end;

end;

procedure TEditorMouseOptions.ResetTextToDefault;

  procedure AddBtnClick(AnAction: TMouseOptButtonAction; const AButton: TSynMouseButton;
    AShift, AShiftMask: TShiftState; AddLinkDummy: Boolean = False;
    ASelContShift: TShiftState = []; AClickCount: TSynMAClickCount = ccSingle;
    AMoveCaret: Boolean = True; ADir: TSynMAClickDir = cdUp);

      procedure AddSelCommand(const ACmd: TSynEditorMouseCommand);
      begin
        AShiftMask := AShiftMask + ASelContShift;
        FTextActions.AddCommand(  ACmd, True, AButton, AClickCount, cdDown, AShift,              AShiftMask, emcoSelectionStart);
        if ASelContShift <> [] then
          FTextActions.AddCommand(ACmd, True, AButton, AClickCount, cdDown, AShift+ASelContShift, AShiftMask, emcoSelectionContinue);
      end;

  begin
    with FTextActions do begin
      case AnAction of
        mbaNone: {nothing};
        mbaSelect:       AddSelCommand(emcStartSelections);
        mbaSelectColumn: AddSelCommand(emcStartColumnSelections);
        mbaSelectLine:   AddSelCommand(emcStartLineSelections);
        //mbaSelectTokens: AddSelCommand(emcStartSelectTokens);
        //mbaSelectWords:  AddSelCommand(emcStartSelectWords);
        //mbaSelectLines:  AddSelCommand(emcStartSelectLines);
        //mbaSelectTokens: AddCommand(emcStartSelectTokens, True, AButton, AClickCount, cdDown, AShift, AShiftMask, emcoSelectionStart);
        mbaSelectWords:  AddCommand(emcStartSelectWords,  True, AButton, AClickCount, cdDown, AShift, AShiftMask, emcoSelectionStart);
        //mbaSelectLines:  AddCommand(emcStartSelectLines,  True, AButton, AClickCount, cdDown, AShift, AShiftMask, emcoSelectionStart);
        mbaSelectSetWord:
            AddCommand(emcSelectWord,       True,  AButton, AClickCount, ADir, AShift, AShiftMask);
        mbaSelectSetLineSmart:
            AddCommand(emcSelectLine,       True,  AButton, AClickCount, ADir, AShift, AShiftMask, emcoSelectLineSmart);
        mbaSelectSetLineFull:
            AddCommand(emcSelectLine,       True,  AButton, AClickCount, ADir, AShift, AShiftMask, emcoSelectLineFull);
        mbaSelectSetPara:
            AddCommand(emcSelectPara,       True,  AButton, AClickCount, ADir, AShift, AShiftMask);
        mbaPaste:            // TODOS act on up? but needs to prevent selection on down
            AddCommand(emcPasteSelection,   True,  AButton, AClickCount, cdDown,  AShift, AShiftMask, 0, 0, 0, True);
        mbaDeclarationJump,
        mbaDeclarationOrBlockJump: begin
            if AddLinkDummy then
              AddCommand(emcMouseLink,      False, AButton, AClickCount, ADir,    [SYNEDIT_LINK_MODIFIER], [SYNEDIT_LINK_MODIFIER], emcoMouseLinkShow, 999);
            AddCommand(emcMouseLink,        False, AButton, AClickCount, ADir,    AShift, AShiftMask);
            if AnAction = mbaDeclarationOrBlockJump then
              AddCommand(emcSynEditCommand, True,  AButton, AClickCount, ADir,    AShift, AShiftMask, ecFindBlockOtherEnd, 1);
          end;
        mbaAddHistoryPoint:
          AddCommand(emcSynEditCommand,     True,  AButton, AClickCount, ADir, AShift, AShiftMask, ecAddJumpPoint);
        mbaHistoryBack:
          AddCommand(emcSynEditCommand,     False, AButton, AClickCount, ADir, AShift, AShiftMask, ecJumpBack);
        mbaHistoryForw:
          AddCommand(emcSynEditCommand,     False, AButton, AClickCount, ADir, AShift, AShiftMask, ecJumpForward);
        mbaSetFreeBookmark:
          AddCommand(emcSynEditCommand,     True,  AButton, AClickCount, ADir, AShift, AShiftMask, ecSetFreeBookmark);
        mbaZoomReset: begin
            AddCommand(emcWheelZoomNorm,    False, AButton, AClickCount, ADir, AShift, AShiftMask);
            FMainActions.AddCommand(emcWheelZoomNorm,    False,  AButton, AClickCount, ADir, AShift, AShiftMask);
          end;
        mbaContextMenu:
            AddCommand(emcContextMenu, AMoveCaret, AButton, AClickCount, ADir, AShift, AShiftMask, emcoSelectionCaretMoveNever);
        mbaContextMenuDebug:
            AddCommand(emcContextMenu, True,       AButton, AClickCount, ADir, AShift, AShiftMask, emcoSelectionCaretMoveOutside, 0, 1);
        mbaContextMenuTab:
            AddCommand(emcContextMenu, True,       AButton, AClickCount, ADir, AShift, AShiftMask, emcoSelectionCaretMoveOutside, 0, 2);
        mbaMultiCaretToggle:
          begin
            AddCommand(emcPluginMultiCaretToggleCaret, False, AButton, AClickCount, ADir, AShift, AShiftMask);
            FSelActions.AddCommand(emcPluginMultiCaretSelectionToCarets, False, AButton, AClickCount, ADir, AShift, AShiftMask);
          end;
      end;
    end;
  end;

  procedure AddWheelAct(AnAction: TMouseOptWheelAction; const AShift, AShiftMask: TShiftState);
  var
    opt: TSynEditorMouseCommandOpt;
    opt2: integer;
  begin
    opt2 := 0;
    with FMainActions do begin
      case AnAction of
        mwaNone: {nothing};
        mwaScroll:                 opt := emcoWheelScrollSystem;
        mwaScrollSingleLine:       opt := emcoWheelScrollLines;
        mwaScrollPage:             opt := emcoWheelScrollPages;
        mwaScrollPageLessOne:      opt := emcoWheelScrollPagesLessOne;
        mwaScrollHalfPage: begin
                                   opt := emcoWheelScrollPages;
                                   opt2 := 50;
          end;
        mwaScrollHoriz:            opt := emcoWheelScrollSystem;
        mwaScrollHorizSingleLine:  opt := emcoWheelScrollLines;
        mwaScrollHorizPage:        opt := emcoWheelScrollPages;
        mwaScrollHorizPageLessOne: opt := emcoWheelScrollPagesLessOne;
        mwaScrollHorizHalfPage: begin
                                   opt := emcoWheelScrollPages;
                                   opt2 := 50;
          end;
        mwaZoom: begin
            AddCommand(emcWheelZoomOut, False,  mbXWheelDown, ccAny, cdDown, AShift, AShiftMask);
            AddCommand(emcWheelZoomIn,  False,  mbXWheelUp,   ccAny, cdDown, AShift, AShiftMask);
          end;
      end;

      if AnAction in [mwaScroll, mwaScrollSingleLine, mwaScrollPage, mwaScrollPageLessOne, mwaScrollHalfPage] then begin
        AddCommand(emcWheelVertScrollDown,       False,  mbXWheelDown, ccAny, cdDown, AShift, AShiftMask, opt, 0, opt2);
        AddCommand(emcWheelVertScrollUp,         False,  mbXWheelUp,   ccAny, cdDown, AShift, AShiftMask, opt, 0, opt2);
      end;
      if AnAction in [mwaScrollHoriz, mwaScrollHorizSingleLine, mwaScrollHorizPage, mwaScrollHorizPageLessOne, mwaScrollHorizHalfPage] then begin
        AddCommand(emcWheelHorizScrollDown,       False,  mbXWheelDown, ccAny, cdDown, AShift, AShiftMask, opt, 0, opt2);
        AddCommand(emcWheelHorizScrollUp,         False,  mbXWheelUp,   ccAny, cdDown, AShift, AShiftMask, opt, 0, opt2);
      end;

    end;
  end;

var
  ModKeys, SelKey: TShiftState;
begin
  FMainActions.Clear;
  FSelActions.Clear;
  FTextActions.Clear;

  // Left Btn
  ModKeys := [ssShift];
  if FTextAltLeftClick     <> mbaNone then ModKeys := ModKeys + [ssAlt];
  if FTextCtrlLeftClick    <> mbaNone then ModKeys := ModKeys + [SYNEDIT_LINK_MODIFIER];
  if FTextAltCtrlLeftClick <> mbaNone then ModKeys := ModKeys + [ssAlt] + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftAltLeftClick     <> mbaNone then ModKeys := ModKeys + [ssAlt];
  if FTextShiftCtrlLeftClick    <> mbaNone then ModKeys := ModKeys + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftAltCtrlLeftClick <> mbaNone then ModKeys := ModKeys + [ssAlt] + [SYNEDIT_LINK_MODIFIER];
  if FTextAltDoubleLeftClick     <> mbaNone then ModKeys := ModKeys + [ssAlt];
  if FTextCtrlDoubleLeftClick    <> mbaNone then ModKeys := ModKeys + [SYNEDIT_LINK_MODIFIER];

  if FTextShiftLeftClick = mbaNone
  then SelKey := [ssShift]
  else SelKey := [];
  AddBtnClick(mbaSelect,                  mbXLeft,   [],                      ModKeys, False, SelKey);
  AddBtnClick(FTextShiftLeftClick,        mbXLeft,   [ssShift],               ModKeys, False, SelKey);

  if FTextShiftCtrlLeftClick = mbaNone
  then SelKey := [ssShift]
  else SelKey := [];
  AddBtnClick(FTextCtrlLeftClick,         mbXLeft,   [SYNEDIT_LINK_MODIFIER],          ModKeys, False, SelKey);
  AddBtnClick(FTextShiftCtrlLeftClick,    mbXLeft,   [ssShift, SYNEDIT_LINK_MODIFIER], ModKeys, False, SelKey);

  if FTextShiftAltLeftClick = mbaNone
  then SelKey := [ssShift]
  else SelKey := [];
  AddBtnClick(FTextAltLeftClick,          mbXLeft,   [ssAlt],                 ModKeys, False, SelKey);
  AddBtnClick(FTextShiftAltLeftClick,     mbXLeft,   [ssShift, ssAlt],               ModKeys, False, SelKey);

  if FTextShiftAltCtrlLeftClick = mbaNone
  then SelKey := [ssShift]
  else SelKey := [];
  AddBtnClick(FTextAltCtrlLeftClick,      mbXLeft, [ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys, False, SelKey);
  AddBtnClick(FTextShiftAltCtrlLeftClick, mbXLeft, [ssShift, ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys, False, SelKey);

  SelKey := [];
  AddBtnClick(FTextDoubleLeftClick,        mbXLeft,   [], ModKeys, False, SelKey, ccDouble);
  AddBtnClick(FTextTripleLeftClick,        mbXLeft,   [], ModKeys, False, SelKey, ccTriple);
  AddBtnClick(FTextQuadLeftClick,          mbXLeft,   [], ModKeys, False, SelKey, ccQuad);
  AddBtnClick(FTextShiftDoubleLeftClick,   mbXLeft,   [ssShift],               ModKeys, False, SelKey, ccDouble);
  AddBtnClick(FTextCtrlDoubleLeftClick,    mbXLeft,   [SYNEDIT_LINK_MODIFIER], ModKeys, False, SelKey, ccDouble);
  AddBtnClick(FTextAltDoubleLeftClick,     mbXLeft,   [ssAlt],                 ModKeys, False, SelKey, ccDouble);


  SelKey := [];
  ModKeys := [];
  if FTextShiftMiddleClick         <> mbaNone then ModKeys := ModKeys + [ssShift];
  if FTextCtrlMiddleClick          <> mbaNone then ModKeys := ModKeys + [SYNEDIT_LINK_MODIFIER];
  if FTextAltMiddleClick           <> mbaNone then ModKeys := ModKeys + [ssAlt];
  if FTextAltCtrlMiddleClick       <> mbaNone then ModKeys := ModKeys + [ssAlt] + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftCtrlMiddleClick     <> mbaNone then ModKeys := ModKeys + [ssShift] + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftAltMiddleClick      <> mbaNone then ModKeys := ModKeys + [ssShift, ssAlt];
  if FTextShiftAltCtrlMiddleClick  <> mbaNone then ModKeys := ModKeys + [ssShift, ssAlt] + [SYNEDIT_LINK_MODIFIER];
  AddBtnClick(FTextMiddleClick,     mbXMiddle, [], ModKeys, FTextCtrlMiddleClick = mbaNone);
  AddBtnClick(FTextShiftMiddleClick,mbXMiddle, [ssShift], ModKeys);
  AddBtnClick(FTextAltMiddleClick,  mbXMiddle, [ssAlt], ModKeys);
  AddBtnClick(FTextCtrlMiddleClick, mbXMiddle, [SYNEDIT_LINK_MODIFIER], ModKeys);
  AddBtnClick(FTextAltCtrlMiddleClick,      mbXMiddle, [ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys);
  AddBtnClick(FTextShiftCtrlMiddleClick,    mbXMiddle, [ssShift, SYNEDIT_LINK_MODIFIER], ModKeys);
  AddBtnClick(FTextShiftAltMiddleClick,     mbXMiddle, [ssShift, ssAlt], ModKeys);
  AddBtnClick(FTextShiftAltCtrlMiddleClick, mbXMiddle, [ssShift, ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys);

  SelKey := [];
  ModKeys := [];
  if FTextShiftRightClick         <> mbaNone then ModKeys := ModKeys + [ssShift];
  if FTextCtrlRightClick          <> mbaNone then ModKeys := ModKeys + [SYNEDIT_LINK_MODIFIER];
  if FTextAltRightClick           <> mbaNone then ModKeys := ModKeys + [ssAlt];
  if FTextAltCtrlRightClick       <> mbaNone then ModKeys := ModKeys + [ssAlt] + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftCtrlRightClick     <> mbaNone then ModKeys := ModKeys + [ssShift] + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftAltRightClick      <> mbaNone then ModKeys := ModKeys + [ssShift, ssAlt];
  if FTextShiftAltCtrlRightClick  <> mbaNone then ModKeys := ModKeys + [ssShift, ssAlt] + [SYNEDIT_LINK_MODIFIER];
  AddBtnClick(FTextRightClick,     mbXRight, [], ModKeys, FTextCtrlRightClick = mbaNone, [], ccSingle, FTextRightMoveCaret);
  AddBtnClick(FTextShiftRightClick,mbXRight, [ssShift], ModKeys, False, [], ccSingle, FTextRightMoveCaret);
  AddBtnClick(FTextAltRightClick,  mbXRight, [ssAlt], ModKeys, False, [], ccSingle, FTextRightMoveCaret);
  AddBtnClick(FTextCtrlRightClick, mbXRight, [SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, FTextRightMoveCaret);
  AddBtnClick(FTextAltCtrlRightClick,      mbXRight, [ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, FTextRightMoveCaret);
  AddBtnClick(FTextShiftCtrlRightClick,    mbXRight, [ssShift, SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, FTextRightMoveCaret);
  AddBtnClick(FTextShiftAltRightClick,     mbXRight, [ssShift, ssAlt], ModKeys, False, [], ccSingle, FTextRightMoveCaret);
  AddBtnClick(FTextShiftAltCtrlRightClick, mbXRight, [ssShift, ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, FTextRightMoveCaret);

  SelKey := [];
  ModKeys := [];
  if FTextShiftExtra1Click         <> mbaNone then ModKeys := ModKeys + [ssShift];
  if FTextCtrlExtra1Click          <> mbaNone then ModKeys := ModKeys + [SYNEDIT_LINK_MODIFIER];
  if FTextAltExtra1Click           <> mbaNone then ModKeys := ModKeys + [ssAlt];
  if FTextAltCtrlExtra1Click       <> mbaNone then ModKeys := ModKeys + [ssAlt] + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftCtrlExtra1Click     <> mbaNone then ModKeys := ModKeys + [ssShift] + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftAltExtra1Click      <> mbaNone then ModKeys := ModKeys + [ssShift, ssAlt];
  if FTextShiftAltCtrlExtra1Click  <> mbaNone then ModKeys := ModKeys + [ssShift, ssAlt] + [SYNEDIT_LINK_MODIFIER];
  AddBtnClick(FTextExtra1Click,     mbXExtra1, [], ModKeys, FTextCtrlExtra1Click = mbaNone, [], ccSingle, True, cdDown);
  AddBtnClick(FTextShiftExtra1Click,mbXExtra1, [ssShift], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextAltExtra1Click,  mbXExtra1, [ssAlt], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextCtrlExtra1Click, mbXExtra1, [SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextAltCtrlExtra1Click,      mbXExtra1, [ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextShiftCtrlExtra1Click,    mbXExtra1, [ssShift, SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextShiftAltExtra1Click,     mbXExtra1, [ssShift, ssAlt], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextShiftAltCtrlExtra1Click, mbXExtra1, [ssShift, ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, True, cdDown);

  // TODO: on w32 extra btn do not call mouse up
  SelKey := [];
  ModKeys := [];
  if FTextShiftExtra2Click         <> mbaNone then ModKeys := ModKeys + [ssShift];
  if FTextCtrlExtra2Click          <> mbaNone then ModKeys := ModKeys + [SYNEDIT_LINK_MODIFIER];
  if FTextAltExtra2Click           <> mbaNone then ModKeys := ModKeys + [ssAlt];
  if FTextAltCtrlExtra2Click       <> mbaNone then ModKeys := ModKeys + [ssAlt] + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftCtrlExtra2Click     <> mbaNone then ModKeys := ModKeys + [ssShift] + [SYNEDIT_LINK_MODIFIER];
  if FTextShiftAltExtra2Click      <> mbaNone then ModKeys := ModKeys + [ssShift, ssAlt];
  if FTextShiftAltCtrlExtra2Click  <> mbaNone then ModKeys := ModKeys + [ssShift, ssAlt] + [SYNEDIT_LINK_MODIFIER];
  AddBtnClick(FTextExtra2Click,     mbXExtra2, [], ModKeys, FTextCtrlExtra2Click = mbaNone, [], ccSingle, True, cdDown);
  AddBtnClick(FTextShiftExtra2Click,mbXExtra2, [ssShift], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextAltExtra2Click,  mbXExtra2, [ssAlt], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextCtrlExtra2Click, mbXExtra2, [SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextAltCtrlExtra2Click,      mbXExtra2, [ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextShiftCtrlExtra2Click,    mbXExtra2, [ssShift, SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextShiftAltExtra2Click,     mbXExtra2, [ssShift, ssAlt], ModKeys, False, [], ccSingle, True, cdDown);
  AddBtnClick(FTextShiftAltCtrlExtra2Click, mbXExtra2, [ssShift, ssAlt, SYNEDIT_LINK_MODIFIER], ModKeys, False, [], ccSingle, True, cdDown);

  ModKeys := [];
  if FShiftWheel         <> mwaNone then ModKeys := ModKeys + [ssShift];
  if FCtrlWheel          <> mwaNone then ModKeys := ModKeys + [SYNEDIT_LINK_MODIFIER];
  if FAltWheel           <> mwaNone then ModKeys := ModKeys + [ssAlt];
  if FAltCtrlWheel       <> mwaNone then ModKeys := ModKeys + [ssAlt] + [SYNEDIT_LINK_MODIFIER];
  if FShiftCtrlWheel     <> mwaNone then ModKeys := ModKeys + [ssShift] + [SYNEDIT_LINK_MODIFIER];
  if FShiftAltWheel      <> mwaNone then ModKeys := ModKeys + [ssShift, ssAlt];
  if FShiftAltCtrlWheel  <> mwaNone then ModKeys := ModKeys + [ssShift, ssAlt] + [SYNEDIT_LINK_MODIFIER];
  AddWheelAct(FWheel, [], []);
  AddWheelAct(FCtrlWheel,  [ssCtrl],  ModKeys);
  AddWheelAct(FAltWheel,   [ssAlt],   ModKeys);
  AddWheelAct(FShiftWheel, [ssShift], ModKeys);
  AddWheelAct(FAltCtrlWheel,      [ssAlt, ssCtrl], ModKeys);
  AddWheelAct(FShiftCtrlWheel,    [ssShift, ssCtrl], ModKeys);
  AddWheelAct(FShiftAltWheel,     [ssShift, ssAlt], ModKeys);
  AddWheelAct(FShiftAltCtrlWheel, [ssShift, ssAlt, ssCtrl], ModKeys);

  if FTextDrag then
    with FSelActions do begin
      AddCommand(emcStartDragMove, False, mbXLeft, ccSingle, cdDown, [], [], emcoNotDragedNoCaretOnUp);
    end;
    FTextActions.AddCommand(emcNone, True, mbXLeft, ccSingle, cdUp, [], [], 0, 99);
end;

procedure TEditorMouseOptions.ResetToUserScheme;
var
  i: LongInt;
begin
  i := SelectedUserSchemeIndex;
  if i < 0 then exit;
  AssignActions(UserSchemesAtPos[i]);
end;

procedure TEditorMouseOptions.AssignActions(Src: TEditorMouseOptions);
begin
  FMainActions.Assign         (Src.MainActions);
  FSelActions.Assign          (Src.SelActions);
  FTextActions.Assign         (Src.TextActions);
  FGutterActions.Assign       (Src.GutterActions);
  FGutterActionsFold.Assign   (Src.GutterActionsFold);
  FGutterActionsFoldExp.Assign(Src.GutterActionsFoldExp);
  FGutterActionsFoldCol.Assign(Src.GutterActionsFoldCol);
  FGutterActionsLines.Assign  (Src.GutterActionsLines);
  FGutterActionsChanges.Assign(Src.GutterActionsChanges);
  FGutterActionsOverView.Assign(Src.GutterActionsOverView);
  FGutterActionsOverViewMarks.Assign(Src.GutterActionsOverViewMarks);
end;

procedure TEditorMouseOptions.SetTextCtrlLeftClick(AValue: TMouseOptButtonActionOld);
begin
  // upgrade old values
  if AValue in [low(MouseOptButtonActionOld)..high(MouseOptButtonActionOld)] then
    AValue := MouseOptButtonActionOld[AValue];
  if FTextCtrlLeftClick = AValue then Exit;
  FTextCtrlLeftClick := AValue;
end;

procedure TEditorMouseOptions.SetTextMiddleClick(AValue: TMouseOptButtonActionOld);
begin
  // upgrade old values
  if AValue in [low(MouseOptButtonActionOld)..high(MouseOptButtonActionOld)] then
    AValue := MouseOptButtonActionOld[AValue];
  if FTextMiddleClick = AValue then Exit;
  FTextMiddleClick := AValue;
end;

procedure TEditorMouseOptions.AssignEx(Src: TEditorMouseOptions; WithUserSchemes: Boolean);
var
  i: Integer;
begin
  FName                 := Src.FName;

  FGutterLeft           := Src.GutterLeft;
  FSelectOnLineNumbers  := Src.SelectOnLineNumbers;
  FTextDrag             := Src.TextDrag;
  FTextRightMoveCaret   := Src.TextRightMoveCaret;
  FSelectedUserScheme   := Src.FSelectedUserScheme;

    // left multi click
  FTextDoubleLeftClick       := Src.TextDoubleLeftClick;
  FTextTripleLeftClick      := Src.TextTripleLeftClick;
  FTextQuadLeftClick         := Src.TextQuadLeftClick;
  FTextShiftDoubleLeftClick  := Src.TextShiftDoubleLeftClick;
  FTextAltDoubleLeftClick    := Src.TextAltDoubleLeftClick;
  FTextCtrlDoubleLeftClick   := Src.TextCtrlDoubleLeftClick;
    // left + modifier click
  FTextAltLeftClick          := Src.TextAltLeftClick;
  FTextCtrlLeftClick         := Src.TextCtrlLeftClick;
  FTextAltCtrlLeftClick      := Src.TextAltCtrlLeftClick;
  FTextShiftLeftClick        := Src.TextShiftLeftClick;
  FTextShiftAltLeftClick     := Src.TextShiftAltLeftClick;
  FTextShiftCtrlLeftClick    := Src.TextShiftCtrlLeftClick;
  FTextShiftAltCtrlLeftClick := Src.TextShiftAltCtrlLeftClick;
    // middle click
  FTextMiddleClick           := Src.TextMiddleClick;
  FTextAltMiddleClick        := Src.TextAltMiddleClick;
  FTextCtrlMiddleClick       := Src.TextCtrlMiddleClick;
  FTextShiftMiddleClick      := Src.TextShiftMiddleClick;
  FTextAltCtrlMiddleClick      := Src.TextAltCtrlMiddleClick;
  FTextShiftAltMiddleClick     := Src.TextShiftAltMiddleClick;
  FTextShiftCtrlMiddleClick    := Src.TextShiftCtrlMiddleClick;
  FTextShiftAltCtrlMiddleClick := Src.TextShiftAltCtrlMiddleClick;
  // wheel
  FWheel                := Src.Wheel;
  FCtrlWheel            := Src.CtrlWheel;
  FAltWheel             := Src.AltWheel;
  FShiftWheel           := Src.ShiftWheel;
  FAltCtrlWheel         := Src.AltCtrlWheel;
  FShiftCtrlWheel       := Src.ShiftCtrlWheel;
  FShiftAltWheel        := Src.ShiftAltWheel;
  FShiftAltCtrlWheel    := Src.ShiftAltCtrlWheel;
  // right
  FTextAltCtrlRightClick := Src.TextAltCtrlRightClick;
  FTextAltRightClick := Src.TextAltRightClick;
  FTextCtrlRightClick := Src.TextCtrlRightClick;
  FTextRightClick := Src.TextRightClick;
  FTextShiftAltCtrlRightClick := Src.TextShiftAltCtrlRightClick;
  FTextShiftAltRightClick := Src.TextShiftAltRightClick;
  FTextShiftCtrlRightClick := Src.TextShiftCtrlRightClick;
  FTextShiftRightClick := Src.TextShiftRightClick;
  // extra-1 click
  FTextAltCtrlExtra1Click := Src.TextAltCtrlExtra1Click;
  FTextAltExtra1Click := Src.TextAltExtra1Click;
  FTextCtrlExtra1Click := Src.TextCtrlExtra1Click;
  FTextExtra1Click := Src.TextExtra1Click;
  FTextShiftAltCtrlExtra1Click := Src.TextShiftAltCtrlExtra1Click;
  FTextShiftAltExtra1Click := Src.TextShiftAltExtra1Click;
  FTextShiftCtrlExtra1Click := Src.TextShiftCtrlExtra1Click;
  FTextShiftExtra1Click := Src.TextShiftExtra1Click;
  // extra-2 click
  FTextAltCtrlExtra2Click := Src.TextAltCtrlExtra2Click;
  FTextAltExtra2Click := Src.TextAltExtra2Click;
  FTextCtrlExtra2Click := Src.TextCtrlExtra2Click;
  FTextExtra2Click := Src.TextExtra2Click;
  FTextShiftAltCtrlExtra2Click := Src.TextShiftAltCtrlExtra2Click;
  FTextShiftAltExtra2Click := Src.TextShiftAltExtra2Click;
  FTextShiftCtrlExtra2Click := Src.TextShiftCtrlExtra2Click;
  FTextShiftExtra2Click := Src.TextShiftExtra2Click;



  AssignActions(Src);

  if WithUserSchemes then begin
    ClearUserSchemes;
    for i := 0 to Src.FUserSchemes.Count - 1 do begin
      FUserSchemes.AddObject(Src.FUserSchemes[i], TEditorMouseOptions.Create);
      TEditorMouseOptions(FUserSchemes.Objects[i]).Assign
        ( TEditorMouseOptions(Src.FUserSchemes.Objects[i]) );
    end;
  end;
end;

procedure TEditorMouseOptions.Assign(Src: TEditorMouseOptions);
begin
  AssignEx(Src, True);
end;

function TEditorMouseOptions.IsPresetEqualToMouseActions: Boolean;
var
  Temp: TEditorMouseOptions;
  i: Integer;
begin
  i := SelectedUserSchemeIndex;
  Temp := TEditorMouseOptions.Create;
  Temp.AssignEx(self, i >= 0);
  if i >= 0 then begin
    Temp.ResetToUserScheme;
  end else begin
    Temp.ResetTextToDefault;
    Temp.ResetGutterToDefault;
  end;
  Result :=
    Temp.MainActions.Equals(self.MainActions) and
    Temp.SelActions.Equals (self.SelActions) and
    Temp.TextActions.Equals (self.TextActions) and
    Temp.GutterActions.Equals       (self.GutterActions) and
    Temp.GutterActionsFold.Equals   (self.GutterActionsFold) and
    Temp.GutterActionsFoldCol.Equals(self.GutterActionsFoldCol) and
    Temp.GutterActionsFoldExp.Equals(self.GutterActionsFoldExp) and
    Temp.GutterActionsLines.Equals  (self.GutterActionsLines) and
    Temp.GutterActionsChanges.Equals(Self.GutterActionsChanges) and
    Temp.GutterActionsOverView.Equals(Self.GutterActionsOverView) and
    Temp.GutterActionsOverViewMarks.Equals(Self.GutterActionsOverViewMarks);
  Temp.Free;
end;

function TEditorMouseOptions.CalcCustomSavedActions: Boolean;
begin
  Result := not IsPresetEqualToMouseActions;
  FCustomSavedActions := Result;
end;

procedure TEditorMouseOptions.LoadFromXml(aXMLConfig: TRttiXMLConfig; aPath: String;
  aOldPath: String; FileVersion: Integer);

  Procedure LoadMouseAct(Path: String; MActions: TSynEditMouseActions);
  var
    c, i: Integer;
    MAct: TSynEditMouseActionKeyCmdHelper;
    //ErrShown: Boolean;
  begin
    //ErrShown := False;
    MActions.Clear;
    MAct := TSynEditMouseActionKeyCmdHelper.Create(nil);

    c := aXMLConfig.GetValue(Path + 'Count', 0);
    for i := 0 to c - 1 do begin
      try
        MActions.IncAssertLock;
        try
          // If the object would ever be extended, old configs will not have all properties.
          Mact.Clear;
          aXMLConfig.ReadObject(Path + 'M' + IntToStr(i) + '/', MAct);
          MActions.Add.Assign(MAct);
        finally
          MActions.DecAssertLock;
        end;
        MActions.AssertNoConflict(MAct);
      except
        MActions.Delete(MActions.Count-1);
        //if not ErrShown then
        //  IDEMessageDialog(dlgMouseOptErrorDup, dlgMouseOptErrorDupText, mtError, [mbOk]);
        //ErrShown := True;
      end;
    end;
    MAct.Free;
  end;

var
  AltColumnMode: Boolean;
  TextDoubleSelLine: Boolean;
begin
  Reset;
  if FileVersion < 11 then
    FGutterLeft := moGLDownClick;
  AltColumnMode := False;
  TextDoubleSelLine := False;
  if aOldPath <> '' then begin
    // Read deprecated value
    // It is on by default, so only if a user switched it off, actions is required
    if not aXMLConfig.GetValue(aOldPath + 'DragDropEditing', True) then
      TextDrag := False;
    aXMLConfig.DeleteValue(aOldPath + 'DragDropEditing');

    if aXMLConfig.GetValue(aOldPath + 'AltSetsColumnMode', False) then
      AltColumnMode := True;
    aXMLConfig.DeleteValue(aOldPath + 'AltSetsColumnMode');

    if not aXMLConfig.GetValue(aOldPath + 'CtrlMouseLinks', True) then
      TextCtrlLeftClick := mbaNone;
    aXMLConfig.DeleteValue(aOldPath + 'CtrlMouseLinks');

    if aXMLConfig.GetValue(aOldPath + 'DoubleClickSelectsLine', False) then
      TextDoubleSelLine := True;
    aXMLConfig.DeleteValue(aOldPath + 'DoubleClickSelectsLine');
  end;

  //AltColumnMode, before TextAltLeftClick
  if (not AltColumnMode) then
    AltColumnMode := aXMLConfig.GetValue(aPath + 'Default/AltColumnMode', True);
  aXMLConfig.DeleteValue(aPath + 'Default/AltColumnMode');

  if (not AltColumnMode) then
    TextAltLeftClick := mbaNone;

  if aXMLConfig.GetValue(aPath + 'Default/TextDoubleSelLine', TextDoubleSelLine) then begin
    FTextDoubleLeftClick       := mbaSelectSetLineSmart;
    FTextTripleLeftClick       := mbaSelectSetLineFull;
  end;
  aXMLConfig.DeleteValue(aPath + 'Default/TextDoubleSelLine');

  CustomSavedActions := False;
  aXMLConfig.ReadObject(aPath + 'Default/', Self);

  if (FSelectedUserScheme <> '') and (UserSchemes[FSelectedUserScheme] = nil) then
    FSelectedUserScheme := '';

  if CustomSavedActions then begin
    // Load
    LoadMouseAct(aPath + 'Main/',          MainActions);
    LoadMouseAct(aPath + 'MainText/',      TextActions);
    LoadMouseAct(aPath + 'MainSelection/', SelActions);
    LoadMouseAct(aPath + 'Gutter/',        GutterActions);
    LoadMouseAct(aPath + 'GutterFold/',    GutterActionsFold);
    LoadMouseAct(aPath + 'GutterFoldExp/', GutterActionsFoldExp);
    LoadMouseAct(aPath + 'GutterFoldCol/', GutterActionsFoldCol);
    LoadMouseAct(aPath + 'GutterLineNum/', GutterActionsLines);
    LoadMouseAct(aPath + 'GutterLineChange/', GutterActionsChanges);
    LoadMouseAct(aPath + 'GutterOverView/',   GutterActionsOverView);
    LoadMouseAct(aPath + 'GutterOverViewMarks/',   GutterActionsOverViewMarks);

    if Version < 1 then begin
      try
        FMainActions.AddCommand(emcWheelVertScrollDown,       False,  mbXWheelDown, ccAny, cdDown, [], []);
        FMainActions.AddCommand(emcWheelVertScrollUp,         False,  mbXWheelUp,   ccAny, cdDown, [], []);
      except
      end;
    end;
  end
  else
  if (FSelectedUserScheme <> '') then begin
    ResetToUserScheme;
  end else begin
    ResetTextToDefault;
    ResetGutterToDefault;
  end;
end;

procedure TEditorMouseOptions.SaveToXml(aXMLConfig: TRttiXMLConfig; aPath: String);

  Procedure SaveMouseAct(Path: String; MActions: TSynEditMouseActions);
  var
    i, OldCnt: Integer;
    MAct: TSynEditMouseActionKeyCmdHelper;
  begin
    MAct := TSynEditMouseActionKeyCmdHelper.Create(nil);
    OldCnt := aXMLConfig.GetValue(Path + 'Count', 0);
    for i := 0 to MActions.Count - 1 do begin
      if MActions[i].Command = emcSynEditCommand then begin
        MAct.Assign(MActions[i]);
        aXMLConfig.WriteObject(Path + 'M' + IntToStr(i) + '/', MAct);
      end else
        aXMLConfig.WriteObject(Path + 'M' + IntToStr(i) + '/', MActions[i]);
    end;
    aXMLConfig.SetDeleteValue(Path + 'Count', MActions.Count,0);
    for i := MActions.Count to OldCnt do
      aXMLConfig.DeletePath(Path + 'M' + IntToStr(i));
    MAct.Free;
  end;

var
  DefMouseSettings: TEditorMouseOptions;
begin
  FVersion := EditorMouseOptsFormatVersion;
  DefMouseSettings := TEditorMouseOptions.Create;
  CalcCustomSavedActions;
  aXMLConfig.WriteObject(aPath + 'Default/', Self, DefMouseSettings);
  DefMouseSettings.Free;
  if CustomSavedActions then begin
    // Save full settings / based on empty
    SaveMouseAct(aPath + 'Main/',          MainActions);
    SaveMouseAct(aPath + 'MainText/',      TextActions);
    SaveMouseAct(aPath + 'MainSelection/', SelActions);
    SaveMouseAct(aPath + 'Gutter/',        GutterActions);
    SaveMouseAct(aPath + 'GutterFold/',    GutterActionsFold);
    SaveMouseAct(aPath + 'GutterFoldExp/', GutterActionsFoldExp);
    SaveMouseAct(aPath + 'GutterFoldCol/', GutterActionsFoldCol);
    SaveMouseAct(aPath + 'GutterLineNum/', GutterActionsLines);
    SaveMouseAct(aPath + 'GutterLineChange/', GutterActionsChanges);
    SaveMouseAct(aPath + 'GutterOverView/',   GutterActionsOverView);
    SaveMouseAct(aPath + 'GutterOverViewMarks/',GutterActionsOverViewMarks);
  end else begin
    // clear unused entries
    aXMLConfig.DeletePath(aPath + 'Main');
    aXMLConfig.DeletePath(aPath + 'MainSelection');
    aXMLConfig.DeletePath(aPath + 'Gutter');
    aXMLConfig.DeletePath(aPath + 'GutterFold');
    aXMLConfig.DeletePath(aPath + 'GutterFoldExp');
    aXMLConfig.DeletePath(aPath + 'GutterFoldCol');
    aXMLConfig.DeletePath(aPath + 'GutterLineNum');
  end;
end;

procedure TEditorMouseOptions.ImportFromXml(aXMLConfig: TRttiXMLConfig; aPath: String);

  Procedure LoadMouseAct(Path: String; MActions: TSynEditMouseActions);
  var
    i, c: Integer;
    MAct: TSynEditMouseActionKeyCmdHelper;
  begin
    MActions.Clear;
    MAct := TSynEditMouseActionKeyCmdHelper.Create(nil);
    c := aXMLConfig.GetValue(Path + 'Count', 0);
    for i := 0 to c - 1 do begin
      try
        MActions.IncAssertLock;
        try
          Mact.Clear;
          aXMLConfig.ReadObject(Path + 'M' + IntToStr(i) + '/', MAct);
          MActions.Add.Assign(MAct);
        finally
          MActions.DecAssertLock;
        end;
        MActions.AssertNoConflict(MAct);
      except
        MActions.Delete(MActions.Count-1);
        IDEMessageDialog(dlgMouseOptErrorDup, dlgMouseOptErrorDupText + LineEnding
                   + Path + 'M' + IntToStr(i) + LineEnding + MAct.DisplayName,
                   mtError, [mbOk]);
      end;
    end;
    Mact.Free;
  end;

begin
  LoadMouseAct(aPath + 'Main/',          MainActions);
  LoadMouseAct(aPath + 'MainText/',      TextActions);
  LoadMouseAct(aPath + 'MainSel/',       SelActions);
  LoadMouseAct(aPath + 'Gutter/',        GutterActions);
  LoadMouseAct(aPath + 'GutterFold/',    GutterActionsFold);
  LoadMouseAct(aPath + 'GutterFoldExp/', GutterActionsFoldExp);
  LoadMouseAct(aPath + 'GutterFoldCol/', GutterActionsFoldCol);
  LoadMouseAct(aPath + 'GutterLineNum/', GutterActionsLines);
  LoadMouseAct(aPath + 'GutterLineChange/', GutterActionsChanges);
  LoadMouseAct(aPath + 'GutterOverView/',   GutterActionsOverView);
  LoadMouseAct(aPath + 'GutterOverViewMarks/',GutterActionsOverViewMarks);
end;

procedure TEditorMouseOptions.ExportToXml(aXMLConfig: TRttiXMLConfig; aPath: String);
var
  MAct: TSynEditMouseActionKeyCmdHelper;

  Procedure SaveMouseAct(Path: String; MActions: TSynEditMouseActions);
  var
    i: Integer;
  begin
    for i := 0 to MActions.Count - 1 do
      if MActions[i].Command = emcSynEditCommand then begin
        MAct.Assign(MActions[i]);
        aXMLConfig.WriteObject(Path + 'M' + IntToStr(i) + '/', MAct);
      end
      else
        aXMLConfig.WriteObject(Path + 'M' + IntToStr(i) + '/', MActions[i]);
    aXMLConfig.SetDeleteValue(Path + 'Count', MActions.Count,0);
  end;

begin
  MAct := TSynEditMouseActionKeyCmdHelper.Create(nil);
  SaveMouseAct(aPath + 'Main/',          MainActions);
  SaveMouseAct(aPath + 'MainText/',      TextActions);
  SaveMouseAct(aPath + 'MainSel/',       SelActions);
  SaveMouseAct(aPath + 'Gutter/',        GutterActions);
  SaveMouseAct(aPath + 'GutterFold/',    GutterActionsFold);
  SaveMouseAct(aPath + 'GutterFoldExp/', GutterActionsFoldExp);
  SaveMouseAct(aPath + 'GutterFoldCol/', GutterActionsFoldCol);
  SaveMouseAct(aPath + 'GutterLineNum/', GutterActionsLines);
  SaveMouseAct(aPath + 'GutterLineChange/', GutterActionsChanges);
  SaveMouseAct(aPath + 'GutterOverView/',   GutterActionsOverView);
  SaveMouseAct(aPath + 'GutterOverViewMarks/',GutterActionsOverViewMarks);
  MAct.Free;
end;

procedure TEditorMouseOptions.LoadUserSchemes;
var
  i, j, k, c: Integer;
  FileList: TStringList;
  XMLConfig: TRttiXMLConfig;
  n: String;
begin
  ClearUserSchemes;
  if DirectoryExistsUTF8(UserSchemeDirectory(False)) then begin
    FileList := FindAllFiles(UserSchemeDirectory(False), '*.xml', False);
    for i := 0 to FileList.Count - 1 do begin
      XMLConfig := nil;
      try
        XMLConfig := TRttiXMLConfig.Create(FileList[i]);
        c := XMLConfig.GetValue('Lazarus/MouseSchemes/Names/Count', 0);
        for j := 0 to c-1 do begin
          n := XMLConfig.GetValue('Lazarus/MouseSchemes/Names/Item'+IntToStr(j+1)+'/Value', '');
          if n <> '' then begin
            k := FUserSchemes.AddObject(UTF8UpperCase(n), TEditorMouseOptions.Create);
            TEditorMouseOptions(FUserSchemes.Objects[k]).FName := n;
            TEditorMouseOptions(FUserSchemes.Objects[k]).ImportFromXml
              (XMLConfig, 'Lazarus/MouseSchemes/Scheme' + n + '/');
          end;
        end;
      except
        ShowMessage(Format(dlgUserSchemeError, [FileList[i]]));
      end;
      XMLConfig.Free;
    end;
    FileList.Free;
  end;
end;

function TEditorMouseOptions.UserSchemeCount: Integer;
begin
  Result := FUserSchemes.Count;
end;

function TEditorMouseOptions.IndexOfUserScheme(SchemeName: String): Integer;
begin
  Result := FUserSchemes.IndexOf(UTF8UpperCase(SchemeName));
end;

function TEditorMouseOptions.GetSelectedUserSchemeIndex: Integer;
begin
  if FSelectedUserScheme = '' then
    Result := -1
  else
    Result := IndexOfUserScheme(FSelectedUserScheme);
end;

procedure TEditorMouseOptions.SetSelectedUserScheme(const AValue: String);
begin
  if FSelectedUserScheme = AValue then exit;
  FSelectedUserScheme := AValue;
  ResetToUserScheme;
end;

procedure TEditorMouseOptions.SetSelectedUserSchemeIndex(const AValue: Integer);
begin
  if AValue < 0 then
    SelectedUserScheme := ''
  else
    SelectedUserScheme := TEditorMouseOptions(FUserSchemes.Objects[AValue]).Name;
end;

{ TEditorMouseOptionPresets }

constructor TEditorMouseOptionPresets.Create;
var
  FileList: TStringList;
  XMLConfig: TRttiXMLConfig;
  i, j, c: Integer;
  n: String;
begin
  FPreset := TQuickStringlist.Create;

  if DirectoryExistsUTF8(UserSchemeDirectory(False)) then begin
    FileList := FindAllFiles(UserSchemeDirectory(False), '*.xml', False);
    for i := 0 to FileList.Count - 1 do begin
      XMLConfig := nil;
      try
        XMLConfig := TRttiXMLConfig.Create(FileList[i]);
        c := XMLConfig.GetValue('Lazarus/MouseSchemes/Names/Count', 0);
        for j := 0 to c-1 do begin
          n := XMLConfig.GetValue('Lazarus/MouseSchemes/Names/Item'+IntToStr(j+1)+'/Value', '');
          if n <> '' then begin
            //NewMouse := TEditorMouseOptions.Create;

            //Singleton.RegisterScheme(XMLConfig, n, 'Lazarus/MouseSchemes/');
          end;
        end;
      except
        ShowMessage(Format(dlgUserSchemeError, [FileList[i]]));
      end;
      XMLConfig.Free;
    end;
    FileList.Free;
  end;

end;

destructor TEditorMouseOptionPresets.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FPreset);
end;

{ TEditorOptionsEditAccessOrderList }

function TEditorOptionsEditAccessOrderList.GetItems(Index: Integer): TEditorOptionsEditAccessOrderEntry;
begin
  Result := TEditorOptionsEditAccessOrderEntry(FList[Index]);
end;

constructor TEditorOptionsEditAccessOrderList.Create;
begin
  Flist := TFPList.Create;
  FSearchOrder := eoeaOrderByEditFocus;
end;

destructor TEditorOptionsEditAccessOrderList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TEditorOptionsEditAccessOrderList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  FList.Clear;
end;

procedure TEditorOptionsEditAccessOrderList.InitDefaults;
var
  i: Integer;
  Entry: TEditorOptionsEditAccessOrderEntry;
begin
  for i := 0 to high(EditorOptionsEditAccessDefaults) do begin
    Entry := TEditorOptionsEditAccessOrderEntry.Create(Self);
    Entry.InitFrom(EditorOptionsEditAccessDefaults[i]);
    FList.Add(Entry);
  end;
  Entry.FIsFallback := True;
end;

procedure TEditorOptionsEditAccessOrderList.Assign(Src: TEditorOptionsEditAccessOrderList);
var
  i: Integer;
  Entry: TEditorOptionsEditAccessOrderEntry;
begin
  Clear;
  FSearchOrder := Src.FSearchOrder;
  for i := 0 to Src.Count - 1 do begin
    Entry := TEditorOptionsEditAccessOrderEntry.Create(Self);
    Entry.Assign(Src[i]);
    FList.Add(Entry);
  end;
end;

procedure TEditorOptionsEditAccessOrderList.LoadFromXMLConfig(XMLConfig: TRttiXMLConfig;
  Path: String);
var
  i: Integer;
  def: TEditorOptionsEditAccessOrderList;
begin
  def := TEditorOptionsEditAccessOrderList.Create;
  XMLConfig.ReadObject(Path + 'Main/', self, def);
  def.Free;
  Path := Path + 'Entry/';
  for i := 0 to Count - 1 do
    XMLConfig.ReadObject(Path + Items[i].ID + '/', Items[i], Items[i].FDefaults);
end;

procedure TEditorOptionsEditAccessOrderList.SaveToXMLConfig(XMLConfig: TRttiXMLConfig;
  Path: String);
var
  i: Integer;
  def: TEditorOptionsEditAccessOrderList;
begin
  def := TEditorOptionsEditAccessOrderList.Create;
  XMLConfig.WriteObject(Path + 'Main/', Self, def);
  def.Free;
  Path := Path + 'Entry/';
  for i := 0 to Count - 1 do
    XMLConfig.WriteObject(Path + Items[i].ID + '/', Items[i], Items[i].FDefaults);
end;

function TEditorOptionsEditAccessOrderList.Count: Integer;
begin
  Result := FList.Count;
end;

{ TEditorOptionsEditAccessOrderEntry }

procedure TEditorOptionsEditAccessOrderEntry.AssignFrom(AValue: TEditorOptionsEditAccessDefaultEntry);
begin
  FId      := AValue.ID;
  FCaption := AValue.Caption;
  FDesc    := AValue.Desc;
  FEnabled := AValue.Enabled;
  FSearchInView  := AValue.SearchInView;
  FSearchLocked  := AValue.SearchLocked;
  FSearchOpenNew := AValue.SearchOpenNew;
  FSearchOrder   := AValue.SearchOrder;
end;

procedure TEditorOptionsEditAccessOrderEntry.SetEnabled(const AValue: Boolean);
begin
  FEnabled := AValue or FIsFallback;
end;

constructor TEditorOptionsEditAccessOrderEntry.Create(AList: TEditorOptionsEditAccessOrderList);
begin
  inherited Create;
  FList := AList;
end;

destructor TEditorOptionsEditAccessOrderEntry.Destroy;
begin
  FreeAndNil(FDefaults);
  inherited Destroy;
end;

procedure TEditorOptionsEditAccessOrderEntry.Assign(Src: TEditorOptionsEditAccessOrderEntry);
begin
  FId      := Src.FID;
  FCaption := Src.FCaption;
  FDesc    := Src.FDesc;
  FEnabled := Src.FEnabled;
  FIsFallback := Src.FIsFallback;
  FSearchInView  := Src.FSearchInView;
  FSearchLocked  := Src.FSearchLocked;
  FSearchOpenNew := Src.FSearchOpenNew;
  FSearchOrder   := Src.FSearchOrder;
  FreeAndNil(FDefaults);
  if Src.FDefaults <> nil then begin
    FDefaults := TEditorOptionsEditAccessOrderEntry.Create(nil);
    FDefaults.Assign(Src.FDefaults);
  end;
end;

procedure TEditorOptionsEditAccessOrderEntry.InitFrom(AValue: TEditorOptionsEditAccessDefaultEntry);
begin
  AssignFrom(AValue);
  FDefaults := TEditorOptionsEditAccessOrderEntry.Create(nil);
  FDefaults.AssignFrom(AValue);
end;

function TEditorOptionsEditAccessOrderEntry.RealSearchOrder: TEditorOptionsEditAccessOrder;
begin
  Result := SearchOrder;
  if Result = eoeaOrderByListPref then begin
    if FList = nil then Result := eoeaOrderByEditFocus;
    Result := FList.SearchOrder;
    if Result = eoeaOrderByListPref then Result := eoeaOrderByEditFocus;
  end;
end;

{ TEditorOptions }

constructor TEditorOptions.Create;
var
  ConfFileName: String;
  fs: TFileStreamUTF8;
  res: TResourceStream;
begin
  inherited Create;
  InitLocale;

  ConfFileName := AppendPathDelim(GetPrimaryConfigPath) + EditOptsConfFileName;
  CopySecondaryConfigFile(EditOptsConfFileName);
  try
    if (not FileExistsUTF8(ConfFileName)) then
    begin
      DebugLn('NOTE: editor options config file not found - using defaults');
      XMLConfig := TRttiXMLConfig.CreateClean(ConfFileName);
    end
    else
      XMLConfig := TRttiXMLConfig.Create(ConfFileName);
  except
    on E: Exception do
    begin
      DebugLn('WARNING: unable to read ', ConfFileName, ' ', E.Message);
      XMLConfig := Nil;
    end;
  end;

  // set defaults
  Init;

  // code templates (dci file)
  fCodeTemplateFileNameRaw :=
    TrimFilename(AppendPathDelim(GetPrimaryConfigPath)+DefaultCodeTemplatesFilename);
  CopySecondaryConfigFile(DefaultCodeTemplatesFilename);
  if not FileExistsUTF8(CodeTemplateFileNameExpand) then
  begin
    res := TResourceStream.Create(HInstance, PChar('lazarus_dci_file'), PChar(RT_RCDATA));
    try
      InvalidateFileStateCache;
      fs := TFileStreamUTF8.Create(CodeTemplateFileNameExpand, fmCreate);
      try
        fs.CopyFrom(res, res.Size);
      finally
        fs.Free;
      end;
    except
      DebugLn('WARNING: unable to write code template file "',
        CodeTemplateFileNameExpand, '"');
    end;
    res.Free;
  end;

  FMultiWinEditAccessOrder := TEditorOptionsEditAccessOrderList.Create;
  FMultiWinEditAccessOrder.InitDefaults;

  FDefaultValues := TEditorOptions.CreateDefaultOnly;
end;

constructor TEditorOptions.CreateDefaultOnly;
begin
  inherited Create;
  Init;
  FDefaultValues := nil;
end;

destructor TEditorOptions.Destroy;
begin
  FreeAndNil(FUserColorSchemeSettings);
  FreeAndNil(FUserDefinedColors);
  fKeyMap.Free;
  FreeAndNil(FMultiWinEditAccessOrder);
  XMLConfig.Free;
  FUserMouseSettings.Free;
  FTempMouseSettings.Free;
  FreeAndNil(FDefaultValues);
  inherited Destroy;
end;

procedure TEditorOptions.Init;
begin
  // General options
  fShowTabCloseButtons := True;
  FMultiLineTab := False;
  FHideSingleTabInWindow := False;
  fTabPosition := tpTop;
  FCopyWordAtCursorOnCopyNone := True;
  FShowGutterHints := True;
  fBlockIndent := 2;
  FBlockTabIndent := 0;
  fBlockIndentType := sbitSpace;
  FTrimSpaceType := settEditLine;
  fUndoLimit := 32767;
  fTabWidth := 8;
  FBracketHighlightStyle := sbhsBoth;
  FGutterSeparatorIndex := 3;
  fSynEditOptions := SynEditDefaultOptions;
  fSynEditOptions2 := SynEditDefaultOptions2;
  FMultiCaretOnColumnSelect := True;
  FMultiCaretDefaultMode := mcmMoveAllCarets;
  FMultiCaretDefaultColumnSelectMode := mcmCancelOnCaretMove;
  FMultiCaretDeleteSkipLineBreak := False;

  // Display options
  fEditorFont := SynDefaultFontName;
  fEditorFontSize := SynDefaultFontSize;
  fDisableAntialiasing := DefaultEditorDisableAntiAliasing;
  FShowOverviewGutter := True;
  FTopInfoView := True;

  // Key Mappings
  fKeyMappingScheme := KeyMapSchemeNames[kmsLazarus];
  fKeyMap := TKeyCommandRelationList.Create;

  // Mouse Mappings
  FUserMouseSettings := TEditorMouseOptions.Create;
  FTempMouseSettings := TEditorMouseOptions.Create;
  FUserMouseSettings.LoadUserSchemes;

  // Color options
  fHighlighterList := HighlighterListSingleton;
  FUserColorSchemeSettings := TColorSchemeFactory.Create;
  FUserColorSchemeSettings.Assign(ColorSchemeFactory);
  FUserDefinedColors := TEditorUserDefinedWordsList.Create;
  FUserDefinedColors.UseGlobalIDECommandList := True;

  FMarkupCurWordTime := 1500;
  FMarkupCurWordFullLen := 3;
  FMarkupCurWordNoKeyword := True;
  FMarkupCurWordTrim := True;
  FMarkupCurWordNoTimer := False;

  // hints
  FDbgHintAutoTypeCastClass := True;

  // Code Tools options
  FCompletionLongLineHintType := DefaultCompletionLongLineHintType;
  FAutoDisplayFuncPrototypes := True;

  // Code folding
  FReverseFoldPopUpOrder := True;

  // pas highlighter
  FPasExtendedKeywordsMode := False;
  FPasStringKeywordMode := spsmDefault;

  // Multi window
  FCtrlMiddleTabClickClosesOthers := True;
  FShowFileNameInCaption := False;

  // Comment
  FAnsiCommentContinueEnabled := False;
  FAnsiCommentMatch := '^\s?(\*)';
  FAnsiCommentMatchMode := scmMatchAtAsterisk;
  FAnsiCommentPrefix := '$1';
  FAnsiIndentMode := [sciAddTokenLen, sciAddPastTokenIndent,
                      sciAlignOnlyTokenLen, sciAlignOnlyPastTokenIndent,
                      sciMatchOnlyPastTokenIndent
                     ];
  FAnsiIndentAlignMax := 40;


  FCurlyCommentContinueEnabled := False;
  FCurlyCommentMatch := '^\s?(\*)';
  FCurlyCommentMatchMode := scmMatchAfterOpening;
  FCurlyCommentPrefix := '$1';
  FCurlyIndentMode := [sciAddTokenLen, sciAddPastTokenIndent,
                      sciAlignOnlyTokenLen, sciAlignOnlyPastTokenIndent,
                      sciMatchOnlyPastTokenIndent
                     ];
  FCurlyIndentAlignMax := 40;

  FSlashCommentContinueEnabled := False;
  FSlashCommentMatch := '^\s?(\*)';
  FSlashCommentMatchMode := scmMatchAfterOpening;
  FSlashCommentPrefix := '$1';
  FSlashIndentMode := [sciAddTokenLen, sciAddPastTokenIndent,
                      sciAlignOnlyTokenLen, sciAlignOnlyPastTokenIndent,
                      sciMatchOnlyPastTokenIndent
                     ];
  FSlashCommentExtend := sceMatching;
  FSlashIndentAlignMax := 40;

  FStringBreakEnabled := False;
  FStringBreakAppend  := ' +';
  FStringBreakPrefix  := '';
end;

procedure TEditorOptions.Load;
// load options from XML file
var
  SynEditOpt: TSynEditorOption;
  SynEditOptName: String;
  i: Integer;
  SynEditOpt2: TSynEditorOption2;
  FileVersion: LongInt;
  DefOpts: TSynEditorOptions;
begin
  try
    FileVersion:=XMLConfig.GetValue('EditorOptions/Version', EditorOptsFormatVersion);

    XMLConfig.ReadObject('EditorOptions/Misc/', Self, FDefaultValues);

    // general options
    DefOpts := SynEditDefaultOptions;
    if (FileVersion < 10) then DefOpts := DefOpts - [eoTabIndent];
    for SynEditOpt := Low(TSynEditorOption) to High(TSynEditorOption) do
    begin
      SynEditOptName := GetSynEditOptionName(SynEditOpt);
      if SynEditOptName <> '' then
        if XMLConfig.GetValue('EditorOptions/General/Editor/' + SynEditOptName,
          SynEditOpt in DefOpts) then
          Include(fSynEditOptions, SynEditOpt)
        else
          Exclude(fSynEditOptions, SynEditOpt);
    end;
    for SynEditOpt2 := Low(TSynEditorOption2) to High(TSynEditorOption2) do
    begin
      case SynEditOpt2 of
        eoCaretSkipsSelection:
          SynEditOptName := 'CaretSkipsSelection';
        eoCaretSkipTab:
          SynEditOptName := 'CaretSkipTab';
        eoAlwaysVisibleCaret:
          SynEditOptName := 'AlwaysVisibleCaret';
        eoEnhanceEndKey:
          SynEditOptName := 'EnhanceEndKey';
        eoFoldedCopyPaste:
          SynEditOptName := 'FoldedCopyPaste';
        eoPersistentBlock:
          SynEditOptName := 'PersistentBlock';
        eoOverwriteBlock:
          SynEditOptName := 'OverwriteBlock';
        eoAutoHideCursor:
          SynEditOptName := 'AutoHideCursor';
        eoCaretMoveEndsSelection, eoPersistentCaretStopBlink:
          WriteStr(SynEditOptName, SynEditOpt2);
        else
          SynEditOptName := '';
      end;
      if SynEditOptName <> '' then
        if XMLConfig.GetValue('EditorOptions/General/Editor/' + SynEditOptName,
          SynEditOpt2 in SynEditDefaultOptions2) then
          Include(fSynEditOptions2, SynEditOpt2)
        else
          Exclude(fSynEditOptions2, SynEditOpt2);
    end;

    fShowTabCloseButtons :=
      XMLConfig.GetValue(
      'EditorOptions/General/Editor/ShowTabCloseButtons', True);
    FHideSingleTabInWindow :=
      XMLConfig.GetValue(
      'EditorOptions/General/Editor/HideSingleTabInWindow', False);
    fShowTabNumbers :=
      XMLConfig.GetValue('EditorOptions/General/Editor/ShowTabNumbers', False);
    FCopyWordAtCursorOnCopyNone :=
      XMLConfig.GetValue(
      'EditorOptions/General/Editor/CopyWordAtCursorOnCopyNone', True);
    FShowGutterHints :=
      XMLConfig.GetValue('EditorOptions/General/Editor/ShowGutterHints', True);
    fUndoAfterSave :=
      XMLConfig.GetValue('EditorOptions/General/Editor/UndoAfterSave', True);
    fFindTextAtCursor :=
      XMLConfig.GetValue('EditorOptions/General/Editor/FindTextAtCursor', True);
    fUseSyntaxHighlight :=
      XMLConfig.GetValue(
      'EditorOptions/General/Editor/UseSyntaxHighlight', True);
    fBlockIndent :=
      XMLConfig.GetValue('EditorOptions/General/Editor/BlockIndent', 2);
    FBlockTabIndent :=
      XMLConfig.GetValue('EditorOptions/General/Editor/BlockTabIndent', 0);
    fBlockIndentType := GetSynBeautifierIndentType
      (XMLConfig.GetValue('EditorOptions/General/Editor/BlockIndentType',
                          'SpaceIndent'));
    FTrimSpaceType := GetTrimSpaceType
      (XMLConfig.GetValue('EditorOptions/General/Editor/SpaceTrimType',
                          'EditLine'));
    fUndoLimit :=
      XMLConfig.GetValue('EditorOptions/General/Editor/UndoLimit', 32767);
    fTabWidth :=
      XMLConfig.GetValue('EditorOptions/General/Editor/TabWidth', 8);
    FBracketHighlightStyle :=
      TSynEditBracketHighlightStyle(XMLConfig.GetValue('EditorOptions/General/Editor/BracketHighlightStyle', 2));

    // Display options
    fVisibleRightMargin :=
      XMLConfig.GetValue('EditorOptions/Display/VisibleRightMargin', True);
    fVisibleGutter :=
      XMLConfig.GetValue('EditorOptions/Display/VisibleGutter', True);
    if FileVersion<4 then begin
      fShowLineNumbers :=
        XMLConfig.GetValue('EditorOptions/Display/ShowLineNumbers', False);
      fShowOnlyLineNumbersMultiplesOf :=
        XMLConfig.GetValue('EditorOptions/Display/ShowOnlyLineNumbersMultiplesOf', 1);
    end else begin
      fShowLineNumbers :=
        XMLConfig.GetValue('EditorOptions/Display/ShowLineNumbers', True);
      fShowOnlyLineNumbersMultiplesOf :=
        XMLConfig.GetValue('EditorOptions/Display/ShowOnlyLineNumbersMultiplesOf', 5);
    end;
    fGutterWidth :=
      XMLConfig.GetValue('EditorOptions/Display/GutterWidth', 30);
    FGutterSeparatorIndex :=
      XMLConfig.GetValue('EditorOptions/Display/GutterSeparatorIndex', 3);
    fRightMargin :=
      XMLConfig.GetValue('EditorOptions/Display/RightMargin', 80);
    fEditorFont  :=
      XMLConfig.GetValue('EditorOptions/Display/EditorFont', SynDefaultFontName);
    if FileVersion < 8 then begin
      fEditorFontSize :=
        XMLConfig.GetValue('EditorOptions/Display/EditorFontHeight',
        SynDefaultFontHeight);
      fEditorFontSize := FontHeightToSize(fEditorFontSize);
    end else begin
      fEditorFontSize :=
        XMLConfig.GetValue('EditorOptions/Display/EditorFontSize',
        SynDefaultFontSize);
    end;
    RepairEditorFontSize(fEditorFontSize);
    fExtraCharSpacing :=
      XMLConfig.GetValue('EditorOptions/Display/ExtraCharSpacing', 0);
    fExtraLineSpacing :=
      XMLConfig.GetValue('EditorOptions/Display/ExtraLineSpacing', 1);
    fDisableAntialiasing :=
      XMLConfig.GetValue('EditorOptions/Display/DisableAntialiasing',
                         FileVersion<7);
    FDoNotWarnForFont :=
      XMLConfig.GetValue('EditorOptions/Display/DoNotWarnForFont', '');

    // Key Mappings options
    fKeyMappingScheme :=
      XMLConfig.GetValue('EditorOptions/KeyMapping/Scheme',
      StrToValidXMLName(KeyMapSchemeNames[kmsLazarus]));
    fKeyMap.LoadFromXMLConfig(XMLConfig
      , 'EditorOptions/KeyMapping/' + fKeyMappingScheme + '/');

    // Color options
    for i := 0 to HighlighterList.Count - 1 do
      HighlighterList[i].FileExtensions :=
        XMLConfig.GetValue('EditorOptions/Color/Lang' +
        StrToValidXMLName(HighlighterList[i].SynClass.GetLanguageName) +
        '/FileExtensions/Value', HighlighterList[i].DefaultFileExtensions)
      // color attributes are stored in the highlighters
    ;

    FUserDefinedColors.LoadFromXMLConfig(xmlconfig, 'EditorOptions/UserDefinedColors');

    FMarkupCurWordTime :=
      XMLConfig.GetValue(
      'EditorOptions/Display/MarkupCurrentWord/Time', 1500);
    FMarkupCurWordFullLen :=
      XMLConfig.GetValue(
      'EditorOptions/Display/MarkupCurrentWord/FullLen', 3);
    // check deprecated value
    if not XMLConfig.GetValue('EditorOptions/Display/MarkupCurrentWord/FullWord', True) then
      FMarkupCurWordFullLen := 0;
    XMLConfig.DeleteValue('EditorOptions/Display/MarkupCurrentWord/FullWord');
    FMarkupCurWordNoKeyword :=
      XMLConfig.GetValue(
      'EditorOptions/Display/MarkupCurrentWord/NoKeyword', True);
    FMarkupCurWordTrim :=
      XMLConfig.GetValue(
      'EditorOptions/Display/MarkupCurrentWord/Trim', True);
    FMarkupCurWordNoTimer :=
      XMLConfig.GetValue(
      'EditorOptions/Display/MarkupCurrentWord/NoTimer', False);
    FShowFileNameInCaption :=
      XMLConfig.GetValue(
      'EditorOptions/Display/ShowFileNameInCaption', False);

    // Code Tools options
    fAutoBlockCompletion :=
      XMLConfig.GetValue(
      'EditorOptions/CodeTools/AutoBlockCompletion', True);
    fAutoDisplayFuncPrototypes :=
      XMLConfig.GetValue(
      'EditorOptions/CodeTools/AutoDisplayFuncPrototypes', True);
    fAutoCodeParameters :=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoCodeParameters', True);
    fAutoToolTipExprEval :=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoToolTipExprEval', True);
    fAutoToolTipSymbTools :=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoToolTipSymbTools', True);
    fAutoDelayInMSec    :=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoDelayInMSec', 1000);
    fCodeTemplateFileNameRaw :=
      XMLConfig.GetValue('EditorOptions/CodeTools/CodeTemplateFileName'
      , TrimFilename(AppendPathDelim(GetPrimaryConfigPath) + DefaultCodeTemplatesFilename));
    fCTemplIndentToTokenStart :=
      XMLConfig.GetValue(
      'EditorOptions/CodeTools/CodeTemplateIndentToTokenStart/Value', False);
    fAutoRemoveEmptyMethods :=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoRemoveEmptyMethods', False);
    FCompletionLongLineHintInMSec :=
      XMLConfig.GetValue('EditorOptions/CodeTools/CompletionLongLineHintInMSec', 0);
    FCompletionLongLineHintType := DefaultCompletionLongLineHintType;
    XMLConfig.ReadObject('EditorOptions/CodeTools/CompletionLongLineHintType',
                         Self, Self, 'CompletionLongLineHintType');

    // Code Folding
    FUseCodeFolding :=
      XMLConfig.GetValue(
      'EditorOptions/CodeFolding/UseCodeFolding', True);
    FUseMarkupWordBracket :=
      XMLConfig.GetValue(
      'EditorOptions/CodeFolding/UseMarkupWordBracket', True);
    FUseMarkupOutline :=
      XMLConfig.GetValue(
      'EditorOptions/CodeFolding/UseMarkupOutline', False);

    FUserMouseSettings.LoadFromXml(XMLConfig, 'EditorOptions/Mouse/',
                                  'EditorOptions/General/Editor/', FileVersion);

    FMultiWinEditAccessOrder.LoadFromXMLConfig(XMLConfig, 'EditorOptions/MultiWin/');
    UserColorSchemeGroup.LoadFromXml(XMLConfig, 'EditorOptions/Color/',
      ColorSchemeFactory, 'EditorOptions/Display/');

  except
    on E: Exception do
      DebugLn('[TEditorOptions.Load] ERROR: ', e.Message);
  end;
end;

procedure TEditorOptions.Save;
// save options to XML file
var
  SynEditOpt: TSynEditorOption;
  SynEditOptName: String;
  i: Integer;
  SynEditOpt2: TSynEditorOption2;

begin
  try
    XMLConfig.SetValue('EditorOptions/Version', EditorOptsFormatVersion);

    XMLConfig.WriteObject('EditorOptions/Misc/', Self, FDefaultValues);

    // general options
    for SynEditOpt := Low(TSynEditorOption) to High(TSynEditorOption) do
    begin
      SynEditOptName := GetSynEditOptionName(SynEditOpt);
      if SynEditOptName <> '' then
        XMLConfig.SetDeleteValue('EditorOptions/General/Editor/' + SynEditOptName,
          SynEditOpt in fSynEditOptions, SynEditOpt in SynEditDefaultOptions);
    end;
    // general options
    for SynEditOpt2 := Low(TSynEditorOption2) to High(TSynEditorOption2) do
    begin
      case SynEditOpt2 of
        eoCaretSkipsSelection:
          SynEditOptName := 'CaretSkipsSelection';
        eoCaretSkipTab:
          SynEditOptName := 'CaretSkipTab';
        eoAlwaysVisibleCaret:
          SynEditOptName := 'AlwaysVisibleCaret';
        eoEnhanceEndKey:
          SynEditOptName := 'EnhanceEndKey';
        eoFoldedCopyPaste:
          SynEditOptName := 'FoldedCopyPaste';
        eoPersistentBlock:
          SynEditOptName := 'PersistentBlock';
        eoOverwriteBlock:
          SynEditOptName := 'OverwriteBlock';
        eoAutoHideCursor:
          SynEditOptName := 'AutoHideCursor';
        eoCaretMoveEndsSelection, eoPersistentCaretStopBlink:
          WriteStr(SynEditOptName, SynEditOpt2);
        else
          SynEditOptName := '';
      end;
      if SynEditOptName <> '' then
        XMLConfig.SetDeleteValue('EditorOptions/General/Editor/' + SynEditOptName,
          SynEditOpt2 in fSynEditOptions2, SynEditOpt2 in SynEditDefaultOptions2);
    end;

    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/ShowTabCloseButtons'
      , fShowTabCloseButtons, True);
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/HideSingleTabInWindow'
      , FHideSingleTabInWindow, False);
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/ShowTabNumbers'
      , fShowTabNumbers, False);
    XMLConfig.SetDeleteValue(
      'EditorOptions/General/Editor/CopyWordAtCursorOnCopyNone',
      FCopyWordAtCursorOnCopyNone, True);
    XMLConfig.SetDeleteValue(
      'EditorOptions/General/Editor/ShowGutterHints',
      FShowGutterHints, True);
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/UndoAfterSave'
      , fUndoAfterSave, True);
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/FindTextAtCursor'
      , fFindTextAtCursor, True);
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/UseSyntaxHighlight'
      , fUseSyntaxHighlight, True);
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/BlockIndent'
      , fBlockIndent, 2);
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/BlockTabIndent'
      , FBlockTabIndent, 0);
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/BlockIndentType'
      , GetSynBeautifierIndentName(fBlockIndentType), 'SpaceIndent');
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/SpaceTrimType'
      , GetTrimSpaceName(FTrimSpaceType), 'EditLine');
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/UndoLimit'
      , fUndoLimit, 32767);
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/TabWidth'
      , fTabWidth, 8);
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/BracketHighlightStyle'
      , Ord(FBracketHighlightStyle), 2);

    // Display options
    XMLConfig.SetDeleteValue('EditorOptions/Display/VisibleRightMargin'
      , fVisibleRightMargin, True);
    XMLConfig.SetDeleteValue('EditorOptions/Display/VisibleGutter',
      fVisibleGutter, True);
    XMLConfig.SetDeleteValue('EditorOptions/Display/ShowLineNumbers',
      fShowLineNumbers, True);
    XMLConfig.SetDeleteValue('EditorOptions/Display/ShowOnlyLineNumbersMultiplesOf',
      fShowOnlyLineNumbersMultiplesOf, 5);
    XMLConfig.SetDeleteValue('EditorOptions/Display/GutterWidth',
      fGutterWidth, 30);
    XMLConfig.SetDeleteValue('EditorOptions/Display/GutterSeparatorIndex',
      fGutterSeparatorIndex, 3);
    XMLConfig.SetDeleteValue('EditorOptions/Display/RightMargin',
      fRightMargin, 80);
    XMLConfig.SetDeleteValue('EditorOptions/Display/EditorFont',
      fEditorFont, SynDefaultFontName);
    XMLConfig.DeleteValue('EditorOptions/Display/EditorFontHeight'); // unused old value
    XMLConfig.SetDeleteValue('EditorOptions/Display/EditorFontSize'
      ,fEditorFontSize, SynDefaultFontSize);
    XMLConfig.SetDeleteValue('EditorOptions/Display/ExtraCharSpacing'
      ,fExtraCharSpacing, 0);
    XMLConfig.SetDeleteValue('EditorOptions/Display/ExtraLineSpacing'
      ,fExtraLineSpacing, 1);
    XMLConfig.SetDeleteValue('EditorOptions/Display/DisableAntialiasing'
      ,fDisableAntialiasing, DefaultEditorDisableAntiAliasing);
    XMLConfig.SetDeleteValue('EditorOptions/Display/DoNotWarnForFont'
      ,FDoNotWarnForFont, '');

    // Key Mappings options
    XMLConfig.SetDeleteValue('EditorOptions/KeyMapping/Scheme', fKeyMappingScheme,
       KeyMapSchemeNames[kmsLazarus]);
    fKeyMap.SaveToXMLConfig(
              XMLConfig, 'EditorOptions/KeyMapping/' + fKeyMappingScheme + '/');

    // Color options
    for i := 0 to HighlighterList.Count - 1 do
      XMLConfig.SetDeleteValue('EditorOptions/Color/Lang' +
        StrToValidXMLName(HighlighterList[i].SynClass.GetLanguageName) +
        '/FileExtensions/Value', HighlighterList[i].FileExtensions,
        HighlighterList[i].DefaultFileExtensions)
      // color attributes are stored in the highlighters
    ;

    FUserDefinedColors.SaveToXMLConfig(xmlconfig, 'EditorOptions/UserDefinedColors');

    XMLConfig.SetDeleteValue('EditorOptions/Display/MarkupCurrentWord/Time',
      FMarkupCurWordTime, 1500);
    XMLConfig.SetDeleteValue('EditorOptions/Display/MarkupCurrentWord/FullLen',
      FMarkupCurWordFullLen, 3);
    XMLConfig.SetDeleteValue('EditorOptions/Display/MarkupCurrentWord/NoKeyword',
      FMarkupCurWordNoKeyword, True);
    XMLConfig.SetDeleteValue('EditorOptions/Display/MarkupCurrentWord/Trim',
      FMarkupCurWordTrim, True);
    XMLConfig.SetDeleteValue('EditorOptions/Display/MarkupCurrentWord/NoTimer',
      FMarkupCurWordNoTimer, False);
    XMLConfig.SetDeleteValue('EditorOptions/Display/ShowFileNameInCaption',
        FShowFileNameInCaption, False);

    // Code Tools options
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/AutoBlockCompletion'
      , fAutoBlockCompletion, True);
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/AutoDisplayFuncPrototypes'
      , fAutoDisplayFuncPrototypes, True);
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/AutoCodeParameters'
      , fAutoCodeParameters, True);
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/AutoToolTipExprEval'
      , fAutoToolTipExprEval, True);
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/AutoToolTipSymbTools'
      , fAutoToolTipSymbTools, True);
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/AutoDelayInMSec'
      , fAutoDelayInMSec, 1000);
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/CodeTemplateFileName'
      , fCodeTemplateFileNameRaw, '');
    XMLConfig.SetDeleteValue(
      'EditorOptions/CodeTools/CodeTemplateIndentToTokenStart/Value'
      , fCTemplIndentToTokenStart, False);
    XMLConfig.SetDeleteValue(
      'EditorOptions/CodeTools/AutoRemoveEmptyMethods'
      , fAutoRemoveEmptyMethods, False);
    XMLConfig.SetDeleteValue(
      'EditorOptions/CodeTools/CompletionLongLineHintInMSec',
      FCompletionLongLineHintInMSec, 0);
    XMLConfig.WriteObject('EditorOptions/CodeTools/CompletionLongLineHintType',
                         Self, nil, 'CompletionLongLineHintType');

    // Code Folding
    XMLConfig.SetDeleteValue('EditorOptions/CodeFolding/UseCodeFolding',
        FUseCodeFolding, True);
    XMLConfig.SetDeleteValue('EditorOptions/CodeFolding/UseMarkupWordBracket',
        FUseMarkupWordBracket, True);
    XMLConfig.SetDeleteValue('EditorOptions/CodeFolding/UseMarkupOutline',
        FUseMarkupOutline, False);

    FUserMouseSettings.SaveToXml(XMLConfig, 'EditorOptions/Mouse/');

    FMultiWinEditAccessOrder.SaveToXMLConfig(XMLConfig, 'EditorOptions/MultiWin/');
    UserColorSchemeGroup.SaveToXml(XMLConfig, 'EditorOptions/Color/', ColorSchemeFactory);

    InvalidateFileStateCache;
    XMLConfig.Flush;
  except
    on E: Exception do
      DebugLn('[TEditorOptions.Save] ERROR: ', e.Message);
  end;
end;

procedure TEditorOptions.TranslateResourceStrings;
begin

end;

function TEditorOptions.GetAdditionalAttributeName(aha:TAdditionalHilightAttribute): string;
begin
  Result:=GetEnumName(TypeInfo(TAdditionalHilightAttribute), ord(aha));
end;

class function TEditorOptions.GetGroupCaption: string;
begin
  Result := dlgGroupEditor;
end;

class function TEditorOptions.GetInstance: TAbstractIDEOptions;
begin
  Result := EditorOpts;
end;

procedure TEditorOptions.DoAfterWrite(Restore: boolean);
begin
  if not Restore then
    Save;
  inherited;
end;

function TEditorOptions.GetSynEditOptionName(SynOption: TSynEditorOption): string;
begin
  case SynOption of
    eoAutoIndent:
      Result := 'AutoIndent';
    eoBracketHighlight:
      Result := 'BracketHighlight';
    eoEnhanceHomeKey:
      Result := 'EnhanceHomeKey';
    eoGroupUndo:
      Result := 'GroupUndo';
    eoHalfPageScroll:
      Result := 'HalfPageScroll';
    eoKeepCaretX:
      Result := 'KeepCaretX';
    eoPersistentCaret:
      Result := 'PersistentCaret';
    eoScrollByOneLess:
      Result := 'ScrollByOneLess';
    eoScrollPastEof:
      Result := 'ScrollPastEof';
    eoScrollPastEol:
      Result := 'ScrollPastEol';
    eoShowScrollHint:
      Result := 'ShowScrollHint';
    eoShowSpecialChars:
      Result := 'ShowSpecialChars';
    eoSmartTabs:
      Result := 'SmartTabs';
    eoTabsToSpaces:
      Result := 'TabsToSpaces';
    eoTabIndent:
      Result := 'TabIndent';
    eoTrimTrailingSpaces:
      Result := 'TrimTrailingSpaces';
    else
      Result := '';
  end;
end;

function TEditorOptions.GetSynBeautifierIndentName(IndentType: TSynBeautifierIndentType): string;
begin
  case IndentType of
    sbitSpace:
      Result := 'SpaceIndent';
    sbitCopySpaceTab:
      Result := 'CopySpaceTabIndent';
    sbitPositionCaret:
      Result := 'PositionIndent';
    else
      WriteStr(Result, IndentType);
  end;
end;

function TEditorOptions.GetSynBeautifierIndentType(IndentName: String): TSynBeautifierIndentType;
begin
  Result := sbitSpace;
  if IndentName = 'CopySpaceTabIndent' then
    Result := sbitCopySpaceTab
  else
  if IndentName = 'PositionIndent' then
    Result := sbitPositionCaret
  else
  if IndentName = 'sbitConvertToTabSpace' then
    Result := sbitConvertToTabSpace
  else
  if IndentName = 'sbitConvertToTabOnly' then
    Result := sbitConvertToTabOnly;
end;

function TEditorOptions.GetTrimSpaceName(IndentType: TSynEditStringTrimmingType): string;
begin
  Result := '';
  case IndentType of
    settLeaveLine:
      Result := 'LeaveLine';
    settEditLine:
      Result := 'EditLine';
    settMoveCaret:
      Result := 'MoveCaret';
    settIgnoreAll:
      Result := 'PosOnly';
  end;
end;

function TEditorOptions.GetTrimSpaceType(IndentName: String): TSynEditStringTrimmingType;
begin
  Result := settLeaveLine;
  if IndentName = 'EditLine' then
    Result := settEditLine
  else if IndentName = 'MoveCaret' then
    Result := settMoveCaret
  else if IndentName = 'PosOnly' then
    Result := settIgnoreAll;
end;

procedure TEditorOptions.AssignKeyMapTo(ASynEdit: TSynEdit; SimilarEdit: TSynEdit);
var
  c, i: Integer;
begin
  if SimilarEdit<>nil then
    ASynEdit.KeyStrokes.Assign(SimilarEdit.Keystrokes)
  else
    KeyMap.AssignTo(ASynEdit.KeyStrokes, TSourceEditorWindowInterface);

  c := ASynEdit.PluginCount - 1;
  while (c >= 0) do begin

    if SimilarEdit<>nil then begin
      i := SimilarEdit.PluginCount - 1;
      while (i >= 0) and not (SimilarEdit.Plugin[i].ClassType = ASynEdit.Plugin[c].ClassType) do
        dec(i);
    end
    else
      i:= -1;

    if (ASynEdit.Plugin[c] is TSynPluginTemplateEdit) then begin
      TSynPluginTemplateEdit(ASynEdit.Plugin[c]).Keystrokes.Clear;
      TSynPluginTemplateEdit(ASynEdit.Plugin[c]).KeystrokesOffCell.Clear;
      if i >= 0 then begin
        TSynPluginTemplateEdit(ASynEdit.Plugin[c]).Keystrokes.Assign(
                               TSynPluginTemplateEdit(SimilarEdit.Plugin[i]).KeyStrokes);
        TSynPluginTemplateEdit(ASynEdit.Plugin[c]).KeystrokesOffCell.Assign(
                               TSynPluginTemplateEdit(SimilarEdit.Plugin[i]).KeystrokesOffCell);
      end else begin
        KeyMap.AssignTo(TSynPluginTemplateEdit(ASynEdit.Plugin[c]).Keystrokes,
                        TLazSynPluginTemplateEditForm, ecIdePTmplOffset);
        KeyMap.AssignTo(TSynPluginTemplateEdit(ASynEdit.Plugin[c]).KeystrokesOffCell,
                        TLazSynPluginTemplateEditFormOff, ecIdePTmplOutOffset);
      end;
    end;

    if (ASynEdit.Plugin[c] is TSynPluginSyncroEdit) then begin
      TSynPluginSyncroEdit(ASynEdit.Plugin[c]).KeystrokesSelecting.Clear;
      TSynPluginSyncroEdit(ASynEdit.Plugin[c]).Keystrokes.Clear;
      TSynPluginSyncroEdit(ASynEdit.Plugin[c]).KeystrokesOffCell.Clear;
      if i >= 0 then begin
        TSynPluginSyncroEdit(ASynEdit.Plugin[c]).KeystrokesSelecting.Assign(
                             TSynPluginSyncroEdit(SimilarEdit.Plugin[i]).KeystrokesSelecting);
        TSynPluginSyncroEdit(ASynEdit.Plugin[c]).Keystrokes.Assign(
                             TSynPluginSyncroEdit(SimilarEdit.Plugin[i]).KeyStrokes);
        TSynPluginSyncroEdit(ASynEdit.Plugin[c]).KeystrokesOffCell.Assign(
                             TSynPluginSyncroEdit(SimilarEdit.Plugin[i]).KeystrokesOffCell);
      end else begin
        KeyMap.AssignTo(TSynPluginSyncroEdit(ASynEdit.Plugin[c]).KeystrokesSelecting,
                        TLazSynPluginSyncroEditFormSel, ecIdePSyncroSelOffset);
        KeyMap.AssignTo(TSynPluginSyncroEdit(ASynEdit.Plugin[c]).Keystrokes,
                        TLazSynPluginSyncroEditForm, ecIdePSyncroOffset);
        KeyMap.AssignTo(TSynPluginSyncroEdit(ASynEdit.Plugin[c]).KeystrokesOffCell,
                        TLazSynPluginSyncroEditFormOff, ecIdePSyncroOutOffset);
      end;
    end;

    if (ASynEdit.Plugin[c] is TSynPluginMultiCaret) then begin
      // Only ecPluginMultiCaretClearAll
      // the others are handled in SynEdit.Keystrokes
      TSynPluginMultiCaret(ASynEdit.Plugin[c]).Keystrokes.Clear;
      if i >= 0 then begin
        TSynPluginMultiCaret(ASynEdit.Plugin[c]).Keystrokes.Assign(
                               TSynPluginMultiCaret(SimilarEdit.Plugin[i]).KeyStrokes);
      end else begin
        KeyMap.AssignTo(TSynPluginMultiCaret(ASynEdit.Plugin[c]).Keystrokes,
                        TLazSynPluginTemplateMultiCaret, 0); //ecIdePTmplOffset);
      end;
    end;

    dec(c);
  end;
end;

function TEditorOptions.CreateSyn(LazSynHilighter: TLazSyntaxHighlighter): TSrcIDEHighlighter;
begin
  if LazSyntaxHighlighterClasses[LazSynHilighter] <> Nil then
  begin
    Result := LazSyntaxHighlighterClasses[LazSynHilighter].Create(Nil);
    GetHighlighterSettings(Result);
  end
  else
    Result := Nil;
end;

function TEditorOptions.ReadColorScheme(const LanguageName: String): String;
(* The name of the currently chosen color-scheme for that language *)
begin
  if LanguageName = '' then
  begin
    Result := ColorSchemeFactory.ColorSchemeGroupAtPos[0].Name;
    exit;
  end;
  if LanguageName <> TPreviewPasSyn.GetLanguageName then
    Result := XMLConfig.GetValue(
      'EditorOptions/Color/Lang' + StrToValidXMLName(LanguageName) +
      '/ColorScheme/Value', '')
  else
    Result := '';
  if ColorSchemeFactory.ColorSchemeGroup[Result] = nil then
    Result := '';
  if Result = '' then
    Result := ReadPascalColorScheme;
end;

function TEditorOptions.ReadPascalColorScheme: String;
(* The name of the currently chosen color-scheme for pascal code *)
var
  FormatVersion: Integer;
begin
  FormatVersion := XMLConfig.GetValue('EditorOptions/Color/Version', EditorOptsFormatVersion);
  if FormatVersion > 1 then
    Result := XMLConfig.GetValue(
      'EditorOptions/Color/Lang' + StrToValidXMLName(
      TPreviewPasSyn.GetLanguageName) + '/ColorScheme/Value', '')
  else
    Result := XMLConfig.GetValue('EditorOptions/Color/ColorScheme', '');
  if ColorSchemeFactory.ColorSchemeGroup[Result] = nil then
    Result := '';
  if (Result = '') then begin
    if DefaultColorSchemeName <> '' then
      Result := DefaultColorSchemeName
    else
      Result := ColorSchemeFactory.ColorSchemeGroupAtPos[0].Name;
  end;
end;

procedure TEditorOptions.WriteColorScheme(const LanguageName, SynColorScheme: String);
begin
  if (LanguageName = '') or (SynColorScheme = '') then
    exit;
  XMLConfig.SetValue('EditorOptions/Color/Lang' + StrToValidXMLName(
    LanguageName) + '/ColorScheme/Value', SynColorScheme);
  XMLConfig.SetValue('EditorOptions/Color/Version', EditorOptsFormatVersion);
end;


procedure TEditorOptions.ReadHighlighterSettings(Syn: TSrcIDEHighlighter;
  SynColorScheme: String);
// if SynColorScheme='' then default ColorScheme will be used
var
  Scheme: TColorScheme;
  LangScheme: TColorSchemeLanguage;
begin
  // initialize with defaults
  if SynColorScheme = '' then
    SynColorScheme := ReadColorScheme(Syn.LanguageName);
  //DebugLn(['TEditorOptions.ReadHighlighterSettings ',SynColorScheme,' Syn.ClassName=',Syn.ClassName]);
  if (SynColorScheme = '') or (Syn.LanguageName = '') then
    exit;

  Scheme := UserColorSchemeGroup.ColorSchemeGroup[SynColorScheme];
  if Scheme = nil then
    exit;
  LangScheme := Scheme.ColorSchemeBySynClass[Syn.ClassType];
  if LangScheme = nil then
    exit;

  LangScheme.ApplyTo(Syn);
end;

procedure TEditorOptions.ReadHighlighterFoldSettings(Syn: TSrcIDEHighlighter;
  ReadForOptions: Boolean);
var
  ConfName: String;
  Path: String;
  i, h, idx: Integer;
  TheFoldInfo: TEditorOptionsFoldRecord;
  DefHl, FoldHl: TSynCustomFoldHighlighter;
begin
  h := HighlighterList.FindByHighlighter(Syn);
  if h < 0 then
    h := HighlighterList.FindByName(Syn.LanguageName);
  if h < 0 then exit;

  if (syn is TSynCustomFoldHighlighter) then begin
    DefHl := TSynCustomFoldHighlighter(TCustomSynClass(Syn.ClassType).Create(nil));
    try
      ReadDefaultsForHighlighterFoldSettings(DefHl);
      FoldHl := TSynCustomFoldHighlighter(Syn);
      TheFoldInfo := EditorOptionsFoldDefaults[HighlighterList[h].TheType];
      for i := 0 to TheFoldInfo.Count - 1 do begin
        idx := TheFoldInfo.Info^[i].Index;
        ConfName := TheFoldInfo.Info^[i].Xml;
        Path := 'EditorOptions/FoldConfig/Lang' +
          StrToValidXMLName(Syn.LanguageName) + '/Type' + ConfName + '/' ;
      // try reading the old config first
      FoldHl.FoldConfig[idx].Enabled :=
        XMLConfig.GetValue(Path + 'Enabled/Value', FoldHl.FoldConfig[idx].Enabled);
      XMLConfig.ReadObject(Path + 'Settings/', FoldHl.FoldConfig[idx], DefHl.FoldConfig[idx]);

        (* if ReadForOptions=True then Enabled appies only to fmFold,fmHide.
           This allows to store what selection was previously active *)
        if not ReadForOptions then begin
          if (not FoldHl.FoldConfig[idx].Enabled) or (not FUseCodeFolding) then
            FoldHl.FoldConfig[idx].Modes := FoldHl.FoldConfig[idx].Modes - [fmFold, fmHide];
          if (not FUseMarkupWordBracket) then
            FoldHl.FoldConfig[idx].Modes := FoldHl.FoldConfig[idx].Modes - [fmMarkup];
          if (not FUseMarkupOutline) then
            FoldHl.FoldConfig[idx].Modes := FoldHl.FoldConfig[idx].Modes - [fmOutline];

          FoldHl.FoldConfig[idx].Enabled := FoldHl.FoldConfig[idx].Modes <> [];
        end;

        if (FoldHl is TSynPasSyn) and (idx = ord(cfbtIfThen)) then begin
          FoldHl.FoldConfig[ord(cfbtIfElse)].Modes := FoldHl.FoldConfig[idx].Modes * [fmOutline];
          FoldHl.FoldConfig[ord(cfbtIfElse)].Enabled := FoldHl.FoldConfig[idx].Enabled and (FoldHl.FoldConfig[ord(cfbtIfElse)].Modes <> []);
        end;

      end;
    finally
      DefHl.Free;
    end;
  end;
end;

procedure TEditorOptions.ReadDefaultsForHighlighterFoldSettings(Syn: TSrcIDEHighlighter);
var
  i, h: Integer;
  TheFoldInfo: TEditorOptionsFoldRecord;
begin
  h := HighlighterList.FindByHighlighter(Syn);
  if h < 0 then
    h := HighlighterList.FindByName(Syn.LanguageName);
  if h < 0 then exit;
  if (syn is TSynCustomFoldHighlighter) then begin
    TheFoldInfo := EditorOptionsFoldDefaults[HighlighterList[h].TheType];
    for i := 0 to TheFoldInfo.Count - 1 do
      with TSynCustomFoldHighlighter(Syn).FoldConfig[TheFoldInfo.Info^[i].Index] do begin
        Enabled := TheFoldInfo.Info^[i].Enabled;
      end;
  end;
end;

procedure TEditorOptions.WriteHighlighterFoldSettings(Syn: TSrcIDEHighlighter);
var
  DefSyn: TSrcIDEHighlighter;
  i, h:   Integer;
  Path:   String;
  ConfName: String;
  TheFoldInfo: TEditorOptionsFoldRecord;
begin
  h := HighlighterList.FindByHighlighter(Syn);
  if h < 0 then
    h := HighlighterList.FindByName(Syn.LanguageName);
  if h < 0 then exit;

  DefSyn := TCustomSynClass(Syn.ClassType).Create(Nil);
  try
    ReadDefaultsForHighlighterFoldSettings(DefSyn);

    if (syn is TSynCustomFoldHighlighter) then begin
      TheFoldInfo := EditorOptionsFoldDefaults[HighlighterList[h].TheType];
      for i := 0 to TheFoldInfo.Count - 1 do begin
        ConfName := TheFoldInfo.Info^[i].Xml;
        Path := 'EditorOptions/FoldConfig/Lang' +
          StrToValidXMLName(Syn.LanguageName) + '/Type' + ConfName + '/' ;
        XMLConfig.DeletePath(Path + 'Enabled/');
        XMLConfig.WriteObject(Path + 'Settings/',
          TSynCustomFoldHighlighter(Syn).FoldConfig[TheFoldInfo.Info^[i].Index],
          TSynCustomFoldHighlighter(DefSyn).FoldConfig[TheFoldInfo.Info^[i].Index]);
      end;
    end;

  finally
    DefSyn.Free;
  end;
end;

procedure TEditorOptions.ReadHighlighterDivDrawSettings(Syn: TSrcIDEHighlighter);
var
  TheInfo: TEditorOptionsDividerRecord;
  Conf: TSynDividerDrawConfig;
  ConfName: String;
  Path: String;
  i, h: Integer;
begin
  h := HighlighterList.FindByHighlighter(Syn);
  if h < 0 then
    h := HighlighterList.FindByName(Syn.LanguageName);
  if h < 0 then exit;
  TheInfo := EditorOptionsDividerDefaults[HighlighterList[h].TheType];

  ReadDefaultsForHighlighterDivDrawSettings(Syn);

  // read settings, that are different from the defaults
  for i := 0 to TheInfo.Count - 1 do begin
    Conf := Syn.DividerDrawConfig[i];
    ConfName := TheInfo.Info^[i].Xml;
    Path := 'EditorOptions/DividerDraw/Lang' + StrToValidXMLName(Syn.LanguageName) +
      '/Type' + ConfName + '/' ;
    Conf.MaxDrawDepth := XMLConfig.GetValue(Path + 'MaxDepth/Value',
        Conf.MaxDrawDepth);
    Conf.TopColor := XMLConfig.GetValue(Path + 'TopColor/Value',
        Conf.TopColor);
    Conf.NestColor := XMLConfig.GetValue(Path + 'NestColor/Value',
        Conf.NestColor);
  end;
end;

procedure TEditorOptions.ReadDefaultsForHighlighterDivDrawSettings(Syn: TSrcIDEHighlighter);
var
  TheInfo: TEditorOptionsDividerRecord;
  i, h: Integer;
begin
  h := HighlighterList.FindByHighlighter(Syn);
  if h < 0 then
    h := HighlighterList.FindByName(Syn.LanguageName);
  if h < 0 then exit;
  TheInfo := EditorOptionsDividerDefaults[HighlighterList[h].TheType];
  for i := 0 to TheInfo.Count - 1 do begin
    Syn.DividerDrawConfig[i].MaxDrawDepth := TheInfo.Info^[i].MaxLeveL;
    Syn.DividerDrawConfig[i].TopColor := clDefault;
    Syn.DividerDrawConfig[i].NestColor := clDefault;
  end;
end;

procedure TEditorOptions.WriteHighlighterDivDrawSettings(Syn: TSrcIDEHighlighter);
var
  DefSyn: TSrcIDEHighlighter;
  i, h:   Integer;
  Path:   String;
  Conf, DefConf: TSynDividerDrawConfig;
  TheInfo: TEditorOptionsDividerRecord;
  ConfName: String;
begin
  h := HighlighterList.FindByHighlighter(Syn);
  if h < 0 then
    h := HighlighterList.FindByName(Syn.LanguageName);
  if h < 0 then exit;
  TheInfo := EditorOptionsDividerDefaults[HighlighterList[h].TheType];

  DefSyn := TCustomSynClass(Syn.ClassType).Create(Nil);
  try
    ReadDefaultsForHighlighterDivDrawSettings(DefSyn);
    for i := 0 to TheInfo.Count - 1 do begin
      Conf := Syn.DividerDrawConfig[i];
      DefConf := DefSyn.DividerDrawConfig[i]; // default value
      ConfName := TheInfo.Info^[i].Xml;
      Path := 'EditorOptions/DividerDraw/Lang' +
        StrToValidXMLName(Syn.LanguageName) + '/Type' + ConfName + '/' ;
      XMLConfig.SetDeleteValue(Path + 'MaxDepth/Value', Conf.MaxDrawDepth,
                               DefConf.MaxDrawDepth);
      XMLConfig.SetDeleteValue(Path + 'TopColor/Value', Conf.TopColor,
                               DefConf.TopColor);
      XMLConfig.SetDeleteValue(Path + 'NestColor/Value', Conf.NestColor,
                               DefConf.NestColor);
    end;

  finally
    DefSyn.Free;
  end;
end;

procedure TEditorOptions.GetHighlighterSettings(Syn: TSrcIDEHighlighter);
// read highlight settings from config file
begin
  ReadHighlighterSettings(Syn, '');
  ReadHighlighterFoldSettings(Syn);
  ReadHighlighterDivDrawSettings(Syn);
  if Syn is TSynPasSyn then begin
    TSynPasSyn(Syn).ExtendedKeywordsMode := PasExtendedKeywordsMode;
    TSynPasSyn(Syn).StringKeywordMode := PasStringKeywordMode;
  end;;
end;

procedure TEditorOptions.SetMarkupColors(aSynEd: TSynEdit);
var
  Scheme: TColorSchemeLanguage;
  SchemeGrp: TColorScheme;
  SynColorScheme: String;
begin
  // Find current color scheme for default colors
  if (aSynEd.Highlighter = nil) then begin
    aSynEd.Color := clWhite;
    aSynEd.Font.Color := clBlack;
    exit;
  end;

  // get current colorscheme:
  SynColorScheme := ReadColorScheme(aSynEd.Highlighter.LanguageName);
  SchemeGrp := UserColorSchemeGroup.ColorSchemeGroup[SynColorScheme];
  if SchemeGrp = nil then
    exit;
  Scheme := SchemeGrp.ColorSchemeBySynClass[aSynEd.Highlighter.ClassType];

  if Assigned(Scheme) then Scheme.ApplyTo(aSynEd);

end;

procedure TEditorOptions.SetMarkupColor(Syn : TSrcIDEHighlighter;
  AddHilightAttr : TAdditionalHilightAttribute; aMarkup : TSynSelectedColor);
var
  SynColorScheme: String;
  SchemeGrp: TColorScheme;
  Scheme: TColorSchemeLanguage;
  Attrib: TColorSchemeAttribute;
begin
  if assigned(Syn) then begin
    SynColorScheme := ReadColorScheme(Syn.LanguageName);
    SchemeGrp := UserColorSchemeGroup.ColorSchemeGroup[SynColorScheme];
    if SchemeGrp = nil then
      exit;
    Scheme := SchemeGrp.ColorSchemeBySynClass[Syn.ClassType];
  end else begin
    SchemeGrp := UserColorSchemeGroup.ColorSchemeGroup[DefaultColorSchemeName];
    if SchemeGrp = nil then
      exit;
    Scheme := SchemeGrp.DefaultColors;
  end;

  Attrib := Scheme.AttributeByEnum[AddHilightAttr];
  if Attrib <> nil then begin
    Attrib.ApplyTo(aMarkup);
    exit;
  end;

  // set default
  aMarkup.Foreground := clNone;
  aMarkup.Background := clNone;
  aMarkup.FrameColor := clNone;
  aMarkup.FrameEdges := sfeAround;
  aMarkup.FrameStyle := slsSolid;
  aMarkup.Style := [];
  aMarkup.StyleMask := [];
end;

procedure TEditorOptions.ApplyFontSettingsTo(ASynEdit: TSynEdit);
begin
  ASynEdit.Font.Size := fEditorFontSize;// set size before name for XLFD !
  ASynEdit.Font.Name := fEditorFont;
  if fDisableAntialiasing then
    ASynEdit.Font.Quality := fqNonAntialiased
  else
    ASynEdit.Font.Quality := fqDefault;
end;

function TEditorOptions.ExtensionToLazSyntaxHighlighter(Ext: String): TLazSyntaxHighlighter;
var
  s, CurExt: String;
  LangID, StartPos, EndPos: Integer;
begin
  Result := lshNone;
  if (Ext = '') or (Ext = '.') or (HighlighterList = Nil) then
    exit;
  Ext := lowercase(Ext);
  if (Ext[1] = '.') then
    Ext := copy(Ext, 2, length(Ext) - 1);
  LangID := 0;
  while LangID < HighlighterList.Count do
  begin
    s := HighlighterList[LangID].FileExtensions;
    StartPos := 1;
    while StartPos <= length(s) do
    begin
      Endpos := StartPos;
      while (EndPos <= length(s)) and (s[EndPos] <> ';') do
        inc(EndPos);
      CurExt := copy(s, Startpos, EndPos - StartPos);
      if (CurExt <> '') and (CurExt[1] = '.') then
        CurExt := copy(CurExt, 2, length(CurExt) - 1);
      if lowercase(CurExt) = Ext then
      begin
        Result := HighlighterList[LangID].TheType;
        exit;
      end;
      Startpos := EndPos + 1;
    end;
    inc(LangID);
  end;
end;

procedure TEditorOptions.GetSynEditSettings(ASynEdit: TSynEdit;
  SimilarEdit: TSynEdit);
// read synedit settings from config file
// if SimilarEdit is given it is used for speed up
var
  MarkCaret: TSynEditMarkupHighlightAllCaret;
  b: TSynBeautifierPascal;
  i: Integer;
  mw: TSourceSynEditMarkupHighlightAllMulti;
  TermsConf: TEditorUserDefinedWords;
  Markup: TSynEditMarkup;
begin
  // general options
  ASynEdit.BeginUpdate(False);
  try
    ASynEdit.Options := fSynEditOptions;
    ASynEdit.Options2 := fSynEditOptions2;
    ASynEdit.BlockIndent := fBlockIndent;
    ASynEdit.BlockTabIndent := FBlockTabIndent;
    (ASynEdit.Beautifier as TSynBeautifier).IndentType := fBlockIndentType;
    if ASynEdit.Beautifier is TSynBeautifierPascal then begin
      b := ASynEdit.Beautifier as TSynBeautifierPascal;

      if FAnsiCommentContinueEnabled then begin
        b.AnsiCommentMode := sccPrefixMatch;
        b.AnsiIndentMode := FAnsiIndentMode;
        b.AnsiMatch := FAnsiCommentMatch;
        b.AnsiPrefix := FAnsiCommentPrefix;
        b.AnsiMatchLine := sclMatchPrev;
        b.AnsiMatchMode := AnsiCommentMatchMode;
        b.AnsiCommentIndent := sbitCopySpaceTab;
        b.AnsiIndentFirstLineMax := AnsiIndentAlignMax;
      end
      else begin
        b.AnsiCommentMode := sccNoPrefix;
        b.AnsiIndentMode := [];
      end;

      if FCurlyCommentContinueEnabled then begin
        b.BorCommentMode := sccPrefixMatch;
        b.BorIndentMode := FCurlyIndentMode;
        b.BorMatch := FCurlyCommentMatch;
        b.BorPrefix := FCurlyCommentPrefix;
        b.BorMatchLine := sclMatchPrev;
        b.BorMatchMode := CurlyCommentMatchMode;
        b.BorCommentIndent := sbitCopySpaceTab;
        b.BorIndentFirstLineMax := CurlyIndentAlignMax;
      end
      else begin
        b.BorCommentMode := sccNoPrefix;
        b.BorIndentMode := [];
      end;

      if FSlashCommentContinueEnabled then begin
        b.SlashCommentMode := sccPrefixMatch;
        b.SlashIndentMode := FSlashIndentMode;
        b.SlashMatch := FSlashCommentMatch;
        b.SlashPrefix := FSlashCommentPrefix;
        b.SlashMatchLine := sclMatchPrev;
        b.SlashMatchMode := SlashCommentMatchMode;
        b.SlashCommentIndent := sbitCopySpaceTab;
        b.ExtendSlashCommentMode := FSlashCommentExtend;
        b.SlashIndentFirstLineMax := SlashIndentAlignMax;
      end
      else begin
        b.SlashCommentMode := sccNoPrefix;
        b.SlashIndentMode := [];
      end;

      b.StringBreakEnabled := FStringBreakEnabled;
      b.StringBreakAppend  := FStringBreakAppend;
      b.StringBreakPrefix  := FStringBreakPrefix;

    end;

    ASynEdit.TrimSpaceType := FTrimSpaceType;
    ASynEdit.TabWidth := fTabWidth;
    ASynEdit.BracketHighlightStyle := FBracketHighlightStyle;
    {$IFDEF WinIME}
    if ASynEdit is TIDESynEditor then begin
      if UseMinimumIme
      then TIDESynEditor(ASynEdit).CreateMinimumIme
      else TIDESynEditor(ASynEdit).CreateFullIme;
    end;
    {$ENDIF}

    if ASynEdit is TIDESynEditor then begin
      TIDESynEditor(ASynEdit).HighlightUserWordCount := UserDefinedColors.Count;
      for i := 0 to UserDefinedColors.Count - 1 do begin
        TermsConf := UserDefinedColors.Lists[i];
        mw := TIDESynEditor(ASynEdit).HighlightUserWords[i];
        if TermsConf.GlobalList or (not TermsConf.HasKeyAssigned)
        then begin
          if TermsConf.GlobalTermsCache = nil then
            TermsConf.GlobalTermsCache := mw.Terms
          else
            mw.Terms := TermsConf.GlobalTermsCache;
        end
        else begin
          if mw.Terms = TermsConf.GlobalTermsCache then
            mw.Terms := nil;
          if TermsConf.GlobalTermsCache <> nil then
            TermsConf.GlobalTermsCache.Clear;
        end;

        mw.MarkupInfo.Assign(TermsConf.ColorAttr);
        mw.Clear;
        mw.Terms.Assign(TermsConf);
        mw.RestoreLocalChanges;
        if TermsConf.AddTermCmd <> nil then
          mw.AddTermCmd := TermsConf.AddTermCmd.Command;
        if TermsConf.RemoveTermCmd <> nil then
          mw.RemoveTermCmd := TermsConf.RemoveTermCmd.Command;
        if TermsConf.ToggleTermCmd <> nil then
          mw.ToggleTermCmd := TermsConf.ToggleTermCmd.Command;
        mw.KeyAddTermBounds        := TermsConf.KeyAddTermBounds;
        mw.KeyAddCase              := TermsConf.KeyAddCase;
        mw.KeyAddWordBoundMaxLen   := TermsConf.KeyAddWordBoundMaxLen;
        mw.KeyAddSelectBoundMaxLen := TermsConf.KeyAddSelectBoundMaxLen;
        mw.KeyAddSelectSmart       := TermsConf.KeyAddSelectSmart;
      end;
    end;

    {$IFnDEF WithoutSynMultiCaret}
    if ASynEdit is TIDESynEditor then begin
      TIDESynEditor(ASynEdit).MultiCaret.EnableWithColumnSelection := MultiCaretOnColumnSelect;
      TIDESynEditor(ASynEdit).MultiCaret.DefaultMode := FMultiCaretDefaultMode;
      TIDESynEditor(ASynEdit).MultiCaret.DefaultColumnSelectMode := FMultiCaretDefaultColumnSelectMode;
      if FMultiCaretDeleteSkipLineBreak
      then TIDESynEditor(ASynEdit).MultiCaret.Options := TIDESynEditor(ASynEdit).MultiCaret.Options + [smcoDeleteSkipLineBreak]
      else TIDESynEditor(ASynEdit).MultiCaret.Options := TIDESynEditor(ASynEdit).MultiCaret.Options - [smcoDeleteSkipLineBreak];
    end;
    {$ENDIF}

    // Display options
    ASynEdit.Gutter.Visible := fVisibleGutter;
    ASynEdit.Gutter.AutoSize := true;
    ASynEdit.Gutter.LineNumberPart.Visible := fShowLineNumbers;
    ASynEdit.Gutter.LineNumberPart(0).ShowOnlyLineNumbersMultiplesOf :=
      fShowOnlyLineNumbersMultiplesOf;
    ASynEdit.RightGutter.Visible := ShowOverviewGutter;
    if ASynEdit is TIDESynEditor then
      TIDESynEditor(ASynEdit).ShowTopInfo := TopInfoView;

    ASynEdit.Gutter.CodeFoldPart.Visible := FUseCodeFolding;
    if not FUseCodeFolding then
      ASynEdit.UnfoldAll;
    ASynEdit.Gutter.CodeFoldPart.ReversePopMenuOrder := ReverseFoldPopUpOrder;

    ASynEdit.Gutter.Width := fGutterWidth;
    ASynEdit.Gutter.SeparatorPart.Visible := FGutterSeparatorIndex <> -1;
    if FGutterSeparatorIndex <> -1 then
    ASynEdit.Gutter.SeparatorPart(0).Index := FGutterSeparatorIndex;

    ASynEdit.RightEdge := fRightMargin;
    if fVisibleRightMargin then
      ASynEdit.Options := ASynEdit.Options - [eoHideRightMargin]
    else
      ASynEdit.Options := ASynEdit.Options + [eoHideRightMargin];

    ApplyFontSettingsTo(ASynEdit);
    //debugln(['TEditorOptions.GetSynEditSettings ',ASynEdit.font.height]);

    ASynEdit.ExtraCharSpacing := fExtraCharSpacing;
    ASynEdit.ExtraLineSpacing := fExtraLineSpacing;
    ASynEdit.MaxUndo := fUndoLimit;
    // The Highlighter on the SynEdit will have been initialized with the configured
    // values already (including all the additional-attributes.
    // Just copy the colors from the SynEdit's highlighter to the SynEdit's Markup and co
    SetMarkupColors(ASynEdit);

    MarkCaret := TSynEditMarkupHighlightAllCaret(ASynEdit.MarkupByClass[TSynEditMarkupHighlightAllCaret]);
    if assigned(MarkCaret) then begin
      if FMarkupCurWordNoTimer then
        MarkCaret.WaitTime := 0
      else
        MarkCaret.WaitTime := FMarkupCurWordTime;
      MarkCaret.FullWord := FMarkupCurWordFullLen > 0;
      MarkCaret.FullWordMaxLen := FMarkupCurWordFullLen;
      MarkCaret.IgnoreKeywords := FMarkupCurWordNoKeyword;
      MarkCaret.Trim := FMarkupCurWordTrim;
    end;

    Markup := ASynEdit.MarkupByClass[TSynEditMarkupFoldColors];
    if (Markup <> nil) then
      Markup.Enabled := FUseMarkupOutline;

    AssignKeyMapTo(ASynEdit, SimilarEdit);

    ASynEdit.MouseOptions := [emUseMouseActions];
    ASynEdit.MouseActions.Assign(FUserMouseSettings.MainActions);
    ASynEdit.MouseSelActions.Assign(FUserMouseSettings.SelActions);
    ASynEdit.MouseTextActions.Assign(FUserMouseSettings.TextActions);
    ASynEdit.Gutter.MouseActions.Assign(FUserMouseSettings.GutterActions);
    if ASynEdit.Gutter.CodeFoldPart <> nil then begin
      ASynEdit.Gutter.CodeFoldPart.MouseActions.Assign(FUserMouseSettings.GutterActionsFold);
      ASynEdit.Gutter.CodeFoldPart.MouseActionsCollapsed.Assign(FUserMouseSettings.GutterActionsFoldCol);
      ASynEdit.Gutter.CodeFoldPart.MouseActionsExpanded.Assign(FUserMouseSettings.GutterActionsFoldExp);
    end;
    if ASynEdit.Gutter.LineNumberPart <> nil then begin
      ASynEdit.Gutter.LineNumberPart.MouseActions.Assign(FUserMouseSettings.GutterActionsLines);
    end;
    if ASynEdit.Gutter.ChangesPart<> nil then
      ASynEdit.Gutter.ChangesPart.MouseActions.Assign(FUserMouseSettings.GutterActionsChanges);

    if (ASynEdit.Gutter.SeparatorPart <> nil) and (GutterSeparatorIndex = 2) and ShowLineNumbers then
      ASynEdit.Gutter.SeparatorPart.MouseActions.Assign(FUserMouseSettings.GutterActionsLines)
    else
    if (ASynEdit.Gutter.SeparatorPart <> nil) and (GutterSeparatorIndex >= 2) then
      ASynEdit.Gutter.SeparatorPart.MouseActions.Assign(FUserMouseSettings.GutterActionsChanges);
    if ASynEdit.RightGutter.LineOverviewPart <> nil then begin
      ASynEdit.RightGutter.LineOverviewPart.MouseActions.Assign(FUserMouseSettings.GutterActionsOverView);
      ASynEdit.RightGutter.LineOverviewPart.MouseActionsForMarks.Assign(FUserMouseSettings.GutterActionsOverViewMarks);
    end;
  finally
    ASynEdit.EndUpdate;
  end;
end;

function TEditorOptions.GetCodeTemplateFileNameExpand:String;
begin
  result:=fCodeTemplateFileNameRaw;
  IDEMacros.SubstituteMacros(result);
end;

function TEditorOptions.GetTabPosition: TTabPosition;
begin
  Result := fTabPosition;
end;

procedure TEditorOptions.GetSynEditPreviewSettings(APreviewEditor: TObject);
// read synedit setings from config file
var
  ASynEdit: TSynEdit;
begin
  if not (APreviewEditor is TSynEdit) then
    exit;
  ASynEdit := TSynEdit(APreviewEditor);

  // Get real settings
  GetSynEditSettings(ASynEdit);

  // Change to preview settings
  ASynEdit.Options := ASynEdit.Options
    - SynEditPreviewExcludeOptions + SynEditPreviewIncludeOptions;
  ASynEdit.Options2 := ASynEdit.Options2 - SynEditPreviewExcludeOptions2;
  ASynEdit.ReadOnly := True;
end;

{ TColorSchemeAttribute }

function TColorSchemeAttribute.OldAdditionalAttributeName(NewAha: String): string;
var
  AttriIdx: Integer;
begin
  AttriIdx := GetEnumValue(TypeInfo(TAdditionalHilightAttribute), NewAha);
  if AttriIdx < 0
    then Result := NewAha
    else Result := ahaXmlNames[TAdditionalHilightAttribute(AttriIdx)];
end;

procedure TColorSchemeAttribute.SetMarkupFoldLineAlpha(AValue: Byte);
begin
  if FMarkupFoldLineAlpha = AValue then Exit;
  FMarkupFoldLineAlpha := AValue;
  Changed;
end;

procedure TColorSchemeAttribute.SetMarkupFoldLineColor(AValue: TColor);
begin
  if FMarkupFoldLineColor = AValue then Exit;
  FMarkupFoldLineColor := AValue;
  Changed;
end;

procedure TColorSchemeAttribute.SetMarkupFoldLineStyle(AValue: TSynLineStyle);
begin
  if FMarkupFoldLineStyle = AValue then Exit;
  FMarkupFoldLineStyle := AValue;
  Changed;
end;

procedure TColorSchemeAttribute.Init;
begin
  inherited Init;
  FFeatures := [hafBackColor, hafForeColor, hafFrameColor, hafStyle, hafFrameStyle, hafFrameEdges];
  FMarkupFoldLineColor := clNone;
  FMarkupFoldLineStyle := slsSolid;
  FMarkupFoldLineAlpha := 0;
end;

function TColorSchemeAttribute.GetIsUsingSchemeGlobals: Boolean;
begin
  Result := FUseSchemeGlobals and (GetSchemeGlobal <> nil);
end;

function TColorSchemeAttribute.GetSchemeGlobal: TColorSchemeAttribute;
begin
  Result := nil;
  if (FOwner <> nil) and (FOwner.FOwner<> nil) and
     (FOwner.FOwner.FDefaultColors <> nil)
  then
    Result := FOwner.FOwner.FDefaultColors.Attribute[StoredName];
  if Result = Self then
    Result := nil;
end;

constructor TColorSchemeAttribute.Create(ASchemeLang: TColorSchemeLanguage;
  attribName: PString; aStoredName: String = '');
begin
  inherited Create(attribName, aStoredName);
  FOwner := ASchemeLang;
  FUseSchemeGlobals := True;
end;

function TColorSchemeAttribute.IsEnabled: boolean;
begin
  Result := (inherited IsEnabled) or (FMarkupFoldLineColor <> clNone);
end;

procedure TColorSchemeAttribute.ApplyTo(aDest: TSynHighlighterAttributes;
  aDefault: TColorSchemeAttribute);
// aDefault (if supplied) is usuallythe Schemes agnDefault / DefaultAttribute
var
  Src: TColorSchemeAttribute;
begin
  Src := Self;
  if IsUsingSchemeGlobals then
    Src := GetSchemeGlobal;
  aDest.BeginUpdate;
  try
    aDest.Background := Src.Background;
    aDest.Foreground := Src.Foreground;
    aDest.FrameColor := Src.FrameColor;
    aDest.FrameEdges := Src.FrameEdges;
    aDest.FrameStyle := Src.FrameStyle;
    aDest.Style      := Src.Style;
    if hafStyleMask in Src.Features then
      aDest.StyleMask  := Src.StyleMask
    else
      aDest.StyleMask  := [low(TFontStyle)..high(TFontStyle)];

    if aDest is TSynHighlighterAttributesModifier then begin
      TSynHighlighterAttributesModifier(aDest).ForeAlpha := Src.ForeAlpha;
      TSynHighlighterAttributesModifier(aDest).BackAlpha := Src.BackAlpha;
      TSynHighlighterAttributesModifier(aDest).FrameAlpha := Src.FrameAlpha;

      if hafPrior in Src.Features then begin
        TSynHighlighterAttributesModifier(aDest).ForePriority      := Src.ForePriority;
        TSynHighlighterAttributesModifier(aDest).BackPriority      := Src.BackPriority;
        TSynHighlighterAttributesModifier(aDest).FramePriority     := Src.FramePriority;
        TSynHighlighterAttributesModifier(aDest).BoldPriority      := Src.BoldPriority;
        TSynHighlighterAttributesModifier(aDest).ItalicPriority    := Src.ItalicPriority;
        TSynHighlighterAttributesModifier(aDest).UnderlinePriority := Src.UnderlinePriority;
      end;
    end;

    if not (aDest is TSynSelectedColor) then begin
      if aDefault <> nil then begin
        if aDefault.IsUsingSchemeGlobals then
          aDefault := aDefault.GetSchemeGlobal;
        if Background = clDefault then
          aDest.Background := aDefault.Background;
        if Foreground = clDefault then
          aDest.Foreground := aDefault.Foreground;
        if FrameColor = clDefault then begin
          aDest.FrameColor := aDefault.FrameColor;
          aDest.FrameEdges := aDefault.FrameEdges;
          aDest.FrameStyle := aDefault.FrameStyle;
        end;
      end;

      //if aDest is TSynHighlighterAttributesModifier then begin
      //end

      if aDest is TColorSchemeAttribute then
        TColorSchemeAttribute(aDest).Group := Src.Group;
    end;
  finally
    aDest.EndUpdate;
  end;
end;

procedure TColorSchemeAttribute.Assign(Src: TPersistent);
begin
  inherited Assign(Src);

  FFeatures := [hafBackColor, hafForeColor, hafFrameColor, hafStyle, hafFrameStyle, hafFrameEdges];
  if Src is TSynHighlighterAttributesModifier then
    FFeatures := FFeatures + [hafAlpha, hafPrior,hafStyleMask];

  if Src is TColorSchemeAttribute then begin
    FGroup := TColorSchemeAttribute(Src).FGroup;
    FUseSchemeGlobals := TColorSchemeAttribute(Src).FUseSchemeGlobals;
    FFeatures := TColorSchemeAttribute(Src).FFeatures;

    FMarkupFoldLineColor := TColorSchemeAttribute(Src).FMarkupFoldLineColor;;
    FMarkupFoldLineStyle := TColorSchemeAttribute(Src).FMarkupFoldLineStyle;;
    FMarkupFoldLineAlpha := TColorSchemeAttribute(Src).FMarkupFoldLineAlpha;;
  end;

end;

function TColorSchemeAttribute.Equals(Other: TColorSchemeAttribute): Boolean;
begin
  Result := (FGroup      = Other.FGroup) and
            (FUseSchemeGlobals = Other.FUseSchemeGlobals) and
            // ignore resourcestring Name and Caption
            (StoredName = Other.StoredName) and
            (Background  = Other.Background) and
            (Foreground  = Other.Foreground) and
            (FrameColor  = Other.FrameColor) and
            ( (FrameColor = clNone) or
              ( (FrameStyle = Other.FrameStyle) and
                (FrameEdges = Other.FrameEdges)
              )
            ) and
            (Style       = Other.Style) and
            (StyleMask   = Other.StyleMask) and
            (Features   = Other.Features);
  end;

function TColorSchemeAttribute.GetStoredValuesForAttrib: TColorSchemeAttribute;
begin
  Result := nil;
  if (FOwner <> nil) and (FOwner.GetStoredValuesForLanguage <> nil) then
    Result := FOwner.GetStoredValuesForLanguage.Attribute[StoredName];
end;

procedure TColorSchemeAttribute.LoadFromXml(aXMLConfig: TRttiXMLConfig; aPath: String;
  Defaults: TColorSchemeAttribute; Version: Integer);
var
  AttriName, Path: String;
  fs: TFontStyles;
begin
  // FormatVersion >= 2
  (* Note: This is currently always called with a default, so the nil handling isn't needed*)
  AttriName := OldAdditionalAttributeName(StoredName);
  if (Version < 5) and (AttriName <> '') then begin
    // Read Version 2-4, 4 if exist, or keep values
    Path := aPath + StrToValidXMLName(AttriName) + '/';

    if aXMLConfig.HasChildPaths(Path) then begin
      if (Defaults <> nil) then
        self.Assign(Defaults);
      Defaults := Self;

      BackGround := aXMLConfig.GetValue(Path + 'BackgroundColor/Value', Defaults.Background);
      ForeGround := aXMLConfig.GetValue(Path + 'ForegroundColor/Value', Defaults.Foreground);
      FrameColor := aXMLConfig.GetValue(Path + 'FrameColor/Value',      Defaults.FrameColor);
      fs   := [];
      if aXMLConfig.GetValue(Path + 'Style/Bold', fsBold in Defaults.Style) then
        Include(fs, fsBold);
      if aXMLConfig.GetValue(Path + 'Style/Italic', fsItalic in Defaults.Style) then
        Include(fs, fsItalic);
      if aXMLConfig.GetValue(Path + 'Style/Underline', fsUnderline in Defaults.Style) then
        Include(fs, fsUnderline);
      Style := fs;
      fs   := [];
      if aXMLConfig.GetValue(Path + 'StyleMask/Bold', fsBold in Defaults.StyleMask) then
        Include(fs, fsBold);
      if aXMLConfig.GetValue(Path + 'StyleMask/Italic', fsItalic in Defaults.StyleMask) then
        Include(fs, fsItalic);
      if aXMLConfig.GetValue(Path + 'StyleMask/Underline', fsUnderline in Defaults.StyleMask) then
        Include(fs, fsUnderline);
      StyleMask := fs;
    end;
  end;

  // Read the Version >= 5 if exist, or keep values
  if StoredName = '' then exit;
  Path := aPath + StrToValidXMLName(StoredName) + '/';
  if (Version <= 5) and (Defaults = nil) then
    Defaults := GetSchemeGlobal;

  if aXMLConfig.HasPath(Path, False) then begin
    aXMLConfig.ReadObject(Path, Self, Defaults);
    if (Version <= 5) then
      UseSchemeGlobals := False;
  end
  else begin
    if (Defaults <> Self) and (Defaults <> nil) then begin
      // do not copy (Stored)Name or Features ...
      Background := Defaults.Background;
      Foreground := Defaults.Foreground;
      FrameColor := Defaults.FrameColor;
      FrameEdges := Defaults.FrameEdges;
      FrameStyle := Defaults.FrameStyle;
      Style      := Defaults.Style;
      StyleMask  := Defaults.StyleMask;
      UseSchemeGlobals := Defaults.UseSchemeGlobals;
      ForePriority      := Defaults.ForePriority;
      BackPriority      := Defaults.BackPriority;
      FramePriority     := Defaults.FramePriority;
      BoldPriority      := Defaults.BoldPriority;
      ItalicPriority    := Defaults.ItalicPriority;
      UnderlinePriority := Defaults.UnderlinePriority;
    end;
    if (Version <= 5) and (Defaults = Self) then     // Data was loaded above (Vers < 5)
      UseSchemeGlobals := False;
  end;
end;

procedure TColorSchemeAttribute.LoadFromXmlV1(aXMLConfig: TRttiXMLConfig; aPath: String;
  Defaults: TColorSchemeAttribute);
var
  fs: TFontStyles;
begin
  // FormatVersion = 1 (only pascal colors)
  if Defaults = nil then
    Defaults := Self;
  if StoredName = '' then exit;
  aPath := aPath + StrToValidXMLName(StoredName) + '/';
  BackGround := aXMLConfig.GetValue(aPath + 'BackgroundColor', Defaults.Background);
  ForeGround := aXMLConfig.GetValue(aPath + 'ForegroundColor', Defaults.Foreground);
  FrameColor := aXMLConfig.GetValue(aPath + 'FrameColorColor', Defaults.FrameColor);
  fs := [];
  if aXMLConfig.GetValue(aPath + 'Bold', fsBold in Defaults.Style) then
    Include(fs, fsBold);
  if aXMLConfig.GetValue(aPath + 'Italic', fsItalic in Defaults.Style) then
    Include(fs, fsItalic);
  if aXMLConfig.GetValue(aPath + 'Underline', fsUnderline in Defaults.Style) then
    Include(fs, fsUnderline);
  Style := fs;
  StyleMask := [];
end;

procedure TColorSchemeAttribute.SaveToXml(aXMLConfig: TRttiXMLConfig; aPath: String;
  Defaults: TColorSchemeAttribute);
var
  AttriName: String;
begin
  if StoredName = '' then
    exit;
  // Delete Version <= 4
  AttriName := OldAdditionalAttributeName(StoredName);
  if AttriName <> '' then
    aXMLConfig.DeletePath(aPath + StrToValidXMLName(AttriName));

  aXMLConfig.WriteObject(aPath + StrToValidXMLName(StoredName) + '/', Self, Defaults);
end;

{ TColorSchemeLanguage }

function TColorSchemeLanguage.GetAttribute(Index: String): TColorSchemeAttribute;
var
  Idx: Integer;
begin
  Idx := FAttributes.IndexOf(UpperCase(Index));
  if Idx = -1 then
    Result := nil
  else
    Result := TColorSchemeAttribute(FAttributes.Objects[Idx]);
end;

function TColorSchemeLanguage.GetAttributeAtPos(Index: Integer): TColorSchemeAttribute;
begin
  Result := TColorSchemeAttribute(FAttributes.Objects[Index]);
end;

function TColorSchemeLanguage.GetAttributeByEnum(Index: TAdditionalHilightAttribute): TColorSchemeAttribute;
begin
  Result := Attribute[AhaToStoredName(Index)];
end;

function TColorSchemeLanguage.GetName: String;
begin
  Result := FOwner.Name;
end;

function TColorSchemeLanguage.AhaToStoredName(aha: TAdditionalHilightAttribute): String;
begin
  Result := GetEnumName(TypeInfo(TAdditionalHilightAttribute), ord(aha));
end;

function TColorSchemeLanguage.GetStoredValuesForLanguage: TColorSchemeLanguage;
begin
  Result := nil;
  if (FOwner <> nil) and (FOwner.GetStoredValuesForScheme <> nil) then
    Result := FOwner.GetStoredValuesForScheme.ColorScheme[FLanguage];
end;

constructor TColorSchemeLanguage.Create(const AGroup: TColorScheme;
  const ALang: TLazSyntaxHighlighter; IsSchemeDefault: Boolean = False);
begin
  inherited Create;
  FIsSchemeDefault := IsSchemeDefault;
  FAttributes := TQuickStringlist.Create;
  FOwner := AGroup;
  FHighlighter := nil;
  FLanguage := ALang;
  if LazSyntaxHighlighterClasses[ALang] <> nil then begin
    FHighlighter := LazSyntaxHighlighterClasses[ALang].Create(nil);
    FLanguageName := FHighlighter.LanguageName;
  end;
  FDefaultAttribute := TColorSchemeAttribute.Create(Self, @dlgAddHiAttrDefault, 'ahaDefault');
  FDefaultAttribute.Features := [hafBackColor, hafForeColor];
  FDefaultAttribute.Group := agnDefault;
  FAttributes.AddObject(UpperCase(FDefaultAttribute.StoredName), FDefaultAttribute);
  FAttributes.Sorted := true;
end;

constructor TColorSchemeLanguage.CreateFromXml(const AGroup: TColorScheme;
  const ALang: TLazSyntaxHighlighter; aXMLConfig: TRttiXMLConfig; aPath: String;
  IsSchemeDefault: Boolean);
var
  csa: TColorSchemeAttribute;
  i: Integer;
  aha: TAdditionalHilightAttribute;
  FormatVersion: longint;
begin
  Create(AGroup, ALang, IsSchemeDefault);   // don't call inherited Create

  FAttributes.Sorted := False;
  if FHighlighter <> nil then begin
    for i := 0 to FHighlighter.AttrCount - 1 do begin
      csa := TColorSchemeAttribute.Create(Self,
                                   FHighlighter.Attribute[i].Caption,
                                   FHighlighter.Attribute[i].StoredName
                                   );
      csa.Assign(FHighlighter.Attribute[i]);
      csa.Group := agnLanguage;
      FAttributes.AddObject(UpperCase(csa.StoredName), csa);
    end;
  end;

  for aha := Low(TAdditionalHilightAttribute) to High(TAdditionalHilightAttribute) do begin
    if aha = ahaNone then continue;
    csa := TColorSchemeAttribute.Create(Self, @AdditionalHighlightAttributes[aha],
                                        AhaToStoredName(aha)
                                       );
    csa.Features := ahaSupportedFeatures[aha];
    csa.Group    := ahaGroupMap[aha];
    FAttributes.AddObject(UpperCase(csa.StoredName), csa);
  end;

  FAttributes.Sorted := true;
  FormatVersion := aXMLConfig.GetValue(aPath + 'Version', 0);
  LoadFromXml(aXMLConfig, aPath, nil, FormatVersion);


end;

destructor TColorSchemeLanguage.Destroy;
begin
  Clear;
  FreeAndNil(FHighlighter);
  FreeAndNil(FAttributes);
  // FreeAndNil(FDefaultAttribute); // part of the list
end;

procedure TColorSchemeLanguage.Clear;
var
  i: Integer;
begin
  if Assigned(FAttributes) then
    for i := 0 to FAttributes.Count - 1 do
      TColorSchemeAttribute(FAttributes.Objects[i]).Free;
  FAttributes.Clear;
end;

procedure TColorSchemeLanguage.Assign(Src: TColorSchemeLanguage);
var
  i, j: Integer;
  Attr: TColorSchemeAttribute;
  NewList: TQuickStringlist;
begin
  // Do not clear old list => external references to Attributes may exist
  FLanguage := Src.FLanguage;
  FLanguageName := src.FLanguageName;
  //FDefaultAttribute.Assign(Src.FDefaultAttribute);
  FDefaultAttribute := nil;
  NewList := TQuickStringlist.Create;
  for i := 0 to Src.AttributeCount - 1 do begin
    j := FAttributes.IndexOf(UpperCase(Src.AttributeAtPos[i].StoredName));
    if j >= 0 then begin
      Attr := TColorSchemeAttribute(FAttributes.Objects[j]);
      FAttributes.Delete(j);
    end
    else
      Attr := TColorSchemeAttribute.Create(Self,
                                       Src.AttributeAtPos[i].Caption,
                                       Src.AttributeAtPos[i].StoredName);
    Attr.Assign(Src.AttributeAtPos[i]);
    NewList.AddObject(UpperCase(Attr.StoredName), Attr);
    if Src.AttributeAtPos[i] = Src.DefaultAttribute then
      FDefaultAttribute := Attr;
  end;
  Clear;
  FreeAndNil(FAttributes);
  FAttributes := NewList;
  FAttributes.Sorted := true;
end;

function TColorSchemeLanguage.Equals(Other: TColorSchemeLanguage): Boolean;
var
  i: Integer;
begin
  Result := //FDefaultAttribute.Equals(Other.FDefaultAttribute) and
            (FLanguage = Other.FLanguage) and
            (FAttributes.Count = Other.FAttributes.Count);
  i := FAttributes.Count - 1;
  while Result and (i >= 0) do begin
    Result := Result and
              (Other.Attribute[AttributeAtPos[i].StoredName] <> nil) and
              AttributeAtPos[i].Equals(Other.Attribute[AttributeAtPos[i].StoredName]);
    dec(i);
  end;
end;

function TColorSchemeLanguage.IndexOfAttr(AnAttr: TColorSchemeAttribute): Integer;
begin
  Result := FAttributes.IndexOfObject(AnAttr);
end;

procedure TColorSchemeLanguage.LoadFromXml(aXMLConfig: TRttiXMLConfig; aPath: String;
  Defaults: TColorSchemeLanguage; ColorVersion: Integer; aOldPath: String);
var
  Def: TColorSchemeAttribute;
  FormatVersion: longint;
  TmpPath: String;
  i: Integer;
  EmptyDef: TColorSchemeAttribute;
begin
//  Path := 'EditorOptions/Color/'

  if not FIsSchemeDefault then
    TmpPath := aPath + 'Lang' + StrToValidXMLName(FLanguageName) + '/'
  else
    TmpPath := aPath;
  if aXMLConfig.HasChildPaths(TmpPath) then begin
    FormatVersion := aXMLConfig.GetValue(TmpPath + 'Version', 0);
    if FormatVersion > ColorVersion then
      FormatVersion := ColorVersion;
    if FIsSchemeDefault and (FormatVersion < 6) then
      FormatVersion := 6;
  end
  else
    FormatVersion := 6;
  FFormatVersion := FormatVersion;
  TmpPath := TmpPath + 'Scheme' + StrToValidXMLName(Name) + '/';

  if (aOldPath <> '') and (FormatVersion > 1) then begin
    // convert some old data (loading user settings only):
    // aOldPath should be 'EditorOptions/Display/'
    if aXMLConfig.GetValue(aOldPath + 'RightMarginColor', '') <> '' then
      aXMLConfig.SetValue(TmpPath + 'ahaRightMargin/ForegroundColor/Value',
                          aXMLConfig.GetValue(aOldPath + 'RightMarginColor', 0)
                         );
    if aXMLConfig.GetValue(aOldPath + 'GutterColor', '') <> '' then
      aXMLConfig.SetValue(TmpPath + 'ahaGutter/BackgroundColor/Value',
                          aXMLConfig.GetValue(aOldPath + 'GutterColor', 0)
                         );
  end;

  // Defaults <> nil => saving diff between Scheme(=Defaults) and userSettings
  // Defaults = nil
  //   Attribute has SchemeDefault => Save diff to SchemeDefault
  //     SchemeDefault_Attri.UseSchemeGlobals must be TRUE => so it serves as default
  //   Attribute hasn't SchemeDefault => Save diff to empty
  if (Defaults = nil) then
    // default all colors = clNone
    EmptyDef := TColorSchemeAttribute.Create(Self, nil, '')
  else
    EmptyDef := nil;

  for i := 0 to AttributeCount - 1 do begin
    if Defaults <> nil then
      Def := Defaults.Attribute[AttributeAtPos[i].StoredName]
    else begin
      if AttributeAtPos[i].GetSchemeGlobal <> nil then
        Def := AttributeAtPos[i].GetSchemeGlobal
      else
        Def := EmptyDef;
    end;

    //if ColorVersion < 2 then begin
    if FormatVersion < 2 then begin
      //if aXMLConfig.HasChildPaths(aPath) or (Defaults <> nil) or (Def <> EmptyDef) then
        AttributeAtPos[i].LoadFromXmlV1(aXMLConfig, aPath, Def)
    end else begin
      //if aXMLConfig.HasPath(TmpPath, False) or (Defaults <> nil) or (Def <> EmptyDef) then
        AttributeAtPos[i].LoadFromXml(aXMLConfig, TmpPath, Def, FormatVersion);
    end;

    if (ColorVersion < 9) and (AttributeAtPos[i].StoredName = AhaToStoredName(ahaMouseLink)) then begin
      // upgrade ahaMouseLink
      AttributeAtPos[i].FrameColor := AttributeAtPos[i].Foreground;
      AttributeAtPos[i].Background := clNone;
      AttributeAtPos[i].Style := [];
      AttributeAtPos[i].StyleMask := [];
      AttributeAtPos[i].FrameStyle := slsSolid;
      AttributeAtPos[i].FrameEdges := sfeBottom;
    end;

    if (ColorVersion < 12) and (AttributeAtPos[i].Group = agnOutlineColors) then begin
      AttributeAtPos[i].MarkupFoldLineColor := AttributeAtPos[i].Foreground;
    end
  end;
  FreeAndNil(EmptyDef);

  // Version 5 and before stored the global background on the Whitespace attribute.
  // If a whitespace Attribute was loaded (UseSchemeGlobals=false) then copy it
  if (FormatVersion <= 5) and (DefaultAttribute <> nil) and
      (FHighlighter <> nil) and (FHighlighter.WhitespaceAttribute <> nil) and
      (Attribute[Highlighter.WhitespaceAttribute.StoredName] <> nil) and
      (not Attribute[Highlighter.WhitespaceAttribute.StoredName].UseSchemeGlobals)
  then
    DefaultAttribute.Background := Attribute[Highlighter.WhitespaceAttribute.StoredName].Background;
end;

procedure TColorSchemeLanguage.SaveToXml(aXMLConfig: TRttiXMLConfig; aPath: String;
  Defaults: TColorSchemeLanguage);
var
  Def: TColorSchemeAttribute;
  i: Integer;
  EmptyDef: TColorSchemeAttribute;
begin
  if (FLanguageName = '') and (not FIsSchemeDefault) then
    exit;
  if not FIsSchemeDefault then
    aPath := aPath + 'Lang' + StrToValidXMLName(FLanguageName) + '/';
  if (Defaults <> nil) and Self.Equals(Defaults) then begin
    aXMLConfig.DeletePath(aPath + 'Scheme' + StrToValidXMLName(Name));
    if not FIsSchemeDefault then begin
      if not aXMLConfig.HasChildPaths(aPath) then
        aXMLConfig.DeletePath(aPath);
    end;
    exit;
  end;
  aXMLConfig.SetValue(aPath + 'Version', EditorOptsFormatVersion);
  aPath := aPath + 'Scheme' + StrToValidXMLName(Name) + '/';

  if (Defaults = nil) then
    // default all colors = clNone
    EmptyDef := TColorSchemeAttribute.Create(Self, nil, '')
  else
    EmptyDef := nil;

  for i := 0 to AttributeCount - 1 do begin
    if Defaults <> nil then
      Def := Defaults.Attribute[AttributeAtPos[i].StoredName]
    else begin
      if AttributeAtPos[i].GetSchemeGlobal <> nil then
        Def := AttributeAtPos[i].GetSchemeGlobal
      else
        Def := EmptyDef;
    end;
    AttributeAtPos[i].SaveToXml(aXMLConfig, aPath, Def);
  end;
  FreeAndNil(EmptyDef);
end;

procedure TColorSchemeLanguage.ApplyTo(ASynEdit: TSynEdit);
  procedure SetMarkupColor(aha: TAdditionalHilightAttribute; aMarkup : TSynSelectedColor);
  var Attrib: TColorSchemeAttribute;
  begin
    Attrib := AttributeByEnum[aha];
    if Attrib <> nil then
      Attrib.ApplyTo(aMarkup)
    else
      DefaultAttribute.ApplyTo(aMarkup);
  end;
  procedure SetMarkupColorByClass(aha: TAdditionalHilightAttribute; aClass: TSynEditMarkupClass);
  begin
    if assigned(ASynEdit.MarkupByClass[aClass]) then
      SetMarkupColor(aha, ASynEdit.MarkupByClass[aClass].MarkupInfo);
  end;
  procedure SetGutterColorByClass(aha: TAdditionalHilightAttribute;
                                  aClass: TSynGutterPartBaseClass);
  begin
    if assigned(ASynEdit.Gutter.Parts.ByClass[aClass, 0]) then
      SetMarkupColor(aha, ASynEdit.Gutter.Parts.ByClass[aClass, 0].MarkupInfo);
  end;
  function GetUsedAttr(aha: TAdditionalHilightAttribute): TColorSchemeAttribute;
  begin
    Result := AttributeByEnum[aha];
    if Assigned(Result) and Result.IsUsingSchemeGlobals then
      Result := Result.GetSchemeGlobal;
  end;
var
  Attri: TColorSchemeAttribute;
  i, c, j: Integer;
  IDESynEdit: TIDESynEditor;
  aha: TAdditionalHilightAttribute;
  col: TColor;
begin
  ASynEdit.BeginUpdate;
  try
    try
      Attri := DefaultAttribute;
      if Attri.IsUsingSchemeGlobals then
        Attri := Attri.GetSchemeGlobal;
      if (Attri.Background = clNone) or (Attri.Background = clDefault)
        then aSynEdit.Color := clWhite
        else aSynEdit.Color := Attri.Background;
      if (Attri.Foreground = clNone) or (Attri.Foreground = clDefault)
        then aSynEdit.Font.Color := clBlack
        else aSynEdit.Font.Color := Attri.Foreground;
    except
      aSynEdit.Color := clWhite;
      aSynEdit.Font.Color := clBlack;
    end;

    Attri := GetUsedAttr(ahaGutter);
    if Attri <> nil then
      aSynEdit.Gutter.Color := Attri.Background;

    Attri := GetUsedAttr(ahaRightMargin);
    if Attri <> nil then
      aSynEdit.RightEdgeColor := Attri.Foreground;

    SetMarkupColor(ahaTextBlock,         aSynEdit.SelectedColor);
    SetMarkupColor(ahaIncrementalSearch, aSynEdit.IncrementColor);
    SetMarkupColor(ahaHighlightAll,      aSynEdit.HighlightAllColor);
    SetMarkupColor(ahaBracketMatch,      aSynEdit.BracketMatchColor);
    SetMarkupColor(ahaMouseLink,         aSynEdit.MouseLinkColor);
    SetMarkupColor(ahaFoldedCode,        aSynEdit.FoldedCodeColor);
    SetMarkupColor(ahaFoldedCodeLine,    aSynEdit.FoldedCodeLineColor);
    SetMarkupColor(ahaHiddenCodeLine,    aSynEdit.HiddenCodeLineColor);
    SetMarkupColor(ahaLineHighlight,     aSynEdit.LineHighlightColor);
    if ASynEdit is TIDESynEditor then begin
      SetMarkupColor(ahaTopInfoHint,  TIDESynEditor(aSynEdit).TopInfoMarkup);
      Attri := GetUsedAttr(ahaCaretColor);
      if Attri <> nil then begin
        TIDESynEditor(aSynEdit).CaretColor := Attri.Foreground;

        col := Attri.Background;
        if (col = clNone) or (col = clDefault) then
          col := $606060;
        TIDESynEditor(aSynEdit).MultiCaret.Color := col;
      end;
    end;
    SetMarkupColorByClass(ahaHighlightWord, TSynEditMarkupHighlightAllCaret);
    SetMarkupColorByClass(ahaWordGroup,     TSynEditMarkupWordGroup);
    SetMarkupColorByClass(ahaSpecialVisibleChars, TSynEditMarkupSpecialChar);
    if ASynEdit is TIDESynEditor then begin
      with TIDESynEditor(ASynEdit) do begin
        if AttributeByEnum[ahaIfDefBlockInactive] <> nil
          then AttributeByEnum[ahaIfDefBlockInactive].ApplyTo(MarkupIfDef.MarkupInfoDisabled )
          else MarkupIfDef.MarkupInfoDisabled.Clear;
        if AttributeByEnum[ahaIfDefBlockActive] <> nil
          then AttributeByEnum[ahaIfDefBlockActive].ApplyTo(MarkupIfDef.MarkupInfoEnabled )
          else MarkupIfDef.MarkupInfoEnabled.Clear;
        if AttributeByEnum[ahaIfDefBlockTmpActive] <> nil
          then AttributeByEnum[ahaIfDefBlockTmpActive].ApplyTo(MarkupIfDef.MarkupInfoTempEnabled )
          else MarkupIfDef.MarkupInfoTempEnabled.Clear;
        if AttributeByEnum[ahaIfDefNodeInactive] <> nil
          then AttributeByEnum[ahaIfDefNodeInactive].ApplyTo(MarkupIfDef.MarkupInfoNodeDisabled )
          else MarkupIfDef.MarkupInfoNodeDisabled.Clear;
        if AttributeByEnum[ahaIfDefNodeActive] <> nil
          then AttributeByEnum[ahaIfDefNodeActive].ApplyTo(MarkupIfDef.MarkupInfoNodeEnabled )
          else MarkupIfDef.MarkupInfoNodeEnabled.Clear;
        if AttributeByEnum[ahaIfDefNodeTmpActive] <> nil
          then AttributeByEnum[ahaIfDefNodeTmpActive].ApplyTo(MarkupIfDef.MarkupInfoTempNodeEnabled )
          else MarkupIfDef.MarkupInfoTempNodeEnabled.Clear;
      end;
    end;
    SetGutterColorByClass(ahaLineNumber,      TSynGutterLineNumber);
    SetGutterColorByClass(ahaModifiedLine,    TSynGutterChanges);
    SetGutterColorByClass(ahaCodeFoldingTree, TSynGutterCodeFolding);
    SetGutterColorByClass(ahaGutterSeparator, TSynGutterSeparator);

    if ASynEdit is TIDESynEditor then
    begin
      IDESynEdit := TIDESynEditor(ASynEdit);

      Attri := GetUsedAttr(ahaIdentComplWindow);
      if Attri<>nil then
      begin
        IDESynEdit.MarkupIdentComplWindow.TextColor := Attri.Foreground;
        IDESynEdit.MarkupIdentComplWindow.WindowColor:= Attri.Background;
      end else
      begin
        IDESynEdit.MarkupIdentComplWindow.TextColor := clNone;
        IDESynEdit.MarkupIdentComplWindow.WindowColor:= clNone;
      end;

      Attri := GetUsedAttr(ahaIdentComplWindowBorder);
      if Attri<>nil then
        IDESynEdit.MarkupIdentComplWindow.BorderColor:= Attri.Foreground
      else
        IDESynEdit.MarkupIdentComplWindow.BorderColor:= clNone;

      Attri := GetUsedAttr(ahaIdentComplWindowHighlight);
      if Attri<>nil then
        IDESynEdit.MarkupIdentComplWindow.HighlightColor:= Attri.Foreground
      else
        IDESynEdit.MarkupIdentComplWindow.HighlightColor:= clNone;

      Attri := GetUsedAttr(ahaIdentComplWindowSelection);
      if Attri<>nil then
      begin
        IDESynEdit.MarkupIdentComplWindow.TextSelectedColor:= Attri.Foreground;
        IDESynEdit.MarkupIdentComplWindow.BackgroundSelectedColor:= Attri.Background;
      end else
      begin
        IDESynEdit.MarkupIdentComplWindow.TextSelectedColor := clNone;
        IDESynEdit.MarkupIdentComplWindow.BackgroundSelectedColor:= clNone;
      end;
    end;

    i := aSynEdit.PluginCount - 1;
    while (i >= 0) and not(aSynEdit.Plugin[i] is TSynPluginTemplateEdit) do
      dec(i);
    if i >= 0 then begin
      SetMarkupColor(ahaTemplateEditOther,TSynPluginTemplateEdit(aSynEdit.Plugin[i]).MarkupInfo);
      SetMarkupColor(ahaTemplateEditCur,  TSynPluginTemplateEdit(aSynEdit.Plugin[i]).MarkupInfoCurrent);
      SetMarkupColor(ahaTemplateEditSync, TSynPluginTemplateEdit(aSynEdit.Plugin[i]).MarkupInfoSync);
    end;
    i := aSynEdit.PluginCount - 1;
    while (i >= 0) and not(aSynEdit.Plugin[i] is TSynPluginSyncroEdit) do
      dec(i);
    if i >= 0 then begin
      SetMarkupColor(ahaSyncroEditOther, TSynPluginSyncroEdit(aSynEdit.Plugin[i]).MarkupInfo);
      SetMarkupColor(ahaSyncroEditCur,   TSynPluginSyncroEdit(aSynEdit.Plugin[i]).MarkupInfoCurrent);
      SetMarkupColor(ahaSyncroEditSync,  TSynPluginSyncroEdit(aSynEdit.Plugin[i]).MarkupInfoSync);
      SetMarkupColor(ahaSyncroEditArea,  TSynPluginSyncroEdit(aSynEdit.Plugin[i]).MarkupInfoArea);
    end;
    i := aSynEdit.MarkupCount - 1;
    while (i >= 0) and not(aSynEdit.Markup[i] is TSynEditMarkupFoldColors) do
      dec(i);
    if i >= 0 then begin
      TSynEditMarkupFoldColors(aSynEdit.Markup[i]).ColorCount := 10;
      j := 0;
      c := 0;
      for aha := ahaOutlineLevel1Color to ahaOutlineLevel10Color do begin
        Attri := GetUsedAttr(aha);
        if Attri = nil then Continue;
        if (Attri.IsEnabled) or
           (FFormatVersion >= 12)
        then begin
          SetMarkupColor(aha, TSynEditMarkupFoldColors(aSynEdit.Markup[i]).Color[j]);

          TSynEditMarkupFoldColors(aSynEdit.Markup[i]).LineColor[j].Color := Attri.MarkupFoldLineColor;
          TSynEditMarkupFoldColors(aSynEdit.Markup[i]).LineColor[j].Style := Attri.MarkupFoldLineStyle;
          TSynEditMarkupFoldColors(aSynEdit.Markup[i]).LineColor[j].Alpha := Attri.MarkupFoldLineAlpha;
          TSynEditMarkupFoldColors(aSynEdit.Markup[i]).LineColor[j].Priority := Attri.FramePriority;
          inc(j);
          if Attri.IsEnabled then
            c := j;
        end;
      end;
      TSynEditMarkupFoldColors(aSynEdit.Markup[i]).ColorCount := c; // discard unused colors at the end
    end;
  finally
    ASynEdit.EndUpdate;
  end;
end;

procedure TColorSchemeLanguage.ApplyTo(AHLighter: TSynCustomHighlighter);
var
  i: Integer;
  Attr: TColorSchemeAttribute;
begin
  AHLighter.BeginUpdate;
  try
    for i := 0 to AHLighter.AttrCount - 1 do begin
      Attr := Attribute[AHLighter.Attribute[i].StoredName];
      if Attr <> nil then
        Attr.ApplyTo(AHLighter.Attribute[i], DefaultAttribute);
    end;
  finally
    AHLighter.EndUpdate;
  end;
end;

function TColorSchemeLanguage.AttributeCount: Integer;
begin
  Result := FAttributes.Count;
end;

{ TColorScheme }

function TColorScheme.GetColorScheme(Index: TLazSyntaxHighlighter): TColorSchemeLanguage;
begin
  Result := FColorSchemes[CompatibleLazSyntaxHilighter[Index]];
end;

function TColorScheme.GetColorSchemeBySynClass(Index: TClass): TColorSchemeLanguage;
var
  i: TLazSyntaxHighlighter;
begin
  for i := low(TLazSyntaxHighlighter) to high(TLazSyntaxHighlighter) do
    if LazSyntaxHighlighterClasses[i] = Index then
      exit(FColorSchemes[CompatibleLazSyntaxHilighter[i]]);
  Result := nil;
end;

function TColorScheme.GetStoredValuesForScheme: TColorScheme;
begin
  Result:=ColorSchemeFactory.ColorSchemeGroup[Name];
end;

constructor TColorScheme.Create(AName: String);
begin
  inherited Create;
  FName := AName;
end;

constructor TColorScheme.CreateFromXml(aXMLConfig: TRttiXMLConfig; const AName, aPath: String);
var
  i: TLazSyntaxHighlighter;
begin
  Create(AName);
  FDefaultColors := TColorSchemeLanguage.CreateFromXml(Self, lshNone, aXMLConfig,
    aPath  + 'Globals/', True);
  for i := low(TLazSyntaxHighlighter) to high(TLazSyntaxHighlighter) do
    // do not create duplicates
    if CompatibleLazSyntaxHilighter[i] = i then
      FColorSchemes[i] := TColorSchemeLanguage.CreateFromXml(Self, i, aXMLConfig,
        aPath)
    else
      FColorSchemes[i] := nil;
end;

destructor TColorScheme.Destroy;
var
  i: TLazSyntaxHighlighter;
begin
  inherited Destroy;
  FreeAndNil(FDefaultColors);
  for i := low(TLazSyntaxHighlighter) to high(TLazSyntaxHighlighter) do
    FreeAndNil(FColorSchemes[i]);
end;

procedure TColorScheme.Assign(Src: TColorScheme);
var
  i: TLazSyntaxHighlighter;
begin
  if Src.FDefaultColors = nil then
    FreeAndNil(FDefaultColors)
  else
  if (FDefaultColors = nil) then
    FDefaultColors := TColorSchemeLanguage.Create(Self, lshNone, True);
  if FDefaultColors <> nil then
    FDefaultColors.Assign(Src.FDefaultColors);
  for i := low(FColorSchemes) to high(FColorSchemes) do begin
    if Src.FColorSchemes[i] = nil then begin
      FreeAndNil(FColorSchemes[i]);
    end else begin
      if FColorSchemes[i] = nil then
        FColorSchemes[i] := TColorSchemeLanguage.Create(Self, i);
      FColorSchemes[i].Assign(Src.FColorSchemes[i]);
    end;
  end;
end;

procedure TColorScheme.LoadFromXml(aXMLConfig: TRttiXMLConfig; aPath: String;
  Defaults: TColorScheme; aOldPath: String);
var
  i: TLazSyntaxHighlighter;
  Def: TColorSchemeLanguage;
  FormatVersion: longint;
begin
  FormatVersion := aXMLConfig.GetValue(aPath + 'Version', 0);
  if Defaults <> nil then
    Def := Defaults.DefaultColors
  else
    Def := nil;
  FDefaultColors.LoadFromXml(aXMLConfig, aPath + 'Globals/', Def, FormatVersion);
  for i := low(TLazSyntaxHighlighter) to high(TLazSyntaxHighlighter) do
    if ColorScheme[i] <> nil then begin
      if Defaults <> nil then
        Def := Defaults.ColorScheme[i]
      else
        Def := nil;
      ColorScheme[i].LoadFromXml(aXMLConfig, aPath, Def, FormatVersion, aOldPath);
    end;
end;

procedure TColorScheme.SaveToXml(aXMLConfig: TRttiXMLConfig; aPath: String;
  Defaults: TColorScheme);
var
  i: TLazSyntaxHighlighter;
  Def: TColorSchemeLanguage;
begin
  if Defaults <> nil then
    Def := Defaults.DefaultColors
  else
    Def := nil;
  FDefaultColors.SaveToXml(aXMLConfig, aPath + 'Globals/', Def);
  if not aXMLConfig.HasChildPaths(aPath + 'Globals') then
    aXMLConfig.DeletePath(aPath + 'Globals');
  for i := low(TLazSyntaxHighlighter) to high(TLazSyntaxHighlighter) do
    if ColorScheme[i] <> nil then begin
      if Defaults <> nil then
        Def := Defaults.ColorScheme[i]
      else
        Def := nil;
      ColorScheme[i].SaveToXml(aXMLConfig, aPath, Def);
    end;
  aXMLConfig.SetValue(aPath + 'Version', EditorOptsFormatVersion);
end;

{ TColorSchemeFactory }

function TColorSchemeFactory.GetColorSchemeGroup(Index: String): TColorScheme;
var
  Idx: integer;
begin
  Idx := FMappings.IndexOf(UpperCase(Index));
  if Idx = -1 then
    Result := nil
  else
    Result := TColorScheme(FMappings.Objects[Idx]);
end;

function TColorSchemeFactory.GetColorSchemeGroupAtPos(Index: Integer): TColorScheme;
begin
  Result := TColorScheme(FMappings.Objects[Index]);
end;

constructor TColorSchemeFactory.Create;
begin
  inherited Create;
  FMappings := TQuickStringlist.Create;
  FMappings.Sorted := true;
end;

destructor TColorSchemeFactory.Destroy;
begin
  Clear;
  FreeAndNil(FMappings);
  inherited Destroy;
end;

procedure TColorSchemeFactory.Clear;
var
  i: Integer;
begin
  if Assigned(FMappings) then
  begin
    for i := 0 to FMappings.Count - 1 do
      TColorScheme(FMappings.Objects[i]).Free;
    FMappings.Clear;
  end;
end;

procedure TColorSchemeFactory.Assign(Src: TColorSchemeFactory);
var
  lMapping: TColorScheme;
  i: Integer;
begin
  FMappings.Sorted := False;
  Clear;
  for i := 0 to Src.FMappings.Count - 1 do begin
    lMapping := TColorScheme.Create(Src.ColorSchemeGroupAtPos[i].Name);
    lMapping.Assign(Src.ColorSchemeGroupAtPos[i]);
    FMappings.AddObject(UpperCase(lMapping.Name), lMapping);
  end;
  FMappings.Sorted := true;
end;

procedure TColorSchemeFactory.LoadFromXml(aXMLConfig: TRttiXMLConfig; aPath: String;
  Defaults: TColorSchemeFactory; aOldPath: String);
var
  i: Integer;
  Def: TColorScheme;
begin
  for i := 0 to FMappings.Count - 1 do begin
    if Defaults <> nil then
      Def := Defaults.ColorSchemeGroupAtPos[i]
    else
      Def := nil;
    ColorSchemeGroupAtPos[i].LoadFromXml(aXMLConfig, aPath,
                                         Def, aOldPath);
  end;
  // all Schemes have read (and relocated) the old values
  if aOldPath <> '' then begin
    aXMLConfig.DeletePath(aOldPath + 'RightMarginColor');
    aXMLConfig.DeletePath(aOldPath + 'GutterColor');
  end;
end;

procedure TColorSchemeFactory.SaveToXml(aXMLConfig: TRttiXMLConfig; aPath: String;
  Defaults: TColorSchemeFactory);
var
  i: Integer;
  Def: TColorScheme;
begin
  for i := 0 to FMappings.Count - 1 do begin
    if Defaults <> nil then
      Def := Defaults.ColorSchemeGroupAtPos[i]
    else
      Def := nil;
    ColorSchemeGroupAtPos[i].SaveToXml(aXMLConfig, aPath, Def);
  end
end;

procedure TColorSchemeFactory.RegisterScheme(aXMLConfig: TRttiXMLConfig; AName, aPath: String);
var
  i, j: integer;
  lMapping: TColorScheme;
begin
  i := FMappings.IndexOf(UpperCase(AName));
  if i <> -1 then begin
    j := 0;
    while i >= 0 do begin
      inc(j);
      i := FMappings.IndexOf(UpperCase(AName+'_'+IntToStr(j)));
    end;
    AName := AName+'_'+IntToStr(j);
  end;
  lMapping := TColorScheme.CreateFromXml(aXMLConfig, AName, aPath);
  FMappings.AddObject(UpperCase(AName), lMapping);
end;

procedure TColorSchemeFactory.GetRegisteredSchemes(AList: TStrings);
var
  i: integer;
begin
  AList.BeginUpdate;
  try
    AList.Clear;
    for i := 0 to FMappings.Count - 1 do
      AList.Add(TColorScheme(FMappings.Objects[i]).Name);
  finally
    AList.EndUpdate;
  end;
end;

{ TQuickStringlist }

function TQuickStringlist.DoCompareText(const s1, s2: string): PtrInt;
var
  i, l: Integer;
begin
  Result := length(s1) - length(s2);
  if Result <> 0 then
    exit;
  i := 1;
  if Result < 0 then
    l := length(s1)
  else
    l := length(s2);
  while i <= l do begin
    Result := ord(s1[i]) - ord(s2[i]);
    if Result <> 0 then
      exit;
    inc(i);
  end;
  Result := 0;
end;

initialization
  RegisterIDEOptionsGroup(GroupEditor, TEditorOptions);

finalization
  ColorSchemeFactory.Free;
  HighlighterListSingleton.Free;

end.
