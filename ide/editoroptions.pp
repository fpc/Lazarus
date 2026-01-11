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
  Classes, SysUtils, typinfo, fgl, Math, resource,
  // LCL
  Graphics, LResources, Forms, Dialogs, ComCtrls, LCLType, Controls, LCLProc,
  // LazUtils
  FileUtil, LazFileUtils, LazUTF8, LazClasses, Laz2_XMLCfg, LazStringUtils, LazLoggerBase,
  // Synedit
  SynEdit, SynEditAutoComplete, SynEditKeyCmds, SynEditTypes,
  SynEditMiscClasses, SynBeautifier, SynEditTextTrimmer, SynEditMouseCmds,
  SynPluginTemplateEdit, SynPluginSyncroEdit,
  SynGutter, SynGutterBase, SynGutterCodeFolding, SynGutterLineNumber,
  SynGutterChanges, SynCompletion,
  SynEditMarkupBracket, SynEditMarkupHighAll, SynEditMarkupWordGroup,
  SynEditMarkupSpecialChar,
  // LazEdit
  TextMateGrammar, LazEditTextAttributes,
  // SynEdit Highlighters
  SynEditHighlighter, SynEditHighlighterFoldBase, SynHighlighterCPP, SynHighlighterHTML,
  SynHighlighterJava, SynHighlighterLFM, SynHighlighterPas, SynHighlighterPerl, SynHighlighterPHP,
  SynHighlighterSQL, SynHighlighterCss, SynHighlighterPython, SynHighlighterUNIXShellScript,
  SynHighlighterXML, SynHighlighterJScript, SynHighlighterDiff, SynHighlighterBat,
  SynHighlighterIni, SynHighlighterPo, SynHighlighterPike, SynPluginMultiCaret,
  SynEditMarkupFoldColoring, SynEditMarkup, SynGutterLineOverview, SynBeautifierPascal,
  SynEditTextDynTabExpander, SynEditTextTabExpander, SynTextMateSyn, SynEditStrConst,
  SynHighlighterPosition, SynGutterMarks, SynEditWrappedView, SynPluginExternalLink,
  SynPluginAutoBraces,
  // codetools
  LinkScanner, CodeToolManager,
  // BuildIntf
  IDEOptionsIntf, MacroIntf,
  // IDEIntf
  IDECommands, SrcEditorIntf, IDEOptEditorIntf, IDEDialogs, EditorSyntaxHighlighterDef,
  // IdeConfig
  LazConf,
  // IDE
  SourceMarks, SourceSynEditor, LazarusIDEStrConsts, KeyMapping, AssemblerDlg;

const
  DefaultCompletionLongLineHintType = sclpExtendRightOnly;
  DefaultEditorDisableAntiAliasing = false;

type
  TPreviewPasSyn = TIDESynFreePasSyn;
  TSrcIDEHighlighter = TSynCustomHighlighter;
  // TSynPositionHighlighter - minimum implementation needed.
  TNonSrcIDEHighlighter = class(TSynPositionHighlighter); // Hold colors, not related to SourceEditor

  TSynHighlightElement = TLazEditTextAttribute deprecated 'use TLazEditTextAttribute // to be removed in 5.99';
  TCustomSynClass = class of TSrcIDEHighlighter;

  TLazSynPluginTemplateMultiCaret = class(TForm)   end;
  TLazSynPluginTemplateEditForm = class(TForm)     end;
  TLazSynPluginTemplateEditFormOff = class(TForm)  end;
  TLazSynPluginSyncroEditFormSel = class(TForm)    end;
  TLazSynPluginSyncroEditForm = class(TForm)       end;
  TLazSynPluginSyncroEditFormOff = class(TForm)    end;

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
    'Code folding tree',   '',                          'Highlight current word',
    'Folded code',         'Folded code Line',          'Hidden code Line',
    'Word-Brackets',       'TemplateEdit Current',      'TemplateEdit Sync',
    'TemplateEdit Cells',  'SyncronEdit Current Cells', 'SyncronEdit Syncron Cells',
    'SyncronEdit Other Cells', 'SyncronEdit Range',
    '', // scaGutterSeparator => uses RTTI only
    '', // ahaGutter
    '',  // ahaRightMargin
    '',  // ahaSpecialVisibleChars
    '',  // ahaTopInfoHint
    '', '', // ahaCaretColor, ahaOverviewGutter
    '', '', // ahaGutterCurrentLine, ahaGutterNumberCurrentLine
    '', '', '',  // ahaIfDefBlockInactive, ahaIfDefBlockActive, ahaIfDefBlockTmpActive
    '', '', '',  // ahaIfDefNodeInactive, ahaIfDefNodeActive, ahaIfDefNodeTmpActive
    '', '', '', '', '', // ahaIdentComplWindow, ahaIdentComplWindowBorder, ahaIdentComplRecent, ahaIdentComplWindowSelection, ahaIdentComplWindowHighlight
    '', '', '',     // ahaIdentComplWindowEntryVar, ahaIdentComplWindowEntryType, ahaIdentComplWindowEntryConst,
    '', '', '', '', // ahaIdentComplWindowEntryProc, ahaIdentComplWindowEntryFunc, ahaIdentComplWindowEntryMethAbstract, ahaIdentComplWindowEntryMethodLowVis,
    '', '', '',     // ahaIdentComplWindowEntryProp, ahaIdentComplWindowEntryIdent, ahaIdentComplWindowEntryLabel,
    '', '', '',     // ahaIdentComplWindowEntryEnum, ahaIdentComplWindowEntryUnit, ahaIdentComplWindowEntryNameSpace,
    '', '', '',     // ahaIdentComplWindowEntryText, ahaIdentComplWindowEntryTempl, ahaIdentComplWindowEntryKeyword,
    '',             // ahaIdentComplWindowEntryUnknown,
    '', '', '', '', '', '', '', '', '', '', // ahaOutlineLevel1Color..ahaOutlineLevel10Color
    '', '', '', // ahaWrapIndend, ahaWrapEol, ahaWrapSubLine
    ''         // ahaExternalLink
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
    { ahaCodeFoldingTreeCurrent } agnGutter,
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
    { ahaOverviewGutter }      agnGutter,
    { ahaGutterCurrentLine }   agnGutter,
    { ahaGutterNumberCurrentLine }   agnGutter,
    { ahaIfDefBlockInactive }  agnIfDef,
    { ahaIfDefBlockActive }    agnIfDef,
    { ahaIfDefBlockTmpActive } agnIfDef,
    { ahaIfDefNodeInactive }   agnIfDef,
    { ahaIfDefNodeActive }     agnIfDef,
    { ahaIfDefNodeTmpActive }  agnIfDef,
    { ahaIdentComplWindow }                 agnIdentComplWindow,
    { ahaIdentComplWindowBorder }           agnIdentComplWindow,
    { ahaIdentComplRecent }                 agnIdentComplWindow,
    { ahaIdentComplWindowSelection }        agnIdentComplWindow,
    { ahaIdentComplWindowHighlight }        agnIdentComplWindow,
    { ahaIdentComplWindowEntryVar}          agnIdentComplWindow,
    { ahaIdentComplWindowEntryType}         agnIdentComplWindow,
    { ahaIdentComplWindowEntryConst}        agnIdentComplWindow,
    { ahaIdentComplWindowEntryProc}         agnIdentComplWindow,
    { ahaIdentComplWindowEntryFunc}         agnIdentComplWindow,
    { ahaIdentComplWindowEntryMethAbstract} agnIdentComplWindow,
    { ahaIdentComplWindowEntryMethodLowVis} agnIdentComplWindow,
    { ahaIdentComplWindowEntryProp}         agnIdentComplWindow,
    { ahaIdentComplWindowEntryIdent}        agnIdentComplWindow,
    { ahaIdentComplWindowEntryLabel}        agnIdentComplWindow,
    { ahaIdentComplWindowEntryUnit}         agnIdentComplWindow,
    { ahaIdentComplWindowEntryEnum}         agnIdentComplWindow,
    { ahaIdentComplWindowEntryNameSpace}    agnIdentComplWindow,
    { ahaIdentComplWindowEntryText}         agnIdentComplWindow,
    { ahaIdentComplWindowEntryTempl}        agnIdentComplWindow,
    { ahaIdentComplWindowEntryKeyword}      agnIdentComplWindow,
    { ahaIdentComplWindowEntryUnknown}      agnIdentComplWindow,
    { ahaOutlineLevel1Color }  agnOutlineColors,
    { ahaOutlineLevel2Color }  agnOutlineColors,
    { ahaOutlineLevel3Color }  agnOutlineColors,
    { ahaOutlineLevel4Color }  agnOutlineColors,
    { ahaOutlineLevel5Color }  agnOutlineColors,
    { ahaOutlineLevel6Color }  agnOutlineColors,
    { ahaOutlineLevel7Color }  agnOutlineColors,
    { ahaOutlineLevel8Color }  agnOutlineColors,
    { ahaOutlineLevel9Color }  agnOutlineColors,
    { ahaOutlineLevel10Color } agnOutlineColors,
    { ahaWrapIndend  } agnWrap,
    { ahaWrapEol     } agnWrap,
    { ahaWrapSubLine } agnWrap,
    { ahaExternalLink } agnText

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
    { ahaCodeFoldingTreeCurrent } [hafForeColor, hafFrameColor],
    { ahaHighlightWord }      [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupAllOverview],
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
    { ahaOverviewGutter }     [hafBackColor, hafForeColor, hafFrameColor],
    { ahaGutterCurrentLine }  [hafBackColor],
    { ahaGutterNumberCurrentLine }  [hafBackColor, hafForeColor, hafFrameColor, hafStyle],
    { ahaIfDefBlockInactive } [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIfDefBlockActive }   [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIfDefBlockTmpActive }[hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIfDefNodeInactive }  [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIfDefNodeActive }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIfDefNodeTmpActive } [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaIdentComplWindow }   [hafBackColor, hafForeColor],
    { ahaIdentComplWindowBorder }    [hafForeColor],
    { ahaIdentComplRecent}    [hafForeColor],
    { ahaIdentComplWindowSelection } [hafBackColor, hafForeColor],
    { ahaIdentComplWindowHighlight }         [hafForeColor],
    { ahaIdentComplWindowEntryVar}           [hafForeColor],
    { ahaIdentComplWindowEntryType}          [hafForeColor],
    { ahaIdentComplWindowEntryConst}         [hafForeColor],
    { ahaIdentComplWindowEntryProc}          [hafForeColor],
    { ahaIdentComplWindowEntryFunc}          [hafForeColor],
    { ahaIdentComplWindowEntryMethAbstract}  [hafForeColor],
    { ahaIdentComplWindowEntryMethodLowVis}  [hafForeColor],
    { ahaIdentComplWindowEntryProp}          [hafForeColor],
    { ahaIdentComplWindowEntryIdent}         [hafForeColor],
    { ahaIdentComplWindowEntryLabel}         [hafForeColor],
    { ahaIdentComplWindowEntryUnit}          [hafForeColor],
    { ahaIdentComplWindowEntryEnum}          [hafForeColor],
    { ahaIdentComplWindowEntryNameSpace}     [hafForeColor],
    { ahaIdentComplWindowEntryText}          [hafForeColor],
    { ahaIdentComplWindowEntryTempl}         [hafForeColor],
    { ahaIdentComplWindowEntryKeyword}       [hafForeColor],
    { ahaIdentComplWindowEntryUnknown}       [hafForeColor],
    { ahaFoldLevel1Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel2Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel3Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel4Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel5Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel6Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel7Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel8Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel9Color }    [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaFoldLevel10Color }   [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask, hafMarkupFoldColor],
    { ahaWrapIndend  } [hafBackColor, hafFrameColor {, hafAlpha, hafPrior}],
    { ahaWrapEol     } [hafBackColor {, hafAlpha, hafPrior}],
    { ahaWrapSubLine } [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask],
    { ahaExternalLink }  [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask]
  );

const
  AdditionalHighlightAttrWithPastEolFeature = TAdditionalHilightAttributes([ahaTextBlock]);
  AdditionalHighlightAttrWithPastEolEnabled = TAdditionalHilightAttributes([ahaTextBlock]);

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

  TColorSchemeAttribute = class(TSynHighlighterLazCustomPasAttribute, IColorSchemeAttribute)
  private
    FAttrFeatures: TColorSchemeAttributeFeatures;
    FDefaultSynFeatures: TLazTextAttributeFeatures;
    FGroup: TAhaGroupName;
    FRegisteredGroup: integer;
    FMarkupFoldLineAlpha: Byte;
    FMarkupFoldLineColor: TColor;
    FMarkupFoldLineStyle: TSynLineStyle;
    FMarkupAllOverviewColor: TColor;
    FOwner: TColorSchemeLanguage;
    FAlreadyGotSchemeGlobal: Boolean;
    FSchemeGlobalCache: TColorSchemeAttribute;
    FUseSchemeGlobals: Boolean;
    function GetGroupName: String;
    function GetIsUsingSchemeGlobals: Boolean;
    procedure SetMarkupAllOverviewColor(AValue: TColor);
    procedure SetMarkupFoldLineAlpha(AValue: Byte);
    procedure SetMarkupFoldLineColor(AValue: TColor);
    procedure SetMarkupFoldLineStyle(AValue: TSynLineStyle);
    // IColorSchemeAttribute
    procedure ApplyTo(aDest: TObject);
  protected
    procedure Init; override;
    procedure AssignColorsFrom(ASource: TLazCustomEditTextAttribute); override;
  public
    constructor Create(ASchemeLang: TColorSchemeLanguage; attribName: PString;
                       const aStoredName: String = '');
    function IsEnabled: boolean; override;
    procedure ApplyTo(aDest: TLazEditTextAttribute; aDefault: TColorSchemeAttribute = nil);
    procedure Assign(Src: TPersistent); override;
    function Equals(Other: TColorSchemeAttribute): Boolean; reintroduce;
    function GetStoredValuesForAttrib: TColorSchemeAttribute; // The IDE default colors from the resources
    function GetSchemeGlobal: TColorSchemeAttribute;
    procedure LoadFromXml(aXMLConfig: TRttiXMLConfig; const aPath: String;
                          Defaults: TColorSchemeAttribute; Version: Integer);
    procedure SaveToXml(aXMLConfig: TRttiXMLConfig; const aPath: String;
                        Defaults: TColorSchemeAttribute);
    property Group: TAhaGroupName read FGroup write FGroup;
    property GroupName: String read GetGroupName;
    property IsUsingSchemeGlobals: Boolean read GetIsUsingSchemeGlobals;
    property AttrFeatures: TColorSchemeAttributeFeatures read FAttrFeatures write FAttrFeatures;
  published
    property UseSchemeGlobals: Boolean read FUseSchemeGlobals write FUseSchemeGlobals;
    // For markup fold color
    property MarkupFoldLineColor: TColor read FMarkupFoldLineColor write SetMarkupFoldLineColor default clNone; // clDefault will take Color[].Frame or Color[].Foreground
    property MarkupFoldLineStyle: TSynLineStyle read FMarkupFoldLineStyle write SetMarkupFoldLineStyle default slsSolid;
    property MarkupFoldLineAlpha: Byte read FMarkupFoldLineAlpha write SetMarkupFoldLineAlpha default 0;
    // For overview gutter (MarkupHighlightAll)
    property MarkupAllOverviewColor: TColor read FMarkupAllOverviewColor write SetMarkupAllOverviewColor default clNone;
  end;

  { TColorSchemeLanguage }

  TColorSchemeLanguage = class(TObject, IColorSchemeLanguage)
  private
    FDefaultAttribute: TColorSchemeAttribute;
    FAttributes: TQuickStringlist; // TColorSchemeAttribute
    FHighlighter: TSynCustomHighlighter;
    FIdeHighlighterID: TIdeSyntaxHighlighterID;
    FOwner: TColorScheme;
    FLanguageName: String;
    FIsSchemeDefault: Boolean;
    FFormatVersion: integer;
    function GetAttribute(const Index: String): TColorSchemeAttribute;
    function GetAttributeAtPos(Index: Integer): TColorSchemeAttribute;
    function GetAttributeByEnum(Index: TAdditionalHilightAttribute): TColorSchemeAttribute;
    function GetName: String;
    function DoesSupportGroup(AGroup: TAhaGroupName): boolean;
    function GetSupportsFileExt: Boolean;
    // IColorSchemeLanguage
    function GetAttributeIntf(AnIndex: integer): IColorSchemeAttribute;
    function GetAttributeIntf(const AStoredName: string): IColorSchemeAttribute;
  public
    constructor Create(AGroup: TColorScheme; AIdeHighlighterID: TIdeSyntaxHighlighterID;
      IsSchemeDefault: Boolean);
    constructor CreateWithDefColor(AGroup: TColorScheme; AIdeHighlighterID: TIdeSyntaxHighlighterID;
      IsSchemeDefault: Boolean);
    constructor CreateFromXml(AGroup: TColorScheme; AIdeHighlighterID: TIdeSyntaxHighlighterID;
      aXMLConfig: TRttiXMLConfig; const aPath: String; IsSchemeDefault: Boolean;
      aPascalScheme: TColorSchemeLanguage = nil; MappedAttributes: TStringList = nil);
    destructor  Destroy; override;
    procedure Clear;
    procedure Assign(Src: TColorSchemeLanguage); reintroduce;
    function Equals(Other: TColorSchemeLanguage): Boolean; reintroduce;
    function GetStoredValuesForLanguage: TColorSchemeLanguage; // The IDE default colors from the resources
    function IndexOfAttr(AnAttr: TColorSchemeAttribute): Integer;
    procedure LoadFromXml(aXMLConfig: TRttiXMLConfig; const aPath: String;
      Defaults: TColorSchemeLanguage; ColorVersion: Integer; const aOldPath: String = '');
    procedure SaveToXml(aXMLConfig: TRttiXMLConfig; aPath: String;
      Defaults: TColorSchemeLanguage);
    procedure ApplyTo(ASynEdit: TSynEdit); // Write markup, etc
    procedure ApplyTo(AHLighter: TSynCustomHighlighter);
    function  AttributeCount: Integer;
    property  Name: String read GetName;
    property  IdeHighlighterID: TIdeSyntaxHighlighterID read FIdeHighlighterID;
    property  LanguageName: String read FLanguageName;
    property  Attribute[const Index: String]: TColorSchemeAttribute read GetAttribute;
    property  AttributeByEnum[Index: TAdditionalHilightAttribute]: TColorSchemeAttribute
              read GetAttributeByEnum;
    property  AttributeAtPos[Index: Integer]: TColorSchemeAttribute read GetAttributeAtPos;
    property  DefaultAttribute: TColorSchemeAttribute read FDefaultAttribute;
    property  SharedHighlighter: TSynCustomHighlighter read FHighlighter;
    property  SupportsFileExt: Boolean read GetSupportsFileExt;
  end;

  { TColorScheme }

  TColorScheme = class(TObject, IColorScheme)
  private type
    TColorSchemesMap = specialize TFPGMapObject<string, TColorSchemeLanguage>;
  private
    FName: String;
    FColorSchemes: TColorSchemesMap; //Array of TColorSchemeLanguage;
    FDefaultColors: TColorSchemeLanguage;
    function GetColorScheme(Index: integer): TColorSchemeLanguage;
    function GetColorSchemeBySynHl(Index: TSynCustomHighlighter): TColorSchemeLanguage;
    // IColorScheme
    function GetName: String;
    function GetLanguage(AnIndex: Integer): IColorSchemeLanguage;
    function GetLanguageForHighlighter(AnHiglighter: TObject {TSynCustomHighlighter}): IColorSchemeLanguage;
    function GetLanguageForHighlighter(AnHighlighterId: TIdeSyntaxHighlighterID): IColorSchemeLanguage;
  public
    constructor Create(const AName: String);
    constructor CreateFromXml(aXMLConfig: TRttiXMLConfig; const AName, aPath: String);
    destructor  Destroy; override;
    procedure Assign(Src: TColorScheme); reintroduce;
    function Count: integer;
    function GetStoredValuesForScheme: TColorScheme; // The IDE default colors from the resources
    procedure LoadFromXml(aXMLConfig: TRttiXMLConfig; const aPath: String;
                          Defaults: TColorScheme; const aOldPath: String = '');
    procedure SaveToXml(aXMLConfig: TRttiXMLConfig; const aPath: String; Defaults: TColorScheme);
    property  Name: string read FName;
    property  DefaultColors: TColorSchemeLanguage read FDefaultColors;
    property  ColorScheme[Index: integer]: TColorSchemeLanguage read GetColorScheme;
    property  ColorSchemeBySynHl[Index: TSynCustomHighlighter]: TColorSchemeLanguage read GetColorSchemeBySynHl;
  end;

  { TColorSchemeFactory }

  TColorSchemeFactory = class(TObject, IColorSchemeList)
  private
    FMappings: TQuickStringlist; // TColorScheme
    function GetColorSchemeGroup(const Index: String): TColorScheme;
    function GetColorSchemeGroupAtPos(Index: Integer): TColorScheme;
    // IColorSchemeList
    function GetScheme(AnIndex: Integer): IColorScheme;
    function GetScheme(AName: String): IColorScheme;
    function GetCurrentSchemeForHighlighter(AnHiglighter: TObject {TSynCustomHighlighter}): IColorScheme;
    function GetCurrentSchemeForHighlighter(AnHighlighterId: TIdeSyntaxHighlighterID): IColorScheme;
    procedure RegisterChangedHandler(AnHandler: TNotifyEvent);
    procedure UnregisterChangedHandler(AnHandler: TNotifyEvent);
    function RegisterAttributeGroup(AName: PString): integer; // pointer to resource string
    procedure InternalAddAttribute(AnAttrGroup: integer; AnHighlighterId: TIdeSyntaxHighlighterID; AStoredName: String; AName: PString;  AFeatures: TColorSchemeAttributeFeatures; ADefaults: TObject = nil);
    procedure AddAttribute(AnAttrGroup: integer; AnHighlighterId: TIdeSyntaxHighlighterID; AStoredName: String; AName: PString;  AFeatures: TColorSchemeAttributeFeatures; ADefaults: TObject = nil);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Clear;
    function Count: integer;
    procedure Assign(Src: TColorSchemeFactory); reintroduce;
    procedure LoadFromXml(aXMLConfig: TRttiXMLConfig; const aPath: String;
      Defaults: TColorSchemeFactory; const aOldPath: String = '');
    procedure SaveToXml(aXMLConfig: TRttiXMLConfig; const aPath: String;
      Defaults: TColorSchemeFactory);
    procedure RegisterScheme(aXMLConfig: TRttiXMLConfig; AName: String; const  aPath: String);
    procedure GetRegisteredSchemes(AList: TStrings);
    property  ColorSchemeGroup[const Index: String]: TColorScheme read GetColorSchemeGroup;
    property  ColorSchemeGroupAtPos[Index: Integer]: TColorScheme read GetColorSchemeGroupAtPos;
  end;

  { TIDESynTextSyn }

  TIDESynTextSyn = class(TSynCustomHighlighter)
  private
//    fTextAttri: TSynHighlighterAttributes;
    FPos: Integer;
  protected
    function GetDefaultAttribute({%H-}Index: integer): TSynHighlighterAttributes; override;
  public
    class function GetLanguageName: string; override;
    procedure ResetRange; override;

    procedure InitForScanningLine; override;
    constructor Create(AOwner: TComponent); override;
    function GetEol: Boolean; override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TLazEditTextAttribute; override;
    function GetTokenPos: Integer; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
  end;

type

  TEditorOptionsDividerInfo = record
    Name: String;      // Name for display
    Xml: String;       // Name for XML
    BoolOpt: Boolean;  // Checkbox only
    MaxLevel: Integer;
  end;
  PEditorOptionsDividerInfoList = ^TEditorOptionsDividerInfo;

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
  EditorOptionsDividerDefaults: array[TLazSyntaxHighlighter] of TEditorOptionsDividerRecord =
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
  PEditorOptionsFoldInfoList = ^TEditorOptionsFoldInfo;

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
  EditorOptionsFoldInfoPas: Array [0..29] of TEditorOptionsFoldInfo
  = (
      (Name:  dlgFoldPasProcedure;     Xml:     'Procedure';
       Index: ord(cfbtProcedure);    Enabled: True),
      (Name:  dlgFoldPasAnonProcedure; Xml:     'AnonymousProcedure';
       Index: ord(cfbtAnonymousProcedure);    Enabled: True),
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
      (Name:  dlgFoldPasRecordCase;    Xml:     'RecordCase';
       Index: ord(cfbtRecordCase);   Enabled: False),
      (Name:  dlgFoldPasRecordCaseSect;    Xml:     'RecordCaseSection';
       Index: ord(cfbtRecordCaseSection);   Enabled: False),

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
  EditorOptionsFoldDefaults: array[TLazSyntaxHighlighter] of TEditorOptionsFoldRecord =
    ( (Count:  0; HasMarkup: False; Info: nil), // none
      (Count:  0; HasMarkup: False; Info: nil), // text
      (Count: 30; HasMarkup: True; Info: @EditorOptionsFoldInfoPas[0]), // Freepas
      (Count: 30; HasMarkup: True; Info: @EditorOptionsFoldInfoPas[0]), // pas
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
  EditorOptsFormatVersion = 13;
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
    * Changes in Version 13:
         CtrlMiddleTabClickClosesOthers was replaced by MiddleTabClickClosesOthersModifier
    * Changes in Version 14:
         Introduced "Modifier" to pascal. Prior versions must load "Reserved word"
  }
  EditorMouseOptsFormatVersion = 1;
  { * Changes in Version 6:
      - MouseWheel is nov configurable
  }

const
  LazSyntaxHighlighterClasses: array[TLazSyntaxHighlighter] of
    TCustomSynClass =
    (nil, TIDESynTextSyn, TIDESynFreePasSyn, TIDESynPasSyn, TSynLFMSyn, TSynXMLSyn,
    TSynHTMLSyn, TSynCPPSyn, TSynPerlSyn, TSynJavaSyn, TSynUNIXShellScriptSyn,
    TSynPythonSyn, TSynPHPSyn, TSynSQLSyn,TSynCssSyn, TSynJScriptSyn, TSynDiffSyn,
    TSynBatSyn, TSynIniSyn, TSynPoSyn, TSynPikeSyn) deprecated 'for internal use only';

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
    ) deprecated 'for internal use only';

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
  public
    SynInstance: TSrcIDEHighlighter;
    TheType:  TLazSyntaxHighlighter;
    FileExtensions: String; // divided by semicolon, e.g. 'pas;pp;inc'
    DefaultFileExtensions: string;
    SampleSource: String;
    AddAttrSampleLines: array[TAdditionalHilightAttribute] of Integer; // first line = 1
//    MappedAttributes: TStringList; // map attributes to pascal
    DefaultCommentType: TCommentType;
    CaretXY: TPoint;
    constructor Create;
    destructor Destroy; override;
    function CreateNewSynInstance: TSrcIDEHighlighter; virtual;
    function GetDefaultFilextension: String;
    procedure SetBothFilextensions(const Extensions: string);
    function SampleLineToAddAttr(Line: Integer): TAdditionalHilightAttribute;
  end;

  { TEditOptLanguageTextMateInfo }

  TEditOptLanguageTextMateInfo = class(TEditOptLanguageInfo)
  public
    FileName: String;
    function CreateNewSynInstance: TSrcIDEHighlighter; override;
  end;

  { TEditOptLangList }

  TEditOptLangList = class(TList, TIdeSyntaxHighlighterList)
  private
    function DoGetTMLGrammar(Sender: TTextMateGrammar; AScopeName: string
      ): TTextMateGrammar;
    function GetLazSyntaxHighlighterType(AnId: TIdeSyntaxHighlighterID): TLazSyntaxHighlighter; deprecated '(to be removed in 4.99) -- Only temporary for StrToLazSyntaxHighlighter';
    function GetInfos(Index: Integer): TEditOptLanguageInfo;
    function GetSharedSynInstances(AnID: TIdeSyntaxHighlighterID): TSrcIDEHighlighter;
  public
    function GetIdForFileExtension(Ext: String): TIdeSyntaxHighlighterID;
    function GetIdForFileExtension(Ext: String; ADelphiMode: boolean): TIdeSyntaxHighlighterID;
    function GetIdForName(AName: String): TIdeSyntaxHighlighterID;
    function GetCaptions(AnID: TIdeSyntaxHighlighterID): String;
    function GetNames(AnID: TIdeSyntaxHighlighterID): String;
    function GetSharedInstances(AnID: TIdeSyntaxHighlighterID): TObject;
    function GetSynHlClasses(AnID: TIdeSyntaxHighlighterID): TClass;

    function GetIdForLazSyntaxHighlighter(AnHighlighterType: TLazSyntaxHighlighter): TIdeSyntaxHighlighterID;
  public
    procedure Init; // called by ColorSchemeFactory
    destructor Destroy; override;

    procedure Clear; reintroduce;
    procedure Delete(Index: Integer); reintroduce;
    procedure Move(CurIndex, NewIndex: Integer); reintroduce;
    procedure Exchange(Index1, Index2: Integer); reintroduce;
    function Extract(item: Pointer): Pointer; reintroduce;

    function GetNewSynInstance(AnID: TIdeSyntaxHighlighterID): TSrcIDEHighlighter;
    function FindByName(const Name: String): Integer;
    function GetDefaultFilextension(AnId: TIdeSyntaxHighlighterID): String;
    function FindByType(AType: TLazSyntaxHighlighter): Integer;                 deprecated '(to be removed in 4.99)';
    function GetDefaultFilextension(AType: TLazSyntaxHighlighter): String;      deprecated '(to be removed in 4.99)';
    function GetInfoByType(AType: TLazSyntaxHighlighter): TEditOptLanguageInfo; deprecated '(to be removed in 4.99)';
    property Items[Index: Integer]: TEditOptLanguageInfo read GetInfos; default;

    property Captions       [AnID: TIdeSyntaxHighlighterID]: String  read GetCaptions;
    property Names          [AnID: TIdeSyntaxHighlighterID]: String  read GetNames;
    property SynHlClasses   [AnID: TIdeSyntaxHighlighterID]: TClass  read GetSynHlClasses;     // class of TSynCustomHighlighter
deprecated 'NONOONONONONONOONON only create ONE ????';
    property SharedInstances[AnID: TIdeSyntaxHighlighterID]: TObject read GetSharedInstances; // TSynCustomHighlighter
    property SharedSynInstances[AnID: TIdeSyntaxHighlighterID]: TSrcIDEHighlighter read GetSharedSynInstances; // TSynCustomHighlighter
  end;

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
    mbaOpenExternalLink,
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
    FDeclarationJumpIncludesExtLink: Boolean;
    FGutterActionsChanges: TSynEditMouseActions;
    FMainActions, FSelActions, FTextActions: TSynEditMouseActions;
    FSelectOnLineNumbers: Boolean;
    FName: String;
    FGutterActions: TSynEditMouseActions;
    FGutterActionsFold, FGutterActionsFoldExp, FGutterActionsFoldCol: TSynEditMouseActions;
    FGutterActionsLines: TSynEditMouseActions;
    FGutterActionsOverView, FGutterActionsOverViewMarks: TSynEditMouseActions;
    FExtLinkActions: TSynEditMouseActions;
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
    FAltCtrlWheel: TMouseOptWheelAction;
    FShiftWheel: TMouseOptWheelAction;
    FShiftAltWheel: TMouseOptWheelAction;
    FShiftCtrlWheel: TMouseOptWheelAction;
    FShiftAltCtrlWheel: TMouseOptWheelAction;

    FHorizWheel: TMouseOptWheelAction;
    FAltHorizWheel: TMouseOptWheelAction;
    FCtrlHorizWheel: TMouseOptWheelAction;
    FAltCtrlHorizWheel: TMouseOptWheelAction;
    FShiftHorizWheel: TMouseOptWheelAction;
    FShiftAltHorizWheel: TMouseOptWheelAction;
    FShiftCtrlHorizWheel: TMouseOptWheelAction;
    FShiftAltCtrlHorizWheel: TMouseOptWheelAction;

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
    Procedure LoadFromXmlMouseAct(aXMLConfig: TRttiXMLConfig; Path: String; MActions: TSynEditMouseActions; AShowError: Boolean = False);
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
    property ExtLinkActions: TSynEditMouseActions read FExtLinkActions;
  published
    property GutterLeft: TMouseOptGutterLeftType read FGutterLeft write FGutterLeft
             default moglUpClickAndSelect;
    property SelectOnLineNumbers: Boolean read FSelectOnLineNumbers write FSelectOnLineNumbers
             default True;
    property TextDrag: Boolean read FTextDrag write FTextDrag
             default True;
    property TextRightMoveCaret: Boolean read FTextRightMoveCaret  write FTextRightMoveCaret
             default False;
    property DeclarationJumpIncludesExtLink: Boolean read FDeclarationJumpIncludesExtLink write FDeclarationJumpIncludesExtLink
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
    //
    property HorizWheel: TMouseOptWheelAction read FHorizWheel write FHorizWheel
             default mwaScrollHoriz;
    property CtrlHorizWheel: TMouseOptWheelAction read FCtrlHorizWheel write FCtrlHorizWheel
             default mwaNone;
    property AltHorizWheel: TMouseOptWheelAction read FAltHorizWheel write FAltHorizWheel
             default mwaScrollHorizPageLessOne;
    property ShiftHorizWheel: TMouseOptWheelAction read FShiftHorizWheel write FShiftHorizWheel
             default mwaScrollHorizSingleLine;
    property ShiftAltHorizWheel: TMouseOptWheelAction read FShiftAltHorizWheel write FShiftAltHorizWheel
             default mwaNone;
    property ShiftCtrlHorizWheel: TMouseOptWheelAction read FShiftCtrlHorizWheel write FShiftCtrlHorizWheel
             default mwaNone;
    property AltCtrlHorizWheel: TMouseOptWheelAction read FAltCtrlHorizWheel write FAltCtrlHorizWheel
             default mwaNone;
    property ShiftAltCtrlHorizWheel: TMouseOptWheelAction read FShiftAltCtrlHorizWheel write FShiftAltCtrlHorizWheel
             default mwaNone;

    // the flag below is set by CalcCustomSavedActions
    property CustomSavedActions: Boolean read FCustomSavedActions write FCustomSavedActions;
    property SelectedUserScheme: String read FSelectedUserScheme write SetSelectedUserScheme;
    property Version : Integer read FVersion write FVersion;
  end;

  { TEditorMouseOptionPresets }

  TEditorMouseOptionPresets = class
  public
    constructor Create;
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
    property Caption: String read FCaption write FCaption;
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

  TEditorOptsScrollPastEolMode = (optScrollFixed, optScrollPage, optScrollNone);

  TEditorSynGutterOptsLineColor = (glcOff, glcOn, glcLineNum);

  { TEditorSynGutterOptions }

  TEditorSynGutterOptions = class(TPersistent)
  private
    FDefaults: TEditorSynGutterOptions;
    FGClass: TSynGutterPartBaseClass;
    FIndex: integer;
    FOffsetLeft: integer;
    FOffsetRight: integer;
    FShowLineColor: TEditorSynGutterOptsLineColor;
    FVisible: boolean;
    FWidth: integer;
  protected
    constructor DoCreate(AIdx: Integer; AGClass: TSynGutterPartBaseClass);
  public
    constructor Create(AIdx: Integer; AGClass: TSynGutterPartBaseClass);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ApplyTo(AGutterPart: TSynGutterPartBase);
    procedure ApplyLineColorTo(AGutterPart: TSynGutterPartBase; Attri, NumAttri: TColorSchemeAttribute);
    procedure ApplyIndexTo(AGutterPart: TSynGutterPartBase);

    property Defaults: TEditorSynGutterOptions read FDefaults;
    property GClass: TSynGutterPartBaseClass read FGClass;
  published
    property Visible: boolean read FVisible write FVisible;
    property Index: integer read FIndex write FIndex;
    property Width: integer read FWidth write FWidth;
    property OffsetLeft: integer read FOffsetLeft write FOffsetLeft;
    property OffsetRight: integer read FOffsetRight write FOffsetRight;
    property ShowLineColor: TEditorSynGutterOptsLineColor read FShowLineColor write FShowLineColor;
  end;

  { TEditorSynGutterOptionsList }

  TEditorSynGutterOptionsList = class(specialize TFPGObjectList<TEditorSynGutterOptions>)
  private
    function GetByClass(AIndex: TSynGutterPartBaseClass): TEditorSynGutterOptions;
  public
    procedure Assign(Source: TEditorSynGutterOptionsList);
    procedure AssignItems(Source: TEditorSynGutterOptionsList);
    procedure Sort;
    property ByClass[AIndex: TSynGutterPartBaseClass]: TEditorSynGutterOptions read GetByClass;
  end;

  TProcHeaderNameMode = (
    pnmGenericOnly,
    pnmGenericAndProcName,
    pnmProcNameOnly,
    pnmPlain
  );
  { TEditorOptionsBase }

  TEditorOptionsBase = class(TIDEEditorOptions)
  private
    fDbgHintUseBackendDebugConverter: Boolean;
    FElasticTabs: Boolean;
    fElasticTabsMinWidth: Integer;
    FTabFont: String;
    FTabFontDisableAntialiasing: Boolean;
    FTabFontSize: Integer;
    {$IFDEF WinIME}
    FUseMinimumIme: Boolean;
    {$ENDIF}
    fExportHtmlWithBackground: Boolean;
    // General options
    fMultiLineTab: Boolean;
    fTabPosition: TTabPosition;
    // Display options
    FGutterPartMarks: TEditorSynGutterOptions;
    FGutterPartChange: TEditorSynGutterOptions;
    FGutterPartFold: TEditorSynGutterOptions;
    FGutterPartLine: TEditorSynGutterOptions;
    FGutterPartOver: TEditorSynGutterOptions;
    FGutterPartSep: TEditorSynGutterOptions;
    FGutterPartList: TEditorSynGutterOptionsList;
    FGutterRightPartList: TEditorSynGutterOptionsList;
    fTopInfoView: boolean;
    // Code tools options
    fDbgHintAutoTypeCastClass: Boolean;
    // Code Folding
    fReverseFoldPopUpOrder: Boolean;
    // Wordwrap
    FWordWrapCaretWrapPos: TLazSynEditWrapCaretPos;
    FWordWrapEnabled: Boolean;
    FWordWrapHLList: TStringList;
    FWordWrapFixedWidth: boolean;
    FWordWrapForceHomeEnd: Boolean;
    FWordWrapIndent: Integer;
    FWordWrapIndentMax: Integer;
    FWordWrapIndentMaxRel: Integer;
    FWordWrapIndentMin: Integer;
    FWordWrapIndentUseOffset: boolean;
    FWordWrapMaxWidth: Integer;
    FWordWrapMinWidth: Integer;

    fUseTabHistory: Boolean;

    fMultiCaretOnColumnSelect: Boolean;
    fMultiCaretDefaultMode: TSynPluginMultiCaretDefaultMode;
    fMultiCaretDeleteSkipLineBreak: Boolean;
    fMultiCaretDefaultColumnSelectMode: TSynPluginMultiCaretDefaultMode;

    // Highlighter Pas
    fPasExtendedKeywordsMode: Boolean;
    fPasStringKeywordMode: TSynPasStringMode;
    FCaseLabelAttriMatchesElseOtherwise: Boolean;
    FDeclaredTypeAttributeMode: TSynPasTypeAttributeMode;
    FDeclaredValueAttributeMachesStringNum: Boolean;
    FDeclaredValueAttributeMode: TSynPasTypeAttributeMode;
    FGenericParamAttrMode: TSynPasTypeAttributeMode;
    FProcHeaderNameDeclMode: TProcHeaderNameMode;
    FProcHeaderNameImplMode: TProcHeaderNameMode;
    // Multi window
    fCtrlMiddleTabClickClosesOthers: Boolean;
    fMiddleTabClickClosesOthersModifier: TShiftState;
    fMiddleTabClickClosesToRightModifier: TShiftState;
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
    FStringAlignMax: Integer;
    FStringAlignPattern: String;
    // Auto brace
    FAutoBraceFilterClose: string;
    FAutoBraceFilterOpen: string;
    FAutoBraceModes: TSynPluginAutoBraceModes;
    // Scroll
    FScrollOnEditLeftOptions: TSynScrollOnEditLeftOptions;
    FScrollOnEditRightOptions: TSynScrollOnEditRightOptions;
    FScrollPastEolMode: TEditorOptsScrollPastEolMode;
    procedure InitForRttiXmlConf;
  protected
    function GetTabPosition: TTabPosition; override;
  public
    constructor Create;
    destructor Destroy; override;

    // Display
    property GutterPartMarks: TEditorSynGutterOptions read FGutterPartMarks;
    property GutterPartLine: TEditorSynGutterOptions read FGutterPartLine;
    property GutterPartChange: TEditorSynGutterOptions read FGutterPartChange;
    property GutterPartSep: TEditorSynGutterOptions read FGutterPartSep;
    property GutterPartFold: TEditorSynGutterOptions read FGutterPartFold;
    property GutterPartOver: TEditorSynGutterOptions read FGutterPartOver;
    property GutterPartList: TEditorSynGutterOptionsList read FGutterPartList;
    property GutterRightPartList: TEditorSynGutterOptionsList read FGutterRightPartList;
    property WordWrapHLList: TStringList read FWordWrapHLList;
  published { use RTTIConf}
    // general options
    property MultiLineTab: Boolean read fMultiLineTab write fMultiLineTab default False;
    property TabPosition: TTabPosition read fTabPosition write fTabPosition default tpTop;
    // Code Tools options
    property DbgHintAutoTypeCastClass: Boolean              // declaration hints
      read fDbgHintAutoTypeCastClass write fDbgHintAutoTypeCastClass default True;
    property DbgHintUseBackendDebugConverter: Boolean              // declaration hints
      read fDbgHintUseBackendDebugConverter write fDbgHintUseBackendDebugConverter default True;
    // General - Misc
    {$IFDEF WinIME}
    property UseMinimumIme: Boolean read FUseMinimumIme write FUseMinimumIme default False;
    {$ENDIF}
    property ExportHtmlWithBackground: Boolean
      read fExportHtmlWithBackground write fExportHtmlWithBackground default False;
    // Display

    property TopInfoView: boolean read fTopInfoView write fTopInfoView default True;
    // Code Folding
    property ReverseFoldPopUpOrder: Boolean
      read fReverseFoldPopUpOrder write fReverseFoldPopUpOrder default True;

    // wordwrap
    property WordWrapEnabled: Boolean read FWordWrapEnabled write FWordWrapEnabled;
    property WordWrapCaretWrapPos: TLazSynEditWrapCaretPos read FWordWrapCaretWrapPos write FWordWrapCaretWrapPos;
    property WordWrapForceHomeEnd: Boolean read FWordWrapForceHomeEnd write FWordWrapForceHomeEnd;
    property WordWrapMinWidth: Integer read FWordWrapMinWidth write FWordWrapMinWidth default 10;
    property WordWrapMaxWidth: Integer read FWordWrapMaxWidth write FWordWrapMaxWidth default 0;
    property WordWrapFixedWidth: boolean read FWordWrapFixedWidth write FWordWrapFixedWidth default False;

    property WordWrapIndent: Integer read FWordWrapIndent write FWordWrapIndent default 0;
    property WordWrapIndentUseOffset: boolean read FWordWrapIndentUseOffset write FWordWrapIndentUseOffset default True;
    property WordWrapIndentMin: Integer read FWordWrapIndentMin write FWordWrapIndentMin default 0;
    property WordWrapIndentMax: Integer read FWordWrapIndentMax write FWordWrapIndentMax default 8;
    property WordWrapIndentMaxRel: Integer read FWordWrapIndentMaxRel write FWordWrapIndentMaxRel  default 0;

    property UseTabHistory: Boolean read fUseTabHistory write fUseTabHistory;

    property MultiCaretOnColumnSelect: Boolean
      read fMultiCaretOnColumnSelect write fMultiCaretOnColumnSelect default True;
    property MultiCaretDefaultMode: TSynPluginMultiCaretDefaultMode
      read fMultiCaretDefaultMode write fMultiCaretDefaultMode default mcmMoveAllCarets;
    property MultiCaretDeleteSkipLineBreak: Boolean
      read fMultiCaretDeleteSkipLineBreak write fMultiCaretDeleteSkipLineBreak default False;
    property MultiCaretDefaultColumnSelectMode: TSynPluginMultiCaretDefaultMode
      read fMultiCaretDefaultColumnSelectMode write fMultiCaretDefaultColumnSelectMode default mcmCancelOnCaretMove;
    // Highlighter Pas
    property PasExtendedKeywordsMode: Boolean
      read fPasExtendedKeywordsMode write fPasExtendedKeywordsMode default False;
    property PasStringKeywordMode: TSynPasStringMode
      read fPasStringKeywordMode write fPasStringKeywordMode default spsmDefault;
    property CaseLabelAttriMatchesElseOtherwise: Boolean
       read FCaseLabelAttriMatchesElseOtherwise write FCaseLabelAttriMatchesElseOtherwise default True;
    property DeclaredTypeAttributeMode: TSynPasTypeAttributeMode
       read FDeclaredTypeAttributeMode write FDeclaredTypeAttributeMode default tamIdentifierOnly;
    property DeclaredValueAttributeMode: TSynPasTypeAttributeMode
       read FDeclaredValueAttributeMode write FDeclaredValueAttributeMode default tamIdentifierOnly;
    property DeclaredValueAttributeMachesStringNum: Boolean
       read FDeclaredValueAttributeMachesStringNum write FDeclaredValueAttributeMachesStringNum default False;
    property GenericParamAttrMode: TSynPasTypeAttributeMode
       read FGenericParamAttrMode write FGenericParamAttrMode default tamIdentifierOnly;
    property ProcHeaderNameDeclMode: TProcHeaderNameMode
       read FProcHeaderNameDeclMode write FProcHeaderNameDeclMode default pnmGenericOnly;
    property ProcHeaderNameImplMode: TProcHeaderNameMode
       read FProcHeaderNameImplMode write FProcHeaderNameImplMode default pnmProcNameOnly;
    // Multi window
    property CtrlMiddleTabClickClosesOthers: Boolean
      read fCtrlMiddleTabClickClosesOthers write fCtrlMiddleTabClickClosesOthers stored False default True;
    property MiddleTabClickClosesOthersModifier: TShiftState
      read fMiddleTabClickClosesOthersModifier write fMiddleTabClickClosesOthersModifier default [ssCtrl];
    property MiddleTabClickClosesToRightModifier: TShiftState
      read fMiddleTabClickClosesToRightModifier write fMiddleTabClickClosesToRightModifier default [];
    property TabFont: String read FTabFont write FTabFont;
    property TabFontSize: Integer read FTabFontSize write FTabFontSize;
    property TabFontDisableAntialiasing: Boolean read FTabFontDisableAntialiasing
      write FTabFontDisableAntialiasing default DefaultEditorDisableAntiAliasing;
    // Comment Continue
    property AnsiCommentContinueEnabled: Boolean
      read FAnsiCommentContinueEnabled write FAnsiCommentContinueEnabled;
    property AnsiCommentMatch: String read FAnsiCommentMatch write FAnsiCommentMatch;
    property AnsiCommentPrefix: String read FAnsiCommentPrefix write FAnsiCommentPrefix;
    property AnsiCommentMatchMode: TSynCommentMatchMode
      read FAnsiCommentMatchMode write FAnsiCommentMatchMode;
    property AnsiIndentMode: TSynCommentIndentFlags
      read FAnsiIndentMode write FAnsiIndentMode;
    property AnsiIndentAlignMax: integer
      read FAnsiIndentAlignMax write FAnsiIndentAlignMax;
    property CurlyCommentContinueEnabled: Boolean
      read FCurlyCommentContinueEnabled write FCurlyCommentContinueEnabled;
    property CurlyCommentMatch: String read FCurlyCommentMatch write FCurlyCommentMatch;
    property CurlyCommentPrefix: String read FCurlyCommentPrefix write FCurlyCommentPrefix;
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
    property StringAlignPattern: String  read FStringAlignPattern write FStringAlignPattern;
    property StringAlignMax:     Integer read FStringAlignMax write FStringAlignMax;
    // Auto braces
    property AutoBraceModes: TSynPluginAutoBraceModes read FAutoBraceModes write FAutoBraceModes;
    property AutoBraceFilterOpen: string read FAutoBraceFilterOpen write FAutoBraceFilterOpen;
    property AutoBraceFilterClose: string read FAutoBraceFilterClose write FAutoBraceFilterClose;
    // Scroll
    property ScrollOnEditLeftOptions: TSynScrollOnEditLeftOptions
      read FScrollOnEditLeftOptions write FScrollOnEditLeftOptions;
    property ScrollOnEditRightOptions: TSynScrollOnEditRightOptions
      read FScrollOnEditRightOptions write FScrollOnEditRightOptions;
    property ScrollPastEolMode: TEditorOptsScrollPastEolMode read FScrollPastEolMode write FScrollPastEolMode default optScrollPage;
    // Tabs
    property ElasticTabs: Boolean read FElasticTabs write FElasticTabs default False;
    property ElasticTabsMinWidth: Integer read fElasticTabsMinWidth write fElasticTabsMinWidth default 1;
  end;

  { TEditorOptionsDefaults }

  TEditorOptionsDefaults = class(TEditorOptionsBase)
  private
  protected
  public
    constructor Create;
  end;

  { TEditorOptions }

  TEditorOptions = class(TEditorOptionsBase)
  private
    XMLConfig: TRttiXMLConfig;
    // General options
    fSynEditOptions: TSynEditorOptions;
    fSynEditOptions2: TSynEditorOptions2;
    fShowFileNameInCaption: Boolean;
    fShowTabCloseButtons: Boolean;
    fHideSingleTabInWindow: Boolean;
    fShowTabNumbers: Boolean;
    fUndoAfterSave: Boolean;
    fFindTextAtCursor: Boolean;
    fUseSyntaxHighlight: Boolean;
    fCopyWordAtCursorOnCopyNone: Boolean;
    fShowGutterHints: Boolean;
    fBlockIndent: Integer;
    fBlockTabIndent: Integer;
    fBlockIndentType: TSynBeautifierIndentType;
    fTrimSpaceType: TSynEditStringTrimmingType;
    fUndoLimit: Integer;
    fTabWidth:  Integer;
    fBracketHighlightStyle: TSynEditBracketHighlightStyle;
    // Display options
    fVisibleRightMargin: Boolean;
    fVisibleGutter: Boolean;
    fShowOnlyLineNumbersMultiplesOf: integer;
    fGutterWidth: Integer;
    fRightMargin: Integer;
    fEditorFont:  String;
    fEditorFontSize:   Integer;
    fExtraCharSpacing: Integer;
    fExtraLineSpacing: Integer;
    fDisableAntialiasing: Boolean;
    fDoNotWarnForFont: string;
    // Key Mappings
    fKeyMappingScheme: String;
    fKeyMap: TKeyCommandRelationList;
    // Mouse Mappings
    fUserMouseSettings: TEditorMouseOptions;
    fTempMouseSettings: TEditorMouseOptions;
    // Color options
    fUserColorSchemeGroup: TColorSchemeFactory;
    fUserDefinedColors: TEditorUserDefinedWordsList;
    // Markup Current Word
    fMarkupCurWordTime: Integer;
    fMarkupCurWordFullLen: Integer;
    fMarkupCurWordNoKeyword: Boolean;
    fMarkupCurWordTrim: Boolean;
    fMarkupCurWordNoTimer: Boolean;
    // Code Tools options
    fAutoBlockCompletion: Boolean;
    fAutoCodeParameters: Boolean;
    fAutoToolTipExprEval: Boolean;
    fAutoToolTipSymbTools: Boolean;
    fAutoDisplayFuncPrototypes: Boolean;
    fAutoDelayInMSec: Integer;
    fAutoHintDelayInMSec: Integer;
    fCodeTemplateFileNameRaw: String;
    fCTemplIndentToTokenStart: Boolean;
    fAutoRemoveEmptyMethods: Boolean;
    fCompletionLongLineHintInMSec: Integer;
    fCompletionLongLineHintType: TSynCompletionLongHintType;
    // Code Folding
    fUseCodeFolding: Boolean;
    fUseMarkupWordBracket: Boolean;
    fUseMarkupOutline: Boolean;
    // Multi window
    fMultiWinEditAccessOrder: TEditorOptionsEditAccessOrderList;
    // Default values for RttiXmlConfig using published properties.
    FDefaultValues: TEditorOptionsDefaults;

    function GetHighlighterList: TEditOptLangList;
    procedure Init;
    function GetCodeTemplateFileNameExpand: String;
    function GetColorSchemeLanguage(aHighLighter: TSynCustomHighlighter;
                          SynColorSchemeName: String = ''): TColorSchemeLanguage;
  protected
  public
    class function GetGroupCaption: string; override;
    class function GetInstance: TAbstractIDEOptions; override;
    procedure DoAfterWrite(Restore: boolean); override;
    procedure DoAfterRead; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    function LoadCodeTemplates(AnAutoComplete: TSynEditAutoComplete): TModalResult;
    function SaveCodeTemplates(AnAutoComplete: TSynEditAutoComplete): TModalResult;
    procedure AssignKeyMapTo(ASynEdit: TSynEdit; SimilarEdit: TSynEdit = nil); // Or copy fromSimilarEdit
    function ReadColorScheme(const LanguageName: String): String; // TODO: rename ReadColorSchemeName
    function ReadPascalColorScheme: String;
    procedure WriteColorScheme(const LanguageName, SynColorScheme: String);
    procedure ReadHighlighterSettings(Syn: TSrcIDEHighlighter; SynColorScheme: String);
    procedure ReadHighlighterFoldSettings(Syn: TSrcIDEHighlighter; ReadForOptions: Boolean = False);
    procedure ReadDefaultsForHighlighterFoldSettings(Syn: TSrcIDEHighlighter);
    procedure WriteHighlighterFoldSettings(Syn: TSrcIDEHighlighter);
    procedure ReadHighlighterDivDrawSettings(Syn: TSrcIDEHighlighter);
    procedure ReadDefaultsForHighlighterDivDrawSettings(Syn: TSrcIDEHighlighter);
    procedure WriteHighlighterDivDrawSettings(Syn: TSrcIDEHighlighter);
    procedure GetHighlighterObjSettings(Syn: TObject); override; // read highlight settings from config file
    procedure GetHighlighterSettings(Syn: TSrcIDEHighlighter); // read highlight settings from config file
    procedure GetSynEditorSettings(ASynEdit: TObject; SimilarEdit: TObject = nil); override;
    procedure GetSynEditSettings(ASynEdit: TSynEdit; SimilarEdit: TSynEdit = nil; AHighlighterId: TIdeSyntaxHighlighterID = IdeHighlighterUnknownId); // read synedit settings from config file
    procedure UpdateSynEditSettingsForHighlighter(ASynEdit: TSynEdit; AHighlighterId: TIdeSyntaxHighlighterID);
    procedure GetSynEditPreviewSettings(APreviewEditor: TObject);
    procedure SetMarkupColor(Syn: TSrcIDEHighlighter;
                             AddHilightAttr: TAdditionalHilightAttribute;
                             aMarkup: TLazEditTextAttributeModifier);
    procedure SetMarkupColors(aSynEd: TSynEdit);
    procedure ApplyFontSettingsTo(ASynEdit: TSynEdit);
    procedure ApplyTabFontSettingsTo(APageCtrl: TPageControl);
  public
    // general options
    property SynEditOptions: TSynEditorOptions
      read fSynEditOptions write fSynEditOptions default SynEditDefaultOptions;
    property SynEditOptions2: TSynEditorOptions2
      read fSynEditOptions2 write fSynEditOptions2 default SynEditDefaultOptions2;
    property ShowFileNameInCaption: Boolean
      read fShowFileNameInCaption write fShowFileNameInCaption;
    property ShowTabCloseButtons: Boolean
      read fShowTabCloseButtons write fShowTabCloseButtons;
    property HideSingleTabInWindow: Boolean
      read FHideSingleTabInWindow write FHideSingleTabInWindow;
    property ShowTabNumbers: Boolean read fShowTabNumbers write fShowTabNumbers;
    property UndoAfterSave: Boolean read fUndoAfterSave write fUndoAfterSave default True;
    property FindTextAtCursor: Boolean
      read fFindTextAtCursor write fFindTextAtCursor default True;
    property UseSyntaxHighlight: Boolean
      read fUseSyntaxHighlight write fUseSyntaxHighlight default True;
    property CopyWordAtCursorOnCopyNone: Boolean
      read FCopyWordAtCursorOnCopyNone write FCopyWordAtCursorOnCopyNone;
    property ShowGutterHints: Boolean read FShowGutterHints write FShowGutterHints;
    property BlockIndent: Integer read fBlockIndent write fBlockIndent default 2;
    property BlockTabIndent: Integer read FBlockTabIndent write FBlockTabIndent default 0;
    property BlockIndentType: TSynBeautifierIndentType
      read fBlockIndentType write fBlockIndentType default sbitCopySpaceTab;
    property TrimSpaceType: TSynEditStringTrimmingType
      read fTrimSpaceType write fTrimSpaceType default settLeaveLine;
    property UndoLimit: Integer read fUndoLimit write fUndoLimit default 32767;
    property TabWidth: Integer read fTabWidth write fTabWidth default 8;
    property BracketHighlightStyle: TSynEditBracketHighlightStyle
      read fBracketHighlightStyle write fBracketHighlightStyle default sbhsBoth;
    // Display options
    property VisibleRightMargin: Boolean
      read fVisibleRightMargin write fVisibleRightMargin default True;
    property VisibleGutter: Boolean read fVisibleGutter write fVisibleGutter default True;
    property ShowOnlyLineNumbersMultiplesOf: integer read fShowOnlyLineNumbersMultiplesOf
      write fShowOnlyLineNumbersMultiplesOf;
    property GutterWidth: Integer read fGutterWidth write fGutterWidth default 30;
    property RightMargin: Integer read fRightMargin write fRightMargin default 80;
    property EditorFont: String read fEditorFont write fEditorFont;
    property EditorFontSize: Integer read fEditorFontSize write fEditorFontSize;
    property ExtraCharSpacing: Integer
      read fExtraCharSpacing write fExtraCharSpacing default 0;
    property ExtraLineSpacing: Integer
      read fExtraLineSpacing write fExtraLineSpacing default 1;
    property DisableAntialiasing: Boolean read fDisableAntialiasing
      write fDisableAntialiasing default DefaultEditorDisableAntiAliasing;
    property DoNotWarnForFont: string read FDoNotWarnForFont write FDoNotWarnForFont;
    // Key Mappings
    property KeyMappingScheme: String read fKeyMappingScheme write fKeyMappingScheme;
    property KeyMap: TKeyCommandRelationList read fKeyMap;
    // Mouse Mappings
    // Current saved config
    property UserMouseSettings: TEditorMouseOptions read FUserMouseSettings;
    // Used by the 2 Mouse-option pages, so they share data (un-saved)
    property TempMouseSettings: TEditorMouseOptions read FTempMouseSettings;
    // Color options
    property HighlighterList: TEditOptLangList read GetHighlighterList;
    property UserColorSchemeGroup: TColorSchemeFactory read fUserColorSchemeGroup;
    property UserDefinedColors: TEditorUserDefinedWordsList read fUserDefinedColors;
    // Markup Current Word
    property MarkupCurWordTime: Integer
      read fMarkupCurWordTime write fMarkupCurWordTime default 1500;
    property MarkupCurWordFullLen: Integer
      read fMarkupCurWordFullLen write fMarkupCurWordFullLen default 3;
    property MarkupCurWordNoKeyword: Boolean
      read fMarkupCurWordNoKeyword write fMarkupCurWordNoKeyword default False;
    property MarkupCurWordTrim: Boolean
      read fMarkupCurWordTrim write fMarkupCurWordTrim default True;
    property MarkupCurWordNoTimer: Boolean
      read fMarkupCurWordNoTimer write fMarkupCurWordNoTimer default False;
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
    property AutoDelayInMSec: Integer read fAutoDelayInMSec
      write fAutoDelayInMSec default 1000;
    property AutoHintDelayInMSec: Integer read fAutoHintDelayInMSec
      write fAutoHintDelayInMSec default 1000;
    property CodeTemplateFileNameRaw: String
      read fCodeTemplateFileNameRaw write fCodeTemplateFileNameRaw;
    property CodeTemplateFileNameExpand: String read GetCodeTemplateFileNameExpand;
    property CodeTemplateIndentToTokenStart: Boolean
      read fCTemplIndentToTokenStart write fCTemplIndentToTokenStart;
    property AutoRemoveEmptyMethods: Boolean
      read fAutoRemoveEmptyMethods write fAutoRemoveEmptyMethods default False;
    property CompletionLongLineHintInMSec: Integer
      read fCompletionLongLineHintInMSec write fCompletionLongLineHintInMSec;
    property CompletionLongLineHintType: TSynCompletionLongHintType
      read fCompletionLongLineHintType write fCompletionLongLineHintType
      default sclpExtendRightOnly;
    // Code Folding
    property UseCodeFolding: Boolean
      read fUseCodeFolding write fUseCodeFolding default True;
    property UseMarkupWordBracket: Boolean
      read fUseMarkupWordBracket write fUseMarkupWordBracket default True;
    property UseMarkupOutline: Boolean
      read fUseMarkupOutline write fUseMarkupOutline default False;
    // Multi window
    property MultiWinEditAccessOrder: TEditorOptionsEditAccessOrderList
        read FMultiWinEditAccessOrder write FMultiWinEditAccessOrder;
  end;

  { TIDEAsmWinHighlighter }

  TIDEAsmWinHighlighter = class(TNonSrcIDEHighlighter) // TODO: move to AssemblerDlg, when editoropts become a package
  public
    constructor Create(AOwner: TComponent); override;
    class function GetLanguageName: string; override;
  end;

var
  EditorOpts: TEditorOptions;

procedure RepairEditorFontSize(var FontSize: integer);
function BuildBorlandDCIFile(ACustomSynAutoComplete: TCustomSynAutoComplete): Boolean;
function HighlighterList: TEditOptLangList;
function ColorSchemeFactory: TColorSchemeFactory;
function UserKeySchemeDirectory(CreateIfNotExists: Boolean = False): String;
function UserSchemeDirectory(CreateIfNotExists: Boolean = False): String;
procedure InitLocale;

implementation

{$R editoroptions.res}

const
  ValidAttribChars = ['a'..'z', 'A'..'Z', '_', '0'..'9'];

var
  DefaultColorSchemeName: String;
  EdOptsChangedHandlers: TMethodList;
  RegisteredAttribGroupNames: array of PString;

function FontHeightToSize(Height: Integer): Integer;
var
  AFont: TFont;
begin
  AFont := TFont.Create;
  AFont.Height := Height;
  Result := AFont.Size;
  AFont.Free;
end;

function OldAdditionalAttributeName(NewAha: String): string;
var
  AttriIdx: Integer;
begin
  AttriIdx := GetEnumValue(TypeInfo(TAdditionalHilightAttribute), NewAha);
  if AttriIdx < 0 then
    Result := NewAha
  else
    Result := ahaXmlNames[TAdditionalHilightAttribute(AttriIdx)];
end;

function GetAddiHilightAttrName(aha: TAdditionalHilightAttribute): String;
begin
  Result := GetEnumName(TypeInfo(TAdditionalHilightAttribute), ord(aha));
end;

function GetSynEditOptionName(SynOption: TSynEditorOption): string;
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

function GetSynBeautifierIndentName(IndentType: TSynBeautifierIndentType): string;
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

function GetSynBeautifierIndentType(IndentName: String): TSynBeautifierIndentType;
begin
  case IndentName of
    'CopySpaceTabIndent':
      Result := sbitCopySpaceTab;
    'PositionIndent':
      Result := sbitPositionCaret;
    'sbitConvertToTabSpace':
      Result := sbitConvertToTabSpace;
    'sbitConvertToTabOnly':
      Result := sbitConvertToTabOnly;
    else
      Result := sbitSpace;
  end;
end;

function GetTrimSpaceName(IndentType: TSynEditStringTrimmingType): string;
begin
  case IndentType of
    settLeaveLine:
      Result := 'LeaveLine';
    settEditLine:
      Result := 'EditLine';
    settMoveCaret:
      Result := 'MoveCaret';
    settIgnoreAll:
      Result := 'PosOnly';
    else
      Result := '';
  end;
end;

function GetTrimSpaceType(IndentName: String): TSynEditStringTrimmingType;
begin
  case IndentName of
    'EditLine':
      Result := settEditLine;
    'MoveCaret':
      Result := settMoveCaret;
    'PosOnly':
      Result := settIgnoreAll;
    else
      Result := settLeaveLine;
  end;
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

{ TEditorSynGutterOptions }

constructor TEditorSynGutterOptions.DoCreate(AIdx: Integer;
  AGClass: TSynGutterPartBaseClass);
begin
  inherited Create;
  FGClass := AGClass;
  FIndex   := AIdx;
  FVisible := True;
  FShowLineColor := glcOn;

  if FGClass = TSynGutterMarks then begin
    FWidth := 2;
  end
  else
  if FGClass = TSynGutterLineNumber then begin
    FWidth := 2;
    FShowLineColor := glcLineNum;
  end
  else
  if FGClass = TSynGutterCodeFolding then begin
    FShowLineColor := glcOff;
  end
  else
  if FGClass = TSynGutterLineOverview then begin
    FShowLineColor := glcOff;
  end;
end;

constructor TEditorSynGutterOptions.Create(AIdx: Integer;
  AGClass: TSynGutterPartBaseClass);
begin
  DoCreate(AIdx, AGClass);
  FDefaults := TEditorSynGutterOptions.DoCreate(AIdx, AGClass);
end;

destructor TEditorSynGutterOptions.Destroy;
begin
  FreeAndNil(FDefaults);
  inherited Destroy;
end;

procedure TEditorSynGutterOptions.Assign(Source: TPersistent);
begin
  if Source is TEditorSynGutterOptions then begin
    FGClass      := TEditorSynGutterOptions(Source).FGClass;
    FIndex       := TEditorSynGutterOptions(Source).FIndex;
    FOffsetLeft  := TEditorSynGutterOptions(Source).FOffsetLeft;
    FOffsetRight := TEditorSynGutterOptions(Source).FOffsetRight;
    FVisible     := TEditorSynGutterOptions(Source).FVisible;
    FWidth       := TEditorSynGutterOptions(Source).FWidth;
    FShowLineColor := TEditorSynGutterOptions(Source).FShowLineColor;
  end;
end;

procedure TEditorSynGutterOptions.ApplyTo(AGutterPart: TSynGutterPartBase);
begin
  if AGutterPart = nil then exit;

  if FGClass = TSynGutterMarks then begin
    TSynGutterMarks(AGutterPart).ColumnCount := FWidth;
  end
  else
  if FGClass = TSynGutterLineNumber then begin
    AGutterPart.AutoSize := True;
    TSynGutterLineNumber(AGutterPart).DigitCount := FWidth;
  end
  else
  if FGClass = TSynGutterSeparator then begin
    AGutterPart.AutoSize := FWidth = 0;
    if FWidth = 0 then begin
      TSynGutterSeparator(AGutterPart).Width := 2;
      TSynGutterSeparator(AGutterPart).LineWidth := 1;
    end
    else begin
      TSynGutterSeparator(AGutterPart).Width := FWidth;
      TSynGutterSeparator(AGutterPart).LineWidth := FWidth;
    end;
  end
  else
  begin
    AGutterPart.AutoSize := FWidth = 0;
    AGutterPart.Width := FWidth;
  end;
  AGutterPart.Visible := FVisible;
  AGutterPart.LeftOffset := FOffsetLeft;
  AGutterPart.RightOffset := FOffsetRight;
end;

procedure TEditorSynGutterOptions.ApplyLineColorTo(AGutterPart: TSynGutterPartBase; Attri,
  NumAttri: TColorSchemeAttribute);
begin
  if AGutterPart = nil then exit;
  case FShowLineColor of
    glcOff:     AGutterPart.MarkupInfoCurrentLine.Clear;
    glcOn:      if Attri    <> nil then Attri.ApplyTo(AGutterPart.MarkupInfoCurrentLine);
    glcLineNum: if NumAttri <> nil then NumAttri.ApplyTo(AGutterPart.MarkupInfoCurrentLine);
  end;
end;

procedure TEditorSynGutterOptions.ApplyIndexTo(AGutterPart: TSynGutterPartBase);
begin
  if AGutterPart <> nil then
    AGutterPart.Index := Index;
end;

{ TEditorSynGutterOptionsList }

function SynGutterOptListSortCompare(const Item1, Item2: TEditorSynGutterOptions): Integer;
begin
  Result := Item1.Index - Item2.Index;
end;

function TEditorSynGutterOptionsList.GetByClass(AIndex: TSynGutterPartBaseClass
  ): TEditorSynGutterOptions;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].GClass = AIndex then
      exit(Items[i]);
end;

procedure TEditorSynGutterOptionsList.Assign(Source: TEditorSynGutterOptionsList);
var
  i: Integer;
  n: TEditorSynGutterOptions;
begin
  Clear;
  for i := 0 to Source.Count - 1 do begin
    n := TEditorSynGutterOptions.Create(Source[i].Index, Source[i].GClass);
    n.Assign(Source[i]);
    Add(n);
  end;
end;

procedure TEditorSynGutterOptionsList.AssignItems(Source: TEditorSynGutterOptionsList);
var
  i: Integer;
  itm, dest: TEditorSynGutterOptions;
begin
  for i := 0 to Source.Count - 1 do begin
    itm := Source.Items[i];
    dest := ByClass[itm.GClass];
    if dest <> nil then
      dest.Assign(itm);
  end;
  Sort;
end;

procedure TEditorSynGutterOptionsList.Sort;
var
  i: Integer;
begin
  inherited Sort(@SynGutterOptListSortCompare);
  // fix gaps
  for i := 0 to Count - 1 do
    Items[i].Index := i;
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
  FColorAttr.AttrFeatures := [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior,hafFrameStyle, hafFrameEdges, hafStyle, hafStyleMask];
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
var
  SrcWords: TEditorUserDefinedWords;
begin
  inherited Assign(Source);
  if not (Source is TEditorUserDefinedWords) then exit;
  ClearIdeCommands;
  SrcWords := TEditorUserDefinedWords(Source);
  FId                      := SrcWords.FId;
  FName                    := SrcWords.FName;
  FGlobalList              := SrcWords.FGlobalList;
  FKeyAddCase              := SrcWords.FKeyAddCase;
  FKeyAddSelectBoundMaxLen := SrcWords.FKeyAddSelectBoundMaxLen;
  FKeyAddSelectSmart       := SrcWords.FKeyAddSelectSmart;
  FKeyAddTermBounds        := SrcWords.FKeyAddTermBounds;
  FKeyAddWordBoundMaxLen   := SrcWords.FKeyAddWordBoundMaxLen;

  FColorAttr.Assign(SrcWords.FColorAttr);
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
  DciFileVersion = 1;
  DciFileVersionName = '!FileVersion';
  DciVersionName = '!Version';

function BuildBorlandDCIFile(ACustomSynAutoComplete: TCustomSynAutoComplete): Boolean;
  // returns if something has changed
var
  sl: TStringList;
  i, sp, ep, v: Integer;
  Value: String;
  Template: TTemplate;
begin
  Result := False;
  sl     := TStringList.Create;
  try
    for i := 0 to ACustomSynAutoComplete.CodeTemplates.Count - 1 do
    begin
      Template := ACustomSynAutoComplete.CodeTemplates[i];
      Value := Template.Value;
      sl.Add('[' + Template.Key + ' | ' + Template.Comment + ']');

      // Store DciFileVersion as attribute to first macro
      v := Template.Attributes.IndexOfName(DciFileVersionName);
      if v >= 0 then
        Template.Attributes.Delete(v);
      if i = 0 then
        Template.Attributes.Values[DciFileVersionName] := IntToStr(DciFileVersion);

      if Template.Attributes.Count>0 then begin
        sl.Add(CodeTemplateAttributesStartMagic);
        sl.AddStrings(Template.Attributes);
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
    if not ACustomSynAutoComplete.CodeTemplSource.Equals(sl) then
    begin
      Result := True;
      ACustomSynAutoComplete.CodeTemplSource := sl;
    end;
  finally
    sl.Free;
  end;
end;

// The lazy-man color scheme factory
var
  TheColorSchemeFactorSingleton: TColorSchemeFactory = nil;
function ColorSchemeFactory: TColorSchemeFactory;
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
    TheColorSchemeFactorSingleton.RegisterScheme(XMLConfig, ASchemeName, 'Lazarus/ColorSchemes/');
    FreeAndNil(XMLConfig);
    FreeAndNil(Stream);
  end;

begin
  if not Assigned(TheColorSchemeFactorSingleton) then begin
    HighlighterList.Init; // defer init

    InitLocale;
    TheColorSchemeFactorSingleton := TColorSchemeFactory.Create;
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
          for j := 1 to c do begin
            n := XMLConfig.GetValue('Lazarus/ColorSchemes/Names/Item'+IntToStr(j)+'/Value', '');
            if n <> '' then
              TheColorSchemeFactorSingleton.RegisterScheme(XMLConfig, n, 'Lazarus/ColorSchemes/');
          end;
        except
          ShowMessage(Format(dlgUserSchemeError, [FileList[i]]));
        end;
        XMLConfig.Free;
      end;
      FileList.Free;
    end;
  end;
  IdeColorSchemeList := TheColorSchemeFactorSingleton;
  _IDE_CallOnIdeColorSchemeListCreated;
  Result := TheColorSchemeFactorSingleton;
end;

function UserKeySchemeDirectory(CreateIfNotExists: Boolean): String;
begin
  Result := AppendPathDelim(GetPrimaryConfigPath) + KeyMappingSchemeConfigDirName;
  If CreateIfNotExists and (not DirectoryExistsUTF8(Result)) then
    CreateDirUTF8(Result);
end;

function UserSchemeDirectory(CreateIfNotExists: Boolean): String;
begin
  Result := AppendPathDelim(GetPrimaryConfigPath) + 'userschemes';
  If CreateIfNotExists and (not DirectoryExistsUTF8(Result)) then
    CreateDirUTF8(Result);
end;

var
  TheIdeHiglighterListSingleton: TEditOptLangList = nil;
function HighlighterList: TEditOptLangList;
begin
  if not Assigned(TheIdeHiglighterListSingleton) then
    TheIdeHiglighterListSingleton := TEditOptLangList.Create;
  Result := TheIdeHiglighterListSingleton;
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
  EditorOptionsFoldInfoPas[ 1].Name := dlgFoldPasAnonProcedure;
  EditorOptionsFoldInfoPas[ 2].Name := dlgFoldLocalPasVarType;
  EditorOptionsFoldInfoPas[ 3].Name := dlgFoldPasProcBeginEnd;
  EditorOptionsFoldInfoPas[ 4].Name := dlgFoldPasBeginEnd;
  EditorOptionsFoldInfoPas[ 5].Name := dlgFoldPasRepeat;
  EditorOptionsFoldInfoPas[ 6].Name := dlgFoldPasCase;
  EditorOptionsFoldInfoPas[ 7].Name := dlgFoldPasTry;
  EditorOptionsFoldInfoPas[ 8].Name := dlgFoldPasExcept;
  EditorOptionsFoldInfoPas[ 9].Name := dlgFoldPasAsm;
  EditorOptionsFoldInfoPas[10].Name := dlgFoldPasProgram;
  EditorOptionsFoldInfoPas[11].Name := dlgFoldPasUnit;
  EditorOptionsFoldInfoPas[12].Name := dlgFoldPasUnitSection;
  EditorOptionsFoldInfoPas[13].Name := dlgFoldPasUses;
  EditorOptionsFoldInfoPas[14].Name := dlgFoldPasVarType;
  EditorOptionsFoldInfoPas[15].Name := dlgFoldPasClass;
  EditorOptionsFoldInfoPas[16].Name := dlgFoldPasClassSection;
  EditorOptionsFoldInfoPas[17].Name := dlgFoldPasRecord;
  EditorOptionsFoldInfoPas[18].Name := dlgFoldPasRecordCase;
  EditorOptionsFoldInfoPas[19].Name := dlgFoldPasRecordCaseSect;
  EditorOptionsFoldInfoPas[20].Name := dlgFoldPasIfDef;
  EditorOptionsFoldInfoPas[21].Name := dlgFoldPasUserRegion;
  EditorOptionsFoldInfoPas[22].Name := dlgFoldPasAnsiComment;
  EditorOptionsFoldInfoPas[23].Name := dlgFoldPasBorComment;
  EditorOptionsFoldInfoPas[24].Name := dlgFoldPasSlashComment;
  EditorOptionsFoldInfoPas[25].Name := dlgFoldPasNestedComment;
  EditorOptionsFoldInfoPas[26].Name := dlgFoldPasIfThen;
  EditorOptionsFoldInfoPas[27].Name := dlgFoldPasForDo;
  EditorOptionsFoldInfoPas[28].Name := dlgFoldPasWhileDo;
  EditorOptionsFoldInfoPas[29].Name := dlgFoldPasWithDo;

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
  AdditionalHighlightAttributes[ahaCodeFoldingTreeCurrent] := dlgAddHiAttrCodeFoldingTreeCur;
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
  AdditionalHighlightAttributes[ahaOverviewGutter]      := dlgOverviewGutterColor;
  AdditionalHighlightAttributes[ahaGutterCurrentLine]   := dlgGutterCurrentLineOther;
  AdditionalHighlightAttributes[ahaGutterNumberCurrentLine] := dlgGutterCurrentLineNumber;
  AdditionalHighlightAttributes[ahaIfDefBlockInactive]  := dlgIfDefBlockInactive;
  AdditionalHighlightAttributes[ahaIfDefBlockActive]    := dlgIfDefBlockActive;
  AdditionalHighlightAttributes[ahaIfDefBlockTmpActive] := dlgIfDefBlockTmpActive;
  AdditionalHighlightAttributes[ahaIfDefNodeInactive]   := dlgIfDefNodeInactive;
  AdditionalHighlightAttributes[ahaIfDefNodeActive]     := dlgIfDefNodeActive;
  AdditionalHighlightAttributes[ahaIfDefNodeTmpActive]  := dlgIfDefNodeTmpActive;
  AdditionalHighlightGroupNames[agnIfDef]        := dlgAddHiAttrGroupIfDef;

  AdditionalHighlightAttributes[ahaIdentComplWindow]                  := dlgAddHiAttrDefaultWindow;
  AdditionalHighlightAttributes[ahaIdentComplWindowBorder]            := dlgAddHiAttrWindowBorder;
  AdditionalHighlightAttributes[ahaIdentComplRecent]                  := dlgAddHiAttrRecentlyUsed;
  AdditionalHighlightAttributes[ahaIdentComplWindowSelection]         := dlgBlockGroupOptions;
  AdditionalHighlightAttributes[ahaIdentComplWindowHighlight]         := dlgAddHiAttrHighlightPrefix;
  AdditionalHighlightAttributes[ahaIdentComplWindowEntryVar]          := dlgIAhadentifierComplEntryVar;
  AdditionalHighlightAttributes[ahaIdentComplWindowEntryType]         := dlgIAhadentifierComplEntryType;
  AdditionalHighlightAttributes[ahaIdentComplWindowEntryConst]        := dlgIAhadentifierComplEntryConst;
  AdditionalHighlightAttributes[ahaIdentComplWindowEntryProc]         := dlgIAhadentifierComplEntryProc;
  AdditionalHighlightAttributes[ahaIdentComplWindowEntryFunc]         := dlgIAhadentifierComplEntryFunc;
  AdditionalHighlightAttributes[ahaIdentComplWindowEntryMethAbstract] := dlgIAhadentifierComplEntryAbstractProcFunc;
  AdditionalHighlightAttributes[ahaIdentComplWindowEntryMethodLowVis] := dlgIAhadentifierComplEntryLowerVisibilityProcFunc;
  AdditionalHighlightAttributes[ahaIdentComplWindowEntryProp]         := dlgIAhadentifierComplEntryProperty;
  AdditionalHighlightAttributes[ahaIdentComplWindowEntryIdent]        := dlgIAhadentifierComplEntryIdent;
  AdditionalHighlightAttributes[ahaIdentComplWindowEntryLabel]        := dlgIAhadentifierComplEntryLabel;
  AdditionalHighlightAttributes[ahaIdentComplWindowEntryEnum]         := dlgIAhadentifierComplEntryEnum;
  AdditionalHighlightAttributes[ahaIdentComplWindowEntryUnit]         := dlgIAhadentifierComplEntryUnit;
  AdditionalHighlightAttributes[ahaIdentComplWindowEntryNameSpace]    := dlgIAhadentifierComplEntryNamespace;
  AdditionalHighlightAttributes[ahaIdentComplWindowEntryText]         := dlgIAhadentifierComplEntryText;
  AdditionalHighlightAttributes[ahaIdentComplWindowEntryTempl]        := dlgIAhadentifierComplEntryCodeTemplate;
  AdditionalHighlightAttributes[ahaIdentComplWindowEntryKeyword]      := dlgIAhadentifierComplEntryKeyword;
  AdditionalHighlightAttributes[ahaIdentComplWindowEntryUnknown]      := dlgIAhadentifierComplEntryOther;
  AdditionalHighlightGroupNames[agnIdentComplWindow]                  := dlgIdentifierCompletion;

  AdditionalHighlightAttributes[ahaOutlineLevel1Color]  := Format(dlgAddHiAttrOutlineLevelColor, [' 1']);
  AdditionalHighlightAttributes[ahaOutlineLevel2Color]  := Format(dlgAddHiAttrOutlineLevelColor, [' 2']);
  AdditionalHighlightAttributes[ahaOutlineLevel3Color]  := Format(dlgAddHiAttrOutlineLevelColor, [' 3']);
  AdditionalHighlightAttributes[ahaOutlineLevel4Color]  := Format(dlgAddHiAttrOutlineLevelColor, [' 4']);
  AdditionalHighlightAttributes[ahaOutlineLevel5Color]  := Format(dlgAddHiAttrOutlineLevelColor, [' 5']);
  AdditionalHighlightAttributes[ahaOutlineLevel6Color]  := Format(dlgAddHiAttrOutlineLevelColor, [' 6']);
  AdditionalHighlightAttributes[ahaOutlineLevel7Color]  := Format(dlgAddHiAttrOutlineLevelColor, [' 7']);
  AdditionalHighlightAttributes[ahaOutlineLevel8Color]  := Format(dlgAddHiAttrOutlineLevelColor, [' 8']);
  AdditionalHighlightAttributes[ahaOutlineLevel9Color]  := Format(dlgAddHiAttrOutlineLevelColor, [' 9']);
  AdditionalHighlightAttributes[ahaOutlineLevel10Color] := Format(dlgAddHiAttrOutlineLevelColor, ['10']);
  AdditionalHighlightGroupNames[agnOutlineColors]  := dlgAddHiAttrGroupOutlineColors;

  AdditionalHighlightAttributes[ahaWrapIndend]  := dlgAddHiAttrWrapIndent;
  AdditionalHighlightAttributes[ahaWrapEol]     := dlgAddHiAttrWrapEol;
  AdditionalHighlightAttributes[ahaWrapSubLine] := dlgAddHiAttrWrapSubLine;

  AdditionalHighlightAttributes[ahaExternalLink] := dlgAddHiExternalLink;

  AdditionalHighlightGroupNames[agnDefault]      := dlgAddHiAttrGroupDefault;
  AdditionalHighlightGroupNames[agnText]         := dlgAddHiAttrGroupText;
  AdditionalHighlightGroupNames[agnLine]         := dlgAddHiAttrGroupLine;
  AdditionalHighlightGroupNames[agnTemplateMode] := dlgAddHiAttrGroupTemplateEdit;
  AdditionalHighlightGroupNames[agnSyncronMode]  := dlgAddHiAttrGroupSyncroEdit;
  AdditionalHighlightGroupNames[agnGutter]       := dlgAddHiAttrGroupGutter;
  AdditionalHighlightGroupNames[agnWrap]         := dlgAddHiAttrGroupWrap;
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
  SynInstance.Free;
  inherited Destroy;
end;

function TEditOptLanguageInfo.CreateNewSynInstance: TSrcIDEHighlighter;
begin
  Result := TCustomSynClass(SynInstance.ClassType).Create(nil);
end;

function TEditOptLanguageInfo.SampleLineToAddAttr(Line: Integer): TAdditionalHilightAttribute;
begin
  if Line < 1 then
    exit(ahaNone);
  for Result := Low(TAdditionalHilightAttribute) to High(TAdditionalHilightAttribute) do
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

{ TEditOptLanguageTextMateInfo }

function TEditOptLanguageTextMateInfo.CreateNewSynInstance: TSrcIDEHighlighter;
begin
  Result := TSynTextMateSyn.Create(nil);
  TSynTextMateSyn(Result).LoadGrammar(FileName, '');
end;

{ TEditOptLangList }

function TEditOptLangList.DoGetTMLGrammar(Sender: TTextMateGrammar;
  AScopeName: string): TTextMateGrammar;
var
  i: Integer;
begin
  for i := 1 to Count - 1 do begin
    if (SharedInstances[i] is TSynTextMateSyn) and
       (TSynTextMateSyn(SharedInstances[i]).TextMateGrammar.LanguageScopeName = AScopeName)
    then
      exit(TSynTextMateSyn(SharedInstances[i]).TextMateGrammar);
  end;
  Result := nil;
end;

function TEditOptLangList.GetLazSyntaxHighlighterType(
  AnId: TIdeSyntaxHighlighterID): TLazSyntaxHighlighter;
begin
  if AnID < 0 then
    exit(lshNone);
  Result := Items[AnId].TheType;
end;

function TEditOptLangList.GetInfos(Index: Integer): TEditOptLanguageInfo;
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.Create('TEditOptLangList.GetInfos Index '
      + IntToStr(Index) + ' out of bounds. Count=' + IntToStr(Count));
  Result := TEditOptLanguageInfo(inherited Items[Index]);
end;

function TEditOptLangList.GetSharedSynInstances(AnID: TIdeSyntaxHighlighterID
  ): TSrcIDEHighlighter;
begin
  Result := TSrcIDEHighlighter(SharedInstances[AnID]);
end;

function TEditOptLangList.GetIdForFileExtension(Ext: String
  ): TIdeSyntaxHighlighterID;
var
  s, CurExt: String;
  StartPos, EndPos: Integer;
begin
  Result := -1;
  if (Ext = '') or (Ext = '.') then
    exit;
  if Ext[1] = '.' then
    System.Delete(Ext, 1, 1);

  Result := Count - 1;
  while (Result >= 0) do begin
    s := Items[Result].FileExtensions;
    StartPos := 1;
    while StartPos <= length(s) do
    begin
      Endpos := StartPos;
      while (EndPos <= length(s)) and (s[EndPos] <> ';') do
        inc(EndPos);
      CurExt := copy(s, Startpos, EndPos - StartPos);
      if (CurExt <> '') and (CurExt[1] = '.') then
        System.Delete(CurExt, 1, 1);
      if CompareText(CurExt, Ext) = 0 then
        exit;
      Startpos := EndPos + 1;
    end;

    dec(Result);
  end;
end;

function TEditOptLangList.GetIdForFileExtension(Ext: String;
  ADelphiMode: boolean): TIdeSyntaxHighlighterID;
begin
  Result := GetIdForFileExtension(Ext);
  if (Result >= 0) and (Items[Result].TheType in [lshFreePascal, lshDelphi]) then begin
    if ADelphiMode then
      Result := GetIdForLazSyntaxHighlighter(lshDelphi)
    else
      Result := GetIdForLazSyntaxHighlighter(lshFreePascal);
  end;
end;

function TEditOptLangList.GetIdForName(AName: String): TIdeSyntaxHighlighterID;
begin
  if AName = '' then
    exit(IdeHighlighterNotSpecifiedId);
  Result := Count - 1;
  while (Result >= 0) and (CompareText(AName, Names[Result]) <> 0) do
    dec(Result);
  if Result < 0 then
    Result := IdeHighlighterUnknownId;
end;

function TEditOptLangList.GetCaptions(AnID: TIdeSyntaxHighlighterID): String;
var
  h: TEditOptLanguageInfo;
begin
  if AnID <= 0 then
    exit(LazSyntaxHighlighterNames{%H-}[lshNone]);
  h := Items[AnID];
  if h.TheType <> lshNone then begin
    if h.TheType=lshFreePascal then
      exit('Free Pascal');
    exit(LazSyntaxHighlighterNames{%H-}[h.TheType]);
  end;
  Result := h.SynInstance.LanguageName;
end;

function TEditOptLangList.GetNames(AnID: TIdeSyntaxHighlighterID): String;
var
  h: TEditOptLanguageInfo;
  i: SizeInt;
begin
  if AnID = IdeHighlighterNotSpecifiedId then // IdeHighlighterNoneID
    Result := ''
  else
  if AnID <= 0 then // IdeHighlighterNoneID
    Result := LazSyntaxHighlighterNames{%H-}[lshNone]
  else begin
    h := Items[AnID];
    if h.TheType <> lshNone then
      Result := LazSyntaxHighlighterNames{%H-}[h.TheType]
    else
      Result := h.SynInstance.LanguageName;
  end;

  // remove chars, that might not be ok in an xml name
  i := Length(Result);
  while i > 0 do begin
    if Result[i] = '_' then begin
      System.Insert('_', Result, i);
      Result[i] := '_';
    end
    else
    if not (Result[i] in ['a'..'z', 'A'..'Z', '0'..'9']) then begin
      System.Insert(IntToHex(ord(Result[i]),2), Result, i+1);
      Result[i] := '_';
    end;
    dec(i);
  end;
end;

function TEditOptLangList.GetSharedInstances(AnID: TIdeSyntaxHighlighterID
  ): TObject;
begin
  if AnID < 0 then // lshNone;
    exit(nil);
  Result := Items[AnID].SynInstance;
end;

function TEditOptLangList.GetSynHlClasses(AnID: TIdeSyntaxHighlighterID
  ): TClass;
begin
  if AnID < 0 then // lshNone;
    exit(nil);
  Result := Items[AnID].SynInstance.ClassType;
end;

function TEditOptLangList.GetIdForLazSyntaxHighlighter(
  AnHighlighterType: TLazSyntaxHighlighter): TIdeSyntaxHighlighterID;
begin
  // lshNone is internally used for non-buildin HL
  if AnHighlighterType = lshNone then
    exit(IdeHighlighterNoneID);

  Result := Count - 1;
  while (Result >= 0) and (Items[Result].TheType <> AnHighlighterType) do
    dec(Result);
end;

procedure TEditOptLangList.Clear;
begin
  assert(False, 'TEditOptLangList.Clear: Not allowed - Index is used as ID');
end;

procedure TEditOptLangList.Init;
var
  NewInfo: TEditOptLanguageInfo;
  FileList: TStringList;
  i, j: Integer;
  tmlHighlighter: TSynTextMateSyn;
  dir, n: String;
  StrLoader: TStringStream;
begin
  { - Create the meta information for each available highlighter.
    - The entry for lshNone **MUST** be first. It must match IdeHighlighterNoneID
    - Please keep the pascal highlighter next after it.
    - The rest can be ordered as you like.
  }

  // create info for IdeHighlighterNoneID / lshNone
  // MUST be FIRST / Index = 0 = IdeHighlighterNoneID
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshNone;
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := nil;
    SetBothFilextensions('');
    SampleSource := '';
    MappedAttributes := TStringList.Create;
    CaretXY := Point(1,1);
  end;
  Add(NewInfo);

  // create info for pascal
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshFreePascal;
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := LazSyntaxHighlighterClasses{%H-}[TheType].Create(nil);
    SetBothFilextensions('pp;pas;inc;lpr;lrs;dpr;dpk;fpd');
    SampleSource :=
  'program Sample; { Comment with Pasdoc @author someone }'#13+
  '{$mode objfpc}{$R- compiler directive}'#13+
  'type'#13+
  '  TMyData = class abstract'#13+
  '  public'#13+
  '    function GetItem(AnIndex: integer = -1): boolean; virtual; abstract; experimental;'#13+
  '    generic procedure Test<Param1>(foo: Param1);'#13+
  '    property Item[AnIndex: integer]: boolean read GetItem;'#13+
  '  end deprecated ''reason'';'#13+
  '  generic TProc<A: specialize TGen<byte>> = procedure (foo: A); cdecl;'#13+
  ''#13+
  '  generic TSomeGeneric<Param1; Param2: TConstrain; const Val: word> = class(specialize TFoo<Param2>)'#13+
  '  end;'#13+
  ''#13+
  'procedure TForm1.Button1Click(Sender: TObject);'#13+
  'label JumpPos;'#13+
  'const BREAK_CHAR: char = ^C;'#13+
  'var  // Slash Comment'#13+
  '  Number, I, X: Integer;'#13+
  '  MyVar: System.integer = 2 * (1 + 5);'#13+
  '  Text: String; MoreText: AnsiString;'#13+
  '  List: Array of record x,y: Byte; end;'#13+
  'begin'#13+
  '  Number := 12345 * (2 + 9); // << Brackets at caret'#13+
  '  Caption := ''The number is '' + IntToStr(Number);'#13+
  '  asm'#13+
  '    MOV AX,1234h'#13+
  '    MOV Number,AX'#13+
  '  end;'#13+
  '  {%region /fold}'#13+
  '  {%endregion}'#13+
  '  X := 10 + (Number * (ord(''A'') + (I - Abs(X * (I+(1-((X))))))));'#13+
  '  inc(X); {$R+} { Selected text }'#13+
  '  for I := 0 to Number do {$R-} { execution point }'#13+
  '  begin'#13+
  '    Inc(X, 2); {$R+} { Enabled breakpoint }'#13+
  '    Dec(X, 3); {$R+} { Disabled breakpoint }'#13+
  '    {$R-} // { Invalid breakpoint }'#13+
  '    WriteLN(X); {$R-} { Unknown breakpoint }'#13+
  '    X := X + 1.0; {$R-} { Error line }'#13+
  '    case ModalResult o'#13+
  '      mrOK: inc(X)'#13+
  '      mrCancel, mrIgnore: dec(X)'#13+
  '    end'#13+
  '    ListBox1.Items.Add(IntToStr(X)); // TODO: more work'#13+
      '    // A multiline'#13 +
      '    // comment'#13 +
//{ $IFDEF WithSynMarkupIfDef}
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
//{ $ENDIF}
  '  end;'#13+
  'JumpPos:'#13+
  'end;'#13+
  '(* Multiline Ansi-Comment'#13+
  '* Foo Bar'#13+
  '*)'#13+
  ''#13+
  'generic procedure TMyData.Test<Param1>(foo: Param1);'#13+
  'begin {} end;'#13+
  ''#13+
  '{$Mode delphi}'#13+
  'procedure TGenClass<ABC>.Something<XYZ>(a: XYZ);'#13+
  'begin {} end;'#13+
  ''#13+
  '{  Multiline Curly-Comment'#13+
  'Foo Bar'#13+
  '}'#13+
  ''#13 + #13;
    AddAttrSampleLines[ahaDisabledBreakpoint] := 37;
    AddAttrSampleLines[ahaEnabledBreakpoint] := 36;
    AddAttrSampleLines[ahaInvalidBreakpoint] := 38;
    AddAttrSampleLines[ahaUnknownBreakpoint] := 39;
    AddAttrSampleLines[ahaErrorLine] := 40;
    AddAttrSampleLines[ahaExecutionPoint] := 34;
    AddAttrSampleLines[ahaTextBlock] := 33;
    AddAttrSampleLines[ahaFoldedCode] := 30;
    CaretXY := Point(21, 24);
  end;
  Add(NewInfo);

  // create info for pascal
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshDelphi;
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := LazSyntaxHighlighterClasses{%H-}[TheType].Create(nil);
    SetBothFilextensions('pp;pas;inc;lpr;lrs;dpr;dpk;fpd');
    SampleSource :=
  'program Sample; { Comment with Pasdoc @author someone }'#13+
  '{$mode objfpc}{$R- compiler directive}'#13+
  'type'#13+
  '  TMyData = class abstract'#13+
  '  public'#13+
  '    function GetItem(AnIndex: integer = -1): boolean; virtual; abstract; experimental;'#13+
  '    generic procedure Test<Param1>(foo: Param1);'#13+
  '    property Item[AnIndex: integer]: boolean read GetItem;'#13+
  '  end deprecated ''reason'';'#13+
  '  generic TProc<A: specialize TGen<byte>> = procedure (foo: A); cdecl;'#13+
  ''#13+
  '  generic TSomeGeneric<Param1; Param2: TConstrain; const Val: word> = class(specialize TFoo<Param2>)'#13+
  '  end;'#13+
  ''#13+
  'procedure TForm1.Button1Click(Sender: TObject);'#13+
  'label JumpPos;'#13+
  'const BREAK_CHAR: char = ^C;'#13+
  'var  // Slash Comment'#13+
  '  Number, I, X: Integer;'#13+
  '  MyVar: System.integer = 2 * (1 + 5);'#13+
  '  Text: String; MoreText: AnsiString;'#13+
  '  List: Array of record x,y: Byte; end;'#13+
  'begin'#13+
  '  Number := 12345 * (2 + 9); // << Brackets at caret'#13+
  '  Caption := ''The number is '' + IntToStr(Number);'#13+
  '  asm'#13+
  '    MOV AX,1234h'#13+
  '    MOV Number,AX'#13+
  '  end;'#13+
  '  {%region /fold}'#13+
  '  {%endregion}'#13+
  '  X := 10 + (Number * (ord(''A'') + (I - Abs(X * (I+(1-((X))))))));'#13+
  '  inc(X); {$R+} { Selected text }'#13+
  '  for I := 0 to Number do {$R-} { execution point }'#13+
  '  begin'#13+
  '    Inc(X, 2); {$R+} { Enabled breakpoint }'#13+
  '    Dec(X, 3); {$R+} { Disabled breakpoint }'#13+
  '    {$R-} // { Invalid breakpoint }'#13+
  '    WriteLN(X); {$R-} { Unknown breakpoint }'#13+
  '    X := X + 1.0; {$R-} { Error line }'#13+
  '    case ModalResult o'#13+
  '      mrOK: inc(X)'#13+
  '      mrCancel, mrIgnore: dec(X)'#13+
  '    end'#13+
  '    ListBox1.Items.Add(IntToStr(X)); // TODO: more work'#13+
      '    // A multiline'#13 +
      '    // comment'#13 +
//{ $IFDEF WithSynMarkupIfDef}
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
//{ $ENDIF}
  '  end;'#13+
  'JumpPos:'#13+
  'end;'#13+
  '(* Multiline Ansi-Comment'#13+
  '* Foo Bar'#13+
  '*)'#13+
  ''#13+
  'generic procedure TMyData.Test<Param1>(foo: Param1);'#13+
  'begin {} end;'#13+
  ''#13+
  '{$Mode delphi}'#13+
  'procedure TGenClass<ABC>.Something<XYZ>(a: XYZ);'#13+
  'begin {} end;'#13+
  ''#13+
  '{  Multiline Curly-Comment'#13+
  'Foo Bar'#13+
  '}'#13+
  ''#13 + #13;
    AddAttrSampleLines[ahaDisabledBreakpoint] := 37;
    AddAttrSampleLines[ahaEnabledBreakpoint] := 36;
    AddAttrSampleLines[ahaInvalidBreakpoint] := 38;
    AddAttrSampleLines[ahaUnknownBreakpoint] := 39;
    AddAttrSampleLines[ahaErrorLine] := 40;
    AddAttrSampleLines[ahaExecutionPoint] := 34;
    AddAttrSampleLines[ahaTextBlock] := 33;
    AddAttrSampleLines[ahaFoldedCode] := 30;
    CaretXY := Point(21, 24);
  end;
  Add(NewInfo);

  // create info for html
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshHTML;
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := LazSyntaxHighlighterClasses{%H-}[TheType].Create(nil);
    SetBothFilextensions('htm;html;xhtml');
    SampleSource :=
      '<html>'#13 + '<title>Lazarus Sample source for html</title>'#13 +
      '<body bgcolor=#ffffff background="bg.jpg">'#13 +
      '<!-- Comment -->'#13 + '<img src="lazarus.jpg">'#13 +
      '<p>'#13 + '  Some Text'#13 +
      '  Ampersands: &nbsp;F&nbsp;P&nbsp;C'#13 + '</p>'#13 +
      '<invalid_tag>'#13 + '<!-- Selected text -->'#13 +
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
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := LazSyntaxHighlighterClasses{%H-}[TheType].Create(nil);
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
      Add('Reserved word=Reserved word');
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
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := LazSyntaxHighlighterClasses{%H-}[TheType].Create(nil);
    SetBothFilextensions('xml;xsd;xsl;xslt;dtd;lpi;lps;lpk;wsdl;svg');
    SampleSource :=
      '<?xml version="1.0"?>'#13 + '<!DOCTYPE root ['#13 +
      '  ]>'#13 + '<!-- Comment -->'#13 + '<root version="&test;">'#13 +
      '  <![CDATA[ **CDATA section** ]]>'#13 + '</root>'#13 +
      '<!-- Selected text -->'#13 + ''#13 + #13;
    AddAttrSampleLines[ahaTextBlock] := 8;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Element=Reserved word');
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
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := LazSyntaxHighlighterClasses{%H-}[TheType].Create(nil);
    SetBothFilextensions('lfm;dfm;fmx');
    SampleSource :=
      '{ Lazarus Form Definitions }'#13 + 'object TestForm: TTestForm'#13 +
      '  Left = 273'#13 + '  Top = 103'#13 +
      '  Caption = ''sample source'''#13 + 'end'#13 +
      '{ Selected text }'#13 + ''#13 + #13;
    AddAttrSampleLines[ahaTextBlock] := 7;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Element=Reserved word');
      Add('Comment=Comment');
      Add('Identifier=Identifier');
      Add('Key=Reserved word');
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
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := LazSyntaxHighlighterClasses{%H-}[TheType].Create(nil);
    SetBothFilextensions('pl;pm;cgi');
    SampleSource :=
      '#!/usr/bin/perl'#13 + '# Perl sample code'#13 +
      ''#13 + '$i = "10";'#13 + 'print "$ENV{PATH}\n";'#13 +
      '($i =~ /\d+/) || die "Error\n";'#13 + ''#13 +
      '# Selected text'#13 + ''#13 + #13;
    AddAttrSampleLines[ahaTextBlock] := 8;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Identifier=Identifier');
      Add('KeyAttri=Reserved word');
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
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := LazSyntaxHighlighterClasses{%H-}[TheType].Create(nil);
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
      '/* Selected text */'#13#10 + #13#10;
    AddAttrSampleLines[ahaTextBlock] := 12;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Documentation=Comment');
      Add('Identifier=Identifier');
      Add('Reserved word=Reserved word');
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
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := LazSyntaxHighlighterClasses{%H-}[TheType].Create(nil);
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
      '# Selected text'#13#10 + #13#10;
    AddAttrSampleLines[ahaTextBlock] := 12;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Variable=Identifier');
      Add('Key=Reserved word');
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
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := LazSyntaxHighlighterClasses{%H-}[TheType].Create(nil);
    SetBothFilextensions('py;pyw');
    SampleSource :=
      '# Python syntax highlighting'#13#10 +
      'import math'#13#10 + #13#10 +
      '""" Documentation """'#13#10 +
      'def DoSomething(Liste1,Liste2,param3=3):'#13#10 +
      '  for i in Liste1:'#13#10 +
      '    if i in Liste2:'#13#10 +
      '      Liste1.remove(i)'#13#10 +
      '/* Selected text */'#13#10 + #13#10;
    AddAttrSampleLines[ahaTextBlock] := 9;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Identifier=Identifier');
      Add('Documentation=Comment');
      Add('Reserved word=Reserved word');
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
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := LazSyntaxHighlighterClasses{%H-}[TheType].Create(nil);
    SetBothFilextensions('php;php3;php4');
    SampleSource :=
      '<?if ( ($HTTP_HOST == "www.lazarus.com") || ($HTTP_HOST == "lazarus.com") ){'#10 + '   HEADER("Location:http://www.lazarus.freepascal.org/\n\n");'#10
      + '};'#10 + '?>'#10 + #10;
    AddAttrSampleLines[ahaTextBlock] := 8;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Element=Reserved word');
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
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := LazSyntaxHighlighterClasses{%H-}[TheType].Create(nil);
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
      Add('Element=Reserved word');
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
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshCss;
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := TSynCssSyn.Create(nil);
    SetBothFilextensions('css');
    SampleSource :=
      '.field :hover {'#10 +
      '   display:inline;'#10+
      '   border:10px;'#10+
      '   color: #555;'#10+
      '/* comment */'#10+
      '}'#10+#10;
    AddAttrSampleLines[ahaTextBlock] := 4;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Selector=Reserved word');
      Add('Identifier=Identifier');
      Add('Space=Space');
      Add('Symbol=Symbol');
      Add('Number=Number');
      Add('Key=Key');
      Add('String=String');
    end;
    CaretXY := Point(1,1);
  end;
  Add(NewInfo);

  // create info for JScript
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshJScript;
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := LazSyntaxHighlighterClasses{%H-}[TheType].Create(nil);
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
      '/* Selected text */'#13#10 + #13#10;
    AddAttrSampleLines[ahaTextBlock] := 14;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Documentation=Comment');
      Add('Identifier=Identifier');
      Add('Reserved word=Reserved word');
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
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := LazSyntaxHighlighterClasses{%H-}[TheType].Create(nil);
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
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := LazSyntaxHighlighterClasses{%H-}[TheType].Create(nil);
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
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := LazSyntaxHighlighterClasses{%H-}[TheType].Create(nil);
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
    DefaultCommentType := DefaultCommentTypes{%H-}[TheType];
    SynInstance := LazSyntaxHighlighterClasses{%H-}[TheType].Create(nil);
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
  NewInfo.DefaultCommentType := DefaultCommentTypes{%H-}[NewInfo.TheType];
  NewInfo.SynInstance := LazSyntaxHighlighterClasses{%H-}[NewInfo.TheType].Create(nil);
  NewInfo.SetBothFilextensions('pike;pmod');
  NewInfo.SampleSource := TSynPikeSyn.Pike_GetSampleSource();
  with NewInfo do
  begin
    AddAttrSampleLines[ahaTextBlock] := 9;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Documentation=Comment');
      Add('Identifier=Identifier');
      Add('Reserved word=Reserved word');
      Add('Number=Number');
      Add('Space=Space');
      Add('String=String');
      Add('Symbol=Symbol');
    end;
    CaretXY := Point(1,1);
  end;
  Add(NewInfo);

  // create info for text
  NewInfo := TEditOptLanguageInfo.Create;
  NewInfo.TheType := lshText;
  NewInfo.DefaultCommentType := DefaultCommentTypes{%H-}[NewInfo.TheType];
  NewInfo.SynInstance := LazSyntaxHighlighterClasses{%H-}[NewInfo.TheType].Create(nil);
  NewInfo.SetBothFilextensions('txt');
  NewInfo.SampleSource := 'Text in the source editor.'+#13#10+
                          'Example line 2'+#13#10+
                          'Example line 3'+#13#10+
                          'Example line 4'+#13#10;
  with NewInfo do
  begin
    AddAttrSampleLines[ahaTextBlock] := 12;
    MappedAttributes := TStringList.Create;
    CaretXY := Point(1,1);
  end;
  Add(NewInfo);

  // create info for asm Window
  // TODO: move to debugger package
  NewInfo := TEditOptLanguageInfo.Create;
  NewInfo.TheType := lshNone;
  NewInfo.DefaultCommentType := comtNone;
  NewInfo.SynInstance := TIDEAsmWinHighlighter.Create(nil);
  NewInfo.SetBothFilextensions('');
  NewInfo.SampleSource :=
    '0000000100001537 4889C3                   mov rbx,rax'+#13#10+
    '000000010000153A 4889D9                   mov rcx,rbx'+#13#10+
    '000000010000153D E8EE6D0000               call +$00006DEE    # $0000000100008330 fpc_writeln_end text.inc:694'+#13#10+
    '000000010000153A 4889D9                   mov rcx,rbx'+#13#10+
    '000000010000153D E8EE6D0000               call +$00006DEE    # $0000000100008330 fpc_writeln_end text.inc:694'+#13#10+
    '0000000100001537 4889C3                   mov rbx,rax'+#13#10+
    '000000010000153A 4889D9                   mov rcx,rbx'+#13#10+
    '000000010000153D E8EE6D0000               call +$00006DEE    # $0000000100008330 fpc_writeln_end text.inc:694'+#13#10+
    '000000010000153A 4889D9                   mov rcx,rbx'+#13#10+
    '000000010000153D E8EE6D0000               call +$00006DEE    # $0000000100008330 fpc_writeln_end text.inc:694'+#13#10;
  with NewInfo do
  begin
    AddAttrSampleLines[ahaTextBlock] := 5;
    MappedAttributes := TStringList.Create;
    MappedAttributes.Add('ahaAsmSourceLine=Reserved word');
    MappedAttributes.Add('ahaAsmSourceFunc=Reserved word');
    CaretXY := Point(40,4);
  end;
  IdeAsmWinHlId :=
  Add(NewInfo);


  dir := AppendPathDelim(AppendPathDelim(UserSchemeDirectory(False)) + 'tml');
  if DirectoryExistsUTF8(dir) then begin
    FileList := FindAllFiles(dir, '*.json', False);
    for i := 0 to FileList.Count - 1 do begin
      tmlHighlighter := TSynTextMateSyn.Create(nil);
      tmlHighlighter.LoadGrammar(FileList[i], '');
      if (tmlHighlighter.ParserError <> '') then begin
        tmlHighlighter.Free;
        Continue;
      end;

      if (tmlHighlighter.TextMateGrammar.SampleText = '') and
         (tmlHighlighter.TextMateGrammar.SampleTextFile <> '')
      then begin
        StrLoader := TStringStream.Create('');
        try
          StrLoader.LoadFromFile(TrimAndExpandFilename(tmlHighlighter.TextMateGrammar.SampleTextFile, dir));
          tmlHighlighter.TextMateGrammar.SampleText := StrLoader.DataString;
        finally
          StrLoader.Free;
        end;
      end;
      tmlHighlighter.TextMateGrammar.OnGetIncludedGrammar := @DoGetTMLGrammar;

      NewInfo := TEditOptLanguageTextMateInfo.Create;
      TEditOptLanguageTextMateInfo(NewInfo).FileName := FileList[i];
      NewInfo.TheType := lshNone;
      NewInfo.DefaultCommentType := comtNone;
      NewInfo.SynInstance := tmlHighlighter;
      NewInfo.SetBothFilextensions('');
      if (tmlHighlighter.TextMateGrammar.SampleText <> '') then
        NewInfo.SampleSource := tmlHighlighter.TextMateGrammar.SampleText
      else
        NewInfo.SampleSource := 'Text in the source editor.'+#13#10+
                                'Example line 2'+#13#10+
                                'Example line 3'+#13#10+
                                'Example line 4'+#13#10;
      with NewInfo do
      begin
//        AddAttrSampleLines[ahaTextBlock] := 12;
        CaretXY := Point(1,1);
        MappedAttributes := TStringList.Create;
        for j := 0 to tmlHighlighter.AttrCount - 1 do begin
          n := tmlHighlighter.Attribute[j].StoredName+'.';
          if strlicomp(pchar(n), pchar('comment.'), 8) = 0           then MappedAttributes.Add(n+'=Comment');
          if strlicomp(pchar(n), pchar('string.'), 7) = 0            then MappedAttributes.Add(n+'=String');
          if strlicomp(pchar(n), pchar('constant.numeric.'), 17) = 0 then MappedAttributes.Add(n+'=Number');
          if strlicomp(pchar(n), pchar('number.'), 7) = 0            then MappedAttributes.Add(n+'=Number');
          if strlicomp(pchar(n), pchar('keyword.'), 8) = 0           then MappedAttributes.Add(n+'=Reserved word');
          if strlicomp(pchar(n), pchar('key.'), 4) = 0               then MappedAttributes.Add(n+'=Reserved word');
          if strlicomp(pchar(n), pchar('entity.name.'), 12) = 0      then MappedAttributes.Add(n+'=Identifier');
          if strlicomp(pchar(n), pchar('identifier.'), 11) = 0       then MappedAttributes.Add(n+'=Identifier');
        end;
      end;
      Add(NewInfo);

    end;
    FileList.Free;
  end;

  for i := 1 to Count - 1 do begin
    if SharedInstances[i] is TSynTextMateSyn then
      TSynTextMateSyn(SharedInstances[i]).TextMateGrammar.ResolveExternalIncludes;
  end;

end;

destructor TEditOptLangList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  inherited Destroy;
end;

procedure TEditOptLangList.Delete(Index: Integer);
begin
  assert(False, 'TEditOptLangList.Delete: Not allowed - Index is used as ID');
end;

procedure TEditOptLangList.Move(CurIndex, NewIndex: Integer);
begin
  assert(False, 'TEditOptLangList.Move: Not allowed - Index is used as ID');
end;

procedure TEditOptLangList.Exchange(Index1, Index2: Integer);
begin
  assert(False, 'TEditOptLangList.Exchange: Not allowed - Index is used as ID');
end;

function TEditOptLangList.Extract(item: Pointer): Pointer;
begin
  assert(False, 'TEditOptLangList.Extract: Not allowed - Index is used as ID');
  Result := nil;
end;

function TEditOptLangList.GetNewSynInstance(AnID: TIdeSyntaxHighlighterID
  ): TSrcIDEHighlighter;
begin
  Result := Items[AnID].CreateNewSynInstance;
end;

function TEditOptLangList.FindByName(const Name: String): Integer;
begin
  Result := Count - 1;
  while (Result > 0) and (Items[Result].SynInstance.LanguageName <> Name) do
    dec(Result);
  if Result = 0 then // not found // not checking lshNone;
    dec(Result);
end;

function TEditOptLangList.GetDefaultFilextension(AnId: TIdeSyntaxHighlighterID
  ): String;
begin
  if AnId >= 0 then
    Result := Items[AnId].GetDefaultFilextension
  else
    Result := '';
end;

function TEditOptLangList.FindByType(AType: TLazSyntaxHighlighter): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (Items[Result].TheType <> AType) do
    dec(Result);
end;

function TEditOptLangList.GetDefaultFilextension(
  AType: TLazSyntaxHighlighter): String;
var
  i: Integer;
begin
  i := FindByType(AType){%H-};
  if i >= 0 then
    Result := Items[i].GetDefaultFilextension
  else
    Result := '';
end;

function TEditOptLangList.GetInfoByType(AType: TLazSyntaxHighlighter): TEditOptLanguageInfo;
var
  i: LongInt;
begin
  i:=FindByType(AType){%H-};
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
  FExtLinkActions       := TSynEditMouseActions.Create(nil);
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
  FExtLinkActions.Free;
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
  // wheel
  FHorizWheel               := mwaScrollHoriz;
  FCtrlHorizWheel           := mwaNone;
  FAltHorizWheel            := mwaScrollHorizPageLessOne;
  FShiftHorizWheel          := mwaScrollHorizSingleLine;
  FAltCtrlHorizWheel     := mwaNone;
  FShiftCtrlHorizWheel       := mwaNone;
  FShiftAltHorizWheel    := mwaNone;
  FShiftAltCtrlHorizWheel    := mwaNone;
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
  FDeclarationJumpIncludesExtLink := False;
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
    AddCommand(emcToggleBreakPoint,          False, mbXLeft,   ccSingle,    CDir, R, [],                      [ssAlt, SYNEDIT_LINK_MODIFIER]);
    AddCommand(emcToggleBreakPointEnabled,   False, mbXLeft,   ccSingle,    CDir, R, [SYNEDIT_LINK_MODIFIER], [ssAlt, SYNEDIT_LINK_MODIFIER]);
    AddCommand(emcBreakPointProperties,      False, mbXLeft,   ccSingle,    CDir, R, [ssAlt], [ssAlt]);
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
            if DeclarationJumpIncludesExtLink then
              FExtLinkActions.AddCommand(emcPluginExternalLinkDefaultOpen, False, AButton, AClickCount, ADir,    AShift, AShiftMask);
          end;
        mbaOpenExternalLink:
          FExtLinkActions.AddCommand(emcPluginExternalLinkDefaultOpen, False, AButton, AClickCount, ADir,    AShift, AShiftMask);
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
            AddCommand(emcContextMenu, AMoveCaret, AButton, AClickCount, ADir, AShift, AShiftMask, emcoSelectionCaretMoveOutside, 0, 1);
        mbaContextMenuTab:
            AddCommand(emcContextMenu, False,      AButton, AClickCount, ADir, AShift, AShiftMask, emcoSelectionCaretMoveOutside, 0, 2);
        mbaMultiCaretToggle:
          begin
            AddCommand(emcPluginMultiCaretToggleCaret, False, AButton, AClickCount, ADir, AShift, AShiftMask);
            FSelActions.AddCommand(emcPluginMultiCaretSelectionToCarets, False, AButton, AClickCount, ADir, AShift, AShiftMask);
          end;
      end;
    end;
  end;

  procedure AddWheelAct(AnAction: TMouseOptWheelAction; const AShift, AShiftMask: TShiftState; AnHorizontal: Boolean = False);
  var
    opt: TSynEditorMouseCommandOpt;
    opt2: integer;
    mbU, mbD: TSynMouseButton;
  begin
    if AnHorizontal then begin
      mbD := mbXWheelLeft;
      mbU := mbXWheelRight;
    end
    else begin
      mbD := mbXWheelDown;
      mbU := mbXWheelUp;
    end;
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
            AddCommand(emcWheelZoomOut, False,  mbD, ccAny, cdDown, AShift, AShiftMask);
            AddCommand(emcWheelZoomIn,  False,  mbU,   ccAny, cdDown, AShift, AShiftMask);
          end;
      end;

      if AnAction in [mwaScroll, mwaScrollSingleLine, mwaScrollPage, mwaScrollPageLessOne, mwaScrollHalfPage] then begin
        AddCommand(emcWheelVertScrollDown,       False,  mbD, ccAny, cdDown, AShift, AShiftMask, opt, 0, opt2);
        AddCommand(emcWheelVertScrollUp,         False,  mbU,   ccAny, cdDown, AShift, AShiftMask, opt, 0, opt2);
      end;
      if AnAction in [mwaScrollHoriz, mwaScrollHorizSingleLine, mwaScrollHorizPage, mwaScrollHorizPageLessOne, mwaScrollHorizHalfPage] then begin
        AddCommand(emcWheelHorizScrollDown,       False,  mbD, ccAny, cdDown, AShift, AShiftMask, opt, 0, opt2);
        AddCommand(emcWheelHorizScrollUp,         False,  mbU,   ccAny, cdDown, AShift, AShiftMask, opt, 0, opt2);
      end;

    end;
  end;

var
  ModKeys, SelKey: TShiftState;
begin
  FMainActions.Clear;
  FSelActions.Clear;
  FTextActions.Clear;
  FExtLinkActions.Clear;

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

  ModKeys := [];
  if FShiftHorizWheel         <> mwaNone then ModKeys := ModKeys + [ssShift];
  if FCtrlHorizWheel          <> mwaNone then ModKeys := ModKeys + [SYNEDIT_LINK_MODIFIER];
  if FAltHorizWheel           <> mwaNone then ModKeys := ModKeys + [ssAlt];
  if FAltCtrlHorizWheel       <> mwaNone then ModKeys := ModKeys + [ssAlt] + [SYNEDIT_LINK_MODIFIER];
  if FShiftCtrlHorizWheel     <> mwaNone then ModKeys := ModKeys + [ssShift] + [SYNEDIT_LINK_MODIFIER];
  if FShiftAltHorizWheel      <> mwaNone then ModKeys := ModKeys + [ssShift, ssAlt];
  if FShiftAltCtrlHorizWheel  <> mwaNone then ModKeys := ModKeys + [ssShift, ssAlt] + [SYNEDIT_LINK_MODIFIER];
  AddWheelAct(FHorizWheel, [], [], True);
  AddWheelAct(FCtrlHorizWheel,  [ssCtrl],  ModKeys, True);
  AddWheelAct(FAltHorizWheel,   [ssAlt],   ModKeys, True);
  AddWheelAct(FShiftHorizWheel, [ssShift], ModKeys, True);
  AddWheelAct(FAltCtrlHorizWheel,      [ssAlt, ssCtrl], ModKeys, True);
  AddWheelAct(FShiftCtrlHorizWheel,    [ssShift, ssCtrl], ModKeys, True);
  AddWheelAct(FShiftAltHorizWheel,     [ssShift, ssAlt], ModKeys, True);
  AddWheelAct(FShiftAltCtrlHorizWheel, [ssShift, ssAlt, ssCtrl], ModKeys, True);

  if FTextDrag then
    FSelActions.AddCommand(emcStartDragMove, False, mbXLeft, ccSingle, cdDown,
                           [], [ssShift], emcoNotDragedNoCaretOnUp);
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
  FExtLinkActions.Assign      (Src.ExtLinkActions);
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
  Opt: TEditorMouseOptions;
begin
  FName                 := Src.FName;
  FGutterLeft           := Src.GutterLeft;
  FSelectOnLineNumbers  := Src.SelectOnLineNumbers;
  FTextDrag             := Src.TextDrag;
  FTextRightMoveCaret   := Src.TextRightMoveCaret;
  FDeclarationJumpIncludesExtLink := Src.DeclarationJumpIncludesExtLink;
  FSelectedUserScheme   := Src.FSelectedUserScheme;

    // left multi click
  FTextDoubleLeftClick       := Src.TextDoubleLeftClick;
  FTextTripleLeftClick       := Src.TextTripleLeftClick;
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
  // wheel
  FHorizWheel                := Src.HorizWheel;
  FCtrlHorizWheel            := Src.CtrlHorizWheel;
  FAltHorizWheel             := Src.AltHorizWheel;
  FShiftHorizWheel           := Src.ShiftHorizWheel;
  FAltCtrlHorizWheel         := Src.AltCtrlHorizWheel;
  FShiftCtrlHorizWheel       := Src.ShiftCtrlHorizWheel;
  FShiftAltHorizWheel        := Src.ShiftAltHorizWheel;
  FShiftAltCtrlHorizWheel    := Src.ShiftAltCtrlHorizWheel;
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
      Opt := TEditorMouseOptions.Create;
      Opt.Assign(TEditorMouseOptions(Src.FUserSchemes.Objects[i]));
      FUserSchemes.AddObject(Src.FUserSchemes[i], Opt);
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
    Temp.GutterActionsOverViewMarks.Equals(Self.GutterActionsOverViewMarks) and
    Temp.ExtLinkActions.Equals      (Self.ExtLinkActions);
  Temp.Free;
end;

function TEditorMouseOptions.CalcCustomSavedActions: Boolean;
begin
  Result := not IsPresetEqualToMouseActions;
  FCustomSavedActions := Result;
end;

procedure TEditorMouseOptions.LoadFromXmlMouseAct(aXMLConfig: TRttiXMLConfig;
  Path: String; MActions: TSynEditMouseActions; AShowError: Boolean);
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
        if MAct.Command = emcOnMainGutterClick then begin
          MAct.ShiftMask := MAct.ShiftMask + [SYNEDIT_LINK_MODIFIER];
          MAct.Shift     := MAct.Shift + [SYNEDIT_LINK_MODIFIER];
          MAct.Command := emcToggleBreakPointEnabled;
          MActions.Add.Assign(MAct);

          MAct.Shift     := MAct.Shift - [SYNEDIT_LINK_MODIFIER];
          MAct.Command := emcToggleBreakPoint;
        end;

        MActions.Add.Assign(MAct);
      finally
        MActions.DecAssertLock;
      end;
      MActions.AssertNoConflict(MAct);
    except
      MActions.Delete(MActions.Count-1);
      if AShowError then
        IDEMessageDialog(dlgMouseOptErrorDup, dlgMouseOptErrorDupText + LineEnding
                 + Path + 'M' + IntToStr(i) + LineEnding + MAct.DisplayName,
                 mtError, [mbOk]);
    end;
  end;
  Mact.Free;

end;

procedure TEditorMouseOptions.LoadFromXml(aXMLConfig: TRttiXMLConfig; aPath: String;
  aOldPath: String; FileVersion: Integer);
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
    LoadFromXmlMouseAct(aXMLConfig, aPath + 'Main/',          MainActions);
    LoadFromXmlMouseAct(aXMLConfig, aPath + 'MainText/',      TextActions);
    LoadFromXmlMouseAct(aXMLConfig, aPath + 'MainSelection/', SelActions);
    LoadFromXmlMouseAct(aXMLConfig, aPath + 'Gutter/',        GutterActions);
    LoadFromXmlMouseAct(aXMLConfig, aPath + 'GutterFold/',    GutterActionsFold);
    LoadFromXmlMouseAct(aXMLConfig, aPath + 'GutterFoldExp/', GutterActionsFoldExp);
    LoadFromXmlMouseAct(aXMLConfig, aPath + 'GutterFoldCol/', GutterActionsFoldCol);
    LoadFromXmlMouseAct(aXMLConfig, aPath + 'GutterLineNum/', GutterActionsLines);
    LoadFromXmlMouseAct(aXMLConfig, aPath + 'GutterLineChange/', GutterActionsChanges);
    LoadFromXmlMouseAct(aXMLConfig, aPath + 'GutterOverView/',   GutterActionsOverView);
    LoadFromXmlMouseAct(aXMLConfig, aPath + 'GutterOverViewMarks/',   GutterActionsOverViewMarks);
    LoadFromXmlMouseAct(aXMLConfig, aPath + 'ExtLinkActions/',   ExtLinkActions);

    if Version < 1 then begin
      try
        FMainActions.AddCommand(emcWheelVertScrollDown,       False,  mbXWheelDown, ccAny, cdDown, [], []);
        FMainActions.AddCommand(emcWheelVertScrollUp,         False,  mbXWheelUp,   ccAny, cdDown, [], []);
        FMainActions.AddCommand(emcWheelHorizScrollDown,      False,  mbXWheelLeft, ccAny, cdDown, [], []);
        FMainActions.AddCommand(emcWheelHorizScrollUp,        False,  mbXWheelRight,ccAny, cdDown, [], []);
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
    SaveMouseAct(aPath + 'ExtLinkActions/',ExtLinkActions);
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
begin
  LoadFromXmlMouseAct(aXMLConfig, aPath + 'Main/',          MainActions,                     True);
  LoadFromXmlMouseAct(aXMLConfig, aPath + 'MainText/',      TextActions,                     True);
  LoadFromXmlMouseAct(aXMLConfig, aPath + 'MainSel/',       SelActions,                      True);
  LoadFromXmlMouseAct(aXMLConfig, aPath + 'Gutter/',        GutterActions,                   True);
  LoadFromXmlMouseAct(aXMLConfig, aPath + 'GutterFold/',    GutterActionsFold,               True);
  LoadFromXmlMouseAct(aXMLConfig, aPath + 'GutterFoldExp/', GutterActionsFoldExp,            True);
  LoadFromXmlMouseAct(aXMLConfig, aPath + 'GutterFoldCol/', GutterActionsFoldCol,            True);
  LoadFromXmlMouseAct(aXMLConfig, aPath + 'GutterLineNum/', GutterActionsLines,              True);
  LoadFromXmlMouseAct(aXMLConfig, aPath + 'GutterLineChange/', GutterActionsChanges,         True);
  LoadFromXmlMouseAct(aXMLConfig, aPath + 'GutterOverView/',   GutterActionsOverView,        True);
  LoadFromXmlMouseAct(aXMLConfig, aPath + 'GutterOverViewMarks/',GutterActionsOverViewMarks, True);
  LoadFromXmlMouseAct(aXMLConfig, aPath + 'ExtLinkActions/',ExtLinkActions, True);
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
  SaveMouseAct(aPath + 'ExtLinkActions/',ExtLinkActions);
  MAct.Free;
end;

procedure TEditorMouseOptions.LoadUserSchemes;
var
  i, j, c: Integer;
  FileList: TStringList;
  XMLConfig: TRttiXMLConfig;
  n: String;
  mo: TEditorMouseOptions;
begin
  ClearUserSchemes;
  if DirectoryExistsUTF8(UserSchemeDirectory(False)) then begin
    FileList := FindAllFiles(UserSchemeDirectory(False), '*.xml', False);
    for i := 0 to FileList.Count - 1 do begin
      XMLConfig := nil;
      try
        XMLConfig := TRttiXMLConfig.Create(FileList[i]);
        c := XMLConfig.GetValue('Lazarus/MouseSchemes/Names/Count', 0);
        for j := 1 to c do begin
          n := XMLConfig.GetValue('Lazarus/MouseSchemes/Names/Item'+IntToStr(j)+'/Value', '');
          if n <> '' then begin
            mo := TEditorMouseOptions.Create;
            mo.FName := n;
            mo.ImportFromXml(XMLConfig, 'Lazarus/MouseSchemes/Scheme' + n + '/');
            FUserSchemes.AddObject(n, mo);
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
  Result := FUserSchemes.IndexOf(SchemeName);
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
  if DirectoryExistsUTF8(UserSchemeDirectory(False)) then begin
    FileList := FindAllFiles(UserSchemeDirectory(False), '*.xml', False);
    for i := 0 to FileList.Count - 1 do begin
      XMLConfig := nil;
      try
        XMLConfig := TRttiXMLConfig.Create(FileList[i]);
        c := XMLConfig.GetValue('Lazarus/MouseSchemes/Names/Count', 0);
        for j := 1 to c do begin
          n := XMLConfig.GetValue('Lazarus/MouseSchemes/Names/Item'+IntToStr(j)+'/Value', '');
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

{ TEditorOptionsBase }

constructor TEditorOptionsBase.Create;
begin
  inherited Create;
  FScrollOnEditLeftOptions := TSynScrollOnEditLeftOptions.Create;
  FScrollOnEditRightOptions := TSynScrollOnEditRightOptions.Create;

  FGutterPartMarks := TEditorSynGutterOptions.Create(0, TSynGutterMarks);
  FGutterPartLine  := TEditorSynGutterOptions.Create(1, TSynGutterLineNumber);
  FGutterPartChange:= TEditorSynGutterOptions.Create(2, TSynGutterChanges);
  FGutterPartSep   := TEditorSynGutterOptions.Create(3, TSynGutterSeparator);
  FGutterPartFold  := TEditorSynGutterOptions.Create(4, TSynGutterCodeFolding);

  FGutterPartOver  := TEditorSynGutterOptions.Create(0, TSynGutterLineOverview);

  FGutterPartList := TEditorSynGutterOptionsList.Create(True);
  FGutterRightPartList := TEditorSynGutterOptionsList.Create(True);
  FGutterPartList.Add(FGutterPartMarks);
  FGutterPartList.Add(FGutterPartLine);
  FGutterPartList.Add(FGutterPartChange);
  FGutterPartList.Add(FGutterPartSep);
  FGutterPartList.Add(FGutterPartFold);
  FGutterRightPartList.Add(FGutterPartOver);

  FWordWrapHLList := TStringList.Create;
  FWordWrapHLList.Duplicates := dupIgnore;
  FWordWrapHLList.Sorted := True;
  FWordWrapHLList.CaseSensitive := False;
end;

destructor TEditorOptionsBase.Destroy;
begin
  FreeAndNil(FGutterPartList);
  FreeAndNil(FGutterRightPartList);
  FreeAndNil(FScrollOnEditLeftOptions);
  FreeAndNil(FScrollOnEditRightOptions);
  FreeAndNil(FWordWrapHLList);
  inherited Destroy;
end;

procedure TEditorOptionsBase.InitForRttiXmlConf;
begin
  // General options
  fMultiLineTab := False;
  fTabPosition := tpTop;
  fMultiCaretOnColumnSelect := True;
  fMultiCaretDefaultMode := mcmMoveAllCarets;
  fMultiCaretDefaultColumnSelectMode := mcmCancelOnCaretMove;
  fMultiCaretDeleteSkipLineBreak := False;
  fExportHtmlWithBackground := False;
  // Display options
  fTopInfoView := True;
  // hints
  fDbgHintAutoTypeCastClass := True;
  fDbgHintUseBackendDebugConverter := True;
  // Code folding
  fReverseFoldPopUpOrder := True;
  // wordwrap
  FWordWrapMinWidth := 10;
  FWordWrapIndentUseOffset := True;
  FWordWrapIndentMax := 8;
  // pas highlighter
  fPasExtendedKeywordsMode := False;
  fPasStringKeywordMode := spsmDefault;
  FCaseLabelAttriMatchesElseOtherwise := True;
  FDeclaredTypeAttributeMode := tamIdentifierOnly;
  FDeclaredValueAttributeMode := tamIdentifierOnly;
  FGenericParamAttrMode := tamIdentifierOnly;
  FDeclaredValueAttributeMachesStringNum := False;
  FProcHeaderNameDeclMode := pnmGenericOnly;
  FProcHeaderNameImplMode := pnmProcNameOnly;
  // Multi window
  fCtrlMiddleTabClickClosesOthers := True;
  fMiddleTabClickClosesOthersModifier := [ssCtrl];
  fMiddleTabClickClosesToRightModifier := [];
  TabFont := '';
  TabFontSize := 0;
  TabFontDisableAntialiasing := DefaultEditorDisableAntiAliasing;
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
  FStringAlignPattern  := '';
  FStringAlignMax      := 0;

  FAutoBraceModes := [];
  FAutoBraceFilterOpen := '';
  FAutoBraceFilterClose := '';

  FScrollPastEolMode := optScrollPage;

  FElasticTabsMinWidth := 1;
end;

function TEditorOptionsBase.GetTabPosition: TTabPosition;
begin
  Result := fTabPosition;
end;

{ TEditorOptionsDefaults }

constructor TEditorOptionsDefaults.Create;
begin
  inherited Create;
  InitForRttiXmlConf;
end;

{ TEditorOptions }

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
  if EdOptsChangedHandlers <> nil then
    EdOptsChangedHandlers.CallNotifyEvents(nil);
end;

procedure TEditorOptions.DoAfterRead;
begin
  inherited DoAfterRead;
  if EdOptsChangedHandlers <> nil then
    EdOptsChangedHandlers.CallNotifyEvents(nil);
end;

constructor TEditorOptions.Create;
var
  ConfFileName: String;
begin
  inherited Create;
  InitLocale;
  ConfFileName := AppendPathDelim(GetPrimaryConfigPath) + EditOptsConfFileName;
  CopySecondaryConfigFile(EditOptsConfFileName);
  try
    if not FileExistsUTF8(ConfFileName) then
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
  InitForRttiXmlConf;
  Init;
  // code templates (dci file)
  fCodeTemplateFileNameRaw :=
    TrimFilename(AppendPathDelim(GetPrimaryConfigPath)+DefaultCodeTemplatesFilename);
  CopySecondaryConfigFile(DefaultCodeTemplatesFilename);
  FMultiWinEditAccessOrder := TEditorOptionsEditAccessOrderList.Create;
  FMultiWinEditAccessOrder.InitDefaults;
  fCompletionLongLineHintType := DefaultCompletionLongLineHintType;
  // Default values for RttiXmlConfig
  FDefaultValues := TEditorOptionsDefaults.Create;
end;

destructor TEditorOptions.Destroy;
begin
  FreeAndNil(FDefaultValues);
  FreeAndNil(fMultiWinEditAccessOrder);
  FreeAndNil(fUserDefinedColors);
  FreeAndNil(fUserColorSchemeGroup);
  fUserMouseSettings.Free;
  fTempMouseSettings.Free;
  fKeyMap.Free;
  XMLConfig.Free;
  inherited Destroy;
end;

procedure TEditorOptions.Init;
begin
  // General options
  fSynEditOptions := SynEditDefaultOptions;
  fSynEditOptions2 := SynEditDefaultOptions2;
  fShowFileNameInCaption := False;
  fShowTabCloseButtons := True;
  fHideSingleTabInWindow := False;
  fCopyWordAtCursorOnCopyNone := True;
  fShowGutterHints := True;
  fBlockIndent := 2;
  FBlockTabIndent := 0;
  fBlockIndentType := sbitSpace;
  fTrimSpaceType := settEditLine;
  fUndoLimit := 32767;
  fTabWidth := 8;
  fElasticTabsMinWidth := 1;
  fBracketHighlightStyle := sbhsBoth;
  // Display options
  fEditorFont := SynDefaultFontName;
  fEditorFontSize := SynDefaultFontSize;
  fDisableAntialiasing := DefaultEditorDisableAntiAliasing;
  // Key Mappings
  fKeyMappingScheme := KeyMapSchemeNames[kmsLazarus];
  fKeyMap := TKeyCommandRelationList.Create;
  // Mouse Mappings
  fUserMouseSettings := TEditorMouseOptions.Create;
  fTempMouseSettings := TEditorMouseOptions.Create;
  fUserMouseSettings.LoadUserSchemes;
  // Color options
  FUserColorSchemeGroup := TColorSchemeFactory.Create;
  FUserColorSchemeGroup.Assign(ColorSchemeFactory); // Copy from global singleton.
  fUserDefinedColors := TEditorUserDefinedWordsList.Create;
  fUserDefinedColors.UseGlobalIDECommandList := True;
  // Markup Current Word
  fMarkupCurWordTime := 1500;
  fMarkupCurWordFullLen := 3;
  fMarkupCurWordNoKeyword := True;
  fMarkupCurWordTrim := True;
  fMarkupCurWordNoTimer := False;
  // Code Tools
  fAutoDisplayFuncPrototypes := True;
end;

function TEditorOptions.GetHighlighterList: TEditOptLangList;
begin
  Result := EditorOptions.HighlighterList;
end;

function TEditorOptions.GetCodeTemplateFileNameExpand: String;
begin
  Result := fCodeTemplateFileNameRaw;
  IDEMacros.SubstituteMacros(result);
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
  OldGutterSeparatorIndex: Int64;
  OldShowLineNumbers: Boolean;
begin
  try
    FileVersion:=XMLConfig.GetValue('EditorOptions/Version', EditorOptsFormatVersion);

    XMLConfig.ReadObject('EditorOptions/Misc/', Self, FDefaultValues);
    RepairEditorFontSize(FTabFontSize);

    // general options
    DefOpts := SynEditDefaultOptions;
    if (FileVersion < 10) then DefOpts := DefOpts - [eoTabIndent];
    for SynEditOpt := Low(TSynEditorOption) to High(TSynEditorOption) do
    begin
      SynEditOptName := GetSynEditOptionName(SynEditOpt);
      if SynEditOptName <> '' then
        if XMLConfig.GetValue('EditorOptions/General/Editor/'+SynEditOptName,SynEditOpt in DefOpts) then
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
        eoCaretMoveEndsSelection, eoPersistentCaretStopBlink, eoNoScrollOnSelectRange,
        eoBookmarkRestoresScroll:
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
      XMLConfig.GetValue('EditorOptions/General/Editor/ShowTabCloseButtons', True);
    FHideSingleTabInWindow :=
      XMLConfig.GetValue('EditorOptions/General/Editor/HideSingleTabInWindow', False);
    fShowTabNumbers :=
      XMLConfig.GetValue('EditorOptions/General/Editor/ShowTabNumbers', False);
    FCopyWordAtCursorOnCopyNone :=
      XMLConfig.GetValue('EditorOptions/General/Editor/CopyWordAtCursorOnCopyNone', True);
    FShowGutterHints :=
      XMLConfig.GetValue('EditorOptions/General/Editor/ShowGutterHints', True);
    fUndoAfterSave :=
      XMLConfig.GetValue('EditorOptions/General/Editor/UndoAfterSave', True);
    fFindTextAtCursor :=
      XMLConfig.GetValue('EditorOptions/General/Editor/FindTextAtCursor', True);
    fUseSyntaxHighlight :=
      XMLConfig.GetValue('EditorOptions/General/Editor/UseSyntaxHighlight', True);
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
    XMLConfig.ReadObject('EditorOptions/GutterParts/Marks/',  FGutterPartMarks,  FGutterPartMarks.Defaults);
    XMLConfig.ReadObject('EditorOptions/GutterParts/Line/',   FGutterPartLine,   FGutterPartLine.Defaults);
    XMLConfig.ReadObject('EditorOptions/GutterParts/Change/', FGutterPartChange, FGutterPartChange.Defaults);
    XMLConfig.ReadObject('EditorOptions/GutterParts/Sep/',    FGutterPartSep,    FGutterPartSep.Defaults);
    XMLConfig.ReadObject('EditorOptions/GutterParts/Fold/',   FGutterPartFold,   FGutterPartFold.Defaults);
    XMLConfig.ReadObject('EditorOptions/GutterParts/Over/',   FGutterPartOver,   FGutterPartOver.Defaults);
    FGutterPartList.Sort;
    FGutterRightPartList.Sort;

    fVisibleRightMargin :=
      XMLConfig.GetValue('EditorOptions/Display/VisibleRightMargin', True);
    fVisibleGutter :=
      XMLConfig.GetValue('EditorOptions/Display/VisibleGutter', True);
    if FileVersion<4 then begin
      OldShowLineNumbers :=
        XMLConfig.GetValue('EditorOptions/Display/ShowLineNumbers', False);
      if OldShowLineNumbers then
        FGutterPartLine.Visible := True;
      fShowOnlyLineNumbersMultiplesOf :=
        XMLConfig.GetValue('EditorOptions/Display/ShowOnlyLineNumbersMultiplesOf', 1);
    end else begin
      OldShowLineNumbers :=
        XMLConfig.GetValue('EditorOptions/Display/ShowLineNumbers', True);
      if not OldShowLineNumbers then
        FGutterPartLine.Visible := False;
      fShowOnlyLineNumbersMultiplesOf :=
        XMLConfig.GetValue('EditorOptions/Display/ShowOnlyLineNumbersMultiplesOf', 5);
    end;

    OldGutterSeparatorIndex :=
      XMLConfig.GetValue('EditorOptions/Display/GutterSeparatorIndex', 3);
    if OldGutterSeparatorIndex = -1 then
      FGutterPartSep.Visible := False
    else
    if OldGutterSeparatorIndex <> 3 then begin
      for i := OldGutterSeparatorIndex+1 to FGutterPartList.Count - 1 do
        FGutterPartList[i].Index := FGutterPartList[i].Index + 1;
      FGutterPartSep.Index := OldGutterSeparatorIndex;
      FGutterPartList.Sort;
    end;

    if not XMLConfig.GetValue('EditorOptions/Misc/ShowOverviewGutter', True) then
      FGutterPartOver.Visible := False;


    fGutterWidth :=
      XMLConfig.GetValue('EditorOptions/Display/GutterWidth', 30);
    fRightMargin :=
      XMLConfig.GetValue('EditorOptions/Display/RightMargin', 80);
    fEditorFont  :=
      XMLConfig.GetValue('EditorOptions/Display/EditorFont', SynDefaultFontName);
    if FileVersion < 8 then begin
      fEditorFontSize :=
        XMLConfig.GetValue('EditorOptions/Display/EditorFontHeight', SynDefaultFontHeight);
      fEditorFontSize := FontHeightToSize(fEditorFontSize);
    end else begin
      fEditorFontSize :=
        XMLConfig.GetValue('EditorOptions/Display/EditorFontSize', SynDefaultFontSize);
    end;
    RepairEditorFontSize(fEditorFontSize);
    fExtraCharSpacing :=
      XMLConfig.GetValue('EditorOptions/Display/ExtraCharSpacing', 0);
    fExtraLineSpacing :=
      XMLConfig.GetValue('EditorOptions/Display/ExtraLineSpacing', 1);
    fDisableAntialiasing :=
      XMLConfig.GetValue('EditorOptions/Display/DisableAntialiasing', FileVersion<7);
    FDoNotWarnForFont :=
      XMLConfig.GetValue('EditorOptions/Display/DoNotWarnForFont', '');

    // Key Mappings options
    LoadCustomKeySchemas;
    fKeyMappingScheme :=
      XMLConfig.GetValue('EditorOptions/KeyMapping/Scheme', KeyMapSchemeNames[kmsLazarus]);
    if not fKeyMap.LoadFromXMLConfig(XMLConfig
      , 'EditorOptions/KeyMapping/' + fKeyMappingScheme + '/') then
      fKeyMappingScheme := KeyMapSchemeNames[kmsLazarus];

    // Color options
    for i := IdeHighlighterStartId to HighlighterList.Count - 1 do
      HighlighterList[i].FileExtensions :=
        XMLConfig.GetValue('EditorOptions/Color/Lang' +
        StrToValidXMLName(HighlighterList[i].SynInstance.LanguageName) +
        '/FileExtensions/Value', HighlighterList[i].DefaultFileExtensions)
      // color attributes are stored in the highlighters
    ;

    FUserDefinedColors.LoadFromXMLConfig(xmlconfig, 'EditorOptions/UserDefinedColors');

    FMarkupCurWordTime :=
      XMLConfig.GetValue('EditorOptions/Display/MarkupCurrentWord/Time', 1500);
    FMarkupCurWordFullLen :=
      XMLConfig.GetValue('EditorOptions/Display/MarkupCurrentWord/FullLen', 3);
    // check deprecated value
    if not XMLConfig.GetValue('EditorOptions/Display/MarkupCurrentWord/FullWord', True) then
      FMarkupCurWordFullLen := 0;
    XMLConfig.DeleteValue('EditorOptions/Display/MarkupCurrentWord/FullWord');
    FMarkupCurWordNoKeyword :=
      XMLConfig.GetValue('EditorOptions/Display/MarkupCurrentWord/NoKeyword', True);
    FMarkupCurWordTrim :=
      XMLConfig.GetValue('EditorOptions/Display/MarkupCurrentWord/Trim', True);
    FMarkupCurWordNoTimer :=
      XMLConfig.GetValue('EditorOptions/Display/MarkupCurrentWord/NoTimer', False);
    FShowFileNameInCaption :=
      XMLConfig.GetValue('EditorOptions/Display/ShowFileNameInCaption', False);

    // Code Tools options
    fAutoBlockCompletion :=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoBlockCompletion', True);
    fAutoDisplayFuncPrototypes :=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoDisplayFuncPrototypes', True);
    fAutoCodeParameters :=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoCodeParameters', True);
    fAutoToolTipExprEval :=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoToolTipExprEval', True);
    fAutoToolTipSymbTools :=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoToolTipSymbTools', True);
    fAutoDelayInMSec    :=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoDelayInMSec', 1000);
    fAutoHintDelayInMSec    :=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoDelayHintInMSec', 1000);
    fCodeTemplateFileNameRaw :=
      XMLConfig.GetValue('EditorOptions/CodeTools/CodeTemplateFileName'
      , TrimFilename(AppendPathDelim(GetPrimaryConfigPath) + DefaultCodeTemplatesFilename));
    fCTemplIndentToTokenStart :=
      XMLConfig.GetValue('EditorOptions/CodeTools/CodeTemplateIndentToTokenStart/Value', False);
    fAutoRemoveEmptyMethods :=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoRemoveEmptyMethods', False);
    FCompletionLongLineHintInMSec :=
      XMLConfig.GetValue('EditorOptions/CodeTools/CompletionLongLineHintInMSec', 0);
    // Read from old location
    XMLConfig.GetValue('EditorOptions/Misc/CompletionLongLineHintType', ord(DefaultCompletionLongLineHintType),
      FCompletionLongLineHintType, TypeInfo(TSynCompletionLongHintType));
    // Read from new location / old value as default
    XMLConfig.GetValue('EditorOptions/CodeTools/CompletionLongLineHintType', ord(FCompletionLongLineHintType),
      FCompletionLongLineHintType, TypeInfo(TSynCompletionLongHintType));
    // Code Folding
    FUseCodeFolding :=
      XMLConfig.GetValue('EditorOptions/CodeFolding/UseCodeFolding', True);
    FUseMarkupWordBracket :=
      XMLConfig.GetValue('EditorOptions/CodeFolding/UseMarkupWordBracket', True);
    FUseMarkupOutline :=
      XMLConfig.GetValue('EditorOptions/CodeFolding/UseMarkupOutline', False);

    FUserMouseSettings.LoadFromXml(XMLConfig, 'EditorOptions/Mouse/',
                                  'EditorOptions/General/Editor/', FileVersion);

    FMultiWinEditAccessOrder.LoadFromXMLConfig(XMLConfig, 'EditorOptions/MultiWin/');
    UserColorSchemeGroup.LoadFromXml(XMLConfig, 'EditorOptions/Color/',
      ColorSchemeFactory, 'EditorOptions/Display/');
    for i := IdeHighlighterStartId to HighlighterList.Count - 1 do
      GetHighlighterSettings(HighlighterList.SharedSynInstances[i]);

    //Wordwrap
    FWordWrapHLList.DelimitedText :=
      XMLConfig.GetValue('EditorOptions/Misc/WordWrapHighlighters', '');

  except
    on E: Exception do
      DebugLn('[TEditorOptions.Load] ERROR: ', e.Message);
  end;
  if FileVersion < 13 then
    if not CtrlMiddleTabClickClosesOthers then // user set option to false
      MiddleTabClickClosesOthersModifier := [];
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

    XMLConfig.WriteObject('EditorOptions/GutterParts/Marks/',  FGutterPartMarks,  FGutterPartMarks.Defaults);
    XMLConfig.WriteObject('EditorOptions/GutterParts/Line/',   FGutterPartLine,   FGutterPartLine.Defaults);
    XMLConfig.WriteObject('EditorOptions/GutterParts/Change/', FGutterPartChange, FGutterPartChange.Defaults);
    XMLConfig.WriteObject('EditorOptions/GutterParts/Sep/',    FGutterPartSep,    FGutterPartSep.Defaults);
    XMLConfig.WriteObject('EditorOptions/GutterParts/Fold/',   FGutterPartFold,   FGutterPartFold.Defaults);
    XMLConfig.WriteObject('EditorOptions/GutterParts/Over/',   FGutterPartOver,   FGutterPartOver.Defaults);

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
        eoCaretMoveEndsSelection, eoPersistentCaretStopBlink, eoNoScrollOnSelectRange,
        eoBookmarkRestoresScroll:
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
    XMLConfig.DeleteValue('EditorOptions/Display/ShowLineNumbers');
    XMLConfig.SetDeleteValue('EditorOptions/Display/ShowOnlyLineNumbersMultiplesOf',
      fShowOnlyLineNumbersMultiplesOf, 5);
    XMLConfig.SetDeleteValue('EditorOptions/Display/GutterWidth',
      fGutterWidth, 30);
    XMLConfig.DeleteValue('EditorOptions/Display/GutterSeparatorIndex');
    XMLConfig.DeleteValue('EditorOptions/Misc/ShowOverviewGutter');
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
    for i := IdeHighlighterStartId to HighlighterList.Count - 1 do
      XMLConfig.SetDeleteValue('EditorOptions/Color/Lang' +
        StrToValidXMLName(HighlighterList[i].SynInstance.LanguageName) +
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
    // Remove an old value from Misc section. Done in Lazarus 3.99, March 2024.
    XMLConfig.DeleteValue('EditorOptions/Misc/ShowFileNameInCaption');

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
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/AutoDelayHintInMSec'
      , fAutoHintDelayInMSec, 1000);
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/CodeTemplateFileName'
      , fCodeTemplateFileNameRaw, '');
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/CodeTemplateIndentToTokenStart/Value'
      , fCTemplIndentToTokenStart, False);
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/AutoRemoveEmptyMethods'
      , fAutoRemoveEmptyMethods, False);
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/CompletionLongLineHintInMSec',
      FCompletionLongLineHintInMSec, 0);
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/CompletionLongLineHintType',
      FCompletionLongLineHintType, ord(DefaultCompletionLongLineHintType), TypeInfo(TSynCompletionLongHintType));
    // Remove an old (buggy) values. Done in Lazarus 3.99, March 2024.
    XMLConfig.DeleteValue('EditorOptions/CodeTools/CompletionLongLineHintTypeCompletionLongLineHintType');
    XMLConfig.DeleteValue('EditorOptions/Misc/CompletionLongLineHintType');

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

    //WordWrap
    XMLConfig.SetDeleteValue('EditorOptions/Misc/WordWrapHighlighters'
      , FWordWrapHLList.DelimitedText, '');

    InvalidateFileStateCache;
    XMLConfig.Flush;
  except
    on E: Exception do
      DebugLn('[TEditorOptions.Save] ERROR: ', e.Message);
  end;
end;

function TEditorOptions.LoadCodeTemplates(AnAutoComplete: TSynEditAutoComplete
  ): TModalResult;

  function ResourceDCIAsText: String;
  var
    data: TResourceStream;
    i: Int64;
  begin
    data := TResourceStream.Create(HInstance, PChar('codetemplates'), PChar(RT_RCDATA));
    i := data.Size;
    SetLength(Result{%H-}, i);
    if i > 0 then
      data.Read(Result[1], i);
    data.Free;
  end;

var
  s: String;
  FileVersion, i, j, v: Integer;
  NewAutoComplete: TSynEditAutoComplete;
  Added: Boolean;
  Template: TTemplate;
begin
  s := CodeTemplateFileNameExpand;
  Result := mrAbort;
  if FileExistsUTF8(s) then begin
    try
      AnAutoComplete.CodeTemplSource.LoadFromFile(s);
      Result := mrOK;
    except
      Result := mrAbort;
    end;
    if Result = mrAbort then
      exit;

    FileVersion := AnAutoComplete.CodeTemplates.Count;
    if (FileVersion > 0) then begin
      Template := AnAutoComplete.CodeTemplates[0];
      FileVersion := Template.Attributes.IndexOfName(DciFileVersionName);
      if (FileVersion >= 0) then
        FileVersion := StrToIntDef(Template.Attributes.ValueFromIndex[FileVersion], 0);
    end;
    if FileVersion < DciFileVersion then begin
      // Merge new entries
      NewAutoComplete := TSynEditAutoComplete.Create(nil);
      NewAutoComplete.CodeTemplSource.Text := ResourceDCIAsText;
      Added := False;
      for i := 0 to NewAutoComplete.CodeTemplates.Count - 1 do begin
        Template := NewAutoComplete.CodeTemplates[i];
        j := Template.Attributes.IndexOfName(DciVersionName);
        if j < 0 then
          continue;
        v := StrToIntDef(Template.Attributes.ValueFromIndex[j], 0);
        if v <= FileVersion then
          continue;
        if AnAutoComplete.CodeTemplates.ByKey(Template.Key) <> Nil then
          continue;
        AnAutoComplete.AddCompletion(Template);
        Added := True;
      end;
      NewAutoComplete.Free;
      if Added then
        if BuildBorlandDCIFile(AnAutoComplete) then
          SaveCodeTemplates(AnAutoComplete);
    end;
  end
  else begin
    AnAutoComplete.CodeTemplSource.Text := ResourceDCIAsText;
  end;
end;

function TEditorOptions.SaveCodeTemplates(AnAutoComplete: TSynEditAutoComplete
  ): TModalResult;
begin
  try
    AnAutoComplete.CodeTemplSource.SaveToFile(CodeTemplateFileNameExpand);
    Result := mrOK;
  except
    Result := mrAbort;
  end;
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

function TEditorOptions.GetColorSchemeLanguage(aHighLighter: TSynCustomHighlighter;
  SynColorSchemeName: String): TColorSchemeLanguage;
var
  Scheme: TColorScheme;
begin
  Result := nil;
  // initialize with defaults
  if SynColorSchemeName = '' then
    SynColorSchemeName := ReadColorScheme(aHighLighter.LanguageName);
  if (SynColorSchemeName = '') then
    exit;
  Scheme := UserColorSchemeGroup.ColorSchemeGroup[SynColorSchemeName];
  if Scheme = nil then
    exit;
  Result := Scheme.ColorSchemeBySynHl[aHighLighter];
end;

function TEditorOptions.ReadColorScheme(const LanguageName: String): String;
(* The name of the currently chosen color-scheme for that language *)
begin
  if LanguageName = '' then
    Exit(ColorSchemeFactory.ColorSchemeGroupAtPos[0].Name);
  if LanguageName <> TPreviewPasSyn.GetLanguageName then
    Result := XMLConfig.GetValue(
      'EditorOptions/Color/Lang' + StrToValidXMLName(LanguageName) + '/ColorScheme/Value', '')
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
    Result := XMLConfig.GetValue('EditorOptions/Color/Lang' +
      StrToValidXMLName(TPreviewPasSyn.GetLanguageName) + '/ColorScheme/Value', '')
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
  XMLConfig.SetValue('EditorOptions/Color/Lang' + StrToValidXMLName(LanguageName) +
                     '/ColorScheme/Value', SynColorScheme);
  XMLConfig.SetValue('EditorOptions/Color/Version', EditorOptsFormatVersion);
end;

procedure TEditorOptions.ReadHighlighterSettings(Syn: TSrcIDEHighlighter;
  SynColorScheme: String);
// if SynColorScheme='' then default ColorScheme will be used
var
  LangScheme: TColorSchemeLanguage;
begin
  LangScheme := GetColorSchemeLanguage(Syn, SynColorScheme);
  if LangScheme = nil then
    exit;
  LangScheme.ApplyTo(Syn);
end;

procedure TEditorOptions.ReadHighlighterFoldSettings(Syn: TSrcIDEHighlighter;
  ReadForOptions: Boolean);
var
  Path, ValidLang: String;
  i, h, idx: Integer;
  FoldRec: TEditorOptionsFoldRecord;
  FoldInf: TEditorOptionsFoldInfo;
  DefHl, FoldHl: TSynCustomFoldHighlighter;
begin
  h := HighlighterList.FindByName(Syn.LanguageName);
  if h < 0 then exit;

  if Syn is TSynCustomFoldHighlighter then begin
    FoldHl := TSynCustomFoldHighlighter(Syn);
    DefHl := TSynCustomFoldHighlighter(TCustomSynClass(Syn.ClassType).Create(nil));
    try
      ReadDefaultsForHighlighterFoldSettings(DefHl);
      ValidLang := StrToValidXMLName(Syn.LanguageName);
      FoldRec := EditorOptionsFoldDefaults[HighlighterList[h].TheType];
      for i := 0 to FoldRec.Count - 1 do begin
        FoldInf := FoldRec.Info[i];
        idx := FoldInf.Index;
        Path := 'EditorOptions/FoldConfig/Lang' + ValidLang + '/Type' + FoldInf.Xml + '/' ;
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
  h := HighlighterList.FindByName(Syn.LanguageName);
  if h < 0 then exit;
  if (syn is TSynCustomFoldHighlighter) then begin
    TheFoldInfo := EditorOptionsFoldDefaults[HighlighterList[h].TheType];
    for i := 0 to TheFoldInfo.Count - 1 do
      with TSynCustomFoldHighlighter(Syn).FoldConfig[TheFoldInfo.Info[i].Index] do
        Enabled := TheFoldInfo.Info[i].Enabled;
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
  h := HighlighterList.FindByName(Syn.LanguageName);
  if h < 0 then
    exit;

  DefSyn := TCustomSynClass(Syn.ClassType).Create(Nil);
  try
    ReadDefaultsForHighlighterFoldSettings(DefSyn);

    if (syn is TSynCustomFoldHighlighter) then begin
      TheFoldInfo := EditorOptionsFoldDefaults[HighlighterList[h].TheType];
      for i := 0 to TheFoldInfo.Count - 1 do begin
        ConfName := TheFoldInfo.Info[i].Xml;
        Path := 'EditorOptions/FoldConfig/Lang' +
          StrToValidXMLName(Syn.LanguageName) + '/Type' + ConfName + '/' ;
        XMLConfig.DeletePath(Path + 'Enabled/');
        XMLConfig.WriteObject(Path + 'Settings/',
          TSynCustomFoldHighlighter(Syn).FoldConfig[TheFoldInfo.Info[i].Index],
          TSynCustomFoldHighlighter(DefSyn).FoldConfig[TheFoldInfo.Info[i].Index]);
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
  h := HighlighterList.FindByName(Syn.LanguageName);
  if h < 0 then exit;
  TheInfo := EditorOptionsDividerDefaults[HighlighterList[h].TheType];

  ReadDefaultsForHighlighterDivDrawSettings(Syn);

  // read settings, that are different from the defaults
  for i := 0 to TheInfo.Count - 1 do begin
    Conf := Syn.DividerDrawConfig[i];
    ConfName := TheInfo.Info[i].Xml;
    Path := 'EditorOptions/DividerDraw/Lang' + Syn.LanguageName + '/Type' + ConfName + '/' ;
    Conf.MaxDrawDepth := XMLConfig.GetValue(Path + 'MaxDepth/Value', Conf.MaxDrawDepth);
    Conf.TopColor := XMLConfig.GetValue(Path + 'TopColor/Value', Conf.TopColor);
    Conf.NestColor := XMLConfig.GetValue(Path + 'NestColor/Value', Conf.NestColor);
  end;
end;

procedure TEditorOptions.ReadDefaultsForHighlighterDivDrawSettings(Syn: TSrcIDEHighlighter);
var
  TheInfo: TEditorOptionsDividerRecord;
  i, h: Integer;
begin
  h := HighlighterList.FindByName(Syn.LanguageName);
  if h < 0 then exit;
  TheInfo := EditorOptionsDividerDefaults[HighlighterList[h].TheType];
  for i := 0 to TheInfo.Count - 1 do begin
    Syn.DividerDrawConfig[i].MaxDrawDepth := TheInfo.Info[i].MaxLeveL;
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
  h := HighlighterList.FindByName(Syn.LanguageName);
  if h < 0 then exit;
  TheInfo := EditorOptionsDividerDefaults[HighlighterList[h].TheType];

  DefSyn := TCustomSynClass(Syn.ClassType).Create(Nil);
  try
    ReadDefaultsForHighlighterDivDrawSettings(DefSyn);
    for i := 0 to TheInfo.Count - 1 do begin
      Conf := Syn.DividerDrawConfig[i];
      DefConf := DefSyn.DividerDrawConfig[i]; // default value
      ConfName := TheInfo.Info[i].Xml;
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

procedure TEditorOptions.GetHighlighterObjSettings(Syn: TObject);
begin
  GetHighlighterSettings(TSrcIDEHighlighter(Syn));
end;

procedure TEditorOptions.GetHighlighterSettings(Syn: TSrcIDEHighlighter);
// read highlight settings from config file
begin
  syn.BeginUpdate;
  try
    ReadHighlighterSettings(Syn, '');
    ReadHighlighterFoldSettings(Syn);
    ReadHighlighterDivDrawSettings(Syn);
    if Syn is TSynPasSyn then begin
      TSynPasSyn(Syn).ExtendedKeywordsMode := PasExtendedKeywordsMode;
      TSynPasSyn(Syn).StringKeywordMode := PasStringKeywordMode;
      TSynPasSyn(Syn).CaseLabelAttriMatchesElseOtherwise    := FCaseLabelAttriMatchesElseOtherwise;
      TSynPasSyn(Syn).DeclaredTypeAttributeMode             := FDeclaredTypeAttributeMode;
      TSynPasSyn(Syn).DeclaredValueAttributeMode            := FDeclaredValueAttributeMode;
      TSynPasSyn(Syn).DeclaredValueAttributeMachesStringNum := FDeclaredValueAttributeMachesStringNum;
      TSynPasSyn(Syn).GenericConstraintAttributeMode        := FGenericParamAttrMode;
      TSynPasSyn(Syn).SpecializeParamAttributeMode          := FGenericParamAttrMode;
      case FProcHeaderNameDeclMode of
        pnmGenericOnly:        TSynPasSyn(Syn).ProcNameIntfAttributeMode := [pamDots];
        pnmGenericAndProcName: TSynPasSyn(Syn).ProcNameIntfAttributeMode := [pamDots, pamGenParamKeyword, pamGenParamSym, pamGenParamSeparator];
        pnmProcNameOnly:       TSynPasSyn(Syn).ProcNameIntfAttributeMode := [pamSupressGenParamAttr, pamDots, pamGenParamKeyword, pamGenParamSym, pamGenParamSeparator];
        pnmPlain:              TSynPasSyn(Syn).ProcNameIntfAttributeMode := [pamSupressGenParamAttr, pamDots];
      end;
      case FProcHeaderNameImplMode of
        pnmGenericOnly:        TSynPasSyn(Syn).ProcNameImplAttributeMode := [pamDots];
        pnmGenericAndProcName: TSynPasSyn(Syn).ProcNameImplAttributeMode := [pamDots, pamGenParamKeyword, pamGenParamSym, pamGenParamSeparator];
        pnmProcNameOnly:       TSynPasSyn(Syn).ProcNameImplAttributeMode := [pamSupressGenParamAttr, pamDots, pamGenParamKeyword, pamGenParamSym, pamGenParamSeparator];
        pnmPlain:              TSynPasSyn(Syn).ProcNameImplAttributeMode := [pamSupressGenParamAttr, pamDots];
      end;
    end;
  finally
    Syn.EndUpdate;
  end;
end;

procedure TEditorOptions.GetSynEditorSettings(ASynEdit: TObject;
  SimilarEdit: TObject);
begin
  GetSynEditSettings(ASynEdit as TSynEdit, SimilarEdit as TSynEdit);
end;

procedure TEditorOptions.SetMarkupColor(Syn : TSrcIDEHighlighter;
  AddHilightAttr : TAdditionalHilightAttribute; aMarkup : TLazEditTextAttributeModifier);
var
  SchemeGrp: TColorScheme;
  Scheme: TColorSchemeLanguage;
  Attrib: TColorSchemeAttribute;
begin
  if assigned(Syn) then begin
    Scheme := GetColorSchemeLanguage(Syn);
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

procedure TEditorOptions.SetMarkupColors(aSynEd: TSynEdit);
var
  Scheme: TColorSchemeLanguage;
  TmpHl: TIDESynTextSyn;
  Attri, AttriNum: TColorSchemeAttribute;
  i: Integer;
begin
  // Find current color scheme for default colors
  if (aSynEd.Highlighter = nil) then begin
    TmpHl := TIDESynTextSyn.Create(nil);
    Scheme := GetColorSchemeLanguage(TmpHl);
    if Assigned(Scheme) then begin
      Scheme.ApplyTo(aSynEd);
    end
    else begin
      aSynEd.Color := clWhite;
      aSynEd.Font.Color := clBlack;
    end;
    TmpHl.Free;
    exit;
  end;
  // get current colorscheme:
  Scheme := GetColorSchemeLanguage(aSynEd.Highlighter);
  if Assigned(Scheme) then Scheme.ApplyTo(aSynEd);

  Attri := Scheme.AttributeByEnum[ahaGutterCurrentLine];
  AttriNum := Scheme.AttributeByEnum[ahaGutterNumberCurrentLine];
  for i := 0 to GutterPartList.Count - 1 do
    GutterPartList[i].ApplyLineColorTo(aSynEd.Gutter.Parts.ByClass[GutterPartList[i].GClass, 0], Attri, AttriNum);
  for i := 0 to GutterRightPartList.Count - 1 do
    GutterRightPartList[i].ApplyLineColorTo(aSynEd.Gutter.Parts.ByClass[GutterRightPartList[i].GClass, 0], Attri, AttriNum);
end;

procedure TEditorOptions.ApplyFontSettingsTo(ASynEdit: TSynEdit);
begin
  ASynEdit.Font.BeginUpdate;
  ASynEdit.Font.Size := fEditorFontSize;// set size before name for XLFD !
  ASynEdit.Font.Name := fEditorFont;
  if fDisableAntialiasing then
    ASynEdit.Font.Quality := fqNonAntialiased
  else
    ASynEdit.Font.Quality := fqDefault;
  ASynEdit.Font.EndUpdate;
end;

procedure TEditorOptions.ApplyTabFontSettingsTo(APageCtrl: TPageControl);
begin
  if FTabFont <> '' then begin
    if FTabFontSize < 0 then
      APageCtrl.Font.Height := -FTabFontSize
    else
      APageCtrl.Font.Size := FTabFontSize;
    APageCtrl.Font.Name := FTabFont;
  end
  else begin
    APageCtrl.Font.Size := 0;
    APageCtrl.Font.Name := 'default';
  end;
  if FTabFontDisableAntialiasing then
    APageCtrl.Font.Quality := fqNonAntialiased
  else
    APageCtrl.Font.Quality := fqDefault;
end;

procedure TEditorOptions.GetSynEditSettings(ASynEdit: TSynEdit; SimilarEdit: TSynEdit;
  AHighlighterId: TIdeSyntaxHighlighterID);
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

    UpdateSynEditSettingsForHighlighter(ASynEdit, AHighlighterId);

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
      b.StringAlignPattern := FStringAlignPattern;
      b.StringAlignMax     := FStringAlignMax;

    end;

    if ElasticTabs then begin
      ASynEdit.TabViewClass := TSynEditStringDynTabExpander;
      ASynEdit.Options := fSynEditOptions - [eoSmartTabs, eoTabsToSpaces];
      TSynEditStringDynTabExpander(ASynEdit.TextViewsManager.SynTextViewByClass[TSynEditStringDynTabExpander])
        .MinTabWidth := FElasticTabsMinWidth;
    end else begin
      ASynEdit.TabViewClass := TSynEditStringTabExpander;
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
        mw.Terms.IncChangeNotifyLock;
        mw.Clear;
        mw.Terms.Assign(TermsConf);
        mw.RestoreLocalChanges;
        mw.Terms.DecChangeNotifyLock;
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

      TIDESynEditor(ASynEdit).AutoBraces.Modes := FAutoBraceModes;
      TIDESynEditor(ASynEdit).AutoBraces.FilterOpenTokens  := FAutoBraceFilterOpen;
      TIDESynEditor(ASynEdit).AutoBraces.FilterCloseTokens := FAutoBraceFilterClose;
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
    ASynEdit.Gutter.LineNumberPart(0).ShowOnlyLineNumbersMultiplesOf :=
      fShowOnlyLineNumbersMultiplesOf;
    if ASynEdit is TIDESynEditor then
      TIDESynEditor(ASynEdit).ShowTopInfo := TopInfoView;

    ASynEdit.Gutter.CodeFoldPart.Visible := FUseCodeFolding;
    if not FUseCodeFolding then
      ASynEdit.UnfoldAll;
    ASynEdit.Gutter.CodeFoldPart.ReversePopMenuOrder := ReverseFoldPopUpOrder;

    ASynEdit.Gutter.Width := fGutterWidth;

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

    if (ASynEdit.Gutter.SeparatorPart <> nil) and
       (abs(FGutterPartSep.Index - FGutterPartLine.Index) = 1) and
       FGutterPartLine.Visible
    then
      ASynEdit.Gutter.SeparatorPart.MouseActions.Assign(FUserMouseSettings.GutterActionsLines)
    else
    if (ASynEdit.Gutter.SeparatorPart <> nil) and (abs(FGutterPartSep.Index - FGutterPartChange.Index) = 1) then
      ASynEdit.Gutter.SeparatorPart.MouseActions.Assign(FUserMouseSettings.GutterActionsChanges);
    if ASynEdit.RightGutter.LineOverviewPart <> nil then begin
      ASynEdit.RightGutter.LineOverviewPart.MouseActions.Assign(FUserMouseSettings.GutterActionsOverView);
      ASynEdit.RightGutter.LineOverviewPart.MouseActionsForMarks.Assign(FUserMouseSettings.GutterActionsOverViewMarks);
    end;
    if (ASynEdit is TIDESynEditor) and (TIDESynEditor(ASynEdit).ExternalHttpLink <> nil) then
      TIDESynEditor(ASynEdit).ExternalHttpLink.MouseActions.Assign(fUserMouseSettings.ExtLinkActions);

    GutterPartList.Sort;
    for i := 0 to GutterPartList.Count - 1 do begin
      GutterPartList[i].ApplyTo(ASynEdit.Gutter.Parts.ByClass[GutterPartList[i].GClass, 0]);
      GutterPartList[i].ApplyIndexTo(ASynEdit.Gutter.Parts.ByClass[GutterPartList[i].GClass, 0]);
    end;
    GutterRightPartList.Sort;
    for i := 0 to GutterRightPartList.Count - 1 do begin
      GutterRightPartList[i].ApplyTo(ASynEdit.RightGutter.Parts.ByClass[GutterRightPartList[i].GClass, 0]);
      //TODO: currently separators are not managed => index is not correct
      //GutterRightPartList[i].ApplyIndexTo(ASynEdit.RightGutter.Parts.ByClass[GutterRightPartList[i].GClass, 0]);
    end;
    for i := 0 to ASynEdit.RightGutter.Parts.ByClassCount[TSynGutterSeparator] - 1 do
      ASynEdit.RightGutter.Parts.ByClass[TSynGutterSeparator, i].Visible := FGutterPartOver.Visible;

    ASynEdit.ScrollOnEditLeftOptions.Assign(ScrollOnEditLeftOptions);
    ASynEdit.ScrollOnEditRightOptions.Assign(ScrollOnEditRightOptions);
  finally
    ASynEdit.EndUpdate;
  end;
end;

procedure TEditorOptions.UpdateSynEditSettingsForHighlighter(
  ASynEdit: TSynEdit; AHighlighterId: TIdeSyntaxHighlighterID);
begin
  if (ASynEdit is TIDESynEditor) and WordWrapEnabled and ((FWordWrapHLList = nil) or
    (FWordWrapHLList.Count = 0) or (AHighlighterId = IdeHighlighterUnknownId) or
    (FWordWrapHLList.IndexOf(HighlighterList.Names[AHighlighterId]) < 0)) then
  begin
    TIDESynEditor(ASynEdit).WordWrapEnabled := True;
    ASynEdit.Options  := ASynEdit.Options  - [eoScrollPastEol];
    ASynEdit.Options2 := ASynEdit.Options2 - [eoScrollPastEolAddPage, eoScrollPastEolAutoCaret];

    TIDESynEditor(ASynEdit).WordWrapCaretWrapPos  := WordWrapCaretWrapPos;
    TIDESynEditor(ASynEdit).WordWrapForceHomeEnd  := FWordWrapForceHomeEnd;
    TIDESynEditor(ASynEdit).WordWrapMinWidth      := WordWrapMinWidth;
    if WordWrapFixedWidth then
      TIDESynEditor(ASynEdit).WordWrapMaxWidth      := WordWrapMinWidth
    else
      TIDESynEditor(ASynEdit).WordWrapMaxWidth      := WordWrapMaxWidth;
    TIDESynEditor(ASynEdit).WordWrapIndent           := WordWrapIndent;
    TIDESynEditor(ASynEdit).WordWrapIndentUseOffset  := WordWrapIndentUseOffset;
    TIDESynEditor(ASynEdit).WordWrapIndentMin        := WordWrapIndentMin;
    TIDESynEditor(ASynEdit).WordWrapIndentMax        := WordWrapIndentMax;
    TIDESynEditor(ASynEdit).WordWrapIndentMaxRel     := WordWrapIndentMaxRel;
  end
  else
  begin
    if ASynEdit is TIDESynEditor then
      TIDESynEditor(ASynEdit).WordWrapEnabled := False;
    if eoScrollPastEol in fSynEditOptions then
    case FScrollPastEolMode of
      optScrollFixed: ASynEdit.Options2 := ASynEdit.Options2 + [eoScrollPastEolAutoCaret];
      optScrollPage: begin
          ASynEdit.Options  := ASynEdit.Options  - [eoScrollPastEol];
          ASynEdit.Options2 := ASynEdit.Options2 + [eoScrollPastEolAddPage, eoScrollPastEolAutoCaret];
        end;
      optScrollNone: begin
          ASynEdit.Options  := ASynEdit.Options  - [eoScrollPastEol];
          ASynEdit.Options2 := ASynEdit.Options2 + [eoScrollPastEolAutoCaret];
        end;
    end;
  end;
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

{ TIDEAsmWinHighlighter }

constructor TIDEAsmWinHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FreeHighlighterAttributes;
  AddAttribute( TSynHighlighterAttributes.Create(@dlgAddHiAttrDefault, 'ahaDefault')  );
  AddAttribute( TSynHighlighterAttributes.Create(@dbgAsmWindowSourceLine, 'ahaAsmSourceLine')  );
  AddAttribute( TSynHighlighterAttributes.Create(@dbgAsmWindowSourceFunc, 'ahaAsmSourceFunc')  );
  AddAttribute( TSynHighlighterAttributesModifier.Create(@AdditionalHighlightAttributes[ahaTextBlock], GetAddiHilightAttrName(ahaTextBlock)) );
  AddAttribute( TSynHighlighterAttributesModifier.Create(@AdditionalHighlightAttributes[ahaLineHighlight], GetAddiHilightAttrName(ahaLineHighlight)) );
  AddAttribute( TSynHighlighterAttributesModifier.Create(@AdditionalHighlightAttributes[ahaMouseLink], GetAddiHilightAttrName(ahaMouseLink)) );
  AddAttribute( TSynHighlighterAttributesModifier.Create(@dbgAsmWindowLinkTarget, 'ahaAsmLinkTarget') );
end;

class function TIDEAsmWinHighlighter.GetLanguageName: string;
begin
  Result := 'Disassembler Window';
end;

{ TColorSchemeAttribute }

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

procedure TColorSchemeAttribute.ApplyTo(aDest: TObject);
begin
  ApplyTo(aDest as TLazEditTextAttribute, nil);
end;

procedure TColorSchemeAttribute.Init;
begin
  inherited Init;
  FAttrFeatures := [hafBackColor, hafForeColor, hafFrameColor, hafStyle, hafFrameStyle, hafFrameEdges, hafPrior];
  FMarkupFoldLineColor := clNone;
  FMarkupAllOverviewColor := clNone;
  FMarkupFoldLineStyle := slsSolid;
  FMarkupFoldLineAlpha := 0;
end;

procedure TColorSchemeAttribute.AssignColorsFrom(ASource: TLazCustomEditTextAttribute);
var
  SrcAttr: TColorSchemeAttribute absolute ASource;
begin
  inherited AssignColorsFrom(ASource);
  if ASource is TColorSchemeAttribute then begin
    FUseSchemeGlobals    := SrcAttr.FUseSchemeGlobals;
    FMarkupFoldLineColor := SrcAttr.FMarkupFoldLineColor;
    FMarkupFoldLineStyle := SrcAttr.FMarkupFoldLineStyle;
    FMarkupFoldLineAlpha := SrcAttr.FMarkupFoldLineAlpha;
    FMarkupAllOverviewColor := SrcAttr.FMarkupAllOverviewColor;
  end;
end;

function TColorSchemeAttribute.GetIsUsingSchemeGlobals: Boolean;
begin
  Result := FUseSchemeGlobals and (GetSchemeGlobal <> nil);
end;

procedure TColorSchemeAttribute.SetMarkupAllOverviewColor(AValue: TColor);
begin
  if FMarkupAllOverviewColor = AValue then Exit;
  FMarkupAllOverviewColor := AValue;
  Changed;
end;

function TColorSchemeAttribute.GetGroupName: String;
begin
  if FGroup = agnRegistered then
    Result := RegisteredAttribGroupNames[FRegisteredGroup]^
  else
    Result := AdditionalHighlightGroupNames[Group];
end;

function TColorSchemeAttribute.GetSchemeGlobal: TColorSchemeAttribute;
begin
  if FAlreadyGotSchemeGlobal then
    Result := FSchemeGlobalCache
  else begin
    Result := nil;
    if (FOwner <> nil) and (FOwner.FOwner <> nil) and
       (FOwner.FOwner.FDefaultColors <> nil)
    then
      Result := FOwner.FOwner.FDefaultColors.Attribute[StoredName];
    if Result = Self then
      Result := nil;
    FSchemeGlobalCache := Result;
    FAlreadyGotSchemeGlobal := True;
  end;
end;

constructor TColorSchemeAttribute.Create(ASchemeLang: TColorSchemeLanguage;
  attribName: PString; const aStoredName: String);
begin
  inherited Create(attribName, aStoredName);
  FOwner := ASchemeLang;
  FUseSchemeGlobals := True;
end;

function TColorSchemeAttribute.IsEnabled: boolean;
begin
  Result := (inherited IsEnabled) or (FMarkupFoldLineColor <> clNone) or
            (FMarkupAllOverviewColor <> clNone);
end;

procedure TColorSchemeAttribute.ApplyTo(aDest: TLazEditTextAttribute;
  aDefault: TColorSchemeAttribute);
// aDefault (if supplied) is usually the Schemes agnDefault / DefaultAttribute
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
    aDest.Features   := Src.Features;
    if aDest is TLazEditTextAttributeModifier then begin
      if hafStyleMask in Src.AttrFeatures then
        aDest.StyleMask  := Src.StyleMask
      else
        aDest.StyleMask  := [low(TFontStyle)..high(TFontStyle)];
    end;

    if aDest is TLazEditTextAttributeModifier then begin
      TLazEditTextAttributeModifier(aDest).ForeAlpha := Src.ForeAlpha;
      TLazEditTextAttributeModifier(aDest).BackAlpha := Src.BackAlpha;
      TLazEditTextAttributeModifier(aDest).FrameAlpha := Src.FrameAlpha;
      if aDest is TSynHighlighterLazCustomPasAttribute then begin
        TSynHighlighterLazCustomPasAttribute(aDest).CustomWords.Assign(CustomWords);
        TSynHighlighterLazCustomPasAttribute(aDest).CustomWordTokenKind := CustomWordTokenKind;
      end;
      if aDest.StoredName = '' then aDest.StoredName := Src.StoredName;
    end;

    if hafPrior in Src.AttrFeatures then begin
      aDest.ForePriority      := Src.ForePriority;
      aDest.BackPriority      := Src.BackPriority;
      aDest.FramePriority     := Src.FramePriority;
      aDest.BoldPriority      := Src.BoldPriority;
      aDest.ItalicPriority    := Src.ItalicPriority;
      aDest.UnderlinePriority := Src.UnderlinePriority;
    end;

    if not (aDest is TLazEditTextAttributeModifier) then begin
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

      if aDest is TColorSchemeAttribute then
        TColorSchemeAttribute(aDest).Group := Src.Group;
    end;
  finally
    aDest.EndUpdate;
  end;
end;

procedure TColorSchemeAttribute.Assign(Src: TPersistent);
var
  SrcAttr: TColorSchemeAttribute;
begin
  if Src is TLazCustomEditTextAttribute then
    AssignSupportedFeaturesFrom(TLazCustomEditTextAttribute(Src));
  inherited Assign(Src);
  FAttrFeatures := [hafBackColor, hafForeColor, hafFrameColor,
                hafStyle, hafFrameStyle, hafFrameEdges, hafPrior];
  if Src is TSynHighlighterLazCustomPasAttribute then
    FAttrFeatures := FAttrFeatures + [hafCustomWords];
  if Src is TLazEditTextAttributeModifier then
    FAttrFeatures := FAttrFeatures + [hafAlpha, hafStyleMask];

  if Src is TColorSchemeAttribute then begin
    SrcAttr := TColorSchemeAttribute(Src);
    FGroup               := SrcAttr.FGroup;
    FUseSchemeGlobals    := SrcAttr.FUseSchemeGlobals;
    FAttrFeatures            := SrcAttr.FAttrFeatures;
    FMarkupFoldLineColor := SrcAttr.FMarkupFoldLineColor;
    FMarkupFoldLineStyle := SrcAttr.FMarkupFoldLineStyle;
    FMarkupFoldLineAlpha := SrcAttr.FMarkupFoldLineAlpha;
    FDefaultSynFeatures  := SrcAttr.FDefaultSynFeatures;
    FMarkupAllOverviewColor := SrcAttr.FMarkupAllOverviewColor;
    FRegisteredGroup     := SrcAttr.FRegisteredGroup;
  end;
end;

function TColorSchemeAttribute.Equals(Other: TColorSchemeAttribute): Boolean;
begin
  Result := (FGroup      = Other.FGroup) and
            (FUseSchemeGlobals = Other.FUseSchemeGlobals) and
            // ignore resourcestring Name and Caption
            (StoredName  = Other.StoredName) and
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
            (AttrFeatures   = Other.AttrFeatures);
end;

function TColorSchemeAttribute.GetStoredValuesForAttrib: TColorSchemeAttribute;
var
  csl: TColorSchemeLanguage;
begin
  Result := nil;
  if FOwner <> nil then begin
    csl := FOwner.GetStoredValuesForLanguage;
    if csl <> nil then
      Result := csl.Attribute[StoredName];
  end;
end;

procedure TColorSchemeAttribute.LoadFromXml(aXMLConfig: TRttiXMLConfig;
  const aPath: String; Defaults: TColorSchemeAttribute; Version: Integer);
var
  Path: String;
begin
  // FormatVersion >= 5
  (* Note: This is currently always called with a default, so the nil handling isn't needed*)
  Assert(Version > 4, 'TColorSchemeAttribute.LoadFromXml: Version ('+IntToStr(Version)+' < 5.');
  if StoredName = '' then exit;
  Path := aPath + StrToValidXMLName(StoredName) + '/';
  if aXMLConfig.HasPath(Path, False) then begin
    aXMLConfig.ReadObject(Path, Self, Defaults);
    CustomWords.Text := aXMLConfig.GetValue(Path+'CustomWords', '');
  end
  else begin
    if (Defaults <> Self) and (Defaults <> nil) then begin
      // do not copy (Stored)Name or AttrFeatures ...
      Background := Defaults.Background;
      Foreground := Defaults.Foreground;
      FrameColor := Defaults.FrameColor;
      FrameEdges := Defaults.FrameEdges;
      FrameStyle := Defaults.FrameStyle;
      Style      := Defaults.Style;
      StyleMask  := Defaults.StyleMask;
      UseSchemeGlobals  := Defaults.UseSchemeGlobals;
      ForePriority      := Defaults.ForePriority;
      BackPriority      := Defaults.BackPriority;
      FramePriority     := Defaults.FramePriority;
      BoldPriority      := Defaults.BoldPriority;
      ItalicPriority    := Defaults.ItalicPriority;
      UnderlinePriority := Defaults.UnderlinePriority;
      CustomWords.Text  := Defaults.CustomWords.Text;
    end;
  end;
end;

procedure TColorSchemeAttribute.SaveToXml(aXMLConfig: TRttiXMLConfig; const aPath: String;
  Defaults: TColorSchemeAttribute);
var
  AttriName: String;
  Path: String;
begin
  if StoredName = '' then
    exit;
  // Delete Version <= 4
  AttriName := OldAdditionalAttributeName(StoredName);
  if AttriName <> '' then
    aXMLConfig.DeletePath(aPath + StrToValidXMLName(AttriName));

  Path := aPath + StrToValidXMLName(StoredName) + '/';
  aXMLConfig.WriteObject(Path, Self, Defaults);
  aXMLConfig.SetDeleteValue(Path + 'CustomWords', CustomWords.Text, '');
end;

{ TColorSchemeLanguage }

function TColorSchemeLanguage.GetAttribute(const Index: String): TColorSchemeAttribute;
var
  Idx: Integer;
begin
  Idx := FAttributes.IndexOf(Index);
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
  Result := Attribute[GetAddiHilightAttrName(Index)];
end;

function TColorSchemeLanguage.GetName: String;
begin
  Result := FOwner.Name;
end;

function TColorSchemeLanguage.DoesSupportGroup(AGroup: TAhaGroupName): boolean;
begin
  Result := True;
  if (FHighlighter = nil) then Exit;

  if FHighlighter is TNonSrcIDEHighlighter then begin
    Result := AGroup in [agnDefault, agnLanguage];
    exit;
  end;

  case AGroup of
//    agnDefault: ;
//    agnLanguage: ;
//    agnText: ;
//    agnLine: ;
//    agnGutter: ;
//    agnTemplateMode: ;
//    agnSyncronMode: ;
    agnIfDef: Result := FHighlighter is TSynPasSyn;
//    agnIdentComplWindow: ;
    agnOutlineColors: Result := FHighlighter is TSynCustomFoldHighlighter;
  end;
end;

function TColorSchemeLanguage.GetSupportsFileExt: Boolean;
begin
  Result := (FHighlighter = nil) or not(FHighlighter is TNonSrcIDEHighlighter);
end;

function TColorSchemeLanguage.GetAttributeIntf(AnIndex: integer): IColorSchemeAttribute;
begin
  Result := GetAttributeAtPos(AnIndex);
end;

function TColorSchemeLanguage.GetAttributeIntf(const AStoredName: string): IColorSchemeAttribute;
begin
  Result := GetAttribute(AStoredName);
end;

function TColorSchemeLanguage.GetStoredValuesForLanguage: TColorSchemeLanguage;
var
  cs: TColorScheme;
begin
  Result := nil;
  if FOwner <> nil then begin
    cs := FOwner.GetStoredValuesForScheme;
    if cs <> nil then
      //Result := cs.ColorScheme[FIdeHighlighterID];
      Result := cs.ColorSchemeBySynHl[FHighlighter];
  end;
end;

constructor TColorSchemeLanguage.Create(AGroup: TColorScheme;
  AIdeHighlighterID: TIdeSyntaxHighlighterID; IsSchemeDefault: Boolean);
begin
  inherited Create;
  FIsSchemeDefault := IsSchemeDefault;
  FAttributes := TQuickStringlist.Create;
  FOwner := AGroup;
  FHighlighter := nil;
  FIdeHighlighterID := AIdeHighlighterID;
  FHighlighter := HighlighterList.SharedSynInstances[AIdeHighlighterID];
  if FHighlighter <> nil then
    FLanguageName := FHighlighter.LanguageName;
end;

constructor TColorSchemeLanguage.CreateWithDefColor(AGroup: TColorScheme;
  AIdeHighlighterID: TIdeSyntaxHighlighterID; IsSchemeDefault: Boolean);
begin
  Create(AGroup, AIdeHighlighterID, IsSchemeDefault);
  FDefaultAttribute := TColorSchemeAttribute.Create(Self, @dlgAddHiAttrDefault, 'ahaDefault');
  FDefaultAttribute.AttrFeatures := [hafBackColor, hafForeColor];
  FDefaultAttribute.Group := agnDefault;
  FAttributes.AddObject(FDefaultAttribute.StoredName, FDefaultAttribute);
end;

constructor TColorSchemeLanguage.CreateFromXml(AGroup: TColorScheme;
  AIdeHighlighterID: TIdeSyntaxHighlighterID; aXMLConfig: TRttiXMLConfig;
  const aPath: String; IsSchemeDefault: Boolean;
  aPascalScheme: TColorSchemeLanguage; MappedAttributes: TStringList);
var
  hla: TLazEditTextAttribute;
  csa, pasattr: TColorSchemeAttribute;
  aha: TAdditionalHilightAttribute;
  FormatVersion, i: Integer;
  TmpPath, n: String;
begin
  CreateWithDefColor(AGroup, AIdeHighlighterID, IsSchemeDefault); // don't call inherited Create

  FAttributes.Sorted := False;
  if FHighlighter <> nil then begin
    for i := 0 to FHighlighter.AttrCount - 1 do begin
      hla := FHighlighter.Attribute[i];
      if hla.StoredName = FDefaultAttribute.StoredName then continue;
      csa := TColorSchemeAttribute.Create(Self, hla.Caption, hla.StoredName);
      csa.Assign(hla);
      csa.Group := agnLanguage;
      csa.FDefaultSynFeatures := csa.Features;
      if (FHighlighter <> nil) and (FHighlighter is TNonSrcIDEHighlighter) then
        if hla is TSynHighlighterLazCustomPasAttribute then
          csa.AttrFeatures := [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafStyle, hafStyleMask, hafCustomWords]
        else
        if hla is TLazEditTextAttributeModifier then
          csa.AttrFeatures := [hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior, hafStyle, hafStyleMask]
        else
          csa.AttrFeatures := [hafBackColor, hafForeColor, hafFrameColor, hafStyle];
      FAttributes.AddObject(csa.StoredName, csa);
    end;
  end;

  for aha := Low(TAdditionalHilightAttribute) to High(TAdditionalHilightAttribute) do begin
    if aha = ahaNone then continue;
    if not DoesSupportGroup(ahaGroupMap[aha]) then continue;
    csa := TColorSchemeAttribute.Create(Self, @AdditionalHighlightAttributes[aha],
                                        GetAddiHilightAttrName(aha) );
    csa.AttrFeatures := ahaSupportedFeatures[aha];
    csa.Group    := ahaGroupMap[aha];
    if aha in AdditionalHighlightAttrWithPastEolFeature then begin
      csa.UpdateSupportedFeatures([lafPastEOL], []);
      if aha in AdditionalHighlightAttrWithPastEolEnabled then
        csa.Features := [lafPastEOL];
    end;
    csa.FDefaultSynFeatures := csa.Features;
    FAttributes.AddObject(csa.StoredName, csa);
  end;
  FAttributes.Sorted := true;

  if (not FIsSchemeDefault) and (aPascalScheme <> nil) and (MappedAttributes <> nil) then begin
    TmpPath := aPath + 'Lang' + StrToValidXMLName(FLanguageName) + '/'  + 'Scheme' + StrToValidXMLName(Name) + '/';
    if not aXMLConfig.HasPath(TmpPath, False) then begin
      for i := 0 to FAttributes.Count - 1 do begin
        csa := TColorSchemeAttribute(FAttributes.Objects[i]);
        n := MappedAttributes.Values[csa.StoredName];
        if (n <> '') then begin
          pasattr := aPascalScheme.Attribute[n];
          if pasattr <> nil then
            csa.AssignColors(pasattr);
        end;
      end;
      exit;
    end;
  end;

  FormatVersion := aXMLConfig.GetValue(aPath + 'Version', 0);
  LoadFromXml(aXMLConfig, aPath, nil, FormatVersion);
end;

destructor TColorSchemeLanguage.Destroy;
begin
  Clear;
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
  Attr, SrcAttr: TColorSchemeAttribute;
  NewList: TQuickStringlist;
begin
  // Do not clear old list => external references to Attributes may exist
  FIdeHighlighterID := Src.FIdeHighlighterID;
  FLanguageName := Src.FLanguageName;
  FHighlighter := Src.FHighlighter;
  //FDefaultAttribute.Assign(Src.FDefaultAttribute);
  FDefaultAttribute := nil;
  NewList := TQuickStringlist.Create;
  for i := 0 to Src.AttributeCount - 1 do begin
    SrcAttr := Src.AttributeAtPos[i];
    // Reuse existing Attribute if possible.
    j := FAttributes.IndexOf(SrcAttr.StoredName);
    if j >= 0 then begin
      Attr := TColorSchemeAttribute(FAttributes.Objects[j]);
      DebugLn(['      Use existing attr ', Attr.StoredName]);
      FAttributes.Delete(j);
    end
    else begin
      Attr := TColorSchemeAttribute.Create(Self, SrcAttr.Caption, SrcAttr.StoredName);
    end;
    Attr.Assign(SrcAttr);
    NewList.AddObject(Attr.StoredName, Attr);
    if SrcAttr = Src.DefaultAttribute then
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
  csa, othercsa: TColorSchemeAttribute;
begin
  Result := //FDefaultAttribute.Equals(Other.FDefaultAttribute) and
            (FIdeHighlighterID = Other.FIdeHighlighterID) and
            (FAttributes.Count = Other.FAttributes.Count);
  i := FAttributes.Count - 1;
  while Result and (i >= 0) do begin
    csa := AttributeAtPos[i];
    othercsa := Other.Attribute[csa.StoredName];
    Result := Result and (othercsa <> nil) and csa.Equals(othercsa);
    dec(i);
  end;
end;

function TColorSchemeLanguage.IndexOfAttr(AnAttr: TColorSchemeAttribute): Integer;
begin
  Result := FAttributes.IndexOfObject(AnAttr);
end;

procedure TColorSchemeLanguage.LoadFromXml(aXMLConfig: TRttiXMLConfig;
  const aPath: String; Defaults: TColorSchemeLanguage; ColorVersion: Integer;
  const aOldPath: String);
var
  Def, EmptyDef, CurAttr: TColorSchemeAttribute;
  FormatVersion, RealFormatVersion: longint;
  TmpPath: String;
  i: Integer;
begin
//  Path := 'EditorOptions/Color/'
  RealFormatVersion := 0;
  if not FIsSchemeDefault then
    TmpPath := aPath + 'Lang' + StrToValidXMLName(FLanguageName) + '/'
  else
    TmpPath := aPath;
  if aXMLConfig.HasChildPaths(TmpPath) then begin
    FormatVersion := aXMLConfig.GetValue(TmpPath + 'Version', 0);
    RealFormatVersion := FormatVersion;
    if FormatVersion > ColorVersion then
      FormatVersion := ColorVersion;
    if FIsSchemeDefault and (FormatVersion < 6) then
      FormatVersion := 6;
  end
  else
    FormatVersion := 6;
  FFormatVersion := FormatVersion;

  TmpPath := TmpPath + 'Scheme' + StrToValidXMLName(Name) + '/';
  if not aXMLConfig.HasChildPaths(TmpPath) then begin
    // nothing to be loaded // FormatVersion is wrong
    FormatVersion := 6;
  end;

  if (aOldPath <> '') and (FormatVersion > 1) and aXMLConfig.HasChildPaths(TmpPath)
  then begin
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
  if Defaults = nil then begin
    // default all colors = clNone
    EmptyDef := TColorSchemeAttribute.Create(Self, nil, '');
    EmptyDef.UpdateSupportedFeatures([low(TLazTextAttributeFeatures)..high(TLazTextAttributeFeatures)], []);
  end
  else
    EmptyDef := nil;

  for i := 0 to AttributeCount - 1 do begin
    CurAttr := AttributeAtPos[i];
    Def := nil;
    if (ColorVersion < 14) and (RealFormatVersion < 14) and
       (Defaults = nil) and
       (CurAttr.GetSchemeGlobal = nil) and
       (CurAttr.StoredName = SYNS_XML_AttrModifier) and
       not (aXMLConfig.HasPath(TmpPath+SYNS_XML_AttrModifier, False))
    then
      Def := Attribute[SYNS_XML_AttrReservedWord];

    if Def = nil then begin
      if Defaults <> nil then
        Def := Defaults.Attribute[CurAttr.StoredName]
      else begin
        Def := CurAttr.GetSchemeGlobal;
        if Def = nil then begin
          Def := EmptyDef;
          Def.Features := CurAttr.FDefaultSynFeatures;
        end;
      end;
    end;
    CurAttr.LoadFromXml(aXMLConfig, TmpPath, Def, FormatVersion);
    if (ColorVersion < 9) and (RealFormatVersion < 9)
    and (CurAttr.StoredName = GetAddiHilightAttrName(ahaMouseLink)) then
    begin
      // upgrade ahaMouseLink
      CurAttr.FrameColor := CurAttr.Foreground;
      CurAttr.Background := clNone;
      CurAttr.Style := [];
      CurAttr.StyleMask := [];
      CurAttr.FrameStyle := slsSolid;
      CurAttr.FrameEdges := sfeBottom;
    end;

    if (ColorVersion < 12) and (CurAttr.Group = agnOutlineColors) then
      CurAttr.MarkupFoldLineColor := CurAttr.Foreground;
  end;
  FreeAndNil(EmptyDef);

  // Version 5 and before stored the global background on the Whitespace attribute.
  // If a whitespace Attribute was loaded (UseSchemeGlobals=false) then copy it
  if (FormatVersion <= 5) and (DefaultAttribute <> nil)
  and (FHighlighter <> nil) and (FHighlighter.WhitespaceAttribute <> nil) then
  begin
    CurAttr := Attribute[FHighlighter.WhitespaceAttribute.StoredName];
    if (CurAttr <> nil) and not CurAttr.UseSchemeGlobals then
      DefaultAttribute.Background := CurAttr.Background;
  end;
end;

procedure TColorSchemeLanguage.SaveToXml(aXMLConfig: TRttiXMLConfig; aPath: String;
  Defaults: TColorSchemeLanguage);
var
  Def, EmptyDef, CurAttr: TColorSchemeAttribute;
  i: Integer;
begin
  if (FLanguageName = '') and (not FIsSchemeDefault) then
    exit;
  if not FIsSchemeDefault then
    aPath := aPath + 'Lang' + StrToValidXMLName(FLanguageName) + '/';
  if (Defaults <> nil) and Self.Equals(Defaults) then begin
    aXMLConfig.DeletePath(aPath + 'Scheme' + StrToValidXMLName(Name));
    if not (FIsSchemeDefault or aXMLConfig.HasChildPaths(aPath)) then
      aXMLConfig.DeletePath(aPath);
    exit;
  end;
  aXMLConfig.SetValue(aPath + 'Version', EditorOptsFormatVersion);
  aPath := aPath + 'Scheme' + StrToValidXMLName(Name) + '/';

  if (Defaults = nil) then begin
    // default all colors = clNone
    EmptyDef := TColorSchemeAttribute.Create(Self, nil, '');
    EmptyDef.UpdateSupportedFeatures([low(TLazTextAttributeFeatures)..high(TLazTextAttributeFeatures)], []);
  end
  else
    EmptyDef := nil;

  for i := 0 to AttributeCount - 1 do begin
    CurAttr := AttributeAtPos[i];
    if Defaults <> nil then
      Def := Defaults.Attribute[CurAttr.StoredName]
    else begin
      Def := CurAttr.GetSchemeGlobal;
      if Def = nil then begin
        Def := EmptyDef;
        Def.Features := CurAttr.FDefaultSynFeatures;
      end;
    end;
    CurAttr.SaveToXml(aXMLConfig, aPath, Def);
  end;
  FreeAndNil(EmptyDef);
end;

procedure TColorSchemeLanguage.ApplyTo(ASynEdit: TSynEdit);
  procedure SetMarkupColor(aha: TAdditionalHilightAttribute; aMarkup : TLazEditTextAttribute);
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
  OGutter: TSynGutterLineOverview;
  OGutterProv: TSynGutterLineOverviewProvider;
  MarkupCaret: TSynEditMarkupHighlightAllCaret;
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

      if TIDESynEditor(aSynEdit).WrapView <> nil then begin
        SetMarkupColor(ahaWrapIndend,  TIDESynEditor(aSynEdit).WrapView.MarkupInfoWrapIndent);
        TIDESynEditor(aSynEdit).WrapView.MarkupInfoWrapIndent.FrameEdges := sfeLeft;
        SetMarkupColor(ahaWrapEol,     TIDESynEditor(aSynEdit).WrapView.MarkupInfoWrapEol);
        SetMarkupColor(ahaWrapSubLine, TIDESynEditor(aSynEdit).WrapView.MarkupInfoWrapSubLine);
      end;

      if TIDESynEditor(ASynEdit).ExternalHttpLink <> nil then begin
        SetMarkupColor(ahaExternalLink, TIDESynEditor(ASynEdit).ExternalHttpLink.MarkupInfo);
        //TIDESynEditor(ASynEdit).ExternalHttpLink
      end;
    end;
    SetMarkupColorByClass(ahaHighlightWord, TSynEditMarkupHighlightAllCaret);
    SetMarkupColorByClass(ahaWordGroup,     TSynEditMarkupWordGroup);
    SetMarkupColorByClass(ahaSpecialVisibleChars, TSynEditMarkupSpecialChar);

    MarkupCaret := TSynEditMarkupHighlightAllCaret(ASynEdit.MarkupByClass[TSynEditMarkupHighlightAllCaret]);
    if (MarkupCaret <> nil) and (MarkupCaret.OverViewGutterPart <> nil) then begin
      Attri := GetUsedAttr(ahaHighlightWord);
      if Attri <> nil then
        MarkupCaret.OverViewGutterPart.Color := Attri.MarkupAllOverviewColor
      else
        MarkupCaret.OverViewGutterPart.Color := clNone;

      MarkupCaret.ScanOffScreenLimit := MaxInt;
      if MarkupCaret.OverViewGutterPart.Color = clNone then
        MarkupCaret.ScanMode := smsmASync
      else
        MarkupCaret.ScanMode := smsmASyncForceAll;
    end;

    if ASynEdit is TIDESynEditor then begin
      with TIDESynEditor(ASynEdit) do begin
        Attri := AttributeByEnum[ahaIfDefBlockInactive];
        if Attri <> nil
          then Attri.ApplyTo(MarkupIfDef.MarkupInfoDisabled )
          else MarkupIfDef.MarkupInfoDisabled.Clear;
        Attri := AttributeByEnum[ahaIfDefBlockActive];
        if Attri <> nil
          then Attri.ApplyTo(MarkupIfDef.MarkupInfoEnabled )
          else MarkupIfDef.MarkupInfoEnabled.Clear;
        Attri := AttributeByEnum[ahaIfDefBlockTmpActive];
        if Attri <> nil
          then Attri.ApplyTo(MarkupIfDef.MarkupInfoTempEnabled )
          else MarkupIfDef.MarkupInfoTempEnabled.Clear;
        Attri := AttributeByEnum[ahaIfDefNodeInactive];
        if Attri <> nil
          then Attri.ApplyTo(MarkupIfDef.MarkupInfoNodeDisabled )
          else MarkupIfDef.MarkupInfoNodeDisabled.Clear;
        Attri := AttributeByEnum[ahaIfDefNodeActive];
        if Attri <> nil
          then Attri.ApplyTo(MarkupIfDef.MarkupInfoNodeEnabled )
          else MarkupIfDef.MarkupInfoNodeEnabled.Clear;
        Attri := AttributeByEnum[ahaIfDefNodeTmpActive];
        if Attri <> nil
          then Attri.ApplyTo(MarkupIfDef.MarkupInfoTempNodeEnabled )
          else MarkupIfDef.MarkupInfoTempNodeEnabled.Clear;
      end;
    end;
    SetGutterColorByClass(ahaLineNumber,      TSynGutterLineNumber);
    SetGutterColorByClass(ahaModifiedLine,    TSynGutterChanges);
    SetGutterColorByClass(ahaCodeFoldingTree, TSynGutterCodeFolding);
    SetGutterColorByClass(ahaGutterSeparator, TSynGutterSeparator);
    if assigned(ASynEdit.Gutter.Parts.ByClass[TSynGutterMarks, 0]) then
      ASynEdit.Gutter.Parts.ByClass[TSynGutterMarks, 0].MarkupInfo.Clear; // always use gutter color for marks
    Attri := AttributeByEnum[ahaCodeFoldingTreeCurrent];
    if Attri <> nil then begin
      if ASynEdit.Gutter.Parts.ByClass[TSynGutterCodeFolding,0] <> nil then
        Attri.ApplyTo(TSynGutterCodeFolding(ASynEdit.Gutter.Parts.ByClass[TSynGutterCodeFolding,0]).MarkupInfoCurrentFold);
    end;

    OGutter := TSynGutterLineOverview(ASynEdit.RightGutter.Parts.ByClass[TSynGutterLineOverview, 0]);
    if OGutter <> nil then begin
      for i := 0 to OGutter.Providers.Count - 1 do begin
        OGutterProv := OGutter.Providers[i];
        if OGutterProv is TSynGutterLOvProviderModifiedLines then begin
          Attri := GetUsedAttr(ahaModifiedLine);
          if Attri <> nil then begin
            OGutterProv.Color := Attri.FrameColor;
            TSynGutterLOvProviderModifiedLines(OGutterProv).ColorSaved := Attri.Foreground;
          end;
        end
        else
        if OGutterProv is TSynGutterLOvProviderCurrentPage then begin
          Attri := GetUsedAttr(ahaOverviewGutter);
          if Attri <> nil then begin
            OGutterProv.Color := Attri.FrameColor;
          end;
        end
        else
        if OGutterProv is TIDESynGutterLOvProviderPascal then begin
          Attri := GetUsedAttr(ahaOverviewGutter);
          if Attri <> nil then begin
            OGutterProv.Color := Attri.Foreground;
            TIDESynGutterLOvProviderPascal(OGutterProv).Color2 := Attri.Background;
          end;
        end;
      end;
    end;

    if ASynEdit is TIDESynEditor then
    begin
      IDESynEdit := TIDESynEditor(ASynEdit);

      for aha := low(TIdentWindowAhaColorRange) to high(TIdentWindowAhaColorRange) do begin
        Attri := GetUsedAttr(aha);
        Attri.ApplyTo(IDESynEdit.MarkupIdentComplWindow.Color[aha]);
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
  hlattrs: TLazEditTextAttribute;
begin
  AHLighter.BeginUpdate;
  try
    for i := 0 to AHLighter.AttrCount - 1 do begin
      hlattrs := AHLighter.Attribute[i];
      Attr := Attribute[hlattrs.StoredName];
      if Attr <> nil then
        Attr.ApplyTo(hlattrs, DefaultAttribute);
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

function TColorScheme.GetColorSchemeBySynHl(Index: TSynCustomHighlighter
  ): TColorSchemeLanguage;
begin
  if Index = nil then
    Result := FColorSchemes['']
  else
  if FColorSchemes.IndexOf(Index.LanguageName) < 0 then
    Result := nil
  else
    Result := FColorSchemes[Index.LanguageName];
end;

function TColorScheme.GetName: String;
begin
  Result := FName;
end;

function TColorScheme.GetLanguage(AnIndex: Integer): IColorSchemeLanguage;
begin
  Result := FColorSchemes.Data[AnIndex];
end;

function TColorScheme.GetLanguageForHighlighter(AnHighlighterId: TIdeSyntaxHighlighterID
  ): IColorSchemeLanguage;
begin
  Result := ColorSchemeBySynHl[HighlighterList.SharedSynInstances[AnHighlighterId]];
end;

function TColorScheme.GetLanguageForHighlighter(AnHiglighter: TObject): IColorSchemeLanguage;
begin
  Result := ColorSchemeBySynHl[AnHiglighter as TSynCustomHighlighter];
end;

function TColorScheme.GetColorScheme(Index: integer): TColorSchemeLanguage;
begin
  Result := FColorSchemes.Data[Index];
end;

function TColorScheme.GetStoredValuesForScheme: TColorScheme;
begin
  Result:=ColorSchemeFactory.ColorSchemeGroup[Name];
end;

constructor TColorScheme.Create(const AName: String);
begin
  inherited Create;
  FName := AName;
  FColorSchemes := TColorSchemesMap.Create(True);
end;

constructor TColorScheme.CreateFromXml(aXMLConfig: TRttiXMLConfig; const AName, aPath: String);
var
  i: integer;
  n: String;
  PascalScheme: TColorSchemeLanguage;
begin
  Create(AName);
  FDefaultColors := TColorSchemeLanguage.CreateFromXml(Self, IdeHighlighterNoneID, aXMLConfig,
                                                       aPath + 'Globals/', True);
  PascalScheme := nil;
  for i := IdeHighlighterStartId to HighlighterList.Count - 1 do begin
    n := HighlighterList.SharedSynInstances[i].LanguageName;
    if FColorSchemes.IndexOf(n) < 0 then
      FColorSchemes[n] := TColorSchemeLanguage.CreateFromXml(Self, i, aXMLConfig,
        aPath, False, PascalScheme, HighlighterList[i].MappedAttributes);
    if i = IdeHighlighterStartId then
      PascalScheme := FColorSchemes[n];
   end;
end;

destructor TColorScheme.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FDefaultColors);
  FColorSchemes.Free;
end;

procedure TColorScheme.Assign(Src: TColorScheme);
var
  i: integer;
  l: TColorSchemeLanguage;
begin
  if Src.FDefaultColors = nil then
    FreeAndNil(FDefaultColors)
  else if FDefaultColors = nil then
    FDefaultColors := TColorSchemeLanguage.Create(Self, IdeHighlighterNoneID, True);
  if FDefaultColors <> nil then
    FDefaultColors.Assign(Src.FDefaultColors);
  FColorSchemes.Clear;
  for i := 0 to Src.FColorSchemes.Count-1 do begin
    l := TColorSchemeLanguage.Create(Self, -1, False);
    l.Assign(Src.FColorSchemes.Data[i]);
    FColorSchemes[Src.FColorSchemes.Keys[i]] := l;
  end;
end;

function TColorScheme.Count: integer;
begin
  Result := FColorSchemes.Count;
end;

procedure TColorScheme.LoadFromXml(aXMLConfig: TRttiXMLConfig;
  const aPath: String; Defaults: TColorScheme; const aOldPath: String);
var
  i: Integer;
  Def: TColorSchemeLanguage;
  FormatVersion: longint;
  n: String;
begin
  FormatVersion := aXMLConfig.GetValue(aPath + 'Version', 0);
  if Defaults <> nil then
    Def := Defaults.DefaultColors
  else
    Def := nil;
  FDefaultColors.LoadFromXml(aXMLConfig, aPath + 'Globals/', Def, FormatVersion);
  for i := 0 to FColorSchemes.Count - 1 do begin
    n := FColorSchemes.Keys[i];
    if Defaults <> nil then
      Def := Defaults.FColorSchemes[n]
    else
      Def := nil;
    FColorSchemes.Data[i].LoadFromXml(aXMLConfig, aPath, Def, FormatVersion, aOldPath);
  end;
end;

procedure TColorScheme.SaveToXml(aXMLConfig: TRttiXMLConfig;
  const aPath: String; Defaults: TColorScheme);
var
  i: Integer;
  Def: TColorSchemeLanguage;
  n: String;
begin
  if Defaults <> nil then
    Def := Defaults.DefaultColors
  else
    Def := nil;
  FDefaultColors.SaveToXml(aXMLConfig, aPath + 'Globals/', Def);
  if not aXMLConfig.HasChildPaths(aPath + 'Globals') then
    aXMLConfig.DeletePath(aPath + 'Globals');
  for i := 0 to FColorSchemes.Count - 1 do begin
    n := FColorSchemes.Keys[i];
    if (Defaults <> nil) and (Defaults.FColorSchemes.IndexOf(n) >= 0) then
      Def := Defaults.FColorSchemes[n]
    else
      Def := nil;
    FColorSchemes.Data[i].SaveToXml(aXMLConfig, aPath, Def);
  end;
  aXMLConfig.SetValue(aPath + 'Version', EditorOptsFormatVersion);
end;

{ TColorSchemeFactory }

function TColorSchemeFactory.GetColorSchemeGroup(const Index: String
  ): TColorScheme;
var
  Idx: integer;
begin
  Idx := FMappings.IndexOf(Index);
  if Idx = -1 then
    Result := nil
  else
    Result := TColorScheme(FMappings.Objects[Idx]);
end;

function TColorSchemeFactory.GetColorSchemeGroupAtPos(Index: Integer): TColorScheme;
begin
  Result := TColorScheme(FMappings.Objects[Index]);
end;

function TColorSchemeFactory.GetScheme(AnIndex: Integer): IColorScheme;
begin
  Result := nil;
  if EditorOpts <> nil then
    Result := EditorOpts.UserColorSchemeGroup.ColorSchemeGroupAtPos[AnIndex];
end;

function TColorSchemeFactory.GetScheme(AName: String): IColorScheme;
begin
  Result := nil;
  if EditorOpts <> nil then
    Result := EditorOpts.UserColorSchemeGroup.ColorSchemeGroup[AName];
end;

function TColorSchemeFactory.GetCurrentSchemeForHighlighter(AnHiglighter: TObject): IColorScheme;
begin
  Result := nil;
  if EditorOpts <> nil then
    Result := EditorOpts.UserColorSchemeGroup.GetColorSchemeGroup(EditorOpts.ReadColorScheme((AnHiglighter as TSynCustomHighlighter).LanguageName));
end;

function TColorSchemeFactory.GetCurrentSchemeForHighlighter(AnHighlighterId: TIdeSyntaxHighlighterID
  ): IColorScheme;
begin
  Result := nil;
  if EditorOpts <> nil then
    Result := EditorOpts.UserColorSchemeGroup.GetColorSchemeGroup(EditorOpts.ReadColorScheme(HighlighterList.Names[AnHighlighterId]));
end;

procedure TColorSchemeFactory.RegisterChangedHandler(AnHandler: TNotifyEvent);
begin
  if EdOptsChangedHandlers = nil then
    EdOptsChangedHandlers := TMethodList.Create;
  EdOptsChangedHandlers.Add(TMethod(AnHandler));
end;

procedure TColorSchemeFactory.UnregisterChangedHandler(AnHandler: TNotifyEvent);
begin
  if EdOptsChangedHandlers <> nil then
    EdOptsChangedHandlers.Remove(TMethod(AnHandler));
end;

function TColorSchemeFactory.RegisterAttributeGroup(AName: PString): integer;
begin
  Result := Length(RegisteredAttribGroupNames);
  SetLength(RegisteredAttribGroupNames, Result + 1);
  RegisteredAttribGroupNames[Result] := AName;
end;

procedure TColorSchemeFactory.InternalAddAttribute(AnAttrGroup: integer;
  AnHighlighterId: TIdeSyntaxHighlighterID; AStoredName: String; AName: PString;
  AFeatures: TColorSchemeAttributeFeatures; ADefaults: TObject);
var
  h: TSynCustomHighlighter;
  i: Integer;
  cs: TColorScheme;
  csl: TColorSchemeLanguage;
  csa: TColorSchemeAttribute;
begin
  h := HighlighterList.SharedSynInstances[AnHighlighterId];
  if h = nil then
    exit;

  for i := 0 to FMappings.Count - 1 do begin
    cs := ColorSchemeGroupAtPos[i];
    csl := cs.ColorSchemeBySynHl[h];
    if csl = nil then
      continue;

    csa := TColorSchemeAttribute.Create(csl, AName, AStoredName);
    csa.Clear;
    if ADefaults <> nil then begin
      csa.AssignSupportedFeaturesFrom(ADefaults as TLazEditTextAttribute);
      csa.AssignColors(ADefaults as TLazEditTextAttribute);
    end;
    csa.InternalSaveDefaultValues;
    csa.FDefaultSynFeatures := csa.Features;

    csa.FGroup := agnRegistered;
    csa.FRegisteredGroup := AnAttrGroup;
    csa.FAttrFeatures := AFeatures;

    csl.FAttributes.AddObject(AStoredName, csa);
  end;
end;

procedure TColorSchemeFactory.AddAttribute(AnAttrGroup: integer;
  AnHighlighterId: TIdeSyntaxHighlighterID; AStoredName: String; AName: PString;
  AFeatures: TColorSchemeAttributeFeatures; ADefaults: TObject);
begin
  InternalAddAttribute(AnAttrGroup, AnHighlighterId, AStoredName, AName, AFeatures, ADefaults);
  if EditorOpts <> nil then
    EditorOpts.UserColorSchemeGroup.InternalAddAttribute(AnAttrGroup, AnHighlighterId, AStoredName, AName, AFeatures, ADefaults);
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

function TColorSchemeFactory.Count: integer;
begin
  Result := FMappings.Count;
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
    FMappings.AddObject(lMapping.Name, lMapping);
  end;
  FMappings.Sorted := true;
end;

procedure TColorSchemeFactory.LoadFromXml(aXMLConfig: TRttiXMLConfig;
  const aPath: String; Defaults: TColorSchemeFactory; const aOldPath: String);
var
  i: Integer;
  Def: TColorScheme;
begin
  for i := 0 to FMappings.Count - 1 do begin
    if Defaults <> nil then
      Def := Defaults.ColorSchemeGroupAtPos[i]
    else
      Def := nil;
    ColorSchemeGroupAtPos[i].LoadFromXml(aXMLConfig, aPath, Def, aOldPath);
  end;
  // all Schemes have read (and relocated) the old values
  if aOldPath <> '' then begin
    aXMLConfig.DeletePath(aOldPath + 'RightMarginColor');
    aXMLConfig.DeletePath(aOldPath + 'GutterColor');
  end;
end;

procedure TColorSchemeFactory.SaveToXml(aXMLConfig: TRttiXMLConfig;
  const aPath: String; Defaults: TColorSchemeFactory);
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

procedure TColorSchemeFactory.RegisterScheme(aXMLConfig: TRttiXMLConfig;
  AName: String; const aPath: String);
var
  lMapping: TColorScheme;
  i, j: integer;
begin
  i := FMappings.IndexOf(AName);
  if i <> -1 then begin
    j := 0;
    repeat
      inc(j);
      i := FMappings.IndexOf(AName+'_'+IntToStr(j));
    until i = -1;
    AName := AName+'_'+IntToStr(j);
    DebugLn(['TColorSchemeFactory.RegisterScheme: Adjusting AName to ', AName]);
  end;
  lMapping := TColorScheme.CreateFromXml(aXMLConfig, AName, aPath);
  FMappings.AddObject(AName, lMapping);
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

{ TIDESynTextSyn }

procedure TIDESynTextSyn.InitForScanningLine;
begin
  inherited InitForScanningLine;
  FPos := 0;
end;

function TIDESynTextSyn.GetDefaultAttribute(Index: integer
  ): TSynHighlighterAttributes;
begin
  Result := nil;
end;

class function TIDESynTextSyn.GetLanguageName: string;
begin
  Result := 'Plain Text';
end;

procedure TIDESynTextSyn.ResetRange;
begin

end;

constructor TIDESynTextSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDefaultFilter := '';
end;

function TIDESynTextSyn.GetEol: Boolean;
begin
  Result := FPos > 0;
end;

function TIDESynTextSyn.GetToken: string;
begin
  Result := CurrentLineText;
end;

procedure TIDESynTextSyn.GetTokenEx(out TokenStart: PChar; out
  TokenLength: integer);
begin
  TokenStart := PChar(CurrentLineText);
  TokenLength := Length(CurrentLineText);
end;

function TIDESynTextSyn.GetTokenAttribute: TLazEditTextAttribute;
begin
  Result := nil;
end;

function TIDESynTextSyn.GetTokenPos: Integer;
begin
  Result := 0;
end;

function TIDESynTextSyn.GetTokenKind: integer;
begin
  Result := 0;
end;

procedure TIDESynTextSyn.Next;
begin
  inc(FPos);
end;

procedure TIDESynTextSyn.SetRange(Value: Pointer);
begin

end;

{ TQuickStringlist }

function TQuickStringlist.DoCompareText(const s1, s2: string): PtrInt;
begin
  Result := CompareText(s1, s2);
end;


initialization
  RegisterIDEOptionsGroup(GroupEditor, TEditorOptions);
  IdeSyntaxHighlighters := HighlighterList;

finalization
  IdeColorSchemeList := nil;
  TheColorSchemeFactorSingleton.Free;
  TheIdeHiglighterListSingleton.Free;
  RegisteredAttribGroupNames := nil;
  EdOptsChangedHandlers.Free;

end.
