{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019-Now by Michael Van Canneyt, member of the
    Free Pascal development team

    WEB Widget Set : Basic bare HTML Widgets

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit stub.htmlwidgets;

{$mode objfpc}
{$h+}
interface

uses
  Classes, SysUtils, stub.webwidget, stub.js, stub.web;

Type
  TTextMode = (tmText,tmHTML);

  { TButtonWidget }

  TButtonWidget = Class(TWebWidget)
  private
    FText: String;
    FTextMode: TTextMode;
    procedure SetText(AValue: String);
    procedure SetTextMode(AValue: TTextMode);
  Protected
  Published
    Property Text : String Read FText Write SetText;
    Property TextMode : TTextMode Read FTextMode Write SetTextMode;
  end;


  TWebPage = Class(TCustomWebWidget)
  private
  Public
    Constructor Create(AOwner : TComponent); override;
    // Later on, allow IFrame;
  Published
    Property ParentID;
    Property ElementID;
    Property Classes;
    Property Styles;
    Property StyleRefresh;
    // Events
    Property BeforeRenderHTML;
    Property AfterRenderHTML;
    Property OnAbort;
    Property OnAnimationCancel;
    Property OnAnimationEnd;
    Property OnAnimationIteration;
    Property OnAnimationStart;
    Property OnAuxClick;
    Property OnBlur;
    Property OnCancel;
    Property OnCanPlay;
    Property OnCanPlayThrough;
    Property OnChange;
    Property OnClick;
    Property OnCompositionEnd;
    Property OnCompositionStart;
    Property OnCompositionUpdate;
    Property OnContextMenu;
    Property OnCopy;
    Property OnCut;
    Property OnCueChange;
    Property OnDblClick;
    Property OnDurationChange;
    Property OnEnded ;
    Property OnError ;
    Property OnFocus;
    Property OnFocusIn ;
    Property OnFocusOut ;
    Property OnGotPointerCapture;
    Property OnInput;
    Property OnInvalid;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnLoad;
    Property OnLoadedData;
    Property OnLoadedMetaData;
    Property OnLoadend;
    Property OnLoadStart;
    Property OnLostPointerCapture;
    Property OnMouseDown;
    Property OnMouseEnter;
    Property OnMouseLeave;
    Property OnMouseMove;
    Property OnMouseOut;
    Property OnMouseUp;
    Property OnOverFlow;
    Property OnPaste;
    Property OnPause;
    Property OnPlay;
    Property OnPointerCancel;
    Property OnPointerDown;
    Property OnPointerEnter;
    Property OnPointerLeave;
    Property OnPointerMove;
    Property OnPointerOut;
    Property OnPointerOver;
    Property OnPointerUp;
    Property OnReset;
    Property OnResize;
    Property OnScroll;
    Property OnSelect;
    Property OnSubmit;
    Property OnTouchStart;
    Property OnTransitionCancel;
    Property OnTransitionEnd;
    Property OnTransitionRun;
    Property OnTransitionStart;
    Property OnWheel;
  end;

  { TCustomInputWidget }

  TCustomInputWidget = Class(TWebWidget)
  private
    FValue : String;
    FValueName : String;
    FText : String;
    FReadOnly : Boolean;
    FRequired : Boolean;
    function GetReadOnly: Boolean;
    function GetRequired: Boolean;
    function GetText: String;
    function GetValue: String;
    function GetValueName: String;
    procedure SetReadonly(AValue: Boolean);
    procedure SetRequired(AValue: Boolean);
    procedure SetText(AValue: String);
    procedure SetValue(AValue: String);
    procedure SetValueName(AValue: String);
  Protected
    procedure SetName(const NewName: TComponentName); override;
    // Text to show (checkbox etc). Enable in descendents as needed
    Property Text : String Read GetText Write SetText;
  Public
    // Value as string
    Property Value : String Read GetValue Write SetValue;
    // Value Name to use when submitting using form.
    Property ValueName : String Read GetValueName Write SetValueName;
    Property ReadOnly : Boolean Read GetReadOnly Write SetReadonly;
    Property Required : Boolean Read GetRequired Write SetRequired;
  end;

  { TTextInputWidget }

  TInputTextType = (ittText,ittPassword,ittNumber,ittEmail,ittSearch,ittTelephone,ittURL,ittColor);
  TTextInputWidget = class(TCustomInputWidget)
  private
    FMaxLength : Integer;
    FMinLength : Integer;
    FTextType : TInputTextType;
    function GetAsNumber: NativeInt;
    function GetMaxLength: NativeInt;
    function GetMinLength: NativeInt;
    function GetTextType: TInputTextType;
    procedure SetAsNumber(AValue: NativeInt);
    procedure SetMaxLength(AValue: NativeInt);
    procedure SetMinLength(AValue: NativeInt);
    procedure SetTextType(AValue: TInputTextType);
  Public
  Published
    Property Value;
    Property ValueName;
    Property Required;
    Property TextType : TInputTextType Read GetTextType Write SetTextType;
    property AsNumber : NativeInt Read GetAsNumber Write SetAsNumber;
    Property MaxLength : NativeInt Read GetMaxLength Write SetMaxLength;
    Property MinLength : NativeInt Read GetMinLength Write SetMinLength;
    // Todo: List support
  end;


  { TButtonInputWidget }
  TInputButtonType = (ibtSubmit,ibtReset,ibtImage);
  TInputButtonTypes = set of TInputButtonType;

  TButtonInputWidget = class(TCustomInputWidget)
  private
    FButtonType: TInputButtonType;
    FSrc: String;
    procedure SetButtonType(AValue: TInputButtonType);
    procedure SetSrc(AValue: String);
  Public
  Published
    Property ButtonType : TInputButtonType Read FButtonType Write SetButtonType;
    Property Value;
    Property ValueName;
    Property Src : String Read FSrc Write SetSrc;
  end;

  { TCheckableInputWidget }

  TCheckableInputWidget = class(TCustomInputWidget)
  private
    FChecked: Boolean;
    function GetChecked: Boolean;
    procedure SetChecked(AValue: Boolean);
  Public
    Property Value;
    Property ValueName;
    Property Checked : Boolean Read GetChecked Write SetChecked;
    Property Text;
  end;

  { TRadioInputWidget }

  TRadioInputWidget = class(TCheckableInputWidget)
  Published
    Property Value;
    Property ValueName;
    Property Checked;
    Property Text;
  end;

  { TCheckboxInputWidget }

  TCheckboxInputWidget = class(TCheckableInputWidget)
  Published
    Property Value;
    Property ValueName;
    Property Checked;
    Property Text;
  end;


  { TDateInputWidget }

  TDateInputWidget = class(TCustomInputWidget)
  private
    FDate: TDateTime;
    function GetDate: TDateTime;
    procedure SetDate(AValue: TDateTime);
  Published
    Property Required;
    Property ValueName;
    Property Date : TDateTime Read GetDate Write SetDate;
  end;


  TFileInputWidget = class(TCustomInputWidget)
  private
    FMultiple: Boolean;
    function GetMultiple: Boolean;
    procedure SetMultiple(AValue: Boolean);
  Published
    Property ValueName;
    Property Required;
    Property Multiple : Boolean Read GetMultiple Write SetMultiple;
  end;

  { THiddenInputWidget }

  THiddenInputWidget = class(TCustomInputWidget)
  Published
    Property ValueName;
    Property Value;
    Property Required;
  end;

  { TTextAreaWidget }

  TTextAreaWrap = (tawSoft,tawHard,tawOff);
  TTextAreaWidget = Class(TWebWidget)
  private
    FLines: TStrings;
    FMaxLength: Cardinal;
    FValueName : String;
    FRows,
    FColumns : Cardinal;
    FWrap: TTextAreaWrap;
    FRequired,
    FReadOnly : Boolean;
    function GetColumns: Cardinal;
    function GetLines: TStrings;
    function GetReadOnly: Boolean;
    function GetRequired: Boolean;
    function GetRows: Cardinal;
    function GetValueName: string;
    procedure SetColumns(AValue: Cardinal);
    procedure SetLines(AValue: TStrings);
    procedure SetMaxLength(AValue: Cardinal);
    procedure SetReadonly(AValue: Boolean);
    procedure SetRequired(AValue: Boolean);
    procedure SetRows(AValue: Cardinal);
    procedure SetValueName(AValue: string);
    procedure SetWrap(AValue: TTextAreaWrap);
  Protected
    procedure SetName(const NewName: TComponentName); override;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
  Published
    Property ValueName : string Read GetValueName Write SetValueName;
    Property Rows : Cardinal Read GetRows Write SetRows;
    Property Columns : Cardinal Read GetColumns Write SetColumns;
    Property Lines : TStrings Read GetLines Write SetLines;
    Property MaxLength : Cardinal Read FMaxLength Write SetMaxLength;
    Property Wrap : TTextAreaWrap Read FWrap Write SetWrap;
    Property ReadOnly : Boolean Read GetReadOnly Write SetReadonly;
    Property Required : Boolean Read GetRequired Write SetRequired;
  end;

  { TImageWidget }

  TImageWidget = class(TWebWidget)
  private
    FHeight: Integer;
    FWidth: Integer;
    FSrc : String;
    function GetHeight: Integer;
    function GetSrc: String;
    function GetWidth: Integer;
    procedure SetHeight(AValue: Integer);
    procedure SetSrc(AValue: String);
    procedure SetWidth(AValue: Integer);
  Protected
  Published
    Property Src : String Read GetSrc Write SetSrc;
    Property Width : Integer Read GetWidth Write SetWidth;
    Property Height : Integer Read GetHeight Write SetHeight;
  end;

  { TSelectWidget }

  TJSHTMLOptionElementArray = Array of TJSHTMLOptionElement;
  TCustomSelectWidget = Class;


  { TCustomSelectWidget }

  TCustomSelectWidget = class(TWebWidget)
  Private
    FSelectedIndex : Integer;
    FMultiple : Boolean;
    function GetMultiple: Boolean;
    function GetSelectedIndex: Integer;
    procedure SetMultiple(AValue: Boolean);
    procedure SetSelectedIndex(AValue: Integer);
  Protected
    // Can be made public/published
    // Items that are selected
    property SelectedIndex : Integer Read GetSelectedIndex Write SetSelectedindex;
    Property Multiple : Boolean Read GetMultiple Write SetMultiple;
  Public
    Constructor Create(aOWner : TComponent); override;
  end;

  TSelectWidget = class(TCustomSelectWidget)
  private
    FItems : TStrings;
    FValues : TStrings;
    function GetItems: TStrings;
    function GetValues: TStrings;
    procedure setItems(AValue: TStrings);
    procedure setValues(AValue: TStrings);
  Public
    Constructor Create(aOWner : TComponent); override;
    Destructor Destroy; override;
  Published
    Property Items : TStrings Read GetItems Write setItems;
    Property Values : TStrings Read GetValues Write setValues;
    property SelectedIndex;
    Property Multiple;
    property Classes;
  end;

  { TLabelWidget }

  TLabelWidget = Class(TWebWidget)
  private
    FLabelFor: TWebWidget;
    FText: String;
    function GetText: String;
    procedure SetLabelFor(AValue: TWebWidget);
    procedure SetText(AValue: String);
  Protected
    procedure SetName(const NewName: TComponentName); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  Published
    Property Text : String Read GetText Write SetText;
    Property LabelFor : TWebWidget Read FLabelFor Write SetLabelFor;
  end;

  TTextTag = (ttParagraph,ttBold,ttItalic,ttUnderline,ttStrikeThrough,ttSpan,ttQuote,ttBlockQuote,ttH1,ttH2,ttH3,ttH4,ttH5,ttH6,ttPre,ttRuby,ttArticle,ttAddress,ttAbbr,ttCustom);

  { TTextWidget }

  { TCustomTextWidget }

  TCustomTextWidget = Class(TCustomWebWidget)
  private
    FCustomTag: String;
    FEnvelopeTag: TTextTag;
    FTextMode: TTextMode;
    procedure SetCustomTag(AValue: String);
    procedure SetEnvelopeTag(AValue: TTextTag);
    procedure SetTextMode(AValue: TTextMode);
  Protected
  Published
    Property CustomTag : String Read FCustomTag Write SetCustomTag;
    Property EnvelopeTag : TTextTag Read FEnvelopeTag Write SetEnvelopeTag;
    Property TextMode : TTextMode Read FTextMode Write SetTextMode;
  end;

  TTextWidget = Class(TCustomTextWidget)
  private
    FText : String;
    procedure SetText(AValue: String);
  published
    Property Text : String Read FText Write SetText;
  end;

  { TTextLinesWidget }

  TTextLinesWidget = Class(TCustomTextWidget)
  private
    FLines : TStrings;
    FForceLineBreaks: Boolean;
    procedure SetLines(AValue: TStrings);
    procedure SetForceLineBreaks(AValue: Boolean);
  Public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    Property Lines : TStrings Read FLines Write SetLines;
    // When forcelinebreaks is true a <br> will be appended to every line.
    // Note that for TextMode=tmText this means the lines will be rendered as-is, but there will still be a <br> between the lines
    Property ForceLineBreaks : Boolean Read FForceLineBreaks Write SetForceLineBreaks;
  end;

  { TCustomTableColumn }
  TColumnOption = (coHeader,coCaptionHeader);
  TColumnOptions = set of TColumnOption;

  TCustomTableColumn = Class(TCollectionItem)
  private
    FAlignment: TAlignment;
    FCaption: String;
    FClassNames: String;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetCaption(AValue: String);
    procedure SetClassNames(AValue: String);
  Protected
    Function RenderColumn : Boolean; virtual;
    Function GetDisplayName: string; override;
    function GetCaption: String; virtual;
  Public
    Procedure Assign(Source : TPersistent); override;
    Property Alignment : TAlignment Read FAlignment Write SetAlignment;
    Property Caption : String Read GetCaption Write SetCaption;
    Property ClassNames : String Read FClassNames Write SetClassNames;
  end;

  { TCustomTableColumns }

  TCustomTableColumns = Class(TCollection)
  private
    function GetCustomColumn(Index : Integer): TCustomTableColumn;
    procedure SetCustomColumn(Index : Integer; AValue: TCustomTableColumn);
  Protected
    Property CustomColumns [Index : Integer] : TCustomTableColumn Read GetCustomColumn Write SetCustomColumn; default;
  Public
    Function Add(aCaption : String): TCustomTableColumn; overload;
  end;

  { TCustomTableWidget }
  TTableOption = (toHeader,    // use THead tag
                  toHeaderRow, // Create header row
                  toBody,      // use TBody tag
                  toFooter,    // use TFoot tag
                  toFooterRow,  // create footer row
                  toRowID,      // add ID to tr: -kind-row
                  toCellID,     // add ID to cell td: -kind-row-col
                  toHeaderRowData,     // Add rowno to <tr data-row> for header.
                  toHeaderCellDataRow, // Add rowno to <th data-row> for header. Automatic if onheadercellclick is set.
                  toHeaderCellDataCol, // Add colno to <th data-col> for header. Automatic if onheadercellclick is set.
                  toBodyRowData,       // Add rowno to <tr data-row> for body.
                  toBodyCellDataRow,   // Add rowno to <th data-row> for body. Automatic if oncellclick is set.
                  toBodyCellDataCol,   // Add colno to <th data-col> for body. Automatic if oncellclick is set.
                  tofooterRowData,     // Add rowno to <tr data-row> for footer
                  tofooterCellDataRow, // Add rowno to <th data-row> for footer. Automatic if onfootercellclick is set.
                  tofooterCellDataCol  // Add colno to <th data-col> for footer. Automatic if onfootercellclick is set.
                  );
  TTableOptions = Set of TTableOption;

  TRowKind = (rkHeader,rkBody,rkFooter);

Type
  TCustomTableWidget = Class;

  // Constructed only once when rendering !
  { TTableWidgetCelldata }
  TTableWidgetCellData = Class
  private
    FAsHTML: Boolean;
    FClassNames: String;
    FCol: Integer;
    FColumn: TCustomTableColumn;
    FContent: TJSHTMLElement;
    FKind: TRowKind;
    FRow: Integer;
    FTable: TCustomTableWidget;
    FTableID: String;
    FTag: String;
    FText: String;
    FWidget: TWebWidget;
  Protected
    Procedure SetRowColKind(aRow,aCol : Integer; aKind : TRowKind); virtual;
    Procedure Reset; // do not reset row,col, column
  Public
    Constructor Create(aTable : TCustomTableWidget;aTableID : String); virtual;
    Property Table : TCustomTableWidget Read FTable;
    Property Column : TCustomTableColumn Read FColumn Write FColumn;
    Property Row : Integer Read FRow;
    Property Col : Integer Read FCol;
    Property Kind : TRowKind Read FKind;
    Property Tag : String Read FTag Write FTag;
    Property ClassNames : String Read FClassNames Write FClassNames;
    Property Text : String Read FText Write FText;
    Property AsHTML : Boolean Read FAsHTML Write FAsHTML;
    Property Content : TJSHTMLElement Read FContent Write FContent;
    Property Widget : TWebWidget Read FWidget Write FWidget;
    Property TableID : String Read FTableID;
  end;


  TTableRowEnumerator = Class
  end;

  TOnCellDataEvent = Procedure (Sender : TObject; Enum : TTableRowEnumerator; aCell : TTableWidgetCellData) of object;

  TCustomTableWidget = Class(TCustomWebWidget)
  private
    FCaption: String;
    FColumns: TCustomTableColumns;
    FOnCellClick: THTMLNotifyEvent;
    FOnFooterCellClick: THTMLNotifyEvent;
    FOnFooterRowClick: THTMLNotifyEvent;
    FOnHeaderCellClick: THTMLNotifyEvent;
    FOnHeaderRowClick: THTMLNotifyEvent;
    FOnRowClick: THTMLNotifyEvent;
    FTableOptions: TTableOptions;
    FOnGetCellData : TOnCellDataEvent;
    procedure SetCaption(AValue: String);
    procedure SetColumns(AValue: TCustomTableColumns);
    procedure SetTableOptions(AValue: TTableOptions);
  Protected
    function CreateColumns: TCustomTableColumns; virtual;
    function DefaultTableOptions: TTableOptions; virtual;
  Protected
    // These can be made public/published
    Property CustomColumns : TCustomTableColumns Read FColumns Write SetColumns;
    Property TableOptions : TTableOptions read FTableOptions write SetTableOptions;
    Property Caption : String Read FCaption Write SetCaption;
    Property OnGetCellData : TOnCellDataEvent Read FOnGetCellData Write FOnGetCellData;
    Property OnCellClick :  THTMLNotifyEvent Read FOnCellClick Write FOnCellClick;
    Property OnHeaderCellClick :  THTMLNotifyEvent Read FOnHeaderCellClick Write FOnHeaderCellClick;
    Property OnFooterCellClick :  THTMLNotifyEvent Read FOnFooterCellClick Write FOnFooterCellClick;
    Property OnRowClick :  THTMLNotifyEvent Read FOnRowClick Write FOnRowClick;
    Property OnHeaderRowClick :  THTMLNotifyEvent Read FOnHeaderRowClick Write FOnHeaderRowClick;
    Property OnFooterRowClick :  THTMLNotifyEvent Read FOnFooterRowClick Write FOnFooterRowClick;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
  end;

  { TEventTableWidget }

  TEventTableWidget = Class(TCustomTableWidget)
  private
    FRowCount: Integer;
    procedure SetRowCount(AValue: Integer);
  Published
    Property RowCount : Integer Read FRowCount Write SetRowCount;
    Property CustomColumns;
    Property TableOptions;
    Property Caption;
    Property OnGetCellData;
    Property OnCellClick;
    Property OnHeaderCellClick;
    Property OnFooterCellClick;
    Property OnRowClick;
    Property OnHeaderRowClick;
    Property OnFooterRowClick;
  end;

  { TCustomStringsTableWidget }

  TCustomStringsTableWidget = Class(TCustomTableWidget)
  private
    function GetRowCount: Integer;
    procedure SetRowCount(AValue: Integer);
  Public
    Property RowCount : Integer Read GetRowCount Write SetRowCount;
    Property CustomColumns;
    Property TableOptions;
    Property Caption;
    Property OnGetCellData;
    Property OnCellClick;
    Property OnHeaderCellClick;
    Property OnFooterCellClick;
    Property OnRowClick;
    Property OnHeaderRowClick;
    Property OnFooterRowClick;
  end;

  TStringsTableWidget = Class(TCustomStringsTableWidget)
  Published
    Property RowCount;
    Property CustomColumns;
    Property TableOptions;
    Property Caption;
    Property OnGetCellData;
    Property OnCellClick;
    Property OnHeaderCellClick;
    Property OnFooterCellClick;
    Property OnRowClick;
    Property OnHeaderRowClick;
    Property OnFooterRowClick;
  end;

  { TDivWidget }
  THTMLElementTag = (
      etUnknown, eta, etabbr, etacronym, etaddress, etapplet, etarea, etb, etbase,
      etbasefont, etbdo, etbig, etblockquote, etbody, etbr, etbutton,
      etcaption, etcenter, etcite, etcode, etcol, etcolgroup, etdd, etdel,
      etdfn, etdir, etdiv, etdl, etdt, etem, etfieldset, etfont, etform,
      etframe, etframeset, eth1, eth2, eth3, eth4, eth5, eth6, ethead, ethr,
      ethtml, eti, etiframe, etimg, etinput, etins, etisindex, etkbd, etlabel,
      etlegend, etli, etlink, etmap, etmenu, etmeta, etnoframes, etnoscript,
      etobject, etol, etoptgroup, etoption, etp, etparam, etpre, etq, ets,
      etsamp, etscript, etselect, etsmall, etspan, etstrike, etstrong,
      etstyle, etsub, etsup, ettable, ettbody, ettd, ettextarea, ettfoot,
      etth, etthead, ettitle, ettr, ettt, etu, etul, etvar,
      etText,etAudio,etVideo,etSource
  );
  THTMLElementTagSet = set of THTMLElementTag;

  { TCustomTagWidget }

  TCustomTagWidget = Class(TWebWidget)
  private
    FElementTag: THTMLElementTag;
    FTextContent: String;
    procedure SetElementTag(AValue: THTMLElementTag);
    procedure SetTextContent(AValue: String);
  Protected
    // Set tag you wish to use
    Property elementTag : THTMLElementTag Read FElementTag Write SetElementTag;
    // If set, the text will be set as InnerText of the tag
    Property TextContent : String Read FTextContent Write SetTextContent;
  end;

  { TTagWidget }

  TTagWidget = Class(TCustomTagWidget)
  Public
    Constructor Create(aOwner : TComponent); override;
  Published
    Property elementTag;
    Property TextContent;
  end;

  TDivWidget = Class(TCustomTagWidget)
  Public
    Constructor Create(aOwner : TComponent); override;
  end;

  { TParagraphWidget }

  TParagraphWidget = Class(TCustomTagWidget)
  Public
    Constructor Create(aOwner : TComponent); override;
  end;

  { TMediaWidget }

  TMediaWidget = Class(TCustomTagWidget)
  private
    FAutoPlay: Boolean;
    FControls: Boolean;
    FCrossOrigin: String;
    FCurrentTime: Double;
    FDefaultMuted: Boolean;
    FDisableRemotePlayback: Boolean;
    FLoop: Boolean;
    FMuted: Boolean;
    FPreload: String;
    FPreservesPitch: Boolean;
    FSrc: String;
    FVolume: Double;
    procedure SetAutoPlay(AValue: Boolean);
    procedure SetControls(AValue: Boolean);
    procedure SetCrossOrigin(AValue: String);
    procedure SetCurrentTime(AValue: Double);
    procedure SetDefaultMuted(AValue: Boolean);
    procedure SetDisableRemotePlayback(AValue: Boolean);
    procedure SetLoop(AValue: Boolean);
    procedure SetMuted(AValue: Boolean);
    procedure SetPreload(AValue: String);
    procedure SetPreservesPitch(AValue: Boolean);
    procedure SetSrc(AValue: String);
    procedure SetVolume(AValue: Double);
  Published
    Property Src : String read FSrc write SetSrc;
    Property Controls : Boolean read FControls write SetControls;
    Property AutoPlay : Boolean read FAutoPlay write SetAutoPlay;
    Property CrossOrigin : String read FCrossOrigin write SetCrossOrigin;
    Property DefaultMuted : Boolean read FDefaultMuted write SetDefaultMuted;
    Property CurrentTime : Double read FCurrentTime write SetCurrentTime;
    Property DisableRemotePlayback : Boolean read FDisableRemotePlayback write SetDisableRemotePlayback;
    Property PreservesPitch : Boolean read FPreservesPitch write SetPreservesPitch;
    Property Loop : Boolean read FLoop write SetLoop;
    Property Muted : Boolean read FMuted write SetMuted;
    Property Preload : String read FPreload write SetPreload;
    Property Volume : Double read FVolume write SetVolume;
  end;

  { TVideoWidget }

  TVideoWidget = Class(TMediaWidget)
  Public
    Constructor Create(aOwner : TComponent); override;
  end;

  { TAudioWidget }

  TAudioWidget = Class(TMediaWidget)
  Public
    Constructor Create(aOwner : TComponent); override;
  end;


(*
Const
  TextTagNames : Array[TTextTag] of string
   = ('p','b','i','u','s','span','quote','blockquote','h1','h2','h3','h4','h5','h6','pre','ruby','article','address','abbr','');
  RowKindNames  : Array[TRowKind] of string = ('header','body','footer');

  HTMLTagNames : Array[THTMLElementTag] of string = (
    '?', 'a', 'abbr', 'acronym', 'address', 'applet', 'area', 'b', 'base',
    'basefont', 'bdo', 'big', 'blockquote', 'body', 'br', 'button',
    'caption', 'center', 'cite', 'code', 'col', 'colgroup', 'dd', 'del',
    'dfn', 'dir', 'div', 'dl', 'dt', 'em', 'fieldset', 'font', 'form',
    'frame', 'frameset', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'head', 'hr',
    'html', 'i', 'iframe', 'img', 'input', 'ins', 'isindex', 'kbd', 'label',
    'legend', 'li', 'link', 'map', 'menu', 'meta', 'noframes', 'noscript',
    'object', 'ol', 'optgroup', 'option', 'p', 'param', 'pre', 'q', 's',
    'samp', 'script', 'select', 'small', 'span', 'strike', 'strong',
    'style', 'sub', 'sup', 'table', 'tbody', 'td', 'textarea', 'tfoot',
    'th', 'thead', 'title', 'tr', 'tt', 'u', 'ul', 'var',
    'Text','Audio','Video','Source'
  );
*)

implementation

uses DateUtils;

resourcestring
  SErrInvalidRowCount = 'Invalid row count: %d';


{ TEventTableWidget }

procedure TEventTableWidget.SetRowCount(AValue: Integer);
begin
  if FRowCount=AValue then Exit;
  FRowCount:=AValue;
end;


{ TCustomStringsTableWidget.TStringRowsEnumerator }


{ TCustomStringsTableWidget }


function TCustomStringsTableWidget.GetRowCount: Integer;
begin
  Result:=0;
end;


procedure TCustomStringsTableWidget.SetRowCount(AValue: Integer);
begin
  if AValue<0 then
     raise EWidgets.CreateFmt(SerrInvalidRowCount, [aValue]);
end;

{ TTagWidget }

constructor TTagWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  ElementTag:=etdiv;
end;

{ TMediaWidget }



procedure TMediaWidget.SetAutoPlay(AValue: Boolean);
begin
  if FAutoPlay=AValue then Exit;
  FAutoPlay:=AValue;
end;



procedure TMediaWidget.SetControls(AValue: Boolean);
begin
  if FControls=AValue then Exit;
  FControls:=AValue;
end;

procedure TMediaWidget.SetCrossOrigin(AValue: String);
begin
  if FCrossOrigin=AValue then Exit;
  FCrossOrigin:=AValue;
end;

procedure TMediaWidget.SetCurrentTime(AValue: Double);
begin
  if FCurrentTime=AValue then Exit;
  FCurrentTime:=AValue;
end;

procedure TMediaWidget.SetDefaultMuted(AValue: Boolean);
begin
  if FDefaultMuted=AValue then Exit;
  FDefaultMuted:=AValue;
end;

procedure TMediaWidget.SetDisableRemotePlayback(AValue: Boolean);
begin
  if FDisableRemotePlayback=AValue then Exit;
  FDisableRemotePlayback:=AValue;
end;

procedure TMediaWidget.SetLoop(AValue: Boolean);
begin
  if FLoop=AValue then Exit;
  FLoop:=AValue;
end;

procedure TMediaWidget.SetMuted(AValue: Boolean);
begin
  if FMuted=AValue then Exit;
  FMuted:=AValue;
end;

procedure TMediaWidget.SetPreload(AValue: String);
begin
  if FPreload=AValue then Exit;
  FPreload:=AValue;
end;

procedure TMediaWidget.SetPreservesPitch(AValue: Boolean);
begin
  if FPreservesPitch=AValue then Exit;
  FPreservesPitch:=AValue;
end;


procedure TMediaWidget.SetVolume(AValue: Double);
begin
  if FVolume=AValue then Exit;
  FVolume:=AValue;
end;

procedure TMediaWidget.SetSrc(AValue: String);
begin
  if FSrc=AValue then Exit;
  FSrc:=AValue;
end;

{ TVideoWidget }

constructor TVideoWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  elementTag:=etVideo;
end;

{ TAudioWidget }

constructor TAudioWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  elementTag:=etAudio;
end;




{ TTableWidgetCellData }

procedure TTableWidgetCellData.SetRowColKind(aRow, aCol: Integer; aKind: TRowKind);
begin
  if (aRow<>-1) then
    FRow:=aRow;
  if (aCol<>-1) then
    FCol:=aCol;
  FKind:=aKind;
end;

procedure TTableWidgetCellData.Reset;
begin
  Ftag:='td';
  FClassNames:='';
  FText:='';
  FContent:=Nil;
  FAsHTML:=False;
  FWidget:=Nil;
end;

constructor TTableWidgetCellData.Create(aTable: TCustomTableWidget; aTableID: String);
begin
  FTable:=aTable;
  FTableID:=aTableID;
  SetRowColKind(0,0,rkBody);
end;

{ TCustomTableWidget }

procedure TCustomTableWidget.SetColumns(AValue: TCustomTableColumns);
begin
  if FColumns=AValue then Exit;
  FColumns.Assign(AValue);
end;

procedure TCustomTableWidget.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
end;


procedure TCustomTableWidget.SetTableOptions(AValue: TTableOptions);
begin
  if FTableOptions=AValue then Exit;
  FTableOptions:=AValue;
end;


function TCustomTableWidget.CreateColumns: TCustomTableColumns;
begin
  Result:=TCustomTableColumns.Create(TCustomTableColumn);
end;

function TCustomTableWidget.DefaultTableOptions: TTableOptions;
begin
  Result:=[toHeader,toBody,toFooter,toHeaderRow]
end;

constructor TCustomTableWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTableOptions:=DefaultTableOptions;
  FColumns:=CreateColumns;
end;

destructor TCustomTableWidget.Destroy;
begin
  FreeAndNil(FColumns);
  inherited Destroy;
end;


{ TCustomTableColumn }

procedure TCustomTableColumn.SetAlignment(AValue: TAlignment);
begin
  if FAlignment=AValue then Exit;
  FAlignment:=AValue;
end;

function TCustomTableColumn.GetCaption: String;
begin
  Result:=FCaption;
end;

procedure TCustomTableColumn.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
end;

procedure TCustomTableColumn.SetClassNames(AValue: String);
begin
  if FClassNames=AValue then Exit;
  FClassNames:=AValue;
end;

function TCustomTableColumn.RenderColumn: Boolean;
begin
  Result:=True;
end;

function TCustomTableColumn.GetDisplayName: string;
begin
  Result:=Caption;
end;

procedure TCustomTableColumn.Assign(Source: TPersistent);

Var
  C : TCustomTableColumn;

begin
  if Source is TCustomTableColumn then
    begin
    C:=Source as TCustomTableColumn;
    FCaption:=C.FCaption;
    FClassNames:=C.FClassNames;
    FAlignment:=C.Alignment;
    end
  else
    inherited Assign(Source);
end;

{ TCustomTableColumns }

function TCustomTableColumns.GetCustomColumn(Index : Integer): TCustomTableColumn;
begin
  Result:=TCustomTableColumn(Items[Index]);
end;

procedure TCustomTableColumns.SetCustomColumn(Index : Integer; AValue: TCustomTableColumn);
begin
  Items[Index]:=aValue;
end;

function TCustomTableColumns.Add(aCaption: String): TCustomTableColumn;
begin
  Result:=(Inherited add) as TCustomTableColumn;
  Result.Caption:=aCaption;
end;

{ TCustomTextWidget }

procedure TCustomTextWidget.SetEnvelopeTag(AValue: TTextTag);
begin
  // Writeln('Setting text tag : ',aValue);
  if FEnvelopeTag=AValue then Exit;
  FEnvelopeTag:=AValue;
  if (FEnvelopeTag=ttCustom) and (FCustomTag='') then
    FCustomTag:='div';
end;

procedure TCustomTextWidget.SetCustomTag(AValue: String);
begin
  if FCustomTag=AValue then Exit;
  FCustomTag:=AValue;
  if (FCustomTag<>'') then
    FEnvelopeTag:=ttCustom;
end;


procedure TCustomTextWidget.SetTextMode(AValue: TTextMode);
begin
  if FTextMode=AValue then Exit;
  FTextMode:=AValue;
end;



{ TTextLinesWidget }

procedure TTextLinesWidget.SetLines(AValue: TStrings);
begin
  if FLines=AValue then Exit;
  FLines.Assign(AValue);
end;

procedure TTextLinesWidget.SetForceLineBreaks(AValue: Boolean);
begin
  if FForceLineBreaks=AValue then Exit;
  FForceLineBreaks:=AValue;
end;


constructor TTextLinesWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FLines:=TstringList.Create;
end;

destructor TTextLinesWidget.Destroy;
begin
  FLines:=TstringList.Create;
  inherited Destroy;
end;

{ TTextWidget }

procedure TTextWidget.SetText(AValue: String);
begin
  if FText=AValue then Exit;
  FText:=AValue;
end;




{ TLabelWidget }

procedure TLabelWidget.SetLabelFor(AValue: TWebWidget);
begin
  if (FLabelFor=AValue) then Exit;
  if Assigned(FLabelFor) then
    FLabelFor.RemoveFreeNotification(Self);
  FLabelFor:=AValue;
  if Assigned(FLabelFor) then
    FLabelFor.FreeNotification(Self);
end;

function TLabelWidget.GetText: String;
begin
  Result:=FText;
end;

procedure TLabelWidget.SetText(AValue: String);
begin
  If Text=aValue then exit;
  Ftext:=aValue;
end;

procedure TLabelWidget.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (aComponent=FLabelFor) then
    FLabelFor:=Nil;
end;

procedure TLabelWidget.SetName(const NewName: TComponentName);

Var
  Old : String;

begin
  Old:=Name;
  inherited SetName(NewName);
  if (csDesigning in ComponentState) then
    if Old=Text then
      Text:=Old;
end;

{ TSelectWidget }

function TCustomSelectWidget.GetSelectedIndex: Integer;
begin
  Result:=FSelectedIndex
end;

function TCustomSelectWidget.GetMultiple: Boolean;

begin
  Result:=FMultiple;
end;



procedure TCustomSelectWidget.SetMultiple(AValue: Boolean);
begin
  If (AValue=Multiple) then exit;
  FMultiple:=aValue;
end;

procedure TCustomSelectWidget.SetSelectedIndex(AValue: Integer);

begin
  if (SelectedIndex=aValue) then
    Exit;
  FSelectedIndex:=aValue;
end;



constructor TCustomSelectWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOWner);
  FSelectedIndex:=-1;
end;


{ TSelectWidget }

function TSelectWidget.GetItems: TStrings;
begin
  Result:=FItems;
end;


function TSelectWidget.GetValues: TStrings;
begin
  Result:=FValues;
end;



procedure TSelectWidget.setItems(AValue: TStrings);
begin
  If (AValue=FItems) then exit;
  FItems.Assign(aValue);
end;


procedure TSelectWidget.setValues(AValue: TStrings);
begin
  If (AValue=FValues) then exit;
  FValues.Assign(aValue);
end;



constructor TSelectWidget.Create(aOWner: TComponent);
begin
  inherited Create(aOWner);
  FItems:=TStringList.Create;
  FValues:=TStringList.Create;
end;

destructor TSelectWidget.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FValues);
  inherited Destroy;
end;

{ TImageWidget }

function TImageWidget.GetHeight: Integer;
begin
  Result:=Fheight;
end;


function TImageWidget.GetSrc: String;
begin
  Result:=FSrc;
end;

function TImageWidget.GetWidth: Integer;
begin
  Result:=FWidth;
end;

procedure TImageWidget.SetHeight(AValue: Integer);
begin
  if AValue=Height then exit;
  FHeight:=AValue;
end;

procedure TImageWidget.SetSrc(AValue: String);
begin
  if AValue=Src then exit;
  FSrc:=AValue;
end;

procedure TImageWidget.SetWidth(AValue: Integer);
begin
  if AValue=Width then exit;
  FWidth:=AValue;
end;


{ TTextAreaWidget }

procedure TTextAreaWidget.SetLines(AValue: TStrings);
begin
  if FLines=AValue then Exit;
  FLines.Assign(AValue);
end;

procedure TTextAreaWidget.SetMaxLength(AValue: Cardinal);
begin
  if FMaxLength=AValue then Exit;
  FMaxLength:=AValue;
end;

procedure TTextAreaWidget.SetReadonly(AValue: Boolean);
begin
  If aValue=ReadOnly then exit;
  FReadOnly:=aValue;
end;

procedure TTextAreaWidget.SetRequired(AValue: Boolean);
begin
  If aValue=Required then exit;
  FRequired:=aValue;
end;

function TTextAreaWidget.GetColumns: Cardinal;
begin
  Result:=FColumns;
end;

function TTextAreaWidget.GetLines: TStrings;
begin
  Result:=FLines;
end;

function TTextAreaWidget.GetReadOnly: Boolean;
begin
  Result:=FReadonly;
end;

function TTextAreaWidget.GetRequired: Boolean;
begin
  Result:=FRequired;
end;

function TTextAreaWidget.GetRows: Cardinal;
begin
  Result:=FRows;
end;


function TTextAreaWidget.GetValueName: string;
begin
  Result:=FValueName;
end;

procedure TTextAreaWidget.SetColumns(AValue: Cardinal);
begin
  if AValue=FColumns then exit;
  FColumns:=aValue;
end;

procedure TTextAreaWidget.SetRows(AValue: Cardinal);
begin
  if AValue=FRows then exit;
  FRows:=aValue;
end;


procedure TTextAreaWidget.SetValueName(AValue: string);
begin
  if aValue=FValueName then exit;
  FValueName:=aValue;
end;

procedure TTextAreaWidget.SetName(const NewName: TComponentName);

var
  Old : String;
begin
  Old:=Name;
  inherited SetName(NewName);
  if csDesigning in ComponentState then
    begin
    if (FLines.Count=0) then
      FLines.Add(Name)
    else if (FLines.Count=1) and (FLines[0]=Old) then
      FLines[0]:=Name;
    end;
end;



constructor TTextAreaWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FLines:=TStringList.Create;
  FColumns:=50;
  FRows:=10;
end;

destructor TTextAreaWidget.Destroy;
begin
  FreeAndNil(Flines);
  inherited;
end;





procedure TTextAreaWidget.SetWrap(AValue: TTextAreaWrap);

begin
  if FWrap=AValue then Exit;
  FWrap:=AValue;
end;



{ TFileInputWidget }

procedure TFileInputWidget.SetMultiple(AValue: Boolean);
begin
  if FMultiple=AValue then Exit;
  FMultiple:=AValue;
end;

function TFileInputWidget.GetMultiple: Boolean;
begin
  Result:=FMultiple;
end;



{ TDateInputWidget }

function TDateInputWidget.GetDate: TDateTime;

begin
  Result:=FDate;
end;


procedure TDateInputWidget.SetDate(AValue: TDateTime);
begin
  FDate:=aValue;
  Value:=FormatDateTime('yyyy-mm-dd',FDate);
end;


{ TCheckableInputWidget }


procedure TCheckableInputWidget.SetChecked(AValue: Boolean);
begin
  // Get actual value
  if Checked=AValue then Exit;
  FChecked:=AValue;
end;


function TCheckableInputWidget.GetChecked: Boolean;
begin
  Result:=FChecked;
end;


{ TButtonInputWidget }

procedure TButtonInputWidget.SetButtonType(AValue: TInputButtonType);
begin
  if FButtonType=AValue then Exit;
  FButtonType:=AValue;
end;

procedure TButtonInputWidget.SetSrc(AValue: String);
begin
  if FSrc=AValue then Exit;
  FSrc:=AValue;
end;

{ TTextInputWidget }

function TTextInputWidget.GetAsNumber: NativeInt;
begin
  Result:=StrToIntDef(Value,0);
end;

function TTextInputWidget.GetMaxLength: NativeInt;
begin
  Result:=FMaxLength;
end;

function TTextInputWidget.GetMinLength: NativeInt;
begin
  Result:=FMinLength;
end;

function TTextInputWidget.GetTextType: TInputTextType;
begin
  Result:=FTextType;
end;

procedure TTextInputWidget.SetAsNumber(AValue: NativeInt);
begin
  Value:=IntToStr(aValue);
end;


procedure TTextInputWidget.SetMaxLength(AValue: NativeInt);
begin
  if (aValue=FMaxLength) then exit;
  FMaxLength:=aValue;
end;

procedure TTextInputWidget.SetMinLength(AValue: NativeInt);
begin
  if (aValue=FMinLength) then exit;
  FMinLength:=aValue;
end;

procedure TTextInputWidget.SetTextType(AValue: TInputTextType);
begin
  if aValue=FTextType then exit;
  FTextType:=aValue;
end;

{ TWebPage }

constructor TWebPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Classes:='WebPage';
end;


{ TViewPort }
{ TButtonWidget }

{ TButtonWidget }

procedure TButtonWidget.SetText(AValue: String);

begin
  if FText=AValue then Exit;
  FText:=AValue;
end;

procedure TButtonWidget.SetTextMode(AValue: TTextMode);
begin
  if FTextMode=AValue then Exit;
  FTextMode:=AValue;
end;



{ TCustomInputWidget }

function TCustomInputWidget.GetValue: String;

begin
  Result:=FValue
end;

function TCustomInputWidget.GetText: String;

begin
  Result:=FText;
end;

function TCustomInputWidget.GetReadOnly: Boolean;
begin
  Result:=FReadonly;
end;

function TCustomInputWidget.GetRequired: Boolean;
begin
  Result:=FRequired;
end;

function TCustomInputWidget.GetValueName: String;

begin
  Result:=FValueName;
  if Result='' then
    Result:=Name;
end;

procedure TCustomInputWidget.SetReadonly(AValue: Boolean);
begin
  If aValue=ReadOnly then exit;
  FReadOnly:=aValue;
end;

procedure TCustomInputWidget.SetRequired(AValue: Boolean);
begin
  If aValue=Required then exit;
  FRequired:=aValue;
end;

procedure TCustomInputWidget.SetText(AValue: String);

begin
  FText:=aValue;
end;

procedure TCustomInputWidget.SetValue(AValue: String);

begin
  FValue:=aValue;
end;

procedure TCustomInputWidget.SetValueName(AValue: String);

begin
  FValueName:=aValue;
end;

procedure TCustomInputWidget.SetName(const NewName: TComponentName);

Var
  Old : String;

begin
  Old:=Name;
  inherited SetName(NewName);
  if (Value=Old) then
    Value:=NewName;
end;


{ TCustomTagWidget }

procedure TCustomTagWidget.SetElementTag(AValue: THTMLElementTag);

begin
  if FElementTag=AValue then Exit;
  FElementTag:=AValue;
end;

procedure TCustomTagWidget.SetTextContent(AValue: String);
begin
  if FTextContent=AValue then Exit;
  FTextContent:=AValue;
end;


{ TDivWidget }


constructor TDivWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  ElementTag:=etDiv;
end;

constructor TParagraphWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  ElementTag:=etP;
end;


end.



