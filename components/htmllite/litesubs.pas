{Version 7.5}
{*********************************************************}
{*                     LITESUBS.PAS                      *}
{*              Copyright (c) 1995-2002 by               *}
{*                   L. David Baldwin                    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$i LiteCons.inc}

{
This module is comprised mostly of the various Section object definitions.
As the HTML document is parsed, it is divided up into sections.  Some sections
are quite simple, like TParagraphSpace.  Others are more complex such as
TSection which can hold a complete paragraph.

The HTML document is then stored as a list, TSectionList, of the various
sections.

Closely related to TSectionList is TCell.  TCell holds the list of sections for
each cell in a Table (the ThtmlTable section).  In this way each table cell may
contain a document of it's own.

The Section objects each store relevant data for the section such as the text,
fonts, images, and other info needed for formating.

Each Section object is responsible for its own formated layout.  The layout is
done in the DrawLogic method.  Layout for the whole document is done in the
TSectionList.DoLogic method which essentially just calls all the Section
DrawLogic's.  It's only necessary to call TSectionList.DoLogic when a new
layout is required (when the document is loaded or when its width changes).

Each Section is also responsible for drawing itself (its Draw method).  The
whole document is drawn with the TSectionList.Draw method.
}

unit LiteSubs;

{$IFNDEF HL_LAZARUS}
{$R HTML32.Res}
{$ENDIF not HL_LAZARUS}

interface
uses
  {$IFDEF HL_LAZARUS}
  Classes, SysUtils, VCLGlobals, LCLType, LCLLinux, Messages,
  GraphType, Graphics, Controls, Forms, Dialogs, Buttons, StdCtrls, ExtCtrls,
  LiteUn2, LiteGif2;
  {$ELSE}
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, LiteUn2, LiteGif2, mmSystem;
  {$ENDIF}

const
  MaxCols = 200;  {number columns allowed in table}  

type
  TGetImageEvent = procedure(Sender: TObject; const SRC: string;
                    var Stream: TMemoryStream) of Object;
  TFormSubmitEvent = procedure(Sender: TObject; const Action, Target, EncType, Method: string;
                    Results: TStringList) of Object;
  TObjectClickEvent = procedure(Sender, Obj: TObject; const OnClick: string) of Object;
  TExpandNameEvent = procedure(Sender: TObject; const SRC: string; var Result: string) of Object;
  SubSuperType = (Normal, SubSc, SupSc);
  TCell = Class;
  TSectionList = Class;
  TSection = Class;

  TMyFont = class(TFont)
  public
    NormalSize: integer;   {normal unscaled size}
    Fixed: boolean;        {set if font is fixed font and can't be changed}
    procedure Assign(Source: TPersistent); override;
    procedure SetNormalSize(List: TSectionList; Value: integer);
    procedure UpdateFont(List: TSectionList; NewColor: TColor);
  end;

  TFontObj = class(TObject)   {font information}
  private
    Section: TSection;
    FVisited, FHover: boolean;
    procedure SetVisited(Value: boolean);
    procedure SetHover(Value: boolean);
    function GetURL: string;
  public
    Pos : integer;        {0..Len  Index where font takes effect}
    TheFont : TMyFont;
    FontHeight,       {tmHeight+tmExternalLeading}
    tmHeight,
    Overhang, Descent : integer;
    SScript: SubSuperType;  {Normal, SubSc, SupSc}
    UrlTarget: TUrlTarget;
    constructor Create(ASection: TSection; F: TMyFont; Position: integer);
    destructor Destroy; override;
    procedure UpdateFont;
    procedure FontChanged(Sender: TObject);
    function GetOverhang : integer;
    function GetHeight(var Desc: integer): integer;

    property URL: string read GetURL;     
    property Visited: boolean read FVisited Write SetVisited;
    property Hover: boolean read FHover Write SetHover;
  end;

  TFontList = class(TFreeList)  {a list of TFontObj's}
  Public
    procedure UpDateFonts;
    function GetFontAt(Posn : integer; var OHang : integer) : TMyFont;
    function GetFontCountAt(Posn, Leng : integer) : integer;
    function GetFontObjAt(Posn : integer;
                      var Index : integer) : TFontObj;
  end;

  TImageFormControlObj = class;

  TFloatingObj = class(TObject)
  protected
    Pos : integer;        {0..Len  index of image position}
    ImageHeight,          {does not include VSpace}  
    ImageWidth: integer;
    ObjAlign: AlignmentType;
    Indent: integer;
    HSpace, VSpace:  integer;       {horizontal, vertical extra space}  
  end;

  TImageObj = class(TFloatingObj)   {inline image info} 
  private
    FBitmap: TBitmap;
    FHover, FHoverImage: boolean;
    function GetBitmap: TBitmap;
    procedure SetHover(Value: boolean);
  public
    SpecHeight, SpecWidth: integer;   {as specified by <img> tag}
    PercentWidth: boolean;           {if width is percent}
    ObjHeight, ObjWidth: integer;   {width as drawn}
    ImageKnown: boolean;      {know size of image}
    Source, Alt : String;    {the src= and alt= attributes}
    NoBorder: boolean;        {set if don't want blue border}
    Image: TPersistent;  {bitmap possibly converted from GIF, Jpeg, etc or animated GIF}
    Mask: TBitmap;    {Image's mask if needed for transparency}
    ParentSectionList: TSectionList;
    Transparent: Transparency;    {None, Lower Left Corner, or Transp GIF}
    IsMap, UseMap: boolean;
    HasBlueBox: boolean;          {Link box drawn around image}
    DrawX: integer;
    DrawYY: integer;
    MapName: String;
    MyFormControl: TImageFormControlObj;  {if an <INPUT type=image}
    MyCell: TCell;
    constructor Create(Position: integer; L: TAttributeList);
    destructor Destroy; override;
    procedure DrawLogic(SectionList: TSectionList; Canvas: TCanvas;
                  FO: TFontObj; AvailableWidth: integer);
    procedure Draw(Canvas: TCanvas; X: integer; TopY, YBaseline: integer; FO: TFontObj);
    function InsertImage(const UName: String; var Reformat: boolean): boolean;

    property Bitmap: TBitmap read GetBitmap;
    property Hover: boolean read FHover write SetHover;
  end;

  TImageObjList = class(TFreeList)  {a list of TImageObj's}  
  Public
    function FindImage(Posn: integer): TFloatingObj;
    function GetHeightAt(Posn: integer; var AAlign: AlignmentType) : Integer;
    function GetWidthAt(Posn: integer; var AAlign: AlignmentType; var HSpc: integer) : integer;
    function GetImageCountAt(Posn: integer): integer;
    function PtInImage(X: integer; Y: integer; var IX, IY, Posn: integer;
                var AMap, UMap: boolean; var MapItem: TMapItem;
                var ImageObj: TImageObj): boolean;    
    function PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean;    
  end;

  IndentManager = class(IndentManagerBasic)
    procedure Update(Y: integer; Img: TFloatingObj);  
    end;

  TFormControlObj = class;
  TRadioButtonFormControlObj = class;

  ThtmlForm = class(TObject)
  Public
    MasterList: TSectionList;
    Method: string[4];
    Action, Target, EncType: String;
    ControlList: TFreeList;
    NonHiddenCount: integer;
    constructor Create(AMasterList: TSectionList; L : TAttributeList);
    destructor Destroy; override;
    procedure DoRadios(Radio: TRadioButtonFormControlObj);
    procedure InsertControl(Ctrl: TFormControlObj);
    procedure ResetControls;
    procedure SubmitTheForm(const ButtonSubmission: string);
    procedure SetSizes(Canvas: TCanvas);
    procedure ControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

  TFormControlObj = class(TObject)
  private
    FYValue: integer;
    function GetControl: TWinControl; virtual;
  public
    Pos : integer;        {0..Len  index of image position}
    MasterList: TSectionList;
    MyForm: ThtmlForm;
    Value, Name: String;
    BaseLine: boolean;   {True if sits on text baseline}
    FControl: TWinControl;
    ShowIt: boolean;
    OnClickMessage: String;   

    constructor Create(AMasterList: TSectionList; Position: integer; L: TAttributeList);
    destructor Destroy; override;
    procedure ResetToValue; virtual;
    function GetSubmission(Index: integer; var S: string): boolean; virtual;
    procedure SetHeightWidth(Canvas: TCanvas); virtual;
    procedure EnterEvent(Sender: TObject);    {these two would be better private}
    procedure ExitEvent(Sender: TObject);
    procedure FormControlClick(Sender: TObject);

    property TheControl: TWinControl read GetControl;  {the Delphi control, TButton, TMemo, etc}
    property YValue: integer read FYValue;
  end;

  TImageFormControlObj = class(TFormControlObj)
  public
    XPos, YPos, XTmp, YTmp: integer;   {click position}
    constructor Create(AMasterList: TSectionList; Position: integer; L: TAttributeList);
    procedure ImageClick;
    function GetSubmission(Index: integer; var S: string): boolean; override;
  end;

  THiddenFormControlObj = class(TFormControlObj)
    function GetSubmission(Index: integer; var S: string): boolean; override;
  end;

  TEditFormControlObj = class(TFormControlObj)
  public
    EditSize: integer;
    constructor Create(AMasterList: TSectionList; Position: integer;
                L: TAttributeList; const Typ: string);
    procedure ResetToValue; override;
    function GetSubmission(Index: integer; var S: string): boolean; override;
    procedure SetHeightWidth(Canvas: TCanvas); override;
  end;

  WhichType = (Submit, ResetB, Button);

  TButtonFormControlObj = class(TFormControlObj)
  public
    Which: WhichType;
    constructor Create(AMasterList: TSectionList; Position: integer;
                L: TAttributeList; const Typ: string);
    procedure ButtonClick(Sender: TObject);
    procedure SetHeightWidth(Canvas: TCanvas); override;
  end;

  TRadioButtonFormControlObj = class(TFormControlObj)
  private
    function GetControl: TWinControl; override;
  public
    IsChecked: boolean;
    RButton: TRadioButton;
    MyCell: TCell;
    constructor Create(AMasterList: TSectionList; Position: integer;
                L: TAttributeList; ACell: TCell);
    procedure RadioClick(Sender: TObject);
    procedure ResetToValue; override;
    function GetSubmission(Index: integer; var S: string): boolean; override;
  end;

  TCheckBoxFormControlObj = class(TFormControlObj)
  public
    IsChecked: boolean;
    constructor Create(AMasterList: TSectionList; Position: integer; L: TAttributeList);
    procedure ResetToValue; override;
    function GetSubmission(Index: integer; var S: string): boolean; override;
  end;

  LineRec = class(TObject)  {holds info on a line of text}
    Start : PChar;          {where it starts in Buff}
    SpaceBefore, SpaceAfter,
    LineHt,                 {total height of line}
    LineImgHt,              {top to bottom including any floating image}
    Ln,                     {# chars in line}
    Descent,
    LineIndent : integer;
    DrawX: integer;
    DrawY: integer;
    end;

  TSectionBase = class(TObject)   {abstract base for document sections}
  public
    ParentSectionList: TSectionList;   {what list it's in}
    SectionHeight: integer;            {pixel height of section}
    DrawHeight: integer;               {floating image may overhang} 
    YValue: integer;                   {Vertical position at top}
    StartCurs: integer;
    Len: integer;
    constructor Create(AMasterList: TSectionList);
    procedure CopyToClipboard; virtual;
    function DrawLogic(Canvas : TCanvas; Y: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; virtual;
    function Draw(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X : integer; Y: integer) : integer;  virtual;
    function GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj): boolean; virtual;
    function PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean; virtual;
    function FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var SCell: TObject; var Intext: boolean): integer; virtual;
    function FindString(From: integer; PC: PChar; MatchCase: boolean): integer; virtual;
    function FindSourcePos(DocPos: integer): integer; virtual;    
    function FindDocPos(SourcePos: integer; Prev: boolean): integer; virtual;    
    function CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean; virtual;
    function GetChAtPos(Pos: integer; var Ch: char; var Obj: TObject): boolean; virtual;
    procedure UpdateFonts; virtual;
    procedure UpdateSpacing; virtual;
    procedure SetParent(List: TSectionList);
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); virtual;
    end;

  IntArray = array[0..MaxCols] of integer;

  TCellList = class(TFreeList)  {a list of TCellObj's to form a table row}
  public
    RowHeight: integer;
    RowSpanHeight: integer;   {height of largest rowspan}
    BkGnd: boolean;
    BkColor: TColor;
    procedure DoAttributes(Attr: TAttributeList);
    procedure InitializeRow;
    function DrawLogic1(Canvas : TCanvas; const Widths : IntArray; Span,
          CellPadding, CellSpacing: integer; var More: boolean): integer;
    procedure DrawLogic2(Canvas : TCanvas; Y: integer; CellPadding,
              CellSpacing: integer; var Curs: integer);
    function Draw(Canvas: TCanvas; MasterList: TSectionList; const ARect: TRect;
         const Widths : IntArray; X: integer; Y, YOffset: integer; CellPadding,
         CellSpacing : integer; Border: boolean; Rgn: THandle; MyRow: integer) : integer;
    procedure UpdateFonts;
    end;

  TCellObj = Class;

  ThtmlTable = class(TSectionBase)   {holds a Table}
  public
    Rows: TFreeList;   {a list of TCellLists}
    Caption: TCellObj; {holds the caption info}
    ListsProcessed,
    Border,                   {if has a border}
    TopCaption: boolean;      {if caption is on top (vs. bottom)}
    Indent,                   {table indent}
    CaptionIndent: integer;   {indent of caption}
    Justify: JustifyType;     {Left, Center, Right}
    Float: boolean;           {if floating}
    NumCols,                  {Number columns in table}
    TableWidth,               {width of table}
    CaptionWidth: integer;    {width of caption}
    WidthAttr: integer;    {Width attribute as entered}
    AsPercent: boolean;    {if it's a percent}
    HeightAttr: integer;   {Height attribute as entered}  
    HtAsPercent: boolean;  {if it's a percent}            
    UseAbsolute: boolean;  {width entries are considered absolute}
    CaptionHeight,            {height of caption itself}
    TableHeight: integer;     {height of table itself, not incl caption}
    CellPadding, CellSpacing: integer;
    CaptionMinWidth: integer; {minimum width caption can be shrunk to}
    Widths,                   {holds column widths}
    Percents: IntArray;       {percent widths of columns}
    HSpace, VSpace:  integer; {horizontal, vertical extra space}
    Level: integer;           {indent level}
    EndList: boolean;     {marker for copy}
    DrawX: integer;
    DrawY: integer;
    BkGnd, BdrOn: boolean;
    BkColor, BdrColor: TColor;
    MyCell: TCell;

    constructor Create(Master: TSectionList;Attr: TAttributeList;
                    AJustify: JustifyType; ACell: TCell; ALevel: integer);
    destructor Destroy; override;
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); override;
    procedure AddDummyCells;
    procedure GetMinMaxAbs(Canvas: TCanvas; var TotalMinWidth,
         TotalMaxWidth: integer; var MinWidths, MaxWidths: IntArray);
    procedure GetWidthsAbs(Canvas: TCanvas; TablWidth: integer; Specified: boolean;
                var MinWidths, MaxWidths: IntArray);
    procedure GetWidths(Canvas: TCanvas; var TotalMinWidth, TotalMaxWidth: integer;
              var MinWidths, MaxWidths: IntArray; TheWidth: integer);
    procedure xxx(const MaxWidths, MinWidths: IntArray; TheWidth: integer);
    function DrawLogic(Canvas : TCanvas; Y: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; override;
    function Draw(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X : integer; Y: integer) : integer;  override;
    function GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj): boolean; override;
    function PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean; override;
    function FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var SCell: TObject; var Intext: boolean): integer; override;
    function CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean; override;
    function GetChAtPos(Pos: integer; var Ch: char; var Obj: TObject): boolean; override;
    function FindString(From: integer; PC: PChar; MatchCase: boolean):
                              integer; override;
    function FindSourcePos(DocPos: integer): integer; override;    
    function FindDocPos(SourcePos: integer; Prev: boolean): integer; override;    
    procedure CopyToClipboard; override;
    procedure UpdateFonts; override;
    end;

  ClearAttrType = (clrNone, clLeft, clRight, clAll);
  ListTypeType = (None, Ordered, Unordered, Definition);
  XArray = array[0..300] of integer;
  PXArray = ^XArray;

  IndexObj = class
    Pos: integer;
    Index: integer;
    end;

  TSection = class(TSectionBase)
  {TSection holds <p>, <li>, many other things, and the base for lists}
  private
    function GetIndexObj(I: integer): IndexObj;   
    property PosIndex[I: integer]: IndexObj read GetIndexObj;
  public
    Buff : PChar;            {holds the text for the section}
    XP: PXArray;   
    BuffSize: integer;       {buffer may be larger}
    Fonts : TFontList;   {List of FontObj's in this section}
    Images: TImageObjList;   {list of TImageObj's, the images in section}
    FormControls: TList;      {list of TFormControls in section}
    SIndexList: TFreeList;    {list of Source index changes}  
    Level,            {nesting level of lists}
    Indent,           {indent of section}
    ListNumb : integer;  {1, 2, 3, etc for ordered lists}
    Lines : TFreeList;   {List of LineRecs,  info on all the lines in section}
    DefFont : TMyFont;
    ListType: ListTypeType;
    Justify: JustifyType; {Left, Centered, Right}
    ClearAttr: ClearAttrType;
    LevelIndent: integer;   {The indent for this list level}

    constructor Create(AMasterList: TSectionList; ALevel: integer; AFont: TMyFont;
                AnURL: TUrlTarget; AJustify: JustifyType);
    destructor Destroy; override;
    procedure DoClearAttribute(L: TAttributeList);
    procedure Finish;
    procedure AddChar(C: char; Index: integer; NoBreak: boolean); 
    procedure AddTokenObj(S : TokenObj; NoBreak: boolean); virtual;
    function BreakInfo(Index: integer; NoBreak: boolean): JustifyType;
    procedure Allocate(N : integer);
    function AddImage(L: TAttributeList; ACell: TCell; Index: integer; NoBreak: boolean): TImageObj;
    function AddFormControl(Which: Symb; AMasterList: TSectionList;
             L: TAttributeList; ACell: TCell; Index: integer; NoBreak: boolean): TFormControlObj;
    procedure ChangeFont(List: TSectionList; NewFont: TMyFont);
    procedure ChangeStyle(Sy: Symb);
    procedure HRef(Sy: Symb; List: TSectionList; AnURL: TUrlTarget;
              AFont: TMyFont);
    function FindCountThatFits(Canvas: TCanvas; Width : integer; Start : PChar;
                                  Max : integer) : integer;
    function FindCountThatFits1(Canvas: TCanvas; Width : integer;
                    Start : PChar; Max: integer; Y: integer; IMgr: IndentManager;
                    var ImgHt: integer; NxImages: TList) : integer;
    function FindTextWidth(Canvas: TCanvas; Start: PChar; N: integer;
                           RemoveSpaces: boolean): integer;
    function DrawLogic(Canvas : TCanvas; Y: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; override;
    function Draw(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X : integer; Y: integer) : integer;  override;
    procedure CopyToClipboard; override;
    function GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj): boolean; override;
    function PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean; override;   
    function FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var SCell: TObject; var Intext: boolean): integer; override;
    function FindString(From: integer; PC: PChar; MatchCase: boolean):
                              integer; override;
    function FindSourcePos(DocPos: integer): integer; override;    
    function FindDocPos(SourcePos: integer; Prev: boolean): integer; override;    
    procedure UpdateFonts; override;
    function CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean; override;
    function GetChAtPos(Pos: integer; var Ch: char; var Obj: TObject): boolean; override;
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); override;
    end;

  TCell = class(TFreeList)   {a list which holds sections of a table cell}
    MasterList: TSectionList;  {the TSectionList that holds the whole document}
    FontSize: integer;
    YValue: integer;   {vertical position at top of cell}
    StartCurs: integer;
    Len: integer;
    IMgr: IndentManager;
    BkGnd: boolean;
    BkColor: TColor;

    constructor Create(Master: TSectionList);
    destructor Destroy; override;
    procedure Add(Item: TSectionBase);
    procedure CopyToClipboard;
    procedure UpdateFonts;
    function DoLogic(Canvas: TCanvas; Y: integer; Width: integer;
                  var ScrollWidth: integer; var Curs: integer;
                  StartY,  StartCount: integer): integer; virtual;
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); virtual;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: integer;
                          Y:integer): integer; virtual;
    function FindSectionAtPosition(Pos: integer;
             var TopPos: integer; var Index: integer): TSectionBase;
    function GetURL(Canvas: TCanvas; X: integer; Y: integer;
             var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj): boolean; virtual;
    function PtInObject(X: integer; Y: integer;  var Obj: TObject;  
              var IX, IY: integer): boolean;
    function FindCursor(Canvas: TCanvas; X: Integer; Y: integer;
                var XR: integer; var YR: integer; var Ht: integer;
                var SCell: TObject; var Intext: boolean): integer;
    function FindString(From: integer; PC: PChar; MatchCase: boolean): integer;
    function FindSourcePos(DocPos: integer): integer;     
    function FindDocPos(SourcePos: integer; Prev: boolean): integer;     
    function CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean;
    function GetChAtPos(Pos: integer; var Ch: char; var Obj: TObject): boolean;
    end;

  TSectionList = class(TCell)    {a list of all the sections--holds document}
  Private
    procedure AdjustFormControls;
  Public
    ShowImages: boolean;     {set if showing images}
    YOff: integer;           {marks top of window that's displayed}
    YOffChange: boolean;     {when above changes}
    NoPartialLine: boolean;  {set when printing if no partial line allowed
                              at page bottom}
    SelB, SelE: integer;
    FontName : string[lf_FaceSize+1];  {font info for document}
    PreFontName : string[lf_FaceSize+1];  {<pre>, <code> font for document}
    FontColor,
    LinkVisitedColor, LinkActiveColor,
    HotSpotColor: TColor;
    {$ifdef ver100_plus}
    Charset: TFontCharset;
    {$endif}
    UnLine: TFontStyles;      {[fsUnderline] or [] depending on htNoLinkUnderline}
    TheOwner: TWinControl;        {the viewer that owns this document}
    PPanel: TWinControl;          {the viewer's PaintPanel}
    GetImage: TGetImageEvent;     {for OnImageRequest Event}
    ExpandName: TExpandNameEvent;
    ObjectClick: TObjectClickEvent;
    BackGround: TColor;
    OnBackgroundChange: TNotifyEvent;
    BackgroundBitmap: TBitmap;
    BackgroundMask: TBitmap;
    BitmapName: String;      {name of background bitmap}
    BitmapLoaded: boolean;   {if background bitmap is loaded}
    htmlFormList: TFreeList;
    AGifList: TList;      {list of all animated Gifs}
    SubmitForm: TFormSubmitEvent;
    ScriptEvent: TScriptEvent;
    CB: SelTextCount;
    PageBottom: integer;
    MapList: TFreeList;    {holds list of client maps, TMapItems}
    Timer: TTimer;      {for animated GIFs}
    FormControlList:  TList;   {List of all TFormControlObj's in this SectionList}
    MissingImages: TStringList;  {images to be supplied later}
    ControlEnterEvent: TNotifyEvent;
    LinkList: TList;    {List of links (TFontObj's)}
    ActiveLink: TFontObj;
    LinksActive: boolean;
    ActiveImage: TImageObj;
    ShowDummyCaret: boolean;
    Parser: TObject;

    constructor Create(Owner, APaintPanel: TWinControl);
    procedure Clear;
    destructor Destroy; override;
    procedure CheckGIFList(Sender: TObject);
    procedure SetYOffset(Y: integer);
    function GetSelLength: integer;
    function GetSelTextBuf(Buffer: PChar; BufSize: integer): integer;
    procedure SetFonts(const Name, PreName: String; ASize: integer;
              AColor, AHotSpot, AVisitedColor, AActiveColor, ABackground: TColor;
              LnksActive: boolean);
    procedure SetBackground(ABackground: TColor);
    procedure SetBackgroundBitmap(Name: String);
    function GetBackgroundBitmap: TBitmap;
    function FindPositionByIndex(Index: integer): integer;
    procedure CancelActives;
    function GetURL(Canvas: TCanvas; X: integer; Y: integer;
             var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj): boolean; override;
    function GetTheBitmap(const BMName: String; var Transparent: Transparency;
               var AMask: TBitmap; var FromCache, Delay: boolean): TPersistent;
    function DoLogic(Canvas: TCanvas; Y: integer; Width: integer;
                  var ScrollWidth: integer; var Curs: integer;
                  StartY,  StartCount: integer): integer; override;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: integer;
                          Y:integer): integer; override;
    procedure InsertImage(const Src: string; Stream: TMemoryStream; var Reformat: boolean);
  end;

  TCellObj = class(TObject)  {holds a TCell and some other information}
    ColSpan, RowSpan,      {column and row spans for this cell}
    Wd: integer;  {total width (may cover more than one column)}
    Ht,           {total height (may cover more than one row)}
    VSize: integer;     {Actual vertical size of contents}
    SpecHt: integer;    {Height as specified}
    YIndent: integer;   {Vertical indent}
    VAlign: AlignmentType;  {Top, Middle, or Bottom}
    WidthAttr: integer;   {Width attribute (percentage or absolute)}
    AsPercent: boolean;   {it's a percent}
    Cell: TCell;

    constructor Create(Master: TSectionList; AVAlign: AlignmentType;
                Attr: TAttributeList);
    destructor Destroy; override;
    procedure UpdateFonts;
    end;

const
  SmallListIndent = 15;  {for <li> without <ul>}
  ImageSpace = 5;   {extra space for left, right images}

var
  ListIndent: integer{$IFNDEF HL_LAZARUS} = 35{$ENDIF};  {defines successive indents}

implementation

uses htmllite, LitePars, LiteSbs1, LiteReadThd;

type
  TSectionClass = Class of TSectionBase;
  EProcessError = class(Exception);

procedure IndentManager.Update(Y: integer; Img: TFloatingObj);
{Given a new floating image, update the edge information.  Fills  Img.Indent,
 the distance from the left edge to the upper left corner of the image}
var
  IH, IW: integer;
  IR: IndentRec;
begin
if Assigned(Img) then
  begin
  IW := Img.ImageWidth + Img.HSpace;
  IH := Img.ImageHeight + 2*Img.VSpace;               
  if (Img.ObjAlign = ALeft) then   
    begin
    IR := IndentRec.Create;
    with IR do
      begin
      Img.Indent := LeftIndent(Y)-LfEdge;
      X := Img.Indent + IW;               
      YT := Y;
      YB := Y + IH;
      Lev := 0;
      L.Add(IR);
      end;
    end
  else if (Img.ObjAlign = ARight) then  
    begin
    IR := IndentRec.Create;
    with IR do
      begin
      X := RightSide(Y) - RtEdge - IW;
      Img.Indent := X + RtEdge + Img.HSpace;
      YT := Y;
      YB := Y + IH;
      Lev := 0;
      R.Add(IR);
      end;
    end;
  end;
end;

{----------------TMyFont.Assign}
procedure TMyFont.Assign(Source: TPersistent);
begin
if Source is TMyFont then
  begin
  NormalSize := TMyFont(Source).NormalSize;
  Fixed := TMyFont(Source).Fixed;
  end;
inherited Assign(Source);
end;

procedure TMyFont.SetNormalSize(List: TSectionList; Value: integer);
begin
NormalSize := Value;
Size := MulDiv(List.FontSize, Value, 12);
end;

procedure TMyFont.UpdateFont(List: TSectionList; NewColor: TColor);
begin
if not Fixed then Name := List.FontName
  else Name := List.PreFontName;
{$ifdef ver100_plus}    
Charset := List.Charset;
{$endif}
Size := MulDiv(List.FontSize, NormalSize, 12);  {Scale the font size}
Color := NewColor or $2000000;   
end;

constructor TFontObj.Create(ASection: TSection; F: TMyFont; Position: integer);
begin
inherited Create;
Section := ASection;
TheFont := F;
TheFont.OnChange := {$IFDEF HL_LAZARUS}@{$ENDIF}FontChanged;
Pos := Position;
UrlTarget := TUrlTarget.Create;
FontChanged(Self);
end;

destructor TFontObj.Destroy;
begin
TheFont.Free;
UrlTarget.Free;
inherited Destroy;
end;

procedure TFontObj.SetVisited(Value: boolean);
begin
if Value <> FVisited then
  begin
  FVisited := Value;
  if FHover then
    TheFont.Color := Section.ParentSectionList.LinkActiveColor or $2000000
  else if Value then
    TheFont.Color := Section.ParentSectionList.LinkVisitedColor or $2000000
  else
    TheFont.Color := Section.ParentSectionList.HotspotColor or $2000000;
  end;
end;

procedure TFontObj.SetHover(Value: boolean);
begin
if Value <> FHover then
  begin
  FHover := Value;
  if FHover then
    TheFont.Color := Section.ParentSectionList.LinkActiveColor or $2000000
  else if FVisited then
    TheFont.Color := Section.ParentSectionList.LinkVisitedColor or $2000000
  else
    TheFont.Color := Section.ParentSectionList.HotspotColor or $2000000;
  end;
end;

function TFontObj.GetURL: string;      
begin
Result := UrlTarget.Url;
end;

procedure TFontObj.UpdateFont;   
var
  Color: TColor;
begin
if UrlTarget.Url <> '' then Color := Section.ParentSectionList.HotSpotColor
else Color := Section.ParentSectionList.FontColor;
TheFont.UpdateFont(Section.ParentSectionList, Color);
end;

procedure TFontObj.FontChanged(Sender: TObject);
var
  Save: THandle;
  tm : TTextmetric;
  DC: HDC;
begin
DC := GetDC(0);
Save := SelectObject(DC, TheFont.Handle);
GetTextMetrics(DC, tm);
tmHeight := tm.tmHeight;
FontHeight := tm.tmHeight + tm.tmExternalLeading;
Descent := tm.tmDescent;
Overhang := tm.tmOverhang;
SelectObject(DC, Save);
ReleaseDC(0, DC);
end;

function TFontObj.GetOverhang: integer;
begin
Result := Overhang;
end;

function TFontObj.GetHeight(var Desc: integer): integer;
begin
Desc := Descent;
Result := FontHeight;
end;

procedure TFontList.UpDateFonts;
var
  I: integer;
begin
for I := 0 to Count-1 do
  TFontObj(Items[I]).UpdateFont;
end;

function TFontList.GetFontAt(Posn : integer;
                var OHang : integer) : TMyFont;
{given a character index, find the font that's effective there}
var
  I, PosX: integer;
  F : TFontObj;
begin
I := 0;
PosX := 0;
while (I < Count)  do
  begin
  PosX := TFontObj(Items[I]).Pos;
  Inc(I);
  if PosX >= Posn then Break;
  end;
Dec(I);
if PosX > Posn then Dec(I);
F := TFontObj(Items[I]);
OHang := F.GetOverhang;
Result := F.TheFont;
end;

function TFontList.GetFontCountAt(Posn, Leng : integer) : integer;
{Given a position, return the number of chars before the font changes}
var
  I, PosX : integer;
begin
I := 0;
PosX := 0;
while I < Count do
  begin
  PosX := TFontObj(Items[I]).Pos;
  if PosX >= Posn then Break;
  Inc(I);
  end;
if PosX = Posn then Inc(I);
if I = Count then
  Result := Leng-Posn
else
  Result := TFontObj(Items[I]).Pos - Posn;
end;

{----------------TFontList.GetFontObjAt}
function TFontList.GetFontObjAt(Posn : integer;
                      var Index : integer) : TFontObj;
{Given a position, returns the FontObj which applies there and the index of
 the FontObj in the list}
var
  PosX: integer;
begin
Index := 0;
PosX := 0;
while (Index < Count)  do
  begin
  PosX := TFontObj(Items[Index]).Pos;
  Inc(Index);
  if PosX >= Posn then Break;
  end;
Dec(Index);
if PosX > Posn then Dec(Index);
Result := TFontObj(Items[Index]);
end;

{----------------TImageObj.Create}
constructor TImageObj.Create(Position: integer; L: TAttributeList);
var
  I: integer;
  S: string;
  NewSpace: integer;  
begin
inherited Create;
Pos := Position;
ObjAlign := ABottom;   {default}  
NewSpace := -1;  
for I := 0 to L.Count-1 do
  with TAttribute(L[I]) do
    case Which of
      SrcSy: Source := Name;
      AltSy: Alt := Name;
      IsMapSy:  IsMap := True;
      UseMapSy:
        begin
        UseMap := True;
        S := Trim(Uppercase(Name));
        if (Length(S) > 1) and (S[1] = '#') then
          System.Delete(S, 1, 1);
        MapName := S;
        end;
      AlignSy:
        begin
        S := UpperCase(Name);
        if S = 'TOP' then ObjAlign := ATop
        else if (S = 'MIDDLE') or (S = 'ABSMIDDLE') then ObjAlign := AMiddle
        else if S = 'LEFT' then ObjAlign := ALeft
        else if S = 'RIGHT' then ObjAlign := ARight;
        end;
      BorderSy: NoBorder := Value = 0;
      TranspSy: Transparent := LLCorner;
      HeightSy: SpecHeight := Intmax(1, Value); {spec ht of 0 becomes 1}  
      WidthSy: if System.Pos('%', Name) = 0 then
                   SpecWidth := Value
               else if (Value > 0) and (Value <=100) then
                 begin
                 SpecWidth := Value;
                 PercentWidth := True;
                 end;
      HSpaceSy:  NewSpace := IntMin(40, Abs(Value));  
      VSpaceSy:  VSpace := IntMin(40, Abs(Value));   
      ActiveSy:  FHoverImage := True;
      end;
if NewSpace >= 0 then
  HSpace := NewSpace
else if ObjAlign in [ALeft, ARight] then
  HSpace := ImageSpace   {default}
else HSpace := 0;
end;

destructor TImageObj.Destroy;
begin
if (Source <> '') then
  BitmapList.DecUsage(Source);
if (Image is TGifImage) and TGifImage(Image).IsCopy then   
  Image.Free;
FBitmap.Free;
inherited Destroy;
end;

function TImageObj.GetBitmap: TBitmap;    
begin
Result := Nil;
if Image = ErrorBitmap then Exit;
if (Image is TGifImage) then
  Result := TGifImage(Image).Bitmap
else if (Image is TBitmap) then
  begin
  if Assigned(FBitmap) then
    Result := FBitmap
  else
    begin
    FBitmap := TBitmap.Create;
    FBitmap.Assign(TBitmap(Image));
    FBitmap.Palette := CopyPalette(ThePalette);
    Result := FBitmap;
    end;
  end;
end;

procedure TImageObj.SetHover(Value: boolean);
begin
if (Value <> FHover) and FHoverImage and (Image is TGifImage) then
  with TGifImage(Image) do
    begin
    if Value then
      if NumFrames = 2 then
        CurrentFrame := 2
      else
        begin
        Animate := True;
        ParentSectionList.AGifList.Add(Image);
        end
    else
      begin
      Animate := False;
      CurrentFrame := 1;
      ParentSectionList.AGifList.Remove(Image);
      end;
    FHover := Value;
    end;
end;

{----------------TImageObj.InsertImage}
function TImageObj.InsertImage(const UName: string; var Reformat: boolean): boolean;   
var
  TmpImage: TPersistent;
  FromCache, IsAniGIF, Delay: boolean;
begin
Result := False;
Reformat := False;
if (Image = DefBitmap) then
  begin
  Result := True;
  TmpImage := ParentSectionList.GetTheBitmap(UName, Transparent, Mask, FromCache, Delay);
  if not Assigned(TmpImage) then Exit;
  IsAniGIF := TmpImage is TGifImage;

  if IsAniGIF then
    begin
    if FromCache then   {it would be}
      Image := TGifImage.CreateCopy(TGifImage(TmpImage))  {it's in Cache already, make copy}
    else
      Image := TmpImage;
    ParentSectionList.AGifList.Add(Image);
    TGifImage(Image).Animate := True;
    if Assigned(ParentSectionList.Timer) then   
      ParentSectionList.Timer.Enabled := True;
    end
  else Image := TmpImage;

  if not ImageKnown then
    begin      {need to get the dimensions}
    Reformat := True;
    end;
  end;
end;

{----------------TImageObj.DrawLogic}
procedure TImageObj.DrawLogic(SectionList: TSectionList; Canvas: TCanvas;
              FO: TFontObj; AvailableWidth: integer);
{calculate the height and width}
var
  TmpImage: TPersistent;
  ImHeight, ImWidth: integer;
  ViewImages, FromCache, Delay: boolean;
  AltWidth, AltHeight: integer;
  Rslt: string;

begin
ParentSectionList := SectionList;
ViewImages := SectionList.ShowImages;
Delay := False;

TmpImage := Image;
if ViewImages and not Assigned(TmpImage) then
  begin
  if Source <> '' then
    with SectionList do
      begin
      if not Assigned(GetImage) then
        Source := (TheOwner as ThtmlLite).HTMLExpandFilename(Source)
      else if Assigned(ExpandName) then   
        begin
        ExpandName(TheOwner, Source, Rslt);
        Source := Rslt;
        end;
      if MissingImages.IndexOf(Uppercase(Source)) = -1 then
        TmpImage := ParentSectionList.GetTheBitmap(Source, Transparent, Mask, FromCache, Delay)
      else Delay := True;  {already in list, don't request it again}  
      end;
  if not Assigned(TmpImage) then
    begin
    if Delay then  
      begin
      Image := DefBitmap;
      TmpImage := DefBitmap;
      ParentSectionList.MissingImages.AddObject(Source, Self); {add it even if it's there already}  
      end
    else
      begin
      Image := ErrorBitmap;
      TmpImage := ErrorBitmap;
      Mask := ErrorBitmapMask;
      Transparent := LLCorner;
      end;
    end
  else if TmpImage is TGifImage then
    begin
    if FromCache then
      begin  {it's in Cache already, make copy}
      Image := TGifImage.CreateCopy(TGifImage(TmpImage));
      TmpImage := Image;
      end
    else
      Image := TmpImage;
    if not FHoverImage then    
      ParentSectionList.AGifList.Add(Image)
    else TGifImage(Image).Animate := False;
    end
  else Image := TBitmap(TmpImage);
  end;
if not ViewImages then
  TmpImage := DefBitMap;

if TmpImage is TGifImage then
  begin
  ImHeight := TGifImage(TmpImage).Height;
  ImWidth := TGifImage(TmpImage).Width;
  end
else
  begin
  ImHeight := TBitmap(TmpImage).Height;
  ImWidth := TBitmap(TmpImage).Width;
  end;

if not ImageKnown then
  if not ((Image = ErrorBitmap) or (TmpImage = DefBitmap)) then
    begin
    if PercentWidth then
      begin
      ObjWidth := MulDiv(AvailableWidth, SpecWidth, 100);
      if SpecHeight <> 0 then ObjHeight := SpecHeight
        else ObjHeight := ImHeight;
      end
    else if (SpecWidth <> 0) and (SpecHeight <> 0) then
      begin       {Both width and height specified}
      ObjHeight := SpecHeight;
      ObjWidth := SpecWidth;
      ImageKnown := True;
      end
    else if SpecHeight <> 0 then
      begin
      ObjHeight := SpecHeight;
      ObjWidth := MulDiv(SpecHeight, ImWidth, ImHeight);
      ImageKnown := True;
      end
    else if SpecWidth <> 0 then
      begin
      ObjWidth := SpecWidth;
      ObjHeight := MulDiv(SpecWidth, ImHeight, ImWidth);
      ImageKnown := True;
      end
    else
      begin       {neither height and width specified}
      ObjHeight := ImHeight;
      ObjWidth := ImWidth;
      ImageKnown := True;
      end;
    end
  else {don't know the image yet}
    if (SpecHeight <> 0) and (SpecWidth <> 0) then
      begin       {Both width and height specified}
      ObjHeight := SpecHeight;
      ObjWidth := SpecWidth;
      ImageKnown := True;   {do know the image size}
      end
    else
      begin       {neither height and width specified}
      ObjHeight := ImHeight;
      ObjWidth := ImWidth;
      end;

if (not ViewImages or (TmpImage = ErrorBitmap) or (Image = DefBitmap))
       and Not ImageKnown then
  begin
  Canvas.Font.Name := 'Arial';{use same font as in Draw}
  Canvas.Font.Size := 8;      {should be option?}
  if Alt <> '' then
    begin
    AltWidth := Canvas.TextWidth(Alt) + 2;
    AltHeight := Canvas.TextHeight(Alt);
    end
  else
    begin
    AltHeight := 0;
    AltWidth := 0;
    end;
  ObjWidth := IntMax(ObjWidth, 16+8 + AltWidth);
  ObjHeight := IntMax(ObjHeight, IntMax(16+8, AltHeight));
  end;

ImageHeight := ObjHeight;
ImageWidth := ObjWidth;

HasBlueBox := (FO.URLTarget.Url <> '') and not NoBorder;

if HasBlueBox then
  begin
  Inc(ImageHeight, 2);      {extra pixel top and bottom for rectangle}
  Inc(ImageWidth, 2);
  end;
end;

procedure TImageObj.Draw(Canvas: TCanvas; X: integer; TopY, YBaseline: integer;
                                 FO: TFontObj);
var
  TmpImage: TPersistent;
  TmpMask: TBitmap;
  MiddleAlignTop: integer;
  ViewImages: boolean;
  SubstImage: boolean;
  Ofst: integer;
  SaveColor: TColor;

  procedure DoDraw(XX: integer; Y: integer);
  var
    DC: HDC;
    Img: TBitmap;

    function PrintTransparentBitmap(Bitmap, Mask: TBitmap): HBitmap;
    var
      DC, MemDC: HDC;
      OldPal: HPalette;
      TmpBitmap: HBitmap;
    begin
    DC := GetDC(0);
    MemDC := CreateCompatibleDC(DC);
    try
      Result := CreateCompatibleBitmap(DC, Bitmap.Width, Bitmap.Height);
      TmpBitmap := SelectObject(MemDC, Result);
      OldPal := SelectPalette(MemDC, ThePalette, False);
      RealizePalette(MemDC);
      BitBlt(MemDC, 0, 0, Bitmap.Width, Bitmap.Height, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
      BitBlt(MemDC, 0, 0, Bitmap.Width, Bitmap.Height, Mask.Canvas.Handle, 0, 0, SRCPAINT);
      SelectObject(MemDC, TmpBitmap);
      SelectPalette(MemDC, OldPal, False);
    finally
      DeleteDC(MemDC);
      ReleaseDC(0, DC); 
      end;
    end;

  begin
  if (TmpImage is TGifImage) then
    with TGifImage(TmpImage) do
      begin
      ShowIt := True;
      Visible := True;
      Draw(Canvas, ParentSectionList, MyCell, XX, Y, ObjWidth, ObjHeight);
      Exit;
      end;
  DC := Canvas.Handle;
  try
    if ((Transparent <> NotTransp) or (TmpImage = ErrorBitmap)) and Assigned(TmpMask) then
      if TmpImage = ErrorBitmap then
        FinishTransparentBitmap(DC, TBitmap(TmpImage), Mask, XX, Y,
                    TBitmap(TmpImage).Width, TBitmap(TmpImage).Height)
      else
        FinishTransparentBitmap(DC, TBitmap(TmpImage), Mask, XX, Y, ObjWidth, ObjHeight)
    else
      begin
      Img := TBitmap(TmpImage);
      if (TmpImage = DefBitMap) or (TmpImage = ErrorBitmap) then
        BitBlt(DC, XX, Y, Img.Width, Img.Height, Img.Canvas.Handle, 0, 0, SRCCOPY)
      else
        begin
        SetStretchBltMode(DC, ColorOnColor);
        StretchBlt(DC, XX, Y, ObjWidth, ObjHeight, Img.Canvas.Handle, 0, 0, Img.Width, Img.Height, SRCCOPY);
        end;
      end;
  except
    end;
  end;

begin
with ParentSectionList do
  begin
  ViewImages := ShowImages;
  Dec(TopY, YOff);
  Dec(YBaseLine, YOff);
  end;
if ViewImages then
  begin
  TmpImage := Image;
  if Image is TBitmap then
    TmpMask := Mask;
  end
else
  begin
  TmpImage := DefBitMap;
  TmpMask := Nil;
  end;
SubstImage := not ViewImages or (TmpImage = ErrorBitmap) or (TmpImage = DefBitmap); {substitute image}

with Canvas do
  begin
  Brush.Style := bsClear;
  SaveColor := Font.Color;
  Font.Color := clBlack;  {else transparent won't work for blue text}
  Font.Size := 8;
  Font.Name := 'Arial';        {make this a property?}
  if SubstImage then Ofst := 4 else Ofst := 0;
  if ObjAlign = AMiddle then
    MiddleAlignTop := YBaseLine+FO.Descent-(FO.tmHeight div 2)-(ImageHeight div 2)
  else MiddleAlignTop := 0;   {not used}
  
  DrawX := X;
  case ObjAlign of      
      ATop: DrawYY := TopY;   
      ALeft, ARight: DrawYY := TopY+VSpace;   
      AMiddle: DrawYY := MiddleAlignTop;
      ABottom: DrawYY := YBaseLine-ImageHeight;
      end;
  if HasBlueBox then
    begin
    Inc(DrawX, 1);
    Inc(DrawYY, 1);
    end;

  if not SubstImage or (ObjHeight >= 16+8) and (ObjWidth >= 16+8) then
    DoDraw(DrawX+Ofst, DrawYY+Ofst);
  Inc(DrawYY, ParentSectionList.YOff);  
  SetTextAlign(Canvas.Handle, TA_Top);
  if SubstImage and not HasBlueBox then
    begin
    Font.Color := SaveColor;
    {calc the offset from the image's base to the alt= text baseline}
    case ObjAlign of
      ATop, ALeft, ARight:
        begin
        if Alt <> '' then
          WrapText(Canvas, X+24, TopY+Ofst+VSpace, X+ObjWidth-2, TopY+ObjHeight-1+VSpace, Alt);
        RaisedRect(ParentSectionList, Canvas, X, TopY+VSpace,    
                                      X+ObjWidth-1, TopY+ObjHeight-1+VSpace, False);
        end;
      AMiddle:
        begin   {MiddleAlignTop is always initialized}
        if Alt <> '' then
          WrapText(Canvas, X+24, MiddleAlignTop+Ofst, X+ObjWidth-2,
              MiddleAlignTop+ObjHeight-1, Alt);
        RaisedRect(ParentSectionList, Canvas, X, MiddleAlignTop,
                         X+ObjWidth-1, MiddleAlignTop+ObjHeight-1, False);
        end;
      ABottom:
        begin
        if Alt <> '' then
          WrapText(Canvas, X+24, YBaseLine-ObjHeight+Ofst, X+ObjWidth-2,
                   YBaseLine-1, Alt);
        RaisedRect(ParentSectionList, Canvas, X, YBaseLine-ObjHeight,
                                      X+ObjWidth-1, YBaseLine-1, False);
        end;
      end;
    end;
  if HasBlueBox then
    begin
    Pen.Color := FO.TheFont.Color;
    Font.Color := Pen.Color;
    if (Alt <> '') and SubstImage then  {output Alt message}
      case ObjAlign of
        ATop, ALeft, ARight:
          WrapText(Canvas, X+24, TopY+Ofst, X+ObjWidth-2, TopY+ObjHeight-1, Alt);
        AMiddle:
          WrapText(Canvas, X+24, MiddleAlignTop+Ofst, X+ObjWidth-2,
              MiddleAlignTop+ObjHeight-1, Alt);
        ABottom:
          WrapText(Canvas, X+24, YBaseLine-ObjHeight+Ofst, X+ObjWidth-2,
                   YBaseLine-1, Alt);
        end;
    case ObjAlign of   {draw blue box}
      ATop: Rectangle(X, TopY, X+ImageWidth, TopY+ImageHeight);  
      ALeft, ARight: Rectangle(X, TopY+VSpace, X+ImageWidth, TopY+VSpace+ImageHeight);  
      AMiddle: Rectangle(X, MiddleAlignTop, X+ImageWidth, MiddleAlignTop + ImageHeight);
      ABottom: Rectangle(X, YBaseLine-ImageHeight, X+ImageWidth, YBaseLine);
      end;
    end;
  end;
end;

function TImageObjList.FindImage(Posn: integer): TFloatingObj;
{find the image at a given character position}
var
  I: integer;
begin
for I := 0 to Count-1 do
  if TFloatingObj(Items[I]).Pos = Posn then  
    begin
    Result := TFloatingObj(Items[I]);
    Exit;
    end;
Result := Nil;
end;

function TImageObjList.GetHeightAt(Posn: integer; var AAlign: AlignmentType) : Integer;
var
  Img: TFloatingObj;  
begin
Img := FindImage(Posn);
if Assigned(Img) then
  begin
  Result := Img.ImageHeight;
  AAlign := Img.ObjAlign;
  end
else Result := -1;
end;

function TImageObjList.GetWidthAt(Posn: integer; var AAlign: AlignmentType; var HSpc: integer) : integer;
var
  Img: TFloatingObj;  
begin
Img := FindImage(Posn);
if Assigned(Img) then
  begin
  Result := Img.ImageWidth;
  AAlign := Img.ObjAlign;
  HSpc := Img.HSpace;    
  end
else Result := -1;
end;

function TImageObjList.GetImageCountAt(Posn: integer): integer;
{Return count of chars before the next image.  0 if at the image, 9999 if no
 images after Posn}
var
  I, Pos: integer;
begin
if Count = 0 then
  begin
  Result := 9999;
  Exit;
  end;
I := 0;
while I < count do
  begin
  Pos := TFloatingObj(Items[I]).Pos;
  if Pos >= Posn then break;
  Inc(I);
  end;
if I = Count then Result := 9999
else
  Result := TFloatingObj(Items[I]).Pos - Posn;
end;

function TImageObjList.PtInImage(X: integer; Y: integer; var IX, IY, Posn: integer;
                var AMap, UMap: boolean; var MapItem: TMapItem;
                var ImageObj: TImageObj): boolean;
var
  I, J, LimX, LimY: integer;
  LIY: integer;
  Obj: TObject;   
begin
Result := False;
for I := 0 to Count-1 do
  begin
  Obj := TObject(Items[I]);
  if Obj is TImageObj then
    with TImageObj(Obj) do
      begin
      IX := X-DrawX;    {these are actual image, box if any is outside}
      LIY := Y - DrawYY;
      if HasBlueBox then begin LimX := ImageWidth-2; Limy := ImageHeight-2; end
        else begin LimX := ImageWidth; Limy := ImageHeight; end;
      if (IX >= 0) and (IX < LimX) and (LIY >= 0) and (LIY < LimY) then
        begin
        IY := LIY;
        Result := True;
        AMap := IsMap;
        Posn := Pos;
        UMap := False;
        ImageObj := TImageObj(Obj);   
        if UseMap then
          with ParentSectionList.MapList do
            for J := 0 to Count-1 do
              begin
              MapItem := TMapItem(Items[J]);
              if MapItem.MapName = MapName then
                begin
                UMap := True;
                Exit;
                end;
              end;
        Exit;
        end;
      end;
  end;
end;

function TImageObjList.PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean;
var
  I, LimX, LimY: integer;
  LIY: integer;
  Item: TObject;   
begin
Result := False;
for I := 0 to Count-1 do
  begin
  Item := TImageObj(Items[I]); 
  if Item is TImageObj then
    with TImageObj(Item) do
      begin
      IX := X-DrawX;    {these are actual image, box if any is outside}
      LIY := Y - DrawYY;
      if HasBlueBox then begin LimX := ImageWidth-2; Limy := ImageHeight-2; end
        else begin LimX := ImageWidth; Limy := ImageHeight; end;
      if (IX >= 0) and (IX < LimX) and (LIY >= 0) and (LIY < LimY) then
        begin
        IY := LIY;
        Result := True;
        Obj := Item;  
        Exit;
        end;
      end;
  end;
end;

{----------------ThtmlForm.Create}
constructor ThtmlForm.Create(AMasterList: TSectionList; L : TAttributeList);
var
  I: integer;
begin
inherited Create;
MasterList := AMasterList;
AMasterList.htmlFormList.Add(Self);
Method := 'Get';
if Assigned(L) then
  for I := 0 to L.Count-1 do
    with TAttribute(L[I]) do
      case Which of
        MethodSy: Method := Name;
        ActionSy: Action := Name;
        TargetSy: Target := Name;
        EncTypeSy: EncType := Name;
        end;
ControlList := TFreeList.Create;
end;

destructor ThtmlForm.Destroy;
begin
ControlList.Free;
inherited Destroy;
end;

procedure ThtmlForm.InsertControl(Ctrl: TFormControlObj);
begin
ControlList.Add(Ctrl);
if not (Ctrl is THiddenFormControlObj) then Inc(NonHiddenCount);
end;

procedure ThtmlForm.DoRadios(Radio: TRadioButtonFormControlObj);
var
  S: string;
  Ctrl: TFormControlObj;
  I: integer;
begin
if Radio.Name <>'' then
  begin
  S := Radio.Name;
  for I := 0 to ControlList.Count-1 do
    begin
    Ctrl := TFormControlObj(ControlList.Items[I]);
    if  (Ctrl is TRadioButtonFormControlObj) and (Ctrl <> Radio) then
      if CompareText(Ctrl.Name, S) = 0 then
        TRadioButtonFormControlObj(Ctrl).RButton.Checked := False;
    end;
  end;
end;

procedure ThtmlForm.ResetControls;
var
  I: integer;
begin
for I := 0 to ControlList.Count-1 do
  TFormControlObj(ControlList.Items[I]).ResetToValue;
end;

procedure ThtmlForm.ControlKeyDown(Sender: TObject; var Key: Word;
              Shift: TShiftState);
begin
if (Sender is TEdit) then
  if (Key = VK_RETURN) then   
    SubmitTheForm('');
end;

procedure ThtmlForm.SubmitTheForm(const ButtonSubmission: string);
var
  I, J: integer;
  SL: TStringList;
  S: string;
begin
if Assigned(MasterList.SubmitForm) then
  begin
  SL := TStringList.Create;
  for I := 0 to ControlList.Count-1 do
    with  TFormControlObj(ControlList.Items[I]) do
      begin
      J := 0;
      while GetSubmission(J, S) do
        begin
        if S <> '' then
          SL.Add(S);
        Inc(J);
        end;
      end;
  if ButtonSubmission <> '' then
    SL.Add(ButtonSubmission);
  MasterList.SubmitForm(MasterList.TheOwner, Action, Target, EncType, Method, SL);
  end;
end;

procedure ThtmlForm.SetSizes(Canvas: TCanvas);
var
  I: integer;
begin
for I := 0 to ControlList.Count-1 do
  TFormControlObj(ControlList.Items[I]).SetHeightWidth(Canvas);
end;

{----------------TFormControlObj.Create}
constructor TFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList);
var
  T: TAttribute;
begin
inherited Create;
Pos := Position;
MasterList := AMasterList;
with (MasterList.Parser as ThlParser) do
  begin
  if not Assigned(CurrentForm) then   {maybe someone forgot the <form> tag}
    CurrentForm := ThtmlForm.Create(AMasterList, Nil);
  AMasterList.FormControlList.Add(Self);
  MyForm := CurrentForm;
  end;
if L.Find(ValueSy, T) then
  Value := T.Name;
if L.Find(NameSy, T) then
  Name := T.Name;
if L.Find(OnClickSy, T) then  
  OnClickMessage := T.Name;
MyForm.InsertControl(Self);
end;

destructor TFormControlObj.Destroy;
begin
if Assigned(FControl) then {hidden controls are Nil}
  begin
  TPaintPanel(MasterList.PPanel).RemoveControl(FControl);
  FControl.Free;
  end;
inherited Destroy;
end;

procedure TFormControlObj.EnterEvent(Sender: TObject);
{Once form control entered, insure all form controls are tab active}
var
  I: integer;
begin
MasterList.ControlEnterEvent(Self);
with MasterList.FormControlList do
  begin
  for I := 0 to Count-1 do
    with  TFormControlObj(Items[I]) do
      if not ShowIt and Assigned(FControl) then
        begin
        FControl.Show;   {makes it tab active}
        FControl.Left := -4000; {even if it can't be seen}
        end;
  end;
end;

procedure TFormControlObj.ExitEvent(Sender: TObject);
begin
MasterList.AdjustFormControls;
end;

function TFormControlObj.GetControl: TWinControl;
begin
Result := FControl;
end;

procedure TFormControlObj.ResetToValue;
begin end;

function TFormControlObj.GetSubmission(Index: integer; var S: string): boolean;
begin
Result := False;
end;

procedure TFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
if Assigned(FControl) and not Assigned(FControl.Parent) then
  FControl.Parent := TPaintPanel(MasterList.PPanel);
end;

procedure TFormControlObj.FormControlClick(Sender: TObject);  
begin
if Assigned(MasterList.ObjectClick) then
  MasterList.ObjectClick(MasterList.TheOwner, Self, OnClickMessage);
end;

constructor TImageFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList);
begin
inherited Create(AMasterList, Position, L);
XPos := -1;   {so a button press won't submit image data}
end;

procedure TImageFormControlObj.ImageClick;
begin
FormControlClick(Self);   
XPos := XTmp; YPos := YTmp;
MyForm.SubmitTheForm('');
end;

function TImageFormControlObj.GetSubmission(Index: integer; var S: string): boolean;
begin
Result := False;
if (Index <= 1) and (XPos >= 0) then
  begin
  S := '';
  if Name <> '' then S := Name+'.';
  if Index = 0 then S := S+'x='+IntToStr(XPos)
  else
    begin  {index = 1}
    S := S+'y='+IntToStr(YPos);
    XPos := -1;
    end;
  Result := True;
  end;
end;

{----------------THiddenFormControlObj.GetSubmission}
function THiddenFormControlObj.GetSubmission(Index: integer; var S: string): boolean;
begin
Result := Index = 0;
if Result then
  S := Name+'='+Value;
end;

{----------------TEditFormControlObj.Create}
constructor TEditFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList; const Typ: string);
var
  T: TAttribute;
  PntPanel: TPaintPanel;
  I: integer;
begin
inherited Create(AMasterList, Position, L);
EditSize := 20;           
if L.Find(SizeSy, T) then     
  begin
  if T.Value > 0 then EditSize := T.Value
  else
    begin    {see if it's comma delimited list}
    I := IntMin(System.Pos(',', T.Name), System.Pos(' ', T.Name));
    if I > 1 then EditSize := StrToIntDef(copy(T.Name, 1, I-1), 20);
    end;
  end;
PntPanel := TPaintPanel(AMasterList.PPanel);
FControl := TEdit.Create(PntPanel);
with TEdit(FControl) do
  begin
  Top := -400;   {so will be invisible until placed}
  Width := 120;
  Height := 20;
  Text := Value;
  Font.Name := AMasterList.PreFontName;
  Font.Size := 10;
  if L.Find(MaxLengthSy, T) then
    MaxLength := T.Value;
  if Typ = 'password' then
    PassWordChar := '*';
  OnKeyDown := {$IFDEF HL_LAZARUS}@{$ENDIF}MyForm.ControlKeyDown;
  OnEnter := {$IFDEF HL_LAZARUS}@{$ENDIF}EnterEvent;
  OnExit := {$IFDEF HL_LAZARUS}@{$ENDIF}ExitEvent;
  OnClick := {$IFDEF HL_LAZARUS}@{$ENDIF}FormControlClick;
  end;
end;

procedure TEditFormControlObj.ResetToValue;
begin
TEdit(FControl).Text := Value;
end;

function TEditFormControlObj.GetSubmission(Index: integer; var S: string): boolean;
begin
if Index = 0 then
  begin
  Result := True;
  S := Name+'='+TEdit(FControl).Text;
  end
else Result := False;
end;

procedure TEditFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
if not Assigned(FControl.Parent) then
  FControl.Parent := TPaintPanel(MasterList.PPanel);  
with TEdit(FControl) do
  begin
  Canvas.Font := Font;
  Width := Canvas.TextWidth('A')*EditSize+5;
  end;
end;

{----------------TButtonFormControlObj.Create}
constructor TButtonFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList; const Typ: string);
var
  PntPanel: TPaintPanel;
begin
inherited Create(AMasterList, Position, L);
if Typ = 'submit' then
  begin
  Which := Submit;
  if Value = '' then
    Value := 'Submit';
  end
else if Typ = 'reset' then
  begin
  Which := ResetB;
  if Value = '' then
    Value := 'Reset';
  end
else
  begin
  Which := Button;
  if Value = '' then
    Value := 'Button';
  end;
PntPanel := TPaintPanel(AMasterList.PPanel);
FControl := TButton.Create(PntPanel);
with TButton(FControl) do
  begin
  Top := -400;   {so will be invisible until placed}
  OnClick := {$IFDEF HL_LAZARUS}@{$ENDIF}ButtonClick;
  Caption := Value;
  OnEnter := {$IFDEF HL_LAZARUS}@{$ENDIF}EnterEvent;
  OnExit := {$IFDEF HL_LAZARUS}@{$ENDIF}ExitEvent;
  end;
end;

procedure TButtonFormControlObj.ButtonClick(Sender: TObject);
var
  S: string;
begin
FormControlClick(Self);   
if Which = ResetB then
  MyForm.ResetControls
else if Which = Submit then
  if Name = '' then
    MyForm.SubmitTheForm('')
  else
    begin
    S := Name;
    MyForm.SubmitTheForm(S+'='+Value);
    end;       
end;

procedure TButtonFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
if Assigned(FControl) and not Assigned(FControl.Parent) then
  FControl.Parent := TPaintPanel(MasterList.PPanel);
with TButton(FControl) do
  begin
  Canvas.Font := Font;
  Height := Canvas.TextHeight('A')+8;
  Width := Canvas.TextWidth(Caption)+20;
  end;
end;

{----------------TCheckBoxFormControlObj.Create}
constructor TCheckBoxFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList);
var
  T: TAttribute;
  PntPanel: TPaintPanel;
begin
inherited Create(AMasterList, Position, L);
if Value = '' then Value := 'on';
BaseLine := True;  {sits on text baseline}
if L.Find(CheckedSy, T) then IsChecked := True;
PntPanel := TPaintPanel(AMasterList.PPanel);
FControl := TCheckBox.Create(PntPanel);
with TCheckBox(FControl) do
  begin
  Top := -400;   {so will be invisible until placed}
  Width := 13;
  Height := 13;
  Checked := IsChecked;
  OnClick := {$IFDEF HL_LAZARUS}@{$ENDIF}FormControlClick;
  OnEnter := {$IFDEF HL_LAZARUS}@{$ENDIF}EnterEvent;
  OnExit := {$IFDEF HL_LAZARUS}@{$ENDIF}ExitEvent;
  end;
end;

procedure TCheckBoxFormControlObj.ResetToValue;
begin
TCheckBox(FControl).Checked := IsChecked;
end;

function TCheckBoxFormControlObj.GetSubmission(Index: integer; var S: string): boolean;
begin
if (Index = 0) and TCheckBox(FControl).Checked then
  begin
  Result := True;
  S := Name+'='+Value;
  end
else Result := False;
end;

constructor TRadioButtonFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList; ACell: TCell);
var
  T: TAttribute;
  PntPanel: TPaintPanel;
begin
inherited Create(AMasterList, Position, L);
MyCell := ACell;
PntPanel := TPaintPanel(AMasterList.PPanel);
FControl := TPanel.Create(PntPanel);
BaseLine := True;  {sits on text baseline}
if L.Find(CheckedSy, T) then IsChecked := True;
{Use a TPanel to isolate RadioButton action}
with TPanel(FControl) do
  begin
  Top := -400;   {so will be invisible until placed}
  Width := 13;
  Height := 14;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  ParentColor := False;
  end;
RButton := TRadioButton.Create(FControl);
RButton.Checked := IsChecked;
FControl.InsertControl(RButton);
RButton.OnClick := {$IFDEF HL_LAZARUS}@{$ENDIF}RadioClick;
RButton.OnEnter := {$IFDEF HL_LAZARUS}@{$ENDIF}EnterEvent;
RButton.OnExit := {$IFDEF HL_LAZARUS}@{$ENDIF}ExitEvent;
end;

function TRadioButtonFormControlObj.GetControl: TWinControl;
begin
Result := RButton;
end;

procedure TRadioButtonFormControlObj.RadioClick(Sender: TObject);
begin
MyForm.DoRadios(Self);
FormControlClick(Self);    
end;

procedure TRadioButtonFormControlObj.ResetToValue;
begin
RButton.Checked := IsChecked;
end;

function TRadioButtonFormControlObj.GetSubmission(Index: integer;
             var S: string): boolean;
begin
if (Index = 0) and RButton.Checked then
  begin
  Result := True;
  S := Name+'='+Value;
  end
else Result := False;
end;

{----------------TCell.Create}
constructor TCell.Create(Master: TSectionList);
begin
inherited Create;
MasterList := Master;
IMgr := IndentManager.Create;
end;

destructor TCell.Destroy;
begin
IMgr.Free;
inherited Destroy;
end;

{----------------TCell.Add}
procedure TCell.Add(Item: TSectionBase);
begin
if Assigned(Item) then
  begin
  inherited Add(Item);
  if (Item is TSection) then   
    TSection(Item).Finish;
  Item.SetParent(MasterList);
  end;
end;

{----------------TCell.UpdateFonts}
procedure TCell.UpdateFonts;
var
  I: integer;
begin
for I := 0 to Count-1 do
  TSectionBase(Items[I]).UpdateFonts;
end;

{----------------TCell.FindSectionAtPosition}
function TCell.FindSectionAtPosition(Pos: integer;
         var TopPos: integer; var Index: integer): TSectionBase;
{Find the section which contains the Y Position, Pos.  Return also the position
 of the top of that section and the index of that section}
var
  I: integer;
  H, Delta: integer;
begin
H := 0;
for I := 0 to Count-1 do
  begin
  Delta := TSectionBase(Items[I]).SectionHeight;
  Inc(H, Delta);
  if H > Pos then
    begin
    TopPos := H-Delta;
    Result := TSectionBase(Items[I]);
    Index := I;
    Exit;
    end;
  end;
Result := Nil;
end;

{----------------TCell.GetURL}
function TCell.GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj): boolean;
{Y is absolute}
var
  I: integer;
  H: integer;
begin
Result := False;
FormControl := Nil;
UrlTarg := Nil;
H := 0;
for I := 0 to Count-1 do
  with TSectionBase(Items[I]) do
    begin
    if (Y >= H) and (Y < H+DrawHeight) then
        begin
        Result := GetURL(Canvas, X, Y-H, UrlTarg, FormControl);
        if Result then
          Exit;
        end;
    Inc(H, SectionHeight);
    end;
end;

{----------------TCell.PtInObject}
function TCell.PtInObject(X: integer; Y: integer;  var Obj: TObject;  
              var IX, IY: integer): boolean;
{Y is absolute}
var
  I: integer;
  H: integer;
begin
Result := False;
Obj := Nil;
H := 0;
for I := 0 to Count-1 do
  with TSectionBase(Items[I]) do
    begin
    if (Y >= H) and (Y < H+DrawHeight) then
        begin
        Result := PtInObject(X, Y-H, Obj, IX, IY);
        if Result then
          Exit;
        end;
    Inc(H, SectionHeight);
    end;
end;

{----------------TCell.FindCursor}
function TCell.FindCursor(Canvas: TCanvas; X: Integer; Y: integer;
                var XR: integer; var YR: integer; var Ht: integer;
                var SCell: TObject; var Intext: boolean): integer;
{Y, YR is absolute}
var
  Dummy: integer;
  H: integer;
  S: TSectionBase;
begin
S := FindSectionAtPosition(Y, H, Dummy);
if Assigned(S) then
  begin
  Result := S.FindCursor(Canvas, X, Y-H, XR, YR, Ht, SCell, InText);
  Inc(YR, H);
  end
else Result := -1;
if (Result >= 0) and not Assigned(SCell) then SCell := Self;
end;

{----------------TCell.FindString}
function TCell.FindString(From: integer; PC: PChar; MatchCase: boolean): integer;
var
  I: integer;
begin
Result := -1;
for I := 0 to Count-1 do
  begin
  Result := TSectionBase(Items[I]).FindString(From, PC, MatchCase);
  if Result >= 0 then
    Break;
  end;
end;

{----------------TCell.FindSourcePos}
function TCell.FindSourcePos(DocPos: integer): integer;
var
  I: integer;
begin
Result := -1;
for I := 0 to Count-1 do
  begin
  Result := TSectionBase(Items[I]).FindSourcePos(DocPos);
  if Result >= 0 then
    Break;
  end;
end;

{----------------TCell.FindDocPos}
function TCell.FindDocPos(SourcePos: integer; Prev: boolean): integer;  
var
  I: integer;
begin
Result := -1;
if not Prev then
  for I := 0 to Count-1 do
    begin
    Result := TSectionBase(Items[I]).FindDocPos(SourcePos, Prev);
    if Result >= 0 then
      Break;
    end
else  //Prev, iterate backwards
  for I := Count-1 downto 0 do
    begin
    Result := TSectionBase(Items[I]).FindDocPos(SourcePos, Prev);
    if Result >= 0 then
      Break;
    end
end;

{----------------TCell.CursorToXY}
function TCell.CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean;
var
  I: integer;
begin
Result := False;
for I := 0 to Count-1 do
  begin
  Result := TSectionBase(Items[I]).CursorToXY(Canvas, Cursor, X, Y);
  if Result then Break;
  end;
end;

{----------------TCell.GetChAtPos}   
function TCell.GetChAtPos(Pos: integer; var Ch: char; var Obj: TObject): boolean;
var
  I: integer;
begin
Result := False;
if (Pos >= StartCurs) and (Pos <= StartCurs+Len) then
  for I := 0 to Count-1 do
    begin
    Result := TSectionBase(Items[I]).GetChAtPos(Pos, Ch, Obj);
    if Result then Break;
    end;
end;

{----------------TCell.CopyToClipboard}
procedure TCell.CopyToClipboard;
var
  I: integer;                    
  SLE, SLB: integer;
begin
if not Assigned(MasterList) then Exit;  {dummy cell}
SLB := MasterList.SelB;
SLE := MasterList.SelE;
if SLE <= SLB then Exit;   {nothing to do}

for I := 0 to Count-1 do
  with TSectionBase(Items[I]) do
    begin
    if (SLB >= StartCurs + Len) then Continue;
    if (SLE <= StartCurs) then Break;
    CopyToClipboard;
    end;
end;

{----------------TCell.DoLogic}
function TCell.DoLogic(Canvas: TCanvas; Y: integer; Width: integer;
                 var ScrollWidth: integer; var Curs: integer;
                 StartY,  StartCount: integer): integer;
{Do the entire layout of the cell or document.  Return the total document
 pixel height}
var
  I, Sw, TheCount: integer;
  H, IB: integer;
begin
  IMgr.Clear;
  IMgr.Reset(0, Width);
  IMgr.Width := Width;
  YValue := Y;
  StartCurs := Curs;
  H := StartY;
  TheCount := Count;
  I := StartCount;
  while I < TheCount do
    begin
    try
      H := TSectionBase(Items[I]).DrawLogic(Canvas, Y+H, IMgr, Sw, Curs)+ H;
      ScrollWidth := IntMax(ScrollWidth, Sw);
      Inc(I);
    except
      on E:EProcessError do
        begin
        MessageDlg(E.Message, mtError, [mbOK], 0);
        TSectionBase(Items[I]).Free;
        Delete(I);
        Dec(TheCount);
        end;
      end;
    end;
  Len := Curs - StartCurs;
  Result := H;
  IB := IMgr.ImageBottom - YValue;   {check for image overhang}
  if IB > Result then
    Result := IB;
 end;

{----------------TCell.MinMaxWidth}
procedure TCell.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
{Find the Width the cell would take if no wordwrap, Max, and the width if wrapped
 at largest word, Min}
var
  I, Mn, Mx: integer;
begin
Max := 0; Min := 0;
for I := 0 to Count-1 do
  begin
  TSectionBase(Items[I]).MinMaxWidth(Canvas, Mn, Mx);
  Max := IntMax(Max, Mx);
  Min := IntMax(Min, Mn);
  end;
end;

{----------------TCell.Draw}
function TCell.Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: integer;
                            Y: integer): integer;
{draw the document or cell.  Note: individual sections not in ARect don't bother
 drawing}
var
  I: integer;
  H: integer;
begin
  IMgr.Reset(X, X+IMgr.Width);
  IMgr.ClipWidth := ClipWidth;
  H := Y;
  for I := 0 to Count-1 do
    begin
    H := TSectionBase(Items[I]).Draw(Canvas, ARect, IMgr, X, H);
    end;
  Result := H;
end;

{----------------TSectionList}
constructor TSectionList.Create(Owner, APaintPanel: TWinControl);
begin
inherited Create(Self);
TheOwner := Owner;
PPanel := APaintPanel;
htmlFormList := TFreeList.Create;
AGifList := TList.Create;
MapList := TFreeList.Create;
FormControlList := TList.Create;
MissingImages := TStringList.Create;   
MissingImages.Sorted := False;
LinkList := TList.Create;
UnLine := [fsUnderline];
end;

destructor TSectionList.Destroy;
begin
Clear;
htmlFormList.Free;
MapList.Free;
AGifList.Free;
Timer.Free;
FormControlList.Free;
MissingImages.Free;
LinkList.Free;
inherited Destroy;
end;

function TSectionList.GetURL(Canvas: TCanvas; X: integer; Y: integer;
             var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj): boolean;
var
  OldLink: TFontObj;
  OldImage: TImageObj;
begin
OldLink := ActiveLink;
OldImage := ActiveImage;
ActiveLink := Nil;
ActiveImage := Nil;
Result := inherited GetUrl(Canvas, X, Y, UrlTarg, FormControl);
if LinksActive and (ActiveLink <> OldLink) then
  begin
  if OldLink <> Nil then OldLink.Hover := False;
  if ActiveLink <> Nil then ActiveLink.Hover := True;
  PPanel.Invalidate;
  end;
if (ActiveImage <> OldImage) then
  begin        
  if OldImage <> Nil then OldImage.Hover := False;
  if ActiveImage <> Nil then ActiveImage.Hover := True;
  PPanel.Invalidate;
  end;
end;

procedure TSectionList.CancelActives;
begin
if Assigned(ActiveLink) or Assigned(ActiveImage) then
  PPanel.Invalidate;
if Assigned(ActiveLink) then
  begin
  ActiveLink.Hover := False;
  ActiveLink := Nil;
  end;
if Assigned(ActiveImage) then
  begin
  ActiveImage.Hover := False;
  ActiveImage := Nil;
  end;
end;

procedure TSectionList.CheckGIFList(Sender: TObject);
var
  I: integer;
begin
for I := 0 to AGifList.Count-1 do
    with TGifImage(AGifList.Items[I]) do
      if ShowIt then
        begin
        CheckTime(PPanel);
        end;
Timer.Interval := 50;
end;

procedure TSectionList.SetYOffset(Y: integer);
var
  I, J: integer;
begin
if Y <> YOff then
  begin
  YOff := Y;
  YOffChange := True;
  {After next Draw, hide all formcontrols that aren't to be shown}
  for I := 0 to htmlFormList.Count-1 do
    with ThtmlForm(htmlFormList.Items[I]) do
      for J := 0 to ControlList.Count-1 do
        with  TFormControlObj(ControlList.Items[J]) do
          ShowIt := False;
  end;
end;

procedure TSectionList.Clear;
begin
BackgroundBitmap := Nil;
BackgroundMask := Nil;  
if BitmapLoaded and (BitmapName <> '') then
  BitmapList.DecUsage(BitmapName);
BitmapName := '';
BitmapLoaded := False;
htmlFormList.Clear;
if Assigned(FormControlList) then
  FormControlList.Clear;
AGifList.Clear;
Timer.Free;
Timer := Nil;
SelB := 0;
SelE := 0;
MapList.Clear;
MissingImages.Clear;
if Assigned(LinkList) then
  LinkList.Clear;
ActiveLink := Nil;   
ActiveImage := Nil;
inherited Clear;
end;

{----------------TSectionList.GetSelLength:}
function TSectionList.GetSelLength: integer;
var
  I: integer;
begin
Result := 0;
if SelE <= SelB then Exit;   {nothing to do}
CB := SelTextCount.Create;
try
  for I := 0 to Count-1 do
    with TSectionBase(Items[I]) do
      begin
      if (SelB >= StartCurs + Len) then Continue;
      if (SelE <= StartCurs) then Break;
      CopyToClipboard;
      end;
  Result := CB.Terminate;
finally
  CB.Free;
  end;
end;

{----------------TSectionList.GetSelTextBuf}
function TSectionList.GetSelTextBuf(Buffer: PChar; BufSize: integer): integer;
var
  I: integer;
begin
if BufSize >= 1 then
  begin
  Buffer[0] := #0;
  Result := 1;
  end
else Result := 0;
if SelE <= SelB then Exit;   {nothing to do}
CB := SelTextBuf.Create(Buffer, BufSize);  
try
  for I := 0 to Count-1 do
    with TSectionBase(Items[I]) do
      begin
      if (SelB >= StartCurs + Len) then Continue;
      if (SelE <= StartCurs) then Break;
      CopyToClipboard;
      end;
  Result := CB.Terminate;
finally
  CB.Free;
  end;
end;

{----------------TSectionList.DoLogic}
function TSectionList.DoLogic(Canvas: TCanvas; Y: integer; Width: integer;
              var ScrollWidth: integer; var Curs: integer;
                 StartY,  StartCount: integer): integer;
var
  I: integer;
begin
if Assigned(Timer) then Timer.Enabled := False;
for I := 0 to htmlFormList.Count-1 do
  ThtmlForm(htmlFormList.Items[I]).SetSizes(Canvas);

Result := inherited DoLogic(Canvas, Y, Width, ScrollWidth, Curs, StartY, StartCount);

for I := 0 to AGifList.Count-1 do
  with TGifImage(AGifList.Items[I]) do
    begin
    CurrentFrame := 1;   {required for dtDoNothing and background}
    Animate := False;    {starts iteration count from 1}
    Animate := True;
    end;
if not Assigned(Timer) then
  begin
  Timer := TTimer.Create(TheOwner as ThtmlLite);
  Timer.Interval := 50;
  Timer.OnTimer := {$IFDEF HL_LAZARUS}@{$ENDIF}CheckGIFList;
  end;
if Assigned(Timer) then Timer.Enabled := AGifList.Count >= 1;
AdjustFormControls;
end;

procedure TSectionList.AdjustFormControls;
var
  I: integer;

  function ActiveInList: boolean; {see if active control is a form control}
  var
    Control: TWinControl;
    I: integer;
  begin
  with FormControlList do
    begin
    Result := False;
    Control := Screen.ActiveControl;
    for I := 0 to Count-1 do
      with  TFormControlObj(Items[I]) do
        if FControl = Control then
          begin
          Result := True;
          Break;
          end;
    end;
  end;

begin
if (FormControlList.Count = 0) then Exit;
with FormControlList do
  if not ActiveInList then
    begin  {if none of the formcontrols are active, turn off tabs for those off screen}
    for I := 0 to Count-1 do
      with  TFormControlObj(Items[I]) do
        if not ShowIt and Assigned(FControl) then
          FControl.Hide;   {hides and turns off tabs}
    end
  else
    for I := 0 to Count-1 do
      with  TFormControlObj(Items[I]) do
        if not ShowIt and Assigned(FControl) then
          begin
          FControl.Show;   {turns on tabs}
          FControl.Left := -4000;  {but it still can't be seen}
          end;
end;

{----------------TSectionList.Draw}
function TSectionList.Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: integer;
                          Y:integer): integer;
var
  OldPal: HPalette;
  I: integer;
begin
PageBottom := ARect.Bottom + YOff;
if Assigned(Timer) then Timer.Enabled := False;
for I := 0 to AGifList.Count-1 do
  with TGifImage(AGifList.Items[I]) do
    begin
    ShowIt := False;
    end;
OldPal := SelectPalette(Canvas.Handle, ThePalette, True);
RealizePalette(Canvas.Handle);
try
  Result := inherited Draw(Canvas, ARect, ClipWidth, X, Y);
finally
  SelectPalette(Canvas.Handle, OldPal, True);
  end;
if YOffChange then
  begin
  AdjustFormControls;
  YOffChange := False;
  end;
if Assigned(Timer) then Timer.Enabled := AGifList.Count >= 1;
end;

procedure TSectionList.SetFonts(const Name, PreName: String; ASize: integer;
          AColor, AHotSpot, AVisitedColor, AActiveColor, ABackground: TColor;
          LnksActive: boolean);
begin
FontName := Name;
PreFontName := PreName;
FontSize := ASize;
FontColor := AColor;
HotSpotColor := AHotSpot;
LinkVisitedColor := AVisitedColor;
LinkActiveColor := AActiveColor;
LinksActive := LnksActive;
SetBackground(ABackground);
UpdateFonts;
end;

procedure TSectionList.SetBackground(ABackground: TColor);
begin
Background := ABackground;
if Assigned(OnBackGroundChange) then
  OnBackgroundChange(Self);
end;

procedure TSectionList.SetBackgroundBitmap(Name: String);
begin
BackgroundBitmap := Nil;
BitmapName := Name;
BitmapLoaded := False;
end;

{----------------TSectionList.InsertImage}
procedure TSectionList.InsertImage(const Src: string; Stream: TMemoryStream;
               var Reformat: boolean); 
var
  UName: string;
  I, J: integer;
  Pair: TBitmapItem;
  NonAnimated, Rformat: boolean;
  Image: TPersistent;
  AMask: TBitmap;
  Tr, Transparent: Transparency;
  Obj: TObject;
  Tmp: TGifImage;
begin
Image := Nil;  AMask := Nil;
Reformat := False;
UName := Trim(Uppercase(Src));
I := BitmapList.IndexOf(UName);  {first see if the bitmap is already loaded}
J := MissingImages.IndexOf(UName); {see if it's in missing image list}
if (I = -1) and (J >= 0) then
  begin
  Transparent := NotTransp;    
  if Assigned(Stream) and (Stream.Memory <> Nil) and (Stream.Size >= 1) then
    begin
    NonAnimated := True;
    if KindOfImage(Stream.Memory) in [GIF, Gif89] then
      Image := CreateAGifFromStream(NonAnimated, Stream);
    if Assigned(Image) then
      begin
      if NonAnimated then
        begin     {else already have animated GIF}
        Tmp := TGifImage(Image);
        Image := TBitmap.Create;
        Image.Assign(Tmp.MaskedBitmap);
        if Tmp.IsTransparent then
          begin
          AMask := TBitmap.Create;
          AMask.Assign(Tmp.Mask);
          Transparent := TGif;
          end;
        Tmp.Free;
        end;
      end
    else
      Image := GetImageAndMaskFromStream(Stream, Transparent, AMask);
    end;
  if Assigned(Image) then  {put in Cache}
    try
      if Assigned(AMask) then Tr := Transparent
        else Tr := NotTransp;
      Pair := TBitmapItem.Create(Image, AMask, Tr);
      try
        BitmapList.AddObject(UName, Pair);  {put new bitmap in list}
        BitmapList.DecUsage(UName);    {this does not count as being used yet}
      except
        Pair.Mask := Nil;
        Pair.MImage:= Nil;
        Pair.Free;
      end;
    except  {accept inability to create}
    end;
  end;
if (I >= 0) or Assigned(Image) then  {a valid image in the Cache}
  begin
  while J >= 0 do
    begin
    Obj := MissingImages.Objects[J];
    if (Obj = Self) then
      BitmapLoaded := False  {the background image, set to load}
    else if (Obj is TImageObj) then
      begin
      TImageObj(Obj).InsertImage(UName, Rformat);
      Reformat := Reformat or Rformat;
      end;
    MissingImages.Delete(J);
    J := MissingImages.IndexOf(UName);
    end;
  end;
end;

{----------------TSectionList.GetTheBitmap}
function TSectionList.GetTheBitmap(const BMName: String; var Transparent: Transparency;
         var AMask: TBitmap; var FromCache, Delay: boolean): TPersistent;
{Note: bitmaps and Mask returned by this routine are on "loan".  Do not destroy
 them}
{Transparent may be set to NotTransp or LLCorner on entry but may discover it's
 TGif here}

{$ifdef ShareWare}
const
  OneTime: boolean = False;
{$endif}

var
  UName: string;
  Ext: string[10];
  I: integer;
  Pair: TBitmapItem;
  Tr: Transparency;
  NonAnimated: boolean;
  Stream: TMemoryStream;
  Tmp: TGifImage;

begin
{$ifdef ShareWare}
{$Include DemoVers.inc}
{$endif}
AMask := Nil;
Delay := False;
FromCache := False;
if BMName <> '' then
  begin
  UName := Trim(Uppercase(BMName));
  I := BitmapList.IndexOf(UName);  {first see if the bitmap is already loaded}
  if I > -1 then
    begin         {yes, handle the case where the image is already loaded}
    Result := BitmapList.GetImage(I);
    FromCache := True;
    if Result is TBitmap then
      with  BitmapList.Objects[I] as TBitmapItem do
        begin
        if Transp = TGif then
          Transparent := TGif   {it's a transparent GIF}
        else if Transp = Tpng then
          Transparent := TPng
        else if Transparent = LLCorner then
          begin
          if not Assigned (Mask) then  {1st bitmap may not have been marked transp}
            Mask := GetImageMask(TBitmap(MImage), False, 0);
          if Assigned(Mask) then Transp := LLCorner;
          end;
        AMask := Mask;
        end;
    Exit;
    end;

  {The image is not loaded yet, need to get it}
  Result := Nil;
  if Assigned(GetImage) then
    begin    {the OnImageRequest}
    Stream := Nil;
    GetImage(TheOwner, BMName, Stream);
    if Stream = WaitStream then
      Delay := True
    else if Assigned(Stream) and (Stream.Memory <> Nil) and (Stream.Size >= 1) then
      begin
      NonAnimated := True;
      if KindOfImage(Stream.Memory) in [GIF, Gif89] then
        Result := CreateAGifFromStream(NonAnimated, Stream);
      if Assigned(Result) then
        begin
        if NonAnimated then
          begin     {else already have animated GIF}
          Tmp := TGifImage(Result);
          Result := TBitmap.Create;
          Result.Assign(Tmp.MaskedBitmap);
          if Tmp.IsTransparent then
            begin
            AMask := TBitmap.Create;
            AMask.Assign(Tmp.Mask);
            Transparent := TGif;
            end
          else if Transparent = LLCorner then
            AMask := GetImageMask(TBitmap(Result), False, 0);
          Tmp.Free;
          end;
        end
      else
        Result := GetImageAndMaskFromStream(Stream, Transparent, AMask);
      end;
    end
  else
    begin   {look for the image file}
    Ext := ExtractFileExt(BMName);
    NonAnimated := True;
    if (CompareText(Ext, '.gif')=0) then   {remove .gfr check}  
      Result := CreateAGif(BMName, NonAnimated);
      if Assigned(Result) then
        begin
        if NonAnimated then
          begin     {else already have animated GIF}
          Tmp := TGifImage(Result);
          Result := TBitmap.Create;
          Result.Assign(Tmp.MaskedBitmap);
          if Tmp.IsTransparent then
            begin
            AMask := TBitmap.Create;
            AMask.Assign(Tmp.Mask);
            Transparent := TGif;
            end
          else if Transparent = LLCorner then
            AMask := GetImageMask(TBitmap(Result), False, 0);
          Tmp.Free;
          end;
        end
      else
        Result := GetImageAndMaskFromFile(BMName, Transparent, AMask);
    end;
  if Assigned(Result) then  {put in Image List for use later also}
    try
      if Assigned(AMask) then Tr := Transparent
        else Tr := NotTransp;
      Pair := TBitmapItem.Create(Result, AMask, Tr);
      try
        BitmapList.AddObject(UName, Pair);  {put new bitmap in list}
      except
        Pair.Mask := Nil;
        Pair.MImage:= Nil;
        Pair.Free;
      end;
    except  {accept inability to create}
    end;
  end
else Result := Nil;
end;

function TSectionList.FindPositionByIndex(Index: integer): integer;
{given a section index, find the vertical pixel distance to that section}
var
  I: integer;
begin
Result := 0;
for I := 0 to IntMin(Index-1, Count-2) do
  Result := TSectionBase(Items[I]).SectionHeight+ Result;
end;

function TSectionList.GetBackgroundBitmap: TBitmap;
var
  Mask: TBitmap;   
  Dummy1: Transparency;
  TmpResult: TPersistent;
  FromCache, Delay: boolean;
  Rslt: string;
begin
if ShowImages and not BitmapLoaded and (BitmapName <> '') then
  begin
  if not Assigned(BackgroundBitmap) then
    begin
    Dummy1 := NotTransp;
    if not Assigned(GetImage) then
      BitmapName := (TheOwner as ThtmlLite).HTMLExpandFilename(BitmapName)
    else if Assigned(ExpandName) then
      begin
      ExpandName(TheOwner, BitmapName, Rslt);
      BitmapName := Rslt;
      end;
    TmpResult := GetTheBitmap(BitmapName, Dummy1, Mask, FromCache, Delay); {might be Nil}
    if TmpResult is TBitmap then
      begin
      BackgroundBitmap := TBitmap(TmpResult);
      BackgroundMask := Mask;   
      end
    else
      begin
      BackgroundBitmap := Nil;
      if Delay then
        MissingImages.AddObject(BitmapName, Self);
      end;
    BitmapLoaded := True;   
    end;
  end;
Result := BackgroundBitmap;
end;

{----------------TCellObj.Create}
constructor TCellObj.Create(Master: TSectionList; AVAlign: AlignmentType;
             Attr: TAttributeList);
var
  I: integer;    
begin
inherited Create;
Cell := TCell.Create(Master);
ColSpan := 1;
RowSpan := 1;
VAlign := AVAlign;
if Assigned(Attr) then
  for I := 0 to Attr.Count-1 do
    with TAttribute(Attr[I]) do
      case Which of
        ColSpanSy:
          if Value > 1 then ColSpan := Value;
        RowSpanSy:
          if Value > 1 then RowSpan := Value;
        WidthSy:
          if Pos('%', Name) > 0 then
            begin
            if (Value > 0) and (Value <= 100) then
              begin
              WidthAttr := Value*10;    
              AsPercent := True;
              end;
            end
          else if (Value > 0) then   
            WidthAttr := Value;
        HeightSy: SpecHt := Value;   
        BGColorSy:
          Cell.BkGnd := GetColor(Name, Cell.BkColor);
        end;
end;

destructor TCellObj.Destroy;
begin
Cell.Free;
inherited Destroy;
end;

procedure TCellObj.UpdateFonts;
begin
Cell.UpdateFonts;
end;

{----------------TSectionBase.Create}
constructor TSectionBase.Create(AMasterList: TSectionList);
begin
inherited Create;
ParentSectionList := AMasterList;
end;

procedure TSectionBase.CopyToClipboard;
begin
end;

{----------------TSectionBase.DrawLogic}
function TSectionBase.DrawLogic(Canvas : TCanvas; Y: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer;
begin
StartCurs := Curs;
Result := SectionHeight;
DrawHeight := SectionHeight;
MaxWidth := IMgr.Width;
end;

function TSectionBase.Draw(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X: integer; Y: integer) : integer;
begin
YValue := Y;
Result := Y+SectionHeight;
end;

function TSectionBase.GetURL(Canvas: TCanvas; X: integer; Y: integer;
     var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj): boolean;
begin
Result := False;
end;

function TSectionBase.PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean;
begin
Result := False;
end;

function TSectionBase.FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var SCell: TObject; var Intext: boolean): integer;
begin
Result := -1;
end;

function TSectionBase.FindString(From: integer; PC: PChar; MatchCase: boolean): integer;
begin
Result := -1;
end;

function TSectionBase.FindSourcePos(DocPos: integer): integer;
begin
Result := -1;
end;

function TSectionBase.FindDocPos(SourcePos: integer; Prev: boolean): integer; 
begin
Result := -1;
end;

function TSectionBase.CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean;
begin
Result := False;
end;

function TSectionBase.GetChAtPos(Pos: integer; var Ch: char; var Obj: TObject): boolean;
begin
Result := False;
end;

procedure TSectionBase.UpdateFonts;
begin
UpdateSpacing;
end;

procedure TSectionBase.UpdateSpacing;
begin
end;

procedure TSectionBase.SetParent(List: TSectionList);
begin
ParentSectionList := List;
UpdateSpacing;
end;

procedure TSectionBase.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
begin
Min := 0;  Max := 0;
end;

{----------------TCellList.DoAttributes}
procedure TCellList.DoAttributes(Attr: TAttributeList);
var
  I: integer;
begin
for I := 0 to Attr.Count-1 do
  with TAttribute(Attr[I]) do
    if Which = BGColorSy then
      BkGnd := GetColor(Name, BkColor);
end;

{----------------TCellList.InitializeRow}     
procedure TCellList.InitializeRow;
var
  I: integer;
begin
if BkGnd then
  for I := 0 to Count-1 do
    with TCellObj(Items[I]).Cell do     
      if not BkGnd then
        begin
        BkGnd := True;
        BkColor := Self.BkColor;
        end;
end;

{----------------TCellList.UpdateFonts}
procedure TCellList.UpdateFonts;
var
  I: integer;
begin
for I := 0 to Count-1 do
  if Assigned(Items[I]) then   
    TCellObj(Items[I]).UpdateFonts;
end;

{----------------TCellList.DrawLogic1}
function TCellList.DrawLogic1(Canvas : TCanvas; const Widths : IntArray; Span,
           CellPadding, CellSpacing: integer; var More: boolean): integer;
{Find vertical size of each cell, Row height of this row.  But final Y position
 is not known at this time.}
var
  I, J, Dummy: integer;
  DummyCurs, H, TmpSize: integer;
  CellObj: TCellObj;
begin
H := 0;
DummyCurs := 0;
More := False;
for I := 0 to Count-1 do
  begin
  CellObj := TCellObj(Items[I]);
  if Assigned(CellObj) then     
    with CellObj do
      if ColSpan > 0 then  {skip the dummy cells}
        begin
        Wd := 0;
        for J := I to ColSpan+I-1 do
          Inc(Wd, Widths[J]);   {accumulate column widths}
        if Span = RowSpan then
          begin
          VSize := Cell.DoLogic(Canvas, 0, Wd-2*CellPadding-CellSpacing,
                   Dummy, DummyCurs, 0, 0);
          if VSize > SpecHt-2*CellPadding then TmpSize := VSize
            else TmpSize := SpecHt-2*CellPadding;
          if TmpSize > H  then H := TmpSize;
          end
        else if RowSpan > Span then More := True;
        end;
  end;
Result := H;
end;

{----------------TCellList.DrawLogic2}
procedure TCellList.DrawLogic2(Canvas : TCanvas; Y: integer; CellPadding,
          CellSpacing: integer; var Curs: integer);
{Calc Y indents. Set up Y positions of all cells.}
var
  I, FullPad, Dummy: integer;
  Tmp: integer;
  CellObj: TCellObj;  
begin
for I := 0 to Count-1 do
  begin
  CellObj := TCellObj(Items[I]);
  if Assigned(CellObj) then   
    with CellObj do
      if Cell.Count > 0 then
        begin
        FullPad := 2*CellPadding+CellSpacing;
        Tmp := Ht - VSize - FullPad;
        case VAlign of
          ATop: YIndent := 0;
          AMiddle: YIndent := Tmp div 2;
          ABottom: YIndent := Tmp;
          end;
        Cell.DoLogic(Canvas, Y+CellPadding+CellSpacing+YIndent, Wd-FullPad,
                             Dummy, Curs, 0, 0);
        end;
  end;
end;

{----------------TCellList.Draw}
function TCellList.Draw(Canvas: TCanvas; MasterList: TSectionList; const ARect: TRect;
     const Widths : IntArray; X: integer; Y, YOffset: integer; CellPadding,
     CellSpacing : integer; Border: boolean; Rgn: THandle; MyRow: integer) : integer;
var
  I, Padding: integer;
  YO: integer;
  ARgn: THandle;
  CellObj: TCellObj;
  AddOn: integer;   
begin
YO := Y - YOffset;
Result := RowHeight+Y;

if (YO+RowSpanHeight >= ARect.Top) and (YO < ARect.Bottom) then
  for I := 0 to Count-1 do
    begin
    CellObj := TCellObj(Items[I]);
    if Assigned(CellObj) then     
      with CellObj do
        begin
        if (Cell.Count > 0) then
          begin
          Padding := CellPadding+CellSpacing;
          if Cell.BkGnd then
            begin
            Canvas.Brush.Color := Cell.BkColor or $2000000;
            Canvas.FillRect(Rect(X+CellSpacing, IntMax(YO+CellSpacing, TopLim),
                     X+Wd, IntMin(YO+Ht, BotLim)));  
            end;
          Cell.Draw(Canvas, ARect, Wd-Padding-CellPadding, X+Padding,
                        Y+Padding+YIndent);
          if Border then
            begin
            RaisedRect(Cell.MasterList, Canvas, X+CellSpacing-1, YO+CellSpacing-1,
                                          X+Wd, YO+Ht, False);
            end;

          if Rgn <> 0 then
            begin
            if Border then
              AddOn := 1
            else
              AddOn := 0;
            ARgn := CreateRectRgn(X+CellSpacing-AddOn, IntMax(YO+CellSpacing-AddOn, TopLim),
                                  X+Wd+AddOn, IntMin(YO+Ht+AddOn, BotLim));  
            CombineRgn(Rgn, Rgn, ARgn, RGN_DIFF);
            DeleteObject(ARgn);
            end;
          end;
        end;
    X := X + Widths[I];  
    end;
end;

{----------------ThtmlTable.Create}
constructor ThtmlTable.Create(Master: TSectionList;Attr: TAttributeList;
            AJustify: JustifyType; ACell: TCell; ALevel: integer);
var
  I: integer;
begin
inherited Create(Master);
MyCell := ACell;
Level := ALevel;
Rows := TFreeList.Create;
Caption := TCellObj.Create(Master, ATop, Nil);
TopCaption := True;
Justify := AJustify;
CellPadding := 1;
CellSpacing := 2;    
HSpace := ImageSpace;
for I := 0 to Attr.Count-1 do
  with TAttribute(Attr[I]) do
    case Which of
      BorderSy:
        Border := Value > 0;   {Border=0 is no border}
      AlignSy:
        if CompareText(Name, 'CENTER') = 0 then Justify := Centered
        else if CompareText(Name, 'LEFT') = 0 then
          begin
          Justify := Left;
          Float := True;
          end
        else if CompareText(Name, 'RIGHT') = 0 then
          begin
          Justify := Right;
          Float := True;
          end;
      CellSpacingSy:
        if Value >= 0 then CellSpacing := IntMin(Value, 40);
      CellPaddingSy:
        if Value >= 0 then CellPadding := IntMin(Value, 50);
      WidthSy:
        if Pos('%', Name) > 0 then
          begin
          if (Value > 0) and (Value <= 100) then WidthAttr := Value*10;
          AsPercent := True;
          end
        else WidthAttr := Value;
      HeightSy:                      
        if (Pos('%', Name) > 0) and (ACell = Master) then
          begin
          if (Value > 0) and (Value <= 110) then HeightAttr := Value*10;
          HtAsPercent := True;
          end
        else HeightAttr := Value;
      BGColorSy:
        BkGnd := GetColor(Name, BkColor);
      BorderColorSy:
        BdrOn := GetColor(Name, BdrColor);
      HSpaceSy: HSpace := IntMin(40, Abs(Value));   
      VSpaceSy: VSpace := IntMin(200, Abs(Value));   
      end;
if Border then Inc(CellSpacing, 2);   {includes border lines}
if Border then CellSpacing := IntMax(1, CellSpacing);
end;

{----------------ThtmlTable.Destroy}
destructor ThtmlTable.Destroy;
begin
Rows.Free;
Caption.Free;
inherited Destroy;
end;

procedure ThtmlTable.UpdateFonts;
var
  I: integer;
begin
for I := 0 to Rows.Count-1 do
  TCellList(Rows.Items[I]).UpdateFonts;
Caption.UpdateFonts;
end;

{----------------ThtmlTable.AddDummyCells}
procedure ThtmlTable.AddDummyCells;
var
  Cl, Rw, K, RowCount: integer;
  AnyAbsolute: boolean;

  function DummyCell(RSpan: integer): TCellObj;
  begin
  Result := TCellObj.Create(ParentSectionList, ATop, Nil);
  Result.ColSpan := 0;
  Result.RowSpan := RSpan;
  end;

Begin
if not BkGnd and (MyCell.BkGnd) then
  begin    {Transfer any Background colors}
  BkGnd := True;
  BkColor := MyCell.BkColor;
  end;

RowCount := Rows.Count;
if not ListsProcessed then
  begin   {put dummy cells in rows to make up for ColSpan > 1}
  NumCols := 0;
  AnyAbsolute := False;
  for Rw := 0 to RowCount-1 do
    begin
    with TCellList(Rows[Rw]) do
      begin
      InitializeRow;
      for Cl := Count-1 downto 0 do
        with TCellObj(Items[Cl]) do
          begin
          if WidthAttr > 0 then
            begin
            if not AsPercent then AnyAbsolute := True;
            end;
          if Self.BkGnd and not Cell.BkGnd then    {transfer bgcolor to cells}
            begin
            Cell.BkGnd := True;
            Cell.BkColor := Self.BkColor;
            end;
          for K := 1 to ColSpan-1 do
            if RowSpan > 1 then
              TCellList(Rows[Rw]).Insert(Cl+K, DummyCell(RowSpan)) {these could be
                Nil also except they're needed for expansion in the next section}
            else
              TCellList(Rows[Rw]).Insert(Cl+K, Nil);   
          end;
      end;
    NumCols := IntMax(NumCols, TCellList(Rows[Rw]).Count);  {temporary # cols}
    end;

  {Absolute calc only if  some absolute widths entered}
  UseAbsolute := AnyAbsolute;  

  {put dummy cells in cols to make up for RowSpan > 1}
  for Cl := 0 to NumCols-1 do
    for Rw := 0 to RowCount-1 do
      with TCellList(Rows[Rw]) do
        if Count > Cl then
          if Assigned(Items[Cl]) then  
            with TCellObj(Items[Cl]) do
              begin
              RowSpan := IntMin(RowSpan, RowCount-Rw);  {practical limit}
              if RowSpan > 1 then
                for K := Rw+1 to Rw+RowSpan-1 do
                  begin  {insert dummy cells in following rows if RowSpan > 1}
                  while TCellList(Rows[K]).Count < Cl do {add padding if row is short}
                     TCellList(Rows[K]).Add(DummyCell(0));
                  TCellList(Rows[K]).Insert(Cl, DummyCell(0));
                  end;
              end;

  NumCols := 0;  {find the number of columns}     
  for Rw := 0 to RowCount-1 do
    begin
    NumCols := IntMax(NumCols, TCellList(Rows[Rw]).Count);
    end;
  if NumCols > MaxCols then
    Raise EProcessError.Create('Table has too many Columns');

  ListsProcessed := True;
  end;    {if not ListsProcessed}
end;

{----------------ThtmlTable.GetMinMaxAbs}
procedure ThtmlTable.GetMinMaxAbs(Canvas: TCanvas; var TotalMinWidth,
     TotalMaxWidth: integer; var MinWidths, MaxWidths: IntArray);
var
  I, J, Min, Max, N, Span, Addon, D: integer;
  More: boolean;

Begin
FillChar(MinWidths, Sizeof(MinWidths), 0);
FillChar(MaxWidths, Sizeof(MaxWidths), 0);
Addon := 2*CellPadding + CellSpacing;
Span := 1;
More := True;
while More do
  begin
  More := False;
  for J := 0 to Rows.Count-1 do
    with TCellList(Rows[J]) do
      begin
      for I := 0 to Count-1 do
        if Assigned(Items[I]) then    
          with TCellObj(Items[I]) do
            begin
            More := More or (ColSpan > Span); {set if need another iteration}
            if ColSpan = Span then
              begin
              Cell.MinMaxWidth(Canvas, Min, Max);
              Inc(Min, Addon);
              Inc(Max, Addon);
              if Span = 1 then
                begin
                if not AsPercent and (WidthAttr > 0) then
                  begin
                  Min := IntMax(Min, WidthAttr+Addon);
                  Max := IntMax(Min, WidthAttr+Addon);
                  end;
                MinWidths[I] := Intmax(MinWidths[I], Min);
                MaxWidths[I] := Intmax(MaxWidths[I], Max);
                end
              else
                begin
                TotalMinWidth := 0;  TotalMaxWidth := 0;
                for N := I to I+ColSpan-1 do
                  begin   {find the current totals for the span}
                  Inc(TotalMaxWidth, MaxWidths[N]);
                  Inc(TotalMinWidth, MinWidths[N]);
                  end;
                if not AsPercent and (WidthAttr > 0) then
                  begin
                  Min := IntMax(Min, WidthAttr+Addon);
                  Max := IntMax(Min, WidthAttr+Addon);
                  end;
                if (TotalMinWidth < Min) then
                  if TotalMinWidth > 0 then
                    begin
                    D := Min - TotalMinWidth;
                    for N := I to I+ColSpan-1 do  {increase the sub widths to match the span}
                      MinWidths[N] := MinWidths[N]+MulDiv(MinWidths[N], D, TotalMinWidth);
                    end
                  else MinWidths[I] := Min;  {this for multiple empty cols}
                if (TotalMaxWidth < Max) then
                  if TotalMaxWidth > 0 then
                    begin     {increase the sub widths to match the span}
                    D := Max - TotalMaxWidth;
                    for N := I to I+ColSpan-1 do  {increase the sub widths to match the span}
                      MaxWidths[N] := MaxWidths[N]+MulDiv(MaxWidths[N], D, TotalMaxWidth);
                    end
                  else MaxWidths[I] := Max;
                end;
              end;
            end;
      end;
  Inc(Span);
  end;

{Find the total min and max width}
TotalMaxWidth := 0;  TotalMinWidth := 0;
for I := 0 to NumCols-1 do
  begin
  Inc(TotalMaxWidth, MaxWidths[I]);
  Inc(TotalMinWidth, MinWidths[I]);
  end;

end;

{----------------ThtmlTable.GetWidthsAbs}
procedure ThtmlTable.GetWidthsAbs(Canvas: TCanvas; TablWidth: integer;
            Specified: boolean; var MinWidths, MaxWidths: IntArray);
var
  N, D, W, dd, TotalMinWidth, TotalMaxWidth: integer;

Begin
GetMinMaxAbs(Canvas, TotalMinWidth, TotalMaxWidth, MinWidths, MaxWidths);

if TotalMinWidth >=TablWidth then  {use the minimum column widths, table will expand}
  Move(MinWidths, Widths, Sizeof(MinWidths))
else if (TotalMaxWidth <= TablWidth) and not Specified then  
  {use the max column widths, table will be smaller}
  Move(MaxWidths, Widths, Sizeof(MaxWidths))
else  {make table fit}
  begin
  D := TotalMaxWidth - TotalMinWidth;
  W := TablWidth - TotalMinWidth;
  if D > 0 then  {expand only those columns with some slop in them}
    begin
    for  N := 0 to NumCols-1 do
      begin
      dd := MaxWidths[N] - MinWidths[N];  {some dd's may be 0}
      Widths[N] := MinWidths[N] + MulDiv(dd, W, D);
      end;
    end
  else  {no adjustable columns, will have to expand them all}
    for N := 0 to NumCols-1 do
      Widths[N] := MinWidths[N] + MulDiv(MinWidths[N], W, TotalMinWidth);
  end;
end;

{----------------ThtmlTable.GetWidths}
procedure ThtmlTable.GetWidths(Canvas: TCanvas; var TotalMinWidth, TotalMaxWidth: integer;
          var MinWidths, MaxWidths: IntArray; TheWidth: integer);
var
  I, J, Min, Max, N, Span, Addon, Distributable, TotalPC,
  ExcessMin, ExcessMax, NonPC, PCWidth, NewTotalPC, MaxSum: integer;
  More: boolean;

Begin
{Find the max and min widths of each column}
FillChar(MaxWidths, Sizeof(MaxWidths), 0);
FillChar(MinWidths, Sizeof(MinWidths), 0);
FillChar(Percents, Sizeof(Percents), 0);
Addon := 2*CellPadding + CellSpacing;
Span := 1;
More := True;
while More do
  begin
  More := False;
  for J := 0 to Rows.Count-1 do
    with TCellList(Rows[J]) do
      begin
      for I := 0 to Count-1 do
        if Assigned(Items[I]) then  
          with TCellObj(Items[I]) do
            begin
            PCWidth := 0;
            if WidthAttr > 0 then
              if AsPercent then PCWidth := WidthAttr
              else if TheWidth > 0 then
                PCWidth := IntMin(1000, MulDiv(WidthAttr, 1000, TheWidth));
            More := More or (ColSpan > Span); {set if need another iteration}
            if ColSpan = Span then
              begin
              Cell.MinMaxWidth(Canvas, Min, Max);
              Inc(Min, Addon);
              Inc(Max, Addon);
              if Span = 1 then
                begin
                MaxWidths[I] := IntMax(MaxWidths[I],  Max);
                MinWidths[I] := IntMax(MinWidths[I],  Min);
                Percents[I] := Intmax(Percents[I], PCWidth);  {collect percents}
                end
              else
                begin
                TotalMaxWidth := 0;  TotalMinWidth := 0;
                TotalPC := 0;  NonPC := 0;
                for N := I to I+ColSpan-1 do
                  begin   {Total up the pertinant column widths}
                  Inc(TotalMaxWidth, MaxWidths[N]);
                  Inc(TotalMinWidth, MinWidths[N]);
                  if Percents[N] > 0 then
                    Inc(TotalPC, Percents[N])  {total percents}
                  else Inc(NonPC);      {count of cell with no percent}
                  end;
                ExcessMin := Min - TotalMinWidth;
                ExcessMax := Max - TotalMaxWidth;
                if (PCWidth > 0) or (TotalPC > 0) then
                  begin   {manipulate for percentages}
                  if NonPC > 0 then
                    {find the extra percentages to divvy up}
                    Distributable := IntMax(0, (PCWidth-TotalPC) div NonPC)
                  else Distributable := 0;
                  if (NonPC = 0) and (PCWidth > TotalPC) then
                    begin
                    for  N := I to I+ColSpan-1 do  {stretch percentages to fit}
                      Percents[N] := MulDiv(Percents[N], PCWidth, TotalPC);
                    end
                  else if Distributable > 0 then    {spread colspan percentage excess over the unspecified cols}
                    for N := I to I+ColSpan-1 do
                      if Percents[N] = 0 then Percents[N] := Distributable;
                  NewTotalPC := IntMax(TotalPC, PCWidth);
                  if ExcessMin > 0 then
                    begin
                    if NonPC > 0 then  {split excess over non-specified cells}
                      begin
                      {proportion the distribution so cells with large MaxWidth get more}
                      MaxSum := 0;
                      for  N := I to I+ColSpan-1 do
                        if Percents[N] = 0 then
                          Inc(MaxSum, MaxWidths[N]);
                      for  N := I to I+ColSpan-1 do
                        if Percents[N] = 0 then
                          Inc(MinWidths[N], MulDiv(ExcessMin, MaxWidths[N], MaxSum));
                      end
                    else
                      for  N := I to I+ColSpan-1 do
                        MinWidths[N] := IntMax(MulDiv(Min, Percents[N], NewTotalPC), MinWidths[N]);
                    end;
                  if ExcessMax > 0 then
                    begin
                    if NonPC > 0 then  {split excess over non-specified cells}
                      begin
                      Distributable := ExcessMax div NonPC;
                      for  N := I to I+ColSpan-1 do
                        if Percents[N] = 0 then
                          Inc(MaxWidths[N], Distributable);
                      end
                    else
                      for  N := I to I+ColSpan-1 do
                        MaxWidths[N] := IntMax(MulDiv(Max, Percents[N], NewTotalPC), MaxWidths[N]);
                    end;
                  end
                else
                  begin  {no width dimensions entered}
                  if ExcessMin > 0 then
                    for  N := I to I+ColSpan-1 do
                      if TotalMinWidth = 0 then
                        MinWidths[N] := Min div ColSpan
                      else  {split up the widths in proportion to widths already there}
                        MinWidths[N] := MulDiv(Min, MinWidths[N], TotalMinWidth);
                  if ExcessMax > 0 then
                    for  N := I to I+ColSpan-1 do
                      if TotalMaxWidth = 0 then
                        MaxWidths[N] := Max div ColSpan
                      else   {split up the widths in proportion to widths already there}
                        MaxWidths[N] := MulDiv(Max, MaxWidths[N], TotalMaxWidth);
                  end;
                end;
              end;
            end;
      end;
  Inc(Span);
  end;

TotalMaxWidth := 0;  TotalMinWidth := 0;
for I := 0 to NumCols-1 do
  begin
  Inc(TotalMaxWidth, MaxWidths[I]);
  Inc(TotalMinWidth, MinWidths[I]);
  end;
end;

{----------------ThtmlTable.MinMaxWidth}
procedure ThtmlTable.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
var
  MaxWidths, MinWidths: IntArray;
  Mn, Dummy: integer;
begin
AddDummyCells;     {in case it hasn't been done}
if UseAbsolute and (WidthAttr = 0) then
  GetMinMaxAbs(Canvas, Mn, Max, MinWidths, MaxWidths)
else if not AsPercent then
  GetWidths(Canvas, Mn, Max, MinWidths, MaxWidths, WidthAttr)
else
  GetWidths(Canvas, Mn, Max, MinWidths, MaxWidths, 0);

Inc(Mn, CellSpacing);
Inc(Max, CellSpacing);
if not AsPercent then
  begin
  Mn := IntMax(Mn, WidthAttr);
  Max := IntMax(Max, WidthAttr);
  end;
Caption.Cell.MinMaxWidth(Canvas, CaptionMinWidth, Dummy);
Min := IntMax(CaptionMinWidth, Mn);  {caption may be wider than table}
Max := IntMax(CaptionMinWidth, Max);
end;

procedure ThtmlTable.xxx(const MaxWidths, MinWidths: IntArray; TheWidth: integer);
{Divide up the table into columns.  TheWidth is the specified width of the table.
 At this point, it is known that everything will fit into TheWidth. Percents are
 being used}
var
  I, W, PCNotMinWid, TotalWid, Unsp, UnspDiff, Delta, Addon, Count: integer;
  UseMin: array[0..MaxCols] of boolean;
  NoChange: boolean;
begin
FillChar(UseMin, Sizeof(UseMin), False);
PCNotMinWid := 0;  TotalWid := 0;  Unsp := 0; UnspDiff := 0;
{First calculate everything assuming the data entered is perfectly correct}
for I := 0 to NumCols - 1 do
  begin
  if Percents[I] > 0 then
    begin
    W := MulDiv(TheWidth, Percents[I], 1000);  {width based on percentage}
    if W > MinWidths[I] then
      begin
      Widths[I] := W;
      Inc(PCNotMinWid, Percents[I]);
      end
    else
      begin   {percent is too small, use Min width}
      Widths[I] := MinWidths[I];
      UseMin[I] := True;
      end;
    end
  else
    begin    {no percent}
    Widths[I] := MinWidths[I];
    Inc(Unsp);     {an unspecified column}
    Inc(UnspDiff, MaxWidths[I]-MinWidths[I]); {total max-min for unspecified cols}
    end;
  Inc(TotalWid, Widths[I]);
  end;

Delta := TotalWid - TheWidth;    {see what the error is}
if Delta < 0 then     {table is too small}
  begin
  if Unsp > 0 then
    begin
    if (UnspDiff > 0) and (UnspDiff >= Abs(Delta) div 2) then  
      {increase the unspecified columns widths prop to Max, Min unless the difference is trivial}
      begin
      for I := 0 to NumCols-1 do
        if (Percents[I] = 0) then
          Inc(Widths[I], MulDiv(-Delta, MaxWidths[I] - MinWidths[I], UnspDiff));
      end
    else
      begin  {increase the unspecified columns widths uniformly}
      Addon := -Delta div Unsp;
      for I := 0 to NumCols - 1 do
        if (Percents[I] = 0) then
          Inc(Widths[I], Addon);
      end;
    end
  else
    begin            {no unspecified widths, increase the specified columns which are not minimum}
    for I := 0 to NumCols - 1 do
      if (Percents[I] > 0) and not UseMin[I] then
        Inc(Widths[I], MulDiv(-Delta, Percents[I], PCNotMinWid));
    end;
  end
else if Delta > 0 then    {calculated table is too large}
  begin
  Count := 0;
  {make one or more trial run to see what happens when shrinking the columns
   that can be shrunck.  May hit another MinWidth situation}
  repeat
    NoChange := True;
    for I := 0 to NumCols - 1 do
      if (Percents[I] > 0) and not UseMin[I] then
        begin
        W := Widths[I] - MulDiv(Delta, Percents[I], PCNotMinWid);
        if W < MinWidths[I] then
          begin    {new width is smaller than MinWidth, make adustments}
          UseMin[I] := True;
          NoChange := False;
          Dec(PCNotMinWid, Percents[I]);
          Dec(Delta, Widths[I]-MinWidths[I]);
          Widths[I] := MinWidths[I];
          end;
        end;
    Inc(Count);
  until NoChange or (Count >= 4);   {count guards against endless loop}
  for I := 0 to NumCols - 1 do  {now actually change the widths}
    if (Percents[I] > 0) and not UseMin[I] then
      Dec(Widths[I], MulDiv(Delta, Percents[I], PCNotMinWid));
  end;

TotalWid := 0;     {fix up any round off errors}
for I := 0 to NumCols - 1 do
  Inc(TotalWid, Widths[I]);
Delta := TotalWid-TheWidth;     {round off error}
if Delta > 0 then
  begin
  for I := 0 to NumCols-1 do
    if not UseMin[I] then
      begin
      Dec(Widths[I], Delta);   {remove extra from first non minimum}
      Break;
      end;
  end
else Inc(Widths[0], -Delta);   {tack it on anywhere}
end;

{----------------ThtmlTable.DrawLogic}
function ThtmlTable.DrawLogic(Canvas : TCanvas; Y: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer;
Label
  GotWidths;
type
  HeightArray = array[0..16000] of integer;
var
  I, J, K, N, Span,
  TotalMaxWidth, TotalMinWidth, D, W, DS,
  Total, TotalPC, Residual, NewResidual, W1, W2, NewTotal: integer;
  More, Mr, HasPercents, UsesPercents, Done: boolean;
  MaxWidths, MinWidths: IntArray;
  NewWidth, Dummy: integer;
  Heights: ^HeightArray;
  OwnerWidth: integer;
  H, TotalHt, Addon: integer;
  Specified: boolean;     
  AddedOn: integer;
  DisplayHt, NewHeight, Sum: integer;  

Begin
YValue := Y;
StartCurs := Curs;
IMgr.SetLevel(Y, Level);    

OwnerWidth := IMgr.RightSide(Y) - IMgr.LeftIndent(Y);
if WidthAttr > 0 then
  begin
  Specified := True;   
  if AsPercent then
    NewWidth := MulDiv(OwnerWidth, WidthAttr, 1000)
  else NewWidth := WidthAttr;
  end
else
  begin
  Specified := False;    
  NewWidth := OwnerWidth;
  end;
Dec(NewWidth, CellSpacing);
NewWidth := IntMax(NewWidth, 20);

AddDummyCells;

{Figure the width of each column}
if UseAbsolute and not Specified then  
  begin
  GetWidthsAbs(Canvas, NewWidth, Specified, MinWidths, MaxWidths); {fills in Widths array}
  GoTo GotWidths;
  end
else
  GetWidths(Canvas, TotalMinWidth, TotalMaxWidth, MinWidths, MaxWidths, NewWidth); 

if (TotalMinWidth >= NewWidth) then        
  begin   {table won't fit, use minimun widths}
  Move(MinWidths, Widths, Sizeof(IntArray));
  GoTo GotWidths;
  end;

if Specified then
  begin
  xxx(MaxWidths, MinWidths, NewWidth);
  GoTo GotWidths;
  end;

TotalPC := 0;   {see if any percentage widths entered}
for I := 0 to NumCols-1 do
  Inc(TotalPC, Percents[I]);
UsesPercents := (TotalPc > 0) and (TotalPc <= 1000) {ignore ridiculous values}
                 or (WidthAttr > 0);

if UsesPercents then
  begin {find the largest width that will accomodate the %'s}
  Residual := 0; W1 := 0; W2 := 0;
  for I := 0 to NumCols-1 do
    if Percents[I] > 0 then  {a percent has been entered}
      W1 := IntMax(W1, MulDiv(MaxWidths[I], 1000, Percents[I])) {look for maximum}
    else
      Inc(Residual, MaxWidths[I]);  {accumlate the cols which have no percent}
  if TotalPC < 1000 then
    W2 := MulDiv(Residual, 1000, 1000-TotalPC)
  else if Residual > 0 then W2 := 30000
  else W2 := 0;
  Total := IntMax(W1, W2);
  if Total <= NewWidth then
    begin  {a fit is found using percents and maxwidths}
    if WidthAttr > 0 then
      Total := NewWidth;    {don't try to make it smaller than NewWidth}  
    NewResidual := MulDiv(Total, 1000-TotalPC, 1000);
    for I := 0 to NumCols-1 do
      if Percents[I] > 0 then   {figure widths to fit this situation}
        Widths[I] := MulDiv(Total, Percents[I], 1000)
      else if Residual > 0 then
        Widths[I] := MulDiv(MaxWidths[I], NewResidual, Residual)
      else Widths[I] := 0;    {this is an table syntax error condition}
    GoTo GotWidths;
    end;

  Done := False;
  repeat  {with the above possibilites taken care of, we can assume the final
           width will = NewWidth}
    HasPercents := False;
    Total := 0;  Residual := 0;
    for I := 0 to NumCols-1 do
      begin
      if Percents[I] > 0 then
        begin
        W := MulDiv(NewWidth, Percents[I], 1000)-1; {a Percent's width based on NewWidth}
        if W < MinWidths[I] then  {but it must be > MinWidth}
          begin   {eliminate the percentage value as not achievable}
          Percents[I] := 0;
          Inc(Residual, MinWidths[I]);  {and put it in the residuals}
          end
        else
          begin
          HasPercents := True;   {still valid percents}
          Inc(Total, W);
          end;
        end
      else Inc(Residual, MinWidths[I]);
      end;
    if not HasPercents then Break;  {no percents are achievable}
    if Total+Residual <= NewWidth then
      begin  {a solution with at least some percentages can be found}
      Done := True;
      TotalMaxWidth := 0;  TotalMinWidth := 0;  {recalc these}
      for I := 0 to NumCols-1 do
        begin
        if Percents[I] > 0 then
          begin
          MinWidths[I] := MulDiv(NewWidth, Percents[I], 1000);
          MaxWidths[I] := MinWidths[I];  {this fixes the width thru later calculations}
          end;
        Inc(TotalMaxWidth, MaxWidths[I]);
        Inc(TotalMinWidth, MinWidths[I]);
        end;
      end
    else  {it doesn't fit screen, reduce percentages and try again}
      begin
      NewTotal := NewWidth-Residual;  {percent items must fit this}
      for I := 0 to NumCols-1 do
        if Percents[I] > 0 then
          Percents[I] := integer(Percents[I]) * NewTotal div Total;
      end;
  until Done;
  end;

D := TotalMaxWidth - TotalMinWidth;
if (TotalMaxWidth <= NewWidth) or (D = 0) then
  Move(MaxWidths, Widths, Sizeof(IntArray))
else
  begin
  W := NewWidth - TotalMinWidth;
  for I := 0 to NumCols-1 do
    begin
    ds := MaxWidths[I] - MinWidths[I];
    Widths[I] := MinWidths[I] + MulDiv(ds, W, D);
    end;
  end;

GotWidths:

{Find Table Width}
TableWidth := CellSpacing;
for I := 0 to NumCols-1 do
  Inc(TableWidth, Widths[I]);
Caption.Cell.MinMaxWidth(Canvas, CaptionMinWidth, Dummy);
CaptionWidth := IntMax(TableWidth, CaptionMinWidth); {make sure caption fits}

GetMem(Heights, Rows.Count * Sizeof(integer));
try
  {Find the height of each row allowing for RowSpans}
  FillChar(Heights^, Rows.Count*Sizeof(integer), 0);
  Span := 1;
  More := True;
  while More do
    begin
    More := False;
    for J := 0 to Rows.Count-1 do
      with TCellList(Rows[J]) do
        begin
        if J+Span > Rows.Count then Break;  {otherwise will overlap}
        H := DrawLogic1(Canvas, Widths, Span, CellPadding, CellSpacing, Mr) +
               + 2*CellPadding+CellSpacing;
        More := More or Mr;
        if Span = 1 then
          Heights^[J] := H
        else
          begin
          TotalHt := 0;  {sum up the height so far for the rows involved}
          for K := J to J+Span-1 do
            Inc(TotalHt, Heights^[K]);
          if H > TotalHt then   {apportion the excess over the rows}
            begin
            Addon := ((H-TotalHt) div Span);
            AddedOn := 0;
            for K := J to J+Span-1 do
              begin
              Inc(Heights^[K], Addon);
              Inc(AddedOn, Addon);
              end;
            Inc(Heights^[J+Span-1], (H-TotalHt)-AddedOn); {make up for round off error}
            end;
          end;
        end;
    Inc(Span);
    end;

  if TopCaption then
    begin         {layout the caption}
    SectionHeight := Caption.Cell.DoLogic(Canvas, Y, CaptionWidth, Dummy, Curs, 0, 0);
    CaptionHeight := SectionHeight;
    Inc(Y, SectionHeight);
    end
  else SectionHeight := 0;

  if HeightAttr > 0 then   
    begin
    if HtAsPercent then
      with ThtmlLite(ParentSectionList.TheOwner) do
        begin
        DisplayHt := ClientHeight - 2*FMarginHeightX - CellSpacing - 3;
        NewHeight := MulDiv(DisplayHt, HeightAttr, 1000);
        end
    else NewHeight := HeightAttr;
    TotalHt := 0;
    for J := 0 to Rows.Count-1 do
      Inc(TotalHt, Heights^[J]);
    if TotalHt < NewHeight then
      begin
      Addon := (NewHeight-TotalHt) div Rows.Count;
      Sum := 0;
      for J := 0 to Rows.Count-2 do
        begin
        Inc(Heights^[J], Addon);
        Inc(Sum, Heights^[J]);
        end;
      Heights^[Rows.Count-1] := NewHeight-Sum;
      end;
    end;   

  TableHeight := SectionHeight;
  for J := 0 to Rows.Count-1 do
    with  TCellList(Rows[J]) do
      begin
      RowHeight := Heights^[J];
      RowSpanHeight := 0;        
      Inc(SectionHeight, Heights^[J]);
      for I := 0 to Count-1 do
        if Assigned(Items[I]) then  
          with TCellObj(Items[I]) do
            begin   {find the actual height, Ht, of each cell}
            Ht := 0;
            for K := J to J+RowSpan-1 do
              Inc(Ht, Heights^[K]);
            if RowSpanHeight < Ht then RowSpanHeight := Ht;
            end;
      DrawLogic2(Canvas, Y, CellPadding, CellSpacing, Curs);
      Inc(Y, RowHeight);
      end;
  Inc(SectionHeight, CellSpacing);
  TableHeight := SectionHeight-TableHeight;
Finally
  FreeMem(Heights, Rows.Count * Sizeof(integer));
  end;

if not TopCaption then
  begin
  CaptionHeight := Caption.Cell.DoLogic(Canvas, YValue+TableHeight,
                   CaptionWidth, Dummy, Curs, 0, 0);
  Inc(SectionHeight, CaptionHeight);
  end;

{figure the indents, CaptionWidth is = or larger than TableWidth}
CaptionIndent := 0;
if CaptionWidth < OwnerWidth then
case Justify of
  Centered: CaptionIndent := (OwnerWidth-CaptionWidth) div 2;
  Right: CaptionIndent := OwnerWidth-CaptionWidth;
  end;
Inc(CaptionIndent, IMgr.LeftIndent(YValue));     
Indent := CaptionIndent + (CaptionWidth-TableWidth) div 2; {table indent}

Len := Curs-StartCurs;
MaxWidth := CaptionWidth;
if Float then             
  begin
  Inc(SectionHeight, 2*VSpace);   
  IMgr.UpdateTable(YValue, MaxWidth+HSpace+1, SectionHeight, Justify);
  DrawHeight := SectionHeight;
  SectionHeight := 0;
  Result := 0;
  end
else
  begin
  Result := SectionHeight;
  DrawHeight := Result;
  end;
end;

{----------------ThtmlTable.Draw}
function ThtmlTable.Draw(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X: integer; Y: integer) : integer;
var
  I, XX: integer;
  YY, YTable, YO, YOffset: integer;
  Rgn: THandle;
begin
Result := Y+SectionHeight;
if Float then        
  Y := Y + VSpace;
YOffset := ParentSectionList.YOff;
YO := Y - YOffset;

if (YO+DrawHeight >= ARect.Top) and (YO < ARect.Bottom) then
  begin
  XX := X+Indent;   {for the table}
  YY := Y;
  DrawX := XX;
  DrawY := YY;
  if TopCaption then
    YY := Caption.Cell.Draw(Canvas, ARect, CaptionWidth, XX+CaptionIndent-Indent, YY);
  YTable := YY;
  if BdrOn then
    begin
    Rgn:= CreateRectRgn(XX, IntMax(Arect.Top-1, YTable-YOffset),
             XX+TableWidth, IntMin(ARect.Bottom, YTable+TableHeight-YOffset));
    end
  else Rgn := 0;
  for I := 0 to Rows.Count-1 do
    YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
            XX, YY, YOffset, CellPadding, CellSpacing, Border, Rgn, I);
  if Rgn <> 0 then
    begin
    Canvas.Brush.Color := BdrColor or $2000000;
    FillRgn(Canvas.Handle, Rgn, Canvas.Brush.Handle);
    DeleteObject(Rgn);
    end;
  if Border then
    RaisedRect(ParentSectionList, Canvas, XX, YTable-YOffset, XX+TableWidth-1,
                                         YY+CellSpacing-YOffset-1, True);
  if not TopCaption then
    Caption.Cell.Draw(Canvas, ARect, CaptionWidth, XX+CaptionIndent-Indent,
                          YTable+TableHeight);
  end;
end;

{----------------ThtmlTable.GetURL}
function ThtmlTable.GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj): boolean;
{Y is relative to top of section}
var
  CaptionOK, TableOK: boolean;

  function GetTableURL(X: integer; Y: integer): boolean;
  var
    I, J, XX, YY: integer;
  begin
  YY := 0;
  for J := 0 to Rows.Count-1 do
    begin
    XX := DrawX;
    with  TCellList(Rows[J]) do
      begin
      for I := 0 to Count-1 do
        begin
        if Assigned(Items[I]) then     
          with TCellObj(Items[I]) do
            begin
            if (X >=XX) and (X < XX+Wd)
                 and (Y >= YY) and (Y < YY+Ht) then
              begin
              Result := Cell.GetUrl(Canvas, X,
                 Y-YY-(CellSpacing+CellPadding+YIndent), UrlTarg, FormControl);
              Exit;
              end;
            end;
        Inc(XX, Widths[I]); 
        end;
      Inc(YY, RowHeight);
      end;
    end;
  Result := False;
  end;

begin
Result := False;
if (Y <= DrawHeight) then
  begin
  TableOK := (X >= DrawX) and (X <= TableWidth+DrawX);
  CaptionOK := (X >= DrawX+CaptionIndent-Indent) and (X <= DrawX+CaptionWidth+CaptionIndent-Indent);
  if TopCaption then
    if Y < CaptionHeight then
      begin
      if CaptionOK then
        Result := Caption.Cell.GetURL(Canvas, X, Y, UrlTarg, FormControl);
      end
    else
      begin
      if TableOK then
        Result := GetTableURL(X, Y-CaptionHeight);
      end
  else
    if Y < TableHeight then
      begin
      if TableOK then
        Result := GetTableURL(X, Y);
      end
    else
      begin
      if CaptionOK then
        Result := Caption.Cell.GetURL(Canvas, X, Y-TableHeight, UrlTarg, FormControl);
      end;
  end;
end;

function ThtmlTable.PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean;
{Y is relative to top of section}
var
  CaptionOK, TableOK: boolean;

  function GetTableObj(X: integer; Y: integer): boolean;
  var
    I, J, XX, YY: integer;
  begin
  YY := 0;
  for J := 0 to Rows.Count-1 do
    begin
    XX := DrawX;
    with  TCellList(Rows[J]) do
      begin
      for I := 0 to Count-1 do
        begin
        if Assigned(Items[I]) then  
          with TCellObj(Items[I]) do
            begin
            if (X >=XX) and (X < XX+Wd)
                 and (Y >= YY) and (Y < YY+Ht) then
              begin
              Result := Cell.PtInObject(X, Y-YY-(CellSpacing+CellPadding+YIndent),
                           Obj, IX, IY);
              Exit;
              end;
            end;
        Inc(XX, Widths[I]);      
        end;
      Inc(YY, RowHeight);
      end;
    end;
  Result := False;
  end;

begin
Result := False;
if (Y <= DrawHeight) then
  begin
  TableOK := (X >= DrawX) and (X <= TableWidth+DrawX);
  CaptionOK := (X >= DrawX+CaptionIndent-Indent) and (X <= DrawX+CaptionWidth+CaptionIndent-Indent);
  if TopCaption then
    if Y < CaptionHeight then
      begin
      if CaptionOK then
        Result := Caption.Cell.PtInObject(X, Y, Obj, IX, IY);
      end
    else
      begin
      if TableOK then
        Result := GetTableObj(X, Y-CaptionHeight);
      end
  else
    if Y < TableHeight then
      begin
      if TableOK then
        Result := GetTableObj(X, Y);
      end
    else
      begin
      if CaptionOK then
        Result := Caption.Cell.PtInObject(X, Y-TableHeight, Obj, IX, IY);
      end;
  end;
end;

{----------------ThtmlTable.FindCursor}
function ThtmlTable.FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var SCell: TObject; var Intext: boolean): integer;
{Y is relative to top of section}
var
  CaptionOK, TableOK: boolean;

  function GetTableCursor(X: integer; Y: integer; var XR: integer;
           var YR: integer; var CaretHt: integer; var Intext: boolean): integer;
  var
    I, J, XX, YY: integer;
  begin
  YY := 0;
  for J := 0 to Rows.Count-1 do
    begin
    XX := DrawX;
    with  TCellList(Rows[J]) do
      begin
      for I := 0 to Count-1 do
        begin
        if Assigned(Items[I]) then  
          with TCellObj(Items[I]) do
            begin
            if (X >=XX) and (X < XX+Wd)
                 and (Y >= YY) and (Y < YY+Ht) then
              begin
              Result := Cell.FindCursor(Canvas, X,
                 Y-YY-(CellSpacing+CellPadding+YIndent), XR, YR, CaretHt, SCell, InText);
              Inc(YR, YY+(CellSpacing+CellPadding+YIndent));
              Exit;
              end;
            end;
        Inc(XX, Widths[I]);   
        end;
      Inc(YY, RowHeight);
      end;
    end;
  Result := -1;
  end;

begin
Result := -1;
if (Y <= SectionHeight) then
  begin
  TableOK := (X >= DrawX) and (X <= TableWidth+DrawX);
  CaptionOK := (X >= DrawX+CaptionIndent-Indent) and (X <= DrawX+CaptionWidth+CaptionIndent-Indent);
  if TopCaption then
    if Y < CaptionHeight then
      begin
      if CaptionOK then
        begin
        Result := Caption.Cell.FindCursor(Canvas, X, Y, XR, YR, CaretHt, SCell, InText);
        end;
      end
    else
      begin
      if TableOK then
        begin
        Result := GetTableCursor(X, Y-CaptionHeight, XR, YR, CaretHt, InText);
        Inc(YR, CaptionHeight);
        end;
      end
  else
    if Y < TableHeight then
      begin
      if TableOK then
        begin
        Result := GetTableCursor(X, Y, XR, YR, CaretHt, InText);
        end;
      end
    else
      begin
      if CaptionOK then
        begin
        Result := Caption.Cell.FindCursor(Canvas, X, Y-TableHeight,
                  XR, YR, CaretHt, SCell, InText);
        Inc(YR, TableHeight);
        end;
      end;
  end;
end;

function ThtmlTable.CursorToXY(Canvas: TCanvas; Cursor: integer;
                                var X: integer; var Y: integer): boolean;
{note: returned X value is not correct here but it isn't used}
var
  I, J: integer;
begin
Result := False;
if (Len = 0) or (Cursor > StartCurs + Len) then Exit;
if TopCaption then
  begin
  Result := Caption.Cell.CursorToXy(Canvas, Cursor, X, Y);
  if Result then Exit;
  end;
for J := 0 to Rows.Count-1 do
  with  TCellList(Rows[J]) do
    for I := 0 to Count-1 do
      if Assigned(Items[I]) then 
        with TCellObj(Items[I]) do
          begin
          Result := Cell.CursorToXy(Canvas, Cursor, X, Y);
          if Result then Exit;
          end;
if not TopCaption then
  Result := Caption.Cell.CursorToXy(Canvas, Cursor, X, Y);
end;

{----------------ThtmlTable.GetChAtPos}
function ThtmlTable.GetChAtPos(Pos: integer; var Ch: char; var Obj: TObject): boolean;
var
  I, J: integer;
begin
Result := False;
if (Len = 0) or (Pos < StartCurs) or (Pos > StartCurs + Len) then Exit;

Result := Caption.Cell.GetChAtPos(Pos, Ch, Obj);
if Result then Exit;

for J := 0 to Rows.Count-1 do
  with  TCellList(Rows[J]) do
    for I := 0 to Count-1 do
      if Assigned(Items[I]) then 
        with TCellObj(Items[I]) do
          begin
          Result := Cell.GetChAtPos(Pos, Ch, Obj);
          if Result then Exit;
          end;
end;

{----------------ThtmlTable.FindString}
function ThtmlTable.FindString(From: integer; PC: PChar;
                              MatchCase: boolean): integer;
var
  I, J: integer;
begin
Result := -1;
if TopCaption then
  begin
  Result := Caption.Cell.FindString(From, PC, MatchCase);
  if Result >= 0 then Exit;
  end;
for J := 0 to Rows.Count-1 do
    with  TCellList(Rows[J]) do
      for I := 0 to Count-1 do
        if Assigned(Items[I]) then 
          with TCellObj(Items[I]) do
            begin
            Result := Cell.FindString(From, PC, MatchCase);
            if Result >= 0 then Exit;
            end;
if not TopCaption then
  Result := Caption.Cell.FindString(From, PC, MatchCase);
end;

{----------------ThtmlTable.FindSourcePos}
function ThtmlTable.FindSourcePos(DocPos: integer): integer;   
var
  I, J: integer;
begin
Result := -1;
if TopCaption then
  begin
  Result := Caption.Cell.FindSourcePos(DocPos);
  if Result >= 0 then Exit;
  end;
for J := 0 to Rows.Count-1 do
    with  TCellList(Rows[J]) do
      for I := 0 to Count-1 do
        if Assigned(Items[I]) then 
          with TCellObj(Items[I]) do
            begin
            Result := Cell.FindSourcePos(DocPos);
            if Result >= 0 then Exit;
            end;
if not TopCaption then
  Result := Caption.Cell.FindSourcePos(DocPos);
end;

{----------------ThtmlTable.FindDocPos}
function ThtmlTable.FindDocPos(SourcePos: integer; Prev: boolean): integer;   
var
  I, J: integer;
begin
if not Prev then
  begin
  Result := Caption.Cell.FindDocPos(SourcePos, Prev);
  if Result >= 0 then Exit;

  if not Prev then
    for J := 0 to Rows.Count-1 do
        with  TCellList(Rows[J]) do
          for I := 0 to Count-1 do
            if Assigned(Items[I]) then 
              with TCellObj(Items[I]) do
                begin
                Result := Cell.FindDocPos(SourcePos, Prev);
                if Result >= 0 then Exit;
                end;
  end
else  //Prev , iterate in reverse
  begin
  for J := Rows.Count-1 downto 0 do
      with  TCellList(Rows[J]) do
        for I := Count-1 downto 0 do
          if Assigned(Items[I]) then 
            with TCellObj(Items[I]) do
              begin
              Result := Cell.FindDocPos(SourcePos, Prev);
              if Result >= 0 then Exit;
              end;
  Result := Caption.Cell.FindDocPos(SourcePos, Prev);
  end;
end;

{----------------ThtmlTable.CopyToClipboard}
procedure ThtmlTable.CopyToClipboard;
var
  I, J: integer;
begin
if TopCaption then
  Caption.Cell.CopyToClipboard;

for J := 0 to Rows.Count-1 do
    with  TCellList(Rows[J]) do
      for I := 0 to Count-1 do
        if Assigned(Items[I]) then 
          with TCellObj(Items[I]) do
            Cell.CopyToClipboard;
if not TopCaption then
  Caption.Cell.CopyToClipboard;
end;

{----------------TSection.Create}
constructor TSection.Create(AMasterList: TSectionList; ALevel: integer; AFont: TMyFont; AnURL: TUrlTarget;
            AJustify: JustifyType);
var
  FO : TFontObj;
  F: TMyFont;
  Parser: ThlParser;
begin
inherited Create(AMasterList);
Parser := ThlParser(ParentSectionList.Parser);
Buff := Nil;
Len := 0;
BuffSize := 0;
Parser.CurrentSScript := Normal;
Fonts := TFontList.Create;
F := TMyFont.Create;
F.Assign(AFont);
F.Style := F.Style + Parser.CurrentStyle;
FO := TFontObj.Create(Self, F, 0);
if Assigned(AnURL) and (Length(AnURL.Url) > 0) then
  begin
  FO.UrlTarget.Assign(AnUrl.Url, AnUrl.Target);
  ParentSectionList.LinkList.Add(FO);
  end;
Fonts.Add(FO);
DefFont := TMyFont.Create;
DefFont.Assign(F);

Images := TImageObjList.Create;
FormControls := TFormControlList.Create;

Level := ALevel;
Indent := ALevel * ListIndent;
ListType := None;
Lines := TFreeList.Create;
Justify := AJustify;
end;

{----------------TSection.Destroy}
destructor TSection.Destroy;
begin
if Assigned(Buff) then FreeMem(Buff, BuffSize);
if Assigned(XP) then
  FreeMem(XP);       
Fonts.Free;
Images.Free;
FormControls.Free;
SIndexList.Free;    
Lines.Free;
DefFont.Free;
inherited Destroy;
end;

procedure TSection.DoClearAttribute(L: TAttributeList);
var
  T: TAttribute;
  S: string[15];
begin
if L.Find(ClearSy, T) then
  begin
  S := LowerCase(T.Name);
  if (S = 'left') then ClearAttr := clLeft
  else if (S = 'right') then ClearAttr := clRight
  else ClearAttr := clAll;
  end;
end;

{----------------TSection.AddChar}
procedure TSection.AddChar(C: char; Index: integer; NoBreak: boolean);
var
  Tok: TokenObj;
begin
Tok := TokenObj.Create;
Tok.S := C;
Tok.I^[1] := Index;
AddTokenObj(Tok, NoBreak);
Tok.Free;
end;

function TSection.GetIndexObj(I: integer): IndexObj;   
begin
Result := IndexObj(SIndexList[I]);
end;

procedure TSection.Finish;    
{complete some things after all information added}
var
  Last, I: integer;
  IO: IndexObj;
begin
if Len > 0 then
  begin
  Buff[Len] := #0;
  if Assigned(XP) then  {XP = Nil when printing}
    begin
    Last := 0;   {to prevent warning msg}
    SIndexList := TFreeList.Create;
    for I := 0 to Len-1 do
      begin
      if (I = 0) or (XP^[I] <> Last+1) then
        begin
        IO := IndexObj.Create;
        IO.Pos := I;
        IO.Index := XP^[I];
        SIndexList.Add(IO);
        end;
      Last := XP^[I];
      end;
    FreeMem(XP);
    XP := Nil;     
    end;
  end;
end;

{----------------TSection.AddTokenObj}
procedure TSection.AddTokenObj(S : TokenObj; NoBreak: boolean);
var
  L, I : integer;

  Procedure Remove(I: integer);
  begin
  Move(S.I^[I+1], S.I^[I], ((Length(S.S))-I)*Sizeof(integer));
  System.Delete(S.S, I, 1);
  end;
begin
if Length(S.S) = 0 then Exit;
{Delete leading spaces or multiple spaces}
if not NoBreak then
  begin
  if ((Len = 0) or (Buff[Len-1] = ' ')) and (S.S[1] = ' ') then
    begin
    if Length(S.S) = 1 then Exit;
    Remove(1);
    end;
  end
else
  begin
  if ((Len = 0) or (Buff[Len-1] in [#5, #160, ' '])) and (S.S[1] = #5) then
    begin
    if Length(S.S) = 1 then Exit;
    Remove(1)
    end;
  I := Pos(' '#5, S.S);
  while I > 0 do
    begin
    Remove(I+1);
    I := Pos(' '#5, S.S);
    end;
  I := Pos(#5#5, S.S);
  while I > 0 do
    begin
    Remove(I);
    I := Pos(#5#5, S.S);
    end;
  I := Pos(#5' ', S.S);
  while I > 0 do
    begin
    Remove(I);
    I := Pos(#5' ', S.S);
    end;
  I := Pos(#5, S.S);
  while I > 0 do
    begin
    S.S[I] := #160;
    I := Pos(#5, S.S);
    end;
  end;

{After floating images at start, delete an annoying space}
if Len > 0 then
  for I := 0 to Len-1 do
    begin
    if (not (Buff[I] in [#4, #7])) or not (Images.FindImage(I).ObjAlign in [ALeft, ARight]) then 
      Break;
    if (I = Len-1) and (Length(S.S) > 0) and (S.S[1] in [' ', #160]) then  
      begin
      if Length(S.S) = 1 then Exit;
      Remove(1)
      end;
    end;

L := Len+Length(S.S);
if BuffSize < L+1 then Allocate(L + 100);  {L+1 so there is always extra for font at end}
Move(S.S[1], (Buff+Len)^, Length(S.S));
Move(S.I[1], XP^[Len], Length(S.S)*Sizeof(integer));    
Len := L;
end;

function TSection.BreakInfo(Index: integer; NoBreak: boolean): JustifyType;  {called when <br> encountered}
begin
Result := Justify;
if Len = 0 then   {need to have at least one space}
  begin
  AddChar('X', Index, NoBreak);   {fool AddTokenObj into adding a leading space}
  Buff[0] := ' ';
  end;
end;

{----------------TSection.Allocate}
procedure TSection.Allocate(N : integer);  
begin
if BuffSize < N then
  begin
  ReAllocMem(Buff, N);
  ReAllocMem(XP, N*Sizeof(integer));
  BuffSize := N;
  end;
end;

procedure TSection.ChangeFont(List: TSectionList; NewFont: TMyFont);
{will not accommodate a font size change}
var
  F: TMyFont;
  FO: TFontObj;
  LastUrl: TUrlTarget;
begin
FO := TFontObj(Fonts[Fonts.Count-1]);
LastUrl := FO.UrlTarget;
If FO.Pos = Len then
  FO.TheFont.Assign(NewFont) {fontobj already at this position, modify it}
else
  begin
  F := TMyFont.Create;
  F.Assign(NewFont);
  FO := TFontObj.Create(Self, F, Len);
  Fonts.Add(FO);
  if Assigned(LastUrl) then
    FO.URLTarget.Assign(LastUrl.Url, LastUrl.Target);
  end;
with ThlParser(ParentSectionList.Parser) do   
  begin
  with FO.TheFont, (ParentSectionList.Parser as ThlParser) do
    Style := Style + CurrentStyle; {add in <b>, <i>, etc}
  FO.SScript := CurrentSScript;
  if CurrentSScript in [SupSc, SubSc] then
    FO.TheFont.SetNormalSize(List, MulDiv(FO.TheFont.NormalSize, 3, 4));
  end;
end;

procedure TSection.ChangeStyle(Sy: Symb);
var
  Style: TFontStyles;
  F: TMyFont;
  FO: TFontObj;
begin
if Sy in [BSy, BEndSy, ISy, IEndSy, EmSy, EmEndSy, StrongSy, StrongEndSy,
          USy, UEndSy, CiteSy, CiteEndSy, VarSy, VarEndSy] then
  begin
  FO := TFontObj(Fonts[Fonts.Count-1]);
  Style := FO.TheFont.Style;
  case Sy of
    BSy, StrongSy:  Style := Style + [fsBold];
    BEndSy, StrongEndSy:  Style := Style - [fsBold];
    ISy, EmSy, CiteSy, VarSy:  Style := Style + [fsItalic];
    IEndSy, EmEndSy, CiteEndSy, VarEndSy:  Style := Style - [fsItalic];
    USy:  Style := Style + [fsUnderline];
    UEndSy:  Style := Style - [fsUnderline];
    end;
  If FO.Pos = Len then
    FO.TheFont.Style := Style   {fontobj already at this position, modify it}
  else
    begin
    F := TMyFont.Create;
    F.Assign(FO.TheFont);   {just like the last one}
    F.Style := Style;
    FO := TFontObj.Create(Self, F, Len);
    Fonts.Add(FO);
    end;
  end;
end;

procedure TSection.HRef(Sy: Symb; List: TSectionList; AnURL: TUrlTarget;
          AFont: TMyFont);
var
  FO: TFontObj;
begin
ChangeFont(List, AFont);
FO := TFontObj(Fonts[Fonts.Count-1]);
FO.UrlTarget.Clear;
if Sy = HRefSy then
  begin
  FO.UrlTarget.Assign(AnUrl.Url, AnUrl.Target);
  List.LinkList.Add(FO);
  end;
end;

function TSection.AddImage(L: TAttributeList; ACell: TCell; Index: integer; NoBreak: boolean): TImageObj; 
begin
Result := TImageObj.Create(Len, L);
Result.MyCell := ACell;
Images.Add(Result);
if NoBreak then
  AddChar(#7, Index, NoBreak)    {marker for nobreak image}
else
  AddChar(#4, Index, NoBreak);    {marker for image}
end;

{----------------TSection.AddFormControl}
function TSection.AddFormControl(Which: Symb; AMasterList: TSectionList;
         L: TAttributeList; ACell: TCell; Index: integer; NoBreak: boolean): TFormControlObj;
var
  T: TAttribute;
  FCO: TFormControlObj;
  S: string[20];
  IO: TImageObj;

  procedure GetEditFCO;
  begin
  FCO := TEditFormControlObj.Create(AMasterList, Len, L, S);
  end;

begin
S := '';
if Which = InputSy then
  begin
  if L.Find(TypeSy, T) then
    begin
    S := LowerCase(T.Name);
    if (S = 'text') or (S = 'password') then
      GetEditFCO
    else if (S = 'submit') or (S = 'reset') or (S = 'button') then
      FCO := TButtonFormControlObj.Create(AMasterList, Len, L, S)
    else if S = 'radio' then
      FCO := TRadioButtonFormControlObj.Create(AMasterList, Len, L, ACell)
    else if S = 'checkbox' then
      FCO := TCheckBoxFormControlObj.Create(AMasterList, Len, L)
    else if S = 'hidden' then
      FCO := THiddenFormControlObj.Create(AMasterList, Len, L)
    else if S = 'image' then
      FCO := TImageFormControlObj.Create(AMasterList, Len, L)
    else
      GetEditFCO;
    end
  else
    GetEditFCO;
  end
else if Which = SelectSy then
  begin
  if L.Find(MultipleSy, T) or L.Find(SizeSy, T) and (T.Value > 1) then
    FCO := TListBoxFormControlObj.Create(AMasterList, Len, L)
  else
    FCO := TComboFormControlObj.Create(AMasterList, Len, L);
  end
else
  FCO := TTextAreaFormControlObj.Create(AMasterList, Len, L);
if S = 'image' then
  begin
  IO := AddImage(L, ACell, Index, NoBreak);  {leave out of FormControlList} 
  IO.MyFormControl := TImageFormControlObj(FCO);
  end
else if S <> 'hidden' then
  begin
  FormControls.Add(FCO);
  if NoBreak then
    AddChar(#6, Index, NoBreak)    {marker for no break FormControl}
  else
    AddChar(#2, Index, NoBreak);    {marker for FormControl}
  end;
Result := FCO;
end;

{----------------TSection.FindCountThatFits}
function TSection.FindCountThatFits(Canvas: TCanvas; Width : integer;
                Start : PChar; Max : integer) : integer;
{Given a width, find the count of chars (<= Max) which will fit allowing for
 font changes.  Line wrapping will be done later}
var
  Cnt, XX, I, J, J1, J2, J3, OHang, Tmp : integer;
  Picture: boolean;
  Align: AlignmentType;
  HSpc: integer;  

  function Find(Width, Max: integer; Start: PChar): integer;
  {return count <= Max which fits in Width}
  var
    L, H, I, X: integer;
    ExtS: TSize;
  {$ifndef ver120_plus}
    NilP: integer absolute 0;
  {$endif}
  begin
  L := 0; H := Max-1;
  while L <= H do
    begin
    I := (L+H) shr 1;
{$ifdef ver120_plus}
    GetTextExtentExPoint(Canvas.Handle, Start, I+1, 0, Nil, Nil, ExtS);
{$else} {do Nil the hard way for Delphi 3}
    GetTextExtentExPoint(Canvas.Handle, Start, I+1, 0, NilP, NilP, ExtS);
{$endif}
    x := ExtS.cx - OHang;
    if X <= Width then
      L := I+1
    else H := I-1;
    end;
  Result := L;
  end;

begin
Cnt := 0;
XX := 0;
while True do
  begin
  Canvas.Font := Fonts.GetFontAt(Start-Buff, OHang);
  J1 := Fonts.GetFontCountAt(Start-Buff, Len);
  J2 := Images.GetImageCountAt(Start-Buff);
  J3 := TFormControlList(FormControls).GetControlCountAt(Start-Buff);
  if J2 = 0 then
    begin
    Tmp:= Images.GetWidthAt(Start-Buff, Align, HSpc);
    if not (Align in [ALeft, ARight]) then
      XX := XX + Tmp + 2*HSpc;                  
    I := 1;  J := 1;
    Picture := True;
    if XX > Width then break;  
    end
  else if J3 = 0 then
    begin
    XX := XX + TFormControlList(FormControls).GetWidthAt(Start-Buff);
    I := 1;  J := 1;
    Picture := True;
    if XX > Width then break;   
    end
  else
    begin
    Picture := False;
    J := IntMin(J1, J2);
    J := IntMin(J, J3);
    I := Find(Width-XX, J, Start);
    end;
  if Cnt+I >= Max then      {I has been initialized}
    begin
    Cnt := Max;
    Break;
    end
  else Inc(Cnt, I);

  if not Picture then
    begin
    if I < J then Break;
    XX := XX + GetXExtent(Canvas.Handle, Start, I) - OHang;
    end;

  Inc(Start, I);
  end;
Result := Cnt;
end;

{----------------TSection.FindCountThatFits1}
function TSection.FindCountThatFits1(Canvas: TCanvas; Width : integer;
                Start : PChar; Max: integer; Y: integer; IMgr: IndentManager;
                var ImgHt: integer; NxImages: TList) : integer;
{Given a width, find the count of chars (<= Max) which will fit allowing for
 font changes.  Line wrapping will be done later}
var
  Cnt, XX, I, J, J1, J2, J3, OHang, ImgWidth : integer;
  Picture: boolean;
  Align: AlignmentType;
  ImageAtStart: boolean;
  FlObj: TFloatingObj;
  HSpc: integer;     

  function Find(Width, Max: integer; Start: PChar): integer;
  {return count <= Max which fits in Width}
  var
    L, H, I, X: integer;
    ExtS: TSize;
  {$ifndef ver120_plus}
    NilP: integer absolute 0;
  {$endif}
  begin
  L := 0; H := Max-1;
  while L <= H do
    begin
    I := (L+H) shr 1;
{$ifdef ver120_plus}
    GetTextExtentExPoint(Canvas.Handle, Start, I+1, 0, Nil, Nil, ExtS);
{$else} {do Nil the hard way for Delphi 3}
    GetTextExtentExPoint(Canvas.Handle, Start, I+1, 0, NilP, NilP, ExtS);
{$endif}
    x := ExtS.cx - OHang;
    if X <= Width then
      L := I+1
    else H := I-1;
    end;
  Result := L;
  end;

begin
ImageAtStart := True;
ImgHt := 0;
Cnt := 0;
XX := 0;
while True do
  begin
  Canvas.Font := Fonts.GetFontAt(Start-Buff, OHang);
  J1 := Fonts.GetFontCountAt(Start-Buff, Len);
  J2 := Images.GetImageCountAt(Start-Buff);
  J3 := TFormControlList(FormControls).GetControlCountAt(Start-Buff);
  if J2 = 0 then
    begin   {next is an image}
    ImgWidth := Images.GetWidthAt(Start-Buff, Align, HSpc);
    if Align in [ALeft, ARight] then
      begin
      FlObj := Images.FindImage(Start-Buff);
      if ImageAtStart then
        begin
        IMgr.Update(Y, FlObj);
        Inc(XX, ImgWidth + FlObj.HSpace);
        ImgHt := IntMax(ImgHt, FlObj.ImageHeight + 2*FlObj.VSpace);    
        end
      else
        NxImages.Add(FlObj);    {save it for the next line}
      end
    else
      begin
      Inc(XX, ImgWidth+2*HSpc);   
      ImageAtStart := False;
      end;
    I := 1;  J := 1;
    Picture := True;
    if XX > Width then break;
    end
  else if J3 = 0 then
    begin
    XX := XX + TFormControlList(FormControls).GetWidthAt(Start-Buff);
    I := 1;  J := 1;
    Picture := True;
    ImageAtStart := False;
    if XX > Width then break;     
    end
  else
    begin
    Picture := False;
    J := IntMin(J1, J2);
    J := IntMin(J, J3);
    I := Find(Width-XX, J, Start);
    end;
  if Cnt+I >= Max then     {I has been initialized}
    begin
    Cnt := Max;
    Break;
    end
  else Inc(Cnt, I);

  if not Picture then      {Picture has been initialized}
    begin
    if I < J then Break;    {J has been initialized}
    XX := XX + GetXExtent(Canvas.Handle, Start, I) - OHang;
    ImageAtStart := False;
    end;

  Inc(Start, I);
  end;
Result := Cnt;
end;

{----------------TSection.MinMaxWidth}
procedure TSection.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
{Min is the width the section would occupy when wrapped as tightly as possible.
 Max, the width if no wrapping were used.}
var
  I, Indx, FloatMin: integer;
  P, P1: PChar;
  Obj: TObject;  

begin
Min := 0;
Max := 0;
if not Assigned(Buff) then Exit;

for I := 0 to Images.Count-1 do     {call drawlogic for all the images}
  begin
  Obj := TObject(Images[I]);
  if (Obj is TImageObj) then   
    with TImageObj(Obj) do
      begin
      DrawLogic(Self.ParentSectionList, Canvas, Fonts.GetFontObjAt(Pos, Indx), 0);
      if not PercentWidth then
        if ObjAlign in [ALeft, ARight] then
          Max := Max + ImageWidth + HSpace
        else Min := IntMax(Min, ImageWidth);
      end
  else
    with TFloatingObj(Obj) do        
      if ObjAlign in [ALeft, ARight] then
        Max := Max + ImageWidth + HSpace
      else Min := IntMax(Min, ImageWidth);
  end;
FloatMin := Max;
Max := FindTextWidth(Canvas, Buff, Len, False) + Indent + Max;

Buff[Len] := #0;  {always extra space on end}
P := Buff;
while P^ = ' ' do Inc(P);
P1 := P;
while P^ <> #0 do
  begin
  while not (P1^ in [' ', #4, #0]) do Inc(P1);
  Min := IntMax(Min, FindTextWidth(Canvas, P, P1-P, False));
  while (P1^ in [' ', #4]) do Inc(P1);
  P := P1;
  end;
Min := Min + FloatMin + Indent;
end;

{----------------TSection.FindTextWidth}
function TSection.FindTextWidth(Canvas: TCanvas; Start: PChar; N: integer;
                                        RemoveSpaces: boolean): integer;
{find actual line width of N chars starting at Start.  If RemoveSpaces set,
 don't count spaces on right end}
var
  I, J, J1, OHang, Wid, HSpc: integer;
  Align: AlignmentType;
begin
Result := 0;
if RemoveSpaces then
  while ((Start+N-1)^ = ' ') and (N > 1) do
    Dec(N);    {remove spaces on end}
while N > 0 do
  begin
  J := Images.GetImageCountAt(Start-Buff);
  J1 := TFormControlList(FormControls).GetControlCountAt(Start-Buff);
  if J = 0 then  {it's and image}
    begin
    Wid := Images.GetWidthAt(Start-Buff, Align, HSpc);
    {Here we count floating images as 1 char but do not include their width,
      This is required for the call in FindCursor}
    if not (Align in [ALeft, ARight]) then
      begin
      Result := Result + Wid + 2*HSpc;  
      end;
    Dec(N);   {image counts as one char}
    Inc(Start);
    end
  else if J1 = 0 then
    begin
    Result := Result + TFormControlList(FormControls).GetWidthAt(Start-Buff);
    Dec(N);   {control counts as one char}
    Inc(Start);
    end
  else
    begin
    Canvas.Font := Fonts.GetFontAt(Start-Buff, OHang);
    I := IntMin(J, J1);
    I := IntMin(I, IntMin(Fonts.GetFontCountAt(Start-Buff, Len), N));
    Inc(Result, GetXExtent(Canvas.Handle, Start, I) - OHang);
    Dec(N, I);
    Inc(Start, I);
    end;
  end;
end;

{----------------TSection.DrawLogic}
function TSection.DrawLogic(Canvas : TCanvas; Y: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer;
{returns height of the section}
var
  PStart, P, Last : PChar;
  Max, N, Width, I, Indx, ImgHt: integer;
  Finished: boolean;
  LR : LineRec;
  NxImages: TList;
  Tmp: integer;
  Obj: TFloatingObj;

  function GetClearSpace: integer;
  var
    CL, CR: integer;
  begin
  Result := 0;
  if (ClearAttr <> clrNone) then
    begin  {may need to move down past floating image}
    IMgr.GetClearY(CL, CR);
    case ClearAttr of
      clLeft:  Result := IntMax(0, CL-Y-1);
      clRight:  Result := IntMax(0, CR-Y-1);
      clAll: Result := IntMax(CL-Y-1, IntMax(0, CR-Y-1));
      end;
    end;
  end;

  procedure LineComplete(NN : integer);
  var
    I, J, DHt, Desc, Tmp, Cnt, Index, H, SB, SA : integer;
    FP : TFontObj;
    Align: AlignmentType;
    BaseLine: boolean;
    NoChar: boolean;   
    P: PChar;
    FCO: TFormControlObj;
  begin
  DHt := 0;    {for the fonts on this line get the maximum height}
  Cnt := 0;
  Desc := 0;    
  NoChar := True;
  P := PStart;
  for I := 0 to NN-1 do
    begin
    if not (P^ in [#2, #4, #6, #7]) then
      begin   {check for the no character case}
      NoChar := False;
      Break;
      end;
    Inc(P);
    end;

  if not NoChar then   
    repeat
      FP := Fonts.GetFontObjAt(PStart-Buff+Cnt, Index);
      Tmp := FP.GetHeight(Desc);
      DHt := IntMax(DHt, Tmp);  
      LR.Descent := IntMax(LR.Descent, Desc);
      J := Fonts.GetFontCountAt(PStart-Buff+Cnt, Len);
      Inc(Cnt, J);
    until Cnt >= NN;

  Cnt := 0;   {if images, then maybe they add extra space}
  SB := 0;
  SA := 0;  {space before and after}
  repeat
    Cnt := Cnt + Images.GetImageCountAt(PStart-Buff+Cnt);
    if Cnt < NN then
      begin
      H := Images.GetHeightAt(PStart-Buff+Cnt, Align);
      case Align of
        ATop: SA := IntMax(SA, H - DHt);
        AMiddle:
             begin
             Tmp := (H - DHt) div 2;
             SA := IntMax(SA, Tmp);
             SB := IntMax(SB, (H - DHt - Tmp));
             end;
        ABottom: SB := IntMax(SB, H - (DHt - Desc));
        end;
      end;
    Inc(Cnt);  {to skip by the image}
  until Cnt >= NN;

  Cnt := 0;   {now check on form controls}
  repeat
    Cnt := Cnt + TFormControlList(FormControls).GetControlCountAt(PStart-Buff+Cnt);
    if Cnt < NN then
      begin
      H := TFormControlList(FormControls).GetHeightAt(PStart-Buff+Cnt, BaseLine);
      if BaseLine then
        SB := IntMax(SB, H-(DHt-Desc))
      else
        SB := IntMax(SB, H-DHt);
      FCO := TFormControlList(FormControls).FindControl(PStart-Buff+Cnt);   
      if Assigned(FCO) then
        FCO.FYValue := Y;
      end;
    Inc(Cnt);  {to skip by the control}
  until Cnt >= NN;

  LR.Start := PStart;
  LR.LineHt := DHt;
  LR.Ln := NN;
  Tmp := Imgr.LeftIndent(Y);
  if Justify = Left then
    LR.LineIndent := Tmp
  else if Justify = Centered then
    LR.LineIndent := IntMax(Tmp, (Tmp + IMgr.RightSide(Y)-(FindTextWidth(Canvas, PStart, NN, True))) div 2)  
  else LR.LineIndent := (IMgr.RightSide(Y)-(FindTextWidth(Canvas, PStart, NN, True)))-1; 
  LR.SpaceBefore := LR.SpaceBefore + SB;
  LR.SpaceAfter := SA;
  Lines.Add(LR);
  Inc(PStart, NN);
  SectionHeight := SectionHeight +DHt + SA + LR.SpaceBefore;
  Tmp := DHt +SA + SB;
  Inc(Y, Tmp);  
  LR.LineImgHt := IntMax(Tmp, ImgHt);
  for I := 0 to NxImages.Count-1 do
    begin
    IMgr.Update(Y, TFloatingObj(NxImages[I]));  {update Image manager and Image} 
    {include images in Line height}
    LR.LineImgHt := IntMax(LR.LineImgHt,         
       Tmp+TFloatingObj(NxImages[I]).ImageHeight + 2*TFloatingObj(NxImages[I]).VSpace);
    end;
  NxImages.Clear;
  end;

begin
YValue := Y;
StartCurs := Curs;
PStart := Buff;
Last := Buff + Len - 1;
SectionHeight := 0;
Lines.Clear;
if Indent = SmallListIndent then  
  IMgr.SetLevelSmall(Y, Level)   {special case, <li> without <ul>}
else IMgr.SetLevel(Y, Level);
if (Len = 0) then
  begin
  Result := GetClearSpace;
  DrawHeight := Result;
  SectionHeight := Result;
  MaxWidth := 0;
  Exit;
  end;
Finished := False;
LevelIndent := Imgr.LeftIndent(Y);
MaxWidth := IMgr.Width;
Width := IMgr.RightSide(Y)-IMgr.LeftIndent(Y);
for I := 0 to Images.Count-1 do     {call drawlogic for all the images}
  begin   
  Obj := TFloatingObj(Images[I]);
  with Obj do
    begin
    if Obj is TImageObj then   
      TImageObj(Obj).DrawLogic(Self.ParentSectionList, Canvas, Fonts.GetFontObjAt(Pos, Indx), Width);
    MaxWidth := IntMax(MaxWidth, ImageWidth + Self.Indent);  {HScrollBar for wide images}
    end;
  end;
for I := 0 to FormControls.Count-1 do
  with TFormControlObj(FormControls[I]) do
    if Assigned(FControl) then
      MaxWidth := IntMax(MaxWidth, FControl.Width + Self.Indent);
NxImages := TList.Create;
while not Finished do
  begin
  Max := Last - PStart + 1;
  if Max <= 0 then Break;
  LR := LineRec.Create;     {a new line}
  if (Lines.Count = 0) then
    begin  {may need to move down past floating image}
    Tmp := GetClearSpace;
    if Tmp > 0 then
      begin
      LR.LineHt := Tmp;
      Inc(SectionHeight, Tmp);
      LR.Ln := 0;
      LR.Start := PStart;
      Inc(Y, Tmp);
      Lines.Add(LR);
      LR := LineRec.Create;
      end;
    end;

  if Self is TPreformated then Width := 32000
  else Width := IMgr.RightSide(Y)-IMgr.LeftIndent(Y);
  N := IntMax(FindCountThatFits1(Canvas, Width, PStart, Max, Y, IMgr,
              ImgHt, NxImages), 1); {N = at least 1}
  if N = Max then
    begin   {Do the remainder}
    LineComplete(N);
    Finished := True;
    end
  else
    begin
    P := PStart + N -1;
    if (P^ = ' ') then
      begin  {move past spaces so as not to print any on next line}
      while (N < Max) and ((P+1)^ = ' ') do
        begin
        Inc(P);
        Inc(N);
        end;
      LineComplete(N);
      Finished := N >= Max;
      end
    else if (N < Max) and ((P+1)^ in [#2, #4]) then {an image or control}
      begin
      LineComplete(N);
      Finished := False;
      end
    else
      Begin  {non space, wrap it by backing off to previous space or image}
      while not (P^ in [' ', #2, #4]) and (P > PStart) do Dec(P);
      if P = PStart then
        begin {no space found, forget the wrap, write the whole word and any
               spaces found after it}
        P := PStart+N-1;
        while (P <> Last) and not ((P+1)^ in [' ', #2, #4]) do
          begin
          Inc(P);
          end;
        while (P <> Last) and ((P+1)^ = ' ') do
          begin
          Inc(P);
          end;    
        MaxWidth := IntMax(MaxWidth, FindTextWidth(Canvas, PStart, P-PStart+1, True));  
        LineComplete(P-PStart+1);
        Finished := P = Last;
        end
      else
        begin  {found space}
        LineComplete(P-PStart+1);
        end;
      end;
    end;
  end;
NxImages.Free;
Curs := StartCurs + Len;
if Level > 0 then
  {for lists, clear left floating images}
  begin
  Tmp := IMgr.GetLevelClear - YValue;
  if Tmp > SectionHeight then SectionHeight := Tmp;
  end;
DrawHeight := IMgr.ImageBottom - YValue;  {in case image overhangs}
if DrawHeight < SectionHeight then
  DrawHeight := SectionHeight;        
Result := SectionHeight;
end;

{----------------TSection.Draw}
function TSection.Draw(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X: integer; Y: integer) : integer;
var
  I: integer;
  MySelB, MySelE: integer;
  DC: HDC;
  Ctrl: TFormControlObj;
  YOffset: integer;

  procedure DrawTheText(LR: LineRec; Start : PChar; Cnt, Descent: integer);
  var
    I, J, J1, J2, J3, J4, XX, OHang, Index, Addon, TopP, Tmp : integer;
    Obj: TFloatingObj;
    FO: TFontObj;
    ARect: TRect;
    Inverted, ImageAtStart: boolean;
    S: string;

    function ChkInversion(C : integer; var Count: Integer) : boolean;
    var
      LongCount: integer;
    begin
    Result := False;
    Count := 32000;
    if MySelE < MySelB then Exit;
    if (MySelB <= C) and (MySelE > C) then
      begin
      Result := True;
      LongCount := MySelE - C;
      end
    else if MySelB > C then LongCount := MySelB - C
    else if (MySelB = C) and ParentSectionList.ShowDummyCaret then
      LongCount := 1
    else LongCount := 32000;
    if LongCount > 32000 then Count := 32000
      else Count := LongCount;
    end;

  begin  {Y is at bottom of line here}
  ImageAtStart := True;
  XX := X + LR.LineIndent;
  LR.DrawY := Y-LR.LineHt;
  LR.DrawX := XX;
  while Cnt > 0 do
    begin
    I := 1;
    J1 := Fonts.GetFontCountAt(Start-Buff, Len)-1;
    J2 := Images.GetImageCountAt(Start-Buff)-1;
    J4 := TFormControlList(FormControls).GetControlCountAt(Start-Buff)-1;
    FO := Fonts.GetFontObjAt(Start-Buff, Index);
    Canvas.Font := FO.TheFont;
    OHang := FO.OverHang;
    if J2 = -1 then
      begin  {it's an image}
      Obj := Images.FindImage(Start-Buff);
      FO := Fonts.GetFontObjAt(Start-Buff, Index);
      if Obj is TImageObj then
        begin
        if Obj.ObjAlign in [ALeft, ARight] then
          begin
          if ImageAtStart then
            begin
            TImageObj(Obj).Draw(Canvas, IMgr.LfEdge+Obj.Indent,
                 Y-LR.LineHt-LR.SpaceBefore, Y-Descent, FO);
            end
          else
            begin {if not at start, draw on next line}
            TImageObj(Obj).Draw(Canvas, IMgr.LfEdge+Obj.Indent, Y, Y-Descent, FO);
            end;
          end
        else
          begin
          TImageObj(Obj).Draw(Canvas, XX+Obj.HSpace, Y-LR.LineHt, Y-Descent, FO);
          XX := XX + Obj.ImageWidth + 2*Obj.HSpace;
          ImageAtStart := False;
          end;
        end;
      end
    else if J4 = -1 then
      begin  {it's a form control}
      Ctrl := TFormControlList(FormControls).FindControl(Start-Buff);
      if Assigned(Ctrl.FControl) then
        with Ctrl, FControl do
          begin
          ShowIt := True;
          if BaseLine then
            TopP := Y - Height - Descent -YOffset {sits on baseline}
          else TopP := Y-Height-YOffset;
          Show;
          Left := XX;
          Top := TopP;
          if Ctrl is TRadioButtonFormControlObj then
            with  TRadioButtonFormControlObj(Ctrl) do
              begin
              TRadioButtonFormControlObj(Ctrl).RButton.Show;
              if MyCell.BkGnd then
                (FControl as TPanel).Color := MyCell.BkColor
              else (FControl as TPanel).Color := ParentSectionList.Background;
              TRadioButtonFormControlObj(Ctrl).RButton.Repaint;
              end;
          Inc(XX, Width);
          end;
      ImageAtStart := False;
      end
    else
      begin
      J := IntMin(J1, J2);
      J := IntMin(J, J4);
      Inverted := ChkInversion(Start-Buff, J3);
      J := IntMin(J, J3-1);
      I := IntMin(Cnt, J+1);
      if Inverted then
        begin
        SetBkMode(Canvas.Handle, Opaque);
        Canvas.Brush.Color := Canvas.Font.Color;
        Canvas.Font.Color := ParentSectionList.Background;
        end
      else
        SetBkMode(Canvas.Handle, Transparent);

      SetTextAlign(Canvas.Handle, TA_BaseLine);  {control and image upsets this}
      SetLength(S, I);     
      Move(Start^, S[1], I);  
      J := Pos(#160, S);
      while J > 0 do     {substitute spaces for #160}
        begin
        S[J] := ' ';
        J := Pos(#160, S);
        end;
      if Self is TPreformated then
        begin   {so will clip in Table cells}
        ARect := Rect(X, Y-LR.LineHt-LR.SpaceBefore-YOffset, X+IMgr.ClipWidth, Y-YOffset+1);
        ExtTextOut(Canvas.Handle, XX-OHang div 2, Y - Descent -YOffset, ETO_CLIPPED,
               @ARect, PChar(S), I, Nil);
        Addon := 0;
        end
      else
        begin
        with FO do
          if SScript = Normal then Addon := 0
            else if SScript = SupSc then Addon := -(FontHeight div 3)
            else Addon := Descent div 2 +1;
        TextOut(Canvas.Handle, XX-OHang div 2, Y - Descent + Addon - YOffset, PChar(S), I);
        end;
      {Put in a dummy caret to show character position}
      if ParentSectionList.ShowDummyCaret and not Inverted
                                  and (MySelB = Start-Buff) then
        begin
        Canvas.Pen.Color := Canvas.Font.Color;
        Tmp := Y - Descent+ FO.Descent + Addon - YOffset;
        Canvas.Rectangle(XX-Ohang, Tmp, XX-Ohang+1, Tmp-FO.FontHeight);
        end;
      XX := XX + GetXExtent(Canvas.Handle, Start, I)-OHang;
      ImageAtStart := False;
      end;
    Dec(Cnt, I);
    Inc(Start, I);
    end;
  end;

  procedure DoDraw(I: integer);
  const
    MaxRoman = 20;
    LowRoman: array[1..MaxRoman] of string[5] = ('i', 'ii', 'iii', 'iv', 'v', 'vi',
       'vii', 'viii', 'ix', 'x', 'xi', 'xii', 'xiii', 'xiv', 'xv', 'xvi', 'xvii',
       'xviii', 'xix', 'xx');
    HighRoman: array[1..MaxRoman] of string[5] = ('I', 'II', 'III', 'IV', 'V', 'VI',
       'VII', 'VIII', 'IX', 'X', 'XI', 'XII', 'XIII', 'XIV', 'XV', 'XVI', 'XVII',
       'XVIII', 'XIX', 'XX');
  var
    NStr : string[7];
    BkGnd, BkGnd1: TColor;
    XS, AlphaNumb: integer;

    procedure Circle(X, Y: integer);
    var
      Rad: integer;
    begin
    Rad := 5 div 2;
    Canvas.Ellipse(X-Rad, Y-Rad, X+Rad+1, Y+Rad+1);
    end;

  begin
  with LineRec(Lines[I]) do
    begin
    Inc(Y, LineHt+SpaceBefore);
    XS := LevelIndent + X;
    if (I = 0) and (ListType <> None) then
      if ListType = Definition then  {definition list, do nothing}
      else if ListType = Ordered then   {ordered list}
        begin
        AlphaNumb := IntMin(ListNumb-1, 25);
        case TOListItem(Self).IndexType of
          'a': NStr := chr(ord('a')+AlphaNumb);
          'A': NStr := chr(ord('A')+AlphaNumb);
          'i': NStr := LowRoman[IntMin(ListNumb, MaxRoman)];
          'I': NStr := HighRoman[IntMin(ListNumb, MaxRoman)];
          else NStr := IntToStr(ListNumb);
          end;
        Canvas.Font := DefFont;   {Fonts[0] may have been changed}
        NStr := NStr+'.';
        SetBkMode(DC, Transparent);
        Canvas.TextOut(XS-5-Canvas.TextWidth(NStr), Y-Descent-YOffset, NStr);
        end
      else if (ListType = Unordered) and not TUListItem(Self).Plain then
        with Canvas do
          begin
          BkGnd := ParentSectionList.Background;
          BkGnd1 := BkGnd and $FFFFFF;
          if (BkGnd = clBtnFace) or (BkGnd1 = clWhite)
                or (BkGnd1 = clSilver) or
                ((BkGnd = clWindow) and (GetSysColor(Color_Window) = $FFFFFF))then
            case Level of
              0,3: begin Brush.Color := clRed; Pen.Color := clRed; end;
              1,4: begin Brush.Color := clNavy; Pen.Color := clNavy; end;
              2,5: begin Brush.Color := clMaroon; Pen.Color := clMaroon; end;
              end
          else
            begin
            Pen.Color := ParentSectionList.FontColor;
            Brush.Style := bsClear;
            end;
          Circle(XS-8, Y-(LineHt div 2) - YOffset);
          Brush.Color := BkGnd;
          Brush.Style := bsSolid;
          Pen.Color := ParentSectionList.FontColor;
          end;
    DrawTheText(LineRec(Lines[I]), Start, Ln, Descent);
    Inc(Y, SpaceAfter);
    end;
  end;

begin
Result := Y + SectionHeight;
YOffset := ParentSectionList.YOff;

if (Len > 0) and (Y-YOffset+DrawHeight >= ARect.Top) and (Y-YOffset < ARect.Bottom) then
  begin
  DC := Canvas.Handle;
  SetTextAlign(DC, TA_BaseLine);

  MySelB := ParentSectionList.SelB-StartCurs;
  MySelE := ParentSectionList.SelE-StartCurs;
  for I := 0 to Lines.Count-1 do
    with LineRec(Lines[I]) do
      if (Y-YOffset+LineImgHt >= ARect.Top) and (Y-YOffset < ARect.Bottom) then
        DoDraw(I)
      else  {do not completely draw extremely long paragraphs}
        Inc(Y, SpaceBefore + LineHt + SpaceAfter);
  end;
end;

{----------------TSection.CopyToClipboard}
procedure TSection.CopyToClipboard;
var
  I, J, Strt, X1, X2: integer;
  MySelB, MySelE: integer;
begin
MySelB := ParentSectionList.SelB - StartCurs;
MySelE := ParentSectionList.SelE - StartCurs;
for I := 0 to Lines.Count-1 do
  with LineRec(Lines.Items[I]) do
    begin
    Strt := Start-Buff;
    if (MySelE <= Strt) or (MySelB > Strt + Ln) then Continue;
    if MySelB-Strt > 0 then X1 := MySelB-Strt
      else X1 := 0;
    if MySelE-Strt < Ln then X2 := MySelE - Strt    
      else X2 := Ln;
    if X1 = 0 then     {output any line indent}
      for J := 0 to LineIndent div ListIndent -1 do
        ParentSectionList.CB.AddText('   ', 3);
    ParentSectionList.CB.AddText(Start+X1, X2-X1);
    if X2 = Ln then ParentSectionList.CB.AddTextCR('', 0);
    end;
end;

{----------------TSection.PtInObject}
function TSection.PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean;
{Y is distance from start of section}
begin
Result := (Images.Count > 0) and Images.PtInObject(X, YValue+Y, Obj, IX, IY);
end;

{----------------TSection.GetURL}
function TSection.GetURL(Canvas: TCanvas; X: integer; Y: integer;
          var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj): boolean;
{Y is distance from start of section}
var
  I, H, L, Index, Width, TotalHt, IX, IY, Posn: integer;
  FO : TFontObj;
  LR: LineRec;
  IMap, UMap: boolean;
  MapItem: TMapItem;
  ImageObj: TImageObj;

  function MakeCopy(UrlTarget: TUrlTarget): TUrlTarget;
  begin
  Result := TUrlTarget.Create;
  Result.Assign(UrlTarget.Url, UrlTarget.Target);
  end;

begin
Result := False;
{First, check to see if in an image}
if (Images.Count > 0) and
    Images.PtInImage(X, YValue+Y, IX, IY, Posn, IMap, UMap, MapItem, ImageObj) then
  begin
  ParentSectionList.ActiveImage := ImageObj;
  if Assigned(ImageObj.MyFormControl) then
    begin
    FormControl := ImageObj.MyFormControl;
    Result := True;
    FormControl.XTmp := IX;
    FormControl.YTmp := IY;
    end
  else if UMap then
    begin
    if MapItem.GetURL(IX, IY, UrlTarg) then
      Result := True;
    end
  else
    begin
    FO := Fonts.GetFontObjAt(Posn, Index);
    if FO.UrlTarget.Url <> '' then
      begin   {found an URL}
      Result := True;
      UrlTarg := MakeCopy(FO.UrlTarget);
      ParentSectionList.ActiveLink := FO;
      if IMap then
        UrlTarg.Url := UrlTarg.Url + '?'+IntToStr(IX)+','+IntToStr(IY);
      end;
    end;
  Exit;
  end;

I := 0;  H := 0;
LR := Nil;
with Lines do
  begin
  while I < Count do
    begin
    LR := LineRec(Lines[I]);
    with LR do
      TotalHt := LineHt+SpaceBefore+SpaceAfter;
    if H+TotalHt > Y then Break;
    Inc(H, TotalHt);
    Inc(I);
    end;
  if I >= Count then Exit;
  end;
with LR do
  begin
  if X < DrawX then Exit;   {LR has been initialized}
  Width := X - DrawX;
  L := FindCountThatFits(Canvas, Width, Start, Ln);
  if L >= Ln then Exit;
  FO := Fonts.GetFontObjAt(L+(Start-Buff), Index);
  if FO.UrlTarget.Url <> '' then
    begin   {found an URL}
    if not ((Start+L)^ in [#4, #7]) then   {an image here would be in HSpace area}
      Result := True
    else Exit;
    UrlTarg := MakeCopy(FO.UrlTarget);
    ParentSectionList.ActiveLink := FO;
    end;
  end;
end;

{----------------TSection.FindCursor}
function TSection.FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var SCell: TObject; var Intext: boolean): integer;
{Given an X, Y, find the character position and the resulting XR, YR position
 for a caret along with its height, CaretHt.  Coordinates are relative to this
 section}
var
  I, H, L, Width, TotalHt, L1, W, Delta: integer;
  LR: LineRec;
begin
Result := -1;
I := 0;  H := 0; L1 := 0;
LR := Nil;
with Lines do
  begin
  while I < Count do
    begin
    LR := LineRec(Lines[I]);
    with LR do
      TotalHt := LineHt+SpaceBefore+SpaceAfter;
    if H+TotalHt > Y then Break;
    Inc(H, TotalHt);
    Inc(I);
    Inc(L1, LR.Ln);  {L1 accumulates char count of previous lines}
    end;
  if I >= Count then Exit;
  end;
with LR do
  begin
  InText := True;   
  CaretHt := LineHt;    {LR has been initialized}
  YR := H + SpaceBefore;
  if X < DrawX then
    begin
    Result := L1+StartCurs;
    InText := False;   
    Exit;
    end;
  Width := X-DrawX;
  L := FindCountThatFits(Canvas, Width, Start, Ln);
  W := FindTextWidth(Canvas, Start, L, False);
  XR := DrawX + W;
  if L < Ln then
    begin   {check to see if passed 1/2 character mark}
    Delta := FindTextWidth(Canvas, Start+L, 1, False);
    if Width > W+(Delta div 2) then
      begin
      Inc(L);
      Inc(XR, Delta);
      end;
    end
  else InText := False;
  Result := L+L1+StartCurs;
  end;
end;

{----------------TSection.FindString}
function TSection.FindString(From: integer; PC: PChar; MatchCase: boolean): integer;
var
  P: PChar;
  I: integer;
  LenPC: word;
  UCh, LCh: Char;
  S1, S2: string[255];

  function ScanCaseless(P: PChar; LCh, UCh: Char): PChar;
  {Ch is lower case here}
  var
    PU, PL: PChar;
  begin
  PU := StrScan(P, UCh);
  PL := StrScan(P, LCh);
  if not Assigned(PU) then Result := PL
  else if not Assigned(PL) then Result := PU
  else if (PU <= PL) then Result := PU
  else Result := PL;
  end;

begin
Result := -1;
if (Len = 0) or (From >= StartCurs + Len) then Exit;
if From < StartCurs then I := 0
else I := From-StartCurs;

if MatchCase then
  begin    {case sensitive search}
  P := StrPos(Buff + I, PC);
  if Assigned(P) then
    Result := StartCurs+(P-Buff);
  end
else
  begin    {Caseless search}
  UCh := PC^;
  LCh := AnsiLowerCase(UCh)[1];   {make lower case}
  UCh := AnsiUpperCase(LCh)[1];   {make upper case}
  LenPC := IntMin(StrLen(PC), 255);
  P := ScanCaseless(Buff + I, LCh, UCh);
  S1 := StrPas(PC);
  S2[0] := chr(LenPC);
  while Assigned(P) and (StrLen(P) >= LenPC) do
    begin
    System.Move(P^, S2[1], LenPC);
    if AnsiCompareText(S1, S2) = 0 then
      begin
      Result := StartCurs + (P-Buff);
      Exit;
      end;
    Inc(P);
    P := ScanCaseless(P, LCh, UCh);
    end;
  end;
end;

{----------------TSection.FindSourcePos}
function TSection.FindSourcePos(DocPos: integer): integer;   
var
  I: integer;
  IO: IndexObj;
begin
Result := -1;
if (Len = 0) or (DocPos >= StartCurs + Len) then Exit;

for I := SIndexList.Count-1 downto 0 do
  begin
  IO := PosIndex[I];
  if IO.Pos <= DocPos-StartCurs then
    begin
    Result := IO.Index + DocPos-StartCurs - IO.Pos;
    break;
    end;
  end;
end;

{----------------TSection.FindDocPos}
function TSection.FindDocPos(SourcePos: integer; Prev: boolean): integer;
{for a given Source position, find the nearest document position either Next or
 previous}
var
  I: integer;
  IO, IOPrev: IndexObj;
begin
Result := -1;
if Len = 0 then Exit;

if not Prev then
  begin
  I:= SIndexList.Count-1;
  IO := PosIndex[I];
  if SourcePos > IO.Index + (Len-1) - IO.Pos then Exit; {beyond this section}

  IOPrev := PosIndex[0];
  if SourcePos <= IOPrev.Index then
    begin  //in this section but before the start of Document text
    Result := StartCurs;
    Exit;
    end;

  for I := 1 to SIndexList.Count-1 do
    begin
    IO := PosIndex[I];
    if (SourcePos >= IOPrev.Index) and (SourcePos < IO.Index) then
      begin //between IOprev and IO
      if SourcePos-IOPrev.Index+IOPrev.Pos < IO.Pos then
        Result := StartCurs+IOPrev.Pos+(SourcePos-IOPrev.Index)
      else Result := StartCurs+IO.Pos;
      Exit;
      end;
    IOPrev := IO;
    end;
  //after the last IndexObj in list
  Result := StartCurs+IOPrev.Pos+(SourcePos-IOPrev.Index);
  end
else     //prev  -- we're iterating from the end of TSectionList
  begin
  IOPrev := PosIndex[0];
  if SourcePos < IOPrev.Index then Exit;   //before this section

  I:= SIndexList.Count-1;
  IO := PosIndex[I];
  if SourcePos > IO.Index + (Len-1) - IO.Pos then
    begin   //SourcePos is after the end of this section
    Result := StartCurs + (Len-1);
    Exit;
    end;

  for I := 1 to SIndexList.Count-1 do
    begin
    IO := PosIndex[I];
    if (SourcePos >= IOPrev.Index) and (SourcePos < IO.Index) then
      begin //between IOprev and IO
      if SourcePos-IOPrev.Index+IOPrev.Pos < IO.Pos then
        Result := StartCurs+IOPrev.Pos+(SourcePos-IOPrev.Index)
      else Result := StartCurs+IO.Pos-1;
      Exit;
      end;
    IOPrev := IO;
    end;
  //after the last IndexObj in list
  Result := StartCurs+IOPrev.Pos+(SourcePos-IOPrev.Index);
  end;
end;

{----------------TSection.CursorToXY}
function TSection.CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean;
var
  I, Curs: integer;
  LR: LineRec;
begin
Result := False;
if (Len = 0) or (Cursor > StartCurs + Len) then Exit;

I := 0;
LR := Nil;
Curs := Cursor - StartCurs;
Y := YValue;
with Lines do
  begin
  while I < Count do
    begin
    LR := LineRec(Lines[I]);
    with LR do
      begin
      if Curs < Ln then Break;
      Inc(Y, LineHt+SpaceBefore+SpaceAfter);
      Dec(Curs, Ln);
      end;
    Inc(I);
    end;
  if I >= Count then Exit;
  end;
X := LR.DrawX + FindTextWidth(Canvas, LR.Start, Curs, False);
Result := True;
end;

{----------------TSection.GetChAtPos}
function TSection.GetChAtPos(Pos: integer; var Ch: char; var Obj: TObject): boolean;
begin
Result := False;
if (Len = 0) or (Pos < StartCurs) or (Pos >= StartCurs + Len) then Exit;
Ch := Buff[Pos-StartCurs];
Obj := Self;
Result := True;
end;

procedure TSection.UpdateFonts;
begin
Fonts.UpdateFonts;
DefFont.UpdateFont(ParentSectionList, ParentSectionList.FontColor);
inherited UpdateFonts;
end;

{$IFDEF HL_LAZARUS}
initialization
  ListIndent:=35;
{$ENDIF}

end.



