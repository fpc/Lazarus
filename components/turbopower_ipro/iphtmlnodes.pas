unit IpHtmlNodes;

{ Global defines potentially affecting this unit }
{$I IPDEFINE.INC}

interface

uses
  Classes, SysUtils, Graphics, GraphUtil, LCLIntf, LCLType, Types, 
  Controls, StdCtrls, ExtCtrls, Buttons, Dialogs,
  IpConst, IpCSS, IpHtmlTypes, IpHtmlClasses, IpHtmlProp, 
  IpHtmlUtils, IpMsg, IpHtml;

type
  { Descendants of TIpHtmlNode }

  TIpHtmlNodeText = class(TIpHtmlNode)
  private
    FEscapedText: string;
    FFirstW: Boolean;
    function GetAnsiText: string;
    procedure SetAnsiText(const Value: string);
    procedure SetEscapedText(const Value: string);
    procedure AddAWord(StartP: PAnsiChar);
    function CutAndAddWord(StartP, EndP: PAnsiChar): PAnsiChar;
    procedure DoPreformattedWords(N: PAnsiChar);
    procedure DoNormalWords(N: PAnsiChar);
    procedure BuildWordList;
  protected
    PropsR : TIpHtmlProps; {reference}
    procedure ReportDrawRects(M : TRectMethod); override;
    procedure Enqueue; override;
    procedure EnqueueElement(const Entry: PIpHtmlElement); override;
    function ElementQueueIsEmpty: Boolean; override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property ANSIText: string read GetAnsiText write SetAnsiText;
    property EscapedText: string read FEscapedText write SetEscapedText;
  end;
  
  
  { Descendants of TIpHtmlNodeNv <- TIpHtmlNode }
  
  TIpHtmlNodeMETA = class(TIpHtmlNodeNv)
  private
    FScheme: string;
    FContent: string;
    FHttpEquiv: string;
    FName: string;
  public
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Content: string read FContent write FContent;
    property HttpEquiv: string read FHttpEquiv write FHttpEquiv;
    property Name: string read FName write FName;
    property Scheme: string read FScheme write FScheme;
  end;

  TIpHtmlNodePARAM = class(TIpHtmlNodeNv)
  private
    FId: string;
    FValueType: TIpHtmlObjectValueType;
    FValue: string;
    FName: string;
  public
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
    property ValueType: TIpHtmlObjectValueType read FValueType write FValueType;
  end;

  TIpHtmlNodeSCRIPT = class(TIpHtmlNodeNv);
  
  
  { Descendants of TIpHtmlNodeCore <-- TIpHtmlNodeMulti <-- TIpHtmlNode }
  
  TIpHtmlNodeAREA = class(TIpHtmlNodeCore)
  private
    FShape: TIpHtmlMapShape;
    FTabIndex: Integer;
    FTarget: string;
  protected
    FNoHRef: Boolean;
    FHRef: string;
    FCoords: string;
    FAlt: string;
    FRect : TRect;
    FRgn : HRgn;
    function GetHint: string; override;
  public
    destructor Destroy; override;
    function PtInRects(const P: TPoint): Boolean;
    procedure Reset;
    property Rect: TRect read FRect write FRect;
    property Rgn: HRgn read FRgn write FRgn;

  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Alt : string read FAlt write FAlt;
    property Coords : string read FCoords write FCoords;
    property HRef : string read FHRef write FHRef;
    property NoHRef : Boolean read FNoHRef write FNoHRef;
    property Shape : TIpHtmlMapShape read FShape write FShape;
    property TabIndex : Integer read FTabIndex write FTabIndex;
    property Target: string read FTarget write FTarget;
  end;

  TIpHtmlNodeCOL = class(TIpHtmlNodeCore)
  private
    FAlign: TIpHtmlAlign;
    FVAlign: TIpHtmlVAlign3;
    FSpan: Integer;
    FWidth: TIpHtmlMultiLength;
  protected
    function GetAlign: TIpHtmlAlign; override;
    procedure SetAlign(const Value: TIpHtmlAlign); override;
  public
    destructor Destroy; override;
    procedure LoadAndApplyCSSProps; override;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Align;
    property Span: Integer read FSpan write FSpan;
    property VAlign: TIpHtmlVAlign3 read FVAlign write FVAlign;
    property Width: TIpHtmlMultiLength read FWidth write FWidth;
  end;

  TIpHtmlNodeCOLGROUP = class(TIpHtmlNodeCore)
  private
    FAlign: TIpHtmlAlign;
    FSpan: Integer;
    FVAlign: TIpHtmlVAlign3;
    FWidth: TIpHtmlMultiLength;
  protected
    function GetAlign: TIpHtmlAlign; override;
    procedure SetAlign(const Value: TIpHtmlAlign); override;
  public
    destructor Destroy; override;
    procedure LoadAndApplyCSSProps; override;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Align;
    property Span: Integer read FSpan write FSpan;
    property VAlign: TIpHtmlVAlign3 read FVAlign write FVAlign;
    property Width: TIpHtmlMultiLength read FWidth write FWidth;
  end;
  
  TIpHtmlNodeFIELDSET = class(TIpHtmlNodeCore);

  TIpHtmlNodeFRAME = class(TIpHtmlNodeCore)
  private
    FFrameBorder: Integer;
    FLongDesc: string;
    FMarginHeight: Integer;
    FMarginWidth: Integer;
    FName: string;
    FNoResize: Boolean;
    FScrolling: TIpHtmlFrameScrolling;
    FSrc: string;
    procedure SetFrameBorder(const Value: Integer);
    procedure SetMarginHeight(const Value: Integer);
    procedure SetMarginWidth(const Value: Integer);
    procedure SetScrolling(const Value: TIpHtmlFrameScrolling);
  public
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property FrameBorder : Integer read FFrameBorder write SetFrameBorder;
    property LongDesc : string read FLongDesc write FLongDesc;
    property MarginHeight : Integer read FMarginHeight write SetMarginHeight;
    property MarginWidth : Integer read FMarginWidth write SetMarginWidth;
    property Name : string read FName write FName;
    property NoResize : Boolean read FNoResize write FNoResize;
    property Scrolling : TIpHtmlFrameScrolling read FScrolling write SetScrolling;
    property Src : string read FSrc write FSrc;
  end;

  TIpHtmlNodeLEGEND = class(TIpHtmlNodeCore)
  private
    FAlign: TIpHtmlVAlignment2;
  public
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Align : TIpHtmlVAlignment2 read FAlign write FAlign;
  end;

  TIpHtmlNodeLINK = class(TIpHtmlNodeCore)
  private
    FHRef: string;
    FRev: string;
    FRel: string;
    FType: string;
  public
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property HRef : string read FHRef write FHRef;
    property Rel : string read FRel write FRel;
    property Rev : string read FRev write FRev;
    property Type_ : string read FType write FType;
  end;

  TIpHtmlNodeMAP = class(TIpHtmlNodeCore)
  private
    FName : string;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Name : string read FName write FName;
  end;
  
  TIpHtmlNodeNOFRAMES = class(TIpHtmlNodeCore);

  TIpHtmlNodeOPTGROUP = class(TIpHtmlNodeCore)
  private
    FDisabled: Boolean;
    FGroupLabel: string;
  public
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Disabled : Boolean read FDisabled write FDisabled;
    property GroupLabel : string read FGroupLabel write FGroupLabel;
  end;
  
  TIpHtmlNodeOPTION = class(TIpHtmlNodeCore)
  private
    FDisabled: Boolean;
    FOptionLabel: string;
    FSelected: Boolean;
    FValue: string;
  public
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Disabled : Boolean read FDisabled write FDisabled;
    property OptionLabel : string read FOptionLabel write FOptionLabel;
    property Selected : Boolean read FSelected write FSelected;
    property Value : string read FValue write FValue;
  end;

  TIpHtmlNodeTHeadFootBody = class(TIpHtmlNodeCore)
  private
    FAlign: TIpHtmlAlign;
  protected
    function GetAlign: TIpHtmlAlign; override;
    procedure SetAlign(const Value: TIpHtmlAlign); override;
  public
    procedure LoadAndApplyCSSProps; override;
  end;

  TIpHtmlNodeTABLEHEADFOOTBODYClass = class of TIpHtmlNodeTHeadFootBody;

  TIpHtmlNodeTR = class(TIpHtmlNodeCore)
  private
    FAlign: TIpHtmlAlign;
    FVAlign: TIpHtmlVAlign;
    FBgColor: TColor;
    FTextColor: TColor;
    procedure SetBgColor(const AValue: TColor);
    procedure SetTextColor(const AValue: TColor);
  protected
    procedure AppendSelection(var S: String; var Completed: Boolean); override;
    function GetAlign: TIpHtmlAlign; override;
    procedure SetAlign(const Value: TIpHtmlAlign); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    procedure LoadAndApplyCSSProps; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Align;
    property VAlign : TIpHtmlVAlign read FVAlign write FVAlign;
    property BgColor: TColor read FBgColor write SetBgColor;
    property TextColor: TColor read FTextColor write SetTextColor;
  end;

  
  { Descendants of TIpHtmlNodeHeadFootBody <-- TIpHtmlNodeCore <-- TIpHtmNodeMulti <-- TIpHtmlNode }
  
  TIpHtmlNodeTBODY = class(TIpHtmlNodeTHeadFootBody)
  private
    FVAlign: TIpHtmlVAlign3;
  public
    constructor Create(ParentNode : TIpHtmlNode);
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Align;
    property VAlign : TIpHtmlVAlign3 read FVAlign write FVAlign;
  end;

  TIpHtmlNodeTFOOT = class(TIpHtmlNodeTHeadFootBody)
  private
    FVAlign: TIpHtmlVAlign3;
  public
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property VAlign : TIpHtmlVAlign3 read FVAlign write FVAlign;
  end;
  
  TIpHtmlNodeTHEAD = class(TIpHtmlNodeTHeadFootBody)
  private
    FVAlign: TIpHtmlVAlign3;
  protected
  public
    constructor Create(ParentNode : TIpHtmlNode);
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Align;
    property VAlign : TIpHtmlVAlign3 read FVAlign write FVAlign;
  end;


  { Descendants of TIpHtmlNodeBlock <-- TIpHtmlNodeCore <-- TIpHtmNodeMulti <-- TIpHtmlNode }
  
  TIpHtmlNodeCAPTION = class(TIpHtmlNodeBlock)
  private
    FAlign: TIpHtmlVAlignment2;
  public
    constructor Create(ParentNode: TIpHtmlNode);
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Align: TIpHtmlVAlignment2 read FAlign write FAlign;
  end;

  TIpHtmlNodeTableHeaderOrCell = class(TIpHtmlNodeBlock)
  private
    FAlign: TIpHtmlAlign;
    FCalcWidthMin: Integer;
    FCalcWidthMax: Integer;
    FColspan: Integer;
    FHeight: TIpHtmlPixels;
    FNowrap: Boolean;
    FRowspan: Integer;
    FWidth: TIpHtmlLength;
    FVAlign: TIpHtmlVAlign3;
  protected
    procedure AppendSelection(var S: String; var Completed: Boolean); override;
    function GetAlign: TIpHtmlAlign; override;
    procedure SetAlign(const Value: TIpHtmlAlign); override;
  public
    FPadRect : TRect;
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure Layout(RenderProps: TIpHtmlProps; const TargetRect : TRect); override;
    procedure LoadAndApplyCSSProps; override;
    procedure Render(RenderProps: TIpHtmlProps); override;
    procedure CalcMinMaxPropWidth(RenderProps: TIpHtmlProps; var Min, Max: Integer); override;
    procedure DimChanged(Sender: TObject);
  public
    property PadRect : TRect read FPadRect write FPadRect;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Align;
    property BgColor;
    property CalcWidthMin: Integer read FCalcWidthMin write FCalcWidthMin;
    property CalcWidthMax: Integer read FCalcWidthMax write FCalcWidthMax;
    property Colspan : Integer read FColspan write FColspan;
    property Height : TIpHtmlPixels{Integer} read FHeight write FHeight;
    property Nowrap : Boolean read FNowrap write FNowrap;
    property Rowspan : Integer read FRowspan write FRowspan;
    property VAlign : TIpHtmlVAlign3 read FVAlign write FVAlign;
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;
  
  
  { Descendants of TIpHtmlNodeTableHeaderOrCell <-- TIpHtmlNodeBlock 
      <-- TIpHtmlNodeCore <-- TIpHtmNodeMulti <-- TIpHtmlNode }

  TIpHtmlNodeTH = class(TIpHtmlNodeTableHeaderOrCell)
  public
    constructor Create(ParentNode: TIpHtmlNode);
  end;

  TIpHtmlNodeTD = class(TIpHtmlNodeTableHeaderOrCell)
  public
    constructor Create(ParentNode: TIpHtmlNode);
  end;

  
  { Descendants of TIpHtmlNodeInline <-- TIpHtmlNodeCore <-- TIpHtmlNodeMulti <-- TIpHtmlNode }
  
  TIpHtmlNodeA = class(TIpHtmlNodeInline)
  private
    FHRef: string;
    FName: string;
    FRel: string;
    FRev: string;
    FShape: TIpHtmlMapShape;
    FTabIndex: Integer;
    FTarget: string;
    procedure SetHRef(const Value: string);
    procedure SetName(const Value: string);
  protected
    FHasRef : Boolean;
    FHot: Boolean;
    MapAreaList: TFPList;
    procedure ClearAreaList; override;
    procedure SetHot(const Value: Boolean);
    procedure BuildAreaList; override;
    procedure AddMapArea(const R: TRect);
    function GetHint: string; override;
    property HasRef: Boolean read FHasRef;
  public
    constructor Create(ParentNode: TIpHtmlNode);
    destructor Destroy; override;
//    procedure MakeVisible; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  public
    // These were originally protected, but had to be made public for unit IpHtmlNodes.
    procedure DoOnBlur;
    procedure DoOnFocus;
    function PtInRects(const P: TPoint): Boolean;
    function RelMapPoint(const P: TPoint): TPoint;
    property Hot: Boolean read FHot write SetHot;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property HRef: string read FHRef write SetHRef;
    property Name: string read FName write SetName;
    property Rel: string read FRel write FRel;
    property Rev: string read FRev write FRev;
    property Shape: TIpHtmlMapShape read FShape write FShape;
    property TabIndex: Integer read FTabIndex write FTabIndex;
    property Target: string read FTarget write FTarget;
  end;

  TIpHtmlNodeADDRESS = class(TIpHtmlNodeInline);

  TIpHtmlNodeAPPLET = class(TIpHtmlNodeInline)
  private
    FArchive: string;
    FObjectCode: string;
    FVSpace: Integer;
    FHSpace: Integer;
    FHeight: Integer;
    FWidth: TIpHtmlLength;
    FName: string;
    FCodebase: string;
    FCode: string;
    FAlt: string;
    FAlignment: TIpHtmlImageAlign;
  protected
    function GetHint: string; override;
  public
    destructor Destroy; override;
    procedure WidthChanged(Sender: TObject);
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Align : TIpHtmlImageAlign read FAlignment write FAlignment;
    property Archive : string read FArchive write FArchive;
    property Alt : string read FAlt write FAlt;
    property ClassID;
    property Code : string read FCode write FCode;
    property Codebase : string read FCodebase write FCodebase;
    property Height : Integer read FHeight write FHeight;
    property HSpace : Integer read FHSpace write FHSpace;
    property Id;
    property Name : string read FName write FName;
    property ObjectCode : string read FObjectCode write FObjectCode;
    property Style;
    property Title;
    property VSpace : Integer read FVSpace write FVSpace;
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;

  TIpHtmlNodeBLINK = class(TIpHtmlNodeInline);

  TIpHtmlNodeBLOCKQUOTE = class(TIpHtmlNodeInline)
  public
    procedure Enqueue; override;
  end;

  TIpHtmlNodeBR = class(TIpHtmlNodeInline)
  private
    FClear: TIpHtmlBreakClear;
    FId: string;
  protected
    procedure SetClear(const Value: TIpHtmlBreakClear);
    function GetMargin(AMargin: TIpHtmlElemMargin; ADefault:Integer): Integer; override;
  public
    constructor Create(ParentNode: TIpHtmlNode);
    procedure Enqueue; override;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property ClassID;
    property Clear : TIpHtmlBreakClear read FClear write SetClear;
    property Id : string read FId write FId;
    property Title;
  end;

  TIpHtmlNodeDD = class(TIpHtmlNodeInline)
  public
    constructor Create(ParentNode: TIpHtmlNode);
    procedure Enqueue; override;
  end;

  TIpHtmlNodeDIV = class(TIpHtmlNodeInline)
  private
    FAlign : TIpHtmlAlign;
  protected
    function GetAlign: TIpHtmlAlign; override;
    procedure SetAlign(const Value: TIpHtmlAlign); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure Enqueue; override;
    procedure LoadAndApplyCSSProps; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    (*
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Align : TIpHtmlAlign read FAlign write FAlign;
    *)
  end;

  TIpHtmlNodeDL = class(TIpHtmlNodeInline)
  private
    FCompact : Boolean;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    procedure Enqueue; override;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Compact : Boolean read FCompact write FCompact;
  end;

  TIpHtmlNodeDT = class(TIpHtmlNodeInline)
  public
    constructor Create(ParentNode: TIpHtmlNode);
    procedure Enqueue; override;
  end;

  TIpHtmlNodeFORM = class(TIpHtmlNodeInline)
  private
    FAccept: string;
    FAcceptCharset: string;
    FName: string;
    FEnctype: string;
    FAction: string;
    FMethod: TIpHtmlFormMethod;
  protected
    procedure AddChild(Node: TIpHtmlNode; const UserData: Pointer);
    procedure ResetControl(Node: TIpHtmlNode; const UserData: Pointer);
    procedure ResetRequest; override;
    {$IFNDEF HtmlWithoutHttp}
    procedure SubmitRequest; override;
    {$ENDIF}
  public
    constructor Create(ParentNode: TIpHtmlNode);
    destructor Destroy; override;
    procedure ResetForm;
    procedure SubmitForm;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Accept : string read FAccept write FAccept;
    property AcceptCharset : string read FAcceptCharset write FAcceptCharset;
    property Action : string read FAction write FAction;
    property Enctype : string read FEnctype write FEnctype;
    property Method : TIpHtmlFormMethod read FMethod write FMethod;
    property Name : string read FName write FName;
  end;

  TIpHtmlNodeGenInline = class(TIpHtmlNodeInline)
  protected
    Props: TIpHtmlProps;
    procedure ApplyProps(const RenderProps: TIpHtmlProps); virtual; abstract;
  public
    constructor Create(ParentNode: TIpHtmlNode);
    destructor Destroy; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  end;

  TIpHtmlNodeLABEL = class(TIpHtmlNodeInline)
  private
    FLabelFor: string;
  public
    constructor Create(ParentNode: TIpHtmlNode);
    destructor Destroy; override;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property LabelFor : string read FLabelFor write FLabelFor;
  end;

  TIpHtmlNodeList = class(TIpHtmlNodeInline)
  private
    FCompact: Boolean;
    FListType: TIpHtmlULType;
    procedure SetListType(const Value: TIpHtmlULType);
  public
    procedure Enqueue; override;
    procedure LoadAndApplyCSSProps; override;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Compact: Boolean read FCompact write FCompact;
    property ListType: TIpHtmlULType read FListType write SetListType;
  end;

  TIpHtmlNodeNOSCRIPT = class(TIpHtmlNodeInline);
  
  TIpHtmlNodeOBJECT = class(TIpHtmlNodeInline)
  private
    FAlignment: TIpHtmlImageAlign;
    FArchive: string;
    FBorder: Integer;
    FCodebase: string;
    FCodeType: string;
    FData: string;
    FDeclare: Boolean;
    FHeight: Integer;
    FHSpace: Integer;
    FName: string;
    FStandby: string;
    FUseMap: string;
    FVSpace: Integer;
    FWidth: TIpHtmlLength;
  protected
  public
    destructor Destroy; override;
    procedure WidthChanged(Sender: TObject);
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Align : TIpHtmlImageAlign read FAlignment write FAlignment;
    property Archive : string read FArchive write FArchive;
    property Border : Integer read FBorder write FBorder;
    property ClassID;
    property Codebase : string read FCodebase write FCodebase;
    property CodeType : string read FCodeType write FCodeType;
    property Data : string read FData write FData;
    property Declare : Boolean read FDeclare write FDeclare;
    property Height : Integer read FHeight write FHeight;
    property HSpace : Integer read FHSpace write FHSpace;
    property Name : string read FName write FName;
    property Standby : string read FStandby write FStandby;
    property UseMap : string read FUseMap write FUseMap;
    property VSpace : Integer read FVSpace write FVSpace;
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;
  
  TIpHtmlNodePRE = class(TIpHtmlNodeInline)
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  end;

  TIpHtmlNodeQ = class(TIpHtmlNodeInline);
  
  
  { Descendants of TIpHtmlNodeAlignInline <-- TIpHtmlNodeInline 
      <-- TIpHtmlNodeCore <-- TIpHtmlNodeMulti <-- TIpHtmlNode }
  
  TIpHtmlNodeHR = class(TIpHtmlNodeAlignInline)
  private
    FColor: TColor;
    FNoShade : Boolean;
    FSize : TIpHtmlInteger;
    FWidth : TIpHtmlLength;
  protected
    SizeWidth : TIpHtmlPixels;
    FDim : TSize;
    function GrossDrawRect: TRect;
  public
    constructor Create(ParentNode: TIpHtmlNode);
    destructor Destroy; override;
    procedure Draw(Block: TIpHtmlNodeBlock); override;
    procedure CalcMinMaxWidth(var Min, Max: Integer); override;
    procedure Enqueue; override;
    function GetDim(ParentWidth: Integer): TSize; override;
    procedure WidthChanged(Sender: TObject);
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Color : TColor read FColor write FColor;
    property NoShade  : Boolean read FNoShade write FNoShade;
    property Size : TIpHtmlInteger read FSize write FSize;
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;
    
  TIpHtmlNodeIMG = class(TIpHtmlNodeAlignInline)
  private
    FAlt: string;
    FBorder: Integer;
    FHeight: TIpHtmlPixels;
    FHSpace: Integer;
    FIsMap: Boolean;
    FLongDesc: string;
    FName: string;
    FPicture : TPicture;
    FSrc: string;
    FUseMap: string;
    FVSpace: Integer;
    FWidth: TIpHtmlLength;
    function GetBorder: Integer;
    procedure SetBorder(const Value: Integer);
    procedure SetUseMap(const Value: string);
    procedure SetHSpace(const Value: Integer);
    procedure SetVSpace(const Value: Integer);
  protected
    FSize: TSize;
    NetDrawRect: TRect;
    SizeWidth: TIpHtmlPixels;
    procedure ReportDrawRects(M : TRectMethod); override;
    procedure ReportMapRects(M : TRectMethod); override;
    function GetHint: string; override;
    procedure InvalidateSize; override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure Draw(Block: TIpHtmlNodeBlock); override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    procedure CalcMinMaxWidth(var Min, Max: Integer); override;
    function GetDim(ParentWidth: Integer): TSize; override;
    procedure ImageChange(NewPicture : TPicture); override;
    procedure DimChanged(Sender: TObject);
  public
    // Were protected initially, but had to be made public for unit IpHtmlNodes
    function GrossDrawRect: TRect;
    procedure LoadImage;
    procedure UnloadImage;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Alt: string read FAlt write FAlt;
    property Border: Integer read GetBorder write SetBorder;
    property Height: TIpHtmlPixels read FHeight write FHeight;
    property HSpace: Integer read FHSpace write SetHSpace;
    property IsMap: Boolean read FIsMap write FIsMap;
    property LongDesc: string read FLongDesc write FLongDesc;
    property Name: string read FName write FName;
    property Picture: TPicture read FPicture;
    property Src: string read FSrc write FSrc;
    property UseMap: string read FUseMap write SetUseMap;
    property VSpace: Integer read FVSpace write SetVSpace;
    property Width: TIpHtmlLength read FWidth write FWidth;
  end;

  TIpHtmlNodeLI = class(TIpHtmlNodeAlignInline)
  private
    FCompact: Boolean;
    FListType : TIpHtmlULType;
    FValue : Integer;
    procedure SetListType(const Value: TIpHtmlULType);
    procedure SetValue(const Value: Integer);
  protected
    WordEntry : PIpHtmlElement;
    function GrossDrawRect: TRect;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    procedure Draw(Block: TIpHtmlNodeBlock); override;
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    procedure CalcMinMaxWidth(var Min, Max: Integer); override;
    function GetDim(ParentWidth: Integer): TSize; override;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Compact : Boolean read FCompact write FCompact;
    property ListType : TIpHtmlULType read FListType write SetListType;
    property Value : Integer read FValue write SetValue;
  end;
  
  TIpHtmlNodeOL = class(TIpHtmlNodeInline)
  private
    FCompact: Boolean;
    FStart: Integer;
    FOLStyle: TIpHtmlOLStyle;
    procedure SetStart(const Value: Integer);
    procedure SetOLStyle(const Value: TIpHtmlOLStyle);
  protected
    Counter: Integer;
    function GetNumString: string;
  public
    constructor Create(ParentNode: TIpHtmlNode);
    procedure Enqueue; override;
    procedure LoadAndApplyCSSProps; override;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Compact: Boolean read FCompact write FCompact;
    property Start: Integer read FStart write SetStart;
    property Style: TIpHtmlOLStyle read FOLStyle write SetOLStyle;
  end;

  { TIpHtmlNodeTABLE }

  TIpHtmlNodeTABLE = class(TIpHtmlNodeAlignInline)
  private
    FBgColor: TColor;
    FBorder: Integer;
    FBorderColor: TColor;
    FBorderStyle: TCSSBorderStyle;
    FFrame: TIpHtmlFrameProp;
    FRules: TIpHtmlRules;
    FSummary: string;
    function GetCellPadding: Integer;
    function GetCellSpacing: Integer;
    function GetMaxWidth: Integer;
    function GetMinWidth: Integer;
    function GetTableWidth: Integer;
    procedure SetBorder(const Value: Integer);
    procedure SetCellPadding(const Value: Integer);
    procedure SetCellSpacing(const Value: Integer);
    procedure SetFrame(const Value: TIpHtmlFrameProp);
    procedure SetRules(const Value: TIpHtmlRules);
  protected
    FWidth: TIpHtmlLength;
    SizeWidth : TIpHtmlPixels; {last computed width of table}
    procedure SetRect(TargetRect: TRect); override;
    procedure InvalidateSize; override;
    function GetColCount: Integer;
  public
    FCaption: TIpHtmlNodeCAPTION;
    FLayouter : TIpHtmlBaseTableLayouter;
    BorderRect: TRect;
    BorderRect2: TRect; {includes caption if any}
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure Draw(Block: TIpHtmlNodeBlock); override;
    function ExpParentWidth: Integer; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    procedure CalcMinMaxWidth(var Min, Max: Integer); override;
    procedure Enqueue; override;
    function GetDim(ParentWidth: Integer): TSize; override;
    procedure LoadAndApplyCSSProps; override;
    procedure WidthChanged(Sender: TObject);
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property BgColor: TColor read FBgColor write FBgColor;
    property Border: Integer read FBorder write SetBorder;
    property BorderStyle: TCSSBorderStyle read FBorderStyle write FBorderStyle;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property CalcMinWidth: Integer read GetMinWidth;
    property CalcMaxWidth: Integer read GetMaxWidth;
    property CalcTableWidth: Integer read GetTableWidth;
    property CellPadding: Integer read GetCellPadding write SetCellPadding;
    property CellSpacing: Integer read GetCellSpacing write SetCellSpacing;
    property ColCount: Integer read GetColCount;
    property Frame: TIpHtmlFrameProp read FFrame write SetFrame;
    property Rules: TIpHtmlRules read FRules write SetRules;
    property Summary: string read FSummary write FSummary;
    property Width: TIpHtmlLength read FWidth write FWidth;
  end;  

  
  { Descendants of TIpHtmlNodeGenInline <-- TIpHtmlNodeInline 
      <-- TIpHtmlNodeCore <-- TIpHtmlNodeMulti <-- TIpHtmlNode}
  
  TIpHtmlNodeBASEFONT = class(TIpHtmlNodeGenInline)
  private
    FSize: Integer;
  protected
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  public
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Size : Integer read FSize write FSize;
  end;

  TIpHtmlNodeDEL = class(TIpHtmlNodeGenInline)
  private
    FCite: string;
    FDateTime: string;
  protected
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  public
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Cite: string read FCite write FCite;
    property DateTime: string read FDateTime write FDateTime;
  end;
  
  TIpHtmlNodeFONT = class(TIpHtmlNodeGenInline)
  private
    FSize: TIpHtmlRelSize;
    FFace: string;
    FColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetFace(const Value: string);
  protected
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure SizeChanged(Sender: TObject);
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Color : TColor read FColor write SetColor;
    property Face : string read FFace write SetFace;
    property Size : TIpHtmlRelSize read FSize write FSize;
  end;

  TIpHtmlNodeFontStyle = class(TIpHtmlNodeGenInline)
  private
    FHFStyle: TIpHtmlFontStyles;
  protected
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  public
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Style: TIpHtmlFontStyles read FHFStyle write FHFStyle;
  end;

  TIpHtmlNodeINS = class(TIpHtmlNodeGenInline)
  private
    FCite: string;
    FDateTime: string;
  protected
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  public
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Cite: string read FCite write FCite;
    property DateTime: string read FDateTime write FDateTime;
  end;

  TIpHtmlNodeNOBR = class(TIpHtmlNodeGenInline)
  protected
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  public
  end;

  TIpHtmlNodePhrase = class(TIpHtmlNodeGenInline)
  private
    FPhrStyle: TIpHtmlPhraseStyle;
  protected
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  public
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Style : TIpHtmlPhraseStyle read FPhrStyle write FPhrStyle;
  end;

  TIpHtmlNodeSPAN = class(TIpHtmlNodeGenInline)
  private
    FAlign : TIpHtmlAlign;
  protected
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
    function GetAlign: TIpHtmlAlign; override;
    procedure SetAlign(const Value: TIpHtmlAlign); override;
  public
    constructor Create(ParentNode: TIpHtmlNode);
    procedure LoadAndApplyCSSProps; override;
    (*
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Align : TIpHtmlAlign read FAlign write FAlign;
    *)
  end;

  { Descendants of TIpHtmlNodeList <-- TIpHtmlNodeInline <-- TIpHtmlNodeCore 
      <-- TIpHtmlNodeMulti <-- TIpHtmlNode}
  
  TIpHtmlNodeUL = class(TIpHtmlNodeList)
  public
    constructor Create(ParentNode: TIpHtmlNode);
  end;
  
  TIpHtmlNodeDIR = class(TIpHtmlNodeList);
  TIpHtmlNodeMENU = class(TIpHtmlNodeList);
  
  
  { Descendants of TIpHtmlNodeControl <-- TIpHtmlNodeAlignInline 
      <-- TIpHtmlNodeInline <-- TIpHtmlNodeCore <-- TIpHtmlNodeMulti 
      <-- TIpHtmlNode}
  
  TIpHtmlNodeBUTTON = class(TIpHtmlNodeControl)
  private
    FTabIndex: Integer;
    FValue: string;
    FName: string;
    FInputType: TIpHtmlButtonType;
    function GetButtonCaption: String;
    procedure SetInputType(const AValue: TIpHtmlButtonType);
    procedure SetValue(const AValue: String);
  protected
    procedure CalcSize;
    procedure SubmitClick(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure CreateControl(Parent : TWinControl); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure AddValues(NameList, ValueList : TStringList); override;
    procedure Reset; override;
    function Successful: Boolean; override;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property ButtonType : TIpHtmlButtonType read FInputType write SetInputType;
    property Disabled;
    property Name : string read FName write FName;
    property TabIndex : Integer read FTabIndex write FTabIndex;
    property Value : string read FValue write SetValue;
  end;
 
  TIpHtmlNodeINPUT = class(TIpHtmlNodeControl)
  private
    FChecked: Boolean;
    FInputType: TIpHtmlInputType;
    FMaxLength: Integer;
    FName: string;
    FReadOnly: Boolean;
    FTabIndex: Integer;
    FSize: Integer;
    FSrc: string;
    FValue: string;
  protected
    FPicture : TPicture;
    FFileEdit : TEdit;
    FFileSelect : TButton;
    procedure SubmitClick(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure FileSelect(Sender: TObject);
    procedure getControlValue;
    procedure ButtonClick(Sender: TObject);
    procedure ControlOnEditingDone(Sender: TObject);
    procedure ControlOnChange(Sender: TObject);
    function GetHint: string; override;
    procedure SetImageGlyph(Picture: TPicture);
    procedure CreateControl(Parent : TWinControl); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure AddValues(NameList, ValueList : TStringList); override;
    procedure Draw(Block: TIpHtmlNodeBlock); override;
    procedure Reset; override;
    function Successful: Boolean; override;
    procedure ImageChange(NewPicture : TPicture); override;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Alt;
    property Checked : Boolean read FChecked write FChecked;
    property Disabled;
    property InputType : TIpHtmlInputType read FInputType write FInputType;
    property MaxLength : Integer read FMaxLength write FMaxLength;
    property Name : string read FName write FName;
    property ReadOnly : Boolean read FReadOnly write FReadOnly;
    property Size : Integer read FSize write FSize;
    property Src : string read FSrc write FSrc;
    property TabIndex : Integer read FTabIndex write FTabIndex;
    property Value : string read FValue write FValue;
  end;
  
  TIpHtmlNodeSELECT = class(TIpHtmlNodeControl)
  private
    FMultiple: Boolean;
    FComboBox: Boolean;
    FName: string;
    FSize: Integer;
    FWidth: integer;
    FTabIndex: Integer;
  protected
    procedure CreateControl(Parent : TWinControl); override;
    procedure ButtonClick(Sender: TObject);
    procedure ControlOnEditingDone(Sender: TObject);
    procedure ListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure setText(aText: string);
    function getText: string;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure AddValues(NameList, ValueList : TStringList); override;
    procedure Reset; override;
    function Successful: Boolean; override;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Disabled;
    property Multiple : Boolean read FMultiple write FMultiple;
    property ComboBox : Boolean read FComboBox write FComboBox;
    property Name : string read FName write FName;
    property Size : Integer read FSize write FSize;
    property Width : Integer read FWidth write FWidth;
    property TabIndex : Integer read FTabIndex write FTabIndex;
    property Text : string read getText write setText;
  end;
  
  TIpHtmlNodeTEXTAREA = class(TIpHtmlNodeControl)
  private
    FReadOnly: Boolean;
    FTabIndex: Integer;
    FCols: Integer;
    FRows: Integer;
    FName: string;
  protected
    procedure CreateControl(Parent : TWinControl); override;
    procedure ControlOnEditingDone(Sender: TObject);
  public
    constructor Create(ParentNode: TIpHtmlNode);
    destructor Destroy; override;
    procedure AddValues(NameList, ValueList : TStringList); override;
    procedure Reset; override;
    function Successful: Boolean; override;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Cols : Integer read FCols write FCols;
    property Disabled;
    property Name : string read FName write FName;
    property ReadOnly : Boolean read FReadOnly write FReadOnly;
    property Rows : Integer read FRows write FRows;
    property TabIndex : Integer read FTabIndex write FTabIndex;
  end;

  
implementation

uses
  {$IFDEF UseGifImageUnit} //TODO all of this units not exists
    GifImage,
  {$ELSE}
    IpAnim,
    {$IFDEF AndersGIFImage }
      IpAnAGif,
    {$ENDIF}
    {$IFDEF ImageLibGIFImage }
      IpAnImgL,
    {$ENDIF}
  {$ENDIF}
  LazStringUtils;
  

type
  // Helper classes to access protected class members
  TIpHtmlOpener = class(TIpHtml);
  TIpHtmlNodeOpener = class(TIpHtmlNode);
  TIpHtmlNodeBlockOpener = class(TIpHtmlNodeBlock);
  TIpHtmlBaseTableLayouterOpener = class(TIpHtmlBaseTableLayouter);
  TFriendPanel = class(TCustomPanel);
  
type
  THtmlRadioButton = class(TRadioButton)
  protected
    FChecked: Boolean;
    procedure SetChecked(Value: Boolean); override;
    function GetChecked: Boolean; override;
    procedure CreateWnd; override;
end;

procedure THtmlRadioButton.CreateWnd;
begin
  inherited CreateWnd;
  //SendMessage(Handle, BM_SETCHECK, Integer(FChecked), 0);
end;

function THtmlRadioButton.GetChecked: Boolean;
begin
  Result := FChecked;
end;

procedure THtmlRadioButton.SetChecked(Value: Boolean);
begin
  inherited SetChecked(Value);
end;

function CalcBorderColor(AColor: TColor; AStyle: TCSSBorderStyle; 
  ASide: TIpHtmlFrameProp): TColor;
begin
  case AStyle of
    cbsRidge,
    cbsInset:
      if ASide in [hfAbove, hfLhs] then
        Result := ColorAdjustLuma(AColor, -60, False)
      else
        Result := ColorAdjustLuma(AColor, 60, False);
    cbsGroove,
    cbsOutset:
    if ASide in [hfAbove, hfLhs] then
      Result := ColorAdjustLuma(AColor, 60, False)
    else
      Result := ColorAdjustLuma(AColor, -60, False);
  else
    Result := AColor;
  end;
end;

{-------------------------------------------------------------------------------
                         Descendants of TIpHtmlNodeText
-------------------------------------------------------------------------------}

{ TIpHtmlNodeText }

constructor TIpHtmlNodeText.Create(ParentNode : TIpHtmlNode);
var
  lOwner: TIpHtmlOpener;
begin
  inherited Create(ParentNode);
  lOwner := TIpHtmlOpener(FOwner);
  PropsR := TIpHtmlProps.Create(lOwner.PropACache, lOwner.PropBCache);
end;

destructor TIpHtmlNodeText.Destroy;
begin
  inherited;
  PropsR.Free;
end;

procedure TIpHtmlNodeText.SetProps(const RenderProps: TIpHtmlProps);
var
  bgCol: TColor;
begin
  bgCol := PropsR.BgColor;
  PropsR.Assign(RenderProps);
  if FParentNode = FOwner.Body then
    PropsR.BgColor := bgCol;
end;

procedure TIpHtmlNodeText.Enqueue;
begin
  BuildWordList;
end;

procedure TIpHtmlNodeText.AddAWord(StartP: PAnsiChar);
begin
  if FFirstW then
    TIpHtmlOpener(Owner).AddWord(StartP, PropsR, Self)
  else
    TIpHtmlOpener(Owner).AddWord(StartP, nil, Self);
  FFirstW := False;
end;

function TIpHtmlNodeText.CutAndAddWord(StartP, EndP: PAnsiChar): PAnsiChar;
var
  EndCh: AnsiChar;
begin
  EndCh := EndP^;
  EndP^ := #0;
  AddAWord(StartP);
  EndP^ := EndCh;
  Result := EndP;
end;

procedure TIpHtmlNodeText.DoPreformattedWords(N: PAnsiChar);
var
  N2: PAnsiChar;
  ImplicitLF: Boolean;
begin
  ImplicitLF := False;
  while N^ <> #0 do begin
    case N^ of
    CR :
      ImplicitLF := True;
    LF :
      begin
        EnqueueElement(TIpHtmlOpener(Owner).HardLF);
        Inc(N);
        ImplicitLF := False;
      end;
    else
      begin
        if ImplicitLF then begin
          EnqueueElement(TIpHtmlOpener(Owner).HardLF);
          Inc(N);
          ImplicitLF := False;
        end;
        N2 := StrScan(N, CR);
        if N2 <> nil then
          N := CutAndAddWord(N, N2)
        else begin
          N2 := StrScan(N, LF);
          if N2 <> nil then
            N := CutAndAddWord(N, N2)
          else begin
            AddAWord(N);
            N^ := #0;
          end;
        end;
      end;
    end;
  end;
end;

procedure TIpHtmlNodeText.DoNormalWords(N: PAnsiChar);
var
  NewEntry : PIpHtmlElement;
  N2: PAnsiChar;
begin
  while N^ <> #0 do begin
    case N^ of
    LF :
      begin
        EnqueueElement(TIpHtmlOpener(Owner).HardLF);
        Inc(N);
      end;
    ' ' :
      begin
        if not ElementQueueIsEmpty then begin
          NewEntry := TIpHtmlOpener(Owner).NewElement(etWord, Self);
          NewEntry.AnsiWord := ' ';
          NewEntry.IsBlank := 1;
          if FFirstW then
            NewEntry.Props := PropsR
          else
            NewEntry.Props := nil;
          EnqueueElement(NewEntry);
          FFirstW := False;
        end;
        Inc(N);
      end;
    else
      begin
        N2 := N;
        while not (N2^ in [#0, ' ', LF]) do
          Inc(N2);
        if N2^ <> #0 then
          N := CutAndAddWord(N, N2)
        else begin
          AddAWord(N);
          N^ := #0;
        end;
      end;
    end;
  end;
end;

procedure TIpHtmlNodeText.BuildWordList;
var
  l : Integer;
  B : PAnsiChar;
begin
  FFirstW := True;
  l := length(EscapedText);
  if l > 0 then begin
    Getmem(B, l + 1);
    try
      TrimFormatting(EscapedText, B, PropsR.Preformatted);
      if PropsR.Preformatted then
        DoPreformattedWords(B)
      else
        DoNormalWords(B);
    finally
      FreeMem(B);
    end;
  end;
end;

function TIpHtmlNodeText.GetAnsiText: string;
begin
  Result := EscapeToAnsi(FEscapedText);
end;

procedure TIpHtmlNodeText.EnqueueElement(const Entry: PIpHtmlElement);
begin
  TIpHtmlNodeOpener(FParentNode).EnqueueElement(Entry);
end;

function FindInnerBlock(Node : TIpHTMLNode): TIpHtmlNodeBlock;
begin
  while (Node <> nil) and not (Node is TIpHtmlNodeBlock) do
    Node := TIpHtmlNodeOpener(Node).FParentNode;
  Result := TIpHtmlNodeBlock(Node);
end;

procedure TIpHtmlNodeText.SetAnsiText(const Value: string);
begin
  EscapedText := AnsiToEscape(Value);
end;

procedure TIpHtmlNodeText.SetEscapedText(const Value: string);
var
  Block: TIpHtmlNodeBlock;
begin
  FEscapedText := Value;
  Block := FindInnerBlock(Self);
  if Block = nil then
    exit;

  {we need to clear the queue so that it will be built again}
  TIpHtmlNodeBlockOpener(Block).FLayouter.ClearWordList;

  {then, we need to Invalidate the block so that
   the rendering logic recalculates everything}
  TIpHtmlNodeBlockOpener(Block).InvalidateSize;
end;

procedure TIpHtmlNodeText.ReportDrawRects(M: TRectMethod);
begin
  ReportCurDrawRects(Self, M);
end;

function TIpHtmlNodeText.ElementQueueIsEmpty: Boolean;
begin
  Result := TIpHtmlNodeOpener(FParentNode).ElementQueueIsEmpty;
end;


{-------------------------------------------------------------------------------
                         Descendants of TIpHtmlNodeCore
-------------------------------------------------------------------------------}

{ TIpHtmlNodeAREA }

destructor TIpHtmlNodeAREA.Destroy;
var
  I: Integer;
begin
  I := TIpHtmlOpener(Owner).AreaList.IndexOf(Self);
  if I <> -1 then
    TIpHtmlOpener(Owner).AreaList.Delete(I);
  inherited;
end;

function TIpHtmlNodeAREA.GetHint: string;
begin
  if Alt <> '' then
    Result := Alt
  else
    Result := HRef;
end;

function TIpHtmlNodeAREA.PtInRects(const P: TPoint): Boolean;
begin
  if PtInRect(FRect, P) then
    Result := True
  else
  if FRgn <> 0 then
    Result := PtInRegion(FRgn, P.x, P.y)
  else
    Result := False;
end;

procedure TIpHtmlNodeAREA.Reset;
begin
  if FRgn <> 0 then
    DeleteObject(FRgn);
  SetRectEmpty(FRect);
end;


{ TIpHtmlNodeCOL }

destructor TIpHtmlNodeCOL.Destroy;
begin
  inherited;
  FWidth.Free;
end;

procedure TIpHtmlNodeCOL.LoadAndApplyCSSProps;
begin
  inherited;
  if not (FCombinedCSSProps.Alignment in [haDefault, haUnknown]) then
    Align := FCombinedCSSProps.Alignment;
  // wp: what about VAlign?
end;

function TIpHtmlNodeCOL.GetAlign: TIpHtmlAlign;
begin
  Result := FAlign;
end;

procedure TIpHtmlNodeCOL.SetAlign(const Value: TIpHtmlAlign);
begin
  FAlign := Value;
end;


{ TIpHtmlNodeCOLGROUP }

destructor TIpHtmlNodeCOLGROUP.Destroy;
begin
  inherited;
  FWidth.Free;
end;

procedure TIpHtmlNodeCOLGROUP.LoadAndApplyCSSProps;
begin
  inherited;
  if not (FCombinedCSSProps.Alignment in [haDefault, haUnknown]) then
    Align := FCombinedCSSProps.Alignment;
  // wp: what about VAlign?
end;

function TIpHtmlNodeCOLGROUP.GetAlign: TIpHtmlAlign;
begin
  Result := FAlign;
end;

procedure TIpHtmlNodeCOLGROUP.SetAlign(const Value: TIpHtmlAlign);
begin
  FAlign := Value;
end;


{ TIpHtmlNodeFRAME }

procedure TIpHtmlNodeFRAME.SetFrameBorder(const Value: Integer);
begin
  if Value <> FFrameBorder then begin
    FFrameBorder := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeFRAME.SetMarginHeight(const Value: Integer);
begin
  if Value <> FMarginHeight then begin
    FMarginHeight := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeFRAME.SetMarginWidth(const Value: Integer);
begin
  if Value <> FMarginWidth then begin
    FMarginWidth := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeFRAME.SetScrolling(const Value: TIpHtmlFrameScrolling);
begin
  if Value <> FScrolling then begin
    FScrolling := Value;
    InvalidateSize;
  end;
end;


{ TIpHtmlNodeMAP }

constructor TIpHtmlNodeMAP.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  TIpHtmlOpener(Owner).MapList.Add(Self);
end;

destructor TIpHtmlNodeMAP.Destroy;
begin
  TIpHtmlOpener(Owner).MapList.Remove(Self);
  inherited;
end;


{ TIpHtmlNodeTHeadFootBody }

function TIpHtmlNodeTHeadFootBody.GetAlign: TIpHtmlAlign;
begin
  Result := FAlign;
end;

procedure TIpHtmlNodeTHeadFootBody.LoadAndApplyCSSProps;
begin
  inherited;
  if not (FCombinedCSSProps.Alignment in [haDefault, haUnknown]) then
    Align := FCombinedCSSProps.Alignment;
  // wp: what about VAlign?
end;

procedure TIpHtmlNodeTHeadFootBody.SetAlign(const Value: TIpHtmlAlign);
begin
  FAlign := Value;
end;


{ TIpNodeTR }

constructor TIpHtmlNodeTR.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  ElementName := 'tr';
  FAlign := haDefault;
  FValign := hvaMiddle;
  FBgColor := clNone;
  FTextColor := clNone;
end;

function TIpHtmlNodeTR.GetAlign: TIpHtmlAlign;
begin
  Result := FAlign;
end;

procedure TIpHtmlNodeTR.LoadAndApplyCSSProps;
begin
  inherited;
  if Assigned(FCombinedCSSProps) then begin
    if not (FCombinedCSSProps.Alignment in [haDefault, haUnknown]) then
      Align := FCombinedCSSProps.Alignment;
    if FCombinedCSSProps.BgColor <> clNone then
      BgColor := FCombinedCSSProps.BGColor;
    // wp: what about VAlign?
  end;
end;

procedure TIpHtmlNodeTR.SetAlign(const Value: TIpHtmlAlign);
begin
  FAlign := Value;
end;

procedure TIpHtmlNodeTR.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.FontColor := TextColor;
  Props.BgColor := BgColor;
  inherited SetProps(Props);
end;

procedure TIpHtmlNodeTR.AppendSelection(var S: String; var Completed: Boolean);
var
  prev: TIpHtmlNode;
begin
  if Completed then
    exit;
  prev := GetPrevSiblingNode(Self);
  if prev is TIpHtmlNodeTR then S := S + LineEnding;
  inherited AppendSelection(S, Completed);
end;

procedure TIpHtmlNodeTR.SetBgColor(const AValue: TColor);
begin
  if AValue <> FBgColor then begin
    FBgColor := AValue;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeTR.SetTextColor(const AValue: TColor);
begin
  if AValue <> FTextColor then begin
    FTextColor := AValue;
    InvalidateSize;
  end;
end;


{-------------------------------------------------------------------------------
                   Descendants of TIpHtmlNodeTHeadFootBody
-------------------------------------------------------------------------------}

{ TIpHtmlNodeTBODY }

constructor TIpHtmlNodeTBODY.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  ElementName := 'tbody';
  FVAlign := hva3Middle;
end;


{ TIpHtmlNodeTHEAD }

constructor TIpHtmlNodeTHEAD.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  ElementName := 'thead';
  FVAlign := hva3Middle;
end;


{-------------------------------------------------------------------------------
                     Descendants of TIpHtmlNodeBlock
-------------------------------------------------------------------------------}

{ TIpHtmlNodeCAPTION }

constructor TIpHtmlNodeCAPTION.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  ElementName := 'caption';
end;


{ TIpHtmlNodeTableHeaderOrCell }

constructor TIpHtmlNodeTableHeaderOrCell.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode, TableElemLayouterClass);
  FRowSpan := 1;
  FColSpan := 1;
  FAlign := haDefault;
  FVAlign := hva3Middle;
  BgColor := clNone;
end;

destructor TIpHtmlNodeTableHeaderOrCell.Destroy;
begin
  FWidth.Free;
  FHeight.Free;
  inherited;
end;

procedure TIpHtmlNodeTableHeaderOrCell.AppendSelection(var S: String;
  var Completed: Boolean);
var
  prev: TIpHtmlNode;
begin
  if Completed then
    exit;
  prev := GetPrevSiblingNode(self);
  if prev is TIpHtmlNodeTableHeaderOrCell then S := S + #9;
  inherited AppendSelection(S, Completed);
end;

procedure TIpHtmlNodeTableHeaderOrCell.CalcMinMaxPropWidth(RenderProps: TIpHtmlProps;
  var Min, Max: Integer);
begin
  FLayouter.CalcMinMaxPropWidth(RenderProps, Min, Max);
end;

function TIpHtmlNodeTableHeaderOrCell.GetAlign: TIpHtmlAlign;
begin
  Result := FAlign;
end;

procedure TIpHtmlNodeTableHeaderOrCell.Render(RenderProps: TIpHtmlProps);
begin
  FLayouter.Render(RenderProps);
end;

procedure TIpHtmlNodeTableHeaderOrCell.SetAlign(const Value: TIpHtmlAlign);
begin
  FAlign := Value;
end;

procedure TIpHtmlNodeTableHeaderOrCell.Layout(RenderProps: TIpHtmlProps;
  const TargetRect: TRect);
begin
  FLayouter.Layout(Props, TargetRect);
end;

procedure TIpHtmlNodeTableHeaderOrCell.LoadAndApplyCSSProps;
begin
  inherited;
  if not (FCombinedCSSProps.Alignment in [haDefault, haUnknown]) then
    Align := FCombinedCSSProps.Alignment;
  // wp: what about VAlign?
end;

procedure TIpHtmlNodeTableHeaderOrCell.DimChanged(Sender: TObject);
begin
  InvalidateSize;
end;


{-------------------------------------------------------------------------------
                   Descendants of TIpHtmlNodeTableHeaderOrCell
-------------------------------------------------------------------------------}

{ TIpHtmlNodeTH }

constructor TIpHtmlNodeTH.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  ElementName := 'th';
end;


{ TIpHtmlNodeTD }

constructor TIpHtmlNodeTD.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  ElementName := 'td';
end;

                        
{-------------------------------------------------------------------------------
                         Descendants of TIpHtmlNodeInline 
-------------------------------------------------------------------------------}

{ TIpHtmlNodeA }

constructor TIpHtmlNodeA.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  ElementName := 'a';
  MapAreaList := TFPList.Create;
end;

destructor TIpHtmlNodeA.Destroy;
begin
  if HasRef then
    TIpHtmlOpener(Owner).AnchorList.Remove(Self);
  inherited;
  MapAreaList.Free;
end;

procedure TIpHtmlNodeA.AddMapArea(const R: TRect);
var
  RCopy : PRect;
  c : Integer;
begin
  c := MapAreaList.Count;
  if c > 0 then begin
    RCopy := PRect(FAreaList[c-1]);
    if (R.Left = RCopy.Right)
    and (R.Top = RCopy.Top)
    and (R.Bottom = RCopy.Bottom) then begin
      RCopy.Right := R.Right;
      Exit;
    end;
  end;
  New(RCopy);
  RCopy^  := R;
  MapAreaList.Add(RCopy);
end;

procedure TIpHtmlNodeA.ClearAreaList;
var
  m: Pointer;
begin
  inherited;
  while MapAreaList.Count > 0 do begin
    m:=MapAreaList[0];
    FreeMem(m);
    MapAreaList.Delete(0);
  end;
end;

procedure TIpHtmlNodeA.BuildAreaList;
var
  i : Integer;
begin
  inherited;
  for i := 0 to Pred(ChildCount) do
    TIpHtmlNodeOpener(ChildNode[i]).ReportMapRects(AddMapArea);
end;

function TIpHtmlNodeA.PtInRects(const P: TPoint): Boolean;
var
  i : Integer;
begin
  if FAreaList.Count = 0 then
    BuildAreaList;
  for i := 0 to Pred(FAreaList.Count) do begin
    with PRect(FAreaList[i])^ do
    if PtInRect(PRect(FAreaList[i])^,P) then begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TIpHtmlNodeA.RelMapPoint(const P: TPoint): TPoint;
var
  i : Integer;
begin
  if FAreaList.Count = 0 then
    BuildAreaList;
  for i := 0 to Pred(MapAreaList.Count) do begin
    with PRect(MapAreaList[i])^ do
    if PtInRect(PRect(FAreaList[i])^,P) then begin
      Result := Point(
        P.x - PRect(FAreaList[i])^.Left,
        P.y - PRect(FAreaList[i])^.Top);
      Exit;
    end;
  end;
  Result := Point(-1, -1);
end;

procedure TIpHtmlNodeA.SetHot(const Value: Boolean);
var
  i : Integer;
  R : TRect;
begin
  FHot := Value;
  if FAreaList.Count = 0 then
    BuildAreaList;
  if FOwner.NeedResize then
    SetProps(Props);
  for i := 0 to Pred(FAreaList.Count) do
    if PageRectToScreen(PRect(FAreaList[i])^, R) then
      TIpHtmlOpener(Owner).InvalidateRect(R);
end;

procedure TIpHtmlNodeA.SetHRef(const Value: string);
var
  NewHasRef : Boolean;
begin
  FHRef := Value;
  NewHasRef := Value <> '';
  if NewHasRef <> HasRef then begin
    if HasRef then
      TIpHtmlOpener(Owner).AnchorList.Remove(Self)
    else
      TIpHtmlOpener(Owner).AnchorList.Add(Self);
    FHasRef := NewHasRef;
  end;
end;

procedure TIpHtmlNodeA.DoOnBlur;
begin
  {FHasFocus := False;}
  Hot := False;
end;

procedure TIpHtmlNodeA.DoOnFocus;
begin
  {FHasFocus := True;}
  MakeVisible;
  Hot := True;
end;

procedure TIpHtmlNodeA.SetName(const Value: string);
begin
  if FName <> '' then
    with TIpHtmlOpener(Owner).NameList do
      Delete(IndexOf(FName));
  FName := Value;
  if FName <> '' then
    TIpHtmlOpener(Owner).NameList.AddObject(FName, Self);
end;
  (*
procedure TIpHtmlNodeA.MakeVisible;
var
  i : Integer;
  R : TRect;
begin
  if AreaList.Count = 0 then
    BuildAreaList;
  SetRectEmpty(R);
  for i := 0 to Pred(AreaList.Count) do
    UnionRect(R, R, PRect(AreaList[i])^);

  Owner.MakeVisible(R, true );
  //Owner.MakeVisible(R, False);  // original
end;
    *)
procedure TIpHtmlNodeA.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.DelayCache:=True;
  if FHot then begin
    Props.FontColor := Props.LinkColor;
    Props.FontStyle := Props.FontStyle + [fsUnderline];
  end else
    if HasRef then begin
      if TIpHtmlOpener(Owner).LinksUnderlined then
        Props.FontStyle := Props.FontStyle + [fsUnderline]
      else
        Props.FontStyle := Props.FontStyle - [fsUnderline];
      if TIpHtmlOpener(Owner).LinkVisited(HRef) then
        Props.FontColor := Props.VLinkColor
      else
        Props.FontColor := Props.LinkColor;
    end;
  Props.DelayCache:=False;
  inherited SetProps(Props);
end;

function TIpHtmlNodeA.GetHint: string;
begin
  if Title = '' then
    Result := HRef
  else
    Result := Title;
end;


{ TIpHtmlNodeAPPLET }

destructor TIpHtmlNodeAPPLET.Destroy;
begin
  inherited;
  FWidth.Free;
end;

function TIpHtmlNodeAPPLET.GetHint: string;
begin
  Result := Alt;
end;

procedure TIpHtmlNodeAPPLET.WidthChanged(Sender: TObject);
begin
  InvalidateSize;
end;


{ TIpHtmlNodeBLOCKQUOTE }

procedure TIpHtmlNodeBLOCKQUOTE.Enqueue;
begin
  EnqueueElement(TIpHtmlOpener(Owner).FLIndent);
  inherited;
  EnqueueElement(TIpHtmlOpener(Owner).FLOutdent);
end;


{ TIpHtmlNodeBR }

constructor TIpHtmlNodeBR.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  ElementName := 'br';
end;

procedure TIpHtmlNodeBR.Enqueue;
var
  h: Integer;
  elem: PIpHtmlElement;
begin
  h := 0;
  (*      // wp: is there any reason why h should be > 0 for other node types?
  if (ParentNode is TIpHtmlNodeP) or
     (ParentNode is TIpHtmlNodeDIV) or
     (ParentNode is TIpHtmlNodeLI) or
//     (ParentNode is TIpHtmlNodePRE) or
     (ParentNode is TIpHtmlNodeHeader) or
     (ParentNode is TIpHtmlNodeBody)
  then
    h := 0
  else
    h := Props.FontSize;
  *)

  case Clear of
    hbcNone :
      begin
        elem := TIpHtmlOpener(Owner).BuildLinefeedEntry(etHardLF, h);
        EnqueueElement(elem);
      end;
    hbcLeft :
      EnqueueElement(TIpHtmlOpener(Owner).HardLFClearLeft);
    hbcRight :
      EnqueueElement(TIpHtmlOpener(Owner).HardLFClearRight);
    hbcAll :
      EnqueueElement(TIpHtmlOpener(Owner).HardLFClearBoth);
  end;
end;

function TIpHtmlNodeBR.GetMargin(AMargin: TIpHtmlElemMargin; ADefault: Integer): Integer;
var
  default: Integer;
begin
  if (ParentNode is TIpHtmlNodeP) then
    default := 0
  else
    default := ADefault;
  Result := inherited GetMargin(AMargin, default);
end;

procedure TIpHtmlNodeBR.SetClear(const Value: TIpHtmlBreakClear);
begin
  FClear := Value;
  InvalidateSize;
end;


{ TIpHtmlNodeDD }

constructor TIpHtmlNodeDD.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  ElementName := 'dd';
end;

procedure TIpHtmlNodeDD.Enqueue;
var
  h, hf: Integer;
  elem: PIpHtmlElement;
begin
  hf := Props.FontSize;
  if ChildCount > 0 then begin
    h := GetMargin(Props.ElemMarginTop, hf div 2);
    elem := TIpHtmlOpener(Owner).BuildLineFeedEntry(etSoftLF, h);
    EnqueueElement(elem);
  end;

  EnqueueElement(TIpHtmlOpener(Owner).FLIndent);
  inherited;
  EnqueueElement(TIpHtmlOpener(Owner).FLOutdent);

  if ChildCount > 0 then begin
    h := GetMargin(Props.ElemMarginTop, hf);
    elem := TIpHtmlOpener(Owner).BuildLineFeedEntry(etSoftLF, h);
    EnqueueElement(elem);
  end;
end;


{ TIpHtmlNodeDIV }

constructor TIpHtmlNodeDIV.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  ElementName := 'div';
end;

destructor TIpHtmlNodeDIV.Destroy;
begin
  inherited;
end;

function TIpHtmlNodeDIV.GetAlign: TIpHtmlAlign;
begin
  Result := FAlign;
end;

procedure TIpHtmlNodeDIV.LoadAndApplyCSSProps;
begin
  inherited;
  if not (FCombinedCSSProps.Alignment in [haDefault, haUnknown]) then
    Align := FCombinedCSSProps.Alignment;
end;

procedure TIpHtmlNodeDIV.SetAlign(const Value: TIpHtmlAlign);
begin
  FAlign := Value;
end;

procedure TIpHtmlNodeDIV.SetProps(const RenderProps: TIpHtmlProps);
var
  bgCol: TColor;
begin
  bgCol := Props.BgColor;
  Props.Assign(RenderProps);
  Props.Alignment := Align;
  LoadAndApplyCSSProps;
  if FParentNode = FOwner.Body then
    Props.BgColor := bgCol;
  inherited SetProps(Props);
end;

procedure TIpHtmlNodeDIV.Enqueue;
var
  elem: PIpHtmlElement;
  h: Integer;
begin
  //hf := Props.FontSize;
  if ChildCount > 0 then begin
    h := GetMargin(Props.ElemMarginTop, 0); //hf div 4);
    elem := TIpHtmlOpener(Owner).BuildLinefeedEntry(etSoftLF, h);
    EnqueueElement(elem);
  end;
  inherited Enqueue;
  if ChildCount > 0 then begin
    h := GetMargin(Props.ElemMarginBottom, 0);  //hf div 4);
    elem := TIpHtmlOpener(Owner).BuildLinefeedEntry(etSoftLF, h);
    EnqueueElement(elem);
  end;
end;
(* this is the original code
begin
  if FChildren.Count > 0 then begin
    if Props.ElemMarginTop.Style=hemsAuto then
      EnqueueElement(Owner.HardLF)
    else begin
      // ToDo: Props.ElemMarginTop
      EnqueueElement(Owner.HardLFClearBoth);
    end;
  end;
  inherited Enqueue;
  if FChildren.Count > 0 then begin
    if Props.ElemMarginTop.Style=hemsAuto then
      EnqueueElement(Owner.HardLF)
    else begin
      // ToDo: Props.ElemMarginTop
      EnqueueElement(Owner.HardLFClearBoth)
    end;
  end;
end;
*)


{ TIpHtmlNodeDL }

constructor TIpHtmlNodeDL.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  ElementName := 'dl';
end;

procedure TIpHtmlNodeDL.Enqueue;
begin
  EnqueueElement(TIpHtmlOpener(Owner).HardLF);
  EnqueueElement(TIpHtmlOpener(Owner).FLIndent);
  inherited;
  EnqueueElement(TIpHtmlOpener(Owner).FLOutdent);
end;


{ TIpHtmlNodeDT }

constructor TIpHtmlNodeDT.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  ElementName := 'dt';
end;

procedure TIpHtmlNodeDT.Enqueue;
begin
  inherited;
  EnqueueElement(TIpHtmlOpener(Owner).HardLF);
end;


{ TIpHtmlNodeFORM }

constructor TIpHtmlNodeFORM.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  ElementName := 'form';
end;

destructor TIpHtmlNodeFORM.Destroy;
begin
  inherited;
end;

procedure TIpHtmlNodeFORM.AddChild(Node: TIpHtmlNode; const UserData: Pointer);
begin
  if Node is TIpHtmlNodeControl then
    if TIpHtmlNodeControl(Node).SuccessFul then
      TFPList(UserData).Add(Node);
end;

{$IFNDEF HtmlWithoutHttp}
procedure TIpHtmlNodeFORM.SubmitForm;
var
  CList : TFPList;
  FList,
  VList : TStringList;
  URLData: string;
  FormData: TIpFormDataEntity;

  procedure IndentifySuccessfulControls;
  begin
    EnumChildren(AddChild, CList);
  end;

  procedure BuildDataset;
  var
    i : Integer;
  begin
    for i := 0 to Pred(CList.Count) do
      with TIpHtmlNodeControl(CList[i]) do
        AddValues(FList, VList);
  end;

  procedure URLEncodeDataset;

    function Escape(const S: string): string;
    var
      i : Integer;
    begin
      Result := '';
      for i := 1 to length(S) do
        case S[i] of
        #0..#31, '+', '&', '%', '=' :
          Result := Result + '%'+IntToHex(ord(S[i]),2);
        ' ' :
          Result := Result + '+';
        else
          Result := Result + S[i];
        end;
    end;

  var
    i : Integer;
  begin
    URLData := '';
    for i := 0 to Pred(FList.Count) do begin
      if URLData <> '' then
        URLData := URLData + '&';
      URLData := URLData +
        Escape(FList[i]) +
        '=' +
        Escape(VList[i]);
    end;
  end;

  procedure MimeEncodeDataset;
  var
    i : Integer;
  begin
    FormData := TIpFormDataEntity.Create(nil);
    for i := 0 to Pred(FList.Count) do
      if LazStartsStr('file://', VList[i]) then
        FormData.AddFile(copy(VList[i], 8, length(VList[i])),
          Accept, 'plain', embinary)
      else
        FormData.AddFormData(FList[i], VList[i]);
  end;

  procedure SubmitDataset;
  begin
    case Method of
    hfmGet :
      TIpHtmlOpener(Owner).Get(Action + '?' + URLData);
    hfmPost :
      begin
        TIpHtmlOpener(Owner).Post(Action, FormData);
        {The Formdata object will be freed by the post logic,
         which is called asynchroneously via PostMessage.
         Clear the pointer to prevent our finalization
         section from stepping on it prematurely.}
        FormData := nil;
      end;
    end;
  end;

begin
  FormData := nil;
  CList := nil;
  FList := nil;
  VList := nil;
  try
    CList := TFPList.Create;
    FList := TStringList.Create;
    VList := TStringList.Create;
    IndentifySuccessfulControls;
    BuildDataset;
    case Method of
    hfmGet :
      URLEncodeDataset;
    else
      MimeEncodeDataset;
    end;
    SubmitDataset;
  finally
    FormData.Free;
    CList.Free;
    FList.Free;
    VList.Free;
  end;
end;

procedure TIpHtmlNodeFORM.SubmitRequest;
begin
  SubmitForm;
end;
{$ENDIF}

procedure TIpHtmlNodeFORM.ResetRequest;
begin
  ResetForm;
end;

procedure TIpHtmlNodeFORM.ResetControl(Node: TIpHtmlNode; const UserData: Pointer);
begin
  if Node is TIpHtmlNodeControl then
    TIpHtmlNodeControl(Node).Reset;
end;

procedure TIpHtmlNodeFORM.ResetForm;
begin
  EnumChildren(ResetControl, nil);
end;
 

{ TIpHtmlNodeGenInline }

constructor TIpHtmlNodeGenInline.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  Props := TIpHtmlProps.Create(TIpHtmlOpener(Owner).PropACache, TIpHtmlOpener(Owner).PropBCache);
end;

destructor TIpHtmlNodeGenInline.Destroy;
begin
  Props.Free;
  inherited;
end;

procedure TIpHtmlNodeGenInline.SetProps(const RenderProps: TIpHtmlProps);
begin
  ApplyProps(RenderProps);
  inherited SetProps(Props);
end;


{ TIpHtmlNodeLABEL }

constructor TIpHtmlNodeLABEL.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  TIpHtmlOpener(Owner).FControlList.Add(Self);
end;

destructor TIpHtmlNodeLABEL.Destroy;
begin
  TIpHtmlOpener(Owner).FControlList.Remove(Self);
  inherited;
end;


{ TIpHtmlNodeList }

procedure TIpHtmlNodeList.Enqueue;
var
  i: Integer;
  lOwner: TIpHtmlOpener;
  lParentNode: TIpHtmlNodeOpener;
begin
  lOwner := TIpHtmlOpener(Owner);
  lParentNode := TIpHtmlNodeOpener(FParentNode);
  
  if ChildCount > 0 then begin
    EnqueueElement(lOwner.SoftLF);
  end;
  
  {render list}
  lParentNode.EnqueueElement(lOwner.FLIndent);
  for i := 0 to Pred(ChildCount) do
    if ChildNode[i] is TIpHtmlNodeLI then begin
      TIpHtmlNodeLI(ChildNode[i]).Enqueue;
      lParentNode.EnqueueElement(lOwner.SoftLF);
    end else
      TIpHtmlNodeOpener(ChildNode[i]).Enqueue;
  lParentNode.EnqueueElement(lOwner.FLOutdent);
  EnqueueElement(lOwner.SoftLF);
end;

procedure TIpHtmlNodeList.LoadAndApplyCSSProps;
var
  i: Integer;
begin
  inherited;
  if FCombinedCSSProps <> nil then
    case FCombinedCSSProps.ListType of
      ltULCircle: FListType := ulCircle;
      ltULDisc: FListType := ulDisc;
      ltULSquare: FListType := ulSquare;
    end;
  for i := 0 to ChildCount-1 do
    if ChildNode[i] is TIpHtmlNodeLI then
      if TIpHtmlNodeLI(ChildNode[i]).ListType = ulUndefined then
        TIpHtmlNodeLI(ChildNode[i]).ListType := FListType
end;  

procedure TIpHtmlNodeList.SetListType(const Value: TIpHtmlULType);
begin
  if Value <> FListType then begin
    FListType := Value;
    InvalidateSize;
  end;
end;


{ TIpHtmlNodeOBJECT }

destructor TIpHtmlNodeOBJECT.Destroy;
begin
  inherited;
  FWidth.Free;
end;

procedure TIpHtmlNodeOBJECT.WidthChanged(Sender: TObject);
begin
  InvalidateSize;
end;


{ TIpHtmlNodePRE }

constructor TIpHtmlNodePRE.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  ElementName := 'pre';
end;

destructor TIpHtmlNodePRE.Destroy;
begin
  inherited;
end;

procedure TIpHtmlNodePRE.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.DelayCache:=True;
  Props.Preformatted := True;
  Props.FontName := Owner.FixedTypeface;
  Props.FontSize := Props.FontSize - 2;
  Props.DelayCache:=False;
  inherited SetProps(Props);
end;

procedure TIpHtmlNodePRE.Enqueue;
var
  h: Integer;
  elem: PIpHtmlElement;
begin
  //hf := Props.FontSize;
  if ChildCount > 0 then begin
    h := GetMargin(Props.ElemMarginTop, 0);
    elem := TIpHtmlOpener(Owner).BuildLineFeedEntry(etSoftLF, h);
    EnqueueElement(elem);
  end;
  //EnqueueElement(Owner.HardLF);
  inherited Enqueue;
  if ChildCount > 0 then begin
    h := GetMargin(Props.ElemMarginTop, 0);
    elem := TIpHtmlOpener(Owner).BuildLineFeedEntry(etSoftLF, h);
    EnqueueElement(elem);
  end;

  {
  if FChildren.Count > 0 then
    EnqueueElement(Owner.HardLF);
    }
end;


{-------------------------------------------------------------------------------
                   Descendants of TIpHtmlNodeAlignInline 
-------------------------------------------------------------------------------}

{ TIpHtmlNodeHR }

constructor TIpHtmlNodeHR.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  FColor := clNone;
  Align := hiaCenter;
  SizeWidth := TIpHtmlPixels.Create;
end;

destructor TIpHtmlNodeHR.Destroy;
begin
  inherited;
  FWidth.Free;
  SizeWidth.Free;
  FSize.Free;
end;

procedure TIpHtmlNodeHR.CalcMinMaxWidth(var Min, Max: Integer);
begin
  Min := 0;
  Max := 0;
  case Width.LengthType of
  hlAbsolute :
    begin
      Min := Width.LengthValue;
      Max := Min;
    end;
  end;
end;

procedure TIpHtmlNodeHR.Draw;
var
  R : TRect;
  TopLeft : TPoint;
  Dim : TSize;
  SaveBrushColor,
  SavePenColor : TColor;
  aCanvas: TCanvas;
begin
  aCanvas := Owner.Target;
  TopLeft := GrossDrawRect.TopLeft;
  R.TopLeft := TopLeft;
  Dim := GetDim(0);
  R.Right := TopLeft.x + Dim.cx;
  R.Bottom := TopLeft.y + Dim.cy;
  if not PageRectToScreen(R, R) then
    Exit;
  if NoShade or (Color <> clNone) then begin
    SavePenColor := aCanvas.Pen.Color;
    SaveBrushColor := aCanvas.Brush.Color;
    if Color = clNone then begin
      aCanvas.Pen.Color := clBlack;
      aCanvas.Brush.Color := clBlack;
    end else begin
      aCanvas.Pen.Color := Color;
      aCanvas.Brush.Color := Color;
    end;
    aCanvas.FillRect(R);
    aCanvas.Pen.Color := SavePenColor;
    aCanvas.Brush.Color := SaveBrushColor;
  end else begin
    SavePenColor := aCanvas.Pen.Color;
    SaveBrushColor := aCanvas.Brush.Color;
    aCanvas.Pen.Color := clGray;
    aCanvas.Brush.Color := clGray;
    aCanvas.FillRect(R);
    aCanvas.Pen.Color := clWhite;
    aCanvas.MoveTo(R.Left - 1, R.Bottom + 1);
    aCanvas.LineTo(R.Left - 1, R.Top - 1);
    aCanvas.LineTo(R.Right + 1, R.Top - 1);
    aCanvas.Pen.Color := clBlack;
    aCanvas.LineTo(R.Right + 1, R.Bottom + 1);
    aCanvas.LineTo(R.Left - 1, R.Bottom + 1);
    aCanvas.Pen.Color := SavePenColor;
    aCanvas.Brush.Color := SaveBrushColor;
  end;
end;

procedure TIpHtmlNodeHR.Enqueue;
begin
  EnqueueElement(TIpHtmlOpener(Owner).SoftLF);
  inherited;
  EnqueueElement(TIpHtmlOpener(Owner).SoftLF);
end;

function TIpHtmlNodeHR.GetDim(ParentWidth: Integer): TSize;
begin
  if (SizeWidth.PixelsType <> hpAbsolute)
  or ((ParentWidth <> 0) and (SizeWidth.Value <> ParentWidth)) then begin
    case Width.LengthType of
    hlUndefined :
      FDim.cx := 0;
    hlAbsolute :
      FDim.cx := Width.LengthValue;
    hlPercent :
      FDim.cx := round(ParentWidth * Width.LengthValue / 100);
    end;
    FDim.cy := Size.Value;
    SizeWidth.Value := ParentWidth;
    SizeWidth.PixelsType := hpAbsolute;
  end;
  Result := FDim;
end;

function TIpHtmlNodeHR.GrossDrawRect: TRect;
begin
  Result := PIpHtmlElement(Element).WordRect2;
end;

procedure TIpHtmlNodeHR.WidthChanged(Sender: TObject);
begin
  InvalidateSize;
end;


{ TIpHtmlNodeIMG }

constructor TIpHtmlNodeIMG.Create;
begin
  inherited;
  ElementName := 'img';
  SizeWidth := TIpHtmlPixels.Create;
end;

destructor TIpHtmlNodeIMG.Destroy;
begin
  UnloadImage;
  UseMap := '';
  inherited;
  FWidth.Free;
  SizeWidth.Free;
  FHeight.Free;
end;

procedure TIpHtmlNodeIMG.LoadImage;
var
  lOwner: TIpHtmlOpener;
begin
  lOwner := TIpHtmlOpener(Owner);
  
  if Src <> '' then begin
    if FPicture <> lOwner.DefaultImage then begin
      FPicture.Free;
      FPicture := nil;
    end;
    lOwner.DoGetImage(Self, lOwner.BuildPath(Src), FPicture);
    if FPicture = nil then
      FPicture := lOwner.DefaultImage;

    {$IFDEF UseGifImageUnit}
    if (FPicture <> nil) and (FPicture.Graphic <> nil) and (FPicture.Graphic is TGifImage) then
      lOwner.GifImages.Add(Self);
    {$ELSE}
    if (FPicture <> nil) and (FPicture.Graphic <> nil) and (FPicture.Graphic is TIpAnimatedGraphic) then
      lOwner.AnimationFrames.Add(Self);
    {$ENDIF}
  end;
end;

procedure TIpHtmlNodeIMG.UnloadImage;
var
  lOwner: TIpHtmlOpener;
begin
  lOwner := TIpHtmlOpener(Owner);
  
  {$IFDEF UseGifImageUnit}
  if (FPicture <> nil) and (FPicture.Graphic <> nil) and (FPicture.Graphic is TGifImage) then
    Owner.GifImages.Remove(Self);
  {$ELSE}
  if (FPicture <> nil) and (FPicture.Graphic <> nil) and (FPicture.Graphic is TIpAnimatedGraphic) then
    lOwner.AnimationFrames.Remove(Self);
  {$ENDIF}
  if FPicture <> lOwner.DefaultImage then begin
    FPicture.Free;
    FPicture := nil;
  end;
end;

function TIpHtmlNodeIMG.GetBorder: Integer;
begin
  if (FPicture <> nil) and (FPicture.Graphic = nil) then
    Result := 1
  else
    Result := fBorder;
end;

procedure TIpHtmlNodeIMG.Draw;
var
  R: TRect;
  TopLeft: TPoint;
  Dim: TSize;
begin
  if FPicture = nil then
    LoadImage;

  if (FPicture <> nil) and (FPicture.Graphic = nil) then
    LoadImage;
  Owner.AddRect(GrossDrawRect, Element, Block);
  TopLeft := GrossDrawRect.TopLeft;
  R.TopLeft := TopLeft;
  Dim := GetDim(0);
  R.Right := TopLeft.x + Dim.cx;
  R.Bottom := TopLeft.y + Dim.cy;
  
  if Border <> 0 then begin
    if Border = 1 then begin
      ScreenLine(
        R.TopLeft,
        Point(R.Right, R.Top),
        1,
        RGB(220,220,220));
      ScreenLine(
        R.BottomRight,
        Point(R.Left, R.Bottom),
        1,
        RGB(64,64,64));
      ScreenLine(
        R.TopLeft,
        Point(R.Left, R.Bottom),
        1,
        RGB(192,192,192));
      ScreenLine(
        R.BottomRight,
        Point(R.Right, R.Top),
        1,
        RGB(128,128,128));
    end else begin
      ScreenPolygon(
        [R.TopLeft,
        Point(R.Right - 1, R.Top),
        Point(R.Right - Border, R.Top + Border - 1),
        Point(R.Left + Border - 1, R.Top + Border - 1)],
        RGB(220,220,220));
      ScreenPolygon(
        [
        Point(R.Right - 1, R.Bottom - 1),
        Point(R.Right - Border, R.Bottom - Border),
        Point(R.Left + (Border - 1), R.Bottom - Border),
        Point(R.Left, R.Bottom - 1)],
          RGB(64,64,64));
      ScreenPolygon(
        [R.TopLeft,
        Point(R.Left, R.Bottom - 1),
        Point(R.Left + (Border - 1), R.Bottom - Border),
        Point(R.Left + (Border - 1), R.Top + (Border - 1))],
        RGB(192,192,192));
      ScreenPolygon(
        [
        Point(R.Right - 1, R.Bottom - 1),
        Point(R.Right - 1, R.Top),
        Point(R.Right - Border, R.Top + (Border - 1)),
        Point(R.Right - Border, R.Bottom - Border)],
        RGB(128,128,128));
    end;
    InflateRect(R, -Border, -Border);
  end;

  InflateRect(R, -HSpace, -VSpace);

  if FPicture <> nil then begin
    if FPicture.Graphic=nil then begin
      if PageRectToScreen(R,R) then
        Owner.Target.TextRect(R, R.Left, R.Top, GetHint);
      Exit;
    end;
    FPicture.Graphic.Transparent := True;
    NetDrawRect := R;
    if PageRectToScreen(R, R) then begin
        {$IFDEF UseGifImageUnit}
        if (FPicture.Graphic is TGifImage) and (TGifImage(FPicture.Graphic).Images.Count > 1) then 
        begin
          with TGifImage(FPicture.Graphic) do
            DrawOptions := DrawOptions + [goDirectDraw];
          TIpHtmlOpener(Owner).AddGifQueue(FPicture.Graphic, R);
        end else
        {$ELSE}
        if (FPicture.Graphic is TIpAnimatedGraphic) and (TIpAnimatedGraphic(FPicture.Graphic).Images.Count > 1) then 
        begin
          TIpAnimatedGraphic(FPicture.Graphic).AggressiveDrawing := True;
          TIpHtmlOpener(Owner).AddGifQueue(FPicture.Graphic, R);
        end else
      begin
        {$ENDIF}
        if FPicture = TIpHtmlOpener(Owner).DefaultImage then begin
          if (NetDrawRect.Right - NetDrawRect.Left > FPicture.Graphic.Width) and
             (NetDrawRect.Bottom - NetDrawRect.Top > FPicture.Graphic.Height) then 
          begin
            Owner.Target.Brush.Color := Props.FontColor;
            Owner.Target.FrameRect(R);
            Owner.Target.Draw(R.Left + 1, R.Top + 1, FPicture.Graphic);
          end else
            Owner.Target.StretchDraw(R, FPicture.Graphic);
        end else
          Owner.Target.StretchDraw(R, FPicture.Graphic);
      end;
    end;
  end
end;

function TIpHtmlNodeIMG.GrossDrawRect : TRect;
begin
  Result := PIpHtmlElement(Element).WordRect2;
end;

procedure TIpHtmlNodeIMG.ReportDrawRects(M: TRectMethod);
begin
  M(GrossDrawRect);
end;

procedure TIpHtmlNodeIMG.ReportMapRects(M: TRectMethod);
begin
  if IsMap then
    M(GrossDrawRect);
end;

procedure TIpHtmlNodeIMG.ImageChange(NewPicture: TPicture);
var
  OldDim, Dim: TSize;
  lOwner: TIpHtmlOpener;
begin
  lOwner := TIpHtmlOpener(Owner);
  
  {$IFOPT C+}
  Owner.CheckImage(NewPicture);
  {$ENDIF}
  OldDim := GetDim(-1);
  
  {$IFDEF UseGifImageUnit}
  if (FPicture <> nil) and (FPicture.Graphic <> nil) and (FPicture.Graphic is TGifImage) then
    lOwner.GifImages.Remove(Self);
  {$ELSE}
  if (FPicture <> nil) and (FPicture.Graphic <> nil) and (FPicture.Graphic is TIpAnimatedGraphic) then
    lOwner.AnimationFrames.Remove(Self);
  {$ENDIF}
  
  if FPicture <> lOwner.DefaultImage then
    FPicture.Free;
  FPicture := NewPicture;
  
  {$IFDEF UseGifImageUnit}
  if (FPicture <> nil) and (FPicture.Graphic <> nil) and (FPicture.Graphic is TGifImage) then
    lOwner.GifImages.Add(Self);
  {$ELSE}
  if (FPicture <> nil) and (FPicture.Graphic <> nil) and (FPicture.Graphic is TIpAnimatedGraphic) then
    lOwner.AnimationFrames.Add(Self);
  {$ENDIF}
  
  SizeWidth.PixelsType := hpUndefined;
  Dim := GetDim(0);
  if (Dim.cx <> OldDim.cx) or (Dim.cy <> OldDim.cy) then
    InvalidateSize
  else
    Invalidate;
end;

procedure TIpHtmlNodeIMG.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
end;

function TIpHtmlNodeIMG.GetDim(ParentWidth: Integer): TSize;
var
  DimKnown, NoLoad : Boolean;
begin
  if ParentWidth < 0 then begin
    NoLoad := True;
    ParentWidth := 0;
  end else
    NoLoad := False;
  if (SizeWidth.PixelsType <> hpAbsolute)
  or ((ParentWidth <> 0) and (SizeWidth.Value <> ParentWidth)) then begin
    DimKnown := True;
    if (Height.PixelsType <> hpUndefined)
    and (Width.LengthType <> hlUndefined) then begin
      case Width.LengthType of
      hlUndefined :
        DimKnown := False;
      hlAbsolute :
        begin
          FSize := SizeRec(Width.LengthValue, Height.Value);
        end;
      hlPercent :
        begin
          FSize := SizeRec(
            round(ParentWidth * Width.LengthValue / 100) - 2*HSpace - 2*Border,
            Height.Value);
        end;
      end;
    end else
      DimKnown := False;
    if not DimKnown then begin
      if (FPicture <> nil) then begin
        if FPicture.Graphic=nil then
          // todo: needs to return the "text size" of GetHint
          FSize := SizeRec(100,20)
        else
        if ScaleBitmaps then
          FSize := SizeRec(round(FPicture.Width * Aspect), round(FPicture.Height * Aspect))
        else
          FSize := SizeRec(FPicture.Width, FPicture.Height)
      end else begin
        if NoLoad then
          FSize := SizeRec(0, 0)
        else begin
          LoadImage;
          if FPicture <> nil then begin
            if ScaleBitmaps then
              FSize := SizeRec(round(FPicture.Width * Aspect), round(FPicture.Height * Aspect))
            else
              if FPicture.Graphic=nil then
                // todo: needs to return the "text size" of GetHint
                FSize := SizeRec(100,20)
              else
              FSize := SizeRec(FPicture.Width, FPicture.Height);
          end else
            FSize := SizeRec(0, 0);
        end;
      end;
      if FPicture <> nil then begin
        case Width.LengthType of
        hlUndefined :;
        hlAbsolute :
          begin
            FSize := SizeRec(Width.LengthValue, FSize.cy);
          end;
        hlPercent :
          begin
            FSize := SizeRec(
              round(ParentWidth * Width.LengthValue / 100) - 2*HSpace - 2*Border,
              FSize.cy);
          end;
        end;
        if Height.PixelsType <> hpUndefined then
          FSize.cy := Height.Value;
      end;
    end;
    FSize := SizeRec(FSize.cx + 2*HSpace + 2*Border, FSize.cy + 2*VSpace + 2*Border);
    SizeWidth.Value := ParentWidth;
    SizeWidth.PixelsType := hpAbsolute;
  end;
  Result := FSize;
end;

procedure TIpHtmlNodeIMG.CalcMinMaxWidth(var Min, Max: Integer);
var
  Dim : TSize;
begin
  Dim := GetDim(0);
  Min := Dim.cx;
  Max := Min;
end;

procedure TIpHtmlNodeIMG.SetUseMap(const Value: string);
var
  lOwner: TIpHtmlOpener;
begin
  lOwner := TIpHtmlOpener(Owner);
  
  if FUseMap <> '' then begin
    lOwner.MapImgList.Remove(Self);
    lOwner.ClearAreaList;
  end;
  FUseMap := Value;
  if FUseMap <> '' then begin
    lOwner.MapImgList.Add(Self);
    lOwner.ClearAreaList;
  end;
end;

function TIpHtmlNodeIMG.GetHint: string;
begin
  Result := Alt;
end;

procedure TIpHtmlNodeIMG.SetBorder(const Value: Integer);
begin
  FBorder := Value;
  InvalidateSize;
end;

procedure TIpHtmlNodeIMG.SetHSpace(const Value: Integer);
begin
  FHSpace := Value;
  InvalidateSize;
end;

procedure TIpHtmlNodeIMG.SetVSpace(const Value: Integer);
begin
  FVSpace := Value;
  InvalidateSize;
end;

procedure TIpHtmlNodeIMG.DimChanged(Sender: TObject);
begin
  InvalidateSize;
end;

procedure TIpHtmlNodeIMG.InvalidateSize;
begin
  inherited;
  SizeWidth.PixelsType := hpUndefined;
end;


{ TIpHtmlNodeLI }

constructor TIpHtmlNodeLI.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  ElementName := 'li';
  Align := hiaBottom;
  WordEntry := TIpHtmlOpener(Owner).NewElement(etWord, Self);
  WordEntry.Props := Props;
end;

procedure TIpHtmlNodeLI.CalcMinMaxWidth(var Min, Max: Integer);
begin
  if ScaleBitmaps then begin
    Min := round(8 * Aspect);
    Max := round(8 * Aspect);
  end else begin
    Min := 8;
    Max := 8;
  end;
end;

procedure TIpHtmlNodeLI.Draw;
var
  R : TRect;
  SaveColor : Tcolor;
begin
  if PageRectToScreen(GrossDrawRect, R) then
  begin
    SaveColor := Owner.Target.Brush.Color;
    case ListType of
      ulDisc :
        begin
          Owner.Target.Brush.Color := Props.FontColor;
          if ScaleBitmaps then
            Owner.Target.Ellipse(R.Left, R.Top, R.Left + round(7 * Aspect), R.Top + round(7 * Aspect))
          else
            Owner.Target.Ellipse(R.Left, R.Top, R.Left + 7, R.Top + 7);
          Owner.Target.Brush.Color := SaveColor;
        end;
      ulSquare :
        begin
          Owner.Target.Brush.Color := Props.FontColor;
          if ScaleBitmaps then
            Owner.Target.Rectangle(R.Left, R.Top, R.Left + round(7 * Aspect), R.Top + round(7 * Aspect))
          else
            Owner.Target.Rectangle(R.Left, R.Top, R.Left + 7, R.Top + 7);
          Owner.Target.Brush.Color := SaveColor;
        end;
      ulCircle :
        begin
          if ScaleBitmaps then
            Owner.Target.Ellipse(R.Left, R.Top, R.Left + round(7 * Aspect), R.Top + round(7 * Aspect))
          else
            Owner.Target.Ellipse(R.Left, R.Top, R.Left + 7, R.Top + 7);
        end;
      end;
  end;
end;

procedure SetRawWordValue(Entry: PIpHtmlElement; const Value: string);
var
  L : Integer;
begin
  Entry.AnsiWord := EscapeToAnsi(Value);
  Entry.IsBlank := 0;
  L := length(Entry.AnsiWord);
  while Entry.IsBlank < L do
    if Entry.AnsiWord[Entry.IsBlank + 1] = ' ' then
      Inc(Entry.IsBlank)
    else
      break;
  if Entry.IsBlank < L  then
    Entry.IsBlank := 0;
end;

procedure TIpHtmlNodeLI.Enqueue;
var
  S : string;
  i : Integer;
begin
  if FParentNode is TIpHtmlNodeOL then begin
    S := TIpHtmlNodeOL(FParentNode).GetNumString;
    SetRawWordValue(WordEntry, S + '.');
    EnqueueElement(WordEntry);
  end else
    EnqueueElement(Element);
  EnqueueElement(TIpHtmlOpener(Owner).FLIndent);
  for i := 0 to Pred(ChildCount) do
    TIpHtmlNodeOpener(ChildNode[i]).Enqueue;
  EnqueueElement(TIpHtmlOpener(Owner).FLOutdent);
end;

function TIpHtmlNodeLI.GetDim(ParentWidth: Integer): TSize;
begin
  if ScaleBitmaps then
    Result := SizeRec(round(Aspect * 8), round(Aspect * 8))
  else
    Result := SizeRec(8, 8);
end;

function TIpHtmlNodeLI.GrossDrawRect: TRect;
begin
  Result := PIpHtmlElement(Element).WordRect2;
end;

procedure TIpHtmlNodeLI.SetListType(const Value: TIpHtmlULType);
begin
  if Value <> FListType then begin
    FListType := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeLI.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  inherited SetProps(Props);
end;

procedure TIpHtmlNodeLI.SetValue(const Value: Integer);
begin
  if Value <> FValue then begin
    FValue := Value;
    InvalidateSize;
  end;
end;
 

{ TIpHtmlNodeOL }

constructor TIpHtmlNodeOL.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  ElementName := 'ol';
end;

procedure TIpHtmlNodeOL.Enqueue;
var
  i: Integer;
  lParentNode: TIpHtmlNodeOpener;
  lOwner: TIpHtmlOpener;
begin
  lOwner := TIpHtmlOpener(Owner);
  lParentNode := TIpHtmlNodeOpener(FParentNode);
  
  {render list}
  if ChildCount > 0 then begin
    EnqueueElement(lOwner.SoftLF);
  end;
  lParentNode.EnqueueElement(lOwner.FLIndent);
  for i := 0 to Pred(ChildCount) do
    if ChildNode[i] is TIpHtmlNodeLI then begin
      Counter := Start + i;
      TIpHtmlNodeLI(ChildNode[i]).Enqueue;
      lParentNode.EnqueueElement(lOwner.SoftLF);
    end else
      TIpHtmlNodeOpener(ChildNode[i]).Enqueue;
  lParentNode.EnqueueElement(lOwner.FLOutdent);
  lParentNode.EnqueueElement(lOwner.SoftLF);
end;

function TIpHtmlNodeOL.GetNumString: string;

  function IntToRomanStr(i : Integer): string;
  const
    RC : array[0..6] of AnsiChar = ('M', 'D', 'C', 'L', 'X', 'V', 'I');
    RV : array[0..6] of Integer = (1000, 500, 100, 50, 10, 5, 1);
  var
    n : Integer;
  begin
    Result := '';
    n := 0;
    repeat
      while i >= RV[n] do begin
        Result := Result + RC[n];
        Dec(i, RV[n]);
      end;
      Inc(n);
    until i = 0;
  end;

begin
  Result := ''; // stop warning
  case Style of
  olArabic :
    str(Counter, Result);
  olLowerAlpha :
    Result := chr(ord('a') + Counter - 1);
  olUpperAlpha :
    Result := chr(ord('A') + Counter - 1);
  olLowerRoman :
    Result := LowerCase(IntToRomanStr(Counter));
  olUpperRoman :
    Result := IntToRomanStr(Counter);
  end;
end;

procedure TIpHtmlNodeOL.LoadAndApplyCSSProps;
begin
  inherited;
  // Override list style by CSS
  if FCombinedCSSProps <> nil then
    case FCombinedCSSProps.ListType of
      ltOLDecimal   : FOLStyle := olArabic;
      ltOLLowerAlpha: FOLStyle := olLowerAlpha;
      ltOLUpperAlpha: FOLStyle := olUpperAlpha;
      ltOLLowerRoman: FOLStyle := olLowerRoman;
      ltOLUpperRoman: FOLStyle := olUpperRoman;
    end;
end;

procedure TIpHtmlNodeOL.SetStart(const Value: Integer);
begin
  if Value <> FStart then begin
    FStart := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeOL.SetOLStyle(const Value: TIpHtmlOLStyle);
begin
  if Value <> FOLStyle then begin
    FOLStyle := Value;
    InvalidateSize;
  end;
end;


{ TIpHtmlNodeTABLE }

constructor TIpHtmlNodeTABLE.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  ElementName := 'table';
  BgColor := clNone;
  SizeWidth := TIpHtmlPixels.Create;
  SizeWidth.PixelsType := hpUndefined;
  FBorderColor := $808080;
  FBorderStyle := cbsInset;
  FLayouter := TableLayouterClass.Create(Self);
end;

destructor TIpHtmlNodeTABLE.Destroy;
begin
  FWidth.Free;
  SizeWidth.Free;
  FreeAndNil(FLayouter);
  inherited;
end;

procedure TIpHtmlNodeTABLE.SetRect(TargetRect: TRect);
var
  dx,dy : Integer;
  z, i, j : Integer;
  R : TRect;
begin
  if ColCount = 0 then Exit;

  dx := TargetRect.Left - BorderRect2.Left;
  dy := TargetRect.Top - BorderRect2.Top;

  OffsetRect(BorderRect, dx, dy);
  OffsetRect(BorderRect2, dx, dy);
  if FCaption <> nil then begin
    with FCaption do begin
      if not IsRectEmpty(PageRect) then begin
        R := PageRect;
        OffsetRect(R, dx, dy);
        Layout(Props, R);
      end;
    end;
  end;

  for z := 0 to Pred(ChildCount) do
    if (TIpHtmlNode(ChildNode[z]) is TIpHtmlNodeTHeadFootBody) then
      with TIpHtmlNodeCore(ChildNode[z]) do
        for i := 0 to Pred(ChildCount) do begin
          if TIpHtmlNode(ChildNode[i]) is TIpHtmlNodeTR then
            with TIpHtmlNodeTR(ChildNode[i]) do begin
              for j := 0 to Pred(ChildCount) do
                if TIpHtmlNode(ChildNode[j]) is TIpHtmlNodeTableHeaderOrCell then
                  with TIpHtmlNodeTableHeaderOrCell(ChildNode[j]) do begin
                    if not IsRectEmpty(PadRect) then
                      OffsetRect(FPadRect, dx, dy);
                    if not IsRectEmpty(PageRect) then begin
                      R := PageRect;
                      OffsetRect(R, dx, dy);
                      Layout(Props, R);
                    end;
                  end;
            end;
        end;
end;

procedure TIpHtmlNodeTABLE.Draw(Block: TIpHtmlNodeBlock);
var
  z, i, j : Integer;
  R : TRect;
  Al : TIpHtmlVAlign3;
  TRBgColor, TrTextColor: TColor;
  aCanvas : TCanvas;
  lLayouter: TIpHtmlBaseTableLayouterOpener;
begin
  aCanvas := Owner.Target;
  lLayouter := TIpHtmlBaseTableLayouterOpener(FLayouter);

  if (FOwner.Body.BgPicture <> nil) or (Props.BGColor = 1) then
    aCanvas.Brush.Style := bsClear
  else
  if (Props.BGColor <> clNone) and PageRectToScreen(BorderRect, R) then begin
    aCanvas.Brush.Color :=Props.BGColor;
    aCanvas.FillRect(R);
  end;
  aCanvas.Pen.Color := clBlack;

  Al := Props.VAlignment;

  for z := 0 to Pred(ColCount) do
    lLayouter.FRowSp[z] := 0;

  for z := 0 to Pred(ChildCount) do
    if (TIpHtmlNode(ChildNode[z]) is TIpHtmlNodeTHeadFootBody) then
      with TIpHtmlNodeCore(ChildNode[z]) do
        for i := 0 to Pred(ChildCount) do begin
          if TIpHtmlNode(ChildNode[i]) is TIpHtmlNodeTR then
            with TIpHtmlNodeTR(ChildNode[i]) do begin
              case VAlign of
                hvaTop    : Al := hva3Top;
                hvaMiddle : Al := hva3Middle;
                hvaBottom : Al := hva3Bottom;
              end;

              TrBgColor := BgColor;
              TrTextColor := TextColor;

              for j := 0 to Pred(ChildCount) do
                if TIpHtmlNode(ChildNode[j]) is TIpHtmlNodeTableHeaderOrCell then
                  with TIpHtmlNodeTableHeaderOrCell(ChildNode[j]) do begin
                    if VAlign <> hva3Default then
                      Al := VAlign;

                    // set TR color, Render override them anyway if TD/TH have own settings
                    if FOwner.NeedResize then
                    begin
                      Props.BGColor := TrBgColor;
                      Props.FontColor := TrTextColor;

                      Props.VAlignment := Al;
                    end;
                    Render(Props);
                    {paint left rule if selected}
                    case Rules of
                    hrNone,
                    hrGroups :;
                    hrRows :;
                    hrCols,
                    hrAll :
                      begin
                        if not IsRectEmpty(PadRect) then begin
                          R := PadRect;
                          Inflaterect(R, 1, 1);
                          ScreenFrame(R, False);
                        end;
                      end;
                    end;

                  end;

            end;
        end;

  {render frames}
  // to frame
  if Frame in [hfAbove, hfHSides, hfBox, hfBorder] then
    if Border = 1 then
      ScreenLine(
        BorderRect.TopLeft,
        Point(BorderRect.Right-1, BorderRect.Top),
        1,
        CalcBorderColor(BorderColor, BorderStyle, hfAbove))
    else
      ScreenPolygon(
        [BorderRect.TopLeft,
        Point(BorderRect.Right, BorderRect.Top),
        Point(BorderRect.Right - (Border - 1), BorderRect.Top + Border - 1),
        Point(BorderRect.Left + Border - 1, BorderRect.Top + Border - 1)],
        CalcBorderColor(BorderColor, BorderStyle, hfAbove));
  // bottom frame
  if Frame in [hfBelow, hfHSides, hfBox, hfBorder] then
    if Border = 1 then
      ScreenLine(
        Point(BorderRect.Right - 1, BorderRect.Bottom - 1),
        Point(BorderRect.Left, BorderRect.Bottom - 1),
        1,
        CalcBorderColor(BorderColor, BorderStyle, hfBelow))
    else
    ScreenPolygon(
      [
      Point(BorderRect.Right - 1, BorderRect.Bottom - 1),
      Point(BorderRect.Right - (Border - 1), BorderRect.Bottom - (Border - 1) - 1),
      Point(BorderRect.Left + Border, BorderRect.Bottom - (Border - 1) - 1),
      Point(BorderRect.Left, BorderRect.Bottom - 1)],
        CalcBorderColor(BorderColor, BorderStyle, hfBelow));
  // left frame
  if Frame in [hfLhs, hfvSides, hfBox, hfBorder] then
    if Border = 1 then
      ScreenLine(
        BorderRect.TopLeft,
        Point(BorderRect.Left, BorderRect.Bottom - 1),
        1,
        CalcBorderColor(BorderColor, BorderStyle, hfLhs))
    else
      ScreenPolygon(
        [BorderRect.TopLeft,
        Point(BorderRect.Left, BorderRect.Bottom - 1),
        Point(BorderRect.Left + (Border - 1), BorderRect.Bottom - Border),
        Point(BorderRect.Left + (Border - 1), BorderRect.Top + (Border - 1))],
        CalcBorderColor(BorderColor, BorderStyle, hfLhs));
  // right frame
  if Frame in [hfRhs, hfvSides, hfBox, hfBorder] then
    if Border = 1 then
      ScreenLine(
        Point(BorderRect.Right - 1, BorderRect.Bottom - 1),
        Point(BorderRect.Right - 1, BorderRect.Top),
        1,
        CalcBorderColor(BorderColor, BorderStyle, hfRhs))
    else
      ScreenPolygon(
        [
        Point(BorderRect.Right - 1, BorderRect.Bottom - 1),
        Point(BorderRect.Right - 1, BorderRect.Top),
        Point(BorderRect.Right - (Border - 1) - 1, BorderRect.Top + (Border - 1)),
        Point(BorderRect.Right - (Border - 1) - 1, BorderRect.Bottom - Border)],
        CalcBorderColor(BorderColor, BorderStyle, hfRhs));

  {render caption}
  if assigned(FCaption) then
    FCaption.Render(Props);
end;

procedure TIpHtmlNodeTABLE.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.NoBreak := False;
  inherited SetProps(RenderProps);
  //BgColor := Props.BgColor;
end;

function TIpHtmlNodeTABLE.GetDim(ParentWidth: Integer): TSize;
begin
  if (SizeWidth.PixelsType <> hpAbsolute) or (SizeWidth.Value <> ParentWidth) then
  begin
    SizeWidth.PixelsType := hpUndefined;
    FLayouter.CalcSize(ParentWidth, Props);
    SizeWidth.Value := ParentWidth;
    SizeWidth.PixelsType := hpAbsolute;
  end;
  Result := SizeRec(BorderRect2.Right - BorderRect2.Left,
                    BorderRect2.Bottom - BorderRect2.Top);
end;

procedure TIpHtmlNodeTABLE.CalcMinMaxWidth(var Min, Max: Integer);
begin
  FLayouter.CalcMinMaxColTableWidth(Props, Min, Max);
  case Width.LengthType of
  hlAbsolute :
    begin
      Min := MaxI2(Min, Width.LengthValue);
      Max := MaxI2(Max, Min);
    end;
  end;
end;

procedure TIpHtmlNodeTABLE.InvalidateSize;
begin
  SizeWidth.PixelsType := hpUndefined;
  FLayouter.ResetSize;
  inherited;
end;

function TIpHtmlNodeTABLE.GetColCount: Integer;
begin
  Result := FLayouter.GetColCount;
end;

procedure TIpHtmlNodeTABLE.Enqueue;
var
  lOwner: TIpHtmlOpener;
begin
  lOwner := TIpHtmlOpener(Owner);
  
//The commented code below prevents a blank line before the table
{
  case Align of
  hiaTop,
  hiaMiddle,
  hiaBottom,
  hiaCenter :
    EnqueueElement(Owner.SoftLF);
  end;
}
  EnqueueElement(lOwner.SoftLF);

  EnqueueElement(Element);

  EnqueueElement(lOwner.SoftLF);
  EnqueueElement(lOwner.HardLF);  // LFs needed otherwise next element is too close
{
  case Align of
  hiaTop,
  hiaMiddle,
  hiaBottom,
  hiaCenter :
    EnqueueElement(Owner.SoftLF);
  end;
}
end;

procedure TIpHtmlNodeTABLE.SetBorder(const Value: Integer);
begin
  FBorder := Value;
  if Border = 0 then begin
    Frame := hfVoid;
    Rules := hrNone;
  end else begin
    Frame := hfBorder;
    Rules := hrAll;
  end;
  InvalidateSize;
end;

function TIpHtmlNodeTABLE.GetMaxWidth: Integer;
begin
  Result := TIpHtmlBaseTableLayouterOpener(FLayouter).FMax;
end;

function TIpHtmlNodeTABLE.GetMinWidth: Integer;
begin
  Result := TIpHtmlBaseTableLayouterOpener(FLayouter).FMin;
end;

function TIpHtmlNodeTABLE.GetTableWidth: Integer;
begin
  Result := TIpHtmlBaseTableLayouterOpener(FLayouter).FTableWidth;
end;

function TIpHtmlNodeTABLE.GetCellPadding: Integer;
begin
  Result := TIpHtmlBaseTableLayouterOpener(FLayouter).FCellPadding;
end;

function TIpHtmlNodeTABLE.GetCellSpacing: Integer;
begin
  Result := TIpHtmlBaseTableLayouterOpener(FLayouter).FCellSpacing;
end;

procedure TIpHtmlNodeTABLE.SetCellPadding(const Value: Integer);
begin
  TIpHtmlBaseTableLayouterOpener(FLayouter).FCellPadding := Value;
  InvalidateSize;
end;

procedure TIpHtmlNodeTABLE.SetCellSpacing(const Value: Integer);
begin
  TIpHtmlBaseTableLayouterOpener(FLayouter).FCellSpacing := Value;
  InvalidateSize;
end;

procedure TIpHtmlNodeTABLE.SetFrame(const Value: TIpHtmlFrameProp);
begin
  FFrame := Value;
  InvalidateSize;
end;

procedure TIpHtmlNodeTABLE.SetRules(const Value: TIpHtmlRules);
begin
  FRules := Value;
  InvalidateSize;
end;

procedure TIpHtmlNodeTABLE.WidthChanged(Sender: TObject);
begin
  InvalidateSize;
end;

function TIpHtmlNodeTABLE.ExpParentWidth: Integer;
begin
  case Width.LengthType of
  hlAbsolute :
    Result := Width.LengthValue;
  else
    Result := inherited ExpParentWidth;
  end;
end;

procedure TIpHtmlNodeTABLE.LoadAndApplyCSSProps;
begin
  inherited LoadAndApplyCSSProps;
  if FCombinedCSSProps = nil then
    exit;
  if FCombinedCSSProps.Border.Style <> cbsNone then
  begin
    FBorder := FCombinedCSSProps.Border.Width;
    BorderColor := FCombinedCSSProps.Border.Color;
    BorderStyle := FCombinedCSSProps.Border.Style;
    if Frame = hfVoid then
    begin
      Frame := hfBorder;
      Rules := hrGroups;
    end;
  end;
  if FCombinedCSSProps.Width.LengthType <> cltUndefined then begin
    FWidth.Free;
    FWidth := TIpHtmlLength.Create;
    FWidth.LengthValue := FCombinedCSSProps.Width.LengthValue;
    FWidth.LengthType := TIpHtmlLengthType(ord(FCombinedCSSProps.Width.LengthType));
    FWidth.OnChange := WidthChanged;
  end;
end;


{-------------------------------------------------------------------------------
                    Descendants of TIpHtmlNodeGenInline 
-------------------------------------------------------------------------------}

{ TIpHtmlNodeBASEFONT }

procedure TIpHtmlNodeBASEFONT.ApplyProps(
  const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.FontSize := FONTSIZESVALUESARRAY[Size-1];
  Props.BaseFontSize := Size;
end;


{ TIpHtmlNodeDEL }

procedure TIpHtmlNodeDEL.ApplyProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.FontStyle := Props.FontStyle + [fsStrikeOut];
end;


{ TIpHtmlNodeFONT }

procedure TIpHtmlNodeFONT.ApplyProps(const RenderProps: TIpHtmlProps);

  function GetFontSizeValue(aSize: integer): integer;
  begin
    aSize:=MaxI2(aSize,low(FONTSIZESVALUESARRAY));
    aSize:=MinI2(aSize,high(FONTSIZESVALUESARRAY));
    Result:=FONTSIZESVALUESARRAY[aSize];
  end;

var
  TmpSize : Integer;
begin
  Props.Assign(RenderProps);
  if Face <> '' then
    Props.FontName := FindFontName(Face);
  case Size.SizeType of
  hrsAbsolute :
    begin
      TmpSize:=Size.Value;
      Props.FontSize := GetFontSizeValue(TmpSize);
    end;
  hrsRelative :
    begin
      TmpSize := Props.BaseFontSize + Size.Value;
      Props.FontSize := GetFontSizeValue(TmpSize);
    end;
  end;
  if Color <> clNone then
    Props.FontColor := Color;
end;

constructor TIpHtmlNodeFONT.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  FSize := TIpHtmlRelSize.Create;
end;

destructor TIpHtmlNodeFONT.Destroy;
begin
  inherited;
  FreeAndNil(FSize);
end;

procedure TIpHtmlNodeFONT.SetColor(const Value: TColor);
begin
  if Value <> FColor then begin
    FColor := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeFONT.SetFace(const Value: string);
begin
  if Value <> FFace then begin
    FFace := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeFONT.SizeChanged(Sender: TObject);
begin
  InvalidateSize;
end;


{ TIpHtmlNodeFontStyle }

procedure TIpHtmlNodeFontStyle.ApplyProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  case Style of
    hfsTT: 
      begin
        Props.FontName := Owner.FixedTypeface;
        ElementName := 'tt';
      end;
    hfsI: 
      begin
        Props.FontStyle := Props.FontStyle + [fsItalic];
        ElementName := 'i';
      end;
    hfsB: 
      begin
        Props.FontStyle := Props.FontStyle + [fsBold];
        ElementName := 'b';
      end;
    hfsU: 
      begin
        Props.FontStyle := Props.FontStyle + [fsUnderline];
        ElementName := 'u';
      end;
    hfsSTRIKE: 
      begin
        Props.FontStyle := Props.FontStyle + [fsStrikeout];
        ElementName := 'strike';
      end;
    hfsS:
      begin
        Props.FontStyle := Props.FontStyle + [fsStrikeout];
        ElementName := 's';
      end;
    hfsBIG: begin
        Props.FontSize := Props.FontSize + 2;
        ElementName := 'big';
      end;
    hfsSMALL: 
      begin
        Props.FontSize := Props.FontSize - 2;
        ElementName := 'small';
      end;
    hfsSUB: 
      begin
        Props.FontSize := Props.FontSize - 4;
        Props.FontBaseline := Props.FontBaseline - 2;
        ElementName := 'sub';
      end;
    hfsSUP: 
      begin
        Props.FontSize := Props.FontSize - 4;
        Props.FontBaseline := Props.FontBaseline + 4;
        ElementName := 'sup';
      end;
  end;
end;


{ TIpHtmlNodeINS }

procedure TIpHtmlNodeINS.ApplyProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.FontStyle := Props.FontStyle + [fsUnderline];
end;


{ TIpHtmlNodeNOBR }

procedure TIpHtmlNodeNOBR.ApplyProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.NoBreak := True;
end;


{ TIpHtmlNodePhrase }

procedure TIpHtmlNodePhrase.ApplyProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  case Style of
    hpsEM, 
    hpsVAR, 
    hpsCITE    : Props.FontStyle := Props.FontStyle + [fsItalic];
    hpsSTRONG  : Props.FontStyle := Props.FontStyle + [fsBold];
    hpsCODE, 
    hpsKBD, 
    hpsSAMP    : Props.FontName := Owner.FixedTypeface;
  end;
  case Style of
    hpsEM      : ElementName := 'em';
    hpsSTRONG  : ElementName := 'strong';
    hpsDFN     : ElementName := 'dfn';
    hpsCODE    : ElementName := 'code';
    hpsSAMP    : ElementName := 'samp';
    hpsKBD     : ElementName := 'kbd';
    hpsVAR     : ElementName := 'var';
    hpsCITE    : ElementName := 'cite';
    hpsABBR    : ElementName := 'abbr';
    hpsACRONYM : ElementName := 'acronym';
  end;
end;


{ TIpHtmlNodeSPAN }

constructor TIpHtmlNodeSPAN.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  ElementName := 'span';
end;

procedure TIpHtmlNodeSPAN.ApplyProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.DelayCache:=True;
  Props.Alignment := Align;
  LoadAndApplyCSSProps;
  Props.DelayCache:=False;
end;

function TIpHtmlNodeSPAN.GetAlign: TIpHtmlAlign;
begin
  Result := FAlign;
end;

procedure TIpHtmlNodeSPAN.LoadAndApplyCSSProps;
begin
  inherited;
  if not (FCombinedCSSProps.Alignment in [haDefault, haUnknown]) then
    Align := FCombinedCSSProps.Alignment;
end;

procedure TIpHtmlNodeSPAN.SetAlign(const Value: TIpHtmlAlign);
begin
  FAlign := Value;
end;


{-------------------------------------------------------------------------------
                     Descendants of TIpHtmlNodeList
-------------------------------------------------------------------------------}

constructor TIpHtmlNodeUL.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  ElementName := 'ul';
end;


{-------------------------------------------------------------------------------
                     Descendants of TIpHtmlNodeControl
-------------------------------------------------------------------------------}

{ TIpHtmlNodeBUTTON }

procedure TIpHtmlNodeBUTTON.AddValues(NameList, ValueList : TStringList);
begin
end;

constructor TIpHtmlNodeBUTTON.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  ElementName := 'button';
  with TIpHtmlOpener(Owner) do
  begin
    FControlList.Add(Self);
    if DoneLoading then
      Self.CreateControl(FControlParent);
  end;
end;

destructor TIpHtmlNodeBUTTON.Destroy;
begin
  TIpHtmlOpener(Owner).FControlList.Remove(Self);
  inherited;
end;

procedure TIpHtmlNodeBUTTON.CreateControl(Parent: TWinControl);
begin
  inherited;
  TIpHtmlOpener(Owner).ControlCreate(Self);

  FControl := TButton.Create(Parent);
  FControl.Visible := False;
  FControl.Parent := Parent;
  adjustFromCss;

  with TButton(FControl) do begin
    Enabled := not Self.Disabled;
    Caption := GetButtonCaption;
    OnClick := ButtonClick;
    CalcSize;
  end;
end;

function TIpHtmlNodeBUTTON.GetButtonCaption: String;
begin
  if FValue = '' then
    case FInputType of
      hbtSubmit: Result := SHtmlDefSubmitCaption;
      hbtReset: Result := SHtmlDefResetCaption;
      hbtButton: Result := '';
    end
  else
    Result := FValue;
end;

procedure TIpHtmlNodeBUTTON.Reset;
begin
end;

procedure TIpHtmlNodeBUTTON.ResetClick(Sender: TObject);
begin
  ResetRequest;
end;

procedure TIpHtmlNodeBUTTON.SubmitClick(Sender: TObject);
begin
  SubmitRequest;
end;

procedure TIpHtmlNodeBUTTON.ButtonClick(Sender: TObject);
begin
  case ButtonType of
  hbtSubmit :
    begin
      SubmitRequest;
    end;
  hbtReset :
    begin
      ResetRequest;
    end;
  hbtButton :
    begin
      TIpHtmlOpener(Owner).ControlClick(Self);
    end;
  end;
end;

function TIpHtmlNodeBUTTON.Successful: Boolean;
begin
  Result := False;
end;

procedure TIpHtmlNodeBUTTON.CalcSize;
var
  oldFontSize: integer;
  lCanvas: TCanvas;
begin
  with Control as TButton do
  begin
    lCanvas := TFriendPanel(Parent).Canvas;
    oldFontSize := lCanvas.Font.Size;
    Width := TFriendPanel(Parent).Canvas.TextWidth(Caption) + 40;
    Height := TFriendPanel(Parent).Canvas.TextHeight('Tg') + 10;
    lCanvas.Font.Size := oldFontSize;
  end;
end;

procedure TIpHtmlNodeBUTTON.SetInputType(const AValue: TIpHtmlButtonType);
begin
  if FInputType = AValue then exit;
  FInputType := AValue;
  if TIpHtmlOpener(Owner).DoneLoading and (FControl <> nil) and (Self.Value = '') then
    SetValue(GetButtonCaption);
end;

procedure TIpHtmlNodeBUTTON.SetValue(const AValue: String);
begin
  if FValue = AValue then Exit;
  FValue := AValue;
  if TIpHtmlOpener(Owner).DoneLoading and (FControl <> nil) then
  begin
    (FControl as TButton).Caption := GetButtonCaption;
    CalcSize;
  end;
end;


{ TIpHtmlNodeINPUT }

constructor TIpHtmlNodeINPUT.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  ElementName := 'input';
  Props.BgColor := clWhite;
end;

destructor TIpHtmlNodeINPUT.Destroy;
begin
  inherited;
  FPicture.Free;
end;

procedure TIpHtmlNodeINPUT.SetImageGlyph(Picture: TPicture);
var
  FBitmap : TBitmap;
begin
  with TBitbtn(FControl) do begin
    FBitmap := TBitmap.Create;
    try
      FBitmap.Width := Picture.Width;
      FBitmap.Height := Picture.Height;
      Picture.Graphic.Transparent := False;
      FBitmap.TransparentMode := tmFixed;
      FBitmap.TransparentColor := RGBToColor(254, 254, 254);
      FBitmap.Canvas.Draw(0, 0, Picture.Graphic);
      Glyph.Assign(FBitmap);
      Width := FBitmap.Width + 4;
      Height := FBitmap.Height + 4;
    finally
      FBitmap.Free;
    end;
  end;
end;

procedure TIpHtmlNodeINPUT.Reset;
begin
  case InputType of
  hitText :
    begin
      with TEdit(FControl) do
        Text := Value;
    end;
  hitPassword :
    begin
      with TEdit(FControl) do
        Text := Value;
    end;
  hitCheckbox :
    begin
      with TCheckBox(FControl) do
        Checked := Self.Checked;
    end;
  hitRadio :
    begin
      with THtmlRadioButton(FControl) do
        Checked := Self.Checked;
    end;
  end;
end;

procedure TIpHtmlNodeINPUT.CreateControl(Parent: TWinControl);
var
  iCurFontSize: integer;
  aCanvas : TCanvas;

  function OwnerForm: TIpHtmlNode;
  begin
    Result := FParentNode;
    while (Result <> nil) and not (Result is TIpHtmlNodeFORM) do
      Result := Result.ParentNode;
  end;

  procedure setCommonProperties;
  begin
    FControl.Parent := Parent;
    FControl.Visible := False;
    AdjustFromCss;
    aCanvas.Font.Size := FControl.Font.Size;
  end;

  procedure SetWidthHeight(iSize, iTopPlus, iSidePlus: integer);
  begin
    if iSize <> -1 then
      FControl.Width := iSize * aCanvas.TextWidth('0') + iSidePlus
    else
      FControl.Width := 20 * aCanvas.TextWidth('0')  + iSidePlus;
    FControl.Height := aCanvas.TextHeight('Wy') + iTopPlus;
  end;

begin
  inherited;
  TIpHtmlOpener(Owner).ControlCreate(Self);

  aCanvas := TFriendPanel(Parent).Canvas;
  iCurFontSize := aCanvas.Font.Size;
  case InputType of
  hitText :
    begin
      FControl := TEdit.Create(Parent);
      setCommonProperties;
      with TEdit(FControl) do begin
        Color := Brush.Color;
        Text := Value;
        MaxLength := Self.MaxLength;
        SetWidthHeight(Self.Size, 8, 0);
        Enabled := not Self.Disabled;
        ReadOnly := Self.ReadOnly;
        OnChange := ButtonClick;
        OnEditingDone := ControlOnEditingDone;
      end;
    end;
  hitPassword :
    begin
      FControl := TEdit.Create(Parent);
      setCommonProperties;
      with TEdit(FControl) do begin
        Color := Brush.Color;
        Text := Value;
        MaxLength := Self.MaxLength;
        SetWidthHeight(Self.Size, 8, 0);
        Enabled := not Self.Disabled;
        ReadOnly := Self.ReadOnly;
        PasswordChar := '*';
        OnChange := ButtonClick;
        OnEditingDone := ControlOnEditingDone;
      end;
    end;
  hitCheckbox :
    begin
      FControl := TCheckBox.Create(Parent);
      setCommonProperties;
      with TCheckBox(FControl) do begin
        Color := Brush.Color;
        SetWidthHeight(1, 8, 0);
        Checked := Self.Checked;
        Enabled := not Self.Disabled and not Self.Readonly;
        OnClick := ButtonClick;
        OnEditingDone := ControlOnEditingDone;
      end;
    end;
  hitRadio :
    begin
      FControl := THtmlRadioButton.Create(Parent);
      FControl.Tag := PtrInt(OwnerForm);
      setCommonProperties;
      with THtmlRadioButton(FControl) do begin
        Color := Brush.Color;
        SetWidthHeight(1, 8, 0);
        Checked := Self.Checked;
        Enabled := not Self.Disabled and not Self.Readonly;
        OnClick := ButtonClick;
        OnEditingDone := ControlOnEditingDone;
      end;
    end;
  hitSubmit :
    begin
      FControl := TButton.Create(Parent);
      setCommonProperties;
      with TButton(FControl) do begin
        if Self.Value <> '' then
          Caption := Self.Value
        else
          Caption := SHtmlDefSubmitCaption;
        Color := Brush.Color;
        Width := aCanvas.TextWidth(Caption) + 40;
        Height := aCanvas.TextHeight(Caption) + 10;
        Enabled := not Self.Disabled and not Self.Readonly;
        OnClick := SubmitClick;
      end;
    end;
  hitReset :
    begin
      FControl := TButton.Create(Parent);
      setCommonProperties;
      with TButton(FControl) do begin
        if Self.Value <> '' then
          Caption := Self.Value
        else
          Caption := SHtmlDefResetCaption;
        Color := Brush.Color;
        Width := aCanvas.TextWidth(Caption) + 40;
        Height := aCanvas.TextHeight(Caption) + 10;
        Enabled := not Self.Disabled and not Self.Readonly;
        OnClick := ResetClick;
      end;
    end;
  hitFile :
    begin
      FControl := TPanel.Create(Parent);
      setCommonProperties;
      with TPanel(FControl) do begin
        Width := 200;
        Height := aCanvas.TextHeight('Wy') + 12;
        Enabled := not Self.Disabled and not Self.Readonly;
        BevelInner := bvNone;
        BevelOuter := bvNone;
        BorderStyle := bsNone;
      end;
      FFileSelect := TButton.Create(Parent);
      with FFileSelect do begin
        Parent := FControl;
        Height := aCanvas.TextHeight(SHtmlDefBrowseCaption) + 10;
        Width := aCanvas.TextWidth(SHtmlDefBrowseCaption) + 40;
        Left := FControl.Left + FControl.Width - Width;
        Top := 1;
        Caption := SHtmlDefBrowseCaption;
        OnClick := FileSelect;
      end;
      FFileEdit := TEdit.Create(Parent);
      with FFileEdit do begin
        Parent := FControl;
        Color := Brush.Color;
        Left := 1;
        Top := 1;
        Width := FControl.Width - FFileSelect.Width;
        Height := FControl.Height - 2;
      end;
    end;
  hitHidden :
    begin
    end;
  hitImage :
    begin
      FControl := TBitbtn.Create(Parent);
      setCommonProperties;
      with TIpHtmlOpener(Owner) do
        DoGetImage(Self, BuildPath(Src), FPicture);
      if FPicture = nil
        then FPicture := TIpHtmlOpener(Owner).DefaultImage;
      with TBitbtn(FControl) do begin
        Caption := Self.Value;
        Enabled := not Self.Disabled and not Self.Readonly;
        SetImageGlyph(FPicture);
      end;
    end;
  hitButton :
    begin
      FControl := TButton.Create(Parent);
      setCommonProperties;
      with TButton(FControl) do begin
        Caption := Self.Value;
        Width := aCanvas.TextWidth(Caption) + 40;
        Height := aCanvas.TextHeight(Caption) + 10;
        Enabled := not Self.Disabled and not Self.Readonly;
        OnClick := ButtonClick;
      end;
    end;
  end;
  if FControl <> nil then
  begin
    FControl.Hint := Alt;
    FControl.ShowHint:=True;
    if (FControl is TEdit) then
      FControl.ControlStyle:=FControl.ControlStyle + [csOpaque];
  end;
  aCanvas.Font.Size := iCurFontSize;
end;

procedure TIpHtmlNodeINPUT.Draw;
begin
{
  if Assigned(FInlineCSSProps) then
  begin
       if FInlineCSSProps.BGColor <> clNone then FControl.Color := FInlineCSSProps.BGColor;
       if FInlineCSSProps.Color <> clNone then FControl.Font.Color := FInlineCSSProps.Color;
       if FInlineCSSProps.Font.Size <> '' then FControl.Font.size := GetFontSizeFromCSS(FControl.Font.size, FInlineCSSProps.Font.Size);
  end;
}
  inherited;
  if (Props.BgColor <> clNone) and (
    (FControl is THtmlRadioButton) or
    (FControl is TCustomEdit)) then
    FControl.Color := Props.BgColor;
end;

procedure TIpHtmlNodeINPUT.ImageChange(NewPicture: TPicture);
begin
  {$IFOPT C+}
  Owner.CheckImage(NewPicture);
  {$ENDIF}
  if FPicture <> TIpHtmlOpener(Owner).DefaultImage then
    FPicture.Free;
  FPicture := NewPicture;
  SetImageGlyph(FPicture);
  InvalidateSize;
end;

procedure TIpHtmlNodeINPUT.AddValues(NameList, ValueList : TStringList);
var
  S : string;
begin
  S := '';
  case InputType of
  hitText,
  hitPassword :
    S := TEdit(FControl).Text;
  hitCheckbox :
    S := Value;
  hitRadio :
    S := Value;
  hitFile :
    S := 'file://'+FFileEdit.Text;
  hitHidden :
    S := FValue;
  end;
  if S <> '' then begin
    NameList.Add(Name);
    ValueList.Add(S);
  end;
end;

function TIpHtmlNodeINPUT.Successful: Boolean;
begin
  Result :=
    (Name <> '')and
    ( (InputType = hitHidden) or
      ( (not Disabled) and
        (InputType in [hitText, hitPassword, hitCheckbox, hitRadio , hitFile])
      )
    );
  if Result then begin
    case InputType of
    hitText,
    hitPassword :
      Result := TEdit(FControl).Text <> '';
    hitCheckbox :
      Result := TCheckBox(FControl).Checked;
    hitRadio :
      Result := THtmlRadioButton(FControl).Checked;
    hitFile :
      Result := FFileEdit.Text <> '';
    hitHidden :
      Result := FValue <> '';
    end;
  end;
end;

procedure TIpHtmlNodeINPUT.SubmitClick(Sender: TObject);
var
  vCancel: boolean;
begin
  vCancel := False;
  TIpHtmlOpener(Owner).ControlClick2(Self, vCancel);
  if not vCancel then SubmitRequest;
end;

procedure TIpHtmlNodeINPUT.ResetClick(Sender: TObject);
begin
  ResetRequest;
end;

procedure TIpHtmlNodeINPUT.getControlValue;
begin
  case InputType of
  hitText,
  hitPassword :
    Value := TEdit(FControl).Text;
  hitCheckbox :
    Checked := TCheckBox(FControl).Checked;
  hitRadio :
    Checked := THtmlRadioButton(FControl).Checked;
  end;
end;

procedure TIpHtmlNodeINPUT.ButtonClick(Sender: TObject);
begin
  getControlValue;
  TIpHtmlOpener(Owner).ControlClick(Self);
end;

procedure TIpHtmlNodeINPUT.ControlOnEditingDone(Sender: TObject);
begin
  getControlValue;
  TIpHtmlOpener(Owner).ControlOnEditingDone(Self);
end;

procedure TIpHtmlNodeINPUT.ControlOnChange(Sender: TObject);
begin
  getControlValue;
  TIpHtmlOpener(Owner).ControlOnChange(Self);
end;

function TIpHtmlNodeINPUT.GetHint: string;
begin
  Result := Alt;
end;

procedure TIpHtmlNodeINPUT.FileSelect(Sender: TObject);
begin
  with TOpenDialog.Create(FControl) do
    try
      if Execute then
        FFileEdit.Text := FileName;
    finally
      free;
    end;
end;


{ TIpHtmlNodeSELECT }

constructor TIpHtmlNodeSELECT.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  ElementName := 'select';
  FWidth := -1;
  FSize := -1;
end;

destructor TIpHtmlNodeSELECT.Destroy;
begin
  inherited;
end;

procedure TIpHtmlNodeSELECT.AddValues(NameList, ValueList : TStringList);
var
  i : Integer;
begin
  if FControl is TListBox then
    with TListBox(FControl) do begin
      for i := 0 to Pred(Items.Count) do
        if Selected[i] then begin
          NameList.Add(Self.Name);
          ValueList.Add(Items[i]);
        end;
    end
  else with TComboBox(FControl) do begin
    NameList.Add(Self.Name);
    ValueList.Add(Items[ItemIndex]);
  end;
end;

procedure TIpHtmlNodeSELECT.CreateControl(Parent: TWinControl);
var
  aCanvas : TCanvas;
  SelectedText: string;
  MinW: Integer;

  procedure AdjustControl;
  var
    Sz: Integer;
  begin
    Sz := Size;
    if Sz = -1 then Sz:= 1;
    FControl.Visible := False;
    FControl.Parent := Parent;
    FControl.Height := (4 + aCAnvas.TextHeight('Wy')) * Sz;
    FControl.Enabled := not Disabled;
    FControl.OnClick := ButtonClick;
    adjustFromCss;
  end;

  procedure CreateControlSub(Opt: TIpHtmlNodeOPTION);
  var
    k: Integer;
    B: PAnsiChar;
    S: String;
  begin
    if (Opt.ChildCount > 0) and (TObject(Opt.ChildNode[0]) is TIpHtmlNodeText) then 
    begin
      S := TIpHtmlNodeText(Opt.ChildNode[0]).EscapedText;
      Getmem(B, length(S) + 1);
      try
        TrimFormatting(S, B);
        S := Trim(B);
        if Multiple then begin
          k := TListBox(FControl).Items.Add(S);
          TListBox(FControl).Selected[k] := Opt.Selected;
        end else begin
          TComboBox(FControl).Items.Add(S);
          if Opt.Selected then
            SelectedText := S;
        end;
        MinW := MaxI2(MinW, aCanvas.TextWidth(S));
      finally
        FreeMem(B);
      end;
    end;
  end;

var
  i, j, iCurFontSize: integer;
  OptGroup: TIpHtmlNodeOPTGROUP;
begin
  inherited;
  TIpHtmlOpener(Owner).ControlCreate(Self);

  aCanvas := TFriendPanel(Parent).Canvas;
  iCurFontSize := aCanvas.Font.Size;
  if Multiple then begin
    FControl := TListBox.Create(Parent);
    AdjustControl;
    with TListBox(FControl) do begin
      IntegralHeight := True;
      MultiSelect := True;
      OnSelectionChange := ListBoxSelectionChange;
    end;
  end else begin
    FControl := TComboBox.Create(Parent);
    AdjustControl;
    with TComboBox(FControl) do begin
      Style := csDropDownList;
      OnEditingDone := ControlOnEditingdone;
    end;
  end;
  MinW := 50;
  SelectedText := '';
  for i := 0 to Pred(ChildCount) do
    if ChildNode[i] is TIpHtmlNodeOPTION then
      CreateControlSub(TIpHtmlNodeOPTION(ChildNode[i]))
    else if ChildNode[i] is TIpHtmlNodeOPTGROUP then begin
      OptGroup := TIpHtmlNodeOPTGROUP(ChildNode[i]);
      for j := 0 to Pred(OptGroup.ChildCount) do
        if OptGroup.ChildNode[j] is TIpHtmlNodeOPTION then
          CreateControlSub(TIpHtmlNodeOPTION(OptGroup.ChildNode[j]))
    end;
  if SelectedText <> '' then
    with TComboBox(FControl) do
      ItemIndex := Items.IndexOf(SelectedText);
  if FComboBox and (Width <> -1) then
    FControl.Width := Width*aCanvas.TextWidth('0')+ 20
  else
    FControl.Width := MinW + 40;
  FControl.ShowHint:=True;
  FControl.Hint:= Alt;
  aCanvas.Font.Size := iCurFontSize;
end;

procedure TIpHtmlNodeSELECT.Reset;
var
  SelectedText : string;

  procedure ResetSub(Opt: TIpHtmlNodeOPTION);
  var
    k: Integer;
    B: PAnsiChar;
    S: String;
  begin
    if (Opt.ChildCount > 0) and (Opt.ChildNode[0] is TIpHtmlNodeText) then 
    begin
      S := TIpHtmlNodeText(Opt.ChildNode[0]).EscapedText;
      GetMem(B, length(S) + 1);
      try
        TrimFormatting(S, B);
        if Multiple then begin
          k := TListBox(FControl).Items.Add(Trim(B));
          TListBox(FControl).Selected[k] := Opt.Selected;
        end else begin
          TComboBox(FControl).Items.Add(Trim(B));
          if Opt.Selected then
            SelectedText := Trim(B);
        end;
      finally
        FreeMem(B);
      end;
    end;
  end;

var
  i, j: Integer;
  OptGroup: TIpHtmlNodeOPTGROUP;
begin
  SelectedText := '';
  if Multiple then
    TListBox(FControl).Clear
  else
    TComboBox(FControl).Clear;
  for i := 0 to Pred(ChildCount) do
    if ChildNode[i] is TIpHtmlNodeOPTION then
      // Option
      ResetSub(TIpHtmlNodeOPTION(ChildNode[i]))
    else if ChildNode[i] is TIpHtmlNodeOPTGROUP then begin
      // Option Group
      OptGroup := TIpHtmlNodeOPTGROUP(ChildNode[i]);
      for j := 0 to Pred(OptGroup.ChildCount) do
        if OptGroup.ChildNode[j] is TIpHtmlNodeOPTION then
          ResetSub(TIpHtmlNodeOPTION(OptGroup.ChildNode[j]));
    end;
  if not Multiple and (SelectedText <> '') then
    with TComboBox(FControl) do
      ItemIndex := Items.IndexOf(SelectedText);
end;

function TIpHtmlNodeSELECT.Successful: Boolean;
begin
  Result := (Name <> '') and not Disabled;
  if Result then
    if FControl is TListBox then
      Result := TListBox(FControl).SelCount > 0
    else
      Result := TComboBox(FControl).ItemIndex <> -1;
end;

procedure TIpHtmlNodeSELECT.ButtonClick(Sender: TObject);
begin
  TIpHtmlOpener(Owner).ControlClick(Self);
end;

procedure TIpHtmlNodeSELECT.ControlOnEditingDone(Sender: TObject);
begin
  TIpHtmlOpener(Owner).ControlOnEditingDone(Self);
end;

procedure TIpHtmlNodeSELECT.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  TIpHtmlOpener(Owner).ControlOnEditingDone(Self);
end;

procedure TIpHtmlNodeSELECT.SetText(aText: string);
begin
  if FComboBox then TComboBox(FControl).Text := aText;
end;

function TIpHtmlNodeSELECT.getText: string;
begin
  if FComboBox then
    result := TComboBox(FControl).Text
  else if FMultiple then
    result := IntToStr(TComboBox(FControl).ItemIndex)
  else
    result := IntToStr(TComboBox(FControl).ItemIndex);
end;


{ TIpHtmlNodeTEXTAREA }

constructor TIpHtmlNodeTEXTAREA.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  ElementName := 'textarea';
end;

destructor TIpHtmlNodeTEXTAREA.Destroy;
begin
  inherited;
end;

procedure TIpHtmlNodeTEXTAREA.AddValues(NameList, ValueList: TStringList);
begin
  NameList.Add(Name);
  ValueList.Add(TMemo(FControl).Text);
end;

procedure TIpHtmlNodeTEXTAREA.CreateControl(Parent: TWinControl);
var
  i : Integer;
  S : string;
  B : PAnsiChar;
  iCurFontSize: integer;
  aCanvas : TCanvas;
begin
  inherited;
  TIpHtmlOpener(Owner).ControlCreate(Self);

  aCanvas := TFriendPanel(Parent).Canvas;
  iCurFontSize := aCanvas.Font.Size;
  FControl := TMemo.Create(Parent);
  FControl.Visible := False;
  FControl.Parent := Parent;
  TMemo(FControl).OnEditingDone:= ControlOnEditingDone;
  adjustFromCss;

  with TMemo(FControl) do begin
    Width := Cols * TFriendPanel(Parent).Canvas.TextWidth('0'); 
    Height := Rows * TFriendPanel(Parent).Canvas.TextHeight('Wy');
    Enabled := not Self.Disabled;
  end;

  for i := 0 to Pred(ChildCount) do
    if ChildNode[i] is TIpHtmlNodeText then begin
      S := TIpHtmlNodeText(ChildNode[i]).EscapedText;
      Getmem(B, length(S) + 1);
      try
        TrimFormatting(S, B);
        TMemo(FControl).Lines.Add(B);
      finally
        FreeMem(B);
      end;
    end;
    aCanvas.Font.Size := iCurFontSize;
end;

procedure TIpHtmlNodeTEXTAREA.Reset;
var
  i : Integer;
  S : string;
  B : PAnsiChar;
begin
  TMemo(FControl).Clear;
  for i := 0 to Pred(ChildCount) do
    if ChildNode[i] is TIpHtmlNodeText then begin
      S := TIpHtmlNodeText(ChildNode[i]).EscapedText;
      GetMem(B, length(S) + 1);
      try
        TrimFormatting(S, B);
        TMemo(FControl).Lines.Add(B);
      finally
        Freemem(B);
      end;
    end;
end;

function TIpHtmlNodeTEXTAREA.Successful: Boolean;
begin
  Result := trim(TMemo(FControl).Text) <> '';
end;

procedure TIpHtmlNodeTEXTAREA.ControlOnEditingDone(Sender: TObject);
begin
  TIpHtmlOpener(Owner).ControlOnEditingDone(Self);
end;


end.

