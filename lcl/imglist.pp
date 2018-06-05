{
 /***************************************************************************
                                  imglist.pp
                                  ----------
                Component Library TCustomImageList, TChangeLink Controls
                   Initial Revision  : Fri Aug 16 21:00:00 CET 1999


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{
@author(TCustomImageList - Marc Weustink <weus@quicknet.nl>)
@author(TChangeLink - Marc Weustink <weus@quicknet.nl>)
@created(16-Aug-1999)
@lastmod(26-feb-2003)

Detailed description of the Unit.

History
  26-feb-2003 Olivier Guilbaud <golivier@free.fr>
     - Add TCustomImageList.Assign()
     - Add TCustomImageList.WriteData()
     - Add TCustomImageList.ReadData()
     - Add override TCustomImageList.DefineProperties()
       Warning : the delphi or kylix format of datas is not compatible.
     - Modify Delete and Clear for preserve memory
}
unit ImgList;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
  // RTL + FCL
  Types, math, SysUtils, Classes, FPReadBMP, FPimage, FPImgCanv, FPCanvas,
  Contnrs, zstream,
  // LazUtils
  FPCAdds, Laz_AVL_Tree, AvgLvlTree,
  // LCL
  LCLStrConsts, LCLIntf, LResources, LCLType, LCLProc, Graphics, GraphType,
  LCLClasses, IntfGraphics,
  WSReferences, RtlConsts;

type
  TImageIndex = type integer;

  { TChangeLink }
  {
    @abstract(Use a TChangelink to get notified of imagelist changes)
    Introduced by Marc Weustink <weus@quicknet.nl>
    Currently maintained by Marc Weustink <weus@quicknet.nl>
  }

  TCustomImageList = class; //forward declaration

  TDestroyResolutionHandleEvent = procedure(Sender: TCustomImageList; AWidth: Integer; AReferenceHandle: TLCLHandle) of object;

  TChangeLink = class(TObject)
  private
    FSender: TCustomImageList;
    FOnChange: TNotifyEvent;
    FOnDestroyResolutionHandle: TDestroyResolutionHandleEvent;

    procedure DoDestroyResolutionReference(const AWidth: Integer; AResolutionReference: TLCLHandle);
  public
    destructor Destroy; override;
    procedure Change; virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDestroyResolutionHandle: TDestroyResolutionHandleEvent read FOnDestroyResolutionHandle write FOnDestroyResolutionHandle;
    property Sender: TCustomImageList read FSender write FSender;
  end;

  { TCustomImageList }
  {
    @abstract(Contains a list of images)
    Introduced by Marc Weustink <marc@dommelstein.net>

    Delphis TCustomImageList is based on the Win32 imagelists which has
    internally only one bitmap to hold all images. This reduces handle
    allocation.
    The original TCustomImageList implementation was LCL only based, so for
    other platforms the single bitmap implementation had some speed drawbacks.
    Therefore it was implemented as list of bitmaps, however it doesnt reduce
    handle allocation.
    In its current form, the imagelist is again based on a 32bit RGBA raw
    imagedata and the widgetset is notified when images are added or removed,
    so the widgetset can create its own optimal storage. The LCL keeps only the
    data, so all transparency info will be stored cross platform. (not all
    platforms have a 8bit alpha channel).

    NOTE: due to its implementation, the TCustomImageList is not a TBitmap
    collection. If a fast storage of bitmaps is needed, create your own list!
  }
  
  // Some temp rework defines, for old functionality both need so be set

  TDrawingStyle = (dsFocus, dsSelected, dsNormal, dsTransparent);
  TImageType = (itImage, itMask);
  TOverlay = 0..14; // windows limitation

  TCustomImageListResolution = class(TLCLReferenceComponent)
  private
    FWidth: Integer;
    FHeight: Integer;
    FData: array of TRGBAQuad;
    FReference: TWSCustomImageListReference;
    FAllocCount: Integer;
    FImageList: TCustomImageList;
    FCount: Integer;
    FAutoCreatedInDesignTime: Boolean;

    procedure AllocData(ACount: Integer);
    function  GetReference: TWSCustomImageListReference;

    function Add(Image, Mask: TCustomBitmap): Integer;
    procedure InternalInsert(AIndex: Integer; AData: PRGBAQuad); overload;
    procedure InternalMove(ACurIndex, ANewIndex: Cardinal; AIgnoreCurrent: Boolean);
    procedure InternalReplace(AIndex: Integer; AData: PRGBAQuad);
    function  InternalSetData(AIndex: Integer; AData: PRGBAQuad): PRGBAQuad;
    procedure CheckIndex(AIndex: Integer; AForInsert: Boolean = False);

    procedure Clear;
    procedure Delete(AIndex: Integer);

    procedure GetFullRawImage(out Image: TRawImage);

    procedure AddImages(AValue: TCustomImageListResolution);

    procedure WriteData(AStream: TStream; const ACompress: Boolean);
    procedure ReadData(AStream: TStream);
  protected
    function  GetReferenceHandle: THandle; override;
    function  WSCreateReference(AParams: TCreateParams): PWSReference; override;
    class procedure WSRegisterClass; override;
    procedure ReferenceDestroying; override;
  public
    destructor Destroy; override;
  public
    function GetHotSpot: TPoint; virtual;
    procedure FillDescription(out ADesc: TRawImageDescription);
    procedure GetBitmap(Index: Integer; Image: TCustomBitmap); overload;
    procedure GetBitmap(Index: Integer; Image: TCustomBitmap; AEffect: TGraphicsDrawEffect); overload;
    procedure GetIcon(Index: Integer; Image: TIcon; AEffect: TGraphicsDrawEffect); overload;
    procedure GetIcon(Index: Integer; Image: TIcon); overload;
    procedure GetFullBitmap(Image: TCustomBitmap; AEffect: TGraphicsDrawEffect = gdeNormal);
    procedure GetRawImage(Index: Integer; out Image: TRawImage);

    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer; AEnabled: Boolean = True); overload;
    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer; ADrawEffect: TGraphicsDrawEffect); overload;
    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer; ADrawingStyle: TDrawingStyle; AImageType: TImageType;
      AEnabled: Boolean = True); overload;
    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer; ADrawingStyle: TDrawingStyle; AImageType: TImageType;
      ADrawEffect: TGraphicsDrawEffect); overload; virtual;
    procedure StretchDraw(Canvas: TCanvas; Index: Integer; ARect: TRect; Enabled: Boolean = True);

    procedure DrawOverlay(ACanvas: TCanvas; AX, AY, AIndex: Integer; AOverlay: TOverlay; AEnabled: Boolean = True); overload;
    procedure DrawOverlay(ACanvas: TCanvas; AX, AY, AIndex: Integer; AOverlay: TOverlay; ADrawEffect: TGraphicsDrawEffect); overload;
    procedure DrawOverlay(ACanvas: TCanvas; AX, AY, AIndex: Integer; AOverlay: TOverlay; ADrawingStyle:
      TDrawingStyle; AImageType: TImageType; ADrawEffect: TGraphicsDrawEffect); overload;

    property ImageList: TCustomImageList read FImageList;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Count: Integer read FCount;

    property AutoCreatedInDesignTime: Boolean read FAutoCreatedInDesignTime write FAutoCreatedInDesignTime;

    property Reference: TWSCustomImageListReference read GetReference;
  end;

  { TScaledImageListResolution }

  TScaledImageListResolution = record
  private
    FResolution: TCustomImageListResolution;
    FScaleFactor: Double;
    FWidth: Integer;
    FHeight: Integer;
    function GetCount: Integer;
    function GetSize: TSize;
    function GetValid: Boolean;
  public
    class function Create(AResolution: TCustomImageListResolution;
      const AScaleFactor: Double): TScaledImageListResolution; static; // FPC record constructor bug

    procedure GetBitmap(Index: Integer; Image: TCustomBitmap); overload;
    procedure GetBitmap(Index: Integer; Image: TCustomBitmap; AEffect: TGraphicsDrawEffect); overload;

    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer; AEnabled: Boolean = True); overload;
    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer; ADrawEffect: TGraphicsDrawEffect); overload;
    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer; ADrawingStyle: TDrawingStyle; AImageType: TImageType;
      AEnabled: Boolean = True); overload;
    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer; ADrawingStyle: TDrawingStyle; AImageType: TImageType;
      ADrawEffect: TGraphicsDrawEffect); overload;
    procedure StretchDraw(Canvas: TCanvas; Index: Integer; ARect: TRect; Enabled: Boolean = True);

    procedure DrawOverlay(ACanvas: TCanvas; AX, AY, AIndex: Integer; AOverlay: TOverlay; AEnabled: Boolean = True); overload;
    procedure DrawOverlay(ACanvas: TCanvas; AX, AY, AIndex: Integer; AOverlay: TOverlay; ADrawEffect: TGraphicsDrawEffect); overload;
    procedure DrawOverlay(ACanvas: TCanvas; AX, AY, AIndex: Integer; AOverlay: TOverlay; ADrawingStyle:
      TDrawingStyle; AImageType: TImageType; ADrawEffect: TGraphicsDrawEffect); overload;

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Size: TSize read GetSize;
    property Resolution: TCustomImageListResolution read FResolution;
    property Count: Integer read GetCount;
    property Valid: Boolean read GetValid;
  end;

  TCustomImageListResolutionClass = class of TCustomImageListResolution;
  TCustomImageListResolutions = class(TObject)
  private
    FList: TObjectList;
    FImageList: TCustomImageList;
    FResolutionClass: TCustomImageListResolutionClass;

    function Find(const AImageWidth: Integer; out Index: Integer): Boolean;
    function GetImageLists(const AImageWidth: Integer): TCustomImageListResolution;
    function GetImageLists(const AImageWidth: Integer; const AScaleFromExisting,
      AutoCreatedInDesignTime: Boolean): TCustomImageListResolution;
    function GetItems(const AIndex: Integer): TCustomImageListResolution;
    function GetCount: Integer;
    function FindBestToCopyFrom(const ATargetWidth, AIgnoreIndex: Integer): Integer;
  public
    constructor Create(const AImageList: TCustomImageList; const AResolutionClass: TCustomImageListResolutionClass);
    destructor Destroy; override;
  public
    function FindBestToScaleFrom(const ATargetWidth: Integer): Integer;
    procedure Delete(const AIndex: Integer);
    procedure Clear;

    property ImageLists[const AImageWidth: Integer]: TCustomImageListResolution read GetImageLists;
    property Items[const AIndex: Integer]: TCustomImageListResolution read GetItems; default;
    property Count: Integer read GetCount;
  end;

  TCustomImageListResolutionEnumerator = class
  private
    FCurrent: Integer;
    FImgList: TCustomImageList;
    FDesc: Boolean;
    function GetCurrent: TCustomImageListResolution;
  public
    function GetEnumerator: TCustomImageListResolutionEnumerator;
    constructor Create(AImgList: TCustomImageList; ADesc: Boolean);
    function MoveNext: Boolean;
    property Current: TCustomImageListResolution read GetCurrent;
  end;

  TCustomImageListGetWidthForPPI = procedure(Sender: TCustomImageList;
    AImageWidth, APPI: Integer; var AResultWidth: Integer) of object;

  TCustomImageList = class(TLCLComponent)
  private
    FDrawingStyle: TDrawingStyle;
    FData: TCustomImageListResolutions;
    FImageType: TImageType;
    FHeight: Integer;
    FMasked: boolean;
    FShareImages: Boolean;
    FWidth: Integer;
    FAllocBy: Integer;
    FBlendColor: TColor;
    FOnChange: TNotifyEvent;
    FChangeLinkList: TList;
    FBkColor: TColor;
    FChanged: boolean;
    FUpdateCount: integer;
    FOverlays: array[TOverlay] of Integer;
    fHasOverlays: boolean;
    FOnGetWidthForPPI: TCustomImageListGetWidthForPPI;
    FScaled: Boolean;

    procedure NotifyChangeLink;
    procedure SetBkColor(const Value: TColor);
    procedure SetDrawingStyle(const AValue: TDrawingStyle);
    procedure SetHeight(const Value: Integer);
    procedure SetMasked(const AValue: boolean);
    procedure SetShareImages(const AValue: Boolean);
    procedure SetWidth(const Value: Integer);
    function GetReference(AImageWidth: Integer): TWSCustomImageListReference;
    function GetReferenceForPPI(AImageWidth, APPI: Integer): TWSCustomImageListReference;
    function GetResolutionForPPI(AImageWidth, APPI: Integer;
      const ACanvasScaleFactor: Double): TScaledImageListResolution;
    function GetWidthForPPI(AImageWidth, APPI: Integer): Integer;
    function GetHeightForPPI(AImageWidth, APPI: Integer): Integer;
    function GetCount: Integer;
    function GetSizeForPPI(AImageWidth, APPI: Integer): TSize;
    function GetBestIconIndexForSize(AIcon: TCustomIcon; AWidth: Integer): Integer; // the icon must be sorted
    function GetResolutionByIndex(AIndex: Integer): TCustomImageListResolution;
    function GetResolutionCount: Integer;
    procedure CreateDefaultResolution;
    procedure DoDestroyResolutionReference(const AWidth: Integer; AResolutionReference: TLCLHandle);
  protected
    function GetResolution(AImageWidth: Integer): TCustomImageListResolution;
    function GetResolutionClass: TCustomImageListResolutionClass; virtual;
    procedure CheckIndex(AIndex: Integer; AForInsert: Boolean = False);
    procedure Initialize; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetWidthHeight(NewWidth, NewHeight: integer);
    procedure ClearOverlays;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;

    class procedure ScaleImage(const ABitmap, AMask: TCustomBitmap;
      TargetWidth, TargetHeight: Integer; var AData: TRGBAQuadArray);
    class procedure ScaleImage(const ABitmap, AMask: TCustomBitmap;
      SourceRect: TRect; TargetWidth, TargetHeight: Integer; var AData: TRGBAQuadArray);
    class procedure ScaleImage(const ABitmap, AMask: HBITMAP;
      SourceRect: TRect; TargetWidth, TargetHeight: Integer; var AData: TRGBAQuadArray);

    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteData(AStream: TStream); virtual;
    procedure ReadData(AStream: TStream); virtual;
    procedure WriteAdvData(AStream: TStream); virtual;
    procedure ReadAdvData(AStream: TStream); virtual;
    function Equals(Obj: TObject): boolean;
      {$IF FPC_FULLVERSION>=20402}override;{$ENDIF}
    procedure BeginUpdate;
    procedure EndUpdate;

    function Add(Image, Mask: TCustomBitmap): Integer;
    function AddMultipleResolutions(Images: array of TCustomBitmap): Integer; // always pass sorted array from smallest to biggest
    function AddSliced(Image: TCustomBitmap; AHorizontalCount, AVerticalCount: Integer): Integer;
    function AddSlice(Image: TCustomBitmap; AImageRect: TRect): Integer;
    function AddSliceCentered(Image: TCustomBitmap): Integer;
    function AddIcon(Image: TCustomIcon): Integer;
    procedure AddImages(AValue: TCustomImageList);
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    function AddLazarusResource(const ResourceName: string; MaskColor: TColor = clNone): integer;
    function AddResourceName(Instance: THandle; const ResourceName: string; MaskColor: TColor = clNone): integer;
    procedure Change;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer; AEnabled: Boolean = True); overload;
    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer; ADrawEffect: TGraphicsDrawEffect); overload;
    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer; ADrawingStyle: TDrawingStyle; AImageType: TImageType;
      AEnabled: Boolean = True); overload;
    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer; ADrawingStyle: TDrawingStyle; AImageType: TImageType;
      ADrawEffect: TGraphicsDrawEffect); overload;
    procedure DrawForPPI(ACanvas: TCanvas; AX, AY, AIndex: Integer;
      AImageWidthAt96PPI, ATargetPPI: Integer; ACanvasFactor: Double; AEnabled: Boolean = True); overload;
    procedure DrawForPPI(ACanvas: TCanvas; AX, AY, AIndex: Integer;
      AImageWidthAt96PPI, ATargetPPI: Integer; ACanvasFactor: Double; ADrawEffect: TGraphicsDrawEffect); overload;
    procedure DrawOverlay(ACanvas: TCanvas; AX, AY, AIndex: Integer; AOverlay: TOverlay; AEnabled: Boolean = True); overload;
    procedure DrawOverlay(ACanvas: TCanvas; AX, AY, AIndex: Integer; AOverlay: TOverlay; ADrawEffect: TGraphicsDrawEffect); overload;
    procedure DrawOverlay(ACanvas: TCanvas; AX, AY, AIndex: Integer; AOverlay: TOverlay; ADrawingStyle:
      TDrawingStyle; AImageType: TImageType; ADrawEffect: TGraphicsDrawEffect); overload;

    procedure GetBitmap(Index: Integer; Image: TCustomBitmap); overload;
    procedure GetBitmap(Index: Integer; Image: TCustomBitmap; AEffect: TGraphicsDrawEffect); overload;
    procedure GetFullBitmap(Image: TCustomBitmap; AEffect: TGraphicsDrawEffect = gdeNormal);
    procedure GetFullRawImage(out Image: TRawImage);

    procedure GetIcon(Index: Integer; Image: TIcon; AEffect: TGraphicsDrawEffect); overload;
    procedure GetIcon(Index: Integer; Image: TIcon); overload;
    procedure GetRawImage(Index: Integer; out Image: TRawImage);
    function GetHotSpot: TPoint;

    procedure Insert(AIndex: Integer; AImage, AMask: TCustomBitmap);
    procedure InsertIcon(AIndex: Integer; AIcon: TCustomIcon);
    procedure InsertMasked(Index: Integer; AImage: TCustomBitmap; MaskColor: TColor);
    procedure Move(ACurIndex, ANewIndex: Integer);
    procedure Overlay(AIndex: Integer; Overlay: TOverlay);
    property HasOverlays: boolean read fHasOverlays;
    procedure Replace(AIndex: Integer; AImage, AMask: TCustomBitmap; const AllResolutions: Boolean = True);
    procedure ReplaceSlice(AIndex: Integer; Image: TCustomBitmap; AImageRect: TRect; const AllResolutions: Boolean = True);
    procedure ReplaceSliceCentered(AIndex, AImageWidth: Integer; Image: TCustomBitmap; const AllResolutions: Boolean = True);
    procedure ReplaceIcon(AIndex: Integer; AIcon: TCustomIcon);
    procedure ReplaceMasked(Index: Integer; NewImage: TCustomBitmap; MaskColor: TColor; const AllResolutions: Boolean = True);
    procedure RegisterChanges(Value: TChangeLink);
    procedure StretchDraw(Canvas: TCanvas; Index: Integer; ARect: TRect; Enabled: Boolean = True);
    procedure UnRegisterChanges(Value: TChangeLink);

    procedure RegisterResolutions(const AResolutionWidths: array of Integer); virtual;
    procedure DeleteResolution(const AWidth: Integer);
    function FindResolution(AImageWidth: Integer; out AResolution: TCustomImageListResolution): Boolean;
  public
    property AllocBy: Integer read FAllocBy write FAllocBy default 4;
    property BlendColor: TColor read FBlendColor write FBlendColor default clNone;
    property BkColor: TColor read FBkColor write SetBkColor default clNone;
    property Count: Integer read GetCount;
    property DrawingStyle: TDrawingStyle read FDrawingStyle write SetDrawingStyle default dsNormal;
    property Height: Integer read FHeight write SetHeight default 16;
    property HeightForPPI[AImageWidth, APPI: Integer]: Integer read GetHeightForPPI;
    property Width: Integer read FWidth write SetWidth default 16;
    property WidthForPPI[AImageWidth, APPI: Integer]: Integer read GetWidthForPPI;
    property SizeForPPI[AImageWidth, APPI: Integer]: TSize read GetSizeForPPI;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Masked: boolean read FMasked write SetMasked default False;
    property Reference[AImageWidth: Integer]: TWSCustomImageListReference read GetReference;
    property ReferenceForPPI[AImageWidth, APPI: Integer]: TWSCustomImageListReference read GetReferenceForPPI;
    property Resolution[AImageWidth: Integer]: TCustomImageListResolution read GetResolution;
    property ResolutionByIndex[AIndex: Integer]: TCustomImageListResolution read GetResolutionByIndex;
    property ResolutionForPPI[AImageWidth, APPI: Integer; const ACanvasScaleFactor: Double]: TScaledImageListResolution read GetResolutionForPPI;
    property ResolutionCount: Integer read GetResolutionCount;
    function Resolutions: TCustomImageListResolutionEnumerator;
    function ResolutionsDesc: TCustomImageListResolutionEnumerator;
    property Scaled: Boolean read FScaled write FScaled default False;
    property ShareImages: Boolean read FShareImages write SetShareImages default False;
    property ImageType: TImageType read FImageType write FImageType default itImage;
    property OnGetWidthForPPI: TCustomImageListGetWidthForPPI read FOnGetWidthForPPI write FOnGetWidthForPPI;
  end;

  TLCLGlyphsMissingResources = (
    gmrAllMustExist, // Show exception if any image/resolution is not found
    gmrOneMustExist, // Show exception if no resolution is found. Missing resolutions will be auto-generated from the biggest one.
    gmrIgnoreAll);   // Ignore all missing resources. No image will be added and TLCLGlyphs.GetImageIndex Result is -1 if no resolution is found.

  TLCLGlyphs = class(TCustomImageList)
  private type
    TEntryKey = record
      GlyphName: string;
    end;
    PEntryKey = ^TEntryKey;

    TEntry = class
    public
      // key
      GlyphName: string;

      // value
      ImageIndex: Integer; // the image index in TLCLGlyphs
    end;
    TResolution = record
      Width: Integer;
      ScaleSuffix: Integer;
    end;

  private
    FMissingResources: TLCLGlyphsMissingResources;
    FImageIndexes: TAvgLvlTree;
    FLoadResolutions: array of TResolution;
    FSuffix100Scale: Integer;
  public
    function GetImageIndex(const AResourceName: string): Integer;

    // AResolutionWidths must be sorted from smallest to biggest
    procedure RegisterResolutions(const AResolutionWidths: array of Integer); override;
    procedure RegisterResolutions(const AResolutionWidths, AResolutionScaleSuffixes: array of Integer); overload;

    procedure SetWidth100Suffix(const AWidth100Suffix: Integer);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property MissingResources: TLCLGlyphsMissingResources read FMissingResources write FMissingResources;
  end;

function LCLGlyphs: TLCLGlyphs;
function GetDefaultGlyph(ResourceName: string; ScalePercent: Integer = 100;
  IgnoreMissingResource: Boolean = False): TCustomBitmap;

implementation

uses
  WSImglist;

{$I imglist.inc}

end.

