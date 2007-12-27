{
 /***************************************************************************
                                  imglist.pp
                                  ----------
                Component Library TCustomImageList, TChangeLink Controls
                   Initial Revision  : Fri Aug 16 21:00:00 CET 1999


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
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

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
  SysUtils, Classes, FPCAdds, LCLStrConsts, LCLIntf, LResources, LCLType,
  LCLProc, Graphics, GraphType, LCLClasses, IntfGraphics, FPReadBMP;

type
  TImageIndex = type integer;

  { TChangeLink }
  {
    @abstract(Use a TChangelink to get notified of imagelist changes)
    Introduced by Marc Weustink <weus@quicknet.nl>
    Currently maintained by Marc Weustink <weus@quicknet.nl>
  }

  TCustomImageList = class; //forward declaration

  TChangeLink = class(TObject)
  private
    FSender: TCustomImageList;
    FOnChange: TNotifyEvent;
  public
    destructor Destroy; override;
    procedure Change; dynamic;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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

  {.$define IMGLIST_OLDSTYLE}     // Set to keep original functionality
  {.$define IMGLIST_KEEP_EXTRA}   // Not needed for Delphi compat.

  {$ifdef IMGLIST_OLDSTYLE}
  // hack to set defines in dependent widgetsets.
  TOldstyleCustomImageList = Boolean;
  {$endif}

  TDrawingStyle = (dsFocus, dsSelected, dsNormal, dsTransparent);
  TImageType = (itImage, itMask);

  TCustomImageList = class(TLCLHandleComponent)
  private
    FDrawingStyle: TDrawingStyle;
    FData: array of TRGBAQuad;
    {$ifdef IMGLIST_OLDSTYLE}
    FHandle: THandle;
    FImageList: TList;
    FMaskList: TList;
    FBitmap: TBitmap;
    FMaskBitmap: TBitmap;
    {$endif}
    FImageType: TImageType;
    FHeight: Integer;
    FMasked: boolean;
    FShareImages: Boolean;
    FWidth: Integer;
    FAllocBy: Integer;
    FCount: Integer;
    FAllocCount: Integer;
    FBlendColor: TColor;
    FOnChange: TNotifyEvent;
    FChangeLinkList: TList;
    FBkColor: TColor;
    FChanged: boolean;
    FUpdateCount: integer;

    {$ifdef IMGLIST_OLDSTYLE}
    procedure AllocBitmap(Amount: Integer);
    {$else}
    procedure AllocData(ACount: Integer);
    {$endif}
    
    {$ifndef IMGLIST_OLDSTYLE}
    procedure InternalMove(ACurIndex, ANewIndex: Cardinal; AIgnoreCurrent: Boolean);
    function InternalSetImage(AIndex: Integer; AImage: TRawImage): PRGBAQuad;
    {$endif}

    procedure NotifyChangeLink;
    procedure SetBkColor(const Value: TColor);
    procedure SetDrawingStyle(const AValue: TDrawingStyle);
    procedure SetHeight(const Value: Integer);
    procedure SetMasked(const AValue: boolean);
    procedure SetShareImages(const AValue: Boolean);
    procedure SetWidth(const Value: Integer);

    {$ifdef IMGLIST_OLDSTYLE}
    procedure ShiftImages(const Source: TCanvas; Start, Shift: Integer);
    {$endif}
  protected
    procedure CheckIndex(AIndex: Integer; AForInsert: Boolean = False);
    procedure GetImages(Index: Integer; const Image, Mask: TBitmap);
    procedure Initialize; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetWidthHeight(NewWidth,NewHeight: integer); virtual;

    function  WSCreateHandle(AParams: TCreateParams): TLCLIntfHandle; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteData(AStream: TStream); virtual;
    procedure ReadData(AStream: TStream); virtual;
    procedure BeginUpdate;
    procedure EndUpdate;

    function Add(Image, Mask: TBitmap): Integer;
    {$ifdef IMGLIST_OLDSTYLE}
    function AddDirect(Image, Mask: TBitmap): Integer;
    function AddCopy(SrcImage, SrcMask: TBitmap): Integer;
    {$endif}
    function AddIcon(Image: TIcon): Integer;
    procedure AddImages(AValue: TCustomImageList);
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    function AddLazarusResource(const ResourceName: string; MaskColor: TColor = clNone): integer;
    procedure Change;
    procedure Clear;
    {.$ifdef IMGLIST_KEEP_EXTRA}
    constructor CreateSize(AWidth, AHeight: Integer);
    {.$endif}
    procedure Delete(AIndex: Integer);
    destructor Destroy; override;
    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer; AEnabled: Boolean = True); overload;
    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer; ADrawEffect: TGraphicsDrawEffect); overload;
    procedure FillDescription(out ADesc: TRawImageDescription);
    procedure GetBitmap(Index: Integer; Image: TBitmap); overload;
    procedure GetBitmap(Index: Integer; Image: TBitmap; AEffect: TGraphicsDrawEffect); overload;
    procedure GetRawImage(Index: Integer; out Image: TRawImage);
    {$ifdef IMGLIST_KEEP_EXTRA}
    procedure GetInternalImage(Index: integer; var Image, Mask: TBitmap;
                               var ImageRect: TRect);
    {$endif}
    function GetHotSpot: TPoint; virtual;
    procedure GetIcon(Index: Integer; Image: TIcon);

    {$ifdef IMGLIST_OLDSTYLE}
    function HandleAllocated: Boolean;
    {$endif}
    procedure Insert(AIndex: Integer; AImage, AMask: TBitmap);
    procedure InsertIcon(Index: Integer; Image: TIcon);
    procedure InsertMasked(Index: Integer; Image: TBitmap; MaskColor: TColor);
    procedure Move(ACurIndex, ANewIndex: Integer);
    procedure Replace(AIndex: Integer; AImage, AMask: TBitmap);
    procedure ReplaceIcon(Index: Integer; Image: TIcon);
    procedure ReplaceMasked(Index: Integer; NewImage: TBitmap; MaskColor: TColor);
    procedure RegisterChanges(Value: TChangeLink);
    procedure StretchDraw(Canvas: TCanvas; Index: Integer; ARect: TRect; Enabled: Boolean = True);
    procedure UnRegisterChanges(Value: TChangeLink);
  public
    property AllocBy: Integer read FAllocBy write FAllocBy default 4;
    property BlendColor: TColor read FBlendColor write FBlendColor default clNone;
    property BkColor: TColor read FBkColor write SetBkColor default clNone;
    property Count: Integer read FCount;
    property DrawingStyle: TDrawingStyle read FDrawingStyle write SetDrawingStyle default dsNormal;
    {$ifdef IMGLIST_OLDSTYLE}
    property Handle: THandle read FHandle;
    {$else}
    property HandleAllocated;
    {$endif}
    property Height: Integer read FHeight write SetHeight default 16;
    property Width: Integer read FWidth write SetWidth default 16;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Masked: boolean read FMasked write SetMasked;
    {$ifdef IMGLIST_KEEP_EXTRA}
    property Bitmap: TBitmap read FBitmap;
    property MaskBitmap: TBitmap read FMaskBitmap;
    {$endif}
    property ShareImages: Boolean read FShareImages write SetShareImages;
    property ImageType: TImageType read FImageType write FImageType default itImage;
  end;

implementation

uses
  dialogs,
  WSImglist;

{$I imglist.inc}

end.

