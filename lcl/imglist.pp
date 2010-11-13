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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
  LCLProc, Graphics, GraphType, LCLClasses, IntfGraphics, FPReadBMP,
  WSReferences;

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
    procedure Change; virtual;
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

  TDrawingStyle = (dsFocus, dsSelected, dsNormal, dsTransparent);
  TImageType = (itImage, itMask);

  TCustomImageList = class(TLCLReferenceComponent)
  private
    FReference: TWSCustomImageListReference;
    FDrawingStyle: TDrawingStyle;
    FData: array of TRGBAQuad;
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

    procedure AllocData(ACount: Integer);
    function  GetReference: TWSCustomImageListReference;

    procedure InternalInsert(AIndex: Integer; AImage, AMask: HBitmap;
      AWidth, AHeight: Integer);
    procedure InternalMove(ACurIndex, ANewIndex: Cardinal; AIgnoreCurrent: Boolean);
    procedure InternalReplace(AIndex: Integer; AImage, AMask: HBitmap);
    function  InternalSetImage(AIndex: Integer; AImage: TRawImage): PRGBAQuad;
    procedure NotifyChangeLink;
    procedure SetBkColor(const Value: TColor);
    procedure SetDrawingStyle(const AValue: TDrawingStyle);
    procedure SetHeight(const Value: Integer);
    procedure SetMasked(const AValue: boolean);
    procedure SetShareImages(const AValue: Boolean);
    procedure SetWidth(const Value: Integer);
  protected
    procedure CheckIndex(AIndex: Integer; AForInsert: Boolean = False);
    function  GetReferenceHandle: THandle; override;
    procedure Initialize; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetWidthHeight(NewWidth, NewHeight: integer); virtual;

    class procedure WSRegisterClass; override;
    function WSCreateReference(AParams: TCreateParams): PWSReference; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;

    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteData(AStream: TStream); virtual;
    procedure ReadData(AStream: TStream); virtual;
    function Equals(Obj: TObject): boolean;
      {$if not (defined(ver2_2_2) or defined(ver2_2_0))}{$IF FPC_FULLVERSION>=20402}override;{$ENDIF}{$ENDIF}
    procedure BeginUpdate;
    procedure EndUpdate;

    function Add(Image, Mask: TCustomBitmap): Integer;
    function AddIcon(Image: TIcon): Integer;
    procedure AddImages(AValue: TCustomImageList);
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    function AddLazarusResource(const ResourceName: string; MaskColor: TColor = clNone): integer;
    procedure Change;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer; AEnabled: Boolean = True); overload;
    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: Integer; ADrawEffect: TGraphicsDrawEffect); overload;
    procedure FillDescription(out ADesc: TRawImageDescription);
    procedure GetBitmap(Index: Integer; Image: TCustomBitmap); overload;
    procedure GetBitmap(Index: Integer; Image: TCustomBitmap; AEffect: TGraphicsDrawEffect); overload;
    procedure GetFullBitmap(Image: TCustomBitmap; AEffect: TGraphicsDrawEffect = gdeNormal);
    procedure GetFullRawImage(out Image: TRawImage);
    procedure GetRawImage(Index: Integer; out Image: TRawImage);
    function GetHotSpot: TPoint; virtual;

    procedure Insert(AIndex: Integer; AImage, AMask: TCustomBitmap);
    procedure InsertMasked(Index: Integer; AImage: TCustomBitmap; MaskColor: TColor);
    procedure Move(ACurIndex, ANewIndex: Integer);
    procedure Replace(AIndex: Integer; AImage, AMask: TCustomBitmap);
    procedure ReplaceMasked(Index: Integer; NewImage: TCustomBitmap; MaskColor: TColor);
    procedure RegisterChanges(Value: TChangeLink);
    procedure StretchDraw(Canvas: TCanvas; Index: Integer; ARect: TRect; Enabled: Boolean = True);
    procedure UnRegisterChanges(Value: TChangeLink);
  public
    property AllocBy: Integer read FAllocBy write FAllocBy default 4;
    property BlendColor: TColor read FBlendColor write FBlendColor default clNone;
    property BkColor: TColor read FBkColor write SetBkColor default clNone;
    property Count: Integer read FCount;
    property DrawingStyle: TDrawingStyle read FDrawingStyle write SetDrawingStyle default dsNormal;
    property HandleAllocated;
    property Height: Integer read FHeight write SetHeight default 16;
    property Width: Integer read FWidth write SetWidth default 16;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Masked: boolean read FMasked write SetMasked default False;
    property Reference: TWSCustomImageListReference read GetReference;
    property ShareImages: Boolean read FShareImages write SetShareImages default False;
    property ImageType: TImageType read FImageType write FImageType default itImage;
  end;

implementation

uses
  WSImglist;

{$I imglist.inc}

end.

