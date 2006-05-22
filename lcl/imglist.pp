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
    Introduced by Marc Weustink <weus@quicknet.nl>

    ToDo:
      Delphis TCustomImageList has internally only one bitmap to hold all
      images. This reduces handle allocation, which is a problem under win9x,
      but it is not very fast.
      Because the LCL runs on many platforms, that do not have this limitations,
      the TCustomImageList should also support a one handle per image mode.
      The TCustomImageList should ask the interface, if handle allocation
      should be reduced and if so do it like Delphi.
      
      The current TCustomImageList is simply a list of bitmaps. The masks are
      not saved at all yet.

      So a lot ToDo.
  }
  TDrawingStyle = (dsFocus, dsSelected, dsNormal, dsTransparent);
  TImageType = (itImage, itMask);

  TCustomImageList = Class(TLCLComponent)
  private
    FDrawingStyle: TDrawingStyle;
    FImageList: TList;
    FMaskList: TList;
    FBitmap: TBitmap;
    FImageType: TImageType;
    FMaskBitmap: TBitmap;
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
    FHandle: THandle;
    FChanged: boolean;
    procedure AllocBitmap(Amount: Integer);
    procedure NotifyChangeLink;
    procedure SetBkColor(const Value: TColor);
    procedure SetDrawingStyle(const AValue: TDrawingStyle);
    procedure SetHeight(const Value: Integer);
    procedure SetMasked(const AValue: boolean);
    procedure SetShareImages(const AValue: Boolean);
    procedure SetWidth(const Value: Integer);

    procedure ShiftImages(const Source: TCanvas; Start, Shift: Integer);
  protected
    FUpdateCount: integer;
    procedure GetImages(Index: Integer; const Image, Mask: TBitmap);
    procedure Initialize; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetWidthHeight(NewWidth,NewHeight: integer); virtual;
  public
    constructor Create(TheOwner: TComponent); override;

    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteData(AStream: TStream); virtual;
    procedure ReadData(AStream: TStream); virtual;
    procedure BeginUpdate;
    procedure EndUpdate;

    function Add(Image, Mask: TBitmap): Integer; // using AddCopy for Delphi compatibility
    function AddDirect(Image, Mask: TBitmap): Integer;
    function AddCopy(SrcImage, SrcMask: TBitmap): Integer;
    function AddIcon(Image: TIcon): Integer;
    procedure AddImages(Value: TCustomImageList);
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    function AddFromLazarusResource(const ResourceName: string): integer;
    procedure Change;
    procedure Clear;
    constructor CreateSize(AWidth, AHeight: Integer);
    procedure Delete(Index: Integer);
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; X, Y, Index: Integer; Enabled: Boolean = True);
    procedure GetBitmap(Index: Integer; Image: TBitmap);
    procedure GetInternalImage(Index: integer; var Image, Mask: TBitmap;
                               var ImageRect: TRect);
    function GetHotSpot: TPoint; virtual;
    procedure GetIcon(Index: Integer; Image: TIcon);
    function HandleAllocated: Boolean;
    procedure Insert(Index: Integer; Image, Mask: TBitmap);
    procedure InsertIcon(Index: Integer; Image: TIcon);
    procedure InsertMasked(Index: Integer; Image: TBitmap; MaskColor: TColor);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Replace(Index: Integer; Image, Mask: TBitmap);
    procedure ReplaceIcon(Index: Integer; Image: TIcon);
    procedure ReplaceMasked(Index: Integer; NewImage: TBitmap; MaskColor: TColor);
    procedure RegisterChanges(Value: TChangeLink);
    procedure UnRegisterChanges(Value: TChangeLink);
  public
    property AllocBy: Integer read FAllocBy write FAllocBy default 4;
    property BlendColor: TColor read FBlendColor write FBlendColor default clNone;
    property BkColor: TColor read FBkColor write SetBkColor default clNone;
    property Count: Integer read FCount;
    property DrawingStyle: TDrawingStyle read FDrawingStyle write SetDrawingStyle default dsNormal;
    property Handle: THandle read FHandle;
    property Height: Integer read FHeight write SetHeight default 16;
    property Width: Integer read FWidth write SetWidth default 16;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Masked: boolean read FMasked write SetMasked;
    property Bitmap: TBitmap read FBitmap;
    property MaskBitmap: TBitmap read FMaskBitmap;
    property ShareImages: Boolean read FShareImages write SetShareImages;
    property ImageType: TImageType read FImageType write FImageType default itImage;
  end;


  { TImageList }
{  TImageList = class(TCustomImageList)
  published
    Property Height;
    Property Width;
  end;
}

implementation

uses dialogs;

{$I imglist.inc}

end.

