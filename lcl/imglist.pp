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
 *  See the file COPYING.LCL, included in this distribution,                 *
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

{$IFDEF VER1_0_10}
  {$DEFINE DisableFPImage}
{$ENDIF}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
  SysUtils, Classes, FPCAdds, LCLStrConsts, LCLIntf, LResources, LCLType,
  LCLProc, Graphics, GraphType, LCLClasses
  {$IFNDEF DisableFPImage}
  ,IntfGraphics, FPReadBMP
  {$ENDIF}
  ;

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

    Function GetCount: Integer;

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

    function Add(Image, Mask: TBitmap): Integer; // currently AddDirect
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
    procedure Draw(Canvas: TCanvas; X, Y, Index: Integer; Enabled: Boolean{$IFNDEF VER1_0}=True{$ENDIF});
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
    property Count: Integer read GetCount;
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

{
  $Log$
  Revision 1.26  2005/07/24 08:08:58  mattias
  fixed updating FCount in TCustomImageList.Insert/Delete

  Revision 1.25  2004/09/10 16:28:50  mattias
  implemented very rudimentary TTabControl

  Revision 1.24  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.23  2004/08/15 17:00:58  mattias
  improved DefineProperties to read/write endian independent

  Revision 1.22  2004/04/29 18:08:17  mattias
  fixed 1.0.10 compilation

  Revision 1.21  2004/04/03 16:47:46  mattias
  implemented converting gdkbitmap to RawImage mask

  Revision 1.20  2004/03/28 12:49:22  mattias
  implemented mask merge and extraction for raw images

  Revision 1.19  2004/03/22 19:10:04  mattias
  implemented icons for TPage in gtk, mask for TCustomImageList

  Revision 1.18  2004/03/17 00:34:37  marc
  * Interface reconstruction. Created skeleton units, classes and wscontrols

  Revision 1.17  2004/02/28 00:34:35  mattias
  fixed CreateComponent for buttons, implemented basic Drag And Drop

  Revision 1.16  2004/02/25 11:12:06  marc
  + Added delphi stream reading support

  Revision 1.15  2004/02/22 10:43:20  mattias
  added child-parent checks

  Revision 1.14  2004/02/02 19:13:31  mattias
  started reading TImageList in Delphi format

  Revision 1.13  2004/02/02 16:59:28  mattias
  more Actions  TAction, TBasicAction, ...

  Revision 1.12  2003/04/04 16:35:24  mattias
  started package registration

  Revision 1.11  2003/03/11 07:46:43  mattias
  more localization for gtk- and win32-interface and lcl

  Revision 1.10  2003/02/26 23:31:53  mattias
  added imagelisteditor from Olivier

  Revision 1.9  2002/12/16 12:12:50  mattias
  fixes for fpc 1.1

  Revision 1.8  2002/11/09 15:02:06  lazarus
  MG: fixed LM_LVChangedItem, OnShowHint, small bugs

  Revision 1.7  2002/08/06 19:57:39  lazarus
  MG: added actnlist.pp

  Revision 1.6  2002/05/10 06:05:50  lazarus
  MG: changed license to LGPL

  Revision 1.5  2002/02/03 00:24:00  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.4  2001/06/15 10:31:06  lazarus
  MG: set longstrings as default

  Revision 1.3  2001/02/06 13:55:23  lazarus
  Changed the files from mode delphi to mode objfpc
  Shane

  Revision 1.2  2001/01/11 20:16:47  lazarus
  Added some TImageList code.
  Added a bookmark resource with 10 resource images.
  Removed some of the IFDEF's in mwCustomEdit around the inherited code.
  Shane

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.4  1999/09/26 17:06:44  lazarus
  MWE: Exept for resource loading, streaming and icons, finished
       implementation of TCustomImageList.

  Revision 1.3  1999/09/26 15:37:20  lazarus
  MWE: implemented some more methods and documented most

  Revision 1.2  1999/08/20 15:44:40  lazarus
  TImageList changes added from Marc Weustink

  Revision 1.1  1999/08/12 16:21:54  lazarus
  Templates initially created    CAW

}

