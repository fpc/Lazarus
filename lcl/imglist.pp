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
@lastmod(26-Sep-1999)

Detailed description of the Unit.
}
unit ImgList;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
  SysUtils, Classes, vclGlobals, Graphics, GraphType;

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
      not saved at all.
      
      So a lot ToDo.
  }
  TCustomImageList = Class(TComponent)
  private
    FImageList : TList;  //shane
    FBitmap: TBitmap;
    FMaskBitmap: TBitmap;
    FHeight: Integer;
    FWidth: Integer;
    FAllocBy: Integer;
    FCount: Integer;
    FAllocCount: Integer;
    FBlendColor: TColor;
    FOnChange: TNotifyEvent;
    FChangeLinkList: TList;
    FBkColor: TColor;
    FHandle: THandle;
    procedure AllocBitmap(Amount: Integer);
    procedure NotifyChangeLink;
    procedure SetBkColor(const Value: TColor);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);

    Function GetCount: Integer;

    procedure ShiftImages(const Source: TCanvas; Start, Shift: Integer);
  protected
    procedure GetImages(Index: Integer; const Image, Mask: TBitmap);
    procedure Initialize; virtual;
  public
    function Add(Image, Mask: TBitmap): Integer;
    function AddIcon(Image: TIcon): Integer;
    procedure AddImages(Value: TCustomImageList);
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    procedure Change;
    procedure Clear;
    constructor Create(AOwner : TComponent); override;
    constructor CreateSize(AWidth, AHeight: Integer);
    procedure Delete(Index: Integer);
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; X, Y, Index: Integer; Enabled: Boolean{=True});
    procedure GetBitmap(Index: Integer; Image: TBitmap);
    procedure GetInternalImage(Index: integer; var Image, Mask: TBitmap);
    function GetHotSpot: TPoint; virtual;
    procedure GetIcon(Index: Integer; Image: TIcon);
    function HandleAllocated: Boolean;
    procedure Insert(Index: Integer; Image, Mask: TBitmap);
    procedure InsertIcon(Index: Integer; Image: TIcon);
    procedure InsertMasked(Index: Integer; Image: TBitmap; MaskColor: TColor);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure RegisterChanges(Value: TChangeLink);
    procedure Replace(Index: Integer; Image, Mask: TBitmap);
    procedure ReplaceIcon(Index: Integer; Image: TIcon);
    procedure ReplaceMasked(Index: Integer; NewImage: TBitmap; MaskColor: TColor);
    procedure UnRegisterChanges(Value: TChangeLink);
    property AllocBy: Integer read FAllocBy write FAllocBy default 4;
    property BlendColor: TColor read FBlendColor write FBlendColor default clNone;
    property BkColor: TColor read FBkColor write SetBkColor default clNone;
    property Count: Integer read GetCount;
    property Handle: THandle read FHandle;
    property Height: Integer read FHeight write SetHeight default 16;
    property Width: Integer read FWidth write SetWidth default 16;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
// ----------------------
// for debugging purposes only
    property Bitmap: TBitmap read FBitmap;
    property MaskBitmap: TBitmap read FMaskBitmap;
// ----------------------
  published
    { no published parts }
  end;


implementation

{$I imglist.inc}

end.

{
  $Log$
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

