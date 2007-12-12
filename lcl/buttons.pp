{ $Id$}

{
 /***************************************************************************
                                 buttons.pp
                                 ----------
                             Component Library Code


                   Initial Revision : Sun Mar 28 23:15:32 CST 1999
                   Revised: Sat Jul 3 1999

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

unit Buttons;

{$mode objfpc}{$H+}


interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

uses
  Types, Classes, SysUtils, Math, LCLType, LCLProc, LCLIntf, LCLStrConsts,
  GraphType, Graphics, ImgList, ActnList, Controls, StdCtrls, LMessages, Forms,
  Themes, Menus{for ShortCut procedures}, LResources, ImageListCache;

type
  { TButton }

  TButtonLayout =
  (
    blGlyphLeft,
    blGlyphRight,
    blGlyphTop,
    blGlyphBottom
  );
  TButtonState =
  (
    bsUp,       // button is up
    bsDisabled, // button disabled (grayed)
    bsDown,     // button is down
    bsExclusive,// button is the only down in his group
    bsHot       // button is under mouse
  );

  {
   TNumGlyphs holds the number of glyphs in an image.
   If we change this the code in SetNumGlyphs for @link(TCustomSpeedButton)
   needs to be changed
  }
  TNumGlyphs = 1..5;

  {Some type aliases, because TButton is now in StdCtrls,
   but was in this unit in Lazarus 0.9.22 and earlier}
  TCustomButton = StdCtrls.TCustomButton;
  TButton = StdCtrls.TButton;

  { TButtonGlyph }

  TButtonGlyph = class(TObject, IUnknown, IImageCacheListener)
  private
    FImageIndexes: array[TButtonState] of Integer;
    FImages: TCustomImageList;
    FOriginal: TBitmap;
    FNumGlyphs: TNumGlyphs;
    FOnChange: TNotifyEvent;
    FImagesCache: TImageListCache;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
  protected
    // IUnknown
    function QueryInterface(const iid: tguid; out obj): longint; stdcall;
    function _AddRef: longint; stdcall;
    function _Release: longint; stdcall;

    // IImageCacheListener
    procedure CacheSetImageList(AImageList: TCustomImageList);
    procedure CacheSetImageIndex(AIndex, AImageIndex: Integer);

    procedure GlyphChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetImageIndexAndEffect(State: TButtonState; var AIndex: Integer; var AEffect: TGraphicsDrawEffect);
    function Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
                  State: TButtonState; Transparent: Boolean;
                  BiDiFlags: Longint): TRect;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property Images: TCustomImageList read FImages;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  { TCustomBitBtn }

  // when adding items here, also update TBitBtn.GetCaptionOfKind
  TBitBtnKind = (bkCustom, bkOK, bkCancel, bkHelp, bkYes, bkNo,
                 bkClose, bkAbort, bkRetry, bkIgnore, bkAll,
                 bkNoToAll, bkYesToAll);
  TBitBtnKinds = set of TBitBtnKind;

  TCustomBitBtn = class(TCustomButton)
  private
    FKind: TBitBtnKind;
    FLayout: TButtonLayout;
    FMargin: integer;
    FSpacing: Integer;
    FMultiGlyph: TBitmap;
    function GetGlyph: TBitmap;
    function GetNumGlyphs: Integer;
    Function IsGlyphStored: Boolean;
    Procedure SetGlyph(AValue: TBitmap);
    Procedure SetKind(AValue: TBitBtnKind);
    Procedure SetLayout(AValue: TButtonLayout);
    procedure SetMargin(const AValue: integer);
    procedure SetNumGlyphs(AValue: Integer);
    Procedure SetSpacing(AValue: Integer);
    procedure RealizeKind;
    //Return the caption associated with the aKind value.
    function GetCaptionOfKind(aKind: TBitBtnKind): String;
  protected
    FButtonGlyph: TButtonGlyph;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure GlyphChanged(Sender: TObject);
    procedure InitializeWnd; override;
    procedure TextChanged; override;
    class function GetControlClassDefaultSize: TPoint; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; Override;
    procedure Click; override;
  public
    property Glyph: TBitmap read GetGlyph write SetGlyph stored IsGlyphStored;
    property NumGlyphs: Integer read GetNumGlyphs write SetNumGlyphs default 1;
    property Kind: TBitBtnKind read FKind write SetKind default bkCustom;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: integer read FMargin write SetMargin default -1;
    property Spacing: Integer read FSpacing write SetSpacing default 3;
  end;

  { TBitBtn }
  { To set custom bitbtn glyphs for the whole application, see below for
    GetDefaultBitBtnGlyph }

  TBitBtn = class(TCustomBitBtn)
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Cancel;
    property Caption;
    property Color;
    property Constraints;
    property Default;
    property Enabled;
    property Font;
    property Glyph;
    property Kind;
    property Layout;
    property Margin;
    property ModalResult;
    property NumGlyphs;
    property OnChangeBounds;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Spacing;
    property TabOrder;
    property TabStop;
    property Visible;
  end;


  { TSpeedButtonActionLink }

  TSpeedButtonActionLink = class(TControlActionLink)
  protected
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    procedure SetGroupIndex(Value: Integer); override;
    procedure SetChecked(Value: Boolean); override;
  end;

  { TCustomSpeedButton }

  TCustomSpeedButton = class(TGraphicControl)
  private
    FAllowAllUp: Boolean;
    FDown: Boolean;
    FDownLoaded : Boolean;// value of Down set during loading
    FDragging: Boolean;
    FFlat: Boolean;
    FGlyph: TButtonGlyph;
    FGroupIndex: Integer;
    FLastDrawDetails: TThemedElementDetails;
    FLayout: TButtonLayout;
    FMargin: integer;
    FMouseInControl: Boolean;
    FShortcut: TShortCut;
    FShowAccelChar: boolean;
    FShowCaption: boolean;
    FSpacing: integer;
    FTransparent: Boolean;
    function GetGlyph: TBitmap;
    procedure SetShowCaption(const AValue: boolean);
    procedure UpdateExclusive;
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGlyph(Value: TBitmap);
    procedure SetLayout(const Value: TButtonLayout);
    procedure SetShowAccelChar(Value: boolean);
    procedure SetTransparent(const Value: boolean);
    procedure CMButtonPressed(var Message: TLMessage); message CM_BUTTONPRESSED;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
  private
    procedure DoBeforeMouseMessage;
    procedure DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
    procedure WMLButtonDown(Var Message: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TLMLButtonUp); message LM_LBUTTONUP;
    procedure WMLButtonDBLCLK(Var Message: TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
  protected
    FState: TButtonState;
    function GetNumGlyphs: Integer;
    procedure GlyphChanged(Sender: TObject);
    function  DialogChar(var Message: TLMKey): boolean; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure PaintBackground(var PaintRect: TRect); virtual;
    procedure SetDown(Value: Boolean);
    procedure SetGroupIndex(const Value: Integer);
    procedure SetFlat(const Value: Boolean);
    procedure SetMargin(const Value: integer);
    procedure SetNumGlyphs(Value: integer);
    procedure SetSpacing(const Value: integer);
    procedure RealSetText(const Value: TCaption); override;
    procedure SetEnabled(NewEnabled: boolean); override;
    procedure UpdateState(InvalidateOnChange: boolean); virtual;
    function GetDrawDetails: TThemedElementDetails; virtual;
    property MouseInControl: Boolean read FMouseInControl;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    class function GetControlClassDefaultSize: TPoint; override;
    procedure Loaded; override;
  protected
    function GetGlyphSize(PaintRect: TRect): TSize; virtual;
    function GetTextSize(PaintRect: TRect): TSize; virtual;
    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect; const AOffset: TPoint;
      AState: TButtonState; ATransparent: Boolean; BiDiFlags: Longint): TRect; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindDownButton: TCustomSpeedButton;
    procedure Click; override; // make Click public
  public
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default false;
    property Down: Boolean read FDown write SetDown default false;
    property Flat: Boolean read FFlat write SetFlat default false;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: integer read FMargin write SetMargin default -1;
    property NumGlyphs: Integer read GetNumGlyphs write SetNumGlyphs default 1;
    property ShowAccelChar: boolean read FShowAccelChar write SetShowAccelChar default true;
    property ShowCaption: boolean read FShowCaption write SetShowCaption default true;
    property Spacing: integer read FSpacing write SetSpacing default 4;
    property Transparent: Boolean read FTransparent write SetTransparent default true;
  end;


  { TSpeedButton }

  TSpeedButton = class(TCustomSpeedButton)
  published
    property Action;
    property Align;
    property Anchors;
    property AllowAllUp;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Color;
    property Down;
    property Enabled;
    property Flat;
    property Font;
    property Glyph;
    property GroupIndex;
    property Layout;
    property Margin;
    property NumGlyphs;
    property Spacing;
    property Transparent;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnChangeBounds;
    property ShowCaption;
    property ShowHint;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
  end;

  { To override the default TBitBtn glyphs set GetDefaultBitBtnGlyph below.
    Example:

    function GetBitBtnGlyph(Kind: TBitBtnKind): TBitmap;
    begin
      if Kind in [bkOK, bkCancel] then begin
        Result:=TBitmap.Create;
        case Kind of
          bkOk:      Result.Assign(MyOkGlyph);
          bkCancel:  Result.Assign(MyCancelGlyph);
        end;
      end else
        Result:=nil;
    end;
    }
type
  TGetDefaultBitBtnGlyph = function(Kind: TBitBtnKind; var Handled: Boolean): TBitmap;
var
  GetDefaultBitBtnGlyph: TGetDefaultBitBtnGlyph = nil;

function GetLCLDefaultBtnGlyph(Kind: TBitBtnKind): TGraphic;

procedure Register;

implementation

uses
  WSButtons;

const
  BitBtnModalResults: array[TBitBtnKind] of TModalResult = (
    0, mrOK, mrCancel, 0, mrYes, mrNo,
    0, mrAbort, mrRetry, mrIgnore, mrAll,
    mrNoToAll, mrYesToAll);

  BitBtnImages: array[TBitBtnKind] of Longint = (
    idButtonOk, idButtonOk, idButtonCancel, idButtonHelp, idButtonYes,
    idButtonNo, idButtonClose, idButtonAbort, idButtonRetry, idButtonIgnore,
    idButtonAll, idButtonNoToAll, idButtonYesToAll);

  BitBtnResNames: array[TBitBtnKind] of String =
  (
{bkCustom  } '',
{bkOK      } 'btn_ok',
{bkCancel  } 'btn_cancel',
{bkHelp    } 'btn_help',
{bkYes     } 'btn_yes',
{bkNo      } 'btn_no',
{bkClose   } 'btn_close',
{bkAbort   } 'btn_abort',
{bkRetry   } 'btn_retry',
{bkIgnore  } 'btn_ignore',
{bkAll     } 'btn_all',
{bkNoToAll } 'btn_no',
{bkYesToAll} 'btn_all'
  );

function GetLCLDefaultBtnGlyph(Kind: TBitBtnKind): TGraphic;
begin
  Result := nil;
  if BitBtnResNames[Kind] = '' then
    Exit;
  Result := LoadBitmapFromLazarusResource(BitBtnResNames[Kind]);
end;

procedure Register;
begin
  RegisterComponents('Additional',[TBitBtn,TSpeedButton]);
end;

{$I bitbtn.inc}
{$I buttonglyph.inc}
{$I speedbutton.inc}

initialization
  {$I btn_icons.lrs}
end.
