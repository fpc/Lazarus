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
  Types, Classes, SysUtils, LCLType, LCLProc, LCLIntf, LCLStrConsts,
  GraphType, Graphics, ImgList, ActnList, Controls, StdCtrls, lMessages, Forms,
  Menus {for ShortCut procedures};

type
  { TButton }

  TButtonLayout = (blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom);
  TButtonState = (
    bsUp,       // button is up
    bsDisabled, // button disabled (grayed)
    bsDown,     // button is down
    bsExclusive // button is the only up in his group
    );

  {TNumGlyphs holds the number of glyphs in an image.
    We restrict it to 4 to stay compatible but we don't NEED to.
    If we change this the code in SetNumGlyphs for @link(TCustomSpeedButton)
    needs to be changed }
  TNumGlyphs = 1..4;

  { TCustomButton }

  TCustomButton = class(TButtonControl)
  private
    FCancel: Boolean;
    FDefault: Boolean;
    FActive: boolean;
    FModalResult: TModalResult;
    FShortCut: TShortcut;
    procedure SetCancel(NewCancel: boolean);
    procedure SetDefault(Value: Boolean);
    procedure SetModalResult(const AValue: TModalResult);
    procedure CMUIActivate(var Message: TLMessage); message CM_UIACTIVATE;
    procedure WMDefaultClicked(var Message: TLMessage); message LM_CLICKED;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
  protected
    procedure Click; override;
    procedure CreateWnd; override;
    procedure DoSendBtnDefault; virtual;
    procedure ControlKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ControlKeyUp(var Key: Word; Shift: TShiftState); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure RealSetText(const Value: TCaption); override;
    function DialogChar(var Message: TLMKey): boolean; override;
    function ChildClassAllowed(ChildClass: TClass): boolean; override;
    function IsBorderSpacingInnerBorderStored: Boolean; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure ExecuteDefaultAction; override;
    procedure ExecuteCancelAction; override;
    procedure ActiveDefaultControlChanged(NewControl: TControl); override;
    procedure UpdateRolesForForm; override;
  public
    property Active: boolean read FActive stored false;
    property Color default clBtnFace;
    property Default: Boolean read FDefault write SetDefault default false;
    property ModalResult: TModalResult read FModalResult write SetModalResult default mrNone;
    property Cancel: Boolean read FCancel write SetCancel default false;
    property TabStop default true;
  end;


  { TButton }

  TButton = class(TCustomButton)
  public
    procedure Click; override;
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
    property ModalResult;
    property OnChangeBounds;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;


  { TButtonGlyph }

  TButtonGlyph = class
  private
    FOriginal: TBitmap;
    FNumGlyphs: TNumGlyphs;
    FOnChange: TNotifyEvent;
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
  protected
    procedure GlyphChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    function Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
                  State: TButtonState; Transparent: Boolean;
                  BiDiFlags: Longint): TRect;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
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
    FButtonGlyph: TButtonGlyph;
    FKind: TBitBtnKind;
    FLayout: TButtonLayout;
    FMargin: integer;
    FSpacing: Integer;
    Function GetGlyph: TBitmap;
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
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure GlyphChanged(Sender: TObject);
    procedure InitializeWnd; override;
    procedure TextChanged; override;
    function IsBorderSpacingInnerBorderStored: Boolean; override;
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
    FDownBuffered : Boolean;//buffered value of FDown
    FDragging: Boolean;
    FFlat: Boolean;
    FGlyph:   TButtonGlyph;
    FGroupIndex: Integer;
    FLastDrawFlags: integer;
    FLayout: TButtonLayout;
    FMargin: integer;
    FMouseInControl: Boolean;
    FShortcut: TShortCut;
    FShowAccelChar: boolean;
    FSpacing: integer;
    FTransparent: Boolean;
    function GetGlyph: TBitmap;
    procedure UpdateExclusive;
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGlyph(Value: TBitmap);
    procedure SetLayout(const Value: TButtonLayout);
    procedure SetShowAccelChar(Value: boolean);
    procedure SetTransparent(const Value: boolean);
    procedure CMButtonPressed(var Message: TLMessage); message CM_BUTTONPRESSED;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
  protected
    FState: TButtonState;
    function GetNumGlyphs: Integer;
    procedure GlyphChanged(Sender: TObject);
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure SetDown(Value: Boolean);
    procedure SetGroupIndex(const Value: Integer);
    procedure SetFlat(const Value: Boolean);
    procedure SetMargin(const Value: integer);
    procedure SetNumGlyphs(Value: integer);
    procedure SetSpacing(const Value: integer);
    procedure RealSetText(const Value: TCaption); override;
    procedure SetEnabled(NewEnabled: boolean); override;
    procedure UpdateState(InvalidateOnChange: boolean); virtual;
    function GetDrawFlags: integer; virtual;
    property MouseInControl: Boolean read FMouseInControl;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
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
    property Spacing: integer read FSpacing write SetSpacing default 4;
    property ShowAccelChar: boolean read FShowAccelChar write SetShowAccelChar default true;
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
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnChangeBounds;
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
  TGetDefaultBitBtnGlyph = function(Kind: TBitBtnKind): TBitmap;
var
  GetDefaultBitBtnGlyph: TGetDefaultBitBtnGlyph = nil;


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

procedure Register;
begin
  RegisterComponents('Standard',[TButton]);
  RegisterComponents('Additional',[TBitBtn,TSpeedButton]);
end;

{$I buttons.inc}
{$I bitbtn.inc}
{$I buttonglyph.inc}
{$I speedbutton.inc}

end.
