{ $Id$}

{
 /***************************************************************************
                                 buttons.pp
                                 ----------
                             Component Library Code


                   Initial Revision  : Sun Mar 28 23:15:32 CST 1999
                   Revised : Sat Jul 3 1999

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

unit Buttons;

{$mode objfpc}{$H+}


interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

uses StdCtrls, VCLGlobals, Classes, LCLType, LCLProc, LCLLinux,LCLStrConsts,
  GraphType, Graphics, SysUtils, Controls, lMessages, Forms, Messages;

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
    We restrict it to 4 to stay compatable but we don't NEED to.
    If we change this the code in SetNumGlyphs for @link(TSpeedButton) needs to
    be changed }
  TNumGlyphs = 1..4;

  TButton = class(TButtonControl)
  private
    FCancel : Boolean;
    FDefault : Boolean;
    FModalResult : TModalResult;
    //fOwner: TControl;
    //FOnPressed: TNotifyEvent;
    //FOnReleased: TNotifyEvent;
    FOnLeave: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    //FOnResize: TNotifyEvent;
    FShortCut : TLMShortcut;
    Procedure SetDefault(Value : Boolean);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER; 
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMDefaultClicked(var Message: TMessage); message LM_CLICKED;
  protected
    procedure Click; override;
    //TODO: make this compatible
    procedure CreateWnd; override;
    procedure DoSendBtnDefault; virtual;

    property OnMouseEnter : TNotifyEvent read FOnEnter write FOnEnter;
    property OnMouseLeave : TNotifyEvent read FOnLeave write FOnLeave;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetText(const Value: TCaption); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Anchors;
    property Default : Boolean read FDefault write SetDefault default false;
    property Enabled;
    property ModalResult : TModalResult read FModalResult write FModalResult default mrNone;
    property Cancel : Boolean read FCancel write FCancel default False;
    property Caption;
    property Font;
    property TabStop;
    property TabOrder;
    property PopupMenu;
    property ShowHint;
    property ParentShowHint;
    property Visible;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


  { TButtonGlyph }

  TButtonGlyph = class
  private
    FOriginal : TBitmap;
    FNumGlyphs : TNumGlyphs;

    FOnChange  : TNotifyEvent;
    procedure SetGlyph(Value : TBitmap);
    procedure SetNumGlyphs(Value : TNumGlyphs);
  protected
    procedure GlyphChanged(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    function Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      State: TButtonState; Transparent: Boolean; BiDiFlags: Longint): TRect;
    property Glyph : TBitmap read FOriginal write SetGlyph;
    property NumGlyphs : TNumGlyphs read FNumGlyphs write SetNumGlyphs;

    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;


  { TBitBtn }

  TBitBtnKind = (bkCustom, bkOK, bkCancel, bkHelp,bkYes, bkNo,
                 bkClose, bkAbort, bkRetry, bkIgnore, bkAll);

  TBitBtn = class(TButton)
  private
    FCanvas : TCanvas;
    FGlyph  : TButtonGlyph;
    FKind   : TBitBtnKind;
    FLayout : TButtonLayout;
    FSpacing : Integer;
    Function GetGlyph : TBitmap;
    Function IsCustom : Boolean;
    Function IsGlyphStored : Boolean;
    Procedure SetGlyph(Value : TBitmap);
    Procedure SetKind(Value : TBitBtnKind);
    Procedure SetLayout(Value : TButtonLayout);
    Procedure SetSpacing(Value : Integer);
    
    //Return the caption associed with the akind value.
    function GetCaptionOfKind(aKind :TBitBtnKind) : String;
  protected
    Procedure Click; override;
    procedure GlyphChanged(Sender : TObject);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; Override;
  published
    property Default stored IsCustom;
    property Glyph : TBitmap read GetGlyph write SetGlyph stored IsGlyphStored;
    property Kind : TBitBtnKind read FKind write SetKind;
    property Layout: TButtonLayout read FLayout write SetLayout;
    property ModalResult stored IsCustom;
    property Spacing : Integer read FSpacing write SetSpacing;
    property Visible;
    property OnEnter;
    property OnExit;
    property ShowHint;
    property ParentShowHint;
  end;
   
   
  { TSpeedButton }
  
  TMyCollectionItem = class(TCollectionItem)
  end;

  TSpeedButton = class(TGraphicControl)
  private
    FAllowAllUp : Boolean;
    FDown : Boolean;
    FDragging : Boolean;
    FFlat : Boolean;
    FGlyph:   TButtonGlyph;
    FGroupIndex : Integer;
    FLastDrawFlags: integer;
    FLayout: TButtonLayout;
    FMargin : integer;
    FMouseInControl : Boolean;
    FShortcut : Longint;
    FSpacing : integer;
    FState : TButtonState;
    FTransparent : Boolean;
    function GetGlyph : TBitmap;
    procedure UpdateExclusive;
    procedure UpdateTracking;
    procedure SetAllowAllUp(Value : Boolean);
    procedure SetGlyph(value : TBitmap);
    procedure SetLayout(const Value : TButtonLayout);
    procedure SetTransparent(const Value : boolean);
    procedure CMButtonPressed(var MEssage : TLMessage); message CM_BUTTONPRESSED;
    procedure CMMouseEnter(var Message :TLMessage); message CM_MouseEnter;
    procedure CMMouseLeave(var Message :TLMessage); message CM_MouseLeave;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
  protected
    function GetNumGlyphs : Integer;
    procedure GlyphChanged(Sender : TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure SetDown(Value : Boolean);
    procedure SetGroupIndex(const Value : Integer);
    procedure SetFlat(const Value : Boolean);
    procedure SetMargin(const Value : integer);
    procedure SetNumGlyphs(Value : integer);
    procedure SetSpacing(const Value : integer);
    procedure SetText(const Value: TCaption); override;
    procedure UpdateState(InvalidateOnChange: boolean); virtual;
    function GetDrawFlags: integer; virtual;
    property MouseInControl : Boolean read FMouseInControl;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default false;
    property Caption;
    property Down: Boolean read FDown write SetDown default false;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default false;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: integer read FMargin write SetMargin default -1;
    property NumGlyphs: Integer read GetNumGlyphs write SetNumGlyphs default 1;
    property Spacing: integer read FSpacing write SetSpacing default 4;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property ShowHint;
    property ParentShowHint;
  end;


implementation


const
  BitbtnCaption : array[TBitBtnKind] of String = (
    '', rsmbOK, rsmbCancel, rsmbHelp, rsmbYes, rsmbNo,
    rsmbClose, rsmbAbort, rsmbRetry, rsmbIgnore, rsmbAll);

  BitBtnModalResults : array[TBitBtnKind] of TModalResult = (
    0, mrOK, mrCancel, 0, mrYes, mrNo,
    0, mrAbort, mrRetry, mrIgnore, mrAll);

  BitBtnImages : array[TBitBtnKind] of Longint = (
    idButtonOk, idButtonOk, idButtonCancel, idButtonHelp, idButtonYes,
    idButtonNo, idButtonClose, idButtonAbort, idButtonRetry, idButtonIgnore,
    idButtonAll);

{$I buttons.inc}
{$I bitbtn.inc}
{$I buttonglyph.inc}
{$I speedbutton.inc}

initialization
finalization

end.



{ =============================================================================

  $Log$
  Revision 1.36  2003/02/28 10:21:16  mattias
  lcl localization from Olivier

  Revision 1.35  2003/02/06 06:33:57  mattias
  fixed message

  Revision 1.34  2003/01/27 13:49:16  mattias
  reduced speedbutton invalidates, added TCanvas.Frame

  Revision 1.33  2002/12/25 11:53:47  mattias
  Button.Default now sets focus

  Revision 1.32  2002/10/25 10:42:07  lazarus
  MG: broke minor circles

  Revision 1.31  2002/10/25 09:47:37  lazarus
  MG: added inputdialog.inc

  Revision 1.30  2002/10/10 13:29:08  lazarus
  AJ: added LoadStockPixmap routine & minor fixes to/for GNOMEInt

  Revision 1.29  2002/10/01 18:00:02  lazarus
  AJ: Initial TUpDown, minor property additions to improve reading Delphi created forms.

  Revision 1.28  2002/09/27 20:52:20  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

  Here is the run down of what it includes -

   -Vasily Volchenko's Updated Russian Localizations

   -improvements to GTK Styles/SysColors
   -initial GTK Palette code - (untested, and for now useless)

   -Hint Windows and Modal dialogs now try to stay transient to
    the main program form, aka they stay on top of the main form
    and usually minimize/maximize with it.

   -fixes to Form BorderStyle code(tool windows needed a border)

   -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
    when flat

   -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
    and to match GTK theme better. It works most of the time now,
    but some themes, noteably Default, don't work.

   -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
    mode.

   -misc other cleanups/ fixes in gtk interface

   -speedbutton's should now draw correctly when flat in Win32

   -I have included an experimental new CheckBox(disabled by
    default) which has initial support for cbGrayed(Tri-State),
    and WordWrap, and misc other improvements. It is not done, it
    is mostly a quick hack to test DrawFrameControl
    DFCS_BUTTONCHECK, however it offers many improvements which
    can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

   -fixes Message Dialogs to more accurately determine
    button Spacing/Size, and Label Spacing/Size based on current
    System font.
   -fixes MessageDlgPos, & ShowMessagePos in Dialogs
   -adds InputQuery & InputBox to Dialogs

   -re-arranges & somewhat re-designs Control Tabbing, it now
    partially works - wrapping around doesn't work, and
    subcontrols(Panels & Children, etc) don't work. TabOrder now
    works to an extent. I am not sure what is wrong with my code,
    based on my other tests at least wrapping and TabOrder SHOULD
    work properly, but.. Anyone want to try and fix?

   -SynEdit(Code Editor) now changes mouse cursor to match
    position(aka over scrollbar/gutter vs over text edit)

   -adds a TRegion property to Graphics.pp, and Canvas. Once I
    figure out how to handle complex regions(aka polygons) data
    properly I will add Region functions to the canvas itself
    (SetClipRect, intersectClipRect etc.)

   -BitBtn now has a Stored flag on Glyph so it doesn't store to
    lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
    bkOk, bkCancel, etc.) This should fix most crashes with older
    GDKPixbuf libs.

  Revision 1.27  2002/09/13 16:58:27  lazarus
  MG: removed the 1x1 bitmap from TBitBtn

  Revision 1.26  2002/09/09 07:26:42  lazarus
  MG: started TCollectionPropertyEditor

  Revision 1.25  2002/09/05 14:57:55  lazarus
  MG: added show pages menu to TNotebookcomponenteditor

  Revision 1.24  2002/09/04 09:32:17  lazarus
  MG: improved streaming error handling

  Revision 1.23  2002/09/03 11:32:49  lazarus

  Added shortcut keys to labels
  Support for alphabetically sorting the properties
  Standardize message and add shortcuts ala Kylix
  Published BorderStyle, unpublished BorderWidth
  ShowAccelChar and FocusControl
  ShowAccelChar and FocusControl for TLabel, escaped ampersands now work.

  Revision 1.22  2002/09/03 08:07:17  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.21  2002/09/01 16:11:21  lazarus
  MG: double, triple and quad clicks now works

  Revision 1.20  2002/08/30 06:46:03  lazarus

  Use comboboxes. Use history. Prettify the dialog. Preselect text on show.
  Make the findreplace a dialog. Thus removing resiying code (handled by Anchors now anyway).
  Make Anchors work again and publish them for various controls.
  SelStart and Co. for TEdit, SelectAll procedure for TComboBox and TEdit.
  Clean up and fix some bugs for TComboBox, plus selection stuff.

  Revision 1.19  2002/08/28 11:41:53  lazarus
  MG: activated environment opts in debugger

  Revision 1.18  2002/08/27 06:40:50  lazarus
  MG: ShortCut support for buttons from Andrew

  Revision 1.17  2002/08/26 17:28:20  lazarus
  MG: fixed speedbutton in designmode

  Revision 1.16  2002/08/19 20:34:47  lazarus
  MG: improved Clipping, TextOut, Polygon functions

  Revision 1.15  2002/05/10 06:05:48  lazarus
  MG: changed license to LGPL

  Revision 1.14  2002/03/25 17:59:19  lazarus
  GTK Cleanup
  Shane

  Revision 1.13  2002/02/24 20:51:23  lazarus
  Improved TSpeedButton (Glyph, Spacing, Margin, drawing)
  Added PageCount to TNotebook
  Optimized component selection buttons a bit.

  Revision 1.12  2002/02/06 08:58:29  lazarus
  MG: fixed compiler warnings and asking to create non existing files

  Revision 1.11  2002/02/03 00:24:00  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.10  2001/10/16 10:51:09  lazarus
  MG: added clicked event to TButton, MessageDialog reacts to return key

  Revision 1.9  2001/06/14 14:57:58  lazarus
  MG: small bugfixes and less notes

  Revision 1.7  2001/01/09 21:06:06  lazarus
  Started taking KeyDown messages in TDesigner
  Shane

  Revision 1.6  2001/01/03 18:44:54  lazarus
  The Speedbutton now has a numglyphs setting.
  I started the TStringPropertyEditor

  Revision 1.5  2000/12/01 18:12:40  lazarus
  Modified Gloabal so TDesignForm isn't included anymore.
  Shane

  Revision 1.4  2000/12/01 15:50:39  lazarus
  changed the TCOmponentInterface SetPropByName.  It works for a few properties, but not all.
  Shane

  Revision 1.3  2000/11/29 21:22:35  lazarus
  New Object Inspector code
  Shane

  Revision 1.2  2000/07/16 12:44:31  lazarus
  added OnMouseEnter, OnMouseLeave property (changes by chris, committed by stoppok)

  Revision 1.1  2000/07/13 10:28:23  michael
  + Initial import

  Revision 1.33  2000/07/03 17:29:18  lazarus
  Some cleanups/enhancements to bitbtn by christer.t.johansson@se.abb.com (committed by stoppok)

  Revision 1.32  2000/06/13 20:50:42  lazarus
  MWE:
    - Started to remove obsolete/dead code/messages

  HJO:
    * Fixed messages in showmodal of 2nd form
    * Fixed modal result for button

  Revision 1.31  2000/05/14 21:56:11  lazarus
  MWE:
    + added local messageloop
    + added PostMessage
    * fixed Peekmessage
    * fixed ClientToScreen
    * fixed Flat style of Speedutton (TODO: Draw)
    + Added TApplicatio.OnIdle

  Revision 1.30  2000/05/08 12:54:19  lazarus
  Removed some writeln's
  Added alignment for the TLabel.  Isn't working quite right.
  Added the shell code for WindowFromPoint and GetParent.
  Added FindLCLWindow
  Shane

  Revision 1.29  2000/03/23 20:40:03  lazarus
  Added some drag code
  Shane

  Revision 1.28  2000/03/22 17:09:29  lazarus
  *** empty log message ***

  Revision 1.27  2000/03/21 20:42:54  lazarus
  *** empty log message ***

  Revision 1.23  2000/03/21 14:40:06  lazarus
  Playing with bitbtn's
  Shane

  Revision 1.22  2000/03/21 14:29:13  lazarus
  *** empty log message ***

  Revision 1.21  2000/03/15 20:15:31  lazarus
  MOdified TBitmap but couldn't get it to work
  Shane

  Revision 1.20  2000/03/14 21:18:23  lazarus
  Added the ability to click on the speedbuttons
  Shane

  Revision 1.18  2000/03/10 18:31:09  lazarus
  Added TSpeedbutton code
  Shane

  Revision 1.17  2000/03/10 12:55:07  lazarus
  *** empty log message ***

  Revision 1.16  2000/03/07 19:00:15  lazarus
  Minor changes.  Added the skeleton for TSpeedbutton
  Shane

  Revision 1.15  2000/03/07 16:52:58  lazarus
  Fixxed a problem with the main.pp unit determining a new files FORM name.
  Shane

  Revision 1.14  2000/03/03 20:22:02  lazarus
  Trying to add TBitBtn
  Shane

  Revision 1.13  2000/02/28 19:16:03  lazarus
  Added code to the FILE CLOSE to check if the file was modified.  HAven't gotten the application.messagebox working yet though.  It won't stay visible.
  Shane

  Revision 1.12  2000/01/11 19:12:37  lazarus
  Added some test code for ShowCaret
  Shane

  Revision 1.11  2000/01/10 21:24:12  lazarus
  Minor cleanup and changes.

  Revision 1.10  1999/12/18 18:27:31  lazarus
  MWE:
    Rearranged some events to get a LM_SIZE, LM_MOVE and LM_WINDOWPOSCHANGED
    Initialized the TextMetricstruct to zeros to clear unset values
    Get mwEdit to show more than one line
    Fixed some errors in earlier commits

  Revision 1.9  1999/12/10 00:47:00  lazarus
  MWE:
    Fixed some samples
    Fixed Dialog parent is no longer needed
    Fixed (Win)Control Destruction
    Fixed MenuClick

  Revision 1.8  1999/11/01 01:28:29  lazarus
  MWE: Implemented HandleNeeded/CreateHandle/CreateWND
       Now controls are created on demand. A call to CreateComponent shouldn't
       be needed. It is now part of CreateWnd

  Revision 1.7  1999/10/19 19:16:51  lazarus
  renamed stdcontrols.pp stdctrls.pp
  Shane

  Revision 1.6  1999/08/07 17:59:09  lazarus

        buttons.pp   the DoLeave and DoEnter were connected to the wrong
                     event.

        The rest were modified to use the new SendMessage function.   MAH

  Revision 1.5  1999/07/29 14:22:12  peter
    * fixed buttons

  Revision 1.4  1999/07/29 02:50:51  lazarus

       FOnClicked notifications was moved to TControl so it must be removed
       from here. This goes for the OnClicked property as well.    MAH

  Revision 1.3  1999/07/29 02:05:31  lazarus

       Added the Assert routine for doing Tracing of the code

  Revision 1.2  1999/07/27 19:37:09  lazarus
  Added the mousepress event.  Seems to be working.  The Mouse release button event isn't doing anything though.

  Revision 1.1  1999/07/23 05:56:01  lazarus
  New Implementation

  Revision 1.4  1999/07/22 20:50:41  lazarus
  Canvas changes

  Revision 1.3  1999/07/19 01:39:41  lazarus
  Resize Changes CEB

  Revision 1.2  1999/07/19 00:56:08  lazarus

       Add SetBounds to TControl - this makes the resize only get fired once
       as opposed to each time a position is changed.

       Modified TCustomDialog and TOpenFileDialog to be non gtk specific.

       Added SendMessage plus some other changes to TControl.

       Added several new messages to the LMessages.pp file.      MAH

  Revision 1.1  1999/07/12 14:30:48  peter
    * reinserted to fix file permissions

  Revision 1.23  1999/07/11 21:21:40  lazarus

       inserted the multiple messages in the class again - MAH

  Revision 1.22  1999/07/11 19:34:04  lazarus

       modifed AddControl so that any descendent class of TControl only needs
       to call AddControl; The routine itself gets information as to the owner
       the control is to be added to.    MAH

  Revision 1.21  1999/07/11 01:56:52  lazarus

       cleaned up formatting and removed any references
       to InterfaceObject.    MAH

  Revision 1.20  1999/07/09 13:54:34  lazarus
  Changed to use Dispatch instead of DispatchStr for messaging.
  You pass it LM_Message which is an integer value and therefore you
  can now use Dispatch to send the integer value back to the class.
  There is currently a problem with having multiple "message" procedures
  in one class so I commented them out for now.

  Shane

  Revision 1.19  1999/07/04 00:10:52  lazarus
  Code Cleanup

  Revision 1.18  1999/07/03 23:26:20  lazarus
  test update

  Revision 1.17  1999/06/27 21:34:32  lazarus
  Minor messaging changes.
  Changed from TMyNotifyEvent to TNotifyEvent procedures

  Revision 1.16  1999/05/24 21:20:10  lazarus
  *** empty log message ***

  Revision 1.15  1999/05/14 18:44:02  lazarus
  *** empty log message ***

  Revision 1.14  1999/05/14 14:52:58  michael
  + Removed objpas from uses clause

  Revision 1.13  1999/05/07 05:46:07  lazarus
  *** empty log message ***


  Revision 1.12  1999/05/07 05:38:55  lazarus
  *** empty log message ***


  Revision 1.8  1999/05/01 04:36:35  lazarus
  *** empty log message ***

  Revision 1.7  1999/04/23 19:40:21  lazarus
  Removed TLabelButton and am using simply TButton now.  Can set TButton.Caption at anytime and it goes out and changes the button caption (code in TControl.Settext does that).

  PROBLEM:  TButton (and possibly everything else) doesn't resize once it's created.  When you set the top,left,height,width the top and left are adjusted, but the height and width are not.  This
is a major problem and must be figured out.

  04/22/1999 2:22PM
  Shane Miller
  lazarus@miraclec.com

  Revision 1.6  1999/04/20 23:38:30  lazarus
  *** empty log message ***

  Revision 1.4  1999/04/20 05:42:07  lazarus
  *** empty log message ***

  Revision 1.3  1999/04/19 05:42:07  lazarus
  *** empty log message ***

  Revision 1.2  1999/04/18 05:42:07  lazarus
  *** empty log message ***

  Revision 1.1  1999/04/14 07:31:44  michael
  + Initial implementation

}


