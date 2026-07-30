{  $Id: helpintf.pas 9271 2006-05-13 12:00:43Z mattias $  }
{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    This unit defines various base classes for the Help System used by the IDE.
}
unit IDEHelpIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Math,
  // LCL
  LMessages, LCLType, LCLIntf, Forms, Controls, Graphics, HelpIntfs, LazHelpIntf,
  TextTools, LazLogger;

type
  { THelpDBIRegExprMessage
    Help registration item for matching a message (e.g. a fpc warning) with
    a regular expression.
    For example a line like
      "/usr/share/lazarus/components/synedit/syneditkeycmds.pp(532,10) Warning: Function result does not seem to be set"
     could be matched with
      Expression=') Warning: Function result does not seem to be set'
    }

  THelpDBIRegExprMessage = class(THelpDBIMessage)
  private
    FExpression: string;
    FModifierStr: string;
  public
    constructor Create(TheNode: THelpNode; const RegularExpression,
                       TheModifierStr: string);
    function MessageMatches(const TheMessage: string; {%H-}MessageParts: TStrings
                            ): boolean; override;
    property Expression: string read FExpression write FExpression;
    property ModifierStr: string read FModifierStr write FModifierStr;
  end;

  TIDEHelpManagerCreateHintFlag = (
    ihmchAddFocusHint
    );
  TIDEHelpManagerCreateHintFlags = set of TIDEHelpManagerCreateHintFlag;

  TFPDocEditorPart = (
    fpdepShortDesc,
    fpdepDescription,
    fpdepErrors,
    fpdepTopicShort,
    fpdepTopicDesc
    );
  TFPDocEditorTxtBtnParams = record
    Part: TFPDocEditorPart;
    CodeBuf: Pointer; // TCodeBuffer
    CodeTool: Pointer; // TCodeTool
    CodeNode: Pointer; // TCodeTreeNode
    Filename: string;
    Line, Col: integer;
    Selection: string; // on Success is changed to new value
    Success: boolean;
  end;

  TFPDocEditorTxtBtnClick = procedure(var Params: TFPDocEditorTxtBtnParams) of object;

  { TBaseHelpManager }

  TBaseHelpManager = class(TComponent)
  private
    type
      TFPDocEditorTextBtnHandler = record
        Caption, Hint: string;
        OnExecute: TFPDocEditorTxtBtnClick;
      end;
      TFPDocEditorTextBtnHandlers = array of TFPDocEditorTextBtnHandler;
  protected
    FFPDocEditorTextBtnHandlers: TFPDocEditorTextBtnHandlers;
    FCombineSameIdentifiersInUnit: boolean;
    FShowCodeBrowserOnUnknownIdentifier: boolean;
  public
    procedure ConnectMainBarEvents; virtual; abstract;
    procedure LoadHelpOptions; virtual; abstract;
    procedure SaveHelpOptions; virtual; abstract;

    function ShowHelpForSourcePosition(const Filename: string;
                                       const CodePos: TPoint;
                                       var ErrMsg: string): TShowHelpResult; virtual; abstract;
    procedure ShowHelpForMessage; virtual; abstract;
    procedure ShowHelpForObjectInspector(Sender: TObject); virtual; abstract;
    procedure ShowHelpForIDEControl(Sender: TControl); virtual; abstract;
    procedure HelpButtonClick(Sender: TObject);
    function GetHintForSourcePosition(const ExpandedFilename: string;
      const CodePos: TPoint; out BaseURL, HTMLHint: string;
      Flags: TIDEHelpManagerCreateHintFlags = []): TShowHelpResult; virtual; abstract;
    function ConvertSourcePosToPascalHelpContext(const CaretPos: TPoint;
      const Filename: string): TPascalHelpContextList; virtual; abstract;
    // fpdoc
    function GetFPDocFilenameForSource(SrcFilename: string;
      ResolveIncludeFiles: Boolean;
      out AnOwner: TObject// a package or a project or LazarusHelp or nil for user defined
      ): string; virtual; abstract;
    procedure RegisterFPDocEditorTextButton(const aCaption, aHint: string; const OnExecute: TFPDocEditorTxtBtnClick); virtual;
    property FPDocEditorTextBtnHandlers: TFPDocEditorTextBtnHandlers read FFPDocEditorTextBtnHandlers;

    property CombineSameIdentifiersInUnit: boolean
      read FCombineSameIdentifiersInUnit write FCombineSameIdentifiersInUnit;
    property ShowCodeBrowserOnUnknownIdentifier: boolean
      read FShowCodeBrowserOnUnknownIdentifier write FShowCodeBrowserOnUnknownIdentifier;
  end;
  

var
  LazarusHelp: TBaseHelpManager; // initialized by the IDE
  FPCMessagesHelpDB: THelpDatabase; // initialized by the IDE

type
  { TIDEHTMLControlIntf }

  TIDEHTMLControlIntf = interface
    function GetURL: string;
    procedure SetURL(const AValue: string);
    property URL: string read GetURL write SetURL;
    procedure SetHTMLContent(Stream: TStream; const NewURL: string = '');
    procedure GetPreferredControlSize(out AWidth, AHeight: integer);
  end;

  { TAbstractIDEHTMLProvider
    An instance of this class connects 3 parts:
     1. IDE html files  (via implementation)
     2. a html viewer control (via ControlIntf)
     3. IDE or designtime package code
    All three can communicate. }

  TAbstractIDEHTMLProvider = class(TComponent)
  protected
    FBaseURL: string;
    FControlIntf: TIDEHTMLControlIntf;
    procedure SetBaseURL(const AValue: string); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function URLHasStream(const URL: string): boolean; virtual; abstract;
      { Open a URL asynchronously
        The standard IDE implementation supports the following for OpenURLAsync:
        source://local-file-name   : open a file local-file-name in the source editor
        openpackage://package-name : open a package editor
        fpdoc://#package-name.unitname.element : this opens the help for the fpdoc entry
        }
    procedure OpenURLAsync(const URL: string); virtual; abstract;
    function GetStream(const URL: string; Shared: boolean
      ): TStream; virtual; abstract; { Shared=true: provider assumes ownership
                  of returned TStream and increases internal reference count.
                  If not found it raises an exception.
                  Shared=false: caller must free stream}
    procedure ReleaseStream(const URL: string); virtual; abstract;
    property BaseURL: string read FBaseURL write SetBaseURL;// fallback for relative URLs
    function MakeURLAbsolute(const aBaseURL, aURL: string): string; virtual;
    property ControlIntf: TIDEHTMLControlIntf read FControlIntf write FControlIntf;
  end;

  TIDEHTMLControlFlag = (
    ihcScrollable,
    ihcWithClipboardMenu
  );
  TIDEHTMLControlFlags = set of TIDEHTMLControlFlag;

  TCreateIDEHTMLControlEvent =
    function(Owner: TComponent; var Provider: TAbstractIDEHTMLProvider;
             Flags: TIDEHTMLControlFlags = []): TControl;
  TCreateIDEHTMLProviderEvent =
    function(Owner: TComponent): TAbstractIDEHTMLProvider;


  { TSolidHintWindowRendered }

  TSolidHintWindowRendered = class(THintWindowRendered)
  protected
    procedure WMNCHitTest(var Message: TLMessage); message LM_NCHITTEST;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { THintWindowManager }

  THintWindowShowPos = (
    hwpAfter,   // Below/Right
    hwpCenter,  //
    hwpBefore   // Above/Left
  );
  THintWindowShowFlag = (
    hwfSwapXYPreference,  // Swap order in which Y/X are computed
                          // - E.g.: Prefer placing to the Left/Right of the exclusion (keeping Y at ScreenPos)
                          //         Rather than Above/Below the Exclusion (keeping X at ScreenPos)
                          // - Ignored, if hwpCenter
                          // - If there is no exclusion, then the Offset will be applied according to this
    hwfAllowOppositePos,  // Allows above/below (or left/right, if XY-Pref) to be swapped if there is not enough space
    hwfAllowSwapXYPref,   // Only, if there is not enough space on the original preference
    hwfPreventSliding     // Don't push to left/right (up/down), if there is not enough space
  );
  THintWindowShowFlags = set of THintWindowShowFlag;

  THintWindowManager = class
  //private const
  //  HINTWIN_DEF_FLAGS = THintWindowShowFlags([]);
  private
    // 2 HintWindows, one for simple text and one for rendered hint with child control.
    // Only one is visible at a time.
    FHintTextW: THintWindow;
    FHintRenderW: THintWindowRendered;
    FCurrentHintW: THintWindow;  // One of the windows or Nil.
    // Provider for the rendered hint.
    FHtmlHelpProvider: TAbstractIDEHTMLProvider;
    FBaseURL: string;
    FFlags: TIDEHTMLControlFlags;
    FOrigMousePos: TPoint;
    // These will be passed to HintWindow.
    FAutoHide: Boolean;
    FHideInterval: Integer;
    FOnMouseDown: TMouseEvent;
    FWindowName: string;
    function HtmlHelpProvider: TAbstractIDEHTMLProvider;
    function HintTextWindow: THintWindow;
    function HintRenderWindow: THintWindowRendered;
    procedure SetAutoHide(AValue: Boolean);
    procedure SetHideInterval(AValue: Integer);
    procedure SetOnMouseDown(AValue: TMouseEvent);
    procedure SetWindowName(AValue: string);
  protected
  public
    constructor Create; overload;
    destructor Destroy; override;
    function HintIsVisible: boolean;
    function ShowHint(ScreenPos: TPoint; TheHint: string;
      MouseOffset: Boolean = True; // If false don't allow *ANY* offset to mouse (ScreenPos).
                                         // No distance: Don't add internal offset.
                                         // No overlap:  Don't allow moving oversized hint, cut off instead.
                                         //   Hint may slide sidewards below the point (or instead upwards, behind it)
      HintFont: TFont = nil;
      AHintYPos: THintWindowShowPos = hwpAfter;
      AHintXPos: THintWindowShowPos = hwpAfter;
      AHintFlags: THintWindowShowFlags = [];
      AnExclusionRect: PRect = nil
      ): boolean;
    procedure HideHint;
    procedure HideIfVisible;
  public
    property CurHintWindow: THintWindow read FCurrentHintW;
    property BaseURL: string read FBaseURL write FBaseURL;
    property Flags: TIDEHTMLControlFlags read FFlags write FFlags;
    property AutoHide: Boolean read FAutoHide write SetAutoHide;
    property HideInterval: Integer read FHideInterval write SetHideInterval;
    property OnMouseDown: TMouseEvent read FOnMouseDown write SetOnMouseDown;
    property WindowName: string read FWindowName write SetWindowName;
  end;

var
  CreateIDEHTMLControl: TCreateIDEHTMLControlEvent = nil;// will be set by the IDE
    // and can be overidden by a package like turbopoweriprodsgn.lpk
  CreateIDEHTMLProvider: TCreateIDEHTMLProviderEvent = nil;// will be set by the IDE

  FPCKeyWordHelpPrefix: string = 'FPCKeyword_'; // built-in identifiers, e.g. if and delete
  FPCDirectiveHelpPrefix: string = 'FPCDirective_';
  IDEDirectiveHelpPrefix: string = 'IDEDirective_';

implementation

{ TSolidHintWindowRendered }

procedure TSolidHintWindowRendered.WMNCHitTest(var Message: TLMessage);
begin
  Message.Result := HTCLIENT;
end;

procedure TSolidHintWindowRendered.KeyDown(var Key: Word; Shift: TShiftState);
Var
  AOldKey : Word;
begin
  AOldKey := Key;
  inherited KeyDown(Key, Shift);
  if AOldKey=VK_ESCAPE then
    Hide;
end;

constructor TSolidHintWindowRendered.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  KeyPreview := True;
end;

{ THelpDBIRegExprMessage }

constructor THelpDBIRegExprMessage.Create(TheNode: THelpNode;
  const RegularExpression, TheModifierStr: string);
begin
  Node:=TheNode;
  FExpression:=RegularExpression;
  FModifierStr:=TheModifierStr;
end;

function THelpDBIRegExprMessage.MessageMatches(const TheMessage: string;
  MessageParts: TStrings): boolean;
begin
  Result:=REMatches(TheMessage,Expression,ModifierStr);
  //writeln('THelpDBIRegExprMessage.MessageMatches TheMessage="',TheMessage,'" Expression="',Expression,'" Result=',Result);
end;

{ TBaseHelpManager }

procedure TBaseHelpManager.HelpButtonClick(Sender: TObject);
begin
  if Sender is TControl then
    ShowHelpForIDEControl(TControl(Sender));
end;

procedure TBaseHelpManager.RegisterFPDocEditorTextButton(const aCaption, aHint: string;
  const OnExecute: TFPDocEditorTxtBtnClick);
var
  Item: TFPDocEditorTextBtnHandler;
begin
  Item.Caption:=aCaption;
  Item.Hint:=aHint;
  Item.OnExecute:=OnExecute;
  Insert(Item,FFPDocEditorTextBtnHandlers,length(FFPDocEditorTextBtnHandlers));
end;

{ TAbstractIDEHTMLProvider }

procedure TAbstractIDEHTMLProvider.SetBaseURL(const AValue: string);
begin
  if FBaseURL=AValue then exit;
  FBaseURL:=AValue;
end;

constructor TAbstractIDEHTMLProvider.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TAbstractIDEHTMLProvider.Destroy;
begin
  FControlIntf:=nil; // decrease reference count
  inherited Destroy;
end;

function TAbstractIDEHTMLProvider.MakeURLAbsolute(const aBaseURL, aURL: string): string;
var
  URLType: string;
  URLPath: string;
  URLParams: string;
begin
  Result:=aURL;
  SplitURL(aURL,URLType,URLPath,URLParams);
  //DebugLn(['TAbstractIDEHTMLProvider.BuildURL URL=',aURL,' URLType=',URLType,' URLPath=',URLPath,' URLParams=',URLParams]);
  if URLType='' then begin
    // no URLType => use aURL as URLPath
    Result:=aURL;
    if not URLFilenameIsAbsolute(Result) then begin
      if aBaseURL<>'' then
        Result:=aBaseURL+Result
      else
        Result:=BaseURL+Result;
    end;
  end else begin
    Result:=aURL;
  end;
end;

{ THintWindowManager }

constructor THintWindowManager.Create;
begin
  inherited Create;
  FFlags := [ihcWithClipboardMenu];
  FHideInterval := 3000;
end;

destructor THintWindowManager.Destroy;
begin
  FreeAndNil(FHintRenderW);
  FreeAndNil(FHintTextW);
  inherited Destroy;
end;

function THintWindowManager.HintTextWindow: THintWindow;
begin
  if FHintTextW = nil then
  begin
    FHintTextW := THintWindow.Create(Nil);
    FHintTextW.AutoHide := FAutoHide;
    FHintTextW.HideInterval := FHideInterval;
    FHintTextW.OnMouseDown := FOnMouseDown;
    if FWindowName <> '' then
      FHintTextW.Name := FWindowName;
  end;
  FCurrentHintW := FHintTextW;
  Result := FHintTextW;
end;

function THintWindowManager.HintRenderWindow: THintWindowRendered;
begin
  if FHintRenderW = nil then
  begin
    FHintRenderW := TSolidHintWindowRendered.Create(Nil);
    FHintRenderW.AutoHide := FAutoHide;
    FHintRenderW.HideInterval := FHideInterval;
    FHintRenderW.OnMouseDown := FOnMouseDown;
    if FWindowName <> '' then
      FHintRenderW.Name := FWindowName;
  end;
  FCurrentHintW := FHintRenderW;
  Result := FHintRenderW;
end;

function THintWindowManager.HintIsVisible: boolean;
begin
  Result := Assigned(FCurrentHintW) and FCurrentHintW.Visible;
end;

function THintWindowManager.HtmlHelpProvider: TAbstractIDEHTMLProvider;
var
  HelpControl: TControl;
begin
  if FHtmlHelpProvider = nil then
  begin
    //Include(FFlags, ihcScrollable);  // Debug (memo hint control does not work)
    HelpControl := CreateIDEHTMLControl(HintRenderWindow, FHtmlHelpProvider, FFlags);
    HelpControl.Parent := HintRenderWindow;
    HelpControl.Align := alClient;
  end;
  Result := FHtmlHelpProvider;
end;

function THintWindowManager.ShowHint(ScreenPos: TPoint; TheHint: string; MouseOffset: Boolean;
  HintFont: TFont; AHintYPos: THintWindowShowPos; AHintXPos: THintWindowShowPos;
  AHintFlags: THintWindowShowFlags; AnExclusionRect: PRect): boolean;

  procedure AdjustHintRect(AHintWindow: THintWindow; AScrollBarHeight: integer = 0; AScrollBarWidth: integer = 0);
  var
    OrigRect, ExclusionRect, BestRect, CurRect, MonitorBounds: TRect;
    dy, x, y, CurLoss, OW, OH: Integer;
    HintMonitor: TMonitor;
    OA: Int64;

    procedure RectToMonitor(var TmpRect: TRect; KeepWidth, KeepHeight: Boolean);
    begin
      if HintMonitor = nil then
        exit;
      // offset hint to fit into monitor
      if TmpRect.Bottom > MonitorBounds.Bottom then
      begin
        if KeepHeight then
          TmpRect.Top := MonitorBounds.Bottom - (TmpRect.Bottom - TmpRect.Top);
        TmpRect.Bottom := MonitorBounds.Bottom;
      end;
      if TmpRect.Top < MonitorBounds.Top then
      begin
        if KeepHeight then
          TmpRect.Bottom := Min(MonitorBounds.Top + (TmpRect.Bottom - TmpRect.Top), MonitorBounds.Bottom);
        TmpRect.Top := MonitorBounds.Top;
      end;

      if TmpRect.Right > MonitorBounds.Right then
      begin
        if KeepWidth then
          TmpRect.Left := MonitorBounds.Right - (TmpRect.Right - TmpRect.Left);
        TmpRect.Right := MonitorBounds.Right;
      end;
      if TmpRect.Left < MonitorBounds.Left then
      begin
        if KeepWidth then
          TmpRect.Right:= Min(MonitorBounds.Left + (TmpRect.Right - TmpRect.Left), MonitorBounds.Right);
        TmpRect.Left := MonitorBounds.Left;
      end;
    end;

    function OffsetHintRect(TmpRect: TRect; AOffset: TPoint; KeepWidth, KeepHeight: Boolean): TRect;
    begin
      Result := TmpRect;
      OffsetRect(Result, AOffset.X, AOffset.Y );
      RectToMonitor(Result, KeepWidth, KeepHeight);
    end;

    procedure ApplyScrollBarSpace(var TmpRect: TRect; ABefore, AnAbove, KeepWidth, KeepHeight: Boolean);
    var
      ReducedWidth, ReducedHeight: Boolean;
    begin
      ReducedWidth  := (TmpRect.Width  < OrigRect.Width)  and (AScrollBarHeight > 0);
      ReducedHeight := (TmpRect.Height < OrigRect.Height) and (AScrollBarWidth  > 0);
      if ReducedHeight <> ReducedWidth then begin // if both were reduced, they can't be increased
        if ReducedWidth then begin // the width was decreased -> scrollbar will be shown, increase width
          if AnAbove then
            //TmpRect.Top := Max(0, TmpRect.Top - GetSystemMetrics(SM_CYHSCROLL))
            Dec(TmpRect.Top, AScrollBarHeight)
          else
            Inc(TmpRect.Bottom, AScrollBarHeight);
        end;
        if ReducedHeight then begin // the height was decreased -> scrollbar will be shown, increase width
          if ABefore then
            //TmpRect.Left := Max(0, TmpRect.Left - GetSystemMetrics(SM_CXVSCROLL))
            Dec(TmpRect.Left, AScrollBarWidth)
          else
            Inc(TmpRect.Right, AScrollBarWidth);
        end;
        TmpRect := OffsetHintRect(TmpRect, Point(0, 0), KeepWidth, KeepHeight);
      end;
    end;

    function UpdateBestFound(TmpRect: TRect): boolean;
    const
      BASE = int64($4000);
    var
      Loss, L1, L2, L3: Integer;
      TmpClientRect: TRect;
    begin
      TmpClientRect := TmpRect;
      if TmpClientRect.Width  < OrigRect.Width  then TmpClientRect.Bottom := Max(0, TmpClientRect.Bottom - AScrollBarHeight);
      if TmpClientRect.Height < OrigRect.Height then TmpClientRect.Right  := Max(0, TmpClientRect.Right  - AScrollBarWidth);
      // the amount of pixels lost in width/height

      L1 := Int64(Max(0, OW - TmpClientRect.Width))  * BASE div OW;
      L2 := Int64(Max(0, OH - TmpClientRect.Height)) * BASE div OH;
      // The surface loss
      L3 := Int64(Max(0, OA - TmpClientRect.Height*TmpClientRect.Width)) * BASE div OA;
      // If either has full size, then make the loss count less.
      if L1 <= 2 then begin L2 := L2 div 2; L3 := L3 div 2; end;
      if L2 <= 2 then begin L1 := L1 div 2; L3 := L3 div 2; end;
      Loss := Max(Max(L1, L2), L3);
      if Loss < CurLoss then begin
        CurLoss  := Loss;
        BestRect := TmpRect;
      end;

      Result := CurLoss <= 0;
    end;

  begin
    HintMonitor := Screen.MonitorFromPoint(ScreenPos);
    if HintMonitor <> nil then
      MonitorBounds := HintMonitor.WorkareaRect;

    OrigRect := AHintWindow.HintRect;
    OW := Min(100000, OrigRect.Width); // limit for comparison
    OH := Min(100000, OrigRect.Width);
    OA := Int64(OH)*Int64(OW);

    if AnExclusionRect <> nil then begin
      ExclusionRect := AnExclusionRect^;
      MouseOffset := False; // don't allow to slide over exclusion rect // don't use "dy"
    end
    else
      ExclusionRect := Rect(ScreenPos.X, ScreenPos.Y, ScreenPos.X+1, ScreenPos.Y+1);
    if MouseOffset then
      dy := 15
    else
      dy := 0;

    if AHintXPos = hwpCenter then exclude(AHintFlags, hwfSwapXYPreference)
    else
    if AHintYPos = hwpCenter then include(AHintFlags, hwfSwapXYPreference);

    CurLoss := MaxInt;

    repeat
      if hwfSwapXYPreference in AHintFlags then begin
        case AHintYPos of
          hwpAfter:  y := ExclusionRect.Top;
          hwpCenter: y := ExclusionRect.Top + (ExclusionRect.Height - OrigRect.Height) div 2;
          hwpBefore: y := ExclusionRect.Bottom - OrigRect.Height;
        end;

        case AHintXPos of
          hwpAfter:  x := ExclusionRect.Right + dy;
          hwpCenter: x := ExclusionRect.Right + (ExclusionRect.Width - OrigRect.Width) div 2;
          hwpBefore: x := ExclusionRect.Left - OrigRect.Width - dy;
        end;
        CurRect := OffsetHintRect(OrigRect, Point(x, y),
                                  MouseOffset,                            // shrink Width,  if no overlap allowed
                                  not (hwfPreventSliding in AHintFlags)); // shrink Height, instead of sliding
        ApplyScrollBarSpace(CurRect, AHintXPos = hwpBefore, AHintYPos = hwpBefore, MouseOffset, not (hwfPreventSliding in AHintFlags));
        if UpdateBestFound(CurRect) then
          break;

        if (hwfAllowOppositePos in AHintFlags) and (AHintXPos <> hwpCenter) then begin
          case AHintXPos of
            hwpAfter:  x := ExclusionRect.Left - OrigRect.Width - dy;
            hwpBefore: x := ExclusionRect.Right + dy;
          end;
          CurRect := OffsetHintRect(OrigRect, Point(x, y),
                                    MouseOffset,                            // shrink Width,  if no overlap allowed
                                    not (hwfPreventSliding in AHintFlags)); // shrink Height, instead of sliding
          ApplyScrollBarSpace(CurRect, AHintXPos = hwpAfter, AHintYPos = hwpBefore, MouseOffset, not (hwfPreventSliding in AHintFlags));
          if UpdateBestFound(CurRect) then
            break;
        end;

        if (hwfAllowSwapXYPref in AHintFlags) and (AHintYPos <> hwpCenter) then begin
          AHintFlags := AHintFlags - [hwfAllowSwapXYPref] + [hwfSwapXYPreference];
          Continue;
        end;
      end
      else begin
        case AHintXPos of
          hwpAfter:  x := ExclusionRect.Left;
          hwpCenter: x := ExclusionRect.Left + (ExclusionRect.Width - OrigRect.Width) div 2;
          hwpBefore: x := ExclusionRect.Right - OrigRect.Width;
        end;

        case AHintYPos of
          hwpAfter:  y := ExclusionRect.Bottom + dy;
          hwpCenter: y := ExclusionRect.Bottom + (ExclusionRect.Height - OrigRect.Height) div 2;
          hwpBefore: y := ExclusionRect.Top - OrigRect.Height - dy;
        end;
        CurRect := OffsetHintRect(OrigRect, Point(x, y),
                                  not (hwfPreventSliding in AHintFlags), // shrink width, instead of sliding
                                  MouseOffset);                          // shrink height, if no overlap allowed
        ApplyScrollBarSpace(CurRect, AHintXPos = hwpBefore, AHintYPos = hwpBefore, not (hwfPreventSliding in AHintFlags), MouseOffset);
        if UpdateBestFound(CurRect) then
          break;

        if (hwfAllowOppositePos in AHintFlags) and (AHintYPos <> hwpCenter) then begin
          case AHintYPos of
            hwpAfter:  y := ExclusionRect.Top - OrigRect.Height - dy;
            hwpBefore: y := ExclusionRect.Bottom + dy;
          end;
          CurRect := OffsetHintRect(OrigRect, Point(x, y),
                                     not (hwfPreventSliding in AHintFlags), // shrink width, instead of sliding
                                     MouseOffset);                          // shrink height, if no overlap allowed
          ApplyScrollBarSpace(CurRect, AHintXPos = hwpBefore, AHintYPos = hwpAfter, not (hwfPreventSliding in AHintFlags), MouseOffset);
          if UpdateBestFound(CurRect) then
            break;
        end;

        if (hwfAllowSwapXYPref in AHintFlags) and (AHintXPos <> hwpCenter) then begin
          AHintFlags := AHintFlags - [hwfAllowSwapXYPref] + [hwfSwapXYPreference];
          Continue;
        end;
      end;

      break;
    until False;

    AHintWindow.HintRect := BestRect;
  end;

  procedure DoText;
  var
    HintWinRect: TRect;
  begin
    if HintFont<>nil then
      HintTextWindow.Font := HintFont;
    HintWinRect := HintTextWindow.CalcHintRect(Screen.Width, TheHint, Nil);
    HintTextWindow.HintRect := HintWinRect;      // Adds borders.
    AdjustHintRect(HintTextWindow);
    HintTextWindow.ActivateHint(TheHint);
  end;

  procedure DoHtml;
  var
    ms: TMemoryStream;
    NewWidth, NewHeight: integer;
    R1, R2: TRect;
    ReducedHeight, ReducedWidth: Boolean;
  begin
    if HintFont<>nil then
      HintRenderWindow.Font := HintFont;
    HtmlHelpProvider.BaseURL:=FBaseURL;
    ms:=TMemoryStream.Create;
    try                               // TheHint<>'' is checked earlier.
      Assert(TheHint<>'', 'THintWindowManager.ShowHint: TheHint is empty');
      ms.Write(TheHint[1],length(TheHint));
      ms.Position:=0;
      HtmlHelpProvider.ControlIntf.SetHTMLContent(ms,'');
    finally
      ms.Free;
    end;
    HtmlHelpProvider.ControlIntf.GetPreferredControlSize(NewWidth,NewHeight);

    if NewWidth <= 0 then
      NewWidth := 500;
    if NewHeight <= 0 then
      NewHeight := 200;

    {$IFDEF LCLCOCOA}
    dec(ScreenPos.Y);
    {$ENDIF}
    HintRenderWindow.HintRectAdjust := Rect(0, 0, NewWidth, NewHeight);
    R1 := HintRenderWindow.HintRect;
    AdjustHintRect(HintRenderWindow, GetSystemMetrics(SM_CYHSCROLL), GetSystemMetrics(SM_CXVSCROLL));
    //R2 := HintRenderWindow.HintRect;
    //ReducedWidth := R1.Right-R1.Left>R2.Right-R2.Left;
    //ReducedHeight := R1.Bottom-R1.Top>R2.Bottom-R2.Top;
    //if ReducedHeight <> ReducedWidth then begin // if both were reduced, they can't be increased
    //  if ReducedWidth then begin // the width was decreased -> scrollbar will be shown, increase width
    //    if R2.Bottom <= ScreenPos.y then
    //      R2.Top := Max(0, R2.Top - GetSystemMetrics(SM_CYHSCROLL))
    //    else
    //      Inc(R2.Bottom, GetSystemMetrics(SM_CYHSCROLL));
    //  end;
    //  if ReducedHeight then begin // the height was decreased -> scrollbar will be shown, increase width
    //    if R2.Right <= ScreenPos.x then
    //      R2.Left := Max(0, R2.Left - GetSystemMetrics(SM_CXVSCROLL))
    //    else
    //      Inc(R2.Right, GetSystemMetrics(SM_CXVSCROLL));
    //  end;
    //  HintRenderWindow.HintRect := R2;
    //
    //  if MouseOffset then
    //    HintRenderWindow.OffsetHintRect(Point(0, 0), 0)
    //  else
    //    HintRenderWindow.OffsetHintRect(Point(0, 0), 0, True, False); // shrink height only for fixed (no MouseOffset) hints
    //end;
    HintRenderWindow.ActivateRendered;
  end;

begin
  if TheHint = '' then Exit(False);
  FOrigMousePos := Mouse.CursorPos;
  if FHintTextW <> nil then
    FHintTextW.Visible := false;
  if FHintRenderW <> nil then
    FHintRenderW.Visible := false;
  if CompareText(copy(TheHint,1,6),'<HTML>')=0 then // Text is HTML
    DoHtml
  else                                              // Plain text
    DoText;
  Result:=True;
end;

procedure THintWindowManager.HideHint;
begin
  if Assigned(FCurrentHintW) then
    FCurrentHintW.Visible := False;
end;

procedure THintWindowManager.HideIfVisible;
begin
  if HintIsVisible then
    FCurrentHintW.Visible := False;
end;

// Setters

procedure THintWindowManager.SetAutoHide(AValue: Boolean);
begin
  FAutoHide := AValue;
  if Assigned(FHintTextW) then FHintTextW.AutoHide := FAutoHide;
  if Assigned(FHintRenderW) then FHintRenderW.AutoHide := FAutoHide;
end;

procedure THintWindowManager.SetHideInterval(AValue: Integer);
begin
  FHideInterval := AValue;
  if Assigned(FHintTextW) then FHintTextW.HideInterval := FHideInterval;
  if Assigned(FHintRenderW) then FHintRenderW.HideInterval := FHideInterval;
end;

procedure THintWindowManager.SetOnMouseDown(AValue: TMouseEvent);
begin
  FOnMouseDown:=AValue;
  if Assigned(FHintTextW) then FHintTextW.OnMouseDown := FOnMouseDown;
  if Assigned(FHintRenderW) then FHintRenderW.OnMouseDown := FOnMouseDown;
end;

procedure THintWindowManager.SetWindowName(AValue: string);
begin
  FWindowName := AValue;
  if Assigned(FHintTextW) then FHintTextW.Name := FWindowName;
  if Assigned(FHintRenderW) then FHintRenderW.Name := FWindowName;
end;

end.

