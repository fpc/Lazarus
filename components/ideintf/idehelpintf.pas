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
  Classes, SysUtils,
  // LCL
  LMessages, LCLType, LCLIntf, Forms, Controls, Graphics, HelpIntfs, LazHelpIntf,
  // IdeIntf
  TextTools;

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

  { TBaseHelpManager }

  TBaseHelpManager = class(TComponent)
  private
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

  THintWindowManager = class
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
    function ShowHint(ScreenPos: TPoint; TheHint: string; const MouseOffset: Boolean = True;
      HintFont: TFont = nil): boolean;
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

  FPCKeyWordHelpPrefix: string = 'FPCKeyword_';
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

function THintWindowManager.ShowHint(ScreenPos: TPoint; TheHint: string;
  const MouseOffset: Boolean; HintFont: TFont): boolean;

  procedure DoText;
  var
    HintWinRect: TRect;
  begin
    if HintFont<>nil then
      HintTextWindow.Font := HintFont;
    HintWinRect := HintTextWindow.CalcHintRect(Screen.Width, TheHint, Nil);
    HintTextWindow.HintRect := HintWinRect;      // Adds borders.
    if MouseOffset then
      HintTextWindow.OffsetHintRect(ScreenPos)
    else                   // shrink height only for fixed (no MouseOffset) hints
      HintTextWindow.OffsetHintRect(ScreenPos, 0, True, False);
    HintTextWindow.ActivateHint(TheHint);
  end;

  procedure DoHtml;
  var
    ms: TMemoryStream;
    NewWidth, NewHeight: integer;
    R1, R2: TRect;
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

    HintRenderWindow.HintRectAdjust := Rect(0, 0, NewWidth, NewHeight);
    if MouseOffset then
      HintRenderWindow.OffsetHintRect(ScreenPos)
    else
    begin
      R1 := HintRenderWindow.HintRect;
      HintRenderWindow.OffsetHintRect(ScreenPos, 0, True, False); // shrink height only for fixed (no MouseOffset) hints
      R2 := HintRenderWindow.HintRect;
      if R1.Bottom-R1.Top>R2.Bottom-R2.Top then // the height was decreased -> scrollbar will be shown, increase width
      begin
        Inc(R2.Right, GetSystemMetrics(SM_CXVSCROLL));
        HintRenderWindow.HintRect := R2;
        HintRenderWindow.OffsetHintRect(Point(0, 0), 0);
      end;
    end;
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

