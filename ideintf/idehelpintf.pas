{  $Id: helpintf.pas 9271 2006-05-13 12:00:43Z mattias $  }
{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    This unit defines various base classes for the Help System used by the IDE.
}
unit IDEHelpIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, HelpIntfs, LazHelpIntf,
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
    function MessageMatches(const TheMessage: string; MessageParts: TStrings
                            ): boolean; override;
    property Expression: string read FExpression write FExpression;
    property ModifierStr: string read FModifierStr write FModifierStr;
  end;

  { TBaseHelpManager }

  TBaseHelpManager = class(TComponent)
  public
    procedure ConnectMainBarEvents; virtual; abstract;
    procedure LoadHelpOptions; virtual; abstract;
    procedure SaveHelpOptions; virtual; abstract;

    function ShowHelpForSourcePosition(const Filename: string;
                                       const CodePos: TPoint;
                                       var ErrMsg: string): TShowHelpResult; virtual; abstract;
    procedure ShowHelpForMessage(Line: integer); virtual; abstract;
    procedure ShowHelpForObjectInspector(Sender: TObject); virtual; abstract;
    function GetHintForSourcePosition(const ExpandedFilename: string;
                      const CodePos: TPoint;
                      out BaseURL, HTMLHint: string): TShowHelpResult; virtual; abstract;
    function CreateHint(aHintWindow: THintWindow; ScreenPos: TPoint;
                    const BaseURL: string; var TheHint: string;
                    out HintWinRect: TRect): boolean; virtual; abstract;

    function ConvertSourcePosToPascalHelpContext(const CaretPos: TPoint;
                            const Filename: string): TPascalHelpContextList; virtual; abstract;
  end;
  

var
  LazarusHelp: TBaseHelpManager; // initialized by the IDE

type
  { TIDEHTMLControlIntf }

  TIDEHTMLControlIntf = interface
    function GetURL: string;
    procedure SetURL(const AValue: string);
    property URL: string read GetURL write SetURL;
    procedure SetHTMLContent(Stream: TStream);
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
    procedure SetControlIntf(const AValue: TIDEHTMLControlIntf); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetStream(const URL: string
      ): TStream; virtual; abstract; { provider assumes ownership of returned TStream
                                       and increases internal reference count.
                                       If not found it raises an exception. }
    procedure ReleaseStream(const URL: string); virtual; abstract;
    property BaseURL: string read FBaseURL write SetBaseURL;// fallback for relative URLs
    function BuildURL(const CurBaseURL, CurURL: string): string; virtual;
    property ControlIntf: TIDEHTMLControlIntf read FControlIntf write SetControlIntf;
  end;

  TCreateIDEHTMLControlEvent =
    function(Owner: TComponent; var Provider: TAbstractIDEHTMLProvider): TControl;
  TCreateIDEHTMLProviderEvent =
    function(Owner: TComponent): TAbstractIDEHTMLProvider;

var
  CreateIDEHTMLControl: TCreateIDEHTMLControlEvent = nil;// will be set by the IDE
    // and overidden by a package like turbopoweriprodsgn.lpk
  CreateIDEHTMLProvider: TCreateIDEHTMLProviderEvent = nil;// will be set by the IDE


implementation


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

procedure TAbstractIDEHTMLProvider.SetControlIntf(
  const AValue: TIDEHTMLControlIntf);
begin
  if FControlIntf=AValue then exit;
  FControlIntf:=AValue;
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

function TAbstractIDEHTMLProvider.BuildURL(const CurBaseURL, CurURL: string
  ): string;
var
  URLType: string;
  URLPath: string;
  URLParams: string;
begin
  Result:=CurURL;
  SplitURL(CurURL,URLType,URLPath,URLParams);
  //DebugLn(['TAbstractIDEHTMLProvider.BuildURL CurURL=',CurURL,' URLType=',URLType,' URLPath=',URLPath,' URLParams=',URLParams]);
  if URLType='' then begin
    // no URLType => use CurURL as URLPath
    Result:=CurURL;
    //DebugLn(['TAbstractIDEHTMLProvider.BuildURL AAA1 ',Result]);
    if not URLFilenameIsAbsolute(Result) then
      Result:=CurBaseURL+Result;
  end else begin
    Result:=CurURL;
  end;
end;

end.

