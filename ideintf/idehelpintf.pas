{  $Id: helpintf.pas 9271 2006-05-13 12:00:43Z mattias $  }
{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  Classes, SysUtils, HelpIntfs, LazHelpIntf, TextTools;

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

    function ConvertSourcePosToPascalHelpContext(const CaretPos: TPoint;
                            const Filename: string): TPascalHelpContextList; virtual; abstract;
  end;

var
  LazarusHelp: TBaseHelpManager; // initialized by the IDE

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

end.

