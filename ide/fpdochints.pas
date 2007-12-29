{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    Hint using the fpdoc data.
}
unit FPDocHints;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics,
  CodeToolManager, CodeCache, BasicCodeTools, IdentCompletionTool,
  SrcEditorIntf,
  SrcEditHintFrm, CodeHelp;

type

  { TFPDocHintProvider }

  TFPDocHintProvider = class(TCodeHintProvider)
  private
    FHintValid: boolean;
    FWaitingForIdle: boolean;
    procedure SetHintValid(const AValue: boolean);
    procedure SetWaitingForIdle(const AValue: boolean);
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure ReadLazDocData;
  public
    destructor Destroy; override;
    procedure Paint(Canvas: TCanvas; const ARect: TRect); override;
    procedure UpdateHint; override;
    property WaitingForIdle: boolean read FWaitingForIdle write SetWaitingForIdle;
    property HintValid: boolean read FHintValid write SetHintValid;
  end;

implementation

{ TFPDocHintProvider }

procedure TFPDocHintProvider.SetWaitingForIdle(const AValue: boolean);
begin
  if FWaitingForIdle=AValue then exit;
  FWaitingForIdle:=AValue;
  if Application<>nil then begin
    if FWaitingForIdle then
      Application.AddOnIdleHandler(@ApplicationIdle)
    else
      Application.RemoveOnIdleHandler(@ApplicationIdle);
  end;
end;

procedure TFPDocHintProvider.SetHintValid(const AValue: boolean);
begin
  if FHintValid=AValue then exit;
  FHintValid:=AValue;
end;

procedure TFPDocHintProvider.ApplicationIdle(Sender: TObject; var Done: Boolean
  );
begin
  WaitingForIdle:=false;
  ReadLazDocData;
end;

procedure TFPDocHintProvider.ReadLazDocData;
var
  Position: LongInt;
  Item: TIdentifierListItem;
  Code: TCodeBuffer;
  CacheWasUsed: boolean;
  Chain: TLazDocElementChain;
  Y,X: integer;
begin
  if (SourceEditorWindow=nil) or (CodeToolBoss=nil)
  or (CodeToolBoss.IdentifierList=nil) then
    exit;
  Position:=SourceEditorWindow.CompletionBoxPosition;
  if (Position<0) or (Position>=CodeToolBoss.IdentifierList.GetFilteredCount) then
    exit;
  Item:=CodeToolBoss.IdentifierList.FilteredItems[Position];
  DebugLn(['TFPDocHintProvider.ReadLazDocData Identifier=',Item.Identifier]);
  Chain:=nil;
  try
    if (Item.Node<>nil) then begin
      if (Item.Tool.Scanner=nil) then exit;
      Code:=TCodeBuffer(Item.Tool.Scanner.MainCode);
      if Code=nil then begin
        DebugLn(['TFPDocHintProvider.ReadLazDocData FAILED Tool without MainCode']);
        exit;
      end;
      Code.AbsoluteToLineCol(Item.Node.StartPos,Y,X);
      if (Y<1) or (X<1) then begin
        DebugLn(['TFPDocHintProvider.ReadLazDocData FAILED X=',X,' Y=',Y]);
        exit;
      end;
      LazDocBoss.GetElementChain(Code,X,Y,true,Chain,CacheWasUsed);
      DebugLn(['TFPDocHintProvider.ReadLazDocData Chain=',Chain<>nil]);
      if Chain=nil then begin
        DebugLn(['TFPDocHintProvider.ReadLazDocData FAILED Chain=nil']);
        exit;
      end;
    end else begin

    end;
  finally
    Chain.Free;
  end;
end;

destructor TFPDocHintProvider.Destroy;
begin
  WaitingForIdle:=false;
  inherited Destroy;
end;

procedure TFPDocHintProvider.Paint(Canvas: TCanvas; const ARect: TRect);
begin

end;

procedure TFPDocHintProvider.UpdateHint;
begin
  WaitingForIdle:=true;
  inherited UpdateHint;
end;

end.

