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
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, StdCtrls,
  CodeToolManager, CodeCache, BasicCodeTools, IdentCompletionTool, CodeTree,
  CodeAtom,
  IDEHelpIntf, SrcEditorIntf, SrcEditHintFrm, CodeHelp;

type

  { TFPDocHintProvider }

  TFPDocHintProvider = class(TCodeHintProvider)
  private
    FHintValid: boolean;
    FWaitingForIdle: boolean;
    FBaseURL: string;
    FHTMLHint: string;
    FHTMLControl: TControl;
    FHTMLProvider: TAbstractIDEHTMLProvider;
    FTextControl: TLabel;
    procedure SetHintValid(const AValue: boolean);
    procedure SetWaitingForIdle(const AValue: boolean);
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure ReadLazDocData;
    procedure UpdateHintControl;
  public
    destructor Destroy; override;
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
  CacheWasUsed: boolean;
  Node: TCodeTreeNode;
  HelpResult: TCodeHelpParseResult;
  Caret: TCodeXYPosition;
  CleanPos: LongInt;
begin
  FBaseURL:='';
  FHTMLHint:='';
  
  // find current completion item
  if (SourceEditorManagerIntf=nil) or (CodeToolBoss=nil)
  or (CodeToolBoss.IdentifierList=nil) then
    exit;
  Position:=SourceEditorManagerIntf.CompletionBoxPosition;
  if (Position<0) or (Position>=CodeToolBoss.IdentifierList.GetFilteredCount) then
    exit;
  Item:=CodeToolBoss.IdentifierList.FilteredItems[Position];
  DebugLn(['TFPDocHintProvider.ReadLazDocData Identifier=',Item.Identifier]);
  try
    // find current codetool node
    Node:=Item.Node;
    if (Node=nil) then begin
      DebugLn(['TFPDocHintProvider.ReadLazDocData FAILED no node']);
      exit;
    end;
    if (Item.Tool.Scanner=nil) then exit;
    //DebugLn(['TFPDocHintProvider.ReadLazDocData Src=',copy(Item.Tool.Src,Node.StartPos,30),' ',Node.DescAsString]);
    
    // search the position of the identifier, not the keyword
    CleanPos:=Node.StartPos;
    case Node.Desc of
    ctnProcedure:
      begin
        Item.Tool.MoveCursorToProcName(Node,true);
        CleanPos:=Item.Tool.CurPos.StartPos;
      end;
    ctnProperty:
      begin
        if Item.Tool.MoveCursorToPropName(Node) then
          CleanPos:=Item.Tool.CurPos.StartPos;
      end;
    end;
    
    // get help text
    if (not Item.Tool.CleanPosToCaret(CleanPos,Caret)) then begin
      DebugLn(['TFPDocHintProvider.ReadLazDocData FAILED CleanPosToCaret']);
      exit;
    end;
    //DebugLn(['TFPDocHintProvider.ReadLazDocData ',Item.Identifier,' ',Item.Tool.MainFilename,' ',Caret.Code.Filename,' ',Caret.X,',',Caret.Y]);
    HelpResult:=CodeHelpBoss.GetHTMLHint(Caret.Code,Caret.X,Caret.Y,
                                    [chhoSmartHint, chhoComplete, chhoComments],
                                    FBaseURL,FHTMLHint,CacheWasUsed);
    if HelpResult<>chprSuccess then begin
      DebugLn(['TFPDocHintProvider.ReadLazDocData FAILED Identifier=',Item.Identifier]);
      exit;
    end;
  finally
    UpdateHintControl;
  end;
end;

procedure TFPDocHintProvider.UpdateHintControl;
var
  IsHTML: Boolean;
  ms: TMemoryStream;
begin
  IsHTML:=SysUtils.CompareText(copy(FHTMLHint,1,6),'<HTML>')=0;
  if IsHTML then begin
    if (FHTMLControl=nil) then begin
      FHTMLProvider:=nil;
      FHTMLControl:=CreateIDEHTMLControl(nil,FHTMLProvider);
      FHTMLControl.Parent:=Control;
      FHTMLControl.Align:=alClient;
    end;
    if FTextControl<>nil then
      FTextControl.Visible:=false;
    FHTMLControl.Visible:=true;
    FHTMLProvider.BaseURL:=FBaseURL;
    ms:=TMemoryStream.Create;
    try
      if FHTMLHint<>'' then
        ms.Write(FHTMLHint[1],length(FHTMLHint));
      ms.Position:=0;
      FHTMLProvider.ControlIntf.SetHTMLContent(ms);
    finally
      ms.Free;
    end;
  end else begin
    if (FTextControl=nil) then begin
      FTextControl:=TLabel.Create(nil);
      FTextControl.Parent:=Control;
      FTextControl.Align:=alClient;
      FTextControl.WordWrap:=true;
    end;
    if FHTMLControl<>nil then
      FHTMLControl.Visible:=false;
    FTextControl.Visible:=true;
    FTextControl.Caption:=FHTMLHint;
  end;
end;

destructor TFPDocHintProvider.Destroy;
begin
  // important: free provider before control
  FreeAndNil(FHTMLProvider);
  FreeAndNil(FHTMLControl);
  FreeAndNil(FTextControl);
  WaitingForIdle:=false;
  inherited Destroy;
end;

procedure TFPDocHintProvider.UpdateHint;
begin
  WaitingForIdle:=true;
  inherited UpdateHint;
end;

end.

