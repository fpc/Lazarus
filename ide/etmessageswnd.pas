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
    Window for (compiler) messages.
}
unit etMessagesWnd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IDEMsgIntf, IDEImagesIntf, IDEExternToolIntf,
  LazIDEIntf, Forms, Controls, Graphics, Dialogs, etMessageFrame,
  etSrcEditMarks, etQuickFixes;

type

  { TMessagesView }

  TMessagesView = class(TIDEMessagesWindowInterface)
    MessagesFrame1: TMessagesFrame;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function OnOpenMessage(Sender: TObject; Msg: TMessageLine): boolean;
  private
    ImgIDNone: integer;
    ImgIDInformation: integer;
    ImgIDHint: integer;
    ImgIDNote: integer;
    ImgIDWarning: integer;
    ImgIDError: integer;
    ImgIDFatal: integer;
    function GetDblClickJumps: boolean;
    procedure SetDblClickJumps(AValue: boolean);
  protected
    function GetViews(Index: integer): TExtToolView; override;
  public
    SourceMarks: TETMarks;
    function AddCustomMessage(TheUrgency: TMessageLineUrgency; Msg: string;
      aSrcFilename: string=''; LineNumber: integer=0; Column: integer=0;
      const ViewCaption: string=''): TMessageLine; override;
    procedure Clear; override;
    procedure DeleteView(View: TExtToolView); override;
    function FindUnfinishedView: TExtToolView; override;
    function GetSelectedLine: TMessageLine; override;
    function GetView(aCaption: string; CreateIfNotExist: boolean
      ): TExtToolView; override;
    function IndexOfView(View: TExtToolView): integer; override;
    function SelectFirstUrgentMessage(aMinUrgency: TMessageLineUrgency;
      WithSrcPos: boolean): boolean; override;
    function SelectNextUrgentMessage(aMinUrgency: TMessageLineUrgency;
      WithSrcPos, Downwards: boolean): boolean; override;
    function ViewCount: integer; override;
    property DblClickJumps: boolean read GetDblClickJumps write SetDblClickJumps;
  end;

var
  MessagesView: TMessagesView;

const
  MessagesMenuRootName = 'Messages';

procedure RegisterStandardMessagesViewMenuItems;

implementation

procedure RegisterStandardMessagesViewMenuItems;
begin

end;

{$R *.lfm}

{ TMessagesView }

procedure TMessagesView.FormCreate(Sender: TObject);
begin
  IDEMessagesWindow:=Self;

  SourceMarks:=TETMarks.Create(Self);
  with SourceMarks do begin
    ImageList:=IDEImages.Images_12;
    //OnGetSynEditOfFile:=@SourceMarksGetSynEditOfFile;
    MarkStyles[mluNone].ImageIndex:=-1;
    MarkStyles[mluProgress].ImageIndex:=-1;
    MarkStyles[mluDebug].ImageIndex:=IDEImages.LoadImage(12, 'state12x12_information');
    MarkStyles[mluVerbose3].ImageIndex:=IDEImages.LoadImage(12, 'state12x12_information');
    MarkStyles[mluVerbose2].ImageIndex:=IDEImages.LoadImage(12, 'state12x12_information');
    MarkStyles[mluVerbose].ImageIndex:=IDEImages.LoadImage(12, 'state12x12_information');
    MarkStyles[mluHint].ImageIndex:=IDEImages.LoadImage(12, 'state12x12_hint');
    MarkStyles[mluNote].ImageIndex:=IDEImages.LoadImage(12, 'state12x12_note');
    MarkStyles[mluWarning].ImageIndex:=IDEImages.LoadImage(12, 'state12x12_warning');
    MarkStyles[mluImportant].ImageIndex:=-1;
    MarkStyles[mluError].ImageIndex:=IDEImages.LoadImage(12, 'state12x12_error');
    MarkStyles[mluFatal].ImageIndex:=IDEImages.LoadImage(12, 'state12x12_fatal');
    MarkStyles[mluPanic].ImageIndex:=IDEImages.LoadImage(12, 'state12x12_fatal');
  end;
  MessagesFrame1.MessagesCtrl.SourceMarks:=SourceMarks;
  MessagesFrame1.MessagesCtrl.OnOpenMessage:=@OnOpenMessage;

  ActiveControl:=MessagesFrame1.MessagesCtrl;
end;

procedure TMessagesView.FormDestroy(Sender: TObject);
begin
  IDEMessagesWindow:=nil;
end;

function TMessagesView.OnOpenMessage(Sender: TObject; Msg: TMessageLine
  ): boolean;
begin
  Result:=false;
  // ask quickfixes
  if IDEQuickFixes.OpenMsg(Msg) then exit(true);
  if Msg.GetFullFilename<>'' then begin
    // ToDo: open file in source editor and mark it as error
    if LazarusIDE.DoOpenFileAndJumpToPos(Msg.GetFullFilename,
      Point(Msg.Line,Msg.Column),-1,-1,-1,OpnFlagsPlainFile)<>mrOk
    then
      exit;
    // ToDo: set error line in source editor
    Result:=true;
  end;
end;

procedure TMessagesView.SetDblClickJumps(AValue: boolean);
begin
  if AValue then
    MessagesFrame1.MessagesCtrl.Options:=
      MessagesFrame1.MessagesCtrl.Options-[mcoSingleClickOpensFile]
  else
    MessagesFrame1.MessagesCtrl.Options:=
      MessagesFrame1.MessagesCtrl.Options+[mcoSingleClickOpensFile]
end;

function TMessagesView.GetViews(Index: integer): TExtToolView;
begin
  Result:=MessagesFrame1.Views[Index];
end;

function TMessagesView.AddCustomMessage(TheUrgency: TMessageLineUrgency;
  Msg: string; aSrcFilename: string; LineNumber: integer; Column: integer;
  const ViewCaption: string): TMessageLine;
begin
  Result:=MessagesFrame1.AddCustomMessage(TheUrgency,Msg,aSrcFilename,
    LineNumber,Column,ViewCaption);
end;

procedure TMessagesView.Clear;
begin
  MessagesFrame1.ClearViews;
end;

procedure TMessagesView.DeleteView(View: TExtToolView);
begin
  if View is TLMsgWndView then
    MessagesFrame1.DeleteView(TLMsgWndView(View));
end;

function TMessagesView.FindUnfinishedView: TExtToolView;
begin
  Result:=MessagesFrame1.FindUnfinishedView;
end;

function TMessagesView.GetSelectedLine: TMessageLine;
begin
  Result:=MessagesFrame1.MessagesCtrl.GetSelectedMsg;
end;

function TMessagesView.GetView(aCaption: string; CreateIfNotExist: boolean
  ): TExtToolView;
begin
  Result:=MessagesFrame1.GetView(aCaption,CreateIfNotExist);
end;

function TMessagesView.IndexOfView(View: TExtToolView): integer;
begin
  if View is TLMsgWndView then
    Result:=MessagesFrame1.IndexOfView(TLMsgWndView(View))
  else
    Result:=-1;
end;

function TMessagesView.SelectFirstUrgentMessage(
  aMinUrgency: TMessageLineUrgency; WithSrcPos: boolean): boolean;
begin
  Result:=MessagesFrame1.SelectFirstUrgentMessage(aMinUrgency,WithSrcPos);
end;

function TMessagesView.SelectNextUrgentMessage(
  aMinUrgency: TMessageLineUrgency; WithSrcPos, Downwards: boolean): boolean;
begin
  Result:=MessagesFrame1.SelectNextUrgentMessage(aMinUrgency,WithSrcPos,Downwards);
end;

function TMessagesView.ViewCount: integer;
begin
  Result:=MessagesFrame1.ViewCount;
end;

function TMessagesView.GetDblClickJumps: boolean;
begin
  Result:=not (mcoSingleClickOpensFile in MessagesFrame1.MessagesCtrl.Options);
end;

end.

