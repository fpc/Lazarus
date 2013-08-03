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
  Forms, Controls, Graphics, Dialogs, etMessageFrame, etSrcEditMarks;

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
  public
    SourceMarks: TETMarks;
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

end;

end.

