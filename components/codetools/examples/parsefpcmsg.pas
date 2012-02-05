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
    Demo for parsing fpc compiler/msg/error*.msg files.
}
program parsefpcmsg;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, FileProcs,
  CodeToolManager, CodeCache, CodeToolsFPCMsgs;

var
  Code: TCodeBuffer;
  Filename: String;
  MsgFile: TFPCMsgFile;
  i: Integer;
  Found: TFPCMsgItem;
  Item: TFPCMsgItem;
  Msg: String;
  s: TfmiSpecialItem;
begin
  if Paramcount<>1 then begin
    writeln('Usage: '+ParamStr(0)+' fpc_file_errore.msg');
    writeln(ParamCount);
    Halt;
  end;
  Filename:=TrimAndExpandFilename(ParamStrUTF8(1));

  // load the file
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('unable to read '+Filename);

  MsgFile:=TFPCMsgFile.Create;
  MsgFile.LoadFromText(Code.Source);
  for s:=succ(fmisiNone) to high(TfmiSpecialItem) do
    if MsgFile.SpecialItems[s]=nil then
      raise Exception.Create('special message '+dbgs(s)+' is missing');

  // check a specific message
  Item:=MsgFile.FindWithID(1009);
  Msg:=MsgFile.GetMsgText(Item);
  if Item.PatternFits(Msg)<0 then begin
    writeln('message does not fit itself: ',Item.GetName,'="',Item.Pattern,'"');
    writeln('Msg: ',Msg);
    writeln('Fits=',Item.PatternFits(Msg));
    raise Exception.Create('bug?');
  end;

  for i:=0 to MsgFile.Count-1 do begin
    Item:=MsgFile[i];
    Msg:=MsgFile.GetMsgText(Item);
    Found:=MsgFile.FindWithMessage(Msg);
    if Found=nil then begin
      // this should never happen
      writeln('message does not fit itself: i=',i,
        ' MsgFile[i]=',Item.GetName,'="',Item.Pattern,'"');
      writeln('Msg: ',Msg);
      writeln('Fits=',Item.PatternFits(Msg));
      raise Exception.Create('bug?');
    end else if Found<>Item then begin
      writeln('message pattern is ambiguous: i=',i,
        ' MsgFile[i]=',Item.GetName,'="',Item.Pattern,'"',
        ' Other=',Found.GetName,'="',Found.Pattern,'"');
    end;
  end;
  MsgFile.Free;
end.

