{
 /***************************************************************************
                               Clipbrd.pp
                             -------------------
                             Component Library Clipboard Controls
                   Initial Revision  : Sat Feb 26 2000


 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}

{
@author(TClipBoard - Author Name <smiller@lakefield.net>)
@created(26-Feb-2000)
@lastmod(26-Feb-2000)

Detailed description of the Unit.
Clipboard unit.  For Copying and Pasting.  You know what it's for!  Why am I explaining it?  :-)
}

unit Clipbrd;

{$MODE Objfpc}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
  Classes, SysUtils, LCLLinux;

type
  { TClipBoard }
  {
    @abstract(This is the clipboard class for Copy/Paste functions)
    Introduced by Shane Miller <smiller@lakefield.net>
    Rewrite done by Hongli Lai <hongli@telekabel.nl>
  }

  // TODO: use the Windows clipboard when compiling under Windows
  // TODO: use the Linux clipboard (??gpm??) when compiling under Linux
  // TODO: use OS's clipboard if available

  TClipboard = Class(TPersistent)
  private
    FText: String;
    FData: TMemoryStream;

    FUseText, FUseComponent, Locked: Boolean;
    FFormat: Word;
    procedure SetAsText(Value: String);
    Function GetAsText: String;
    Function GetFormatCount: Integer;
    Function GetFormats(Value: Integer): Word;
  public
    property AsText: String read GetAsText write SetAsText;
    // TODO: Implement FormatCount;
    property FormatCount: Integer read GetFormatCount;
    // TODO: Implement Formats[]
    property Formats[Index: Integer]: Word read GetFormats;
    destructor Destroy; Override;
    // TODO: Implement Assign correctly
    procedure Assign(Source: TPersistent); Override;
    procedure Clear;
    procedure Close;
    // TODO: Implement GetAsHandle
    function GetAsHandle(Format: Word): Integer;
    // TODO: Parent doesn't do anything yet!
    function GetComponent(Owner, Parent: TComponent): TComponent;
    function GetData: TPersistent;
    function GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
    function HasFormat(Format: Word): Boolean;
    procedure Open;
    procedure SetAsHandle(Format: Word; Value: Integer);
    procedure SetComponent(Component: TComponent);
    procedure SetTextBuf(Buffer: PChar);
  end;

function Clipboard: TClipboard;
function SetClipboard(NewClipboard: TClipboard): TClipboard;


implementation

var
  FClipboard: TClipboard;

{$I clipbrd.inc}

function Clipboard: TClipboard;
begin
  if not Assigned(FClipboard) then
     FClipboard := TClipboard.Create;
  Result := FClipboard;
end;

function SetClipboard(NewClipboard: TClipboard): TClipboard;
begin
  if Assigned(FClipboard) then
  begin
     FClipboard.Free;
     FClipboard := nil;
  end;
  FClipboard := NewClipboard;
  Result := FClipboard;
end;

initialization
  FClipboard := nil;
finalization
  FClipboard.Free;
end.

{
  $Log$
  Revision 1.1  2000/07/13 10:28:23  michael
  + Initial import

  Revision 1.8  2000/04/24 23:22:52  lazarus
  MWE:
    + Added ide option to the makfiles to only compile the ide
      (don't compile its dependencies because they will be
      compiled anyhow)

  "Vincent Snijders" <vrs@dds.nl>:
    - removed the references to the windows unit in mwCustomEdit
    = moved the cf_xxxx constants from clipbrd.pp to lcllinux

  Revision 1.7  2000/02/28 21:08:09  lazarus
  MWE:
    Converted to unix files :-)

  Revision 1.6  2000/02/28 21:05:27  lazarus
  MWE:
    Committed the changes for TClipbrd made by
    Hongli Lai <hongli@telekabel.nl>:
      Initial development. The "old" TClipboard doesn't get updated,
      so I started to write a new one.

  Revision 1.5  1999/10/25 17:38:51  lazarus
  More stuff added for compatability.  Most stuff added was put in the windows.pp file.  CONST scroll bar messages and such.  2 functions were also added to that unit that needs to be completed.
  Shane

  Revision 1.4  1999/10/25 15:33:54  lazarus
  Added a few more procedures for compatability.
  Shane

  Revision 1.3  1999/10/22 21:01:50  lazarus

        Removed calls to InterfaceObjects except for controls.pp. Commented
        out any gtk depend lines of code.     MAH

  Revision 1.2  1999/10/22 19:20:02  lazarus
  Added a clipboard function to create a default clipboard
  Shane

  Revision 1.1  1999/10/19 19:59:15  lazarus
  Added clipbrd.pp and clipbrd.inc.  Nothing done to them yet.
  Shane

  Revision 1.1  1999/08/12 16:21:54  lazarus

}